import sys
import re
from codecs import encode, decode
from collections import namedtuple

DEBUG = False


# Whitespace slots inside a single `// expected-X{{...}}` directive. Stored
# verbatim from parse so render reproduces the source byte-for-byte:
#
#   //{slash}expected-{category}{re}{at}{@+N}{:M}{count}{N}{braces}{{...}}
Whitespace = namedtuple("Whitespace", ["slash", "at", "count", "braces"])
DEFAULT_WHITESPACE = Whitespace(slash=" ", at="", count="", braces="")


def dprint(*args):
    if DEBUG:
        print(*args, file=sys.stderr)


class KnownException(Exception):
    pass


def parse_error_category(s, prefix):
    if "no expected directives found" in s:
        return None
    parts = s.split("diagnostics")
    diag_category = parts[0]
    category_parts = parts[0].strip().strip("'").split("-")
    expected = category_parts[0]
    if expected != prefix:
        raise Exception(
            f"expected prefix '{prefix}', but found '{expected}'. Multiple verify prefixes are not supported."
        )
    diag_category = category_parts[1]
    if "seen but not expected" in parts[1]:
        seen = True
    elif "expected but not seen" in parts[1]:
        seen = False
    else:
        raise KnownException(f"unexpected category '{parts[1]}'")
    return (diag_category, seen)


class Line:
    def __init__(self, content, line_n):
        self.content = content
        # Every `expected-*` directive on this line, in source order. A line
        # may carry several: the verifier scans the buffer for `expected-`,
        # so `// expected-error{{a}} expected-note{{b}}` is two directives
        # sharing one comment, and `// expected-error{{a}} //
        # expected-note{{b}}` is two directives each with their own. `content`
        # holds one `{{DIAG}}` placeholder per entry.
        self.diags = []
        self.line_n = line_n
        self.targeting_diags = []

    @property
    def diag(self):
        """The line's first directive, or None. Most of the pipeline deals with
        one directive per line; multi-directive lines are handled explicitly by
        iterating `diags`."""
        return self.diags[0] if self.diags else None

    @diag.setter
    def diag(self, value):
        self.diags = [value] if value is not None else []

    def update_line_n(self, n):
        self.line_n = n

    def render(self):
        if not self.diags:
            return self.content
        parts = self.content.split("{{DIAG}}")
        assert len(parts) == len(self.diags) + 1
        res = parts[0]
        # The `//` of a comment belongs to the first directive in it; the
        # ones packed after it render bare. If that first directive is being
        # removed, its `//` must be handed to whichever directive in the
        # comment survives, otherwise the survivor would render as code.
        pending_slash = None
        for diag, tail in zip(self.diags, parts[1:]):
            rendered = diag.render()
            owns_slashes = getattr(diag, "has_slashes", True)
            if rendered:
                if not owns_slashes and pending_slash is not None:
                    # The survivor renders its own leading whitespace, so only
                    # borrow the removed directive's when it has none.
                    own_ws = (
                        getattr(diag, "whitespace_strings", None)
                        or DEFAULT_WHITESPACE
                    ).slash
                    borrowed = "" if own_ws else pending_slash
                    rendered = "//" + borrowed + rendered
                pending_slash = None
            elif owns_slashes:
                ws = (
                    getattr(diag, "whitespace_strings", None)
                    or DEFAULT_WHITESPACE
                )
                pending_slash = ws.slash
            res += rendered + tail
        if not res.strip():
            return ""
        return res.rstrip() + "\n"


class Diag:
    def __init__(
        self,
        prefix,
        diag_content,
        category,
        parsed_target_line_n,
        line_is_absolute,
        col,
        count,
        line,
        is_re,
        whitespace_strings,
        is_from_source_file,
        nested_lines,
        diag_content_raw=None,
        original_count_str=None,
        fixits_raw_str="",
        had_none_fixit_marker=False,
        preserved_markers=None,
        had_absolute_line_in_source=False,
        has_slashes=True,
        source_span=None,
    ):
        self.prefix = prefix
        self.diag_content = diag_content
        # Raw text from {{...}} preserved for round-trip rendering. None for
        # synthesized diags (which fall through to escape-on-render).
        self.diag_content_raw = diag_content_raw
        # The count digit as written in source ("", "1", "2", ...) or None if
        # absent. Frozen at parse time and never mutated. render() preserves it
        # iff the current count value still equals what was written.
        self.original_count_str = original_count_str
        self.category = category
        self.parsed_target_line_n = parsed_target_line_n
        self.line_is_absolute = line_is_absolute
        self.count = count
        self.line = line
        self.target = None
        self.is_re = is_re
        self.absolute_target()
        self.whitespace_strings = whitespace_strings
        self.is_from_source_file = is_from_source_file
        self._col = col
        self.nested_lines = nested_lines
        self.parent = None
        self.closer = None
        self.fixits_raw_str = fixits_raw_str
        self.had_none_fixit_marker = had_none_fixit_marker
        # Non-fix-it markers (group-name=, documentation-file=) seen inside
        # the trailing fix-it run on the source line, in source order. They
        # need to be re-emitted verbatim if actual_fixits replaces the
        # source's fix-its.
        self.preserved_markers = (
            list(preserved_markers) if preserved_markers else []
        )
        # Whether any fix-it marker on the source side carried an absolute
        # `<line>:<col>` position. When False, actual fix-its with absolute
        # lines coming from the verifier are rewritten as relative offsets
        # at render time so the test stays stable across line shifts.
        self.had_absolute_line_in_source = had_absolute_line_in_source
        # None means: no fix-it error reported for this diag, render
        # fixits_raw_str as is. A list (possibly empty) means: replace the
        # source fix-its with these exact marker strings.
        self.actual_fixits = None
        # Child-note (`{{children:...}}`) support, used only under
        # -verify-child-notes. A parent diag owns a block of child notes:
        #   `children` holds the child-note Lines (each with an is_child_note
        #   Diag), and `children_closer` is the Line rendering the `// }}`
        #   that closes the block. On a child-note diag, `child_of` points
        #   back at the owning parent Diag and `is_child_note` is True.
        self.children = []
        self.children_closer = None
        self.is_child_note = False
        self.child_of = None
        # False for a directive packed onto a comment opened by an earlier
        # directive on the same line (`// expected-error{{a}}
        # expected-note{{b}}`): it renders without a leading `//`.
        self.has_slashes = has_slashes
        # 1-based, inclusive column range this directive occupies in its source
        # line, fix-it run included. A verifier error always points somewhere
        # inside this range, which is what tells sibling directives on one line
        # apart. None for synthesized directives (they own their whole line).
        self.source_span = source_span

    def decrement_count(self):
        if self.count <= 0:
            raise KnownException(
                f"more diagnostics reported against "
                f"'expected-{self.category}{{{{{self.diag_content}}}}}' than it "
                f"expects. Aborting to avoid corrupting the test."
            )
        self.count -= 1

    def increment_count(self):
        assert self.count >= 0
        self.count += 1

    def unset_target(self):
        assert self.target is not None
        self.target.targeting_diags.remove(self)
        self.target = None

    def set_target(self, target):
        if self.target:
            self.unset_target()
        self.target = target
        self.target.targeting_diags.append(self)

    def absolute_target(self):
        if self.target:
            return self.target.line_n
        if self.line_is_absolute:
            return self.parsed_target_line_n
        return self.line.line_n + self.parsed_target_line_n

    def relative_target(self):
        return self.absolute_target() - self.line.line_n

    def col(self):
        # expected-expansion requires column. Otherwise only retain column info if it's already there.
        if self._col and (
            self.category == "expansion" or self.is_from_source_file
        ):
            return self._col
        return None

    def take(self, other_diag):
        assert self.count == 0
        assert other_diag.count > 0
        assert other_diag.target == self.target
        assert not other_diag.line_is_absolute
        assert not other_diag.is_re and not self.is_re
        self.line_is_absolute = False
        self.diag_content = other_diag.diag_content
        self.diag_content_raw = other_diag.diag_content_raw
        # original_count_str is deliberately not copied: render keys off it
        # vs the new count to decide whether to keep self's original digit.
        self.count = other_diag.count
        self.category = other_diag.category
        self.fixits_raw_str = other_diag.fixits_raw_str
        self.had_none_fixit_marker = other_diag.had_none_fixit_marker
        self.actual_fixits = other_diag.actual_fixits
        self.preserved_markers = other_diag.preserved_markers
        self.had_absolute_line_in_source = (
            other_diag.had_absolute_line_in_source
        )
        other_diag.count = 0

    def _render_fixits(self):
        if self.actual_fixits is None:
            return self.fixits_raw_str
        # Re-emit any non-fix-it markers seen inside the source fix-it run
        # (e.g. {{group-name=...}}) in their original order, then the actual
        # fix-its from the verifier, then preserve {{none}} if present.
        parts = list(self.preserved_markers)
        actuals = self.actual_fixits
        if not self.had_absolute_line_in_source:
            # The verifier always emits absolute `<line>:<col>` positions in
            # actual fix-its; convert them to relative offsets so the test
            # source is stable across line shifts. Source already using
            # absolute lines is left alone. Relative offsets in fix-it
            # bodies are interpreted by the verifier as offsets from the
            # diagnostic line, not the comment line.
            diag_line_n = self.absolute_target()
            actuals = [
                relativize_fixit_marker(a, diag_line_n) for a in actuals
            ]
        parts.extend(actuals)
        if self.had_none_fixit_marker:
            parts.append(
                "{{none}}"
            )  # keep {{none}}, it still means "no documentation-file" etc.
        if not parts:
            return ""
        # Match the source's separator pattern: if the original fix-it run
        # was packed directly against `}}` of the message (no whitespace),
        # stay packed; otherwise emit a leading space.
        leading = (
            ""
            if self.fixits_raw_str and self.fixits_raw_str[0] not in " \t"
            else " "
        )
        return leading + " ".join(parts)

    def render(self):
        assert self.count >= 0
        if self.count == 0:
            return ""
        line_location_s = ""
        if self.relative_target() != 0:
            if self.line_is_absolute:
                line_location_s = f"@{self.absolute_target()}"
            elif self.relative_target() > 0:
                line_location_s = f"@+{self.relative_target()}"
            else:
                line_location_s = (
                    f"@{self.relative_target()}"  # the minus sign is implicit
                )
        # If the source had an explicit digit and the count value still equals
        # what was written, preserve the original (e.g. "1" stays "1").
        original_count = (
            int(self.original_count_str) if self.original_count_str else 1
        )
        if self.count == original_count and self.original_count_str is not None:
            count_s = self.original_count_str
        elif self.count != 1:
            count_s = str(self.count)
        else:
            count_s = ""
        re_s = "-re" if self.is_re else ""
        ws = self.whitespace_strings or DEFAULT_WHITESPACE
        col_s = f":{self.col()}" if self.col() else ""
        # Col-only forms (`@:N`) need the leading "@" even with no line offset,
        # otherwise the verifier would see `:N` as part of the message slot.
        if col_s and not line_location_s:
            line_location_s = "@"
        # Smush prevention: if a count is being newly added (no original digit)
        # and there's no ws between location and count, force a separator so
        # the C++ verifier doesn't read "@+1" + "2" as "@+12".
        ws_count = ws.count
        if count_s and not ws_count and self.original_count_str is None:
            ws_count = " "
        slashes_s = "//" if self.has_slashes else ""
        base_s = (
            f"{slashes_s}{ws.slash}expected-{self.prefix}{self.category}{re_s}"
            f"{ws.at}{line_location_s}{col_s}{ws_count}{count_s}{ws.braces}"
        )
        if self.category == "expansion":
            return base_s + "{{"
        else:
            if self.diag_content_raw is not None:
                content_s = self.diag_content_raw
            else:
                # Synthesized from a verifier message; escape backslashes so
                # the C++ lexer reads them back literally.
                # python trivia: raw strings can't end with a backslash
                content_s = self.diag_content.replace("\\", "\\\\")
            return base_s + "{{" + content_s + "}}" + self._render_fixits()


class ExpansionDiagClose:
    def __init__(self, whitespace, line):
        self.whitespace = whitespace
        self.line = line
        self.parent = None
        self.category = "closing"
        # Set when this `}}` closes a `{{children:...}}` block instead of an
        # `expected-expansion`; points at the owning parent Diag.
        self.child_of = None
        self.is_child_note = False

    def render(self):
        return "//" + self.whitespace + "}}"


expected_diag_re = re.compile(
    r"//(\s*)expected-([a-zA-Z0-9-]*)(note|warning|error|remark)(-re)?(\s*?)(@[+-]?\d+|@(?=:))?(:\d+)?(\s*)(\d+)?(\s*)\{\{(.*?)\}\}"
)
# Same as `expected_diag_re` minus the `//`, for a directive packed onto a
# comment an earlier directive on the same line already opened. Only ever
# matched anchored directly after that directive, never searched for: the
# verifier finds directives by scanning for `expected-` anywhere in the buffer,
# but matching a bare `expected-` at an arbitrary position would also hit
# ordinary code and string literals. Group layout is identical so both regexes
# share the group-extraction code, with group 1 holding the whitespace before
# `expected-` instead of the whitespace after `//`.
continuation_diag_re = re.compile(
    r"(\s*)expected-([a-zA-Z0-9-]*)(note|warning|error|remark)(-re)?(\s*?)(@[+-]?\d+|@(?=:))?(:\d+)?(\s*)(\d+)?(\s*)\{\{(.*?)\}\}"
)
expected_expansion_diag_re = re.compile(
    r"//(\s*)expected-([a-zA-Z0-9-]*)(expansion)(-re)?(\s*?)(@[+-]?\d+|@(?=:))(:\d+)(\s*)(\d+)?(\s*)\{\{(.*?)"
)
expected_expansion_close_re = re.compile(r"//(\s*)\}\}")

fixit_marker_re = re.compile(r"\{\{(?P<content>(?:[^}]|\}(?!\}))*)\}\}+")

# Matches the `<line>:<col>` form of a fix-it position, optionally with a
# leading `+`/`-` sign that makes the line offset relative to the comment
# line. Without a sign, the line is absolute.
_fixit_pos_with_line_re = re.compile(r"^(?P<sign>[+-])?(?P<line>\d+):(?P<col>\d+)$")
# Matches a full fix-it range `<start>-<end>` where each side is either a
# line:col pair (with optional sign) or a bare column.
_fixit_range_re = re.compile(
    r"^(?P<start>[+-]?\d+(?::\d+)?)-(?P<end>[+-]?\d+(?::\d+)?)(?P<rest>=.*)\Z",
    re.DOTALL,
)


def _fixit_content_has_absolute_line(content):
    """Whether a fix-it marker's *content* (no `{{`/`}}`) carries an absolute
    line number on either the start or the end of its range.
    """
    rm = _fixit_range_re.match(content)
    if not rm:
        return False
    for pos in (rm.group("start"), rm.group("end")):
        if (
            ":" in pos
            and not pos.startswith("+")
            and not pos.startswith("-")
        ):
            return True
    return False


def _relativize_fixit_pos(pos_str, comment_line_n):
    """If *pos_str* is an absolute `<line>:<col>` position, rewrite it as a
    sign-prefixed offset relative to *comment_line_n*. Already-relative or
    column-only positions are returned unchanged.
    """
    m = _fixit_pos_with_line_re.match(pos_str)
    if not m or m.group("sign"):
        return pos_str
    line_n = int(m.group("line"))
    col = m.group("col")
    offset = line_n - comment_line_n
    if offset == 0:
        # Same line as the comment: drop the line prefix entirely.
        return col
    sign = "+" if offset > 0 else "-"
    return f"{sign}{abs(offset)}:{col}"


def relativize_fixit_marker(marker_text, comment_line_n):
    """Rewrite any absolute line numbers inside a `{{...}}` fix-it marker as
    relative offsets from *comment_line_n*. Markers that already use
    relative offsets, or that do not carry line information, are returned
    unchanged.
    """
    m = fixit_marker_re.match(marker_text)
    if not m:
        return marker_text
    content = m.group("content")
    rm = _fixit_range_re.match(content)
    if not rm:
        return marker_text
    new_start = _relativize_fixit_pos(rm.group("start"), comment_line_n)
    new_end = _relativize_fixit_pos(rm.group("end"), comment_line_n)
    if new_start == rm.group("start") and new_end == rm.group("end"):
        return marker_text
    return "{{" + new_start + "-" + new_end + rm.group("rest") + "}}"


def consume_trailing_fixits(s):
    """Pull fix-it and related ``{{...}}`` markers off the head of *s*.

    Returns ``(raw_text, has_none_marker, preserved_markers)`` where:

    * ``raw_text`` is the substring of *s* covering everything consumed.
      Non-fix-it markers (``{{documentation-file=...}}``,
      ``{{group-name=...}}``) are only included when they appear *between*
      fix-its in the run; trailing ones stay in *s* so the line content
      around the fix-it expectation continues to round-trip verbatim.
    * ``has_none_marker`` is True if a ``{{none}}`` marker was seen.
    * ``preserved_markers`` is the list of non-fix-it markers consumed
      inside the run, in source order; these must be re-emitted verbatim
      when the fix-it run is rewritten.

    Stops at ``{{children:...}}`` (which is parsed elsewhere).
    """
    pos = 0
    last_consumed_end = 0
    has_none = False
    saw_any = False
    preserved = []
    # Non-fix-it markers seen since the last fix-it (or start). These
    # become "preserved" only if a subsequent fix-it/{{none}} extends the
    # consumed run past them; otherwise they are left for line.content.
    pending_preserved = []
    while True:
        ws_match = re.match(r"[ \t]*", s[pos:])
        next_pos = pos + ws_match.end()
        # `||` separates fix-it alternatives; only meaningful between markers.
        if saw_any and s[next_pos : next_pos + 2] == "||":
            next_pos += 2
            ws_match2 = re.match(r"[ \t]*", s[next_pos:])
            next_pos += ws_match2.end()
        m = fixit_marker_re.match(s, next_pos)
        if not m:
            break
        content = m.group("content")
        if content.startswith("children:"):
            break
        if content.startswith("documentation-file=") or content.startswith(
            "group-name="
        ):
            pending_preserved.append(m.group(0))
            pos = m.end()
            continue
        # Real fix-it (or {{none}}). Commit any pending preserved markers
        # and extend the consumed range to cover this fix-it.
        preserved.extend(pending_preserved)
        pending_preserved = []
        if content == "none":
            has_none = True
        pos = m.end()
        last_consumed_end = m.end()
        saw_any = True
    return (s[:last_consumed_end], has_none, preserved)


def split_fixit_markers(s):
    pos = 0
    results = []
    while pos < len(s):
        ws_match = re.match(r"[ \t]*", s[pos:])
        pos += ws_match.end()
        if pos >= len(s):
            break
        m = fixit_marker_re.match(s, pos)
        if not m:
            break
        results.append(m.group(0))
        pos = m.end()
    return results


def _consumed_end(s, m):
    """End offset in *s* of the directive *m* matched, fix-it run included."""
    if m.re is expected_expansion_diag_re:
        return m.end()
    fixits_raw_str, _, _ = consume_trailing_fixits(s[m.end() :])
    return m.end() + len(fixits_raw_str)


def _find_diag_matches(s):
    """Every `expected-*` directive match on the line, in source order.

    The first directive of a comment carries the `//`; any further directives
    packed onto that same comment are matched as continuations, anchored right
    after the previous directive's message (and fix-it run) so a bare
    `expected-` elsewhere on the line is never mistaken for a directive.
    """
    matches = []
    pos = 0
    while True:
        m = expected_diag_re.search(s, pos)
        if not m:
            return matches
        matches.append(m)
        pos = _consumed_end(s, m)
        while c := continuation_diag_re.match(s, pos):
            matches.append(c)
            pos = _consumed_end(s, c)


def parse_diags(line, filename, prefix, all_prefixes=False):
    """Parse every directive on *line*, replacing each with a `{{DIAG}}`
    placeholder in `line.content`, and return them in source order. Directives
    whose prefix doesn't match are left in the content verbatim."""
    s = line.content
    matches = _find_diag_matches(s)
    if not matches:
        matches = list(expected_expansion_diag_re.finditer(s))
        if len(matches) > 1:
            raise KnownException(
                f"multiple expansions on line {filename}:{line.line_n}. Aborting due to missing implementation."
            )
    if not matches:
        ms = expected_expansion_close_re.findall(s)
        if not ms:
            return []
        if len(ms) > 1:
            raise KnownException(
                f"multiple closed scopes on line {filename}:{line.line_n}. Aborting due to missing implementation."
            )
        line.content = expected_expansion_close_re.sub("{{DIAG}}", s)
        return [ExpansionDiagClose(ms[0], line)]

    diags = []
    new_content = ""
    last_end = 0
    for m in matches:
        [
            ws_slash,
            check_prefix,
            category_s,
            re_s,
            ws_at,
            target_line_s,
            target_col_s,
            ws_count,
            count_s,
            ws_braces,
            diag_s,
        ] = m.groups()
        if check_prefix != prefix and check_prefix != "" and not all_prefixes:
            # Leave it in the content verbatim, as if it weren't a directive.
            continue
        if not target_line_s or target_line_s == "@":
            target_line_n = 0
            is_absolute = False
        elif target_line_s.startswith("@+"):
            target_line_n = int(target_line_s[2:])
            is_absolute = False
        elif target_line_s.startswith("@-"):
            target_line_n = int(target_line_s[1:])
            is_absolute = False
        else:
            target_line_n = int(target_line_s[1:])
            is_absolute = True
        col = int(target_col_s[1:]) if target_col_s else None
        count = int(count_s) if count_s else 1
        consumed_end = _consumed_end(s, m)
        if m.re is not expected_expansion_diag_re:
            fixits_raw_str, has_none_marker, preserved_markers = (
                consume_trailing_fixits(s[m.end() :])
            )
            # Detect whether any source-side fix-it marker carries an absolute
            # `<line>:<col>` position; if so, future updates preserve absolute
            # form, otherwise actual fix-its are rewritten as relative offsets.
            had_absolute_line = False
            for marker in split_fixit_markers(fixits_raw_str):
                fm = fixit_marker_re.match(marker)
                if not fm:
                    continue
                content = fm.group("content")
                if (
                    content == "none"
                    or content.startswith("documentation-file=")
                    or content.startswith("group-name=")
                ):
                    continue
                if _fixit_content_has_absolute_line(content):
                    had_absolute_line = True
                    break
        else:
            fixits_raw_str, has_none_marker, preserved_markers = "", False, []
            had_absolute_line = False

        new_content += s[last_end : m.start()] + "{{DIAG}}"
        last_end = consumed_end

        unescaped_diag_s = decode(
            encode(diag_s, "utf-8", "backslashreplace"), "unicode-escape"
        )
        diags.append(
            Diag(
                check_prefix,
                unescaped_diag_s,
                category_s,
                target_line_n,
                is_absolute,
                col,
                count,
                line,
                bool(re_s),
                Whitespace(
                    slash=ws_slash, at=ws_at, count=ws_count, braces=ws_braces
                ),
                True,
                [],
                diag_content_raw=diag_s,
                original_count_str=count_s if count_s else None,
                fixits_raw_str=fixits_raw_str,
                had_none_fixit_marker=has_none_marker,
                preserved_markers=preserved_markers,
                had_absolute_line_in_source=had_absolute_line,
                has_slashes=m.re is not continuation_diag_re,
                source_span=(m.start() + 1, consumed_end),
            )
        )
    new_content += s[last_end:]
    line.content = new_content
    return diags


def parse_diag(line, filename, prefix, all_prefixes=False, col=None):
    """The single directive on *line*, or None. When the line carries several
    and *col* is given, the one the column points into is returned."""
    diags = parse_diags(line, filename, prefix, all_prefixes)
    if not diags:
        return None
    if col is not None:
        for diag in diags:
            span = getattr(diag, "source_span", None)
            if span and span[0] <= col <= span[1]:
                return diag
    return diags[0]


def find_diag_on_line(line, col, category=None, content=None):
    """Pick the directive on *line* that a verifier error at column *col*
    refers to. Every error the verifier reports against a directive points
    inside it, so the column is what tells sibling directives on one line
    apart. Falls back to a content/category match, and then to the last
    directive starting at or before the column, for the error kinds whose
    location sits just outside the directive (e.g. a `{{children:` marker)."""
    if not line.diags:
        return None
    if col:
        for diag in line.diags:
            span = getattr(diag, "source_span", None)
            if span and span[0] <= col <= span[1]:
                return diag
    if content is not None:
        for diag in line.diags:
            if (
                getattr(diag, "diag_content", None) == content
                and diag.category == category
                and getattr(diag, "count", 0) > 0
            ):
                return diag
    if col:
        before = [
            diag
            for diag in line.diags
            if getattr(diag, "source_span", None)
            and diag.source_span[0] <= col
        ]
        if before:
            return before[-1]
    return line.diags[0]


def add_line(new_line, lines):
    assert new_line.line_n > 0
    lines.insert(new_line.line_n - 1, new_line)
    for i in range(new_line.line_n, len(lines)):
        line = lines[i]
        assert line.line_n == i
        line.update_line_n(i + 1)
    assert all(line.line_n == i + 1 for i, line in enumerate(lines))


def remove_line(old_line, lines):
    lines.remove(old_line)
    for i in range(old_line.line_n - 1, len(lines)):
        line = lines[i]
        assert line.line_n == i + 2
        line.update_line_n(i + 1)
    assert all(line.line_n == i + 1 for i, line in enumerate(lines))


indent_re = re.compile(r"\s*")


def get_indent(s):
    return indent_re.match(s).group(0)


def orig_line_n_to_new_line_n(line_n, orig_lines):
    return orig_lines[line_n - 1].line_n


def infer_line_context(target, line_n):
    for other in target.targeting_diags:
        if other.is_re:
            raise KnownException(
                "mismatching diag on line with regex matcher. Skipping due to missing implementation"
            )
    reverse = (
        True
        if [
            other
            for other in target.targeting_diags
            if other.relative_target() < 0
        ]
        else False
    )

    targeting = [
        other for other in target.targeting_diags if not other.line_is_absolute
    ]
    targeting.sort(reverse=reverse, key=lambda d: d.relative_target())
    prev_offset = 0
    prev_line = target
    direction = -1 if reverse else 1
    for d in targeting:
        if d.relative_target() != prev_offset + direction:
            break
        prev_offset = d.relative_target()
        prev_line = d.line
    total_offset = prev_offset - 1 if reverse else prev_offset + 1
    if reverse:
        new_line_n = prev_line.line_n + 1
    else:
        new_line_n = prev_line.line_n
    assert new_line_n == line_n + (not reverse) - total_offset
    return (prev_line, total_offset, new_line_n)


def add_diag(
    orig_target_line_n,
    col,
    diag_s,
    diag_category,
    lines,
    orig_lines,
    prefix,
    nested_context,
    insert_after=None,
):
    if nested_context:
        prev_line = None
        for line in lines:
            if line.diag and line.diag.absolute_target() < orig_target_line_n:
                prev_line = line
        if prev_line:
            new_line_n = prev_line.line_n + 1
        else:
            prev_line = nested_context.line
            new_line_n = 1
    else:
        line_n = orig_line_n_to_new_line_n(orig_target_line_n, orig_lines)
        target = lines[line_n - 1]

        if insert_after is not None:
            # Place the new directive immediately after `insert_after` rather
            # than stacking it above the other diagnostics targeting this line.
            # Used for a synthesized sibling expansion, which has a higher
            # expansion index than every pre-existing sibling and so must be
            # laid out below them for source order to match index order.
            prev_line = insert_after
            new_line_n = insert_after.line_n + 1
            total_offset = target.line_n - new_line_n
        else:
            prev_line, total_offset, new_line_n = infer_line_context(
                target, line_n
            )
    indent = get_indent(prev_line.content)
    new_line = Line(indent + "{{DIAG}}\n", new_line_n)
    add_line(new_line, lines)

    whitespace_strings = None
    if prev_line.diag:
        whitespace_strings = prev_line.diag.whitespace_strings
        if prev_line.diag == nested_context:
            if not whitespace_strings:
                whitespace_strings = DEFAULT_WHITESPACE
            whitespace_strings = whitespace_strings._replace(
                slash=whitespace_strings.slash + "  "
            )

    new_diag = Diag(
        prefix,
        diag_s,
        diag_category,
        orig_target_line_n if nested_context else total_offset,
        bool(nested_context),
        col,
        1,
        new_line,
        False,
        whitespace_strings,
        False,
        [],
    )
    new_line.diag = new_diag
    if not nested_context:
        new_diag.set_target(target)
    return new_diag


def _make_children_closer(parent_diag, parent_line):
    indent = get_indent(parent_line.content)
    closer_line = Line(indent + "{{DIAG}}\n", len(parent_diag.children) + 1)
    # Align the closing `}}` with the parent directive (one level shallower
    # than the child notes it encloses), so a block nested inside an expansion
    # lines up like the hand-written examples.
    parent_ws = parent_diag.whitespace_strings or DEFAULT_WHITESPACE
    close = ExpansionDiagClose(parent_ws.slash, closer_line)
    close.child_of = parent_diag
    closer_line.diag = close
    return closer_line


def _ensure_children_block(parent_diag, parent_line):
    """Make sure `parent_diag` has a `{{children:` opener glued to its line and
    a `// }}` closer line, synthesizing them if the parent had no block yet."""
    if "{{children:" not in parent_line.content:
        content = parent_line.content
        if content.endswith("\n"):
            body, nl = content[:-1], "\n"
        else:
            body, nl = content, ""
        parent_line.content = body.rstrip() + " {{children:" + nl
    if parent_diag.children_closer is None:
        parent_diag.children_closer = _make_children_closer(
            parent_diag, parent_line
        )


def add_child_note(
    parent_diag, parent_line, content, prefix, target_line,
    absolute_line=None,
):
    """Append a new child-note directive to `parent_diag`'s `{{children:...}}`
    block. `target_line`, when given, is the source Line the note points at, so
    its `@±N` offset stays correct across line shifts (mirroring add_diag);
    otherwise the note targets the parent line (best effort for child notes in
    a foreign buffer, which cannot be addressed by a plain offset).

    `absolute_line`, when given, pins the note at that absolute line with a
    `@N` location instead of a relative `@±N` offset. This is used for a child
    note that lives *inside* a macro expansion buffer: like the diagnostics the
    nested recursion synthesizes there, it must be addressed by an absolute
    line, since relative offsets are rejected inside an expansion. It is
    mutually exclusive with `target_line`."""
    _ensure_children_block(parent_diag, parent_line)
    indent = get_indent(parent_line.content)
    child_line = Line(indent + "{{DIAG}}\n", len(parent_diag.children) + 1)
    # Indent the child note two spaces past its parent directive (mirroring the
    # nesting add_diag uses inside expansions), so a block whose parent is
    # itself nested in an expansion still reads as one level deeper.
    parent_ws = parent_diag.whitespace_strings or DEFAULT_WHITESPACE
    child_diag = Diag(
        prefix,
        content,
        "note",
        absolute_line if absolute_line is not None else 0,
        absolute_line is not None,
        None,
        1,
        child_line,
        False,
        Whitespace(slash=parent_ws.slash + "  ", at="", count="", braces=""),
        False,
        [],
    )
    child_diag.is_child_note = True
    child_diag.child_of = parent_diag
    child_line.diag = child_diag
    if target_line is not None:
        assert absolute_line is None
        child_diag.set_target(target_line)
    add_line(child_line, parent_diag.children)
    return child_diag


def _split_dead_expansion_for_foreign_prefixes(line, lines, prefix):
    """If the dead `expected-expansion` directive at `line` has surviving
    nested entries with foreign prefixes, replace it with one new expansion
    directive per foreign prefix at the same source location, each owning
    only the entries with its prefix. Returns True if any new directives
    were inserted, False otherwise."""
    # Group surviving nested entries by their prefix, preserving source order.
    groups = []  # list of (foreign_prefix, [Line, ...])
    for nested_line in line.diag.nested_lines:
        if not nested_line.diag or nested_line.diag.count == 0:
            continue
        nested_prefix = nested_line.diag.prefix
        if not nested_prefix or nested_prefix == prefix:
            continue
        for gp, gl in groups:
            if gp == nested_prefix:
                gl.append(nested_line)
                break
        else:
            groups.append((nested_prefix, [nested_line]))
    if not groups:
        return False

    indent = get_indent(line.content)
    closer_ws = (
        line.diag.closer.diag.whitespace
        if line.diag.closer and isinstance(line.diag.closer.diag, ExpansionDiagClose)
        else " "
    )
    insertion_line_n = line.line_n
    for foreign_prefix, nested in groups:
        # Reset nested line_n's so they sit at their own positions within the
        # new expansion (fold/expand later renumbers them in the main lines).
        for i, nl in enumerate(nested):
            nl.line_n = i + 1
        new_line = Line(indent + "{{DIAG}}\n", insertion_line_n)
        new_diag = Diag(
            foreign_prefix,
            "",
            "expansion",
            line.diag.parsed_target_line_n,
            line.diag.line_is_absolute,
            line.diag._col,
            1,
            new_line,
            False,
            line.diag.whitespace_strings,
            # Marking as "from source" prevents remove_dead_diags's take()
            # from absorbing this synthesized sibling into the (dead)
            # original directive that we are about to remove.
            True,
            list(nested),
        )
        new_line.diag = new_diag
        # Synthesize a closer that mirrors the original's whitespace.
        closer_line = Line(indent + "{{DIAG}}\n", None)
        closer_line.diag = ExpansionDiagClose(closer_ws, closer_line)
        new_diag.closer = closer_line
        # Pick a target. Normally the original parent's target Line is
        # preserved so all split directives still resolve to the same
        # absolute line. However, if the original target Line happens to
        # land inside *this* new directive's own nested_lines, pointing the
        # directive at its own body is meaningless; redirect to its closer
        # so the rendered `@+N:C` lands just past the directive's body.
        if line.diag.target is not None and line.diag.target in nested:
            new_diag.set_target(closer_line)
        elif line.diag.target is not None:
            new_diag.set_target(line.diag.target)

        add_line(new_line, lines)
        insertion_line_n = new_line.line_n + 1

    return True


def _strip_children_opener(parent_line):
    """Remove a ` {{children:` opener (and any surrounding blank space) from a
    parent directive line whose child-note block has been emptied."""
    content = parent_line.content
    if content.endswith("\n"):
        body, nl = content[:-1], "\n"
    else:
        body, nl = content, ""
    idx = body.rfind("{{children:")
    if idx == -1:
        return
    parent_line.content = body[:idx].rstrip() + nl


def _collapse_dead_children(parent_diag, prefix):
    """Drop dead child notes from `parent_diag`'s block and, if the block ends
    up empty, remove the opener/closer so the parent renders without a
    `{{children:...}}` block (an empty block is a verifier error). If the
    parent directive itself is being removed (count 0), drop the whole block:
    its child notes belonged to a parent that no longer exists, and the
    verifier does not report them separately."""
    if not parent_diag.children and parent_diag.children_closer is None:
        return
    if parent_diag.count == 0:
        parent_diag.children = []
        parent_diag.children_closer = None
        _strip_children_opener(parent_diag.line)
        return
    remove_dead_diags(parent_diag.children, prefix)
    surviving = [
        cl
        for cl in parent_diag.children
        if cl.diag is None or cl.diag.count != 0
    ]
    parent_diag.children = surviving
    if not surviving:
        _strip_children_opener(parent_diag.line)
        parent_diag.children_closer = None


def remove_dead_diags(lines, prefix):
    for line in lines.copy():
        if line not in lines:
            # Already removed by an earlier take(); skip.
            continue
        if not line.diags:
            continue
        # A line can carry several directives; each is retired on its own, and
        # the line itself only goes away once nothing is left to render on it.
        for diag in list(line.diags):
            if line not in lines:
                break
            _retire_diag_if_dead(diag, line, lines, prefix)
        if line in lines and line.render() == "":
            remove_line(line, lines)


def _retire_diag_if_dead(diag, line, lines, prefix):
    if getattr(diag, "children", None) or (
        getattr(diag, "children_closer", None) is not None
    ):
        _collapse_dead_children(diag, prefix)
    if diag.category == "expansion":
        if not diag.prefix or diag.prefix == prefix:
            # Whether the verifier already reported this expansion as
            # missing (parent count was decremented to 0 in update_lines).
            was_reported_missing = diag.count == 0
            remove_dead_diags(diag.nested_lines, prefix)
            if (
                was_reported_missing
                and _split_dead_expansion_for_foreign_prefixes(
                    line, lines, prefix
                )
            ):
                # The dead expansion has been replaced with one new
                # expansion directive per foreign prefix. Drop the original
                # so the cleanup in remove_dead_diags removes it.
                diag.nested_lines = []
                diag.closer = None
                diag.count = 0
            elif diag.nested_lines:
                diag.count = 1
            else:
                diag.count = 0
    if diag.count != 0:
        return
    # Try absorbing a same-category sibling first so the dead diag's
    # formatting (whitespace, original_count_str) survives a content
    # rewrite. Nested expansion diags have no target, so skip.
    #
    # Expansion directives never absorb a sibling. take() transfers a
    # diagnostic's *message* state, but an expansion directive carries no
    # message: what identifies it is its anchor column, and what it renders
    # is its nested_lines plus its closer, none of which take() moves. So
    # absorbing a live sibling expansion would keep this directive's stale
    # column while dropping the sibling's nested diagnostics along with its
    # line, leaving an empty `expected-expansion` anchored at the wrong
    # column. That is worse than simply deleting the dead directive and
    # letting the live sibling render itself, and it does not converge:
    # re-running on the mangled output reproduces the same shape forever.
    if (
        diag.target is not None
        and diag.category != "expansion"
        and not getattr(diag, "is_child_note", False)
    ):
        for other_diag in diag.target.targeting_diags:
            if (
                other_diag.is_from_source_file
                or other_diag.count == 0
                or other_diag.category != diag.category
                or getattr(other_diag, "is_child_note", False)
            ):
                continue
            if other_diag.is_re or diag.is_re:
                continue
            assert diag.is_from_source_file
            diag.take(other_diag)
            remove_line(other_diag.line, lines)
            return
    # Even if take() didn't merge (e.g. because the synthesized sibling
    # has a different category, as in the wrong-category-with-fix-it
    # case), transfer the dead diag's fix-it state to a live sibling on
    # the same target. The fix-it was reported by the verifier against
    # this source location, so it logically belongs to whichever diag
    # ends up rendering at this location.
    if diag.actual_fixits is not None and diag.target is not None:
        for other_diag in diag.target.targeting_diags:
            if (
                other_diag is diag
                or other_diag.is_from_source_file
                or other_diag.count == 0
                or other_diag.actual_fixits is not None
                or getattr(other_diag, "is_child_note", False)
            ):
                continue
            other_diag.actual_fixits = diag.actual_fixits
            other_diag.had_none_fixit_marker = diag.had_none_fixit_marker
            other_diag.preserved_markers = diag.preserved_markers
            other_diag.had_absolute_line_in_source = (
                diag.had_absolute_line_in_source
            )
            break


def fold_expansions(lines):
    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.diag or not line.diag.parent:
            i += 1
            continue
        remove_line(line, lines)
        if line.diag.category == "closing":
            line.diag.parent.closer = line
        else:
            line.line_n = len(line.diag.parent.nested_lines) + 1
            add_line(line, line.diag.parent.nested_lines)


def expand_expansions(lines):
    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.diag or line.diag.category != "expansion":
            i += 1
            continue
        for j, nested in enumerate(line.diag.nested_lines + [line.diag.closer]):
            nested.line_n = line.line_n + j + 1
            add_line(nested, lines)
        i += 1


def _child_parent_of(line):
    """The parent Diag that owns `line` as part of its `{{children:...}}`
    block, or None. Set for both parsed child-note lines (via the note diag's
    `child_of`) and for literal lines inside the block (via `_children_literal`,
    e.g. `@#marker` child notes we don't parse structurally)."""
    for diag in line.diags:
        parent = getattr(diag, "child_of", None)
        if parent is not None:
            return parent
    return getattr(line, "_children_literal", None)


def fold_children(lines):
    """Pull the lines of every `{{children:...}}` block out of the main line
    list and into their parent diag's `children`/`children_closer`, mirroring
    `fold_expansions` for the child-note nesting."""
    i = 0
    while i < len(lines):
        line = lines[i]
        parent = _child_parent_of(line)
        if parent is None:
            i += 1
            continue
        remove_line(line, lines)
        if line.diag is not None and line.diag.category == "closing":
            parent.children_closer = line
        else:
            line.line_n = len(parent.children) + 1
            add_line(line, parent.children)


def expand_children(lines):
    """Re-insert each surviving `{{children:...}}` block's lines after their
    parent line, mirroring `expand_expansions`."""
    i = 0
    while i < len(lines):
        line = lines[i]
        block = []
        for d in line.diags:
            if not getattr(d, "children", None):
                continue
            block.extend(d.children)
            if d.children_closer is not None:
                block.append(d.children_closer)
        if not block:
            i += 1
            continue
        for j, nested in enumerate(block):
            nested.line_n = line.line_n + j + 1
            add_line(nested, lines)
        i += 1 + len(block)


def error_refers_to_diag(diag_error, diag, target_line_n):
    if diag_error.col and diag.col() and diag_error.col != diag.col():
        return False
    return (
        target_line_n == diag.absolute_target()
        and diag_error.category == diag.category
        and (
            diag.category == "expansion"
            or diag_error.content == diag.diag_content
        )
    )


def find_other_targeting(lines, orig_lines, is_nested, diag_error, prefix):
    if is_nested:
        other_diags = [
            diag
            for line in lines
            for diag in line.diags
            if isinstance(diag, Diag)
            and (not diag.prefix or diag.prefix == prefix)
            and error_refers_to_diag(diag_error, diag, diag_error.line)
        ]
    else:
        target = orig_lines[diag_error.line - 1]
        other_diags = [
            d
            for d in target.targeting_diags
            if (not d.prefix or d.prefix == prefix)
            and error_refers_to_diag(diag_error, d, target.line_n)
        ]
    return other_diags


def find_last_expansion_left_of(orig_lines, is_nested, diag_error, prefix):
    """Return the source-order-last `expected-expansion` directive targeting
    `diag_error`'s line at a strictly lower anchor column, or None.

    Expansion anchors are identified by (line, column), so one target line can
    carry several unrelated expansion directives. Laying a new directive out
    below every directive whose anchor column precedes its own keeps expansion
    directives that share a target line sorted by ascending column, matching the
    left-to-right order of the anchors on the line they describe.

    The comparison is strict so that siblings at the *same* anchor are left to
    the expansion-index layout rules instead.
    """
    # `insert_after` is only honoured for top-level directives; inside a nested
    # expansion `add_diag` places directives by target line within the expansion
    # buffer. `diag_error.line` also indexes that buffer rather than orig_lines.
    if is_nested or not diag_error.col:
        return None
    target = orig_lines[diag_error.line - 1]
    to_the_left = [
        d
        for d in target.targeting_diags
        if d.category == "expansion"
        and (not d.prefix or d.prefix == prefix)
        and d.col()
        and d.col() < diag_error.col
    ]
    if not to_the_left:
        return None
    # targeting_diags is in creation order, not source order.
    return max(to_the_left, key=lambda d: d.line.line_n)


def update_lines(
    diag_errors, lines, orig_lines, prefix, filename, nested_context,
    orig_filename=None
):
    # `orig_filename` is the file actually being rewritten (the one `orig_lines`
    # belong to). It stays constant across the nested-expansion recursion, where
    # `filename` becomes the expansion buffer's synthetic name. Child notes that
    # point back into the outer file are resolved against `orig_lines` using it.
    if orig_filename is None:
        orig_filename = filename
    for diag_error in diag_errors:
        if not isinstance(diag_error, NotFoundDiag):
            continue
        line_n = diag_error.line
        line = orig_lines[line_n - 1]
        assert line.diags or nested_context
        # The line may hold several directives; the reported column says which.
        diag = find_diag_on_line(
            line, diag_error.col, diag_error.category, diag_error.content
        )
        if diag is None or diag_error.content != diag.diag_content:
            raise KnownException(
                f"{filename}:{line_n} - found diag {diag.diag_content if diag else None} but expected {diag_error.content}"
            )
        if diag_error.category != diag.category:
            raise KnownException(
                f"{filename}:{line_n} - found {diag.category} diag but expected {diag_error.category}"
            )
        diag.decrement_count()

    # Group FixitErrors by their target diag. When count > 1 and the verifier
    # produces distinct actual fix-it sets per occurrence (e.g.
    # `expected-warning 2 {{msg}} {{wrong}}` against two diagnostics that
    # emit different fix-its), split the source's count expectation into
    # one per occurrence so each set of actual fix-its lands on its own
    # directive.
    fixit_errors_by_diag = {}
    for diag_error in diag_errors:
        if not isinstance(diag_error, FixitError):
            continue
        line_n = diag_error.line
        line = orig_lines[line_n - 1]
        diag = find_diag_on_line(line, diag_error.col)
        if diag is None:
            raise KnownException(
                f"{filename}:{line_n} - fix-it mismatch reported, but no expected-* directive parsed on that line"
            )
        bucket = fixit_errors_by_diag.setdefault(id(diag), (diag, []))
        bucket[1].append(diag_error.actual_fixits)

    for diag, actuals_list in fixit_errors_by_diag.values():
        unique = []
        for actual in actuals_list:
            if actual not in unique:
                unique.append(actual)
        if len(unique) <= 1 or diag.count <= 1 or nested_context is not None:
            diag.actual_fixits = unique[0]
            continue
        # Split: peel off siblings for each earlier distinct actual_fixits
        # set. The original diag keeps the last set so the synthesized
        # siblings (inserted ahead of it via add_diag) line up with the
        # earlier actual diagnostics in source order.
        diag.actual_fixits = unique[-1]
        diag.count -= len(unique) - 1
        if diag.count < 1:
            # Shouldn't happen if the verifier reports at most `count` fix-it
            # errors per directive, but guard against pathological inputs.
            diag.count = 1
        for extra_actual in unique[:-1]:
            new_diag = add_diag(
                diag.absolute_target(),
                diag.col(),
                diag.diag_content,
                diag.category,
                lines,
                orig_lines,
                diag.prefix,
                nested_context,
            )
            new_diag.actual_fixits = extra_actual
            new_diag.had_none_fixit_marker = diag.had_none_fixit_marker
            new_diag.preserved_markers = list(diag.preserved_markers)
            new_diag.had_absolute_line_in_source = (
                diag.had_absolute_line_in_source
            )

    # Process bottom-to-top so inserting directives above a target line does
    # not shift the not-yet-processed targets below it. Within a single line,
    # break ties by expansion index (also descending): sibling expansions that
    # need to be synthesized are each inserted just above the shared target, so
    # the last one processed ends up highest. Feeding them highest-index-first
    # therefore lays them out in ascending index order, matching the order the
    # verifier assigns expansion indices to `expected-expansion` directives.
    def _sort_key(diag_error):
        index = getattr(diag_error, "expansion_index", None)
        return (diag_error.line, index if index is not None else -1)

    diag_errors.sort(reverse=True, key=_sort_key)

    # Snapshot the sibling expected-expansion directives already present at
    # each expansion anchor, in source order, before synthesizing any new
    # ones. The verifier's expansion index is positional in this order, so
    # capturing it up front keeps index->directive routing stable even as we
    # add missing siblings during this pass. `synthesized_expansions` records
    # siblings we create here, so a later nested diag for a different index at
    # the same anchor reuses or extends the set instead of mis-filing into an
    # existing sibling.
    #
    # Both maps are keyed by (line, column), not by line alone: a single source
    # line can be the site of several unrelated expansions distinguished only
    # by column (e.g. a Clang declaration whose synthesized macro attribute
    # expands at column 1 while the macro's own output is anchored at the end
    # of the declaration). The verifier numbers expansions per anchor, so each
    # column has its own index-0 expansion, and keying by line alone would
    # collapse those distinct expansions into one directive.
    preexisting_expansions = {}
    synthesized_expansions = {}
    for diag_error in diag_errors:
        anchor = (diag_error.line, diag_error.col)
        if (
            isinstance(diag_error, NestedDiag)
            and diag_error.expansion_index is not None
            and anchor not in preexisting_expansions
        ):
            preexisting_expansions[anchor] = find_other_targeting(
                lines, orig_lines, bool(nested_context), diag_error, prefix
            )

    for diag_error in diag_errors:
        if not isinstance(diag_error, ExtraDiag) and not isinstance(
            diag_error, NestedDiag
        ):
            continue
        expansion_index = getattr(diag_error, "expansion_index", None)
        is_indexed_expansion = (
            isinstance(diag_error, NestedDiag) and expansion_index is not None
        )
        anchor = (diag_error.line, diag_error.col)
        sibling_anchor = None
        if is_indexed_expansion and expansion_index is not None:
            # Route the nested diag to the specific sibling expansion the
            # verifier reported by its expansion index. Indices below the count
            # of pre-existing siblings map positionally onto them; higher
            # indices belong to siblings we synthesize during this pass.
            preexisting = preexisting_expansions.get(anchor, [])
            if expansion_index < len(preexisting):
                diag = preexisting[expansion_index]
            else:
                diag = synthesized_expansions.get(anchor + (expansion_index,))
                # A synthesized sibling has a higher index than every
                # pre-existing sibling at this anchor, so it must be laid out
                # after them for source order to match expansion-index order.
                # It must also be laid out after every expansion directive
                # anchored further left on the same line, so that directives
                # sharing a target line read in ascending column order. Anchor
                # to whichever of those candidates sits lowest in the file
                # instead of stacking above them.
                #
                # Siblings at one anchor are processed highest-index-first, and
                # each is anchored to the same line, so they still end up in
                # ascending index order. Distinct anchors resolve independently
                # of processing order: whichever column is handled first, a
                # later lower column stacks above it and a later higher column
                # anchors below it.
                anchor_candidates = []
                if preexisting:
                    anchor_candidates.append(preexisting[-1].line)
                left = find_last_expansion_left_of(
                    orig_lines, bool(nested_context), diag_error, prefix
                )
                if left is not None:
                    anchor_candidates.append(left.line)
                if anchor_candidates:
                    sibling_anchor = max(
                        anchor_candidates, key=lambda line: line.line_n
                    )
        else:
            other_diags = find_other_targeting(
                lines, orig_lines, bool(nested_context), diag_error, prefix
            )
            diag = other_diags[0] if other_diags else None
        if diag:
            diag.increment_count()
        else:
            diag = add_diag(
                diag_error.line,
                diag_error.col,
                diag_error.content,
                diag_error.category,
                lines,
                orig_lines,
                diag_error.prefix,
                nested_context,
                insert_after=sibling_anchor,
            )
            if is_indexed_expansion:
                synthesized_expansions[anchor + (expansion_index,)] = diag
        if isinstance(diag_error, NestedDiag):
            if not diag.closer:
                whitespace = (
                    diag.whitespace_strings.slash
                    if diag.whitespace_strings
                    else " "
                )
                diag.closer = Line(
                    get_indent(diag.line.content) + "//" + whitespace + "}}\n",
                    None,
                )
            update_lines(
                [diag_error.nested],
                diag.nested_lines,
                orig_lines,
                prefix,
                diag_error.file,
                diag,
                orig_filename,
            )
        elif isinstance(diag_error, ExtraDiag) and diag_error.child_notes:
            # The whole parent diagnostic was unexpected and carried child
            # notes; synthesize a `{{children:...}}` block for it.
            for cfile, cline, ccol, cmsg, cross_expansion in (
                diag_error.child_notes
            ):
                if nested_context is not None:
                    # The parent diagnostic lives inside a macro expansion. A
                    # child note whose location is *also* inside the *same*
                    # expansion buffer is addressable with an absolute `@N` line
                    # (like the nested diagnostics the recursion synthesizes
                    # there). A child note that points back into the outer file,
                    # or into a *different* expansion, is only expressible with
                    # `@#marker` syntax, which we cannot synthesize
                    # automatically -- refuse those.
                    if cfile == orig_filename:
                        raise KnownException(
                            "cannot synthesize a child note that points out of "
                            "a macro expansion into the outer file; add it "
                            "manually using '@#marker' syntax to reference the "
                            "location outside the expansion"
                        )
                    if cross_expansion:
                        raise KnownException(
                            "cannot synthesize a child note that lives in a "
                            "different macro expansion than its parent; add it "
                            "manually using '@#marker' syntax to reference the "
                            "location in the other expansion"
                        )
                    add_child_note(
                        diag, diag.line, cmsg, diag_error.prefix, None,
                        absolute_line=cline,
                    )
                    continue
                # Top-level parent. A child note located in the file being
                # updated is targeted at its real source line so its `@±N`
                # offset stays correct. A child note located inside a macro
                # expansion buffer would need `@#marker` syntax to reference
                # into the expansion, which we cannot synthesize automatically
                # -- refuse (the mirror of the in-expansion parent case above).
                if cfile != orig_filename:
                    raise KnownException(
                        "cannot synthesize a child note that points into a "
                        "macro expansion; add it manually using '@#marker' "
                        "syntax to reference the location inside the expansion"
                    )
                target = None
                if 1 <= cline <= len(orig_lines):
                    target = orig_lines[cline - 1]
                add_child_note(
                    diag, diag.line, cmsg, diag_error.prefix, target
                )

    # Add child notes reported against parents that matched an existing
    # directive. Done after the pass above so any newly-added parent diags
    # are already in place. `ExtraChildNote`s are always processed here at the
    # top level (they are never folded into the nested-expansion recursion), so
    # the parent directive is located by absolute line into the outer file --
    # this also covers child notes on a diagnostic emitted inside a macro
    # expansion, whose parent directive lives in `orig_lines`.
    for diag_error in diag_errors:
        if not isinstance(diag_error, ExtraChildNote):
            continue
        if not (1 <= diag_error.line <= len(orig_lines)):
            raise KnownException(
                f"could not locate parent diagnostic at "
                f"{diag_error.file}:{diag_error.line} for unexpected child note"
            )
        parent_line = orig_lines[diag_error.line - 1]
        parent_diag = find_diag_on_line(parent_line, diag_error.col)
        if parent_diag is None or parent_diag.category in (
            "closing",
            "expansion",
        ):
            raise KnownException(
                f"could not find a parent diagnostic directive at "
                f"{diag_error.file}:{diag_error.line} to attach child note "
                f"'{diag_error.content}'"
            )
        # When the parent diagnostic sits inside a macro expansion, a child
        # note that is *also* inside the *same* expansion buffer is addressable
        # with an absolute `@N` line. One that points back into the outer file,
        # or into a *different* expansion, needs `@#marker` syntax we cannot
        # generate automatically.
        if parent_diag.parent is not None:
            if diag_error.child_file == orig_filename:
                raise KnownException(
                    "cannot synthesize a child note that points out of a macro "
                    "expansion into the outer file; add it manually using "
                    "'@#marker' syntax to reference the location outside the "
                    "expansion"
                )
            # The child note shares its parent's expansion only if their whole
            # chains of expansion sites match. (Distinct expansions can share a
            # synthesized buffer name, and even the same site line within it, so
            # neither the file name nor a single (file, line) anchor is enough to
            # tell them apart -- the full chain up to the outer file is.)
            anchors = diag_error.child_expansion_anchors
            # A nested expansion: the child note is reached through more than one
            # expansion site, so its innermost anchor sits in an intermediate
            # expansion buffer rather than the outer file. Its absolute line is
            # meaningful only in that buffer, which the simple absolute-line
            # synthesis here cannot address, so refuse. (A robust implementation
            # would reconstruct the matched parent directive's own site chain and
            # compare it against `anchors`; that is not done here.)
            if len(anchors) > 1:
                raise KnownException(
                    "cannot synthesize a child note that lives in a nested "
                    "macro expansion; add it manually using '@#marker' "
                    "syntax to reference the location in the expansion"
                )
            if anchors:
                anchor_line = anchors[0][1]
                if 1 <= anchor_line <= len(orig_lines):
                    child_site = orig_lines[anchor_line - 1].line_n
                    parent_site = parent_diag.parent.absolute_target()
                    if child_site != parent_site:
                        raise KnownException(
                            "cannot synthesize a child note that lives in a "
                            "different macro expansion than its parent; add it "
                            "manually using '@#marker' syntax to reference the "
                            "location in the other expansion"
                        )
            add_child_note(
                parent_diag,
                parent_line,
                diag_error.content,
                diag_error.prefix or prefix,
                None,
                absolute_line=diag_error.child_line,
            )
            continue
        # A top-level parent whose child note lives inside a macro expansion
        # buffer would need `@#marker` syntax to reference into the expansion,
        # which we cannot synthesize automatically (mirror of the case above).
        if diag_error.child_file != orig_filename:
            raise KnownException(
                "cannot synthesize a child note that points into a macro "
                "expansion; add it manually using '@#marker' syntax to "
                "reference the location inside the expansion"
            )
        target = None
        if diag_error.child_file == diag_error.file and 1 <= (
            diag_error.child_line
        ) <= len(orig_lines):
            target = orig_lines[diag_error.child_line - 1]
        add_child_note(
            parent_diag,
            parent_line,
            diag_error.content,
            diag_error.prefix or prefix,
            target,
        )

    # Strip `{{children:...}}` blocks that the verifier rejected because the
    # invocation lacks -verify-child-notes. The notes they listed resurface as
    # top-level `unexpected note produced` diagnostics handled above.
    for diag_error in diag_errors:
        if not isinstance(diag_error, StripChildrenBlock):
            continue
        if not (1 <= diag_error.line <= len(orig_lines)):
            continue
        parent_diag = find_diag_on_line(
            orig_lines[diag_error.line - 1], diag_error.col
        )
        if parent_diag is None or parent_diag.category in (
            "closing",
            "expansion",
        ):
            continue
        parent_diag.children = []
        parent_diag.children_closer = None
        _strip_children_opener(parent_diag.line)


def _opens_children_block(content):
    """True if `content` (a parsed line, with the diag replaced by `{{DIAG}}`)
    opens a multi-line `{{children:...}}` block, i.e. the last `{{children:`
    on the line is not closed by a `}}` before end of line."""
    idx = content.rfind("{{children:")
    if idx == -1:
        return False
    return "}}" not in content[idx + len("{{children:") :]


def update_test_file(filename, diag_errors, prefix, updated_test_files):
    dprint(f"updating test file {filename}")
    if filename in updated_test_files:
        raise KnownException(f"{filename} already updated, but got new output")
    else:
        updated_test_files.add(filename)
    with open(filename, "r") as f:
        lines = [
            Line(line, i + 1) for i, line in enumerate(f.readlines() + [""])
        ]
    orig_lines = list(lines)

    expansion_context = []
    children_context = None
    unmatched_closers = []
    for line in lines:
        dprint(f"parsing line {line.render()}")
        diags = parse_diags(line, filename, prefix, all_prefixes=True)
        diag = diags[0] if diags else None
        if children_context is not None:
            # Inside a `{{children:...}}` block: route the closer and the
            # child-note (or literal) lines to the owning parent rather than
            # treating them as top-level diags/expansions.
            if diag and diag.category == "closing":
                line.diags = diags
                diag.child_of = children_context
                children_context = None
            elif diag:
                line.diags = diags
                for d in diags:
                    d.is_child_note = True
                    d.child_of = children_context
            elif line.content.lstrip().startswith("}}"):
                # A `}}`-first line that didn't parse as a `// }}` closer
                # (e.g. a C-comment `}}*/` block terminator): treat it as a
                # literal closer so we don't run past the end of the block.
                line._children_literal = children_context
                children_context = None
            else:
                line._children_literal = children_context
            continue
        if diag and diag.category == "closing":
            if not expansion_context:
                # A `// }}` with no matching `expected-expansion` opener above
                # it. The verifier ignores such a stray closer, so it is
                # punctuation left over from an expansion directive that is no
                # longer there. Drop the whole line. `lines` is being iterated,
                # so defer the removal until the file has been fully parsed.
                dprint(f"  unmatched closer, dropping line")
                unmatched_closers.append(line)
                continue
            dprint(f"  parsed closer {diag.render()}")
            line.diag = diag
            diag.parent = expansion_context.pop()
            continue
        if diag:
            dprint(f"  parsed diag {diag.render()}")
            line.diags = diags
            for d in diags:
                if expansion_context:
                    d.parent = expansion_context[-1]
                else:
                    target_idx = d.absolute_target() - 1
                    if 0 <= target_idx < len(lines):
                        d.set_target(lines[target_idx])
                    # Otherwise the directive points outside the file (e.g. the
                    # code it targeted was deleted, leaving the offset dangling
                    # past the end). Leave it targetless: the verifier reports
                    # its expansion as "not produced", so it is dropped as a
                    # dead directive rather than crashing here on an
                    # out-of-range line index.
            last = diags[-1]
            if last.category == "expansion":
                expansion_context.append(last)
            elif last.category == "closing":
                expansion_context.pop()
            elif _opens_children_block(line.content):
                # The opener is glued to the end of the line, so the block
                # belongs to the last directive on it.
                children_context = last
        else:
            dprint(f"  no diag")

    for closer_line in unmatched_closers:
        remove_line(closer_line, lines)

    # Fold `{{children:...}}` blocks before expansions so that child notes
    # nested inside an `expected-expansion` are pulled onto their parent nested
    # diagnostic's `children` list first; folding expansions afterwards then
    # carries that parent diagnostic (with its children attached) into the
    # expansion's `nested_lines`.
    fold_children(lines)
    fold_expansions(lines)
    update_lines(diag_errors, lines, orig_lines, prefix, filename, None)
    remove_dead_diags(lines, prefix)
    expand_expansions(lines)
    expand_children(lines)
    with open(filename, "w") as f:
        for line in lines:
            f.write(line.render())


def update_test_files(errors, prefix, unparsed_files):
    errors_by_file = {}
    for error in errors:
        filename = error.file
        if filename not in errors_by_file:
            errors_by_file[filename] = []
        errors_by_file[filename].append(error)
    updated_test_files = set()
    for filename, diag_errors in errors_by_file.items():
        if filename in unparsed_files:
            continue
        try:
            update_test_file(filename, diag_errors, prefix, updated_test_files)
        except KnownException as e:
            return (
                f"Error in update-verify-tests while updating {filename}: {e}",
                None,
            )
    updated_files = list(updated_test_files)
    assert updated_files or unparsed_files
    if not updated_files:
        return (
            f"no files updated: found diagnostics in unparsed files {', '.join(unparsed_files)}",
            None,
        )
    return (None, updated_files)


"""
ex:
test.swift:2:6: error: expected error not produced
  // expected-error@+1{{asdf}}
~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~
"""
diag_error_re = re.compile(
    r"(\S+):(\d+):(\d+): error: expected (\S+) not produced"
)


"""
ex:
test.swift:2:3: error: unexpected error produced: cannot find 'a' in scope
  a = 2
  ^
"""
diag_error_re2 = re.compile(
    r"(\S+):(\d+):(\d+): error: unexpected (\S+) produced: (.*)"
)


"""
ex:
test.swift:2:43: error: incorrect message found
  bar = 2  // expected-error{{asdf}}
                              ^~~~
                              cannot find 'bar' in scope
"""
diag_error_re3 = re.compile(
    r"(\S+):(\d+):(\d+): error: incorrect message found"
)


"""
ex:
test.swift:2:15: error: expected warning, not error
  // expected-warning@+1{{cannot find 'bar' in scope}}
              ^~~~~~~
              error
"""
diag_error_re4 = re.compile(
    r"(\S+):(\d+):(\d+): error: expected (\S+), not (\S+)"
)

"""
ex:
test.swift:12:14: note: in expansion 0 from here
func foo() {}
             ^

The trailing integer is the index of the expansion among all expansions that
share this source location, in the order their `expected-expansion` directives
appear in the source (see DiagnosticVerifier's ExpansionContext). It routes a
nested diagnostic to the correct sibling expansion when several expansions are
attached at the same location (e.g. multiple peer macros on one declaration).
"""
diag_expansion_note_re = re.compile(
    r"(\S+):(\d+):(\d+): note: in expansion (\d+) from here"
)

"""
ex:
test.h:8:52: note: file 'test.h' is not parsed for 'expected' statements. Use '-verify-additional-file test.h' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file                                                                                                                                    
void foo(int len, int * __counted_by(len) p);                                                                           
                                           ^
"""
diag_not_parsed_note_re = re.compile(
    r"(\S+):(\d+):(\d+): note: file '(\S+)' is not parsed for 'expected' statements"
)

"""
ex:
/path/to/sdk/file.h:19:29: remark: diagnostic produced elsewhere: did not add safe interop wrapper
struct _LIBCPP_TEMPLATE_VIS input_iterator_tag {};
                            ^
/path/to/sdk/file.h:19:29: note: diagnostic produced elsewhere: implicit functions are ignored
struct _LIBCPP_TEMPLATE_VIS input_iterator_tag {};
                            ^
"""
diag_produced_elsewhere_re = re.compile(
    r"(\S+):(\d+):(\d+): (?:note|remark|warning): diagnostic produced elsewhere: (.*)"
)


"""
Emitted under -verify-child-notes when a matched parent diagnostic has a child
note that no `{{children:...}}` entry accounted for. Always followed by a
`note: for parent matched here` pointing at the parent's expected-* directive
(possibly with intervening `note: in expansion from here` notes if the child
note originates in a macro expansion).
ex:
test.swift:1:8: error: unexpected child note produced: 'A' previously declared here
struct A {}
       ^
test.swift:5:13: note: for parent matched here
struct A {} // expected-error{{invalid redeclaration of 'A'}} {{children:
            ^
"""
diag_unexpected_child_note_re = re.compile(
    r"(\S+):(\d+):(\d+): error: unexpected child note produced: (.*)"
)
diag_for_parent_matched_re = re.compile(
    r"(\S+):(\d+):(\d+): note: for parent matched here"
)

"""
Emitted under -verify-child-notes as a child note attached to an
`unexpected <kind> produced` parent (the whole parent diagnostic was
unexpected, so its child notes are reported as notes on it).
ex:
test.swift:2:8: error: unexpected error produced: invalid redeclaration of 'A'
struct A {}
       ^
test.swift:1:8: note: with child note: 'A' previously declared here
struct A {}
       ^
"""
diag_with_child_note_re = re.compile(
    r"(\S+):(\d+):(\d+): note: with child note: (.*)"
)

"""
Emitted under -verify-child-notes when a `{{children:...}}` entry matched a
child note that belongs to a different parent. We can't meaningfully re-target
child notes across parents automatically, so this is surfaced as a hard error.
ex:
test.swift:5:13: error: matched child note with different parent
"""
diag_matched_wrong_parent_re = re.compile(
    r"(\S+):(\d+):(\d+): error: matched child note with different parent"
)

"""
Emitted for a `{{children:...}}` block when the invocation does NOT pass
-verify-child-notes: the block is rejected and its child notes are cleared, so
the actual notes are reported separately as `unexpected note produced` and get
re-added as top-level `expected-note`s. We respond by stripping the now-invalid
block off the parent directive; the reported location is the `{{children:`
marker, i.e. on the parent directive's own line.
ex:
test.swift:2:64: error: child diagnostics block requires -verify-child-notes
struct A {} // expected-error{{invalid redeclaration of 'A'}} {{children:
                                                              ^
"""
diag_children_requires_flag_re = re.compile(
    r"(\S+):(\d+):(\d+): error: child diagnostics block requires "
    r"-verify-child-notes"
)



"""
ex:
test.swift:2:89: error: expected fix-it not seen; actual fix-it seen: {{3-8=_}}
test.swift:2:89: error: expected fix-it not seen
test.swift:2:89: error: expected no fix-its; actual fix-it seen: {{3-8=_}}
test.swift:2:89: error: unexpected fix-it seen; actual fix-its seen: {{3-8=_}} {{9-10=x}}
"""
fixit_error_re = re.compile(
    r"(\S+):(\d+):(\d+): error: "
    r"(expected fix-it not seen|expected no fix-its|unexpected fix-it seen)"
    r"(?:; actual fix-its? seen: (.*))?$"
)


class NotFoundDiag:
    def __init__(self, file, line, col, category, content, prefix):
        self.file = file
        self.line = line
        self.col = col
        self.category = category
        self.content = content
        self.prefix = prefix

    def __str__(self):
        return f"{self.file}:{self.line}:{self.col}: error expected {self.category} not produced (expected {self.content})"


class ExtraDiag:
    def __init__(self, file, line, col, category, content, prefix):
        self.file = file
        self.line = line
        self.col = col
        self.category = category
        self.content = content
        self.prefix = prefix
        # Child notes reported as `note: with child note: ...` attached to
        # this (wholly unexpected) parent diagnostic, in output order. Each
        # entry is (file, line, col, content). Applied when the parent diag
        # directive is synthesized, producing a `{{children:...}}` block.
        self.child_notes = []

    def __str__(self):
        return f"{self.file}:{self.line}:{self.col}: error unexpected {self.category} produced: {self.content}"


class ExtraChildNote:
    """A `unexpected child note produced` reported for a child note whose
    parent diagnostic *did* match an expected directive. The note must be
    added to that parent's `{{children:...}}` block (created if absent), so
    this is bucketed and rendered against the *parent's* file: `file`/`line`/
    `col` locate the parent directive, while `child_*` record the actual
    child note's reported location and message."""

    def __init__(
        self, child_file, child_line, child_col, content, parent_file,
        parent_line, parent_col, prefix, child_expansion_anchors=None,
    ):
        # Route/edit this against the parent directive's file and line.
        self.file = parent_file
        self.line = parent_line
        self.col = parent_col
        self.category = "note"
        self.content = content
        self.child_file = child_file
        self.child_line = child_line
        self.child_col = child_col
        self.prefix = prefix
        # The full chain of expansion sites the child note lives in, as a list
        # of (file, line) taken from its consecutive `note: in expansion from
        # here` notes, ordered innermost first and terminating at the outer
        # file. Empty if the child note is not in an expansion. A single-element
        # chain means the child note sits directly in one expansion of the outer
        # file; a longer chain means it is in a nested expansion. The whole chain
        # is needed to identify *which* expansion instance the note belongs to:
        # distinct nested expansions can share a synthesized buffer name (and
        # even the same site line within it), so only the sequence of outer
        # sites up to the outer file disambiguates them.
        self.child_expansion_anchors = child_expansion_anchors or []

    def __str__(self):
        return (
            f"{self.child_file}:{self.child_line}:{self.child_col}: "
            f"unexpected child note '{self.content}' of parent at "
            f"{self.file}:{self.line}:{self.col}"
        )


class StripChildrenBlock:
    """A `child diagnostics block requires -verify-child-notes` error: the
    parent directive at `line` carries a `{{children:...}}` block but the
    invocation lacks the flag. The block must be removed; the child notes it
    listed resurface as top-level `unexpected note produced` diagnostics and
    are re-added as top-level `expected-note`s by the ExtraDiag path."""

    def __init__(self, file, line, col, prefix):
        self.file = file
        self.line = line
        self.col = col
        self.category = None
        self.content = None
        self.prefix = prefix

    def __str__(self):
        return (
            f"{self.file}:{self.line}:{self.col}: strip children block "
            f"(requires -verify-child-notes)"
        )


class NestedDiag:
    def __init__(self, file, line, col, nested, expansion_index):
        self.file = file
        self.line = line
        self.col = col
        self.category = "expansion"
        self.content = None
        self.nested = nested
        self.expansion_index = expansion_index
        self.prefix = ""

    def __str__(self):
        return f"""
{self.file}:{self.line}:{self.col}: note: in expansion from here (
    {self.nested}
)
"""


class FixitError:
    def __init__(self, file, line, col, actual_fixits):
        self.file = file
        self.line = line
        self.col = col
        self.actual_fixits = actual_fixits
        # Sit alongside NotFoundDiag/ExtraDiag/NestedDiag; the per-file
        # bucket sort needs these attrs even though they're unused here.
        self.category = None
        self.content = None
        self.prefix = ""

    def __str__(self):
        return (
            f"{self.file}:{self.line}:{self.col}: fix-it mismatch "
            f"(actual: {' '.join(self.actual_fixits) or '<none>'})"
        )


def check_expectations(tool_output, prefix):
    """
    The entry point function.
    Called by the stand-alone update-verify-tests.py as well as litplugin.py.
    """
    top_level = []
    unparsed_files = set()
    try:
        i = 0
        while i < len(tool_output):
            line = tool_output[i].strip()
            extra_lines = []

            curr = []
            dprint(f"line: {line.strip()}")
            if diag_produced_elsewhere_re.match(line.strip()):
                dprint(
                    f"diagnostic produced elsewhere (ignored): {line.strip()}"
                )
                n_extra_lines = 3
                if i + n_extra_lines < len(tool_output):
                    next_line = tool_output[i + n_extra_lines]
                    if diag_expansion_note_re.match(next_line.strip()):
                        dprint(f"expansion note (ignored): {next_line.strip()}")
                        n_extra_lines += 1
                extra_lines = tool_output[i + 1 : i + n_extra_lines]
            elif not "error:" in line:
                if "note:" in line:
                    if m := diag_not_parsed_note_re.match(line.strip()):
                        dprint(f"unparsed file: {m.group(4)}")
                        unparsed_files.add(m.group(4))
                        extra_lines = tool_output[i + 1 : i + 3]
                        dprint(f"extra lines: {extra_lines}")
                    else:
                        raise KnownException(
                            f"unhandled note found (line {i+1}): '{line.strip()}'"
                        )
                else:
                    dprint(f"ignored line: {line.strip()}")
            elif m := diag_error_re.match(line):
                dprint(f"diag not found: {line.strip()}")
                extra_lines = tool_output[i + 1 : i + 3]
                dprint(f"extra lines: {extra_lines}")
                diag = parse_diag(
                    Line(extra_lines[0], int(m.group(2))),
                    m.group(1),
                    prefix,
                    col=int(m.group(3)),
                )
                curr.append(
                    NotFoundDiag(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        m.group(4),
                        diag.diag_content,
                        diag.prefix,
                    )
                )
            elif m := diag_error_re2.match(line):
                dprint(f"unexpected diag: {line.strip()}")
                extra_lines = tool_output[i + 1 : i + 3]
                dprint(f"extra lines: {extra_lines}")
                curr.append(
                    ExtraDiag(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        m.group(4),
                        m.group(5),
                        prefix,
                    )
                )
            elif m := diag_unexpected_child_note_re.match(line):
                dprint(f"unexpected child note: {line.strip()}")
                cfile, cline, ccol, cmsg = (
                    m.group(1),
                    int(m.group(2)),
                    int(m.group(3)),
                    m.group(4),
                )
                # The error line is followed by its source+caret, then any
                # `note: in expansion from here` triples (present when the
                # child note originates in a macro expansion), then the
                # required `note: for parent matched here` triple. The
                # expansion triples form the child note's site chain, ordered
                # innermost first and ending at the outer file.
                j = i + 3
                child_expansion_anchors = []
                while j < len(tool_output) and (
                    em := diag_expansion_note_re.match(tool_output[j].strip())
                ):
                    child_expansion_anchors.append(
                        (em.group(1), int(em.group(2)))
                    )
                    j += 3
                pm = (
                    diag_for_parent_matched_re.match(tool_output[j].strip())
                    if j < len(tool_output)
                    else None
                )
                if not pm:
                    raise KnownException(
                        f"'unexpected child note produced' (line {i+1}) was "
                        f"not followed by 'for parent matched here'"
                    )
                j += 3
                extra_lines = tool_output[i + 1 : j]
                dprint(f"extra lines: {extra_lines}")
                # Appended directly (not via `curr`) so the expansion-note
                # wrapping below never folds it into a NestedDiag.
                top_level.append(
                    ExtraChildNote(
                        cfile,
                        cline,
                        ccol,
                        cmsg,
                        pm.group(1),
                        int(pm.group(2)),
                        int(pm.group(3)),
                        prefix,
                        child_expansion_anchors,
                    )
                )
            elif m := diag_matched_wrong_parent_re.match(line):
                raise KnownException(
                    f"child note matched a different parent (line {i+1}); "
                    f"update-verify-tests cannot re-target child notes "
                    f"automatically"
                )
            elif m := diag_children_requires_flag_re.match(line):
                dprint(f"children block without flag: {line.strip()}")
                extra_lines = tool_output[i + 1 : i + 3]
                top_level.append(
                    StripChildrenBlock(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        prefix,
                    )
                )
            # Create two mirroring mismatches when the compiler reports that the category or diagnostic is incorrect.
            # This makes it easier to handle cases where the same diagnostic is mentioned both in an incorrect message/category
            # diagnostic, as well as in an error not produced diagnostic. This can happen for things like 'expected-error 2{{foo}}'
            # if only one diagnostic is emitted on that line, and the content of that diagnostic is actually 'bar'.
            elif m := diag_error_re3.match(line):
                dprint(f"wrong diag message: {line.strip()}")
                extra_lines = tool_output[i + 1 : i + 4]
                dprint(f"extra lines: {extra_lines}")
                diag = parse_diag(
                    Line(extra_lines[0], int(m.group(2))),
                    m.group(1),
                    prefix,
                    col=int(m.group(3)),
                )
                curr.append(
                    NotFoundDiag(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        diag.category,
                        diag.diag_content,
                        diag.prefix,
                    )
                )
                curr.append(
                    ExtraDiag(
                        m.group(1),
                        diag.absolute_target(),
                        int(m.group(3)),
                        diag.category,
                        extra_lines[2].strip(),
                        diag.prefix,
                    )
                )
            elif m := diag_error_re4.match(line):
                dprint(f"wrong diag kind: {line.strip()}")
                extra_lines = tool_output[i + 1 : i + 4]
                dprint(f"extra lines: {extra_lines}")
                diag = parse_diag(
                    Line(extra_lines[0], int(m.group(2))),
                    m.group(1),
                    prefix,
                    col=int(m.group(3)),
                )
                assert diag.category == m.group(4)
                assert extra_lines[2].strip() == m.group(5)
                curr.append(
                    NotFoundDiag(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        diag.category,
                        diag.diag_content,
                        diag.prefix,
                    )
                )
                curr.append(
                    ExtraDiag(
                        m.group(1),
                        diag.absolute_target(),
                        int(m.group(3)),
                        m.group(5),
                        diag.diag_content,
                        diag.prefix,
                    )
                )
            elif m := fixit_error_re.match(line):
                dprint(f"fix-it mismatch: {line.strip()}")
                actual_fixits_str = m.group(5) or ""
                n_extra = 4 if actual_fixits_str else 3
                extra_lines = tool_output[i + 1 : i + n_extra]
                dprint(f"extra lines: {extra_lines}")
                curr.append(
                    FixitError(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        split_fixit_markers(actual_fixits_str),
                    )
                )
            else:
                dprint(f"no match: {line.strip()}")
            i += 1 + len(extra_lines)

            while (
                curr
                and i < len(tool_output)
                and (m := diag_expansion_note_re.match(tool_output[i].strip()))
            ):
                nested_note_lines = tool_output[i : i + 3]
                dprint(f"nested note lines: {nested_note_lines}")
                curr = [
                    NestedDiag(
                        m.group(1),
                        int(m.group(2)),
                        int(m.group(3)),
                        e,
                        int(m.group(4)),
                    )
                    for e in curr
                ]
                i += len(nested_note_lines)

            # `note: with child note: ...` lines trail an `unexpected <kind>
            # produced` parent (after any expansion notes for the parent's own
            # location). Attach each as a child note of the underlying
            # ExtraDiag so a `{{children:...}}` block is synthesized with it.
            while (
                curr
                and i < len(tool_output)
                and (m := diag_with_child_note_re.match(tool_output[i].strip()))
            ):
                dprint(f"with child note: {tool_output[i].strip()}")
                cfile, cline, ccol, cmsg = (
                    m.group(1),
                    int(m.group(2)),
                    int(m.group(3)),
                    m.group(4),
                )
                i += 3
                # A child note that originates inside a macro expansion trails
                # its `with child note:` line with `note: in expansion from
                # here` triples locating each enclosing expansion site. They
                # form the child note's site chain, innermost first, ending at
                # the outer file.
                child_anchors = []
                while i < len(tool_output) and (
                    em := diag_expansion_note_re.match(tool_output[i].strip())
                ):
                    child_anchors.append((em.group(1), int(em.group(2))))
                    i += 3
                for e in curr:
                    base = e
                    # The `NestedDiag`s wrapping the parent locate the parent's
                    # own expansion sites. They were applied outermost-last, so
                    # walking from the outside in yields outermost first;
                    # reverse to get the same innermost-first order as the
                    # child's chain.
                    parent_anchors = []
                    while isinstance(base, NestedDiag):
                        parent_anchors.append((base.file, base.line))
                        base = base.nested
                    parent_anchors.reverse()
                    if isinstance(base, ExtraDiag):
                        # The child note shares the parent's expansion only if
                        # their entire site chains match. Comparing the whole
                        # chain -- not just the innermost (file, line) -- is
                        # required because distinct nested expansions can share a
                        # synthesized buffer name (and even the same site line
                        # within it); only the sequence of enclosing sites up to
                        # the outer file distinguishes them. An empty parent
                        # chain (a top-level parent) never counts as sharing.
                        same_expansion = (
                            bool(parent_anchors)
                            and child_anchors == parent_anchors
                        )
                        base.child_notes.append(
                            (cfile, cline, ccol, cmsg, not same_expansion)
                        )
            top_level.extend(curr)

    except KnownException as e:
        return (
            f"Error in update-verify-tests while parsing tool output: {e}",
            None,
        )
    if top_level:
        return update_test_files(top_level, prefix, unparsed_files)
    else:
        return ("no mismatching diagnostics found", None)


# ---------------------------------------------------------------------------
# minimize-verify-tests: merge redundant prefixed expected-* directives
# ---------------------------------------------------------------------------

# Regex to extract `-verify-additional-prefix <prefix>` from a RUN command.
_additional_prefix_re = re.compile(r"-verify-additional-prefix\s+(\S+)")

# Detects that a RUN command involves the diagnostic verifier.  Matches
# `-verify` as a standalone flag (not part of `-verify-additional-prefix`,
# `-verify-ignore-unrelated`, etc.) or known lit substitutions that imply it.
_verify_flag_re = re.compile(
    r"(?:-verify(?![-\w])|%-*target-typecheck-verify-swift\b|"
    r"%-*target-swift-frontend-verify\b)"
)


def _parse_run_lines(raw_lines):
    """Return a list of full RUN-line command strings with continuations
    joined.  Each entry is a single logical RUN command."""
    runs = []
    current = None
    for raw in raw_lines:
        text = raw.rstrip("\n")
        m = re.match(r"^\s*//\s*RUN:\s*(.*)", text)
        if m:
            part = m.group(1)
            if current is not None:
                current += " " + part
            else:
                current = part
            if current.endswith("\\"):
                current = current[:-1]
            else:
                runs.append(current)
                current = None
        else:
            if current is not None:
                runs.append(current)
                current = None
    if current is not None:
        runs.append(current)
    return runs


def _collect_verify_prefixes(raw_lines):
    """Parse every verify-RUN line and return two structures:

    * ``run_prefixes``: a list of ``frozenset`` where each element is the
      set of active prefixes for that verify-RUN line (always includes the
      default ``""``).
    * ``prefix_to_runs``: a dict mapping each prefix to the
      ``frozenset`` of verify-RUN indices where it is active.

    Non-verify RUN lines are silently skipped.
    """
    commands = _parse_run_lines(raw_lines)
    run_prefixes = []
    for cmd in commands:
        if not _verify_flag_re.search(cmd):
            continue
        prefixes = {""}
        for m in _additional_prefix_re.finditer(cmd):
            prefixes.add(m.group(1))
        run_prefixes.append(frozenset(prefixes))

    prefix_to_runs = {}
    for run_idx, pset in enumerate(run_prefixes):
        for p in pset:
            prefix_to_runs.setdefault(p, set()).add(run_idx)
    # Freeze the sets so they are hashable / easily comparable.
    prefix_to_runs = {p: frozenset(s) for p, s in prefix_to_runs.items()}
    return run_prefixes, prefix_to_runs


def _trailing_content(diag):
    """Return the portion of the line content after the ``{{DIAG}}``
    placeholder.  This captures ``{{children:...}}`` blocks and any other
    trailing text that is semantically part of the directive."""
    marker = "{{DIAG}}"
    idx = diag.line.content.find(marker)
    if idx < 0:
        return ""
    return diag.line.content[idx + len(marker):]


# Matches `@-N` or `@+N` that follow an `expected-*` directive prefix
# inside a ``{{children:...}}`` block.
_children_ref_re = re.compile(
    r"(expected-[a-zA-Z0-9-]*(?:note|warning|error|remark)(?:-re)?\s*)@([+-])(\d+)"
)


def _normalized_trailing(diag):
    """Like ``_trailing_content`` but with relative ``@-N`` / ``@+N``
    references inside ``{{children:...}}`` blocks resolved to absolute
    line numbers.  This allows two directives on different lines to be
    recognised as overlapping when their children blocks reference the
    same target lines."""
    trailing = _trailing_content(diag)
    line_n = diag.line.line_n

    def _resolve(m):
        prefix = m.group(1)
        sign = m.group(2)
        offset = int(m.group(3))
        abs_n = line_n + (-offset if sign == "-" else offset)
        return f"{prefix}@={abs_n}"

    return _children_ref_re.sub(_resolve, trailing)


def _overlap_key(diag):
    """Return a hashable key that groups directives which could potentially
    be merged: same target line, same category, same content, same count,
    same regex flag, same fix-it annotations, and same trailing content
    (which includes ``{{children:...}}`` blocks with offsets normalised
    to absolute line numbers).

    For multi-line ``{{children:`` blocks the continuation lines have been
    folded into ``nested_lines``; those are included as a content
    fingerprint so that blocks with different child notes stay separate."""
    nested_key = ()
    if diag.nested_lines:
        parts = []
        for nl in diag.nested_lines:
            nd = nl.diag
            if nd:
                parts.append((nd.absolute_target(), nd.category,
                              nd.diag_content, nd.count, nd.is_re,
                              nd.fixits_raw_str))
        nested_key = tuple(parts)
    return (
        diag.absolute_target(),
        diag.category,
        diag.diag_content,
        diag.count,
        diag.is_re,
        diag.fixits_raw_str,
        _normalized_trailing(diag),
        nested_key,
    )


def _pick_keeper(diags):
    """From a list of mergeable ``Diag`` objects choose the one to keep.

    Preference order:
    1. On the same line as the target (relative offset 0) — avoids ``@+N``.
    2. Closest to the target (smallest absolute relative offset).
    3. Earliest in the file (smallest ``line.line_n``).
    """
    def sort_key(d):
        rel = abs(d.relative_target())
        return (rel != 0, rel, d.line.line_n)

    return min(diags, key=sort_key)


def _find_merge_prefix(prefixes, prefix_to_runs, runs_to_prefixes):
    """Given a list of prefixes, compute their combined RUN-line coverage
    and return a single prefix that covers exactly that set, or ``None``
    if no such prefix exists.  Prefers the default (empty) prefix."""
    coverage = set()
    for p in prefixes:
        rset = prefix_to_runs.get(p)
        if rset is None:
            return None
        coverage |= rset
    candidates = runs_to_prefixes.get(frozenset(coverage))
    if not candidates:
        return None
    if "" in candidates:
        return ""
    return min(candidates, key=lambda p: (len(p), p))


def _nested_diag_key(nd):
    """Overlap key for a directive nested inside an expansion block."""
    return (
        nd.absolute_target(),
        nd.category,
        nd.diag_content,
        nd.count,
        nd.is_re,
        nd.fixits_raw_str,
        _trailing_content(nd),
    )


def _merge_nested_diags(keeper, exp_list, prefix_to_runs, runs_to_prefixes):
    """Merge the directives nested inside the expansion blocks in
    *exp_list*, installing the result as *keeper*'s nested lines.

    Directives from every block in the group are pooled, so one that
    appears in several blocks collapses into a single directive whose
    prefix covers all of their runs.  Directives that appear only once, or
    whose prefixes have no single replacement, are kept as they are.

    Returns True if any directives were actually merged.
    """
    all_nested = []
    for d in exp_list:
        all_nested.extend(d.nested_lines)

    # Group nested diags by overlap key within the expansion.
    nested_groups = {}
    for nl in all_nested:
        nested_groups.setdefault(_nested_diag_key(nl.diag), []).append(nl)

    merged = False
    merged_nested = []
    for nl_list in nested_groups.values():
        if len(nl_list) > 1:
            np = _find_merge_prefix(
                [nl.diag.prefix for nl in nl_list],
                prefix_to_runs, runs_to_prefixes)
            if np is not None:
                nl_list[0].diag.prefix = np
                merged_nested.append(nl_list[0])
                merged = True
                continue
        # Single diag or unmergeable — keep all.
        merged_nested.extend(nl_list)

    if not merged and len(exp_list) == 1:
        # Nothing changed, so leave the block's nested lines alone rather
        # than reordering them gratuitously.
        return False

    # Sort by target line within expansion, then by category for
    # stability.
    merged_nested.sort(
        key=lambda nl: (nl.diag.absolute_target(), nl.diag.category))

    keeper.nested_lines = merged_nested
    for i, nl in enumerate(keeper.nested_lines):
        nl.line_n = i + 1
    return merged


def _merge_expansion_groups(expansion_diags, prefix_to_runs,
                            runs_to_prefixes, lines):
    """Merge expansion blocks that target the same source location.

    Two expansion blocks at the same ``(absolute_target, column)`` are
    combined into one whose prefix covers the union of runs.  Their
    nested directives are individually merged where an appropriate prefix
    exists; non-overlapping nested directives are kept with their original
    prefix.

    Blocks that stay separate — because a location has only one block, or
    because the group's prefixes have no single replacement — still get
    their own contents minimized.
    """
    # Group by (absolute_target, col).
    groups = {}
    for d in expansion_diags:
        key = (d.absolute_target(), d.col())
        groups.setdefault(key, []).append(d)

    changed = False
    for exp_list in groups.values():
        # Find a prefix for the merged expansion block.
        exp_prefix = None
        if len(exp_list) > 1:
            exp_prefix = _find_merge_prefix(
                [d.prefix for d in exp_list], prefix_to_runs,
                runs_to_prefixes)

        if exp_prefix is None:
            # The blocks stay separate, so pooling their contents would
            # move directives from one block into another.  Minimize each
            # block's own contents instead.
            for d in exp_list:
                changed |= _merge_nested_diags(
                    d, [d], prefix_to_runs, runs_to_prefixes)
            continue

        keeper = _pick_keeper(exp_list)
        _merge_nested_diags(keeper, exp_list, prefix_to_runs,
                            runs_to_prefixes)
        keeper.prefix = exp_prefix

        # Remove the other expansion blocks from `lines`.
        for d in exp_list:
            if d is keeper:
                continue
            if d.target is not None:
                d.unset_target()
            remove_line(d.line, lines)
        changed = True

    return changed


def _has_multiline_children_open(diag):
    """Return True if *diag*'s line opens a multi-line ``{{children:``
    block (the block continues on subsequent lines rather than closing
    on the same line)."""
    trailing = _trailing_content(diag)
    # Look for {{children: that is NOT closed by }} on the same line.
    idx = trailing.find("{{children:")
    if idx < 0:
        return False
    after = trailing[idx:]
    # Count braces: the block is multi-line if we never see the matching }}.
    depth = 0
    i = 0
    while i < len(after):
        if after[i:i+2] == "{{":
            depth += 1
            i += 2
        elif after[i:i+2] == "}}":
            depth -= 1
            if depth == 0:
                return False  # closed on the same line
            i += 2
        else:
            i += 1
    return True  # never closed — multi-line


def minimize_verify_test(filename):
    """Read *filename*, merge redundant prefixed expected-* directives where
    a single prefix can replace a set of overlapping ones, and write the
    result back.  Returns an error string on failure, or ``None`` on
    success."""
    with open(filename, "r") as f:
        raw_lines = f.readlines()

    run_prefixes, prefix_to_runs = _collect_verify_prefixes(raw_lines)
    if not run_prefixes:
        return None  # no verify-RUN lines — nothing to do

    # Build the inverse mapping: frozenset-of-runs → list of prefixes that
    # cover exactly that set.
    runs_to_prefixes = {}
    for pfx, rset in prefix_to_runs.items():
        runs_to_prefixes.setdefault(rset, []).append(pfx)

    lines = [Line(line, i + 1) for i, line in enumerate(raw_lines + [""])]
    orig_lines = list(lines)

    # Parse every expected-* directive (all prefixes).
    # Both expansion_context (for expected-expansion blocks) and
    # children_context (for multi-line {{children: blocks) track nesting
    # so that continuation lines get a parent reference and are later
    # folded out of the main line list.
    expansion_context = []
    children_context = []
    for line in lines:
        diag = parse_diag(line, filename, "", all_prefixes=True)
        if diag:
            line.diag = diag
            # Pick the innermost active context.
            parent_ctx = (children_context or expansion_context)
            if isinstance(diag, ExpansionDiagClose):
                if children_context:
                    diag.parent = children_context[-1]
                    children_context.pop()
                elif expansion_context:
                    diag.parent = expansion_context[-1]
                    expansion_context.pop()
            elif diag.category == "expansion":
                if parent_ctx:
                    diag.parent = parent_ctx[-1]
                else:
                    diag.set_target(lines[diag.absolute_target() - 1])
                expansion_context.append(diag)
            else:
                if parent_ctx:
                    diag.parent = parent_ctx[-1]
                    # Notes inside multi-line {{children: blocks still
                    # need their target resolved before folding, so that
                    # absolute_target() works after line renumbering.
                    if children_context:
                        diag.set_target(
                            lines[diag.absolute_target() - 1])
                else:
                    diag.set_target(lines[diag.absolute_target() - 1])
                # Check if this diag opens a multi-line children block.
                if (not parent_ctx
                        and not isinstance(diag, ExpansionDiagClose)
                        and _has_multiline_children_open(diag)):
                    children_context.append(diag)

    # Fold expansion blocks *and* multi-line children blocks so nested
    # lines live inside the parent diag.
    fold_expansions(lines)

    # Separate expansion and regular diags.
    expansion_diags = []
    regular_diags = []
    for line in lines:
        if line.diag and not isinstance(line.diag, ExpansionDiagClose):
            if line.diag.category == "expansion":
                expansion_diags.append(line.diag)
            else:
                regular_diags.append(line.diag)

    changed = False

    # --- Pass 1: merge expansion blocks at the same location. ---
    changed |= _merge_expansion_groups(
        expansion_diags, prefix_to_runs, runs_to_prefixes, lines)

    # --- Pass 2: merge regular (non-expansion) diags. ---
    groups = {}
    for diag in regular_diags:
        key = _overlap_key(diag)
        groups.setdefault(key, []).append(diag)

    for key, diag_list in groups.items():
        if len(diag_list) < 2:
            continue

        merge_prefix = _find_merge_prefix(
            [d.prefix for d in diag_list], prefix_to_runs, runs_to_prefixes)
        if merge_prefix is None:
            continue

        keeper = _pick_keeper(diag_list)
        keeper.prefix = merge_prefix
        for d in diag_list:
            if d is keeper:
                continue
            if d.target is not None:
                d.unset_target()
            remove_line(d.line, lines)
        changed = True

    if changed:
        # Re-insert nested lines for both expansion blocks and multi-line
        # children blocks.
        expand_expansions(lines)
        # Also expand children-block nested lines on non-expansion diags.
        i = 0
        while i < len(lines):
            line = lines[i]
            if (line.diag
                    and not isinstance(line.diag, ExpansionDiagClose)
                    and line.diag.category != "expansion"
                    and line.diag.nested_lines):
                for j, nested in enumerate(
                        line.diag.nested_lines + [line.diag.closer]):
                    nested.line_n = line.line_n + j + 1
                    add_line(nested, lines)
            i += 1
        with open(filename, "w") as f:
            for line in lines:
                f.write(line.render())
    return None
