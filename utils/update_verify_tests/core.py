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
        self.diag = None
        self.line_n = line_n
        self.targeting_diags = []

    def update_line_n(self, n):
        self.line_n = n

    def render(self):
        if not self.diag:
            return self.content
        assert "{{DIAG}}" in self.content
        res = self.content.replace("{{DIAG}}", self.diag.render())
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

    def decrement_count(self):
        self.count -= 1
        assert self.count >= 0

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
        base_s = (
            f"//{ws.slash}expected-{self.prefix}{self.category}{re_s}"
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


def parse_diag(line, filename, prefix, all_prefixes=False):
    s = line.content
    matches = list(expected_diag_re.finditer(s))
    matched_re = expected_diag_re
    if not matches:
        matches = list(expected_expansion_diag_re.finditer(s))
        matched_re = expected_expansion_diag_re
    if not matches:
        ms = expected_expansion_close_re.findall(s)
        if not ms:
            return None
        if len(ms) > 1:
            raise KnownException(
                f"multiple closed scopes on line {filename}:{line.line_n}. Aborting due to missing implementation."
            )
        line.content = expected_expansion_close_re.sub("{{DIAG}}", s)
        return ExpansionDiagClose(ms[0], line)
    if len(matches) > 1:
        raise KnownException(
            f"multiple diags on line {filename}:{line.line_n}. Aborting due to missing implementation."
        )
    m = matches[0]
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
        return None
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
    if matched_re is expected_diag_re:
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
    line.content = (
        s[: m.start()] + "{{DIAG}}" + s[m.end() + len(fixits_raw_str) :]
    )

    unescaped_diag_s = decode(
        encode(diag_s, "utf-8", "backslashreplace"), "unicode-escape"
    )
    return Diag(
        check_prefix,
        unescaped_diag_s,
        category_s,
        target_line_n,
        is_absolute,
        col,
        count,
        line,
        bool(re_s),
        Whitespace(slash=ws_slash, at=ws_at, count=ws_count, braces=ws_braces),
        True,
        [],
        diag_content_raw=diag_s,
        original_count_str=count_s if count_s else None,
        fixits_raw_str=fixits_raw_str,
        had_none_fixit_marker=has_none_marker,
        preserved_markers=preserved_markers,
        had_absolute_line_in_source=had_absolute_line,
    )


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

        prev_line, total_offset, new_line_n = infer_line_context(target, line_n)
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
        if not line.diag:
            continue
        if getattr(line.diag, "children", None) or (
            getattr(line.diag, "children_closer", None) is not None
        ):
            _collapse_dead_children(line.diag, prefix)
        if line.diag.category == "expansion":
            if not line.diag.prefix or line.diag.prefix == prefix:
                # Whether the verifier already reported this expansion as
                # missing (parent count was decremented to 0 in update_lines).
                was_reported_missing = line.diag.count == 0
                remove_dead_diags(line.diag.nested_lines, prefix)
                if (
                    was_reported_missing
                    and _split_dead_expansion_for_foreign_prefixes(
                        line, lines, prefix
                    )
                ):
                    # The dead expansion has been replaced with one new
                    # expansion directive per foreign prefix. Drop the original
                    # so the cleanup at the bottom of the loop removes it.
                    line.diag.nested_lines = []
                    line.diag.closer = None
                    line.diag.count = 0
                elif line.diag.nested_lines:
                    line.diag.count = 1
                else:
                    line.diag.count = 0
        if line.diag.count != 0:
            continue
        # Try absorbing a same-category sibling first so the dead diag's
        # formatting (whitespace, original_count_str) survives a content
        # rewrite. Nested expansion diags have no target, so skip.
        took = False
        if line.diag.target is not None and not getattr(
            line.diag, "is_child_note", False
        ):
            for other_diag in line.diag.target.targeting_diags:
                if (
                    other_diag.is_from_source_file
                    or other_diag.count == 0
                    or other_diag.category != line.diag.category
                    or getattr(other_diag, "is_child_note", False)
                ):
                    continue
                if other_diag.is_re or line.diag.is_re:
                    continue
                assert line.diag.is_from_source_file
                line.diag.take(other_diag)
                remove_line(other_diag.line, lines)
                took = True
                break
        if took:
            continue
        # Even if take() didn't merge (e.g. because the synthesized sibling
        # has a different category, as in the wrong-category-with-fix-it
        # case), transfer the dead diag's fix-it state to a live sibling on
        # the same target. The fix-it was reported by the verifier against
        # this source location, so it logically belongs to whichever diag
        # ends up rendering at this location.
        if (
            line.diag.actual_fixits is not None
            and line.diag.target is not None
        ):
            for other_diag in line.diag.target.targeting_diags:
                if (
                    other_diag is line.diag
                    or other_diag.is_from_source_file
                    or other_diag.count == 0
                    or other_diag.actual_fixits is not None
                    or getattr(other_diag, "is_child_note", False)
                ):
                    continue
                other_diag.actual_fixits = line.diag.actual_fixits
                other_diag.had_none_fixit_marker = (
                    line.diag.had_none_fixit_marker
                )
                other_diag.preserved_markers = line.diag.preserved_markers
                other_diag.had_absolute_line_in_source = (
                    line.diag.had_absolute_line_in_source
                )
                break
        if line.render() == "":
            remove_line(line, lines)


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
    if line.diag is not None:
        parent = getattr(line.diag, "child_of", None)
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
        d = line.diag
        if d is None or not getattr(d, "children", None):
            i += 1
            continue
        block = list(d.children)
        if d.children_closer is not None:
            block.append(d.children_closer)
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
            line.diag
            for line in lines
            if line.diag
            and (not line.diag.prefix or line.diag.prefix == prefix)
            and error_refers_to_diag(diag_error, line.diag, diag_error.line)
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
        assert line.diag or nested_context
        if not line.diag or diag_error.content != line.diag.diag_content:
            raise KnownException(
                f"{filename}:{line_n} - found diag {line.diag.diag_content} but expected {diag_error.content}"
            )
        if diag_error.category != line.diag.category:
            raise KnownException(
                f"{filename}:{line_n} - found {line.diag.category} diag but expected {diag_error.category}"
            )
        line.diag.decrement_count()

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
        if not line.diag:
            raise KnownException(
                f"{filename}:{line_n} - fix-it mismatch reported, but no expected-* directive parsed on that line"
            )
        bucket = fixit_errors_by_diag.setdefault(id(line.diag), (line.diag, []))
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

    diag_errors.sort(reverse=True, key=lambda diag_error: diag_error.line)
    for diag_error in diag_errors:
        if not isinstance(diag_error, ExtraDiag) and not isinstance(
            diag_error, NestedDiag
        ):
            continue
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
            )
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
        parent_diag = parent_line.diag
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
        parent_diag = orig_lines[diag_error.line - 1].diag
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
    for line in lines:
        dprint(f"parsing line {line.render()}")
        diag = parse_diag(line, filename, prefix, all_prefixes=True)
        if children_context is not None:
            # Inside a `{{children:...}}` block: route the closer and the
            # child-note (or literal) lines to the owning parent rather than
            # treating them as top-level diags/expansions.
            if diag and diag.category == "closing":
                line.diag = diag
                diag.child_of = children_context
                children_context = None
            elif diag:
                line.diag = diag
                diag.is_child_note = True
                diag.child_of = children_context
            elif line.content.lstrip().startswith("}}"):
                # A `}}`-first line that didn't parse as a `// }}` closer
                # (e.g. a C-comment `}}*/` block terminator): treat it as a
                # literal closer so we don't run past the end of the block.
                line._children_literal = children_context
                children_context = None
            else:
                line._children_literal = children_context
            continue
        if diag:
            dprint(f"  parsed diag {diag.render()}")
            line.diag = diag
            if expansion_context:
                diag.parent = expansion_context[-1]
            else:
                diag.set_target(lines[diag.absolute_target() - 1])
            if diag.category == "expansion":
                expansion_context.append(diag)
            elif diag.category == "closing":
                expansion_context.pop()
            elif _opens_children_block(line.content):
                children_context = diag
        else:
            dprint(f"  no diag")

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
test.swift:12:14: note: in expansion from here
func foo() {}
             ^
"""
diag_expansion_note_re = re.compile(
    r"(\S+):(\d+):(\d+): note: in expansion from here"
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
    def __init__(self, file, line, col, nested):
        self.file = file
        self.line = line
        self.col = col
        self.category = "expansion"
        self.content = None
        self.nested = nested
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
                    Line(extra_lines[0], int(m.group(2))), m.group(1), prefix
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
                    Line(extra_lines[0], int(m.group(2))), m.group(1), prefix
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
                    Line(extra_lines[0], int(m.group(2))), m.group(1), prefix
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
                    NestedDiag(m.group(1), int(m.group(2)), int(m.group(3)), e)
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
