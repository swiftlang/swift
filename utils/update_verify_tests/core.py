import sys
import re
from codecs import encode, decode

DEBUG = False


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
    ):
        self.prefix = prefix
        self.diag_content = diag_content
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
        self.count = other_diag.count
        self.category = other_diag.category
        self.count = other_diag.count
        other_diag.count = 0

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
        count_s = "" if self.count == 1 else f"{self.count}"
        re_s = "-re" if self.is_re else ""
        if self.whitespace_strings:
            whitespace1_s = self.whitespace_strings[0]
            whitespace2_s = self.whitespace_strings[1]
        else:
            whitespace1_s = " "
            whitespace2_s = ""
        if count_s and not whitespace2_s:
            whitespace2_s = " "  # required to parse correctly
        elif not count_s and whitespace2_s == " ":
            """Don't emit a weird extra space.
            However if the whitespace is something other than the
            standard single space, let it be to avoid disrupting manual formatting.
            """
            whitespace2_s = ""
        col_s = f":{self.col()}" if self.col() else ""
        base_s = f"//{whitespace1_s}expected-{self.prefix}{self.category}{re_s}{line_location_s}{col_s}{whitespace2_s}{count_s}"
        if self.category == "expansion":
            return base_s + "{{"
        else:
            # python trivia: raw strings can't end with a backslash
            escaped_diag_s = self.diag_content.replace("\\", "\\\\")
            return base_s + "{{" + escaped_diag_s + "}}"


class ExpansionDiagClose:
    def __init__(self, whitespace, line):
        self.whitespace = whitespace
        self.line = line
        self.parent = None
        self.category = "closing"

    def render(self):
        return "//" + self.whitespace + "}}"


expected_diag_re = re.compile(
    r"//(\s*)expected-([a-zA-Z-]*)(note|warning|error|remark)(-re)?(@[+-]?\d+)?(:\d+)?(\s*)(\d+)?\{\{(.*)\}\}"
)
expected_expansion_diag_re = re.compile(
    r"//(\s*)expected-([a-zA-Z-]*)(expansion)(-re)?(@[+-]?\d+)(:\d+)(\s*)(\d+)?\{\{(.*)"
)
expected_expansion_close_re = re.compile(r"//(\s*)\}\}")


def parse_diag(line, filename, prefix):
    s = line.content
    ms = expected_diag_re.findall(s)
    matched_re = expected_diag_re
    if not ms:
        ms = expected_expansion_diag_re.findall(s)
        matched_re = expected_expansion_diag_re
    if not ms:
        ms = expected_expansion_close_re.findall(s)
        if not ms:
            return None
        if len(ms) > 1:
            raise KnownException(
                f"multiple closed scopes on line {filename}:{line.line_n}. Aborting due to missing implementation."
            )
        line.content = expected_expansion_close_re.sub("{{DIAG}}", s)
        return ExpansionDiagClose(ms[0], line)
    if len(ms) > 1:
        raise KnownException(
            f"multiple diags on line {filename}:{line.line_n}. Aborting due to missing implementation."
        )
    [
        whitespace1_s,
        check_prefix,
        category_s,
        re_s,
        target_line_s,
        target_col_s,
        whitespace2_s,
        count_s,
        diag_s,
    ] = ms[0]
    if check_prefix != prefix and check_prefix != "":
        return None
    if not target_line_s:
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
    line.content = matched_re.sub("{{DIAG}}", s)

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
        [whitespace1_s, whitespace2_s],
        True,
        [],
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
        whitespace_strings = (
            prev_line.diag.whitespace_strings.copy()
            if prev_line.diag.whitespace_strings
            else None
        )
        if prev_line.diag == nested_context:
            if not whitespace_strings:
                whitespace_strings = [" ", "", ""]
            whitespace_strings[0] += "  "

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


def remove_dead_diags(lines):
    for line in lines.copy():
        if not line.diag:
            continue
        if line.diag.category == "expansion":
            remove_dead_diags(line.diag.nested_lines)
            if line.diag.nested_lines:
                line.diag.count = 1
            else:
                line.diag.count = 0
        if line.diag.count != 0:
            continue
        if line.render() == "":
            remove_line(line, lines)
        else:
            assert line.diag.is_from_source_file
            for other_diag in line.diag.target.targeting_diags:
                if (
                    other_diag.is_from_source_file
                    or other_diag.count == 0
                    or other_diag.category != line.diag.category
                ):
                    continue
                if other_diag.is_re or line.diag.is_re:
                    continue
                line.diag.take(other_diag)
                remove_line(other_diag.line, lines)
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


def find_other_targeting(lines, orig_lines, is_nested, diag_error):
    if is_nested:
        other_diags = [
            line.diag
            for line in lines
            if line.diag
            and error_refers_to_diag(diag_error, line.diag, diag_error.line)
        ]
    else:
        target = orig_lines[diag_error.line - 1]
        other_diags = [
            d
            for d in target.targeting_diags
            if error_refers_to_diag(diag_error, d, target.line_n)
        ]
    return other_diags


def update_lines(
    diag_errors, lines, orig_lines, prefix, filename, nested_context
):
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

    diag_errors.sort(reverse=True, key=lambda diag_error: diag_error.line)
    for diag_error in diag_errors:
        if not isinstance(diag_error, ExtraDiag) and not isinstance(
            diag_error, NestedDiag
        ):
            continue
        other_diags = find_other_targeting(
            lines, orig_lines, bool(nested_context), diag_error
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
                    diag.whitespace_strings[0]
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
            )


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
    for line in lines:
        dprint(f"parsing line {line.render()}")
        diag = parse_diag(line, filename, prefix)
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
        else:
            dprint(f"  no diag")

    fold_expansions(lines)
    update_lines(diag_errors, lines, orig_lines, prefix, filename, None)
    remove_dead_diags(lines)
    expand_expansions(lines)
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

    def __str__(self):
        return f"{self.file}:{self.line}:{self.col}: error unexpected {self.category} produced: {self.content}"


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
            if not "error:" in line:
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
