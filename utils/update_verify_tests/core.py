import sys
import re

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
        return res


class Diag:
    def __init__(
        self,
        prefix,
        diag_content,
        category,
        parsed_target_line_n,
        line_is_absolute,
        count,
        line,
        is_re,
        whitespace_strings,
        is_from_source_file,
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
            whitespace3_s = self.whitespace_strings[2]
        else:
            whitespace1_s = " "
            whitespace2_s = ""
            whitespace3_s = ""
        if count_s and not whitespace2_s:
            whitespace2_s = " "  # required to parse correctly
        elif not count_s and whitespace2_s == " ":
            """Don't emit a weird extra space.
            However if the whitespace is something other than the
            standard single space, let it be to avoid disrupting manual formatting.
            The existence of a non-empty whitespace2_s implies this was parsed with
            a count > 1 and then decremented, otherwise this whitespace would have
            been parsed as whitespace3_s.
            """
            whitespace2_s = ""
        return f"//{whitespace1_s}expected-{self.prefix}{self.category}{re_s}{line_location_s}{whitespace2_s}{count_s}{whitespace3_s}{{{{{self.diag_content}}}}}"


expected_diag_re = re.compile(
    r"//(\s*)expected-([a-zA-Z-]*)(note|warning|error)(-re)?(@[+-]?\d+)?(?:(\s*)(\d+))?(\s*)\{\{(.*)\}\}"
)


def parse_diag(line, filename, prefix):
    s = line.content
    ms = expected_diag_re.findall(s)
    if not ms:
        return None
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
        whitespace2_s,
        count_s,
        whitespace3_s,
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
    count = int(count_s) if count_s else 1
    line.content = expected_diag_re.sub("{{DIAG}}", s)

    return Diag(
        check_prefix,
        diag_s,
        category_s,
        target_line_n,
        is_absolute,
        count,
        line,
        bool(re_s),
        [whitespace1_s, whitespace2_s, whitespace3_s],
        True,
    )


def add_line(new_line, lines):
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


def add_diag(orig_line_n, diag_s, diag_category, lines, orig_lines, prefix):
    line_n = orig_line_n_to_new_line_n(orig_line_n, orig_lines)
    target = lines[line_n - 1]
    for other in target.targeting_diags:
        if other.is_re:
            raise KnownException(
                "mismatching diag on line with regex matcher. Skipping due to missing implementation"
            )
    reverse = (
        True
        if [other for other in target.targeting_diags if other.relative_target() < 0]
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

    new_line = Line(get_indent(prev_line.content) + "{{DIAG}}\n", new_line_n)
    add_line(new_line, lines)

    whitespace_strings = prev_line.diag.whitespace_strings if prev_line.diag else None
    new_diag = Diag(
        prefix,
        diag_s,
        diag_category,
        total_offset,
        False,
        1,
        new_line,
        False,
        whitespace_strings,
        False,
    )
    new_line.diag = new_diag
    new_diag.set_target(target)


def remove_dead_diags(lines):
    for line in lines:
        if not line.diag or line.diag.count != 0:
            continue
        if line.render() == "":
            remove_line(line, lines)
        else:
            assert line.diag.is_from_source_file
            for other_diag in line.targeting_diags:
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


def update_test_file(filename, diag_errors, prefix, updated_test_files):
    dprint(f"updating test file {filename}")
    if filename in updated_test_files:
        raise KnownException(f"{filename} already updated, but got new output")
    else:
        updated_test_files.add(filename)
    with open(filename, "r") as f:
        lines = [Line(line, i + 1) for i, line in enumerate(f.readlines() + [''])]
    orig_lines = list(lines)

    for line in lines:
        diag = parse_diag(line, filename, prefix)
        if diag:
            line.diag = diag
            diag.set_target(lines[diag.absolute_target() - 1])

    for diag_error in diag_errors:
        if not isinstance(diag_error, NotFoundDiag):
            continue
        # this is a diagnostic expected but not seen
        line_n = diag_error.line
        assert lines[line_n - 1].diag
        if not lines[line_n - 1].diag or diag_error.content != lines[line_n - 1].diag.diag_content:
            raise KnownException(
                f"{filename}:{line_n} - found diag {lines[line_n - 1].diag.diag_content} but expected {diag_error.content}"
            )
        if diag_error.category != lines[line_n - 1].diag.category:
            raise KnownException(
                f"{filename}:{line_n} - found {lines[line_n - 1].diag.category} diag but expected {diag_error.category}"
            )
        lines[line_n - 1].diag.decrement_count()

    diag_errors.sort(reverse=True, key=lambda t: t.line)
    for diag_error in diag_errors:
        if not isinstance(diag_error, ExtraDiag):
            continue
        line_n = diag_error.line
        target = orig_lines[line_n - 1]
        other_diags = [
            d
            for d in target.targeting_diags
            if d.diag_content == diag_error.content and d.category == diag_error.category
        ]
        other_diag = other_diags[0] if other_diags else None
        if other_diag:
            other_diag.increment_count()
        else:
            add_diag(line_n, diag_error.content, diag_error.category, lines, orig_lines, diag_error.prefix)
    remove_dead_diags(lines)
    with open(filename, "w") as f:
        for line in lines:
            f.write(line.render())


def update_test_files(errors, prefix):
    errors_by_file = {}
    for error in errors:
        filename = error.file
        if filename not in errors_by_file:
            errors_by_file[filename] = []
        errors_by_file[filename].append(error)
    updated_test_files = set()
    for filename, diag_errors in errors_by_file.items():
        try:
            update_test_file(filename, diag_errors, prefix, updated_test_files)
        except KnownException as e:
            return f"Error in update-verify-tests while updating {filename}: {e}"
    updated_files = list(updated_test_files)
    assert updated_files
    if len(updated_files) == 1:
        return f"updated file {updated_files[0]}"
    updated_files_s = "\n\t".join(updated_files)
    return "updated files:\n\t{updated_files_s}"


"""
ex:
test.swift:2:6: error: expected error not produced
  // expected-error@+1{{asdf}}
~~~~~^~~~~~~~~~~~~~~~~~~~~~~~~
"""
diag_error_re = re.compile(r"(\S+):(\d+):(\d+): error: expected (\S+) not produced")


"""
ex:
test.swift:2:3: error: unexpected error produced: cannot find 'a' in scope
  a = 2
  ^
"""
diag_error_re2 = re.compile(r"(\S+):(\d+):(\d+): error: unexpected (\S+) produced: (.*)")


"""
ex:
test.swift:2:43: error: incorrect message found
  bar = 2  // expected-error{{asdf}}
                              ^~~~
                              cannot find 'bar' in scope
"""
diag_error_re3 = re.compile(r"(\S+):(\d+):(\d+): error: incorrect message found")


"""
ex:
test.swift:2:15: error: expected warning, not error
  // expected-warning@+1{{cannot find 'bar' in scope}}
              ^~~~~~~
              error
"""
diag_error_re4 = re.compile(r"(\S+):(\d+):(\d+): error: expected (\S+), not (\S+)")


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


def check_expectations(tool_output, prefix):
    """
    The entry point function.
    Called by the stand-alone update-verify-tests.py as well as litplugin.py.
    """
    curr = []
    try:
        i = 0
        while i < len(tool_output):
            line = tool_output[i].strip()

            if not "error:" in line:
                pass
            elif m := diag_error_re.match(line):
                diag = parse_diag(Line(tool_output[i+1], int(m.group(2))), m.group(1), prefix)
                i += 2
                curr.append(NotFoundDiag(m.group(1), int(m.group(2)), int(m.group(3)), m.group(4), diag.diag_content, diag.prefix))
            elif m := diag_error_re2.match(line):
                curr.append(ExtraDiag(m.group(1), int(m.group(2)), int(m.group(3)), m.group(4), m.group(5), prefix))
                i += 2
            # Create two mirroring mismatches when the compiler reports that the category or diagnostic is incorrect.
            # This makes it easier to handle cases where the same diagnostic is mentioned both in an incorrect message/category
            # diagnostic, as well as in an error not produced diagnostic. This can happen for things like 'expected-error 2{{foo}}'
            # if only one diagnostic is emitted on that line, and the content of that diagnostic is actually 'bar'.
            elif m := diag_error_re3.match(line):
                diag = parse_diag(Line(tool_output[i+1], int(m.group(2))), m.group(1), prefix)
                curr.append(NotFoundDiag(m.group(1), int(m.group(2)), int(m.group(3)), diag.category, diag.diag_content, diag.prefix))
                curr.append(ExtraDiag(m.group(1), diag.absolute_target(), int(m.group(3)), diag.category, tool_output[i+3].strip(), diag.prefix))
                i += 3
            elif m := diag_error_re4.match(line):
                diag = parse_diag(Line(tool_output[i+1], int(m.group(2))), m.group(1), prefix)
                assert diag.category == m.group(4)
                assert tool_output[i+3].strip() == m.group(5)
                curr.append(NotFoundDiag(m.group(1), int(m.group(2)), int(m.group(3)), diag.category, diag.diag_content, diag.prefix))
                curr.append(ExtraDiag(m.group(1), diag.absolute_target(), int(m.group(3)), m.group(5), diag.diag_content, diag.prefix))
                i += 3
            else:
                dprint("no match")
                dprint(line.strip())
            i += 1

    except KnownException as e:
        return (1, f"Error in update-verify-tests while parsing tool output: {e}")
    if curr:
        return (0, update_test_files(curr, prefix))
    else:
        return (1, "no mismatching diagnostics found")

