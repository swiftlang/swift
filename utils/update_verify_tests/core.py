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


diag_error_re = re.compile(r"File (\S+) Line (\d+): (.+)")
diag_error_re2 = re.compile(r"File \S+ Line \d+ \(directive at (\S+):(\d+)\): (.+)")


def parse_diag_error(s):
    m = diag_error_re2.match(s)
    if not m:
        m = diag_error_re.match(s)
    if not m:
        return None
    return (m.group(1), int(m.group(2)), m.group(3))


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
        return f"//{whitespace1_s}{self.prefix}-{self.category}{re_s}{line_location_s}{whitespace2_s}{count_s}{whitespace3_s}{{{{{self.diag_content}}}}}"


expected_diag_re = re.compile(
    r"//(\s*)([a-zA-Z]+)-(note|warning|error)(-re)?(@[+-]?\d+)?(?:(\s*)(\d+))?(\s*)\{\{(.*)\}\}"
)


def parse_diag(line, filename, lines, prefix):
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
    if check_prefix != prefix:
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
        prefix,
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


def has_live_diags(lines):
    for line in lines:
        if line.diag and line.diag.count > 0:
            return True
    return False


def get_expected_no_diags_line_n(lines, prefix):
    for line in lines:
        if f"{prefix}-no-diagnostics" in line.content:
            return line.line_n
    return None


def update_test_file(filename, diag_errors, prefix, updated_test_files):
    dprint(f"updating test file {filename}")
    if filename in updated_test_files:
        raise KnownException(f"{filename} already updated, but got new output")
    else:
        updated_test_files.add(filename)
    with open(filename, "r") as f:
        lines = [Line(line, i + 1) for i, line in enumerate(f.readlines())]
    orig_lines = list(lines)
    expected_no_diags_line_n = get_expected_no_diags_line_n(orig_lines, prefix)

    for line in lines:
        diag = parse_diag(line, filename, lines, prefix)
        if diag:
            line.diag = diag
            diag.set_target(lines[diag.absolute_target() - 1])

    for line_n, diag_s, diag_category, seen in diag_errors:
        if seen:
            continue
        # this is a diagnostic expected but not seen
        assert lines[line_n - 1].diag
        if diag_s != lines[line_n - 1].diag.diag_content:
            raise KnownException(
                f"{filename}:{line_n} - found diag {lines[line_n - 1].diag.diag_content} but expected {diag_s}"
            )
        if diag_category != lines[line_n - 1].diag.category:
            raise KnownException(
                f"{filename}:{line_n} - found {lines[line_n - 1].diag.category} diag but expected {diag_category}"
            )
        lines[line_n - 1].diag.decrement_count()
    diag_errors_left = []
    diag_errors.sort(reverse=True, key=lambda t: t[0])
    for line_n, diag_s, diag_category, seen in diag_errors:
        if not seen:
            continue
        target = orig_lines[line_n - 1]
        other_diags = [
            d
            for d in target.targeting_diags
            if d.diag_content == diag_s and d.category == diag_category
        ]
        other_diag = other_diags[0] if other_diags else None
        if other_diag:
            other_diag.increment_count()
        else:
            add_diag(line_n, diag_s, diag_category, lines, orig_lines, prefix)
    remove_dead_diags(lines)
    has_diags = has_live_diags(lines)
    with open(filename, "w") as f:
        if not has_diags and expected_no_diags_line_n is None:
            f.write("// expected-no-diagnostics\n")
        for line in lines:
            if has_diags and line.line_n == expected_no_diags_line_n:
                continue
            f.write(line.render())


def update_test_files(errors, prefix):
    errors_by_file = {}
    for (filename, line, diag_s), (diag_category, seen) in errors:
        if filename not in errors_by_file:
            errors_by_file[filename] = []
        errors_by_file[filename].append((line, diag_s, diag_category, seen))
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


def check_expectations(tool_output, prefix):
    """
    The entry point function.
    Called by the stand-alone update-verify-tests.py as well as litplugin.py.
    """
    curr = []
    curr_category = None
    try:
        for line in tool_output:
            if line.startswith("error: "):
                curr_category = parse_error_category(line[len("error: ") :], prefix)
                continue

            diag_error = parse_diag_error(line.strip())
            if diag_error:
                curr.append((diag_error, curr_category))
            else:
                dprint("no match")
                dprint(line.strip())
    except KnownException as e:
        return f"Error in update-verify-tests while parsing tool output: {e}"
    if curr:
        return update_test_files(curr, prefix)
    else:
        return "no mismatching diagnostics found"

