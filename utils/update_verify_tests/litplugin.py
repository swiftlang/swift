import os
import shlex
import pathlib
from update_verify_tests.core import check_expectations

"""
This file provides the `uvt_lit_plugin` function, which is invoked on failed RUN lines when lit is executed with --update-tests.
It checks whether the failed command is a swift compiler invocation with the `-verify` flag and analyses the output to try to
repair the failed test. If the updated file was originally created by `split-file` it updates the corresponding slice in the source file.
"""


class SplitFileTarget:
    def __init__(self, slice_start_idx, test_path, lines, name):
        self.slice_start_idx = slice_start_idx
        self.test_path = test_path
        self.lines = lines
        self.name = name

    def copyFrom(self, source):
        lines_before = self.lines[: self.slice_start_idx + 1]
        self.lines = self.lines[self.slice_start_idx + 1 :]
        slice_end_idx = None
        for i, l in enumerate(self.lines):
            if SplitFileTarget._get_split_line_path(l) != None:
                slice_end_idx = i
                break
        if slice_end_idx is not None:
            lines_after = self.lines[slice_end_idx:]
        else:
            lines_after = []
        with open(source, "r") as f:
            new_lines = lines_before + f.readlines() + lines_after
        with open(self.test_path, "w") as f:
            for l in new_lines:
                f.write(l)

    def __str__(self):
        return f"slice {self.name} in {self.test_path}"

    @staticmethod
    def get_target_dir(commands, test_path):
        # posix=True breaks Windows paths because \ is treated as an escaping character
        for cmd in commands:
            split = shlex.split(cmd, posix=False)
            if "split-file" not in split:
                continue
            start_idx = split.index("split-file")
            split = split[start_idx:]
            if len(split) < 3:
                continue
            p = unquote(split[1].strip())
            if not test_path.samefile(p):
                continue
            return unquote(split[2].strip())
        return None

    @staticmethod
    def create(path, commands, test_path, target_dir):
        path = pathlib.Path(path)
        with open(test_path, "r") as f:
            lines = f.readlines()
        for i, l in enumerate(lines):
            p = SplitFileTarget._get_split_line_path(l)
            if p and path.samefile(os.path.join(target_dir, p)):
                idx = i
                break
        else:
            return None
        return SplitFileTarget(idx, test_path, lines, p)

    @staticmethod
    def _get_split_line_path(l):
        if len(l) < 6:
            return None
        if l.startswith("//"):
            l = l[2:]
        else:
            l = l[1:]
        if l.startswith("--- "):
            l = l[4:]
        else:
            return None
        return l.rstrip()


def unquote(s):
    if len(s) > 1 and s[0] == s[-1] and (s[0] == '"' or s[0] == "'"):
        return s[1:-1]
    return s


def propagate_split_files(test_path, updated_files, commands):
    test_path = pathlib.Path(test_path)
    split_target_dir = SplitFileTarget.get_target_dir(commands, test_path)
    if not split_target_dir:
        return updated_files

    new = []
    for file in updated_files:
        target = SplitFileTarget.create(file, commands, test_path, split_target_dir)
        if target:
            target.copyFrom(file)
            new.append(str(target))
        else:
            new.append(file)
    return new


def uvt_lit_plugin(result, test, commands):
    if (
        not any(e.endswith("swift-frontend") for e in result.command.args)
        or not "-verify" in result.command.args
    ):
        return None

    prefix = ""
    for i, arg in enumerate(result.command.args):
        if arg == "-verify-additional-prefix":
            if i + 1 >= len(result.command.args):
                return None
            if prefix:
                # can only handle at most 1 additional prefix at the moment
                return None
            prefix = result.command.args[i + 1]

    (err, updated_files) = check_expectations(result.stderr.split("\n"), prefix)
    if err:
        return err

    updated_files = propagate_split_files(test.getFilePath(), updated_files, commands)

    if len(updated_files) > 1:
        return "\n\t".join(["updated files:"] + updated_files)
    return f"updated file: {updated_files[0]}"
