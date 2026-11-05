#!/usr/bin/env python3
# utils/astgen-split-expectations.py - Split -verify expectations by parser
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""Rewrite a `-verify` test so it passes under both the C++ parser and ASTGen.

Where the two parsers disagree about a diagnostic, the test needs separate
expectations: `expected-cparser-error` is checked only when the C++ parser is in
use, `expected-astgen-error` only when ASTGen is, and plain `expected-error`
applies to both (see test/lit.cfg).

This tool runs the test under ASTGen, reads the verifier's complaints, and
rewrites the expectations:

  * an expectation ASTGen does not satisfy becomes `expected-cparser-*`;
  * a diagnostic ASTGen produces that nothing expected is added as
    `expected-astgen-*` on its line;
  * an expectation whose message (or fix-its) differs is split into a
    `cparser` copy and an `astgen` copy.

It repeats until the test passes in both modes or stops making progress. The
C++ parser run is a precondition: a test that already fails there is left
alone.

Splitting expectations makes the test pass; it does not make the two parsers
agree. The report distinguishes the cases worth a look:

  * "ASTGen is silent"  -- the C++ parser diagnoses something ASTGen does not,
    so ASTGen may accept code the C++ parser rejects. These are the ones to
    investigate.
  * "ASTGen only"       -- ASTGen diagnoses something the C++ parser does not.
  * "reworded"          -- both diagnose the line, with different text.

Usage:
  utils/astgen-split-expectations.py [options] TEST...

TEST is a path, absolute or relative to the test/ directory.

Options:
  --build-dir DIR     the Swift build directory containing test-*/ (default:
                      the most recent one next to the source checkout)
  --dry-run           show the rewritten expectations without writing them
  --summary           with --dry-run, print the report but not the diff
  --max-iterations N  give up after N rewrite rounds (default 4)
  --force             run even if the test fails with the C++ parser
  --keep-partial      keep the rewritten test even if it still fails
"""
import argparse
import difflib
import fcntl
import glob
import os
import re
import subprocess
import sys

SRC_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TEST_DIR = os.path.join(SRC_ROOT, "test")

KINDS = ("error", "warning", "note", "remark")

LOCK_PATH = os.path.join(os.environ.get("TMPDIR", "/tmp"), "astgen-split-expectations.lock")

# Tests may contain bytes that are not valid UTF-8 on purpose.
FILE_MODE = dict(encoding="utf-8", errors="surrogateescape", newline="")

# `expected-[prefix-]error[-re][@+N] [count] {{message}} {{fix-it}}...`
DIRECTIVE_RE = re.compile(
    r"expected-(?:(?P<prefix>[A-Za-z0-9_]+)-)?(?P<kind>error|warning|note|remark)"
    r"(?P<re>-re)?"
    r"(?:\s*@(?P<offset>[+-]?\d+|:\d+))?"
    r"(?:\s+(?P<count>\d+|\*))?"
    r"\s*\{\{")


class Directive:
    """One `expected-*` directive on a source line (0-based `line`)."""

    def __init__(self, line, start, end, match, message, message_col, fixits):
        self.line = line
        self.start = start      # index of "expected-"
        self.end = end          # index just past the last "}}"
        self.prefix = match.group("prefix")
        self.kind = match.group("kind")
        self.re = match.group("re") or ""
        self.offset = match.group("offset")
        self.count = match.group("count")
        self.message = message
        self.message_col = message_col      # 1-based column of the message text
        self.fixits = fixits                # [(1-based col, text)]

    def target_line(self):
        """The 0-based line this directive expects a diagnostic on."""
        if self.offset is None:
            return self.line
        if self.offset.startswith(":"):
            return int(self.offset[1:]) - 1
        return self.line + int(self.offset)

    def with_prefix(self, text, prefix):
        """`text`, the source of this directive, with `prefix` added."""
        return text.replace("expected-", "expected-%s-" % prefix, 1)

    def render(self, prefix, message=None, fixits=None):
        text = "expected-"
        if prefix:
            text += prefix + "-"
        text += self.kind + self.re
        if self.offset is not None:
            text += "@" + self.offset
        if self.count is not None:
            text += " " + self.count
        text += " {{" + (self.message if message is None else message) + "}}"
        for _, fixit in (self.fixits if fixits is None else fixits):
            text += "{{" + fixit + "}}"
        return text


def parse_directives(lines):
    result = []
    for i, text in enumerate(lines):
        pos = 0
        while True:
            m = DIRECTIVE_RE.search(text, pos)
            if not m:
                break
            msg_start = m.end()
            close = text.find("}}", msg_start)
            if close < 0:
                break
            message = text[msg_start:close]
            end = close + 2
            fixits = []
            while True:
                fm = re.compile(r"\s*\{\{").match(text, end)
                if not fm:
                    break
                fclose = text.find("}}", fm.end())
                if fclose < 0:
                    break
                # The verifier reports a fix-it at its opening braces.
                fixits.append((fm.end() - 1, text[fm.end():fclose]))
                end = fclose + 2
            result.append(Directive(i, m.start(), end, m, message,
                                    msg_start + 1, fixits))
            pos = end
    return result


def lines_not_safe_for_comments(lines):
    """0-based indices of lines where appending `// ...` would change the
    program: inside a multi-line string literal or a block comment."""
    unsafe = set()
    in_string = False
    in_block = 0
    for i, text in enumerate(lines):
        if in_string or in_block:
            unsafe.add(i)
        j = 0
        while j < len(text):
            if in_string:
                k = text.find('"""', j)
                if k < 0:
                    break
                in_string = False
                j = k + 3
                continue
            if in_block:
                k = text.find("*/", j)
                if k < 0:
                    break
                in_block -= 1
                j = k + 2
                continue
            if text.startswith("//", j):
                break
            if text.startswith("/*", j):
                in_block += 1
                j += 2
                continue
            if text.startswith('"""', j):
                in_string = True
                j += 3
                continue
            if text[j] == '"':
                # Skip a single-line string literal.
                j += 1
                while j < len(text) and text[j] != '"':
                    j += 2 if text[j] == "\\" else 1
                j += 1
                continue
            j += 1
        if in_string:
            unsafe.add(i)
    return unsafe


class Failure:
    def __init__(self, line, col, kind, detail, actual):
        self.line = line        # 0-based
        self.col = col          # 1-based
        self.kind = kind        # see parse_lit_output
        self.detail = detail    # diagnostic kind, or fix-it text
        self.actual = actual    # actual message


def parse_lit_output(output, test_path):
    """Parse the verifier's complaints about `test_path` out of lit's output."""
    failures = []
    other = []
    lines = [re.sub(r"^# \| ?", "", l.rstrip("\n")) for l in output.splitlines()
             if l.startswith("# |")]
    header_re = re.compile(r"^(?P<file>.*?):(?P<line>\d+):(?P<col>\d+): error: (?P<what>.*)$")
    i = 0
    while i < len(lines):
        m = header_re.match(lines[i])
        if not m:
            i += 1
            continue
        what = m.group("what")
        is_ours = os.path.abspath(m.group("file")) == os.path.abspath(test_path)
        line = int(m.group("line")) - 1
        col = int(m.group("col"))
        tail = lines[i + 3].strip() if i + 3 < len(lines) else ""
        consumed = 3
        failure = None
        mm = re.match(r"expected (error|warning|note|remark) not produced$", what)
        if mm:
            failure = Failure(line, col, "missing", mm.group(1), None)
        mm = re.match(r"unexpected (error|warning|note|remark) produced: (.*)$", what)
        if mm:
            failure = Failure(line, col, "unexpected", mm.group(1), mm.group(2))
        if what == "incorrect message found":
            failure = Failure(line, col, "incorrect", None, tail)
            consumed = 4
        mm = re.match(r"expected fix-it not seen(?:; actual fix-its? seen: (.*))?$", what)
        if mm:
            fixits = re.findall(r"\{\{(.*?)\}\}", mm.group(1) or "")
            failure = Failure(line, col, "fixit", None, fixits)
        if failure is not None:
            if is_ours:
                failures.append(failure)
            else:
                other.append(what)
        i += consumed if failure is not None else 1
    return failures, other


def find_build_dir():
    """The swift-<host> build directory with the most recently built compiler."""
    pattern = os.path.join(os.path.dirname(SRC_ROOT), "build", "*", "swift-*", "bin", "swift-frontend")
    candidates = glob.glob(pattern)
    if not candidates:
        sys.exit("error: no build directory found; pass --build-dir")
    frontend = max(candidates, key=os.path.getmtime)
    return os.path.dirname(os.path.dirname(frontend))


def find_lit(build_dir):
    candidates = glob.glob(os.path.join(os.path.dirname(build_dir), "llvm-*", "bin", "llvm-lit"))
    if not candidates:
        sys.exit("error: no llvm-lit found next to the build directory")
    return max(candidates, key=os.path.getmtime)


def test_dir_for(build_dir):
    """swift-macosx-arm64 builds its tests in test-macosx-arm64."""
    host = os.path.basename(build_dir)[len("swift-"):]
    path = os.path.join(build_dir, "test-" + host)
    if not os.path.isdir(path):
        sys.exit("error: no test directory %s; has the test target been configured?" % path)
    return path


def run_lit(build_dir, lit, rel_test, mode):
    env = dict(os.environ)
    # lit deletes the module cache on startup unless told not to, which would
    # break two instances of this tool running at once.
    args = [lit, "-j1", "-v", "--param", "keep_modulecache=1",
            "--filter", re.escape(rel_test) + "$"]
    if mode == "astgen":
        args += ["--param", "swift_parser_astgen=1"]
    else:
        env["SWIFT_FRONTEND_TEST_OPTIONS"] = "-disable-experimental-feature ParserASTGen"
    args.append(test_dir_for(build_dir))
    # Two lit runs at once corrupt each other's timing data, so take turns.
    with open(LOCK_PATH, "w") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        p = subprocess.run(args, capture_output=True, text=True, errors="replace", env=env, cwd=build_dir)
    out = p.stdout + p.stderr
    if re.search(r"^\s+Unsupported\s+:", out, re.M) and p.returncode == 0:
        return None, out
    return p.returncode == 0, out


class Report:
    def __init__(self):
        self.silent = []        # (line, kind, message): ASTGen is silent
        self.astgen_only = []   # (line, kind, message)
        self.reworded = []      # (line, kind, cparser, astgen)
        self.manual = []        # things the tool could not place


def rewrite(path, lines, failures, report):
    """Apply one round of rewrites; returns the new lines and whether anything
    changed."""
    directives = parse_directives(lines)
    unsafe = lines_not_safe_for_comments(lines)
    unexpected_at = {}
    for f in failures:
        if f.kind == "unexpected":
            unexpected_at.setdefault(f.line, []).append(f)

    # Per line: (start, end, replacement) edits and trailing additions.
    edits = {}
    appends = {}
    handled = set()

    def directive_for_message(f):
        # A missing diagnostic is reported at the directive itself, a wrong
        # message at the message text.
        found = [d for d in directives
                 if d.line == f.line
                 and (d.start + 1 if f.kind == "missing" else d.message_col) == f.col]
        return found[0] if found else None

    def directive_for_fixit(f):
        for d in directives:
            if d.line == f.line and any(abs(f.col - c) <= 1 for c, _ in d.fixits):
                return d
        return None

    def cparser_copy(d):
        return d.with_prefix(lines[d.line][d.start:d.end], "cparser")

    changed = False
    for f in failures:
        if f.kind == "unexpected":
            if f.line in unsafe:
                report.manual.append((f.line, "expected-astgen-%s {{%s}}" % (f.detail, f.actual)))
                continue
            appends.setdefault(f.line, []).append(
                "expected-astgen-%s {{%s}}" % (f.detail, f.actual))
            report.astgen_only.append((f.line, f.detail, f.actual))
            changed = True
            continue

        d = directive_for_message(f) if f.kind in ("missing", "incorrect") else directive_for_fixit(f)
        if d is None or id(d) in handled:
            if d is None:
                report.manual.append((f.line, "no directive at column %d" % f.col))
            continue
        if d.prefix is not None:
            report.manual.append((d.line, "%s directive is wrong: %s" % (d.prefix, d.message)))
            continue
        handled.add(id(d))
        changed = True

        if f.kind == "missing":
            replacement = cparser_copy(d)
            # ASTGen often reports the same problem a line or two away, since
            # the parsers recover differently.
            nearby = any(unexpected_at.get(l) for l in range(d.target_line() - 3, d.target_line() + 4))
            if not nearby:
                report.silent.append((d.target_line(), d.kind, d.message))
            else:
                report.reworded.append((d.target_line(), d.kind, d.message, None))
        elif f.kind == "incorrect":
            replacement = cparser_copy(d) + " " + d.render("astgen", message=f.actual, fixits=[])
            report.reworded.append((d.target_line(), d.kind, d.message, f.actual))
        else:  # fixit
            astgen_fixits = [(0, t) for t in f.actual]
            # A fix-it whose text contains braces or spans lines, like those
            # that move a token across lines, cannot be written in a directive.
            if any("{{" in t or "}}" in t or "\n" in t or re.match(r"\d+:\d+", t) for _, t in astgen_fixits):
                astgen_fixits = []
            replacement = cparser_copy(d) + " " + d.render("astgen", fixits=astgen_fixits)
            report.reworded.append((d.target_line(), d.kind, d.message, d.message + " (fix-its)"))
        edits.setdefault(d.line, []).append((d.start, d.end, replacement))

    new_lines = list(lines)
    for i, line_edits in edits.items():
        text = new_lines[i]
        for start, end, replacement in sorted(line_edits, reverse=True):
            text = text[:start] + replacement + text[end:]
        new_lines[i] = text
    for i, additions in appends.items():
        text = new_lines[i].rstrip("\n")
        newline = "\n" if new_lines[i].endswith("\n") else ""
        if "//" in text and "expected-" in text.split("//", 1)[1]:
            text = text.rstrip() + " " + " ".join(additions)
        else:
            text = text.rstrip() + " // " + " ".join(additions)
        new_lines[i] = text + newline
    return new_lines, changed


def check_runnable(path):
    with open(path, **FILE_MODE) as f:
        text = f.read()
    run_lines = [l for l in text.splitlines() if "RUN:" in l]
    reasons = []
    if any("-verify-additional-prefix" in l for l in run_lines):
        reasons.append("a RUN line has its own -verify-additional-prefix")
    if any("ParserASTGen" in l for l in run_lines):
        reasons.append("a RUN line selects the parser itself")
    if re.search(r"expected-(legacy|new)-", text):
        reasons.append("it already splits expectations with its own prefixes")
    return reasons


def process(path, args, build_dir, lit):
    rel = os.path.relpath(path, TEST_DIR)
    print("== %s" % rel)
    reasons = check_runnable(path)
    if reasons:
        print("   skipped: " + "; ".join(reasons))
        return False

    if not args.force:
        ok, out = run_lit(build_dir, lit, rel, "cparser")
        if ok is not True:
            print("   skipped: it does not pass with the C++ parser (use --force)")
            return False

    with open(path, **FILE_MODE) as f:
        original = f.readlines()
    lines = list(original)
    report = Report()

    for iteration in range(args.max_iterations):
        # Run the candidate text.
        with open(path, "w", **FILE_MODE) as f:
            f.writelines(lines)
        ok, out = run_lit(build_dir, lit, rel, "astgen")
        if ok is True:
            break
        if ok is None:
            print("   skipped: the test is unsupported under ASTGen")
            ok = None
            break
        failures, other = parse_lit_output(out, path)
        if not failures:
            if other:
                print("   cannot fix: diagnostics outside the test file:")
                for o in sorted(set(other))[:5]:
                    print("     " + o)
            else:
                print("   cannot fix: failure is not a verifier mismatch")
            break
        new_lines, changed = rewrite(path, lines, failures, report)
        if not changed or new_lines == lines:
            break
        lines = new_lines
    else:
        ok = False

    with open(path, "w", **FILE_MODE) as f:
        f.writelines(lines)
    astgen_ok, _ = run_lit(build_dir, lit, rel, "astgen")
    cparser_ok, _ = run_lit(build_dir, lit, rel, "cparser")
    print("   result: astgen %s, cparser %s" % (
        "passes" if astgen_ok else "FAILS", "passes" if cparser_ok else "FAILS"))

    both_pass = bool(astgen_ok and cparser_ok)
    if args.dry_run or (not both_pass and not args.keep_partial):
        if args.dry_run and not args.summary:
            sys.stdout.writelines(difflib.unified_diff(original, lines, "a/" + rel, "b/" + rel))
        elif lines != original:
            print("   not written: the test still fails (use --keep-partial to keep the edits)")
        with open(path, "w", **FILE_MODE) as f:
            f.writelines(original)

    def show(title, items, fmt):
        if items:
            print("   %s (%d):" % (title, len(items)))
            for item in sorted(set(items)):
                print("     " + fmt(item))

    if len(report.silent) > 25:
        print("   note: ASTGen is silent about many diagnostics; an error in its parse")
        print("   may be stopping compilation before the rest are produced. Fix the")
        print("   first ASTGen-only error and run this again.")
    show("ASTGen is silent", report.silent,
         lambda t: "%d: %s {{%s}}" % (t[0] + 1, t[1], t[2]))
    show("ASTGen only", report.astgen_only,
         lambda t: "%d: %s {{%s}}" % (t[0] + 1, t[1], t[2]))
    n = len(set(report.reworded))
    if n:
        print("   reworded: %d" % n)
    show("needs manual attention", report.manual, lambda t: "%d: %s" % (t[0] + 1, t[1]))
    return both_pass


def main():
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("tests", nargs="+")
    parser.add_argument("--build-dir")
    parser.add_argument("--dry-run", action="store_true")
    parser.add_argument("--summary", action="store_true",
                        help="with --dry-run, print the report but not the diff")
    parser.add_argument("--max-iterations", type=int, default=4)
    parser.add_argument("--force", action="store_true")
    parser.add_argument("--keep-partial", action="store_true",
                        help="keep the rewritten test even if it still fails")
    args = parser.parse_args()

    build_dir = os.path.abspath(args.build_dir) if args.build_dir else find_build_dir()
    lit = find_lit(build_dir)

    ok = True
    for t in args.tests:
        path = t if os.path.isabs(t) else os.path.join(TEST_DIR, t)
        if not os.path.exists(path):
            sys.exit("error: no such test: %s" % t)
        ok = process(os.path.abspath(path), args, build_dir, lit) and ok
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
