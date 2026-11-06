#!/usr/bin/env python3
# utils/astgen-reduce-crash.py - Reduce a file that crashes ASTGen
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

"""Delta-debug a Swift file that crashes under ParserASTGen, by *deleting*
lines rather than truncating the file.

Prefix truncation misattributes these crashes: cutting the file at the
apparently-offending line yields a differently-malformed file that crashes for
its own reasons. Several ASTGen crashes were mis-diagnosed that way before this
existed. This removes chunks while checking that the *same* crash persists,
identified by its assertion text or fatalError message.

Usage:
  utils/astgen-reduce-crash.py FILE [FILE...]
  utils/astgen-reduce-crash.py 'FILE::-enable-experimental-feature Foo'

Set SWIFT_FRONTEND to point at the swift-frontend to test; it defaults to a
Ninja build next to the source checkout.
"""
import os
import subprocess
import sys

FE = os.environ.get(
    "SWIFT_FRONTEND",
    os.path.join(
        os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
        "build/Ninja-RelWithDebInfoAssert/swift-macosx-arm64/bin/swift-frontend"))
SDK = subprocess.run(["xcrun", "--show-sdk-path"], capture_output=True, text=True).stdout.strip()
TMP = "/tmp/reduce_probe.swift"


def signature(lines, extra_args):
    """Return a short string identifying the crash, or None if it doesn't crash."""
    with open(TMP, "w") as f:
        f.writelines(lines)
    p = subprocess.run(
        [FE, "-typecheck", "-sdk", SDK, TMP,
         "-enable-experimental-feature", "ParserASTGen"] + extra_args,
        capture_output=True, text=True, errors="replace")
    crashed = p.returncode < 0 or p.returncode >= 128 or "PLEASE submit a bug report" in p.stderr
    if not crashed:
        return None
    for line in p.stderr.splitlines():
        for marker in ("Assertion failed:", "Fatal error:", "UNREACHABLE",
                       "child not contained in its parent",
                       "child overlaps previous child",
                       "child source range not contained"):
            if marker in line:
                return line.strip()[:90]
    return "crash"


def reduce_file(path, extra_args):
    lines = open(path, encoding="utf-8", errors="replace").read().splitlines(keepends=True)
    target = signature(lines, extra_args)
    if target is None:
        return None, None
    # Repeatedly try deleting chunks, halving the chunk size when a pass finds nothing.
    n = max(len(lines) // 2, 1)
    while n >= 1:
        i = 0
        changed = False
        while i < len(lines):
            trial = lines[:i] + lines[i + n:]
            if trial and signature(trial, extra_args) == target:
                lines = trial
                changed = True
            else:
                i += n
        if not changed:
            n //= 2
        # else: retry at the same chunk size
    return lines, target


for src in sys.argv[1:]:
    path, _, argstr = src.partition("::")
    extra = argstr.split() if argstr else []
    reduced, target = reduce_file(path, extra)
    print("=" * 70)
    print(os.path.basename(path), "->", target)
    if reduced is None:
        print("  (does not crash)")
        continue
    print("  {} lines".format(len(reduced)))
    for line in reduced:
        print("  | " + line.rstrip())
