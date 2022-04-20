#!/usr/bin/env python3
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import subprocess
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--path", help="path to a binary to check")
parser.add_argument("--triple", help="target triple of the freestanding stdlib")
parser.add_argument("--size-path", help="path to llvm-size binary to use")
args = parser.parse_args()


################################################################################
#
# EXPECTED/ALLOWED TEXT SEGMENT SIZE
#
# Before bumping the maximum text segment size please consult with:
# @kubamracek, @compnerd
#
# The 'freestanding' build of the Swift runtime and standard library is
# intended to be lean and allow dead-code elimination from the standard library
# based on client code usage.
#
################################################################################

# As of 2022-01-05: TEXT segment is 640 KiB on x86_64 Darwin, stdlib without
# asserts. Let's allow for some minor increases (~ 15%) and cap at 740 kiB:

max_text_size_x86_64_apple_macos = 740 * 1024

################################################################################


if args.triple == "x86_64-apple-macos":
    max_text_size = max_text_size_x86_64_apple_macos
    actual_text_size = 0

    nm = args.size_path
    lines = subprocess.check_output(
        [nm, "--format=darwin", args.path]) \
        .decode("utf-8").strip().splitlines()
    for line in lines:
        if line.startswith("Segment __TEXT: "):
            actual_text_size = int(line[len("Segment __TEXT: "):])

    if actual_text_size == 0:
        print("cannot determine TEXT segment size")
        sys.exit(1)

else:
    print("triple {} not handled yet".format(args.triple))
    sys.exit(1)

print("max_text_size: %d" % max_text_size)
print("actual_text_size: %d" % actual_text_size)
fail = actual_text_size > max_text_size

sys.exit(1 if fail else 0)
