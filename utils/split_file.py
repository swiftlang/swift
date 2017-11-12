#!/usr/bin/env python
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import argparse
import os
import re
import sys

parser = argparse.ArgumentParser(
    description="""
Take the file at <path> and write it to multiple files, switching to a new file
every time an annotation of the form "// BEGIN file1.swift" is encountered. If
<dir> is specified, place the files in <dir>; otherwise, put them in the
current directory.
""")
parser.add_argument(
    "-o", dest="out_dir", default=".", metavar="<dir>",
    help="directory path where the output files are placed in. "
         "(defaults to current directory)")
parser.add_argument(
    "input", type=argparse.FileType("r"), nargs="?", default=sys.stdin,
    metavar="<path>",
    help="input file. (defaults to stdin)")
args = parser.parse_args()

fp_out = None

for line in args.input:
    m = re.match(r'^//\s*BEGIN\s+([^\s]+)\s*$', line)
    if m:
        if fp_out:
            fp_out.close()
        fp_out = open(os.path.join(args.out_dir, m.group(1)), 'w')
    elif fp_out:
        fp_out.write(line)

args.input.close()
if fp_out:
    fp_out.close()
