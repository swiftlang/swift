#!/usr/bin/env python

# ===--- generate_harness.py ---------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https://swift.org/LICENSE.txt for license information
#  See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===---------------------------------------------------------------------===//

# Generate boilerplate, CMakeLists.txt and utils/main.swift from templates.

from __future__ import print_function

import argparse
import os
import subprocess

script_dir = os.path.dirname(os.path.realpath(__file__))
perf_dir = os.path.realpath(os.path.join(script_dir, "../.."))
gyb = os.path.realpath(os.path.join(perf_dir, "../utils/gyb"))
parser = argparse.ArgumentParser()
parser.add_argument(
    "--output-dir", help="Output directory (for validation test)", default=perf_dir
)
args = parser.parse_args()
output_dir = args.output_dir


def all_files(directory, extension):  # matching: [directory]/**/*[extension]
    return [
        os.path.join(root, f)
        for root, _, files in os.walk(directory)
        for f in files
        if f.endswith(extension)
    ]


def will_write(filename):  # ensure path to file exists before writing
    print(filename)
    output_path = os.path.split(filename)[0]
    if not os.path.exists(output_path):
        os.makedirs(output_path)


if __name__ == "__main__":
    # Generate Your Boilerplate
    # Make sure longer paths are done first as CMakeLists.txt and main.swift
    # depend on the other gybs being generated first.
    gyb_files = sorted(all_files(perf_dir, ".gyb"), key=len, reverse=True)
    for f in gyb_files:
        relative_path = os.path.relpath(f[:-4], perf_dir)
        out_file = os.path.join(output_dir, relative_path)
        will_write(out_file)
        subprocess.call([gyb, "--line-directive", "", "-o", out_file, f])
