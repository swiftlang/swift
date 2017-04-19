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
import re
import subprocess

import jinja2

script_dir = os.path.dirname(os.path.realpath(__file__))
perf_dir = os.path.realpath(os.path.join(script_dir, '../..'))
gyb = os.path.realpath(os.path.join(perf_dir, '../utils/gyb'))
single_source_dir = os.path.join(perf_dir, 'single-source')
multi_source_dir = os.path.join(perf_dir, 'multi-source')
parser = argparse.ArgumentParser()
parser.add_argument("--output-dir",
                    help="Output directory (for validation test)",
                    default=perf_dir)
args = parser.parse_args()
output_dir = args.output_dir

template_map = {
    'CMakeLists.txt_template': os.path.join(output_dir, 'CMakeLists.txt'),
    'main.swift_template': os.path.join(output_dir, 'utils/main.swift')
}

### The test suites. Currently, "other" and "string"
other_tests = ["Ackermann", "Fibonacci"]
string_tests = [
    "StringWalkASCIIScalars",
    "StringWalkASCIICharacters",
    "StringWalkUnicodeScalars",
    "StringWalkUnicodeCharacters",
    "StringWalkASCIIScalarsBackwards",
    "StringWalkASCIICharactersBackwards",
    "StringWalkUnicodeScalarsBackwards",
    "StringWalkUnicodeCharactersBackwards",
]

ignored_run_funcs = other_tests + string_tests

template_loader = jinja2.FileSystemLoader(searchpath="/")
template_env = jinja2.Environment(loader=template_loader, trim_blocks=True,
                                  lstrip_blocks=True)


def all_files(directory, extension):  # matching: [directory]/**/*[extension]
    return [
        os.path.join(root, f)
        for root, _, files in os.walk(directory)
        for f in files if f.endswith(extension)
    ]


def will_write(filename):  # ensure path to file exists before writing
    print(filename)
    output_path = os.path.split(filename)[0]
    if not os.path.exists(output_path):
        os.makedirs(output_path)


if __name__ == '__main__':
    # Generate Your Boilerplate
    gyb_files = all_files(perf_dir, '.gyb')
    for f in gyb_files:
        relative_path = os.path.relpath(f[:-4], perf_dir)
        out_file = os.path.join(output_dir, relative_path)
        will_write(out_file)
        subprocess.call([gyb, '--line-directive', '', '-o', out_file, f])

    # CMakeList single-source
    test_files = all_files(single_source_dir, '.swift')
    tests = sorted(os.path.basename(x).split('.')[0] for x in test_files)

    # CMakeList multi-source
    class MultiSourceBench(object):
        def __init__(self, path):
            self.name = os.path.basename(path)
            self.files = [x for x in os.listdir(path)
                          if x.endswith('.swift')]

    if os.path.isdir(multi_source_dir):
        multisource_benches = [
            MultiSourceBench(os.path.join(multi_source_dir, x))
            for x in os.listdir(multi_source_dir)
            if os.path.isdir(os.path.join(multi_source_dir, x))
        ]
    else:
        multisource_benches = []

    # main.swift imports
    imports = sorted(tests + [msb.name for msb in multisource_benches])

    # main.swift run functions
    def get_run_funcs(filepath):
        content = open(filepath).read()
        matches = re.findall(r'func run_(.*?)\(', content)
        return filter(lambda x: x not in ignored_run_funcs, matches)

    def find_run_funcs():
        swift_files = all_files(perf_dir, '.swift')
        return [func for f in swift_files for func in get_run_funcs(f)]

    run_funcs = [(f, f) for f in sorted(find_run_funcs())]

    # Replace originals with files generated from templates
    for template_file in template_map:
        template_path = os.path.join(script_dir, template_file)
        template = template_env.get_template(template_path)
        out_file = template_map[template_file]
        will_write(out_file)
        open(out_file, 'w').write(
            template.render(tests=tests,
                            multisource_benches=multisource_benches,
                            imports=imports,
                            run_funcs=run_funcs,
                            string_tests=string_tests,
                            other_tests=other_tests)
        )
