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

# The test suites. Currently, "other" and "string"
other_tests = [
    "Ackermann",
    "Fibonacci",
    "ExistentialTestArrayConditionalShift_ClassValueBuffer1",
    "ExistentialTestArrayConditionalShift_ClassValueBuffer2",
    "ExistentialTestArrayConditionalShift_ClassValueBuffer3",
    "ExistentialTestArrayConditionalShift_ClassValueBuffer4",
    "ExistentialTestArrayConditionalShift_IntValueBuffer0",
    "ExistentialTestArrayConditionalShift_IntValueBuffer1",
    "ExistentialTestArrayConditionalShift_IntValueBuffer2",
    "ExistentialTestArrayConditionalShift_IntValueBuffer3",
    "ExistentialTestArrayConditionalShift_IntValueBuffer4",
    "ExistentialTestArrayMutating_ClassValueBuffer1",
    "ExistentialTestArrayMutating_ClassValueBuffer2",
    "ExistentialTestArrayMutating_ClassValueBuffer3",
    "ExistentialTestArrayMutating_ClassValueBuffer4",
    "ExistentialTestArrayMutating_IntValueBuffer0",
    "ExistentialTestArrayMutating_IntValueBuffer1",
    "ExistentialTestArrayMutating_IntValueBuffer2",
    "ExistentialTestArrayMutating_IntValueBuffer3",
    "ExistentialTestArrayMutating_IntValueBuffer4",
    "ExistentialTestArrayOneMethodCall_ClassValueBuffer1",
    "ExistentialTestArrayOneMethodCall_ClassValueBuffer2",
    "ExistentialTestArrayOneMethodCall_ClassValueBuffer3",
    "ExistentialTestArrayOneMethodCall_ClassValueBuffer4",
    "ExistentialTestArrayOneMethodCall_IntValueBuffer0",
    "ExistentialTestArrayOneMethodCall_IntValueBuffer1",
    "ExistentialTestArrayOneMethodCall_IntValueBuffer2",
    "ExistentialTestArrayOneMethodCall_IntValueBuffer3",
    "ExistentialTestArrayOneMethodCall_IntValueBuffer4",
    "ExistentialTestArrayShift_ClassValueBuffer1",
    "ExistentialTestArrayShift_ClassValueBuffer2",
    "ExistentialTestArrayShift_ClassValueBuffer3",
    "ExistentialTestArrayShift_ClassValueBuffer4",
    "ExistentialTestArrayShift_IntValueBuffer0",
    "ExistentialTestArrayShift_IntValueBuffer1",
    "ExistentialTestArrayShift_IntValueBuffer2",
    "ExistentialTestArrayShift_IntValueBuffer3",
    "ExistentialTestArrayShift_IntValueBuffer4",
    "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer1",
    "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer2",
    "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer3",
    "ExistentialTestArrayTwoMethodCalls_ClassValueBuffer4",
    "ExistentialTestArrayTwoMethodCalls_IntValueBuffer0",
    "ExistentialTestArrayTwoMethodCalls_IntValueBuffer1",
    "ExistentialTestArrayTwoMethodCalls_IntValueBuffer2",
    "ExistentialTestArrayTwoMethodCalls_IntValueBuffer3",
    "ExistentialTestArrayTwoMethodCalls_IntValueBuffer4",
    "ExistentialTestMutatingAndNonMutating_ClassValueBuffer1",
    "ExistentialTestMutatingAndNonMutating_ClassValueBuffer2",
    "ExistentialTestMutatingAndNonMutating_ClassValueBuffer3",
    "ExistentialTestMutatingAndNonMutating_ClassValueBuffer4",
    "ExistentialTestMutatingAndNonMutating_IntValueBuffer0",
    "ExistentialTestMutatingAndNonMutating_IntValueBuffer1",
    "ExistentialTestMutatingAndNonMutating_IntValueBuffer2",
    "ExistentialTestMutatingAndNonMutating_IntValueBuffer3",
    "ExistentialTestMutatingAndNonMutating_IntValueBuffer4",
    "ExistentialTestMutating_ClassValueBuffer1",
    "ExistentialTestMutating_ClassValueBuffer2",
    "ExistentialTestMutating_ClassValueBuffer3",
    "ExistentialTestMutating_ClassValueBuffer4",
    "ExistentialTestMutating_IntValueBuffer0",
    "ExistentialTestMutating_IntValueBuffer1",
    "ExistentialTestMutating_IntValueBuffer2",
    "ExistentialTestMutating_IntValueBuffer3",
    "ExistentialTestMutating_IntValueBuffer4",
    "ExistentialTestOneMethodCall_ClassValueBuffer1",
    "ExistentialTestOneMethodCall_ClassValueBuffer2",
    "ExistentialTestOneMethodCall_ClassValueBuffer3",
    "ExistentialTestOneMethodCall_ClassValueBuffer4",
    "ExistentialTestOneMethodCall_IntValueBuffer0",
    "ExistentialTestOneMethodCall_IntValueBuffer1",
    "ExistentialTestOneMethodCall_IntValueBuffer2",
    "ExistentialTestOneMethodCall_IntValueBuffer3",
    "ExistentialTestOneMethodCall_IntValueBuffer4",
    "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer1",
    "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer2",
    "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer3",
    "ExistentialTestPassExistentialOneMethodCall_ClassValueBuffer4",
    "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer0",
    "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer1",
    "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer2",
    "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer3",
    "ExistentialTestPassExistentialOneMethodCall_IntValueBuffer4",
    "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer1",
    "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer2",
    "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer3",
    "ExistentialTestPassExistentialTwoMethodCalls_ClassValueBuffer4",
    "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer0",
    "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer1",
    "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer2",
    "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer3",
    "ExistentialTestPassExistentialTwoMethodCalls_IntValueBuffer4",
    "ExistentialTestTwoMethodCalls_ClassValueBuffer1",
    "ExistentialTestTwoMethodCalls_ClassValueBuffer2",
    "ExistentialTestTwoMethodCalls_ClassValueBuffer3",
    "ExistentialTestTwoMethodCalls_ClassValueBuffer4",
    "ExistentialTestTwoMethodCalls_IntValueBuffer0",
    "ExistentialTestTwoMethodCalls_IntValueBuffer1",
    "ExistentialTestTwoMethodCalls_IntValueBuffer2",
    "ExistentialTestTwoMethodCalls_IntValueBuffer3",
    "ExistentialTestTwoMethodCalls_IntValueBuffer4",
]

string_tests = [
    "StringWalkASCIIScalars",
    "StringWalkASCIICharacters",
    "StringWalkUnicodeScalars",
    "StringWalkUnicodeCharacters",
    "StringWalkMixedScalars",
    "StringWalkMixedCharacters",
    "StringWalkASCIIScalarsBackwards",
    "StringWalkASCIICharactersBackwards",
    "StringWalkUnicodeScalarsBackwards",
    "StringWalkUnicodeCharactersBackwards",
    "StringWalkMixedScalarsBackwards",
    "StringWalkMixedCharactersBackwards",
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
