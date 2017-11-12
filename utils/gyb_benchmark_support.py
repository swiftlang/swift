# ===--- gyb_benchmark_support.py --------------------*- coding: utf-8 -*-===//
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import re


script_dir = os.path.dirname(os.path.realpath(__file__))
perf_dir = os.path.realpath(os.path.join(script_dir, '../benchmark'))
single_source_dir = os.path.join(perf_dir, 'single-source')
multi_source_dir = os.path.join(perf_dir, 'multi-source')


def all_files(directory, extension):  # matching: [directory]/**/*[extension]
    return [
        os.path.join(root, f)
        for root, _, files in os.walk(directory)
        for f in files if f.endswith(extension)
    ]


# CMakeList single-source
test_files = all_files(single_source_dir, '.swift')
tests = sorted(os.path.basename(x).split('.')[0] for x in test_files)


# CMakeList multi-source
class MultiSourceBench(object):
    def __init__(self, path):
        self.name = os.path.basename(path)
        self.files = [x for x in os.listdir(path)
                      if x.endswith('.swift')]


multisource_benches = [
    MultiSourceBench(os.path.join(multi_source_dir, x))
    for x in os.listdir(multi_source_dir)
    if os.path.isdir(os.path.join(multi_source_dir, x))
] if os.path.isdir(multi_source_dir) else []


def get_run_funcs(filepath):
    content = open(filepath).read()
    return re.findall(r'func run_(.*?)\(', content)


def find_run_funcs():
    swift_files = all_files(single_source_dir, '.swift')
    swift_files += all_files(multi_source_dir, '.swift')
    return sorted([func for f in swift_files for func in get_run_funcs(f)])


all_run_funcs = find_run_funcs()
