#!/usr/bin/env python
# -*- coding: utf-8 -*-

# ===--- check_added_bench -----------------------------------------------===//
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
#
# Runs BenchmarkDoctor's check on newly added benchmarks.
#
# ===---------------------------------------------------------------------===//

from __future__ import print_function

import argparse
import os

from imp import load_source
# import Benchmark_Driver  # doesn't work because it misses '.py' extension
Benchmark_Driver = load_source(
    'Benchmark_Driver', os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'Benchmark_Driver'))
# from Benchmark_Driver import BenchmarkDriver, BenchmarkDoctor
BenchmarkDriver = Benchmark_Driver.BenchmarkDriver
BenchmarkDoctor = Benchmark_Driver.BenchmarkDoctor

class Args(object):
    def __init__(self, tests):
        self.benchmarks = None
        self.filters = None
        self.tests = os.path.join(tests, 'bin')
        self.optimization = 'O'

def main():
    argparser = argparse.ArgumentParser()
    argparser.add_argument(
        'oldbuilddir', nargs=1, type=str, help='old benchmark build directory')
    argparser.add_argument(
        'newbuilddir', nargs=1, type=str, help='new benchmark build directory')
    argparser.add_argument(
        '-v', '--verbose', action='store_true',
        help='show more details during benchmark analysis')
    args = argparser.parse_args()

    old = BenchmarkDriver(Args(args.oldbuilddir[0]))
    new = BenchmarkDriver(Args(args.newbuilddir[0]))
    added = set(new.tests).difference(set(old.tests))
    new.tests = list(added)
    doctor = BenchmarkDoctor(args, driver=new)
    doctor.check()
    return 0


if __name__ == '__main__':
    exit(main())
