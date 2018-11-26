#!/usr/bin/env python
# -*- coding: utf-8 -*-

# ===--- run_smoke_bench -------------------------------------------------===//
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
# Performs a very fast check which benchmarks regressed and improved.
#
# Initially runs the benchmars with a low sample count and just re-runs those
# benchmarks which differ.
#
# ===---------------------------------------------------------------------===//

from __future__ import print_function

import argparse
import os
import re
import subprocess
import sys

VERBOSE = False


def log(msg):
    print(msg)
    sys.stdout.flush()


def main():
    global VERBOSE
    argparser = argparse.ArgumentParser()
    argparser.add_argument(
        '-verbose', action='store_true',
        help='print verbose messages')
    argparser.add_argument(
        '-O', action='append_const', const='O', dest='opt_levels',
        help='test -O benchmarks')
    argparser.add_argument(
        '-Osize', action='append_const', const='Osize', dest='opt_levels',
        help='test -Osize benchmarks')
    argparser.add_argument(
        '-Onone', action='append_const', const='Onone', dest='opt_levels',
        help='test -Onone benchmarks')
    argparser.add_argument(
        '-threshold', type=float,
        help='The threshold in % which triggers a re-run', default=5)
    argparser.add_argument(
        '-num-samples', type=int,
        help='The (minimum) number of samples to run', default=3)
    argparser.add_argument(
        'oldbuilddir', nargs=1, type=str,
        help='old benchmark build directory')
    argparser.add_argument(
        'newbuilddir', nargs=1, type=str,
        help='new benchmark build directory')
    argparser.add_argument(
        '-check-added', action='store_const',
        help="Run BenchmarkDoctor's check on newly added benchmarks",
        const=lambda args: check_added(args), dest='func')
    argparser.set_defaults(func=test_opt_levels)
    args = argparser.parse_args()
    VERBOSE = args.verbose

    return args.func(args)


def test_opt_levels(args):
    for opt_level in args.opt_levels or ['O', 'Osize', 'Onone']:
        log('Testing optimization level -' + opt_level)
        test_opt_level(opt_level, args.oldbuilddir[0], args.newbuilddir[0],
                       float(args.threshold) / 100, args.num_samples)

    return 0


def test_opt_level(opt_level, old_dir, new_dir, threshold, num_samples):
    num_results_dont_differ = 0
    iter = 1
    to_test = None
    prev_num_tests = None

    def log_filename(bench_dir):
        return os.path.join(bench_dir, 'result_' + opt_level)

    old_logf = open(log_filename(old_dir), 'w')
    new_logf = open(log_filename(new_dir), 'w')

    # #,TEST,SAMPLES,MIN(μs),MAX(μs),MEAN(μs),SD(μs),MEDIAN(μs),PEAK_MEMORY(B)
    score_re = re.compile(r"(\d+),(\w+),\d+,(\d+)")

    while to_test is None or len(to_test) > 0:
        tested_benchmarks = set()

        # (benchmark_name, benchmark_directory) -> (min_value, result_line)
        values = {}

        # Run the benchmarks and store the results in 'values'.
        for bench_dir in (old_dir, new_dir):
            log('    Iteration ' + str(iter) + ' for ' + bench_dir +
                ': num samples = ' + str(num_samples) +
                (', running all tests' if to_test is None
                    else ', re-testing ' + str(len(to_test)) + ' tests'))

            result = get_results(bench_dir, opt_level, num_samples, to_test)
            for line in result.splitlines():
                m = score_re.match(line)
                if m:
                    testname = m.group(2)
                    val = int(m.group(3))
                    values[(testname, bench_dir)] = (val, line)
                    tested_benchmarks.add(testname)

        # Some local utility functions

        def bench_in(bench, bench_dir):
            return (bench, bench_dir) in values

        def within_threshold(bench):
            old_val = values[(bench, old_dir)][0]
            new_val = values[(bench, new_dir)][0]
            if not new_val:
                return True
            f = float(old_val) / float(new_val)
            return f >= 1.0 - threshold and f <= 1.0 + threshold

        def write_line(bench, bench_dir, logf):
            result_line = values[(bench, bench_dir)][1]
            logf.write(result_line + '\n')

        # Check which benchmarks are added/removed and which need to be re-run
        to_test = []
        for bench in sorted(tested_benchmarks):
            if bench_in(bench, old_dir) and not bench_in(bench, new_dir):
                write_line(bench, old_dir, old_logf)
            elif bench_in(bench, new_dir) and not bench_in(bench, old_dir):
                write_line(bench, new_dir, new_logf)
            elif within_threshold(bench) or num_results_dont_differ >= 4:
                write_line(bench, old_dir, old_logf)
                write_line(bench, new_dir, new_logf)
            else:
                to_test.append(bench)
                if VERBOSE:
                    log('        test again ' + bench)

        # Track how many times we could not reduce the number of benchmarks
        if prev_num_tests == len(to_test):
            num_results_dont_differ += 1
        else:
            num_results_dont_differ = 0
        prev_num_tests = len(to_test)

        # Increase the number of samples for benchmarks which re-run
        if num_samples < 10:
            num_samples += 1

        iter += 1

    old_logf.close()
    new_logf.close()

    print('Logfiles written to ' + log_filename(old_dir) + ' and ' +
          log_filename(new_dir))


def get_results(bench_dir, opt_level, num_samples, to_test):
    try:
        exe = os.path.join(bench_dir, 'bin', 'Benchmark_' + opt_level)
        args = [exe, '--num-samples=' + str(num_samples),
                '--sample-time=0.0025']
        if to_test:
            args += to_test
        env = {'DYLD_LIBRARY_PATH': os.path.join(bench_dir, 'lib', 'swift',
               'macos')}
        output = subprocess.check_output(args, env=env)
    except subprocess.CalledProcessError as e:
        sys.stderr.write(e.output)
        sys.stderr.flush()
        return sys.exit(e.returncode)
    else:
        return output


class DriverArgs(object):
    def __init__(self, tests):
        self.benchmarks = None
        self.filters = None
        self.tests = os.path.join(tests, 'bin')
        self.optimization = 'O'


def check_added(args):
    from imp import load_source
    # import Benchmark_Driver  # doesn't work because it misses '.py' extension
    Benchmark_Driver = load_source(
        'Benchmark_Driver', os.path.join(os.path.dirname(
            os.path.abspath(__file__)), 'Benchmark_Driver'))
    # from Benchmark_Driver import BenchmarkDriver, BenchmarkDoctor
    BenchmarkDriver = Benchmark_Driver.BenchmarkDriver
    BenchmarkDoctor = Benchmark_Driver.BenchmarkDoctor

    old = BenchmarkDriver(DriverArgs(args.oldbuilddir[0]))
    new = BenchmarkDriver(DriverArgs(args.newbuilddir[0]))
    added = set(new.tests).difference(set(old.tests))
    new.tests = list(added)
    doctor = BenchmarkDoctor(args, driver=new)
    doctor.check()


if __name__ == '__main__':
    sys.exit(main())
