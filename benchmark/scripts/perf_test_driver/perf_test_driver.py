#!/usr/bin/env python

# ===--- perf_test_driver.py ---------------------------------------------===//
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

import functools
import multiprocessing
import os
import re
import subprocess


class Result(object):

    def __init__(self, name, status, output, xfail_list):
        self.name = name
        self.status = status
        self.output = output
        self.is_xfailed = any(
            (re.match(x, self.name) is not None for x in xfail_list))

    def is_failure(self):
        return self.get_result() in ['FAIL', 'XPASS']

    def get_result(self):
        if self.is_xfailed:
            if self.status:
                return 'XFAIL'
            return 'XPASS'
        if self.status:
            return 'FAIL'
        return 'PASS'

    def get_name(self):
        return self.name

    def merge_in_extra_data(self, d):
        """Rather than modifying the extra data dict, return it as a no-op"""
        return d

    def print_data(self, max_test_len):
        fmt = '{:<%d}{:}' % (max_test_len + 5)
        print(fmt.format(self.get_name(), self.get_result()))


def _unwrap_self(args):
    return type(args[0]).process_input(*args)

BenchmarkDriver_OptLevels = ['Onone', 'O', 'Ounchecked']


class BenchmarkDriver(object):

    def __init__(self, binary_dir, xfail_list, enable_parallel=False,
                 opt_levels=BenchmarkDriver_OptLevels):
        self.targets = [(os.path.join(binary_dir, 'Benchmark_%s' % o), o)
                        for o in opt_levels]
        self.xfail_list = xfail_list
        self.enable_parallel = enable_parallel
        self.data = None

    def print_data_header(self, max_test_len):
        fmt = '{:<%d}{:}' % (max_test_len + 5)
        print(fmt.format('Name', 'Result'))

    def prepare_input(self, name, opt_level):
        raise RuntimeError("Abstract method")

    def process_input(self, data):
        raise RuntimeError("Abstract method")

    def run_for_opt_level(self, binary, opt_level, test_filter):
        print("testing driver at path: %s" % binary)
        names = [n.strip() for n in subprocess.check_output(
            [binary, "--list"]).split()[2:]]
        if test_filter:
            regex = re.compile(test_filter)
            names = [n for n in names if regex.match(n)]

        def prepare_input_wrapper(name):
            x = {'opt': opt_level, 'path': binary, 'test_name': name}
            x.update(self.prepare_input(name))
            return x

        prepared_input = [prepare_input_wrapper(n) for n in names]
        results = None
        if self.enable_parallel:
            p = multiprocessing.Pool()
            z = zip([self] * len(prepared_input), prepared_input)
            results = p.map(_unwrap_self, z)
        else:
            results = map(self.process_input, prepared_input)

        def reduce_results(acc, r):
            acc['result'].append(r)
            acc['has_failure'] = acc['has_failure'] or r.is_failure()
            acc['max_test_len'] = max(acc['max_test_len'], len(r.get_name()))
            acc['extra_data'] = r.merge_in_extra_data(acc['extra_data'])
            return acc

        return functools.reduce(reduce_results, results, {
            'result': [],
            'has_failure': False,
            'max_test_len': 0,
            'extra_data': {}
        })

    def print_data(self, data, max_test_len):
        print("Results:")
        self.print_data_header(max_test_len)
        for d in data:
            for r in d['result']:
                r.print_data(max_test_len)

    def run(self, test_filter=None):
        self.data = [
            self.run_for_opt_level(binary, opt_level, test_filter)
            for binary, opt_level in self.targets]
        max_test_len = functools.reduce(max,
                                        [d['max_test_len'] for d in self.data])
        has_failure = functools.reduce(max,
                                       [d['has_failure'] for d in self.data])
        self.print_data(self.data, max_test_len)
        return not has_failure
