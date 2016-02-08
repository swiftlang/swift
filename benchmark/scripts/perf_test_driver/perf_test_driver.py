#!/usr/bin/env python

# ===--- perf_test_driver.py ----------------------------------------------===//
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See http://swift.org/LICENSE.txt for license information
#  See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===//

import os
import subprocess
import multiprocessing
import re

class Result(object):
    def __init__(self, name, status, output, xfail_list):
        self.name = name
        self.status = status
        self.output = output
        self.is_xfailed = any((re.match(x, self.name) is not None for x in xfail_list))

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

    def get_status(self):
        return self.status

    def get_output(self):
        return self.output

    def get_data(self):
        return self.data

    def merge_in_extra_data(self, d):
        """Rather than modifying the extra data dict, just return it as a no-op"""
        return d

def _unwrap_self(args):
    return type(args[0]).process_input(*args)

class BenchmarkDriver(object):

    OptLevels = ['Onone', 'O', 'Ounchecked']

    def __init__(self, binary_dir, xfail_list, enable_parallel=False):
        self.targets = [(os.path.join(binary_dir, 'Benchmark_%s' % o), o) for o in BenchmarkDriver.OptLevels]
        self.xfail_list = xfail_list
        self.enable_parallel = enable_parallel
        self.data = None

    def prepare_input(self, name, opt_level):
        raise RuntimeError("Abstract method")

    def process_input(self, data):
        raise RuntimeError("Abstract method")

    def run_for_opt_level(self, binary, opt_level):
        print("testing driver at path: %s" % binary)
        names = [n.strip() for n in subprocess.check_output([binary, "--list"]).split()[2:]]

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

        return reduce(reduce_results, results, {'result': [], 'has_failure': False, 'max_test_len': 0, 'extra_data': {}})

    def print_data(self, data, max_test_len):
        print("Results:")
        fmt = '{:<%d}{:}' % (max_test_len + 5)
        for d in data:
            for r in d['result']:
                print(fmt.format(r.get_name(), r.get_result()))

    def run(self):
        self.data = [self.run_for_opt_level(binary, opt_level) for binary, opt_level in self.targets]
        max_test_len = reduce(max, [d['max_test_len']for d in self.data])
        has_failure = reduce(max, [d['has_failure']for d in self.data])
        self.print_data(self.data, max_test_len)
        return not has_failure
