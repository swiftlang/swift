#!/usr/bin/python
# -*- coding: utf-8 -*-

# ===--- test_Benchmark_Driver.py ----------------------------------------===//
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

import os
import unittest

from imp import load_source

from test_utils import captured_output
# import Benchmark_Driver  # doesn't work because it misses '.py' extension
Benchmark_Driver = load_source(
    'Benchmark_Driver', os.path.join(os.path.dirname(
        os.path.abspath(__file__)), 'Benchmark_Driver'))
# from Benchmark_Driver import parse_args
parse_args = Benchmark_Driver.parse_args


class Test_parse_args(unittest.TestCase):
    def assert_contains(self, texts, output):
        assert not isinstance(texts, str)
        for text in texts:
            self.assertIn(text, output)

    def test_requires_command_argument(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit, parse_args, [])
        self.assert_contains(['usage:', 'COMMAND', 'too few arguments'],
                             err.getvalue())

    def test_command_help_lists_commands(self):
        with captured_output() as (out, _):
            self.assertRaises(SystemExit, parse_args, ['-h'])
        self.assert_contains(['COMMAND', 'run', 'compare'],
                             out.getvalue())

    def test_run_benchmarks_by_name_or_ordinal(self):
        benchmarks = ['AngryPhonebook', '42']
        self.assertEquals(
            parse_args(['run'] + benchmarks).benchmarks, benchmarks)

    def test_run_benchmarks_matching_pattern(self):
        regexes = ['Prefix', '.*Suffix.*']
        filters = ['-f', regexes[0], '-f', regexes[1]]
        self.assertEquals(parse_args(['run'] + filters).filters, regexes)

    def test_run_benchmarks_and_filters_are_exclusive(self):
        with captured_output() as (_, err):
            self.assertRaises(SystemExit,
                              parse_args, 'run -f Filter1 Benchmark1'.split())
        self.assert_contains(
            ['error',
             'argument BENCHMARK: not allowed with argument -f/--filter'],
            err.getvalue())

    def test_tests_location(self):
        here = os.path.dirname(os.path.abspath(__file__))
        self.assertEquals(parse_args(['run']).tests, here)
        tests = '/benchmarks/are/here'
        self.assertEquals(parse_args(['run', '-t', tests]).tests, tests)

    def test_optimization_argument(self):
        self.assertEquals(parse_args(['run']).optimization, 'O')
        self.assertEquals(
            parse_args(['run', '-o', 'O']).optimization, 'O')
        self.assertEquals(
            parse_args(['run', '-o', 'Onone']).optimization, 'Onone')
        self.assertEquals(
            parse_args(['run', '-o', 'Osize']).optimization, 'Osize')

        with captured_output() as (_, err):
            self.assertRaises(SystemExit,
                              parse_args, ['run', '-o', 'bogus'])
        self.assert_contains(
            ['error:',
             "argument -o/--optimization: invalid choice: 'bogus'",
             "(choose from 'O', 'Onone', 'Osize')"],
            err.getvalue())


if __name__ == '__main__':
    unittest.main()
