#!/usr/bin/env python

# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""
Utility script used to easily run the build_swift module unit tests.
"""


from __future__ import absolute_import, unicode_literals

import argparse
import os
import sys
import unittest


MODULE_DIR = os.path.abspath(os.path.dirname(__file__))
UTILS_DIR = os.path.abspath(os.path.join(MODULE_DIR, os.pardir))


def parse_args():
    parser = argparse.ArgumentParser(
        description='Utility script used to run the build_swift module unit '
                    'test suite.')

    parser.set_defaults(verbosity=1)

    parser.add_argument('-v', '--verbose',
                        dest='verbosity',
                        action='store_const',
                        const=2,
                        help='Verbose output')
    parser.add_argument('-q', '--quiet',
                        dest='verbosity',
                        action='store_const',
                        const=0,
                        help='Minimal output')

    parser.add_argument('-f', '--failfast',
                        action='store_true',
                        help='Stop on first failure')
    parser.add_argument('-c', '--catch',
                        action='store_true',
                        help='Catch control-C and display results')
    parser.add_argument('-b', '--buffer',
                        action='store_true',
                        help='Buffer stdout and stderr during test runs')

    parser.add_argument('-p', '--pattern',
                        default='test*.py',
                        help='Pattern to match tests ("%(default)s" default)')

    return parser.parse_args()


def main():
    args = parse_args()

    if args.catch:
        unittest.installHandler()

    runner = unittest.TextTestRunner(
        verbosity=args.verbosity,
        failfast=args.failfast,
        buffer=args.buffer)

    # Add the swift/utils directory to the Python path.
    sys.path.append(UTILS_DIR)

    # Discover all tests for the module.
    module_tests = unittest.defaultTestLoader.discover(
        MODULE_DIR, pattern=args.pattern)

    # Create and run test suite.
    suite = unittest.TestSuite()
    suite.addTests(module_tests)
    result = runner.run(suite)

    return not result.wasSuccessful()


if __name__ == '__main__':
    sys.exit(main())
