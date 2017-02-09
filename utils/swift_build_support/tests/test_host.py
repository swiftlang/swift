# test_host.py - Unit tests for swift_build_support.cmake -*-- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import platform
import unittest

import swift_build_support.host as sbs_host


class HostTestCase(unittest.TestCase):

    def test_system_memory(self):
        # We make sure that we get an integer back. If we get an integer back,
        # we know that we at least were able to get some sort of information
        # from the system and it could be parsed as an integer. This is just a
        # smoke test.
        supported_platforms = [('Darwin', 'x86_64')]

        mem = sbs_host.system_memory()

        if (platform.system(), platform.machine()) not in supported_platforms:
            self.assertIsNone(mem)
        else:
            self.assertIsInstance(mem, int)

    def test_lto_link_job_counts(self):
        # Make sure that:
        #
        # 1. we get back a dictionary with two keys in it, the first called
        # llvm, the other called swift.
        #
        # 2. The values associated with these keys is either None (if we do not
        # support the platform) or is an int that is reasonable (i.e. <
        # 100). The number 100 is just a heuristic number that is appropriate
        # currently since LTO uses so much memory. If and when that changes,
        # this number should change.
        supported_platforms = [('Darwin', 'x86_64')]
        reasonable_upper_bound_of_lto_threads = 100

        result = sbs_host.max_lto_link_job_counts()
        self.assertIsInstance(result, dict)
        self.assertEqual(len(result), 2)

        if (platform.system(), platform.machine()) not in supported_platforms:
            self.assertIsNone(result['llvm'])
            self.assertIsNone(result['swift'])
            return

        self.assertIsNotNone(result['llvm'])
        self.assertIsNotNone(result['swift'])
        self.assertIsInstance(result['llvm'], int)
        self.assertIsInstance(result['swift'], int)
        self.assertLess(result['llvm'], reasonable_upper_bound_of_lto_threads)
        self.assertLess(result['swift'], reasonable_upper_bound_of_lto_threads)
