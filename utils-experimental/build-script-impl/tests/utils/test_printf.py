# tests/utils/test_printf.py ------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import unittest
import sys
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from build_script.utils import printf, printf_with_argv0


class PrintfTestCase(unittest.TestCase):

    def test_printf(self):

        out = StringIO()
        sys.stdout = out

        printf("Hello {1} {0}", "build_script", "Swift")

        self.assertEqual(out.getvalue(), "Hello Swift build_script\n")

        sys.stdout = sys.__stdout__

    def test_printf_with_argv0(self):

        out = StringIO()
        sys.stdout = out

        printf_with_argv0("Hello {1} {0}", "build_script", "Swift")

        self.assertEqual(out.getvalue(),
                         sys.argv[0] + ": Hello Swift build_script\n")

        sys.stdout = sys.__stdout__
