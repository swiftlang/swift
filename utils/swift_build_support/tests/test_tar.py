# test_tar.py - Unit tests for swift_build_support.tar -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import platform
import sys
import tempfile
import unittest
try:
    from StringIO import StringIO
except ImportError:
    from io import StringIO

from swift_build_support.tar import tar


class TarTestCase(unittest.TestCase):
    def setUp(self):
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout
        sys.stderr = self.stderr

    def tearDown(self):
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr

    def test_tar_this_file_succeeds(self):
        # `tar` complains about absolute paths, so use a relative path here.
        source = os.path.relpath(__file__)
        _, destination = tempfile.mkstemp()
        tar(source=source, destination=destination)

        if platform.system() == "Darwin":
            expect = "+ tar -c -z -f {dest} {source}\n"
        else:
            expect = "+ tar -c -z -f {dest} --owner=0 --group=0 {source}\n"

        self.assertEqual(self.stdout.getvalue(), "")
        self.assertEqual(self.stderr.getvalue(),
                         expect.format(dest=destination, source=source))

    def test_tar_nonexistent_file_raises(self):
        with self.assertRaises(SystemExit):
            tar(source='/path/to/something/that/surely/doesnt/exist',
                destination='/another/path/that/shouldnt/exist')


if __name__ == '__main__':
    unittest.main()
