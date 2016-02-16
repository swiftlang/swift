# test_tar.py - Unit tests for swift_build_support.tar -*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors

import os
import subprocess
import tempfile
import unittest

from swift_build_support.tar import tar


class TarTestCase(unittest.TestCase):
	def test_tar_this_file_succeeds(self):
		# `tar` complains about absolute paths, so use a relative path here.
		source = os.path.relpath(__file__)
		_, destination = tempfile.mkstemp()
		tar(source=source, destination=destination)

	def test_tar_nonexistent_file_raises(self):
		with self.assertRaises(subprocess.CalledProcessError):
			tar(source='/path/to/something/that/surely/doesnt/exist',
				destination='/another/path/that/shouldnt/exist')


if __name__ == '__main__':
	unittest.main()
