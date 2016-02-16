# swift_build_support/debug.py - Print information on the build -*- python -*-
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
#
# Convenient functions for printing out information on the build process.
#
# ----------------------------------------------------------------------------

from __future__ import print_function

import subprocess
import sys


def _output(args):
    try:
        out = subprocess.check_output(args, stderr=subprocess.PIPE)
        return out.rstrip()
    except subprocess.CalledProcessError:
        return None


def print_xcodebuild_versions(sdks, file=sys.stdout):
	"""
	Print the host machine's `xcodebuild` version, as well as version
	information for each of the given SDKs (for a full list of available
	SDKs, invoke `xcodebuild -showsdks` on the command line).
	"""
	print(u'--- SDK versions ---', file=file)
	print(u'{}\n'.format(_output(['xcodebuild', '-version'])), file=file)
	for sdk in sdks:
		print(u'{}\n'.format(_output(['xcodebuild', '-version', '-sdk', sdk])),
			  file=file)
