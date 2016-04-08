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


def get_xcode_sdks():
    """
    A generator for canonical Xcode SDK identifiers (e.g. 'macosx10.11' or
    'iphonesimulator9.2') available in the currently selected Xcode.

    :return: A generator of SDK identifiers (strings).
    """
    try:
        command = ['xcodebuild', '-showsdks']
        out = subprocess.check_output(command, stderr=subprocess.PIPE)
        lines = [line.rstrip() for line in out.splitlines()]
        import pdb
        sdk_flag = '-sdk'
        for line in lines:
            if sdk_flag in line:
                prefix, identifier = line.split('-sdk')
                yield identifier.lstrip().rstrip()
    except subprocess.CalledProcessError:
        pass


def print_xcodebuild_versions(file=sys.stdout):
    """
    Print the host machine's `xcodebuild` version, as well as version
    information for all available SDKs.
    """
    print(u'{}\n'.format(_output(['xcodebuild', '-version'])), file=file)
    print(u'--- SDK versions ---', file=file)
    print(u'{}\n'.format(_output(['xcodebuild', '-version', '-sdk'])),
          file=file)
    # You can't test beyond this because each developer's machines may have
    # a different set of SDKs installed.

