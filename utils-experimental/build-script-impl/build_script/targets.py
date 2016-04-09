# build_script/targets.py ---------------------------------------*- python -*-
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
"""
Utility functions for deployment targets
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import


def split(target):
    res = target.split('-', 1)
    if len(res) != 2:
        res = (res[0], None)
    return tuple(res)


def is_darwin_type(target):
    sys, _ = split(target)
    return sys in [
        'macosx',
        'iphoneos', 'iphonesimulator',
        'appletvos', 'appletvsimulator',
        'watchos', 'watchsimulator']


def is_osx(target):
    sys, _ = split(target)
    return sys == 'macosx'


def xcrun_sdk_name(target):
    sys, _ = split(target)
    return sys
