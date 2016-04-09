# utils/which.py ------------------------------------------------*- python -*-
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
A rough implementation of shutil.which() for Python 2.7

This can be removed if the Swift build toolchain migrates completely to
Python 3.3+.
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import sys
import os
import os.path


def _uniq_list(list_):
    '''\
    Return a list of unique elements from given list. Unlike
    `list(set(someList))`, this function keeps the order of original elements.

    >>> ary = [3,5,1,3,2,1,2]
    >>> _uniq_list(ary)
    [3,5,1,2]
    >>> ary = _uniq_list(reverse(ary))
    [2,1,3,5]
    '''
    output = []
    for item in list_:
        if item not in output:
            output.append(item)
    return output


def which(cmd):
    '''\
    Find and return the path to an executable from system executable paths.
    If not found return `None`.

    >>> which('a-tool-that-doesnt-exist')
    None
    >>> lspath = which('ls')
    >>> os.path.split(lspath)[0] == 'ls'
    True
    '''
    path = os.environ.get("PATH", os.defpath).split(os.pathsep)
    path = _uniq_list(os.path.realpath(p) for p in path)

    if sys.platform == 'win32':
        pathext = os.environ.get("PATHEXT", "").split(os.pathsep)
        cmds = [cmd + ext for ext in pathext]
    else:
        cmds = [cmd]

    for dir in path:
        for cmd in cmds:
            cmdpath = os.path.join(dir, cmd)
            is_executable = (os.path.exists(cmdpath) and
                             not os.path.isdir(cmdpath) and
                             os.access(cmdpath, os.X_OK))
            if is_executable:
                return cmdpath
    return None
