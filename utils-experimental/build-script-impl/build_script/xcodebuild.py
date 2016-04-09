# build_script/xcodebuild.py ------------------------------------*- python -*-
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
Represent xcodebuild command
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import re
from . import shell


class XcodebuildOptions(object):
    def __init__(self, *args):
        self._options = []
        for var, value in args:
            self.define(var, value)

    def define(self, var, value):
        # Strip type suffix.
        var = re.sub(':[^:]*$', '', var)
        self._options.append(var + "=" + value)

    def __len__(self):
        return self._options.__len__()

    def __iter__(self):
        return self._options.__iter__()

    def __add__(self, other):
        ret = XcodebuildOptions()
        ret._options += self._options
        ret._options += list(other)
        return ret

    def __iadd__(self, other):
        self._options += list(other)
        return self


class Xcodebuild(object):

    def configure():
        pass  # no configure for xcodebuild projects

    def build(self, project_dir, target, configuration, action, options):
        build_cmd = ['xcodebuild',
                     '-target', target,
                     '-configuration', configuration]

        if action != "build":
            # We don't need to pass "build" action
            build_cmd += [action, ]

        build_cmd += options

        # Do build
        with shell.pushd(project_dir):
            shell.invoke(build_cmd)

    def build_workspace(self, workspace, scheme, options):
        build_cmd = ['xcodebuild',
                     '-workspace', workspace,
                     '-scheme', scheme]
        build_cmd += options

        shell.invoke(build_cmd)
