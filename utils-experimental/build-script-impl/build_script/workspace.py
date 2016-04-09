# build_script/workspace.py ------------- -----------------------*- python -*-
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
Represents whole source tree and the build directory.
"""
# ----------------------------------------------------------------------------

import os.path


class Workspace(object):
    def __init__(self, source_root, build_root):
        self._source_root = source_root
        self._build_root = build_root

    def source_root_dir(self):
        """Return path string of the source root directory.
        """
        return self._source_root

    def subdir(self, *components, **kwargs):
        """Join one or more path components to source_root_dir of this
        workspace. Return path string only if the composed path exists
        and is really a directory. None otherwise.
        """
        # FIXME: We should sanitize **kwargs. We only accepts `no_exist` only
        path = os.path.join(self._source_root, *components)
        if kwargs.get('no_exist', False) or os.path.isdir(path):
            return path
        return None

    def build_dir(self, deployment_target, product):
        """Return path string of the build directory for given
        deployment_target and product_name. The returned path may or may not
        exist.
        """
        return os.path.join(self._build_root,
                            product + '-' + deployment_target)

    def build_root_dir(self):
        """Return path string of the build *root* directory.
        """
        return self._build_root
