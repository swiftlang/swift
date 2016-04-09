# tests/test_workspace.py ---------------------------------------*- python -*-
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
import os
import shutil
import tempfile

from build_script.workspace import Workspace


class WorkspaceTestCase(unittest.TestCase):

    def test_workspace(self):

        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())

        os.makedirs(os.path.join(tmpdir1, 'foo'))
        os.makedirs(os.path.join(tmpdir1, 'foo', 'src'))

        workspace = Workspace(source_root=tmpdir1,
                              build_root=tmpdir2)

        self.assertEqual(workspace.source_root_dir(), tmpdir1)
        self.assertEqual(workspace.build_root_dir(), tmpdir2)

        # Return source directory if exists
        self.assertEqual(workspace.subdir('foo'),
                         os.path.join(tmpdir1, 'foo'))
        self.assertEqual(workspace.subdir('foo', 'src'),
                         os.path.join(tmpdir1, 'foo', 'src'))

        # Return None if not exists
        self.assertIsNone(workspace.subdir('baz'))
        self.assertIsNone(workspace.subdir('foo', 'not-exists'))

        # Return path even if not exists
        self.assertEqual(workspace.subdir('baz', no_exist=True),
                         os.path.join(tmpdir1, 'baz'))
        self.assertEqual(workspace.subdir('foo', 'not-exists', no_exist=True),
                         os.path.join(tmpdir1, 'foo', 'not-exists'))

        # build_dir() always return the path
        self.assertEqual(workspace.build_dir('target', 'product'),
                         os.path.join(tmpdir2, 'product-target'))

        shutil.rmtree(tmpdir1)
        shutil.rmtree(tmpdir2)
