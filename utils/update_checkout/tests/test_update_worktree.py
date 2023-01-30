# ===--- test_clone.py ----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import os

from . import scheme_mock
from .scheme_mock import call_quietly


WORKTREE_NAME = "feature"


def path_for_worktree(workspace_path, worktree_name):
    return os.path.join(workspace_path, worktree_name)


def setup_worktree(workspace_path, local_path, worktree_name):
    worktree_path = path_for_worktree(workspace_path, worktree_name)
    os.makedirs(worktree_path)
    for project in os.listdir(local_path):
        local_project_path = os.path.join(local_path, project)
        worktree_project_path = os.path.join(worktree_path, project)
        call_quietly(['git',
                      '-C', local_project_path,
                      'worktree', 'add', worktree_project_path])


def teardown_worktree(workspace_path, local_path, worktree_name):
    worktree_path = path_for_worktree(workspace_path, worktree_name)
    for project in os.listdir(local_path):
        local_project_path = os.path.join(local_path, project)
        worktree_project_path = os.path.join(worktree_path, project)
        call_quietly(['git',
                      '-C', local_project_path,
                      'worktree', 'remove', worktree_project_path])


class WorktreeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(WorktreeTestCase, self).__init__(*args, **kwargs)

    def test_worktree(self):
        self.call([self.update_checkout_path,
                   '--config', self.config_path,
                   '--source-root', self.worktree_path,
                   '--scheme', 'main'])

    def setUp(self):
        super(WorktreeTestCase, self).setUp()
        self.worktree_path = os.path.join(self.workspace, WORKTREE_NAME)
        setup_worktree(self.workspace, self.local_path, WORKTREE_NAME)

    def tearDown(self):
        teardown_worktree(self.workspace, self.local_path, WORKTREE_NAME)
        super(WorktreeTestCase, self).tearDown()
