# ===--- SchemeMock.py ----------------------------------------------------===#
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https:#swift.org/LICENSE.txt for license information
#  See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#
"""This file defines objects for mocking an update-checkout scheme. It creates
a json .config file and a series of .git repos with "fake commits".
"""

import json
import os
import subprocess
import unittest

# For now we only use a config with a single scheme. We should add support for
# handling multiple schemes.
MOCK_REMOTE = {
    'repo1': [
        # This is a series of changes to repo1. (File, NewContents)
        ('A.txt', 'A'),
        ('B.txt', 'B'),
        ('A.txt', 'a'),
    ],
    'repo2': [
        # This is a series of changes to repo1. (File, NewContents)
        ('X.txt', 'X'),
        ('Y.txt', 'Y'),
        ('X.txt', 'z'),
    ],
}

MOCK_CONFIG = {
    # This is here just b/c we expect it. We should consider consolidating
    # clone-patterns into a dictionary where we map protocols (i.e. ['ssh,
    # 'https'] to patterns). Then we can define this issue.
    'ssh-clone-pattern': 'DO_NOT_USE',
    # We reset this value with our remote path when we process
    'https-clone-pattern': '',
    'repos': {
        'repo1': {
            'remote': {'id': 'repo1'},
        },
        'repo2': {
            'remote': {'id': 'repo2'},
        },
    },
    'default-branch-scheme': 'master',
    'branch-schemes': {
        'master': {
            'aliases': ['master'],
            'repos': {
                'repo1': 'master',
                'repo2': 'master',
            }
        }
    }
}


def call_quietly(*args, **kwargs):
    with open(os.devnull, 'w') as f:
        kwargs['stdout'] = f
        kwargs['stderr'] = f
        subprocess.check_call(*args, **kwargs)


def create_dir(d):
    if not os.path.isdir(d):
        os.makedirs(d)


def teardown_mock_remote(base_dir):
    call_quietly(['rm', '-rf', base_dir])


def get_config_path(base_dir):
    return os.path.join(base_dir, 'test-config.json')


def setup_mock_remote(base_dir):
    create_dir(base_dir)

    # We use local as a workspace for creating commits.
    LOCAL_PATH = os.path.join(base_dir, 'local')
    # We use remote as a directory that simulates our remote unchecked out
    # repo.
    REMOTE_PATH = os.path.join(base_dir, 'remote')

    create_dir(REMOTE_PATH)
    create_dir(LOCAL_PATH)

    for (k, v) in MOCK_REMOTE.items():
        local_repo_path = os.path.join(LOCAL_PATH, k)
        remote_repo_path = os.path.join(REMOTE_PATH, k)
        create_dir(remote_repo_path)
        create_dir(local_repo_path)
        call_quietly(['git', 'init', '--bare', remote_repo_path])
        call_quietly(['git', 'clone', '-l', remote_repo_path, local_repo_path])
        for (i, (filename, contents)) in enumerate(v):
            filename_path = os.path.join(local_repo_path, filename)
            with open(filename_path, 'w') as f:
                f.write(contents)
            call_quietly(['git', 'add', filename], cwd=local_repo_path)
            call_quietly(['git', 'commit', '-m', 'Commit %d' % i],
                         cwd=local_repo_path)
            call_quietly(['git', 'push', 'origin', 'master'],
                         cwd=local_repo_path)

    base_config = MOCK_CONFIG
    https_clone_pattern = os.path.join('file://%s' % REMOTE_PATH, '%s')
    base_config['https-clone-pattern'] = https_clone_pattern

    with open(get_config_path(base_dir), 'w') as f:
        json.dump(base_config, f)

    return (LOCAL_PATH, REMOTE_PATH)


BASEDIR_ENV_VAR = 'UPDATECHECKOUT_TEST_WORKSPACE_DIR'
CURRENT_FILE_DIR = os.path.dirname(os.path.abspath(__file__))
UPDATE_CHECKOUT_PATH = os.path.abspath(os.path.join(CURRENT_FILE_DIR,
                                                    os.path.pardir,
                                                    os.path.pardir,
                                                    'update-checkout'))


class SchemeMockTestCase(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(SchemeMockTestCase, self).__init__(*args, **kwargs)

        self.workspace = os.getenv(BASEDIR_ENV_VAR)
        if self.workspace is None:
            raise RuntimeError('Misconfigured test suite! Environment '
                               'variable %s must be set!' % BASEDIR_ENV_VAR)
        self.config_path = get_config_path(self.workspace)
        self.update_checkout_path = UPDATE_CHECKOUT_PATH
        if not os.access(self.update_checkout_path, os.X_OK):
            raise RuntimeError('Error! Could not find executable '
                               'update-checkout at path: %s'
                               % self.update_checkout_path)
        self.source_root = os.path.join(self.workspace, 'source_root')

    def setUp(self):
        create_dir(self.source_root)
        (self.local_path, self.remote_path) = setup_mock_remote(self.workspace)

    def tearDown(self):
        teardown_mock_remote(self.workspace)

    def call(self, *args, **kwargs):
        kwargs['cwd'] = self.source_root
        call_quietly(*args, **kwargs)
