# ===--- test_clone.py ----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import os

from . import scheme_mock


class CloneTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(CloneTestCase, self).__init__(*args, **kwargs)

    def test_simple_clone(self):
        self.call([self.update_checkout_path,
                   '--config', self.config_path,
                   '--source-root', self.source_root,
                   '--clone'])

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            self.assertTrue(os.path.isdir(repo_path))

    def test_clone_with_additional_scheme(self):
        output = self.call([self.update_checkout_path,
                            '--config', self.config_path,
                            '--config', self.additional_config_path,
                            '--source-root', self.source_root,
                            '--clone',
                            '--scheme', 'extra'])

        # Test that we're actually checking out the 'extra' scheme based on the output
        self.assertIn(b"git checkout refs/heads/main", output)


class SchemeWithMissingRepoTestCase(scheme_mock.SchemeMockTestCase):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

        self.base_args = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
        ]

        repos = self.get_all_repos()
        repos.pop()

        self.scheme_name = "missing-repo"
        scheme = {
            "aliases": [self.scheme_name],
            "repos": dict((repo, "main") for repo in repos),
        }
        self.add_branch_scheme(self.scheme_name, scheme)

    # Test that we do not clone a repository that is not listed in the
    # given branch-scheme.
    def test_clone(self):
        self.call(self.base_args + ["--scheme", self.scheme_name, "--clone"])

        missing_repo_path = os.path.join(
            self.source_root, self.get_all_repos().pop()
        )
        self.assertFalse(os.path.isdir(missing_repo_path))

    # Test that we do not update a repository that is not listed in the given
    # branch-scheme---doing so will cause an irrelevant failure in an attempt
    # to query the branch-scheme about the target branch for that repository.
    def test_update(self):
        # First, clone using the default branch-scheme, which has mappings for
        # all repositories.
        self.call(self.base_args + ["--clone"])

        # Then, update using our custom scheme---a subset of the default one.
        self.call(self.base_args + ["--scheme", self.scheme_name])
