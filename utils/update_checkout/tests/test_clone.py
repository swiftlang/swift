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
import subprocess
import sys
import shutil
from unittest.mock import patch
import contextlib
from io import StringIO

from . import scheme_mock
from update_checkout.update_checkout import obtain_all_additional_swift_sources, main


class CloneTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(CloneTestCase, self).__init__(*args, **kwargs)

    def test_simple_clone(self):
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            self.assertTrue(os.path.isdir(repo_path))

    def test_clone_with_additional_scheme(self):
        output = self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--config",
                self.additional_config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "extra",
                "--verbose",
            ]
        )

        # Test that we're actually checking out the 'extra' scheme based on the output
        self.assertIn("git checkout refs/heads/main", output)

    def test_clone_missing_repos(self):
        output = self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
            ]
        )
        self.assertNotIn(
            "You don't have all swift sources. Call this script with --clone to get them.",
            output,
        )

        repo = self.get_all_repos()[0]
        repo_path = os.path.join(self.source_root, repo)
        shutil.rmtree(repo_path)
        output = self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
            ]
        )
        self.assertIn(
            "You don't have all swift sources. Call this script with --clone to get them.",
            output,
        )

    def test_clone_with_git_config(self):
        """
        Test that git is cloning with the following settings:
         - core.symlinks = true
         - core.autocrlf = false
        """
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            output = subprocess.check_output(
                ["git", "-C", repo_path, "config", "--get", "core.symlinks"], text=True
            )
            self.assertIn("true", output)
            output = subprocess.check_output(
                ["git", "-C", repo_path, "config", "--get", "core.autocrlf"], text=True
            )
            self.assertIn("false", output)

    @patch("update_checkout.update_checkout.obtain_all_additional_swift_sources")
    @patch("sys.exit", return_value=None)
    def test_clone_with_incorrect_git_config(self, mock_exit, mock_obtain):
        repo = self.get_all_repos()[0]

        def side_effect(*args, **kwargs):
            result = obtain_all_additional_swift_sources(*args, **kwargs)
            repo_path = os.path.join(self.source_root, repo)
            subprocess.run(["git", "-C", repo_path, "config", "core.symlinks", "false"])
            subprocess.run(["git", "-C", repo_path, "config", "core.autocrlf", "true"])
            return result

        mock_obtain.side_effect = side_effect

        sys.argv = [
            "update-checkout",
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--clone",
        ]
        with contextlib.redirect_stdout(StringIO()) as stdout:
            main()
            output = stdout.getvalue()
            self.assertIn(
                f"[WARNING] '{repo}' was not cloned with 'core.symlinks=true'. This can cause build/tests failures.",
                output,
            )
            self.assertIn(
                f"[WARNING] '{repo}' was not cloned with 'core.autocrlf=false'. This can cause build/tests failures.",
                output,
            )

    @patch("update_checkout.update_checkout.obtain_all_additional_swift_sources")
    @patch("sys.exit", return_value=None)
    def test_clone_with_missing_git_config_entry(self, mock_exit, mock_obtain):
        repo = self.get_all_repos()[0]

        def side_effect(*args, **kwargs):
            result = obtain_all_additional_swift_sources(*args, **kwargs)
            repo_path = os.path.join(self.source_root, repo)
            subprocess.run(
                ["git", "-C", repo_path, "config", "--unset", "core.symlinks"]
            )
            subprocess.run(
                ["git", "-C", repo_path, "config", "--unset", "core.autocrlf"]
            )
            return result

        mock_obtain.side_effect = side_effect

        sys.argv = [
            "update-checkout",
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--clone",
        ]
        with contextlib.redirect_stdout(StringIO()):
            main()

    @patch("update_checkout.update_checkout.obtain_all_additional_swift_sources")
    @patch("sys.exit", return_value=None)
    def test_clone_with_retry(self, mock_exit, mock_obtain):
        call_count = [0]

        def side_effect(*args, **kwargs):
            call_count[0] += 1
            if call_count[0] <= 1:
                return ([], [("fake-repo", "Simulated failure")])
            else:
                return obtain_all_additional_swift_sources(*args, **kwargs)

        mock_obtain.side_effect = side_effect

        # TODO: eventually call update_checkout using `main` rather than
        # `subprocess.call` to be able to mock some methods.
        sys.argv = [
            "update-checkout",
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--clone",
            "--max-retries",
            "1",
        ]
        with contextlib.redirect_stdout(StringIO()):
            main()

        self.assertEqual(call_count[0], 2)


class SchemeWithHashTestCase(scheme_mock.SchemeMockTestCase):
    def __init__(self, *args, **kwargs):
        super(SchemeWithHashTestCase, self).__init__(*args, **kwargs)

    def setUp(self):
        import json

        super().setUp()
        remote_repo_path = os.path.join(self.workspace, "remote", "repo1")

        commits = (
            self.call(["git", "rev-list", "main"], cwd=remote_repo_path, text=True)
            .strip()
            .split("\n")
        )

        self.commit_hash = commits[-1]
        self.commit_scheme_name = "commit-hash-scheme"
        commit_scheme = {
            "aliases": [self.commit_scheme_name],
            "repos": {
                "repo1": self.commit_hash,
                "repo2": "main",
            },
        }
        self.add_branch_scheme(self.commit_scheme_name, commit_scheme)

        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

    def test_clone_with_skip_history(self):
        self._test_clone_with_commit_hash(["--skip-history"])

    def test_clone_with_reset_to_remote(self):
        self._test_clone_with_commit_hash(["--reset-to-remote"])

    def _test_clone_with_commit_hash(self, additional_flags):
        """
        Test that cloning works with commit hashes.
        """
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                self.commit_scheme_name,
                "--verbose",
            ] + additional_flags
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            self.assertTrue(os.path.isdir(repo_path))

        current_commit = self.call(
            ["git", "rev-parse", "HEAD"], cwd=os.path.join(self.source_root, "repo1")
        ).strip()
        self.assertEqual(current_commit, self.commit_hash)


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

        missing_repo_path = os.path.join(self.source_root, self.get_all_repos().pop())
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


class SchemeWithAdditionalChanges(scheme_mock.SchemeMockTestCase):
    def __init__(self, *args, **kwargs):
        super(SchemeWithAdditionalChanges, self).__init__(*args, **kwargs)

    def _add_commit_to_repo1(self):
        local_repo_path = os.path.join(self.local_path, "repo1")
        filename = "additional_content.txt"
        filename_path = os.path.join(local_repo_path, filename)
        with open(filename_path, "w") as f:
            f.write("More stuff")
        self.call(["git", "add", filename], cwd=local_repo_path)
        self.call(["git", "commit", "-m", "Additional commit"], cwd=local_repo_path)
        self.call(["git", "push", "origin", "main"], cwd=local_repo_path)

    def test_call_update_checkout_twice_with_additional_commit(self):
        """
        Test that calling update checkout a second time will pick up
        additional commits done after the first time.
        """
        update_checkout_command = [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
                "--verbose",
                "--reset-to-remote",
            ]

        self.call(update_checkout_command)
        first_commit = self.call(
            ["git", "rev-parse", "HEAD"], cwd=os.path.join(self.source_root, "repo1")
        ).strip()

        self._add_commit_to_repo1()
        self.call(update_checkout_command)
        second_commit = self.call(
            ["git", "rev-parse", "HEAD"], cwd=os.path.join(self.source_root, "repo1")
        ).strip()

        self.assertNotEqual(first_commit, second_commit)
