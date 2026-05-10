# ===--- test_ci_optimizations.py -----------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

"""
Tests for CI clone optimizations:
  - Shallow clone (--skip-history → --depth 1)
  - Single branch  (--skip-history → --single-branch)
  - Partial clone  (--partial-clone → --filter=blob:none)
  - Shallow fetch  (--skip-history propagated to update_single_repository)
"""

import os

from . import scheme_mock

def _add_branch_to_remotes(remote_path, repo_names, branch_name):
    """Create *branch_name* on every bare remote repo."""
    for repo_name in repo_names:
        remote_repo = os.path.join(remote_path, repo_name)
        scheme_mock.call_quietly(
            ["git", "branch", branch_name, "main"],
            cwd=remote_repo,
        )


def _add_commit_to_repos(local_path, repo_names, filename, content):
    """Commit a new file to each local clone and push it to origin."""
    for repo_name in repo_names:
        local_repo = os.path.join(local_path, repo_name)
        filepath = os.path.join(local_repo, filename)
        with open(filepath, "w") as f:
            f.write(content)
        scheme_mock.call_quietly(["git", "add", filename], cwd=local_repo)
        scheme_mock.call_quietly(
            ["git", "commit", "-m", f"Add {filename}"], cwd=local_repo
        )
        scheme_mock.call_quietly(["git", "push", "origin", "main"], cwd=local_repo)


class ShallowCloneTestCase(scheme_mock.SchemeMockTestCase):
    """--skip-history should produce depth-1, single-branch clones."""

    def setUp(self):
        super().setUp()
        # Add a second branch to every bare remote so single-branch tests are
        # meaningful.
        _add_branch_to_remotes(self.remote_path, self.get_all_repos(), "other-branch")

    def test_skip_history_produces_shallow_clone(self):
        """Repos cloned with --skip-history must be depth-1 shallow clones."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
                "--skip-history",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            is_shallow = self.call(
                ["git", "rev-parse", "--is-shallow-repository"], cwd=repo_path
            ).strip()
            self.assertEqual(
                is_shallow,
                "true",
                f"'{repo}' should be a shallow clone after --skip-history",
            )

    def test_skip_history_fetches_only_single_branch(self):
        """--skip-history must not fetch unrelated remote branches."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
                "--skip-history",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            remote_branches = self.call(["git", "branch", "-r"], cwd=repo_path).strip()
            self.assertIn("origin/main", remote_branches)
            self.assertNotIn(
                "origin/other-branch",
                remote_branches,
                f"'{repo}' should not have fetched 'other-branch' (--single-branch)",
            )

    def test_regular_clone_fetches_all_branches(self):
        """A regular clone (no --skip-history) should fetch all remote branches."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            remote_branches = self.call(["git", "branch", "-r"], cwd=repo_path).strip()
            self.assertIn("origin/main", remote_branches)
            self.assertIn(
                "origin/other-branch",
                remote_branches,
                f"'{repo}' should have all branches without --skip-history",
            )

    def test_regular_clone_is_not_shallow(self):
        """A regular clone must have full history (not shallow)."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            is_shallow = self.call(
                ["git", "rev-parse", "--is-shallow-repository"], cwd=repo_path
            ).strip()
            self.assertEqual(
                is_shallow,
                "false",
                f"'{repo}' should have full history without --skip-history",
            )

class PartialCloneTestCase(scheme_mock.SchemeMockTestCase):
    """--partial-clone should configure --filter=blob:none on every repo."""

    def _get_partial_clone_filter(self, repo_path):
        """Return the configured partialclonefilter, or None if not set."""
        try:
            return self.call(
                ["git", "config", "remote.origin.partialclonefilter"],
                cwd=repo_path,
            ).strip()
        except scheme_mock.CallQuietlyException:
            return None

    def test_partial_clone_sets_filter(self):
        """--partial-clone must set remote.origin.partialclonefilter=blob:none."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
                "--partial-clone",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            filter_value = self._get_partial_clone_filter(repo_path)
            self.assertEqual(
                filter_value,
                "blob:none",
                f"'{repo}' should have partialclonefilter=blob:none",
            )

    def test_regular_clone_has_no_filter(self):
        """Without --partial-clone the filter config key must not be set."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            filter_value = self._get_partial_clone_filter(repo_path)
            self.assertIsNone(
                filter_value,
                f"'{repo}' should not have a partial clone filter without --partial-clone",
            )

    def test_partial_clone_combined_with_skip_history(self):
        """--partial-clone and --skip-history together: shallow AND filter."""
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--clone",
                "--scheme",
                "main",
                "--partial-clone",
                "--skip-history",
            ]
        )

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)

            is_shallow = self.call(
                ["git", "rev-parse", "--is-shallow-repository"], cwd=repo_path
            ).strip()
            self.assertEqual(
                is_shallow,
                "true",
                f"'{repo}' should be shallow with --skip-history",
            )

            filter_value = self._get_partial_clone_filter(repo_path)
            self.assertEqual(
                filter_value,
                "blob:none",
                f"'{repo}' should have partialclonefilter with --partial-clone",
            )


class ShallowUpdateTestCase(scheme_mock.SchemeMockTestCase):
    """update_single_repository must honor --skip-history when fetching."""

    _NEW_FILE = "ci_update_file.txt"

    def _push_new_commits(self):
        _add_commit_to_repos(
            self.local_path,
            self.get_all_repos(),
            self._NEW_FILE,
            "added by CI update test",
        )

    def test_update_with_skip_history_stays_shallow(self):
        """Repos must remain shallow after an update run with --skip-history."""
        base_cmd = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--scheme",
            "main",
            "--skip-history",
            "--reset-to-remote",
        ]

        self.call(base_cmd + ["--clone"])
        self._push_new_commits()
        self.call(base_cmd)

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            is_shallow = self.call(
                ["git", "rev-parse", "--is-shallow-repository"], cwd=repo_path
            ).strip()
            self.assertEqual(
                is_shallow,
                "true",
                f"'{repo}' should remain shallow after update with --skip-history",
            )

    def test_update_with_skip_history_tracks_latest_commit(self):
        """After an update with --skip-history, HEAD must match the remote tip."""
        base_cmd = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--scheme",
            "main",
            "--skip-history",
            "--reset-to-remote",
        ]

        self.call(base_cmd + ["--clone"])
        self._push_new_commits()
        self.call(base_cmd)

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            local_head = self.call(["git", "rev-parse", "HEAD"], cwd=repo_path).strip()
            remote_tip = self.call(
                ["git", "rev-parse", "origin/main"], cwd=repo_path
            ).strip()
            self.assertEqual(
                local_head,
                remote_tip,
                f"'{repo}' HEAD should equal origin/main after shallow update",
            )
