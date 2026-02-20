# ===--- test_stash.py ----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import os

from . import scheme_mock


class StashTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(StashTestCase, self).__init__(*args, **kwargs)

    def _clone_repos(self):
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

    def _run_stash_update(self):
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--stash",
            ]
        )

    def test_stash_removes_untracked_files(self):
        """Test that --stash removes untracked files via git clean."""
        self._clone_repos()

        # Create untracked files in each cloned repo.
        untracked = {}
        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            untracked_file = os.path.join(repo_path, "untracked.txt")
            with open(untracked_file, "w") as f:
                f.write("untracked content")
            untracked[repo] = untracked_file
            self.assertTrue(os.path.exists(untracked_file))

        self._run_stash_update()

        # Verify untracked files were cleaned.
        for repo, untracked_file in untracked.items():
            self.assertFalse(
                os.path.exists(untracked_file),
                f"Untracked file in '{repo}' should have been removed by --stash",
            )

    def test_stash_resets_tracked_changes(self):
        """Test that --stash resets modified tracked files via git reset."""
        self._clone_repos()

        # Modify a tracked file in each cloned repo.
        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            tracked_file = os.path.join(repo_path, "A.txt" if repo == "repo1" else "X.txt")
            with open(tracked_file, "r") as f:
                original = f.read()
            with open(tracked_file, "w") as f:
                f.write("modified content")
            # Confirm the file was actually changed.
            with open(tracked_file, "r") as f:
                self.assertEqual(f.read(), "modified content")

        self._run_stash_update()

        # Verify tracked files were reset to their committed state.
        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)
            tracked_file = os.path.join(repo_path, "A.txt" if repo == "repo1" else "X.txt")
            with open(tracked_file, "r") as f:
                contents = f.read()
            self.assertNotEqual(
                contents,
                "modified content",
                f"Tracked file in '{repo}' should have been reset by --stash",
            )

    def test_stash_handles_mixed_changes(self):
        """Test that --stash handles both tracked modifications and untracked
        files across multiple repositories simultaneously."""
        self._clone_repos()

        untracked_files = []
        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)

            # Modify a tracked file.
            tracked_file = os.path.join(repo_path, "A.txt" if repo == "repo1" else "X.txt")
            with open(tracked_file, "w") as f:
                f.write("modified content")

            # Create an untracked file.
            untracked_file = os.path.join(repo_path, "new_file.txt")
            with open(untracked_file, "w") as f:
                f.write("new file content")
            untracked_files.append(untracked_file)

        self._run_stash_update()

        for repo in self.get_all_repos():
            repo_path = os.path.join(self.source_root, repo)

            # Verify tracked file was reset.
            tracked_file = os.path.join(repo_path, "A.txt" if repo == "repo1" else "X.txt")
            with open(tracked_file, "r") as f:
                self.assertNotEqual(
                    f.read(),
                    "modified content",
                    f"Tracked file in '{repo}' should have been reset",
                )

        # Verify untracked files were removed.
        for untracked_file in untracked_files:
            self.assertFalse(
                os.path.exists(untracked_file),
                f"Untracked file should have been removed by --stash",
            )
