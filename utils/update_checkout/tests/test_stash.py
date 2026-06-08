# ===--- test_stash.py ----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import os

from . import scheme_mock

UNTRACKED_FILENAME = "untracked.txt"
UNTRACKED_CONTENTS = "An untracked file.\n"
TRACKED_CONTENTS = "Local changes to a tracked file.\n"


class LocalChangesTestCase(scheme_mock.SchemeMockTestCase):
    """Tests for the '--stash' and '--clean' options, which differ in
    whether the local changes they clear out are preserved in a stash.
    """

    def __init__(self, *args, **kwargs):
        super(LocalChangesTestCase, self).__init__(*args, **kwargs)

        self.base_args = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
        ]

    def setUp(self):
        super().setUp()
        self.call(self.base_args + ["--clone"])
        for repo_path in self._repo_paths():
            # 'git stash' commits, so make sure an identity is configured
            # even if the test machine has none.
            self.call(
                ["git", "config", "--local", "user.name", "swift_test"],
                cwd=repo_path,
            )
            self.call(
                ["git", "config", "--local", "user.email", "no-reply@swift.org"],
                cwd=repo_path,
            )

    def _repo_paths(self):
        return [os.path.join(self.source_root, repo) for repo in self.get_all_repos()]

    def _tracked_file(self, repo_path):
        """The first file committed to the given mock repository."""
        repo = os.path.basename(repo_path)
        return scheme_mock.MOCK_REMOTE[repo][0][0]

    def _make_local_changes(self):
        for repo_path in self._repo_paths():
            with open(os.path.join(repo_path, UNTRACKED_FILENAME), "w") as f:
                f.write(UNTRACKED_CONTENTS)
            with open(os.path.join(repo_path, self._tracked_file(repo_path)), "w") as f:
                f.write(TRACKED_CONTENTS)

    def _assert_working_tree_clean(self, repo_path):
        untracked_path = os.path.join(repo_path, UNTRACKED_FILENAME)
        self.assertFalse(
            os.path.exists(untracked_path),
            f"{untracked_path} should no longer be in the working tree",
        )
        status = self.call(["git", "status", "--porcelain"], cwd=repo_path)
        self.assertEqual(
            status.strip(),
            "",
            f"{repo_path} should have a clean working tree, got:\n{status}",
        )

    def _stash_list(self, repo_path):
        return self.call(["git", "stash", "list"], cwd=repo_path).strip()

    def test_stash_preserves_local_changes(self):
        self._make_local_changes()

        self.call(self.base_args + ["--stash"])

        for repo_path in self._repo_paths():
            self._assert_working_tree_clean(repo_path)
            self.assertNotEqual(
                self._stash_list(repo_path),
                "",
                f"{repo_path} should have the local changes stashed",
            )

            # Make sure the changes actually pop back out of the stash.
            self.call(["git", "stash", "pop"], cwd=repo_path)
            with open(os.path.join(repo_path, UNTRACKED_FILENAME)) as f:
                self.assertEqual(f.read(), UNTRACKED_CONTENTS)
            with open(os.path.join(repo_path, self._tracked_file(repo_path))) as f:
                self.assertEqual(f.read(), TRACKED_CONTENTS)

    def test_clean_discards_local_changes(self):
        self._make_local_changes()

        self.call(self.base_args + ["--clean"])

        for repo_path in self._repo_paths():
            self._assert_working_tree_clean(repo_path)
            self.assertEqual(
                self._stash_list(repo_path),
                "",
                f"{repo_path} should have the local changes deleted, not stashed",
            )
