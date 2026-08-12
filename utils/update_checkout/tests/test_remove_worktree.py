# ===--- test_remove_worktree.py ------------------------------------------===#
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


ADD_WORKTREE_PATH = os.path.abspath(
    os.path.join(
        scheme_mock.CURRENT_FILE_DIR,
        os.path.pardir,
        os.path.pardir,
        "add-worktree",
    )
)

REMOVE_WORKTREE_PATH = os.path.abspath(
    os.path.join(
        scheme_mock.CURRENT_FILE_DIR,
        os.path.pardir,
        os.path.pardir,
        "remove-worktree",
    )
)


class RemoveWorktreeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

    def clone_source_root(self):
        self.call(self.base_args + ["--clone"])

    def create_worktrees(self):
        self.call(
            [ADD_WORKTREE_PATH, "--source-root", self.source_root, self.worktree_dir]
        )

    def add_worktrees(self):
        self.clone_source_root()
        self.create_worktrees()

    def dirty(self, repo):
        with open(os.path.join(self.worktree_dir, repo, "dirty.txt"), "w") as f:
            f.write("uncommitted\n")

    def add_submodule_worktrees(self, repo):
        """Sets up worktrees where `repo` contains a populated submodule, and
        returns the submodule path inside that repo's worktree.

        The submodule must be committed to the source repository before the
        worktree is created, since the worktree is detached at the source HEAD.
        git refuses to `worktree remove` a worktree containing submodules unless
        forced, so this exercises that path.
        """
        self.clone_source_root()

        submodule_origin = os.path.join(self.workspace, "submodule-origin")
        self.call(["git", "init", submodule_origin])
        self.call(
            ["git", "-C", submodule_origin, "commit", "--allow-empty", "-m", "sub"]
        )

        source_repo = os.path.join(self.source_root, repo)
        # 'protocol.file.allow=always' is required for submodules over file://.
        self.call(
            [
                "git",
                "-c",
                "protocol.file.allow=always",
                "-C",
                source_repo,
                "submodule",
                "add",
                submodule_origin,
                "sub",
            ]
        )
        self.call(["git", "-C", source_repo, "commit", "-m", "add submodule"])

        self.create_worktrees()

        worktree_repo = os.path.join(self.worktree_dir, repo)
        self.call(
            [
                "git",
                "-c",
                "protocol.file.allow=always",
                "-C",
                worktree_repo,
                "submodule",
                "update",
                "--init",
                "--recursive",
            ]
        )
        return os.path.join(worktree_repo, "sub")

    def worktree_exists(self, repo):
        return os.path.exists(os.path.join(self.worktree_dir, repo))

    def test_removes_all_worktrees(self):
        self.add_worktrees()

        self.call([REMOVE_WORKTREE_PATH, self.worktree_dir])

        # Once every worktree is removed, the worktree directory is deleted.
        self.assertFalse(os.path.exists(self.worktree_dir))

    def test_skips_dirty_without_force(self):
        self.add_worktrees()
        self.dirty("repo1")

        # The dirty worktree is reported and left in place (non-zero exit),
        # while the clean worktrees are still removed.
        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cm:
            self.call([REMOVE_WORKTREE_PATH, self.worktree_dir])

        # The report matches git's own `worktree remove` wording.
        self.assertIn(
            "contains modified or untracked files, use --force to delete it",
            cm.exception.output,
        )
        self.assertTrue(self.worktree_exists("repo1"))
        self.assertFalse(self.worktree_exists("repo2"))

    def test_force_removes_dirty_and_tolerates_already_removed(self):
        self.add_worktrees()
        self.dirty("repo1")

        # First run removes the clean worktrees and fails on the dirty one.
        with self.assertRaises(scheme_mock.PrintableSubprocessError):
            self.call([REMOVE_WORKTREE_PATH, self.worktree_dir])

        # '--force' removes the remaining dirty worktree and tolerates the
        # worktrees the first run already removed.
        self.call([REMOVE_WORKTREE_PATH, "--force", self.worktree_dir])

        self.assertFalse(os.path.exists(self.worktree_dir))

    def test_removes_clean_submodule_worktree(self):
        self.add_submodule_worktrees("repo1")

        # git refuses to remove a submodule worktree without --force, so the
        # tool always removes with --force internally; a clean one still goes.
        self.call([REMOVE_WORKTREE_PATH, self.worktree_dir])

        self.assertFalse(os.path.exists(self.worktree_dir))

    def test_skips_dirty_submodule_without_force(self):
        submodule_path = self.add_submodule_worktrees("repo1")

        # Uncommitted changes inside the submodule must be detected too.
        with open(os.path.join(submodule_path, "dirty.txt"), "w") as f:
            f.write("uncommitted\n")

        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cm:
            self.call([REMOVE_WORKTREE_PATH, self.worktree_dir])

        # When only the submodule is dirty, the report points at the submodule
        # path rather than the worktree, so the dirt is easy to find.
        self.assertIn(
            f"'{os.path.realpath(submodule_path)}' contains modified or "
            "untracked files",
            cm.exception.output,
        )
        self.assertTrue(self.worktree_exists("repo1"))
        self.assertFalse(self.worktree_exists("repo2"))

        # '--force' removes the dirty submodule worktree.
        self.call([REMOVE_WORKTREE_PATH, "--force", self.worktree_dir])

        self.assertFalse(os.path.exists(self.worktree_dir))

    def test_missing_directory_fails(self):
        missing = os.path.join(self.workspace, "does-not-exist")
        with self.assertRaises(scheme_mock.PrintableSubprocessError):
            self.call([REMOVE_WORKTREE_PATH, missing])

    def test_empty_directory_is_removed(self):
        empty = os.path.join(self.workspace, "empty")
        os.makedirs(empty)
        # No worktrees to remove: succeeds and deletes the empty directory.
        self.call([REMOVE_WORKTREE_PATH, empty])
        self.assertFalse(os.path.exists(empty))


if __name__ == "__main__":
    import unittest

    unittest.main()
