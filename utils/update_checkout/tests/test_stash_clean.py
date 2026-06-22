# ===--- test_stash_clean.py ----------------------------------------------===#
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


class StashAndCleanTestCase(scheme_mock.SchemeMockTestCase):
    """Coverage for the top-level repository's behaviour under the destructive
    pre-update options.

    --stash MUST preserve uncommitted tracked + untracked work (recoverable
    from `git stash`); --clean MUST discard it. Both modes additionally delete
    ignored files via the shared trailing `git clean -fdx`.
    """

    def _repo1_path(self):
        return os.path.join(self.source_root, "repo1")

    def _git(self, args, **kwargs):
        kwargs.setdefault("cwd", self._repo1_path())
        return self.call(["git"] + args, **kwargs)

    def _clone(self):
        self.call(self.base_args + ["--clone"])

    def _seed_tracked_file(self):
        """Commit and push a tracked file and a .gitignore in the
        update-checkout-managed tree.

        Operates on `source_root/repo1` (the tree update-checkout acts on), NOT
        `local_path/repo1`. Pushing makes local `main` == remote `main` so the
        update that follows the stash/clean step is a no-op; only the working
        tree is later made dirty. Requires `_clone()` to have run first."""
        repo = self._repo1_path()
        # update-checkout's own clone of source_root has no git identity (only
        # the harness's local_path clones get one), so configure it before the
        # seed commit. See scheme_mock.configure_git_identity.
        scheme_mock.configure_git_identity(repo)
        with open(os.path.join(repo, "tracked.txt"), "w") as f:
            f.write("committed content\n")
        with open(os.path.join(repo, ".gitignore"), "w") as f:
            f.write("*.ignored\n")
        self._git(["add", "tracked.txt", ".gitignore"])
        self._git(["commit", "-m", "add tracked.txt and .gitignore"])
        self._git(["push", "origin", "main"])

    def _make_uncommitted_changes(self):
        """A tracked modification, a brand-new untracked file, and an ignored
        file."""
        repo = self._repo1_path()
        with open(os.path.join(repo, "tracked.txt"), "w") as f:
            f.write("MY UNCOMMITTED EDIT\n")
        with open(os.path.join(repo, "untracked.txt"), "w") as f:
            f.write("brand new untracked file\n")
        with open(os.path.join(repo, "build.ignored"), "w") as f:
            f.write("ignored build artifact\n")

    def _assert_working_tree_dirty(self):
        """Guard against a vacuous pass: the tracked edit + untracked file must
        actually be present before update-checkout runs (otherwise the
        'discards' assertions below would hold trivially)."""
        status = self._git(["status", "--porcelain"])
        self.assertNotEqual(
            status.strip(),
            "",
            "test setup is broken: repo1 working tree is not dirty before the "
            "update-checkout run.",
        )

    def test_stash_preserves_uncommitted_changes(self):
        self._clone()
        self._seed_tracked_file()
        self._make_uncommitted_changes()
        self._assert_working_tree_dirty()

        self.call(self.base_args + ["--stash"])

        # A stash entry must exist (the changes were not destroyed).
        # NOTE: this assertion MUST precede the `stash pop` below. Against the
        # inverted code the stash is empty, so this fails here as a clean
        # AssertionError; if `pop` ran first it would raise a subprocess error
        # ("No stash entries found") and the RED would become a test error
        # rather than the asserted failure.
        stash_list = self._git(["stash", "list"])
        self.assertNotEqual(
            stash_list.strip(),
            "",
            "--stash must create a stash entry, but `git stash list` is empty "
            "(the uncommitted changes were destroyed).",
        )

        # And the changes must be genuinely recoverable, not just a blob.
        self._git(["stash", "pop"])
        with open(os.path.join(self._repo1_path(), "tracked.txt")) as f:
            self.assertEqual(f.read(), "MY UNCOMMITTED EDIT\n")
        untracked = os.path.join(self._repo1_path(), "untracked.txt")
        self.assertTrue(
            os.path.isfile(untracked),
            "--stash must preserve untracked files (via `git stash -u`).",
        )
        with open(untracked) as f:
            self.assertEqual(f.read(), "brand new untracked file\n")
        # Ignored files are NOT stashed by `stash -u`; the trailing `clean -fdx`
        # removes them (the documented "delete ignored files" behaviour). The
        # `stash pop` above cannot reintroduce an ignored file, so this holds
        # post-pop.
        self.assertFalse(
            os.path.isfile(os.path.join(self._repo1_path(), "build.ignored")),
            "--stash's trailing `clean -fdx` must delete ignored files.",
        )

    def test_clean_discards_uncommitted_changes(self):
        self._clone()
        self._seed_tracked_file()
        self._make_uncommitted_changes()
        self._assert_working_tree_dirty()

        self.call(self.base_args + ["--clean"])

        # --clean discards: no stash entry is created.
        stash_list = self._git(["stash", "list"])
        self.assertEqual(
            stash_list.strip(),
            "",
            "--clean must not create a stash entry.",
        )
        # Tracked modification reverted to committed content.
        with open(os.path.join(self._repo1_path(), "tracked.txt")) as f:
            self.assertEqual(f.read(), "committed content\n")
        # Untracked file removed.
        self.assertFalse(
            os.path.isfile(os.path.join(self._repo1_path(), "untracked.txt")),
            "--clean must delete untracked files.",
        )
        # Ignored file removed by the trailing `clean -fdx`.
        self.assertFalse(
            os.path.isfile(os.path.join(self._repo1_path(), "build.ignored")),
            "--clean must delete ignored files.",
        )
