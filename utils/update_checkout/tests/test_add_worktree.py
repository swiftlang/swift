# ===--- test_add_worktree.py ---------------------------------------------===#
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

import json
import os
import shutil

from . import scheme_mock


ADD_WORKTREE_PATH = os.path.abspath(
    os.path.join(
        scheme_mock.CURRENT_FILE_DIR,
        os.path.pardir,
        os.path.pardir,
        "add-worktree",
    )
)


class AddWorktreeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

    def clone_source_root(self):
        self.call(self.base_args + ["--clone"])

    def head_commit(self, repo_path):
        return self.call(["git", "rev-parse", "HEAD"], cwd=repo_path).strip()

    def is_detached(self, repo_path):
        ref = self.call(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_path
        ).strip()
        return ref == "HEAD"

    def test_creates_detached_worktrees(self):
        self.clone_source_root()
        self.call(
            [ADD_WORKTREE_PATH, "--source-root", self.source_root, self.worktree_dir]
        )

        for repo in self.repo_names:
            worktree_path = os.path.join(self.worktree_dir, repo)
            self.assertTrue(os.path.isdir(worktree_path))
            # A worktree's '.git' is a file, not a directory.
            self.assertTrue(os.path.isfile(os.path.join(worktree_path, ".git")))
            self.assertTrue(self.is_detached(worktree_path))
            self.assertEqual(
                self.head_commit(worktree_path),
                self.head_commit(os.path.join(self.source_root, repo)),
            )

    def test_idempotent(self):
        self.clone_source_root()
        command = [
            ADD_WORKTREE_PATH,
            "--source-root",
            self.source_root,
            self.worktree_dir,
        ]
        self.call(command)
        # A second run must not fail even though the worktrees already exist.
        output = self.call(command)
        self.assertIn("add-worktree succeeded", output)

    def test_scheme_switch_detaches_on_collision(self):
        self.clone_source_root()
        # The scheme's branches match the primary checkout's branches, so every
        # worktree collides on checkout and must fall back to a detached HEAD.
        output = self.call(
            [
                ADD_WORKTREE_PATH,
                "--source-root",
                self.source_root,
                "--config",
                self.config_path,
                "--scheme",
                "main",
                self.worktree_dir,
            ]
        )
        self.assertIn("add-worktree succeeded", output)
        for repo in self.repo_names:
            self.assertTrue(self.is_detached(os.path.join(self.worktree_dir, repo)))


class AddWorktreeSchemeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

    def setUp(self):
        super().setUp()

        # Advance 'main' so the primary checkout and the target scheme end up at
        # different commits, letting us prove the scheme actually moves the
        # worktree.
        local_repo1 = os.path.join(self.local_path, "repo1")
        self.call(["git", "commit", "--allow-empty", "-m", "second"], cwd=local_repo1)
        self.call(["git", "push", "origin", "main"], cwd=local_repo1)
        self.call(self.base_args + ["--clone"])

        remote_repo1 = self.remote_path(repo_name="repo1")
        commits = self.call(["git", "rev-list", "main"], cwd=remote_repo1).split()
        self.old_commit = commits[-1]

        self.scheme_name = "commit-scheme"
        self.add_branch_scheme(
            self.scheme_name,
            {
                "aliases": [self.scheme_name],
                "repos": {"repo1": self.old_commit, "repo2": "main"},
            },
        )
        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

    def test_scheme_switches_commit(self):
        self.call(
            [
                ADD_WORKTREE_PATH,
                "--source-root",
                self.source_root,
                "--config",
                self.config_path,
                "--scheme",
                self.scheme_name,
                self.worktree_dir,
            ]
        )

        worktree_repo1 = os.path.join(self.worktree_dir, "repo1")
        current_commit = self.call(
            ["git", "rev-parse", "HEAD"], cwd=worktree_repo1
        ).strip()
        self.assertEqual(current_commit, self.old_commit)


class AddWorktreeSchemeSelectionTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir = os.path.join(self.workspace, "worktrees")

        # A 'partial' scheme that omits repo2, plus an 'extra' repo that is in
        # the source root but in no scheme at all.
        self.config["repos"]["extra"] = {"remote": {"id": "extra"}}
        self.config["branch-schemes"]["main"]["repos"]["extra"] = "main"
        self.add_branch_scheme(
            "partial",
            {"aliases": ["partial"], "repos": {"repo1": "main"}},
        )

    def worktree_exists(self, repo):
        return os.path.exists(os.path.join(self.worktree_dir, repo))

    def test_only_scheme_repos_get_worktrees(self):
        self.call(self.base_args + ["--clone"])
        self.call(
            [
                ADD_WORKTREE_PATH,
                "--source-root",
                self.source_root,
                "--config",
                self.config_path,
                "--scheme",
                "partial",
                self.worktree_dir,
            ]
        )

        # Only the scheme's repo gets a worktree; repos outside the scheme
        # (repo2, extra) are not worktree'd even though they exist in the
        # source root.
        self.assertTrue(self.worktree_exists("repo1"))
        self.assertFalse(self.worktree_exists("repo2"))
        self.assertFalse(self.worktree_exists("extra"))

    def test_missing_scheme_repo_fails(self):
        self.call(self.base_args + ["--clone"])
        # 'main' includes repo2; remove it from the source root so a scheme
        # repository is missing.
        shutil.rmtree(os.path.join(self.source_root, "repo2"))

        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cm:
            self.call(
                [
                    ADD_WORKTREE_PATH,
                    "--source-root",
                    self.source_root,
                    "--config",
                    self.config_path,
                    "--scheme",
                    "main",
                    self.worktree_dir,
                ]
            )
        self.assertIn("is missing from", cm.exception.output)
        # No worktrees were created for the incomplete scheme.
        self.assertFalse(self.worktree_exists("repo1"))


class AddWorktreeSharedSchemeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.worktree_dir_a = os.path.join(self.workspace, "worktrees-a")
        self.worktree_dir_b = os.path.join(self.workspace, "worktrees-b")

    def setUp(self):
        super().setUp()

        # Create a 'feature' branch on each remote that the primary checkout
        # will not have checked out, so the scheme's branch differs from the
        # primary's branch. This lets the first worktree claim the branch while
        # a second worktree must fall back to a detached HEAD.
        for repo_name in self.repo_names:
            local_repo = os.path.join(self.local_path, repo_name)
            self.call(["git", "checkout", "-b", "feature"], cwd=local_repo)
            self.call(["git", "commit", "--allow-empty", "-m", "F"], cwd=local_repo)
            self.call(["git", "push", "origin", "feature"], cwd=local_repo)
            self.call(["git", "checkout", "main"], cwd=local_repo)

        self.scheme_name = "feature-scheme"
        self.add_branch_scheme(
            self.scheme_name,
            {
                "aliases": [self.scheme_name],
                "repos": {repo: "feature" for repo in self.repo_names},
            },
        )
        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

        # Populate the primary checkout on 'main', not 'feature'.
        self.call(self.base_args + ["--clone"])

    def current_branch(self, repo_path):
        return self.call(
            ["git", "rev-parse", "--abbrev-ref", "HEAD"], cwd=repo_path
        ).strip()

    def head_commit(self, repo_path):
        return self.call(["git", "rev-parse", "HEAD"], cwd=repo_path).strip()

    def test_second_worktree_detaches_from_scheme_branch(self):
        scheme_args = [
            ADD_WORKTREE_PATH,
            "--source-root",
            self.source_root,
            "--config",
            self.config_path,
            "--scheme",
            self.scheme_name,
        ]

        # The first worktree claims the scheme's branch.
        self.call(scheme_args + [self.worktree_dir_a])
        repo1_a = os.path.join(self.worktree_dir_a, "repo1")
        self.assertEqual(self.current_branch(repo1_a), "feature")

        # The second worktree can't check out a branch already used by the
        # first, so it falls back to a detached HEAD at the same commit and
        # still succeeds.
        output = self.call(scheme_args + [self.worktree_dir_b])
        self.assertIn("add-worktree succeeded", output)
        repo1_b = os.path.join(self.worktree_dir_b, "repo1")
        self.assertEqual(self.current_branch(repo1_b), "HEAD")
        self.assertEqual(self.head_commit(repo1_a), self.head_commit(repo1_b))
