# ===--- test_cross_repo_pr.py -----------------------------------------------===#
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
import random

from . import scheme_mock
from typing import Dict, List


class CrossRepoPRTestCase(scheme_mock.SchemeMockTestCase):

    def setUp(self):
        self.update_checkout_base_args = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
            "--scheme",
            "main",
            "--max-retries",
            "0",
        ]

        self.config = {
            "repos": {
                "repo1": {
                    "remote": {"id": os.path.join("apple", "repo1")},
                },
                "repo2": {
                    "remote": {"id": os.path.join("swiftlang", "repo2")},
                },
                "swift": {
                    "remote": {"id": os.path.join("swiftlang", "swift")},
                },
            },
            "default-branch-scheme": "main",
            "branch-schemes": {
                "main": {
                    "aliases": ["main"],
                    "repos": {
                        "repo1": "main",
                        "repo2": "main",
                        "swift": "main",
                    },
                },
            },
        }

        super().setUp()

        self.call(self.update_checkout_base_args + ["--clone"])

    def set_up_pr_merge_ref(
        self,
        *,
        repo_name: str,
        pr_id: int,
        stale: bool,
        conflicts_with_base: bool = False,
    ):
        # Conflict is only relevant for the stale case.
        self.assertTrue(stale is True or conflicts_with_base is False)

        repo_path = os.path.join(self.local_path, repo_name)
        conflict_filename = "conflict.txt"
        conflict_file_path = os.path.join(repo_path, conflict_filename)

        # Goal:
        #   C---D (my_branch)
        #  /   /
        # A---B---E (main)
        #
        # We will use commit D for the PR merge ref. Commit E is created if
        # the `stale` argument is true.

        # Create commit B. Commit A should already exist from initial setup.
        self.call(
            ["git", "commit", "--allow-empty", "-m", "B"],
            cwd=repo_path,
        )

        # Create my_branch.
        self.call(["git", "checkout", "-b", "my_branch", "HEAD~1"], cwd=repo_path)

        # Arrange for an eventual conflict between commits E and D if asked to.
        if conflicts_with_base:
            with open(conflict_file_path, "w") as f:
                f.write("pr-version\n")
            self.call(["git", "add", conflict_filename], cwd=repo_path)

        # Create commit C and the merge commit D.
        self.call(
            ["git", "commit", "--allow-empty", "-m", "C"],
            cwd=repo_path,
        )
        self.call(["git", "merge", "--no-ff", "-m", "D", "main"], cwd=repo_path)

        # Advance main (create commit E) to make the merge commit D stale if
        # asked to.
        if stale:
            self.call(["git", "checkout", "main"], cwd=repo_path)

            # Arrange for an eventual conflict between commits E and D if
            # asked to.
            if conflicts_with_base:
                with open(conflict_file_path, "w") as f:
                    f.write("main-version\n")
                self.call(["git", "add", conflict_filename], cwd=repo_path)

            # Create commit E.
            self.call(
                ["git", "commit", "--allow-empty", "-m", "E"],
                cwd=repo_path,
            )

        # Push main and the PR merge ref to the remote.
        self.call(
            ["git", "push", "origin", "main", f"my_branch:refs/pull/{pr_id}/merge"],
            cwd=repo_path,
        )

    # When the merge ref is stale, update-checkout is expected to check it out
    # and merge the tip of the base branch into it.
    def verify_head_for_stale_pr_merge_ref(self, *, repo_name: str, pr_id: int):
        repo_path = os.path.join(self.source_root, repo_name)
        remote_path = self.remote_path(repo_name=repo_name)

        expected_parent_commits = self.call(
            ["git", "rev-parse", f"refs/pull/{pr_id}/merge", "refs/heads/main"],
            cwd=remote_path,
        ).split()

        actual_parent_commits = self.call(
            ["git", "log", "--pretty=%P", "-1", "HEAD"],
            cwd=repo_path,
        ).split()

        self.assertEqual(expected_parent_commits, actual_parent_commits)

    # When the merge ref is up to date, update-checkout is expected to simply
    # check it out.
    def verify_head_for_up_to_date_pr_merge_ref(self, *, repo_name: str, pr_id: int):
        repo_path = os.path.join(self.source_root, repo_name)
        remote_path = self.remote_path(repo_name=repo_name)

        expected_head = self.call(
            ["git", "rev-parse", f"refs/pull/{pr_id}/merge"], cwd=remote_path
        )

        actual_head = self.call(["git", "rev-parse", "HEAD"], cwd=repo_path)

        self.assertEqual(expected_head, actual_head)

    def test_checkout_stale_pr_merge_ref(self):
        pr_id = 1
        self.set_up_pr_merge_ref(repo_name="repo1", pr_id=pr_id, stale=True)
        self.call(
            self.update_checkout_base_args
            + [
                "--github-comment",
                f"""
                https://github.com/apple/repo1/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(repo_name="repo1", pr_id=pr_id)

    def test_checkout_stale_pr_merge_ref_swift(self):
        pr_id = 1
        self.set_up_pr_merge_ref(repo_name="swift", pr_id=pr_id, stale=True)
        self.call(
            self.update_checkout_base_args
            + [
                "--github-comment",
                f"""
                https://github.com/swiftlang/swift/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(repo_name="swift", pr_id=pr_id)

    def test_checkout_stale_pr_merge_ref_conflict_with_base_branch(self):
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name="repo1", pr_id=pr_id, stale=True, conflicts_with_base=True
        )

        # update-checkout should fail because re-merging main into the PR
        # merge ref produces a conflict.
        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cxt_mgr:
            self.call(
                self.update_checkout_base_args
                + [
                    "--github-comment",
                    f"""
                    https://github.com/apple/repo1/pull/{pr_id}
                    @swift-ci please test
                    """,
                ]
            )

        self.assertIn("CONFLICT (add/add)", cxt_mgr.exception.output)

        # The merge should have been aborted, leaving no MERGE_HEAD and no
        # uncommitted changes in the working tree.
        repo_path = os.path.join(self.source_root, "repo1")
        self.assertFalse(os.path.exists(os.path.join(repo_path, ".git", "MERGE_HEAD")))
        status_output = self.call(["git", "status", "--porcelain"], cwd=repo_path)
        self.assertEqual(status_output.strip(), "")

    def test_checkout_multiple_stale_pr_merge_ref(self):
        self.set_up_pr_merge_ref(repo_name="repo1", pr_id=1, stale=True)
        self.set_up_pr_merge_ref(repo_name="repo2", pr_id=2, stale=True)
        self.call(
            self.update_checkout_base_args
            + [
                "--github-comment",
                f"""
                https://github.com/apple/repo1/pull/1
                @swift-ci please test
                https://github.com/swiftlang/repo2/pull/2
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(repo_name="repo1", pr_id=1)
        self.verify_head_for_stale_pr_merge_ref(repo_name="repo2", pr_id=2)

    def test_checkout_up_to_date_pr_merge_ref(self):
        pr_id = 1
        self.set_up_pr_merge_ref(repo_name="repo1", pr_id=pr_id, stale=False)
        self.call(
            self.update_checkout_base_args
            + [
                "--github-comment",
                f"""
                https://github.com/apple/repo1/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_up_to_date_pr_merge_ref(repo_name="repo1", pr_id=pr_id)
