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
                "rebranch": {
                    "aliases": ["rebranch"],
                    "repos": {
                        "repo1": "rebranch1",
                        "repo2": "main",
                        "swift": "main",
                    },
                },
            },
        }

        super().setUp()

    def get_branch(self, *, rev_scheme_name: str, repo_name: str):
        return self.config["branch-schemes"][rev_scheme_name]["repos"][repo_name]

    def set_up_pr_merge_ref(
        self,
        *,
        repo_name: str,
        # The revision scheme (aka branch scheme) is used to determine the base
        # branch for our imaginary PR.
        rev_scheme_name: str,
        pr_id: int,
        stale: bool,
        conflicts_with_base: bool = False,
    ):
        # Conflict is only relevant for the stale case.
        self.assertTrue(stale is True or conflicts_with_base is False)

        pr_base_branch = self.get_branch(
            rev_scheme_name=rev_scheme_name, repo_name=repo_name
        )
        pr_head_branch = "pr_head_branch"
        pr_merge_branch = f"pull/{pr_id}/merge"

        # Goal:
        #       .---C (pr_head_branch)
        #      /     \
        #     /   .---D (pr_merge_branch)
        #    /   /
        # --o---B---------E (pr_base_branch)
        #
        # We will use commit D for the PR merge ref. Commit E is created if
        # the `stale` argument is true.

        repo_path = os.path.join(self.local_path, repo_name)

        # Create and checkout pr_base_branch if it does not exist (e.g. because
        # we cloned using a different branch scheme).
        self.call(["git", "checkout", "-B", pr_base_branch, "HEAD"], cwd=repo_path)

        # Create commit B.
        self.call(["git", "commit", "--allow-empty", "-m", "B"], cwd=repo_path)

        # Create and checkout pr_head_branch.
        self.call(["git", "checkout", "-b", pr_head_branch, "HEAD~1"], cwd=repo_path)

        # NOTE: This file is not dead weight.
        change_file_path = os.path.join(repo_path, "change.txt")

        # If asked to, arrange for an eventual conflict between commits E and D.
        if conflicts_with_base:
            with open(change_file_path, "w") as f:
                f.write("pr-version\n")
            self.call(["git", "add", "."], cwd=repo_path)

        # Create commit C.
        self.call(["git", "commit", "--allow-empty", "-m", "C"], cwd=repo_path)

        # Create and checkout pr_merge_branch.
        self.call(
            ["git", "checkout", "-b", pr_merge_branch, pr_base_branch], cwd=repo_path
        )

        # Create commit D (merge pr_head_branch into pr_merge_branch).
        self.call(["git", "merge", "--no-ff", "-m", "D", pr_head_branch], cwd=repo_path)

        # If asked to, advance pr_base_branch (create commit E) to make the
        # merge commit D stale.
        if stale:
            self.call(["git", "checkout", pr_base_branch], cwd=repo_path)

            # Always add an actual change to this commit so that soft-resetting
            # it has an effect on the working directory.
            with open(change_file_path, "w") as f:
                f.write("base-version\n")
            self.call(["git", "add", "."], cwd=repo_path)

            # Create commit E.
            self.call(
                ["git", "commit", "--allow-empty", "-m", "E"],
                cwd=repo_path,
            )

        # Push pr_base_branch and the PR merge ref to the remote.
        self.call(
            [
                "git",
                "push",
                "origin",
                pr_base_branch,
                f"{pr_merge_branch}:refs/{pr_merge_branch}",
            ],
            cwd=repo_path,
        )

    # When the merge ref is stale, update-checkout is expected to check it out
    # and merge the tip of the base branch into it.
    def verify_head_for_stale_pr_merge_ref(
        self, *, repo_name: str, rev_scheme_name: str, pr_id: int
    ):
        repo_path = os.path.join(self.source_root, repo_name)
        remote_path = self.remote_path(repo_name=repo_name)
        pr_base_branch = self.get_branch(
            rev_scheme_name=rev_scheme_name, repo_name=repo_name
        )

        expected_parent_commits = self.call(
            [
                "git",
                "rev-parse",
                f"refs/pull/{pr_id}/merge",
                f"refs/heads/{pr_base_branch}",
            ],
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
        self.call(self.update_checkout_base_args + ["--clone"])
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_shallow_clone(self):
        self.call(self.update_checkout_base_args + ["--clone", "--skip-history"])
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_during_clone(self):
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--clone",
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_during_shallow_clone(self):
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--clone",
                "--skip-history",
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_other_rev_scheme(self):
        self.call(self.update_checkout_base_args + ["--clone"])
        repo_name = "repo1"
        rev_scheme = "rebranch"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_swift(self):
        self.call(self.update_checkout_base_args + ["--clone"])
        repo_name = "swift"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/swiftlang/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    def test_checkout_stale_pr_merge_ref_conflict_with_base_branch(self):
        self.call(self.update_checkout_base_args + ["--clone"])
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name="repo1",
            rev_scheme_name=rev_scheme,
            pr_id=pr_id,
            stale=True,
            conflicts_with_base=True,
        )

        # update-checkout should fail because re-merging main into the PR
        # merge ref produces a conflict.
        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cxt_mgr:
            self.call(
                self.update_checkout_base_args
                + [
                    "--scheme",
                    rev_scheme,
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
        self.call(self.update_checkout_base_args + ["--clone"])
        pr1_id = 1
        pr2_id = 2
        repo1_name = "repo1"
        repo2_name = "repo2"
        rev_scheme = "main"
        self.set_up_pr_merge_ref(
            repo_name=repo1_name, rev_scheme_name=rev_scheme, pr_id=pr1_id, stale=True
        )
        self.set_up_pr_merge_ref(
            repo_name=repo2_name, rev_scheme_name=rev_scheme, pr_id=pr2_id, stale=True
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo1_name}/pull/{pr1_id}
                @swift-ci please test
                https://github.com/swiftlang/{repo2_name}/pull/{pr2_id}
                """,
            ]
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo1_name, rev_scheme_name=rev_scheme, pr_id=pr1_id
        )
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo2_name, rev_scheme_name=rev_scheme, pr_id=pr2_id
        )

    def test_checkout_up_to_date_pr_merge_ref(self):
        self.call(self.update_checkout_base_args + ["--clone"])
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=False
        )
        self.call(
            self.update_checkout_base_args
            + [
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        self.verify_head_for_up_to_date_pr_merge_ref(repo_name=repo_name, pr_id=pr_id)

    def test_checkout_stale_pr_merge_ref_idempotency(self):
        self.call(self.update_checkout_base_args + ["--clone"])
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )

        update_checkout_args = self.update_checkout_base_args + [
            "--scheme",
            rev_scheme,
            "--github-comment",
            f"""
            https://github.com/apple/{repo_name}/pull/{pr_id}
            @swift-ci please test
            """,
        ]
        # Calling repeatedly should not affect the result.
        self.call(update_checkout_args)
        self.call(update_checkout_args)
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )

    # Make sure we do check PR merge ref staleness against
    # 'refs/heads/base_branch', and not some other 'refs/heads/*/base_branch'.
    # This failure mode can occur when 'git-ls-remote' is asked to enumerate
    # refs matching just 'base_branch' instead of 'refs/heads/base_branch'.
    def test_no_branch_ambiguity_in_staleness_check(self):
        repo_name = "repo1"
        rev_scheme = "main"
        pr_id = 1
        self.set_up_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id, stale=True
        )

        # To simulate the scenario described above, create a branch that, if
        # mistaken for the actual base branch, would cause update-checkout to
        # conclude that the merge ref is up to date.
        base_branch = self.get_branch(rev_scheme_name=rev_scheme, repo_name=repo_name)
        not_base_branch = f"foo/{base_branch}"
        repo_path = os.path.join(self.local_path, repo_name)
        self.call(
            ["git", "branch", not_base_branch, f"pull/{pr_id}/merge^1"], cwd=repo_path
        )
        self.call(["git", "push", "origin", not_base_branch], cwd=repo_path)

        self.call(
            self.update_checkout_base_args
            + [
                "--clone",
                "--scheme",
                rev_scheme,
                "--github-comment",
                f"""
                https://github.com/apple/{repo_name}/pull/{pr_id}
                @swift-ci please test
                """,
            ]
        )
        # Check that update-checkout took the path for a stale merge ref.
        self.verify_head_for_stale_pr_merge_ref(
            repo_name=repo_name, rev_scheme_name=rev_scheme, pr_id=pr_id
        )
