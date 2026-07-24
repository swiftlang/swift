# ===--- test_in_tree_config_change.py -------------------------------------===#
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

"""Tests that update-checkout re-reads the config after updating the `swift`
repo. The config that drives the run lives inside `swift`, so it can change
when switching schemes or when picking up a new revision of the same scheme
branch. The rest of the repos must be updated against the version of the
config that matches the swift checkout the run ends up at.
"""

import copy
import json
import os

from . import scheme_mock


class InTreeConfigChangeTestCase(scheme_mock.SchemeMockTestCase):

    IN_TREE_CONFIG_RELPATH = os.path.join("utils", "update-checkout-config.json")

    def setUp(self):
        self.config["repos"]["swift"] = {"remote": {"id": "swift"}}
        self.config["branch-schemes"]["main"]["repos"]["swift"] = "main"
        self.config["branch-schemes"]["release"] = {
            "aliases": ["release"],
            "repos": {
                "repo1": "main",
                "repo2": "main",
                "swift": "main",
            },
        }

        super().setUp()

        local_swift_path = os.path.join(self.local_path, "swift")
        in_tree_config = os.path.join(local_swift_path, self.IN_TREE_CONFIG_RELPATH)
        os.makedirs(os.path.dirname(in_tree_config))
        with open(in_tree_config, "w") as f:
            json.dump(self.config, f)
        self.call(["git", "add", "."], cwd=local_swift_path)
        self.call(
            ["git", "commit", "-m", "Add in-tree update-checkout-config.json"],
            cwd=local_swift_path,
        )
        self.call(["git", "push", "origin", "main"], cwd=local_swift_path)
        self.call(
            [
                self.update_checkout_path,
                "--config",
                self.config_path,
                "--source-root",
                self.source_root,
                "--scheme",
                "main",
                "--max-retries",
                "0",
                "--clone",
            ]
        )

    def _push_branch(self, repo_name, new_branch):
        local_repo_path = os.path.join(self.local_path, repo_name)
        self.call(["git", "checkout", "-b", new_branch], cwd=local_repo_path)
        self.call(
            ["git", "commit", "--allow-empty", "-m", f"commit on {new_branch}"],
            cwd=local_repo_path,
        )
        self.call(["git", "push", "origin", new_branch], cwd=local_repo_path)

    def _push_swift_main_with_config(self, new_config):
        local_swift_path = os.path.join(self.local_path, "swift")
        self.call(["git", "checkout", "main"], cwd=local_swift_path)
        with open(
            os.path.join(local_swift_path, self.IN_TREE_CONFIG_RELPATH), "w"
        ) as f:
            json.dump(new_config, f)
        self.call(["git", "add", "."], cwd=local_swift_path)
        self.call(
            ["git", "commit", "-m", "Update update-checkout-config.json"],
            cwd=local_swift_path,
        )
        self.call(["git", "push", "origin", "main"], cwd=local_swift_path)

    def _in_tree_config_path(self):
        return os.path.join(
            self.source_root, "swift", self.IN_TREE_CONFIG_RELPATH
        )

    def _head_ref(self, repo_name):
        repo_path = os.path.join(self.source_root, repo_name)
        return self.call(["git", "symbolic-ref", "HEAD"], cwd=repo_path).strip()

    def test_config_change_between_revisions_is_picked_up(self):
        """A new revision on the same scheme branch that updates a repo's
        branch mapping must take effect in the same run that pulls it in."""
        self._push_branch(repo_name="repo1", new_branch="branch-x")

        new_config = copy.deepcopy(self.config)
        new_config["branch-schemes"]["main"]["repos"]["repo1"] = "branch-x"
        self._push_swift_main_with_config(new_config)

        self.call(
            [
                self.update_checkout_path,
                "--config",
                self._in_tree_config_path(),
                "--source-root",
                self.source_root,
                "--scheme",
                "main",
                "--max-retries",
                "0",
            ]
        )

        self.assertEqual(self._head_ref("repo1"), "refs/heads/branch-x")

    def test_scheme_switch_uses_new_schemes_config(self):
        """Switching to a scheme whose mapping changed in a yet-to-be-fetched
        revision of swift main must use the post-fetch mapping."""
        self._push_branch(repo_name="repo1", new_branch="release-branch")

        new_config = copy.deepcopy(self.config)
        new_config["branch-schemes"]["release"]["repos"]["repo1"] = "release-branch"
        self._push_swift_main_with_config(new_config)

        self.call(
            [
                self.update_checkout_path,
                "--config",
                self._in_tree_config_path(),
                "--source-root",
                self.source_root,
                "--scheme",
                "release",
                "--max-retries",
                "0",
            ]
        )

        self.assertEqual(self._head_ref("repo1"), "refs/heads/release-branch")
