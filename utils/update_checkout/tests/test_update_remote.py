# ===--- test_update_remote.py ---------------------------------------------===#
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

import copy
import json
import os

from . import scheme_mock

NEW_FILENAME = "only_in_moved_remote.txt"


class UpdateRemoteTestCase(scheme_mock.SchemeMockTestCase):
    """Tests for the '--update-remote' option, which points the 'origin'
    remote of an existing clone at the URL computed from the configuration
    when the two disagree.
    """

    def __init__(self, *args, **kwargs):
        super(UpdateRemoteTestCase, self).__init__(*args, **kwargs)

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

        # Simulate the remote moving: copy repo1's remote to a new location
        # and add a commit that only exists there.
        self.old_remote_path = os.path.join(self.remote_path, "repo1")
        self.moved_remote_path = os.path.join(self.remote_path, "repo1-moved")
        self.call(
            ["git", "clone", "--bare", self.old_remote_path, self.moved_remote_path]
        )

        workdir = os.path.join(self.workspace, "moved-workdir")
        self.call(["git", "clone", self.moved_remote_path, workdir])
        self.call(["git", "config", "--local", "user.name", "swift_test"], cwd=workdir)
        self.call(
            ["git", "config", "--local", "user.email", "no-reply@swift.org"],
            cwd=workdir,
        )
        with open(os.path.join(workdir, NEW_FILENAME), "w") as f:
            f.write("New content")
        self.call(["git", "add", NEW_FILENAME], cwd=workdir)
        self.call(["git", "commit", "-m", "Commit in moved remote"], cwd=workdir)
        self.call(["git", "push", "origin", "main"], cwd=workdir)

        # Point the configuration's repo1 remote at the new location.
        self.config["repos"] = copy.deepcopy(self.config["repos"])
        self.config["repos"]["repo1"]["remote"]["url"] = (
            "file://%s" % self.moved_remote_path
        )
        with open(self.config_path, "w") as f:
            json.dump(self.config, f)

    def _origin_url(self, repo):
        repo_path = os.path.join(self.source_root, repo)
        return self.call(["git", "remote", "get-url", "origin"], cwd=repo_path).strip()

    def test_update_remote_points_origin_at_configured_url(self):
        self.call(self.base_args + ["--update-remote"])

        self.assertEqual(
            self._origin_url("repo1"), "file://%s" % self.moved_remote_path
        )
        # repo2's configured remote is unchanged, so its origin is too.
        self.assertEqual(
            self._origin_url("repo2"),
            "file://%s" % os.path.join(self.remote_path, "repo2"),
        )
        # The update must have fetched from the new location.
        self.assertTrue(
            os.path.exists(os.path.join(self.source_root, "repo1", NEW_FILENAME)),
            "the commit that only exists in the moved remote should have "
            "been fetched",
        )

    def test_origin_is_left_alone_without_update_remote(self):
        self.call(self.base_args)

        self.assertEqual(self._origin_url("repo1"), "file://%s" % self.old_remote_path)
        self.assertFalse(
            os.path.exists(os.path.join(self.source_root, "repo1", NEW_FILENAME)),
            "without --update-remote the update should still run against "
            "the old remote",
        )
