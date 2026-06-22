# ===--- SchemeMock.py ----------------------------------------------------===#
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https:#swift.org/LICENSE.txt for license information
#  See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#
"""This file defines objects for mocking an update-checkout scheme. It creates
a json .config file and a series of .git repos with "fake commits".
"""

import copy
import json
import os
import random
import subprocess
import tempfile
import unittest
import urllib.parse

from typing import Any, Dict

MOCK_CONFIG = {
    # This is here just b/c we expect it. We should consider consolidating
    # clone-patterns into a dictionary where we map protocols (i.e. ['ssh,
    # 'https'] to patterns). Then we can define this issue.
    "ssh-clone-pattern": "DO_NOT_USE",
    # We reset this value with our remote path when we process
    "https-clone-pattern": "",
    "repos": {
        "repo1": {
            "remote": {"id": "repo1"},
        },
        "repo2": {
            "remote": {"id": os.path.join("org", "remote2")},
        },
    },
    "default-branch-scheme": "main",
    "branch-schemes": {
        "main": {
            "aliases": ["main"],
            "repos": {
                "repo1": "main",
                "repo2": "main",
            },
        }
    },
}

MOCK_ADDITIONAL_SCHEME = {
    "branch-schemes": {
        "extra": {
            "aliases": ["extra"],
            "repos": {
                # Spell this differently just to make it distinguishable in
                # test output, even though we only have one branch.
                # TODO: Support multiple test branches in the repo instead.
                "repo1": "refs/heads/main",
                "repo2": "refs/heads/main",
            },
        }
    }
}


class PrintableSubprocessError(Exception):
    def __init__(self, *, underlying_error: subprocess.SubprocessError):
        self.underlying_error = underlying_error

    @property
    def stdout(self):
        return self.underlying_error.stdout

    @property
    def stderr(self):
        return self.underlying_error.stderr

    @property
    def output(self):
        return self.underlying_error.output

    @property
    def cmd(self):
        return self.underlying_error.cmd

    @property
    def returncode(self):
        return self.underlying_error.returncode

    def __str__(self):
        decoded_output = self.underlying_error.output
        if isinstance(decoded_output, bytes):
            decoded_output = decoded_output.decode()

        return f"""Command returned a non-zero exit status {self.underlying_error.returncode}:
Command: {' '.join(self.underlying_error.cmd)}
Output:

{decoded_output}"""


def call_quietly(*args, **kwargs):
    """
    Runs the command described by `args` and returns its combined stdout and
    stderr output.
    """

    kwargs["encoding"] = "utf-8"
    try:
        return subprocess.run(
            *args,
            check=True,
            capture_output=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            **kwargs,
        ).stdout
    except subprocess.SubprocessError as e:
        raise PrintableSubprocessError(underlying_error=e) from e


def create_dir(d):
    os.makedirs(d)


def teardown_mock_remote(base_dir):
    call_quietly(["rm", "-rf", base_dir])


def get_config_path(base_dir):
    return os.path.join(base_dir, "test-config.json")


def get_additional_config_path(base_dir):
    return os.path.join(base_dir, "test-additional-config.json")


def get_path_from_url(url: str) -> str:
    """
    Returns the path component of the given URL.
    """

    return urllib.parse.urlsplit(url).path


def configure_git_identity(repo_path):
    """Configure a deterministic local git identity and disable commit signing
    for the clone at `repo_path`.

    update-checkout's own clones (under `source_root`) do not set these up, so a
    test that commits into such a clone must call this first; otherwise the
    commit depends on an ambient global identity, which is absent on a pristine
    CI worker, and aborts. The harness's `local_path` clones go through this too,
    via `setup_mock_remote`.
    """
    call_quietly(
        ["git", "config", "--local", "user.name", "swift_test"], cwd=repo_path
    )
    call_quietly(
        ["git", "config", "--local", "user.email", "no-reply@swift.org"],
        cwd=repo_path,
    )
    call_quietly(
        ["git", "config", "--local", "commit.gpgsign", "false"], cwd=repo_path
    )


# TODO: Move this to SchemeMockTestCase.
def setup_mock_remote(test_case, base_dir, base_config, remotes_path, local_path):
    for local_repo_name in base_config["repos"].keys():
        local_repo_path = os.path.join(local_path, local_repo_name)
        remote_repo_path = test_case._compute_remote_path(repo_name=local_repo_name)

        create_dir(remote_repo_path)
        create_dir(local_repo_path)
        call_quietly(["git", "init", "--bare", remote_repo_path])
        call_quietly(
            ["git", "symbolic-ref", "HEAD", "refs/heads/main"], cwd=remote_repo_path
        )
        call_quietly(["git", "clone", "-l", remote_repo_path, local_repo_path])
        configure_git_identity(local_repo_path)
        call_quietly(
            ["git", "symbolic-ref", "HEAD", "refs/heads/main"], cwd=local_repo_path
        )

        call_quietly(
            [
                "git",
                "commit",
                "--allow-empty",
                "-m",
                # Equal commits created in different repositories at these
                # short time intervals tend to get the same SHA, which can
                # compromise tests. Supplying a random commit message makes
                # these SHA collisions far less likely.
                str(random.random()),
            ],
            cwd=local_repo_path,
        )
        call_quietly(["git", "push", "origin", "main"], cwd=local_repo_path)

    https_clone_pattern = os.path.join(f"file://{remotes_path}", "%s")
    base_config["https-clone-pattern"] = https_clone_pattern

    with open(get_config_path(base_dir), "w") as f:
        json.dump(base_config, f)

    with open(get_additional_config_path(base_dir), "w") as f:
        json.dump(MOCK_ADDITIONAL_SCHEME, f)


TEST_SUITE_ARENA_ENV_VAR = "UPDATE_CHECKOUT_TEST_SUITE_ARENA"
TEST_SUITE_ARENA_DIR = os.getenv(TEST_SUITE_ARENA_ENV_VAR)
CURRENT_FILE_DIR = os.path.dirname(os.path.abspath(__file__))
UPDATE_CHECKOUT_EXECUTABLE = (
    "update-checkout.cmd" if os.name == "nt" else "update-checkout"
)
UPDATE_CHECKOUT_PATH = os.path.abspath(
    os.path.join(
        CURRENT_FILE_DIR, os.path.pardir, os.path.pardir, UPDATE_CHECKOUT_EXECUTABLE
    )
)


class SchemeMockTestCase(unittest.TestCase):

    def __init__(self, *args, **kwargs):
        super(SchemeMockTestCase, self).__init__(*args, **kwargs)

        # The test runner will first create TestCase instances for all test
        # methods in the suite and only then run each test. Although we should
        # not be mutating a configuration at run time, have each instance get a
        # copy of the given configuration so that potential mutations to it do
        # not affect subsequent tests.
        self.config = copy.deepcopy(MOCK_CONFIG)

        if TEST_SUITE_ARENA_DIR is None:
            raise RuntimeError(
                "Misconfigured test suite! Environment "
                "variable %s must be set!" % TEST_SUITE_ARENA_ENV_VAR
            )

        # Each individual test gets a temporary directory in the arena. This is
        # important even though the tests are run serially, because an exception
        # will interrupt execution of a test and prevent teardown, but will not
        # prevent subsequent tests from running.
        self.workspace = tempfile.mkdtemp(dir=TEST_SUITE_ARENA_DIR)
        self.config_path = get_config_path(self.workspace)
        self.additional_config_path = get_additional_config_path(self.workspace)
        self.update_checkout_path = UPDATE_CHECKOUT_PATH
        if not os.access(self.update_checkout_path, os.X_OK):
            raise RuntimeError(
                "Error! Could not find executable "
                "update-checkout at path: %s" % self.update_checkout_path
            )
        self.source_root = os.path.join(self.workspace, "source_root")
        # We use remote as a directory that simulates our remote unchecked out
        # repo.
        self._remotes_path = os.path.join(self.workspace, "remote")
        # We use local as a workspace for creating commits.
        self.local_path = os.path.join(self.workspace, "local")

        self.base_args = [
            self.update_checkout_path,
            "--config",
            self.config_path,
            "--source-root",
            self.source_root,
        ]

    def setUp(self):
        create_dir(self.source_root)
        create_dir(self._remotes_path)
        create_dir(self.local_path)
        setup_mock_remote(
            self, self.workspace, self.config, self._remotes_path, self.local_path
        )

    def tearDown(self):
        teardown_mock_remote(self.workspace)

    def call(self, *args, **kwargs):
        if "cwd" not in kwargs:
            kwargs["cwd"] = self.source_root
        return call_quietly(*args, **kwargs)

    @property
    def repo_names(self):
        return list(self.config["repos"].keys())

    def _compute_remote_path(self, *, repo_name: str):
        remote_info = self.config["repos"][repo_name]["remote"]
        id = remote_info.get("id")
        url = remote_info.get("url")
        if id is not None:
            return os.path.join(self._remotes_path, remote_info["id"])
        elif url is not None:
            url_path = get_path_from_url(url)
            workspace_path = os.path.dirname(self._remotes_path)
            self.assertEqual(
                os.path.commonpath((workspace_path, url_path)), workspace_path
            )
            return os.path.join(self._remotes_path, os.path.basename(url_path))
        else:
            raise RuntimeError(
                f"'remote' for '{local_repo_name}' must have an 'id' or 'url' item"
            )

    def remote_path(self, *, repo_name: str):
        return self._compute_remote_path(repo_name=repo_name)

    def add_branch_scheme(self, name, scheme):
        self.config["branch-schemes"][name] = scheme
