# ===--- test_config_restart.py -------------------------------------------===#
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

import contextlib
import copy
from io import StringIO
import json
import os
from pathlib import Path
import unittest
from unittest.mock import patch

from . import scheme_mock
from update_checkout.update_checkout import main


class ConfigRestartTestCase(scheme_mock.SchemeMockTestCase):
    """Updating the swift repository can change the configuration (or
    update-checkout itself) underneath us.  If the configuration changes during
    an update, the update must be restarted with the new configuration; if the
    script changes, update-checkout must re-exec itself.

    ``update_all_repositories`` is replaced with a fake that records the
    configuration it was given and optionally rewrites files, as updating the
    swift repository would."""

    def setUp(self):
        super().setUp()
        self.calls = []
        # Each entry is run, in order, after the corresponding call to the
        # fake update_all_repositories; missing entries do nothing.
        self.after_update = []

    def _read_config(self):
        with open(self.config_path) as f:
            return json.load(f)

    def _write_config(self, config, **dump_kwargs):
        with open(self.config_path, "w") as f:
            json.dump(config, f, **dump_kwargs)

    def _retarget_repo1(self, branch):
        def action():
            config = self._read_config()
            config["branch-schemes"]["main"]["repos"]["repo1"] = branch
            self._write_config(config)

        return action

    def _fake_update_all_repositories(
        self, args, config, scheme_name, scheme_map, cross_repos_pr
    ):
        index = len(self.calls)
        self.calls.append((copy.deepcopy(config), copy.deepcopy(scheme_map)))
        if index < len(self.after_update):
            self.after_update[index]()
        return [], []

    def _run_main(self, **patches):
        output = StringIO()
        with contextlib.ExitStack() as stack:
            stack.enter_context(
                patch(
                    "update_checkout.update_checkout.update_all_repositories",
                    new=self._fake_update_all_repositories,
                )
            )
            stack.enter_context(
                patch(
                    "sys.argv",
                    ["update-checkout", "--config", self.config_path]
                    + ["--source-root", self.source_root],
                )
            )
            for target, replacement in patches.items():
                stack.enter_context(
                    patch(f"update_checkout.update_checkout.{target}", new=replacement)
                )
            stack.enter_context(contextlib.redirect_stdout(output))
            with self.assertRaises(SystemExit) as cm:
                main()
        self.assertEqual(cm.exception.code, 0)
        return output.getvalue()

    def _repo1_branches(self):
        return [scheme_map["repo1"] for _, scheme_map in self.calls]

    def test_unchanged_config_does_not_restart(self):
        output = self._run_main()

        self.assertEqual(len(self.calls), 1)
        self.assertNotIn("Configuration changed", output)

    def test_changed_config_restarts_with_new_config(self):
        self.after_update = [self._retarget_repo1("release")]

        output = self._run_main()

        self.assertEqual(self._repo1_branches(), ["main", "release"])
        self.assertEqual(output.count("Configuration changed"), 1)

    def test_config_is_reloaded_for_each_restart(self):
        self.after_update = [
            self._retarget_repo1("one"),
            self._retarget_repo1("two"),
        ]

        self._run_main()

        self.assertEqual(self._repo1_branches(), ["main", "one", "two"])

    def test_formatting_only_change_does_not_restart(self):
        self.after_update = [
            lambda: self._write_config(self._read_config(), indent=8, sort_keys=True)
        ]

        output = self._run_main()

        self.assertEqual(len(self.calls), 1)
        self.assertNotIn("Configuration changed", output)

    def test_restarts_are_bounded(self):
        self.after_update = [self._retarget_repo1(f"b{i}") for i in range(10)]

        output = self._run_main()

        # The initial update plus at most three restarts.
        self.assertEqual(self._repo1_branches(), ["main", "b0", "b1", "b2"])
        self.assertEqual(output.count("Configuration changed"), 3)
        self.assertIn("configuration keeps changing", output)

    def test_unchanged_scripts_do_not_reexec(self):
        script = os.path.join(self.workspace, "fake_script.py")
        with open(script, "w") as f:
            f.write("# v1\n")
        restarted = []

        self._run_main(
            script_file_paths=lambda: [Path(script)],
            restart_script=lambda: restarted.append(True),
        )

        self.assertEqual(len(self.calls), 1)
        self.assertEqual(restarted, [])

    def test_changed_script_reexecs(self):
        script = os.path.join(self.workspace, "fake_script.py")
        with open(script, "w") as f:
            f.write("# v1\n")

        def modify_script():
            with open(script, "w") as f:
                f.write("# v2\n")

        self.after_update = [modify_script]
        restarted = []

        def fake_restart():
            restarted.append(True)
            # The real thing never returns; stop the run here too.
            raise SystemExit(0)

        self._run_main(
            script_file_paths=lambda: [Path(script)], restart_script=fake_restart
        )

        self.assertEqual(restarted, [True])
        self.assertEqual(len(self.calls), 1)


if __name__ == "__main__":
    unittest.main()
