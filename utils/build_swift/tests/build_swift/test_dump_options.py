# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors


"""Tests for build-script's --dump-options flag.

The flag emits a single JSON document describing every option both the
Python layer (driver_arguments.create_argument_parser) and the bash
layer (build-script-impl KNOWN_SETTINGS) recognize. These tests
spot-check both layers — if a long-stable option ever falls out of the
dump, downstream tooling (shell completion, doc generation) silently
breaks.
"""


import json
import platform
import subprocess
import unittest

from build_swift import driver_arguments
from build_swift.constants import BUILD_SCRIPT_IMPL_PATH


def _all_option_strings(parser):
    """Flatten parser._actions into the set of every recognized option."""
    return {opt for action in parser._actions for opt in action.option_strings}


class TestDumpOptionsPythonLayer(unittest.TestCase):

    def setUp(self):
        self.parser = driver_arguments.create_argument_parser()
        self.options = _all_option_strings(self.parser)

    def test_well_known_options_present(self):
        for opt in [
            '-i', '--ios',
            '--build-llvm',
            '--reconfigure',
            '--dump-config',
            '--dump-options',
        ]:
            self.assertIn(
                opt, self.options,
                msg='{} missing from build-script parser'.format(opt))


class TestDumpOptionsImplLayer(unittest.TestCase):

    def setUp(self):
        if platform.system() == 'Windows':
            self.skipTest('build-script-impl cannot run on Windows')

        result = subprocess.run(
            [BUILD_SCRIPT_IMPL_PATH, '--dump-options'],
            check=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            universal_newlines=True)
        self.entries = json.loads(result.stdout)
        self.names = {e['name'] for e in self.entries}

    def test_dump_is_nonempty_list(self):
        self.assertIsInstance(self.entries, list)
        self.assertGreater(len(self.entries), 0)

    def test_well_known_options_present(self):
        for name in [
            'reconfigure',
            'build-jobs',
            'host-cc',
            'cmake-generator',
            'workspace',
        ]:
            self.assertIn(
                name, self.names,
                msg='--{} missing from build-script-impl dump'.format(name))

    def test_entry_shape(self):
        for entry in self.entries:
            self.assertEqual(
                set(entry.keys()), {'name', 'option', 'default', 'help'})
            self.assertEqual(entry['option'], '--' + entry['name'])
