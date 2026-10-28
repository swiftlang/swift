# tests/products/test_stdlib_docs.py ----------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------

import os
import shutil
import tempfile
import unittest

from swift_build_support.products.stdlib_docs import (
    EXCLUDED_SYMBOL_GRAPH_MODULES,
    remove_excluded_symbol_graphs,
    symbol_graph_module_name,
)


class SymbolGraphModuleNameTestCase(unittest.TestCase):

    def test_plain_module(self):
        self.assertEqual(
            symbol_graph_module_name('Swift.symbols.json'), 'Swift')

    def test_extension_graph_uses_extending_module(self):
        self.assertEqual(
            symbol_graph_module_name('_RegexParser@Swift.symbols.json'),
            '_RegexParser')

    def test_non_symbol_graph_file(self):
        self.assertIsNone(symbol_graph_module_name('README.txt'))


class RemoveExcludedSymbolGraphsTestCase(unittest.TestCase):

    def setUp(self):
        self.root = os.path.realpath(tempfile.mkdtemp())

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def _touch(self, variant, filename):
        directory = os.path.join(self.root, variant)
        os.makedirs(directory, exist_ok=True)
        with open(os.path.join(directory, filename), 'w') as f:
            f.write('{}')

    def _remaining(self):
        found = set()
        for root, _, files in os.walk(self.root):
            for name in files:
                found.add(os.path.relpath(os.path.join(root, name), self.root))
        return found

    def test_keeps_public_modules(self):
        self._touch('swiftCore-macosx-arm64', 'Swift.symbols.json')
        self._touch('swift_Concurrency-macosx-arm64',
                    '_Concurrency@Swift.symbols.json')
        remove_excluded_symbol_graphs(self.root)
        self.assertEqual(self._remaining(), {
            'swiftCore-macosx-arm64/Swift.symbols.json',
            'swift_Concurrency-macosx-arm64/_Concurrency@Swift.symbols.json',
        })

    def test_removes_excluded_modules_and_their_extensions(self):
        self._touch('swiftCore-macosx-arm64', 'Swift.symbols.json')
        self._touch('swift_RegexParser-macosx-arm64',
                    '_RegexParser.symbols.json')
        self._touch('swift_RegexParser-macosx-arm64',
                    '_RegexParser@Swift.symbols.json')
        self._touch('swiftSwiftOnoneSupport-macosx-arm64',
                    'SwiftOnoneSupport.symbols.json')
        self._touch('swiftStdlibUnittestFoundationExtras-macosx-arm64',
                    'StdlibUnittestFoundationExtras@Foundation.symbols.json')
        remove_excluded_symbol_graphs(self.root)
        self.assertEqual(
            self._remaining(), {'swiftCore-macosx-arm64/Swift.symbols.json'})

    def test_ignores_non_symbol_graph_files(self):
        self._touch('swiftCore-macosx-arm64', 'notes.txt')
        remove_excluded_symbol_graphs(self.root)
        self.assertEqual(self._remaining(), {'swiftCore-macosx-arm64/notes.txt'})

    def test_missing_directory_is_not_an_error(self):
        remove_excluded_symbol_graphs(os.path.join(self.root, 'absent'))

    def test_exclusion_list_covers_known_extraneous_modules(self):
        for module in ('_RegexParser', 'SwiftOnoneSupport', 'StdlibUnittest',
                       'StdlibUnittestFoundationExtras', 'RuntimeUnittest',
                       'SwiftReflectionTest', 'OSLogTestHelper',
                       'SwiftPrivate', 'SwiftPrivateThreadExtras',
                       'SwiftPrivateLibcExtras', 'Darwin',
                       'DifferentiationUnittest'):
            self.assertIn(module, EXCLUDED_SYMBOL_GRAPH_MODULES)


if __name__ == '__main__':
    unittest.main()
