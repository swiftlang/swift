# ===--- test_merge_config.py ---------------------------------------------===#
#
#  This source file is part of the Swift.org open source project
#
#  Copyright (c) 2025 Apple Inc. and the Swift project authors
#  Licensed under Apache License v2.0 with Runtime Library Exception
#
#  See https:#swift.org/LICENSE.txt for license information
#  See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import unittest

from update_checkout.update_checkout import merge_config, merge_no_duplicates


class MergeTestCase(unittest.TestCase):
    def test_no_duplicates(self):
        self.assertEqual(merge_no_duplicates({}, {}), {})
        self.assertEqual(merge_no_duplicates({"a": 1}, {"b": 2}), {"a": 1, "b": 2})
        with self.assertRaises(ValueError):
            merge_no_duplicates({"a": 1, "b": 2}, {"b": 3})

    def test_merge_config(self):
        default_config = {
            "ssh-clone-pattern": "git@1",
            "https-clone-pattern": "https://1",
            "repos": {
                "swift": {"remote": {"id": "swiftlang/swift"}},
                "llvm-project": {"remote": {"id": "swiftlang/llvm-project"}},
            },
            "default-branch-scheme": "main",
            "branch-schemes": {
                "main": {
                    "aliases": ["swift/main", "main", "stable/20240723"],
                },
            },
        }

        self.assertEqual(merge_config(default_config, {
            "note": "this is machine generated or something",
            "ssh-clone-pattern": "git@2",
            "repos": {
                "llvm-project": {"remote": {"id": "blah/llvm-project"}},
                "swift-syntax": {"remote": {"id": "swiftlang/swift-syntax"}},
            },
            "default-branch-scheme": "bonus",
            "branch-schemes": {
                "bonus": {
                    "aliases": ["bonus", "also-bonus"],
                },
            },
        }), {
            "ssh-clone-pattern": "git@2",
            "https-clone-pattern": "https://1",
            "repos": {
                "swift": {"remote": {"id": "swiftlang/swift"}},
                "llvm-project": {"remote": {"id": "blah/llvm-project"}},
                "swift-syntax": {"remote": {"id": "swiftlang/swift-syntax"}},
            },
            "default-branch-scheme": "bonus",
            "branch-schemes": {
                "main": {
                    "aliases": ["swift/main", "main", "stable/20240723"],
                },
                "bonus": {
                    "aliases": ["bonus", "also-bonus"],
                },
            },
            "note": "this is machine generated or something",
        })

        with self.assertRaises(ValueError):
            merge_config(default_config, default_config)
