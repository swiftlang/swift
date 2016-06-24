# tests/test_build_script.py ------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import json
import os
import unittest

from swift_build_support import arguments
from swift_build_support import shell


class BuildScriptTestCase(unittest.TestCase):
    build_script_path = os.path.join(os.path.dirname(__file__), "..", "..",
                                     "build-script")

    @classmethod
    def get_build_script_argument_state(cls, args):
        """
        Get a dumped representation of the build-script argument state for the
        given arguments.
        """

        # Test the initialization arguments and propagation of defaults.
        data = shell.capture(
            [cls.build_script_path] + args + ["--dump-argument-state"],
            echo=False)
        state = json.loads(data)

        # Convert the loaded state into a NestedNamespace, for convenient
        # access.
        def convert(item):
            if isinstance(item, dict):
                result = arguments.NestedNamespace()
                for (key, value) in item.items():
                    result[key] = convert(value)
                return result
            else:
                return item
        return convert(state)

    def test_product_configuration(self):
        # Validate which projects are built by default.
        args = BuildScriptTestCase.get_build_script_argument_state([])
        self.assertEqual(args.products.cmark.build, True)
        self.assertEqual(args.products.llvm.build, True)
        self.assertEqual(args.products.swift.build, True)
        self.assertEqual(args.products.llbuild.build, False)
        self.assertEqual(args.products.libdispatch.build, False)
        self.assertEqual(args.products.swiftpm.build, False)

        # Validate --skip-build propagation.
        args = BuildScriptTestCase.get_build_script_argument_state([
            "--skip-build"])
        self.assertEqual(args.products.cmark.build, False)
        self.assertEqual(args.products.llvm.build, False)
        self.assertEqual(args.products.swift.build, False)
        self.assertEqual(args.products.llbuild.build, False)
        self.assertEqual(args.products.libdispatch.build, False)
        self.assertEqual(args.products.swiftpm.build, False)
