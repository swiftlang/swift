# ===--- test_unknown_scheme.py -------------------------------------------===#
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

from . import scheme_mock


class UnknownSchemeTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(UnknownSchemeTestCase, self).__init__(*args, **kwargs)

    def test_clone_with_unknown_scheme(self):
        with self.assertRaises(scheme_mock.PrintableSubprocessError) as cm:
            self.call(
                [
                    self.update_checkout_path,
                    "--config",
                    self.config_path,
                    "--source-root",
                    self.source_root,
                    "--clone",
                    "--scheme",
                    "mian",
                ]
            )

        self.assertEqual(cm.exception.returncode, 1)
        self.assertIn(
            "update-checkout error: unknown branch scheme 'mian'",
            cm.exception.output,
        )
        self.assertIn("Did you mean one of: main?", cm.exception.output)
        self.assertNotIn("Traceback", cm.exception.output)
