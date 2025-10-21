# ===--- test_dump.py -----------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https:#swift.org/LICENSE.txt for license information
# See https:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ===----------------------------------------------------------------------===#

import json

from . import scheme_mock


class DumpTestCase(scheme_mock.SchemeMockTestCase):

    def __init__(self, *args, **kwargs):
        super(DumpTestCase, self).__init__(*args, **kwargs)

    def test_dump_hashes_json(self):
        # First do the clone.
        self.call([self.update_checkout_path,
                   '--config', self.config_path,
                   '--source-root', self.source_root,
                   '--clone'])

        # Then dump the hashes.
        output = self.call([self.update_checkout_path,
                            '--config', self.config_path,
                            '--source-root', self.source_root,
                            '--dump-hashes'])
        # The output should be valid JSON
        result = json.loads(output)

        # And it should have some basic properties we expect from this JSON
        self.assertIn("https-clone-pattern", result)
        self.assertEqual(result["repos"], scheme_mock.MOCK_CONFIG["repos"])
        self.assertEqual(result["ssh-clone-pattern"], "DO_NOT_USE")
        self.assertSetEqual(set(result["branch-schemes"].keys()), {"repro"})
