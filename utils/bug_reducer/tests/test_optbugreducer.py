# ==--- opt_bug_reducer_test.py ------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http:#swift.org/LICENSE.txt for license information
# See http:#swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ==----------------------------------------------------------------------===#


import json
import os
import platform
import random
import shutil
import subprocess
import unittest


import bug_reducer.bug_reducer_utils as bug_reducer_utils


@unittest.skipUnless(platform.system() == 'Darwin',
                     'opt_bug_reducer is only available on Darwin for now')
class OptBugReducerTestCase(unittest.TestCase):

    def setUp(self):
        self.file_dir = os.path.dirname(os.path.abspath(__file__))
        self.reducer = os.path.join(os.path.dirname(self.file_dir),
                                    'bug_reducer', 'bug_reducer.py')
        self.build_dir = os.path.abspath(os.environ['BUGREDUCE_TEST_SWIFT_OBJ_ROOT'])
        self.tmp_dir = os.path.abspath(os.environ['BUGREDUCE_TEST_TMP_DIR'])

        self.module_cache = os.path.join(self.tmp_dir, 'module_cache')
        self.sdk = subprocess.check_output(['xcrun', '--sdk', 'macosx',
                                            '--toolchain', 'Default',
                                            '--show-sdk-path']).strip("\n")
        self.tools = bug_reducer_utils.SwiftTools(self.build_dir)
        json_data = json.loads(subprocess.check_output(
            [self.tools.sil_passpipeline_dumper, '-Performance']))
        self.passes = []
        for y in (x[2:] for x in json_data):
            for z in y:
                self.passes.append('--pass=-' + z[1])
        random.seed(0xf487c07f)
        random.shuffle(self.passes)
        self.passes.insert(random.randint(0, len(self.passes)),
                           '--pass=-bug-reducer-tester')

        if os.access(self.tmp_dir, os.F_OK):
            shutil.rmtree(self.tmp_dir)
        os.makedirs(self.tmp_dir)
        os.makedirs(self.module_cache)

    def _get_test_file_path(self, module_name):
        (root, _) = os.path.splitext(os.path.abspath(__file__))
        root_basename = ''.join(os.path.basename(root).split('_'))
        return os.path.join(self.file_dir,
                            '{}_{}.swift'.format(root_basename, module_name))

    def _get_sib_file_path(self, filename):
        (root, ext) = os.path.splitext(filename)
        return os.path.join(self.tmp_dir, os.path.basename(root) + '.sib')

    def run_swiftc_command(self, name):
        input_file_path = self._get_test_file_path(name)
        args = [self.tools.swiftc,
                '-module-cache-path', self.module_cache,
                '-sdk', self.sdk,
                '-Onone', '-parse-as-library',
                '-module-name', name,
                '-emit-sib',
                '-resource-dir', os.path.join(self.build_dir, 'lib', 'swift'),
                '-o', self._get_sib_file_path(input_file_path),
                input_file_path]
        #print ' '.join(args)
        return subprocess.check_call(args)

    def test_basic(self):
        name = 'testbasic'
        result_code = self.run_swiftc_command(name)
        assert result_code == 0, "Failed initial compilation"
        args = [
            self.reducer,
            'opt',
            self.build_dir,
            self._get_sib_file_path(self._get_test_file_path(name)),
            '--sdk=%s' % self.sdk,
            '--module-cache=%s' % self.module_cache,
            '--module-name=%s' % name,
            '--work-dir=%s' % self.tmp_dir,
            '--extra-arg=-bug-reducer-tester-target-func=test_target'
        ]
        args.extend(self.passes)
        output = subprocess.check_output(args).split("\n")
        success_message = 'Found miscompiling passes: --bug-reducer-tester'
        self.assertTrue(success_message in output)

if __name__ == '__main__':
    unittest.main()
