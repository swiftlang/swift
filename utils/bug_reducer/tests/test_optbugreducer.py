# ==--- opt_bug_reducer_test.py ------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ==----------------------------------------------------------------------===#


import json
import os
import platform
import random
import re
import shutil
import subprocess
import unittest


import bug_reducer.swift_tools as swift_tools


@unittest.skipUnless(platform.system() == 'Darwin',
                     'opt_bug_reducer is only available on Darwin for now')
class OptBugReducerTestCase(unittest.TestCase):

    def setUp(self):
        self.file_dir = os.path.dirname(os.path.abspath(__file__))
        self.reducer = os.path.join(os.path.dirname(self.file_dir),
                                    'bug_reducer', 'bug_reducer.py')
        self.build_dir = os.path.abspath(
            os.environ['BUGREDUCE_TEST_SWIFT_OBJ_ROOT'])

        (root, _) = os.path.splitext(os.path.abspath(__file__))
        self.root_basename = ''.join(os.path.basename(root).split('_'))
        self.tmp_dir = os.path.join(os.path.abspath(
            os.environ['BUGREDUCE_TEST_TMP_DIR']),
            self.root_basename)
        subprocess.call(['mkdir', '-p', self.tmp_dir])

        self.module_cache = os.path.join(self.tmp_dir, 'module_cache')
        self.sdk = subprocess.check_output(['xcrun', '--sdk', 'macosx',
                                            '--toolchain', 'Default',
                                            '--show-sdk-path']).strip("\n")
        self.tools = swift_tools.SwiftTools(self.build_dir)
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
        return os.path.join(self.file_dir,
                            '{}_{}.swift'.format(
                                self.root_basename, module_name))

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
        subprocess.check_call(args)

    def test_basic(self):
        name = 'testbasic'
        self.run_swiftc_command(name)
        args = [
            self.reducer,
            'opt',
            self.build_dir,
            self._get_sib_file_path(self._get_test_file_path(name)),
            '--sdk=%s' % self.sdk,
            '--module-cache=%s' % self.module_cache,
            '--module-name=%s' % name,
            '--work-dir=%s' % self.tmp_dir,
            '--extra-arg=-bug-reducer-tester-target-func=test_target',
            '--extra-arg=-bug-reducer-tester-failure-kind=opt-crasher'
        ]
        args.extend(self.passes)
        output = subprocess.check_output(args).split("\n")
        self.assertTrue('*** Found miscompiling passes!' in output)
        self.assertTrue('*** Final Passes: --bug-reducer-tester' in output)
        re_end = 'testoptbugreducer_testbasic_initial'
        output_file_re = re.compile('\*\*\* Final File: .*' + re_end)
        output_matches = [
            1 for o in output if output_file_re.match(o) is not None]
        self.assertEquals(sum(output_matches), 1)
        # Make sure our final output command does not have -emit-sib in
        # the output. We want users to get sil output when they type in
        # the relevant command.
        self.assertEquals([], [o for o in output if '-emit-sib' in o])

    def test_suffix_in_need_of_prefix(self):
        name = 'testsuffixinneedofprefix'
        self.run_swiftc_command(name)
        args = [
            self.reducer,
            'opt',
            self.build_dir,
            self._get_sib_file_path(self._get_test_file_path(name)),
            '--sdk=%s' % self.sdk,
            '--module-cache=%s' % self.module_cache,
            '--module-name=%s' % name,
            '--work-dir=%s' % self.tmp_dir,
            '--extra-arg=-bug-reducer-tester-target-func=closure_test_target',
            '--extra-arg=-bug-reducer-tester-failure-kind=opt-crasher'
        ]
        args.extend(self.passes)
        output = subprocess.check_output(args).split("\n")
        self.assertTrue('*** Found miscompiling passes!' in output)
        self.assertTrue('*** Final Passes: --bug-reducer-tester' in output)
        re_end = 'testoptbugreducer_testsuffixinneedofprefix_initial'
        output_file_re = re.compile('\*\*\* Final File: .*' + re_end)
        output_matches = [
            1 for o in output if output_file_re.match(o) is not None]
        self.assertEquals(sum(output_matches), 1)
        # Make sure our final output command does not have -emit-sib in the
        # output. We want users to get sil output when they type in the
        # relevant command.
        self.assertEquals([], [o for o in output if '-emit-sib' in o])

    def test_reduce_function(self):
        name = 'testreducefunction'
        self.run_swiftc_command(name)
        args = [
            self.reducer,
            'opt',
            self.build_dir,
            self._get_sib_file_path(self._get_test_file_path(name)),
            '--sdk=%s' % self.sdk,
            '--module-cache=%s' % self.module_cache,
            '--module-name=%s' % name,
            '--work-dir=%s' % self.tmp_dir,
            '--extra-arg=-bug-reducer-tester-target-func=__TF_test_target',
            '--extra-arg=-bug-reducer-tester-failure-kind=opt-crasher',
            '--reduce-sil'
        ]
        args.extend(self.passes)
        output = subprocess.check_output(args).split("\n")
        self.assertTrue('*** Found miscompiling passes!' in output)
        self.assertTrue(
            '*** Final Functions: $s18testreducefunction6foo413yyF' in output)
        self.assertTrue('*** Final Passes: --bug-reducer-tester' in output)
        re_end = 'testoptbugreducer_testreducefunction_initial_'
        re_end += '30775a3d942671a403702a9846afa7a4.sib'
        output_file_re = re.compile('\*\*\* Final File: .*' + re_end)
        output_matches = [
            1 for o in output if output_file_re.match(o) is not None]
        self.assertEquals(sum(output_matches), 1)
        # Make sure our final output command does not have -emit-sib in the
        # output. We want users to get sil output when they type in the
        # relevant command.
        self.assertEquals([], [o for o in output if '-emit-sib' in o])


if __name__ == '__main__':
    unittest.main()
