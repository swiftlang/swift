from __future__ import absolute_import
import os
import sys

import lit.Test
import lit.TestRunner
import lit.util
from .base import TestFormat

kIsWindows = sys.platform in ['win32', 'cygwin']

class GoogleTest(TestFormat):
    def __init__(self, test_sub_dir, test_suffix):
        self.test_sub_dir = os.path.normcase(str(test_sub_dir)).split(';')
        self.test_suffix = str(test_suffix)

        # On Windows, assume tests will also end in '.exe'.
        if kIsWindows:
            self.test_suffix += '.exe'

    def getGTestTests(self, path, litConfig, localConfig):
        """getGTestTests(path) - [name]

        Return the tests available in gtest executable.

        Args:
          path: String path to a gtest executable
          litConfig: LitConfig instance
          localConfig: TestingConfig instance"""

        try:
            lines = lit.util.capture([path, '--gtest_list_tests'],
                                     env=localConfig.environment)
            if kIsWindows:
              lines = lines.replace('\r', '')
            lines = lines.split('\n')
        except:
            litConfig.error("unable to discover google-tests in %r" % path)
            raise StopIteration

        nested_tests = []
        for ln in lines:
            if not ln.strip():
                continue

            prefix = ''
            index = 0
            while ln[index*2:index*2+2] == '  ':
                index += 1
            while len(nested_tests) > index:
                nested_tests.pop()

            ln = ln[index*2:]
            if ln.endswith('.'):
                nested_tests.append(ln)
            elif any([name.startswith('DISABLED_')
                      for name in nested_tests + [ln]]):
                # Gtest will internally skip these tests. No need to launch a
                # child process for it.
                continue
            else:
                yield ''.join(nested_tests) + ln

    # Note: path_in_suite should not include the executable name.
    def getTestsInExecutable(self, testSuite, path_in_suite, execpath,
                             litConfig, localConfig):
        if not execpath.endswith(self.test_suffix):
            return
        (dirname, basename) = os.path.split(execpath)
        # Discover the tests in this executable.
        for testname in self.getGTestTests(execpath, litConfig, localConfig):
            testPath = path_in_suite + (basename, testname)
            yield lit.Test.Test(testSuite, testPath, localConfig, file_path=execpath)

    def getTestsInDirectory(self, testSuite, path_in_suite,
                            litConfig, localConfig):
        source_path = testSuite.getSourcePath(path_in_suite)
        for filename in os.listdir(source_path):
            filepath = os.path.join(source_path, filename)
            if os.path.isdir(filepath):
                # Iterate over executables in a directory.
                if not os.path.normcase(filename) in self.test_sub_dir:
                    continue
                dirpath_in_suite = path_in_suite + (filename, )
                for subfilename in os.listdir(filepath):
                    execpath = os.path.join(filepath, subfilename)
                    for test in self.getTestsInExecutable(
                            testSuite, dirpath_in_suite, execpath,
                            litConfig, localConfig):
                      yield test
            elif ('.' in self.test_sub_dir):
                for test in self.getTestsInExecutable(
                        testSuite, path_in_suite, filepath,
                        litConfig, localConfig):
                    yield test

    def execute(self, test, litConfig):
        testPath,testName = os.path.split(test.getSourcePath())
        while not os.path.exists(testPath):
            # Handle GTest parametrized and typed tests, whose name includes
            # some '/'s.
            testPath, namePrefix = os.path.split(testPath)
            testName = namePrefix + '/' + testName

        cmd = [testPath, '--gtest_filter=' + testName]
        if litConfig.useValgrind:
            cmd = litConfig.valgrindArgs + cmd

        if litConfig.noExecute:
            return lit.Test.PASS, ''

        try:
            out, err, exitCode = lit.util.executeCommand(
                cmd, env=test.config.environment,
                timeout=litConfig.maxIndividualTestTime)
        except lit.util.ExecuteCommandTimeoutException:
            return (lit.Test.TIMEOUT,
                    'Reached timeout of {} seconds'.format(
                        litConfig.maxIndividualTestTime)
                   )

        if exitCode:
            return lit.Test.FAIL, out + err

        passing_test_line = '[  PASSED  ] 1 test.'
        if passing_test_line not in out:
            msg = ('Unable to find %r in gtest output:\n\n%s%s' %
                   (passing_test_line, out, err))
            return lit.Test.UNRESOLVED, msg

        return lit.Test.PASS,''

