from __future__ import absolute_import
import os
import sys

import lit.Test
import lit.util

class TestFormat(object):
    pass

###

class FileBasedTest(TestFormat):
    def getTestsInDirectory(self, testSuite, path_in_suite,
                            litConfig, localConfig):
        source_path = testSuite.getSourcePath(path_in_suite)
        for filename in os.listdir(source_path):
            # Ignore dot files and excluded tests.
            if (filename.startswith('.') or
                filename in localConfig.excludes):
                continue

            filepath = os.path.join(source_path, filename)
            if not os.path.isdir(filepath):
                base,ext = os.path.splitext(filename)
                if ext in localConfig.suffixes:
                    yield lit.Test.Test(testSuite, path_in_suite + (filename,),
                                        localConfig)

###

import re
import tempfile

class OneCommandPerFileTest(TestFormat):
    # FIXME: Refactor into generic test for running some command on a directory
    # of inputs.

    def __init__(self, command, dir, recursive=False,
                 pattern=".*", useTempInput=False):
        if isinstance(command, str):
            self.command = [command]
        else:
            self.command = list(command)
        if dir is not None:
            dir = str(dir)
        self.dir = dir
        self.recursive = bool(recursive)
        self.pattern = re.compile(pattern)
        self.useTempInput = useTempInput

    def getTestsInDirectory(self, testSuite, path_in_suite,
                            litConfig, localConfig):
        dir = self.dir
        if dir is None:
            dir = testSuite.getSourcePath(path_in_suite)

        for dirname,subdirs,filenames in os.walk(dir):
            if not self.recursive:
                subdirs[:] = []

            subdirs[:] = [d for d in subdirs
                          if (d != '.svn' and
                              d not in localConfig.excludes)]

            for filename in filenames:
                if (filename.startswith('.') or
                    not self.pattern.match(filename) or
                    filename in localConfig.excludes):
                    continue

                path = os.path.join(dirname,filename)
                suffix = path[len(dir):]
                if suffix.startswith(os.sep):
                    suffix = suffix[1:]
                test = lit.Test.Test(
                    testSuite, path_in_suite + tuple(suffix.split(os.sep)),
                    localConfig)
                # FIXME: Hack?
                test.source_path = path
                yield test

    def createTempInput(self, tmp, test):
        abstract

    def execute(self, test, litConfig):
        if test.config.unsupported:
            return (lit.Test.UNSUPPORTED, 'Test is unsupported')

        cmd = list(self.command)

        # If using temp input, create a temporary file and hand it to the
        # subclass.
        if self.useTempInput:
            tmp = tempfile.NamedTemporaryFile(suffix='.cpp')
            self.createTempInput(tmp, test)
            tmp.flush()
            cmd.append(tmp.name)
        elif hasattr(test, 'source_path'):
            cmd.append(test.source_path)
        else:
            cmd.append(test.getSourcePath())

        out, err, exitCode = lit.util.executeCommand(cmd)

        diags = out + err
        if not exitCode and not diags.strip():
            return lit.Test.PASS,''

        # Try to include some useful information.
        report = """Command: %s\n""" % ' '.join(["'%s'" % a
                                                 for a in cmd])
        if self.useTempInput:
            report += """Temporary File: %s\n""" % tmp.name
            report += "--\n%s--\n""" % open(tmp.name).read()
        report += """Output:\n--\n%s--""" % diags

        return lit.Test.FAIL, report
