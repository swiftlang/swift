# tests/test_shell.py -------------------------------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------

import os
import os.path
import shutil
import sys
import tempfile
import unittest
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from swift_build_support import shell


class ShellTestCase(unittest.TestCase):

    def setUp(self):
        self.tmpdir = os.path.realpath(tempfile.mkdtemp())
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout
        sys.stderr = self.stderr

    def tearDown(self):
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr
        if os.path.exists(self.tmpdir):
            shutil.rmtree(self.tmpdir)

    def test_quote_command(self):
        self.assertEqual(shell.quote_command(["a b", "", "c"]), "'a b' '' c")

    def test_call(self):
        shell.dry_run = False
        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        bar_file = os.path.join(self.tmpdir, 'bar.txt')

        with open(foo_file, 'w') as f:
            f.write("Hello Swift")

        shell.call(['cp', foo_file, bar_file])

        with open(bar_file, 'r') as f:
            self.assertEqual(f.read(), "Hello Swift")

        self.assertEqual(self.stdout.getvalue(), "")
        self.assertEqual(self.stderr.getvalue(), '''\
+ cp {foo_file} {bar_file}
'''.format(foo_file=foo_file, bar_file=bar_file))

    def test_capture(self):
        self.assertEqual(shell.capture(["echo", "hi"]), "hi\n")

        with self.assertRaises(SystemExit):
            shell.capture(["false"])

        self.assertIsNone(shell.capture(["false"], optional=True))

        self.assertEqual(
            shell.capture(["sh", "-c", "echo foo && false"],
                          allow_non_zero_exit=True), "foo\n")

        with self.assertRaises(SystemExit):
            shell.capture(["**not-a-command**"], optional=False)

        self.assertIsNone(shell.capture(["**not-a-command**"], optional=True))

    def test_rmtree(self):
        shell.dry_run = False
        path = os.path.join(self.tmpdir, 'foo', 'bar')
        shell.makedirs(path)

        self.assertTrue(os.path.isdir(path))

        shell.rmtree(os.path.join(path))
        self.assertFalse(
            os.path.exists(os.path.join(path)))
        self.assertTrue(
            os.path.exists(os.path.join(self.tmpdir, 'foo')))

        self.assertEqual(self.stdout.getvalue(), "")
        self.assertEqual(self.stderr.getvalue(), '''\
+ mkdir -p {path}
+ rm -rf {path}
'''.format(path=path))

    def test_pushd(self):
        shell.dry_run = False
        basedir = os.getcwd()

        with shell.pushd(self.tmpdir):
            self.assertEqual(os.getcwd(), self.tmpdir)
        self.assertEqual(os.getcwd(), basedir)

        # pushd inside pushd
        with shell.pushd(self.tmpdir):
            self.assertEqual(os.getcwd(), self.tmpdir)
            shell.makedirs('foo')
            with shell.pushd('foo'):
                self.assertEqual(os.getcwd(),
                                 os.path.join(self.tmpdir, 'foo'))
            self.assertEqual(os.getcwd(), self.tmpdir)
        self.assertEqual(os.getcwd(), basedir)

        # cd inside pushd
        with shell.pushd(self.tmpdir):
            os.chdir('foo')
            self.assertEqual(os.getcwd(), os.path.join(self.tmpdir, 'foo'))
            os.chdir('..')
            self.assertEqual(os.getcwd(), self.tmpdir)
            shell.rmtree('foo')
        self.assertEqual(os.getcwd(), basedir)

        self.assertEqual(self.stdout.getvalue(), "")
        self.assertEqual(self.stderr.getvalue(), '''\
+ pushd {tmpdir}
+ popd
+ pushd {tmpdir}
+ mkdir -p foo
+ pushd foo
+ popd
+ popd
+ pushd {tmpdir}
+ rm -rf foo
+ popd
'''.format(tmpdir=self.tmpdir))

    def test_dry_run(self):
        shell.dry_run = True

        basedir = os.getcwd()
        foobar_dir = os.path.join(self.tmpdir, 'foo', 'bar')

        shell.makedirs(foobar_dir)
        self.assertFalse(os.path.exists(os.path.join(self.tmpdir, 'foo')))
        self.assertFalse(os.path.exists(foobar_dir))

        with shell.pushd(foobar_dir):
            self.assertEqual(os.getcwd(), basedir)
            shell.call(['touch', 'testfile'])
            self.assertFalse(os.path.exists(
                os.path.join(foobar_dir, 'testfile')))

        self.assertEqual(os.getcwd(), basedir)

        shell.rmtree(self.tmpdir)
        self.assertTrue(os.path.exists(self.tmpdir))

        self.assertEqual(self.stdout.getvalue(), '''\
+ mkdir -p {foobar_dir}
+ pushd {foobar_dir}
+ touch testfile
+ popd
+ rm -rf {tmpdir}
'''.format(foobar_dir=foobar_dir, tmpdir=self.tmpdir))
        self.assertEqual(self.stderr.getvalue(), "")
        self.dry_run = False
