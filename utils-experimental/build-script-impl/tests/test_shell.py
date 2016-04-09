# tests/test_shell.py -------------------------------------------*- python -*-
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

import unittest
import sys
import os
import os.path
import shutil
import tempfile
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from build_script import shell


class ShellTestCase(unittest.TestCase):

    def setUp(self):
        self.tmpdir = os.path.realpath(tempfile.mkdtemp())

    def tearDown(self):
        if os.path.exists(self.tmpdir):
            shutil.rmtree(self.tmpdir)

    def test_query(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        with open(foo_file, 'w') as f:
            f.write("Hello Swift")

        result = shell.query(['cat', foo_file])
        self.assertEqual(result, 'Hello Swift')

    def test_invoke(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        bar_file = os.path.join(self.tmpdir, 'bar.txt')
        with open(foo_file, 'w') as f:
            f.write("Hello Swift")

        shell.invoke(['cp', foo_file, bar_file])

        with open(bar_file, 'r') as f:
            self.assertEqual(f.read(), "Hello Swift")

    def test_runscript(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        bar_file = os.path.join(self.tmpdir, 'bar.txt')
        script = '''\
echo Hello Swift > {foo_file}
cat {foo_file} | tr '[A-Z]' '[a-z]' > {bar_file}
'''
        script = script.format(foo_file=foo_file, bar_file=bar_file)
        shell.runscript(script)

        with open(bar_file, 'r') as f:
            self.assertEqual(f.read(), "hello swift\n")

    def test_copy(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        bar_file = os.path.join(self.tmpdir, 'bar.txt')

        with open(foo_file, 'w') as f:
            f.write("Hello Swift")
        shell.copy(foo_file, bar_file)
        with open(bar_file, 'r') as f:
            self.assertEqual(f.read(), "Hello Swift")

    def test_symlink(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')
        bar_file = os.path.join(self.tmpdir, 'bar.txt')
        with open(foo_file, 'w') as f:
            f.write("Hello Swift")

        # absolute
        shell.symlink(foo_file, bar_file)
        self.assertTrue(os.path.islink(bar_file))
        self.assertEqual(os.readlink(bar_file), foo_file)
        with open(bar_file, 'r') as f:
            self.assertEqual(f.read(), "Hello Swift")
        os.remove(bar_file)
        self.assertTrue(os.path.exists(foo_file))
        self.assertFalse(os.path.exists(bar_file))

        # relative manually
        rel_target = os.path.join('..',
                                  os.path.basename(self.tmpdir),
                                  'foo.txt')
        shell.symlink(rel_target, bar_file)
        self.assertTrue(os.path.islink(bar_file))
        self.assertEqual(os.readlink(bar_file), rel_target)
        os.remove(bar_file)

        # auto relative
        shell.symlink(foo_file, bar_file, relative=True)
        self.assertEqual(os.readlink(bar_file), 'foo.txt')

    def test_remove(self):
        shell.echo = False
        shell.dry_run = False

        foo_file = os.path.join(self.tmpdir, 'foo.txt')

        with open(foo_file, 'w') as f:
            f.write("Hello Swift")
        self.assertTrue(os.path.exists(foo_file))
        shell.remove(foo_file)
        self.assertFalse(os.path.exists(foo_file))
        self.assertTrue(os.path.exists(self.tmpdir))

    def test_makedirs(self):
        shell.echo = False
        shell.dry_run = False

        path = os.path.join(self.tmpdir, 'foo', 'bar', 'baz')

        shell.makedirs(path)
        self.assertTrue(os.path.isdir(path))

    def test_rmtree(self):
        shell.echo = False
        shell.dry_run = False

        path = os.path.join(self.tmpdir, 'foo', 'bar', 'baz')
        os.makedirs(path)

        self.assertTrue(os.path.isdir(path))
        shell.rmtree(os.path.join(self.tmpdir, 'foo', 'bar'))
        self.assertFalse(
            os.path.exists(os.path.join(self.tmpdir, 'foo', 'bar')))
        self.assertTrue(
            os.path.exists(os.path.join(self.tmpdir, 'foo')))

    def test_copytree(self):
        shell.echo = False
        shell.dry_run = False

        path1 = os.path.join(self.tmpdir, 'path', 'to')
        os.makedirs(path1)
        open(os.path.join(path1, 'foo'), 'a').close()
        os.symlink('foo', os.path.join(path1, 'bar'))

        shell.copytree(os.path.join(self.tmpdir, 'path'),
                       os.path.join(self.tmpdir, 'copy'))

        self.assertTrue(os.path.isdir(os.path.join(self.tmpdir, 'copy')))
        # symlink remains symlink
        new_bar = os.path.join(self.tmpdir, 'copy', 'to', 'bar')
        self.assertEqual(os.readlink(new_bar), 'foo')

    def test_chdir(self):
        shell.echo = False
        shell.dry_run = False

        basedir = os.getcwd()

        # Basic
        shell.chdir(self.tmpdir)
        self.assertEqual(os.getcwd(), self.tmpdir)
        shell.chdir(basedir)
        self.assertEqual(os.getcwd(), basedir)

        foo_dir = os.path.join(self.tmpdir, 'foo')
        bar_dir = os.path.join(self.tmpdir, 'bar')
        os.makedirs(foo_dir)
        os.makedirs(bar_dir)

        # relative path
        shell.chdir(foo_dir)
        self.assertEqual(os.getcwd(), foo_dir)
        shell.chdir(os.path.join('..', 'bar'))
        self.assertEqual(os.getcwd(), bar_dir)
        shell.chdir(os.path.join('..'))
        self.assertEqual(os.getcwd(), self.tmpdir)

        shell.chdir(basedir)
        self.assertEqual(os.getcwd(), basedir)

    def test_pushd(self):

        shell.echo = False
        shell.dry_run = False

        basedir = os.getcwd()
        with shell.pushd(self.tmpdir):
            self.assertEqual(os.getcwd(), self.tmpdir)
        self.assertEqual(os.getcwd(), basedir)

        # pushd inside pushd
        with shell.pushd(self.tmpdir):
            self.assertEqual(os.getcwd(), self.tmpdir)
            os.makedirs('foo')
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
            os.rmdir('foo')
        self.assertEqual(os.getcwd(), basedir)

    def test_echo(self):

        shell.echo = True
        shell.dry_run = False

        out = StringIO()
        _orig_stdout = sys.stdout
        sys.stdout = out

        foobar_dir = os.path.join(self.tmpdir, 'foo', 'bar')
        foobar2_dir = os.path.join(self.tmpdir, 'foo', 'bar2')

        shell.makedirs(foobar_dir)
        with shell.pushd(foobar_dir):
            shell.makedirs('subdir')
            shell.chdir('subdir')
            shell.invoke(['touch', 'testfile'])
            shell.query(['date', '+%Y-%m-%d'])
            shell.copy('testfile', 'testfile2')
            shell.symlink('testfile', 'testlink')
        shell.copytree(foobar_dir, foobar2_dir)
        shell.rmtree(foobar_dir)
        self.assertTrue(
            os.path.exists(os.path.join(foobar2_dir, 'subdir', 'testfile')))
        self.assertEqual(
            os.readlink(os.path.join(foobar2_dir, 'subdir', 'testlink')),
            'testfile')
        self.assertEqual(out.getvalue(), '''\
+ mkdir -p {foobar_dir}
+ pushd {foobar_dir}
+ mkdir -p subdir
+ cd subdir
+ touch testfile
+ date +%Y-%m-%d
+ cp testfile testfile2
+ ln -s testfile testlink
+ popd
+ cp -r {foobar_dir} {foobar2_dir}
+ rm -rf {foobar_dir}
'''.format(foobar_dir=foobar_dir,
           foobar2_dir=foobar2_dir))

        sys.stdout = _orig_stdout

    def test_dryrun(self):
        shell.echo = False
        shell.dry_run = True

        out = StringIO()
        _orig_stdout = sys.stdout
        sys.stdout = out

        basedir = os.getcwd()
        foobar_dir = os.path.join(self.tmpdir, 'foo', 'bar')
        foobar2_dir = os.path.join(self.tmpdir, 'foo', 'bar2')

        shell.makedirs(foobar_dir)
        self.assertFalse(os.path.exists(foobar_dir))
        with shell.pushd(foobar_dir):
            self.assertEqual(os.getcwd(), basedir)
            shell.makedirs('subdir')
            shell.chdir('subdir')
            self.assertEqual(os.getcwd(), basedir)
            shell.invoke(['touch', 'testfile'])
            self.assertFalse(os.path.exists(os.path.join(foobar_dir,
                                                         'subdir',
                                                         'testfile')))
            shell.query(['date', '+%Y-%m-%d'])
            shell.copy('testfile', 'testfile2')
            self.assertFalse(os.path.exists(os.path.join(foobar_dir,
                                                         'subdir',
                                                         'testfile2')))
            shell.symlink('testfile', 'testlink')
            self.assertFalse(os.path.exists(os.path.join(foobar_dir,
                                                         'subdir',
                                                         'testlink')))
        shell.copytree(foobar_dir, foobar2_dir)
        self.assertFalse(os.path.exists(foobar2_dir))
        shell.rmtree(self.tmpdir)
        self.assertTrue(os.path.exists(self.tmpdir))

        # dry_run always echo **except** query that doesn't have dry_run
        # semantics
        self.assertEqual(out.getvalue(), '''\
+ mkdir -p {foobar_dir}
+ pushd {foobar_dir}
+ mkdir -p subdir
+ cd subdir
+ touch testfile
+ cp testfile testfile2
+ ln -s testfile testlink
+ popd
+ cp -r {foobar_dir} {foobar2_dir}
+ rm -rf {tmpdir}
'''.format(foobar_dir=foobar_dir,
           foobar2_dir=foobar2_dir,
           tmpdir=self.tmpdir))

        sys.stdout = _orig_stdout
