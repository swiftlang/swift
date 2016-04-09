# tests/products/test_libdispatch.py ----------------------------*- python -*-
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
import os
import sys
import shutil
import tempfile
try:
    # py2
    from StringIO import StringIO
except ImportError:
    # py3
    from io import StringIO

from build_script.driver_arguments import Args
from build_script.workspace import Workspace
from build_script.products import Libdispatch
from build_script import shell


class MockSwift(object):
    def __init__(self):
        self.build_dir = '/path/to/swift/build'


class LibdispatchTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        shell.dry_run = True
        source_root = os.path.realpath(tempfile.mkdtemp())
        build_root = os.path.join(source_root, 'build')
        os.makedirs(os.path.join(source_root, 'swift'))
        os.makedirs(os.path.join(source_root, 'swift-corelibs-libdispatch'))
        os.makedirs(build_root)

        cls.workspace = Workspace(source_root=source_root,
                                  build_root=build_root)

        Libdispatch.prepare(cls.workspace)

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.workspace.source_root_dir())
        shell.dry_run = False

    def setUp(self):
        self._stdout = StringIO()
        self.orig_stdout = sys.stdout
        sys.stdout = self._stdout

    def tearDown(self):
        sys.stdout = self.orig_stdout

    def test_configure(self):
        args = Args(reconfigure=True, host_target='dummy')
        build = Libdispatch(
            deployment_target='dummy',
            target_build_dir='/path/to/libdispatch/build',
            target_install_destdir='/path/to/libdispatch/install',
            swift_build=MockSwift(),
            args=args)

        build.configure()
        self.assertEqual(self._stdout.getvalue(), '''\
--- Configuring libdispatch ---
+ mkdir -p /path/to/libdispatch/build
+ pushd /path/to/libdispatch/build
+ {source_dir}/configure \
--prefix=/path/to/libdispatch/install \
--with-swift-toolchain=/path/to/swift/build
+ popd
'''.format(source_dir=self.workspace.subdir('swift-corelibs-libdispatch')))

    def test_build(self):
        args = Args(reconfigure=True, host_target='dummy')
        build = Libdispatch(
            deployment_target='dummy',
            target_build_dir='/path/to/libdispatch/build',
            target_install_destdir='/path/to/libdispatch/install',
            swift_build=MockSwift(),
            args=args)

        build.build()
        self.assertEqual(self._stdout.getvalue(), '''\
--- Building libdispatch ---
+ pushd /path/to/libdispatch/build
+ make
+ cd tests
+ make build-tests
+ popd
''')

    def test_test(self):
        args = Args(skip_test_libdispatch=False, host_target="dummy")
        build = Libdispatch(
            deployment_target='dummy',
            target_build_dir='/path/to/libdispatch/build',
            target_install_destdir='/path/to/libdispatch/install',
            swift_build=MockSwift(),
            args=args)

        build.test()
        self.assertEqual(self._stdout.getvalue(), '''\
--- Running tests for libdispatch ---
+ pushd /path/to/libdispatch/build
+ make test
+ popd
''')

    def test_install(self):
        args = Args(install_libdispatch=True,
                    reconfigure=True,
                    host_target='dummy')
        build = Libdispatch(
            deployment_target='dummy',
            target_build_dir='/path/to/libdispatch/build',
            target_install_destdir='/path/to/libdispatch/install',
            swift_build=MockSwift(),
            args=args)
        build.install()
        self.assertEqual(self._stdout.getvalue(), '''\
--- Installing libdispatch ---
+ pushd /path/to/libdispatch/build
+ make install
+ popd
''')
