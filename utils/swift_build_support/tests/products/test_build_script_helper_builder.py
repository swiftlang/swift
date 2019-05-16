# tests/products/test_build_script_helper_builder.py -------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
# ----------------------------------------------------------------------------

import argparse
import os
import platform
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
from swift_build_support.products.build_script_helper_builder import \
    BuildScriptHelperBuilder
from swift_build_support.products.product import Product
from swift_build_support.targets import StdlibDeploymentTarget
from swift_build_support.workspace import Workspace


# BuildScriptHelperBuilder needs concrete subclasses to work.
class TestingBuildScriptHelperBuilder(BuildScriptHelperBuilder):
    def _should_test(self):
        return True


class NonTestingBuildScriptHelperBuilder(BuildScriptHelperBuilder):
    def _should_test(self):
        return False


# We also need a concrete product
class MockProduct(Product):
    pass


class BuildScriptHelperBuilderTestCase(unittest.TestCase):
    def setUp(self):
        # Setup workspace
        tmpdir1 = os.path.realpath(tempfile.mkdtemp())
        tmpdir2 = os.path.realpath(tempfile.mkdtemp())
        os.makedirs(os.path.join(tmpdir1, MockProduct.product_source_name()))

        self.host = StdlibDeploymentTarget.host_target()

        self.workspace = Workspace(source_root=tmpdir1,
                                   build_root=tmpdir2)

        # Setup args
        self.args = argparse.Namespace(
            build_variant=None,
            install_destdir='/dest/dir/path',
            install_prefix='/install/prefix/path.toolchain/usr')

        # Setup shell
        shell.dry_run = True
        self._orig_stdout = sys.stdout
        self._orig_stderr = sys.stderr
        self.stdout = StringIO()
        self.stderr = StringIO()
        sys.stdout = self.stdout
        sys.stderr = self.stderr

        # Setup some expected values which are reused in many tests
        self.expected_source_dir = self.workspace.source_dir(
            MockProduct.product_source_name())
        self.expected_script_path = os.path.join(self.expected_source_dir,
                                                 'Utilities',
                                                 'build-script-helper.py')
        self.expected_build_dir = self.workspace.build_dir(
            self.host.name, MockProduct.product_name())
        self.expected_toolchain_path = self.args.install_destdir
        if platform.system() == 'Darwin':
            self.expected_toolchain_path += '/install/prefix/path.toolchain'

    def tearDown(self):
        shutil.rmtree(self.workspace.build_root)
        shutil.rmtree(self.workspace.source_root)
        sys.stdout = self._orig_stdout
        sys.stderr = self._orig_stderr
        shell.dry_run = False
        self.workspace = None
        self.args = None

    def test_build_debug(self):
        self.args.build_variant = 'Debug'

        builder = NonTestingBuildScriptHelperBuilder(
            MockProduct, self.args, None, self.workspace, self.host)
        builder.build()

        expected_output = "+ {} build --verbose --package-path {} "\
                          "--build-path {} --configuration debug "\
                          "--toolchain {}\n".format(
                              self.expected_script_path,
                              self.expected_source_dir,
                              self.expected_build_dir,
                              self.expected_toolchain_path)

        self.assertEqual(self.stdout.getvalue(), expected_output)

    def test_build_release(self):
        self.args.build_variant = 'Release'

        builder = NonTestingBuildScriptHelperBuilder(
            MockProduct, self.args, None, self.workspace, self.host)
        builder.build()

        expected_output = "+ {} build --verbose --package-path {} "\
                          "--build-path {} --configuration release "\
                          "--toolchain {}\n".format(
                              self.expected_script_path,
                              self.expected_source_dir,
                              self.expected_build_dir,
                              self.expected_toolchain_path)

        self.assertEqual(self.stdout.getvalue(), expected_output)

    def test_build_release_with_debinfo(self):
        self.args.build_variant = 'RelWithDebInfo'

        builder = NonTestingBuildScriptHelperBuilder(
            MockProduct, self.args, None, self.workspace, self.host)
        builder.build()

        expected_output = "+ {} build --verbose --package-path {} "\
                          "--build-path {} --configuration release "\
                          "--toolchain {}\n".format(
                              self.expected_script_path,
                              self.expected_source_dir,
                              self.expected_build_dir,
                              self.expected_toolchain_path)

        self.assertEqual(self.stdout.getvalue(), expected_output)

    def test_test_with_non_testing_product(self):
        self.args.build_variant = 'Debug'

        builder = NonTestingBuildScriptHelperBuilder(
            MockProduct, self.args, None, self.workspace, self.host)
        builder.test()

        self.assertEqual(self.stdout.getvalue(), "")

    def test_test_with_testing_product(self):
        self.args.build_variant = 'Debug'

        builder = TestingBuildScriptHelperBuilder(
            MockProduct, self.args, None, self.workspace, self.host)
        builder.test()

        expected_output = "+ {} test --verbose --package-path {} "\
                          "--build-path {} --configuration debug "\
                          "--toolchain {}\n".format(
                              self.expected_script_path,
                              self.expected_source_dir,
                              self.expected_build_dir,
                              self.expected_toolchain_path)

        self.assertEqual(self.stdout.getvalue(), expected_output)
