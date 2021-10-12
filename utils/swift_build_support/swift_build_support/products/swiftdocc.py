# swift_build_support/products/swiftdocc.py ---------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from build_swift.build_swift.constants import MULTIROOT_DATA_FILE_PATH

from . import cmark
from . import foundation
from . import libcxx
from . import libdispatch
from . import libicu
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import swiftpm
from . import swiftsyntax
from . import xctest
from .. import shell


class SwiftDocC(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-docc"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return True

    def run_build_script_helper(self, action, host_target, additional_params=[]):
        script_path = os.path.join(
            self.source_dir, 'build-script-helper.py')

        configuration = 'release' if self.is_release() else 'debug'

        helper_cmd = [
            script_path,
            action,
            '--toolchain', self.install_toolchain_path(host_target),
            '--configuration', configuration,
            '--build-dir', self.build_dir,
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH,
        ]
        if action != 'install':
            helper_cmd.extend([
                # There might have been a Package.resolved created by other builds
                # or by the package being opened using Xcode. Discard that and
                # reset the dependencies to be local.
                '--update'
            ])
        if self.args.verbose_build:
            helper_cmd.append('--verbose')
        helper_cmd.extend(additional_params)

        shell.call(helper_cmd)

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        self.run_build_script_helper('build', host_target)

    def should_test(self, host_target):
        return self.args.test_swiftdocc

    def test(self, host_target):
        self.run_build_script_helper('test', host_target)

    def should_install(self, host_target):
        return self.args.install_swiftdocc

    def install(self, host_target):
        # swift-docc is installed at '/usr/bin/docc' in the built toolchain.
        install_toolchain_path = self.install_toolchain_path(host_target)
        install_dir = os.path.join(install_toolchain_path, 'bin')

        additional_params = ['--install-dir', install_dir]

        # PLEASE READ:
        # The code below does something different than other build-script products do.
        #
        # Installing swift-docc without also building swift-docc-render will find the
        # already built swift-docc-render template in the host toolchain and copy it
        # over to the install toolchain.
        # This allows developers with local swift-docc changes but no swift-docc-render
        # changes to build and run their swift-docc changes without requiring 'node'
        # which is required to build and install swift-docc-render from source.
        # (Displaying the output from swift-docc  with built swift-docc-render template
        # doesn't require 'node' or other tools.)
        if not self.args.build_swiftdoccrender:
            # swift-docc and swift-docc-render are expected to come in pairs.
            # When building swift-docc without building swift-docc-render,
            # attempt to copy the prebuilt swift-docc-render from the host toolchain.
            docc_path = self.toolchain.find_tool("docc")
            if docc_path is None:
                warn_msg = 'Host toolchain could not locate an prebuilt'\
                           'swift-docc-render to copy.'\
                           '(Use `--swiftdoccrender` to build swift-docc-render)'
                print('-- Warning: {}', warn_msg)
            else:
                additional_params.append('--copy-doccrender-from')
                # Drop two path components from the docc path to get to the `/usr/` dir.
                host_toolchain_path = os.path.dirname(os.path.dirname(docc_path))
                # Get the location of the swift-docc-render in the host toolchain.
                built_render_dir = self.get_render_install_destdir(
                    host_toolchain_path)
                additional_params.append(built_render_dir)

                additional_params.append('--copy-doccrender-to')
                # Get the install location of the swift-docc-render in the install
                # toolchain.
                render_install_dir = self.get_render_install_destdir(
                    install_toolchain_path)
                additional_params.append(render_install_dir)
                note_msg = 'Copying already built swift-docc-render output '\
                           'from %s to %s.' % (built_render_dir, render_install_dir)
                print('-- Note: {}', note_msg)

        self.run_build_script_helper('install', host_target, additional_params)

    # This is defined on swift-docc to avoid a circular import.
    @classmethod
    def get_render_install_destdir(cls, toolchain_install_dir):
        # swift-docc-render is installed at '/usr/share/docc/render' in the built
        # toolchain.
        return os.path.join(toolchain_install_dir, 'share', 'docc', 'render')

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                libicu.LibICU,
                swift.Swift,
                libdispatch.LibDispatch,
                foundation.Foundation,
                xctest.XCTest,
                llbuild.LLBuild,
                swiftpm.SwiftPM,
                swiftsyntax.SwiftSyntax]
