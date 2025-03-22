# swift_build_support/products/foundationtests.py -----------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2024 Apple Inc. and the Swift project authors
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
from . import llbuild
from . import llvm
from . import product
from . import swift
from . import swiftpm
from . import swiftsyntax
from . import xctest
from .. import shell


class FoundationTests(product.Product):
    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def swiftpm_unified_build_product_arena(cls):
        return 'foundationtests'

    @classmethod
    def is_ignore_install_all_product(cls):
        return True

    @classmethod
    def is_nondarwin_only_build_product(cls):
        return True

    def should_build(self, host_target):
        return False

    def should_install(self, host_target):
        return False

    def should_test(self, host_target):
        return self.args.test_foundation

    def configuration(self):
        return 'release' if self.is_release() else 'debug'

    def test(self, host_target):
        swift_exec = os.path.join(
            self.install_toolchain_path(host_target),
            'bin',
            'swift'
        )
        package_path = os.path.join(self.source_dir, '..', 'swift-corelibs-foundation')
        package_path = os.path.abspath(package_path)
        include_path = os.path.join(
            self.install_toolchain_path(host_target),
            'lib',
            'swift'
        )
        plutil_cmd = [
            swift_exec,
            'build',
            '--toolchain', self.install_toolchain_path(host_target),
            '--configuration', self.configuration(),
            '--scratch-path', self.build_dir,
            '--package-path', package_path,
            '--product', 'plutil',
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH
        ]
        if self.args.verbose_build:
            plutil_cmd.append('--verbose')

        # On amazon-linux2 the gold linker (version 1.14) crashes when linking
        # debug info for swift-foundation. Workaround this issue by building without
        # debug info. In order to re-use the build products from swiftfoundationtests
        # we need to build without debug info here as well. rdar://137760869
        if host_target.startswith('linux'):
            plutil_cmd += ['-Xswiftc', '-gnone']

        shell.call(plutil_cmd, env={
            'SWIFTCI_USE_LOCAL_DEPS': '1',
            'DISPATCH_INCLUDE_PATH': include_path
        })

        cmd = [
            swift_exec,
            'test',
            '--toolchain', self.install_toolchain_path(host_target),
            '--configuration', self.configuration(),
            '--scratch-path', self.build_dir,
            '--package-path', package_path,
            '--test-product', 'swift-corelibs-foundationPackageTests',
            '--multiroot-data-file', MULTIROOT_DATA_FILE_PATH
        ]
        if self.args.verbose_build:
            cmd.append('--verbose')

        # On amazon-linux2 the gold linker (version 1.14) crashes when linking
        # debug info for swift-foundation. Workaround this issue by building without
        # debug info. In order to re-use the build products from swiftfoundationtests
        # we need to build without debug info here as well. rdar://137760869
        if host_target.startswith('linux'):
            cmd += ['-Xswiftc', '-gnone']

        shell.call(cmd, env={
            'SWIFTCI_USE_LOCAL_DEPS': '1',
            'DISPATCH_INCLUDE_PATH': include_path
        })

    @classmethod
    def get_dependencies(cls):
        return [cmark.CMark,
                llvm.LLVM,
                libcxx.LibCXX,
                swift.Swift,
                libdispatch.LibDispatch,
                foundation.Foundation,
                xctest.XCTest,
                llbuild.LLBuild,
                swiftpm.SwiftPM,
                swiftsyntax.SwiftSyntax]
