# swift_build_support/products/pythonkit.py ---------------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os

from . import product
from .. import shell
from .. import targets


class PythonKit(product.Product):
    @classmethod
    def product_source_name(cls):
        return "PythonKit"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return True

    def build(self, host_target):
        toolchain_path = targets.toolchain_path(self.args.install_destdir,
                                                self.args.install_prefix)
        swiftc = os.path.join(toolchain_path, 'usr', 'bin', 'swift')

        shell.call([
            self.toolchain.cmake,
            '-G', 'Ninja',
            '-D', 'BUILD_SHARED_LIBS=YES',
            '-D', 'CMAKE_INSTALL_PREFIX={}/usr'.format(
                self.args.install_destdir),
            '-D', 'CMAKE_MAKE_PROGRAM={}'.format(self.toolchain.ninja),
            '-D', 'CMAKE_Swift_COMPILER={}'.format(swiftc),
            '-B', self.build_dir,
            '-S', self.source_dir,
        ])
        shell.call([
            self.toolchain.cmake,
            '--build', self.build_dir,
        ])

    def should_test(self, host_target):
        return self.args.test_pythonkit

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return self.args.install_pythonkit

    def install(self, host_target):
        shell.call([
            self.toolchain.cmake,
            '--build', self.build_dir,
            '--target', 'install',
        ])
