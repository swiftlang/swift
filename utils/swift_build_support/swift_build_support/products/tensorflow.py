# swift_build_support/products/tensorflow.py --------------------*- python -*-
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


class TensorFlowSwiftAPIs(product.Product):
    @classmethod
    def product_source_name(cls):
        return "tensorflow-swift-apis"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_tensorflow_swift_apis

    def build(self, host_target):
        toolchain_path = targets.toolchain_path(self.args.install_destdir,
                                                self.args.install_prefix)
        swiftc = os.path.join(toolchain_path, 'usr', 'bin', 'swiftc')

        # FIXME: this is a workaround for CMake <3.16 which does not correctly
        # generate the build rules if you are not in the build directory.  As a
        # result, we need to create the build tree before we can use it and
        # change into it.
        #
        # NOTE: unfortunately, we do not know if the build is using Python
        # 2.7 or Python 3.2+.  In the latter, the `exist_ok` named parameter
        # would alleviate some of this issue.
        try:
            os.makedirs(self.build_dir)
        except OSError:
            pass

        with shell.pushd(self.build_dir):
            shell.call([
                self.toolchain.cmake,
                '-G', 'Ninja',
                '-D', 'BUILD_SHARED_LIBS=YES',
                '-D', 'CMAKE_INSTALL_PREFIX={}/usr'.format(
                    self.install_toolchain_path()),
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
        return False

    def test(self, host_target):
        pass

    def should_install(self, host_target):
        return self.args.install_tensorflow_swift_apis

    def install(self, host_target):
        shell.call([
            self.toolchain.cmake,
            '--build', self.build_dir,
            '--target', 'install',
        ])
