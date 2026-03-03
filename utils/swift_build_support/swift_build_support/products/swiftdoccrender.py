# swift_build_support/products/swiftdoccrender.py ---------------*- python -*-
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

from . import product
from . import swiftdocc
from .. import shell


class SwiftDocCRender(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-docc-render-artifact"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    @classmethod
    def is_swiftpm_unified_build_product(cls):
        return False

    def should_build(self, host_target):
        # Swift-DocC-Render is a pre-built, installable artifact.
        return False

    def should_test(self, host_target):
        # Swift-DocC-Render is a pre-built, installable artifact.
        return False

    def should_install(self, host_target):
        # Swift-DocC-Render should always be installed if Swift-DocC is being installed
        return self.args.install_swiftdocc or (
            self.args.install_all and self.args.build_swiftdocc
        )

    def install(self, host_target):
        # Swift-DocC-Render is installed at '/usr/share/docc/render' in the built
        # toolchain.
        install_toolchain_path = self.install_toolchain_path(host_target)
        install_path = os.path.join(install_toolchain_path, 'share', 'docc', 'render')

        # The pre-built version of Swift-DocC-Render is distributed in the 'dist'
        # folder at the root of the swift-docc-render-artifact repository
        artifact_dist_path = os.path.join(self.source_dir, 'dist')

        # Add a trailing slash so that we copy the contents of the 'dist' directory
        # instead of the 'dist' directory itself.
        artifact_dist_path_with_trailing_slash = os.path.join(artifact_dist_path, '')

        shell.call(["mkdir", "-p", install_path])
        shell.call(
            ["rsync", "-a", artifact_dist_path_with_trailing_slash, install_path])

    @classmethod
    def get_dependencies(cls):
        return [swiftdocc.SwiftDocC]
