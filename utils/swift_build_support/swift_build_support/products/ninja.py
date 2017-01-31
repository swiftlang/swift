# swift_build_support/products/ninja.py -------------------------*- python -*-
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
"""
Ninja build
"""
# ----------------------------------------------------------------------------

import os.path
import platform
import sys

from . import product
from .. import cache_util
from .. import shell


class Ninja(product.Product):
    @cache_util.reify
    def ninja_bin_path(self):
        return os.path.join(self.build_dir, 'ninja')

    def do_build(self):
        if os.path.exists(self.ninja_bin_path):
            return

        env = None
        if platform.system() == "Darwin":
            from .. import xcrun
            sysroot = xcrun.sdk_path("macosx")
            osx_version_min = self.args.darwin_deployment_version_osx
            assert sysroot is not None
            env = {
                "CXX": self.toolchain.cxx,
                "CFLAGS": (
                    "-isysroot {sysroot} -mmacosx-version-min={osx_version}"
                ).format(sysroot=sysroot, osx_version=osx_version_min),
                "LDFLAGS": (
                    "-mmacosx-version-min={osx_version}"
                ).format(osx_version=osx_version_min),
            }
        elif self.toolchain.cxx:
            env = {
                "CXX": self.toolchain.cxx,
            }

        # Ninja can only be built in-tree.  Copy the source tree to the build
        # directory.
        shell.rmtree(self.build_dir)
        shell.copytree(self.source_dir, self.build_dir)
        with shell.pushd(self.build_dir):
            shell.call([sys.executable, 'configure.py', '--bootstrap'],
                       env=env)
