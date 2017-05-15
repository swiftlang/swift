# build_script/products/ninja.py --------------------------------*- python -*-
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
"""
Ninja builder
"""
# ----------------------------------------------------------------------------

import os.path

from ..host import host
from ..exceptions import BuildError
from .. import shell
from ..utils import printf


class Ninja(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('ninja')
        if cls.source_dir is None:
            raise BuildError("Can't find Ninja source directory")

    def __init__(self, deployment_target, target_build_dir, args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.args = args

    @property
    def ninja_bin_path(self):
        return os.path.join(self.build_dir, 'ninja')

    def configure(self):
        if not self.args.reconfigure and os.path.exists(self.ninja_bin_path):
            # If we already have a built ninja, we don't need to recompile it.
            return

        printf('--- Configuring Ninja ---')
        # Ninja can only be built in-tree.
        # Copy the source tree to the build directory.
        if os.path.exists(self.build_dir):
            shell.rmtree(self.build_dir)

        shell.copytree(self.source_dir, self.build_dir, symlinks=True)

    def build(self):

        env = {}
        if host.is_darwin():
            macosx_min = ('-mmacosx-version-min=%s' %
                          self.args.darwin_deployment_version_osx)
            cflags = [
                '-isysroot', host.sdk_path('macosx'), macosx_min]
            env = [
                ('CXX', host.find_clang_cxx()),
                ('CFLAGS', ' '.join(cflags)),
                ('LDFLAGS', macosx_min)]

        printf('--- Building Ninja ---')
        with shell.pushd(self.build_dir):
            shell.invoke(['python', './configure.py', '--bootstrap'],
                         env=env)

    def test(self):
        pass  # We don't test Ninja

    def install(self):
        pass  # We don't install Ninja
