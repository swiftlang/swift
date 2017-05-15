# build_script/products/libdispatch .py -------------------------*- python -*-
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
libdispatch builder
"""
# ----------------------------------------------------------------------------

import os

from .. import shell
from ..utils import printf
from ..exceptions import BuildError


class Libdispatch(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swift-corelibs-libdispatch')
        if cls.source_dir is None:
            raise BuildError("Couldn't find libdipatch source directory.")

        if not os.path.exists(os.path.join(cls.source_dir, 'configure')):
            # This is first time to build
            with shell.pushd(cls.source_dir):
                shell.invoke(['autoreconf', '-fvi'])

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 swift_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir

        self.swift_build = swift_build

        self.args = args

    def is_configured(self):
        return os.path.exists(os.path.join(self.build_dir, 'config.status'))

    def configure(self):
        if not self.args.reconfigure and self.is_configured():
            # Already configured.
            return

        printf("--- Configuring libdispatch ---")
        # Do configre
        configure_command = [os.path.join(self.source_dir, 'configure')]
        configure_command += (
            '--prefix=' + self.install_destdir,
            '--with-swift-toolchain=' + self.swift_build.build_dir,
        )
        # Prepare build directory
        if not os.path.exists(self.build_dir):
            shell.makedirs(self.build_dir)
        # Do configure
        with shell.pushd(self.build_dir):
            shell.invoke(configure_command)

    def build(self):
        printf("--- Building libdispatch ---")
        with shell.pushd(self.build_dir):
            shell.invoke(['make'])
            shell.chdir('tests')
            shell.invoke(['make', 'build-tests'])

    def test(self):
        if self.deployment_target != self.args.host_target:
            return
        if self.args.skip_test_libdispatch:
            return
        printf("--- Running tests for libdispatch ---")
        with shell.pushd(self.build_dir):
            shell.invoke(["make", "test"])

    def install(self):
        if not self.args.install_libdispatch:
            return

        printf("--- Installing libdispatch ---")
        with shell.pushd(self.build_dir):
            shell.invoke(["make", "install"])
