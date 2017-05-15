# build_script/products/xctest.py -------------------------------*- python -*-
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
XCTest builder
"""
# ----------------------------------------------------------------------------

from __future__ import absolute_import

import os.path

from .. import targets
from .. import shell
from .. import swift_utils
from ..host import host
from ..exceptions import BuildError
from ..xcodebuild import Xcodebuild, XcodebuildOptions
from ..utils import printf


class XCTest(object):

    source_dir = None

    @classmethod
    def prepare(cls, workspace):
        cls.source_dir = workspace.subdir('swift-corelibs-xctest')
        if cls.source_dir is None:
            raise BuildError("Couldn't find XCTest source directory.")

    def __init__(self,
                 deployment_target,
                 target_build_dir,
                 target_install_destdir,
                 swift_build,
                 foundation_build,
                 args):
        self.deployment_target = deployment_target
        self.build_dir = target_build_dir
        self.install_destdir = target_install_destdir

        self.swift_build = swift_build
        self.foundation_build = foundation_build

        self.args = args

    def configure(self):
        pass  # XCTest has no configuration

    def build(self):
        printf("--- Building XCTest ---")

        if host.is_darwin():
            xcodebuild_options = XcodebuildOptions()
            define = xcodebuild_options.define
            define('SWIFT_EXEC', self.swift_build.swiftc_bin_path)
            define('SWIFT_LINK_OBJC_RUNTIME', 'YES')
            define('SYMROOT', self.build_dir)
            define('OBJROOT', self.build_dir)

            # xcodebuild requires swift-stdlib-tool to build a Swift
            # framework. This is normally present when building XCTest
            # via a packaged .xctoolchain, but here we are using the
            # swiftc that was just built--no toolchain exists yet. As a
            # result, we must copy swift-stdlib-tool ourselves.
            swift_stdlib_tool_path = os.path.join(
                self.swift_build.bin_dir, 'swift-stdlib-tool')
            if not os.path.exists(swift_stdlib_tool_path):
                # FIXME: What if substitute source has changed?
                shell.copy(
                    swift_utils('swift-stdlib-tool-substitute'),
                    swift_stdlib_tool_path)

            xcodebuild = Xcodebuild()
            xcodebuild.build_workspace(
                workspace=os.path.join(self.source_dir, 'XCTest.xcworkspace'),
                scheme="SwiftXCTest",
                options=xcodebuild_options)
        else:
            foundation_path = self.foundation_build.foundation_path
            command = [os.path.join(self.source_dir, 'build_script.py')]
            command += [
                '--swiftc=' + self.swift_build.swiftc_bin_path,
                '--build-dir=' + self.build_dir,
                '--foundation-build-dir=' + foundation_path]
            shell.invoke(command)

    def test(self):
        if self.deployment_target != self.args.host_target:
            return
        if self.args.skip_test_xctest:
            return

        printf("--- Running tests for XCTest ---")
        if host.is_darwin():
            xcodebuild_options = XcodebuildOptions()
            define = xcodebuild_options.define
            define('SWIFT_EXEC', self.swift_build.swiftc_bin_path)
            define('SWIFT_LINK_OBJC_RUNTIME', 'YES')
            define('SYMROOT', self.build_dir)
            define('OBJROOT', self.build_dir)

            xcodebuild = Xcodebuild()
            xcodebuild.build_workspace(
                workspace=os.path.join(self.source_dir, 'XCTest.xcworkspace'),
                scheme="SwiftXCTestFunctionalTests",
                options=xcodebuild_options)
        else:
            foundation_path = self.foundation_build.foundation_path
            command = [os.path.join(self.source_dir, 'build_script.py'),
                       "test"]
            command += [
                '--swiftc=' + self.swift_build.swiftc_bin_path,
                '--foundation-build-dir=' + foundation_path,
                self.build_dir]
            shell.invoke(command)

    def install(self):
        if not self.args.install_xctest:
            return

        printf("--- Installing XCTest ---")
        if host.is_linux():
            lib_target = "linux"
        elif host.is_freebsd():
            lib_target = "freebsd"
        elif host.is_cygwin():
            lib_target = "windows"
        else:
            raise BuildError(
                "--install-xctest is not supported on this platform")

        install_prefix = os.path.join(
            # FIXME: We should use self.install_destdir instead.
            self.args.install_destdir,
            # strip leading '/' because join('/foo', '/bar') results '/bar'
            self.args.install_prefix.lstrip('/'),
            'lib', 'swift', lib_target)

        module_install_prefix = os.path.join(
            install_prefix,
            targets.split(self.deployment_target)[1])

        command = [os.path.join(self.source_dir, 'build_script.py'),
                   "install"]
        command += [
            '--library-install-path=' + install_prefix,
            '--module-install-path=' + module_install_prefix,
            self.build_dir]

        shell.invoke(command)
