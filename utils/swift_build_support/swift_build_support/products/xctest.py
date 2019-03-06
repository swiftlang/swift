# swift_build_support/products/xctest.py -------------------------*- python -*-
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

from . import (foundation, libdispatch, llvm, product, swift)
from .. import shell

import os.path
import sys


class XCTest(product.Product):
    @classmethod
    def product_source_name(cls):
        """product_source_name() -> str

        The name of the source code directory of this product.
        """
        return "swift-corelibs-xctest"

    @classmethod
    def builder_class(cls):
        def chooser(product_class, args, toolchain, workspace, host):
            if host.platform.name == 'macosx':
                return XCTestScriptBuilder(
                    product_class, args, toolchain, workspace, host)
            else:
                return XCTestCMakeBuilder(
                    product_class, args, toolchain, workspace, host)
        return chooser


class XCTestScriptBuilder(object):
    def __init__(self, product_class, args, toolchain, workspace, host):
        self.__source_dir = workspace.source_dir(
            product_class.product_source_name())
        self.__build_dir = workspace.build_dir(
            host.name, product_class.product_name())
        self.__workspace = workspace

        self.__build_script_path = os.path.join(
            self.__source_dir, 'build_script.py')

        swiftc_bin = os.path.join(
            workspace.build_dir(args.host_target, swift.Swift.product_name()),
            'bin', 'swiftc')

        foundation_build_dir = workspace.build_dir(
            args.host_target, foundation.Foundation.product_name())

        swift_build_dir = workspace.build_dir(host, swift.Swift.product_name())

        self.__build_script_arguments = [
            '--swiftc={}'.format(swiftc_bin),
            '--build-dir={}'.format(self.__build_dir),
            '--foundation-build-dir={}'.format(foundation_build_dir),
            '--swift-build-dir={}'.format(swift_build_dir)]

        if args.build_libdispatch:
            dispatch_source_dir = workspace.source_dir(
                libdispatch.LibDispatch.product_source_name())
            dispatch_build_dir = workspace.build_dir(
                host.name, libdispatch.LibDispatch.product_name())
            self.__build_script_arguments += [
                '--libdispatch-src-dir={}'.format(dispatch_source_dir),
                '--libdispatch-build-dir={}'.format(dispatch_build_dir)]

        if args.xctest_build_variant == "Debug":
            self.__build_script_arguments += ['--debug']
        else:
            self.__build_script_arguments += ['--release']

    def do_build(self):
        shell.call([self.__build_script_path] + self.__build_script_arguments)

    def do_test(self):
        lit_path = os.path.join(
            self.__workspace.source_dir(llvm.LLVM.product_source_name()),
            'utils', 'lit', 'lit.py')

        print("--- Running tests for xctest ---")
        shell.call([
            self.__build_script_path, 'test'] +
            self.__build_script_arguments +
            ['--lit={}'.format(lit_path),
             self.__build_dir])
        print("--- Finished tests for xctest ---")

    def do_install(self):
        # TODO
        pass


class XCTestCMakeBuilder(product.CMakeProductBuilder):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(
            self, product_class, args, toolchain, workspace, host)

        llvm_bin = self._workspace.build_dir(
            self._args.host_target, llvm.LLVM.product_name())
        if not sys.platform.startswith('win32'):
            cc = os.path.join(llvm_bin, 'clang')
            cxx = os.path.join(llvm_bin, 'clang++')
        else:
            cc = os.path.join(llvm_bin, 'clang-cl.exe')
            cxx = os.path.join(llvm_bin, 'clang-cl.exe')

        swiftc_bin = os.path.join(
            self._workspace.build_dir(
                self._args.host_target, swift.Swift.product_name()),
            'bin', 'swiftc')

        dispatch_source_dir = self._workspace.source_dir(
            libdispatch.LibDispatch.product_source_name())
        dispatch_build_dir = self._workspace.build_dir(
            self._host.name, libdispatch.LibDispatch.product_name())

        foundation_build_dir = self._workspace.build_dir(
            self._args.host_target, foundation.Foundation.product_name())

        llvm_source_dir = self._workspace.source_dir(
            llvm.LLVM.product_source_name())
        llvm_build_dir = self._workspace.build_dir(
            self._host, llvm.LLVM.product_name())

        # TODO: build-script-impl defines --xctest-cmake-options but nobody
        # uses it.
        self._cmake_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._build_variant)
        self._cmake_options.define('CMAKE_C_COMPILER:PATH', cc)
        self._cmake_options.define('CMAKE_CXX_COMPILER:PATH', cxx)
        self._cmake_options.define('CMAKE_SWIFT_COMPILER:PATH', swiftc_bin)
        self._cmake_options.define(
            'CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)
        self._cmake_options.define('CMAKE_INSTALL_LIBDIR:PATH', "lib")
        self._cmake_options.define(
            'XCTEST_PATH_TO_LIBDISPATCH_SOURCE:PATH', dispatch_source_dir)
        self._cmake_options.define(
            'XCTEST_PATH_TO_LIBDISPATCH_BUILD:PATH', dispatch_build_dir)
        self._cmake_options.define(
            'XCTEST_PATH_TO_FOUNDATION_BUILD:PATH', foundation_build_dir)
        self._cmake_options.define(
            'XCTEST_PATH_TO_COREFOUNDATION_BUILD:PATH',
            os.path.join(foundation_build_dir, 'CoreFoundation-prefix'))
        self._cmake_options.define('CMAKE_PREFIX_PATH:PATH', llvm_build_dir)
        self._cmake_options.define('ENABLE_TESTING:BOOL', True)

        if self._host.platform.name == "windows":
            self._cmake_options.define('BUILD_SHARED_LIBS:BOOL', True)
            self._cmake_options.define(
                'LIT_COMMAND:PATH', os.path.join(
                    llvm_source_dir, 'utils', 'lit', 'lit.py'))

    @property
    def _build_variant(self):
        return self._args.xctest_build_variant

    @property
    def _should_build(self):
        return self._args.build_xctest

    @property
    def _should_test(self):
        # TODO: build-script-impl defines --skip-test-xctest
        return self._args.test

    @property
    def _test_executable_target(self):
        return None

    @property
    def _test_results_targets(self):
        return ['check-xctest']
