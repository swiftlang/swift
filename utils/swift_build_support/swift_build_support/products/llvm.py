# swift_build_support/products/llvm.py --------------------------*- python -*-
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

from . import product
from .. import (cmake, shell)

import errno
import os
import os.path
import platform
import sys


class LLVM(product.Product):

    @classmethod
    def builder_class(cls):
        return LLVMBuilder

    def __init__(self, args, toolchain, source_dir, build_dir):
        product.Product.__init__(self, args, toolchain, source_dir,
                                 build_dir)

        # Add the cmake option for enabling or disabling assertions.
        self.cmake_options.define(
            'LLVM_ENABLE_ASSERTIONS:BOOL', args.llvm_assertions)

        # Add the cmake option for LLVM_TARGETS_TO_BUILD.
        self.cmake_options.define(
            'LLVM_TARGETS_TO_BUILD', args.llvm_targets_to_build)

        # Add the cmake options for vendors
        self.cmake_options += self._compiler_vendor_flags

        # Add the cmake options for compiler version information.
        self.cmake_options += self._version_flags

    @property
    def _compiler_vendor_flags(self):
        result = cmake.CMakeOptions()
        if self.args.compiler_vendor == "none":
            return result

        if self.args.compiler_vendor != "apple":
            raise RuntimeError("Unknown compiler vendor?!")

        result.define('CLANG_VENDOR', 'Apple')
        result.define('CLANG_VENDOR_UTI', 'com.apple.compilers.llvm.clang')
        # This is safe since we always provide a default.
        result.define('PACKAGE_VERSION', self.args.clang_user_visible_version)

        return result

    @property
    def _version_flags(self):
        result = cmake.CMakeOptions()
        if self.args.clang_compiler_version is not None:
            result.define('CLANG_REPOSITORY_STRING', "clang-{}".format(
                self.args.clang_compiler_version
            ))
        return result


class LLVMBase(product.CMakeProductBuilder):
    @property
    def _is_llvm_lto_enabled(self):
        return self._args.lto_type in ['thin', 'full']

    @property
    def _is_swift_lto_enabled(self):
        # TODO: build-script-impl has different arguments for LLVM and Swift,
        # but build-script only has --lto-type
        return self._args.lto_type in ['thin', 'full']

    @property
    def _cmake_needs_to_specify_standard_computed_defaults(self):
        cmake_version_output = shell.capture(['cmake', '--version'])
        version_lines = [line
                         for line in cmake_version_output.splitlines()
                         if "cmake version" in line]
        if len(version_lines) > 0:
            version_line_fields = version_lines[0].split(" ")
            if len(version_line_fields) >= 3:
                version = version_line_fields[2]
                if version == "3.4.0":
                    return True
        return False

    @property
    def _llvm_cmake_options(self):
        options = cmake.CMakeOptions()

        if self._host.platform.is_darwin:
            cmake_osx_deployment_target = ""
            if self._host.name == 'macosx-x86_64':
                cmake_osx_deployment_target = \
                    self._args.darwin_deployment_version_osx
            options.define(
                'CMAKE_OSX_DEPLOYMENT_TARGET:STRING',
                cmake_osx_deployment_target)
            options.define(
                'CMAKE_OSX_SYSROOT:PATH',
                shell.capture([
                    'xcrun', '--sdk', self._host.platform.name,
                    '--show-sdk-path']))
            options.define('COMPILER_RT_ENABLE_IOS:BOOL', False)
            options.define('COMPILER_RT_ENABLE_WATCHOS:BOOL', False)
            options.define('COMPILER_RT_ENABLE_TVOS:BOOL', False)
            options.define(
                'SANITIZER_MIN_OSX_VERSION', cmake_osx_deployment_target)
            options.define(
                'LLVM_ENABLE_MODULES:BOOL', self._args.llvm_enable_modules)

            if self._is_llvm_lto_enabled:
                if self._cmake_needs_to_specify_standard_computed_defaults:
                    options.define(
                        'CMAKE_C_STANDARD_COMPUTED_DEFAULT', 'AppleClang')
                    options.define(
                        'CMAKE_CXX_STANDARD_COMPUTED_DEFAULT', 'AppleClang')

                options.define(
                    'LLVM_PARALLEL_LINK_JOBS',
                    min(self._args.llvm_max_parallel_lto_link_jobs,
                        self._args.build_jobs))

            if self._is_swift_lto_enabled:
                options.define('LLVM_ENABLE_MODULE_DEBUGGING:BOOL', False)

        # NOTE: compute_cmake_llvm_tool_disable_flags is commented out in
        # build-script-impl

        options.define('LLVM_TOOL_COMPILER_RT_BUILD:BOOL',
                       self._args.build_compiler_rt)
        options.define('LLVM_BUILD_EXTERNAL_COMPILER_RT:BOOL',
                       self._args.build_compiler_rt)

        llvm_target_archs = {
            'linux-armv6': 'ARM',
            'linux-armv7': 'ARM',
            'iphoneos-armv7': 'ARM',
            'iphoneos-armv7s': 'ARM',
            'watchos-armv7k': 'ARM',
            'iphoneos-arm64': 'AArch64',
            'appletvos-arm64': 'AArch64',
            'iphonesimulator-i386': 'X86',
            'iphonesimulator-x86_64': 'X86',
            'appletvsimulator-x86_64': 'X86',
            'watchsimulator-i386': 'X86',
        }

        if self._host.name in llvm_target_archs:
            options.define(
                'LLVM_TARGET_ARCH', llvm_target_archs[self._host.name])

        host_triple = self._swift_host_triple
        if host_triple:
            options.define('LLVM_HOST_TRIPLE:STRING', host_triple)

        if self._args.lit_args:
            options.define('LLVM_LIT_ARGS', self._args.lit_args)

        if self._args.clang_profile_instr_use:
            options.define(
                'CLANG_PROFDATA_FILE', self._args.clang_profile_instr_use)

        options.define('CMAKE_INSTALL_PREFIX:PATH', self._host_install_prefix)
        options.define('INTERNAL_INSTALL_PREFIX', 'local')

        return options


class LLVMBuilder(LLVMBase):
    def __init__(self, product_class, args, toolchain, workspace, host):
        product.CMakeProductBuilder.__init__(self, product_class, args,
                                             toolchain, workspace, host)

        self._cmake_options.define('CMAKE_LIBTOOL', self._toolchain.libtool)
        self._cmake_options.define(
            'CMAKE_C_FLAGS_RELWITHDEBINFO', self.__relWithDebInfoCFlags)
        self._cmake_options.define(
            'CMAKE_CXX_FLAGS_RELWITHDEBINFO', self.__relWithDebInfoCFlags)
        self._cmake_options.define(
            'CMAKE_BUILD_TYPE:STRING', self._build_variant)
        self._cmake_options.define('LLVM_TOOL_SWIFT_BUILD:BOOL', 'NO')
        self._cmake_options.define('LLVM_INCLUDE_DOCS:BOOL', True)
        self._cmake_options.define('LLVM_ENABLE_LTO', self._args.lto_type)

        clang_tools_extra_source_dir = os.path.join(
            self._source_dir, os.path.pardir, 'clang-tools-extra')
        if os.path.exists(clang_tools_extra_source_dir):
            self._cmake_options.define(
                'LLVM_EXTERNAL_CLANG_TOOLS_EXTRA_SOURCE_DIR',
                clang_tools_extra_source_dir)

        if self._args.build_toolchain_only:
            self._cmake_options.define('LLVM_BUILD_TOOLS:BOOL', False)
            self._cmake_options.define(
                'LLVM_INSTALL_TOOLCHAIN_ONLY:BOOL', True)
            self._cmake_options.define('LLVM_INCLUDE_TESTS:BOOL', False)
            self._cmake_options.define('CLANG_INCLUDE_TESTS:BOOL', False)
            self._cmake_options.define('LLVM_INCLUDE_UTILS:BOOL', False)
            self._cmake_options.define('LLVM_TOOL_LLI_BUILD:BOOL', False)
            self._cmake_options.define('LLVM_TOOL_LLVM_AR_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_CLANG_CHECK_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_ARCMT_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_C_ARCMT_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_C_INDEX_TEST_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_DRIVER_BUILD:BOOL',
                not self._args.build_runtime_with_host_compiler)
            self._cmake_options.define('CLANG_TOOL_DIAGTOOL_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_SCAN_BUILD_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_SCAN_VIEW_BUILD:BOOL', False)
            self._cmake_options.define(
                'CLANG_TOOL_CLANG_FORMAT_BUILD:BOOL', False)

        if not self._args.llvm_include_tests:
            self._cmake_options.define('LLVM_INCLUDE_TESTS:BOOL', False)
            self._cmake_options.define('CLANG_INCLUDE_TESTS:BOOL', False)

        self._cmake_options.define('CMAKE_C_FLAGS', self.__cflags)
        self._cmake_options.define('CMAKE_CXX_FLAGS', self.__cflags)

        if self._is_cross_tools_host:
            host_target_llvm_build_dir = self._workspace.build_dir(
                self._args.host_target, LLVM.product_name())
            self._cmake_options.define(
                'LLVM_TABLEGEN',
                os.path.join(host_target_llvm_build_dir, 'bin', 'llvm-tblgen'))
            self._cmake_options.define(
                'CLANG_TABLEGEN',
                os.path.join(
                    host_target_llvm_build_dir, 'bin', 'clang-tblgen'))
            self._cmake_options.define(
                'LLVM_NATIVE_BUILD', host_target_llvm_build_dir)

        if self._host.platform.name == 'windows':
            self._cmake_options.define('LLVM_ENABLE_PROJECTS', 'clang')
            self._cmake_options.define(
                'LLVM_DEFAULT_TARGET_TRIPLE',
                '{}-unknown-windows-msvc'.format(self._host.arch))
            if self._is_debinfo_build_variant:
                self._cmake_options.define('LLVM_ENABLE_PDB:BOOL', True)

        self._cmake_options += self._llvm_cmake_options

    def do_build(self):
        cmake_invocation = cmake.CMake(self._args, self._toolchain)
        cmake_invocation.generate_if_needed(self._source_dir,
                                            self._build_dir,
                                            self._cmake_options)

        # When we are building LLVM create symlinks to the c++ headers. We need
        # to do this before building LLVM since compiler-rt depends on being
        # built with the just built clang compiler. These are normally put into
        # place during the cmake step of LLVM's build when libcxx is in
        # tree... but we are not building llvm with libcxx in tree when we
        # build swift. So we need to do configure's work here.
        host_cxx_headers_dir = None
        if platform.system() == 'Darwin':
            host_cxx_dir = os.path.dirname(self._toolchain.cxx)
            host_cxx_headers_dir = os.path.join(
                host_cxx_dir, '..', '..', 'usr', 'include', 'c++')
        elif platform.system() == 'Haiku':
            host_cxx_headers_dir = '/boot/system/develop/headers/c++'
        elif platform.system() == 'Windows':
            pass  # Windows does not need this workaround
        else:  # Linux
            host_cxx_headers_dir = '/usr/include/c++'

        if host_cxx_headers_dir:
            # Find the path in which the local clang build is expecting to find
            # the c++ header files.
            built_cxx_include_dir = os.path.join(
                self._build_dir, 'include', 'c++')

            print(
                "symlinking the system headers ({}) into the local clang "
                "build directory ({})".format(
                    host_cxx_headers_dir, built_cxx_include_dir))
            # This should be equivalent to ln -s -f
            try:
                os.symlink(host_cxx_headers_dir, built_cxx_include_dir)
            except OSError as e:
                if e.errno == errno.EEXIST:
                    os.remove(os.path.join(built_cxx_include_dir))
                    os.symlink(host_cxx_headers_dir, built_cxx_include_dir)
                else:
                    raise e

        if self._should_build:
            cmake_invocation.build_targets(self._build_dir,
                                           self._build_variant,
                                           self._build_targets)
            # In Windows, add LLVM bin to the path
            if sys.platform.startswith('win32'):
                os.environ['PATH'] += \
                    os.path.pathsep + os.path.join(self._build_dir, 'bin')

    @property
    def _build_targets(self):
        build_targets = ['all']
        if self._args.clean_llvm:
            build_targets = ['clean']
        if not self._should_build:
            build_targets = [
                'llvm-tblgen',
                'clang-headers',
                'intrinsics_gen',
                'clang-tablegen-targets'
            ]
        return build_targets

    @property
    def _build_variant(self):
        return self._args.llvm_build_variant

    @property
    def _should_build(self):
        return self._args.build_llvm

    @property
    def _should_test(self):
        return False  # We don't test LLVM

    @property
    def __cflags(self):
        cflags = self._common_cross_cflags

        # In Windows, we build LLVM using cl.exe initially, so we might need to
        # use different CFLAGS.
        is_msvc = os.path.basename(self._toolchain.cc) == 'cl.exe'
        is_clang_cl = os.path.basename(self._toolchain.cc) == 'clang-cl.exe'

        if self._is_release_build_variant:
            if is_msvc or is_clang_cl:
                cflags += ' /GS-'
            else:
                cflags += ' -fno-stack-protector'
        if self._is_debinfo_build_variant:
            if self._is_llvm_lto_enabled:
                cflags += ' -gline-tables-only' if not is_msvc else ' /Zd'
            else:
                cflags += ' -g' if not is_msvc else ' /Zi'
        return cflags

    @property
    def __relWithDebInfoCFlags(self):
        # In Windows, we build LLVM using cl.exe initially, so we might need to
        # use different CFLAGS.
        is_msvc = os.path.basename(self._toolchain.cc) == 'cl.exe'

        return "-O2 -DNDEBUG" if not is_msvc else "/O2 /DNDEBUG"
