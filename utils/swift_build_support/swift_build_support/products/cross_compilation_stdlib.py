# swift_build_support/products/cross_compilation_stdlib.py -------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2025 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

import os
import shutil

from . import llvm
from . import product
from . import swift
from .cmake_product import CMakeProduct
from .. import cmake
from .. import shell


class CrossCompilationStdlib(product.Product):
    @classmethod
    def product_source_name(cls):
        return "swift"

    @classmethod
    def is_build_script_impl_product(cls):
        return False

    @classmethod
    def is_before_build_script_impl_product(cls):
        return False

    def should_build(self, host_target):
        return self.args.build_cross_compilation_stdlib

    def should_test(self, host_target):
        return False

    def _build_dir(self, target_name, product):
        return os.path.join(
            self.build_dir, f'build-{target_name}', product
        )

    def build(self, host_target):
        if not self.args.cross_compilation_target:
            raise ValueError("Cross-compilation target must be specified with"
                             " --cross-compilation-target")

        target_name = self.args.cross_compilation_target
        target_triple = self._get_target_triple(target_name)
        self._build(host_target, target_name, target_triple)

    def _get_sysroot(self):
        if not self.args.cross_compilation_sysroot:
            raise ValueError("Cross-compilation sysroot must be specified with"
                             " --cross-compilation-sysroot")
        return self.args.cross_compilation_sysroot

    def _build(self, host_target, target_name, target_triple):
        llvm_build_dir = self._configure_llvm(target_name)
        llvm_cmake_dir = os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm')
        sysroot = self._get_sysroot()

        # Stdlib
        self._build_stdlib(
            host_target, target_name, target_triple, llvm_cmake_dir, sysroot
        )

        # CMake toolchain file
        toolchain_file = self._create_toolchain_file(
            target_name, target_triple, sysroot
        )

        # Dispatch (+ static, if enabled)
        self._build_dispatch(target_name, target_triple, toolchain_file)
        if self.args.build_swift_static_stdlib:
            self._build_dispatch(target_name, target_triple, toolchain_file,
                                 static=True)

        # Foundation (+ static, if enabled)
        self._build_foundation(target_name, target_triple, toolchain_file)
        if self.args.build_swift_static_stdlib:
            self._build_foundation(target_name, target_triple, toolchain_file,
                                   static=True)

        # Swift Testing, XCTest
        self._build_swift_testing(target_name, target_triple, toolchain_file)
        self._build_xctest(target_name, target_triple, toolchain_file)

    def _configure_llvm(self, target_name):
        build_dir = self._build_dir(target_name, 'llvm')
        llvm_source_dir = os.path.join(
            os.path.dirname(self.source_dir), 'llvm-project', 'llvm')
        cmake_options = cmake.CMakeOptions()
        cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
        cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')
        if self.args.build_runtime_with_host_compiler:
            cmake_options.define('CMAKE_ASM_COMPILER:PATH', self.toolchain.cc)

        llvm_cmake = cmake.CMake(
            self.args, self.toolchain,
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler
        )
        # Only configure LLVM, not build it because we just need
        # LLVM CMake functionalities
        shell.call(["env", self.toolchain.cmake, "-B", build_dir]
                   + list(llvm_cmake.common_options(self))
                   + list(cmake_options)
                   + [llvm_source_dir])
        return build_dir

    def _target_c_flags(self):
        # lld is required to link for cross-compilation
        flags = '-w -fuse-ld=lld'

        if self.args.cross_compilation_target_flags:
            flags += f' {self.args.cross_compilation_target_flags}'

        return flags

    def _target_package_path(self, target_triple):
        return os.path.join(self.build_dir, 'Toolchains', target_triple)

    def _build_stdlib(
        self, host_target, target_name, target_triple, llvm_cmake_dir, sysroot
    ):
        platform, arch = target_name.split('-')
        stdlib = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(os.path.dirname(self.source_dir), 'swift'),
            build_dir=self._build_dir(target_name, 'stdlib'))

        stdlib.cmake_options.define('CMAKE_INSTALL_PREFIX:PATH', '/usr')
        stdlib.cmake_options.define('CMAKE_BUILD_TYPE:STRING', self._build_variant)
        stdlib.cmake_options.define(
            'CMAKE_SYSTEM_NAME:STRING', self._get_system_name(platform)
        )
        stdlib.cmake_options.define(
            'CMAKE_SYSTEM_PROCESSOR:STRING', self._get_system_processor(arch)
        )
        stdlib.cmake_options.define(
            'CMAKE_SYSTEM_VERSION:STRING', self.args.cross_compilation_system_version
        )
        stdlib.cmake_options.define('CMAKE_SYSROOT:PATH', sysroot)

        target_c_flags = self._target_c_flags()
        # Add -target and --sysroot since libdispatch sub-dependency of Swift
        # doesn't pick up on CMAKE_SYSTEM_PROCESSOR or CMAKE_SYSROOT. This can be
        # fixed in the future I'm sure.
        target_c_flags += f' -target {target_triple}'
        target_c_flags += f' --sysroot {sysroot}'
        stdlib.cmake_options.define('SWIFT_USE_LINKER:STRING', 'lld')
        stdlib.cmake_options.define('CMAKE_C_FLAGS:STRING', target_c_flags)
        stdlib.cmake_options.define('CMAKE_CXX_FLAGS:STRING', target_c_flags)

        stdlib.cmake_options.define(
            'SWIFT_STDLIB_BUILD_TYPE:STRING', self._build_variant
        )

        stdlib.cmake_options.define('CMAKE_C_COMPILER_TARGET:STRING', target_triple)
        stdlib.cmake_options.define('CMAKE_CXX_COMPILER_TARGET:STRING', target_triple)
        stdlib.cmake_options.define('CMAKE_Swift_COMPILER_TARGET:STRING', target_triple)

        if self.args.build_runtime_with_host_compiler:
            stdlib.cmake_options.define(
                'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', "TRUE"
            )
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.cc)
            )
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.swiftc)
            )
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                os.path.dirname(self.toolchain.llvm_ar)
            )
        else:
            # Toolchain configuration
            toolchain_path = self.native_toolchain_path(host_target)
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin')
            )
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin')
            )
            stdlib.cmake_options.define(
                'SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                os.path.join(toolchain_path, 'bin')
            )
            stdlib.cmake_options.define(
                'BOOTSTRAPPING_MODE:STRING', 'CROSSCOMPILE')
            stdlib.cmake_options.define(
                'SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', 'FALSE')

        stdlib.cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
        stdlib.cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
        stdlib.cmake_options.define('CMAKE_Swift_COMPILER_WORKS:BOOL', 'TRUE')
        stdlib.cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')

        stdlib.cmake_options.define('LLVM_DIR:PATH', llvm_cmake_dir)

        # Standalone stdlib configuration
        stdlib.cmake_options.define('SWIFT_INCLUDE_TOOLS:BOOL', 'FALSE')
        stdlib.cmake_options.define('SWIFT_INCLUDE_DOCS:BOOL', 'FALSE')
        stdlib.cmake_options.define('SWIFT_BUILD_REMOTE_MIRROR:BOOL', 'TRUE')
        stdlib.cmake_options.define('SWIFT_BUILD_SOURCEKIT:BOOL', 'FALSE')

        # Stdlib configuration
        stdlib.cmake_options.define('SWIFT_SDKS:STRING', platform.upper())
        stdlib.cmake_options.define(
            'SWIFT_SDK_%s_ARCH_%s_PATH' % (platform.upper(), arch.lower()),
            sysroot
        )

        # Disable overlay that's hardcoded to point to /usr
        if platform == 'linux':
            stdlib.cmake_options.define(
                'SWIFT_SDK_LINUX_CXX_OVERLAY_SWIFT_COMPILE_FLAGS:STRING', ''
            )

        # Library features
        stdlib.cmake_options.define('SWIFT_BUILD_DYNAMIC_STDLIB:BOOL', 'TRUE')
        stdlib.cmake_options.define(
            'SWIFT_BUILD_STATIC_STDLIB:BOOL',
            'TRUE' if self.args.build_swift_static_stdlib else 'FALSE'
        )
        stdlib.cmake_options.define('SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL', 'TRUE')
        stdlib.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY:BOOL', 'TRUE'
        )
        stdlib.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED:BOOL', 'TRUE'
        )
        stdlib.cmake_options.define('SWIFT_ENABLE_SYNCHRONIZATION:BOOL', 'TRUE')
        stdlib.cmake_options.define('SWIFT_ENABLE_VOLATILE:BOOL', 'TRUE')
        stdlib.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION:BOOL', 'TRUE'
        )
        stdlib.cmake_options.define(
            'SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB:BOOL', 'FALSE'
        )

        # Source directories
        source_dir = os.path.dirname(self.source_dir)
        stdlib.cmake_options.define(
            'SWIFT_PATH_TO_LIBDISPATCH_SOURCE:PATH',
            os.path.join(source_dir, 'swift-corelibs-libdispatch')
        )
        stdlib.cmake_options.define(
            'SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING:BOOL', 'TRUE'
        )
        stdlib.cmake_options.define(
            'SWIFT_PATH_TO_STRING_PROCESSING_SOURCE:PATH',
            os.path.join(source_dir, 'swift-experimental-string-processing')
        )

        # NOTE: Tests are currently disabled due to cross-compilation not being
        # fully supported yet. This will be revisited in the future.
        stdlib.cmake_options.define('SWIFT_INCLUDE_TESTS:BOOL', 'FALSE')

        # armv7 currently requires c11 threading model due to pthreads issues
        if arch == 'armv7':
            stdlib.cmake_options.define('SWIFT_THREADING_PACKAGE:STRING', 'c11')

        stdlib.build_with_cmake(
            [], self._build_variant, [],
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler
        )

        # Copy swiftrt.o to the sysroot. This is required since -sdk looks for
        # swiftrt.o at <sysroot>/usr/lib/swift/linux/<arch> instead of the
        # -resource-dir, which may be a bug?
        arch_dir = os.path.join(sysroot, 'usr', 'lib', 'swift', platform, arch)
        shell.makedirs(arch_dir)
        shutil.copy(
            os.path.join(stdlib.build_dir, 'lib', 'swift', platform, arch, 'swiftrt.o'),
            arch_dir
        )

        dest_dir = self._target_package_path(target_triple)
        with shell.pushd(stdlib.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _append_platform_cmake_options(
        self, target_name, target_triple, toolchain_file, cmake_options
    ):
        swift_build_dir = self._build_dir(target_name, 'stdlib')
        swift_resource_dir = os.path.join(swift_build_dir, 'lib', 'swift')
        swift_flags = ['-use-ld=lld',
                       '-sdk', self._get_sysroot(),
                       '-resource-dir', swift_resource_dir]

        cmake_options.define('CMAKE_Swift_COMPILER_TARGET', target_triple)
        cmake_options.define('CMAKE_TOOLCHAIN_FILE:PATH', toolchain_file)
        cmake_options.define('CMAKE_Swift_FLAGS', ' '.join(swift_flags))
        cmake_options.define('CMAKE_BUILD_TYPE:STRING', self.args.build_variant)

        target_c_flags = self._target_c_flags()
        cmake_options.define('CMAKE_C_FLAGS:STRING', target_c_flags)
        cmake_options.define('CMAKE_CXX_FLAGS:STRING', target_c_flags)

    def _build_dispatch(self, target_name, target_triple, toolchain_file, static=False):
        dispatch = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-corelibs-libdispatch'
            ),
            build_dir=self._build_dir(
                target_name, 'libdispatch-static' if static else 'libdispatch'
            )
        )
        self._append_platform_cmake_options(
            target_name, target_triple, toolchain_file, dispatch.cmake_options
        )
        dispatch.cmake_options.define(
            'BUILD_SHARED_LIBS:BOOL', 'FALSE' if static else 'TRUE'
        )
        dispatch.cmake_options.define('ENABLE_SWIFT:BOOL', 'TRUE')

        dispatch.build_with_cmake(
            [], self.args.build_variant, [],
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
            ignore_extra_cmake_options=True
        )

        dest_dir = self._target_package_path(target_triple)
        with shell.pushd(dispatch.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_foundation(
        self, target_name, target_triple, toolchain_file, static=False
    ):
        foundation = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-corelibs-foundation'),
            build_dir=self._build_dir(
                target_name, 'foundation-static' if static else 'foundation'
            )
        )
        self._append_platform_cmake_options(
            target_name, target_triple, toolchain_file, foundation.cmake_options
        )
        foundation.cmake_options.define(
            'BUILD_SHARED_LIBS:BOOL', 'FALSE' if static else 'TRUE'
        )
        foundation.cmake_options.define('FOUNDATION_BUILD_TOOLS', 'FALSE')

        # Build directories
        dispatch_build_dir = self._build_dir(target_name, 'libdispatch')
        foundation.cmake_options.define(
            'dispatch_DIR', os.path.join(dispatch_build_dir, 'cmake', 'modules')
        )

        # Source directories
        source_root = os.path.dirname(self.source_dir)
        host_toolchain = self.native_toolchain_path(self.args.host_target)
        foundation.cmake_options.define(
            '_SwiftCollections_SourceDIR',
            os.path.join(source_root, 'swift-collections')
        )
        foundation.cmake_options.define(
            '_SwiftFoundation_SourceDIR',
            os.path.join(source_root, 'swift-foundation')
        )
        foundation.cmake_options.define(
            '_SwiftFoundationICU_SourceDIR',
            os.path.join(source_root, 'swift-foundation-icu')
        )
        foundation.cmake_options.define(
            'SwiftFoundation_MACRO',
            os.path.join(host_toolchain, 'lib', 'swift', 'host', 'plugins')
        )

        foundation.build_with_cmake(
            [], self.args.build_variant, [],
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
            ignore_extra_cmake_options=True
        )

        dest_dir = self._target_package_path(target_triple)
        with shell.pushd(foundation.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _host_toolchain_path(self):
        if self.args.build_runtime_with_host_compiler:
            return os.path.dirname(os.path.dirname(self.toolchain.swiftc))

        return self.native_toolchain_path(self.args.host_target)

    def _build_swift_testing(self, target_name, target_triple, toolchain_file):
        swift_testing = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-testing'),
            build_dir=self._build_dir(target_name, 'swift-testing')
        )
        self._append_platform_cmake_options(
            target_name, target_triple, toolchain_file, swift_testing.cmake_options
        )
        swift_testing.cmake_options.define('BUILD_SHARED_LIBS', 'TRUE')

        # Build directories
        dispatch_build_dir = self._build_dir(target_name, 'libdispatch')
        swift_testing.cmake_options.define(
            'dispatch_DIR', os.path.join(dispatch_build_dir, 'cmake', 'modules')
        )
        foundation_build_dir = self._build_dir(target_name, 'foundation')
        swift_testing.cmake_options.define(
            'Foundation_DIR', os.path.join(foundation_build_dir, 'cmake', 'modules')
        )

        host_toolchain = self._host_toolchain_path()
        swift_testing.cmake_options.define(
            'SwiftTesting_MACRO',
            os.path.join(
                host_toolchain, 'lib', 'swift', 'host', 'plugins', 'libTestingMacros.so'
            )
        )

        swift_testing.build_with_cmake(
            [], self.args.build_variant, [],
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
            ignore_extra_cmake_options=True
        )

        dest_dir = self._target_package_path(target_triple)
        with shell.pushd(swift_testing.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    def _build_xctest(self, target_name, target_triple, toolchain_file):
        xctest = CMakeProduct(
            args=self.args,
            toolchain=self.toolchain,
            source_dir=os.path.join(
                os.path.dirname(self.source_dir), 'swift-corelibs-xctest'),
            build_dir=self._build_dir(target_name, 'xctest')
        )
        self._append_platform_cmake_options(
            target_name, target_triple, toolchain_file, xctest.cmake_options
        )
        xctest.cmake_options.define('BUILD_SHARED_LIBS', 'TRUE')

        # Build directories
        dispatch_build_dir = self._build_dir(target_name, 'libdispatch')
        xctest.cmake_options.define(
            'dispatch_DIR', os.path.join(dispatch_build_dir, 'cmake', 'modules')
        )
        foundation_build_dir = self._build_dir(target_name, 'foundation')
        xctest.cmake_options.define(
            'Foundation_DIR', os.path.join(foundation_build_dir, 'cmake', 'modules')
        )

        xctest.build_with_cmake(
            [], self.args.build_variant, [],
            prefer_native_toolchain=not self.args.build_runtime_with_host_compiler,
            ignore_extra_cmake_options=True
        )

        dest_dir = self._target_package_path(target_triple)
        with shell.pushd(xctest.build_dir):
            shell.call([self.toolchain.cmake, '--install', '.', '--prefix', '/usr'],
                       env={'DESTDIR': dest_dir})

    @property
    def _build_variant(self):
        return self.args.build_variant

    def _get_system_name(self, platform):
        if platform == 'linux':
            return 'Linux'
        elif platform == 'freebsd':
            return 'FreeBSD'
        elif platform == 'openbsd':
            return 'OpenBSD'
        else:
            raise ValueError(f"Unsupported platform for cross-compilation: {platform}")

    def _get_target_triple(self, target_name):
        (platform, arch) = target_name.split('-')
        if platform == 'linux':
            if arch in ('armv6', 'armv7'):
                return f"{arch}-unknown-{platform}-gnueabihf"
            else:
                return f"{arch}-unknown-{platform}-gnu"
        elif platform == 'freebsd' or platform == 'openbsd':
            return f"{arch}-unknown-{platform}"
        else:
            raise ValueError(f"Unsupported platform for cross-compilation: {platform}")

    def _get_system_processor(self, arch):
        if arch == 'armv7':
            return 'armv7-a'
        return arch

    def _create_toolchain_file(self, target_name, target_triple, sysroot):
        platform, arch = target_name.split('-')
        toolchain_file_path = self._build_dir(
            target_name, 'cross-compilation-toolchain.cmake'
        )
        with open(toolchain_file_path, 'w') as f:
            f.write(f"""
set(CMAKE_C_COMPILER_TARGET {target_triple})
set(CMAKE_CXX_COMPILER_TARGET {target_triple})
set(CMAKE_SYSROOT {sysroot})
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_SYSTEM_NAME {self._get_system_name(platform)})
set(CMAKE_SYSTEM_PROCESSOR {self._get_system_processor(arch)})
set(CMAKE_Swift_COMPILER_TARGET {target_triple})
""")
        return toolchain_file_path

    def should_install(self, host_target):
        return False

    @classmethod
    def get_dependencies(cls):
        return [llvm.LLVM, swift.Swift]
