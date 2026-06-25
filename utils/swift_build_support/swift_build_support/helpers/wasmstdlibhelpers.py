# swift_build_support/helpers/wasmstdlibhelpers.py --------------*- python -*-
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023-2026 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
# ----------------------------------------------------------------------------

"""Shared helpers for building the standalone Swift standard library for a
WebAssembly target.

Used by `WASIStdlib` / `WASIThreadsStdlib` (`wasistdlib.py`)
and `EmscriptenStdlib` (`emscriptenstdlib.py`). Every option that
differs between the WASI and Emscripten standalone stdlib builds is injected
through the `append_platform_cmake_options(cmake_options)` callable, following
the single-callable pattern of `wasmsysroothelpers.py`.
"""

import os

from .. import cmake
from .. import shell
from ..products.cmake_product import CMakeProduct


def build_stdlib(args, toolchain, source_dir, build_dir, host_target,
                 short_triple, append_platform_cmake_options):
    """Configure a target-only LLVM, then configure and build the standalone
    Swift stdlib for a WebAssembly target.

    `append_platform_cmake_options(cmake_options)` sets every option that
    differs between the WASI and Emscripten stdlib builds; see each product's
    `_append_platform_cmake_options`. It is invoked before the options
    shared verbatim by both builds (which this helper owns), so the shared
    block is emitted last and would win under CMake last-wins on any
    collision. In practice the two sets are disjoint (guarded by
    `test_stdlib_configure_has_no_duplicate_options`), so the call order
    does not affect the resulting configuration.
    """
    prefer_native = not args.build_runtime_with_host_compiler

    # A single bare CMakeProduct drives both the LLVM configure and the stdlib
    # configure/build, mirroring the products' prior structure (one `self`).
    # This is safe because host_cmake_options() returns a fresh CMakeOptions
    # (it never mutates cp.cmake_options), so the LLVM-configure defines cannot
    # leak into the stdlib configure.
    stdlib = CMakeProduct(
        args=args, toolchain=toolchain,
        source_dir=source_dir, build_dir=build_dir)

    llvm_build_dir = _configure_llvm(stdlib, host_target, short_triple)
    llvm_cmake_dir = os.path.join(llvm_build_dir, 'lib', 'cmake', 'llvm')

    opts = stdlib.cmake_options

    append_platform_cmake_options(opts)

    opts.define('CMAKE_INSTALL_PREFIX:PATH', '/usr')
    opts.define('CMAKE_BUILD_TYPE:STRING', args.build_variant)
    opts.define('CMAKE_SYSTEM_PROCESSOR:STRING', 'wasm32')
    # UNIX is explicitly set to TRUE because CMake recognizes neither WASI nor
    # Emscripten as a UNIX platform, yet the variable is used in LLVM's CMake
    # configuration.
    opts.define('UNIX:BOOL', 'TRUE')
    opts.define('SWIFT_STDLIB_BUILD_TYPE:STRING', args.build_variant)

    if args.build_runtime_with_host_compiler:
        opts.define('SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', "TRUE")
        opts.define('SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                    os.path.dirname(toolchain.cc))
        opts.define('SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                    os.path.dirname(toolchain.swiftc))
        opts.define('SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                    os.path.dirname(toolchain.llvm_ar))
    else:
        toolchain_path = stdlib.native_toolchain_path(host_target)
        opts.define('CMAKE_AR:STRING',
                    os.path.join(toolchain_path, 'bin', 'llvm-ar'))
        opts.define('CMAKE_RANLIB:STRING',
                    os.path.join(toolchain_path, 'bin', 'llvm-ranlib'))
        opts.define('SWIFT_NATIVE_CLANG_TOOLS_PATH:STRING',
                    os.path.join(toolchain_path, 'bin'))
        opts.define('SWIFT_NATIVE_SWIFT_TOOLS_PATH:STRING',
                    os.path.join(toolchain_path, 'bin'))
        opts.define('SWIFT_NATIVE_LLVM_TOOLS_PATH:STRING',
                    os.path.join(toolchain_path, 'bin'))
        opts.define('BOOTSTRAPPING_MODE:STRING', 'CROSSCOMPILE')
        opts.define('SWIFT_BUILD_RUNTIME_WITH_HOST_COMPILER:BOOL', 'FALSE')

    opts.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
    opts.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
    opts.define('CMAKE_Swift_COMPILER_WORKS:BOOL', 'TRUE')
    opts.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')

    opts.define('LLVM_DIR:PATH', llvm_cmake_dir)

    opts.define('SWIFT_INCLUDE_TOOLS:BOOL', 'FALSE')
    opts.define('SWIFT_INCLUDE_DOCS:BOOL', 'FALSE')
    opts.define('SWIFT_BUILD_REMOTE_MIRROR:BOOL', 'FALSE')
    opts.define('SWIFT_BUILD_SOURCEKIT:BOOL', 'FALSE')

    opts.define('SWIFT_PRIMARY_VARIANT_ARCH:STRING', 'wasm32')
    opts.define('SWIFT_BUILD_STATIC_STDLIB:BOOL', 'TRUE')
    opts.define('SWIFT_BUILD_DYNAMIC_STDLIB:BOOL', 'FALSE')
    opts.define('SWIFT_BUILD_STATIC_SDK_OVERLAY:BOOL', 'TRUE')
    opts.define('SWIFT_STDLIB_INSTALL_ONLY_CLANG_RESOURCE_HEADERS:BOOL', 'TRUE')
    opts.define('SWIFT_STDLIB_STABLE_ABI:BOOL', 'TRUE')
    opts.define('SWIFT_STDLIB_TRACING:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_HAS_ASLR:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_INSTALL_PARENT_MODULE_FOR_SHIMS:BOOL', 'FALSE')
    opts.define('SWIFT_RUNTIME_CRASH_REPORTER_CLIENT:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_SINGLE_THREADED_CONCURRENCY:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_DISPATCH:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_SUPPORTS_BACKTRACE_REPORTING:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_HAS_DLADDR:BOOL', 'FALSE')
    opts.define('SWIFT_STDLIB_COMPACT_ABSOLUTE_FUNCTION_POINTER:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_DISTRIBUTED:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_STRING_PROCESSING:BOOL', 'TRUE')
    opts.define('SWIFT_PATH_TO_STRING_PROCESSING_SOURCE:PATH',
                os.path.join(source_dir, '..',
                             'swift-experimental-string-processing'))
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_SYNCHRONIZATION:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_VOLATILE:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_OBSERVATION:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_EXPERIMENTAL_DIFFERENTIABLE_PROGRAMMING', 'TRUE')
    opts.define('SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB:BOOL', 'TRUE')
    opts.define('SWIFT_SHOULD_BUILD_EMBEDDED_STDLIB_CROSS_COMPILING', 'TRUE')
    opts.define('SWIFT_INCLUDE_TESTS:BOOL', 'TRUE')
    opts.define('SWIFT_ENABLE_SOURCEKIT_TESTS:BOOL', 'FALSE')
    opts.define('LLVM_LIT_ARGS', args.lit_args)

    stdlib.build_with_cmake(
        [], args.build_variant, [], prefer_native_toolchain=prefer_native)


def _configure_llvm(cp, host_target, short_triple):
    """Configure (without building) a target-only LLVM tree so the stdlib
    build can use LLVM's CMake modules with zlib/libxml2/libedit/terminfo
    disabled. `cp` is the same CMakeProduct that later drives the stdlib
    build. Only the LLVM-options element of host_cmake_options' 3-tuple is
    used here, matching the products' prior behavior."""
    args = cp.args
    toolchain = cp.toolchain
    prefer_native = not args.build_runtime_with_host_compiler

    build_root = os.path.dirname(cp.build_dir)
    llvm_build_dir = os.path.join(build_root, 'llvm-%s' % short_triple)
    llvm_source_dir = os.path.join(
        os.path.dirname(cp.source_dir), 'llvm-project', 'llvm')

    cmake_options, _, _ = cp.host_cmake_options(host_target)
    cmake_options.define('CMAKE_BUILD_TYPE:STRING', args.build_variant)
    cmake_options.define('CMAKE_C_COMPILER_WORKS:BOOL', 'TRUE')
    cmake_options.define('CMAKE_CXX_COMPILER_WORKS:BOOL', 'TRUE')
    cmake_options.define('LLVM_COMPILER_CHECKED:BOOL', 'TRUE')
    cmake_options.define('LLVM_ENABLE_ZLIB:BOOL', 'FALSE')
    cmake_options.define('LLVM_ENABLE_LIBXML2:BOOL', 'FALSE')
    cmake_options.define('LLVM_ENABLE_LIBEDIT:BOOL', 'FALSE')
    cmake_options.define('LLVM_ENABLE_TERMINFO:BOOL', 'FALSE')
    if args.build_runtime_with_host_compiler:
        cmake_options.define('CMAKE_ASM_COMPILER:PATH', toolchain.cc)

    llvm_cmake = cmake.CMake(
        args, toolchain, prefer_native_toolchain=prefer_native)
    shell.call(["env", toolchain.cmake, "-B", llvm_build_dir]
               + list(llvm_cmake.common_options(cp))
               + list(cmake_options)
               + [llvm_source_dir])
    return llvm_build_dir
