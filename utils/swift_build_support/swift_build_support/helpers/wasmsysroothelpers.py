# swift_build_support/helpers/wasmsysroothelpers.py --------------*- python -*-
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

"""Shared helpers for building WebAssembly sysroots (libc++, compiler-rt).

Used by :class:`WASISysroot` (``wasisysroot.py``) and
:class:`EmscriptenLLVMRuntimeLibs` (``emscriptensysroot.py``).
Platform-specific cmake setup is injected via the
``append_platform_cmake_options(cmake_options)`` callable, mirroring the
pattern established by ``wasmswiftsdkhelpers.py`` one layer up.

Note: callers compute ``compiler_rt_source_dir`` themselves from
``os.path.dirname(self.source_dir)`` plus product-specific suffixes
(``llvm-project/compiler-rt`` for WASI, ``compiler-rt`` for Emscripten —
coincidentally the same absolute path). Renaming either product's
``product_source_name`` would silently shift that math.
"""

from ..products import cmake_product


def build_runtimes(args, toolchain, runtimes_source_dir, runtimes_build_dir,
                   install_path, enable_threads, libdir_suffix,
                   append_platform_cmake_options):
    """Build and install libc++ / libc++abi for a WebAssembly target.

    ``append_platform_cmake_options(cmake_options)`` is responsible for
    setting every platform-specific ``CMAKE_*`` option (``CMAKE_SYSROOT``,
    ``CMAKE_STAGING_PREFIX``, ``CMAKE_SYSTEM_NAME``,
    ``CMAKE_SYSTEM_PROCESSOR``, ``UNIX``, ``CMAKE_AR``, ``CMAKE_RANLIB``,
    compilers, target triples, flags). This helper owns the shared
    ``LIBCXX_*`` / ``LIBCXXABI_*`` / ``LLVM_ENABLE_RUNTIMES`` /
    ``LIBCXX_LIBDIR_SUFFIX`` / ``LIBCXXABI_LIBDIR_SUFFIX`` knobs plus
    the build + install invocations.
    """
    cmake = cmake_product.CMakeProduct(
        args=args, toolchain=toolchain,
        source_dir=runtimes_source_dir, build_dir=runtimes_build_dir)

    append_platform_cmake_options(cmake.cmake_options)
    cmake.cmake_options.define('LLVM_ENABLE_RUNTIMES:STRING',
                               'libcxx;libcxxabi')
    cmake.cmake_options.define('LIBCXX_LIBDIR_SUFFIX:STRING', libdir_suffix)
    cmake.cmake_options.define('LIBCXXABI_LIBDIR_SUFFIX:STRING', libdir_suffix)
    cmake.cmake_options.define('CXX_SUPPORTS_CXX11:BOOL', 'TRUE')

    threads = 'TRUE' if enable_threads else 'FALSE'
    cmake.cmake_options.define('LIBCXX_ENABLE_THREADS:BOOL', threads)
    cmake.cmake_options.define('LIBCXX_HAS_PTHREAD_API:BOOL', threads)
    cmake.cmake_options.define('LIBCXX_HAS_EXTERNAL_THREAD_API:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXX_BUILD_EXTERNAL_THREAD_LIBRARY:BOOL',
                               'FALSE')
    cmake.cmake_options.define('LIBCXX_HAS_WIN32_THREAD_API:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXX_ENABLE_SHARED:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXX_ENABLE_EXPERIMENTAL_LIBRARY:BOOL',
                               'FALSE')
    cmake.cmake_options.define('LIBCXX_ENABLE_EXCEPTIONS:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXX_ENABLE_FILESYSTEM:BOOL', 'TRUE')
    cmake.cmake_options.define('LIBCXX_CXX_ABI', 'libcxxabi')
    cmake.cmake_options.define('LIBCXX_HAS_MUSL_LIBC:BOOL', 'TRUE')

    cmake.cmake_options.define('LIBCXX_ABI_VERSION', '2')
    cmake.cmake_options.define('LIBCXXABI_ENABLE_EXCEPTIONS:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXXABI_ENABLE_SHARED:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXXABI_USE_LLVM_UNWINDER:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXXABI_SILENT_TERMINATE:BOOL', 'TRUE')
    cmake.cmake_options.define('LIBCXXABI_ENABLE_THREADS:BOOL', threads)
    cmake.cmake_options.define('LIBCXXABI_HAS_PTHREAD_API:BOOL', threads)
    cmake.cmake_options.define('LIBCXXABI_HAS_EXTERNAL_THREAD_API:BOOL',
                               'FALSE')
    cmake.cmake_options.define('LIBCXXABI_BUILD_EXTERNAL_THREAD_LIBRARY:BOOL',
                               'FALSE')
    cmake.cmake_options.define('LIBCXXABI_HAS_WIN32_THREAD_API:BOOL', 'FALSE')
    cmake.cmake_options.define('LIBCXXABI_ENABLE_PIC:BOOL', 'FALSE')
    cmake.cmake_options.define('UNIX:BOOL', 'TRUE')

    cmake.build_with_cmake(
        [], args.build_variant, [],
        prefer_native_toolchain=not args.build_runtime_with_host_compiler,
        ignore_extra_cmake_options=True)
    cmake.install_with_cmake(['install'], install_path)


def build_compiler_rt(args, toolchain, compiler_rt_source_dir,
                      compiler_rt_build_dir, install_path,
                      compiler_rt_os_dir, append_platform_cmake_options):
    """Build and install ``compiler-rt`` (non-builtins) for a WebAssembly target.

    Same platform-callable contract as :func:`build_runtimes`. Keeps the
    shared ``COMPILER_RT_*`` knobs plus the build + install invocations
    in one place.
    """
    compiler_rt = cmake_product.CMakeProduct(
        args=args, toolchain=toolchain,
        source_dir=compiler_rt_source_dir, build_dir=compiler_rt_build_dir)

    append_platform_cmake_options(compiler_rt.cmake_options)

    compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ARCH:STRING',
                                     'wasm32')
    compiler_rt.cmake_options.define('COMPILER_RT_DEFAULT_TARGET_ONLY:BOOL',
                                     'TRUE')
    compiler_rt.cmake_options.define('COMPILER_RT_BAREMETAL_BUILD:BOOL',
                                     'TRUE')
    compiler_rt.cmake_options.define('COMPILER_RT_BUILD_XRAY:BOOL', 'FALSE')
    compiler_rt.cmake_options.define('COMPILER_RT_BUILD_PROFILE:BOOL', 'TRUE')
    compiler_rt.cmake_options.define('COMPILER_RT_INCLUDE_TESTS:BOOL', 'FALSE')
    compiler_rt.cmake_options.define('COMPILER_RT_HAS_FPIC_FLAG:BOOL', 'FALSE')
    compiler_rt.cmake_options.define('COMPILER_RT_EXCLUDE_ATOMIC_BUILTIN:BOOL',
                                     'FALSE')
    compiler_rt.cmake_options.define('COMPILER_RT_OS_DIR:STRING',
                                     compiler_rt_os_dir)

    compiler_rt.build_with_cmake(
        [], args.build_variant, [],
        prefer_native_toolchain=not args.build_runtime_with_host_compiler,
        ignore_extra_cmake_options=True)
    compiler_rt.install_with_cmake(['install'], install_path)
