#===--- CMakeLists.txt - Runtime module ------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===------------------------------------------------------------------------===#
#
#  The Runtime module isn't the runtime itself; that lives in libswiftCore;
#  rather, it's a high level Swift interface to things in the runtime.
#
#===------------------------------------------------------------------------===#

set(swift_runtime_link_libraries
  swiftCore
  swift_Concurrency
)

set(concurrency)
if(SWIFT_BUILD_STDLIB AND SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
  set(concurrency _Concurrency)
endif()

set(cxxstdlib_overlay)
if(SWIFT_ENABLE_EXPERIMENTAL_CXX_INTEROP)
  set(cxxstdlib_overlay CxxStdlib)
endif()

set(RUNTIME_SOURCES
  Address.swift
  Backtrace.swift
  Backtrace+Codable.swift
  BacktraceFormatter.swift
  Base64.swift
  ByteSwapping.swift
  CachingMemoryReader.swift
  CompactBacktrace.swift
  CompactImageMap.swift
  Compression.swift
  Context.swift
  CoreSymbolication.swift
  Dwarf.swift
  EightByteBuffer.swift
  Elf.swift
  ElfImageCache.swift
  FramePointerUnwinder.swift
  Image.swift
  ImageMap.swift
  ImageMap+Darwin.swift
  ImageMap+Linux.swift
  ImageSource.swift
  Libc.swift
  LimitSequence.swift
  MemoryReader.swift
  OSReleaseScanner.swift
  ProcMapsScanner.swift
  Registers.swift
  Runtime.swift
  RichFrame.swift
  SymbolicatedBacktrace.swift
  Utils.swift
  Win32Extras.cpp

  get-cpu-context.${SWIFT_ASM_EXT}
)

set(RUNTIME_COMPILE_FLAGS
  "-cxx-interoperability-mode=default"
  "-Xfrontend;-experimental-spi-only-imports"
  "-Xcc;-I${SWIFT_SOURCE_DIR}/include"
  "-Xcc;-I${CMAKE_BINARY_DIR}/include"
  "-Xcc;-I${SWIFT_STDLIB_SOURCE_DIR}/public/RuntimeModule/modules"
  "-disable-upcoming-feature;MemberImportVisibility")

###TODO: Add these when we add static linking support
#
#list(APPEND RUNTIME_COMPILE_FLAGS
#  "-Xcc;-I${SWIFT_PATH_TO_ZLIB_SOURCE}"
#  "-Xcc;-I${SWIFT_PATH_TO_ZSTD_SOURCE}/lib"
#  "-Xcc;-I${SWIFT_PATH_TO_LIBLZMA_SOURCE}/src/liblzma/api")

if(SWIFT_ASM_AVAILABLE)
  list(APPEND RUNTIME_SOURCES get-cpu-context.${SWIFT_ASM_EXT})
  list(APPEND RUNTIME_COMPILE_FLAGS "-DSWIFT_ASM_AVAILABLE")
else()
  message(warning "Assembly language not available on this platform; backtracing will fail.")
endif()

set(LLVM_OPTIONAL_SOURCES
  get-cpu-context.S
  get-cpu-context.asm
)

# We have to build with a deployment target of at least 10.15, otherwise
# the tests will all fail because dyld will get confused at the use of
# @rpath (from magic-symbols-for-install-name.c) and the `some Sequence<Frame>`
# in Backtrace won't be accessible.
if(SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX VERSION_LESS "10.15")
  set(osx_deployment_target "10.15")
else()
  set(osx_deployment_target "${SWIFT_DARWIN_DEPLOYMENT_VERSION_OSX}")
endif()

add_swift_target_library(swiftRuntime ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${RUNTIME_SOURCES}

  SWIFT_MODULE_DEPENDS ${concurrency} ${cxxstdlib_overlay}

  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT

  PRIVATE_LINK_LIBRARIES ${swift_runtime_link_libraries}

  DEPLOYMENT_VERSION_OSX ${osx_deployment_target}

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${RUNTIME_COMPILE_FLAGS}
    -parse-stdlib

  LINK_FLAGS
    ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR "zippered"

  TARGET_SDKS OSX LINUX LINUX_STATIC
)
