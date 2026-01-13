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

set(backtracing_sources
  Address.swift
  Backtrace.swift
  Backtrace+Codable.swift
  BacktraceFormatter.swift
  BacktraceJSONFormatter.swift
  Base64.swift
  ByteSwapping.swift
  CachingMemoryReader.swift
  CompactBacktrace.swift
  CompactImageMap.swift
  Compression.swift
  Context.swift
  CoreSymbolication.swift
  CrashLog.swift
  CrashLogCapture.swift
  Dwarf.swift
  EightByteBuffer.swift
  Elf.swift
  ElfImageCache.swift
  FramePointerUnwinder.swift
  Image.swift
  ImageMap.swift
  ImageMap+Darwin.swift
  ImageMap+Linux.swift
  ImageMap+Win32.swift
  ImageSource.swift
  Libc.swift
  LimitSequence.swift
  MemoryReader.swift
  OSReleaseScanner.swift
  PeCoff.swift
  PeImageCache.swift
  ProcMapsScanner.swift
  Registers.swift
  Runtime.swift
  RichFrame.swift
  SymbolicatedBacktrace.swift
  Utils.swift
  Win32Unwinder.swift
)

set(runtime_sources
  empty.c
  Mangling.swift
)
if(SWIFT_ENABLE_BACKTRACING)
  list(APPEND runtime_sources ${backtracing_sources})
endif()

set(runtime_compile_flags
  "-cxx-interoperability-mode=default"
  "-Xfrontend;-experimental-spi-only-imports"
  "-Xcc;-I${SWIFT_SOURCE_DIR}/include"
  "-Xcc;-I${CMAKE_BINARY_DIR}/include"
  "-Xcc;-I${SWIFT_STDLIB_SOURCE_DIR}/public/RuntimeModule/modules"
  "-disable-upcoming-feature;MemberImportVisibility")

###TODO: Add these when we add static linking support
#
#list(APPEND runtime_compile_flags
#  "-Xcc;-I${SWIFT_PATH_TO_ZLIB_SOURCE}"
#  "-Xcc;-I${SWIFT_PATH_TO_ZSTD_SOURCE}/lib"
#  "-Xcc;-I${SWIFT_PATH_TO_LIBLZMA_SOURCE}/src/liblzma/api")

if(NOT SWIFT_ASM_AVAILABLE)
  message(warning "Assembly language not available on this platform; backtracing will fail.")
else()
  list(APPEND runtime_compile_flags "-DSWIFT_ASM_AVAILABLE")
endif()

set(LLVM_OPTIONAL_SOURCES
  get-cpu-context.S
  get-cpu-context-aarch64.asm
  get-cpu-context-x86_64.asm
  ${backtracing_sources}
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
  ${runtime_sources}

  SWIFT_MODULE_DEPENDS ${concurrency} ${cxxstdlib_overlay}

  SWIFT_MODULE_DEPENDS_ANDROID Android
  SWIFT_MODULE_DEPENDS_LINUX Glibc
  SWIFT_MODULE_DEPENDS_LINUX_STATIC Musl
  SWIFT_MODULE_DEPENDS_FREEBSD Glibc
  SWIFT_MODULE_DEPENDS_OPENBSD Glibc
  SWIFT_MODULE_DEPENDS_CYGWIN Glibc
  SWIFT_MODULE_DEPENDS_HAIKU Glibc
  SWIFT_MODULE_DEPENDS_WINDOWS CRT WinSDK

  PRIVATE_LINK_LIBRARIES ${swift_runtime_link_libraries}

  DEPLOYMENT_VERSION_OSX ${osx_deployment_target}

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${runtime_compile_flags}
    -parse-stdlib

  LINK_FLAGS
    ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR "zippered"

  TARGET_SDKS OSX LINUX LINUX_STATIC WINDOWS
)

if(SWIFT_ENABLE_BACKTRACING)
  # Add the correct assembly file for each SDK; this needs to be done this
  # way because Windows has a different assembler for each architecture, and
  # they don't all support the same syntax, so cannot share a single file.
  foreach(sdk ${SWIFT_SDKS})
    set(sdk_supported_archs
      ${SWIFT_SDK_${sdk}_ARCHITECTURES}
      ${SWIFT_SDK_${sdk}_MODULE_ARCHITECTURES})
    list(REMOVE_DUPLICATES sdk_supported_archs)

    foreach(arch ${sdk_supported_archs})
      set(target "swiftRuntime-${SWIFT_SDK_${sdk}_LIB_SUBDIR}-${arch}")
      if("${SWIFT_SDK_${sdk}_NAME}" STREQUAL "Windows")
        message(STATUS "Using get-cpu-context-${arch}.asm for target ${target}")
        target_sources(${target}
          PRIVATE get-cpu-context-${arch}.asm)
        # Turn on /safeseh for 32-bit x86 or we won't be able to link
        if("${arch}" STREQUAL "i686")
          target_compile_options(${target} PRIVATE
            $<$<COMPILE_LANGUAGE:ASM_MASM>:/safeseh>)
        endif()
      else()
        message(STATUS "Using get-cpu-context.S for target ${target}")
        target_sources(${target}
          PRIVATE get-cpu-context.S)
      endif()
    endforeach()

  endforeach()
endif()
