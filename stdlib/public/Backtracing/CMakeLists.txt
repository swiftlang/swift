#===--- CMakeLists.txt - Backtracing support library -----------------------===#
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

set(swift_backtracing_link_libraries
  swiftCore
)

set(BACKTRACING_SOURCES
  Backtrace.swift
  BacktraceFormatter.swift
  Context.swift
  CoreSymbolication.swift
  FramePointerUnwinder.swift
  MemoryReader.swift
  Registers.swift
  SymbolicatedBacktrace.swift
  Utils.swift

  get-cpu-context.${SWIFT_ASM_EXT}
)

set(LLVM_OPTIONAL_SOURCES
  get-cpu-context.S
  get-cpu-context.asm
)

add_swift_target_library(swift_Backtracing ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${BACKTRACING_SOURCES}

  SWIFT_MODULE_DEPENDS_IOS Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_OSX Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_TVOS Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_WATCHOS Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_MACCATALYST Darwin _Concurrency
  SWIFT_MODULE_DEPENDS_LINUX Glibc _Concurrency
  SWIFT_MODULE_DEPENDS_WINDOWS CRT _Concurrency

  LINK_LIBRARIES ${swift_backtracing_link_libraries}

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    -parse-stdlib

  LINK_FLAGS
    ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR "zippered"

  TARGET_SDKS OSX
)
