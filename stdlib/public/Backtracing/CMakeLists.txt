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
  swift_Concurrency
)

set(concurrency)
if(SWIFT_BUILD_STDLIB AND SWIFT_ENABLE_EXPERIMENTAL_CONCURRENCY)
  set(concurrency _Concurrency)
endif()

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
  Shims.cpp

  get-cpu-context.${SWIFT_ASM_EXT}
)

set(BACKTRACING_COMPILE_FLAGS)

if(SWIFT_ASM_AVAILABLE)
  list(APPEND BACKTRACING_SOURCES get-cpu-context.${SWIFT_ASM_EXT})
  list(APPEND BACKTRACING_COMPILE_FLAGS "-DSWIFT_ASM_AVAILABLE")
else()
  message(warning "Assembly language not available on this platform; backtracing will fail.")
endif()

set(LLVM_OPTIONAL_SOURCES
  get-cpu-context.S
  get-cpu-context.asm
)

add_swift_target_library(swift_Backtracing ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  ${BACKTRACING_SOURCES}

  SWIFT_MODULE_DEPENDS ${concurrency}

  LINK_LIBRARIES ${swift_backtracing_link_libraries}

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${BACKTRACING_COMPILE_FLAGS}
    -parse-stdlib

  LINK_FLAGS
    ${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}

  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR "zippered"

  TARGET_SDKS OSX
)
