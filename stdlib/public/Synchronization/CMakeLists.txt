#===--- CMakeLists.txt - Synchronization library ---------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2023 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

add_swift_target_library(swiftSynchronization ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
# List of files here:
  Atomic.swift
  AtomicBool.swift
  AtomicFloats.swift
  AtomicLazyReference.swift
  AtomicMemoryOrderings.swift
  AtomicOptional.swift
  AtomicPointers.swift
  AtomicRepresentable.swift
  WordPair.swift

  GYB_SOURCES
    AtomicIntegers.swift.gyb
    AtomicStorage.swift.gyb

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    "-enable-builtin-module"
    "-enable-experimental-feature" "RawLayout"
    "-enable-experimental-feature" "StaticExclusiveOnly"

  INSTALL_IN_COMPONENT
    stdlib

  MACCATALYST_BUILD_FLAVOR
    "zippered"
)