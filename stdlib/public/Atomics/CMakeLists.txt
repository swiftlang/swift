#===--- CMakeLists.txt - Differentiable programming support library ------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

add_swift_target_library(swiftAtomics ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  AtomicMemoryOrderings.swift
  UnsafeAtomic.swift
  UnsafeAtomicLazyReference.swift
  UnsafeAtomicMutablePointer.swift
  UnsafeAtomicState.swift
  UnsafeAtomicUnmanaged.swift

  GYB_SOURCES
  RawAtomics.swift.gyb
  UnsafeAtomicOperations.swift.gyb

  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS} "-parse-stdlib"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib)
