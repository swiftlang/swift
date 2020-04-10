#===----------------------------------------------------------------------===#
#
# This source file is part of the Swift.org open source project
#
# Copyright (c) 2020 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See https://swift.org/LICENSE.txt for license information
# See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
#
#===----------------------------------------------------------------------===#

add_swift_target_library(swiftAtomics ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  AtomicInteger.swift
  AtomicMemoryOrderings.swift
  AtomicProtocol.swift
  AtomicOptional.swift
  PrimitiveAtomic.swift
  UnsafeAtomicLazyReference.swift

  GYB_SOURCES
  AtomicIntegerConformances.swift.gyb
  AtomicPointerConformances.swift.gyb
  UnsafeAtomic.swift.gyb

  SWIFT_COMPILE_FLAGS ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS} "-parse-stdlib"
  LINK_FLAGS "${SWIFT_RUNTIME_SWIFT_LINK_FLAGS}"
  INSTALL_IN_COMPONENT stdlib
  MACCATALYST_BUILD_FLAVOR zippered)
