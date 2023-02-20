#===--- CMakeLists.txt - Reflection support library ---------------------===#
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

set(SWIFT_REFLECTION_SWIFT_FLAGS)

list(APPEND SWIFT_REFLECTION_SWIFT_FLAGS
  "-parse-stdlib")

add_swift_target_library(swiftReflection ${SWIFT_STDLIB_LIBRARY_BUILD_TYPES} IS_STDLIB
  Case.swift
  Field.swift
  GenericArguments.swift
  KeyPath.swift
  PartialType.swift
  Type.swift

  SWIFT_COMPILE_FLAGS
    ${SWIFT_STANDARD_LIBRARY_SWIFT_FLAGS}
    ${SWIFT_REFLECTION_SWIFT_FLAGS}
  SWIFT_MODULE_DEPENDS
    _Runtime
  INSTALL_IN_COMPONENT
    stdlib
  MACCATALYST_BUILD_FLAVOR zippered
  )
