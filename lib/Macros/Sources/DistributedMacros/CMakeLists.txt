#===--- CMakeLists.txt - Distributed macros library ----------------------===#
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

add_swift_macro_library(DistributedMacros
  # macros
  DistributedResolveMacro.swift
  DistributedRequirementStubsMacro.swift
  DistributedResolveStubMacro.swift
  # Given Pavel's abstracting over ActorSystem, this macro could work:
  DistributedMakeStubTypeMacro.swift
  # support
  Availability.swift
  DistributedMacroErrors.swift
  Extensions.swift

  SWIFT_DEPENDENCIES
    SwiftDiagnostics
    SwiftOperators
    SwiftSyntaxBuilder
    SwiftSyntax
    SwiftSyntaxMacros
)
