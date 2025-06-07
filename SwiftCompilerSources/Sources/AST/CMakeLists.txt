# This source file is part of the Swift.org open source project
#
# Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
# Licensed under Apache License v2.0 with Runtime Library Exception
#
# See http://swift.org/LICENSE.txt for license information
# See http://swift.org/CONTRIBUTORS.txt for Swift project authors

add_swift_compiler_module(AST
  DEPENDS
    Basic
  SOURCES
    Declarations.swift
    Conformance.swift
    DiagnosticEngine.swift
    GenericSignature.swift
    Registration.swift
    SubstitutionMap.swift
    Type.swift)
