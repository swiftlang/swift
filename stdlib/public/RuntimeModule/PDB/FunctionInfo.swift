//===--- FunctionInfo.swift - PDB support for Swift -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a structure used to hold information about a function.
//
//===----------------------------------------------------------------------===//

import Swift

struct FunctionInfo {
  enum Scope {
    case global
    case local
  }

  var name: String
  var address: UInt32
  var length: UInt32
  var scope: Scope
  var moduleIndex: Int
}
