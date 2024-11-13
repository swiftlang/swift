//===--- Error.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

enum XcodeGenError: Error, CustomStringConvertible {
  case pathNotFound(AbsolutePath)
  case noSwiftBuildDir(AbsolutePath, couldBeParent: Bool)
  case expectedParent(AbsolutePath)

  var description: String {
    switch self {
    case .pathNotFound(let basePath):
      return "'\(basePath)' not found"
    case .noSwiftBuildDir(let basePath, let couldBeParent):
      let base = "no swift build directory found in '\(basePath)'"
      let note = "; did you mean to pass the path of the parent?"
      return couldBeParent ? "\(base)\(note)" : base
    case .expectedParent(let basePath):
      return "expected '\(basePath)' to have parent directory"
    }
  }
}
