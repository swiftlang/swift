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
  case couldNotInferProjectRoot(reason: String)

  var description: String {
    switch self {
    case .pathNotFound(let basePath):
      return "'\(basePath)' not found"
    case .noSwiftBuildDir(let basePath, let couldBeParent):
      let base = "no swift build directory found in '\(basePath)'"
      let note = "; did you mean to pass the path of the parent?"
      return couldBeParent ? "\(base)\(note)" : base
    case .couldNotInferProjectRoot(let reason):
      return """
        could not infer project root path; \(reason); please manually specify \
        using '--project-root-dir' instead
        """
    }
  }
}
