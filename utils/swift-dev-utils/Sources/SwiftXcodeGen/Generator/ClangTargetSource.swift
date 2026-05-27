//===--- ClangTargetSource.swift ------------------------------------------===//
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

/// The path at which to find source files for a particular target.
struct ClangTargetSource {
  var name: String
  var path: RelativePath
  var mayHaveUnbuildableFiles: Bool

  init(
    at path: RelativePath, named name: String,
    mayHaveUnbuildableFiles: Bool
  ) {
    self.name = name
    self.path = path
    self.mayHaveUnbuildableFiles = mayHaveUnbuildableFiles
  }
}
