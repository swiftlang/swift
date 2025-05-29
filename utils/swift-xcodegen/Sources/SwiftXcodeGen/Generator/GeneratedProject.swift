//===--- GeneratedProject.swift -------------------------------------------===//
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

public struct GeneratedProject: Sendable {
  public let path: AbsolutePath
  let allBuildTargets: [Scheme.BuildTarget]

  init(at path: AbsolutePath, allBuildTargets: [Scheme.BuildTarget]) {
    self.path = path
    self.allBuildTargets = allBuildTargets
  }
}
