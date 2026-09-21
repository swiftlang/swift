//===--- NinjaBuildDirTests.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Testing

@testable import SwiftXcodeGen

/// Creates a minimal Ninja build dir layout under `parent`.
private func makeBuildDir(in parent: AbsolutePath) throws -> AbsolutePath {
  let buildDir = parent.appending("Ninja")
  try buildDir.appending("swift-dreamos-leg46").makeDir()
  return buildDir
}

@Suite
struct NinjaBuildDirTests {
  @Test
  func explicitProjectRoot() throws {
    try withTemporaryDirectory { root in
      try root.appending("swift").makeDir()
      let buildDir = try makeBuildDir(in: root.appending("build"))

      let explicit = root.appending("explicit")
      try explicit.makeDir()

      let ninja = try NinjaBuildDir(at: buildDir, projectRootDir: explicit)
      #expect(ninja.projectRootDir == explicit)
    }
  }

  @Test
  func inferProjectRootFromBuildDir() throws {
    try withTemporaryDirectory { root in
      try root.appending("swift").makeDir()
      let buildDir = try makeBuildDir(in: root.appending("build"))

      let ninja = try NinjaBuildDir(at: buildDir, projectRootDir: nil)
      // Note: Not normalising the paths for comparison because we do not expect
      // them to be normalised.
      #expect(ninja.projectRootDir == root)
    }
  }

  @Test
  func fallBackWhenSwiftNotFoundRelativeToBuildDir() throws {
    try withTemporaryDirectory { root in
      // '<build-dir>/../../swift' does not exist.
      let buildDir = try makeBuildDir(in: root.appending("build"))

      let ninja = try NinjaBuildDir(at: buildDir, projectRootDir: nil)
      #expect(ninja.projectRootDir == NinjaBuildDir.fallbackProjectRootPath)
    }
  }

  @Test
  func fallBackWhenBuildDirParentNotNamedBuild() throws {
    try withTemporaryDirectory { root in
      // '<build-dir>/..' not named 'build'.
      try root.appending("swift").makeDir()
      let buildDir = try makeBuildDir(in: root.appending("notbuild"))

      let ninja = try NinjaBuildDir(at: buildDir, projectRootDir: nil)
      #expect(ninja.projectRootDir == NinjaBuildDir.fallbackProjectRootPath)
    }
  }
}
