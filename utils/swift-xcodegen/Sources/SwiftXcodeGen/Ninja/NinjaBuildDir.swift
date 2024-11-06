//===--- NinjaBuildDir.swift ----------------------------------------------===//
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

public final class NinjaBuildDir: Sendable {
  public let path: AbsolutePath
  public let projectRootDir: AbsolutePath
  public let tripleSuffix: String

  private let repoBuildDirs = MutexBox<[Repo: RepoBuildDir]>()

  private static func detectTripleSuffix(
    buildDir: AbsolutePath
  ) throws -> String {
    for dir in try buildDir.getDirContents() {
      guard buildDir.appending(dir).isDirectory,
            let triple = dir.fileName.tryDropPrefix("swift-") else {
        continue
      }
      return triple
    }
    let couldBeParent = buildDir.fileName.hasPrefix("swift-")
    throw XcodeGenError.noSwiftBuildDir(buildDir, couldBeParent: couldBeParent)
  }
  
  private static func detectProjectRoot(
    buildDir: AbsolutePath
  ) throws -> AbsolutePath {
    guard let parent = buildDir.parentDir else {
      throw XcodeGenError.expectedParent(buildDir)
    }
    guard let projectDir = parent.parentDir else {
      throw XcodeGenError.expectedParent(parent)
    }
    return projectDir
  }
  
  public init(at path: AbsolutePath, projectRootDir: AbsolutePath?) throws {
    guard path.exists else {
      throw XcodeGenError.pathNotFound(path)
    }
    self.path = path
    self.tripleSuffix = try Self.detectTripleSuffix(buildDir: path)
    self.projectRootDir = try projectRootDir ?? Self.detectProjectRoot(buildDir: path)
  }
  
  public func buildDir(for repo: Repo) throws -> RepoBuildDir {
    try repoBuildDirs.withLock { repoBuildDirs in
      if let buildDir = repoBuildDirs[repo] {
        return buildDir
      }
      let dir = try RepoBuildDir(repo, for: self)
      repoBuildDirs[repo] = dir
      return dir
    }
  }
}
