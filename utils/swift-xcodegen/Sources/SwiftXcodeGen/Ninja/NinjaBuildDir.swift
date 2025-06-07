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
  private let _tripleSuffix: Result<String, Error>

  private let repoBuildDirs = MutexBox<[Repo: RepoBuildDir]>()

  private static func detectTripleSuffix(
    buildDir: AbsolutePath
  ) -> Result<String, Error> {
    Result {
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
  }

  public var tripleSuffix: String {
    get throws {
      try _tripleSuffix.get()
    }
  }

  // We can infer the project root from the location of swift-xcodegen itself.
  //                      1     2         3          4           5         6      7
  // #filePath = <root>/swift/utils/swift-xcodegen/Sources/SwiftXcodeGen/Ninja/NinjaBuildDir.swift
  private static let inferredProjectRootPath = AbsolutePath(#filePath).dropLast(7)

  private static func detectProjectRoot() throws -> AbsolutePath {
    let inferredSwiftPath = inferredProjectRootPath.appending(Repo.swift.relativePath)
    guard inferredSwiftPath.exists else {
      throw XcodeGenError.couldNotInferProjectRoot(
        reason: "expected swift repo at '\(inferredSwiftPath)'"
      )
    }
    return inferredProjectRootPath
  }
  
  public init(at path: AbsolutePath, projectRootDir: AbsolutePath?) throws {
    guard path.exists else {
      throw XcodeGenError.pathNotFound(path)
    }
    self.path = path
    self._tripleSuffix = Self.detectTripleSuffix(buildDir: path)
    self.projectRootDir = try projectRootDir ?? Self.detectProjectRoot()
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
