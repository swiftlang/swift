//===--- RepoBuildDir.swift -----------------------------------------------===//
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

public final class RepoBuildDir: Sendable {
  public let projectRootDir: AbsolutePath
  public let repo: Repo
  public let path: AbsolutePath
  public let repoPath: AbsolutePath
  public let repoRelativePath: RelativePath

  private let repoDirCache: DirectoryCache

  private let _ninjaFile = MutexBox<NinjaBuildFile?>()
  private let _runnableTargets = MutexBox<RunnableTargets?>()
  private let _clangArgs = MutexBox<ClangBuildArgsProvider?>()
  private let _swiftTargets = MutexBox<SwiftTargets?>()

  init(_ repo: Repo, for parent: NinjaBuildDir) throws {
    self.projectRootDir = parent.projectRootDir
    self.repo = repo
    self.path =
      try repo.buildDirPrefix.map { prefix in
        parent.path.appending("\(prefix)-\(try parent.tripleSuffix)")
      } ?? parent.path
    self.repoRelativePath = repo.relativePath
    self.repoPath = projectRootDir.appending(repo.relativePath)
    self.repoDirCache = DirectoryCache(root: repoPath)

    guard self.path.exists else {
      throw XcodeGenError.pathNotFound(self.path)
    }
    guard self.repoPath.exists else {
      throw XcodeGenError.pathNotFound(self.repoPath)
    }
  }
}

extension RepoBuildDir {
  var clangArgs: ClangBuildArgsProvider {
    get throws {
      try _clangArgs.withLock { _clangArgs in
        if let clangArgs = _clangArgs {
          return clangArgs
        }
        let clangArgs = try ClangBuildArgsProvider(for: self)
        _clangArgs = clangArgs
        return clangArgs
      }
    }
  }

  var swiftTargets: SwiftTargets {
    get throws {
      try _swiftTargets.withLock { _swiftTargets in
        if let swiftTargets = _swiftTargets {
          return swiftTargets
        }
        let swiftTargets = try SwiftTargets(for: self)
        _swiftTargets = swiftTargets
        return swiftTargets
      }
    }
  }

  var ninjaFile: NinjaBuildFile {
    get throws {
      try _ninjaFile.withLock { _ninjaFile in
        if let ninjaFile = _ninjaFile {
          return ninjaFile
        }
        let fileName = path.appending("build.ninja")
        guard fileName.exists else {
          throw XcodeGenError.pathNotFound(fileName)
        }

        log.debug("[*] Reading '\(fileName)'")
        let ninjaFile = try NinjaParser.parse(filePath: fileName)
        _ninjaFile = ninjaFile
        return ninjaFile
      }
    }
  }

  var runnableTargets: RunnableTargets {
    get throws {
      try _runnableTargets.withLock { _runnableTargets in
        if let runnableTargets = _runnableTargets {
          return runnableTargets
        }

        let runnableTargets = try RunnableTargets(from: self)
        _runnableTargets = runnableTargets
        return runnableTargets
      }
    }
  }

  public var buildConfiguration: BuildConfiguration? {
    get throws {
      try ninjaFile.buildConfiguration
    }
  }

  func getAllRepoSubpaths(of parent: RelativePath) throws -> [RelativePath] {
    try repoDirCache.getAllSubpaths(of: parent)
  }
}
