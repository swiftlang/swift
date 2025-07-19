//===--- ProjectSpec.swift ------------------------------------------------===//
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

/// The specification for a project to generate.
public struct ProjectSpec {
  public var name: String
  public var buildDir: RepoBuildDir
  public var runnableBuildDir: RepoBuildDir

  /// Whether to include Clang targets.
  public var addClangTargets: Bool

  /// Whether to include Swift targets.
  public var addSwiftTargets: Bool

  /// Whether to add Swift dependencies to the project.
  public var addSwiftDependencies: Bool

  /// Whether to add targets for runnable executables.
  public var addRunnableTargets: Bool

  /// Whether to add a build target for runnable targets, if false they will
  /// be added as freestanding schemes.
  public var addBuildForRunnableTargets: Bool

  /// Whether to infer build arguments for files that don't have any, based
  /// on the build arguments of surrounding files.
  public var inferArgs: Bool

  /// Whether to prefer using folder references for groups containing non-source
  /// files.
  public var preferFolderRefs: Bool

  /// Whether to enable the use of buildable folders for targets.
  public var useBuildableFolders: Bool

  /// If provided, the paths added will be implicitly appended to this path.
  let mainRepoDir: RelativePath?

  private(set) var clangTargetSources: [ClangTargetSource] = []
  private(set) var swiftTargetSources: [SwiftTargetSource] = []

  private(set) var referencesToAdd: [PathReference] = []
  private(set) var excludedPaths: [ExcludedPath] = []
  private(set) var knownUnbuildables: Set<RelativePath> = []

  public init(
    _ name: String,
    for buildDir: RepoBuildDir,
    runnableBuildDir: RepoBuildDir,
    addClangTargets: Bool,
    addSwiftTargets: Bool,
    addSwiftDependencies: Bool,
    addRunnableTargets: Bool,
    addBuildForRunnableTargets: Bool,
    inferArgs: Bool,
    preferFolderRefs: Bool,
    useBuildableFolders: Bool,
    mainRepoDir: RelativePath? = nil
  ) {
    self.name = name
    self.buildDir = buildDir
    self.runnableBuildDir = runnableBuildDir
    self.addClangTargets = addClangTargets
    self.addSwiftTargets = addSwiftTargets
    self.addSwiftDependencies = addSwiftDependencies
    self.addRunnableTargets = addRunnableTargets
    self.addBuildForRunnableTargets = addBuildForRunnableTargets
    self.inferArgs = inferArgs
    self.preferFolderRefs = preferFolderRefs
    self.useBuildableFolders = useBuildableFolders
    self.mainRepoDir = mainRepoDir
  }

  var repoRoot: AbsolutePath {
    buildDir.repoPath
  }
}

extension ProjectSpec {
  public struct ExcludedPath {
    var path: RelativePath
    var reason: String?
  }

  struct PathReference {
    enum Kind {
      case file, folder
    }
    var kind: Kind
    var path: RelativePath

    static func file(_ path: RelativePath) -> Self {
      .init(kind: .file, path: path)
    }
    static func folder(_ path: RelativePath) -> Self {
      .init(kind: .folder, path: path)
    }

    func withPath(_ newPath: RelativePath) -> Self {
      var result = self
      result.path = newPath
      return result
    }
  }
}

extension ProjectSpec {
  private var mainRepoPath: AbsolutePath {
    // Add the main repo dir if we were asked to.
    if let mainRepoDir {
      repoRoot.appending(mainRepoDir)
    } else {
      repoRoot
    }
  }

  private func mapKnownPath(_ path: RelativePath) -> RelativePath {
    // Add the main repo dir if we were asked to.
    if let mainRepoDir {
      mainRepoDir.appending(path)
    } else {
      path
    }
  }

  private func mapPath(
    _ path: RelativePath,
    for description: String
  ) -> RelativePath? {
    let path = mapKnownPath(path)
    let absPath = repoRoot.appending(path)
    guard absPath.exists else {
      log.warning("Skipping \(description) at '\(absPath)'; does not exist")
      return nil
    }
    return path
  }
}

extension ProjectSpec {
  public mutating func addExcludedPath(
    _ path: RelativePath,
    reason: String? = nil
  ) {
    guard let path = mapPath(path, for: "exclusion") else { return }
    excludedPaths.append(.init(path: path, reason: reason))
  }

  public mutating func addUnbuildableFile(_ path: RelativePath) {
    guard let path = mapPath(path, for: "unbuildable file") else { return }
    self.knownUnbuildables.insert(path)
  }

  public mutating func addReference(to path: RelativePath) {
    guard let path = mapPath(path, for: "file") else { return }
    let isDir = repoRoot.appending(path).isDirectory
    referencesToAdd.append(isDir ? .folder(path) : .file(path))
  }

  public mutating func addHeaders(in path: RelativePath) {
    guard let path = mapPath(path, for: "headers") else { return }
    if preferFolderRefs {
      referencesToAdd.append(.folder(path))
      return
    }
    do {
      for header in try buildDir.getHeaderFilePaths(for: path) {
        referencesToAdd.append(.file(header))
      }
    } catch {
      log.warning("Skipping headers in \(path); '\(error)'")
    }
  }

  public mutating func addTopLevelDocs() {
    do {
      for doc in try mainRepoPath.getDirContents() where doc.isDocLike {
        referencesToAdd.append(.file(mapKnownPath(doc)))
      }
    } catch {
      log.warning("Skipping top-level docs for \(repoRoot); '\(error)'")
    }
  }

  public mutating func addDocsGroup(at path: RelativePath) {
    guard let path = mapPath(path, for: "docs") else { return }
    if preferFolderRefs {
      referencesToAdd.append(.folder(path))
      return
    }
    do {
      for doc in try buildDir.getAllRepoSubpaths(of: path) where doc.isDocLike {
        referencesToAdd.append(.file(doc))
      }
    } catch {
      log.warning("Skipping docs in \(path); '\(error)'")
    }
  }

  public mutating func addClangTarget(
    at path: RelativePath,
    named name: String? = nil,
    mayHaveUnbuildableFiles: Bool = false
  ) {
    guard addClangTargets else { return }
    guard let path = mapPath(path, for: "Clang target") else { return }
    let name = name ?? path.fileName
    clangTargetSources.append(
      ClangTargetSource(
        at: path,
        named: name,
        mayHaveUnbuildableFiles: mayHaveUnbuildableFiles
      )
    )
  }

  public mutating func addClangTargets(
    below path: RelativePath,
    addingPrefix prefix: String? = nil,
    mayHaveUnbuildableFiles: Bool = false,
    excluding excludedChildren: Set<RelativePath> = []
  ) {
    guard addClangTargets else { return }
    let originalPath = path
    guard let path = mapPath(path, for: "Clang targets") else { return }
    let absPath = repoRoot.appending(path)
    do {
      for child in try absPath.getDirContents()
      where !excludedChildren.contains(child) {
        guard absPath.appending(child).isDirectory else {
          continue
        }
        var name = child.fileName
        if let prefix = prefix {
          name = prefix + name
        }
        addClangTarget(
          at: originalPath.appending(child),
          named: name,
          mayHaveUnbuildableFiles: mayHaveUnbuildableFiles
        )
      }
    } catch {
      log.warning("Skipping Clang targets in \(path); '\(error)'")
    }
  }

  public mutating func addSwiftTargets(
    below path: RelativePath
  ) {
    guard addSwiftTargets else { return }
    guard let path = mapPath(path, for: "Swift targets") else { return }
    swiftTargetSources.append(SwiftTargetSource(below: path))
  }
}
