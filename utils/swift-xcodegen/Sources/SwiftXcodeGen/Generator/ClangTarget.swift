//===--- ClangTarget.swift ------------------------------------------------===//
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

struct ClangTarget {
  var name: String
  var parentPath: RelativePath
  var sources: [RelativePath]
  var unbuildableSources: [RelativePath]
  var headers: [RelativePath]

  init(
    name: String, parentPath: RelativePath,
    sources: [RelativePath], unbuildableSources: [RelativePath] = [],
    headers: [RelativePath]
  ) {
    self.name = name
    self.parentPath = parentPath
    self.sources = sources
    self.unbuildableSources = unbuildableSources
    self.headers = headers
  }
}

extension RepoBuildDir {
  func getClangSourceFilePaths(for path: RelativePath) throws -> [RelativePath] {
    try getAllRepoSubpaths(of: path).filter(\.isClangSource)
  }

  func getHeaderFilePaths(for path: RelativePath) throws -> [RelativePath] {
    try getAllRepoSubpaths(of: path).filter(\.isHeaderLike)
  }

  func getClangTarget(
    for target: ClangTargetSource, knownUnbuildables: Set<RelativePath>
  ) throws -> ClangTarget? {
    let path = target.path
    let name = target.name

    let sourcePaths = try getClangSourceFilePaths(for: path)
    let headers = try getHeaderFilePaths(for: path)
    if sourcePaths.isEmpty && headers.isEmpty {
      return nil
    }

    var sources: [RelativePath] = []
    var unbuildableSources: [RelativePath] = []
    for path in sourcePaths {
      // If we have no arguments, or have a known unbuildable, treat as not
      // buildable. We'll still include it in the project, but in a separate
      // target that isn't built by default.
      if try !clangArgs.hasBuildArgs(for: path) ||
          knownUnbuildables.contains(path) {
        unbuildableSources.append(path)
        continue
      }
      // If we have no '.o' present for a given file, assume it's not buildable.
      // The 'mayHaveUnbuildableFiles' condition is really only used here to
      // reduce IO and only check targets we know are problematic.
      if target.mayHaveUnbuildableFiles,
          try !clangArgs.isObjectFilePresent(for: path) {
        log.debug("! Treating '\(path)' as unbuildable; no '.o' file")
        unbuildableSources.append(path)
        continue
      }
      sources.append(path)
    }

    return ClangTarget(
      name: name, parentPath: path, sources: sources,
      unbuildableSources: unbuildableSources, headers: headers
    )
  }
}
