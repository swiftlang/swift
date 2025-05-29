//===--- DirectoryCache.swift ---------------------------------------------===//
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

import Foundation

/// A simple cache for the recursive contents of a directory under a given
/// root path. This is pretty basic and doesn't handle cases where we've already
/// cached the parent.
struct DirectoryCache {
  private let root: AbsolutePath
  private let storage = MutexBox<[RelativePath: [RelativePath]]>()

  init(root: AbsolutePath) {
    self.root = root
  }

  func getAllSubpaths(of path: RelativePath) throws -> [RelativePath] {
    if let result = storage.withLock(\.[path]) {
      return result
    }
    let absPath = root.appending(path).rawPath
    let result = try FileManager.default.subpathsOfDirectory(atPath: absPath)
      .map { path.appending($0) }
    storage.withLock { storage in
      storage[path] = result
    }
    return result
  }
}
