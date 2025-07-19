//===--- CommandArgTree.swift ---------------------------------------------===//
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

/// A tree of compile command arguments, indexed by path such that those unique
/// to a particular file can be queried, with common-prefixed arguments
/// associated with a common parent.
struct CommandArgTree {
  private var storage: [RelativePath: [Command.Argument]]

  init() {
    self.storage = [:]
  }

  mutating func insert(_ args: [Command.Argument], for path: RelativePath) {
    for component in path.stackedComponents {
      // If we haven't added any arguments, add them. If we're adding arguments
      // for the file itself, this is the only way we'll add arguments,
      // otherwise we can form a common prefix with the other arguments.
      let inserted = storage.insertValue(args, for: component)
      guard !inserted && component != path else { continue }

      // We use withValue(for:default:) to mutate in-place without CoW.
      storage.withValue(for: component, default: []) { existingArgs in
        let slice = existingArgs.commonPrefix(with: args)
        existingArgs.removeSubrange(slice.endIndex...)
      }
    }
  }

  /// Retrieve the arguments at a given path, including those in the parent.
  func getArgs(for path: RelativePath) -> [Command.Argument] {
    storage[path] ?? []
  }

  /// Retrieve the arguments at a given path, excluding those already covered
  /// by a given parent.
  func getUniqueArgs(
    for path: RelativePath,
    parent: RelativePath
  ) -> [Command.Argument] {
    let childArgs = getArgs(for: path)
    let parentArgs = getArgs(for: parent)
    return Array(childArgs[parentArgs.count...])
  }
}
