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
/// to a particular file can be queried, with common arguments associated
/// with a common parent.
struct CommandArgTree {
  private var storage: [RelativePath: Set<Command.Argument>]

  init() {
    self.storage = [:]
  }

  mutating func insert(_ args: [Command.Argument], for path: RelativePath) {
    let args = Set(args)
    for component in path.stackedComponents {
      // If we haven't added any arguments, add them. If we're adding arguments
      // for the file itself, this is the only way we'll add arguments,
      // otherwise we can form an intersection with the other arguments.
      let inserted = storage.insertValue(args, for: component)
      guard !inserted && component != path else { continue }

      // We use subscript(_:default:) to mutate in-place without CoW.
      storage[component, default: []].formIntersection(args)
    }
  }

  /// Retrieve the arguments at a given path, including those in the parent.
  func getArgs(for path: RelativePath) -> Set<Command.Argument> {
    storage[path] ?? []
  }

  /// Retrieve the arguments at a given path, excluding those already covered
  /// by a given parent.
  func getUniqueArgs(
    for path: RelativePath, parent: RelativePath
  ) -> Set<Command.Argument> {
    getArgs(for: path).subtracting(getArgs(for: parent))
  }

  /// Whether the given path has any unique args not covered by `parent`.
  func hasUniqueArgs(for path: RelativePath, parent: RelativePath) -> Bool {
    let args = getArgs(for: path)
    guard !args.isEmpty else { return false }
    // Assuming `parent` is an ancestor of path, the arguments for parent is
    // guaranteed to be a subset of the arguments for `path`. As such, we
    // only have to compare sizes here.
    return args.count != getArgs(for: parent).count
  }
}
