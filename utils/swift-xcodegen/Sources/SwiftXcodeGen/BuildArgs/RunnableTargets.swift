//===--- RunnableTargets.swift --------------------------------------------===//
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

/// A target that defines a runnable executable.
struct RunnableTarget: Hashable {
  var name: String
  var ninjaTargetName: String
  var path: AbsolutePath
}

struct RunnableTargets {
  private var addedPaths: Set<RelativePath> = []
  private var targets: [RunnableTarget] = []

  init(from buildDir: RepoBuildDir) throws {
    for rule in try buildDir.ninjaFile.buildEdges {
      tryAddTarget(rule, buildDir: buildDir)
    }
  }
}

extension RunnableTargets: RandomAccessCollection {
  typealias Element = RunnableTarget
  typealias Index = Int

  var startIndex: Int { targets.startIndex }
  var endIndex: Int { targets.endIndex }

  func index(_ i: Int, offsetBy distance: Int) -> Int {
    targets.index(i, offsetBy: distance)
  }

  subscript(position: Int) -> RunnableTarget {
    targets[position]
  }
}

extension RunnableTargets {
  private func getRunnablePath(
    for outputs: [String]
  ) -> (String, RelativePath)? {
    // We're only interested in rules with the path 'bin/<executable>'.
    for output in outputs {
      // https://github.com/swiftlang/swift-format/issues/1037
      // swift-format-ignore
      guard case let .relative(r) = AnyPath(output),
            r.components.count == 2, r.components.first == "bin"
      else { return nil }
      return (output, r)
    }
    return nil
  }

  private mutating func tryAddTarget(
    _ rule: NinjaBuildFile.BuildEdge,
    buildDir: RepoBuildDir
  ) {
    // https://github.com/swiftlang/swift-format/issues/1037
    // swift-format-ignore
    guard let (name, path) = getRunnablePath(for: rule.outputs),
          addedPaths.insert(path).inserted
    else { return }

    let absPath = buildDir.path.appending(path)
    guard absPath.exists, absPath.isExecutable else { return }

    let target = RunnableTarget(
      name: path.fileName,
      ninjaTargetName: name,
      path: absPath
    )
    targets.append(target)
  }
}
