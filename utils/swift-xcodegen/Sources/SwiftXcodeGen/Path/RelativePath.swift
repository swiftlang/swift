//===--- RelativePath.swift -----------------------------------------------===//
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
import System

public struct RelativePath: PathProtocol, Sendable {
  public let storage: FilePath
  public init(_ storage: FilePath) {
    precondition(
      storage.isRelative, "Expected '\(storage)' to be a relative path"
    )
    self.storage = storage.lexicallyNormalized()
  }

  private init(normalizedComponents: FilePath.ComponentView.SubSequence) {
    // Already normalized, no need to do it ourselves.
    self.storage = FilePath(root: nil, normalizedComponents)
  }

  public var asAnyPath: AnyPath {
    .relative(self)
  }
}

public extension RelativePath {
  var absoluteInWorkingDir: AbsolutePath {
    .init(FileManager.default.currentDirectoryPath).appending(self)
  }

  func absolute(in base: AbsolutePath) -> AbsolutePath {
    precondition(base.isDirectory, "Expected '\(base)' to be a directory")
    return base.appending(self)
  }

  init(_ component: Component) {
    self.init(FilePath(root: nil, components: component))
  }

  /// Incrementally stacked components of the path, starting at the parent.
  /// e.g for a/b/c, returns [a, a/b, a/b/c].
  @inline(__always)
  var stackedComponents: [RelativePath] {
    let components = self.components
    var stackedComponents: [RelativePath] = []
    var index = components.startIndex
    while index != components.endIndex {
      stackedComponents.append(
        RelativePath(normalizedComponents: components[...index])
      )
      components.formIndex(after: &index)
    }
    return stackedComponents
  }
}

extension RelativePath: ExpressibleByStringLiteral, ExpressibleByStringInterpolation {
  public init(stringLiteral value: String) {
    self.init(value)
  }
}

extension RelativePath: Decodable {
  public init(from decoder: Decoder) throws {
    self.init(try decoder.singleValueContainer().decode(String.self))
  }
}
