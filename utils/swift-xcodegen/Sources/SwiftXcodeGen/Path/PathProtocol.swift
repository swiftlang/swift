//===--- PathProtocol.swift -----------------------------------------------===//
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

import System

public protocol PathProtocol: Hashable, CustomStringConvertible {
  var storage: FilePath { get }
  var asAnyPath: AnyPath { get }
  init(_ storage: FilePath)
}

public extension PathProtocol {
  typealias Component = FilePath.Component

  var parentDir: Self? {
    // Remove the last component and check to see if it's empty.
    var result = storage
    guard result.removeLastComponent(), !result.isEmpty else { return nil }
    return Self(result)
  }

  /// Drops the last `n` components, or all components if `n` is greater
  /// than the number of components.
  func dropLast(_ n: Int = 1) -> Self {
    Self(FilePath(root: storage.root, storage.components.dropLast(n)))
  }

  var fileName: String {
    storage.lastComponent?.string ?? ""
  }

  var isEmpty: Bool {
    storage.isEmpty
  }

  func appending(_ relPath: RelativePath) -> Self {
    Self(storage.pushing(relPath.storage))
  }

  func appending(_ str: String) -> Self {
    Self(storage.appending(str))
  }

  func commonAncestor(with other: Self) -> Self {
    precondition(storage.root == other.storage.root)
    var result = [Component]()
    for (comp, otherComp) in zip(components, other.components) {
      guard comp == otherComp else { break }
      result.append(comp)
    }
    return Self(FilePath(root: storage.root, result))
  }

  /// Attempt to remove `other` as a prefix of `self`, or `nil` if `other` is
  /// not a prefix of `self`.
  func removingPrefix(_ other: Self) -> RelativePath? {
    var result = storage
    guard result.removePrefix(other.storage) else { return nil }
    return RelativePath(result)
  }

  func hasExtension(_ exts: FileExtension...) -> Bool {
    // Note that querying `.extension` involves re-parsing, so only do it
    // once here.
    guard let pathExt = storage.extension else { return false }
    return exts.contains(where: { $0.matches(pathExt) })
  }

  func starts(with other: Self) -> Bool {
    self.storage.starts(with: other.storage)
  }

  var components: FilePath.ComponentView {
    storage.components
  }

  var description: String { storage.string }

  init(stringLiteral value: String) {
    self.init(value)
  }

  init(_ rawPath: String) {
    self.init(FilePath(rawPath))
  }

  var rawPath: String {
    storage.string
  }

  func escaped(addQuotesIfNeeded: Bool) -> String {
    rawPath.escaped(addQuotesIfNeeded: addQuotesIfNeeded)
  }

  var escaped: String {
    rawPath.escaped
  }
}

extension PathProtocol {
  /// Whether this is a .swift.gyb file.
  var isSwiftGyb: Bool {
    hasExtension(.gyb) && rawPath.dropLast(4).hasExtension(.swift)
  }

  var isHeaderLike: Bool {
    if hasExtension(.h, .def, .td, .modulemap) {
      return true
    }
    // Consider all gyb files to be header-like, except .swift.gyb, which
    // will be handled separately when creating Swift targets.
    if hasExtension(.gyb) && !isSwiftGyb {
      return true
    }
    return false
  }

  var isClangSource: Bool {
    hasExtension(.c, .cpp, .m, .mm)
  }

  var isSourceLike: Bool {
    isClangSource || hasExtension(.swift)
  }

  /// Checks whether this file a source file that should be excluded from
  /// any generated targets.
  var isExcludedSource: Bool {
    // We don't get useful build arguments for these.
    hasExtension(.asm, .s, .cc, .cl, .inc, .proto)
  }

  var isDocLike: Bool {
    hasExtension(.md, .rst) || fileName.starts(with: "README")
  }
}

extension Collection where Element: PathProtocol {
  /// Computes the common parent for a collection of paths. If there is only
  /// a single unique path, this returns the parent for that path.
  var commonAncestor: Element? {
    guard let first = self.first else { return nil }
    let result = dropFirst().reduce(first, { $0.commonAncestor(with: $1) })
    return result == first ? result.parentDir : result
  }
}

extension StringProtocol {
  func hasExtension(_ exts: FileExtension...) -> Bool {
    guard let pathExt = FilePath(String(self)).extension else { return false }
    return exts.contains(where: { $0.matches(pathExt) })
  }
}

extension FileExtension {
  func matches(_ extStr: String) -> Bool {
    rawValue.compare(extStr, options: .caseInsensitive) == .orderedSame
  }
}
