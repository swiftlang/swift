/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
*/

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Represents an individual component of a file path.
  @available(SwiftStdlib 9999, *)
  public struct Component: Sendable {
    internal var _path: FilePath
    internal var _range: Range<_SystemString.Index>
    internal var _verbatimContext: Bool

    internal init(_path: FilePath, _range: Range<_SystemString.Index>, _verbatimContext: Bool) {
      self._path = _path
      self._range = _range
      self._verbatimContext = _verbatimContext
    }

    internal var _slice: _SystemString.SubSequence {
      _internalInvariant(_range.lowerBound >= _path._storage.startIndex)
      _internalInvariant(_range.upperBound <= _path._storage.endIndex)
      return _path._storage[_range]
    }

    /// Whether a component is a regular file or directory name, or a special
    /// directory `.` or `..`
    @available(SwiftStdlib 9999, *)
    public enum Kind: Sendable, Equatable {
      case currentDirectory
      case parentDirectory
      case regular
    }

    /// The kind of this component.
    @available(SwiftStdlib 9999, *)
    public var kind: Kind {
      if _verbatimContext { return .regular }
      let s = _slice
      if s.elementsEqual([._dot]) { return .currentDirectory }
      if s.elementsEqual([._dot, ._dot]) { return .parentDirectory }
      return .regular
    }
  }
}

// MARK: - Component Hashable, Comparable, descriptions

@available(SwiftStdlib 9999, *)
extension FilePath.Component: Hashable {
  @available(SwiftStdlib 9999, *)
  public static func == (lhs: FilePath.Component, rhs: FilePath.Component) -> Bool {
    lhs._slice.elementsEqual(rhs._slice)
  }
  @available(SwiftStdlib 9999, *)
  public func hash(into hasher: inout Hasher) {
    for c in _slice {
      hasher.combine(c)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Component: Comparable {
  @available(SwiftStdlib 9999, *)
  public static func < (lhs: FilePath.Component, rhs: FilePath.Component) -> Bool {
    lhs._slice.lexicographicallyPrecedes(rhs._slice)
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Component: CustomStringConvertible, CustomDebugStringConvertible {
  @available(SwiftStdlib 9999, *)
  public var description: String {
    unsafe _slice.withCodeUnits {
      unsafe $0.withMemoryRebound(to: FilePath._Encoding.CodeUnit.self) {
        unsafe String(decoding: $0, as: FilePath._Encoding.self)
      }
    }
  }
  @available(SwiftStdlib 9999, *)
  public var debugDescription: String {
    description.debugDescription
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Component: ExpressibleByStringLiteral {
  /// Creates a file path component from a string literal.
  ///
  /// Precondition: `stringLiteral` is non-empty and contains no `NUL`
  /// or directory separator.
  @available(SwiftStdlib 9999, *)
  public init(stringLiteral: String) {
    guard let c = FilePath.Component(stringLiteral) else {
      fatalError(
        "FilePath.Component string literal must be non-empty"
        + " and must not contain NUL or a directory separator")
    }
    self = c
  }

  /// Creates a file path component from a string.
  ///
  /// Returns `nil` if `string` is empty or contains `NUL` or a
  /// directory separator.
  @available(SwiftStdlib 9999, *)
  public init?(_ string: String) {
    guard !string.isEmpty else { return nil }
    guard let path = FilePath(string) else { return nil }
    self.init(_validating: path)
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Component {
  /// Shared validation behind `init?(_:)` and `init?(codeUnits:)`.
  ///
  /// Succeeds only when `path` is exactly one component with no anchor and
  /// no trailing separator — i.e. a bare component with no embedded *or*
  /// trailing directory separator, matching the contract that a component
  /// contains no separators. So `a/b` (interior) and `a/` (trailing) are
  /// both rejected, as is any anchored input. NUL-rejection has already
  /// happened in `FilePath.init?`; callers funnel through one of those.
  internal init?(_validating path: FilePath) {
    guard path.anchor == nil, !path.hasTrailingSeparator else { return nil }
    let comps = path.components
    guard comps.count == 1 else { return nil }
    self = comps.first!
  }
}
