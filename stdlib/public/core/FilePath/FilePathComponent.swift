/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

extension FilePath {
  /// Represents an individual component of a file path.
  @available(SwiftStdlib 9999, *)
  public struct Component: Sendable {
    internal var _path: FilePath
    internal var _range: Range<_SystemString.Index>
    internal var _verbatimContext: Bool

    internal init(_ path: FilePath, _ range: Range<_SystemString.Index>, verbatimContext: Bool = false) {
      self._path = path
      self._range = range
      self._verbatimContext = verbatimContext
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

extension FilePath.Component: Comparable {
  @available(SwiftStdlib 9999, *)
  public static func < (lhs: FilePath.Component, rhs: FilePath.Component) -> Bool {
    lhs._slice.lexicographicallyPrecedes(rhs._slice)
  }
}

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
    guard !string.utf8.contains(0) else { return nil }
    for scalar in string.unicodeScalars {
      if scalar == "/" { return nil }
      if _isWindows && scalar == "\\" { return nil }
    }
    guard let path = FilePath(string) else { return nil }
    guard path.anchor == nil else { return nil }
    let comps = path.components
    guard comps.count == 1 else { return nil }
    self = comps.first!
  }
}
