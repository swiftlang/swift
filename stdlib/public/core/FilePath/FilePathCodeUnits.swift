/*
 This source file is part of the SE-0529 reference implementation

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift System project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
*/

// MARK: - CodeUnit typealias

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// The type used to represent a "character" in the platform's
  /// native path encoding.
  #if os(Windows)
  @available(SwiftStdlib 9999, *)
  public typealias CodeUnit = UInt16
  #else
  @available(SwiftStdlib 9999, *)
  public typealias CodeUnit = CChar
  #endif

  /// The Unicode encoding corresponding to `CodeUnit`.
  #if os(Windows)
  internal typealias _Encoding = UTF16
  #else
  internal typealias _Encoding = UTF8
  #endif
}

// MARK: - withCodeUnits (C interop)

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// Calls the given closure with a pointer to the path's null-terminated
  /// contents and the number of code units preceding the null terminator.
  /// The pointer is valid only for the duration of the closure, and the
  /// count does not include the null terminator.
  ///
  /// On Windows the pointer is wide (`UnsafePointer<UInt16>`); see
  /// also `String.withCString(encodedAs:_:)`.
  @available(SwiftStdlib 9999, *)
  public func withCodeUnits<Result, E: Error>(
    _ body: (UnsafePointer<FilePath.CodeUnit>, Int) throws(E) -> Result
  ) throws(E) -> Result {
    // Storage is already [FilePath.CodeUnit] with a trailing null, so
    // we can just hand out its base address and the length sans null.
    let storage = _storage.nullTerminatedStorage
    let count = storage.count - 1
    return try unsafe storage.withUnsafeBufferPointer { buf throws(E) in
      try unsafe body(buf.baseAddress!, count)
    }
  }
}

// MARK: - Code unit access (Span)

// The proposal specifies two SEPARATE forms of byte access, both present:
//
//   1. `withCodeUnits(_:)` above — closure-based, hands back a
//      null-terminated `UnsafePointer` plus the code-unit count, for C
//      interop (tracks `String.withCString`). FilePath only.
//   2. `var codeUnits: Span<CodeUnit>` on FilePath, Component, Anchor, and
//      ComponentView, plus `var nullTerminatedCodeUnits` on FilePath — safe,
//      borrowed, direct byte access.
//
// These are NOT subsumed by one another: the Span getters are the proposal's
// Span surface and coexist with the C-interop `withCodeUnits(_:)`.
//
// The Span getters borrow `_SystemString.nullTerminatedStorage` via its
// `.span` (see `_span` / `_nullTerminatedSpan` in FilePathSystemString.swift),
// sub-extracted to each type's byte range. They are computed getters on
// Escapable types, so per SE-0456 the borrow on `self` is inferred and no
// `@_lifetime` annotation is required. Enabled by
// `.enableExperimentalFeature("Lifetimes")` in Package.swift. Span access is
// safe, so the bodies carry no `unsafe` expressions under StrictMemorySafety.

@available(SwiftStdlib 9999, *)
extension FilePath {
  /// A span of the platform code units comprising this path, not
  /// including the null terminator.
  @available(SwiftStdlib 9999, *)
  public var codeUnits: Span<FilePath.CodeUnit> {
    _storage._span
  }

  /// A span of the platform code units comprising this path, including
  /// the trailing null terminator as its final element.
  @available(SwiftStdlib 9999, *)
  public var nullTerminatedCodeUnits: Span<FilePath.CodeUnit> {
    _storage._nullTerminatedSpan
  }

  /// Creates a file path from a span of platform code units.
  ///
  /// The span should not include a null terminator. Returns `nil`
  /// if the span contains `NUL`, which is not a valid path byte
  /// on any supported platform.
  @available(SwiftStdlib 9999, *)
  public init?(codeUnits: Span<CodeUnit>) {
    var chars = [FilePath.CodeUnit]()
    chars.reserveCapacity(codeUnits.count + 1)
    for i in codeUnits.indices {
      let c = codeUnits[i]
      guard c != ._null else { return nil }
      chars.append(c)
    }
    chars.append(._null)
    let str = _SystemString(nullTerminated: chars)
    self.init(normalizing: str)
  }

  // NOTE: The proposal specifies an OutputSpan-based initializer:
  //
  //   public init<E: Error>(
  //     capacity: Int,
  //     initializingCodeUnitsWith initializer:
  //       (inout OutputSpan<FilePath.CodeUnit>) throws(E) -> Void
  //   ) throws(E)
  //
  // OutputSpan requires experimental features not available without
  // compiler flags.  Stubbed until OutputSpan is generally available.
}

@available(SwiftStdlib 9999, *)
extension FilePath.Component {
  /// A span of the platform code units comprising this component.
  @available(SwiftStdlib 9999, *)
  public var codeUnits: Span<FilePath.CodeUnit> {
    _path._storage._nullTerminatedSpan.extracting(_range)
  }

  /// Creates a file path component from a span of platform code units.
  ///
  /// Returns `nil` if the code units are empty, contain `NUL`, or are
  /// otherwise invalid (e.g. contain more than one component).
  @available(SwiftStdlib 9999, *)
  public init?(codeUnits: Span<FilePath.CodeUnit>) {
    guard !codeUnits.isEmpty else { return nil }
    var chars = [FilePath.CodeUnit]()
    chars.reserveCapacity(codeUnits.count)
    for i in codeUnits.indices {
      let c = codeUnits[i]
      guard c != ._null else { return nil }
      chars.append(c)
    }
    let str = _SystemString(chars)
    let path = FilePath(normalizing: str)
    guard path.anchor == nil else { return nil }
    let comps = path.components
    guard comps.count == 1 else { return nil }
    self = comps.first!

    // TODO: what about checking for trailing slash? do we have tests for that?
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.Anchor {
  /// A span of the platform code units comprising this anchor.
  @available(SwiftStdlib 9999, *)
  public var codeUnits: Span<FilePath.CodeUnit> {
    _path._storage._nullTerminatedSpan.extracting(
      _path._storage.startIndex..<_end)
  }
}

@available(SwiftStdlib 9999, *)
extension FilePath.ComponentView {
  /// A span of the platform code units comprising the relative
  /// components portion of the path.
  @available(SwiftStdlib 9999, *)
  public var codeUnits: Span<FilePath.CodeUnit> {
    // The component view spans [_relStart, _relEnd) in the path's storage.
    // Strip a trailing separator (it is suffix, not part of components) —
    // same boundary logic as the former buffer-based stand-in.
    var end = _relEnd
    if end > _relStart
       && isSeparator(_path._storage[_path._storage.index(before: end)]) {
      let (_, relBegin) = _path._storage._parseRoot()
      let sepIdx = _path._storage.index(before: end)
      if sepIdx >= relBegin {
        end = sepIdx
      }
    }
    return _path._storage._nullTerminatedSpan.extracting(_relStart..<end)
  }
}
