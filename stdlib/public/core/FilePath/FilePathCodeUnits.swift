/*
 This source file is part of the Swift.org open source project

 Copyright (c) 2020 - 2026 Apple Inc. and the Swift project authors
 Licensed under Apache License v2.0 with Runtime Library Exception

 See https://swift.org/LICENSE.txt for license information
 See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
    self.init(_normalizing: str)
  }

  // TODO: Add the init

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
    guard let path = FilePath(codeUnits: codeUnits) else { return nil }
    self.init(_validating: path)
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
    // The component view spans `[_relStart, _relEnd)`. By construction
    // `_relEnd` excludes any structural suffix (trailing separator on the
    // relative region, or a Darwin resource-fork suffix), so this range
    // is exactly the components-region bytes.
    _path._storage._nullTerminatedSpan.extracting(_relStart..<_relEnd)
  }
}
