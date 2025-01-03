//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Unicode {

  /// Normalized representations of Unicode text.
  ///
  /// This type exposes `Sequence`s and `AsyncSequence`s which
  /// wrap a source of Unicode scalars and lazily normalize it.
  ///
  /// The `.normalized` property is available on all
  /// `Sequence`s and `AsyncSequence`s where the element type
  /// is `Unicode.Scalar`.
  ///
  /// ```swift
  /// let text = "ṩ" // Latin Small Letter S with Dot Below and Dot Above
  ///
  /// for scalar in text.unicodeScalars.normalized.nfd {
  ///   print(scalar)
  /// }
  ///
  /// // Prints:
  /// // "s"
  /// // "\u{0323}" (Combining Dot Below)
  /// // "\u{0307}" (Combining Dot Above)
  /// ```
  ///
  /// ## Topics
  ///
  /// ### Normal Forms
  ///
  /// - ``NormalizedScalars/nfd``
  /// - ``NormalizedScalars/nfc``
  ///
  @frozen
  public struct NormalizedScalars<Source> {

    public var source: Source

    @inlinable
    public init(_ source: Source) {
      self.source = source
    }
  }
}

extension Sequence where Element == Unicode.Scalar {

  /// Normalized representations of this sequence's contents.
  ///
  /// ```swift
  /// let text = "ṩ" // Latin Small Letter S with Dot Below and Dot Above
  ///
  /// for scalar in text.unicodeScalars.normalized.nfd {
  ///   print(scalar)
  /// }
  ///
  /// // Prints:
  /// // "s"
  /// // "\u{0323}" (Combining Dot Below)
  /// // "\u{0307}" (Combining Dot Above)
  /// ```
  ///
  @inlinable
  public var normalized: Unicode.NormalizedScalars<Self> {
    Unicode.NormalizedScalars(self)
  }
}
