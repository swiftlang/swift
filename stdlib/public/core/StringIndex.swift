//===--- StringIndex.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SwiftShims

/*

String's Index has the following layout:

 ┌──────────┬───────────────────╥────────────────┬──────────╥────────────────┐
 │ b63:b16  │      b15:b14      ║     b13:b8     │  b7:b1   ║       b0       │
 ├──────────┼───────────────────╫────────────────┼──────────╫────────────────┤
 │ position │ transcoded offset ║ grapheme cache │ reserved ║ scalar aligned │
 └──────────┴───────────────────╨────────────────┴──────────╨────────────────┘
                                └──────── resilient ────────┘

Position, transcoded offset, and scalar aligned are fully exposed in the ABI.
Grapheme cache and reserved are partially resilient: the fact that there are 13
bits with a default value of `0` is ABI, but not the layout, construction, or
interpretation of those bits. All use of grapheme cache should be behind
non-inlinable function calls. Inlinable code should not set a non-zero value to
grapheme cache bits: doing so breaks back deployment as they will be interpreted
as a set cache.

- position aka `encodedOffset`: A 48-bit offset into the string's code units

- transcoded offset: a 2-bit sub-scalar offset, derived from transcoding

<resilience barrier>

- grapheme cache: A 6-bit value remembering the distance to the next grapheme
boundary.

- reserved: 7-bit for future use.

<resilience barrier>

- scalar aligned, whether this index is known to be scalar-aligned (see below)


*/
extension String {
  /// A position of a character or code unit in a string.
  @frozen
  public struct Index {
    @usableFromInline
    internal var _rawBits: UInt64

    @inlinable @inline(__always)
    init(_ raw: UInt64) {
      self._rawBits = raw
      self._invariantCheck()
    }
  }
}

extension String.Index {
  @inlinable @inline(__always)
  internal var orderingValue: UInt64 { return _rawBits &>> 14 }

  // Whether this is at the canonical "start" position, that is encoded AND
  // transcoded offset of 0.
  @inlinable @inline(__always)
  internal var isZeroPosition: Bool { return orderingValue == 0 }

  /// The UTF-16 code unit offset corresponding to this Index
  public func utf16Offset<S: StringProtocol>(in s: S) -> Int {
    return s.utf16.distance(from: s.utf16.startIndex, to: self)
  }

  /// The offset into a string's code units for this index.
  @available(swift, deprecated: 4.2, message: """
    encodedOffset has been deprecated as most common usage is incorrect. \
    Use utf16Offset(in:) to achieve the same behavior.
    """)
  @inlinable
  public var encodedOffset: Int { return _encodedOffset }

  @inlinable @inline(__always)
  internal var _encodedOffset: Int {
    return Int(truncatingIfNeeded: _rawBits &>> 16)
  }

  @inlinable @inline(__always)
  internal var transcodedOffset: Int {
    return Int(truncatingIfNeeded: orderingValue & 0x3)
  }

  @usableFromInline
  internal var characterStride: Int? {
    let value = (_rawBits & 0x3F00) &>> 8
    return value > 0 ? Int(truncatingIfNeeded: value) : nil
  }

  @inlinable @inline(__always)
  internal init(encodedOffset: Int, transcodedOffset: Int) {
    let pos = UInt64(truncatingIfNeeded: encodedOffset)
    let trans = UInt64(truncatingIfNeeded: transcodedOffset)
    _internalInvariant(pos == pos & 0x0000_FFFF_FFFF_FFFF)
    _internalInvariant(trans <= 3)

    self.init((pos &<< 16) | (trans &<< 14))
  }

  /// Creates a new index at the specified UTF-16 code unit offset
  ///
  /// - Parameter offset: An offset in UTF-16 code units.
  public init<S: StringProtocol>(utf16Offset offset: Int, in s: S) {
    let (start, end) = (s.utf16.startIndex, s.utf16.endIndex)
    guard offset >= 0,
          let idx = s.utf16.index(start, offsetBy: offset, limitedBy: end)
    else {
      self = end.nextEncoded
      return
    }
    self = idx
  }

  /// Creates a new index at the specified code unit offset.
  ///
  /// - Parameter offset: An offset in code units.
  @available(swift, deprecated: 4.2, message: """
    encodedOffset has been deprecated as most common usage is incorrect. \
    Use String.Index(utf16Offset:in:) to achieve the same behavior.
    """)
  @inlinable
  public init(encodedOffset offset: Int) {
    self.init(_encodedOffset: offset)
  }

  @inlinable @inline(__always)
  internal init(_encodedOffset offset: Int) {
    self.init(encodedOffset: offset, transcodedOffset: 0)
  }

  @usableFromInline
  internal init(
    encodedOffset: Int, transcodedOffset: Int, characterStride: Int
  ) {
    self.init(encodedOffset: encodedOffset, transcodedOffset: transcodedOffset)
    if _slowPath(characterStride > 0x3F) { return }
    self._rawBits |= UInt64(truncatingIfNeeded: characterStride &<< 8)
    self._invariantCheck()
  }

  @usableFromInline
  internal init(encodedOffset pos: Int, characterStride char: Int) {
    self.init(encodedOffset: pos, transcodedOffset: 0, characterStride: char)
  }

  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    _internalInvariant(_encodedOffset >= 0)
    if self._isScalarAligned {
      _internalInvariant_5_1(transcodedOffset == 0)
    }
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Creation helpers, which will make migration easier if we decide to use and
// propagate the reserved bits.
extension String.Index {
  @inlinable @inline(__always)
  internal var strippingTranscoding: String.Index {
    return String.Index(_encodedOffset: self._encodedOffset)
  }

  @inlinable @inline(__always)
  internal var nextEncoded: String.Index {
    _internalInvariant(self.transcodedOffset == 0)
    return String.Index(_encodedOffset: self._encodedOffset &+ 1)
  }

  @inlinable @inline(__always)
  internal var priorEncoded: String.Index {
    _internalInvariant(self.transcodedOffset == 0)
    return String.Index(_encodedOffset: self._encodedOffset &- 1)
  }

  @inlinable @inline(__always)
  internal var nextTranscoded: String.Index {
    return String.Index(
      encodedOffset: self._encodedOffset,
      transcodedOffset: self.transcodedOffset &+ 1)
  }

  @inlinable @inline(__always)
  internal var priorTranscoded: String.Index {
    return String.Index(
      encodedOffset: self._encodedOffset,
      transcodedOffset: self.transcodedOffset &- 1)
  }


  // Get an index with an encoded offset relative to this one.
  // Note: strips any transcoded offset.
  @inlinable @inline(__always)
  internal func encoded(offsetBy n: Int) -> String.Index {
    return String.Index(_encodedOffset: self._encodedOffset &+ n)
  }

  @inlinable @inline(__always)
  internal func transcoded(withOffset n: Int) -> String.Index {
    _internalInvariant(self.transcodedOffset == 0)
    return String.Index(encodedOffset: self._encodedOffset, transcodedOffset: n)
  }
}

/*
  Index Scalar Alignment

  SE-0180 unifies the Index type of String and all its views and allows
  non-scalar-aligned indices to be used across views. In order to guarantee
  behavior, we often have to check and perform scalar alignment. To speed up
  these checks, we allocate a bit denoting known-to-be-scalar-aligned, so that
  the alignment check can skip the load. The below shows what views need to
  check for alignment before they can operate, and whether the indices they
  produce are aligned.

  ┌───────────────╥───────────────────────────┬─────────────────────────┐
  │ View          ║ Requires Scalar Alignment │ Produces Scalar Aligned │
  ╞═══════════════╬═══════════════════════════╪═════════════════════════╡
  │ Native UTF8   ║ no                        │ no                      │
  ├───────────────╫───────────────────────────┼─────────────────────────┤
  │ Native UTF16  ║ yes                       │ no                      │
  ╞═══════════════╬═══════════════════════════╪═════════════════════════╡
  │ Foreign UTF8  ║ yes                       │ no                      │
  ├───────────────╫───────────────────────────┼─────────────────────────┤
  │ Foreign UTF16 ║ no                        │ no                      │
  ╞═══════════════╬═══════════════════════════╪═════════════════════════╡
  │ UnicodeScalar ║ yes                       │ yes                     │
  ├───────────────╫───────────────────────────┼─────────────────────────┤
  │ Character     ║ yes                       │ yes                     │
  └───────────────╨───────────────────────────┴─────────────────────────┘

  The "requires scalar alignment" applies to any operation taking a String.Index
  that's not defined entirely in terms of other operations taking a
  String.Index. These include:

  * index(after:)
  * index(before:)
  * subscript
  * distance(from:to:) (since `to` is compared against directly)
  * UTF16View._nativeGetOffset(for:)

*/
extension String.Index {
  @_alwaysEmitIntoClient // Swift 5.1
  @inline(__always)
  internal var _isScalarAligned: Bool { return 0 != _rawBits & 0x1 }

  @_alwaysEmitIntoClient // Swift 5.1
  @inline(__always)
  internal var _scalarAligned: String.Index {
    var idx = self
    idx._rawBits |= 0x1
    idx._invariantCheck()
    return idx
  }
}

extension String.Index: Equatable {
  @inlinable @inline(__always)
  public static func == (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs.orderingValue == rhs.orderingValue
  }
}

extension String.Index: Comparable {
  @inlinable @inline(__always)
  public static func < (lhs: String.Index, rhs: String.Index) -> Bool {
    return lhs.orderingValue < rhs.orderingValue
  }
}

extension String.Index: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(orderingValue)
  }
}
