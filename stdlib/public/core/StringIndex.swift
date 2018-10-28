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

 ┌──────────┬───────────────────┬────────────────┬──────────┐
 │ b63:b16  │      b15:b14      │     b13:b8     │ b7:b0    │
 ├──────────┼───────────────────┼────────────────┼──────────┤
 │ position │ transcoded offset │ grapheme cache │ reserved │
 └──────────┴───────────────────┴────────────────┴──────────┘

- grapheme cache: A 6-bit value remembering the distance to the next grapheme
boundary
- position aka `encodedOffset`: An offset into the string's code units
- transcoded offset: a sub-scalar offset, derived from transcoding

The use and interpretation of both `reserved` and `grapheme cache` is not part
of Index's ABI; it should be hidden behind non-inlinable calls. However, the
position of the sequence of 14 bits allocated is part of Index's ABI, as well as
the default value being `0`.

*/

extension String.Index {
  @inlinable
  internal var orderingValue: UInt64 {
    @inline(__always) get { return _rawBits &>> 14 }
  }

  // Whether this is at the canonical "start" position, that is encoded AND
  // transcoded offset of 0.
  @inlinable
  internal var isZeroPosition: Bool {
    @inline(__always) get { return orderingValue == 0 }
  }

  /// The offset into a string's code units for this index.
  @inlinable
  public var encodedOffset: Int {
    @inline(__always) get { return Int(truncatingIfNeeded: _rawBits &>> 16) }
  }

  @inlinable
  internal var transcodedOffset: Int {
    @inline(__always) get {
      return Int(truncatingIfNeeded: orderingValue & 0x3)
    }
  }

  @usableFromInline
  internal var characterStride: Int? {
    let value = (_rawBits & 0x3F00) &>> 8
    return value > 0 ? Int(truncatingIfNeeded: value) : nil
  }

  @inlinable @inline(__always)
  internal init(encodedOffset: Int, transcodedOffset: Int) {
#if arch(i386) || arch(arm)
    unimplemented_utf8_32bit()
#else
    _sanityCheck(encodedOffset == encodedOffset & 0x0000_FFFF_FFFF_FFFF)
    _sanityCheck(transcodedOffset <= 3)
    let pos = UInt64(truncatingIfNeeded: encodedOffset)
    let trans = UInt64(truncatingIfNeeded: transcodedOffset)

    self.init((pos &<< 16) | (trans &<< 14))
#endif
  }

  /// Creates a new index at the specified code unit offset.
  ///
  /// - Parameter offset: An offset in code units.
  @inlinable @inline(__always)
  public init(encodedOffset: Int) {
    self.init(encodedOffset: encodedOffset, transcodedOffset: 0)
  }

  @usableFromInline
  internal init(
    encodedOffset: Int, transcodedOffset: Int, characterStride: Int
  ) {
    self.init(encodedOffset: encodedOffset, transcodedOffset: transcodedOffset)
    if _slowPath(characterStride > 63) { return }

    _sanityCheck(characterStride == characterStride & 0x3F)
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
    _sanityCheck(encodedOffset >= 0)
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

// Creation helpers
extension String.Index {
  @inlinable @inline(__always)
  internal init(transcodedAfter i: String.Index) {
    _sanityCheck((0...2) ~= i.transcodedOffset)
    self.init(
      encodedOffset: i.encodedOffset, transcodedOffset: i.transcodedOffset &+ 1)
  }
  @inlinable @inline(__always)
  internal init(transcodedBefore i: String.Index) {
    _sanityCheck((1...3) ~= i.transcodedOffset)
    self.init(
      encodedOffset: i.encodedOffset, transcodedOffset: i.transcodedOffset &- 1)
  }

  @inlinable
  internal var strippingTranscoding: String.Index {
    @inline(__always) get {
      return String.Index(encodedOffset: self.encodedOffset)
    }
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

// TODO(UTF8): restore these to StringIndexConversions.swift
extension String.Index {
  /// Creates an index in the given string that corresponds exactly to the
  /// specified position.
  ///
  /// If the index passed as `sourcePosition` represents the start of an
  /// extended grapheme cluster---the element type of a string---then the
  /// initializer succeeds.
  ///
  /// The following example converts the position of the Unicode scalar `"e"`
  /// into its corresponding position in the string. The character at that
  /// position is the composed `"é"` character.
  ///
  ///     let cafe = "Cafe\u{0301}"
  ///     print(cafe)
  ///     // Prints "Café"
  ///
  ///     let scalarsIndex = cafe.unicodeScalars.firstIndex(of: "e")!
  ///     let stringIndex = String.Index(scalarsIndex, within: cafe)!
  ///
  ///     print(cafe[...stringIndex])
  ///     // Prints "Café"
  ///
  /// If the index passed as `sourcePosition` doesn't have an exact
  /// corresponding position in `target`, the result of the initializer is
  /// `nil`. For example, an attempt to convert the position of the combining
  /// acute accent (`"\u{0301}"`) fails. Combining Unicode scalars do not have
  /// their own position in a string.
  ///
  ///     let nextScalarsIndex = cafe.unicodeScalars.index(after: scalarsIndex)
  ///     let nextStringIndex = String.Index(nextScalarsIndex, within: cafe)
  ///
  ///     print(nextStringIndex)
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in a view of the `target` parameter.
  ///     `sourcePosition` must be a valid index of at least one of the views
  ///     of `target`.
  ///   - target: The string referenced by the resulting index.
  public init?(
    _ sourcePosition: String.Index,
    within target: String
  ) {
    guard target._guts.isOnGraphemeClusterBoundary(sourcePosition) else {
      return nil
    }
    self = sourcePosition
  }

  /// Returns the position in the given UTF-8 view that corresponds exactly to
  /// this index.
  ///
  /// This example first finds the position of the character `"é"`, and then
  /// uses this method find the same position in the string's `utf8` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.firstIndex(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf8)!
  ///         print(Array(cafe.utf8[j...]))
  ///     }
  ///     // Prints "[195, 169]"
  ///
  /// - Parameter utf8: The view to use for the index conversion. This index
  ///   must be a valid index of at least one view of the string shared by
  ///   `utf8`.
  /// - Returns: The position in `utf8` that corresponds exactly to this index.
  ///   If this index does not have an exact corresponding position in `utf8`,
  ///   this method returns `nil`. For example, an attempt to convert the
  ///   position of a UTF-16 trailing surrogate returns `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public func samePosition(
    in utf8: String.UTF8View
    ) -> String.UTF8View.Index? {
    return String.UTF8View.Index(self, within: utf8)
  }

  /// Returns the position in the given UTF-16 view that corresponds exactly to
  /// this index.
  ///
  /// The index must be a valid index of `String(utf16)`.
  ///
  /// This example first finds the position of the character `"é"` and then
  /// uses this method find the same position in the string's `utf16` view.
  ///
  ///     let cafe = "Café"
  ///     if let i = cafe.firstIndex(of: "é") {
  ///         let j = i.samePosition(in: cafe.utf16)!
  ///         print(cafe.utf16[j])
  ///     }
  ///     // Prints "233"
  ///
  /// - Parameter utf16: The view to use for the index conversion. This index
  ///   must be a valid index of at least one view of the string shared by
  ///   `utf16`.
  /// - Returns: The position in `utf16` that corresponds exactly to this
  ///   index. If this index does not have an exact corresponding position in
  ///   `utf16`, this method returns `nil`. For example, an attempt to convert
  ///   the position of a UTF-8 continuation byte returns `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index? {
    return String.UTF16View.Index(self, within: utf16)
  }
}


