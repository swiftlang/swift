//===--- StringUTF16.swift ------------------------------------------------===//
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

// FIXME(ABI)#71 : The UTF-16 string view should have a custom iterator type to
// allow performance optimizations of linear traversals.

extension String.UTF16View {
  #if !INTERNAL_CHECKS_ENABLED
  @inlinable @inline(__always) internal func _invariantCheck() {}
  #else
  @usableFromInline @inline(never) @_effects(releasenone)
  internal func _invariantCheck() {
    // TODO: Ensure start/end are not sub-scalr UTF-8 transcoded indices
  }
  #endif // INTERNAL_CHECKS_ENABLED
}

extension String.UTF16View: BidirectionalCollection {
  public typealias Index = String.Index

  /// The position of the first code unit if the `String` is
  /// nonempty; identical to `endIndex` otherwise.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    @inline(__always) get { return _guts.startIndex }
  }

  /// The "past the end" position---that is, the position one greater than
  /// the last valid subscript argument.
  ///
  /// In an empty UTF-16 view, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    @inline(__always) get { return _guts.endIndex }
  }

  @inlinable @inline(__always)
  public func index(after i: Index) -> Index {
    // TODO(UTF8) known-ASCII fast path

    if _slowPath(_guts.isForeign) { return _foreignIndex(after: i) }

    // For a BMP scalar (1-3 UTF-8 code units), advance past it. For a non-BMP
    // scalar, use a transcoded offset first.
    let len = _guts.fastUTF8ScalarLength(startingAt: i.encodedOffset)
    if len == 4 && i.transcodedOffset == 0 {
      return Index(transcodedAfter: i)
    }
    return Index(encodedOffset: i.encodedOffset &+ len)
  }

  @inlinable @inline(__always)
  public func index(before i: Index) -> Index {
    precondition(!i.isZeroPosition)

    if _slowPath(_guts.isForeign) { return _foreignIndex(before: i) }

    // TODO(UTF8) known-ASCII fast path

    if i.transcodedOffset != 0 {
      _sanityCheck(i.transcodedOffset == 1)
      return Index(encodedOffset: i.encodedOffset)
    }

    let len = _guts.fastUTF8ScalarLength(endingAt: i.encodedOffset)
    if len == 4 {
      // 2 UTF-16 code units comprise this scalar; advance to the beginning and
      // start mid-scalar transcoding
      return Index(
        encodedOffset: i.encodedOffset &- len,
        transcodedOffset: 1)
    }

    // Single UTF-16 code unit
    _sanityCheck((1...3) ~= len)
    return Index(encodedOffset: i.encodedOffset &- len)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n)
    }

    // TODO(UTF8) known-ASCII fast path

    let lowerOffset = _getOffset(for: i)
    let result = _getIndex(for: lowerOffset + n)
    return result
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n, limitedBy: limit)
    }

    // TODO(UTF8) known-ASCII fast path

    let iOffset = _getOffset(for: i)
    let limitOffset = _getOffset(for: limit)

    // If distance < 0, limit has no effect if it is greater than i.
    if _slowPath(n < 0 && limit <= i && limitOffset > iOffset + n) {
      return nil
    }
    //  If distance > 0, limit has no effect if it is less than i. Likewise,
    if _slowPath(n >= 0 && limit >= i && limitOffset < iOffset + n) {
      return nil
    }

    let result = _getIndex(for: iOffset + n)
    return result
  }

  public func distance(from start: Index, to end: Index) -> Int {
    if _slowPath(_guts.isForeign) {
      return _foreignDistance(from: start, to: end)
    }

    // TODO(UTF8) known-ASCII fast paths

    let lower = _getOffset(for: start)
    let upper = _getOffset(for: end)
    return upper &- lower
  }

  @inlinable
  public var count: Int {
    if _slowPath(_guts.isForeign) {
      return _foreignCount()
    }
    return _getOffset(for: endIndex)
  }

  /// Accesses the code unit at the given position.
  ///
  /// The following example uses the subscript to print the value of a
  /// string's first UTF-16 code unit.
  ///
  ///     let greeting = "Hello, friend!"
  ///     let i = greeting.utf16.startIndex
  ///     print("First character's UTF-16 code unit: \(greeting.utf16[i])")
  ///     // Prints "First character's UTF-16 code unit: 72"
  ///
  /// - Parameter position: A valid index of the view. `position` must be
  ///   less than the view's end index.
  @inlinable
  public subscript(i: Index) -> UTF16.CodeUnit {
    @inline(__always) get {
      String(_guts)._boundsCheck(i)
      // TODO(UTF8): known-ASCII fast path

      if _fastPath(_guts.isFastUTF8) {
        let scalar = _guts.fastUTF8Scalar(
          startingAt: _guts.scalarAlign(i).encodedOffset)
        if scalar.value <= 0xFFFF {
          return UInt16(truncatingIfNeeded: scalar.value)
        }
        return scalar.utf16[i.transcodedOffset]
      }

      return _foreignSubscript(position: i)
    }
  }
}
extension String.UTF16View: CustomStringConvertible {
 @inlinable
 public var description: String {
   @inline(__always) get { return String(_guts) }
 }
}

extension String.UTF16View: CustomDebugStringConvertible {
 public var debugDescription: String {
   return "StringUTF16(\(self.description.debugDescription))"
 }
}

extension String {
  /// A UTF-16 encoding of `self`.
  @inlinable // FIXME(sil-serialize-all)
  public var utf16: UTF16View {
    @inline(__always) get { return UTF16View(_guts) }
    @inline(__always) set { self = String(newValue._guts) }
  }

  /// Creates a string corresponding to the given sequence of UTF-16 code units.
  @inlinable @inline(__always)
  @available(swift, introduced: 4.0)
  public init(_ utf16: UTF16View) {
    self.init(utf16._guts)
  }
}

// Index conversions
extension String.UTF16View.Index {
  /// Creates an index in the given UTF-16 view that corresponds exactly to the
  /// specified string position.
  ///
  /// If the index passed as `sourcePosition` represents either the start of a
  /// Unicode scalar value or the position of a UTF-16 trailing surrogate,
  /// then the initializer succeeds. If `sourcePosition` does not have an
  /// exact corresponding position in `target`, then the result is `nil`. For
  /// example, an attempt to convert the position of a UTF-8 continuation byte
  /// results in `nil`.
  ///
  /// The following example finds the position of a space in a string and then
  /// converts that position to an index in the string's `utf16` view.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let stringIndex = cafe.firstIndex(of: "é")!
  ///     let utf16Index = String.Index(stringIndex, within: cafe.utf16)!
  ///
  ///     print(cafe.utf16[...utf16Index])
  ///     // Prints "Café"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in at least one of the views of the string
  ///     shared by `target`.
  ///   - target: The `UTF16View` in which to find the new position.
  public init?(
    _ idx: String.Index, within target: String.UTF16View
  ) {
    if _slowPath(target._guts.isForeign) {
      guard idx._foreignIsWithin(target) else { return nil }
    } else {
      guard target._guts.isOnUnicodeScalarBoundary(idx) else { return nil }
    }

    self = idx
  }

  /// Returns the position in the given view of Unicode scalars that
  /// corresponds exactly to this index.
  ///
  /// This index must be a valid index of `String(unicodeScalars).utf16`.
  ///
  /// This example first finds the position of a space (UTF-16 code point `32`)
  /// in a string's `utf16` view and then uses this method to find the same
  /// position in the string's `unicodeScalars` view.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.utf16.firstIndex(of: 32)!
  ///     let j = i.samePosition(in: cafe.unicodeScalars)!
  ///     print(cafe.unicodeScalars[..<j])
  ///     // Prints "Café"
  ///
  /// - Parameter unicodeScalars: The view to use for the index conversion.
  ///   This index must be a valid index of at least one view of the string
  ///   shared by `unicodeScalars`.
  /// - Returns: The position in `unicodeScalars` that corresponds exactly to
  ///   this index. If this index does not have an exact corresponding
  ///   position in `unicodeScalars`, this method returns `nil`. For example,
  ///   an attempt to convert the position of a UTF-16 trailing surrogate
  ///   returns `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }
}

// Reflection
extension String.UTF16View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-16 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

// Slicing
extension String.UTF16View {
  public typealias SubSequence = Substring.UTF16View

  public subscript(r: Range<Index>) -> Substring.UTF16View {
    return Substring.UTF16View(self, _bounds: r)
  }
}

// Foreign string support
extension String.UTF16View {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(after i: Index) -> Index {
    _sanityCheck(_guts.isForeign)
    return Index(encodedOffset: i.encodedOffset + 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(before i: Index) -> Index {
    _sanityCheck(_guts.isForeign)
    return Index(encodedOffset: i.encodedOffset - 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignSubscript(position i: Index) -> UTF16.CodeUnit {
    _sanityCheck(_guts.isForeign)
    return _guts.foreignErrorCorrectedUTF16CodeUnit(at: i)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignDistance(from start: Index, to end: Index) -> Int {
    _sanityCheck(_guts.isForeign)
    return end.encodedOffset - start.encodedOffset
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    _sanityCheck(_guts.isForeign)
    let l = limit.encodedOffset - i.encodedOffset
    if n > 0 ? l >= 0 && l < n : l <= 0 && n < l {
      return nil
    }
    return Index(encodedOffset: i.encodedOffset + n)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(_ i: Index, offsetBy n: Int) -> Index {
    _sanityCheck(_guts.isForeign)
    return Index(encodedOffset: i.encodedOffset + n)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignCount() -> Int {
    _sanityCheck(_guts.isForeign)
    return endIndex.encodedOffset - startIndex.encodedOffset
  }
}

extension String.Index {
  @usableFromInline @inline(never) // opaque slow-path
  @_effects(releasenone)
  internal func _foreignIsWithin(_ target: String.UTF16View) -> Bool {
    _sanityCheck(target._guts.isForeign)

    // If we're transcoding, we're a UTF-8 view index, not UTF-16.
    return self.transcodedOffset == 0
  }
}

// Breadcrumb-aware acceleration
extension String.UTF16View {
  // A simple heuristic we can always tweak later. Not needed for correctness
  @inlinable
  internal var _shortHeuristic: Int {  @inline(__always) get { return 32 } }

  @usableFromInline
  @_effects(releasenone)
  internal func _getOffset(for idx: Index) -> Int {
    // Trivial and common: start
    if idx == startIndex { return 0 }

    if idx.encodedOffset < _shortHeuristic || !_guts.hasBreadcrumbs {
      return _distance(from: startIndex, to: idx)
    }

    // Simple and common: endIndex aka `length`.
    let breadcrumbsPtr = _guts.getBreadcrumbsPtr()
    if idx == endIndex { return breadcrumbsPtr.pointee.utf16Length }

    // Otherwise, find the nearest lower-bound breadcrumb and count from there
    let (crumb, crumbOffset) = breadcrumbsPtr.pointee.getBreadcrumb(
      forIndex: idx)
    return crumbOffset + _distance(from: crumb, to: idx)
  }

  @usableFromInline
  @_effects(releasenone)
  internal func _getIndex(for offset: Int) -> Index {
    // Trivial and common: start
    if offset == 0 { return startIndex }

    if offset < _shortHeuristic || !_guts.hasBreadcrumbs {
      return _index(startIndex, offsetBy: offset)
    }

    // Simple and common: endIndex aka `length`.
    let breadcrumbsPtr = _guts.getBreadcrumbsPtr()
    if offset == breadcrumbsPtr.pointee.utf16Length { return endIndex }

    // Otherwise, find the nearest lower-bound breadcrumb and advance that
    let (crumb, remaining) = breadcrumbsPtr.pointee.getBreadcrumb(
      forOffset: offset)
    return _index(crumb, offsetBy: remaining)
  }
}

// TODO(UTF8 perf): Breadcrumb-accelerate:
//   * distance
//   * index(_:offsetBy), index(_:offsetBy:limitedBy:)
//   * count
//
