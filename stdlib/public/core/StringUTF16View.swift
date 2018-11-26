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

extension String {
  /// A view of a string's contents as a collection of UTF-16 code units.
  ///
  /// You can access a string's view of UTF-16 code units by using its `utf16`
  /// property. A string's UTF-16 view encodes the string's Unicode scalar
  /// values as 16-bit integers.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.utf16 {
  ///         print(v)
  ///     }
  ///     // 70
  ///     // 108
  ///     // 111
  ///     // 119
  ///     // 101
  ///     // 114
  ///     // 115
  ///     // 32
  ///     // 55357
  ///     // 56464
  ///
  /// Unicode scalar values that make up a string's contents can be up to 21
  /// bits long. The longer scalar values may need two `UInt16` values for
  /// storage. Those "pairs" of code units are called *surrogate pairs*.
  ///
  ///     let flowermoji = "💐"
  ///     for v in flowermoji.unicodeScalars {
  ///         print(v, v.value)
  ///     }
  ///     // 💐 128144
  ///
  ///     for v in flowermoji.utf16 {
  ///         print(v)
  ///     }
  ///     // 55357
  ///     // 56464
  ///
  /// To convert a `String.UTF16View` instance back into a string, use the
  /// `String` type's `init(_:)` initializer.
  ///
  ///     let favemoji = "My favorite emoji is 🎉"
  ///     if let i = favemoji.utf16.firstIndex(where: { $0 >= 128 }) {
  ///         let asciiPrefix = String(favemoji.utf16[..<i])
  ///         print(asciiPrefix)
  ///     }
  ///     // Prints "My favorite emoji is "
  ///
  /// UTF16View Elements Match NSString Characters
  /// ============================================
  ///
  /// The UTF-16 code units of a string's `utf16` view match the elements
  /// accessed through indexed `NSString` APIs.
  ///
  ///     print(flowers.utf16.count)
  ///     // Prints "10"
  ///
  ///     let nsflowers = flowers as NSString
  ///     print(nsflowers.length)
  ///     // Prints "10"
  ///
  /// Unlike `NSString`, however, `String.UTF16View` does not use integer
  /// indices. If you need to access a specific position in a UTF-16 view, use
  /// Swift's index manipulation methods. The following example accesses the
  /// fourth code unit in both the `flowers` and `nsflowers` strings:
  ///
  ///     print(nsflowers.character(at: 3))
  ///     // Prints "119"
  ///
  ///     let i = flowers.utf16.index(flowers.utf16.startIndex, offsetBy: 3)
  ///     print(flowers.utf16[i])
  ///     // Prints "119"
  ///
  /// Although the Swift overlay updates many Objective-C methods to return
  /// native Swift indices and index ranges, some still return instances of
  /// `NSRange`. To convert an `NSRange` instance to a range of
  /// `String.Index`, use the `Range(_:in:)` initializer, which takes an
  /// `NSRange` and a string as arguments.
  ///
  ///     let snowy = "❄️ Let it snow! ☃️"
  ///     let nsrange = NSRange(location: 3, length: 12)
  ///     if let range = Range(nsrange, in: snowy) {
  ///         print(snowy[range])
  ///     }
  ///     // Prints "Let it snow!"
  @_fixed_layout
  public struct UTF16View {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable
    internal init(_ guts: _StringGuts) {
      self._guts = guts
      _invariantCheck()
    }
  }
}

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
  @inlinable
  public var startIndex: Index {
    @inline(__always) get { return _guts.startIndex }
  }

  /// The "past the end" position---that is, the position one greater than
  /// the last valid subscript argument.
  ///
  /// In an empty UTF-16 view, `endIndex` is equal to `startIndex`.
  @inlinable
  public var endIndex: Index {
    @inline(__always) get { return _guts.endIndex }
  }

  @inlinable @inline(__always)
  public func index(after i: Index) -> Index {
    // TODO(String performance) known-ASCII fast path

    if _slowPath(_guts.isForeign) { return _foreignIndex(after: i) }

    // For a BMP scalar (1-3 UTF-8 code units), advance past it. For a non-BMP
    // scalar, use a transcoded offset first.
    let len = _guts.fastUTF8ScalarLength(startingAt: i.encodedOffset)
    if len == 4 && i.transcodedOffset == 0 {
      return i.nextTranscoded
    }
    return i.strippingTranscoding.encoded(offsetBy: len)
  }

  @inlinable @inline(__always)
  public func index(before i: Index) -> Index {
    precondition(!i.isZeroPosition)
    // TODO(String performance) known-ASCII fast path

    if _slowPath(_guts.isForeign) { return _foreignIndex(before: i) }

    if i.transcodedOffset != 0 {
      _internalInvariant(i.transcodedOffset == 1)
      return i.strippingTranscoding
    }

    let len = _guts.fastUTF8ScalarLength(endingAt: i.encodedOffset)
    if len == 4 {
      // 2 UTF-16 code units comprise this scalar; advance to the beginning and
      // start mid-scalar transcoding
      return i.encoded(offsetBy: -len).nextTranscoded
    }

    // Single UTF-16 code unit
    _internalInvariant((1...3) ~= len)
    return i.encoded(offsetBy: -len)
  }

  public func index(_ i: Index, offsetBy n: Int) -> Index {
    // TODO(String performance) known-ASCII fast path
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n)
    }

    let lowerOffset = _nativeGetOffset(for: i)
    let result = _nativeGetIndex(for: lowerOffset + n)
    return result
  }

  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    // TODO(String performance) known-ASCII fast path
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n, limitedBy: limit)
    }

    let iOffset = _nativeGetOffset(for: i)
    let limitOffset = _nativeGetOffset(for: limit)

    // If distance < 0, limit has no effect if it is greater than i.
    if _slowPath(n < 0 && limit <= i && limitOffset > iOffset + n) {
      return nil
    }
    // If distance > 0, limit has no effect if it is less than i.
    if _slowPath(n >= 0 && limit >= i && limitOffset < iOffset + n) {
      return nil
    }

    let result = _nativeGetIndex(for: iOffset + n)
    return result
  }

  public func distance(from start: Index, to end: Index) -> Int {
    // TODO(String performance) known-ASCII fast path
    if _slowPath(_guts.isForeign) {
      return _foreignDistance(from: start, to: end)
    }

    let lower = _nativeGetOffset(for: start)
    let upper = _nativeGetOffset(for: end)
    return upper &- lower
  }

  @inlinable
  public var count: Int {
    if _slowPath(_guts.isForeign) {
      return _foreignCount()
    }
    return _nativeGetOffset(for: endIndex)
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
      // TODO(String performance) known-ASCII fast path
      String(_guts)._boundsCheck(i)

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
  @inlinable
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
    _internalInvariant(_guts.isForeign)
    return i.nextEncoded
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(before i: Index) -> Index {
    _internalInvariant(_guts.isForeign)
    return i.priorEncoded
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignSubscript(position i: Index) -> UTF16.CodeUnit {
    _internalInvariant(_guts.isForeign)
    return _guts.foreignErrorCorrectedUTF16CodeUnit(at: i)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignDistance(from start: Index, to end: Index) -> Int {
    _internalInvariant(_guts.isForeign)
    return end.encodedOffset - start.encodedOffset
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    _internalInvariant(_guts.isForeign)
    let l = limit.encodedOffset - i.encodedOffset
    if n > 0 ? l >= 0 && l < n : l <= 0 && n < l {
      return nil
    }
    return i.encoded(offsetBy: n)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(_ i: Index, offsetBy n: Int) -> Index {
    _internalInvariant(_guts.isForeign)
    return i.encoded(offsetBy: n)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignCount() -> Int {
    _internalInvariant(_guts.isForeign)
    return endIndex.encodedOffset - startIndex.encodedOffset
  }
}

extension String.Index {
  @usableFromInline @inline(never) // opaque slow-path
  @_effects(releasenone)
  internal func _foreignIsWithin(_ target: String.UTF16View) -> Bool {
    _internalInvariant(target._guts.isForeign)

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
  internal func _nativeGetOffset(for idx: Index) -> Int {
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
  internal func _nativeGetIndex(for offset: Int) -> Index {
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
    if remaining == 0 { return crumb }

    return _guts.withFastUTF8 { utf8 in
      var readIdx = crumb.encodedOffset
      let readEnd = utf8.count
      _internalInvariant(readIdx < readEnd)

      var utf16I = 0
      let utf16End: Int = remaining

      // Adjust for sub-scalar initial transcoding: If we're starting the scan
      // at a trailing surrogate, then we set our starting count to be -1 so as
      // offset counting the leading surrogate.
      if crumb.transcodedOffset != 0 {
        utf16I = -1
      }

      while true {
        let len = _utf8ScalarLength(utf8[_unchecked: readIdx])
        let utf16Len = len == 4 ? 2 : 1
        utf16I &+= utf16Len

        if utf16I >= utf16End {
          // Uncommon: final sub-scalar transcoded offset
          if _slowPath(utf16I > utf16End) {
            _internalInvariant(utf16Len == 2)
            return Index(encodedOffset: readIdx, transcodedOffset: 1)
          }
          return Index(encodedOffset: readIdx &+ len)
        }

        readIdx &+= len
      }
    }
  }
}

extension String {
  @usableFromInline // @testable
  internal func _nativeCopyUTF16CodeUnits(
    into buffer: UnsafeMutableBufferPointer<UInt16>,
    range: Range<String.Index>
  ) {
    _internalInvariant(_guts.isFastUTF8)

    if _slowPath(range.isEmpty) { return }

    return _guts.withFastUTF8 { utf8 in
      var writeIdx = 0
      let writeEnd = buffer.count
      var readIdx = range.lowerBound.encodedOffset
      let readEnd = range.upperBound.encodedOffset

      // Handle mid-transcoded-scalar initial index
      if _slowPath(range.lowerBound.transcodedOffset != 0) {
        _internalInvariant(range.lowerBound.transcodedOffset == 1)
        let (scalar, len) = _decodeScalar(utf8, startingAt: readIdx)
        buffer[writeIdx] = scalar.utf16[1]
        readIdx &+= len
        writeIdx &+= 1
      }

      // Transcode middle
      while readIdx < readEnd {
        let (scalar, len) = _decodeScalar(utf8, startingAt: readIdx)
        buffer[writeIdx] = scalar.utf16[0]
        readIdx &+= len
        writeIdx &+= 1
        if _slowPath(scalar.utf16.count == 2) {
          buffer[writeIdx] = scalar.utf16[1]
          writeIdx &+= 1
        }
      }

      // Handle mid-transcoded-scalar final index
      if _slowPath(range.upperBound.transcodedOffset == 1) {
        _internalInvariant(writeIdx < writeEnd)
        let (scalar, _) = _decodeScalar(utf8, startingAt: readIdx)
        _internalInvariant(scalar.utf16.count == 2)

        buffer[writeIdx] = scalar.utf16[0]
        writeIdx &+= 1
      }
      _internalInvariant(writeIdx <= writeEnd)

    }
  }
}

