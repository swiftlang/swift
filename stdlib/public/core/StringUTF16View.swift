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

// TODO(UTF8 merge): Find a common place for these helpers
extension _StringGuts {
  @_effects(releasenone)
  // TODO(UTF8): Should probably take a String.Index, assert no transcoding
  internal func foreignUTF16CodeUnit(at i: Int) -> UInt16 {
    // Currently, foreign means NSString
    return _cocoaStringSubscript(_object.cocoaObject, i)
  }
}

internal let _leadingSurrogateBias: UInt16 = 0xd800
internal let _trailingSurrogateBias: UInt16 = 0xdc00
internal let _surrogateMask: UInt16 = 0xfc00

@inline(__always)
internal func _isTrailingSurrogate(_ cu: UInt16) -> Bool {
  return cu & _surrogateMask == _trailingSurrogateBias
}
@inline(__always)
internal func _isLeadingSurrogate(_ cu: UInt16) -> Bool {
  return cu & _surrogateMask == _leadingSurrogateBias
}

internal func _numTranscodedUTF8CodeUnits(_ x: UInt16) -> Int {
  _sanityCheck(!_isTrailingSurrogate(x))

  if _slowPath(_isLeadingSurrogate(x)) { return 4 }

  switch x {
    case 0..<0x80: return 1
    case 0x80..<0x0800: return 2
    case _: return 3
  }
}


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
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UTF16View {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ guts: _StringGuts) {
      self._guts = guts
      _invariantCheck()
    }
  }
}

extension String.UTF16View {
  @inlinable @inline(__always)
  internal func _invariantCheck() {
    #if INTERNAL_CHECKS_ENABLED
    #endif
  }
}

extension String.UTF16View: BidirectionalCollection {
  public typealias Index = String.Index

  /// The position of the first code unit if the `String` is
  /// nonempty; identical to `endIndex` otherwise.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    @inline(__always) get { return Index(encodedOffset: 0) }
  }

  /// The "past the end" position---that is, the position one greater than
  /// the last valid subscript argument.
  ///
  /// In an empty UTF-16 view, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    @inline(__always) get { return Index(encodedOffset: _guts.count) }
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
    precondition(i.encodedOffset > 0)

    if _slowPath(_guts.isForeign) { return _foreignIndex(before: i) }

    // TODO(UTF8) known-ASCII fast path

    if i.transcodedOffset != 0 {
      _sanityCheck(i.transcodedOffset == 1)
      return Index(encodedOffset: i.encodedOffset)
    }

    let len = _guts.fastUTF8ScalarLength(endingAt: i.encodedOffset)
    if len == 4 {
      return Index(
        encodedOffset: i.encodedOffset &- len,
        transcodedOffset: 1)
    }

    _sanityCheck((1...3) ~= len)
    return Index(encodedOffset: i.encodedOffset &- len)
  }

  @inlinable @inline(__always)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n)
    }

    // TODO(UTF8) known-ASCII fast path
    return __index(i, offsetBy: n)
  }

  @inlinable @inline(__always)
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    if _slowPath(_guts.isForeign) {
      return _foreignIndex(i, offsetBy: n, limitedBy: limit)
    }

    // TODO(UTF8) known-ASCII fast paths
    return __index(i, offsetBy: n, limitedBy: limit)
  }

  @inlinable @inline(__always)
  public func distance(from start: Index, to end: Index) -> Int {
    if _slowPath(_guts.isForeign) {
      return _foreignDistance(from: start, to: end)
    }

    // TODO(UTF8) known-ASCII fast paths
    return __distance(from: start, to: end)
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
      _precondition(i.encodedOffset >= 0 && i < endIndex)
      // TODO(UTF8): known-ASCII fast path

      if _fastPath(_guts.isFastUTF8) {
        let scalar = _guts.fastUTF8Scalar(startingAt: i.encodedOffset)
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

  #if _runtime(_ObjC)
  // These may become less important once <rdar://problem/19255291> is addressed.
  @available(
    *, unavailable,
    message: "Indexing a String's UTF16View requires a String.UTF16View.Index, which can be constructed from Int when Foundation is imported")
  public subscript(i: Int) -> UTF16.CodeUnit {
    Builtin.unreachable()
  }

  @available(
    *, unavailable,
    message: "Slicing a String's UTF16View requires a Range<String.UTF16View.Index>, String.UTF16View.Index can be constructed from Int when Foundation is imported")
  public subscript(bounds: Range<Int>) -> UTF16View {
    Builtin.unreachable()
  }
#endif
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
  @inlinable // FIXME(sil-serialize-all)
  public init?(
    _ sourcePosition: String.Index, within target: String.UTF16View
  ) {
    guard sourcePosition.transcodedOffset == 0 else { return nil }
    self.init(encodedOffset: sourcePosition.encodedOffset)
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

    // Currently, foreign means NSString
    return Index(encodedOffset: i.encodedOffset + 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(before i: Index) -> Index {
    _sanityCheck(_guts.isForeign)

    // Currently, foreign means NSString
    return Index(encodedOffset: i.encodedOffset - 1)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignSubscript(position i: Index) -> UTF16.CodeUnit {
    _sanityCheck(_guts.isForeign)

    // Currently, foreign means NSString
    return _guts.foreignUTF16CodeUnit(at: i.encodedOffset)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignDistance(from start: Index, to end: Index) -> Int {
    _sanityCheck(_guts.isForeign)

    // Currently, foreign means NSString
    return end.encodedOffset - start.encodedOffset
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    _sanityCheck(_guts.isForeign)

    // Currently, foreign means NSString
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

    // Currently, foreign means NSString
    return Index(encodedOffset: i.encodedOffset + n)
  }

}
