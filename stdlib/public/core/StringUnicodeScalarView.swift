//===----------------------------------------------------------------------===//
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

extension _StringGuts {
  @usableFromInline @inline(__always) // fast-path: fold common fastUTF8 check
  internal func scalarAlign(_ idx: Index) -> Index {
    // TODO(UTF8 perf): isASCII check

    if _slowPath(idx.transcodedOffset != 0 || idx.encodedOffset == 0) {
      // Transcoded indices are already scalar aligned
      return String.Index(encodedOffset: idx.encodedOffset)
    }
    if _slowPath(self.isForeign) {
      return foreignScalarAlign(idx)
    }

    return self.withFastUTF8 { utf8 in
      var i = idx.encodedOffset
      while _slowPath(_isContinuation(utf8[i])) {
        i -= 1
        // TODO(UTF8 merge): Verify it's not possible to form Substring from
        // sub-scalar indices, otherwise `utf8` could start with continuation
        // byte.
        _sanityCheck(
          i >= 0, "Malformed contents: starts with continuation byte")
      }
      // If no alignment is performed, keep grapheme cache
      if i == idx.encodedOffset {
        return idx
      }

      return Index(encodedOffset: i)
    }
  }

  // TODO(UTF8): Should probably take a String.Index, assert no transcoding
  @usableFromInline @inline(__always)
  internal func fastUTF8ScalarLength(startingAt i: Int) -> Int {
    _sanityCheck(isFastUTF8)
    let len = _utf8ScalarLength(self.withFastUTF8 { $0[i] })
    _sanityCheck((1...4) ~= len)
    return len
  }

  // TODO(UTF8): Should probably take a String.Index, assert no transcoding
  @usableFromInline @inline(__always)
  internal func fastUTF8ScalarLength(endingAt i: Int) -> Int {
    _sanityCheck(isFastUTF8)

    return self.withFastUTF8 { utf8 in
      _sanityCheck(i == utf8.count || !_isContinuation(utf8[i]))
      var len = 1
      while _isContinuation(utf8[i - len]) {
        _sanityCheck(i - len > 0)
        len += 1
      }
      _sanityCheck(len <= 4)
      return len
    }
  }

  @usableFromInline @inline(__always)
  internal func fastUTF8Scalar(startingAt i: Int) -> Unicode.Scalar {
    _sanityCheck(isFastUTF8)
    return self.withFastUTF8 { utf8 in
      let cu0 = utf8[i]
      switch _utf8ScalarLength(cu0) {
      case 1: return _decodeUTF8(cu0)
      case 2: return _decodeUTF8(cu0, utf8[i &+ 1])
      case 3: return _decodeUTF8(cu0, utf8[i &+ 1], utf8[i &+ 2])
      case 4: return _decodeUTF8(cu0, utf8[i &+ 1], utf8[i &+ 2], utf8[i &+ 3])
      default: Builtin.unreachable()
      }
    }
  }

  @usableFromInline
  @_effects(releasenone)
  internal func isOnUnicodeScalarBoundary(_ i: String.Index) -> Bool {
    // TODO(UTF8 perf): isASCII check
    // TODO(UTF8): Guts bounds check helper, or something in terms of Index

    // Beginning and end are always scalar aligned; mid-scalar never is
    //
    // TODO(UTF8 merge): Is this only under guarantee of well-formedness?
    guard i.transcodedOffset == 0 else { return false }
    if i == self.startIndex || i == self.endIndex { return true }

    if _fastPath(isFastUTF8) {
      return self.withFastUTF8 { return !_isContinuation($0[i.encodedOffset]) }
    }

    return i == foreignScalarAlign(i)
  }
}

extension String {
  /// A view of a string's contents as a collection of Unicode scalar values.
  ///
  /// You can access a string's view of Unicode scalar values by using its
  /// `unicodeScalars` property. Unicode scalar values are the 21-bit codes
  /// that are the basic unit of Unicode. Each scalar value is represented by
  /// a `Unicode.Scalar` instance and is equivalent to a UTF-32 code unit.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.unicodeScalars {
  ///         print(v.value)
  ///     }
  ///     // 70
  ///     // 108
  ///     // 111
  ///     // 119
  ///     // 101
  ///     // 114
  ///     // 115
  ///     // 32
  ///     // 128144
  ///
  /// Some characters that are visible in a string are made up of more than one
  /// Unicode scalar value. In that case, a string's `unicodeScalars` view
  /// contains more elements than the string itself.
  ///
  ///     let flag = "🇵🇷"
  ///     for c in flag {
  ///         print(c)
  ///     }
  ///     // 🇵🇷
  ///
  ///     for v in flag.unicodeScalars {
  ///         print(v.value)
  ///     }
  ///     // 127477
  ///     // 127479
  ///
  /// You can convert a `String.UnicodeScalarView` instance back into a string
  /// using the `String` type's `init(_:)` initializer.
  ///
  ///     let favemoji = "My favorite emoji is 🎉"
  ///     if let i = favemoji.unicodeScalars.firstIndex(where: { $0.value >= 128 }) {
  ///         let asciiPrefix = String(favemoji.unicodeScalars[..<i])
  ///         print(asciiPrefix)
  ///     }
  ///     // Prints "My favorite emoji is "
  @_fixed_layout
  public struct UnicodeScalarView {
    @usableFromInline
    internal var _guts: _StringGuts

    @inlinable @inline(__always)
    internal init(_ _guts: _StringGuts) {
      self._guts = _guts
      _invariantCheck()
    }
  }
}

extension String.UnicodeScalarView {
  @inlinable @inline(__always)
  internal func _invariantCheck() {
    #if INTERNAL_CHECKS_ENABLED
    #endif
  }
}

extension String.UnicodeScalarView: BidirectionalCollection {
  public typealias Index = String.Index

  /// The position of the first Unicode scalar value if the string is
  /// nonempty.
  ///
  /// If the string is empty, `startIndex` is equal to `endIndex`.
  @inlinable
  public var startIndex: Index {
    @inline(__always) get { return _guts.startIndex }
  }

  /// The "past the end" position---that is, the position one greater than
  /// the last valid subscript argument.
  ///
  /// In an empty Unicode scalars view, `endIndex` is equal to `startIndex`.
  @inlinable
  public var endIndex: Index {
    @inline(__always) get { return _guts.endIndex }
  }

  /// Returns the next consecutive location after `i`.
  ///
  /// - Precondition: The next location exists.
  @inlinable @inline(__always)
  public func index(after i: Index) -> Index {
    _sanityCheck(i < endIndex)
    // TODO(UTF8): isKnownASCII bit fast-path...

    if _fastPath(_guts.isFastUTF8) {
      let len = _guts.fastUTF8ScalarLength(startingAt: i.encodedOffset)
      return Index(encodedOffset: i.encodedOffset &+ len)
    }

    return _foreignIndex(after: i)
  }

  /// Returns the previous consecutive location before `i`.
  ///
  /// - Precondition: The previous location exists.
  @inlinable @inline(__always)
  public func index(before i: Index) -> Index {
    precondition(i.encodedOffset > 0)
    if _fastPath(_guts.isFastUTF8) {
      // TODO(UTF8): isKnownASCII bit fast-path...

      let len = _guts.withFastUTF8 { utf8 -> Int in
        var len = 1
        while _isContinuation(utf8[i.encodedOffset &- len]) {
          len += 1
        }
        _sanityCheck(len == _utf8ScalarLength(utf8[i.encodedOffset - len]))
        return len
      }
      _sanityCheck(len <= 4, "invalid UTF8")
      return Index(encodedOffset: i.encodedOffset &- len)
    }

    return _foreignIndex(before: i)
  }

  /// Accesses the Unicode scalar value at the given position.
  ///
  /// The following example searches a string's Unicode scalars view for a
  /// capital letter and then prints the character and Unicode scalar value
  /// at the found index:
  ///
  ///     let greeting = "Hello, friend!"
  ///     if let i = greeting.unicodeScalars.firstIndex(where: { "A"..."Z" ~= $0 }) {
  ///         print("First capital letter: \(greeting.unicodeScalars[i])")
  ///         print("Unicode scalar value: \(greeting.unicodeScalars[i].value)")
  ///     }
  ///     // Prints "First capital letter: H"
  ///     // Prints "Unicode scalar value: 72"
  ///
  /// - Parameter position: A valid index of the character view. `position`
  ///   must be less than the view's end index.
  @inlinable
  public subscript(position: Index) -> Unicode.Scalar {
    @inline(__always) get {
      String(_guts)._boundsCheck(position)
      let i = _guts.scalarAlign(position)
      if _fastPath(_guts.isFastUTF8) {
        return _guts.fastUTF8Scalar(startingAt: i.encodedOffset)
      }

      return _foreignSubscript(aligned: i)
    }
  }
}

// TODO(UTF8): design specialized iterator, rather than default indexing one
// extension String.UnicodeScalarView {
//   /// An iterator over the Unicode scalars that make up a `UnicodeScalarView`
//   /// collection.
//   @_fixed_layout
//   public struct Iterator : IteratorProtocol {
//     // TODO:

//     /// Advances to the next element and returns it, or `nil` if no next
//     /// element exists.
//     ///
//     /// Once `nil` has been returned, all subsequent calls return `nil`.
//     ///
//     /// - Precondition: `next()` has not been applied to a copy of `self`
//     ///   since the copy was made.
//     @inlinable @inline(__always)
//     public mutating func next() -> Unicode.Scalar? {
//       unimplemented_utf8()
//     }
//   }
// }

// extension String.UnicodeScalarView {
//   /// Returns an iterator over the Unicode scalars that make up this view.
//   ///
//   /// - Returns: An iterator over this collection's `Unicode.Scalar` elements.
//   @inlinable @inline(__always)
//   public func makeIterator() -> Iterator {
//     unimplemented_utf8()
//   }
// }

extension String.UnicodeScalarView: CustomStringConvertible {
 @inlinable
 public var description: String {
   @inline(__always) get { return String(_guts) }
 }
}

extension String.UnicodeScalarView: CustomDebugStringConvertible {
 public var debugDescription: String {
   return "StringUnicodeScalarView(\(self.description.debugDescription))"
 }
}

extension String {
  /// Creates a string corresponding to the given collection of Unicode
  /// scalars.
  ///
  /// You can use this initializer to create a new string from a slice of
  /// another string's `unicodeScalars` view.
  ///
  ///     let picnicGuest = "Deserving porcupine"
  ///     if let i = picnicGuest.unicodeScalars.firstIndex(of: " ") {
  ///         let adjective = String(picnicGuest.unicodeScalars[..<i])
  ///         print(adjective)
  ///     }
  ///     // Prints "Deserving"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.unicodeScalars` view.
  ///
  /// - Parameter unicodeScalars: A collection of Unicode scalar values.
  @inlinable @inline(__always)
  public init(_ unicodeScalars: UnicodeScalarView) {
    self.init(unicodeScalars._guts)
  }

  /// The index type for a string's `unicodeScalars` view.
  public typealias UnicodeScalarIndex = UnicodeScalarView.Index

  /// The string's value represented as a collection of Unicode scalar values.
  @inlinable
  public var unicodeScalars: UnicodeScalarView {
    @inline(__always) get { return UnicodeScalarView(_guts) }
    @inline(__always) set { _guts = newValue._guts }
  }
}

extension String.UnicodeScalarView : RangeReplaceableCollection {
  /// Creates an empty view instance.
  @inlinable @inline(__always)
  public init() {
    self.init(_StringGuts())
  }

  /// Reserves enough space in the view's underlying storage to store the
  /// specified number of ASCII characters.
  ///
  /// Because a Unicode scalar value can require more than a single ASCII
  /// character's worth of storage, additional allocation may be necessary
  /// when adding to a Unicode scalar view after a call to
  /// `reserveCapacity(_:)`.
  ///
  /// - Parameter n: The minimum number of ASCII character's worth of storage
  ///   to allocate.
  ///
  /// - Complexity: O(*n*), where *n* is the capacity being reserved.
  public mutating func reserveCapacity(_ n: Int) {
    self._guts.reserveCapacity(n)
  }

  /// Appends the given Unicode scalar to the view.
  ///
  /// - Parameter c: The character to append to the string.
  public mutating func append(_ c: Unicode.Scalar) {
    // TODO(UTF8 perf): This is a horribly slow means...
    self.append(contentsOf: [c])
  }

  /// Appends the Unicode scalar values in the given sequence to the view.
  ///
  /// - Parameter newElements: A sequence of Unicode scalar values.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the resulting view.
  public mutating func append<S : Sequence>(contentsOf newElements: S)
  where S.Element == Unicode.Scalar {
    // TODO(UTF8 perf): This is a horribly slow means...
    let scalars = String(decoding: newElements.map { $0.value }, as: UTF32.self)
    self = (String(self._guts) + scalars).unicodeScalars
  }

  /// Replaces the elements within the specified bounds with the given Unicode
  /// scalar values.
  ///
  /// Calling this method invalidates any existing indices for use with this
  /// string.
  ///
  /// - Parameters:
  ///   - bounds: The range of elements to replace. The bounds of the range
  ///     must be valid indices of the view.
  ///   - newElements: The new Unicode scalar values to add to the string.
  ///
  /// - Complexity: O(*m*), where *m* is the combined length of the view and
  ///   `newElements`. If the call to `replaceSubrange(_:with:)` simply
  ///   removes elements at the end of the string, the complexity is O(*n*),
  ///   where *n* is equal to `bounds.count`.
  public mutating func replaceSubrange<C>(
    _ bounds: Range<Index>,
    with newElements: C
  ) where C : Collection, C.Element == Unicode.Scalar {
    // TODO(UTF8 perf): This is a horribly slow means...
    //
    // TODO(UTF8 perf): Consider storing a string directly, or implemeting RSR
    // on guts.

    let utf8Replacement = newElements.flatMap { String($0).utf8 }
    let replacement = utf8Replacement.withUnsafeBufferPointer {
      return String._uncheckedFromUTF8($0)
    }
    var copy = String(_guts)
    copy.replaceSubrange(bounds, with: replacement)
    self = copy.unicodeScalars
  }
}

// Index conversions
extension String.UnicodeScalarIndex {
  /// Creates an index in the given Unicode scalars view that corresponds
  /// exactly to the specified `UTF16View` position.
  ///
  /// The following example finds the position of a space in a string's `utf16`
  /// view and then converts that position to an index in the string's
  /// `unicodeScalars` view:
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let utf16Index = cafe.utf16.firstIndex(of: 32)!
  ///     let scalarIndex = String.Index(utf16Index, within: cafe.unicodeScalars)!
  ///
  ///     print(String(cafe.unicodeScalars[..<scalarIndex]))
  ///     // Prints "Café"
  ///
  /// If the index passed as `sourcePosition` doesn't have an exact
  /// corresponding position in `unicodeScalars`, the result of the
  /// initializer is `nil`. For example, an attempt to convert the position of
  /// the trailing surrogate of a UTF-16 surrogate pair results in `nil`.
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in the `utf16` view of a string.
  ///     `utf16Index` must be an element of
  ///     `String(unicodeScalars).utf16.indices`.
  ///   - unicodeScalars: The `UnicodeScalarView` in which to find the new
  ///     position.
  public init?(
    _ sourcePosition: String.Index,
    within unicodeScalars: String.UnicodeScalarView
  ) {
    guard unicodeScalars._guts.isOnUnicodeScalarBoundary(sourcePosition) else {
      return nil
    }
    self = sourcePosition
  }

  /// Returns the position in the given string that corresponds exactly to this
  /// index.
  ///
  /// This example first finds the position of a space (UTF-8 code point `32`)
  /// in a string's `utf8` view and then uses this method find the same position
  /// in the string.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.unicodeScalars.firstIndex(of: "🍵")
  ///     let j = i.samePosition(in: cafe)!
  ///     print(cafe[j...])
  ///     // Prints "🍵"
  ///
  /// - Parameter characters: The string to use for the index conversion.
  ///   This index must be a valid index of at least one view of `characters`.
  /// - Returns: The position in `characters` that corresponds exactly to
  ///   this index. If this index does not have an exact corresponding
  ///   position in `characters`, this method returns `nil`. For example,
  ///   an attempt to convert the position of a UTF-8 continuation byte
  ///   returns `nil`.
  public func samePosition(in characters: String) -> String.Index? {
    return String.Index(self, within: characters)
  }
}

// Reflection
extension String.UnicodeScalarView : CustomReflectable {
  /// Returns a mirror that reflects the Unicode scalars view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}


//===--- Slicing Support --------------------------------------------------===//
/// In Swift 3.2, in the absence of type context,
///
///   someString.unicodeScalars[
///     someString.unicodeScalars.startIndex
///     ..< someString.unicodeScalars.endIndex]
///
/// was deduced to be of type `String.UnicodeScalarView`.  Provide a
/// more-specific Swift-3-only `subscript` overload that continues to produce
/// `String.UnicodeScalarView`.
extension String.UnicodeScalarView {
  public typealias SubSequence = Substring.UnicodeScalarView

  @available(swift, introduced: 4)
  public subscript(r: Range<Index>) -> String.UnicodeScalarView.SubSequence {
    return String.UnicodeScalarView.SubSequence(self, _bounds: r)
  }
}

// Foreign string Support
extension String.UnicodeScalarView {
  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(after i: Index) -> Index {
    _sanityCheck(_guts.isForeign)
    let cu = _guts.foreignErrorCorrectedUTF16CodeUnit(at: i)
    let len = _isLeadingSurrogate(cu) ? 2 : 1

    return Index(encodedOffset: i.encodedOffset + len)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignIndex(before i: Index) -> Index {
    _sanityCheck(_guts.isForeign)
    let priorIdx = String.Index(encodedOffset: i.encodedOffset - 1)
    let cu = _guts.foreignErrorCorrectedUTF16CodeUnit(at: priorIdx)
    let len = _isTrailingSurrogate(cu) ? 2 : 1

    return Index(encodedOffset: i.encodedOffset - len)
  }

  @usableFromInline @inline(never)
  @_effects(releasenone)
  internal func _foreignSubscript(aligned i: Index) -> Unicode.Scalar {
    _sanityCheck(_guts.isForeign)
    _sanityCheck(_guts.isOnUnicodeScalarBoundary(i),
      "should of been aligned prior")

    return _guts.foreignErrorCorrectedScalar(startingAt: i)
  }
}
