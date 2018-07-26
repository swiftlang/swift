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
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UnicodeScalarView :
    BidirectionalCollection,
    CustomStringConvertible,
    CustomDebugStringConvertible
  {
    @usableFromInline
    internal var _guts: _StringGuts

    /// The offset of this view's `_guts` from the start of an original string,
    /// in UTF-16 code units. This is here to support legacy Swift 3-style
    /// slicing where `s.unicodeScalars[i..<j]` produces a
    /// `String.UnicodeScalarView`. The offset should be subtracted from the
    /// `encodedOffset` of view indices before it is passed to `_guts`.
    ///
    /// Note: This should be removed when Swift 3 semantics are no longer
    /// supported.
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _coreOffset: Int

    @inlinable // FIXME(sil-serialize-all)
    internal init(_ _guts: _StringGuts, coreOffset: Int = 0) {
      self._guts = _guts
      self._coreOffset = coreOffset
    }

    public typealias Index = String.Index

    /// Translates a `_guts` index into a `UnicodeScalarIndex` using this
    /// view's `_coreOffset`.
    @inlinable // FIXME(sil-serialize-all)
    internal func _fromCoreIndex(_ i: Int) -> Index {
      return Index(encodedOffset: i + _coreOffset)
    }

    /// Translates a `UnicodeScalarIndex` into a `_guts` index using this
    /// view's `_coreOffset`.
    @inlinable // FIXME(sil-serialize-all)
    internal func _toCoreIndex(_ i: Index) -> Int {
      return i.encodedOffset - _coreOffset
    }

    /// The position of the first Unicode scalar value if the string is
    /// nonempty.
    ///
    /// If the string is empty, `startIndex` is equal to `endIndex`.
    @inlinable // FIXME(sil-serialize-all)
    public var startIndex: Index {
      return _fromCoreIndex(_guts.startIndex)
    }

    /// The "past the end" position---that is, the position one greater than
    /// the last valid subscript argument.
    ///
    /// In an empty Unicode scalars view, `endIndex` is equal to `startIndex`.
    @inlinable // FIXME(sil-serialize-all)
    public var endIndex: Index {
      return _fromCoreIndex(_guts.endIndex)
    }

    /// Returns the next consecutive location after `i`.
    ///
    /// - Precondition: The next location exists.
    @inlinable // FIXME(sil-serialize-all)
    public func index(after i: Index) -> Index {
      let offset = _toCoreIndex(i)
      let length: Int = _visitGuts(_guts, args: offset,
        ascii: { (_,_) -> Int in return 1 },
        utf16: { utf16, offset in
          return utf16.unicodeScalarWidth(startingAt: offset) },
        opaque: { opaque, offset in
          return opaque.unicodeScalarWidth(startingAt: offset) }
      )
      return _fromCoreIndex(offset + length)
    }

    /// Returns the previous consecutive location before `i`.
    ///
    /// - Precondition: The previous location exists.
    @inlinable // FIXME(sil-serialize-all)
    public func index(before i: Index) -> Index {
      let offset = _toCoreIndex(i)
      let length: Int = _visitGuts(_guts, args: offset,
        ascii: { (_,_) -> Int in return 1 },
        utf16: { utf16, offset in
          return utf16.unicodeScalarWidth(endingAt: offset) },
        opaque: { opaque, offset in
          return opaque.unicodeScalarWidth(endingAt: offset) }
      )
      return _fromCoreIndex(offset - length)
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
    @inlinable // FIXME(sil-serialize-all)
    public subscript(position: Index) -> Unicode.Scalar {
      let offset = position.encodedOffset
      return _guts.unicodeScalar(startingAt: offset)
    }

    /// An iterator over the Unicode scalars that make up a `UnicodeScalarView`
    /// collection.
    @_fixed_layout // FIXME(sil-serialize-all)
    public struct Iterator : IteratorProtocol {
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _guts: _StringGuts

      // FIXME(TODO: JIRA): the below is absurdly wasteful.
      // UnicodeScalarView.Iterator should be able to be passed in-registers.

      @usableFromInline // FIXME(sil-serialize-all)
      internal var _asciiIterator: _UnmanagedASCIIString.UnicodeScalarIterator?
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _utf16Iterator: _UnmanagedUTF16String.UnicodeScalarIterator?
      @usableFromInline // FIXME(sil-serialize-all)
      internal var _opaqueIterator: _UnmanagedOpaqueString.UnicodeScalarIterator?

      @usableFromInline
      internal var _smallIterator: _SmallUTF8String.UnicodeScalarIterator?

      @inlinable // FIXME(sil-serialize-all)
      internal init(_ guts: _StringGuts) {
        if _slowPath(guts._isOpaque) {
          self.init(_opaque: guts)
          return
        }
        self.init(_concrete: guts)
      }

      @inlinable // FIXME(sil-serialize-all)
      @inline(__always)
      internal init(_concrete guts: _StringGuts) {
        _sanityCheck(!guts._isOpaque)
        self._guts = guts
        defer { _fixLifetime(self) }
        if _guts.isASCII {
          self._asciiIterator =
            _guts._unmanagedASCIIView.makeUnicodeScalarIterator()
        } else {
          self._utf16Iterator =
            _guts._unmanagedUTF16View.makeUnicodeScalarIterator()
        }
      }

      @usableFromInline // @opaque
      init(_opaque _guts: _StringGuts) {
        _sanityCheck(_guts._isOpaque)
        defer { _fixLifetime(self) }
        self._guts = _guts
        // TODO: Replace the whole iterator scheme with a sensible solution.
        if self._guts._isSmall {
          self._smallIterator =
            _guts._smallUTF8String.makeUnicodeScalarIterator()
        } else {
          self._opaqueIterator = _guts._asOpaque().makeUnicodeScalarIterator()
        }
      }

      /// Advances to the next element and returns it, or `nil` if no next
      /// element exists.
      ///
      /// Once `nil` has been returned, all subsequent calls return `nil`.
      ///
      /// - Precondition: `next()` has not been applied to a copy of `self`
      ///   since the copy was made.
      @inlinable // FIXME(sil-serialize-all)
      public mutating func next() -> Unicode.Scalar? {
        if _slowPath(_opaqueIterator != nil) {
          return _opaqueIterator!.next()
        }
        if _asciiIterator != nil {
          return _asciiIterator!.next()
        }
        if _guts._isSmall {
          return _smallIterator!.next()
        }
        return _utf16Iterator!.next()
      }
    }

    /// Returns an iterator over the Unicode scalars that make up this view.
    ///
    /// - Returns: An iterator over this collection's `Unicode.Scalar` elements.
    @inlinable // FIXME(sil-serialize-all)
    public func makeIterator() -> Iterator {
      return Iterator(_guts)
    }

    @inlinable // FIXME(sil-serialize-all)
    public var description: String {
      return String(_guts)
    }

    public var debugDescription: String {
      return "StringUnicodeScalarView(\(self.description.debugDescription))"
    }
  }

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
  @inlinable // FIXME(sil-serialize-all)
  public init(_ unicodeScalars: UnicodeScalarView) {
    self.init(unicodeScalars._guts)
  }

  /// The index type for a string's `unicodeScalars` view.
  public typealias UnicodeScalarIndex = UnicodeScalarView.Index
}

extension _StringGuts {
  @inlinable
  internal func unicodeScalar(startingAt offset: Int) -> Unicode.Scalar {
    return _visitGuts(self, args: offset,
      ascii: { ascii, offset in
        let u = ascii.codeUnit(atCheckedOffset: offset)
        return Unicode.Scalar(_unchecked: UInt32(u)) },
      utf16: { utf16, offset in
        return utf16.unicodeScalar(startingAt: offset) },
      opaque: { opaque, offset in
        return opaque.unicodeScalar(startingAt: offset) })
  }

  @inlinable
  internal func unicodeScalar(endingAt offset: Int) -> Unicode.Scalar {
    return _visitGuts(self, args: offset,
      ascii: { ascii, offset in
        let u = ascii.codeUnit(atCheckedOffset: offset &- 1)
        return Unicode.Scalar(_unchecked: UInt32(u)) },
      utf16: { utf16, offset in
        return utf16.unicodeScalar(endingAt: offset) },
      opaque: { opaque, offset in
        return opaque.unicodeScalar(endingAt: offset) })
  }
}

extension String.UnicodeScalarView : _SwiftStringView {
  @inlinable // FIXME(sil-serialize-all)
  internal var _persistentContent : String { return String(_guts) }

  @inlinable // FIXME(sil-serialize-all)
  var _wholeString : String {
    return String(_guts)
  }

  @inlinable // FIXME(sil-serialize-all)
  var _encodedOffsetRange : Range<Int> {
    return 0..<_guts.count
  }
}

extension String {
  /// The string's value represented as a collection of Unicode scalar values.
  @inlinable // FIXME(sil-serialize-all)
  public var unicodeScalars: UnicodeScalarView {
    get {
      return UnicodeScalarView(_guts)
    }
    set {
      _guts = newValue._guts
    }
  }
}

extension String.UnicodeScalarView : RangeReplaceableCollection {
  /// Creates an empty view instance.
  @inlinable // FIXME(sil-serialize-all)
  public init() {
    self = String.UnicodeScalarView(_StringGuts())
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
    _guts.reserveCapacity(n)
  }

  /// Appends the given Unicode scalar to the view.
  ///
  /// - Parameter c: The character to append to the string.
  public mutating func append(_ c: Unicode.Scalar) {
    if _fastPath(_guts.isASCII && c.value <= 0x7f) {
      _guts.withMutableASCIIStorage(unusedCapacity: 1) { storage in
        unowned(unsafe) let s = storage._value
        s.end.pointee = UInt8(c.value)
        s.count += 1
      }
    } else {
      let width = UTF16.width(c)
      _guts.withMutableUTF16Storage(unusedCapacity: width) { storage in
        unowned(unsafe) let s = storage._value
        _sanityCheck(s.count + width <= s.capacity)
        if _fastPath(width == 1) {
          s.end.pointee = UTF16.CodeUnit(c.value)
        } else {
          _sanityCheck(width == 2)
          s.end[0] = UTF16.leadSurrogate(c)
          s.end[1] = UTF16.trailSurrogate(c)
        }
        s.count += width
      }
    }
  }

  /// Appends the Unicode scalar values in the given sequence to the view.
  ///
  /// - Parameter newElements: A sequence of Unicode scalar values.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the resulting view.
  public mutating func append<S : Sequence>(contentsOf newElements: S)
  where S.Element == Unicode.Scalar {
    // FIXME: Keep ASCII storage if possible
    _guts.reserveUnusedCapacity(newElements.underestimatedCount)
    var it = newElements.makeIterator()
    var next = it.next()
    while let n = next {
      _guts.withMutableUTF16Storage(unusedCapacity: UTF16.width(n)) { storage in
        var p = storage._value.end
        let limit = storage._value.capacityEnd
        while let n = next {
          let w = UTF16.width(n)
          guard p + w <= limit else { break }
          if w == 1 {
            p.pointee = UTF16.CodeUnit(n.value)
          } else {
            _sanityCheck(w == 2)
            p[0] = UTF16.leadSurrogate(n)
            p[1] = UTF16.trailSurrogate(n)
          }
          p += w
          next = it.next()
        }
        storage._value.count = p - storage._value.start
      }
    }
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
    let rawSubRange: Range<Int> = _toCoreIndex(bounds.lowerBound) ..<
      _toCoreIndex(bounds.upperBound)
    let lazyUTF16 = newElements.lazy.flatMap { $0.utf16 }
    _guts.replaceSubrange(rawSubRange, with: lazyUTF16)
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
  ///   - sourcePosition: A position in the `utf16` view of a string. `utf16Index`
  ///     must be an element of `String(unicodeScalars).utf16.indices`.
  ///   - unicodeScalars: The `UnicodeScalarView` in which to find the new
  ///     position.
  @inlinable // FIXME(sil-serialize-all)
  public init?(
    _ sourcePosition: String.UTF16Index,
    within unicodeScalars: String.UnicodeScalarView
  ) {
    if !unicodeScalars._isOnUnicodeScalarBoundary(sourcePosition) { return nil }
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
  @inlinable // FIXME(sil-serialize-all)
  public func samePosition(in characters: String) -> String.Index? {
    return String.Index(self, within: characters)
  }
}

extension String.UnicodeScalarView {
  @inlinable // FIXME(sil-serialize-all)
  internal func _isOnUnicodeScalarBoundary(_ i: Index) -> Bool {
    if _fastPath(_guts.isASCII) { return true }
    if i == startIndex || i == endIndex {
      return true
    }
    if i.transcodedOffset != 0 { return false }
    let i2 = _toCoreIndex(i)
    if _fastPath(
      !UTF16.isTrailSurrogate(_guts.codeUnit(atCheckedOffset: i2))) {
       return true
    }
    return i2 == 0 || !UTF16.isLeadSurrogate(
      _guts.codeUnit(atCheckedOffset:i2 &- 1))
  }

  // NOTE: Don't make this function inlineable.  Grapheme cluster
  // segmentation uses a completely different algorithm in Unicode 9.0.
  @inlinable // FIXME(sil-serialize-all)
  internal func _isOnGraphemeClusterBoundary(_ i: Index) -> Bool {
    if i == startIndex || i == endIndex {
      return true
    }
    if !_isOnUnicodeScalarBoundary(i) { return false }
    let str = String(_guts)
    return i == str.index(before: str.index(after: i))
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

  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4)
  public subscript(bounds: Range<Index>) -> String.UnicodeScalarView.SubSequence {
    return String.UnicodeScalarView.SubSequence(self, _bounds: bounds)
  }
}
