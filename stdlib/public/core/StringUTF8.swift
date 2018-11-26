//===--- StringUTF8.swift - A UTF8 view of String -------------------------===//
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
  /// A view of a string's contents as a collection of UTF-8 code units.
  ///
  /// You can access a string's view of UTF-8 code units by using its `utf8`
  /// property. A string's UTF-8 view encodes the string's Unicode scalar
  /// values as 8-bit integers.
  ///
  ///     let flowers = "Flowers 💐"
  ///     for v in flowers.utf8 {
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
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// A string's Unicode scalar values can be up to 21 bits in length. To
  /// represent those scalar values using 8-bit integers, more than one UTF-8
  /// code unit is often required.
  ///
  ///     let flowermoji = "💐"
  ///     for v in flowermoji.unicodeScalars {
  ///         print(v, v.value)
  ///     }
  ///     // 💐 128144
  ///
  ///     for v in flowermoji.utf8 {
  ///         print(v)
  ///     }
  ///     // 240
  ///     // 159
  ///     // 146
  ///     // 144
  ///
  /// In the encoded representation of a Unicode scalar value, each UTF-8 code
  /// unit after the first is called a *continuation byte*.
  ///
  /// UTF8View Elements Match Encoded C Strings
  /// =========================================
  ///
  /// Swift streamlines interoperation with C string APIs by letting you pass a
  /// `String` instance to a function as an `Int8` or `UInt8` pointer. When you
  /// call a C function using a `String`, Swift automatically creates a buffer
  /// of UTF-8 code units and passes a pointer to that buffer. The code units
  /// of that buffer match the code units in the string's `utf8` view.
  ///
  /// The following example uses the C `strncmp` function to compare the
  /// beginning of two Swift strings. The `strncmp` function takes two
  /// `const char*` pointers and an integer specifying the number of characters
  /// to compare. Because the strings are identical up to the 14th character,
  /// comparing only those characters results in a return value of `0`.
  ///
  ///     let s1 = "They call me 'Bell'"
  ///     let s2 = "They call me 'Stacey'"
  ///
  ///     print(strncmp(s1, s2, 14))
  ///     // Prints "0"
  ///     print(String(s1.utf8.prefix(14)))
  ///     // Prints "They call me '"
  ///
  /// Extending the compared character count to 15 includes the differing
  /// characters, so a nonzero result is returned.
  ///
  ///     print(strncmp(s1, s2, 15))
  ///     // Prints "-17"
  ///     print(String(s1.utf8.prefix(15)))
  ///     // Prints "They call me 'B"
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UTF8View
    : BidirectionalCollection,
      CustomStringConvertible,
      CustomDebugStringConvertible {

    /// Underlying UTF-16-compatible representation
    @usableFromInline
    internal var _guts: _StringGuts

    /// Distances to `(startIndex, endIndex)` from the endpoints of _guts,
    /// measured in UTF-8 code units.
    ///
    /// Note: this is *only* here to support legacy Swift3-style slicing where
    /// `s.utf8[i..<j]` produces a `String.UTF8View`, and should be removed when
    /// those semantics are no longer supported.
    @usableFromInline
    internal let _legacyOffsets: (start: Int8, end: Int8)

    /// Flags indicating whether the limits of this view did not originally fall
    /// on grapheme cluster boundaries in the original string. This is used to
    /// emulate (undocumented) Swift 3 behavior where String.init?(_:) returned
    /// nil in such cases.
    ///
    /// Note: this is *only* here to support legacy Swift3-style slicing where
    /// `s.utf8[i..<j]` produces a `String.UTF8View`, and should be removed when
    /// those semantics are no longer supported.
    @usableFromInline
    internal let _legacyPartialCharacters: (start: Bool, end: Bool)

    @inlinable // FIXME(sil-serialize-all)
    internal init(
      _ _guts: _StringGuts,
      legacyOffsets: (Int, Int) = (0, 0),
      legacyPartialCharacters: (Bool, Bool) = (false, false)
    ) {
      self._guts = _guts
      self._legacyOffsets = (Int8(legacyOffsets.0), Int8(legacyOffsets.1))
      self._legacyPartialCharacters = legacyPartialCharacters
    }

    public typealias Index = String.Index

    /// The position of the first code unit if the UTF-8 view is
    /// nonempty.
    ///
    /// If the UTF-8 view is empty, `startIndex` is equal to `endIndex`.
    @inlinable // FIXME(sil-serialize-all)
    public var startIndex: Index {
      let r = _index(atEncodedOffset: _guts.startIndex)
      if _legacyOffsets.start == 0 { return r }
      return index(r, offsetBy: numericCast(_legacyOffsets.start))
    }

    /// The "past the end" position---that is, the position one
    /// greater than the last valid subscript argument.
    ///
    /// In an empty UTF-8 view, `endIndex` is equal to `startIndex`.
    @inlinable // FIXME(sil-serialize-all)
    public var endIndex: Index {
      _sanityCheck(_legacyOffsets.end >= -3 && _legacyOffsets.end <= 0,
        "out of bounds legacy end")

      var r = Index(encodedOffset: _guts.endIndex)
      if _fastPath(_legacyOffsets.end == 0) {
        return r
      }
      switch _legacyOffsets.end {
      case -3: r = index(before: r); fallthrough
      case -2: r = index(before: r); fallthrough
      case -1: return index(before: r)
      default: Builtin.unreachable()
      }
    }

    @inlinable // FIXME(sil-serialize-all)
    internal func _index(atEncodedOffset n: Int) -> Index {
      if _fastPath(_guts.isASCII) { return Index(encodedOffset: n) }
      let count = _guts.count
      if n == count { return endIndex }

      var p = UTF16.ForwardParser()
      var i = _guts.makeIterator(in: n..<count)
      var buffer = Index._UTF8Buffer()
    Loop:
      while true {
        switch p.parseScalar(from: &i) {
        case .valid(let u16):
          let u8 = Unicode.UTF8.transcode(u16, from: Unicode.UTF16.self)
           ._unsafelyUnwrappedUnchecked
          if buffer.count + u8.count > buffer.capacity { break Loop }
          buffer.append(contentsOf: u8)
        case .error:
          let u8 = Unicode.UTF8.encodedReplacementCharacter
          if buffer.count + u8.count > buffer.capacity { break Loop }
          buffer.append(contentsOf: u8)
        case .emptyInput:
          break Loop
        }
      }
      return Index(encodedOffset: n, .utf8(buffer: buffer))
    }

    /// Returns the next consecutive position after `i`.
    ///
    /// - Precondition: The next position is representable.
    @inlinable // FIXME(sil-serialize-all)
    @inline(__always)
    public func index(after i: Index) -> Index {
      if _fastPath(_guts.isASCII) {
        precondition(i.encodedOffset < _guts.count)
        return Index(encodedOffset: i.encodedOffset + 1)
      }

      var j = i

      // Ensure j's cache is utf8
      if _slowPath(j._cache.utf8 == nil) {
        j = _index(atEncodedOffset: j.encodedOffset)
        precondition(j != endIndex, "Index out of bounds")
      }

      let buffer = j._cache.utf8._unsafelyUnwrappedUnchecked

      var scalarLength16 = 1
      let b0 = buffer.first._unsafelyUnwrappedUnchecked
      var nextBuffer = buffer

      let leading1s = (~b0).leadingZeroBitCount
      if _fastPath(leading1s == 0) { // ASCII in buffer; just consume it
        nextBuffer.removeFirst()
      }
      else {
        // Number of bytes consumed in this scalar
        let n8 = j._transcodedOffset + 1
        // If we haven't reached a scalar boundary...
        if _fastPath(n8 < leading1s) {
          // Advance to the next position in this scalar
          return Index(
            encodedOffset: j.encodedOffset,
            transcodedOffset: n8, .utf8(buffer: buffer))
        }
        // We reached a scalar boundary; compute the underlying utf16's width
        // based on the number of utf8 code units
        scalarLength16 = n8 >> 2 + 1
        nextBuffer.removeFirst(n8)
      }

      if _fastPath(!nextBuffer.isEmpty) {
        return Index(
          encodedOffset: j.encodedOffset + scalarLength16,
          .utf8(buffer: nextBuffer))
      }
      // If nothing left in the buffer, refill it.
      return _index(atEncodedOffset: j.encodedOffset + scalarLength16)
    }

    @inlinable // FIXME(sil-serialize-all)
    public func index(before i: Index) -> Index {
      if _fastPath(_guts.isASCII) {
        precondition(i.encodedOffset > 0)
        return Index(encodedOffset: i.encodedOffset - 1)
      }

      if i._transcodedOffset != 0 {
        _sanityCheck(i._cache.utf8 != nil)
        var r = i
        r._compoundOffset = r._compoundOffset &- 1
        return r
      }

      // Handle the scalar boundary the same way as the not-a-utf8-index case.
      _precondition(i.encodedOffset > 0, "Can't move before startIndex")

      // Parse a single scalar
      let u = _guts.unicodeScalar(endingAt: i.encodedOffset)
      let u8 = Unicode.UTF8.encode(u)._unsafelyUnwrappedUnchecked
      return Index(
        encodedOffset: i.encodedOffset &- (u8.count < 4 ? 1 : 2),
        transcodedOffset: u8.count &- 1,
        .utf8(buffer: String.Index._UTF8Buffer(u8))
      )
    }

    @inlinable // FIXME(sil-serialize-all)
    public func distance(from i: Index, to j: Index) -> Int {
      if _fastPath(_guts.isASCII) {
        return j.encodedOffset - i.encodedOffset
      }
      return j >= i
        ? _forwardDistance(from: i, to: j) : -_forwardDistance(from: j, to: i)
    }

    @inlinable // FIXME(sil-serialize-all)
    @inline(__always)
    internal func _forwardDistance(from i: Index, to j: Index) -> Int {
      return j._transcodedOffset - i._transcodedOffset +
        String.UTF8View._count(fromUTF16: IteratorSequence(_guts.makeIterator(
          in: i.encodedOffset..<j.encodedOffset)))
    }

    /// Accesses the code unit at the given position.
    ///
    /// The following example uses the subscript to print the value of a
    /// string's first UTF-8 code unit.
    ///
    ///     let greeting = "Hello, friend!"
    ///     let i = greeting.utf8.startIndex
    ///     print("First character's UTF-8 code unit: \(greeting.utf8[i])")
    ///     // Prints "First character's UTF-8 code unit: 72"
    ///
    /// - Parameter position: A valid index of the view. `position`
    ///   must be less than the view's end index.
    @inlinable // FIXME(sil-serialize-all)
    public subscript(position: Index) -> UTF8.CodeUnit {
      @inline(__always)
      get {
        if _fastPath(_guts.isASCII) {
          let ascii = _guts._unmanagedASCIIView
          let offset = position.encodedOffset
          _precondition(offset < ascii.count, "Index out of bounds")
          return ascii.buffer[position.encodedOffset]
        }
        var j = position
        while true {
          if case .utf8(let buffer) = j._cache {
            _onFastPath()
            return buffer[
              buffer.index(buffer.startIndex, offsetBy: j._transcodedOffset)]
          }
          j = _index(atEncodedOffset: j.encodedOffset)
          precondition(j < endIndex, "Index out of bounds")
        }
      }
    }

    @inlinable // FIXME(sil-serialize-all)
    public var description: String {
      return String(_guts)
    }

    @inlinable // FIXME(sil-serialize-all)
    public var debugDescription: String {
      return "UTF8View(\(self.description.debugDescription))"
    }
  }

  /// A UTF-8 encoding of `self`.
  @inlinable // FIXME(sil-serialize-all)
  public var utf8: UTF8View {
    get {
      return UTF8View(self._guts)
    }
    set {
      self = String(describing: newValue)
    }
  }

  /// A contiguously stored null-terminated UTF-8 representation of the string.
  ///
  /// To access the underlying memory, invoke `withUnsafeBufferPointer` on the
  /// array.
  ///
  ///     let s = "Hello!"
  ///     let bytes = s.utf8CString
  ///     print(bytes)
  ///     // Prints "[72, 101, 108, 108, 111, 33, 0]"
  ///
  ///     bytes.withUnsafeBufferPointer { ptr in
  ///         print(strlen(ptr.baseAddress!))
  ///     }
  ///     // Prints "6"
  @inlinable // FIXME(sil-serialize-all)
  public var utf8CString: ContiguousArray<CChar> {
    var result = ContiguousArray<CChar>()
    result.reserveCapacity(utf8.count + 1)
    for c in utf8 {
      result.append(CChar(bitPattern: c))
    }
    result.append(0)
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _withUnsafeBufferPointerToUTF8<R>(
    _ body: (UnsafeBufferPointer<UTF8.CodeUnit>) throws -> R
  ) rethrows -> R {
    if _guts.isASCII {
      return try body(_guts._unmanagedASCIIView.buffer)
    }
    var nullTerminatedUTF8 = ContiguousArray<UTF8.CodeUnit>()
    nullTerminatedUTF8.reserveCapacity(utf8.count + 1)
    nullTerminatedUTF8 += utf8
    nullTerminatedUTF8.append(0)
    return try nullTerminatedUTF8.withUnsafeBufferPointer(body)
  }

  /// Creates a string corresponding to the given sequence of UTF-8 code units.
  ///
  /// If `utf8` is an ill-formed UTF-8 code sequence, the result is `nil`.
  ///
  /// You can use this initializer to create a new string from a slice of
  /// another string's `utf8` view.
  ///
  ///     let picnicGuest = "Deserving porcupine"
  ///     if let i = picnicGuest.utf8.firstIndex(of: 32) {
  ///         let adjective = String(picnicGuest.utf8[..<i])
  ///         print(adjective)
  ///     }
  ///     // Prints "Optional(Deserving)"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.utf8` view.
  ///
  /// - Parameter utf8: A UTF-8 code sequence.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, deprecated: 3.2,
    message: "Failable initializer was removed in Swift 4. When upgrading to Swift 4, please use non-failable String.init(_:UTF8View)")
  @available(swift, obsoleted: 4.0,
    message: "Please use non-failable String.init(_:UTF8View) instead")
  public init?(_ utf8: UTF8View) {
    if utf8.startIndex._transcodedOffset != 0
      || utf8.endIndex._transcodedOffset != 0
      || utf8._legacyPartialCharacters.start
      || utf8._legacyPartialCharacters.end {
      return nil
    }
    self = String(utf8._guts)
  }

  /// Creates a string corresponding to the given sequence of UTF-8 code units.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4.0, message:
    "Please use failable String.init?(_:UTF8View) when in Swift 3.2 mode")
  public init(_ utf8: UTF8View) {
    self = String(utf8._guts)
  }

  /// The index type for subscripting a string.
  public typealias UTF8Index = UTF8View.Index
}

extension String.UTF8View : _SwiftStringView {
  @inlinable // FIXME(sil-serialize-all)
  internal var _persistentContent : String {
    return String(self._guts)
  }

  @inlinable // FIXME(sil-serialize-all)
  var _wholeString : String {
    return String(_guts)
  }

  @inlinable // FIXME(sil-serialize-all)
  var _encodedOffsetRange : Range<Int> {
    return 0..<_guts.count
  }
}

extension String.UTF8View {
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    internal typealias _OutputBuffer = _ValidUTF8Buffer<UInt64>
    @usableFromInline
    internal let _guts: _StringGuts
    @usableFromInline
    internal let _endOffset: Int
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _nextOffset: Int
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _buffer: _OutputBuffer
  }

  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension String.UTF8View.Iterator : IteratorProtocol {
  public typealias Element = String.UTF8View.Element

  @inlinable // FIXME(sil-serialize-all)
  internal init(_ utf8: String.UTF8View) {
    self._guts = utf8._guts
    self._nextOffset = 0
    self._buffer = _OutputBuffer()
    self._endOffset = utf8._guts.count
  }

  @inlinable // FIXME(sil-serialize-all)
  public mutating func next() -> Unicode.UTF8.CodeUnit? {
    if _slowPath(_nextOffset == _endOffset) {
      if _slowPath(_buffer.isEmpty) {
        return nil
      }
    }
    if _guts.isASCII {
      defer { _nextOffset += 1 }
      return _guts._unmanagedASCIIView.buffer[_nextOffset]
    }
    if _guts._isSmall {
      defer { _nextOffset += 1 }
      return _guts._smallUTF8String[_nextOffset]
    }

    if _fastPath(!_buffer.isEmpty) {
      return _buffer.removeFirst()
    }
    return _fillBuffer()
  }

  @usableFromInline
  @inline(never)
  internal mutating func _fillBuffer() -> Unicode.UTF8.CodeUnit {
    _sanityCheck(!_guts.isASCII, "next() already checks for known ASCII")
    if _slowPath(_guts._isOpaque) {
      return _opaqueFillBuffer()
    }

    defer { _fixLifetime(_guts) }
    return _fillBuffer(from: _guts._unmanagedUTF16View)
  }

  @usableFromInline // @opaque
  internal mutating func _opaqueFillBuffer() -> Unicode.UTF8.CodeUnit {
    _sanityCheck(_guts._isOpaque)
    defer { _fixLifetime(_guts) }
    return _fillBuffer(from: _guts._asOpaque())
  }

  // NOT @usableFromInline
  internal mutating func _fillBuffer<V: _StringVariant>(
    from variant: V
  ) -> Unicode.UTF8.CodeUnit {
    // Eat as many ASCII characters as possible
    let asciiEnd = Swift.min(_nextOffset + _buffer.capacity, _endOffset)
    for cu in variant[_nextOffset..<asciiEnd] {
      if !UTF16._isASCII(cu) { break }
      _buffer.append(UInt8(truncatingIfNeeded: cu))
      _nextOffset += 1
    }
    if _nextOffset == asciiEnd {
      return _buffer.removeFirst()
    }
    // Decode UTF-16, encode UTF-8
    for scalar in IteratorSequence(
      variant[_nextOffset..<_endOffset].makeUnicodeScalarIterator()) {
      let u8 = UTF8.encode(scalar)._unsafelyUnwrappedUnchecked
      let c8 = u8.count
      guard _buffer.count + c8 <= _buffer.capacity else { break }
      _buffer.append(contentsOf: u8)
      _nextOffset += 1 &+ (c8 &>> 2)
    }
    return _buffer.removeFirst()
  }
}

extension String.UTF8View {
  @inlinable // FIXME(sil-serialize-all)
  internal static func _count<Source: Sequence>(fromUTF16 source: Source) -> Int
  where Source.Element == Unicode.UTF16.CodeUnit
  {
    var result = 0
    var prev: Unicode.UTF16.CodeUnit = 0
    for u in source {
      switch u {
      case 0..<0x80: result += 1
      case 0x80..<0x800: result += 2
      case 0x800..<0xDC00: result += 3
      case 0xDC00..<0xE000: result += UTF16.isLeadSurrogate(prev) ? 1 : 3
      default: result += 3
      }
      prev = u
    }
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  public var count: Int {
    if _fastPath(_guts.isASCII) { return _guts.count }
    return _visitGuts(_guts,
      ascii: { ascii in return ascii.count },
      utf16: { utf16 in return String.UTF8View._count(fromUTF16: utf16) },
      opaque: { opaque in return String.UTF8View._count(fromUTF16: opaque) })
  }
}

// Index conversions
extension String.UTF8View.Index {
  /// Creates an index in the given UTF-8 view that corresponds exactly to the
  /// specified `UTF16View` position.
  ///
  /// The following example finds the position of a space in a string's `utf16`
  /// view and then converts that position to an index in the string's
  /// `utf8` view.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let utf16Index = cafe.utf16.firstIndex(of: 32)!
  ///     let utf8Index = String.UTF8View.Index(utf16Index, within: cafe.utf8)!
  ///
  ///     print(Array(cafe.utf8[..<utf8Index]))
  ///     // Prints "[67, 97, 102, 195, 169]"
  ///
  /// If the position passed in `utf16Index` doesn't have an exact
  /// corresponding position in `utf8`, the result of the initializer is
  /// `nil`. For example, because UTF-8 and UTF-16 represent high Unicode code
  /// points differently, an attempt to convert the position of the trailing
  /// surrogate of a UTF-16 surrogate pair fails.
  ///
  /// The next example attempts to convert the indices of the two UTF-16 code
  /// points that represent the teacup emoji (`"🍵"`). The index of the lead
  /// surrogate is successfully converted to a position in `utf8`, but the
  /// index of the trailing surrogate is not.
  ///
  ///     let emojiHigh = cafe.utf16.index(after: utf16Index)
  ///     print(String.UTF8View.Index(emojiHigh, within: cafe.utf8))
  ///     // Prints "Optional(String.Index(...))"
  ///
  ///     let emojiLow = cafe.utf16.index(after: emojiHigh)
  ///     print(String.UTF8View.Index(emojiLow, within: cafe.utf8))
  ///     // Prints "nil"
  ///
  /// - Parameters:
  ///   - sourcePosition: A position in a `String` or one of its views.
  ///   - target: The `UTF8View` in which to find the new position.
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ sourcePosition: String.Index, within target: String.UTF8View) {
    switch sourcePosition._cache {
    case .utf8:
      self.init(encodedOffset: sourcePosition.encodedOffset,
        transcodedOffset:sourcePosition._transcodedOffset, sourcePosition._cache)

    default:
      guard String.UnicodeScalarView(target._guts)._isOnUnicodeScalarBoundary(
        sourcePosition) else { return nil }
      self.init(encodedOffset: sourcePosition.encodedOffset)
    }
  }
}

// Reflection
extension String.UTF8View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-8 view of a string.
  @inlinable // FIXME(sil-serialize-all)
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UTF8View : CustomPlaygroundQuickLookable {
  @inlinable // FIXME(sil-serialize-all)
  @available(*, deprecated, message: "UTF8View.customPlaygroundQuickLook will be removed in a future Swift version")
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

// backward compatibility for index interchange.
extension String.UTF8View {
  @inlinable // FIXME(sil-serialize-all)
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public func index(after i: Index?) -> Index {
    return index(after: i!)
  }
  @inlinable // FIXME(sil-serialize-all)
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public func index(_ i: Index?, offsetBy n: Int) -> Index {
    return index(i!, offsetBy: n)
  }
  @inlinable // FIXME(sil-serialize-all)
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional indices")
  public func distance(
    from i: Index?, to j: Index?) -> Int {
    return distance(from: i!, to: j!)
  }
  @inlinable // FIXME(sil-serialize-all)
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public subscript(i: Index?) -> Unicode.UTF8.CodeUnit {
    return self[i!]
  }
}

//===--- Slicing Support --------------------------------------------------===//
/// In Swift 3.2, in the absence of type context,
///
///   someString.utf8[someString.utf8.startIndex..<someString.utf8.endIndex]
///
/// was deduced to be of type `String.UTF8View`.  Provide a more-specific
/// Swift-3-only `subscript` overload that continues to produce
/// `String.UTF8View`.
extension String.UTF8View {
  public typealias SubSequence = Substring.UTF8View

  @inlinable // FIXME(sil-serialize-all)
  @available(swift, introduced: 4)
  public subscript(r: Range<Index>) -> String.UTF8View.SubSequence {
    return String.UTF8View.SubSequence(self, _bounds: r)
  }

  @inlinable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4)
  public subscript(r: Range<Index>) -> String.UTF8View {
    let wholeString = String(_guts)
    let legacyPartialCharacters = (
      (self._legacyPartialCharacters.start &&
        r.lowerBound.encodedOffset == 0) ||
      r.lowerBound.samePosition(in: wholeString) == nil,
      (self._legacyPartialCharacters.end &&
        r.upperBound.encodedOffset == _guts.count) ||
      r.upperBound.samePosition(in: wholeString) == nil)

    if r.upperBound._transcodedOffset == 0 {
      return String.UTF8View(
        _guts._extractSlice(
        r.lowerBound.encodedOffset..<r.upperBound.encodedOffset),
        legacyOffsets: (r.lowerBound._transcodedOffset, 0),
        legacyPartialCharacters: legacyPartialCharacters)
    }

    let b0 = r.upperBound._cache.utf8!.first!
    let scalarLength8 = (~b0).leadingZeroBitCount
    let scalarLength16 = scalarLength8 == 4 ? 2 : 1
    let coreEnd = r.upperBound.encodedOffset + scalarLength16
    return String.UTF8View(
      _guts._extractSlice(r.lowerBound.encodedOffset..<coreEnd),
      legacyOffsets: (
        r.lowerBound._transcodedOffset,
        r.upperBound._transcodedOffset - scalarLength8),
      legacyPartialCharacters: legacyPartialCharacters)
  }

  @inlinable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4)
  public subscript(bounds: ClosedRange<Index>) -> String.UTF8View {
    return self[bounds.relative(to: self)]
  }
}
