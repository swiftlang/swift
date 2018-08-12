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
      let r: Index
      if _fastPath(_guts._isASCIIOrSmallASCII) {
        r = Index(encodedOffset: 0)
      } else {
        r =  _nonASCIIIndex(atEncodedOffset: 0)
      }
      _sanityCheck(r.encodedOffset == 0)
      if _fastPath(_legacyOffsets.start == 0) { return r }

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

    @inline(never)
    @_effects(releasenone)
    @usableFromInline
    internal func _nonASCIIIndex(atEncodedOffset n: Int) -> Index {
      _sanityCheck(!_guts._isASCIIOrSmallASCII)
      let count = _guts.count
      if n == count { return endIndex }
      let buffer: Index._UTF8Buffer = _visitGuts(
        _guts, range: (n..<count, performBoundsCheck: true),
        ascii: { _ in
          Builtin.unreachable()
          /* return Index._UTF8Buffer() */ },
        utf16: { utf16 in
          var i = utf16.makeIterator()
          return UTF8View._fillBuffer(from: &i) },
        opaque: { opaque in
          var i = opaque.makeIterator()
          return UTF8View._fillBuffer(from: &i)}
      )

      return Index(encodedOffset: n, transcodedOffset: 0, buffer: buffer)
    }

    @inline(__always)
    internal
    static func _fillBuffer<Iter: IteratorProtocol>(
      from i: inout Iter
    ) -> Index._UTF8Buffer where Iter.Element == UInt16 {
      var p = UTF16.ForwardParser()
      var buffer = Index._UTF8Buffer()
      while true {
        switch p.parseScalar(from: &i) {
        case .valid(let u16):
          let u8 = Unicode.UTF8.transcode(u16, from: Unicode.UTF16.self)
           ._unsafelyUnwrappedUnchecked
          if buffer.count + u8.count > buffer.capacity {
            return buffer
          }
          buffer.append(contentsOf: u8)
        case .error:
          let u8 = Unicode.UTF8.encodedReplacementCharacter
          if buffer.count + u8.count > buffer.capacity {
            return buffer
          }
          buffer.append(contentsOf: u8)
        case .emptyInput:
          return buffer
        }
      }
    }

    /// Returns the next consecutive position after `i`.
    ///
    /// - Precondition: The next position is representable.
    @inlinable // FIXME(sil-serialize-all)
    @inline(__always)
    public func index(after i: Index) -> Index {
      if _fastPath(_guts._isASCIIOrSmallASCII) {
        precondition(i.encodedOffset < _guts.count)
        return Index(encodedOffset: i.encodedOffset + 1)
      }

      return _nonASCIIIndex(after: i)
    }

    @inline(never)
    @_effects(releasenone)
    @usableFromInline
    internal func _nonASCIIIndex(after i: Index) -> Index {
      _sanityCheck(!_guts._isASCIIOrSmallASCII)

      var j = i

      // Ensure j's cache is utf8
      if _slowPath(j.utf8Buffer == nil) {
        j = _nonASCIIIndex(atEncodedOffset: j.encodedOffset)
        precondition(j != endIndex, "Index out of bounds")
      }

      let buffer = j.utf8Buffer._unsafelyUnwrappedUnchecked

      var scalarLength16 = 1
      let b0 = buffer.first._unsafelyUnwrappedUnchecked
      var nextBuffer = buffer

      let leading1s = (~b0).leadingZeroBitCount
      if _fastPath(leading1s == 0) { // ASCII in buffer; just consume it
        nextBuffer.removeFirst()
      }
      else {
        // Number of bytes consumed in this scalar
        let n8 = j.transcodedOffset + 1
        // If we haven't reached a scalar boundary...
        if _fastPath(n8 < leading1s) {
          // Advance to the next position in this scalar
          return Index(
            encodedOffset: j.encodedOffset,
            transcodedOffset: n8, buffer: buffer)
        }
        // We reached a scalar boundary; compute the underlying utf16's width
        // based on the number of utf8 code units
        scalarLength16 = n8 >> 2 + 1
        nextBuffer.removeFirst(n8)
      }

      if _fastPath(!nextBuffer.isEmpty) {
        return Index(
          encodedOffset: j.encodedOffset + scalarLength16,
          transcodedOffset: 0,
          buffer: nextBuffer)
      }
      // If nothing left in the buffer, refill it.
      return _nonASCIIIndex(atEncodedOffset: j.encodedOffset + scalarLength16)
    }

    @inlinable // FIXME(sil-serialize-all)
    public func index(before i: Index) -> Index {
      if _fastPath(_guts._isASCIIOrSmallASCII) {
        precondition(i.encodedOffset > 0)
        return Index(encodedOffset: i.encodedOffset - 1)
      }

      return _nonASCIIIndex(before: i)
    }

    @inline(never)
    @_effects(releasenone)
    @usableFromInline
    internal func _nonASCIIIndex(before i: Index) -> Index {
      _sanityCheck(!_guts._isASCIIOrSmallASCII)
      if i.transcodedOffset != 0 {
        _sanityCheck(i.utf8Buffer != nil)
        return Index(
          encodedOffset: i.encodedOffset,
          transcodedOffset: i.transcodedOffset &- 1,
          buffer: i.utf8Buffer._unsafelyUnwrappedUnchecked)
      }

      // Handle the scalar boundary the same way as the not-a-utf8-index case.
      _precondition(i.encodedOffset > 0, "Can't move before startIndex")

      // Parse a single scalar
      let u = _guts.unicodeScalar(endingAt: i.encodedOffset)
      let u8 = Unicode.UTF8.encode(u)._unsafelyUnwrappedUnchecked
      return Index(
        encodedOffset: i.encodedOffset &- (u8.count < 4 ? 1 : 2),
        transcodedOffset: u8.count &- 1,
        buffer: String.Index._UTF8Buffer(u8))
    }

    @inlinable // FIXME(sil-serialize-all)
    public func distance(from i: Index, to j: Index) -> Int {
      if _fastPath(_guts._isASCIIOrSmallASCII) {
        return j.encodedOffset - i.encodedOffset
      }
      return _nonASCIIDistance(from: i, to: j)
    }

    @inline(never)
    @_effects(releasenone)
    @usableFromInline
    internal func _nonASCIIDistance(from i: Index, to j: Index) -> Int {
      let forwards = j >= i

      let start, end: Index
      if forwards {
        start = i
        end = j
      } else {
        start = j
        end = i
      }
      let countAbs = end.transcodedOffset - start.transcodedOffset
        + _gutsNonASCIIUTF8Count(start.encodedOffset..<end.encodedOffset)
      return forwards ? countAbs : -countAbs
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
        if _fastPath(_guts._isASCIIOrSmallASCII) {
          let offset = position.encodedOffset
          _precondition(offset < _guts.count, "Index out of bounds")

          if _guts._isSmall {
            return _guts._smallUTF8String[offset]
          }
          return _guts._unmanagedASCIIView.buffer[offset]
        }

        return _nonASCIISubscript(position: position)
      }
    }

    @inline(never)
    @_effects(releasenone)
    @usableFromInline
    internal func _nonASCIISubscript(position: Index) -> UTF8.CodeUnit {
      _sanityCheck(!_guts._isASCIIOrSmallASCII)
      var j = position
      while true {
        if let buffer = j.utf8Buffer {
          _onFastPath()
          return buffer[
            buffer.index(buffer.startIndex, offsetBy: j.transcodedOffset)]
        }
        j = _nonASCIIIndex(atEncodedOffset: j.encodedOffset)
        precondition(j < endIndex, "Index out of bounds")
      }
    }

    @inlinable // FIXME(sil-serialize-all)
    public var description: String {
      return String(_guts)
    }

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
    @usableFromInline
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

  internal mutating func _clear() {
    self._nextOffset = self._endOffset
    self._buffer = _OutputBuffer()
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

// Used to calculate a running count. For non-BMP scalars, it's important if the
// prior code unit was a leading surrogate (validity).
internal func _utf8Count(_ utf16CU: UInt16, prev: UInt16) -> Int {
  switch utf16CU {
  case 0..<0x80: return 1
  case 0x80..<0x800: return 2
  case 0x800..<0xDC00: return 3
  case 0xDC00..<0xE000: return UTF16.isLeadSurrogate(prev) ? 1 : 3
  default: return 3
  }
}

extension String.UTF8View {
  internal static func _count<Source: RandomAccessCollection>(
    fromUTF16 source: Source
  ) -> Int where Source.Element == Unicode.UTF16.CodeUnit {
    var result = 0
    var prev: Unicode.UTF16.CodeUnit = 0
    for u in source {
      result += _utf8Count(u, prev: prev)
      prev = u
    }
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  public var count: Int {
    let gutsCount = _guts.count
    if _fastPath(_guts._isASCIIOrSmallASCII) { return gutsCount }
    return _gutsNonASCIIUTF8Count(0..<gutsCount)
  }

  @inline(never)
  @_effects(releasenone)
  @usableFromInline
  internal func _gutsNonASCIIUTF8Count(
    _ range: Range<Int>
  ) -> Int {
    _sanityCheck(!_guts._isASCIIOrSmallASCII)
    return _visitGuts(_guts, range: (range, performBoundsCheck: true),
      ascii: { ascii in return ascii.count },
      utf16: { utf16 in return String.UTF8View._count(fromUTF16: utf16) },
      opaque: { opaque in return String.UTF8View._count(fromUTF16: opaque) }
    )
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
  public init?(_ idx: String.Index, within target: String.UTF8View) {
    guard idx.isUTF8 ||
          String.UnicodeScalarView(target._guts)._isOnUnicodeScalarBoundary(idx)
    else {
      return nil
    }

    self = idx
  }
}

// Reflection
extension String.UTF8View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-8 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
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
}

extension String.UTF8View {
  /// Copies `self` into the supplied buffer.
  ///
  /// - Precondition: The memory in `self` is uninitialized. The buffer must
  ///   contain sufficient uninitialized memory to accommodate `source.underestimatedCount`.
  ///
  /// - Postcondition: The `Pointee`s at `buffer[startIndex..<returned index]` are
  ///   initialized.
  public func _copyContents(
    initializing buffer: UnsafeMutableBufferPointer<Iterator.Element>
  ) -> (Iterator,UnsafeMutableBufferPointer<Iterator.Element>.Index) {
    guard var ptr = buffer.baseAddress else {
        _preconditionFailure(
          "Attempt to copy string contents into nil buffer pointer")
    }
    var it = self.makeIterator()

    if _guts.isASCII {
      defer { _fixLifetime(_guts) }
      let asciiView = _guts._unmanagedASCIIView
      _precondition(asciiView.count <= buffer.count,
        "Insufficient space allocated to copy string contents")
      ptr.initialize(from: asciiView.start, count: asciiView.count)
      it._clear()
      return (it, buffer.index(buffer.startIndex, offsetBy: asciiView.count))
    }
    else {
      for idx in buffer.startIndex..<buffer.count {
        guard let x = it.next() else {
          return (it, idx)
        }
        ptr.initialize(to: x)
        ptr += 1
      }
      return (it,buffer.endIndex)
    }
  }
}
