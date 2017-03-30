//===--- StringUTF8.swift - A UTF8 view of _StringCore --------------------===//
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
//
//  _StringCore currently has three representations: Native ASCII,
//  Native UTF-16, and Opaque Cocoa.  Expose each of these as UTF-8 in a
//  way that will hopefully be efficient to traverse
//
//===----------------------------------------------------------------------===//

// FIXME(ABI)#72 : The UTF-8 string view should conform to
// `BidirectionalCollection`.

// FIXME(ABI)#73 : The UTF-8 string view should have a custom iterator type to
// allow performance optimizations of linear traversals.

extension _StringCore {
  /// An integral type that holds a sequence of UTF-8 code units, starting in
  /// its low byte.
  internal typealias _UTF8Chunk = UInt64

  /// Encode text starting at `i` as UTF-8.  Returns a pair whose first
  /// element is the index of the text following whatever got encoded,
  /// and the second element contains the encoded UTF-8 starting in its
  /// low byte.  Any unused high bytes in the result will be set to
  /// 0xFF.
  func _encodeSomeUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(i <= count)

    if let asciiBuffer = self.asciiBuffer {
      // How many UTF-16 code units might we use before we've filled up
      // our _UTF8Chunk with UTF-8 code units?
      let utf16Count =
        Swift.min(MemoryLayout<_UTF8Chunk>.size, asciiBuffer.count - i)

      var result: _UTF8Chunk = ~0 // Start with all bits set

      _memcpy(
        dest: UnsafeMutableRawPointer(Builtin.addressof(&result)),
        src: asciiBuffer.baseAddress! + i,
        size: numericCast(utf16Count))

      // Convert the _UTF8Chunk into host endianness.
      return (i + utf16Count, _UTF8Chunk(littleEndian: result))
    } else if _fastPath(_baseAddress != nil) {
      // Transcoding should return a _UTF8Chunk in host endianness.
      return _encodeSomeContiguousUTF16AsUTF8(from: i)
    } else {
#if _runtime(_ObjC)
      return _encodeSomeNonContiguousUTF16AsUTF8(from: i)
#else
      _sanityCheckFailure("_encodeSomeUTF8: Unexpected cocoa string")
#endif
    }
  }

  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is contiguous UTF-16.
  func _encodeSomeContiguousUTF16AsUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress != nil)

    let storage = UnsafeBufferPointer(start: startUTF16, count: self.count)
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }

#if _runtime(_ObjC)
  /// Helper for `_encodeSomeUTF8`, above.  Handles the case where the
  /// storage is non-contiguous UTF-16.
  func _encodeSomeNonContiguousUTF16AsUTF8(from i: Int) -> (Int, _UTF8Chunk) {
    _sanityCheck(elementWidth == 2)
    _sanityCheck(_baseAddress == nil)

    let storage = _CollectionOf<Int, UInt16>(
      _startIndex: 0, endIndex: self.count
    ) {
      (i: Int) -> UInt16 in
      return _cocoaStringSubscript(self, i)
    }
    return _transcodeSomeUTF16AsUTF8(storage, i)
  }
#endif
}

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
  public struct UTF8View
    : Collection, 
      CustomStringConvertible, 
      CustomDebugStringConvertible {
    internal let _core: _StringCore
    internal let _startIndex: Index
    internal let _endIndex: Index

    init(_ _core: _StringCore) {
      self._core = _core
      self._endIndex = Index(_coreIndex: _core.endIndex, Index._emptyBuffer)
      if _fastPath(_core.count != 0) {
        let (_, buffer) = _core._encodeSomeUTF8(from: 0)
        self._startIndex = Index(_coreIndex: 0, buffer)
      } else {
        self._startIndex = self._endIndex
      }
    }

    init(_ _core: _StringCore, _ s: Index, _ e: Index) {
      self._core = _core
      self._startIndex = s
      self._endIndex = e
    }

    // If not overridden, the default implementation provided by the Collection
    // would be used, which unnecessarily penalizes algorithms that use
    // UTF8View as a Sequence.
    public var underestimatedCount: Int {
      return _core.count
    }

    /// A position in a string's `UTF8View` instance.
    ///
    /// You can convert between indices of the different string views by using
    /// conversion initializers and the `samePosition(in:)` method overloads.
    /// For example, the following code sample finds the index of the first
    /// space in the string's character view and then converts that to the same
    /// position in the UTF-8 view.
    ///
    ///     let hearts = "Hearts <3 ♥︎ 💘"
    ///     if let i = hearts.characters.index(of: " ") {
    ///         let j = i.samePosition(in: hearts.utf8)
    ///         print(Array(hearts.utf8.prefix(upTo: j)))
    ///         print(hearts.utf8.prefix(upTo: j))
    ///     }
    ///     // Prints "[72, 101, 97, 114, 116, 115]"
    ///     // Prints "Hearts"
    public struct Index {
      internal typealias Buffer = _StringCore._UTF8Chunk

      init(_coreIndex: Int, _ _buffer: Buffer) {
        self._coreIndex = _coreIndex
        self._buffer = _buffer
      }

      /// True iff the index is at the end of its view or if the next
      /// byte begins a new UnicodeScalar.
      internal func _isOnUnicodeScalarBoundary(in core: _StringCore) -> Bool {
        let buffer = UInt32(extendingOrTruncating: _buffer)
        let (codePoint, _) = UTF8._decodeOne(buffer)
        return codePoint != nil || _isEndIndex(of: core)
      }

      /// True iff the index is at the end of its view
      internal func _isEndIndex(of core: _StringCore) -> Bool {
        return _buffer == Index._emptyBuffer
          && _coreIndex == core.endIndex
      }

      /// The number of UTF-8 code units remaining in the buffer before the
      /// next unicode scalar value is reached. This simulates calling
      /// `index(after: i)` until `i._coreIndex` is incremented, but doesn't
      /// need a `_core` reference.
      internal var _utf8ContinuationBytesUntilNextUnicodeScalar: Int {
        var buffer = _buffer
        var count = 0
        
        while true {
          let currentUnit = UTF8.CodeUnit(extendingOrTruncating: buffer)
          if currentUnit & 0b1100_0000 != 0b1000_0000 {
            break
          }
          count += 1
          buffer = Index._nextBuffer(after: buffer)
        }
        return count
      }

      /// The value of the buffer when it is empty
      internal static var _emptyBuffer: Buffer {
        return ~0
      }

      /// A Buffer value with the high byte set
      internal static var _bufferHiByte: Buffer {
        return 0xFF &<< ((MemoryLayout<Buffer>.size &- 1) &* 8)
      }
      
      /// Consume a byte of the given buffer: shift out the low byte
      /// and put FF in the high byte
      internal static func _nextBuffer(after thisBuffer: Buffer) -> Buffer {
        return (thisBuffer &>> (8 as Buffer)) | _bufferHiByte
      }

      /// The position of `self`, rounded up to the nearest unicode
      /// scalar boundary, in the underlying UTF-16.
      internal let _coreIndex: Int
      /// If `self` is at the end of its `_core`, has the value `_emptyBuffer`.
      /// Otherwise, the low byte contains the value of the UTF-8 code unit
      /// at this position.
      internal let _buffer: Buffer
    }

    public typealias IndexDistance = Int

    /// The position of the first code unit if the UTF-8 view is
    /// nonempty.
    ///
    /// If the UTF-8 view is empty, `startIndex` is equal to `endIndex`.
    public var startIndex: Index {
      return self._startIndex
    }

    /// The "past the end" position---that is, the position one
    /// greater than the last valid subscript argument.
    ///
    /// In an empty UTF-8 view, `endIndex` is equal to `startIndex`.
    public var endIndex: Index {
      return self._endIndex
    }

    /// Returns the next consecutive position after `i`.
    ///
    /// - Precondition: The next position is representable.
    public func index(after i: Index) -> Index {
      // FIXME: swift-3-indexing-model: range check i?
      let currentUnit = UTF8.CodeUnit(extendingOrTruncating: i._buffer)
      let hiNibble = currentUnit &>> (4 as UTF8.CodeUnit)

      // Amounts to increment the UTF-16 index based on the high nibble of a
      // UTF-8 code unit. If the high nibble is:
      //
      // - 0b0000-0b0111: U+0000...U+007F: increment the UTF-16 pointer by 1
      // - 0b1000-0b1011: UTF-8 continuation byte, do not increment 
      //                  the UTF-16 pointer
      // - 0b1100-0b1110: U+0080...U+FFFF: increment the UTF-16 pointer by 1
      // - 0b1111:        U+10000...U+1FFFFF: increment the UTF-16 pointer by 2
      let u16Increments = Int(bitPattern:
      // 1111 1110 1101 1100 1011 1010 1001 1000 0111 0110 0101 0100 0011 0010 0001 0000
         0b10___01___01___01___00___00___00___00___01___01___01___01___01___01___01___01)
      
      // Map the high nibble of the current code unit into the
      // amount by which to increment the UTF-16 index.
      let increment = (u16Increments &>>
        Int(extendingOrTruncating: hiNibble &<< (1 as UTF8.CodeUnit))) & 0x3
      let nextCoreIndex = i._coreIndex &+ increment
      let nextBuffer = Index._nextBuffer(after: i._buffer)

      // If the nextBuffer is nonempty, we have all we need
      if _fastPath(nextBuffer != Index._emptyBuffer) {
        return Index(_coreIndex: nextCoreIndex, nextBuffer)
      }
      // If the underlying UTF16 isn't exhausted, fill a new buffer
      else if _fastPath(nextCoreIndex < _core.endIndex) {
        let (_, freshBuffer) = _core._encodeSomeUTF8(from: nextCoreIndex)
        return Index(_coreIndex: nextCoreIndex, freshBuffer)
      }
      else {
        // Produce the endIndex
        _precondition(
          nextCoreIndex == _core.endIndex,
          "Can't increment past endIndex of String.UTF8View")
        return Index(_coreIndex: nextCoreIndex, nextBuffer)
      }
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
    public subscript(position: Index) -> UTF8.CodeUnit {
      let result = UTF8.CodeUnit(extendingOrTruncating: position._buffer & 0xFF)
      _precondition(result != 0xFF, "cannot subscript using endIndex")
      return result
    }

    /// Accesses the contiguous subrange of elements enclosed by the specified
    /// range.
    ///
    /// - Complexity: O(*n*) if the underlying string is bridged from
    ///   Objective-C, where *n* is the length of the string; otherwise, O(1).
    public subscript(bounds: Range<Index>) -> UTF8View {
      return UTF8View(_core, bounds.lowerBound, bounds.upperBound)
    }

    public var description: String {
      return String._fromCodeUnitSequenceWithRepair(UTF8.self, input: self).0
    }

    public var debugDescription: String {
      return "UTF8View(\(self.description.debugDescription))"
    }
  }

  /// A UTF-8 encoding of `self`.
  public var utf8: UTF8View {
    get {
      return UTF8View(self._core)
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

  internal func _withUnsafeBufferPointerToUTF8<R>(
    _ body: (UnsafeBufferPointer<UTF8.CodeUnit>) throws -> R
  ) rethrows -> R {
    if let asciiBuffer = self._core.asciiBuffer {
      return try body(UnsafeBufferPointer(
        start: asciiBuffer.baseAddress,
        count: asciiBuffer.count))
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
  ///     if let i = picnicGuest.utf8.index(of: 32) {
  ///         let adjective = String(picnicGuest.utf8.prefix(upTo: i))
  ///         print(adjective)
  ///     }
  ///     // Prints "Optional(Deserving)"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.utf8` view.
  ///
  /// - Parameter utf8: A UTF-8 code sequence.
  public init?(_ utf8: UTF8View) {
    let wholeString = String(utf8._core)

    if let start = utf8.startIndex.samePosition(in: wholeString),
       let end = utf8.endIndex.samePosition(in: wholeString) {
      self = wholeString[start..<end]
      return
    }
    return nil
  }

  /// The index type for subscripting a string's `utf8` view.
  public typealias UTF8Index = UTF8View.Index
}

extension String.UTF8View.Index : Comparable {
  // FIXME: swift-3-indexing-model: add complete set of forwards for Comparable 
  //        assuming String.UTF8View.Index continues to exist
  public static func == (
    lhs: String.UTF8View.Index,
    rhs: String.UTF8View.Index
  ) -> Bool {
    // If the underlying UTF16 index differs, they're unequal
    if lhs._coreIndex != rhs._coreIndex {
      return false
    }

    // Match up bytes in the buffer
    var buffer = (lhs._buffer, rhs._buffer)
    var isContinuation: Bool
    while true {
      let unit = (
        UTF8.CodeUnit(extendingOrTruncating: buffer.0),
        UTF8.CodeUnit(extendingOrTruncating: buffer.1))

      isContinuation = UTF8.isContinuation(unit.0)
      if !isContinuation {
        // We don't check for unit equality in this case because one of
        // the units might be an 0xFF read from the end of the buffer.
        return !UTF8.isContinuation(unit.1)
      }
      // Continuation bytes must match exactly
      else if unit.0 != unit.1 {
        return false
      }

      // Move the buffers along.
      buffer = (
        String.UTF8Index._nextBuffer(after: buffer.0),
        String.UTF8Index._nextBuffer(after: buffer.1))
    }
  }

  public static func < (
    lhs: String.UTF8View.Index,
    rhs: String.UTF8View.Index
  ) -> Bool {
    if lhs._coreIndex == rhs._coreIndex && lhs._buffer != rhs._buffer {
      // The index with more continuation bytes remaining before the next
      return lhs._utf8ContinuationBytesUntilNextUnicodeScalar >
        rhs._utf8ContinuationBytesUntilNextUnicodeScalar
    }
    return lhs._coreIndex < rhs._coreIndex
  }
}

// Index conversions
extension String.UTF8View.Index {
  internal init(_ core: _StringCore, _utf16Offset: Int) {
      let (_, buffer) = core._encodeSomeUTF8(from: _utf16Offset)
      self.init(_coreIndex: _utf16Offset, buffer)
  }

  /// Creates an index in the given UTF-8 view that corresponds exactly to the
  /// specified `UTF16View` position.
  ///
  /// The following example finds the position of a space in a string's `utf16`
  /// view and then converts that position to an index in the string's
  /// `utf8` view.
  ///
  ///     let cafe = "Café 🍵"
  ///
  ///     let utf16Index = cafe.utf16.index(of: 32)!
  ///     let utf8Index = String.UTF8View.Index(utf16Index, within: cafe.utf8)!
  ///
  ///     print(Array(cafe.utf8.prefix(upTo: utf8Index)))
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
  ///   - utf16Index: A position in a `UTF16View` instance. `utf16Index` must
  ///     be an element in `String(utf8).utf16.indices`.
  ///   - utf8: The `UTF8View` in which to find the new position.
  public init?(_ utf16Index: String.UTF16Index, within utf8: String.UTF8View) {
    let utf16 = String.UTF16View(utf8._core)

    if utf16Index != utf16.startIndex
    && utf16Index != utf16.endIndex {
      _precondition(
        utf16Index >= utf16.startIndex
        && utf16Index <= utf16.endIndex,
        "Invalid String.UTF16Index for this UTF-8 view")

      // Detect positions that have no corresponding index.  Note that
      // we have to check before and after, because an unpaired
      // surrogate will be decoded as a single replacement character,
      // thus making the corresponding position valid in UTF8.
      if UTF16.isTrailSurrogate(utf16[utf16Index])
        && UTF16.isLeadSurrogate(utf16[utf16.index(before: utf16Index)]) {
        return nil
      }
    }
    self.init(utf8._core, _utf16Offset: utf16Index._offset)
  }

  /// Creates an index in the given UTF-8 view that corresponds exactly to the
  /// specified `UnicodeScalarView` position.
  ///
  /// The following example converts the position of the Unicode scalar `"e"`
  /// into its corresponding position in the string's `utf8` view.
  ///
  ///     let cafe = "Cafe\u{0301}"
  ///     let scalarsIndex = cafe.unicodeScalars.index(of: "e")!
  ///     let utf8Index = String.UTF8View.Index(scalarsIndex, within: cafe.utf8)
  ///
  ///     print(Array(cafe.utf8.prefix(through: utf8Index)))
  ///     // Prints "[67, 97, 102, 101]"
  ///
  /// - Parameters:
  ///   - unicodeScalarIndex: A position in a `UnicodeScalarView` instance.
  ///     `unicodeScalarIndex` must be an element of
  ///     `String(utf8).unicodeScalars.indices`.
  ///   - utf8: The `UTF8View` in which to find the new position.
  public init(
    _ unicodeScalarIndex: String.UnicodeScalarIndex,
    within utf8: String.UTF8View
  ) {
    self.init(utf8._core, _utf16Offset: unicodeScalarIndex._position)
  }

  /// Creates an index in the given UTF-8 view that corresponds exactly to the
  /// specified string position.
  ///
  /// The following example converts the position of the teacup emoji (`"🍵"`)
  /// into its corresponding position in the string's `utf8` view.
  ///
  ///     let cafe = "Café 🍵"
  ///     let characterIndex = cafe.characters.index(of: "🍵")!
  ///     let utf8Index = String.UTF8View.Index(characterIndex, within: cafe.utf8)
  ///
  ///     print(Array(cafe.utf8.suffix(from: utf8Index)))
  ///     // Prints "[240, 159, 141, 181]"
  ///
  /// - Parameters:
  ///   - characterIndex: A position in a `CharacterView` instance.
  ///     `characterIndex` must be an element of
  ///     `String(utf8).characters.indices`.
  ///   - utf8: The `UTF8View` in which to find the new position.
  public init(_ characterIndex: String.Index, within utf8: String.UTF8View) {
    self.init(utf8._core, _utf16Offset: characterIndex._base._position)
  }

  /// Returns the position in the given UTF-16 view that corresponds exactly to
  /// this index.
  ///
  /// The index must be a valid index of `String(utf16).utf8`.
  ///
  /// This example first finds the position of a space (UTF-8 code point `32`)
  /// in a string's `utf8` view and then uses this method to find the same
  /// position in the string's `utf16` view.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.utf8.index(of: 32)!
  ///     let j = i.samePosition(in: cafe.utf16)!
  ///     print(cafe.utf16.prefix(upTo: j))
  ///     // Prints "Café"
  ///
  /// - Parameter utf16: The view to use for the index conversion.
  /// - Returns: The position in `utf16` that corresponds exactly to this
  ///   index. If this index does not have an exact corresponding position in
  ///   `utf16`, this method returns `nil`. For example, an attempt to convert
  ///   the position of a UTF-8 continuation byte returns `nil`.
  public func samePosition(
    in utf16: String.UTF16View
  ) -> String.UTF16View.Index? {
    return String.UTF16View.Index(self, within: utf16)
  }

  /// Returns the position in the given view of Unicode scalars that
  /// corresponds exactly to this index.
  ///
  /// This index must be a valid index of `String(unicodeScalars).utf8`.
  ///
  /// This example first finds the position of a space (UTF-8 code point `32`)
  /// in a string's `utf8` view and then uses this method to find the same position
  /// in the string's `unicodeScalars` view.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.utf8.index(of: 32)!
  ///     let j = i.samePosition(in: cafe.unicodeScalars)!
  ///     print(cafe.unicodeScalars.prefix(upTo: j))
  ///     // Prints "Café"
  ///
  /// - Parameter unicodeScalars: The view to use for the index conversion.
  /// - Returns: The position in `unicodeScalars` that corresponds exactly to
  ///   this index. If this index does not have an exact corresponding
  ///   position in `unicodeScalars`, this method returns `nil`. For example,
  ///   an attempt to convert the position of a UTF-8 continuation byte
  ///   returns `nil`.
  public func samePosition(
    in unicodeScalars: String.UnicodeScalarView
  ) -> String.UnicodeScalarIndex? {
    return String.UnicodeScalarIndex(self, within: unicodeScalars)
  }

  /// Returns the position in the given string that corresponds exactly to this
  /// index.
  ///
  /// This index must be a valid index of `characters.utf8`.
  ///
  /// This example first finds the position of a space (UTF-8 code point `32`)
  /// in a string's `utf8` view and then uses this method find the same position
  /// in the string.
  ///
  ///     let cafe = "Café 🍵"
  ///     let i = cafe.utf8.index(of: 32)!
  ///     let j = i.samePosition(in: cafe)!
  ///     print(cafe[cafe.startIndex ..< j])
  ///     // Prints "Café"
  ///
  /// - Parameter characters: The string to use for the index conversion.
  /// - Returns: The position in `characters` that corresponds exactly to
  ///   this index. If this index does not have an exact corresponding
  ///   position in `characters`, this method returns `nil`. For example,
  ///   an attempt to convert the position of a UTF-8 continuation byte
  ///   returns `nil`.
  public func samePosition(
    in characters: String
  ) -> String.Index? {
    return String.Index(self, within: characters)
  }
}

// Reflection
extension String.UTF8View : CustomReflectable {
  /// Returns a mirror that reflects the UTF-8 view of a string.
  public var customMirror: Mirror {
    return Mirror(self, unlabeledChildren: self)
  }
}

extension String.UTF8View : CustomPlaygroundQuickLookable {
  public var customPlaygroundQuickLook: PlaygroundQuickLook {
    return .text(description)
  }
}

extension String {
  @available(*, unavailable, message: "Please use String.utf8CString instead.")
  public var nulTerminatedUTF8: ContiguousArray<UTF8.CodeUnit> {
    Builtin.unreachable()
  }
}
