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
  @inline(__always)
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

    init(_ _core: _StringCore) {
      self._core = _core
    }

    public typealias Index = String.Index
    public typealias IndexDistance = Int

    /// The position of the first code unit if the UTF-8 view is
    /// nonempty.
    ///
    /// If the UTF-8 view is empty, `startIndex` is equal to `endIndex`.
    public var startIndex: Index {
      return _index(atEncodedOffset: _core.startIndex)
    }

    /// The "past the end" position---that is, the position one
    /// greater than the last valid subscript argument.
    ///
    /// In an empty UTF-8 view, `endIndex` is equal to `startIndex`.
    public var endIndex: Index {
      return Index(encodedOffset: _core.endIndex)
    }

    internal func _index(atEncodedOffset n: Int) -> Index {
      if _fastPath(_core.isASCII) { return Index(encodedOffset: n) }
      var p = UTF16.ForwardParser()
      var i = _core[n...].makeIterator()
      let s = p.parseScalar(from: &i)
      
      if case .valid(let u16) = s {
        _onFastPath()
        let u8 = UTF8.transcode(u16, from: UTF16.self)
        _sanityCheck(u16.count >= 0 && u16.count <= 2)
        let stride = UInt8(extendingOrTruncating: u16.count)
        return Index(
          encodedOffset: n,
          .utf8(
            encodedScalar: u8._unsafelyUnwrappedUnchecked, stride: stride))
      }
      
      if case .error(let stride) = s {
        return Index(
          encodedOffset: n,
          .utf8(
            encodedScalar: UTF8.encodedReplacementCharacter,
            stride: UInt8(extendingOrTruncating: stride)))
      }
      _onFastPath()
      return Index(encodedOffset: n)
    }
  
    /// Returns the next consecutive position after `i`.
    ///
    /// - Precondition: The next position is representable.
    public func index(after i: Index) -> Index {
      _precondition(i != endIndex, "Can't advance past endIndex")
      if _fastPath(_core.isASCII) {
        return Index(encodedOffset: i.encodedOffset + 1)
      }
      var j = i
      while true {
        if case .utf8(let encodedScalar, let stride) = j._cache {
          _onFastPath()
          j._transcodedOffset += 1
          if _fastPath(j._transcodedOffset < encodedScalar.count) {
            return j
          }
          return _index(atEncodedOffset: j.encodedOffset &+ numericCast(stride))
        }
        j = _index(atEncodedOffset: j.encodedOffset)
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
      _precondition(position != endIndex, "cannot subscript using endIndex")
      if _fastPath(_core.isASCII) {
        return UTF8.CodeUnit(_core[position.encodedOffset])
      }
      var j = position
      while true {
        if case let .utf8(encodedScalar, _) = j._cache {
          _onFastPath()
          _sanityCheck((0..<4).contains(j._transcodedOffset))
          
          let i = encodedScalar.index(
            encodedScalar.startIndex, offsetBy: j._transcodedOffset)
          
          return encodedScalar[i]
        }
        j = _index(atEncodedOffset: j.encodedOffset)
      }
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
  /// You can use this initializer to create a new string from
  /// another string's `utf8` view.
  ///
  ///     let picnicGuest = "Deserving porcupine"
  ///     if let i = picnicGuest.utf8.index(of: 32) {
  ///         let adjective = String(picnicGuest.utf8[..<i])
  ///         print(adjective)
  ///     }
  ///     // Prints "Optional(Deserving)"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.utf8` view.
  ///
  /// - Parameter utf8: A UTF-8 code sequence.
  public init(_ utf8: UTF8View) {
    self = String(utf8._core)
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
  ///         let adjective = String(picnicGuest.utf8[..<i])
  ///         print(adjective)
  ///     }
  ///     // Prints "Optional(Deserving)"
  ///
  /// The `adjective` constant is created by calling this initializer with a
  /// slice of the `picnicGuest.utf8` view.
  ///
  /// - Parameter utf8: A UTF-8 code sequence.
  public init?(_ utf8: UTF8View.SubSequence) {
    let wholeString = String(utf8.base._core)
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

extension String.UTF8View : _SwiftStringView {
  var _persistentContent : String { return String(self._core) }
}

extension String.UTF8View {
  public struct Iterator {
    typealias _OutputBuffer = UInt64
    internal let _source: _StringCore
    internal var _sourceIndex: Int
    internal var _buffer: _OutputBuffer
  }
  public func makeIterator() -> Iterator {
    return Iterator(_core)
  }
}

extension String.UTF8View.Iterator : IteratorProtocol {
  internal init(_ source: _StringCore) {
    _source = source
    _sourceIndex = 0
    _buffer = 0
  }
  
  public mutating func next() -> Unicode.UTF8.CodeUnit? {
    if _fastPath(_buffer != 0) {
      let r = UInt8(extendingOrTruncating: _buffer) &- 1
      _buffer >>= 8
      return r
    }
    if _slowPath(_sourceIndex == _source.count) { return nil }

    defer { _fixLifetime(_source) }
    
    if _fastPath(_source._unmanagedASCII != nil),
    let ascii = _source._unmanagedASCII {
      let result = ascii[_sourceIndex]
      _sourceIndex += 1
      for i in 0 ..< _OutputBuffer.bitWidth>>3 {
        if _sourceIndex == _source.count { break }
        _buffer |= _OutputBuffer(ascii[_sourceIndex] &+ 1) &<< (i << 3)
        _sourceIndex += 1
      }
      return result
    }
    
    if _fastPath(_source._unmanagedUTF16 != nil),
    let utf16 = _source._unmanagedUTF16 {
      return _next(refillingFrom: utf16)
    }
    return _next(refillingFrom: _source)
  }

  internal mutating func _next<Source: Collection>(
    refillingFrom source: Source
  ) -> Unicode.UTF8.CodeUnit?
  where Source.Element == Unicode.UTF16.CodeUnit,
  Source._Element == Unicode.UTF16.CodeUnit,
  Source.Index == Int
  {
    _sanityCheck(_buffer == 0)
    var shift = 0

    // ASCII fastpath
    while _sourceIndex != _source.endIndex && shift < _OutputBuffer.bitWidth {
      let u = _source[_sourceIndex]
      if u >= 0x80 { break }
      _buffer |= _OutputBuffer(u &+ 1) &<< shift
      _sourceIndex += 1
      shift = shift &+ 8
    }
    
    var i = IndexingIterator(_elements: source, _position: _sourceIndex)
    var parser = Unicode.UTF16.ForwardParser()
  Loop:
    while true {
      let u8: UTF8.EncodedScalar
      switch parser.parseScalar(from: &i) {
      case .valid(let s):
        u8 = UTF8.transcode(s, from: UTF16.self)._unsafelyUnwrappedUnchecked
      case .error(_):
        u8 = UTF8.encodedReplacementCharacter
      case .emptyInput:
        break Loop
      }
      var newBuffer = _buffer
      for x in u8 {
        newBuffer |= _OutputBuffer(x &+ 1) &<< shift
        shift = shift &+ 8
      }
      guard _fastPath(shift <= _OutputBuffer.bitWidth) else { break Loop }
      _buffer = newBuffer
      _sourceIndex = i._position &- parser._buffer.count
    }
    guard _fastPath(_buffer != 0) else { return nil }
    let result = UInt8(extendingOrTruncating: _buffer) &- 1
    _buffer >>= 8
    return result
  }
}

extension String.UTF8View {
  public var count: Int {
    if _fastPath(_core.isASCII) { return _core.count }
    defer { _fixLifetime(_core) }
    if _fastPath(_core._unmanagedUTF16 != nil), let b = _core._unmanagedUTF16 {
      var result = 0
      UTF8._transcode(b, from: UTF16.self) { result += $0.count }
      return result
    }
    var result = 0
    UTF8._transcode(_core, from: UTF16.self) { result += $0.count }
    return result
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
  ///     let utf16Index = cafe.utf16.index(of: 32)!
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
  public init?(_ sourcePosition: String.Index, within target: String.UTF8View) {
    guard String.UnicodeScalarView(target._core)._isOnUnicodeScalarBoundary(
      sourcePosition) else { return nil }
    self.init(encodedOffset: sourcePosition.encodedOffset)
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

// backward compatibility for index interchange.  
extension String.UTF8View {
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public func index(after i: Index?) -> Index {
    return index(after: i!)
  }
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public func index(_ i: Index?, offsetBy n: IndexDistance) -> Index {
    return index(i!, offsetBy: n)
  }
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional indices")
  public func distance(
    from i: Index?, to j: Index?) -> IndexDistance {
    return distance(from: i!, to: j!)
  }
  @available(
    swift, obsoleted: 4.0,
    message: "Any String view index conversion can fail in Swift 4; please unwrap the optional index")
  public subscript(i: Index?) -> Unicode.UTF8.CodeUnit {
    return self[i!]
  }
}
