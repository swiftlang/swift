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
        src: asciiBuffer.baseAddress + i,
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

  public typealias UTF8View = AnyUInt8UnicodeView

  // /// A UTF-8 encoding of `self`.
  public var utf8: UTF8View {
    get {
      return self.content.utf8
    }
    set {
      self.content.utf8 = newValue
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
    result.reserveCapacity(numericCast(utf8.count + 1))
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
    nullTerminatedUTF8.reserveCapacity(numericCast(utf8.count + 1))
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
  public init(_ utf8: UTF8View) {
    self.init()
    self.content.utf8 = utf8
  }

  /// The index type for subscripting a string's `utf8` view.
  public typealias UTF8Index = UTF8View.Index
}

extension String.UTF8View : CustomStringConvertible, CustomDebugStringConvertible {
  public var description: String {
    return String(self)
  }

  public var debugDescription: String {
    return "StringUnicodeScalarView(\(self.description.debugDescription))"
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
