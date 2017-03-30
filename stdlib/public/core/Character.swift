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

/// A single extended grapheme cluster, which approximates a user-perceived
/// character.
///
/// The `Character` type represents a character made up of one or more Unicode
/// scalar values, grouped by a Unicode boundary algorithm. Generally, a
/// `Character` instance matches what the reader of a string will perceive as
/// a single character. The number of visible characters is generally the most
/// natural way to count the length of a string.
///
///     let greeting = "Hello! 🐥"
///     print("Character count: \(greeting.characters.count)")
///     // Prints "Character count: 8"
///
/// Because each character in a string can be made up of one or more Unicode
/// code points, the number of characters in a string may not match the length
/// of the Unicode code point representation or the length of the string in a
/// particular binary representation.
///
///     print("Unicode code point count: \(greeting.unicodeScalars.count)")
///     // Prints "Unicode code point count: 15"
///
///     print("UTF-8 representation count: \(greeting.utf8.count)")
///     // Prints "UTF-8 representation count: 18"
///
/// Every `Character` instance is composed of one or more Unicode code points
/// that are grouped together as an *extended grapheme cluster*. The way these
/// code points are grouped is defined by a canonical, localized, or otherwise
/// tailored Unicode segmentation algorithm.
///
/// For example, a country's Unicode flag character is made up of two regional
/// indicator code points that correspond to that country's ISO 3166-1 alpha-2
/// code. The alpha-2 code for The United States is "US", so its flag
/// character is made up of the Unicode code points `"\u{1F1FA}"` (REGIONAL
/// INDICATOR SYMBOL LETTER U) and `"\u{1F1F8}"` (REGIONAL INDICATOR SYMBOL
/// LETTER S). When placed next to each other in a Swift string literal, these
/// two code points are combined into a single grapheme cluster, represented
/// by a `Character` instance in Swift.
///
///     let usFlag: Character = "\u{1F1FA}\u{1F1F8}"
///     print(usFlag)
///     // Prints "🇺🇸"
///
/// For more information about the Unicode terms used in this discussion, see
/// the [Unicode.org glossary][glossary]. In particular, this discussion
/// mentions [extended grapheme clusters][clusters] and [Unicode scalar
/// values][scalars].
///
/// [glossary]: http://www.unicode.org/glossary/
/// [clusters]: http://www.unicode.org/glossary/#extended_grapheme_cluster
/// [scalars]: http://www.unicode.org/glossary/#unicode_scalar_value
public struct Character :
  _ExpressibleByBuiltinExtendedGraphemeClusterLiteral,
  ExpressibleByExtendedGraphemeClusterLiteral, Hashable {

  // Fundamentally, it is just a String, but it is optimized for the
  // common case where the UTF-8 representation fits in 63 bits.  The
  // remaining bit is used to discriminate between small and large
  // representations.  In the small representation, the unused bytes
  // are filled with 0xFF.
  //
  // If the grapheme cluster can be represented as `.small`, it
  // should be represented as such.
  @_versioned
  internal enum Representation {
    // A _StringBuffer whose first grapheme cluster is self.
    // NOTE: may be more than 1 Character long.
    case large(_StringBuffer._Storage)
    case small(Builtin.Int63)
  }

  /// Creates a character containing the given Unicode scalar value.
  ///
  /// - Parameter scalar: The Unicode scalar value to convert into a character.
  public init(_ scalar: UnicodeScalar) {
    var asInt: UInt64 = 0
    var shift: UInt64 = 0

    let output: (UTF8.CodeUnit) -> Void = {
      asInt |= UInt64($0) << shift
      shift += 8
    }

    UTF8.encode(scalar, into: output)
    asInt |= (~0) << shift
    _representation = .small(Builtin.trunc_Int64_Int63(asInt._value))
  }

  @effects(readonly)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = Character(
      String._fromWellFormedCodeUnitSequence(
        UTF32.self, input: CollectionOfOne(UInt32(value))))
  }

  /// Creates a character with the specified value.
  ///
  /// Do not call this initializer directly. It is used by the compiler when you
  /// use a string literal to initialize a `Character` instance. For example:
  ///
  ///     let snowflake: Character = "❄︎"
  ///     print(snowflake)
  ///     // Prints "❄︎"
  ///
  /// The assignment to the `snowflake` constant calls this initializer behind
  /// the scenes.
  public init(unicodeScalarLiteral value: Character) {
    self = value
  }

  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    // Most character literals are going to be fewer than eight UTF-8 code
    // units; for those, build the small character representation directly.
    let maxCodeUnitCount = MemoryLayout<UInt64>.size
    if _fastPath(Int(utf8CodeUnitCount) <= maxCodeUnitCount) {
      var buffer: UInt64 = ~0
      _memcpy(
        dest: UnsafeMutableRawPointer(Builtin.addressof(&buffer)),
        src: UnsafeMutableRawPointer(start),
        size: UInt(utf8CodeUnitCount))
      // Copying the bytes directly from the literal into an integer assumes
      // little endianness, so convert the copied data into host endianness.
      let utf8Chunk = UInt64(littleEndian: buffer)
      let bits = maxCodeUnitCount &* 8 &- 1
      // Verify that the highest bit isn't set so that we can truncate it to
      // 63 bits.
      if _fastPath(utf8Chunk & (1 << numericCast(bits)) != 0) {
        _representation = .small(Builtin.trunc_Int64_Int63(utf8Chunk._value))
        return
      }
    }
    // For anything that doesn't fit in 63 bits, build the large
    // representation.
    self = Character(_largeRepresentationString: String(
      _builtinExtendedGraphemeClusterLiteral: start,
      utf8CodeUnitCount: utf8CodeUnitCount,
      isASCII: isASCII))
  }

  /// Creates a character with the specified value.
  ///
  /// Do not call this initializer directly. It is used by the compiler when
  /// you use a string literal to initialize a `Character` instance. For
  /// example:
  ///
  ///     let oBreve: Character = "o\u{306}"
  ///     print(oBreve)
  ///     // Prints "ŏ"
  ///
  /// The assignment to the `oBreve` constant calls this initializer behind the
  /// scenes.
  public init(extendedGraphemeClusterLiteral value: Character) {
    self = value
  }

  /// Creates a character from a single-character string.
  ///
  /// The following example creates a new character from the uppercase version
  /// of a string that only holds one character.
  ///
  ///     let a = "a"
  ///     let capitalA = Character(a.uppercased())
  ///
  /// - Parameter s: The single-character string to convert to a `Character`
  ///   instance. `s` must contain exactly one extended grapheme cluster.
  public init(_ s: String) {
    // The small representation can accept up to 8 code units as long
    // as the last one is a continuation.  Since the high bit of the
    // last byte is used for the enum's discriminator, we have to
    // reconstruct it.  As a result, we can't store 0x7f in the final
    // byte, because we wouldn't be able to distinguish it from an
    // unused 0xFF byte.  Rather than trying to squeeze in other
    // one-byte code points there, we simplify decoding by banning
    // starting a code point in the last byte, and assuming that its
    // high bit is 1.
    _precondition(
      s._core.count != 0, "Can't form a Character from an empty String")
    _precondition(
      s.index(after: s.startIndex) == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    let (count, initialUTF8) = s._core._encodeSomeUTF8(from: 0)
    // Notice that the result of sizeof() is a small non-zero number and can't
    // overflow when multiplied by 8.
    let bits = MemoryLayout.size(ofValue: initialUTF8) &* 8 &- 1
    if _fastPath(
      count == s._core.count && (initialUTF8 & (1 << numericCast(bits))) != 0) {
      _representation = .small(Builtin.trunc_Int64_Int63(initialUTF8._value))
    }
    else {
      self = Character(_largeRepresentationString: s)
    }
  }
  
  // TODO: make one init responsible for both small and large
  // TODO: this only handles 7-code-unit characters for now
  internal init?<S: Sequence>(_smallUtf8: S) where S.Iterator.Element == UTF8.CodeUnit {
    // TODO: would it be faster to get length via underestimatedCount,
    //       even though that might take two transcoding passes?
    //       (given anything with count > 1 would be non-ASCII by definition)
    var u: UInt64 = ~0
    var n: UInt64 = 0
    var mask: UInt64 = 0xff
    for u8 in _smallUtf8 {
      guard n < 7 else { return nil }
      u &= ~mask
      u |= UInt64(u8) << (n * 8)
      n += 1
      mask <<= 8
    }
    _representation = .small(Builtin.trunc_Int64_Int63(u._value))
  }

  /// Creates a Character from a String that is already known to require the
  /// large representation.
  internal init(_largeRepresentationString s: String) {
    if let native = s._core.nativeBuffer,
       native.start == s._core._baseAddress! {
      _representation = .large(native._storage)
      return
    }
    var nativeString = ""
    nativeString.append(s)
    _representation = .large(nativeString._core.nativeBuffer!._storage)
  }

  /// Returns the index of the lowest byte that is 0xFF, or 8 if
  /// there is none.
  static func _smallSize(_ value: UInt64) -> Int {
    var mask: UInt64 = 0xFF
    for i in 0..<8 {
      if (value & mask) == mask {
        return i
      }
      mask <<= 8
    }
    return 8
  }

  static func _smallValue(_ value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value)) | (1<<63)
  }

  internal struct _SmallUTF8 : RandomAccessCollection {

    init(_ u8: UInt64) {
      let utf8Count = Character._smallSize(u8)
      _sanityCheck(utf8Count <= 8, "Character with more than 8 UTF-8 code units")
      self.count = UInt16(utf8Count)
      self.data = u8
    }
    
    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: Int {
      return 0
    }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `index(after:)`.
    var endIndex: Int {
      return Int(count)
    }

    /// Access the code unit at `position`.
    ///
    /// - Precondition: `position` is a valid position in `self` and
    ///   `position != endIndex`.
    subscript(position: Int) -> UTF8.CodeUnit {
      _sanityCheck(position >= 0)
      _sanityCheck(position < Int(count))
      // Note: using unchecked arithmetic because overflow cannot happen if the
      // above sanity checks hold.
      return UTF8.CodeUnit(
        truncatingBitPattern: data >> (UInt64(position) &* 8))
    }

    internal struct Iterator : IteratorProtocol {
      init(_ data: UInt64) {
        self._data = data
      }

      internal mutating func next() -> UInt8? {
        let result = UInt8(truncatingBitPattern: _data)
        if result == 0xFF {
          return nil
        }
        _data = (_data >> 8) | 0xFF00_0000_0000_0000
        return result
      }

      internal var _data: UInt64
    }

    internal func makeIterator() -> Iterator {
      return Iterator(data)
    }

    var count: UInt16
    var data: UInt64
  }

  struct _SmallUTF16 : RandomAccessCollection {
    typealias Indices = CountableRange<Int>
    
    init(_ u8: UInt64) {
      let count = UTF16.transcodedLength(
        of: _SmallUTF8(u8).makeIterator(),
        decodedAs: UTF8.self,
        repairingIllFormedSequences: true)!.0
      _sanityCheck(count <= 4, "Character with more than 4 UTF-16 code units")
      self.count = UInt16(count)
      var u16: UInt64 = 0
      let output: (UTF16.CodeUnit) -> Void = {
        u16 = u16 << 16
        u16 = u16 | UInt64($0)
      }
      _ = transcode(
        _SmallUTF8(u8).makeIterator(),
        from: UTF8.self, to: UTF16.self,
        stoppingOnError: false,
        into: output)
      self.data = u16
    }

    /// The position of the first element in a non-empty collection.
    ///
    /// In an empty collection, `startIndex == endIndex`.
    var startIndex: Int {
      return 0
    }

    /// The collection's "past the end" position.
    ///
    /// `endIndex` is not a valid argument to `subscript`, and is always
    /// reachable from `startIndex` by zero or more applications of
    /// `successor()`.
    var endIndex: Int {
      return Int(count)
    }

    /// Access the code unit at `position`.
    ///
    /// - Precondition: `position` is a valid position in `self` and
    ///   `position != endIndex`.
    subscript(position: Int) -> UTF16.CodeUnit {
      _sanityCheck(position >= 0)
      _sanityCheck(position < Int(count))
      // Note: using unchecked arithmetic because overflow cannot happen if the
      // above sanity checks hold.
      return UTF16.CodeUnit(truncatingBitPattern:
        data >> ((UInt64(count) &- UInt64(position) &- 1) &* 16))
    }

    var count: UInt16
    var data: UInt64
  }

  /// The character's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  public var hashValue: Int {
    // FIXME(performance): constructing a temporary string is extremely
    // wasteful and inefficient.
    return String(self).hashValue
  }

  public typealias UTF16View = String.UTF16View
  public var utf16: UTF16View {
    return String(self).utf16
  }

  @_versioned
  internal var _representation: Representation
}

extension Character : CustomStringConvertible {
  public var description: String {
    return String(describing: self)
  }
}

extension Character : LosslessStringConvertible {}

extension Character : CustomDebugStringConvertible {
  /// A textual representation of the character, suitable for debugging.
  public var debugDescription: String {
    return String(self).debugDescription
  }
}

extension String {
  /// Creates a string containing the given character.
  ///
  /// - Parameter c: The character to convert to a string.
  public init(_ c: Character) {
    switch c._representation {
    case let .small(_63bits):
      let value = Character._smallValue(_63bits)
      let smallUTF8 = Character._SmallUTF8(value)
      self = String._fromWellFormedCodeUnitSequence(
        UTF8.self, input: smallUTF8)
    case let .large(value):
      let buf = String(_StringCore(_StringBuffer(value)))
      self = String(buf[buf.startIndex..<buf.index(after: buf.startIndex)])
    }
  }
}

/// `.small` characters are stored in an Int63 with their UTF-8 representation,
/// with any unused bytes set to 0xFF. ASCII characters will have all bytes set
/// to 0xFF except for the lowest byte, which will store the ASCII value. Since
/// 0x7FFFFFFFFFFFFF80 or greater is an invalid UTF-8 sequence, we know if a
/// value is ASCII by checking if it is greater than or equal to
/// 0x7FFFFFFFFFFFFF00.
internal var _minASCIICharReprBuiltin: Builtin.Int63 {
  @inline(__always) get {
    let x: Int64 = 0x7FFFFFFFFFFFFF00
    return Builtin.truncOrBitCast_Int64_Int63(x._value)
  }
}

extension Character : Equatable {
  public static func == (lhs: Character, rhs: Character) -> Bool {
    switch (lhs._representation, rhs._representation) {
    case let (.small(lbits), .small(rbits)) where
      Bool(Builtin.cmp_uge_Int63(lbits, _minASCIICharReprBuiltin))
      && Bool(Builtin.cmp_uge_Int63(rbits, _minASCIICharReprBuiltin)):
      return Bool(Builtin.cmp_eq_Int63(lbits, rbits))
    default:
      // FIXME(performance): constructing two temporary strings is extremely
      // wasteful and inefficient.
      return String(lhs) == String(rhs)
    }
  }
}

extension Character : Comparable {
  public static func < (lhs: Character, rhs: Character) -> Bool {
    switch (lhs._representation, rhs._representation) {
    case let (.small(lbits), .small(rbits)) where
      // Note: This is consistent with Foundation but unicode incorrect.
      // See String._compareASCII.
      Bool(Builtin.cmp_uge_Int63(lbits, _minASCIICharReprBuiltin))
      && Bool(Builtin.cmp_uge_Int63(rbits, _minASCIICharReprBuiltin)):
      return Bool(Builtin.cmp_ult_Int63(lbits, rbits))
    default:
      // FIXME(performance): constructing two temporary strings is extremely
      // wasteful and inefficient.
      return String(lhs) < String(rhs)
    }
  }
}

extension Character {
  // public init<S: Sequence>(_ scalars: S) where S.Iterator.Element == UnicodeScalar {
  //   // FIXME: Horribly inefficient, but the stuff to make it fast is private.
  //   // FIXME: Also, constructing "👩‍❤️‍👩" is foiled by precondition checks
  //   let string = String(String.UnicodeScalarView(scalars))
  //   // TBD: failable initializer, precondition, or neither?
  //   assert(string.characters.count == 1)
  //   self = string.first!
  // }
  
  // FIXME: Horribly inefficient, but the stuff to make it fast is private.
  // TBD: set or get-only?
  public var unicodeScalars: String.UnicodeScalarView {
    return String(self).unicodeScalars
  }
}

