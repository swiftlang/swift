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

/// A single extended grapheme cluster that approximates a user-perceived
/// character.
///
/// The `Character` type represents a character made up of one or more Unicode
/// scalar values, grouped by a Unicode boundary algorithm. Generally, a
/// `Character` instance matches what the reader of a string will perceive as
/// a single character. Strings are collections of `Character` instances, so
/// the number of visible characters is generally the most natural way to
/// count the length of a string.
///
///     let greeting = "Hello! 🐥"
///     print("Length: \(greeting.count)")
///     // Prints "Length: 8"
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
@_fixed_layout
public struct Character :
  _ExpressibleByBuiltinUTF16ExtendedGraphemeClusterLiteral,
  ExpressibleByExtendedGraphemeClusterLiteral, Hashable {

  // Fundamentally, it is just a String, but it is optimized for the common case
  // where the UTF-16 representation fits in 63 bits.  The remaining bit is used
  // to discriminate between small and large representations.  Since a grapheme
  // cluster cannot have U+0000 anywhere but in its first scalar, we can store
  // zero in empty code units above the first one.
  @_versioned
  internal enum Representation {
    case smallUTF16(Builtin.Int63)
    case large(_StringBuffer._Storage)
  }

  /// Creates a character containing the given Unicode scalar value.
  ///
  /// - Parameter content: The Unicode scalar value to convert into a character.
  public init(_ content: Unicode.Scalar) {
    let content16 = UTF16.encode(content)._unsafelyUnwrappedUnchecked
    _representation = .smallUTF16(
      Builtin.zext_Int32_Int63(content16._storage._value))
  }

  @effects(readonly)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self = Character(
      String._fromWellFormedCodeUnitSequence(
        UTF32.self, input: CollectionOfOne(UInt32(value))))
  }

  // Inlining ensures that the whole constructor can be folded away to a single
  // integer constant in case of small character literals.
  @inline(__always)
  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf8CodeUnitCount: Builtin.Word,
    isASCII: Builtin.Int1
  ) {
    let utf8 = UnsafeBufferPointer(
      start: UnsafePointer<Unicode.UTF8.CodeUnit>(start),
      count: Int(utf8CodeUnitCount))
    
    if utf8.count == 1 {
      _representation = .smallUTF16(
        Builtin.zext_Int8_Int63(utf8.first._unsafelyUnwrappedUnchecked._value))
      return
    }

  FastPath: 
    repeat {
      var shift = 0
      let maxShift = 64 - 16
      var bits: UInt64 = 0
      
      for s8 in Unicode._ParsingIterator(
        codeUnits: utf8.makeIterator(), parser: UTF8.ForwardParser()) {
        
        let s16
          = UTF16.transcode(s8, from: UTF8.self)._unsafelyUnwrappedUnchecked

        for u16 in s16 {
          guard _fastPath(shift <= maxShift) else { break FastPath }
          bits |= UInt64(u16) &<< shift
          shift += 16
        }
      }
      guard _fastPath(Int64(extendingOrTruncating: bits) >= 0) else {
        break FastPath
      }
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
      return
    }
    while false
    
    // For anything that doesn't fit in 63 bits, build the large
    // representation.
    self = Character(_largeRepresentationString:
      String(
        _builtinExtendedGraphemeClusterLiteral: start,
        utf8CodeUnitCount: utf8CodeUnitCount,
        isASCII: isASCII))
  }

  // Inlining ensures that the whole constructor can be folded away to a single
  // integer constant in case of small character literals.
  @inline(__always)
  @effects(readonly)
  public init(
    _builtinExtendedGraphemeClusterLiteral start: Builtin.RawPointer,
    utf16CodeUnitCount: Builtin.Word
  ) {
    let utf16 = UnsafeBufferPointer(
      start: UnsafePointer<Unicode.UTF16.CodeUnit>(start),
      count: Int(utf16CodeUnitCount))

    switch utf16.count {
    case 1:
      _representation = .smallUTF16(Builtin.zext_Int16_Int63(utf16[0]._value))
    case 2:
      let bits = UInt32(utf16[0]) | UInt32(utf16[1]) &<< 16
      _representation = .smallUTF16(Builtin.zext_Int32_Int63(bits._value))
    case 3:
      let bits = UInt64(utf16[0])
        | UInt64(utf16[1]) &<< 16
        | UInt64(utf16[2]) &<< 32
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
    case 4 where utf16[3] < 0x8000:
      let bits = UInt64(utf16[0])
        | UInt64(utf16[1]) &<< 16
        | UInt64(utf16[2]) &<< 32
        | UInt64(utf16[3]) &<< 48
      _representation = .smallUTF16(Builtin.trunc_Int64_Int63(bits._value))
    default:
      _representation = Character(
        _largeRepresentationString: String(
          _StringCore(
            baseAddress: UnsafeMutableRawPointer(start), 
            count: utf16.count,
            elementShift: 1,
            hasCocoaBuffer: false,
            owner: nil)
        ))._representation
    }
  }
  
  /// Creates a character with the specified value.
  ///
  /// Do not call this initalizer directly. It is used by the compiler when
  /// you use a string literal to initialize a `Character` instance. For
  /// example:
  ///
  ///     let oBreve: Character = "o\u{306}"
  ///     print(oBreve)
  ///     // Prints "ŏ"
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
    _precondition(
      s._core.count != 0, "Can't form a Character from an empty String")
    _debugPrecondition(
      s.index(after: s.startIndex) == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    if _fastPath(s._core.count <= 4) {
      let b = _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>(s._core)
      if _fastPath(Int64(extendingOrTruncating: b._storage) >= 0) {
        _representation = .smallUTF16(
          Builtin.trunc_Int64_Int63(b._storage._value))
        return
      }
    }
    self = Character(_largeRepresentationString: s)
  }

  /// Creates a Character from a String that is already known to require the
  /// large representation.
  ///
  /// - Note: `s` should contain only a single grapheme, but we can't require
  ///   that formally because of grapheme cluster literals and the shifting
  ///   sands of Unicode.  https://bugs.swift.org/browse/SR-4955
  @_versioned
  internal init(_largeRepresentationString s: String) {
    if let native = s._core.nativeBuffer,
      native.start == s._core._baseAddress!,
      native.usedCount == s._core.count {
      _representation = .large(native._storage)
      return
    }
    var nativeString = ""
    nativeString.append(s)
    _representation = .large(nativeString._core.nativeBuffer!._storage)
  }

  static func _smallValue(_ value: Builtin.Int63) -> UInt64 {
    return UInt64(Builtin.zext_Int63_Int64(value))
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

  typealias UTF16View = String.UTF16View
  var utf16: UTF16View {
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

extension Character {
  @_versioned
  internal var _smallUTF16 : _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>? {
    guard case .smallUTF16(let _63bits) = _representation else { return nil }
    _onFastPath()
    let bits = UInt64(Builtin.zext_Int63_Int64(_63bits))
    let minBitWidth = type(of: bits).bitWidth - bits.leadingZeroBitCount
    return _UIntBuffer<UInt64, Unicode.UTF16.CodeUnit>(
      _storage: bits,
      _bitCount: UInt8(16 * Swift.max(1, (minBitWidth + 15) / 16))
    )
  }

  @_versioned
  internal var _largeUTF16 : _StringCore? {
    guard case .large(let storage) = _representation else { return nil }
    return _StringCore(_StringBuffer(storage))
  }
}

extension String {
  /// Creates a string containing the given character.
  ///
  /// - Parameter c: The character to convert to a string.
  public init(_ c: Character) {
    if let utf16 = c._smallUTF16 {
      self = String(decoding: utf16, as: Unicode.UTF16.self)
    }
    else {
      self = String(c._largeUTF16!)
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
  @_inlineable
  @inline(__always)
  public static func == (lhs: Character, rhs: Character) -> Bool {
    let l0 = lhs._smallUTF16
    if _fastPath(l0 != nil), let l = l0?._storage {
      let r0 = rhs._smallUTF16
      if _fastPath(r0 != nil), let r = r0?._storage {
        if (l | r) < 0x300 { return l == r }
        if l == r { return true }
      }
    }
    
    // FIXME(performance): constructing two temporary strings is extremely
    // wasteful and inefficient.
    return String(lhs) == String(rhs)
  }
}

extension Character : Comparable {
  @_inlineable
  @inline(__always)
  public static func < (lhs: Character, rhs: Character) -> Bool {
    let l0 = lhs._smallUTF16
    if _fastPath(l0 != nil), let l = l0?._storage {
      let r0 = rhs._smallUTF16
      if _fastPath(r0 != nil), let r = r0?._storage {
        if (l | r) < 0x80 { return l < r }
        if l == r { return false }
      }
    }
    // FIXME(performance): constructing two temporary strings is extremely
    // wasteful and inefficient.
    return String(lhs) < String(rhs)
  }
}
