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

  internal typealias Packed = PackedUnsignedIntegers<UInt63, UInt16>
  
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
  case large(_UTF16StringStorage)
  case small(UInt63)
  }

  /// Creates a character containing the given Unicode scalar value.
  ///
  /// - Parameter scalar: The Unicode scalar value to convert into a character.
  public init(_ scalar: UnicodeScalar) {
    _representation = .small(
      Packed(UTF16.EncodedScalar(scalar), bitsPerElement: 16)!.representation)
  }

  @effects(readonly)
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self.init(UnicodeScalar(_unchecked: UInt32(value)))
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
    let utf8 = UnsafeBufferPointer(
      start: UnsafePointer<UTF8.CodeUnit>(start), count: Int(utf8CodeUnitCount))
    let utf16 = _UnicodeViews(utf8, ValidUTF8.self).transcoded(to: UTF16.self)
    self.init(_utf16: utf16)
  }

  public init<C: Collection>(_utf16: C)
  where C.Iterator.Element == UTF16.CodeUnit {
    if let small = Packed(_utf16, bitsPerElement: 16) {
      _representation = .small(small.representation)
    }
    else {
      _representation = .large(_UTF16StringStorage(_utf16))
    }
  }

  public init<CodeUnits : RandomAccessCollection, Encoding : UnicodeEncoding>(
    _codeUnits: CodeUnits, _: Encoding.Type
  ) where
  Encoding.EncodedScalar.Iterator.Element == CodeUnits.Iterator.Element,
  CodeUnits.SubSequence : RandomAccessCollection,
  CodeUnits.SubSequence.Index == CodeUnits.Index,
  CodeUnits.SubSequence.SubSequence == CodeUnits.SubSequence,
  CodeUnits.SubSequence.Iterator.Element == CodeUnits.Iterator.Element
  {
    // This is messy; we should be doing this on UnicodeContent
    if _fastPath(Encoding.self is Latin1.Type) {
      self.init(
        _utf16: _codeUnits.lazy.map {
          UTF16.CodeUnit($0 as Any as! Latin1.CodeUnit)
        })
    }
    else if _fastPath(Encoding.EncodedScalar.self is UTF16.EncodedScalar.Type) {
      self.init(
        _utf16: _codeUnits.lazy.map { $0 as Any as! UTF16.CodeUnit })
    }
    else {
      self.init(
        _utf16: _UnicodeViews(_codeUnits, Encoding.self)
          .transcoded(to: UTF16.self)
      )
    }
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
    _precondition(!s.content.utf16.isEmpty, "Can't form a Character from an empty String")
    _precondition(
      s.index(after: s.startIndex) == s.endIndex,
      "Can't form a Character from a String containing more than one extended grapheme cluster")

    self.init(_utf16: s.content.utf16)
  }
  
  /// The character's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  public var hashValue: Int {
    // FIXME(performance): constructing a temporary string is wasteful in some
    // cases.
    return String(self).hashValue
  }

  public struct UTF16View : RandomAccessCollection {
    public var startIndex: Int { return 0 }
    
    public var endIndex: Int {
      switch representation {
      case .small(let r):
        return Packed(representation: r, bitsPerElement: 16).count
      case .large(let r):
        return r.count
      }
    }

    public subscript(i: Int) -> UTF16.CodeUnit {
      switch representation {
      case .small(let r):
        return Packed(representation: r, bitsPerElement: 16)[i]
      case .large(let r):
        return r[i]
      }
    }
    
    let representation: Representation
  }
        
  public var utf16: UTF16View {
    return UTF16View(representation: _representation)
  }

  @_versioned
  internal var _representation: Representation
}

extension Character : CustomStringConvertible {
  public var description: String {
    return String(self)
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
    case .small:
      self.init(content: Content(utf16: c.utf16))
    case let .large(u16):
      self.init(content: Content(.utf16(u16)))
    }
  }
}

extension Character : Equatable {
  public static func == (lhs: Character, rhs: Character) -> Bool {
    // If both are Latin-1 there's no need to normalize
    if (
      !lhs.utf16.contains { $0 > 0xFF }
      && !rhs.utf16.contains { $0 > 0xFF }) {
      return lhs.utf16.elementsEqual(rhs.utf16)
    }
    return String(lhs) == String(rhs)
  }
}

extension Character : Comparable {
  public static func < (lhs: Character, rhs: Character) -> Bool {
    // If both are Latin-1 there's no need to normalize
    if (
      !lhs.utf16.contains { $0 > 0xFF }
      && !rhs.utf16.contains { $0 > 0xFF }) {
      return lhs.utf16.lexicographicallyPrecedes(rhs.utf16)
    }
    return String(lhs) < String(rhs)
  }
}

extension Character {
  public typealias UnicodeScalarView = _UnicodeViews<UTF16View, UTF16>.Scalars
  public var unicodeScalars: UnicodeScalarView {
    return _UnicodeViews(utf16, UTF16.self).scalars
  }
}

