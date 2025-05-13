//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

extension Character {
  @inlinable
  internal var _firstScalar: Unicode.Scalar {
    return self.unicodeScalars.first!
  }
  @inlinable
  internal var _isSingleScalar: Bool {
    return self.unicodeScalars.index(
      after: self.unicodeScalars.startIndex
    ) == self.unicodeScalars.endIndex
  }

  /// A Boolean value indicating whether this is an ASCII character.
  @inlinable
  public var isASCII: Bool {
    return asciiValue != nil
  }

  /// The ASCII encoding value of this character, if it is an ASCII character.
  ///
  ///     let chars: [Character] = ["a", " ", "™"]
  ///     for ch in chars {
  ///         print(ch, "-->", ch.asciiValue)
  ///     }
  ///     // Prints:
  ///     // a --> Optional(97)
  ///     //   --> Optional(32)
  ///     // ™ --> nil
  ///
  /// A character with the value "\r\n" (CR-LF) is normalized to "\n" (LF) and
  /// has an `asciiValue` property equal to 10.
  ///
  ///     let cr = "\r" as Character
  ///     // cr.asciiValue == 13
  ///     let lf = "\n" as Character
  ///     // lf.asciiValue == 10
  ///     let crlf = "\r\n" as Character
  ///     // crlf.asciiValue == 10
  @inlinable
  public var asciiValue: UInt8? {
    if _slowPath(self == "\r\n") { return 0x000A /* LINE FEED (LF) */ }
    if _slowPath(!_isSingleScalar || _firstScalar.value >= 0x80) { return nil }
    return UInt8(_firstScalar.value)
  }

  /// A Boolean value indicating whether this character represents whitespace,
  /// including newlines.
  ///
  /// For example, the following characters all represent whitespace:
  ///
  /// - "\t" (U+0009 CHARACTER TABULATION)
  /// - " " (U+0020 SPACE)
  /// - U+2029 PARAGRAPH SEPARATOR
  /// - U+3000 IDEOGRAPHIC SPACE
  public var isWhitespace: Bool {
    return _firstScalar.properties.isWhitespace
  }

  /// A Boolean value indicating whether this character represents a newline.
  ///
  /// For example, the following characters all represent newlines:
  ///
  /// - "\n" (U+000A): LINE FEED (LF)
  /// - U+000B: LINE TABULATION (VT)
  /// - U+000C: FORM FEED (FF)
  /// - "\r" (U+000D): CARRIAGE RETURN (CR)
  /// - "\r\n" (U+000D U+000A): CR-LF
  /// - U+0085: NEXT LINE (NEL)
  /// - U+2028: LINE SEPARATOR
  /// - U+2029: PARAGRAPH SEPARATOR
  @inlinable
  public var isNewline: Bool {
    switch _firstScalar.value {
      case 0x000A...0x000D /* LF ... CR */: return true
      case 0x0085 /* NEXT LINE (NEL) */: return true
      case 0x2028 /* LINE SEPARATOR */: return true
      case 0x2029 /* PARAGRAPH SEPARATOR */: return true
      default: return false
    }
  }

  /// A Boolean value indicating whether this character represents a number.
  ///
  /// For example, the following characters all represent numbers:
  ///
  /// - "7" (U+0037 DIGIT SEVEN)
  /// - "⅚" (U+215A VULGAR FRACTION FIVE SIXTHS)
  /// - "㊈" (U+3288 CIRCLED IDEOGRAPH NINE)
  /// - "𝟠" (U+1D7E0 MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT)
  /// - "๒" (U+0E52 THAI DIGIT TWO)
  public var isNumber: Bool {
    return _firstScalar.properties.numericType != nil
  }

  /// A Boolean value indicating whether this character represents a whole
  /// number.
  ///
  /// For example, the following characters all represent whole numbers:
  ///
  /// - "1" (U+0031 DIGIT ONE) => 1
  /// - "५" (U+096B DEVANAGARI DIGIT FIVE) => 5
  /// - "๙" (U+0E59 THAI DIGIT NINE) => 9
  /// - "万" (U+4E07 CJK UNIFIED IDEOGRAPH-4E07) => 10_000
  @inlinable
  public var isWholeNumber: Bool {
    return wholeNumberValue != nil
  }

  /// The numeric value this character represents, if it represents a whole
  /// number.
  ///
  /// If this character does not represent a whole number, or the value is too
  /// large to represent as an `Int`, the value of this property is `nil`.
  ///
  ///     let chars: [Character] = ["4", "④", "万", "a"]
  ///     for ch in chars {
  ///         print(ch, "-->", ch.wholeNumberValue)
  ///     }
  ///     // Prints:
  ///     // 4 --> Optional(4)
  ///     // ④ --> Optional(4)
  ///     // 万 --> Optional(10000)
  ///     // a --> nil
  public var wholeNumberValue: Int? {
    guard _isSingleScalar else { return nil }
    guard let value = _firstScalar.properties.numericValue else { return nil }
    return Int(exactly: value)
  }

  /// A Boolean value indicating whether this character represents a
  /// hexadecimal digit.
  ///
  /// Hexadecimal digits include 0-9, Latin letters a-f and A-F, and their
  /// fullwidth compatibility forms. To get the character's value, use the
  /// `hexDigitValue` property.
  @inlinable
  public var isHexDigit: Bool {
    return hexDigitValue != nil
  }

  /// The numeric value this character represents, if it is a hexadecimal digit.
  ///
  /// Hexadecimal digits include 0-9, Latin letters a-f and A-F, and their
  /// fullwidth compatibility forms. If the character does not represent a
  /// hexadecimal digit, the value of this property is `nil`.
  ///
  ///     let chars: [Character] = ["1", "a", "Ｆ", "g"]
  ///     for ch in chars {
  ///         print(ch, "-->", ch.hexDigitValue)
  ///     }
  ///     // Prints:
  ///     // 1 --> Optional(1)
  ///     // a --> Optional(10)
  ///     // Ｆ --> Optional(15)
  ///     // g --> nil
  public var hexDigitValue: Int? {
    guard _isSingleScalar else { return nil }
    let value = _firstScalar.value
    switch value {
      // DIGIT ZERO..DIGIT NINE
      case 0x0030...0x0039: return Int(value &- 0x0030)
      // LATIN CAPITAL LETTER A..LATIN CAPITAL LETTER F
      case 0x0041...0x0046: return Int((value &+ 10) &- 0x0041)
      // LATIN SMALL LETTER A..LATIN SMALL LETTER F
      case 0x0061...0x0066: return Int((value &+ 10) &- 0x0061)
      // FULLWIDTH DIGIT ZERO..FULLWIDTH DIGIT NINE
      case 0xFF10...0xFF19: return Int(value &- 0xFF10)
      // FULLWIDTH LATIN CAPITAL LETTER A..FULLWIDTH LATIN CAPITAL LETTER F
      case 0xFF21...0xFF26: return Int((value &+ 10) &- 0xFF21)
      // FULLWIDTH LATIN SMALL LETTER A..FULLWIDTH LATIN SMALL LETTER F
      case 0xFF41...0xFF46: return Int((value &+ 10) &- 0xFF41)

      default: return nil
    }
  }

  /// A Boolean value indicating whether this character is a letter.
  ///
  /// For example, the following characters are all letters:
  ///
  /// - "A" (U+0041 LATIN CAPITAL LETTER A)
  /// - "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  /// - "ϴ" (U+03F4 GREEK CAPITAL THETA SYMBOL)
  /// - "ڈ" (U+0688 ARABIC LETTER DDAL)
  /// - "日" (U+65E5 CJK UNIFIED IDEOGRAPH-65E5)
  /// - "ᚨ" (U+16A8 RUNIC LETTER ANSUZ A)
  public var isLetter: Bool {
    return _firstScalar.properties.isAlphabetic
  }

  /// Returns an uppercased version of this character.
  ///
  /// Because case conversion can result in multiple characters, the result
  /// of `uppercased()` is a string.
  ///
  ///     let chars: [Character] = ["e", "é", "и", "π", "ß", "1"]
  ///     for ch in chars {
  ///         print(ch, "-->", ch.uppercased())
  ///     }
  ///     // Prints:
  ///     // e --> E
  ///     // é --> É
  ///     // и --> И
  ///     // π --> Π
  ///     // ß --> SS
  ///     // 1 --> 1
  public func uppercased() -> String { return String(self).uppercased() }

  /// Returns a lowercased version of this character.
  ///
  /// Because case conversion can result in multiple characters, the result
  /// of `lowercased()` is a string.
  ///
  ///     let chars: [Character] = ["E", "É", "И", "Π", "1"]
  ///     for ch in chars {
  ///         print(ch, "-->", ch.lowercased())
  ///     }
  ///     // Prints:
  ///     // E --> e
  ///     // É --> é
  ///     // И --> и
  ///     // Π --> π
  ///     // 1 --> 1
  public func lowercased() -> String { return String(self).lowercased() }

  @usableFromInline
  internal var _isUppercased: Bool { return String(self) == self.uppercased() }
  @usableFromInline
  internal var _isLowercased: Bool { return String(self) == self.lowercased() }

  /// A Boolean value indicating whether this character is considered uppercase.
  ///
  /// Uppercase characters vary under case-conversion to lowercase, but not when
  /// converted to uppercase. The following characters are all uppercase:
  ///
  /// - "É" (U+0045 LATIN CAPITAL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  /// - "И" (U+0418 CYRILLIC CAPITAL LETTER I)
  /// - "Π" (U+03A0 GREEK CAPITAL LETTER PI)
  @inlinable
  public var isUppercase: Bool {
    if _fastPath(_isSingleScalar) {
      if _firstScalar.isASCII {
        return _firstScalar.value >= 0x41 && _firstScalar.value <= 0x5A
      } else if _firstScalar.properties.isUppercase {
        return true
      }
    }
    return _isUppercased && isCased
  }

  /// A Boolean value indicating whether this character is considered lowercase.
  ///
  /// Lowercase characters change when converted to uppercase, but not when
  /// converted to lowercase. The following characters are all lowercase:
  ///
  /// - "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  /// - "и" (U+0438 CYRILLIC SMALL LETTER I)
  /// - "π" (U+03C0 GREEK SMALL LETTER PI)
  @inlinable
  public var isLowercase: Bool {
    if _fastPath(_isSingleScalar) {
      if _firstScalar.isASCII {
        return _firstScalar.value >= 0x61 && _firstScalar.value <= 0x7A
      } else if _firstScalar.properties.isLowercase {
        return true
      }
    }
    return _isLowercased && isCased
  }

  /// A Boolean value indicating whether this character changes under any form
  /// of case conversion.
  @inlinable
  public var isCased: Bool {
    if _fastPath(_isSingleScalar && _firstScalar.properties.isCased) {
      return true
    }
    return !_isUppercased || !_isLowercased
  }

  /// A Boolean value indicating whether this character represents a symbol.
  ///
  /// This property is `true` only for characters composed of scalars in the
  /// "Math_Symbol", "Currency_Symbol", "Modifier_Symbol", or "Other_Symbol"
  /// categories in the
  /// [Unicode Standard](https://unicode.org/reports/tr44/#General_Category_Values).
  ///
  /// For example, the following characters all represent symbols:
  ///
  /// - "®" (U+00AE REGISTERED SIGN)
  /// - "⌹" (U+2339 APL FUNCTIONAL SYMBOL QUAD DIVIDE)
  /// - "⡆" (U+2846 BRAILLE PATTERN DOTS-237)
  public var isSymbol: Bool {
    return _firstScalar.properties.generalCategory._isSymbol
  }

  /// A Boolean value indicating whether this character represents a symbol
  /// that naturally appears in mathematical contexts.
  ///
  /// For example, the following characters all represent math symbols:
  ///
  /// - "+" (U+002B PLUS SIGN)
  /// - "∫" (U+222B INTEGRAL)
  /// - "ϰ" (U+03F0 GREEK KAPPA SYMBOL)
  ///
  /// The set of characters that have an `isMathSymbol` value of `true` is not
  /// a strict subset of those for which `isSymbol` is `true`. This includes
  /// characters used both as letters and commonly in mathematical formulas.
  /// For example, "ϰ" (U+03F0 GREEK KAPPA SYMBOL) is considered both a
  /// mathematical symbol and a letter.
  ///
  /// This property corresponds to the "Math" property in the
  /// [Unicode Standard](http://www.unicode.org/versions/latest/).
  public var isMathSymbol: Bool {
    return _firstScalar.properties.isMath
  }

  /// A Boolean value indicating whether this character represents a currency
  /// symbol.
  ///
  /// For example, the following characters all represent currency symbols:
  ///
  /// - "$" (U+0024 DOLLAR SIGN)
  /// - "¥" (U+00A5 YEN SIGN)
  /// - "€" (U+20AC EURO SIGN)
  public var isCurrencySymbol: Bool {
    return _firstScalar.properties.generalCategory == .currencySymbol
  }

  /// A Boolean value indicating whether this character represents punctuation.
  ///
  /// For example, the following characters all represent punctuation:
  ///
  /// - "!" (U+0021 EXCLAMATION MARK)
  /// - "؟" (U+061F ARABIC QUESTION MARK)
  /// - "…" (U+2026 HORIZONTAL ELLIPSIS)
  /// - "—" (U+2014 EM DASH)
  /// - "“" (U+201C LEFT DOUBLE QUOTATION MARK)
  public var isPunctuation: Bool {
    return _firstScalar.properties.generalCategory._isPunctuation
  }
}
