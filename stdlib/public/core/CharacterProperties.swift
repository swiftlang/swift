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

  /// Whether this Character is ASCII.
  @inlinable
  public var isASCII: Bool {
    return asciiValue != nil
  }

  /// Returns the ASCII encoding value of this Character, if ASCII.
  ///
  /// Note: "\r\n" (CR-LF) is normalized to "\n" (LF), which will return 0x0A
  @inlinable
  public var asciiValue: UInt8? {
    if _slowPath(self == "\r\n") { return 0x000A /* LINE FEED (LF) */ }
    if _slowPath(!_isSingleScalar || _firstScalar.value >= 0x80) { return nil }
    return UInt8(_firstScalar.value)
  }

  /// Whether this Character represents whitespace, including newlines.
  ///
  /// Examples:
  ///   * "\t" (U+0009 CHARACTER TABULATION)
  ///   * " " (U+0020 SPACE)
  ///   * U+2029 PARAGRAPH SEPARATOR
  ///   * U+3000 IDEOGRAPHIC SPACE
  ///
  public var isWhitespace: Bool {
    return _firstScalar.properties.isWhitespace
  }

  /// Whether this Character represents a newline.
  ///
  /// Examples:
  ///   * "\n" (U+000A): LINE FEED (LF)
  ///   * U+000B: LINE TABULATION (VT)
  ///   * U+000C: FORM FEED (FF)
  ///   * "\r" (U+000D): CARRIAGE RETURN (CR)
  ///   * "\r\n" (U+000A U+000D): CR-LF
  ///   * U+0085: NEXT LINE (NEL)
  ///   * U+2028: LINE SEPARATOR
  ///   * U+2029: PARAGRAPH SEPARATOR
  ///
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

  /// Whether this Character represents a number.
  ///
  /// Examples:
  ///   * "7" (U+0037 DIGIT SEVEN)
  ///   * "⅚" (U+215A VULGAR FRACTION FIVE SIXTHS)
  ///   * "㊈" (U+3288 CIRCLED IDEOGRAPH NINE)
  ///   * "𝟠" (U+1D7E0 MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT)
  ///   * "๒" (U+0E52 THAI DIGIT TWO)
  ///
  public var isNumber: Bool {
    return _firstScalar.properties.numericType != nil
  }

  /// Whether this Character represents a whole number. See
  /// `Character.wholeNumberValue`
  @inlinable
  public var isWholeNumber: Bool {
    return wholeNumberValue != nil
  }

  /// If this Character is a whole number, return the value it represents, else
  /// nil.
  ///
  /// Examples:
  ///   * "1" (U+0031 DIGIT ONE) => 1
  ///   * "५" (U+096B DEVANAGARI DIGIT FIVE) => 5
  ///   * "๙" (U+0E59 THAI DIGIT NINE) => 9
  ///   * "万" (U+4E07 CJK UNIFIED IDEOGRAPH-4E07) => 10_000
  ///
  /// Note: Returns nil on 32-bit platforms if the result would overflow `Int`.
  public var wholeNumberValue: Int? {
    guard _isSingleScalar else { return nil }
    guard let value = _firstScalar.properties.numericValue else { return nil }
    return Int(exactly: value)
  }

  /// Whether this Character represents a hexadecimal digit.
  ///
  /// Hexadecimal digits include 0-9, Latin letters a-f and A-F, and their
  /// fullwidth compatibility forms. To get their value, see
  /// `Character.hexDigitValue`
  @inlinable
  public var isHexDigit: Bool {
    return hexDigitValue != nil
  }

  /// If this Character is a hexadecimal digit, returns the value it represents,
  /// else nil.
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

  /// Whether this Character is a letter.
  ///
  /// Examples:
  ///   * "A" (U+0041 LATIN CAPITAL LETTER A)
  ///   * "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///   * "ϴ" (U+03F4 GREEK CAPITAL THETA SYMBOL)
  ///   * "ڈ" (U+0688 ARABIC LETTER DDAL)
  ///   * "日" (U+65E5 CJK UNIFIED IDEOGRAPH-65E5)
  ///   * "ᚨ" (U+16A8 RUNIC LETTER ANSUZ A)
  ///
  public var isLetter: Bool {
    return _firstScalar.properties.isAlphabetic
  }

  /// Perform case conversion to uppercase
  ///
  /// Examples:
  ///   * "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///     => "É" (U+0045 LATIN CAPITAL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///   * "и" (U+0438 CYRILLIC SMALL LETTER I)
  ///     => "И" (U+0418 CYRILLIC CAPITAL LETTER I)
  ///   * "π" (U+03C0 GREEK SMALL LETTER PI)
  ///     => "Π" (U+03A0 GREEK CAPITAL LETTER PI)
  ///   * "ß" (U+00DF LATIN SMALL LETTER SHARP S)
  ///     => "SS" (U+0053 LATIN CAPITAL LETTER S, U+0053 LATIN CAPITAL LETTER S)
  ///
  /// Note: Returns a String as case conversion can result in multiple
  /// Characters.
  public func uppercased() -> String { return String(self).uppercased() }

  /// Perform case conversion to lowercase
  ///
  /// Examples:
  ///   * "É" (U+0045 LATIN CAPITAL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///     => "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///   * "И" (U+0418 CYRILLIC CAPITAL LETTER I)
  ///     => "и" (U+0438 CYRILLIC SMALL LETTER I)
  ///   * "Π" (U+03A0 GREEK CAPITAL LETTER PI)
  ///     => "π" (U+03C0 GREEK SMALL LETTER PI)
  ///
  /// Note: Returns a String as case conversion can result in multiple
  /// Characters.
  public func lowercased() -> String { return String(self).lowercased() }

  @usableFromInline
  internal var _isUppercased: Bool { return String(self) == self.uppercased() }
  @usableFromInline
  internal var _isLowercased: Bool { return String(self) == self.lowercased() }

  /// Whether this Character is considered uppercase.
  ///
  /// Uppercase Characters vary under case-conversion to lowercase, but not when
  /// converted to uppercase.
  ///
  /// Examples:
  ///   * "É" (U+0045 LATIN CAPITAL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///   * "И" (U+0418 CYRILLIC CAPITAL LETTER I)
  ///   * "Π" (U+03A0 GREEK CAPITAL LETTER PI)
  ///
  @inlinable
  public var isUppercase: Bool {
    if _fastPath(_isSingleScalar && _firstScalar.properties.isUppercase) {
      return true
    }
    return _isUppercased && isCased
  }

  /// Whether this Character is considered lowercase.
  ///
  /// Lowercase Characters vary under case-conversion to uppercase, but not when
  /// converted to lowercase.
  ///
  /// Examples:
  ///   * "é" (U+0065 LATIN SMALL LETTER E, U+0301 COMBINING ACUTE ACCENT)
  ///   * "и" (U+0438 CYRILLIC SMALL LETTER I)
  ///   * "π" (U+03C0 GREEK SMALL LETTER PI)
  ///
  @inlinable
  public var isLowercase: Bool {
    if _fastPath(_isSingleScalar && _firstScalar.properties.isLowercase) {
      return true
    }
    return _isLowercased && isCased
  }

  /// Whether this Character changes under any form of case conversion.
  @inlinable
  public var isCased: Bool {
    if _fastPath(_isSingleScalar && _firstScalar.properties.isCased) {
      return true
    }
    return !_isUppercased || !_isLowercased
  }

  /// Whether this Character represents a symbol
  ///
  /// Examples:
  ///   * "®" (U+00AE REGISTERED SIGN)
  ///   * "⌹" (U+2339 APL FUNCTIONAL SYMBOL QUAD DIVIDE)
  ///   * "⡆" (U+2846 BRAILLE PATTERN DOTS-237)
  ///
  public var isSymbol: Bool {
    return _firstScalar.properties.generalCategory._isSymbol
  }

  /// Whether this Character represents a symbol used mathematical formulas
  ///
  /// Examples:
  ///   * "+" (U+002B PLUS SIGN)
  ///   * "∫" (U+222B INTEGRAL)
  ///   * "ϰ" (U+03F0 GREEK KAPPA SYMBOL)
  ///
  /// Note: This is not a strict subset of isSymbol. This includes characters
  /// used both as letters and commonly in mathematical formulas. For example,
  /// "ϰ" (U+03F0 GREEK KAPPA SYMBOL) is considered a both mathematical symbol
  /// and a letter.
  ///
  public var isMathSymbol: Bool {
    return _firstScalar.properties.isMath
  }

  /// Whether this Character represents a currency symbol
  ///
  /// Examples:
  ///   * "$" (U+0024 DOLLAR SIGN)
  ///   * "¥" (U+00A5 YEN SIGN)
  ///   * "€" (U+20AC EURO SIGN)
  ///
  public var isCurrencySymbol: Bool {
    return _firstScalar.properties.generalCategory == .currencySymbol
  }

  /// Whether this Character represents punctuation
  ///
  /// Examples:
  ///   * "!" (U+0021 EXCLAMATION MARK)
  ///   * "؟" (U+061F ARABIC QUESTION MARK)
  ///   * "…" (U+2026 HORIZONTAL ELLIPSIS)
  ///   * "—" (U+2014 EM DASH)
  ///   * "“" (U+201C LEFT DOUBLE QUOTATION MARK)
  ///
  public var isPunctuation: Bool {
    return _firstScalar.properties.generalCategory._isPunctuation
  }
}
