//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A single extended grapheme cluster that forms the smallest unit of
/// human-readable text.
///
/// A character most closely corresponds to what end users think of as a "character"
/// or "letter". However, what users think of as a character may be composed of
/// one or more Unicode code points. For example, "é" can be a single code point
/// or two code points ("e" + "´").
@frozen
public struct Character {
  @usableFromInline
  internal var _str: String

  /// Creates a character containing the given Unicode scalar value.
  @inlinable
  public init(_ scalar: UnicodeScalar) {
    _str = String(scalar)
  }

  /// Creates a character from a single-character string.
  @inlinable
  public init(_ s: String) {
    _precondition(s.count == 1, "String must contain exactly one character")
    _str = s
  }

  @inlinable
  internal init(_unchecked s: String) {
    _str = s
  }
}

// MARK: - ExpressibleByExtendedGraphemeClusterLiteral

extension Character: ExpressibleByExtendedGraphemeClusterLiteral {
  /// The type of the extended grapheme cluster literal, `Character`.
  public typealias ExtendedGraphemeClusterLiteralType = Character

  /// Creates a character from the given extended grapheme cluster literal.
  @_transparent
  public init(extendedGraphemeClusterLiteral value: Character) {
    self = value
  }
}

// MARK: - ExpressibleByUnicodeScalarLiteral

extension Character: ExpressibleByUnicodeScalarLiteral {
  /// The type of the Unicode scalar literal, `Character`.
  public typealias UnicodeScalarLiteralType = Character

  /// Creates a character from the given Unicode scalar literal.
  @_transparent
  public init(unicodeScalarLiteral value: Character) {
    self = value
  }
}

// MARK: - Equatable and Comparable

extension Character: Equatable {
  /// Returns a Boolean value indicating whether two characters are equal.
  @inlinable
  public static func == (lhs: Character, rhs: Character) -> Bool {
    return lhs._str == rhs._str
  }
}

extension Character: Comparable {
  /// Returns a Boolean value indicating whether the first character is ordered
  /// before the second in a lexicographical ordering.
  @inlinable
  public static func < (lhs: Character, rhs: Character) -> Bool {
    return lhs._str < rhs._str
  }
}

// MARK: - Hashable conformance

extension Character: Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    _str.hash(into: &hasher)
  }
}

// MARK: - String conversion

extension Character: CustomStringConvertible {
  /// A textual representation of the character.
  @inlinable
  public var description: String {
    return _str
  }
}

extension Character: CustomDebugStringConvertible {
  /// A textual representation of the character, suitable for debugging.
  public var debugDescription: String {
    return _str.debugDescription
  }
}

// MARK: - ASCII support

extension Character {
  /// The ASCII encoding value of this character, if it is an ASCII character.
  ///
  /// ASCII characters have code points in the range 0...127. If this character
  /// is not ASCII, this property is `nil`.
  @inlinable
  public var asciiValue: UInt8? {
    guard _str.count == 1 else { return nil }
    let scalar = _str.unicodeScalars.first!
    return scalar.isASCII ? UInt8(scalar.value) : nil
  }

  /// Creates a character from the given ASCII code point.
  ///
  /// - Parameter ascii: An ASCII code point (0...127).
  @inlinable
  public init?(ascii: UInt8) {
    guard ascii <= 127 else { return nil }
    self.init(UnicodeScalar(ascii)!)
  }

  /// A Boolean value indicating whether this character represents an ASCII character.
  @inlinable
  public var isASCII: Bool {
    return asciiValue != nil
  }
}

// MARK: - Unicode scalar access

extension Character {
  /// A view of the character's contents as a collection of Unicode scalar values.
  @inlinable
  public var unicodeScalars: String.UnicodeScalarView {
    return _str.unicodeScalars
  }
}

// MARK: - Case operations

extension Character {
  /// A lowercase version of the character.
  @inlinable
  public var lowercased: Character {
    return Character(_unchecked: _str.lowercased())
  }

  /// An uppercase version of the character.
  @inlinable
  public var uppercased: Character {
    return Character(_unchecked: _str.uppercased())
  }

  /// A Boolean value indicating whether the character is a lowercase letter.
  @inlinable
  public var isLowercase: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isLowercase }
  }

  /// A Boolean value indicating whether the character is an uppercase letter.
  @inlinable
  public var isUppercase: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isUppercase }
  }
}

// MARK: - Character classification

extension Character {
  /// A Boolean value indicating whether the character is a letter.
  @inlinable
  public var isLetter: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isAlphabetic }
  }

  /// A Boolean value indicating whether the character is a number.
  @inlinable
  public var isNumber: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isNumeric }
  }

  /// A Boolean value indicating whether the character is a symbol.
  @inlinable
  public var isSymbol: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isSymbol }
  }

  /// A Boolean value indicating whether the character is a punctuation mark.
  @inlinable
  public var isPunctuation: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isPunctuation }
  }

  /// A Boolean value indicating whether the character is whitespace.
  @inlinable
  public var isWhitespace: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isWhitespace }
  }

  /// A Boolean value indicating whether the character is a newline.
  @inlinable
  public var isNewline: Bool {
    return _str.unicodeScalars.allSatisfy { $0.properties.isNewline }
  }
}

// MARK: - UTF-8 support

extension Character {
  /// A UTF-8 view of the character's contents.
  @inlinable
  public var utf8: String.UTF8View {
    return _str.utf8
  }
}

// MARK: - UTF-16 support

extension Character {
  /// A UTF-16 view of the character's contents.
  @inlinable
  public var utf16: String.UTF16View {
    return _str.utf16
  }
}