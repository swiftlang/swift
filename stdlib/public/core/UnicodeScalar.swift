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
// UnicodeScalar Type
//===----------------------------------------------------------------------===//

/// A Unicode scalar value.
///
/// The `UnicodeScalar` type, representing a single Unicode scalar value, is
/// the element type of a string's `unicodeScalars` collection.
///
/// You can create a `UnicodeScalar` instance by using a string literal that
/// contains a single character representing exactly one Unicode scalar value.
///
///     let letterK: UnicodeScalar = "K"
///     let kim: UnicodeScalar = "김"
///     print(letterK, kim)
///     // Prints "K 김"
///
/// You can also create Unicode scalar values directly from their numeric
/// representation.
///
///     let airplane = UnicodeScalar(9992)
///     print(airplane)
///     // Prints "✈︎"
@_fixed_layout
public struct UnicodeScalar :
  _ExpressibleByBuiltinUnicodeScalarLiteral,
  ExpressibleByUnicodeScalarLiteral {

  @_versioned
  var _value: UInt32

  /// A numeric representation of the Unicode scalar.
  public var value: UInt32 { return _value }

  @_transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self._value = UInt32(value)
  }

  /// Creates a Unicode scalar with the specified value.
  ///
  /// Do not call this initializer directly. It may be used by the compiler
  /// when you use a string literal to initialize a `UnicodeScalar` instance.
  ///
  ///     let letterK: UnicodeScalar = "K"
  ///     print(letterK)
  ///     // Prints "K"
  ///
  /// In this example, the assignment to the `letterK` constant is handled by
  /// this initializer behind the scenes.
  @_transparent
  public init(unicodeScalarLiteral value: UnicodeScalar) {
    self = value
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. `v` must be
  ///   a valid Unicode scalar value, in the range `0...0xD7FF` or
  ///   `0xE000...0x10FFFF`. In case of an invalid unicode scalar value, nil is
  ///   returned.
  ///
  /// For example, the following code sample creates a `UnicodeScalar` instance
  /// with a value of an emoji character:
  ///
  ///     let codepoint: UInt32 = 127881
  ///     let emoji = UnicodeScalar(codepoint)
  ///     print(emoji!)
  ///     // Prints "🎉"
  ///
  /// In case of an invalid input value, nil is returned.
  ///
  ///     let codepoint: UInt32 = extValue // This might be an invalid value. 
  ///     if let emoji = UnicodeScalar(codepoint) {
  ///       print(emoji)
  ///     } else {
  ///       // Do something else
  ///     }
  public init?(_ v: UInt32) {
    // Unicode 6.3.0:
    //
    //     D9.  Unicode codespace: A range of integers from 0 to 10FFFF.
    //
    //     D76. Unicode scalar value: Any Unicode code point except
    //     high-surrogate and low-surrogate code points.
    //
    //     * As a result of this definition, the set of Unicode scalar values
    //     consists of the ranges 0 to D7FF and E000 to 10FFFF, inclusive.
    if (v < 0xD800 || v > 0xDFFF) && v <= 0x10FFFF {
      self._value = v
      return
    }
    // Return nil in case of invalid unicode scalar value.
    return nil
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. `v` must be
  ///   a valid Unicode scalar value, in the range `0...0xD7FF` or
  ///   `0xE000...0xFFFF`. In case of an invalid unicode scalar value, nil is
  ///   returned.
  ///
  /// For example, the following code sample creates a `UnicodeScalar` instance
  /// with a value of `밥`, the Korean word for rice:
  ///
  ///     let codepoint: UInt16 = 48165
  ///     let bap = UnicodeScalar(codepoint)
  ///     print(bap!)
  ///     // Prints "밥"
  ///
  /// In case an invalid input value, nil is returned.
  ///
  ///     let codepoint: UInt32 = extValue // This might be an invalid value. 
  ///     if let bap = UnicodeScalar(codepoint) {
  ///       print(bap)
  ///     } else {
  ///       // Do something else
  ///     }
  public init?(_ v: UInt16) {
    self.init(UInt32(v))
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// For example, the following code sample creates a `UnicodeScalar` instance
  /// with a value of `7`:
  ///
  ///     let codepoint: UInt8 = 55
  ///     let seven = UnicodeScalar(codepoint)
  ///     print(seven!)
  ///     // Prints "7"
  ///
  /// - Parameter v: The code point to use for the scalar.
  public init(_ v: UInt8) {
    self._value = UInt32(v)
  }

  /// Creates a duplicate of the given Unicode scalar.
  public init(_ v: UnicodeScalar) {
    // This constructor allows one to provide necessary type context to
    // disambiguate between function overloads on 'String' and 'UnicodeScalar'.
    self = v
  }

  /// Returns a string representation of the Unicode scalar.
  ///
  /// Scalar values representing characters that are normally unprintable or
  /// that otherwise require escaping are escaped with a backslash.
  ///
  ///     let tab = UnicodeScalar(9)
  ///     print(tab)
  ///     // Prints " "
  ///     print(tab.escaped(asASCII: false))
  ///     // Prints "\t"
  ///
  /// When the `forceASCII` parameter is `true`, a `UnicodeScalar` instance
  /// with a value greater than 127 is represented using an escaped numeric
  /// value; otherwise, non-ASCII characters are represented using their
  /// typical string value.
  ///
  ///     let bap = UnicodeScalar(48165)
  ///     print(bap.escaped(asASCII: false))
  ///     // Prints "밥"
  ///     print(bap.escaped(asASCII: true))
  ///     // Prints "\u{BC25}"
  ///
  /// - Parameter forceASCII: Pass `true` if you need the result to use only
  ///   ASCII characters; otherwise, pass `false`.
  /// - Returns: A string representation of the scalar.
  public func escaped(asASCII forceASCII: Bool) -> String {
    func lowNibbleAsHex(_ v: UInt32) -> String {
      let nibble = v & 15
      if nibble < 10 {
        return String(UnicodeScalar(nibble+48)!)    // 48 = '0'
      } else {
        // FIXME: was UnicodeScalar(nibble-10+65), which is now
        // ambiguous.  <rdar://problem/18506025>
        return String(UnicodeScalar(nibble+65-10)!) // 65 = 'A'
      }
    }

    if self == "\\" {
      return "\\\\"
    } else if self == "\'" {
      return "\\\'"
    } else if self == "\"" {
      return "\\\""
    } else if _isPrintableASCII {
      return String(self)
    } else if self == "\0" {
      return "\\0"
    } else if self == "\n" {
      return "\\n"
    } else if self == "\r" {
      return "\\r"
    } else if self == "\t" {
      return "\\t"
    } else if UInt32(self) < 128 {
      return "\\u{"
        + lowNibbleAsHex(UInt32(self) >> 4)
        + lowNibbleAsHex(UInt32(self)) + "}"
    } else if !forceASCII {
      return String(self)
    } else if UInt32(self) <= 0xFFFF {
      var result = "\\u{"
      result += lowNibbleAsHex(UInt32(self) >> 12)
      result += lowNibbleAsHex(UInt32(self) >> 8)
      result += lowNibbleAsHex(UInt32(self) >> 4)
      result += lowNibbleAsHex(UInt32(self))
      result += "}"
      return result
    } else {
      // FIXME: Type checker performance prohibits this from being a
      // single chained "+".
      var result = "\\u{"
      result += lowNibbleAsHex(UInt32(self) >> 28)
      result += lowNibbleAsHex(UInt32(self) >> 24)
      result += lowNibbleAsHex(UInt32(self) >> 20)
      result += lowNibbleAsHex(UInt32(self) >> 16)
      result += lowNibbleAsHex(UInt32(self) >> 12)
      result += lowNibbleAsHex(UInt32(self) >> 8)
      result += lowNibbleAsHex(UInt32(self) >> 4)
      result += lowNibbleAsHex(UInt32(self))
      result += "}"
      return result
    }
  }

  /// A Boolean value indicating whether the Unicode scalar is an ASCII
  /// character.
  ///
  /// ASCII characters have a scalar value between 0 and 127, inclusive. For
  /// example:
  ///
  ///     let canyon = "Cañón"
  ///     for scalar in canyon.unicodeScalars {
  ///         print(scalar, scalar.isASCII, scalar.value)
  ///     }
  ///     // Prints "C true 67"
  ///     // Prints "a true 97"
  ///     // Prints "ñ false 241"
  ///     // Prints "ó false 243"
  ///     // Prints "n true 110"
  public var isASCII: Bool {
    return value <= 127
  }

  // FIXME: Is there a similar term of art in Unicode?
  public var _isASCIIDigit: Bool {
    return self >= "0" && self <= "9"
  }

  // FIXME: Unicode makes this interesting.
  internal var _isPrintableASCII: Bool {
    return (self >= UnicodeScalar(0o040) && self <= UnicodeScalar(0o176))
  }
}

extension UnicodeScalar : CustomStringConvertible, CustomDebugStringConvertible {
  /// A textual representation of the Unicode scalar.
  public var description: String {
    return String._fromWellFormedCodeUnitSequence(
      UTF32.self,
      input: repeatElement(self.value, count: 1))
  }

  /// An escaped textual representation of the Unicode scalar, suitable for
  /// debugging.
  public var debugDescription: String {
    return "\"\(escaped(asASCII: true))\""
  }
}

extension UnicodeScalar : LosslessStringConvertible {
  public init?(_ description: String) {
    let scalars = description.unicodeScalars
    guard let v = scalars.first, scalars.count == 1 else {
      return nil
    }
    self = v
  }
}

extension UnicodeScalar : Hashable {
  /// The Unicode scalar's hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  public var hashValue: Int {
    return Int(self.value)
  }
}

extension UnicodeScalar {
  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. `v` must be
  ///   a valid Unicode scalar value, in the ranges `0...0xD7FF` or
  ///   `0xE000...0x10FFFF`. In case of an invalid unicode scalar value, nil is
  ///   returned.
  ///
  /// For example, the following code sample creates a `UnicodeScalar` instance
  /// with a value of an emoji character:
  ///
  ///     let codepoint = 127881
  ///     let emoji = UnicodeScalar(codepoint)
  ///     print(emoji)
  ///     // Prints "🎉"
  ///
  /// In case an invalid input value, nil is returned.
  ///
  ///     let codepoint: UInt32 = extValue // This might be an invalid value. 
  ///     if let emoji = UnicodeScalar(codepoint) {
  ///       print(emoji)
  ///     } else {
  ///       // Do something else
  ///     }
  public init?(_ v: Int) {
    if let us = UnicodeScalar(UInt32(v)) {
      self = us
    } else {
      return nil
    }
  }
}

extension UnicodeScalar {
  static var replacementCharacter: UnicodeScalar {
    return UnicodeScalar(_unchecked: 0xFFFD)
  }
}

extension UInt8 {
  /// Construct with value `v.value`.
  ///
  /// - Precondition: `v.value` can be represented as ASCII (0..<128).
  public init(ascii v: UnicodeScalar) {
    _precondition(v.value < 128,
        "Code point value does not fit into ASCII")
    self = UInt8(v.value)
  }
}
extension UInt32 {
  /// Construct with value `v.value`.
  public init(_ v: UnicodeScalar) {
    self = v.value
  }
}
extension UInt64 {
  /// Construct with value `v.value`.
  public init(_ v: UnicodeScalar) {
    self = UInt64(v.value)
  }
}

extension UnicodeScalar : Equatable {
  public static func == (lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
    return lhs.value == rhs.value
  }
}

extension UnicodeScalar : Comparable {
  public static func < (lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
    return lhs.value < rhs.value
  }
}

extension UnicodeScalar {
  public struct UTF16View {
    internal var value: UnicodeScalar
  }

  public var utf16: UTF16View {
    return UTF16View(value: self)
  }
}

extension UnicodeScalar.UTF16View : RandomAccessCollection {

  public typealias Indices = CountableRange<Int>

  /// The position of the first code unit.
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  public var endIndex: Int {
    return 0 + UTF16.width(value)
  }

  /// Accesses the code unit at the specified position.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  public subscript(position: Int) -> UTF16.CodeUnit {
    return position == 0 ? (
      endIndex == 1 ? UTF16.CodeUnit(value.value) : UTF16.leadSurrogate(value)
    ) : UTF16.trailSurrogate(value)
  }
}

/// Returns c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
public // SPI(SwiftExperimental)
func _ascii16(_ c: UnicodeScalar) -> UTF16.CodeUnit {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

