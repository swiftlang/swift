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
// Unicode.Scalar Type
//===----------------------------------------------------------------------===//

extension Unicode {
  /// A Unicode scalar value.
  ///
  /// The `Unicode.Scalar` type, representing a single Unicode scalar value, is
  /// the element type of a string's `unicodeScalars` collection.
  ///
  /// You can create a `Unicode.Scalar` instance by using a string literal that
  /// contains a single character representing exactly one Unicode scalar value.
  ///
  ///     let letterK: Unicode.Scalar = "K"
  ///     let kim: Unicode.Scalar = "김"
  ///     print(letterK, kim)
  ///     // Prints "K 김"
  ///
  /// You can also create Unicode scalar values directly from their numeric
  /// representation.
  ///
  ///     let airplane = Unicode.Scalar(9992)
  ///     print(airplane)
  ///     // Prints "✈︎"
  @_fixed_layout
  public struct Scalar {    
    @inlinable // FIXME(sil-serialize-all)
    internal init(_value: UInt32) {
      self._value = _value
    }

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _value: UInt32
  }
}

extension Unicode.Scalar :
    _ExpressibleByBuiltinUnicodeScalarLiteral,
    ExpressibleByUnicodeScalarLiteral {
  /// A numeric representation of the Unicode scalar.
  @inlinable // FIXME(sil-serialize-all)
  public var value: UInt32 { return _value }

  @_transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self._value = UInt32(value)
  }

  /// Creates a Unicode scalar with the specified value.
  ///
  /// Do not call this initializer directly. It may be used by the compiler
  /// when you use a string literal to initialize a `Unicode.Scalar` instance.
  ///
  ///     let letterK: Unicode.Scalar = "K"
  ///     print(letterK)
  ///     // Prints "K"
  ///
  /// In this example, the assignment to the `letterK` constant is handled by
  /// this initializer behind the scenes.
  @_transparent
  public init(unicodeScalarLiteral value: Unicode.Scalar) {
    self = value
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// For example, the following code sample creates a `Unicode.Scalar`
  /// instance with a value of an emoji character:
  ///
  ///     let codepoint: UInt32 = 127881
  ///     let emoji = Unicode.Scalar(codepoint)
  ///     print(emoji!)
  ///     // Prints "🎉"
  ///
  /// In case of an invalid input value, nil is returned.
  ///
  ///     let codepoint: UInt32 = extValue   // This might be an invalid value
  ///     if let emoji = Unicode.Scalar(codepoint) {
  ///       print(emoji)
  ///     } else {
  ///       // Do something else
  ///     }
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. The
  ///   initializer succeeds if `v` is a valid Unicode scalar value---that is,
  ///   if `v` is in the range `0...0xD7FF` or `0xE000...0x10FFFF`. If `v` is
  ///   an invalid Unicode scalar value, the result is `nil`.
  @inlinable // FIXME(sil-serialize-all)
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
    // Return nil in case of an invalid unicode scalar value.
    return nil
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// For example, the following code sample creates a `Unicode.Scalar`
  /// instance with a value of `"밥"`, the Korean word for rice:
  ///
  ///     let codepoint: UInt16 = 48165
  ///     let bap = Unicode.Scalar(codepoint)
  ///     print(bap!)
  ///     // Prints "밥"
  ///
  /// In case of an invalid input value, the result is `nil`.
  ///
  ///     let codepoint: UInt16 = extValue   // This might be an invalid value
  ///     if let bap = Unicode.Scalar(codepoint) {
  ///         print(bap)
  ///     } else {
  ///         // Do something else
  ///     }
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. The
  ///   initializer succeeds if `v` is a valid Unicode scalar value, in the
  ///   range `0...0xD7FF` or `0xE000...0x10FFFF`. If `v` is an invalid
  ///   unicode scalar value, the result is `nil`.
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ v: UInt16) {
    self.init(UInt32(v))
  }

  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// For example, the following code sample creates a `Unicode.Scalar`
  /// instance with a value of `"7"`:
  ///
  ///     let codepoint: UInt8 = 55
  ///     let seven = Unicode.Scalar(codepoint)
  ///     print(seven)
  ///     // Prints "7"
  ///
  /// - Parameter v: The code point to use for the scalar.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ v: UInt8) {
    self._value = UInt32(v)
  }

  /// Creates a duplicate of the given Unicode scalar.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ v: Unicode.Scalar) {
    // This constructor allows one to provide necessary type context to
    // disambiguate between function overloads on 'String' and 'Unicode.Scalar'.
    self = v
  }

  /// Returns a string representation of the Unicode scalar.
  ///
  /// Scalar values representing characters that are normally unprintable or
  /// that otherwise require escaping are escaped with a backslash.
  ///
  ///     let tab = Unicode.Scalar(9)
  ///     print(tab)
  ///     // Prints " "
  ///     print(tab.escaped(asASCII: false))
  ///     // Prints "\t"
  ///
  /// When the `forceASCII` parameter is `true`, a `Unicode.Scalar` instance
  /// with a value greater than 127 is represented using an escaped numeric
  /// value; otherwise, non-ASCII characters are represented using their
  /// typical string value.
  ///
  ///     let bap = Unicode.Scalar(48165)
  ///     print(bap.escaped(asASCII: false))
  ///     // Prints "밥"
  ///     print(bap.escaped(asASCII: true))
  ///     // Prints "\u{BC25}"
  ///
  /// - Parameter forceASCII: Pass `true` if you need the result to use only
  ///   ASCII characters; otherwise, pass `false`.
  /// - Returns: A string representation of the scalar.
  @inlinable // FIXME(sil-serialize-all)
  public func escaped(asASCII forceASCII: Bool) -> String {
    func lowNibbleAsHex(_ v: UInt32) -> String {
      let nibble = v & 15
      if nibble < 10 {
        return String(Unicode.Scalar(nibble+48)!)    // 48 = '0'
      } else {
        return String(Unicode.Scalar(nibble-10+65)!) // 65 = 'A'
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
  @inlinable // FIXME(sil-serialize-all)
  public var isASCII: Bool {
    return value <= 127
  }

  // FIXME: Is there a similar term of art in Unicode?
  @inlinable // FIXME(sil-serialize-all)
  public var _isASCIIDigit: Bool {
    return self >= "0" && self <= "9"
  }

  // FIXME: Unicode makes this interesting.
  @inlinable // FIXME(sil-serialize-all)
  internal var _isPrintableASCII: Bool {
    return (self >= Unicode.Scalar(0o040) && self <= Unicode.Scalar(0o176))
  }
}

extension Unicode.Scalar : CustomStringConvertible, CustomDebugStringConvertible {
  /// A textual representation of the Unicode scalar.
  @inlinable // FIXME(sil-serialize-all)
  public var description: String {
    return String(self)
  }

  /// An escaped textual representation of the Unicode scalar, suitable for
  /// debugging.
  public var debugDescription: String {
    return "\"\(escaped(asASCII: true))\""
  }
}

extension Unicode.Scalar : LosslessStringConvertible {
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ description: String) {
    let scalars = description.unicodeScalars
    guard let v = scalars.first, scalars.count == 1 else {
      return nil
    }
    self = v
  }
}

extension Unicode.Scalar : Hashable {
  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(self.value)
  }
}

extension Unicode.Scalar {
  /// Creates a Unicode scalar with the specified numeric value.
  ///
  /// - Parameter v: The Unicode code point to use for the scalar. `v` must be
  ///   a valid Unicode scalar value, in the ranges `0...0xD7FF` or
  ///   `0xE000...0x10FFFF`. In case of an invalid unicode scalar value, nil is
  ///   returned.
  ///
  /// For example, the following code sample creates a `Unicode.Scalar` instance
  /// with a value of an emoji character:
  ///
  ///     let codepoint = 127881
  ///     let emoji = Unicode.Scalar(codepoint)
  ///     print(emoji)
  ///     // Prints "🎉"
  ///
  /// In case of an invalid input value, nil is returned.
  ///
  ///     let codepoint: UInt32 = extValue // This might be an invalid value. 
  ///     if let emoji = Unicode.Scalar(codepoint) {
  ///       print(emoji)
  ///     } else {
  ///       // Do something else
  ///     }
  @inlinable // FIXME(sil-serialize-all)
  public init?(_ v: Int) {
    if let us = Unicode.Scalar(UInt32(v)) {
      self = us
    } else {
      return nil
    }
  }
}

extension UInt8 {
  /// Construct with value `v.value`.
  ///
  /// - Precondition: `v.value` can be represented as ASCII (0..<128).
  @inlinable // FIXME(sil-serialize-all)
  public init(ascii v: Unicode.Scalar) {
    _precondition(v.value < 128,
        "Code point value does not fit into ASCII")
    self = UInt8(v.value)
  }
}
extension UInt32 {
  /// Construct with value `v.value`.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ v: Unicode.Scalar) {
    self = v.value
  }
}
extension UInt64 {
  /// Construct with value `v.value`.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ v: Unicode.Scalar) {
    self = UInt64(v.value)
  }
}

extension Unicode.Scalar : Equatable {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (lhs: Unicode.Scalar, rhs: Unicode.Scalar) -> Bool {
    return lhs.value == rhs.value
  }
}

extension Unicode.Scalar : Comparable {
  @inlinable // FIXME(sil-serialize-all)
  public static func < (lhs: Unicode.Scalar, rhs: Unicode.Scalar) -> Bool {
    return lhs.value < rhs.value
  }
}

extension Unicode.Scalar {
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct UTF16View {
    @inlinable // FIXME(sil-serialize-all)    
    internal init(value: Unicode.Scalar) {
      self.value = value
    }
    @usableFromInline // FIXME(sil-serialize-all)
    internal var value: Unicode.Scalar
  }

  @inlinable // FIXME(sil-serialize-all)
  public var utf16: UTF16View {
    return UTF16View(value: self)
  }
}

extension Unicode.Scalar.UTF16View : RandomAccessCollection {

  public typealias Indices = Range<Int>

  /// The position of the first code unit.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Int {
    return 0 + UTF16.width(value)
  }

  /// Accesses the code unit at the specified position.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(position: Int) -> UTF16.CodeUnit {
    return position == 0 ? (
      endIndex == 1 ? UTF16.CodeUnit(value.value) : UTF16.leadSurrogate(value)
    ) : UTF16.trailSurrogate(value)
  }
}

/// Returns c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
@inlinable // FIXME(sil-serialize-all)
public // SPI(SwiftExperimental)
func _ascii16(_ c: Unicode.Scalar) -> UTF16.CodeUnit {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

extension Unicode.Scalar {
  @inlinable // FIXME(sil-serialize-all)
  internal static var _replacementCharacter: Unicode.Scalar {
    return Unicode.Scalar(_value: UTF32._replacementCodeUnit)
  }
}

extension Unicode.Scalar {
  /// Creates an instance of the NUL scalar value.
  @available(*, unavailable, message: "use 'Unicode.Scalar(0)'")
  public init() {
    Builtin.unreachable()
  }
}
