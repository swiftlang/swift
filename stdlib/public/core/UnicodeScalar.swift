//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// UnicodeScalar Type
//===----------------------------------------------------------------------===//

/// A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
public struct UnicodeScalar :
  _BuiltinUnicodeScalarLiteralConvertible,
  UnicodeScalarLiteralConvertible {

  var _value: Builtin.Int32

  /// A numeric representation of `self`.
  public var value: UInt32 {
    get {
      return UInt32(_value)
    }
  }

  @transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self._value = value
  }

  /// Create an instance initialized to `value`.
  @transparent
  public init(unicodeScalarLiteral value: UnicodeScalar) {
    self = value
  }

  /// Creates an instance of the NUL scalar value.
  public init() {
    self._value = Int32(0).value
  }

  init(_ value : Builtin.Int32) {
    self._value = value
  }

  /// Create an instance with numeric value `v`.
  ///
  /// Requires: `v` is a valid Unicode scalar value.
  public init(_ v: UInt32) {
    // Unicode 6.3.0:
    //
    //     D9.  Unicode codespace: A range of integers from 0 to 10FFFF.
    //
    //     D76. Unicode scalar value: Any Unicode code point except
    //     high-surrogate and low-surrogate code points.
    //
    //     * As a result of this definition, the set of Unicode scalar values
    //     consists of the ranges 0 to D7FF and E000 to 10FFFF, inclusive.

    _precondition(v < 0xD800 || v > 0xDFFF,
        "high- and low-surrogate code points are not valid Unicode scalar values")
    _precondition(v <= 0x10FFFF, "value is outside of Unicode codespace")

    self._value = v.value
  }

  /// Create an instance with numeric value `v`.
  ///
  /// Requires: `v` is a valid Unicode scalar value.
  public init(_ v: UInt16) {
    self = UnicodeScalar(UInt32(v))
  }

  /// Create an instance with numeric value `v`.
  public init(_ v: UInt8) {
    self = UnicodeScalar(UInt32(v))
  }

  /// Create a duplicate of `v`.
  public init(_ v: UnicodeScalar) {
    // This constructor allows one to provide necessary type context to
    // disambiguate between function overloads on 'String' and 'UnicodeScalar'.
    self = v
  }

  /// Return a String representation of `self` .
  ///
  /// - parameter asASCII: if `true`, forces most values into a numeric
  /// representation.
  public func escape(#asASCII: Bool) -> String {
    func lowNibbleAsHex(v: UInt32) -> String {
      var nibble = v & 15
      if nibble < 10 {
        return String(UnicodeScalar(nibble+48))    // 48 = '0'
      } else {
        // FIXME: was UnicodeScalar(nibble-10+65), which is now
        // ambiguous.  <rdar://problem/18506025>
        return String(UnicodeScalar(nibble+65-10)) // 65 = 'A'
      }
    }

    if self == "\\" {
      return "\\\\"
    } else if self == "\'" {
      return "\\\'"
    } else if self == "\"" {
      return "\\\""
    } else if _isPrintableASCII() {
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
    } else if !asASCII {
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

  /// Returns true if this is an ASCII character (code point 0 to 127
  /// inclusive).
  public func isASCII() -> Bool {
    return value <= 127
  }

  // FIXME: Locales make this interesting
  func _isAlpha() -> Bool {
    return (self >= "A" && self <= "Z") || (self >= "a" && self <= "z")
  }

  // FIXME: Is there an similar term of art in Unicode?
  public func _isASCIIDigit() -> Bool {
    return self >= "0" && self <= "9"
  }

  // FIXME: Unicode makes this interesting
  func _isDigit() -> Bool {
    return _isASCIIDigit()
  }

  // FIXME: Unicode and locales make this interesting
  var _uppercase: UnicodeScalar {
    if self >= "a" && self <= "z" {
      return UnicodeScalar(UInt32(self) &- 32)
    } else if self >= "à" && self <= "þ" && self != "÷" {
      return UnicodeScalar(UInt32(self) &- 32)
    }
    return self
  }

  // FIXME: Unicode and locales make this interesting
  var _lowercase: UnicodeScalar {
    if self >= "A" && self <= "Z" {
      return UnicodeScalar(UInt32(self) &+ 32)
    } else if self >= "À" && self <= "Þ" && self != "×" {
      return UnicodeScalar(UInt32(self) &+ 32)
    }
    return self
  }

  // FIXME: Unicode makes this interesting.
  public // @testable
  func _isSpace() -> Bool {
    // FIXME: The constraint-based type checker goes painfully exponential
    // when we turn this into one large expression. Break it up for now,
    // until we can optimize the constraint solver better.
    if self == " "  || self == "\t" { return true }
    if self == "\n" || self == "\r" { return true }
    return self == "\u{0B}" || self == "\u{0C}"
  }

  // FIXME: Unicode makes this interesting.
  func _isPrintableASCII() -> Bool {
    return (self >= UnicodeScalar(0o040) && self <= UnicodeScalar(0o176))
  }
}

extension UnicodeScalar : CustomStringConvertible, CustomDebugStringConvertible {
  /// A textual representation of `self`.
  public var description: String {
    return "\"\(escape(asASCII: false))\""
  }
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "\"\(escape(asASCII: true))\""
  }
}

extension UnicodeScalar : Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`
  ///
  /// - note: the hash value is not guaranteed to be stable across
  /// different invocations of the same program.  Do not persist the
  /// hash value across program runs.
  public var hashValue: Int {
    return Int(self.value)
  }
}

extension UnicodeScalar {
  /// Construct with value `v`.
  ///
  /// Requires: `v` is a valid unicode scalar value.
  public init(_ v: Int) {
    self = UnicodeScalar(UInt32(v))
  }
}

extension UInt8 {
  /// Construct with value `v.value`.
  ///
  /// Requires: `v.value` can be represented as ASCII (0..<128).
  public init(ascii v: UnicodeScalar) {
    _precondition(v.value < 128,
        "Code point value does not fit into ASCII")
    self = UInt8(v.value)
  }
}
extension UInt32 {
  /// Construct with value `v.value`.
  ///
  /// Requires: `v.value` can be represented as UInt32.
  public init(_ v: UnicodeScalar) {
    self = v.value
  }
}
extension UInt64 {
  /// Construct with value `v.value`.
  ///
  /// Requires: `v.value` can be represented as UInt64.
  public init(_ v: UnicodeScalar) {
    self = UInt64(v.value)
  }
}

extension UnicodeScalar : Comparable, Equatable {
}

public func ==(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
  return lhs.value == rhs.value
}

public func <(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
  return lhs.value < rhs.value
}

extension UnicodeScalar {
  struct UTF16View {
    var value: UnicodeScalar
  }

  var utf16: UTF16View {
    return UTF16View(value: self)
  }
}

extension UnicodeScalar.UTF16View : CollectionType {
  /// The position of the first code unit.
  var startIndex: Int {
    return 0
  }

  /// The "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  var endIndex: Int {
    return 0 + UTF16.width(value)
  }

  /// Access the code unit at `position`.
  ///
  /// Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  subscript(position: Int) -> UTF16.CodeUnit {
    return position == 0 ? (
      endIndex == 1 ? UTF16.CodeUnit(value.value) : UTF16.leadSurrogate(value)
    ) : UTF16.trailSurrogate(value)
  }

  /// Return a *generator* over the code points that comprise this
  /// *sequence*.
  ///
  /// - complexity: O(1)
  func generate() -> IndexingGenerator<UnicodeScalar.UTF16View> {
    return IndexingGenerator(self)
  }
}

/// Return c as a UTF8.CodeUnit.  Meant to be used as _ascii8("x").
public // SPI(SwiftExperimental)
func _ascii8(c: UnicodeScalar) -> UTF8.CodeUnit {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF8.CodeUnit(c.value)
}

/// Return c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
public // SPI(SwiftExperimental)
func _ascii16(c: UnicodeScalar) -> UTF16.CodeUnit {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

