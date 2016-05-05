//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// UnicodeScalar Type
//===----------------------------------------------------------------------===//

/// A [Unicode scalar value](http://www.unicode.org/glossary/#unicode_scalar_value).
@_fixed_layout
public struct UnicodeScalar :
  _BuiltinUnicodeScalarLiteralConvertible,
  UnicodeScalarLiteralConvertible {

  var _value: UInt32

  /// A numeric representation of `self`.
  public var value: UInt32 { return _value }

  @_transparent
  public init(_builtinUnicodeScalarLiteral value: Builtin.Int32) {
    self._value = UInt32(value)
  }

  /// Create an instance initialized to `value`.
  @_transparent
  public init(unicodeScalarLiteral value: UnicodeScalar) {
    self = value
  }

  /// Create an instance with numeric value `v`.
  ///
  /// - Precondition: `v` is a valid Unicode scalar value.
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

    self._value = v
  }

  /// Create an instance with numeric value `v`.
  ///
  /// - Precondition: `v` is a valid Unicode scalar value.
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

  /// Returns a String representation of `self` .
  ///
  /// - parameter forceASCII: If `true`, forces most values into a numeric
  ///   representation.
  @warn_unused_result
  public func escaped(asASCII forceASCII: Bool) -> String {
    func lowNibbleAsHex(_ v: UInt32) -> String {
      let nibble = v & 15
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

  /// Returns `true` if this is an ASCII character (code point 0 to 127
  /// inclusive).
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
  /// A textual representation of `self`.
  public var description: String {
    return "\"\(escaped(asASCII: false))\""
  }
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "\"\(escaped(asASCII: true))\""
  }
}

extension UnicodeScalar : Hashable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  public var hashValue: Int {
    return Int(self.value)
  }
}

extension UnicodeScalar {
  /// Construct with value `v`.
  ///
  /// - Precondition: `v` is a valid unicode scalar value.
  public init(_ v: Int) {
    self = UnicodeScalar(UInt32(v))
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
  ///
  /// - Precondition: `v.value` can be represented as UInt32.
  public init(_ v: UnicodeScalar) {
    self = v.value
  }
}
extension UInt64 {
  /// Construct with value `v.value`.
  ///
  /// - Precondition: `v.value` can be represented as UInt64.
  public init(_ v: UnicodeScalar) {
    self = UInt64(v.value)
  }
}

extension UnicodeScalar : Comparable, Equatable {
}

@warn_unused_result
public func ==(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
  return lhs.value == rhs.value
}

@warn_unused_result
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

extension UnicodeScalar.UTF16View : RandomAccessCollection {

  typealias Indices = CountableRange<Int>

  /// The position of the first code unit.
  var startIndex: Int {
    return 0
  }

  /// The "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `index(after:)`.
  var endIndex: Int {
    return 0 + UTF16.width(value)
  }

  /// Access the code unit at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  subscript(position: Int) -> UTF16.CodeUnit {
    return position == 0 ? (
      endIndex == 1 ? UTF16.CodeUnit(value.value) : UTF16.leadSurrogate(value)
    ) : UTF16.trailSurrogate(value)
  }
}

/// Returns c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
@warn_unused_result
public // SPI(SwiftExperimental)
func _ascii16(_ c: UnicodeScalar) -> UTF16.CodeUnit {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

extension UnicodeScalar {
  /// Creates an instance of the NUL scalar value.
  @available(*, unavailable, message: "use the 'UnicodeScalar(\"\\0\")'")
  public init() {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "escaped")
  public func escape(asASCII forceASCII: Bool) -> String {
    Builtin.unreachable()
  }
}
