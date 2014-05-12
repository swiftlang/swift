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

struct UnicodeScalar :
    ExtendedGraphemeClusterLiteralConvertible, ReplPrintable {

  var _value: Builtin.Int32

  var value: UInt32 {
    get {
      return UInt32(_value)
    }
  }

  static func convertFromExtendedGraphemeClusterLiteral(
      value: String) -> UnicodeScalar {
    let unicodeScalars = value.unicodeScalars
    return unicodeScalars[unicodeScalars.startIndex]
  }

  init() {
    self._value = Int32(0).value
  }

  init(_ value : Builtin.Int32) {
    self._value = value
  }

  init(_ v : UInt32) {
    var lowHalf = v & 0xFFFF
    // reserved in each plane
    assert(lowHalf != 0xFFFE && lowHalf != 0xFFFF)
    // UTF-16 surrogate pair values are not valid code points
    assert(v < 0xD800 || v > 0xDFFF)
    // U+FDD0...U+FDEF are also reserved
    assert(v < 0xFDD0 || v > 0xFDEF)
    // beyond what is defined to be valid
    assert(v < 0x10FFFF)

    self._value = v.value
  }

  init(_ v: UnicodeScalar) {
    // This constructor allows one to provide necessary type context to
    // disambiguate between function overloads on 'String' and 'UnicodeScalar'.
    self = v
  }

  func replPrint() {
    print("\"")
    print(escape())
    print("\"")
  }

  func escape() -> String {
    func lowNibbleAsHex(v: UInt32) -> String {
      var nibble = v & 15
      if nibble < 10 {
        return String(UnicodeScalar(nibble+48))    // 48 = '0'
      } else {
        return String(UnicodeScalar(nibble-10+65)) // 65 = 'A'
      }
    }

    if self == "\\" {
      return "\\\\"
    } else if self == "\'" {
      return "\\\'"
    } else if self == "\"" {
      return "\\\""
    } else if isPrint() {
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
      return "\\x"  
        + lowNibbleAsHex(UInt32(self) >> 4)
        + lowNibbleAsHex(UInt32(self))
    } else if _isUTF8() {
      return String(self)
    } else if UInt32(self) <= 0xFFFF {
      return "\\u"  
        + lowNibbleAsHex(UInt32(self) >> 12)
        + lowNibbleAsHex(UInt32(self) >> 8)
        + lowNibbleAsHex(UInt32(self) >> 4)
        + lowNibbleAsHex(UInt32(self))
    } else {
      // FIXME: Type checker performance prohibits this from being a 
      // single chained "+".
      var result = "\\U"  
      result += lowNibbleAsHex(UInt32(self) >> 28)
      result += lowNibbleAsHex(UInt32(self) >> 24)
      result += lowNibbleAsHex(UInt32(self) >> 20)
      result += lowNibbleAsHex(UInt32(self) >> 16)
      result += lowNibbleAsHex(UInt32(self) >> 12)
      result += lowNibbleAsHex(UInt32(self) >> 8)
      result += lowNibbleAsHex(UInt32(self) >> 4)
      result += lowNibbleAsHex(UInt32(self))
      return result
    }
  }

  /// \returns true if this is an ASCII character (code point 0 to 127
  /// inclusive).
  func isASCII() -> Bool {
    return value <= 127
  }

  // FIXME: Locales make this interesting
  func isAlpha() -> Bool {
    return (self >= "A" && self <= "Z") || (self >= "a" && self <= "z")
  }

  // FIXME: Locales make this interesting
  func isDigit() -> Bool {
    return self >= "0" && self <= "9"
  }

  // FIXME: Locales make this interesting
  var uppercase : UnicodeScalar {
    if self >= "a" && self <= "z" {
      return UnicodeScalar(UInt32(self) - 32)
    } else if self >= "à" && self <= "þ" && self != "÷" {
      return UnicodeScalar(UInt32(self) - 32)
    }
    return self
  }

  // FIXME: Locales make this interesting
  var lowercase : UnicodeScalar {
    if self >= "A" && self <= "Z" {
      return UnicodeScalar(UInt32(self) + 32)
    } else if self >= "À" && self <= "Þ" && self != "×" {
      return UnicodeScalar(UInt32(self) + 32)
    }
    return self
  }

  // FIXME: Locales make this interesting.
  func isSpace() -> Bool {
    // FIXME: The constraint-based type checker goes painfully exponential
    // when we turn this into one large expression. Break it up for now,
    // until we can optimize the constraint solver better.
    if self == " "  || self == "\t" { return true }
    if self == "\n" || self == "\r" { return true }
    return self == "\x0B" || self == "\x0C"
  }
}

extension UnicodeScalar : FormattedPrintable {
  func format(kind: UnicodeScalar, layout: String) -> String {
    return String(self).format(kind, layout: layout)
  }
}

extension UnicodeScalar : Hashable {
  var hashValue: Int {
    return Int(self.value)
  }
}

extension UnicodeScalar {
  init(_ v : Int) {
    self = UnicodeScalar(UInt32(v))
  }
}

extension UInt8 {
  init(_ v : UnicodeScalar) {
    assert(v.value <= UInt32(UInt8.max), "Code point value does not fit into UInt8")
    self = UInt8(v.value)
  }
}
extension UInt32 {
  init(_ v : UnicodeScalar) {
    self = v.value
  }
}
extension UInt64 {
  init(_ v : UnicodeScalar) {
    self = UInt64(v.value)
  }
}

func - (lhs: UnicodeScalar, rhs: UnicodeScalar) -> Int {
  return Int(lhs.value) - Int(rhs.value)
}

func - (lhs: UnicodeScalar, rhs: Int) -> UnicodeScalar {
  return UnicodeScalar(Int(lhs.value) - rhs)
}

func + (lhs: UnicodeScalar, rhs: Int) -> UnicodeScalar {
  return UnicodeScalar(Int(lhs.value) + rhs)
}

func + (lhs: Int, rhs: UnicodeScalar) -> UnicodeScalar {
  return rhs + lhs
}

func ==(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
  return lhs.value == rhs.value
}

extension UnicodeScalar : Comparable {
}

func <(lhs: UnicodeScalar, rhs: UnicodeScalar) -> Bool {
  return lhs.value < rhs.value
}

extension UnicodeScalar {
  func isPrint() -> Bool {
    return (self >= UnicodeScalar(0o040) && self <= UnicodeScalar(0o176))
  }
}

/// Helpers to provide type context to guide type inference in code like::
///
///   var zero = _asUnicodeCodePoint('0')
func _asUnicodeCodePoint(us: UnicodeScalar) -> Builtin.Int32 {
  return us._value
}
func _asUnicodeCodePoint(us: UnicodeScalar) -> UInt32 {
  return us.value
}
func _asUTF16CodeUnit(us: UnicodeScalar) -> UTF16.CodeUnit {
  var codePoint = us.value
  assert(codePoint <= UInt32(UInt16.max))
  return UTF16.CodeUnit(codePoint)
}

