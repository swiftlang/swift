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


// Interfaces with a questionable future that are needed in order to
// be a drop-in replacement for String
//
extension String {
  public init(count sz: Int, repeatedValue c: Character) {
    let s = String(c)
    self = String(_StringBuffer(capacity: s.core.count * sz, 
                                initialSize: 0, 
                                elementWidth: s.core.elementWidth))
    for i in 0..<sz {
      self += s
    }
  }

  public init(count: Int, repeatedValue c: UnicodeScalar) {
    self = String._fromWellFormedCodeUnitSequence(UTF32.self,
        input: Repeat(count: count, repeatedValue: c.value))
  }
  
  public var _lines : [String] {
    return _split("\n")
  }
  
  public func _split(separator: UnicodeScalar) -> [String] {
    var scalarSlices = Swift.split(unicodeScalars, { $0 == separator })
    return scalarSlices.map { String($0) }
  }
  
  public var isEmpty : Bool {
    return core.count == 0
  }
}

extension String {
  public init(_ _c: UnicodeScalar) {
    self = String(count: 1, repeatedValue: _c)
  }

  func _isAll(predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in unicodeScalars { if !predicate(c) { return false } }

    return true
  }

  public func hasPrefix(prefix: String) -> Bool {
    return Swift.startsWith(self, prefix)
  }
  
  public func hasSuffix(suffix: String) -> Bool {
    return Swift.startsWith(lazy(self).reverse(), lazy(suffix).reverse())
  }

  func _isAlpha() -> Bool { return _isAll({ $0._isAlpha() }) }
  func _isDigit() -> Bool { return _isAll({ $0._isDigit() }) }
  func _isSpace() -> Bool { return _isAll({ $0._isSpace() }) }
}

/// Represent a positive integer value in the given radix,
/// writing each ASCII character into stream.  The value of `ten'
/// should be either "A" or "a", depending on whether you want upper-
/// or lower-case letters when radix > 10
func _formatPositiveInteger( 
  value: UInt64,
  radix: UInt64,
  ten: UnicodeScalar = "a") (  stream: (UTF8.CodeUnit)->Void )
{

  if value == 0 {
    return
  }

  _formatPositiveInteger(value / radix, radix, ten: ten)(stream: stream)
  var digit = UInt32(value % radix)
  var baseCharOrd: UInt32 =
      digit <= 9
          ? _asUnicodeScalar("0").value
          : _asUnicodeScalar(ten).value - 10
  stream(UTF8.CodeUnit(baseCharOrd + digit))
}

func _formatSignedInteger(
  value: Int64,
  radix: UInt64,
  ten: UnicodeScalar = "a") (  stream: (UTF8.CodeUnit)->Void ) {
  
  if value == 0 {
    stream(UTF8.CodeUnit(_asUnicodeScalar("0").value))
  }
  else {
    if (value < 0) {
      let minusCharacter: UnicodeScalar = "-"
      stream(UTF8.CodeUnit(_asUnicodeScalar("-").value))
    }
    // Compute the absolute value without causing overflow when value
    // == Int64.min
    let absValue = value < 0 ? UInt64(~value) + 1 : UInt64(value)
    _formatPositiveInteger(absValue, radix, ten: ten)(stream: stream)
  }
}

// Conversions to string from other types.
extension String {

  public init(_ v: Int64, radix: Int = 10, _uppercase: Bool = false) {
    var format = _formatSignedInteger(v, UInt64(radix), 
                                      ten: _uppercase ? "A" : "a")
    var asciiCount = 0
    format(stream: { _ in ++asciiCount;() })
    var buffer = _StringBuffer(
      capacity: asciiCount, initialSize: asciiCount, elementWidth: 1)
    var p = UnsafePointer<UTF8.CodeUnit>(buffer.start)
    format(stream: { p++.memory = $0 })
    self = String(buffer)
  }

  // FIXME: This function assumes UTF-16
  public init(_ v: UInt64, radix: Int = 10, _uppercase: Bool = false) {
    var format = _formatPositiveInteger(v, UInt64(radix), 
                                        ten: _uppercase ? "A" : "a")
    var asciiCount = v == 0 ? 1 : 0
    format(stream: { _ in ++asciiCount;() })
    var buffer = _StringBuffer(
      capacity: asciiCount, initialSize: asciiCount, elementWidth: 1)
    var p = UnsafePointer<UTF8.CodeUnit>(buffer.start)
    format(stream: { p++.memory = $0 })
    if v == 0 {
      p++.memory = UTF8.CodeUnit("0")
    }
    self = String(buffer)
  }

  public init(_ v : Int8, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : Int16, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : Int32, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : Int, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : UInt8, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : UInt16, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : UInt32, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  public init(_ v : UInt, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }

  typealias _Double = Double
  typealias _Float = Float
  typealias _Bool = Bool
  
  init(_ v : _Double) {
    self = _doubleToString(v)
  }

  init(_ v : _Float) {
    self = String(Double(v))
  }

  init(_ b : _Bool) {
    if b {
      self = "true"
    } else {
      self = "false"
    }
  }
}

// Conversions from string to other types.
extension String {
  /// If the string represents an integer that fits into an Int, returns
  /// the corresponding integer.  This accepts strings that match the regular
  /// expression "[-+]?[0-9]+" only.
  public func toInt() -> Int? {
    var scalars = self.unicodeScalars

    var start = scalars.startIndex
    if start == scalars.endIndex {
      return .None
    }
    
    // Interpet '+' or '-' before the number.
    var negativeFactor = -1
    var firstC = scalars[start]
    if (firstC == "+") {
      ++start
      
      // Reject "+" alone.
      if start == scalars.endIndex {
        return .None
      }
    } else if (firstC == "-") {
      ++start
      negativeFactor = 1

      // Reject "-" alone.
      if start == scalars.endIndex {
        return .None
      }
    }

    // Interpret the string as an integer.
    // Since Int.min has a larger absolute value, perform addition with
    // negative numbers; detect underflows before they happen. 
    var res : Int = 0
    for c in scalars[start..<scalars.endIndex] {
      if !c._isASCIIDigit() {
        // Conversion failed if a non-digit is encountered.
        return .None
      }

      // Underflow occurs if res * 10 < Int.min.
      if res < Int.min / 10 {
        return .None
      }
      res = res * 10

      var d = Int(c.value - _asUnicodeScalar("0").value)
      // Underflow occurs if res - d < Int.min.
      if res < Int.min + d {
        return .None
      }
      res = res - d
    }

    // If res is Int.min and the result should be positive, the next
    // operation will overflow.
    if negativeFactor == -1 && res == Int.min {
      return .None
    }

    return .Some(res * negativeFactor)
  }
}

extension String {
  /// Produce a substring of the given string from the given character
  /// index to the end of the string.
  func _substr(start: Int) -> String {
    var rng = unicodeScalars
    var startIndex = rng.startIndex
    for i in 0..<start {
      ++startIndex
    }
    return String(rng[startIndex..<rng.endIndex])
  }

  /// Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  public func _splitFirst(delim: UnicodeScalar)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = unicodeScalars
    for i in indices(rng) {
      if rng[i] == delim {
        return (String(rng[rng.startIndex..<i]), 
                String(rng[i.successor()..<rng.endIndex]), 
                true)
      }
    }
    return (self, "", false)
  }

  /// Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  public func _splitFirstIf(predicate: (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    var rng = unicodeScalars
    for i in indices(rng) {
      if predicate(rng[i]) {
        return (String(rng[rng.startIndex..<i]),
                rng[i], 
                String(rng[i.successor()..<rng.endIndex]), 
                true)
      }
    }
    return (self, "🎃", String(), false)
  }

  /// Split the given string at each occurrence of a character for which
  /// the given predicate evaluates true, returning an array of strings that
  /// before/between/after those delimiters.
  func _splitIf(predicate: (UnicodeScalar) -> Bool) -> [String] {
    var scalarSlices = Swift.split(unicodeScalars, predicate)
    return scalarSlices.map { String($0) }
  }
}

