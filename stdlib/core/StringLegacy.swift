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
  
  init<
    Encoding: UnicodeCodec, Input: Collection
  where Input.GeneratorType.Element == Encoding.CodeUnit
  >(
    _ _encoding: Encoding.Type, input: Input
  )
  {
    self = String(_StringBuffer(encoding: _encoding, input: input))
  }
  
  init(count sz: Int, repeatedValue c: Character) {
    let s = String(c)
    self = String(_StringBuffer(capacity: s.core.count * sz, 
                                initialSize: 0, 
                                elementWidth: s.core.elementWidth))
    for i in 0..sz {
      self += s
    }
  }

  init(count: Int, repeatedValue c: UnicodeScalar) {
    self = String(UTF32.self, 
                  input: Repeat(count: count, repeatedValue: c.value))
  }
  
  func byteLength() -> Int {
    return _encodedLength(UTF8.self)
  }

  var lines : String[] {
    return split("\n")
  }
  
  func split(separator: UnicodeScalar) -> String[] {
    var scalarSlices = Swift.split(unicodeScalars, { $0 == separator })
    return scalarSlices.map { $0 as String }
  }
  
  func size() -> Int {
    var count = 0
    for c in unicodeScalars {
      ++count
    }
    return count
  }

  var isEmpty : Bool {
    return core.count == 0
  }
}

extension String {
  var uppercaseString : String {
    let end = utf8.endIndex
    var resultArray = ContiguousArray<UTF8.CodeUnit>(
      count: countElements(utf8), repeatedValue: 0)
    
    var i = utf8.startIndex
    var j = 0
    while i != end {
      let u8 = utf8[i++]
      if u8 < 0x80 {
        if 97..123 ~= u8   {
          resultArray[j++] = u8 - 32
        } else {
          resultArray[j++] = u8
        }
      } else if u8 < 0xE0 {
        resultArray[j++] = u8
        let u8_1 = utf8[i++]
        if u8 == 0xC3 && 0xA0..0xBF ~= Int(u8_1) && u8_1 != 0xB7 {
          resultArray[j++] = u8_1 - 0x20
        } else {
          resultArray[j++] = u8_1
        }
      } else {
        resultArray[j++] = u8
        if u8 >= 0xF0 {
          resultArray[j++] = utf8[i++]
        }
        resultArray[j++] = utf8[i++]
        resultArray[j++] = utf8[i++]
      }
    }

    return String(UTF8.self, input: resultArray)
  }

  var lowercaseString : String {
    let end = utf8.endIndex
    var resultArray = ContiguousArray<UTF8.CodeUnit>(
      count: countElements(utf8), repeatedValue: 0)
    
    var i = utf8.startIndex
    var j = 0
    while i != end {
      let u8 = utf8[i++]
      if u8 < 0x80 {
        if 65..91 ~= u8   {
          resultArray[j++] = u8 + 32
        } else {
          resultArray[j++] = u8
        }
      } else if u8 < 0xE0 {
        resultArray[j++] = u8
        let u8_1 = utf8[i++]
        if u8 == 0xC3 && 0x80..0x9F ~= u8_1 && u8_1 != 0x97 {
          resultArray[j++] = u8_1 + 0x20
        } else {
          resultArray[j++] = u8_1
        }
      } else {
        resultArray[j++] = u8
        if u8 >= 0xF0 {
          resultArray[j++] = utf8[i++]
        }
        resultArray[j++] = utf8[i++]
        resultArray[j++] = utf8[i++]
      }
    }

    return String(UTF8.self, input: resultArray)
  }

  init(_ _c: UnicodeScalar) {
    self = String(count: 1, repeatedValue: _c)
  }

  func _isAll(predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in unicodeScalars { if !predicate(c) { return false } }

    return true
  }

  func hasPrefix(prefix: String) -> Bool {
    return Swift.startsWith(self, prefix)
  }
  
  func hasSuffix(suffix: String) -> Bool {
    return Swift.startsWith(reverse(self), reverse(suffix))
  }

  func _isAlpha() -> Bool { return _isAll({ $0.isAlpha() }) }
  func _isDigit() -> Bool { return _isAll({ $0.isDigit() }) }
  func _isSpace() -> Bool { return _isAll({ $0.isSpace() }) }
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
  var baseCharOrd: UInt32 = digit <= 9 ? _asUnicodeCodePoint("0")
                                       : _asUnicodeCodePoint(ten) - 10
  stream(UTF8.CodeUnit(baseCharOrd + digit))
}

func _formatSignedInteger(
  value: Int64,
  radix: UInt64,
  ten: UnicodeScalar = "a") (  stream: (UTF8.CodeUnit)->Void ) {
  
  if value == 0 {
    stream(UTF8.CodeUnit(_asUnicodeCodePoint("0")))
  }
  else {
    if (value < 0) {
      let minusCharacter: UnicodeScalar = "-"
      stream(UTF8.CodeUnit(_asUnicodeCodePoint("-")))
    }
    // Compute the absolute value without causing overflow when value
    // == Int64.min
    let absValue = value < 0 ? UInt64(~value) + 1 : UInt64(value)
    _formatPositiveInteger(absValue, radix, ten: ten)(stream: stream)
  }
}

// Conversions to string from other types.
extension String {

  init(_ v: Int64, radix: Int = 10, _uppercase: Bool = false) {
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

  // FIXME: This function assumes UTF16
  init(_ v: UInt64, radix: Int = 10, _uppercase: Bool = false) {
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

  init(_ v : Int8, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : Int16, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : Int32, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : Int, radix : Int = 10, _uppercase : Bool = false) {
    self = String(Int64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : UInt8, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : UInt16, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : UInt32, radix : Int = 10, _uppercase : Bool = false) {
    self = String(UInt64(v), radix: radix, _uppercase: _uppercase)
  }
  init(_ v : UInt, radix : Int = 10, _uppercase : Bool = false) {
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
  /// the corresponding integer.
  func toInt() -> Int? {
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
    } else if (firstC == "-") {
      ++start
      negativeFactor = 1
    }

    // Interpret the string as an integer.
    // Since Int.min has a larger absolute value, perform addition with
    // negative numbers; detect underflows before they happen. 
    var res : Int = 0
    for c in scalars[start..scalars.endIndex] {
      if !c.isDigit() {
        // Conversion failed if a non-digit is encountered.
        return .None
      }

      // Underflow occurs if res * 10 < Int.min.
      if res < Int.min / 10 {
        return .None
      }
      res = res * 10

      var d : Int = (c - "0")
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
    for i in 0..start {
      ++startIndex
    }
    return rng[startIndex..rng.endIndex]
  }

  /// Split the given string at the given delimiter character, returning 
  /// the strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  func _splitFirst(delim: UnicodeScalar)
    -> (before: String, after: String, wasFound : Bool)
  {
    var rng = unicodeScalars
    for i in indices(rng) {
      if rng[i] == delim {
        return (rng[rng.startIndex..i], rng[i.succ()..rng.endIndex], true)
      }
    }
    return (self, "", false)
  }

  /// Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character, and a boolean value
  /// indicating whether any character was found.
  func _splitFirstIf(pred: (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    var rng = unicodeScalars
    for i in indices(rng) {
      if pred(rng[i]) {
        return (rng[rng.startIndex..i], rng[i], rng[i.succ()..rng.endIndex], true)
      }
    }
    return (self, "🎃", String(), false)
  }

  /// Split the given string at each occurrence of a character for which
  /// the given predicate evaluates true, returning an array of strings that
  /// before/between/after those delimiters.
  func _splitIf(pred: (UnicodeScalar) -> Bool) -> String[] {
    var scalarSlices = Swift.split(unicodeScalars, pred)
    return scalarSlices.map { $0 as String }
  }
}

