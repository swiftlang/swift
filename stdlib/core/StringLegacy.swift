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


extension String {
  /// Construct an instance that is the concatenation of `count` copies
  /// of `repeatedValue`
  public init(count: Int, repeatedValue c: Character) {
    let s = String(c)
    self = String(_storage: _StringBuffer(
        capacity: s._core.count * count,
        initialSize: 0,
        elementWidth: s._core.elementWidth))
    for i in 0..<count {
      self += s
    }
  }

  /// Construct an instance that is the concatenation of `count` copies
  /// of `Character(repeatedValue)`
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

  /// `true` iff `self` contains no characters.
  public var isEmpty : Bool {
    return _core.count == 0
  }
}

extension String {
  public init(_ _c: UnicodeScalar) {
    self = String(count: 1, repeatedValue: _c)
  }

  func _isAll(@noescape predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in unicodeScalars { if !predicate(c) { return false } }

    return true
  }

  func _isAlpha() -> Bool { return _isAll({ $0._isAlpha() }) }
  func _isDigit() -> Bool { return _isAll({ $0._isDigit() }) }
  func _isSpace() -> Bool { return _isAll({ $0._isSpace() }) }
}

#if _runtime(_ObjC)
/// Determines if `theString` starts with `prefix` comparing the strings under
/// canonical equivalence.
@asmname("swift_stdlib_NSStringHasPrefixNFD")
func _stdlib_NSStringHasPrefixNFD(theString: AnyObject, prefix: AnyObject) -> Bool

/// Determines if `theString` ends with `suffix` comparing the strings under
/// canonical equivalence.
@asmname("swift_stdlib_NSStringHasSuffixNFD")
func _stdlib_NSStringHasSuffixNFD(theString: AnyObject, suffix: AnyObject) -> Bool

extension String {
  /// Return `true` iff `self` begins with `prefix`
  public func hasPrefix(prefix: String) -> Bool {
    return _stdlib_NSStringHasPrefixNFD(
      self._bridgeToObjectiveCImpl(), prefix._bridgeToObjectiveCImpl())
  }

  /// Return `true` iff `self` ends with `suffix`
  public func hasSuffix(suffix: String) -> Bool {
    return _stdlib_NSStringHasSuffixNFD(
      self._bridgeToObjectiveCImpl(), suffix._bridgeToObjectiveCImpl())
  }
}
#else
// FIXME: Implement hasPrefix and hasSuffix without objc
// rdar://problem/18878343
#endif

// Conversions to string from other types.
extension String {

  // FIXME: can't just use a default arg for radix below; instead we
  // need these single-arg overloads <rdar://problem/17775455>
  
  /// Create an instance representing `v` in base 10.
  public init<T: _SignedIntegerType>(_ v: T) {
    self = _int64ToString(v.toIntMax())
  }
  
  /// Create an instance representing `v` in base 10.
  public init<T: _UnsignedIntegerType>(_ v: T)  {
    self = _uint64ToString(v.toUIntMax())
  }

  /// Create an instance representing `v` in the given `radix` (base).
  ///
  /// Numerals greater than 9 are represented as roman letters,
  /// starting with `a` if `uppercase` is `false` or `A` otherwise.
  public init<T: _SignedIntegerType>(
    _ v: T, radix: Int, uppercase: Bool = false
  ) {
    _precondition(radix > 1, "Radix must be greater than 1")
    self = _int64ToString(
      v.toIntMax(), radix: Int64(radix), uppercase: uppercase)
  }
  
  /// Create an instance representing `v` in the given `radix` (base).
  ///
  /// Numerals greater than 9 are represented as roman letters,
  /// starting with `a` if `uppercase` is `false` or `A` otherwise.
  public init<T: _UnsignedIntegerType>(
    _ v: T, radix: Int, uppercase: Bool = false
  )  {
    _precondition(radix > 1, "Radix must be greater than 1")
    self = _uint64ToString(
      v.toUIntMax(), radix: Int64(radix), uppercase: uppercase)
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

      var d = Int(c.value - UnicodeScalar("0").value)
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
  public func _splitFirstIf(@noescape predicate: (UnicodeScalar) -> Bool)
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
