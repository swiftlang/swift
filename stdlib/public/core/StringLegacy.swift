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


extension String {
  /// Construct an instance that is the concatenation of `count` copies
  /// of `repeatedValue`.
  public init(repeating repeatedValue: Character, count: Int) {
    let s = String(repeatedValue)
    self = String(_storage: _StringBuffer(
        capacity: s._core.count * count,
        initialSize: 0,
        elementWidth: s._core.elementWidth))
    for _ in 0..<count {
      self += s
    }
  }

  /// Construct an instance that is the concatenation of `count` copies
  /// of `Character(repeatedValue)`.
  public init(repeating repeatedValue: UnicodeScalar, count: Int) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF32.self,
      input: repeatElement(repeatedValue.value, count: count))
  }
  
  public var _lines : [String] {
    return _split(separator: "\n")
  }
  
  @warn_unused_result
  public func _split(separator separator: UnicodeScalar) -> [String] {
    let scalarSlices = unicodeScalars.split { $0 == separator }
    return scalarSlices.map { String($0) }
  }

  /// `true` iff `self` contains no characters.
  public var isEmpty : Bool {
    return _core.count == 0
  }
}

extension String {
  public init(_ _c: UnicodeScalar) {
    self = String(repeating: _c, count: 1)
  }

  @warn_unused_result
  internal func _isAll(@noescape predicate: (UnicodeScalar) -> Bool) -> Bool {
    for c in unicodeScalars { if !predicate(c) { return false } }

    return true
  }

  internal var _isAlpha: Bool { return _isAll({ $0._isAlpha }) }

  internal var _isDigit: Bool { return _isAll({ $0._isDigit }) }

  internal var _isSpace: Bool { return _isAll({ $0._isSpace }) }
}

#if _runtime(_ObjC)
/// Determines if `theString` starts with `prefix` comparing the strings under
/// canonical equivalence.
@_silgen_name("swift_stdlib_NSStringHasPrefixNFD")
func _stdlib_NSStringHasPrefixNFD(theString: AnyObject, _ prefix: AnyObject) -> Bool

/// Determines if `theString` ends with `suffix` comparing the strings under
/// canonical equivalence.
@_silgen_name("swift_stdlib_NSStringHasSuffixNFD")
func _stdlib_NSStringHasSuffixNFD(theString: AnyObject, _ suffix: AnyObject) -> Bool

extension String {
  /// Returns `true` iff `self` begins with `prefix`.
  public func hasPrefix(prefix: String) -> Bool {
    return _stdlib_NSStringHasPrefixNFD(
      self._bridgeToObjectiveCImpl(), prefix._bridgeToObjectiveCImpl())
  }

  /// Returns `true` iff `self` ends with `suffix`.
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
  public init<T : _SignedInteger>(_ v: T) {
    self = _int64ToString(v.toIntMax())
  }
  
  /// Create an instance representing `v` in base 10.
  public init<T : UnsignedInteger>(_ v: T) {
    self = _uint64ToString(v.toUIntMax())
  }

  /// Create an instance representing `v` in the given `radix` (base).
  ///
  /// Numerals greater than 9 are represented as roman letters,
  /// starting with `a` if `uppercase` is `false` or `A` otherwise.
  public init<T : _SignedInteger>(
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
  public init<T : UnsignedInteger>(
    _ v: T, radix: Int, uppercase: Bool = false
  ) {
    _precondition(radix > 1, "Radix must be greater than 1")
    self = _uint64ToString(
      v.toUIntMax(), radix: Int64(radix), uppercase: uppercase)
  }
}

extension String {
  /// Produce a substring of the given string from the given character
  /// index to the end of the string.
  func _substr(from start: Int) -> String {
    let rng = unicodeScalars
    var startIndex = rng.startIndex
    for _ in 0..<start {
      startIndex._successorInPlace()
    }
    return String(rng[startIndex..<rng.endIndex])
  }

  /// Split the given string at the given delimiter character, returning the
  /// strings before and after that character (neither includes the character
  /// found) and a boolean value indicating whether the delimiter was found.
  public func _splitFirst(separator delim: UnicodeScalar)
    -> (before: String, after: String, wasFound : Bool)
  {
    let rng = unicodeScalars
    for i in rng.indices {
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
  /// character that matches, the string after that character,
  /// and a boolean value indicating whether any character was found.
  public func _splitFirstIf(@noescape predicate: (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    let rng = unicodeScalars
    for i in rng.indices {
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
    let scalarSlices = unicodeScalars.split(isSeparator: predicate)
    return scalarSlices.map { String($0) }
  }
}

extension String {
  @available(*, unavailable, message="Renamed to init(repeating:count:) and reordered parameters")
  public init(count: Int, repeatedValue c: Character) {
    fatalError("unavailable function can't be called")
  }

  @available(*, unavailable, message="Renamed to init(repeating:count:) and reordered parameters")
  public init(count: Int, repeatedValue c: UnicodeScalar) {
    fatalError("unavailable function can't be called")
  }
}
