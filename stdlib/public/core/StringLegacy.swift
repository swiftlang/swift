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

import SwiftShims

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
  public func _split(separator: UnicodeScalar) -> [String] {
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
}

#if _runtime(_ObjC)
/// Determines if `theString` starts with `prefix` comparing the strings under
/// canonical equivalence.
@_silgen_name("swift_stdlib_NSStringHasPrefixNFD")
func _stdlib_NSStringHasPrefixNFD(_ theString: AnyObject, _ prefix: AnyObject) -> Bool

@_silgen_name("swift_stdlib_NSStringHasPrefixNFDPointer")
func _stdlib_NSStringHasPrefixNFDPointer(_ theString: OpaquePointer, _ prefix: OpaquePointer) -> Bool

/// Determines if `theString` ends with `suffix` comparing the strings under
/// canonical equivalence.
@_silgen_name("swift_stdlib_NSStringHasSuffixNFD")
func _stdlib_NSStringHasSuffixNFD(_ theString: AnyObject, _ suffix: AnyObject) -> Bool
@_silgen_name("swift_stdlib_NSStringHasSuffixNFDPointer")
func _stdlib_NSStringHasSuffixNFDPointer(_ theString: OpaquePointer, _ suffix: OpaquePointer) -> Bool

extension String {
  /// Returns `true` iff `self` begins with `prefix`.
  public func hasPrefix(_ prefix: String) -> Bool {
    let selfCore = self._core
    let prefixCore = prefix._core
    if selfCore.hasContiguousStorage && prefixCore.hasContiguousStorage {
      if selfCore.isASCII && prefixCore.isASCII {
        // Prefix longer than self.
        let prefixCount = prefixCore.count
        if prefixCount > selfCore.count || prefixCount == 0 {
          return false
        }
        return Int(_swift_stdlib_memcmp(
          selfCore.startASCII, prefixCore.startASCII, prefixCount)) == 0
      }
      let lhsStr = _NSContiguousString(selfCore)
      let rhsStr = _NSContiguousString(prefixCore)
      return lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
        return _stdlib_NSStringHasPrefixNFDPointer($0, $1)
      }
    }
    return _stdlib_NSStringHasPrefixNFD(
      self._bridgeToObjectiveCImpl(), prefix._bridgeToObjectiveCImpl())
  }

  /// Returns `true` iff `self` ends with `suffix`.
  public func hasSuffix(_ suffix: String) -> Bool {
    let selfCore = self._core
    let suffixCore = suffix._core
    if selfCore.hasContiguousStorage && suffixCore.hasContiguousStorage {
      if selfCore.isASCII && suffixCore.isASCII {
        // Prefix longer than self.
        let suffixCount = suffixCore.count
        let selfCount = selfCore.count
        if suffixCount > selfCount || suffixCount == 0 {
          return false
        }
        return Int(_swift_stdlib_memcmp(
                   selfCore.startASCII + (selfCount - suffixCount),
                   suffixCore.startASCII, suffixCount)) == 0
      }
      let lhsStr = _NSContiguousString(selfCore)
      let rhsStr = _NSContiguousString(suffixCore)
      return lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
        return _stdlib_NSStringHasSuffixNFDPointer($0, $1)
      }
    }
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
                String(rng[rng.index(after: i)..<rng.endIndex]),
                true)
      }
    }
    return (self, "", false)
  }

  /// Split the given string at the first character for which the given
  /// predicate returns true. Returns the string before that character, the 
  /// character that matches, the string after that character,
  /// and a boolean value indicating whether any character was found.
  public func _splitFirstIf(_ predicate: @noescape (UnicodeScalar) -> Bool)
    -> (before: String, found: UnicodeScalar, after: String, wasFound: Bool)
  {
    let rng = unicodeScalars
    for i in rng.indices {
      if predicate(rng[i]) {
        return (String(rng[rng.startIndex..<i]),
                rng[i], 
                String(rng[rng.index(after: i)..<rng.endIndex]),
                true)
      }
    }
    return (self, "🎃", String(), false)
  }
}

extension String {
  @available(*, unavailable, message: "Renamed to init(repeating:count:) and reordered parameters")
  public init(count: Int, repeatedValue c: Character) {
    Builtin.unreachable()
  }

  @available(*, unavailable, message: "Renamed to init(repeating:count:) and reordered parameters")
  public init(count: Int, repeatedValue c: UnicodeScalar) {
    Builtin.unreachable()
  }
}
