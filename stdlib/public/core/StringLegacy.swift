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

import SwiftShims

extension String {
  /// Creates a new string representing the given string repeated the specified
  /// number of times.
  ///
  /// For example, use this initializer to create a string with ten `"00"`
  /// strings in a row.
  ///
  ///     let zeroes = String(repeating: "00", count: 10)
  ///     print(zeroes)
  ///     // Prints "00000000000000000000"
  ///
  /// - Parameters:
  ///   - repeatedValue: The string to repeat.
  ///   - count: The number of times to repeat `repeatedValue` in the resulting
  ///     string.
  public init(repeating repeatedValue: String, count: Int) {
    if count == 0 {
      self = ""
      return
    }
    precondition(count > 0, "Negative count not allowed")
    let s = repeatedValue
    self = String(_storage: _StringBuffer(
        capacity: s._core.count * count,
        initialSize: 0,
        elementWidth: s._core.elementWidth))
    for _ in 0..<count {
      self += s
    }
  }

  public var _lines : [String] {
    return _split(separator: "\n")
  }
  
  public func _split(separator: UnicodeScalar) -> [String] {
    let scalarSlices = unicodeScalars.split { $0 == separator }
    return scalarSlices.map { String($0) }
  }

  /// A Boolean value indicating whether a string has no characters.
  public var isEmpty: Bool {
    return _core.count == 0
  }
}

extension String {
  public init(_ _c: UnicodeScalar) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF32.self,
      input: repeatElement(_c.value, count: 1))
  }
}

extension Collection {
  public func _mismatch<Other: Collection>(
    _ other: Other
  ) -> (Index, Other.Index)
  where Iterator.Element == Other.Iterator.Element,
    Iterator.Element : Equatable {
    var il = startIndex, ir = other.startIndex
    let el = endIndex, er = other.endIndex
    while il != el && ir != er && self[il] == other[ir] {
      self.formIndex(after: &il)
      other.formIndex(after: &ir)
    }
    return (il, ir)
  }
}

extension String {
  /// Returns a Boolean value indicating whether the string begins with the
  /// specified prefix.
  ///
  /// The comparison is both case sensitive and Unicode safe. The
  /// case-sensitive comparison will only match strings whose corresponding
  /// characters have the same case.
  ///
  ///     let cafe = "Café du Monde"
  ///
  ///     // Case sensitive
  ///     print(cafe.hasPrefix("café"))
  ///     // Prints "false"
  ///
  /// The Unicode-safe comparison matches Unicode scalar values rather than the
  /// code points used to compose them. The example below uses two strings
  /// with different forms of the `"é"` character---the first uses the composed
  /// form and the second uses the decomposed form.
  ///
  ///     // Unicode safe
  ///     let composedCafe = "Café"
  ///     let decomposedCafe = "Cafe\u{0301}"
  ///
  ///     print(cafe.hasPrefix(composedCafe))
  ///     // Prints "true"
  ///     print(cafe.hasPrefix(decomposedCafe))
  ///     // Prints "true"
  ///
  /// - Parameter prefix: A possible prefix to test against this string.
  /// - Returns: `true` if the string begins with `prefix`, otherwise, `false`.
  public func hasPrefix(_ prefix: String) -> Bool {
    let l = self.content.fccNormalizedUTF16
    let r = prefix.content.fccNormalizedUTF16
    let (ml, mr) = l._mismatch(r)
    if mr != r.endIndex { return false }
    if ml == l.endIndex { return true }
    // verify that ml is on a Character boundary.
    // FIXME: this is a really inefficient way to check!
    return self.starts(with: prefix)
  }

  /// Returns a Boolean value indicating whether the string ends with the
  /// specified suffix.
  ///
  /// The comparison is both case sensitive and Unicode safe. The
  /// case-sensitive comparison will only match strings whose corresponding
  /// characters have the same case.
  ///
  ///     let plans = "Let's meet at the café"
  ///
  ///     // Case sensitive
  ///     print(plans.hasSuffix("Café"))
  ///     // Prints "false"
  ///
  /// The Unicode-safe comparison matches Unicode scalar values rather than the
  /// code points used to compose them. The example below uses two strings
  /// with different forms of the `"é"` character---the first uses the composed
  /// form and the second uses the decomposed form.
  ///
  ///     // Unicode safe
  ///     let composedCafe = "café"
  ///     let decomposedCafe = "cafe\u{0301}"
  ///
  ///     print(plans.hasSuffix(composedCafe))
  ///     // Prints "true"
  ///     print(plans.hasSuffix(decomposedCafe))
  ///     // Prints "true"
  ///
  /// - Parameter suffix: A possible suffix to test against this string.
  /// - Returns: `true` if the string ends with `suffix`, otherwise, `false`.
  public func hasSuffix(_ suffix: String) -> Bool {
    let l = self.content.fccNormalizedUTF16.reversed()
    let r = suffix.content.fccNormalizedUTF16.reversed()
    let (ml, mr) = l._mismatch(r)
    if mr != r.endIndex { return false }
    if ml == l.endIndex { return true }
    // verify that ml is on a Character boundary.
    // FIXME: this is a really inefficient way to check!
    return self.reversed().starts(with: suffix.reversed())
  }
}

// Conversions to string from other types.
extension String {
  /// Creates a string representing the given value in base 10, or some other
  /// specified base.
  ///
  /// The following example converts the maximal `Int` value to a string and
  /// prints its length:
  ///
  ///     let max = String(Int.max)
  ///     print("\(max) has \(max.utf16.count) digits.")
  ///     // Prints "9223372036854775807 has 19 digits."
  ///
  /// Numerals greater than 9 are represented as Roman letters. These letters
  /// start with `"A"` if `uppercase` is `true`; otherwise, with `"a"`.
  /// 
  ///     let v = 999_999
  ///     print(String(v, radix: 2))
  ///     // Prints "11110100001000111111"
  ///
  ///     print(String(v, radix: 16))
  ///     // Prints "f423f"
  ///     print(String(v, radix: 16, uppercase: true))
  ///     // Prints "F423F"
  ///
  /// - Parameters:
  ///   - value: The value to convert to a string.
  ///   - radix: The base to use for the string representation. `radix` must be
  ///     at least 2 and at most 36. The default is 10.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  public init<T : _SignedInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    _precondition(radix > 1, "Radix must be greater than 1")
    self = _int64ToString(
      value.toIntMax(), radix: Int64(radix), uppercase: uppercase)
  }
  
  /// Creates a string representing the given value in base 10, or some other
  /// specified base.
  ///
  /// The following example converts the maximal `Int` value to a string and
  /// prints its length:
  ///
  ///     let max = String(Int.max)
  ///     print("\(max) has \(max.utf16.count) digits.")
  ///     // Prints "9223372036854775807 has 19 digits."
  ///
  /// Numerals greater than 9 are represented as Roman letters. These letters
  /// start with `"A"` if `uppercase` is `true`; otherwise, with `"a"`.
  ///
  ///     let v: UInt = 999_999
  ///     print(String(v, radix: 2))
  ///     // Prints "11110100001000111111"
  ///
  ///     print(String(v, radix: 16))
  ///     // Prints "f423f"
  ///     print(String(v, radix: 16, uppercase: true))
  ///     // Prints "F423F"
  ///
  /// - Parameters:
  ///   - value: The value to convert to a string.
  ///   - radix: The base to use for the string representation. `radix` must be
  ///     at least 2 and at most 36. The default is 10.
  ///   - uppercase: Pass `true` to use uppercase letters to represent numerals
  ///     greater than 9, or `false` to use lowercase letters. The default is
  ///     `false`.
  public init<T : UnsignedInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    _precondition(radix > 1, "Radix must be greater than 1")
    self = _uint64ToString(
      value.toUIntMax(), radix: Int64(radix), uppercase: uppercase)
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
  public func _splitFirstIf(_ predicate: (UnicodeScalar) -> Bool)
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

