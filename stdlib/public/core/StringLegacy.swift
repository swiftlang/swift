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
  /// For example, you can use this initializer to create a string with ten
  /// `"ab"` strings in a row.
  ///
  ///     let s = String(repeating: "ab", count: 10)
  ///     print(s)
  ///     // Prints "abababababababababab"
  ///
  /// - Parameters:
  ///   - repeatedValue: The string to repeat.
  ///   - count: The number of times to repeat `repeatedValue` in the resulting
  ///     string.
  public init(repeating repeatedValue: String, count: Int) {
    _precondition(count >= 0, "Negative count not allowed")
    guard count > 1 else {
      self = count == 0 ? "" : repeatedValue
      return
    }

    // TODO(String performance): We can directly call appendInPlace
    var result = String()
    result.reserveCapacity(repeatedValue._guts.count &* count)
    for _ in 0..<count {
      result += repeatedValue
    }
    self = result
  }

  /// A Boolean value indicating whether a string has no characters.
  @inlinable
  public var isEmpty: Bool {
    @inline(__always) get { return _guts.isEmpty }
  }
}

extension StringProtocol {
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
  /// The Unicode-safe comparison matches Unicode extended grapheme clusters
  /// rather than the code points used to compose them. The example below uses
  /// two strings with different forms of the `"é"` character---the first uses
  /// the composed form and the second uses the decomposed form.
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
  /// - Returns: `true` if the string begins with `prefix`; otherwise, `false`.
  @inlinable
  public func hasPrefix<Prefix: StringProtocol>(_ prefix: Prefix) -> Bool {
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
  /// The Unicode-safe comparison matches Unicode extended grapheme clusters
  /// rather than the code points used to compose them. The example below uses
  /// two strings with different forms of the `"é"` character---the first uses
  /// the composed form and the second uses the decomposed form.
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
  /// - Returns: `true` if the string ends with `suffix`; otherwise, `false`.
  @inlinable
  public func hasSuffix<Suffix: StringProtocol>(_ suffix: Suffix) -> Bool {
    return self.reversed().starts(with: suffix.reversed())
  }
}

extension String {
  public func hasPrefix(_ prefix: String) -> Bool {
    if _fastPath(self._guts.isNFCFastUTF8 && prefix._guts.isNFCFastUTF8) {
      guard prefix._guts.count <= self._guts.count else { return false }
      let isPrefix = unsafe prefix._guts.withFastUTF8 { nfcPrefix in
        let prefixEnd = nfcPrefix.count
        return unsafe self._guts.withFastUTF8(range: 0..<prefixEnd) { nfcSlicedSelf in
          return unsafe _binaryCompare(nfcSlicedSelf, nfcPrefix) == 0
        }
      }
      let endIndex = Index(_encodedOffset: prefix._guts.count)
      // In addition to a byte comparison check, we also need to check that
      // the prefix ends on a grapheme cluster boundary of the String
      return isPrefix && self._guts.isOnGraphemeClusterBoundary(endIndex)
    }

    return starts(with: prefix)
  }

  public func hasSuffix(_ suffix: String) -> Bool {
    if _fastPath(self._guts.isNFCFastUTF8 && suffix._guts.isNFCFastUTF8) {
      let suffixStart = self._guts.count - suffix._guts.count
      guard suffixStart >= 0 else { return false }
      let isSuffix = unsafe suffix._guts.withFastUTF8 { nfcSuffix in
        return unsafe self._guts.withFastUTF8(range: suffixStart..<self._guts.count) {
          nfcSlicedSelf in return unsafe _binaryCompare(nfcSlicedSelf, nfcSuffix) == 0
        }
      }
      let startIndex = Index(_encodedOffset: suffixStart)
      // In addition to a byte comparison check, we also need to check that
      // the suffix starts on a grapheme cluster boundary of the String
      return isSuffix && self._guts.isOnGraphemeClusterBoundary(startIndex)
    }

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
  ///     print("\(max) has \(max.count) digits.")
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
  public init<T: BinaryInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    self = value._description(radix: radix, uppercase: uppercase)
  }
}
