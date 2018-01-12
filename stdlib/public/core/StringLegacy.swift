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

extension _StringVariant {
  @_versioned
  func _repeated(_ count: Int) -> _SwiftStringStorage<CodeUnit> {
    _sanityCheck(count > 0)
    let c = self.count
    let storage = _copyToNativeStorage(
      of: CodeUnit.self,
      unusedCapacity: (count - 1) * c)
    var p = storage.start + c
    for _ in 1 ..< count {
      p.initialize(from: storage.start, count: c)
      p += c
    }
    _sanityCheck(p == storage.start + count * c)
    storage.count = p - storage.start
    return storage
  }
}

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
  @_inlineable // FIXME(sil-serialize-all)
  public init(repeating repeatedValue: String, count: Int) {
    if count == 0 {
      self = ""
    } else if count == 1 {
      self = repeatedValue
    } else {
      precondition(count > 0, "Negative count not allowed")
      defer { _fixLifetime(repeatedValue) }
      if _slowPath(repeatedValue._guts._isOpaque) {
        let opaque = repeatedValue._guts._asOpaque()
        self.init(_StringGuts(opaque._repeated(count)))
      } else if repeatedValue._guts.isASCII {
        let ascii = repeatedValue._guts._unmanagedASCIIView
        self.init(_StringGuts(ascii._repeated(count)))
      } else {
        let utf16 = repeatedValue._guts._unmanagedUTF16View
        self.init(_StringGuts(utf16._repeated(count)))
      }
    }
  }

  /// A Boolean value indicating whether a string has no characters.
  @_inlineable // FIXME(sil-serialize-all)
  public var isEmpty: Bool {
    return _guts.count == 0
  }
}

extension String {
  @_inlineable // FIXME(sil-serialize-all)
  public init(_ _c: Unicode.Scalar) {
    self = String._fromWellFormedCodeUnitSequence(
      UTF32.self,
      input: repeatElement(_c.value, count: 1))
  }
}

#if _runtime(_ObjC)
/// Determines if `theString` starts with `prefix` comparing the strings under
/// canonical equivalence.
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringHasPrefixNFD")
internal func _stdlib_NSStringHasPrefixNFD(
  _ theString: AnyObject, _ prefix: AnyObject) -> Bool

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringHasPrefixNFDPointer")
internal func _stdlib_NSStringHasPrefixNFDPointer(
  _ theString: OpaquePointer, _ prefix: OpaquePointer) -> Bool

/// Determines if `theString` ends with `suffix` comparing the strings under
/// canonical equivalence.
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringHasSuffixNFD")
internal func _stdlib_NSStringHasSuffixNFD(
  _ theString: AnyObject, _ suffix: AnyObject) -> Bool
@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_NSStringHasSuffixNFDPointer")
internal func _stdlib_NSStringHasSuffixNFDPointer(
  _ theString: OpaquePointer, _ suffix: OpaquePointer) -> Bool

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
  /// - Returns: `true` if the string begins with `prefix`; otherwise, `false`.
  @_inlineable // FIXME(sil-serialize-all)
  public func hasPrefix(_ prefix: String) -> Bool {
    let prefixCount = prefix._guts.count
    if prefixCount == 0 {
      return true
    }
    if _fastPath(!self._guts._isOpaque && !prefix._guts._isOpaque) {
      let result: Bool
      if self._guts.isASCII && prefix._guts.isASCII {
        let selfASCII = self._guts._unmanagedASCIIView
        let prefixASCII = prefix._guts._unmanagedASCIIView
        if prefixASCII.count > selfASCII.count {
          // Prefix is longer than self.
          result = false
        } else {
          result = (0 as CInt) == _stdlib_memcmp(
            selfASCII.rawStart,
            prefixASCII.rawStart,
            prefixASCII.count)
        }
      } else {
        let lhsStr = _NSContiguousString(_unmanaged: self._guts)
        let rhsStr = _NSContiguousString(_unmanaged: prefix._guts)
        result = lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
          return _stdlib_NSStringHasPrefixNFDPointer($0, $1)
        }
      }
      _fixLifetime(self)
      _fixLifetime(prefix)
      return result
    }
    return _stdlib_NSStringHasPrefixNFD(
      self._bridgeToObjectiveCImpl(),
      prefix._bridgeToObjectiveCImpl())
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
  /// - Returns: `true` if the string ends with `suffix`; otherwise, `false`.
  @_inlineable // FIXME(sil-serialize-all)
  public func hasSuffix(_ suffix: String) -> Bool {
    let suffixCount = suffix._guts.count
    if suffixCount == 0 {
      return true
    }
    if _fastPath(!self._guts._isOpaque && !suffix._guts._isOpaque) {
      let result: Bool
      if self._guts.isASCII && suffix._guts.isASCII {
        let selfASCII = self._guts._unmanagedASCIIView
        let suffixASCII = suffix._guts._unmanagedASCIIView
        if suffixASCII.count > self._guts.count {
          // Suffix is longer than self.
          result = false
        } else {
          result = (0 as CInt) == _stdlib_memcmp(
            selfASCII.rawStart + (selfASCII.count - suffixASCII.count),
            suffixASCII.rawStart,
            suffixASCII.count)
        }
      } else {
        let lhsStr = _NSContiguousString(_unmanaged: self._guts)
        let rhsStr = _NSContiguousString(_unmanaged: suffix._guts)
        result = lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
          return _stdlib_NSStringHasSuffixNFDPointer($0, $1)
        }
      }
      _fixLifetime(self)
      _fixLifetime(suffix)
      return result
    }
    return _stdlib_NSStringHasSuffixNFD(
      self._bridgeToObjectiveCImpl(),
      suffix._bridgeToObjectiveCImpl())
  }
}
#else
// FIXME: Implement hasPrefix and hasSuffix without objc
// rdar://problem/18878343
#endif

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal func _ascii8(_ c: Unicode.Scalar) -> UInt8 {
  _sanityCheck(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UInt8(c.value)
}

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal func _digitASCII(
  _ digit: UInt8, numeralsOnly: Bool, uppercase: Bool
) -> UInt8 {
  if numeralsOnly || digit < 10 {
    return _ascii8("0") &+ digit
  } else {
    let base = (uppercase ? _ascii8("A") : _ascii8("a")) &- 10
    return base &+ digit
  }
}

@_inlineable // FIXME(sil-serialize-all)
@_versioned // FIXME(sil-serialize-all)
internal func _integerToString<T : FixedWidthInteger>(
  _ value: T, radix: Int, uppercase: Bool
) -> String {
  if value == 0 {
    return "0"
  }

  // Bit shifting / masking is much faster than division when `radix`
  // is a power of two.
  let radixIsPowerOfTwo = radix.nonzeroBitCount == 1
  let radix = T.Magnitude(radix)
  let quotientAndRemainder: (T.Magnitude) -> (T.Magnitude, T.Magnitude) =
    radixIsPowerOfTwo
      ? { ( $0 &>> radix.trailingZeroBitCount, $0 & (radix - 1) ) }
      : { $0.quotientAndRemainder(dividingBy: radix) }

  let isNegative = T.isSigned && value < 0
  var value = value.magnitude

  var result: [UInt8] = []
  while value != 0 {
    let (q, r) = quotientAndRemainder(value)
    result.append(
      _digitASCII(
        UInt8(truncatingIfNeeded: r),
        numeralsOnly: radix <= 10,
        uppercase: uppercase))
    value = q
  }
  
  if isNegative {
    result.append(_ascii8("-"))
  }
  return String._fromWellFormedCodeUnitSequence(
    UTF8.self, input: result.reversed())
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
  @_inlineable // FIXME(sil-serialize-all)
  public init<T : FixedWidthInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    // FIXME(integers): support a more general BinaryInteger protocol
    _precondition(2...36 ~= radix, "Radix must be between 2 and 36")
    if T.bitWidth <= 64 {
      self = T.isSigned
        ? _int64ToString(
          Int64(value), radix: Int64(radix), uppercase: uppercase)
        : _uint64ToString(
          UInt64(value), radix: Int64(radix), uppercase: uppercase)
    } else {
      self = _integerToString(value, radix: radix, uppercase: uppercase)
    }
  }
  
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
  @_inlineable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4)
  public init<T : FixedWidthInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) where T : SignedInteger {
    // FIXME(integers): tiebreaker between T : FixedWidthInteger and other obsoleted
    _precondition(2...36 ~= radix, "Radix must be between 2 and 36")
    if T.bitWidth <= 64 {
      self = _int64ToString(
        Int64(value), radix: Int64(radix), uppercase: uppercase)
    } else {
      self = _integerToString(value, radix: radix, uppercase: uppercase)
    }
  }
  
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
  @_inlineable // FIXME(sil-serialize-all)
  public init<T : FixedWidthInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) where T : UnsignedInteger {
    // FIXME(integers): support a more general BinaryInteger protocol
    _precondition(2...36 ~= radix, "Radix must be between 2 and 36")
    if T.bitWidth <= 64 {
      self = _uint64ToString(
        UInt64(value), radix: Int64(radix), uppercase: uppercase)
    } else {
      self = _integerToString(value, radix: radix, uppercase: uppercase)
    }
  }
  
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
  @_inlineable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4, message: "Please use the version for FixedWidthInteger instead.")
  public init<T : SignedInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    _precondition(2...36 ~= radix, "Radix must be between 2 and 36")
    self = _int64ToString(
      Int64(value), radix: Int64(radix), uppercase: uppercase)
  }
  
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
  @_inlineable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4, message: "Please use the version for FixedWidthInteger instead.")
  public init<T : UnsignedInteger>(
    _ value: T, radix: Int = 10, uppercase: Bool = false
  ) {
    _precondition(2...36 ~= radix, "Radix must be between 2 and 36")
    self = _uint64ToString(
      UInt64(value), radix: Int64(radix), uppercase: uppercase)
  }
}
