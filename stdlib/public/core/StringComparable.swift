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

#if _runtime(_ObjC)
/// Compare two strings using the Unicode collation algorithm in the
/// deterministic comparison mode. (The strings which are equivalent according
/// to their NFD form are considered equal. Strings which are equivalent
/// according to the plain Unicode collation algorithm are additionally ordered
/// based on their NFD.)
///
/// See Unicode Technical Standard #10.
///
/// The behavior is equivalent to `NSString.compare()` with default options.
///
/// - returns:
///   * an unspecified value less than zero if `lhs < rhs`,
///   * zero if `lhs == rhs`,
///   * an unspecified value greater than zero if `lhs > rhs`.
@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollation")
public func _stdlib_compareNSStringDeterministicUnicodeCollation(
  _ lhs: AnyObject, _ rhs: AnyObject
) -> Int32

@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollationPtr")
public func _stdlib_compareNSStringDeterministicUnicodeCollationPointer(
  _ lhs: OpaquePointer, _ rhs: OpaquePointer
) -> Int32
#endif

@_silgen_name("_swift_string_memcmp")
func _swift_string_memcmp(
  _ s1: UnsafeMutableRawPointer,
  _ s2: UnsafeMutableRawPointer,
  _ n: Int
) -> Int

extension String {
#if _runtime(_ObjC)
  /// This is consistent with Foundation, but incorrect as defined by Unicode.
  /// Unicode weights some ASCII punctuation in a different order than ASCII
  /// value. Such as:
  ///
  ///   0022  ; [*02FF.0020.0002] # QUOTATION MARK
  ///   0023  ; [*038B.0020.0002] # NUMBER SIGN
  ///   0025  ; [*038C.0020.0002] # PERCENT SIGN
  ///   0026  ; [*0389.0020.0002] # AMPERSAND
  ///   0027  ; [*02F8.0020.0002] # APOSTROPHE
  ///
  /// - Precondition: Both `self` and `rhs` are ASCII strings.
  public // @testable
  func _compareASCII(_ rhs: String) -> Int {
    var compare = Int(_swift_stdlib_memcmp(
      self._core.startASCII, rhs._core.startASCII,
      min(self._core.count, rhs._core.count)))
    if compare == 0 {
      compare = self._core.count - rhs._core.count
    }
    // This efficiently normalizes the result to -1, 0, or 1 to match the
    // behavior of NSString's compare function.
    return (compare > 0 ? 1 : 0) - (compare < 0 ? 1 : 0)
  }
#endif

  private
  func _compareCodeUnitsASCII(_ rhs: String) -> Int {
    let n = min(_core.count, rhs._core.count)
    let selfStart = UnsafeMutableRawPointer(_core.startASCII)
    let rhsStart = UnsafeMutableRawPointer(rhs._core.startASCII)
    let firstDiff = _swift_string_memcmp(selfStart, rhsStart, n)
    if _core.count == rhs._core.count && firstDiff == n {
      return 0
    }
    return _compareString(rhs, offset: firstDiff)
  }

  @inline(never)
  @_semantics("stdlib_binary_only") // Hide the ICU dependency
  private
  func _compareCodeUnitsUTF16(_ rhs: String) -> Int {
    let n = min(_core.count, rhs._core.count) << _core.elementShift
    let selfStart = UnsafeMutableRawPointer(_core.startUTF16)
    let rhsStart = UnsafeMutableRawPointer(rhs._core.startUTF16)
    var firstDiff = _swift_string_memcmp(selfStart, rhsStart, n)
    if _core.count == rhs._core.count && firstDiff == n {
      return 0
    }
    // At this point we have to fall back to the UCA.
    // In order to properly order contractions and surrogate pairs we can't
    // invoke the UCA with UTF16 strings that start in the middle of a contraction
    // or surrogate pair. Rather than carry out a lot of expensive operations to
    // figure out if we're in the middle of a contraction or surrogate pair, we
    // simply step back a fixed number of code units, equal to the longest
    // possible contraction, or the length of a surrogate pair (2), whichever is
    // greater, minus 1 (while taking care that we don't step back past the start
    // of the strings).
    // This will produce a correct result at the cost of re-comparing a few
    // characters that we know are equal, which is likely much cheaper than
    // calculating a more precise number of code units to step back.
    firstDiff = firstDiff >> _core.elementShift
    let surrogateLength = 2
    let stepBack = max(Int(_swift_stdlib_unicode_find_longest_contraction()), surrogateLength) - 1
    firstDiff = firstDiff >= stepBack ? firstDiff - stepBack : 0
    return _compareString(rhs, offset: firstDiff)
  }

  public  // @testable
  func _compareCodeUnits(_ rhs: String) -> Int {
    if _core.isASCII == rhs._core.isASCII &&
       _core.hasContiguousStorage && rhs._core.hasContiguousStorage {
         return _core.isASCII ? _compareCodeUnitsASCII(rhs) : _compareCodeUnitsUTF16(rhs)
    }
    return _compareString(rhs)
  }

  /// Compares two strings with the Unicode Collation Algorithm.
  public  // @testable
  func _compareDeterministicUnicodeCollation(_ rhs: String) -> Int {
    return self._compareDeterministicUnicodeCollation(rhs, offset: 0)
  }

  @inline(never)
  @_semantics("stdlib_binary_only") // Hide the CF/ICU dependency
  public
  func _compareDeterministicUnicodeCollation(_ rhs: String, offset: Int = 0) -> Int {
    // Note: this operation should be consistent with equality comparison of
    // Character.
#if _runtime(_ObjC)
    if self._core.hasContiguousStorage && rhs._core.hasContiguousStorage {
      let lhsStr = _NSContiguousString(self._core)
      let rhsStr = _NSContiguousString(rhs._core)
      let res = lhsStr._unsafeWithNotEscapedSelfPointerPair(rhsStr) {
        return Int(
            _stdlib_compareNSStringDeterministicUnicodeCollationPointer($0, $1))
      }
      return res
    }
    return Int(_stdlib_compareNSStringDeterministicUnicodeCollation(
      _bridgeToObjectiveCImpl(), rhs._bridgeToObjectiveCImpl()))
#else
    switch (_core.isASCII, rhs._core.isASCII) {
    case (true, false):
      return Int(_swift_stdlib_unicode_compare_utf8_utf16(
          _core.startASCII, Int32(_core.count),
          rhs._core.startUTF16, Int32(rhs._core.count)))
    case (false, true):
      // Just invert it and recurse for this case.
      return -rhs._compareDeterministicUnicodeCollation(self)
    case (false, false):
      return Int(_swift_stdlib_unicode_compare_utf16_utf16(
        _core.startUTF16 + offset, Int32(_core.count - offset),
        rhs._core.startUTF16 + offset, Int32(rhs._core.count - offset)))
    case (true, true):
      return Int(_swift_stdlib_unicode_compare_utf8_utf8(
        _core.startASCII + offset, Int32(_core.count - offset),
        rhs._core.startASCII + offset, Int32(rhs._core.count - offset)))
    }
#endif
  }

  public  // @testable
  func _compareString(_ rhs: String, offset: Int = 0) -> Int {
#if _runtime(_ObjC)
    // We only want to perform this optimization on objc runtimes. Elsewhere,
    // we will make it follow the unicode collation algorithm even for ASCII.
    // This is consistent with Foundation, but incorrect as defined by Unicode.
    if _core.isASCII && rhs._core.isASCII {
      return _compareASCII(rhs)
    }
#endif
    return _compareDeterministicUnicodeCollation(rhs, offset: offset)
  }
}

extension String : Equatable {
  public static func == (lhs: String, rhs: String) -> Bool {
#if _runtime(_ObjC)
    // We only want to perform this optimization on objc runtimes. Elsewhere,
    // we will make it follow the unicode collation algorithm even for ASCII.
    // This is consistent with Foundation, but incorrect as defined by Unicode.
    if lhs._core.isASCII && rhs._core.isASCII {
      if lhs._core.count != rhs._core.count {
        return false
      }
      return _swift_stdlib_memcmp(
        lhs._core.startASCII, rhs._core.startASCII,
        rhs._core.count) == 0
    }
    return lhs._compareString(rhs) == 0
#else
    return lhs._compareCodeUnits(rhs) == 0
#endif
  }
}

extension String : Comparable {
  public static func < (lhs: String, rhs: String) -> Bool {
    return lhs._compareCodeUnits(rhs) < 0
  }
}

