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
@_inlineable // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollation")
public func _stdlib_compareNSStringDeterministicUnicodeCollation(
  _ lhs: AnyObject, _ rhs: AnyObject
) -> Int32

@_inlineable // FIXME(sil-serialize-all)
@_silgen_name("swift_stdlib_compareNSStringDeterministicUnicodeCollationPtr")
public func _stdlib_compareNSStringDeterministicUnicodeCollationPointer(
  _ lhs: OpaquePointer, _ rhs: OpaquePointer
) -> Int32
#endif

#if _runtime(_ObjC)
extension _UnmanagedString where CodeUnit == UInt8 {
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
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned
  internal func compareASCII(to other: _UnmanagedString<UInt8>) -> Int {
    // FIXME Results should be the same across all platforms.
    if self.start == other.start {
      return (self.count &- other.count).signum()
    }
    var cmp = Int(truncatingIfNeeded:
      _stdlib_memcmp(
        self.rawStart, other.rawStart,
        Swift.min(self.count, other.count)))
    if cmp == 0 {
      cmp = self.count &- other.count
    }
    return cmp.signum()
  }
}
#endif

extension _StringGuts {

  //
  // HACK HACK HACK: Work around for ARC :-(
  //
  @inline(never)
  @effects(readonly)
  public
  static func _compareDeterministicUnicodeCollation(
    _leftUnsafeStringGutsBitPattern leftBits: (UInt, UInt),
    _rightUnsafeStringGutsBitPattern rightBits: (UInt, UInt)
  ) -> Int {
    let left = _StringGuts(
      object: _StringObject(rawBits: leftBits.0),
      otherBits: leftBits.1)
    let right = _StringGuts(
      object: _StringObject(rawBits: rightBits.0),
      otherBits: rightBits.1)
    return _compareDeterministicUnicodeCollation(
      left, 0..<left.count, to: right, 0..<right.count)
  }
  @inline(never)
  @effects(readonly)
  public
  static func _compareDeterministicUnicodeCollation(
    _leftUnsafeStringGutsBitPattern leftBits: (UInt, UInt),
    _ leftRange: Range<Int>,
    _rightUnsafeStringGutsBitPattern rightBits: (UInt, UInt),
    _ rightRange: Range<Int>
  ) -> Int {
    let left = _StringGuts(
      object: _StringObject(rawBits: leftBits.0),
      otherBits: leftBits.1)
    let right = _StringGuts(
      object: _StringObject(rawBits: rightBits.0),
      otherBits: rightBits.1)
    return _compareDeterministicUnicodeCollation(
      left, leftRange, to: right, rightRange)
  }

  /// Compares two slices of strings with the Unicode Collation Algorithm.
  @inline(never) // Hide the CF/ICU dependency
  @effects(readonly)
  public  // @testable
  static func _compareDeterministicUnicodeCollation(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    to right: _StringGuts, _ rightRange: Range<Int>) -> Int {
    // Note: this operation should be consistent with equality comparison of
    // Character.
#if _runtime(_ObjC)
    if _fastPath(left._isContiguous && right._isContiguous) {
      let l = _NSContiguousString(_unmanaged: left, range: leftRange)
      let r = _NSContiguousString(_unmanaged: right, range: rightRange)
      return l._unsafeWithNotEscapedSelfPointerPair(r) {
        return Int(
          _stdlib_compareNSStringDeterministicUnicodeCollationPointer($0, $1))
      }
    } else {
      let l = left._ephemeralCocoaString(leftRange)
      let r = right._ephemeralCocoaString(rightRange)
      return Int(_stdlib_compareNSStringDeterministicUnicodeCollation(l, r))
    }
#else
    switch (_guts.isASCII, rhs._guts.isASCII) {
    case (true, false):
      let l = left._unmanagedASCIIView[leftRange]
      let r = right._unmanagedUTF16View[rightRange]
      return Int(_swift_stdlib_unicode_compare_utf8_utf16(
          l.start, Int32(l.count),
          r.start, Int32(r.count)))
    case (false, true):
      // Just invert it and recurse for this case.
      return -rhs._compareDeterministicUnicodeCollation(self)
    case (false, false):
      let l = left._unmanagedUTF16View[leftRange]
      let r = right._unmanagedUTF16View[rightRange]
      return Int(_swift_stdlib_unicode_compare_utf16_utf16(
          l.start, Int32(l.count),
          r.start, Int32(r.count)))
    case (true, true):
      let l = left._unmanagedASCIIView[leftRange]
      let r = right._unmanagedASCIIView[rightRange]
      return Int(_swift_stdlib_unicode_compare_utf8_utf8(
          l.start, Int32(l.count),
          r.start, Int32(r.count)))
    }
#endif
  }
}

extension _StringGuts {
  @inline(__always)
  @_inlineable
  public func _bitwiseEqualTo(_ other: _StringGuts) -> Bool {
    return self._object.rawBits == other._object.rawBits
      && self._otherBits == other._otherBits
  }

  @_inlineable
  @_versioned
  internal static func isEqual(
    _ left: _StringGuts, to right: _StringGuts
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) {
      return true
    }
    return compare(left, to: right) == 0
  }

  @_inlineable
  @_versioned
  internal static func isEqual(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    to right: _StringGuts, _ rightRange: Range<Int>
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) && leftRange == rightRange {
      return true
    }
    return compare(left, leftRange, to: right, rightRange) == 0
  }

  @_inlineable
  @_versioned
  internal static func isLess(
    _ left: _StringGuts, than right: _StringGuts
  ) -> Bool {
    return compare(left, to: right) == -1
  }

  @_inlineable
  @_versioned
  internal static func isLess(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    than right: _StringGuts, _ rightRange: Range<Int>
  ) -> Bool {
    return compare(left, leftRange, to: right, rightRange) == -1
  }

  @_inlineable
  @_versioned
  internal static func compare(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    to right: _StringGuts, _ rightRange: Range<Int>
  ) -> Int {
    defer { _fixLifetime(left) }
    defer { _fixLifetime(right) }
#if _runtime(_ObjC)
    // We only want to perform this optimization on objc runtimes. Elsewhere,
    // we will make it follow the unicode collation algorithm even for ASCII.
    // This is consistent with Foundation, but incorrect as defined by Unicode.
    //
    // FIXME: String ordering should be consistent across all platforms.
    if left.isASCII && right.isASCII {
      let leftASCII = left._unmanagedASCIIView[leftRange]
      let rightASCII = right._unmanagedASCIIView[rightRange]
      let result = leftASCII.compareASCII(to: rightASCII)
      return result
    }
#endif
    let leftBits = (left._object.rawBits, left._otherBits)
    let rightBits = (right._object.rawBits, right._otherBits)
    return _compareDeterministicUnicodeCollation(
      _leftUnsafeStringGutsBitPattern: leftBits, leftRange,
      _rightUnsafeStringGutsBitPattern: rightBits, rightRange)
  }

  @_inlineable
  @_versioned
  internal static func compare(
    _ left: _StringGuts, to right: _StringGuts
  ) -> Int {
    defer { _fixLifetime(left) }
    defer { _fixLifetime(right) }
#if _runtime(_ObjC)
    // We only want to perform this optimization on objc runtimes. Elsewhere,
    // we will make it follow the unicode collation algorithm even for ASCII.
    // This is consistent with Foundation, but incorrect as defined by Unicode.
    //
    // FIXME: String ordering should be consistent across all platforms.
    if left.isASCII && right.isASCII {
      let leftASCII = left._unmanagedASCIIView
      let rightASCII = right._unmanagedASCIIView
      let result = leftASCII.compareASCII(to: rightASCII)
      return result
    }
#endif
    let leftBits = (left._object.rawBits, left._otherBits)
    let rightBits = (right._object.rawBits, right._otherBits)
    return _compareDeterministicUnicodeCollation(
      _leftUnsafeStringGutsBitPattern: leftBits,
      _rightUnsafeStringGutsBitPattern: rightBits)
  }
}

extension StringProtocol {
  @_inlineable // FIXME(sil-serialize-all)
  public static func ==<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    return _StringGuts.isEqual(
      lhs._wholeString._guts, lhs._encodedOffsetRange,
      to: rhs._wholeString._guts, rhs._encodedOffsetRange)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func !=<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    return !(lhs == rhs)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func < <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return _StringGuts.isLess(
      lhs._wholeString._guts, lhs._encodedOffsetRange,
      than: rhs._wholeString._guts, rhs._encodedOffsetRange)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func > <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return rhs < lhs
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func <= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(rhs < lhs)
  }

  @_inlineable // FIXME(sil-serialize-all)
  public static func >= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(lhs < rhs)
  }
}

extension String : Equatable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @_inlineable // FIXME(sil-serialize-all)
  public static func ==(lhs: String, rhs: String) -> Bool {
    return _StringGuts.isEqual(lhs._guts, to: rhs._guts)
  }
}

extension String : Comparable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @_inlineable // FIXME(sil-serialize-all)
  public static func < (lhs: String, rhs: String) -> Bool {
    return _StringGuts.isLess(lhs._guts, than: rhs._guts)
  }
}

extension Substring : Equatable {}
