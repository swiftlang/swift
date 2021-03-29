//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// N.B.: This free function has been manually specialized below where
// `UTF8CodeUnits == UnsafeBufferPoint<UInt8>`. Ensure that any changes are
// made in sync.
@_alwaysEmitIntoClient
@inline(never)
internal func _parseASCIIDigits<
  UTF8CodeUnits: Collection, Result: FixedWidthInteger
>(
  _ codeUnits: UTF8CodeUnits, radix: Int, isNegative: Bool
) -> Result? where UTF8CodeUnits.Element == UInt8 {
  _internalInvariant(radix >= 2 && radix <= 36)
  guard _fastPath(!codeUnits.isEmpty) else { return nil }
  let multiplicand = Result(radix)
  var result = 0 as Result
  if radix <= 10 {
    let upperBound = 48 /* "0" */ &+ UInt8(radix)
    for digit in codeUnits {
      let digitValue: Result
      if _fastPath(digit >= 48 && digit < upperBound) {
        digitValue = Result(digit &- 48)
      } else {
        return nil
      }
      let (temporary, overflow1) =
        result.multipliedReportingOverflow(by: multiplicand)
      guard _fastPath(!overflow1) else { return nil }
      let (nextResult, overflow2) = isNegative
        ? temporary.subtractingReportingOverflow(digitValue)
        : temporary.addingReportingOverflow(digitValue)
      guard _fastPath(!overflow2) else { return nil }
      result = nextResult
    }
  } else {
    let uppercaseUpperBound = 65 /* "A" */ &+ UInt8(radix &- 10)
    let lowercaseUpperBound = 97 /* "a" */ &+ UInt8(radix &- 10)
    for digit in codeUnits {
      let digitValue: Result
      if _fastPath(digit >= 48 /* "0" */ && digit < 58) {
        digitValue = Result(digit &- 48)
      } else if _fastPath(digit >= 65 && digit < uppercaseUpperBound) {
        digitValue = Result(digit &- 65 &+ 10)
      } else if _fastPath(digit >= 97 && digit < lowercaseUpperBound) {
        digitValue = Result(digit &- 97 &+ 10)
      } else {
        return nil
      }
      let (temporary, overflow1) =
        result.multipliedReportingOverflow(by: multiplicand)
      guard _fastPath(!overflow1) else { return nil }
      let (nextResult, overflow2) = isNegative
        ? temporary.subtractingReportingOverflow(digitValue)
        : temporary.addingReportingOverflow(digitValue)
      guard _fastPath(!overflow2) else { return nil }
      result = nextResult
    }
  }
  return result
}

// N.B.: This free function is a manually specialized version of the function
// above. Ensure that any changes are made in sync.
@_alwaysEmitIntoClient
@inline(never)
internal func _parseASCIIDigits<Result: FixedWidthInteger>(
  _ codeUnits: UnsafeBufferPointer<UInt8>, radix: Int, isNegative: Bool
) -> Result? {
  _internalInvariant(radix >= 2 && radix <= 36)
  guard _fastPath(!codeUnits.isEmpty) else { return nil }
  let multiplicand = Result(radix)
  var result = 0 as Result
  if radix <= 10 {
    let upperBound = 48 /* "0" */ &+ UInt8(radix)
    for digit in codeUnits {
      let digitValue: Result
      if _fastPath(digit >= 48 && digit < upperBound) {
        digitValue = Result(digit &- 48)
      } else {
        return nil
      }
      let (temporary, overflow1) =
        result.multipliedReportingOverflow(by: multiplicand)
      guard _fastPath(!overflow1) else { return nil }
      let (nextResult, overflow2) = isNegative
        ? temporary.subtractingReportingOverflow(digitValue)
        : temporary.addingReportingOverflow(digitValue)
      guard _fastPath(!overflow2) else { return nil }
      result = nextResult
    }
  } else {
    let uppercaseUpperBound = 65 /* "A" */ &+ UInt8(radix &- 10)
    let lowercaseUpperBound = 97 /* "a" */ &+ UInt8(radix &- 10)
    for digit in codeUnits {
      let digitValue: Result
      if _fastPath(digit >= 48 /* "0" */ && digit < 58) {
        digitValue = Result(digit &- 48)
      } else if _fastPath(digit >= 65 && digit < uppercaseUpperBound) {
        digitValue = Result(digit &- 65 &+ 10)
      } else if _fastPath(digit >= 97 && digit < lowercaseUpperBound) {
        digitValue = Result(digit &- 97 &+ 10)
      } else {
        return nil
      }
      let (temporary, overflow1) =
        result.multipliedReportingOverflow(by: multiplicand)
      guard _fastPath(!overflow1) else { return nil }
      let (nextResult, overflow2) = isNegative
        ? temporary.subtractingReportingOverflow(digitValue)
        : temporary.addingReportingOverflow(digitValue)
      guard _fastPath(!overflow2) else { return nil }
      result = nextResult
    }
  }
  return result
}

@_alwaysEmitIntoClient
@inline(never)
internal func _parseASCII<UTF8CodeUnits: Collection, Result: FixedWidthInteger>(
  _ codeUnits: UTF8CodeUnits, radix: Int
) -> Result? where UTF8CodeUnits.Element == UInt8 {
  _internalInvariant(!codeUnits.isEmpty)
  let first = codeUnits.first!
  if first == 45 /* "-" */ {
    return _parseASCIIDigits(
      codeUnits.dropFirst(), radix: radix, isNegative: true)
  }
  if first == 43 /* "+" */ {
    return _parseASCIIDigits(
      codeUnits.dropFirst(), radix: radix, isNegative: false)
  }
  return _parseASCIIDigits(codeUnits, radix: radix, isNegative: false)
}

@_alwaysEmitIntoClient
@inline(never)
internal func _parseASCII<Result: FixedWidthInteger>(
  _ codeUnits: UnsafeBufferPointer<UInt8>, radix: Int
) -> Result? {
  _internalInvariant(!codeUnits.isEmpty)
  let first = codeUnits[0]
  if first == 45 /* "-" */ {
    return _parseASCIIDigits(
      UnsafeBufferPointer(rebasing: codeUnits[1...]),
      radix: radix, isNegative: true)
  }
  if first == 43 /* "+" */ {
    return _parseASCIIDigits(
      UnsafeBufferPointer(rebasing: codeUnits[1...]),
      radix: radix, isNegative: false)
  }
  return _parseASCIIDigits(codeUnits, radix: radix, isNegative: false)
}

extension FixedWidthInteger {
  /// Creates a new integer value from the given string and radix.
  ///
  /// The string passed as `text` may begin with a plus or minus sign character
  /// (`+` or `-`), followed by one or more numeric digits (`0-9`) or letters
  /// (`a-z` or `A-Z`). Parsing of the string is case insensitive.
  ///
  ///     let x = Int("123")
  ///     // x == 123
  ///
  ///     let y = Int("-123", radix: 8)
  ///     // y == -83
  ///     let y = Int("+123", radix: 8)
  ///     // y == +83
  ///
  ///     let z = Int("07b", radix: 16)
  ///     // z == 123
  ///
  /// If `text` is in an invalid format or contains characters that are out of
  /// bounds for the given `radix`, or if the value it denotes in the given
  /// `radix` is not representable, the result is `nil`. For example, the
  /// following conversions result in `nil`:
  ///
  ///     Int(" 100")                       // Includes whitespace
  ///     Int("21-50")                      // Invalid format
  ///     Int("ff6600")                     // Characters out of bounds
  ///     Int("zzzzzzzzzzzzz", radix: 36)   // Out of range
  ///
  /// - Parameters:
  ///   - text: The ASCII representation of a number in the radix passed as
  ///     `radix`.
  ///   - radix: The radix, or base, to use for converting `text` to an integer
  ///     value. `radix` must be in the range `2...36`. The default is 10.
  @inlinable
  @inline(__always)
  public init?<S: StringProtocol>(_ text: S, radix: Int = 10) {
    _precondition(2...36 ~= radix, "Radix not in range 2...36")
    guard _fastPath(!text.isEmpty) else { return nil }
    let result: Self? =
      text.utf8.withContiguousStorageIfAvailable {
        _parseASCII($0, radix: radix)
      } ?? _parseASCII(text.utf8, radix: radix)
    guard let result_ = result else { return nil }
    self = result_
  }

  /// Creates a new integer value from the given string.
  ///
  /// The string passed as `description` may begin with a plus or minus sign
  /// character (`+` or `-`), followed by one or more numeric digits (`0-9`).
  ///
  ///     let x = Int("123")
  ///     // x == 123
  ///
  /// If `description` is in an invalid format, or if the value it denotes in
  /// base 10 is not representable, the result is `nil`. For example, the
  /// following conversions result in `nil`:
  ///
  ///     Int(" 100")                       // Includes whitespace
  ///     Int("21-50")                      // Invalid format
  ///     Int("ff6600")                     // Characters out of bounds
  ///     Int("10000000000000000000000000") // Out of range
  ///
  /// - Parameter description: The ASCII representation of a number.
  @inlinable
  @inline(__always)
  public init?(_ description: String) {
    self.init(description, radix: 10)
  }
}

// -----------------------------------------------------------------------------
// Old entry points preserved for ABI compatibility.
// -----------------------------------------------------------------------------

/// Returns c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
@usableFromInline
internal func _ascii16(_ c: Unicode.Scalar) -> UTF16.CodeUnit {
  _internalInvariant(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

@usableFromInline
internal func _asciiDigit<CodeUnit: UnsignedInteger, Result: BinaryInteger>(
  codeUnit u_: CodeUnit, radix: Result
) -> Result? {
  let digit = _ascii16("0")..._ascii16("9")
  let lower = _ascii16("a")..._ascii16("z")
  let upper = _ascii16("A")..._ascii16("Z")

  let u = UInt16(truncatingIfNeeded: u_)
  let d: UInt16
  if _fastPath(digit ~= u) { d = u &- digit.lowerBound }
  else if _fastPath(upper ~= u) { d = u &- upper.lowerBound &+ 10 }
  else if _fastPath(lower ~= u) { d = u &- lower.lowerBound &+ 10 }
  else { return nil }
  guard _fastPath(d < radix) else { return nil }
  return Result(truncatingIfNeeded: d)
}

@usableFromInline
internal func _parseUnsignedASCII<
  Rest: IteratorProtocol, Result: FixedWidthInteger
>(
  first: Rest.Element, rest: inout Rest, radix: Result, positive: Bool
) -> Result?
where Rest.Element: UnsignedInteger {
  let r0 = _asciiDigit(codeUnit: first, radix: radix)
  guard _fastPath(r0 != nil), var result = r0 else { return nil }
  if !positive {
    let (result0, overflow0)
      = (0 as Result).subtractingReportingOverflow(result)
    guard _fastPath(!overflow0) else { return nil }
    result = result0
  }

  while let u = rest.next() {
    let d0 = _asciiDigit(codeUnit: u, radix: radix)
    guard _fastPath(d0 != nil), let d = d0 else { return nil }
    let (result1, overflow1) = result.multipliedReportingOverflow(by: radix)
    let (result2, overflow2) = positive
      ? result1.addingReportingOverflow(d)
      : result1.subtractingReportingOverflow(d)
    guard _fastPath(!overflow1 && !overflow2)
    else { return nil }
    result = result2
  }
  return result
}

//
// Before it was superseded, this function was about 20KB of always-inline code,
// most of which were MOV instructions.
//

@usableFromInline
internal func _parseASCII<
  CodeUnits: IteratorProtocol, Result: FixedWidthInteger
>(
  codeUnits: inout CodeUnits, radix: Result
) -> Result?
where CodeUnits.Element: UnsignedInteger {
  let c0_ = codeUnits.next()
  guard _fastPath(c0_ != nil), let c0 = c0_ else { return nil }
  if _fastPath(c0 != _ascii16("+") && c0 != _ascii16("-")) {
    return _parseUnsignedASCII(
      first: c0, rest: &codeUnits, radix: radix, positive: true)
  }
  let c1_ = codeUnits.next()
  guard _fastPath(c1_ != nil), let c1 = c1_ else { return nil }
  if _fastPath(c0 == _ascii16("-")) {
    return _parseUnsignedASCII(
      first: c1, rest: &codeUnits, radix: radix, positive: false)
  }
  else {
    return _parseUnsignedASCII(
      first: c1, rest: &codeUnits, radix: radix, positive: true)
  }
}

extension FixedWidthInteger {
  // _parseASCII function thunk that prevents inlining used as an implementation
  // detail for FixedWidthInteger.init(_: radix:) on the slow path to save code
  // size.
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @inline(never)
  @usableFromInline
  internal static func _parseASCIISlowPath<
    CodeUnits: IteratorProtocol, Result: FixedWidthInteger
  >(
    codeUnits: inout CodeUnits, radix: Result
  ) -> Result?
  where CodeUnits.Element: UnsignedInteger {
    return _parseASCII(codeUnits: &codeUnits, radix: radix)
  }
}
