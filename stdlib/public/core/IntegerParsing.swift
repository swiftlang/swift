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

@_alwaysEmitIntoClient
internal func _parseIntegerDigits<Result: FixedWidthInteger>(
  ascii codeUnits: UnsafeBufferPointer<UInt8>, radix: Int, isNegative: Bool
) -> Result? {
  _internalInvariant(radix >= 2 && radix <= 36)
  guard _fastPath(!codeUnits.isEmpty) else { return nil }
  
  // ASCII constants, named for clarity:
  let _0 = 48 as UInt8, _A = 65 as UInt8, _a = 97 as UInt8
  
  let numericalUpperBound: UInt8
  let uppercaseUpperBound: UInt8
  let lowercaseUpperBound: UInt8
  if radix <= 10 {
    numericalUpperBound = _0 &+ UInt8(truncatingIfNeeded: radix)
    uppercaseUpperBound = _A
    lowercaseUpperBound = _a
  } else {
    numericalUpperBound = _0 &+ 10
    uppercaseUpperBound = _A &+ UInt8(truncatingIfNeeded: radix &- 10)
    lowercaseUpperBound = _a &+ UInt8(truncatingIfNeeded: radix &- 10)
  }
  let multiplicand = Result(truncatingIfNeeded: radix)
  var result = 0 as Result
  for unsafe digit in unsafe codeUnits {
    let digitValue: Result
    if _fastPath(digit >= _0 && digit < numericalUpperBound) {
      digitValue = Result(truncatingIfNeeded: digit &- _0)
    } else if _fastPath(digit >= _A && digit < uppercaseUpperBound) {
      digitValue = Result(truncatingIfNeeded: digit &- _A &+ 10)
    } else if _fastPath(digit >= _a && digit < lowercaseUpperBound) {
      digitValue = Result(truncatingIfNeeded: digit &- _a &+ 10)
    } else {
      return nil
    }
    let overflow1: Bool
    (result, overflow1) = result.multipliedReportingOverflow(by: multiplicand)
    let overflow2: Bool
    (result, overflow2) = isNegative
      ? result.subtractingReportingOverflow(digitValue)
      : result.addingReportingOverflow(digitValue)
    guard _fastPath(!overflow1 && !overflow2) else { return nil }
  }
  return result
}

@_alwaysEmitIntoClient
@inline(__always)
internal func _parseInteger<Result: FixedWidthInteger>(
  ascii codeUnits: UnsafeBufferPointer<UInt8>, radix: Int
) -> Result? {
  _internalInvariant(!codeUnits.isEmpty)
  
  // ASCII constants, named for clarity:
  let _plus = 43 as UInt8, _minus = 45 as UInt8
  
  let first = unsafe codeUnits[0]
  if first == _minus {
    return unsafe _parseIntegerDigits(
      ascii: UnsafeBufferPointer(rebasing: codeUnits[1...]),
      radix: radix, isNegative: true)
  }
  if first == _plus {
    return unsafe _parseIntegerDigits(
      ascii: UnsafeBufferPointer(rebasing: codeUnits[1...]),
      radix: radix, isNegative: false)
  }
  return unsafe _parseIntegerDigits(ascii: codeUnits, radix: radix, isNegative: false)
}

@_alwaysEmitIntoClient
@inline(never)
internal func _parseInteger<S: StringProtocol, Result: FixedWidthInteger>(
  ascii text: S, radix: Int
) -> Result? {
  var str = String(text)
  return str.withUTF8 { unsafe _parseInteger(ascii: $0, radix: radix) }
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
        unsafe _parseInteger(ascii: $0, radix: radix)
      } ?? _parseInteger(ascii: text, radix: radix)
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

//===----------------------------------------------------------------------===//
// Old entry points preserved for ABI compatibility.
//===----------------------------------------------------------------------===//

/// Returns c as a UTF16.CodeUnit.  Meant to be used as _ascii16("x").
@usableFromInline // Previously '@inlinable'.
internal func _ascii16(_ c: Unicode.Scalar) -> UTF16.CodeUnit {
  _internalInvariant(c.value >= 0 && c.value <= 0x7F, "not ASCII")
  return UTF16.CodeUnit(c.value)
}

@usableFromInline // Previously '@inlinable @inline(__always)'.
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

@usableFromInline // Previously '@inlinable @inline(__always)'.
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

// This function has been superseded because it is about 20KB of previously
// always-inline code, most of which are MOV instructions.

@usableFromInline // Previously '@inlinable @inline(__always)'.
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
