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

@inlinable
@inline(__always)
internal func _asciiDigit<CodeUnit : UnsignedInteger, Result : BinaryInteger>(
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

@inlinable
@inline(__always)
internal func _parseUnsignedASCII<
  Rest : IteratorProtocol, Result: FixedWidthInteger
>(
  first: Rest.Element, rest: inout Rest, radix: Result, positive: Bool
) -> Result?
where Rest.Element : UnsignedInteger {
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
// TODO (TODO: JIRA): This needs to be completely rewritten. It's about 20KB of
// always-inline code, most of which are MOV instructions.
//

@inlinable
@inline(__always)
internal func _parseASCII<
  CodeUnits : IteratorProtocol, Result: FixedWidthInteger
>(
  codeUnits: inout CodeUnits, radix: Result
) -> Result?
where CodeUnits.Element : UnsignedInteger {
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
    CodeUnits : IteratorProtocol, Result: FixedWidthInteger
  >(
    codeUnits: inout CodeUnits, radix: Result
  ) -> Result?
  where CodeUnits.Element : UnsignedInteger {
    return _parseASCII(codeUnits: &codeUnits, radix: radix)
  }

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
  @inlinable // @specializable
  @_semantics("optimize.sil.specialize.generic.partial.never")
  public init?<S : StringProtocol>(_ text: S, radix: Int = 10) {
    _precondition(2...36 ~= radix, "Radix not in range 2...36")

    if let str = text as? String, str._guts.isFastUTF8 {
      guard let ret = str._guts.withFastUTF8 ({ utf8 -> Self? in
        var iter = utf8.makeIterator()
        return _parseASCII(codeUnits: &iter, radix: Self(radix))
      }) else {
        return nil
      }
      self = ret
      return
    }

    // TODO(String performance): We can provide fast paths for common radices,
    // native UTF-8 storage, etc.

    var iter = text.utf8.makeIterator()
    guard let ret = Self._parseASCIISlowPath(
      codeUnits: &iter, radix: Self(radix)
    ) else { return nil }

    self = ret
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
  @_semantics("optimize.sil.specialize.generic.partial.never")
  @inline(__always)
  public init?(_ description: String) {
    self.init(description, radix: 10)
  }
}
