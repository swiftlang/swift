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
  @inlinable @inline(__always)
  public init?<S: StringProtocol>(_ text: S, radix: Int = 10) {
    precondition(2...36 ~= radix)
    let result_: Self?
    if let text = text as? String, text._guts.isSmall {
      result_ = _parseSmall(text._guts.rawBits, radix: radix)
    } else {
      result_ = text.utf8.withContiguousStorageIfAvailable { utf8 -> Self? in
        var iter = utf8.makeIterator()
        return _parseFromUTF8Inlined(&iter, radix: radix)
      } ?? {
        var iter = text.utf8.makeIterator()
        return _parseFromUTF8(&iter, radix: radix)
      }()
    }
    guard _fastPath(result_ != nil), let result = result_ else { return nil }
    self = result
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
  public init?(_ description: String) {
    self.init(description, radix: 10)
  }
}

@inlinable @inline(__always)
internal func _parseSmall<Result: FixedWidthInteger>(
  _ rawGuts: _StringObject.RawBitPattern, radix: Int
) -> Result? {
  // We work with the bytes as they are arranged in the StringObject,
  // i.e. the leading characters are in the least-significant bits.
  // This is as opposed to SmallString order, which swaps them on big-endian
  // platforms to maintain the same in-memory order.
  var word1 = rawGuts.0
  let word2 = rawGuts.1 & 0x00ff_ffff_ffff_ffff
  let count = Int((rawGuts.1 &>> 56) & 0x0f)
  // Handle sign.
  // "0"..."9" == 0x30...0x39
  // "-" == 0x2d
  // "+" == 0x2b
  var hasMinus = false
  let first = word1 & 0xff
  if first < 0x30 { // Plus, minus or an invalid character.
    guard _fastPath(count > 1), // Disallow "-"/"+" without any digits.
      _fastPath(Result.isSigned && first == UInt8(ascii: "-"))
      || _fastPath(first == UInt8(ascii: "+"))
      else { return nil }
    hasMinus = _fastPath(first == UInt8(ascii: "-"))
    word1 = (word1 ^ first) | 0x30 // Clear first char and a "0".
  } else {
    guard _fastPath(count > 0) else { return nil }
  }
  // Choose specialization based on radix (normally branch is optimized away).
  let result_: UInt64?
  switch radix {
  case 10: result_ = _parseBase10Unsigned(from: (word1, word2), count: count)
  case 16: result_ = _parseHexUnsigned(from: (word1, word2), count: count)
  case 2: result_ = _parseBinaryUnsigned(from: (word1, word2), count: count)
  default:
    var raw = (word1.littleEndian, word2.littleEndian)
    result_ = withUnsafeBytes(of: &raw) { rawBufPtr -> UInt64? in
      let start = rawBufPtr.baseAddress._unsafelyUnwrappedUnchecked
        .assumingMemoryBound(to: UInt8.self)
      let ptr = UnsafeBufferPointer(start: start, count: count)
      var utf8 = ptr.makeIterator()
      let first = utf8.next()._unsafelyUnwrappedUnchecked
      return _parseFromUTF8Unsigned(first: first, rest: &utf8, radix: radix)
    }
  }
  guard _fastPath(result_ != nil), var result = result_ else { return nil }
  // Apply sign & check limits.
  // Note: This assumes Result is stored as two's complement,
  //   which is already assumed elsewhere in the stdlib.
  let max = Result.max.magnitude &+ (hasMinus ? 1 : 0)
  guard _fastPath(result <= max) else { return nil }
  if hasMinus { result = 0 &- result }
  // Or ensure the compiler generates a conditional negate.
  return Result(truncatingIfNeeded: result)
}

@inlinable @inline(__always)
internal func _parseBase10Unsigned(
  from rawGuts: _StringObject.RawBitPattern, count: Int
) -> UInt64? {
  // Note: Base 10 is parsed in 4-byte chunks.
  let (word1, word2) = rawGuts
  func convertToDigits(_ word: UInt64, bitCount: Int) -> UInt64? {
    let shift = (64 &- bitCount) // bitCount must be >0.
    // Convert characters to digits.
    var digits = word &- _allLanes(UInt8(ascii: "0"))
    // Align digits to big end and shift out garbage.
    digits &<<= shift
    // Check limits.
    // All non-ASCII also cause a high bit to be sit in one of the below.
    let below0 = digits // Underflow causes at least one high bit to be set.
    let above9 = digits &+ _allLanes(0x7f - 9)
    guard _fastPath((below0 | above9) & _allLanes(0x80) == 0)
      else { return nil }
    return digits
  }
  func parseDigitChunk(_ chunk32: UInt32) -> UInt64 {
    // We are given 0x0s0r_0q0p,
    // where p is the first character thus highest value (1000s' place).
    // We first turn this into 0x000s_000q_000r_000p, to spread them out over
    // the UInt64. Note this has the side-effect of swapping q & r.
    var chunk = UInt64(chunk32)
    chunk = (chunk | (chunk &<< 24)) & 0x000f_000f_000f_000f
    // Multiplying with the 'magic' number 0x03e8_000a_0064_0001,
    // we effectively take the sum of the multiplications below.
    // Note the that 10 & 100 are swapped to compensate for the swap above.
    // 0x000s_000q_000r_000p * 1
    // 0x000q_000r_000p_0000 * 100 (0x64)
    // 0x000r_000p_0000_0000 * 10 (0xa)
    // 0x000p_0000_0000_0000 * 1000 (0x3e8)
    // ------------------------------------ +
    // The top 4 nibbles will now contain the parsed value. The less-significant
    // bits will never affect those bits since 0x000r * 100 is always < 0x1_000.
    return (chunk &* 0x03e8_000a_0064_0001) &>> 48
  }
  // Main.
  let bitCount = count &* 8
  if bitCount <= 32 { // 1 chunk case (results <= 9999).
    let digits_ = convertToDigits(word1, bitCount: bitCount)
    guard _fastPath(digits_ != nil), let digits = digits_ else { return nil }
    return parseDigitChunk(UInt32(digits &>> 32))
  } else {
    // To handle the 2-word case more easily, we'll only parse the characters
    // that wouldn't fit into word2 as part of word1.
    let word1ParsedBitCount = bitCount % 64
    // Normally digitCount should be >0 but atm it handles that
    // as if it where 8, so that still actually checks out for our case.
    let d1_ = convertToDigits(word1, bitCount: word1ParsedBitCount)
    guard _fastPath(d1_ != nil), let d1 = d1_ else { return nil }
    var result = parseDigitChunk(UInt32(truncatingIfNeeded: d1))
    result &*= 10_000
    result &+= parseDigitChunk(UInt32(d1 &>> 32))
    // 2-word case (results > 99_999_999).
    if bitCount > 64 {
      // Move unparsed bits from word1 to word2 to fill it up to 64.
      let shift = (64 &- word1ParsedBitCount) // We know 0 < shift < 64.
      let word2 = (word2 &<< shift) | (word1 &>> word1ParsedBitCount)
      // Parse word2.
      let d2_ = convertToDigits(word2, bitCount: 64)
      guard _fastPath(d2_ != nil), let d2 = d2_ else { return nil }
      result &*= 10_000
      result &+= parseDigitChunk(UInt32(truncatingIfNeeded: d2))
      result &*= 10_000
      result &+= parseDigitChunk(UInt32(d2 &>> 32))
    }
    return result
  }
}

@inlinable @inline(__always)
internal func _parseHexUnsigned(
  from rawGuts: _StringObject.RawBitPattern, count: Int
) -> UInt64? {
  let (word1, word2) = rawGuts
  func parseChunk(chunk: UInt64, bitCount: Int) -> UInt64? {
    let shift = 64 &- bitCount // bitCount must be >0.
    // "0"..."9" == 0x30...0x39
    // "A"..."Z" == 0x41...0x5A
    // "a"..."z" == 0x61...0x7A
    var chunk = chunk
    var invalid = chunk // Could have high bits set due to non-ascii.
    let letterFlags = (chunk &+ _allLanes(0x7f - UInt8(ascii: "9")))
      & _allLanes(0x80)
    let letterValueMask = letterFlags &- (letterFlags &>> 7)
    // Make letters uppercase (zero bit 5).
    chunk = chunk & ~(_allLanes(0x20) & letterValueMask)
    // Check all letters are above "A" or above.
    let belowA = _allLanes(0x7f + UInt8(ascii: "A")) &- chunk
    invalid |= (belowA & letterFlags)
    // Convert characters to digits.
    let extraLetterOffset =
      _allLanes(UInt8(ascii: "A") - (UInt8(ascii: "9") + 1))
    var digits = chunk &- _allLanes(UInt8(ascii: "0"))
      &- (extraLetterOffset & letterValueMask)
    // Align digits to big end.
    digits &<<= shift
    // Check for invalid characters.
    invalid &= _allLanes(0x80) // Should result in all zero if valid.
    guard (digits | invalid) & ~_allLanes(0x0f) == 0 else { return nil }
    // Turn the digits into a single value.
    // 0x0w0v_0u0t_0s0r_0q0p => 0x0000_0000_pqrs_tuvw.
    // Basically just a PEXT instruction, but LLVM doesn't generate those atm.
    var x = digits
    x = (x | x &<< 12) & 0xff00_ff00_ff00_ff00 // 0xvw00_tu00_rs00_pq00
    x = (x | x &>> 24) & 0x0000_ffff_0000_ffff // 0x0000_tuvw_0000_pqrs
    x = (x &<< 16 | x &>> 32) & 0xffff_ffff    // 0x0000_0000_pqrs_tuvw
    return x
  }
  // Branch between simple path (results <= UInt32.max) and the general path.
  let bitCount = count &* 8
  if bitCount <= 64 {
    return parseChunk(chunk: word1, bitCount: bitCount)
  } else {
    let value1_ = parseChunk(chunk: word1, bitCount: 64)
    guard _fastPath(value1_ != nil), let value1 = value1_ else { return nil }
    let word2BitCount = bitCount &- 64
    let value2_ = parseChunk(chunk: word2, bitCount: word2BitCount)
    guard _fastPath(value2_ != nil), let value2 = value2_ else { return nil }
    return (value1 &<< (word2BitCount/2)) | value2
  }
}

@inlinable @inline(__always)
internal func _parseBinaryUnsigned(
  from rawGuts: _StringObject.RawBitPattern, count: Int
) -> UInt64? {
  let (word1, word2) = rawGuts
  /// - Note: `chunk` must store leading char in LSB. `bitCount` must be `>0`.
  func parseChunk(chunk: UInt64, bitCount: Int) -> UInt64? {
    let shift = 64 &- bitCount // Note: bitCount must be >0.
    // Convert characters to digits.
    var digits = chunk &- _allLanes(UInt8(ascii: "0"))
    // Align digits to big end and shift out garbage.
    digits &<<= shift
    // The following check is enough, even if the original value overflowed.
    guard _fastPath(digits & ~_allLanes(0x01) == 0) else { return nil }
    // Turn the digits into a single value. We start with (p-w being char 0-7):
    //   0b0000_000w__0000_000v__0000_000u__0000_000t
    //   __0000_000s__0000_000r__0000_000q__0000_000p
    // We can use a multiplication to effectively do the addition below:
    //   0b0000_000w__0000_000v__(…)__0000_000q__0000_000p
    //   0b0000_00v0__0000_00u0__(…)__0000_00p
    //   (…)
    //   0b0q00_0000__0p
    //   0bp
    //   ------------------------------------------------- +
    //   0bpqrs_tuvw__0pqr_stuv__00pq_rstu__000p_qrst
    //   __0000_pqrs__0000_0pqr__0000_00pq__0000_000p
    // Making the most-significant byte our desired output.
    return (digits &* 0x8040_2010_0804_0201) &>> 56
  }
  // Branch between simple path (results <= UInt8.max) and the general path.
  let bitCount = count &* 8
  if bitCount <= 64 {
    return parseChunk(chunk: word1, bitCount: bitCount)
  } else {
    let value1_ = parseChunk(chunk: word1, bitCount: 64)
    guard _fastPath(value1_ != nil), let value1 = value1_ else { return nil }
    let word2BitCount = bitCount &- 64
    let value2_ = parseChunk(chunk: word2, bitCount: word2BitCount)
    guard _fastPath(value2_ != nil), let value2 = value2_ else { return nil }
    return (value1 &<< (word2BitCount/8)) | value2
  }
}

@inlinable
internal func _parseFromUTF8<
  Iterator: IteratorProtocol, Result: FixedWidthInteger
>(_ utf8: inout Iterator, radix: Int) -> Result?
where Iterator.Element == UInt8 {
  return _parseFromUTF8Inlined(&utf8, radix: radix)
}

@inlinable @inline(__always)
internal func _parseFromUTF8Inlined<
  Iterator: IteratorProtocol, Result: FixedWidthInteger
>(_ utf8: inout Iterator, radix: Int) -> Result?
where Iterator.Element == UInt8 {
  typealias UResult = Result.Magnitude
  guard _fastPath(UResult(exactly: radix) != nil) else {
    preconditionFailure("Result.Magnitude must be able to represent the radix")
  }
  // Get first character.
  let first_ = utf8.next()
  guard _fastPath(first_ != nil), var first = first_ else { return nil }
  // Handle sign.
  let hasMinus: Bool
  if first < UInt8(ascii: "0") { // Both "+" and "-" are < any digit/letter.
    guard _fastPath(Result.isSigned && first == UInt8(ascii: "-"))
      || _fastPath(first == UInt8(ascii: "+")) else { return nil }
    hasMinus = (first == UInt8(ascii: "-"))
    // Get a new first character.
    let second_ = utf8.next()
    guard _fastPath(second_ != nil), let second = second_ else { return nil }
    first = second
  } else {
    hasMinus = false
  }
  // Parse value.
  let result_: UResult?
    = _parseFromUTF8Unsigned(first: first, rest: &utf8, radix: radix)
  guard _fastPath(result_ != nil), var result = result_ else { return nil }
  // Apply sign & check limits.
  let max = Result.max.magnitude &+ (hasMinus ? 1 : 0)
  guard _fastPath(result <= max) else { return nil }
  if hasMinus { result = 0 &- result }
  return Result(truncatingIfNeeded: result)
}

@inlinable @inline(__always)
internal func _parseFromUTF8Unsigned<
  Iterator: IteratorProtocol, Result: FixedWidthInteger
>(first: UInt8, rest utf8: inout Iterator, radix: Int) -> Result?
where Iterator.Element == UInt8, Result: UnsignedInteger {
  func parseCodeUnit(_ cu: UInt8) -> Result? {
    var digit = UInt(cu &- UInt8(ascii: "0"))
    if radix > 10 {
      // Clear bit 5 to uppercase, and invalidate digits under "A".
      let letterOffset = (cu & ~0x20) &- UInt8(ascii: "A")
      // Convert to a value, widening to ensure we don't roll any back over.
      let letterValue = UInt(letterOffset) &+ 10
      // Make a all-bits mask based on whether digit >= 10.
      let isLetter = UInt(bitPattern: Int(bitPattern: 10 &- digit) &>> 8)
      // Overwrite digit value if we have a letter (a ^ a == 0; 0 ^ a == a).
      digit ^= (digit ^ letterValue) & isLetter
    }
    // We only need to check the upper bound as we're using a UInt8.
    guard _fastPath(digit < radix) else { return nil }
    return Result(truncatingIfNeeded: digit)
  }
  let result_ = parseCodeUnit(first)
  guard _fastPath(result_ != nil), var result = result_ else { return nil }
  while let cu = utf8.next() {
    let digit_ = parseCodeUnit(cu)
    guard _fastPath(digit_ != nil), let digit = digit_ else { return nil }
    // Add digit: result = result * radix + digit.
    let overflow1, overflow2: Bool
    (result, overflow1) = result.multipliedReportingOverflow(by: Result(radix))
    (result, overflow2) = result.addingReportingOverflow(digit)
    guard _fastPath(!overflow1 && !overflow2) else { return nil }
  }
  return result
}

@_alwaysEmitIntoClient
internal func _allLanes(_ x: UInt8) -> UInt64 {
  return UInt64(x) &* 0x0101_0101_0101_0101
}

// Legacy ABI
// The functions below need to be preserved for ABI stability, though in most
// cases they'll likely have been inlined already since they were all
// originally marked @inline(__always).

@usableFromInline
internal func _ascii16(_ c: Unicode.Scalar) -> UTF16.CodeUnit {
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
  @usableFromInline
  internal static func _parseASCIISlowPath<
    CodeUnits: IteratorProtocol, Result: FixedWidthInteger
  >(codeUnits: inout CodeUnits, radix: Result) -> Result?
  where CodeUnits.Element: UnsignedInteger {
    return _parseASCII(codeUnits: &codeUnits, radix: radix)
  }
}
