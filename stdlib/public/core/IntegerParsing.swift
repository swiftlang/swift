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
    let wholeGuts = text._wholeGuts
    // The specialized paths require that all of the contiguous bytes can
    // be read using UInt64 loads from (address & ~7).
    if wholeGuts._object.isPreferredRepresentation, Self.bitWidth <= 64 {
      let r64_ = wholeGuts.withFastUTF8({ utf8 -> UInt64? in
        let utf8 = (S.self == String.self) ? utf8 :
          UnsafeBufferPointer(rebasing: utf8[text._offsetRange])
        let isSigned = Self.isSigned
        let max = UInt64(Self.max)
        switch radix {
        case 10: return _parseIntegerBase10(from: utf8,
                                            allowNegative: isSigned, max: max)
        case 16: return _parseIntegerBase16(from: utf8,
                                            allowNegative: isSigned, max: max)
        case 2: return _parseIntegerBase2(from: utf8,
                                          allowNegative: isSigned, max: max)
        default: return _parseInteger(from: utf8, radix: radix,
                                      allowNegative: isSigned, max: max)
        }
      })
      guard _fastPath(r64_ != nil), let r64 = r64_ else { return nil }
      self = Self(truncatingIfNeeded: r64)
    } else {
      var iter = text.utf8.makeIterator()
      let result_: Self? = _parseInteger(from: &iter, radix: radix)
      guard _fastPath(result_ != nil), let result = result_ else { return nil }
      self = result
    }
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

@usableFromInline
internal func _parseIntegerBase10(
  from utf8: UnsafeBufferPointer<UInt8>, allowNegative: Bool, max: UInt64
) -> UInt64? {
  return _parseIntegerSpecialized(from: utf8, allowNegative: allowNegative,
                                  max: max, using: _parseUnsignedBase10(from:))
}

@usableFromInline
internal func _parseIntegerBase16(
  from utf8: UnsafeBufferPointer<UInt8>, allowNegative: Bool, max: UInt64
) -> UInt64? {
  return _parseIntegerSpecialized(from: utf8, allowNegative: allowNegative,
                                  max: max, using: _parseUnsignedBase16(from:))
}

@usableFromInline
internal func _parseIntegerBase2(
  from utf8: UnsafeBufferPointer<UInt8>, allowNegative: Bool, max: UInt64
) -> UInt64? {
  return _parseIntegerSpecialized(from: utf8, allowNegative: allowNegative,
                                  max: max, using: _parseUnsignedBase2(from:))
}

@usableFromInline
internal func _parseInteger(
  from utf8: UnsafeBufferPointer<UInt8>,
  radix: Int,
  allowNegative: Bool,
  max: UInt64
) -> UInt64? {
  return _parseIntegerSpecialized(from: utf8, allowNegative: allowNegative,
                                  max: max, using: { utf8 in
    var iter = utf8.makeIterator()
    let first = iter.next()._unsafelyUnwrappedUnchecked
    return _parseFromUTF8Unsigned(first: first, rest: &iter, radix: radix)
  })
}

@inline(__always)
internal func _parseIntegerSpecialized(
  from utf8: UnsafeBufferPointer<UInt8>,
  allowNegative: Bool,
  max: UInt64,
  using parseUnsigned: (UnsafeBufferPointer<UInt8>) -> UInt64?
) -> UInt64? {
  var utf8 = utf8
  var minusBit: UInt64 = 0
  guard _fastPath(utf8.count > 0) else { return nil }
  let first = utf8.first._unsafelyUnwrappedUnchecked
  if first < 0x30 { // Plus, minus or an invalid character.
    guard _fastPath(utf8.count > 1), // Disallow "-"/"+" without any digits.
      _fastPath(first == UInt8(ascii: "-")) // Note "-0" is valid for UInts.
      || _fastPath(first == UInt8(ascii: "+")) else { return nil }
    minusBit = _fastPath(first == UInt8(ascii: "-")) ? 1 : 0
    utf8 = UnsafeBufferPointer(start: utf8.baseAddress.unsafelyUnwrapped + 1,
                               count: utf8.count &- 1)
  }
  // Choose specialization based on radix (normally branch is optimized away).
  let result_ = parseUnsigned(utf8)
  guard _fastPath(result_ != nil), var result = result_ else { return nil }
  // Apply sign & check limits.
    if allowNegative {
    // Note: This assumes Result is stored as two's complement,
    //   but this is also already assumed elsewhere in the stdlib.
    guard _fastPath(result <= max &+ minusBit) else { return nil }
    result = (result ^ (0 &- minusBit)) &+ minusBit
  } else {
    guard _fastPath(minusBit == 0) || result == 0,
      _fastPath(result <= max) else { return nil }
  }
  return result
}

@inline(__always)
internal func _parseUnsignedBase10(
  from utf8: UnsafeBufferPointer<UInt8>
) -> UInt64? {
  // Note: Base 10 is parsed in 4-byte chunks.
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
  let p = UnsafeRawPointer(utf8.baseAddress._unsafelyUnwrappedUnchecked)
  let count = utf8.count
  let firstCount = (count &- 1) % 8 &+ 1
  let firstChunk = _loadUnalignedChunk(p, count: firstCount)
  let firstDigits_ = convertToDigits(firstChunk, bitCount: firstCount &* 8)
  guard _fastPath(firstDigits_ != nil), let firstDigits = firstDigits_
      else { return nil }
  var value = parseDigitChunk(UInt32(truncatingIfNeeded: firstDigits &>> 32))
  if count <= 4 { return value }
  value &+= parseDigitChunk(UInt32(truncatingIfNeeded: firstDigits)) &* 10_000
  var consumed = firstCount
  while consumed < count {
    let chunk = _loadUnalignedChunk(p + consumed, count: 8)
    let chunkDigits_ = convertToDigits(chunk, bitCount: 64)
    guard _fastPath(chunkDigits_ != nil), let chunkDigits = chunkDigits_
      else { return nil }
    let chunkValue =
      parseDigitChunk(UInt32(truncatingIfNeeded: chunkDigits)) &* 10_000
      &+ parseDigitChunk(UInt32(truncatingIfNeeded: chunkDigits &>> 32))
    let overflow1, overflow2: Bool
    (value, overflow1) = value.multipliedReportingOverflow(by: 100_000_000)
    (value, overflow2) = value.addingReportingOverflow(chunkValue)
    guard _fastPath(!overflow1 && !overflow2) else { return nil }
    consumed &+= 8
  }
  return value
}

@inline(__always)
internal func _parseUnsignedBase16(
  from utf8: UnsafeBufferPointer<UInt8>
) -> UInt64? {
  func parseChunk(_ chunk: UInt64, bitCount: Int) -> UInt64? {
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
  let p = UnsafeRawPointer(utf8.baseAddress._unsafelyUnwrappedUnchecked)
  let count = utf8.count
  let firstCount = (count &- 1) % 8 &+ 1
  let firstChunk = _loadUnalignedChunk(p, count: firstCount)
  let firstValue_ = parseChunk(firstChunk, bitCount: firstCount &* 8)
  guard _fastPath(firstValue_ != nil), var value = firstValue_
      else { return nil }
  var consumed = firstCount
  while consumed < count {
    guard _fastPath(value < 0x1_0000_0000) else { return nil }
    let chunk = _loadUnalignedChunk(p + consumed, count: 8)
    let chunkValue_ = parseChunk(chunk, bitCount: 64)
    guard _fastPath(chunkValue_ != nil), let chunkValue = chunkValue_
      else { return nil }
    value = (value &<< 32) &+ chunkValue
    consumed &+= 8
  }
  return value
}

@inline(__always)
internal func _parseUnsignedBase2(
  from utf8: UnsafeBufferPointer<UInt8>
) -> UInt64? {
  /// - Note: `chunk` must store leading char in LSB. `bitCount` must be `>0`.
  func parseChunk(_ chunk: UInt64, bitCount: Int) -> UInt64? {
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
  let p = UnsafeRawPointer(utf8.baseAddress._unsafelyUnwrappedUnchecked)
  let count = utf8.count
  let firstCount = (count &- 1) % 8 &+ 1
  let firstChunk = _loadUnalignedChunk(p, count: firstCount)
  let firstValue_ = parseChunk(firstChunk, bitCount: firstCount &* 8)
  guard _fastPath(firstValue_ != nil), var value = firstValue_
      else { return nil }
  var consumed = firstCount
  while consumed < count {
    guard _fastPath(value < 0x100_0000_0000_0000) else { return nil }
    let chunk = _loadUnalignedChunk(p + consumed, count: 8)
    let chunkValue_ = parseChunk(chunk, bitCount: 64)
    guard _fastPath(chunkValue_ != nil), let chunkValue = chunkValue_
      else { return nil }
    value = (value &<< 8) &+ chunkValue
    consumed &+= 8
  }
  return value
}

@_alwaysEmitIntoClient
internal func _loadUnalignedChunk(
  _ ptr: UnsafeRawPointer, count: Int
) -> UInt64 {
  // Load up to 8 bytes into a UInt64, in little-endian order. Caller must
  // guarantee that for each byte the entire 64 bits in which it falls using
  // 64-bit alignment is allowed to be read. The unused parts of the UInt64
  // are garbage and are the caller's resposibility to ignore.
  let first = UInt(bitPattern: ptr)
  let last = first &+ UInt(bitPattern: count) &- 1
    let w1 = UnsafeRawPointer(bitPattern: first & ~0x07)
    ._unsafelyUnwrappedUnchecked.load(as: UInt64.self)
  let w2 = UnsafeRawPointer(bitPattern: last & ~0x07)
    ._unsafelyUnwrappedUnchecked.load(as: UInt64.self)
  let offset = (first & 0x07) &* 8 // Bit offset inside first UInt64.
  // Combine the two words.
  // If count is not high enough to push `last` into the next UInt64, w1 and w2
  // will be identical, but the shift below will push the second copy out of
  // relevance. Also note that if offset is 0, the second shift will mask to 0
  // instead of 64, but in this case w1 and w2 must be identical and neither
  // is shifted thus making `w1 | w2` still correct.
  if 1 == 1.littleEndian {
    return (w1 &>> offset) | (w2 &<< (64 &- offset))
  } else {
    return ((w1 &<< offset) | (w2 &>> (64 &- offset))).littleEndian
  }
}

@usableFromInline
internal func _parseInteger<
  Iterator: IteratorProtocol, Result: FixedWidthInteger
>(from utf8: inout Iterator, radix: Int) -> Result?
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
    guard _fastPath(first == UInt8(ascii: "-"))
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
  if Result.isSigned {
    let max = Result.max.magnitude &+ (hasMinus ? 1 : 0)
    guard _fastPath(result <= max) else { return nil }
    if hasMinus { result = 0 &- result }
  } else {
    guard _fastPath(!hasMinus) || result == 0,
      _fastPath(result <= Result.max.magnitude) else { return nil }
  }
  return Result(truncatingIfNeeded: result)
}

@inline(__always)
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
      // Make an all-bits mask based on whether digit > 9.
      let isLetter = UInt(bitPattern: Int(bitPattern: 9 &- digit) &>> 8)
      // Overwrite digit value if we have a letter (a ^ a == 0; 0 ^ a == a).
      digit ^= (digit ^ letterValue) & isLetter
    }
    // We only need to check the upper bound since we're using an unsigned int.
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

@_alwaysEmitIntoClient @inline(__always)
internal func _allLanes(_ x: UInt8) -> UInt64 {
  return UInt64(x) &* 0x0101_0101_0101_0101
}
