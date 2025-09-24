//===--- FloatingPointToString.swift -------------------------*- Swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
// Converts floating-point types to "optimal" text formats.
//
// The "optimal" form is one with a minimum number of significant
// digits which will parse to exactly the original value.  This form
// is ideal for JSON serialization and general printing where you
// don't have specific requirements on the number of significant
// digits.
//
//===---------------------------------------------------------------------===//
///
/// For binary16, this code uses a simple approach that is normally
/// implemented with variable-length arithmetic.  However, due to the
/// limited range of binary16, this can be implemented with only
/// 32-bit integer arithmetic.
///
/// For other formats, we use a modified form of the Grisu2
/// algorithm from Florian Loitsch; "Printing Floating-Point Numbers
/// Quickly and Accurately with Integers", 2010.
/// https://doi.org/10.1145/1806596.1806623
///
/// Some of the Grisu2 modifications were suggested by the "Errol
/// paper": Marc Andrysco, Ranjit Jhala, Sorin Lerner; "Printing
/// Floating-Point Numbers: A Faster, Always Correct Method", 2016.
/// https://doi.org/10.1145/2837614.2837654
/// In particular, the Errol paper explored the impact of higher-precision
/// fixed-width arithmetic on Grisu2 and showed a way to rapidly test
/// the correctness of Grisu-style algorithms.
///
/// A few further improvements were inspired by the Ryu algorithm
/// from Ulf Anders; "Ryū: fast float-to-string conversion", 2018.
/// https://doi.org/10.1145/3296979.3192369
///
/// The full algorithm is extensively commented in the Float64 version
/// below; refer to that for details.
///
/// In summary, this implementation is:
///
/// * Fast.  It uses only fixed-width integer arithmetic and has
///   constant memory requirements.  For double-precision values on
///   64-bit processors, it is competitive with Ryu. For double-precision
///   values on 32-bit processors, and higher-precision values on all
///   processors, it is considerably faster.
///
/// * Always Accurate. Except for NaNs, converting the decimal form
///   back to binary will always yield an equal value. For the IEEE
///   754 formats, the round trip will produce exactly the same bit
///   pattern in memory. This assumes, of course, that the conversion
///   from text to binary uses a correctly-rounded algorithm such as
///   Clinger 1990 or Eisel-Lemire 2021.
///
/// * Always Short.  This always selects an accurate result with the
///   minimum number of significant digits.
///
/// * Always Close.  Among all accurate, short results, this always
///   chooses the result that is closest to the exact floating-point
///   value. (In case of an exact tie, it rounds the last digit even.)
///
/// Beyond the requirements above, the precise text form has been
/// tuned to try to maximize readability:
/// * Always include a '.' or an 'e' so the result is obviously
///   a floating-point value
/// * Exponential form always has 1 digit before the decimal point
/// * When present, a '.' is never the first or last character
/// * There is a consecutive range of integer values that can be
///   represented in any particular type (-2^54...2^54 for double).
///   We do not use exponential form for integral numbers in this
///   range.
/// * Generally follow existing practice: Don't use use exponential
///   form for fractional values bigger than 10^-4; always write at
///   least 2 digits for an exponent.
/// * Apart from the above, we do prefer shorter output.

/// Note: If you want to compare performance of this implementation
/// versus some others, keep in mind that this implementation does
/// deliberately sacrifice some performance.  Any attempt to compare
/// the performance of this implementation to others should
/// try to compensate for the following:
/// * The output ergonomics described above do take some time.
///   It would be faster to always emit the form "123456e-78"
//    (See `finishFormatting()`)
/// * The implementations in published papers generally include
///   large tables with every power of 10 computed out.  We've
///   factored these tables down to conserve code size, which
///   requires some additional work to reconstruct the needed power
///   of 10. (See the `intervalContainingPowerOf10_*` functions)

///
/// This Swift implementation was ported from an earlier C version;
/// the output is exactly the same in all cases.
/// A few notes on the Swift transcription:
/// * We use MutableSpan<UTF8.CodeUnit> and MutableRawSpan to
///   identify blocks of working memory.
/// * We use unsafe/unchecked operations extensively, supported by
///   several years of analysis and testing of the original C
///   implementation to ensure that no unsafety actually occurs.  For
///   Float32, that testing was exhaustive -- we verified all 4
///   billion possible Float32 values.
/// * The Swift code uses an idiom of building up to 8 digit characters
///   in a UInt64 and then writing the whole block to memory.
/// * The Swift version is slightly faster than the C version;
///   mostly thanks to various minor algorithmic tweaks that were
///   found during the translation process.
/// * Most of this file is annotated for SwiftStdlib 6.2
///   because it relies on UInt128, MutableSpan, and InlineArray.
///
// ----------------------------------------------------------------------------


// ================================================================
//
// Float16
//
// ================================================================

#if (os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64)

// Float16 is not currently supported on Intel x86_64 macOS,
// (including macCatalyst on x86_64) but this symbol somehow got
// exported there.
// This preserves that export.

// Note: Other platforms that don't support Float16 should
// NOT export this.
@available(SwiftStdlib 5.3, *)
@_silgen_name("swift_float16ToString")
public func _float16ToStringImpl(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float,
  _ debug: Bool
) -> UInt64 {
    fatalError()
}

#else

// Support Legacy ABI on top of new implementation
@available(SwiftStdlib 5.3, *)
@_silgen_name("swift_float16ToString")
public func _float16ToStringImpl(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float16,
  _ debug: Bool
) -> UInt64 {
  if #available(SwiftStdlib 6.2, *) {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
      _unchecked: textBuffer,
      count: Int(bufferLength))
    let textRange = _Float16ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
      unsafe _memmove(
        dest: textBuffer,
        src: textBuffer + textRange.lowerBound,
        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
  } else {
    fatalError()
  }
}

// Convert a Float16 to an optimal ASCII representation.
// See notes above for comments on the output format here.
// Inputs:
// * `value`: Float16 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 32 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`

@available(SwiftStdlib 6.2, *)
internal func _Float16ToASCII(
  value f: Float16,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // We need a MutableRawSpan in order to use wide store/load operations
  // TODO: Tune this value down to the actual minimum for Float16
  precondition(utf8Buffer.count >= 32)
  var buffer = utf8Buffer.mutableBytes

  // Step 1: Handle various input cases:
  let binaryExponent: Int
  let significand: Float16.RawSignificand
  let exponentBias = (1 << (Float16.exponentBitCount - 1)) - 2 // 14
  if (f.exponentBitPattern == 0x1f) { // NaN or Infinity
    if (f.isInfinite) {
      return _infinity(buffer: &buffer, sign: f.sign)
    } else { // f.isNaN
      let quietBit =
        (f.significandBitPattern >> (Float16.significandBitCount - 1)) & 1
      let payloadMask = UInt16(1 &<< (Float16.significandBitCount - 2)) - 1
      let payload16 = f.significandBitPattern & payloadMask
      return nan_details(
        buffer: &buffer,
        sign: f.sign,
        quiet: quietBit != 0,
        payloadHigh: 0,
        payloadLow: UInt64(truncatingIfNeeded:payload16))
    }
  } else if (f.exponentBitPattern == 0) {
    if (f.isZero) {
      return _zero(buffer: &buffer, sign: f.sign)
    } else { // Subnormal
      binaryExponent = 1 - exponentBias
      significand = f.significandBitPattern &<< 2
    }
  } else { // normal
    binaryExponent = Int(f.exponentBitPattern) &- exponentBias
    let hiddenBit = Float16.RawSignificand(1) << Float16.significandBitCount
    significand = (f.significandBitPattern &+ hiddenBit) &<< 2
  }

  // Step 2: Determine the exact target interval
  let halfUlp: Float16.RawSignificand = 2
  let quarterUlp = halfUlp >> 1
  let upperMidpointExact =
    significand &+ halfUlp
  let lowerMidpointExact =
    significand &- ((f.significandBitPattern == 0) ? quarterUlp : halfUlp)

  var firstDigit = 1
  var nextDigit = firstDigit

  // Emit the text form differently depending on what range it's in.
  // We use `storeBytes(of:toUncheckedByteOffset:as:)` for most of
  // the output, but are careful to use the checked/safe form
  // `storeBytes(of:toByteOffset:as:)` for the last byte so that we
  // reliably crash if we overflow the provided buffer.

  // Step 3: If it's < 10^-5, format as exponential form
  if binaryExponent < -13 || (binaryExponent == -13 && significand < 0x1a38) {
    var decimalExponent = -5
    var u =
      (UInt32(upperMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
    var l =
      (UInt32(lowerMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
    var t =
      (UInt32(significand) << (28 - 13 &+ binaryExponent)) &* 100000
    let mask = (UInt32(1) << 28) - 1
    if t < ((1 << 28) / 10) {
      u &*= 100
      l &*= 100
      t &*= 100
      decimalExponent &-= 2
    }
    if t < (1 << 28) {
      u &*= 10
      l &*= 10
      t &*= 10
      decimalExponent &-= 1
    }
    let uDigit = u >> 28
    if uDigit == (l >> 28) {
      // More than one digit, so write first digit, ".", then the rest
      unsafe buffer.storeBytes(
        of: 0x30 + UInt8(truncatingIfNeeded: uDigit),
        toUncheckedByteOffset: nextDigit,
        as: UInt8.self)
      nextDigit &+= 1
      unsafe buffer.storeBytes(
        of: 0x2e,
        toUncheckedByteOffset: nextDigit,
        as: UInt8.self)
      nextDigit &+= 1
      while true {
        u = (u & mask) &* 10
        l = (l & mask) &* 10
        t = (t & mask) &* 10
        let uDigit = u >> 28
        if uDigit != (l >> 28) {
          // Stop before emitting the last digit
          break
        }
        unsafe buffer.storeBytes(
          of: 0x30 &+ UInt8(truncatingIfNeeded: uDigit),
          toUncheckedByteOffset: nextDigit,
          as: UInt8.self)
        nextDigit &+= 1
      }
    }
    let digit = 0x30 &+ (t &+ (1 << 27)) >> 28
    unsafe buffer.storeBytes(
      of: UInt8(truncatingIfNeeded: digit),
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    unsafe buffer.storeBytes(
      of: 0x65, // "e"
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    unsafe buffer.storeBytes(
      of: 0x2d, // "-"
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    unsafe buffer.storeBytes(
      of: UInt8(truncatingIfNeeded: -decimalExponent / 10 &+ 0x30),
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    // Last write on this branch, so use a safe checked store
    buffer.storeBytes(
      of: UInt8(truncatingIfNeeded: -decimalExponent % 10 &+ 0x30),
      toByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
  } else {

    // Step 4: Greater than 10^-5, so use decimal format "123.45"
    // (Note: Float16 is never big enough to need exponential for
    // positive exponents)
    // First, split into integer and fractional parts:

    let intPart : Float16.RawSignificand
    let fractionPart : Float16.RawSignificand
    if binaryExponent < 13 {
      intPart = significand >> (13 &- binaryExponent)
      fractionPart = significand &- (intPart &<< (13 &- binaryExponent))
    } else {
      intPart = significand &<< (binaryExponent &- 13)
      fractionPart = significand &- (intPart >> (binaryExponent &- 13))
    }

    // Step 5: Emit the integer part
    let text = _intToEightDigits(UInt32(intPart))
    unsafe buffer.storeBytes(
      of: text,
      toUncheckedByteOffset: nextDigit,
      as: UInt64.self)
    nextDigit &+= 8

    // Skip leading zeros
    if intPart < 10 {
      firstDigit &+= 7
    } else if intPart < 100 {
      firstDigit &+= 6
    } else if intPart < 1000 {
      firstDigit &+= 5
    } else if intPart < 10000 {
      firstDigit &+= 4
    } else {
      firstDigit &+= 3
    }

    // After the integer part comes a period...
    unsafe buffer.storeBytes(
      of: 0x2e,
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1

    if fractionPart == 0 {
      // Step 6: No fraction, so ".0" and we're done
      // "0" write is free since buffer is pre-initialized
      nextDigit &+= 1
    } else {
      // Step 7: Emit the fractional part by repeatedly
      // multiplying by 10 to produce successive digits:
      var u = UInt32(upperMidpointExact) &<< (28 - 13 &+ binaryExponent)
      var l = UInt32(lowerMidpointExact) &<< (28 - 13 &+ binaryExponent)
      var t = UInt32(fractionPart) &<< (28 - 13 &+ binaryExponent)
      let mask = (UInt32(1) << 28) - 1
      var uDigit: UInt8 = 0
      var lDigit: UInt8 = 0
      while true {
        u = (u & mask) &* 10
        l = (l & mask) &* 10
        uDigit = UInt8(truncatingIfNeeded: u >> 28)
        lDigit = UInt8(truncatingIfNeeded: l >> 28)
        if uDigit != lDigit {
          t = (t & mask) &* 10
          break
        }
        // This overflows, but we don't care at this point.
        t &*= 10
        unsafe buffer.storeBytes(
          of: 0x30 &+ uDigit,
          toUncheckedByteOffset: nextDigit,
          as: UInt8.self)
        nextDigit &+= 1
      }
      t &+= 1 << 27
      if (t & mask) == 0 { // Exactly 1/2
        t = (t >> 28) & ~1 // Round last digit even
        // Rounding `t` even can end up moving `t` below
        // `l`.  Detect and correct for this possibility.
        // Exhaustive testing shows that the only input value
        // affected by this is 0.015625 == 2^-6, which
        // incorrectly prints as "0.01562" without this fix.
        // With this, it prints correctly as "0.01563"
        if t < lDigit || (t == lDigit && l > 0) {
	        t += 1
        }
      } else {
        t >>= 28
      }
      // Last write on this branch, so use a checked store
      buffer.storeBytes(
        of: UInt8(truncatingIfNeeded: 0x30 + t),
        toByteOffset: nextDigit,
        as: UInt8.self)
      nextDigit &+= 1
    }
  }
  if f.sign == .minus {
    buffer.storeBytes(
      of: 0x2d,
      toByteOffset: firstDigit &- 1,
      as: UInt8.self) // "-"
    firstDigit &-= 1
  }
  return firstDigit..<nextDigit
}
#endif

// ================================================================
//
// Float32
//
// ================================================================

// Support Legacy ABI on top of new implementation
@_silgen_name("swift_float32ToString")
@usableFromInline
internal func _float32ToStringImpl(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float32,
  _ debug: Bool
) -> UInt64 {
  if #available(SwiftStdlib 6.2, *) {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
      _unchecked: textBuffer,
      count: Int(bufferLength))
    let textRange = _Float32ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
      unsafe _memmove(
        dest: textBuffer,
        src: textBuffer + textRange.lowerBound,
        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
  } else {
    fatalError()
  }
}

// Convert a Float32 to an optimal ASCII representation.
// See notes above for comments on the output format here.
// See _Float64ToASCII for comments on the algorithm.
// Inputs:
// * `value`: Float32 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 32 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`
@available(SwiftStdlib 6.2, *)
internal func _Float32ToASCII(
  value f: Float32,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // Note: The algorithm here is the same as for Float64, only
  // with narrower arithmetic.  Refer to `_Float64ToASCII` for
  // more detailed comments and explanation.

  // We need a MutableRawSpan in order to use wide store/load operations
  // TODO: Tune this limit down to the actual minimum we need here
  // TODO: `assert` that the buffer is filled with 0x30 bytes (in debug builds)
  precondition(utf8Buffer.count >= 32)
  var buffer = utf8Buffer.mutableBytes

  // Step 1: Handle the special cases, decompose the input

  let binaryExponent: Int
  let significand: Float.RawSignificand
  let exponentBias = (1 << (Float.exponentBitCount - 1)) - 2 // 126
  if (f.exponentBitPattern == 0xff) {
    if (f.isInfinite) {
      return _infinity(buffer: &buffer, sign: f.sign)
    } else { // f.isNaN
      let quietBit =
        (f.significandBitPattern >> (Float.significandBitCount - 1)) & 1
      let payloadMask = UInt32(1 << (Float.significandBitCount - 2)) - 1
      let payload32 = f.significandBitPattern & payloadMask
      return nan_details(
        buffer: &buffer,
        sign: f.sign,
        quiet: quietBit != 0,
        payloadHigh: 0,
        payloadLow: UInt64(truncatingIfNeeded:payload32))
    }
  } else if (f.exponentBitPattern == 0) {
    if (f.isZero) {
      return _zero(buffer: &buffer, sign: f.sign)
    } else { // f.isSubnormal
      binaryExponent = 1 - exponentBias
      significand = f.significandBitPattern &<< Float.exponentBitCount
    }
  } else {
    binaryExponent = Int(f.exponentBitPattern) &- exponentBias
    significand =
      ((f.significandBitPattern &+ (1 << Float.significandBitCount))
         &<< Float.exponentBitCount)
  }

  // Step 2: Determine the exact unscaled target interval

  let halfUlp: Float.RawSignificand = 1 << (Float.exponentBitCount - 1)
  let quarterUlp = halfUlp >> 1
  let upperMidpointExact =
    significand &+ halfUlp
  let lowerMidpointExact =
    significand &- ((f.significandBitPattern == 0) ? quarterUlp : halfUlp)
  let isOddSignificand = ((f.significandBitPattern & 1) != 0)

  // Step 3: Estimate the base 10 exponent

  var base10Exponent = decimalExponentFor2ToThe(binaryExponent)

  // Step 4: Compute power-of-10 scale factor

  var powerOfTenRoundedDown: UInt64 = 0
  var powerOfTenRoundedUp: UInt64 = 0

  let bulkFirstDigits = 1
  let powerOfTenExponent = _intervalContainingPowerOf10_Binary32(
    p: -base10Exponent &+ bulkFirstDigits &- 1,
    lower: &powerOfTenRoundedDown,
    upper: &powerOfTenRoundedUp)
  let extraBits = binaryExponent &+ powerOfTenExponent

  // Step 5: Scale the interval (with rounding)

  // Experimentally, 11 is as large as we can go here without
  // introducing errors.
  // We need 7 to generate 2 digits at a time below.
  // 11 should allow us to generate 3 digits at a time, but
  // that doesn't seem to be any faster.
  let integerBits = 11
  let fractionBits = 64 - integerBits
  var u: UInt64
  var l: UInt64
  if isOddSignificand {
    // Narrow the interval (odd significand)
    let u1 = _multiply64x32RoundingDown(
      powerOfTenRoundedDown,
      upperMidpointExact)
    u = u1 >> (integerBits - extraBits)
    let l1 = _multiply64x32RoundingUp(
      powerOfTenRoundedUp,
      lowerMidpointExact)
    let bias = UInt64((1 &<< (integerBits &- extraBits)) &- 1)
    l = (l1 &+ bias) >> (integerBits &- extraBits)
  } else {
    // Widen the interval (even significand)
    let u1 = _multiply64x32RoundingUp(
      powerOfTenRoundedUp,
      upperMidpointExact)
    let bias = UInt64((1 &<< (integerBits &- extraBits)) &- 1)
    u = (u1 &+ bias) >> (integerBits &- extraBits)
    let l1 = _multiply64x32RoundingDown(
      powerOfTenRoundedDown,
      lowerMidpointExact)
    l = l1 >> (integerBits &- extraBits)
  }

  // Step 6: Align first digit, adjust exponent

  while u < (UInt64(1) &<< fractionBits) {
    base10Exponent &-= 1
    l &*= 10
    u &*= 10
  }

  // Step 7: Generate decimal digits into the destination buffer

  var t = u
  var delta = u &- l
  let fractionMask: UInt64 = (1 << fractionBits) - 1

  // Overwrite the first digit at index 7:
  let firstDigit = 7
  let digit = (t >> fractionBits) &+ 0x30
  t &= fractionMask
  unsafe buffer.storeBytes(
    of: UInt8(truncatingIfNeeded: digit),
    toUncheckedByteOffset: firstDigit,
    as: UInt8.self)
  var nextDigit = firstDigit &+ 1

  // Generate 2 digits at a time...
  while (delta &* 10) < ((t &* 10) & fractionMask) {
    delta &*= 100
    t &*= 100
    let d12 = Int(truncatingIfNeeded: t >> fractionBits)
    let text = unsafe asciiDigitTable[unchecked: d12]
    unsafe buffer.storeBytes(
      of: text,
      toUncheckedByteOffset: nextDigit,
      as: UInt16.self)
    nextDigit &+= 2
    t &= fractionMask
  }

  // ... and a final single digit, if necessary
  if delta < t {
    delta &*= 10
    t &*= 10
    let text = 0x30 + UInt8(truncatingIfNeeded: t >> fractionBits)
    unsafe buffer.storeBytes(
      of: text,
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    t &= fractionMask
  }

  // Adjust the final digit to be closer to the original value
  let isBoundary = (f.significandBitPattern == 0)
  if delta > t &+ (1 << fractionBits) {
    let skew: UInt64
    if isBoundary {
      skew = delta &- delta / 3 &- t
    } else {
      skew = delta / 2 &- t
    }
    let one = UInt64(1) << (64 - integerBits)
    let lastAccurateBit = UInt64(1) << 24
    let fractionMask = (one - 1) & ~(lastAccurateBit - 1)
    let oneHalf = one >> 1
    var lastDigit = unsafe buffer.unsafeLoad(
      fromUncheckedByteOffset: nextDigit &- 1,
      as: UInt8.self)
    if ((skew &+ (lastAccurateBit >> 1)) & fractionMask) == oneHalf {
      // Skew is integer + 1/2, round even after adjustment
      let adjust = skew >> (64 - integerBits)
      lastDigit &-= UInt8(truncatingIfNeeded: adjust)
      lastDigit &= ~1
    } else {
      // Round nearest
      let adjust = (skew &+ oneHalf) >> (64 - integerBits)
      lastDigit &-= UInt8(truncatingIfNeeded: adjust)
    }
    unsafe buffer.storeBytes(
      of: lastDigit,
      toUncheckedByteOffset: nextDigit &- 1,
      as: UInt8.self)
  }

  // Step 8: Finish formatting
  let forceExponential =
    ((binaryExponent > 25)
       || (binaryExponent == 25 && !isBoundary))
  return _finishFormatting(
    buffer: &buffer,
    sign: f.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Exponent: base10Exponent)
}

// ================================================================
//
// Float64
//
// ================================================================

// Support Legacy ABI on top of new implementation
@_silgen_name("swift_float64ToString")
@usableFromInline
internal func _float64ToStringImpl(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float64,
  _ debug: Bool
) -> UInt64 {
  if #available(SwiftStdlib 6.2, *) {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
      _unchecked: textBuffer,
      count: Int(bufferLength))
    let textRange = _Float64ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
      unsafe _memmove(
        dest: textBuffer,
        src: textBuffer + textRange.lowerBound,
        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
  } else {
    fatalError()
  }
}

// Convert a Float64 to an optimal ASCII representation.
// See notes above for comments on the output format here.
// The algorithm is extensively commented inline; the comments
// at the top of this source file give additional context.
// Inputs:
// * `value`: Float64 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 32 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`
@available(SwiftStdlib 6.2, *)
internal func _Float64ToASCII(
  value d: Float64,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // We need a MutableRawSpan in order to use wide store/load operations
  precondition(utf8Buffer.count >= 32)
  var buffer = utf8Buffer.mutableBytes

  //
  // Step 1: Handle the special cases, decompose the input
  //
  let binaryExponent: Int
  let significand: Double.RawSignificand
  let exponentBias = 1022 // (1 << (Double.exponentBitCount - 1)) - 2

  if (d.exponentBitPattern == 0x7ff) {
    if (d.isInfinite) {
      return _infinity(buffer: &buffer, sign: d.sign)
    } else { // d.isNaN
      let quietBit =
        (d.significandBitPattern >> (Double.significandBitCount - 1)) & 1
      let payloadMask = (UInt64(1) &<< (Double.significandBitCount - 2)) - 1
      let payload64 = d.significandBitPattern & payloadMask
      return nan_details(
        buffer: &buffer,
        sign: d.sign,
        quiet: quietBit != 0,
        payloadHigh: 0,
        payloadLow: UInt64(truncatingIfNeeded:payload64))
    }
  } else if (d.exponentBitPattern == 0) {
    if (d.isZero) {
      return _zero(buffer: &buffer, sign: d.sign)
    } else { // d.isSubnormal
      binaryExponent = 1 - exponentBias
      significand = d.significandBitPattern &<< Double.exponentBitCount
    }
  } else {
    binaryExponent = Int(d.exponentBitPattern) &- exponentBias
    significand =
      ((d.significandBitPattern &+ (1 << Double.significandBitCount))
         &<< Double.exponentBitCount)
  }
  // The input has been decomposed as significand * 2^binaryExponent,
  // where `significand` is a 64-bit fraction with the binary
  // point at the far left.

  // Step 2: Determine the exact unscaled target interval

  // Grisu-style algorithms construct the shortest decimal digit
  // sequence within a specific interval.  To build the appropriate
  // interval, we start by computing the midpoints between this
  // floating-point value and the adjacent ones.  Note that this
  // step is an exact computation.

  let halfUlp: Double.RawSignificand = 1 << (Double.exponentBitCount - 1)
  let quarterUlp = halfUlp >> 1
  let upperMidpointExact = significand &+ halfUlp
  let lowerMidpointExact =
    significand &- ((d.significandBitPattern == 0) ? quarterUlp : halfUlp)
  let isOddSignificand = ((d.significandBitPattern & 1) != 0)

  // Step 3: Estimate the base 10 exponent

  // Grisu algorithms are based in part on a simple technique for
  // generating a base-10 form for a binary floating-point number.
  // Start with a binary floating-point number `f * 2^e` and then
  // estimate the decimal exponent `p`. You can then rewrite your
  // original number as:
  //
  // ```
  //     f * 2^e * 10^-p * 10^p
  // ```
  //
  // The last term is part of our output, and a good estimate for
  // `p` will ensure that `2^e * 10^-p` is close to 1.  Multiplying
  // the first three terms then yields a fraction suitable for
  // producing the decimal digits.  Here we use a very fast estimate
  // of `p` that is never off by more than 1; we'll have
  // opportunities later to correct any error.

  var base10Exponent = decimalExponentFor2ToThe(binaryExponent)

  // Step 4: Compute power-of-10 scale factor

  // Compute `10^-p` to 128-bit precision.  We generate
  // both over- and under-estimates to ensure we can exactly
  // bound the later use of these values.
  // The `powerOfTenRounded{Up,Down}` values are 128-bit
  // pure fractions with the decimal point at the far left.

  var powerOfTenRoundedDown: UInt128 = 0
  var powerOfTenRoundedUp: UInt128 = 0

  // Note the extra factor of 10^bulkFirstDigits -- that will give
  // us a headstart on digit generation later on.  (In contrast, Ryu
  // uses an extra factor of 10^17 here to get all the digits up
  // front, but then has to back out any extra digits.  Doing that
  // with a 17-digit value requires 64-bit division, which is the
  // root cause of Ryu's poor performance on 32-bit processors.  We
  // also might have to back out extra digits if 7 is too many, but
  // will only need 32-bit division in that case.)

  let bulkFirstDigits = 7
  let bulkFirstDigitFactor: UInt32 = 1000000 // 10^(bulkFirstDigits - 1)

  let powerOfTenExponent = _intervalContainingPowerOf10_Binary64(
    p: -base10Exponent &+ bulkFirstDigits &- 1,
    lower: &powerOfTenRoundedDown,
    upper: &powerOfTenRoundedUp)

  let extraBits = binaryExponent + powerOfTenExponent

  // Step 5: Scale the interval (with rounding)

  // As mentioned above, the final digit generation works
  // with an interval, so we actually apply the scaling
  // to the upper and lower midpoint values separately.

  // As part of the scaling here, we'll switch from a pure
  // fraction with zero bit integer portion and 128-bit fraction
  // to a fixed-point form with 32 bits in the integer portion.

  let integerBits = 32
  let roundingBias =
    UInt128((1 &<< UInt64(truncatingIfNeeded: integerBits &- extraBits)) &- 1)
  var u: UInt128
  var l: UInt128
  if isOddSignificand {
    // Case A: Narrow the interval (odd significand)

    // Loitsch' original Grisu2 always rounds so as to narrow the
    // interval.  Since our digit generation will select a value
    // within the scaled interval, narrowing the interval
    // guarantees that we will find a digit sequence that converts
    // back to the original value.

    // This ensures accuracy but, as explained in Loitsch' paper,
    // this carries a risk that there will be a shorter digit
    // sequence outside of our narrowed interval that we will
    // miss. This risk obviously gets lower with increased
    // precision, but it wasn't until the Errol paper that anyone
    // had a good way to test whether a particular implementation
    // had sufficient precision. That paper shows a way to enumerate
    // the worst-case numbers; those numbers that are extremely close
    // to the mid-points between adjacent floating-point values.
    // These are the values that might sit just outside of the
    // narrowed interval. By testing these values, we can verify
    // the correctness of our implementation.

    // Multiply out the upper midpoint, rounding down...
    let u1 = _multiply128x64RoundingDown(
      powerOfTenRoundedDown,
      upperMidpointExact)
    // Account for residual binary exponent and adjust
    // to the fixed-point format
    u = u1 >> (integerBits - extraBits)

    // Conversely for the lower midpoint...
    let l1 = _multiply128x64RoundingUp(
      powerOfTenRoundedUp,
      lowerMidpointExact)
    l = (l1 + roundingBias) >> (integerBits - extraBits)
  } else {
    // Case B: Widen the interval (even significand)

    // As explained in Errol Theorem 6, in certain cases there is
    // a short decimal representation at the exact boundary of the
    // scaled interval.  When such a number is converted back to
    // binary, it will get rounded to the adjacent even
    // significand.

    // So when the significand is even, we round so as to widen
    // the interval in order to ensure that the exact midpoints
    // are considered.  Of couse, this ensures that we find a
    // short result but carries a risk of selecting a result
    // outside of the exact scaled interval (which would be
    // inaccurate).
    // (This technique of rounding differently for even/odd significands
    // seems to be new; I've not seen it described in any of the
    // papers on floating-point printing.)

    // The same testing approach described above (based on results
    // in the Errol paper) also applies
    // to this case.

    let u1 = _multiply128x64RoundingUp(
      powerOfTenRoundedUp,
      upperMidpointExact)
    u = (u1 &+ roundingBias) >> (integerBits - extraBits)
    let l1 = _multiply128x64RoundingDown(
      powerOfTenRoundedDown,
      lowerMidpointExact)
    l = l1 >> (integerBits - extraBits)
  }

  // Step 6: Align the first digit, adjust exponent

  // Calculations above used an estimate for the power-of-ten scale.
  // Here, we compensate for any error in that estimate by testing
  // whether we have the expected number of digits in the integer
  // portion and correcting as necessary.  This also serves to
  // prune leading zeros from subnormals.

  // Except for subnormals, this loop never runs more than once.
  // For subnormals, this might run as many as 16 times.
  let minimumU = UInt128(bulkFirstDigitFactor) << (128 - integerBits)
  while u < minimumU {
    base10Exponent -= 1
    l &*= 10
    u &*= 10
  }

  // Step 7: Produce decimal digits

  // One standard approach generates digits for the scaled upper and
  // lower boundaries and stops at the first digit that
  // differs. For example, note that 0.1234 is the shortest decimal
  // between u = 0.123456 and l = 0.123345.

  // Grisu optimizes this by generating digits for the upper bound
  // (multiplying by 10 to isolate each digit) while simultaneously
  // scaling the interval width `delta`.  As we remove each digit
  // from the upper bound, the remainder is the difference between
  // the base-10 value generated so far and the true upper bound.
  // When that remainder is less than the scaled width of the
  // interval, we know the current digits specify a value within the
  // target interval.

  // The logic below actually blends three different digit-generation
  // strategies:
  // * The first digits are already in the integer portion of the
  //   fixed-point value, thanks to the `bulkFirstDigits` factor above.
  //   We can just break those down and write them out.
  // * If we generated too many digits, we use a Ryu-inspired technique
  //   to backtrack.
  // * If we generated too few digits (the usual case), we use an
  //   optimized form of the Grisu2 method to produce the remaining
  //   values.

  //
  // Generate digits and build the output.
  //

  // Generate digits for `t` with interval width `delta = u - l`
  // As above, these are fixed-point with 32-bit integer, 96-bit fraction
  var t = u
  var delta = u &- l
  let fractionMask = (UInt128(1) << 96) - 1

  var nextDigit = 5
  var firstDigit = nextDigit

  // Our initial scaling gave us the first 7 digits already:
  let d12345678 = UInt32(truncatingIfNeeded: t._high >> 32)
  t &= fractionMask

  if delta >= t {
    // Oops!  We have too many digits.  Back out the extra ones to
    // get the right answer.  This is similar to Ryu, but since
    // we've only produced seven digits, we only need 32-bit
    // arithmetic here.  (Ryu needs 64-bit arithmetic to back out
    // digits, which severely compromises performance on 32-bit
    // processors.  The same problem occurs with Ryu for 128-bit
    // floats on 64-bit processors.)
    // A few notes:
    // * Our target hardware always supports 32-bit hardware division,
    //   so this should be reasonably fast.
    // * For small integers (like "2.0"), Ryu would have to back out 16
    //   digits; we only have to back out 6.
    // * Very few double-precision values actually need fewer than 7
    //   digits.  So this is rarely used except in workloads that
    //   specifically use double for small integers.

    // Why this is critical for performance: In order to use the
    // 8-digits-at-a-time optimization below, we need at least 30
    // bits in the integer part of our fixed-point format above.
    // If we only use bulkDigits = 1, that leaves only 128 - 30 =
    // 98 bit accuracy for our scaling step, which isn't enough
    // (experiments suggest that binary64 needs ~110 bits for
    // correctness).  So we have to use a large bulkDigits value
    // to make full use of the 128-bit scaling above, which forces
    // us to have some form of logic to handle the case of too
    // many digits.  The alternatives are either to use >128 bit
    // arithmetic, or to back up and repeat the original scaling
    // with bulkDigits = 1.

    let uHigh = u._high
    let lHigh = (l &+ UInt128(UInt64.max))._high
    let tHigh: UInt64
    if d.significand == 0 {
      tHigh = (uHigh &+ lHigh &* 2) / 3
    } else {
      tHigh = (uHigh &+ lHigh) / 2
    }
    var u0 = UInt32(truncatingIfNeeded: uHigh >> (64 - integerBits))
    var l0 = UInt32(truncatingIfNeeded: lHigh >> (64 - integerBits))
    if lHigh & ((1 << (64 - integerBits)) - 1) != 0 {
      l0 &+= 1
    }
    var t0 = UInt32(truncatingIfNeeded: tHigh >> (64 - integerBits))
    var t0digits = 8

    var u1 = u0 / 10
    var l1 = (l0 &+ 9) / 10
    var trailingZeros = (t == 0)
    var droppedDigit = UInt32(
      truncatingIfNeeded: ((tHigh &* 10) >> (64 - integerBits)) % 10)
    while u1 >= l1 && u1 != 0 {
      u0 = u1
      l0 = l1
      trailingZeros = trailingZeros && (droppedDigit == 0)
      droppedDigit = t0 % 10
      t0 /= 10
      t0digits -= 1
      u1 = u0 / 10
      l1 = (l0 &+ 9) / 10
    }
    // Correct the final digit
    if droppedDigit > 5 || (droppedDigit == 5 && !trailingZeros) { // > 0.5000
      t0 &+= 1
    } else if droppedDigit == 5 && trailingZeros { // == 0.5000
      t0 &+= 1
      t0 &= ~1
    }
    // t0 has t0digits digits.  Write them out
    let text = _intToEightDigits(t0)
    buffer.storeBytes(
      of: text,
      toByteOffset: nextDigit,
      as: UInt64.self)
    nextDigit &+= 8
    // Skip the leading zeros
    firstDigit &+= 9 - t0digits
  } else {
    // Our initial scaling did not produce too many digits.  The
    // `d12345678` value holds the first 7 digits (plus a leading
    // zero).  The remainder of this algorithm is basically just a
    // heavily-optimized variation of Grisu2.

    // Write out exactly 8 digits, assuming little-endian.
    let chars = _intToEightDigits(d12345678)
    unsafe buffer.storeBytes(
      of: chars,
      toUncheckedByteOffset: nextDigit,
      as: UInt64.self)
    nextDigit &+= 8
    firstDigit &+= 1

    // >90% of random binary64 values need at least 15 digits.
    // We have seven so there's probably at least 8 more, which
    // we can grab all at once.
    let TenToTheEighth = 100000000 as UInt128 // 10^(15-bulkFirstDigits)
    let d0 = delta * TenToTheEighth
    var t0 = t * TenToTheEighth
    // The integer part of t0 is the next 8 digits
    let next8Digits = UInt32(truncatingIfNeeded: t0._high >> 32)
    t0 &= fractionMask
    if d0 < t0 {
      // We got 8 more digits! (So number is at least 15 digits)
      // Write them out:
      let chars = _intToEightDigits(next8Digits)
      unsafe buffer.storeBytes(
        of: chars,
        toUncheckedByteOffset: nextDigit,
        as: UInt64.self)
      nextDigit &+= 8
      t = t0
      delta = d0
    }

    // Generate remaining digits one at a time, following Grisu:
    while (delta < t) {
      delta &*= 10
      t &*= 10
      unsafe buffer.storeBytes(
        of: UInt8(truncatingIfNeeded: t._high >> 32) &+ 0x30,
        toUncheckedByteOffset: nextDigit,
        as: UInt8.self)
      nextDigit &+= 1
      t &= fractionMask
    }

    // Adjust the final digit to be closer to the original value.
    // This accounts for the fact that sometimes there is more than
    // one shortest digit sequence.

    // For example, consider how the above would work if you had the
    // value 0.1234 and computed u = 0.1257, l = 0.1211.  The above
    // digit generation works with `u`, so produces 0.125.  But the
    // values 0.122, 0.123, and 0.124 are just as short and 0.123 is
    // therefore the best choice, since it's closest to the original
    // value.

    // We know delta and t are both less than 10.0 here, so we can
    // shed some excess integer bits to simplify the following:
    let adjustIntegerBits = 4 // Integer bits for "adjust" phase
    let deltaHigh64 = UInt64(
      truncatingIfNeeded: delta >> (64 - integerBits + adjustIntegerBits))
    let tHigh64 = UInt64(
      truncatingIfNeeded: t >> (64 - integerBits + adjustIntegerBits))

    let one = UInt64(1) << (64 - adjustIntegerBits)
    let adjustFractionMask = one - 1
    let oneHalf = one >> 1
    if deltaHigh64 >= tHigh64 &+ one {
      // The `skew` is the difference between our
      // computed digits and the original exact value.
      var skew: UInt64
      if (d.significandBitPattern == 0) {
        skew = deltaHigh64 &- deltaHigh64 / 3 &- tHigh64
      } else {
        skew = deltaHigh64 / 2 &- tHigh64
      }

      var lastDigit = unsafe buffer.unsafeLoad(
        fromUncheckedByteOffset: nextDigit - 1,
        as: UInt8.self)

      // We use the `skew` to figure out whether there's
      // a better base-10 value than our current one.
      if (skew & adjustFractionMask) == oneHalf {
        // Difference is an integer + exactly 1/2, so ...
        let adjust = skew >> (64 - adjustIntegerBits)
        lastDigit &-= UInt8(truncatingIfNeeded: adjust)
        // ... we round the last digit even.
        lastDigit &= ~1
      } else {
        let adjust = (skew + oneHalf) >> (64 - adjustIntegerBits)
        lastDigit &-= UInt8(truncatingIfNeeded: adjust)
      }
      buffer.storeBytes(
        of: lastDigit,
        toByteOffset: nextDigit - 1,
        as: UInt8.self)
    }
  }

  // Step 8: Finalize formatting by rearranging
  // the digits and filling in decimal points,
  // exponents, and zero padding.
  let isBoundary = (d.significandBitPattern == 0)
  let forceExponential =
    ((binaryExponent > 54) || (binaryExponent == 54 && !isBoundary))
  return _finishFormatting(
    buffer: &buffer,
    sign: d.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Exponent: base10Exponent)
}


// ================================================================
//
// Float80
//
// ================================================================

// Float80 is only available on Intel x86/x86_64 processors on certain operating systems
// This matches the condition for the Float80 type

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS)))) && (arch(i386) || arch(x86_64))

// Support Legacy ABI on top of new implementation
@_silgen_name("swift_float80ToString")
@usableFromInline
internal func _float80ToStringImpl(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float80,
  _ debug: Bool
) -> UInt64 {
  if #available(SwiftStdlib 6.2, *) {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
      _unchecked: textBuffer,
      count: Int(bufferLength))
    let textRange = _Float80ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
      unsafe _memmove(
        dest: textBuffer,
        src: textBuffer + textRange.lowerBound,
        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
  } else {
    fatalError()
  }
}

// Convert a Float80 to an optimal ASCII representation.
// See notes above for comments on the output format here.
// See _Float64ToASCII for comments on the algorithm.
// Inputs:
// * `value`: Float80 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 32 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`
@available(SwiftStdlib 6.2, *)
internal func _Float80ToASCII(
  value f: Float80,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // We need a MutableRawSpan in order to use wide store/load operations
  precondition(utf8Buffer.count >= 32)
  var buffer = utf8Buffer.mutableBytes

  // Step 1: Handle special cases, decompose the input

  // The Intel 80-bit floating point format has some quirks that
  // make this a lot more complex than the corresponding logic for
  // the IEEE 754 portable formats.

  // f.significandBitPattern is processed to try to mimic the
  // semantics of IEEE portable formats.  But for the following,
  // we need the actual raw bits:
  let rawSignificand = f._representation.explicitSignificand
  let binaryExponent: Int
  let significand: Float80.RawSignificand
  let exponentBias = (1 << (Float80.exponentBitCount - 1)) - 2 // 16382
  let isBoundary = f.significandBitPattern == 0
  if f.exponentBitPattern == 0x7fff { // NaN or Infinity
    // 80387 semantics and 80287 semantics differ somewhat;
    // we follow 80387 semantics here.
    // See: Wikipedia.org "Extended Precision"
    // See: Intel's "Floating Point Reference Sheet"
    // https://software.intel.com/content/dam/develop/external/us/en/documents/floating-point-reference-sheet.pdf
    let selector = rawSignificand >> 62
    let payload = rawSignificand & ((1 << 62) - 1)
    switch selector {
    case 0: // ∞ or snan on 287, invalid on 387
      fallthrough
    case 1: // Pseudo-NaN: snan on 287, invalid on 387
      // Invalid patterns treated as plain "nan"
      return nan_details(
        buffer: &buffer,
        sign: .plus,
        quiet: true,
        payloadHigh: 0,
        payloadLow: payload)
    case 2:
      if payload == 0 {  // snan on 287, ∞ on 387
        return _infinity(buffer: &buffer, sign: f.sign)
      } else { // snan on 287 and 387
        return nan_details(
          buffer: &buffer,
          sign: f.sign,
          quiet: false,
          payloadHigh: 0,
          payloadLow: payload)
      }
    case 3:
      // Zero payload and sign bit set is "indefinite" (treated as qNaN here),
      // otherwise qNaN on 387, sNaN on 287
      return nan_details(
        buffer: &buffer,
        sign: f.sign,
        quiet: true,
        payloadHigh: 0,
        payloadLow: payload)
    default:
      fatalError()
    }
  } else if f.exponentBitPattern == 0 {
    if rawSignificand == 0 { // Zero
      return _zero(buffer: &buffer, sign: f.sign)
    } else { // subnormal
      binaryExponent = 1 - exponentBias
      significand = rawSignificand
    }
  } else if rawSignificand >> 63 == 1 { // Normal
    binaryExponent = Int(bitPattern:f.exponentBitPattern) - exponentBias
    significand = rawSignificand
  } else {
    return nan_details(
      buffer: &buffer,
      sign: .plus,
      quiet: true,
      payloadHigh: 0,
      payloadLow: 0)
  }

  // Step 2: Determine the exact unscaled target interval
  let halfUlp = UInt64(1) << 63
  let quarterUlp = halfUlp >> 1
  let threeQuarterUlp = halfUlp + quarterUlp
  // Significand is the upper 64 bits of our 128-bit franction
  // Upper midpoint adds 1/2 ULP:
  let upperMidpointExact = UInt128(_low: halfUlp, _high: significand)
  // Lower midpoint subtracts 1 ULP and then adds 1/2 or 3/4 ULP:
  let lowerMidpointExact = UInt128(
    _low: isBoundary ? threeQuarterUlp : halfUlp,
    _high: significand - 1)

  let forceExponential =
    (binaryExponent > 65
       || (binaryExponent == 65 && !isBoundary))
  return _backend_256bit(
    buffer: &buffer,
    upperMidpointExact: upperMidpointExact,
    lowerMidpointExact: lowerMidpointExact,
    sign: f.sign,
    isBoundary: isBoundary,
    isOddSignificand: (f.significandBitPattern & 1) != 0,
    binaryExponent: binaryExponent,
    forceExponential: forceExponential)
}
#endif

// ================================================================
//
// Float128
//
// ================================================================

#if false
// Note: We don't need _float128ToStringImpl, since that's only for
// backwards compatibility, and the legacy ABI never supported
// Float128.

@available(SwiftStdlib 6.2, *)
internal func _Float128ToASCII(
  value d: Float128,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // TODO:  Write Me!

  // Note: All the interesting parts are already implemented in _backend_256bit(...),
  // so this can easily be implemented someday by just copyihng _Float80ToASCII
  // and making the obvious changes.  (See the introductory parts of
  // _Float64ToASCII for the structure common to all IEEE 754 formats.)
}
#endif

// ================================================================
//
// Float80/Float128 common backend
//
// This uses 256-bit fixed-width arithmetic to efficiently compute the
// optimal form for a decomposed float80 or binary128 value.  It is
// less heavily commented than the 128-bit Double implementation
// above; see that implementation for detailed explanation of the
// logic here.
//
// Float80 could be handled more efficiently with 192-bit fixed-width
// arithmetic.  But the code size savings from sharing this logic
// between float80 and binary128 are substantial, and the resulting
// float80 performance is still much better than competing
// implementations.
//
// Also in the interest of code size savings, this eschews some of the
// optimizations used by the 128-bit Double implementation above.
// Those optimizations are simple to reintroduce if you're interested
// in further performance improvements.
//
// If you are interested in extreme code size, you can also use this
// backend for binary32 and binary64, eliminating the separate 128-bit
// implementation. That variation offers surprisingly reasonable
// performance overall.
//
// ================================================================

#if !(os(Windows) || os(Android) || ($Embedded && !os(Linux) && !(os(macOS) || os(iOS) || os(watchOS) || os(tvOS)))) && (arch(i386) || arch(x86_64))

@available(SwiftStdlib 6.2, *)
fileprivate func _backend_256bit(
  buffer: inout MutableRawSpan,
  upperMidpointExact: UInt128,
  lowerMidpointExact: UInt128,
  sign: FloatingPointSign,
  isBoundary: Bool,
  isOddSignificand: Bool,
  binaryExponent: Int,
  forceExponential: Bool
) -> Range<Int> {

  // Step 3: Estimate the base 10 exponent
  var base10Exponent = decimalExponentFor2ToThe(binaryExponent)

  // Step 4: Compute a power-of-10 scale factor
  var powerOfTenRoundedDown = _UInt256()
  var powerOfTenRoundedUp = _UInt256()
  let powerOfTenExponent = _intervalContainingPowerOf10_Binary128(
    p: -base10Exponent,
    lower: &powerOfTenRoundedDown,
    upper: &powerOfTenRoundedUp)
  let extraBits = binaryExponent &+ powerOfTenExponent

  // Step 5: Scale the interval (with rounding)
  let integerBits = 14
  let high64FractionBits = 64 - integerBits
  var u: _UInt256
  var l: _UInt256
  if isOddSignificand {
    // Narrow the interval (odd significand)
    u = powerOfTenRoundedDown
    u.multiplyRoundingDown(by: upperMidpointExact)
    u.shiftRightRoundingDown(by: integerBits &- extraBits)

    l = powerOfTenRoundedUp
    l.multiplyRoundingUp(by: lowerMidpointExact)
    l.shiftRightRoundingUp(by: integerBits &- extraBits)
  } else {
    // Widen the interval (even significand)
    u = powerOfTenRoundedUp
    u.multiplyRoundingUp(by: upperMidpointExact)
    u.shiftRightRoundingUp(by: integerBits &- extraBits)

    l = powerOfTenRoundedDown
    l.multiplyRoundingDown(by: lowerMidpointExact)
    l.shiftRightRoundingDown(by: integerBits &- extraBits)
  }

  // Step 6: Align first digit, adjust exponent
  while u.high._high < (UInt64(1) << high64FractionBits) {
    base10Exponent &-= 1
    l.multiply(by: UInt32(10))
    u.multiply(by: UInt32(10))
  }
  var t = u
  var delta = u &- l

  // Step 7: Generate digits

  // Leave 8 bytes at the beginning for finishFormatting to use
  let firstDigit = 8
  var nextDigit = firstDigit
  buffer.storeBytes(
    of: 0x30 + UInt8(truncatingIfNeeded: t.extractIntegerPart(integerBits)),
    toByteOffset: nextDigit,
    as: UInt8.self)
  nextDigit &+= 1

  // It would be nice to generate 8 digits at a time and take
  // advantage of intToEightDigits, but our integer portion has only
  // 14 bits.  We can't make that bigger without either sacrificing
  // too much precision for correct Float128 or folding the first
  // digits into the scaling (as we do with Double) which would
  // require a back-out phase here (as we do with Double).

  // If there is at least one more digit possible...
  if delta < t {

    // Try grabbing four digits at a time
    var d0 = delta
    var t0 = t
    d0.multiply(by: 10000)
    t0.multiply(by: 10000)
    var d1234 = t0.extractIntegerPart(integerBits)
    while d0 < t0 {
      let d12 = d1234 / 100
      let d34 = d1234 % 100
      unsafe buffer.storeBytes(
        of: asciiDigitTable[Int(bitPattern:d12)],
        toUncheckedByteOffset: nextDigit,
        as: UInt16.self)
      unsafe buffer.storeBytes(
        of: asciiDigitTable[Int(bitPattern:d34)],
        toUncheckedByteOffset: nextDigit &+ 2,
        as: UInt16.self)
      nextDigit &+= 4
      t = t0
      delta = d0
      d0.multiply(by: 10000)
      t0.multiply(by: 10000)
      d1234 = t0.extractIntegerPart(integerBits)
    }

    // Finish by generating one digit at a time...
    while delta < t {
      delta.multiply(by: UInt32(10))
      t.multiply(by: UInt32(10))
      let digit = UInt8(truncatingIfNeeded: t.extractIntegerPart(integerBits))
      unsafe buffer.storeBytes(
        of: 0x30 &+ digit,
        toUncheckedByteOffset: nextDigit,
        as: UInt8.self)
      nextDigit &+= 1
    }
  }

  // Adjust the final digit to be closer to the original value
  // We've already consumed most of our available precision, and only
  // need a couple of integer bits, so we can narrow down to
  // 64 bits here.
  let deltaHigh64 = delta.high._high
  let tHigh64 = t.high._high
  if deltaHigh64 >= tHigh64 &+ (UInt64(1) << high64FractionBits) {
    let skew: UInt64
    if isBoundary {
      skew = deltaHigh64 &- deltaHigh64 / 3 &- tHigh64
    } else {
      skew = deltaHigh64 / 2 &- tHigh64
    }
    let one = UInt64(1) << high64FractionBits
    let fractionMask = one - 1
    let oneHalf = one >> 1
    var lastDigit = unsafe buffer.unsafeLoad(
      fromUncheckedByteOffset: nextDigit &- 1,
      as: UInt8.self)
    if (skew & fractionMask) == oneHalf {
      let adjust = skew >> high64FractionBits
      lastDigit &-= UInt8(truncatingIfNeeded: adjust)
      lastDigit &= ~1
    } else {
      let adjust = (skew + oneHalf) >> high64FractionBits
      lastDigit &-= UInt8(truncatingIfNeeded: adjust)
    }
    buffer.storeBytes(
      of: lastDigit,
      toByteOffset: nextDigit &- 1,
      as: UInt8.self)
  }

  return _finishFormatting(
    buffer: &buffer,
    sign: sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Exponent: base10Exponent)
}
#endif

// ================================================================
//
// Common Helper functions
//
// ================================================================

// Code above computes the appropriate significant digits and stores
// them in `buffer` between `firstDigit` and `nextDigit`.
// `finishFormatting` converts this into the final text form,
// inserting decimal points, minus signs, exponents, etc, as
// necessary.  To minimize the work here, this assumes that there are
// at least 5 unused bytes at the beginning of `buffer` before
// `firstDigit` and that all unused bytes are filled with `"0"` (0x30)
// characters.

@available(SwiftStdlib 6.2, *)
fileprivate func _finishFormatting(
  buffer: inout MutableRawSpan,
  sign: FloatingPointSign,
  firstDigit: Int,
  nextDigit: Int,
  forceExponential: Bool,
  base10Exponent: Int
) -> Range<Int> {
  // Performance note: This could be made noticeably faster by
  // writing the output consistently in exponential form with no
  // decimal point, e.g., "31415926e-07".  But the extra cost seems
  // worthwhile to achieve "3.1415926" instead.
  var firstDigit = firstDigit
  var nextDigit = nextDigit

  let digitCount = nextDigit &- firstDigit
  if base10Exponent < -4 || forceExponential {
    // Exponential form: "-1.23456789e+123"
    // Rewrite "123456789" => "1.23456789" by moving the first
    // digit to the left one byte and overwriting a period.
    // (This is one reason we left empty space to the left of the digits.)
    // We don't do this for single-digit significands: "1e+78", "5e-324"
    if digitCount > 1 {
      let t = unsafe buffer.unsafeLoad(
        fromUncheckedByteOffset: firstDigit,
        as: UInt8.self)
      unsafe buffer.storeBytes(
        of: 0x2e,
        toUncheckedByteOffset: firstDigit,
        as: UInt8.self)
      firstDigit &-= 1
      unsafe buffer.storeBytes(
        of: t,
        toUncheckedByteOffset: firstDigit,
        as: UInt8.self)
    }
    // Append the exponent:
    unsafe buffer.storeBytes(
      of: 0x65, // "e"
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    var e = base10Exponent
    let expSign: UInt8
    if base10Exponent < 0 {
      expSign = 0x2d // "-"
      e = 0 &- e
    } else {
      expSign = 0x2b // "+"
    }
    unsafe buffer.storeBytes(
      of: expSign,
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    if e > 99 {
      if e > 999 {
        let d = asciiDigitTable[e / 100]
        unsafe buffer.storeBytes(
          of: d,
          toUncheckedByteOffset: nextDigit,
          as: UInt16.self)
        nextDigit &+= 2
      } else {
        let d = 0x30 &+ UInt8(truncatingIfNeeded: (e / 100))
        unsafe buffer.storeBytes(
          of: d,
          toUncheckedByteOffset: nextDigit,
          as: UInt8.self)
        nextDigit &+= 1
      }
      e = e % 100
    }
    let d = unsafe asciiDigitTable[unchecked: e]
    buffer.storeBytes(
      of: d,
      toByteOffset: nextDigit,
      as: UInt16.self)
    nextDigit &+= 2
    } else if base10Exponent < 0 {
    // "-0.000123456789"
    // We need up to 5 leading characters before the digits.
    // Note that the formatters above all insert extra leading "0" characters
    // to the beginning of the buffer, so we don't need to memset() here,
    // just back up the start to include them...
    firstDigit &+= base10Exponent - 1
    // ... and then overwrite a decimal point to get "0." at the beginning
    buffer.storeBytes(
      of: 0x2e, // "."
      toByteOffset: firstDigit &+ 1,
      as: UInt8.self)
  } else if base10Exponent &+ 1 < digitCount {
    // "123456.789"
    // We move the first digits forward one position
    // so we can insert a decimal point in the middle.
    // Note: This is the only case where we actually move
    // more than one digit around in the buffer.
    // TODO: Find out how to use C memmove() here
    firstDigit &-= 1
    for i in 0...(base10Exponent &+ 1) {
      let t = unsafe buffer.unsafeLoad(
        fromUncheckedByteOffset: firstDigit &+ i &+ 1,
        as: UInt8.self)
      unsafe buffer.storeBytes(
        of: t,
        toUncheckedByteOffset: firstDigit &+ i,
        as: UInt8.self)
    }
    buffer.storeBytes(
      of: 0x2e,
      toByteOffset: firstDigit &+ base10Exponent &+ 1,
      as: UInt8.self)
  } else {
    // "12345678900.0"
    // Fill trailing zeros, put ".0" at the end
    // so the result is obviously floating-point.
    // Remember buffer was initialized with "0"
    nextDigit = firstDigit &+ base10Exponent &+ 3
    buffer.storeBytes(
      of: 0x2e,
      toByteOffset: nextDigit &- 2,
      as: UInt8.self)
  }
  if sign == .minus {
    buffer.storeBytes(
      of: 0x2d, // "-"
      toByteOffset: firstDigit &- 1,
      as: UInt8.self)
    firstDigit &-= 1
  }

  return unsafe Range(_uncheckedBounds: (lower: firstDigit, upper: nextDigit))
}

// Table with ASCII strings for all 2-digit decimal numbers.
// Stored as little-endian UInt16s for efficiency
@available(SwiftStdlib 6.2, *)
fileprivate let asciiDigitTable: InlineArray<100, UInt16> = [
  0x3030, 0x3130, 0x3230, 0x3330, 0x3430,
  0x3530, 0x3630, 0x3730, 0x3830, 0x3930,
  0x3031, 0x3131, 0x3231, 0x3331, 0x3431,
  0x3531, 0x3631, 0x3731, 0x3831, 0x3931,
  0x3032, 0x3132, 0x3232, 0x3332, 0x3432,
  0x3532, 0x3632, 0x3732, 0x3832, 0x3932,
  0x3033, 0x3133, 0x3233, 0x3333, 0x3433,
  0x3533, 0x3633, 0x3733, 0x3833, 0x3933,
  0x3034, 0x3134, 0x3234, 0x3334, 0x3434,
  0x3534, 0x3634, 0x3734, 0x3834, 0x3934,
  0x3035, 0x3135, 0x3235, 0x3335, 0x3435,
  0x3535, 0x3635, 0x3735, 0x3835, 0x3935,
  0x3036, 0x3136, 0x3236, 0x3336, 0x3436,
  0x3536, 0x3636, 0x3736, 0x3836, 0x3936,
  0x3037, 0x3137, 0x3237, 0x3337, 0x3437,
  0x3537, 0x3637, 0x3737, 0x3837, 0x3937,
  0x3038, 0x3138, 0x3238, 0x3338, 0x3438,
  0x3538, 0x3638, 0x3738, 0x3838, 0x3938,
  0x3039, 0x3139, 0x3239, 0x3339, 0x3439,
  0x3539, 0x3639, 0x3739, 0x3839, 0x3939
]

// The constants below assume we're on a little-endian processor
@available(SwiftStdlib 6.2, *)
fileprivate func _infinity(
  buffer: inout MutableRawSpan,
  sign: FloatingPointSign
) -> Range<Int> {
  if sign == .minus {
    buffer.storeBytes(
      of: 0x666e692d, // "-inf"
      toByteOffset: 0,
      as: UInt32.self)
    return 0..<4
  } else {
    buffer.storeBytes(
      of: 0x00666e69, // "inf\0"
      toByteOffset: 0,
      as: UInt32.self)
    return 0..<3
  }
}

@available(SwiftStdlib 6.2, *)
fileprivate func _zero(
  buffer: inout MutableRawSpan,
  sign: FloatingPointSign
) -> Range<Int> {
  if sign == .minus {
    buffer.storeBytes(
      of: 0x302e302d, // "-0.0"
      toByteOffset: 0,
      as: UInt32.self)
    return 0..<4
  } else {
    buffer.storeBytes(
      of: 0x00302e30, // "0.0\0"
      toByteOffset: 0,
      as: UInt32.self)
    return 0..<3
  }
}

@available(SwiftStdlib 6.2, *)
fileprivate let hexdigits: InlineArray<16, UInt8> = [
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66
]

@available(SwiftStdlib 6.2, *)
fileprivate func _hexWithoutLeadingZeros(
  buffer: inout MutableRawSpan,
  offset: inout Int,
  value: UInt64
) {
  var shift = 60
  while (shift > 0) && ((value >> shift) & 0xf == 0) {
    shift -= 4
  }
  while shift >= 0 {
    let d = hexdigits[Int(truncatingIfNeeded: (value >> shift) & 0xf)]
    shift -= 4
    buffer.storeBytes(
      of: d,
      toByteOffset: offset,
      as: UInt8.self)
    offset += 1
  }
}

@available(SwiftStdlib 6.2, *)
fileprivate func _hexWithLeadingZeros(
  buffer: inout MutableRawSpan,
  offset: inout Int,
  value: UInt64
) {
  var shift = 60
  while shift >= 0 {
    let d = hexdigits[Int(truncatingIfNeeded: (value >> shift) & 0xf)]
    shift -= 4
    buffer.storeBytes(
      of: d,
      toByteOffset: offset,
      as: UInt8.self)
    offset += 1
  }
}

@available(SwiftStdlib 6.2, *)
fileprivate func nan_details(
  buffer: inout MutableRawSpan,
  sign: FloatingPointSign,
  quiet: Bool,
  payloadHigh: UInt64,
  payloadLow: UInt64
) -> Range<Int> {
  // value is a NaN of some sort
  var i = 0
  if sign == .minus {
    buffer.storeBytes(
      of: 0x2d, // "-"
      toByteOffset: 0,
      as: UInt8.self)
    i = 1
  }
  if !quiet {
    buffer.storeBytes(
      of: 0x73, // "s"
      toByteOffset: i,
      as: UInt8.self)
    i += 1
  }
  buffer.storeBytes(of: 0x6e, toByteOffset: i, as: UInt8.self) // "n"
  buffer.storeBytes(of: 0x61, toByteOffset: i + 1, as: UInt8.self) // "a"
  buffer.storeBytes(of: 0x6e, toByteOffset: i + 2, as: UInt8.self) // "n"
  i += 3
  if payloadHigh != 0 || payloadLow != 0 {
    buffer.storeBytes(of: 0x28, toByteOffset: i, as: UInt8.self) // "("
    i += 1
    buffer.storeBytes(of: 0x30, toByteOffset: i, as: UInt8.self) // "0"
    i += 1
    buffer.storeBytes(of: 0x78, toByteOffset: i, as: UInt8.self) // "x"
    i += 1
    if payloadHigh == 0 {
      _hexWithoutLeadingZeros(buffer: &buffer, offset: &i, value: payloadLow)
    } else {
      _hexWithoutLeadingZeros(buffer: &buffer, offset: &i, value: payloadHigh)
      _hexWithLeadingZeros(buffer: &buffer, offset: &i, value: payloadLow)
    }
    buffer.storeBytes(of: 0x29, toByteOffset: i, as: UInt8.self) // ")"
    i += 1
  }
  return 0..<i
}

// Convert an integer less than 10^8 into exactly 8 ASCII digits in a
// UInt64.  Assuming little-endian, the resulting UInt64 can be stored
// directly to memory.
//
// This implementation is based on work by Paul Khuong:
// https://pvk.ca/Blog/2017/12/22/appnexus-common-framework-its-out-also-how-to-print-integers-faster/
@available(SwiftStdlib 6.2, *)
@inline(__always)
fileprivate func _intToEightDigits(_ n: UInt32) -> UInt64 {
  // Break into two numbers of 4 decimal digits each
  let div8 = n / 10000
  let mod8 = n &- div8 &* 10000
  let fours = UInt64(div8) | (UInt64(mod8) << 32)

  // Break into 4 numbers of 2 decimal digits each
  let mask100: UInt64 = 0x0000007f0000007f
  let div4 = ((fours &* 10486) >> 20) & mask100
  let mod4 = fours &- 100 &* div4
  let pairs = div4 | (mod4 &<< 16)

  // Break into 8 numbers of a single decimal digit each
  let mask10: UInt64 = 0x000f000f000f000f
  let div2 = ((pairs &* 103) >> 10) & mask10
  let mod2 = pairs &- 10 &* div2
  let singles = div2 | (mod2 &<< 8)

  // Convert 8 digits to ASCII characters
  return singles &+ 0x3030303030303030
}

// ================================================================
//
// Arithmetic Helpers
//
// The code above works with fixed-point values.  Standard
// addition/subtraction/comparison works fine, but we need rounding
// control when multiplying such values.
//
// For exmaple, `multiply128x64RoundingDown` multiplies a 0.128
// fixed-point value by a 0.64 fixed-point fraction, returning a 0.128
// value that's been rounded down from the exact 192-bit result.
//
// ================================================================

@inline(__always)
fileprivate func _multiply64x32RoundingDown(
  _ lhs: UInt64,
  _ rhs: UInt32
) -> UInt64 {
  let mask32 = UInt64(UInt32.max)
  let t = ((lhs & mask32) * UInt64(rhs)) >> 32
  return t + (lhs >> 32) * UInt64(rhs)
}

@inline(__always)
fileprivate func _multiply64x32RoundingUp(
  _ lhs: UInt64,
  _ rhs: UInt32
) -> UInt64 {
  let mask32 = UInt64(UInt32.max)
  let t = (((lhs & mask32) * UInt64(rhs)) + mask32) >> 32
  return t + (lhs >> 32) * UInt64(rhs)
}

@available(SwiftStdlib 6.2, *)
@inline(__always)
fileprivate func _multiply128x64RoundingDown(
  _ lhs: UInt128,
  _ rhs: UInt64
) -> UInt128 {
  let lhsHigh = UInt128(truncatingIfNeeded: lhs._high)
  let lhsLow = UInt128(truncatingIfNeeded: lhs._low)
  let rhs128 = UInt128(truncatingIfNeeded: rhs)
  return (lhsHigh &* rhs128) &+ ((lhsLow &* rhs128) >> 64)
}

@available(SwiftStdlib 6.2, *)
@inline(__always)
fileprivate func _multiply128x64RoundingUp(
  _ lhs: UInt128,
  _ rhs: UInt64
) -> UInt128 {
  let lhsHigh = UInt128(truncatingIfNeeded: lhs._high)
  let lhsLow = UInt128(truncatingIfNeeded: lhs._low)
  let rhs128 = UInt128(truncatingIfNeeded: rhs)
  let h = lhsHigh &* rhs128
  let l = lhsLow &* rhs128
  let bias = (UInt128(1) << 64) &- 1
  return h + ((l &+ bias) &>> 64)
}

// Custom 256-bit unsigned integer type, with various arithmetic
// helpers as methods.
// Used by 80- and 128-bit floating point formatting logic above...
@available(SwiftStdlib 6.2, *)
fileprivate struct _UInt256 {
  var high: UInt128
  var low: UInt128

  init() {
    self.high = 0
    self.low = 0
  }

  init(high: UInt64, _ midHigh: UInt64, _ midLow: UInt64, low: UInt64) {
    self.high = UInt128(_low: midHigh, _high: high)
    self.low = UInt128(_low: low, _high: midLow)
  }

  init(high: UInt128, low: UInt128) {
    self.high = high
    self.low = low
  }

  mutating func shiftRightRoundingDown(by shift: Int) {
    assert(shift < 32 && shift >= 0)
    var t = UInt128(low._low >> shift)
    t |= UInt128(low._high) &<< (64 - shift)
    let newlow = t._low
    t = UInt128(t._high)
    t |= UInt128(high._low) &<< (64 - shift)
    low = UInt128(_low: newlow, _high: t._low)
    t = UInt128(t._high)
    t |= UInt128(high._high) &<< (64 - shift)
    high = t
  }

  mutating func shiftRightRoundingUp(by shift: Int) {
    assert(shift < 32 && shift >= 0)
    let bias = (UInt64(1) &<< shift) - 1
    var t = UInt128((low._low + bias) >> shift)
    t |= UInt128(low._high) &<< (64 - shift)
    let newlow = t._low
    t = UInt128(t._high)
    t |= UInt128(high._low) &<< (64 - shift)
    low = UInt128(_low: newlow, _high: t._low)
    t = UInt128(t._high)
    t |= UInt128(high._high) &<< (64 - shift)
    high = t
  }

  mutating func multiply(by rhs: UInt32) {
    var t = UInt128(low._low) &* UInt128(rhs)
    let newlow = t._low
    t = UInt128(t._high) &+ UInt128(low._high) &* UInt128(rhs)
    low = UInt128(_low: newlow, _high: t._low)
    t = UInt128(t._high) &+ UInt128(high._low) &* UInt128(rhs)
    let newmidhigh = t._low
    t = UInt128(t._high) &+ UInt128(high._high) &* UInt128(rhs)
    high = UInt128(_low: newmidhigh, _high: t._low)
    assert(t._high == 0)
  }

  mutating func multiplyRoundingDown(by rhs: UInt128) {
    var current = UInt128(low._low) * UInt128(rhs._low)

    current = UInt128(current._high)
    var t = UInt128(low._low) &* UInt128(rhs._high)
    current += UInt128(t._low)
    var next = UInt128(t._high)
    t = UInt128(low._high) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)

    current = next + UInt128(current._high)
    t = UInt128(low._high) &* UInt128(rhs._high)
    current += UInt128(t._low)
    next = UInt128(t._high)
    t = UInt128(high._low) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)
    let newlow = current._low

    current = next + UInt128(current._high)
    t = UInt128(high._low) &* UInt128(rhs._high)
    current += UInt128(t._low)
    next = UInt128(t._high)
    t = UInt128(high._high) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)
    low = UInt128(_low: newlow, _high: current._low)

    current = next + UInt128(current._high)
    t = UInt128(high._high) &* UInt128(rhs._high)
    high = current + t
  }

  mutating func multiplyRoundingUp(by rhs: UInt128) {
    var current = UInt128(low._low) &* UInt128(rhs._low)
    current += UInt128(UInt64.max)

    current = UInt128(current._high)
    var t = UInt128(low._low) &* UInt128(rhs._high)
    current += UInt128(t._low)
    var next = UInt128(t._high)
    t = UInt128(low._high) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)
    current += UInt128(UInt64.max)

    current = next + UInt128(current._high)
    t = UInt128(low._high) &* UInt128(rhs._high)
    current += UInt128(t._low)
    next = UInt128(t._high)
    t = UInt128(high._low) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)
    let newlow = current._low

    current = next + UInt128(current._high)
    t = UInt128(high._low) &* UInt128(rhs._high)
    current += UInt128(t._low)
    next = UInt128(t._high)
    t = UInt128(high._high) &* UInt128(rhs._low)
    current += UInt128(t._low)
    next += UInt128(t._high)
    low = UInt128(_low: newlow, _high: current._low)

    current = next + UInt128(current._high)
    t = UInt128(high._high) &* UInt128(rhs._high)
    high = current + t
  }

  mutating func extractIntegerPart(_ bits: Int) -> UInt {
    assert(bits < 16)
    let integral = high._high >> (64 &- bits)
    high = UInt128(
      _low: high._low,
      _high: high._high &- (integral &<< (64 &- bits)))
    return UInt(truncatingIfNeeded: integral)
  }

  static func &- (lhs: _UInt256, rhs: _UInt256) -> _UInt256 {
    var t = UInt128(lhs.low._low) &+ UInt128(~rhs.low._low) &+ 1
    let newlowlow = t._low
    t = UInt128(t._high) &+ UInt128(lhs.low._high) &+ UInt128(~rhs.low._high)
    let newlow = UInt128(_low: newlowlow, _high: t._low)
    t = UInt128(t._high) &+ UInt128(lhs.high._low) &+ UInt128(~rhs.high._low)
    let newhigh = UInt128(
      _low: t._low,
      _high: t._high &+ lhs.high._high &+ ~rhs.high._high)
    return _UInt256(high: newhigh, low: newlow)
  }

  static func < (lhs: _UInt256, rhs: _UInt256) -> Bool {
    return (lhs.high < rhs.high)
      || (lhs.high == rhs.high
            && lhs.low < rhs.low)
  }
}

// ================================================================
//
// Powers of 10
//
// ================================================================

@available(SwiftStdlib 6.2, *)
@inline(__always)
fileprivate func _intervalContainingPowerOf10_Binary32(
  p: Int,
  lower: inout UInt64,
  upper: inout UInt64
) -> Int {
  if p >= 0 {
    let base = powersOf10_Exact128[p &* 2 &+ 1]
    lower = base
    if p < 28 {
      upper = base
    } else {
      upper = base &+ 1
    }
  } else {
    let base = powersOf10_negativeBinary32[p &+ 40]
    lower = base
    upper = base &+ 1
  }
  return binaryExponentFor10ToThe(p)
}

@available(SwiftStdlib 6.2, *)
@inline(__always)
fileprivate func _intervalContainingPowerOf10_Binary64(
  p: Int,
  lower: inout UInt128,
  upper: inout UInt128
) -> Int {
  if p >= 0 && p <= 55 {
    let upper64 = powersOf10_Exact128[p &* 2 &+ 1]
    let lower64 = powersOf10_Exact128[p &* 2]
    upper = UInt128(_low: lower64, _high: upper64)
    lower = upper
    return binaryExponentFor10ToThe(p)
  }

  let index = p &+ 400
  let mainPower = index / 28
  let baseHigh = powersOf10_Binary64[mainPower &* 2 &+ 1]
  let baseLow = powersOf10_Binary64[mainPower &* 2]
  let extraPower = index &- mainPower &* 28
  let baseExponent = binaryExponentFor10ToThe(p &- extraPower)

  if extraPower == 0 {
    lower = UInt128(_low: baseLow, _high: baseHigh)
    upper = lower &+ 1
    return baseExponent
  } else {
    let extra = powersOf10_Exact128[extraPower &* 2 &+ 1]
    lower = ((UInt128(truncatingIfNeeded:baseHigh)
                &* UInt128(truncatingIfNeeded:extra))
               &+ ((UInt128(truncatingIfNeeded:baseLow)
                      &* UInt128(truncatingIfNeeded:extra)) &>> 64))
    upper = lower &+ 2
    return baseExponent &+ binaryExponentFor10ToThe(extraPower)
  }
}

@inline(__always)
fileprivate func binaryExponentFor10ToThe(_ p: Int) -> Int {
  return Int(((Int64(p) &* 55732705) >> 24) &+ 1)
}

@inline(__always)
fileprivate func decimalExponentFor2ToThe(_ p: Int) -> Int {
  return Int((Int64(p) &* 20201781) >> 26)
}

// Each of the constant values here have an implicit binary point at
// the extreme left and when not exact, are rounded _down_ from the
// exact values.  For example, the first row of the first table says
// that:
//
//   0x0.8b61313bbabce2c6 x 2^-132
//
// is the result of rounding down the exact binary value of 10^-40 to
// 64 significant bits.  The logic above uses these tables to compute
// bounds for the exact value of the power of 10.

// Note the binary exponent is not stored; it is computed by the
// `binaryExponentFor10ToThe(p)` function.

// This covers the negative powers of 10 for Float32.
// Positive powers of 10 come from the next table below.
// Table size: 320 bytes
@available(SwiftStdlib 6.2, *)
fileprivate let powersOf10_negativeBinary32: InlineArray<_, UInt64> = [
  0x8b61313bbabce2c6, // x 2^-132 ~= 10^-40
  0xae397d8aa96c1b77, // x 2^-129 ~= 10^-39
  0xd9c7dced53c72255, // x 2^-126 ~= 10^-38
  0x881cea14545c7575, // x 2^-122 ~= 10^-37
  0xaa242499697392d2, // x 2^-119 ~= 10^-36
  0xd4ad2dbfc3d07787, // x 2^-116 ~= 10^-35
  0x84ec3c97da624ab4, // x 2^-112 ~= 10^-34
  0xa6274bbdd0fadd61, // x 2^-109 ~= 10^-33
  0xcfb11ead453994ba, // x 2^-106 ~= 10^-32
  0x81ceb32c4b43fcf4, // x 2^-102 ~= 10^-31
  0xa2425ff75e14fc31, // x 2^-99 ~= 10^-30
  0xcad2f7f5359a3b3e, // x 2^-96 ~= 10^-29
  0xfd87b5f28300ca0d, // x 2^-93 ~= 10^-28
  0x9e74d1b791e07e48, // x 2^-89 ~= 10^-27
  0xc612062576589dda, // x 2^-86 ~= 10^-26
  0xf79687aed3eec551, // x 2^-83 ~= 10^-25
  0x9abe14cd44753b52, // x 2^-79 ~= 10^-24
  0xc16d9a0095928a27, // x 2^-76 ~= 10^-23
  0xf1c90080baf72cb1, // x 2^-73 ~= 10^-22
  0x971da05074da7bee, // x 2^-69 ~= 10^-21
  0xbce5086492111aea, // x 2^-66 ~= 10^-20
  0xec1e4a7db69561a5, // x 2^-63 ~= 10^-19
  0x9392ee8e921d5d07, // x 2^-59 ~= 10^-18
  0xb877aa3236a4b449, // x 2^-56 ~= 10^-17
  0xe69594bec44de15b, // x 2^-53 ~= 10^-16
  0x901d7cf73ab0acd9, // x 2^-49 ~= 10^-15
  0xb424dc35095cd80f, // x 2^-46 ~= 10^-14
  0xe12e13424bb40e13, // x 2^-43 ~= 10^-13
  0x8cbccc096f5088cb, // x 2^-39 ~= 10^-12
  0xafebff0bcb24aafe, // x 2^-36 ~= 10^-11
  0xdbe6fecebdedd5be, // x 2^-33 ~= 10^-10
  0x89705f4136b4a597, // x 2^-29 ~= 10^-9
  0xabcc77118461cefc, // x 2^-26 ~= 10^-8
  0xd6bf94d5e57a42bc, // x 2^-23 ~= 10^-7
  0x8637bd05af6c69b5, // x 2^-19 ~= 10^-6
  0xa7c5ac471b478423, // x 2^-16 ~= 10^-5
  0xd1b71758e219652b, // x 2^-13 ~= 10^-4
  0x83126e978d4fdf3b, // x 2^-9 ~= 10^-3
  0xa3d70a3d70a3d70a, // x 2^-6 ~= 10^-2
  0xcccccccccccccccc, // x 2^-3 ~= 10^-1
]

// All the powers of 10 that can be represented exactly
// in 128 bits, represented as binary floating-point values
// using the same convention as in the previous table, only
// with 128 bit significands.

// This table is used in four places:
// * The high order 64 bits are used for positive powers of 10
//   when converting Float32.
// * The full 128-bit value is used for 10^0 through 10^55 for Float64.
// * The first 28 entries are combined with the next table for
//   all other Float64 values.
// * This is combined with the 256-bit table below for Float80/Float128
//   support.

// Table size: 896 bytes
@available(SwiftStdlib 6.2, *)
fileprivate let powersOf10_Exact128: InlineArray<_, UInt64> = [
  // Low order ... high order
  0x0000000000000000, 0x8000000000000000, // x 2^1 == 10^0 exactly
  0x0000000000000000, 0xa000000000000000, // x 2^4 == 10^1 exactly
  0x0000000000000000, 0xc800000000000000, // x 2^7 == 10^2 exactly
  0x0000000000000000, 0xfa00000000000000, // x 2^10 == 10^3 exactly
  0x0000000000000000, 0x9c40000000000000, // x 2^14 == 10^4 exactly
  0x0000000000000000, 0xc350000000000000, // x 2^17 == 10^5 exactly
  0x0000000000000000, 0xf424000000000000, // x 2^20 == 10^6 exactly
  0x0000000000000000, 0x9896800000000000, // x 2^24 == 10^7 exactly
  0x0000000000000000, 0xbebc200000000000, // x 2^27 == 10^8 exactly
  0x0000000000000000, 0xee6b280000000000, // x 2^30 == 10^9 exactly
  0x0000000000000000, 0x9502f90000000000, // x 2^34 == 10^10 exactly
  0x0000000000000000, 0xba43b74000000000, // x 2^37 == 10^11 exactly
  0x0000000000000000, 0xe8d4a51000000000, // x 2^40 == 10^12 exactly
  0x0000000000000000, 0x9184e72a00000000, // x 2^44 == 10^13 exactly
  0x0000000000000000, 0xb5e620f480000000, // x 2^47 == 10^14 exactly
  0x0000000000000000, 0xe35fa931a0000000, // x 2^50 == 10^15 exactly
  0x0000000000000000, 0x8e1bc9bf04000000, // x 2^54 == 10^16 exactly
  0x0000000000000000, 0xb1a2bc2ec5000000, // x 2^57 == 10^17 exactly
  0x0000000000000000, 0xde0b6b3a76400000, // x 2^60 == 10^18 exactly
  0x0000000000000000, 0x8ac7230489e80000, // x 2^64 == 10^19 exactly
  0x0000000000000000, 0xad78ebc5ac620000, // x 2^67 == 10^20 exactly
  0x0000000000000000, 0xd8d726b7177a8000, // x 2^70 == 10^21 exactly
  0x0000000000000000, 0x878678326eac9000, // x 2^74 == 10^22 exactly
  0x0000000000000000, 0xa968163f0a57b400, // x 2^77 == 10^23 exactly
  0x0000000000000000, 0xd3c21bcecceda100, // x 2^80 == 10^24 exactly
  0x0000000000000000, 0x84595161401484a0, // x 2^84 == 10^25 exactly
  0x0000000000000000, 0xa56fa5b99019a5c8, // x 2^87 == 10^26 exactly
  0x0000000000000000, 0xcecb8f27f4200f3a, // x 2^90 == 10^27 exactly
  0x4000000000000000, 0x813f3978f8940984, // x 2^94 == 10^28 exactly
  0x5000000000000000, 0xa18f07d736b90be5, // x 2^97 == 10^29 exactly
  0xa400000000000000, 0xc9f2c9cd04674ede, // x 2^100 == 10^30 exactly
  0x4d00000000000000, 0xfc6f7c4045812296, // x 2^103 == 10^31 exactly
  0xf020000000000000, 0x9dc5ada82b70b59d, // x 2^107 == 10^32 exactly
  0x6c28000000000000, 0xc5371912364ce305, // x 2^110 == 10^33 exactly
  0xc732000000000000, 0xf684df56c3e01bc6, // x 2^113 == 10^34 exactly
  0x3c7f400000000000, 0x9a130b963a6c115c, // x 2^117 == 10^35 exactly
  0x4b9f100000000000, 0xc097ce7bc90715b3, // x 2^120 == 10^36 exactly
  0x1e86d40000000000, 0xf0bdc21abb48db20, // x 2^123 == 10^37 exactly
  0x1314448000000000, 0x96769950b50d88f4, // x 2^127 == 10^38 exactly
  0x17d955a000000000, 0xbc143fa4e250eb31, // x 2^130 == 10^39 exactly
  0x5dcfab0800000000, 0xeb194f8e1ae525fd, // x 2^133 == 10^40 exactly
  0x5aa1cae500000000, 0x92efd1b8d0cf37be, // x 2^137 == 10^41 exactly
  0xf14a3d9e40000000, 0xb7abc627050305ad, // x 2^140 == 10^42 exactly
  0x6d9ccd05d0000000, 0xe596b7b0c643c719, // x 2^143 == 10^43 exactly
  0xe4820023a2000000, 0x8f7e32ce7bea5c6f, // x 2^147 == 10^44 exactly
  0xdda2802c8a800000, 0xb35dbf821ae4f38b, // x 2^150 == 10^45 exactly
  0xd50b2037ad200000, 0xe0352f62a19e306e, // x 2^153 == 10^46 exactly
  0x4526f422cc340000, 0x8c213d9da502de45, // x 2^157 == 10^47 exactly
  0x9670b12b7f410000, 0xaf298d050e4395d6, // x 2^160 == 10^48 exactly
  0x3c0cdd765f114000, 0xdaf3f04651d47b4c, // x 2^163 == 10^49 exactly
  0xa5880a69fb6ac800, 0x88d8762bf324cd0f, // x 2^167 == 10^50 exactly
  0x8eea0d047a457a00, 0xab0e93b6efee0053, // x 2^170 == 10^51 exactly
  0x72a4904598d6d880, 0xd5d238a4abe98068, // x 2^173 == 10^52 exactly
  0x47a6da2b7f864750, 0x85a36366eb71f041, // x 2^177 == 10^53 exactly
  0x999090b65f67d924, 0xa70c3c40a64e6c51, // x 2^180 == 10^54 exactly
  0xfff4b4e3f741cf6d, 0xd0cf4b50cfe20765, // x 2^183 == 10^55 exactly
]

// Every 28th power of 10 across the full range of Double.
// Combined with a 64-bit exact power of 10 from the previous
// table, this lets us reconstruct a 128-bit lower bound for
// any power of 10 across the full range of double with a single
// 64-bit by 128-bit multiplication.

// The published algorithms generally use a full table here of
// 800 128-bit values (6400 bytes).  Breaking it into two tables
// gives a significant code-size savings for a modest performance
// penalty.

// Table size: 464 bytes
@available(SwiftStdlib 6.2, *)
fileprivate let powersOf10_Binary64: InlineArray<_, UInt64> = [
  // low-order half, high-order half
  0x3931b850df08e738, 0x95fe7e07c91efafa, // x 2^-1328 ~= 10^-400
  0xba954f8e758fecb3, 0x9774919ef68662a3, // x 2^-1235 ~= 10^-372
  0x9028bed2939a635c, 0x98ee4a22ecf3188b, // x 2^-1142 ~= 10^-344
  0x47b233c92125366e, 0x9a6bb0aa55653b2d, // x 2^-1049 ~= 10^-316
  0x4ee367f9430aec32, 0x9becce62836ac577, // x 2^-956 ~= 10^-288
  0x6f773fc3603db4a9, 0x9d71ac8fada6c9b5, // x 2^-863 ~= 10^-260
  0xc47bc5014a1a6daf, 0x9efa548d26e5a6e1, // x 2^-770 ~= 10^-232
  0x80e8a40eccd228a4, 0xa086cfcd97bf97f3, // x 2^-677 ~= 10^-204
  0xb8ada00e5a506a7c, 0xa21727db38cb002f, // x 2^-584 ~= 10^-176
  0xc13e60d0d2e0ebba, 0xa3ab66580d5fdaf5, // x 2^-491 ~= 10^-148
  0xc2974eb4ee658828, 0xa54394fe1eedb8fe, // x 2^-398 ~= 10^-120
  0xcb4ccd500f6bb952, 0xa6dfbd9fb8e5b88e, // x 2^-305 ~= 10^-92
  0x3f2398d747b36224, 0xa87fea27a539e9a5, // x 2^-212 ~= 10^-64
  0xdde50bd1d5d0b9e9, 0xaa242499697392d2, // x 2^-119 ~= 10^-36
  0xfdc20d2b36ba7c3d, 0xabcc77118461cefc, // x 2^-26 ~= 10^-8
  0x0000000000000000, 0xad78ebc5ac620000, // x 2^67 == 10^20 exactly
  0x9670b12b7f410000, 0xaf298d050e4395d6, // x 2^160 == 10^48 exactly
  0x3b25a55f43294bcb, 0xb0de65388cc8ada8, // x 2^253 ~= 10^76
  0x58edec91ec2cb657, 0xb2977ee300c50fe7, // x 2^346 ~= 10^104
  0x29babe4598c311fb, 0xb454e4a179dd1877, // x 2^439 ~= 10^132
  0x577b986b314d6009, 0xb616a12b7fe617aa, // x 2^532 ~= 10^160
  0x0c11ed6d538aeb2f, 0xb7dcbf5354e9bece, // x 2^625 ~= 10^188
  0x6d953e2bd7173692, 0xb9a74a0637ce2ee1, // x 2^718 ~= 10^216
  0x9d6d1ad41abe37f1, 0xbb764c4ca7a4440f, // x 2^811 ~= 10^244
  0x4b2d8644d8a74e18, 0xbd49d14aa79dbc82, // x 2^904 ~= 10^272
  0xe0470a63e6bd56c3, 0xbf21e44003acdd2c, // x 2^997 ~= 10^300
  0x505f522e53053ff2, 0xc0fe908895cf3b44, // x 2^1090 ~= 10^328
  0xcca845ab2beafa9a, 0xc2dfe19c8c055535, // x 2^1183 ~= 10^356
  0x1027fff56784f444, 0xc4c5e310aef8aa17, // x 2^1276 ~= 10^384
]

// Needed by 80- and 128-bit formatters above

// We could cut this in half by keeping only the positive powers and doing
// a single additional 256-bit multiplication by 10^-4984 to recover the negative powers.

// Table size: 5728 bytes
@available(SwiftStdlib 6.2, *)
fileprivate let powersOf10_Binary128: InlineArray<_, UInt64> = [
  // Low-order ... high-order
  0xaec2e6aff96b46ae, 0xf91044c2eff84750, 0x2b55c9e70e00c557, 0xb6536903bf8f2bda, // x 2^-16556 ~= 10^-4984
  0xda1b3c3dd3889587, 0x73a7380aba84a6b1, 0xbddb2dfde3f8a6e3, 0xb9e5428330737362, // x 2^-16370 ~= 10^-4928
  0xa2d23c57cfebb9ec, 0x9f165c039ead6d77, 0x88227fdfc13ab53d, 0xbd89006346a9a34d, // x 2^-16184 ~= 10^-4872
  0x0333d510cf27e5a5, 0x4e3cc383eaa17b7b, 0xe05fe4207ca3d508, 0xc13efc51ade7df64, // x 2^-15998 ~= 10^-4816
  0xff242c569bc1f539, 0x5c67ba58680c4cce, 0x3c55f3f947fef0e9, 0xc50791bd8dd72edb, // x 2^-15812 ~= 10^-4760
  0xe4b75ae27bec50bf, 0x25b0419765fdfcdb, 0x0915564d8ab057ee, 0xc8e31de056f89c19, // x 2^-15626 ~= 10^-4704
  0x548b1e80a94f3434, 0xe418e9217ce83755, 0x801e38463183fc88, 0xccd1ffc6bba63e21, // x 2^-15440 ~= 10^-4648
  0x541950a0fdc2b4d9, 0xeea173da1f0eb7b4, 0xcfadf6b2aa7c4f43, 0xd0d49859d60d40a3, // x 2^-15254 ~= 10^-4592
  0x7e64501be95ad76b, 0x451e855d8acef835, 0x9e601e707a2c3488, 0xd4eb4a687c0253e8, // x 2^-15068 ~= 10^-4536
  0xdadd9645f360cb51, 0xf290163350ecb3eb, 0xa8edffdccfe4db4b, 0xd9167ab0c1965798, // x 2^-14882 ~= 10^-4480
  0x7e447db3018ffbdf, 0x4fa1860c08a85923, 0xb17cd86e7fcece75, 0xdd568fe9ab559344, // x 2^-14696 ~= 10^-4424
  0x61cd4655bf64d265, 0xb19fd88fe285b3bc, 0x1151250681d59705, 0xe1abf2cd11206610, // x 2^-14510 ~= 10^-4368
  0xa5703f5ce7a619ec, 0x361243a84b55574d, 0x025a8e1e5dbb41d6, 0xe6170e21b2910457, // x 2^-14324 ~= 10^-4312
  0xb93897a6cf5d3e61, 0x18746fcc6a190db9, 0x66e849253e5da0c2, 0xea984ec57de69f13, // x 2^-14138 ~= 10^-4256
  0x309043d12ab5b0ac, 0x79c93cff11f09319, 0xf5a7800f23ef67b8, 0xef3023b80a732d93, // x 2^-13952 ~= 10^-4200
  0xa3baa84c049b52b9, 0xbec466ee1b586342, 0x0e85fc7f4edbd3ca, 0xf3defe25478e074a, // x 2^-13766 ~= 10^-4144
  0xd1f4628316b15c7a, 0xae16192410d3135e, 0x4268a54f70bd28c4, 0xf8a551706112897c, // x 2^-13580 ~= 10^-4088
  0x9eb9296cc5749dba, 0x48324e275376dfdd, 0x5052e9289f0f2333, 0xfd83933eda772c0b, // x 2^-13394 ~= 10^-4032
  0xff6aae669a5a0d8a, 0x24fed95087b9006e, 0x01b02378a405b421, 0x813d1dc1f0c754d6, // x 2^-13207 ~= 10^-3976
  0xf993f18de00dc89b, 0x15617da021b89f92, 0xb782db1fc6aba49b, 0x83c4e245ed051dc1, // x 2^-13021 ~= 10^-3920
  0xc6a0d64a712172b1, 0x2217669197ac1504, 0x4250be2eeba87d15, 0x86595584116caf3c, // x 2^-12835 ~= 10^-3864
  0x0bdc0c67a220687b, 0x44a66a6d6fd6537b, 0x3f1f93f1943ca9b6, 0x88fab70d8b44952a, // x 2^-12649 ~= 10^-3808
  0xb60b57164ad28122, 0xde5bd4572c25a830, 0x2c87f18b39478aa2, 0x8ba947b223e5783e, // x 2^-12463 ~= 10^-3752
  0xbd59568efdb9bfee, 0x292f8f2c98d7f44c, 0x4054f5360249ebd1, 0x8e6549867da7d11a, // x 2^-12277 ~= 10^-3696
  0x9fa0721e66791acc, 0x1789061d717d454c, 0xc1187fa0c18adbbe, 0x912effea7015b2c5, // x 2^-12091 ~= 10^-3640
  0x982b64e953ac4e27, 0x45efb05f20cf48b3, 0x4b4de34e0ebc3e06, 0x9406af8f83fd6265, // x 2^-11905 ~= 10^-3584
  0xa53f5950eec21dca, 0x3bd8754763bdbca1, 0xac73f0226eff5ea1, 0x96ec9e7f9004839b, // x 2^-11719 ~= 10^-3528
  0x320e19f88f1161b7, 0x72e93fe0cce7cfd9, 0x2184706ea46a4c38, 0x99e11423765ec1d0, // x 2^-11533 ~= 10^-3472
  0x491aba48dfc0e36e, 0xd3de560ee34022b2, 0xddadb80577b906bd, 0x9ce4594a044e0f1b, // x 2^-11347 ~= 10^-3416
  0x06789d038697142f, 0x7a466a75be73db21, 0x60dbd8aa443b560f, 0x9ff6b82ef415d222, // x 2^-11161 ~= 10^-3360
  0x40ed8056af76ac43, 0x08251c601e346456, 0x7401c6f091f87727, 0xa3187c82120dace6, // x 2^-10975 ~= 10^-3304
  0x8c643ee307bffec6, 0xf369a11c6f66c05a, 0x4d5b32f713d7f476, 0xa649f36e8583e81a, // x 2^-10789 ~= 10^-3248
  0xe32f5e080e36b4be, 0x3adf30ff2eb163d4, 0xb4b39dd9ddb8d317, 0xa98b6ba23e2300c7, // x 2^-10603 ~= 10^-3192
  0x6b9d538c192cfb1b, 0x1c5af3bd4d2c60b5, 0xec41c1793d69d0d1, 0xacdd3555869159d1, // x 2^-10417 ~= 10^-3136
  0x1adadaeedf7d699c, 0x71043692494aa743, 0x3ca5a7540d9d56c9, 0xb03fa252bd05a815, // x 2^-10231 ~= 10^-3080
  0xec3e4e5fc6b03617, 0x47c9b16afe8fdf74, 0x92e1bc1fbb33f18d, 0xb3b305fe328e571f, // x 2^-10045 ~= 10^-3024
  0x1d42fa68b12bdb23, 0xac46a7b3f2b4b34e, 0xa908fd4a88728b6a, 0xb737b55e31cdde04, // x 2^-9859 ~= 10^-2968
  0x887dede507f2b618, 0x359a8fa0d014b9a7, 0x7c4c65d15c614c56, 0xbace07232df1c802, // x 2^-9673 ~= 10^-2912
  0x504708e718b4b669, 0xfb4d9440822af452, 0xef84cc99cb4c5d17, 0xbe7653b01aae13e5, // x 2^-9487 ~= 10^-2856
  0x5b7977525516bff0, 0x75913092420c9b35, 0xcfc147ade4843a24, 0xc230f522ee0a7fc2, // x 2^-9301 ~= 10^-2800
  0xad5d11883cc1302b, 0x860a754894b9a0bc, 0x4668677d5f46c29b, 0xc5fe475d4cd35cff, // x 2^-9115 ~= 10^-2744
  0x42032f9f971bfc07, 0x9fb576046ab35018, 0x474b3cb1fe1d6a7f, 0xc9dea80d6283a34c, // x 2^-8929 ~= 10^-2688
  0xd3e7fbb72403a4dd, 0x8ca223055819af54, 0xd6ea3b733029ef0b, 0xcdd276b6e582284f, // x 2^-8743 ~= 10^-2632
  0xba2431d885f2b7d9, 0xc9879fc42869f610, 0x3736730a9e47fef8, 0xd1da14bc489025ea, // x 2^-8557 ~= 10^-2576
  0xa11edbcd65dd1844, 0xcb8edae81a295887, 0x3d24e68dc1027246, 0xd5f5e5681a4b9285, // x 2^-8371 ~= 10^-2520
  0xa0f076652f69ad08, 0x9d19c341f5f42f2a, 0x742ab8f3864562c8, 0xda264df693ac3e30, // x 2^-8185 ~= 10^-2464
  0x29f760ef115f2824, 0xe0ee47c041c9de0f, 0x8c119f3680212413, 0xde6bb59f56672cda, // x 2^-7999 ~= 10^-2408
  0x8b90230b3409c9d3, 0x9d76eef2c1543e65, 0x43190b523f872b9c, 0xe2c6859f5c284230, // x 2^-7813 ~= 10^-2352
  0xd44ce9993bc6611e, 0x777c9b2dfbede079, 0x2a0969bf88679396, 0xe7372943179706fc, // x 2^-7627 ~= 10^-2296
  0xe8c5f5a63fd0fbd1, 0x0ccc12293f1d7a58, 0x131565be33dda91a, 0xebbe0df0c8201ac5, // x 2^-7441 ~= 10^-2240
  0xdb97988dd6b776f4, 0xeb2106f435f7e1d5, 0xccfb1cc2ef1f44de, 0xf05ba3330181c750, // x 2^-7255 ~= 10^-2184
  0x2fcbc8df94a1d54b, 0x796d0a8120801513, 0x5f8385b3a882ff4c, 0xf5105ac3681f2716, // x 2^-7069 ~= 10^-2128
  0xc8700c11071a40f5, 0x23cb9e9df9331fe4, 0x166c15f456786c27, 0xf9dca895a3226409, // x 2^-6883 ~= 10^-2072
  0x9589f4637a50cbb5, 0xea8242b0030e4a51, 0x6c656c3b1f2c9d91, 0xfec102e2857bc1f9, // x 2^-6697 ~= 10^-2016
  0xc4be56c83349136c, 0x6188db81ac8e775d, 0xfa70b9a2ca60b004, 0x81def119b76837c8, // x 2^-6510 ~= 10^-1960
  0xb85d39054658b363, 0xe7df06bc613fda21, 0x6a22490e8e9ec98b, 0x8469e0b6f2b8bd9b, // x 2^-6324 ~= 10^-1904
  0x800b1e1349fef248, 0x469cfd2e6ca32a77, 0x69138459b0fa72d4, 0x87018eefb53c6325, // x 2^-6138 ~= 10^-1848
  0xb62593291c768919, 0xc098e6ed0bfbd6f6, 0x6c83ad1260ff20f4, 0x89a63ba4c497b50e, // x 2^-5952 ~= 10^-1792
  0x92ee7fce474479d3, 0xe02017175bf040c6, 0xd82ef2860273de8d, 0x8c5827f711735b46, // x 2^-5766 ~= 10^-1736
  0x7b0e6375ca8c77d9, 0x5f07e1e10097d47f, 0x416d7f9ab1e67580, 0x8f17964dfc3961f2, // x 2^-5580 ~= 10^-1680
  0xc8d869ed561af1ce, 0x8b6648e941de779b, 0x56700866b85d57fe, 0x91e4ca5db93dbfec, // x 2^-5394 ~= 10^-1624
  0xfc04df783488a410, 0x64d1f15da2c146b1, 0x43cf71d5c4fd7868, 0x94c0092dd4ef9511, // x 2^-5208 ~= 10^-1568
  0xfbaf03b48a965a64, 0x9b6122aa2b72a13c, 0x387898a6e22f821b, 0x97a9991fd8b3afc0, // x 2^-5022 ~= 10^-1512
  0x50f7f7c13119aadd, 0xe415d8b25694250a, 0x8f8857e875e7774e, 0x9aa1c1f6110c0dd0, // x 2^-4836 ~= 10^-1456
  0xce214403545fd685, 0xf36d1ad779b90e09, 0xa5c58d5f91a476d7, 0x9da8ccda75b341b5, // x 2^-4650 ~= 10^-1400
  0x63ddfb68f971b0c5, 0x2822e38faf74b26e, 0x6e1f7f1642ebaac8, 0xa0bf0465b455e921, // x 2^-4464 ~= 10^-1344
  0xf0d00cec9daf7444, 0x6bf3eea6f661a32a, 0xfad2be1679765f27, 0xa3e4b4a65e97b76a, // x 2^-4278 ~= 10^-1288
  0x463b4ab4bd478f57, 0x6f6583b5b36d5426, 0x800cfab80c4e2eb1, 0xa71a2b283c14fba6, // x 2^-4092 ~= 10^-1232
  0xef163df2fa96e983, 0xa825f32bc8f6b080, 0x850b0c5976b21027, 0xaa5fb6fbc115010b, // x 2^-3906 ~= 10^-1176
  0x7db1b3f8e100eb43, 0x2862b1f61d64ddc3, 0x61363686961a41e5, 0xadb5a8bdaaa53051, // x 2^-3720 ~= 10^-1120
  0xfd349cf00ba1e09a, 0x6d282fe1b7112879, 0xc6f075c4b81fc72d, 0xb11c529ec0d87268, // x 2^-3534 ~= 10^-1064
  0xf7221741b221cf6f, 0x3739f15b06ac3c76, 0xb4e4be5b6455ef96, 0xb494086bbfea00c3, // x 2^-3348 ~= 10^-1008
  0xc4e5a2f864c403bb, 0x6e33cdcda4367276, 0x24d256c540a50309, 0xb81d1f9569068d8e, // x 2^-3162 ~= 10^-952
  0x276e3f0f67f0553b, 0x00de73d9d5be6974, 0x6d4aa5b50bb5dc0d, 0xbbb7ef38bb827f2d, // x 2^-2976 ~= 10^-896
  0x51a34a3e674484ed, 0x1fb6069f8b26f840, 0x925624c0d7d93317, 0xbf64d0275747de70, // x 2^-2790 ~= 10^-840
  0xcc775c8cb6de1dbc, 0x6d60d02eac6309ee, 0x8e5a2e5116baf191, 0xc3241cf0094a8e70, // x 2^-2604 ~= 10^-784
  0x6023c8fa17d7b105, 0x069cf8f51d2e5e65, 0xb0560c246f90e9e8, 0xc6f631e782d57096, // x 2^-2418 ~= 10^-728
  0x92c17acb2d08d5fd, 0xc26ffb8e81532725, 0x2ffff1289a804c5a, 0xcadb6d313c8736fc, // x 2^-2232 ~= 10^-672
  0x47df78ab9e92897a, 0xc02b302a892b81dc, 0xa855e127113c887b, 0xced42ec885d9dbbe, // x 2^-2046 ~= 10^-616
  0xdaf2dec03ec0c322, 0x72db3bc15b0c7014, 0xe00bad8dfc0d8c8e, 0xd2e0d889c213fd60, // x 2^-1860 ~= 10^-560
  0xd3a04799e4473ac8, 0xa116409a2fdf1e9e, 0xc654d07271e6c39f, 0xd701ce3bd387bf47, // x 2^-1674 ~= 10^-504
  0x5c8a5dc65d745a24, 0x2726c48a85389fa7, 0x84c663cee6b86e7c, 0xdb377599b6074244, // x 2^-1488 ~= 10^-448
  0xd7ebc61ba77a9e66, 0x8bf77d4bc59b35b1, 0xcb285ceb2fed040d, 0xdf82365c497b5453, // x 2^-1302 ~= 10^-392
  0x744ce999bfed213a, 0x363b1f2c568dc3e2, 0xfd1b1b2308169b25, 0xe3e27a444d8d98b7, // x 2^-1116 ~= 10^-336
  0x6a40608fe10de7e7, 0xf910f9f648232f14, 0xd1b3400f8f9cff68, 0xe858ad248f5c22c9, // x 2^-930 ~= 10^-280
  0x9bdbfc21260dd1ad, 0x4609ac5c7899ca36, 0xa4f8bf5635246428, 0xece53cec4a314ebd, // x 2^-744 ~= 10^-224
  0xd88181aad19d7454, 0xf80f36174730ca34, 0xdc44e6c3cb279ac1, 0xf18899b1bc3f8ca1, // x 2^-558 ~= 10^-168
  0xee19bfa6947f8e02, 0xaa09501d5954a559, 0x4d4617b5ff4a16d5, 0xf64335bcf065d37d, // x 2^-372 ~= 10^-112
  0xebbc75a03b4d60e6, 0xac2e4f162cfad40a, 0xeed6e2f0f0d56712, 0xfb158592be068d2e, // x 2^-186 ~= 10^-56
  0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, // x 2^1 == 10^0 exactly
  0x0000000000000000, 0x2000000000000000, 0xbff8f10e7a8921a4, 0x82818f1281ed449f, // x 2^187 == 10^56 exactly
  0x51775f71e92bf2f2, 0x74a7ef0198791097, 0x03e2cf6bc604ddb0, 0x850fadc09923329e, // x 2^373 ~= 10^112
  0xb204b3d9686f55b5, 0xfb118fc9c217a1d2, 0x90fb44d2f05d0842, 0x87aa9aff79042286, // x 2^559 ~= 10^168
  0xd7924bff833149fa, 0xbc10c5c5cda97c8d, 0x82bd6b70d99aaa6f, 0x8a5296ffe33cc92f, // x 2^745 ~= 10^224
  0xa67d072d3c7fa14b, 0x7ec63730f500b406, 0xdb0b487b6423e1e8, 0x8d07e33455637eb2, // x 2^931 ~= 10^280
  0x546f2a35dc367e47, 0x949063d8a46f0c0e, 0x213a4f0aa5e8a7b1, 0x8fcac257558ee4e6, // x 2^1117 ~= 10^336
  0x50611a621c0ee3ae, 0x202d895116aa96be, 0x1c306f5d1b0b5fdf, 0x929b7871de7f22b9, // x 2^1303 ~= 10^392
  0xffa6738a27dcf7a3, 0x3c11d8430d5c4802, 0xa7ea9c8838ce9437, 0x957a4ae1ebf7f3d3, // x 2^1489 ~= 10^448
  0x5bf36c0f40bde99d, 0x284ba600ee9f6303, 0xbf1d49cacccd5e68, 0x9867806127ece4f4, // x 2^1675 ~= 10^504
  0xa6e937834ed12e58, 0x73f26eb82f6b8066, 0x655494c5c95d77f2, 0x9b63610bb9243e46, // x 2^1861 ~= 10^560
  0x0cd4b7660adc6930, 0x8f868688f8eb79eb, 0x02e008393fd60b55, 0x9e6e366733f85561, // x 2^2047 ~= 10^616
  0x3efb9807d86d3c6a, 0x84c10a1d22f5adc5, 0x55e04dba4b3bd4dd, 0xa1884b69ade24964, // x 2^2233 ~= 10^672
  0xf065089401df33b4, 0x1fc02370c451a755, 0x44b222741eb1ebbf, 0xa4b1ec80f47c84ad, // x 2^2419 ~= 10^728
  0xa62d0da836fce7d5, 0x75933380ceb5048c, 0x1cf4a5c3bc09fa6f, 0xa7eb6799e8aec999, // x 2^2605 ~= 10^784
  0x7a400df820f096c2, 0x802c4085068d2dd5, 0x3c4a575151b294dc, 0xab350c27feb90acc, // x 2^2791 ~= 10^840
  0xf48b51375df06e86, 0x412fe9e72afd355e, 0x870a8d87239d8f35, 0xae8f2b2ce3d5dbe9, // x 2^2977 ~= 10^896
  0x881883521930127c, 0xe53fd3fcb5b4df25, 0xdd929f09c3eff5ac, 0xb1fa17404a30e5e8, // x 2^3163 ~= 10^952
  0x270cd9f1348eb326, 0x37ed82fe9c75fccf, 0x1931b583a9431d7e, 0xb5762497dbf17a9e, // x 2^3349 ~= 10^1008
  0x8919b01a5b3d9ec1, 0x6a7669bdfc6f699c, 0xe30db03e0f8dd286, 0xb903a90f561d25e2, // x 2^3535 ~= 10^1064
  0xf0461526b4201aa5, 0x7fe40defe17e55f5, 0x9eb5cb19647508c5, 0xbca2fc30cc19f090, // x 2^3721 ~= 10^1120
  0xd67bf35422978bbf, 0x0dbb1c416ebe661f, 0x24bd4c00042ad125, 0xc054773d149bf26b, // x 2^3907 ~= 10^1176
  0xdd093192ef5508d0, 0x6eac3085943ccc0f, 0x7ea30dbd7ea479e3, 0xc418753460cdcca9, // x 2^4093 ~= 10^1232
  0xfe4ff20db6d25dc2, 0x5d5d5a9519e34a42, 0x764f4cf916b4dece, 0xc7ef52defe87b751, // x 2^4279 ~= 10^1288
  0xd8adfb2e00494c5e, 0x72435286baf0e84e, 0xbeb7fbdc1cbe8b37, 0xcbd96ed6466cf081, // x 2^4465 ~= 10^1344
  0xe07c1e4384f594af, 0x0c6b90b8874d5189, 0xdce472c619aa3f63, 0xcfd7298db6cb9672, // x 2^4651 ~= 10^1400
  0x5dd902c68fa448cf, 0xea8d16bd9544e48e, 0xe47defc14a406e4f, 0xd3e8e55c3c1f43d0, // x 2^4837 ~= 10^1456
  0x1223d79357bedca8, 0xeae6c2843752ac35, 0xb7157c60a24a0569, 0xd80f0685a81b2a81, // x 2^5023 ~= 10^1512
  0xcff72d64bc79e429, 0xccc52c236decd778, 0xfb0b98f6bbc4f0cb, 0xdc49f3445824e360, // x 2^5209 ~= 10^1568
  0x3731f76b905dffbb, 0x5e2bddd7d12a9e42, 0xc6c6c1764e047e15, 0xe09a13d30c2dba62, // x 2^5395 ~= 10^1624
  0xeb58d8ef2ada7c09, 0xbc1a3b726b789947, 0x87e8dcfc09dbc33a, 0xe4ffd276eedce658, // x 2^5581 ~= 10^1680
  0x249a5c06dc5d5db7, 0xa8f09440be97bfe6, 0xb1a3642a8da3cf4f, 0xe97b9b89d001dab3, // x 2^5767 ~= 10^1736
  0xbf34ff7963028cd9, 0xc20578fa3851488b, 0x2d4070f33b21ab7b, 0xee0ddd84924ab88c, // x 2^5953 ~= 10^1792
  0x002d0511317361d5, 0xd6919e041129a1a7, 0xa2bf0c63a814e04e, 0xf2b70909cd3fd35c, // x 2^6139 ~= 10^1848
  0x1fa87f28acf1dcd2, 0xe7a0a88981d1a0f9, 0x08f13995cf9c2747, 0xf77790f0a48a45ce, // x 2^6325 ~= 10^1904
  0x1b6ff8afbe589b72, 0xc851bb3f9aeb1211, 0x7a37993eb21444fa, 0xfc4fea4fd590b40a, // x 2^6511 ~= 10^1960
  0xef23a4cbc039f0c2, 0xbb3f8498a972f18e, 0xb7b1ada9cdeba84d, 0x80a046447e3d49f1, // x 2^6698 ~= 10^2016
  0x2cc44f2b602b6231, 0xf231f4b7996b7278, 0x0cc6866c5d69b2cb, 0x8324f8aa08d7d411, // x 2^6884 ~= 10^2072
  0x822c97629a3a4c69, 0x8a9afcdbc940e6f9, 0x7fe2b4308dcbf1a3, 0x85b64a659077660e, // x 2^7070 ~= 10^2128
  0xf66cfcf42d4896b0, 0x1f11852a20ed33c5, 0x1d73ef3eaac3c964, 0x88547abb1d8e5bd9, // x 2^7256 ~= 10^2184
  0x63093ad0caadb06c, 0x31be1482014cdaf0, 0x1e34291b1ef566c7, 0x8affca2bd1f88549, // x 2^7442 ~= 10^2240
  0xab50f69048738e9a, 0xa126c32ff4882be8, 0x9e9383d73d486881, 0x8db87a7c1e56d873, // x 2^7628 ~= 10^2296
  0xe57e659432b0a73e, 0x47a0e15dfc7986b8, 0x9cc5ee51962c011a, 0x907eceba168949b3, // x 2^7814 ~= 10^2352
  0x8a6ff950599f8ae5, 0xd1cbbb7d005a76d3, 0x413407cfeeac9743, 0x93530b43e5e2c129, // x 2^8000 ~= 10^2408
  0xd4e6b6e847550caa, 0x56a3106227b87706, 0x7efa7d29c44e11b7, 0x963575ce63b6332d, // x 2^8186 ~= 10^2464
  0xd835c90b09842263, 0xb69f01a641da2a42, 0x5a848859645d1c6f, 0x9926556bc8defe43, // x 2^8372 ~= 10^2520
  0x9b0ae73c204ecd61, 0x0794fd5e5a51ac2f, 0x51edea897b34601f, 0x9c25f29286e9ddb6, // x 2^8558 ~= 10^2576
  0x3130484fb0a61d89, 0x32b7105223a27365, 0xb50008d92529e91f, 0x9f3497244186fca4, // x 2^8744 ~= 10^2632
  0x8cd036553f38a1e8, 0x5e997e9f45d7897d, 0xf09e780bcc8238d9, 0xa2528e74eaf101fc, // x 2^8930 ~= 10^2688
  0xe1f8b43b08b5d0ef, 0xa0eaf3f62dc1777c, 0x3a5828869701a165, 0xa580255203f84b47, // x 2^9116 ~= 10^2744
  0x3c7f62e3154fa708, 0x5786f3927eb15bd5, 0x8b231a70eb5444ce, 0xa8bdaa0a0064fa44, // x 2^9302 ~= 10^2800
  0x1ebc24a19cd70a2a, 0x843fddd10c7006b8, 0xfa1bde1f473556a4, 0xac0b6c73d065f8cc, // x 2^9488 ~= 10^2856
  0x46b6aae34cfd26fc, 0x00db7d919b136c68, 0x7730e00421da4d55, 0xaf69bdf68fc6a740, // x 2^9674 ~= 10^2912
  0x1c4edcb83fc4c49d, 0x61c0edd56bbcb3e8, 0x7f959cb702329d14, 0xb2d8f1915ba88ca5, // x 2^9860 ~= 10^2968
  0x428c840d247382fe, 0x9cc3b1569b1325a4, 0x40c3a071220f5567, 0xb6595be34f821493, // x 2^10046 ~= 10^3024
  0xbeb82e734787ec63, 0xbeff12280d5a1676, 0x11c48d02b8326bd3, 0xb9eb5333aa272e9b, // x 2^10232 ~= 10^3080
  0x302349e12f45c73f, 0xb494bcc96d53e49c, 0x566765461bd2f61b, 0xbd8f2f7a1ba47d6d, // x 2^10418 ~= 10^3136
  0x5704ebf5f16946ce, 0x431388ec68ac7a26, 0xb889018e4f6e9a52, 0xc1454a673cb9b1ce, // x 2^10604 ~= 10^3192
  0x5a30431166af9b23, 0x132d031fc1d1fec0, 0xf85333a94848659f, 0xc50dff6d30c3aefc, // x 2^10790 ~= 10^3248
  0x7573d4b3ffe4ba3b, 0xf888498a40220657, 0x1a1aeae7cf8a9d3d, 0xc8e9abc872eb2bc1, // x 2^10976 ~= 10^3304
  0xb5eaef7441511eb9, 0xc9cf998035a91664, 0x12e29f09d9061609, 0xccd8ae88cf70ad84, // x 2^11162 ~= 10^3360
  0x73aed4f1908f4d01, 0x8c53e7beeca4578f, 0xdf7601457ca20b35, 0xd0db689a89f2f9b1, // x 2^11348 ~= 10^3416
  0x5adbd55696e1cdd9, 0x4949d09424b87626, 0xcbdcd02f23cc7690, 0xd4f23ccfb1916df5, // x 2^11534 ~= 10^3472
  0x3f500ccf4ea03593, 0x9b80aac81b50762a, 0x44289dd21b589d7a, 0xd91d8fe9a3d019cc, // x 2^11720 ~= 10^3528
  0x134ca67a679b84ae, 0x8909e424a112a3cd, 0x95aa118ec1d08317, 0xdd5dc8a2bf27f3f7, // x 2^11906 ~= 10^3584
  0xe89e3cf733d9ff40, 0x014344660a175c36, 0x72c4d2cad73b0a7b, 0xe1b34fb846321d04, // x 2^12092 ~= 10^3640
  0x68c0a2c6c02dae9a, 0x0b11160a6edb5f57, 0xe20a88f1134f906d, 0xe61e8ff47461cda9, // x 2^12278 ~= 10^3696
  0x47fa54906741561a, 0xaa13acba1e5511f5, 0xc7c91d5c341ed39d, 0xea9ff638c54554e1, // x 2^12464 ~= 10^3752
  0x365460ed91271c24, 0xabe33496aff629b4, 0xf659ede2159a45ec, 0xef37f1886f4b6690, // x 2^12650 ~= 10^3808
  0xe4cbf4acc7fba37f, 0x350e915f7055b1b8, 0x78d946bab954b82f, 0xf3e6f313130ef0ef, // x 2^12836 ~= 10^3864
  0xe692accdfa5bd859, 0xf4d4d3202379829e, 0xc9b1474d8f89c269, 0xf8ad6e3fa030bd15, // x 2^13022 ~= 10^3920
  0xeca0018ea3b8d1b4, 0xe878edb67072c26d, 0x6b1d2745340e7b14, 0xfd8bd8b770cb469e, // x 2^13208 ~= 10^3976
  0xce5fec949ab87cf7, 0x0151dcd7a53488c3, 0xf22e502fcdd4bca2, 0x81415538ce493bd5, // x 2^13395 ~= 10^4032
  0x5e1731fbff8c032e, 0xe752f53c2f8fa6c1, 0x7c1735fc3b813c8c, 0x83c92edf425b292d, // x 2^13581 ~= 10^4088
  0xb552102ea83f47e6, 0xdf0fd2002ff6b3a3, 0x0367500a8e9a178f, 0x865db7a9ccd2839e, // x 2^13767 ~= 10^4144
  0x76507bafe00ec873, 0x71b256ecd954434c, 0xc9ac50475e25293a, 0x88ff2f2bade74531, // x 2^13953 ~= 10^4200
  0x5e2075ba289a360b, 0xac376f28b45e5acc, 0x0879b2e5f6ee8b1c, 0x8badd636cc48b341, // x 2^14139 ~= 10^4256
  0xab87d85e6311e801, 0xb7f786d14d58173d, 0x2f33c652bd12fab7, 0x8e69eee1f23f2be5, // x 2^14325 ~= 10^4312
  0x7fed9b68d77255be, 0x35dc241819de7182, 0xad6a6308a8e8b557, 0x9133bc8f2a130fe5, // x 2^14511 ~= 10^4368
  0x728ae72899d4bd12, 0xe5413d9414142a55, 0x9dbaa465efe141a0, 0x940b83f23a55842a, // x 2^14697 ~= 10^4424
  0x0f7740145246fb8f, 0x186ef2c39acb4103, 0x888c9ab2fc5b3437, 0x96f18b1742aad751, // x 2^14883 ~= 10^4480
  0xd8bb0fba2183c6ef, 0xbf66d66cc34f0197, 0xba00864671d1053f, 0x99e6196979b978f1, // x 2^15069 ~= 10^4536
  0x9b71ed2ceb790e49, 0x6faac32d59cc1f5d, 0x61d59d402aae4fea, 0x9ce977ba0ce3a0bd, // x 2^15255 ~= 10^4592
  0xa0aa6d5e63991cfb, 0x19482fa0ac45669c, 0x803c1cd864033781, 0x9ffbf04722750449, // x 2^15441 ~= 10^4648
  0x95a9949e04b8bff3, 0x900aa3c2f02ac9d4, 0xa28a151725a55e10, 0xa31dcec2fef14b30, // x 2^15627 ~= 10^4704
  0x3acf9496dade0ce9, 0xbd8ecf923d23bec0, 0x5b8452af2302fe13, 0xa64f605b4e3352cd, // x 2^15813 ~= 10^4760
  0x6204425d2b58e822, 0xdee162a8a1248550, 0x82b84cabc828bf93, 0xa990f3c09110c544, // x 2^15999 ~= 10^4816
  0x091a2658e0639f32, 0x66fa2184cee0b861, 0x8d29dd5122e4278d, 0xace2d92db0390b59, // x 2^16185 ~= 10^4872
  0x80acda113324758a, 0xded179c26d9ab828, 0x58f8fde02c03a6c6, 0xb045626fb50a35e7, // x 2^16371 ~= 10^4928
  0x7128a8aad239ce8f, 0x8737bd250290cd5b, 0xd950102978dbd0ff, 0xb3b8e2eda91a232d, // x 2^16557 ~= 10^4984
]

@available(SwiftStdlib 6.2, *)
fileprivate func _intervalContainingPowerOf10_Binary128(
  p: Int,
  lower: inout _UInt256,
  upper: inout _UInt256
) -> Int {
  if p >= 0 && p <= 55 {
    let exactLow = powersOf10_Exact128[p * 2]
    let exactHigh = powersOf10_Exact128[p * 2 + 1]
    lower = _UInt256(high: exactHigh, exactLow, 0, low: 0)
    upper = lower
    return binaryExponentFor10ToThe(p)
  }

  let index = p + 4984
  let offset = (index / 56) * 4
  lower = _UInt256(
    high: powersOf10_Binary128[offset + 3],
    powersOf10_Binary128[offset + 2],
    powersOf10_Binary128[offset + 1],
    low: powersOf10_Binary128[offset + 0])
  let extraPower = index % 56
  var e = binaryExponentFor10ToThe(p - extraPower)

  if extraPower > 0 {
    let extra = UInt128(
      _low: powersOf10_Exact128[extraPower * 2],
      _high: powersOf10_Exact128[extraPower * 2 + 1])
    lower.multiplyRoundingDown(by: extra)
    e += binaryExponentFor10ToThe(extraPower)
  }
  upper = lower
  upper.low += 2
  return e
}
