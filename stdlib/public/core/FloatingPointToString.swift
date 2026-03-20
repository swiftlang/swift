//===--- FloatingPointToString.swift -------------------------*- Swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018-2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
// Converts floating-point types to "optimal" text formats.
//
// The "optimal" form is one with a minimum number of significant digits which
// will parse to exactly the original value.  This form is ideal for JSON
// serialization, logging, debugging, and any general printing where you don't
// have specific UI requirements.
//
// Because it doesn't have to support an arbitrary number of output
// digits, this format can be generated _much_ faster than the
// `printf` `%e`/`%f`/`%g` forms, which makes it even more ideal for
// JSON, logging, and general output.
//
//===---------------------------------------------------------------------===//
///
/// This code has used a variety of different algorithms over the years, all
/// generating exactly identical output.  The current implementation starts
/// with a floating-point value `s * 2^e` where s and e are both integers.  We
/// compute
/// ```
///   p = ceil(e * log10(2))
/// ```
/// and then do a fixed-precision multiply
/// ```
///   (s + 1/2) * 2^e * 10^-p
/// ```
/// with different rounding depending on whether the original
/// `s` is even or odd.
///
/// * About 40% of the time, the integer portion of this last value is
///   precisely the integer significand of the optimal base-10
///   representation we want.  We only need to convert this integer to
///   digits with a fast integer conversion algorithm, such as the
///   one below based on work by Paul Khuong.
///
/// * The remaining 60% of the time, we need to generate a single
///   additional digit (with correct rounding).
///
/// The description above omits many key details, of course.  A more complete
/// description appears in the extensive comments for the Float32 implementation
/// below.  (The Float16/64/80 implementations use the same approach,
/// only with fewer comments.)
///
/// The implementation draws inspiration from the following papers:
///
/// ------------
/// Raffaello Guilietti; "The Schubfach way to render doubles", 2021.
/// Published online.
///
/// Credit: Guilietti computes the power-of-10 scaling factor based on
/// the width of the rounding interval in order to obtain a
/// nearly-final result from the initial scaling, avoiding the need
/// for per-digit iteration.
/// ------------
/// Florian Loitsch; "Printing Floating-Point Numbers Quickly and
/// Accurately with Integers", 2010.
/// https://doi.org/10.1145/1806596.1806623
///
/// Credit: Our test for whether the initial digits give us a value in
/// the rounding interval is from Loitsch' Grisu algorithm.
/// ------------
/// Marc Andrysco, Ranjit Jhala, Sorin Lerner; "Printing Floating-Point
/// Numbers: A Faster, Always Correct Method", 2016.
/// https://doi.org/10.1145/2837614.2837654
///
/// Credit: Andrysco, Jhala, and Lerner explored the impact of
/// higher precision arithmetic on Grisu-style algorithms and
/// provided a robust way to test implementations of such algorithms
/// for correctness.
/// ------------
/// TODO: Cite vitaut's exploration of Schubfach variants?
///
/// In short, this implementation is:
///
/// * Fast.  It uses only fixed-width integer arithmetic and has
///   constant memory requirements.
///
/// * Always Round-trip Accurate. Except for NaNs, converting the
///   decimal form back to binary will always yield an equal
///   value. For the IEEE 754 formats, the round trip will produce
///   exactly the same bit pattern in memory. (This assumes, of course,
///   that the conversion from text to binary uses a correctly-rounded
///   algorithm such as Clinger 1990, Eisel-Lemire 2021, or that
///   used in FloatingPointFromString.swift.)
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
///
/// Note: If you want to compare performance of this implementation
/// versus some others, keep in mind that this implementation does
/// deliberately sacrifice some performance to achieve other goals.
/// Any attempt to compare the performance of this implementation
/// to others should consider the following:
/// * The output ergonomics described above do take some time.
///   It would be faster to always emit an integer significand
///   followed by an exponent (e.g., "123456e-78").
/// * The implementations in published papers generally include
///   large tables with every power of 10 computed out.  We've
///   factored these tables down to conserve code size, which
///   requires some additional work to reconstruct the needed power
///   of 10. (See the `_powerOf10_*()` functions)
/// * Compare code size as well as performance.  Our Float64
///   implementation here is a little over 3kiB compiled, _including_
///   the supporting data tables.  (Most published implementations
///   require twice that just for their supporting tables.)
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
  // Code below works with raw memory.
  var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
    _unchecked: textBuffer,
    count: Int(bufferLength))
  for i in 0..<Int(bufferLength) {
    unsafe buffer[unchecked: i] = 0x30
  }
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
}

// Convert a Float16 to an optimal ASCII representation.
// Inputs:
// * `value`: Float16 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 20 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`

// Notes on the implementation here: This implementation writes out
// all integer values in non-exponential form.  This is technically
// not "optimal".  For example, "65504.0" shows 5 significant digits,
// even though the 3 significant digits of "6.55e+04" is sufficient.
// But that just looks silly.
//
// As a side-effect of the above, we end up handling integer values as
// a special case, so the general path only has to deal with values
// whose binary exponent is negative.  This means they are handled by
// multiplying by a positive power of ten, which can be done exactly
// with integer arithmetic due to the small range of Float16.
@available(SwiftStdlib 5.3, *)
internal func _Float16ToASCII(
  value f: Float16,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // We need a MutableRawSpan in order to use wide store/load operations
  // TODO: Tune this value down to the actual minimum for Float16
  assert(utf8Buffer.count >= 20)
  var buffer = unsafe utf8Buffer.mutableBytes

  let significandBitCount = Float16.significandBitCount + 1 // 11
  let baseExponentBias = (1 << (Float16.exponentBitCount - 1)) - 2 // 14
  let exponentBias = baseExponentBias + significandBitCount // 14 + 11 = 25
  let binaryExponent: Int
  let significand: Float16.RawSignificand

  // Step 1: Handle the special cases, decompose the input

  if f.exponentBitPattern == 0x1f {
    if (f.isInfinite) {
      return _infinity(buffer: &buffer, sign: f.sign)
    } else { // f.isNaN
      let quietBit =
        (f.significandBitPattern >> (Float16.significandBitCount - 1)) & 1
      let payloadMask = UInt16(1 << (Float16.significandBitCount - 2)) - 1
      let payload16 = f.significandBitPattern & payloadMask
      return nan_details(
        buffer: &buffer,
        sign: f.sign,
        quiet: quietBit != 0,
        payloadHigh: 0,
        payloadLow: UInt64(truncatingIfNeeded:payload16))
    }
  } else if f.exponentBitPattern == 0 {
    if (f.isZero) {
      return _zero(buffer: &buffer, sign: f.sign)
    } else { // f.isSubnormal
      significand = f.significandBitPattern
      binaryExponent = 1 &- exponentBias
    }
  } else {
    // Normal
    let b = Int(f.exponentBitPattern) &- exponentBias
    let implicitBit = Float16.RawSignificand(1) << Float16.significandBitCount
    let s = f.significandBitPattern &+ implicitBit
    if b > 0 {
      // Special handling for integer values, as described above.
      let base10Significand = UInt32(s) &<< b
      let digits = _intToEightDecimalDigits(base10Significand)
      var start = 1
      buffer.storeBytes(
        of: digits &+ 0x3030303030303030,
        toByteOffset: start,
        as: UInt64.self)
      var end = start + 8

      // Trim leading zeros and insert `-` if appropriate:
      start += digits.trailingZeroBitCount / 8
      if f.sign == .minus {
        buffer.storeBytes(
          of: 0x2d, // "-"
          toByteOffset: start &- 1,
          as: UInt8.self)
        start &-= 1
      }

      // Add ".0" at end
      buffer.storeBytes(
        of: 0x2e, // "."
        toByteOffset: end,
        as: UInt8.self)
      end += 2
      return start..<end
    } else {
      significand = s
      binaryExponent = b
    }
  }

  // Step 2: Compute the base 10 exponent to match the rounding interval
  // log10(2) * 2^8 ~= 77     log10(3/4) * 2^8 ~= -32
  let maybeLogThreeQuarters = f.significandBitPattern == 0 ? -32 : 0
  let rawBase10Power = binaryExponent &* 77 &+ maybeLogThreeQuarters
  let roundedBase10Power = (rawBase10Power &+ 0xff) >> 8
  var base10Power = Int(truncatingIfNeeded: roundedBase10Power)

  // Step 3: Look up power-of-10 scale factor
  let powerOfTen = UInt64(powersOf10_Float16[0 &- base10Power])

  // Step 4: Scale the interval (with rounding)

  // If significand is odd, we need to narrow the interval
  let narrowInterval = f.significandBitPattern & 1

  // Scale 1 binary ULP
  var scaledBinaryUlp = powerOfTen &<< (32 &+ binaryExponent)
  scaledBinaryUlp &+= 2 &- 4 &* UInt64(narrowInterval)

  // We'll use 32.32 fixed point from here on...
  let fractionBits = 32
  let fractionMask = (UInt64(1) << fractionBits) - 1
  let oneHalf = UInt64(1) << (fractionBits - 1)

  // Scaled upper limit of the rounding interval
  let u0 = powerOfTen &* UInt64(significand)
  let u1 = u0 &<< (32 &+ binaryExponent)
  let upperBound = u1 &+ (scaledBinaryUlp >> 1)
  var base10Significand = UInt32(truncatingIfNeeded: upperBound >> fractionBits)
  let delta = upperBound & fractionMask

  // Scaled distance from the value above to the lower bound
  // of the rounding interval.
  var intervalWidth = scaledBinaryUlp
  if f.significandBitPattern == 0 {
    intervalWidth &-= intervalWidth >> 2
  }

  // Step 6: Compute one more decimal digit if necessary
  if intervalWidth < delta {
    // Adjust intervalWidth and delta to measure from the
    // actual target value to the lower bound of the rounding
    // interval.  (Up to this point, they've measured from the upper
    // bound of the rounding interval.)
    let halfUlp = scaledBinaryUlp &* 5
    var delta = delta &* 10 &- halfUlp
    intervalWidth = intervalWidth &* 10 &- halfUlp

    // Compute the additional digit
    var digit = UInt8(truncatingIfNeeded: delta >> fractionBits)
    delta &= fractionMask

    // Note that `digit` is the largest value below the target value;
    // `digit + 1` is the smallest value above the target value.  We
    // want to select whichever is closer to the target value.
    let greater = unsafe unsafeBitCast(delta > oneHalf, to: UInt8.self)
    // If they're equally close, pick the even one
    let greaterOrEqual = unsafe unsafeBitCast(delta >= oneHalf, to: UInt8.self)
    // If the lower one is not in the interval, choose the higher one
    let outOfInterval = unsafe unsafeBitCast(delta > intervalWidth, to: UInt8.self)
    digit &+= greater | (greaterOrEqual & digit) | outOfInterval

    // Add the new digit into the base10Significand
    base10Significand = base10Significand &* 10 &+ UInt32(digit)
    base10Power &-= 1
  }

  // Step 7: Convert the digits and write them out...
  var digits = _intToEightDecimalDigits(base10Significand)
  let trailingZeros = digits.leadingZeroBitCount / 8
  digits &<<= trailingZeros * 8
  base10Power &+= trailingZeros

  let startOffset = 6
  buffer.storeBytes(
    of: digits &+ 0x3030303030303030,
    toByteOffset: startOffset,
    as: UInt64.self)
  let firstDigit = startOffset &+ digits.trailingZeroBitCount / 8
  let nextDigit = startOffset &+ 8

  // Step 8: Finish formatting
  return _finishFormatting(
    buffer: &buffer,
    sign: f.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: false,
    base10Power: base10Power)
}

fileprivate let powersOf10_Float16: _InlineArray<_, UInt32> = [
  1, 10, 100, 1000, 10000, 100000, 1000000, 10000000
]

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
  // Code below works with raw memory.
  var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
    _unchecked: textBuffer,
    count: Int(bufferLength))
  for i in 0..<Int(bufferLength) {
    unsafe buffer[unchecked: i] = 0x30
  }
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
}

// Convert a Float32 to an optimal ASCII representation.
// Inputs:
// * `value`: Float32 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 20 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`
internal func _Float32ToASCII(
  value f: Float32,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  // We need a MutableRawSpan in order to use wide store/load operations
  // TODO: `assert` that the buffer is filled with 0x30 bytes (in debug builds)
  assert(utf8Buffer.count >= 20)
  var buffer = unsafe utf8Buffer.mutableBytes

  let significandBitCount = Float.significandBitCount + 1 // 24
  let exponentBias = (1 << (Float.exponentBitCount - 1)) - 2 + significandBitCount // 126 + 24
  let binaryExponent: Int
  let significand: Float.RawSignificand

  // Step 1: Decompose the input, handle the special cases

  if f.exponentBitPattern == 0xff {
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
  } else if f.exponentBitPattern == 0 {
    if (f.isZero) {
      return _zero(buffer: &buffer, sign: f.sign)
    } else { // f.isSubnormal
      significand = f.significandBitPattern
      binaryExponent = 1 &- exponentBias
    }
  } else {
    // Normal
    let implicitBit = Float.RawSignificand(1) << Float.significandBitCount
    significand = f.significandBitPattern &+ implicitBit
    binaryExponent = Int(f.exponentBitPattern) &- exponentBias
  }

  // Step 2: Compute the base 10 exponent to match the rounding interval

  // Following Schubfach, let RI = the width of the rounding interval
  // Compute base10Power such that
  //    10^base10Power > RI > 10^(base10Power - 1)
  // This guarantees:
  // * AT MOST one multiple of 10^base10Power in our rounding interval
  //   So if we find one, it's unique and we're done!
  // * AT LEAST one multiple of 10^(base10Power-1) in that interval.
  //   So if we don't find a multiple of 10^base10Power, then
  //   we're certain to find a solution with just one more digit.

  // In the common case where RI = 2^e exactly, this implies that:
  //    base10Power = ceiling(e * log10(2))
  // For an exact power of 2, the rounding interval is 25% narrower, so
  //    base10Power = ceiling(e * log10(2) + log10(0.75))
  let maybeLogThreeQuarters = (f.significandBitPattern == 0
                                 ? Int64(-536607787)
                                 : Int64(0))
  let rawBase10Power = Int64(binaryExponent) &* 1292913986 &+ maybeLogThreeQuarters
  let roundedBase10Power = (rawBase10Power &+ 0xffffffff) &>> 32
  var base10Power = Int(truncatingIfNeeded: roundedBase10Power)

  // Step 3: Look up power-of-10 scale factor

  // To obtain a binary floating-point value for the power of 10, look
  // up the significand in a table and compute the corresponding
  // exponent.  The power of 10 is then:
  //     powerOfTen * 2^powerOfTenExponent
  let powerOfTen = powersOf10_Float32[33 &- base10Power]
  let powerOfTenExponent = binaryExponentFor10ToThe(0 &- base10Power)

  // Step 4: Scale the interval (with rounding)
  // We compute `upperBound` the upper bound of the rounding interval and
  // `intervalWidth` the width, both scaled by 10^(-base10Power)

  // After scaling, we need enough integer bits to represent the
  // scaled significand.  The worst case is the asymmetric case: then
  // 10^p > 3/4 * 2^e, so the maximum possible value of 2^e 10^-p is
  // 4/3, which means we'll need one more bit than the binary
  // significand.
  let integerBits = significandBitCount + 1
  let fractionBits = 64 - integerBits
  let fractionMask = (UInt64(1) << fractionBits) - 1
  let oneHalf = UInt64(1) << (fractionBits - 1)

  // If the significand is even, we want to widen the interval by
  // rounding up the upper midpoint and increasing intervalWidth.
  // Conversely if the significand is odd.  This ensures that the
  // exact endpoints of the rounding interval are handled correctly.
  let narrowInterval = f.significandBitPattern & 1

  // Scale one binary ULP
  let align = integerBits &- (binaryExponent &+ powerOfTenExponent)
  var scaledBinaryUlp = powerOfTen &>> align
  // Widen or narrow the Ulp estimate -- this will flow through
  // to the other places we need even/odd adjustments.
  scaledBinaryUlp &+= 3 &- 6 &* UInt64(narrowInterval)

  // Scaled upper bound of the rounding interval
  let mask32 = UInt64(UInt32.max)
  let low = (powerOfTen & mask32) &* UInt64(significand)
  let high = (powerOfTen &>> 32) &* UInt64(significand)
  // (high << 32) + low is the 96-bit intermediate value. Combine and
  // align to 64 bit fixed-point, and add 1/2 ULP for the upper bound.
  let upperBound = ((low &>> align)
                      &+ (high &<< (32 &- align))
                      &+ (scaledBinaryUlp &>> 1))

  // Width of the scaled rounding interval
  // In the non-power-of-two case, the rounding interval has width 1 ULP.
  var intervalWidth = scaledBinaryUlp
  if f.significandBitPattern == 0 {
    // In the power-of-two case, the interval is 3/4 ULP.
    intervalWidth &-= intervalWidth &>> 2
  }

  // Step 5: Emit most of the decimal digits into the destination buffer

  // firstDigit starts at 6 to give us room to insert an initial minus
  // sign and/or leading zeros.  This is used by the ergonomic formatting
  // logic done in `finishFormatting()`.
  var firstDigit = 6
  var nextDigit = firstDigit

  // Split the scaled upperBound into the integer part
  // (initial base-10 significand) and fraction `delta` (distance
  // between that significand and the upper bound of the
  // interval).
  let base10Significand = UInt32(truncatingIfNeeded: upperBound &>> fractionBits)
  var delta = upperBound & fractionMask

  // Convert the digits we have so far and write them out...  We could
  // delay this and fold an extra digit (if necessary) into the
  // base10Significand.  That would work because a UInt32 can hold any
  // 9-digit integer.  It would allow us to mimic the Float16
  // structure above, where this entire function basically just
  // converts a pair of integers (significand, binaryExponent) into
  // another pair of integers (base10Significand, base10Power) and
  // then at the end converts that pair of integers into a textual
  // representation.

  // But if we do the conversion here, we know we don't have more than
  // 8 digits, which eliminates the need to do extra fussing with the
  // ninth digit.
  let digits = _intToEightDecimalDigits(base10Significand)
  buffer.storeBytes(
    of: digits + 0x3030303030303030,
    toByteOffset: firstDigit,
    as: UInt64.self)
  nextDigit &+= 8

  // Trim leading zero digits
  // (Little-endian assumption means that the _trailing_
  // zero digits in `digits` are the most-significant ones.)
  firstDigit &+= digits.trailingZeroBitCount / 8

  // Step 6: Compute one more decimal digit if necessary

  // Because of how we computed base10Power above, either the digits
  // so far are in the rounding interval and we're essentially done,
  // or we need exactly one more digit.

  // Intuitively: `intervalWidth` is the width of the rounding interval,
  // `delta` is the distance from the value we've computed so far
  // to our current target value.
  if intervalWidth > delta {
    // Step 6, Case A: We have a value that lies within the rounding
    // interval, and it's a multiple of 10^base10Power.  As described
    // earlier, there can only be one such. This is the fast case.

    // Experimentally, ~39% of all inputs get here.

    // Trim trailing zeros
    let trailingZeros = digits.leadingZeroBitCount / 8
    nextDigit &-= trailingZeros
    base10Power &+= trailingZeros
    // No final-digit adjustment is needed
  } else {
    // Step 6, Case B: We need another digit
    // Experimentally, ~61% of all inputs get here.

    // Adjust `delta` to measure the distance from the value we've computed so
    // far to the actual float value (instead of the upper endpoint).  This
    // requires subtracting 1/2 ULP from both delta and intervalWidth:
    let halfUlp = scaledBinaryUlp &* 5
    // This is `delta -= ulp/2; delta *= 10` reordered to reduce precision loss
    delta = delta &* 10 &- halfUlp
    // Adjust `intervalWidth` to reflect only the lower part of the rounding interval
    intervalWidth = intervalWidth &* 10 &- halfUlp

    // Multiplying by 10 above scaled us from 10^base10Power
    // to 10^(base10Power - 1), which gives us the extra digit we want:
    var digit = UInt8(truncatingIfNeeded: delta &>> fractionBits)
    delta &= fractionMask

    // `delta` is now the scaled distance from our current base-10
    // value to the actual binary target value. `digit` is the closest
    // base-10 digit _below_ our target, `digit + 1` is the closest
    // _above_ the target.  We need to choose between `digit` and `digit + 1`.

    // Note: Using arithmetic to combine booleans as below is considerably
    // faster than the obvious if-else chain.
    let epsilon = UInt64(64)
    // If `delta` is definitively > 1/2, then it's further away than `delta + 1`
    let greater = unsafe unsafeBitCast(delta > (oneHalf &+ epsilon), to: UInt8.self)
    // They might be equally close (ignoring precision loss in the earlier calculations),
    // in which case we will want the even one.
    let greaterOrEqual = unsafe unsafeBitCast(delta > (oneHalf &- epsilon), to: UInt8.self)
    // The lower value could be outside of the interval (only in the asymmetric
    // exact-power-of-two case), in which case, we definitely want the upper one:
    let outOfInterval = unsafe unsafeBitCast(delta > intervalWidth, to: UInt8.self)
    // Select the upper value if any of these three conditions holds:
    digit &+= greater | (greaterOrEqual & digit) | outOfInterval

    // Write out the extra digit
    buffer.storeBytes(
      of: digit &+ 0x30,
      toByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    base10Power &-= 1
  }

  // Step 7: Finish formatting
  let isBoundary = (f.significandBitPattern == 0)
  let forceExponential = ((binaryExponent > 1) || (binaryExponent == 1 && !isBoundary))
  return _finishFormatting(
    buffer: &buffer,
    sign: f.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Power: base10Power)
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
  // Code below works with raw memory.
  var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
    _unchecked: textBuffer,
    count: Int(bufferLength))
  for i in 0..<Int(bufferLength) {
    unsafe buffer[unchecked: i] = 0x30
  }
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
}

// Convert a Float64 to an optimal ASCII representation.
// See the Float32 implementation for extensive comments
// explaining the algorithm.
// Inputs:
// * `value`: Float64 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 32 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<32,UTF8.CodeUnit>(repeating:0x30)`
internal func _Float64ToASCII(
  value d: Float64,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  assert(utf8Buffer.count >= 32)
  var buffer = unsafe utf8Buffer.mutableBytes

  let significandBitCount = Double.significandBitCount + 1 // 53
  let exponentBias = 1022 + 53
  let binaryExponent: Int
  let significand: Double.RawSignificand

  // Step 1: Handle the special cases, decompose the input

  if  d.exponentBitPattern == 0x7ff {
    if (d.isInfinite) {
      return _infinity(buffer: &buffer, sign: d.sign)
    } else { // d.isNaN
      let quietBit =
        (d.significandBitPattern >> (Double.significandBitCount - 1)) & 1
      let payloadMask = (UInt64(1) << (Double.significandBitCount - 2)) - 1
      let payload64 = d.significandBitPattern & payloadMask
      return nan_details(
        buffer: &buffer,
        sign: d.sign,
        quiet: quietBit != 0,
        payloadHigh: 0,
        payloadLow: payload64
      )
    }
  } else if d.exponentBitPattern == 0 {
    if (d.isZero) {
      return _zero(buffer: &buffer, sign: d.sign)
    } else { // d.isSubnormal
      significand = d.significandBitPattern
      binaryExponent = 1 &- exponentBias
    }
  } else {
    // Normal
    let implicitBit = Double.RawSignificand(1) << Double.significandBitCount
    significand = d.significandBitPattern &+ implicitBit
    binaryExponent = Int(d.exponentBitPattern) &- exponentBias
  }

  // Step 2: Compute the base 10 exponent to match the rounding interval

  let maybeLogThreeQuarters = (d.significandBitPattern == 0
                                 ? Int64(-536607787)
                                 : Int64(0))
  let rawBase10Power = Int64(binaryExponent) &* 1292913986 &+ maybeLogThreeQuarters
  let roundedBase10Power = (rawBase10Power &+ 0xffffffff) &>> 32
  var base10Power = Int(truncatingIfNeeded: roundedBase10Power)

  // Step 3: Look up power-of-10 scale factor

  var powerOfTen: _UInt128 = 0
  let powerOfTenExponent = _powerOf10_Binary64(p: 0 &- base10Power,
                                               significand: &powerOfTen)

  // Step 4: Scale the interval (with rounding)

  // After scaling, we'll use a 54.74 fixed-point format
  let integerBits = significandBitCount + 1
  let fractionBits = 128 - integerBits
  let fractionMask = (_UInt128(1) << fractionBits) - 1
  let oneHalf = _UInt128(1) << (fractionBits - 1)

  let narrowInterval = d.significandBitPattern & 1

  // Scale one binary ULP
  let align = integerBits &- (binaryExponent &+ powerOfTenExponent)
  var scaledBinaryUlp = powerOfTen &>> align
  scaledBinaryUlp &+= _UInt128(bitPattern: _Int128(3 &- 6 &* Int(narrowInterval)))

  // Scale the upper limit
  let low = _UInt128(powerOfTen._low) &* _UInt128(significand)
  let high = _UInt128(powerOfTen._high) &* _UInt128(significand)
  let upperBound = ((low &>> align)
                      &+ (high &<< (64 - align))
                      &+ (scaledBinaryUlp &>> 1))

  // Compute the width of the scaled rounding interval
  var intervalWidth = scaledBinaryUlp
  if d.significandBitPattern == 0 {
    // In the power-of-two case, the interval is 3/4 ULP.
    intervalWidth &-= intervalWidth &>> 2
  }

  // Step 5: Emit most of the decimal digits into the destination buffer

  var firstDigit = 6
  var nextDigit = firstDigit

  let base10Significand = UInt64(truncatingIfNeeded: upperBound >> fractionBits)
  var delta = upperBound & fractionMask

  // Convert the digits we have so far and write them out...
  let digits = _intToSixteenDecimalDigits(base10Significand)
  let zeros = _UInt128(
    _low: 0x3030303030303030,
    _high: 0x3030303030303030)
  buffer.storeBytes(
    of: digits &+ zeros,
    toByteOffset: firstDigit,
    as: _UInt128.self)
  nextDigit &+= 16

  // Trim leading zero digits
  firstDigit &+= digits.trailingZeroBitCount / 8

  // Step 6: Compute one more decimal digit if necessary

  if intervalWidth > delta {
    // Step 6, Case A: We have a value that lies within the interval
    // We only need to trim trailing zeros
    let trailingZeros = digits.leadingZeroBitCount / 8
    nextDigit &-= trailingZeros
    base10Power &+= trailingZeros
    // No final-digit adjustment is needed
  } else {
    // Step 6, Case B: We need another digit
    let halfUlp = scaledBinaryUlp &* 5
    delta = delta &* 10 &- halfUlp
    intervalWidth = intervalWidth &* 10 &- halfUlp

    var digit = UInt8(truncatingIfNeeded: delta >> fractionBits)
    delta &= fractionMask

    let epsilon = _UInt128(64)
    let greater = unsafe unsafeBitCast(delta > (oneHalf &+ epsilon), to: UInt8.self)
    let greaterOrEqual = unsafe unsafeBitCast(delta > (oneHalf &- epsilon), to: UInt8.self)
    let outOfInterval = unsafe unsafeBitCast(delta > intervalWidth, to: UInt8.self)
    digit &+= greater | (greaterOrEqual & digit) | outOfInterval

    buffer.storeBytes(
      of: digit &+ 0x30,
      toByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    base10Power &-= 1
  }

  // Step 7: Finish formatting
  let isBoundary = (d.significandBitPattern == 0)
  let forceExponential =
    ((binaryExponent > 1)
       || (binaryExponent == 1 && !isBoundary))
  return _finishFormatting(
    buffer: &buffer,
    sign: d.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Power: base10Power)
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
  // Code below works with raw memory.
  var buffer = unsafe MutableSpan<UTF8.CodeUnit>(
    _unchecked: textBuffer,
    count: Int(bufferLength))
  for i in 0..<Int(bufferLength) {
    unsafe buffer[unchecked: i] = 0x30
  }
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
}

// Convert a Float80 to an optimal ASCII representation.
// See the Float32 implementation for extensive comments
// explaining the algorithm.
// Inputs:
// * `value`: Float80 input
// * `buffer`: Buffer to place the result
// Returns: Range of bytes within `buffer` that contain the result
//
// Buffer must be at least 40 bytes long and must be pre-filled
// with "0" characters, e.g., via
// `InlineArray<40,UTF8.CodeUnit>(repeating:0x30)`
internal func _Float80ToASCII(
  value f: Float80,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>
) -> Range<Int> {
  assert(utf8Buffer.count >= 40)
  var buffer = unsafe utf8Buffer.mutableBytes

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
  let significandBits = Float80.significandBitCount + 1
  let exponentBias = (1 << (Float80.exponentBitCount - 1)) - 2 + significandBits // 16382 + 64
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
    } else { // Subnormal
      binaryExponent = 1 &- exponentBias
      significand = rawSignificand
    }
  } else if rawSignificand >> 63 == 1 { // Normal (J-bit set)
    binaryExponent = Int(f.exponentBitPattern) &- exponentBias
    significand = rawSignificand
  } else {
    // Invalid: non-zero exponent with J-bit clear ("unnormal")
    return nan_details(
      buffer: &buffer,
      sign: .plus,
      quiet: true,
      payloadHigh: 0,
      payloadLow: 0)
  }

  // Step 2: Compute the base 10 exponent to match the rounding interval

  let maybeLogThreeQuarters = (isBoundary
                                 ? Int64(-536607787)
                                 : Int64(0))
  let rawBase10Power = Int64(binaryExponent) &* 1292913986 &+ maybeLogThreeQuarters
  let roundedBase10Power = (rawBase10Power &+ 0xffffffff) &>> 32
  var base10Power = Int(truncatingIfNeeded: roundedBase10Power)

  // Step 3: Look up power-of-10 scale factor

  var powerOfTen = _UInt256()
  let powerOfTenExponent = _powerOf10_Binary128(
    p: 0 &- base10Power,
    significand: &powerOfTen)

  // Step 4: Scale the interval (with rounding)

  // Fixed-point format: 65 integer bits, 191 fraction bits = 256 total
  // The integer part can be up to 65 bits (max ≈ 4/3 * 2^64 ≈ 2.46e19)
  let integerBits = significandBits + 1
  let fractionMask = _UInt256(high: 0, UInt64.max >> 1, UInt64.max, low: UInt64.max)
  let oneHalf = _UInt256(high: 0, UInt64(1) << 62, 0, low: 0)

  let narrowInterval = f.significandBitPattern & 1

  // Scale one binary ULP: scaledBinaryUlp = powerOfTen >> align
  let align = integerBits &- (binaryExponent &+ powerOfTenExponent)
  var scaledBinaryUlp = powerOfTen &>> align
  // Widen or narrow the ULP for even/odd significand
  let adjustment = 4 &- 8 &* Int(narrowInterval)
  scaledBinaryUlp &+= adjustment

  // Scale the upper limit: multiply 256-bit powerOfTen by 64-bit significand
  // Product is 320 bits; we extract a 256-bit window shifted by `align`.
  let sig = _UInt128(significand)
  let w0 = _UInt128(powerOfTen.low._low) &* sig
  let w1 = _UInt128(powerOfTen.low._high) &* sig
  let w2 = _UInt128(powerOfTen.high._low) &* sig
  let w3 = _UInt128(powerOfTen.high._high) &* sig

  // Accumulate columns of the 320-bit product
  let col1 = _UInt128(w0._high) &+ _UInt128(w1._low)
  let col2 = _UInt128(col1._high) &+ _UInt128(w1._high) &+ _UInt128(w2._low)
  let col3 = _UInt128(col2._high) &+ _UInt128(w2._high) &+ _UInt128(w3._low)
  let col4 = _UInt128(col3._high) &+ _UInt128(w3._high)

  // Top 256 bits of 320-bit product (= product >> 64)
  let productTop = _UInt256(
    high: col4._low, col3._low, col2._low, low: col1._low)

  // Shift by remaining (align - 64) bits.
  // align is in roughly [62, 68].
  let subAlign = align &- 64
  let shifted: _UInt256
  if subAlign > 0 {
    shifted = productTop &>> subAlign
  } else if subAlign < 0 {
    // Need to shift left and include bits from column 0 (w0._low)
    let leftShift = 0 &- subAlign
    let anti = 64 &- leftShift
    shifted = _UInt256(
      high: (productTop.high._high << leftShift) | (productTop.high._low >> anti),
      (productTop.high._low << leftShift) | (productTop.low._high >> anti),
      (productTop.low._high << leftShift) | (productTop.low._low >> anti),
      low: (productTop.low._low << leftShift) | (w0._low >> anti))
  } else {
    shifted = productTop
  }

  let upperBound = shifted &+ (scaledBinaryUlp &>> 1)

  // Compute the width of the scaled rounding interval
  var intervalWidth = scaledBinaryUlp
  if isBoundary {
    // Power-of-two: interval is 3/4 ULP
    intervalWidth = intervalWidth &- (intervalWidth &>> 2)
  }

  // Step 5: Emit most of the decimal digits into the destination buffer

  // Float80 requires 1-21 digits for round-trip correctness; our
  // initial scaling will produce a value of up to 65 bits (0-20
  // digits) at this point.

  var firstDigit = 8
  var nextDigit = firstDigit

  // Extract the 65-bit integer part
  let base10Significand = upperBound.high >> 63
  var delta = upperBound & fractionMask

  // Convert to decimal digits: split at 10^16
  // max value ≈ 4/3 * 2^64 ≈ 2.46e19, so hi4 ≤ 2460 (4 digits)
  let hi4 = UInt32(truncatingIfNeeded: base10Significand / 10000000000000000)
  let lo16 = UInt64(truncatingIfNeeded: base10Significand % 10000000000000000)
  let digitsHi = _intToEightDecimalDigits(hi4)
  let digitsLo = _intToSixteenDecimalDigits(lo16)
  buffer.storeBytes(
    of: digitsHi &+ 0x3030303030303030,
    toByteOffset: firstDigit,
    as: UInt64.self)
  let zeros16 = _UInt128(
    _low: 0x3030303030303030,
    _high: 0x3030303030303030)
  buffer.storeBytes(
    of: digitsLo &+ zeros16,
    toByteOffset: firstDigit &+ 8,
    as: _UInt128.self)
  nextDigit &+= 24

  // Trim leading zero digits
  if digitsHi == 0 {
    firstDigit &+= 8 &+ digitsLo.trailingZeroBitCount / 8
  } else {
    firstDigit &+= digitsHi.trailingZeroBitCount / 8
  }

  // Step 6: Compute one more decimal digit if necessary

  if intervalWidth > delta {
    // Step 6, Case A: Value lies within the interval.
    // Trim trailing zeros.
    let trailingZeros: Int
    if digitsLo == 0 {
      trailingZeros = 16 &+ digitsHi.leadingZeroBitCount / 8
    } else {
      trailingZeros = digitsLo.leadingZeroBitCount / 8
    }
    nextDigit &-= trailingZeros
    base10Power &+= trailingZeros
  } else {
    // Step 6, Case B: We need another digit
    var halfUlp = scaledBinaryUlp
    halfUlp.multiply(by: UInt32(5))
    // delta = delta * 10 - scaledBinaryUlp * 5
    delta.multiply(by: UInt32(10))
    delta = delta &- halfUlp
    // intervalWidth = intervalWidth * 10 - scaledBinaryUlp * 5
    intervalWidth.multiply(by: UInt32(10))
    intervalWidth = intervalWidth &- halfUlp

    // Extract the next digit
    var digit = UInt8(truncatingIfNeeded: delta.high >> 63)
    delta = delta & fractionMask

    let epsilon = _UInt256(UInt64(256))
    let greater = unsafe unsafeBitCast(
      delta > (oneHalf &+ epsilon), to: UInt8.self)
    let greaterOrEqual = unsafe unsafeBitCast(
      delta > (oneHalf &- epsilon), to: UInt8.self)
    let outOfInterval = unsafe unsafeBitCast(
      delta > intervalWidth, to: UInt8.self)
    digit &+= greater | (greaterOrEqual & digit) | outOfInterval

    buffer.storeBytes(
      of: digit &+ 0x30,
      toByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    base10Power &-= 1
  }

  // Step 7: Finish formatting
  let forceExponential =
    ((binaryExponent > 1)
       || (binaryExponent == 1 && !isBoundary))
  return _finishFormatting(
    buffer: &buffer,
    sign: f.sign,
    firstDigit: firstDigit,
    nextDigit: nextDigit,
    forceExponential: forceExponential,
    base10Power: base10Power)
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
// at least 6 unused bytes at the beginning of `buffer` before
// `firstDigit` and that all unused bytes are filled with `"0"` (0x30)
// characters.

fileprivate func _finishFormatting(
  buffer: inout MutableRawSpan,
  sign: FloatingPointSign,
  firstDigit: Int,
  nextDigit: Int,
  forceExponential: Bool,
  base10Power: Int
) -> Range<Int> {
  // Performance note: This could be made noticeably faster by
  // writing the output consistently in exponential form with no
  // decimal point, e.g., "31415926e-07".  But the extra cost seems
  // worthwhile to achieve "3.1415926" instead.
  var firstDigit = firstDigit
  var nextDigit = nextDigit

  let digitCount = nextDigit &- firstDigit
  var p = base10Power &+ digitCount &- 1
  if p < -4 || forceExponential {
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
        of: 0x2e, // "."
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
    let powerSign: UInt8
    if p < 0 {
      powerSign = 0x2d // "-"
      p = 0 &- p
    } else {
      powerSign = 0x2b // "+"
    }
    unsafe buffer.storeBytes(
      of: powerSign,
      toUncheckedByteOffset: nextDigit,
      as: UInt8.self)
    nextDigit &+= 1
    if p > 99 {
      if p > 999 {
        let d = asciiDigitTable[p / 100]
        unsafe buffer.storeBytes(
          of: d,
          toUncheckedByteOffset: nextDigit,
          as: UInt16.self)
        nextDigit &+= 2
      } else {
        let d = 0x30 &+ UInt8(truncatingIfNeeded: (p / 100))
        unsafe buffer.storeBytes(
          of: d,
          toUncheckedByteOffset: nextDigit,
          as: UInt8.self)
        nextDigit &+= 1
      }
      p = p % 100
    }
    // For historical reasons, exponents are always at least 2 digits
    let d = unsafe asciiDigitTable[unchecked: p]
    buffer.storeBytes(
      of: d,
      toByteOffset: nextDigit,
      as: UInt16.self)
    nextDigit &+= 2
  } else if p < 0 {
    // "-0.000123456789"
    // We need up to 6 leading characters before the digits.
    // Note that the formatters above all insert extra leading "0" characters
    // to the beginning of the buffer, so we don't need to memset() here,
    // just back up the start to include them...
    firstDigit &+= p &- 1
    // ... and then overwrite a decimal point to get "0." at the beginning
    buffer.storeBytes(
      of: 0x2e, // "."
      toByteOffset: firstDigit &+ 1,
      as: UInt8.self)
  } else if p &+ 1 < digitCount {
    // "123456.789"
    // We move the first digits forward one position
    // so we can insert a decimal point in the middle.
    // Note: This is the only case where we actually move
    // more than one digit around in the buffer.
    // TODO: Find out how to use C memmove() here
    firstDigit &-= 1
    for i in 0...(p &+ 1) {
      let t = unsafe buffer.unsafeLoad(
        fromUncheckedByteOffset: firstDigit &+ i &+ 1,
        as: UInt8.self)
      unsafe buffer.storeBytes(
        of: t,
        toUncheckedByteOffset: firstDigit &+ i,
        as: UInt8.self)
    }
    buffer.storeBytes(
      of: 0x2e, // "."
      toByteOffset: firstDigit &+ p &+ 1,
      as: UInt8.self)
  } else {
    // "12345678900.0"
    // Fill trailing zeros, put ".0" at the end
    // so the result is obviously floating-point.
    // Remember buffer was initialized with "0"
    nextDigit = firstDigit &+ p &+ 3
    buffer.storeBytes(
      of: 0x2e, // "."
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
fileprivate let asciiDigitTable: _InlineArray<100, UInt16> = [
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

fileprivate let hexdigits: _InlineArray<16, UInt8> = [
  0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
  0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66
]

// Emit up to 16 hex digits, pruning leading zeros
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

// Emit exactly 16 hex digits
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

// Convert an integer less than 10^8 into exactly 8 decimal digits in a
// UInt64.  Assuming little-endian, you can add 0x3030303030303030 and
// store the result directly to memory to get an ASCII decimal.
//
// This implementation is based on work by Paul Khuong:
// https://pvk.ca/Blog/2017/12/22/appnexus-common-framework-its-out-also-how-to-print-integers-faster/
@inline(__always)
fileprivate func _intToEightDecimalDigits(_ n: UInt32) -> UInt64 {
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

  return singles
}

@inline(__always)
fileprivate func _intToSixteenDecimalDigits(_ n: UInt64) -> _UInt128 {
  let hi8 = n / 100000000
  let lo8 = n &- hi8 * 100000000

  return _UInt128(
    _low: _intToEightDecimalDigits(UInt32(truncatingIfNeeded: hi8)),
    _high: _intToEightDecimalDigits(UInt32(truncatingIfNeeded: lo8))
  )
}

// ================================================================
//
// Arithmetic Helpers
//
// ================================================================

// Custom 256-bit unsigned integer type, with various arithmetic
// helpers as methods.
// Used by 80- and 128-bit floating point formatting logic above...
fileprivate struct _UInt256 {
  var high: _UInt128
  var low: _UInt128

  init() {
    self.high = 0
    self.low = 0
  }

  init(high: UInt64, _ midHigh: UInt64, _ midLow: UInt64, low: UInt64) {
    self.high = _UInt128(_low: midHigh, _high: high)
    self.low = _UInt128(_low: low, _high: midLow)
  }

  init(high: _UInt128, low: _UInt128) {
    self.high = high
    self.low = low
  }

  init(_ low: _UInt128) {
      self.high = 0
      self.low = low
  }

  init(_ low: UInt64) {
      self.high = 0
      self.low = _UInt128(low)
  }

  mutating func multiply(by rhs: UInt32) {
    var t = _UInt128(low._low) &* _UInt128(rhs)
    let newlow = t._low
    t = _UInt128(t._high) &+ _UInt128(low._high) &* _UInt128(rhs)
    low = _UInt128(_low: newlow, _high: t._low)
    t = _UInt128(t._high) &+ _UInt128(high._low) &* _UInt128(rhs)
    let newmidhigh = t._low
    t = _UInt128(t._high) &+ _UInt128(high._high) &* _UInt128(rhs)
    high = _UInt128(_low: newmidhigh, _high: t._low)
    assert(t._high == 0)
  }

  mutating func multiplyRoundingDown(by rhs: _UInt128) {
    var current = _UInt128(low._low) * _UInt128(rhs._low)

    current = _UInt128(current._high)
    var t = _UInt128(low._low) &* _UInt128(rhs._high)
    current += _UInt128(t._low)
    var next = _UInt128(t._high)
    t = _UInt128(low._high) &* _UInt128(rhs._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)

    current = next + _UInt128(current._high)
    t = _UInt128(low._high) &* _UInt128(rhs._high)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(high._low) &* _UInt128(rhs._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    let newlow = current._low

    current = next + _UInt128(current._high)
    t = _UInt128(high._low) &* _UInt128(rhs._high)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(high._high) &* _UInt128(rhs._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    low = _UInt128(_low: newlow, _high: current._low)

    current = next + _UInt128(current._high)
    t = _UInt128(high._high) &* _UInt128(rhs._high)
    high = current + t
  }

  // TODO: Code size could be reduced here either by coding it
  // as a nested for loop with additional bookkeeping, or
  // by recursively using the 256-bit x 128-bit version above.
  mutating func multiplyRoundingDown(by rhs: _UInt256) {
    var current = _UInt128(low._low) * _UInt128(rhs.low._low)

    current = _UInt128(current._high)
    var t = _UInt128(low._low) &* _UInt128(rhs.low._high)
    current += _UInt128(t._low)
    var next = _UInt128(t._high)
    t = _UInt128(low._high) &* _UInt128(rhs.low._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)

    current = next + _UInt128(current._high)
    t = _UInt128(low._low) &* _UInt128(rhs.high._low)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(low._high) &* _UInt128(rhs.low._high)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    t = _UInt128(high._low) &* _UInt128(rhs.low._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)

    current = next + _UInt128(current._high)
    t = _UInt128(low._low) &* _UInt128(rhs.high._high)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(low._high) &* _UInt128(rhs.high._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    t = _UInt128(high._low) &* _UInt128(rhs.low._high)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    t = _UInt128(high._high) &* _UInt128(rhs.low._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)

    current = next + _UInt128(current._high)
    t = _UInt128(low._high) &* _UInt128(rhs.high._high)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(high._low) &* _UInt128(rhs.high._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    t = _UInt128(high._high) &* _UInt128(rhs.low._high)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    let newlow = current._low

    current = next + _UInt128(current._high)
    t = _UInt128(high._low) &* _UInt128(rhs.high._high)
    current += _UInt128(t._low)
    next = _UInt128(t._high)
    t = _UInt128(high._high) &* _UInt128(rhs.high._low)
    current += _UInt128(t._low)
    next += _UInt128(t._high)
    low = _UInt128(_low: newlow, _high: current._low)

    current = next + _UInt128(current._high)
    high = current + _UInt128(high._high) &* _UInt128(rhs.high._high)
  }

  static func &- (lhs: _UInt256, rhs: _UInt256) -> _UInt256 {
    var t = _UInt128(lhs.low._low) &+ _UInt128(~rhs.low._low) &+ 1
    let w0 = t._low
    t = _UInt128(t._high) &+ _UInt128(lhs.low._high) &+ _UInt128(~rhs.low._high)
    let w1 = t._low
    t = _UInt128(t._high) &+ _UInt128(lhs.high._low) &+ _UInt128(~rhs.high._low)
    let w2 = t._low
    let w3 = t._high &+ lhs.high._high &+ ~rhs.high._high
    return _UInt256(high: w3, w2, w1, low: w0)
  }

  static func < (lhs: _UInt256, rhs: _UInt256) -> Bool {
    return (lhs.high < rhs.high)
      || (lhs.high == rhs.high
            && lhs.low < rhs.low)
  }

  static func > (lhs: _UInt256, rhs: _UInt256) -> Bool {
    return rhs < lhs
  }

  static func &+ (lhs: _UInt256, rhs: _UInt256) -> _UInt256 {
    let low = lhs.low &+ rhs.low
    let carry: _UInt128 = low < lhs.low ? 1 : 0
    let high = lhs.high &+ rhs.high &+ carry
    return _UInt256(high: high, low: low)
  }

  static func &+= (lhs: inout _UInt256, rhs: Int) {
    if rhs >= 0 {
      let r = _UInt128(UInt(rhs))
      let newlow = lhs.low &+ r
      let carry: _UInt128 = newlow < lhs.low ? 1 : 0
      lhs.high &+= carry
      lhs.low = newlow
    } else {
      let magnitude = _UInt128(UInt(bitPattern: 0 &- rhs))
      let borrow: _UInt128 = lhs.low < magnitude ? 1 : 0
      lhs.low &-= magnitude
      lhs.high &-= borrow
    }
  }

  static func & (lhs: _UInt256, rhs: _UInt256) -> _UInt256 {
    return _UInt256(high: lhs.high & rhs.high, low: lhs.low & rhs.low)
  }

  // Right-shift by 0..<128 bits
  static func &>> (lhs: _UInt256, rhs: Int) -> _UInt256 {
    var hi = lhs.high._high
    var midhi = lhs.high._low
    var midlo = lhs.low._high
    var lo = lhs.low._low
    // Shift by whole 64-bit words first
    if rhs >= 64 {
      lo = midlo
      midlo = midhi
      midhi = hi
      hi = 0
    }
    // Shift by remaining bits
    let r = rhs & 63
    if r > 0 {
      let anti = 64 &- r
      lo = (lo >> r) | (midlo << anti)
      midlo = (midlo >> r) | (midhi << anti)
      midhi = (midhi >> r) | (hi << anti)
      hi = hi >> r
    }
    return _UInt256(high: hi, midhi, midlo, low: lo)
  }
}

// ================================================================
//
// Powers of 10
//
// ================================================================

@inline(__always)
fileprivate func _powerOf10_Binary64(
  p: Int,
  significand: inout _UInt128
) -> Int {
  if p >= 0 && p <= 55 {
    let upper64 = powersOf10_Exact128[p &* 2 &+ 1]
    let lower64 = powersOf10_Exact128[p &* 2]
    significand = _UInt128(_low: lower64, _high: upper64)
    return binaryExponentFor10ToThe(p)
  }

  let index = p &+ 400
  let mainPower = index / 28
  let baseHigh = powersOf10_Binary64[mainPower &* 2 &+ 1]
  let baseLow = powersOf10_Binary64[mainPower &* 2]
  let extraPower = index &- mainPower &* 28
  let baseExponent = binaryExponentFor10ToThe(p &- extraPower)

  if extraPower == 0 {
    significand = _UInt128(_low: baseLow, _high: baseHigh)
    return baseExponent
  } else {
    let extra = powersOf10_Exact128[extraPower &* 2 &+ 1]
    let high = (_UInt128(truncatingIfNeeded:baseHigh)
                &* _UInt128(truncatingIfNeeded:extra))
    let low = (_UInt128(truncatingIfNeeded:baseLow)
               &* _UInt128(truncatingIfNeeded:extra)
               &+ _UInt128(UInt64.max >> 1))
    significand = high &+ (low &>> 64)
    return baseExponent &+ binaryExponentFor10ToThe(extraPower)
  }
}

// To keep the supporting table size to a minimum, this
// picks a value from each of three tables and multiplies
// them together to get a sufficiently-accurate final result.
fileprivate func _powerOf10_Binary128(
  p: Int,
  significand: inout _UInt256
) -> Int {
  if p >= 0 && p <= 55 {
    let exactLow = powersOf10_Exact128[p * 2]
    let exactHigh = powersOf10_Exact128[p * 2 + 1]
    significand = _UInt256(high: exactHigh, exactLow, 0, low: 0)
    return binaryExponentFor10ToThe(p)
  }

  let index = p + 5376
  let finePower = index % 56
  let mediumPower = index % 896 - finePower
  let coarsePower = p - mediumPower - finePower

  let coarseIndex = (index / 896) * 4
  let mediumIndex = (mediumPower / 56) * 4
  let fineIndex = finePower * 2

  significand = _UInt256(
    high: powersOf10_Binary128_Coarse[coarseIndex + 3],
    powersOf10_Binary128_Coarse[coarseIndex + 2],
    powersOf10_Binary128_Coarse[coarseIndex + 1],
    low: powersOf10_Binary128_Coarse[coarseIndex + 0])

  let medium = _UInt256(
    high: powersOf10_Binary128_Medium[mediumIndex + 3],
    powersOf10_Binary128_Medium[mediumIndex + 2],
    powersOf10_Binary128_Medium[mediumIndex + 1],
    low: powersOf10_Binary128_Medium[mediumIndex + 0])
  significand.multiplyRoundingDown(by: medium)

  let fine = _UInt128(
    _low: powersOf10_Exact128[fineIndex],
    _high: powersOf10_Exact128[fineIndex + 1])
  significand.multiplyRoundingDown(by: fine)

  let coarseExponent = binaryExponentFor10ToThe(coarsePower)
  let mediumExponent = binaryExponentFor10ToThe(mediumPower)
  let fineExponent = binaryExponentFor10ToThe(finePower)

  return coarseExponent + mediumExponent + fineExponent
}

@inline(__always)
fileprivate func binaryExponentFor10ToThe(_ p: Int) -> Int {
  return Int(((Int64(p) &* 55732705) >> 24) &+ 1)
}

// Each of the constant values here have an implicit binary point at the extreme
// left and when not exact, are rounded _down_ from the exact values.  For
// example, the first row of the powersOf10_Binary64 table below says that:
//
//   0x0.95fe7e07c91efafa3931b850df08e738 x 2^-1328
//
// is the result of rounding down the exact binary value of 10^-400 to
// 128 significant bits.  The logic above uses these tables to compute
// bounds for the exact value of the power of 10.

// Note the binary exponent is not stored; it is computed by the
// `binaryExponentFor10ToThe(p)` function.

// ## powersOf10_Float32

// Complete table suitable for Float32 conversion.

fileprivate let powersOf10_Float32: _InlineArray<_, UInt64> = [
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
  0x8000000000000000, // x 2^1 == 10^0 exactly
  0xa000000000000000, // x 2^4 == 10^1 exactly
  0xc800000000000000, // x 2^7 == 10^2 exactly
  0xfa00000000000000, // x 2^10 == 10^3 exactly
  0x9c40000000000000, // x 2^14 == 10^4 exactly
  0xc350000000000000, // x 2^17 == 10^5 exactly
  0xf424000000000000, // x 2^20 == 10^6 exactly
  0x9896800000000000, // x 2^24 == 10^7 exactly
  0xbebc200000000000, // x 2^27 == 10^8 exactly
  0xee6b280000000000, // x 2^30 == 10^9 exactly
  0x9502f90000000000, // x 2^34 == 10^10 exactly
  0xba43b74000000000, // x 2^37 == 10^11 exactly
  0xe8d4a51000000000, // x 2^40 == 10^12 exactly
  0x9184e72a00000000, // x 2^44 == 10^13 exactly
  0xb5e620f480000000, // x 2^47 == 10^14 exactly
  0xe35fa931a0000000, // x 2^50 == 10^15 exactly
  0x8e1bc9bf04000000, // x 2^54 == 10^16 exactly
  0xb1a2bc2ec5000000, // x 2^57 == 10^17 exactly
  0xde0b6b3a76400000, // x 2^60 == 10^18 exactly
  0x8ac7230489e80000, // x 2^64 == 10^19 exactly
  0xad78ebc5ac620000, // x 2^67 == 10^20 exactly
  0xd8d726b7177a8000, // x 2^70 == 10^21 exactly
  0x878678326eac9000, // x 2^74 == 10^22 exactly
  0xa968163f0a57b400, // x 2^77 == 10^23 exactly
  0xd3c21bcecceda100, // x 2^80 == 10^24 exactly
  0x84595161401484a0, // x 2^84 == 10^25 exactly
  0xa56fa5b99019a5c8, // x 2^87 == 10^26 exactly
  0xcecb8f27f4200f3a, // x 2^90 == 10^27 exactly
  0x813f3978f8940984, // x 2^94 ~= 10^28
  0xa18f07d736b90be5, // x 2^97 ~= 10^29
  0xc9f2c9cd04674ede, // x 2^100 ~= 10^30
  0xfc6f7c4045812296, // x 2^103 ~= 10^31
  0x9dc5ada82b70b59d, // x 2^107 ~= 10^32
  0xc5371912364ce305, // x 2^110 ~= 10^33
  0xf684df56c3e01bc6, // x 2^113 ~= 10^34
  0x9a130b963a6c115c, // x 2^117 ~= 10^35
  0xc097ce7bc90715b3, // x 2^120 ~= 10^36
  0xf0bdc21abb48db20, // x 2^123 ~= 10^37
  0x96769950b50d88f4, // x 2^127 ~= 10^38
  0xbc143fa4e250eb31, // x 2^130 ~= 10^39
  0xeb194f8e1ae525fd, // x 2^133 ~= 10^40
  0x92efd1b8d0cf37be, // x 2^137 ~= 10^41
  0xb7abc627050305ad, // x 2^140 ~= 10^42
  0xe596b7b0c643c719, // x 2^143 ~= 10^43
  0x8f7e32ce7bea5c6f, // x 2^147 ~= 10^44
  0xb35dbf821ae4f38b, // x 2^150 ~= 10^45
  0xe0352f62a19e306e, // x 2^153 ~= 10^46
  0x8c213d9da502de45, // x 2^157 ~= 10^47
  0xaf298d050e4395d6, // x 2^160 ~= 10^48
  0xdaf3f04651d47b4c, // x 2^163 ~= 10^49
  0x88d8762bf324cd0f, // x 2^167 ~= 10^50
]

// ## powersOf10_Exact128

// All the powers of 10 that can be represented exactly
// in 128 bits, represented as binary floating-point values
// using the same convention as in the previous table, only
// with 128 bit significands.

// This table is used in four places:
// * The full 128-bit value is used for 10^0 through 10^55 for Float64.
// * The first 28 entries are combined with the next table for
//   all other Float64 values.
// * This is combined with the 256-bit table below for Float80 support.

// Table size: 896 bytes
fileprivate let powersOf10_Exact128: _InlineArray<_, UInt64> = [
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

// ## powersOf10_Binary64

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
fileprivate let powersOf10_Binary64: _InlineArray<_, UInt64> = [
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

// ## powersOf10_Binary128

// Needed by 80-bit formatter.  This is sufficient precision
// to support a future Float128 implementation if anyone's interested.

// Total table size: 512 + 384 = 896 bytes 

// Table size: 512 bytes
fileprivate let powersOf10_Binary128_Medium: _InlineArray<_, UInt64> = [
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
]

// Table size: 12 * 32 = 384 bytes
fileprivate let powersOf10_Binary128_Coarse: _InlineArray<_, UInt64> = [
  0x9f855c9864e639d5, 0x257261546827afab, 0xeddb8bd5b1dd7ed8, 0x9f2f658cea3b24bc, // x 2^-17858 ~= 10^-5376
  0xdadd9645f360cb51, 0xf290163350ecb3eb, 0xa8edffdccfe4db4b, 0xd9167ab0c1965798, // x 2^-14882 ~= 10^-4480
  0x982b64e953ac4e27, 0x45efb05f20cf48b3, 0x4b4de34e0ebc3e06, 0x9406af8f83fd6265, // x 2^-11905 ~= 10^-3584
  0x42032f9f971bfc07, 0x9fb576046ab35018, 0x474b3cb1fe1d6a7f, 0xc9dea80d6283a34c, // x 2^-8929 ~= 10^-2688
  0xb62593291c768919, 0xc098e6ed0bfbd6f6, 0x6c83ad1260ff20f4, 0x89a63ba4c497b50e, // x 2^-5952 ~= 10^-1792
  0x276e3f0f67f0553b, 0x00de73d9d5be6974, 0x6d4aa5b50bb5dc0d, 0xbbb7ef38bb827f2d, // x 2^-2976 ~= 10^-896
  0x0000000000000000, 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, // x 2^1 == 10^0 exactly
  0xf48b51375df06e86, 0x412fe9e72afd355e, 0x870a8d87239d8f35, 0xae8f2b2ce3d5dbe9, // x 2^2977 ~= 10^896
  0xbf34ff7963028cd9, 0xc20578fa3851488b, 0x2d4070f33b21ab7b, 0xee0ddd84924ab88c, // x 2^5953 ~= 10^1792
  0x8cd036553f38a1e8, 0x5e997e9f45d7897d, 0xf09e780bcc8238d9, 0xa2528e74eaf101fc, // x 2^8930 ~= 10^2688
  0x134ca67a679b84ae, 0x8909e424a112a3cd, 0x95aa118ec1d08317, 0xdd5dc8a2bf27f3f7, // x 2^11906 ~= 10^3584
  0x0f7740145246fb8f, 0x186ef2c39acb4103, 0x888c9ab2fc5b3437, 0x96f18b1742aad751, // x 2^14883 ~= 10^4480
]
