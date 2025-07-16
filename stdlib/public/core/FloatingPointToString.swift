//===--- FloatingPointToString.swift -------------------------*- Swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018-2020 Apple Inc. and the Swift project authors
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
/// the correctness of such algorithms.
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
/// * Always Accurate. Converting the decimal form back to binary
///   will always yield exactly the same value. For the IEEE 754
///   formats, the round-trip will produce exactly the same bit
///   pattern in memory.
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
///   Never use exponential form for integral numbers in this range.
/// * Generally follow existing practice: Don't use use exponential
///   form for fractional values bigger than 10^-4; always write at
///   least 2 digits for an exponent.
/// * Apart from the above, we do prefer shorter output.

///
/// This Swift implementation was ported from an earlier C version;
/// the output is exactly the same in all cases.
/// A few notes on the Swift transcription:
/// * We use MutableSpan<UTF8.CodeUnit> and MutableRawSpan to
///   identify blocks of working memory.
/// * We use unsafe/unchecked operations extensively, supported
///   by several years of analysis and testing to ensure that
///   no unsafety actually occurs.  For Float32, that testing
///   was exhaustive -- we verified all 4 billion possible Float32 values.
/// * The Swift code uses an idiom of building up to 8 ASCII characters
///   in a UInt64 and then writing the whole block to memory.
/// * The Swift version is slightly faster than the C version;
///   mostly thanks to various minor algorithmic tweaks that were
///   found during the translation process.
///
// ----------------------------------------------------------------------------

// Float16 is not currently supported on Intel macOS.
// (This will change once there's a fully-stable Float16
// ABI on that platform.)
#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
// Implement the legacy ABI on top of the new one
@_silgen_name("swift_float16ToString2")
internal func _float16ToStringImpl2(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float16,
  _ debug: Bool) -> UInt64 {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(_unchecked: textBuffer,
                                                   count: Int(bufferLength))
    let textRange = Float16ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
        unsafe _memmove(dest: textBuffer,
                        src: textBuffer + textRange.lowerBound,
                        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
}
#endif

@_silgen_name("swift_float32ToString2")
internal func _float32ToStringImpl2(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float32,
  _ debug: Bool) -> UInt64 {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(_unchecked: textBuffer,
                                                   count: Int(bufferLength))
    let textRange = Float32ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
        unsafe _memmove(dest: textBuffer,
                        src: textBuffer + textRange.lowerBound,
                        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
}

@_silgen_name("swift_float64ToString2")
internal func _float64ToStringImpl2(
  _ textBuffer: UnsafeMutablePointer<UTF8.CodeUnit>,
  _ bufferLength: UInt,
  _ value: Float64,
  _ debug: Bool) -> UInt64 {
    // Code below works with raw memory.
    var buffer = unsafe MutableSpan<UTF8.CodeUnit>(_unchecked: textBuffer,
                                                   count: Int(bufferLength))
    let textRange = Float64ToASCII(value: value, buffer: &buffer)
    let textLength = textRange.upperBound - textRange.lowerBound

    // Move the text to the start of the buffer
    if textRange.lowerBound != 0 {
        unsafe _memmove(dest: textBuffer,
                        src: textBuffer + textRange.lowerBound,
                        size: UInt(truncatingIfNeeded: textLength))
    }
    return UInt64(truncatingIfNeeded: textLength)
}

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))
internal func Float16ToASCII(
  value f: Float16,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    if #available(macOS 9999, *) {
        return _Float16ToASCII(value: f, buffer: &utf8Buffer)
    } else {
        return 0..<0
    }
}

@available(macOS 9999, *)
fileprivate func _Float16ToASCII(
  value f: Float16,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    // We need a MutableRawSpan in order to use wide store/load operations
    precondition(utf8Buffer.count >= 32)
    var buffer = utf8Buffer.mutableBytes

    // Step 1: Handle various input cases:
    let binaryExponent: Int
    let significand: Float16.RawSignificand
    let exponentBias = (1 << (Float16.exponentBitCount - 1)) - 2; // 14
    if (f.exponentBitPattern == 0x1f) { // NaN or Infinity
        if (f.isInfinite) {
            return infinity(buffer: &buffer, sign: f.sign)
        } else { // f.isNaN
            let quietBit = (f.significandBitPattern >> (Float16.significandBitCount - 1)) & 1;
            let payloadMask = UInt16(1 &<< (Float16.significandBitCount - 2)) - 1
            let payload16 = f.significandBitPattern & payloadMask
            return nan_details(buffer: &buffer,
                               sign: f.sign,
                               quiet: quietBit == 0,
                               payloadHigh: 0,
                               payloadLow: UInt64(truncatingIfNeeded:payload16))
        }
    } else if (f.exponentBitPattern == 0) {
        if (f.isZero) {
            return zero(buffer: &buffer, sign: f.sign)
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
    let upperMidpointExact = significand &+ halfUlp
    let lowerMidpointExact = significand &- ((f.significandBitPattern == 0) ? quarterUlp : halfUlp)

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
        var u = (UInt32(upperMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
        var l = (UInt32(lowerMidpointExact) << (28 - 13 &+ binaryExponent)) &* 100000
        var t = (UInt32(significand) << (28 - 13 &+ binaryExponent)) &* 100000
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
            unsafe buffer.storeBytes(of: 0x30 + UInt8(truncatingIfNeeded: uDigit),
                                     toUncheckedByteOffset: nextDigit,
                                     as: UInt8.self)
            nextDigit &+= 1
            unsafe buffer.storeBytes(of: 0x2e,
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
                unsafe buffer.storeBytes(of: 0x30 &+ UInt8(truncatingIfNeeded: uDigit),
                                         toUncheckedByteOffset: nextDigit,
                                         as: UInt8.self)
                nextDigit &+= 1
            }
        }
        let digit = 0x30 &+ (t &+ (1 &<< 27)) >> 28
        unsafe buffer.storeBytes(of: UInt8(truncatingIfNeeded: digit),
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        unsafe buffer.storeBytes(of: 0x65, // "e"
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        unsafe buffer.storeBytes(of: 0x2d, // "-"
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        unsafe buffer.storeBytes(of: UInt8(truncatingIfNeeded: -decimalExponent / 10 &+ 0x30),
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        // Last write on this branch, so use a safe checked store
        buffer.storeBytes(of: UInt8(truncatingIfNeeded: -decimalExponent % 10 &+ 0x30),
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
        let text = intToEightDigits(UInt32(intPart))
        unsafe buffer.storeBytes(of: text,
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
        unsafe buffer.storeBytes(of: 0x2e,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1

        if fractionPart == 0 {
            // Step 6: No fraction, so ".0" and we're done
            // Last write on this branch, so use a checked store
            buffer.storeBytes(of: 0x30,
                              toByteOffset: nextDigit,
                              as: UInt8.self)
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
                unsafe buffer.storeBytes(of: 0x30 &+ uDigit,
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
                if t < lDigit || (t == lDigit && l > 0) {
	            t += 1
                }
            } else {
                t >>= 28
            }
            // Last write on this branch, so use a checked store
            buffer.storeBytes(of: UInt8(truncatingIfNeeded: 0x30 + t),
                              toByteOffset: nextDigit,
                              as: UInt8.self)
            nextDigit &+= 1
        }
    }
    if f.sign == .minus {
        buffer.storeBytes(of: 0x2d,
                          toByteOffset: firstDigit &- 1,
                          as: UInt8.self) // "-"
        firstDigit &-= 1
    }
    return firstDigit..<nextDigit
}
#endif


internal func Float32ToASCII(
  value f: Float32,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    if #available(macOS 9999, *) {
        return _Float32ToASCII(value: f, buffer: &utf8Buffer)
    } else {
        return 0..<0
    }
}

@available(macOS 9999, *)
fileprivate func _Float32ToASCII(
  value f: Float32,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    // Note: The algorithm here is the same as for Float64, only
    // with narrower arithmetic.  Refer to `_Float64ToASCII` for
    // more detailed comments and explanation.

    // We need a MutableRawSpan in order to use wide store/load operations
    precondition(utf8Buffer.count >= 32)
    var buffer = utf8Buffer.mutableBytes

    // Step 1: Handle the special cases, decompose the input

    let binaryExponent: Int
    let significand: Float.RawSignificand
    let exponentBias = (1 << (Float.exponentBitCount - 1)) - 2; // 126
    if (f.exponentBitPattern == 0xff) {
        if (f.isInfinite) {
            return infinity(buffer: &buffer, sign: f.sign)
        } else { // f.isNaN
            let quietBit = (f.significandBitPattern >> (Float.significandBitCount - 1)) & 1
            let payloadMask = UInt32(1 << (Float.significandBitCount - 2)) - 1
            let payload32 = f.significandBitPattern & payloadMask
            return nan_details(buffer: &buffer,
                               sign: f.sign,
                               quiet: quietBit == 0,
                               payloadHigh: 0,
                               payloadLow: UInt64(truncatingIfNeeded:payload32))
        }
    } else if (f.exponentBitPattern == 0) {
        if (f.isZero) {
            return zero(buffer: &buffer, sign: f.sign)
        } else { // f.isSubnormal
            binaryExponent = 1 - exponentBias
            significand = f.significandBitPattern &<< Float.exponentBitCount
        }
    } else {
        binaryExponent = Int(f.exponentBitPattern) &- exponentBias
        significand = (f.significandBitPattern &+ (1 << Float.significandBitCount)) &<< Float.exponentBitCount
    }

    // Step 2: Determine the exact unscaled target interval

    let halfUlp: Float.RawSignificand = 1 << (Float.exponentBitCount - 1)
    let quarterUlp = halfUlp >> 1
    let upperMidpointExact = significand &+ halfUlp
    let lowerMidpointExact = significand &- ((f.significandBitPattern == 0) ? quarterUlp : halfUlp)
    let isOddSignificand = ((f.significandBitPattern & 1) != 0)

    // Step 3: Estimate the base 10 exponent

    var base10Exponent = decimalExponentFor2ToThe(binaryExponent)

    // Step 4: Compute power-of-10 scale factor

    var powerOfTenRoundedDown: UInt64 = 0
    var powerOfTenRoundedUp: UInt64 = 0

    let bulkFirstDigits = 1
    let powerOfTenExponent = intervalContainingPowerOf10_Binary32(
      -base10Exponent &+ bulkFirstDigits &- 1,
      &powerOfTenRoundedDown, &powerOfTenRoundedUp)
    let extraBits = binaryExponent &+ powerOfTenExponent

    // Step 5: Scale the interval (with rounding)

    // Experimentally, 11 is as large as we can go here without introducing errors.
    // We need 7 to generate 2 digits at a time below.
    // 11 should allow us to generate 3 digits at a time, but
    // that doesn't seem to be any faster.
    let integerBits = 11
    let fractionBits = 64 - integerBits
    var u: UInt64
    var l: UInt64
    if isOddSignificand {
        // Narrow the interval (odd significand)
        let u1 = multiply64x32RoundingDown(powerOfTenRoundedDown, upperMidpointExact)
        u = u1 >> (integerBits - extraBits)
        let l1 = multiply64x32RoundingUp(powerOfTenRoundedUp, lowerMidpointExact)
        let bias = UInt64((1 &<< (integerBits &- extraBits)) &- 1)
        l = (l1 &+ bias) >> (integerBits &- extraBits)
    } else {
        // Widen the interval (even significand)
        let u1 = multiply64x32RoundingUp(powerOfTenRoundedUp, upperMidpointExact)
        let bias = UInt64((1 &<< (integerBits &- extraBits)) &- 1)
        u = (u1 &+ bias) >> (integerBits &- extraBits)
        let l1 = multiply64x32RoundingDown(powerOfTenRoundedDown, lowerMidpointExact)
        l = l1 >> (integerBits &- extraBits)
    }

    // Step 6: Align first digit, adjust exponent

    while u < (1 &<< fractionBits) {
        base10Exponent &-= 1
        l &*= 10
        u &*= 10
    }

    // Step 7: Generate decimal digits into the destination buffer

    var t = u
    var delta = u &- l
    let fractionMask: UInt64 = (1 << fractionBits) - 1

    // Write 8 leading zeros to the beginning of the buffer:
    unsafe buffer.storeBytes(of: 0x3030303030303030,
                             toUncheckedByteOffset: 0,
                             as: UInt64.self)

    // Overwrite the first digit at index 7:
    let firstDigit = 7
    let digit = (t >> fractionBits) &+ 0x30
    t &= fractionMask
    unsafe buffer.storeBytes(of: UInt8(truncatingIfNeeded: digit),
                             toUncheckedByteOffset: firstDigit,
                             as: UInt8.self)
    var nextDigit = firstDigit &+ 1

    // Generate 2 digits at a time...
    while (delta &* 10) < ((t &* 10) & fractionMask) {
        delta &*= 100
        t &*= 100
        let d12 = Int(truncatingIfNeeded: t >> fractionBits)
        let text = unsafe asciiDigitTable[unchecked: d12]
        unsafe buffer.storeBytes(of: text,
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
        unsafe buffer.storeBytes(of: text,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        t &= fractionMask
    }

    // Adjust the final digit to be closer to the original value
    let isBoundary = (f.significandBitPattern == 0)
    if delta > t &+ (1 &<< fractionBits) {
        let skew: UInt64
        if isBoundary {
            skew = delta &- delta / 3 &- t
        } else {
            skew = delta / 2 &- t
        }
        let one = UInt64(1) << (64 - integerBits)
        let lastAccurateBit = UInt64(1) << 24
        let fractionMask = (one - 1) & ~(lastAccurateBit - 1);
        let oneHalf = one >> 1
        var lastDigit = unsafe buffer.unsafeLoad(fromUncheckedByteOffset: nextDigit &- 1,
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
        unsafe buffer.storeBytes(of: lastDigit,
                                 toUncheckedByteOffset: nextDigit &- 1,
                                 as: UInt8.self)
    }

    // Step 8: Finish formatting
    let forceExponential = (binaryExponent > 25) || (binaryExponent == 25 && !isBoundary)
    return finishFormatting(&buffer, f.sign, firstDigit, nextDigit,
                            forceExponential, base10Exponent)
}

internal func Float64ToASCII(
  value d: Float64,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    if #available(macOS 9999, *) {
        return _Float64ToASCII(value: d, buffer: &utf8Buffer)
    } else {
        return 0..<0
    }
}

@available(macOS 9999, *)
fileprivate func _Float64ToASCII(
  value d: Float64,
  buffer utf8Buffer: inout MutableSpan<UTF8.CodeUnit>) -> Range<Int>
{
    // We need a MutableRawSpan in order to use wide store/load operations
    precondition(utf8Buffer.count >= 32)
    var buffer = utf8Buffer.mutableBytes

    //
    // Step 1: Handle the special cases, decompose the input
    //
    let binaryExponent: Int
    let significand: Double.RawSignificand
    let exponentBias = (1 << (Double.exponentBitCount - 1)) - 2; // 1022

    if (d.exponentBitPattern == 0x7ff) {
        if (d.isInfinite) {
            return infinity(buffer: &buffer, sign: d.sign)
        } else { // d.isNaN
            let quietBit = (d.significandBitPattern >> (Double.significandBitCount - 1)) & 1
            let payloadMask = UInt64(1 << (Double.significandBitCount - 2)) - 1
            let payload64 = d.significandBitPattern & payloadMask
            return nan_details(buffer: &buffer,
                               sign: d.sign,
                               quiet: quietBit == 0,
                               payloadHigh: 0,
                               payloadLow: UInt64(truncatingIfNeeded:payload64))
        }
    } else if (d.exponentBitPattern == 0) {
        if (d.isZero) {
            return zero(buffer: &buffer, sign: d.sign)
        } else { // d.isSubnormal
            binaryExponent = 1 - exponentBias
            significand = d.significandBitPattern &<< Double.exponentBitCount
        }
    } else {
        binaryExponent = Int(d.exponentBitPattern) &- exponentBias
        significand = (d.significandBitPattern &+ (1 << Double.significandBitCount)) &<< Double.exponentBitCount
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
    let lowerMidpointExact = significand &- ((d.significandBitPattern == 0) ? quarterUlp : halfUlp)
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
    let bulkFirstDigitFactor = 1000000  // 10^(bulkFirstDigits - 1)

    let powerOfTenExponent = intervalContainingPowerOf10_Binary64(
        -base10Exponent &+ bulkFirstDigits &- 1,
        &powerOfTenRoundedDown, &powerOfTenRoundedUp)

    let extraBits = binaryExponent + powerOfTenExponent

    // Step 5: Scale the interval (with rounding)

    // As mentioned above, the final digit generation works
    // with an interval, so we actually apply the scaling
    // to the upper and lower midpoint values separately.

    // As part of the scaling here, we'll switch from a pure
    // fraction with zero bit integer portion and 128-bit fraction
    // to a fixed-point form with 32 bits in the integer portion.

    let integerBits = 32
    let roundingBias = UInt128((1 &<< UInt64(truncatingIfNeeded: integerBits &- extraBits)) &- 1)
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
        let u1 = multiply128x64RoundingDown(powerOfTenRoundedDown, upperMidpointExact)
        // Account for residual binary exponent and adjust
        // to the fixed-point format
        u = u1 >> (integerBits - extraBits)

        // Conversely for the lower midpoint...
        let l1 = multiply128x64RoundingUp(powerOfTenRoundedUp, lowerMidpointExact)
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

        let u1 = multiply128x64RoundingUp(powerOfTenRoundedUp, upperMidpointExact)
        u = (u1 &+ roundingBias) >> (integerBits - extraBits)
        let l1 = multiply128x64RoundingDown(powerOfTenRoundedDown, lowerMidpointExact)
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
    unsafe buffer.storeBytes(of: 0x3030303030303030 as UInt64,
                             toUncheckedByteOffset: 0,
                             as: UInt64.self)

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
        var droppedDigit = UInt32(truncatingIfNeeded: ((tHigh &* 10) >> (64 - integerBits)) % 10)
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
        let text = intToEightDigits(t0) >> ((8 - t0digits) * 8)
        unsafe buffer.storeBytes(of: text,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt64.self)
        nextDigit &+= t0digits
        firstDigit &+= 1
    } else {
        // Our initial scaling did not produce too many digits.  The
        // `d12345678` value holds the first 7 digits (plus a leading
        // zero).  The remainder of this algorithm is basically just a
        // heavily-optimized variation of Grisu2.

        // Write out exactly 8 digits, assuming little-endian.
        let chars = intToEightDigits(d12345678)
        unsafe buffer.storeBytes(of: chars,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt64.self)
        nextDigit &+= 8
        firstDigit &+= 1

        // >90% of random binary64 values need at least 15 digits.
        // We already have seven, try grabbing the next 8 digits all at once.
        let TenToTheEighth = 100000000 as UInt128; // 10^(15-bulkFirstDigits)
        let d0 = delta * TenToTheEighth
        var t0 = t * TenToTheEighth
        let next8Digits = UInt32(truncatingIfNeeded: t0._high >> 32)
        t0 &= fractionMask
        if d0 < t0 {
            // We got 8 more digits! (So number is at least 15 digits)
            // Write them out:
            let chars = intToEightDigits(next8Digits)
            unsafe buffer.storeBytes(of: chars,
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
            unsafe buffer.storeBytes(of: UInt8(truncatingIfNeeded: t._high >> 32) &+ 0x30,
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
        let deltaHigh64 = UInt64(truncatingIfNeeded: delta >> (64 - integerBits + adjustIntegerBits))
        let tHigh64 = UInt64(truncatingIfNeeded: t >> (64 - integerBits + adjustIntegerBits))

        let one = UInt64(1) << (64 - adjustIntegerBits)
        let adjustFractionMask = one - 1;
        let oneHalf = one >> 1;
        if deltaHigh64 >= tHigh64 &+ one {
            // The `skew` is the difference between our
            // computed digits and the original exact value.
            var skew: UInt64
            if (d.significandBitPattern == 0) {
                skew = deltaHigh64 &- deltaHigh64 / 3 &- tHigh64
            } else {
                skew = deltaHigh64 / 2 &- tHigh64
            }

            // We use the `skew` to figure out whether there's
            // a better base-10 value than our current one.
            if (skew & adjustFractionMask) == oneHalf {
                // Difference is an integer + exactly 1/2, so ...
                let adjust = skew >> (64 - adjustIntegerBits)
                var t = unsafe buffer.unsafeLoad(fromUncheckedByteOffset: nextDigit - 1,
                                                 as: UInt8.self)
                t &-= UInt8(truncatingIfNeeded: adjust)
                // ... we round the last digit even.
                t &= ~1
                unsafe buffer.storeBytes(of: t,
                                         toUncheckedByteOffset: nextDigit - 1,
                                         as: UInt8.self)
            } else {
                let adjust = (skew + oneHalf) >> (64 - adjustIntegerBits)
                var t = unsafe buffer.unsafeLoad(fromUncheckedByteOffset: nextDigit - 1,
                                                 as: UInt8.self)
                t &-= UInt8(truncatingIfNeeded: adjust)
                unsafe buffer.storeBytes(of: t,
                                         toUncheckedByteOffset: nextDigit - 1,
                                         as: UInt8.self)
            }
        }
    }

    // Step 8: Finalize formatting by rearranging
    // the digits and filling in decimal points,
    // exponents, and zero padding.
    let isBoundary = (d.significandBitPattern == 0)
    let forceExponential = (binaryExponent > 54) || (binaryExponent == 54 && !isBoundary)
    return finishFormatting(&buffer, d.sign, firstDigit, nextDigit,
                            forceExponential, base10Exponent)
}

@available(macOS 9999, *)
// TODO: This doesn't guarantee inlining in all cases :(
@inline(__always)
fileprivate func finishFormatting(_ buffer: inout MutableRawSpan,
                                  _ sign: FloatingPointSign,
                                  _ firstDigit: Int,
                                  _ nextDigit: Int,
                                  _ forceExponential: Bool,
                                  _ base10Exponent: Int) -> Range<Int>
{
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
            let t = unsafe buffer.unsafeLoad(fromUncheckedByteOffset: firstDigit,
                                             as: UInt8.self)
            unsafe buffer.storeBytes(of: 0x2e,
                                     toUncheckedByteOffset: firstDigit,
                                     as: UInt8.self)
            firstDigit &-= 1
            unsafe buffer.storeBytes(of: t,
                                     toUncheckedByteOffset: firstDigit,
                                     as: UInt8.self)
        }
        // Append the exponent:
        unsafe buffer.storeBytes(of: 0x65,
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
        unsafe buffer.storeBytes(of: expSign,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt8.self)
        nextDigit &+= 1
        if e > 99 {
            if e > 999 {
                let d = asciiDigitTable[e / 100]
                unsafe buffer.storeBytes(of: d,
                                         toUncheckedByteOffset: nextDigit,
                                         as: UInt16.self)
                nextDigit &+= 2
            } else {
                let d = 0x30 &+ UInt8(truncatingIfNeeded: (e / 100))
                unsafe buffer.storeBytes(of: d,
                                         toUncheckedByteOffset: nextDigit,
                                         as: UInt8.self)
                nextDigit &+= 1
            }
            e = e % 100
        }
        let d = unsafe asciiDigitTable[unchecked: e]
        unsafe buffer.storeBytes(of: d,
                                 toUncheckedByteOffset: nextDigit,
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
        buffer.storeBytes(of: 0x2e, // "."
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
            let t = unsafe buffer.unsafeLoad(fromUncheckedByteOffset: firstDigit &+ i &+ 1,
                                             as: UInt8.self)
            unsafe buffer.storeBytes(of: t,
                                     toUncheckedByteOffset: firstDigit &+ i,
                                     as: UInt8.self)
        }
        unsafe buffer.storeBytes(of: 0x2e,
                                 toUncheckedByteOffset: firstDigit &+ base10Exponent &+ 1,
                                 as: UInt8.self)
    } else {
        // "12345678900.0"
        // Fill trailing zeros, put ".0" at the end
        // so the result is obviously floating-point.
        let zeroEnd = firstDigit &+ base10Exponent &+ 3
        // TODO: Find out how to use C memset() here:
        // Blast 8 "0" digits into the buffer
        unsafe buffer.storeBytes(of: 0x3030303030303030 as UInt64,
                                 toUncheckedByteOffset: nextDigit,
                                 as: UInt64.self)
        // Add more "0" digits if needed...
        // (Note: Can't use a standard range loop because nextDigit+8
        // can legitimately be larger than zeroEnd here.)
        var i = nextDigit + 8
        while i < zeroEnd {
            unsafe buffer.storeBytes(of: 0x30,
                                     toUncheckedByteOffset: i,
                                     as: UInt8.self)
            i &+= 1
        }
        nextDigit = zeroEnd
        unsafe buffer.storeBytes(of: 0x2e,
                                 toUncheckedByteOffset: nextDigit &- 2,
                                 as: UInt8.self)
    }
    if sign == .minus {
        unsafe buffer.storeBytes(of: 0x2d,
                                 toUncheckedByteOffset: firstDigit &- 1,
                                 as: UInt8.self) // "-"
        firstDigit &-= 1
    }

    return unsafe Range(_uncheckedBounds: (lower: firstDigit, upper: nextDigit))
}

// Table with ASCII strings for all 2-digit decimal numbers.
// Stored as little-endian UInt16s for efficiency
@available(macOS 9999, *)
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
fileprivate func infinity(buffer: inout MutableRawSpan, sign: FloatingPointSign) -> Range<Int> {
    if sign == .minus {
        buffer.storeBytes(of: 0x666e692d, toByteOffset: 0, as: UInt32.self) // "-inf"
        return 0..<4
    } else {
        buffer.storeBytes(of: 0x00666e69, toByteOffset: 0, as: UInt32.self) // "inf\0"
        return 0..<3
    }
}

fileprivate func zero(buffer: inout MutableRawSpan, sign: FloatingPointSign) -> Range<Int> {
    if sign == .minus {
        buffer.storeBytes(of: 0x302e302d, toByteOffset: 0, as: UInt32.self) // "-0.0"
        return 0..<4
    } else {
        buffer.storeBytes(of: 0x00302e30, toByteOffset: 0, as: UInt32.self) // "0.0\0"
        return 0..<3
    }
}

@available(macOS 9999, *)
fileprivate let hexdigits: InlineArray<16, UInt8> = [ 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66 ]

@available(macOS 9999, *)
fileprivate func hexWithoutLeadingZeros(buffer: inout MutableRawSpan, offset: inout Int, value: UInt64) {
    var shift = 60
    while (shift > 0) && ((value >> shift) & 0xf == 0) {
        shift -= 4
    }
    while shift >= 0 {
        let d = hexdigits[Int(truncatingIfNeeded: (value >> shift) & 0xf)]
        shift -= 4
        buffer.storeBytes(of: d, toByteOffset: offset, as: UInt8.self)
        offset += 1
    }
}

@available(macOS 9999, *)
fileprivate func hexWithLeadingZeros(buffer: inout MutableRawSpan, offset: inout Int, value: UInt64) {
    var shift = 60
    while shift >= 0 {
        let d = hexdigits[Int(truncatingIfNeeded: (value >> shift) & 0xf)]
        shift -= 4
        buffer.storeBytes(of: d, toByteOffset: offset, as: UInt8.self)
        offset += 1
    }
}

@available(macOS 9999, *)
fileprivate func nan_details(buffer: inout MutableRawSpan,
                             sign: FloatingPointSign,
                             quiet: Bool,
                             payloadHigh: UInt64,
                             payloadLow: UInt64) -> Range<Int>
{
    // value is a NaN of some sort
    var i = 0
    if sign == .minus {
        buffer.storeBytes(of: 0x2d, toByteOffset: 0, as: UInt8.self) // "-"
        i = 1
    }
    if quiet {
        buffer.storeBytes(of: 0x73, toByteOffset: i, as: UInt8.self) // "s"
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
            hexWithoutLeadingZeros(buffer: &buffer, offset: &i, value: payloadLow)
        } else {
            hexWithoutLeadingZeros(buffer: &buffer, offset: &i, value: payloadHigh)
            hexWithLeadingZeros(buffer: &buffer, offset: &i, value: payloadLow)
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
@inline(__always)
fileprivate func intToEightDigits(_ n: UInt32) -> UInt64 {
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

@inline(__always)
fileprivate func multiply64x32RoundingDown(_ lhs: UInt64, _ rhs: UInt32) -> UInt64 {
    let mask32 = UInt64(UInt32.max)
    let t = ((lhs & mask32) * UInt64(rhs)) >> 32
    return t + (lhs >> 32) * UInt64(rhs)
}

@inline(__always)
fileprivate func multiply64x32RoundingUp(_ lhs: UInt64, _ rhs: UInt32) -> UInt64 {
    let mask32 = UInt64(UInt32.max)
    let t = (((lhs & mask32) * UInt64(rhs)) + mask32) >> 32
    return t + (lhs >> 32) * UInt64(rhs)
}

// Arithmetic on fractions:
// E.g., `128x64` multiplies a 0.128 fixed-point
// value by a 0.64 fixed-point fraction, returning
// a 0.128 value that's been rounded down from the
// exact 192-bit result.
@available(SwiftStdlib 6.0, *)
@inline(__always)
fileprivate func multiply128x64RoundingDown(_ lhs: UInt128, _ rhs: UInt64) -> UInt128 {
    let lhsHigh = UInt128(truncatingIfNeeded: lhs._high)
    let lhsLow = UInt128(truncatingIfNeeded: lhs._low)
    let rhs128 = UInt128(truncatingIfNeeded: rhs)
    return (lhsHigh &* rhs128) &+ ((lhsLow &* rhs128) >> 64)
}

@available(SwiftStdlib 6.0, *)
@inline(__always)
fileprivate func multiply128x64RoundingUp(_ lhs: UInt128, _ rhs: UInt64) -> UInt128 {
    let lhsHigh = UInt128(truncatingIfNeeded: lhs._high)
    let lhsLow = UInt128(truncatingIfNeeded: lhs._low)
    let rhs128 = UInt128(truncatingIfNeeded: rhs)
    let h = lhsHigh &* rhs128
    let l = lhsLow &* rhs128
    let bias = (UInt128(1) << 64) &- 1
    return h + ((l &+ bias) &>> 64)
}

@available(macOS 9999, *)
@inline(__always)
fileprivate func intervalContainingPowerOf10_Binary32(_ p: Int, _ lower: inout UInt64, _ upper: inout UInt64) -> Int {
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

@available(macOS 9999, *)
@inline(__always)
fileprivate func intervalContainingPowerOf10_Binary64(_ p: Int, _ lower: inout UInt128, _ upper: inout UInt128) -> Int {
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
        lower = ((UInt128(truncatingIfNeeded:baseHigh) &* UInt128(truncatingIfNeeded:extra))
                   &+ ((UInt128(truncatingIfNeeded:baseLow) &* UInt128(truncatingIfNeeded:extra)) &>> 64))
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

@available(macOS 9999, *)
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

@available(macOS 9999, *)
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

@available(macOS 9999, *)
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
