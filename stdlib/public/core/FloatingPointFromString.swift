
//===--- FloatingPointFromString.swift -----------------------*- Swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
// Converts ASCII text sequences to binary floating-point.  This is the internal
// implementation backing APIs such as `Double(_:String)`.  The public APIs
// themselves (and the public-facing documentation for said APIs) are defined in
// `FloatingPointParsing.swift.gyb`.
//
// This is derived from the strtofp.c implementation from Apple's libc,
// with a number of changes to support Swift semantics:
// However, unlike that implementation:
// * It accepts a `Span` covering the bytes of the
//   input string, instead of a pointer to a null-terminated
//   input text.
// * This implementation does not obey the system
//   locale -- the decimal point character is
//   assumed to always be "." (period).
// * This implementation does not obey the current
//   rounding mode -- it always uses IEEE 754 default
//   rounding (to-nearest with ties rounded even).
// * This implementation has been restructured
//   to make use of safe Swift programming constructs.
//
// Implementation notes below are copied from the original:

// IMPLEMENTATION NOTES
// --------------------------------
//
// This is a new implementation that uses ideas from a number of
// sources, including Clinger's 1990 paper, Gay's gdtoa library,
// Lemire's fast_double_parser implementation, Google's abseil
// library, as well as work I've done for the Swift standard library.
//
// All of the parsers use the same initial parsing logic and fall back
// to the same arbitrary-precision integer code.  In between these,
// they use varying format-specific optimizations.
//
// First Step: Initial Parsing
//
// The initial parsing of the textual input is handled by
// `fastParse64`.  Following Lemire, this uses a fixed-size 64-bit
// accumulator for speed and is heavily optimized assuming that the
// input significand has at most 19 digits. Longer input will overflow
// the accumulator, triggering an additional scan of the input.  This
// initial parse also detects Hex floats, nans, and "inf"/"infinity"
// strings and dispatches those to specialized implementations.
//
// With the initial parse complete, the challenge is to compute
//    decimalSignificand * 10^decimalExponent
// with precisely correct rounding as quickly as possible.
//
// Last Step: arbitrary-precision integer calculation (slowDecimalToBinary)
//
// Specific formatters use a variety of optimized paths that provide
// quick results for specific classes of input.  But none of those
// work for every input value.  So we have a final fallback that uses
// arbitrary-precision integer arithmetic to compute the exact results
// with guaranteed accuracy for all inputs.  Of course, the required
// arbitrary-precision arithmetic can be significantly more expensive,
// especially when the significand is very long or the exponent is
// very large.
//
// There are two interesting optimizations used in this fallback path:
//
// Powers of 5: We break the power of 10 computation into a power of 5
// and a power of 2.  The latter can be simply folded into the final
// FP exponent, so this effectively reduces the power of 10
// computation to the computation of a power of 5, which is a
// significantly smaller number.  For very large exponents, the power
// of 5 computation can be the majority of the overall run time.
// (Note that the Swift version of `fiveToTheN()` incorporates a
// significant performance improvement compared to the original
// strtofp.c)
//
// Limit on significand digits: I first saw this optimization in the
// Abseil library.  First, consider the exact decimal expansions for
// all the exact midpoints between adjacent pairs of floating-point
// values.  There is some maximum number of significant digits
// `maxDecimalMidpointDigits`.  Following an argument from Clinger, we
// only need to be able to distinguish whether we are above or below
// any such midpoint.  So it suffices to consider the first
// `maxDecimalMidpointDigits`, appending a single digit that is
// non-zero if the trailing digits are non-zero. This allows us to
// limit the total size of the arithmetic used in this stage.  In
// particular, for double, this limits us to less than 1024 bytes of
// total space, which can easily fit on the stack, allowing us to
// parse any double input regardless of length without allocation.
//
// For binary128, the comparable limit is 11564 digits, which gives a
// maximum work buffer size of nearly 10k.  This seems a bit large for
// the stack, but a buffer of 1536 bytes is big enough to process any
// binary128 with less than 100 digits, regardless of exponent.
//
// Note: Compared to Clinger's AlgorithmR, this requires fewer
// arbitrary-precision operations and gives the correct answer
// directly without requiring a nearly-correct initial value.
// Compared to Clinger's AlgorithmM, this takes advantage of the fact
// that our integer arithmetic is occuring in the same base as used by
// the final FP format.  This means we can interpret the bits from a
// simple calculation instead of doing additional work to abstractly
// compute the target format.
//
// These first and last steps (`fastParse64` and
// `slowDecimalToBinary`) are sufficient to provide guaranteed correct
// results for any input string being parsed to any output format.
// The optimizations described next are accelerators that allow us to
// provide a result more quickly for common cases where the additional
// code complexity and testing cost can be justified.
//
// Optimization: Use a single Floating-point calculation
//
// Clinger(1990) observed that when converting to double, if the
// significand and 10^k are both exactly representable by doubles,
// then
//    (double)significand * (double)10^k
// is always correct with a single double multiplication.
// Similarly, if 10^-k is exactly representable, then
//    (double)significand / (double)10^(-k)
// is always correct with a single double division.
//
// In particular, any significand of 15 digits or less can be exactly
// represented by a double, as can any power of 10 up to 10^22.
//
// There are a few similar cases where we can provide exact inputs to
// a single floating-point operation.  Since a single FP operation
// always produces correctly-rounded results (in the current rounding
// mode), these always produce correct results for the corresponding
// range of inputs.  Since this relies on the hardware FPU, it is very
// fast when it can be used.
//
// This optimization works especially well for hand-entered input,
// which typically has few digits and a small exponent.  It works less
// well for JSON, as random double values in JSON are typically
// presented with 16 or 17 digits.  Fast FMA or mixed-precision
// arithmetic can extend this technique further in certain
// environments.
//
// TODO: Experiment with software FP calculations to see if
// we can expand this even further.  In particular, computations
// with a 64-bit significand would allow us to cover the common
// JSON cases.
//
// Optimization: Interval calculation
//
// We can easily compute fixed-precision upper and lower bounds for
// the power-of-10 value from a lookup table.  Likewise, we can
// construct bounds for an arbitrary-length significand by inspecting
// just the first digits.  From these bounds, we can compute upper and
// lower bounds for the final result, all with fast fixed-precision
// integer arithmetic.  Depending on the precision, these upper and
// lower bounds can coincide for more than 99% of all inputs,
// guaranteeing the correct result in those cases.  This also allows
// us to use fast fixed-precision arithmetic for very long inputs,
// only using the first digits of the significand in cases where the
// additional digits do not affect the result.

// Not supported on 16-bit platforms at all right now.
#if !_pointerBitWidth(_16)

// Table for fast parsing of octal/decimal/hex strings
fileprivate let _hexdigit : _InlineArray<256, UInt8> = [
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
     0,    1,    2,    3,     4,    5,    6,    7,
     8,    9, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff,   10,   11,   12,    13,   14,   15, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff,   10,   11,   12,    13,   14,   15, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff,
  0xff, 0xff, 0xff, 0xff,  0xff, 0xff, 0xff, 0xff
]

// Look up function for the table above
fileprivate func hexdigit(_ char: UInt8) -> UInt8 {
    _hexdigit[Int(char)]
}

// These are the 64-bit binary significands of the
// largest binary floating-point value that is not greater
// than the corresponding power of 10.

// That is, when these are not exact, they are less than
// the exact decimal value.  This allows us to use these
// to construct tight intervals around the true power
// of ten value.
fileprivate let powersOf10_Float: _InlineArray<_, UInt64> = [
  0xb0af48ec79ace837, // x 2^-232 ~= 10^-70
  0xdcdb1b2798182244, // x 2^-229 ~= 10^-69
  0x8a08f0f8bf0f156b, // x 2^-225 ~= 10^-68
  0xac8b2d36eed2dac5, // x 2^-222 ~= 10^-67
  0xd7adf884aa879177, // x 2^-219 ~= 10^-66
  0x86ccbb52ea94baea, // x 2^-215 ~= 10^-65
  0xa87fea27a539e9a5, // x 2^-212 ~= 10^-64
  0xd29fe4b18e88640e, // x 2^-209 ~= 10^-63
  0x83a3eeeef9153e89, // x 2^-205 ~= 10^-62
  0xa48ceaaab75a8e2b, // x 2^-202 ~= 10^-61
  0xcdb02555653131b6, // x 2^-199 ~= 10^-60
  0x808e17555f3ebf11, // x 2^-195 ~= 10^-59
  0xa0b19d2ab70e6ed6, // x 2^-192 ~= 10^-58
  0xc8de047564d20a8b, // x 2^-189 ~= 10^-57
  0xfb158592be068d2e, // x 2^-186 ~= 10^-56
  0x9ced737bb6c4183d, // x 2^-182 ~= 10^-55
  0xc428d05aa4751e4c, // x 2^-179 ~= 10^-54
  0xf53304714d9265df, // x 2^-176 ~= 10^-53
  0x993fe2c6d07b7fab, // x 2^-172 ~= 10^-52
  0xbf8fdb78849a5f96, // x 2^-169 ~= 10^-51
  0xef73d256a5c0f77c, // x 2^-166 ~= 10^-50
  0x95a8637627989aad, // x 2^-162 ~= 10^-49
  0xbb127c53b17ec159, // x 2^-159 ~= 10^-48
  0xe9d71b689dde71af, // x 2^-156 ~= 10^-47
  0x9226712162ab070d, // x 2^-152 ~= 10^-46
  0xb6b00d69bb55c8d1, // x 2^-149 ~= 10^-45
  0xe45c10c42a2b3b05, // x 2^-146 ~= 10^-44
  0x8eb98a7a9a5b04e3, // x 2^-142 ~= 10^-43
  0xb267ed1940f1c61c, // x 2^-139 ~= 10^-42
  0xdf01e85f912e37a3, // x 2^-136 ~= 10^-41
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

  // These values are exact; we use them together with
  // _CoarseBinary64 below for binary64 format.
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
]

// Rounded-down values supporting the full range of binary64.
// As above, when not exact, these are rounded down to the
// nearest value lower than or equal to the exact power of 10.
//
// Table size: 232 bytes
//
// We only store every 28th power of ten here.
// We can multiply by an exact 64-bit power of
// ten from powersOf10_Exact64 above to reconstruct the
// significand for any power of 10.
let powersOf10_CoarseBinary64: _InlineArray<30, UInt64> = [
  0xdd5a2c3eab3097cb, // x 2^-1395 ~= 10^-420
  0xdf82365c497b5453, // x 2^-1302 ~= 10^-392
  0xe1afa13afbd14d6d, // x 2^-1209 ~= 10^-364
  0xe3e27a444d8d98b7, // x 2^-1116 ~= 10^-336
  0xe61acf033d1a45df, // x 2^-1023 ~= 10^-308
  0xe858ad248f5c22c9, // x 2^-930 ~= 10^-280
  0xea9c227723ee8bcb, // x 2^-837 ~= 10^-252
  0xece53cec4a314ebd, // x 2^-744 ~= 10^-224
  0xef340a98172aace4, // x 2^-651 ~= 10^-196
  0xf18899b1bc3f8ca1, // x 2^-558 ~= 10^-168
  0xf3e2f893dec3f126, // x 2^-465 ~= 10^-140
  0xf64335bcf065d37d, // x 2^-372 ~= 10^-112
  0xf8a95fcf88747d94, // x 2^-279 ~= 10^-84
  0xfb158592be068d2e, // x 2^-186 ~= 10^-56
  0xfd87b5f28300ca0d, // x 2^-93 ~= 10^-28
  0x8000000000000000, // x 2^1 == 10^0 exactly
  0x813f3978f8940984, // x 2^94 == 10^28 exactly
  0x82818f1281ed449f, // x 2^187 ~= 10^56
  0x83c7088e1aab65db, // x 2^280 ~= 10^84
  0x850fadc09923329e, // x 2^373 ~= 10^112
  0x865b86925b9bc5c2, // x 2^466 ~= 10^140
  0x87aa9aff79042286, // x 2^559 ~= 10^168
  0x88fcf317f22241e2, // x 2^652 ~= 10^196
  0x8a5296ffe33cc92f, // x 2^745 ~= 10^224
  0x8bab8eefb6409c1a, // x 2^838 ~= 10^252
  0x8d07e33455637eb2, // x 2^931 ~= 10^280
  0x8e679c2f5e44ff8f, // x 2^1024 ~= 10^308
  0x8fcac257558ee4e6, // x 2^1117 ~= 10^336
  0x91315e37db165aa9, // x 2^1210 ~= 10^364
  0x929b7871de7f22b9, // x 2^1303 ~= 10^392
]

// To avoid storing the power-of-two exponents, this calculation has
// been exhaustively tested to verify that it provides exactly the
// values in comments above.
@inline(__always)
fileprivate func binaryExponentFor10ToThe(_ p: Int) -> Int {
  return Int(((Int64(p) &* 55732705) >> 24) &+ 1)
}

// We do a lot of calculations with fixed-point 0.64 values.  This
// utility encapsulates the standard idiom needed to multiply such
// values with a truncated (rounded down) result.
fileprivate func multiply64x64RoundingDown(_ lhs: UInt64, _ rhs: UInt64) -> UInt64 {
  UInt64(truncatingIfNeeded: (_UInt128(lhs) * _UInt128(rhs)) >> 64)
}

// Description of target FP format
fileprivate struct TargetFormat {
  // Number of bits in the significand.  Note that IEEE754 formats
  // store one bit less than this number.  So this is 53 for Double, not 52.
  let significandBits: Int
  // Range of binary exponents, using IEEE754 conventions
  let minBinaryExponent: Int
  let maxBinaryExponent: Int
  // Bounds for the possible decimal exponents.  These are used to
  // quickly exclude extreme values that cannot possibly convert to
  // a finite form.
  let minDecimalExponent: Int
  let maxDecimalExponent: Int
  // This is a key parameter for optimizing storage, as explained in
  // the section "Limit on significand digits" at the top of this
  // file.  For IEEE754 formats, this is the number of significant
  // digits in the exact decimal representation of the value halfway
  // between the largest subnormal and the smallest normal value.
  let maxDecimalMidpointDigits: Int
}

// Result of a parse operation
fileprivate enum ParseResult {
  case decimal(digits: UInt64,
               base10Exponent: Int16,
               unparsedDigitCount: UInt16,
               firstUnparsedDigitOffset: UInt16,
               leadingDigitCount: UInt8,
               sign: FloatingPointSign)
  case binary(significand: UInt64, exponent: Int16, sign: FloatingPointSign)
  case zero(sign: FloatingPointSign)
  case infinity(sign: FloatingPointSign)
  case nan(payload: UInt64, signaling: Bool, sign: FloatingPointSign)
  case failure
}

// ================================================================
// ================================================================
//
// Hex float parsing
//
// ================================================================
// ================================================================

// The major intricacy here involves correct handling of overlong hexfloats,
// which in turn requires correct rounding of any excess digits.  We
// can do this using only fixed precision by accumulating a large
// fixed number of bits (at least one more than the significand of the
// largest format we support) and scanning any remaining digits to see
// if the trailing portion is non-zero.

fileprivate func hexFloat(
  targetFormat: TargetFormat,
  input: Span<UInt8>,
  sign: FloatingPointSign,
  start: Int
) -> ParseResult {
  var i = start + 2 // Skip leading '0x'
  let firstDigitOffset = i
  let limit = UInt64(1) << 60
  var significand: UInt64 = 0

  //
  // Digits before the binary point
  //

  // Accumulate the most significant 64 bits...
  while i < input.count && hexdigit(input[i]) < 16 && significand < limit {
    significand &<<= 4
    significand += UInt64(hexdigit(input[i]))
    i += 1
  }

  // Initialize binary exponent to the number of bits we collected above
  // minus 1 (to match the IEEE 754 convention that keeps exactly one
  // bit to the immediate left of the binary point).
  var binaryExponent = 64 - 1 - significand.leadingZeroBitCount
  var remainderNonZero = false

  // Including <4 bits from the next digit
  if i < input.count && hexdigit(input[i]) < 16 && significand.leadingZeroBitCount > 0 {
    // Number of bits we can fill
    let bits = significand.leadingZeroBitCount
    significand &<<= bits
    // Fill that many bits from the next digit
    let digit = hexdigit(input[i])
    let upperPartialDigit = digit >> (4 - bits)
    significand += UInt64(upperPartialDigit)
    // Did we drop any non-zero bits?
    remainderNonZero = remainderNonZero || ((digit - (upperPartialDigit << (4 - bits))) != 0)

    // Track the binary exponent for all bits present in the text,
    // even less-significant bits that we don't actually store.
    binaryExponent += 4
    i += 1
  }

  // Verify the remaining digits before the binary point
  // and adjust the exponent.
  while i < input.count && hexdigit(input[i]) < 16 {
    let digit = hexdigit(input[i])
    remainderNonZero = remainderNonZero || (digit != 0)
    binaryExponent += 4
    i += 1
  }

  // If there's a binary point, we need to do similar
  // for the digits beyond that...
  if i < input.count && input[i] == 0x2e { // '.'
    i += 1
    //
    // Digits after the binary point
    //
    if significand == 0 {
      // Skip leading zeros after the binary point
      // in "0000.00000000001234"
      while i < input.count && input[i] == 0x30 {
        binaryExponent -= 4
        i += 1
      }
      // Handle leading zero bits from the first non-zero digit
      if i < input.count && hexdigit(input[i]) < 16 {
        let digit = hexdigit(input[i])
        let upperZeros = digit.leadingZeroBitCount - 4
        binaryExponent -= upperZeros
        significand = UInt64(digit)
        i += 1
      }
    }
    // Pack more bits into the accumulator (up to 60)
    while i < input.count && hexdigit(input[i]) < 16 && significand < limit {
      significand &<<= 4
      significand += UInt64(hexdigit(input[i]))
      i += 1
    }
    // Part of the digit that would overflow our 64-bit accumulator
    if i < input.count && hexdigit(input[i]) < 16 && significand.leadingZeroBitCount > 0 {
      let bits = significand.leadingZeroBitCount
      significand &<<= bits
      // Fill that many bits from the next digit
      let digit = hexdigit(input[i])
      let upperPartialDigit = digit >> (4 - bits)
      significand += UInt64(upperPartialDigit)
      // Did we drop any non-zero bits?
      remainderNonZero = remainderNonZero || ((digit - (upperPartialDigit << (4 - bits))) != 0)
      i += 1
    }
    // Verify any remaining digits after the binary point
    while i < input.count && hexdigit(input[i]) < 16 {
      let digit = hexdigit(input[i])
      remainderNonZero = remainderNonZero || (digit != 0)
      i += 1
    }
    if i == firstDigitOffset + 1 { // Only a decimal point, no digits
      return .failure
    }
  } else if i == firstDigitOffset { // No decimal point, no digits
    return .failure
  }

  // If there's an exponent phrase, parse that out and
  // fold it into the binary exponent
  if i < input.count && (input[i] | 0x20) == 0x70 { // 'p' or 'P'
    i += 1
    var exponentSign = +1
    if i >= input.count {
      // Truncated exponent phrase: "0x1.234p"
      return .failure
    } else if input[i] == 0x2d { // '-'
      exponentSign = -1
      i += 1
    } else if input[i] == 0x2b { // '+'
      exponentSign = +1
      i += 1
    }

    if i >= input.count {
      // Truncated exponent phrase: "0x1.234p+"
      return .failure
    }
    var explicitExponent = 0
    // Truncate exponents bigger than 1 million
    while i < input.count && hexdigit(input[i]) < 10 {
      if explicitExponent < 999999 {
        explicitExponent *= 10
        explicitExponent += Int(hexdigit(input[i]))
      }
      i += 1
    }
    binaryExponent += explicitExponent * exponentSign
  }
  if i < input.count { // Trailing garbage
    return .failure
  }
  if significand == 0 { // All digits were zero
    return .zero(sign: sign)
  }

  if binaryExponent > targetFormat.maxBinaryExponent {
    return .infinity(sign: sign) // Overflow
  }

  if binaryExponent < targetFormat.minBinaryExponent - targetFormat.significandBits {
    return .zero(sign: sign) // Underflow
  }

  // Select the appropriate number of bits for the target format,
  // and use the rest to correctly round the final result

  var significantBits: Int
  if binaryExponent >= targetFormat.minBinaryExponent {
    significantBits = targetFormat.significandBits
  } else {
    significantBits = binaryExponent - targetFormat.minBinaryExponent + targetFormat.significandBits
    binaryExponent = targetFormat.minBinaryExponent - 1 // Subnormal exponent
  }

  // Align the significand so the most-significant bit
  // is the MSbit of the accumulator.
  significand <<= significand.leadingZeroBitCount

  // Narrow to the target format and compute a 0.64 fraction
  // for the rest
  var targetSignificand = significand >> (64 - significantBits)

  // Folding in `remainderNonZero` here helps us distinguish
  // "1.00000000000000000000000000000000000000" (== 1.0) from
  // "1.00000000000000000000000000000000000001" (> 1.0)
  // without needing arbitrary-precision integers for the significand.
  // It is `true` if any bits that were too insignificant to store
  // were actually non-zero and might therefore cause the result to
  // be rounded up.
  let significandFraction = (significand << significantBits) + (remainderNonZero ? 1 : 0)

  // Round up if fraction is > 1/2 or == 1/2 with odd significand
  if (significandFraction > 0x8000000000000000
      || (significandFraction == 0x8000000000000000
          && (targetSignificand & 1) == 1)) {
    // Round up, test for overflow
    targetSignificand += 1
    if targetSignificand >= (UInt64(1) << targetFormat.significandBits) {
      // Normal overflowed, need to renormalize
      targetSignificand >>= 1
      binaryExponent += 1
    } else if (binaryExponent < targetFormat.minBinaryExponent
               && targetSignificand >= (UInt64(1) << (targetFormat.significandBits - 1))) {
      // Subnormal overflowed to normal
      binaryExponent += 1
    }
  }
  return .binary(significand: targetSignificand,
                 exponent: Int16(binaryExponent),
                 sign: sign)
}

// ================================================================
// ================================================================
//
// NaN parsing
//
// ================================================================
// ================================================================

fileprivate func nan_payload(
  input: Span<UInt8>,
  start: Int,
  end: Int
) -> UInt64? {
  if start == end {
    return 0
  }
  var p = start

  // Figure out what base we're using
  var base: UInt64 = 10 // Decimal by default
  if p + 1 < end {
    if input[p] == 0x30 {
      if input[p + 1] == 0x78 { // '0x' => hex
        p += 2
        base = 16
      } else { // '0' => octal
        p += 1
        base = 8
      }
    }
  }
  // Accumulate the payload, preserving only
  // the least-significant 64 bits.
  var payload : UInt64 = 0
  for i in p..<end {
    let d = hexdigit(input[i])
    if d >= base {
      return nil
    }
    payload &*= UInt64(base)
    payload &+= UInt64(d)
  }
  return payload
}

// ================================================================
// ================================================================
//
// Initial parse (independent of target format)
//
// ================================================================
// ================================================================

// Following Lemire, this collects the first 19 digits of a decimal
// input into a 64-bit accumulator and returns that along with the
// parsed exponent.
//
// If there are more than 19 digits, it includes the offset of the
// first digit not included in the accumulator and the count of such
// digits.  This allows callers to parse those extra digits only when
// needed.  This is an important optimization: For >99% of all inputs,
// you can parse Double with correct rounding by inspecting only the
// first 19 digits.  So we can use high-performance fixed-width
// integer arithmetic for most cases, only falling back to
// arbitrary-precision arithmetic for the <1% of inputs that need such
// handling to obtain correct results.
//
// In downstream use, the 19 digit prefix is treated as an integer
// significand; the remaining digits are considered the fractional part of
// the significand.  In essence, the decimal point is moved to just
// after this prefix of 19 digits or less by appropriately adjusting
// the decimal exponent.
//
// Extreme values (ones obviously out of range of the current
// floating point format) are rounded here to infinity/zero.
// Malformed input returns `.failure`.
//
// For special forms, the `ParseResult` enum includes sufficient
// detail for the caller to immediately construct and return an
// appropriate result:
// * Hex floats returned as `.binary`
// * Literal Infinity returned as `.infinity`
// * NaN with or without payload returned as `.nan`
// * True zero returned as `.zero`

fileprivate func fastParse64(
  targetFormat: TargetFormat,
  input: Span<UInt8>
) -> ParseResult {
  var i = 0
  var sign: FloatingPointSign

  // Reject empty strings or grotesquely overlong ones
  // Among other concerns, we want to be sure that 32-bit and 16-bit
  // exponent calculations below don't overflow.
  if input.count == 0 || input.count > 16384 {
    return .failure
  }
  // Is this positive or negative?
  let maybeSign = unsafe input[unchecked: 0]
  if maybeSign == 0x2d { // '-'
    if input.count == 1 {
      return .failure
    }
    sign = .minus
    i &+= 1
  } else {
    if maybeSign == 0x2b { // '+'
      if input.count == 1 {
        return .failure
      }
      i &+= 1
    }
    sign = .plus
  }

  //
  // Handle leading zeros and special forms (hex, NaN, Inf)
  //
  let firstDigitOffset = i
  let first = unsafe input[unchecked: i]
  if first == 0x30 {
    if unsafe i &+ 1 < input.count && input[unchecked: i &+ 1] == 0x78 { // 'x'
      // Hex float
      return hexFloat(
        targetFormat: targetFormat,
        input: input,
        sign: sign,
        start: i)
    } else {
      // Skip leading zeros
      i &+= 1
      while unsafe i < input.count && input[unchecked: i] == 0x30 {
        i &+= 1
      }
      if i >= input.count {
        return .zero(sign: sign)
      }
    }
  } else if first > 0x39 {
    // Starts with a non-digit (and non-'.')
    // Hexfloats, infinity, NaN are all at least 3 characters
    if i &+ 3 > input.count { return .failure }
    let second = input[i &+ 1]
    let third = input[i &+ 2]
    if (first | 0x20) == 0x69 { // 'i' or 'I'
      // Infinity??
      if ((second | 0x20) == 0x6e // 'n' or 'N'
          && (third | 0x20) == 0x66) { // 'f' or 'F'
        if i &+ 3 == input.count {
          return .infinity(sign: sign)
        } else if i &+ 8 == input.count {
          if ((input[i &+ 3] | 0x20) == 0x69 // 'i' or 'I'
              && (input[i &+ 4] | 0x20) == 0x6e // 'n' or 'N'
              && (input[i &+ 5] | 0x20) == 0x69 // 'i' or 'I'
              && (input[i &+ 6] | 0x20) == 0x74 // 't' or 'T'
              && (input[i &+ 7] | 0x20) == 0x79) { // 'y' or 'Y'
            return .infinity(sign: sign)
          }
        }
      }
    } else if (first | 0x20) == 0x6e { // 'n' or 'N'
      if ((second | 0x20) == 0x61 // 'a' or 'A'
          && (third | 0x20) == 0x6e) { // 'n' or 'N'
        // NaN
        if i &+ 3 == input.count {
          // Bare 'nan' with no payload
          return .nan(payload: 0, signaling: false, sign: sign)
        } else if input[i &+ 3] == 0x28 && input[input.count &- 1] == 0x29 {
          // 'nan' with payload
          if let payload = nan_payload(input: input, start: i &+ 4, end: input.count &- 1) {
            return .nan(payload: payload, signaling: false, sign: sign)
          }
        }
      }
    } else if (first | 0x20) == 0x73 && i &+ 3 < input.count { // 's' or 'S'
      if ((second | 0x20) == 0x6e // 'n' or 'N'
          && (third | 0x20) == 0x61 // 'a' or 'A'
          && (input[i &+ 3] | 0x20) == 0x6e) { // 'n' or 'N'
        // sNaN
        if i &+ 4 == input.count {
          // Bare 'snan' with no payload
          return .nan(payload: 0, signaling: true, sign: sign)
        } else if input[i &+ 4] == 0x28 && input[input.count &- 1] == 0x29 {
          // 'snan' with payload
          if let payload = nan_payload(input: input, start: i &+ 5, end: input.count &- 1) {
            return .nan(payload: payload, signaling: true, sign: sign)
          }
        }
      }
    }
    return .failure
  }

  var base10Exponent = Int32(0)

  //
  // Collect digits before the decimal point (if any)
  //
  // Following Lemire, this collects digits into a UInt64 and
  // deliberately allows that value to overflow if the significand
  // is large.  This makes the common case (Double with 17 or fewer
  // digits) faster by avoiding extra checks.  We'll track enough
  // information here to allow us to detect such overflow later on
  // and do a second more careful scan when that happens.
  //
  var leadingDigits = UInt64(0)
  var firstNonZeroDigitOffset = i
  // "count of digits starting with first non-zero digit",
  // but that's too long for a variable name, so...
  var nonZeroDigitCount = 0
  // Collect digits into "leadingDigits"
  var t = unsafe input[unchecked: i] &- 0x30
  if t < 10 {
    leadingDigits = UInt64(t)
    i &+= 1
    if i >= input.count {
      // Exactly one non-zero digit
      return .decimal(digits: leadingDigits,
                      base10Exponent: 0,
                      unparsedDigitCount: 0,
                      firstUnparsedDigitOffset: UInt16(i),
                      leadingDigitCount: 1,
                      sign: sign)
    }
    t = unsafe input[unchecked: i] &- 0x30
    while t < 10 && i < input.count {
      leadingDigits &*= 10
      leadingDigits &+= UInt64(t)
      i &+= 1
      t = unsafe input[unchecked: i] &- 0x30
    }
    nonZeroDigitCount = i - firstNonZeroDigitOffset
  }

  var unparsedDigitCount = 0

  //
  // If there's a decimal point, process digits beyond it
  //
  if unsafe i < input.count && input[unchecked: i] == 0x2e { // '.'
    i &+= 1
    let firstDigitAfterDecimalPointOffset = i
    var firstSignificantDigitAfterDecimalPointOffset = i

    // Skip leading zeros after decimal point:
    // "0.000000001234" has 4 significant digits
    if nonZeroDigitCount == 0 {
      while unsafe i < input.count && input[unchecked: i] == 0x30 {
        i &+= 1
      }
      firstNonZeroDigitOffset = i
      firstSignificantDigitAfterDecimalPointOffset = i
    }
    if i < input.count {
      // Note: While testing `.description` + `Double(_:String)` against
      // random Double values to validate round-trip correctness, almost
      // 1/3 of `float_parse64()` is spent in the following loop.

      // TODO: Evaluate SIMD or SWAR techniques here...
      // In the common case of input such as "1.794373235e+12",
      // we should be able to use `input.count - i` to estimate
      // the number of digits after the decimal point.

      // In particular, these techniques may be relevant:
      // https://lemire.me/blog/2018/09/30/quickly-identifying-a-sequence-of-digits-in-a-string-of-characters/
      // and
      // https://lemire.me/blog/2022/01/21/swar-explained-parsing-eight-digits/
      var t = unsafe input[unchecked: i] &- 0x30
      while t < 10 && i < input.count {
        leadingDigits &*= 10
        leadingDigits &+= UInt64(t)
        i &+= 1
        t = unsafe input[unchecked: i] &- 0x30
      }
    }
    nonZeroDigitCount &+= i &- firstSignificantDigitAfterDecimalPointOffset
    base10Exponent &-= Int32(truncatingIfNeeded: i &- firstDigitAfterDecimalPointOffset)

    if i == firstDigitOffset &+ 1 {
      return .failure // No digits, only a '.'
    }
  } else if i == firstDigitOffset {
    return .failure // No digits
  }

  // Parse an exponent phrase "e+123"
  if unsafe i < input.count && (input[unchecked: i] | 0x20) == 0x65 { // 'e' or 'E'
    i &+= 1
    var exponentSign = Int32(1)
    if i >= input.count {
      // Truncated exponent phrase: "1.234e"
      return .failure
    } else if unsafe input[unchecked: i] == 0x2d { // '-'
      exponentSign = -1
      i &+= 1
    } else if unsafe input[unchecked: i] == 0x2b { // '+'
      exponentSign = +1
      i &+= 1
    }

    if i >= input.count {
      // Truncated exponent phrase: "1.234e+"
      return .failure
    }

    // For speed, we let the exponent overflow here.
    var explicitExponent = Int32(0)
    let firstExponentDigitOffset = i
    var byte = unsafe input[unchecked: i]
    while i < input.count && byte >= 0x30 && byte <= 0x39 {
      explicitExponent &*= 10
      explicitExponent &+= Int32(byte) &- 0x30
      i &+= 1
      byte = unsafe input[unchecked: i]
    }
    // Did we scan more than 8 digits in the exponent?
    if i &- firstExponentDigitOffset > 8 {
      // If so, explicitExponent might have overflowed.  Scan
      // again more carefully to correctly handle both
      // "1e0000000000000001" (== 10.0) and "1e99999999999999" (== inf)
      i = firstExponentDigitOffset // restart at beginning of exponent
      explicitExponent = 0
      repeat {
        let byte = input[i]
        if  byte < 0x30 || byte > 0x39 {
          return .failure
        }

        if explicitExponent < 999999 {
            explicitExponent *= 10
            explicitExponent += Int32(byte) - 0x30
        }
        i &+= 1
      } while i < input.count
    }

    base10Exponent &+= explicitExponent &* exponentSign
  }

  // If we didn't scan exactly to the end of our input, then it's
  // malformed.
  if i != input.count {
    return .failure
  }

  // If there are no non-zero digits, then this is
  // an explicit zero such as "0", "0.0", or "0e0"
  if nonZeroDigitCount == 0 {
    return .zero(sign: sign)
  }

  // Round rediculously large values to infinity
  // In particular, this ensures that downstream calculations
  // will never be faced with an outrageous value for `base10Exponent`
  if base10Exponent &+ Int32(truncatingIfNeeded: nonZeroDigitCount) > targetFormat.maxDecimalExponent {
    return .infinity(sign: sign)
  }
  // Round rediculously small values to zero
  if base10Exponent &+ Int32(truncatingIfNeeded: nonZeroDigitCount) < targetFormat.minDecimalExponent {
    return .zero(sign: sign)
  }

  let firstUnparsedDigitOffset : UInt16
  if _fastPath(nonZeroDigitCount <= 19) {
    // Common case with <= 19 digits:  `leadingDigits`
    // holds the entire significand
    firstUnparsedDigitOffset = 0
    unparsedDigitCount = 0
  } else {
    // The `leadingDigits` accumulator probably overflowed, so we
    // need to recover.  We already know the total number of
    // digits and we've already verified the syntax is correct, so
    // we just re-scan the first 19 digits here.
    leadingDigits = 0
    var leadingDigitCount = 0
    i = firstNonZeroDigitOffset
    while leadingDigitCount < 19 {
      let t = unsafe input[unchecked: i] &- 0x30
      if t < 10 {
        leadingDigits &*= 10
        leadingDigits &+= UInt64(t)
        leadingDigitCount &+= 1
      }
      i &+= 1
    }
    firstUnparsedDigitOffset = UInt16(i)
    unparsedDigitCount = nonZeroDigitCount - 19
    base10Exponent &+= Int32(truncatingIfNeeded: nonZeroDigitCount - 19)
  }

  return .decimal(digits: leadingDigits,
                  base10Exponent: Int16(truncatingIfNeeded: base10Exponent),
                  unparsedDigitCount: UInt16(truncatingIfNeeded: unparsedDigitCount),
                  firstUnparsedDigitOffset: firstUnparsedDigitOffset,
                  leadingDigitCount: UInt8(truncatingIfNeeded: nonZeroDigitCount - unparsedDigitCount),
                  sign: sign)
}

// ================================================================
// ================================================================
//
// Arbitrary-precision decimal-to-binary conversion
//
// ================================================================
// ================================================================

// First, we have a bunch of utilities for various basic
// arithmetic operations on multiple-precision values.
// These values are represented as a series of words
// stored in a sub-range of a MutableSpan<MPWord>.
// Conventionally, the full span is provided as `work`
// and the range corresponding to the particular value
// in question is provided as `range`.  Words are
// stored with the least-significant word in the lowest
// index.  Routines that need to grow the value do so by
// expanding the upper end of the range.

// Single word of a multiple-precision integer
typealias MPWord = UInt32
// Big enough to hold two words of a multiple-precision integer
typealias MPDWord = UInt64

// Shift the multi-precision integer left by
// the indicated number of bits, extending as needed
// at the most-significant end.
// This effectively multiplies the value by 2^shift
fileprivate func shiftLeftMP(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  shift: Int
) {
  let bitsPerMPWord = MPWord.bitWidth
  let wordsShift = shift / bitsPerMPWord
  let bitsShift = shift % bitsPerMPWord

  var src = range.upperBound &- 1
  var dest = src &+ wordsShift

  var t = UInt64(unsafe work[unchecked: src])
  src &-= 1

  t &<<= bitsShift
  if (t >> bitsPerMPWord)  > 0 {
    dest &+= 1
  } else {
    t &<<= bitsPerMPWord
    if src >= range.lowerBound {
      t |= UInt64(unsafe work[unchecked: src]) &<< bitsShift
      src &-= 1
    }
  }
  range = unsafe Range(_uncheckedBounds: (lower: range.lowerBound, upper: dest &+ 1))

  while src >= range.lowerBound {
    unsafe work[unchecked: dest] = MPWord(truncatingIfNeeded: t >> bitsPerMPWord)
    dest &-= 1
    t &<<= bitsPerMPWord
    t |= UInt64(unsafe work[unchecked: src]) &<< bitsShift
    src &-= 1
  }

  while dest >= range.lowerBound {
    unsafe work[unchecked: dest] = MPWord(truncatingIfNeeded: t >> bitsPerMPWord)
    dest &-= 1
    t &<<= bitsPerMPWord
  }
}

// Divide two multiple-precision integers
//
// Following "Algorithm D" from Knuth AOCP Section 4.3.1
// Refer to Knuth's description for details.
// Inputs:
//   Numerator, Denominator as ranges within the work area
// Outputs:
//   quotient stored in numerator area
//   numerator is destroyed
//   nonZeroRemainder set to true iff remainder was non-zero
//

/*
// Utility for debugging `divideMPbyMP`: This dumps a
// multi-precision integer as a single hex number.
fileprivate func mpAsHex(work: borrowing MutableSpan<MPWord>, lower: Int, upper: Int) -> String {
    var i = upper - 1
    var out = "0x0"
    let chars = ["0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"]
    while i >= lower {
        let n = work[i]
        for digit in 0..<8 {
            let bits = (n >> (28 - (digit * 4))) & 0x0f
            out += chars[Int(bits)]
        }
        i -= 1
    }
    return out
}
*/

@inline(never)
fileprivate func divideMPbyMP(
  work: inout MutableSpan<MPWord>,
  numerator: inout Range<Int>,
  denominator: inout Range<Int>,
  remainderNonZero: inout Bool
) {
  _internalInvariant(numerator.upperBound > numerator.lowerBound)
  _internalInvariant(denominator.upperBound > denominator.lowerBound)
  // Denominator and numerator cannot overlap
  _internalInvariant(denominator.lowerBound > numerator.upperBound)
  _internalInvariant(work[numerator.upperBound - 1] != 0)
  _internalInvariant(work[denominator.upperBound - 1] != 0)

  let bitsPerMPWord = MPWord.bitWidth

  // Full long division algorithm assumes denominator is more than 1 word,
  // so we need to handle the 1-word case separately.
  if denominator.upperBound &- denominator.lowerBound == 1 {
    let n = unsafe UInt64(work[unchecked: denominator.lowerBound])
    var t = UInt64(0)
    var i = numerator.upperBound
    while i > numerator.lowerBound {
      i &-= 1
      t &<<= bitsPerMPWord
      t &+= unsafe UInt64(work[unchecked: i])
      let q0 = t / n
      unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: q0)
      t &-= q0 &* n
    }
    remainderNonZero = (t != 0)
    i = numerator.upperBound
    while unsafe work[unchecked: i &- 1] == 0 {
      i &-= 1
    }
    numerator = unsafe Range(_uncheckedBounds: (lower: numerator.lowerBound, upper: i))
    return
  }

  // D1. Normalize: Multiply numerator and denominator by a power of 2
  // so that denominator has the most significant bit set in the
  // most significant word.  This guarantees that qhat below
  // will always be very good.
  let shift = work[denominator.upperBound &- 1].leadingZeroBitCount
  shiftLeftMP(work: &work, range: &denominator, shift: shift)
  shiftLeftMP(work: &work, range: &numerator, shift: shift)

  // Add a high-order word to the numerator if necessary
  var numerator_msw = numerator.upperBound
  let numerator_lsw = numerator.lowerBound
  if unsafe work[unchecked: numerator_msw &- 1] >= work[unchecked: denominator.upperBound &- 1] {
      unsafe work[unchecked: numerator_msw] = 0
      numerator_msw &+= 1
  }

  // D2. Iterate
  // Numerator and denominator must not be immediately adjacent in
  // memory, since we need an extra word for the remainder and quotient.
  // Through the following, the numerator area will contain successive
  // remainders and shrink as we iterate; the quotient will get filled
  // in word-by-word just above it in memory.
  assert(numerator_msw < denominator.lowerBound)

  var quotient_msw = numerator_msw
  var quotient_lsw = numerator_msw
  let denominator_words = denominator.upperBound &- denominator.lowerBound
  let iterations = (numerator_msw &- numerator_lsw) &- denominator_words
  for _ in 0..<iterations {
    // Debugging aid:
    // print("\(mpAsHex(work: work, lower: quotient_lsw, upper: quotient_msw)) + \(mpAsHex(work: work, lower: numerator_lsw, upper: numerator_msw)) / \(mpAsHex(work: work, lower: denominator.lowerBound, upper: denominator.upperBound))")

    // D3. Trial division of high-order bits
    // First divide 2 words of numerator by 1 word of denominator
    // Correct by considering 3rd word of numerator and 2nd of denominator
    // Per Knuth, qhat is either correct or 1 too high after this
    var qhat : MPWord
    let numerator2 = (UInt64(unsafe work[unchecked: numerator_msw &- 1]) << bitsPerMPWord + UInt64(unsafe work[unchecked: numerator_msw &- 2]))
    if unsafe work[unchecked: numerator_msw &- 1] == work[unchecked: denominator.upperBound &- 1] {
      qhat = MPWord.max
    } else {
      qhat = MPWord(numerator2 / UInt64(unsafe work[unchecked: denominator.upperBound &- 1]))
    }
    while true {
      let r = (numerator2 &- UInt64(qhat) &* UInt64(unsafe work[unchecked: denominator.upperBound &- 1]))
      if r <= UInt64(MPWord.max) &&
         (UInt64(unsafe work[unchecked: denominator.upperBound &- 2]) * UInt64(qhat)) > (UInt64(unsafe work[unchecked: numerator_msw &- 3]) &+ (r &<< bitsPerMPWord)) {
        qhat &-= 1
      } else {
        break
      }
    }

    // D4. numerator -= qhat * denominator
    var t = UInt64(0)
    var num = numerator_msw &- 1 &- denominator_words
    for d in denominator {
      t &+= UInt64(qhat) &* UInt64(unsafe work[unchecked: d])
      let tlower = MPWord(truncatingIfNeeded:t)
      let borrow = (unsafe work[unchecked: num] < tlower)
      // Must be &-= because this assumes wrapping arithmetic
      unsafe work[unchecked: num] &-= tlower
      t >>= bitsPerMPWord
      t &+= unsafe UInt64(unsafeBitCast(borrow, to: UInt8.self))
      num &+= 1
    }

    // D5/D6. qhat may have been one too high; if so, correct for that
    // Per Knuth, this happens very infrequently
    if unsafe work[unchecked: numerator_msw &- 1] < t {
      qhat &-= 1
      t = 0
      var n = numerator_msw &- 1 &- denominator_words
      for d in denominator {
        t &+= UInt64(unsafe work[unchecked: n]) + UInt64(unsafe work[unchecked: d])
        unsafe work[unchecked: n] = MPWord(truncatingIfNeeded: t)
        t >>= bitsPerMPWord
        n &+= 1
      }
    }
    quotient_lsw &-= 1
    unsafe work[unchecked: quotient_lsw] = qhat

    // D7. Iterate
    numerator_msw &-= 1
  }

  // D8. Post-process the remainder
  // Note:  Unlike Knuth's presentation, we don't care about
  // the exact remainder, only whether or not it's exactly zero.
  var remainderHash = MPWord(0)
  var i = numerator_lsw
  while i < numerator_msw {
    remainderHash |= unsafe work[unchecked: i]
    i &+= 1
  }
  remainderNonZero = (remainderHash != 0)

  // Normalize and return the quotient
  while unsafe work[unchecked: quotient_msw &- 1] == 0 {
    quotient_msw &-= 1
  }
  numerator = unsafe Range(_uncheckedBounds: (lower: quotient_lsw, upper: quotient_msw))
}

// Multiply a multi-precision value by a UInt32
fileprivate func multiplyMPByN(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  multiplier: UInt32
) {
  let bitsPerMPWord = MPWord.bitWidth
  var i = range.lowerBound
  var t = UInt64(0)
  while i < range.upperBound {
    t &+= UInt64(multiplier) &* UInt64(unsafe work[unchecked: i])
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  while t > 0 {
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  range = range.lowerBound..<i
}

// Multiply a multi-precision value by a UInt128
fileprivate func multiplyMPByN(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  multiplier: _UInt128
) {
  let bitsPerMPWord = MPWord.bitWidth
  var i = range.lowerBound
  var t = _UInt128(0)
  while i < range.upperBound {
    t &+= multiplier &* _UInt128(unsafe work[unchecked: i])
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  while t > 0 {
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  range = range.lowerBound..<i
}

// Add a UInt32 to a multi-precision value.
fileprivate func addToMP(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  addend: UInt32
) {
  let bitsPerMPWord = MPWord.bitWidth
  var i = range.lowerBound
  var t = UInt64(addend)
  while i < range.upperBound {
    t &+= UInt64(unsafe work[unchecked: i])
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  while t > 0 {
    unsafe work[unchecked: i] = MPWord(truncatingIfNeeded: t)
    t >>= bitsPerMPWord
    i &+= 1
  }
  range = range.lowerBound..<i
}

// Given the already-parsed first 19 digits and a reference to the
// rest of the digits still sitting in the input string, build a
// multi-precision integer with the full integer value (ignoring
// decimal point).
//
// Key optimization: Clinger (1990) observed that parsing an FP value is
// really a matter of comparing the input to the exact midpoints
// between pairs of representable floating-point values.  Because 2
// divides 10, the exact midpoints between adjacent binary
// floating-point values have exact finite decimal representations, so
// there is some maximum number of decimal digits among all such
// midpoints. From Clinger's observation, we only need to determine
// whether the input is above, below, or equal to one of those exact
// midpoints, which we can do by considering only
// `maxDecimalMidpointDigits` plus one bit indicating whether there
// are non-zero digits beyond those.  This allows us to handle
// arbitrarily-long inputs using arithmetic with a limited precision
// determined solely by the target format.

fileprivate func initMPFromDigits(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  leadingDigits: UInt64,
  leadingDigitCount: Int,
  input: Span<UInt8>,
  firstUnparsedDigitOffset: Int,
  unparsedDigitCount: Int,
  maxDecimalMidpointDigits: Int
) {
  let bitsPerMPWord = MPWord.bitWidth
  let lsw = range.lowerBound
  var msw = lsw
  var first19 = leadingDigits
  while first19 > 0 {
    work[msw] = MPWord(truncatingIfNeeded: first19)
    first19 >>= bitsPerMPWord
    msw &+= 1
  }

  range = lsw..<msw

  // The number of additional digits we need to parse
  var remainingDigitCount : Int
  // Additional digits beyond those will be summarized (per Clinger's observation)
  var summarizeDigitCount : Int
  let totalDigits = leadingDigitCount &+ unparsedDigitCount
  if totalDigits > maxDecimalMidpointDigits {
    remainingDigitCount = maxDecimalMidpointDigits &- (totalDigits &- unparsedDigitCount)
    summarizeDigitCount = unparsedDigitCount &- remainingDigitCount
  } else {
    remainingDigitCount = unparsedDigitCount
    summarizeDigitCount = 0
  }

  let powersOfTen : _InlineArray<10,UInt32> = [
    1, 10, 100, 1000, 10000, 100000,
    1000000, 10000000, 100000000, 1000000000
  ]

  // We know the only characters in this range are
  // ASCII digits '0'...'9' and '.', so we can simplify
  // some of the textual checks:
  var digitPos = firstUnparsedDigitOffset
  while remainingDigitCount > 0 {
    let batchSize = min(remainingDigitCount, 9)
    var batch = UInt32(0)
    for _ in 0..<batchSize {
      var digitChar = unsafe input[unchecked: digitPos]
      if digitChar == 0x2e { // Skip '.'
        digitPos &+= 1
        digitChar = unsafe input[unchecked: digitPos]
      }
      batch = batch &* 10 &+ UInt32(digitChar &- 0x30)
      digitPos &+= 1
    }
    multiplyMPByN(work: &work, range: &range, multiplier: unsafe powersOfTen[unchecked: batchSize])
    addToMP(work: &work, range: &range, addend: batch)
    remainingDigitCount &-= batchSize
  }

  // If there are more than maxDecimalMidpointDigits digits, we
  // summarize the digits beyond that with a single digit: zero if
  // all the extra digits are zero, else one.
  if summarizeDigitCount > 0 {
    multiplyMPByN(work: &work, range: &range, multiplier: UInt32(10))
    while summarizeDigitCount > 0 {
      let digit = unsafe input[unchecked: digitPos]
      if digit >= 0x30 { // Excludes '.' (0x2e)
        // It's not a zero digit (or decimal point), we're done.
        unsafe work[unchecked: lsw] &+= 1
        return
      }
      summarizeDigitCount &-= 1
      digitPos &+= 1
    }
  }
}

//
// Calculating Powers of Five
//
// There are two variations:  One initializes a multi-precision
// integer to a specific power of five.  The other multiplies
// a given multi-precision integer by a particular power of five.

// 32-bit powers of 5 from 5 ** 0 to 5 ** 13
fileprivate let powersOfFive_32 : _InlineArray<14, UInt32> = [
  1, 5, 25, 125, 625, 3125, 15625, 78125, 390625, 1953125,
  9765625, 48828125, 244140625, 1220703125
]

// 64-bit powers of 5 from 5 ** 14 to 5 ** 27
fileprivate let powersOfFive_64: _InlineArray<14, UInt64> = [
  6103515625, 30517578125, 152587890625, 762939453125, 3814697265625,
  19073486328125, 95367431640625, 476837158203125, 2384185791015625,
  11920928955078125, 59604644775390625, 298023223876953125,
  1490116119384765625, 7450580596923828125
]

// Multiply the multi-precision integer by 5^n

// This is the most expensive operation we have when converting
// very large Float80 values.  If that matters to you, it might
// be worthwhile looking for ways to improve this.
fileprivate func multiplyByFiveToTheN(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  power: Int
) {
  var remainingPower = power

  // Note: It might be possible to optimize this by using
  // fewer multiplications by larger values.  But a direct
  // implementation of this idea turned out to be just as
  // expensive as the simpler repeated multiplications here.

  // 5 ** 40 = 9094947017729282379150390625
  // Largest power of 5 such that multiplying it by any 32-bit value
  // gives a < 128-bit result
  while remainingPower >= 40 {
    let fiveToThe40 = _UInt128(high: 493038065, low: 14077307678380127585)
    multiplyMPByN(work: &work, range: &range, multiplier: fiveToThe40)
    remainingPower &-= 40
  }

  // We know remainingPower < 40, so we need at most one factor of 27
  // 5 ** 27 is the largest power of 5 that fits in 64 bits
  if remainingPower >= 27 {
    let fiveToThe27 = _UInt128(7450580596923828125)
    multiplyMPByN(work: &work, range: &range, multiplier: fiveToThe27)
    remainingPower &-= 27
  }

  if remainingPower >= 14 {
    // We know 14 <= remainingPower < 27
    let next = unsafe powersOfFive_64[unchecked: remainingPower &- 14]
    multiplyMPByN(work: &work, range: &range, multiplier: _UInt128(next))
  } else if remainingPower > 0 {
    // We know remainingPower < 14
    let next = unsafe powersOfFive_32[unchecked: remainingPower]
    multiplyMPByN(work: &work, range: &range, multiplier: next)
  }
}

// Table of pre-computed multi-word powers of 5.  These are the
// largest powers of 5 that it into successive numbers of 32-bit
// words.  (For example, 5 ** 13 is the largest power that fits in 1
// word, 5 ** 82 is the largest that fits in six words).  Each
// constant is broken down into 32-bit components stored from LSW to
// MSW, assuming that MPWord == UInt32.
fileprivate let powersOfFive : _InlineArray<_, UInt32> = [
  1220703125, // 5 ** 13
  4195354525, 1734723475, // 5 ** 27
  1520552677, 3503241150, 2465190328, // 5 ** 41
  4148285293, 4294227171, 3487696741, 3503246160, // 5 ** 55
  1377153393, 419140343, 3542739626, 1966161609, 995682444, // 5 ** 68
  3638046937, 1807989461, 112486150, 1644435829, 286361822, 1414949856, // 5 ** 82
  3776417409, 3833115195, 474402842, 2046101519, 1659368615, 1657637457, 2010764683, // 5 ** 96
  1399551081, 2264871549, 2930733413, 271785330, 1503646741, 3175827184, 883420894, 2857468478, // 5 ** 110
  3691486353, 2604572144, 3704384674, 3457541460, 101893061, 3173229722, 3228011613, 3028118404, 4060706939, // 5 ** 124
  // In theory, this table could be extended with the following powers:
  // 137, 151, 165, 179, 192, 206, 220, 234, 248, 261, 275,
  // 289, 303, 316, 330, 344, 358, 372, 385, 399
  // That would let us compute a power of 5 for any Double with
  // one constant lookup from this table and one multi-precision multiplication.
  // With a little fussing in the calculation below, we could economize by only
  // storing every second entry in the above list; that would give us any
  // Double with one constant lookup and two multiplications.
]

// Build a multi-precision integer 5^n

// This is obviously a performance bottleneck when parsing Float80
// values with large negative exponents, such as `1e-4000`.  But note
// that the exponent here is not necessarily limited to the range of
// the target format, which means we can in theory have performance
// concerns even for Float16.  Consider, for example
// `1000···000e-100000` with 100,000 zeros in the significand, which
// parses to `1.0`.  Without the space-bounding optimizations below,
// this would require dividing by `5 ** 100000`.  Even with those,
// Float64 can require up to `5 ** 768`.
fileprivate func fiveToTheN(
  work: inout MutableSpan<MPWord>,
  range: inout Range<Int>,
  power: Int
) {
  if power <= 13 {
    // Power <= 13 can be satisfied from the 32-bit table
    unsafe work[unchecked: range.lowerBound] = unsafe powersOfFive_32[unchecked: power]
    range = range.lowerBound..<(range.lowerBound &+ 1)
    return
  }
  if power <= 27 {
    // 14 <= Power <= 27 can be satisfied from the 64-bit table
    let t = unsafe powersOfFive_64[unchecked: power &- 14]
    unsafe work[unchecked: range.lowerBound] = MPWord(truncatingIfNeeded: t)
    unsafe work[unchecked: range.lowerBound &+ 1] = MPWord(truncatingIfNeeded: t >> 32)
    range = range.lowerBound..<(range.lowerBound &+ 2)
    return
  }

  // Otherwise, initialize with a multi-word value, then multiply
  // to get the final exact value.
  // The table of pre-computed values here supports a larger
  // range of exponents than the original C version, while
  // requiring only a small amount of fixed table storage.
  let maxPower = 124 // Largest power supported by the table above
  let clampedPower = power > maxPower ? maxPower : power
  // We need to find the appropriate power of five from the table
  // above.  First, how many words are in that?  (This formula
  // has been verified experimentally for every input up to 399.)
  let words = ((clampedPower &+ 1) &* 1189) >> 14
  // The power in the above table with that many words
  let seedPower = (words &* 441) >> 5
  // Find the starting offset of the constant in the table:
  let startIndex = words &* (words &- 1) / 2 // Triangular numbers
  for i in 0..<words {
    unsafe work[unchecked: range.lowerBound &+ i] = unsafe powersOfFive[unchecked: startIndex &+ i]
  }
  range = range.lowerBound..<(range.lowerBound &+ words)
  multiplyByFiveToTheN(work: &work, range: &range, power: power &- seedPower)
}

// Return the number of significant bits
// in the multi-precision integer.
fileprivate func bitCountMP(
  work: borrowing MutableSpan<MPWord>,
  range: Range<Int>
) -> Int {
  _internalInvariant(work[range.upperBound - 1] != 0)

  let bitsPerMPWord = MPWord.bitWidth
  let emptyBitsInMSWord = unsafe work[unchecked: range.upperBound &- 1].leadingZeroBitCount
  let totalBits = bitsPerMPWord &* (range.upperBound &- range.lowerBound)
  return totalBits &- emptyBitsInMSWord
}

// Returns a UInt64 with the most-significant
// `count` bits from the multi-precision integer.

// TODO: Float128 needs more than 64 bits. So to support Float128, we
// would need a separate 128-bit version.
@inline(never)
fileprivate func mostSignificantBitsFrom(
  work: borrowing MutableSpan<MPWord>,
  range: Range<Int>,
  count: Int,
  remainderNonZero: Bool
) -> UInt64 {
  let bitsPerMPWord = MPWord.bitWidth

  var i = range.upperBound &- 1
  var t = unsafe UInt64(work[unchecked: i])
  var remainderNonZero = remainderNonZero
  var fraction = UInt64(0)

  if _fastPath(i > range.lowerBound) {
    t &<<= bitsPerMPWord
    i &-= 1
    t |= unsafe UInt64(work[unchecked: i])

    if i > range.lowerBound {
      let extraBitCount = t.leadingZeroBitCount
      t &<<= extraBitCount
      i &-= 1
      let nextWord = unsafe UInt64(work[unchecked: i])

      // TODO: For Float80, we need to compute fraction here so we
      // can use these next bits for that

      t |= nextWord &>> (32 &- extraBitCount)

      if !remainderNonZero {
        var extraBits = nextWord &<< (32 &+ extraBitCount)
        while i > range.lowerBound {
          i &-= 1
          extraBits |= unsafe UInt64(work[unchecked: i])
        }
        remainderNonZero = (extraBits != 0)
      }
    }
  }

  t &<<= t.leadingZeroBitCount
  let roundUpNonZero = unsafe UInt64(unsafeBitCast(remainderNonZero, to: UInt8.self))

  // We special-case count == 0 here to avoid an extra
  // arithmetic check below for t >>= 64 - count
  if _slowPath(count == 0) {
    return (t - 1 + roundUpNonZero) >> 63
    // TODO: For Float80    } else if count == 64 {
  }

  fraction = t &<< count
  t &>>= 64 &- count

  if fraction == 0 {
    return t
  } else {
    let roundUpOddSignificand = (t & 1)
    let round = (fraction &- 1 &+ (roundUpNonZero | roundUpOddSignificand)) >> 63
    return t &+ round
  }
}

// Convert Decimal to Binary, with guaranteed correct results
// for any input.

// This computes a correctly-rounded result for any possible input,
// but it requires arbitrary-precision arithmetic, so the specific
// format handlers below use this as a fallback only when no faster
// method suffices.  Note that this is not as slow as you might think:
// In particular, it uses a fixed amount of stack-allocated storage
// whose size depends only on the target format, not on the length of
// the input.

fileprivate func slowDecimalToBinary(
  targetFormat: TargetFormat,
  input: Span<UInt8>,
  work: inout MutableSpan<MPWord>,
  digits: UInt64,
  base10Exponent parsedExponent: Int16,
  unparsedDigitCount: UInt16,
  firstUnparsedDigitOffset: UInt16,
  leadingDigitCount: UInt8,
  sign: FloatingPointSign
) -> ParseResult {
  // Number of digits in our input
  let digitCount = Int(leadingDigitCount) &+ Int(unparsedDigitCount)
  // Number of digits we actually need to use in calculations
  let significandDigits = min(digitCount, targetFormat.maxDecimalMidpointDigits &+ 1)
  let decimalExponent = Int(parsedExponent) &- significandDigits &+ digitCount &- Int(unparsedDigitCount)
  // Slightly over-estimate the number of bits needed to represent the decimal significand
  let significandBitsNeeded = (significandDigits &* 1701) >> 9
  let bitsPerMPWord = MPWord.bitWidth
  let significandWordsNeeded = (significandBitsNeeded &+ (bitsPerMPWord - 1)) / bitsPerMPWord

  var binaryExponent = 0
  let targetSignificand: UInt64

  if decimalExponent >= 0 {
    // ================================================================
    // Exponent >= 0

    // Multiply everything out to get the integer value.
    // Conceptually, the number of bits and the rounded high-order
    // bits in that integer give the binary exponent and
    // significand.

    // Slightly over-estimate the number of bits needed to represent 5^decimalExponent
    let exponentBitsNeeded = ((decimalExponent &+ 1) &* 1189) >> 9
    let exponentWordsNeeded = (exponentBitsNeeded &+ (bitsPerMPWord - 1)) / bitsPerMPWord

    let totalWordsNeeded = significandWordsNeeded &+ exponentWordsNeeded
    // TODO: For Float16/32/64, we always have a large enough work
    // space on the stack.  If we ever try to support Float80/128,
    // we may need to allow the caller to provide a smaller buffer
    // and allocate temporarily on the heap for extreme inputs.
    precondition(totalWordsNeeded <= work.count)

    // Where in the work buffer the current value resides
    var valueRange = 0..<0
    // Load the full user-provided decimal significand into
    // a multi-word integer
    initMPFromDigits(work: &work,
                     range: &valueRange,
                     leadingDigits: digits,
                     leadingDigitCount: Int(leadingDigitCount),
                     input: input,
                     firstUnparsedDigitOffset: Int(firstUnparsedDigitOffset),
                     unparsedDigitCount: Int(unparsedDigitCount),
                     maxDecimalMidpointDigits: targetFormat.maxDecimalMidpointDigits)
    // TODO: If `valueRange` at this point has 3 or fewer words
    // (96 bits or less), then we should load that value into a
    // local _UInt128, then overwrite `valueRange` with
    // `fiveToTheN` and `multiplyMPByN()`.  That would be faster than
    // using `multiplyByFiveToTheN()` for large exponents, since `fiveToTheN`
    // is consistently faster.

    // Multiply by 5^n
    multiplyByFiveToTheN(work: &work, range: &valueRange, power: decimalExponent)

    let bits = bitCountMP(work: work, range: valueRange)
    binaryExponent = bits &+ decimalExponent &- 1

    var significand = mostSignificantBitsFrom(work: work,
                                              range: valueRange,
                                              count: targetFormat.significandBits,
                                              remainderNonZero: false)

    if significand >= (UInt64(1) &<< targetFormat.significandBits) {
      significand >>= 1
      binaryExponent &+= 1
    }
    targetSignificand = significand
    if binaryExponent > targetFormat.maxBinaryExponent {
      return .infinity(sign: sign)
    }
    // binaryExponent is always positive here, so we can never
    // have an exponent less than minBinaryExponent and the
    // result can never be subnormal.
  } else {
    // ================================================================
    // Exponent < 0

    // Basic idea: Since decimalExponent is negative, we can't work
    // directly with 10^decimalExponent (it's an infinite binary
    // fraction), but 10^-decimalExponent is an integer.  So we use
    // varint arithmetic to compute
    //    digits / 10^(-decimalExponent)
    // scaling the numerator so that the quotient will have enough
    // bits (at least 53 for Double). The tricky part is keeping
    // the right information to accurately round the result.  To do so,
    // we need a few extra bits in the quotient and a record of whether
    // the remainder of the division was zero or not.

    // Slightly over-estimate the number of bits needed to represent 5^decimalExponent
    let exponentBitsNeeded = ((1 &- decimalExponent) &* 1189) >> 9
    let exponentWordsNeeded = (exponentBitsNeeded &+ (bitsPerMPWord - 1)) / bitsPerMPWord
    let numeratorBitsNeeded = max(significandBitsNeeded,
                                  exponentBitsNeeded &+ targetFormat.significandBits &+ 2)
    let numeratorWordsNeeded = (numeratorBitsNeeded &+ (bitsPerMPWord &- 1)) / bitsPerMPWord &+ 2
    let denominatorWordsNeeded = exponentWordsNeeded
    let totalWordsNeeded = numeratorWordsNeeded &+ denominatorWordsNeeded

    // TODO: Heap allocate if `work` isn't big enough
    // TODO: For Float16/32/64, we always have a large enough work
    // space on the stack.  If we ever try to support Float80/128,
    // we may need to allow the caller to provide a smaller buffer
    // and allocate temporarily on the heap for extreme inputs.
    precondition(totalWordsNeeded <= work.count)

    // Divide work buffer into Numerator storage followed by denominator storage
    var numeratorRange = 0..<0

    // Denominator holds power of 10^n
    // (Actually, 5^n, the remaining factor of 2^n is handled later.)
    var denominatorRange = numeratorWordsNeeded..<numeratorWordsNeeded
    fiveToTheN(work: &work, range: &denominatorRange, power: Int(0 &- decimalExponent))
    _internalInvariant(denominatorRange.upperBound &- denominatorRange.lowerBound <= denominatorWordsNeeded)
    _internalInvariant(work[denominatorRange.upperBound &- 1] != 0)
    _internalInvariant(denominatorRange.lowerBound >= numeratorWordsNeeded)

    // Populate numerator with digits
    initMPFromDigits(work: &work,
                     range: &numeratorRange,
                     leadingDigits: digits,
                     leadingDigitCount: Int(leadingDigitCount),
                     input: input,
                     firstUnparsedDigitOffset: Int(firstUnparsedDigitOffset),
                     unparsedDigitCount: Int(unparsedDigitCount),
                     maxDecimalMidpointDigits: targetFormat.maxDecimalMidpointDigits)
    _internalInvariant(numeratorRange.upperBound <= numeratorWordsNeeded)
    _internalInvariant(work[numeratorRange.upperBound &- 1] != 0)
    // Multiply the numerator by a power of two to ensure final
    // quotient has at least sigBits + 2 bits
    let denominatorBits = bitCountMP(work: work, range: denominatorRange)
    let numeratorBits = bitCountMP(work: work, range: numeratorRange)
    let numeratorShiftEstimate = denominatorBits &- numeratorBits &+ targetFormat.significandBits &+ 2;
    let numeratorShift : Int
    if numeratorShiftEstimate > 0 {
      shiftLeftMP(work: &work, range: &numeratorRange, shift: numeratorShiftEstimate)
      numeratorShift = numeratorShiftEstimate
    } else {
      numeratorShift = 0
    }

    // Divide, compute exact binaryExponent
    // Note: division is destructive, overwrites numerator with quotient
    var remainderNonZero = false
    divideMPbyMP(work: &work,
                 numerator: &numeratorRange,
                 denominator: &denominatorRange,
                 remainderNonZero: &remainderNonZero)
    let quotientRange = numeratorRange
    let quotientBits = bitCountMP(work: work, range: quotientRange)
    binaryExponent = quotientBits &- 1
    binaryExponent &+= decimalExponent
    binaryExponent &-= numeratorShift

    if binaryExponent > targetFormat.minBinaryExponent {
      // Normal decimal
      var significand = mostSignificantBitsFrom(work: work,
                                                range: quotientRange,
                                                count: targetFormat.significandBits,
                                                remainderNonZero: remainderNonZero)
      if significand >= (UInt64(1) &<< targetFormat.significandBits) {
        significand >>= 1
        binaryExponent &+= 1
      }
      targetSignificand = significand
      if binaryExponent > targetFormat.maxBinaryExponent {
        return .infinity(sign: sign)
      }
    } else if binaryExponent >= targetFormat.minBinaryExponent &- targetFormat.significandBits {
      // Subnormal decimal
      let bitsNeeded = binaryExponent &- (targetFormat.minBinaryExponent &- targetFormat.significandBits)
      binaryExponent = targetFormat.minBinaryExponent &- 1 // Flag as subnormal
      let significand = mostSignificantBitsFrom(work: work,
                                                range: quotientRange,
                                                count: bitsNeeded,
                                                remainderNonZero: remainderNonZero)
      // Usually, overflowing the expected number of bits doesn't
      // break anything; it just results in a significand 1 bit longer
      // than we expected.

      // Except when the significand overflows into the
      // exponent.  Then we've transitioned from a subnormal to
      // a normal, so the extra overflow bit will naturally get
      // dropped, we just have to bump the exponent.
      if significand >= (UInt64(1) &<< (targetFormat.significandBits &- 1)) {
        binaryExponent &+= 1
      }
      targetSignificand = significand
    } else {
      // Underflow
      return .zero(sign: sign)
    }
  }

  return .binary(significand: targetSignificand,
                 exponent: Int16(truncatingIfNeeded: binaryExponent),
                 sign: sign)
}

// ================================================================
// ================================================================
//
// Float16
//
// ================================================================
// ================================================================

#if !((os(macOS) || targetEnvironment(macCatalyst)) && arch(x86_64))

@available(SwiftStdlib 5.3, *)
@c(_swift_stdlib_strtof16_clocale)
@usableFromInline
internal func _swift_stdlib_strtof16_clocale(
  _ cText: Optional<UnsafePointer<CChar>>,
  _ output: Optional<UnsafeMutablePointer<Float16>>
) -> Optional<UnsafePointer<CChar>>
{
  // FIXME: This function was added during the Float16 bringup, but Unlike the
  // Float and Double versions of this function, it was never actually called
  // from inline code.  Can we just drop the whole thing?
  fatalError()
}

@available(SwiftStdlib 5.3, *)
internal func parse_float16(_ span: Span<UInt8>) -> Optional<Float16> {
  let targetFormat = TargetFormat(
    significandBits: 11,
    minBinaryExponent: -14,
    maxBinaryExponent: 15,
    minDecimalExponent: -7,
    maxDecimalExponent: 5,
    maxDecimalMidpointDigits: 22
  )

  // Verify the text format and parse the key pieces
  var parsed = fastParse64(targetFormat: targetFormat, input: span)

  // If we parsed a decimal, use `slowDecimalToBinary` to convert to binary
  if case .decimal(digits: let digits,
                   base10Exponent: let base10Exponent,
                   unparsedDigitCount: let unparsedDigitCount,
                   firstUnparsedDigitOffset: let firstUnparsedDigitOffset,
                   leadingDigitCount: let leadingDigitCount,
                   sign: let sign) = parsed {
    // ================================================================
    // Fixed-precision interval arithmetic
    // ================================================================
    // Except for the rounding of lower/upper significand below, this
    // is exactly identical to the float32 code.  With 53 extra bits
    // in the significand estimate here, we almost never fall through,
    // even with a larger-than-necessary interval width.  The only fall-through
    // cases in practice should be subnormals (which are simply not implemented
    // in the fast path yet).
    let intervalWidth: UInt64 = 40
    let powerOfTenRoundedDown = powersOf10_Float[Int(base10Exponent) + 70]
    let powerOfTenExponent = binaryExponentFor10ToThe(Int(base10Exponent))
    let normalizeDigits = digits.leadingZeroBitCount
    let d = digits << normalizeDigits
    let dExponent = 64 - normalizeDigits
    let l = multiply64x64RoundingDown(powerOfTenRoundedDown, d)
    let lExponent = powerOfTenExponent + dExponent
    let normalizeProduct = l.leadingZeroBitCount
    let normalizedL = l << normalizeProduct
    let normalizedLExponent = lExponent - normalizeProduct
    // Note: Wrapping is essential in the next two lines
    let lowerSignificand = (normalizedL &+ 0x0fffffffffffff) >> 53
    let upperSignificand = (normalizedL &+ 0x10000000000000 &+ intervalWidth) >> 53
    let binaryExponent = lowerSignificand == 0 ? normalizedLExponent : normalizedLExponent - 1

    // Now we have a binary exponent and upper/lower bounds on the
    // significand
    if binaryExponent > targetFormat.maxBinaryExponent {
      parsed = .infinity(sign: sign) // Overflow
    } else if binaryExponent < targetFormat.minBinaryExponent - targetFormat.significandBits {
      parsed = .zero(sign: sign) // Underflow
    } else if (binaryExponent > targetFormat.minBinaryExponent
                 && upperSignificand == lowerSignificand) {
      // Normal with converged bounds
      parsed = .binary(significand: lowerSignificand,
                       exponent: Int16(truncatingIfNeeded: binaryExponent),
                       sign: sign)
    } else {
      // ================================================================
      // Slow arbitrary-precision fallback
      // ================================================================
      // Work area big enough to correctly convert
      // any decimal input regardless of length to binary16:
      var workStorage = _InlineArray<16, MPWord>(repeating: MPWord(0))
      var work = workStorage.mutableSpan
      parsed = slowDecimalToBinary(targetFormat: targetFormat,
                                   input: span,
                                   work: &work,
                                   digits: digits,
                                   base10Exponent: base10Exponent,
                                   unparsedDigitCount: unparsedDigitCount,
                                   firstUnparsedDigitOffset: firstUnparsedDigitOffset,
                                   leadingDigitCount: leadingDigitCount,
                                   sign: sign)
    }
  }

  // Now we have either binary or a special form...
  switch parsed {
  case .binary(significand: let sig, exponent: let binaryExponent, sign: let sign):
    // Hex float or converted decimal
    // Parsers above give us the significand/exponent
    // already adjusted for subnormal, etc.
    // So the conversion here is simple:
    let exponentBits = UInt(binaryExponent + 15)
    return Float16(sign: sign,
                   exponentBitPattern: exponentBits,
                   significandBitPattern: UInt16(sig))
  case .zero(sign: let sign):
    // Literal zero or underflow
    if sign == .minus {
      return -0.0
    } else {
      return 0.0
    }
  case .infinity(sign: let sign):
    // Literal infinity or overflow
    if sign == .minus {
      return -Float16.infinity
    } else {
      return Float16.infinity
    }
  case .nan(payload: let payload, signaling: let signaling, sign: let sign):
    let p = 255 & Float16.RawSignificand(truncatingIfNeeded: payload)
    if sign == .minus {
      return -Float16(nan: p, signaling: signaling)
    } else {
      return Float16(nan: p, signaling: signaling)
    }
  default:
    return nil
  }
}
#endif


// ================================================================
// ================================================================
//
// Float32
//
// ================================================================
// ================================================================

// Caveat:  This function was called directly from inlineable
// code until Feb 2020 (commit 4d0e2adbef4d changed this),
// so it still needs to be exported with the same C-callable ABI
// for as long as we support code compiled with Swift 5.3.

@c(_swift_stdlib_strtof_clocale)
@usableFromInline
internal func _swift_stdlib_strtof_clocale(
  _ cText: Optional<UnsafePointer<CChar>>,
  _ output: Optional<UnsafeMutablePointer<Float>>
) -> Optional<UnsafePointer<CChar>>
{
  guard let cText = unsafe cText, let output = unsafe output else {
    return unsafe cText
  }
  var i = 0
  while unsafe cText[i] != 0 {
    i &+= 1
  }

  let charSpan = unsafe Span<UInt8>(_unchecked: cText, count: i)
  let result = parse_float32(charSpan)
  if let result {
    unsafe output.pointee = result
    return unsafe cText + i
  } else {
    return nil
  }
}

// Powers of 10 that can be _exactly_ represented in a Float32
fileprivate let floatPowersOf10_exact: _InlineArray<11, Float32> = [
  1.0, 10.0, 100.0, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10
]

internal func parse_float32(_ span: Span<UInt8>) -> Optional<Float32> {
  let targetFormat = TargetFormat(
    significandBits: 24,
    minBinaryExponent: -126,
    maxBinaryExponent: 127,
    minDecimalExponent: -46,
    maxDecimalExponent: 40,
    maxDecimalMidpointDigits: 113
  )

  // Verify the text format and parse the key pieces
  var parsed = fastParse64(targetFormat: targetFormat, input: span)

  // If we parsed a decimal, we need to convert to binary
  if case .decimal(digits: let digits,
                   base10Exponent: let base10Exponent,
                   unparsedDigitCount: let unparsedDigitCount,
                   firstUnparsedDigitOffset: let firstUnparsedDigitOffset,
                   leadingDigitCount: let leadingDigitCount,
                   sign: let sign) = parsed {

    // ================================================================
    // Use a single FP operation if the power-of-10 and digits both fit
    // ================================================================
    if base10Exponent > -11 && base10Exponent < 11 && leadingDigitCount < 8 {
      let sdigits = sign == .minus ? -Float(digits) : Float(digits)
      if base10Exponent < 0 {
        return sdigits / floatPowersOf10_exact[Int(-base10Exponent)]
      } else {
        return sdigits * floatPowersOf10_exact[Int(base10Exponent)]
      }
    }

    // ================================================================
    // Fixed-precision interval arithmetic
    // ================================================================
    // This uses fast 64-bit fixed-precision arithmetic to compute
    // upper and lower bounds for the significand.  If those bounds
    // agree, we can return the result.  With 40 fractional bits, this
    // should very rarely fall through, even with the pessimized
    // interval width here.  (The interval for <= 19 digits could be
    // reduced to 4, but the saved branch is more valuable here.)
    // The Float64 version of this code is extensively commented...
    let intervalWidth: UInt64 = 36
    let powerOfTenRoundedDown = powersOf10_Float[Int(base10Exponent) + 70]
    let powerOfTenExponent = binaryExponentFor10ToThe(Int(base10Exponent))
    let normalizeDigits = digits.leadingZeroBitCount
    let d = digits << normalizeDigits
    let dExponent = 64 - normalizeDigits
    let l = multiply64x64RoundingDown(powerOfTenRoundedDown, d)
    let lExponent = powerOfTenExponent + dExponent
    let normalizeProduct = l.leadingZeroBitCount
    let normalizedL = l << normalizeProduct
    let normalizedLExponent = lExponent - normalizeProduct
    // Note: Wrapping in the next two lines is possible, but that
    // just leads to losing the upper bit, which isn't stored
    // explicitly in IEEE754 formats anyway.  In effect, we're
    // using 65-bit arithmetic here.
    let lowerSignificand = (normalizedL &+ 0x7fffffffff) >> 40
    let upperSignificand = (normalizedL &+ 0x8000000000 &+ intervalWidth) >> 40
    let binaryExponent = lowerSignificand == 0 ? normalizedLExponent : normalizedLExponent - 1

    // Now we have a binary exponent and upper/lower bounds on the
    // significand
    if binaryExponent > targetFormat.maxBinaryExponent {
      // Overflow
      parsed = .infinity(sign: sign)
    } else if binaryExponent < targetFormat.minBinaryExponent - targetFormat.significandBits {
      // Underflow
      parsed = .zero(sign: sign)
    } else if (binaryExponent > targetFormat.minBinaryExponent
                 && upperSignificand == lowerSignificand) {
      // Normal with converged bounds
      parsed = .binary(significand: lowerSignificand,
                       exponent: Int16(truncatingIfNeeded: binaryExponent),
                       sign: sign)
    } else {
      // ================================================================
      // Slow arbitrary-precision fallback
      // ================================================================
      // Work area big enough to correctly convert
      // any decimal input regardless of length to binary32:
      var workStorage = _InlineArray<32, MPWord>(repeating: MPWord(0))
      var work = workStorage.mutableSpan
      parsed = slowDecimalToBinary(targetFormat: targetFormat,
                                   input: span,
                                   work: &work,
                                   digits: digits,
                                   base10Exponent: base10Exponent,
                                   unparsedDigitCount: unparsedDigitCount,
                                   firstUnparsedDigitOffset: firstUnparsedDigitOffset,
                                   leadingDigitCount: leadingDigitCount,
                                   sign: sign)
    }
  }

  // Now we have either binary or a special form...
  switch parsed {
  case .binary(significand: let sig, exponent: let binaryExponent, sign: let sign):
    // Hex float or converted decimal
    // Parsers above give us the significand/exponent
    // already adjusted for subnormal, etc.
    // So the conversion here is simple:
    let exponentBits = UInt(binaryExponent + 127)
    return Float32(sign: sign,
                   exponentBitPattern: exponentBits,
                   significandBitPattern: UInt32(sig))
  case .zero(sign: let sign):
    // Literal zero or underflow
    if sign == .minus {
      return -0.0
    } else {
      return 0.0
    }
  case .infinity(sign: let sign):
    // Literal infinity or overflow
    if sign == .minus {
      return -Float32.infinity
    } else {
      return Float32.infinity
    }
  case .nan(payload: let payload, signaling: let signaling, sign: let sign):
    let p = 0x1fffff & Float32.RawSignificand(truncatingIfNeeded: payload)
    if sign == .minus {
      return -Float32(nan: p, signaling: signaling)
    } else {
      return Float32(nan: p, signaling: signaling)
    }
  default:
    return nil
  }
}

// ================================================================
// ================================================================
//
// Float64
//
// ================================================================
// ================================================================

// Caveat:  This function was called directly from inlineable
// code until Feb 2020 (commit 4d0e2adbef4d changed this),
// so it still needs to be exported with the same C-callable ABI
// for as long as we support code compiled with Swift 5.3.

@c(_swift_stdlib_strtod_clocale)
@usableFromInline
internal func _swift_stdlib_strtod_clocale(
  _ cText: Optional<UnsafePointer<CChar>>,
  _ output: Optional<UnsafeMutablePointer<Double>>
) -> Optional<UnsafePointer<CChar>>
{
  guard let cText = unsafe cText, let output = unsafe output else {
    return unsafe cText
  }

  // i = strlen(cText)
  var i = 0
  while unsafe cText[i] != 0 {
    i &+= 1
  }

  let charSpan = unsafe Span<UInt8>(_unchecked: cText, count: i)
  let result = parse_float64(charSpan)
  if let result {
    unsafe output.pointee = result
    return unsafe cText + i
  } else {
    return nil
  }
}

// Powers of 10 that can be _exactly_ represented in a Float64
fileprivate let doublePowersOf10_exact: _InlineArray<23, Float64> = [
  1.0, 10.0, 100.0, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9, 1e10, 1e11,
  1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19, 1e20, 1e21, 1e22,
]

// TODO: Someday, this should be exposed as a public API with the
// signature: Double.init(_:UTF8Span)
internal func parse_float64(_ span: Span<UInt8>) -> Optional<Float64> {
  let targetFormat = TargetFormat(
    significandBits: 53,
    minBinaryExponent: -1022,
    maxBinaryExponent: 1023,
    minDecimalExponent: -325,
    maxDecimalExponent: 310,
    maxDecimalMidpointDigits: 768
  )

  // Verify the text format and parse the key pieces
  var parsed = fastParse64(targetFormat: targetFormat, input: span)

  // If we parsed a decimal, we need to convert it to binary
  if case .decimal(digits: let digits,
                   base10Exponent: let base10Exponent,
                   unparsedDigitCount: let unparsedDigitCount,
                   firstUnparsedDigitOffset: let firstUnparsedDigitOffset,
                   leadingDigitCount: let leadingDigitCount,
                   sign: let sign) = parsed {

    // ================================================================
    // A single FP operation provides correctly-rounded results if
    // all of the inputs are exact.  We go to some lengths here to
    // use this path for inputs with up to 17 digits.
    // ================================================================
    if base10Exponent >= -22 && base10Exponent < 19 {
      if base10Exponent < 0 {
        if leadingDigitCount <= 15 {
          // At most 15 digits with a negative exponent, we can
          // use a single correctly-rounded FP division
          let sdigits = sign == .minus ? -Double(digits) : Double(digits)
          return sdigits / doublePowersOf10_exact[Int(-base10Exponent)]
        }
      } else if base10Exponent == 0 {
        // At most 19 digits with a zero exponent, we can use
        // a single correctly-rounded int64-to-double conversion.
        if unparsedDigitCount == 0 {
          switch sign {
          case .plus:
            return Double(digits)
          case .minus:
            return Double(-Int64(truncatingIfNeeded: digits))
          }
        }
      } else if unparsedDigitCount == 0 {
        // At most 19 digits with a positive exponent; a little
        // prep work lets us use a single correctly-rounded FMA:
        let lowMask = UInt64(0x7ff)
        let highMask = ~lowMask
        let highDigits = Double(digits & highMask) // <= 53 bits
        let lowDigits = Double(digits & lowMask) // 11 bits
        // p10 is exact (10^18 has 42 bits)
        var p10 = doublePowersOf10_exact[Int(base10Exponent)]
        if sign == .minus { p10 = -p10 }
        let u = lowDigits * p10 // Exact (11 bits + 42 bits)
        // Inputs to FMA here are all exact, so result is correctly rounded
        return u.addingProduct(highDigits, p10)
      }
    }

    // ================================================================
    // Fixed-width interval arithmetic
    //
    // This copy is commented pretty extensively.  Everything here
    // also applies to the other formats that use similar logic.
    // Basic idea: We're going to compute upper and lower bounds for
    // the significand using fixed-precision arithmetic.  If they
    // agree, we're done.  Otherwise, we fall back to slow but fully
    // accurate arbitrary-precision calculations.
    // ================================================================

    // For performance reasons, we directly compute the lower bound and
    // then add a fixed interval to get the upper bound.  This sacrifices
    // some accuracy, of course, which means we'll fall through to the
    // slow path a little more often.  But testing shows that it pays off
    // on average.

    // 64-bit arithmetic gives us only 11 fractional bits in our significand
    // calculation, so it's worthwhile setting the interval width smaller
    // when we can.  If unparsedDigitCount > 0, then we have 19 decimal digits
    // in `digits` and can consider the input as if it were:
    //    (first 19 digits).(remaining digits) * 10^p
    // In this form, (first 19 digits) is a lower bound for the decimal
    // significand and (first 19 digits) + 1 is an upper bound.
    let intervalWidth: UInt64 = unparsedDigitCount == 0 ? 12 : 80

    // For code size, we compute an approximation to the power of 10
    // by multiplying two values together.  The coarse table stores
    // every 28th power of 10, the fine table stores powers from 0..<28
    let coarseIndex = (Int(base10Exponent) * 585 + 256) >> 14 // divide by 28
    let coarsePower = coarseIndex * 28
    let exactPower = Int(base10Exponent) - coarsePower // base10Exponent % 28
    let exact = powersOf10_Float[exactPower + 70]
    let coarse = powersOf10_CoarseBinary64[coarseIndex + 15]

    // The exact values are exact.  The coarse values are rounded,
    // but they're never rounded up, so:
    //    coarse <= true value <= coarse + 1
    let powerOfTenRoundedDown = multiply64x64RoundingDown(coarse, exact)
    // So the corresponding upper bound for the power of 10 is:
    //    <= ((coarse + 1) * exact + UINT64_MAX) >> 64
    //    <= (coarse * exact + exact + UINT64_MAX) >> 64
    //    <= powerOfTenRoundedDown + 2

    // Note: Constants in the power-of-10 table have the binary point
    // at the far left.  That is, they all have the form
    //     0.10101010... * 2^e
    // So the `powerOfTenRoundedDown` is the product of two numbers
    // in [1/2,1.0) and this is the corresponding power.
    let powerOfTenExponent = (binaryExponentFor10ToThe(coarsePower)
                                + binaryExponentFor10ToThe(exactPower))

    // Normalize the base-10 significand.  `digits` has the binary
    // point at the far right; when we multiply here, we'll switch
    // to the convention with the binary point at the far left by
    // adding 64 to our binary exponent.
    let normalizeDigits = digits.leadingZeroBitCount
    _internalInvariant(normalizeDigits <= 4 || unparsedDigitCount == 0)
    let d = digits << normalizeDigits
    let dExponent = 64 - normalizeDigits
    // The upper bound for d is:
    //   exactly d (for <= 19 digit case)
    //   d + 16 (for > 19 digit case)

    // A 64-bit lower bound on the binary significand
    let l = multiply64x64RoundingDown(powerOfTenRoundedDown, d)
    let lExponent = powerOfTenExponent + dExponent
    // In terms of `l128` = the full 128-bit product corresponding to `l`,
    // we see that the corresponding upper bound is:
    // <= 19 digits:  (powerOfTenRoundedDown + 2) * d == l128 + d + d
    //  > 19 digits:  (powerOfTenRoundedDown + 2) * (d + 16)

    // Rounding to 64 bits, the upper bound for <= 19 digits:
    //    (l128 + d + d + UINT64_MAX) >> 64   <=   l + 3
    // For >19 digits, we similarly get l + 20 as an upper bound.

    // Normalize again
    let normalizeProduct = l.leadingZeroBitCount
    _internalInvariant(normalizeProduct <= 2)
    // Upper bound is (l + 3) or (l + 20)
    let normalizedL = l << normalizeProduct
    // After normalizing, upper bound is (l + 12) or (l + 80)
    let normalizedLExponent = lExponent - normalizeProduct

    // We have 64-bit lower bound (53-bit significand + 11
    // fraction bits).  Add the interval width to get the upper
    // bound and round each one separately.  Using a rounding
    // offset slightly less than 0x400 == exact 1/2 for the lower
    // bound means that we never handle exact 1/2 ties in the fast
    // path.
    let lowerSignificand = (normalizedL &+ 0x3ff) >> 11
    let upperSignificand = (normalizedL &+ 0x400 &+ intervalWidth) >> 11

    // If the lower significand wrapped, we effectively overflowed
    // to a 65-bit result, which will show up as a zero value
    // here.  Because IEEE754 doesn't store the high bit, we don't
    // need to recover the high bit here, just account for it by
    // adding one to the exponent if lowerSignificand==0.

    // BUT: We also need at some point to switch conventions from
    // having a binary point at the far left to the IEEE754
    // convention with one significant bit to the left of the
    // binary point. So cumulatively, we add one if
    // lowerSignificand is zero and always subtract one, which
    // gives us:
    let binaryExponent = lowerSignificand == 0 ? normalizedLExponent : normalizedLExponent - 1

    // We have an accurate binary exponent for the converted value, so
    // can identify overflow/underflow pretty accurately here.
    if binaryExponent > targetFormat.maxBinaryExponent {
      parsed = .infinity(sign: sign) // Overflow
    } else if binaryExponent < targetFormat.minBinaryExponent - targetFormat.significandBits {
      parsed = .zero(sign: sign) // Underflow
    /*
    } else if binaryExponent <= targetFormat.minBinaryExponent {
      // TODO: Process subnormal here
    */
    } else if (binaryExponent > targetFormat.minBinaryExponent
                 && upperSignificand == lowerSignificand) {
      parsed = .binary(significand: lowerSignificand,
                       exponent: Int16(binaryExponent),
                       sign: sign)
    } else {
      // TODO: Can we exploit the known binaryExponent and/or
      // near-miss significand bounds to speed up
      // slowDecimalToBinary??  Note that the significand bounds
      // differ by only 1, so we know one of them is correct.
      // (In fact, if +/- one ULP is sufficient for you, you
      // could always choose the lowerSignificand above and not
      // have this slow path fallback at all.)

      // ================================================================
      // Slow arbitrary-precision fallback
      // ================================================================
      // Work area big enough to correctly convert
      // any decimal input regardless of length to binary64:
      var workStorage = _InlineArray<164, MPWord>(repeating: MPWord(0))
      var work = workStorage.mutableSpan
      parsed = slowDecimalToBinary(targetFormat: targetFormat,
                                   input: span,
                                   work: &work,
                                   digits: digits,
                                   base10Exponent: base10Exponent,
                                   unparsedDigitCount: unparsedDigitCount,
                                   firstUnparsedDigitOffset: firstUnparsedDigitOffset,
                                   leadingDigitCount: leadingDigitCount,
                                   sign: sign)
    }
  }

  // Now we have either binary or a special form...
  switch parsed {
  case .binary(significand: let sig, exponent: let binaryExponent, sign: let sign):
    // Hex float or converted decimal
    // Parsers above give us the significand/exponent
    // already adjusted for subnormal, etc.
    // So the conversion here is simple:
    let exponentBits = UInt(binaryExponent + 1023)
    return Float64(sign: sign,
                   exponentBitPattern: exponentBits,
                   significandBitPattern: UInt64(sig))
  case .zero(sign: let sign):
    // Literal zero or underflow
    if sign == .minus {
      return -0.0
    } else {
      return 0.0
    }
  case .infinity(sign: let sign):
    // Literal infinity or overflow
    if sign == .minus {
      return -Float64.infinity
    } else {
      return Float64.infinity
    }
  case .nan(payload: let payload, signaling: let signaling, sign: let sign):
    let p = 0x3ffffffffffff & Float64.RawSignificand(truncatingIfNeeded: payload)
    if sign == .minus {
      return -Float64(nan: p, signaling: signaling)
    } else {
      return Float64(nan: p, signaling: signaling)
    }
  default:
    return nil
  }
}

#endif // !_pointerBitWidth(_16)
