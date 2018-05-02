//===--- SwiftDtoa.c ---------------------------------------------*- c -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
// Note: This is really a C file, but Swift's build system for Linux is
// partially allergic to C, so it's being compiled as ".cpp" for now.  Please
// don't infect it with C++-isms.
//
///
/// The core algorithm here (see `swift_decompose_double` below) is a
/// modified form of the Grisu2 algorithm from Florian Loitsch;
/// "Printing Floating-Point Numbers Quickly and Accurately with
/// Integers", 2010.  https://dl.acm.org/citation.cfm?id=1806623
///
/// This includes some improvements suggested by the "Errol paper":
/// Marc Andrysco, Ranjit Jhala, Sorin Lerner; "Printing
/// Floating-Point Numbers: A Faster, Always Correct Method", 2016.
/// https://dl.acm.org/citation.cfm?id=2837654
///
/// The following summary assumes you're familiar with Grisu-style
/// algorithms in general:
///
/// Loitsch' original Grisu2 implementation guarantees round-trip
/// accuracy but only generates the shortest decimal expansion about 99%
/// of the time.  Grisu3 is similar, but fails rather than producing
/// a result that is not the shortest possible.
///
/// The Errol paper provides a deeper analysis of the cases where
/// Grisu2 fails to find the shortest decimal expansion. There
/// are two root causes of such failures:
///
/// * Insufficient precision leads to scattered failures across the
///   entire range.  The enumeration technique described in the Errol
///   paper shows a way to construct a superset of the numbers subject
///   to such failures.  With this list, we can simply test whether we
///   have sufficient precision.
///
///   For Double, the Errol3 algorithm uses double-double arithmetic
///   with about 106 bits precision. This turns out to be not quite
///   sufficient, requiring Errol3 to pre-screen the input against a
///   list of exceptions culled from the larger list of possible
///   failures.  Using high-precision integers, we've discovered that
///   110 bit precision is sufficient to satisfy the Errol test cases
///   without requiring any pre-screening.
///
///   For Float and Float80, the same approach shows that we need 53
///   and 135 bits, respectively.  It is an interesting coincidence
///   that for all three cases, an n-bit significand can be formatted
///   optimally with no more than 2n+7 bits of intermediate precision.
///
/// * Sometimes, the shortest value might occur exactly at the
///   midpoint between two adjacent binary floating-point values.
///   When converted back to binary, this will round to the adjacent
///   even significand.  We handle this by widening the interval
///   whenever the significand is even in order to allow these
///   exact midpoints to be considered.
///
/// In addition to addressing the shortness failures characterized in the Errol
/// paper, the implementation here also incorporates final-digit corrections
/// that allow it to produce the optimal decimal decomposition in all cases.
///
/// In summary, this implementation is:
///
/// * Fast.  It uses only fixed-width integer arithmetic and has
///   constant memory requirements.
///
/// * Simple. It is only a little more complex than Loitsch' original
///   implementation of Grisu2.  The full digit decomposition for double
///   is less than 300 lines of standard C, including routine arithmetic
///   helper functions.
///
/// * Always Accurate. Converting the decimal form back to binary
///   will always yield exactly the same value.  For the IEEE 754
///   formats, the round-trip will produce exactly the same bit
///   pattern in memory.
///
/// * Always Short.  This always selects an accurate result with the
///   minimum number of decimal digits.
///
/// * Always Close.  Among all accurate, short results, this always
///   chooses the result that is closest to the exact floating-point
///   value. (In case of an exact tie, it rounds the last digit even.)
///
/// For single-precision Float, we can simply test all 2^32 values.
/// This requires only a few minutes on a mid-range modern laptop. The
/// Double and Float80 formatters rely on results from the Errol paper
/// to ensure correctness.  In addition, we have verified more than
/// 10^14 randomly-chosen Double values by comparing the results to the
/// output of Grisu3 (where Grisu3 fails, we've compared to Errol4).
///
// ----------------------------------------------------------------------------

#include <float.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "swift/Runtime/SwiftDtoa.h"

#if defined(__SIZEOF_INT128__)
  // We get a significant speed boost if we can use the __uint128_t
  // type that's present in GCC and Clang on 64-bit architectures.  In
  // particular, we can do 128-bit arithmetic directly and can
  // represent 192-bit integers as a collection of 64-bit elements.
  #define HAVE_UINT128_T 1
#else
  // On 32-bit, we have to use slower code that manipulates 128-bit
  // and 192-bit integers as collections of 32-bit elements.
  #define HAVE_UINT128_T 0
#endif

// Try to verify that the system floating-point types really are what we
// expect.  Note that the code below is specific to these exact
// floating-point representations.

#if (FLT_RADIX != 2)
// Either you're using hexadecimal float format on S390, or you have a
// really broken C environment.
#error "This platform claims to not use binary floating point."
#endif

#if SWIFT_DTOA_FLOAT_SUPPORT
#if (FLT_MANT_DIG != 24) || (FLT_MIN_EXP != -125) || (FLT_MAX_EXP != 128)
#error "Are you certain `float` on this platform is really IEEE 754 single-precision binary32 format?"
#endif
#endif

#if SWIFT_DTOA_DOUBLE_SUPPORT
#if (DBL_MANT_DIG != 53) || (DBL_MIN_EXP != -1021) || (DBL_MAX_EXP != 1024)
#error "Are you certain `double` on this platform is really IEEE 754 double-precision binary64 format?"
#endif
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
#if (LDBL_MANT_DIG != 64) || (LDBL_MIN_EXP != -16381) || (LDBL_MAX_EXP != 16384)
#error "Are you certain `long double` on this platform is really Intel 80-bit extended precision?"
#endif
#endif


// See the implementations at the bottom of this file for detailed explanations
// of the purpose of each function.

//
// Helper functions used by float, double, and float80 machinery.
//

#if SWIFT_DTOA_FLOAT_SUPPORT || SWIFT_DTOA_DOUBLE_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT
static int binaryExponentFor10ToThe(int p);
static int decimalExponentFor2ToThe(int e);
#endif

//
// Helper functions used only by the single-precision float formatter
//

#if SWIFT_DTOA_FLOAT_SUPPORT
static uint64_t multiply64x32RoundingDown(uint64_t lhs, uint32_t rhs);
static uint64_t multiply64x32RoundingUp(uint64_t lhs, uint32_t rhs);
static uint64_t multiply64x64RoundingDown(uint64_t lhs, uint64_t rhs);
static uint64_t multiply64x64RoundingUp(uint64_t lhs, uint64_t rhs);
static void intervalContainingPowerOf10_Float(int p, uint64_t *lower, uint64_t *upper, int *exponent);
#endif

//
// Helpers used by both the double-precision and float80 formatters
//

#if SWIFT_DTOA_DOUBLE_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT
#if HAVE_UINT128_T
typedef __uint128_t swift_uint128_t;
#define initialize128WithHighLow64(dest, high64, low64) ((dest) = ((__uint128_t)(high64) << 64) | (low64))
#else
typedef struct {
    uint32_t low, b, c, high;
} swift_uint128_t;
#define initialize128WithHighLow64(dest, high64, low64) \
    ((dest).low = (uint32_t)(low64),                    \
     (dest).b = (uint32_t)((low64) >> 32),              \
     (dest).c = (uint32_t)(high64),                     \
     (dest).high = (uint32_t)((high64) >> 32))
#endif
#endif


//
// Helper functions used only by the double-precision formatter
//

#if SWIFT_DTOA_DOUBLE_SUPPORT
#if HAVE_UINT128_T
#define increment128(dest) ((dest) += 1)
#define isLessThan128x128(lhs, rhs) ((lhs) < (rhs))
#define subtract128x128(lhs, rhs) (*(lhs) -= (rhs))
#define multiply128xi32(lhs, rhs) (*(lhs) *= (rhs))
#define initialize128WithHigh64(dest, value) ((dest) = (__uint128_t)(value) << 64)
#define extractHigh64From128(arg) ((uint64_t)((arg) >> 64))
static int extractIntegerPart128(__uint128_t *fixed128, int fractionBits) {
    return (int)(*fixed128 >> fractionBits);
}
static void clearIntegerPart128(__uint128_t *fixed128, int fractionBits) {
    const swift_uint128_t fixedPointMask = (((__uint128_t)1 << fractionBits) - 1);
    *fixed128 &= fixedPointMask;
}
#else
#define increment128(dest)             \
    do {                               \
        uint64_t t = (dest).low + 1;   \
        (dest).low = (uint32_t)t;      \
        t >>= 32;                      \
        t += (dest).b;                 \
        (dest).b = (uint32_t)t;        \
        t >>= 32;                      \
        t += (dest).c;                 \
        (dest).c = (uint32_t)t;        \
        t >>= 32;                      \
        (dest).high += (uint32_t)t;    \
    } while (0)
static int isLessThan128x128(swift_uint128_t lhs, swift_uint128_t rhs);
static void subtract128x128(swift_uint128_t *lhs, swift_uint128_t rhs);
static void multiply128xi32(swift_uint128_t *lhs, uint32_t rhs);
#define initialize128WithHigh64(dest, value)            \
    ((dest).low = (dest).b = 0,                         \
     (dest).c = (uint32_t)(value),                      \
     (dest).high = (uint32_t)((value) >> 32))
#define extractHigh64From128(arg) (((uint64_t)(arg).high << 32)|((arg).c))
static int extractIntegerPart128(swift_uint128_t *fixed128, int fractionBits) {
    const int highFractionBits = fractionBits % 32;
    return (int)(fixed128->high >> highFractionBits);
}
static void clearIntegerPart128(swift_uint128_t *fixed128, int fractionBits) {
    const int highFractionBits = fractionBits % 32;
    fixed128->high &= ((uint32_t)1 << highFractionBits) - 1;
}
#endif
static swift_uint128_t multiply128x64RoundingDown(swift_uint128_t lhs, uint64_t rhs);
static swift_uint128_t multiply128x64RoundingUp(swift_uint128_t lhs, uint64_t rhs);
static swift_uint128_t shiftRightRoundingDown128(swift_uint128_t lhs, int shift);
static swift_uint128_t shiftRightRoundingUp128(swift_uint128_t lhs, int shift);
static void intervalContainingPowerOf10_Double(int p, swift_uint128_t *lower, swift_uint128_t *upper, int *exponent);
#endif

//
// Helper functions used only by the extended-precision long double formatter
//

#if SWIFT_DTOA_FLOAT80_SUPPORT
#if HAVE_UINT128_T
// A 192-bit unsigned integer type stored as 3 64-bit words
typedef struct {uint64_t low, mid, high;} swift_uint192_t;
#define initialize192WithHighMidLow64(dest, high64, mid64, low64) \
    ((dest).low = (low64),                                        \
     (dest).mid = (mid64),                                        \
     (dest).high = (high64))
#else
// A 192-bit unsigned integer type stored as 6 32-bit words
typedef struct {uint32_t low, b, c, d, e, high;} swift_uint192_t;
#define initialize192WithHighMidLow64(dest, high64, mid64, low64) \
    ((dest).low = (uint64_t)(low64),                              \
     (dest).b = (uint64_t)(low64) >> 32,                          \
     (dest).c = (uint64_t)(mid64),                                \
     (dest).d = (uint64_t)(mid64) >> 32,                          \
     (dest).e = (uint64_t)(high64),                               \
     (dest).high = (uint64_t)(high64) >> 32)
#endif
static void multiply192x64RoundingDown(swift_uint192_t *lhs, uint64_t rhs);
static void multiply192x64RoundingUp(swift_uint192_t *lhs, uint64_t rhs);
static void multiply192xi32(swift_uint192_t *lhs, uint32_t rhs);
static void multiply192x128RoundingDown(swift_uint192_t *lhs, swift_uint128_t rhs);
static void multiply192x128RoundingUp(swift_uint192_t *lhs, swift_uint128_t rhs);
static void subtract192x192(swift_uint192_t *lhs, swift_uint192_t rhs);
static int isLessThan192x192(swift_uint192_t lhs, swift_uint192_t rhs);
static void shiftRightRoundingDown192(swift_uint192_t *lhs, int shift);
static void shiftRightRoundingUp192(swift_uint192_t *lhs, int shift);
static void intervalContainingPowerOf10_Float80(int p, swift_uint192_t *lower, swift_uint192_t *upper, int *exponent);
#endif

//
// --------------- Digit generation ---------------------
//

// This is the interesting part.

// These routines take a floating-point value and efficiently compute
// everything necessary to write an optimal base-10 representation of
// that value.  In particular, they compute the base-10 exponent and
// corresponding digits.

// swift_decompose_double is thoroughly commented; swift_decompose_float
// and swift_decompose_float80 are fundamentally the same algorithm, but
// adjusted to perform optimally for those types.

#if SWIFT_DTOA_DOUBLE_SUPPORT
// Return raw bits encoding the double
static uint64_t bitPatternForDouble(double d) {
    union { double d; uint64_t u; } converter;
    converter.d = d;
    return converter.u;
}

int swift_decompose_double(double d,
    int8_t *digits, size_t digits_length, int *decimalExponent)
{
    // Bits in raw significand (not including hidden bit, if present)
    static const int significandBitCount = DBL_MANT_DIG - 1;
    static const uint64_t significandMask
        = ((uint64_t)1 << significandBitCount) - 1;
    // Bits in raw exponent
    static const int exponentBitCount = 11;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // Note: IEEE 754 conventionally uses 1023 as the exponent
    // bias.  That's because they treat the significand as a
    // fixed-point number with one bit (the hidden bit) integer
    // portion.  The logic here reconstructs the significand as a
    // pure fraction, so we need to accomodate that when
    // reconstructing the binary exponent.
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 1022

    // Step 0: Deconstruct the target number
    // Note: this strongly assumes IEEE 754 binary64 format
    uint64_t raw = bitPatternForDouble(d);
    int exponentBitPattern = (raw >> significandBitCount) & exponentMask;
    uint64_t significandBitPattern = raw & significandMask;

    // Step 1: Handle the various input cases:
    int binaryExponent;
    uint64_t significand;
    if (digits_length < 17) {
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
        // Return no digits
        return 0;
    } else if (exponentBitPattern == 0) {
        if (significandBitPattern == 0) { // Zero
            digits[0] = 0;
            *decimalExponent = 0;
            return 1;
        } else { // subnormal
            binaryExponent = 1 - exponentBias;
            significand = significandBitPattern
                          << (64 - significandBitCount - 1);
        }
    } else { // normal
        binaryExponent = exponentBitPattern - exponentBias;
        uint64_t hiddenBit = (uint64_t)1 << significandBitCount;
        uint64_t fullSignificand = significandBitPattern + hiddenBit;
        significand = fullSignificand << (64 - significandBitCount - 1);
    }

    // Step 2: Determine the exact unscaled target interval

    // Grisu-style algorithms construct the shortest decimal digit
    // sequence within a specific interval.  To build the appropriate
    // interval, we start by computing the exact midpoints between
    // this floating-point value and the adjacent ones.

    uint64_t halfUlp = (uint64_t)1 << (64 - significandBitCount - 2);
    uint64_t quarterUlp = halfUlp >> 1;
    uint64_t upperMidpointExact = significand + halfUlp;

    int isBoundary = significandBitPattern == 0;
    uint64_t lowerMidpointExact
        = significand - (isBoundary ? quarterUlp : halfUlp);

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
    // `p` will ensure that `2^e * 10^-p` is going to be close to 1.
    // So multiplying the first three terms yields a fraction suitable
    // for producing the decimal digits.  So we need to estimate `p`:

    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor
    // Compute `10^-p` to 128-bit precision.  We generate
    // both over- and under-estimates to ensure we can exactly
    // bound the later use of these values.
    swift_uint128_t powerOfTenRoundedDown;
    swift_uint128_t powerOfTenRoundedUp;
    int powerOfTenExponent = 0;
    intervalContainingPowerOf10_Double(-base10Exponent,
                                       &powerOfTenRoundedDown,
                                       &powerOfTenRoundedUp,
                                       &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)

    // As mentioned above, the final digit generation works
    // with an interval, so we actually apply the scaling
    // to the upper and lower midpoint values separately.

    // As part of the scaling here, we'll switch from a pure
    // fraction to a fixed-point form.  Using 14 bit integer portion
    // will allow us to compute four decimal digits at a time.
    static const int integerBits = 14;
    static const int fractionBits = 128 - integerBits;

    // We scale the interval in one of two different ways,
    // depending on whether the significand is even or odd...

    swift_uint128_t u, l;
    if (significandBitPattern & 1) {
        // Case A: Narrow the interval (odd significand)

        // Loitsch' original Grisu2 always narrows the interval.
        // Since our digit generation will select a value within
        // the scaled interval, narrowing the interval guarantees
        // that we will find a digit sequence that converts back
        // to the original value.

        // This ensures accuracy but, as explained in Loitsch' paper,
        // this carries a risk that there will be a shorter digit
        // sequence outside of our narrowed interval that we will
        // miss.  This risk obviously gets lower with increased
        // precision, but it wasn't until the Errol paper that anyone
        // had a good way to test whether a particular implementation
        // had sufficient precision.  That paper shows a way to enumerate
        // the worst-case numbers; those numbers that are extremely close
        // to the mid-points between adjacent floating-point values.
        // These are the values that might sit just outside of the
        // narrowed interval.  By testing these values, we can verify
        // the correctness of our implementation.

        // Multiply out the upper midpoint, rounding down...
        swift_uint128_t u1 = multiply128x64RoundingDown(powerOfTenRoundedDown,
                                                    upperMidpointExact);
        // Account for residual binary exponent and adjust
        // to the fixed-point format
        u = shiftRightRoundingDown128(u1, integerBits - extraBits);

        // Conversely for the lower midpoint...
        swift_uint128_t l1 = multiply128x64RoundingUp(powerOfTenRoundedUp,
                                                  lowerMidpointExact);
        l = shiftRightRoundingUp128(l1, integerBits - extraBits);

    } else {
        // Case B: Widen the interval (even significand)

        // As explained in Errol Theorem 6, in certain cases there is
        // a short decimal representation at the exact boundary of the
        // scaled interval.  When such a number is converted back to
        // binary, it will get rounded to the adjacent even
        // significand.

        // So when the significand is even, we widen the interval in
        // order to ensure that the exact midpoints are considered.
        // Of couse, this ensures that we find a short result but
        // carries a risk of selecting a result outside of the exact
        // scaled interval (which would be inaccurate).

        // The same testing approach described above also applies
        // to this case.

        swift_uint128_t u1 = multiply128x64RoundingUp(powerOfTenRoundedUp,
                                                  upperMidpointExact);
        u = shiftRightRoundingUp128(u1, integerBits - extraBits);

        swift_uint128_t l1 = multiply128x64RoundingDown(powerOfTenRoundedDown,
                                                    lowerMidpointExact);
        l = shiftRightRoundingDown128(l1, integerBits - extraBits);
    }

    // Step 6: Align first digit, adjust exponent

    // This preps for digit generation.  It just multiplies repeatedly
    // by 10 until we have exactly one decimal digit in the integer
    // part, adjusting the exponent as we go.

    // In particular, this prunes leading zeros from subnormals.
    // Generate digits for `t` with interval width `delta`
    swift_uint128_t t = u;
    swift_uint128_t delta = u;
    subtract128x128(&delta, l); // Explained below.
    int exponent = base10Exponent + 1;

    // Except for subnormals, this loop should never run more than once.
#if HAVE_UINT128_T
    static const swift_uint128_t fixedPointOne = (__uint128_t)1 << fractionBits;
    while (t < fixedPointOne)
#else
    // Because 1.0 in fixed point has a lot of zeros, it suffices
    // to only compare the high-order word here.  This is a minor
    // performance win.
    while (t.high < ((uint32_t)1 << (fractionBits % 32)))
#endif
    {
        exponent -= 1;
        multiply128xi32(&delta, 10);
        multiply128xi32(&t, 10);
    }

    // Step 7: Generate digits

    // This is a common part of Grisu-style algorithms.  The
    // underlying idea is to generate digits for the scaled upper and
    // lower boundaries, and stop when we hit the first different
    // digit (at which point, the digit for the upper midpoint is the
    // candidate final digit).  To understand this, just note that
    // 0.1234 is the shortest decimal between u = 0.123456 and l =
    // 0.123345.

    // Grisu uses a slightly optimized technique: it generates digits
    // for the upper bound (multiplying by 10 to isolate each digit)
    // and multiplies the interval width `delta` at the same time.
    // The `different digit` criteria above translates to a test for
    // `delta` being larger than the remainder.

    int8_t *digit_p = digits;

    // Adjustment above already set up the first digit:
    int nextDigit = extractIntegerPart128(&t, fractionBits);
    clearIntegerPart128(&t, fractionBits);

    // Further optimization: Generating four digits at a time reduces
    // the total arithmetic required per digit.  Note: The following
    // block can be entirely removed with no effect on the result.
    // If you're trying to understand this algorithm, just skip this
    // block on first reading.
    swift_uint128_t d0 = delta;
    multiply128xi32(&d0, 10000);
    swift_uint128_t t0 = t;
    multiply128xi32(&t0, 10000);
    int fourDigits = extractIntegerPart128(&t0, fractionBits); // 4 digits
    clearIntegerPart128(&t0, fractionBits);
    while (isLessThan128x128(d0, t0)) {
        *digit_p++ = nextDigit;
        int d = fourDigits / 100; // top 2 digits
        *digit_p++ = d / 10;
        *digit_p++ = d % 10;
        d = fourDigits % 100; // bottom 2 digits
        *digit_p++ = d / 10;
        nextDigit = d % 10;
        t = t0;
        delta = d0;
        multiply128xi32(&d0, 10000);
        multiply128xi32(&t0, 10000);
        fourDigits = extractIntegerPart128(&t0, fractionBits);
        clearIntegerPart128(&t0, fractionBits);
    }

    // Finish by generating one digit at a time.
    while (isLessThan128x128(delta, t)) {
        *digit_p++ = nextDigit;
        multiply128xi32(&delta, 10);
        multiply128xi32(&t, 10);
        nextDigit = extractIntegerPart128(&t, fractionBits);
        clearIntegerPart128(&t, fractionBits);
    }

    // Adjust the final digit to be closer to the original value.  It accounts
    // for the fact that sometimes there is more than one shortest digit
    // sequence.

    // For example, consider how the above would work if you had the
    // value 0.1234 and computed u = 0.1257, l = 0.1211.  The above
    // digit generation works with `u`, so produces 0.125.  But the
    // values 0.122, 0.123, and 0.124 are just as short and 0.123 is
    // the best choice, since it's closest to the original value.

    // If `delta <= t + 1.0`, then the interval is narrower than
    // one decimal digit, so there is no other option.

    // Note: We've already consumed most of our available precision,
    // so it's okay to just work in 64 bits here...
    uint64_t deltaHigh64 = extractHigh64From128(delta);
    uint64_t tHigh64 = extractHigh64From128(t);
    if (deltaHigh64 > tHigh64 + ((uint64_t)1 << (fractionBits % 64))) {
        // Note: 64-bit arithmetic is okay here
        uint64_t skew;
        if (isBoundary) {
            // If we're at the boundary where the exponent shifts,
            // then the original value is 1/3 of the way from
            // the bottom of the interval ...
            skew = deltaHigh64 - deltaHigh64 / 3 - tHigh64;
        } else {
            // ... otherwise it's exactly in the middle.
            skew = deltaHigh64 / 2 - tHigh64;
        }

        // The `skew` above is the difference between our
        // computed digits and the original exact value.
        // Use that to offset the final digit:
        uint64_t one = (uint64_t)(1) << (64 - integerBits);
        uint64_t fractionMask = one - 1;
        uint64_t oneHalf = one >> 1;
        if ((skew & fractionMask) == oneHalf) {
            int adjust = (int)(skew >> (64 - integerBits));
            // If the skew is exactly integer + 1/2, round the
            // last digit even after adjustment
            nextDigit = (nextDigit - adjust) & ~1;
        } else {
            // Else round to nearest...
            int adjust = (int)((skew + oneHalf) >> (64 - integerBits));
            nextDigit = (nextDigit - adjust);
        }
    }
    *digit_p++ = nextDigit; // Store the final digit.

    *decimalExponent = exponent;
    return digit_p - digits;
}
#endif

#if SWIFT_DTOA_FLOAT_SUPPORT
// Return raw bits encoding the float
static uint64_t bitPatternForFloat(float f) {
    union { float f; uint32_t u; } converter;
    converter.f = f;
    return converter.u;
}

// Decompose an IEEE 754 binary32 single-precision float
// into decimal digits and a corresponding decimal exponent.

// See swift_decompose_double for detailed comments on the algorithm here
int swift_decompose_float(float f,
    int8_t *digits, size_t digits_length, int *decimalExponent)
{
    static const int significandBitCount = FLT_MANT_DIG - 1;
    static const uint32_t significandMask
        = ((uint32_t)1 << significandBitCount) - 1;
    static const int exponentBitCount = 8;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_decompose_double
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 125

    // Step 0: Deconstruct the target number
    // Note: this strongly assumes IEEE 754 binary32 format
    uint32_t raw = bitPatternForFloat(f);
    int exponentBitPattern = (raw >> significandBitCount) & exponentMask;
    uint32_t significandBitPattern = raw & significandMask;

    // Step 1: Handle the various input cases:
    int binaryExponent;
    uint32_t significand;
    if (digits_length < 9) {
        // Ensure we have space for 9 digits
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
        // Return no digits
        return 0;
    } else if (exponentBitPattern == 0) {
        if (significandBitPattern == 0) { // Zero
            // Return one zero digit and decimalExponent = 0.
            digits[0] = 0;
            *decimalExponent = 0;
            return 1;
        } else { // Subnormal
            binaryExponent = 1 - exponentBias;
            significand = significandBitPattern << (32 - significandBitCount - 1);
        }
    } else { // normal
        binaryExponent = exponentBitPattern - exponentBias;
        uint32_t hiddenBit = (uint32_t)1 << (uint32_t)significandBitCount;
        uint32_t fullSignificand = significandBitPattern + hiddenBit;
        significand = fullSignificand << (32 - significandBitCount - 1);
    }

    // Step 2: Determine the exact unscaled target interval
    static const uint32_t halfUlp = (uint32_t)1 << (32 - significandBitCount - 2);
    uint32_t upperMidpointExact = significand + halfUlp;

    int isBoundary = significandBitPattern == 0;
    static const uint32_t quarterUlp = halfUlp >> 1;
    uint32_t lowerMidpointExact
        = significand - (isBoundary ? quarterUlp : halfUlp);

    // Step 3: Estimate the base 10 exponent
    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor
    uint64_t powerOfTenRoundedDown = 0;
    uint64_t powerOfTenRoundedUp = 0;
    int powerOfTenExponent = 0;
    intervalContainingPowerOf10_Float(-base10Exponent,
                                      &powerOfTenRoundedDown,
                                      &powerOfTenRoundedUp,
                                      &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)
    static const int integerBits = 5;
    const int shift = integerBits - extraBits;
    const int roundUpBias = (1 << shift) - 1;
    static const int fractionBits = 64 - integerBits;
    uint64_t u, l;
    if (significandBitPattern & 1) {
        // Narrow the interval (odd significand)
        uint64_t u1 = multiply64x32RoundingDown(powerOfTenRoundedDown,
                                                upperMidpointExact);
        u = u1 >> shift; // Rounding down

        uint64_t l1 = multiply64x32RoundingUp(powerOfTenRoundedUp,
                                              lowerMidpointExact);
        l = (l1 + roundUpBias) >> shift; // Rounding Up
    } else {
        // Widen the interval (even significand)
        uint64_t u1 = multiply64x32RoundingUp(powerOfTenRoundedUp,
                                              upperMidpointExact);
        u = (u1 + roundUpBias) >> shift; // Rounding Up

        uint64_t l1 = multiply64x32RoundingDown(powerOfTenRoundedDown,
                                                lowerMidpointExact);
        l = l1 >> shift; // Rounding down
    }

    // Step 6: Align first digit, adjust exponent
    // In particular, this prunes leading zeros from subnormals
    static const uint64_t fixedPointOne = (uint64_t)1 << fractionBits;
    static const uint64_t fixedPointMask = fixedPointOne - 1;
    uint64_t t = u;
    uint64_t delta = u - l;
    int exponent = base10Exponent + 1;

    while (t < fixedPointOne) {
        exponent -= 1;
        delta *= 10;
        t *= 10;
    }

    // Step 7: Generate digits
    int8_t *digit_p = digits;
    int nextDigit = (int)(t >> fractionBits);
    t &= fixedPointMask;

    // Generate one digit at a time...
    while (t > delta) {
        *digit_p++ = nextDigit;
        delta *= 10;
        t *= 10;
        nextDigit = (int)(t >> fractionBits);
        t &= fixedPointMask;
    }

    // Adjust the final digit to be closer to the original value
    if (delta > t + fixedPointOne) {
        uint64_t skew;
        if (isBoundary) {
            skew = delta - delta / 3 - t;
        } else {
            skew = delta / 2 - t;
        }
        uint64_t one = (uint64_t)(1) << (64 - integerBits);
        uint64_t lastAccurateBit = 1ULL << 24;
        uint64_t fractionMask = (one - 1) & ~(lastAccurateBit - 1);
        uint64_t oneHalf = one >> 1;
        if (((skew + (lastAccurateBit >> 1)) & fractionMask) == oneHalf) {
            // If the skew is exactly integer + 1/2, round the last
            // digit even after adjustment
            int adjust = (int)(skew >> (64 - integerBits));
            nextDigit = (nextDigit - adjust) & ~1;
        } else {
            // Else round to nearest...
            int adjust = (int)((skew + oneHalf) >> (64 - integerBits));
            nextDigit = (nextDigit - adjust);
        }
    }
    *digit_p++ = nextDigit;

    *decimalExponent = exponent;
    return digit_p - digits;
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
// See `swift_decompose_double` for detailed comments on this implementatoin.
int swift_decompose_float80(long double d,
    int8_t *digits, size_t digits_length, int *decimalExponent)
{
    static const int exponentBitCount = 15;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_decompose_double to understand
    // why we use 16,382 instead of 16,383 here.
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 16,382

    // Step 0: Deconstruct the target number
    // Note: this strongly assumes Intel 80-bit extended format in LSB
    // byte order
    const uint64_t *raw_p = (const uint64_t *)&d;
    int exponentBitPattern = raw_p[1] & exponentMask;
    uint64_t significandBitPattern = raw_p[0];

    // Step 1: Handle the various input cases:
    int64_t binaryExponent;
    uint64_t significand;
    if (digits_length < 21) {
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
        // Return no digits
        return 0;
    } else if (exponentBitPattern == 0) {
        if (significandBitPattern == 0) { // Zero
            digits[0] = 0;
            *decimalExponent = 0;
            return 1;
        } else { // subnormal
            binaryExponent = 1 - exponentBias;
            significand = significandBitPattern;
        }
    } else if (significandBitPattern >> 63) { // Normal
        binaryExponent = exponentBitPattern - exponentBias;
        significand = significandBitPattern;
    } else {
        // "Unnormal" values are rejected as invalid by 80387 and later.
        // Treat them the same as NaNs here.
        return 0;
    }

    // Step 2: Determine the exact unscaled target interval
    uint64_t halfUlp = (uint64_t)1 << 63;
    uint64_t quarterUlp = halfUlp >> 1;
    uint64_t threeQuarterUlp = halfUlp + quarterUlp;
    swift_uint128_t upperMidpointExact, lowerMidpointExact;
    initialize128WithHighLow64(upperMidpointExact, significand, halfUlp);
    int isBoundary = (significandBitPattern & 0x7fffffffffffffff) == 0;
    // Subtract 1/4 or 1/2 ULP by first subtracting 1 full ULP, then adding some back
    initialize128WithHighLow64(lowerMidpointExact, significand - 1, isBoundary ? threeQuarterUlp : halfUlp);

    // Step 3: Estimate the base 10 exponent
    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor
    swift_uint192_t powerOfTenRoundedDown;
    swift_uint192_t powerOfTenRoundedUp;
    int powerOfTenExponent = 0;
    intervalContainingPowerOf10_Float80(-base10Exponent,
                                        &powerOfTenRoundedDown,
                                        &powerOfTenRoundedUp,
                                        &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)
    static const int integerBits = 14;
    static const int fractionBits = 192 - integerBits;
#if HAVE_UINT128_T
    static const int highFractionBits = fractionBits % 64;
#else
    static const int highFractionBits = fractionBits % 32;
#endif
    swift_uint192_t u, l;
    if (significandBitPattern & 1) {
        // Narrow the interval (odd significand)
        u = powerOfTenRoundedDown;
        multiply192x128RoundingDown(&u, upperMidpointExact);
        shiftRightRoundingDown192(&u, integerBits - extraBits);

        l = powerOfTenRoundedUp;
        multiply192x128RoundingUp(&l, lowerMidpointExact);
        shiftRightRoundingUp192(&l, integerBits - extraBits);
    } else {
        // Widen the interval (even significand)
        u = powerOfTenRoundedUp;
        multiply192x128RoundingUp(&u, upperMidpointExact);
        shiftRightRoundingUp192(&u, integerBits - extraBits);

        l = powerOfTenRoundedDown;
        multiply192x128RoundingDown(&l, lowerMidpointExact);
        shiftRightRoundingDown192(&l, integerBits - extraBits);
    }

    // Step 6: Align first digit, adjust exponent
    // In particular, this prunes leading zeros from subnormals
    static const uint64_t fixedPointOneHigh = (uint64_t)1 << highFractionBits;
    static const uint64_t fixedPointMaskHigh = fixedPointOneHigh - 1;
    swift_uint192_t t = u;
    swift_uint192_t delta = u;
    subtract192x192(&delta, l);
    int exponent = base10Exponent + 1;

    while (t.high < fixedPointOneHigh) {
        exponent -= 1;
        multiply192xi32(&delta, 10);
        multiply192xi32(&t, 10);
    }

    // Step 7: Generate digits
    int8_t *digit_p = digits;

    // Adjustment above already set up the first digit:
    int nextDigit = (int)(t.high >> highFractionBits);
    t.high &= fixedPointMaskHigh;

    // Generate four digits at a time ...
    swift_uint192_t d0 = delta;
    swift_uint192_t t0 = t;
    multiply192xi32(&d0, 10000);
    multiply192xi32(&t0, 10000);
    int fourDigits = (int)(t0.high >> highFractionBits);
    t0.high &= fixedPointMaskHigh;
    while (isLessThan192x192(d0, t0)) {
        *digit_p++ = nextDigit;
        int d = fourDigits / 100;
        *digit_p++ = d / 10;
        *digit_p++ = d % 10;
        d = fourDigits % 100;
        *digit_p++ = d / 10;
        nextDigit = d % 10;
        t = t0;
        delta = d0;
        multiply192xi32(&d0, 10000);
        multiply192xi32(&t0, 10000);
        fourDigits = (int)(t0.high >> highFractionBits);
        t0.high &= fixedPointMaskHigh;
    }

    // Generate one digit at a time...
    while (isLessThan192x192(delta, t)) {
        *digit_p++ = nextDigit;
        multiply192xi32(&delta, 10);
        multiply192xi32(&t, 10);
        nextDigit = (int)(t.high >> highFractionBits);
        t.high &= fixedPointMaskHigh;
    }

    // Adjust the final digit to be closer to the original value
    // We've already consumed most of our available precision, so it's
    // okay to just work in 64 bits here...
#if HAVE_UINT128_T
    uint64_t deltaHigh64 = delta.high;
    uint64_t tHigh64 = t.high;
#else
    uint64_t deltaHigh64 = ((uint64_t)delta.high << 32) + delta.e;
    uint64_t tHigh64 = ((uint64_t)t.high << 32) + t.e;
#endif
    if (deltaHigh64 > tHigh64 + ((uint64_t)1 << (fractionBits % 64))) {
        uint64_t skew;
        if (isBoundary) {
            skew = deltaHigh64 - deltaHigh64 / 3 - tHigh64;
        } else {
            skew = deltaHigh64 / 2 - tHigh64;
        }
        uint64_t one = (uint64_t)(1) << (64 - integerBits);
        uint64_t fractionMask = one - 1;
        uint64_t oneHalf = one >> 1;
        if ((skew & fractionMask) == oneHalf) {
            int adjust = (int)(skew >> (64 - integerBits));
            // If the skew is integer + 1/2, round the last digit even
            // after adjustment
            nextDigit = (nextDigit - adjust) & ~1;
        } else {
            // Else round to nearest...
            int adjust = (int)((skew + oneHalf) >> (64 - integerBits));
            nextDigit = (nextDigit - adjust);
        }
    }
    *digit_p++ = nextDigit;

    *decimalExponent = exponent;
    return digit_p - digits;
}
#endif

//
// ---------------- High-level API -----------------
//
// Functions that format a Float/Double/Float80
// directly into a buffer.
//
// These handle various exception cases (infinity, Nan, zero)
// before invoking the general base-10 conversion.

#if SWIFT_DTOA_FLOAT_SUPPORT || SWIFT_DTOA_DOUBLE_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT
static size_t swift_format_constant(char *dest, size_t length, const char *s) {
    const size_t l = strlen(s);
    if (length <= l) {
        return 0;
    }
    strcpy(dest, s);
    return l;
}
#endif

#if SWIFT_DTOA_FLOAT_SUPPORT
size_t swift_format_float(float d, char *dest, size_t length)
{
    if (!isfinite(d)) {
        if (isinf(d)) {
            // Infinity
            if (signbit(d)) {
                return swift_format_constant(dest, length, "-inf");
            } else {
                return swift_format_constant(dest, length, "inf");
            }
        } else {
            // NaN
            static const int significandBitCount = 23;
            uint32_t raw = bitPatternForFloat(d);
            const char *sign = signbit(d) ? "-" : "";
            const char *signaling = ((raw >> (significandBitCount - 1)) & 1) ? "" : "s";
            uint32_t payload = raw & ((1L << (significandBitCount - 2)) - 1);
            char buff[32];
            if (payload != 0) {
                snprintf(buff, sizeof(buff), "%s%snan(0x%x)",
                         sign, signaling, payload);
            } else {
                snprintf(buff, sizeof(buff), "%s%snan",
                         sign, signaling);
            }
            return swift_format_constant(dest, length, buff);
        }
    }

    // zero
    if (d == 0.0) {
        if (signbit(d)) {
            return swift_format_constant(dest, length, "-0.0");
        } else {
            return swift_format_constant(dest, length, "0.0");
        }
    }

    // Decimal numeric formatting
    int decimalExponent;
    int8_t digits[9];
    int digitCount =
        swift_decompose_float(d, digits, sizeof(digits), &decimalExponent);
    // People use float to model integers <= 2^24, so we use that
    // as a cutoff for decimal vs. exponential format.
    if (decimalExponent < -3 || fabsf(d) > 0x1.0p24F) {
        return swift_format_exponential(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    } else {
        return swift_format_decimal(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    }
}
#endif

#if SWIFT_DTOA_DOUBLE_SUPPORT
size_t swift_format_double(double d, char *dest, size_t length)
{
    if (!isfinite(d)) {
        if (isinf(d)) {
            // Infinity
            if (signbit(d)) {
                return swift_format_constant(dest, length, "-inf");
            } else {
                return swift_format_constant(dest, length, "inf");
            }
        } else {
            // NaN
            static const int significandBitCount = 52;
            uint64_t raw = bitPatternForDouble(d);
            const char *sign = signbit(d) ? "-" : "";
            const char *signaling = ((raw >> (significandBitCount - 1)) & 1) ? "" : "s";
            uint64_t payload = raw & ((1ull << (significandBitCount - 2)) - 1);
            char buff[32];
            if (payload != 0) {
                snprintf(buff, sizeof(buff), "%s%snan(0x%" PRIx64 ")",
                         sign, signaling, payload);
            } else {
                snprintf(buff, sizeof(buff), "%s%snan",
                         sign, signaling);
            }
            return swift_format_constant(dest, length, buff);
        }
    }

    // zero
    if (d == 0.0) {
        if (signbit(d)) {
            return swift_format_constant(dest, length, "-0.0");
        } else {
            return swift_format_constant(dest, length, "0.0");
        }
    }

    // Decimal numeric formatting
    int decimalExponent;
    int8_t digits[17];
    int digitCount =
        swift_decompose_double(d, digits, sizeof(digits), &decimalExponent);
    // People use double to model integers <= 2^53, so we use that
    // as a cutoff for decimal vs. exponential format.
    if (decimalExponent < -3 || fabs(d) > 0x1.0p53) {
        return swift_format_exponential(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    } else {
        return swift_format_decimal(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    }
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
size_t swift_format_float80(long double d, char *dest, size_t length)
{
    if (!isfinite(d)) {
        if (isinf(d)) {
            // Infinity
            if (signbit(d)) {
                return swift_format_constant(dest, length, "-inf");
            } else {
                return swift_format_constant(dest, length, "inf");
            }
        } else {
            // NaN
            // Assumes Intel 80-bit extended format in LSB byte order:
            uint64_t significandBitPattern = *(const uint64_t *)&d;
            const char *sign = signbit(d) ? "-" : "";
            const char *signaling = ((significandBitPattern >> 62) & 1) ? "" : "s";
            uint64_t payload = significandBitPattern & (((uint64_t)1 << 61) - 1);
            char buff[32];
            if (payload != 0) {
                snprintf(buff, sizeof(buff), "%s%snan(0x%" PRIx64 ")",
                         sign, signaling, payload);
            } else {
                snprintf(buff, sizeof(buff), "%s%snan",
                         sign, signaling);
            }
            return swift_format_constant(dest, length, buff);
        }
    }

    // zero
    if (d == 0.0) {
        if (signbit(d)) {
            return swift_format_constant(dest, length, "-0.0");
        } else {
            return swift_format_constant(dest, length, "0.0");
        }
    }

    // Decimal numeric formatting
    int decimalExponent;
    int8_t digits[21];
    int digitCount =
        swift_decompose_float80(d, digits, sizeof(digits), &decimalExponent);
    // People use long double to model integers <= 2^64, so we use that
    // as a cutoff for decimal vs. exponential format.
    // The constant is written out as a float80 (aka "long double") literal
    // here since it can't be expressed as a 64-bit integer.
    if (decimalExponent < -3 || fabsl(d) > 0x1.0p64L) {
        return swift_format_exponential(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    } else {
        return swift_format_decimal(dest, length, signbit(d),
                 digits, digitCount, decimalExponent);
    }
}
#endif

/**
 * Routines to format a decomposed value into a standard string form.
 */

// Format into exponential format: "1.234e+56"
// Returns number of characters actually written to `dest`.
// Returns zero if buffer is too small.
size_t swift_format_exponential(char *dest, size_t length,
    bool negative, const int8_t *digits, int digit_count, int exponent)
{
    // Largest buffer we could possibly need:
    size_t maximum_size = digit_count + 9;
    if (length < maximum_size) {
        // We only do the detailed check if the size is borderline.
        size_t actual_size =
            + (negative ? 1 : 0) // Leading minus
            + digit_count // digits
            + (digit_count > 1 ? 1 : 0) // decimal
            + 1 // 'e'
            + 1 // sign
            + (exponent > 99 ? (exponent > 999 ? 4 : 3) : 2) // exponent
            + 1; // trailing zero byte
        if (length < actual_size) {
            if (length > 0) {
                dest[0] = 0;
            }
            return 0;
        }
    }
    char *p = dest;
    if (negative) {
        *p++ = '-';
    }

    *p++ = digits[0] + '0';
    exponent -= 1;
    if (digit_count > 1) {
        *p++ = '.';
        for (int i = 1; i < digit_count; i++) {
            *p++ = digits[i] + '0';
        }
    }
    *p++ = 'e';
    if (exponent < 0) {
        *p++ = '-';
        exponent = -exponent;
    } else {
        *p++ = '+';
    }
    if (exponent > 99) {
        if (exponent > 999) {
            *p++ = (exponent / 1000 % 10) + '0';
        }
        *p++ = (exponent / 100 % 10) + '0';
        exponent %= 100;
    }
    *p++ = (exponent / 10) + '0';
    *p++ = (exponent % 10) + '0';
    *p = '\0';
    return p - dest;
}

// Format into decimal form: "123456789000.0", "1234.5678", "0.0000001234"
// Returns number of bytes of `dest` actually used, or zero if
// provided buffer is too small.
size_t swift_format_decimal(char *dest, size_t length,
    bool negative, const int8_t *digits, int digit_count, int exponent)
{
    // Largest buffer we could possibly need:
    size_t maximum_size =
        digit_count // All the digits
        + (exponent > 0 ? exponent : -exponent) // Max # of extra zeros
        + 4; // Max # of other items
    if (length < maximum_size) {
        // We only do the detailed check if the size is borderline.
        if (exponent <= 0) { // "0.0000001234"
            size_t actual_size =
                (negative ? 1 : 0) // Leading minus
                + 2 // Leading "0."
                + (-exponent) // Leading zeros after decimal point
                + digit_count
                + 1; // Trailing zero byte
            if (length < actual_size) {
                if (length > 0) {
                    dest[0] = 0;
                }
                return 0;
            }
        } else if (exponent < digit_count) { // "123.45"
            size_t actual_size =
                (negative ? 1 : 0) // Leading minus
                + digit_count
                + 1 // embedded decimal point
                + 1; // Trailing zero byte
            if (length < actual_size) {
                if (length > 0) {
                    dest[0] = 0;
                }
                return 0;
            }
        } else { // "12345000.0"
            size_t actual_size =
                (negative ? 1 : 0) // Leading minus
                + digit_count
                + (exponent - digit_count) // trailing zeros
                + 2 // ".0" to mark this as floating point
                + 1; // Trailing zero byte
            if (length < actual_size) {
                if (length > 0) {
                    dest[0] = 0;
                }
                return 0;
            }
        }
    }

    char *p = dest;
    if (negative) {
        *p++ = '-';
    }

    if (exponent <= 0) {
        *p++ = '0';
        *p++ = '.';
        while (exponent < 0) {
            *p++ = '0';
            exponent += 1;
        }
        for (int i = 0; i < digit_count; ++i) {
            *p++ = digits[i] + '0';
        }
    } else if (exponent < digit_count) {
        for (int i = 0; i < digit_count; i++) {
            if (exponent == 0) {
                *p++ = '.';
            }
            *p++ = digits[i] + '0';
            exponent -= 1;
        }
    } else {
        for (int i = 0; i < digit_count; i++) {
            *p++ = digits[i] + '0';
            exponent -= 1;
        }
        while (exponent > 0) {
            *p++ = '0';
            exponent -= 1;
        }
        *p++ = '.';
        *p++ = '0';
    }
    *p = '\0';
    return p - dest;
}

//
// ------------  Arithmetic helpers ----------------
//

// The core algorithm relies heavily on fraction and fixed-point
// arithmetic with 64-bit, 128-bit, and 192-bit integer values. (For
// float, double, and float80, respectively.) They also need precise
// control over all rounding.
//
// Note that most arithmetic operations are the same for integers and
// fractions, so we can just use the normal integer operations in most
// places.  Multiplication however, is different for fixed-size
// fractions.  Integer multiplication preserves the low-order part and
// discards the high-order part (ignoring overflow).  Fraction
// multiplication preserves the high-order part and discards the
// low-order part (rounding).  So most of the arithmetic helpers here
// are for multiplication.

// Note: With 64-bit GCC and Clang, we get a noticable performance
// gain by using `__uint128_t`.  Otherwise, we have to break things
// down into 32-bit chunks so we don't overflow 64-bit temporaries.

#if SWIFT_DTOA_FLOAT_SUPPORT
// Multiply a 64-bit fraction by a 32-bit fraction, rounding down.
static uint64_t multiply64x32RoundingDown(uint64_t lhs, uint32_t rhs) {
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = ((lhs & mask32) * rhs) >> 32;
    return t + (lhs >> 32) * rhs;
}

// Multiply a 64-bit fraction by a 32-bit fraction, rounding up.
static uint64_t multiply64x32RoundingUp(uint64_t lhs, uint32_t rhs) {
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = (((lhs & mask32) * rhs) + mask32) >> 32;
    return t + (lhs >> 32) * rhs;
}

// Multiply a 64-bit fraction by a 64-bit fraction, rounding down.
static uint64_t multiply64x64RoundingDown(uint64_t lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    __uint128_t full = (__uint128_t)lhs * rhs;
    return (uint64_t)(full >> 64);
#else
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = (lhs & mask32) * (rhs & mask32);
    t >>= 32;
    uint64_t a = (lhs >> 32) * (rhs & mask32);
    uint64_t b = (lhs & mask32) * (rhs >> 32);
    // Useful: If w,x,y,z are all 32-bit values, then:
    // w * x + y + z
    //   <= (2^64 - 2^33 + 1) + (2^32 - 1) + (2^32 - 1)
    //   <= 2^64 - 1
    //
    // That is, a product of two 32-bit values plus two more 32-bit
    // values can't overflow 64 bits.  (But "three more" can, so be
    // careful!)
    //
    // Here: t + a + (b & mask32) <= 2^64 - 1
    t += a + (b & mask32);
    t >>= 32;
    t += (b >> 32);
    return t + (lhs >> 32) * (rhs >> 32);
#endif
}

// Multiply a 64-bit fraction by a 64-bit fraction, rounding up.
static uint64_t multiply64x64RoundingUp(uint64_t lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    static const __uint128_t roundingUpBias = ((__uint128_t)1 << 64) - 1;
    __uint128_t full = (__uint128_t)lhs * rhs;
    return (uint64_t)((full + roundingUpBias) >> 64);
#else
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = (lhs & mask32) * (rhs & mask32);
    t = (t + mask32) >> 32;
    uint64_t a = (lhs >> 32) * (rhs & mask32);
    uint64_t b = (lhs & mask32) * (rhs >> 32);
    t += (a & mask32) + (b & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    return t + (lhs >> 32) * (rhs >> 32);
#endif
}

#endif

#if SWIFT_DTOA_DOUBLE_SUPPORT
// Multiply a 128-bit fraction by a 64-bit fraction, rounding down.
static swift_uint128_t multiply128x64RoundingDown(swift_uint128_t lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    uint64_t lhsl = (uint64_t)lhs;
    uint64_t lhsh = (uint64_t)(lhs >> 64);
    swift_uint128_t h = (swift_uint128_t)lhsh * rhs;
    swift_uint128_t l = (swift_uint128_t)lhsl * rhs;
    return h + (l >> 64);
#else
    swift_uint128_t result;
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t rhs0 = rhs & mask32;
    uint64_t rhs1 = rhs >> 32;
    uint64_t t = (lhs.low) * rhs0;
    t >>= 32;
    uint64_t a = (lhs.b) * rhs0;
    uint64_t b = (lhs.low) * rhs1;
    t += a + (b & mask32);
    t >>= 32;
    t += (b >> 32);
    a = lhs.c * rhs0;
    b = lhs.b * rhs1;
    t += (a & mask32) + (b & mask32);
    result.low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs.high * rhs0;
    b = lhs.c * rhs1;
    t += (a & mask32) + (b & mask32);
    result.b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += lhs.high * rhs1;
    result.c = t;
    result.high = t >> 32;
    return result;
#endif
}

// Multiply a 128-bit fraction by a 64-bit fraction, rounding up.
static swift_uint128_t multiply128x64RoundingUp(swift_uint128_t lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    uint64_t lhsl = (uint64_t)lhs;
    uint64_t lhsh = (uint64_t)(lhs >> 64);
    swift_uint128_t h = (swift_uint128_t)lhsh * rhs;
    swift_uint128_t l = (swift_uint128_t)lhsl * rhs;
    const static __uint128_t bias = ((__uint128_t)1 << 64) - 1;
    return h + ((l + bias) >> 64);
#else
    swift_uint128_t result;
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t rhs0 = rhs & mask32;
    uint64_t rhs1 = rhs >> 32;
    uint64_t t = (lhs.low) * rhs0 + mask32;
    t >>= 32;
    uint64_t a = (lhs.b) * rhs0;
    uint64_t b = (lhs.low) * rhs1;
    t += (a & mask32) + (b & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs.c * rhs0;
    b = lhs.b * rhs1;
    t += (a & mask32) + (b & mask32);
    result.low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs.high * rhs0;
    b = lhs.c * rhs1;
    t += (a & mask32) + (b & mask32);
    result.b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += lhs.high * rhs1;
    result.c = t;
    result.high = t >> 32;
    return result;
#endif
}

#if !HAVE_UINT128_T
// Multiply a 128-bit fraction by a 32-bit integer in a 32-bit environment.
// (On 64-bit, we use a fast inline macro.)
static void multiply128xi32(swift_uint128_t *lhs, uint32_t rhs) {
    uint64_t t = (uint64_t)(lhs->low) * rhs;
    lhs->low = (uint32_t)t;
    t = (t >> 32) + (uint64_t)(lhs->b) * rhs;
    lhs->b = (uint32_t)t;
    t = (t >> 32) + (uint64_t)(lhs->c) * rhs;
    lhs->c = (uint32_t)t;
    t = (t >> 32) + (uint64_t)(lhs->high) * rhs;
    lhs->high = (uint32_t)t;
}

// Compare two 128-bit integers in a 32-bit environment
// (On 64-bit, we use a fast inline macro.)
static int isLessThan128x128(swift_uint128_t lhs, swift_uint128_t rhs) {
    return ((lhs.high < rhs.high)
            || ((lhs.high == rhs.high)
                && ((lhs.c < rhs.c)
                    || ((lhs.c == rhs.c)
                        && ((lhs.b < rhs.b)
                            || ((lhs.b == rhs.b)
                                && (lhs.low < rhs.low)))))));
}

// Subtract 128-bit values in a 32-bit environment
static void subtract128x128(swift_uint128_t *lhs, swift_uint128_t rhs) {
    uint64_t t = (uint64_t)lhs->low + (~rhs.low) + 1;
    lhs->low = (uint32_t)t;
    t = (t >> 32) + lhs->b + (~rhs.b);
    lhs->b = (uint32_t)t;
    t = (t >> 32) + lhs->c + (~rhs.c);
    lhs->c = (uint32_t)t;
    t = (t >> 32) + lhs->high + (~rhs.high);
    lhs->high = (uint32_t)t;
}
#endif

// Shift a 128-bit integer right, rounding down.
static swift_uint128_t shiftRightRoundingDown128(swift_uint128_t lhs, int shift) {
#if HAVE_UINT128_T
    return lhs >> shift;
#else
    // Note: Shift is always less than 32
    swift_uint128_t result;
    uint64_t t = (uint64_t)lhs.low >> shift;
    t += ((uint64_t)lhs.b << (32 - shift));
    result.low = t;
    t >>= 32;
    t += ((uint64_t)lhs.c << (32 - shift));
    result.b = t;
    t >>= 32;
    t += ((uint64_t)lhs.high << (32 - shift));
    result.c = t;
    t >>= 32;
    result.high = t;
    return result;
#endif
}

// Shift a 128-bit integer right, rounding up.
static swift_uint128_t shiftRightRoundingUp128(swift_uint128_t lhs, int shift) {
#if HAVE_UINT128_T
    uint64_t bias = ((uint64_t)1 << shift) - 1;
    return ((lhs + bias) >> shift);
#else
    swift_uint128_t result;
    const uint64_t bias = (1 << shift) - 1;
    uint64_t t = ((uint64_t)lhs.low + bias) >> shift;
    t += ((uint64_t)lhs.b << (32 - shift));
    result.low = t;
    t >>= 32;
    t += ((uint64_t)lhs.c << (32 - shift));
    result.b = t;
    t >>= 32;
    t += ((uint64_t)lhs.high << (32 - shift));
    result.c = t;
    t >>= 32;
    result.high = t;
    return result;
#endif
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
// Multiply a 192-bit fraction by a 64-bit fraction, rounding down.
static void multiply192x64RoundingDown(swift_uint192_t *lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    // Compute the three 128-bit cross-products
    __uint128_t cd = (__uint128_t)lhs->low * rhs;
    __uint128_t bc = (__uint128_t)lhs->mid * rhs;
    __uint128_t ab = (__uint128_t)lhs->high * rhs;
    // Add up the three 64-bit outputs (including carries)
    __uint128_t c = (cd >> 64) + (uint64_t)bc;
    __uint128_t b = (bc >> 64) + (uint64_t)ab + (c >> 64);
    __uint128_t a = (ab >> 64) + (b >> 64);
    lhs->high = a;
    lhs->mid = b;
    lhs->low = c;
#else
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t rhs0 = rhs & mask32;
    uint64_t rhs1 = rhs >> 32;
    uint64_t t = lhs->low * rhs0;
    t >>= 32;
    uint64_t a = lhs->low * rhs1;
    uint64_t b = lhs->b * rhs0;
    t += a + (b & mask32);
    t >>= 32;
    t += (b >> 32);
    a = lhs->b * rhs1;
    b = lhs->c * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->c * rhs1;
    b = lhs->d * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->d * rhs1;
    b = lhs->e * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->c = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->e * rhs1;
    b = lhs->high * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->d = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += lhs->high * rhs1;
    lhs->e = t;
    lhs->high = t >> 32;
#endif
}

// Multiply a 192-bit fraction by a 64-bit fraction, rounding up.
static void multiply192x64RoundingUp(swift_uint192_t *lhs, uint64_t rhs) {
#if HAVE_UINT128_T
    // Compute the three 128-bit cross-products
    __uint128_t cd = (__uint128_t)lhs->low * rhs + UINT64_MAX;
    __uint128_t bc = (__uint128_t)lhs->mid * rhs;
    __uint128_t ab = (__uint128_t)lhs->high * rhs;
    // Add up the three 64-bit outputs (including carries)
    __uint128_t c = (cd >> 64) + (uint64_t)bc;
    __uint128_t b = (bc >> 64) + (uint64_t)ab + (c >> 64);
    __uint128_t a = (ab >> 64) + (b >> 64);
    lhs->high = a;
    lhs->mid = b;
    lhs->low = c;
#else
    static const uint64_t mask32 = UINT32_MAX;
    static const uint64_t bias = mask32;
    uint64_t rhs0 = rhs & mask32;
    uint64_t rhs1 = rhs >> 32;
    uint64_t t = lhs->low * rhs0 + bias;
    t >>= 32;
    uint64_t a = lhs->low * rhs1;
    uint64_t b = lhs->b * rhs0;
    t += (a & mask32) + (b & mask32) + bias;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->b * rhs1;
    b = lhs->c * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->c * rhs1;
    b = lhs->d * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->d * rhs1;
    b = lhs->e * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->c = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = lhs->e * rhs1;
    b = lhs->high * rhs0;
    t += (a & mask32) + (b & mask32);
    lhs->d = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += lhs->high * rhs1;
    lhs->e = t;
    lhs->high = t >> 32;
#endif
}

// Multiply a 192-bit fraction by a 64-bit integer.
// This is used in the digit generation to multiply by ten or
// 10,000. Note that rounding never appliles here.
// As used below, this will never overflow.
static void multiply192xi32(swift_uint192_t *lhs, uint32_t rhs) {
#if HAVE_UINT128_T
    __uint128_t t = (__uint128_t)lhs->low * rhs;
    lhs->low = (uint64_t)t;
    t = (t >> 64) + (__uint128_t)lhs->mid * rhs;
    lhs->mid = (uint64_t)t;
    t = (t >> 64) + (__uint128_t)lhs->high * rhs;
    lhs->high = (uint64_t)t;
#else
    uint64_t t = (uint64_t)lhs->low * rhs;
    lhs->low = t;
    t = (t >> 32) + (uint64_t)lhs->b * rhs;
    lhs->b = t;
    t = (t >> 32) + (uint64_t)lhs->c * rhs;
    lhs->c = t;
    t = (t >> 32) + (uint64_t)lhs->d * rhs;
    lhs->d = t;
    t = (t >> 32) + (uint64_t)lhs->e * rhs;
    lhs->e = t;
    t = (t >> 32) + (uint64_t)lhs->high * rhs;
    lhs->high = t;
#endif
}

// Multiply a 192-bit fraction by a 128-bit fraction, rounding down.
static void multiply192x128RoundingDown(swift_uint192_t *lhs, swift_uint128_t rhs) {
#if HAVE_UINT128_T
    // A full multiply of three 64-bit values by two 64-bit values
    // yields five such components.  We discard the bottom two (except
    // for carries) to get a rounded-down three-element result.
    __uint128_t current = (__uint128_t)lhs->low * (uint64_t)rhs;

    current = (current >> 64);
    __uint128_t t = (__uint128_t)lhs->low * (rhs >> 64);
    current += (uint64_t)t;
    __uint128_t next = t >> 64;
    t = (__uint128_t)lhs->mid * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;

    current = next + (current >> 64);
    t = (__uint128_t)lhs->mid * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (__uint128_t)lhs->high * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->low = (uint64_t)current;

    current = next + (current >> 64);
    t = (__uint128_t)lhs->high * (rhs >> 64);
    current += t;
    lhs->mid = (uint64_t)current;
    lhs->high = (uint64_t)(current >> 64);
#else
    uint64_t a, b, c, d; // temporaries
    // Six 32-bit values multiplied by 4 32-bit values.  Oh my.
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = lhs->low * rhs.low;
    t >>= 32;
    a = (uint64_t)lhs->low * rhs.b;
    b = (uint64_t)lhs->b * rhs.low;
    t += a + (b & mask32);
    t >>= 32;
    t += (b >> 32);
    a = (uint64_t)lhs->low * rhs.c;
    b = (uint64_t)lhs->b * rhs.b;
    c = (uint64_t)lhs->c * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);
    a = (uint64_t)lhs->low * rhs.high;
    b = (uint64_t)lhs->b * rhs.c;
    c = (uint64_t)lhs->c * rhs.b;
    d = (uint64_t)lhs->d * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->b * rhs.high;
    b = (uint64_t)lhs->c * rhs.c;
    c = (uint64_t)lhs->d * rhs.b;
    d = (uint64_t)lhs->e * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    lhs->low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->c * rhs.high;
    b = (uint64_t)lhs->d * rhs.c;
    c = (uint64_t)lhs->e * rhs.b;
    d = (uint64_t)lhs->high * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    lhs->b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->d * rhs.high;
    b = (uint64_t)lhs->e * rhs.c;
    c = (uint64_t)lhs->high * rhs.b;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    lhs->c = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);
    a = (uint64_t)lhs->e * rhs.high;
    b = (uint64_t)lhs->high * rhs.c;
    t += (a & mask32) + (b & mask32);
    lhs->d = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += (uint64_t)lhs->high * rhs.high;
    lhs->e = t;
    lhs->high = t >> 32;
#endif
}

// Multiply a 192-bit fraction by a 128-bit fraction, rounding up.
static void multiply192x128RoundingUp(swift_uint192_t *lhs, swift_uint128_t rhs) {
#if HAVE_UINT128_T
    // Same as the rounding-down version, but we add
    // UINT128_MAX to the bottom two to force an extra
    // carry if they are non-zero.
    swift_uint128_t current = (swift_uint128_t)lhs->low * (uint64_t)rhs;
    current += UINT64_MAX;

    current = (current >> 64);
    swift_uint128_t t = (swift_uint128_t)lhs->low * (rhs >> 64);
    current += (uint64_t)t;
    swift_uint128_t next = t >> 64;
    t = (swift_uint128_t)lhs->mid * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    // Round up by adding UINT128_MAX (upper half)
    current += UINT64_MAX;

    current = next + (current >> 64);
    t = (swift_uint128_t)lhs->mid * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (swift_uint128_t)lhs->high * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->low = (uint64_t)current;

    current = next + (current >> 64);
    t = (swift_uint128_t)lhs->high * (rhs >> 64);
    current += t;
    lhs->mid = (uint64_t)current;
    lhs->high = (uint64_t)(current >> 64);
#else
    uint64_t a, b, c, d; // temporaries
    // Six 32-bit values multiplied by 4 32-bit values.  Oh my.
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = (uint64_t)lhs->low * rhs.low + mask32;
    t >>= 32;
    a = (uint64_t)lhs->low * rhs.b;
    b = (uint64_t)lhs->b * rhs.low;
    t += (a & mask32) + (b & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    a = (uint64_t)lhs->low * rhs.c;
    b = (uint64_t)lhs->b * rhs.b;
    c = (uint64_t)lhs->c * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);
    a = (uint64_t)lhs->low * rhs.high;
    b = (uint64_t)lhs->b * rhs.c;
    c = (uint64_t)lhs->c * rhs.b;
    d = (uint64_t)lhs->d * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->b * rhs.high;
    b = (uint64_t)lhs->c * rhs.c;
    c = (uint64_t)lhs->d * rhs.b;
    d = (uint64_t)lhs->e * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    lhs->low = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->c * rhs.high;
    b = (uint64_t)lhs->d * rhs.c;
    c = (uint64_t)lhs->e * rhs.b;
    d = (uint64_t)lhs->high * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    lhs->b = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    a = (uint64_t)lhs->d * rhs.high;
    b = (uint64_t)lhs->e * rhs.c;
    c = (uint64_t)lhs->high * rhs.b;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    lhs->c = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);
    a = (uint64_t)lhs->e * rhs.high;
    b = (uint64_t)lhs->high * rhs.c;
    t += (a & mask32) + (b & mask32);
    lhs->d = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);
    t += (uint64_t)lhs->high * rhs.high;
    lhs->e = t;
    lhs->high = t >> 32;
#endif
}

// Subtract two 192-bit integers or fractions.
static void subtract192x192(swift_uint192_t *lhs, swift_uint192_t rhs) {
#if HAVE_UINT128_T
    swift_uint128_t t = (swift_uint128_t)lhs->low + (~rhs.low) + 1;
    lhs->low = t;
    t = (t >> 64) + lhs->mid + (~rhs.mid);
    lhs->mid = t;
    lhs->high += (t >> 64) + (~rhs.high);
#else
    uint64_t t = (uint64_t)lhs->low + (~rhs.low) + 1;
    lhs->low = t;
    t = (t >> 32) + lhs->b + (~rhs.b);
    lhs->b = t;
    t = (t >> 32) + lhs->c + (~rhs.c);
    lhs->c = t;
    t = (t >> 32) + lhs->d + (~rhs.d);
    lhs->d = t;
    t = (t >> 32) + lhs->e + (~rhs.e);
    lhs->e = t;
    lhs->high += (t >> 32) + (~rhs.high);
#endif
}

// Compare two 192-bit integers or fractions.
static int isLessThan192x192(swift_uint192_t lhs, swift_uint192_t rhs) {
#if HAVE_UINT128_T
    return (lhs.high < rhs.high)
        || (lhs.high == rhs.high
            && (lhs.mid < rhs.mid
                || (lhs.mid == rhs.mid
                    && lhs.low < rhs.low)));
#else
    return (lhs.high < rhs.high
            || (lhs.high == rhs.high
                && (lhs.e < rhs.e
                    || (lhs.e == rhs.e
                        && (lhs.d < rhs.d
                            || (lhs.d == rhs.d
                                && (lhs.c < rhs.c
                                    || (lhs.c == rhs.c
                                        && (lhs.b < rhs.b
                                            || (lhs.b == rhs.b
                                                && (lhs.low < rhs.low)))))))))));
#endif
}

// Shift a 192-bit integer right, rounding down.
static void shiftRightRoundingDown192(swift_uint192_t *lhs, int shift) {
#if HAVE_UINT128_T
    __uint128_t t = (__uint128_t)lhs->low >> shift;
    t += ((__uint128_t)lhs->mid << (64 - shift));
    lhs->low = t;
    t >>= 64;
    t += ((__uint128_t)lhs->high << (64 - shift));
    lhs->mid = t;
    t >>= 64;
    lhs->high = t;
#else
    uint64_t t = (uint64_t)lhs->low >> shift;
    t += ((uint64_t)lhs->b << (32 - shift));
    lhs->low = t;
    t >>= 32;
    t += ((uint64_t)lhs->c << (32 - shift));
    lhs->b = t;
    t >>= 32;
    t += ((uint64_t)lhs->d << (32 - shift));
    lhs->c = t;
    t >>= 32;
    t += ((uint64_t)lhs->e << (32 - shift));
    lhs->d = t;
    t >>= 32;
    t += ((uint64_t)lhs->high << (32 - shift));
    lhs->e = t;
    t >>= 32;
    lhs->high = t;
#endif
}

// Shift a 192-bit integer right, rounding up.
// Note: The shift will always be less than 20.  Someday, that
// might suggest a way to further optimize this.
static void shiftRightRoundingUp192(swift_uint192_t *lhs, int shift) {
#if HAVE_UINT128_T
    const uint64_t bias = (1 << shift) - 1;
    __uint128_t t = ((__uint128_t)lhs->low + bias) >> shift;
    t += ((__uint128_t)lhs->mid << (64 - shift));
    lhs->low = t;
    t >>= 64;
    t += ((__uint128_t)lhs->high << (64 - shift));
    lhs->mid = t;
    t >>= 64;
    lhs->high = t;
#else
    const uint64_t bias = (1 << shift) - 1;
    uint64_t t = ((uint64_t)lhs->low + bias) >> shift;
    t += ((uint64_t)lhs->b << (32 - shift));
    lhs->low = t;
    t >>= 32;
    t += ((uint64_t)lhs->c << (32 - shift));
    lhs->b = t;
    t >>= 32;
    t += ((uint64_t)lhs->d << (32 - shift));
    lhs->c = t;
    t >>= 32;
    t += ((uint64_t)lhs->e << (32 - shift));
    lhs->d = t;
    t >>= 32;
    t += ((uint64_t)lhs->high << (32 - shift));
    lhs->e = t;
    t >>= 32;
    lhs->high = t;
#endif
}
#endif

//
// ------------ Power of 10 calculation ----------------
//

//
// ------------  Power-of-10 tables. --------------------------
//
// Grisu-style algorithms rely on being able to rapidly
// find a high-precision approximation of any power of 10.
// These values were computed by a simple script that
// relied on Python's excellent variable-length
// integer support.

#if SWIFT_DTOA_FLOAT_SUPPORT || SWIFT_DTOA_DOUBLE_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT
// Float table
//
// The constant powers of 10 here represent pure fractions
// with a binary point at the far left. (Each number in
// this first table is implicitly divided by 2^64.)
//
// Table size: 320 bytes
//
// A 64-bit significand allows us to exactly represent
// powers of 10 up to 10^27.  For larger powers, the
// value here is rounded DOWN from the exact value.
// For those powers, the value here is less than the
// exact power of 10; adding one gives a value greater
// than the exact power of 10.
//
// For single-precision Float, we use these directly
// for positive powers of 10.  For negative powers of
// ten, we multiply a value here by 10^-40.
//
// For Double and Float80, we use the 28 exact values
// here to help reduce the size of those tables.
static const uint64_t powersOf10_Float[40] = {
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
};
#endif

#if SWIFT_DTOA_DOUBLE_SUPPORT
// As above, but with 128-bit fractions.
//
// Table size: 464 bytes
//
// We only store every 28th power of ten here.
// We can multiply by an exact 64-bit power of
// ten from the table above to reconstruct the
// significand for any power of 10.
static const uint64_t powersOf10_Double[] = {
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
};
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
// Every 83rd power of 10 across the range of Float80
//
// Table size: 2,928 bytes
//
// Note: We could cut this in half at the cost of one additional
// 192-bit multiply by only storing the positive values and
// multiplying by 10^-5063 to obtain the negative ones, similar
// to how we handle Float above.
static const uint64_t powersOf10_Float80[] = {
    // low 64 bits, middle 64 bits, high 64 bits
    0x56825ada98468526, 0x0abc23fcfda07e29, 0x871db786569ca2dd, // x 2^-16818 ~= 10^-5063
    0xe3885beff5ee930d, 0xd1e638f68c97f0e5, 0xde90d1b113564507, // x 2^-16543 ~= 10^-4980
    0x5a7d22b5236bcad4, 0xbab3a28a6f0a489c, 0xb74ea21ab2946479, // x 2^-16267 ~= 10^-4897
    0x7d1f78bf0f4e2878, 0xcf4aea39615ffe6e, 0x96f9351fcfdd2686, // x 2^-15991 ~= 10^-4814
    0xad725c0d6c214314, 0x5bd5c19f18bd2857, 0xf8afafd6ef185238, // x 2^-15716 ~= 10^-4731
    0xe418e9217ce83755, 0x801e38463183fc88, 0xccd1ffc6bba63e21, // x 2^-15440 ~= 10^-4648
    0x4dcd52747e029d0c, 0x867b3096b1619df8, 0xa8b11ff4721d92fb, // x 2^-15164 ~= 10^-4565
    0xed2903f7e1df2b78, 0xc846664fe1364ee8, 0x8aefaaae9060380f, // x 2^-14888 ~= 10^-4482
    0xed7a7f4e1e171498, 0x7da1a627b88527f1, 0xe4dbb751faa311b0, // x 2^-14613 ~= 10^-4399
    0x320796dc9b1a158c, 0x2a11a871597b8012, 0xbc7d620092481a7e, // x 2^-14337 ~= 10^-4316
    0x796014ec6f4c0dcb, 0xcfa99f62903708d7, 0x9b3dee433c1311e9, // x 2^-14061 ~= 10^-4233
    0x08920ae76bdb8282, 0x952b06c385a08ff6, 0xffb7a402531fd4c9, // x 2^-13786 ~= 10^-4150
    0x18faa162f2c4d6b9, 0x050be8c5d21c6db6, 0xd29c7528965ae5bd, // x 2^-13510 ~= 10^-4067
    0x576a6c1abab7f7e7, 0xc05fb0b5c550f28d, 0xad7617610634129e, // x 2^-13234 ~= 10^-3984
    0x6cc3b2fb9cae4875, 0xe1bd5b09b7202157, 0x8edd4417ae3dc210, // x 2^-12958 ~= 10^-3901
    0xfafc1dc8fd12a6f8, 0xc3f29d230036529b, 0xeb542860da9bc7d8, // x 2^-12683 ~= 10^-3818
    0x9d198c56bc799f35, 0x42960adf9591f02a, 0xc1d1a4b6bc2eafc8, // x 2^-12407 ~= 10^-3735
    0x1803d096e43ff4bc, 0xdef9759d6432dab7, 0x9fa18c5de65a16fb, // x 2^-12131 ~= 10^-3652
    0x74872779576a577c, 0x9150140eb5101a96, 0x83793dfc83fd2f9b, // x 2^-11855 ~= 10^-3569
    0x415d0667f0f88262, 0x4898d98d1314d99f, 0xd890d45a257f4644, // x 2^-11580 ~= 10^-3486
    0x30a5256a610c1c72, 0x6ebca4d0365d504d, 0xb25d93f98145bdab, // x 2^-11304 ~= 10^-3403
    0xfb149b1f86a46376, 0xb5143323a7f8e16c, 0x92e74be10524c389, // x 2^-11028 ~= 10^-3320
    0x7b7532e25fead4c8, 0x0df6ab8ac6a0ec2f, 0xf1fb6e82e4df4a77, // x 2^-10753 ~= 10^-3237
    0x3b738d6a3caae67a, 0x346b2dd31826cfed, 0xc74c79bce7fe949f, // x 2^-10477 ~= 10^-3154
    0x22d3a12777cee527, 0xe185ac46f6ef1993, 0xa424ef0bb5ad3129, // x 2^-10201 ~= 10^-3071
    0xb7b6bccb9f60adec, 0x30ad7df78fe30cc8, 0x8730d40821cd89f3, // x 2^-9925 ~= 10^-2988
    0x21049149d72d44d5, 0x1e86debc54dd290d, 0xdeb04cb82aec22cb, // x 2^-9650 ~= 10^-2905
    0xeb69d287f5e7f920, 0x8d7e84a13801d034, 0xb7688f9800d26b3a, // x 2^-9374 ~= 10^-2822
    0x47b3da9aeff7df71, 0x82678774d6c0ac59, 0x970e8fd25ead01e3, // x 2^-9098 ~= 10^-2739
    0x50d3e92e2e4f8210, 0x9492db3d978aaca8, 0xf8d2dcaf37504b51, // x 2^-8823 ~= 10^-2656
    0xf4ce72058f777a4c, 0xb9eb2c585e924efe, 0xcceef83fdedcc506, // x 2^-8547 ~= 10^-2573
    0xb0e624a35b791884, 0x7104a98dbbb38b94, 0xa8c8fc3b03c3d1ed, // x 2^-8271 ~= 10^-2490
    0x6c94ecd8291e2ac9, 0x978b03821014b68c, 0x8b03518396007c08, // x 2^-7995 ~= 10^-2407
    0x96475208daa03ee3, 0x10eaa1481b149e5a, 0xe4fc163319551441, // x 2^-7720 ~= 10^-2324
    0xd709a820ff4ac847, 0x75aab7cb5cb15414, 0xbc980b270680156a, // x 2^-7444 ~= 10^-2241
    0xf273bca6b4de9e24, 0xb5025d88d4b252e1, 0x9b53e384eccabcf7, // x 2^-7168 ~= 10^-2158
    0x6c55a0603a928f40, 0x9e20db16dfb6b461, 0xffdbcf7251089796, // x 2^-6893 ~= 10^-2075
    0x9006db4d43cffe42, 0x9b4bca4cd6cec2db, 0xd2ba3f510a3aa638, // x 2^-6617 ~= 10^-1992
    0xa6b3c457fd0cd4d6, 0x28a4de91ba868fbf, 0xad8ea05a5f27642a, // x 2^-6341 ~= 10^-1909
    0xe7b14ed140f8d98e, 0x7b2f7d61ce5d426c, 0x8ef179291b6f5424, // x 2^-6065 ~= 10^-1826
    0x4a964d052fd03e10, 0x06897060bf491e6e, 0xeb75718d285cd8bf, // x 2^-5790 ~= 10^-1743
    0x22b2270f0e8dd87c, 0xa8510fa2f5a9e4de, 0xc1ed0ed498f7c54c, // x 2^-5514 ~= 10^-1660
    0x09102915726a9905, 0x5a0eb896edc89b54, 0x9fb8208d65ea5eda, // x 2^-5238 ~= 10^-1577
    0xb80d5f481d01deb9, 0x673f2aa50486f5ba, 0x838bd699b7c539e6, // x 2^-4962 ~= 10^-1494
    0x668b62b20ec2633b, 0x8682604c7123f859, 0xd8af761f94d2db2c, // x 2^-4687 ~= 10^-1411
    0xb2adaed8559cc199, 0x712339ba54f12372, 0xb276ce87987995d5, // x 2^-4411 ~= 10^-1328
    0x6beb873308685711, 0xac1ce34246ed56ad, 0x92fc133455668c02, // x 2^-4135 ~= 10^-1245
    0x593293d68a2261bc, 0x3c368f9497ca075d, 0xf21da89a29fa1c61, // x 2^-3860 ~= 10^-1162
    0x854051f9f0e4ca66, 0x8c5d5a234eda57f7, 0xc768aa46d6d1b675, // x 2^-3584 ~= 10^-1079
    0x333d09b2299c5e6b, 0xcf1f49c33399c5ac, 0xa43c26a751d4f7e7, // x 2^-3308 ~= 10^-996
    0x25a440d8b1620532, 0x274ebc67c3e21943, 0x8743f33df0feed29, // x 2^-3032 ~= 10^-913
    0x3ca95e3deb5be648, 0x52d18ccca1c558c2, 0xdecfcc3329238dd8, // x 2^-2757 ~= 10^-830
    0x59d1a7704af3acd7, 0xfae7722c6af19467, 0xb78280c024488353, // x 2^-2481 ~= 10^-747
    0x78813f3e80148049, 0x73b2baf13aa1c233, 0x9723ed8a28baf5ac, // x 2^-2205 ~= 10^-664
    0xf296a8198aa40fb8, 0x235532b08487fe6a, 0xf8f60e812de0cd7d, // x 2^-1930 ~= 10^-581
    0xa7fbdcb40b4f648f, 0x4f20ba9a64a7f6e7, 0xcd0bf4d206072167, // x 2^-1654 ~= 10^-498
    0x8dbf63ea468c724f, 0xa0e25c08b5c189d6, 0xa8e0dbe18ffb82cf, // x 2^-1378 ~= 10^-415
    0x765995c6cfd406ce, 0x4c3bcb5021afcc31, 0x8b16fb203055ac76, // x 2^-1102 ~= 10^-332
    0xe1d09ab6fb409872, 0x82b7e12780e7401a, 0xe51c79a85916f484, // x 2^-827 ~= 10^-249
    0x89cbe2422f9e1df9, 0x7415d448f6b6f0e7, 0xbcb2b812db11a5de, // x 2^-551 ~= 10^-166
    0x605fe83842e4d290, 0xc986afbe3ee11aba, 0x9b69dbe1b548ce7c, // x 2^-275 ~= 10^-83
    0x0000000000000000, 0x0000000000000000, 0x8000000000000000, // x 2^1 == 10^0 exactly
    0x0d6953169e1c7a1e, 0xf50a3fa490c30190, 0xd2d80db02aabd62b, // x 2^276 ~= 10^83
    0x3720b80c7d8ee39d, 0xaf561aa79a10ae6a, 0xada72ccc20054ae9, // x 2^552 ~= 10^166
    0x7cb3f026a212df74, 0x29cb4d87f2a7400e, 0x8f05b1163ba6832d, // x 2^828 ~= 10^249
    0x7dda22f9451d28a4, 0xe41c5bd18c57e88f, 0xeb96bf6ebadf77d8, // x 2^1103 ~= 10^332
    0xd5da00e6e2d05e5d, 0x5e510c5a752f0f8e, 0xc2087cd3215a16ad, // x 2^1379 ~= 10^415
    0x5603ba353e0b2fac, 0x48bbddc4d7359e49, 0x9fceb7ee780436f0, // x 2^1655 ~= 10^498
    0x15ceea8df15e47c7, 0x6a83c85cf158c652, 0x839e71d847c1779e, // x 2^1931 ~= 10^581
    0x514478d1fcd48eea, 0x3a4181cdda0d6e24, 0xd8ce1c3a2fffaea7, // x 2^2206 ~= 10^664
    0xe8b634620f1062be, 0x7304c7fb8a2f8a8a, 0xb2900ca735bdf121, // x 2^2482 ~= 10^747
    0xc3ec2fd9302c9bda, 0x729a6a7e830e1cf2, 0x9310dd78089bd66f, // x 2^2758 ~= 10^830
    0x1750ef5f751be079, 0x52ccabc96fc88a23, 0xf23fe788c763dffa, // x 2^3033 ~= 10^913
    0xaa80925ec1c80b65, 0x97681c548ff6c12f, 0xc784decd820a6180, // x 2^3309 ~= 10^996
    0xb4212d4b435a2317, 0x8df0a55abbb2c99a, 0xa453618b9dfd92db, // x 2^3585 ~= 10^1079
    0x939c2fedd434642a, 0x7d7de34bf5aa96b4, 0x875715282612729b, // x 2^3861 ~= 10^1162
    0xb7d9a0e46bbebb36, 0x3b2057dea52d686b, 0xdeef5022af37f1f6, // x 2^4136 ~= 10^1245
    0x931a74148ea64e59, 0xfe7fe67bd1074d0c, 0xb79c7593a1c17df0, // x 2^4412 ~= 10^1328
    0xabd20df0f1f1ad54, 0x0fff83cc7fa6b77b, 0x97394e479b6573b1, // x 2^4688 ~= 10^1411
    0x25f2467421674b7a, 0x828ff55a248bc026, 0xf919454d86f16685, // x 2^4963 ~= 10^1494
    0xe32dbd7131e6ab7d, 0xf674dd4821982084, 0xcd28f57dc585d094, // x 2^5239 ~= 10^1577
    0x866ab816a532b07d, 0xdc567471f9639b4e, 0xa8f8bee890f905c7, // x 2^5515 ~= 10^1660
    0xaca3a975993a2626, 0xc41cf207a71d87e4, 0x8b2aa784c405e2f1, // x 2^5791 ~= 10^1743
    0x731f0b7d820918bd, 0x355fde18e8448607, 0xe53ce1b25fb31788, // x 2^6066 ~= 10^1826
    0x0be7c29568db3f20, 0xc1328a3f1bf4d2b8, 0xbccd68c49888be61, // x 2^6342 ~= 10^1909
    0x9c6e0b1b927b7d3f, 0xffc9b96619da642a, 0x9b7fd75a060350cd, // x 2^6618 ~= 10^1992
    0x2a84c8fb4bd2edc9, 0xa679df45d339389b, 0x80121ad60ca2c518, // x 2^6894 ~= 10^2075
    0x0d4648d0876cf1c3, 0x48c67661c087fb5a, 0xd2f5e0469040e0eb, // x 2^7169 ~= 10^2158
    0xfe2d99a281a011ac, 0x65c13361e6b2c078, 0xadbfbcb6c676a69b, // x 2^7445 ~= 10^2241
    0xf4ec157aa4147562, 0xac89bfa5e79484a6, 0x8f19ebdf7661e3e9, // x 2^7721 ~= 10^2324
    0xe945f8c80090be1f, 0x9b8672e64aadbed2, 0xebb812063c9e01db, // x 2^7996 ~= 10^2407
    0xca12d8ad3b36d2e4, 0xd252322ea50ad274, 0xc223eeb2e1bde452, // x 2^8272 ~= 10^2490
    0xf554fb41e5b3e384, 0x977acb4d4af624fc, 0x9fe55281904ba38b, // x 2^8548 ~= 10^2573
    0xf3f69093398e2573, 0x111ae5735ec0e878, 0x83b10fb893300cde, // x 2^8824 ~= 10^2656
    0x30f65a8da0d10429, 0x1eecf4cf8a0b25f5, 0xd8ecc6aa93e876fc, // x 2^9099 ~= 10^2739
    0xa577f5f0c9f1e5a7, 0x91a5430ed623abf0, 0xb2a94e58da4930c3, // x 2^9375 ~= 10^2822
    0x202d2f87585ec0d7, 0x7a63589863efd480, 0x9325aaac89304b57, // x 2^9651 ~= 10^2905
    0x049544fbba3c01a1, 0x53accb0f60ac6095, 0xf2622b4f6c68d6ce, // x 2^9926 ~= 10^2988
    0x268a0d5f9d5ce861, 0xfe40703e1a91de57, 0xc7a117517a09153b, // x 2^10202 ~= 10^3071
    0xb6cd470a2a3b1d63, 0x5f963916b20ea587, 0xa46a9fb9111003bc, // x 2^10478 ~= 10^3154
    0xa3101c09fd8e6e96, 0xa2c6328011db5211, 0x876a39c722f798a7, // x 2^10754 ~= 10^3237
    0x8507e5fdb0ec5d83, 0x7ce93cc7f8feeed4, 0xdf0ed8875e7b8914, // x 2^11029 ~= 10^3320
    0xe19b7ebe4c7bfbca, 0x40930d1129943838, 0xb7b66e12fe1af499, // x 2^11305 ~= 10^3403
    0xc90c4ec15c21a357, 0xd91c86512d147305, 0x974eb20b241a65f6, // x 2^11581 ~= 10^3486
    0xb90341199c02a4eb, 0x69e684f53db6e8ce, 0xf93c8114f6c31f8a, // x 2^11856 ~= 10^3569
    0xa873f1318cef91cb, 0xbf8718466b31a7ca, 0xcd45fa43b1ce4c8e, // x 2^12132 ~= 10^3652
    0xacfe0dcc5262e273, 0x6e1bbb68662fd27a, 0xa910a550810203f8, // x 2^12408 ~= 10^3735
    0x59e7c8921bbe3758, 0x834743c5eab7dcea, 0x8b3e56b1b5c57589, // x 2^12684 ~= 10^3818
    0x145eed64fda2e6af, 0x1c605bdcc764238f, 0xe55d4e51d30b5592, // x 2^12959 ~= 10^3901
    0xb747164b17268ea2, 0xd8aa19f1d85da07d, 0xbce81d3cc784a1ca, // x 2^13235 ~= 10^3984
    0x3666af1cb2f0356b, 0xc8dd55687a68bb70, 0x9b95d5ee4f80366d, // x 2^13511 ~= 10^4067
    0x70d67261b5bde1e9, 0x1d76f2d15166ec20, 0x8024383bab19730d, // x 2^13787 ~= 10^4150
    0x084a3ba0b748546a, 0xc67f9026f83dca47, 0xd313b714d3a1c65e, // x 2^14062 ~= 10^4233
    0x4411a8127eea085e, 0x441eb397ffcdab0d, 0xadd8501ad0361d15, // x 2^14338 ~= 10^4316
    0x7b62a54ed6233032, 0x75458a1c8300e014, 0x8f2e2985332eae98, // x 2^14614 ~= 10^4399
    0x162d5b51a1dd9594, 0x655bb1b7aa4e8196, 0xebd96954582af06f, // x 2^14889 ~= 10^4482
    0x55e6c62f920d3682, 0x79fd57cf7c37941c, 0xc23f6474669f4abe, // x 2^15165 ~= 10^4565
    0x19482fa0ac45669c, 0x803c1cd864033781, 0x9ffbf04722750449, // x 2^15441 ~= 10^4648
    0xa412d1f95f4624cd, 0xc95abe9ce589e048, 0x83c3b03af95c9674, // x 2^15717 ~= 10^4731
    0xc1207e487c57b4e1, 0xf93dd2c7669a8ed1, 0xd90b75715d861b38, // x 2^15992 ~= 10^4814
    0xeb20d9a25e0372bd, 0xb5073df6adc221b4, 0xb2c2939d0763fcac, // x 2^16268 ~= 10^4897
    0x1a648c339e28cc45, 0xbd14f0fa3e24b6ae, 0x933a7ad2419ea0b5, // x 2^16544 ~= 10^4980
};
#endif

#if SWIFT_DTOA_FLOAT_SUPPORT || SWIFT_DTOA_DOUBLE_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT
// The power-of-10 tables do not directly store the associated binary
// exponent.  That's because the binary exponent is a simple linear
// function of the decimal power (and vice versa), so it's just as
// fast (and uses much less memory) to compute it:

// The binary exponent corresponding to a particular power of 10.
// This matches the power-of-10 tables across the full range of Float80.
static int binaryExponentFor10ToThe(int p) {
    return (int)(((((int64_t)p) * 55732705) >> 24) + 1);
}

// A decimal exponent that approximates a particular binary power.
static int decimalExponentFor2ToThe(int e) {
    return (int)(((int64_t)e * 20201781) >> 26);
}
#endif

#if SWIFT_DTOA_FLOAT_SUPPORT
// Given a power `p`, this returns three values:
// * 64-bit fractions `lower` and `upper`
// * integer `exponent`
//
// The returned values satisty the following:
// ```
//    lower * 2^exponent <= 10^p <= upper * 2^exponent
// ```
//
// Note: Max(*upper - *lower) = 3
static void intervalContainingPowerOf10_Float(int p, uint64_t *lower, uint64_t *upper, int *exponent) {
    if (p < 0) {
        uint64_t base = powersOf10_Float[p + 40];
        int baseExponent = binaryExponentFor10ToThe(p + 40);
        uint64_t tenToTheMinus40 = 0x8b61313bbabce2c6; // x 2^-132 ~= 10^-40
        *upper = multiply64x64RoundingUp(base + 1, tenToTheMinus40 + 1);
        *lower = multiply64x64RoundingDown(base, tenToTheMinus40);
        *exponent = baseExponent - 132;
    } else {
        uint64_t base = powersOf10_Float[p];
        *upper = base + 1;
        *lower = base;
        *exponent = binaryExponentFor10ToThe(p);
    }
}
#endif

#if SWIFT_DTOA_DOUBLE_SUPPORT
// As above, but returning 128-bit fractions suitable for
// converting doubles.
static void intervalContainingPowerOf10_Double(int p, swift_uint128_t *lower, swift_uint128_t *upper, int *exponent) {
    if (p >= 0 && p <= 54) {
        if (p <= 27) {
            // Use one 64-bit exact value
            swift_uint128_t exact;
            initialize128WithHigh64(exact, powersOf10_Float[p]);
            *upper = exact;
            *lower = exact;
            *exponent = binaryExponentFor10ToThe(p);
            return;
        } else {
            // Multiply two 64-bit exact values to get a 128-bit exact value
            swift_uint128_t base;
            initialize128WithHigh64(base, powersOf10_Float[p - 27]);
            int baseExponent = binaryExponentFor10ToThe(p - 27);
            uint64_t extra = powersOf10_Float[27];
            int extraExponent = binaryExponentFor10ToThe(27);
            swift_uint128_t exact = multiply128x64RoundingDown(base, extra);
            *upper = exact;
            *lower = exact;
            *exponent = baseExponent + extraExponent;
            return;
        }
    }

    // Multiply a 128-bit approximate value with a 64-bit exact value
    int index = p + 400;
    // Copy a pair of uint64_t into a swift_uint128_t
    const uint64_t *base_p = powersOf10_Double + (index / 28) * 2;
    swift_uint128_t base;
    initialize128WithHighLow64(base, base_p[1], base_p[0]);
    int extraPower = index % 28;
    int baseExponent = binaryExponentFor10ToThe(p - extraPower);

    int e = baseExponent;
    if (extraPower > 0) {
        int64_t extra = powersOf10_Float[extraPower];
        e += binaryExponentFor10ToThe(extraPower);
        *lower = multiply128x64RoundingDown(base, extra);
        increment128(base);
        *upper = multiply128x64RoundingUp(base, extra);
    } else {
        *lower = base;
        increment128(base);
        *upper = base;
    }
    *exponent = e;
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
// As above, but returning 192-bit fractions suitable for
// converting float80.
static void intervalContainingPowerOf10_Float80(int p, swift_uint192_t *lower, swift_uint192_t *upper, int *exponent) {
    if (p >= 0 && p <= 27) {
        // We have an exact form, return a zero-width interval.
        uint64_t exact = powersOf10_Float[p];
        initialize192WithHighMidLow64(*upper, exact, 0, 0);
        initialize192WithHighMidLow64(*lower, exact, 0, 0);
        *exponent = binaryExponentFor10ToThe(p);
        return;
    }

    int index = p + 5063;
    const uint64_t *base_p = powersOf10_Float80 + (index / 83) * 3;
    // Note: The low-order value in the Float80 table above
    // is never UINT64_MAX, so there's never a carry from
    // the increment here.
    initialize192WithHighMidLow64(*upper, base_p[2], base_p[1], base_p[0] + 1);
    initialize192WithHighMidLow64(*lower, base_p[2], base_p[1], base_p[0]);
    int extraPower = index % 83;
    int e = binaryExponentFor10ToThe(p - extraPower);

    while (extraPower > 27) {
        uint64_t power27 = powersOf10_Float[27];
        multiply192x64RoundingDown(lower, power27);
        multiply192x64RoundingUp(upper, power27);
        e += binaryExponentFor10ToThe(27);
        extraPower -= 27;
    }
    if (extraPower > 0) {
        uint64_t extra = powersOf10_Float[extraPower];
        multiply192x64RoundingDown(lower, extra);
        multiply192x64RoundingUp(upper, extra);
        e += binaryExponentFor10ToThe(extraPower);
    }
    *exponent = e;
}
#endif

