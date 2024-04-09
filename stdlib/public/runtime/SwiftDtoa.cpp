//===--- SwiftDtoa.cpp ---------------------------------------------*- C++ -*-===//
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
// Note: This source file is used in different projects where it gets
// compiled variously as ".c" or ".cpp".  Please keep the code clean
// portable C so others can share your improvements.
//
/// For binary16, this uses a simple approach that is normally
/// implemented with variable-length arithmetic.  However, due to
/// the limited range of binary16, this can be implemented simply
/// with only 32-bit integer arithmetic.
///
/// For other formats, SwiftDtoa uses a modified form of the Grisu2
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
/// * Portable.  The code is written in portable C99.  The core
///   implementations utilize only fixed-size integer arithmetic.
///   128-bit integer support is utilized if present but is not
///   necessary.  There are thin wrappers that accept platform-native
///   floating point types and delegate to the portable core
///   functions.
///
// ----------------------------------------------------------------------------

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
  // represent 256-bit integers as collections of 64-bit elements.
  #define HAVE_UINT128_T 1
#else
  // On 32-bit, we use slower code that manipulates 128-bit
  // and 256-bit integers as collections of 32-bit elements.
  #define HAVE_UINT128_T 0
#endif

//
// Predefine various arithmetic helpers.  Most implementations and extensive
// comments are at the bottom of this file.
//

#if SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
// The power-of-10 tables do not directly store the associated binary
// exponent.  That's because the binary exponent is a simple linear
// function of the decimal power (and vice versa), so it's just as
// fast (and uses much less memory) to compute it:

// The binary exponent corresponding to a particular power of 10.
// This matches the power-of-10 tables across the full range of binary128.
#define binaryExponentFor10ToThe(p) ((int)(((((int64_t)(p)) * 55732705) >> 24) + 1))

// A decimal exponent that approximates a particular binary power.
#define decimalExponentFor2ToThe(e) ((int)(((int64_t)e * 20201781) >> 26))
#endif

//
// Helper functions used only by the single-precision binary32 formatter
//

#if SWIFT_DTOA_BINARY32_SUPPORT
static uint64_t multiply64x32RoundingDown(uint64_t lhs, uint32_t rhs) {
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = ((lhs & mask32) * rhs) >> 32;
    return t + (lhs >> 32) * rhs;
}
static uint64_t multiply64x32RoundingUp(uint64_t lhs, uint32_t rhs) {
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = (((lhs & mask32) * rhs) + mask32) >> 32;
    return t + (lhs >> 32) * rhs;
}
static void intervalContainingPowerOf10_Binary32(int p, uint64_t *lower, uint64_t *upper, int *exponent);
#endif

//
// Helpers used by binary32, binary64, float80, and binary128.
//

#if SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
#if HAVE_UINT128_T
typedef __uint128_t swift_uint128_t;
#define initialize128WithHighLow64(dest, high64, low64) ((dest) = ((__uint128_t)(high64) << 64) | (low64))
#define shiftLeft128(u128, shift) (*(u128) <<= shift)
#else
typedef struct {
    uint32_t low, b, c, high;
} swift_uint128_t;
#define initialize128WithHighLow64(dest, high64, low64) \
    ((dest).low = (uint32_t)(low64),                    \
     (dest).b = (uint32_t)((low64) >> 32),              \
     (dest).c = (uint32_t)(high64),                     \
     (dest).high = (uint32_t)((high64) >> 32))
static void shiftLeft128(swift_uint128_t *, int shift);
#endif
inline static int finishFormatting(char *, size_t, char *, char *, int, int);
#endif


//
// Helper functions needed by the binary64 formatter.
//

#if SWIFT_DTOA_BINARY64_SUPPORT
#if HAVE_UINT128_T
#define isLessThan128x128(lhs, rhs) ((lhs) < (rhs))
#define subtract128x128(lhs, rhs) (*(lhs) -= (rhs))
#define multiply128xu32(lhs, rhs) (*(lhs) *= (rhs))
#define initialize128WithHigh64(dest, value) ((dest) = (__uint128_t)(value) << 64)
#define extractHigh64From128(arg) ((uint64_t)((arg) >> 64))
#define is128bitZero(arg) ((arg) == 0)
static int extractIntegerPart128(__uint128_t *fixed128, int integerBits) {
    const int fractionBits = 128 - integerBits;
    int integerPart = (int)(*fixed128 >> fractionBits);
    const swift_uint128_t fixedPointMask = (((__uint128_t)1 << fractionBits) - 1);
    *fixed128 &= fixedPointMask;
    return integerPart;
}
#define shiftRightRoundingDown128(val, shift) ((val) >> (shift))
#define shiftRightRoundingUp128(val, shift) (((val) + (((uint64_t)1 << (shift)) - 1)) >> (shift))

#else

static int isLessThan128x128(swift_uint128_t lhs, swift_uint128_t rhs);
static void subtract128x128(swift_uint128_t *lhs, swift_uint128_t rhs);
static void multiply128xu32(swift_uint128_t *lhs, uint32_t rhs);
#define initialize128WithHigh64(dest, value)            \
    ((dest).low = (dest).b = 0,                         \
     (dest).c = (uint32_t)(value),                      \
     (dest).high = (uint32_t)((value) >> 32))
#define extractHigh64From128(arg) (((uint64_t)(arg).high << 32)|((arg).c))
#define is128bitZero(dest) \
  (((dest).low | (dest).b | (dest).c | (dest).high) == 0)
// Treat a uint128_t as a fixed-point value with `integerBits` bits in
// the integer portion.  Return the integer portion and zero it out.
static int extractIntegerPart128(swift_uint128_t *fixed128, int integerBits) {
    const int highFractionBits = 32 - integerBits;
    int integerPart = (int)(fixed128->high >> highFractionBits);
    fixed128->high &= ((uint32_t)1 << highFractionBits) - 1;
    return integerPart;
}
static swift_uint128_t shiftRightRoundingDown128(swift_uint128_t lhs, int shift);
static swift_uint128_t shiftRightRoundingUp128(swift_uint128_t lhs, int shift);
#endif
static swift_uint128_t multiply128x64RoundingDown(swift_uint128_t lhs, uint64_t rhs);
static swift_uint128_t multiply128x64RoundingUp(swift_uint128_t lhs, uint64_t rhs);
static void intervalContainingPowerOf10_Binary64(int p, swift_uint128_t *lower, swift_uint128_t *upper, int *exponent);
#endif

//
// Helper functions used by the 256-bit backend needed for
// float80 and binary128
//

#if SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
#if HAVE_UINT128_T
// A 256-bit unsigned integer type stored as 3 64-bit words
typedef struct {uint64_t low, midlow, midhigh, high;} swift_uint256_t;
#define initialize256WithHighMidLow64(dest, high64, midhigh64, midlow64, low64) \
    ((dest).low = (low64),                                        \
     (dest).midlow = (midlow64),                                  \
     (dest).midhigh = (midhigh64),                                \
     (dest).high = (high64))
#define is256bitZero(dest) \
  (((dest).low | (dest).midlow | (dest).midhigh | (dest).high) == 0)
static int extractIntegerPart256(swift_uint256_t *fixed256, int integerBits) {
    int integerPart = (int)(fixed256->high >> (64 - integerBits));
    const uint64_t fixedPointMask = (((uint64_t)1 << (64 - integerBits)) - 1);
    fixed256->high &= fixedPointMask;
    return integerPart;
}
#else
// A 256-bit unsigned integer type stored as 8 32-bit words
typedef struct { uint32_t elt[8]; } swift_uint256_t; // [0]=low, [7]=high
#define initialize256WithHighMidLow64(dest, high64, midhigh64, midlow64, low64) \
    ((dest).elt[0] = (uint64_t)(low64),                              \
     (dest).elt[1] = (uint64_t)(low64) >> 32,                            \
     (dest).elt[2] = (uint64_t)(midlow64),                             \
     (dest).elt[3] = (uint64_t)(midlow64) >> 32,                       \
     (dest).elt[4] = (uint64_t)(midhigh64),                            \
     (dest).elt[5] = (uint64_t)(midhigh64) >> 32,                      \
     (dest).elt[6] = (uint64_t)(high64),                               \
     (dest).elt[7] = (uint64_t)(high64) >> 32)
#define is256bitZero(dest) \
  (((dest).elt[0] | (dest).elt[1] | (dest).elt[2] | (dest).elt[3] \
| (dest).elt[4] | (dest).elt[5] | (dest).elt[6] | (dest).elt[7]) == 0)
static int extractIntegerPart256(swift_uint256_t *fixed256, int integerBits) {
    int integerPart = (int)(fixed256->elt[7] >> (32 - integerBits));
    const uint64_t fixedPointMask = (((uint64_t)1 << (32 - integerBits)) - 1);
    fixed256->elt[7] &= fixedPointMask;
    return integerPart;
}
#endif
static void multiply256xu32(swift_uint256_t *lhs, uint32_t rhs);
// Multiply a 256-bit fraction times a 128-bit fraction, with controlled rounding
static void multiply256x128RoundingDown(swift_uint256_t *lhs, swift_uint128_t rhs);
static void multiply256x128RoundingUp(swift_uint256_t *lhs, swift_uint128_t rhs);
static void subtract256x256(swift_uint256_t *lhs, swift_uint256_t rhs);
static int isLessThan256x256(swift_uint256_t lhs, swift_uint256_t rhs);
static void shiftRightRoundingDown256(swift_uint256_t *lhs, int shift);
static void shiftRightRoundingUp256(swift_uint256_t *lhs, int shift);
static void intervalContainingPowerOf10_Binary128(int p, swift_uint256_t *lower, swift_uint256_t *upper, int *exponent);
static size_t _swift_dtoa_256bit_backend(char *, size_t, swift_uint128_t, swift_uint128_t, int, int, int, int, bool);
#endif


// A table of all two-digit decimal numbers
#if SWIFT_DTOA_BINARY16_SUPPORT || SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
static const char asciiDigitTable[] =
  "0001020304050607080910111213141516171819"
  "2021222324252627282930313233343536373839"
  "4041424344454647484950515253545556575859"
  "6061626364656667686970717273747576777879"
  "8081828384858687888990919293949596979899";
#endif

// ================================================================
//
// Helpers to output formatted results for infinity, zero, and NaN
//
// ================================================================

static size_t infinity(char *dest, size_t len, int negative) {
  if (negative) {
    if (len >= 5) {
      memcpy(dest, "-inf", 5);
      return 4;
    }
  } else {
    if (len >= 4) {
      memcpy(dest, "inf", 4);
      return 3;
    }
  }
  if (len > 0) {
    dest[0] = '\0';
  }
  return 0;
}

static size_t zero(char *dest, size_t len, int negative) {
  if (negative) {
    if (len >= 5) {
      memcpy(dest, "-0.0", 5);
      return 4;
    }
  } else {
    if (len >= 4) {
      memcpy(dest, "0.0", 4);
      return 3;
    }
  }
  if (len > 0) {
    dest[0] = '\0';
  }
  return 0;
}

static size_t nan_details(char *dest, size_t len, int negative, int quiet, uint64_t payloadHigh, uint64_t payloadLow) {
  const char *sign = negative ? "-" : "";
  const char *signalingChar = quiet ? "" : "s";
  char buff[64];
  if (payloadLow != 0) {
    if (payloadHigh != 0) {
      snprintf(buff, sizeof(buff), "%s%snan(0x%" PRIx64 "%016" PRIx64 ")",
               sign, signalingChar, payloadHigh, payloadLow);
    } else {
      snprintf(buff, sizeof(buff), "%s%snan(0x%" PRIx64 ")",
               sign, signalingChar, payloadLow);
    }
  } else {
    snprintf(buff, sizeof(buff), "%s%snan",
             sign, signalingChar);
  }
  size_t nanlen = strlen(buff);
  if (nanlen < len) {
    memcpy(dest, buff, nanlen + 1);
    return nanlen;
  }
  if (len > 0) {
    dest[0] = '\0';
  }
  return 0;
}


// ================================================================
//
// BINARY16
//
// ================================================================


#if SWIFT_DTOA_BINARY16_SUPPORT
#if !SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT
// Format a C `_Float16`
size_t swift_dtoa_optimal_binary16(_Float16 d, char *dest, size_t length) {
  return swift_dtoa_optimal_binary16_p(&d, dest, length);
}
#endif

// Format an IEEE 754 binary16 half-precision floating point value
// into an optimal text form.

// This does not assume that the C environment has any support
// for binary16.

// Because binary16 has such a limited range, a simple exact
// implementation can fit in 32 bit arithmetic.  Since we can easily
// verify every single binary16 value, this can be experimentally
// optimized.
size_t swift_dtoa_optimal_binary16_p(const void *f, char *dest, size_t length) {
    static const int significandBitCount = 10;
    static const uint32_t significandMask
        = ((uint32_t)1 << significandBitCount) - 1;
    static const int exponentBitCount = 5;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_dtoa_optimal_binary64_p
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 14

    if (length < 1) {
      return 0;
    }

    // Step 0: Deconstruct IEEE 754 binary16 format
    uint16_t raw = *(const uint16_t *)f;
    int exponentBitPattern = (raw >> significandBitCount) & exponentMask;
    uint16_t significandBitPattern = raw & significandMask;
    int negative = raw >> 15;

    // Step 1: Handle the various input cases:
    int binaryExponent;
    uint16_t significand;
    int isBoundary = significandBitPattern == 0;
    if (exponentBitPattern == exponentMask) { // NaN or Infinity
        if (isBoundary) { // Infinity
            return infinity(dest, length, negative);
        } else {
            const int quiet = (significandBitPattern >> (significandBitCount - 1)) & 1;
            uint16_t payload = significandBitPattern & ((1U << (significandBitCount - 2)) - 1);
            return nan_details(dest, length, negative, quiet, 0, payload);
        }
    } else if (exponentBitPattern == 0) {
        if (isBoundary) { // Zero
          return zero(dest, length, negative);
        } else { // Subnormal
            binaryExponent = 1 - exponentBias;
            significand = significandBitPattern;
        }
    } else { // normal
        binaryExponent = exponentBitPattern - exponentBias;
        uint16_t hiddenBit = (uint32_t)1 << (uint32_t)significandBitCount;
        uint16_t fullSignificand = significandBitPattern + hiddenBit;
        significand = fullSignificand;
    }

    // Step 2: Determine the exact target interval
    significand <<= 2;
    static const uint16_t halfUlp = 2;
    uint32_t upperMidpointExact = significand + halfUlp;

    static const uint16_t quarterUlp = 1;
    uint32_t lowerMidpointExact
      = significand - (isBoundary ? quarterUlp : halfUlp);

    // Shortest output from here is "1.0" plus null byte
    if (length < 4) {
      dest[0] = '\0';
      return 0;
    }

    char *p = dest;
    if (negative) {
      *p++ = '-';
    }

    if (binaryExponent < -13 || (binaryExponent == -13 && significand < 0x1a38)) {
      // Format values < 10^-5 as exponential form
      // We know value < 10^-5, so we can do the first scaling step unconditionally
      int decimalExponent = -5;
      uint32_t u = (upperMidpointExact << (28 - 13 + binaryExponent)) * 100000;
      uint32_t l = (lowerMidpointExact << (28 - 13 + binaryExponent)) * 100000;
      uint32_t t = (significand << (28 - 13 + binaryExponent)) * 100000;
      const uint32_t mask = (1 << 28) - 1;
      if (t < ((1 << 28) / 10)) {
        u *= 100; l *= 100; t *= 100;
        decimalExponent -= 2;
      }
      if (t < (1 << 28)) {
        u *= 10; l *= 10; t *= 10;
        decimalExponent -= 1;
      }
      const int uDigit = u >> 28, lDigit = l >> 28;
      if (uDigit == lDigit) {
        // There's more than one digit, emit a '.' and the rest
        if (p > dest + length - 6) {
          dest[0] = '\0';
          return 0;
        }
        *p++ = (t >> 28) + '0';
        *p++ = '.';
        while (true) {
          u = (u & mask) * 10; l = (l & mask) * 10;
          const int uDigit = u >> 28, lDigit = l >> 28;
          if (uDigit != lDigit) {
            t = (t & mask) * 10;
            break;
          }
          t *= 10;
          *p++ = uDigit + '0';
        }
      }
      t = (t + (1 << 27)) >> 28; // Add 1/2 to round
      if (p > dest + length - 6) { // Exactly 6 bytes written below
        dest[0] = '\0';
        return 0;
      }
      *p++ = t + '0';
      memcpy(p, "e-", 2);
      p += 2;
      memcpy(p, asciiDigitTable + (-decimalExponent) * 2, 2);
      p += 2;
      *p = '\0';
      return p - dest;
    }

    // Format the value using decimal format

    // There's an integer portion of no more than 5 digits
    int intportion;
    if (binaryExponent < 13) {
      intportion = significand >> (13 - binaryExponent);
      significand -= intportion << (13 - binaryExponent);
    } else {
      intportion = significand << (binaryExponent - 13);
      significand -= intportion >> (binaryExponent - 13);
    }
    if (intportion < 10) {
      if (p > dest + length - 3) {
        dest[0] = '\0';
        return 0;
      }
      *p++ = intportion + '0'; // One digit is the most common case
    } else if (intportion < 1000) {
      // 2 or 3 digits
      if (p > dest + length - 4) {
        dest[0] = '\0';
        return 0;
      }
      if (intportion > 99) {
        *p++ = intportion / 100 + '0';
      }
      memcpy(p, asciiDigitTable + (intportion % 100) * 2, 2);
      p += 2;
    } else {
      // 4 or 5 digits
      if (p > dest + length - 6) {
        dest[0] = '\0';
        return 0;
      }
      if (intportion > 9999) {
        *p++ = intportion / 10000 + '0';
        intportion %= 10000;
      }
      memcpy(p, asciiDigitTable + (intportion / 100) * 2, 2);
      memcpy(p + 2, asciiDigitTable + (intportion % 100) * 2, 2);
      p += 4;
    }
    if (p > dest + length - 3) {
      dest[0] = '\0';
      return 0;
    }
    *p++ = '.';
    if (significand == 0) { // No fraction, so we're done.
      *p++ = '0';
      *p = '\0';
      return p - dest;
    }

    // Format the fractional part
    uint32_t u = upperMidpointExact << (28 - 13 + binaryExponent);
    uint32_t l = lowerMidpointExact << (28 - 13 + binaryExponent);
    uint32_t t = significand << (28 - 13 + binaryExponent);
    const uint32_t mask = (1 << 28) - 1;
    unsigned uDigit, lDigit;
    while (true) {
      u = (u & mask) * 10; l = (l & mask) * 10;
      uDigit = u >> 28; lDigit = l >> 28;
      if (uDigit != lDigit) {
        t = (t & mask) * 10;
        break;
      }
      t *= 10;
      if (p > dest + length - 3) {
        dest[0] = '\0';
        return 0;
      }
      *p++ = uDigit + '0';
    }
    t += 1 << 27; // Add 1/2
    if ((t & mask) == 0) { // Was exactly 1/2 (now zero)
      t = (t >> 28) & ~1; // Round even
    } else {
      t >>= 28;
    }
    if (t <= lDigit && l > 0)
      t += 1;
    *p++ = t + '0';
    *p = '\0';
    return p - dest;
}
#endif

// ================================================================
//
// BINARY32
//
// ================================================================


#if SWIFT_DTOA_BINARY32_SUPPORT
#if FLOAT_IS_BINARY32
// Format a C `float`
size_t swift_dtoa_optimal_float(float d, char *dest, size_t length) {
  return swift_dtoa_optimal_binary32_p(&d, dest, length);
}
#endif

// Format an IEEE 754 single-precision binary32 format floating-point number.
size_t swift_dtoa_optimal_binary32_p(const void *f, char *dest, size_t length)
{
    static const int significandBitCount = FLT_MANT_DIG - 1;
    static const uint32_t significandMask
        = ((uint32_t)1 << significandBitCount) - 1;
    static const int exponentBitCount = 8;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_dtoa_optimal_binary64_p
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 125

    // Step 0: Deconstruct the target number
    // Note: this strongly assumes IEEE 754 binary32 format
    uint32_t raw = *(const uint32_t *)f;
    int exponentBitPattern = (raw >> significandBitCount) & exponentMask;
    uint32_t significandBitPattern = raw & significandMask;
    int negative = raw >> 31;

    // Step 1: Handle the various input cases:
    int binaryExponent;
    uint32_t significand;
    if (length < 1) {
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
      if (significandBitPattern == 0) { // Infinity
        return infinity(dest, length, negative);
      } else { // NaN
        const int quiet = (significandBitPattern >> (significandBitCount - 1)) & 1;
        uint32_t payload = raw & ((1UL << (significandBitCount - 2)) - 1);
        return nan_details(dest, length, negative, quiet != 0, 0, payload);
      }
    } else if (exponentBitPattern == 0) {
        if (significandBitPattern == 0) { // Zero
          return zero(dest, length, negative);
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
    uint64_t upperMidpointExact = (uint64_t)(significand + halfUlp);

    int isBoundary = significandBitPattern == 0;
    static const uint32_t quarterUlp = halfUlp >> 1;
    uint64_t lowerMidpointExact
        = (uint64_t)(significand - (isBoundary ? quarterUlp : halfUlp));

    // Step 3: Estimate the base 10 exponent
    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor
    uint64_t powerOfTenRoundedDown = 0;
    uint64_t powerOfTenRoundedUp = 0;
    int powerOfTenExponent = 0;
    static const int bulkFirstDigits = 1;
    intervalContainingPowerOf10_Binary32(-base10Exponent + bulkFirstDigits - 1,
                                      &powerOfTenRoundedDown,
                                      &powerOfTenRoundedUp,
                                      &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)
    static const int integerBits = 8;
    const int shift = integerBits - extraBits;
    const int roundUpBias = (1 << shift) - 1;
    static const int fractionBits = 64 - integerBits;
    static const uint64_t fractionMask = ((uint64_t)1 << fractionBits) - (uint64_t)1;
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
    uint64_t t = u;
    uint64_t delta = u - l;
    while (t < (uint64_t)1 << fractionBits) {
      base10Exponent -= 1;
      t *= 10;
      delta *= 10;
    }

    // Step 7: Generate decimal digits into the destination buffer
    char *p = dest;
    if (p > dest + length - 3) {
      dest[0] = '\0';
      return 0;
    }
    if (negative) {
      *p++ = '-';
    }
    char * const firstOutputChar = p;
    // Format first digit as a 2-digit value to get a leading '0'
    memcpy(p, asciiDigitTable + (t >> fractionBits) * 2, 2);
    t &= fractionMask;
    p += 2;

    // Emit two digits at a time
    while ((delta * 10) < ((t * 10) & fractionMask)) {
      if (p > dest + length - 3) {
        dest[0] = '\0';
        return 0;
      }
      delta *= 100;
      t *= 100;
      memcpy(p, asciiDigitTable + (t >> fractionBits) * 2, 2);
      t &= fractionMask;
      p += 2;
    }

    // Emit any final digit
    if (delta < t) {
      if (p > dest + length - 2) {
        dest[0] = '\0';
        return 0;
      }
      delta *= 10;
      t *= 10;
      *p++ = '0' + (t >> fractionBits);
      t &= fractionMask;
    }

    // Adjust the final digit to be closer to the original value
    if (delta > t + ((uint64_t)1 << fractionBits)) {
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
            p[-1] -= adjust;
            p[-1] &= ~1;
        } else {
            // Else round to nearest...
            int adjust = (int)((skew + oneHalf) >> (64 - integerBits));
            p[-1] -= adjust;
        }
    }

    int forceExponential = binaryExponent > 25 || (binaryExponent == 25 && !isBoundary);
    return finishFormatting(dest, length, p, firstOutputChar, forceExponential, base10Exponent);
}
#endif


// ================================================================
//
// BINARY64
//
// ================================================================

#if SWIFT_DTOA_BINARY64_SUPPORT
#if LONG_DOUBLE_IS_BINARY64
size_t swift_dtoa_optimal_long_double(long double d, char *dest, size_t length) {
  return swift_dtoa_optimal_binary64_p(&d, dest, length);
}
#endif
#if DOUBLE_IS_BINARY64
size_t swift_dtoa_optimal_double(double d, char *dest, size_t length) {
  return swift_dtoa_optimal_binary64_p(&d, dest, length);
}
#endif

// Format an IEEE 754 double-precision binary64 format floating-point number.

// The calling convention here assumes that C `double` is this format,
// but otherwise, this does not utilize any floating-point arithmetic
// or library routines.
size_t swift_dtoa_optimal_binary64_p(const void *d, char *dest, size_t length)
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
    // pure fraction, so we need to accommodate that when
    // reconstructing the binary exponent.
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 1022

    // Step 0: Deconstruct an IEEE 754 binary64 double-precision value
    uint64_t raw = *(const uint64_t *)d;
    int exponentBitPattern = (raw >> significandBitCount) & exponentMask;
    uint64_t significandBitPattern = raw & significandMask;
    int negative = raw >> 63;

    // Step 1: Handle the various input cases:
    if (length < 1) {
      return 0;
    }
    int binaryExponent;
    int isBoundary = significandBitPattern == 0;
    uint64_t significand;
    if (exponentBitPattern == exponentMask) { // NaN or Infinity
        if (isBoundary) { // Infinity
            return infinity(dest, length, negative);
        } else {
            const int quiet = (raw >> (significandBitCount - 1)) & 1;
            uint64_t payload = raw & ((1ull << (significandBitCount - 2)) - 1);
            return nan_details(dest, length, negative, quiet, 0, payload);
        }
    } else if (exponentBitPattern == 0) {
        if (isBoundary) { // Zero
          return zero(dest, length, negative);
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
    // interval, we start by computing the midpoints between this
    // floating-point value and the adjacent ones.  Note that this
    // step is an exact computation.

    uint64_t halfUlp = (uint64_t)1 << (64 - significandBitCount - 2);
    uint64_t quarterUlp = halfUlp >> 1;
    uint64_t upperMidpointExact = significand + halfUlp;

    uint64_t lowerMidpointExact
        = significand - (isBoundary ? quarterUlp : halfUlp);

    int isOddSignificand = (significandBitPattern & 1) != 0;

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

    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor

    // Compute `10^-p` to 128-bit precision.  We generate
    // both over- and under-estimates to ensure we can exactly
    // bound the later use of these values.
    swift_uint128_t powerOfTenRoundedDown;
    swift_uint128_t powerOfTenRoundedUp;
    int powerOfTenExponent = 0;
    static const int bulkFirstDigits = 7;
    static const int bulkFirstDigitFactor = 1000000; // 10^(bulkFirstDigits - 1)
    // Note the extra factor of 10^bulkFirstDigits -- that will give
    // us a headstart on digit generation later on.  (In contrast, Ryu
    // uses an extra factor of 10^17 here to get all the digits up
    // front, but then has to back out any extra digits.  Doing that
    // with a 17-digit value requires 64-bit division, which is the
    // root cause of Ryu's poor performance on 32-bit processors.  We
    // also might have to back out extra digits if 7 is too many, but
    // will only need 32-bit division in that case.)
    intervalContainingPowerOf10_Binary64(-base10Exponent + bulkFirstDigits - 1,
                                       &powerOfTenRoundedDown,
                                       &powerOfTenRoundedUp,
                                       &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)

    // As mentioned above, the final digit generation works
    // with an interval, so we actually apply the scaling
    // to the upper and lower midpoint values separately.

    // As part of the scaling here, we'll switch from a pure
    // fraction with zero bit integer portion and 128-bit fraction
    // to a fixed-point form with 32 bits in the integer portion.
    static const int integerBits = 32;

    // We scale the interval in one of two different ways,
    // depending on whether the significand is even or odd...

    swift_uint128_t u, l;
    if (isOddSignificand) {
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

        // So when the significand is even, we round so as to widen
        // the interval in order to ensure that the exact midpoints
        // are considered.  Of couse, this ensures that we find a
        // short result but carries a risk of selecting a result
        // outside of the exact scaled interval (which would be
        // inaccurate).

        // The same testing approach described above (based on results
        // in the Errol paper) also applies
        // to this case.

        swift_uint128_t u1 = multiply128x64RoundingUp(powerOfTenRoundedUp,
                                                  upperMidpointExact);
        u = shiftRightRoundingUp128(u1, integerBits - extraBits);

        swift_uint128_t l1 = multiply128x64RoundingDown(powerOfTenRoundedDown,
                                                    lowerMidpointExact);
        l = shiftRightRoundingDown128(l1, integerBits - extraBits);
    }

    // Step 6: Align first digit, adjust exponent

    // Calculations above used an estimate for the power-of-ten scale.
    // Here, we compensate for any error in that estimate by testing
    // whether we have the expected number of digits in the integer
    // portion and correcting as necessary.  This also serves to
    // prune leading zeros from subnormals.

    // Except for subnormals, this loop should never run more than once.
    // For subnormals, this might run as many as 16 + bulkFirstDigits
    // times.
#if HAVE_UINT128_T
    while (u < ((__uint128_t)bulkFirstDigitFactor << (128 - integerBits)))
#else
    while (u.high < ((uint32_t)bulkFirstDigitFactor << (32 - integerBits)))
#endif
    {
        base10Exponent -= 1;
        multiply128xu32(&l, 10);
        multiply128xu32(&u, 10);
    }

    // Step 7: Produce decimal digits

    // One standard approach generates digits for the scaled upper and
    // lower boundaries and stops when at the first digit that
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

    // Generate digits for `t` with interval width `delta = u - l`
    swift_uint128_t t = u;
    swift_uint128_t delta = u;
    subtract128x128(&delta, l);

    char *p = dest;
    if (negative) {
      if (p >= dest + length) {
        dest[0] = '\0';
        return 0;
      }
      *p++ = '-';
    }
    char * const firstOutputChar = p;

    // The `bulkFirstDigits` adjustment above already set up the first 7 digits
    // Format as 8 digits (with a leading zero that we'll exploit later on).
    uint32_t d12345678 = extractIntegerPart128(&t, integerBits);

    if (!isLessThan128x128(delta, t)) {
      // Oops!  We have too many digits.  Back out the extra ones to
      // get the right answer.  This is similar to Ryu, but since
      // we've only produced seven digits, we only need 32-bit
      // arithmetic here.  A few notes:
      // * Our target hardware always supports 32-bit hardware division,
      //   so this should be reasonably fast.
      // * For small integers (like "2"), Ryu would have to back out 16
      //   digits; we only have to back out 6.
      // * Very few double-precision values actually need fewer than 7
      //   digits.  So this is rarely used except in workloads that
      //   specifically use double for small integers.  This is more
      //   common for binary32, of course.

      // TODO: Add benchmarking for "small integers" -1000...1000 to
      // verify that this does not unduly penalize those values.

      // Why this is critical for performance: In order to use the
      // 8-digits-at-a-time optimization below, we need at least 30
      // bits in the integer part of our fixed-point format above.  If
      // we only use bulkDigits = 1, that leaves only 128 - 30 = 98
      // bit accuracy for our scaling step, which isn't enough
      // (binary64 needs ~110 bits for correctness).  So we have to
      // use a large bulkDigits value to make full use of the 128-bit
      // scaling above, which forces us to have some form of logic to
      // handle the case of too many digits.  The alternatives are to
      // use >128 bit values (slower) or do some complex finessing of
      // bit counts by working with powers of 5 instead of 10.

#if HAVE_UINT128_T
      uint64_t uHigh = u >> 64;
      uint64_t lHigh = l >> 64;
      if (0 != (uint64_t)l) {
        lHigh += 1;
      }
#else
      uint64_t uHigh = ((uint64_t)u.high << 32) + u.c;
      uint64_t lHigh = ((uint64_t)l.high << 32) + l.c;
      if (0 != (l.b | l.low)) {
        lHigh += 1;
      }
#endif
      uint64_t tHigh;
      if (isBoundary) {
        tHigh = (uHigh + lHigh * 2) / 3;
      } else {
        tHigh = (uHigh + lHigh) / 2;
      }

      uint32_t u0 = uHigh >> (64 - integerBits);
      uint32_t l0 = lHigh >> (64 - integerBits);
      if ((lHigh & ((1ULL << (64 - integerBits)) - 1)) != 0) {
        l0 += 1;
      }
      uint32_t t0 = tHigh >> (64 - integerBits);
      int t0digits = 8;

      uint32_t u1 = u0 / 10;
      uint32_t l1 = (l0 + 9) / 10;
      int trailingZeros = is128bitZero(t);
      int droppedDigit = ((tHigh * 10) >> (64 - integerBits)) % 10;
      while (u1 >= l1 && u1 != 0) {
        u0 = u1;
        l0 = l1;
        trailingZeros &= droppedDigit == 0;
        droppedDigit = t0 % 10;
        t0 /= 10;
        t0digits--;
        u1 = u0 / 10;
        l1 = (l0 + 9) / 10;
      }
      // Correct the final digit
      if (droppedDigit > 5 || (droppedDigit == 5 && !trailingZeros)) {
        t0 += 1;
      } else if (droppedDigit == 5 && trailingZeros) {
        t0 += 1;
        t0 &= ~1;
      }
      // t0 has t0digits digits.  Write them out
      if (p > dest + length - t0digits - 1) { // Make sure we have space
        dest[0] = '\0';
        return 0;
      }
      int i = t0digits;
      while (i > 1) { // Write out 2 digits at a time back-to-front
        i -= 2;
        memcpy(p + i, asciiDigitTable + (t0 % 100) * 2, 2);
        t0 /= 100;
      }
      if (i > 0) { // Handle an odd number of digits
        p[0] = t0 + '0';
      }
      p += t0digits; // Move the pointer past the digits we just wrote
    } else {
      //
      // Our initial scaling did not produce too many digits.
      // The `d12345678` value holds the first 7 digits (plus
      // a leading zero that will be useful later).  We write
      // those out and then incrementally generate as many
      // more digits as necessary.  The remainder of this
      // algorithm is basically just Grisu2.
      //

      if (p > dest + length - 9) {
        dest[0] = '\0';
        return 0;
      }
      // Write out the 7 digits we got earlier + leading zero
      int d1234 = d12345678 / 10000;
      int d5678 = d12345678 % 10000;
      int d78 = d5678 % 100;
      int d56 = d5678 / 100;
      memcpy(p + 6, asciiDigitTable + d78 * 2, 2);
      memcpy(p + 4, asciiDigitTable + d56 * 2, 2);
      int d34 = d1234 % 100;
      int d12 = d1234 / 100;
      memcpy(p + 2, asciiDigitTable + d34 * 2, 2);
      memcpy(p, asciiDigitTable + d12 * 2, 2);
      p += 8;

      // Seven digits wasn't enough, so let's get some more.
      // Most binary64 values need >= 15 digits total.  We already have seven,
      // so try grabbing the next 8 digits all at once.
      // (This is suboptimal for binary32, but the code savings
      // from sharing this implementation are worth it.)
      static const uint32_t bulkDigitFactor = 100000000; // 10^(15-bulkFirstDigits)
      swift_uint128_t d0 = delta;
      multiply128xu32(&d0, bulkDigitFactor);
      swift_uint128_t t0 = t;
      multiply128xu32(&t0, bulkDigitFactor);
      int bulkDigits = extractIntegerPart128(&t0, integerBits); // 9 digits
      if (isLessThan128x128(d0, t0)) {
        if (p > dest + length - 9) {
          dest[0] = '\0';
          return 0;
        }
        // Next 8 digits are good; add them to the output
        int d1234 = bulkDigits / 10000;
        int d5678 = bulkDigits % 10000;
        int d78 = d5678 % 100;
        int d56 = d5678 / 100;
        memcpy(p + 6, asciiDigitTable + d78 * 2, 2);
        memcpy(p + 4, asciiDigitTable + d56 * 2, 2);
        int d34 = d1234 % 100;
        int d12 = d1234 / 100;
        memcpy(p + 2, asciiDigitTable + d34 * 2, 2);
        memcpy(p, asciiDigitTable + d12 * 2, 2);
        p += 8;

        t = t0;
        delta = d0;
      }

      // Finish up by generating and writing one digit at a time.
      while (isLessThan128x128(delta, t)) {
        if (p > dest + length - 2) {
          dest[0] = '\0';
          return 0;
        }
        multiply128xu32(&delta, 10);
        multiply128xu32(&t, 10);
        *p++ = '0' + extractIntegerPart128(&t, integerBits);
      }

      // Adjust the final digit to be closer to the original value.  This accounts
      // for the fact that sometimes there is more than one shortest digit
      // sequence.

      // For example, consider how the above would work if you had the
      // value 0.1234 and computed u = 0.1257, l = 0.1211.  The above
      // digit generation works with `u`, so produces 0.125.  But the
      // values 0.122, 0.123, and 0.124 are just as short and 0.123 is
      // therefore the best choice, since it's closest to the original
      // value.

      // We know delta and t are both less than 10.0 here, so we can
      // shed some excess integer bits to simplify the following:
      const int adjustIntegerBits = 4; // Integer bits for "adjust" phase
      shiftLeft128(&delta, integerBits - adjustIntegerBits);
      shiftLeft128(&t, integerBits - adjustIntegerBits);

      // Note: We've already consumed most of our available precision,
      // so it's okay to just work in 64 bits for this...
      uint64_t deltaHigh64 = extractHigh64From128(delta);
      uint64_t tHigh64 = extractHigh64From128(t);

      // If `delta < t + 1.0`, then the interval is narrower than
      // one decimal digit, so there is no other option.
      if (deltaHigh64 >= tHigh64 + ((uint64_t)1 << (64 - adjustIntegerBits))) {
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
        uint64_t one = (uint64_t)(1) << (64 - adjustIntegerBits);
        uint64_t fractionMask = one - 1;
        uint64_t oneHalf = one >> 1;
        if ((skew & fractionMask) == oneHalf) {
          int adjust = (int)(skew >> (64 - adjustIntegerBits));
          // If the skew is exactly integer + 1/2, round the
          // last digit even after adjustment
          p[-1] -= adjust;
          p[-1] &= ~1;
        } else {
          // Else round to nearest...
          int adjust = (int)((skew + oneHalf) >> (64 - adjustIntegerBits));
          p[-1] -= adjust;
        }
      }
    }

    // Step 8: Shuffle digits into the final textual form
    int forceExponential = binaryExponent > 54 || (binaryExponent == 54 && !isBoundary);
    return finishFormatting(dest, length, p, firstOutputChar, forceExponential, base10Exponent);
}
#endif

// ================================================================
//
// FLOAT80
//
// ================================================================

#if SWIFT_DTOA_FLOAT80_SUPPORT
#if LONG_DOUBLE_IS_FLOAT80
size_t swift_dtoa_optimal_long_double(long double d, char *dest, size_t length) {
  return swift_dtoa_optimal_float80_p(&d, dest, length);
}
#endif

// Format an Intel x87 80-bit extended precision floating-point format
// This does not rely on the C environment for floating-point arithmetic
// or library support of any kind.
size_t swift_dtoa_optimal_float80_p(const void *d, char *dest, size_t length)
{
    static const int exponentBitCount = 15;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_dtoa_optimal_binary64_p to understand
    // why we use 16,382 instead of 16,383 here.
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 16,382

    // Step 0: Deconstruct the target number
    // Note: this strongly assumes Intel 80-bit extended format in LSB
    // byte order
    const uint64_t *raw_p = (const uint64_t *)d;
    int exponentBitPattern = raw_p[1] & exponentMask;
    int negative = (raw_p[1] >> 15) & 1;
    uint64_t significandBitPattern = raw_p[0];

    // Step 1: Handle the various input cases:
    int64_t binaryExponent;
    uint64_t significand;
    int isBoundary = (significandBitPattern & 0x7fffffffffffffff) == 0;
    if (length < 1) {
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
      // Following 80387 semantics as documented in Wikipedia.org "Extended Precision"
      // Also see Intel's "Floating Point Reference Sheet"
      // https://software.intel.com/content/dam/develop/external/us/en/documents/floating-point-reference-sheet.pdf
      int selector = significandBitPattern >> 62; // Top 2 bits
      uint64_t payload = significandBitPattern & (((uint64_t)1 << 62) - 1); // bottom 62 bits
      switch (selector) {
      case 0: // ∞ or snan on 287, invalid on 387
      case 1: // Pseudo-NaN: snan on 287, invalid on 387
        break;
      case 2:
        if (payload == 0) { // snan on 287, ∞ on 387
          return infinity(dest, length, negative);
        } else { // snan on 287 and 387
          return nan_details(dest, length, negative, 0 /* quiet */, 0, payload);
        }
        break;
      case 3:
        // Zero payload and sign bit set is "indefinite" (treated as qNaN here),
        // Otherwise qNan on 387, sNaN on 287
        return nan_details(dest, length, negative, 1 /* quiet */, 0, payload);
      }
      // Handle "invalid" patterns as plain "nan"
      return nan_details(dest, length, 0 /* negative */, 1 /* quiet */, 0, payload);
    } else if (exponentBitPattern == 0) {
        if (significandBitPattern == 0) { // Zero
          return zero(dest, length, negative);
        } else { // subnormal
            binaryExponent = 1 - exponentBias;
            significand = significandBitPattern;
        }
    } else if (significandBitPattern >> 63) { // Normal
        binaryExponent = exponentBitPattern - exponentBias;
        significand = significandBitPattern;
    } else {
        // Invalid pattern rejected by 80387 and later.
        // Handle "invalid" patterns as plain "nan"
        return nan_details(dest, length, 0 /* negative */, 1 /* quiet */, 0, 0);
    }

    // Step 2: Determine the exact unscaled target interval
    uint64_t halfUlp = (uint64_t)1 << 63;
    uint64_t quarterUlp = halfUlp >> 1;
    uint64_t threeQuarterUlp = halfUlp + quarterUlp;
    swift_uint128_t upperMidpointExact, lowerMidpointExact;
    initialize128WithHighLow64(upperMidpointExact, significand, halfUlp);
    // Subtract 1/4 or 1/2 ULP by first subtracting 1 full ULP, then adding some back
    initialize128WithHighLow64(lowerMidpointExact, significand - 1, isBoundary ? threeQuarterUlp : halfUlp);

    return _swift_dtoa_256bit_backend
      (
       dest,
       length,
       upperMidpointExact,
       lowerMidpointExact,
       negative,
       isBoundary,
       (significandBitPattern & 1) != 0,
       binaryExponent,
       binaryExponent > 65 || (binaryExponent == 65 && !isBoundary)  // forceExponential
       );

}
#endif

// ================================================================
//
// BINARY128
//
// ================================================================

#if SWIFT_DTOA_BINARY128_SUPPORT
#if LONG_DOUBLE_IS_BINARY128
size_t swift_dtoa_optimal_long_double(long double d, char *dest, size_t length) {
  return swift_dtoa_optimal_binary128_p(&d, dest, length);
}
#endif

// Format an IEEE 754 binary128 quad-precision floating-point number.
// This does not rely on the C environment for floating-point arithmetic
// or library support of any kind.
size_t swift_dtoa_optimal_binary128_p(const void *d, char *dest, size_t length)
{
    static const int exponentBitCount = 15;
    static const int exponentMask = (1 << exponentBitCount) - 1;
    // See comments in swift_dtoa_optimal_binary64_p to understand
    // why we use 16,382 instead of 16,383 here.
    static const int64_t exponentBias = (1 << (exponentBitCount - 1)) - 2; // 16,382

    // Step 0: Deconstruct the target number in IEEE 754 binary128 LSB format
    const uint64_t *raw_p = (const uint64_t *)d;
    int exponentBitPattern = (raw_p[1] >> 48) & exponentMask;
    int negative = (raw_p[1] >> 63) & 1;
    uint64_t significandHigh = raw_p[1] & 0xffffffffffffULL;
    uint64_t significandLow = raw_p[0];

    // Step 1: Handle the various input cases:
    int64_t binaryExponent;
    int isBoundary = (significandLow == 0) && (significandHigh == 0);
    if (length < 1) {
        return 0;
    } else if (exponentBitPattern == exponentMask) { // NaN or Infinity
      if (isBoundary) { // Infinity
        return infinity(dest, length, negative);
      } else { // NaN
        int signaling = (significandHigh >> 47) & 1;
        uint64_t payloadHigh = significandHigh & 0x3fffffffffffULL;
        uint64_t payloadLow = significandLow;
        return nan_details(dest, length, negative, signaling == 0, payloadHigh, payloadLow);
      }
    } else if (exponentBitPattern == 0) {
        if (isBoundary) { // Zero
          return zero(dest, length, negative);
        } else { // subnormal
            binaryExponent = 1 - exponentBias;
        }
    } else { // Normal
        binaryExponent = exponentBitPattern - exponentBias;
        significandHigh |= (1ULL << 48);
    }
    // Align significand to 0.113 fractional form
    significandHigh <<= 15;
    significandHigh |= significandLow >> (64 - 15);
    significandLow <<= 15;

    // Step 2: Determine the exact unscaled target interval
    uint64_t halfUlp = (uint64_t)1 << 14;
    uint64_t quarterUlp = halfUlp >> 1;
    swift_uint128_t upperMidpointExact, lowerMidpointExact;
    initialize128WithHighLow64(upperMidpointExact, significandHigh, significandLow + halfUlp);
    // Subtract 1/4 or 1/2 ULP
    if (significandLow == 0) {
      initialize128WithHighLow64(lowerMidpointExact,
                                 significandHigh - 1,
                                 significandLow - (isBoundary ? quarterUlp : halfUlp));
    } else {
      initialize128WithHighLow64(lowerMidpointExact,
                                 significandHigh,
                                 significandLow - (isBoundary ? quarterUlp : halfUlp));
    }

    return _swift_dtoa_256bit_backend
      (
       dest,
       length,
       upperMidpointExact,
       lowerMidpointExact,
       negative,
       isBoundary,
       (significandLow & 0x8000) != 0,
       binaryExponent,
       binaryExponent > 114 || (binaryExponent == 114 && !isBoundary)  // forceExponential
       );
}
#endif

// ================================================================
//
// FLOAT80/BINARY128 common backend
//
// This uses 256-bit fixed-width arithmetic to efficiently compute the
// optimal form for a decomposed float80 or binary128 value.  It is
// less heavily commented than the 128-bit version above; see that
// implementation for detailed explanation of the logic here.
//
// This sacrifices some performance for float80, which can be done
// more efficiently with 192-bit fixed-width arithmetic.  But the code
// size savings from sharing this logic between float80 and binary128
// are substantial, and the resulting float80 performance is still much
// better than most competing implementations.
//
// Also in the interest of code size savings, this eschews some of the
// optimizations used by the 128-bit backend above.  Those
// optimizations are simple to reintroduce if you're interested in
// further performance improvements.
//
// If you are interested in extreme code size, you can also use this
// backend for binary32 and binary64, eliminating the separate 128-bit
// implementation. That variation offers surprisingly reasonable
// performance overall.
//
// ================================================================

#if SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
static size_t _swift_dtoa_256bit_backend
(
 char *dest,
 size_t length,
 swift_uint128_t upperMidpointExact,
 swift_uint128_t lowerMidpointExact,
 int negative,
 int isBoundary,
 int isOddSignificand,
 int binaryExponent,
 bool forceExponential
)
{
    // Step 3: Estimate the base 10 exponent
    int base10Exponent = decimalExponentFor2ToThe(binaryExponent);

    // Step 4: Compute a power-of-10 scale factor
    swift_uint256_t powerOfTenRoundedDown;
    swift_uint256_t powerOfTenRoundedUp;
    int powerOfTenExponent = 0;
    intervalContainingPowerOf10_Binary128(-base10Exponent,
                                        &powerOfTenRoundedDown,
                                        &powerOfTenRoundedUp,
                                        &powerOfTenExponent);
    const int extraBits = binaryExponent + powerOfTenExponent;

    // Step 5: Scale the interval (with rounding)
    static const int integerBits = 14; // Enough for 4 decimal digits
#if HAVE_UINT128_T
    static const int highFractionBits = 64 - integerBits;
#else
    static const int highFractionBits = 32 - integerBits;
#endif
    swift_uint256_t u, l;
    if (isOddSignificand) {
        // Narrow the interval (odd significand)
        u = powerOfTenRoundedDown;
        multiply256x128RoundingDown(&u, upperMidpointExact);
        shiftRightRoundingDown256(&u, integerBits - extraBits);

        l = powerOfTenRoundedUp;
        multiply256x128RoundingUp(&l, lowerMidpointExact);
        shiftRightRoundingUp256(&l, integerBits - extraBits);
    } else {
        // Widen the interval (even significand)
        u = powerOfTenRoundedUp;
        multiply256x128RoundingUp(&u, upperMidpointExact);
        shiftRightRoundingUp256(&u, integerBits - extraBits);

        l = powerOfTenRoundedDown;
        multiply256x128RoundingDown(&l, lowerMidpointExact);
        shiftRightRoundingDown256(&l, integerBits - extraBits);
    }

    // Step 6: Align first digit, adjust exponent
#if HAVE_UINT128_T
    while (u.high < (uint64_t)1 << highFractionBits)
#else
    while (u.elt[7] < (uint64_t)1 << highFractionBits)
#endif
    {
        base10Exponent -= 1;
        multiply256xu32(&l, 10);
        multiply256xu32(&u, 10);
    }

    swift_uint256_t t = u;
    swift_uint256_t delta = u;
    subtract256x256(&delta, l);

    // Step 7: Generate digits
    char *p = dest;
    if (p > dest + length - 4) { // Shortest output is "1.0" (4 bytes)
      dest[0] = '\0';
      return 0;
    }
    if (negative) {
      *p++ = '-';
    }
    char * const firstOutputChar = p;

    // Adjustment above already set up the first digit
    *p++ = '0';
    *p++ = '0' + extractIntegerPart256(&t, integerBits);

    // Generate 4 digits at a time
    swift_uint256_t d0 = delta;
    multiply256xu32(&d0, 10000);
    swift_uint256_t t0 = t;
    multiply256xu32(&t0, 10000);
    int d1234 = extractIntegerPart256(&t0, integerBits);
    while (isLessThan256x256(d0, t0)) {
      if (p > dest + length - 5) {
        dest[0] = '\0';
        return 0;
      }
      int d34 = d1234 % 100;
      int d12 = d1234 / 100;
      memcpy(p + 2, asciiDigitTable + d34 * 2, 2);
      memcpy(p, asciiDigitTable + d12 * 2, 2);
      p += 4;
      t = t0;
      delta = d0;
      multiply256xu32(&d0, 10000);
      multiply256xu32(&t0, 10000);
      d1234 = extractIntegerPart256(&t0, integerBits);
    }

    // Generate one digit at a time...
    while (isLessThan256x256(delta, t)) {
      if (p > dest + length - 2) {
        dest[0] = '\0';
        return 0;
      }
      multiply256xu32(&delta, 10);
      multiply256xu32(&t, 10);
      *p++ = extractIntegerPart256(&t, integerBits) + '0';
    }

    // Adjust the final digit to be closer to the original value
    // We've already consumed most of our available precision, and only
    // need a couple of integer bits, so we can narrow down to
    // 64 bits here.
#if HAVE_UINT128_T
    uint64_t deltaHigh64 = delta.high;
    uint64_t tHigh64 = t.high;
#else
    uint64_t deltaHigh64 = ((uint64_t)delta.elt[7] << 32) + delta.elt[6];
    uint64_t tHigh64 = ((uint64_t)t.elt[7] << 32) + t.elt[6];
#endif
    if (deltaHigh64 >= tHigh64 + ((uint64_t)1 << (64 - integerBits))) {
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
        p[-1] -= adjust;
        p[-1] &= ~1;
      } else {
        // Else round to nearest...
        int adjust = (int)((skew + oneHalf) >> (64 - integerBits));
        p[-1] -= adjust;
      }
    }

    return finishFormatting(dest, length, p, firstOutputChar, forceExponential, base10Exponent);
}
#endif

#if SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
static int finishFormatting(char *dest, size_t length,
                     char *p,
                     char *firstOutputChar,
                     int forceExponential,
                     int base10Exponent)
{
    int digitCount = p - firstOutputChar - 1;
    if (base10Exponent < -4 || forceExponential) {
      // Exponential form: convert "0123456" => "1.23456e78"
      firstOutputChar[0] = firstOutputChar[1];
      if (digitCount > 1) {
        firstOutputChar[1] = '.';
      } else {
        p--;
      }
      // Add exponent at the end
      if (p > dest + length - 5) {
        dest[0] = '\0';
        return 0;
      }
      *p++ = 'e';
      if (base10Exponent < 0) {
        *p++ = '-';
        base10Exponent = -base10Exponent;
      } else {
        *p++ = '+';
      }
      if (base10Exponent > 99) {
        if (base10Exponent > 999) {
          if (p > dest + length - 5) {
            dest[0] = '\0';
            return 0;
          }
          memcpy(p, asciiDigitTable + (base10Exponent / 100) * 2, 2);
          p += 2;
        } else {
          if (p > dest + length - 4) {
            dest[0] = '\0';
            return 0;
          }
          *p++ = (base10Exponent / 100) + '0';
        }
        base10Exponent %= 100;
      }
      memcpy(p, asciiDigitTable + base10Exponent * 2, 2);
      p += 2;
    } else if (base10Exponent < 0) { // "0123456" => "0.00123456"
      // Slide digits back in buffer and prepend zeros and a period
      if (p > dest + length + base10Exponent - 1) {
        dest[0] = '\0';
        return 0;
      }
      memmove(firstOutputChar - base10Exponent, firstOutputChar, p - firstOutputChar);
      memset(firstOutputChar, '0', -base10Exponent);
      firstOutputChar[1] = '.';
      p += -base10Exponent;
    } else if (base10Exponent + 1 < digitCount) { // "0123456" => "123.456"
      // Slide integer digits forward and insert a '.'
      memmove(firstOutputChar, firstOutputChar + 1, base10Exponent + 1);
      firstOutputChar[base10Exponent + 1] = '.';
    } else { // "0123456" => "12345600.0"
      // Slide digits forward 1 and append suitable zeros and '.0'
      if (p + base10Exponent - digitCount > dest + length - 3) {
        dest[0] = '\0';
        return 0;
      }
      memmove(firstOutputChar, firstOutputChar + 1, p - firstOutputChar - 1);
      p -= 1;
      memset(p, '0', base10Exponent - digitCount + 1);
      p += base10Exponent - digitCount + 1;
      *p++ = '.';
      *p++ = '0';
    }
    *p = '\0';
    return p - dest;
}
#endif

// ================================================================
//
// Arithmetic helpers
//
// ================================================================

// The core algorithm relies heavily on fixed-point arithmetic with
// 128-bit and 256-bit integer values. (For binary32/64 and
// float80/binary128, respectively.) They also need precise control
// over all rounding.
//
// Note that most arithmetic operations are the same for integers and
// fractions, so we can just use the normal integer operations in most
// places.  Multiplication however, is different for fixed-size
// fractions.  Integer multiplication preserves the low-order part and
// discards the high-order part (ignoring overflow).  Fraction
// multiplication preserves the high-order part and discards the
// low-order part (rounding).  So most of the arithmetic helpers here
// are for multiplication.

// Note: With 64-bit GCC and Clang, we get a noticeable performance
// gain by using `__uint128_t`.  Otherwise, we have to break things
// down into 32-bit chunks so we don't overflow 64-bit temporaries.

#if SWIFT_DTOA_BINARY64_SUPPORT
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
static void multiply128xu32(swift_uint128_t *lhs, uint32_t rhs) {
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

#if !HAVE_UINT128_T
// Shift a 128-bit integer right, rounding down.
static swift_uint128_t shiftRightRoundingDown128(swift_uint128_t lhs, int shift) {
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
}
#endif

#if !HAVE_UINT128_T
// Shift a 128-bit integer right, rounding up.
static swift_uint128_t shiftRightRoundingUp128(swift_uint128_t lhs, int shift) {
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
}
#endif
#endif

 // Shift a 128-bit integer left, discarding high bits
#if (SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT) && !HAVE_UINT128_T
static void shiftLeft128(swift_uint128_t *lhs, int shift) {
    // Note: Shift is always less than 32
    uint64_t t = (uint64_t)lhs->high << (shift + 32);
    t += (uint64_t)lhs->c << shift;
    lhs->high = t >> 32;
    t <<= 32;
    t += (uint64_t)lhs->b << shift;
    lhs->c = t >> 32;
    t <<= 32;
    t += (uint64_t)lhs->low << shift;
    lhs->b = t >> 32;
    lhs->low = t;
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
// Multiply a 256-bit fraction by a 32-bit integer.
// This is used in the digit generation to multiply by ten or
// 10,000. Note that rounding is never an issue.
// As used above, this will never overflow.
static void multiply256xu32(swift_uint256_t *lhs, uint32_t rhs) {
#if HAVE_UINT128_T
    __uint128_t t = (__uint128_t)lhs->low * rhs;
    lhs->low = (uint64_t)t;
    t = (t >> 64) + (__uint128_t)lhs->midlow * rhs;
    lhs->midlow = (uint64_t)t;
    t = (t >> 64) + (__uint128_t)lhs->midhigh * rhs;
    lhs->midhigh = (uint64_t)t;
    t = (t >> 64) + (__uint128_t)lhs->high * rhs;
    lhs->high = (uint64_t)t;
#else
    uint64_t t = 0;
    for (int i = 0; i < 8; ++i) {
      t = (t >> 32) + (uint64_t)lhs->elt[i] * rhs;
      lhs->elt[i] = t;
    }
#endif
}

// Multiply a 256-bit fraction by a 128-bit fraction, rounding down.
static void multiply256x128RoundingDown(swift_uint256_t *lhs, swift_uint128_t rhs) {
#if HAVE_UINT128_T
    // A full multiply of four 64-bit values by two 64-bit values
    // yields six such components.  We discard the bottom two (except
    // for carries) to get a rounded-down four-element result.
    __uint128_t current = (__uint128_t)lhs->low * (uint64_t)rhs;

    current = (current >> 64);
    __uint128_t t = (__uint128_t)lhs->low * (rhs >> 64);
    current += (uint64_t)t;
    __uint128_t next = t >> 64;
    t = (__uint128_t)lhs->midlow * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;

    current = next + (current >> 64);
    t = (__uint128_t)lhs->midlow * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (__uint128_t)lhs->midhigh * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->low = (uint64_t)current;

    current = next + (current >> 64);
    t = (__uint128_t)lhs->midhigh * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (__uint128_t)lhs->high * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->midlow = (uint64_t)current;

    current = next + (current >> 64);
    t = (__uint128_t)lhs->high * (rhs >> 64);
    current += t;
    lhs->midhigh = (uint64_t)current;
    lhs->high = (uint64_t)(current >> 64);
#else
    uint64_t a, b, c, d; // temporaries
    // Eight 32-bit values multiplied by 4 32-bit values.  Oh my.
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = 0;

    a = (uint64_t)lhs->elt[0] * rhs.low;
    t += (a & mask32);
    t >>= 32;
    t += (a >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.b;
    b = (uint64_t)lhs->elt[1] * rhs.low;
    t += (a & mask32) + (b & mask32);
    t >>= 32;
    t += (a >> 32) + (b >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.c;
    b = (uint64_t)lhs->elt[1] * rhs.b;
    c = (uint64_t)lhs->elt[2] * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.high;
    b = (uint64_t)lhs->elt[1] * rhs.c;
    c = (uint64_t)lhs->elt[2] * rhs.b;
    d = (uint64_t)lhs->elt[3] * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);

    for (int i = 0; i < 4; ++i) {
      a = (uint64_t)lhs->elt[i + 1] * rhs.high;
      b = (uint64_t)lhs->elt[i + 2] * rhs.c;
      c = (uint64_t)lhs->elt[i + 3] * rhs.b;
      d = (uint64_t)lhs->elt[i + 4] * rhs.low;
      t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
      lhs->elt[i] = t;
      t >>= 32;
      t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    }

    a = (uint64_t)lhs->elt[5] * rhs.high;
    b = (uint64_t)lhs->elt[6] * rhs.c;
    c = (uint64_t)lhs->elt[7] * rhs.b;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    lhs->elt[4] = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);

    a = (uint64_t)lhs->elt[6] * rhs.high;
    b = (uint64_t)lhs->elt[7] * rhs.c;
    t += (a & mask32) + (b & mask32);
    lhs->elt[5] = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);

    t += (uint64_t)lhs->elt[7] * rhs.high;
    lhs->elt[6] = t;
    lhs->elt[7] = t >> 32;
#endif
}

// Multiply a 256-bit fraction by a 128-bit fraction, rounding up.
static void multiply256x128RoundingUp(swift_uint256_t *lhs, swift_uint128_t rhs) {
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
    t = (swift_uint128_t)lhs->midlow * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    // Round up by adding UINT128_MAX (upper half)
    current += UINT64_MAX;

    current = next + (current >> 64);
    t = (swift_uint128_t)lhs->midlow * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (swift_uint128_t)lhs->midhigh * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->low = (uint64_t)current;

    current = next + (current >> 64);
    t = (swift_uint128_t)lhs->midhigh * (rhs >> 64);
    current += (uint64_t)t;
    next = t >> 64;
    t = (swift_uint128_t)lhs->high * (uint64_t)rhs;
    current += (uint64_t)t;
    next += t >> 64;
    lhs->midlow = (uint64_t)current;

    current = next + (current >> 64);
    t = (swift_uint128_t)lhs->high * (rhs >> 64);
    current += t;
    lhs->midhigh = (uint64_t)current;
    lhs->high = (uint64_t)(current >> 64);
#else
    uint64_t a, b, c, d; // temporaries
    // Eight 32-bit values multiplied by 4 32-bit values.  Oh my.
    static const uint64_t mask32 = UINT32_MAX;
    uint64_t t = 0;

    a = (uint64_t)lhs->elt[0] * rhs.low + mask32;
    t += (a & mask32);
    t >>= 32;
    t += (a >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.b;
    b = (uint64_t)lhs->elt[1] * rhs.low;
    t += (a & mask32) + (b & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.c;
    b = (uint64_t)lhs->elt[1] * rhs.b;
    c = (uint64_t)lhs->elt[2] * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);

    a = (uint64_t)lhs->elt[0] * rhs.high;
    b = (uint64_t)lhs->elt[1] * rhs.c;
    c = (uint64_t)lhs->elt[2] * rhs.b;
    d = (uint64_t)lhs->elt[3] * rhs.low;
    t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32) + mask32;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);

    for (int i = 0; i < 4; ++i) {
      a = (uint64_t)lhs->elt[i + 1] * rhs.high;
      b = (uint64_t)lhs->elt[i + 2] * rhs.c;
      c = (uint64_t)lhs->elt[i + 3] * rhs.b;
      d = (uint64_t)lhs->elt[i + 4] * rhs.low;
      t += (a & mask32) + (b & mask32) + (c & mask32) + (d & mask32);
      lhs->elt[i] = t;
      t >>= 32;
      t += (a >> 32) + (b >> 32) + (c >> 32) + (d >> 32);
    }

    a = (uint64_t)lhs->elt[5] * rhs.high;
    b = (uint64_t)lhs->elt[6] * rhs.c;
    c = (uint64_t)lhs->elt[7] * rhs.b;
    t += (a & mask32) + (b & mask32) + (c & mask32);
    lhs->elt[4] = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32) + (c >> 32);

    a = (uint64_t)lhs->elt[6] * rhs.high;
    b = (uint64_t)lhs->elt[7] * rhs.c;
    t += (a & mask32) + (b & mask32);
    lhs->elt[5] = t;
    t >>= 32;
    t += (a >> 32) + (b >> 32);

    t += (uint64_t)lhs->elt[7] * rhs.high;
    lhs->elt[6] = t;
    lhs->elt[7] = t >> 32;

#endif
}

// Subtract two 256-bit integers or fractions.
static void subtract256x256(swift_uint256_t *lhs, swift_uint256_t rhs) {
#if HAVE_UINT128_T
    swift_uint128_t t = (swift_uint128_t)lhs->low + (~rhs.low) + 1;
    lhs->low = t;
    t = (t >> 64) + lhs->midlow + (~rhs.midlow);
    lhs->midlow = t;
    t = (t >> 64) + lhs->midhigh + (~rhs.midhigh);
    lhs->midhigh = t;
    lhs->high += (t >> 64) + (~rhs.high);
#else
    uint64_t t = ((uint64_t)1) << 32;
    for (int i = 0; i < 8; i++) {
      t = (t >> 32) + lhs->elt[i] + (~rhs.elt[i]);
      lhs->elt[i] = t;
    }
#endif
}

// Compare two 256-bit integers or fractions.
static int isLessThan256x256(swift_uint256_t lhs, swift_uint256_t rhs) {
#if HAVE_UINT128_T
    return (lhs.high < rhs.high)
        || (lhs.high == rhs.high
            && (lhs.midhigh < rhs.midhigh
                || (lhs.midhigh == rhs.midhigh
                    && (lhs.midlow < rhs.midlow
                        || (lhs.midlow == rhs.midlow
                            && lhs.low < rhs.low)))));
#else
    for (int i = 7; i >= 0; i--) {
      if (lhs.elt[i] < rhs.elt[i]) {
        return true;
      } else if (lhs.elt[i] > rhs.elt[i]) {
        return false;
      }
    }
    return false;
#endif
}

// Shift a 256-bit integer right (by less than 32 bits!), rounding down.
static void shiftRightRoundingDown256(swift_uint256_t *lhs, int shift) {
#if HAVE_UINT128_T
    __uint128_t t = (__uint128_t)lhs->low >> shift;
    t += ((__uint128_t)lhs->midlow << (64 - shift));
    lhs->low = t;
    t >>= 64;
    t += ((__uint128_t)lhs->midhigh << (64 - shift));
    lhs->midlow = t;
    t >>= 64;
    t += ((__uint128_t)lhs->high << (64 - shift));
    lhs->midhigh = t;
    t >>= 64;
    lhs->high = t;
#else
    uint64_t t = (uint64_t)lhs->elt[0] >> shift;
    for (int i = 0; i < 7; ++i) {
      t += ((uint64_t)lhs->elt[i + 1] << (32 - shift));
      lhs->elt[i] = t;
      t >>= 32;
    }
    lhs->elt[7] = t;
#endif
}

// Shift a 256-bit integer right, rounding up.
// Note: The shift will always be less than 20.  Someday, that
// might suggest a way to further optimize this.
static void shiftRightRoundingUp256(swift_uint256_t *lhs, int shift) {
#if HAVE_UINT128_T
    const uint64_t bias = (1 << shift) - 1;
    __uint128_t t = ((__uint128_t)lhs->low + bias) >> shift;
    t += ((__uint128_t)lhs->midlow << (64 - shift));
    lhs->low = t;
    t >>= 64;
    t += ((__uint128_t)lhs->midhigh << (64 - shift));
    lhs->midlow = t;
    t >>= 64;
    t += ((__uint128_t)lhs->high << (64 - shift));
    lhs->midhigh = t;
    t >>= 64;
    lhs->high = t;
#else
    const uint64_t bias = (1 << shift) - 1;
    uint64_t t = ((uint64_t)lhs->elt[0] + bias) >> shift;
    for (int i = 0; i < 7; ++i) {
      t += ((uint64_t)lhs->elt[i + 1] << (32 - shift));
      lhs->elt[i] = t;
      t >>= 32;
    }
    lhs->elt[7] = t;
#endif
}
#endif

// ================================================================
//
// Power of 10 calculation
//
// ================================================================

//
// ------------  Power-of-10 tables. --------------------------
//
// Grisu-style algorithms rely on being able to rapidly
// find a high-precision approximation of any power of 10.
// These values were computed by a simple script that
// relied on Python's excellent variable-length
// integer support.

#if SWIFT_DTOA_BINARY32_SUPPORT
// Table with negative powers of 10 to 64 bits
//
// Table size: 320 bytes
static uint64_t powersOf10_negativeBinary32[] = {
    0x8b61313bbabce2c6ULL, // x 2^-132 ~= 10^-40
    0xae397d8aa96c1b77ULL, // x 2^-129 ~= 10^-39
    0xd9c7dced53c72255ULL, // x 2^-126 ~= 10^-38
    0x881cea14545c7575ULL, // x 2^-122 ~= 10^-37
    0xaa242499697392d2ULL, // x 2^-119 ~= 10^-36
    0xd4ad2dbfc3d07787ULL, // x 2^-116 ~= 10^-35
    0x84ec3c97da624ab4ULL, // x 2^-112 ~= 10^-34
    0xa6274bbdd0fadd61ULL, // x 2^-109 ~= 10^-33
    0xcfb11ead453994baULL, // x 2^-106 ~= 10^-32
    0x81ceb32c4b43fcf4ULL, // x 2^-102 ~= 10^-31
    0xa2425ff75e14fc31ULL, // x 2^-99 ~= 10^-30
    0xcad2f7f5359a3b3eULL, // x 2^-96 ~= 10^-29
    0xfd87b5f28300ca0dULL, // x 2^-93 ~= 10^-28
    0x9e74d1b791e07e48ULL, // x 2^-89 ~= 10^-27
    0xc612062576589ddaULL, // x 2^-86 ~= 10^-26
    0xf79687aed3eec551ULL, // x 2^-83 ~= 10^-25
    0x9abe14cd44753b52ULL, // x 2^-79 ~= 10^-24
    0xc16d9a0095928a27ULL, // x 2^-76 ~= 10^-23
    0xf1c90080baf72cb1ULL, // x 2^-73 ~= 10^-22
    0x971da05074da7beeULL, // x 2^-69 ~= 10^-21
    0xbce5086492111aeaULL, // x 2^-66 ~= 10^-20
    0xec1e4a7db69561a5ULL, // x 2^-63 ~= 10^-19
    0x9392ee8e921d5d07ULL, // x 2^-59 ~= 10^-18
    0xb877aa3236a4b449ULL, // x 2^-56 ~= 10^-17
    0xe69594bec44de15bULL, // x 2^-53 ~= 10^-16
    0x901d7cf73ab0acd9ULL, // x 2^-49 ~= 10^-15
    0xb424dc35095cd80fULL, // x 2^-46 ~= 10^-14
    0xe12e13424bb40e13ULL, // x 2^-43 ~= 10^-13
    0x8cbccc096f5088cbULL, // x 2^-39 ~= 10^-12
    0xafebff0bcb24aafeULL, // x 2^-36 ~= 10^-11
    0xdbe6fecebdedd5beULL, // x 2^-33 ~= 10^-10
    0x89705f4136b4a597ULL, // x 2^-29 ~= 10^-9
    0xabcc77118461cefcULL, // x 2^-26 ~= 10^-8
    0xd6bf94d5e57a42bcULL, // x 2^-23 ~= 10^-7
    0x8637bd05af6c69b5ULL, // x 2^-19 ~= 10^-6
    0xa7c5ac471b478423ULL, // x 2^-16 ~= 10^-5
    0xd1b71758e219652bULL, // x 2^-13 ~= 10^-4
    0x83126e978d4fdf3bULL, // x 2^-9 ~= 10^-3
    0xa3d70a3d70a3d70aULL, // x 2^-6 ~= 10^-2
    0xccccccccccccccccULL, // x 2^-3 ~= 10^-1
};
#endif

#if SWIFT_DTOA_BINARY32_SUPPORT || SWIFT_DTOA_BINARY64_SUPPORT || SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
// Tables with powers of 10
//
// The constant powers of 10 here represent pure fractions
// with a binary point at the far left. (Each number in
// this first table is implicitly divided by 2^128.)
//
// Table size: 896 bytes
//
// A 64-bit significand allows us to exactly represent powers of 10 up
// to 10^27.  In 128 bits, we can exactly represent powers of 10 up to
// 10^55.  As with all of these tables, the binary exponent is not stored;
// it is computed by the `binaryExponentFor10ToThe(p)` function.
static const uint64_t powersOf10_Exact128[56 * 2] = {
    // Low order ... high order
    0x0000000000000000ULL, 0x8000000000000000ULL, // x 2^1 == 10^0 exactly
    0x0000000000000000ULL, 0xa000000000000000ULL, // x 2^4 == 10^1 exactly
    0x0000000000000000ULL, 0xc800000000000000ULL, // x 2^7 == 10^2 exactly
    0x0000000000000000ULL, 0xfa00000000000000ULL, // x 2^10 == 10^3 exactly
    0x0000000000000000ULL, 0x9c40000000000000ULL, // x 2^14 == 10^4 exactly
    0x0000000000000000ULL, 0xc350000000000000ULL, // x 2^17 == 10^5 exactly
    0x0000000000000000ULL, 0xf424000000000000ULL, // x 2^20 == 10^6 exactly
    0x0000000000000000ULL, 0x9896800000000000ULL, // x 2^24 == 10^7 exactly
    0x0000000000000000ULL, 0xbebc200000000000ULL, // x 2^27 == 10^8 exactly
    0x0000000000000000ULL, 0xee6b280000000000ULL, // x 2^30 == 10^9 exactly
    0x0000000000000000ULL, 0x9502f90000000000ULL, // x 2^34 == 10^10 exactly
    0x0000000000000000ULL, 0xba43b74000000000ULL, // x 2^37 == 10^11 exactly
    0x0000000000000000ULL, 0xe8d4a51000000000ULL, // x 2^40 == 10^12 exactly
    0x0000000000000000ULL, 0x9184e72a00000000ULL, // x 2^44 == 10^13 exactly
    0x0000000000000000ULL, 0xb5e620f480000000ULL, // x 2^47 == 10^14 exactly
    0x0000000000000000ULL, 0xe35fa931a0000000ULL, // x 2^50 == 10^15 exactly
    0x0000000000000000ULL, 0x8e1bc9bf04000000ULL, // x 2^54 == 10^16 exactly
    0x0000000000000000ULL, 0xb1a2bc2ec5000000ULL, // x 2^57 == 10^17 exactly
    0x0000000000000000ULL, 0xde0b6b3a76400000ULL, // x 2^60 == 10^18 exactly
    0x0000000000000000ULL, 0x8ac7230489e80000ULL, // x 2^64 == 10^19 exactly
    0x0000000000000000ULL, 0xad78ebc5ac620000ULL, // x 2^67 == 10^20 exactly
    0x0000000000000000ULL, 0xd8d726b7177a8000ULL, // x 2^70 == 10^21 exactly
    0x0000000000000000ULL, 0x878678326eac9000ULL, // x 2^74 == 10^22 exactly
    0x0000000000000000ULL, 0xa968163f0a57b400ULL, // x 2^77 == 10^23 exactly
    0x0000000000000000ULL, 0xd3c21bcecceda100ULL, // x 2^80 == 10^24 exactly
    0x0000000000000000ULL, 0x84595161401484a0ULL, // x 2^84 == 10^25 exactly
    0x0000000000000000ULL, 0xa56fa5b99019a5c8ULL, // x 2^87 == 10^26 exactly
    0x0000000000000000ULL, 0xcecb8f27f4200f3aULL, // x 2^90 == 10^27 exactly
    0x4000000000000000ULL, 0x813f3978f8940984ULL, // x 2^94 == 10^28 exactly
    0x5000000000000000ULL, 0xa18f07d736b90be5ULL, // x 2^97 == 10^29 exactly
    0xa400000000000000ULL, 0xc9f2c9cd04674edeULL, // x 2^100 == 10^30 exactly
    0x4d00000000000000ULL, 0xfc6f7c4045812296ULL, // x 2^103 == 10^31 exactly
    0xf020000000000000ULL, 0x9dc5ada82b70b59dULL, // x 2^107 == 10^32 exactly
    0x6c28000000000000ULL, 0xc5371912364ce305ULL, // x 2^110 == 10^33 exactly
    0xc732000000000000ULL, 0xf684df56c3e01bc6ULL, // x 2^113 == 10^34 exactly
    0x3c7f400000000000ULL, 0x9a130b963a6c115cULL, // x 2^117 == 10^35 exactly
    0x4b9f100000000000ULL, 0xc097ce7bc90715b3ULL, // x 2^120 == 10^36 exactly
    0x1e86d40000000000ULL, 0xf0bdc21abb48db20ULL, // x 2^123 == 10^37 exactly
    0x1314448000000000ULL, 0x96769950b50d88f4ULL, // x 2^127 == 10^38 exactly
    0x17d955a000000000ULL, 0xbc143fa4e250eb31ULL, // x 2^130 == 10^39 exactly
    0x5dcfab0800000000ULL, 0xeb194f8e1ae525fdULL, // x 2^133 == 10^40 exactly
    0x5aa1cae500000000ULL, 0x92efd1b8d0cf37beULL, // x 2^137 == 10^41 exactly
    0xf14a3d9e40000000ULL, 0xb7abc627050305adULL, // x 2^140 == 10^42 exactly
    0x6d9ccd05d0000000ULL, 0xe596b7b0c643c719ULL, // x 2^143 == 10^43 exactly
    0xe4820023a2000000ULL, 0x8f7e32ce7bea5c6fULL, // x 2^147 == 10^44 exactly
    0xdda2802c8a800000ULL, 0xb35dbf821ae4f38bULL, // x 2^150 == 10^45 exactly
    0xd50b2037ad200000ULL, 0xe0352f62a19e306eULL, // x 2^153 == 10^46 exactly
    0x4526f422cc340000ULL, 0x8c213d9da502de45ULL, // x 2^157 == 10^47 exactly
    0x9670b12b7f410000ULL, 0xaf298d050e4395d6ULL, // x 2^160 == 10^48 exactly
    0x3c0cdd765f114000ULL, 0xdaf3f04651d47b4cULL, // x 2^163 == 10^49 exactly
    0xa5880a69fb6ac800ULL, 0x88d8762bf324cd0fULL, // x 2^167 == 10^50 exactly
    0x8eea0d047a457a00ULL, 0xab0e93b6efee0053ULL, // x 2^170 == 10^51 exactly
    0x72a4904598d6d880ULL, 0xd5d238a4abe98068ULL, // x 2^173 == 10^52 exactly
    0x47a6da2b7f864750ULL, 0x85a36366eb71f041ULL, // x 2^177 == 10^53 exactly
    0x999090b65f67d924ULL, 0xa70c3c40a64e6c51ULL, // x 2^180 == 10^54 exactly
    0xfff4b4e3f741cf6dULL, 0xd0cf4b50cfe20765ULL, // x 2^183 == 10^55 exactly
};
#endif

#if SWIFT_DTOA_BINARY64_SUPPORT
// Rounded values supporting the full range of binary64
//
// Table size: 464 bytes
//
// We only store every 28th power of ten here.
// We can multiply by an exact 64-bit power of
// ten from the table above to reconstruct the
// significand for any power of 10.
static const uint64_t powersOf10_Binary64[] = {
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

#if SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
// Every 56th power of 10 across the range of Float80/Binary128
//
// Table size: 5,728 bytes
//
// Note: We could cut this in half at the cost of one additional
// 256-bit multiply by only storing the positive values and
// multiplying by 10^-4984 to obtain the negative ones.
static const uint64_t powersOf10_Binary128[] = {
    // Low-order ... high-order
    0xaec2e6aff96b46aeULL, 0xf91044c2eff84750ULL, 0x2b55c9e70e00c557ULL, 0xb6536903bf8f2bdaULL, // x 2^-16556 ~= 10^-4984
    0xda1b3c3dd3889587ULL, 0x73a7380aba84a6b1ULL, 0xbddb2dfde3f8a6e3ULL, 0xb9e5428330737362ULL, // x 2^-16370 ~= 10^-4928
    0xa2d23c57cfebb9ecULL, 0x9f165c039ead6d77ULL, 0x88227fdfc13ab53dULL, 0xbd89006346a9a34dULL, // x 2^-16184 ~= 10^-4872
    0x333d510cf27e5a5ULL, 0x4e3cc383eaa17b7bULL, 0xe05fe4207ca3d508ULL, 0xc13efc51ade7df64ULL, // x 2^-15998 ~= 10^-4816
    0xff242c569bc1f539ULL, 0x5c67ba58680c4cceULL, 0x3c55f3f947fef0e9ULL, 0xc50791bd8dd72edbULL, // x 2^-15812 ~= 10^-4760
    0xe4b75ae27bec50bfULL, 0x25b0419765fdfcdbULL, 0x915564d8ab057eeULL, 0xc8e31de056f89c19ULL, // x 2^-15626 ~= 10^-4704
    0x548b1e80a94f3434ULL, 0xe418e9217ce83755ULL, 0x801e38463183fc88ULL, 0xccd1ffc6bba63e21ULL, // x 2^-15440 ~= 10^-4648
    0x541950a0fdc2b4d9ULL, 0xeea173da1f0eb7b4ULL, 0xcfadf6b2aa7c4f43ULL, 0xd0d49859d60d40a3ULL, // x 2^-15254 ~= 10^-4592
    0x7e64501be95ad76bULL, 0x451e855d8acef835ULL, 0x9e601e707a2c3488ULL, 0xd4eb4a687c0253e8ULL, // x 2^-15068 ~= 10^-4536
    0xdadd9645f360cb51ULL, 0xf290163350ecb3ebULL, 0xa8edffdccfe4db4bULL, 0xd9167ab0c1965798ULL, // x 2^-14882 ~= 10^-4480
    0x7e447db3018ffbdfULL, 0x4fa1860c08a85923ULL, 0xb17cd86e7fcece75ULL, 0xdd568fe9ab559344ULL, // x 2^-14696 ~= 10^-4424
    0x61cd4655bf64d265ULL, 0xb19fd88fe285b3bcULL, 0x1151250681d59705ULL, 0xe1abf2cd11206610ULL, // x 2^-14510 ~= 10^-4368
    0xa5703f5ce7a619ecULL, 0x361243a84b55574dULL, 0x25a8e1e5dbb41d6ULL, 0xe6170e21b2910457ULL, // x 2^-14324 ~= 10^-4312
    0xb93897a6cf5d3e61ULL, 0x18746fcc6a190db9ULL, 0x66e849253e5da0c2ULL, 0xea984ec57de69f13ULL, // x 2^-14138 ~= 10^-4256
    0x309043d12ab5b0acULL, 0x79c93cff11f09319ULL, 0xf5a7800f23ef67b8ULL, 0xef3023b80a732d93ULL, // x 2^-13952 ~= 10^-4200
    0xa3baa84c049b52b9ULL, 0xbec466ee1b586342ULL, 0xe85fc7f4edbd3caULL, 0xf3defe25478e074aULL, // x 2^-13766 ~= 10^-4144
    0xd1f4628316b15c7aULL, 0xae16192410d3135eULL, 0x4268a54f70bd28c4ULL, 0xf8a551706112897cULL, // x 2^-13580 ~= 10^-4088
    0x9eb9296cc5749dbaULL, 0x48324e275376dfddULL, 0x5052e9289f0f2333ULL, 0xfd83933eda772c0bULL, // x 2^-13394 ~= 10^-4032
    0xff6aae669a5a0d8aULL, 0x24fed95087b9006eULL, 0x1b02378a405b421ULL, 0x813d1dc1f0c754d6ULL, // x 2^-13207 ~= 10^-3976
    0xf993f18de00dc89bULL, 0x15617da021b89f92ULL, 0xb782db1fc6aba49bULL, 0x83c4e245ed051dc1ULL, // x 2^-13021 ~= 10^-3920
    0xc6a0d64a712172b1ULL, 0x2217669197ac1504ULL, 0x4250be2eeba87d15ULL, 0x86595584116caf3cULL, // x 2^-12835 ~= 10^-3864
    0xbdc0c67a220687bULL, 0x44a66a6d6fd6537bULL, 0x3f1f93f1943ca9b6ULL, 0x88fab70d8b44952aULL, // x 2^-12649 ~= 10^-3808
    0xb60b57164ad28122ULL, 0xde5bd4572c25a830ULL, 0x2c87f18b39478aa2ULL, 0x8ba947b223e5783eULL, // x 2^-12463 ~= 10^-3752
    0xbd59568efdb9bfeeULL, 0x292f8f2c98d7f44cULL, 0x4054f5360249ebd1ULL, 0x8e6549867da7d11aULL, // x 2^-12277 ~= 10^-3696
    0x9fa0721e66791accULL, 0x1789061d717d454cULL, 0xc1187fa0c18adbbeULL, 0x912effea7015b2c5ULL, // x 2^-12091 ~= 10^-3640
    0x982b64e953ac4e27ULL, 0x45efb05f20cf48b3ULL, 0x4b4de34e0ebc3e06ULL, 0x9406af8f83fd6265ULL, // x 2^-11905 ~= 10^-3584
    0xa53f5950eec21dcaULL, 0x3bd8754763bdbca1ULL, 0xac73f0226eff5ea1ULL, 0x96ec9e7f9004839bULL, // x 2^-11719 ~= 10^-3528
    0x320e19f88f1161b7ULL, 0x72e93fe0cce7cfd9ULL, 0x2184706ea46a4c38ULL, 0x99e11423765ec1d0ULL, // x 2^-11533 ~= 10^-3472
    0x491aba48dfc0e36eULL, 0xd3de560ee34022b2ULL, 0xddadb80577b906bdULL, 0x9ce4594a044e0f1bULL, // x 2^-11347 ~= 10^-3416
    0x6789d038697142fULL, 0x7a466a75be73db21ULL, 0x60dbd8aa443b560fULL, 0x9ff6b82ef415d222ULL, // x 2^-11161 ~= 10^-3360
    0x40ed8056af76ac43ULL, 0x8251c601e346456ULL, 0x7401c6f091f87727ULL, 0xa3187c82120dace6ULL, // x 2^-10975 ~= 10^-3304
    0x8c643ee307bffec6ULL, 0xf369a11c6f66c05aULL, 0x4d5b32f713d7f476ULL, 0xa649f36e8583e81aULL, // x 2^-10789 ~= 10^-3248
    0xe32f5e080e36b4beULL, 0x3adf30ff2eb163d4ULL, 0xb4b39dd9ddb8d317ULL, 0xa98b6ba23e2300c7ULL, // x 2^-10603 ~= 10^-3192
    0x6b9d538c192cfb1bULL, 0x1c5af3bd4d2c60b5ULL, 0xec41c1793d69d0d1ULL, 0xacdd3555869159d1ULL, // x 2^-10417 ~= 10^-3136
    0x1adadaeedf7d699cULL, 0x71043692494aa743ULL, 0x3ca5a7540d9d56c9ULL, 0xb03fa252bd05a815ULL, // x 2^-10231 ~= 10^-3080
    0xec3e4e5fc6b03617ULL, 0x47c9b16afe8fdf74ULL, 0x92e1bc1fbb33f18dULL, 0xb3b305fe328e571fULL, // x 2^-10045 ~= 10^-3024
    0x1d42fa68b12bdb23ULL, 0xac46a7b3f2b4b34eULL, 0xa908fd4a88728b6aULL, 0xb737b55e31cdde04ULL, // x 2^-9859 ~= 10^-2968
    0x887dede507f2b618ULL, 0x359a8fa0d014b9a7ULL, 0x7c4c65d15c614c56ULL, 0xbace07232df1c802ULL, // x 2^-9673 ~= 10^-2912
    0x504708e718b4b669ULL, 0xfb4d9440822af452ULL, 0xef84cc99cb4c5d17ULL, 0xbe7653b01aae13e5ULL, // x 2^-9487 ~= 10^-2856
    0x5b7977525516bff0ULL, 0x75913092420c9b35ULL, 0xcfc147ade4843a24ULL, 0xc230f522ee0a7fc2ULL, // x 2^-9301 ~= 10^-2800
    0xad5d11883cc1302bULL, 0x860a754894b9a0bcULL, 0x4668677d5f46c29bULL, 0xc5fe475d4cd35cffULL, // x 2^-9115 ~= 10^-2744
    0x42032f9f971bfc07ULL, 0x9fb576046ab35018ULL, 0x474b3cb1fe1d6a7fULL, 0xc9dea80d6283a34cULL, // x 2^-8929 ~= 10^-2688
    0xd3e7fbb72403a4ddULL, 0x8ca223055819af54ULL, 0xd6ea3b733029ef0bULL, 0xcdd276b6e582284fULL, // x 2^-8743 ~= 10^-2632
    0xba2431d885f2b7d9ULL, 0xc9879fc42869f610ULL, 0x3736730a9e47fef8ULL, 0xd1da14bc489025eaULL, // x 2^-8557 ~= 10^-2576
    0xa11edbcd65dd1844ULL, 0xcb8edae81a295887ULL, 0x3d24e68dc1027246ULL, 0xd5f5e5681a4b9285ULL, // x 2^-8371 ~= 10^-2520
    0xa0f076652f69ad08ULL, 0x9d19c341f5f42f2aULL, 0x742ab8f3864562c8ULL, 0xda264df693ac3e30ULL, // x 2^-8185 ~= 10^-2464
    0x29f760ef115f2824ULL, 0xe0ee47c041c9de0fULL, 0x8c119f3680212413ULL, 0xde6bb59f56672cdaULL, // x 2^-7999 ~= 10^-2408
    0x8b90230b3409c9d3ULL, 0x9d76eef2c1543e65ULL, 0x43190b523f872b9cULL, 0xe2c6859f5c284230ULL, // x 2^-7813 ~= 10^-2352
    0xd44ce9993bc6611eULL, 0x777c9b2dfbede079ULL, 0x2a0969bf88679396ULL, 0xe7372943179706fcULL, // x 2^-7627 ~= 10^-2296
    0xe8c5f5a63fd0fbd1ULL, 0xccc12293f1d7a58ULL, 0x131565be33dda91aULL, 0xebbe0df0c8201ac5ULL, // x 2^-7441 ~= 10^-2240
    0xdb97988dd6b776f4ULL, 0xeb2106f435f7e1d5ULL, 0xccfb1cc2ef1f44deULL, 0xf05ba3330181c750ULL, // x 2^-7255 ~= 10^-2184
    0x2fcbc8df94a1d54bULL, 0x796d0a8120801513ULL, 0x5f8385b3a882ff4cULL, 0xf5105ac3681f2716ULL, // x 2^-7069 ~= 10^-2128
    0xc8700c11071a40f5ULL, 0x23cb9e9df9331fe4ULL, 0x166c15f456786c27ULL, 0xf9dca895a3226409ULL, // x 2^-6883 ~= 10^-2072
    0x9589f4637a50cbb5ULL, 0xea8242b0030e4a51ULL, 0x6c656c3b1f2c9d91ULL, 0xfec102e2857bc1f9ULL, // x 2^-6697 ~= 10^-2016
    0xc4be56c83349136cULL, 0x6188db81ac8e775dULL, 0xfa70b9a2ca60b004ULL, 0x81def119b76837c8ULL, // x 2^-6510 ~= 10^-1960
    0xb85d39054658b363ULL, 0xe7df06bc613fda21ULL, 0x6a22490e8e9ec98bULL, 0x8469e0b6f2b8bd9bULL, // x 2^-6324 ~= 10^-1904
    0x800b1e1349fef248ULL, 0x469cfd2e6ca32a77ULL, 0x69138459b0fa72d4ULL, 0x87018eefb53c6325ULL, // x 2^-6138 ~= 10^-1848
    0xb62593291c768919ULL, 0xc098e6ed0bfbd6f6ULL, 0x6c83ad1260ff20f4ULL, 0x89a63ba4c497b50eULL, // x 2^-5952 ~= 10^-1792
    0x92ee7fce474479d3ULL, 0xe02017175bf040c6ULL, 0xd82ef2860273de8dULL, 0x8c5827f711735b46ULL, // x 2^-5766 ~= 10^-1736
    0x7b0e6375ca8c77d9ULL, 0x5f07e1e10097d47fULL, 0x416d7f9ab1e67580ULL, 0x8f17964dfc3961f2ULL, // x 2^-5580 ~= 10^-1680
    0xc8d869ed561af1ceULL, 0x8b6648e941de779bULL, 0x56700866b85d57feULL, 0x91e4ca5db93dbfecULL, // x 2^-5394 ~= 10^-1624
    0xfc04df783488a410ULL, 0x64d1f15da2c146b1ULL, 0x43cf71d5c4fd7868ULL, 0x94c0092dd4ef9511ULL, // x 2^-5208 ~= 10^-1568
    0xfbaf03b48a965a64ULL, 0x9b6122aa2b72a13cULL, 0x387898a6e22f821bULL, 0x97a9991fd8b3afc0ULL, // x 2^-5022 ~= 10^-1512
    0x50f7f7c13119aaddULL, 0xe415d8b25694250aULL, 0x8f8857e875e7774eULL, 0x9aa1c1f6110c0dd0ULL, // x 2^-4836 ~= 10^-1456
    0xce214403545fd685ULL, 0xf36d1ad779b90e09ULL, 0xa5c58d5f91a476d7ULL, 0x9da8ccda75b341b5ULL, // x 2^-4650 ~= 10^-1400
    0x63ddfb68f971b0c5ULL, 0x2822e38faf74b26eULL, 0x6e1f7f1642ebaac8ULL, 0xa0bf0465b455e921ULL, // x 2^-4464 ~= 10^-1344
    0xf0d00cec9daf7444ULL, 0x6bf3eea6f661a32aULL, 0xfad2be1679765f27ULL, 0xa3e4b4a65e97b76aULL, // x 2^-4278 ~= 10^-1288
    0x463b4ab4bd478f57ULL, 0x6f6583b5b36d5426ULL, 0x800cfab80c4e2eb1ULL, 0xa71a2b283c14fba6ULL, // x 2^-4092 ~= 10^-1232
    0xef163df2fa96e983ULL, 0xa825f32bc8f6b080ULL, 0x850b0c5976b21027ULL, 0xaa5fb6fbc115010bULL, // x 2^-3906 ~= 10^-1176
    0x7db1b3f8e100eb43ULL, 0x2862b1f61d64ddc3ULL, 0x61363686961a41e5ULL, 0xadb5a8bdaaa53051ULL, // x 2^-3720 ~= 10^-1120
    0xfd349cf00ba1e09aULL, 0x6d282fe1b7112879ULL, 0xc6f075c4b81fc72dULL, 0xb11c529ec0d87268ULL, // x 2^-3534 ~= 10^-1064
    0xf7221741b221cf6fULL, 0x3739f15b06ac3c76ULL, 0xb4e4be5b6455ef96ULL, 0xb494086bbfea00c3ULL, // x 2^-3348 ~= 10^-1008
    0xc4e5a2f864c403bbULL, 0x6e33cdcda4367276ULL, 0x24d256c540a50309ULL, 0xb81d1f9569068d8eULL, // x 2^-3162 ~= 10^-952
    0x276e3f0f67f0553bULL, 0xde73d9d5be6974ULL, 0x6d4aa5b50bb5dc0dULL, 0xbbb7ef38bb827f2dULL, // x 2^-2976 ~= 10^-896
    0x51a34a3e674484edULL, 0x1fb6069f8b26f840ULL, 0x925624c0d7d93317ULL, 0xbf64d0275747de70ULL, // x 2^-2790 ~= 10^-840
    0xcc775c8cb6de1dbcULL, 0x6d60d02eac6309eeULL, 0x8e5a2e5116baf191ULL, 0xc3241cf0094a8e70ULL, // x 2^-2604 ~= 10^-784
    0x6023c8fa17d7b105ULL, 0x69cf8f51d2e5e65ULL, 0xb0560c246f90e9e8ULL, 0xc6f631e782d57096ULL, // x 2^-2418 ~= 10^-728
    0x92c17acb2d08d5fdULL, 0xc26ffb8e81532725ULL, 0x2ffff1289a804c5aULL, 0xcadb6d313c8736fcULL, // x 2^-2232 ~= 10^-672
    0x47df78ab9e92897aULL, 0xc02b302a892b81dcULL, 0xa855e127113c887bULL, 0xced42ec885d9dbbeULL, // x 2^-2046 ~= 10^-616
    0xdaf2dec03ec0c322ULL, 0x72db3bc15b0c7014ULL, 0xe00bad8dfc0d8c8eULL, 0xd2e0d889c213fd60ULL, // x 2^-1860 ~= 10^-560
    0xd3a04799e4473ac8ULL, 0xa116409a2fdf1e9eULL, 0xc654d07271e6c39fULL, 0xd701ce3bd387bf47ULL, // x 2^-1674 ~= 10^-504
    0x5c8a5dc65d745a24ULL, 0x2726c48a85389fa7ULL, 0x84c663cee6b86e7cULL, 0xdb377599b6074244ULL, // x 2^-1488 ~= 10^-448
    0xd7ebc61ba77a9e66ULL, 0x8bf77d4bc59b35b1ULL, 0xcb285ceb2fed040dULL, 0xdf82365c497b5453ULL, // x 2^-1302 ~= 10^-392
    0x744ce999bfed213aULL, 0x363b1f2c568dc3e2ULL, 0xfd1b1b2308169b25ULL, 0xe3e27a444d8d98b7ULL, // x 2^-1116 ~= 10^-336
    0x6a40608fe10de7e7ULL, 0xf910f9f648232f14ULL, 0xd1b3400f8f9cff68ULL, 0xe858ad248f5c22c9ULL, // x 2^-930 ~= 10^-280
    0x9bdbfc21260dd1adULL, 0x4609ac5c7899ca36ULL, 0xa4f8bf5635246428ULL, 0xece53cec4a314ebdULL, // x 2^-744 ~= 10^-224
    0xd88181aad19d7454ULL, 0xf80f36174730ca34ULL, 0xdc44e6c3cb279ac1ULL, 0xf18899b1bc3f8ca1ULL, // x 2^-558 ~= 10^-168
    0xee19bfa6947f8e02ULL, 0xaa09501d5954a559ULL, 0x4d4617b5ff4a16d5ULL, 0xf64335bcf065d37dULL, // x 2^-372 ~= 10^-112
    0xebbc75a03b4d60e6ULL, 0xac2e4f162cfad40aULL, 0xeed6e2f0f0d56712ULL, 0xfb158592be068d2eULL, // x 2^-186 ~= 10^-56
    0x0ULL, 0x0ULL, 0x0ULL, 0x8000000000000000ULL, // x 2^1 == 10^0 exactly
    0x0ULL, 0x2000000000000000ULL, 0xbff8f10e7a8921a4ULL, 0x82818f1281ed449fULL, // x 2^187 == 10^56 exactly
    0x51775f71e92bf2f2ULL, 0x74a7ef0198791097ULL, 0x3e2cf6bc604ddb0ULL, 0x850fadc09923329eULL, // x 2^373 ~= 10^112
    0xb204b3d9686f55b5ULL, 0xfb118fc9c217a1d2ULL, 0x90fb44d2f05d0842ULL, 0x87aa9aff79042286ULL, // x 2^559 ~= 10^168
    0xd7924bff833149faULL, 0xbc10c5c5cda97c8dULL, 0x82bd6b70d99aaa6fULL, 0x8a5296ffe33cc92fULL, // x 2^745 ~= 10^224
    0xa67d072d3c7fa14bULL, 0x7ec63730f500b406ULL, 0xdb0b487b6423e1e8ULL, 0x8d07e33455637eb2ULL, // x 2^931 ~= 10^280
    0x546f2a35dc367e47ULL, 0x949063d8a46f0c0eULL, 0x213a4f0aa5e8a7b1ULL, 0x8fcac257558ee4e6ULL, // x 2^1117 ~= 10^336
    0x50611a621c0ee3aeULL, 0x202d895116aa96beULL, 0x1c306f5d1b0b5fdfULL, 0x929b7871de7f22b9ULL, // x 2^1303 ~= 10^392
    0xffa6738a27dcf7a3ULL, 0x3c11d8430d5c4802ULL, 0xa7ea9c8838ce9437ULL, 0x957a4ae1ebf7f3d3ULL, // x 2^1489 ~= 10^448
    0x5bf36c0f40bde99dULL, 0x284ba600ee9f6303ULL, 0xbf1d49cacccd5e68ULL, 0x9867806127ece4f4ULL, // x 2^1675 ~= 10^504
    0xa6e937834ed12e58ULL, 0x73f26eb82f6b8066ULL, 0x655494c5c95d77f2ULL, 0x9b63610bb9243e46ULL, // x 2^1861 ~= 10^560
    0xcd4b7660adc6930ULL, 0x8f868688f8eb79ebULL, 0x2e008393fd60b55ULL, 0x9e6e366733f85561ULL, // x 2^2047 ~= 10^616
    0x3efb9807d86d3c6aULL, 0x84c10a1d22f5adc5ULL, 0x55e04dba4b3bd4ddULL, 0xa1884b69ade24964ULL, // x 2^2233 ~= 10^672
    0xf065089401df33b4ULL, 0x1fc02370c451a755ULL, 0x44b222741eb1ebbfULL, 0xa4b1ec80f47c84adULL, // x 2^2419 ~= 10^728
    0xa62d0da836fce7d5ULL, 0x75933380ceb5048cULL, 0x1cf4a5c3bc09fa6fULL, 0xa7eb6799e8aec999ULL, // x 2^2605 ~= 10^784
    0x7a400df820f096c2ULL, 0x802c4085068d2dd5ULL, 0x3c4a575151b294dcULL, 0xab350c27feb90accULL, // x 2^2791 ~= 10^840
    0xf48b51375df06e86ULL, 0x412fe9e72afd355eULL, 0x870a8d87239d8f35ULL, 0xae8f2b2ce3d5dbe9ULL, // x 2^2977 ~= 10^896
    0x881883521930127cULL, 0xe53fd3fcb5b4df25ULL, 0xdd929f09c3eff5acULL, 0xb1fa17404a30e5e8ULL, // x 2^3163 ~= 10^952
    0x270cd9f1348eb326ULL, 0x37ed82fe9c75fccfULL, 0x1931b583a9431d7eULL, 0xb5762497dbf17a9eULL, // x 2^3349 ~= 10^1008
    0x8919b01a5b3d9ec1ULL, 0x6a7669bdfc6f699cULL, 0xe30db03e0f8dd286ULL, 0xb903a90f561d25e2ULL, // x 2^3535 ~= 10^1064
    0xf0461526b4201aa5ULL, 0x7fe40defe17e55f5ULL, 0x9eb5cb19647508c5ULL, 0xbca2fc30cc19f090ULL, // x 2^3721 ~= 10^1120
    0xd67bf35422978bbfULL, 0xdbb1c416ebe661fULL, 0x24bd4c00042ad125ULL, 0xc054773d149bf26bULL, // x 2^3907 ~= 10^1176
    0xdd093192ef5508d0ULL, 0x6eac3085943ccc0fULL, 0x7ea30dbd7ea479e3ULL, 0xc418753460cdcca9ULL, // x 2^4093 ~= 10^1232
    0xfe4ff20db6d25dc2ULL, 0x5d5d5a9519e34a42ULL, 0x764f4cf916b4deceULL, 0xc7ef52defe87b751ULL, // x 2^4279 ~= 10^1288
    0xd8adfb2e00494c5eULL, 0x72435286baf0e84eULL, 0xbeb7fbdc1cbe8b37ULL, 0xcbd96ed6466cf081ULL, // x 2^4465 ~= 10^1344
    0xe07c1e4384f594afULL, 0xc6b90b8874d5189ULL, 0xdce472c619aa3f63ULL, 0xcfd7298db6cb9672ULL, // x 2^4651 ~= 10^1400
    0x5dd902c68fa448cfULL, 0xea8d16bd9544e48eULL, 0xe47defc14a406e4fULL, 0xd3e8e55c3c1f43d0ULL, // x 2^4837 ~= 10^1456
    0x1223d79357bedca8ULL, 0xeae6c2843752ac35ULL, 0xb7157c60a24a0569ULL, 0xd80f0685a81b2a81ULL, // x 2^5023 ~= 10^1512
    0xcff72d64bc79e429ULL, 0xccc52c236decd778ULL, 0xfb0b98f6bbc4f0cbULL, 0xdc49f3445824e360ULL, // x 2^5209 ~= 10^1568
    0x3731f76b905dffbbULL, 0x5e2bddd7d12a9e42ULL, 0xc6c6c1764e047e15ULL, 0xe09a13d30c2dba62ULL, // x 2^5395 ~= 10^1624
    0xeb58d8ef2ada7c09ULL, 0xbc1a3b726b789947ULL, 0x87e8dcfc09dbc33aULL, 0xe4ffd276eedce658ULL, // x 2^5581 ~= 10^1680
    0x249a5c06dc5d5db7ULL, 0xa8f09440be97bfe6ULL, 0xb1a3642a8da3cf4fULL, 0xe97b9b89d001dab3ULL, // x 2^5767 ~= 10^1736
    0xbf34ff7963028cd9ULL, 0xc20578fa3851488bULL, 0x2d4070f33b21ab7bULL, 0xee0ddd84924ab88cULL, // x 2^5953 ~= 10^1792
    0x2d0511317361d5ULL, 0xd6919e041129a1a7ULL, 0xa2bf0c63a814e04eULL, 0xf2b70909cd3fd35cULL, // x 2^6139 ~= 10^1848
    0x1fa87f28acf1dcd2ULL, 0xe7a0a88981d1a0f9ULL, 0x8f13995cf9c2747ULL, 0xf77790f0a48a45ceULL, // x 2^6325 ~= 10^1904
    0x1b6ff8afbe589b72ULL, 0xc851bb3f9aeb1211ULL, 0x7a37993eb21444faULL, 0xfc4fea4fd590b40aULL, // x 2^6511 ~= 10^1960
    0xef23a4cbc039f0c2ULL, 0xbb3f8498a972f18eULL, 0xb7b1ada9cdeba84dULL, 0x80a046447e3d49f1ULL, // x 2^6698 ~= 10^2016
    0x2cc44f2b602b6231ULL, 0xf231f4b7996b7278ULL, 0xcc6866c5d69b2cbULL, 0x8324f8aa08d7d411ULL, // x 2^6884 ~= 10^2072
    0x822c97629a3a4c69ULL, 0x8a9afcdbc940e6f9ULL, 0x7fe2b4308dcbf1a3ULL, 0x85b64a659077660eULL, // x 2^7070 ~= 10^2128
    0xf66cfcf42d4896b0ULL, 0x1f11852a20ed33c5ULL, 0x1d73ef3eaac3c964ULL, 0x88547abb1d8e5bd9ULL, // x 2^7256 ~= 10^2184
    0x63093ad0caadb06cULL, 0x31be1482014cdaf0ULL, 0x1e34291b1ef566c7ULL, 0x8affca2bd1f88549ULL, // x 2^7442 ~= 10^2240
    0xab50f69048738e9aULL, 0xa126c32ff4882be8ULL, 0x9e9383d73d486881ULL, 0x8db87a7c1e56d873ULL, // x 2^7628 ~= 10^2296
    0xe57e659432b0a73eULL, 0x47a0e15dfc7986b8ULL, 0x9cc5ee51962c011aULL, 0x907eceba168949b3ULL, // x 2^7814 ~= 10^2352
    0x8a6ff950599f8ae5ULL, 0xd1cbbb7d005a76d3ULL, 0x413407cfeeac9743ULL, 0x93530b43e5e2c129ULL, // x 2^8000 ~= 10^2408
    0xd4e6b6e847550caaULL, 0x56a3106227b87706ULL, 0x7efa7d29c44e11b7ULL, 0x963575ce63b6332dULL, // x 2^8186 ~= 10^2464
    0xd835c90b09842263ULL, 0xb69f01a641da2a42ULL, 0x5a848859645d1c6fULL, 0x9926556bc8defe43ULL, // x 2^8372 ~= 10^2520
    0x9b0ae73c204ecd61ULL, 0x794fd5e5a51ac2fULL, 0x51edea897b34601fULL, 0x9c25f29286e9ddb6ULL, // x 2^8558 ~= 10^2576
    0x3130484fb0a61d89ULL, 0x32b7105223a27365ULL, 0xb50008d92529e91fULL, 0x9f3497244186fca4ULL, // x 2^8744 ~= 10^2632
    0x8cd036553f38a1e8ULL, 0x5e997e9f45d7897dULL, 0xf09e780bcc8238d9ULL, 0xa2528e74eaf101fcULL, // x 2^8930 ~= 10^2688
    0xe1f8b43b08b5d0efULL, 0xa0eaf3f62dc1777cULL, 0x3a5828869701a165ULL, 0xa580255203f84b47ULL, // x 2^9116 ~= 10^2744
    0x3c7f62e3154fa708ULL, 0x5786f3927eb15bd5ULL, 0x8b231a70eb5444ceULL, 0xa8bdaa0a0064fa44ULL, // x 2^9302 ~= 10^2800
    0x1ebc24a19cd70a2aULL, 0x843fddd10c7006b8ULL, 0xfa1bde1f473556a4ULL, 0xac0b6c73d065f8ccULL, // x 2^9488 ~= 10^2856
    0x46b6aae34cfd26fcULL, 0xdb7d919b136c68ULL, 0x7730e00421da4d55ULL, 0xaf69bdf68fc6a740ULL, // x 2^9674 ~= 10^2912
    0x1c4edcb83fc4c49dULL, 0x61c0edd56bbcb3e8ULL, 0x7f959cb702329d14ULL, 0xb2d8f1915ba88ca5ULL, // x 2^9860 ~= 10^2968
    0x428c840d247382feULL, 0x9cc3b1569b1325a4ULL, 0x40c3a071220f5567ULL, 0xb6595be34f821493ULL, // x 2^10046 ~= 10^3024
    0xbeb82e734787ec63ULL, 0xbeff12280d5a1676ULL, 0x11c48d02b8326bd3ULL, 0xb9eb5333aa272e9bULL, // x 2^10232 ~= 10^3080
    0x302349e12f45c73fULL, 0xb494bcc96d53e49cULL, 0x566765461bd2f61bULL, 0xbd8f2f7a1ba47d6dULL, // x 2^10418 ~= 10^3136
    0x5704ebf5f16946ceULL, 0x431388ec68ac7a26ULL, 0xb889018e4f6e9a52ULL, 0xc1454a673cb9b1ceULL, // x 2^10604 ~= 10^3192
    0x5a30431166af9b23ULL, 0x132d031fc1d1fec0ULL, 0xf85333a94848659fULL, 0xc50dff6d30c3aefcULL, // x 2^10790 ~= 10^3248
    0x7573d4b3ffe4ba3bULL, 0xf888498a40220657ULL, 0x1a1aeae7cf8a9d3dULL, 0xc8e9abc872eb2bc1ULL, // x 2^10976 ~= 10^3304
    0xb5eaef7441511eb9ULL, 0xc9cf998035a91664ULL, 0x12e29f09d9061609ULL, 0xccd8ae88cf70ad84ULL, // x 2^11162 ~= 10^3360
    0x73aed4f1908f4d01ULL, 0x8c53e7beeca4578fULL, 0xdf7601457ca20b35ULL, 0xd0db689a89f2f9b1ULL, // x 2^11348 ~= 10^3416
    0x5adbd55696e1cdd9ULL, 0x4949d09424b87626ULL, 0xcbdcd02f23cc7690ULL, 0xd4f23ccfb1916df5ULL, // x 2^11534 ~= 10^3472
    0x3f500ccf4ea03593ULL, 0x9b80aac81b50762aULL, 0x44289dd21b589d7aULL, 0xd91d8fe9a3d019ccULL, // x 2^11720 ~= 10^3528
    0x134ca67a679b84aeULL, 0x8909e424a112a3cdULL, 0x95aa118ec1d08317ULL, 0xdd5dc8a2bf27f3f7ULL, // x 2^11906 ~= 10^3584
    0xe89e3cf733d9ff40ULL, 0x14344660a175c36ULL, 0x72c4d2cad73b0a7bULL, 0xe1b34fb846321d04ULL, // x 2^12092 ~= 10^3640
    0x68c0a2c6c02dae9aULL, 0xb11160a6edb5f57ULL, 0xe20a88f1134f906dULL, 0xe61e8ff47461cda9ULL, // x 2^12278 ~= 10^3696
    0x47fa54906741561aULL, 0xaa13acba1e5511f5ULL, 0xc7c91d5c341ed39dULL, 0xea9ff638c54554e1ULL, // x 2^12464 ~= 10^3752
    0x365460ed91271c24ULL, 0xabe33496aff629b4ULL, 0xf659ede2159a45ecULL, 0xef37f1886f4b6690ULL, // x 2^12650 ~= 10^3808
    0xe4cbf4acc7fba37fULL, 0x350e915f7055b1b8ULL, 0x78d946bab954b82fULL, 0xf3e6f313130ef0efULL, // x 2^12836 ~= 10^3864
    0xe692accdfa5bd859ULL, 0xf4d4d3202379829eULL, 0xc9b1474d8f89c269ULL, 0xf8ad6e3fa030bd15ULL, // x 2^13022 ~= 10^3920
    0xeca0018ea3b8d1b4ULL, 0xe878edb67072c26dULL, 0x6b1d2745340e7b14ULL, 0xfd8bd8b770cb469eULL, // x 2^13208 ~= 10^3976
    0xce5fec949ab87cf7ULL, 0x151dcd7a53488c3ULL, 0xf22e502fcdd4bca2ULL, 0x81415538ce493bd5ULL, // x 2^13395 ~= 10^4032
    0x5e1731fbff8c032eULL, 0xe752f53c2f8fa6c1ULL, 0x7c1735fc3b813c8cULL, 0x83c92edf425b292dULL, // x 2^13581 ~= 10^4088
    0xb552102ea83f47e6ULL, 0xdf0fd2002ff6b3a3ULL, 0x367500a8e9a178fULL, 0x865db7a9ccd2839eULL, // x 2^13767 ~= 10^4144
    0x76507bafe00ec873ULL, 0x71b256ecd954434cULL, 0xc9ac50475e25293aULL, 0x88ff2f2bade74531ULL, // x 2^13953 ~= 10^4200
    0x5e2075ba289a360bULL, 0xac376f28b45e5accULL, 0x879b2e5f6ee8b1cULL, 0x8badd636cc48b341ULL, // x 2^14139 ~= 10^4256
    0xab87d85e6311e801ULL, 0xb7f786d14d58173dULL, 0x2f33c652bd12fab7ULL, 0x8e69eee1f23f2be5ULL, // x 2^14325 ~= 10^4312
    0x7fed9b68d77255beULL, 0x35dc241819de7182ULL, 0xad6a6308a8e8b557ULL, 0x9133bc8f2a130fe5ULL, // x 2^14511 ~= 10^4368
    0x728ae72899d4bd12ULL, 0xe5413d9414142a55ULL, 0x9dbaa465efe141a0ULL, 0x940b83f23a55842aULL, // x 2^14697 ~= 10^4424
    0xf7740145246fb8fULL, 0x186ef2c39acb4103ULL, 0x888c9ab2fc5b3437ULL, 0x96f18b1742aad751ULL, // x 2^14883 ~= 10^4480
    0xd8bb0fba2183c6efULL, 0xbf66d66cc34f0197ULL, 0xba00864671d1053fULL, 0x99e6196979b978f1ULL, // x 2^15069 ~= 10^4536
    0x9b71ed2ceb790e49ULL, 0x6faac32d59cc1f5dULL, 0x61d59d402aae4feaULL, 0x9ce977ba0ce3a0bdULL, // x 2^15255 ~= 10^4592
    0xa0aa6d5e63991cfbULL, 0x19482fa0ac45669cULL, 0x803c1cd864033781ULL, 0x9ffbf04722750449ULL, // x 2^15441 ~= 10^4648
    0x95a9949e04b8bff3ULL, 0x900aa3c2f02ac9d4ULL, 0xa28a151725a55e10ULL, 0xa31dcec2fef14b30ULL, // x 2^15627 ~= 10^4704
    0x3acf9496dade0ce9ULL, 0xbd8ecf923d23bec0ULL, 0x5b8452af2302fe13ULL, 0xa64f605b4e3352cdULL, // x 2^15813 ~= 10^4760
    0x6204425d2b58e822ULL, 0xdee162a8a1248550ULL, 0x82b84cabc828bf93ULL, 0xa990f3c09110c544ULL, // x 2^15999 ~= 10^4816
    0x91a2658e0639f32ULL, 0x66fa2184cee0b861ULL, 0x8d29dd5122e4278dULL, 0xace2d92db0390b59ULL, // x 2^16185 ~= 10^4872
    0x80acda113324758aULL, 0xded179c26d9ab828ULL, 0x58f8fde02c03a6c6ULL, 0xb045626fb50a35e7ULL, // x 2^16371 ~= 10^4928
    0x7128a8aad239ce8fULL, 0x8737bd250290cd5bULL, 0xd950102978dbd0ffULL, 0xb3b8e2eda91a232dULL, // x 2^16557 ~= 10^4984
};
#endif

#if SWIFT_DTOA_BINARY32_SUPPORT
// Given a power `p`, this returns three values:
// * 64-bit fractions `lower` and `upper`
// * integer `exponent`
//
// The returned values satisfy the following:
// ```
//    lower * 2^exponent <= 10^p <= upper * 2^exponent
// ```
//
// Note: Max(*upper - *lower) = 3
static void intervalContainingPowerOf10_Binary32(int p, uint64_t *lower, uint64_t *upper, int *exponent) {
  if (p >= 0) {
    uint64_t base = powersOf10_Exact128[p * 2 + 1];
    *lower = base;
    if (p < 28) {
      *upper = base;
    } else {
      *upper = base + 1;
    }
  } else {
    uint64_t base = powersOf10_negativeBinary32[p + 40];
    *lower = base;
    *upper = base + 1;
  }
  *exponent = binaryExponentFor10ToThe(p);
}
#endif

#if SWIFT_DTOA_BINARY64_SUPPORT
// Given a power `p`, this returns three values:
// * 128-bit fractions `lower` and `upper`
// * integer `exponent`
//
// Note: This function takes on average about 10% of the total runtime
// for formatting a double, as the general case here requires several
// multiplications to accurately reconstruct the lower and upper
// bounds.
//
// The returned values satisfy the following:
// ```
//    lower * 2^exponent <= 10^p <= upper * 2^exponent
// ```
//
// Note: Max(*upper - *lower) = 3
static void intervalContainingPowerOf10_Binary64(int p, swift_uint128_t *lower, swift_uint128_t *upper, int *exponent) {
    if (p >= 0 && p <= 55) {
        // Use one 64-bit exact value
        swift_uint128_t exact;
        initialize128WithHighLow64(exact,
                                   powersOf10_Exact128[p * 2 + 1],
                                   powersOf10_Exact128[p * 2]);
        *upper = exact;
        *lower = exact;
        *exponent = binaryExponentFor10ToThe(p);
        return;
    }

    // Multiply a 128-bit approximate value with a 64-bit exact value
    int index = p + 400;
    // Copy a pair of uint64_t into a swift_uint128_t
    int mainPower = index / 28;
    const uint64_t *base_p = powersOf10_Binary64 + mainPower * 2;
    swift_uint128_t base;
    initialize128WithHighLow64(base, base_p[1], base_p[0]);
    int extraPower = index - mainPower * 28;
    int baseExponent = binaryExponentFor10ToThe(p - extraPower);

    int e = baseExponent;
    if (extraPower == 0) {
        // We're using a tightly-rounded lower bound, so +1 gives a tightly-rounded upper bound
        *lower = base;
#if HAVE_UINT128_T
        *upper = *lower + 1;
#else
        *upper = *lower;
        upper->low += 1;
#endif
    } else {
        // We need to multiply two values to get a lower bound
        int64_t extra = powersOf10_Exact128[extraPower * 2 + 1];
        e += binaryExponentFor10ToThe(extraPower);
        *lower = multiply128x64RoundingDown(base, extra);
        // +2 is enough to get an upper bound
        // (Verified through exhaustive testing.)
#if HAVE_UINT128_T
        *upper = *lower + 2;
#else
        *upper = *lower;
        upper->low += 2;
#endif
    }
    *exponent = e;
}
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT || SWIFT_DTOA_BINARY128_SUPPORT
// As above, but returning 256-bit fractions suitable for
// converting float80/binary128.
static void intervalContainingPowerOf10_Binary128(int p, swift_uint256_t *lower, swift_uint256_t *upper, int *exponent) {
    if (p >= 0 && p <= 55) {
        // We have an exact form, return a zero-width interval
        // and avoid the multiplication.
        uint64_t exactLow = powersOf10_Exact128[p * 2];
        uint64_t exactHigh = powersOf10_Exact128[p * 2 + 1];
        initialize256WithHighMidLow64(*lower, exactHigh, exactLow, 0, 0);
        *upper = *lower;
        *exponent = binaryExponentFor10ToThe(p);
        return;
    }

    int index = p + 4984;
    const uint64_t *base_p = powersOf10_Binary128 + (index / 56) * 4;
    // The values in the table are always tightly rounded down, so we use that
    // directly as a lower bound.
    initialize256WithHighMidLow64(*lower, base_p[3], base_p[2], base_p[1], base_p[0]);
    int extraPower = index % 56;
    int e = binaryExponentFor10ToThe(p - extraPower);

    if (extraPower > 0) {
        swift_uint128_t extra;
        initialize128WithHighLow64(extra,
                                   powersOf10_Exact128[extraPower * 2 + 1],
                                   powersOf10_Exact128[extraPower * 2]);
        multiply256x128RoundingDown(lower, extra);
        e += binaryExponentFor10ToThe(extraPower);
    }
    // We could compute upper similar to lower using rounding-up
    // multiplications, but this is faster.
    // Since there's just one multiplication, we can prove that 2 is
    // enough to get a true upper bound, and we've verified (through
    // exhaustive testing) that the least-significant component never
    // wraps.
    *upper = *lower;
#if HAVE_UINT128_T
    upper->low += 2;
#else
    upper->elt[0] += 2;
#endif

    *exponent = e;
}
#endif
