//===--- SwiftDtoa.h ---------------------------------------------*- c -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018, 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===---------------------------------------------------------------------===//
//
/// About SwiftDtoa
/// ===============
///
/// SwiftDtoa is the C implementation that supports the `.description`
/// and `.debugDescription` properties for the standard Swift
/// floating-point types.  These functions produce the "optimal form"
/// for the binary floating point value.  The optimal form is a
/// decimal representation that satisfies the following properties:
///
/// 1. Accurate.  Parsing the value back to a binary floating-point
///    value of the same precision will exactly yield the original
///    value.  For example, `Double(d.description) == d` for all `Double`
///    values `d` (except for NaN values, of course).
///
/// 2. Short.  Of all accurate results, the returned value will
///    contain the minimum number of significant digits.  Note that
///    this is not quite the same as C++ `to_chars` which promises the
///    minimal number of characters.
///
/// 3. Close.  Of all accurate, short results, the value printed will
///    be the one that is closest to the exact binary floating-point
///    value.
///
/// The optimal form is the ideal textual form for use in JSON and
/// similar interchange formats because it is accurate, compact, and
/// can be generated very quickly.  It is also ideal for logging and
/// debugging use; the accuracy guarantees that the result can be
/// cut-and-pasted to obtain the exact original value, and the
/// shortness property eliminates unnecessary digits that can be
/// confusing to readers.
///
/// Algorithms that produce such output have been known since at least
/// 1990, when Steele and White published their Dragon4 algorithm.
/// However, the earliest algorithms required high-precision
/// arithmetic which limited their use.  Starting in 2010 with the
/// publication of Grisu3, there has been a surge of interest and
/// there are now a number of algorithms that can produce optimal
/// forms very quickly.  This particular implementation is loosely
/// based on Grisu2 but incorporates concepts from Errol and Ryu that
/// make it significantly faster and ensure accuracy in all cases.
///
/// About SwiftDtoa v1
/// ------------------
///
/// The first version of SwiftDtoa was committed to the Swift runtime
/// in 2018.  It supported Swift's Float, Double, and Float80 formats.
///
/// About SwiftDtoa v1a
/// -------------------
///
/// Version 1a of SwiftDtoa added support for Float16.
///
/// About SwiftDtoa v2
/// ------------------
///
/// Version 2 of SwiftDtoa is a major overhaul with a number of
/// algorithmic improvements to make it faster (especially for Float16
/// and Float80), smaller, and more portable (the code only requires
/// C99 and makes no use of C or C++ floating-point facilities).  It
/// also includes experimental support for IEEE 754 quad-precision
/// binary128 format, which is not currently supported by Swift.
//
//===---------------------------------------------------------------------===//

#ifndef SWIFT_DTOA_H
#define SWIFT_DTOA_H

#define __STDC_WANT_IEC_60559_TYPES_EXT__ // FLT16_MAX
#include <float.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

//
// IEEE 754 Binary16 support (also known as "half-precision")
//

// Enable this by default.
// Force disable: -DSWIFT_DTOA_BINARY16_SUPPORT=0
#ifndef SWIFT_DTOA_BINARY16_SUPPORT
 #define SWIFT_DTOA_BINARY16_SUPPORT 1
#endif

/// Does this platform support needs to pass _Float16 as a float in
/// C function?
#ifndef SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT
// Windows does not define FLT16_MAX even though it supports _Float16 as argument.
# if (!defined(FLT16_MAX) || defined(__wasm__)) && !defined(_WIN32)
#  define SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT 1
# else
#  define SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT 0
# endif
#endif

//
// IEEE 754 Binary32 support (also known as "single-precision")
//

// Does "float" on this system use binary32 format?
// (Almost all modern systems do this.)
#if (FLT_RADIX == 2) && (FLT_MANT_DIG == 24) && (FLT_MIN_EXP == -125) && (FLT_MAX_EXP == 128)
  #define FLOAT_IS_BINARY32 1
#else
  #undef FLOAT_IS_BINARY32
#endif

// We can format binary32 values even if the local C environment
// does not support it.  But `float` == binary32 almost everywhere,
// so we enable it by default.
// Force disable: -DSWIFT_DTOA_BINARY32_SUPPORT=0
#ifndef SWIFT_DTOA_BINARY32_SUPPORT
 #define SWIFT_DTOA_BINARY32_SUPPORT 1
#endif

//
// IEEE 754 Binary64 support (also known as "double-precision")
//

// Does "double" on this system use binary64 format?
// (Almost all modern systems do this.)
#if (FLT_RADIX == 2) && (DBL_MANT_DIG == 53) && (DBL_MIN_EXP == -1021) && (DBL_MAX_EXP == 1024)
  #define DOUBLE_IS_BINARY64 1
#else
  #undef DOUBLE_IS_BINARY64
#endif

// Does "long double" on this system use binary64 format?
// (Windows, for example.)
#if (FLT_RADIX == 2) && (LDBL_MANT_DIG == 53) && (LDBL_MIN_EXP == -1021) && (LDBL_MAX_EXP == 1024)
  #define LONG_DOUBLE_IS_BINARY64 1
#else
  #undef LONG_DOUBLE_IS_BINARY64
#endif

// We can format binary64 values even if the local C environment
// does not support it.  But `double` == binary64 almost everywhere,
// so we enable it by default.
// Force disable: -DSWIFT_DTOA_BINARY64_SUPPORT=0
#ifndef SWIFT_DTOA_BINARY64_SUPPORT
 #define SWIFT_DTOA_BINARY64_SUPPORT 1
#endif

//
// Intel x87 Float80 support
//

// Is "long double" on this system the same as Float80?
// (macOS, Linux, and FreeBSD when running on x86 or x86_64 processors.)
#if (FLT_RADIX == 2) && (LDBL_MANT_DIG == 64) && (LDBL_MIN_EXP == -16381) && (LDBL_MAX_EXP == 16384)
 #define LONG_DOUBLE_IS_FLOAT80 1
#else
 #undef LONG_DOUBLE_IS_FLOAT80
#endif

// We can format float80 values even if the local C environment
// does not support it.  However, by default, we only enable it for
// environments where float80 == long double.
// Force enable: -DSWIFT_DTOA_FLOAT80_SUPPORT=1
// Force disable: -DSWIFT_DTOA_FLOAT80_SUPPORT=0
#ifndef SWIFT_DTOA_FLOAT80_SUPPORT
 #if LONG_DOUBLE_IS_FLOAT80
  #define SWIFT_DTOA_FLOAT80_SUPPORT 1
 #endif
#endif

//
// IEEE 754 Binary128 support
//

// Is "long double" on this system the same as Binary128?
// (Android on LP64 hardware.)
#if (FLT_RADIX == 2) && (LDBL_MANT_DIG == 113) && (LDBL_MIN_EXP == -16381) && (LDBL_MAX_EXP == 16384)
 #define LONG_DOUBLE_IS_BINARY128 1
#else
 #undef LONG_DOUBLE_IS_BINARY128
#endif

// We can format binary128 values even if the local C environment
// does not support it.  However, by default, we only enable it for
// environments where binary128 == long double.
// Force enable: -DSWIFT_DTOA_BINARY128_SUPPORT=1
// Force disable: -DSWIFT_DTOA_BINARY128_SUPPORT=0
#ifndef SWIFT_DTOA_BINARY128_SUPPORT
 #if LONG_DOUBLE_IS_BINARY128
  #define SWIFT_DTOA_BINARY128_SUPPORT 1
 #endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Format a floating point value as an ASCII string
//
// Input:
// * `d` is the number to be formatted
// * `dest` is a buffer of length `length`
//
// Output:
// * Return value is the length of the string placed into `dest`
//   or zero if the buffer is too small.
// * For infinity, it copies "inf" or "-inf".
// * For NaN, it outputs a Swift-style detailed dump, including
//   sign, signaling/quiet, and payload (if any).  Typical output:
//   "nan", "-nan", "-snan(0x1234)".
// * For zero, it outputs "0.0" or "-0.0" depending on the sign.
// * The destination buffer is always null-terminated (even on error)
//   unless the length is zero.
//
// Note: If you want to customize the output for Infinity, zero, or
// Nan, you can easily write a wrapper function that uses `fpclassify`
// to identify those cases and only calls through to these functions
// for normal and subnormal values.
//
// Guarantees:
//
// * Accurate. If you parse the result back to the same floating-point
//   format via an accurate algorithm (such as Clinger's algorithm),
//   the resulting value will be _exactly_ equal to the original value.
//   On most systems, this implies that using `strtod` to parse the
//   output of `swift_dtoa_optimal_double` will yield exactly the
//   original value.
//
// * Short. No other accurate result will have fewer digits.
//
// * Close. If there are multiple possible decimal forms that are
//   both accurate and short, the form computed here will be
//   closest to the original binary value.
//
// Naming: The `_p` forms take a `const void *` pointing to the value
// in memory.  These forms do not require any support from the local C
// environment.  In particular, they should work correctly even on
// systems with no floating-point support.  Forms ending in a C
// floating-point type (e.g., "_float", "_double") are identical but
// take the corresponding argument type.  These forms obviously
// require the C environment to support passing floating-point types as
// function arguments.

#if SWIFT_DTOA_BINARY16_SUPPORT
size_t swift_dtoa_optimal_binary16_p(const void *, char *dest, size_t length);
#if !SWIFT_DTOA_PASS_FLOAT16_AS_FLOAT
// If `_Float16` is defined, provide this convenience wrapper.
size_t swift_dtoa_optimal_binary16(_Float16, char *dest, size_t length);
#endif
#endif

#if SWIFT_DTOA_BINARY32_SUPPORT
size_t swift_dtoa_optimal_binary32_p(const void *, char *dest, size_t length);
#if FLOAT_IS_BINARY32
// If `float` happens to be binary32, define the convenience wrapper.
size_t swift_dtoa_optimal_float(float, char *dest, size_t length);
#endif
#endif

#if SWIFT_DTOA_BINARY64_SUPPORT
size_t swift_dtoa_optimal_binary64_p(const void *, char *dest, size_t length);
#if DOUBLE_IS_BINARY64
// If `double` happens to be binary64, define the convenience wrapper.
size_t swift_dtoa_optimal_double(double, char *dest, size_t length);
#endif
#if LONG_DOUBLE_IS_BINARY64
// If `long double` happens to be binary64, define the convenience wrapper.
size_t swift_dtoa_optimal_long_double(long double, char *dest, size_t length);
#endif
#endif

#if SWIFT_DTOA_FLOAT80_SUPPORT
// Universal entry point works on all platforms, regardless of
// whether the local system has direct support for float80
size_t swift_dtoa_optimal_float80_p(const void *, char *dest, size_t length);
#if LONG_DOUBLE_IS_FLOAT80
// If 'long double' happens to be float80, define a convenience wrapper.
size_t swift_dtoa_optimal_long_double(long double, char *dest, size_t length);
#endif
#endif

#if SWIFT_DTOA_BINARY128_SUPPORT
// Universal entry point works on all platforms, regardless of
// whether the local system has direct support for float80
size_t swift_dtoa_optimal_binary128_p(const void *, char *dest, size_t length);
#if LONG_DOUBLE_IS_BINARY128
// If 'long double' happens to be binary128, define a convenience wrapper.
size_t swift_dtoa_optimal_long_double(long double, char *dest, size_t length);
#endif
#endif

#ifdef __cplusplus
}
#endif
#endif
