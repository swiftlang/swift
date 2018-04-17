//===--- MathStubs.cpp - Swift Language Runtime Stubs ---------------------===//
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
//
// Math stubs for functions which should be defined in the core standard
// library, but are difficult or impossible to write in Swift at the
// moment.
//
//===----------------------------------------------------------------------===//

#include "../SwiftShims/Visibility.h"

#include <climits>
#include <cstdlib>

#if __has_attribute(__mode__)
#define SWIFT_MODE_DI __attribute__((__mode__(DI)))
#define SWIFT_MODE_TI __attribute__((__mode__(TI)))
#else
#define SWIFT_MODE_DI
#define SWIFT_MODE_TI
#endif

typedef int di_int SWIFT_MODE_DI;
typedef int ti_int SWIFT_MODE_TI;

extern "C" {

// Although this builtin is provided by clang rt builtins,
// it isn't provided by libgcc, which is the default
// runtime library on Linux, even when compiling with clang.
// This implementation is copied here to avoid a new dependency
// on compiler-rt on Linux.
// FIXME: rdar://14883575 Libcompiler_rt omits muloti4
#if (defined(__linux__) && defined(__x86_64__)) || \
    (defined(__linux__) && defined(__aarch64__)) || \
    (defined(__linux__) && defined(__powerpc64__)) || \
    (defined(__linux__) && defined(__s390x__)) || \
    (defined(__ANDROID__) && defined(__arm64__))

SWIFT_RUNTIME_STDLIB_INTERFACE
ti_int
__muloti4(ti_int a, ti_int b, int* overflow)
{
    const int N = (int)(sizeof(ti_int) * CHAR_BIT);
    const ti_int MIN = (ti_int)1 << (N-1);
    const ti_int MAX = ~MIN;
    *overflow = 0;
    ti_int result = a * b;
    if (a == MIN)
    {
        if (b != 0 && b != 1)
            *overflow = 1;
        return result;
    }
    if (b == MIN)
    {
        if (a != 0 && a != 1)
            *overflow = 1;
        return result;
    }
    ti_int sa = a >> (N - 1);
    ti_int abs_a = (a ^ sa) - sa;
    ti_int sb = b >> (N - 1);
    ti_int abs_b = (b ^ sb) - sb;
    if (abs_a < 2 || abs_b < 2)
        return result;
    if (sa == sb)
    {
        if (abs_a > MAX / abs_b)
            *overflow = 1;
    }
    else
    {
        if (abs_a > MIN / -abs_b)
            *overflow = 1;
    }
    return result;
}

#endif

// FIXME: ideally we would have a slow path here for Windows which would be
// lowered to instructions as though MSVC had generated.  There does not seem to
// be a MSVC provided multiply with overflow detection that I can see, but this
// avoids an unnecessary dependency on compiler-rt for a single function.
#if (defined(__linux__) && defined(__arm__)) || defined(_WIN32)

// Similar to above, but with mulodi4.  Perhaps this is
// something that shouldn't be done, and is a bandaid over
// some other lower-level architecture issue that I'm
// missing.  Perhaps relevant bug report:
// FIXME: https://llvm.org/bugs/show_bug.cgi?id=14469

SWIFT_RUNTIME_STDLIB_INTERFACE
di_int
__mulodi4(di_int a, di_int b, int* overflow)
{
    const int N = (int)(sizeof(di_int) * CHAR_BIT);
    const di_int MIN = (di_int)1 << (N-1);
    const di_int MAX = ~MIN;
    *overflow = 0;
    di_int result = a * b;
    if (a == MIN)
    {
        if (b != 0 && b != 1)
            *overflow = 1;
        return result;
    }
    if (b == MIN)
    {
        if (a != 0 && a != 1)
            *overflow = 1;
        return result;
    }
    di_int sa = a >> (N - 1);
    di_int abs_a = (a ^ sa) - sa;
    di_int sb = b >> (N - 1);
    di_int abs_b = (b ^ sb) - sb;
    if (abs_a < 2 || abs_b < 2)
        return result;
    if (sa == sb)
    {
        if (abs_a > MAX / abs_b)
            *overflow = 1;
    }
    else
    {
        if (abs_a > MIN / -abs_b)
            *overflow = 1;
    }
    return result;
}

#endif

}

