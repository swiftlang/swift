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

typedef int si_int;
typedef int di_int SWIFT_MODE_DI;
typedef int ti_int SWIFT_MODE_TI;

typedef unsigned su_int;
typedef unsigned du_int SWIFT_MODE_DI;
typedef unsigned tu_int SWIFT_MODE_TI;

typedef union
{
    tu_int all;
    struct
    {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
        du_int low;
        du_int high;
#else
        du_int high;
        du_int low;
#endif /* __BYTE_ORDER__ == __LITTLE_ENDIAN__ */
    }s;
} utwords;

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

#if defined(_WIN32)

tu_int
__udivmodti4(tu_int a, tu_int b, tu_int* rem)
{
    const unsigned n_udword_bits = sizeof(du_int) * CHAR_BIT;
    const unsigned n_utword_bits = sizeof(tu_int) * CHAR_BIT;
    utwords n;
    n.all = a;
    utwords d;
    d.all = b;
    utwords q;
    utwords r;
    unsigned sr;
    /* special cases, X is unknown, K != 0 */
    if (n.s.high == 0)
    {
        if (d.s.high == 0)
        {
            /* 0 X
             * ---
             * 0 X
             */
            if (rem)
                *rem = n.s.low % d.s.low;
            return n.s.low / d.s.low;
        }
        /* 0 X
         * ---
         * K X
         */
        if (rem)
            *rem = n.s.low;
        return 0;
    }
    /* n.s.high != 0 */
    if (d.s.low == 0)
    {
        if (d.s.high == 0)
        {
            /* K X
             * ---
             * 0 0
             */
            if (rem)
                *rem = n.s.high % d.s.low;
            return n.s.high / d.s.low;
        }
        /* d.s.high != 0 */
        if (n.s.low == 0)
        {
            /* K 0
             * ---
             * K 0
             */
            if (rem)
            {
                r.s.high = n.s.high % d.s.high;
                r.s.low = 0;
                *rem = r.all;
            }
            return n.s.high / d.s.high;
        }
        /* K K
         * ---
         * K 0
         */
        if ((d.s.high & (d.s.high - 1)) == 0)     /* if d is a power of 2 */
        {
            if (rem)
            {
                r.s.low = n.s.low;
                r.s.high = n.s.high & (d.s.high - 1);
                *rem = r.all;
            }
            return n.s.high >> __builtin_ctzll(d.s.high);
        }
        /* K K
         * ---
         * K 0
         */
        sr = __builtin_clzll(d.s.high) - __builtin_clzll(n.s.high);
        /* 0 <= sr <= n_udword_bits - 2 or sr large */
        if (sr > n_udword_bits - 2)
        {
           if (rem)
                *rem = n.all;
            return 0;
        }
        ++sr;
        /* 1 <= sr <= n_udword_bits - 1 */
        /* q.all = n.all << (n_utword_bits - sr); */
        q.s.low = 0;
        q.s.high = n.s.low << (n_udword_bits - sr);
        /* r.all = n.all >> sr; */
        r.s.high = n.s.high >> sr;
        r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
    }
    else  /* d.s.low != 0 */
    {
        if (d.s.high == 0)
        {
            /* K X
             * ---
             * 0 K
             */
            if ((d.s.low & (d.s.low - 1)) == 0)     /* if d is a power of 2 */
            {
                if (rem)
                    *rem = n.s.low & (d.s.low - 1);
                if (d.s.low == 1)
                    return n.all;
                sr = __builtin_ctzll(d.s.low);
                q.s.high = n.s.high >> sr;
                q.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
                return q.all;
            }
            /* K X
             * ---
             * 0 K
             */
            sr = 1 + n_udword_bits + __builtin_clzll(d.s.low)
                                   - __builtin_clzll(n.s.high);
            /* 2 <= sr <= n_utword_bits - 1
             * q.all = n.all << (n_utword_bits - sr);
             * r.all = n.all >> sr;
             */
            if (sr == n_udword_bits)
            {
                q.s.low = 0;
                q.s.high = n.s.low;
                r.s.high = 0;
                r.s.low = n.s.high;
            }
            else if (sr < n_udword_bits)  // 2 <= sr <= n_udword_bits - 1
            {
                q.s.low = 0;
                q.s.high = n.s.low << (n_udword_bits - sr);
                r.s.high = n.s.high >> sr;
                r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
            }
            else              // n_udword_bits + 1 <= sr <= n_utword_bits - 1
            {
                q.s.low = n.s.low << (n_utword_bits - sr);
                q.s.high = (n.s.high << (n_utword_bits - sr)) |
                           (n.s.low >> (sr - n_udword_bits));
                r.s.high = 0;
                r.s.low = n.s.high >> (sr - n_udword_bits);
            }
        }
        else
        {
            /* K X
             * ---
             * K K
             */
            sr = __builtin_clzll(d.s.high) - __builtin_clzll(n.s.high);
            /*0 <= sr <= n_udword_bits - 1 or sr large */
            if (sr > n_udword_bits - 1)
            {
               if (rem)
                    *rem = n.all;
                return 0;
            }
            ++sr;
            /* 1 <= sr <= n_udword_bits
             * q.all = n.all << (n_utword_bits - sr);
             * r.all = n.all >> sr;
             */
            q.s.low = 0;
            if (sr == n_udword_bits)
            {
                q.s.high = n.s.low;
                r.s.high = 0;
                r.s.low = n.s.high;
            }
            else
            {
                r.s.high = n.s.high >> sr;
                r.s.low = (n.s.high << (n_udword_bits - sr)) | (n.s.low >> sr);
                q.s.high = n.s.low << (n_udword_bits - sr);
            }
        }
    }
    /* Not a special case
     * q and r are initialized with:
     * q.all = n.all << (n_utword_bits - sr);
     * r.all = n.all >> sr;
     * 1 <= sr <= n_utword_bits - 1
     */
    su_int carry = 0;
    for (; sr > 0; --sr)
    {
        /* r:q = ((r:q)  << 1) | carry */
        r.s.high = (r.s.high << 1) | (r.s.low  >> (n_udword_bits - 1));
        r.s.low  = (r.s.low  << 1) | (q.s.high >> (n_udword_bits - 1));
        q.s.high = (q.s.high << 1) | (q.s.low  >> (n_udword_bits - 1));
        q.s.low  = (q.s.low  << 1) | carry;
        /* carry = 0;
         * if (r.all >= d.all)
         * {
         *     r.all -= d.all;
         *      carry = 1;
         * }
         */
        const ti_int s = (ti_int)(d.all - r.all - 1) >> (n_utword_bits - 1);
        carry = s & 1;
        r.all -= d.all & s;
    }
    q.all = (q.all << 1) | carry;
    if (rem)
        *rem = r.all;
    return q.all;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
tu_int
__udivti3(tu_int a, tu_int b)
{
    return __udivmodti4(a, b, NULL);
}

SWIFT_RUNTIME_STDLIB_INTERFACE
tu_int
__umodti3(tu_int a, tu_int b)
{
    tu_int r;
    __udivmodti4(a, b, &r);
    return r;
}

SWIFT_RUNTIME_STDLIB_INTERFACE
ti_int
__divti3(ti_int a, ti_int b)
{
    const int bits_in_tword_m1 = (int)(sizeof(ti_int) * CHAR_BIT) - 1;
    ti_int s_a = a >> bits_in_tword_m1;                   /* s_a = a < 0 ? -1 : 0 */
    ti_int s_b = b >> bits_in_tword_m1;                   /* s_b = b < 0 ? -1 : 0 */
    a = (a ^ s_a) - s_a;                                  /* negate if s_a == -1 */
    b = (b ^ s_b) - s_b;                                  /* negate if s_b == -1 */
    s_a ^= s_b;                                           /* sign of quotient */
    return (__udivmodti4(a, b, (tu_int*)0) ^ s_a) - s_a;  /* negate if s_a == -1 */
}

SWIFT_RUNTIME_STDLIB_INTERFACE
ti_int
__modti3(ti_int a, ti_int b)
{
    const int bits_in_tword_m1 = (int)(sizeof(ti_int) * CHAR_BIT) - 1;
    ti_int s = b >> bits_in_tword_m1;  /* s = b < 0 ? -1 : 0 */
    b = (b ^ s) - s;                   /* negate if s == -1 */
    s = a >> bits_in_tword_m1;         /* s = a < 0 ? -1 : 0 */
    a = (a ^ s) - s;                   /* negate if s == -1 */
    tu_int r;
    __udivmodti4(a, b, &r);
    return ((ti_int)r ^ s) - s;        /* negate if s == -1 */
}

#endif

}

