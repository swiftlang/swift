//===--- Stubs.cpp - Swift Language ABI Runtime Stubs ---------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Misc stubs for functions which should be in swift.swift, but are difficult
// or impossible to write in swift at the moment.
//
//===----------------------------------------------------------------------===//

#include <mach/mach_time.h>
#include <sys/resource.h>
#include <sys/errno.h>
#include <unistd.h>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "llvm/ADT/StringExtras.h"
#include "Debug.h"

static uint64_t uint64ToStringImpl(char *Buffer, uint64_t Value,
                                   int64_t Radix, bool Uppercase,
                                   bool Negative) {
  char *P = Buffer;
  uint64_t Y = Value;

  if (Y == 0) {
    *P++ = '0';
  } else if (Radix == 10) {
    while (Y) {
      *P++ = '0' + char(Y % 10);
      Y /= 10;
    }
  } else {
    unsigned Radix32 = Radix;
    while (Y) {
      *P++ = llvm::hexdigit(Y % Radix32, !Uppercase);
      Y /= Radix32;
    }
  }

  if (Negative)
    *P++ = '-';
  std::reverse(Buffer, P);
  return size_t(P - Buffer);
}

extern "C" uint64_t swift_int64ToString(char *Buffer, size_t BufferLength,
                                        int64_t Value, int64_t Radix,
                                        bool Uppercase) {
  if ((Radix >= 10 && BufferLength < 32) || (Radix < 10 && BufferLength < 65))
    swift::crash("swift_int64ToString: insufficient buffer size");

  if (Radix == 0 || Radix > 36)
    swift::crash("swift_int64ToString: invalid radix for string conversion");

  bool Negative = Value < 0;

  // Compute an absolute value safely, without using unary negation on INT_MIN,
  // which is undefined behavior.
  uint64_t UnsignedValue = Value;
  if (Negative) {
    // Assumes two's complement representation.
    UnsignedValue = ~UnsignedValue + 1;
  }

  return uint64ToStringImpl(Buffer, UnsignedValue, Radix, Uppercase,
                            Negative);
}

extern "C" uint64_t swift_uint64ToString(char *Buffer, intptr_t BufferLength,
                                         uint64_t Value, int64_t Radix,
                                         bool Uppercase) {
  if ((Radix >= 10 && BufferLength < 32) || (Radix < 10 && BufferLength < 64))
    swift::crash("swift_int64ToString: insufficient buffer size");

  if (Radix == 0 || Radix > 36)
    swift::crash("swift_int64ToString: invalid radix for string conversion");

  return uint64ToStringImpl(Buffer, Value, Radix, Uppercase,
                            /*Negative=*/false);
}

extern "C" uint64_t swift_doubleToString(char *Buffer, size_t BufferLength,
                                         double Value) {
  if (BufferLength < 32)
    swift::crash("swift_doubleToString: insufficient buffer size");

  int i = snprintf(Buffer, BufferLength, "%0.15g", Value);
  if (i < 0)
    swift::crash(
        "swift_doubleToString: unexpected return value from sprintf");
  if (size_t(i) >= BufferLength)
    swift::crash("swift_doubleToString: insufficient buffer size");
  
  // Add ".0" to a float that (a) is not in scientific notation, (b) does not
  // already have a fractional part, (c) is not infinite, and (d) is not a NaN
  // value.
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr &&
      strchr(Buffer, 'n') == nullptr) {
    Buffer[i++] = '.';
    Buffer[i++] = '0';
  }
  
  return i;
}

static bool _swift_replOutputIsUTF8(void) {
  const char *lang = getenv("LANG");
  return lang && strstr(lang, "UTF-8");
}

extern "C" uint32_t swift_replOutputIsUTF8(void) {
  static auto rval = _swift_replOutputIsUTF8();
  return rval;
}

#if __arm64__

// FIXME: rdar://14883575 Libcompiler_rt omits muloti4
typedef int      ti_int __attribute__ ((mode (TI)));
extern "C"
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
