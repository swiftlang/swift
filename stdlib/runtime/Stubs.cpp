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
#include <pthread.h>
#include <unistd.h>
#include <cstring>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <algorithm>
#include <sys/stat.h> // stat
#include <fcntl.h>    // open
#include <unistd.h>  // read, close
#include <dirent.h>
#include <limits.h>
#include "llvm/ADT/StringExtras.h"

// static func String(v : Int64, radix : Int) -> String
extern "C"
uint64_t
print_int(char* TmpBuffer, intptr_t buf_len, int64_t X, intptr_t Radix,
          bool uppercase) {
  assert(Radix != 0 && Radix <= 36 && "Invalid radix for string conversion");
  char *P = TmpBuffer;

  bool WasNeg = X < 0;
  __uint64_t Y = WasNeg ? -X : X;

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
      *P++ = llvm::hexdigit(Y % Radix32, !uppercase);
      Y /= Radix32;
    }
  }

  if (WasNeg) *P++ = '-';
  std::reverse(TmpBuffer, P);
  return size_t(P - TmpBuffer);
}

// static func String(v : UInt64, radix : Int) -> String
extern "C"
uint64_t
print_uint(char* TmpBuffer, intptr_t buf_len, uint64_t Y, intptr_t Radix,
           bool uppercase) {
  assert(Radix != 0 && Radix <= 36 && "Invalid radix for string conversion");
  char *P = TmpBuffer;

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
      *P++ = llvm::hexdigit(Y % Radix32, !uppercase);
      Y /= Radix32;
    }
  }

  std::reverse(TmpBuffer, P);
  return size_t(P - TmpBuffer);
}

// static func String(v : Double) -> String
extern "C"
uint64_t
print_double(char* Buffer, double X) {
  long long i = sprintf(Buffer, "%g", X);
  // Add ".0" to a float that (a) is not in scientific notation, (b) does not
  // already have a fractional part, (c) is not infinite, and (d) is not a NaN
  // value.
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr &&
      strchr(Buffer, 'n') == nullptr) {
    Buffer[i++] = '.';
    Buffer[i++] = '0';
  }
  if (i < 0) {
    __builtin_trap();
  }
  return i;
}

// FIXME: We shouldn't be writing implemenetations for functions in the swift
// module in C, and this isn't really an ideal place to put those
// implementations.
extern "C" void print_Int64(int64_t l) {
  printf("%lld", l);
}

extern "C" void print_UInt64(uint64_t l) {
  printf("%llu", l);
}

extern "C" void print_Double(double l) {
  char Buffer[256];
  uint64_t i = print_double(Buffer, l);
  Buffer[i] = '\0';
  printf("%s", Buffer);
}

extern "C"
uint32_t
swift_runningOnX86_64() {
#if __x86_64__
  return true;
#else
  return false;
#endif
}

static bool
_swift_replOutputIsUTF8(void) {
  const char *lang = getenv("LANG");
  return lang && strstr(lang, "UTF-8");
}

extern "C"
uint32_t
swift_replOutputIsUTF8(void) {
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
