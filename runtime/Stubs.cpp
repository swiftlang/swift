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
#include "llvm/ADT/StringExtras.h"

// FIXME: We shouldn't be writing implemenetations for functions in the swift
// module in C, and this isn't really an ideal place to put those
// implementations.
extern "C" void _TSs5printFT3valNSs5Int64_T_(int64_t l) {
  printf("%lld", l);
}

extern "C" void _TSs5printFT3valNSs6UInt64_T_(uint64_t l) {
  printf("%llu", l);
}

extern "C" void _TSs5printFT3valNSs6Double_T_(double l) {
  char Buffer[256];
  sprintf(Buffer, "%g", l);
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr)
    strcat(Buffer, ".0");
  printf("%s", Buffer);
}

// static func String(v : Int128, radix : Int) -> String
extern "C"
unsigned long long
print_int(char* TmpBuffer, __int64_t buf_len, __int128_t X, uint64_t Radix) {
  assert(Radix != 0 && Radix <= 36 && "Invalid radix for string conversion");
  char *P = TmpBuffer;
  
  bool WasNeg = X < 0;
  __uint128_t Y = WasNeg ? -X : X;

  if (Y == 0) {
    *P++ = '0';  // Special case. 
  } else if (Radix == 10) {  // Special case for 10, since we care so much about performance right now.
    while (Y) {
      *P++ = '0' + char(Y % 10);
      Y /= 10;
    }
  } else {
    unsigned Radix32 = Radix;
    while (Y) {
      *P++ = llvm::hexdigit(Y % Radix32);
      Y /= Radix32;
    }
  }
  
  if (WasNeg) *P++ = '-';
  std::reverse(TmpBuffer, P);
  return size_t(P - TmpBuffer);
}

// static func String(v : Double) -> String
extern "C"
unsigned long long
print_double(char* Buffer, double X) {
  long long i = sprintf(Buffer, "%g", X);
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr) {
    Buffer[i++] = '.';
    Buffer[i++] = '0';
  }
  if (i < 0) {
    __builtin_trap();
  }
  return i;
}

extern "C" bool _TNSs4Bool13getLogicValuefRS_FT_i1(bool* b) {
  return *b;
}

// FIXME: load_protocol and store_protocol are extremely ugly hacks.
struct protocol {
  void **witness_table;
  char buffer[24];
};

extern "C"
void
load_protocol(protocol *retval, protocol *p) {
  retval->witness_table = p->witness_table;
  typedef void* (*copyTy)(void*, void*, void**);
  copyTy initializeBufferWithCopyOfBuffer = (copyTy)(size_t)retval->witness_table[1];
  initializeBufferWithCopyOfBuffer(&retval->buffer,&p->buffer,retval->witness_table);
}

extern "C"
void
store_protocol(protocol *value, protocol *p) {
  p->witness_table = value->witness_table;
  typedef void* (*copyTy)(void*, void*, void**);
  copyTy initializeBufferWithCopyOfBuffer = (copyTy)(size_t)value->witness_table[1];
  initializeBufferWithCopyOfBuffer(&p->buffer,&value->buffer,value->witness_table);
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

#if defined(__i386__) || defined(__x86_64__)
static inline uint64_t
rdtsc() {
  uint32_t lo, hi;
  asm("rdtsc" : "=a" (lo), "=d" (hi));
  return uint64_t(hi) << 32 | lo;
}
#else
#error "not supported"
#endif

static double interruptOverhead;
static double loopOverhead;

static uint64_t
_swift_startBenchmark(void) {
  return rdtsc();
}

extern "C"
void
swift_printBenchmark(uint64_t start, uint64_t laps, char *buffer, int64_t len) {
  double val = rdtsc() - start;
  val /= laps;
  val /= interruptOverhead;
  val -= loopOverhead;
  printf("%12.2f  %*s\n", val, (int)len, buffer);
}

extern "C"
__attribute__((noinline,used))
__typeof__(&_swift_startBenchmark)
_swift_initBenchmark() asm("_swift_startBenchmark");

__typeof__(&_swift_startBenchmark)
_swift_initBenchmark() {
  asm(".symbol_resolver _swift_startBenchmark");
  union {
    unsigned reg[4*3];
    char brand[48];
  } u;
  char brand[48];
  char *s2 = u.brand;
  char *s1 = brand;
  unsigned eax, ebx, ecx, edx;
  memset(&u, 0, sizeof(u));
  int r;

  // Let's not have the OS compete with our CPU time if we can avoid it
  r = setvbuf(stdout, 0, _IOFBF, 0);
  assert(r == 0);

  // XXX -- There doesn't seem to be an API to figure out the max value
  struct sched_param pthr_sched_param;
  pthr_sched_param.sched_priority = 79;
  r = pthread_setschedparam(pthread_self(), SCHED_FIFO, &pthr_sched_param);
  assert(r == 0);

  eax = 0x80000002;
  asm("cpuid" : "+a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx));
  u.reg[0] = eax;
  u.reg[1] = ebx;
  u.reg[2] = ecx;
  u.reg[3] = edx;

  eax = 0x80000003;
  asm("cpuid" : "+a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx));
  u.reg[4] = eax;
  u.reg[5] = ebx;
  u.reg[6] = ecx;
  u.reg[7] = edx;

  eax = 0x80000004;
  asm("cpuid" : "+a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx));
  u.reg[8] = eax;
  u.reg[9] = ebx;
  u.reg[10] = ecx;
  u.reg[11] = edx;

  while (*s2 == ' ') {
    s2++;
  }
  do {
    while (s2[0] == ' ' && s2[1] == ' ') {
      s2++;
    }
  } while ((*s1++ = *s2++));
  printf("Processor: %s\n\n", brand);

  uint64_t start = rdtsc();
  for (unsigned long i = 0; i < 1000000000ull; i++) {
    asm("");
  }
  double delta = (rdtsc() - start) / 1000000000.0;
  assert((delta >= 1.0 && delta < 1.05) || (delta >= 2.0 && delta < 2.05));
  if (delta >= 2.0) {
    loopOverhead = 2.0;
    interruptOverhead = delta / 2.0;
  } else {
    loopOverhead = 1.0;
    interruptOverhead = delta / 1.0;
  }
  assert((interruptOverhead - 1.0) < 0.01);

  eax = 6;
  asm("cpuid" : "+a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx));

  if (eax & 2) {
    fprintf(stderr, "WARNING: TurboBoost. Results will be less reliable.\n");
    fprintf(stderr, "         Consider: sudo /usr/local/bin/pstates -D\n\n");
  }

  if (geteuid()) {
    fprintf(stderr, "WARNING: Non-elevated priority. Results will be less reliable.\n");
    fprintf(stderr, "         Consider: sudo ./myBench\n\n");
  }

  return _swift_startBenchmark;
}
