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
