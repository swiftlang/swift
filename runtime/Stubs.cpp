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
#include "llvm/ADT/StringExtras.h"

// FIXME: We shouldn't be writing implemenetations for functions in the swift
// module in C, and this isn't really an ideal place to put those
// implementations.
extern "C" void _TSs5printFT3valNSs5Int64_T_(int64_t l) {
  printf("%lld", l);
}

extern "C" void _TSs5printFT3valNSs6Double_T_(double l) {
  char Buffer[256];
  sprintf(Buffer, "%g", l);
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr)
    strcat(Buffer, ".0");
  printf("%s", Buffer);
}

// String implementation.

// func [infix_left=190] + (lhs : String,
//                          rhs : String) -> String
extern "C" char* _TSsop1pFT3lhsNSs6String3rhsS__S_(char* lhs, char* rhs) {
   size_t ls = strlen(lhs);
   size_t rs = strlen(rhs);
   char* s = (char*)malloc(ls+rs+1);
   memcpy(s, lhs, ls);
   strcpy(s+ls, rhs);
   return s;
}

// static func String(v : Int128, radix : Int) -> String
extern "C" char *
_TNSs6String6StringFT1vNSs6Int1285radixNSs5Int64_S_(__int128_t X,
                                                    uint64_t Radix) {
  assert(Radix != 0 && Radix <= 36 && "Invalid radix for string conversion");
         
  char TmpBuffer[128];
  char *P = TmpBuffer+128;
  
  *--P = 0; // Null terminate buffer.
  
  bool WasNeg = X < 0;
  __uint128_t Y = WasNeg ? -X : X;

  if (Y == 0) {
    *--P = '0';  // Special case. 
  } else if (Radix == 10) {  // Special case for 10, since we care so much about performance right now.
    while (Y) {
      *--P = '0' + char(Y % 10);
      Y /= 10;
    }
  } else {
    unsigned Radix32 = Radix;
    while (Y) {
      *--P = llvm::hexdigit(Y % Radix32);
      Y /= Radix32;
    }
  }
  
  if (WasNeg) *--P = '-';
  return strdup(P);
}

// static func String(v : Double) -> String
extern "C" char *_TNSs6String6StringFT1vNSs6Double_S_(double X) {
  char Buffer[256];
  sprintf(Buffer, "%g", X);
  if (strchr(Buffer, 'e') == nullptr && strchr(Buffer, '.') == nullptr)
    strcat(Buffer, ".0");
  return strdup(Buffer);
}

extern "C" bool _TNSs4Bool13getLogicValuefRS_FT_i1(bool* b) {
  return *b;
}

