//===- NativeFormatting.h - Low level formatting helpers ---------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_SUPPORT_NATIVE_FORMATTING_H
#define LLVM_SUPPORT_NATIVE_FORMATTING_H

#include "llvm/ADT/Optional.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdint>

namespace llvm {
enum class FloatStyle { Exponent, ExponentUpper, Fixed, Percent };
enum class IntegerStyle {
  Integer,
  Number,
};
enum class HexPrintStyle { Upper, Lower, PrefixUpper, PrefixLower };

size_t getDefaultPrecision(FloatStyle Style);

bool isPrefixedHexStyle(HexPrintStyle S);

void write_integer(raw_ostream &S, unsigned int N, size_t MinDigits,
                   IntegerStyle Style);
void write_integer(raw_ostream &S, int N, size_t MinDigits, IntegerStyle Style);
void write_integer(raw_ostream &S, unsigned long N, size_t MinDigits,
                   IntegerStyle Style);
void write_integer(raw_ostream &S, long N, size_t MinDigits,
                   IntegerStyle Style);
void write_integer(raw_ostream &S, unsigned long long N, size_t MinDigits,
                   IntegerStyle Style);
void write_integer(raw_ostream &S, long long N, size_t MinDigits,
                   IntegerStyle Style);

void write_hex(raw_ostream &S, uint64_t N, HexPrintStyle Style,
               Optional<size_t> Width = None);
void write_double(raw_ostream &S, double D, FloatStyle Style,
                  Optional<size_t> Precision = None);
}

#endif

