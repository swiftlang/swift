//===--- Punycode.cpp - UTF-8 to Punycode transcoding -----------*- C++ -*-===//
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
// These functions implement a variant of the Punycode algorithm from RFC3492,
// originally designed for encoding international domain names, for the purpose
// encoding Swift identifiers into mangled symbol names. This version differs
// from RFC3492 in the following respects:
// - '_' is used as the encoding delimiter instead of the '-'.
// - Encoding digits are mapped to [a-zA-J] instead of to [a-z0-9], because
//   symbol names are case-sensitive, and Swift mangled identifiers cannot begin
//   with a digit.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Punycode.h"
#include "llvm/Support/ConvertUTF.h"
#include "llvm/Support/ErrorHandling.h"
#include <climits>

using namespace swift;
using namespace Punycode;

// RFC 3492
// Section 5: Parameter values for Punycode

static const int base         = 36;
static const int tmin         = 1;
static const int tmax         = 26;
static const int skew         = 38;
static const int damp         = 700;
static const int initial_bias = 72;
static const UTF32 initial_n    = 128;

static const char delimiter = '_';

static char digit_value(int digit) {
  assert(digit < base && "invalid punycode digit");
  if (digit < 26)
    return 'a' + digit;
  return 'A' - 26 + digit;
}

static int digit_index(char value) {
  if (value >= 'a' && value <= 'z')
    return value - 'a';
  if (value >= 'A' && value <= 'J')
    return value - 'A' + 26;
  return -1;
}

// Section 6.1: Bias adaptation function

static int adapt(int delta, int numpoints, bool firsttime) {
  if (firsttime)
    delta = delta / damp;
  else
    delta = delta / 2;
  
  delta += delta / numpoints;
  int k = 0;
  while (delta > ((base - tmin) * tmax) / 2) {
    delta /= base - tmin;
    k += base;
  }
  return k + (((base - tmin + 1) * delta) / (delta + skew));
}

// Section 6.2: Decoding procedure

bool Punycode::decodePunycode(StringRef inputPunycode,
                              SmallVectorImpl<char> &outUTF8) {
  // -- Build the decoded string as UTF32 first because we need random access.
  SmallVector<UTF32, 32> output;

  UTF32 n = initial_n;
  int i = 0;
  int bias = initial_bias;
  /// let output = an empty string indexed from 0
  // consume all code points before the last delimiter (if there is one)
  //  and copy them to output,
  size_t lastDelimiter = inputPunycode.find_last_of(delimiter);
  if (lastDelimiter != StringRef::npos) {
    for (char c : inputPunycode.slice(0, lastDelimiter)) {
      // fail on any non-basic code point
      if ((signed char)c < 0)
        return true;
      output.push_back((UTF32)c);
    }
    // if more than zero code points were consumed then consume one more
    //  (which will be the last delimiter)
    inputPunycode = inputPunycode.slice(lastDelimiter + 1, inputPunycode.size());
  }
  
  while (!inputPunycode.empty()) {
    int oldi = i;
    int w = 1;
    for (int k = base; ; k += base) {
      // consume a code point, or fail if there was none to consume
      if (inputPunycode.empty())
        return true;
      char codePoint = inputPunycode.front();
      inputPunycode = inputPunycode.slice(1, inputPunycode.size());
      // let digit = the code point's digit-value, fail if it has none
      int digit = digit_index(codePoint);
      if (digit < 0)
        return true;
      
      i = i + digit * w;
      int t = k <= bias ? tmin
            : k >= bias + tmax ? tmax
            : k - bias;
      if (digit < t)
        break;
      w = w * (base - t);
    }
    bias = adapt(i - oldi, output.size() + 1, oldi == 0);
    n = n + i / (output.size() + 1);
    i = i % (output.size() + 1);
    // if n is a basic code point then fail
    if (n < 0x80)
      return true;
    // insert n into output at position i
    output.insert(output.begin() + i, n);
    i++;
  }
  
  // -- Transcode to a UTF-8 result.
  size_t reserve = outUTF8.size() + output.size()*4;
  outUTF8.reserve(reserve);
  const UTF32 *utf32_begin = output.begin();
  UTF8 *utf8_begin = (UTF8*)outUTF8.end();
  auto res = ConvertUTF32toUTF8(&utf32_begin, output.end(),
                                &utf8_begin, (UTF8*)outUTF8.data() + reserve,
                                lenientConversion);
  assert(res == conversionOK && "wide-to-utf8 conversion failed!");
  (void)res;
  outUTF8.set_size(utf8_begin - (UTF8*)outUTF8.begin());
  
  return false;
}

// Section 6.3: Encoding procedure

void Punycode::encodePunycode(StringRef inputUTF8,
                              SmallVectorImpl<char> &outPunycode) {
  UTF32 n = initial_n;
  int delta = 0;
  int bias = initial_bias;
  
  // let h = b = the number of basic code points in the input
  // copy them to the output in order...
  size_t h = 0;
  SmallVector<UTF32, 32> inputCodePoints;
  for (auto *i = reinterpret_cast<UTF8 const *>(inputUTF8.begin()),
            *end = reinterpret_cast<UTF8 const *>(inputUTF8.end());
       i < end;
       ) {
    UTF32 c;
    auto conv = llvm::convertUTF8Sequence(&i, end, &c, strictConversion);
    assert(conv == conversionOK && "invalid UTF-8 input");
    (void)conv;
    inputCodePoints.push_back(c);
    if (c < 0x80) {
      ++h;
      outPunycode.push_back(c);
    }
  }
  size_t b = h;
  // ...followed by a delimiter if b > 0
  if (b > 0)
    outPunycode.push_back(delimiter);
  
  while (h < inputCodePoints.size()) {
    // let m = the minimum code point >= n in the input
    UTF32 m = 0x10FFFF;
    for (UTF32 codePoint : inputCodePoints) {
      if (codePoint >= n && codePoint < m)
        m = codePoint;
    }
    
    delta = delta + (m - n) * (h + 1);
    n = m;
    for (UTF32 c : inputCodePoints) {
      if (c < n) ++delta;
      if (c == n) {
        int q = delta;
        for (int k = base; ; k += base) {
          int t = k <= bias ? tmin
                : k >= bias + tmax ? tmax
                : k - bias;
          
          if (q < t) break;
          outPunycode.push_back(digit_value(t + ((q - t) % (base - t))));
          q = (q - t) / (base - t);
        }
        outPunycode.push_back(digit_value(q));
        bias = adapt(delta, h + 1, h == b);
        delta = 0;
        ++h;
      }
    }
    ++delta; ++n;
  }
}
