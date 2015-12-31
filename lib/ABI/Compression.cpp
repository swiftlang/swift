//===--- Compression.cpp - Compression of symbols -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/Compression.h"
#include "CBCTables.h"
#include "HuffTables.h"
#include <assert.h>

using namespace llvm;
using namespace swift;

using EncodingKind = swift::Compress::EncodingKind;

static unsigned CBCindexOfChar(char c) {
  int idx = CBC::IndexOfChar[int(c)];
  assert(idx >= 0 && "Invalid char");
  return (unsigned) idx;
}

std::string swift::Compress::DecodeCBCString(StringRef In) {
  unsigned EndIndex = In.size();

  // The String Builder.
  std::string SB;
  SB.reserve(In.size());

  // Processed Index - The index of the first non-processed char.
  unsigned PI = 0;

  while (true) {
    if (PI >= EndIndex) break;
    const char c = In[PI];
    if (c == CBC::EscapeChar0) {
      if ((PI+1) >= EndIndex) {
        assert(false && "Invalid Encoding");
        return "";
      }
      const char N = In[PI+1];

      if (N == CBC::EscapeChar0 || N == CBC::EscapeChar1) {
        SB += N;
        PI += 2;
        continue;
      }

      unsigned Idx = CBCindexOfChar(N);
      if (Idx > CBC::CharsetLength || CBC::Charset[Idx] != N) {
        assert(false && "bad indexOfChar");
        return "";
      }
      SB += CBC::CodeBook[Idx];
      PI += 2;
      continue;
    }

    if (c == CBC::EscapeChar1) {
      if ((PI+2) >= EndIndex) {
        assert(false && "Invalid Encoding");
        return "";
      }

      const char N0 = In[PI+1];
      const char N1 = In[PI+2];
      unsigned JointIndex = (CBC::CharsetLength * CBCindexOfChar(N0)) +
                                                  CBCindexOfChar(N1);
      if (JointIndex > CBC::NumFragments) {
        assert(false && "Read bad index");
        return "";
      }
      SB += CBC::CodeBook[JointIndex];
      PI += 3;
      continue;
    }
    // We did not find a match. Just decode the character.
    SB += c;
    PI++;
  }

  return SB;
}

std::string swift::Compress::EncodeCBCString(StringRef In) {
  unsigned EndIndex = In.size();

  // The String Builder.
  std::string SB;
  SB.reserve(In.size());

  // Processed Index - The index of the first non-processed char.
  unsigned PI = 0;

  while (true) {
StartMatch:
    if (PI >= EndIndex) break;

    const char c = In[PI];
    if (c == CBC::EscapeChar0 || c == CBC::EscapeChar1) {
      SB += CBC::EscapeChar0;
      SB += c;
      PI++;
      goto StartMatch;
    }

    // The number of unprocessed chars.
    unsigned DistanceToEnd = EndIndex - PI;
    int FragIdx = CBC::matchStringSuffix(In.data() + PI, DistanceToEnd);

    if (FragIdx >= 0){
        unsigned FragmentLength = CBC::CodeBookLen[FragIdx];
        // Try encoding using a single escape character.
        if (FragIdx < int(CBC::CharsetLength)) {
          SB += CBC::EscapeChar0;
          SB += CBC::Charset[FragIdx];
          PI += FragmentLength;
          goto StartMatch;
        }

        // Try encoding using two escape character. Make sure that the encoding
        // is profitable.
        if (FragmentLength > 3) {
          SB += CBC::EscapeChar1;
          SB += CBC::Charset[FragIdx / CBC::CharsetLength];
          SB += CBC::Charset[FragIdx % CBC::CharsetLength];
          PI += FragmentLength;
          goto StartMatch;
        }
      }

    // We did not find a match. Just save the character. :(
    SB += c;
    PI++;
  }

  return SB;
}

static char DecodeFixedWidth(APInt &num) {
  unsigned BW = num.getBitWidth();
  assert(BW > 16 &&
         "The APInt needs to be large enough for arithmetic on CharsetLength");
  APInt C = APInt(BW, Huffman::CharsetLength);

  APInt Quotient(BW, 0), Remainder(BW, 0);
  APInt::udivrem(num, C, Quotient, Remainder);
  num = Quotient.zext(BW);
  return Huffman::Charset[Remainder.getZExtValue()];
}

static void EncodeFixedWidth(APInt &num, char ch) {
  APInt C = APInt(num.getBitWidth(), Huffman::CharsetLength);
  // TODO: autogenerate a table for the reverse lookup.
  for (unsigned i = 0; i < Huffman::CharsetLength; i++) {
    if (Huffman::Charset[i] == ch) {
      num *= C;
      num += APInt(num.getBitWidth(), i);
      return;
    }
  }
  assert(false);
}


APInt
swift::Compress::EncodeStringAsNumber(StringRef In, EncodingKind Kind) {
  unsigned BW = std::max(32u, (unsigned) In.size() *
                         Huffman::LongestEncodingLength);
    APInt num = APInt(BW, 0);

  // We set the high bit to zero in order to support encoding
  // of chars that start with zero (for variable length encoding).
  if (Kind == EncodingKind::Variable) {
    num = ++num;
  }

  // Append the characters in the string in reverse. This will allow
  // us to decode by appending to a string and not prepending.
  for (int i = In.size() - 1; i >= 0; i--) {
    char ch = In[i];
    if (Kind == EncodingKind::Variable) {
      Huffman::variable_encode(num, ch);
    } else  {
      EncodeFixedWidth(num, ch);
    }
  }
  return num;

}

std::string swift::Compress::DecodeStringFromNumber(const APInt &In,
                                                    EncodingKind Kind) {
  APInt num = In;
  std::string sb;

  if (Kind == EncodingKind::Variable) {
    // Keep decoding until we reach our sentinel value.
    // See the encoder implementation for more details.
    while (num.ugt(1)) {
      sb += Huffman::variable_decode(num);
    }
  } else {
    // Decode this number as a regular fixed-width sequence of characters.
    while (num.getBoolValue()) {
      sb += DecodeFixedWidth(num);
    }
  }

  return sb;
}

std::string swift::Compress::CompressName(StringRef In) {
   std::string cbc = EncodeCBCString(In);
   APInt num = EncodeStringAsNumber(cbc, EncodingKind::Variable);
   return DecodeStringFromNumber(num, EncodingKind::Fixed);
}

std::string swift::Compress::DecompressName(StringRef In) {
   APInt num = EncodeStringAsNumber(In, EncodingKind::Fixed);
   std::string str = DecodeStringFromNumber(num, EncodingKind::Variable);
   return DecodeCBCString(str);
}

