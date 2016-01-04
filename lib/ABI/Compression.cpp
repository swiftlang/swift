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

/// Extract all of the characters from the number \p Num one by one and
/// insert them into the string builder \p SB.
static void DecodeFixedWidth(APInt &Num, std::string &SB) {
  uint64_t CL = Huffman::CharsetLength;

  // NL is the number of characters that we can hold in a 64bit number.
  // Each letter takes Log2(CL) bits. Doing this computation in floating-
  // point arithmetic could give a slightly better (more optimistic) result,
  // but the computation may not be constant at compile time.
  uint64_t NumLetters = 64 / Log2_64_Ceil(CL);

  assert(Num.getBitWidth() > 8 &&
         "Not enough bits for arithmetic on this alphabet");

  // Try to decode eight numbers at once. It is much faster to work with
  // local 64bit numbers than working with APInt. In this loop we try to
  // extract NL characters at once and process them using a local 64-bit
  // number.

  // Calculate CharsetLength**NumLetters (CL to the power of NL), which is the
  // highest numeric value that can hold NumLetters characters in a 64bit
  // number. Notice: this loop is optimized away and CLX is computed to a
  // constant integer at compile time.
  uint64_t CLX = 1;
  for (unsigned  i = 0; i < NumLetters; i++) { CLX *= CL; }

  while (Num.ugt(CLX)) {
    unsigned BW = Num.getBitWidth();
    APInt C = APInt(BW, CLX);
    APInt Quotient(1, 0), Remainder(1, 0);
    APInt::udivrem(Num, C, Quotient, Remainder);

    // Try to reduce the bitwidth of the API after the division. This can
    // accelerate the division operation in future iterations because the
    // number becomes smaller (fewer bits) with each iteration. However,
    // We can't reduce the number to something too small because we still
    // need to be able to perform the "mod charset_length" operation.
    Num = Quotient.zextOrTrunc(std::max(Quotient.getActiveBits(), 64u));
    uint64_t Tail = Remainder.getZExtValue();
    for (unsigned i = 0; i < NumLetters; i++) {
      SB += Huffman::Charset[Tail % CL];
      Tail = Tail / CL;
    }
  }

  // Pop characters out of the APInt one by one.
  while (Num.getBoolValue()) {
    unsigned BW = Num.getBitWidth();

    APInt C = APInt(BW, CL);
    APInt Quotient(1, 0), Remainder(1, 0);
    APInt::udivrem(Num, C, Quotient, Remainder);
    Num = Quotient;
    SB += Huffman::Charset[Remainder.getZExtValue()];
  }
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
  // Allocate enough space for the first character plus one bit which is the
  // stop bit for variable length encoding.
  unsigned BW = (1 + Huffman::LongestEncodingLength);
  APInt num = APInt(BW, 0);

  // We set the high bit to zero in order to support encoding
  // of chars that start with zero (for variable length encoding).
  if (Kind == EncodingKind::Variable) {
    num = ++num;
  }

  // Encode variable-length strings.
  if (Kind == EncodingKind::Variable) {
    size_t num_bits = 0;
    size_t bits = 0;

    // Append the characters in the string in reverse. This will allow
    // us to decode by appending to a string and not prepending.
    for (int i = In.size() - 1; i >= 0; i--) {
      char ch = In[i];

      // The local variables 'bits' and 'num_bits' are used as a small
      // bitstream. Keep accumulating bits into them until they overflow.
      // At that point move them into the APInt.
      uint64_t local_bits;
      uint64_t local_num_bits;
      // Find the huffman encoding of the character.
      Huffman::variable_encode(local_bits, local_num_bits, ch);
      // Add the encoded character into our bitstream.
      num_bits += local_num_bits;
      bits = (bits << local_num_bits) + local_bits;

      // Check if there is enough room for another word. If not, flush
      // the local bitstream into the APInt.
      if (num_bits >= (64 - Huffman::LongestEncodingLength)) {
        // Make room for the new bits and add the bits.
        num = num.zext(num.getBitWidth() + num_bits);
        num = num.shl(num_bits); num = num + bits;
        num_bits = 0; bits = 0;
      }
    }

    // Flush the local bitstream into the APInt number.
    if (num_bits) {
      num = num.zext(num.getBitWidth() + num_bits);
      num = num.shl(num_bits); num = num + bits;
      num_bits = 0; bits = 0;
    }

    // Make sure that we have a minimal word size to be able to perform
    // calculations on our alphabet.
    return num.zextOrSelf(std::max(64u, num.getBitWidth()));
  }

  // Encode fixed width strings.
  for (int i = In.size() - 1; i >= 0; i--) {
    char ch = In[i];
    // Extend the number and create room for encoding another character.
    unsigned MinBits = num.getActiveBits() + Huffman::LongestEncodingLength;
    num = num.zextOrTrunc(std::max(64u, MinBits));
    EncodeFixedWidth(num, ch);
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
    DecodeFixedWidth(num, sb);
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

