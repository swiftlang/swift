//===--- PunycodeUTF8.cpp - Unicode to Punycode transcoding ---------------===//
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

#include "swift/Basic/Punycode.h"
#include "swift/Basic/ManglingUtils.h"
#include <vector>

using namespace swift;

static bool isContinuationByte(uint8_t unit) {
  return (unit & 0xC0) == 0x80;
}

/// Reencode well-formed UTF-8 as UTF-32.
///
/// This entry point is only called from compiler-internal entry points, so does
/// only minimal validation. In particular, it does *not* check for overlong
/// encodings.
/// If \p mapNonSymbolChars is true, non-symbol ASCII characters (characters
/// except [$_a-zA-Z0-9]) are also encoded like non-ASCII unicode characters.
/// Returns false if \p InputUTF8 contains surrogate code points.
static bool convertUTF8toUTF32(StringRef InputUTF8,
                               std::vector<uint32_t> &OutUTF32,
                               bool mapNonSymbolChars) {
  auto ptr = InputUTF8.begin();
  auto end = InputUTF8.end();
  while (ptr < end) {
    uint8_t first = *ptr++;
    if (first < 0x80) {
      if (NewMangling::isValidSymbolChar(first) || !mapNonSymbolChars) {
        OutUTF32.push_back(first);
      } else {
        OutUTF32.push_back((uint32_t)first + 0xD800);
      }
    } else if (first < 0xC0) {
      // Invalid continuation byte.
      return false;
    } else if (first < 0xE0) {
      // Two-byte sequence.
      if (ptr == end)
        return false;
      uint8_t second = *ptr++;
      if (!isContinuationByte(second))
        return false;
      OutUTF32.push_back(((first & 0x1F) << 6) | (second & 0x3F));
    } else if (first < 0xF0) {
      // Three-byte sequence.
      if (end - ptr < 2)
        return false;
      uint8_t second = *ptr++;
      uint8_t third = *ptr++;
      if (!isContinuationByte(second) || !isContinuationByte(third))
        return false;
      OutUTF32.push_back(((first & 0xF) << 12) | ((second & 0x3F) << 6)
                                               | ( third  & 0x3F      ));
    } else if (first < 0xF8) {
      // Four-byte sequence.
      if (end - ptr < 3)
        return false;
      uint8_t second = *ptr++;
      uint8_t third = *ptr++;
      uint8_t fourth = *ptr++;
      if (!isContinuationByte(second) || !isContinuationByte(third)
          || !isContinuationByte(fourth))
        return false;
      OutUTF32.push_back(((first & 0x7) << 18) | ((second & 0x3F) << 12)
                                               | ((third  & 0x3F) <<  6)
                                               | ( fourth & 0x3F       ));
    } else {
      // Unused sequence length.
      return false;
    }
  }
  return true;
}

bool Punycode::encodePunycodeUTF8(StringRef InputUTF8,
                                  std::string &OutPunycode,
                                  bool mapNonSymbolChars) {
  std::vector<uint32_t> InputCodePoints;
  InputCodePoints.reserve(InputUTF8.size());
  
  if (!convertUTF8toUTF32(InputUTF8, InputCodePoints, mapNonSymbolChars))
    return false;
  
  return encodePunycode(InputCodePoints, OutPunycode);
}

