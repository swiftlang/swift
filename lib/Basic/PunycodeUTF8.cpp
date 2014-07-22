//===--- PunycodeUTF8.cpp - Unicode to Punycode transcoding -----*- C++ -*-===//
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

#include "swift/Basic/Punycode.h"
#include "llvm/Support/ConvertUTF.h"
#include <vector>

using namespace swift;

bool Punycode::encodePunycodeUTF8(StringRef InputUTF8,
                                  std::string &OutPunycode) {
  std::vector<uint32_t> InputCodePoints(InputUTF8.size());
  const UTF8 *SourceStart = reinterpret_cast<const UTF8 *>(InputUTF8.data());
  UTF32 *TargetStart = InputCodePoints.data();
  auto ConvStatus = ConvertUTF8toUTF32(
      &SourceStart, SourceStart + InputUTF8.size(), &TargetStart,
      InputCodePoints.data() + InputCodePoints.size(), strictConversion);
  if (ConvStatus != conversionOK)
    return false;
  return encodePunycode(InputCodePoints, OutPunycode);
}

bool Punycode::decodePunycodeUTF8(StringRef InputPunycode,
                                  std::string &OutUTF8) {
  std::vector<uint32_t> OutCodePoints;
  if (!decodePunycode(InputPunycode, OutCodePoints))
    return false;

  const size_t SizeUpperBound = OutCodePoints.size() * 4;
  std::vector<UTF8> Result(SizeUpperBound);
  const UTF32 *SourceStart = OutCodePoints.data();
  UTF8 *TargetStart = Result.data();
  auto ConvStatus = ConvertUTF32toUTF8(
      &SourceStart, SourceStart + OutCodePoints.size(), &TargetStart,
      Result.data() + Result.size(), strictConversion);
  if (ConvStatus != conversionOK) {
    OutUTF8.clear();
    return false;
  }
  OutUTF8 = std::string(Result.data(), TargetStart);
  return true;
}

