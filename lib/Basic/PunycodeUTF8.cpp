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
  InputCodePoints.resize(TargetStart - InputCodePoints.data());
  return encodePunycode(InputCodePoints, OutPunycode);
}

