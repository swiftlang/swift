//===--- LangOptions.cpp - Language & configuration options ---------------===//
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
//  This file defines the LangOptions class, which provides various
//  language and configuration flags.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LangOptions.h"
#include "swift/Basic/Range.h"

using namespace swift;

StringRef LangOptions::getTargetConfigOption(StringRef Name) {
  // Last one wins.
  for (auto &Opt : reversed(TargetConfigOptions)) {
    if (Opt.first == Name)
      return Opt.second;
  }
  return StringRef();
}

bool LangOptions::hasBuildConfigOption(StringRef Name) {
  return std::find(BuildConfigOptions.begin(), BuildConfigOptions.end(), Name)
      != BuildConfigOptions.end();
}
