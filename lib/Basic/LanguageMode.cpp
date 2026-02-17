//===--- Basic/LanguageMode.cpp ---------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LanguageMode.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

bool LanguageMode::isFuture() const { return mode == AnyLanguageMode::future; }

bool LanguageMode::isEffectiveIn(
    version::Version effectiveLanguageVersion) const {
  auto majorAndMinor = version();
  return effectiveLanguageVersion.isVersionAtLeast(majorAndMinor.first,
                                                   majorAndMinor.second);
}

std::pair<unsigned, unsigned> LanguageMode::version() const {
  switch (mode) {
#define LANGUAGE_MODE(NAME, MAJOR, MINOR)                                      \
  case AnyLanguageMode::NAME:                                                  \
    return {MAJOR, MINOR};
  case AnyLanguageMode::future:
    return {version::Version::getFutureMajorLanguageVersion(), 0};
#include "swift/Basic/LanguageModes.def"
  }
}

std::string LanguageMode::versionString() const {
  switch (mode) {
#define MAJOR_LANGUAGE_MODE(NAME, MAJOR)                                       \
  case AnyLanguageMode::NAME:                                                  \
    return #MAJOR;
#define LANGUAGE_MODE(NAME, MAJOR, MINOR)                                      \
  case AnyLanguageMode::NAME:                                                  \
    return #MAJOR "." #MINOR;
#include "swift/Basic/LanguageModes.def"
  case AnyLanguageMode::future:
    return std::to_string(version::Version::getFutureMajorLanguageVersion());
  }
}

void LanguageMode::dump() const { llvm::errs() << versionString(); }
