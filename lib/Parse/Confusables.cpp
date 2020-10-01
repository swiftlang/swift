//===--- Confusables.cpp - Swift Confusable Character Diagnostics ---------===//
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

#include "swift/Parse/Confusables.h"

char swift::confusable::tryConvertConfusableCharacterToASCII(uint32_t codepoint) {
  switch (codepoint) {
#define CONFUSABLE(CONFUSABLE_POINT, CONFUSABLE_NAME, BASE_POINT, BASE_NAME)   \
  case CONFUSABLE_POINT:                                                       \
    return BASE_POINT;
#include "swift/Parse/Confusables.def"
  default: return 0;
  }
}

std::pair<llvm::StringRef, llvm::StringRef>
swift::confusable::getConfusableAndBaseCodepointNames(uint32_t codepoint) {
  switch (codepoint) {
#define CONFUSABLE(CONFUSABLE_POINT, CONFUSABLE_NAME, BASE_POINT, BASE_NAME)   \
  case CONFUSABLE_POINT:                                                       \
    return std::make_pair(CONFUSABLE_NAME, BASE_NAME);
#include "swift/Parse/Confusables.def"
  default:
    return std::make_pair("", "");
  }
}
