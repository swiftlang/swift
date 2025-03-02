//===-- ArgParsingTest.cpp --------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ArgParsingTest.h"

using namespace swift;

ArgParsingTest::ArgParsingTest() : diags(sourceMgr) {}

void ArgParsingTest::parseArgs(const Args &args) {
  std::vector<const char *> adjustedArgs;

  if (this->langMode) {
    adjustedArgs.reserve(args.size() + 2);
    adjustedArgs.push_back("-swift-version");
    adjustedArgs.push_back(this->langMode->data());
  } else {
    adjustedArgs.reserve(args.size());
  }

  for (auto &arg : args) {
    adjustedArgs.push_back(arg.data());
  }

  this->invocation.parseArgs(adjustedArgs, this->diags);
}

LangOptions &ArgParsingTest::getLangOptions() {
  return this->invocation.getLangOptions();
}

void PrintTo(const Args &value, std::ostream *os) {
  *os << '"';

  if (!value.empty()) {
    const auto lastIdx = value.size() - 1;
    for (size_t idx = 0; idx != lastIdx; ++idx) {
      *os << value[idx] << ' ';
    }
    *os << value[lastIdx];
  }

  *os << '"';
}
