//===-- FeatureParsingTest.h ------------------------------------*- C++ -*-===//
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

#ifndef FEATURE_PARSING_TEST_H
#define FEATURE_PARSING_TEST_H

#include "ArgParsingTest.h"
#include "swift/Basic/Feature.h"

struct FeatureParsingTest : public ArgParsingTest {
  static const std::string defaultLangMode;

  FeatureParsingTest();
};

struct FeatureWrapper final {
  swift::Feature id;
  std::string name;
  std::string langMode;

  FeatureWrapper(swift::Feature id);

  operator swift::Feature() const { return id; }
};

// MARK: - Printers

// Google Test requires custom printers to be declared in the same namespace as
// the type they take.
namespace swift {
void PrintTo(const StrictConcurrency &, std::ostream *);
void PrintTo(const LangOptions::FeatureState &, std::ostream *);
void PrintTo(const LangOptions::FeatureState::Kind &, std::ostream *);
} // end namespace swift

#endif // FEATURE_PARSING_TEST_H
