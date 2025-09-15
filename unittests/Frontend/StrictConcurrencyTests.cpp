//===-- StrictConcurrencyTests.cpp ------------------------------*- C++ -*-===//
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

#include "FeatureParsingTest.h"

using namespace swift;

namespace {

static const FeatureWrapper strictConcurrencyF(Feature::StrictConcurrency);

using StrictConcurrencyTestCase = ArgParsingTestCase<StrictConcurrency>;

class StrictConcurrencyTest
    : public FeatureParsingTest,
      public ::testing::WithParamInterface<StrictConcurrencyTestCase> {};

TEST_P(StrictConcurrencyTest, ) {
  auto &testCase = GetParam();
  parseArgs(testCase.args);
  ASSERT_EQ(getLangOptions().StrictConcurrencyLevel, testCase.expectedResult);
}

// clang-format off
static const StrictConcurrencyTestCase strictConcurrencyTestCases[] = {
  StrictConcurrencyTestCase(
      {},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-swift-version", strictConcurrencyF.langMode},
      StrictConcurrency::Complete),
  StrictConcurrencyTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name},
      StrictConcurrency::Complete),
  StrictConcurrencyTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + ":migrate"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name},
      StrictConcurrency::Complete),
  StrictConcurrencyTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + ":migrate"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-upcoming-feature", strictConcurrencyF.name},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-experimental-feature", strictConcurrencyF.name},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + "=minimal"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + "=minimal"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-upcoming-feature", strictConcurrencyF.name + "=minimal"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-experimental-feature", strictConcurrencyF.name + "=minimal"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + "=targeted"},
      StrictConcurrency::Targeted),
  StrictConcurrencyTestCase({
        "-enable-upcoming-feature",
        strictConcurrencyF.name + "=targeted:migrate",
      },
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + "=targeted"},
      StrictConcurrency::Targeted),
  StrictConcurrencyTestCase({
        "-enable-experimental-feature",
        strictConcurrencyF.name + "=targeted:migrate",
      },
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-upcoming-feature", strictConcurrencyF.name + "=targeted"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-experimental-feature", strictConcurrencyF.name + "=targeted"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + "=complete"},
      StrictConcurrency::Complete),
  StrictConcurrencyTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + "=complete"},
      StrictConcurrency::Complete),
  StrictConcurrencyTestCase(
      {"-disable-upcoming-feature", strictConcurrencyF.name + "=complete"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase(
      {"-disable-experimental-feature", strictConcurrencyF.name + "=complete"},
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase({
        "-enable-upcoming-feature", strictConcurrencyF.name + "=targeted",
        "-disable-upcoming-feature", strictConcurrencyF.name,
        "-enable-upcoming-feature", strictConcurrencyF.name + "=minimal",
      },
      StrictConcurrency::Minimal),
  StrictConcurrencyTestCase({
        "-enable-upcoming-feature", strictConcurrencyF.name + "=targeted",
        "-enable-upcoming-feature", strictConcurrencyF.name + "=complete",
        "-disable-upcoming-feature", strictConcurrencyF.name,
      },
      StrictConcurrency::Complete), // FIXME?
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(, StrictConcurrencyTest,
                         ::testing::ValuesIn(strictConcurrencyTestCases));

} // end anonymous namespace
