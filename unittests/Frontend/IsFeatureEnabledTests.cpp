//===-- IsFeatureEnabledTests.cpp -------------------------------*- C++ -*-===//
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
#include <map>

using namespace swift;

namespace {

static const FeatureWrapper baselineF(Feature::AsyncAwait);
static const FeatureWrapper upcomingF(Feature::DynamicActorIsolation);
static const FeatureWrapper adoptableUpcomingF(Feature::ExistentialAny);
static const FeatureWrapper experimentalF(Feature::StructLetDestructuring);
static const FeatureWrapper strictConcurrencyF(Feature::StrictConcurrency);

using FeatureState = LangOptions::FeatureState::Kind;

using IsFeatureEnabledTestCase =
    ArgParsingTestCase<std::map<Feature, FeatureState>>;

class IsFeatureEnabledTest
    : public FeatureParsingTest,
      public ::testing::WithParamInterface<IsFeatureEnabledTestCase> {};

// Test that the chosen features for testing match our expectations.
TEST_F(IsFeatureEnabledTest, VerifyTestedFeatures) {
  auto feature = baselineF;
  {
    ASSERT_FALSE(Feature::getUpcomingFeature(feature.name));
    ASSERT_FALSE(Feature::getExperimentalFeature(feature.name));
    ASSERT_FALSE(feature.id.isMigratable());
  }

  feature = upcomingF;
  {
    ASSERT_TRUE(Feature::getUpcomingFeature(feature.name));
    ASSERT_FALSE(feature.id.isMigratable());
    ASSERT_LT(defaultLangMode, feature.langMode);
  }

  feature = adoptableUpcomingF;
  {
    ASSERT_TRUE(Feature::getUpcomingFeature(feature.name));
    ASSERT_TRUE(feature.id.isMigratable());
    ASSERT_LT(defaultLangMode, feature.langMode);
  }

  feature = strictConcurrencyF;
  {
    ASSERT_TRUE(Feature::getUpcomingFeature(feature.name));
    ASSERT_FALSE(feature.id.isMigratable());
    ASSERT_LT(defaultLangMode, feature.langMode);
  }

  feature = experimentalF;
  {
    // If these tests start failing because `experimentalF` was promoted, swap
    // it for another experimental feature one that is available in production.
    ASSERT_TRUE(feature.id.isAvailableInProduction());
    ASSERT_TRUE(Feature::getExperimentalFeature(feature.name));
    ASSERT_FALSE(feature.id.isMigratable());
  }
}

TEST_P(IsFeatureEnabledTest, ) {
  auto &testCase = GetParam();
  parseArgs(testCase.args);

  for (auto &pair : testCase.expectedResult) {
    auto feature = pair.first;
    auto actualState = getLangOptions().getFeatureState(feature);
    auto expectedState = pair.second;
    ASSERT_EQ(actualState, expectedState)
        << "Feature: " + feature.getName().str();
  }
}

// MARK: - Default state

// clang-format off
static const IsFeatureEnabledTestCase defaultStateTestCases[] = {
  IsFeatureEnabledTestCase(
      {}, {
        {baselineF, FeatureState::Enabled},
        {upcomingF, FeatureState::Off},
        {adoptableUpcomingF, FeatureState::Off},
        {strictConcurrencyF, FeatureState::Off},
        {experimentalF, FeatureState::Off},
      }),
  IsFeatureEnabledTestCase(
      {"-swift-version", upcomingF.langMode},
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-swift-version", strictConcurrencyF.langMode},
      {{strictConcurrencyF, FeatureState::Enabled}}),
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(DefaultState, IsFeatureEnabledTest,
                         ::testing::ValuesIn(defaultStateTestCases));

// MARK: - Single enable

// clang-format off
static const IsFeatureEnabledTestCase singleEnableTestCases[] = {
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", baselineF.name},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", baselineF.name + ":undef"},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", baselineF.name + ":migrate"},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", baselineF.name},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", baselineF.name + ":undef"},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", baselineF.name + ":migrate"},
      {{baselineF, FeatureState::Enabled}}),

  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", upcomingF.name},
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", upcomingF.name + ":undef"},
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", upcomingF.name + ":migrate"},
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", upcomingF.name},
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", upcomingF.name + ":undef"},
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", upcomingF.name + ":migrate"},
      {{upcomingF, FeatureState::Off}}),

  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", adoptableUpcomingF.name},
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", adoptableUpcomingF.name + ":undef"},
      {{adoptableUpcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate"},
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),
// Swift 7 is asserts-only.
#ifndef NDEBUG
  // Requesting migration mode in target language mode has no effect.
  IsFeatureEnabledTestCase({
        "-swift-version", adoptableUpcomingF.langMode,
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
#endif
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", adoptableUpcomingF.name},
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", adoptableUpcomingF.name + ":undef"},
      {{adoptableUpcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", adoptableUpcomingF.name + ":migrate"},
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),
// Swift 7 is asserts-only.
#ifndef NDEBUG
  // Requesting migration mode in target language mode has no effect.
  IsFeatureEnabledTestCase({
        "-swift-version", adoptableUpcomingF.langMode,
        "-enable-experimental-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
#endif
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name},
      {{strictConcurrencyF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + ":undef"},
      {{strictConcurrencyF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", strictConcurrencyF.name + ":migrate"},
      {{strictConcurrencyF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name},
      {{strictConcurrencyF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + ":undef"},
      {{strictConcurrencyF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", strictConcurrencyF.name + ":migrate"},
      {{strictConcurrencyF, FeatureState::Off}}),

  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", experimentalF.name},
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", experimentalF.name + ":undef"},
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-upcoming-feature", experimentalF.name + ":migrate"},
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", experimentalF.name},
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", experimentalF.name + ":undef"},
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-enable-experimental-feature", experimentalF.name + ":migrate"},
      {{experimentalF, FeatureState::Off}}),
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(SingleEnable, IsFeatureEnabledTest,
                         ::testing::ValuesIn(singleEnableTestCases));

// MARK: - Single disable

// clang-format off
static const IsFeatureEnabledTestCase singleDisableTestCases[] = {
  IsFeatureEnabledTestCase(
      {"-disable-upcoming-feature", baselineF.name},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-disable-experimental-feature", baselineF.name},
      {{baselineF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase(
      {"-disable-upcoming-feature", upcomingF.name},
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-disable-experimental-feature", upcomingF.name},
      {{upcomingF, FeatureState::Off}}),

  // Disabling in target language mode has no effect.
  IsFeatureEnabledTestCase({
        "-swift-version", upcomingF.langMode,
        "-disable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-swift-version", upcomingF.langMode,
        "-disable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),

  IsFeatureEnabledTestCase(
      {"-disable-upcoming-feature", strictConcurrencyF.name},
      {{strictConcurrencyF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-disable-experimental-feature", strictConcurrencyF.name},
      {{strictConcurrencyF, FeatureState::Off}}),

  // Disabling in target language mode has no effect.
  IsFeatureEnabledTestCase({
        "-disable-upcoming-feature", strictConcurrencyF.name,
        "-swift-version", strictConcurrencyF.langMode,
      },
      {{strictConcurrencyF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-experimental-feature", strictConcurrencyF.name,
        "-swift-version", strictConcurrencyF.langMode,
      },
      {{strictConcurrencyF, FeatureState::Enabled}}),

  IsFeatureEnabledTestCase(
      {"-disable-upcoming-feature", experimentalF.name},
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase(
      {"-disable-experimental-feature", experimentalF.name},
      {{experimentalF, FeatureState::Off}}),
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(SingleDisable, IsFeatureEnabledTest,
                         ::testing::ValuesIn(singleDisableTestCases));

// MARK: - Double enable

// clang-format off
static const IsFeatureEnabledTestCase doubleEnableTestCases[] = {

  // MARK: Non-adoptable & upcoming

  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name + ":undef",
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name + ":migrate",
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name + ":undef",
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name + ":migrate",
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name + ":undef",
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name + ":migrate",
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name + ":undef",
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name + ":migrate",
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),

  // MARK: Adoptable & upcoming

  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":undef",
        "-enable-upcoming-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate",
        "-enable-upcoming-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name,
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":undef",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name,
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":undef",
        "-enable-experimental-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate",
        "-enable-experimental-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name,
        "-enable-experimental-feature", adoptableUpcomingF.name + ":undef",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", adoptableUpcomingF.name,
        "-enable-experimental-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name + ":undef",
        "-enable-upcoming-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name + ":migrate",
        "-enable-upcoming-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name,
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":undef",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name,
        "-enable-upcoming-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name + ":undef",
        "-enable-experimental-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name + ":migrate",
        "-enable-experimental-feature", adoptableUpcomingF.name,
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name,
        "-enable-experimental-feature", adoptableUpcomingF.name + ":undef",
      },
      {{adoptableUpcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", adoptableUpcomingF.name,
        "-enable-experimental-feature", adoptableUpcomingF.name + ":migrate",
      },
      {{adoptableUpcomingF, FeatureState::EnabledForMigration}}),

  // MARK: Experimental

  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name + ":undef",
        "-enable-experimental-feature", experimentalF.name,
      },
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name + ":migrate",
        "-enable-experimental-feature", experimentalF.name,
      },
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name,
        "-enable-experimental-feature", experimentalF.name + ":undef",
      },
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name,
        "-enable-experimental-feature", experimentalF.name + ":migrate",
      },
      {{experimentalF, FeatureState::Enabled}}),
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(DoubleEnable, IsFeatureEnabledTest,
                         ::testing::ValuesIn(doubleEnableTestCases));

// MARK: - Enable / disable

// clang-format off
static const IsFeatureEnabledTestCase enableDisableTestCases[] = {
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-experimental-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Off}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name + ":undef",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", upcomingF.name,
        "-disable-experimental-feature", upcomingF.name + ":migrate",
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-experimental-feature", upcomingF.name,
        "-enable-experimental-feature", upcomingF.name,
      },
      {{upcomingF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name,
        "-disable-experimental-feature", experimentalF.name,
      },
      {{experimentalF, FeatureState::Off}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name,
        "-disable-experimental-feature", experimentalF.name + ":undef",
      },
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-enable-experimental-feature", experimentalF.name,
        "-disable-experimental-feature", experimentalF.name + ":migrate",
      },
      {{experimentalF, FeatureState::Enabled}}),
  IsFeatureEnabledTestCase({
        "-disable-experimental-feature", experimentalF.name,
        "-enable-experimental-feature", experimentalF.name,
      },
      {{experimentalF, FeatureState::Enabled}}),
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(EnableDisable, IsFeatureEnabledTest,
                         ::testing::ValuesIn(enableDisableTestCases));

// MARK: - Last option wins

// clang-format off
static const IsFeatureEnabledTestCase lastOptionWinsTestCases[] = {
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", experimentalF.name,
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-experimental-feature", experimentalF.name,
        "-disable-upcoming-feature", upcomingF.name,
      }, {
        {upcomingF, FeatureState::Off},
        {experimentalF, FeatureState::Off}
      }),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", experimentalF.name,
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-upcoming-feature", upcomingF.name,
        "-disable-experimental-feature", experimentalF.name,
        "-disable-upcoming-feature", upcomingF.name,
        "-enable-experimental-feature", experimentalF.name,
        "-enable-upcoming-feature", upcomingF.name,
      }, {
        {upcomingF, FeatureState::Enabled},
        {experimentalF, FeatureState::Enabled}
      }),
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", strictConcurrencyF.name + "=targeted",
        "-disable-upcoming-feature", strictConcurrencyF.name,
        "-enable-upcoming-feature", strictConcurrencyF.name + "=minimal",
      },
      {{strictConcurrencyF, FeatureState::Off}}), // FIXME?
  IsFeatureEnabledTestCase({
        "-enable-upcoming-feature", strictConcurrencyF.name + "=targeted",
        "-enable-upcoming-feature", strictConcurrencyF.name + "=complete",
        "-disable-upcoming-feature", strictConcurrencyF.name,
      },
      {{strictConcurrencyF, FeatureState::Enabled}}), // FIXME?
};
// clang-format on
INSTANTIATE_TEST_SUITE_P(LastOptionWins, IsFeatureEnabledTest,
                         ::testing::ValuesIn(lastOptionWinsTestCases));

} // end anonymous namespace
