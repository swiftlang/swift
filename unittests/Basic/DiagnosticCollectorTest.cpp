//===--- DiagnosticCollectorTest.cpp --------------------------------------===//
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

#define DEBUG_TYPE "diagnostic-collector-test"

#include "swift/Basic/DiagnosticCollector.h"

#include "gtest/gtest.h"

using namespace swift;

struct DiagnosticInfo {
  DiagnosticInfo(bool isOdd) : isOdd(isOdd) {}
  bool isOdd;
};

TEST(DiagnosticCollectorTest, GetEmptyCollector) {
  DiagnosticCollector<uint32_t, DiagnosticInfo> diagnostics;
  // Expect the diagnostics to be empty!
  EXPECT_TRUE(diagnostics.getDiagnostics(32).empty());
  EXPECT_TRUE(diagnostics.empty());
  EXPECT_TRUE(diagnostics.empty(32));
}

TEST(DiagnosticCollectorTest, CollectorInsertion) {
  DiagnosticCollector<uint32_t, DiagnosticInfo> diagnostics;
  // is the anchor odd?
  diagnostics.emplaceDiagnostic(32, false);
  diagnostics.emplaceDiagnostic(33, true);
  diagnostics.emplaceDiagnostic(32, false);

  auto evenDiagnostics = diagnostics.getDiagnostics(32);
  EXPECT_EQ(evenDiagnostics.size(), 2);
  for (const DiagnosticInfo &diagInfo : evenDiagnostics)
    EXPECT_FALSE(diagInfo.isOdd);

  auto oddDiagnostics = diagnostics.getDiagnostics(33);
  ASSERT_EQ(oddDiagnostics.size(), 1);
  EXPECT_EQ(oddDiagnostics[0].isOdd, true);
}

TEST(DiagnosticCollectorTest, AnchorIterator) {
  DiagnosticCollector<uint32_t, DiagnosticInfo> diagnostics;

  diagnostics.addDiagnostic(30, {false});
  diagnostics.addDiagnostic(31, {true});
  diagnostics.addDiagnostic(32, {false});
  diagnostics.addDiagnostic(31, {true});
  diagnostics.addDiagnostic(30, {false});

  // Ensure that we're iterating the anchors in the order they were first
  // inserted

  llvm::SmallVector<uint32_t, 3> expectedAnchorOrder = {30, 31, 32};
  unsigned anchorCount = 0;
  for (auto anchorIt = diagnostics.begin(), begin = anchorIt,
            end = diagnostics.end();
       anchorIt != end; ++anchorIt) {
    size_t diff = anchorIt - begin;
    EXPECT_EQ(expectedAnchorOrder[diff], *anchorIt);
    EXPECT_FALSE(diagnostics.empty(*anchorIt));
    ++anchorCount;
  }
  EXPECT_EQ(anchorCount, expectedAnchorOrder.size());
}
