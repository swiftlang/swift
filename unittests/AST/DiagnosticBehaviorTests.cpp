//===--- DiagnosticBehaviorTests.cpp --------------------------------------===//
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

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/SourceManager.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {
class TestDiagnosticConsumer : public DiagnosticConsumer {
  llvm::function_ref<void(const DiagnosticInfo &)> callback;

public:
  TestDiagnosticConsumer(decltype(callback) callback) : callback(callback) {}

  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    this->callback(Info);
  }
};

static void
testCase(llvm::function_ref<void(DiagnosticEngine &)> diagnose,
         llvm::function_ref<void(DiagnosticEngine &, const DiagnosticInfo &)>
             callback,
         unsigned expectedNumCallbackCalls) {
  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);

  unsigned count = 0;

  const auto countingCallback = [&](const DiagnosticInfo &info) {
    ++count;
    callback(diags, info);
  };

  TestDiagnosticConsumer consumer(countingCallback);
  diags.addConsumer(consumer);
  diagnose(diags);
  diags.removeConsumer(consumer);

  EXPECT_EQ(count, expectedNumCallbackCalls);
}

TEST(DiagnosticBehavior, WarnUntilSwiftLangMode) {
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({5}));
        diags.diagnose(SourceLoc(), diag::error_immediate_mode_missing_stdlib)
            .warnUntilSwiftVersion(4);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Kind, DiagnosticKind::Error);
        EXPECT_EQ(info.FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_immediate_mode_missing_stdlib.ID));
      },
      /*expectedNumCallbackCalls=*/1);

  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({4}));
        diags.diagnose(SourceLoc(), diag::error_immediate_mode_missing_stdlib)
            .warnUntilSwiftVersion(5);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Kind, DiagnosticKind::Warning);
        EXPECT_EQ(info.FormatString, diags.getFormatStringForDiagnostic(
                                         diag::error_in_swift_lang_mode.ID));

        auto wrappedDiagInfo = info.FormatArgs.front().getAsDiagnostic();
        EXPECT_EQ(wrappedDiagInfo->FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_immediate_mode_missing_stdlib.ID));
      },
      /*expectedNumCallbackCalls=*/1);

  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({4}));
        diags.diagnose(SourceLoc(), diag::error_immediate_mode_missing_stdlib)
            .warnUntilSwiftVersion(99);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Kind, DiagnosticKind::Warning);
        EXPECT_EQ(info.FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_in_a_future_swift_lang_mode.ID));

        auto wrappedDiagInfo = info.FormatArgs.front().getAsDiagnostic();
        EXPECT_EQ(wrappedDiagInfo->FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_immediate_mode_missing_stdlib.ID));
      },
      /*expectedNumCallbackCalls=*/1);
}

} // end anonymous namespace
