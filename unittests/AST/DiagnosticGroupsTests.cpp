//===--- DiagnosticGroupsTests.cpp ----------------------------------------===//
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
//
// NB: The correctness of the diagnostic group graph is verified in lib/AST
// ('namespace validation').
//
//===----------------------------------------------------------------------===//

#include "swift/AST/DiagnosticGroups.h"
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

struct TestDiagnostic : public Diagnostic {
  TestDiagnostic(DiagID ID, DiagGroupID GroupID) : Diagnostic(ID, GroupID) {}
};

static void testCase(llvm::function_ref<void(DiagnosticEngine &)> diagnose,
                     llvm::function_ref<void(const DiagnosticInfo &)> callback,
                     unsigned expectedNumCallbackCalls) {
  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);

  unsigned count = 0;

  const auto countingCallback = [&](const DiagnosticInfo &info) {
    ++count;
    callback(info);
  };

  TestDiagnosticConsumer consumer(countingCallback);
  diags.addConsumer(consumer);
  diagnose(diags);
  diags.removeConsumer(consumer);

  EXPECT_EQ(count, expectedNumCallbackCalls);
}

TEST(DiagnosticGroups, TargetAll) {
  // Test that uncategorized diagnostics are escalated when escalating all
  // warnings.
  testCase(
      [](DiagnosticEngine &diags) {
        const std::vector rules = {
            WarningAsErrorRule(WarningAsErrorRule::Action::Enable)};
        diags.setWarningsAsErrorsRules(rules);

        TestDiagnostic diagnostic(
            diag::warn_unsupported_module_interface_library_evolution.ID,
            DiagGroupID::no_group);
        diags.diagnose(SourceLoc(), diagnostic);
      },
      [](const DiagnosticInfo &info) {
        EXPECT_EQ(info.Kind, DiagnosticKind::Error);
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticGroups, OverrideBehaviorLimitations) {
  // Test that escalating warnings to errors for *errors* in a diagnostic group
  // overrides emission site behavior limitations.
  {
    TestDiagnostic diagnostic(diag::error_immediate_mode_missing_stdlib.ID,
                              DiagGroupID::DeprecatedDeclaration);

    // Make sure ID actually is an error by default.
    testCase(
        [&diagnostic](DiagnosticEngine &diags) {
          diags.diagnose(SourceLoc(), diagnostic);
        },
        [](const DiagnosticInfo &info) {
          EXPECT_EQ(info.Kind, DiagnosticKind::Error);
        },
        /*expectedNumCallbackCalls=*/1);

    testCase(
        [&diagnostic](DiagnosticEngine &diags) {
          const std::vector rules = {WarningAsErrorRule(
              WarningAsErrorRule::Action::Enable, "DeprecatedDeclaration")};
          diags.setWarningsAsErrorsRules(rules);

          diags.diagnose(SourceLoc(), diagnostic);
          diags.diagnose(SourceLoc(), diagnostic)
              .limitBehaviorUntilSwiftVersion(DiagnosticBehavior::Warning, 99);
        },
        [](const DiagnosticInfo &info) {
          EXPECT_EQ(info.Kind, DiagnosticKind::Error);
        },
        /*expectedNumCallbackCalls=*/2);
  }

  // Test that escalating warnings to errors for *warnings* in a diagnostic
  // group overrides emission site behavior limitations.
  {
    TestDiagnostic diagnostic(
        diag::warn_unsupported_module_interface_library_evolution.ID,
        DiagGroupID::DeprecatedDeclaration);

    // Make sure ID actually is a warning by default.
    testCase(
        [&diagnostic](DiagnosticEngine &diags) {
          diags.diagnose(SourceLoc(), diagnostic);
        },
        [](const DiagnosticInfo &info) {
          EXPECT_EQ(info.Kind, DiagnosticKind::Warning);
        },
        /*expectedNumCallbackCalls=*/1);

    testCase(
        [&diagnostic](DiagnosticEngine &diags) {
          const std::vector rules = {WarningAsErrorRule(
              WarningAsErrorRule::Action::Enable, "DeprecatedDeclaration")};
          diags.setWarningsAsErrorsRules(rules);

          diags.diagnose(SourceLoc(), diagnostic)
              .limitBehaviorUntilSwiftVersion(DiagnosticBehavior::Warning, 99);
        },
        [](const DiagnosticInfo &info) {
          EXPECT_EQ(info.Kind, DiagnosticKind::Error);
        },
        /*expectedNumCallbackCalls=*/1);
  }
}

} // end anonymous namespace
