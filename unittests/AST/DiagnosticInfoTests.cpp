//===--- DiagnosticInfoTests.cpp --------------------------------*- C++ -*-===//
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
#include "swift/AST/DiagnosticGroups.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/AST/DiagnosticsModuleDiffer.h"
#include "swift/AST/DiagnosticsSema.h"
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

// MARK: PrintDiagnosticNamesMode

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_None) {
  // Test that we don't embed anything in the format string if told so.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::None);

        TestDiagnostic diagnostic(diag::error_immediate_mode_missing_stdlib.ID,
                                  DiagGroupID::DeprecatedDeclaration);
        diags.diagnose(SourceLoc(), diagnostic);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_immediate_mode_missing_stdlib.ID));
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_Identifier) {
  // Test that we embed correct identifier in the format string.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::Identifier);

        diags.diagnose(SourceLoc(), diag::error_immediate_mode_missing_stdlib);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_TRUE(info.FormatString.ends_with(
            " [error_immediate_mode_missing_stdlib]"));
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_Identifier_WrappedDiag) {
  // For a wrapper diagnostic, test that we embed the identifier of the wrapped
  // diagnostic.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::Identifier);

        diags.diagnose(SourceLoc(), diag::error_immediate_mode_missing_stdlib)
            .limitBehaviorUntilSwiftVersion(DiagnosticBehavior::Warning, 99);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.ID, diag::error_in_a_future_swift_lang_mode.ID);
        EXPECT_TRUE(info.FormatString.ends_with(
            " [error_immediate_mode_missing_stdlib]"));
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_Group_NoGroup) {
  // Test that we don't embed anything in the format string if the diagnostic
  // has no group.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::Group);

        TestDiagnostic diagnostic(diag::error_immediate_mode_missing_stdlib.ID,
                                  DiagGroupID::no_group);
        diags.diagnose(SourceLoc(), diagnostic);
      },
      [](DiagnosticEngine &diags, const DiagnosticInfo &info) {
        EXPECT_EQ(info.FormatString,
                  diags.getFormatStringForDiagnostic(
                      diag::error_immediate_mode_missing_stdlib.ID));
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_Group) {
  // Test that we include the correct group.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::Group);

        TestDiagnostic diagnostic(diag::error_immediate_mode_missing_stdlib.ID,
                                  DiagGroupID::DeprecatedDeclaration);
        diags.diagnose(SourceLoc(), diagnostic);
      },
      [](DiagnosticEngine &, const DiagnosticInfo &info) {
        EXPECT_FALSE(info.FormatString.ends_with(" [DeprecatedDeclaration]"));
        EXPECT_EQ(info.Category, "DeprecatedDeclaration");
      },
      /*expectedNumCallbackCalls=*/1);
}

TEST(DiagnosticInfo, PrintDiagnosticNamesMode_Group_WrappedDiag) {
  // For a wrapper diagnostic, test that we embed the group name of the wrapped
  // diagnostic.
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setPrintDiagnosticNamesMode(PrintDiagnosticNamesMode::Group);

        TestDiagnostic diagnostic(diag::error_immediate_mode_missing_stdlib.ID,
                                  DiagGroupID::DeprecatedDeclaration);
        diags.diagnose(SourceLoc(), diagnostic)
            .limitBehaviorUntilSwiftVersion(DiagnosticBehavior::Warning, 99);
      },
      [](DiagnosticEngine &, const DiagnosticInfo &info) {
        EXPECT_EQ(info.ID, diag::error_in_a_future_swift_lang_mode.ID);
        EXPECT_FALSE(info.FormatString.ends_with(" [DeprecatedDeclaration]"));
        EXPECT_EQ(info.Category, "DeprecatedDeclaration");
      },
      /*expectedNumCallbackCalls=*/1);
}

// Test that the category is appropriately set in these cases, and that the
// category of a wrapped diagnostic is favored.
TEST(DiagnosticInfo, CategoryDeprecation) {
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({5}));

        const auto diag = diag::iuo_deprecated_here;
        EXPECT_TRUE(diags.isDeprecationDiagnostic(diag.ID));

        diags.diagnose(SourceLoc(), diag);
        diags.diagnose(SourceLoc(), diag).warnUntilSwiftVersion(6);
        diags.diagnose(SourceLoc(), diag).warnUntilSwiftVersion(99);
      },
      [](DiagnosticEngine &, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Category, "deprecation");
      },
      /*expectedNumCallbackCalls=*/3);
}
TEST(DiagnosticInfo, CategoryNoUsage) {
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({5}));

        const auto diag = diag::expression_unused_function;
        EXPECT_TRUE(diags.isNoUsageDiagnostic(diag.ID));

        diags.diagnose(SourceLoc(), diag);
        diags.diagnose(SourceLoc(), diag).warnUntilSwiftVersion(6);
        diags.diagnose(SourceLoc(), diag).warnUntilSwiftVersion(99);
      },
      [](DiagnosticEngine &, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Category, "no-usage");
      },
      /*expectedNumCallbackCalls=*/3);
}
TEST(DiagnosticInfo, CategoryAPIDigesterBreakage) {
  testCase(
      [](DiagnosticEngine &diags) {
        diags.setLanguageVersion(version::Version({5}));

        const auto diag = diag::enum_case_added;
        EXPECT_TRUE(diags.isAPIDigesterBreakageDiagnostic(diag.ID));

        diags.diagnose(SourceLoc(), diag, StringRef());
        diags.diagnose(SourceLoc(), diag, StringRef()).warnUntilSwiftVersion(6);
        diags.diagnose(SourceLoc(), diag, StringRef())
            .warnUntilSwiftVersion(99);
      },
      [](DiagnosticEngine &, const DiagnosticInfo &info) {
        EXPECT_EQ(info.Category, "api-digester-breaking-change");
      },
      /*expectedNumCallbackCalls=*/3);
}

} // end anonymous namespace
