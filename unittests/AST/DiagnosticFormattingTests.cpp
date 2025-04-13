//===--- DiagnosticFormattingTests.cpp ------------------------------------===//
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

#include "swift/AST/Attr.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/Support/raw_ostream.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

static void testCase(StringRef formatStr, StringRef resultStr,
                     ArrayRef<DiagnosticArgument> args) {
  SourceManager sourceMgr;
  DiagnosticEngine diags(sourceMgr);

  std::string actualResultStr;
  llvm::raw_string_ostream os(actualResultStr);

  diags.formatDiagnosticText(os, formatStr, args);

  ASSERT_EQ(actualResultStr, resultStr.str());
}

TEST(DiagnosticFormatting, TypeAttribute) {
  EscapingTypeAttr attr{SourceLoc(), SourceLoc()};
  testCase("%0", "'@escaping'", {&attr});
  testCase("%kind0", "attribute 'escaping'", {&attr});
}

} // end anonymous namespace
