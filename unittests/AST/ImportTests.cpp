//===--- ImportTests.cpp - Tests for representation of imports ------------===//
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

#include "TestContext.h"
#include "swift/AST/Import.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {
/// Helper class used to create ImportPath and hold all strings for identifiers
class ImportPathContext {
  TestContext Ctx;

public:
  ImportPathContext() = default;

  /// Helper routine for building ImportPath
  /// Build()
  /// @see ImportPathBuilder
  inline ImportPath Build(StringRef Name) noexcept {
    return ImportPath::Builder(Ctx.Ctx, Name, '.').copyTo(Ctx.Ctx);
  }
};

} // namespace

TEST(ImportPath, Comparison) {
  ImportPathContext ctx;

  /// Simple soundness check:
  EXPECT_FALSE(ctx.Build("A.B.C") < ctx.Build("A.B.C"));

  /// Check order chain:
  /// A < A.A < A.A.A < A.A.B < A.B < A.B.A < AA < B < B.A
  EXPECT_LT(ctx.Build("A"), ctx.Build("A.A"));
  EXPECT_LT(ctx.Build("A.A"), ctx.Build("A.A.A"));
  EXPECT_LT(ctx.Build("A.A.A"), ctx.Build("A.A.B"));
  EXPECT_LT(ctx.Build("A.A.B"), ctx.Build("A.B"));
  EXPECT_LT(ctx.Build("A.B"), ctx.Build("A.B.A"));
  EXPECT_LT(ctx.Build("A.B.A"), ctx.Build("AA"));
  EXPECT_LT(ctx.Build("B"), ctx.Build("B.A"));

  /// Further ImportPaths are semantically incorrect, but we must
  /// check that comparing them does not cause compiler to crash.
  EXPECT_LT(ctx.Build("."), ctx.Build("A"));
  EXPECT_LT(ctx.Build("A"), ctx.Build("AA."));
  EXPECT_LT(ctx.Build("A"), ctx.Build("AA.."));
  EXPECT_LT(ctx.Build(".A"), ctx.Build("AA"));
}
