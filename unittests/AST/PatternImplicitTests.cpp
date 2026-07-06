//===--- PatternImplicitTests.cpp - Tests for Pattern implicit flag -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Regression tests for the AST cache pattern implicit-flag fix.
//
// The per-file AST cache deserializes patterns via createImplicit (inherited
// from the swiftmodule format), but patterns parsed from source are NOT
// implicit. The cache deserialization path calls clearImplicit() recursively
// on patterns within non-implicit PatternBindingDecls to restore the correct
// state. Synthesized (implicit) PBDs keep their inner patterns implicit.
//
// These tests verify the Pattern::clearImplicit() primitive that underpins
// that fix: it must clear the implicit flag on the pattern it's called on,
// and the flag must be observable via isImplicit().
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Decl.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace {

TEST(PatternImplicit, ClearImplicitOnNamedPattern) {
  TestContext C;
  auto &ctx = C.Ctx;

  auto *vd = new (ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Let,
                              SourceLoc(), ctx.getIdentifier("x"),
                              /*DC=*/nullptr);
  auto *pattern = NamedPattern::createImplicit(ctx, vd, Type());

  ASSERT_TRUE(pattern->isImplicit());
  pattern->clearImplicit();
  EXPECT_FALSE(pattern->isImplicit());
}

TEST(PatternImplicit, ClearImplicitIsNonRecursive) {
  TestContext C;
  auto &ctx = C.Ctx;

  auto *vd = new (ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Let,
                              SourceLoc(), ctx.getIdentifier("y"),
                              /*DC=*/nullptr);
  auto *sub = NamedPattern::createImplicit(ctx, vd, Type());
  auto *typed = TypedPattern::createImplicit(ctx, sub, Type());

  ASSERT_TRUE(typed->isImplicit());
  ASSERT_TRUE(sub->isImplicit());

  // clearImplicit only clears the pattern it's called on, not sub-patterns.
  // The recursive walker in Deserialization.cpp handles sub-patterns.
  typed->clearImplicit();
  EXPECT_FALSE(typed->isImplicit());
  EXPECT_TRUE(sub->isImplicit())
      << "clearImplicit is non-recursive; sub-patterns retain implicit";
}

TEST(PatternImplicit, SetImplicitAfterClear) {
  TestContext C;
  auto &ctx = C.Ctx;

  auto *vd = new (ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Var,
                              SourceLoc(), ctx.getIdentifier("z"),
                              /*DC=*/nullptr);
  auto *pattern = NamedPattern::createImplicit(ctx, vd, Type());

  pattern->clearImplicit();
  ASSERT_FALSE(pattern->isImplicit());
  pattern->setImplicit();
  EXPECT_TRUE(pattern->isImplicit())
      << "setImplicit must re-arm after clearImplicit";
}

TEST(PatternImplicit, ClearImplicitOnAlreadyNonImplicit) {
  TestContext C;
  auto &ctx = C.Ctx;

  auto *vd = new (ctx) VarDecl(/*IsStatic=*/false, VarDecl::Introducer::Let,
                              SourceLoc(), ctx.getIdentifier("w"),
                              /*DC=*/nullptr);
  // NamedPattern has no public non-implicit factory; construct directly.
  auto *pattern = new (ctx) NamedPattern(vd);

  ASSERT_FALSE(pattern->isImplicit());
  pattern->clearImplicit();
  EXPECT_FALSE(pattern->isImplicit())
      << "clearImplicit on a non-implicit pattern is a no-op";
}

} // end anonymous namespace
