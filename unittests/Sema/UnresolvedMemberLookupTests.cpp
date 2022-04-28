//===--- UnresolvedMemberLookupTests.cpp --------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SemaFixture.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;

TEST_F(SemaTest, TestLookupAlwaysLooksThroughOptionalBase) {
  auto *intTypeDecl = getStdlibNominalTypeDecl("Int");
  auto *optTypeDecl = getStdlibNominalTypeDecl("Optional");
  auto intType = intTypeDecl->getDeclaredType();
  auto intOptType = OptionalType::get(intType);
  auto stringType = getStdlibType("String");

  auto *intMember = addExtensionVarMember(intTypeDecl, "test", intOptType);
  addExtensionVarMember(optTypeDecl, "test", stringType);

  auto *UME = new (Context)
      UnresolvedMemberExpr(SourceLoc(), DeclNameLoc(),
                           DeclNameRef(Context.getIdentifier("test")), true);
  auto *UMCRE = new (Context) UnresolvedMemberChainResultExpr(UME, UME);

  ConstraintSystem cs(DC, ConstraintSystemOptions());
  auto *expr = cs.generateConstraints(UMCRE, DC);
  ASSERT_TRUE(expr);

  cs.addConstraint(
      ConstraintKind::Conversion, cs.getType(expr), intOptType,
      cs.getConstraintLocator(UMCRE, ConstraintLocator::ContextualType));
  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  // We should have a solution.
  ASSERT_EQ(solutions.size(), 1u);

  auto &solution = solutions[0];
  auto *locator = cs.getConstraintLocator(UME,
                                          ConstraintLocator::UnresolvedMember);
  auto choice = solution.getOverloadChoice(locator).choice;

  // The `test` member on `Int` should be selected.
  ASSERT_EQ(choice.getDecl(), intMember);
}

TEST_F(SemaTest, TestLookupPrefersResultsOnOptionalRatherThanBase) {
  auto *intTypeDecl = getStdlibNominalTypeDecl("Int");
  auto *optTypeDecl = getStdlibNominalTypeDecl("Optional");
  auto intType = intTypeDecl->getDeclaredType();
  auto intOptType = OptionalType::get(intType);

  addExtensionVarMember(intTypeDecl, "test", intOptType);
  auto *optMember = addExtensionVarMember(optTypeDecl, "test", intType);

  auto *UME = new (Context)
      UnresolvedMemberExpr(SourceLoc(), DeclNameLoc(),
                           DeclNameRef(Context.getIdentifier("test")), true);
  auto *UMCRE = new (Context) UnresolvedMemberChainResultExpr(UME, UME);

  ConstraintSystem cs(DC, ConstraintSystemOptions());
  auto *expr = cs.generateConstraints(UMCRE, DC);
  ASSERT_TRUE(expr);

  cs.addConstraint(
      ConstraintKind::Conversion, cs.getType(expr), intOptType,
      cs.getConstraintLocator(expr, ConstraintLocator::ContextualType));
  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  // We should have a solution.
  ASSERT_EQ(solutions.size(), 1u);

  auto &solution = solutions[0];
  auto *locator = cs.getConstraintLocator(UME,
                                          ConstraintLocator::UnresolvedMember);
  auto choice = solution.getOverloadChoice(locator).choice;
  auto score = solution.getFixedScore();

  // The `test` member on `Optional` should be chosen over the member on `Int`,
  // even though the score is otherwise worse.
  ASSERT_EQ(score.Data[SK_ValueToOptional], 1u);
  ASSERT_EQ(choice.getDecl(), optMember);
}
