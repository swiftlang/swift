//===--- SILDebugInfoExpressionTest.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILDebugInfoExpression.h"
#include "swift/SIL/SILDebugVariable.h"
#include "gtest/gtest.h"

using namespace swift;

namespace {

// Equality and hashing only compare Decl pointers; they never dereference
// them.  Use distinct addresses as stand-ins so no ASTContext is needed.
alignas(Decl) char FieldXStorage, FieldYStorage;
Decl *const FieldX = reinterpret_cast<Decl *>(&FieldXStorage);
Decl *const FieldY = reinterpret_cast<Decl *>(&FieldYStorage);

SILDebugInfoExpression fragment(Decl *field) {
  SILDIExprElement elements[] = {
      SILDIExprElement::createOperator(SILDIExprOperator::Fragment),
      SILDIExprElement::createDecl(field)};
  return SILDebugInfoExpression(elements);
}

SILDebugInfoExpression deref() {
  SILDIExprElement elements[] = {
      SILDIExprElement::createOperator(SILDIExprOperator::Dereference)};
  return SILDebugInfoExpression(elements);
}

} // end anonymous namespace

TEST(SILDebugInfoExpression, ElementEquality) {
  auto fragOp = SILDIExprElement::createOperator(SILDIExprOperator::Fragment);
  auto derefOp =
      SILDIExprElement::createOperator(SILDIExprOperator::Dereference);
  EXPECT_EQ(fragOp, fragOp);
  EXPECT_NE(fragOp, derefOp);
  EXPECT_NE(hash_value(fragOp), hash_value(derefOp));

  auto x = SILDIExprElement::createDecl(FieldX);
  auto y = SILDIExprElement::createDecl(FieldY);
  EXPECT_EQ(x, SILDIExprElement::createDecl(FieldX));
  EXPECT_NE(x, y);

  EXPECT_EQ(SILDIExprElement::createConstInt(1),
            SILDIExprElement::createConstInt(1));
  EXPECT_NE(SILDIExprElement::createConstInt(1),
            SILDIExprElement::createConstInt(2));
  // Same payload bits, different kinds.
  EXPECT_NE(SILDIExprElement::createConstInt(0),
            SILDIExprElement::createDecl(nullptr));
}

// Two non-empty expressions used to compare equal, because the only
// comparison available was via the implicit conversion to bool.
TEST(SILDebugInfoExpression, ExpressionEquality) {
  EXPECT_EQ(fragment(FieldX), fragment(FieldX));
  EXPECT_EQ(hash_value(fragment(FieldX)), hash_value(fragment(FieldX)));
  EXPECT_NE(fragment(FieldX), fragment(FieldY));
  EXPECT_NE(fragment(FieldX), deref());
  EXPECT_NE(fragment(FieldX), SILDebugInfoExpression());
  EXPECT_EQ(SILDebugInfoExpression(), SILDebugInfoExpression());
}

// Different fragments of one variable are different SILDebugVariable keys.
// SimplifyCFG's trampoline debug_value join relies on this.
TEST(SILDebugInfoExpression, DebugVariableFragmentsAreDistinct) {
  SILDebugVariable var("frag", /*Constant=*/true, /*ArgNo=*/0);
  SILDebugVariable varX = var, varY = var;
  varX.DIExpr = fragment(FieldX);
  varY.DIExpr = fragment(FieldY);
  EXPECT_FALSE(varX == varY);
  EXPECT_FALSE(varX == var);
  EXPECT_TRUE(varX.withoutDIExpr() == varY.withoutDIExpr());
}
