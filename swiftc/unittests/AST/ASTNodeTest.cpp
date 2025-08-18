//===--- ASTNodeTest.cpp - Tests for AST nodes ---------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/AST/ASTNode.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/AST/Stmt.h"
#include "swiftc/Basic/SourceLoc.h"
#include <gtest/gtest.h>

using namespace swiftc;

class ASTNodeTest : public ::testing::Test {
protected:
  SourceRange makeRange(uint32_t start, uint32_t end) {
    return SourceRange(SourceLoc(start), SourceLoc(end));
  }
};

TEST_F(ASTNodeTest, BasicNodeProperties) {
  SourceRange range = makeRange(100, 110);
  
  // Test with a concrete expression type
  auto intLiteral = std::make_unique<IntegerLiteralExpr>(42, range);
  
  EXPECT_EQ(intLiteral->getKind(), NodeKind::IntegerLiteralExpr);
  EXPECT_EQ(intLiteral->getSourceRange().getStart().getRawValue(), 100u);
  EXPECT_EQ(intLiteral->getSourceRange().getEnd().getRawValue(), 110u);
  EXPECT_EQ(intLiteral->getStartLoc().getRawValue(), 100u);
  EXPECT_EQ(intLiteral->getEndLoc().getRawValue(), 110u);
}

TEST_F(ASTNodeTest, NodeKindClassification) {
  SourceRange range = makeRange(50, 60);
  
  auto intLiteral = std::make_unique<IntegerLiteralExpr>(42, range);
  auto stringLiteral = std::make_unique<StringLiteralExpr>("hello", range);
  auto boolLiteral = std::make_unique<BooleanLiteralExpr>(true, range);
  
  EXPECT_TRUE(intLiteral->isa<Expr>());
  EXPECT_TRUE(intLiteral->isa<IntegerLiteralExpr>());
  EXPECT_FALSE(intLiteral->isa<StringLiteralExpr>());
  
  EXPECT_TRUE(stringLiteral->isa<Expr>());
  EXPECT_TRUE(stringLiteral->isa<StringLiteralExpr>());
  EXPECT_FALSE(stringLiteral->isa<IntegerLiteralExpr>());
  
  EXPECT_TRUE(boolLiteral->isa<Expr>());
  EXPECT_TRUE(boolLiteral->isa<BooleanLiteralExpr>());
  EXPECT_FALSE(boolLiteral->isa<IntegerLiteralExpr>());
}

TEST_F(ASTNodeTest, DynamicCasting) {
  SourceRange range = makeRange(25, 35);
  
  std::unique_ptr<Expr> expr = std::make_unique<IntegerLiteralExpr>(123, range);
  
  // Test successful cast
  auto* intLiteral = expr->dyn_cast<IntegerLiteralExpr>();
  EXPECT_NE(intLiteral, nullptr);
  EXPECT_EQ(intLiteral->getValue(), 123);
  
  // Test failed cast
  auto* stringLiteral = expr->dyn_cast<StringLiteralExpr>();
  EXPECT_EQ(stringLiteral, nullptr);
  
  // Test base class cast
  auto* baseExpr = expr->dyn_cast<Expr>();
  EXPECT_NE(baseExpr, nullptr);
}

TEST_F(ASTNodeTest, ConstDynamicCasting) {
  SourceRange range = makeRange(75, 85);
  
  const std::unique_ptr<Expr> expr = std::make_unique<StringLiteralExpr>("test", range);
  
  // Test successful const cast
  const auto* stringLiteral = expr->dyn_cast<StringLiteralExpr>();
  EXPECT_NE(stringLiteral, nullptr);
  EXPECT_EQ(stringLiteral->getValue(), "test");
  
  // Test failed const cast
  const auto* intLiteral = expr->dyn_cast<IntegerLiteralExpr>();
  EXPECT_EQ(intLiteral, nullptr);
}

TEST_F(ASTNodeTest, NodeHierarchy) {
  SourceRange range = makeRange(200, 210);
  
  // Test expression hierarchy
  auto expr = std::make_unique<IntegerLiteralExpr>(42, range);
  EXPECT_TRUE(expr->isa<ASTNode>());
  EXPECT_TRUE(expr->isa<Expr>());
  EXPECT_TRUE(expr->isa<IntegerLiteralExpr>());
  
  // Test that it's not other types
  EXPECT_FALSE(expr->isa<Stmt>());
  EXPECT_FALSE(expr->isa<Decl>());
  EXPECT_FALSE(expr->isa<Type>());
}

TEST_F(ASTNodeTest, SourceRangeInvariant) {
  SourceRange range = makeRange(300, 250); // Invalid range (end < start)
  
  // The AST node should still be created, but with an invalid range
  auto expr = std::make_unique<IntegerLiteralExpr>(42, range);
  EXPECT_EQ(expr->getStartLoc().getRawValue(), 300u);
  EXPECT_EQ(expr->getEndLoc().getRawValue(), 250u);
  
  // Note: In a real implementation, we might want to validate ranges
  // or normalize them, but for now we just store what's given
}