//===--- ExprTest.cpp - Tests for expression AST nodes -------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/AST/Expr.h"
#include "swiftc/Basic/SourceLoc.h"
#include <gtest/gtest.h>

using namespace swiftc;

class ExprTest : public ::testing::Test {
protected:
  SourceRange makeRange(uint32_t start, uint32_t end) {
    return SourceRange(SourceLoc(start), SourceLoc(end));
  }
};

TEST_F(ExprTest, IntegerLiteralExpr) {
  SourceRange range = makeRange(100, 103);
  auto expr = std::make_unique<IntegerLiteralExpr>(42, range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::IntegerLiteralExpr);
  EXPECT_EQ(expr->getValue(), 42);
  EXPECT_TRUE(expr->isa<LiteralExpr>());
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, FloatingPointLiteralExpr) {
  SourceRange range = makeRange(200, 204);
  auto expr = std::make_unique<FloatingPointLiteralExpr>(3.14, range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::FloatingPointLiteralExpr);
  EXPECT_DOUBLE_EQ(expr->getValue(), 3.14);
  EXPECT_TRUE(expr->isa<LiteralExpr>());
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, StringLiteralExpr) {
  SourceRange range = makeRange(300, 307);
  auto expr = std::make_unique<StringLiteralExpr>("hello", range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::StringLiteralExpr);
  EXPECT_EQ(expr->getValue(), "hello");
  EXPECT_TRUE(expr->isa<LiteralExpr>());
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, BooleanLiteralExpr) {
  SourceRange range = makeRange(400, 404);
  auto trueExpr = std::make_unique<BooleanLiteralExpr>(true, range);
  auto falseExpr = std::make_unique<BooleanLiteralExpr>(false, range);
  
  EXPECT_EQ(trueExpr->getKind(), NodeKind::BooleanLiteralExpr);
  EXPECT_TRUE(trueExpr->getValue());
  
  EXPECT_EQ(falseExpr->getKind(), NodeKind::BooleanLiteralExpr);
  EXPECT_FALSE(falseExpr->getValue());
  
  EXPECT_TRUE(trueExpr->isa<LiteralExpr>());
  EXPECT_TRUE(falseExpr->isa<LiteralExpr>());
}

TEST_F(ExprTest, DeclRefExpr) {
  SourceRange range = makeRange(500, 508);
  auto expr = std::make_unique<DeclRefExpr>("variable", range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::IdentifierExpr);
  EXPECT_EQ(expr->getName(), "variable");
  EXPECT_TRUE(expr->isa<Expr>());
  EXPECT_FALSE(expr->isa<LiteralExpr>());
}

TEST_F(ExprTest, BinaryExpr) {
  SourceRange range = makeRange(600, 610);
  auto left = std::make_unique<IntegerLiteralExpr>(1, makeRange(600, 601));
  auto right = std::make_unique<IntegerLiteralExpr>(2, makeRange(605, 606));
  
  auto expr = std::make_unique<BinaryExpr>(std::move(left), "+", std::move(right), range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::BinaryOperatorExpr);
  EXPECT_EQ(expr->getOperator(), "+");
  EXPECT_NE(expr->getLeft(), nullptr);
  EXPECT_NE(expr->getRight(), nullptr);
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, UnaryExpr) {
  SourceRange range = makeRange(700, 703);
  auto operand = std::make_unique<IntegerLiteralExpr>(42, makeRange(701, 703));
  
  auto expr = std::make_unique<UnaryExpr>("-", std::move(operand), range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::UnaryOperatorExpr);
  EXPECT_EQ(expr->getOperator(), "-");
  EXPECT_NE(expr->getOperand(), nullptr);
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, CallExpr) {
  SourceRange range = makeRange(800, 810);
  auto base = std::make_unique<DeclRefExpr>("function", makeRange(800, 808));
  
  std::vector<std::unique_ptr<Expr>> args;
  args.push_back(std::make_unique<IntegerLiteralExpr>(1, makeRange(809, 810)));
  
  auto expr = std::make_unique<CallExpr>(std::move(base), std::move(args), range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::CallExpr);
  EXPECT_NE(expr->getCallee(), nullptr);
  EXPECT_EQ(expr->getArguments().size(), 1u);
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, MemberRefExpr) {
  SourceRange range = makeRange(900, 915);
  auto base = std::make_unique<DeclRefExpr>("object", makeRange(900, 906));
  
  auto expr = std::make_unique<MemberRefExpr>(std::move(base), "property", range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::MemberRefExpr);
  EXPECT_EQ(expr->getMemberName(), "property");
  EXPECT_NE(expr->getBase(), nullptr);
  EXPECT_TRUE(expr->isa<Expr>());
}

TEST_F(ExprTest, ArrayExpr) {
  SourceRange range = makeRange(1000, 1010);
  
  std::vector<std::unique_ptr<Expr>> elements;
  elements.push_back(std::make_unique<IntegerLiteralExpr>(1, makeRange(1001, 1002)));
  elements.push_back(std::make_unique<IntegerLiteralExpr>(2, makeRange(1004, 1005)));
  elements.push_back(std::make_unique<IntegerLiteralExpr>(3, makeRange(1007, 1008)));
  
  auto expr = std::make_unique<ArrayExpr>(std::move(elements), range);
  
  EXPECT_EQ(expr->getKind(), NodeKind::ArrayExpr);
  EXPECT_EQ(expr->getElements().size(), 3u);
  EXPECT_TRUE(expr->isa<Expr>());
}