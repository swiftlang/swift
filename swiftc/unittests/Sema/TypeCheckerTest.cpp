//===--- TypeCheckerTest.cpp - Tests for type checking -------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Sema/TypeChecker.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Type.h"
#include "swiftc/Basic/Diagnostic.h"
#include <gtest/gtest.h>

using namespace swiftc;

class TypeCheckerTest : public ::testing::Test {
protected:
  DiagnosticEngine Diag;
  TypeChecker TC;
  
  void SetUp() override {
    TC = TypeChecker(Diag);
  }
  
  void expectNoDiagnostics() {
    EXPECT_FALSE(Diag.hasErrors()) << "Unexpected type checking errors";
  }
  
  void expectErrors() {
    EXPECT_TRUE(Diag.hasErrors()) << "Expected type checking errors but got none";
  }
};

TEST_F(TypeCheckerTest, LiteralTypeInference) {
  SourceRange range(SourceLoc(1), SourceLoc(10));
  
  auto intLiteral = std::make_unique<IntegerLiteralExpr>(42, range);
  auto type = TC.typeCheckExpression(intLiteral.get());
  
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "Int");
  expectNoDiagnostics();
  
  auto floatLiteral = std::make_unique<FloatingPointLiteralExpr>(3.14, range);
  type = TC.typeCheckExpression(floatLiteral.get());
  
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "Double");
  expectNoDiagnostics();
  
  auto stringLiteral = std::make_unique<StringLiteralExpr>("hello", range);
  type = TC.typeCheckExpression(stringLiteral.get());
  
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "String");
  expectNoDiagnostics();
  
  auto boolLiteral = std::make_unique<BooleanLiteralExpr>(true, range);
  type = TC.typeCheckExpression(boolLiteral.get());
  
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "Bool");
  expectNoDiagnostics();
}

TEST_F(TypeCheckerTest, BinaryExpressionTypeChecking) {
  SourceRange range(SourceLoc(1), SourceLoc(10));
  
  auto left = std::make_unique<IntegerLiteralExpr>(1, range);
  auto right = std::make_unique<IntegerLiteralExpr>(2, range);
  auto binaryExpr = std::make_unique<BinaryExpr>(std::move(left), "+", std::move(right), range);
  
  auto type = TC.typeCheckExpression(binaryExpr.get());
  
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "Int");
  expectNoDiagnostics();
}

TEST_F(TypeCheckerTest, TypeMismatchErrors) {
  SourceRange range(SourceLoc(1), SourceLoc(10));
  
  // Try to add Int and String (should fail)
  auto left = std::make_unique<IntegerLiteralExpr>(42, range);
  auto right = std::make_unique<StringLiteralExpr>("hello", range);
  auto binaryExpr = std::make_unique<BinaryExpr>(std::move(left), "+", std::move(right), range);
  
  auto type = TC.typeCheckExpression(binaryExpr.get());
  
  EXPECT_EQ(type, nullptr);
  expectErrors();
}

TEST_F(TypeCheckerTest, FunctionTypeChecking) {
  SourceRange range(SourceLoc(1), SourceLoc(50));
  
  // Create a simple function: func add(a: Int, b: Int) -> Int { return a + b }
  std::vector<std::unique_ptr<ParamDecl>> params;
  params.push_back(std::make_unique<ParamDecl>("a", "Int", range));
  params.push_back(std::make_unique<ParamDecl>("b", "Int", range));
  
  auto returnType = std::make_unique<IdentifierType>("Int", range);
  
  // Create return statement: return a + b
  auto aRef = std::make_unique<DeclRefExpr>("a", range);
  auto bRef = std::make_unique<DeclRefExpr>("b", range);
  auto addExpr = std::make_unique<BinaryExpr>(std::move(aRef), "+", std::move(bRef), range);
  auto returnStmt = std::make_unique<ReturnStmt>(std::move(addExpr), range);
  
  std::vector<std::unique_ptr<Stmt>> stmts;
  stmts.push_back(std::move(returnStmt));
  auto body = std::make_unique<CompoundStmt>(std::move(stmts), range);
  
  auto funcDecl = std::make_unique<FuncDecl>("add", std::move(params), 
                                           std::move(returnType), std::move(body), range);
  
  bool success = TC.typeCheckDeclaration(funcDecl.get());
  
  EXPECT_TRUE(success);
  expectNoDiagnostics();
}

TEST_F(TypeCheckerTest, VariableDeclarationTypeChecking) {
  SourceRange range(SourceLoc(1), SourceLoc(20));
  
  auto initializer = std::make_unique<IntegerLiteralExpr>(42, range);
  auto varDecl = std::make_unique<VarDecl>("x", std::move(initializer), true, range);
  
  bool success = TC.typeCheckDeclaration(varDecl.get());
  
  EXPECT_TRUE(success);
  expectNoDiagnostics();
  
  // Variable should have inferred type Int
  EXPECT_NE(varDecl->getType(), nullptr);
  EXPECT_EQ(varDecl->getType()->getName(), "Int");
}

TEST_F(TypeCheckerTest, CallExpressionTypeChecking) {
  SourceRange range(SourceLoc(1), SourceLoc(30));
  
  // Create a call expression: print("hello")
  auto callee = std::make_unique<DeclRefExpr>("print", range);
  
  std::vector<std::unique_ptr<Expr>> args;
  args.push_back(std::make_unique<StringLiteralExpr>("hello", range));
  
  auto callExpr = std::make_unique<CallExpr>(std::move(callee), std::move(args), range);
  
  auto type = TC.typeCheckExpression(callExpr.get());
  
  // print function returns Void
  EXPECT_NE(type, nullptr);
  EXPECT_EQ(type->getName(), "Void");
  expectNoDiagnostics();
}