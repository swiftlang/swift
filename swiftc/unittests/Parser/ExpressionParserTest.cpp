//===--- ExpressionParserTest.cpp - Tests for expression parsing ---------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Parser/Parser.h"
#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/AST/Expr.h"
#include <gtest/gtest.h>
#include <llvm/Support/MemoryBuffer.h>

using namespace swiftc;

class ExpressionParserTest : public ::testing::Test {
protected:
  SourceManager SM;
  DiagnosticEngine Diag;
  
  std::unique_ptr<Expr> parseExpression(const std::string& source) {
    // Wrap expression in a function to parse it
    std::string wrappedSource = "func test() { let _ = " + source + " }";
    
    auto buffer = llvm::MemoryBuffer::getMemBuffer(wrappedSource, "test.swift");
    SourceLoc startLoc = SM.addSourceFile(std::move(buffer), "test.swift");
    
    Lexer lexer(SM, Diag, startLoc);
    Parser parser(lexer, Diag);
    
    auto decls = parser.parseSourceFile();
    if (decls.empty()) return nullptr;
    
    auto* funcDecl = dynamic_cast<FuncDecl*>(decls[0].get());
    if (!funcDecl || !funcDecl->getBody()) return nullptr;
    
    auto* compoundStmt = dynamic_cast<CompoundStmt*>(funcDecl->getBody());
    if (!compoundStmt || compoundStmt->getStatements().empty()) return nullptr;
    
    auto* varDecl = dynamic_cast<VarDecl*>(compoundStmt->getStatements()[0].get());
    if (!varDecl) return nullptr;
    
    return varDecl->getInitializer();
  }
  
  void expectNoDiagnostics() {
    EXPECT_FALSE(Diag.hasErrors()) << "Unexpected parse errors";
  }
  
  void setUp() override {
    Diag.clear();
  }
};

TEST_F(ExpressionParserTest, LiteralExpressions) {
  auto intExpr = parseExpression("42");
  EXPECT_NE(intExpr, nullptr);
  expectNoDiagnostics();
  EXPECT_NE(dynamic_cast<IntegerLiteralExpr*>(intExpr.get()), nullptr);
  
  auto floatExpr = parseExpression("3.14");
  EXPECT_NE(floatExpr, nullptr);
  expectNoDiagnostics();
  EXPECT_NE(dynamic_cast<FloatingPointLiteralExpr*>(floatExpr.get()), nullptr);
  
  auto stringExpr = parseExpression("\"hello\"");
  EXPECT_NE(stringExpr, nullptr);
  expectNoDiagnostics();
  EXPECT_NE(dynamic_cast<StringLiteralExpr*>(stringExpr.get()), nullptr);
  
  auto boolExpr = parseExpression("true");
  EXPECT_NE(boolExpr, nullptr);
  expectNoDiagnostics();
  EXPECT_NE(dynamic_cast<BooleanLiteralExpr*>(boolExpr.get()), nullptr);
}

TEST_F(ExpressionParserTest, IdentifierExpressions) {
  auto identExpr = parseExpression("myVariable");
  EXPECT_NE(identExpr, nullptr);
  expectNoDiagnostics();
  
  auto* declRefExpr = dynamic_cast<DeclRefExpr*>(identExpr.get());
  EXPECT_NE(declRefExpr, nullptr);
  EXPECT_EQ(declRefExpr->getName(), "myVariable");
}

TEST_F(ExpressionParserTest, BinaryExpressions) {
  auto addExpr = parseExpression("1 + 2");
  EXPECT_NE(addExpr, nullptr);
  expectNoDiagnostics();
  
  auto* binaryExpr = dynamic_cast<BinaryExpr*>(addExpr.get());
  EXPECT_NE(binaryExpr, nullptr);
  EXPECT_EQ(binaryExpr->getOperator(), "+");
  
  auto mulExpr = parseExpression("3 * 4");
  EXPECT_NE(mulExpr, nullptr);
  expectNoDiagnostics();
  
  auto comparisonExpr = parseExpression("x == y");
  EXPECT_NE(comparisonExpr, nullptr);
  expectNoDiagnostics();
}

TEST_F(ExpressionParserTest, UnaryExpressions) {
  auto negExpr = parseExpression("-42");
  EXPECT_NE(negExpr, nullptr);
  expectNoDiagnostics();
  
  auto* unaryExpr = dynamic_cast<UnaryExpr*>(negExpr.get());
  EXPECT_NE(unaryExpr, nullptr);
  EXPECT_EQ(unaryExpr->getOperator(), "-");
  
  auto notExpr = parseExpression("!true");
  EXPECT_NE(notExpr, nullptr);
  expectNoDiagnostics();
}

TEST_F(ExpressionParserTest, CallExpressions) {
  auto callExpr = parseExpression("func()");
  EXPECT_NE(callExpr, nullptr);
  expectNoDiagnostics();
  
  auto* call = dynamic_cast<CallExpr*>(callExpr.get());
  EXPECT_NE(call, nullptr);
  EXPECT_EQ(call->getArguments().size(), 0u);
  
  auto callWithArgsExpr = parseExpression("func(a, b, c)");
  EXPECT_NE(callWithArgsExpr, nullptr);
  expectNoDiagnostics();
  
  auto* callWithArgs = dynamic_cast<CallExpr*>(callWithArgsExpr.get());
  EXPECT_NE(callWithArgs, nullptr);
  EXPECT_EQ(callWithArgs->getArguments().size(), 3u);
}

TEST_F(ExpressionParserTest, MemberAccessExpressions) {
  auto memberExpr = parseExpression("object.property");
  EXPECT_NE(memberExpr, nullptr);
  expectNoDiagnostics();
  
  auto* memberRef = dynamic_cast<MemberRefExpr*>(memberExpr.get());
  EXPECT_NE(memberRef, nullptr);
  EXPECT_EQ(memberRef->getMemberName(), "property");
}

TEST_F(ExpressionParserTest, SubscriptExpressions) {
  auto subscriptExpr = parseExpression("array[0]");
  EXPECT_NE(subscriptExpr, nullptr);
  expectNoDiagnostics();
  
  auto* subscript = dynamic_cast<SubscriptExpr*>(subscriptExpr.get());
  EXPECT_NE(subscript, nullptr);
  EXPECT_EQ(subscript->getIndices().size(), 1u);
}

TEST_F(ExpressionParserTest, ParenthesizedExpressions) {
  auto parenExpr = parseExpression("(42)");
  EXPECT_NE(parenExpr, nullptr);
  expectNoDiagnostics();
  
  auto* paren = dynamic_cast<ParenExpr*>(parenExpr.get());
  EXPECT_NE(paren, nullptr);
}

TEST_F(ExpressionParserTest, ArrayLiterals) {
  auto arrayExpr = parseExpression("[1, 2, 3]");
  EXPECT_NE(arrayExpr, nullptr);
  expectNoDiagnostics();
  
  auto* array = dynamic_cast<ArrayExpr*>(arrayExpr.get());
  EXPECT_NE(array, nullptr);
  EXPECT_EQ(array->getElements().size(), 3u);
  
  auto emptyArrayExpr = parseExpression("[]");
  EXPECT_NE(emptyArrayExpr, nullptr);
  expectNoDiagnostics();
}

TEST_F(ExpressionParserTest, DictionaryLiterals) {
  auto dictExpr = parseExpression("[\"key\": \"value\"]");
  EXPECT_NE(dictExpr, nullptr);
  expectNoDiagnostics();
  
  auto* dict = dynamic_cast<DictionaryExpr*>(dictExpr.get());
  EXPECT_NE(dict, nullptr);
  EXPECT_EQ(dict->getElements().size(), 1u);
  
  auto emptyDictExpr = parseExpression("[:]");
  EXPECT_NE(emptyDictExpr, nullptr);
  expectNoDiagnostics();
}

TEST_F(ExpressionParserTest, ComplexExpressions) {
  // Test operator precedence
  auto complexExpr = parseExpression("1 + 2 * 3");
  EXPECT_NE(complexExpr, nullptr);
  expectNoDiagnostics();
  
  // Test chained member access
  auto chainedExpr = parseExpression("object.property.method()");
  EXPECT_NE(chainedExpr, nullptr);
  expectNoDiagnostics();
  
  // Test nested function calls
  auto nestedExpr = parseExpression("outer(inner(value))");
  EXPECT_NE(nestedExpr, nullptr);
  expectNoDiagnostics();
}