//===--- TokenizerTest.cpp - Tests for Token operations ------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Lexer/Token.h"
#include <gtest/gtest.h>

using namespace swiftc;

TEST(TokenTest, BasicTokenCreation) {
  SourceLoc loc(100);
  Token token(TokenKind::Identifier, loc, "myVariable");
  
  EXPECT_EQ(token.getKind(), TokenKind::Identifier);
  EXPECT_EQ(token.getLoc().getRawValue(), 100u);
  EXPECT_EQ(token.getText(), "myVariable");
  
  EXPECT_TRUE(token.is(TokenKind::Identifier));
  EXPECT_FALSE(token.is(TokenKind::Func));
  EXPECT_FALSE(token.isNot(TokenKind::Identifier));
  EXPECT_TRUE(token.isNot(TokenKind::Func));
}

TEST(TokenTest, KeywordIdentification) {
  Token letToken(TokenKind::Let, SourceLoc(1), "let");
  Token varToken(TokenKind::Var, SourceLoc(2), "var");
  Token funcToken(TokenKind::Func, SourceLoc(3), "func");
  Token identifierToken(TokenKind::Identifier, SourceLoc(4), "myVar");
  
  EXPECT_TRUE(letToken.isKeyword());
  EXPECT_TRUE(varToken.isKeyword());
  EXPECT_TRUE(funcToken.isKeyword());
  EXPECT_FALSE(identifierToken.isKeyword());
}

TEST(TokenTest, OperatorIdentification) {
  Token plusToken(TokenKind::Plus, SourceLoc(1), "+");
  Token equalToken(TokenKind::Equal, SourceLoc(2), "=");
  Token arrowToken(TokenKind::Arrow, SourceLoc(3), "->");
  Token identifierToken(TokenKind::Identifier, SourceLoc(4), "variable");
  
  EXPECT_TRUE(plusToken.isOperator());
  EXPECT_TRUE(equalToken.isOperator());
  EXPECT_TRUE(arrowToken.isOperator());
  EXPECT_FALSE(identifierToken.isOperator());
}

TEST(TokenTest, LiteralIdentification) {
  Token intToken(TokenKind::IntegerLiteral, SourceLoc(1), "42");
  Token floatToken(TokenKind::FloatingPointLiteral, SourceLoc(2), "3.14");
  Token stringToken(TokenKind::StringLiteral, SourceLoc(3), "\"hello\"");
  Token identifierToken(TokenKind::Identifier, SourceLoc(4), "variable");
  
  EXPECT_TRUE(intToken.isLiteral());
  EXPECT_TRUE(floatToken.isLiteral());
  EXPECT_TRUE(stringToken.isLiteral());
  EXPECT_FALSE(identifierToken.isLiteral());
}

TEST(TokenTest, TokenRange) {
  SourceLoc loc(100);
  Token token(TokenKind::Identifier, loc, "hello");
  
  SourceRange range = token.getRange();
  EXPECT_EQ(range.Start.getRawValue(), 100u);
  EXPECT_EQ(range.End.getRawValue(), 105u); // 100 + 5 characters
}

TEST(TokenTest, DefaultToken) {
  Token token;
  
  EXPECT_EQ(token.getKind(), TokenKind::Unknown);
  EXPECT_TRUE(token.getLoc().isInvalid());
  EXPECT_EQ(token.getText(), "");
}

TEST(TokenKindTest, GetTokenKindName) {
  EXPECT_EQ(getTokenKindName(TokenKind::Let), "let");
  EXPECT_EQ(getTokenKindName(TokenKind::Var), "var");
  EXPECT_EQ(getTokenKindName(TokenKind::Func), "func");
  EXPECT_EQ(getTokenKindName(TokenKind::Plus), "+");
  EXPECT_EQ(getTokenKindName(TokenKind::Equal), "=");
  EXPECT_EQ(getTokenKindName(TokenKind::LeftParen), "(");
  EXPECT_EQ(getTokenKindName(TokenKind::RightParen), ")");
}

TEST(TokenKindTest, KeywordRecognition) {
  EXPECT_EQ(getKeywordKind("let"), TokenKind::Let);
  EXPECT_EQ(getKeywordKind("var"), TokenKind::Var);
  EXPECT_EQ(getKeywordKind("func"), TokenKind::Func);
  EXPECT_EQ(getKeywordKind("class"), TokenKind::Class);
  EXPECT_EQ(getKeywordKind("struct"), TokenKind::Struct);
  EXPECT_EQ(getKeywordKind("enum"), TokenKind::Enum);
  EXPECT_EQ(getKeywordKind("protocol"), TokenKind::Protocol);
  EXPECT_EQ(getKeywordKind("if"), TokenKind::If);
  EXPECT_EQ(getKeywordKind("else"), TokenKind::Else);
  EXPECT_EQ(getKeywordKind("return"), TokenKind::Return);
  EXPECT_EQ(getKeywordKind("true"), TokenKind::True);
  EXPECT_EQ(getKeywordKind("false"), TokenKind::False);
  EXPECT_EQ(getKeywordKind("nil"), TokenKind::Nil);
  
  // Non-keywords should return Unknown
  EXPECT_EQ(getKeywordKind("notAKeyword"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("variable"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind(""), TokenKind::Unknown);
}