//===--- OperatorTest.cpp - Tests for operator tokenization --------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Lexer/Token.h"
#include <gtest/gtest.h>

using namespace swiftc;

TEST(OperatorTest, ArithmeticOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::Plus), "+");
  EXPECT_EQ(getTokenKindName(TokenKind::Minus), "-");
  EXPECT_EQ(getTokenKindName(TokenKind::Star), "*");
  EXPECT_EQ(getTokenKindName(TokenKind::Slash), "/");
  EXPECT_EQ(getTokenKindName(TokenKind::Percent), "%");
}

TEST(OperatorTest, AssignmentOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::Equal), "=");
  EXPECT_EQ(getTokenKindName(TokenKind::PlusEqual), "+=");
  EXPECT_EQ(getTokenKindName(TokenKind::MinusEqual), "-=");
  EXPECT_EQ(getTokenKindName(TokenKind::StarEqual), "*=");
  EXPECT_EQ(getTokenKindName(TokenKind::SlashEqual), "/=");
  EXPECT_EQ(getTokenKindName(TokenKind::PercentEqual), "%=");
}

TEST(OperatorTest, ComparisonOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::EqualEqual), "==");
  EXPECT_EQ(getTokenKindName(TokenKind::ExclaimEqual), "!=");
  EXPECT_EQ(getTokenKindName(TokenKind::Less), "<");
  EXPECT_EQ(getTokenKindName(TokenKind::Greater), ">");
  EXPECT_EQ(getTokenKindName(TokenKind::LessEqual), "<=");
  EXPECT_EQ(getTokenKindName(TokenKind::GreaterEqual), ">=");
}

TEST(OperatorTest, LogicalOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::AmpAmp), "&&");
  EXPECT_EQ(getTokenKindName(TokenKind::PipePipe), "||");
  EXPECT_EQ(getTokenKindName(TokenKind::Exclaim), "!");
}

TEST(OperatorTest, BitwiseOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::Amp), "&");
  EXPECT_EQ(getTokenKindName(TokenKind::Pipe), "|");
  EXPECT_EQ(getTokenKindName(TokenKind::Caret), "^");
  EXPECT_EQ(getTokenKindName(TokenKind::Tilde), "~");
  EXPECT_EQ(getTokenKindName(TokenKind::LessLess), "<<");
  EXPECT_EQ(getTokenKindName(TokenKind::GreaterGreater), ">>");
}

TEST(OperatorTest, SpecialOperators) {
  EXPECT_EQ(getTokenKindName(TokenKind::Question), "?");
  EXPECT_EQ(getTokenKindName(TokenKind::QuestionQuestion), "??");
  EXPECT_EQ(getTokenKindName(TokenKind::Arrow), "->");
  EXPECT_EQ(getTokenKindName(TokenKind::FatArrow), "=>");
  EXPECT_EQ(getTokenKindName(TokenKind::At), "@");
  EXPECT_EQ(getTokenKindName(TokenKind::Hash), "#");
  EXPECT_EQ(getTokenKindName(TokenKind::Dollar), "$");
  EXPECT_EQ(getTokenKindName(TokenKind::Backtick), "`");
  EXPECT_EQ(getTokenKindName(TokenKind::Backslash), "\\");
}

TEST(OperatorTest, OperatorClassification) {
  Token plusToken(TokenKind::Plus, SourceLoc(1), "+");
  Token equalToken(TokenKind::Equal, SourceLoc(2), "=");
  Token arrowToken(TokenKind::Arrow, SourceLoc(3), "->");
  Token ampampToken(TokenKind::AmpAmp, SourceLoc(4), "&&");
  
  EXPECT_TRUE(plusToken.isOperator());
  EXPECT_TRUE(equalToken.isOperator());
  EXPECT_TRUE(arrowToken.isOperator());
  EXPECT_TRUE(ampampToken.isOperator());
  
  Token identifierToken(TokenKind::Identifier, SourceLoc(5), "variable");
  Token keywordToken(TokenKind::Let, SourceLoc(6), "let");
  
  EXPECT_FALSE(identifierToken.isOperator());
  EXPECT_FALSE(keywordToken.isOperator());
}