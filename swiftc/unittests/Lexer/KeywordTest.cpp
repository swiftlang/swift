//===--- KeywordTest.cpp - Tests for keyword recognition -----------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Lexer/Token.h"
#include <gtest/gtest.h>

using namespace swiftc;

TEST(KeywordTest, DeclarationKeywords) {
  EXPECT_EQ(getKeywordKind("let"), TokenKind::Let);
  EXPECT_EQ(getKeywordKind("var"), TokenKind::Var);
  EXPECT_EQ(getKeywordKind("func"), TokenKind::Func);
  EXPECT_EQ(getKeywordKind("class"), TokenKind::Class);
  EXPECT_EQ(getKeywordKind("struct"), TokenKind::Struct);
  EXPECT_EQ(getKeywordKind("enum"), TokenKind::Enum);
  EXPECT_EQ(getKeywordKind("protocol"), TokenKind::Protocol);
  EXPECT_EQ(getKeywordKind("extension"), TokenKind::Extension);
  EXPECT_EQ(getKeywordKind("typealias"), TokenKind::Typealias);
}

TEST(KeywordTest, ControlFlowKeywords) {
  EXPECT_EQ(getKeywordKind("if"), TokenKind::If);
  EXPECT_EQ(getKeywordKind("else"), TokenKind::Else);
  EXPECT_EQ(getKeywordKind("for"), TokenKind::For);
  EXPECT_EQ(getKeywordKind("while"), TokenKind::While);
  EXPECT_EQ(getKeywordKind("repeat"), TokenKind::Repeat);
  EXPECT_EQ(getKeywordKind("switch"), TokenKind::Switch);
  EXPECT_EQ(getKeywordKind("case"), TokenKind::Case);
  EXPECT_EQ(getKeywordKind("default"), TokenKind::Default);
  EXPECT_EQ(getKeywordKind("break"), TokenKind::Break);
  EXPECT_EQ(getKeywordKind("continue"), TokenKind::Continue);
  EXPECT_EQ(getKeywordKind("return"), TokenKind::Return);
  EXPECT_EQ(getKeywordKind("guard"), TokenKind::Guard);
  EXPECT_EQ(getKeywordKind("do"), TokenKind::Do);
}

TEST(KeywordTest, AccessControlKeywords) {
  EXPECT_EQ(getKeywordKind("public"), TokenKind::Public);
  EXPECT_EQ(getKeywordKind("private"), TokenKind::Private);
  EXPECT_EQ(getKeywordKind("internal"), TokenKind::Internal);
  EXPECT_EQ(getKeywordKind("fileprivate"), TokenKind::Fileprivate);
  EXPECT_EQ(getKeywordKind("open"), TokenKind::Open);
}

TEST(KeywordTest, ModifierKeywords) {
  EXPECT_EQ(getKeywordKind("static"), TokenKind::Static);
  EXPECT_EQ(getKeywordKind("final"), TokenKind::Final);
  EXPECT_EQ(getKeywordKind("override"), TokenKind::Override);
  EXPECT_EQ(getKeywordKind("mutating"), TokenKind::Mutating);
  EXPECT_EQ(getKeywordKind("nonmutating"), TokenKind::Nonmutating);
  EXPECT_EQ(getKeywordKind("lazy"), TokenKind::Lazy);
  EXPECT_EQ(getKeywordKind("weak"), TokenKind::Weak);
  EXPECT_EQ(getKeywordKind("unowned"), TokenKind::Unowned);
  EXPECT_EQ(getKeywordKind("optional"), TokenKind::Optional);
  EXPECT_EQ(getKeywordKind("required"), TokenKind::Required);
  EXPECT_EQ(getKeywordKind("convenience"), TokenKind::Convenience);
  EXPECT_EQ(getKeywordKind("dynamic"), TokenKind::Dynamic);
}

TEST(KeywordTest, OperatorKeywords) {
  EXPECT_EQ(getKeywordKind("infix"), TokenKind::Infix);
  EXPECT_EQ(getKeywordKind("prefix"), TokenKind::Prefix);
  EXPECT_EQ(getKeywordKind("postfix"), TokenKind::Postfix);
  EXPECT_EQ(getKeywordKind("operator"), TokenKind::Operator);
  EXPECT_EQ(getKeywordKind("precedencegroup"), TokenKind::Precedencegroup);
}

TEST(KeywordTest, TypeKeywords) {
  EXPECT_EQ(getKeywordKind("associatedtype"), TokenKind::Associatedtype);
  EXPECT_EQ(getKeywordKind("Self"), TokenKind::Self_);
  EXPECT_EQ(getKeywordKind("super"), TokenKind::Super);
  EXPECT_EQ(getKeywordKind("as"), TokenKind::As);
  EXPECT_EQ(getKeywordKind("is"), TokenKind::Is);
  EXPECT_EQ(getKeywordKind("some"), TokenKind::Some);
  EXPECT_EQ(getKeywordKind("any"), TokenKind::Any);
}

TEST(KeywordTest, LiteralKeywords) {
  EXPECT_EQ(getKeywordKind("true"), TokenKind::True);
  EXPECT_EQ(getKeywordKind("false"), TokenKind::False);
  EXPECT_EQ(getKeywordKind("nil"), TokenKind::Nil);
}

TEST(KeywordTest, ErrorHandlingKeywords) {
  EXPECT_EQ(getKeywordKind("throw"), TokenKind::Throw);
  EXPECT_EQ(getKeywordKind("try"), TokenKind::Try);
  EXPECT_EQ(getKeywordKind("catch"), TokenKind::Catch);
}

TEST(KeywordTest, SpecialKeywords) {
  EXPECT_EQ(getKeywordKind("import"), TokenKind::Import);
  EXPECT_EQ(getKeywordKind("init"), TokenKind::Init);
  EXPECT_EQ(getKeywordKind("deinit"), TokenKind::Deinit);
  EXPECT_EQ(getKeywordKind("subscript"), TokenKind::Subscript);
  EXPECT_EQ(getKeywordKind("willSet"), TokenKind::Willset);
  EXPECT_EQ(getKeywordKind("didSet"), TokenKind::Didset);
  EXPECT_EQ(getKeywordKind("get"), TokenKind::Get);
  EXPECT_EQ(getKeywordKind("set"), TokenKind::Set);
  EXPECT_EQ(getKeywordKind("where"), TokenKind::Where);
  EXPECT_EQ(getKeywordKind("in"), TokenKind::In);
  EXPECT_EQ(getKeywordKind("inout"), TokenKind::Inout);
}

TEST(KeywordTest, CaseSensitivity) {
  // Keywords are case-sensitive
  EXPECT_EQ(getKeywordKind("Let"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("LET"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("Func"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("FUNC"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("True"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("FALSE"), TokenKind::Unknown);
}

TEST(KeywordTest, NonKeywords) {
  EXPECT_EQ(getKeywordKind("variable"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("myFunction"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("CustomClass"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind(""), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("123"), TokenKind::Unknown);
  EXPECT_EQ(getKeywordKind("_private"), TokenKind::Unknown);
}