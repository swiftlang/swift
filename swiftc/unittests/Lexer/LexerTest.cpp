//===--- LexerTest.cpp - Tests for Lexer ---------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

#include "swiftc/Lexer/Lexer.h"
#include "swiftc/Lexer/Token.h"
#include "swiftc/Basic/SourceManager.h"
#include "swiftc/Basic/Diagnostic.h"
#include <gtest/gtest.h>
#include <llvm/Support/MemoryBuffer.h>

using namespace swiftc;

class LexerTest : public ::testing::Test {
protected:
  SourceManager SM;
  DiagnosticEngine Diag;
  
  std::vector<Token> tokenize(const std::string& source, const std::string& filename = "test.swift") {
    SourceLoc startLoc(1);
    
    Lexer lexer(source, startLoc, Diag);
    std::vector<Token> tokens;
    
    Token token;
    do {
      token = lexer.lex();
      tokens.push_back(token);
    } while (token.isNot(TokenKind::Eof));
    
    return tokens;
  }
  
  void expectTokens(const std::string& source, const std::vector<TokenKind>& expectedKinds) {
    auto tokens = tokenize(source);
    
    // Remove EOF token for comparison
    if (!tokens.empty() && tokens.back().is(TokenKind::Eof)) {
      tokens.pop_back();
    }
    
    ASSERT_EQ(tokens.size(), expectedKinds.size()) 
        << "Source: " << source;
    
    for (size_t i = 0; i < tokens.size(); ++i) {
      EXPECT_EQ(tokens[i].getKind(), expectedKinds[i])
          << "Token " << i << " in source: " << source
          << " Expected: " << (int)expectedKinds[i]
          << " Got: " << (int)tokens[i].getKind();
    }
  }
};

TEST_F(LexerTest, Keywords) {
  expectTokens("let var func", {TokenKind::Let, TokenKind::Var, TokenKind::Func});
  expectTokens("class struct enum", {TokenKind::Class, TokenKind::Struct, TokenKind::Enum});
  expectTokens("if else for while", {TokenKind::If, TokenKind::Else, TokenKind::For, TokenKind::While});
  expectTokens("return break continue", {TokenKind::Return, TokenKind::Break, TokenKind::Continue});
  expectTokens("public private internal", {TokenKind::Public, TokenKind::Private, TokenKind::Internal});
  expectTokens("true false nil", {TokenKind::True, TokenKind::False, TokenKind::Nil});
}

TEST_F(LexerTest, Identifiers) {
  expectTokens("hello", {TokenKind::Identifier});
  expectTokens("_private", {TokenKind::Identifier});
  expectTokens("MyClass", {TokenKind::Identifier});
  expectTokens("variable123", {TokenKind::Identifier});
  expectTokens("camelCase", {TokenKind::Identifier});
  expectTokens("snake_case", {TokenKind::Identifier});
}

TEST_F(LexerTest, Numbers) {
  expectTokens("42", {TokenKind::IntegerLiteral});
  expectTokens("0", {TokenKind::IntegerLiteral});
  expectTokens("123456789", {TokenKind::IntegerLiteral});
  expectTokens("3.14", {TokenKind::FloatingPointLiteral});
  expectTokens("0.5", {TokenKind::FloatingPointLiteral});
  expectTokens("1.0e10", {TokenKind::FloatingPointLiteral});
  expectTokens("2.5e-3", {TokenKind::FloatingPointLiteral});
}

TEST_F(LexerTest, StringLiterals) {
  expectTokens("\"hello\"", {TokenKind::StringLiteral});
  expectTokens("\"\"", {TokenKind::StringLiteral});
  expectTokens("\"hello world\"", {TokenKind::StringLiteral});
  expectTokens("\"with\\nescapes\"", {TokenKind::StringLiteral});
  expectTokens("\"unicode: 🚀\"", {TokenKind::StringLiteral});
}

TEST_F(LexerTest, Operators) {
  expectTokens("+ - * /", {TokenKind::Plus, TokenKind::Minus, TokenKind::Star, TokenKind::Slash});
  expectTokens("== != < >", {TokenKind::EqualEqual, TokenKind::ExclaimEqual, TokenKind::Less, TokenKind::Greater});
  expectTokens("&& || !", {TokenKind::AmpAmp, TokenKind::PipePipe, TokenKind::Exclaim});
  expectTokens("= += -=", {TokenKind::Equal, TokenKind::PlusEqual, TokenKind::MinusEqual});
  expectTokens("-> =>", {TokenKind::Arrow, TokenKind::FatArrow});
}

TEST_F(LexerTest, Punctuation) {
  expectTokens("( ) { }", {TokenKind::LeftParen, TokenKind::RightParen, TokenKind::LeftBrace, TokenKind::RightBrace});
  expectTokens("[ ] , ;", {TokenKind::LeftBracket, TokenKind::RightBracket, TokenKind::Comma, TokenKind::Semicolon});
  expectTokens(": . ...", {TokenKind::Colon, TokenKind::Dot, TokenKind::DotDotDot});
  expectTokens("..<", {TokenKind::DotDotLess});
}

TEST_F(LexerTest, SimpleProgram) {
  std::string program = R"(
    let x = 42
    func greet(name: String) -> String {
        return "Hello, \(name)!"
    }
  )";
  
  expectTokens(program, {
    TokenKind::Let, TokenKind::Identifier, TokenKind::Equal, TokenKind::IntegerLiteral,
    TokenKind::Func, TokenKind::Identifier, TokenKind::LeftParen, TokenKind::Identifier,
    TokenKind::Colon, TokenKind::Identifier, TokenKind::RightParen, TokenKind::Arrow,
    TokenKind::Identifier, TokenKind::LeftBrace,
    TokenKind::Return, TokenKind::StringLiteral,
    TokenKind::RightBrace
  });
}

TEST_F(LexerTest, WhitespaceHandling) {
  // Test that whitespace is properly skipped
  expectTokens("let    x=42", {TokenKind::Let, TokenKind::Identifier, TokenKind::Equal, TokenKind::IntegerLiteral});
  expectTokens("let\n\tx\n=\n42", {TokenKind::Let, TokenKind::Identifier, TokenKind::Equal, TokenKind::IntegerLiteral});
}

TEST_F(LexerTest, Comments) {
  // Single line comments should be skipped
  expectTokens("let x = 42 // this is a comment", {TokenKind::Let, TokenKind::Identifier, TokenKind::Equal, TokenKind::IntegerLiteral});
  
  // Multi-line comments should be skipped
  expectTokens("let /* comment */ x = 42", {TokenKind::Let, TokenKind::Identifier, TokenKind::Equal, TokenKind::IntegerLiteral});
}

TEST_F(LexerTest, ErrorRecovery) {
  // Test that lexer can handle invalid characters gracefully
  auto tokens = tokenize("let x = @#$");
  
  // Should have at least let, identifier, equal tokens before hitting unknown tokens
  EXPECT_GE(tokens.size(), 4u); // let, x, =, and at least one more token
  EXPECT_EQ(tokens[0].getKind(), TokenKind::Let);
  EXPECT_EQ(tokens[1].getKind(), TokenKind::Identifier);
  EXPECT_EQ(tokens[2].getKind(), TokenKind::Equal);
}

TEST_F(LexerTest, TokenText) {
  auto tokens = tokenize("let identifier = 42");
  
  EXPECT_EQ(tokens[0].getText(), "let");
  EXPECT_EQ(tokens[1].getText(), "identifier");
  EXPECT_EQ(tokens[2].getText(), "=");
  EXPECT_EQ(tokens[3].getText(), "42");
}

TEST_F(LexerTest, TokenLocations) {
  auto tokens = tokenize("let x");
  
  EXPECT_TRUE(tokens[0].getLoc().isValid());
  EXPECT_TRUE(tokens[1].getLoc().isValid());
  
  // Second token should be after first token
  EXPECT_GT(tokens[1].getLoc().getRawValue(), tokens[0].getLoc().getRawValue());
}