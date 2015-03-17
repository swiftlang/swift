#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

// The test fixture.
class LexerTest : public ::testing::Test {
public:
  LangOptions LangOpts;
  SourceManager SourceMgr;

  std::vector<Token> tokenizeAndKeepEOF(unsigned BufferID) {
    Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
            /*InSILMode=*/false);
    std::vector<Token> Tokens;
    do {
      Tokens.emplace_back();
      L.lex(Tokens.back());
    } while (Tokens.back().isNot(tok::eof));
    return Tokens;
  }

  std::vector<Token> checkLex(StringRef Source,
                              ArrayRef<tok> ExpectedTokens,
                              bool KeepComments = false,
                              bool KeepEOF = false) {
    unsigned BufID = SourceMgr.addMemBufferCopy(Source);

    std::vector<Token> Toks;
    if (KeepEOF)
      Toks = tokenizeAndKeepEOF(BufID);
    else
      Toks = tokenize(LangOpts, SourceMgr, BufID, 0, 0, KeepComments);
    EXPECT_EQ(ExpectedTokens.size(), Toks.size());
    for (unsigned i = 0, e = ExpectedTokens.size(); i != e; ++i) {
      EXPECT_EQ(ExpectedTokens[i], Toks[i].getKind()) << "i = " << i;
    }

    return Toks;
  }

  SourceLoc getLocForEndOfToken(SourceLoc Loc) {
    return Lexer::getLocForEndOfToken(SourceMgr, Loc);
  }
};

TEST_F(LexerTest, TokenizeSkipComments) {
  const char *Source =
      "// Blah\n"
      "(/*yo*/)";
  std::vector<tok> ExpectedTokens{ tok::l_paren, tok::r_paren };
  checkLex(Source, ExpectedTokens, /*KeepComments=*/false);
}

TEST_F(LexerTest, TokenizeWithComments) {
  const char *Source =
      "// Blah\n"
      "(/*yo*/)";
  std::vector<tok> ExpectedTokens{
    tok::comment, tok::l_paren, tok::comment, tok::r_paren
  };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens,
                                     /*KeepComments=*/true);
  EXPECT_EQ(Toks[0].getLength(), 8U);
  EXPECT_EQ(Toks[2].getLength(), 6U);
  EXPECT_EQ(getLocForEndOfToken(Toks[0].getLoc()),
            Toks[0].getLoc().getAdvancedLoc(8));
}

TEST_F(LexerTest, EOFTokenLengthIsZero) {
  const char *Source = "meow";
  std::vector<tok> ExpectedTokens{ tok::identifier, tok::eof };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens,
                                     /*KeepComments=*/true,
                                     /*KeepEOF=*/true);
  EXPECT_EQ(Toks[1].getLength(), 0U);
}

TEST_F(LexerTest, BrokenStringLiteral1) {
  StringRef Source("\"meow\0", 6);
  std::vector<tok> ExpectedTokens{ tok::unknown, tok::eof };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens,
                                     /*KeepComments=*/true,
                                     /*KeepEOF=*/true);
  EXPECT_EQ(Toks[0].getLength(), 6U);
  EXPECT_EQ(Toks[1].getLength(), 0U);
}

TEST_F(LexerTest, BrokenStringLiteral2) {
  StringRef Source("\"\\(meow\0", 8);
  std::vector<tok> ExpectedTokens{ tok::unknown, tok::eof };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens,
                                     /*KeepComments=*/true,
                                     /*KeepEOF=*/true);
  EXPECT_EQ(Toks[0].getLength(), 8U);
  EXPECT_EQ(Toks[1].getLength(), 0U);
}

TEST_F(LexerTest, StringLiteralWithNUL1) {
  StringRef Source("\"\0\"", 3);
  std::vector<tok> ExpectedTokens{ tok::string_literal, tok::eof };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens,
                                     /*KeepComments=*/true,
                                     /*KeepEOF=*/true);
  EXPECT_EQ(Toks[0].getLength(), 3U);
  EXPECT_EQ(Toks[1].getLength(), 0U);
}

TEST_F(LexerTest, CharacterLiterals) {
  const char *Source =
      R"('' '\n' '\t' '\v' '\b' '\r' '\f' '\a' '\\' '\?' '\'' '\"')";
  std::vector<uint32_t> ExpectedCodePoints{
      0xFFFD, '\n', '\t', 0xFFFD, 0xFFFD, '\r', 0xFFFD, 0xFFFD, '\\', 0xFFFD,
      '\'', '\"' };

  LangOptions LangOpts;
  LangOpts.EnableCharacterLiterals = true;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(Source);

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);

  std::vector<Token> Tokens;
  do {
    Tokens.emplace_back();
    L.lex(Tokens.back());
  } while (Tokens.back().isNot(tok::eof));
  Tokens.pop_back(); // Remove tok::eof.

  ASSERT_EQ(ExpectedCodePoints.size(), Tokens.size());
  for (size_t i = 0, e = Tokens.size(); i != e; ++i) {
    auto Tok = Tokens[i];
    ASSERT_EQ(tok::character_literal, Tok.getKind());
    EXPECT_EQ(ExpectedCodePoints[i], L.getEncodedCharacterLiteral(Tok));
  }
}

TEST_F(LexerTest, RestoreBasic) {
  const char *Source = "aaa \t\0 bbb ccc";

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, 14));

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);

  Token Tok;

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  Lexer::State S = L.getStateForBeginningOfToken(Tok);

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());

  L.restoreState(S);

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());
}

TEST_F(LexerTest, RestoreNewlineFlag) {
  const char *Source = "aaa \n \0\tbbb \nccc";

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, 16));

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);

  Token Tok;

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  Lexer::State S = L.getStateForBeginningOfToken(Tok);

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());

  L.restoreState(S);

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());
}

TEST_F(LexerTest, RestoreStopAtCodeCompletion) {
  const char *Source = "aaa \n \0\tbbb \nccc";

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, 16));
  SourceMgr.setCodeCompletionPoint(BufferID, 6);

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);

  Token Tok;

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::code_complete, Tok.getKind());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  Lexer::State S = L.getStateForBeginningOfToken(Tok);

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());

  L.restoreState(S);

  // Ensure that we don't get tok::code_complete here.  We saved the lexer
  // position after it, so we should not be getting it.

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::eof, Tok.getKind());
}

TEST_F(LexerTest, getLocForStartOfToken) {
  const char *Source = "aaa \n \tbbb \"hello\" \"-\\(val)-\"";

  unsigned BufferID = SourceMgr.addMemBufferCopy(Source);

  // First is character offset, second is its token offset.
  unsigned Offs[][2] =
    { {1, 0}, {2, 0}, {3, 3}, {4, 4}, {6, 6}, {9, 7}, {14, 11},
      // interpolated string
      {20, 19}, {23, 23}, {24, 23}, {25, 23}, {26, 26}, {27, 19} };

  for (auto Pair : Offs) {
    ASSERT_EQ(Lexer::getLocForStartOfToken(SourceMgr, BufferID, Pair[0]),
              SourceMgr.getLocForOffset(BufferID, Pair[1]));
  }
}

TEST_F(LexerTest, NestedSubLexers) {
  const char *Source = "aaa0 bbb1 ccc2 ddd3 eee4 fff5 ggg6";

  unsigned BufferID = SourceMgr.addMemBufferCopy(Source);

  Lexer Primary(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
                /*InSILMode=*/false);
  std::vector<Token> TokensPrimary;
  do {
    TokensPrimary.emplace_back();
    Primary.lex(TokensPrimary.back());
  } while (TokensPrimary.back().isNot(tok::eof));
  ASSERT_EQ(8U, TokensPrimary.size());
  ASSERT_EQ(tok::eof, TokensPrimary.back().getKind());

  Lexer Sub1(Primary, Primary.getStateForBeginningOfToken(TokensPrimary[1]),
             Primary.getStateForBeginningOfToken(*(TokensPrimary.end() - 2)));
  std::vector<Token> TokensSub1;
  do {
    TokensSub1.emplace_back();
    Sub1.lex(TokensSub1.back());
  } while (TokensSub1.back().isNot(tok::eof));
  ASSERT_EQ(6U, TokensSub1.size());
  ASSERT_EQ("bbb1", TokensSub1.front().getText());
  ASSERT_EQ("fff5", (TokensSub1.end() - 2)->getText());
  ASSERT_EQ(tok::eof, TokensSub1.back().getKind());
  ASSERT_EQ("ggg6", TokensSub1.back().getText());

  Lexer Sub2(Sub1, Sub1.getStateForBeginningOfToken(TokensSub1[1]),
             Sub1.getStateForBeginningOfToken(*(TokensSub1.end() - 2)));
  std::vector<Token> TokensSub2;
  do {
    TokensSub2.emplace_back();
    Sub2.lex(TokensSub2.back());
  } while (TokensSub2.back().isNot(tok::eof));
  ASSERT_EQ(4U, TokensSub2.size());
  ASSERT_EQ("ccc2", TokensSub2.front().getText());
  ASSERT_EQ("eee4", (TokensSub2.end() - 2)->getText());
  ASSERT_EQ(tok::eof, TokensSub2.back().getKind());
  ASSERT_EQ("fff5", TokensSub2.back().getText());
}

TEST_F(LexerTest, TokenizePlaceholder) {
  const char *Source = "aa <#one#> bb <# two #>";
  std::vector<tok> ExpectedTokens{
    tok::identifier, tok::identifier, tok::identifier, tok::identifier
  };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens);
  EXPECT_EQ("aa", Toks[0].getText());
  EXPECT_EQ("<#one#>", Toks[1].getText());
  EXPECT_EQ("bb", Toks[2].getText());
  EXPECT_EQ("<# two #>", Toks[3].getText());
}

TEST_F(LexerTest, NoPlaceholder) {
  auto checkTok = [&](StringRef Source) {
    unsigned BufID = SourceMgr.addMemBufferCopy(Source);
    std::vector<Token> Toks = tokenize(LangOpts, SourceMgr, BufID, 0, 0, false);
    ASSERT_FALSE(Toks.empty());
    EXPECT_NE(tok::identifier, Toks[0].getKind());
  };
  checkTok("<#");
  checkTok("<#a#");
  checkTok("<#a\n#>");
  checkTok("< #a#>");
}

TEST_F(LexerTest, NestedPlaceholder) {
  const char *Source = "<#<#aa#>#>";
  std::vector<tok> ExpectedTokens{
    tok::oper_prefix, tok::pound, tok::identifier, tok::pound, tok::oper_postfix
  };
  std::vector<Token> Toks = checkLex(Source, ExpectedTokens);
  EXPECT_EQ("<#aa#>", Toks[2].getText());
}
