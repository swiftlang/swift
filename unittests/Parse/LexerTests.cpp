#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

// The test fixture.
class LexerTest : public ::testing::Test {
  SourceManager SourceMgr;

public:

  std::vector<Token> tokenizeAndKeepEOF(unsigned BufferID) {
    Lexer L(SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);
    std::vector<Token> Tokens;
    do {
      Tokens.emplace_back();
      L.lex(Tokens.back());
    } while (Tokens.back().isNot(tok::eof));
    return Tokens;
  }

  std::vector<Token> checkLex(StringRef Source,
                              ArrayRef<tok> ExpectedTokens,
                              bool KeepComments,
                              bool KeepEOF = false) {
    MemoryBuffer *Buf = MemoryBuffer::getMemBuffer(Source);
    unsigned BufID = SourceMgr->AddNewSourceBuffer(Buf, llvm::SMLoc());

    std::vector<Token> Toks;
    if (KeepEOF)
      Toks = tokenizeAndKeepEOF(BufID);
    else
      Toks = tokenize(SourceMgr, BufID, 0, 0, KeepComments);
    EXPECT_EQ(ExpectedTokens.size(), Toks.size());
    for (unsigned i = 0, e = ExpectedTokens.size(); i != e; ++i) {
      EXPECT_EQ(ExpectedTokens[i], Toks[i].getKind());
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

TEST_F(LexerTest, CharacterLiterals) {
  const char *Source =
      R"('' '\n' '\t' '\v' '\b' '\r' '\f' '\a' '\\' '\?' '\'' '\"')";
  std::vector<uint32_t> ExpectedCodePoints{
      0xFFFD, '\n', '\t', 0xFFFD, 0xFFFD, '\r', 0xFFFD, 0xFFFD, '\\', 0xFFFD,
      '\'', '\"' };

  MemoryBuffer *Buf = MemoryBuffer::getMemBuffer(Source);
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr->AddNewSourceBuffer(Buf, llvm::SMLoc());

  Lexer L(SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false);

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

