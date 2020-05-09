#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Syntax/Trivia.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/Process.h"
#include "gtest/gtest.h"

#if __has_include(<sys/mman.h>)
# include <sys/mman.h>
# define HAS_MMAP 1
#else
# define HAS_MMAP 0
#endif

using namespace swift;
using namespace llvm;
using syntax::TriviaKind;

// The test fixture.
class LexerTest : public ::testing::Test {
public:
  LangOptions LangOpts;
  SourceManager SourceMgr;

  std::vector<Token> tokenizeAndKeepEOF(unsigned BufferID) {
    Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
            LexerMode::Swift);
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
      Toks = tokenize(LangOpts, SourceMgr, BufID, 0, 0, /*Diags=*/nullptr, KeepComments);
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

TEST_F(LexerTest, ContentStartHashbangSkip) {
  const char *Source = "#!/usr/bin/swift\naaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 17), Tok.getLoc());
}

TEST_F(LexerTest, ContentStartHashbangSkipUTF8BOM) {
  const char *Source = "\xEF\xBB\xBF" "#!/usr/bin/swift\naaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 20), Tok.getLoc());
}

TEST_F(LexerTest, ContentStartOperatorLeftBound) {
  const char *Source = "+a";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::oper_prefix, Tok.getKind());
  ASSERT_EQ("+", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 0), Tok.getLoc());
}

TEST_F(LexerTest, ContentStartOperatorLeftBoundUTF8BOM) {
  const char *Source = "\xEF\xBB\xBF" "+a";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::oper_prefix, Tok.getKind());
  ASSERT_EQ("+", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getLoc());
}

TEST_F(LexerTest, ContentStartConflictMarker) {
  const char *Source =
    "<<<<<<< HEAD\n"
    "xxx\n"
    "=======\n"
    "yyy\n"
    ">>>>>>> 12345670\n"
    "aaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
}

TEST_F(LexerTest, ContentStartConflictMarkerUTF8BOM) {
  const char *Source =
  "\xEF\xBB\xBF"
  "<<<<<<< HEAD\n"
  "xxx\n"
  "=======\n"
  "yyy\n"
  ">>>>>>> 12345670\n"
  "aaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
}

TEST_F(LexerTest, ContentStartTokenIsStartOfLine) {
  const char *Source = "aaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 0), Tok.getLoc());
  ASSERT_TRUE(Tok.isAtStartOfLine());
}

TEST_F(LexerTest, ContentStartTokenIsStartOfLineUTF8BOM) {
  const char *Source = "\xEF\xBB\xBF" "aaa";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);
  
  Token Tok;
  
  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getLoc());
  ASSERT_TRUE(Tok.isAtStartOfLine());
}

TEST_F(LexerTest, BOMNoCommentNoTrivia) {
  const char *Source = "\xEF\xBB\xBF" "// comment\naaa //xx \n/* x */";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::None,
          TriviaRetentionMode::WithoutTrivia);
  
  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::eof, Tok.getKind());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
}

TEST_F(LexerTest, BOMTokenCommentNoTrivia) {
  const char *Source = "\xEF\xBB\xBF" "// comment\naaa //xx \n/* x */";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::ReturnAsTokens,
          TriviaRetentionMode::WithoutTrivia);
  
  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::comment, Tok.getKind());
  ASSERT_EQ("// comment\n", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::comment, Tok.getKind());
  ASSERT_EQ("//xx \n", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 18), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 18), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::comment, Tok.getKind());
  ASSERT_EQ("/* x */", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 24), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 24), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::eof, Tok.getKind());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
}

TEST_F(LexerTest, BOMAttachCommentNoTrivia) {
  const char *Source = "\xEF\xBB\xBF" "// comment\naaa //xx \n/* x */";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::AttachToNextToken,
          TriviaRetentionMode::WithoutTrivia);
  
  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getCommentRange().getStart());
  ASSERT_EQ(10u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::eof, Tok.getKind());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 18), Tok.getCommentRange().getStart());
  ASSERT_EQ(13u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{}}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
}

TEST_F(LexerTest, BOMNoCommentTrivia) {
  const char *Source = "\xEF\xBB\xBF" "// comment\naaa //xx \n/* x */";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::None,
          TriviaRetentionMode::WithTrivia);
  
  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::GarbageText, strlen("\xEF\xBB\xBF")),
    ParsedTriviaPiece(TriviaKind::LineComment, strlen("// comment")),
    ParsedTriviaPiece(TriviaKind::Newline, 1)
  }}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::Space, 1)
  }}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::eof, Tok.getKind());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getCommentRange().getStart());
  ASSERT_EQ(0u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::LineComment, strlen("//xx ")),
    ParsedTriviaPiece(TriviaKind::Newline, 1),
    ParsedTriviaPiece(TriviaKind::BlockComment, strlen("/* x */"))
  }}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
}

TEST_F(LexerTest, BOMAttachCommentTrivia) {
  const char *Source = "\xEF\xBB\xBF" "// comment\naaa //xx \n/* x */";
  
  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source));
  
  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift,
          HashbangMode::Disallowed, CommentRetentionMode::AttachToNextToken,
          TriviaRetentionMode::WithTrivia);
  
  Token Tok;
  ParsedTrivia LeadingTrivia, TrailingTrivia;
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 14), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 3), Tok.getCommentRange().getStart());
  ASSERT_EQ(10u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::GarbageText, strlen("\xEF\xBB\xBF")),
    ParsedTriviaPiece(TriviaKind::LineComment, strlen("// comment")),
    ParsedTriviaPiece(TriviaKind::Newline, 1)
  }}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::Space, 1)
  }}), TrailingTrivia);
  
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::eof, Tok.getKind());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 31), Tok.getLoc());
  ASSERT_EQ(SourceMgr.getLocForOffset(BufferID, 18), Tok.getCommentRange().getStart());
  ASSERT_EQ(13u, Tok.getCommentRange().getByteLength());
  ASSERT_EQ((ParsedTrivia{{
    ParsedTriviaPiece(TriviaKind::LineComment, strlen("//xx ")),
    ParsedTriviaPiece(TriviaKind::Newline, 1),
    ParsedTriviaPiece(TriviaKind::BlockComment, strlen("/* x */"))
  }}), LeadingTrivia);
  ASSERT_EQ((ParsedTrivia{{}}), TrailingTrivia);
}

TEST_F(LexerTest, RestoreBasic) {
  const char *Source = "aaa \t\0 bbb ccc";

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, 14));

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);

  Token Tok;

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());

  LexerState S = L.getStateForBeginningOfToken(Tok);

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

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);

  Token Tok;

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  L.lex(Tok);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());

  LexerState S = L.getStateForBeginningOfToken(Tok);

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

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, LexerMode::Swift);

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

  LexerState S = L.getStateForBeginningOfToken(Tok);

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

TEST_F(LexerTest, getLocForStartOfTokenWithCustomSourceLocation) {
  const char *Source =
      "aaa \n"
      // This next line is exactly 50 bytes to make it easy to compare with the
      // previous test.
      "#sourceLocation(file: \"custom-50.swuft\", line: 9)\n"
      " \tbbb \"hello\" \"-\\(val)-\"";

  unsigned BufferID = SourceMgr.addMemBufferCopy(Source);

  // First is character offset, second is its token offset.
  unsigned Offs[][2] =
    { {1, 0}, {2, 0}, {3, 3}, {4, 4},
      {56, 56}, {59, 57}, {64, 61},
      // interpolated string
      {70, 69}, {73, 73}, {74, 73}, {75, 73}, {76, 76}, {77, 69} };

  for (auto Pair : Offs) {
    ASSERT_EQ(Lexer::getLocForStartOfToken(SourceMgr, BufferID, Pair[0]),
              SourceMgr.getLocForOffset(BufferID, Pair[1]));
  }
}

TEST_F(LexerTest, NestedSubLexers) {
  const char *Source = "aaa0 bbb1 ccc2 ddd3 eee4 fff5 ggg6";

  unsigned BufferID = SourceMgr.addMemBufferCopy(Source);

  Lexer Primary(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr,
                LexerMode::Swift);
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
    std::vector<Token> Toks = tokenize(LangOpts, SourceMgr, BufID, 0, 0, /*Diags=*/nullptr, false);
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

class StringCaptureDiagnosticConsumer : public DiagnosticConsumer {
public:
  virtual void handleDiagnostic(SourceManager &SM,
                                const swift::DiagnosticInfo &Info) override {
    std::string DiagMsg;
    llvm::raw_string_ostream DiagOS(DiagMsg);
    DiagnosticEngine::formatDiagnosticText(DiagOS, Info.FormatString,
                                           Info.FormatArgs);
    auto LC = SM.getLineAndColumn(Info.Loc);
    std::ostringstream StrOS;
    StrOS << LC.first << ", " << LC.second << ": " << DiagOS.str();
    messages.push_back(StrOS.str());
  }

  std::vector<std::string> messages;
};

bool containsPrefix(const std::vector<std::string> &strs,
                    const std::string &prefix) {
  for (auto &str : strs) {
    if (StringRef(str).startswith(StringRef(prefix))) {
      return true;
    }
  }
  return false;
}

TEST_F(LexerTest, DiagnoseEmbeddedNul) {
  const char Source[] = " \0 \0 aaa \0 \0 bbb";
  size_t SourceLen = sizeof(Source) - 1;

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, SourceLen));

  StringCaptureDiagnosticConsumer DiagConsumer;
  DiagnosticEngine Diags(SourceMgr);
  Diags.addConsumer(DiagConsumer);

  Lexer L(LangOpts, SourceMgr, BufferID, &Diags,
          LexerMode::Swift, HashbangMode::Disallowed,
          CommentRetentionMode::None, TriviaRetentionMode::WithTrivia);

  ASSERT_TRUE(containsPrefix(DiagConsumer.messages,
                             "1, 2: nul character embedded in middle of file"));
  ASSERT_TRUE(containsPrefix(DiagConsumer.messages,
                             "1, 4: nul character embedded in middle of file"));
}

TEST_F(LexerTest, DiagnoseEmbeddedNulOffset) {
  const char Source[] = " \0 \0 aaa \0 \0 bbb";
  size_t SourceLen = sizeof(Source) - 1;

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(StringRef(Source, SourceLen));

  StringCaptureDiagnosticConsumer DiagConsumer;
  DiagnosticEngine Diags(SourceMgr);
  Diags.addConsumer(DiagConsumer);

  Lexer L(LangOpts, SourceMgr, BufferID, &Diags,
          LexerMode::Swift, HashbangMode::Disallowed,
          CommentRetentionMode::None, TriviaRetentionMode::WithTrivia,
          /*Offset=*/5, /*EndOffset=*/SourceLen);

  ASSERT_FALSE(containsPrefix(
      DiagConsumer.messages, "1, 2: nul character embedded in middle of file"));
  ASSERT_FALSE(containsPrefix(
      DiagConsumer.messages, "1, 4: nul character embedded in middle of file"));
}

#if HAS_MMAP

// This test requires mmap because llvm::sys::Memory doesn't support protecting
// pages to have no permissions.
TEST_F(LexerTest, EncodedStringSegmentPastTheEnd) {
  Expected<size_t> ExptPageSize = llvm::sys::Process::getPageSize();
  ASSERT_TRUE(bool(ExptPageSize));
  size_t PageSize = *ExptPageSize;

  void *FirstPage = mmap(/*addr*/nullptr, PageSize * 2, PROT_NONE,
                         MAP_PRIVATE | MAP_ANON, /*fd*/-1, /*offset*/0);
  SWIFT_DEFER { (void)munmap(FirstPage, PageSize * 2); };
  ASSERT_NE(FirstPage, MAP_FAILED);
  int ProtectResult = mprotect(FirstPage, PageSize, PROT_READ | PROT_WRITE);
  ASSERT_EQ(ProtectResult, 0);

  auto check = [FirstPage, PageSize](StringRef Input, StringRef Expected) {
    char *StartPtr = static_cast<char *>(FirstPage) + PageSize - Input.size();
    memcpy(StartPtr, Input.data(), Input.size());

    SmallString<64> Buffer;
    StringRef Escaped = Lexer::getEncodedStringSegment({StartPtr, Input.size()},
                                                       Buffer);
    EXPECT_EQ(Escaped, Expected);
  };

  check("needs escaping\\r",
        "needs escaping\r");
  check("does not need escaping",
        "does not need escaping");
  check("invalid escape at the end \\",
        "invalid escape at the end ");
}

#endif // HAS_MMAP
