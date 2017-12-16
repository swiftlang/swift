#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;
using namespace llvm;

class LexerTriviaTest : public ::testing::Test {};

TEST_F(LexerTriviaTest, RestoreWithTrivia) {
  using namespace swift::syntax;
  StringRef SourceStr = "aaa \n bbb /*C*/ccc";

  LangOptions LangOpts;
  SourceManager SourceMgr;
  unsigned BufferID = SourceMgr.addMemBufferCopy(SourceStr);

  Lexer L(LangOpts, SourceMgr, BufferID, /*Diags=*/nullptr, /*InSILMode=*/false,
          CommentRetentionMode::AttachToNextToken,
          TriviaRetentionMode::WithTrivia);

  Token Tok;
  Trivia LeadingTrivia, TrailingTrivia;

  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("aaa", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());
  ASSERT_EQ(LeadingTrivia, Trivia());
  ASSERT_EQ(TrailingTrivia, (Trivia{{TriviaPiece::spaces(1)}}));

  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());
  ASSERT_EQ(LeadingTrivia,
            (Trivia{{TriviaPiece::newlines(1), TriviaPiece::spaces(1)}}));
  ASSERT_EQ(TrailingTrivia, (Trivia{{TriviaPiece::spaces(1)}}));

  LexerState S = L.getStateForBeginningOfToken(Tok, LeadingTrivia);

  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("ccc", Tok.getText());
  ASSERT_FALSE(Tok.isAtStartOfLine());
  ASSERT_EQ(LeadingTrivia, (Trivia{{TriviaPiece::blockComment("/*C*/")}}));
  ASSERT_EQ(TrailingTrivia, Trivia());

  L.restoreState(S);
  L.lex(Tok, LeadingTrivia, TrailingTrivia);
  ASSERT_EQ(tok::identifier, Tok.getKind());
  ASSERT_EQ("bbb", Tok.getText());
  ASSERT_TRUE(Tok.isAtStartOfLine());
  ASSERT_EQ(LeadingTrivia,
            (Trivia{{TriviaPiece::newlines(1), TriviaPiece::spaces(1)}}));
  ASSERT_EQ(TrailingTrivia, (Trivia{{TriviaPiece::spaces(1)}}));
}
