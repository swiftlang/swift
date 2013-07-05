#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace llvm;

// The test fixture.
class LexerTest : public ::testing::Test {
  SourceMgr SourceMgr;

public:
  std::vector<Token> checkLex(StringRef Source,
                              ArrayRef<tok> ExpectedTokens,
                              bool KeepComments) {
    MemoryBuffer *Buf = MemoryBuffer::getMemBuffer(Source);
    unsigned BufID = SourceMgr.AddNewSourceBuffer(Buf, llvm::SMLoc());

    std::vector<Token> Toks = tokenize(SourceMgr, BufID, 0, 0, KeepComments);
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
