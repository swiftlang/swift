#include "swift/Parse/Token.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/SyntaxFactory.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

// TODO

TEST(RawSyntaxTests, accumulateAbsolutePosition1) {
  auto Token = RawTokenSyntax::make(tok::identifier,
                                    OwnedString("aaa"),
                                    SourcePresence::Present,
                                    Trivia {{
                                      TriviaPiece::newlines(2),
                                      TriviaPiece::carriageReturns(2),
                                      TriviaPiece::carriageReturnLineFeeds(2) }},
                                    Trivia {{  }});
  AbsolutePosition Pos;
  Token->accumulateAbsolutePosition(Pos);
  ASSERT_EQ(7u, Pos.getLine());
  ASSERT_EQ(4u, Pos.getColumn());
  ASSERT_EQ(11u, Pos.getOffset());
}

TEST(RawSyntaxTests, accumulateAbsolutePosition2) {
  auto Token = RawTokenSyntax::make(tok::identifier,
                                    OwnedString("aaa"),
                                    SourcePresence::Present,
                                    Trivia {{
                                      TriviaPiece::blockComment("/* \n\r\r\n */") }},
                                    Trivia {{  }});
  AbsolutePosition Pos;
  Token->accumulateAbsolutePosition(Pos);
  ASSERT_EQ(4u, Pos.getLine());
  ASSERT_EQ(7u, Pos.getColumn());
  ASSERT_EQ(13u, Pos.getOffset());
}
