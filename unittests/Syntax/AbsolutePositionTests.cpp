#include "swift/Syntax/SyntaxFactory.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Syntax/SyntaxBuilders.h"
#include "llvm/ADT/SmallString.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::syntax;

TEST(PositionTests, AbsolutePosition1) {
  Trivia Leading;
  Leading.Pieces = {TriviaPiece::newlines(2), TriviaPiece::carriageReturns(2),
    TriviaPiece::carriageReturnLineFeeds(2)};
  auto Token = SyntaxFactory::makeIdentifier("aaa", Leading, {});
  AbsolutePosition Pos = Token.getAbsolutePosition();
  ASSERT_EQ(7u, Pos.getLine());
  ASSERT_EQ(1u, Pos.getColumn());
  ASSERT_EQ(8u, Pos.getOffset());
  AbsolutePosition EndPos = Token.getAbsoluteEndPositionAfterTrailingTrivia();
  ASSERT_EQ(7u, EndPos.getLine());
  ASSERT_EQ(4u, EndPos.getColumn());
  ASSERT_EQ(11u, EndPos.getOffset());
}

TEST(PositionTests, AbsolutePosition2) {
  Trivia Leading;
  Leading.Pieces = { TriviaPiece::blockComment("/* \n\r\r\n */") };
  auto Token = SyntaxFactory::makeIdentifier("aaa", Leading, {});
  AbsolutePosition Pos = Token.getAbsolutePosition();
  ASSERT_EQ(4u, Pos.getLine());
  ASSERT_EQ(4u, Pos.getColumn());
  ASSERT_EQ(10u, Pos.getOffset());
  AbsolutePosition EndPos = Token.getAbsoluteEndPositionAfterTrailingTrivia();
  ASSERT_EQ(4u, EndPos.getLine());
  ASSERT_EQ(7u, EndPos.getColumn());
  ASSERT_EQ(13u, EndPos.getOffset());
}
