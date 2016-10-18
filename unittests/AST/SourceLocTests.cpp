//===--- SourceLocTests.cpp - Tests for source locations of AST nodes -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "TestContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Stmt.h"
#include "gtest/gtest.h"

using namespace swift;
using namespace swift::unittest;

namespace swift {
  void PrintTo(SourceLoc loc, std::ostream *os) {
    *os << loc.getOpaquePointerValue();
    if (loc.isValid())
      *os << " '" << *(char *)loc.getOpaquePointerValue() << "'";
  }

  void PrintTo(SourceRange range, std::ostream *os) {
    PrintTo(range.Start, os);
    *os << " - ";
    PrintTo(range.End, os);
  }
} // end namespace swift

TEST(SourceLoc, AssignExpr) {
  TestContext C;

  //                                                0123456789012
  auto bufferID = C.Ctx.SourceMgr.addMemBufferCopy("aa.bb = cc.dd");
  SourceLoc start = C.Ctx.SourceMgr.getLocForBufferStart(bufferID);

  auto destBase = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("aa"),
      DeclRefKind::Ordinary,
      DeclNameLoc(start));
  auto dest = new (C.Ctx) UnresolvedDotExpr(
      destBase,
      start.getAdvancedLoc(2),
      C.Ctx.getIdentifier("bb"),
      DeclNameLoc(start.getAdvancedLoc(3)),
      /*implicit*/false);
  auto destImplicit = new (C.Ctx) UnresolvedDotExpr(
      destBase,
      start.getAdvancedLoc(2),
      C.Ctx.getIdentifier("bb"),
      DeclNameLoc(start.getAdvancedLoc(3)),
      /*implicit*/true);

  auto sourceBase = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("cc"),
      DeclRefKind::Ordinary,
      DeclNameLoc(start.getAdvancedLoc(8)));
  auto source = new (C.Ctx) UnresolvedDotExpr(
      sourceBase,
      start.getAdvancedLoc(10),
      C.Ctx.getIdentifier("dd"),
      DeclNameLoc(start.getAdvancedLoc(11)),
      /*implicit*/false);
  auto sourceImplicit = new (C.Ctx) UnresolvedDotExpr(
      sourceBase,
      start.getAdvancedLoc(10),
      C.Ctx.getIdentifier("dd"),
      DeclNameLoc(start.getAdvancedLoc(11)),
      /*implicit*/true);


  auto invalid = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("invalid"),
      DeclRefKind::Ordinary,
      DeclNameLoc());

  auto complete = new (C.Ctx) AssignExpr(dest, start.getAdvancedLoc(6), source,
                                         /*implicit*/false);
  EXPECT_EQ(start, complete->getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), complete->getEqualLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), complete->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(11), complete->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(11)),
            complete->getSourceRange());

  // Implicit dest should not change the source range.
  auto completeImplDest = new (C.Ctx) AssignExpr( destImplicit
                                                , start.getAdvancedLoc(6)
                                                , source, /*implicit*/false);
  EXPECT_EQ(start, completeImplDest->getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), completeImplDest->getEqualLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), completeImplDest->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(11), completeImplDest->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(11)),
            completeImplDest->getSourceRange());

  // Implicit source should not change the source range.
  auto completeImplSrc = new (C.Ctx) AssignExpr( dest, start.getAdvancedLoc(6)
                                               , sourceImplicit
                                               , /*implicit*/false);
  EXPECT_EQ(start, completeImplSrc->getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), completeImplSrc->getEqualLoc());
  EXPECT_EQ(start.getAdvancedLoc(6), completeImplSrc->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(11), completeImplSrc->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(11)),
            completeImplSrc->getSourceRange());

  auto invalidSource = new (C.Ctx) AssignExpr(dest, SourceLoc(), invalid,
                                              /*implicit*/false);
  EXPECT_EQ(start, invalidSource->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidSource->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidSource->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(3), invalidSource->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(3)),
            invalidSource->getSourceRange());

  auto invalidDest = new (C.Ctx) AssignExpr(invalid, SourceLoc(), source,
                                            /*implicit*/false);
  EXPECT_EQ(start.getAdvancedLoc(8), invalidDest->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidDest->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidDest->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(11), invalidDest->getEndLoc());
  EXPECT_EQ(SourceRange(start.getAdvancedLoc(8), start.getAdvancedLoc(11)),
            invalidDest->getSourceRange());

  auto invalidAll = new (C.Ctx) AssignExpr(invalid, SourceLoc(), invalid,
                                           /*implicit*/false);
  EXPECT_EQ(SourceLoc(), invalidAll->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getEndLoc());
  EXPECT_EQ(SourceRange(), invalidAll->getSourceRange());
}

TEST(SourceLoc, StmtConditionElement) {
  TestContext C;
  
  // In a pattern binding statement condition element the SourceRange is only
  // valid iff the Initializer has a valid end loc and either:
  // a. the IntroducerLoc has a valid start loc
  // b. if the IntroducerLoc is invalid, the pattern has a valid start loc
  // If neither of these hold, source range must be invalid.

  auto bufferID = C.Ctx.SourceMgr //       0123456789012345678901234567890
                        .addMemBufferCopy("if let x = Optional.some(1) { }");
  SourceLoc start = C.Ctx.SourceMgr.getLocForBufferStart(bufferID);
  
  auto vardecl = new (C.Ctx) VarDecl( false, true, start.getAdvancedLoc(7)
                                    , C.Ctx.getIdentifier("x")
                                    , Type()
                                    , nullptr);
  auto pattern = new (C.Ctx) NamedPattern(vardecl);
  auto init = new (C.Ctx) IntegerLiteralExpr( "1", start.getAdvancedLoc(25)
                                            , false);
  
  // Case a, when the IntroducerLoc is valid.
  auto introducer = StmtConditionElement( start.getAdvancedLoc(3)
                                        , pattern, init);
  
  EXPECT_EQ(start.getAdvancedLoc(3), introducer.getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(25), introducer.getEndLoc());
  EXPECT_EQ( SourceRange(start.getAdvancedLoc(3), start.getAdvancedLoc(25))
           , introducer.getSourceRange());
  
  // Case b, when the IntroducerLoc is invalid, but the pattern has a valid loc.
  auto patternStmtCond = StmtConditionElement(SourceLoc(), pattern, init);
  
  EXPECT_EQ(start.getAdvancedLoc(7), patternStmtCond.getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(25), patternStmtCond.getEndLoc());
  EXPECT_EQ( SourceRange(start.getAdvancedLoc(7), start.getAdvancedLoc(25))
           , patternStmtCond.getSourceRange());
  
  // If the IntroducerLoc is valid but the stmt cond init is invalid.
  auto invalidInit = new (C.Ctx) IntegerLiteralExpr("1", SourceLoc(), false);
  auto introducerStmtInvalid = StmtConditionElement( start.getAdvancedLoc(3)
                                                   , pattern, invalidInit);
  
  EXPECT_EQ(SourceLoc(), introducerStmtInvalid.getStartLoc());
  EXPECT_EQ(SourceLoc(), introducerStmtInvalid.getEndLoc());
  EXPECT_EQ(SourceRange(), introducerStmtInvalid.getSourceRange());
  
  // If the IntroducerLoc is invalid, the pattern is valid, but the stmt cond 
  // init is invalid.
  auto patternStmtInvalid = StmtConditionElement( SourceLoc(), pattern
                                                , invalidInit);
  
  EXPECT_EQ(SourceLoc(), patternStmtInvalid.getStartLoc());
  EXPECT_EQ(SourceLoc(), patternStmtInvalid.getEndLoc());
  EXPECT_EQ(SourceRange(), patternStmtInvalid.getSourceRange());
}

TEST(SourceLoc, TupleExpr) {
  TestContext C;
  
  // In a TupleExpr, if the parens are both invalid, then you can only have a
  // valid range iff both the first element and last element have valid ranges.
  // Source ranges also have the property:
  //   Start.isValid() == End.isValid()
  // For example, given the buffer "one", of the form:
  // (tuple_expr
  //   (declref_expr range=[test.swift:1:0 - line:1:2] ...)
  //   (declref_expr range=invalid ...))
  // the range of this TupleExpr is SourceLoc() (invalid).
  //       v invalid                v invalid
  //       (     one,         two   )
  //       valid ^    invalid ^
  // COL:  xxxxxx012xxxxxxxxxxxxxxxxx
  // but the SourceRange of 'one' is 1:0 - 1:2.
  
  //                                                012
  auto bufferID = C.Ctx.SourceMgr.addMemBufferCopy("one");
  SourceLoc start = C.Ctx.SourceMgr.getLocForBufferStart(bufferID);
  
  auto one = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("one"),
      DeclRefKind::Ordinary,
      DeclNameLoc(start));
  
  auto two = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("two"),
      DeclRefKind::Ordinary,
      DeclNameLoc());
  
  // the tuple from the example
  SmallVector<Expr *, 2> subExprsRight({ one, two });
  SmallVector<Identifier, 2> subExprNamesRight(2, Identifier());
  auto rightInvalidTuple = TupleExpr::createImplicit(C.Ctx, subExprsRight, subExprNamesRight);
  
  EXPECT_EQ(start, one->getStartLoc());
  EXPECT_EQ(SourceLoc(), rightInvalidTuple->getStartLoc());
  EXPECT_EQ(SourceLoc(), rightInvalidTuple->getEndLoc());
  EXPECT_EQ(SourceRange(), rightInvalidTuple->getSourceRange());

  SmallVector<Expr *, 2> subExprsLeft({ two, one });
  SmallVector<Identifier, 2> subExprNamesLeft(2, Identifier());
  auto leftInvalidTuple = TupleExpr::createImplicit(C.Ctx, subExprsLeft, subExprNamesLeft);
  
  EXPECT_EQ(start, one->getStartLoc());
  EXPECT_EQ(SourceLoc(), leftInvalidTuple->getStartLoc());
  EXPECT_EQ(SourceLoc(), leftInvalidTuple->getEndLoc());
  EXPECT_EQ(SourceRange(), leftInvalidTuple->getSourceRange());
}
