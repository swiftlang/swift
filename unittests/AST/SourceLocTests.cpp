//===--- SourceLocTests.cpp - Tests for source locations of AST nodes -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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
  EXPECT_EQ(start, invalidSource->getLoc()); // If the equal loc is invalid, but start is valid, point at the start
  EXPECT_EQ(start.getAdvancedLoc(3), invalidSource->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(3)),
            invalidSource->getSourceRange());

  auto invalidDest = new (C.Ctx) AssignExpr(invalid, SourceLoc(), source,
                                            /*implicit*/false);
  EXPECT_EQ(start.getAdvancedLoc(8), invalidDest->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidDest->getEqualLoc());
  EXPECT_EQ(start.getAdvancedLoc(8), invalidDest->getLoc()); // If the equal loc is invalid, but start is valid, point at the start
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
  
  auto vardecl = new (C.Ctx) VarDecl(/*IsStatic*/false, /*IsLet*/true,
                                     /*IsCaptureList*/false,
                                     start.getAdvancedLoc(7)
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
  // valid range if there exists at least one expr with a valid source range.
  // The tuple's source range will be the upper bound of the inner source
  // ranges.
  // Source ranges also have the property:
  //   Start.isValid() == End.isValid()
  // For example, given the buffer "one", of the form:
  // (tuple_expr
  //   (declref_expr range=[test.swift:1:0 - line:1:2] ...)
  //   (declref_expr range=invalid ...))
  // the range of this TupleExpr is 1:0 - 1:2.
  //       v invalid                v invalid
  //       (     one,         two   )
  //       valid ^    invalid ^
  // COL:  xxxxxx012xxxxxxxxxxxxxxxxx
  // and the SourceRange of 'one' is 1:0 - 1:2.
  
  //                                                01234567
  auto bufferID = C.Ctx.SourceMgr.addMemBufferCopy("one four");
  SourceLoc start = C.Ctx.SourceMgr.getLocForBufferStart(bufferID);
  
  auto one = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("one"),
      DeclRefKind::Ordinary,
      DeclNameLoc(start));
  
  auto two = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("two"),
      DeclRefKind::Ordinary,
      DeclNameLoc());
  
  auto three = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("three"),
      DeclRefKind::Ordinary,
      DeclNameLoc());
  
  auto four = new (C.Ctx) UnresolvedDeclRefExpr(
      C.Ctx.getIdentifier("four"),
      DeclRefKind::Ordinary,
      DeclNameLoc(start.getAdvancedLoc(4)));
  
  EXPECT_EQ(start, one->getStartLoc());
  EXPECT_EQ(SourceLoc(), two->getStartLoc());
  
  // a tuple with only invalid elements
  SmallVector<Expr *, 2> subExprsInvalid({ two, three });
  SmallVector<Identifier, 2> subExprNamesInvalid(2, Identifier());
  auto allInvalid = TupleExpr::createImplicit(C.Ctx, subExprsInvalid, subExprNamesInvalid);
  
  EXPECT_EQ(SourceLoc(), allInvalid->getStartLoc());
  EXPECT_EQ(SourceLoc(), allInvalid->getEndLoc());
  EXPECT_EQ(SourceRange(), allInvalid->getSourceRange());
  
  // the tuple from the example
  SmallVector<Expr *, 2> subExprsRight({ one, two });
  SmallVector<Identifier, 2> subExprNamesRight(2, Identifier());
  auto rightInvalidTuple = TupleExpr::createImplicit(C.Ctx, subExprsRight, subExprNamesRight);
  
  EXPECT_EQ(start, rightInvalidTuple->getStartLoc());
  EXPECT_EQ(start, rightInvalidTuple->getEndLoc());
  EXPECT_EQ(SourceRange(start, start), rightInvalidTuple->getSourceRange());

  SmallVector<Expr *, 2> subExprsLeft({ two, one });
  SmallVector<Identifier, 2> subExprNamesLeft(2, Identifier());
  auto leftInvalidTuple = TupleExpr::createImplicit(C.Ctx, subExprsLeft, subExprNamesLeft);
  
  EXPECT_EQ(start, leftInvalidTuple->getStartLoc());
  EXPECT_EQ(start, leftInvalidTuple->getEndLoc());
  EXPECT_EQ(SourceRange(start, start), leftInvalidTuple->getSourceRange());
  
  // Some TupleExprs are triples. If only the middle expr has a valid SourceLoc
  // then the TupleExpr's SourceLoc should point at that.
  SmallVector<Expr *, 3> subExprsTriple({ two, one, two });
  SmallVector<Identifier, 3> subExprNamesTriple(3, Identifier());
  auto tripleValidMid = TupleExpr::createImplicit(C.Ctx, subExprsTriple, subExprNamesTriple);
  EXPECT_EQ(start, tripleValidMid->getStartLoc());
  EXPECT_EQ(start, tripleValidMid->getEndLoc());
  EXPECT_EQ(SourceRange(start, start), tripleValidMid->getSourceRange());
  
  // Some TupleExprs are quadruples. Quadruples should point at the range from
  // the first to the last valid exprs.
  SmallVector<Expr *, 4> subExprsQuad({ one, two, four, three });
  SmallVector<Identifier, 4> subExprNamesQuad(4, Identifier());
  auto quadValidMids = TupleExpr::createImplicit(C.Ctx, subExprsQuad, subExprNamesQuad);
  EXPECT_EQ(start, quadValidMids->getStartLoc());
  EXPECT_EQ(start.getAdvancedLoc(4), quadValidMids->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(4)), quadValidMids->getSourceRange());
  
}
