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

  auto invalidSource = new (C.Ctx) AssignExpr(dest, SourceLoc(), invalid,
                                              /*implicit*/true);
  EXPECT_EQ(start, invalidSource->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidSource->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidSource->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(3), invalidSource->getEndLoc());
  EXPECT_EQ(SourceRange(start, start.getAdvancedLoc(3)),
            invalidSource->getSourceRange());

  auto invalidDest = new (C.Ctx) AssignExpr(invalid, SourceLoc(), source,
                                            /*implicit*/true);
  EXPECT_EQ(start.getAdvancedLoc(8), invalidDest->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidDest->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidDest->getLoc());
  EXPECT_EQ(start.getAdvancedLoc(11), invalidDest->getEndLoc());
  EXPECT_EQ(SourceRange(start.getAdvancedLoc(8), start.getAdvancedLoc(11)),
            invalidDest->getSourceRange());

  auto invalidAll = new (C.Ctx) AssignExpr(invalid, SourceLoc(), invalid,
                                           /*implicit*/true);
  EXPECT_EQ(SourceLoc(), invalidAll->getStartLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getEqualLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getLoc());
  EXPECT_EQ(SourceLoc(), invalidAll->getEndLoc());
  EXPECT_EQ(SourceRange(), invalidAll->getSourceRange());
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
  SmallVector<Expr *, 2> SubExprs({ one, two });
  SmallVector<Identifier, 2> SubExprNames(2, Identifier());
  auto exampleTuple = TupleExpr::createImplicit(C.Ctx, SubExprs, SubExprNames);
  
  EXPECT_EQ(start, one->getStartLoc());
  EXPECT_EQ(SourceLoc(), exampleTuple->getStartLoc());
  EXPECT_EQ(SourceLoc(), exampleTuple->getEndLoc());
  EXPECT_EQ(SourceRange(), exampleTuple->getSourceRange());
}
