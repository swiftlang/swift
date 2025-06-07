//===--- ConstraintGenerationTests.cpp ------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SemaFixture.h"
#include "swift/AST/Expr.h"
#include "llvm/Support/Casting.h"

using namespace swift;
using namespace swift::unittest;
using namespace swift::constraints;

static Expr *applySolution(ConstraintSystem &cs, Expr *expr,
                           Solution &solution) {
  SyntacticElementTarget target(expr, cs.DC, CTP_Unused, Type(),
                                /*isDiscarded=*/false);
  auto result = cs.applySolution(solution, target);
  return result ? result->getAsExpr() : nullptr;
}

static Type getTypeOfCoercedExpr(ExplicitCastExpr *castExpr) {
  return castExpr->getSubExpr()->getType();
}

TEST_F(SemaTest, TestImplicitForceCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42, SourceLoc());

  auto *castExpr = ForcedCheckedCastExpr::createImplicit(Context, literal,
                                                         Context.TheAnyType);

  auto *expr = cs.generateConstraints(castExpr, DC);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(castExpr)->isEqual(Context.TheAnyType));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<ForcedCheckedCastExpr>(resultExpr))
                  ->isEqual(getStdlibType("Int")));
}

TEST_F(SemaTest, TestImplicitCoercionConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42, SourceLoc());

  auto *castExpr = CoerceExpr::createImplicit(Context, literal,
                                              getStdlibType("Double"));

  auto *expr = cs.generateConstraints(castExpr, DC);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Double")));
  ASSERT_TRUE(
      solution.getResolvedType(castExpr)->isEqual(getStdlibType("Double")));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<CoerceExpr>(resultExpr))
                  ->isEqual(getStdlibType("Double")));
}

TEST_F(SemaTest, TestImplicitConditionalCastConstraintGeneration) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  auto *literal = IntegerLiteralExpr::createFromUnsigned(Context, 42, SourceLoc());

  auto *castExpr = ConditionalCheckedCastExpr::createImplicit(
      Context, literal, getStdlibType("Double"));

  auto *expr = cs.generateConstraints(castExpr, DC);

  ASSERT_NE(expr, nullptr);

  SmallVector<Solution, 2> solutions;
  cs.solve(solutions);

  ASSERT_EQ(solutions.size(), (unsigned)1);

  auto &solution = solutions.front();

  ASSERT_TRUE(solution.getResolvedType(literal)->isEqual(getStdlibType("Int")));
  ASSERT_TRUE(solution.getResolvedType(castExpr)->isEqual(
      OptionalType::get(getStdlibType("Double"))));

  auto *resultExpr = applySolution(cs, expr, solution);
  ASSERT_NE(resultExpr, nullptr);
  ASSERT_TRUE(getTypeOfCoercedExpr(cast<ConditionalCheckedCastExpr>(resultExpr))
              ->isEqual(getStdlibType("Int")));
}

TEST_F(SemaTest, TestCaptureListIsNotOpenedEarly) {
  ConstraintSystem cs(DC, ConstraintSystemOptions());

  DeclAttributes attrs;
  auto *closure = new (Context) ClosureExpr(attrs,
                                            /*bracetRange=*/SourceRange(),
                                            /*capturedSelfDecl=*/nullptr,
                                            ParameterList::createEmpty(Context),
                                            /*asyncLoc=*/SourceLoc(),
                                            /*throwsLoc=*/SourceLoc(),
                                            /*thrownType*/nullptr,
                                            /*arrowLoc=*/SourceLoc(),
                                            /*inLoc=*/SourceLoc(),
                                            /*explicitResultType=*/nullptr, DC);
  closure->setImplicit();
  // Create a return statement so this is treated as a single expression.
  auto *RS = ReturnStmt::createImplied(
      Context, TupleExpr::createImplicit(Context, {}, {}));
  closure->setBody(BraceStmt::createImplicit(Context, /*elements=*/{RS}));

  SmallVector<CaptureListEntry> captures;
  {
    // The capture variable.
    auto *xVar = new (Context)
        VarDecl(/*isStatic=*/false, VarDecl::Introducer::Var,
                /*loc=*/SourceLoc(), Context.getIdentifier("x"), DC);
    xVar->setImplicit();

    auto *PBD = PatternBindingDecl::createImplicit(
        Context, StaticSpellingKind::None,
        /*pattern=*/NamedPattern::createImplicit(Context, xVar),
        IntegerLiteralExpr::createFromUnsigned(Context, 42, SourceLoc()), DC);

    captures.push_back(CaptureListEntry(PBD));
  }

  auto *captureList = CaptureListExpr::create(Context, captures, closure);

  // _ = { [x = 42] in }
  Expr *assign = new (Context)
      AssignExpr(new (Context) DiscardAssignmentExpr(/*loc=*/SourceLoc(),
                                                     /*Implicit=*/true),
                 /*EqualLoc=*/SourceLoc(), captureList, /*Implicit=*/true);

  auto *processed = cs.generateConstraints(assign, DC);
  ASSERT_NE(processed, nullptr);

  for (const auto &capture : captureList->getCaptureList()) {
    ASSERT_FALSE(cs.hasType(capture.getVar()));
  }

  auto *closureType = cs.getType(closure)->castTo<TypeVariableType>();

  ASTExtInfo extInfo;
  ASSERT_TRUE(cs.resolveClosure(
      closureType,
      FunctionType::get(/*params*/ {}, Context.TheEmptyTupleType, extInfo),
      cs.getConstraintLocator(closure)));

  for (const auto &capture : captureList->getCaptureList()) {
    ASSERT_TRUE(cs.hasType(capture.getVar()));
  }
}

TEST_F(SemaTest, TestMultiStmtClosureBodyParentAndDepth) {
  // {
  //   ()
  //   return ()
  // }
  DeclAttributes attrs;
  auto *closure = new (Context) ClosureExpr(attrs,
                                            /*braceRange=*/SourceRange(),
                                            /*capturedSelfDecl=*/nullptr,
                                            ParameterList::createEmpty(Context),
                                            /*asyncLoc=*/SourceLoc(),
                                            /*throwsLoc=*/SourceLoc(),
                                            /*thrownType*/ nullptr,
                                            /*arrowLoc=*/SourceLoc(),
                                            /*inLoc=*/SourceLoc(),
                                            /*explicitResultType=*/nullptr, DC);
  closure->setImplicit();

  auto *RS = ReturnStmt::createImplicit(
      Context, TupleExpr::createImplicit(Context, {}, {}));

  closure->setBody(BraceStmt::createImplicit(Context, {
    TupleExpr::createImplicit(Context, {}, {}), RS
  }));

  SyntacticElementTarget target(closure, DC, ContextualTypeInfo(),
                                /*isDiscarded*/ true);

  ConstraintSystem cs(DC, ConstraintSystemOptions());
  cs.solve(target);

  ASSERT_EQ(cs.getParentExpr(closure), nullptr);
  ASSERT_EQ(cs.getExprDepth(closure), 0);

  // We visit the ReturnStmt twice when computing the parent map, ensure we
  // don't invalidate its parent on the second walk during the conjunction.
  auto *result = RS->getResult();
  ASSERT_EQ(cs.getParentExpr(result), closure);
  ASSERT_EQ(cs.getExprDepth(result), 1);
}

TEST_F(SemaTest, TestIfExprLocator) {
  // Test to make sure we get the expected conjunction and locators.

  // if true { 1 } else { 2 }
  auto *ifCond = new (Context) BooleanLiteralExpr(true, SourceLoc());
  auto *thenBrace = BraceStmt::createImplicit(
      Context,
      {IntegerLiteralExpr::createFromUnsigned(Context, 1, SourceLoc())});
  auto *elseBrace = BraceStmt::createImplicit(
      Context,
      {IntegerLiteralExpr::createFromUnsigned(Context, 2, SourceLoc())});
  auto *ifStmt =
      new (Context) IfStmt(SourceLoc(), ifCond, thenBrace, SourceLoc(),
                           elseBrace, /*implicit*/ true, Context);
  auto *ifExpr = SingleValueStmtExpr::createWithWrappedBranches(
      Context, ifStmt, DC, /*mustBeExpr*/ true);

  SyntacticElementTarget target(ifExpr, DC, ContextualTypeInfo(),
                                /*isDiscarded*/ true);

  ConstraintSystem cs(DC, ConstraintSystemOptions());
  auto hadError = cs.generateConstraints(target);
  ASSERT_FALSE(hadError);

  auto *conjunction = &cs.getConstraints().front();
  ASSERT_EQ(conjunction->getKind(), ConstraintKind::Conjunction);

  auto nested = conjunction->getNestedConstraints();
  ASSERT_EQ(nested.size(), 4);

  auto *condConstraint = nested[0];
  auto *thenConstraint = nested[1];
  auto *elseConstraint = nested[2];
  auto *joinConstraint = nested[3];

  auto *ifStmtLoc =
      cs.getConstraintLocator(ifExpr, LocatorPathElt::SyntacticElement(ifStmt));

  auto *condLoc =
      cs.getConstraintLocator(ifStmtLoc, LocatorPathElt::Condition());
  ASSERT_EQ(condConstraint->getLocator(), condLoc);

  auto *thenLoc =
      cs.getConstraintLocator(ifStmtLoc, LocatorPathElt::TernaryBranch(true));
  ASSERT_EQ(thenConstraint->getLocator(), thenLoc);

  auto *elseLoc =
      cs.getConstraintLocator(ifStmtLoc, LocatorPathElt::TernaryBranch(false));
  ASSERT_EQ(elseConstraint->getLocator(), elseLoc);

  auto *joinLoc = cs.getConstraintLocator(
      ifStmtLoc,
      LocatorPathElt::SyntacticElement(joinConstraint->getSyntacticElement()));
  ASSERT_EQ(joinConstraint->getLocator(), joinLoc);
}

TEST_F(SemaTest, TestSwitchExprLocator) {
  // Test to make sure we get the expected conjunction and locators.

  // case true: 1
  auto *trueBrace = BraceStmt::createImplicit(
      Context,
      {IntegerLiteralExpr::createFromUnsigned(Context, 1, SourceLoc())});
  auto *truePattern = ExprPattern::createImplicit(
      Context, new (Context) BooleanLiteralExpr(true, SourceLoc()), DC);
  auto *trueCase =
      CaseStmt::create(Context, CaseParentKind::Switch, SourceLoc(),
                       {CaseLabelItem(truePattern)}, SourceLoc(), SourceLoc(),
                       trueBrace, /*caseBodyVars*/ std::nullopt);

  // case false: 2
  auto *falseBrace = BraceStmt::createImplicit(
      Context,
      {IntegerLiteralExpr::createFromUnsigned(Context, 2, SourceLoc())});
  auto *falsePattern = ExprPattern::createImplicit(
      Context, new (Context) BooleanLiteralExpr(false, SourceLoc()), DC);
  auto *falseCase =
      CaseStmt::create(Context, CaseParentKind::Switch, SourceLoc(),
                       {CaseLabelItem(falsePattern)}, SourceLoc(), SourceLoc(),
                       falseBrace, /*caseBodyVars*/ std::nullopt);

  auto *subject = new (Context) BooleanLiteralExpr(true, SourceLoc());

  // switch true { case true: 1 case false: 2 }
  auto *switchStmt = SwitchStmt::create(LabeledStmtInfo(), SourceLoc(), subject,
                                        SourceLoc(), {trueCase, falseCase},
                                        SourceLoc(), SourceLoc(), Context);
  auto *switchExpr = SingleValueStmtExpr::createWithWrappedBranches(
      Context, switchStmt, DC, /*mustBeExpr*/ true);

  SyntacticElementTarget target(switchExpr, DC, ContextualTypeInfo(),
                                /*isDiscarded*/ true);

  ConstraintSystem cs(DC, ConstraintSystemOptions());
  auto hadError = cs.generateConstraints(target);
  ASSERT_FALSE(hadError);

  auto *conjunction = &cs.getConstraints().front();
  ASSERT_EQ(conjunction->getKind(), ConstraintKind::Conjunction);

  auto nested = conjunction->getNestedConstraints();
  ASSERT_EQ(nested.size(), 4);

  auto *subjectConstraint = nested[0];
  auto *trueConstraint = nested[1];
  auto *falseConstraint = nested[2];
  auto *joinConstraint = nested[3];

  auto *switchStmtLoc = cs.getConstraintLocator(
      switchExpr, LocatorPathElt::SyntacticElement(switchStmt));

  // These currently all get the switch statement locator.
  ASSERT_EQ(subjectConstraint->getLocator(), switchStmtLoc);
  ASSERT_EQ(trueConstraint->getLocator(), switchStmtLoc);
  ASSERT_EQ(falseConstraint->getLocator(), switchStmtLoc);

  auto *joinLoc = cs.getConstraintLocator(
      switchStmtLoc,
      LocatorPathElt::SyntacticElement(joinConstraint->getSyntacticElement()));
  ASSERT_EQ(joinConstraint->getLocator(), joinLoc);
}
