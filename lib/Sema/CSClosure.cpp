//===--- CSClosure.cpp - Closures -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements constraint generation and solution application for
// closures. It provides part of the implementation of the ConstraintSystem
// class.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
using namespace swift;
using namespace swift::constraints;

namespace {

// MARK: Constraint generation

/// Statement visitor that generates constraints for a given closure body.
class ClosureConstraintGenerator
    : public StmtVisitor<ClosureConstraintGenerator, void> {
  friend StmtVisitor<ClosureConstraintGenerator, void>;

  ConstraintSystem &cs;
  ClosureExpr *closure;
  Type closureResultType;

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  ClosureConstraintGenerator(ConstraintSystem &cs, ClosureExpr *closure,
                             Type closureResultType)
    : cs(cs), closure(closure), closureResultType(closureResultType) { }

private:
  void visitDecl(Decl *decl) {
    // Just ignore #if; the chosen children should appear in the
    // surrounding context.  This isn't good for source tools but it
    // at least works.
    if (isa<IfConfigDecl>(decl))
      return;

    // Skip #warning/#error; we'll handle them when applying the closure.
    if (isa<PoundDiagnosticDecl>(decl))
      return;

    // Ignore variable declarations, because they're always handled within
    // their enclosing pattern bindings.
    if (isa<VarDecl>(decl))
      return;

    // Generate constraints for pattern binding declarations.
    if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
      SolutionApplicationTarget target(patternBinding);
      if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow))
        hadError = true;

      return;
    }

    llvm_unreachable("Unimplemented case for closure body");
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
    for (auto node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        ASTContext &ctx = cs.getASTContext();
        bool isDiscarded = (!ctx.LangOpts.Playground &&
                            !ctx.LangOpts.DebuggerSupport);
        SolutionApplicationTarget target(
            expr, closure, CTP_Unused, Type(), isDiscarded);
        if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow))
          hadError = true;

        cs.setSolutionApplicationTarget(expr, target);
      } else if (auto stmt = node.dyn_cast<Stmt *>()) {
        visit(stmt);
      } else {
        visitDecl(node.get<Decl *>());
      }
    }
  }

  void visitDoStmt(DoStmt *doStmt) {
    visit(doStmt->getBody());
  }

  void visitForEachStmt(ForEachStmt *forEachStmt) {
    auto sequenceProto = TypeChecker::getProtocol(
        closure->getASTContext(), forEachStmt->getForLoc(),
        KnownProtocolKind::Sequence);
    assert(sequenceProto && "Missing Sequence protocol");

    // Generate constraints for the loop header. This also wires up the
    // types for the patterns.
    auto target = SolutionApplicationTarget::forForEachStmt(
        forEachStmt, sequenceProto, closure, /*bindPatternVarsOneWay=*/true);
    if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow)) {
      hadError = true;
    }

    cs.setSolutionApplicationTarget(forEachStmt, target);

    visit(forEachStmt->getBody());
  }

  void visitGuardStmt(GuardStmt *guardStmt) {
    if (cs.generateConstraints(guardStmt->getCond(), closure))
      hadError = true;
    visit(guardStmt->getBody());
  }

  void visitIfStmt(IfStmt *ifStmt) {
    if (cs.generateConstraints(ifStmt->getCond(), closure))
      hadError = true;

    visit(ifStmt->getThenStmt());
    if (auto elseStmt = ifStmt->getElseStmt())
      visit(elseStmt);
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *repeatWhileStmt) {
    visit(repeatWhileStmt->getBody());

    auto boolDecl = cs.getASTContext().getBoolDecl();
    assert(boolDecl && "Bool is missing");

    SolutionApplicationTarget target(
        repeatWhileStmt->getCond(), closure, CTP_Condition,
        boolDecl->getDeclaredType(), /*isDiscarded=*/false);
    if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow))
      hadError = true;

    cs.setSolutionApplicationTarget(repeatWhileStmt->getCond(), target);
  }

  void visitReturnStmt(ReturnStmt *returnStmt) {
    auto expr = returnStmt->getResult();

    // FIXME: Implies Void return?
    if (!expr)
      return;

    // FIXME: Use SolutionApplicationTarget?
    expr = cs.generateConstraints(expr, closure, /*isInputExpression=*/false);
    if (!expr) {
      hadError = true;
      return;
    }

    // FIXME: Locator should point at the return statement?
    bool hasReturn = hasExplicitResult(closure);
    cs.addConstraint(
        ConstraintKind::Conversion, cs.getType(expr),
        closureResultType,
        cs.getConstraintLocator(
           closure, LocatorPathElt::ClosureBody(hasReturn)));
  }

  void visitWhileStmt(WhileStmt *whileStmt) {
    if (cs.generateConstraints(whileStmt->getCond(), closure))
      hadError = true;
    visit(whileStmt->getBody());
  }

  void visitBreakStmt(BreakStmt *breakStmt) { }
  void visitContinueStmt(ContinueStmt *continueStmt) { }
  void visitDeferStmt(DeferStmt *deferStmt) { }

  void visitThrowStmt(ThrowStmt *throwStmt) {
    Type exnType =
        cs.getASTContext().getErrorDecl()->getDeclaredInterfaceType();
    if (!exnType) {
      hadError = true;
      return;
    }

    SolutionApplicationTarget target(
        throwStmt->getSubExpr(), closure, CTP_ThrowStmt, exnType,
        /*isDiscarded=*/false);
    if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow))
      hadError = true;

    cs.setSolutionApplicationTarget(throwStmt, target);
  }

#define UNSUPPORTED_STMT(STMT) void visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(DoCatch)
  UNSUPPORTED_STMT(Switch)
  UNSUPPORTED_STMT(Case)
  UNSUPPORTED_STMT(Fallthrough)
  UNSUPPORTED_STMT(Fail)
  UNSUPPORTED_STMT(PoundAssert)
#undef UNSUPPORTED_STMT
};

}

bool ConstraintSystem::generateConstraints(
    ClosureExpr *closure, Type resultType) {
  ClosureConstraintGenerator generator(*this, closure, resultType);
  generator.visit(closure->getBody());
  return generator.hadError;
}

// MARK: Solution application

namespace {

/// Statement visitor that applies constraints for a given closure body.
class ClosureConstraintApplication
    : public StmtVisitor<ClosureConstraintApplication, ASTNode> {
  friend StmtVisitor<ClosureConstraintApplication, ASTNode>;

  Solution &solution;
  ClosureExpr *closure;
  Type resultType;
  RewriteTargetFn rewriteTarget;
  bool isSingleExpression;

  ASTContext &getASTContext() const {
    return solution.getConstraintSystem().getASTContext();
  }

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  ClosureConstraintApplication(
      Solution &solution, ClosureExpr *closure, Type resultType,
      RewriteTargetFn rewriteTarget)
    : solution(solution), closure(closure), resultType(resultType),
      rewriteTarget(rewriteTarget),
      isSingleExpression(closure->hasSingleExpressionBody()) { }

private:
  void visitDecl(Decl *decl) {
    // Ignore variable declarations, because they're always handled within
    // their enclosing pattern bindings.
    if (isa<VarDecl>(decl))
      return;

    // Generate constraints for pattern binding declarations.
    if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
      SolutionApplicationTarget target(patternBinding);
      if (!rewriteTarget(target))
        hadError = true;

      return;
    }

    llvm_unreachable("Declarations not supported");
  }

  ASTNode visitBraceStmt(BraceStmt *braceStmt) {
    for (auto &node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        // FIXME: Factor this out completely.
        ASTContext &ctx = solution.getConstraintSystem().getASTContext();
        bool isDiscarded = (!ctx.LangOpts.Playground &&
                            !ctx.LangOpts.DebuggerSupport);
        SolutionApplicationTarget target(
            expr, closure, CTP_Unused, Type(), isDiscarded);

        // Rewrite the expression.
        if (auto rewrittenTarget = rewriteTarget(target))
          node = rewrittenTarget->getAsExpr();
        else
          hadError = true;
      } else if (auto stmt = node.dyn_cast<Stmt *>()) {
        node = visit(stmt);
      } else {
        visitDecl(node.get<Decl *>());
      }
    }

    return braceStmt;
  }

  ASTNode visitDoStmt(DoStmt *doStmt) {
    auto body = visit(doStmt->getBody()).get<Stmt *>();
    doStmt->setBody(cast<BraceStmt>(body));
    return doStmt;
  }

  ASTNode visitForEachStmt(ForEachStmt *forEachStmt) {
    ConstraintSystem &cs = solution.getConstraintSystem();
    auto forEachTarget =
        rewriteTarget(*cs.getSolutionApplicationTarget(forEachStmt));
    if (!forEachTarget)
      hadError = true;

    auto body = visit(forEachStmt->getBody()).get<Stmt *>();
    forEachStmt->setBody(cast<BraceStmt>(body));

    return forEachStmt;
  }

  ASTNode visitGuardStmt(GuardStmt *guardStmt) {
    // Rewrite the condition.
    if (auto condition = rewriteTarget(
            SolutionApplicationTarget(guardStmt->getCond(), closure)))
      guardStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto body = visit(guardStmt->getBody()).get<Stmt *>();
    guardStmt->setBody(cast<BraceStmt>(body));
    return guardStmt;
  }

  ASTNode visitIfStmt(IfStmt *ifStmt) {
    // Rewrite the condition.
    if (auto condition = rewriteTarget(
            SolutionApplicationTarget(ifStmt->getCond(), closure)))
      ifStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    ifStmt->setThenStmt(visit(ifStmt->getThenStmt()).get<Stmt *>());

    if (auto elseStmt = ifStmt->getElseStmt()) {
      ifStmt->setElseStmt(visit(elseStmt).get<Stmt *>());
    }

    return ifStmt;
  }

  ASTNode visitRepeatWhileStmt(RepeatWhileStmt *repeatWhileStmt) {
    auto body = visit(repeatWhileStmt->getBody()).get<Stmt *>();
    repeatWhileStmt->setBody(cast<BraceStmt>(body));

    // Rewrite the condition.
    auto &cs = solution.getConstraintSystem();
    auto target = *cs.getSolutionApplicationTarget(repeatWhileStmt->getCond());
    if (auto condition = rewriteTarget(target))
      repeatWhileStmt->setCond(condition->getAsExpr());
    else
      hadError = true;


    return repeatWhileStmt;
  }

  ASTNode visitReturnStmt(ReturnStmt *returnStmt) {
    auto resultExpr = returnStmt->getResult();
    if (!resultExpr)
      return returnStmt;

    enum {
      convertToResult,
      coerceToVoid,
      coerceFromNever,
    } mode;

    auto resultExprType =
        solution.simplifyType(solution.getType(resultExpr))->getRValueType();
    // A closure with a non-void return expression can coerce to a closure
    // that returns Void.
    if (resultType->isVoid() && !resultExprType->isVoid()) {
      mode = coerceToVoid;

      // A single-expression closure with a Never expression type
      // coerces to any other function type.
    } else if (isSingleExpression && resultExprType->isUninhabited()) {
      mode = coerceFromNever;

      // Normal rule is to coerce to the return expression to the closure type.
    } else {
      mode = convertToResult;
    }

    SolutionApplicationTarget resultTarget(
        resultExpr, closure,
        mode == convertToResult ? CTP_ReturnStmt : CTP_Unused,
        mode == convertToResult ? resultType : Type(),
        /*isDiscarded=*/false);
    if (auto newResultTarget = rewriteTarget(resultTarget))
      resultExpr = newResultTarget->getAsExpr();

    switch (mode) {
    case convertToResult:
      // Record the coerced expression.
      returnStmt->setResult(resultExpr);
      return returnStmt;

    case coerceToVoid: {
      // Evaluate the expression, then produce a return statement that
      // returns nothing.
      TypeChecker::checkIgnoredExpr(resultExpr);
      auto &ctx = solution.getConstraintSystem().getASTContext();
      auto newReturnStmt =
          new (ctx) ReturnStmt(
            returnStmt->getStartLoc(), nullptr, /*implicit=*/true);
      ASTNode elements[2] = { resultExpr, newReturnStmt };
      return BraceStmt::create(ctx, returnStmt->getStartLoc(),
                               elements, returnStmt->getEndLoc(),
                               /*implicit*/ true);
    }

    case coerceFromNever:
      // Replace the return statement with its expression, so that the
      // expression is evaluated directly. This only works because coercion
      // from never is limited to single-expression closures.
      return resultExpr;
    }

    return returnStmt;
  }

  ASTNode visitWhileStmt(WhileStmt *whileStmt) {
    // Rewrite the condition.
    if (auto condition = rewriteTarget(
            SolutionApplicationTarget(whileStmt->getCond(), closure)))
      whileStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto body = visit(whileStmt->getBody()).get<Stmt *>();
    whileStmt->setBody(cast<BraceStmt>(body));
    return whileStmt;
  }

  ASTNode visitBreakStmt(BreakStmt *breakStmt) {
    if (auto target = findBreakOrContinueStmtTarget(
            getASTContext(), closure->getParentSourceFile(), breakStmt->getLoc(),
            breakStmt->getTargetName(), breakStmt->getTargetLoc(),
            /*isContinue=*/false, closure, { })) {
      breakStmt->setTarget(target);
    }

    return breakStmt;
  }

  ASTNode visitContinueStmt(ContinueStmt *continueStmt) {
    if (auto target = findBreakOrContinueStmtTarget(
            getASTContext(), closure->getParentSourceFile(),
            continueStmt->getLoc(), continueStmt->getTargetName(),
            continueStmt->getTargetLoc(), /*isContinue=*/true,
            closure, { })) {
      continueStmt->setTarget(target);
    }

    return continueStmt;
  }

  ASTNode visitDeferStmt(DeferStmt *deferStmt) {
    TypeChecker::typeCheckDecl(deferStmt->getTempDecl());

    Expr *theCall = deferStmt->getCallExpr();
    TypeChecker::typeCheckExpression(theCall, closure);
    deferStmt->setCallExpr(theCall);

    return deferStmt;
  }

  ASTNode visitThrowStmt(ThrowStmt *throwStmt) {
    // Rewrite the condition.
    auto target = *solution.getConstraintSystem()
        .getSolutionApplicationTarget(throwStmt);
    if (auto result = rewriteTarget(target))
      throwStmt->setSubExpr(result->getAsExpr());
    else
      hadError = true;
    return throwStmt;
  }

#define UNSUPPORTED_STMT(STMT) ASTNode visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(DoCatch)
  UNSUPPORTED_STMT(Switch)
  UNSUPPORTED_STMT(Case)
  UNSUPPORTED_STMT(Fallthrough)
  UNSUPPORTED_STMT(Fail)
  UNSUPPORTED_STMT(PoundAssert)
#undef UNSUPPORTED_STMT

};

}

SolutionApplicationToFunctionResult ConstraintSystem::applySolution(
    Solution &solution, AnyFunctionRef fn,
    DeclContext *&currentDC,
    RewriteTargetFn rewriteTarget) {
  auto &cs = solution.getConstraintSystem();
  auto closure = dyn_cast_or_null<ClosureExpr>(fn.getAbstractClosureExpr());
  FunctionType *closureFnType = nullptr;
  if (closure) {
    // Update the closure's type.
    auto closureType = solution.simplifyType(cs.getType(closure));
    cs.setType(closure, closureType);

    // Coerce the parameter types.
    closureFnType = closureType->castTo<FunctionType>();
    auto *params = closure->getParameters();
    TypeChecker::coerceParameterListToType(params, closure, closureFnType);

    // Coerce the result type, if it was written explicitly.
    if (closure->hasExplicitResultType()) {
      closure->setExplicitResultType(closureFnType->getResult());
    }
  }

  // Enter the context of the function before performing any additional
  // transformations.
  llvm::SaveAndRestore<DeclContext *> savedDC(currentDC, fn.getAsDeclContext());

  // Apply the function builder transform, if there is one.
  if (auto transform = solution.getAppliedBuilderTransform(fn)) {
    // Apply the function builder to the closure. We want to be in the
    // context of the closure for subsequent transforms.
    auto newBody = applyFunctionBuilderTransform(
        solution, *transform, fn.getBody(), fn.getAsDeclContext(),
        [&](SolutionApplicationTarget target) {
          auto resultTarget = rewriteTarget(target);
          if (resultTarget) {
            if (auto expr = resultTarget->getAsExpr())
              solution.setExprTypes(expr);
          }

          return resultTarget;
        });
    if (!newBody)
      return SolutionApplicationToFunctionResult::Failure;

    fn.setBody(newBody, /*isSingleExpression=*/false);
    if (closure) {
      solution.setExprTypes(closure);
    }

    return SolutionApplicationToFunctionResult::Success;

  }
  assert(closure && "Can only get here with a closure at the moment");

  // If this closure is checked as part of the enclosing expression, handle
  // that now.
  if (shouldTypeCheckInEnclosingExpression(closure)) {
    ClosureConstraintApplication application(
        solution, closure, closureFnType->getResult(), rewriteTarget);
    application.visit(fn.getBody());
    closure->setBodyState(ClosureExpr::BodyState::TypeCheckedWithSignature);

    return SolutionApplicationToFunctionResult::Success;
  }

  // Otherwise, we need to delay type checking of the closure until later.
  solution.setExprTypes(closure);
  closure->setBodyState(ClosureExpr::BodyState::ReadyForTypeChecking);
  return SolutionApplicationToFunctionResult::Delay;
}
