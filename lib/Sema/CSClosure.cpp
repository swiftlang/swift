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

#include "TypeChecker.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::constraints;

namespace {

/// Find any type variable references inside of an AST node.
class TypeVariableRefFinder : public ASTWalker {
  ConstraintSystem &CS;
  ASTNode Parent;

  llvm::SmallSetVector<TypeVariableType *, 2> referencedVars;

public:
  TypeVariableRefFinder(ConstraintSystem &cs, ASTNode parent)
      : CS(cs), Parent(parent) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      if (auto type = CS.getTypeIfAvailable(DRE->getDecl())) {
        if (auto *typeVar = type->getAs<TypeVariableType>())
          referencedVars.insert(typeVar);
      }
    }

    return {true, expr};
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
    // Return statements have to reference outside result type
    // since all of them are joined by it if it's not specified
    // explicitly.
    if (isa<ReturnStmt>(stmt)) {
      if (auto *closure = getAsExpr<ClosureExpr>(Parent)) {
        auto resultTy = CS.getClosureType(closure)->getResult();
        if (auto *typeVar = resultTy->getAs<TypeVariableType>())
          referencedVars.insert(typeVar);
      }
    }

    return {true, stmt};
  }

  ArrayRef<TypeVariableType *> getReferencedVars() const {
    return referencedVars.getArrayRef();
  }
};

// MARK: Constraint generation

static void createConjunctionForBody(ConstraintSystem &cs, BraceStmt *body,
                                     ASTNode parent,
                                     ConstraintLocator *locator) {
  bool isIsolated = false;
  SmallVector<TypeVariableType *, 2> referencedVars;

  // If this is a top level closure, it has to be isolated.
  if (auto *closure = getAsExpr<ClosureExpr>(parent)) {
    isIsolated = !isa<AbstractClosureExpr>(closure->getParent());
    referencedVars.push_back(cs.getType(closure)->castTo<TypeVariableType>());
  }

  SmallVector<Constraint *, 4> elements;

  for (auto element : body->getElements()) {
    if (auto *decl = element.dyn_cast<Decl *>()) {
      // - Ignore variable declarations, they are handled by pattern bindings;
      // - Ignore #if, the chosen children should appear in the
      // surrounding context;
      // - Skip #warning and #error, they are handled during solution
      //   application.
      if (isa<VarDecl>(decl) || isa<IfConfigDecl>(decl) ||
          isa<PoundDiagnosticDecl>(decl))
        continue;
    }

    TypeVariableRefFinder refFinder(cs, parent);

    // If conjunction would require isolation, each element
    // has to bring any of the referenced outer context
    // type variables into scope.
    if (isIsolated)
      element.walk(refFinder);

    auto *elementLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::ClosureBodyElement(element));

    elements.push_back(Constraint::createClosureBodyElement(
        cs, element, elementLoc, refFinder.getReferencedVars()));
  }

  cs.addUnsolvedConstraint(Constraint::createConjunction(
      cs, elements, isIsolated, locator, referencedVars));
}

/// Statement visitor that generates constraints for a given closure body.
class ClosureConstraintGenerator
    : public StmtVisitor<ClosureConstraintGenerator, void> {
  friend StmtVisitor<ClosureConstraintGenerator, void>;

  ConstraintSystem &cs;
  ClosureExpr *closure;
  ConstraintLocator *locator;

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  ClosureConstraintGenerator(ConstraintSystem &cs, ClosureExpr *closure,
                             ConstraintLocator *locator)
      : cs(cs), closure(closure), locator(locator) {}

private:
  void visitDecl(Decl *decl) {
    if (isSupportedMultiStatementClosure()) {
      if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
        SolutionApplicationTarget target(patternBinding);
        if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow))
          hadError = true;
        return;
      }
    }

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

    llvm_unreachable("Unimplemented case for closure body");
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
    if (isSupportedMultiStatementClosure()) {
      ASTNode parent;
      if (locator->getPath().empty()) {
        parent = locator->getAnchor();
      } else {
        auto parentElt =
            locator->findLast<LocatorPathElt::ClosureBodyElement>();
        assert(parentElt && "body has to have an anchor statement");
        parent = parentElt->getElement();
      }
      createConjunctionForBody(cs, braceStmt, parent, locator);
      return;
    }

    for (auto node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        auto generatedExpr = cs.generateConstraints(
            expr, closure, /*isInputExpression=*/false);
        if (!generatedExpr) {
          hadError = true;
        }
      } else if (auto stmt = node.dyn_cast<Stmt *>()) {
        visit(stmt);
      } else {
        visitDecl(node.get<Decl *>());
      }
    }
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
    cs.addConstraint(ConstraintKind::Conversion, cs.getType(expr),
                     cs.getClosureType(closure)->getResult(),
                     cs.getConstraintLocator(
                         closure, LocatorPathElt::ClosureBody(hasReturn)));
  }

  bool isSupportedMultiStatementClosure() const {
    return !closure->hasSingleExpressionBody() &&
           shouldTypeCheckInEnclosingExpression(closure);
  }

#define UNSUPPORTED_STMT(STMT) void visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(Defer)
  UNSUPPORTED_STMT(If)
  UNSUPPORTED_STMT(Guard)
  UNSUPPORTED_STMT(While)
  UNSUPPORTED_STMT(Do)
  UNSUPPORTED_STMT(DoCatch)
  UNSUPPORTED_STMT(RepeatWhile)
  UNSUPPORTED_STMT(ForEach)
  UNSUPPORTED_STMT(Switch)
  UNSUPPORTED_STMT(Case)
  UNSUPPORTED_STMT(Break)
  UNSUPPORTED_STMT(Continue)
  UNSUPPORTED_STMT(Fallthrough)
  UNSUPPORTED_STMT(Fail)
  UNSUPPORTED_STMT(Throw)
  UNSUPPORTED_STMT(PoundAssert)
#undef UNSUPPORTED_STMT
};
}

bool ConstraintSystem::generateConstraints(ClosureExpr *closure) {
  auto &ctx = closure->getASTContext();

  if (shouldTypeCheckInEnclosingExpression(closure)) {
    ClosureConstraintGenerator generator(*this, closure,
                                         getConstraintLocator(closure));
    generator.visit(closure->getBody());

    if (closure->hasSingleExpressionBody())
      return generator.hadError;
  }

  // If this closure has an empty body and no explicit result type
  // let's bind result type to `Void` since that's the only type empty body
  // can produce. Otherwise, if (multi-statement) closure doesn't have
  // an explicit result (no `return` statements) let's default it to `Void`.
  if (!hasExplicitResult(closure)) {
    auto constraintKind =
        (closure->hasEmptyBody() && !closure->hasExplicitResultType())
            ? ConstraintKind::Bind
            : ConstraintKind::Defaultable;

    addConstraint(
        constraintKind, getClosureType(closure)->getResult(),
        ctx.TheEmptyTupleType,
        getConstraintLocator(closure, ConstraintLocator::ClosureResult));
  }

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyClosureBodyElementConstraint(
    ASTNode element, TypeMatchOptions flags, ConstraintLocatorBuilder locator) {
  auto *closure = castToExpr<ClosureExpr>(locator.getAnchor());

  ClosureConstraintGenerator generator(*this, closure,
                                       getConstraintLocator(locator));

  if (auto *expr = element.dyn_cast<Expr *>()) {
    if (!generateConstraints(expr, closure, /*isInputExpression=*/false))
      return SolutionKind::Error;
  } else if (auto *stmt = element.dyn_cast<Stmt *>()) {
    generator.visit(stmt);
  } else if (auto *cond = element.dyn_cast<StmtCondition *>()) {
    if (generateConstraints(*cond, closure))
      return SolutionKind::Error;
  } else {
    generator.visit(element.get<Decl *>());
  }

  return generator.hadError ? SolutionKind::Error : SolutionKind::Solved;
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
  /// Rewrite an expression without any particularly special context.
  Expr *rewriteExpr(Expr *expr) {
    auto result = rewriteTarget(
      SolutionApplicationTarget(expr, closure, CTP_Unused, Type(),
                                /*isDiscarded=*/false));
    if (result)
      return result->getAsExpr();

    return nullptr;
  }

  void visitDecl(Decl *decl) {
    if (isa<IfConfigDecl>(decl))
      return;

    // Variable declaration would be handled by a pattern binding.
    if (isa<VarDecl>(decl))
      return;

    // Generate constraints for pattern binding declarations.
    if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
      SolutionApplicationTarget target(patternBinding);
      if (!rewriteTarget(target))
        hadError = true;

      return;
    }

    TypeChecker::typeCheckDecl(decl);
  }

  ASTNode visitBraceStmt(BraceStmt *braceStmt) {
    for (auto &node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        // Rewrite the expression.
        if (auto rewrittenExpr = rewriteExpr(expr))
          node = rewrittenExpr;
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

#define UNSUPPORTED_STMT(STMT) ASTNode visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(Defer)
  UNSUPPORTED_STMT(If)
  UNSUPPORTED_STMT(Guard)
  UNSUPPORTED_STMT(While)
  UNSUPPORTED_STMT(Do)
  UNSUPPORTED_STMT(DoCatch)
  UNSUPPORTED_STMT(RepeatWhile)
  UNSUPPORTED_STMT(ForEach)
  UNSUPPORTED_STMT(Switch)
  UNSUPPORTED_STMT(Case)
  UNSUPPORTED_STMT(Break)
  UNSUPPORTED_STMT(Continue)
  UNSUPPORTED_STMT(Fallthrough)
  UNSUPPORTED_STMT(Fail)
  UNSUPPORTED_STMT(Throw)
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
    TypeChecker::coerceParameterListToType(params, closureFnType);

    // Coerce the result type, if it was written explicitly.
    if (closure->hasExplicitResultType()) {
      closure->setExplicitResultType(closureFnType->getResult());
    }
  }

  // Enter the context of the function before performing any additional
  // transformations.
  llvm::SaveAndRestore<DeclContext *> savedDC(currentDC, fn.getAsDeclContext());

  // Apply the result builder transform, if there is one.
  if (auto transform = solution.getAppliedBuilderTransform(fn)) {
    // Apply the result builder to the closure. We want to be in the
    // context of the closure for subsequent transforms.
    auto newBody = applyResultBuilderTransform(
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

    fn.setTypecheckedBody(newBody, /*isSingleExpression=*/false);
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
