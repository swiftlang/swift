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

    llvm_unreachable("Unimplemented case for closure body");
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
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

bool ConstraintSystem::generateConstraints(
    ClosureExpr *closure, Type resultType) {
  ClosureConstraintGenerator generator(*this, closure, resultType);
  generator.visit(closure->getBody());
  return generator.hadError;
}

// MARK: Solution application

/// Coerce the body of the given closure expression so that it returns
/// Void rather than the value in it.
static void coerceClosureExprToVoid(
    Solution &solution, ClosureExpr *closure) {
  auto &cs = solution.getConstraintSystem();
  auto &ctx = cs.getASTContext();

  // Re-write the single-expression closure to return '()'
  assert(closure->hasSingleExpressionBody());

  // A single-expression body contains a single return statement
  // prior to this transformation.
  auto member = closure->getBody()->getFirstElement();

  if (member.is<Stmt *>()) {
    auto returnStmt = cast<ReturnStmt>(member.get<Stmt *>());
    auto singleExpr = returnStmt->getResult();
    auto voidExpr = cs.cacheType(TupleExpr::createEmpty(
        ctx, singleExpr->getStartLoc(), singleExpr->getEndLoc(),
        /*implicit*/ true));
    returnStmt->setResult(voidExpr);

    // For l-value types, reset to the object type. This might not be strictly
    // necessary any more, but it's probably still a good idea.
    if (cs.getType(singleExpr)->is<LValueType>())
      cs.setType(singleExpr,
                 cs.getType(singleExpr)->getWithoutSpecifierType());

    solution.setExprTypes(singleExpr);
    TypeChecker::checkIgnoredExpr(singleExpr);

    SmallVector<ASTNode, 2> elements;
    elements.push_back(singleExpr);
    elements.push_back(returnStmt);

    auto braceStmt = BraceStmt::create(ctx, closure->getStartLoc(),
                                       elements, closure->getEndLoc(),
                                       /*implicit*/ true);

    closure->setImplicit();
    closure->setBody(braceStmt, /*isSingleExpression*/true);
  }

  // Finally, compute the proper type for the closure.
  auto fnType = cs.getType(closure)->getAs<FunctionType>();
  auto newClosureType = FunctionType::get(
      fnType->getParams(), ctx.TheEmptyTupleType, fnType->getExtInfo());
  cs.setType(closure, newClosureType);
}

/// Coerce a closure whose body produces \c Never into one that does not
/// return its result.
static void coerceClosureExprFromNever(
    Solution &solution, ClosureExpr *closure) {
  auto &cs = solution.getConstraintSystem();

  // Re-write the single-expression closure to drop the 'return'.
  assert(closure->hasSingleExpressionBody());

  // A single-expression body contains a single return statement
  // prior to this transformation.
  auto member = closure->getBody()->getFirstElement();

  if (member.is<Stmt *>()) {
    auto returnStmt = cast<ReturnStmt>(member.get<Stmt *>());
    auto singleExpr = returnStmt->getResult();

    solution.setExprTypes(singleExpr);
    TypeChecker::checkIgnoredExpr(singleExpr);

    SmallVector<ASTNode, 1> elements;
    elements.push_back(singleExpr);

    auto braceStmt =
        BraceStmt::create(cs.getASTContext(), closure->getStartLoc(),
                          elements, closure->getEndLoc(),
                          /*implicit*/ true);

    closure->setImplicit();
    closure->setBody(braceStmt, /*isSingleExpression*/true);
  }
}

SolutionApplicationToFunctionResult ConstraintSystem::applySolution(
    Solution &solution, AnyFunctionRef fn,
    DeclContext *&currentDC,
    std::function<
      Optional<SolutionApplicationTarget> (SolutionApplicationTarget)>
        rewriteTarget) {
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
      closure->setAppliedFunctionBuilder();
      solution.setExprTypes(closure);
    }

    return SolutionApplicationToFunctionResult::Success;
  }

  // If there is a single-expression body, transform that body now.
  if (fn.hasSingleExpressionBody()) {
    // Rewrite the body.
    SolutionApplicationTarget originalBody(
        fn.getSingleExpressionBody(), fn.getAsDeclContext(), CTP_Unused,
        Type(), /*isDiscarded=*/false);
    auto rewrittenBody = rewriteTarget(originalBody);
    if (!rewrittenBody)
      return SolutionApplicationToFunctionResult::Failure;

    auto body = rewrittenBody->getAsExpr();
    fn.setSingleExpressionBody(body);

    // Closures with a single-expression body have special rules regarding
    // Void and Never returns. Handle them now.
    if (closure && closure->hasSingleExpressionBody()) {
      auto bodyType = body->getType();

      // A single-expression closure with a non-Void expression type
      // coerces to a Void-returning function type.
      if (closureFnType->getResult()->isVoid() && !bodyType->isVoid()) {
        coerceClosureExprToVoid(solution, closure);
      // A single-expression closure with a Never expression type
      // coerces to any other function type.
      } else if (bodyType->isUninhabited()) {
        coerceClosureExprFromNever(solution, closure);
      } else {
        body = solution.coerceToType(body, closureFnType->getResult(),
                                     cs.getConstraintLocator(
                                       closure,
                                       ConstraintLocator::ClosureResult));
        if (!body)
          return SolutionApplicationToFunctionResult::Failure;

        closure->setSingleExpressionBody(body);
      }
    }

    return SolutionApplicationToFunctionResult::Success;
  }

  // Otherwise, we need to delay type checking of the closure until later.
  solution.setExprTypes(closure);
  return SolutionApplicationToFunctionResult::Delay;
}

