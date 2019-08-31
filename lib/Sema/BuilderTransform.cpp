//===--- BuilderTransform.cpp - Function-builder transformation -----------===//
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
// This file implements routines associated with the function-builder
// transformation.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include <iterator>
#include <map>
#include <memory>
#include <utility>
#include <tuple>

using namespace swift;
using namespace constraints;

namespace {

/// Visitor to classify the contents of the given closure.
class BuilderClosureVisitor
    : public StmtVisitor<BuilderClosureVisitor, Expr *> {
  ConstraintSystem *cs;
  ASTContext &ctx;
  bool wantExpr;
  Type builderType;
  NominalTypeDecl *builder = nullptr;
  llvm::SmallDenseMap<Identifier, bool> supportedOps;

public:
  SkipUnhandledConstructInFunctionBuilder::UnhandledNode unhandledNode;

private:
  /// Produce a builder call to the given named function with the given arguments.
  Expr *buildCallIfWanted(SourceLoc loc,
                          Identifier fnName, ArrayRef<Expr *> args,
                          ArrayRef<Identifier> argLabels,
                          bool allowOneWay) {
    if (!wantExpr)
      return nullptr;

    // FIXME: Setting a TypeLoc on this expression is necessary in order
    // to get diagnostics if something about this builder call fails,
    // e.g. if there isn't a matching overload for `buildBlock`.
    // But we can only do this if there isn't a type variable in the type.
    TypeLoc typeLoc;
    if (!builderType->hasTypeVariable()) {
      typeLoc = TypeLoc(new (ctx) FixedTypeRepr(builderType, loc), builderType);
    }

    auto typeExpr = new (ctx) TypeExpr(typeLoc);
    if (cs) {
      cs->setType(typeExpr, MetatypeType::get(builderType));
      cs->setType(&typeExpr->getTypeLoc(), builderType);
    }

    SmallVector<SourceLoc, 4> argLabelLocs;
    for (auto i : indices(argLabels)) {
      argLabelLocs.push_back(args[i]->getStartLoc());
    }

    typeExpr->setImplicit();
    auto memberRef = new (ctx) UnresolvedDotExpr(
        typeExpr, loc, fnName, DeclNameLoc(loc), /*implicit=*/true);
    SourceLoc openLoc = args.empty() ? loc : args.front()->getStartLoc();
    SourceLoc closeLoc = args.empty() ? loc : args.back()->getEndLoc();
    Expr *result = CallExpr::create(ctx, memberRef, openLoc, args,
                                    argLabels, argLabelLocs, closeLoc,
                                    /*trailing closure*/ nullptr,
                                    /*implicit*/true);

    if (ctx.LangOpts.FunctionBuilderOneWayConstraints && allowOneWay) {
      // Form a one-way constraint to prevent backward propagation.
      result = new (ctx) OneWayExpr(result);
    }

    return result;
  }

  /// Check whether the builder supports the given operation.
  bool builderSupports(Identifier fnName,
                       ArrayRef<Identifier> argLabels = {}) {
    auto known = supportedOps.find(fnName);
    if (known != supportedOps.end()) {
      return known->second;
    }

    bool found = false;
    for (auto decl : builder->lookupDirect(fnName)) {
      if (auto func = dyn_cast<FuncDecl>(decl)) {
        // Function must be static.
        if (!func->isStatic())
          continue;

        // Function must have the right argument labels, if provided.
        if (!argLabels.empty()) {
          auto funcLabels = func->getFullName().getArgumentNames();
          if (argLabels.size() > funcLabels.size() ||
              funcLabels.slice(0, argLabels.size()) != argLabels)
            continue;
        }

        // Okay, it's a good-enough match.
        found = true;
        break;
      }
    }

    return supportedOps[fnName] = found;
  }

public:
  BuilderClosureVisitor(ASTContext &ctx, ConstraintSystem *cs,
                        bool wantExpr, Type builderType)
      : cs(cs), ctx(ctx), wantExpr(wantExpr), builderType(builderType) {
    assert((cs || !builderType->hasTypeVariable()) &&
           "cannot handle builder type with type variables without "
           "constraint system");
    builder = builderType->getAnyNominal();
  }

#define CONTROL_FLOW_STMT(StmtClass)                      \
  Expr *visit##StmtClass##Stmt(StmtClass##Stmt *stmt) { \
    if (!unhandledNode)                                 \
      unhandledNode = stmt;                             \
                                                        \
    return nullptr;                                     \
  }

  Expr *visitBraceStmt(BraceStmt *braceStmt) {
    SmallVector<Expr *, 4> expressions;
    for (const auto &node : braceStmt->getElements()) {
      if (auto stmt = node.dyn_cast<Stmt *>()) {
        auto expr = visit(stmt);
        if (expr)
          expressions.push_back(expr);
        continue;
      }

      if (auto decl = node.dyn_cast<Decl *>()) {
        // Just ignore #if; the chosen children should appear in the
        // surrounding context.  This isn't good for source tools but it
        // at least works.
        if (isa<IfConfigDecl>(decl))
          continue;

        if (!unhandledNode)
          unhandledNode = decl;

        continue;
      }

      auto expr = node.get<Expr *>();
      if (wantExpr && ctx.LangOpts.FunctionBuilderOneWayConstraints)
        expr = new (ctx) OneWayExpr(expr);

      expressions.push_back(expr);
    }

    // Call Builder.buildBlock(... args ...)
    return buildCallIfWanted(braceStmt->getStartLoc(),
                             ctx.Id_buildBlock, expressions,
                             /*argLabels=*/{ },
                             /*allowOneWay=*/true);
  }

  Expr *visitReturnStmt(ReturnStmt *stmt) {
    // Allow implicit returns due to 'return' elision.
    if (!stmt->isImplicit() || !stmt->hasResult()) {
      if (!unhandledNode)
        unhandledNode = stmt;
      return nullptr;
    }

    return stmt->getResult();
  }

  Expr *visitDoStmt(DoStmt *doStmt) {
    if (!builderSupports(ctx.Id_buildDo)) {
      if (!unhandledNode)
        unhandledNode = doStmt;
      return nullptr;
    }

    auto arg = visit(doStmt->getBody());
    if (!arg)
      return nullptr;

    return buildCallIfWanted(doStmt->getStartLoc(), ctx.Id_buildDo, arg,
                             /*argLabels=*/{ }, /*allowOneWay=*/true);
  }

  CONTROL_FLOW_STMT(Yield)
  CONTROL_FLOW_STMT(Defer)

  static Expr *getTrivialBooleanCondition(StmtCondition condition) {
    if (condition.size() != 1)
      return nullptr;

    return condition.front().getBooleanOrNull();
  }

  static bool isBuildableIfChainRecursive(IfStmt *ifStmt,
                                          unsigned &numPayloads,
                                          bool &isOptional) {
    // The conditional must be trivial.
    if (!getTrivialBooleanCondition(ifStmt->getCond()))
      return false;

    // The 'then' clause contributes a payload.
    numPayloads++;

    // If there's an 'else' clause, it contributes payloads:
    if (auto elseStmt = ifStmt->getElseStmt()) {
      // If it's 'else if', it contributes payloads recursively.
      if (auto elseIfStmt = dyn_cast<IfStmt>(elseStmt)) {
        return isBuildableIfChainRecursive(elseIfStmt, numPayloads,
                                           isOptional);
      // Otherwise it's just the one.
      } else {
        numPayloads++;
      }

    // If not, the chain result is at least optional.
    } else {
      isOptional = true;
    }

    return true;
  }

  bool isBuildableIfChain(IfStmt *ifStmt, unsigned &numPayloads,
                          bool &isOptional) {
    if (!isBuildableIfChainRecursive(ifStmt, numPayloads, isOptional))
      return false;

    // If there's a missing 'else', we need 'buildIf' to exist.
    if (isOptional && !builderSupports(ctx.Id_buildIf))
      return false;

    // If there are multiple clauses, we need 'buildEither(first:)' and
    // 'buildEither(second:)' to both exist.
    if (numPayloads > 1) {
      if (!builderSupports(ctx.Id_buildEither, {ctx.Id_first}) ||
          !builderSupports(ctx.Id_buildEither, {ctx.Id_second}))
        return false;
    }

    return true;
  }

  Expr *visitIfStmt(IfStmt *ifStmt) {
    // Check whether the chain is buildable and whether it terminates
    // without an `else`.
    bool isOptional = false;
    unsigned numPayloads = 0;
    if (!isBuildableIfChain(ifStmt, numPayloads, isOptional)) {
      if (!unhandledNode)
        unhandledNode = ifStmt;
      return nullptr;
    }

    // Attempt to build the chain, propagating short-circuits, which
    // might arise either do to error or not wanting an expression.
    auto chainExpr =
      buildIfChainRecursive(ifStmt, 0, numPayloads, isOptional);
    if (!chainExpr)
      return nullptr;
    assert(wantExpr);

    // The operand should have optional type if we had optional results,
    // so we just need to call `buildIf` now, since we're at the top level.
    if (isOptional) {
      chainExpr = buildCallIfWanted(ifStmt->getStartLoc(),
                                    ctx.Id_buildIf, chainExpr,
                                    /*argLabels=*/{ },
                                    /*allowOneWay=*/true);
    } else if (ctx.LangOpts.FunctionBuilderOneWayConstraints) {
      // Form a one-way constraint to prevent backward propagation.
      chainExpr = new (ctx) OneWayExpr(chainExpr);
    }

    return chainExpr;
  }

  /// Recursively build an if-chain: build an expression which will have
  /// a value of the chain result type before any call to `buildIf`.
  /// The expression will perform any necessary calls to `buildEither`,
  /// and the result will have optional type if `isOptional` is true.
  Expr *buildIfChainRecursive(IfStmt *ifStmt, unsigned payloadIndex,
                              unsigned numPayloads, bool isOptional) {
    assert(payloadIndex < numPayloads);
    // Make sure we recursively visit both sides even if we're not
    // building expressions.

    // Build the then clause.  This will have the corresponding payload
    // type (i.e. not wrapped in any way).
    Expr *thenArg = visit(ifStmt->getThenStmt());

    // Build the else clause, if present.  If this is from an else-if,
    // this will be fully wrapped; otherwise it will have the corresponding
    // payload type (at index `payloadIndex + 1`).
    assert(ifStmt->getElseStmt() || isOptional);
    bool isElseIf = false;
    Optional<Expr *> elseChain;
    if (auto elseStmt = ifStmt->getElseStmt()) {
      if (auto elseIfStmt = dyn_cast<IfStmt>(elseStmt)) {
        isElseIf = true;
        elseChain = buildIfChainRecursive(elseIfStmt, payloadIndex + 1,
                                          numPayloads, isOptional);
      } else {
        elseChain = visit(elseStmt);
      }
    }

    // Short-circuit if appropriate.
    if (!wantExpr || !thenArg || (elseChain && !*elseChain))
      return nullptr;

    // Okay, build the conditional expression.

    // Prepare the `then` operand by wrapping it to produce a chain result.
    SourceLoc thenLoc = ifStmt->getThenStmt()->getStartLoc();
    Expr *thenExpr = buildWrappedChainPayload(thenArg, payloadIndex,
                                              numPayloads, isOptional);

    // Prepare the `else operand:
    Expr *elseExpr;
    SourceLoc elseLoc;

    // - If there's no `else` clause, use `Optional.none`.
    if (!elseChain) {
      assert(isOptional);
      elseLoc = ifStmt->getEndLoc();
      elseExpr = buildNoneExpr(elseLoc);

    // - If there's an `else if`, the chain expression from that
    //   should already be producing a chain result.
    } else if (isElseIf) {
      elseExpr = *elseChain;
      elseLoc = ifStmt->getElseLoc();

    // - Otherwise, wrap it to produce a chain result.
    } else {
      elseLoc = ifStmt->getElseLoc();
      elseExpr = buildWrappedChainPayload(*elseChain,
                                          payloadIndex + 1, numPayloads,
                                          isOptional);
    }

    Expr *condition = getTrivialBooleanCondition(ifStmt->getCond());
    assert(condition && "checked by isBuildableIfChain");

    auto ifExpr = new (ctx) IfExpr(condition, thenLoc, thenExpr,
                                   elseLoc, elseExpr);
    ifExpr->setImplicit();
    return ifExpr;
  }

  /// Wrap a payload value in an expression which will produce a chain
  /// result (without `buildIf`).
  Expr *buildWrappedChainPayload(Expr *operand, unsigned payloadIndex,
                                 unsigned numPayloads, bool isOptional) {
    assert(payloadIndex < numPayloads);

    // Inject into the appropriate chain position.
    //
    // We produce a (left-biased) balanced binary tree of Eithers in order
    // to prevent requiring a linear number of injections in the worst case.
    // That is, if we have 13 clauses, we want to produce:
    //
    //                      /------------------Either------------\
    //           /-------Either-------\                     /--Either--\
    //     /--Either--\          /--Either--\          /--Either--\     \
    //   /-E-\      /-E-\      /-E-\      /-E-\      /-E-\      /-E-\    \
    // 0000 0001  0010 0011  0100 0101  0110 0111  1000 1001  1010 1011 1100
    //
    // Note that a prefix of length D of the payload index acts as a path
    // through the tree to the node at depth D.  On the rightmost path
    // through the tree (when this prefix is equal to the corresponding
    // prefix of the maximum payload index), the bits of the index mark
    // where Eithers are required.
    //
    // Since we naturally want to build from the innermost Either out, and
    // therefore work with progressively shorter prefixes, we can do it all
    // with right-shifts.
    for (auto path = payloadIndex, maxPath = numPayloads - 1;
         maxPath != 0; path >>= 1, maxPath >>= 1) {
      // Skip making Eithers on the rightmost path where they aren't required.
      // This isn't just an optimization: adding spurious Eithers could
      // leave us with unresolvable type variables if `buildEither` has
      // a signature like:
      //    static func buildEither<T,U>(first value: T) -> Either<T,U>
      // which relies on unification to work.
      if (path == maxPath && !(maxPath & 1)) continue;

      bool isSecond = (path & 1);
      operand = buildCallIfWanted(operand->getStartLoc(),
                                  ctx.Id_buildEither, operand,
                                  {isSecond ? ctx.Id_second : ctx.Id_first},
                                  /*allowOneWay=*/false);
    }

    // Inject into Optional if required.  We'll be adding the call to
    // `buildIf` after all the recursive calls are complete.
    if (isOptional) {
      operand = buildSomeExpr(operand);
    }

    return operand;
  }

  Expr *buildSomeExpr(Expr *arg) {
    auto optionalDecl = ctx.getOptionalDecl();
    auto optionalType = optionalDecl->getDeclaredType();

    auto loc = arg->getStartLoc();
    auto optionalTypeExpr =
      TypeExpr::createImplicitHack(loc, optionalType, ctx);
    auto someRef = new (ctx) UnresolvedDotExpr(
        optionalTypeExpr, loc, ctx.getIdentifier("some"),
        DeclNameLoc(loc), /*implicit=*/true);
    return CallExpr::createImplicit(ctx, someRef, arg, { });
  }

  Expr *buildNoneExpr(SourceLoc endLoc) {
    auto optionalDecl = ctx.getOptionalDecl();
    auto optionalType = optionalDecl->getDeclaredType();

    auto optionalTypeExpr =
      TypeExpr::createImplicitHack(endLoc, optionalType, ctx);
    return new (ctx) UnresolvedDotExpr(
        optionalTypeExpr, endLoc, ctx.getIdentifier("none"),
        DeclNameLoc(endLoc), /*implicit=*/true);
  }

  CONTROL_FLOW_STMT(Guard)
  CONTROL_FLOW_STMT(While)
  CONTROL_FLOW_STMT(DoCatch)
  CONTROL_FLOW_STMT(RepeatWhile)
  CONTROL_FLOW_STMT(ForEach)
  CONTROL_FLOW_STMT(Switch)
  CONTROL_FLOW_STMT(Case)
  CONTROL_FLOW_STMT(Catch)
  CONTROL_FLOW_STMT(Break)
  CONTROL_FLOW_STMT(Continue)
  CONTROL_FLOW_STMT(Fallthrough)
  CONTROL_FLOW_STMT(Fail)
  CONTROL_FLOW_STMT(Throw)
  CONTROL_FLOW_STMT(PoundAssert)

#undef CONTROL_FLOW_STMT
};

} // end anonymous namespace

BraceStmt *
TypeChecker::applyFunctionBuilderBodyTransform(FuncDecl *FD,
                                               BraceStmt *body,
                                               Type builderType) {
  // Try to build a single result expression.
  BuilderClosureVisitor visitor(Context, nullptr,
                                /*wantExpr=*/true, builderType);
  Expr *returnExpr = visitor.visit(body);
  if (!returnExpr)
    return nullptr;

  // Make sure we have a usable result type for the body.
  Type returnType = AnyFunctionRef(FD).getBodyResultType();
  if (!returnType || returnType->hasError())
    return nullptr;

  auto loc = returnExpr->getStartLoc();
  auto returnStmt =
    new (Context) ReturnStmt(loc, returnExpr, /*implicit*/ true);
  return BraceStmt::create(Context, body->getLBraceLoc(), { returnStmt },
                           body->getRBraceLoc());
}

ConstraintSystem::TypeMatchResult ConstraintSystem::applyFunctionBuilder(
    ClosureExpr *closure, Type builderType, ConstraintLocator *calleeLocator,
    ConstraintLocatorBuilder locator) {
  auto builder = builderType->getAnyNominal();
  assert(builder && "Bad function builder type");
  assert(builder->getAttrs().hasAttribute<FunctionBuilderAttr>());

  // FIXME: Right now, single-expression closures suppress the function
  // builder translation.
  if (closure->hasSingleExpressionBody())
    return getTypeMatchSuccess();

  // Pre-check the closure body: pre-check any expressions in it and look
  // for return statements.
  switch (TC.preCheckFunctionBuilderClosureBody(closure)) {
  case FunctionBuilderClosurePreCheck::Okay:
    // If the pre-check was okay, apply the function-builder transform.
    break;

  case FunctionBuilderClosurePreCheck::Error:
    // If the pre-check had an error, flag that.
    return getTypeMatchFailure(locator);

  case FunctionBuilderClosurePreCheck::HasReturnStmt:
    // If the closure has a return statement, suppress the transform but
    // continue solving the constraint system.
    return getTypeMatchSuccess();
  }

  // Check the form of this closure to see if we can apply the
  // function-builder translation at all.
  {
    // Check whether we can apply this specific function builder.
    BuilderClosureVisitor visitor(getASTContext(), this,
                                  /*wantExpr=*/false, builderType);
    (void)visitor.visit(closure->getBody());

    // If we saw a control-flow statement or declaration that the builder
    // cannot handle, we don't have a well-formed function builder application.
    if (visitor.unhandledNode) {
      // If we aren't supposed to attempt fixes, fail.
      if (!shouldAttemptFixes()) {
        return getTypeMatchFailure(locator);
      }

      // Record the first unhandled construct as a fix.
      if (recordFix(
              SkipUnhandledConstructInFunctionBuilder::create(
                *this, visitor.unhandledNode, builder,
                getConstraintLocator(locator)))) {
        return getTypeMatchFailure(locator);
      }
    }
  }

  // If the builder type has a type parameter, substitute in the type
  // variables.
  if (builderType->hasTypeParameter()) {
    // Find the opened type for this callee and substitute in the type
    // parametes.
    for (const auto &opened : OpenedTypes) {
      if (opened.first == calleeLocator) {
        OpenedTypeMap replacements(opened.second.begin(),
                                   opened.second.end());
        builderType = openType(builderType, replacements);
        break;
      }
    }
    assert(!builderType->hasTypeParameter());
  }

  BuilderClosureVisitor visitor(getASTContext(), this,
                                /*wantExpr=*/true, builderType);
  Expr *singleExpr = visitor.visit(closure->getBody());

  // We've already pre-checked all the original expressions, but do the
  // pre-check to the generated expression just to set up any preconditions
  // that CSGen might have.
  //
  // TODO: just build the AST the way we want it in the first place.
  if (TC.preCheckExpression(singleExpr, closure))
    return getTypeMatchFailure(locator);

  singleExpr = generateConstraints(singleExpr, closure);
  if (!singleExpr)
    return getTypeMatchFailure(locator);

  Type transformedType = getType(singleExpr);
  assert(transformedType && "Missing type");

  // Record the transformation.
  assert(std::find_if(builderTransformedClosures.begin(),
                      builderTransformedClosures.end(),
                      [&](const std::tuple<ClosureExpr *, Type, Expr *> &elt) {
                        return std::get<0>(elt) == closure;
                      }) == builderTransformedClosures.end() &&
         "already transformed this closure along this path!?!");
  builderTransformedClosures.push_back(
    std::make_tuple(closure, builderType, singleExpr));

  // Bind the result type of the closure to the type of the transformed
  // expression.
  Type closureType = getType(closure);
  auto fnType = closureType->castTo<FunctionType>();
  addConstraint(ConstraintKind::Equal, fnType->getResult(), transformedType,
                locator);
  return getTypeMatchSuccess();
}

namespace {

/// Pre-check all the expressions in the closure body.
class PreCheckFunctionBuilderClosure : public ASTWalker {
  TypeChecker &TC;
  ClosureExpr *Closure;
  bool HasReturnStmt = false;
  bool HasError = false;
public:
  PreCheckFunctionBuilderClosure(TypeChecker &tc, ClosureExpr *closure)
    : TC(tc), Closure(closure) {}

  FunctionBuilderClosurePreCheck run() {
    Stmt *oldBody = Closure->getBody();

    Stmt *newBody = oldBody->walk(*this);

    // If the walk was aborted, it was because we had a problem of some kind.
    assert((newBody == nullptr) == (HasError || HasReturnStmt) &&
           "unexpected short-circuit while walking closure body");
    if (!newBody) {
      if (HasError)
        return FunctionBuilderClosurePreCheck::Error;

      return FunctionBuilderClosurePreCheck::HasReturnStmt;
    }

    assert(oldBody == newBody && "pre-check walk wasn't in-place?");

    return FunctionBuilderClosurePreCheck::Okay;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    // Pre-check the expression.  If this fails, abort the walk immediately.
    // Otherwise, replace the expression with the result of pre-checking.
    // In either case, don't recurse into the expression.
    if (TC.preCheckExpression(E, /*DC*/ Closure)) {
      HasError = true;
      return std::make_pair(false, nullptr);
    }

    return std::make_pair(false, E);
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    // If we see a return statement, abort the walk immediately.
    if (isa<ReturnStmt>(S)) {
      HasReturnStmt = true;
      return std::make_pair(false, nullptr);
    }

    // Otherwise, recurse into the statement normally.
    return std::make_pair(true, S);
  }
};

}

FunctionBuilderClosurePreCheck
TypeChecker::preCheckFunctionBuilderClosureBody(ClosureExpr *closure) {
  // Single-expression closures should already have been pre-checked.
  if (closure->hasSingleExpressionBody())
    return FunctionBuilderClosurePreCheck::Okay;

  // Check whether we've already done this analysis.
  auto it = precheckedFunctionBuilderClosures.find(closure);
  if (it != precheckedFunctionBuilderClosures.end())
    return it->second;

  auto result = PreCheckFunctionBuilderClosure(*this, closure).run();

  // Cache the result.
  precheckedFunctionBuilderClosures.insert(std::make_pair(closure, result));

  return result;
}
