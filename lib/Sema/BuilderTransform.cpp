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
#include "MiscDiagnostics.h"
#include "SolutionResult.h"
#include "TypeChecker.h"
#include "swift/AST/ASTVisitor.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeCheckRequests.h"
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
    : private StmtVisitor<BuilderClosureVisitor, VarDecl *> {

  friend StmtVisitor<BuilderClosureVisitor, VarDecl *>;

  ConstraintSystem *cs;
  DeclContext *dc;
  ASTContext &ctx;
  Type builderType;
  NominalTypeDecl *builder = nullptr;
  Identifier buildOptionalId;
  llvm::SmallDenseMap<Identifier, bool> supportedOps;

  SkipUnhandledConstructInFunctionBuilder::UnhandledNode unhandledNode;

  /// Whether an error occurred during application of the builder closure,
  /// e.g., during constraint generation.
  bool hadError = false;

  /// Counter used to give unique names to the variables that are
  /// created implicitly.
  unsigned varCounter = 0;

  /// The record of what happened when we applied the builder transform.
  AppliedBuilderTransform applied;

  /// Produce a builder call to the given named function with the given
  /// arguments.
  Expr *buildCallIfWanted(SourceLoc loc,
                          Identifier fnName, ArrayRef<Expr *> args,
                          ArrayRef<Identifier> argLabels) {
    if (!cs)
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
    cs->setType(typeExpr, MetatypeType::get(builderType));
    cs->setType(&typeExpr->getTypeLoc(), builderType);

    SmallVector<SourceLoc, 4> argLabelLocs;
    for (auto i : indices(argLabels)) {
      argLabelLocs.push_back(args[i]->getStartLoc());
    }

    typeExpr->setImplicit();
    auto memberRef = new (ctx) UnresolvedDotExpr(
        typeExpr, loc, DeclNameRef(fnName), DeclNameLoc(loc),
        /*implicit=*/true);
    memberRef->setFunctionRefKind(FunctionRefKind::SingleApply);
    SourceLoc openLoc = args.empty() ? loc : args.front()->getStartLoc();
    SourceLoc closeLoc = args.empty() ? loc : args.back()->getEndLoc();
    Expr *result = CallExpr::create(ctx, memberRef, openLoc, args,
                                    argLabels, argLabelLocs, closeLoc,
                                    /*trailing closures*/{},
                                    /*implicit*/true);

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
          auto funcLabels = func->getName().getArgumentNames();
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

  /// Build an implicit variable in this context.
  VarDecl *buildVar(SourceLoc loc) {
    // Create the implicit variable.
    Identifier name = ctx.getIdentifier(
        ("$__builder" + Twine(varCounter++)).str());
    auto var = new (ctx) VarDecl(/*isStatic=*/false, VarDecl::Introducer::Var,
                                 /*isCaptureList=*/false, loc, name, dc);
    var->setImplicit();
    return var;
  }

  /// Capture the given expression into an implicitly-generated variable.
  VarDecl *captureExpr(Expr *expr, bool oneWay,
                       llvm::PointerUnion<Stmt *, Expr *> forEntity = nullptr) {
    if (!cs)
      return nullptr;

    Expr *origExpr = expr;

    if (oneWay) {
      // Form a one-way constraint to prevent backward propagation.
      expr = new (ctx) OneWayExpr(expr);
    }

    // Generate constraints for this expression.
    expr = cs->generateConstraints(expr, dc);
    if (!expr) {
      hadError = true;
      return nullptr;
    }

    // Create the implicit variable.
    auto var = buildVar(expr->getStartLoc());

    // Record the new variable and its corresponding expression & statement.
    if (auto forStmt = forEntity.dyn_cast<Stmt *>()) {
      applied.capturedStmts.insert({forStmt, { var, { expr } }});
    } else {
      if (auto forExpr = forEntity.dyn_cast<Expr *>())
        origExpr = forExpr;

      applied.capturedExprs.insert({origExpr, {var, expr}});
    }

    cs->setType(var, cs->getType(expr));
    return var;
  }

  /// Build an implicit reference to the given variable.
  DeclRefExpr *buildVarRef(VarDecl *var, SourceLoc loc) {
    return new (ctx) DeclRefExpr(var, DeclNameLoc(loc), /*Implicit=*/true);
  }

public:
  BuilderClosureVisitor(ASTContext &ctx, ConstraintSystem *cs,
                        DeclContext *dc, Type builderType,
                        Type bodyResultType)
      : cs(cs), dc(dc), ctx(ctx), builderType(builderType) {
    assert((cs || !builderType->hasTypeVariable()) &&
           "cannot handle builder type with type variables without "
           "constraint system");
    builder = builderType->getAnyNominal();
    applied.builderType = builderType;
    applied.bodyResultType = bodyResultType;

    // Use buildOptional(_:) if available, otherwise fall back to buildIf
    // when available.
    if (builderSupports(ctx.Id_buildOptional) ||
        !builderSupports(ctx.Id_buildIf))
      buildOptionalId = ctx.Id_buildOptional;
    else
      buildOptionalId = ctx.Id_buildIf;
  }

  /// Apply the builder transform to the given statement.
  Optional<AppliedBuilderTransform> apply(Stmt *stmt) {
    VarDecl *bodyVar = visit(stmt);
    if (!bodyVar)
      return None;

    applied.returnExpr = buildVarRef(bodyVar, stmt->getEndLoc());

    // If there is a buildFinalResult(_:), call it.
    ASTContext &ctx = cs->getASTContext();
    if (builderSupports(ctx.Id_buildFinalResult, { Identifier() })) {
      applied.returnExpr = buildCallIfWanted(
          applied.returnExpr->getLoc(), ctx.Id_buildFinalResult,
          { applied.returnExpr }, { Identifier() });
    }

    applied.returnExpr = cs->buildTypeErasedExpr(applied.returnExpr,
                                                 dc, applied.bodyResultType,
                                                 CTP_ReturnStmt);

    applied.returnExpr = cs->generateConstraints(applied.returnExpr, dc);
    if (!applied.returnExpr) {
      hadError = true;
      return None;
    }

    return std::move(applied);
  }

  /// Check whether the function builder can be applied to this statement.
  /// \returns the node that cannot be handled by this builder on failure.
  SkipUnhandledConstructInFunctionBuilder::UnhandledNode check(Stmt *stmt) {
    (void)visit(stmt);
    return unhandledNode;
  }

protected:
#define CONTROL_FLOW_STMT(StmtClass)                       \
  VarDecl *visit##StmtClass##Stmt(StmtClass##Stmt *stmt) { \
    if (!unhandledNode)                                    \
      unhandledNode = stmt;                                \
                                                           \
    return nullptr;                                        \
  }

  void visitPatternBindingDecl(PatternBindingDecl *patternBinding) {
    // If any of the entries lacks an initializer, don't handle this node.
    if (!llvm::all_of(range(patternBinding->getNumPatternEntries()),
                      [&](unsigned index) {
            return patternBinding->isExplicitlyInitialized(index);
        })) {
      if (!unhandledNode)
        unhandledNode = patternBinding;
      return;
    }

    // If we aren't generating constraints, there's nothing to do.
    if (!cs)
      return;

    /// Generate constraints for each pattern binding entry
    for (unsigned index : range(patternBinding->getNumPatternEntries())) {
      // Type check the pattern.
      auto pattern = patternBinding->getPattern(index);
      auto contextualPattern = ContextualPattern::forRawPattern(pattern, dc);
      Type patternType = TypeChecker::typeCheckPattern(contextualPattern);

      // Generate constraints for the initialization.
      auto target = SolutionApplicationTarget::forInitialization(
          patternBinding->getInit(index), dc, patternType, pattern,
          /*bindPatternVarsOneWay=*/true);
      if (cs->generateConstraints(target, FreeTypeVariableBinding::Disallow)) {
        hadError = true;
        continue;
      }

      // Keep track of this binding entry.
      applied.patternBindingEntries.insert({{patternBinding, index}, target});
    }
  }

  VarDecl *visitBraceStmt(BraceStmt *braceStmt) {
    SmallVector<Expr *, 4> expressions;
    auto addChild = [&](VarDecl *childVar) {
      if (!childVar)
        return;

      expressions.push_back(buildVarRef(childVar, childVar->getLoc()));
    };

    for (auto node : braceStmt->getElements()) {
      // Implicit returns in single-expression function bodies are treated
      // as the expression.
      if (auto returnStmt =
              dyn_cast_or_null<ReturnStmt>(node.dyn_cast<Stmt *>())) {
        assert(returnStmt->isImplicit());
        node = returnStmt->getResult();
      }

      if (auto stmt = node.dyn_cast<Stmt *>()) {
        addChild(visit(stmt));
        continue;
      }

      if (auto decl = node.dyn_cast<Decl *>()) {
        // Just ignore #if; the chosen children should appear in the
        // surrounding context.  This isn't good for source tools but it
        // at least works.
        if (isa<IfConfigDecl>(decl))
          continue;

        // Skip #warning/#error; we'll handle them when applying the builder.
        if (isa<PoundDiagnosticDecl>(decl)) {
          continue;
        }

        // Pattern bindings are okay so long as all of the entries are
        // initialized.
        if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
          visitPatternBindingDecl(patternBinding);
          continue;
        }

        // Ignore variable declarations, because they're always handled within
        // their enclosing pattern bindings.
        if (isa<VarDecl>(decl))
          continue;

        if (!unhandledNode)
          unhandledNode = decl;

        continue;
      }

      auto expr = node.get<Expr *>();
      if (cs && builderSupports(ctx.Id_buildExpression)) {
        expr = buildCallIfWanted(expr->getLoc(), ctx.Id_buildExpression,
                                 { expr }, { Identifier() });
      }

      addChild(captureExpr(expr, /*oneWay=*/true, node.get<Expr *>()));
    }

    if (!cs || hadError)
      return nullptr;

    // Call Builder.buildBlock(... args ...)
    auto call = buildCallIfWanted(braceStmt->getStartLoc(),
                                  ctx.Id_buildBlock, expressions,
                                  /*argLabels=*/{ });
    if (!call)
      return nullptr;

    return captureExpr(call, /*oneWay=*/true, braceStmt);
  }

  VarDecl *visitReturnStmt(ReturnStmt *stmt) {
    if (!unhandledNode)
      unhandledNode = stmt;
    return nullptr;
  }

  VarDecl *visitDoStmt(DoStmt *doStmt) {
    if (!builderSupports(ctx.Id_buildDo)) {
      if (!unhandledNode)
        unhandledNode = doStmt;
      return nullptr;
    }

    auto childVar = visit(doStmt->getBody());
    if (!childVar)
      return nullptr;

    auto childRef = buildVarRef(childVar, doStmt->getEndLoc());
    auto call = buildCallIfWanted(doStmt->getStartLoc(), ctx.Id_buildDo,
                                  childRef, /*argLabels=*/{ });
    if (!call)
      return nullptr;

    return captureExpr(call, /*oneWay=*/true, doStmt);
  }

  CONTROL_FLOW_STMT(Yield)
  CONTROL_FLOW_STMT(Defer)

  static bool isBuildableIfChainRecursive(IfStmt *ifStmt,
                                          unsigned &numPayloads,
                                          bool &isOptional) {
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

    // If there's a missing 'else', we need 'buildOptional' to exist.
    if (isOptional && !builderSupports(buildOptionalId))
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

  VarDecl *visitIfStmt(IfStmt *ifStmt) {
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
    return buildIfChainRecursive(ifStmt, 0, numPayloads, isOptional,
                                 /*isTopLevel=*/true);
  }

  /// Recursively build an if-chain: build an expression which will have
  /// a value of the chain result type before any call to `buildIf`.
  /// The expression will perform any necessary calls to `buildEither`,
  /// and the result will have optional type if `isOptional` is true.
  VarDecl *buildIfChainRecursive(IfStmt *ifStmt, unsigned payloadIndex,
                                 unsigned numPayloads, bool isOptional,
                                 bool isTopLevel = false) {
    assert(payloadIndex < numPayloads);

    // First generate constraints for the conditions. This can introduce
    // variable bindings that will be used within the "then" branch.
    if (cs && cs->generateConstraints(ifStmt->getCond(), dc)) {
      hadError = true;
      return nullptr;
    }

    // Make sure we recursively visit both sides even if we're not
    // building expressions.

    // Build the then clause.  This will have the corresponding payload
    // type (i.e. not wrapped in any way).
    VarDecl *thenVar = visit(ifStmt->getThenStmt());

    // Build the else clause, if present.  If this is from an else-if,
    // this will be fully wrapped; otherwise it will have the corresponding
    // payload type (at index `payloadIndex + 1`).
    assert(ifStmt->getElseStmt() || isOptional);
    bool isElseIf = false;
    Optional<VarDecl *> elseChainVar;
    if (auto elseStmt = ifStmt->getElseStmt()) {
      if (auto elseIfStmt = dyn_cast<IfStmt>(elseStmt)) {
        isElseIf = true;
        elseChainVar = buildIfChainRecursive(elseIfStmt, payloadIndex + 1,
                                             numPayloads, isOptional);
      } else {
        elseChainVar = visit(elseStmt);
      }
    }

    // Short-circuit if appropriate.
    if (!cs || !thenVar || (elseChainVar && !*elseChainVar))
      return nullptr;

    // Prepare the `then` operand by wrapping it to produce a chain result.
    Expr *thenExpr = buildWrappedChainPayload(
        buildVarRef(thenVar, ifStmt->getThenStmt()->getEndLoc()),
        payloadIndex, numPayloads, isOptional);

    // Prepare the `else operand:
    Expr *elseExpr;
    SourceLoc elseLoc;

    // - If there's no `else` clause, use `Optional.none`.
    if (!elseChainVar) {
      assert(isOptional);
      elseLoc = ifStmt->getEndLoc();
      elseExpr = buildNoneExpr(elseLoc);

    // - If there's an `else if`, the chain expression from that
    //   should already be producing a chain result.
    } else if (isElseIf) {
      elseExpr = buildVarRef(*elseChainVar, ifStmt->getEndLoc());
      elseLoc = ifStmt->getElseLoc();

    // - Otherwise, wrap it to produce a chain result.
    } else {
      elseLoc = ifStmt->getElseLoc();
      elseExpr = buildWrappedChainPayload(
          buildVarRef(*elseChainVar, ifStmt->getEndLoc()),
          payloadIndex + 1, numPayloads, isOptional);
    }

    // The operand should have optional type if we had optional results,
    // so we just need to call `buildIf` now, since we're at the top level.
    if (isOptional && isTopLevel) {
      thenExpr = buildCallIfWanted(ifStmt->getEndLoc(), buildOptionalId,
                                   thenExpr,  /*argLabels=*/{ });
      elseExpr = buildCallIfWanted(ifStmt->getEndLoc(), buildOptionalId,
                                   elseExpr,  /*argLabels=*/{ });
    }

    thenExpr = cs->generateConstraints(thenExpr, dc);
    if (!thenExpr) {
      hadError = true;
      return nullptr;
    }

    elseExpr = cs->generateConstraints(elseExpr, dc);
    if (!elseExpr) {
      hadError = true;
      return nullptr;
    }

    // FIXME: Need a locator for the "if" statement.
    Type resultType = cs->addJoinConstraint(nullptr,
        {
          { cs->getType(thenExpr), cs->getConstraintLocator(thenExpr) },
          { cs->getType(elseExpr), cs->getConstraintLocator(elseExpr) }
        });
    if (!resultType) {
      hadError = true;
      return nullptr;
    }

    // Create a variable to capture the result of this expression.
    auto ifVar = buildVar(ifStmt->getStartLoc());
    cs->setType(ifVar, resultType);
    applied.capturedStmts.insert({ifStmt, { ifVar, { thenExpr, elseExpr }}});
    return ifVar;
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
                                  {isSecond ? ctx.Id_second : ctx.Id_first});
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
        optionalTypeExpr, loc, DeclNameRef(ctx.getIdentifier("some")),
        DeclNameLoc(loc), /*implicit=*/true);
    return CallExpr::createImplicit(ctx, someRef, arg, { });
  }

  Expr *buildNoneExpr(SourceLoc endLoc) {
    auto optionalDecl = ctx.getOptionalDecl();
    auto optionalType = optionalDecl->getDeclaredType();

    auto optionalTypeExpr =
      TypeExpr::createImplicitHack(endLoc, optionalType, ctx);
    return new (ctx) UnresolvedDotExpr(
        optionalTypeExpr, endLoc, DeclNameRef(ctx.getIdentifier("none")),
        DeclNameLoc(endLoc), /*implicit=*/true);
  }

  VarDecl *visitSwitchStmt(SwitchStmt *switchStmt) {
    // Generate constraints for the subject expression, and capture its
    // type for use in matching the various patterns.
    Expr *subjectExpr = switchStmt->getSubjectExpr();
    if (cs) {
      // Form a one-way constraint to prevent backward propagation.
      subjectExpr = new (ctx) OneWayExpr(subjectExpr);

      // FIXME: Add contextual type purpose for switch subjects?
      SolutionApplicationTarget target(subjectExpr, dc, CTP_Unused, Type(),
                                       /*isDiscarded=*/false);
      if (cs->generateConstraints(target, FreeTypeVariableBinding::Disallow)) {
        hadError = true;
        return nullptr;
      }

      cs->setSolutionApplicationTarget(switchStmt, target);
      subjectExpr = target.getAsExpr();
      assert(subjectExpr && "Must have a subject expression here");
    }

    // Generate constraints and capture variables for all of the cases.
    SmallVector<std::pair<CaseStmt *, VarDecl *>, 4> capturedCaseVars;
    for (auto *caseStmt : switchStmt->getCases()) {
      if (auto capturedCaseVar = visitCaseStmt(caseStmt, subjectExpr)) {
        capturedCaseVars.push_back({caseStmt, capturedCaseVar});
      }
    }

    if (!cs)
      return nullptr;

    // Form the expressions that inject the result of each case into the
    // appropriate
    llvm::TinyPtrVector<Expr *> injectedCaseExprs;
    SmallVector<std::pair<Type, ConstraintLocator *>, 4> injectedCaseTerms;
    for (unsigned idx : indices(capturedCaseVars)) {
      auto caseStmt = capturedCaseVars[idx].first;
      auto caseVar = capturedCaseVars[idx].second;

      // Build the expression that injects the case variable into appropriate
      // buildEither(first:)/buildEither(second:) chain.
      Expr *caseVarRef = buildVarRef(caseVar, caseStmt->getEndLoc());
      Expr *injectedCaseExpr = buildWrappedChainPayload(
          caseVarRef, idx, capturedCaseVars.size(), /*isOptional=*/false);

      // Generate constraints for this injected case result.
      injectedCaseExpr = cs->generateConstraints(injectedCaseExpr, dc);
      if (!injectedCaseExpr) {
        hadError = true;
        return nullptr;
      }

      // Record this injected case expression.
      injectedCaseExprs.push_back(injectedCaseExpr);

      // Record the type and locator for this injected case expression, to be
      // used in the "join" constraint later.
      injectedCaseTerms.push_back(
        { cs->getType(injectedCaseExpr)->getRValueType(),
          cs->getConstraintLocator(injectedCaseExpr) });
    }

    // Form the type of the switch itself.
    // FIXME: Need a locator for the "switch" statement.
    Type resultType = cs->addJoinConstraint(nullptr, injectedCaseTerms);
    if (!resultType) {
      hadError = true;
      return nullptr;
    }

    // Create a variable to capture the result of evaluating the switch.
    auto switchVar = buildVar(switchStmt->getStartLoc());
    cs->setType(switchVar, resultType);
    applied.capturedStmts.insert(
        {switchStmt, { switchVar, std::move(injectedCaseExprs) } });
    return switchVar;
  }

  VarDecl *visitCaseStmt(CaseStmt *caseStmt, Expr *subjectExpr) {
    // If needed, generate constraints for everything in the case statement.
    if (cs) {
      auto locator = cs->getConstraintLocator(
          subjectExpr, LocatorPathElt::ContextualType());
      Type subjectType = cs->getType(subjectExpr);

      if (cs->generateConstraints(caseStmt, dc, subjectType, locator)) {
        hadError = true;
        return nullptr;
      }
    }

    // Translate the body.
    return visit(caseStmt->getBody());
  }

  CONTROL_FLOW_STMT(Guard)
  CONTROL_FLOW_STMT(While)
  CONTROL_FLOW_STMT(DoCatch)
  CONTROL_FLOW_STMT(RepeatWhile)
  CONTROL_FLOW_STMT(ForEach)
  CONTROL_FLOW_STMT(Case)
  CONTROL_FLOW_STMT(Break)
  CONTROL_FLOW_STMT(Continue)
  CONTROL_FLOW_STMT(Fallthrough)
  CONTROL_FLOW_STMT(Fail)
  CONTROL_FLOW_STMT(Throw)
  CONTROL_FLOW_STMT(PoundAssert)

#undef CONTROL_FLOW_STMT
};

/// Describes the target into which the result of a particular statement in
/// a closure involving a function builder should be written.
struct FunctionBuilderTarget {
  enum Kind {
    /// The resulting value is returned from the closure.
    ReturnValue,
    /// The temporary variable into which the result should be assigned.
    TemporaryVar,
  } kind;

  /// Captured variable information.
  std::pair<VarDecl *, llvm::TinyPtrVector<Expr *>> captured;

  static FunctionBuilderTarget forReturn(Expr *expr) {
    return FunctionBuilderTarget{ReturnValue, {nullptr, {expr}}};
  }

  static FunctionBuilderTarget forAssign(VarDecl *temporaryVar,
                                         llvm::TinyPtrVector<Expr *> exprs) {
    return FunctionBuilderTarget{TemporaryVar, {temporaryVar, exprs}};
  }
};

/// Handles the rewrite of the body of a closure to which a function builder
/// has been applied.
class BuilderClosureRewriter
    : public StmtVisitor<BuilderClosureRewriter, Stmt *, FunctionBuilderTarget> {
  ASTContext &ctx;
  const Solution &solution;
  DeclContext *dc;
  AppliedBuilderTransform builderTransform;
  std::function<
      Optional<SolutionApplicationTarget> (SolutionApplicationTarget)>
        rewriteTarget;

  /// Retrieve the temporary variable that will be used to capture the
  /// value of the given expression.
  AppliedBuilderTransform::RecordedExpr takeCapturedExpr(Expr *expr) {
    auto found = builderTransform.capturedExprs.find(expr);
    assert(found != builderTransform.capturedExprs.end());

    // Set the type of the temporary variable.
    auto recorded = found->second;
    if (auto temporaryVar = recorded.temporaryVar) {
      Type type = solution.simplifyType(solution.getType(temporaryVar));
      temporaryVar->setInterfaceType(type->mapTypeOutOfContext());
    }

    // Erase the captured expression, so we're sure we never do this twice.
    builderTransform.capturedExprs.erase(found);
    return recorded;
  }

  /// Rewrite an expression without any particularly special context.
  Expr *rewriteExpr(Expr *expr) {
    auto result = rewriteTarget(
      SolutionApplicationTarget(expr, dc, CTP_Unused, Type(),
                                /*isDiscarded=*/false));
    if (result)
      return result->getAsExpr();

    return nullptr;
  }

public:
  /// Retrieve information about a captured statement.
  std::pair<VarDecl *, llvm::TinyPtrVector<Expr *>>
  takeCapturedStmt(Stmt *stmt) {
    auto found = builderTransform.capturedStmts.find(stmt);
    assert(found != builderTransform.capturedStmts.end());

    // Set the type of the temporary variable.
    auto temporaryVar = found->second.first;
    Type type = solution.simplifyType(solution.getType(temporaryVar));
    temporaryVar->setInterfaceType(type->mapTypeOutOfContext());

    // Take the expressions.
    auto exprs = std::move(found->second.second);

    // Erase the statement, so we're sure we never do this twice.
    builderTransform.capturedStmts.erase(found);
    return std::make_pair(temporaryVar, std::move(exprs));
  }

private:
  /// Build the statement or expression to initialize the target.
  ASTNode initializeTarget(FunctionBuilderTarget target) {
    assert(target.captured.second.size() == 1);
    auto capturedExpr = target.captured.second.front();
    SourceLoc implicitLoc = capturedExpr->getEndLoc();
    switch (target.kind) {
    case FunctionBuilderTarget::ReturnValue: {
      // Return the expression.
      Type bodyResultType =
          solution.simplifyType(builderTransform.bodyResultType);

      SolutionApplicationTarget returnTarget(
          capturedExpr, dc, CTP_ReturnStmt, bodyResultType,
          /*isDiscarded=*/false);
      Expr *resultExpr = nullptr;
      if (auto resultTarget = rewriteTarget(returnTarget))
        resultExpr = resultTarget->getAsExpr();

      return new (ctx) ReturnStmt(implicitLoc, resultExpr);
    }

    case FunctionBuilderTarget::TemporaryVar: {
      // Assign the expression into a variable.
      auto temporaryVar = target.captured.first;
      auto declRef = new (ctx) DeclRefExpr(
          temporaryVar, DeclNameLoc(implicitLoc), /*implicit=*/true);
      declRef->setType(LValueType::get(temporaryVar->getType()));

      // Load the right-hand side if needed.
      auto finalCapturedExpr = rewriteExpr(capturedExpr);
      if (finalCapturedExpr->getType()->hasLValueType()) {
        finalCapturedExpr =
            TypeChecker::addImplicitLoadExpr(ctx, finalCapturedExpr);
      }

      auto assign = new (ctx) AssignExpr(
          declRef, implicitLoc, finalCapturedExpr, /*implicit=*/true);
      assign->setType(TupleType::getEmpty(ctx));
      return assign;
    }
    }
  }

  /// Declare the given temporary variable, adding the appropriate
  /// entries to the elements of a brace stmt.
  void declareTemporaryVariable(VarDecl *temporaryVar,
                                std::vector<ASTNode> &elements,
                                Expr *initExpr = nullptr) {
    if (!temporaryVar)
      return;

    // Form a new pattern binding to bind the temporary variable to the
    // transformed expression.
    auto pattern = new (ctx) NamedPattern(temporaryVar,/*implicit=*/true);
    pattern->setType(temporaryVar->getType());

    auto pbd = PatternBindingDecl::create(
        ctx, SourceLoc(), StaticSpellingKind::None, temporaryVar->getLoc(),
        pattern, SourceLoc(), initExpr, dc);
    elements.push_back(temporaryVar);
    elements.push_back(pbd);
  }

  /// Produce a final type-checked pattern binding.
  void finishPatternBindingDecl(PatternBindingDecl *patternBinding) {
    for (unsigned index : range(patternBinding->getNumPatternEntries())) {
      // Find the solution application target for this.
      auto knownTarget =
          builderTransform.patternBindingEntries.find({patternBinding, index});
      assert(knownTarget != builderTransform.patternBindingEntries.end());

      // Rewrite the target.
      auto resultTarget = rewriteTarget(knownTarget->second);
      if (!resultTarget)
        continue;

      patternBinding->setPattern(
          index, resultTarget->getInitializationPattern(),
          resultTarget->getDeclContext());
      patternBinding->setInit(index, resultTarget->getAsExpr());
    }
  }

public:
  BuilderClosureRewriter(
      const Solution &solution,
      DeclContext *dc,
      const AppliedBuilderTransform &builderTransform,
      std::function<
          Optional<SolutionApplicationTarget> (SolutionApplicationTarget)>
            rewriteTarget
    ) : ctx(solution.getConstraintSystem().getASTContext()),
        solution(solution), dc(dc), builderTransform(builderTransform),
        rewriteTarget(rewriteTarget) { }

  Stmt *visitBraceStmt(BraceStmt *braceStmt, FunctionBuilderTarget target,
                       Optional<FunctionBuilderTarget> innerTarget = None) {
    std::vector<ASTNode> newElements;

    // If there is an "inner" target corresponding to this brace, declare
    // it's temporary variable if needed.
    if (innerTarget) {
      declareTemporaryVariable(innerTarget->captured.first, newElements);
    }

    for (auto node : braceStmt->getElements()) {
      // Implicit returns in single-expression function bodies are treated
      // as the expression.
      if (auto returnStmt =
              dyn_cast_or_null<ReturnStmt>(node.dyn_cast<Stmt *>())) {
        assert(returnStmt->isImplicit());
        node = returnStmt->getResult();
      }

      if (auto expr = node.dyn_cast<Expr *>()) {
        // Skip error expressions.
        if (isa<ErrorExpr>(expr))
          continue;

        // Each expression turns into a 'let' that captures the value of
        // the expression.
        auto recorded = takeCapturedExpr(expr);

        // Rewrite the expression
        Expr *finalExpr = rewriteExpr(recorded.generatedExpr);

        // Form a new pattern binding to bind the temporary variable to the
        // transformed expression.
        declareTemporaryVariable(recorded.temporaryVar, newElements, finalExpr);
        continue;
      }

      if (auto stmt = node.dyn_cast<Stmt *>()) {
        // Each statement turns into a (potential) temporary variable
        // binding followed by the statement itself.
        auto captured = takeCapturedStmt(stmt);

        declareTemporaryVariable(captured.first, newElements);

        Stmt *finalStmt = visit(
            stmt,
            FunctionBuilderTarget{FunctionBuilderTarget::TemporaryVar,
                                  std::move(captured)});
        newElements.push_back(finalStmt);
        continue;
      }

      auto decl = node.get<Decl *>();

      // Skip #if declarations.
      if (isa<IfConfigDecl>(decl)) {
        newElements.push_back(decl);
        continue;
      }

      // Diagnose #warning / #error during application.
      if (auto poundDiag = dyn_cast<PoundDiagnosticDecl>(decl)) {
        TypeChecker::typeCheckDecl(poundDiag);
        newElements.push_back(decl);
        continue;
      }

      // Skip variable declarations; they're always part of a pattern
      // binding.
      if (isa<VarDecl>(decl)) {
        newElements.push_back(decl);
        continue;
      }

      // Handle pattern bindings.
      if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
        finishPatternBindingDecl(patternBinding);
        newElements.push_back(decl);
        continue;
      }

      llvm_unreachable("Cannot yet handle declarations");
    }

    // If there is an "inner" target corresponding to this brace, initialize
    // it.
    if (innerTarget) {
      newElements.push_back(initializeTarget(*innerTarget));
    }

    // Capture the result of the buildBlock() call in the manner requested
    // by the caller.
    newElements.push_back(initializeTarget(target));

    return BraceStmt::create(ctx, braceStmt->getLBraceLoc(), newElements,
                             braceStmt->getRBraceLoc());
  }

  Stmt *visitIfStmt(IfStmt *ifStmt, FunctionBuilderTarget target) {
    // Rewrite the condition.
    if (auto condition = rewriteTarget(
            SolutionApplicationTarget(ifStmt->getCond(), dc)))
      ifStmt->setCond(*condition->getAsStmtCondition());

    assert(target.kind == FunctionBuilderTarget::TemporaryVar);
    auto temporaryVar = target.captured.first;

    // Translate the "then" branch.
    auto capturedThen = takeCapturedStmt(ifStmt->getThenStmt());
    auto newThen = visitBraceStmt(cast<BraceStmt>(ifStmt->getThenStmt()),
          FunctionBuilderTarget::forAssign(
            temporaryVar, {target.captured.second[0]}),
          FunctionBuilderTarget::forAssign(
            capturedThen.first, {capturedThen.second.front()}));
    ifStmt->setThenStmt(newThen);

    if (auto elseBraceStmt =
            dyn_cast_or_null<BraceStmt>(ifStmt->getElseStmt())) {
      // Translate the "else" branch when it's a stmt-brace.
      auto capturedElse = takeCapturedStmt(elseBraceStmt);
      Stmt *newElse = visitBraceStmt(
          elseBraceStmt,
          FunctionBuilderTarget::forAssign(
            temporaryVar, {target.captured.second[1]}),
          FunctionBuilderTarget::forAssign(
            capturedElse.first, {capturedElse.second.front()}));
      ifStmt->setElseStmt(newElse);
    } else if (auto elseIfStmt = cast_or_null<IfStmt>(ifStmt->getElseStmt())){
      // Translate the "else" branch when it's an else-if.
      auto capturedElse = takeCapturedStmt(elseIfStmt);
      std::vector<ASTNode> newElseElements;
      declareTemporaryVariable(capturedElse.first, newElseElements);
      newElseElements.push_back(
          visitIfStmt(
            elseIfStmt,
            FunctionBuilderTarget::forAssign(
              capturedElse.first, capturedElse.second)));
      newElseElements.push_back(
          initializeTarget(
            FunctionBuilderTarget::forAssign(
              temporaryVar, {target.captured.second[1]})));

      Stmt *newElse = BraceStmt::create(
          ctx, elseIfStmt->getStartLoc(), newElseElements,
          elseIfStmt->getEndLoc());
      ifStmt->setElseStmt(newElse);
    } else {
      // Form an "else" brace containing an assignment to the temporary
      // variable.
      auto init = initializeTarget(
          FunctionBuilderTarget::forAssign(
            temporaryVar, {target.captured.second[1]}));
      auto newElse = BraceStmt::create(
          ctx, ifStmt->getEndLoc(), { init }, ifStmt->getEndLoc());
      ifStmt->setElseStmt(newElse);
    }

    return ifStmt;
  }

  Stmt *visitDoStmt(DoStmt *doStmt, FunctionBuilderTarget target) {
    // Each statement turns into a (potential) temporary variable
    // binding followed by the statement itself.
    auto body = cast<BraceStmt>(doStmt->getBody());
    auto captured = takeCapturedStmt(body);

    auto newInnerBody = cast<BraceStmt>(
        visitBraceStmt(
          body,
          target,
          FunctionBuilderTarget::forAssign(
            captured.first, {captured.second.front()})));
    doStmt->setBody(newInnerBody);
    return doStmt;
  }

  Stmt *visitSwitchStmt(SwitchStmt *switchStmt, FunctionBuilderTarget target) {
    // Translate the subject expression.
    ConstraintSystem &cs = solution.getConstraintSystem();
    auto subjectTarget =
        rewriteTarget(*cs.getSolutionApplicationTarget(switchStmt));
    if (!subjectTarget)
      return nullptr;

    switchStmt->setSubjectExpr(subjectTarget->getAsExpr());

    // Handle any declaration nodes within the case list first; we'll
    // handle the cases in a second pass.
    for (auto child : switchStmt->getRawCases()) {
      if (auto decl = child.dyn_cast<Decl *>()) {
        TypeChecker::typeCheckDecl(decl);
      }
    }

    // Translate all of the cases.
    bool limitExhaustivityChecks = false;
    assert(target.kind == FunctionBuilderTarget::TemporaryVar);
    auto temporaryVar = target.captured.first;
    unsigned caseIndex = 0;
    for (auto caseStmt : switchStmt->getCases()) {
      if (!visitCaseStmt(
            caseStmt,
            FunctionBuilderTarget::forAssign(
              temporaryVar, {target.captured.second[caseIndex]})))
        return nullptr;

      // Check restrictions on '@unknown'.
      if (caseStmt->hasUnknownAttr()) {
        checkUnknownAttrRestrictions(
            cs.getASTContext(), caseStmt, /*fallthroughDest=*/nullptr,
            limitExhaustivityChecks);
      }

      ++caseIndex;
    }

    TypeChecker::checkSwitchExhaustiveness(
        switchStmt, dc, limitExhaustivityChecks);

    return switchStmt;
  }

  Stmt *visitCaseStmt(CaseStmt *caseStmt, FunctionBuilderTarget target) {
    // Translate the patterns and guard expressions for each case label item.
    for (auto &caseLabelItem : caseStmt->getMutableCaseLabelItems()) {
      SolutionApplicationTarget caseLabelTarget(&caseLabelItem, dc);
      if (!rewriteTarget(caseLabelTarget))
        return nullptr;
    }

    // Transform the body of the case.
    auto body = cast<BraceStmt>(caseStmt->getBody());
    auto captured = takeCapturedStmt(body);
    auto newInnerBody = cast<BraceStmt>(
        visitBraceStmt(
          body,
          target,
          FunctionBuilderTarget::forAssign(
            captured.first, {captured.second.front()})));
    caseStmt->setBody(newInnerBody);

    return caseStmt;
  }

#define UNHANDLED_FUNCTION_BUILDER_STMT(STMT) \
  Stmt *visit##STMT##Stmt(STMT##Stmt *stmt, FunctionBuilderTarget target) { \
    llvm_unreachable("Function builders do not allow statement of kind " \
                     #STMT); \
  }

  UNHANDLED_FUNCTION_BUILDER_STMT(Return)
  UNHANDLED_FUNCTION_BUILDER_STMT(Yield)
  UNHANDLED_FUNCTION_BUILDER_STMT(Guard)
  UNHANDLED_FUNCTION_BUILDER_STMT(While)
  UNHANDLED_FUNCTION_BUILDER_STMT(Defer)
  UNHANDLED_FUNCTION_BUILDER_STMT(DoCatch)
  UNHANDLED_FUNCTION_BUILDER_STMT(RepeatWhile)
  UNHANDLED_FUNCTION_BUILDER_STMT(ForEach)
  UNHANDLED_FUNCTION_BUILDER_STMT(Break)
  UNHANDLED_FUNCTION_BUILDER_STMT(Continue)
  UNHANDLED_FUNCTION_BUILDER_STMT(Fallthrough)
  UNHANDLED_FUNCTION_BUILDER_STMT(Fail)
  UNHANDLED_FUNCTION_BUILDER_STMT(Throw)
  UNHANDLED_FUNCTION_BUILDER_STMT(PoundAssert)
#undef UNHANDLED_FUNCTION_BUILDER_STMT
};

} // end anonymous namespace

BraceStmt *swift::applyFunctionBuilderTransform(
    const Solution &solution,
    AppliedBuilderTransform applied,
    BraceStmt *body,
    DeclContext *dc,
    std::function<
        Optional<SolutionApplicationTarget> (SolutionApplicationTarget)>
          rewriteTarget) {
  BuilderClosureRewriter rewriter(solution, dc, applied, rewriteTarget);
  auto captured = rewriter.takeCapturedStmt(body);
  return cast<BraceStmt>(
    rewriter.visitBraceStmt(
      body,
      FunctionBuilderTarget::forReturn(applied.returnExpr),
      FunctionBuilderTarget::forAssign(
        captured.first, captured.second)));
}

/// Produce any additional syntactic diagnostics for the body of a
static void performAddOnDiagnostics(BraceStmt *stmt, DeclContext *dc) {
  class AddOnDiagnosticWalker : public ASTWalker {
    SmallVector<DeclContext *, 4> dcStack;

  public:
    AddOnDiagnosticWalker(DeclContext *dc) {
      dcStack.push_back(dc);
    }

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      performSyntacticExprDiagnostics(
          expr, dcStack.back(), /*isExprStmt=*/false);

      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (!closure->hasSingleExpressionBody() &&
            !closure->hasAppliedFunctionBuilder()) {
          dcStack.push_back(closure);
          return { true, expr };
        }
      }

      return { false, expr };
    }

    Expr *walkToExprPost(Expr *expr) override {
      if (auto closure = dyn_cast<ClosureExpr>(expr)) {
        if (!closure->hasSingleExpressionBody() &&
            !closure->hasAppliedFunctionBuilder()) {
          assert(dcStack.back() == closure);
          dcStack.pop_back();
        }
      }

      return expr;
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
      performStmtDiagnostics(dcStack.back()->getASTContext(), stmt);
      return { true, stmt };
    }

    std::pair<bool, Pattern*> walkToPatternPre(Pattern *pattern) override {
      return { false, pattern };
    }

    bool walkToTypeLocPre(TypeLoc &typeLoc) override { return false; }

    bool walkToTypeReprPre(TypeRepr *typeRepr) override { return false; }

    bool walkToParameterListPre(ParameterList *params) override {
      return false;
    }
  };

  AddOnDiagnosticWalker walker(dc);
  stmt->walk(walker);
}

Optional<BraceStmt *> TypeChecker::applyFunctionBuilderBodyTransform(
    FuncDecl *func, Type builderType) {
  // Pre-check the body: pre-check any expressions in it and look
  // for return statements.
  //
  // If we encountered an error or there was an explicit result type,
  // bail out and report that to the caller.
  auto &ctx = func->getASTContext();
  auto request = PreCheckFunctionBuilderRequest{func, func->getBody()};
  switch (evaluateOrDefault(
              ctx.evaluator, request, FunctionBuilderBodyPreCheck::Error)) {
  case FunctionBuilderBodyPreCheck::Okay:
    // If the pre-check was okay, apply the function-builder transform.
    break;

  case FunctionBuilderBodyPreCheck::Error:
    return nullptr;

  case FunctionBuilderBodyPreCheck::HasReturnStmt: {
    // One or more explicit 'return' statements were encountered, which
    // disables the function builder transform. Warn when we do this.
    auto returnStmts = findReturnStatements(func);
    assert(!returnStmts.empty());

    ctx.Diags.diagnose(
        returnStmts.front()->getReturnLoc(),
        diag::function_builder_disabled_by_return, builderType);

    // Note that one can remove the function builder attribute.
    auto attr = func->getAttachedFunctionBuilder();
    if (!attr) {
      if (auto accessor = dyn_cast<AccessorDecl>(func)) {
        attr = accessor->getStorage()->getAttachedFunctionBuilder();
      }
    }

    if (attr) {
      ctx.Diags.diagnose(
          attr->getLocation(), diag::function_builder_remove_attr)
        .fixItRemove(attr->getRangeWithAt());
      attr->setInvalid();
    }

    // Note that one can remove all of the return statements.
    {
      auto diag = ctx.Diags.diagnose(
          returnStmts.front()->getReturnLoc(),
          diag::function_builder_remove_returns);
      for (auto returnStmt : returnStmts) {
        diag.fixItRemove(returnStmt->getReturnLoc());
      }
    }

    return None;
  }
  }

  ConstraintSystemOptions options = ConstraintSystemFlags::AllowFixes;
  auto resultInterfaceTy = func->getResultInterfaceType();
  auto resultContextType = func->mapTypeIntoContext(resultInterfaceTy);

  // Determine whether we're inferring the underlying type for the opaque
  // result type of this function.
  ConstraintKind resultConstraintKind = ConstraintKind::Conversion;
  if (auto opaque = resultContextType->getAs<OpaqueTypeArchetypeType>()) {
    if (opaque->getDecl()->isOpaqueReturnTypeOfFunction(func)) {
      resultConstraintKind = ConstraintKind::OpaqueUnderlyingType;
    }
  }

  // Build a constraint system in which we can check the body of the function.
  ConstraintSystem cs(func, options);

  // Find an expression... any expression... to use for a locator.
  // FIXME: This is a hack because we don't have the notion of locators that
  // refer to statements.
  Expr *fakeAnchor = nullptr;
  {
    class FindExprWalker : public ASTWalker {
      Expr *&fakeAnchor;

    public:
      explicit FindExprWalker(Expr *&fakeAnchor) : fakeAnchor(fakeAnchor) { }

      std::pair<bool, Expr *> walkToExprPre(Expr *E) {
        if (!fakeAnchor)
          fakeAnchor = E;

        return { false, nullptr };
      }
    } walker(fakeAnchor);

    func->getBody()->walk(walker);
  }

  if (auto result = cs.matchFunctionBuilder(
          func, builderType, resultContextType, resultConstraintKind,
          /*calleeLocator=*/cs.getConstraintLocator(fakeAnchor),
          /*FIXME:*/cs.getConstraintLocator(fakeAnchor))) {
    if (result->isFailure())
      return nullptr;
  }

  // Solve the constraint system.
  SmallVector<Solution, 4> solutions;
  if (cs.solve(solutions) || solutions.size() != 1) {
    // Try to fix the system or provide a decent diagnostic.
    auto salvagedResult = cs.salvage();
    switch (salvagedResult.getKind()) {
    case SolutionResult::Kind::Success:
      solutions.clear();
      solutions.push_back(std::move(salvagedResult).takeSolution());
      break;

    case SolutionResult::Kind::Error:
    case SolutionResult::Kind::Ambiguous:
      return nullptr;

    case SolutionResult::Kind::UndiagnosedError:
      cs.diagnoseFailureFor(SolutionApplicationTarget(func));
      salvagedResult.markAsDiagnosed();
      return nullptr;

    case SolutionResult::Kind::TooComplex:
      func->diagnose(diag::expression_too_complex)
        .highlight(func->getBodySourceRange());
      salvagedResult.markAsDiagnosed();
      return nullptr;
    }

    // The system was salvaged; continue on as if nothing happened.
  }

  // FIXME: Shouldn't need to do this.
  cs.applySolution(solutions.front());

  // Apply the solution to the function body.
  if (auto result = cs.applySolution(
          solutions.front(),
          SolutionApplicationTarget(func))) {
    performAddOnDiagnostics(result->getFunctionBody(), func);
    return result->getFunctionBody();
  }

  return nullptr;
}

Optional<ConstraintSystem::TypeMatchResult>
ConstraintSystem::matchFunctionBuilder(
    AnyFunctionRef fn, Type builderType, Type bodyResultType,
    ConstraintKind bodyResultConstraintKind,
    ConstraintLocator *calleeLocator, ConstraintLocatorBuilder locator) {
  auto builder = builderType->getAnyNominal();
  assert(builder && "Bad function builder type");
  assert(builder->getAttrs().hasAttribute<FunctionBuilderAttr>());

  // Pre-check the body: pre-check any expressions in it and look
  // for return statements.
  auto request = PreCheckFunctionBuilderRequest{fn, fn.getBody()};
  switch (evaluateOrDefault(getASTContext().evaluator, request,
                            FunctionBuilderBodyPreCheck::Error)) {
  case FunctionBuilderBodyPreCheck::Okay:
    // If the pre-check was okay, apply the function-builder transform.
    break;

  case FunctionBuilderBodyPreCheck::Error:
    // If the pre-check had an error, flag that.
    return getTypeMatchFailure(locator);

  case FunctionBuilderBodyPreCheck::HasReturnStmt:
    // If the body has a return statement, suppress the transform but
    // continue solving the constraint system.
    return None;
  }

  // Check the form of this body to see if we can apply the
  // function-builder translation at all.
  auto dc = fn.getAsDeclContext();
  {
    // Check whether we can apply this specific function builder.
    BuilderClosureVisitor visitor(getASTContext(), nullptr, dc, builderType,
                                  bodyResultType);

    // If we saw a control-flow statement or declaration that the builder
    // cannot handle, we don't have a well-formed function builder application.
    if (auto unhandledNode = visitor.check(fn.getBody())) {
      // If we aren't supposed to attempt fixes, fail.
      if (!shouldAttemptFixes()) {
        return getTypeMatchFailure(locator);
      }

      // Record the first unhandled construct as a fix.
      if (recordFix(
              SkipUnhandledConstructInFunctionBuilder::create(
                *this, unhandledNode, builder,
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

  BuilderClosureVisitor visitor(getASTContext(), this, dc, builderType,
                                bodyResultType);

  auto applied = visitor.apply(fn.getBody());
  if (!applied)
    return getTypeMatchFailure(locator);

  Type transformedType = getType(applied->returnExpr);
  assert(transformedType && "Missing type");

  // Record the transformation.
  assert(std::find_if(
      functionBuilderTransformed.begin(),
      functionBuilderTransformed.end(),
      [&](const std::pair<AnyFunctionRef, AppliedBuilderTransform> &elt) {
        return elt.first == fn;
      }) == functionBuilderTransformed.end() &&
         "already transformed this body along this path!?!");
  functionBuilderTransformed.push_back(
      std::make_pair(fn, std::move(*applied)));

  // If builder is applied to the closure expression then
  // `closure body` to `closure result` matching should
  // use special locator.
  if (auto *closure = fn.getAbstractClosureExpr())
    locator = getConstraintLocator(closure, ConstraintLocator::ClosureResult);

  // Bind the body result type to the type of the transformed expression.
  addConstraint(bodyResultConstraintKind, transformedType, bodyResultType,
                locator);
  return getTypeMatchSuccess();
}

namespace {

/// Pre-check all the expressions in the body.
class PreCheckFunctionBuilderApplication : public ASTWalker {
  AnyFunctionRef Fn;
  bool SkipPrecheck = false;
  std::vector<ReturnStmt *> ReturnStmts;
  bool HasError = false;

  bool hasReturnStmt() const { return !ReturnStmts.empty(); }

public:
  PreCheckFunctionBuilderApplication(AnyFunctionRef fn, bool skipPrecheck)
    : Fn(fn), SkipPrecheck(skipPrecheck) {}

  const std::vector<ReturnStmt *> getReturnStmts() const { return ReturnStmts; }

  FunctionBuilderBodyPreCheck run() {
    Stmt *oldBody = Fn.getBody();

    Stmt *newBody = oldBody->walk(*this);

    // If the walk was aborted, it was because we had a problem of some kind.
    assert((newBody == nullptr) == HasError &&
           "unexpected short-circuit while walking body");
    if (HasError)
      return FunctionBuilderBodyPreCheck::Error;

    if (hasReturnStmt())
      return FunctionBuilderBodyPreCheck::HasReturnStmt;

    assert(oldBody == newBody && "pre-check walk wasn't in-place?");

    return FunctionBuilderBodyPreCheck::Okay;
  }

  std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
    // Pre-check the expression.  If this fails, abort the walk immediately.
    // Otherwise, replace the expression with the result of pre-checking.
    // In either case, don't recurse into the expression.
    if (!SkipPrecheck &&
        ConstraintSystem::preCheckExpression(E, /*DC*/ Fn.getAsDeclContext())) {
      HasError = true;
      return std::make_pair(false, nullptr);
    }

    return std::make_pair(false, E);
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    // If we see a return statement, note it..
    if (auto returnStmt = dyn_cast<ReturnStmt>(S)) {
      if (!returnStmt->isImplicit()) {
        ReturnStmts.push_back(returnStmt);
        return std::make_pair(false, S);
      }
    }

    // Otherwise, recurse into the statement normally.
    return std::make_pair(true, S);
  }

  /// Ignore patterns.
  std::pair<bool, Pattern*> walkToPatternPre(Pattern *pat) override {
    return { false, pat };
  }
};

}

FunctionBuilderBodyPreCheck
PreCheckFunctionBuilderRequest::evaluate(Evaluator &eval, AnyFunctionRef fn,
                                         BraceStmt *body) const {
  // NOTE: 'body' is passed only for the request evaluater caching key.
  // Since source tooling (e.g. code completion) might replace the body,
  // the function alone is not sufficient for the key.
  assert(fn.getBody() == body &&
         "body must be the current body of the function");

  return PreCheckFunctionBuilderApplication(fn, false).run();
}

std::vector<ReturnStmt *> TypeChecker::findReturnStatements(AnyFunctionRef fn) {
  PreCheckFunctionBuilderApplication precheck(fn, true);
  (void)precheck.run();
  return precheck.getReturnStmts();
}
