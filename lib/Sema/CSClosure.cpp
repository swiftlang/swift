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

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "swift/Sema/ConstraintSystem.h"

using namespace swift;
using namespace swift::constraints;

namespace {

// Produce an implicit empty tuple expression.
Expr *getVoidExpr(ASTContext &ctx) {
  auto *voidExpr = TupleExpr::createEmpty(ctx,
                                          /*LParenLoc=*/SourceLoc(),
                                          /*RParenLoc=*/SourceLoc(),
                                          /*Implicit=*/true);
  voidExpr->setType(ctx.TheEmptyTupleType);
  return voidExpr;
}

/// Find any type variable references inside of an AST node.
class TypeVariableRefFinder : public ASTWalker {
  ConstraintSystem &CS;
  ASTNode Parent;

  llvm::SmallPtrSetImpl<TypeVariableType *> &ReferencedVars;

public:
  TypeVariableRefFinder(
      ConstraintSystem &cs, ASTNode parent,
      llvm::SmallPtrSetImpl<TypeVariableType *> &referencedVars)
      : CS(cs), Parent(parent), ReferencedVars(referencedVars) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      if (auto type = CS.getTypeIfAvailable(DRE->getDecl()))
        inferVariables(type);
    }

    return {true, expr};
  }

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *stmt) override {
    // Return statements have to reference outside result type
    // since all of them are joined by it if it's not specified
    // explicitly.
    if (isa<ReturnStmt>(stmt)) {
      if (auto *closure = getAsExpr<ClosureExpr>(Parent)) {
        inferVariables(CS.getClosureType(closure)->getResult());
      }
    }

    return {true, stmt};
  }

private:
  void inferVariables(Type type) {
    type = type->getWithoutSpecifierType();
    // Record the type variable itself because it has to
    // be in scope even when already bound.
    if (auto *typeVar = type->getAs<TypeVariableType>()) {
      ReferencedVars.insert(typeVar);

      // It is possible that contextual type of a parameter/result
      // has been assigned to e.g. an anonymous or named argument
      // early, to facilitate closure type checking. Such a
      // type can have type variables inside e.g.
      //
      // func test<T>(_: (UnsafePointer<T>) -> Void) {}
      //
      // test { ptr in
      //  ...
      // }
      //
      // Type variable representing `ptr` in the body of
      // this closure would be bound to `UnsafePointer<$T>`
      // in this case, where `$T` is a type variable for a
      // generic parameter `T`.
      type = CS.getFixedTypeRecursive(typeVar, /*wantRValue=*/false);

      if (type->isEqual(typeVar))
        return;
    }

    if (type->hasTypeVariable()) {
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      type->getTypeVariables(typeVars);
      ReferencedVars.insert(typeVars.begin(), typeVars.end());
    }
  }
};

/// Find any references to not yet resolved outer closure parameters
/// used in the body of the inner closure. This is required because
/// isolated conjunctions, just like single-expression closures, have
/// to be connected to type variables they are going to use, otherwise
/// they'll get placed in a separate solver component and would never
/// produce a solution.
class UnresolvedClosureParameterCollector : public ASTWalker {
  ConstraintSystem &CS;

  llvm::SmallSetVector<TypeVariableType *, 4> Vars;

public:
  UnresolvedClosureParameterCollector(ConstraintSystem &cs) : CS(cs) {}

  std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      auto *decl = DRE->getDecl();
      if (isa<ParamDecl>(decl)) {
        if (auto type = CS.getTypeIfAvailable(decl)) {
          if (auto *typeVar = type->getAs<TypeVariableType>()) {
            Vars.insert(typeVar);
          } else if (type->hasTypeVariable()) {
            // Parameter or result type could be only partially
            // resolved e.g. `{ (x: X) -> Void in ... }` where
            // `X` is a generic type.
            SmallPtrSet<TypeVariableType *, 4> typeVars;
            type->getTypeVariables(typeVars);
            Vars.insert(typeVars.begin(), typeVars.end());
          }
        }
      }
    }
    return {true, expr};
  }

  ArrayRef<TypeVariableType *> getVariables() const {
    return Vars.getArrayRef();
  }
};

// MARK: Constraint generation

/// Check whether it makes sense to convert this element into a constrant.
static bool isViableElement(ASTNode element) {
  if (auto *decl = element.dyn_cast<Decl *>()) {
    // - Ignore variable declarations, they are handled by pattern bindings;
    // - Ignore #if, the chosen children should appear in the
    // surrounding context;
    // - Skip #warning and #error, they are handled during solution
    //   application.
    if (isa<VarDecl>(decl) || isa<IfConfigDecl>(decl) ||
        isa<PoundDiagnosticDecl>(decl))
      return false;
  }

  if (auto *stmt = element.dyn_cast<Stmt *>()) {
    // Empty brace statements are now viable because they do not require
    // inference.
    if (auto *braceStmt = dyn_cast<BraceStmt>(stmt)) {
      return braceStmt->getNumElements() > 0;
    }
  }

  return true;
}

using ElementInfo = std::tuple<ASTNode, ContextualTypeInfo,
                               /*isDiscarded=*/bool, ConstraintLocator *>;

static void createConjunction(ConstraintSystem &cs,
                              ArrayRef<ElementInfo> elements,
                              ConstraintLocator *locator) {
  bool isIsolated = false;

  SmallVector<Constraint *, 4> constraints;
  SmallVector<TypeVariableType *, 2> referencedVars;

  if (locator->directlyAt<ClosureExpr>()) {
    auto *closure = castToExpr<ClosureExpr>(locator->getAnchor());
    // Conjunction associated with the body of the closure has to
    // reference a type variable representing closure type,
    // otherwise it would get disconnected from its contextual type.
    referencedVars.push_back(cs.getType(closure)->castTo<TypeVariableType>());
    // Body of the closure is always isolated from its context, only
    // its individual elements are allowed access to type information
    // from the ouside e.g. parameters/result type.
    isIsolated = true;
  }

  UnresolvedClosureParameterCollector paramCollector(cs);

  for (const auto &entry : elements) {
    ASTNode element = std::get<0>(entry);
    ContextualTypeInfo context = std::get<1>(entry);
    bool isDiscarded = std::get<2>(entry);
    ConstraintLocator *elementLoc = std::get<3>(entry);

    if (!isViableElement(element))
      continue;

    // If this conjunction going to represent a body of a closure,
    // let's collect references to not yet resolved outer
    // closure parameters.
    if (isIsolated)
      element.walk(paramCollector);

    constraints.push_back(Constraint::createClosureBodyElement(
        cs, element, context, elementLoc, isDiscarded));
  }

  // It's possible that there are no viable elements in the body,
  // because e.g. whole body is an `#if` statement or it only has
  // declarations that are checked during solution application.
  // In such cases, let's avoid creating a conjunction.
  if (constraints.empty())
    return;

  for (auto *externalVar : paramCollector.getVariables())
    referencedVars.push_back(externalVar);

  cs.addUnsolvedConstraint(Constraint::createConjunction(
      cs, constraints, isIsolated, locator, referencedVars));
}

ElementInfo makeElement(ASTNode node, ConstraintLocator *locator,
                        ContextualTypeInfo context = ContextualTypeInfo(),
                        bool isDiscarded = false) {
  return std::make_tuple(node, context, isDiscarded, locator);
}

static ProtocolDecl *getSequenceProtocol(ASTContext &ctx, SourceLoc loc,
                                         bool inAsyncContext) {
  return TypeChecker::getProtocol(ctx, loc,
                                  inAsyncContext
                                      ? KnownProtocolKind::AsyncSequence
                                      : KnownProtocolKind::Sequence);
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

  void visitPattern(Pattern *pattern, ContextualTypeInfo context) {
    auto parentElement =
        locator->getLastElementAs<LocatorPathElt::ClosureBodyElement>();

    if (!parentElement) {
      hadError = true;
      return;
    }

    if (auto *stmt = parentElement->getElement().dyn_cast<Stmt *>()) {
      if (isa<ForEachStmt>(stmt)) {
        visitForEachPattern(pattern, cast<ForEachStmt>(stmt));
        return;
      }

      if (isa<CaseStmt>(stmt)) {
        visitCaseItemPattern(pattern, context);
        return;
      }
    }

    llvm_unreachable("Unsupported pattern");
  }

  void visitCaseItem(CaseLabelItem *caseItem, ContextualTypeInfo context) {
    assert(context.purpose == CTP_CaseStmt);

    // Resolve the pattern.
    auto *pattern = caseItem->getPattern();
    if (!caseItem->isPatternResolved()) {
      pattern = TypeChecker::resolvePattern(pattern, closure,
                                            /*isStmtCondition=*/false);
      if (!pattern) {
        hadError = true;
        return;
      }
    }

    // Let's generate constraints for pattern + where clause.
    // The assumption is that this shouldn't be too complex
    // to handle, but if it turns out to be false, this could
    // always be converted into a conjunction.

    // Generate constraints for pattern.
    visitPattern(pattern, context);

    auto *guardExpr = caseItem->getGuardExpr();

    // Generate constraints for `where` clause (if any).
    if (guardExpr) {
      guardExpr = cs.generateConstraints(guardExpr, closure);
      if (!guardExpr) {
        hadError = true;
        return;
      }
    }

    // Save information about case item so it could be referenced during
    // solution application.
    cs.setCaseLabelItemInfo(caseItem, {pattern, guardExpr});
  }

private:
  /// This method handles both pattern and the sequence expression
  /// associated with `for-in` loop because types in this situation
  /// flow in both directions:
  ///
  /// - From pattern to sequence, informing its element type e.g.
  ///   `for i: Int8 in 0 ..< 8`
  ///
  /// - From sequence to pattern, when pattern has no type information.
  void visitForEachPattern(Pattern *pattern, ForEachStmt *forEachStmt) {
    auto &ctx = cs.getASTContext();

    bool isAsync = forEachStmt->getAwaitLoc().isValid();

    // Verify pattern.
    {
      auto contextualPattern =
          ContextualPattern::forRawPattern(pattern, closure);
      Type patternType = TypeChecker::typeCheckPattern(contextualPattern);

      if (patternType->hasError()) {
        hadError = true;
        return;
      }
    }

    auto *sequenceProto =
        getSequenceProtocol(ctx, forEachStmt->getForLoc(), isAsync);
    if (!sequenceProto) {
      hadError = true;
      return;
    }

    auto *contextualLocator = cs.getConstraintLocator(
        locator, LocatorPathElt::ContextualType(CTP_ForEachStmt));

    // Generate constraints to initialize the pattern.
    auto initType =
        cs.generateConstraints(pattern, contextualLocator,
                               /*shouldBindPatternOneWay=*/false,
                               /*patternBinding=*/nullptr, /*patternIndex=*/0);

    if (!initType) {
      hadError = true;
      return;
    }

    // Let's generate constraints for sequence associated with `for-in`
    // statement. We can't do that separately because pattern can inform
    // a type of the sequence e.g. `for in i: Int8 in 0 ..< 8 { ... }`

    auto *sequenceExpr = forEachStmt->getSequence();
    auto *sequenceLocator = cs.getConstraintLocator(sequenceExpr);

    {
      SolutionApplicationTarget target(
          sequenceExpr, closure, CTP_ForEachSequence,
          sequenceProto->getDeclaredInterfaceType(),
          /*isDiscarded=*/false);

      if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow)) {
        hadError = true;
        return;
      }

      cs.setSolutionApplicationTarget(sequenceExpr, target);
    }

    Type sequenceType =
        cs.createTypeVariable(sequenceLocator, TVO_CanBindToNoEscape);
    // This "workaround" warrants an explanation for posterity.
    //
    // The reason why we can't simplify use \c getType(sequenceExpr) here
    // is due to how dependent member types are handled by \c simplifyTypeImpl
    // - if the base type didn't change (and it wouldn't because it's a fully
    // resolved concrete type) after simplification attempt the
    // whole dependent member type would be just re-created without attempting
    // to resolve it, so we have to use an intermediary here so that
    // \c elementType and \c iteratorType can be resolved correctly.
    cs.addConstraint(ConstraintKind::Conversion, cs.getType(sequenceExpr),
                     sequenceType, sequenceLocator);

    auto elementAssocType = sequenceProto->getAssociatedType(ctx.Id_Element);
    Type elementType = DependentMemberType::get(sequenceType, elementAssocType);

    auto iteratorAssocType = sequenceProto->getAssociatedType(
        isAsync ? ctx.Id_AsyncIterator : ctx.Id_Iterator);
    Type iteratorType =
        DependentMemberType::get(sequenceType, iteratorAssocType);

    cs.addConstraint(
        ConstraintKind::Conversion, elementType, initType,
        cs.getConstraintLocator(contextualLocator,
                                ConstraintLocator::SequenceElementType));

    // Reference the makeIterator witness.
    FuncDecl *makeIterator = isAsync ? ctx.getAsyncSequenceMakeAsyncIterator()
                                     : ctx.getSequenceMakeIterator();

    Type makeIteratorType =
        cs.createTypeVariable(locator, TVO_CanBindToNoEscape);
    cs.addValueWitnessConstraint(LValueType::get(sequenceType), makeIterator,
                                 makeIteratorType, closure,
                                 FunctionRefKind::Compound, contextualLocator);

    // After successful constraint generation, let's record
    // solution application target with all relevant information.
    {
      auto target = SolutionApplicationTarget::forForEachStmt(
          forEachStmt, sequenceProto, closure,
          /*bindTypeVarsOneWay=*/false,
          /*contextualPurpose=*/CTP_ForEachSequence);

      auto &targetInfo = target.getForEachStmtInfo();

      targetInfo.sequenceType = sequenceType;
      targetInfo.elementType = elementType;
      targetInfo.iteratorType = iteratorType;
      targetInfo.initType = initType;

      target.setPattern(pattern);

      cs.setSolutionApplicationTarget(forEachStmt, target);
    }
  }

  void visitCaseItemPattern(Pattern *pattern, ContextualTypeInfo context) {
    Type patternType = cs.generateConstraints(
        pattern, locator, /*bindPatternVarsOneWay=*/false,
        /*patternBinding=*/nullptr, /*patternIndex=*/0);

    if (!patternType) {
      hadError = true;
      return;
    }

    // Convert the contextual type to the pattern, which establishes the
    // bindings.
    cs.addConstraint(ConstraintKind::Conversion, context.getType(), patternType,
                     locator);

    // For any pattern variable that has a parent variable (i.e., another
    // pattern variable with the same name in the same case), require that
    // the types be equivalent.
    pattern->forEachNode([&](Pattern *pattern) {
      auto namedPattern = dyn_cast<NamedPattern>(pattern);
      if (!namedPattern)
        return;

      auto var = namedPattern->getDecl();
      if (auto parentVar = var->getParentVarDecl()) {
        cs.addConstraint(
            ConstraintKind::Equal, cs.getType(parentVar), cs.getType(var),
            cs.getConstraintLocator(
                locator, LocatorPathElt::PatternMatch(namedPattern)));
      }
    });
  }

  void visitPatternBinding(PatternBindingDecl *patternBinding,
                           SmallVectorImpl<ElementInfo> &patterns) {
    auto *baseLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::ClosureBodyElement(patternBinding));

    for (unsigned index : range(patternBinding->getNumPatternEntries())) {
      auto *pattern = TypeChecker::resolvePattern(
          patternBinding->getPattern(index), patternBinding->getDeclContext(),
          /*isStmtCondition=*/true);

      if (!pattern) {
        hadError = true;
        return;
      }

      // Reset binding to point to the resolved pattern. This is required
      // before calling `forPatternBindingDecl`.
      patternBinding->setPattern(index, pattern,
                                 patternBinding->getInitContext(index));

      patterns.push_back(makeElement(
          patternBinding,
          cs.getConstraintLocator(
              baseLoc, LocatorPathElt::PatternBindingElement(index))));
    }
  }

  Optional<SolutionApplicationTarget>
  getTargetForPattern(PatternBindingDecl *patternBinding, unsigned index,
                      Type patternType) {
    auto hasPropertyWrapper = [&](Pattern *pattern) -> bool {
      if (auto *singleVar = pattern->getSingleVar())
        return singleVar->hasAttachedPropertyWrapper();
      return false;
    };

    auto *pattern = patternBinding->getPattern(index);
    auto *init = patternBinding->getInit(index);

    if (!init && patternBinding->isDefaultInitializable(index) &&
        pattern->hasStorage()) {
      init = TypeChecker::buildDefaultInitializer(patternType);
    }

    if (init) {
      return SolutionApplicationTarget::forInitialization(
          init, patternBinding->getDeclContext(), patternType, patternBinding,
          index,
          /*bindPatternVarsOneWay=*/false);
    }

    // If there was no initializer, there could be one from a property
    // wrapper which has to be pre-checked before use. This is not a
    // problem in top-level code because pattern bindings go through
    // `typeCheckExpression` which does pre-check automatically and
    // result builders do not allow declaring local wrapped variables.
    if (hasPropertyWrapper(pattern)) {
      auto target = SolutionApplicationTarget::forInitialization(
          init, patternBinding->getDeclContext(), patternType, patternBinding,
          index,
          /*bindPatternVarsOneWay=*/false);

      if (ConstraintSystem::preCheckTarget(
              target, /*replaceInvalidRefsWithErrors=*/true,
              /*LeaveCLosureBodyUnchecked=*/false))
        return None;

      return target;
    }

    return SolutionApplicationTarget::forUninitializedVar(patternBinding, index,
                                                          patternType);
  }

  void visitPatternBindingElement(PatternBindingDecl *patternBinding) {
    assert(locator->isLastElement<LocatorPathElt::PatternBindingElement>());

    auto index =
        locator->castLastElementTo<LocatorPathElt::PatternBindingElement>()
            .getIndex();

    auto contextualPattern =
        ContextualPattern::forPatternBindingDecl(patternBinding, index);
    Type patternType = TypeChecker::typeCheckPattern(contextualPattern);

    // Fail early if pattern couldn't be type-checked.
    if (!patternType || patternType->hasError()) {
      hadError = true;
      return;
    }

    auto target = getTargetForPattern(patternBinding, index, patternType);
    if (!target ||
        cs.generateConstraints(*target, FreeTypeVariableBinding::Disallow)) {
      hadError = true;
      return;
    }

    // Keep track of this binding entry.
    cs.setSolutionApplicationTarget({patternBinding, index}, *target);
  }

  void visitDecl(Decl *decl) {
    if (isSupportedMultiStatementClosure()) {
      if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
        if (locator->isLastElement<LocatorPathElt::PatternBindingElement>())
          visitPatternBindingElement(patternBinding);
        else
          llvm_unreachable("cannot visit pattern binding directly");
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

    // Other declarations will be handled at application time.
  }

  void visitBreakStmt(BreakStmt *breakStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Break");
  }

  void visitContinueStmt(ContinueStmt *continueStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Continue");
  }

  void visitDeferStmt(DeferStmt *deferStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Defer");
  }

  void visitFallthroughStmt(FallthroughStmt *fallthroughStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Fallthrough");
  }

  void visitStmtCondition(LabeledConditionalStmt *S,
                          SmallVectorImpl<ElementInfo> &elements,
                          ConstraintLocator *locator) {
    auto *condLocator =
        cs.getConstraintLocator(locator, ConstraintLocator::Condition);
    for (auto &condition : S->getCond())
      elements.push_back(makeElement(&condition, condLocator));
  }

  void visitIfStmt(IfStmt *ifStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: If");

    SmallVector<ElementInfo, 4> elements;

    // Condition
    visitStmtCondition(ifStmt, elements, locator);

    // Then Branch
    {
      auto *thenLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::TernaryBranch(/*then=*/true));
      elements.push_back(makeElement(ifStmt->getThenStmt(), thenLoc));
    }

    // Else Branch (if any).
    if (auto *elseStmt = ifStmt->getElseStmt()) {
      auto *elseLoc = cs.getConstraintLocator(
          locator, LocatorPathElt::TernaryBranch(/*then=*/false));
      elements.push_back(makeElement(ifStmt->getElseStmt(), elseLoc));
    }

    createConjunction(cs, elements, locator);
  }

  void visitGuardStmt(GuardStmt *guardStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Guard");

    SmallVector<ElementInfo, 4> elements;

    visitStmtCondition(guardStmt, elements, locator);
    elements.push_back(makeElement(guardStmt->getBody(), locator));

    createConjunction(cs, elements, locator);
  }

  void visitWhileStmt(WhileStmt *whileStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: While");

    SmallVector<ElementInfo, 4> elements;

    visitStmtCondition(whileStmt, elements, locator);
    elements.push_back(makeElement(whileStmt->getBody(), locator));

    createConjunction(cs, elements, locator);
  }

  void visitDoStmt(DoStmt *doStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Do");

    visitBraceStmt(doStmt->getBody());
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *repeatWhileStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: RepeatWhile");

    createConjunction(cs,
                      {makeElement(repeatWhileStmt->getCond(),
                                   cs.getConstraintLocator(
                                       locator, ConstraintLocator::Condition),
                                   getContextForCondition()),
                       makeElement(repeatWhileStmt->getBody(), locator)},
                      locator);
  }

  void visitPoundAssertStmt(PoundAssertStmt *poundAssertStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: PoundAssert");

    createConjunction(cs,
                      {makeElement(poundAssertStmt->getCondition(),
                                   cs.getConstraintLocator(
                                       locator, ConstraintLocator::Condition),
                                   getContextForCondition())},
                      locator);
  }

  void visitThrowStmt(ThrowStmt *throwStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Throw");

    if (!cs.getASTContext().getErrorDecl()) {
      hadError = true;
      return;
    }

    auto errType = cs.getASTContext().getErrorExistentialType();
    auto *errorExpr = throwStmt->getSubExpr();

    createConjunction(
        cs,
        {makeElement(
            errorExpr,
            cs.getConstraintLocator(
                locator, LocatorPathElt::ClosureBodyElement(errorExpr)),
            {errType, CTP_ThrowStmt})},
        locator);
  }

  void visitForEachStmt(ForEachStmt *forEachStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: ForEach");

    auto *stmtLoc = cs.getConstraintLocator(locator);

    SmallVector<ElementInfo, 4> elements;

    // For-each pattern.
    //
    // Note that we don't record a sequence here, it would
    // be handled together with pattern because pattern can
    // inform a type of sequence element e.g. `for i: Int8 in 0 ..< 8`
    {
      Pattern *pattern =
          TypeChecker::resolvePattern(forEachStmt->getPattern(), closure,
                                      /*isStmtCondition=*/false);

      if (!pattern) {
        hadError = true;
        return;
      }

      elements.push_back(makeElement(pattern, stmtLoc));
    }

    // `where` clause if any.
    if (auto *whereClause = forEachStmt->getWhere()) {
      elements.push_back(
          makeElement(whereClause, stmtLoc, getContextForCondition()));
    }

    // Body of the `for-in` loop.
    elements.push_back(makeElement(forEachStmt->getBody(), stmtLoc));

    createConjunction(cs, elements, locator);
  }

  void visitSwitchStmt(SwitchStmt *switchStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Switch");

    auto *switchLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::ClosureBodyElement(switchStmt));

    SmallVector<ElementInfo, 4> elements;
    {
      auto *subjectExpr = switchStmt->getSubjectExpr();
      {
        elements.push_back(makeElement(subjectExpr, switchLoc));

        SolutionApplicationTarget target(subjectExpr, closure, CTP_Unused,
                                         Type(), /*isDiscarded=*/false);

        cs.setSolutionApplicationTarget(switchStmt, target);
      }

      for (auto rawCase : switchStmt->getRawCases())
        elements.push_back(makeElement(rawCase, switchLoc));
    }

    createConjunction(cs, elements, switchLoc);
  }

  void visitDoCatchStmt(DoCatchStmt *doStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: DoCatch");

    auto *doLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::ClosureBodyElement(doStmt));

    SmallVector<ElementInfo, 4> elements;

    // First, let's record a body of `do` statement.
    elements.push_back(makeElement(doStmt->getBody(), doLoc));

    // After that has been type-checked, let's switch to
    // individual `catch` statements.
    for (auto *catchStmt : doStmt->getCatches())
      elements.push_back(makeElement(catchStmt, doLoc));

    createConjunction(cs, elements, doLoc);
  }

  void visitCaseStmt(CaseStmt *caseStmt) {
    assert(isSupportedMultiStatementClosure() &&
           "Unsupported statement: Case");

    Type contextualTy;

    {
      auto parent =
          locator->castLastElementTo<LocatorPathElt::ClosureBodyElement>()
              .getElement();

      if (parent.isStmt(StmtKind::Switch)) {
        auto *switchStmt = cast<SwitchStmt>(parent.get<Stmt *>());
        contextualTy = cs.getType(switchStmt->getSubjectExpr());
      } else if (parent.isStmt(StmtKind::DoCatch)) {
        contextualTy = cs.getASTContext().getErrorExistentialType();
      } else {
        hadError = true;
        return;
      }
    }

    bindSwitchCasePatternVars(closure, caseStmt);

    auto *caseLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::ClosureBodyElement(caseStmt));

    SmallVector<ElementInfo, 4> elements;
    for (auto &caseLabelItem : caseStmt->getMutableCaseLabelItems()) {
      elements.push_back(
          makeElement(&caseLabelItem, caseLoc, {contextualTy, CTP_CaseStmt}));
    }

    elements.push_back(makeElement(caseStmt->getBody(), caseLoc));

    createConjunction(cs, elements, caseLoc);
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
    if (isSupportedMultiStatementClosure()) {
      auto &ctx = cs.getASTContext();

      if (isChildOf(StmtKind::Case)) {
        auto *caseStmt = cast<CaseStmt>(
            locator->castLastElementTo<LocatorPathElt::ClosureBodyElement>()
                .asStmt());

        for (auto caseBodyVar : caseStmt->getCaseBodyVariablesOrEmptyArray()) {
          auto parentVar = caseBodyVar->getParentVarDecl();
          assert(parentVar && "Case body variables always have parents");
          cs.setType(caseBodyVar, cs.getType(parentVar));
        }
      }

      SmallVector<ElementInfo, 4> elements;
      for (auto element : braceStmt->getElements()) {
        bool isDiscarded =
            element.is<Expr *>() &&
            (!ctx.LangOpts.Playground && !ctx.LangOpts.DebuggerSupport);

        if (auto *decl = element.dyn_cast<Decl *>()) {
          if (auto *PDB = dyn_cast<PatternBindingDecl>(decl)) {
            visitPatternBinding(PDB, elements);
            continue;
          }
        }

        elements.push_back(makeElement(
            element,
            cs.getConstraintLocator(
                locator, LocatorPathElt::ClosureBodyElement(element)),
            /*contextualInfo=*/{}, isDiscarded));
      }

      createConjunction(cs, elements, locator);
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
    auto contextualTy = cs.getClosureType(closure)->getResult();

    // Single-expression closures are effectively a `return` statement,
    // so let's give them a special locator as to indicate that.
    // Return statements might not have a result if we have a closure whose
    // implicit returned value is coerced to Void.
    if (closure->hasSingleExpressionBody() && returnStmt->hasResult()) {
      auto *expr = returnStmt->getResult();
      assert(expr && "single expression closure without expression?");

      expr = cs.generateConstraints(expr, closure, /*isInputExpression=*/false);
      if (!expr) {
        hadError = true;
        return;
      }

      cs.addConstraint(
          ConstraintKind::Conversion, cs.getType(expr), contextualTy,
          cs.getConstraintLocator(
              closure, LocatorPathElt::ClosureBody(
                           /*hasReturn=*/!returnStmt->isImplicit())));
      return;
    }

    Expr *resultExpr;

    if (returnStmt->hasResult()) {
      resultExpr = returnStmt->getResult();
      assert(resultExpr && "non-empty result without expression?");
    } else {
      // If this is simplify `return`, let's create an empty tuple
      // which is also useful if contextual turns out to be e.g. `Void?`.
      resultExpr = getVoidExpr(closure->getASTContext());
    }

    SolutionApplicationTarget target(resultExpr, closure, CTP_ReturnStmt,
                                     contextualTy,
                                     /*isDiscarded=*/false);

    if (cs.generateConstraints(target, FreeTypeVariableBinding::Disallow)) {
      hadError = true;
      return;
    }

    cs.setContextualType(target.getAsExpr(), TypeLoc::withoutLoc(contextualTy),
                         CTP_ReturnStmt);
    cs.setSolutionApplicationTarget(returnStmt, target);
  }

  bool isSupportedMultiStatementClosure() const {
    return !closure->hasSingleExpressionBody() &&
           cs.participatesInInference(closure);
  }

#define UNSUPPORTED_STMT(STMT) void visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(Fail)
#undef UNSUPPORTED_STMT

private:
  ContextualTypeInfo getContextForCondition() const {
    auto boolDecl = cs.getASTContext().getBoolDecl();
    assert(boolDecl && "Bool is missing");
    return {boolDecl->getDeclaredInterfaceType(), CTP_Condition};
  }

  bool isChildOf(StmtKind kind) {
    if (locator->getPath().empty())
      return false;

    auto parentElt =
        locator->getLastElementAs<LocatorPathElt::ClosureBodyElement>();
    return parentElt ? parentElt->getElement().isStmt(kind) : false;
  }
};
}

bool ConstraintSystem::generateConstraints(ClosureExpr *closure) {
  auto &ctx = closure->getASTContext();

  if (participatesInInference(closure)) {
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

bool ConstraintSystem::isInResultBuilderContext(ClosureExpr *closure) const {
  if (!closure->hasSingleExpressionBody()) {
    auto *DC = closure->getParent();
    do {
      // Result builder is applied to a function/getter body.
      if (auto *AFD = dyn_cast<AbstractFunctionDecl>(DC)) {
        if (resultBuilderTransformed.count(AFD))
          return true;
      }

      if (auto *parentClosure = dyn_cast<ClosureExpr>(DC)) {
        if (resultBuilderTransformed.count(parentClosure))
          return true;
      }
    } while ((DC = DC->getParent()));
  }
  return false;
}

bool isConditionOfStmt(ConstraintLocatorBuilder locator) {
  auto last = locator.last();
  if (!(last && last->is<LocatorPathElt::Condition>()))
    return false;

  SmallVector<LocatorPathElt, 4> path;
  (void)locator.getLocatorParts(path);

  path.pop_back();

  if (path.empty())
    return false;

  if (auto closureElt = path.back().getAs<LocatorPathElt::ClosureBodyElement>())
    return closureElt->getElement().dyn_cast<Stmt *>();

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifyClosureBodyElementConstraint(
    ASTNode element, ContextualTypeInfo context, bool isDiscarded,
    TypeMatchOptions flags, ConstraintLocatorBuilder locator) {
  auto *closure = castToExpr<ClosureExpr>(locator.getAnchor());

  ClosureConstraintGenerator generator(*this, closure,
                                       getConstraintLocator(locator));

  if (auto *expr = element.dyn_cast<Expr *>()) {
    SolutionApplicationTarget target(expr, closure, context.purpose,
                                     context.getType(), isDiscarded);

    if (generateConstraints(target, FreeTypeVariableBinding::Disallow))
      return SolutionKind::Error;

    setSolutionApplicationTarget(expr, target);
    return SolutionKind::Solved;
  } else if (auto *stmt = element.dyn_cast<Stmt *>()) {
    generator.visit(stmt);
  } else if (auto *cond = element.dyn_cast<StmtConditionElement *>()) {
    if (generateConstraints({*cond}, closure))
      return SolutionKind::Error;
  } else if (auto *pattern = element.dyn_cast<Pattern *>()) {
    generator.visitPattern(pattern, context);
  } else if (auto *caseItem = element.dyn_cast<CaseLabelItem *>()) {
    generator.visitCaseItem(caseItem, context);
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

  /// All `func`s declared in the body of the closure.
  SmallVector<FuncDecl *, 4> LocalFuncs;

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
      if (!rewriteTarget(target)) {
        hadError = true;
        return;
      }

      // Allow `typeCheckDecl` to be called after solution is applied
      // to a pattern binding. That would materialize required
      // information e.g. accessors and do access/availability checks.
    }

    // Local functions cannot be type-checked in-order because they can
    // capture variables declared after them. Let's save them to be
    // processed after the solution has been applied to the body.
    if (auto *func = dyn_cast<FuncDecl>(decl)) {
      LocalFuncs.push_back(func);
      return;
    }

    TypeChecker::typeCheckDecl(decl);
  }

  ASTNode visitBreakStmt(BreakStmt *breakStmt) {
    if (auto target = findBreakOrContinueStmtTarget(
            closure->getASTContext(), closure->getParentSourceFile(),
            breakStmt->getLoc(), breakStmt->getTargetName(),
            breakStmt->getTargetLoc(),
            /*isContinue=*/false, closure)) {
      breakStmt->setTarget(target);
    }

    return breakStmt;
  }

  ASTNode visitContinueStmt(ContinueStmt *continueStmt) {
    if (auto target = findBreakOrContinueStmtTarget(
            closure->getASTContext(), closure->getParentSourceFile(),
            continueStmt->getLoc(), continueStmt->getTargetName(),
            continueStmt->getTargetLoc(), /*isContinue=*/true, closure)) {
      continueStmt->setTarget(target);
    }

    return continueStmt;
  }

  ASTNode visitFallthroughStmt(FallthroughStmt *fallthroughStmt) {
    if (checkFallthroughStmt(closure, fallthroughStmt))
      hadError = true;
    return fallthroughStmt;
  }

  ASTNode visitDeferStmt(DeferStmt *deferStmt) {
    TypeChecker::typeCheckDecl(deferStmt->getTempDecl());

    Expr *theCall = deferStmt->getCallExpr();
    TypeChecker::typeCheckExpression(theCall, closure);
    deferStmt->setCallExpr(theCall);

    return deferStmt;
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

  ASTNode visitGuardStmt(GuardStmt *guardStmt) {
    if (auto condition = rewriteTarget(
            SolutionApplicationTarget(guardStmt->getCond(), closure)))
      guardStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto *body = visit(guardStmt->getBody()).get<Stmt *>();
    guardStmt->setBody(cast<BraceStmt>(body));
    return guardStmt;
  }

  ASTNode visitWhileStmt(WhileStmt *whileStmt) {
    if (auto condition = rewriteTarget(
          SolutionApplicationTarget(whileStmt->getCond(), closure)))
      whileStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto *body = visit(whileStmt->getBody()).get<Stmt *>();
    whileStmt->setBody(cast<BraceStmt>(body));
    return whileStmt;
  }

  ASTNode visitDoStmt(DoStmt *doStmt) {
    auto body = visit(doStmt->getBody()).get<Stmt *>();
    doStmt->setBody(cast<BraceStmt>(body));
    return doStmt;
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

  ASTNode visitPoundAssertStmt(PoundAssertStmt *poundAssertStmt) {
    // FIXME: This should be done through \c solution instead of
    //        constraint system.
    auto &cs = solution.getConstraintSystem();
    // Rewrite the condition.
    auto target =
        *cs.getSolutionApplicationTarget(poundAssertStmt->getCondition());

    if (auto result = rewriteTarget(target))
      poundAssertStmt->setCondition(result->getAsExpr());
    else
      hadError = true;

    return poundAssertStmt;
  }

  ASTNode visitThrowStmt(ThrowStmt *throwStmt) {
    auto &cs = solution.getConstraintSystem();

    // Rewrite the error.
    auto target = *cs.getSolutionApplicationTarget(throwStmt->getSubExpr());
    if (auto result = rewriteTarget(target))
      throwStmt->setSubExpr(result->getAsExpr());
    else
      hadError = true;

    return throwStmt;
  }

  ASTNode visitForEachStmt(ForEachStmt *forEachStmt) {
    ConstraintSystem &cs = solution.getConstraintSystem();

    auto forEachTarget =
        rewriteTarget(*cs.getSolutionApplicationTarget(forEachStmt));

    if (!forEachTarget)
      hadError = true;

    auto body = visit(forEachStmt->getBody()).get<Stmt *>();
    forEachStmt->setBody(cast<BraceStmt>(body));

    // Check to see if the sequence expr is throwing (in async context),
    // if so require the stmt to have a `try`.
    hadError |= diagnoseUnhandledThrowsInAsyncContext(closure, forEachStmt);

    return forEachStmt;
  }

  ASTNode visitSwitchStmt(SwitchStmt *switchStmt) {
    ConstraintSystem &cs = solution.getConstraintSystem();

    // Rewrite the switch subject.
    auto subjectTarget =
        rewriteTarget(*cs.getSolutionApplicationTarget(switchStmt));
    if (subjectTarget) {
      switchStmt->setSubjectExpr(subjectTarget->getAsExpr());
    } else {
      hadError = true;
    }

    // Visit the raw cases.
    bool limitExhaustivityChecks = false;
    for (auto rawCase : switchStmt->getRawCases()) {
      if (auto decl = rawCase.dyn_cast<Decl *>()) {
        visitDecl(decl);
        continue;
      }

      auto caseStmt = cast<CaseStmt>(rawCase.get<Stmt *>());
      visitCaseStmt(caseStmt);

      // Check restrictions on '@unknown'.
      if (caseStmt->hasUnknownAttr()) {
        checkUnknownAttrRestrictions(cs.getASTContext(), caseStmt,
                                     limitExhaustivityChecks);
      }
    }

    TypeChecker::checkSwitchExhaustiveness(switchStmt, closure,
                                           limitExhaustivityChecks);

    return switchStmt;
  }

  ASTNode visitDoCatchStmt(DoCatchStmt *doStmt) {
    // Translate the body.
    auto newBody = visit(doStmt->getBody());
    doStmt->setBody(newBody.get<Stmt *>());

    // Visit the catch blocks.
    for (auto catchStmt : doStmt->getCatches())
      visitCaseStmt(catchStmt);

    return doStmt;
  }

  ASTNode visitCaseStmt(CaseStmt *caseStmt) {
    // Translate the patterns and guard expressions for each case label item.
    for (auto &caseItem : caseStmt->getMutableCaseLabelItems()) {
      SolutionApplicationTarget caseTarget(&caseItem, closure);
      if (!rewriteTarget(caseTarget)) {
        hadError = true;
      }
    }

    for (auto *expected : caseStmt->getCaseBodyVariablesOrEmptyArray()) {
      assert(expected->hasName());
      auto prev = expected->getParentVarDecl();
      auto type = solution.resolveInterfaceType(
          solution.getType(prev)->mapTypeOutOfContext());
      expected->setInterfaceType(type);
    }

    // Translate the body.
    auto *newBody = visit(caseStmt->getBody()).get<Stmt *>();
    caseStmt->setBody(cast<BraceStmt>(newBody));

    return caseStmt;
  }

  ASTNode visitBraceStmt(BraceStmt *braceStmt) {
    auto &cs = solution.getConstraintSystem();

    // Diagnose defer statement being last one in block.
    if (!braceStmt->empty()) {
      if (auto stmt = braceStmt->getLastElement().dyn_cast<Stmt *>()) {
        if (auto deferStmt = dyn_cast<DeferStmt>(stmt)) {
          auto &diags = closure->getASTContext().Diags;
          diags
              .diagnose(deferStmt->getStartLoc(), diag::defer_stmt_at_block_end)
              .fixItReplace(deferStmt->getStartLoc(), "do");
        }
      }
    }

    for (auto &node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        // Rewrite the expression.
        auto target = *cs.getSolutionApplicationTarget(expr);
        if (auto rewrittenTarget = rewriteTarget(target)) {
          node = rewrittenTarget->getAsExpr();

          if (target.isDiscardedExpr())
            TypeChecker::checkIgnoredExpr(castToExpr(node));
        } else {
          hadError = true;
        }
      } else if (auto stmt = node.dyn_cast<Stmt *>()) {
        node = visit(stmt);
      } else {
        visitDecl(node.get<Decl *>());
      }
    }

    // Source compatibility workaround.
    //
    // func test<T>(_: () -> T?) {
    //   ...
    // }
    //
    // A multi-statement closure passed to `test` that has an optional
    // `Void` result type inferred from the body allows:
    //   - empty `return`(s);
    //   - to skip `return nil` or `return ()` at the end.
    //
    // Implicit `return ()` has to be inserted as the last element
    // of the body if there is none. This wasn't needed before SE-0326
    // because result type was (incorrectly) inferred as `Void` due to
    // the body being skipped.
    if (!closure->hasSingleExpressionBody() &&
        closure->getBody() == braceStmt) {
      if (resultType->getOptionalObjectType() &&
          resultType->lookThroughAllOptionalTypes()->isVoid() &&
          !braceStmt->getLastElement().isStmt(StmtKind::Return)) {
        return addImplicitVoidReturn(braceStmt);
      }
    }

    return braceStmt;
  }

  ASTNode addImplicitVoidReturn(BraceStmt *braceStmt) {
    auto &ctx = closure->getASTContext();
    auto &cs = solution.getConstraintSystem();

    auto *resultExpr = getVoidExpr(ctx);
    cs.cacheExprTypes(resultExpr);

    auto *returnStmt = new (ctx) ReturnStmt(SourceLoc(), resultExpr,
                                            /*implicit=*/true);

    // For a target for newly created result and apply a solution
    // to it, to make sure that optional injection happens required
    // number of times.
    {
      SolutionApplicationTarget target(resultExpr, closure, CTP_ReturnStmt,
                                       resultType,
                                       /*isDiscarded=*/false);
      cs.setSolutionApplicationTarget(returnStmt, target);

      visitReturnStmt(returnStmt);
    }

    // Re-create brace statement with an additional `return` at the end.

    SmallVector<ASTNode, 4> elements;
    elements.append(braceStmt->getElements().begin(),
                    braceStmt->getElements().end());
    elements.push_back(returnStmt);

    return BraceStmt::create(ctx, braceStmt->getLBraceLoc(), elements,
                             braceStmt->getRBraceLoc());
  }

  ASTNode visitReturnStmt(ReturnStmt *returnStmt) {
    if (!returnStmt->hasResult()) {
      // If contextual is not optional, there is nothing to do here.
      if (resultType->isVoid())
        return returnStmt;

      // It's possible to infer e.g. `Void?` for cases where
      // `return` doesn't have an expression. If contextual
      // type is `Void` wrapped into N optional types, let's
      // add an implicit `()` expression and let it be injected
      // into optional required number of times.

      assert(resultType->getOptionalObjectType() &&
             resultType->lookThroughAllOptionalTypes()->isVoid());

      auto &cs = solution.getConstraintSystem();

      auto target = *cs.getSolutionApplicationTarget(returnStmt);
      returnStmt->setResult(target.getAsExpr());
    }

    auto *resultExpr = returnStmt->getResult();

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
  UNSUPPORTED_STMT(Fail)
#undef UNSUPPORTED_STMT

public:
  /// Apply solution to the closure and return updated body.
  ASTNode apply() {
    auto body = visit(closure->getBody());

    // Since local functions can capture variables that are declared
    // after them, let's type-check them after all of the pattern
    // bindings have been resolved by applying solution to the body.
    for (auto *func : LocalFuncs)
      TypeChecker::typeCheckDecl(func);

    return body;
  }
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

    // Find any isolated parameters in this closure and mark them as isolated.
    for (auto param : solution.isolatedParams) {
      if (param->getDeclContext() == closure)
        param->setIsolated(true);
    }

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
  //
  // Multi-statement closures are handled separately because they need to
  // wait until all of the `ExtInfo` flags are propagated from the context
  // e.g. parameter could be no-escape if closure is applied to a call.
  if (closure->hasSingleExpressionBody()) {
    bool hadError =
        applySolutionToBody(solution, closure, currentDC, rewriteTarget);
    return hadError ? SolutionApplicationToFunctionResult::Failure
                    : SolutionApplicationToFunctionResult::Success;
  }

  // Otherwise, we need to delay type checking of the closure until later.
  solution.setExprTypes(closure);
  closure->setBodyState(ClosureExpr::BodyState::ReadyForTypeChecking);
  return SolutionApplicationToFunctionResult::Delay;
}

bool ConstraintSystem::applySolutionToBody(Solution &solution,
                                           ClosureExpr *closure,
                                           DeclContext *&currentDC,
                                           RewriteTargetFn rewriteTarget) {
  auto &cs = solution.getConstraintSystem();
  // Enter the context of the function before performing any additional
  // transformations.
  llvm::SaveAndRestore<DeclContext *> savedDC(currentDC, closure);

  auto closureType = cs.getType(closure)->castTo<FunctionType>();
  ClosureConstraintApplication application(
      solution, closure, closureType->getResult(), rewriteTarget);
  auto body = application.apply();

  if (!body || application.hadError)
    return true;

  closure->setBody(cast<BraceStmt>(body.get<Stmt *>()),
                   closure->hasSingleExpressionBody());
  closure->setBodyState(ClosureExpr::BodyState::TypeCheckedWithSignature);
  return false;
}

void ConjunctionElement::findReferencedVariables(
    ConstraintSystem &cs, SmallPtrSetImpl<TypeVariableType *> &typeVars) const {
  auto referencedVars = Element->getTypeVariables();
  typeVars.insert(referencedVars.begin(), referencedVars.end());

  if (Element->getKind() != ConstraintKind::ClosureBodyElement)
    return;

  ASTNode element = Element->getClosureElement();
  auto *locator = Element->getLocator();

  TypeVariableRefFinder refFinder(cs, locator->getAnchor(), typeVars);

  if (auto *patternBinding =
          dyn_cast_or_null<PatternBindingDecl>(element.dyn_cast<Decl *>())) {
    if (auto patternBindingElt =
            locator
                ->getLastElementAs<LocatorPathElt::PatternBindingElement>()) {
      if (auto *init = patternBinding->getInit(patternBindingElt->getIndex()))
        init->walk(refFinder);
      return;
    }
  }

  if (element.is<Decl *>() || element.is<StmtConditionElement *>() ||
      element.is<Expr *>() || element.isStmt(StmtKind::Return))
    element.walk(refFinder);
}
