//===--- CSSyntacticElement.cpp - Syntactic Element Constraints -----------===//
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
//
// This file implements SyntacticElement constraint generation and solution
// application, which is used to type-check the bodies of closures. It provides
// part of the implementation of the ConstraintSystem class.
//
//===----------------------------------------------------------------------===//

#include "MiscDiagnostics.h"
#include "TypeChecker.h"
#include "TypeCheckAvailability.h"
#include "swift/Basic/Assertions.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace swift::constraints;

void ConstraintSystem::setTargetFor(SyntacticElementTargetKey key,
                                    SyntacticElementTarget target) {
  bool inserted = targets.insert({key, target}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedTarget(key));
}

void ConstraintSystem::removeTargetFor(SyntacticElementTargetKey key) {
  bool erased = targets.erase(key);
  ASSERT(erased);
}

std::optional<SyntacticElementTarget>
ConstraintSystem::getTargetFor(SyntacticElementTargetKey key) const {
  auto known = targets.find(key);
  if (known == targets.end())
    return std::nullopt;
  return known->second;
}

std::optional<SyntacticElementTarget>
Solution::getTargetFor(SyntacticElementTargetKey key) const {
  auto known = targets.find(key);
  if (known == targets.end())
    return std::nullopt;
  return known->second;
}

namespace {

// Produce an implicit empty tuple expression.
Expr *getVoidExpr(ASTContext &ctx, SourceLoc contextLoc = SourceLoc()) {
  auto *voidExpr = TupleExpr::createEmpty(ctx,
                                          /*LParenLoc=*/contextLoc,
                                          /*RParenLoc=*/contextLoc,
                                          /*Implicit=*/true);
  voidExpr->setType(ctx.TheEmptyTupleType);
  return voidExpr;
}

/// Find any type variable references inside of an AST node.
class TypeVariableRefFinder : public ASTWalker {
  /// A stack of all closures the walker encountered so far.
  SmallVector<DeclContext *> ClosureDCs;

  ConstraintSystem &CS;
  ASTNode Parent;

  llvm::SmallPtrSetImpl<TypeVariableType *> &ReferencedVars;

public:
  TypeVariableRefFinder(
      ConstraintSystem &cs, ASTNode parent, ContextualTypeInfo context,
      llvm::SmallPtrSetImpl<TypeVariableType *> &referencedVars)
      : CS(cs), Parent(parent), ReferencedVars(referencedVars) {
    if (auto ty = context.getType())
      inferVariables(ty);
    if (auto *closure = getAsExpr<ClosureExpr>(Parent))
      ClosureDCs.push_back(closure);
  }

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *closure = dyn_cast<ClosureExpr>(expr)) {
      ClosureDCs.push_back(closure);
    }

    if (auto *joinExpr = dyn_cast<TypeJoinExpr>(expr)) {
      // If this join is over a known type, let's
      // analyze it too because it can contain type
      // variables.
      if (!joinExpr->getVar())
        inferVariables(joinExpr->getType());
    }

    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      auto *decl = DRE->getDecl();

      if (auto type = CS.getTypeIfAvailable(decl)) {
        auto &ctx = CS.getASTContext();
        // If this is not one of the closure parameters which
        // is inferrable from the body, let's replace type
        // variables with errors to avoid bringing external
        // information to the element component.
        if (type->hasTypeVariable() &&
            !(isa<ParamDecl>(decl) || decl->getName() == ctx.Id_builderSelf)) {
          // If there are type variables left in the simplified version,
          // it means that this is an invalid external declaration
          // relative to this element's context.
          if (CS.simplifyType(type)->hasTypeVariable()) {
            auto transformedTy = type.transformRec([&](Type type) -> std::optional<Type> {
              if (auto *typeVar = type->getAs<TypeVariableType>()) {
                return Type(ErrorType::get(CS.getASTContext()));
              }
              return std::nullopt;
            });

            CS.setType(decl, transformedTy);
            return Action::Continue(expr);
          }
        }

        inferVariables(type);
        return Action::Continue(expr);
      }

      auto var = dyn_cast<VarDecl>(decl);
      if (!var)
        return Action::Continue(expr);

      if (auto *wrappedVar = var->getOriginalWrappedProperty()) {
        // If there is no type it means that the body of the
        // closure hasn't been resolved yet, so we can
        // just skip it and wait for \c applyPropertyWrapperToParameter
        // to assign types.
        if (wrappedVar->hasImplicitPropertyWrapper())
          return Action::Continue(expr);

        auto outermostWrapperAttr =
            wrappedVar->getOutermostAttachedPropertyWrapper();

        // If the attribute doesn't have a type it could only mean
        // that the declaration was incorrect.
        if (!CS.hasType(outermostWrapperAttr->getTypeExpr()))
          return Action::Continue(expr);

        auto wrapperType =
            CS.simplifyType(CS.getType(outermostWrapperAttr->getTypeExpr()));

        if (var->getName().hasDollarPrefix()) {
          // $<name> is the projected value var
          CS.setType(var, computeProjectedValueType(wrappedVar, wrapperType));
        } else {
          // _<name> is the wrapper var
          CS.setType(var, wrapperType);
        }

        return Action::Continue(expr);
      }

      // If there is no type recorded yet, let's check whether
      // it is a placeholder variable implicitly generated by the
      // compiler.
      if (auto *PB = var->getParentPatternBinding()) {
        if (auto placeholderTy = isPlaceholderVar(PB)) {
          auto openedTy = CS.replaceInferableTypesWithTypeVars(
              placeholderTy, CS.getConstraintLocator(expr));
          inferVariables(openedTy);
          CS.setType(var, openedTy);
        }
      }
    }

    // If closure appears inside of a pack expansion, the elements
    // that reference pack elements have to bring expansion's shape
    // type in scope to make sure that the shapes match.
    if (auto *packElement = getAsExpr<PackElementExpr>(expr)) {
      if (auto *outerEnvironment = CS.getPackEnvironment(packElement)) {
        auto *expansionTy = CS.simplifyType(CS.getType(outerEnvironment))
                                ->castTo<PackExpansionType>();
        expansionTy->getCountType()->getTypeVariables(ReferencedVars);
      }
    }

    return Action::Continue(expr);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
    if (auto *closure = dyn_cast<ClosureExpr>(expr)) {
      ClosureDCs.pop_back();
    }
    return Action::Continue(expr);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
    // Return statements have to reference outside result type
    // since all of them are joined by it if it's not specified
    // explicitly.
    if (isa<ReturnStmt>(stmt)) {
      if (auto *closure = getAsExpr<ClosureExpr>(Parent)) {
        // Return is only viable if it belongs to a parent closure.
        if (currentClosureDC() == closure)
          inferVariables(CS.getClosureType(closure)->getResult());
      }
    }

    return Action::Continue(stmt);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    /// Decls get type-checked separately, except for PatternBindingDecls,
    /// whose initializers we want to walk into.
    return Action::VisitNodeIf(isa<PatternBindingDecl>(D));
  }

private:
  DeclContext *currentClosureDC() const {
    return ClosureDCs.empty() ? nullptr : ClosureDCs.back();
  }

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

    // Desugar type before collecting type variables, otherwise
    // we can bring in scope unrelated type variables passed
    // into the closure (via parameter/result) from contextual type.
    // For example `Typealias<$T, $U>.Context` which desugars into
    // `_Context<$U>` would bring in `$T` that could be inferrable
    // only after the body of the closure is solved.
    type = type->getCanonicalType();

    // Don't walk into the opaque archetypes because they are not
    // transparent in this context - `some P` could reference a
    // type variables as substitutions which are visible only to
    // the outer context.
    if (type->is<OpaqueTypeArchetypeType>())
      return;

    if (type->hasTypeVariable()) {
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      type->getTypeVariables(typeVars);

      // Some of the type variables could be non-representative, so
      // we need to recurse into `inferTypeVariables` to property
      // handle them.
      for (auto *typeVar : typeVars)
        inferVariables(typeVar);
    }
  }
};

// MARK: Constraint generation

/// Check whether it makes sense to convert this element into a constraint.
static bool isViableElement(ASTNode element,
                            bool isForSingleValueStmtCompletion,
                            ConstraintSystem &cs) {
  if (auto *decl = element.dyn_cast<Decl *>()) {
    // - Ignore variable declarations, they are handled by pattern bindings;
    // - Skip #warning and #error, they are handled during solution
    //   application.
    if (isa<VarDecl>(decl) || isa<PoundDiagnosticDecl>(decl))
      return false;
  }

  if (auto *stmt = element.dyn_cast<Stmt *>()) {
    if (auto *braceStmt = dyn_cast<BraceStmt>(stmt)) {
      // Empty brace statements are not viable because they do not require
      // inference.
      if (braceStmt->empty())
        return false;

      // Skip if we're doing completion for a SingleValueStmtExpr, and have a
      // brace that doesn't involve a single expression, and doesn't have a
      // code completion token, as it won't contribute to the type of the
      // SingleValueStmtExpr.
      if (isForSingleValueStmtCompletion &&
          !SingleValueStmtExpr::hasResult(braceStmt) &&
          !cs.containsIDEInspectionTarget(braceStmt)) {
        return false;
      }
    }
  }

  return true;
}

using ElementInfo = std::tuple<ASTNode, ContextualTypeInfo,
                               /*isDiscarded=*/bool, ConstraintLocator *>;

static void createConjunction(ConstraintSystem &cs, DeclContext *dc,
                              ArrayRef<ElementInfo> elements,
                              ConstraintLocator *locator, bool isIsolated,
                              ArrayRef<TypeVariableType *> extraTypeVars) {
  SmallVector<Constraint *, 4> constraints;
  SmallVector<TypeVariableType *, 2> referencedVars;
  referencedVars.append(extraTypeVars.begin(), extraTypeVars.end());

  if (locator->directlyAt<ClosureExpr>()) {
    auto *closure = castToExpr<ClosureExpr>(locator->getAnchor());
    // Conjunction associated with the body of the closure has to
    // reference a type variable representing closure type,
    // otherwise it would get disconnected from its contextual type.
    referencedVars.push_back(cs.getType(closure)->castTo<TypeVariableType>());

    // Result builder could be generic but attribute allows its use
    // in "unbound" form (i.e. `@Builder` where `Builder` is defined
    // as `struct Builder<T>`). Generic parameters of such a result
    // builder type are inferable from context, namely from `build*`
    // calls injected by the transform, and are not always resolved at
    // the time conjunction is created.
    //
    // Conjunction needs to reference all the type variables associated
    // with result builder just like parameters and result type of
    // the closure in order to stay connected to its context.
    if (auto builder = cs.getAppliedResultBuilderTransform(closure)) {
      SmallPtrSet<TypeVariableType *, 4> builderVars;
      builder->builderType->getTypeVariables(builderVars);
      referencedVars.append(builderVars.begin(), builderVars.end());
    }

    // Body of the closure is always isolated from its context, only
    // its individual elements are allowed access to type information
    // from the outside e.g. parameters/result type.
    isIsolated = true;
  }

  if (locator->isForSingleValueStmtConjunction()) {
    auto *SVE = castToExpr<SingleValueStmtExpr>(locator->getAnchor());
    referencedVars.push_back(cs.getType(SVE)->castTo<TypeVariableType>());

    // Single value statement conjunctions are always isolated, as we want to
    // solve the branches independently of the rest of the system.
    isIsolated = true;
  }

  if (locator->directlyAt<TapExpr>()) {
    // Body of the interpolation is always isolated from its context, only
    // its individual elements are allowed access to type information
    // from the outside e.g. external declaration references.
    isIsolated = true;
  }

  TypeVarRefCollector paramCollector(cs, dc, locator);

  // Whether we're doing completion, and the conjunction is for a
  // SingleValueStmtExpr, or one of its braces.
  const auto isForSingleValueStmtCompletion =
      cs.isForCodeCompletion() &&
      locator->isForSingleValueStmtConjunctionOrBrace();

  for (const auto &entry : elements) {
    ASTNode element = std::get<0>(entry);
    ContextualTypeInfo context = std::get<1>(entry);
    bool isDiscarded = std::get<2>(entry);
    ConstraintLocator *elementLoc = std::get<3>(entry);

    if (!isViableElement(element, isForSingleValueStmtCompletion, cs))
      continue;

    // If this conjunction going to represent a body of a closure,
    // let's collect references to not yet resolved outer
    // closure parameters.
    if (isIsolated)
      element.walk(paramCollector);

    constraints.push_back(Constraint::createSyntacticElement(
        cs, element, context, elementLoc, isDiscarded));
  }

  // It's possible that there are no viable elements in the body,
  // because e.g. whole body is an `#if` statement or it only has
  // declarations that are checked during solution application.
  // In such cases, let's avoid creating a conjunction.
  if (constraints.empty())
    return;

  for (auto *externalVar : paramCollector.getTypeVars())
    referencedVars.push_back(externalVar);

  cs.addUnsolvedConstraint(Constraint::createConjunction(
      cs, constraints, isIsolated, locator, referencedVars));
}

ElementInfo makeElement(ASTNode node, ConstraintLocator *locator,
                        ContextualTypeInfo context = ContextualTypeInfo(),
                        bool isDiscarded = false) {
  return std::make_tuple(node, context, isDiscarded, locator);
}

ElementInfo makeJoinElement(ConstraintSystem &cs, TypeJoinExpr *join,
                            ConstraintLocator *locator) {

  return makeElement(
      join, cs.getConstraintLocator(locator,
                                    {LocatorPathElt::SyntacticElement(join)}));
}

struct SyntacticElementContext
    : public llvm::PointerUnion<AbstractFunctionDecl *, AbstractClosureExpr *,
                                SingleValueStmtExpr *, ExprPattern *, TapExpr *> {
  // Inherit the constructors from PointerUnion.
  using PointerUnion::PointerUnion;

  /// A join that should be applied to the elements of a SingleValueStmtExpr.
  NullablePtr<TypeJoinExpr> ElementJoin;

  static SyntacticElementContext forTapExpr(TapExpr *tap) { return {tap}; }

  static SyntacticElementContext forFunctionRef(AnyFunctionRef ref) {
    if (auto *decl = ref.getAbstractFunctionDecl()) {
      return {decl};
    }

    return {ref.getAbstractClosureExpr()};
  }

  static SyntacticElementContext forClosure(ClosureExpr *closure) {
    return {closure};
  }

  static SyntacticElementContext forFunction(AbstractFunctionDecl *func) {
    return {func};
  }

  static SyntacticElementContext
  forSingleValueStmtExpr(SingleValueStmtExpr *SVE,
                         TypeJoinExpr *Join = nullptr) {
    auto context = SyntacticElementContext{SVE};
    context.ElementJoin = Join;
    return context;
  }

  static SyntacticElementContext forExprPattern(ExprPattern *EP) {
    return SyntacticElementContext{EP};
  }

  DeclContext *getAsDeclContext() const {
    if (auto *fn = this->dyn_cast<AbstractFunctionDecl *>()) {
      return fn;
    } else if (auto *closure = this->dyn_cast<AbstractClosureExpr *>()) {
      return closure;
    } else if (auto *SVE = dyn_cast<SingleValueStmtExpr *>()) {
      return SVE->getDeclContext();
    } else if (auto *EP = dyn_cast<ExprPattern *>()) {
      return EP->getDeclContext();
    } else if (auto *tap = this->dyn_cast<TapExpr *>()) {
      return tap->getVar()->getDeclContext();
    } else {
      llvm_unreachable("unsupported kind");
    }
  }

  NullablePtr<ClosureExpr> getAsClosureExpr() const {
    return dyn_cast_or_null<ClosureExpr>(
        this->dyn_cast<AbstractClosureExpr *>());
  }

  NullablePtr<AbstractClosureExpr> getAsAbstractClosureExpr() const {
    return this->dyn_cast<AbstractClosureExpr *>();
  }

  NullablePtr<AbstractFunctionDecl> getAsAbstractFunctionDecl() const {
    return this->dyn_cast<AbstractFunctionDecl *>();
  }

  NullablePtr<SingleValueStmtExpr> getAsSingleValueStmtExpr() const {
    return this->dyn_cast<SingleValueStmtExpr *>();
  }

  std::optional<AnyFunctionRef> getAsAnyFunctionRef() const {
    if (auto *fn = this->dyn_cast<AbstractFunctionDecl *>()) {
      return {fn};
    } else if (auto *closure = this->dyn_cast<AbstractClosureExpr *>()) {
      return {closure};
    } else {
      return std::nullopt;
    }
  }

  Stmt *getStmt() const {
    if (auto *fn = this->dyn_cast<AbstractFunctionDecl *>()) {
      return fn->getBody();
    } else if (auto *closure = this->dyn_cast<AbstractClosureExpr *>()) {
      return closure->getBody();
    } else if (auto *SVE = dyn_cast<SingleValueStmtExpr *>()) {
      return SVE->getStmt();
    } else if (auto *tap = this->dyn_cast<TapExpr *>()) {
      return tap->getBody();
    } else {
      llvm_unreachable("unsupported kind");
    }
  }

  bool isSingleExpressionClosure(ConstraintSystem &cs) const {
    if (auto ref = getAsAnyFunctionRef()) {
      if (cs.getAppliedResultBuilderTransform(*ref))
        return false;

      if (auto *closure = ref->getAbstractClosureExpr())
        return closure->hasSingleExpressionBody();
    }

    return false;
  }
};

/// Statement visitor that generates constraints for a given closure body.
class SyntacticElementConstraintGenerator
    : public StmtVisitor<SyntacticElementConstraintGenerator, void> {
  friend StmtVisitor<SyntacticElementConstraintGenerator, void>;

  ConstraintSystem &cs;
  SyntacticElementContext context;
  ConstraintLocator *locator;

  /// Whether a conjunction was generated.
  bool generatedConjunction = false;

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  SyntacticElementConstraintGenerator(ConstraintSystem &cs,
                                      SyntacticElementContext context,
                                      ConstraintLocator *locator)
      : cs(cs), context(context), locator(locator) {}

  void createConjunction(ArrayRef<ElementInfo> elements,
                         ConstraintLocator *locator, bool isIsolated = false,
                         ArrayRef<TypeVariableType *> extraTypeVars = {}) {
    assert(!generatedConjunction && "Already generated conjunction");
    generatedConjunction = true;

    // Inject a join if we have one.
    SmallVector<ElementInfo, 4> scratch;
    if (auto *join = context.ElementJoin.getPtrOrNull()) {
      scratch.append(elements.begin(), elements.end());
      scratch.push_back(makeJoinElement(cs, join, locator));
      elements = scratch;
    }
    ::createConjunction(cs, context.getAsDeclContext(), elements, locator,
                        isIsolated, extraTypeVars);
  }

  void visitExprPattern(ExprPattern *EP) {
    auto target = SyntacticElementTarget::forExprPattern(EP);

    if (cs.preCheckTarget(target)) {
      hadError = true;
      return;
    }
    cs.setType(EP->getMatchVar(), cs.getType(EP));

    if (cs.generateConstraints(target)) {
      hadError = true;
      return;
    }
    cs.setTargetFor(EP, target);
    cs.setExprPatternFor(EP->getSubExpr(), EP);
  }

  void visitPattern(Pattern *pattern, ContextualTypeInfo contextInfo) {
    if (context.is<ExprPattern *>()) {
      // This is for an ExprPattern conjunction, go ahead and generate
      // constraints for the match expression.
      visitExprPattern(cast<ExprPattern>(pattern));
      return;
    }

    auto parentElement =
        locator->getLastElementAs<LocatorPathElt::SyntacticElement>();

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
        visitCaseItemPattern(pattern, contextInfo);
        return;
      }
    }

    llvm_unreachable("Unsupported pattern");
  }

  void visitCaseItem(CaseLabelItem *caseItem, ContextualTypeInfo contextInfo) {
    assert(contextInfo.purpose == CTP_CaseStmt);

    auto *DC = context.getAsDeclContext();
    auto &ctx = DC->getASTContext();

    // Resolve the pattern.
    auto *pattern = caseItem->getPattern();
    if (!caseItem->isPatternResolved()) {
      pattern = TypeChecker::resolvePattern(pattern, context.getAsDeclContext(),
                                            /*isStmtCondition=*/false);
      if (!pattern) {
        hadError = true;
        return;
      }

      caseItem->setPattern(pattern, /*resolved=*/true);
    }

    // Let's generate constraints for pattern + where clause.
    // The assumption is that this shouldn't be too complex
    // to handle, but if it turns out to be false, this could
    // always be converted into a conjunction.

    // Generate constraints for pattern.
    visitPattern(pattern, contextInfo);

    auto *guardExpr = caseItem->getGuardExpr();

    // Generate constraints for `where` clause (if any).
    if (guardExpr) {
      SyntacticElementTarget guardTarget(
          guardExpr, DC, CTP_Condition, ctx.getBoolType(), /*discarded*/ false);

      if (cs.generateConstraints(guardTarget)) {
        hadError = true;
        return;
      }
      guardExpr = guardTarget.getAsExpr();
      cs.setTargetFor(guardExpr, guardTarget);
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
    // The `where` clause should be ignored because \c visitForEachStmt
    // records it as a separate conjunction element to allow for a more
    // granular control over what contextual information is brought into
    // the scope during pattern + sequence and `where` clause solving.
    auto target = SyntacticElementTarget::forForEachPreamble(
        forEachStmt, context.getAsDeclContext(),
        /*ignoreWhereClause=*/true);

    if (cs.generateConstraints(target)) {
      hadError = true;
      return;
    }

    // After successful constraint generation, let's record
    // syntactic element target with all relevant information.
    cs.setTargetFor(forEachStmt, target);
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
    auto *loc = cs.getConstraintLocator(
        locator, {LocatorPathElt::PatternMatch(pattern),
                  LocatorPathElt::ContextualType(context.purpose)});
    cs.addConstraint(ConstraintKind::Equal, context.getType(), patternType,
                     loc);

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
        locator, LocatorPathElt::SyntacticElement(patternBinding));

    for (unsigned index : range(patternBinding->getNumPatternEntries())) {
      if (patternBinding->isInitializerChecked(index))
        continue;

      auto *pattern = TypeChecker::resolvePattern(
          patternBinding->getPattern(index), patternBinding->getDeclContext(),
          /*isStmtCondition=*/true);

      if (!pattern) {
        hadError = true;
        return;
      }

      // Reset binding to point to the resolved pattern. This is required
      // before calling `forPatternBindingDecl`.
      patternBinding->setPattern(index, pattern);

      patterns.push_back(makeElement(
          patternBinding,
          cs.getConstraintLocator(
              baseLoc, LocatorPathElt::PatternBindingElement(index))));
    }
  }

  std::optional<SyntacticElementTarget>
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

    // A property wrapper initializer (either user-defined
    // or a synthesized one) has to be pre-checked before use.
    //
    // This is not a problem in top-level code because pattern
    // bindings go through `typeCheckExpression` which does
    // pre-check automatically and result builders do not allow
    // declaring local wrapped variables (yet).
    if (hasPropertyWrapper(pattern)) {
      auto target = SyntacticElementTarget::forInitialization(
          init, patternType, patternBinding, index,
          /*bindPatternVarsOneWay=*/false);

      if (ConstraintSystem::preCheckTarget(target))
        return std::nullopt;

      return target;
    }

    if (init) {
      return SyntacticElementTarget::forInitialization(
          init, patternType, patternBinding, index,
          /*bindPatternVarsOneWay=*/false);
    }

    return SyntacticElementTarget::forUninitializedVar(patternBinding, index,
                                                       patternType);
  }

  void visitPatternBindingElement(PatternBindingDecl *patternBinding) {
    assert(locator->isLastElement<LocatorPathElt::PatternBindingElement>());

    auto index =
        locator->castLastElementTo<LocatorPathElt::PatternBindingElement>()
            .getIndex();

    if (patternBinding->isInitializerChecked(index))
      return;

    auto contextualPattern =
        ContextualPattern::forPatternBindingDecl(patternBinding, index);
    Type patternType = TypeChecker::typeCheckPattern(contextualPattern);

    // Fail early if pattern couldn't be type-checked.
    if (!patternType || patternType->hasError()) {
      hadError = true;
      return;
    }

    auto target = getTargetForPattern(patternBinding, index, patternType);
    if (!target) {
      hadError = true;
      return;
    }

    // Keep track of this binding entry.
    cs.setTargetFor({patternBinding, index}, *target);

    if (isPlaceholderVar(patternBinding))
      return;

    if (cs.generateConstraints(*target)) {
      hadError = true;
      return;
    }
  }

  void visitDecl(Decl *decl) {
    if (!context.isSingleExpressionClosure(cs)) {
      if (auto patternBinding = dyn_cast<PatternBindingDecl>(decl)) {
        if (locator->isLastElement<LocatorPathElt::PatternBindingElement>())
          visitPatternBindingElement(patternBinding);
        else
          llvm_unreachable("cannot visit pattern binding directly");
        return;
      }
    }

    // Skip #warning/#error; we'll handle them when applying the closure.
    if (isa<PoundDiagnosticDecl>(decl))
      return;

    // Ignore variable declarations, because they're always handled within
    // their enclosing pattern bindings.
    if (isa<VarDecl>(decl))
      return;

    // Other declarations will be handled at application time.
  }

  // These statements don't require any type-checking.
  void visitBreakStmt(BreakStmt *breakStmt) {}
  void visitContinueStmt(ContinueStmt *continueStmt) {}
  void visitDeferStmt(DeferStmt *deferStmt) {}
  void visitFallthroughStmt(FallthroughStmt *fallthroughStmt) {}
  void visitFailStmt(FailStmt *fail) {}

  void visitStmtCondition(LabeledConditionalStmt *S,
                          SmallVectorImpl<ElementInfo> &elements,
                          ConstraintLocator *locator) {
    auto *condLocator =
        cs.getConstraintLocator(locator, ConstraintLocator::Condition);
    for (auto &condition : S->getCond())
      elements.push_back(makeElement(&condition, condLocator));
  }

  void visitIfStmt(IfStmt *ifStmt) {
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

    createConjunction(elements, locator);
  }

  void visitGuardStmt(GuardStmt *guardStmt) {
    SmallVector<ElementInfo, 4> elements;

    visitStmtCondition(guardStmt, elements, locator);
    elements.push_back(makeElement(guardStmt->getBody(), locator));

    createConjunction(elements, locator);
  }

  void visitWhileStmt(WhileStmt *whileStmt) {
    SmallVector<ElementInfo, 4> elements;

    visitStmtCondition(whileStmt, elements, locator);
    elements.push_back(makeElement(whileStmt->getBody(), locator));

    createConjunction(elements, locator);
  }

  void visitDoStmt(DoStmt *doStmt) {
    visitBraceStmt(doStmt->getBody());
  }

  void visitRepeatWhileStmt(RepeatWhileStmt *repeatWhileStmt) {
    createConjunction({makeElement(repeatWhileStmt->getCond(),
                                   cs.getConstraintLocator(
                                       locator, ConstraintLocator::Condition),
                                   getContextForCondition()),
                       makeElement(repeatWhileStmt->getBody(), locator)},
                      locator);
  }

  void visitPoundAssertStmt(PoundAssertStmt *poundAssertStmt) {
    createConjunction({makeElement(poundAssertStmt->getCondition(),
                                   cs.getConstraintLocator(
                                       locator, ConstraintLocator::Condition),
                                   getContextForCondition())},
                      locator);
  }

  void visitThrowStmt(ThrowStmt *throwStmt) {
    // Look up the catch node for this "throw" to determine the error type.
    auto dc = context.getAsDeclContext();
    auto module = dc->getParentModule();
    auto throwLoc = throwStmt->getThrowLoc();
    Type errorType;
    if (auto catchNode = ASTScope::lookupCatchNode(module, throwLoc))
      errorType = catchNode.getExplicitCaughtType(cs.getASTContext());

    if (!errorType) {
      if (!cs.getASTContext().getErrorDecl()) {
        hadError = true;
        return;
      }

      errorType = cs.getASTContext().getErrorExistentialType();
    }

    auto *errorExpr = throwStmt->getSubExpr();

    createConjunction(
        {makeElement(errorExpr,
                     cs.getConstraintLocator(
                         locator, LocatorPathElt::SyntacticElement(errorExpr)),
                     {errorType, CTP_ThrowStmt})},
        locator);
  }

  void visitDiscardStmt(DiscardStmt *discardStmt) {
    auto *fn = discardStmt->getInnermostMethodContext();
    if (!fn) {
      hadError = true;
      return;
    }

    auto nominalType =
        fn->getDeclContext()->getSelfNominalTypeDecl()->getDeclaredType();
    if (!nominalType) {
      hadError = true;
      return;
    }

    auto *selfExpr = discardStmt->getSubExpr();

    createConjunction(
        {makeElement(selfExpr,
                     cs.getConstraintLocator(
                         locator, LocatorPathElt::SyntacticElement(selfExpr)),
                     {nominalType, CTP_DiscardStmt})},
        locator);
  }

  void visitForEachStmt(ForEachStmt *forEachStmt) {
    auto *stmtLoc = cs.getConstraintLocator(locator);

    SmallVector<ElementInfo, 4> elements;

    // For-each pattern.
    //
    // Note that we don't record a sequence here, it would be handled
    // together with pattern because pattern can inform a type of sequence
    // element e.g. `for i: Int8 in 0 ..< 8`
    elements.push_back(makeElement(forEachStmt->getPattern(), stmtLoc));

    // Where clause if any.
    if (auto *where = forEachStmt->getWhere()) {
      Type boolType = cs.getASTContext().getBoolType();
      if (!boolType) {
        hadError = true;
        return;
      }

      ContextualTypeInfo context(boolType, CTP_Condition);
      elements.push_back(
          makeElement(where, stmtLoc, context, /*isDiscarded=*/false));
    }

    // Body of the `for-in` loop.
    elements.push_back(makeElement(forEachStmt->getBody(), stmtLoc));

    createConjunction(elements, locator);
  }

  void visitSwitchStmt(SwitchStmt *switchStmt) {
    SmallVector<ElementInfo, 4> elements;
    {
      auto *subjectExpr = switchStmt->getSubjectExpr();
      {
        elements.push_back(makeElement(subjectExpr, locator));

        SyntacticElementTarget target(subjectExpr, context.getAsDeclContext(),
                                      CTP_Unused, Type(),
                                      /*isDiscarded=*/false);

        cs.setTargetFor(switchStmt, target);
      }

      for (auto rawCase : switchStmt->getRawCases())
        elements.push_back(makeElement(rawCase, locator));
    }

    createConjunction(elements, locator);
  }

  void visitDoCatchStmt(DoCatchStmt *doStmt) {
    SmallVector<ElementInfo, 4> elements;

    // First, let's record a body of `do` statement. Note we need to add a
    // SyntaticElement locator path element here to avoid treating the inner
    // brace conjunction as being isolated if 'doLoc' is for an isolated
    // conjunction (as is the case with 'do' expressions).
    auto *doBodyLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::SyntacticElement(doStmt->getBody()));
    elements.push_back(makeElement(doStmt->getBody(), doBodyLoc));

    // After that has been type-checked, let's switch to
    // individual `catch` statements.
    for (auto *catchStmt : doStmt->getCatches())
      elements.push_back(makeElement(catchStmt, locator));

    createConjunction(elements, locator);
  }

  void visitCaseStmt(CaseStmt *caseStmt) {
    Type contextualTy;

    {
      auto parent =
          locator->castLastElementTo<LocatorPathElt::SyntacticElement>()
              .getElement();

      if (parent.isStmt(StmtKind::Switch)) {
        auto *switchStmt = cast<SwitchStmt>(parent.get<Stmt *>());
        contextualTy = cs.getType(switchStmt->getSubjectExpr());
      } else if (auto doCatch =
                     dyn_cast_or_null<DoCatchStmt>(parent.dyn_cast<Stmt *>())) {
        contextualTy = cs.getCaughtErrorType(doCatch);

        // A non-exhaustive do..catch statement is a potential throw site.
        if (caseStmt == doCatch->getCatches().back() &&
            !doCatch->isSyntacticallyExhaustive()) {
          cs.recordPotentialThrowSite(
              PotentialThrowSite::NonExhaustiveDoCatch, contextualTy,
              cs.getConstraintLocator(doCatch));
        }
      } else {
        hadError = true;
        return;
      }
    }

    auto *caseLoc = cs.getConstraintLocator(
        locator, LocatorPathElt::SyntacticElement(caseStmt));

    SmallVector<ElementInfo, 4> elements;
    for (auto &caseLabelItem : caseStmt->getMutableCaseLabelItems()) {
      elements.push_back(
          makeElement(&caseLabelItem, caseLoc, {contextualTy, CTP_CaseStmt}));
    }

    elements.push_back(makeElement(caseStmt->getBody(), caseLoc));

    createConjunction(elements, caseLoc);
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
    auto &ctx = cs.getASTContext();

    CaptureListExpr *captureList = nullptr;
    {
      if (locator->directlyAt<ClosureExpr>()) {
        auto *closure = castToExpr<ClosureExpr>(locator->getAnchor());
        captureList = getAsExpr<CaptureListExpr>(cs.getParentExpr(closure));
      }
    }

    if (context.isSingleExpressionClosure(cs)) {
      // Generate constraints for the capture list first.
      //
      // TODO: This should be a conjunction connected to
      // the closure body to make sure that each capture
      // is solved in isolation.
      if (captureList) {
        for (const auto &capture : captureList->getCaptureList()) {
          SyntacticElementTarget target(capture.PBD);
          if (cs.generateConstraints(target)) {
            hadError = true;
            return;
          }
        }
      }

      for (auto node : braceStmt->getElements()) {
        if (auto expr = node.dyn_cast<Expr *>()) {
          auto generatedExpr =
              cs.generateConstraints(expr, context.getAsDeclContext());
          if (!generatedExpr) {
            hadError = true;
          }
        } else if (auto stmt = node.dyn_cast<Stmt *>()) {
          visit(stmt);
        } else {
          visitDecl(node.get<Decl *>());
        }
      }
      return;
    }

    SmallVector<ElementInfo, 4> elements;

    // If this brace statement represents a body of an empty or
    // multi-statement closure.
    if (locator->directlyAt<ClosureExpr>()) {
      auto *closure = context.getAsClosureExpr().get();
      // If this closure has an empty body or no `return` statements with
      // results let's bind result type to `Void` since that's the only type
      // empty body can produce.
      //
      // Note that result builder bodies always have a `return` statement
      // at the end, so they don't need to be defaulted.
      if (!cs.getAppliedResultBuilderTransform({closure}) &&
          !hasResultExpr(closure)) {
        auto constraintKind =
            (closure->hasEmptyBody() && !closure->hasExplicitResultType())
                ? ConstraintKind::Bind
                : ConstraintKind::Defaultable;

        cs.addConstraint(
            constraintKind, cs.getClosureType(closure)->getResult(),
            ctx.TheEmptyTupleType,
            cs.getConstraintLocator(closure, ConstraintLocator::ClosureResult));
      }

      // If this multi-statement closure has captures, let's solve
      // them first.
      if (captureList) {
        for (const auto &capture : captureList->getCaptureList())
          visitPatternBinding(capture.PBD, elements);
      }
    }

    if (isChildOf(StmtKind::Case)) {
      auto *caseStmt = cast<CaseStmt>(
          locator->castLastElementTo<LocatorPathElt::SyntacticElement>()
              .asStmt());

      if (recordInferredSwitchCasePatternVars(caseStmt)) {
        hadError = true;
      }
    }

    for (auto element : braceStmt->getElements()) {
      if (cs.isForCodeCompletion() &&
          !cs.containsIDEInspectionTarget(element)) {
        // To improve performance, skip type checking elements that can't
        // influence the code completion token.
        if (element.is<Stmt *>() &&
            !element.isStmt(StmtKind::Guard) &&
            !element.isStmt(StmtKind::Return) &&
            !element.isStmt(StmtKind::Then)) {
          // Statements can't influence the expresion that contains the code
          // completion token.
          // Guard statements might define variables that are used in the code
          // completion expression. Don't skip them.
          // Return statements influence the type of the closure itself. Don't
          // skip them either.
          continue;
        }
        if (element.isExpr(ExprKind::Assign)) {
          // Assignments are also similar to statements and can't influence the
          // code completion token.
          continue;
        }
        if (element.isExpr(ExprKind::Error)) {
          // ErrorExpr can't influcence the expresssion that contains the code
          // completion token. Since they are causing type checking to abort
          // early, just skip them.
          continue;
        }
      }

      if (auto *decl = element.dyn_cast<Decl *>()) {
        if (auto *PDB = dyn_cast<PatternBindingDecl>(decl)) {
          visitPatternBinding(PDB, elements);
          continue;
        }
      }

      bool isDiscarded = false;
      auto contextInfo = cs.getContextualTypeInfo(element);

      if (element.is<Expr *>() &&
          !ctx.LangOpts.Playground && !ctx.LangOpts.DebuggerSupport) {
        isDiscarded = !contextInfo || contextInfo->purpose == CTP_Unused;
      }

      elements.push_back(makeElement(
          element,
          cs.getConstraintLocator(locator,
                                  LocatorPathElt::SyntacticElement(element)),
          contextInfo.value_or(ContextualTypeInfo()), isDiscarded));
    }

    createConjunction(elements, locator);
  }

  void visitReturnStmt(ReturnStmt *returnStmt) {
    // Record an implied result if we have one.
    if (returnStmt->isImplied()) {
      auto kind = context.getAsClosureExpr() ? ImpliedResultKind::ForClosure
                                             : ImpliedResultKind::Regular;
      auto *result = returnStmt->getResult();
      cs.recordImpliedResult(result, kind);
    }

    Expr *resultExpr;
    if (returnStmt->hasResult()) {
      resultExpr = returnStmt->getResult();
      assert(resultExpr && "non-empty result without expression?");
    } else {
      // If this is simplify `return`, let's create an empty tuple
      // which is also useful if contextual turns out to be e.g. `Void?`.
      // Also, attach return stmt source location so if there is a contextual
      // mismatch we can produce a diagnostic in a valid source location.
      resultExpr = getVoidExpr(cs.getASTContext(), returnStmt->getEndLoc());
    }

    auto contextualResultInfo = getContextualResultInfoFor(returnStmt);

    SyntacticElementTarget target(resultExpr, context.getAsDeclContext(),
                                  contextualResultInfo, /*isDiscarded=*/false);

    if (cs.generateConstraints(target)) {
      hadError = true;
      return;
    }

    cs.setContextualInfo(target.getAsExpr(), contextualResultInfo);
    cs.setTargetFor(returnStmt, target);
  }

  void visitThenStmt(ThenStmt *thenStmt) {
    auto *resultExpr = thenStmt->getResult();
    auto contextInfo = cs.getContextualTypeInfo(resultExpr);

    // First check to make sure the ThenStmt is in a valid position.
    SmallVector<ThenStmt *, 4> validThenStmts;
    if (auto SVE = context.getAsSingleValueStmtExpr())
      (void)SVE.get()->getThenStmts(validThenStmts);

    if (!llvm::is_contained(validThenStmts, thenStmt)) {
      auto *thenLoc = cs.getConstraintLocator(thenStmt);
      (void)cs.recordFix(IgnoreOutOfPlaceThenStmt::create(cs, thenLoc));
    }

    // For an if/switch expression, if the contextual type for the branch is
    // still a type variable, we can drop it. This avoids needlessly
    // propagating the type of the branch to subsequent branches, instead
    // we'll let the join handle the conversion.
    if (contextInfo) {
      auto contextualFixedTy =
          cs.getFixedTypeRecursive(contextInfo->getType(), /*wantRValue*/ true);
      if (contextualFixedTy->isTypeVariableOrMember())
        contextInfo = std::nullopt;
    }

    // We form a single element conjunction here to ensure the context type var
    // gets taken out of the active type vars (assuming we dropped it) before we
    // produce a solution.
    auto resultElt = makeElement(resultExpr, locator,
                                 contextInfo.value_or(ContextualTypeInfo()),
                                 /*isDiscarded=*/false);
    createConjunction({resultElt}, locator);
  }

  ContextualTypeInfo getContextualResultInfoFor(ReturnStmt *returnStmt) const {
    auto funcRef = AnyFunctionRef::fromDeclContext(context.getAsDeclContext());
    if (!funcRef)
      return {Type(), CTP_Unused};

    if (auto transform = cs.getAppliedResultBuilderTransform(*funcRef))
      return {transform->bodyResultType, CTP_ReturnStmt};

    if (auto *closure =
            getAsExpr<ClosureExpr>(funcRef->getAbstractClosureExpr())) {
      // Single-expression closures need their contextual type locator anchored
      // on the closure itself. Otherwise we use the default contextual type
      // locator, which will be created for us.
      ConstraintLocator *loc = nullptr;
      if (context.isSingleExpressionClosure(cs) && returnStmt->hasResult())
        loc = cs.getConstraintLocator(closure, {LocatorPathElt::ClosureBody()});

      return {cs.getClosureType(closure)->getResult(), CTP_ClosureResult, loc};
    }

    return {funcRef->getBodyResultType(), CTP_ReturnStmt};
  }

#define UNSUPPORTED_STMT(STMT) void visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
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
        locator->getLastElementAs<LocatorPathElt::SyntacticElement>();
    return parentElt ? parentElt->getElement().isStmt(kind) : false;
  }

  bool recordInferredSwitchCasePatternVars(CaseStmt *caseStmt) {
    llvm::SmallDenseMap<Identifier, SmallVector<VarDecl *, 2>, 4> patternVars;

    auto recordVar = [&](VarDecl *var) {
      if (!var->hasName())
        return;
      patternVars[var->getName()].push_back(var);
    };

    for (auto &caseItem : caseStmt->getMutableCaseLabelItems()) {
      assert(caseItem.isPatternResolved());

      auto *pattern = caseItem.getPattern();
      pattern->forEachVariable([&](VarDecl *var) { recordVar(var); });
    }

    for (auto bodyVar : caseStmt->getCaseBodyVariablesOrEmptyArray()) {
      if (!bodyVar->hasName())
        continue;

      const auto &variants = patternVars[bodyVar->getName()];

      auto getType = [&](VarDecl *var) {
        auto type = cs.simplifyType(cs.getType(var));
        assert(!type->hasTypeVariable());
        return type;
      };

      switch (variants.size()) {
      case 0:
        break;

      case 1:
        // If there is only one choice here, let's use it directly.
        cs.setType(bodyVar, getType(variants.front()));
        break;

      default: {
        // If there are multiple choices it could only mean multiple
        // patterns e.g. `.a(let x), .b(let x), ...:`. Let's join them.
        Type joinType = getType(variants.front());

        SmallVector<VarDecl *, 2> conflicts;
        for (auto *var : llvm::drop_begin(variants)) {
          auto varType = getType(var);
          // Type mismatch between different patterns.
          if (!joinType->isEqual(varType))
            conflicts.push_back(var);
        }

        if (!conflicts.empty()) {
          if (!cs.shouldAttemptFixes())
            return true;

          // dfdf
          auto *locator = cs.getConstraintLocator(bodyVar);
          if (cs.recordFix(RenameConflictingPatternVariables::create(
                  cs, joinType, conflicts, locator)))
            return true;
        }

        cs.setType(bodyVar, joinType);
      }
      }
    }

    return false;
  }
};
}

bool ConstraintSystem::generateConstraints(TapExpr *tap) {
  SyntacticElementConstraintGenerator generator(
      *this, SyntacticElementContext::forTapExpr(tap),
      getConstraintLocator(tap));

  auto *body = tap->getBody();

  if (!body) {
    assert(tap->getSubExpr());
    return false;
  }

  generator.visit(tap->getBody());
  return generator.hadError;
}

bool ConstraintSystem::generateConstraints(AnyFunctionRef fn, BraceStmt *body) {
  NullablePtr<ConstraintLocator> locator;

  if (auto *func = fn.getAbstractFunctionDecl()) {
    locator = getConstraintLocator(func);
  } else {
    locator = getConstraintLocator(fn.getAbstractClosureExpr());
  }

  SyntacticElementConstraintGenerator generator(
      *this, SyntacticElementContext::forFunctionRef(fn), locator.get());

  generator.visit(body);

  return generator.hadError;
}

bool ConstraintSystem::generateConstraints(SingleValueStmtExpr *E) {
  auto *S = E->getStmt();
  auto &ctx = getASTContext();

  auto *loc = getConstraintLocator(E);
  Type resultTy = createTypeVariable(loc, /*options*/ 0);
  setType(E, resultTy);

  // Propagate the implied result kind from the if/switch expression itself
  // into the branches.
  auto impliedResultKind =
      isImpliedResult(E).value_or(ImpliedResultKind::Regular);

  // Assign contextual types for each of the result exprs.
  SmallVector<ThenStmt *, 4> scratch;
  auto branches = E->getThenStmts(scratch);
  for (auto idx : indices(branches)) {
    auto *thenStmt = branches[idx];
    auto *result = thenStmt->getResult();

    // If we have an implicit 'then' statement, record it as an implied result.
    // TODO: Should we track 'implied' as a separate bit on ThenStmt? Currently
    // it's the same as being implicit, but may not always be.
    if (thenStmt->isImplicit())
      recordImpliedResult(result, impliedResultKind);

    auto ctpElt = LocatorPathElt::ContextualType(CTP_SingleValueStmtBranch);
    auto *loc = getConstraintLocator(
        E, {LocatorPathElt::SingleValueStmtResult(idx), ctpElt});

    ContextualTypeInfo info(resultTy, CTP_SingleValueStmtBranch, loc);
    setContextualInfo(result, info);
  }

  TypeJoinExpr *join = nullptr;
  if (branches.empty()) {
    // If we only have statement branches, the expression is typed as Void. This
    // should only be the case for 'if' and 'switch' statements that must be
    // expressions that have branches that all end in a throw, and we'll warn
    // that we've inferred Void.
    addConstraint(ConstraintKind::Bind, resultTy, ctx.getVoidType(), loc);
  } else {
    // Otherwise, we join the result types for each of the branches.
    join = TypeJoinExpr::forBranchesOfSingleValueStmtExpr(
        ctx, resultTy, E, AllocationArena::ConstraintSolver);
  }

  // If this is an implied return in a closure, we need to account for the fact
  // that the result type may be bound to Void. This is necessary to correctly
  // handle the following case:
  //
  // func foo<T>(_ fn: () -> T) {}
  // foo {
  //   if .random() { 0 } else { "" }
  // }
  //
  // Before if/switch expressions, this was treated as a regular statement,
  // with the branches being discarded (and we'd warn). We need to ensure we
  // maintain compatibility by continuing to infer T as Void in the case where
  // the branches mismatch. This example is contrived, but can occur in the real
  // world with e.g branches that insert and remove elements from a set, in both
  // cases the methods have mismatching discardable returns.
  //
  // To maintain this behavior, form a disjunction that will attempt to either
  // bind the expression type to the closure result type, or bind it to Void.
  // Only if we fail to solve with the closure result type will we attempt with
  // Void. We can't rely on the usual defaulting of the closure result type,
  // as we need to solve the conjunction before trying defaults.
  //
  // This only needs to happen for cases where the return is implicit, we don't
  // need to do this with 'return if'. We also don't need to do it for function
  // decls, as we proactively avoid transforming the if/switch into an
  // expression if the result is known to be Void.
  if (impliedResultKind == ImpliedResultKind::ForClosure) {
    auto *CE = cast<ClosureExpr>(E->getDeclContext());
    assert(!getAppliedResultBuilderTransform(CE) &&
           "Should have applied the builder with statement semantics");
    if (getParentExpr(E) == CE) {
      // We may not have a closure type if we're solving a sub-expression
      // independently for e.g code completion.
      // TODO: This won't be necessary once we stop doing the fallback
      // type-check.
      if (auto *closureTy = getClosureTypeIfAvailable(CE)) {
        auto closureResultTy = closureTy->getResult();
        auto *bindToClosure = Constraint::create(
            *this, ConstraintKind::Bind, resultTy, closureResultTy, loc);
        bindToClosure->setFavored();

        auto *bindToVoid = Constraint::create(*this, ConstraintKind::Bind,
                                              resultTy, ctx.getVoidType(), loc);

        addDisjunctionConstraint({bindToClosure, bindToVoid}, loc);
      }
    }
  }

  // Generate the conjunction for the branches.
  auto context = SyntacticElementContext::forSingleValueStmtExpr(E, join);
  auto *stmtLoc =
      getConstraintLocator(loc, LocatorPathElt::SyntacticElement(S));
  SyntacticElementConstraintGenerator generator(*this, context, stmtLoc);
  generator.visit(S);
  return generator.hadError;
}

void ConstraintSystem::generateConstraints(ArrayRef<ExprPattern *> exprPatterns,
                                           ConstraintLocatorBuilder locator) {
  assert(!exprPatterns.empty());
  auto *DC = exprPatterns.front()->getDeclContext();

  // Form a conjunction of ExprPattern elements, isolated from the rest of the
  // pattern.
  SmallVector<ElementInfo> elements;
  SmallVector<TypeVariableType *, 2> referencedTypeVars;
  for (auto *EP : exprPatterns) {
    auto ty = getType(EP)->castTo<TypeVariableType>();
    referencedTypeVars.push_back(ty);

    ContextualTypeInfo context(ty, CTP_ExprPattern);
    elements.push_back(makeElement(EP, getConstraintLocator(EP), context));
  }
  auto *loc = getConstraintLocator(locator);
  createConjunction(*this, DC, elements, loc, /*isIsolated*/ true,
                    referencedTypeVars);
}

bool isConditionOfStmt(ConstraintLocatorBuilder locator) {
  if (!locator.endsWith<LocatorPathElt::Condition>())
    return false;

  SmallVector<LocatorPathElt, 4> path;
  (void)locator.getLocatorParts(path);

  path.pop_back();

  if (path.empty())
    return false;

  if (auto closureElt = path.back().getAs<LocatorPathElt::SyntacticElement>())
    return closureElt->getElement().dyn_cast<Stmt *>();

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::simplifySyntacticElementConstraint(
    ASTNode element, ContextualTypeInfo contextInfo, bool isDiscarded,
    TypeMatchOptions flags, ConstraintLocatorBuilder locator) {
  auto anchor = locator.getAnchor();

  std::optional<SyntacticElementContext> context;
  if (auto *closure = getAsExpr<ClosureExpr>(anchor)) {
    context = SyntacticElementContext::forClosure(closure);
  } else if (auto *fn = getAsDecl<AbstractFunctionDecl>(anchor)) {
    context = SyntacticElementContext::forFunction(fn);
  } else if (auto *SVE = getAsExpr<SingleValueStmtExpr>(anchor)) {
    context = SyntacticElementContext::forSingleValueStmtExpr(SVE);
  } else if (auto *EP = getAsPattern<ExprPattern>(anchor)) {
    context = SyntacticElementContext::forExprPattern(EP);
  } else if (auto *tap = getAsExpr<TapExpr>(anchor)) {
    context = SyntacticElementContext::forTapExpr(tap);
  } else {
    return SolutionKind::Error;
  }

  SyntacticElementConstraintGenerator generator(*this, *context,
                                                getConstraintLocator(locator));

  if (auto *expr = element.dyn_cast<Expr *>()) {
    SyntacticElementTarget target(expr, context->getAsDeclContext(),
                                  contextInfo, isDiscarded);

    if (generateConstraints(target))
      return SolutionKind::Error;

    // If this expression is the operand of a `throw` statement, record it as
    // a potential throw site.
    if (contextInfo.purpose == CTP_ThrowStmt) {
      recordPotentialThrowSite(PotentialThrowSite::ExplicitThrow,
                               getType(expr), getConstraintLocator(expr));
    }

    setTargetFor(expr, target);
    return SolutionKind::Solved;
  } else if (auto *stmt = element.dyn_cast<Stmt *>()) {
    generator.visit(stmt);
  } else if (auto *cond = element.dyn_cast<StmtConditionElement *>()) {
    if (generateConstraints({*cond}, context->getAsDeclContext()))
      return SolutionKind::Error;
  } else if (auto *pattern = element.dyn_cast<Pattern *>()) {
    generator.visitPattern(pattern, contextInfo);
  } else if (auto *caseItem = element.dyn_cast<CaseLabelItem *>()) {
    generator.visitCaseItem(caseItem, contextInfo);
  } else {
    generator.visit(element.get<Decl *>());
  }

  return generator.hadError ? SolutionKind::Error : SolutionKind::Solved;
}

// MARK: Solution application

namespace {

/// Statement visitor that applies constraints for a given closure body.
class SyntacticElementSolutionApplication
    : public StmtVisitor<SyntacticElementSolutionApplication, ASTNode> {
  friend StmtVisitor<SyntacticElementSolutionApplication, ASTNode>;
  friend class ResultBuilderRewriter;

protected:
  Solution &solution;
  SyntacticElementContext context;
  SyntacticElementTargetRewriter &rewriter;

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  SyntacticElementSolutionApplication(Solution &solution,
                                      SyntacticElementContext context,
                                      SyntacticElementTargetRewriter &rewriter)
      : solution(solution), context(context), rewriter(rewriter) {}

  virtual ~SyntacticElementSolutionApplication() {}

private:
  Type getContextualResultType() const {
    // Taps do not have a contextual result type.
    if (context.is<TapExpr *>()) {
      return Type();
    }

    auto fn = context.getAsAnyFunctionRef();

    if (context.is<SingleValueStmtExpr *>()) {
      // if/switch expressions can have `return` inside.
      fn = AnyFunctionRef::fromDeclContext(context.getAsDeclContext());
    }

    if (fn) {
      if (auto transform = solution.getAppliedBuilderTransform(*fn)) {
        return solution.simplifyType(transform->bodyResultType);
      } else if (auto *closure =
                     getAsExpr<ClosureExpr>(fn->getAbstractClosureExpr())) {
        return solution.getResolvedType(closure)
            ->castTo<FunctionType>()
            ->getResult();
      } else {
        return fn->getBodyResultType();
      }
    }

    return Type();
  }

  bool visitPatternBindingDecl(PatternBindingDecl *PBD) {
    // If this is a placeholder variable with an initializer, we just need to
    // set the inferred type.
    if (isPlaceholderVar(PBD) && PBD->getInit(0)) {
      auto *pattern = PBD->getPattern(0);
      pattern->setType(solution.getResolvedType(PBD->getSingleVar()));
      return false;
    }

    SyntacticElementTarget target(PBD);
    return !rewriter.rewriteTarget(target).has_value();
  }

  void visitDecl(Decl *decl) {
    if (auto *PBD = dyn_cast<PatternBindingDecl>(decl)) {
      if (visitPatternBindingDecl(PBD)) {
        hadError = true;
        return;
      }
      // Fall through to allow `typeCheckDecl` to be called after solution is
      // applied to a pattern binding. That will materialize required
      // information e.g. accessors and do access/availability checks.
    }

    // Delay the type-checking of local decls to ensure that parent closures
    // have solutions applied, which is needed by MiscDiagnostics passes such as
    // `diagnoseImplicitSelfUseInClosure`
    rewriter.addLocalDeclToTypeCheck(decl);
  }

  ASTNode visitBreakStmt(BreakStmt *breakStmt) {
    // Force the target to be computed in case it produces diagnostics.
    (void)breakStmt->getTarget();
    return breakStmt;
  }

  ASTNode visitContinueStmt(ContinueStmt *continueStmt) {
    // Force the target to be computed in case it produces diagnostics.
    (void)continueStmt->getTarget();
    return continueStmt;
  }

  ASTNode visitFallthroughStmt(FallthroughStmt *fallthroughStmt) {
    if (checkFallthroughStmt(fallthroughStmt))
      hadError = true;
    return fallthroughStmt;
  }

  ASTNode visitFailStmt(FailStmt *failStmt) {
    return failStmt;
  }

  ASTNode visitDeferStmt(DeferStmt *deferStmt) {
    rewriter.addLocalDeclToTypeCheck(deferStmt->getTempDecl());

    Expr *theCall = deferStmt->getCallExpr();
    TypeChecker::typeCheckExpression(theCall, context.getAsDeclContext());
    deferStmt->setCallExpr(theCall);

    return deferStmt;
  }

  ASTNode visitIfStmt(IfStmt *ifStmt) {
    // Rewrite the condition.
    if (auto condition = rewriter.rewriteTarget(SyntacticElementTarget(
            ifStmt->getCond(), context.getAsDeclContext())))
      ifStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    ifStmt->setThenStmt(castToStmt<BraceStmt>(visit(ifStmt->getThenStmt())));

    if (auto elseStmt = ifStmt->getElseStmt()) {
      ifStmt->setElseStmt(visit(elseStmt).get<Stmt *>());
    }

    return ifStmt;
  }

  ASTNode visitGuardStmt(GuardStmt *guardStmt) {
    if (auto condition = rewriter.rewriteTarget(SyntacticElementTarget(
            guardStmt->getCond(), context.getAsDeclContext())))
      guardStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto *body = visit(guardStmt->getBody()).get<Stmt *>();
    guardStmt->setBody(cast<BraceStmt>(body));
    return guardStmt;
  }

  ASTNode visitWhileStmt(WhileStmt *whileStmt) {
    if (auto condition = rewriter.rewriteTarget(SyntacticElementTarget(
            whileStmt->getCond(), context.getAsDeclContext())))
      whileStmt->setCond(*condition->getAsStmtCondition());
    else
      hadError = true;

    auto *body = visit(whileStmt->getBody()).get<Stmt *>();
    whileStmt->setBody(cast<BraceStmt>(body));
    return whileStmt;
  }

  virtual ASTNode visitDoStmt(DoStmt *doStmt) {
    auto body = visit(doStmt->getBody()).get<Stmt *>();
    doStmt->setBody(cast<BraceStmt>(body));
    return doStmt;
  }

  ASTNode visitRepeatWhileStmt(RepeatWhileStmt *repeatWhileStmt) {
    auto body = visit(repeatWhileStmt->getBody()).get<Stmt *>();
    repeatWhileStmt->setBody(cast<BraceStmt>(body));

    // Rewrite the condition.
    auto &cs = solution.getConstraintSystem();
    auto target = *cs.getTargetFor(repeatWhileStmt->getCond());
    if (auto condition = rewriter.rewriteTarget(target))
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
    auto target = *cs.getTargetFor(poundAssertStmt->getCondition());

    if (auto result = rewriter.rewriteTarget(target))
      poundAssertStmt->setCondition(result->getAsExpr());
    else
      hadError = true;

    return poundAssertStmt;
  }

  ASTNode visitThrowStmt(ThrowStmt *throwStmt) {
    auto &cs = solution.getConstraintSystem();

    // Rewrite the error.
    auto target = *cs.getTargetFor(throwStmt->getSubExpr());
    if (auto result = rewriter.rewriteTarget(target))
      throwStmt->setSubExpr(result->getAsExpr());
    else
      hadError = true;

    return throwStmt;
  }

  ASTNode visitDiscardStmt(DiscardStmt *discardStmt) {
    auto &cs = solution.getConstraintSystem();

    // Rewrite the `discard` expression.
    auto target = *cs.getTargetFor(discardStmt->getSubExpr());
    if (auto result = rewriter.rewriteTarget(target))
      discardStmt->setSubExpr(result->getAsExpr());
    else
      hadError = true;

    return discardStmt;
  }

  ASTNode visitForEachStmt(ForEachStmt *forEachStmt) {
    ConstraintSystem &cs = solution.getConstraintSystem();

    auto forEachTarget = rewriter.rewriteTarget(*cs.getTargetFor(forEachStmt));

    if (!forEachTarget)
      hadError = true;

    auto body = visit(forEachStmt->getBody()).get<Stmt *>();
    forEachStmt->setBody(cast<BraceStmt>(body));

    // Check to see if the sequence expr is throwing (in async context),
    // if so require the stmt to have a `try`.
    hadError |= diagnoseUnhandledThrowsInAsyncContext(
        context.getAsDeclContext(), forEachStmt);

    return forEachStmt;
  }

  ASTNode visitSwitchStmt(SwitchStmt *switchStmt) {
    ConstraintSystem &cs = solution.getConstraintSystem();

    // Rewrite the switch subject.
    auto subjectTarget = rewriter.rewriteTarget(*cs.getTargetFor(switchStmt));
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
      // Body of the `case` statement can contain a `fallthrough`
      // statement that requires both source and destination
      // `case` preambles to be type-checked, so bodies of `case`
      // statements should be visited after preambles.
      visitCaseStmtPreamble(caseStmt);
    }

    for (auto *caseStmt : switchStmt->getCases()) {
      visitCaseStmtBody(caseStmt);

      // Check restrictions on '@unknown'.
      if (caseStmt->hasUnknownAttr()) {
        checkUnknownAttrRestrictions(cs.getASTContext(), caseStmt,
                                     limitExhaustivityChecks);
      }
    }

    // Note we perform a limited exhaustiveness check if we weren't able to
    // apply the solution, as the subject and patterns may not be well-formed.
    TypeChecker::checkSwitchExhaustiveness(
        switchStmt, context.getAsDeclContext(),
        /*limited*/ limitExhaustivityChecks || hadError);

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

  void visitCaseStmtPreamble(CaseStmt *caseStmt) {
    // Translate the patterns and guard expressions for each case label item.
    for (auto &caseItem : caseStmt->getMutableCaseLabelItems()) {
      SyntacticElementTarget caseTarget(&caseItem, context.getAsDeclContext());
      if (!rewriter.rewriteTarget(caseTarget)) {
        hadError = true;
      }
    }

    bindSwitchCasePatternVars(context.getAsDeclContext(), caseStmt);

    for (auto *expected : caseStmt->getCaseBodyVariablesOrEmptyArray()) {
      assert(expected->hasName());
      auto prev = expected->getParentVarDecl();
      auto type = solution.resolveInterfaceType(
          solution.getType(prev)->mapTypeOutOfContext());
      expected->setInterfaceType(type);
    }
  }

  void visitCaseStmtBody(CaseStmt *caseStmt) {
    auto *newBody = visit(caseStmt->getBody()).get<Stmt *>();
    caseStmt->setBody(cast<BraceStmt>(newBody));
  }

  ASTNode visitCaseStmt(CaseStmt *caseStmt) {
    visitCaseStmtPreamble(caseStmt);
    visitCaseStmtBody(caseStmt);
    return caseStmt;
  }

  virtual ASTNode visitBraceElement(ASTNode node) {
    auto &cs = solution.getConstraintSystem();
    if (auto *expr = node.dyn_cast<Expr *>()) {
      // Rewrite the expression.
      auto target = *cs.getTargetFor(expr);
      if (auto rewrittenTarget = rewriter.rewriteTarget(target)) {
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
    return node;
  }

  ASTNode visitBraceStmt(BraceStmt *braceStmt) {
    auto &cs = solution.getConstraintSystem();

    // Diagnose defer statement being last one in block.
    if (!braceStmt->empty()) {
      if (auto stmt = braceStmt->getLastElement().dyn_cast<Stmt *>()) {
        if (auto deferStmt = dyn_cast<DeferStmt>(stmt)) {
          auto &diags = cs.getASTContext().Diags;
          diags
              .diagnose(deferStmt->getStartLoc(), diag::defer_stmt_at_block_end)
              .fixItReplace(deferStmt->getStartLoc(), "do");
        }
      }
    }

    for (auto &node : braceStmt->getElements())
      node = visitBraceElement(node);

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
    auto closure = context.getAsAbstractClosureExpr();
    if (closure && !closure.get()->hasSingleExpressionBody() &&
        closure.get()->getBody() == braceStmt) {
      auto resultType = getContextualResultType();
      if (resultType->getOptionalObjectType() &&
          resultType->lookThroughAllOptionalTypes()->isVoid() &&
          !braceStmt->getLastElement().isStmt(StmtKind::Return)) {
        return addImplicitVoidReturn(braceStmt, resultType);
      }
    }

    return braceStmt;
  }

  ASTNode addImplicitVoidReturn(BraceStmt *braceStmt, Type contextualResultTy) {
    auto &cs = solution.getConstraintSystem();
    auto &ctx = cs.getASTContext();

    auto *resultExpr = getVoidExpr(ctx);
    cs.cacheExprTypes(resultExpr);

    auto *returnStmt = ReturnStmt::createImplicit(ctx, resultExpr);

    // For a target for newly created result and apply a solution
    // to it, to make sure that optional injection happens required
    // number of times.
    {
      SyntacticElementTarget target(resultExpr, context.getAsDeclContext(),
                                    CTP_ReturnStmt, contextualResultTy,
                                    /*isDiscarded=*/false);
      cs.setTargetFor(returnStmt, target);

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
    auto &cs = solution.getConstraintSystem();

    auto resultType = getContextualResultType();

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

      auto target = *cs.getTargetFor(returnStmt);
      returnStmt->setResult(target.getAsExpr());
    }

    auto *resultExpr = returnStmt->getResult();

    enum {
      convertToResult,
      coerceToVoid
    } mode;

    auto resultExprType =
        solution.simplifyType(solution.getType(resultExpr))->getRValueType();
    // A closure with a non-void return expression can coerce to a closure
    // that returns Void.
    // TODO: We probably ought to introduce an implicit conversion expr to Void
    // and eliminate this case.
    if (resultType->isVoid() && !resultExprType->isVoid()) {
      mode = coerceToVoid;

      // Normal rule is to coerce to the return expression to the closure type.
    } else {
      mode = convertToResult;
    }

    auto target = *cs.getTargetFor(returnStmt);

    // If we're not converting to a result, unset the contextual type.
    if (mode != convertToResult) {
      target.setExprConversionType(Type());
      target.setExprContextualTypePurpose(CTP_Unused);
    }

    if (auto newResultTarget = rewriter.rewriteTarget(target)) {
      resultExpr = newResultTarget->getAsExpr();
    }

    switch (mode) {
    case convertToResult:
      // Record the coerced expression.
      returnStmt->setResult(resultExpr);
      return returnStmt;

    case coerceToVoid: {
      // Evaluate the expression, then produce a return statement that
      // returns nothing.
      TypeChecker::checkIgnoredExpr(resultExpr);

      // For a single expression closure, we can just preserve the result expr,
      // and leave the return as implied. This avoids neededing to jump through
      // nested brace statements to dig out the single expression in
      // ClosureExpr::getSingleExpressionBody.
      if (context.isSingleExpressionClosure(cs))
        return resultExpr;

      auto &ctx = solution.getConstraintSystem().getASTContext();
      auto *newReturnStmt = ReturnStmt::createImplicit(
          ctx, returnStmt->getStartLoc(), /*result*/ nullptr);
      ASTNode elements[2] = { resultExpr, newReturnStmt };
      return BraceStmt::create(ctx, returnStmt->getStartLoc(),
                               elements, returnStmt->getEndLoc(),
                               /*implicit*/ true);
    }
    }

    return returnStmt;
  }

  ASTNode visitThenStmt(ThenStmt *thenStmt) {
    auto SVE = context.getAsSingleValueStmtExpr();
    assert(SVE && "Should have diagnosed an out-of-place ThenStmt");
    auto ty = solution.getResolvedType(SVE.get());

    // We need to fixup the conversion type to the full result type,
    // not the branch result type. This is necessary as there may be
    // an additional conversion required for the branch.
    auto target = solution.getTargetFor(thenStmt->getResult());
    target->setExprConversionType(ty);

    auto *resultExpr = thenStmt->getResult();
    if (auto newResultTarget = rewriter.rewriteTarget(*target))
      resultExpr = newResultTarget->getAsExpr();

    thenStmt->setResult(resultExpr);

    // If the expression was typed as Void, its branches are effectively
    // discarded, so treat them as ignored expressions.
    if (ty->lookThroughAllOptionalTypes()->isVoid()) {
      TypeChecker::checkIgnoredExpr(resultExpr);
    }
    return thenStmt;
  }

#define UNSUPPORTED_STMT(STMT) ASTNode visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
#undef UNSUPPORTED_STMT

public:
  /// Apply the solution to the context and return updated statement.
  Stmt *apply() {
    auto body = visit(context.getStmt());
    return body ? body.get<Stmt *>() : nullptr;
  }
};

class ResultBuilderRewriter : public SyntacticElementSolutionApplication {
  const AppliedBuilderTransform &Transform;

public:
  ResultBuilderRewriter(AnyFunctionRef context,
                        const AppliedBuilderTransform &transform,
                        SyntacticElementTargetRewriter &rewriter)
      : SyntacticElementSolutionApplication(
            rewriter.getSolution(),
            SyntacticElementContext::forFunctionRef(context), rewriter),
        Transform(transform) {}

  bool apply() {
    auto body = visit(context.getStmt());

    if (!body || hadError)
      return true;

    auto funcRef = context.getAsAnyFunctionRef();
    assert(funcRef);

    funcRef->setTypecheckedBody(castToStmt<BraceStmt>(body));
    return false;
  }

private:
  ASTNode visitDoStmt(DoStmt *doStmt) override {
    if (auto transformed = transformDo(doStmt)) {
      return visit(transformed.get());
    }

    auto newBody = visit(doStmt->getBody());
    if (!newBody)
      return nullptr;

    doStmt->setBody(castToStmt<BraceStmt>(newBody));
    return doStmt;
  }

  ASTNode visitBraceElement(ASTNode node) override {
    if (auto *SVE = getAsExpr<SingleValueStmtExpr>(node)) {
      // This should never be treated as an expression in a result builder,
      // it should have statement semantics.
      return visitBraceElement(SVE->getStmt());
    }
    return SyntacticElementSolutionApplication::visitBraceElement(node);
  }

  NullablePtr<Stmt> transformDo(DoStmt *doStmt) {
    if (!doStmt->isImplicit())
      return nullptr;

    // Implicit `do` wraps a statement and it's `type_join` expression.
    auto *body = doStmt->getBody();

    // If there are more than two elements, this `do` doesn't need to
    // get be transformed.
    if (body->getNumElements() != 2)
      return nullptr;

    auto *stmt = castToStmt(body->getFirstElement());
    auto *join = castToExpr<TypeJoinExpr>(body->getLastElement());

    switch (stmt->getKind()) {
    case StmtKind::If:
      return transformIf(castToStmt<IfStmt>(stmt), join, /*index=*/0);

    case StmtKind::Switch:
      return transformSwitch(castToStmt<SwitchStmt>(stmt), join);

    default:
      llvm_unreachable("only 'if' and 'switch' statements are transformed");
    }
  }

  NullablePtr<Stmt> transformSwitch(SwitchStmt *switchStmt,
                                    TypeJoinExpr *join) {
    unsigned caseIndex = 0;
    for (auto *caseStmt : switchStmt->getCases()) {
      auto newBody = transformBody(caseStmt->getBody(), join, caseIndex++);
      if (!newBody)
        return nullptr;

      caseStmt->setBody(newBody.get());
    }

    return switchStmt;
  }

  NullablePtr<Stmt> transformIf(IfStmt *ifStmt, TypeJoinExpr *join,
                                unsigned index) {
    // FIXME: Turn this into a condition once warning is an error.
    (void)diagnoseMissingBuildWithAvailability(ifStmt, join);

    auto *joinVar = join->getVar();

    // First, let's add assignment to the end of `then` branch
    {
      auto *thenBody = castToStmt<BraceStmt>(ifStmt->getThenStmt());
      auto newBody = transformBody(thenBody, join, index);
      if (!newBody)
        return nullptr;

      ifStmt->setThenStmt(newBody.get());
    }

    if (auto *elseStmt = ifStmt->getElseStmt()) {
      if (auto *innerIfStmt = getAsStmt<IfStmt>(elseStmt)) {
        auto transformedIf = transformIf(innerIfStmt, join, index + 1);
        if (!transformedIf)
          return nullptr;

        ifStmt->setElseStmt(transformedIf.get());
      } else {
        auto newBody =
            transformBody(castToStmt<BraceStmt>(elseStmt), join, index + 1);
        if (!newBody)
          return nullptr;

        ifStmt->setElseStmt(newBody.get());
      }
    } else {
      auto &ctx = getASTContext();
      SmallVector<ASTNode, 2> elseBranch;

      elseBranch.push_back(
          createAssignment(joinVar, join->getElement(index + 1)));

      ifStmt->setElseStmt(BraceStmt::create(ctx, ifStmt->getEndLoc(),
                                            elseBranch, ifStmt->getEndLoc(),
                                            /*implicit=*/true));
    }

    return ifStmt;
  }

  NullablePtr<BraceStmt> transformBody(BraceStmt *body, TypeJoinExpr *join,
                                       unsigned index) {
    for (auto &element : body->getElements()) {
      if (auto *doStmt = getAsStmt<DoStmt>(element)) {
        if (auto transformed = transformDo(doStmt))
          element = transformed.get();
      }
    }

    return addBuilderAssignment(body, join->getVar(), join->getElement(index));
  }

  // Add `$__bulderN = build{Optional, Either}(...)` at the end of a block body.
  BraceStmt *addBuilderAssignment(BraceStmt *body, DeclRefExpr *joinVar,
                                  Expr *builderCall) {
    SmallVector<ASTNode, 4> newBody;
    llvm::copy(body->getElements(), std::back_inserter(newBody));

    newBody.push_back(createAssignment(joinVar, builderCall));

    return BraceStmt::create(getASTContext(), body->getLBraceLoc(), newBody,
                             body->getRBraceLoc(), body->isImplicit());
  }

  AssignExpr *createAssignment(DeclRefExpr *destRef, Expr *source) {
    auto &ctx = getASTContext();
    auto &CS = solution.getConstraintSystem();

    auto *assignment = new (ctx) AssignExpr(destRef, /*EqualLoc=*/SourceLoc(),
                                            source, /*Implicit=*/true);

    {
      // Assignment expression is always `Void`.
      CS.setType(assignment, ctx.TheEmptyTupleType);

      CS.setTargetFor({assignment},
                      {assignment, context.getAsDeclContext(), CTP_Unused,
                       /*contextualType=*/Type(), /*isDiscarded=*/false});
    }

    return assignment;
  }

  ASTContext &getASTContext() const {
    return context.getAsDeclContext()->getASTContext();
  }

private:
  /// Look for a #available condition. If there is one, we need to check
  /// that the resulting type of the "then" doesn't refer to any types that
  /// are unavailable in the enclosing context.
  ///
  /// Note that this is for staging in support for buildLimitedAvailability();
  /// the diagnostic is currently a warning, so that existing code that
  /// compiles today will continue to compile. Once result builder types
  /// have had the chance to adopt buildLimitedAvailability(), we'll upgrade
  /// this warning to an error.
  [[nodiscard]]
  bool diagnoseMissingBuildWithAvailability(IfStmt *ifStmt,
                                            TypeJoinExpr *join) {
    auto findAvailabilityCondition =
        [](StmtCondition stmtCond) -> const StmtConditionElement * {
      for (const auto &cond : stmtCond) {
        switch (cond.getKind()) {
        case StmtConditionElement::CK_Boolean:
        case StmtConditionElement::CK_PatternBinding:
        case StmtConditionElement::CK_HasSymbol:
          continue;

        case StmtConditionElement::CK_Availability:
          return &cond;
          break;
        }
      }

      return nullptr;
    };

    auto availabilityCond = findAvailabilityCondition(ifStmt->getCond());
    if (!availabilityCond)
      return false;

    SourceLoc loc = availabilityCond->getStartLoc();
    auto builderType = solution.simplifyType(Transform.builderType);
    // Since all of the branches of `if` statement have to join into the same
    // type we can just use the type of the join variable here.
    Type bodyType = solution.getResolvedType(join->getVar());

    return bodyType.findIf([&](Type type) {
      auto nominal = type->getAnyNominal();
      if (!nominal)
        return false;

      auto constraint = getUnsatisfiedAvailabilityConstraint(
          nominal, context.getAsDeclContext(), loc);
      if (constraint && constraint->isConditionallySatisfiable()) {
        auto &ctx = getASTContext();
        ctx.Diags.diagnose(loc,
                           diag::result_builder_missing_limited_availability,
                           builderType);

        // Add a note to the result builder with a stub for
        // buildLimitedAvailability().
        if (auto builder = builderType->getAnyNominal()) {
          SourceLoc buildInsertionLoc;
          std::string stubIndent;
          Type componentType;
          std::tie(buildInsertionLoc, stubIndent, componentType) =
              determineResultBuilderBuildFixItInfo(builder);
          if (buildInsertionLoc.isValid()) {
            std::string fixItString;
            {
              llvm::raw_string_ostream out(fixItString);
              printResultBuilderBuildFunction(
                  builder, componentType,
                  ResultBuilderBuildFunction::BuildLimitedAvailability,
                  stubIndent, out);

              builder
                  ->diagnose(
                      diag::result_builder_missing_build_limited_availability,
                      builderType)
                  .fixItInsert(buildInsertionLoc, fixItString);
            }
          }
        }

        return true;
      }

      return false;
    });
  }
};
} // namespace

static void applySolutionToClosurePropertyWrappers(ClosureExpr *closure,
                                                   const Solution &solution) {
  for (auto *param : *closure->getParameters()) {
    if (!param->hasAttachedPropertyWrapper())
      continue;

    // Set the interface type of each property wrapper synthesized var
    auto *backingVar = param->getPropertyWrapperBackingProperty();
    auto backingType = solution.simplifyType(solution.getType(backingVar))
                           ->mapTypeOutOfContext();
    backingVar->setInterfaceType(backingType);

    if (auto *projectionVar = param->getPropertyWrapperProjectionVar()) {
      projectionVar->setInterfaceType(
          solution.simplifyType(solution.getType(projectionVar))
              ->mapTypeOutOfContext());
    }

    auto *wrappedValueVar = param->getPropertyWrapperWrappedValueVar();
    auto wrappedValueType =
        solution.simplifyType(solution.getType(wrappedValueVar))
            ->mapTypeOutOfContext();
    wrappedValueVar->setInterfaceType(
        wrappedValueType->getWithoutSpecifierType());

    if (param->hasImplicitPropertyWrapper()) {
      if (wrappedValueType->is<LValueType>())
        wrappedValueVar->setImplInfo(StorageImplInfo::getMutableComputed());

      // Add an explicit property wrapper attribute, which is needed for
      // synthesizing the accessors.
      auto &context = wrappedValueVar->getASTContext();
      auto *typeExpr = TypeExpr::createImplicit(backingType, context);
      auto *attr =
          CustomAttr::create(context, SourceLoc(), typeExpr, /*implicit=*/true);
      wrappedValueVar->getAttrs().add(attr);
    }
  }
}

bool ConstraintSystem::applySolution(AnyFunctionRef fn,
                                     SyntacticElementTargetRewriter &rewriter) {
  auto &solution = rewriter.getSolution();
  auto &cs = solution.getConstraintSystem();
  auto *closure = getAsExpr<ClosureExpr>(fn.getAbstractClosureExpr());
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

    if (solution.preconcurrencyClosures.count(closure))
      closure->setIsolatedByPreconcurrency();

    // Coerce the result type, if it was written explicitly.
    if (closure->hasExplicitResultType()) {
      closure->setExplicitResultType(closureFnType->getResult());
    }

    applySolutionToClosurePropertyWrappers(closure, solution);

    TypeChecker::checkClosureAttributes(closure);
    TypeChecker::checkParameterList(closure->getParameters(), closure);
  }

  // Enter the context of the function before performing any additional
  // transformations.
  llvm::SaveAndRestore<DeclContext *> savedDC(rewriter.getCurrentDC(),
                                              fn.getAsDeclContext());

  // Apply the result builder transform, if there is one.
  if (auto transform = solution.getAppliedBuilderTransform(fn)) {
    NullablePtr<BraceStmt> newBody;

    fn.setParsedBody(transform->transformedBody);

    ResultBuilderRewriter builderRewriter(fn, *transform, rewriter);
    return builderRewriter.apply();
  }
  assert(closure && "Can only get here with a closure at the moment");
  return applySolutionToBody(closure, rewriter);
}

bool ConstraintSystem::applySolutionToBody(
    AnyFunctionRef fn, SyntacticElementTargetRewriter &rewriter) {
  // Enter the context of the function before performing any additional
  // transformations.
  llvm::SaveAndRestore<DeclContext *> savedDC(rewriter.getCurrentDC(),
                                              fn.getAsDeclContext());

  auto &solution = rewriter.getSolution();
  SyntacticElementSolutionApplication application(
      solution, SyntacticElementContext::forFunctionRef(fn), rewriter);

  auto *body = application.apply();

  if (!body || application.hadError)
    return true;

  fn.setTypecheckedBody(cast<BraceStmt>(body));
  return false;
}

bool ConstraintSystem::applySolutionToBody(
    TapExpr *tapExpr, SyntacticElementTargetRewriter &rewriter) {
  auto &solution = rewriter.getSolution();
  SyntacticElementSolutionApplication application(
      solution, SyntacticElementContext::forTapExpr(tapExpr), rewriter);

  auto body = application.apply();

  if (!body || application.hadError)
    return true;

  tapExpr->setBody(castToStmt<BraceStmt>(body));
  return false;
}

bool ConstraintSystem::applySolutionToSingleValueStmt(
    SingleValueStmtExpr *SVE, SyntacticElementTargetRewriter &rewriter) {
  auto &solution = rewriter.getSolution();
  setType(SVE, solution.getResolvedType(SVE));

  auto context = SyntacticElementContext::forSingleValueStmtExpr(SVE);
  SyntacticElementSolutionApplication application(solution, context, rewriter);
  auto *stmt = application.apply();
  if (!stmt || application.hadError)
    return true;

  SVE->setStmt(stmt);
  return false;
}

void ConjunctionElement::findReferencedVariables(
    ConstraintSystem &cs, SmallPtrSetImpl<TypeVariableType *> &typeVars) const {
  auto referencedVars = Element->getTypeVariables();
  typeVars.insert(referencedVars.begin(), referencedVars.end());

  if (Element->getKind() != ConstraintKind::SyntacticElement)
    return;

  ASTNode element = Element->getSyntacticElement();
  auto *locator = Element->getLocator();

  ASTNode parent = locator->getAnchor();
  if (auto *SVE = getAsExpr<SingleValueStmtExpr>(parent)) {
    // Use a parent closure if we have one. This is needed to correctly handle
    // return statements that refer to an outer closure.
    if (auto *CE = dyn_cast<ClosureExpr>(SVE->getDeclContext()))
      parent = CE;
  }

  TypeVariableRefFinder refFinder(cs, parent, Element->getElementContext(),
                                  typeVars);

  // If this is a pattern of `for-in` statement, let's walk into `for-in`
  // sequence expression because both elements are type-checked together.
  //
  // Correct expressions wouldn't have any type variables in sequence but
  // they could appear due to circular references or other incorrect syntax.
  if (element.is<Pattern *>()) {
    if (auto parent =
            locator->getLastElementAs<LocatorPathElt::SyntacticElement>()) {
      if (auto *forEach = getAsStmt<ForEachStmt>(parent->getElement())) {
        if (auto *sequence = forEach->getParsedSequence())
          sequence->walk(refFinder);
        return;
      }
    }
  }

  if (auto *patternBinding =
          dyn_cast_or_null<PatternBindingDecl>(element.dyn_cast<Decl *>())) {
    // Let's not walk into placeholder variable initializers, since they
    // are type-checked separately right now.
    if (isPlaceholderVar(patternBinding))
      return;

    if (auto patternBindingElt =
            locator
                ->getLastElementAs<LocatorPathElt::PatternBindingElement>()) {
      if (auto *init = patternBinding->getInit(patternBindingElt->getIndex()))
        init->walk(refFinder);
      return;
    }
  }

  if (element.is<Decl *>() || element.is<StmtConditionElement *>() ||
      element.is<Expr *>() || element.isPattern(PatternKind::Expr) ||
      element.isStmt(StmtKind::Return)) {
    element.walk(refFinder);
  }
}

Type constraints::isPlaceholderVar(PatternBindingDecl *PB) {
  auto *var = PB->getSingleVar();
  if (!var)
    return Type();

  if (!var->getName().hasDollarPrefix())
    return Type();

  auto *pattern = PB->getPattern(0);
  if (auto *typedPattern = dyn_cast<TypedPattern>(pattern)) {
    auto type = typedPattern->getType();
    if (type && type->hasPlaceholder())
      return type;
  }

  return Type();
}
