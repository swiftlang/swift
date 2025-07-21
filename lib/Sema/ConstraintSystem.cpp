//===--- ConstraintSystem.cpp - Constraint-based Type Checking ------------===//
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
// This file implements the constraint-based type checker, anchored by the
// \c ConstraintSystem class, which provides type checking and type
// inference for expressions.
//
//===----------------------------------------------------------------------===//
#include "swift/Sema/ConstraintSystem.h"
#include "CSDiagnostics.h"
#include "OpenedExistentials.h"
#include "TypeCheckAvailability.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckMacros.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeTransform.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "swift/Sema/CSFix.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/IDETypeChecking.h"
#include "swift/Sema/PreparedOverload.h"
#include "swift/Sema/SolutionResult.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Format.h"
#include <cmath>

using namespace swift;
using namespace constraints;
using namespace inference;

#define DEBUG_TYPE "ConstraintSystem"

ExpressionTimer::ExpressionTimer(AnchorType Anchor, ConstraintSystem &CS,
                                 unsigned thresholdInSecs)
    : Anchor(Anchor), Context(CS.getASTContext()),
      StartTime(llvm::TimeRecord::getCurrentTime()),
      ThresholdInSecs(thresholdInSecs),
      PrintDebugTiming(CS.getASTContext().TypeCheckerOpts.DebugTimeExpressions),
      PrintWarning(true) {}

SourceRange ExpressionTimer::getAffectedRange() const {
  ASTNode anchor;

  if (auto *locator = Anchor.dyn_cast<ConstraintLocator *>()) {
    anchor = simplifyLocatorToAnchor(locator);
    // If locator couldn't be simplified down to a single AST
    // element, let's use its root.
    if (!anchor)
      anchor = locator->getAnchor();
  } else {
    anchor = Anchor.get<Expr *>();
  }

  return anchor.getSourceRange();
}

ExpressionTimer::~ExpressionTimer() {
  auto elapsed = getElapsedProcessTimeInFractionalSeconds();
  unsigned elapsedMS = static_cast<unsigned>(elapsed * 1000);

  if (PrintDebugTiming) {
    // Round up to the nearest 100th of a millisecond.
    llvm::errs() << llvm::format("%0.2f", std::ceil(elapsed * 100000) / 100)
                 << "ms\t";
    if (auto *E = Anchor.dyn_cast<Expr *>()) {
      E->getLoc().print(llvm::errs(), Context.SourceMgr);
    } else {
      auto *locator = Anchor.get<ConstraintLocator *>();
      locator->dump(&Context.SourceMgr, llvm::errs());
    }
    llvm::errs() << "\n";
  }

  if (!PrintWarning)
    return;

  const auto WarnLimit = getWarnLimit();

  if (WarnLimit == 0 || elapsedMS < WarnLimit)
    return;

  auto sourceRange = getAffectedRange();

  if (sourceRange.Start.isValid()) {
    Context.Diags
        .diagnose(sourceRange.Start, diag::debug_long_expression, elapsedMS,
                  WarnLimit)
        .highlight(sourceRange);
  }
}

ConstraintSystem::ConstraintSystem(DeclContext *dc,
                                   ConstraintSystemOptions options,
                                   DiagnosticTransaction *diagnosticTransaction)
  : Context(dc->getASTContext()), DC(dc), Options(options),
    diagnosticTransaction(diagnosticTransaction),
    Arena(dc->getASTContext(), Allocator),
    CG(*this)
{
  assert(DC && "context required");
  // Respect the global debugging flag, but turn off debugging while
  // parsing and loading other modules.
  if (Context.TypeCheckerOpts.DebugConstraintSolver &&
      DC->getParentModule()->isMainModule()) {
    Options |= ConstraintSystemFlags::DebugConstraints;
  }
  if (Context.LangOpts.UseClangFunctionTypes)
    Options |= ConstraintSystemFlags::UseClangFunctionTypes;
}

ConstraintSystem::~ConstraintSystem() {
  for (unsigned i = 0, n = TypeVariables.size(); i != n; ++i) {
    auto &impl = TypeVariables[i]->getImpl();
    delete impl.getGraphNode();
    impl.setGraphNode(nullptr);
  }
}

void ConstraintSystem::startExpressionTimer(ExpressionTimer::AnchorType anchor) {
  ASSERT(!Timer);

  const auto &opts = getASTContext().TypeCheckerOpts;
  unsigned timeout = opts.ExpressionTimeoutThreshold;

  // If either the timeout is set, or we're asked to emit warnings,
  // start the timer. Otherwise, don't start the timer, it's needless
  // overhead.
  if (timeout == 0) {
    if (opts.WarnLongExpressionTypeChecking == 0)
      return;

    timeout = ExpressionTimer::NoLimit;
  }

  Timer.emplace(anchor, *this, timeout);
}

void ConstraintSystem::incrementScopeCounter() {
  ++NumSolverScopes;
  if (auto *Stats = getASTContext().Stats)
    ++Stats->getFrontendCounters().NumConstraintScopes;
}

void ConstraintSystem::incrementLeafScopes() {
  if (auto *Stats = getASTContext().Stats)
    ++Stats->getFrontendCounters().NumLeafScopes;
}

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  return llvm::any_of(TypeVariables, [](const TypeVariableType *typeVar) {
    return !typeVar->getImpl().hasRepresentativeOrFixed();
  });
}

void ConstraintSystem::addTypeVariable(TypeVariableType *typeVar) {
  ASSERT(!PreparingOverload);

  TypeVariables.insert(typeVar);

  // Notify the constraint graph.
  CG.addTypeVariable(typeVar);
}

void ConstraintSystem::mergeEquivalenceClasses(TypeVariableType *typeVar1,
                                               TypeVariableType *typeVar2,
                                               bool updateWorkList) {
  assert(typeVar1 == getRepresentative(typeVar1) &&
         "typeVar1 is not the representative");
  assert(typeVar2 == getRepresentative(typeVar2) &&
         "typeVar2 is not the representative");
  assert(typeVar1 != typeVar2 && "cannot merge type with itself");

  // Always merge 'up' the constraint stack, because it is simpler.
  if (typeVar1->getImpl().getID() > typeVar2->getImpl().getID())
    std::swap(typeVar1, typeVar2);

  CG.mergeNodesPre(typeVar2);
  typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getTrail());
  CG.mergeNodes(typeVar1, typeVar2);

  if (updateWorkList) {
    addTypeVariableConstraintsToWorkList(typeVar1);
  }
}

/// Determine whether the given type variables occurs in the given type.
bool ConstraintSystem::typeVarOccursInType(TypeVariableType *typeVar,
                                           Type type,
                                           bool *involvesOtherTypeVariables) {
  SmallPtrSet<TypeVariableType *, 4> typeVars;
  type->getTypeVariables(typeVars);

  bool occurs = typeVars.count(typeVar);
  if (involvesOtherTypeVariables) {
    *involvesOtherTypeVariables =
        occurs ? typeVars.size() > 1 : !typeVars.empty();
  }

  return occurs;
}

void ConstraintSystem::assignFixedType(TypeVariableType *typeVar, Type type,
                                       bool updateState,
                                       bool notifyBindingInference) {
  assert(!type->hasError() &&
         "Should not be assigning a type involving ErrorType!");

  CG.retractFromInference(typeVar);
  typeVar->getImpl().assignFixedType(type, getTrail());

  if (!updateState)
    return;

  if (!type->isTypeVariableOrMember()) {
    // If this type variable represents a literal, check whether we picked the
    // default literal type. First, find the corresponding protocol.
    //
    // If we have the constraint graph, we can check all type variables in
    // the equivalence class. This is the More Correct path.
    // FIXME: Eliminate the less-correct path.
    auto typeVarRep = getRepresentative(typeVar);
    for (auto *tv : CG[typeVarRep].getEquivalenceClass()) {
      auto locator = tv->getImpl().getLocator();
      if (!(locator && (locator->directlyAt<CollectionExpr>() ||
                        locator->directlyAt<LiteralExpr>())))
          continue;

      auto *literalProtocol = TypeChecker::getLiteralProtocol(
          getASTContext(), castToExpr(locator->getAnchor()));
      if (!literalProtocol)
        continue;

      // If the protocol has a default type, check it.
      if (auto defaultType = TypeChecker::getDefaultType(literalProtocol, DC)) {
        // Check whether the nominal types match. This makes sure that we
        // properly handle Array vs. Array<T>.
        if (defaultType->getAnyNominal() != type->getAnyNominal()) {
          increaseScore(SK_NonDefaultLiteral, locator);
        }
      }

      break;
    }
  }

  // Notify the constraint graph.
  CG.bindTypeVariable(typeVar, type);

  addTypeVariableConstraintsToWorkList(typeVar);

  if (notifyBindingInference)
    CG.introduceToInference(typeVar, type);
}

void ConstraintSystem::addTypeVariableConstraintsToWorkList(
       TypeVariableType *typeVar) {
  // Activate the constraints affected by a change to this type variable.
  for (auto *constraint : CG.gatherAllConstraints(typeVar))
    if (!constraint->isActive())
      activateConstraint(constraint);
}

void ConstraintSystem::addConversionRestriction(
    Type srcType, Type dstType,
    ConversionRestrictionKind restriction) {
  auto key = std::make_pair(srcType.getPointer(), dstType.getPointer());
  bool inserted = ConstraintRestrictions.insert(
      std::make_pair(key, restriction)).second;
  if (!inserted)
    return;

  if (solverState) {
    recordChange(SolverTrail::Change::AddedConversionRestriction(
      srcType, dstType));
  }
}

void ConstraintSystem::removeConversionRestriction(
    Type srcType, Type dstType) {
  auto key = std::make_pair(srcType.getPointer(), dstType.getPointer());
  bool erased = ConstraintRestrictions.erase(key);
  ASSERT(erased);
}

void ConstraintSystem::addFix(ConstraintFix *fix) {
  ASSERT(!PreparingOverload);

  bool inserted = Fixes.insert(fix);
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::AddedFix(fix));
}

void ConstraintSystem::removeFix(ConstraintFix *fix) {
  ASSERT(Fixes.back() == fix);
  Fixes.pop_back();
}

void ConstraintSystem::recordDisjunctionChoice(
    ConstraintLocator *locator, unsigned index) {
  bool inserted = DisjunctionChoices.insert({locator, index}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedDisjunctionChoice(locator));
}

void ConstraintSystem::recordAppliedDisjunction(
    ConstraintLocator *locator, FunctionType *fnType) {
  // We shouldn't ever register disjunction choices multiple times.
  bool inserted = AppliedDisjunctions.insert(
      std::make_pair(locator, fnType)).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedAppliedDisjunction(locator));
}

/// Retrieve a dynamic result signature for the given declaration.
static std::tuple<char, ObjCSelector, CanType>
getDynamicResultSignature(ValueDecl *decl) {
  // Handle functions.
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    // Strip `@Sendable` and `any Sendable` because it should be
    // possible to add them without affecting lookup results and
    // shadowing. All of the declarations that are processed here
    // are `@objc` and hence `@preconcurrency`.
    auto type = func->getMethodInterfaceType()->stripConcurrency(
        /*recursive=*/true, /*dropGlobalActor=*/false);
    return std::make_tuple(func->isStatic(), func->getObjCSelector(),
                           type->getCanonicalType());
  }

  if (auto asd = dyn_cast<AbstractStorageDecl>(decl)) {
    auto ty = asd->getInterfaceType();

    // Strip off a generic signature if we have one. This matches the logic
    // for methods, and ensures that we don't take a protocol's generic
    // signature into account for a subscript requirement.
    if (auto *genericFn = ty->getAs<GenericFunctionType>()) {
      ty = FunctionType::get(genericFn->getParams(), genericFn->getResult(),
                             genericFn->getExtInfo());
    }

    // Handle properties and subscripts, anchored by the getter's selector.
    return std::make_tuple(asd->isStatic(), asd->getObjCGetterSelector(),
                           ty->getCanonicalType());
  }

  llvm_unreachable("Not a valid @objc member");
}

LookupResult &ConstraintSystem::lookupMember(Type base, DeclNameRef name,
                                             SourceLoc loc) {
  // Check whether we've already performed this lookup.
  auto &result = MemberLookups[{base, name}];
  if (result) return *result;

  // Lookup the member.
  result = TypeChecker::lookupMember(
      DC, base, name, loc, defaultConstraintSolverMemberLookupOptions);

  // If we are in an @_unsafeInheritExecutor context, swap out
  // declarations for their _unsafeInheritExecutor_ counterparts if they
  // exist.
  if (enclosingUnsafeInheritsExecutor(DC)) {
    introduceUnsafeInheritExecutorReplacements(DC, base, loc, *result);
  }

  // If we aren't performing dynamic lookup, we're done.
  if (!*result || !base->isAnyObject())
    return *result;

  // We are performing dynamic lookup. Filter out redundant results early.
  llvm::DenseMap<std::tuple<char, ObjCSelector, CanType>, ValueDecl *> known;
  bool anyRemovals = false;
  for (const auto &entry : *result) {
    auto *decl = entry.getValueDecl();

    // Remove invalid declarations so the constraint solver doesn't need to
    // cope with them.
    if (decl->isInvalid()) {
      anyRemovals = true;
      continue;
    }

    // If this is the first entry with the signature, record it.
    auto &uniqueEntry = known[getDynamicResultSignature(decl)];
    if (!uniqueEntry) {
      uniqueEntry = decl;
      continue;
    }

    // We have duplication; note that we'll need to remove something,
    anyRemovals = true;

    // If the entry we recorded was unavailable but this new entry is not,
    // replace the recorded entry with this one.
    if (isDeclUnavailable(uniqueEntry) && !isDeclUnavailable(decl)) {
      uniqueEntry = decl;
    }
  }

  // If there's anything to remove, filter it out now.
  if (anyRemovals) {
    result->filter([&](LookupResultEntry entry, bool isOuter) -> bool {
      auto *decl = entry.getValueDecl();

      // Remove invalid declarations so the constraint solver doesn't need to
      // cope with them.
      if (decl->isInvalid())
        return false;

      return known[getDynamicResultSignature(decl)] == decl;
    });
  }

  return *result;
}

ArrayRef<Type>
ConstraintSystem::getAlternativeLiteralTypes(KnownProtocolKind kind,
                                             SmallVectorImpl<Type> &scratch) {
  assert(scratch.empty());
  if (kind == KnownProtocolKind::ExpressibleByIntegerLiteral) {
    // Integer literals can be treated as floating point literals.
    if (auto floatProto = getASTContext().getProtocol(
                            KnownProtocolKind::ExpressibleByFloatLiteral)) {
      if (auto defaultType = TypeChecker::getDefaultType(floatProto, DC))
        scratch.push_back(defaultType);
    }
  }
  return scratch;
}

bool ConstraintSystem::containsIDEInspectionTarget(ASTNode node) const {
  return swift::containsIDEInspectionTarget(node.getSourceRange(),
                                            Context.SourceMgr);
}

bool ConstraintSystem::containsIDEInspectionTarget(
    const ArgumentList *args) const {
  return swift::containsIDEInspectionTarget(args->getSourceRange(),
                                            Context.SourceMgr);
}

void ConstraintSystem::recordPotentialThrowSite(
    CatchNode catchNode, PotentialThrowSite site) {
  potentialThrowSites.push_back({catchNode, site});
  if (solverState)
    recordChange(SolverTrail::Change::RecordedPotentialThrowSite(catchNode));
}

void ConstraintSystem::removePotentialThrowSite(CatchNode catchNode) {
  ASSERT(potentialThrowSites.back().first == catchNode);
  potentialThrowSites.pop_back();
}

void ConstraintSystem::recordPotentialThrowSite(
    PotentialThrowSite::Kind kind, Type type,
    ConstraintLocatorBuilder locator) {
  ASTContext &ctx = getASTContext();

  // Only record potential throw sites when typed throws is enabled.
  if (!ctx.LangOpts.hasFeature(Feature::FullTypedThrows))
    return;

  // Catch node location is determined by the source location.
  auto sourceLoc = locator.getAnchor().getStartLoc();
  if (!sourceLoc)
    return;

  auto catchNode = ASTScope::lookupCatchNode(DC->getParentModule(), sourceLoc);
  if (!catchNode)
    return;

  // If there is an explicit caught type for this node, we don't need to
  // record a potential throw site.
  if (Type explicitCaughtType = catchNode.getExplicitCaughtType(ctx))
    return;

  // do..catch statements without an explicit `throws` clause do infer
  // thrown types.
  if (catchNode.is<DoCatchStmt *>()) {
    PotentialThrowSite site{kind, type, getConstraintLocator(locator)};
    recordPotentialThrowSite(catchNode, site);
    return;
  }

  // Closures without an explicit `throws` clause, and which syntactically
  // appear that they can throw, do infer thrown types.
  auto closure = catchNode.get<ClosureExpr *>();

  // Check whether the closure syntactically throws. If not, there is no
  // need to record a throw site.
  if (!closureEffects(closure).isThrowing())
    return;

  PotentialThrowSite site{kind, type, getConstraintLocator(locator)};
  recordPotentialThrowSite(catchNode, site);
}

Type ConstraintSystem::getExplicitCaughtErrorType(CatchNode catchNode) {
  ASTContext &ctx = getASTContext();

  // If there is an explicit caught type for this node, use it.
  if (Type explicitCaughtType = catchNode.getExplicitCaughtType(ctx)) {
    if (explicitCaughtType->hasTypeParameter())
      explicitCaughtType = DC->mapTypeIntoContext(explicitCaughtType);

    return explicitCaughtType;
  }

  return Type();
}

Type ConstraintSystem::getCaughtErrorType(CatchNode catchNode) {
  ASTContext &ctx = getASTContext();

  // If we have an explicit caught error type for this node, use it.
  if (auto explicitCaughtType = getExplicitCaughtErrorType(catchNode))
    return explicitCaughtType;

  // Retrieve the thrown error type of a closure.
  // FIXME: This will need to change when we do inference of thrown error
  // types in closures.
  if (auto closure = catchNode.dyn_cast<ClosureExpr *>()) {
    return getClosureType(closure)->getEffectiveThrownErrorTypeOrNever();
  }

  if (!ctx.LangOpts.hasFeature(Feature::FullTypedThrows))
    return ctx.getErrorExistentialType();

  // Handle inference of caught error types.

  // Collect all of the potential throw sites for this catch node.
  SmallVector<PotentialThrowSite, 2> throwSites;
  for (const auto &potentialThrowSite : potentialThrowSites) {
    if (potentialThrowSite.first == catchNode) {
      throwSites.push_back(potentialThrowSite.second);
    }
  }

  Type caughtErrorType = ctx.getNeverType();
  for (const auto &throwSite : throwSites) {
    Type type = simplifyType(throwSite.type);

    Type thrownErrorType;
    switch (throwSite.kind) {
    case PotentialThrowSite::Application: {
      auto fnType = type->castTo<AnyFunctionType>();
      thrownErrorType = fnType->getEffectiveThrownErrorTypeOrNever();
      break;
    }

    case PotentialThrowSite::ExplicitThrow:
    case PotentialThrowSite::NonExhaustiveDoCatch:
    case PotentialThrowSite::PropertyAccess:
      thrownErrorType = type;
      break;
    }

    // Perform the errorUnion() of the caught error type so far with the
    // thrown error type of this potential throw site.
    caughtErrorType = TypeChecker::errorUnion(
        caughtErrorType, thrownErrorType,
        [&](Type type) {
      return simplifyType(type);
    });

    // If we ended up at 'any Error', we're done.
    if (caughtErrorType->isErrorExistentialType())
      break;
  }

  return caughtErrorType;
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
    ASTNode anchor, ArrayRef<ConstraintLocator::PathElement> path) {
  auto summaryFlags = ConstraintLocator::getSummaryFlagsForPath(path);
  return getConstraintLocator(anchor, path, summaryFlags);
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
    ASTNode anchor, ArrayRef<ConstraintLocator::PathElement> path,
    unsigned summaryFlags) {
  assert(summaryFlags == ConstraintLocator::getSummaryFlagsForPath(path));

  // Check whether a locator with this anchor + path already exists.
  llvm::FoldingSetNodeID id;
  ConstraintLocator::Profile(id, anchor, path);
  void *insertPos = nullptr;
  auto locator = ConstraintLocators.FindNodeOrInsertPos(id, insertPos);
  if (locator)
    return locator;

  // Allocate a new locator and add it to the set.
  locator = ConstraintLocator::create(getAllocator(), anchor, path,
                                      summaryFlags);
  ConstraintLocators.InsertNode(locator, insertPos);
  return locator;
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
                     const ConstraintLocatorBuilder &builder) {
  // If the builder has an empty path, just extract its base locator.
  if (builder.hasEmptyPath()) {
    return builder.getBaseLocator();
  }

  // We have to build a new locator. Extract the paths from the builder.
  SmallVector<LocatorPathElt, 4> path;
  auto anchor = builder.getLocatorParts(path);
  return getConstraintLocator(anchor, path, builder.getSummaryFlags());
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
    ConstraintLocator *locator,
    ArrayRef<ConstraintLocator::PathElement> newElts) {
  auto oldPath = locator->getPath();
  SmallVector<ConstraintLocator::PathElement, 4> newPath;
  newPath.append(oldPath.begin(), oldPath.end());
  newPath.append(newElts.begin(), newElts.end());
  return getConstraintLocator(locator->getAnchor(), newPath);
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
    const ConstraintLocatorBuilder &builder,
    ArrayRef<ConstraintLocator::PathElement> newElts) {
  SmallVector<ConstraintLocator::PathElement, 4> newPath;
  auto anchor = builder.getLocatorParts(newPath);
  newPath.append(newElts.begin(), newElts.end());
  return getConstraintLocator(anchor, newPath);
}

ConstraintLocator *ConstraintSystem::getImplicitValueConversionLocator(
    ConstraintLocatorBuilder root, ConversionRestrictionKind restriction) {
  SmallVector<LocatorPathElt, 4> path;
  auto anchor = root.getLocatorParts(path);
  {
    if (isExpr<DictionaryExpr>(anchor) && path.size() > 1) {
      // Drop everything except for first `tuple element #`.
      path.pop_back_n(path.size() - 1);
    }

    // Drop any value-to-optional conversions that were applied along the
    // way to reach this one.
    while (!path.empty()) {
      if (path.back().is<LocatorPathElt::OptionalInjection>()) {
        path.pop_back();
        continue;
      }
      break;
    }

    // If conversion is for a tuple element, let's drop `TupleType`
    // components from the path since they carry information for
    // diagnostics that `ExprRewriter` won't be able to re-construct
    // during solution application.
    if (!path.empty() && path.back().is<LocatorPathElt::TupleElement>()) {
      path.erase(llvm::remove_if(path,
                                 [](const LocatorPathElt &elt) {
                                   return elt.is<LocatorPathElt::TupleType>();
                                 }),
                 path.end());
    }
  }

  return getConstraintLocator(/*base=*/getConstraintLocator(anchor, path),
                              LocatorPathElt::ImplicitConversion(restriction));
}

ConstraintLocator *ConstraintSystem::getCalleeLocator(
    ConstraintLocator *locator, bool lookThroughApply,
    llvm::function_ref<Type(Expr *)> getType,
    llvm::function_ref<Type(Type)> simplifyType,
    llvm::function_ref<std::optional<SelectedOverload>(ConstraintLocator *)>
        getOverloadFor) {
  if (locator->findLast<LocatorPathElt::ImplicitConversion>())
    return locator;

  auto anchor = locator->getAnchor();
  auto path = locator->getPath();
  {
    // If we have an implicit x[dynamicMember:] subscript call, the callee
    // is given by the original member locator it is based on, which we can get
    // by stripping away the implicit member element and everything after it.
    auto iter = path.rbegin();
    using ImplicitSubscriptElt = LocatorPathElt::ImplicitDynamicMemberSubscript;
    if (locator->findLast<ImplicitSubscriptElt>(iter)) {
      auto newPath = path.drop_back(iter - path.rbegin() + 1);
      return getConstraintLocator(anchor, newPath);
    }
  }

  {
    // If we have a locator for a member found through key path dynamic member
    // lookup, then we need to chop off the elements after the
    // KeyPathDynamicMember element to get the callee locator.
    auto iter = path.rbegin();
    if (locator->findLast<LocatorPathElt::KeyPathDynamicMember>(iter)) {
      auto newPath = path.drop_back(iter - path.rbegin());
      return getConstraintLocator(anchor, newPath);
    }
  }

  {
    // Pattern match is always a callee regardless of what comes after it.
    auto iter = path.rbegin();
    if (locator->findLast<LocatorPathElt::PatternMatch>(iter)) {
      auto newPath = path.drop_back(iter - path.rbegin());
      return getConstraintLocator(anchor, newPath);
    }
  }

  if (locator->findLast<LocatorPathElt::DynamicCallable>()) {
    return getConstraintLocator(anchor, LocatorPathElt::ApplyFunction());
  }

  if (locator->isLastElement<LocatorPathElt::ArgumentAttribute>()) {
    auto argLoc = getConstraintLocator(anchor, path.drop_back());
    return getCalleeLocator(argLoc, lookThroughApply, getType, simplifyType,
                            getOverloadFor);
  }

  // If we have a locator that starts with a key path component element, we
  // may have a callee given by a property, subscript, initializer, or method
  // component.
  if (auto componentElt =
      locator->getFirstElementAs<LocatorPathElt::KeyPathComponent>()) {
    auto *kpExpr = castToExpr<KeyPathExpr>(anchor);
    auto component = kpExpr->getComponents()[componentElt->getIndex()];

    using ComponentKind = KeyPathExpr::Component::Kind;
    switch (component.getKind()) {
    case ComponentKind::UnresolvedSubscript:
    case ComponentKind::Subscript:
      // For a subscript the callee is given by 'component -> subscript member'.
      return getConstraintLocator(
                                  anchor, {*componentElt, ConstraintLocator::SubscriptMember});
    case ComponentKind::UnresolvedMember:
    case ComponentKind::Member:
      // For a property, the choice is just given by the component.
      return getConstraintLocator(anchor, *componentElt);
    case ComponentKind::TupleElement:
      llvm_unreachable("Not implemented by CSGen");
      break;
    case ComponentKind::UnresolvedApply:
    case ComponentKind::Apply:
      return getConstraintLocator(anchor, *componentElt);
    case ComponentKind::Invalid:
    case ComponentKind::OptionalForce:
    case ComponentKind::OptionalChain:
    case ComponentKind::OptionalWrap:
    case ComponentKind::Identity:
    case ComponentKind::DictionaryKey:
    case ComponentKind::CodeCompletion:
      // These components don't have any callee associated, so just continue.
      break;
    }
  }

  // Make sure we handle subscripts before looking at apply exprs. We don't
  // want to return a subscript member locator for an expression such as x[](y),
  // as its callee is not the subscript, but rather the function it returns.
  if (isExpr<SubscriptExpr>(anchor))
    return getConstraintLocator(anchor, ConstraintLocator::SubscriptMember);

  auto getSpecialFnCalleeLoc = [&](Type fnTy) -> ConstraintLocator * {
    fnTy = simplifyType(fnTy);
    // It's okay for function type to contain type variable(s) e.g.
    // opened generic function types, but not to be one.
    assert(!fnTy->is<TypeVariableType>());

    // For an apply of a metatype, we have a short-form constructor. Unlike
    // other locators to callees, these are anchored on the apply expression
    // rather than the function expr.
    if (fnTy->is<AnyMetatypeType>()) {
      return getConstraintLocator(anchor,
                                  {LocatorPathElt::ApplyFunction(),
        LocatorPathElt::ConstructorMember()});
    }

    // Handle an apply of a nominal type which supports callAsFunction.
    if (fnTy->isCallAsFunctionType(DC)) {
      return getConstraintLocator(anchor,
                                  {LocatorPathElt::ApplyFunction(),
        LocatorPathElt::ImplicitCallAsFunction()});
    }

    // Handling an apply for a nominal type that supports @dynamicCallable.
    if (fnTy->hasDynamicCallableAttribute()) {
      return getConstraintLocator(anchor, LocatorPathElt::ApplyFunction());
    }

    return nullptr;
  };

  if (lookThroughApply) {
    if (auto *applyExpr = getAsExpr<ApplyExpr>(anchor)) {
      auto *fnExpr = applyExpr->getFn();

      // Handle special cases for applies of non-function types.
      if (auto *loc = getSpecialFnCalleeLoc(getType(fnExpr)))
        return loc;

      // Otherwise fall through and look for locators anchored on the function
      // expr. For CallExprs, this can look through things like parens and
      // optional chaining.
      if (auto *callExpr = getAsExpr<CallExpr>(anchor)) {
        anchor = callExpr->getDirectCallee();
      } else {
        anchor = fnExpr;
      }
    }
  }

  if (auto *UDE = getAsExpr<UnresolvedDotExpr>(anchor)) {
    if (UDE->isImplicit() &&
        UDE->getName().getBaseName() == Context.Id_callAsFunction) {
      return getConstraintLocator(anchor,
                                  {LocatorPathElt::ApplyFunction(),
        LocatorPathElt::ImplicitCallAsFunction()});
    }

    return getConstraintLocator(
                                anchor, TypeChecker::getSelfForInitDelegationInConstructor(DC, UDE)
                                ? ConstraintLocator::ConstructorMember
                                : ConstraintLocator::Member);
  }

  if (auto *UME = getAsExpr<UnresolvedMemberExpr>(anchor)) {
    return getConstraintLocator(UME, ConstraintLocator::UnresolvedMember);
  }

  if (isExpr<MemberRefExpr>(anchor))
    return getConstraintLocator(anchor, ConstraintLocator::Member);

  if (isExpr<ObjectLiteralExpr>(anchor))
    return getConstraintLocator(anchor, ConstraintLocator::ConstructorMember);

  if (locator->isFirstElement<LocatorPathElt::CoercionOperand>()) {
    auto *CE = castToExpr<CoerceExpr>(anchor);
    locator = getConstraintLocator(CE->getSubExpr()->getValueProvidingExpr(),
                                   path.drop_front());
    return getCalleeLocator(locator, lookThroughApply, getType, simplifyType,
                            getOverloadFor);
  }

  if (auto FVE = getAsExpr<ForceValueExpr>(anchor))
    return getConstraintLocator(FVE->getSubExpr(), ConstraintLocator::Member);

  return getConstraintLocator(anchor);
}

ConstraintLocator *ConstraintSystem::getOpenOpaqueLocator(
    ConstraintLocatorBuilder locator, OpaqueTypeDecl *opaqueDecl) {
  // Use only the opaque type declaration.
  return getConstraintLocator(
      ASTNode(opaqueDecl),
      { LocatorPathElt::OpenedOpaqueArchetype(opaqueDecl) }, 0);
}

std::pair<Type, ExistentialArchetypeType *>
ConstraintSystem::openAnyExistentialType(Type type,
                                         ConstraintLocator *locator) {
  Type result = ExistentialArchetypeType::getAny(type);
  Type t = result;
  while (t->is<MetatypeType>())
    t = t->getMetatypeInstanceType();
  auto *opened = t->castTo<ExistentialArchetypeType>();

  recordOpenedExistentialType(locator, opened);

  return {result, opened};
}

void ConstraintSystem::recordOpenedExistentialType(
    ConstraintLocator *locator,
    ExistentialArchetypeType *opened,
    PreparedOverloadBuilder *preparedOverload) {
  if (preparedOverload) {
    preparedOverload->openedExistentialType(opened);
    return;
  }

  bool inserted = OpenedExistentialTypes.insert({locator, opened}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedOpenedExistentialType(locator));
}

GenericEnvironment *
ConstraintSystem::getPackExpansionEnvironment(PackExpansionExpr *expr) const {
  auto result = PackExpansionEnvironments.find(expr);
  if (result == PackExpansionEnvironments.end())
    return nullptr;

  return result->second;
}

GenericEnvironment *ConstraintSystem::createPackExpansionEnvironment(
    PackExpansionExpr *expr, CanGenericTypeParamType shapeParam) {
  auto *contextEnv = DC->getGenericEnvironmentOfContext();
  auto elementSig = getASTContext().getOpenedElementSignature(
      contextEnv->getGenericSignature().getCanonicalSignature(), shapeParam);
  auto contextSubs = contextEnv->getForwardingSubstitutionMap();
  auto *env = GenericEnvironment::forOpenedElement(elementSig, UUID::fromTime(),
                                                   shapeParam, contextSubs);
  recordPackExpansionEnvironment(expr, env);
  return env;
}

void ConstraintSystem::recordPackExpansionEnvironment(PackExpansionExpr *expr,
                                                      GenericEnvironment *env) {
  bool inserted = PackExpansionEnvironments.insert({expr, env}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedPackExpansionEnvironment(expr));
}

PackExpansionExpr *
ConstraintSystem::getPackElementExpansion(PackElementExpr *packElement) const {
  const auto match = PackElementExpansions.find(packElement);
  return (match == PackElementExpansions.end()) ? nullptr : match->second;
}

void ConstraintSystem::recordPackElementExpansion(
    PackElementExpr *packElement, PackExpansionExpr *packExpansion) {
  bool inserted =
      PackElementExpansions.insert({packElement, packExpansion}).second;
  ASSERT(inserted);

  if (solverState) {
    recordChange(
        SolverTrail::Change::RecordedPackElementExpansion(packElement));
  }
}

/// Extend the given depth map by adding depths for all of the subexpressions
/// of the given expression.
static void extendDepthMap(
   Expr *expr,
   llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &depthMap) {
  // If we already have an entry in the map, we don't need to update it. This
  // avoids invalidating previous entries when solving a smaller component of a
  // larger AST node, e.g during conjunction solving.
  if (depthMap.contains(expr))
    return;

  class RecordingTraversal : public ASTWalker {
    SmallVector<ClosureExpr *, 4> Closures;

  public:
    llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &DepthMap;
    unsigned Depth = 0;

    explicit RecordingTraversal(
        llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &depthMap)
        : DepthMap(depthMap) {}

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::ArgumentsAndExpansion;
    }

    // For argument lists, bump the depth of the arguments, as they are
    // effectively nested within the argument list. It's debatable whether we
    // should actually do this, as it doesn't reflect the true expression depth,
    // but it's needed to preserve compatibility with the behavior from when
    // TupleExpr and ParenExpr were used to represent argument lists.
    PreWalkResult<ArgumentList *>
    walkToArgumentListPre(ArgumentList *ArgList) override {
      ++Depth;
      return Action::Continue(ArgList);
    }
    PostWalkResult<ArgumentList *>
    walkToArgumentListPost(ArgumentList *ArgList) override {
      --Depth;
      return Action::Continue(ArgList);
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
      DepthMap[E] = {Depth, Parent.getAsExpr()};
      ++Depth;

      if (auto CE = dyn_cast<ClosureExpr>(E))
        Closures.push_back(CE);

      return Action::Continue(E);
    }

    PostWalkResult<Expr *> walkToExprPost(Expr *E) override {
      if (auto CE = dyn_cast<ClosureExpr>(E)) {
        assert(Closures.back() == CE);
        Closures.pop_back();
      }

      --Depth;
      return Action::Continue(E);
    }

    PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
      if (auto RS = dyn_cast<ReturnStmt>(S)) {
        // For return statements, treat the parent of the return expression
        // as the closure itself.
        if (RS->hasResult() && !Closures.empty()) {
          llvm::SaveAndRestore<ParentTy> SavedParent(Parent, Closures.back());
          auto E = RS->getResult();
          E->walk(*this);
          return Action::SkipNode(S);
        }
      }

      return Action::Continue(S);
    }
  };

  RecordingTraversal traversal(depthMap);
  expr->walk(traversal);
}

std::optional<std::pair<unsigned, Expr *>>
ConstraintSystem::getExprDepthAndParent(Expr *expr) {
  // Bring the set of expression weights up to date.
  while (NumInputExprsInWeights < InputExprs.size()) {
    extendDepthMap(InputExprs[NumInputExprsInWeights], ExprWeights);
    ++NumInputExprsInWeights;
  }

  auto e = ExprWeights.find(expr);
  if (e != ExprWeights.end())
    return e->second;

  return std::nullopt;
}

std::optional<std::pair<Type, Type>>
ConstraintSystem::isDictionaryType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() == type->getASTContext().getDictionaryDecl()) {
      auto genericArgs = boundStruct->getGenericArgs();
      return std::make_pair(genericArgs[0], genericArgs[1]);
    }
  }

  return std::nullopt;
}

std::optional<Type> ConstraintSystem::isSetType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() == type->getASTContext().getSetDecl())
      return boundStruct->getGenericArgs()[0];
  }

  return std::nullopt;
}

Type ConstraintSystem::getFixedTypeRecursive(Type type, TypeMatchOptions &flags,
                                             bool wantRValue) {
  ASSERT(!PreparingOverload);

  if (wantRValue)
    type = type->getRValueType();

  if (auto depMemType = type->getAs<DependentMemberType>()) {
    auto baseTy = depMemType->getBase();
    if (!baseTy->hasTypeVariable() && !baseTy->hasDependentMember())
      return type;

    // FIXME: Perform a more limited simplification?
    Type newType = simplifyType(type);
    if (newType.getPointer() == type.getPointer()) return type;

    // Once we've simplified a dependent member type, we need to generate a
    // new constraint.
    flags |= TMF_GenerateConstraints;

    return getFixedTypeRecursive(newType, flags, wantRValue);
  }

  // Tuple types can lose their tuple structure under substitution
  // when a parameter pack is substituted with one element.
  if (type->is<TupleType>()) {
    auto simplified = simplifyType(type);
    if (simplified.getPointer() == type.getPointer())
      return type;

    return getFixedTypeRecursive(simplified, flags, wantRValue);
  }

  if (type->is<AnyMetatypeType>()) {
    auto simplified = simplifyType(type);
    if (simplified.getPointer() == type.getPointer())
      return type;

    return getFixedTypeRecursive(simplified, flags, wantRValue);
  }

  if (auto typeVar = type->getAs<TypeVariableType>()) {
    if (auto fixed = getFixedType(typeVar))
      return getFixedTypeRecursive(fixed, flags, wantRValue);

    return getRepresentative(typeVar);
  }

  return type;
}

TypeVariableType *ConstraintSystem::isRepresentativeFor(
    TypeVariableType *typeVar, ConstraintLocator::PathElementKind kind) const {
  // We only attempt to look for this if type variable is
  // a representative.
  if (getRepresentative(typeVar) != typeVar)
    return nullptr;

  auto &CG = const_cast<ConstraintSystem *>(this)->getConstraintGraph();
  auto &result = CG[typeVar];
  auto equivalence = result.getEquivalenceClass();
  auto member = llvm::find_if(equivalence, [=](TypeVariableType *eq) {
    auto *loc = eq->getImpl().getLocator();
    if (!loc)
      return false;

    auto path = loc->getPath();
    return !path.empty() && path.back().getKind() == kind;
  });

  if (member == equivalence.end())
    return nullptr;

  return *member;
}

static std::optional<std::pair<VarDecl *, Type>>
getPropertyWrapperInformationFromOverload(
    SelectedOverload resolvedOverload, DeclContext *DC,
    llvm::function_ref<std::optional<std::pair<VarDecl *, Type>>(VarDecl *)>
        getInformation) {
  if (auto *decl =
          dyn_cast_or_null<VarDecl>(resolvedOverload.choice.getDeclOrNull())) {
    if (auto declInformation = getInformation(decl)) {
      Type type;
      VarDecl *memberDecl;
      std::tie(memberDecl, type) = *declInformation;
      if (Type baseType = resolvedOverload.choice.getBaseType()) {
        type = baseType->getRValueType()->getTypeOfMember(memberDecl, type);
      }
      return std::make_pair(decl, type);
    }
  }
  return std::nullopt;
}

std::optional<std::pair<VarDecl *, Type>>
ConstraintSystem::getPropertyWrapperProjectionInfo(
    SelectedOverload resolvedOverload) {
  return getPropertyWrapperInformationFromOverload(
      resolvedOverload, DC,
      [](VarDecl *decl) -> std::optional<std::pair<VarDecl *, Type>> {
        if (!decl->hasAttachedPropertyWrapper())
          return std::nullopt;

        auto projectionVar = decl->getPropertyWrapperProjectionVar();
        if (!projectionVar)
          return std::nullopt;

        return std::make_pair(projectionVar,
                              projectionVar->getInterfaceType());
      });
}

std::optional<std::pair<VarDecl *, Type>>
ConstraintSystem::getPropertyWrapperInformation(
    SelectedOverload resolvedOverload) {
  return getPropertyWrapperInformationFromOverload(
      resolvedOverload, DC,
      [](VarDecl *decl) -> std::optional<std::pair<VarDecl *, Type>> {
        if (!decl->hasAttachedPropertyWrapper())
          return std::nullopt;

        auto backingTy = decl->getPropertyWrapperBackingPropertyType();
        if (!backingTy)
          return std::nullopt;

        return std::make_pair(decl, backingTy);
      });
}

std::optional<std::pair<VarDecl *, Type>>
ConstraintSystem::getWrappedPropertyInformation(
    SelectedOverload resolvedOverload) {
  return getPropertyWrapperInformationFromOverload(
      resolvedOverload, DC,
      [](VarDecl *decl) -> std::optional<std::pair<VarDecl *, Type>> {
        if (auto wrapped = decl->getOriginalWrappedProperty())
          return std::make_pair(decl, wrapped->getInterfaceType());

        return std::nullopt;
      });
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      DeclContext *useDC,
                                      ConstraintLocator *locator,
                                      std::optional<unsigned> favoredIndex) {
  // If there is a single choice, add the bind overload directly.
  if (choices.size() == 1) {
    addBindOverloadConstraint(boundType, choices.front(), locator, useDC);
    return;
  }

  SmallVector<Constraint *, 4> candidates;
  generateOverloadConstraints(candidates, boundType, choices, useDC, locator,
                              favoredIndex);
  // For an overload set (disjunction) from newly generated candidates.
  addOverloadSet(candidates, locator);
}

void ConstraintSystem::addOverloadSet(ArrayRef<Constraint *> choices,
                                      ConstraintLocator *locator) {
  assert(!choices.empty() && "Empty overload set");

  // If there is a single choice, attempt it right away.
  if (choices.size() == 1) {
    simplifyConstraint(*choices.front());
    return;
  }

  auto *disjunction =
      Constraint::createDisjunction(*this, choices, locator, ForgetChoice);
  addUnsolvedConstraint(disjunction);
  if (simplifyAppliedOverloads(disjunction, locator))
    retireFailedConstraint(disjunction);
}

FunctionType::ExtInfo ConstraintSystem::closureEffects(ClosureExpr *expr) {
  return evaluateOrDefault(
      getASTContext().evaluator, ClosureEffectsRequest{expr},
      FunctionType::ExtInfo());
}

FunctionType::ExtInfo ClosureEffectsRequest::evaluate(
  Evaluator &evaluator, ClosureExpr *expr) const {
  // A walker that looks for 'try' and 'throw' expressions
  // that aren't nested within closures, nested declarations,
  // or exhaustive catches.
  class FindInnerThrows : public ASTWalker {
    DeclContext *DC;
    bool FoundThrow = false;

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::Expansion;
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
      // If we've found a 'try', record it and terminate the traversal.
      if (isa<TryExpr>(expr)) {
        FoundThrow = true;
        return Action::Stop();
      }

      // Don't walk into a 'try!' or 'try?'.
      if (isa<ForceTryExpr>(expr) || isa<OptionalTryExpr>(expr)) {
        return Action::SkipNode(expr);
      }

      // Do not recurse into other closures.
      if (isa<ClosureExpr>(expr))
        return Action::SkipNode(expr);

      return Action::Continue(expr);
    }

    PreWalkAction walkToDeclPre(Decl *decl) override {
      // Do not walk into function or type declarations.
      return Action::VisitNodeIf(isa<PatternBindingDecl>(decl));
    }

    bool isSyntacticallyExhaustive(DoCatchStmt *stmt) {
      for (auto catchClause : stmt->getCatches()) {
        for (auto &LabelItem : catchClause->getMutableCaseLabelItems()) {
          if (isSyntacticallyExhaustive(catchClause->getStartLoc(),
                                        LabelItem))
            return true;
        }
      }

      return false;
    }

    bool isSyntacticallyExhaustive(SourceLoc CatchLoc,
                                   CaseLabelItem &LabelItem) {
      // If it's obviously non-exhaustive, great.
      if (LabelItem.getGuardExpr())
        return false;

      // If we can show that it's exhaustive without full
      // type-checking, great.
      if (LabelItem.isSyntacticallyExhaustive())
        return true;

      // Okay, resolve the pattern.
      Pattern *pattern = LabelItem.getPattern();
      if (!LabelItem.isPatternResolved()) {
        pattern = TypeChecker::resolvePattern(pattern, DC,
                                       /*isStmtCondition*/false);
        if (!pattern) return false;

        // Save that aside while we explore the type.
        LabelItem.setPattern(pattern, /*resolved=*/true);
      }

      // Require the pattern to have a particular shape: a number
      // of is-patterns applied to an irrefutable pattern.
      pattern = pattern->getSemanticsProvidingPattern();
      while (auto isp = dyn_cast<IsPattern>(pattern)) {
        Type castType;
        if (auto castTypeRepr = isp->getCastTypeRepr()) {
          castType = TypeResolution::resolveContextualType(
              castTypeRepr, DC, TypeResolverContext::InExpression,
              /*unboundTyOpener*/ nullptr,
              /*placeholderHandler*/ nullptr,
              /*packElementOpener*/ nullptr);
        } else {
          castType = isp->getCastType();
        }

        if (castType->hasError()) {
          return false;
        }

        if (!isp->hasSubPattern()) {
          pattern = nullptr;
          break;
        } else {
          pattern = isp->getSubPattern()->getSemanticsProvidingPattern();
        }
      }
      if (pattern && pattern->isRefutablePattern()) {
        return false;
      }

      // Okay, now it should be safe to coerce the pattern.
      // Pull the top-level pattern back out.
      pattern = LabelItem.getPattern();

      auto &ctx = DC->getASTContext();
      if (!ctx.getErrorDecl())
        return false;

      auto contextualPattern =
          ContextualPattern::forRawPattern(pattern, DC);
      pattern = TypeChecker::coercePatternToType(
        contextualPattern, ctx.getErrorExistentialType(),
        TypeResolverContext::InExpression);
      if (!pattern)
        return false;

      LabelItem.setPattern(pattern, /*resolved=*/true);
      return LabelItem.isSyntacticallyExhaustive();
    }

    PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
      // If we've found a 'throw', record it and terminate the traversal.
      if (isa<ThrowStmt>(stmt)) {
        FoundThrow = true;
        return Action::Stop();
      }

      // Handle do/catch differently.
      if (auto doCatch = dyn_cast<DoCatchStmt>(stmt)) {
        // Only walk into the 'do' clause of a do/catch statement
        // if the catch isn't syntactically exhaustive.
        if (!isSyntacticallyExhaustive(doCatch)) {
          if (!doCatch->getBody()->walk(*this))
            return Action::Stop();
        }

        // Walk into all the catch clauses.
        for (auto catchClause : doCatch->getCatches()) {
          if (!catchClause->walk(*this))
            return Action::Stop();
        }

        // We've already walked all the children we care about.
        return Action::SkipNode(stmt);
      }

      if (auto forEach = dyn_cast<ForEachStmt>(stmt)) {
        if (forEach->getTryLoc().isValid()) {
          FoundThrow = true;
          return Action::Stop();
        }
      }

      return Action::Continue(stmt);
    }

  public:
    FindInnerThrows(DeclContext *dc)
        : DC(dc) {}

    bool foundThrow() { return FoundThrow; }
  };

  // If either 'throws' or 'async' was explicitly specified, use that
  // set of effects.
  bool throws = expr->getThrowsLoc().isValid();
  bool async = expr->getAsyncLoc().isValid();
  bool sendable = expr->getAttrs().hasAttribute<SendableAttr>();

  if (throws || async) {
    return ASTExtInfoBuilder()
      .withThrows(throws, /*FIXME:*/Type())
      .withAsync(async)
      .withSendable(sendable)
      .build();
  }

  // Scan the body to determine the effects.
  auto body = expr->getBody();
  if (!body)
    return ASTExtInfoBuilder().withSendable(sendable).build();

  auto throwFinder = FindInnerThrows(expr);
  body->walk(throwFinder);
  return ASTExtInfoBuilder()
      .withThrows(throwFinder.foundThrow(), /*FIXME:*/Type())
      .withAsync(bool(findAsyncNode(expr)))
      .withSendable(sendable)
      .build();
}

bool ConstraintSystem::isAsynchronousContext(DeclContext *dc) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(dc))
    return func->isAsyncContext();

  if (auto closure = dyn_cast<ClosureExpr>(dc)) {
    return evaluateOrDefault(
        getASTContext().evaluator,
        ClosureEffectsRequest{const_cast<ClosureExpr *>(closure)},
        FunctionType::ExtInfo()).isAsync();
  }

  return false;
}

void ConstraintSystem::buildDisjunctionForOptionalVsUnderlying(
    Type boundTy, Type ty, ConstraintLocator *locator) {
  // NOTE: If we use other locator kinds for these disjunctions, we
  // need to account for it in solution scores for forced-unwraps.
  assert(locator->getPath().back().getKind() ==
             ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice ||
         locator->getPath().back().getKind() ==
             ConstraintLocator::DynamicLookupResult);
  assert(!ty->is<InOutType>());
  auto rvalueTy = ty->getWithoutSpecifierType();

  // If the type to bind is a placeholder, we can propagate it, as we don't know
  // whether it can be optional or non-optional, and we would have already
  // recorded a fix for it.
  if (rvalueTy->isPlaceholder()) {
    addConstraint(ConstraintKind::Bind, boundTy, ty, locator);
    return;
  }

  // Create the constraint to bind to the optional type and make it the favored
  // choice.
  auto *bindToOptional =
      Constraint::create(*this, ConstraintKind::Bind, boundTy, ty, locator);
  bindToOptional->setFavored();

  auto underlyingType = rvalueTy->getOptionalObjectType();
  if (!underlyingType) {
    // If we don't have an optional, `ty` hasn't been resolved yet.
    auto *typeVar = rvalueTy->castTo<TypeVariableType>();
    auto *locator = typeVar->getImpl().getLocator();

    // We need to allocate a type variable to represent an object type of a
    // future optional, and add a constraint between `ty` and `underlyingType`
    // to model it.
    underlyingType = createTypeVariable(
        getConstraintLocator(locator, LocatorPathElt::GenericArgument(0)),
        TVO_PrefersSubtypeBinding | TVO_CanBindToLValue |
            TVO_CanBindToNoEscape);

    // Using a `typeVar` here because l-value is going to be applied
    // to the underlying type below.
    addConstraint(ConstraintKind::OptionalObject, typeVar, underlyingType,
                  locator);
  }
  if (ty->is<LValueType>())
    underlyingType = LValueType::get(underlyingType);

  auto *bindToUnderlying = Constraint::create(*this, ConstraintKind::Bind,
                                              boundTy, underlyingType, locator);

  llvm::SmallVector<Constraint *, 2> choices = {bindToOptional,
                                                bindToUnderlying};

  // Create the disjunction
  addDisjunctionConstraint(choices, locator, RememberChoice);
}

namespace {

struct TypeSimplifier {
  ConstraintSystem &CS;
  llvm::function_ref<Type(TypeVariableType *)> GetFixedTypeFn;

  struct ActivePackExpansion {
    bool isPackExpansion = false;
    unsigned index = 0;
  };
  SmallVector<ActivePackExpansion, 4> ActivePackExpansions;

  TypeSimplifier(ConstraintSystem &CS,
                 llvm::function_ref<Type(TypeVariableType *)> getFixedTypeFn)
    : CS(CS), GetFixedTypeFn(getFixedTypeFn) {}

  std::optional<Type> operator()(TypeBase *type) {
    if (auto tvt = dyn_cast<TypeVariableType>(type)) {
      auto fixedTy = GetFixedTypeFn(tvt);

      // TODO: the following logic should be applied when rewriting
      // PackElementType.
      if (ActivePackExpansions.empty()) {
        return fixedTy;
      }

      if (auto fixedPack = fixedTy->getAs<PackType>()) {
        auto &activeExpansion = ActivePackExpansions.back();
        if (activeExpansion.index >= fixedPack->getNumElements()) {
          return std::nullopt;
        }

        auto fixedElt = fixedPack->getElementType(activeExpansion.index);
        auto fixedExpansion = fixedElt->getAs<PackExpansionType>();
        if (activeExpansion.isPackExpansion && fixedExpansion) {
          return fixedExpansion->getPatternType();
        } else if (!activeExpansion.isPackExpansion && !fixedExpansion) {
          return fixedElt;
        } else {
          return std::nullopt;
        }
      }

      return fixedTy;
    }

    if (auto tuple = dyn_cast<TupleType>(type)) {
      if (tuple->getNumElements() == 1) {
        auto element = tuple->getElement(0);
        auto elementType = element.getType();
        auto resolvedType = elementType.transformRec(*this);

        // If this is a single-element tuple with pack expansion
        // variable inside, let's unwrap it if pack is flattened.
        if (!element.hasName()) {
          if (auto *typeVar = elementType->getAs<TypeVariableType>()) {
            if (typeVar->getImpl().isPackExpansion() &&
                !resolvedType->isEqual(typeVar) &&
                !resolvedType->is<PackExpansionType>() &&
                !resolvedType->is<PackType>() &&
                !resolvedType->is<PackArchetypeType>()) {
              return resolvedType;
            }
          }
        }

        // Flatten single-element tuples containing type variables that cannot
        // bind to packs.
        auto typeVar = resolvedType->getAs<TypeVariableType>();
        if (!element.hasName() && typeVar &&
            !typeVar->getImpl().canBindToPack() &&
            !typeVar->getImpl().isPackExpansion()) {
          return typeVar;
        }
      }
    }

    if (auto expansion = dyn_cast<PackExpansionType>(type)) {
      auto patternType = expansion->getPatternType();
      // First, let's check whether pattern type has all of the type variables
      // that represent packs resolved, otherwise we don't have enough information
      // to flatten this pack expansion type.
      //
      // Note that we don't actually need to do deep transformation here
      // because pack variables can only appear in structural positions.
      if (patternType.findIf([&](Type type) {
            if (auto *typeVar = type->getAs<TypeVariableType>()) {
              if (typeVar->getImpl().canBindToPack())
                return GetFixedTypeFn(typeVar)->is<TypeVariableType>();
            }
            return false;
          })) {
        return std::nullopt;
      }

      // Transform the count type, ignoring any active pack expansions.
      auto countType = expansion->getCountType().transformRec(
          TypeSimplifier(CS, GetFixedTypeFn));

      if (!countType->is<PackType>() &&
          !countType->is<PackArchetypeType>()) {
        SmallVector<Type, 2> rootParameterPacks;
        countType->getTypeParameterPacks(rootParameterPacks);
        if (!rootParameterPacks.empty())
          countType = rootParameterPacks[0];
      }

      // If both pattern and count are resolves, let's just return
      // the pattern type for `transformWithPosition` to take care
      // of the rest.
      if (patternType->is<PackType>() && countType->is<PackType>())
        return patternType;

      if (auto countPack = countType->getAs<PackType>()) {
        SmallVector<Type, 4> elts;
        ActivePackExpansions.push_back({false, 0});
        for (auto countElt : countPack->getElementTypes()) {
          auto countExpansion = countElt->getAs<PackExpansionType>();
          ActivePackExpansions.back().isPackExpansion =
            (countExpansion != nullptr);

          auto elt = expansion->getPatternType().transformRec(*this);
          if (countExpansion)
            elt = PackExpansionType::get(elt, countExpansion->getCountType());
          elts.push_back(elt);

          ActivePackExpansions.back().index++;
        }
        ActivePackExpansions.pop_back();

        if (elts.size() == 1)
          return elts[0];
        return PackType::get(CS.getASTContext(), elts);
      } else {
        ActivePackExpansions.push_back({true, 0});
        auto patternType = expansion->getPatternType().transformRec(*this);
        ActivePackExpansions.pop_back();
        return PackExpansionType::get(patternType, countType);
      }
    }

    // If this is a dependent member type for which we end up simplifying
    // the base to a non-type-variable, perform lookup.
    if (auto depMemTy = dyn_cast<DependentMemberType>(type)) {
      // Simplify the base.
      Type newBase = depMemTy->getBase().transformRec(*this);

      if (newBase->isPlaceholder()) {
        return PlaceholderType::get(CS.getASTContext(), depMemTy);
      }

      // If nothing changed, we're done.
      if (newBase.getPointer() == depMemTy->getBase().getPointer())
        return std::nullopt;

      // Dependent member types should only be created for associated types.
      auto assocType = depMemTy->getAssocType();
      assert(depMemTy->getAssocType() && "Expected associated type!");

      // FIXME: It's kind of weird in general that we have to look
      // through lvalue, inout and IUO types here
      Type lookupBaseType = newBase->getWithoutSpecifierType();
      if (auto selfType = lookupBaseType->getAs<DynamicSelfType>())
        lookupBaseType = selfType->getSelfType();

      if (lookupBaseType->mayHaveMembers() ||
          lookupBaseType->is<PackType>()) {
        auto *proto = assocType->getProtocol();
        auto conformance = CS.lookupConformance(lookupBaseType, proto);
        if (!conformance) {
          // Special case: When building slab literals, we go through the same
          // array literal machinery, so there will be a conversion constraint
          // for the element to ExpressibleByArrayLiteral.ArrayLiteralType.
          if (lookupBaseType->isInlineArray()) {
            auto &ctx = CS.getASTContext();
            auto arrayProto =
                ctx.getProtocol(KnownProtocolKind::ExpressibleByArrayLiteral);
            auto elementAssocTy = arrayProto->getAssociatedTypeMembers()[0];

            if (proto == arrayProto && assocType == elementAssocTy) {
              return lookupBaseType->getInlineArrayElementType();
            }
          }

          // If the base type doesn't conform to the associatedtype's protocol,
          // there will be a missing conformance fix applied in diagnostic mode,
          // so the concrete dependent member type is considered a "hole" in
          // order to continue solving.
          auto memberTy = DependentMemberType::get(lookupBaseType, assocType);
          if (CS.shouldAttemptFixes() &&
              CS.getPhase() == ConstraintSystemPhase::Solving) {
            return PlaceholderType::get(CS.getASTContext(), memberTy);
          }

          return memberTy;
        }

        auto result = conformance.getTypeWitness(assocType);
        if (result && !result->hasError())
          return result;
      }

      return DependentMemberType::get(lookupBaseType, assocType);
    }

    return std::nullopt;
  }
};

} // end anonymous namespace

Type ConstraintSystem::simplifyTypeImpl(Type type,
    llvm::function_ref<Type(TypeVariableType *)> getFixedTypeFn) {
  return type.transformRec(TypeSimplifier(*this, getFixedTypeFn));
}

Type ConstraintSystem::simplifyType(Type type) {
  ASSERT(!PreparingOverload);

  if (!type->hasTypeVariable())
    return type;

  // Map type variables down to the fixed types of their representatives.
  return simplifyTypeImpl(type,
      [&](TypeVariableType *tvt) -> Type {
        if (auto fixed = getFixedType(tvt))
          return simplifyType(fixed);

        return getRepresentative(tvt);
      });
}

void Solution::recordSingleArgMatchingChoice(ConstraintLocator *locator) {
  auto &cs = getConstraintSystem();
  assert(argumentMatchingChoices.find(locator) ==
             argumentMatchingChoices.end() &&
         "recording multiple bindings for same locator");
  argumentMatchingChoices.insert(
      {cs.getConstraintLocator(locator, ConstraintLocator::ApplyArgument),
       MatchCallArgumentResult::forArity(1)});
}

Type Solution::simplifyType(Type type, bool wantInterfaceType) const {
  // If we've been asked for an interface type, start by mapping any archetypes
  // out of context.
  if (wantInterfaceType)
    type = type->mapTypeOutOfContext();

  if (!type->hasTypeVariableOrPlaceholder())
    return type;

  // Map type variables to fixed types from bindings.
  auto &cs = getConstraintSystem();
  auto resolvedType =
      cs.simplifyTypeImpl(type, [&](TypeVariableType *tvt) -> Type {
        // If we want the interface type, use the generic parameter if we
        // have one, otherwise map the fixed type out of context.
        if (wantInterfaceType) {
          if (auto *gp = tvt->getImpl().getGenericParameter())
            return gp;
          return getFixedType(tvt)->mapTypeOutOfContext();
        }
        return getFixedType(tvt);
      });
  ASSERT(!(wantInterfaceType && resolvedType->hasPrimaryArchetype()));

  // We may have type variables and placeholders left over. These are solver
  // allocated so cannot escape this function. Turn them into UnresolvedType.
  // - Type variables may still be present from unresolved pack expansions where
  //   e.g the count type is a hole, so the pattern may never become a
  //   concrete type.
  // - Placeholders may be present for any holes.
  if (resolvedType->hasTypeVariableOrPlaceholder()) {
    auto &ctx = cs.getASTContext();
    resolvedType =
        resolvedType.transformRec([&](Type type) -> std::optional<Type> {
          if (!type->hasTypeVariableOrPlaceholder())
            return type;

          auto *typePtr = type.getPointer();
          if (isa<TypeVariableType>(typePtr) || isa<PlaceholderType>(typePtr))
            return Type(ctx.TheUnresolvedType);

          return std::nullopt;
        });
  }

  ASSERT(!resolvedType->getRecursiveProperties().isSolverAllocated());
  return resolvedType;
}

Type Solution::simplifyTypeForCodeCompletion(Type Ty) const {
  auto &CS = getConstraintSystem();

  // First, instantiate all type variables that we know, but don't replace
  // placeholders by unresolved types.
  Ty = CS.simplifyTypeImpl(Ty, [this](TypeVariableType *typeVar) -> Type {
    return getFixedType(typeVar);
  });

  // Next, replace all placeholders by type variables. We know that all type
  // variables now in the type originate from placeholders.
  Ty = Ty.transformRec([](Type type) -> std::optional<Type> {
    if (auto *placeholder = type->getAs<PlaceholderType>()) {
      if (auto *typeVar =
              placeholder->getOriginator().dyn_cast<TypeVariableType *>()) {
        return Type(typeVar);
      }
    }

    return std::nullopt;
  });

  // Replace all type variables (which must come from placeholders) by their
  // generic parameters. Because we call into simplifyTypeImpl
  Ty = CS.simplifyTypeImpl(Ty, [&CS, this](TypeVariableType *typeVar) -> Type {
    // Code completion depends on generic parameter type being represented in
    // terms of `ArchetypeType` since it's easy to extract protocol requirements
    // from it.
    auto getTypeVarAsArchetype = [](TypeVariableType *typeVar) -> Type {
      if (auto *GP = typeVar->getImpl().getGenericParameter()) {
        if (auto *GPD = GP->getDecl()) {
          return GPD->getInnermostDeclContext()->mapTypeIntoContext(GP);
        }
      }
      return Type();
    };

    if (auto archetype = getTypeVarAsArchetype(typeVar)) {
      return archetype;
    }

    // Sometimes the type variable itself doesn't have have an originator that
    // can be replaced by an archetype but one of its equivalent type variable
    // does.
    // Search thorough all equivalent type variables, looking for one that can
    // be replaced by a generic parameter.
    std::vector<std::pair<TypeVariableType *, Type>> bindings(
        typeBindings.begin(), typeBindings.end());
    // Make sure we iterate the bindings in a deterministic order.
    llvm::sort(bindings, [](const std::pair<TypeVariableType *, Type> &lhs,
                            const std::pair<TypeVariableType *, Type> &rhs) {
      return lhs.first->getID() < rhs.first->getID();
    });
    for (auto binding : bindings) {
      if (auto placeholder = binding.second->getAs<PlaceholderType>()) {
        if (placeholder->getOriginator().dyn_cast<TypeVariableType *>() ==
            typeVar) {
          if (auto archetype = getTypeVarAsArchetype(binding.first)) {
            return archetype;
          }
        }
      }
    }

    // When applying the logic below to get contextual types inside result
    // builders, the code completion type variable is connected by a one-way
    // constraint to a type variable in the buildBlock call, but that is not the
    // type variable that represents the argument type. We need to find the type
    // variable representing the argument to retrieve protocol requirements from
    // it. Look for a ArgumentConversion constraint that allows us to retrieve
    // the argument type var.
    auto &cg = CS.getConstraintGraph();

    // FIXME: The type variable is not going to be part of the constraint graph
    // at this point unless it was created at the outermost decision level;
    // otherwise it has already been rolled back! Work around this by creating
    // an empty node if one doesn't exist.
    cg.addTypeVariable(typeVar);

    for (auto argConstraint : cg[typeVar].getConstraints()) {
      if (argConstraint->getKind() == ConstraintKind::ArgumentConversion &&
          argConstraint->getFirstType()->getRValueType()->isEqual(typeVar)) {
        if (auto argTV =
                argConstraint->getSecondType()->getAs<TypeVariableType>()) {
          if (auto archetype = getTypeVarAsArchetype(argTV)) {
            return archetype;
          }
        }
      }
    }

    return typeVar;
  });

  // Logic to determine the contextual type inside buildBlock result builders:
  //
  // When completing inside a result builder, the result builder
  //   @ViewBuilder var body: some View {
  //     Text("Foo")
  //     #^COMPLETE^#
  //   }
  // gets rewritten to
  //   @ViewBuilder var body: some View {
  //     let $__builder2: Text
  //     let $__builder0 = Text("Foo")
  //     let $__builder1 = #^COMPLETE^#
  //     $__builder2 = ViewBuilder.buildBlock($__builder0, $__builder1)
  //     return $__builder2
  //   }
  // Inside the constraint system
  //     let $__builder1 = #^COMPLETE^#
  // gets type checked without context, so we can't know the contextual type for
  // the code completion token. But we know that $__builder1 (and thus the type
  // of #^COMPLETE^#) is used as the second argument to ViewBuilder.buildBlock,
  // so we can extract the contextual type from that call. To do this, figure
  // out the type variable that is used for $__builder1 in the buildBlock call.
  // This type variable is connected to the type variable of $__builder1's
  // definition by a one-way constraint.
  if (auto TV = Ty->getAs<TypeVariableType>()) {
    for (auto constraint : CS.getConstraintGraph()[TV].getConstraints()) {
      if (constraint->getKind() == ConstraintKind::OneWayEqual &&
          constraint->getSecondType()->isEqual(TV)) {
        return simplifyTypeForCodeCompletion(constraint->getFirstType());
      }
    }
  }

  // Remove any remaining type variables and placeholders
  Ty = simplifyType(Ty);

  return Ty->getRValueType();
}

template <typename T>
static inline size_t size_in_bytes(const T &x) {
  return (x.size() * (sizeof(typename T::key_type) + sizeof(unsigned))) +
         (x.size() * (sizeof(typename T::value_type)));
}

size_t Solution::getTotalMemory() const {
  if (TotalMemory)
    return *TotalMemory;

  // clang-format off
  const_cast<Solution *>(this)->TotalMemory
       = sizeof(*this) + size_in_bytes(typeBindings) +
         overloadChoices.getMemorySize() +
         ConstraintRestrictions.getMemorySize() +
         (Fixes.size() * sizeof(void *)) + DisjunctionChoices.getMemorySize() +
         AppliedDisjunctions.getMemorySize() +
         OpenedTypes.getMemorySize() + OpenedExistentialTypes.getMemorySize() +
         OpenedPackExpansionTypes.getMemorySize() +
         PackExpansionEnvironments.getMemorySize() +
         size_in_bytes(PackElementExpansions) +
         (DefaultedConstraints.size() * sizeof(void *)) +
         nodeTypes.getMemorySize() +
         keyPathComponentTypes.getMemorySize() +
         size_in_bytes(KeyPaths) +
         (contextualTypes.size() * sizeof(ASTNode))  +
         size_in_bytes(targets) +
         size_in_bytes(caseLabelItems) +
         size_in_bytes(exprPatterns) +
         size_in_bytes(isolatedParams) +
         size_in_bytes(preconcurrencyClosures) +
         size_in_bytes(resultBuilderTransformed) +
         size_in_bytes(appliedPropertyWrappers) +
         size_in_bytes(argumentLists) +
         size_in_bytes(ImplicitCallAsFunctionRoots) +
         size_in_bytes(SynthesizedConformances);
  // clang-format on

  return *TotalMemory;
}

DeclContext *Solution::getDC() const { return constraintSystem->DC; }

DeclName OverloadChoice::getName() const {
  switch (getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::DeclViaBridge:
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
      return getDecl()->getName();

    case OverloadChoiceKind::KeyPathApplication:
      // TODO: This should probably produce subscript(keyPath:), but we
      // don't currently pre-filter subscript overload sets by argument
      // keywords, so "subscript" is still the name that keypath subscripts
      // are looked up by.
      return DeclBaseName::createSubscript();

    case OverloadChoiceKind::DynamicMemberLookup:
    case OverloadChoiceKind::KeyPathDynamicMemberLookup:
      return DeclName(DynamicMember.getPointer());

    case OverloadChoiceKind::MaterializePack:
    case OverloadChoiceKind::TupleIndex:
    case OverloadChoiceKind::ExtractFunctionIsolation:
      llvm_unreachable("no name!");
  }

  llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
}

std::optional<IUOReferenceKind>
OverloadChoice::getIUOReferenceKind(ConstraintSystem &cs,
                                    bool forSecondApplication) const {
  auto *decl = getDeclOrNull();
  if (!decl || !decl->isImplicitlyUnwrappedOptional())
    return std::nullopt;

  // If this isn't an IUO return () -> T!, it's an IUO value.
  if (!decl->getInterfaceType()->is<AnyFunctionType>())
    return IUOReferenceKind::Value;

  auto refKind = getFunctionRefInfo();
  assert(!forSecondApplication || refKind.isDoubleApply());

  switch (refKind.getApplyLevel()) {
  case FunctionRefInfo::ApplyLevel::Unapplied:
    // Such references never produce IUOs.
    return std::nullopt;
  case FunctionRefInfo::ApplyLevel::SingleApply:
  case FunctionRefInfo::ApplyLevel::DoubleApply:
    // Check whether this is a curried function reference e.g
    // (Self) -> (Args...) -> Ret. Such a function reference can only produce
    // an IUO on the second application.
    auto isCurried = decl->hasCurriedSelf() && !hasAppliedSelf(cs, *this);
    if (forSecondApplication != isCurried)
      return std::nullopt;
    break;
  }
  return IUOReferenceKind::ReturnValue;
}

SolutionResult ConstraintSystem::salvage() {
  if (isDebugMode()) {
    llvm::errs() << "---Attempting to salvage and emit diagnostics---\n";
  }

  setPhase(ConstraintSystemPhase::Diagnostics);

  // Attempt to solve again, capturing all states that come from our attempts to
  // select overloads or bind type variables.
  //
  // FIXME: can this be removed?  We need to arrange for recordFixes to be
  // eliminated.
  SmallVector<Solution, 2> viable;
  viable.clear();

  {
    // Set up solver state.
    SolverState state(*this, FreeTypeVariableBinding::Disallow);
    state.recordFixes = true;

    // Solve the system.
    solveImpl(viable);

    // If we hit a threshold, we're done.
    if (isTooComplex(viable))
      return SolutionResult::forTooComplex(getTooComplexRange());

    // Before removing any "fixed" solutions, let's check
    // if ambiguity is caused by fixes and diagnose if possible.
    if (diagnoseAmbiguityWithFixes(viable))
      return SolutionResult::forAmbiguous(viable);

    // Check whether we have a best solution; this can happen if we found
    // a series of fixes that worked.
    if (auto best = findBestSolution(viable, /*minimize=*/true)) {
      if (*best != 0)
        viable[0] = std::move(viable[*best]);
      viable.erase(viable.begin() + 1, viable.end());
      return SolutionResult::forSolved(std::move(viable[0]));
    }

    if (shouldSuppressDiagnostics())
      return viable.empty() ? SolutionResult::forUndiagnosedError()
                            : SolutionResult::forAmbiguous(viable);

    // FIXME: If we were able to actually fix things along the way,
    // we may have to hunt for the best solution. For now, we don't care.

    // Remove solutions that require fixes; the fixes in those systems should
    // be diagnosed rather than any ambiguity.
    auto hasFixes = [](const Solution &sol) { return !sol.Fixes.empty(); };
    auto newEnd = std::remove_if(viable.begin(), viable.end(), hasFixes);
    viable.erase(newEnd, viable.end());

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (viable.size() > 1) {
      if (isDebugMode()) {
        auto &log = llvm::errs();
        log << "---Ambiguity error: " << viable.size()
            << " solutions found---\n";
        int i = 0;
        for (auto &solution : viable) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(log, solverState->getCurrentIndent());
          log << "\n";
        }
      }

      if (diagnoseAmbiguity(viable)) {
        return SolutionResult::forAmbiguous(viable);
      }
    }

    // Fall through to produce diagnostics.
  }

  // Could not produce a specific diagnostic; punt to the client.
  return SolutionResult::forUndiagnosedError();
}

static void diagnoseOperatorAmbiguity(ConstraintSystem &cs,
                                      Identifier operatorName,
                                      ArrayRef<Solution> solutions,
                                      ConstraintLocator *locator) {
  auto &ctx = cs.getASTContext();
  auto &DE = ctx.Diags;
  auto *anchor = castToExpr(locator->getAnchor());
  auto *applyExpr = cast<ApplyExpr>(cs.getParentExpr(anchor));

  auto isEnumWithAssociatedValues = [](Type type) -> bool {
    if (auto *enumType = type->getAs<EnumType>())
      return !enumType->getDecl()->hasOnlyCasesWithoutAssociatedValues();
    return false;
  };

  const auto &solution = solutions.front();
  if (auto *binaryOp = dyn_cast<BinaryExpr>(applyExpr)) {
    auto *lhs = binaryOp->getLHS();
    auto *rhs = binaryOp->getRHS();

    auto lhsType =
        solution.simplifyType(solution.getType(lhs))->getRValueType();
    auto rhsType =
        solution.simplifyType(solution.getType(rhs))->getRValueType();

    if (lhsType->isEqual(rhsType)) {
      DE.diagnose(anchor->getLoc(), diag::cannot_apply_binop_to_same_args,
                  operatorName.str(), lhsType)
          .highlight(lhs->getSourceRange())
          .highlight(rhs->getSourceRange());

      if (isStandardComparisonOperator(binaryOp->getFn()) &&
          isEnumWithAssociatedValues(lhsType)) {
        DE.diagnose(applyExpr->getLoc(),
                    diag::no_binary_op_overload_for_enum_with_payload,
                    operatorName.str());
        return;
      }
    } else if (operatorName == ctx.Id_MatchOperator) {
      DE.diagnose(anchor->getLoc(), diag::cannot_match_expr_pattern_with_value,
                  lhsType, rhsType);
    } else {
      DE.diagnose(anchor->getLoc(), diag::cannot_apply_binop_to_args,
                  operatorName.str(), lhsType, rhsType)
          .highlight(lhs->getSourceRange())
          .highlight(rhs->getSourceRange());
    }
  } else {
    auto *arg = applyExpr->getArgs()->getUnlabeledUnaryExpr();
    assert(arg && "Expected a unary arg");
    auto argType = solution.simplifyType(solution.getType(arg));
    DE.diagnose(anchor->getLoc(), diag::cannot_apply_unop_to_arg,
                operatorName.str(), argType->getRValueType());
  }

  std::set<std::string> parameters;
  for (const auto &solution : solutions) {
    auto overload = solution.getOverloadChoice(locator);
    auto overloadType = overload.adjustedOpenedType;
    // Let's suggest only concrete overloads here.
    // Notes are going to take care of the rest,
    // since printing types like `(Self, Self)` is not
    // really useful.
    if (overloadType->hasTypeVariable())
      continue;

    auto overloadFnTy = overloadType->getAs<FunctionType>();
    if (!overloadFnTy)
      continue;

    // If arguments to all parameters have been fixed then there is nothing
    // to note about in this overload.
    std::set<unsigned> fixedParams;
    llvm::for_each(solution.Fixes, [&](const ConstraintFix *fix) {
      auto *locator = fix->getLocator();
      if (getAsExpr(locator->getAnchor()) != applyExpr)
        return;

      if (auto argLoc = locator->findLast<LocatorPathElt::ApplyArgToParam>()) {
        fixedParams.insert(argLoc->getParamIdx());
      }
    });

    if (fixedParams.size() == overloadFnTy->getNumParams())
      continue;

    parameters.insert(
        FunctionType::getParamListAsString(overloadFnTy->getParams()));
  }

  // All of the overload choices had generic parameters like `Self`.
  if (parameters.empty())
    return;

  DE.diagnose(anchor->getLoc(), diag::suggest_partial_overloads,
              /*isResult=*/false, operatorName.str(),
              llvm::join(parameters, ", "));
}

std::string swift::describeGenericType(ValueDecl *GP, bool includeName) {
  if (!GP)
    return "";

  Decl *parent = nullptr;
  if (auto *AT = dyn_cast<AssociatedTypeDecl>(GP)) {
    parent = AT->getProtocol();
  } else {
    auto *dc = GP->getDeclContext();
    parent = dc->getInnermostDeclarationDeclContext();
  }

  if (!parent)
    return "";

  llvm::SmallString<64> result;
  llvm::raw_svector_ostream OS(result);

  OS << Decl::getDescriptiveKindName(GP->getDescriptiveKind());

  if (includeName && GP->hasName())
    OS << " '" << GP->getBaseName() << "'";

  OS << " of ";
  OS << Decl::getDescriptiveKindName(parent->getDescriptiveKind());
  if (auto *decl = dyn_cast<ValueDecl>(parent)) {
    if (decl->hasName())
      OS << " '" << decl->getName() << "'";
  }

  return OS.str().str();
}

/// Special handling of conflicts associated with generic arguments.
///
/// func foo<T>(_: T, _: T) {}
/// func bar(x: Int, y: Float) {
///   foo(x, y)
/// }
///
/// It's done by first retrieving all generic parameters from each solution,
/// filtering bindings into a distinct set and diagnosing any differences.
static bool diagnoseConflictingGenericArguments(ConstraintSystem &cs,
                                                const SolutionDiff &diff,
                                                ArrayRef<Solution> solutions) {
  if (!diff.overloads.empty())
    return false;

  bool noFixes = llvm::all_of(solutions, [](const Solution &solution) -> bool {
     const auto score = solution.getFixedScore();
     return score.Data[SK_Fix] == 0 && solution.Fixes.empty();
  });

  bool allMismatches =
      llvm::all_of(solutions, [](const Solution &solution) -> bool {
        return llvm::all_of(
            solution.Fixes, [](const ConstraintFix *fix) -> bool {
              return fix->getKind() == FixKind::AllowArgumentTypeMismatch ||
                     fix->getKind() == FixKind::AllowFunctionTypeMismatch ||
                     fix->getKind() == FixKind::AllowTupleTypeMismatch ||
                     fix->getKind() == FixKind::GenericArgumentsMismatch ||
                     fix->getKind() == FixKind::InsertCall ||
                     fix->getKind() == FixKind::IgnoreCollectionElementContextualMismatch;
            });
      });

  if (!noFixes && !allMismatches)
    return false;

  auto &DE = cs.getASTContext().Diags;

  llvm::SmallDenseMap<TypeVariableType *,
                      std::pair<GenericTypeParamType *, SourceLoc>, 4>
      genericParams;
  // Consider all representative type variables across all solutions.
  for (auto &solution : solutions) {
    for (auto &typeBinding : solution.typeBindings) {
      auto *typeVar = typeBinding.first;
      if (auto *GP = typeVar->getImpl().getGenericParameter()) {
        auto *locator = typeVar->getImpl().getLocator();
        auto *repr = cs.getRepresentative(typeVar);
        // If representative is another generic parameter let's
        // use its generic parameter type instead of originator's,
        // but it's possible that generic parameter is equated to
        // some other type e.g.
        //
        // func foo<T>(_: T) -> T {}
        //
        // In this case when reference to function `foo` is "opened"
        // type variable representing `T` would be equated to
        // type variable representing a result type of the reference.
        if (auto *reprGP = repr->getImpl().getGenericParameter())
          GP = reprGP;

        genericParams[repr] = {GP, getLoc(locator->getAnchor())};
      }
    }
  }

  llvm::SmallDenseMap<std::pair<GenericTypeParamType *, SourceLoc>,
                      SmallVector<Type, 4>>
      conflicts;

  for (const auto &entry : genericParams) {
    auto *typeVar = entry.first;
    auto GP = entry.second;

    swift::SmallSetVector<Type, 4> arguments;
    for (const auto &solution : solutions) {
      auto type = solution.typeBindings.lookup(typeVar);
      // Type variables gathered from a solution's type binding context may not
      // exist in another given solution because some solutions may have
      // additional type variables not present in other solutions due to taking
      // different paths in the solver.
      if (!type)
        continue;

      // Contextual opaque result type is uniquely identified by
      // declaration it's associated with, so we have to compare
      // declarations instead of using pointer equality on such types.
      if (auto *opaque = type->getAs<OpaqueTypeArchetypeType>()) {
        auto *decl = opaque->getDecl();
        arguments.remove_if([&](Type argType) -> bool {
          if (auto *otherOpaque = argType->getAs<OpaqueTypeArchetypeType>()) {
            return decl == otherOpaque->getDecl();
          }
          return false;
        });
      }

      arguments.insert(type);
    }

    if (arguments.size() > 1)
      conflicts[GP].append(arguments.begin(), arguments.end());
  }

  auto getGenericTypeDecl = [&](ArchetypeType *archetype) -> ValueDecl * {
    auto type = archetype->getInterfaceType();

    if (auto *GTPT = type->getAs<GenericTypeParamType>())
      return GTPT->getDecl();

    if (auto *DMT = type->getAs<DependentMemberType>())
      return DMT->getAssocType();

    return nullptr;
  };

  bool diagnosed = false;
  for (auto &conflict : conflicts) {
    SourceLoc loc;
    GenericTypeParamType *GP;

    std::tie(GP, loc) = conflict.first;
    auto conflictingArguments = conflict.second;

    // If there are any substitutions that are not fully resolved
    // solutions cannot be considered conflicting for the given parameter.
    if (llvm::any_of(conflictingArguments,
                     [](const auto &arg) { return arg->hasPlaceholder(); }))
      continue;

    llvm::SmallString<64> arguments;
    llvm::raw_svector_ostream OS(arguments);

    interleave(
        conflictingArguments,
        [&](Type argType) {
          OS << "'" << argType << "'";

          if (auto *opaque = argType->getAs<OpaqueTypeArchetypeType>()) {
            auto *decl = opaque->getDecl()->getNamingDecl();
            OS << " (result type of '" << decl->getBaseName().userFacingName()
               << "')";
            return;
          }

          if (auto archetype = argType->getAs<ArchetypeType>()) {
            if (auto *GTD = getGenericTypeDecl(archetype))
              OS << " (" << describeGenericType(GTD) << ")";
          }
        },
        [&OS] { OS << " vs. "; });

    DE.diagnose(loc, diag::conflicting_arguments_for_generic_parameter, GP,
                OS.str());
    diagnosed = true;
  }

  return diagnosed;
}

/// Diagnose ambiguity related to overloaded declarations where only
/// *some* of the overload choices have ephemeral pointer warnings/errors
/// associated with them. Such situations have be handled specifically
/// because ephemeral fixes do not affect the score.
///
/// If all of the overloads have ephemeral fixes associated with them
/// it's much easier to diagnose through notes associated with each fix.
static bool
diagnoseAmbiguityWithEphemeralPointers(ConstraintSystem &cs,
                                       ArrayRef<Solution> solutions) {
  unsigned numSolutionsWithFixes = 0;
  for (const auto &solution : solutions) {
    if (solution.Fixes.empty()) {
      continue;
    }

    if (!llvm::all_of(solution.Fixes, [](const ConstraintFix *fix) {
          return fix->getKind() == FixKind::TreatEphemeralAsNonEphemeral;
        }))
      return false;

    numSolutionsWithFixes += 1;
  }

  // If all or no solutions have fixes for ephemeral pointers, let's
  // let `diagnoseAmbiguityWithFixes` diagnose the problem.
  if (numSolutionsWithFixes == 0 ||
      numSolutionsWithFixes == solutions.size())
    return false;

  // If only some of the solutions have ephemeral pointer fixes
  // let's let `diagnoseAmbiguity` diagnose the problem either
  // with affected argument or related declaration e.g. function ref.
  return cs.diagnoseAmbiguity(solutions);
}

static bool diagnoseAmbiguityWithContextualType(
    ConstraintSystem &cs, SolutionDiff &solutionDiff,
    ArrayRef<std::pair<const Solution *, const ConstraintFix *>> aggregateFix,
    ArrayRef<Solution> solutions) {
  // Diagnose only if contextual failure is associated with every solution.
  if (aggregateFix.size() < solutions.size())
    return false;

  auto getResultType =
      [](const std::pair<const Solution *, const ConstraintFix *> &entry)
      -> Type {
    auto &solution = *entry.first;
    auto anchor = entry.second->getLocator()->getAnchor();
    return solution.simplifyType(solution.getType(anchor));
  };

  auto resultType = getResultType(aggregateFix.front());
  // If right-hand side of the conversion (result of the AST node)
  // is the same across all of the solutions let's diagnose it as if
  // it it as a single failure.
  if (llvm::all_of(
          aggregateFix,
          [&](const std::pair<const Solution *, const ConstraintFix *> &entry) {
            return resultType->isEqual(getResultType(entry));
          })) {
    auto &fix = aggregateFix.front();
    return fix.second->diagnose(*fix.first, /*asNote=*/false);
  }

  // If result types are different it could only mean that this is an attempt
  // to convert a reference to, or call of overloaded declaration to a
  // particular type.

  auto &solution = *aggregateFix.front().first;
  auto *locator = aggregateFix.front().second->getLocator();
  auto *calleeLocator = solution.getCalleeLocator(locator);

  auto result =
      llvm::find_if(solutionDiff.overloads,
                    [&calleeLocator](const SolutionDiff::OverloadDiff &entry) {
                      return entry.locator == calleeLocator;
                    });

  if (result == solutionDiff.overloads.end())
    return false;

  auto &DE = cs.getASTContext().Diags;

  auto anchor = locator->getAnchor();
  auto name = result->choices.front().getName();
  auto contextualTy = solution.getContextualType(anchor);

  // In some situations `getContextualType` for a contextual type
  // locator is going to return then empty type. This happens because
  // e.g. optional-some patterns and patterns with incorrect type don't
  // have a contextual type for initialization expression but use
  // a conversion with contextual locator nevertheless to indicate
  // the purpose. This doesn't affect non-ambiguity diagnostics
  // because mismatches carry both `from` and `to` types.
  if (!contextualTy)
    return false;

  DE.diagnose(getLoc(anchor),
              contextualTy->is<ProtocolType>()
                  ? diag::no_overloads_have_result_type_conformance
                  : diag::no_candidates_match_result_type,
              name.getBaseName().userFacingName(), contextualTy);

  for (const auto &solution : solutions) {
    auto overload = solution.getOverloadChoice(calleeLocator);
    if (auto *decl = overload.choice.getDeclOrNull()) {
      auto type = solution.simplifyType(overload.boundType);

      if (isExpr<ApplyExpr>(anchor) || isExpr<SubscriptExpr>(anchor)) {
        auto fnType = type->castTo<FunctionType>();
        DE.diagnose(
            decl,
            contextualTy->is<ProtocolType>()
                ? diag::overload_result_type_does_not_conform
                : diag::cannot_convert_candidate_result_to_contextual_type,
            decl, fnType->getResult(), contextualTy);
      } else {
        DE.diagnose(decl, diag::found_candidate_type, type);
      }
    }
  }

  return true;
}

/// Diagnose problems with generic requirement fixes that are anchored on
/// one callee location. The list could contain different kinds of fixes
/// i.e. missing protocol conformances at different positions,
/// same-type requirement mismatches, etc.
static bool diagnoseAmbiguityWithGenericRequirements(
    ConstraintSystem &cs,
    ArrayRef<std::pair<const Solution *, const ConstraintFix *>> aggregate) {
  // If all of the fixes point to the same overload choice,
  // we can diagnose this an a single error.
  bool hasNonDeclOverloads = false;

  llvm::SmallSet<ValueDecl *, 4> overloadChoices;
  for (const auto &entry : aggregate) {
    const auto &solution = *entry.first;
    auto *calleeLocator = solution.getCalleeLocator(entry.second->getLocator());

    if (auto overload = solution.getOverloadChoiceIfAvailable(calleeLocator)) {
      if (auto *D = overload->choice.getDeclOrNull()) {
        overloadChoices.insert(D);
      } else {
        hasNonDeclOverloads = true;
      }
    }
  }

  auto &primaryFix = aggregate.front();
  {
    if (overloadChoices.size() > 0) {
      // Some of the choices are non-declaration,
      // let's delegate that to ambiguity diagnostics.
      if (hasNonDeclOverloads)
        return false;

      if (overloadChoices.size() == 1)
        return primaryFix.second->diagnose(*primaryFix.first);

      // fall through to the tailored ambiguity diagnostic.
    } else {
      // If there are no overload choices it means that
      // the issue is with types, delegate that to the primary fix.
      return primaryFix.second->diagnoseForAmbiguity(aggregate);
    }
  }

  // Produce "no exact matches" diagnostic.
  auto &ctx = cs.getASTContext();
  auto *choice = *overloadChoices.begin();

  ctx.Diags.diagnose(getLoc(primaryFix.second->getLocator()->getAnchor()),
                     diag::no_overloads_match_exactly_in_call,
                     /*isApplication=*/false, choice,
                     choice->getName().isSpecial());

  for (const auto &entry : aggregate) {
    entry.second->diagnose(*entry.first, /*asNote=*/true);
  }

  return true;
}

static bool diagnoseAmbiguity(
    ConstraintSystem &cs, const SolutionDiff::OverloadDiff &ambiguity,
    ArrayRef<std::pair<const Solution *, const ConstraintFix *>> aggregateFix,
    ArrayRef<Solution> solutions) {
  auto *locator = aggregateFix.front().second->getLocator();
  auto anchor = aggregateFix.front().second->getAnchor();

  auto &DE = cs.getASTContext().Diags;

  llvm::SmallPtrSet<ValueDecl *, 4> localAmbiguity;
  {
    for (auto &entry : aggregateFix) {
      const auto &solution = entry.first;
      const auto &overload = solution->getOverloadChoice(ambiguity.locator);
      auto *choice = overload.choice.getDeclOrNull();

      // It's not possible to diagnose different kinds of overload choices.
      if (!choice)
        return false;

      localAmbiguity.insert(choice);
    }
  }

  if (localAmbiguity.empty())
    return false;

  // If all of the fixes are rooted in the same choice.
  if (localAmbiguity.size() == 1) {
    auto &primaryFix = aggregateFix.front();
    return primaryFix.second->diagnose(*primaryFix.first);
  }

  {
    auto fixKind = aggregateFix.front().second->getKind();
    if (llvm::all_of(
            aggregateFix, [&](const std::pair<const Solution *,
                                              const ConstraintFix *> &entry) {
              auto &fix = entry.second;
              return fix->getKind() == fixKind && fix->getLocator() == locator;
            })) {
      auto *primaryFix = aggregateFix.front().second;
      if (primaryFix->diagnoseForAmbiguity(aggregateFix))
        return true;
    }
  }

  auto *decl = *localAmbiguity.begin();
  auto *commonCalleeLocator = ambiguity.locator;

  bool diagnosed = true;
  {
    DiagnosticTransaction transaction(DE);

    auto commonAnchor = commonCalleeLocator->getAnchor();
    if (auto *callExpr = getAsExpr<CallExpr>(commonAnchor))
      commonAnchor = callExpr->getDirectCallee();

    const auto name = decl->getName();

    // Emit an error message for the ambiguity.
    if (locator->isForContextualType()) {
      auto baseName = name.getBaseName();
      DE.diagnose(getLoc(commonAnchor), diag::no_candidates_match_result_type,
                  baseName.userFacingName(),
                  cs.getContextualType(anchor, /*forConstraint=*/false));
    } else if (name.isOperator()) {
      auto *anchor = castToExpr(commonCalleeLocator->getAnchor());

      // If operator is "applied" e.g. `1 + 2` there are tailored
      // diagnostics in case of ambiguity, but if it's referenced
      // e.g. `arr.sort(by: <)` it's better to produce generic error
      // and a note per candidate.
      if (auto *parentExpr = cs.getParentExpr(anchor)) {
        if (auto *apply = dyn_cast<ApplyExpr>(parentExpr)) {
          if (apply->getFn() == anchor) {
            diagnoseOperatorAmbiguity(cs, name.getBaseIdentifier(), solutions,
                                      commonCalleeLocator);
            return true;
          }
        }
      }

      DE.diagnose(anchor->getLoc(), diag::no_overloads_match_exactly_in_call,
                  /*isApplication=*/false, decl, name.isSpecial());
    } else {
      bool isApplication = llvm::any_of(solutions, [&](const auto &S) {
          return llvm::any_of(S.argumentLists, [&](const auto &pair) {
            return pair.first->getAnchor() == commonAnchor;
          });
      });

      DE.diagnose(getLoc(commonAnchor),
                  diag::no_overloads_match_exactly_in_call, isApplication,
                  decl, name.isSpecial());
    }

    // Produce candidate notes
    SmallPtrSet<ValueDecl *, 4> distinctChoices;
    llvm::SmallSet<CanType, 4> candidateTypes;
    for (const auto &solution : solutions) {
      auto overload = solution.getOverloadChoice(commonCalleeLocator);
      auto *decl = overload.choice.getDecl();
      auto type = solution.simplifyType(overload.adjustedOpenedType);
      // Skip if we've already produced a note for this overload
      if (!distinctChoices.insert(decl).second)
        continue;

      auto noteLoc =
          decl->getLoc().isInvalid() ? getLoc(commonAnchor) : decl->getLoc();

      SmallVector<const ConstraintFix *, 4> fixes;
      for (const auto &entry : aggregateFix) {
        if (entry.first == &solution)
          fixes.push_back(entry.second);
      }

      auto emitGeneralFoundCandidateNote = [&]() {
        // Emit a general "found candidate" note
        if (decl->getLoc().isInvalid()) {
          if (candidateTypes.insert(type->getCanonicalType()).second)
            DE.diagnose(getLoc(commonAnchor), diag::found_candidate_type, type);
        } else {
          DE.diagnose(noteLoc, diag::found_candidate);
        }
      };

      if (fixes.size() == 1) {
        diagnosed &= fixes.front()->diagnose(solution, /*asNote*/ true);
      } else if (!fixes.empty() &&
                 llvm::all_of(fixes, [&](const ConstraintFix *fix) {
                   // Ignore coercion fixes in this context, to
                   // focus on the argument mismatches.
                   if (fix->getLocator()->isForCoercion())
                     return true;

                   return fix->getLocator()
                       ->findLast<LocatorPathElt::ApplyArgument>()
                       .has_value();
                 })) {
        // All fixes have to do with arguments, so let's show the parameter
        // lists.
        //
        // It's possible that function type is wrapped in an optional
        // if it's from `@objc optional` method, so we need to ignore that.
        auto *fn =
            type->lookThroughAllOptionalTypes()->getAs<AnyFunctionType>();
        assert(fn);

        auto first = llvm::find_if(fixes, [&](const ConstraintFix *fix) {
          return fix->getLocator()
              ->findLast<LocatorPathElt::ApplyArgument>()
              .has_value();
        });

        if (first != fixes.end()) {
          auto *argList = solution.getArgumentList((*first)->getLocator());
          assert(argList);

          if (fn->getNumParams() == 1 && argList->isUnary()) {
            const auto &param = fn->getParams()[0];
            auto argTy = solution.getResolvedType(argList->getUnaryExpr());

            DE.diagnose(noteLoc,
                        diag::candidate_has_invalid_argument_at_position,
                        solution.simplifyType(param.getPlainType()),
                        /*position=*/1, param.isInOut(), argTy);
          } else {
            DE.diagnose(noteLoc, diag::candidate_partial_match,
                        fn->getParamListAsString(fn->getParams()));
          }
        } else {
          // Only coercion ambiguity fixes.
          emitGeneralFoundCandidateNote();
        }
      } else {
        emitGeneralFoundCandidateNote();
      }
    }

    // If not all of the fixes produced a note, we can't diagnose this.
    if (!diagnosed)
      transaction.abort();
  }

  return diagnosed;
}

using FixInContext = std::pair<const Solution *, const ConstraintFix *>;

// Attempts to diagnose function call ambiguities of types inferred for a result
// generic parameter from contextual type and a closure argument that
// conflicting infer a different type for the same argument. Example:
//   func callit<T>(_ f: () -> T) -> T {
//     f()
//   }
//
//   func context() -> Int {
//     callit {
//       print("hello")
//     }
//   }
// Where generic argument `T` can be inferred both as `Int` from contextual
// result and `Void` from the closure argument result.
static bool diagnoseContextualFunctionCallGenericAmbiguity(
    ConstraintSystem &cs, ArrayRef<FixInContext> contextualFixes,
    ArrayRef<FixInContext> allFixes) {

  if (contextualFixes.empty())
    return false;

  auto contextualFix = contextualFixes.front();
  if (!std::all_of(contextualFixes.begin() + 1, contextualFixes.end(),
                   [&contextualFix](FixInContext fix) {
                     return fix.second->getLocator() ==
                            contextualFix.second->getLocator();
                   }))
    return false;

  auto fixLocator = contextualFix.second->getLocator();
  auto contextualAnchor = fixLocator->getAnchor();
  auto *AE = getAsExpr<ApplyExpr>(contextualAnchor);
  // All contextual failures anchored on the same function call.
  if (!AE)
    return false;

  auto fnLocator = cs.getConstraintLocator(AE->getSemanticFn());
  auto overload = contextualFix.first->getOverloadChoiceIfAvailable(fnLocator);
  if (!overload)
    return false;

  auto applyFnType = overload->adjustedOpenedType->castTo<FunctionType>();
  auto resultTypeVar = applyFnType->getResult()->getAs<TypeVariableType>();
  if (!resultTypeVar)
    return false;

  auto *GP = resultTypeVar->getImpl().getGenericParameter();
  if (!GP)
    return false;

  auto applyLoc =
      cs.getConstraintLocator(AE, {LocatorPathElt::ApplyArgument()});
  auto argMatching =
      contextualFix.first->argumentMatchingChoices.find(applyLoc);
  if (argMatching == contextualFix.first->argumentMatchingChoices.end()) {
    return false;
  }

  auto *args = AE->getArgs();
  llvm::SmallVector<ClosureExpr *, 2> closureArguments;
  for (auto i : indices(*args)) {
    auto *closure = getAsExpr<ClosureExpr>(args->getExpr(i));
    if (!closure)
      continue;

    auto argParamMatch = argMatching->second.parameterBindings[i];
    auto param = applyFnType->getParams()[argParamMatch.front()];
    auto paramFnType = param.getPlainType()->getAs<FunctionType>();
    if (!paramFnType)
      continue;

    if (cs.typeVarOccursInType(resultTypeVar, paramFnType->getResult()))
      closureArguments.push_back(closure);
  }

  // If no closure result's involves the generic parameter, just bail because we
  // won't find a conflict.
  if (closureArguments.empty())
    return false;

  // At least one closure where result type involves the generic parameter.
  // So let's try to collect the set of fixed types for the generic parameter
  // from all the closure contextual fix/solutions and if there are more than
  // one fixed type diagnose it.
  swift::SmallSetVector<Type, 4> genericParamInferredTypes;
  for (auto &fix : contextualFixes)
    genericParamInferredTypes.insert(fix.first->getFixedType(resultTypeVar));

  if (llvm::all_of(allFixes, [&](FixInContext fix) {
        auto fixLocator = fix.second->getLocator();
        if (fixLocator->isForContextualType())
          return true;

        if (!(fix.second->getKind() == FixKind::IgnoreContextualType ||
              fix.second->getKind() == FixKind::AllowTupleTypeMismatch))
          return false;

        auto anchor = fixLocator->getAnchor();
        if (!(anchor == contextualAnchor ||
              fixLocator->isLastElement<LocatorPathElt::ClosureResult>() ||
              fixLocator->isLastElement<LocatorPathElt::ClosureBody>()))
          return false;

        genericParamInferredTypes.insert(
            fix.first->getFixedType(resultTypeVar));
        return true;
      })) {

    if (genericParamInferredTypes.size() != 2)
      return false;

    auto &DE = cs.getASTContext().Diags;
    llvm::SmallString<64> arguments;
    llvm::raw_svector_ostream OS(arguments);
    interleave(
        genericParamInferredTypes,
        [&](Type argType) { OS << "'" << argType << "'"; },
        [&OS] { OS << " vs. "; });

    DE.diagnose(AE->getLoc(), diag::conflicting_arguments_for_generic_parameter,
                GP, OS.str());

    DE.diagnose(AE->getLoc(),
                diag::generic_parameter_inferred_from_result_context, GP,
                genericParamInferredTypes.back());
    DE.diagnose(closureArguments.front()->getStartLoc(),
                diag::generic_parameter_inferred_from_closure, GP,
                genericParamInferredTypes.front());

    return true;
  }
  return false;
}

bool ConstraintSystem::diagnoseAmbiguityWithFixes(
    SmallVectorImpl<Solution> &solutions) {
  if (solutions.empty() || shouldSuppressDiagnostics())
    return false;

  SolutionDiff solutionDiff(solutions);

  if (diagnoseConflictingGenericArguments(*this, solutionDiff, solutions))
    return true;

  if (auto bestScore = solverState->BestScore) {
    solutions.erase(llvm::remove_if(solutions,
                                    [&](const Solution &solution) {
                                      return solution.getFixedScore() >
                                             *bestScore;
                                    }),
                    solutions.end());

    if (llvm::all_of(solutions, [&](const Solution &solution) {
          auto score = solution.getFixedScore();
          return score.Data[SK_Fix] == 0 && solution.Fixes.empty();
        }))
      return false;
  }

  if (solutions.size() < 2)
    return false;

  if (diagnoseAmbiguityWithEphemeralPointers(*this, solutions))
    return true;

  if (isDebugMode()) {
    auto indent = solverState->getCurrentIndent();
    auto &log = llvm::errs().indent(indent);
    log << "--- Ambiguity: Considering #" << solutions.size()
        << " solutions with fixes ---\n";
    int i = 0;
    for (auto &solution : solutions) {
      log << "\n";
      log.indent(indent) << "--- Solution #" << i++ << "---\n";
      solution.dump(log, indent);
      log << "\n";
    }
  }

  // If there either no fixes at all or all of the are warnings,
  // let's diagnose this as regular ambiguity.
  if (llvm::all_of(solutions, [](const Solution &solution) {
        return llvm::all_of(solution.Fixes, [](const ConstraintFix *fix) {
          return !fix->isFatal();
        });
      })) {
    return diagnoseAmbiguity(solutions);
  }

  // Algorithm is as follows:
  //
  // a. Aggregate all of the available fixes based on callee locator;
  // b. For each ambiguous overload match aggregated fixes and diagnose;
  // c. Discard all of the fixes which have been already considered
  //    as part of overload diagnostics;
  // d. Diagnose remaining (uniqued based on kind + locator) fixes
  //    iff they appear in all of the solutions.

  llvm::SmallSetVector<FixInContext, 4> fixes;
  for (auto &solution : solutions) {
    for (auto *fix : solution.Fixes) {
      // If the fix doesn't affect the solution score, it is not the
      // source of ambiguity or failures.
      // Ignore warnings in favor of actual error fixes,
      // because they are not the source of ambiguity/failures.
      if (!fix->impact())
        continue;

      fixes.insert({&solution, fix});
    }
  }

  llvm::MapVector<ConstraintLocator *, SmallVector<FixInContext, 4>>
      fixesByCallee;
  llvm::SmallVector<FixInContext, 4> contextualFixes;

  for (const auto &entry : fixes) {
    const auto &solution = *entry.first;
    const auto *fix = entry.second;

    auto *locator = fix->getLocator();

    if (locator->isForContextualType()) {
      contextualFixes.push_back({&solution, fix});
      continue;
    }

    auto *calleeLocator = solution.getCalleeLocator(locator);
    fixesByCallee[calleeLocator].push_back({&solution, fix});
  }

  bool diagnosed = false;

  // All of the fixes which have been considered already.
  llvm::SmallSetVector<FixInContext, 4> consideredFixes;

  for (const auto &ambiguity : solutionDiff.overloads) {
    auto fixes = fixesByCallee.find(ambiguity.locator);
    if (fixes == fixesByCallee.end())
      continue;

    auto aggregate = fixes->second;
    diagnosed |= ::diagnoseAmbiguity(*this, ambiguity, aggregate, solutions);

    consideredFixes.insert(aggregate.begin(), aggregate.end());
  }

  if (diagnoseAmbiguityWithContextualType(*this, solutionDiff, contextualFixes,
                                          solutions)) {
    consideredFixes.insert(contextualFixes.begin(), contextualFixes.end());
    diagnosed |= true;
  }

  // Remove all of the fixes which have been attached to ambiguous
  // overload choices.
  fixes.set_subtract(consideredFixes);

  // Aggregate all requirement fixes that belong to the same callee
  // and attempt to diagnose possible ambiguities.
  {
    // Aggregates fixes fixes attached to `buildExpression` and `buildBlock`
    // methods at the particular source location.
    llvm::MapVector<SourceLoc, SmallVector<FixInContext, 4>>
        builderMethodRequirementFixes;

    llvm::MapVector<ConstraintLocator *, SmallVector<FixInContext, 4>>
        perCalleeRequirementFixes;

    for (const auto &entry : fixes) {
      auto *fix = entry.second;
      if (!fix->getLocator()->isLastElement<LocatorPathElt::AnyRequirement>())
        continue;

      auto *calleeLoc = entry.first->getCalleeLocator(fix->getLocator());

      auto *UDE = getAsExpr<UnresolvedDotExpr>(calleeLoc->getAnchor());
      if (UDE && isResultBuilderMethodReference(getASTContext(), UDE)) {
        auto *anchor = castToExpr<Expr>(calleeLoc->getAnchor());
        builderMethodRequirementFixes[anchor->getLoc()].push_back(entry);
      } else {
        perCalleeRequirementFixes[calleeLoc].push_back(entry);
      }
    }

    SmallVector<SmallVector<FixInContext, 4>, 4> viableGroups;
    {
      auto takeAggregateIfViable =
          [&](SmallVector<FixInContext, 4> &aggregate) {
            // Ambiguity only if all of the solutions have a requirement
            // fix at the given location.
            if (aggregate.size() == solutions.size())
              viableGroups.push_back(std::move(aggregate));
          };

      for (auto &entry : builderMethodRequirementFixes)
        takeAggregateIfViable(entry.second);

      for (auto &entry : perCalleeRequirementFixes)
        takeAggregateIfViable(entry.second);
    }

    for (auto &aggregate : viableGroups) {
      if (diagnoseAmbiguityWithGenericRequirements(*this, aggregate)) {
        // Remove diagnosed fixes.
        fixes.set_subtract(aggregate);
        diagnosed = true;
      }
    }
  }

  llvm::MapVector<std::pair<FixKind, ConstraintLocator *>,
                  SmallVector<FixInContext, 4>>
      fixesByKind;

  for (const auto &entry : fixes) {
    const auto *fix = entry.second;
    fixesByKind[{fix->getKind(), fix->getLocator()}].push_back(entry);
  }

  // If leftover fix is contained in all of the solutions let's
  // diagnose it as ambiguity.
  for (const auto &entry : fixesByKind) {
    if (llvm::all_of(solutions, [&](const Solution &solution) -> bool {
          return llvm::any_of(
              solution.Fixes, [&](const ConstraintFix *fix) -> bool {
                return std::make_pair(fix->getKind(), fix->getLocator()) ==
                       entry.first;
              });
        })) {
      auto &aggregate = entry.second;
      diagnosed |= aggregate.front().second->diagnoseForAmbiguity(aggregate);
    }
  }

  if (!diagnosed && diagnoseContextualFunctionCallGenericAmbiguity(
                        *this, contextualFixes, fixes.getArrayRef()))
    return true;

  return diagnosed;
}

/// Determine the number of distinct overload choices in the
/// provided set.
static unsigned countDistinctOverloads(ArrayRef<OverloadChoice> choices) {
  llvm::SmallPtrSet<void *, 4> uniqueChoices;
  for (auto choice : choices) {
    uniqueChoices.insert(choice.getOpaqueChoiceSimple());
  }
  return uniqueChoices.size();
}

static Type getOverloadChoiceType(ConstraintLocator *overloadLoc,
                                  const Solution &solution) {
  auto selectedOverload = solution.overloadChoices.find(overloadLoc);
  if (selectedOverload == solution.overloadChoices.end())
    return Type();
  return solution.simplifyType(selectedOverload->second.adjustedOpenedType);
}

/// Determine the name of the overload in a set of overload choices.
static DeclName getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  DeclName name;
  for (auto choice : choices) {
    if (!choice.isDecl())
      continue;

    const DeclName nextName = choice.getDecl()->getName();
    if (!name) {
      name = nextName;
      continue;
    }

    if (name != nextName) {
      // Assume all choices have the same base name and only differ in
      // argument labels. This may not be a great assumption, but we don't
      // really have a way to recover for diagnostics otherwise.
      return name.getBaseName();
    }
  }

  return name;
}

/// Extend the given index map with all of the subexpressions in the given
/// expression.
static void extendPreorderIndexMap(
    Expr *expr, llvm::DenseMap<Expr *, unsigned> &indexMap) {
  class RecordingTraversal : public ASTWalker {
  public:
    llvm::DenseMap<Expr *, unsigned> &IndexMap;
    unsigned Index = 0;

    explicit RecordingTraversal(llvm::DenseMap<Expr *, unsigned> &indexMap)
      : IndexMap(indexMap) { }

    MacroWalking getMacroWalkingBehavior() const override {
      return MacroWalking::ArgumentsAndExpansion;
    }

    PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
      IndexMap[E] = Index;
      ++Index;
      return Action::Continue(E);
    }
  };

  RecordingTraversal traversal(indexMap);
  expr->walk(traversal);
}

bool ConstraintSystem::diagnoseAmbiguity(ArrayRef<Solution> solutions) {
  // Produce a diff of the solutions.
  SolutionDiff diff(solutions);

  // Find the locators which have the largest numbers of distinct overloads.
  std::optional<unsigned> bestOverload;
  // Overloads are scored by lexicographical comparison of (# of distinct
  // overloads, depth, *reverse* of the index). N.B. - cannot be used for the
  // reversing: the score version of index == 0 should be > than that of 1, but
  // -0 == 0 < UINT_MAX == -1, whereas ~0 == UINT_MAX > UINT_MAX - 1 == ~1.
  auto score = [](unsigned depth, unsigned index, unsigned distinctOverloads) {
    return std::make_tuple(depth, ~index, distinctOverloads);
  };
  auto bestScore = score(0, std::numeric_limits<unsigned>::max(), 0);

  // Get a map of expressions to their depths and post-order traversal indices.
  // Heuristically, all other things being equal, we should complain about the
  // ambiguous expression that (1) is deepest, (2) comes earliest in the
  // expression, or (3) has the most overloads.
  llvm::DenseMap<Expr *, unsigned> indexMap;
  for (auto expr : InputExprs) {
    extendPreorderIndexMap(expr, indexMap);
  }

  for (unsigned i = 0, n = diff.overloads.size(); i != n; ++i) {
    auto &overload = diff.overloads[i];
    auto *locator = overload.locator;

    // If there is only one overload difference, it's the best.
    if (n == 1) {
      bestOverload = i;
      break;
    }

    // If there are multiple overload sets involved, let's pick the
    // one that has choices with different types, because that is
    // most likely the source of ambiguity.
    {
      auto overloadTy = getOverloadChoiceType(locator, solutions.front());
      if (std::all_of(solutions.begin() + 1, solutions.end(),
                      [&](const Solution &solution) {
                        return overloadTy->isEqual(
                          getOverloadChoiceType(locator, solution));
                      }))
        continue;
    }

    ASTNode anchor;

    // Simplification of member locator would produce a base expression,
    // this is what we want for diagnostics but not for comparisons here
    // because base expression is located at a different depth which would
    // lead to incorrect results if both reference and base expression are
    // ambiguous e.g. `test[x].count` if both `[x]` and `count` are ambiguous
    // than simplification of `count` would produce `[x]` which is incorrect.
    if (locator->isLastElement<LocatorPathElt::Member>() ||
        locator->isLastElement<LocatorPathElt::ConstructorMember>()) {
      anchor = locator->getAnchor();
    } else {
      anchor = simplifyLocatorToAnchor(overload.locator);
    }

    // If we can't resolve the locator to an anchor with no path,
    // we can't diagnose this well.
    if (!anchor)
      continue;

    // Index and Depth is only applicable to expressions.
    unsigned index = 0;
    unsigned depth = 0;

    if (auto *expr = getAsExpr(anchor)) {
      auto it = indexMap.find(expr);
      if (it == indexMap.end())
        continue;

      index = it->second;

      auto optDepth = getExprDepth(expr);
      if (!optDepth)
        continue;

      depth = *optDepth;
    }

    // If we don't have a name to hang on to, it'll be hard to diagnose this
    // overload.
    if (!getOverloadChoiceName(overload.choices))
      continue;

    unsigned distinctOverloads = countDistinctOverloads(overload.choices);

    // We need at least two overloads to make this interesting.
    if (distinctOverloads < 2)
      continue;

    // If we have more distinct overload choices for this locator than for
    // prior locators, just keep this locator.
    auto thisScore = score(depth, index, distinctOverloads);
    if (thisScore > bestScore) {
      bestScore = thisScore;
      bestOverload = i;
      continue;
    }

    // We have better results. Ignore this one.
  }

  // FIXME: Should be able to pick the best locator, e.g., based on some
  // depth-first numbering of expressions.
  if (bestOverload) {
    auto &overload = diff.overloads[*bestOverload];
    // FIXME: We would prefer to emit the name as written, but that information
    // is not sufficiently centralized in the AST.
    DeclNameRef name(getOverloadChoiceName(overload.choices));
    auto anchor = simplifyLocatorToAnchor(overload.locator);
    if (!anchor) {
      // It's not clear that this is actually valid. Just use the overload's
      // anchor for release builds, but assert so we can properly diagnose
      // this case if it happens to be hit. Note that the overload will
      // *always* be anchored, otherwise everything would be broken, ie. this
      // assertion would be the least of our worries.
      anchor = overload.locator->getAnchor();
      assert(false && "locator could not be simplified to anchor");
    }

    // Emit the ambiguity diagnostic.
    auto &DE = getASTContext().Diags;
    DE.diagnose(getLoc(anchor),
                name.isOperator() ? diag::ambiguous_operator_ref
                                  : diag::ambiguous_decl_ref,
                name);

    TrailingClosureAmbiguityFailure failure(solutions, anchor,
                                            overload.choices);
    if (failure.diagnoseAsNote())
      return true;

    // Emit candidates.  Use a SmallPtrSet to make sure only emit a particular
    // candidate once.  FIXME: Why is one candidate getting into the overload
    // set multiple times? (See also tryDiagnoseTrailingClosureAmbiguity.)
    SmallPtrSet<Decl *, 8> EmittedDecls;
    for (auto choice : overload.choices) {
      switch (choice.getKind()) {
      case OverloadChoiceKind::Decl:
      case OverloadChoiceKind::DeclViaDynamic:
      case OverloadChoiceKind::DeclViaBridge:
      case OverloadChoiceKind::DeclViaUnwrappedOptional: {
        // FIXME: show deduced types, etc, etc.
        auto decl = choice.getDecl();
        if (EmittedDecls.insert(decl).second) {
          auto declModule = decl->getDeclContext()->getParentModule();
          bool printModuleName = declModule != DC->getParentModule();
          DE.diagnose(decl, diag::found_candidate_in_module,
                      printModuleName, declModule);
        }
        break;
      }
      case OverloadChoiceKind::KeyPathApplication:
      case OverloadChoiceKind::DynamicMemberLookup:
      case OverloadChoiceKind::KeyPathDynamicMemberLookup:
        // Skip key path applications and dynamic member lookups, since we don't
        // want them to noise up unrelated subscript diagnostics.
        break;

      case OverloadChoiceKind::TupleIndex:
      case OverloadChoiceKind::MaterializePack:
      case OverloadChoiceKind::ExtractFunctionIsolation:
        // FIXME: Actually diagnose something here.
        break;
      }
    }

    return true;
  }

  // FIXME: If we inferred different types for literals (for example),
  // could diagnose ambiguity that way as well.

  return false;
}

ConstraintLocator *
constraints::simplifyLocator(ConstraintSystem &cs, ConstraintLocator *locator,
                             SourceRange &range) {
  auto path = locator->getPath();
  auto anchor = locator->getAnchor();
  simplifyLocator(anchor, path, range);

  // If we didn't simplify anything, just return the input.
  if (anchor == locator->getAnchor() &&
      path.size() == locator->getPath().size()) {
    return locator;
  }

  // If the old locator didn't have any summary flags, neither will the
  // simplified version, as it must contain a subset of the path elements.
  if (locator->getSummaryFlags() == 0)
    return cs.getConstraintLocator(anchor, path, /*summaryFlags*/ 0);

  return cs.getConstraintLocator(anchor, path);
}

void constraints::simplifyLocator(ASTNode &anchor,
                                  ArrayRef<LocatorPathElt> &path,
                                  SourceRange &range) {
  range = SourceRange();

  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument: {
      auto *anchorExpr = castToExpr(anchor);
      // If the next element is an ApplyArgToParam, we can simplify by looking
      // into the index expression.
      if (path.size() < 2)
        break;

      auto elt = path[1].getAs<LocatorPathElt::ApplyArgToParam>();
      if (!elt)
        break;

      // If the 3rd element is an PackElement, add the index of pack element
      // within packs to locate the correct element.
      std::optional<unsigned> eltPackIdx;
      if (path.size() > 2) {
        if (auto eltPack = path[2].getAs<LocatorPathElt::PackElement>()) {
          eltPackIdx = eltPack->getIndex();
        }
      }

      // Extract application argument.
      if (auto *args = anchorExpr->getArgs()) {
        if (eltPackIdx.has_value()) {
          if (elt->getArgIdx() + eltPackIdx.value() < args->size()) {
            anchor = args->getExpr(elt->getArgIdx() + eltPackIdx.value());
            path = path.slice(3);
            continue;
          }
        } else if (elt->getArgIdx() < args->size()) {
          anchor = args->getExpr(elt->getArgIdx());
          path = path.slice(2);
          continue;
        }
      }
      break;
    }

    case ConstraintLocator::ApplyArgToParam:
      llvm_unreachable("Cannot appear without ApplyArgument");

    case ConstraintLocator::DynamicCallable: {
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::ApplyFunction: {
      // Extract application function.
      if (auto applyExpr = getAsExpr<ApplyExpr>(anchor)) {
        anchor = applyExpr->getFn();
        path = path.slice(1);
        continue;
      }

      // The subscript itself is the function.
      if (auto subscriptExpr = getAsExpr<SubscriptExpr>(anchor)) {
        anchor = subscriptExpr;
        path = path.slice(1);
        continue;
      }

      // If the anchor is an unapplied decl ref, there's nothing to extract.
      if (isExpr<DeclRefExpr>(anchor) || isExpr<OverloadedDeclRefExpr>(anchor)) {
        path = path.slice(1);
        continue;
      }

      break;
    }
    case ConstraintLocator::AutoclosureResult:
    case ConstraintLocator::LValueConversion:
    case ConstraintLocator::DynamicType:
    case ConstraintLocator::UnresolvedMember:
    case ConstraintLocator::ImplicitCallAsFunction:
      // Arguments in autoclosure positions, lvalue and rvalue adjustments,
      // unresolved members, and implicit callAsFunction references are
      // implicit.
      path = path.slice(1);
      continue;

   case ConstraintLocator::TupleType:
   case ConstraintLocator::GenericType:
      path = path.slice(1);
      continue;

    case ConstraintLocator::NamedTupleElement:
    case ConstraintLocator::TupleElement: {
      // Extract tuple element.
      auto elt = path[0].castTo<LocatorPathElt::AnyTupleElement>();
      unsigned index = elt.getIndex();

      if (auto *AE = getAsExpr<AssignExpr>(anchor)) {
        if (isa<TupleExpr>(AE->getSrc())) {
          anchor = AE->getSrc();
        }
      }

      if (auto tupleExpr = getAsExpr<TupleExpr>(anchor)) {
        if (index < tupleExpr->getNumElements()) {
          anchor = tupleExpr->getElement(index);
          path = path.slice(1);
          continue;
        }
      }

      if (auto *CE = getAsExpr<CollectionExpr>(anchor)) {
        if (index < CE->getNumElements()) {
          anchor = CE->getElement(index);
          path = path.slice(1);
          continue;
        }
      }

      break;
    }

    case ConstraintLocator::ConstructorMember:
      // Look through specialization first, because it doesn't play a
      // functional role here.
      if (auto *USE = getAsExpr<UnresolvedSpecializeExpr>(anchor)) {
        anchor = USE->getSubExpr();
        range = anchor.getSourceRange();
      }

      // - This is really an implicit 'init' MemberRef, so point at the base,
      //   i.e. the TypeExpr.
      // - For re-declarations we'd get an overloaded reference
      //   with multiple choices for the same type.
      if (isExpr<TypeExpr>(anchor) || isExpr<OverloadedDeclRefExpr>(anchor)) {
        range = SourceRange();
        path = path.slice(1);
        continue;
      }
      LLVM_FALLTHROUGH;

    case ConstraintLocator::Member:
      if (isExpr<UnresolvedDotExpr>(anchor)) {
        path = path.slice(1);
        continue;
      }
      if (anchor.is<Pattern *>()) {
        path = path.slice(1);
        continue;
      }
      break;
    case ConstraintLocator::MemberRefBase:
      if (auto UDE = getAsExpr<UnresolvedDotExpr>(anchor)) {
        range = UDE->getNameLoc().getSourceRange();
        anchor = UDE->getBase();
        path = path.slice(1);
        continue;
      }

      if (anchor.is<Pattern *>()) {
        path = path.slice(1);
        continue;
      }

      break;

    case ConstraintLocator::SubscriptMember:
      if (isExpr<SubscriptExpr>(anchor)) {
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ClosureBody:
    case ConstraintLocator::ClosureResult:
      if (auto CE = getAsExpr<ClosureExpr>(anchor)) {
        if (CE->hasSingleExpressionBody()) {
          anchor = CE->getSingleExpressionBody();
          path = path.slice(1);
          continue;
        }
      }
      break;

    case ConstraintLocator::ClosureThrownError:
      if (auto CE = getAsExpr<ClosureExpr>(anchor)) {
        if (auto thrownTypeRepr = CE->getExplicitThrownTypeRepr()) {
          anchor = thrownTypeRepr;
          path = path.slice(1);
          break;
        }
      }
      break;

    case ConstraintLocator::CoercionOperand: {
      auto *CE = castToExpr<CoerceExpr>(anchor);
      anchor = CE->getSubExpr()->getValueProvidingExpr();
      path = path.slice(1);
      // When in a argument function type on a coercion context
      // look past the argument, because is just for identify the
      // argument type that is being matched.
      if (!path.empty() && path[0].is<LocatorPathElt::FunctionArgument>()) {
        path = path.slice(1);
      }
      continue;
    }

    case ConstraintLocator::GlobalActorType:
    case ConstraintLocator::ContextualType: {
      // This was just for identifying purposes, strip it off.
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::KeyPathComponent: {
      auto elt = path[0].castTo<LocatorPathElt::KeyPathComponent>();

      // If the next element is an ApplyArgument, we can simplify by looking
      // into the index expression.
      if (path.size() < 3 ||
          path[1].getKind() != ConstraintLocator::ApplyArgument)
        break;

      auto applyArgElt = path[2].getAs<LocatorPathElt::ApplyArgToParam>();
      if (!applyArgElt)
        break;

      auto argIdx = applyArgElt->getArgIdx();
      if (auto *kpe = getAsExpr<KeyPathExpr>(anchor)) {
        auto component = kpe->getComponents()[elt.getIndex()];
        auto *args = component.getArgs();
        assert(args && "Trying to apply a component without args?");
        if (argIdx < args->size()) {
          anchor = args->getExpr(argIdx);
          path = path.slice(3);
          continue;
        }
      }
      break;
    }

    case ConstraintLocator::Condition: {
      if (auto *condStmt = getAsStmt<LabeledConditionalStmt>(anchor)) {
        anchor = &condStmt->getCond().front();
      } else {
        anchor = castToExpr<TernaryExpr>(anchor)->getCondExpr();
      }

      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::TernaryBranch: {
      auto branch = path[0].castTo<LocatorPathElt::TernaryBranch>();

      if (auto *ifStmt = getAsStmt<IfStmt>(anchor)) {
        anchor =
            branch.forThen() ? ifStmt->getThenStmt() : ifStmt->getElseStmt();
      } else {
        auto *ifExpr = castToExpr<TernaryExpr>(anchor);
        anchor =
            branch.forThen() ? ifExpr->getThenExpr() : ifExpr->getElseExpr();
      }

      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::SingleValueStmtResult: {
      auto branchElt = path[0].castTo<LocatorPathElt::SingleValueStmtResult>();
      auto exprIdx = branchElt.getIndex();
      auto *SVE = castToExpr<SingleValueStmtExpr>(anchor);
      SmallVector<Expr *, 4> scratch;
      anchor = SVE->getResultExprs(scratch)[exprIdx];
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::KeyPathDynamicMember:
    case ConstraintLocator::ImplicitDynamicMemberSubscript: {
      // Key path dynamic member lookup should be completely transparent.
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::ArgumentAttribute: {
      // At this point we should have already found argument expression
      // this attribute belongs to, so we can leave this element in place
      // because it points out exact location useful for diagnostics.
      break;
    }

    case ConstraintLocator::ResultBuilderBodyResult: {
      path = path.slice(1);
      break;
    }

    case ConstraintLocator::UnresolvedMemberChainResult: {
      auto *resultExpr = castToExpr<UnresolvedMemberChainResultExpr>(anchor);
      anchor = resultExpr->getSubExpr();
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::SyntacticElement: {
      auto bodyElt = path[0].castTo<LocatorPathElt::SyntacticElement>();
      anchor = bodyElt.getElement();
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::PatternMatch: {
      auto patternElt = path[0].castTo<LocatorPathElt::PatternMatch>();
      anchor = patternElt.getPattern();
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::EnumPatternImplicitCastMatch: {
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::PackType:
    case ConstraintLocator::ParentType:
    case ConstraintLocator::KeyPathType:
    case ConstraintLocator::InstanceType:
    case ConstraintLocator::PlaceholderType:
    case ConstraintLocator::SequenceElementType:
    case ConstraintLocator::ConstructorMemberType:
    case ConstraintLocator::ExistentialConstraintType:
    case ConstraintLocator::ProtocolCompositionMemberType:
      break;

    case ConstraintLocator::GenericArgument:
    case ConstraintLocator::FunctionArgument:
    case ConstraintLocator::SynthesizedArgument:
      break;

    case ConstraintLocator::FunctionResult:
    case ConstraintLocator::DynamicLookupResult:
    case ConstraintLocator::KeyPathComponentResult:
      break;

    case ConstraintLocator::GenericParameter:
      break;

    case ConstraintLocator::ThrownErrorType:
      break;

    case ConstraintLocator::OpenedGeneric:
    case ConstraintLocator::OpenedOpaqueArchetype:
      break;

    case ConstraintLocator::KeyPathRoot:
    case ConstraintLocator::KeyPathValue:
      break;

    case ConstraintLocator::ProtocolRequirement:
    case ConstraintLocator::ConditionalRequirement:
    case ConstraintLocator::ConformanceRequirement:
    case ConstraintLocator::TypeParameterRequirement:
      break;

    case ConstraintLocator::PackElement:
    case ConstraintLocator::PackShape:
    case ConstraintLocator::PackExpansionType:
      break;

    case ConstraintLocator::PackExpansionPattern: {
      if (auto *expansion = getAsExpr<PackExpansionExpr>(anchor))
        anchor = expansion->getPatternExpr();

      path = path.slice(1);
      break;
    }

    case ConstraintLocator::PatternBindingElement: {
      auto pattern = path[0].castTo<LocatorPathElt::PatternBindingElement>();
      auto *patternBinding = cast<PatternBindingDecl>(anchor.get<Decl *>());
      anchor = patternBinding->getInit(pattern.getIndex());
      // If this pattern is uninitialized, let's use it as anchor.
      if (!anchor)
        anchor = patternBinding->getPattern(pattern.getIndex());
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::NamedPatternDecl: {
      auto pattern = cast<NamedPattern>(anchor.get<Pattern *>());
      anchor = pattern->getDecl();
      path = path.slice(1);
      break;
    }

    case ConstraintLocator::AnyPatternDecl: {
      // This element is just a marker for `_` pattern since it doesn't
      // have a declaration. We need to make sure that it only appaears
      // when anchored on `AnyPattern`.
      assert(getAsPattern<AnyPattern>(anchor));
      path = path.slice(1);
      break;
    }

    case ConstraintLocator::ImplicitConversion:
      break;

    case ConstraintLocator::Witness:
    case ConstraintLocator::WrappedValue:
    case ConstraintLocator::OptionalInjection:
    case ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice:
    case ConstraintLocator::FallbackType:
    case ConstraintLocator::KeyPathSubscriptIndex:
    case ConstraintLocator::ExistentialMemberAccessConversion:
      break;
    }

    // If we get here, we couldn't simplify the path further.
    break;
  }
}

ASTNode constraints::simplifyLocatorToAnchor(ConstraintLocator *locator) {
  if (!locator)
    return nullptr;

  auto anchor = locator->getAnchor();
  if (!anchor)
    return {};

  SourceRange range;
  auto path = locator->getPath();
  simplifyLocator(anchor, path, range);

  // We only want the new anchor if all the path elements have been simplified
  // away.
  return path.empty() ? anchor : nullptr;
}

Expr *constraints::getArgumentExpr(ASTNode node, unsigned index) {
  auto *expr = getAsExpr(node);
  if (!expr)
    return nullptr;

  auto *argList = expr->getArgs();
  if (!argList)
    return nullptr;

  if (index >= argList->size())
    return nullptr;

  return argList->getExpr(index);
}

bool constraints::isAutoClosureArgument(Expr *argExpr) {
  if (!argExpr)
    return false;

  if (auto *DRE = dyn_cast<DeclRefExpr>(argExpr)) {
    if (auto *param = dyn_cast<ParamDecl>(DRE->getDecl()))
      return param->isAutoClosure();
  }

  return false;
}

bool constraints::hasAppliedSelf(ConstraintSystem &cs,
                                 const OverloadChoice &choice) {
  return hasAppliedSelf(choice, [&cs](Type type) -> Type {
    return cs.getFixedTypeRecursive(type, /*wantRValue=*/true);
  });
}

bool constraints::hasAppliedSelf(const Solution &S,
                                 const OverloadChoice &choice) {
  return hasAppliedSelf(choice, [&](Type type) -> Type {
    return S.simplifyType(type);
  });
}

bool constraints::hasAppliedSelf(const OverloadChoice &choice,
                                 llvm::function_ref<Type(Type)> getFixedType) {
  auto *decl = choice.getDeclOrNull();
  if (!decl)
    return false;

  auto baseType = choice.getBaseType();
  if (baseType)
    baseType = getFixedType(baseType)->getRValueType();

  // In most cases where we reference a declaration with a curried self
  // parameter, it gets dropped from the type of the reference.
  return decl->hasCurriedSelf() &&
         doesMemberRefApplyCurriedSelf(baseType, decl);
}

/// Check whether given type conforms to `RawRepresentable` protocol
/// and return the witness type.
Type constraints::isRawRepresentable(ConstraintSystem &cs, Type type) {
  auto rawReprType = TypeChecker::getProtocol(
      cs.getASTContext(), SourceLoc(), KnownProtocolKind::RawRepresentable);
  if (!rawReprType)
    return Type();

  auto conformance = cs.lookupConformance(type, rawReprType);
  if (conformance.isInvalid())
    return Type();

  return conformance.getTypeWitnessByName(cs.getASTContext().Id_RawValue);
}

void ConstraintSystem::generateOverloadConstraints(
    SmallVectorImpl<Constraint *> &constraints, Type type,
    ArrayRef<OverloadChoice> choices, DeclContext *useDC,
    ConstraintLocator *locator, std::optional<unsigned> favoredIndex,
    bool requiresFix,
    llvm::function_ref<ConstraintFix *(unsigned, const OverloadChoice &)>
        getFix) {
  auto recordChoice = [&](SmallVectorImpl<Constraint *> &choices,
                          unsigned index, const OverloadChoice &overload,
                          bool isFavored = false) {
    auto *fix = getFix(index, overload);
    // If fix is required but it couldn't be determined, this
    // choice has be filtered out.
    if (requiresFix && !fix)
      return;

    auto *choice = Constraint::createBindOverload(*this, type, overload,
                                                  useDC, fix, locator);

    if (isFavored)
      choice->setFavored();

    choices.push_back(choice);
  };

  if (favoredIndex) {
    const auto &choice = choices[*favoredIndex];
    assert(
        (!choice.isDecl() || !isDeclUnavailable(choice.getDecl(), locator)) &&
        "Cannot make unavailable decl favored!");
    recordChoice(constraints, *favoredIndex, choice, /*isFavored=*/true);
  }

  for (auto index : indices(choices)) {
    if (favoredIndex && (*favoredIndex == index))
      continue;

    recordChoice(constraints, index, choices[index]);
  }
}

ConstraintLocator *
ConstraintSystem::getArgumentInfoLocator(ConstraintLocator *locator) {
  auto anchor = locator->getAnchor();

  // An empty locator which code completion uses for member references.
  if (anchor.isNull() && locator->getPath().empty())
    return nullptr;

  if (locator->findLast<LocatorPathElt::ImplicitConversion>())
    return locator;

  // Applies and unresolved member exprs can have callee locators that are
  // dependent on the type of their function, which may not have been resolved
  // yet. Therefore we need to handle them specially.
  if (auto *apply = getAsExpr<ApplyExpr>(anchor)) {
    auto *fnExpr = getArgumentLabelTargetExpr(apply->getFn());
    return getConstraintLocator(fnExpr);
  }

  if (auto *UME = getAsExpr<UnresolvedMemberExpr>(anchor))
    return getConstraintLocator(UME);

  // All implicit x[dynamicMember:] subscript calls can share the same argument
  // list.
  if (locator->findLast<LocatorPathElt::ImplicitDynamicMemberSubscript>()) {
    return getConstraintLocator(
        ASTNode(), LocatorPathElt::ImplicitDynamicMemberSubscript());
  }

  auto path = locator->getPath();
  {
    // If this is for a dynamic member reference, the argument info is for the
    // original call-site, which we can get by stripping away the
    // KeyPathDynamicMember elements.
    auto iter = path.begin();
    if (locator->findFirst<LocatorPathElt::KeyPathDynamicMember>(iter)) {
      ArrayRef<LocatorPathElt> newPath(path.begin(), iter);
      return getConstraintLocator(anchor, newPath);
    }
  }

  return getCalleeLocator(locator);
}

ArgumentList *ConstraintSystem::getArgumentList(ConstraintLocator *locator) {
  if (!locator)
    return nullptr;

  if (auto *infoLocator = getArgumentInfoLocator(locator)) {
    auto known = ArgumentLists.find(infoLocator);
    if (known != ArgumentLists.end())
      return known->second;
  }
  return nullptr;
}

void ConstraintSystem::recordArgumentList(ConstraintLocator *locator,
                                          ArgumentList *args) {
  bool inserted = ArgumentLists.insert({locator, args}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedArgumentList(locator));
}

void ConstraintSystem::associateArgumentList(ConstraintLocator *locator,
                                             ArgumentList *args) {
  ASSERT(locator && locator->getAnchor());
  auto *argLoc = getArgumentInfoLocator(locator);
  recordArgumentList(argLoc, args);
}

ArgumentList *Solution::getArgumentList(ConstraintLocator *locator) const {
  if (!locator)
    return nullptr;

  if (auto *infoLocator = constraintSystem->getArgumentInfoLocator(locator)) {
    auto known = argumentLists.find(infoLocator);
    if (known != argumentLists.end())
      return known->second;
  }
  return nullptr;
}

std::optional<ConversionRestrictionKind>
Solution::getConversionRestriction(CanType type1, CanType type2) const {
  auto restriction = ConstraintRestrictions.find({type1, type2});
  if (restriction != ConstraintRestrictions.end())
    return restriction->second;
  return std::nullopt;
}

#ifndef NDEBUG
/// Given an apply expr, returns true if it is expected to have a direct callee
/// overload, resolvable using `getChoiceFor`. Otherwise, returns false.
static bool shouldHaveDirectCalleeOverload(const CallExpr *callExpr) {
  auto *fnExpr = callExpr->getDirectCallee();

  // An apply of an apply/subscript doesn't have a direct callee.
  if (isa<ApplyExpr>(fnExpr) || isa<SubscriptExpr>(fnExpr))
    return false;

  // Applies of closures don't have callee overloads.
  if (isa<ClosureExpr>(fnExpr))
    return false;

  // No direct callee for a try!/try?.
  if (isa<ForceTryExpr>(fnExpr) || isa<OptionalTryExpr>(fnExpr))
    return false;

  // If we have an intermediate cast, there's no direct callee.
  if (isa<ExplicitCastExpr>(fnExpr))
    return false;

  // No direct callee for a ternary expr.
  if (isa<TernaryExpr>(fnExpr))
    return false;

  // Assume that anything else would have a direct callee.
  return true;
}
#endif

ASTNode ConstraintSystem::includingParentApply(ASTNode node) {
  if (auto *expr = getAsExpr(node)) {
    if (auto apply = getAsExpr<ApplyExpr>(getParentExpr(expr))) {
      if (apply->getFn() == expr)
        return apply;
    }
  }
  return node;
}

GenericEnvironment *
Solution::getPackExpansionEnvironment(PackExpansionExpr *expr) const {
  auto iter = PackExpansionEnvironments.find(expr);
  if (iter == PackExpansionEnvironments.end())
    return nullptr;

  return iter->second;
}

std::optional<FunctionArgApplyInfo>
Solution::getFunctionArgApplyInfo(ConstraintLocator *locator) const {
  // It's only valid to use `&` in argument positions, but we need
  // to figure out exactly where it was used.
  if (auto *argExpr = getAsExpr<InOutExpr>(locator->getAnchor())) {
    auto *argLoc = getConstraintSystem().getArgumentLocator(argExpr);
    if (!argLoc)
      return std::nullopt;

    locator = argLoc;
  }

  auto anchor = locator->getAnchor();
  auto path = locator->getPath();

  // Look for the apply-arg-to-param element in the locator's path. We may
  // have to look through other elements that are generated from an argument
  // conversion such as GenericArgument for an optional-to-optional conversion,
  // and OptionalInjection for a value-to-optional conversion.
  auto iter = path.rbegin();
  auto applyArgElt = locator->findLast<LocatorPathElt::ApplyArgToParam>(iter);
  if (!applyArgElt)
    return std::nullopt;

#ifndef NDEBUG
  auto nextIter = iter + 1;
  assert(!locator->findLast<LocatorPathElt::ApplyArgToParam>(nextIter) &&
         "Multiple ApplyArgToParam components?");
#endif

  // Form a new locator that ends at the apply-arg-to-param element, and
  // simplify it to get the full argument expression.
  auto argPath = path.drop_back(iter - path.rbegin());
  auto *argLocator = getConstraintLocator(anchor, argPath);

  auto *argExpr = castToExpr(simplifyLocatorToAnchor(argLocator));

  // If we were unable to simplify down to the argument expression, we don't
  // know what this is.
  if (!argExpr)
    return std::nullopt;

  auto *argList = getArgumentList(argLocator);
  if (!argList)
    return std::nullopt;

  std::optional<OverloadChoice> choice;
  Type rawFnType;
  auto *calleeLocator = getCalleeLocator(argLocator);
  if (auto overload = getOverloadChoiceIfAvailable(calleeLocator)) {
    // If we have resolved an overload for the callee, then use that to get the
    // function type and callee.
    choice = overload->choice;
    rawFnType = overload->adjustedOpenedType;
  } else {
    // If we didn't resolve an overload for the callee, we should be dealing
    // with a call of an arbitrary function expr.
    auto *call = castToExpr<CallExpr>(anchor);
    rawFnType = getType(call->getFn());

    // If callee couldn't be resolved due to expression
    // issues e.g. it's a reference to an invalid member
    // let's just return here.
    if (simplifyType(rawFnType)->is<UnresolvedType>())
      return std::nullopt;

    // A tuple construction is spelled in the AST as a function call, but
    // is really more like a tuple conversion.
    if (auto metaTy = simplifyType(rawFnType)->getAs<MetatypeType>()) {
      if (metaTy->getInstanceType()->is<TupleType>())
        return std::nullopt;
    }

    assert(!shouldHaveDirectCalleeOverload(call) &&
             "Should we have resolved a callee for this?");
  }

  // Try to resolve the function type by loading lvalues and looking through
  // optional types, which can occur for expressions like `fn?(5)`.
  auto *fnType = simplifyType(rawFnType)
                     ->getRValueType()
                     ->lookThroughAllOptionalTypes()
                     ->getAs<FunctionType>();
  if (!fnType)
    return std::nullopt;

  // Resolve the interface type for the function. Note that this may not be a
  // function type, for example it could be a generic parameter.
  Type fnInterfaceType;
  auto *callee = choice ? choice->getDeclOrNull() : nullptr;
  if (callee && callee->hasInterfaceType()) {
    // If we have a callee with an interface type, we can use it. This is
    // preferable to simplifyType for the function, as this will allow us to get
    // a GenericFunctionType for generic decls.
    //
    // Note that it's possible to find a callee without an interface type. This
    // can happen for example with closure parameters, where the interface type
    // isn't set until the solution is applied. In that case, use simplifyType.
    fnInterfaceType = callee->getInterfaceType();

    // Strip off the curried self parameter if necessary.
    if (hasAppliedSelf(*this, *choice))
      fnInterfaceType = fnInterfaceType->castTo<AnyFunctionType>()->getResult();

#ifndef NDEBUG
    // If variadic generics are not involved, interface type should
    // always match applied type.
    if (auto *fn = fnInterfaceType->getAs<AnyFunctionType>()) {
      if (llvm::none_of(fn->getParams(), [&](const auto &param) {
            return param.getPlainType()->hasParameterPack();
          })) {
        assert(fn->getNumParams() == fnType->getNumParams() &&
               "Parameter mismatch?");
      }
    }
#endif
  } else {
    fnInterfaceType = simplifyType(rawFnType, /*wantInterfaceType*/ true);
  }

  auto argIdx = applyArgElt->getArgIdx();
  auto paramIdx = applyArgElt->getParamIdx();

  return FunctionArgApplyInfo::get(argList, argExpr, argIdx,
                                   simplifyType(getType(argExpr)), paramIdx,
                                   fnInterfaceType, fnType, callee);
}

bool constraints::isKnownKeyPathType(Type type) {
  return type->isKeyPath() || type->isWritableKeyPath() ||
         type->isReferenceWritableKeyPath() || type->isPartialKeyPath() ||
         type->isAnyKeyPath();
}

bool constraints::isTypeErasedKeyPathType(Type type) {
  assert(type);

  if (type->isPartialKeyPath() || type->isAnyKeyPath())
    return true;

  if (!type->isExistentialType())
    return false;

  auto superclass = type->getSuperclass();
  return superclass ? isTypeErasedKeyPathType(superclass) : false;
}

bool constraints::hasResultExpr(ClosureExpr *closure) {
  auto &ctx = closure->getASTContext();
  return evaluateOrDefault(ctx.evaluator, ClosureHasResultExprRequest{closure},
                           false);
}

Type constraints::getConcreteReplacementForProtocolSelfType(ValueDecl *member) {
  auto *DC = member->getDeclContext();

  if (!DC->getSelfProtocolDecl())
    return Type();

  GenericSignature signature;
  if (auto *genericContext = member->getAsGenericContext()) {
    signature = genericContext->getGenericSignature();
  } else {
    signature = DC->getGenericSignatureOfContext();
  }

  auto selfTy = DC->getSelfInterfaceType();
  return signature->getConcreteType(selfTy);
}

static bool isOperator(Expr *expr, StringRef expectedName) {
  auto name = getOperatorName(expr);
  return name ? name->is(expectedName) : false;
}

std::optional<Identifier> constraints::getOperatorName(Expr *expr) {
  ValueDecl *choice = nullptr;
  if (auto *ODRE = dyn_cast_or_null<OverloadedDeclRefExpr>(expr)) {
    choice = ODRE->getDecls().front();
  } else if (auto *DRE = dyn_cast_or_null<DeclRefExpr>(expr)) {
    choice = DRE->getDecl();
  } else {
    return std::nullopt;
  }

  if (auto *FD = dyn_cast_or_null<AbstractFunctionDecl>(choice))
    return FD->getBaseIdentifier();

  return std::nullopt;
}

bool constraints::isPatternMatchingOperator(ASTNode node) {
  auto *expr = getAsExpr(node);
  if (!expr) return false;

  return isOperator(expr, "~=");
}

bool constraints::isStandardComparisonOperator(ASTNode node) {
  auto *expr = getAsExpr(node);
  if (!expr) return false;

  if (auto opName = getOperatorName(expr)) {
    return opName->isStandardComparisonOperator();
  }
  return false;
}

ConstraintLocator *ConstraintSystem::getArgumentLocator(Expr *expr) {
  auto *application = getParentExpr(expr);
  if (!application)
    return nullptr;

  // Drop all of the semantically insignificant exprs that might be wrapping an
  // argument e.g. `test(((42)))`
  while (application->getSemanticsProvidingExpr() == expr) {
    application = getParentExpr(application);
    if (!application)
      return nullptr;
  }

  ArgumentList *argList = application->getArgs();
  if (!argList && !isa<KeyPathExpr>(application))
    return nullptr;

  ConstraintLocator *loc = nullptr;
  if (auto *KP = dyn_cast<KeyPathExpr>(application)) {
    auto idx = KP->findComponentWithSubscriptArg(expr);
    if (!idx)
      return nullptr;
    loc = getConstraintLocator(KP, {LocatorPathElt::KeyPathComponent(*idx)});
    argList = KP->getComponents()[*idx].getArgs();
  } else {
    loc = getConstraintLocator(application);
  }
  assert(argList);

  auto argIdx = argList->findArgumentExpr(expr);
  if (!argIdx)
    return nullptr;

  ParameterTypeFlags flags;
  flags = flags.withInOut(argList->get(*argIdx).isInOut());
  return getConstraintLocator(
      loc, {LocatorPathElt::ApplyArgument(),
            LocatorPathElt::ApplyArgToParam(*argIdx, *argIdx, flags)});
}

bool constraints::isOperatorArgument(ConstraintLocator *locator,
                                     StringRef expectedOperator) {
  if (!locator->findLast<LocatorPathElt::ApplyArgToParam>())
    return false;

  if (auto *AE = getAsExpr<ApplyExpr>(locator->getAnchor())) {
    if (isa<PrefixUnaryExpr>(AE) || isa<BinaryExpr>(AE) ||
        isa<PostfixUnaryExpr>(AE))
      return expectedOperator.empty() ||
             isOperator(AE->getFn(), expectedOperator);
  }

  return false;
}

bool constraints::isArgumentOfPatternMatchingOperator(
    ConstraintLocator *locator) {
  auto *binaryOp = getAsExpr<BinaryExpr>(locator->getAnchor());
  if (!(binaryOp && binaryOp->isImplicit()))
    return false;
  return isPatternMatchingOperator(binaryOp->getFn());
}

bool constraints::isArgumentOfReferenceEqualityOperator(
    ConstraintLocator *locator) {
  return isOperatorArgument(locator, "===") ||
         isOperatorArgument(locator, "!==");
}

bool ConstraintSystem::isArgumentOfImportedDecl(
    ConstraintLocatorBuilder locator) {
  SmallVector<LocatorPathElt, 4> path;
  auto anchor = locator.getLocatorParts(path);

  if (path.empty())
    return false;

  while (!path.empty()) {
    const auto &last = path.back();

    // Drop all of the `optional payload` or `generic argument`
    // locator elements at the end of the path, they came from
    // either value-to-optional promotion or optional-to-optional
    // conversion.
    if (last.is<LocatorPathElt::OptionalInjection>() ||
        last.is<LocatorPathElt::GenericArgument>()) {
      path.pop_back();
      continue;
    }

    break;
  }

  auto *application = getCalleeLocator(getConstraintLocator(anchor, path));

  auto overload = findSelectedOverloadFor(application);
  if (!(overload && overload->choice.isDecl()))
    return false;

  auto *choice = overload->choice.getDecl();
  return choice->hasClangNode();
}

ConversionEphemeralness
ConstraintSystem::isConversionEphemeral(ConversionRestrictionKind conversion,
                                        ConstraintLocatorBuilder locator) {
  switch (conversion) {
  case ConversionRestrictionKind::ArrayToPointer:
  case ConversionRestrictionKind::ArrayToCPointer:
  case ConversionRestrictionKind::StringToPointer:
    // Always ephemeral.
    return ConversionEphemeralness::Ephemeral;
  case ConversionRestrictionKind::InoutToPointer:
  case ConversionRestrictionKind::InoutToCPointer: {

    // Ephemeral, except if the expression is a reference to a global or
    // static stored variable, or a directly accessed stored property on such a
    // variable.

    auto isDirectlyAccessedStoredVar = [&](ValueDecl *decl) -> bool {
      auto *asd = dyn_cast_or_null<AbstractStorageDecl>(decl);
      if (!asd)
        return false;

      // Check what access strategy is used for a read-write access. It must be
      // direct-to-storage in order for the conversion to be non-ephemeral.
      return asd->isAccessedViaPhysicalStorage(
          AccessSemantics::Ordinary, AccessKind::ReadWrite,
          DC->getParentModule(), DC->getResilienceExpansion());
    };

    SourceRange range;
    auto *argLoc = simplifyLocator(*this, getConstraintLocator(locator), range);
    auto *subExpr =
        castToExpr(argLoc->getAnchor())->getSemanticsProvidingExpr();

    // Look through an InOutExpr if we have one. This is usually the case, but
    // might not be if e.g we're applying an 'add missing &' fix.
    if (auto *ioe = dyn_cast<InOutExpr>(subExpr))
      subExpr = ioe->getSubExpr();

    while (true) {
      subExpr = subExpr->getSemanticsProvidingExpr();

      // Look through force unwraps, which can be modelled as physical lvalue
      // components.
      if (auto *fve = dyn_cast<ForceValueExpr>(subExpr)) {
        subExpr = fve->getSubExpr();
        continue;
      }

      // Look through a member reference if it's directly accessed.
      if (auto *ude = dyn_cast<UnresolvedDotExpr>(subExpr)) {
        auto overload = findSelectedOverloadFor(ude);

        // If we didn't find an overload, it hasn't been resolved yet.
        if (!overload)
          return ConversionEphemeralness::Unresolved;

        // Tuple indices are always non-ephemeral.
        auto *base = ude->getBase();
        if (overload->choice.getKind() == OverloadChoiceKind::TupleIndex) {
          subExpr = base;
          continue;
        }

        // If we don't have a directly accessed declaration associated with the
        // choice, it's ephemeral.
        auto *member = overload->choice.getDeclOrNull();
        if (!isDirectlyAccessedStoredVar(member))
          return ConversionEphemeralness::Ephemeral;

        // If we found a static member, the conversion is non-ephemeral. We can
        // stop iterating as there's nothing interesting about the base.
        if (member->isStatic())
          return ConversionEphemeralness::NonEphemeral;

        // For an instance member, the base must be an @lvalue struct type.
        if (auto *lvt = simplifyType(getType(base))->getAs<LValueType>()) {
          auto *nominal = lvt->getObjectType()->getAnyNominal();
          if (isa_and_nonnull<StructDecl>(nominal)) {
            subExpr = base;
            continue;
          }
        }
        return ConversionEphemeralness::Ephemeral;
      }

      break;
    }

    auto getBaseEphemeralness =
        [&](ValueDecl *base) -> ConversionEphemeralness {
      // We must have a base decl that's directly accessed.
      if (!isDirectlyAccessedStoredVar(base))
        return ConversionEphemeralness::Ephemeral;

      // The base decl must either be static or global in order for it to be
      // non-ephemeral.
      if (base->isStatic() || base->getDeclContext()->isModuleScopeContext()) {
        return ConversionEphemeralness::NonEphemeral;
      } else {
        return ConversionEphemeralness::Ephemeral;
      }
    };

    // Fast path: We have a direct decl ref.
    if (auto *dre = dyn_cast<DeclRefExpr>(subExpr))
      return getBaseEphemeralness(dre->getDecl());

    // Otherwise, try to find an overload for the base.
    if (auto baseOverload = findSelectedOverloadFor(subExpr))
      return getBaseEphemeralness(baseOverload->choice.getDeclOrNull());

    // If we didn't find a base overload for a unresolved member or overloaded
    // decl, it hasn't been resolved yet.
    if (isa<UnresolvedMemberExpr>(subExpr) ||
        isa<OverloadedDeclRefExpr>(subExpr))
      return ConversionEphemeralness::Unresolved;

    // Otherwise, we don't know what we're dealing with. Default to ephemeral.
    return ConversionEphemeralness::Ephemeral;
  }
  case ConversionRestrictionKind::DeepEquality:
  case ConversionRestrictionKind::Superclass:
  case ConversionRestrictionKind::Existential:
  case ConversionRestrictionKind::MetatypeToExistentialMetatype:
  case ConversionRestrictionKind::ExistentialMetatypeToMetatype:
  case ConversionRestrictionKind::ValueToOptional:
  case ConversionRestrictionKind::OptionalToOptional:
  case ConversionRestrictionKind::ClassMetatypeToAnyObject:
  case ConversionRestrictionKind::ExistentialMetatypeToAnyObject:
  case ConversionRestrictionKind::ProtocolMetatypeToProtocolClass:
  case ConversionRestrictionKind::PointerToPointer:
  case ConversionRestrictionKind::PointerToCPointer:
  case ConversionRestrictionKind::ArrayUpcast:
  case ConversionRestrictionKind::DictionaryUpcast:
  case ConversionRestrictionKind::SetUpcast:
  case ConversionRestrictionKind::HashableToAnyHashable:
  case ConversionRestrictionKind::CFTollFreeBridgeToObjC:
  case ConversionRestrictionKind::ObjCTollFreeBridgeToCF:
  case ConversionRestrictionKind::CGFloatToDouble:
  case ConversionRestrictionKind::DoubleToCGFloat:
    // @_nonEphemeral has no effect on these conversions, so treat them as all
    // being non-ephemeral in order to allow their passing to an @_nonEphemeral
    // parameter.
    return ConversionEphemeralness::NonEphemeral;
  }
  llvm_unreachable("invalid conversion restriction kind");
}

Expr *ConstraintSystem::buildAutoClosureExpr(Expr *expr,
                                             FunctionType *closureType,
                                             DeclContext *ClosureContext,
                                             bool isDefaultWrappedValue,
                                             bool isAsyncLetWrapper) {
  auto &Context = DC->getASTContext();
  bool isInDefaultArgumentContext = false;
  if (auto *init = dyn_cast<Initializer>(DC)) {
    auto initKind = init->getInitializerKind();
    isInDefaultArgumentContext =
        initKind == InitializerKind::DefaultArgument ||
        (initKind == InitializerKind::PatternBinding && isDefaultWrappedValue);
  }

  auto info = closureType->getExtInfo();
  auto newClosureType = closureType;

  if (isInDefaultArgumentContext && info.isNoEscape())
    newClosureType = closureType->withExtInfo(info.withNoEscape(false))
                         ->castTo<FunctionType>();

  auto *closure = new (Context)
      AutoClosureExpr(expr, newClosureType, ClosureContext);

  closure->setParameterList(ParameterList::createEmpty(Context));

  if (isAsyncLetWrapper)
    closure->setThunkKind(AutoClosureExpr::Kind::AsyncLet);

  Expr *result = closure;

  if (!newClosureType->isEqual(closureType)) {
    assert(isInDefaultArgumentContext);
    assert(newClosureType
               ->withExtInfo(newClosureType->getExtInfo().withNoEscape(true))
               ->isEqual(closureType));
    result = new (Context) FunctionConversionExpr(closure, closureType);
  }

  cacheExprTypes(result);
  return result;
}

Expr *ConstraintSystem::buildTypeErasedExpr(Expr *expr, DeclContext *dc,
                                            Type contextualType,
                                            ContextualTypePurpose purpose) {
  auto &ctx = dc->getASTContext();

  if (purpose != CTP_ReturnStmt)
    return expr;

  auto *decl = dyn_cast_or_null<ValueDecl>(dc->getAsDecl());
  if (!decl ||
      (!Context.LangOpts.hasFeature(Feature::OpaqueTypeErasure) &&
       !(decl->isDynamic() || decl->getDynamicallyReplacedDecl())))
    return expr;

  auto *opaque = contextualType->getAs<OpaqueTypeArchetypeType>();
  if (!opaque)
    return expr;

  auto protocols = opaque->getConformsTo();
  if (protocols.size() != 1)
    return expr;

  auto *PD = protocols.front();

  auto contextAvailability =
      AvailabilityContext::forLocation(expr->getLoc(), dc);
  auto refinedAvailability =
      AvailabilityContext::forPlatformRange(
        AvailabilityRange::alwaysAvailable(), ctx);

  // The least available type eraser for the enclosing context.
  Type typeEraser;
  for (auto *attr : PD->getAttrs().getAttributes<TypeEraserAttr>()) {
    auto eraser = attr->getResolvedType(PD);
    assert(eraser && "Failed to resolve eraser type!");

    auto *nominal = eraser->getAnyNominal();
    auto nominalAvailability = AvailabilityContext::forDeclSignature(nominal);

    if (contextAvailability.isContainedIn(nominalAvailability) &&
        nominalAvailability.isContainedIn(refinedAvailability)) {
      refinedAvailability = nominalAvailability;
      typeEraser = eraser;
    }
  }

  if (!typeEraser)
    return expr;

  auto *argList = ArgumentList::forImplicitSingle(ctx, ctx.Id_erasing, expr);
  return CallExpr::createImplicit(
      ctx, TypeExpr::createImplicit(typeEraser, ctx), argList);
}

/// If an UnresolvedDotExpr, SubscriptMember, etc has been resolved by the
/// constraint system, return the decl that it references.
ValueDecl *ConstraintSystem::findResolvedMemberRef(ConstraintLocator *locator) {
  // See if we have a resolution for this member.
  auto overload = findSelectedOverloadFor(locator);
  if (!overload)
    return nullptr;

  // We only want to handle the simplest decl binding.
  auto choice = overload->choice;
  if (choice.getKind() != OverloadChoiceKind::Decl)
    return nullptr;

  return choice.getDecl();
}

void SyntacticElementTargetKey::dump() const { dump(llvm::errs()); }

void SyntacticElementTargetKey::dump(raw_ostream &OS) const {
  switch (kind) {
  case Kind::empty:
    OS << "<empty>\n";
    return;

  case Kind::tombstone:
    OS << "<tombstone>\n";
    return;
    
  case Kind::stmtCondElement:
    // TODO: Implement a proper dump function for StmtConditionElement
    OS << "statement condition element\n";
    return;

  case Kind::expr:
  case Kind::closure:
    storage.expr->dump(OS);
    return;

  case Kind::stmt:
    storage.stmt->dump(OS);
    return;

  case Kind::pattern:
    storage.pattern->dump(OS);
    return;

  case Kind::patternBindingEntry:
    OS << "pattern binding entry " << storage.patternBindingEntry.index
       << " in\n";
    storage.patternBindingEntry.patternBinding->dump(OS);
    return;

  case Kind::varDecl:
    storage.varDecl->dump(OS);
    return;

  case Kind::functionRef:
    OS << "<function>\n";
    storage.functionRef->printContext(OS);
    return;
  }
  llvm_unreachable("invalid statement kind");
}

/// Given a specific expression and the remnants of the failed constraint
/// system, produce a specific diagnostic.
///
/// This is guaranteed to always emit an error message.
///
void ConstraintSystem::diagnoseFailureFor(SyntacticElementTarget target) {
  setPhase(ConstraintSystemPhase::Diagnostics);

  SWIFT_DEFER { setPhase(ConstraintSystemPhase::Finalization); };

  auto &DE = getASTContext().Diags;

  // If constraint system is in invalid state always produce
  // a fallback diagnostic that asks to file a bug.
  if (inInvalidState()) {
    DE.diagnose(target.getLoc(), diag::failed_to_produce_diagnostic);
    return;
  }

  if (auto expr = target.getAsExpr()) {
    if (auto *assignment = dyn_cast<AssignExpr>(expr)) {
      if (isa<DiscardAssignmentExpr>(assignment->getDest()))
        expr = assignment->getSrc();
    }

    // Look through RebindSelfInConstructorExpr to avoid weird Sema issues.
    if (auto *RB = dyn_cast<RebindSelfInConstructorExpr>(expr))
      expr = RB->getSubExpr();

    // Unresolved/Anonymous ClosureExprs are common enough that we should give
    // them tailored diagnostics.
    if (auto *closure = dyn_cast<ClosureExpr>(expr->getValueProvidingExpr())) {
      DE.diagnose(closure->getLoc(), diag::cannot_infer_closure_type)
        .highlight(closure->getSourceRange());
      return;
    }

    // If no one could find a problem with this expression or constraint system,
    // then it must be well-formed... but is ambiguous.  Handle this by
    // diagnostic various cases that come up.
    DE.diagnose(expr->getLoc(), diag::type_of_expression_is_ambiguous)
        .highlight(expr->getSourceRange());
  } else if (auto *wrappedVar = target.getAsUninitializedWrappedVar()) {
    auto *outerWrapper = wrappedVar->getOutermostAttachedPropertyWrapper();
    Type propertyType = wrappedVar->getInterfaceType();
    Type wrapperType = outerWrapper->getType();

    // Emit the property wrapper fallback diagnostic
    wrappedVar->diagnose(diag::property_wrapper_incompatible_property,
                         propertyType, wrapperType);
    if (auto nominal = wrapperType->getAnyNominal()) {
      nominal->diagnose(diag::property_wrapper_declared_here,
                        nominal->getName());
    }
  } else if (target.getAsUninitializedVar()) {
    DE.diagnose(target.getLoc(), diag::failed_to_produce_diagnostic);
  } else if (target.isForEachPreamble()) {
    DE.diagnose(target.getLoc(), diag::failed_to_produce_diagnostic);
  } else {
    // Emit a poor fallback message.
    DE.diagnose(target.getAsFunction()->getLoc(),
                diag::failed_to_produce_diagnostic);
  }
}

bool ConstraintSystem::isDeclUnavailable(const Decl *D,
                                         ConstraintLocator *locator) const {
  SourceLoc loc;
  if (locator) {
    if (auto anchor = locator->getAnchor())
      loc = getLoc(anchor);
  }

  return getUnsatisfiedAvailabilityConstraint(D, DC, loc).has_value();
}

bool ConstraintSystem::isConformanceUnavailable(ProtocolConformanceRef conformance,
                                                ConstraintLocator *locator) const {
  if (!conformance.isConcrete())
    return false;

  auto *concrete = conformance.getConcrete();
  auto *rootConf = concrete->getRootConformance();
  auto *ext = dyn_cast<ExtensionDecl>(rootConf->getDeclContext());
  if (ext == nullptr)
    return false;

  return isDeclUnavailable(ext, locator);
}

/// If we aren't certain that we've emitted a diagnostic, emit a fallback
/// diagnostic.
void ConstraintSystem::maybeProduceFallbackDiagnostic(
    SyntacticElementTarget target) const {
  if (Options.contains(ConstraintSystemFlags::SuppressDiagnostics))
    return;

  // Before producing fatal error here, let's check if there are any "error"
  // diagnostics already emitted or waiting to be emitted. Because they are
  // a better indication of the problem.
  ASTContext &ctx = getASTContext();
  if (ctx.hadError() ||
      (diagnosticTransaction && diagnosticTransaction->hasErrors()))
    return;

  ctx.Diags.diagnose(target.getLoc(), diag::failed_to_produce_diagnostic);
}

SourceLoc constraints::getLoc(ASTNode anchor) {
  if (auto *E = anchor.dyn_cast<Expr *>()) {
    return E->getLoc();
  } else if (auto *T = anchor.dyn_cast<TypeRepr *>()) {
    return T->getLoc();
  } else if (auto *V = anchor.dyn_cast<Decl *>()) {
    if (auto VD = dyn_cast<VarDecl>(V))
      return VD->getNameLoc();
    return anchor.getStartLoc();
  } else if (auto *S = anchor.dyn_cast<Stmt *>()) {
    return S->getStartLoc();
  } else if (auto *P = anchor.dyn_cast<Pattern *>()) {
    return P->getLoc();
  } else if (auto *C = anchor.dyn_cast<StmtConditionElement *>()) {
    return C->getStartLoc();
  } else {
    auto *I = anchor.get<CaseLabelItem *>();
    return I->getStartLoc();
  }
}

SourceRange constraints::getSourceRange(ASTNode anchor) {
  return anchor.getSourceRange();
}

static std::optional<Requirement>
getRequirement(ConstraintSystem &cs, ConstraintLocator *reqLocator) {
  ArrayRef<LocatorPathElt> path = reqLocator->getPath();

  // If we have something like ... -> type req # -> pack element #, we're
  // solving a requirement of the form T : P where T is a type parameter pack
  if (!path.empty() && path.back().is<LocatorPathElt::PackElement>())
    path = path.drop_back();

  if (path.empty())
    return std::nullopt;

  auto reqLoc = path.back().getAs<LocatorPathElt::AnyRequirement>();
  if (!reqLoc)
    return std::nullopt;

  if (reqLoc->isConditionalRequirement()) {
    auto conformanceRef =
        reqLocator->findLast<LocatorPathElt::ConformanceRequirement>();
    assert(conformanceRef && "Invalid locator for a conditional requirement");

    auto conformance = conformanceRef->getConformance();
    return conformance->getConditionalRequirements()[reqLoc->getIndex()];
  }

  if (auto openedGeneric =
          reqLocator->findLast<LocatorPathElt::OpenedGeneric>()) {
    auto signature = openedGeneric->getSignature();
    return signature.getRequirements()[reqLoc->getIndex()];
  }

  return std::nullopt;
}

static std::optional<std::pair<GenericTypeParamType *, RequirementKind>>
getRequirementInfo(ConstraintSystem &cs, ConstraintLocator *reqLocator) {
  auto requirement = getRequirement(cs, reqLocator);
  if (!requirement)
    return std::nullopt;

  auto *GP = requirement->getFirstType()->getAs<GenericTypeParamType>();
  if (!GP)
    return std::nullopt;

  auto path = reqLocator->getPath();
  auto iter = path.rbegin();

  auto openedGeneric =
      reqLocator->findLast<LocatorPathElt::OpenedGeneric>(iter);
  assert(openedGeneric);
  (void)openedGeneric;

  auto newPath = path.drop_back(iter - path.rbegin() + 1);
  auto *baseLoc = cs.getConstraintLocator(reqLocator->getAnchor(), newPath);

  auto substitutions = cs.getOpenedTypes(baseLoc);
  auto replacement =
      llvm::find_if(substitutions, [&GP](const OpenedType &entry) {
        auto *typeVar = entry.second;
        return typeVar->getImpl().getGenericParameter() == GP;
      });

  if (replacement == substitutions.end())
    return std::nullopt;

  auto *repr = cs.getRepresentative(replacement->second);
  return std::make_pair(repr->getImpl().getGenericParameter(),
                        requirement->getKind());
}

bool ConstraintSystem::isFixedRequirement(ConstraintLocator *reqLocator,
                                          Type requirementTy) {
  if (auto reqInfo = getRequirementInfo(*this, reqLocator)) {
    auto *GP = reqInfo->first;
    auto reqKind = static_cast<unsigned>(reqInfo->second);
    return FixedRequirements.count(
        std::make_tuple(GP, reqKind, requirementTy.getPointer()));
  }

  return false;
}

void ConstraintSystem::recordFixedRequirement(ConstraintLocator *reqLocator,
                                              Type requirementTy) {
  if (auto reqInfo = getRequirementInfo(*this, reqLocator)) {
    auto *GP = reqInfo->first;
    auto reqKind = static_cast<unsigned>(reqInfo->second);
    recordFixedRequirement(GP, reqKind, requirementTy);
  }
}

void ConstraintSystem::recordFixedRequirement(GenericTypeParamType *GP,
                                              unsigned reqKind,
                                              Type requirementTy) {
  bool inserted = FixedRequirements.insert(
      std::make_tuple(GP, reqKind, requirementTy.getPointer())).second;
  if (inserted) {
    if (solverState) {
      recordChange(SolverTrail::Change::AddedFixedRequirement(
          GP, reqKind, requirementTy));
    }
  }
}

void ConstraintSystem::removeFixedRequirement(GenericTypeParamType *GP,
                                              unsigned reqKind,
                                              Type requirementTy) {
  auto key = std::make_tuple(GP, reqKind, requirementTy.getPointer());
  bool erased = FixedRequirements.erase(key);
  ASSERT(erased);
}

bool ConstraintSystem::isReadOnlyKeyPathComponent(
    const AbstractStorageDecl *storage, SourceLoc referenceLoc) {
  // See whether key paths can store to this component. (Key paths don't
  // get any special power from being formed in certain contexts, such
  // as the ability to assign to `let`s in initialization contexts, so
  // we pass null for the DC to `isSettable` here.)
  if (!getASTContext().isSwiftVersionAtLeast(5)) {
    // As a source-compatibility measure, continue to allow
    // WritableKeyPaths to be formed in the same conditions we did
    // in previous releases even if we should not be able to set
    // the value in this context.
    if (!storage->isSettableInSwift(DC)) {
      // A non-settable component makes the key path read-only, unless
      // a reference-writable component shows up later.
      return true;
    }
  } else if (!storage->isSettableInSwift(nullptr) ||
             !storage->isSetterAccessibleFrom(DC)) {
    // A non-settable component makes the key path read-only, unless
    // a reference-writable component shows up later.
    return true;
  }

  // If the setter is unavailable, then the keypath ought to be read-only
  // in this context.
  if (auto setter = storage->getOpaqueAccessor(AccessorKind::Set)) {
    if (getUnsatisfiedAvailabilityConstraint(setter, DC, referenceLoc))
      return true;
  }

  return false;
}

bool ConstraintSystem::isArgumentGenericFunction(Type argType, Expr *argExpr) {
  // Only makes sense if the argument type involves type variables somehow.
  if (!argType->hasTypeVariable())
    return false;

  // Have we bound an overload for the argument already?
  if (argExpr) {
    if (auto selectedOverload = findSelectedOverloadFor(argExpr)) {
      // If the overload choice is a generic function, then we have a generic
      // function reference.
      auto choice = selectedOverload->choice;
      if (auto func =
              dyn_cast_or_null<AbstractFunctionDecl>(choice.getDeclOrNull())) {
        if (func->isGeneric())
          return true;
      }

      return false;
    }
  }

  // We might have a type variable referring to an overload set.
  auto argTypeVar = argType->getAs<TypeVariableType>();
  if (!argTypeVar)
    return false;

  auto disjunction = getUnboundBindOverloadDisjunction(argTypeVar);
  if (!disjunction)
    return false;

  for (auto constraint : disjunction->getNestedConstraints()) {
    auto *decl = constraint->getOverloadChoice().getDeclOrNull();
    if (!decl)
      continue;

    if (auto func = dyn_cast<AbstractFunctionDecl>(decl))
      if (func->isGeneric())
        return true;
  }

  return false;
}

ProtocolConformanceRef
ConstraintSystem::lookupConformance(Type type, ProtocolDecl *protocol) {
  auto cacheKey = std::make_pair(type.getPointer(), protocol);

  auto cachedConformance = Conformances.find(cacheKey);
  if (cachedConformance != Conformances.end())
    return cachedConformance->second;

  auto conformance =
      swift::lookupConformance(type, protocol, /*allowMissing=*/true);
  Conformances[cacheKey] = conformance;
  return conformance;
}

std::pair<bool, std::optional<KeyPathCapability>>
ConstraintSystem::inferKeyPathLiteralCapability(TypeVariableType *keyPathType) {
  auto *typeLocator = keyPathType->getImpl().getLocator();
  assert(typeLocator->isLastElement<LocatorPathElt::KeyPathType>());

  auto *keyPath = castToExpr<KeyPathExpr>(typeLocator->getAnchor());
  return inferKeyPathLiteralCapability(keyPath);
}

std::pair<bool, std::optional<KeyPathCapability>>
ConstraintSystem::inferKeyPathLiteralCapability(KeyPathExpr *keyPath) {
  bool didOptionalChain = false;
  bool isSendable = true;

  auto fail = []() -> std::pair<bool, std::optional<KeyPathCapability>> {
    return std::make_pair(false, std::nullopt);
  };

  auto delay = []() -> std::pair<bool, std::optional<KeyPathCapability>> {
    return std::make_pair(true, std::nullopt);
  };

  auto success =
      [](KeyPathMutability mutability,
         bool isSendable) -> std::pair<bool, std::optional<KeyPathCapability>> {
    KeyPathCapability capability(mutability, isSendable);
    return std::make_pair(true, capability);
  };

  if (keyPath->hasSingleInvalidComponent())
    return fail();

  // If root is determined to be a hole it means that none of the components
  // are resolvable and key path is not viable.
  auto rootTy =
      getFixedTypeRecursive(getKeyPathRootType(keyPath), /*wantRValue=*/false);
  if (rootTy->isPlaceholder())
    return fail();

  auto isKnownSendability =
      [&](const KeyPathExpr::Component &component) -> bool {
    if (!Context.LangOpts.hasFeature(Feature::InferSendableFromCaptures))
      return true;

    // Key path is sendable only when all of its captures are sendable.
    if (auto *args = component.getArgs()) {
      auto *sendable = Context.getProtocol(KnownProtocolKind::Sendable);

      for (const auto &arg : *args) {
        // No need to check more or delay since we already know
        // that the type is not Sendable.
        if (!isSendable)
          break;

        auto argTy = simplifyType(getType(arg.getExpr()));

        // Sendability cannot be determined until the argument
        // is fully resolved.
        if (argTy->hasTypeVariable())
          return false;

        auto conformance = lookupConformance(argTy, sendable);
        isSendable &= bool(conformance) && !conformance.hasMissingConformance();
      }
    }

    return true;
  };

  auto mutability = KeyPathMutability::Writable;
  for (unsigned i : indices(keyPath->getComponents())) {
    auto &component = keyPath->getComponents()[i];

    switch (component.getKind()) {
    case KeyPathExpr::Component::Kind::Invalid:
    case KeyPathExpr::Component::Kind::Identity:
      break;

    case KeyPathExpr::Component::Kind::CodeCompletion: {
      return fail();
    }

    case KeyPathExpr::Component::Kind::Apply:
    case KeyPathExpr::Component::Kind::UnresolvedApply: {
      if (!isKnownSendability(component))
        return delay();
      break;
    }
    case KeyPathExpr::Component::Kind::UnresolvedSubscript:
    case KeyPathExpr::Component::Kind::Subscript: {
      if (!isKnownSendability(component))
        return delay();
      LLVM_FALLTHROUGH;
    }
    case KeyPathExpr::Component::Kind::Member:
    case KeyPathExpr::Component::Kind::UnresolvedMember: {
      auto *componentLoc =
          getConstraintLocator(keyPath, LocatorPathElt::KeyPathComponent(i));
      auto *calleeLoc = getCalleeLocator(componentLoc);
      auto overload = findSelectedOverloadFor(calleeLoc);
      if (!overload) {
        // If overload cannot be found because member is missing,
        // that's a failure.
        if (hasFixFor(componentLoc, FixKind::DefineMemberBasedOnUse))
          return fail();

        return delay();
      }

      // tuple elements do not change the capability of the key path
      auto choice = overload->choice;
      if (choice.getKind() == OverloadChoiceKind::TupleIndex) {
        continue;
      }

      // Discarded unsupported non-decl member lookups.
      if (!choice.isDecl())
        return fail();

      if (hasFixFor(componentLoc, FixKind::AllowInvalidRefInKeyPath) ||
          hasFixFor(componentLoc, FixKind::UnwrapOptionalBase) ||
          hasFixFor(componentLoc,
                    FixKind::UnwrapOptionalBaseWithOptionalResult))
        return fail();

      auto *storageDecl = dyn_cast<AbstractStorageDecl>(choice.getDecl());
      if (!isa<AbstractFunctionDecl>(choice.getDecl()) && !storageDecl) {
        return fail();
      }

      // Shared switch logic for actor isolation.
      switch (getActorIsolation(choice.getDecl())) {
      case ActorIsolation::Unspecified:
      case ActorIsolation::Nonisolated:
      case ActorIsolation::CallerIsolationInheriting:
      case ActorIsolation::NonisolatedUnsafe:
        break;

      case ActorIsolation::Erased:
        llvm_unreachable("storage cannot have opaque isolation");

      // A reference to an actor-isolated state makes key path non-Sendable.
      case ActorIsolation::ActorInstance:
      case ActorIsolation::GlobalActor:
        isSendable = false;
        break;
      }

      if (storageDecl) {
        if (isReadOnlyKeyPathComponent(storageDecl, component.getLoc())) {
          mutability = KeyPathMutability::ReadOnly;
          continue;
        }

        // A nonmutating setter indicates a reference-writable base.
        if (!storageDecl->isSetterMutating()) {
          mutability = KeyPathMutability::ReferenceWritable;
          continue;
        }
      }

      // Otherwise, the key path maintains its current capability.
      break;
    }

    case KeyPathExpr::Component::Kind::OptionalChain:
      didOptionalChain = true;
      break;

    case KeyPathExpr::Component::Kind::OptionalForce:
      // Forcing an optional preserves its lvalue-ness.
      break;

    case KeyPathExpr::Component::Kind::OptionalWrap:
      // An optional chain should already have been recorded.
      assert(didOptionalChain);
      break;

    case KeyPathExpr::Component::Kind::TupleElement:
      llvm_unreachable("not implemented");
      break;

    case KeyPathExpr::Component::Kind::DictionaryKey:
      llvm_unreachable("DictionaryKey only valid in #keyPath");
      break;
    }
  }

  // Optional chains force the entire key path to be read-only.
  if (didOptionalChain)
    mutability = KeyPathMutability::ReadOnly;

  return success(mutability, isSendable);
}

TypeVarBindingProducer::TypeVarBindingProducer(BindingSet &bindings)
    : BindingProducer(bindings.getConstraintSystem(),
                      bindings.getTypeVariable()->getImpl().getLocator()),
      TypeVar(bindings.getTypeVariable()), CanBeNil(bindings.canBeNil()) {
  if (bindings.isDirectHole()) {
    auto *locator = getLocator();
    // If this type variable is associated with a code completion token
    // and it failed to infer any bindings let's adjust holes's locator
    // to point to a code completion token to avoid attempting to "fix"
    // this problem since its rooted in the fact that constraint system
    // is under-constrained.
    if (bindings.getAssociatedCodeCompletionToken()) {
      locator =
          CS.getConstraintLocator(bindings.getAssociatedCodeCompletionToken());
    }

    Bindings.push_back(Binding::forHole(TypeVar, locator));
    return;
  }

  // A binding to `Any` which should always be considered as a last resort.
  std::optional<Binding> Any;

  auto addBinding = [&](const Binding &binding) {
    // Adjust optionality of existing bindings based on presence of
    // `ExpressibleByNilLiteral` requirement.
    if (requiresOptionalAdjustment(binding)) {
      Bindings.push_back(
          binding.withType(OptionalType::get(binding.BindingType)));
    } else if (binding.BindingType->isAny()) {
      Any.emplace(binding);
    } else {
      Bindings.push_back(binding);
    }
  };

  if (TypeVar->getImpl().isPackExpansion()) {
    SmallVector<Binding> viableBindings;

    // Collect possible contextual types (keep in mind that pack
    // expansion type variable gets bound to its "opened" type
    // regardless). To be viable the binding has to come from `bind`
    // or `equal` constraint (i.e. same-type constraint or explicit
    // generic argument) and be fully resolved.
    llvm::copy_if(bindings.Bindings, std::back_inserter(viableBindings),
                  [&](const Binding &binding) {
                    auto *source = binding.getSource();
                    if (source->getKind() == ConstraintKind::Bind ||
                        source->getKind() == ConstraintKind::Equal) {
                      auto type = binding.BindingType;
                      return type->is<PackExpansionType>() &&
                             !type->hasTypeVariable();
                    }
                    return false;
                  });

    // If there is a single fully resolved contextual type, let's
    // use it as a binding to help with performance and diagnostics.
    if (viableBindings.size() == 1) {
      addBinding(viableBindings.front());
    } else {
      for (const auto &entry : bindings.Defaults) {
        auto *constraint = entry.second;
        Bindings.push_back(getDefaultBinding(constraint));
      }
    }

    return;
  }

  for (const auto &binding : bindings.Bindings) {
    addBinding(binding);
  }

  // Infer defaults based on "uncovered" literal protocol requirements.
  for (const auto &info : bindings.Literals) {
    const auto &literal = info.second;

    if (!literal.viableAsBinding())
      continue;

    // We need to figure out whether this is a direct conformance
    // requirement or inferred transitive one to identify binding
    // kind correctly.
    addBinding({literal.getDefaultType(),
                literal.isDirectRequirement() ? BindingKind::Subtypes
                                              : BindingKind::Supertypes,
                literal.getSource()});
  }

  // Let's always consider `Any` to be a last resort binding because
  // it's always better to infer concrete type and erase it if required
  // by the context.
  if (Any) {
    Bindings.push_back(*Any);
  }

  {
    bool noBindings = Bindings.empty();

    for (const auto &entry : bindings.Defaults) {
      auto *constraint = entry.second;
      if (noBindings) {
        // If there are no direct or transitive bindings to attempt
        // let's add defaults to the list right away.
        Bindings.push_back(getDefaultBinding(constraint));
      } else {
        // Otherwise let's delay attempting default bindings
        // until all of the direct & transitive bindings and
        // their derivatives have been attempted.
        DelayedDefaults.push_back(constraint);
      }
    }
  }
}

bool TypeVarBindingProducer::requiresOptionalAdjustment(
    const Binding &binding) const {
  // If type variable can't be `nil` then adjustment is
  // not required.
  if (!CanBeNil)
    return false;

  if (binding.Kind == BindingKind::Supertypes) {
    auto type = binding.BindingType->getRValueType();
    // If the type doesn't conform to ExpressibleByNilLiteral,
    // produce an optional of that type as a potential binding. We
    // overwrite the binding in place because the non-optional type
    // will fail to type-check against the nil-literal conformance.
    auto *proto = CS.getASTContext().getProtocol(
         KnownProtocolKind::ExpressibleByNilLiteral);

    return !CS.lookupConformance(type, proto);
  } else if (binding.isDefaultableBinding() && binding.BindingType->isAny()) {
    return true;
  }

  return false;
}

PotentialBinding
TypeVarBindingProducer::getDefaultBinding(Constraint *constraint) const {
  assert(constraint->getKind() == ConstraintKind::Defaultable ||
         constraint->getKind() == ConstraintKind::FallbackType);

  auto type = constraint->getSecondType();
  Binding binding{type, BindingKind::Exact, constraint};
  return requiresOptionalAdjustment(binding)
             ? binding.withType(OptionalType::get(type))
             : binding;
}

ValueDecl *constraints::getOverloadChoiceDecl(Constraint *choice) {
  if (choice->getKind() != ConstraintKind::BindOverload)
    return nullptr;
  return choice->getOverloadChoice().getDeclOrNull();
}

bool constraints::isOperatorDisjunction(Constraint *disjunction) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction);

  auto choices = disjunction->getNestedConstraints();
  assert(!choices.empty());

  auto *decl = getOverloadChoiceDecl(choices.front());
  return decl ? decl->isOperator() : false;
}

ASTNode constraints::findAsyncNode(ClosureExpr *closure) {
  auto *body = closure->getBody();
  if (!body)
    return ASTNode();
  return body->findAsyncNode();
}

void constraints::dumpAnchor(ASTNode anchor, SourceManager *SM,
                             raw_ostream &out) {
  if (auto *expr = anchor.dyn_cast<Expr *>()) {
    out << Expr::getKindName(expr->getKind());
    if (SM) {
      out << '@';
      expr->getLoc().print(out, *SM);
    }
  } else if (auto *pattern = anchor.dyn_cast<Pattern *>()) {
    out << Pattern::getKindName(pattern->getKind()) << "Pattern";
    if (SM) {
      out << '@';
      pattern->getLoc().print(out, *SM);
    }
  } else if (auto *decl = anchor.dyn_cast<Decl *>()) {
    if (auto *VD = dyn_cast<ValueDecl>(decl)) {
      VD->dumpRef(out);
    } else {
      out << "<<" << Decl::getKindName(decl->getKind()) << ">>";
      if (SM) {
        out << "@";
        decl->getLoc().print(out, *SM);
      }
    }
  }
  // TODO(diagnostics): Implement the rest of the cases.
}

bool constraints::isResultBuilderMethodReference(ASTContext &ctx,
                                                 UnresolvedDotExpr *UDE) {
  if (!(UDE && UDE->isImplicit()))
    return false;

  SmallVector<Identifier, 5> builderMethods(
      {ctx.Id_buildBlock, ctx.Id_buildExpression, ctx.Id_buildPartialBlock,
       ctx.Id_buildFinalResult, ctx.Id_buildIf});

  return llvm::any_of(builderMethods, [&](const Identifier &methodId) {
    return UDE->getName().compare(DeclNameRef(methodId)) == 0;
  });
}
