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
#include "ConstraintSystem.h"
#include "ConstraintGraph.h"
#include "CSDiagnostics.h"
#include "CSFix.h"
#include "SolutionResult.h"
#include "TypeCheckType.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Format.h"

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "ConstraintSystem"

ExpressionTimer::ExpressionTimer(Expr *E, ConstraintSystem &CS)
  : E(E),
      Context(CS.getASTContext()),
      StartTime(llvm::TimeRecord::getCurrentTime()),
      PrintDebugTiming(CS.getASTContext().TypeCheckerOpts.DebugTimeExpressions),
      PrintWarning(true) {
  if (auto *baseCS = CS.baseCS) {
    // If we already have a timer in the base constraint
    // system, let's seed its start time to the child.
    if (baseCS->Timer) {
      StartTime = baseCS->Timer->startedAt();
      PrintWarning = false;
      PrintDebugTiming = false;
    }
  }
}

ExpressionTimer::~ExpressionTimer() {
  auto elapsed = getElapsedProcessTimeInFractionalSeconds();
  unsigned elapsedMS = static_cast<unsigned>(elapsed * 1000);

  if (PrintDebugTiming) {
    // Round up to the nearest 100th of a millisecond.
    llvm::errs() << llvm::format("%0.2f", ceil(elapsed * 100000) / 100)
                 << "ms\t";
    E->getLoc().print(llvm::errs(), Context.SourceMgr);
    llvm::errs() << "\n";
  }

  if (!PrintWarning)
    return;

  const auto WarnLimit = getWarnLimit();
  if (WarnLimit != 0 && elapsedMS >= WarnLimit && E->getLoc().isValid())
    Context.Diags.diagnose(E->getLoc(), diag::debug_long_expression,
                           elapsedMS, WarnLimit)
      .highlight(E->getSourceRange());
}


ConstraintSystem::ConstraintSystem(DeclContext *dc,
                                   ConstraintSystemOptions options)
  : Context(dc->getASTContext()), DC(dc), Options(options),
    Arena(dc->getASTContext(), Allocator),
    CG(*new ConstraintGraph(*this))
{
  assert(DC && "context required");
}

ConstraintSystem::~ConstraintSystem() {
  delete &CG;
}

void ConstraintSystem::incrementScopeCounter() {
  CountScopes++;
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (getASTContext().Stats)
    getASTContext().Stats->getFrontendCounters().NumConstraintScopes++;
}

void ConstraintSystem::incrementLeafScopes() {
  if (getASTContext().Stats)
    getASTContext().Stats->getFrontendCounters().NumLeafScopes++;
}

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  return llvm::any_of(TypeVariables, [](const TypeVariableType *typeVar) {
    return !typeVar->getImpl().hasRepresentativeOrFixed();
  });
}

void ConstraintSystem::addTypeVariable(TypeVariableType *typeVar) {
  TypeVariables.insert(typeVar);

  // Notify the constraint graph.
  (void)CG[typeVar];
}

void ConstraintSystem::mergeEquivalenceClasses(TypeVariableType *typeVar1,
                                               TypeVariableType *typeVar2,
                                               bool updateWorkList) {
  assert(typeVar1 == getRepresentative(typeVar1) &&
         "typeVar1 is not the representative");
  assert(typeVar2 == getRepresentative(typeVar2) &&
         "typeVar2 is not the representative");
  assert(typeVar1 != typeVar2 && "cannot merge type with itself");
  typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getSavedBindings());

  // Merge nodes in the constraint graph.
  CG.mergeNodes(typeVar1, typeVar2);

  if (updateWorkList) {
    addTypeVariableConstraintsToWorkList(typeVar1);
  }
}

/// Determine whether the given type variables occurs in the given type.
bool ConstraintSystem::typeVarOccursInType(TypeVariableType *typeVar,
                                           Type type,
                                           bool *involvesOtherTypeVariables) {
  SmallVector<TypeVariableType *, 4> typeVars;
  type->getTypeVariables(typeVars);
  bool result = false;
  for (auto referencedTypeVar : typeVars) {
    if (referencedTypeVar == typeVar) {
      result = true;
      if (!involvesOtherTypeVariables || *involvesOtherTypeVariables)
        break;

      continue;
    }

    if (involvesOtherTypeVariables)
      *involvesOtherTypeVariables = true;
  }

  return result;
}

void ConstraintSystem::assignFixedType(TypeVariableType *typeVar, Type type,
                                       bool updateState) {
  assert(!type->hasError() &&
         "Should not be assigning a type involving ErrorType!");

  typeVar->getImpl().assignFixedType(type, getSavedBindings());

  if (!updateState)
    return;

  if (!type->isTypeVariableOrMember()) {
    // If this type variable represents a literal, check whether we picked the
    // default literal type. First, find the corresponding protocol.
    ProtocolDecl *literalProtocol = nullptr;
    // If we have the constraint graph, we can check all type variables in
    // the equivalence class. This is the More Correct path.
    // FIXME: Eliminate the less-correct path.
    auto typeVarRep = getRepresentative(typeVar);
    for (auto tv : CG[typeVarRep].getEquivalenceClass()) {
      auto locator = tv->getImpl().getLocator();
      if (!locator || !locator->getPath().empty())
        continue;

      auto anchor = locator->getAnchor();
      if (!anchor)
        continue;

      literalProtocol =
          TypeChecker::getLiteralProtocol(getASTContext(), anchor);
      if (literalProtocol)
        break;
    }

    // If the protocol has a default type, check it.
    if (literalProtocol) {
      if (auto defaultType = TypeChecker::getDefaultType(literalProtocol, DC)) {
        // Check whether the nominal types match. This makes sure that we
        // properly handle Array vs. Array<T>.
        if (defaultType->getAnyNominal() != type->getAnyNominal())
          increaseScore(SK_NonDefaultLiteral);
      }
    }
  }

  // Notify the constraint graph.
  CG.bindTypeVariable(typeVar, type);
  addTypeVariableConstraintsToWorkList(typeVar);
}

void ConstraintSystem::addTypeVariableConstraintsToWorkList(
       TypeVariableType *typeVar) {
  // Activate the constraints affected by a change to this type variable.
  auto gatheringKind = ConstraintGraph::GatheringKind::AllMentions;
  for (auto *constraint : CG.gatherConstraints(typeVar, gatheringKind))
    if (!constraint->isActive())
      activateConstraint(constraint);
}

/// Retrieve a dynamic result signature for the given declaration.
static std::tuple<char, ObjCSelector, CanType>
getDynamicResultSignature(ValueDecl *decl) {
  if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    // Handle functions.
    auto type = func->getMethodInterfaceType();
    return std::make_tuple(func->isStatic(), func->getObjCSelector(),
                           type->getCanonicalType());
  }

  if (auto asd = dyn_cast<AbstractStorageDecl>(decl)) {
    // Handle properties and subscripts, anchored by the getter's selector.
    return std::make_tuple(asd->isStatic(), asd->getObjCGetterSelector(),
                           asd->getInterfaceType()->getCanonicalType());
  }

  llvm_unreachable("Not a valid @objc member");
}

LookupResult &ConstraintSystem::lookupMember(Type base, DeclNameRef name) {
  // Check whether we've already performed this lookup.
  auto &result = MemberLookups[{base, name}];
  if (result) return *result;

  // Lookup the member.
  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  if (isa<AbstractFunctionDecl>(DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;

  result = TypeChecker::lookupMember(DC, base, name, lookupOptions);

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
    auto &ctx = getASTContext();
    if (uniqueEntry->getAttrs().isUnavailable(ctx) &&
        !decl->getAttrs().isUnavailable(ctx)) {
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

ArrayRef<Type> ConstraintSystem::
getAlternativeLiteralTypes(KnownProtocolKind kind) {
  unsigned index;

  switch (kind) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  case KnownProtocolKind::Id: llvm_unreachable("Not a literal protocol");
#define EXPRESSIBLE_BY_LITERAL_PROTOCOL_WITH_NAME(Id, Name, __, ___)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ExpressibleByArrayLiteral:     index = 0; break;
  case KnownProtocolKind::ExpressibleByDictionaryLiteral:index = 1; break;
  case KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral: index = 2;
    break;
  case KnownProtocolKind::ExpressibleByFloatLiteral: index = 3; break;
  case KnownProtocolKind::ExpressibleByIntegerLiteral: index = 4; break;
  case KnownProtocolKind::ExpressibleByStringInterpolation: index = 5; break;
  case KnownProtocolKind::ExpressibleByStringLiteral: index = 6; break;
  case KnownProtocolKind::ExpressibleByNilLiteral: index = 7; break;
  case KnownProtocolKind::ExpressibleByBooleanLiteral: index = 8; break;
  case KnownProtocolKind::ExpressibleByUnicodeScalarLiteral: index = 9; break;
  case KnownProtocolKind::ExpressibleByColorLiteral: index = 10; break;
  case KnownProtocolKind::ExpressibleByImageLiteral: index = 11; break;
  case KnownProtocolKind::ExpressibleByFileReferenceLiteral: index = 12; break;
  }
  static_assert(NumAlternativeLiteralTypes == 13, "Wrong # of literal types");

  // If we already looked for alternative literal types, return those results.
  if (AlternativeLiteralTypes[index])
    return *AlternativeLiteralTypes[index];

  SmallVector<Type, 4> types;

  // Some literal kinds are related.
  switch (kind) {
#define PROTOCOL_WITH_NAME(Id, Name) \
  case KnownProtocolKind::Id: llvm_unreachable("Not a literal protocol");
#define EXPRESSIBLE_BY_LITERAL_PROTOCOL_WITH_NAME(Id, Name, __, ___)
#include "swift/AST/KnownProtocols.def"

  case KnownProtocolKind::ExpressibleByArrayLiteral:
  case KnownProtocolKind::ExpressibleByDictionaryLiteral:
    break;

  case KnownProtocolKind::ExpressibleByExtendedGraphemeClusterLiteral:
  case KnownProtocolKind::ExpressibleByStringInterpolation:
  case KnownProtocolKind::ExpressibleByStringLiteral:
  case KnownProtocolKind::ExpressibleByUnicodeScalarLiteral:
    break;

  case KnownProtocolKind::ExpressibleByIntegerLiteral:
    // Integer literals can be treated as floating point literals.
    if (auto floatProto = getASTContext().getProtocol(
                            KnownProtocolKind::ExpressibleByFloatLiteral)) {
      if (auto defaultType = TypeChecker::getDefaultType(floatProto, DC)) {
        types.push_back(defaultType);
      }
    }
    break;

  case KnownProtocolKind::ExpressibleByFloatLiteral:
    break;

  case KnownProtocolKind::ExpressibleByNilLiteral:
  case KnownProtocolKind::ExpressibleByBooleanLiteral:
    break;
  case KnownProtocolKind::ExpressibleByColorLiteral:
  case KnownProtocolKind::ExpressibleByImageLiteral:
  case KnownProtocolKind::ExpressibleByFileReferenceLiteral:
    break;
  }

  AlternativeLiteralTypes[index] = allocateCopy(types);
  return *AlternativeLiteralTypes[index];
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
    Expr *anchor, ArrayRef<ConstraintLocator::PathElement> path) {
  auto summaryFlags = ConstraintLocator::getSummaryFlagsForPath(path);
  return getConstraintLocator(anchor, path, summaryFlags);
}

ConstraintLocator *ConstraintSystem::getConstraintLocator(
                     Expr *anchor,
                     ArrayRef<ConstraintLocator::PathElement> path,
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
  Expr *anchor = builder.getLocatorParts(path);
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
  auto *anchor = builder.getLocatorParts(newPath);
  newPath.append(newElts.begin(), newElts.end());
  return getConstraintLocator(anchor, newPath);
}

ConstraintLocator *
ConstraintSystem::getCalleeLocator(ConstraintLocator *locator,
                                   bool lookThroughApply) {
  auto *anchor = locator->getAnchor();
  assert(anchor && "Expected an anchor!");

  auto path = locator->getPath();
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

  // If we have a locator that starts with a key path component element, we
  // may have a callee given by a property or subscript component.
  if (auto componentElt =
          locator->getFirstElementAs<LocatorPathElt::KeyPathComponent>()) {
    auto *kpExpr = cast<KeyPathExpr>(anchor);
    auto component = kpExpr->getComponents()[componentElt->getIndex()];

    using ComponentKind = KeyPathExpr::Component::Kind;
    switch (component.getKind()) {
    case ComponentKind::UnresolvedSubscript:
    case ComponentKind::Subscript:
      // For a subscript the callee is given by 'component -> subscript member'.
      return getConstraintLocator(
          anchor, {*componentElt, ConstraintLocator::SubscriptMember});
    case ComponentKind::UnresolvedProperty:
    case ComponentKind::Property:
      // For a property, the choice is just given by the component.
      return getConstraintLocator(anchor, *componentElt);
    case ComponentKind::TupleElement:
      llvm_unreachable("Not implemented by CSGen");
      break;
    case ComponentKind::Invalid:
    case ComponentKind::OptionalForce:
    case ComponentKind::OptionalChain:
    case ComponentKind::OptionalWrap:
    case ComponentKind::Identity:
      // These components don't have any callee associated, so just continue.
      break;
    }
  }

  // Make sure we handle subscripts before looking at apply exprs. We don't
  // want to return a subscript member locator for an expression such as x[](y),
  // as its callee is not the subscript, but rather the function it returns.
  if (isa<SubscriptExpr>(anchor))
    return getConstraintLocator(anchor, ConstraintLocator::SubscriptMember);

  auto getSpecialFnCalleeLoc = [&](Type fnTy) -> ConstraintLocator * {
    // FIXME: We should probably assert that we don't get a type variable
    // here to make sure we only retrieve callee locators for resolved calls,
    // ensuring that callee locators don't change after binding a type.
    // Unfortunately CSDiag currently calls into getCalleeLocator, so all bets
    // are off. Once we remove that legacy diagnostic logic, we should be able
    // to assert here.
    fnTy = getFixedTypeRecursive(fnTy, /*wantRValue*/ true);

    // For an apply of a metatype, we have a short-form constructor. Unlike
    // other locators to callees, these are anchored on the apply expression
    // rather than the function expr.
    if (fnTy->is<AnyMetatypeType>()) {
      return getConstraintLocator(anchor,
                                  {LocatorPathElt::ApplyFunction(),
                                   LocatorPathElt::ConstructorMember()});
    }

    // Handle an apply of a nominal type which supports callAsFunction.
    if (fnTy->isCallableNominalType(DC)) {
      return getConstraintLocator(anchor,
                                  {LocatorPathElt::ApplyFunction(),
                                   LocatorPathElt::ImplicitCallAsFunction()});
    }
    return nullptr;
  };

  if (lookThroughApply) {
    if (auto *applyExpr = dyn_cast<ApplyExpr>(anchor)) {
      auto *fnExpr = applyExpr->getFn();

      // Handle special cases for applies of non-function types.
      if (auto *loc = getSpecialFnCalleeLoc(getType(fnExpr)))
        return loc;

      // Otherwise fall through and look for locators anchored on the function
      // expr. For CallExprs, this can look through things like parens and
      // optional chaining.
      if (auto *callExpr = dyn_cast<CallExpr>(anchor)) {
        anchor = callExpr->getDirectCallee();
      } else {
        anchor = fnExpr;
      }
    }
  }

  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
    return getConstraintLocator(
        anchor, TypeChecker::getSelfForInitDelegationInConstructor(DC, UDE)
                    ? ConstraintLocator::ConstructorMember
                    : ConstraintLocator::Member);
  }

  if (auto *UME = dyn_cast<UnresolvedMemberExpr>(anchor)) {
    auto *calleeLoc =
        getConstraintLocator(UME, ConstraintLocator::UnresolvedMember);

    // Handle special cases for applies of non-function types.
    // FIXME: Consider re-designing the AST such that an unresolved member expr
    // with arguments uses a CallExpr, which would make this logic unnecessary
    // and clean up a bunch of other special cases. Doing so may require a bit
    // of hacking in CSGen though.
    if (UME->hasArguments()) {
      if (auto overload = findSelectedOverloadFor(calleeLoc)) {
        if (auto *loc = getSpecialFnCalleeLoc(overload->boundType))
          return loc;
      }
    }
    return calleeLoc;
  }

  if (isa<MemberRefExpr>(anchor))
    return getConstraintLocator(anchor, ConstraintLocator::Member);

  return getConstraintLocator(anchor);
}

/// Extend the given depth map by adding depths for all of the subexpressions
/// of the given expression.
static void extendDepthMap(
   Expr *expr,
   llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &depthMap) {
  class RecordingTraversal : public ASTWalker {
  public:
    llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &DepthMap;
    unsigned Depth = 0;

    explicit RecordingTraversal(
        llvm::DenseMap<Expr *, std::pair<unsigned, Expr *>> &depthMap)
        : DepthMap(depthMap) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      DepthMap[E] = {Depth, Parent.getAsExpr()};
      Depth++;
      return { true, E };
    }

    Expr *walkToExprPost(Expr *E) override {
      Depth--;
      return E;
    }
  };

  RecordingTraversal traversal(depthMap);
  expr->walk(traversal);
}

Optional<std::pair<unsigned, Expr *>> ConstraintSystem::getExprDepthAndParent(
    Expr *expr) {
  // Check whether the parent has this information.
  if (baseCS && baseCS != this) {
    if (auto known = baseCS->getExprDepthAndParent(expr))
      return *known;
  }

  // Bring the set of expression weights up to date.
  while (NumInputExprsInWeights < InputExprs.size()) {
    extendDepthMap(InputExprs[NumInputExprsInWeights], ExprWeights);
    ++NumInputExprsInWeights;
  }

  auto e = ExprWeights.find(expr);
  if (e != ExprWeights.end())
    return e->second;

  return None;
}

Type ConstraintSystem::openUnboundGenericType(UnboundGenericType *unbound,
                                              ConstraintLocatorBuilder locator,
                                              OpenedTypeMap &replacements) {
  auto unboundDecl = unbound->getDecl();
  auto parentTy = unbound->getParent();
  if (parentTy) {
    parentTy = openUnboundGenericType(parentTy, locator);
    unbound = UnboundGenericType::get(unboundDecl, parentTy,
                                      getASTContext());
  }

  // Open up the generic type.
  openGeneric(unboundDecl->getDeclContext(), unboundDecl->getGenericSignature(),
              locator, replacements);

  if (parentTy) {
    auto subs = parentTy->getContextSubstitutions(
      unboundDecl->getDeclContext());
    for (auto pair : subs) {
      auto found = replacements.find(
        cast<GenericTypeParamType>(pair.first));
      assert(found != replacements.end() &&
             "Missing generic parameter?");
      addConstraint(ConstraintKind::Bind, found->second, pair.second,
                    locator);
    }
  }

  // Map the generic parameters to their corresponding type variables.
  llvm::SmallVector<Type, 2> arguments;
  for (auto gp : unboundDecl->getInnermostGenericParamTypes()) {
    auto found = replacements.find(
      cast<GenericTypeParamType>(gp->getCanonicalType()));
    assert(found != replacements.end() &&
           "Missing generic parameter?");
    arguments.push_back(found->second);
  }

  // FIXME: For some reason we can end up with unbound->getDecl()
  // pointing at a generic TypeAliasDecl here. If we find a way to
  // handle generic TypeAliases elsewhere, this can just become a
  // call to BoundGenericType::get().
  return TypeChecker::applyUnboundGenericArguments(
      unbound, unboundDecl,
      SourceLoc(), TypeResolution::forContextual(DC), arguments);
}

static void checkNestedTypeConstraints(ConstraintSystem &cs, Type type,
                                       ConstraintLocatorBuilder locator) {
  // If this is a type defined inside of constrainted extension, let's add all
  // of the generic requirements to the constraint system to make sure that it's
  // something we can use.
  GenericTypeDecl *decl = nullptr;
  Type parentTy;
  SubstitutionMap subMap;

  if (auto *NAT = dyn_cast<TypeAliasType>(type.getPointer())) {
    decl = NAT->getDecl();
    parentTy = NAT->getParent();
    subMap = NAT->getSubstitutionMap();
  } else if (auto *AGT = type->getAs<AnyGenericType>()) {
    decl = AGT->getDecl();
    parentTy = AGT->getParent();
    // the context substitution map is fine here, since we can't be adding more
    // info than that, unlike a typealias
  }

  if (!parentTy)
    return;

  // If this decl is generic, the constraints are handled when the generic
  // parameters are applied, so we don't have to handle them here (which makes
  // getting the right substitution maps easier).
  if (!decl || decl->isGeneric())
    return;

  // struct A<T> {
  //   let foo: [T]
  // }
  //
  // extension A : Codable where T: Codable {
  //   enum CodingKeys: String, CodingKey {
  //     case foo = "foo"
  //   }
  // }
  //
  // Reference to `A.CodingKeys.foo` would point to `A` as an
  // unbound generic type. Conditional requirements would be
  // added when `A` is "opened". Les delay this check until then.
  if (parentTy->hasUnboundGenericType())
    return;

  auto extension = dyn_cast<ExtensionDecl>(decl->getDeclContext());
  if (extension && extension->isConstrainedExtension()) {
    auto contextSubMap = parentTy->getContextSubstitutionMap(
        extension->getParentModule(), extension->getSelfNominalTypeDecl());
    if (!subMap) {
      // The substitution map wasn't set above, meaning we should grab the map
      // for the extension itself.
      subMap = parentTy->getContextSubstitutionMap(extension->getParentModule(),
                                                   extension);
    }

    if (auto signature = decl->getGenericSignature()) {
      cs.openGenericRequirements(
          extension, signature, /*skipProtocolSelfConstraint*/ true, locator,
          [&](Type type) {
            // Why do we look in two substitution maps? We have to use the
            // context substitution map to find types, because we need to
            // avoid thinking about them when handling the constraints, or all
            // the requirements in the signature become tautologies (if the
            // extension has 'T == Int', subMap will map T -> Int, so the
            // requirement becomes Int == Int no matter what the actual types
            // are here). However, we need the conformances for the extension
            // because the requirements might look like `T: P, T.U: Q`, where
            // U is an associated type of protocol P.
            return type.subst(QuerySubstitutionMap{contextSubMap},
                              LookUpConformanceInSubstitutionMap(subMap));
          });
    }
  }

  // And now make sure sure the parent is okay, for things like X<T>.Y.Z.
  checkNestedTypeConstraints(cs, parentTy, locator);
}

Type ConstraintSystem::openUnboundGenericType(
    Type type, ConstraintLocatorBuilder locator) {
  assert(!type->getCanonicalType()->hasTypeParameter());

  if (!type->hasUnboundGenericType())
    return type;

  type = type.transform([&](Type type) -> Type {
      if (auto unbound = type->getAs<UnboundGenericType>()) {
        OpenedTypeMap replacements;
        return openUnboundGenericType(unbound, locator, replacements);
      }

      return type;
    });

  if (!type)
    return ErrorType::get(getASTContext());

  return type;
}

Type ConstraintSystem::openType(Type type, OpenedTypeMap &replacements) {
  assert(!type->hasUnboundGenericType());

  if (!type->hasTypeParameter())
    return type;

  return type.transform([&](Type type) -> Type {
      assert(!type->is<GenericFunctionType>());

      // Replace a generic type parameter with its corresponding type variable.
      if (auto genericParam = type->getAs<GenericTypeParamType>()) {
        auto known = replacements.find(
          cast<GenericTypeParamType>(genericParam->getCanonicalType()));
        // FIXME: This should be an assert, however protocol generic signatures
        // drop outer generic parameters.
        // assert(known != replacements.end());
        if (known == replacements.end())
          return ErrorType::get(getASTContext());
        return known->second;
      }

      return type;
    });
}

FunctionType *ConstraintSystem::openFunctionType(
       AnyFunctionType *funcType,
       ConstraintLocatorBuilder locator,
       OpenedTypeMap &replacements,
       DeclContext *outerDC) {
  if (auto *genericFn = funcType->getAs<GenericFunctionType>()) {
    auto signature = genericFn->getGenericSignature();

    openGenericParameters(outerDC, signature, replacements, locator);

    openGenericRequirements(
        outerDC, signature, /*skipProtocolSelfConstraint=*/false, locator,
        [&](Type type) -> Type { return openType(type, replacements); });

    funcType = genericFn->substGenericArgs(
        [&](Type type) { return openType(type, replacements); });
  }

  return funcType->castTo<FunctionType>();
}

Optional<Type> ConstraintSystem::isArrayType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() == type->getASTContext().getArrayDecl())
      return boundStruct->getGenericArgs()[0];
  }

  return None;
}

Optional<std::pair<Type, Type>> ConstraintSystem::isDictionaryType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() == type->getASTContext().getDictionaryDecl()) {
      auto genericArgs = boundStruct->getGenericArgs();
      return std::make_pair(genericArgs[0], genericArgs[1]);
    }
  }

  return None;
}

Optional<Type> ConstraintSystem::isSetType(Type type) {
  if (auto boundStruct = type->getAs<BoundGenericStructType>()) {
    if (boundStruct->getDecl() == type->getASTContext().getSetDecl())
      return boundStruct->getGenericArgs()[0];
  }

  return None;
}

bool ConstraintSystem::isCollectionType(Type type) {
  if (auto *structType = type->getAs<BoundGenericStructType>()) {
    auto &ctx = type->getASTContext();
    auto *decl = structType->getDecl();
    if (decl == ctx.getArrayDecl() || decl == ctx.getDictionaryDecl() ||
        decl == ctx.getSetDecl())
      return true;
  }

  return false;
}

bool ConstraintSystem::isAnyHashableType(Type type) {
  if (auto st = type->getAs<StructType>()) {
    auto &ctx = type->getASTContext();
    return st->getDecl() == ctx.getAnyHashableDecl();
  }

  return false;
}

Type ConstraintSystem::getFixedTypeRecursive(Type type,
                                             TypeMatchOptions &flags,
                                             bool wantRValue) const {

  if (wantRValue)
    type = type->getRValueType();

  if (auto depMemType = type->getAs<DependentMemberType>()) {
    if (!depMemType->getBase()->isTypeVariableOrMember()) return type;

    // FIXME: Perform a more limited simplification?
    Type newType = simplifyType(type);
    if (newType.getPointer() == type.getPointer()) return type;

    // Once we've simplified a dependent member type, we need to generate a
    // new constraint.
    flags |= TMF_GenerateConstraints;

    return getFixedTypeRecursive(newType, flags, wantRValue);
  }

  if (auto typeVar = type->getAs<TypeVariableType>()) {
    if (auto fixed = getFixedType(typeVar))
      return getFixedTypeRecursive(fixed, flags, wantRValue);

    return getRepresentative(typeVar);
  }

  return type;
}

/// Does a var or subscript produce an l-value?
///
/// \param baseType - the type of the base on which this object
///   is being accessed; must be null if and only if this is not
///   a type member
static bool doesStorageProduceLValue(AbstractStorageDecl *storage,
                                     Type baseType, DeclContext *useDC,
                                     const DeclRefExpr *base = nullptr) {
  // Unsettable storage decls always produce rvalues.
  if (!storage->isSettable(useDC, base))
    return false;

  if (!storage->isSetterAccessibleFrom(useDC))
    return false;

  // If there is no base, or if the base isn't being used, it is settable.
  // This is only possible for vars.
  if (auto var = dyn_cast<VarDecl>(storage)) {
    if (!baseType || var->isStatic())
      return true;
  }

  // If the base is an lvalue, then a reference produces an lvalue.
  if (baseType->is<LValueType>())
    return true;

  // Stored properties of reference types produce lvalues.
  if (baseType->hasReferenceSemantics() && storage->hasStorage())
    return true;

  // So the base is an rvalue type. The only way an accessor can
  // produce an lvalue is if we have a property where both the
  // getter and setter are nonmutating.
  return !storage->hasStorage() &&
      !storage->isGetterMutating() &&
      !storage->isSetterMutating();
}

Type ConstraintSystem::getUnopenedTypeOfReference(VarDecl *value, Type baseType,
                                                  DeclContext *UseDC,
                                                  const DeclRefExpr *base,
                                                  bool wantInterfaceType) {
  return TypeChecker::getUnopenedTypeOfReference(
      value, baseType, UseDC,
      [&](VarDecl *var) -> Type {
        if (auto *param = dyn_cast<ParamDecl>(var))
          return getType(param);

        if (!var->hasInterfaceType()) {
          return ErrorType::get(getASTContext());
        }

        return wantInterfaceType ? var->getInterfaceType() : var->getType();
      },
      base, wantInterfaceType);
}

Type TypeChecker::getUnopenedTypeOfReference(
    VarDecl *value, Type baseType, DeclContext *UseDC,
    llvm::function_ref<Type(VarDecl *)> getType, const DeclRefExpr *base,
    bool wantInterfaceType) {
  Type requestedType =
      getType(value)->getWithoutSpecifierType()->getReferenceStorageReferent();

  // If we're dealing with contextual types, and we referenced this type from
  // a different context, map the type.
  if (!wantInterfaceType && requestedType->hasArchetype()) {
    auto valueDC = value->getDeclContext();
    if (valueDC != UseDC) {
      Type mapped = requestedType->mapTypeOutOfContext();
      requestedType = UseDC->mapTypeIntoContext(mapped);
    }
  }

  // Qualify storage declarations with an lvalue when appropriate.
  // Otherwise, they yield rvalues (and the access must be a load).
  if (doesStorageProduceLValue(value, baseType, UseDC, base) &&
      !requestedType->hasError()) {
    return LValueType::get(requestedType);
  }

  return requestedType;
}

void ConstraintSystem::recordOpenedTypes(
       ConstraintLocatorBuilder locator,
       const OpenedTypeMap &replacements) {
  if (replacements.empty())
    return;

  // If the last path element is an archetype or associated type, ignore it.
  SmallVector<LocatorPathElt, 2> pathElts;
  Expr *anchor = locator.getLocatorParts(pathElts);
  if (!pathElts.empty() &&
      pathElts.back().getKind() == ConstraintLocator::GenericParameter)
    return;

  // If the locator is empty, ignore it.
  if (!anchor && pathElts.empty())
    return;

  ConstraintLocator *locatorPtr = getConstraintLocator(locator);
  assert(locatorPtr && "No locator for opened types?");
#if false
  assert(std::find_if(OpenedTypes.begin(), OpenedTypes.end(),
                      [&](const std::pair<ConstraintLocator *,
                          ArrayRef<OpenedType>> &entry) {
                        return entry.first == locatorPtr;
                      }) == OpenedTypes.end() &&
         "already registered opened types for this locator");
#endif

  OpenedType* openedTypes
    = Allocator.Allocate<OpenedType>(replacements.size());
  std::copy(replacements.begin(), replacements.end(), openedTypes);
  OpenedTypes.push_back({ locatorPtr,
    llvm::makeArrayRef(openedTypes,
                       replacements.size()) });
}

/// Determine how many levels of argument labels should be removed from the
/// function type when referencing the given declaration.
static unsigned getNumRemovedArgumentLabels(ValueDecl *decl,
                                            bool isCurriedInstanceReference,
                                            FunctionRefKind functionRefKind) {
  unsigned numParameterLists = decl->getNumCurryLevels();
  switch (functionRefKind) {
  case FunctionRefKind::Unapplied:
  case FunctionRefKind::Compound:
    // Always remove argument labels from unapplied references and references
    // that use a compound name.
    return numParameterLists;

  case FunctionRefKind::SingleApply:
    // If we have fewer than two parameter lists, leave the labels.
    if (numParameterLists < 2)
      return 0;

    // If this is a curried reference to an instance method, where 'self' is
    // being applied, e.g., "ClassName.instanceMethod(self)", remove the
    // argument labels from the resulting function type. The 'self' parameter is
    // always unlabeled, so this operation is a no-op for the actual application.
    return isCurriedInstanceReference ? numParameterLists : 1;

  case FunctionRefKind::DoubleApply:
    // Never remove argument labels from a double application.
    return 0;
  }

  llvm_unreachable("Unhandled FunctionRefKind in switch.");
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfReference(ValueDecl *value,
                                     FunctionRefKind functionRefKind,
                                     ConstraintLocatorBuilder locator,
                                     DeclContext *useDC) {
  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    OpenedTypeMap replacements;

    auto openedType =
        openFunctionType(func->getInterfaceType()->castTo<AnyFunctionType>(),
                         locator, replacements, func->getDeclContext());

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    // If this is a method whose result type is dynamic Self, replace
    // DynamicSelf with the actual object type.
    if (func->getResultInterfaceType()->hasDynamicSelfType()) {
      auto params = openedType->getParams();
      assert(params.size() == 1);
      Type selfTy = params.front().getPlainType()->getMetatypeInstanceType();
      openedType = openedType->replaceCovariantResultType(selfTy, 2)
                        ->castTo<FunctionType>();
    }

    // The reference implicitly binds 'self'.
    return {openedType, openedType->getResult()};
  }

  // Unqualified reference to a local or global function.
  if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(value)) {
    OpenedTypeMap replacements;

    auto funcType = funcDecl->getInterfaceType()->castTo<AnyFunctionType>();
    auto numLabelsToRemove = getNumRemovedArgumentLabels(
        funcDecl, /*isCurriedInstanceReference=*/false, functionRefKind);

    auto openedType = openFunctionType(funcType, locator, replacements,
                                       funcDecl->getDeclContext())
                          ->removeArgumentLabels(numLabelsToRemove);

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    return { openedType, openedType };
  }

  // Unqualified reference to a type.
  if (auto typeDecl = dyn_cast<TypeDecl>(value)) {
    // Resolve the reference to this type declaration in our current context.
    auto type = TypeChecker::resolveTypeInContext(
                                      typeDecl, nullptr,
                                      TypeResolution::forContextual(useDC),
                                      TypeResolverContext::InExpression,
                                      /*isSpecialized=*/false);

    checkNestedTypeConstraints(*this, type, locator);

    // Open the type.
    type = openUnboundGenericType(type, locator);

    // Module types are not wrapped in metatypes.
    if (type->is<ModuleType>())
      return { type, type };

    // If it's a value reference, refer to the metatype.
    type = MetatypeType::get(type);
    return { type, type };
  }

  // Only remaining case: unqualified reference to a property.
  auto *varDecl = cast<VarDecl>(value);

  // Determine the type of the value, opening up that type if necessary.
  bool wantInterfaceType = !varDecl->getDeclContext()->isLocalContext();
  Type valueType =
      getUnopenedTypeOfReference(varDecl, Type(), useDC, /*base=*/nullptr,
                                 wantInterfaceType);

  assert(!valueType->hasUnboundGenericType() &&
         !valueType->hasTypeParameter());
  return { valueType, valueType };
}

/// Bind type variables for archetypes that are determined from
/// context.
///
/// For example, if we are opening a generic function type
/// nested inside another function, we must bind the outer
/// generic parameters to context archetypes, because the
/// nested function can "capture" these outer generic parameters.
///
/// Another case where this comes up is if a generic type is
/// nested inside a function. We don't support codegen for this
/// yet, but again we need to bind any outer generic parameters
/// to context archetypes, because they're not free.
///
/// A final case we have to handle, even though it is invalid, is
/// when a type is nested inside another protocol. We bind the
/// protocol type variable for the protocol Self to an unresolved
/// type, since it will conform to anything. This of course makes
/// no sense, but we can't leave the type variable dangling,
/// because then we crash later.
///
/// If we ever do want to allow nominal types to be nested inside
/// protocols, the key is to set their declared type to a
/// NominalType whose parent is the 'Self' generic parameter, and
/// not the ProtocolType. Then, within a conforming type context,
/// we can 'reparent' the NominalType to that concrete type, and
/// resolve references to associated types inside that NominalType
/// relative to this concrete 'Self' type.
///
/// Also, of course IRGen would have to know to store the 'Self'
/// metadata as an extra hidden generic parameter in the metadata
/// of such a type, etc.
static void bindArchetypesFromContext(
    ConstraintSystem &cs,
    DeclContext *outerDC,
    ConstraintLocator *locatorPtr,
    const OpenedTypeMap &replacements) {

  auto bindPrimaryArchetype = [&](Type paramTy, Type contextTy) {
    auto found = replacements.find(cast<GenericTypeParamType>(
                                     paramTy->getCanonicalType()));

    // We might not have a type variable for this generic parameter
    // because either we're opening up an UnboundGenericType,
    // in which case we only want to infer the innermost generic
    // parameters, or because this generic parameter was constrained
    // away into a concrete type.
    if (found != replacements.end()) {
      auto typeVar = found->second;
      cs.addConstraint(ConstraintKind::Bind, typeVar, contextTy,
                       locatorPtr);
    }
  };

  // Find the innermost non-type context.
  for (const auto *parentDC = outerDC;
       !parentDC->isModuleScopeContext();
       parentDC = parentDC->getParent()) {
    if (parentDC->isTypeContext()) {
      if (parentDC != outerDC && parentDC->getSelfProtocolDecl()) {
        auto selfTy = parentDC->getSelfInterfaceType();
        auto contextTy = cs.getASTContext().TheUnresolvedType;
        bindPrimaryArchetype(selfTy, contextTy);
      }
      continue;
    }

    // If it's not generic, there's nothing to do.
    auto genericSig = parentDC->getGenericSignatureOfContext();
    if (!genericSig)
      break;

    for (auto *paramTy : genericSig->getGenericParams()) {
      Type contextTy = cs.DC->mapTypeIntoContext(paramTy);
      bindPrimaryArchetype(paramTy, contextTy);
    }

    break;
  }
}

void ConstraintSystem::openGeneric(
       DeclContext *outerDC,
       GenericSignature sig,
       ConstraintLocatorBuilder locator,
       OpenedTypeMap &replacements) {
  if (!sig)
    return;

  openGenericParameters(outerDC, sig, replacements, locator);

  // Add the requirements as constraints.
  openGenericRequirements(
      outerDC, sig, /*skipProtocolSelfConstraint=*/false, locator,
      [&](Type type) { return openType(type, replacements); });
}

void ConstraintSystem::openGenericParameters(DeclContext *outerDC,
                                             GenericSignature sig,
                                             OpenedTypeMap &replacements,
                                             ConstraintLocatorBuilder locator) {
  assert(sig);

  // Create the type variables for the generic parameters.
  for (auto gp : sig->getGenericParams()) {
    auto *paramLocator = getConstraintLocator(
        locator.withPathElement(LocatorPathElt::GenericParameter(gp)));

    auto typeVar = createTypeVariable(paramLocator, TVO_PrefersSubtypeBinding |
                                                    TVO_CanBindToHole);
    auto result = replacements.insert(std::make_pair(
        cast<GenericTypeParamType>(gp->getCanonicalType()), typeVar));

    assert(result.second);
    (void)result;
  }

  auto *baseLocator = getConstraintLocator(
      locator.withPathElement(LocatorPathElt::OpenedGeneric(sig)));

  bindArchetypesFromContext(*this, outerDC, baseLocator, replacements);
}

void ConstraintSystem::openGenericRequirements(
    DeclContext *outerDC, GenericSignature signature,
    bool skipProtocolSelfConstraint, ConstraintLocatorBuilder locator,
    llvm::function_ref<Type(Type)> substFn) {
  auto requirements = signature->getRequirements();
  for (unsigned pos = 0, n = requirements.size(); pos != n; ++pos) {
    const auto &req = requirements[pos];

    Optional<Requirement> openedReq;
    auto openedFirst = substFn(req.getFirstType());

    auto kind = req.getKind();
    switch (kind) {
    case RequirementKind::Conformance: {
      auto proto = req.getSecondType()->castTo<ProtocolType>();
      auto protoDecl = proto->getDecl();
      // Determine whether this is the protocol 'Self' constraint we should
      // skip.
      if (skipProtocolSelfConstraint && protoDecl == outerDC &&
          protoDecl->getSelfInterfaceType()->isEqual(req.getFirstType()))
        continue;
      openedReq = Requirement(kind, openedFirst, proto);
      break;
    }
    case RequirementKind::Superclass:
    case RequirementKind::SameType:
      openedReq = Requirement(kind, openedFirst, substFn(req.getSecondType()));
      break;
    case RequirementKind::Layout:
      openedReq = Requirement(kind, openedFirst, req.getLayoutConstraint());
      break;
    }

    auto openedGenericLoc =
        locator.withPathElement(LocatorPathElt::OpenedGeneric(signature));
    addConstraint(*openedReq,
                  openedGenericLoc.withPathElement(
                      LocatorPathElt::TypeParameterRequirement(pos, kind)));
  }
}

/// Add the constraint on the type used for the 'Self' type for a member
/// reference.
///
/// \param cs The constraint system.
///
/// \param objectTy The type of the object that we're using to access the
/// member.
///
/// \param selfTy The instance type of the context in which the member is
/// declared.
static void addSelfConstraint(ConstraintSystem &cs, Type objectTy, Type selfTy,
                              ConstraintLocatorBuilder locator){
  assert(!selfTy->is<ProtocolType>());

  // Otherwise, use a subtype constraint for classes to cope with inheritance.
  if (selfTy->getClassOrBoundGenericClass()) {
    cs.addConstraint(ConstraintKind::Subtype, objectTy, selfTy,
                     cs.getConstraintLocator(locator));
    return;
  }

  // Otherwise, the types must be equivalent.
  cs.addConstraint(ConstraintKind::Bind, objectTy, selfTy,
                   cs.getConstraintLocator(locator));
}

/// Determine whether the given locator is for a witness or requirement.
static bool isRequirementOrWitness(const ConstraintLocatorBuilder &locator) {
  if (auto last = locator.last()) {
    return last->getKind() == ConstraintLocator::ProtocolRequirement ||
           last->getKind() == ConstraintLocator::Witness;
  }

  return false;
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReference(
    Type baseTy, ValueDecl *value, DeclContext *useDC,
    bool isDynamicResult,
    FunctionRefKind functionRefKind,
    ConstraintLocatorBuilder locator,
    const DeclRefExpr *base,
    OpenedTypeMap *replacementsPtr) {
  // Figure out the instance type used for the base.
  Type baseObjTy = getFixedTypeRecursive(baseTy, /*wantRValue=*/true);

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReference(value, functionRefKind, locator, useDC);
  }

  // Check to see if the self parameter is applied, in which case we'll want to
  // strip it off later.
  auto hasAppliedSelf = doesMemberRefApplyCurriedSelf(baseObjTy, value);

  baseObjTy = baseObjTy->getMetatypeInstanceType();
  FunctionType::Param baseObjParam(baseObjTy);

  if (auto *typeDecl = dyn_cast<TypeDecl>(value)) {
    assert(!isa<ModuleDecl>(typeDecl) && "Nested module?");

    auto memberTy = TypeChecker::substMemberTypeWithBase(DC->getParentModule(),
                                                         typeDecl, baseObjTy);

    checkNestedTypeConstraints(*this, memberTy, locator);

    // Open the type if it was a reference to a generic type.
    memberTy = openUnboundGenericType(memberTy, locator);

    // Wrap it in a metatype.
    memberTy = MetatypeType::get(memberTy);

    auto openedType = FunctionType::get({baseObjParam}, memberTy);
    return { openedType, memberTy };
  }

  // Figure out the declaration context to use when opening this type.
  DeclContext *innerDC = value->getInnermostDeclContext();
  DeclContext *outerDC = value->getDeclContext();

  // Open the type of the generic function or member of a generic type.
  Type openedType;
  OpenedTypeMap localReplacements;
  auto &replacements = replacementsPtr ? *replacementsPtr : localReplacements;
  unsigned numRemovedArgumentLabels = getNumRemovedArgumentLabels(
      value, /*isCurriedInstanceReference*/ !hasAppliedSelf, functionRefKind);

  AnyFunctionType *funcType;

  if (isa<AbstractFunctionDecl>(value) ||
      isa<EnumElementDecl>(value)) {
    // This is the easy case.
    funcType = value->getInterfaceType()->castTo<AnyFunctionType>();
  } else {
    // For a property, build a type (Self) -> PropType.
    // For a subscript, build a type (Self) -> (Indices...) -> ElementType.
    //
    // If the access is mutating, wrap the storage type in an lvalue type.
    Type refType;
    if (auto *subscript = dyn_cast<SubscriptDecl>(value)) {
      auto elementTy = subscript->getElementInterfaceType();

      if (doesStorageProduceLValue(subscript, baseTy, useDC, base))
        elementTy = LValueType::get(elementTy);

      // See ConstraintSystem::resolveOverload() -- optional and dynamic
      // subscripts are a special case, because the optionality is
      // applied to the result type and not the type of the reference.
      if (!isRequirementOrWitness(locator)) {
        if (subscript->getAttrs().hasAttribute<OptionalAttr>() ||
            isDynamicResult)
          elementTy = OptionalType::get(elementTy->getRValueType());
      }

      auto indices = subscript->getInterfaceType()
                              ->castTo<AnyFunctionType>()->getParams();
      refType = FunctionType::get(indices, elementTy);
    } else {
      refType = TypeChecker::getUnopenedTypeOfReference(
          cast<VarDecl>(value), baseTy, useDC, base,
          /*wantInterfaceType=*/true);
    }

    auto selfTy = outerDC->getSelfInterfaceType();

    // If this is a reference to an instance member that applies self,
    // where self is a value type and the base type is an lvalue, wrap it in an
    // inout type.
    auto selfFlags = ParameterTypeFlags();
    if (value->isInstanceMember() && hasAppliedSelf &&
        !outerDC->getDeclaredInterfaceType()->hasReferenceSemantics() &&
        baseTy->is<LValueType>() &&
        !selfTy->hasError())
      selfFlags = selfFlags.withInOut(true);

    // If the storage is generic, add a generic signature.
    FunctionType::Param selfParam(selfTy, Identifier(), selfFlags);
    if (auto sig = innerDC->getGenericSignatureOfContext()) {
      funcType = GenericFunctionType::get(sig, {selfParam}, refType);
    } else {
      funcType = FunctionType::get({selfParam}, refType);
    }
  }

  // While opening member function type, let's delay opening requirements
  // to allow contextual types to affect the situation.
  if (auto *genericFn = funcType->getAs<GenericFunctionType>()) {
    openGenericParameters(outerDC, genericFn->getGenericSignature(),
                          replacements, locator);

    openedType = genericFn->substGenericArgs(
        [&](Type type) { return openType(type, replacements); });
  } else {
    openedType = funcType;
  }

  openedType = openedType->removeArgumentLabels(numRemovedArgumentLabels);

  if (!outerDC->getSelfProtocolDecl()) {
    // Class methods returning Self as well as constructors get the
    // result replaced with the base object type.
    if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
      if (func->hasDynamicSelfResult() &&
          !baseObjTy->getOptionalObjectType()) {
        openedType = openedType->replaceCovariantResultType(baseObjTy, 2);
      }
    } else if (auto *decl = dyn_cast<SubscriptDecl>(value)) {
      if (decl->getElementInterfaceType()->hasDynamicSelfType()) {
        openedType = openedType->replaceCovariantResultType(baseObjTy, 2);
      }
    } else if (auto *decl = dyn_cast<VarDecl>(value)) {
      if (decl->getValueInterfaceType()->hasDynamicSelfType()) {
        openedType = openedType->replaceCovariantResultType(baseObjTy, 1);
      }
    }
  }

  // If we are looking at a member of an existential, open the existential.
  Type baseOpenedTy = baseObjTy;

  if (baseObjTy->isExistentialType()) {
    auto openedArchetype = OpenedArchetypeType::get(baseObjTy);
    OpenedExistentialTypes.push_back({ getConstraintLocator(locator),
                                       openedArchetype });
    baseOpenedTy = openedArchetype;
  }

  // Constrain the 'self' object type.
  auto openedFnType = openedType->castTo<FunctionType>();
  auto openedParams = openedFnType->getParams();
  assert(openedParams.size() == 1);

  Type selfObjTy = openedParams.front().getPlainType()->getMetatypeInstanceType();
  if (outerDC->getSelfProtocolDecl()) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Bind, baseOpenedTy, selfObjTy,
                  getConstraintLocator(locator));
  } else if (!isDynamicResult) {
    addSelfConstraint(*this, baseOpenedTy, selfObjTy, locator);
  }

  // Open generic requirements after self constraint has been
  // applied and contextual types have been propagated. This
  // helps diagnostics because instead of self type conversion
  // failing we'll get a generic requirement constraint failure
  // if mismatch is related to generic parameters which is much
  // easier to diagnose.
  if (auto *genericFn = funcType->getAs<GenericFunctionType>()) {
    openGenericRequirements(
        outerDC, genericFn->getGenericSignature(),
        /*skipProtocolSelfConstraint=*/true, locator,
        [&](Type type) { return openType(type, replacements); });
  }

  // Compute the type of the reference.
  Type type;
  if (hasAppliedSelf) {
    // For a static member referenced through a metatype or an instance
    // member referenced through an instance, strip off the 'self'.
    type = openedFnType->getResult();
  } else {
    // For an unbound instance method reference, replace the 'Self'
    // parameter with the base type.
    openedType = openedFnType->replaceSelfParameterType(baseObjTy);
    type = openedType;
  }

  // When accessing protocol members with an existential base, replace
  // the 'Self' type parameter with the existential type, since formally
  // the access will operate on existentials and not type parameters.
  if (!isDynamicResult &&
      baseObjTy->isExistentialType() &&
      outerDC->getSelfProtocolDecl()) {
    auto selfTy = replacements[
      cast<GenericTypeParamType>(outerDC->getSelfInterfaceType()
                                 ->getCanonicalType())];
    type = type.transform([&](Type t) -> Type {
      if (t->is<TypeVariableType>())
        if (t->isEqual(selfTy))
          return baseObjTy;
      if (auto *metatypeTy = t->getAs<MetatypeType>())
        if (metatypeTy->getInstanceType()->isEqual(selfTy))
          return ExistentialMetatypeType::get(baseObjTy);
      return t;
    });
  }

  // If we opened up any type variables, record the replacements.
  recordOpenedTypes(locator, replacements);

  return { openedType, type };
}

Type ConstraintSystem::getEffectiveOverloadType(const OverloadChoice &overload,
                                                bool allowMembers,
                                                DeclContext *useDC) {
  switch (overload.getKind()) {
  case OverloadChoiceKind::Decl:
    // Declaration choices are handled below.
    break;

  case OverloadChoiceKind::BaseType:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup:
  case OverloadChoiceKind::KeyPathApplication:
  case OverloadChoiceKind::TupleIndex:
    return Type();
  }

  auto decl = overload.getDecl();

  // Ignore type declarations.
  if (isa<TypeDecl>(decl))
    return Type();

  // Declarations returning unwrapped optionals don't have a single effective
  // type.
  if (decl->isImplicitlyUnwrappedOptional())
    return Type();

  // In a pattern binding initializer, all of its bound variables have no
  // effective overload type.
  if (auto *PBI = dyn_cast<PatternBindingInitializer>(useDC)) {
    if (auto *VD = dyn_cast<VarDecl>(decl)) {
      if (PBI->getBinding() == VD->getParentPatternBinding()) {
        return Type();
      }
    }
  }

  // Retrieve the interface type.
  auto type = decl->getInterfaceType();
  if (type->hasError()) {
    return Type();
  }

  // If we have a generic function type, drop the generic signature; we don't
  // need it for this comparison.
  if (auto genericFn = type->getAs<GenericFunctionType>()) {
    type = FunctionType::get(genericFn->getParams(),
                             genericFn->getResult(),
                             genericFn->getExtInfo());
  }

  // If this declaration is within a type context, we might not be able
  // to handle it.
  if (decl->getDeclContext()->isTypeContext()) {
    if (!allowMembers)
      return Type();

    if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
      auto elementTy = subscript->getElementInterfaceType();

      if (doesStorageProduceLValue(subscript, overload.getBaseType(), useDC))
        elementTy = LValueType::get(elementTy);
      else if (elementTy->hasDynamicSelfType()) {
        Type selfType = overload.getBaseType()->getRValueType()
            ->getMetatypeInstanceType()->lookThroughAllOptionalTypes();
        elementTy = elementTy->replaceCovariantResultType(selfType, 0);
      }

      // See ConstraintSystem::resolveOverload() -- optional and dynamic
      // subscripts are a special case, because the optionality is
      // applied to the result type and not the type of the reference.
      if (subscript->getAttrs().hasAttribute<OptionalAttr>())
        elementTy = OptionalType::get(elementTy->getRValueType());

      auto indices = subscript->getInterfaceType()
                       ->castTo<AnyFunctionType>()->getParams();
      type = FunctionType::get(indices, elementTy);
    } else if (auto var = dyn_cast<VarDecl>(decl)) {
      type = var->getValueInterfaceType();
      if (doesStorageProduceLValue(var, overload.getBaseType(), useDC))
        type = LValueType::get(type);
    } else if (isa<AbstractFunctionDecl>(decl) || isa<EnumElementDecl>(decl)) {
      if (decl->isInstanceMember() &&
          (!overload.getBaseType() ||
           !overload.getBaseType()->getAnyNominal()))
        return Type();

      // Cope with 'Self' returns.
      if (!decl->getDeclContext()->getSelfProtocolDecl()) {
        if (isa<AbstractFunctionDecl>(decl) &&
            cast<AbstractFunctionDecl>(decl)->hasDynamicSelfResult()) {
          if (!overload.getBaseType())
            return Type();

          if (!overload.getBaseType()->getOptionalObjectType()) {
            Type selfType = overload.getBaseType()
                                ->getRValueType()
                                ->getMetatypeInstanceType();

            // `Int??(0)` if we look through all optional types for `Self`
            // we'll end up with incorrect type `Int?` for result because
            // the actual result type is `Int??`.
            if (isa<ConstructorDecl>(decl) && selfType->getOptionalObjectType())
              return Type();

            type = type->replaceCovariantResultType(
                selfType->lookThroughAllOptionalTypes(), 2);
          }
        }
      }

      type = type->castTo<FunctionType>()->getResult();
    }
  }

  // Handle "@objc optional" for non-subscripts; subscripts are handled above.
  if (decl->getAttrs().hasAttribute<OptionalAttr>() &&
      !isa<SubscriptDecl>(decl))
    type = OptionalType::get(type->getRValueType());

  return type;
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      DeclContext *useDC,
                                      ConstraintLocator *locator,
                                      Optional<unsigned> favoredIndex) {
  // If there is a single choice, add the bind overload directly.
  if (choices.size() == 1) {
    addBindOverloadConstraint(boundType, choices.front(), locator, useDC);
    return;
  }

  SmallVector<Constraint *, 4> candidates;
  generateConstraints(candidates, boundType, choices, useDC, locator,
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

  addDisjunctionConstraint(choices, locator, ForgetChoice);
}

/// If we're resolving an overload set with a decl that has special type
/// checking semantics, compute the type of the reference.  For now, follow
/// the lead of \c getTypeOfMemberReference and return a pair of
/// the full opened type and the reference's type.
static std::pair<Type, Type> getTypeOfReferenceWithSpecialTypeCheckingSemantics(
    ConstraintSystem &CS, ConstraintLocator *locator,
    DeclTypeCheckingSemantics semantics) {
  switch (semantics) {
  case DeclTypeCheckingSemantics::Normal:
    llvm_unreachable("Decl does not have special type checking semantics!");

  case DeclTypeCheckingSemantics::TypeOf: {
    // Proceed with a "DynamicType" operation. This produces an existential
    // metatype from existentials, or a concrete metatype from non-
    // existentials (as seen from the current abstraction level), which can't
    // be expressed in the type system currently.
    auto input = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    auto output = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape);

    FunctionType::Param inputArg(input,
                                 CS.getASTContext().getIdentifier("of"));

    CS.addConstraint(ConstraintKind::DynamicTypeOf, output, input,
        CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    auto refType = FunctionType::get({inputArg}, output);
    return {refType, refType};
  }
  case DeclTypeCheckingSemantics::WithoutActuallyEscaping: {
    // Proceed with a "WithoutActuallyEscaping" operation. The body closure
    // receives a copy of the argument closure that is temporarily made
    // @escaping.
    auto noescapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    auto escapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    CS.addConstraint(ConstraintKind::EscapableFunctionOf,
         escapeClosure, noescapeClosure,
         CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape);
    FunctionType::Param arg(escapeClosure);
    auto bodyClosure = FunctionType::get(arg, result,
        FunctionType::ExtInfo(FunctionType::Representation::Swift,
                              /*noescape*/ true,
                              /*throws*/ true,
                              DifferentiabilityKind::NonDifferentiable,
                              /*clangFunctionType*/ nullptr));
    FunctionType::Param args[] = {
      FunctionType::Param(noescapeClosure),
      FunctionType::Param(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };

    auto refType = FunctionType::get(args, result,
      FunctionType::ExtInfo(FunctionType::Representation::Swift,
                            /*noescape*/ false,
                            /*throws*/ true,
                            DifferentiabilityKind::NonDifferentiable,
                            /*clangFunctionType*/ nullptr));
    return {refType, refType};
  }
  case DeclTypeCheckingSemantics::OpenExistential: {
    // The body closure receives a freshly-opened archetype constrained by the
    // existential type as its input.
    auto openedTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    auto existentialTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    CS.addConstraint(ConstraintKind::OpenedExistentialOf,
         openedTy, existentialTy,
         CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape);
    FunctionType::Param bodyArgs[] = {FunctionType::Param(openedTy)};
    auto bodyClosure = FunctionType::get(bodyArgs, result,
        FunctionType::ExtInfo(FunctionType::Representation::Swift,
                              /*noescape*/ true,
                              /*throws*/ true,
                              DifferentiabilityKind::NonDifferentiable,
                              /*clangFunctionType*/ nullptr));
    FunctionType::Param args[] = {
      FunctionType::Param(existentialTy),
      FunctionType::Param(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };
    auto refType = FunctionType::get(args, result,
      FunctionType::ExtInfo(FunctionType::Representation::Swift,
                            /*noescape*/ false,
                            /*throws*/ true,
                            DifferentiabilityKind::NonDifferentiable,
                            /*clangFunctionType*/ nullptr));
    return {refType, refType};
  }
  }

  llvm_unreachable("Unhandled DeclTypeCheckingSemantics in switch.");
}

/// \returns true if given declaration is an instance method marked as
/// `mutating`, false otherwise.
bool isMutatingMethod(const ValueDecl *decl) {
  if (!(decl->isInstanceMember() && isa<FuncDecl>(decl)))
    return false;
  return cast<FuncDecl>(decl)->isMutating();
}

static bool shouldCheckForPartialApplication(ConstraintSystem &cs,
                                             const ValueDecl *decl,
                                             ConstraintLocator *locator) {
  auto *anchor = locator->getAnchor();
  if (!(anchor && isa<UnresolvedDotExpr>(anchor)))
    return false;

  // FIXME(diagnostics): This check should be removed together with
  // expression based diagnostics.
  if (cs.isExprBeingDiagnosed(anchor))
    return false;

  // If this is a reference to instance method marked as 'mutating'
  // it should be checked for invalid partial application.
  if (isMutatingMethod(decl))
    return true;

  // Another unsupported partial application is related
  // to constructor delegation via `self.init` or `super.init`.

  if (!isa<ConstructorDecl>(decl))
    return false;

  auto *UDE = cast<UnresolvedDotExpr>(anchor);
  // This is `super.init`
  if (UDE->getBase()->isSuperExpr())
    return true;

  // Or this might be `self.init`.
  if (auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase())) {
    if (auto *baseDecl = DRE->getDecl())
      return baseDecl->getBaseName() == cs.getASTContext().Id_self;
  }

  return false;
}

/// Try to identify and fix failures related to partial function application
/// e.g. partial application of `init` or 'mutating' instance methods.
static std::pair<bool, unsigned>
isInvalidPartialApplication(ConstraintSystem &cs, const ValueDecl *member,
                            ConstraintLocator *locator) {
  if (!shouldCheckForPartialApplication(cs, member, locator))
    return {false, 0};

  auto anchor = cast<UnresolvedDotExpr>(locator->getAnchor());
  // If this choice is a partial application of `init` or
  // `mutating` instance method we should report that it's not allowed.
  auto baseTy =
      cs.simplifyType(cs.getType(anchor->getBase()))->getWithoutSpecifierType();

  // Partial applications are not allowed only for constructor
  // delegation, reference on the metatype is considered acceptable.
  if (baseTy->is<MetatypeType>() && isa<ConstructorDecl>(member))
    return {false, 0};

  // If base is a metatype it would be ignored (unless this is an initializer
  // call), but if it is some other type it means that we have a single
  // application level already.
  unsigned level = baseTy->is<MetatypeType>() ? 0 : 1;
  if (auto *call = dyn_cast_or_null<CallExpr>(cs.getParentExpr(anchor))) {
    level += dyn_cast_or_null<CallExpr>(cs.getParentExpr(call)) ? 2 : 1;
  }

  return {true, level};
}

std::pair<Type, bool> ConstraintSystem::adjustTypeOfOverloadReference(
    const OverloadChoice &choice, ConstraintLocator *locator,
    Type boundType, Type refType) {
  // If the declaration is unavailable, note that in the score.
  if (choice.getDecl()->getAttrs().isUnavailable(getASTContext())) {
    increaseScore(SK_Unavailable);
  }

  bool bindConstraintCreated = false;
  const auto kind = choice.getKind();
  if (kind != OverloadChoiceKind::DeclViaDynamic &&
      !isRequirementOrWitness(locator) &&
      choice.getDecl()->getAttrs().hasAttribute<OptionalAttr>() &&
      !isa<SubscriptDecl>(choice.getDecl())) {
    // For a non-subscript declaration that is an optional
    // requirement in a protocol, strip off the lvalue-ness (FIXME:
    // one cannot assign to such declarations for now) and make a
    // reference to that declaration be optional.
    //
    // Subscript declarations are handled within
    // getTypeOfMemberReference(); their result types are optional.

    // Deal with values declared as implicitly unwrapped, or
    // functions with return types that are implicitly unwrapped.
    // TODO: Move this logic to bindOverloadType.
    if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
      // Build the disjunction to attempt binding both T? and T (or
      // function returning T? and function returning T).
      Type ty = createTypeVariable(locator,
                                   TVO_CanBindToLValue | TVO_CanBindToNoEscape);
      buildDisjunctionForImplicitlyUnwrappedOptional(ty, refType, locator);
      addConstraint(ConstraintKind::Bind, boundType,
                    OptionalType::get(ty->getRValueType()), locator);
      bindConstraintCreated = true;
    }

    // TODO: Move this to getTypeOfMemberReference.
    refType = OptionalType::get(refType->getRValueType());
  }

  switch (kind) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::TupleIndex:
  case OverloadChoiceKind::BaseType:
  case OverloadChoiceKind::KeyPathApplication:
    return {refType, bindConstraintCreated};
  case OverloadChoiceKind::DeclViaDynamic: {
    // TODO: Move the IUO handling logic here to bindOverloadType.
    if (isa<SubscriptDecl>(choice.getDecl())) {
      // We always expect function type for subscripts.
      auto fnTy = refType->castTo<AnyFunctionType>();
      if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
        auto resultTy = fnTy->getResult();
        // We expect the element type to be a double-optional.
        auto optTy = resultTy->getOptionalObjectType();
        assert(optTy->getOptionalObjectType());

        // For our original type T -> U?? we will generate:
        // A disjunction V = { U?, U }
        // and a disjunction boundType = { T -> V?, T -> V }
        Type ty = createTypeVariable(locator, TVO_CanBindToNoEscape);

        buildDisjunctionForImplicitlyUnwrappedOptional(ty, optTy, locator);

        // Create a new function type with an optional of this type
        // variable as the result type.
        if (auto *genFnTy = fnTy->getAs<GenericFunctionType>()) {
          fnTy = GenericFunctionType::get(
              genFnTy->getGenericSignature(), genFnTy->getParams(),
              OptionalType::get(ty), genFnTy->getExtInfo());
        } else {
          fnTy = FunctionType::get(fnTy->getParams(), OptionalType::get(ty),
                                   fnTy->getExtInfo());
        }
      }

      buildDisjunctionForDynamicLookupResult(boundType, fnTy, locator);
    } else {
      Type ty = refType;

      // If this is something we need to implicitly unwrap, set up a
      // new type variable and disjunction that will allow us to make
      // the choice of whether to do so.
      if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
        // Duplicate the structure of boundType, with fresh type
        // variables. We'll create a binding disjunction using this,
        // selecting between options for refType, which is either
        // Optional or a function type returning Optional.
        assert(boundType->hasTypeVariable());
        ty = boundType.transform([this](Type elTy) -> Type {
          if (auto *tv = dyn_cast<TypeVariableType>(elTy.getPointer())) {
            return createTypeVariable(tv->getImpl().getLocator(),
                                      tv->getImpl().getRawOptions());
          }
          return elTy;
        });

        buildDisjunctionForImplicitlyUnwrappedOptional(
            ty, refType->getRValueType(), locator);
      }

      // Build the disjunction to attempt binding both T? and T (or
      // function returning T? and function returning T).
      buildDisjunctionForDynamicLookupResult(
          boundType, OptionalType::get(ty->getRValueType()), locator);

      // We store an Optional of the originally resolved type in the
      // overload set.
      // TODO: Move this to getTypeOfMemberReference.
      refType = OptionalType::get(refType->getRValueType());
    }

    return {refType, /*bindConstraintCreated*/ true};
  }
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup:
    return {refType, bindConstraintCreated};
  }

  llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
}

void ConstraintSystem::bindOverloadType(
    const SelectedOverload &overload, Type boundType,
    ConstraintLocator *locator, DeclContext *useDC,
    llvm::function_ref<void(unsigned int, Type, ConstraintLocator *)>
        verifyThatArgumentIsHashable) {
  auto choice = overload.choice;
  auto openedType = overload.openedType;

  auto bindTypeOrIUO = [&](Type ty) {
    if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
      // Build the disjunction to attempt binding both T? and T (or
      // function returning T? and function returning T).
      buildDisjunctionForImplicitlyUnwrappedOptional(boundType, ty, locator);
    } else {
      // Add the type binding constraint.
      addConstraint(ConstraintKind::Bind, boundType, ty, locator);
    }
  };
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::TupleIndex:
  case OverloadChoiceKind::BaseType:
  case OverloadChoiceKind::KeyPathApplication:
  case OverloadChoiceKind::DeclViaDynamic:
    bindTypeOrIUO(openedType);
    return;
  case OverloadChoiceKind::DynamicMemberLookup: {
    // DynamicMemberLookup results are always a (dynamicMember:T1)->T2
    // subscript.
    auto refFnType = openedType->castTo<FunctionType>();

    // Before we drop the argument type on the floor, we need to constrain it
    // to having a literal conformance to ExpressibleByStringLiteral.  This
    // makes the index default to String if otherwise unconstrained.
    assert(refFnType->getParams().size() == 1 &&
           "subscript always has one arg");
    auto argType = refFnType->getParams()[0].getPlainType();

    auto stringLiteral =
        TypeChecker::getProtocol(getASTContext(), choice.getDecl()->getLoc(),
                                 KnownProtocolKind::ExpressibleByStringLiteral);
    if (!stringLiteral)
      return;

    addConstraint(ConstraintKind::LiteralConformsTo, argType,
                  stringLiteral->getDeclaredType(), locator);

    // If this is used inside of the keypath expression, we need to make
    // sure that argument is Hashable.
    if (isa<KeyPathExpr>(locator->getAnchor()))
      verifyThatArgumentIsHashable(0, argType, locator);

    // The resolved decl is for subscript(dynamicMember:), however the original
    // member constraint was for a property. Therefore we need to bind to the
    // result type.
    bindTypeOrIUO(refFnType->getResult());
    return;
  }
  case OverloadChoiceKind::KeyPathDynamicMemberLookup: {
    auto *fnType = openedType->castTo<FunctionType>();
    assert(fnType->getParams().size() == 1 &&
           "subscript always has one argument");
    // Parameter type is KeyPath<T, U> where `T` is a root type
    // and U is a leaf type (aka member type).
    auto keyPathTy =
        fnType->getParams()[0].getPlainType()->castTo<BoundGenericType>();

    auto *keyPathDecl = keyPathTy->getAnyNominal();
    assert(isKnownKeyPathDecl(getASTContext(), keyPathDecl) &&
           "parameter is supposed to be a keypath");

    auto *keyPathLoc = getConstraintLocator(
        locator, LocatorPathElt::KeyPathDynamicMember(keyPathDecl));

    auto rootTy = keyPathTy->getGenericArgs()[0];
    auto leafTy = keyPathTy->getGenericArgs()[1];

    // Member would either point to mutable or immutable property, we
    // don't which at the moment, so let's allow its type to be l-value.
    auto memberTy = createTypeVariable(keyPathLoc, TVO_CanBindToLValue |
                                                       TVO_CanBindToNoEscape);
    // Attempt to lookup a member with a give name in the root type and
    // assign result to the leaf type of the keypath.
    bool isSubscriptRef = locator->isSubscriptMemberRef();
    DeclNameRef memberName = isSubscriptRef
                           ? DeclNameRef::createSubscript()
                           // FIXME: Should propagate name-as-written through.
                           : DeclNameRef(choice.getName());

    addValueMemberConstraint(LValueType::get(rootTy), memberName, memberTy,
                             useDC,
                             isSubscriptRef ? FunctionRefKind::DoubleApply
                                            : FunctionRefKind::Unapplied,
                             /*outerAlternatives=*/{}, keyPathLoc);

    // In case of subscript things are more compicated comparing to "dot"
    // syntax, because we have to get "applicable function" constraint
    // associated with index expression and re-bind it to match "member type"
    // looked up by dynamically.
    if (isSubscriptRef) {
      // Make sure that regular subscript declarations (if any) are
      // preferred over key path dynamic member lookup.
      increaseScore(SK_KeyPathSubscript);

      auto dynamicResultTy = boundType->castTo<TypeVariableType>();
      auto constraints = getConstraintGraph().gatherConstraints(
          dynamicResultTy, ConstraintGraph::GatheringKind::EquivalenceClass,
          [](Constraint *constraint) {
            return constraint->getKind() == ConstraintKind::ApplicableFunction;
          });

      assert(constraints.size() == 1);
      auto *applicableFn = constraints.front();
      retireConstraint(applicableFn);

      // Original subscript expression e.g. `<base>[0]` generated following
      // constraint `($T_A0, [$T_A1], ...) -> $T_R applicable fn $T_S` where
      // `$T_S` is supposed to be bound to each subscript choice e.g.
      // `(Int) -> Int`.
      //
      // Here is what we need to do to make this work as-if expression was
      // `<base>[dynamicMember: \.[0]]`:
      // - Right-hand side function type would have to get a new result type
      //   since it would have to point to result type of `\.[0]`, arguments
      //   though should stay the same.
      // - Left-hand side `$T_S` is going to point to a new "member type"
      //   we are looking up based on the root type of the key path.
      // - Original result type `$T_R` is going to represent result of
      //   the `[dynamicMember: \.[0]]` invocation.

      // Result of the `WritableKeyPath` is going to be l-value type,
      // let's adjust l-valueness of the result type to accommodate that.
      //
      // This is required because we are binding result of the subscript
      // to its "member type" which becomes dynamic result type. We could
      // form additional `applicable fn` constraint here and bind it to a
      // function type, but it would create inconsistency with how properties
      // are handled, which means more special handling in CSApply.
      if (keyPathDecl == getASTContext().getWritableKeyPathDecl() ||
          keyPathDecl == getASTContext().getReferenceWritableKeyPathDecl())
        dynamicResultTy->getImpl().setCanBindToLValue(getSavedBindings(),
                                                      /*enabled=*/true);

      auto fnType = applicableFn->getFirstType()->castTo<FunctionType>();

      auto subscriptResultTy = createTypeVariable(
          getConstraintLocator(locator->getAnchor(),
                               ConstraintLocator::FunctionResult),
          TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      auto adjustedFnTy =
          FunctionType::get(fnType->getParams(), subscriptResultTy);

      ConstraintLocatorBuilder kpLocBuilder(keyPathLoc);
      addConstraint(
          ConstraintKind::ApplicableFunction, adjustedFnTy, memberTy,
          kpLocBuilder.withPathElement(ConstraintLocator::ApplyFunction));

      addConstraint(ConstraintKind::Bind, dynamicResultTy, fnType->getResult(),
                    keyPathLoc);

      addConstraint(ConstraintKind::Equal, subscriptResultTy, leafTy,
                    keyPathLoc);
    } else {
      // Since member type is going to be bound to "leaf" generic parameter
      // of the keypath, it has to be an r-value always, so let's add a new
      // constraint to represent that conversion instead of loading member
      // type into "leaf" directly.
      addConstraint(ConstraintKind::Equal, memberTy, leafTy, keyPathLoc);
    }

    if (isa<KeyPathExpr>(locator->getAnchor()))
      verifyThatArgumentIsHashable(0, keyPathTy, locator);

    // The resolved decl is for subscript(dynamicMember:), however the
    // original member constraint was either for a property, or we've
    // re-purposed the overload type variable to represent the result type of
    // the subscript. In both cases, we need to bind to the result type.
    bindTypeOrIUO(fnType->getResult());
    return;
  }
  }
  llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
}

void ConstraintSystem::resolveOverload(ConstraintLocator *locator,
                                       Type boundType,
                                       OverloadChoice choice,
                                       DeclContext *useDC) {
  // Add a conformance constraint to make sure that given type conforms
  // to Hashable protocol, which is important for key path subscript
  // components.
  auto verifyThatArgumentIsHashable = [&](unsigned index, Type argType,
                                          ConstraintLocator *locator) {
    if (auto *hashable = TypeChecker::getProtocol(
            argType->getASTContext(), choice.getDecl()->getLoc(),
            KnownProtocolKind::Hashable)) {
      addConstraint(ConstraintKind::ConformsTo, argType,
                    hashable->getDeclaredType(),
                    getConstraintLocator(
                        locator, LocatorPathElt::TupleElement(index)));
    }
  };

  // Determine the type to which we'll bind the overload set's type.
  Type refType;
  Type openedFullType;

  bool bindConstraintCreated = false;
  switch (auto kind = choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup: {
    // If we refer to a top-level decl with special type-checking semantics,
    // handle it now.
    const auto semantics =
        TypeChecker::getDeclTypeCheckingSemantics(choice.getDecl());
    if (semantics != DeclTypeCheckingSemantics::Normal) {
      std::tie(openedFullType, refType) =
          getTypeOfReferenceWithSpecialTypeCheckingSemantics(*this, locator,
                                                             semantics);
      // Declarations with special type checking semantics do not require
      // any further adjustments to the constraint system. Break out of
      // here so we don't do any more work.
      break;
    } else if (auto baseTy = choice.getBaseType()) {
      // Retrieve the type of a reference to the specific declaration choice.
      assert(!baseTy->hasTypeParameter());

      auto getDotBase = [](const Expr *E) -> const DeclRefExpr * {
        if (E == nullptr) return nullptr;
        switch (E->getKind()) {
        case ExprKind::MemberRef: {
          auto Base = cast<MemberRefExpr>(E)->getBase();
          return dyn_cast<const DeclRefExpr>(Base);
        }
        case ExprKind::UnresolvedDot: {
          auto Base = cast<UnresolvedDotExpr>(E)->getBase();
          return dyn_cast<const DeclRefExpr>(Base);
        }
        default:
          return nullptr;
        }
      };
      auto anchor = locator ? locator->getAnchor() : nullptr;
      auto base = getDotBase(anchor);
      std::tie(openedFullType, refType)
        = getTypeOfMemberReference(baseTy, choice.getDecl(), useDC,
                                   (kind == OverloadChoiceKind::DeclViaDynamic),
                                   choice.getFunctionRefKind(),
                                   locator, base, nullptr);
    } else {
      std::tie(openedFullType, refType)
        = getTypeOfReference(choice.getDecl(),
                             choice.getFunctionRefKind(), locator, useDC);
    }

    // For a non-subscript declaration found via dynamic lookup, strip
    // off the lvalue-ness (FIXME: as a temporary hack. We eventually
    // want this to work) and make a reference to that declaration be
    // an implicitly unwrapped optional.
    //
    // Subscript declarations are handled within
    // getTypeOfMemberReference(); their result types are unchecked
    // optional.
    std::tie(refType, bindConstraintCreated) =
        adjustTypeOfOverloadReference(choice, locator, boundType, refType);
    break;
  }

  case OverloadChoiceKind::BaseType:
    refType = choice.getBaseType();
    break;

  case OverloadChoiceKind::TupleIndex:
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
      refType = LValueType::get(refType);
    } else {
      // When the base is a tuple rvalue, the member is always an rvalue.
      auto tuple = choice.getBaseType()->castTo<TupleType>();
      refType = tuple->getElementType(choice.getTupleIndex())->getRValueType();
    }
    break;

  case OverloadChoiceKind::KeyPathApplication: {
    // Key path application looks like a subscript(keyPath: KeyPath<Base, T>).
    // The element type is T or @lvalue T based on the key path subtype and
    // the mutability of the base.
    auto keyPathIndexTy = createTypeVariable(
        getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToInOut);
    auto elementTy = createTypeVariable(
            getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
            TVO_CanBindToLValue | TVO_CanBindToNoEscape);
    auto elementObjTy = createTypeVariable(
        getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape);
    addConstraint(ConstraintKind::Equal, elementTy, elementObjTy, locator);

    // The element result is an lvalue or rvalue based on the key path class.
    addKeyPathApplicationConstraint(
                  keyPathIndexTy, choice.getBaseType(), elementTy, locator);

    FunctionType::Param indices[] = {
      FunctionType::Param(keyPathIndexTy, getASTContext().Id_keyPath),
    };
    auto subscriptTy = FunctionType::get(indices, elementTy);

    FunctionType::Param baseParam(choice.getBaseType());
    auto fullTy = FunctionType::get({baseParam}, subscriptTy);
    openedFullType = fullTy;
    refType = subscriptTy;

    // Increase the score so that actual subscripts get preference.
    increaseScore(SK_KeyPathSubscript);
    break;
  }
  }
  assert(!refType->hasTypeParameter() && "Cannot have a dependent type here");

  if (auto *decl = choice.getDeclOrNull()) {
    // If we're binding to an init member, the 'throws' need to line up
    // between the bound and reference types.
    if (auto CD = dyn_cast<ConstructorDecl>(decl)) {
      auto boundFunctionType = boundType->getAs<AnyFunctionType>();

      if (boundFunctionType &&
          CD->hasThrows() != boundFunctionType->throws()) {
        boundType = boundFunctionType->withExtInfo(
            boundFunctionType->getExtInfo().withThrows());
      }
    }

    if (auto *SD = dyn_cast<SubscriptDecl>(decl)) {
      if (locator->isResultOfKeyPathDynamicMemberLookup() ||
          locator->isKeyPathSubscriptComponent()) {
        // Subscript type has a format of (Self[.Type) -> (Arg...) -> Result
        auto declTy = openedFullType->castTo<FunctionType>();
        auto subscriptTy = declTy->getResult()->castTo<FunctionType>();
        // If we have subscript, each of the arguments has to conform to
        // Hashable, because it would be used as a component inside key path.
        for (auto index : indices(subscriptTy->getParams())) {
          const auto &param = subscriptTy->getParams()[index];
          verifyThatArgumentIsHashable(index, param.getParameterType(), locator);
        }
      }
    }

    // Check whether applying this overload would result in invalid
    // partial function application e.g. partial application of
    // mutating method or initializer.

    // This check is supposed to be performed without
    // `shouldAttemptFixes` because name lookup can't
    // detect that particular partial application is
    // invalid, so it has to return all of the candidates.

    bool isInvalidPartialApply;
    unsigned level;

    std::tie(isInvalidPartialApply, level) =
        isInvalidPartialApplication(*this, decl, locator);

    if (isInvalidPartialApply) {
      // No application at all e.g. `Foo.bar`.
      if (level == 0) {
        // Swift 4 and earlier failed to diagnose a reference to a mutating
        // method without any applications at all, which would get
        // miscompiled into a function with undefined behavior. Warn for
        // source compatibility.
        bool isWarning = !getASTContext().isSwiftVersionAtLeast(5);
        (void)recordFix(
            AllowInvalidPartialApplication::create(isWarning, *this, locator));
      } else if (level == 1) {
        // `Self` parameter is applied, e.g. `foo.bar` or `Foo.bar(&foo)`
        (void)recordFix(AllowInvalidPartialApplication::create(
            /*isWarning=*/false, *this, locator));
      }

      // Otherwise both `Self` and arguments are applied,
      // e.g. `foo.bar()` or `Foo.bar(&foo)()`, and there is nothing to do.
    }
  }

  // Note that we have resolved this overload.
  auto overload = SelectedOverload{choice, openedFullType, refType, boundType};
  auto result = ResolvedOverloads.insert({locator, overload});
  assert(result.second && "Already resolved this overload?");
  (void)result;

  // In some cases we already created the appropriate bind constraints.
  if (!bindConstraintCreated) {
    bindOverloadType(overload, boundType, locator, useDC,
                     verifyThatArgumentIsHashable);
  }

  if (getASTContext().TypeCheckerOpts.DebugConstraintSolver) {
    PrintOptions PO;
    PO.PrintTypesForDebugging = true;
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log.indent(solverState ? solverState->depth * 2 : 2)
      << "(overload set choice binding "
      << boundType->getString(PO) << " := "
      << refType->getString(PO) << ")\n";
  }

  // If this overload is disfavored, note that.
  if (choice.isDecl() &&
      choice.getDecl()->getAttrs().hasAttribute<DisfavoredOverloadAttr>()) {
    increaseScore(SK_DisfavoredOverload);
  }
}

Type ConstraintSystem::simplifyTypeImpl(Type type,
    llvm::function_ref<Type(TypeVariableType *)> getFixedTypeFn) const {
  return type.transform([&](Type type) -> Type {
    if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer()))
      return getFixedTypeFn(tvt);

    // If this is a dependent member type for which we end up simplifying
    // the base to a non-type-variable, perform lookup.
    if (auto depMemTy = dyn_cast<DependentMemberType>(type.getPointer())) {
      // Simplify the base.
      Type newBase = simplifyTypeImpl(depMemTy->getBase(), getFixedTypeFn);

      // If nothing changed, we're done.
      if (newBase.getPointer() == depMemTy->getBase().getPointer())
        return type;

      // Dependent member types should only be created for associated types.
      auto assocType = depMemTy->getAssocType();
      assert(depMemTy->getAssocType() && "Expected associated type!");

      // FIXME: It's kind of weird in general that we have to look
      // through lvalue, inout and IUO types here
      Type lookupBaseType = newBase->getWithoutSpecifierType();
      if (auto selfType = lookupBaseType->getAs<DynamicSelfType>())
        lookupBaseType = selfType->getSelfType();

      if (lookupBaseType->mayHaveMembers()) {
        auto *proto = assocType->getProtocol();
        auto conformance = DC->getParentModule()->lookupConformance(
          lookupBaseType, proto);
        if (!conformance)
          return DependentMemberType::get(lookupBaseType, assocType);

        auto subs = SubstitutionMap::getProtocolSubstitutions(
            proto, lookupBaseType, conformance);
        auto result = assocType->getDeclaredInterfaceType().subst(subs);
        if (!result->hasError())
          return result;
      }

      return DependentMemberType::get(lookupBaseType, assocType);
    }

    return type;
  });
}

Type ConstraintSystem::simplifyType(Type type) const {
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

Type Solution::simplifyType(Type type) const {
  if (!type->hasTypeVariable())
    return type;

  // Map type variables to fixed types from bindings.
  return getConstraintSystem().simplifyTypeImpl(type,
      [&](TypeVariableType *tvt) -> Type {
        auto known = typeBindings.find(tvt);
        assert(known != typeBindings.end());
        return known->second;
      });
}

size_t Solution::getTotalMemory() const {
  return sizeof(*this) + typeBindings.getMemorySize() +
         overloadChoices.getMemorySize() +
         ConstraintRestrictions.getMemorySize() +
         llvm::capacity_in_bytes(Fixes) + DisjunctionChoices.getMemorySize() +
         OpenedTypes.getMemorySize() + OpenedExistentialTypes.getMemorySize() +
         (DefaultedConstraints.size() * sizeof(void *)) +
         Conformances.size() * sizeof(std::pair<ConstraintLocator *, ProtocolConformanceRef>);
}

DeclName OverloadChoice::getName() const {
  switch (getKind()) {
    case OverloadChoiceKind::Decl:
    case OverloadChoiceKind::DeclViaDynamic:
    case OverloadChoiceKind::DeclViaBridge:
    case OverloadChoiceKind::DeclViaUnwrappedOptional:
      return getDecl()->getFullName();

    case OverloadChoiceKind::KeyPathApplication:
      // TODO: This should probably produce subscript(keyPath:), but we
      // don't currently pre-filter subscript overload sets by argument
      // keywords, so "subscript" is still the name that keypath subscripts
      // are looked up by.
      return DeclBaseName::createSubscript();

    case OverloadChoiceKind::DynamicMemberLookup:
    case OverloadChoiceKind::KeyPathDynamicMemberLookup:
      return DeclName(DynamicMember.getPointer());

    case OverloadChoiceKind::BaseType:
    case OverloadChoiceKind::TupleIndex:
      llvm_unreachable("no name!");
  }

  llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
}

bool OverloadChoice::isImplicitlyUnwrappedValueOrReturnValue() const {
  if (!isDecl())
    return false;

  auto *decl = getDecl();
  if (!decl->isImplicitlyUnwrappedOptional())
    return false;

  auto itfType = decl->getInterfaceType();
  if (!itfType->getAs<AnyFunctionType>())
    return true;

  switch (getFunctionRefKind()) {
  case FunctionRefKind::Unapplied:
  case FunctionRefKind::Compound:
    return false;
  case FunctionRefKind::SingleApply:
  case FunctionRefKind::DoubleApply:
    return true;
  }
  llvm_unreachable("unhandled kind");
}

SolutionResult ConstraintSystem::salvage() {
  auto &ctx = getASTContext();
  if (ctx.TypeCheckerOpts.DebugConstraintSolver) {
    auto &log = ctx.TypeCheckerDebug->getStream();
    log << "---Attempting to salvage and emit diagnostics---\n";
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

    // Check whether we have a best solution; this can happen if we found
    // a series of fixes that worked.
    if (auto best = findBestSolution(viable, /*minimize=*/true)) {
      if (*best != 0)
        viable[0] = std::move(viable[*best]);
      viable.erase(viable.begin() + 1, viable.end());
      return SolutionResult::forSolved(std::move(viable[0]));
    }

    // Before removing any "fixed" solutions, let's check
    // if ambiguity is caused by fixes and diagnose if possible.
    if (diagnoseAmbiguityWithFixes(viable))
      return SolutionResult::forAmbiguous(viable);

    // FIXME: If we were able to actually fix things along the way,
    // we may have to hunt for the best solution. For now, we don't care.

    // Remove solutions that require fixes; the fixes in those systems should
    // be diagnosed rather than any ambiguity.
    auto hasFixes = [](const Solution &sol) { return !sol.Fixes.empty(); };
    auto newEnd = std::remove_if(viable.begin(), viable.end(), hasFixes);
    viable.erase(newEnd, viable.end());

    // If there are multiple solutions, try to diagnose an ambiguity.
    if (viable.size() > 1) {
      if (getASTContext().TypeCheckerOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "---Ambiguity error: " << viable.size()
            << " solutions found---\n";
        int i = 0;
        for (auto &solution : viable) {
          log << "---Ambiguous solution #" << i++ << "---\n";
          solution.dump(log);
          log << "\n";
        }
      }

      if (diagnoseAmbiguity(viable)) {
        return SolutionResult::forAmbiguous(viable);
      }
    }

    // Fall through to produce diagnostics.
  }

  if (getExpressionTooComplex(viable))
    return SolutionResult::forTooComplex();

  // Could not produce a specific diagnostic; punt to the client.
  return SolutionResult::forUndiagnosedError();
}

static void diagnoseOperatorAmbiguity(ConstraintSystem &cs,
                                      Identifier operatorName,
                                      ArrayRef<Solution> solutions,
                                      ConstraintLocator *locator) {
  auto &DE = cs.getASTContext().Diags;
  auto *anchor = locator->getAnchor();

  auto *applyExpr = dyn_cast_or_null<ApplyExpr>(cs.getParentExpr(anchor));
  if (!applyExpr)
    return;

  auto isNameOfStandardComparisonOperator = [](Identifier opName) -> bool {
    return opName.is("==") || opName.is("!=") || opName.is("===") ||
           opName.is("!==") || opName.is("<") || opName.is(">") ||
           opName.is("<=") || opName.is(">=");
  };

  auto isEnumWithAssociatedValues = [](Type type) -> bool {
    if (auto *enumType = type->getAs<EnumType>())
      return !enumType->getDecl()->hasOnlyCasesWithoutAssociatedValues();
    return false;
  };

  const auto &solution = solutions.front();
  if (auto *binaryOp = dyn_cast<BinaryExpr>(applyExpr)) {
    auto *lhs = binaryOp->getArg()->getElement(0);
    auto *rhs = binaryOp->getArg()->getElement(1);

    auto lhsType = solution.simplifyType(cs.getType(lhs))->getRValueType();
    auto rhsType = solution.simplifyType(cs.getType(rhs))->getRValueType();

    if (lhsType->isEqual(rhsType)) {
      DE.diagnose(anchor->getLoc(), diag::cannot_apply_binop_to_same_args,
                  operatorName.str(), lhsType)
          .highlight(lhs->getSourceRange())
          .highlight(rhs->getSourceRange());

      if (isNameOfStandardComparisonOperator(operatorName) &&
          isEnumWithAssociatedValues(lhsType)) {
        DE.diagnose(applyExpr->getLoc(),
                    diag::no_binary_op_overload_for_enum_with_payload,
                    operatorName.str());
        return;
      }
    } else {
      DE.diagnose(anchor->getLoc(), diag::cannot_apply_binop_to_args,
                  operatorName.str(), lhsType, rhsType)
          .highlight(lhs->getSourceRange())
          .highlight(rhs->getSourceRange());
    }
  } else {
    auto argType = solution.simplifyType(cs.getType(applyExpr->getArg()));
    DE.diagnose(anchor->getLoc(), diag::cannot_apply_unop_to_arg,
                operatorName.str(), argType->getRValueType());
  }

  std::set<std::string> parameters;
  for (const auto &solution : solutions) {
    auto overload = solution.getOverloadChoice(locator);
    auto overloadType = overload.openedType;
    // Let's suggest only concrete overloads here.
    // Notes are going to take care of the rest,
    // since printing types like `(Self, Self)` is not
    // really useful.
    if (overloadType->hasTypeVariable())
      continue;

    if (auto *fnType = overloadType->getAs<FunctionType>())
      parameters.insert(
          FunctionType::getParamListAsString(fnType->getParams()));
  }

  // All of the overload choices had generic parameters like `Self`.
  if (parameters.empty())
    return;

  DE.diagnose(anchor->getLoc(), diag::suggest_partial_overloads,
              /*isResult=*/false, operatorName.str(),
              llvm::join(parameters, ", "));
}

bool ConstraintSystem::diagnoseAmbiguityWithFixes(
    SmallVectorImpl<Solution> &solutions) {
  if (solutions.empty())
    return false;

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

  // Problems related to fixes forming ambiguous solution set
  // could only be diagnosed (at the moment), if all of the fixes
  // have the same callee locator, which means they fix different
  // overloads of the same declaration.
  ConstraintLocator *commonCalleeLocator = nullptr;
  SmallPtrSet<ValueDecl *, 4> distinctChoices;
  SmallVector<std::pair<const Solution *, const ConstraintFix *>, 4>
      viableSolutions;

  bool diagnosable = llvm::all_of(solutions, [&](const Solution &solution) {
    ArrayRef<ConstraintFix *> fixes = solution.Fixes;

    // Currently only support a single fix in a solution,
    // but ultimately should be able to deal with multiple.
    if (fixes.size() != 1)
      return false;

    const auto *fix = fixes.front();
    auto  *locator = fix->getLocator();

    // Assignment failures are all about the source expression,
    // because they treat destination as a contextual type.
    if (auto *anchor = locator->getAnchor()) {
      if (auto *assignExpr = dyn_cast<AssignExpr>(anchor))
        locator = getConstraintLocator(assignExpr->getSrc());
    }

    auto *calleeLocator = getCalleeLocator(locator);
    if (commonCalleeLocator && commonCalleeLocator != calleeLocator)
      return false;

    commonCalleeLocator = calleeLocator;

    auto overload = solution.getOverloadChoiceIfAvailable(calleeLocator);
    if (!overload)
      return false;

    auto *decl = overload->choice.getDeclOrNull();
    if (!decl)
      return false;

    // If this declaration is distinct, let's record this solution
    // as viable, otherwise we'd produce the same diagnostic multiple
    // times, which means that actual problem is elsewhere.
    if (distinctChoices.insert(decl).second)
      viableSolutions.push_back({&solution, fix});
    return true;
  });

  if (!diagnosable || viableSolutions.size() < 2)
    return false;

  auto *decl = *distinctChoices.begin();
  assert(solverState);

  bool diagnosed = true;
  {
    DiagnosticTransaction transaction(getASTContext().Diags);

    const auto *fix = viableSolutions.front().second;
    auto *commonAnchor = commonCalleeLocator->getAnchor();
    auto &DE = getASTContext().Diags;
    auto name = decl->getFullName();

    if (fix->getKind() == FixKind::UseSubscriptOperator) {
      auto *UDE = cast<UnresolvedDotExpr>(commonAnchor);
      DE.diagnose(commonAnchor->getLoc(),
                  diag::could_not_find_subscript_member_did_you_mean,
                  getType(UDE->getBase()));
    } else if (fix->getKind() == FixKind::TreatRValueAsLValue) {
      DE.diagnose(commonAnchor->getLoc(),
                  diag::no_overloads_match_exactly_in_assignment,
                  decl->getBaseName());
    } else if (llvm::all_of(
                   viableSolutions,
                   [](const std::pair<const Solution *, const ConstraintFix *>
                          &fix) {
                     auto *locator = fix.second->getLocator();
                     return locator
                         ->isLastElement<LocatorPathElt::ContextualType>();
                   })) {
      auto baseName = name.getBaseName();
      DE.diagnose(commonAnchor->getLoc(), diag::no_candidates_match_result_type,
                  baseName.userFacingName(), getContextualType());
    } else {
      // Three choices here:
      // 1. If this is a special name avoid printing it because
      //    printing kind is sufficient;
      // 2. If all of the labels match, print a full name;
      // 3. If labels in different choices are different, it means
      //    that we can only print a base name.
      if (name.isSpecial()) {
        DE.diagnose(commonAnchor->getLoc(),
                    diag::no_overloads_match_exactly_in_call_special,
                    decl->getDescriptiveKind());
      } else if (name.isOperator()) {
        auto operatorId = name.getBaseIdentifier();
        diagnoseOperatorAmbiguity(*this, operatorId, solutions,
                                  commonCalleeLocator);
      } else if (llvm::all_of(distinctChoices,
                              [&name](const ValueDecl *choice) {
                                return choice->getFullName() == name;
                              })) {
        DE.diagnose(commonAnchor->getLoc(),
                    diag::no_overloads_match_exactly_in_call,
                    decl->getDescriptiveKind(), name);
      } else {
        DE.diagnose(commonAnchor->getLoc(),
                    diag::no_overloads_match_exactly_in_call_no_labels,
                    decl->getDescriptiveKind(), name.getBaseName());
      }
    }

    for (const auto &viable : viableSolutions) {
      // Create scope so each applied solution is rolled back.
      ConstraintSystem::SolverScope scope(*this);
      applySolution(*viable.first);
      // All of the solutions supposed to produce a "candidate" note.
      diagnosed &= viable.second->diagnose(/*asNote*/ true);
    }

    // If not all of the fixes produced a note, we can't diagnose this.
    if (!diagnosed)
      transaction.abort();
  }

  return diagnosed;
}

/// Determine the number of distinct overload choices in the
/// provided set.
static unsigned countDistinctOverloads(ArrayRef<OverloadChoice> choices) {
  llvm::SmallPtrSet<void *, 4> uniqueChoices;
  unsigned result = 0;
  for (auto choice : choices) {
    if (uniqueChoices.insert(choice.getOpaqueChoiceSimple()).second)
      ++result;
  }
  return result;
}

/// Determine the name of the overload in a set of overload choices.
static DeclName getOverloadChoiceName(ArrayRef<OverloadChoice> choices) {
  DeclName name;
  for (auto choice : choices) {
    if (!choice.isDecl())
      continue;

    DeclName nextName = choice.getDecl()->getFullName();
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

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      IndexMap[E] = Index;
      Index++;
      return { true, E };
    }
  };

  RecordingTraversal traversal(indexMap);
  expr->walk(traversal);
}

bool ConstraintSystem::diagnoseAmbiguity(ArrayRef<Solution> solutions) {
  // Produce a diff of the solutions.
  SolutionDiff diff(solutions);

  // Find the locators which have the largest numbers of distinct overloads.
  Optional<unsigned> bestOverload;
  // Overloads are scored by lexicographical comparison of (# of distinct
  // overloads, depth, *reverse* of the index). N.B. - cannot be used for the
  // reversing: the score version of index == 0 should be > than that of 1, but
  // -0 == 0 < UINT_MAX == -1, whereas ~0 == UINT_MAX > UINT_MAX - 1 == ~1.
  auto score = [](unsigned distinctOverloads, unsigned depth, unsigned index) {
    return std::make_tuple(distinctOverloads, depth, ~index);
  };
  auto bestScore = score(0, 0, std::numeric_limits<unsigned>::max());

  // Get a map of expressions to their depths and post-order traversal indices.
  // Heuristically, all other things being equal, we should complain about the
  // ambiguous expression that (1) has the most overloads, (2) is deepest, or
  // (3) comes earliest in the expression.
  llvm::DenseMap<Expr *, unsigned> indexMap;
  for (auto expr : InputExprs) {
    extendPreorderIndexMap(expr, indexMap);
  }

  for (unsigned i = 0, n = diff.overloads.size(); i != n; ++i) {
    auto &overload = diff.overloads[i];

    // If we can't resolve the locator to an anchor expression with no path,
    // we can't diagnose this well.
    auto *anchor = simplifyLocatorToAnchor(overload.locator);
    if (!anchor)
      continue;
    auto it = indexMap.find(anchor);
    if (it == indexMap.end())
      continue;
    unsigned index = it->second;

    auto optDepth = getExprDepth(anchor);
    if (!optDepth)
      continue;
    unsigned depth = *optDepth;

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
    auto thisScore = score(distinctOverloads, depth, index);
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

    // Emit the ambiguity diagnostic.
    auto &DE = getASTContext().Diags;
    DE.diagnose(anchor->getLoc(),
                name.isOperator() ? diag::ambiguous_operator_ref
                                  : diag::ambiguous_decl_ref,
                name);

    TrailingClosureAmbiguityFailure failure(*this, anchor,
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
      case OverloadChoiceKind::DeclViaUnwrappedOptional:
        // FIXME: show deduced types, etc, etc.
        if (EmittedDecls.insert(choice.getDecl()).second)
          DE.diagnose(choice.getDecl(), diag::found_candidate);
        break;

      case OverloadChoiceKind::KeyPathApplication:
      case OverloadChoiceKind::DynamicMemberLookup:
      case OverloadChoiceKind::KeyPathDynamicMemberLookup:
        // Skip key path applications and dynamic member lookups, since we don't
        // want them to noise up unrelated subscript diagnostics.
        break;

      case OverloadChoiceKind::BaseType:
      case OverloadChoiceKind::TupleIndex:
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

void constraints::simplifyLocator(Expr *&anchor,
                                  ArrayRef<LocatorPathElt> &path,
                                  SourceRange &range) {
  range = SourceRange();

  while (!path.empty()) {
    switch (path[0].getKind()) {
    case ConstraintLocator::ApplyArgument: {
      // Extract application argument.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        anchor = applyExpr->getArg();
        path = path.slice(1);
        continue;
      }

      if (auto subscriptExpr = dyn_cast<SubscriptExpr>(anchor)) {
        anchor = subscriptExpr->getIndex();
        path = path.slice(1);
        continue;
      }

      if (auto objectLiteralExpr = dyn_cast<ObjectLiteralExpr>(anchor)) {
        anchor = objectLiteralExpr->getArg();
        path = path.slice(1);
        continue;
      }

      if (auto *UME = dyn_cast<UnresolvedMemberExpr>(anchor)) {
        anchor = UME->getArgument();
        path = path.slice(1);
        continue;
      }
      break;
    }

    case ConstraintLocator::DynamicCallable: {
      path = path.slice(1);
      continue;
    }

    case ConstraintLocator::ApplyFunction:
      // Extract application function.
      if (auto applyExpr = dyn_cast<ApplyExpr>(anchor)) {
        anchor = applyExpr->getFn();
        path = path.slice(1);
        continue;
      }

      // The subscript itself is the function.
      if (auto subscriptExpr = dyn_cast<SubscriptExpr>(anchor)) {
        anchor = subscriptExpr;
        path = path.slice(1);
        continue;
      }

      // The unresolved member itself is the function.
      if (auto unresolvedMember = dyn_cast<UnresolvedMemberExpr>(anchor)) {
        if (unresolvedMember->getArgument()) {
          anchor = unresolvedMember;
          path = path.slice(1);
          continue;
        }
      }

      break;

    case ConstraintLocator::AutoclosureResult:
    case ConstraintLocator::LValueConversion:
    case ConstraintLocator::RValueAdjustment:
    case ConstraintLocator::UnresolvedMember:
    case ConstraintLocator::ImplicitCallAsFunction:
      // Arguments in autoclosure positions, lvalue and rvalue adjustments,
      // unresolved members, and implicit callAsFunction references are
      // implicit.
      path = path.slice(1);
      continue;

    case ConstraintLocator::NamedTupleElement:
    case ConstraintLocator::TupleElement: {
      // Extract tuple element.
      auto elt = path[0].castTo<LocatorPathElt::AnyTupleElement>();
      unsigned index = elt.getIndex();
      if (auto tupleExpr = dyn_cast<TupleExpr>(anchor)) {
        if (index < tupleExpr->getNumElements()) {
          anchor = tupleExpr->getElement(index);
          path = path.slice(1);
          continue;
        }
      }

      if (auto *CE = dyn_cast<CollectionExpr>(anchor)) {
        if (index < CE->getNumElements()) {
          anchor = CE->getElement(index);
          path = path.slice(1);
          continue;
        }
      }
      break;
    }

    case ConstraintLocator::ApplyArgToParam: {
      auto elt = path[0].castTo<LocatorPathElt::ApplyArgToParam>();
      // Extract tuple element.
      if (auto tupleExpr = dyn_cast<TupleExpr>(anchor)) {
        unsigned index = elt.getArgIdx();
        if (index < tupleExpr->getNumElements()) {
          anchor = tupleExpr->getElement(index);
          path = path.slice(1);
          continue;
        }
      }

      // Extract subexpression in parentheses.
      if (auto parenExpr = dyn_cast<ParenExpr>(anchor)) {
        // This simplication request could be for a synthesized argument.
        if (elt.getArgIdx() == 0) {
          anchor = parenExpr->getSubExpr();
          path = path.slice(1);
          continue;
        }
      }
      break;
    }
    case ConstraintLocator::ConstructorMember:
      if (auto typeExpr = dyn_cast<TypeExpr>(anchor)) {
        // This is really an implicit 'init' MemberRef, so point at the base,
        // i.e. the TypeExpr.
        range = SourceRange();
        anchor = typeExpr;
        path = path.slice(1);
        continue;
      }
      LLVM_FALLTHROUGH;

    case ConstraintLocator::Member:
    case ConstraintLocator::MemberRefBase:
      if (auto UDE = dyn_cast<UnresolvedDotExpr>(anchor)) {
        range = UDE->getNameLoc().getSourceRange();
        anchor = UDE->getBase();
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::SubscriptMember:
      if (isa<SubscriptExpr>(anchor)) {
        path = path.slice(1);
        continue;
      }
      break;

    case ConstraintLocator::ClosureResult:
      if (auto CE = dyn_cast<ClosureExpr>(anchor)) {
        if (CE->hasSingleExpressionBody()) {
          anchor = CE->getSingleExpressionBody();
          path = path.slice(1);
          continue;
        }
      }
      break;

    case ConstraintLocator::ContextualType:
      // This was just for identifying purposes, strip it off.
      path = path.slice(1);
      continue;

    case ConstraintLocator::KeyPathComponent: {
      auto elt = path[0].castTo<LocatorPathElt::KeyPathComponent>();

      // If the next element is an ApplyArgument, we can simplify by looking
      // into the index expression.
      if (path.size() < 2 ||
          path[1].getKind() != ConstraintLocator::ApplyArgument)
        break;

      if (auto *kpe = dyn_cast<KeyPathExpr>(anchor)) {
        auto component = kpe->getComponents()[elt.getIndex()];
        auto indexExpr = component.getIndexExpr();
        assert(indexExpr && "Trying to apply a component without an index?");
        anchor = indexExpr;
        path = path.slice(2);
        continue;
      }
      break;
    }

    case ConstraintLocator::Condition: {
      anchor = cast<IfExpr>(anchor)->getCondExpr();
      path = path.slice(1);
      continue;
    }

    default:
      // FIXME: Lots of other cases to handle.
      break;
    }

    // If we get here, we couldn't simplify the path further.
    break;
  }
}

Expr *constraints::simplifyLocatorToAnchor(ConstraintLocator *locator) {
  if (!locator)
    return nullptr;

  auto *anchor = locator->getAnchor();
  if (!anchor)
    return nullptr;

  SourceRange range;
  auto path = locator->getPath();
  simplifyLocator(anchor, path, range);

  // We only want the new anchor if all the path elements have been simplified
  // away.
  return path.empty() ? anchor : nullptr;
}

Expr *constraints::getArgumentExpr(Expr *expr, unsigned index) {
  Expr *argExpr = nullptr;
  if (auto *AE = dyn_cast<ApplyExpr>(expr))
    argExpr = AE->getArg();
  else if (auto *UME = dyn_cast<UnresolvedMemberExpr>(expr))
    argExpr = UME->getArgument();
  else if (auto *SE = dyn_cast<SubscriptExpr>(expr))
    argExpr = SE->getIndex();
  else
    return nullptr;

  if (auto *PE = dyn_cast<ParenExpr>(argExpr)) {
    assert(index == 0);
    return PE->getSubExpr();
  }

  assert(isa<TupleExpr>(argExpr));
  return cast<TupleExpr>(argExpr)->getElement(index);
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
  auto *decl = choice.getDeclOrNull();
  if (!decl)
    return false;

  auto baseType = choice.getBaseType();
  if (baseType)
    baseType = cs.getFixedTypeRecursive(baseType, /*wantRValue=*/true);

  // In most cases where we reference a declaration with a curried self
  // parameter, it gets dropped from the type of the reference.
  return decl->hasCurriedSelf() &&
         doesMemberRefApplyCurriedSelf(baseType, decl);
}

bool constraints::conformsToKnownProtocol(ConstraintSystem &cs, Type type,
                                          KnownProtocolKind protocol) {
  if (auto *proto =
          TypeChecker::getProtocol(cs.getASTContext(), SourceLoc(), protocol))
    return (bool)TypeChecker::conformsToProtocol(
        type, proto, cs.DC, ConformanceCheckFlags::InExpression);
  return false;
}

/// Check whether given type conforms to `RawPepresentable` protocol
/// and return the witness type.
Type constraints::isRawRepresentable(ConstraintSystem &cs, Type type) {
  auto *DC = cs.DC;

  auto rawReprType = TypeChecker::getProtocol(
      cs.getASTContext(), SourceLoc(), KnownProtocolKind::RawRepresentable);
  if (!rawReprType)
    return Type();

  auto conformance = TypeChecker::conformsToProtocol(
      type, rawReprType, DC, ConformanceCheckFlags::InExpression);
  if (conformance.isInvalid())
    return Type();

  return conformance.getTypeWitnessByName(type, cs.getASTContext().Id_RawValue);
}

Type constraints::isRawRepresentable(
    ConstraintSystem &cs, Type type,
    KnownProtocolKind rawRepresentableProtocol) {
  Type rawTy = isRawRepresentable(cs, type);
  if (!rawTy || !conformsToKnownProtocol(cs, rawTy, rawRepresentableProtocol))
    return Type();

  return rawTy;
}

void ConstraintSystem::generateConstraints(
    SmallVectorImpl<Constraint *> &constraints, Type type,
    ArrayRef<OverloadChoice> choices, DeclContext *useDC,
    ConstraintLocator *locator, Optional<unsigned> favoredIndex,
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

    auto *choice = fix ? Constraint::createFixedChoice(*this, type, overload,
                                                       useDC, fix, locator)
                       : Constraint::createBindOverload(*this, type, overload,
                                                        useDC, locator);

    if (isFavored)
      choice->setFavored();

    choices.push_back(choice);
  };

  if (favoredIndex) {
    const auto &choice = choices[*favoredIndex];
    assert((!choice.isDecl() ||
            !choice.getDecl()->getAttrs().isUnavailable(getASTContext())) &&
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
  auto *anchor = locator->getAnchor();
  if (!anchor)
    return nullptr;

  // Applies and unresolved member exprs can have callee locators that are
  // dependent on the type of their function, which may not have been resolved
  // yet. Therefore we need to handle them specially.
  if (auto *apply = dyn_cast<ApplyExpr>(anchor)) {
    auto *fnExpr = getArgumentLabelTargetExpr(apply->getFn());
    return getConstraintLocator(fnExpr);
  }

  if (auto *UME = dyn_cast<UnresolvedMemberExpr>(anchor))
    return getConstraintLocator(UME);

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

Optional<ConstraintSystem::ArgumentInfo>
ConstraintSystem::getArgumentInfo(ConstraintLocator *locator) {
  if (!locator)
    return None;

  if (auto *infoLocator = getArgumentInfoLocator(locator)) {
    auto known = ArgumentInfos.find(infoLocator);
    if (known != ArgumentInfos.end())
      return known->second;
  }
  return None;
}

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

  // No direct callee for an if expr.
  if (isa<IfExpr>(fnExpr))
    return false;

  // Assume that anything else would have a direct callee.
  return true;
}

Type ConstraintSystem::resolveInterfaceType(Type type) const {
  auto resolvedType = type.transform([&](Type type) -> Type {
    if (auto *tvt = type->getAs<TypeVariableType>()) {
      // If this type variable is for a generic parameter, return that.
      if (auto *gp = tvt->getImpl().getGenericParameter())
        return gp;

      // Otherwise resolve its fixed type, mapped out of context.
      if (auto fixed = getFixedType(tvt))
        return resolveInterfaceType(fixed->mapTypeOutOfContext());

      return getRepresentative(tvt);
    }
    if (auto *dmt = type->getAs<DependentMemberType>()) {
      // For a dependent member, first resolve the base.
      auto newBase = resolveInterfaceType(dmt->getBase());

      // Then reconstruct using its associated type.
      assert(dmt->getAssocType());
      return DependentMemberType::get(newBase, dmt->getAssocType());
    }
    return type;
  });

  assert(!resolvedType->hasArchetype());
  return resolvedType;
}

Optional<FunctionArgApplyInfo>
ConstraintSystem::getFunctionArgApplyInfo(ConstraintLocator *locator) {
  auto *anchor = locator->getAnchor();
  auto path = locator->getPath();

  // Look for the apply-arg-to-param element in the locator's path. We may
  // have to look through other elements that are generated from an argument
  // conversion such as GenericArgument for an optional-to-optional conversion,
  // and OptionalPayload for a value-to-optional conversion.
  auto iter = path.rbegin();
  auto applyArgElt = locator->findLast<LocatorPathElt::ApplyArgToParam>(iter);
  if (!applyArgElt)
    return None;

  auto nextIter = iter + 1;
  assert(!locator->findLast<LocatorPathElt::ApplyArgToParam>(nextIter) &&
         "Multiple ApplyArgToParam components?");

  // Form a new locator that ends at the apply-arg-to-param element, and
  // simplify it to get the full argument expression.
  auto argPath = path.drop_back(iter - path.rbegin());
  auto *argLocator = getConstraintLocator(
      anchor, argPath, ConstraintLocator::getSummaryFlagsForPath(argPath));

  auto *argExpr = simplifyLocatorToAnchor(argLocator);

  // If we were unable to simplify down to the argument expression, we don't
  // know what this is.
  if (!argExpr)
    return None;

  Optional<OverloadChoice> choice;
  Type rawFnType;
  auto *calleeLocator = getCalleeLocator(argLocator);
  if (auto overload = findSelectedOverloadFor(calleeLocator)) {
    // If we have resolved an overload for the callee, then use that to get the
    // function type and callee.
    choice = overload->choice;
    rawFnType = overload->openedType;
  } else {
    // If we didn't resolve an overload for the callee, we should be dealing
    // with a call of an arbitrary function expr.
    if (auto *call = dyn_cast<CallExpr>(anchor)) {
      assert(!shouldHaveDirectCalleeOverload(call) &&
             "Should we have resolved a callee for this?");
      rawFnType = getType(call->getFn());
    } else {
      // FIXME: ArgumentMismatchFailure is currently used from CSDiag, meaning
      // we can end up a BinaryExpr here with an unresolved callee. It should be
      // possible to remove this once we've gotten rid of the old CSDiag logic
      // and just assert that we have a CallExpr.
      auto *apply = cast<ApplyExpr>(anchor);
      rawFnType = getType(apply->getFn());
    }
  }

  // Try to resolve the function type by loading lvalues and looking through
  // optional types, which can occur for expressions like `fn?(5)`.
  auto *fnType = simplifyType(rawFnType)
                     ->getRValueType()
                     ->lookThroughAllOptionalTypes()
                     ->getAs<FunctionType>();
  if (!fnType)
    return None;

  // Resolve the interface type for the function. Note that this may not be a
  // function type, for example it could be a generic parameter.
  Type fnInterfaceType;
  auto *callee = choice ? choice->getDeclOrNull() : nullptr;
  if (callee && callee->hasInterfaceType()) {
    // If we have a callee with an interface type, we can use it. This is
    // preferable to resolveInterfaceType, as this will allow us to get a
    // GenericFunctionType for generic decls.
    //
    // Note that it's possible to find a callee without an interface type. This
    // can happen for example with closure parameters, where the interface type
    // isn't set until the solution is applied. In that case, use
    // resolveInterfaceType.
    fnInterfaceType = callee->getInterfaceType();

    // Strip off the curried self parameter if necessary.
    if (hasAppliedSelf(*this, *choice))
      fnInterfaceType = fnInterfaceType->castTo<AnyFunctionType>()->getResult();

    if (auto *fn = fnInterfaceType->getAs<AnyFunctionType>()) {
      assert(fn->getNumParams() == fnType->getNumParams() &&
             "Parameter mismatch?");
      (void)fn;
    }
  } else {
    fnInterfaceType = resolveInterfaceType(rawFnType);
  }

  auto argIdx = applyArgElt->getArgIdx();
  auto paramIdx = applyArgElt->getParamIdx();

  return FunctionArgApplyInfo(getParentExpr(argExpr), argExpr, argIdx,
                              simplifyType(getType(argExpr)),
                              paramIdx, fnInterfaceType, fnType, callee);
}

bool constraints::isKnownKeyPathType(Type type) {
  if (auto *BGT = type->getAs<BoundGenericType>())
    return isKnownKeyPathDecl(type->getASTContext(), BGT->getDecl());
  return false;
}

bool constraints::isKnownKeyPathDecl(ASTContext &ctx, ValueDecl *decl) {
  return decl == ctx.getKeyPathDecl() || decl == ctx.getWritableKeyPathDecl() ||
         decl == ctx.getReferenceWritableKeyPathDecl() ||
         decl == ctx.getPartialKeyPathDecl() || decl == ctx.getAnyKeyPathDecl();
}

static bool isOperator(Expr *expr, StringRef expectedName) {
  auto name = getOperatorName(expr);
  return name ? name->is(expectedName) : false;
}

Optional<Identifier> constraints::getOperatorName(Expr *expr) {
  ValueDecl *choice = nullptr;
  if (auto *ODRE = dyn_cast_or_null<OverloadedDeclRefExpr>(expr)) {
    choice = ODRE->getDecls().front();
  } else if (auto *DRE = dyn_cast_or_null<DeclRefExpr>(expr)) {
    choice = DRE->getDecl();
  } else {
    return None;
  }

  if (auto *FD = dyn_cast_or_null<AbstractFunctionDecl>(choice))
    return FD->getBaseName().getIdentifier();

  return None;
}

bool constraints::isPatternMatchingOperator(Expr *expr) {
  return isOperator(expr, "~=");
}

bool constraints::isOperatorArgument(ConstraintLocator *locator,
                                     StringRef expectedOperator) {
  if (!locator->findLast<LocatorPathElt::ApplyArgToParam>())
    return false;

  if (auto *AE = dyn_cast_or_null<ApplyExpr>(locator->getAnchor())) {
    if (isa<PrefixUnaryExpr>(AE) || isa<BinaryExpr>(AE) ||
        isa<PostfixUnaryExpr>(AE))
      return expectedOperator.empty() ||
             isOperator(AE->getFn(), expectedOperator);
  }

  return false;
}

bool constraints::isArgumentOfPatternMatchingOperator(
    ConstraintLocator *locator) {
  auto *binaryOp = dyn_cast_or_null<BinaryExpr>(locator->getAnchor());
  if (!(binaryOp && binaryOp->isImplicit()))
    return false;
  return isPatternMatchingOperator(binaryOp->getFn());
}

bool constraints::isArgumentOfReferenceEqualityOperator(
    ConstraintLocator *locator) {
  return isOperatorArgument(locator, "===") ||
         isOperatorArgument(locator, "!==");
}

ConversionEphemeralness
ConstraintSystem::isConversionEphemeral(ConversionRestrictionKind conversion,
                                        ConstraintLocatorBuilder locator) {
  switch (conversion) {
  case ConversionRestrictionKind::ArrayToPointer:
  case ConversionRestrictionKind::StringToPointer:
    // Always ephemeral.
    return ConversionEphemeralness::Ephemeral;
  case ConversionRestrictionKind::InoutToPointer: {

    // Ephemeral, except if the expression is a reference to a global or
    // static stored variable, or a directly accessed stored property on such a
    // variable.

    auto isDirectlyAccessedStoredVar = [&](ValueDecl *decl) -> bool {
      auto *asd = dyn_cast_or_null<AbstractStorageDecl>(decl);
      if (!asd)
        return false;

      // Check what access strategy is used for a read-write access. It must be
      // direct-to-storage in order for the conversion to be non-ephemeral.
      auto access = asd->getAccessStrategy(
          AccessSemantics::Ordinary, AccessKind::ReadWrite,
          DC->getParentModule(), DC->getResilienceExpansion());
      return access.getKind() == AccessStrategy::Storage;
    };

    SourceRange range;
    auto *argLoc = simplifyLocator(*this, getConstraintLocator(locator), range);
    auto *subExpr = argLoc->getAnchor()->getSemanticsProvidingExpr();

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
          if (nominal && isa<StructDecl>(nominal)) {
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
  case ConversionRestrictionKind::ArrayUpcast:
  case ConversionRestrictionKind::DictionaryUpcast:
  case ConversionRestrictionKind::SetUpcast:
  case ConversionRestrictionKind::HashableToAnyHashable:
  case ConversionRestrictionKind::CFTollFreeBridgeToObjC:
  case ConversionRestrictionKind::ObjCTollFreeBridgeToCF:
    // @_nonEphemeral has no effect on these conversions, so treat them as all
    // being non-ephemeral in order to allow their passing to an @_nonEphemeral
    // parameter.
    return ConversionEphemeralness::NonEphemeral;
  }
}

Expr *ConstraintSystem::buildAutoClosureExpr(Expr *expr,
                                             FunctionType *closureType) {
  auto &Context = DC->getASTContext();
  bool isInDefaultArgumentContext = false;
  if (auto *init = dyn_cast<Initializer>(DC))
    isInDefaultArgumentContext =
        init->getInitializerKind() == InitializerKind::DefaultArgument;

  auto info = closureType->getExtInfo();
  auto newClosureType = closureType;

  if (isInDefaultArgumentContext && info.isNoEscape())
    newClosureType = closureType->withExtInfo(info.withNoEscape(false))
                         ->castTo<FunctionType>();

  auto *closure = new (Context) AutoClosureExpr(
      expr, newClosureType, AutoClosureExpr::InvalidDiscriminator, DC);

  closure->setParameterList(ParameterList::createEmpty(Context));

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
