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
#include "swift/AST/GenericEnvironment.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Format.h"

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "ConstraintSystem"

ExpressionTimer::ExpressionTimer(Expr *E, ConstraintSystem &CS)
    : E(E), WarnLimit(CS.TC.getWarnLongExpressionTypeChecking()),
      Context(CS.getASTContext()),
      StartTime(llvm::TimeRecord::getCurrentTime()),
      PrintDebugTiming(CS.TC.getDebugTimeExpressions()), PrintWarning(true) {
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

  if (WarnLimit != 0 && elapsedMS >= WarnLimit && E->getLoc().isValid())
    Context.Diags.diagnose(E->getLoc(), diag::debug_long_expression,
                           elapsedMS, WarnLimit)
      .highlight(E->getSourceRange());
}

ConstraintSystem::ConstraintSystem(TypeChecker &tc, DeclContext *dc,
                                   ConstraintSystemOptions options)
  : TC(tc), DC(dc), Options(options),
    Arena(tc.Context, Allocator),
    CG(*new ConstraintGraph(*this))
{
  assert(DC && "context required");
}

ConstraintSystem::~ConstraintSystem() {
  delete &CG;
}

void ConstraintSystem::incrementScopeCounter() {
  SWIFT_FUNC_STAT;
  CountScopes++;
  // FIXME: (transitional) increment the redundant "always-on" counter.
  if (TC.Context.Stats)
    TC.Context.Stats->getFrontendCounters().NumConstraintScopes++;
}

bool ConstraintSystem::hasFreeTypeVariables() {
  // Look for any free type variables.
  for (auto tv : TypeVariables) {
    if (!tv->getImpl().hasRepresentativeOrFixed()) {
      return true;
    }
  }
  
  return false;
}

void ConstraintSystem::addTypeVariable(TypeVariableType *typeVar) {
  TypeVariables.push_back(typeVar);
  
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

      literalProtocol = TC.getLiteralProtocol(anchor);
      if (literalProtocol)
        break;
    }

    // If the protocol has a default type, check it.
    if (literalProtocol) {
      if (auto defaultType = TC.getDefaultType(literalProtocol, DC)) {
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

void ConstraintSystem::setMustBeMaterializableRecursive(Type type)
{
  assert(type->isMaterializable() &&
         "argument to setMustBeMaterializableRecursive may not be inherently "
         "non-materializable");
  type = getFixedTypeRecursive(type, /*wantRValue=*/false);
  type = type->lookThroughAllOptionalTypes();

  if (auto typeVar = type->getAs<TypeVariableType>()) {
    typeVar->getImpl().setMustBeMaterializable(getSavedBindings());
  } else if (auto *tupleTy = type->getAs<TupleType>()) {
    for (auto elt : tupleTy->getElementTypes()) {
      setMustBeMaterializableRecursive(elt);
    }
  }
}

void ConstraintSystem::addTypeVariableConstraintsToWorkList(
       TypeVariableType *typeVar) {
  // Gather the constraints affected by a change to this type variable.
  SmallVector<Constraint *, 8> constraints;
  CG.gatherConstraints(typeVar, constraints,
                       ConstraintGraph::GatheringKind::AllMentions);

  // Add any constraints that aren't already active to the worklist.
  for (auto constraint : constraints) {
    if (!constraint->isActive()) {
      ActiveConstraints.splice(ActiveConstraints.end(),
                               InactiveConstraints, constraint);
      constraint->setActive(true);
    }
  }
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

LookupResult &ConstraintSystem::lookupMember(Type base, DeclName name) {
  // Check whether we've already performed this lookup.
  auto &result = MemberLookups[{base, name}];
  if (result) return *result;

  // Lookup the member.
  NameLookupOptions lookupOptions = defaultMemberLookupOptions;
  if (isa<AbstractFunctionDecl>(DC))
    lookupOptions |= NameLookupFlags::KnownPrivate;

  result = TC.lookupMember(DC, base, name, lookupOptions);

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
    if (uniqueEntry->getAttrs().isUnavailable(TC.Context) &&
        !decl->getAttrs().isUnavailable(TC.Context)) {
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
#define EXPRESSIBLE_BY_LITERAL_PROTOCOL_WITH_NAME(Id, Name)
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
#define EXPRESSIBLE_BY_LITERAL_PROTOCOL_WITH_NAME(Id, Name)
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
    if (auto floatProto = TC.Context.getProtocol(
                            KnownProtocolKind::ExpressibleByFloatLiteral)) {
      if (auto defaultType = TC.getDefaultType(floatProto, DC)) {
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

Type ConstraintSystem::openUnboundGenericType(UnboundGenericType *unbound,
                                              ConstraintLocatorBuilder locator,
                                              OpenedTypeMap &replacements) {
  auto unboundDecl = unbound->getDecl();

  // If the unbound decl hasn't been validated yet, we have a circular
  // dependency that isn't being diagnosed properly.
  if (!unboundDecl->getGenericSignature()) {
    TC.diagnose(unboundDecl, diag::circular_reference);
    return Type();
  }

  auto parentTy = unbound->getParent();
  if (parentTy) {
    parentTy = openUnboundGenericType(parentTy, locator);
    unbound = UnboundGenericType::get(unboundDecl, parentTy,
                                      getASTContext());
  }

  // Open up the generic type.
  openGeneric(unboundDecl->getInnermostDeclContext(),
              unboundDecl->getDeclContext(),
              unboundDecl->getGenericSignature(),
              /*skipProtocolSelfConstraint=*/false,
              locator,
              replacements);

  if (parentTy) {
    auto subs = parentTy->getContextSubstitutions(
      unboundDecl->getDeclContext());
    for (auto pair : subs) {
      auto found = replacements.find(
        cast<GenericTypeParamType>(pair.first));
      assert(found != replacements.end() &&
             "Missing generic parameter?");
      addConstraint(ConstraintKind::Equal, found->second, pair.second,
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
  return TC.applyUnboundGenericArguments(
      unbound, unboundDecl,
      SourceLoc(), DC, arguments,
      /*resolver*/nullptr);
}

static void checkNestedTypeConstraints(ConstraintSystem &cs, Type type,
                                       ConstraintLocatorBuilder locator) {
  // If this is a type defined inside of constrainted extension, let's add all
  // of the generic requirements to the constraint system to make sure that it's
  // something we can use.
  GenericTypeDecl *decl = nullptr;
  Type parentTy;
  SubstitutionMap subMap;

  if (auto *NAT = dyn_cast<NameAliasType>(type.getPointer())) {
    decl = NAT->getDecl();
    parentTy = NAT->getParent();
    subMap = NAT->getSubstitutionMap();
  } else if (auto *AGT = type->getAs<AnyGenericType>()) {
    decl = AGT->getDecl();
    parentTy = AGT->getParent();
    // the context substitution map is fine here, since we can't be adding more
    // info than that, unlike a typealias
  }

  // If this decl is generic, the constraints are handled when the generic
  // parameters are applied, so we don't have to handle them here (which makes
  // getting the right substitution maps easier).
  if (decl && !decl->isGeneric()) {
    auto extension = dyn_cast<ExtensionDecl>(decl->getDeclContext());
    if (parentTy && extension && extension->isConstrainedExtension()) {
      auto contextSubMap = parentTy->getContextSubstitutionMap(
          extension->getParentModule(),
          extension->getAsNominalTypeOrNominalTypeExtensionContext());
      if (!subMap) {
        // The substitution map wasn't set above, meaning we should grab the map
        // for the extension itself.
        subMap = parentTy->getContextSubstitutionMap(
            extension->getParentModule(), extension);
      }

      if (auto *signature = decl->getGenericSignature()) {
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
                                LookUpConformanceInSubstitutionMap(subMap),
                                SubstFlags::UseErrorType);
            });
      }
    }

    // And now make sure sure the parent is okay, for things like X<T>.Y.Z.
    if (parentTy) {
      checkNestedTypeConstraints(cs, parentTy, locator);
    }
  }
}

Type ConstraintSystem::openUnboundGenericType(
    Type type, ConstraintLocatorBuilder locator) {
  assert(!type->hasTypeParameter());

  checkNestedTypeConstraints(*this, type, locator);

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
        assert(known != replacements.end());
        return known->second;
      }

      return type;
    });
}

/// Remove argument labels from the function type.
static Type removeArgumentLabels(Type type, unsigned numArgumentLabels) {
  // If there is nothing to remove, don't.
  if (numArgumentLabels == 0) return type;
  
  auto fnType = type->getAs<FunctionType>();

  // Drop argument labels from the input type.
  llvm::SmallVector<AnyFunctionType::Param, 4> unlabeledParams;
  unlabeledParams.reserve(fnType->getNumParams());
  for (const auto &param : fnType->getParams())
    unlabeledParams.push_back(param.getWithoutLabel());

  return FunctionType::get(
      unlabeledParams,
      removeArgumentLabels(fnType->getResult(), numArgumentLabels - 1),
      fnType->getExtInfo());
}

Type ConstraintSystem::openFunctionType(
       AnyFunctionType *funcType,
       unsigned numArgumentLabelsToRemove,
       ConstraintLocatorBuilder locator,
       OpenedTypeMap &replacements,
       DeclContext *innerDC,
       DeclContext *outerDC,
       bool skipProtocolSelfConstraint) {
  Type type;

  if (auto *genericFn = funcType->getAs<GenericFunctionType>()) {
    // Open up the generic parameters and requirements.
    openGeneric(innerDC,
                outerDC,
                genericFn->getGenericSignature(),
                skipProtocolSelfConstraint,
                locator,
                replacements);

    // Transform the parameters and output type.
    llvm::SmallVector<AnyFunctionType::Param, 4> openedParams;
    openedParams.reserve(genericFn->getNumParams());
    for (const auto &param : genericFn->getParams()) {
      auto type = openType(param.getPlainType(), replacements);
      openedParams.push_back(AnyFunctionType::Param(type, param.getLabel(),
                                                    param.getParameterFlags()));
    }

    auto resultTy = openType(genericFn->getResult(), replacements);

    // Build the resulting (non-generic) function type.
    funcType = FunctionType::get(
        openedParams, resultTy,
        FunctionType::ExtInfo().withThrows(genericFn->throws()));
  }

  return removeArgumentLabels(funcType, numArgumentLabelsToRemove);
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
  auto &ctx = type->getASTContext();
  if (auto *structType = type->getAs<BoundGenericStructType>()) {
    auto *decl = structType->getDecl();
    if (decl == ctx.getArrayDecl() || decl == ctx.getDictionaryDecl() ||
        decl == ctx.getSetDecl())
      return true;
  }

  return false;
}

bool ConstraintSystem::isAnyHashableType(Type type) {
  if (auto tv = type->getAs<TypeVariableType>()) {
    auto fixedType = getFixedType(tv);
    return fixedType && isAnyHashableType(fixedType);
  }

  if (auto st = type->getAs<StructType>()) {
    return st->getDecl() == TC.Context.getAnyHashableDecl();
  }

  return false;
}

Type ConstraintSystem::getFixedTypeRecursive(Type type,
                                             TypeMatchOptions &flags,
                                             bool wantRValue,
                                             bool retainParens) {

  if (wantRValue)
    type = type->getRValueType();

  if (retainParens) {
    if (auto parenTy = dyn_cast<ParenType>(type.getPointer())) {
      type = getFixedTypeRecursive(parenTy->getUnderlyingType(), flags,
                                   wantRValue, retainParens);
      auto flags = parenTy->getParameterFlags().withInOut(type->is<InOutType>());
      return ParenType::get(getASTContext(), type->getInOutObjectType(), flags);
    }
  }

  while (true) {
    if (auto depMemType = type->getAs<DependentMemberType>()) {
      if (!depMemType->getBase()->isTypeVariableOrMember()) return type;

      // FIXME: Perform a more limited simplification?
      Type newType = simplifyType(type);
      if (newType.getPointer() == type.getPointer()) return type;

      if (wantRValue)
        newType = newType->getRValueType();

      type = newType;

      // Once we've simplified a dependent member type, we need to generate a
      // new constraint.
      flags |= TMF_GenerateConstraints;
      continue;
    }

    if (auto typeVar = type->getAs<TypeVariableType>()) {
      bool hasRepresentative = false;
      if (auto *repr = getRepresentative(typeVar)) {
        if (typeVar != repr) {
          hasRepresentative = true;
          typeVar = repr;
        }
      }

      if (auto fixed = getFixedType(typeVar)) {
        if (wantRValue)
          fixed = fixed->getRValueType();

        type = fixed;
        continue;
      }

      // If type variable has a representative but
      // no fixed type, reflect that in the type itself.
      if (hasRepresentative)
        type = typeVar;

      break;
    }

    break;
  }

  return type;
}

/// Does a var or subscript produce an l-value?
///
/// \param baseType - the type of the base on which this object
///   is being accessed; must be null if and only if this is not
///   a type member
static bool doesStorageProduceLValue(TypeChecker &TC,
                                     AbstractStorageDecl *storage,
                                     Type baseType, DeclContext *useDC,
                                     const DeclRefExpr *base = nullptr) {
  // Unsettable storage decls always produce rvalues.
  if (!storage->isSettable(useDC, base))
    return false;
  
  if (TC.Context.LangOpts.EnableAccessControl &&
      !storage->isSetterAccessibleFrom(useDC))
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

Type TypeChecker::getUnopenedTypeOfReference(VarDecl *value, Type baseType,
                                             DeclContext *UseDC,
                                             const DeclRefExpr *base,
                                             bool wantInterfaceType) {
  validateDecl(value);
  if (!value->hasValidSignature() || value->isInvalid())
    return ErrorType::get(Context);

  Type requestedType = (wantInterfaceType
                        ? value->getInterfaceType()
                        : value->getType());

  requestedType = requestedType->getWithoutSpecifierType()
    ->getReferenceStorageReferent();

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
  if (doesStorageProduceLValue(*this, value, baseType, UseDC, base)) {
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
      (pathElts.back().getKind() == ConstraintLocator::Archetype ||
       pathElts.back().getKind() == ConstraintLocator::AssociatedType))
    return;

  // If the locator is empty, ignore it.
  if (!anchor && pathElts.empty())
    return;

  ConstraintLocator *locatorPtr = getConstraintLocator(locator);
  assert(locatorPtr && "No locator for opened types?");
  assert(std::find_if(OpenedTypes.begin(), OpenedTypes.end(),
                      [&](const std::pair<ConstraintLocator *,
                          ArrayRef<OpenedType>> &entry) {
                        return entry.first == locatorPtr;
                      }) == OpenedTypes.end() &&
         "already registered opened types for this locator");

  OpenedType* openedTypes
    = Allocator.Allocate<OpenedType>(replacements.size());
  std::copy(replacements.begin(), replacements.end(), openedTypes);
  OpenedTypes.push_back({ locatorPtr,
    llvm::makeArrayRef(openedTypes,
                       replacements.size()) });
}

/// Determine how many levels of argument labels should be removed from the
/// function type when referencing the given declaration.
static unsigned getNumRemovedArgumentLabels(TypeChecker &TC, ValueDecl *decl,
                                            bool isCurriedInstanceReference,
                                            FunctionRefKind functionRefKind) {
  unsigned numParameterLists;

  // Enum elements with associated values have to be treated
  // as regular function values.
  //
  // enum E {
  //   case foo(a: Int)
  // }
  // let bar: [Int] = []
  // bar.map(E.foo)
  //
  // `E.foo` has to act as a regular function type passed as a value.
  if (auto *EED = dyn_cast<EnumElementDecl>(decl)) {
    numParameterLists = EED->hasAssociatedValues() ? 2 : 1;

  // Only applicable to functions. Nothing else should have argument labels in
  // the type.
  } else if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
    numParameterLists = func->hasImplicitSelfDecl() ? 2 : 1;
  } else {
    return 0;
  }

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
                                     DeclContext *useDC,
                                     const DeclRefExpr *base) {
  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    OpenedTypeMap replacements;

    auto openedType = openFunctionType(
            func->getInterfaceType()->castTo<AnyFunctionType>(),
            /*numArgumentLabelsToRemove=*/0,
            locator, replacements,
            func->getInnermostDeclContext(),
            func->getDeclContext(),
            /*skipProtocolSelfConstraint=*/false);
    auto openedFnType = openedType->castTo<FunctionType>();

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    // If this is a method whose result type is dynamic Self, replace
    // DynamicSelf with the actual object type.
    if (!func->getDeclContext()->getAsProtocolOrProtocolExtensionContext()) {
      if (func->hasDynamicSelf()) {
        auto params = openedFnType->getParams();
        assert(params.size() == 1);
        Type selfTy = params.front().getType()->getRValueInstanceType();
        openedType = openedType->replaceCovariantResultType(selfTy, 2);
        openedFnType = openedType->castTo<FunctionType>();
      }
    } else {
      openedType = openedType->eraseDynamicSelfType();
      openedFnType = openedType->castTo<FunctionType>();
    }

    // The reference implicitly binds 'self'.
    return { openedType, openedFnType->getResult() };
  }

  // Unqualified reference to a local or global function.
  if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(value)) {
    OpenedTypeMap replacements;

    auto funcType = funcDecl->getInterfaceType()->castTo<AnyFunctionType>();
    auto openedType =
      openFunctionType(
        funcType,
        getNumRemovedArgumentLabels(TC, funcDecl,
                                    /*isCurriedInstanceReference=*/false,
                                    functionRefKind),
        locator, replacements,
        funcDecl->getInnermostDeclContext(),
        funcDecl->getDeclContext(),
        /*skipProtocolSelfConstraint=*/false);

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements);

    return { openedType, openedType };
  }

  // Unqualified reference to a type.
  if (auto typeDecl = dyn_cast<TypeDecl>(value)) {
    // Resolve the reference to this type declaration in our current context.
    auto type = TC.resolveTypeInContext(typeDecl, nullptr, useDC,
                                        TypeResolutionFlags::InExpression,
                                        /*isSpecialized=*/false);

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
  Type valueType = TC.getUnopenedTypeOfReference(varDecl, Type(), useDC, base,
                                                 wantInterfaceType);

  assert(!valueType->hasUnboundGenericType() &&
         !valueType->hasTypeParameter());

  // If this is a let-param whose type is a type variable, this is an untyped
  // closure param that may be bound to an inout type later. References to the
  // param should have lvalue type instead. Express the relationship with a new
  // constraint.
  if (auto *param = dyn_cast<ParamDecl>(varDecl)) {
    if (param->isLet() && valueType->is<TypeVariableType>()) {
      auto found = OpenedParameterTypes.find(param);
      if (found != OpenedParameterTypes.end())
        return { found->second, found->second };

      auto typeVar = createTypeVariable(getConstraintLocator(locator),
                                        TVO_CanBindToLValue);
      addConstraint(ConstraintKind::BindParam, valueType, typeVar,
                    getConstraintLocator(locator));
      OpenedParameterTypes.insert(std::make_pair(param, typeVar));
      return { typeVar, typeVar };
    }
  }

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

  auto *genericEnv = cs.DC->getGenericEnvironmentOfContext();

  auto bindContextArchetype = [&](Type paramTy, Type contextTy) {
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
      if (parentDC != outerDC && parentDC->getAsProtocolOrProtocolExtensionContext()) {
        auto selfTy = parentDC->getSelfInterfaceType();
        auto contextTy = cs.TC.Context.TheUnresolvedType;
        bindContextArchetype(selfTy, contextTy);
      }
      continue;
    }

    // If it's not generic, there's nothing to do.
    auto *genericSig = parentDC->getGenericSignatureOfContext();
    if (!genericSig)
      break;

    for (auto *paramTy : genericSig->getGenericParams()) {
      Type contextTy = genericEnv->mapTypeIntoContext(paramTy);
      bindContextArchetype(paramTy, contextTy);
    }

    break;
  }
}

void ConstraintSystem::openGeneric(
       DeclContext *innerDC,
       DeclContext *outerDC,
       GenericSignature *sig,
       bool skipProtocolSelfConstraint,
       ConstraintLocatorBuilder locator,
       OpenedTypeMap &replacements) {
  if (sig == nullptr)
    return;

  auto locatorPtr = getConstraintLocator(locator);
  auto *genericEnv = innerDC->getGenericEnvironmentOfContext();

  // Create the type variables for the generic parameters.
  for (auto gp : sig->getGenericParams()) {
    auto contextTy = GenericEnvironment::mapTypeIntoContext(genericEnv, gp);
    if (auto *archetype = contextTy->getAs<ArchetypeType>())
      locatorPtr = getConstraintLocator(
          locator.withPathElement(LocatorPathElt(archetype)));

    auto typeVar = createTypeVariable(locatorPtr,
                                      TVO_PrefersSubtypeBinding);
    auto result = replacements.insert(
      std::make_pair(cast<GenericTypeParamType>(gp->getCanonicalType()),
                     typeVar));
    assert(result.second);
    (void) result;
  }

  // Remember that any new constraints generated by opening this generic are
  // due to the opening.
  locatorPtr = getConstraintLocator(
                     locator.withPathElement(ConstraintLocator::OpenedGeneric));

  bindArchetypesFromContext(*this, outerDC, locatorPtr, replacements);

  // Add the requirements as constraints.
  openGenericRequirements(
      outerDC, sig, skipProtocolSelfConstraint, locator,
      [&](Type type) { return openType(type, replacements); });
}

void ConstraintSystem::openGenericRequirements(
    DeclContext *outerDC, GenericSignature *signature,
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

    addConstraint(
        *openedReq,
        locator.withPathElement(ConstraintLocator::OpenedGeneric)
            .withPathElement(LocatorPathElt::getTypeRequirementComponent(pos)));
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
  cs.addConstraint(ConstraintKind::Equal, objectTy, selfTy,
                   cs.getConstraintLocator(locator));
}

/// Determine whether the given locator is for a witness or requirement.
static bool isRequirementOrWitness(const ConstraintLocatorBuilder &locator) {
  if (auto last = locator.last()) {
    return last->getKind() == ConstraintLocator::Requirement ||
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
  bool isInstance = true;
  if (auto baseMeta = baseObjTy->getAs<AnyMetatypeType>()) {
    baseObjTy = baseMeta->getInstanceType();
    isInstance = false;
  }

  // If the base is a module type, just use the type of the decl.
  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReference(value, functionRefKind, locator, useDC, base);
  }

  // Don't open existentials when accessing typealias members of
  // protocols.
  if (auto *alias = dyn_cast<TypeAliasDecl>(value)) {
    if (baseObjTy->isExistentialType()) {
      auto memberTy = alias->getDeclaredInterfaceType();
      // If we end up with a protocol typealias here, it's underlying
      // type must be fully concrete.
      assert(!memberTy->hasTypeParameter());
      auto openedType = FunctionType::get(baseObjTy, memberTy);
      return { openedType, memberTy };
    }
  }

  if (auto *typeDecl = dyn_cast<TypeDecl>(value)) {
    assert(!isa<ModuleDecl>(typeDecl) && "Nested module?");

    auto memberTy = TC.substMemberTypeWithBase(DC->getParentModule(),
                                               typeDecl, baseObjTy);
    // Open the type if it was a reference to a generic type.
    memberTy = openUnboundGenericType(memberTy, locator);

    // Wrap it in a metatype.
    memberTy = MetatypeType::get(memberTy);

    auto openedType = FunctionType::get(baseObjTy, memberTy);
    return { openedType, memberTy };
  }

  // Figure out the declaration context to use when opening this type.
  DeclContext *innerDC = value->getInnermostDeclContext();
  DeclContext *outerDC = value->getDeclContext();

  // Open the type of the generic function or member of a generic type.
  Type openedType;
  OpenedTypeMap localReplacements;
  auto &replacements = replacementsPtr ? *replacementsPtr : localReplacements;
  bool isCurriedInstanceReference = value->isInstanceMember() && !isInstance;
  unsigned numRemovedArgumentLabels =
    getNumRemovedArgumentLabels(TC, value, isCurriedInstanceReference,
                                functionRefKind);

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

      if (doesStorageProduceLValue(TC, subscript, baseTy, useDC, base))
        elementTy = LValueType::get(elementTy);

      // See ConstraintSystem::resolveOverload() -- optional and dynamic
      // subscripts are a special case, because the optionality is
      // applied to the result type and not the type of the reference.
      if (!isRequirementOrWitness(locator)) {
        if (subscript->getAttrs().hasAttribute<OptionalAttr>() ||
            isDynamicResult)
          elementTy = OptionalType::get(elementTy->getRValueType());
      }

      auto indicesTy = subscript->getIndicesInterfaceType();
      refType = FunctionType::get(indicesTy, elementTy,
                                  AnyFunctionType::ExtInfo());
    } else {
      refType = TC.getUnopenedTypeOfReference(cast<VarDecl>(value),
                                              baseTy, useDC, base,
                                              /*wantInterfaceType=*/true);
    }

    auto selfTy = outerDC->getSelfInterfaceType();

    // If self is a value type and the base type is an lvalue, wrap it in an
    // inout type.
    auto selfFlags = ParameterTypeFlags();
    if (!outerDC->getDeclaredInterfaceType()->hasReferenceSemantics() &&
        baseTy->is<LValueType>() &&
        !selfTy->hasError())
      selfFlags = selfFlags.withInOut(true);

    // If the storage is generic, add a generic signature.
    auto selfParam = AnyFunctionType::Param(selfTy, Identifier(), selfFlags);
    if (auto *sig = innerDC->getGenericSignatureOfContext()) {
      funcType = GenericFunctionType::get(sig, {selfParam}, refType,
                                          AnyFunctionType::ExtInfo());
    } else {
      funcType = FunctionType::get({selfParam}, refType,
                                   AnyFunctionType::ExtInfo());
    }
  }

  openedType = openFunctionType(funcType, numRemovedArgumentLabels,
                                locator, replacements, innerDC, outerDC,
                                /*skipProtocolSelfConstraint=*/true);

  if (!outerDC->getAsProtocolOrProtocolExtensionContext()) {
    // Class methods returning Self as well as constructors get the
    // result replaced with the base object type.
    if (auto func = dyn_cast<AbstractFunctionDecl>(value)) {
      if ((isa<FuncDecl>(func) && cast<FuncDecl>(func)->hasDynamicSelf()) ||
          (isa<ConstructorDecl>(func) && !baseObjTy->getOptionalObjectType())) {
        openedType = openedType->replaceCovariantResultType(baseObjTy, 2);
      }
    }
  } else {
    // Protocol requirements returning Self have a dynamic Self return
    // type. Erase the dynamic Self since it only comes into play during
    // protocol conformance checking.
    openedType = openedType->eraseDynamicSelfType();
  }

  // If we are looking at a member of an existential, open the existential.
  Type baseOpenedTy = baseObjTy;

  if (baseObjTy->isExistentialType()) {
    ArchetypeType *openedArchetype = ArchetypeType::getOpened(baseObjTy);
    OpenedExistentialTypes.push_back({ getConstraintLocator(locator),
                                       openedArchetype });
    baseOpenedTy = openedArchetype;
  }

  // Constrain the 'self' object type.
  auto openedFnType = openedType->castTo<FunctionType>();
  auto openedParams = openedFnType->getParams();
  assert(openedParams.size() == 1);

  Type selfObjTy = openedParams.front().getType()->getRValueInstanceType();
  if (outerDC->getAsProtocolOrProtocolExtensionContext()) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Equal, baseOpenedTy, selfObjTy,
                  getConstraintLocator(locator));
  } else if (!isDynamicResult) {
    addSelfConstraint(*this, baseOpenedTy, selfObjTy, locator);
  }

  // Compute the type of the reference.
  Type type;
  if (!value->isInstanceMember() || isInstance) {
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
      outerDC->getAsProtocolOrProtocolExtensionContext()) {
    auto selfTy = replacements[
      cast<GenericTypeParamType>(outerDC->getSelfInterfaceType()
                                 ->getCanonicalType())];
    type = type.transform([&](Type t) -> Type {
      if (auto *selfTy = t->getAs<DynamicSelfType>())
        t = selfTy->getSelfType();
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

// Performance hack: if there are two generic overloads, and one is
// more specialized than the other, prefer the more-specialized one.
static void tryOptimizeGenericDisjunction(ConstraintSystem &cs,
                                          ArrayRef<OverloadChoice> choices,
                                          OverloadChoice *&favoredChoice) {
  if (favoredChoice || choices.size() != 2)
    return;

  const auto &choiceA = choices[0];
  const auto &choiceB = choices[1];

  if (!choiceA.isDecl() || !choiceB.isDecl())
    return;

  auto isViable = [](ValueDecl *decl) -> bool {
    assert(decl);

    auto *AFD = dyn_cast<AbstractFunctionDecl>(decl);
    if (!AFD || !AFD->isGeneric())
      return false;

    auto funcType = AFD->getInterfaceType();
    auto hasAnyOrOptional = funcType.findIf([](Type type) -> bool {
      if (auto objType = type->getOptionalObjectType())
        return true;

      return type->isAny();
    });

    // If function declaration references `Any` or `Any?` type
    // let's not attempt it, because it's unclear
    // without solving which overload is going to be better.
    return !hasAnyOrOptional;
  };

  auto *declA = choiceA.getDecl();
  auto *declB = choiceB.getDecl();

  if (!isViable(declA) || !isViable(declB))
    return;

  auto &TC = cs.TC;
  auto *DC = cs.DC;

  switch (TC.compareDeclarations(DC, declA, declB)) {
  case Comparison::Better:
    favoredChoice = const_cast<OverloadChoice *>(&choiceA);
    break;

  case Comparison::Worse:
    favoredChoice = const_cast<OverloadChoice *>(&choiceB);
    break;

  case Comparison::Unordered:
    break;
  }
}

void ConstraintSystem::addOverloadSet(Type boundType,
                                      ArrayRef<OverloadChoice> choices,
                                      DeclContext *useDC,
                                      ConstraintLocator *locator,
                                      OverloadChoice *favoredChoice,
                                      ArrayRef<OverloadChoice> outerAlternatives) {
  assert(!choices.empty() && "Empty overload set");

  // If there is a single choice, add the bind overload directly.
  if (choices.size() == 1 && outerAlternatives.empty()) {
    addBindOverloadConstraint(boundType, choices.front(), locator, useDC);
    return;
  }

  tryOptimizeGenericDisjunction(*this, choices, favoredChoice);

  SmallVector<Constraint *, 4> overloads;
  
  // As we do for other favored constraints, if a favored overload has been
  // specified, let it be the first term in the disjunction.
  if (favoredChoice) {
    auto bindOverloadConstraint =
        Constraint::createBindOverload(*this,
                                       boundType,
                                       *favoredChoice,
                                       useDC,
                                       locator);

    assert((!favoredChoice->isDecl() ||
            !favoredChoice->getDecl()->getAttrs().isUnavailable(
                getASTContext())) &&
           "Cannot make unavailable decl favored!");
    bindOverloadConstraint->setFavored();
    
    overloads.push_back(bindOverloadConstraint);
  }
  
  for (auto &choice : choices) {
    if (favoredChoice && (favoredChoice == &choice))
      continue;
    
    overloads.push_back(Constraint::createBindOverload(*this, boundType, choice,
                                                       useDC, locator));
  }

  auto innerDisjunction = Constraint::createDisjunction(*this, overloads,
                                                        locator, ForgetChoice);
  if (outerAlternatives.empty()) {
    if (favoredChoice)
      innerDisjunction->setFavored();

    addUnsolvedConstraint(innerDisjunction);
    return;
  }

  SmallVector<Constraint *, 4> outerConstraints;
  outerConstraints.push_back(innerDisjunction);
  innerDisjunction->setFavored();
  for (auto choice : outerAlternatives) {
    outerConstraints.push_back(Constraint::createBindOverload(
                                 *this, boundType, choice,
                                   useDC, locator));
  }

  addDisjunctionConstraint(outerConstraints, locator, ForgetChoice, favoredChoice);
}

/// If we're resolving an overload set with a decl that has special type
/// checking semantics, set up the special-case type system and return true;
/// otherwise return false.
static bool
resolveOverloadForDeclWithSpecialTypeCheckingSemantics(ConstraintSystem &CS,
                                                     ConstraintLocator *locator,
                                                     Type boundType,
                                                     OverloadChoice choice,
                                                     Type &refType,
                                                     Type &openedFullType) {
  assert(choice.getKind() == OverloadChoiceKind::Decl);

  switch (CS.TC.getDeclTypeCheckingSemantics(choice.getDecl())) {
  case DeclTypeCheckingSemantics::Normal:
    return false;
    
  case DeclTypeCheckingSemantics::TypeOf: {
    // Proceed with a "DynamicType" operation. This produces an existential
    // metatype from existentials, or a concrete metatype from non-
    // existentials (as seen from the current abstraction level), which can't
    // be expressed in the type system currently.
    auto input = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    auto output = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult));

    auto inputArg = TupleTypeElt(input, CS.getASTContext().getIdentifier("of"));
    auto inputTuple = TupleType::get(inputArg, CS.getASTContext());
    
    CS.addConstraint(ConstraintKind::DynamicTypeOf, output, input,
        CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    refType = FunctionType::get(inputTuple, output);
    openedFullType = refType;
    return true;
  }
  case DeclTypeCheckingSemantics::WithoutActuallyEscaping: {
    // Proceed with a "WithoutActuallyEscaping" operation. The body closure
    // receives a copy of the argument closure that is temporarily made
    // @escaping.
    auto noescapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    auto escapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    CS.addConstraint(ConstraintKind::EscapableFunctionOf,
         escapeClosure, noescapeClosure,
         CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult));
    auto bodyClosure = FunctionType::get(
      ParenType::get(CS.getASTContext(), escapeClosure), result,
        FunctionType::ExtInfo(FunctionType::Representation::Swift,
                              /*autoclosure*/ false,
                              /*noescape*/ true,
                              /*throws*/ true));
    TupleTypeElt argTupleElts[] = {
      TupleTypeElt(noescapeClosure),
      TupleTypeElt(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };
    
    auto argTuple = TupleType::get(argTupleElts, CS.getASTContext());
    refType = FunctionType::get(argTuple, result,
      FunctionType::ExtInfo(FunctionType::Representation::Swift,
                            /*autoclosure*/ false,
                            /*noescape*/ false,
                            /*throws*/ true));
    openedFullType = refType;
    return true;
  }
  case DeclTypeCheckingSemantics::OpenExistential: {
    // The body closure receives a freshly-opened archetype constrained by the
    // existential type as its input.
    auto openedTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    auto existentialTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    CS.addConstraint(ConstraintKind::OpenedExistentialOf,
         openedTy, existentialTy,
         CS.getConstraintLocator(locator, ConstraintLocator::RValueAdjustment));
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult));
    auto bodyClosure = FunctionType::get(
      ParenType::get(CS.getASTContext(), openedTy), result,
        FunctionType::ExtInfo(FunctionType::Representation::Swift,
                              /*autoclosure*/ false,
                              /*noescape*/ true,
                              /*throws*/ true));
    TupleTypeElt argTupleElts[] = {
      TupleTypeElt(existentialTy),
      TupleTypeElt(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };
    auto argTuple = TupleType::get(argTupleElts, CS.getASTContext());
    refType = FunctionType::get(argTuple, result,
      FunctionType::ExtInfo(FunctionType::Representation::Swift,
                            /*autoclosure*/ false,
                            /*noescape*/ false,
                            /*throws*/ true));
    openedFullType = refType;
    return true;
  }
  }

  llvm_unreachable("Unhandled DeclTypeCheckingSemantics in switch.");
}

void ConstraintSystem::resolveOverload(ConstraintLocator *locator,
                                       Type boundType,
                                       OverloadChoice choice,
                                       DeclContext *useDC) {
  // Determine the type to which we'll bind the overload set's type.
  Type refType;
  Type openedFullType;

  bool isDynamicResult = choice.getKind() == OverloadChoiceKind::DeclViaDynamic;
  bool bindConstraintCreated = false;

  switch (auto kind = choice.getKind()) {
  case OverloadChoiceKind::Decl:
    // If we refer to a top-level decl with special type-checking semantics,
    // handle it now.
    if (resolveOverloadForDeclWithSpecialTypeCheckingSemantics(
          *this, locator, boundType, choice, refType, openedFullType))
      break;
    
    LLVM_FALLTHROUGH;

  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::DynamicMemberLookup: {
    // Retrieve the type of a reference to the specific declaration choice.
    if (auto baseTy = choice.getBaseType()) {
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
                                   isDynamicResult,
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
    if (isDynamicResult) {
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
          Type ty = createTypeVariable(locator);

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
        refType = OptionalType::get(refType->getRValueType());
      }

      bindConstraintCreated = true;
    } else if (!isRequirementOrWitness(locator) &&
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
      if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
        // Build the disjunction to attempt binding both T? and T (or
        // function returning T? and function returning T).
        Type ty = createTypeVariable(locator, TVO_CanBindToLValue);
        buildDisjunctionForImplicitlyUnwrappedOptional(ty, refType, locator);
        addConstraint(ConstraintKind::Bind, boundType,
                      OptionalType::get(ty->getRValueType()), locator);
        bindConstraintCreated = true;
      }

      refType = OptionalType::get(refType->getRValueType());
    }
    // If the declaration is unavailable, note that in the score.
    if (choice.getDecl()->getAttrs().isUnavailable(getASTContext())) {
      increaseScore(SK_Unavailable);
    }

    if (kind == OverloadChoiceKind::DynamicMemberLookup) {
      // DynamicMemberLookup results are always a (dynamicMember:T1)->T2
      // subscript.
      auto refFnType = refType->castTo<FunctionType>();

      // If this is a dynamic member lookup, then the decl we have is for the
      // subscript(dynamicMember:) member, but the type we need to return is the
      // result of the subscript.  Dig through it.
      refType = refFnType->getResult();

      // Before we drop the argument type on the floor, we need to constrain it
      // to having a literal conformance to ExpressibleByStringLiteral.  This
      // makes the index default to String if otherwise unconstrained.
      assert(refFnType->getParams().size() == 1 &&
             "subscript always has one arg");
      auto argType = refFnType->getParams()[0].getPlainType();
      
      auto protoKind = KnownProtocolKind::ExpressibleByStringLiteral;
      auto protocol = getTypeChecker().getProtocol(choice.getDecl()->getLoc(),
                                                   protoKind);
      if (!protocol)
        break;
      addConstraint(ConstraintKind::LiteralConformsTo, argType,
                    protocol->getDeclaredType(),
                    locator);
    }
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
        getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    auto elementTy = createTypeVariable(
            getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
            TVO_CanBindToLValue);
    auto elementObjTy = createTypeVariable(
        getConstraintLocator(locator, ConstraintLocator::FunctionArgument));
    addConstraint(ConstraintKind::Equal, elementTy, elementObjTy, locator);
    
    // The element result is an lvalue or rvalue based on the key path class.
    addKeyPathApplicationConstraint(
                  keyPathIndexTy, choice.getBaseType(), elementTy, locator);
    
    TupleTypeElt indexTupleElts[] = {
      TupleTypeElt(keyPathIndexTy, getASTContext().Id_keyPath),
    };
    auto indexTuple = TupleType::get(indexTupleElts, getASTContext());
    auto subscriptTy = FunctionType::get(indexTuple, elementTy);
    auto fullTy = FunctionType::get(choice.getBaseType(), subscriptTy);
    openedFullType = fullTy;
    refType = subscriptTy;

    // Increase the score so that actual subscripts get preference.
    increaseScore(SK_KeyPathSubscript);
    break;
  }
  }
  assert(!refType->hasTypeParameter() && "Cannot have a dependent type here");
  
  // If we're binding to an init member, the 'throws' need to line up between
  // the bound and reference types.
  if (choice.isDecl()) {
    auto decl = choice.getDecl();
    if (auto CD = dyn_cast<ConstructorDecl>(decl)) {
      auto boundFunctionType = boundType->getAs<AnyFunctionType>();
        
      if (boundFunctionType &&
          CD->hasThrows() != boundFunctionType->throws()) {
        boundType = boundFunctionType->withExtInfo(
            boundFunctionType->getExtInfo().withThrows());
      }
    }
  }

  // Note that we have resolved this overload.
  resolvedOverloadSets
    = new (*this) ResolvedOverloadSetListItem{resolvedOverloadSets,
                                              boundType,
                                              choice,
                                              locator,
                                              openedFullType,
                                              refType};

  // In some cases we already created the appropriate bind constraints.
  if (!bindConstraintCreated) {
    if (choice.isImplicitlyUnwrappedValueOrReturnValue()) {
      // Build the disjunction to attempt binding both T? and T (or
      // function returning T? and function returning T).
      buildDisjunctionForImplicitlyUnwrappedOptional(boundType, refType,
                                                     locator);
    } else {
      // Add the type binding constraint.
      addConstraint(ConstraintKind::Bind, boundType, refType, locator);
    }
  }

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log.indent(solverState ? solverState->depth * 2 : 2)
      << "(overload set choice binding "
      << boundType->getString() << " := "
      << refType->getString() << ")\n";
  }
}

template <typename Fn>
Type simplifyTypeImpl(ConstraintSystem &cs, Type type, Fn getFixedTypeFn) {
  return type.transform([&](Type type) -> Type {
    if (auto tvt = dyn_cast<TypeVariableType>(type.getPointer()))
      return getFixedTypeFn(tvt);

    // If this is a dependent member type for which we end up simplifying
    // the base to a non-type-variable, perform lookup.
    if (auto depMemTy = dyn_cast<DependentMemberType>(type.getPointer())) {
      // Simplify the base.
      Type newBase = simplifyTypeImpl(cs, depMemTy->getBase(), getFixedTypeFn);

      // If nothing changed, we're done.
      if (newBase.getPointer() == depMemTy->getBase().getPointer())
        return type;

      // Dependent member types should only be created for associated types.
      auto assocType = depMemTy->getAssocType();
      assert(depMemTy->getAssocType() && "Expected associated type!");

      // FIXME: It's kind of weird in general that we have to look
      // through lvalue, inout and IUO types here
      Type lookupBaseType = newBase->getWithoutSpecifierType();

      if (lookupBaseType->mayHaveMembers()) {
        auto *proto = assocType->getProtocol();
        auto conformance = cs.DC->getParentModule()->lookupConformance(
          lookupBaseType, proto);
        if (!conformance)
          return DependentMemberType::get(lookupBaseType, assocType);

        auto subs = SubstitutionMap::getProtocolSubstitutions(
          proto, lookupBaseType, *conformance);
        auto result = assocType->getDeclaredInterfaceType().subst(subs);

        if (result && !result->hasError())
          return result;
      }

      return DependentMemberType::get(lookupBaseType, assocType);
    }

    return type;
  });
}

Type ConstraintSystem::simplifyType(Type type) {
  if (!type->hasTypeVariable())
    return type;

  // Map type variables down to the fixed types of their representatives.
  return simplifyTypeImpl(
      *this, type,
      [&](TypeVariableType *tvt) -> Type {
        tvt = getRepresentative(tvt);
        if (auto fixed = getFixedType(tvt)) {
          return simplifyType(fixed);
        }

        return tvt;
      });
}

Type Solution::simplifyType(Type type) const {
  if (!type->hasTypeVariable())
    return type;

  // Map type variables to fixed types from bindings.
  return simplifyTypeImpl(
      getConstraintSystem(), type,
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
         llvm::capacity_in_bytes(Conformances);
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
      return DeclName(DynamicNameAndFRK.getPointer());
      
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
  if (!decl->getAttrs().hasAttribute<ImplicitlyUnwrappedOptionalAttr>())
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
}
