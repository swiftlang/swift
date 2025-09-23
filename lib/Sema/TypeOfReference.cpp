//===--- TypeOfReference.cpp - Opening interface types --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements getTypeOfReference(), getTypeOfMemberReference(), and
// friends. These entry points take a ValueDecl, and replace generic parameters
// with type variables in its interface type, and record constraints for the
// ValueDecl's requirements.
//
//===----------------------------------------------------------------------===//
#include "OpenedExistentials.h"
#include "TypeCheckConcurrency.h"
#include "TypeCheckMacros.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Effects.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeTransform.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/PreparedOverload.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/Defer.h"

using namespace swift;
using namespace constraints;
using namespace inference;

#define DEBUG_TYPE "ConstraintSystem"



Type ConstraintSystem::openUnboundGenericType(GenericTypeDecl *decl,
                                              Type parentTy,
                                              ConstraintLocatorBuilder locator,
                                              bool isTypeResolution,
                                              PreparedOverloadBuilder *preparedOverload) {
  if (parentTy) {
    parentTy = replaceInferableTypesWithTypeVars(
        parentTy, locator, preparedOverload);
  }

  // Open up the generic type.
  SmallVector<OpenedType, 4> replacements;
  openGeneric(decl->getDeclContext(), decl->getGenericSignature(), locator,
              replacements, preparedOverload);

  // FIXME: Get rid of fixmeAllowDuplicates.
  recordOpenedTypes(locator, replacements, preparedOverload,
                    /*fixmeAllowDuplicates=*/true);

  if (parentTy) {
    const auto parentTyInContext =
        isTypeResolution
            // Type resolution produces interface types, so we have to map
            // the parent type into context before binding type variables.
            ? DC->mapTypeIntoContext(parentTy)
            : parentTy;

    const auto subs =
        parentTyInContext->getContextSubstitutions(decl->getDeclContext());
    for (auto pair : replacements) {
      auto found = subs.find(
        cast<GenericTypeParamType>(pair.first->getCanonicalType()));
      if (found == subs.end())
        continue;

      // When a nominal type is declared in generic local context (which is
      // not actually valid anyway), the context substitution map will map
      // the outer generic parameters to themselves. Skip such entries to
      // avoid introducing constraints that contain type parameters into
      // the solver.
      if (found->second->hasTypeParameter())
        continue;

      addConstraint(ConstraintKind::Bind, pair.second, found->second,
                    locator, /*isFavored=*/false, preparedOverload);
    }
  }

  // Map the generic parameters to their corresponding type variables.
  llvm::SmallVector<Type, 2> arguments;
  for (auto gp : decl->getInnermostGenericParamTypes()) {
    auto found = llvm::find_if(replacements, [&](OpenedType pair) {
      return pair.first->isEqual(gp);
    });
    assert(found != replacements.end() &&
           "Missing generic parameter?");
    arguments.push_back(found->second);
  }

  // FIXME: For some reason we can end up with unbound->getDecl()
  // pointing at a generic TypeAliasDecl here. If we find a way to
  // handle generic TypeAliases elsewhere, this can just become a
  // call to BoundGenericType::get().
  auto result =
      TypeResolution::forInterface(
          DC, std::nullopt,
          [](auto) -> Type { llvm_unreachable("should not be used"); },
          [](auto &, auto) -> Type { llvm_unreachable("should not be used"); },
          [](auto, auto) -> Type { llvm_unreachable("should not be used"); },
          [](auto, auto) { /*will be called, but we already handled reqs*/ })
          .applyUnboundGenericArguments(decl, parentTy, SourceLoc(), arguments);
  if (!parentTy && !isTypeResolution) {
    result = DC->mapTypeIntoContext(result);
  }

  return result;
}

static void checkNestedTypeConstraints(ConstraintSystem &cs, Type type,
                                       ConstraintLocatorBuilder locator,
                                       PreparedOverloadBuilder *preparedOverload) {
  // If this is a type defined inside of constrained extension, let's add all
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
        extension->getSelfNominalTypeDecl());
    if (!subMap) {
      // The substitution map wasn't set above, meaning we should grab the map
      // for the extension itself.
      subMap = parentTy->getContextSubstitutionMap(extension);
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
          }, preparedOverload);
    }
  }

  // And now make sure the parent is okay, for things like X<T>.Y.Z.
  checkNestedTypeConstraints(cs, parentTy, locator, preparedOverload);
}

namespace {
class InferableTypeOpener final {
  ConstraintSystem &cs;
  ConstraintLocatorBuilder locator;
  PreparedOverloadBuilder *preparedOverload;

public:
  InferableTypeOpener(ConstraintSystem &cs, ConstraintLocatorBuilder locator,
                      PreparedOverloadBuilder *preparedOverload)
      : cs(cs), locator(locator), preparedOverload(preparedOverload) {}

  TypeVariableType *createTypeVariable(ConstraintLocator *loc) {
    auto flags =
        TVO_CanBindToNoEscape | TVO_PrefersSubtypeBinding | TVO_CanBindToHole;
    return cs.createTypeVariable(loc, flags, preparedOverload);
  }

  Type transformUnboundGenericType(UnboundGenericType *unbound) {
    return cs.openUnboundGenericType(unbound->getDecl(), unbound->getParent(),
                                     locator, /*isTypeResolution=*/false,
                                     preparedOverload);
  }

  Type transformPlaceholderType(PlaceholderType *placeholderTy) {
    auto originator = placeholderTy->getOriginator();
    if (auto *typeRepr = originator.dyn_cast<TypeRepr *>()) {
      if (isa<PlaceholderTypeRepr>(typeRepr)) {
        auto *loc = cs.getConstraintLocator(
            locator, LocatorPathElt::PlaceholderType(typeRepr));
        return createTypeVariable(loc);
      }
    } else if (auto *var = originator.dyn_cast<VarDecl *>()) {
      if (var->getName().hasDollarPrefix()) {
        auto *repr =
            new (cs.getASTContext()) PlaceholderTypeRepr(var->getLoc());
        auto *loc = cs.getConstraintLocator(
            locator, LocatorPathElt::PlaceholderType(repr));
        return createTypeVariable(loc);
      }
    }
    return placeholderTy;
  }

  void openGenericTypeRequirements(GenericTypeDecl *D, SubstitutionMap subs) {
    OpenGenericTypeRequirements openReqs(cs, locator, preparedOverload);
    openReqs(D, QuerySubstitutionMap{subs});
  }

  Type transformBoundGenericType(BoundGenericType *genericTy) {
    SmallVector<Type, 4> genericArgs;
    for (auto arg : genericTy->getGenericArgs())
      genericArgs.push_back(transform(arg));

    auto substTy = BoundGenericType::get(
        genericTy->getDecl(), transform(genericTy->getParent()), genericArgs);

    openGenericTypeRequirements(substTy->getDecl(),
                                substTy->getContextSubstitutionMap());
    return substTy;
  }

  Type transformTypeAliasType(TypeAliasType *aliasTy) {
    SmallVector<Type, 4> genericArgs;
    for (auto arg : aliasTy->getDirectGenericArgs())
      genericArgs.push_back(transform(arg));

    auto substTy = TypeAliasType::get(
        aliasTy->getDecl(), transform(aliasTy->getParent()), genericArgs,
        transform(aliasTy->getSinglyDesugaredType()));
    openGenericTypeRequirements(substTy->getDecl(),
                                substTy->getSubstitutionMap());
    return substTy;
  }

  Type transformErrorType(ErrorType *errTy) {
    // For ErrorTypes we want to eagerly produce a hole since we know this is
    // where the issue is.
    return PlaceholderType::get(cs.getASTContext(), errTy);
  }

  Type transformDependentMemberType(DependentMemberType *DMT) {
    auto assocTy = DMT->getAssocType();
    ASSERT(assocTy && "Should not have structural type here");

    // If the new base is a hole, propagate it to cover the entire
    // DependentMemberType.
    auto newBase = transform(DMT->getBase());
    if (newBase->isPlaceholder())
      return PlaceholderType::get(cs.getASTContext(), DMT);

    return DependentMemberType::get(newBase, assocTy);
  }

  Type transform(Type type) {
    if (!type)
      return type;
    if (!type->hasUnboundGenericType() && !type->hasPlaceholder() &&
        !type->hasError()) {
      return type;
    }
    // If the type has an error, we can't apply the solution. Record a fix for
    // an invalid AST node.
    if (type->hasError()) {
      cs.recordFix(
          IgnoreInvalidASTNode::create(cs, cs.getConstraintLocator(locator)),
          /*impact=*/1, preparedOverload);
    }
    return type.transformRec([&](Type type) -> std::optional<Type> {
      if (!type->hasUnboundGenericType() && !type->hasPlaceholder() &&
          !type->hasError()) {
        return type;
      }
      auto *tyPtr = type.getPointer();
      if (auto unbound = dyn_cast<UnboundGenericType>(tyPtr))
        return transformUnboundGenericType(unbound);
      if (auto *placeholderTy = dyn_cast<PlaceholderType>(tyPtr))
        return transformPlaceholderType(placeholderTy);
      if (auto *genericTy = dyn_cast<BoundGenericType>(tyPtr))
        return transformBoundGenericType(genericTy);
      if (auto *aliasTy = dyn_cast<TypeAliasType>(tyPtr))
        return transformTypeAliasType(aliasTy);
      if (auto *errTy = dyn_cast<ErrorType>(tyPtr))
        return transformErrorType(errTy);
      if (auto *DMT = dyn_cast<DependentMemberType>(tyPtr))
        return transformDependentMemberType(DMT);

      return std::nullopt;
    });
  }
};
} // end anonymous namespace

Type ConstraintSystem::replaceInferableTypesWithTypeVars(
    Type type, ConstraintLocatorBuilder locator,
    PreparedOverloadBuilder *preparedOverload) {
  InferableTypeOpener opener(*this, locator, preparedOverload);
  return opener.transform(type);
}

namespace {

struct TypeOpener : public TypeTransform<TypeOpener> {
  ArrayRef<OpenedType> replacements;
  ConstraintLocatorBuilder locator;
  PreparedOverloadBuilder *preparedOverload;
  ConstraintSystem &cs;

  TypeOpener(ArrayRef<OpenedType> replacements,
             ConstraintLocatorBuilder locator,
             PreparedOverloadBuilder *preparedOverload,
             ConstraintSystem &cs)
      : TypeTransform<TypeOpener>(cs.getASTContext()),
        replacements(replacements), locator(locator),
        preparedOverload(preparedOverload), cs(cs) {}

  std::optional<Type> transform(TypeBase *type, TypePosition pos) {
    if (!type->hasTypeParameter())
      return Type(type);

    return std::nullopt;
  }

  Type transformGenericTypeParamType(GenericTypeParamType *genericParam,
                                     TypePosition pos) {
    for (auto pair : replacements) {
      if (pair.first->isEqual(genericParam))
        return pair.second;
    }

    // FIXME: This should be an assert, however protocol generic signatures
    // drop outer generic parameters.
    return ErrorType::get(ctx);
  }

  Type transformPackExpansionType(PackExpansionType *expansion,
                                  TypePosition pos) {
    return cs.openPackExpansionType(expansion, replacements, locator,
                                    preparedOverload);
  }

  bool shouldUnwrapVanishingTuples() const {
    return false;
  }

  bool shouldDesugarTypeAliases() const {
    return true;
  }
};

}

Type ConstraintSystem::openType(Type type, ArrayRef<OpenedType> replacements,
                                ConstraintLocatorBuilder locator,
                                PreparedOverloadBuilder *preparedOverload) {
  assert(!type->hasUnboundGenericType());

  if (!type->hasTypeParameter())
    return type;

  return TypeOpener(replacements, locator, preparedOverload, *this)
      .doIt(type, TypePosition::Invariant);
}

Type ConstraintSystem::openPackExpansionType(PackExpansionType *expansion,
                                             ArrayRef<OpenedType> replacements,
                                             ConstraintLocatorBuilder locator,
                                             PreparedOverloadBuilder *preparedOverload) {
  auto patternType =
      openType(expansion->getPatternType(), replacements, locator,
               preparedOverload);
  auto shapeType = openType(expansion->getCountType(), replacements, locator,
                            preparedOverload);

  auto openedPackExpansion = PackExpansionType::get(patternType, shapeType);

  // FIXME: It's silly that we need to do this. The whole concept of
  // "opening" pack expansions is broken.
  {
    if (preparedOverload) {
      for (auto change : preparedOverload->Changes) {
        if (change.Kind == PreparedOverload::Change::OpenedPackExpansionType &&
            change.PackExpansion.TheExpansion == openedPackExpansion)
          return change.PackExpansion.TypeVar;
      }
    }

    auto known = OpenedPackExpansionTypes.find(openedPackExpansion);
    if (known != OpenedPackExpansionTypes.end())
      return known->second;
  }

  auto *expansionLoc = getConstraintLocator(locator.withPathElement(
      LocatorPathElt::PackExpansionType(openedPackExpansion)));

  auto *expansionVar = createTypeVariable(expansionLoc, TVO_PackExpansion,
                                          preparedOverload);

  // This constraint is important to make sure that pack expansion always
  // has a binding and connect pack expansion var to any type variables
  // that appear in pattern and shape types.
  auto *c = Constraint::create(*this, ConstraintKind::FallbackType,
                               expansionVar, openedPackExpansion,
                               expansionLoc);
  if (preparedOverload)
    preparedOverload->addedConstraint(c);
  else
    addUnsolvedConstraint(c);

  recordOpenedPackExpansionType(openedPackExpansion, expansionVar,
                                preparedOverload);
  return expansionVar;
}

void ConstraintSystem::recordOpenedPackExpansionType(PackExpansionType *expansion,
                                                     TypeVariableType *expansionVar,
                                                     PreparedOverloadBuilder *preparedOverload) {
  if (preparedOverload) {
    ASSERT(PreparingOverload);
    preparedOverload->openedPackExpansionType(expansion, expansionVar);
    return;
  }

  ASSERT(!PreparingOverload);

  bool inserted = OpenedPackExpansionTypes.insert({expansion, expansionVar}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedOpenedPackExpansionType(expansion));
}

Type ConstraintSystem::openOpaqueType(OpaqueTypeArchetypeType *opaque,
                                      ConstraintLocatorBuilder locator) {
  auto opaqueDecl = opaque->getDecl();
  auto opaqueLocatorKey = getOpenOpaqueLocator(locator, opaqueDecl);

  // If we have already opened this opaque type, look in the known set of
  // replacements.
  auto knownReplacements = OpenedTypes.find(
      getConstraintLocator(opaqueLocatorKey));
  if (knownReplacements != OpenedTypes.end()) {
    auto param = opaque->getInterfaceType()->castTo<GenericTypeParamType>();
    for (const auto &replacement : knownReplacements->second) {
      if (replacement.first->isEqual(param))
        return replacement.second;
    }

    llvm_unreachable("Missing opaque type replacement");
  }

  // Open the generic signature of the opaque decl, and bind the "outer" generic
  // params to our context. The remaining axes of freedom on the type variable
  // corresponding to the underlying type should be the constraints on the
  // underlying return type.
  auto opaqueLocator = locator.withPathElement(
      LocatorPathElt::OpenedOpaqueArchetype(opaqueDecl));

  SmallVector<OpenedType, 4> replacements;
  openGeneric(DC, opaqueDecl->getOpaqueInterfaceGenericSignature(),
              opaqueLocator, replacements, /*preparedOverload=*/nullptr);

  recordOpenedTypes(opaqueLocatorKey, replacements,
                    /*preparedOverload=*/nullptr);

  return openType(opaque->getInterfaceType(), replacements, locator,
                  /*preparedOverload=*/nullptr);
}

Type ConstraintSystem::openOpaqueType(Type type, ContextualTypePurpose context,
                                      ConstraintLocatorBuilder locator,
                                      Decl *ownerDecl) {
  // FIXME: Require non-null ownerDecl and fix remaining callers
  // ASSERT(ownerDecl);

  // Early return if `type` is `NULL` or if there are no opaque archetypes (in
  // which case there is certainly nothing for us to do).
  if (!type || !type->hasOpaqueArchetype())
    return type;

  if (context != CTP_Initialization && context != CTP_ReturnStmt)
    return type;

  auto shouldOpen = [&](OpaqueTypeArchetypeType *opaqueType) {
    if (context == CTP_Initialization) {
      if (!ownerDecl)
        return true;

      return opaqueType->getDecl()->isOpaqueReturnTypeOf(ownerDecl);
    } else {
      if (auto *func = dyn_cast<AbstractFunctionDecl>(DC))
        return opaqueType->getDecl()->isOpaqueReturnTypeOf(func);

      return true;
    }
  };

  return type.transformRec([&](Type type) -> std::optional<Type> {
    auto *opaqueType = type->getAs<OpaqueTypeArchetypeType>();

    if (opaqueType && shouldOpen(opaqueType))
      return openOpaqueType(opaqueType, locator);

    return std::nullopt;
  });
}

/// FIXME: This can be folded into its callers after a bit of cleanup.
static FunctionType *substGenericArgs(
    GenericFunctionType *funcTy,
    llvm::function_ref<Type(Type)> substFn) {
  llvm::SmallVector<AnyFunctionType::Param, 4> params;
  params.reserve(funcTy->getNumParams());

  llvm::transform(funcTy->getParams(), std::back_inserter(params),
                  [&](const AnyFunctionType::Param &param) {
                    return param.withType(substFn(param.getPlainType()));
                  });

  auto resultTy = substFn(funcTy->getResult());

  Type thrownError = funcTy->getThrownError();
  if (thrownError)
    thrownError = substFn(thrownError);

  // Build the resulting (non-generic) function type.
  return FunctionType::get(params, resultTy,
                           funcTy->getExtInfo().withThrows(
                              funcTy->isThrowing(), thrownError));
}

FunctionType *ConstraintSystem::openFunctionType(
       AnyFunctionType *funcType,
       ConstraintLocatorBuilder locator,
       SmallVectorImpl<OpenedType> &replacements,
       DeclContext *outerDC,
       PreparedOverloadBuilder *preparedOverload) {
  if (auto *genericFn = funcType->getAs<GenericFunctionType>()) {
    auto signature = genericFn->getGenericSignature();
    openGenericParameters(outerDC, signature, replacements, locator,
                          preparedOverload);

    openGenericRequirements(outerDC, signature,
                            /*skipProtocolSelfConstraint=*/false, locator,
                            [&](Type type) -> Type {
                              return openType(type, replacements, locator,
                                              preparedOverload);
                            }, preparedOverload);

    funcType = substGenericArgs(genericFn,
        [&](Type type) {
          return openType(type, replacements, locator, preparedOverload);
        });
  }

  return funcType->castTo<FunctionType>();
}

static bool isInLeftHandSideOfAssignment(ConstraintSystem &cs, Expr *expr) {
  // Walk up the parent tree.
  auto parent = cs.getParentExpr(expr);
  if (!parent)
    return false;

  // If the parent is an assignment expression, check whether we are
  // the left-hand side.
  if (auto assignParent = dyn_cast<AssignExpr>(parent)) {
    return expr == assignParent->getDest();
  }

  // Always look up through these parent kinds.
  if (isa<TupleExpr>(parent) || isa<IdentityExpr>(parent) ||
      isa<AnyTryExpr>(parent) || isa<BindOptionalExpr>(parent)) {
    return isInLeftHandSideOfAssignment(cs, parent);
  }

  // If the parent is a lookup expression, only follow from the base.
  if (auto lookupParent = dyn_cast<LookupExpr>(parent)) {
    if (lookupParent->getBase() == expr)
      return isInLeftHandSideOfAssignment(cs, parent);

    // The expression wasn't in the base, so this isn't part of the
    // left-hand side.
    return false;
  }

  // If the parent is an unresolved member reference a.b, only follow
  // from the base.
  if (auto dotParent = dyn_cast<UnresolvedDotExpr>(parent)) {
    if (dotParent->getBase() == expr)
      return isInLeftHandSideOfAssignment(cs, parent);

    // The expression wasn't in the base, so this isn't part of the
    // left-hand side.
    return false;
  }

  return false;
}

/// Does a var or subscript produce an l-value?
///
/// \param baseType - the type of the base on which this object
///   is being accessed; must be null if and only if this is not
///   a type member
static bool doesStorageProduceLValue(
    AbstractStorageDecl *storage, Type baseType,
    DeclContext *useDC,
    ConstraintSystem &cs,
    ConstraintLocator *locator) {
  const DeclRefExpr *base = nullptr;
  if (locator) {
    if (auto *const E = getAsExpr(locator->getAnchor())) {
      if (auto *MRE = dyn_cast<MemberRefExpr>(E)) {
        base = dyn_cast<DeclRefExpr>(MRE->getBase());
      } else if (auto *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
        base = dyn_cast<DeclRefExpr>(UDE->getBase());
      }
    }
  }

  switch (storage->mutabilityInSwift(useDC, base)) {
    case StorageMutability::Immutable:
      // Immutable storage decls always produce rvalues.
      return false;

    case StorageMutability::Mutable:
      break;

    case StorageMutability::Initializable: {
      // If the member is not known to be on the left-hand side of
      // an assignment, treat it as an rvalue so we can't pass it
      // inout.
      if (!locator)
        return false;

      auto *anchor = getAsExpr(simplifyLocatorToAnchor(locator));
      if (!anchor)
        return false;

      // If the anchor isn't known to be the target of an assignment,
      // treat as immutable.
      if (!isInLeftHandSideOfAssignment(cs, anchor))
        return false;

      break;
    }
  }

  if (!storage->isSetterAccessibleFrom(useDC))
    return false;

  // This path handles storage that is mutable in the given context.

  if (!baseType) {
    return true;
  }

  {
    const auto rValueInstanceTy =
        baseType->getRValueType()->getMetatypeInstanceType();
    if (rValueInstanceTy->isExistentialType()) {
      switch (isMemberAvailableOnExistential(rValueInstanceTy, storage)) {
      case ExistentialMemberAccessLimitation::Unsupported:
      case ExistentialMemberAccessLimitation::ReadOnly:
        // Never an lvalue because the current type system cannot represent the
        // setter's type out of context.
        return false;

      case ExistentialMemberAccessLimitation::WriteOnly:
      case ExistentialMemberAccessLimitation::None:
        break;
      }
    }
  }

  if (baseType->is<LValueType>()) {
    return true;
  }

  // The base is an rvalue type. The only way an accessor can
  // produce an lvalue is if we have a property where both the
  // getter and setter are nonmutating.
  return (!storage->isGetterMutating() &&
          !storage->isSetterMutating());
}

Type GetClosureType::operator()(const AbstractClosureExpr *expr) const {
  if (auto closure = dyn_cast<ClosureExpr>(expr)) {
    // Look through type bindings, if we have them.
    auto mutableClosure = const_cast<ClosureExpr *>(closure);
    if (cs.hasType(mutableClosure)) {
      return cs.getFixedTypeRecursive(
          cs.getType(mutableClosure), /*wantRValue=*/true);
    }

    return cs.getClosureTypeIfAvailable(closure);
  }

  return Type();
}

bool
ClosureIsolatedByPreconcurrency::operator()(const ClosureExpr *expr) const {
  return expr->isIsolatedByPreconcurrency() ||
      cs.preconcurrencyClosures.count(expr);
}

Type ConstraintSystem::getUnopenedTypeOfReference(
    VarDecl *value, Type baseType, DeclContext *UseDC,
    ConstraintLocator *locator, bool wantInterfaceType) {
  Type requestedType;
  if (Type type = getTypeIfAvailable(value)) {
    requestedType = type;
  } else if (!value->hasInterfaceType()) {
    requestedType = ErrorType::get(getASTContext());
  } else {
    requestedType = (wantInterfaceType
                     ? value->getInterfaceType()
                     : value->getTypeInContext());
  }

  requestedType =
      requestedType->getWithoutSpecifierType()->getReferenceStorageReferent();

  // Strip pack expansion types off of pack references.
  if (auto *expansion = requestedType->getAs<PackExpansionType>())
    requestedType = expansion->getPatternType();

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
  if (doesStorageProduceLValue(value, baseType, UseDC, *this, locator) &&
      !requestedType->hasError()) {
    return LValueType::get(requestedType);
  }

  return requestedType;
}

void ConstraintSystem::recordOpenedType(
    ConstraintLocator *locator, ArrayRef<OpenedType> replacements,
    PreparedOverloadBuilder *preparedOverload) {
  if (preparedOverload) {
    ASSERT(PreparingOverload);
    preparedOverload->openedTypes(replacements);
    return;
  }

  ASSERT(!PreparingOverload);

  bool inserted = OpenedTypes.insert({locator, replacements}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::RecordedOpenedTypes(locator));
}

void ConstraintSystem::recordOpenedTypes(
       ConstraintLocatorBuilder locator,
       const SmallVectorImpl<OpenedType> &replacements,
       PreparedOverloadBuilder *preparedOverload,
       bool fixmeAllowDuplicates) {
  if (replacements.empty())
    return;

  // If the last path element is an archetype or associated type, ignore it.
  SmallVector<LocatorPathElt, 2> pathElts;
  (void) locator.getLocatorParts(pathElts);
  if (!pathElts.empty() &&
      pathElts.back().getKind() == ConstraintLocator::GenericParameter)
    return;

  ConstraintLocator *locatorPtr = getConstraintLocator(locator);
  assert(locatorPtr && "No locator for opened types?");

  // FIXME: Get rid of fixmeAllowDuplicates.
  if (!fixmeAllowDuplicates || OpenedTypes.count(locatorPtr) == 0) {
    OpenedType *openedTypes
      = Allocator.Allocate<OpenedType>(replacements.size());
    std::copy(replacements.begin(), replacements.end(), openedTypes);

    recordOpenedType(
      locatorPtr, llvm::ArrayRef(openedTypes, replacements.size()),
      preparedOverload);
  }
}

/// Determine how many levels of argument labels should be removed from the
/// function type when referencing the given declaration.
static unsigned getNumRemovedArgumentLabels(ValueDecl *decl,
                                            bool isCurriedInstanceReference,
                                            FunctionRefInfo functionRefInfo) {
  unsigned numParameterLists = decl->getNumCurryLevels();

  // Always remove argument labels from compound references, they have already
  // had their argument labels specified.
  if (functionRefInfo.isCompoundName())
    return numParameterLists;

  switch (functionRefInfo.getApplyLevel()) {
  case FunctionRefInfo::ApplyLevel::Unapplied:
    // Always remove argument labels from unapplied references.
    return numParameterLists;

  case FunctionRefInfo::ApplyLevel::SingleApply:
    // If we have fewer than two parameter lists, leave the labels.
    if (numParameterLists < 2)
      return 0;

    // If this is a curried reference to an instance method, where 'self' is
    // being applied, e.g., "ClassName.instanceMethod(self)", remove the
    // argument labels from the resulting function type. The 'self' parameter is
    // always unlabeled, so this operation is a no-op for the actual application.
    return isCurriedInstanceReference ? numParameterLists : 1;

  case FunctionRefInfo::ApplyLevel::DoubleApply:
    // Never remove argument labels from a double application.
    return 0;
  }

  llvm_unreachable("Unhandled FunctionRefInfo in switch.");
}

unsigned constraints::getNumApplications(bool hasAppliedSelf,
                                         FunctionRefInfo functionRefInfo) {
  switch (functionRefInfo.getApplyLevel()) {
  case FunctionRefInfo::ApplyLevel::Unapplied:
    return 0 + hasAppliedSelf;

  case FunctionRefInfo::ApplyLevel::SingleApply:
    return 1 + hasAppliedSelf;

  case FunctionRefInfo::ApplyLevel::DoubleApply:
    return 2;
  }

  llvm_unreachable("Unhandled FunctionRefInfo in switch.");
}

/// Replaces property wrapper types in the parameter list of the given function type
/// with the wrapped-value or projected-value types (depending on argument label).
static FunctionType *
unwrapPropertyWrapperParameterTypes(ConstraintSystem &cs,
                                    AbstractFunctionDecl *funcDecl,
                                    FunctionRefInfo functionRefInfo,
                                    FunctionType *functionType,
                                    ConstraintLocatorBuilder locator,
                                    PreparedOverloadBuilder *preparedOverload) {
  // Only apply property wrappers to unapplied references to functions.
  if (!functionRefInfo.isUnapplied())
    return functionType;

  // This transform is not applicable to pattern matching context.
  //
  // Note: If the transform is ever enabled for patterns - new branch
  // would have to be added to `nameLoc` selection.
  if (locator.endsWith<LocatorPathElt::PatternMatch>())
    return functionType;

  auto *paramList = funcDecl->getParameters();
  auto paramTypes = functionType->getParams();
  SmallVector<AnyFunctionType::Param, 4> adjustedParamTypes;

  DeclNameLoc nameLoc;
  if (auto *ref = getAsExpr(locator.getAnchor()))
    nameLoc = ref->getNameLoc();

  for (unsigned i : indices(*paramList)) {
    Identifier argLabel;
    if (functionRefInfo.isCompoundName()) {
      auto &context = cs.getASTContext();
      auto argLabelLoc = nameLoc.getArgumentLabelLoc(i);
      auto argLabelToken = Lexer::getTokenAtLocation(context.SourceMgr, argLabelLoc);
      argLabel = context.getIdentifier(argLabelToken.getText());
    }

    auto *paramDecl = paramList->get(i);
    if (!paramDecl->hasAttachedPropertyWrapper() && !argLabel.hasDollarPrefix()) {
      adjustedParamTypes.push_back(paramTypes[i]);
      continue;
    }

    auto *loc = cs.getConstraintLocator(locator);
    auto *wrappedType = cs.createTypeVariable(loc, 0, preparedOverload);
    auto paramType = paramTypes[i].getParameterType();
    auto paramLabel = paramTypes[i].getLabel();
    auto paramInternalLabel = paramTypes[i].getInternalLabel();
    adjustedParamTypes.push_back(AnyFunctionType::Param(
        wrappedType, paramLabel, ParameterTypeFlags(), paramInternalLabel));
    cs.applyPropertyWrapperToParameter(
        paramType, wrappedType, paramDecl, argLabel, ConstraintKind::Equal,
        loc, loc, preparedOverload);
  }

  return FunctionType::get(adjustedParamTypes, functionType->getResult(),
                           functionType->getExtInfo());
}

/// Determine whether the given locator is for a witness or requirement.
static bool isRequirementOrWitness(const ConstraintLocatorBuilder &locator) {
  return locator.endsWith<LocatorPathElt::ProtocolRequirement>() ||
         locator.endsWith<LocatorPathElt::Witness>();
}

FunctionType *ConstraintSystem::adjustFunctionTypeForConcurrency(
    FunctionType *fnType, Type baseType, ValueDecl *decl, DeclContext *dc,
    unsigned numApplies, bool isMainDispatchQueue, bool openGlobalActorType,
    ConstraintLocatorBuilder locator) {

  auto *adjustedTy = swift::adjustFunctionTypeForConcurrency(
      fnType, decl, dc, numApplies, isMainDispatchQueue, GetClosureType{*this},
      ClosureIsolatedByPreconcurrency{*this}, [&](Type type) {
        if (!type->hasTypeParameter())
          return type;

        // FIXME: This should be handled elsewhere.
        if (!openGlobalActorType)
          return type;

        auto replacements = getOpenedTypes(getConstraintLocator(locator));
        ASSERT(!replacements.empty());
        return openType(type, replacements, locator, /*preparedOverload=*/nullptr);
      });

  // Infer @Sendable for global actor isolated function types under the
  // upcoming feature flag.
  if (Context.LangOpts.hasFeature(Feature::GlobalActorIsolatedTypesUsability)
      && !adjustedTy->getExtInfo().isSendable()) {
    if (adjustedTy->getExtInfo().getIsolation().isGlobalActor()) {
      adjustedTy =
          adjustedTy->withExtInfo(adjustedTy->getExtInfo().withSendable());
    }
  }

  if (Context.LangOpts.hasFeature(Feature::InferSendableFromCaptures) &&
      !adjustedTy->getExtInfo().isSendable()) {
    DeclContext *DC = nullptr;
    if (auto *FD = dyn_cast<AbstractFunctionDecl>(decl)) {
      DC = FD->getDeclContext();
    } else if (auto EED = dyn_cast<EnumElementDecl>(decl)) {
      if (EED->hasAssociatedValues() &&
          !locator.endsWith<LocatorPathElt::PatternMatch>()) {
        DC = EED->getDeclContext();
      }
    }

    if (DC) {
      // All global functions should be @Sendable
      if (DC->isModuleScopeContext()) {
        if (!adjustedTy->getExtInfo().isSendable()) {
          adjustedTy =
              adjustedTy->withExtInfo(adjustedTy->getExtInfo().withSendable());
        }
      } else if (numApplies < decl->getNumCurryLevels() &&
                 decl->hasCurriedSelf() ) {
        auto shouldMarkMemberTypeSendable = [&]() {
          Type capturedBaseType = baseType;

          if (!decl->isInstanceMember()) {
            // Static member types are Sendable when the metatype of their
            // base type is Sendable, because they capture that metatype.
            // For example, `(S.Type) -> () -> Void`.
            if (!capturedBaseType)
              capturedBaseType = decl->getDeclContext()->getSelfTypeInContext();

            if (!capturedBaseType->is<AnyMetatypeType>())
              capturedBaseType = MetatypeType::get(capturedBaseType);
          } else if (capturedBaseType) {
            // For instance members we need to check whether instance type
            // is Sendable because @Sendable function values cannot capture
            // non-Sendable values (base instance type in this case).
            // For example, `(C) -> () -> Void` where `C` should be Sendable
            // for the inner function type to be Sendable as well.
            capturedBaseType = capturedBaseType->getMetatypeInstanceType();
          }

          return capturedBaseType && capturedBaseType->isSendableType();
        };

        auto referenceTy = adjustedTy->getResult()->castTo<FunctionType>();
        if (shouldMarkMemberTypeSendable()) {
          referenceTy =
              referenceTy
                  ->withExtInfo(referenceTy->getExtInfo().withSendable())
                  ->getAs<FunctionType>();
        }

        // @Sendable since fully uncurried type doesn't capture anything.
        adjustedTy =
            FunctionType::get(adjustedTy->getParams(), referenceTy,
                              adjustedTy->getExtInfo().withSendable());
      }
    }
  }

  return adjustedTy->castTo<FunctionType>();
}

/// For every parameter in \p type that has an error type, replace that
/// parameter's type by a placeholder type, where \p value is the declaration
/// that declared \p type. This is useful for code completion so we can match
/// the types we do know instead of bailing out completely because \p type
/// contains an error type.
static Type replaceParamErrorTypeByPlaceholder(Type type, ValueDecl *value, bool hasAppliedSelf) {
  if (!type->is<AnyFunctionType>() || !isa<AbstractFunctionDecl>(value)) {
    return type;
  }
  auto funcType = type->castTo<AnyFunctionType>();
  auto funcDecl = cast<AbstractFunctionDecl>(value);

  SmallVector<ParamDecl *> declParams;
  if (hasAppliedSelf) {
    declParams.append(funcDecl->getParameters()->begin(), funcDecl->getParameters()->end());
  } else {
    declParams.push_back(funcDecl->getImplicitSelfDecl());
  }
  auto typeParams = funcType->getParams();
  assert(declParams.size() == typeParams.size());
  SmallVector<AnyFunctionType::Param, 4> newParams;
  newParams.reserve(declParams.size());
  for (auto i : indices(typeParams)) {
    AnyFunctionType::Param param = typeParams[i];
    if (param.getPlainType()->is<ErrorType>()) {
      auto paramDecl = declParams[i];
      auto placeholder =
          PlaceholderType::get(paramDecl->getASTContext(), paramDecl);
      newParams.push_back(param.withType(placeholder));
    } else {
      newParams.push_back(param);
    }
  }
  assert(newParams.size() == declParams.size());
  return FunctionType::get(newParams, funcType->getResult(),
                           funcType->getExtInfo());
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfReferencePre(OverloadChoice choice,
                                        DeclContext *useDC,
                                        ConstraintLocatorBuilder locator,
                                        PreparedOverloadBuilder *preparedOverload) {
  auto *value = choice.getDecl();

  ASSERT(!!preparedOverload == PreparingOverload);

  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    SmallVector<OpenedType, 2> replacements;

    AnyFunctionType *funcType = func->getInterfaceType()
        ->castTo<AnyFunctionType>();
    auto openedType = openFunctionType(
        funcType, locator, replacements, func->getDeclContext(),
        preparedOverload);

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements, preparedOverload);

    return {openedType, Type()};
  }

  // Unqualified reference to a local or global function.
  if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(value)) {
    auto functionRefInfo = choice.getFunctionRefInfo();

    SmallVector<OpenedType, 4> replacements;

    auto funcType = funcDecl->getInterfaceType()->castTo<AnyFunctionType>();
    auto numLabelsToRemove = getNumRemovedArgumentLabels(
        funcDecl, /*isCurriedInstanceReference=*/false, functionRefInfo);

    auto openedType = openFunctionType(funcType, locator, replacements,
                                       funcDecl->getDeclContext(),
                                       preparedOverload)
                          ->removeArgumentLabels(numLabelsToRemove);
    openedType = unwrapPropertyWrapperParameterTypes(
        *this, funcDecl, functionRefInfo, openedType->castTo<FunctionType>(),
        locator, preparedOverload);

    if (isForCodeCompletion() && openedType->hasError()) {
      // In code completion, replace error types by placeholder types so we can
      // match the types we know instead of bailing out completely.
      openedType = replaceParamErrorTypeByPlaceholder(
          openedType, value, /*hasAppliedSelf=*/true);
    }

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements, preparedOverload);

    return {openedType, Type()};
  }

  // Unqualified reference to a type.
  if (auto typeDecl = dyn_cast<TypeDecl>(value)) {
    // Resolve the reference to this type declaration in our current context.
    auto type =
        TypeResolution::forInterface(useDC, TypeResolverContext::InExpression,
                                     /*unboundTyOpener*/ nullptr,
                                     /*placeholderHandler*/ nullptr,
                                     /*packElementOpener*/ nullptr)
            .resolveTypeInContext(typeDecl, /*foundDC*/ nullptr,
                                  /*isSpecialized=*/false);
    type = useDC->mapTypeIntoContext(type);

    checkNestedTypeConstraints(*this, type, locator, preparedOverload);

    // Convert any placeholder types and open generics.
    type = replaceInferableTypesWithTypeVars(type, locator, preparedOverload);

    // Module types are not wrapped in metatypes.
    if (type->is<ModuleType>())
      return { type, Type() };

    // If it's a value reference, refer to the metatype.
    type = MetatypeType::get(type);
    return {type, Type()};
  }

  // Unqualified reference to a macro.
  if (auto macro = dyn_cast<MacroDecl>(value)) {
    Type macroType = macro->getInterfaceType();

    // Open any the generic types.
    SmallVector<OpenedType, 4> replacements;
    Type openedType = openFunctionType(
        macroType->castTo<AnyFunctionType>(), locator, replacements,
        macro->getDeclContext(),
        preparedOverload);

    // If we opened up any type variables, record the replacements.
    recordOpenedTypes(locator, replacements, preparedOverload);

    // FIXME: Should we use replaceParamErrorTypeByPlaceholder() here?

    return {openedType, Type()};
  }

  // Only remaining case: unqualified reference to a property.
  auto *varDecl = cast<VarDecl>(value);

  // Determine the type of the value, opening up that type if necessary.
  bool wantInterfaceType = !varDecl->getDeclContext()->isLocalContext();
  Type valueType =
      getUnopenedTypeOfReference(varDecl, Type(), useDC, 
                                 getConstraintLocator(locator),
                                 wantInterfaceType);

  ASSERT(!valueType->hasUnboundGenericType() &&
         !valueType->hasTypeParameter());

  Type thrownErrorType;
  if (auto accessor = varDecl->getEffectfulGetAccessor()) {
    thrownErrorType =
        accessor->getEffectiveThrownErrorType().value_or(Type());
  }

  return {valueType, thrownErrorType};
}

DeclReferenceType
ConstraintSystem::getTypeOfReferencePost(OverloadChoice choice,
                                         DeclContext *useDC,
                                         ConstraintLocatorBuilder locator,
                                         Type openedType, Type thrownErrorType) {
  auto *value = choice.getDecl();

  if (value->getDeclContext()->isTypeContext() && isa<FuncDecl>(value)) {
    auto *openedFnType = openedType->castTo<FunctionType>();

    // Unqualified lookup can find operator names within nominal types.
    auto func = cast<FuncDecl>(value);
    assert(func->isOperator() && "Lookup should only find operators");

    auto functionRefInfo = choice.getFunctionRefInfo();

    auto origOpenedType = openedFnType;
    if (!isRequirementOrWitness(locator)) {
      unsigned numApplies = getNumApplications(/*hasAppliedSelf*/ false,
                                               functionRefInfo);
      openedFnType = adjustFunctionTypeForConcurrency(
          origOpenedType, /*baseType=*/Type(), func, useDC, numApplies,
          /*isMainDispatchQueue=*/false, /*openGlobalActorType=*/true, locator);
    }

    // If this is a method whose result type is dynamic Self, replace
    // DynamicSelf with the actual object type. Repeat the adjustment
    // for the original and adjusted types.
    auto type = openedFnType;
    if (openedFnType->hasDynamicSelfType()) {
      auto params = openedFnType->getParams();
      assert(params.size() == 1);
      Type selfTy = params.front().getPlainType()->getMetatypeInstanceType();
      type = openedFnType->replaceDynamicSelfType(selfTy)
          ->castTo<FunctionType>();
    }

    auto origType = origOpenedType;
    if (origOpenedType->hasDynamicSelfType()) {
      auto params = origOpenedType->getParams();
      assert(params.size() == 1);
      Type selfTy = params.front().getPlainType()->getMetatypeInstanceType();
      origType = origOpenedType->replaceDynamicSelfType(selfTy)
          ->castTo<FunctionType>();
    }

    // The reference implicitly binds 'self'.
    return {origOpenedType, openedType,
            origType->getResult(), type->getResult(), Type()};
  }

  // Unqualified reference to a local or global function.
  if (auto funcDecl = dyn_cast<AbstractFunctionDecl>(value)) {
    auto origOpenedType = openedType;
    if (!isRequirementOrWitness(locator)) {
      unsigned numApplies = getNumApplications(/*hasAppliedSelf*/ false,
                                               choice.getFunctionRefInfo());
      openedType = adjustFunctionTypeForConcurrency(
          origOpenedType->castTo<FunctionType>(), /*baseType=*/Type(), funcDecl,
          useDC, numApplies, /*isMainDispatchQueue=*/false,
          /*openGlobalActorType=*/true, locator);
    }

    return { origOpenedType, openedType, origOpenedType, openedType, Type() };
  }

  // Unqualified reference to a type.
  if (isa<TypeDecl>(value)) {
    return { openedType, openedType, openedType, openedType, Type() };
  }

  // Unqualified reference to a macro.
  if (isa<MacroDecl>(value)) {
    return { openedType, openedType, openedType, openedType, Type() };
  }

  // Only remaining case: unqualified reference to a property.
  auto *varDecl = cast<VarDecl>(value);

  // Adjust the type for concurrency.
  auto origOpenedType = openedType;

  if (!isRequirementOrWitness(locator)) {
    openedType = adjustVarTypeForConcurrency(
        openedType, varDecl, useDC,
        GetClosureType{*this},
        ClosureIsolatedByPreconcurrency{*this});
  }

  return { origOpenedType, openedType,
           origOpenedType, openedType,
           thrownErrorType };
}

DeclReferenceType
ConstraintSystem::getTypeOfReference(OverloadChoice choice,
                                     DeclContext *useDC,
                                     ConstraintLocatorBuilder locator,
                                     PreparedOverloadBuilder *preparedOverload) {
  ASSERT(!!preparedOverload == PreparingOverload);

  Type openedType, thrownErrorType;
  std::tie(openedType, thrownErrorType) = getTypeOfReferencePre(
      choice, useDC, locator, preparedOverload);

  return getTypeOfReferencePost(choice, useDC, locator,
                                openedType, thrownErrorType);
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
    ArrayRef<OpenedType> replacements,
    PreparedOverloadBuilder *preparedOverload) {

  auto bindPrimaryArchetype = [&](Type paramTy, Type contextTy) {
    // We might not have a type variable for this generic parameter
    // because either we're opening up an UnboundGenericType,
    // in which case we only want to infer the innermost generic
    // parameters, or because this generic parameter was constrained
    // away into a concrete type.
    for (auto pair : replacements) {
      if (pair.first->isEqual(paramTy)) {
        cs.addConstraint(ConstraintKind::Bind, pair.second, contextTy,
                         locatorPtr, /*isFavored=*/false, preparedOverload);
        return;
      }
    }
  };

  // Find the innermost non-type context.
  for (const auto *parentDC = outerDC;
       !parentDC->isModuleScopeContext();
       parentDC = parentDC->getParentForLookup()) {
    if (parentDC->isTypeContext()) {
      if (parentDC != outerDC && parentDC->getSelfProtocolDecl()) {
        auto selfTy = parentDC->getSelfInterfaceType();
        bindPrimaryArchetype(selfTy, ErrorType::get(cs.getASTContext()));
      }
      continue;
    }

    auto genericSig = parentDC->getGenericSignatureOfContext();
    for (auto *paramTy : genericSig.getGenericParams()) {
      Type contextTy = cs.DC->mapTypeIntoContext(paramTy);
      if (paramTy->isParameterPack())
        contextTy = PackType::getSingletonPackExpansion(contextTy);
      bindPrimaryArchetype(paramTy, contextTy);
    }

    break;
  }
}

void ConstraintSystem::openGeneric(
       DeclContext *outerDC,
       GenericSignature sig,
       ConstraintLocatorBuilder locator,
       SmallVectorImpl<OpenedType> &replacements,
       PreparedOverloadBuilder *preparedOverload) {
  if (!sig)
    return;

  openGenericParameters(outerDC, sig, replacements, locator, preparedOverload);

  // Add the requirements as constraints.
  openGenericRequirements(
      outerDC, sig, /*skipProtocolSelfConstraint=*/false, locator,
      [&](Type type) {
        return openType(type, replacements, locator, preparedOverload);
      }, preparedOverload);
}

void ConstraintSystem::openGenericParameters(DeclContext *outerDC,
                                             GenericSignature sig,
                                             SmallVectorImpl<OpenedType> &replacements,
                                             ConstraintLocatorBuilder locator,
                                             PreparedOverloadBuilder *preparedOverload) {
  ASSERT(sig);
  ASSERT(replacements.empty());

  // Create the type variables for the generic parameters.
  for (auto gp : sig.getGenericParams()) {
    auto *typeVar = openGenericParameter(gp, locator, preparedOverload);
    replacements.emplace_back(gp, typeVar);
  }

  auto *baseLocator = getConstraintLocator(
      locator.withPathElement(LocatorPathElt::OpenedGeneric(sig)));

  bindArchetypesFromContext(*this, outerDC, baseLocator, replacements, preparedOverload);
}

TypeVariableType *ConstraintSystem::openGenericParameter(GenericTypeParamType *parameter,
                                                         ConstraintLocatorBuilder locator,
                                                         PreparedOverloadBuilder *preparedOverload) {
  auto *paramLocator = getConstraintLocator(
      locator.withPathElement(LocatorPathElt::GenericParameter(parameter)));

  unsigned options = TVO_PrefersSubtypeBinding;

  if (parameter->isParameterPack())
    options |= TVO_CanBindToPack;

  if (shouldAttemptFixes())
    options |= TVO_CanBindToHole;

  return createTypeVariable(paramLocator, options, preparedOverload);
}

void ConstraintSystem::openGenericRequirements(
    DeclContext *outerDC, GenericSignature signature,
    bool skipProtocolSelfConstraint, ConstraintLocatorBuilder locator,
    llvm::function_ref<Type(Type)> substFn,
    PreparedOverloadBuilder *preparedOverload) {
  auto requirements = signature.getRequirements();
  for (unsigned pos = 0, n = requirements.size(); pos != n; ++pos) {
    auto openedGenericLoc =
      locator.withPathElement(LocatorPathElt::OpenedGeneric(signature));
    openGenericRequirement(outerDC, signature, pos, requirements[pos],
                           skipProtocolSelfConstraint, openedGenericLoc,
                           substFn, preparedOverload);
  }
}

void ConstraintSystem::openGenericRequirement(
    DeclContext *outerDC, GenericSignature signature,
    unsigned index, const Requirement &req,
    bool skipProtocolSelfConstraint, ConstraintLocatorBuilder locator,
    llvm::function_ref<Type(Type)> substFn,
    PreparedOverloadBuilder *preparedOverload) {
  std::optional<Requirement> openedReq;
  auto openedFirst = substFn(req.getFirstType());

  bool prohibitIsolatedConformance = false;
  auto kind = req.getKind();
  switch (kind) {
  case RequirementKind::Conformance: {
    auto protoDecl = req.getProtocolDecl();
    // Determine whether this is the protocol 'Self' constraint we should
    // skip.
    if (skipProtocolSelfConstraint && protoDecl == outerDC &&
        protoDecl->getSelfInterfaceType()->isEqual(req.getFirstType()))
      return;

    // Check whether the given type parameter has requirements that
    // prohibit it from using an isolated conformance.
    if (signature &&
        signature->prohibitsIsolatedConformance(req.getFirstType()))
      prohibitIsolatedConformance = true;

    openedReq = Requirement(kind, openedFirst, req.getSecondType());
    break;
  }
  case RequirementKind::Superclass:
  case RequirementKind::SameType:
  case RequirementKind::SameShape:
    openedReq = Requirement(kind, openedFirst, substFn(req.getSecondType()));
    break;
  case RequirementKind::Layout:
    openedReq = Requirement(kind, openedFirst, req.getLayoutConstraint());
    break;
  }

  addConstraint(*openedReq,
                locator.withPathElement(
                    LocatorPathElt::TypeParameterRequirement(index, kind)),
                /*isFavored=*/false, prohibitIsolatedConformance,
                preparedOverload);
}

Type ConstraintSystem::getTypeOfMemberTypeReference(
    Type baseObjTy, TypeDecl *typeDecl, ConstraintLocator *locator,
    PreparedOverloadBuilder *preparedOverload) {
  ASSERT(!isa<ModuleDecl>(typeDecl) && "Nested module?");

  auto memberTy = TypeChecker::substMemberTypeWithBase(typeDecl, baseObjTy);

  // If the member type is a constraint, e.g. because the
  // reference is to a typealias with an underlying protocol
  // or composition type, the member reference has existential
  // type.
  if (memberTy->isConstraintType())
    memberTy = ExistentialType::get(memberTy);

  checkNestedTypeConstraints(*this, memberTy, locator, preparedOverload);

  // Convert any placeholders and open any generics.
  memberTy = replaceInferableTypesWithTypeVars(
      memberTy, locator, preparedOverload);

  // Wrap it in a metatype, unless this is a value generic.
  // 'substMemberTypeWithBase' returns the underlying value type
  // of the value generic (e.g. 'Int').
  if (!isa<GenericTypeParamDecl>(typeDecl) ||
      !cast<GenericTypeParamDecl>(typeDecl)->isValue()) {
    memberTy = MetatypeType::get(memberTy);
  }

  FunctionType::Param baseObjParam(baseObjTy);
  return FunctionType::get({baseObjParam}, memberTy);
}

std::pair<Type, Type> ConstraintSystem::getOpenedStorageType(
    Type baseTy, AbstractStorageDecl *value, DeclContext *useDC,
    bool hasAppliedSelf, ArrayRef<OpenedType> replacements,
    ConstraintLocator *locator, PreparedOverloadBuilder *preparedOverload) {
  Type thrownErrorType;

  if (auto accessor = value->getEffectfulGetAccessor()) {
    thrownErrorType = accessor->getEffectiveThrownErrorType().value_or(Type());
  }

  // For a property, build a type (Self) -> PropType.
  // For a subscript, build a type (Self) -> (Indices...) throws(?) -> ElementType.
  //
  // If the access is mutating, wrap the storage type in an lvalue type.
  Type refType;
  if (auto *subscript = dyn_cast<SubscriptDecl>(value)) {
    auto elementTy = subscript->getElementInterfaceType();

    if (doesStorageProduceLValue(subscript, baseTy, useDC, *this, locator))
      elementTy = LValueType::get(elementTy);

    auto indices = subscript->getInterfaceType()
                            ->castTo<AnyFunctionType>()->getParams();

    // Transfer the thrown error type into the subscript reference type,
    // which will be used in the application.
    FunctionType::ExtInfo info;
    if (thrownErrorType) {
      info = info.withThrows(true, thrownErrorType);
      thrownErrorType = Type();
    }

    refType = FunctionType::get(indices, elementTy, info);
  } else {
    // Delay the adjustment for preconcurrency until after we've formed
    // the function type for this kind of reference. Otherwise we will lose
    // track of the adjustment in the formed function's return type.

    refType = getUnopenedTypeOfReference(cast<VarDecl>(value), baseTy, useDC,
                                         locator,
                                         /*wantInterfaceType=*/true);
  }

  auto *outerDC = value->getDeclContext();
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

  // If the storage is generic, open the self and ref types.
  selfTy = openType(selfTy, replacements, locator, preparedOverload);
  refType = openType(refType, replacements, locator, preparedOverload);

  if (thrownErrorType) {
    thrownErrorType = openType(thrownErrorType, replacements, locator,
                               preparedOverload);
  }

  FunctionType::Param selfParam(selfTy, Identifier(), selfFlags);

  FunctionType::ExtInfo info;
  return std::make_pair(FunctionType::get({selfParam}, refType, info),
                        thrownErrorType);
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
                              ConstraintLocatorBuilder locator,
                              PreparedOverloadBuilder *preparedOverload) {
  assert(!selfTy->is<ProtocolType>());

  // Otherwise, use a subtype constraint for classes to cope with inheritance.
  if (selfTy->getClassOrBoundGenericClass()) {
    cs.addConstraint(ConstraintKind::Subtype, objectTy, selfTy,
                     cs.getConstraintLocator(locator), /*isFavored=*/false,
                     preparedOverload);
    return;
  }

  // Otherwise, the types must be equivalent.
  cs.addConstraint(ConstraintKind::Bind, objectTy, selfTy,
                   cs.getConstraintLocator(locator), /*isFavored=*/false,
                   preparedOverload);
}

Type constraints::getDynamicSelfReplacementType(
    Type baseObjTy, const ValueDecl *member, ConstraintLocator *memberLocator) {
  // Constructions must always have their dynamic 'Self' result type replaced
  // with the base object type, 'super' or not.
  if (isa<ConstructorDecl>(member))
    return baseObjTy;

  const SuperRefExpr *SuperExpr = nullptr;
  if (auto *E = getAsExpr(memberLocator->getAnchor())) {
    if (auto *LE = dyn_cast<LookupExpr>(E)) {
      SuperExpr = dyn_cast<SuperRefExpr>(LE->getBase());
    } else if (auto *UDE = dyn_cast<UnresolvedDotExpr>(E)) {
      SuperExpr = dyn_cast<SuperRefExpr>(UDE->getBase());
    }
  }

  // For anything else that isn't 'super', we want it to be the base
  // object type.
  if (!SuperExpr)
    return baseObjTy;

  // 'super' is special in that we actually want dynamic 'Self' to behave
  // as if the base were 'self'.
  const auto *selfDecl = SuperExpr->getSelf();
  return selfDecl->getDeclContext()
      ->getInnermostTypeContext()
      ->mapTypeIntoContext(selfDecl->getInterfaceType())
      ->getMetatypeInstanceType();
}

/// Determine whether this locator refers to a member of "DispatchQueue.main",
/// which is a special dispatch queue that executes its work on the main actor.
static bool isMainDispatchQueueMember(ConstraintLocator *locator) {
  if (!locator)
    return false;

  if (locator->getPath().size() != 1 ||
      !locator->isLastElement<LocatorPathElt::Member>())
    return false;

  auto expr = locator->getAnchor().dyn_cast<Expr *>();
  if (!expr)
    return false;

  auto outerUnresolvedDot = dyn_cast<UnresolvedDotExpr>(expr);
  if (!outerUnresolvedDot)
    return false;


  if (!isDispatchQueueOperationName(
          outerUnresolvedDot->getName().getBaseName().userFacingName()))
    return false;

  auto innerUnresolvedDot = dyn_cast<UnresolvedDotExpr>(
      outerUnresolvedDot->getBase());
  if (!innerUnresolvedDot)
    return false;

  if (innerUnresolvedDot->getName().getBaseName().userFacingName() != "main")
    return false;

  auto typeExpr = dyn_cast<TypeExpr>(innerUnresolvedDot->getBase());
  if (!typeExpr)
    return false;

  auto typeRepr = typeExpr->getTypeRepr();
  if (!typeRepr)
    return false;

  auto declRefTR = dyn_cast<DeclRefTypeRepr>(typeRepr);
  if (!declRefTR)
    return false;

  if (declRefTR->getNameRef().getBaseName().userFacingName() != "DispatchQueue")
    return false;

  return true;
}

static bool isExistentialMemberAccessWithExplicitBaseExpression(
    Type baseInstanceTy, ValueDecl *member, ConstraintLocator *locator,
    bool isDynamicLookup) {
  if (isDynamicLookup) {
    return false;
  }

  // '.x' does not have an explicit base expression.
  if (locator->isLastElement<LocatorPathElt::UnresolvedMember>()) {
    return false;
  }

  return baseInstanceTy->isExistentialType() &&
         member->getDeclContext()->getSelfProtocolDecl();
}

Type ConstraintSystem::getMemberReferenceTypeFromOpenedType(
    Type type, Type baseObjTy, ValueDecl *value,
    ConstraintLocator *locator, bool hasAppliedSelf, bool isDynamicLookup) {
  auto *outerDC = value->getDeclContext();

  // Cope with dynamic 'Self'.
  if (outerDC->getSelfClassDecl()) {
    if (type->hasDynamicSelfType()) {
      auto replacementTy = getDynamicSelfReplacementType(
          baseObjTy, value, locator);

      type = type->replaceDynamicSelfType(replacementTy);
    }
  }

  if (hasAppliedSelf) {
    // For a static member referenced through a metatype or an instance
    // member referenced through an instance, strip off the 'self'.
    type = type->castTo<FunctionType>()->getResult();
  } else {
    // For an unbound instance method reference, replace the 'Self'
    // parameter with the base type.
    type = type->replaceSelfParameterType(baseObjTy);
  }

  // From the user perspective, protocol members that are accessed with an
  // existential base are accessed directly on the existential, and not an
  // opened archetype, so the type of the member reference must be abstracted
  // away (upcast) from context-specific types like `Self` in covariant
  // position.
  if (isExistentialMemberAccessWithExplicitBaseExpression(
          baseObjTy, value, locator, isDynamicLookup) &&
      // If there are no type variables, there were no references to 'Self'.
      type->hasTypeVariable()) {
    auto replacements = getOpenedTypes(locator);
    ASSERT(replacements[0].first->isEqual(getASTContext().TheSelfType));
    auto openedTypeVar = replacements[0].second;

    type = typeEraseOpenedExistentialReference(type, baseObjTy, openedTypeVar,
                                               TypePosition::Covariant);
  }

  // Construct an idealized parameter type of the initializer associated
  // with object literal, which generally simplifies the first label
  // (e.g. "colorLiteralRed:") by stripping all the redundant stuff about
  // literals (leaving e.g. "red:").
  {
    auto anchor = locator->getAnchor();
    if (auto *OLE = getAsExpr<ObjectLiteralExpr>(anchor)) {
      auto fnType = type->castTo<FunctionType>();

      SmallVector<AnyFunctionType::Param, 4> params(fnType->getParams().begin(),
                                                    fnType->getParams().end());

      switch (OLE->getLiteralKind()) {
      case ObjectLiteralExpr::colorLiteral:
        params[0] = params[0].withLabel(Context.getIdentifier("red"));
        break;

      case ObjectLiteralExpr::fileLiteral:
      case ObjectLiteralExpr::imageLiteral:
        params[0] = params[0].withLabel(Context.getIdentifier("resourceName"));
        break;
      }

      type =
          FunctionType::get(params, fnType->getResult(), fnType->getExtInfo());
    }
  }

  if (isForCodeCompletion() && type->hasError()) {
    // In code completion, replace error types by placeholder types so we can
    // match the types we know instead of bailing out completely.
    type = replaceParamErrorTypeByPlaceholder(type, value, hasAppliedSelf);
  }

  return type;
}

static FunctionType *applyOptionality(ValueDecl *value, FunctionType *fnTy) {
  Type resultTy;

  // Optional and dynamic subscripts are a special case, because the
  // optionality is applied to the result type and not the type of the
  // reference.
  if (isa<SubscriptDecl>(value)) {
    auto *innerFn = fnTy->getResult()->castTo<FunctionType>();
    resultTy = FunctionType::get(
        innerFn->getParams(),
        OptionalType::get(innerFn->getResult()->getRValueType()),
        innerFn->getExtInfo());
  } else {
    resultTy = OptionalType::get(fnTy->getResult()->getRValueType());
  }

  return FunctionType::get(fnTy->getParams(), resultTy,
                           fnTy->getExtInfo());
}

std::pair<Type, Type>
ConstraintSystem::getTypeOfMemberReferencePre(
    OverloadChoice choice, DeclContext *useDC,
    ConstraintLocator *locator,
    PreparedOverloadBuilder *preparedOverload) {
  ASSERT(!!preparedOverload == PreparingOverload);

  auto *value = choice.getDecl();
  auto functionRefInfo = choice.getFunctionRefInfo();

  // Figure out the instance type used for the base.
  auto baseTy = choice.getBaseType();
  Type baseRValueTy = baseTy->getRValueType();
  auto baseObjTy = baseRValueTy->getMetatypeInstanceType();

  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReferencePre(choice, useDC, locator, preparedOverload);
  }

  Type openedType;
  Type thrownErrorType;

  if (auto *typeDecl = dyn_cast<TypeDecl>(value)) {
    openedType = getTypeOfMemberTypeReference(baseObjTy, typeDecl,
                                              locator, preparedOverload);
    return {openedType, thrownErrorType};
  }

  // Figure out the declaration context to use when opening this type.
  DeclContext *innerDC = value->getInnermostDeclContext();
  DeclContext *outerDC = value->getDeclContext();

  auto genericSig = innerDC->getGenericSignatureOfContext();

  // Open the type of the generic function or member of a generic type.
  SmallVector<OpenedType, 4> replacements;

  // If we have a generic signature, open the parameters. We delay opening
  // requirements to allow contextual types to affect the situation.
  if (genericSig) {
    openGenericParameters(outerDC, genericSig, replacements, locator,
                          preparedOverload);
  }

  // If we opened up any type variables, record the replacements. We do this
  // up-front to allow requirement fix coalescing logic to work correctly with
  // requirements imposed on base type (since that relies on being able to
  // find the recorded opened type). We then make the array immutable for the
  // following logic to ensure they don't attempt to add any additional opened
  // types.
  recordOpenedTypes(locator, replacements, preparedOverload);

  // Check to see if the self parameter is applied, in which case we'll want to
  // strip it off later.
  auto hasAppliedSelf = doesMemberRefApplyCurriedSelf(baseRValueTy, value);

  if (isa<AbstractFunctionDecl>(value) ||
      isa<EnumElementDecl>(value) ||
      isa<MacroDecl>(value)) {
    auto interfaceType = value->getInterfaceType();
    if (interfaceType->is<ErrorType>() || isa<MacroDecl>(value))
      return { interfaceType, Type() };

    if (outerDC->getSelfClassDecl()) {
      if (isa<ConstructorDecl>(value)) {
        interfaceType = interfaceType->withCovariantResultType();
      }
    }

    // This is the easy case.
    openedType = interfaceType->castTo<AnyFunctionType>();

    if (auto *genericFn = openedType->getAs<GenericFunctionType>()) {
      openedType = substGenericArgs(genericFn,
          [&](Type type) {
            return openType(type, replacements, locator, preparedOverload);
          });
    }
  } else {
    auto *storage = cast<AbstractStorageDecl>(value);

    std::tie(openedType, thrownErrorType) = getOpenedStorageType(
        baseTy, storage, useDC, hasAppliedSelf, replacements, locator,
        preparedOverload);
  }

  ASSERT(!openedType->hasTypeParameter());

  // Remove argument labels if this is an unapplied reference.
  unsigned numRemovedArgumentLabels = getNumRemovedArgumentLabels(
      value, /*isCurriedInstanceReference*/ !hasAppliedSelf, functionRefInfo);
  openedType = openedType->removeArgumentLabels(numRemovedArgumentLabels);

  Type baseOpenedTy = baseObjTy;

  // If we are looking at a member of an existential, open the existential.
  if (baseObjTy->isExistentialType()) {
    if (value->isStatic() &&
        locator->isLastElement<LocatorPathElt::UnresolvedMember>()) {
      // Indicates whether this is a valid reference to a static member on a
      // protocol metatype. Such a reference is only valid if performed through
      // leading dot syntax e.g. `foo(.bar)` where implicit base is a protocol
      // metatype and `bar` is static member declared in a protocol  or its
      // extension.
      assert(baseRValueTy->is<MetatypeType>() &&
             "Assumed base of unresolved member access must be a metatype");

      // In diagnostic mode, let's not try to replace base type
      // if there is already a known issue associated with this
      // reference e.g. it might be incorrect initializer call
      // or result type is invalid.
      if (!(shouldAttemptFixes() && hasFixFor(getConstraintLocator(locator)))) {
        if (auto concreteSelf =
                getConcreteReplacementForProtocolSelfType(value)) {
          // Concrete type replacing `Self` could be generic, so we need
          // to make sure that it's opened before use.
          baseOpenedTy = openType(concreteSelf, replacements, locator,
                                  preparedOverload);
        }
      }
    } else {
      // Open the existential.
      auto openedArchetype =
          ExistentialArchetypeType::get(baseObjTy->getCanonicalType());
      recordOpenedExistentialType(getConstraintLocator(locator), openedArchetype,
                                  preparedOverload);
      baseOpenedTy = openedArchetype;
    }
  }

  // Constrain the 'self' object type.
  auto openedParams = openedType->castTo<FunctionType>()->getParams();
  assert(openedParams.size() == 1);

  Type selfObjTy = openedParams.front().getPlainType()->getMetatypeInstanceType();
  if (outerDC->getSelfProtocolDecl()) {
    // For a protocol, substitute the base object directly. We don't need a
    // conformance constraint because we wouldn't have found the declaration
    // if it didn't conform.
    addConstraint(ConstraintKind::Bind, baseOpenedTy, selfObjTy,
                  getConstraintLocator(locator), /*isFavored=*/false,
                  preparedOverload);
  } else if (choice.getKind() != OverloadChoiceKind::DeclViaDynamic) {
    addSelfConstraint(*this, baseOpenedTy, selfObjTy, locator, preparedOverload);
  }

  // Open generic requirements after self constraint has been
  // applied and contextual types have been propagated. This
  // helps diagnostics because instead of self type conversion
  // failing we'll get a generic requirement constraint failure
  // if mismatch is related to generic parameters which is much
  // easier to diagnose.
  if (genericSig) {
    openGenericRequirements(
        outerDC, genericSig,
        /*skipProtocolSelfConstraint=*/true, locator,
        [&](Type type) {
          return openType(type, replacements, locator, preparedOverload);
        }, preparedOverload);
  }

  // Unwrap property wrappers if this is an unapplied reference.
  if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(value)) {
    if (functionRefInfo.isUnapplied()) {
      auto *fullFunctionType = openedType->getAs<AnyFunctionType>();

      auto *functionType = fullFunctionType->getResult()->getAs<FunctionType>();
      functionType = unwrapPropertyWrapperParameterTypes(
          *this, funcDecl, functionRefInfo, functionType,
          locator, preparedOverload);
      openedType =
          FunctionType::get(fullFunctionType->getParams(), functionType,
                            fullFunctionType->getExtInfo());
    }
  }

  return { openedType, thrownErrorType };
}

DeclReferenceType ConstraintSystem::getTypeOfMemberReferencePost(
    OverloadChoice choice, DeclContext *useDC, ConstraintLocator *locator,
    Type openedType, Type thrownErrorType) {
  auto *value = choice.getDecl();

  // Figure out the instance type used for the base.
  Type baseTy = choice.getBaseType();
  Type baseRValueTy = baseTy->getRValueType();
  Type baseObjTy = baseRValueTy->getMetatypeInstanceType();

  if (baseObjTy->is<ModuleType>()) {
    return getTypeOfReferencePost(choice, useDC, locator,
                                  openedType, thrownErrorType);
  }

  if (isa<TypeDecl>(value)) {
    auto type = openedType->castTo<FunctionType>()->getResult();
    return { openedType, openedType, type, type, Type() };
  }

  auto hasAppliedSelf = doesMemberRefApplyCurriedSelf(baseRValueTy, value);

  // Adjust the opened type for concurrency.
  Type origOpenedType = openedType;
  if (isRequirementOrWitness(locator)) {
    // Don't adjust when doing witness matching, because that can cause cycles.
  } else if (isa<AbstractFunctionDecl>(value) || isa<EnumElementDecl>(value)) {
    unsigned numApplies = getNumApplications(
        hasAppliedSelf, choice.getFunctionRefInfo());
    openedType = adjustFunctionTypeForConcurrency(
        origOpenedType->castTo<FunctionType>(), baseObjTy, value, useDC,
        numApplies, isMainDispatchQueueMember(locator),
        /*openGlobalActorType=*/true, locator);
  } else if (auto subscript = dyn_cast<SubscriptDecl>(value)) {
    openedType = adjustFunctionTypeForConcurrency(
        origOpenedType->castTo<FunctionType>(), baseObjTy, subscript, useDC,
        /*numApplies=*/2, /*isMainDispatchQueue=*/false,
        /*openGlobalActorType=*/true, locator);
  } else if (auto var = dyn_cast<VarDecl>(value)) {
    // Adjust the function's result type, since that's the Var's actual type.
    auto origFnType = origOpenedType->castTo<AnyFunctionType>();

    auto resultTy = adjustVarTypeForConcurrency(
          origFnType->getResult(), var, useDC, GetClosureType{*this},
          ClosureIsolatedByPreconcurrency{*this});

    openedType = FunctionType::get(
                  origFnType->getParams(), resultTy, origFnType->getExtInfo());
  }

  bool isDynamicLookup = (choice.getKind() == OverloadChoiceKind::DeclViaDynamic);

  // Check if we need to apply a layer of optionality to the type.
  if (!isRequirementOrWitness(locator)) {
    if (isDynamicLookup || value->getAttrs().hasAttribute<OptionalAttr>()) {
      openedType = applyOptionality(value, openedType->castTo<FunctionType>());
      origOpenedType = applyOptionality(value, origOpenedType->castTo<FunctionType>());
    }
  }

  // Handle DynamicSelfType and a couple of other things.
  Type type = getMemberReferenceTypeFromOpenedType(
      openedType, baseObjTy, value, locator, hasAppliedSelf,
      isDynamicLookup);

  // Do the same thing for the original type, if there can be any difference.
  Type origType = type;
  if (openedType.getPointer() != origOpenedType.getPointer()) {
    origType = getMemberReferenceTypeFromOpenedType(
        origOpenedType, baseObjTy, value, locator, hasAppliedSelf,
        isDynamicLookup);
  }

  return { origOpenedType, openedType, origType, type, thrownErrorType };
}

DeclReferenceType ConstraintSystem::getTypeOfMemberReference(
    OverloadChoice choice, DeclContext *useDC, ConstraintLocator *locator,
    PreparedOverloadBuilder *preparedOverload) {
  ASSERT(!!preparedOverload == PreparingOverload);

  Type openedType, thrownErrorType;
  std::tie(openedType, thrownErrorType)
      = getTypeOfMemberReferencePre(choice, useDC, locator, preparedOverload);

  return getTypeOfMemberReferencePost(
      choice, useDC, locator, openedType, thrownErrorType);
}

Type ConstraintSystem::getEffectiveOverloadType(ConstraintLocator *locator,
                                                const OverloadChoice &overload,
                                                bool allowMembers,
                                                DeclContext *useDC) {
  switch (overload.getKind()) {
  case OverloadChoiceKind::Decl:
    // Declaration choices are handled below.
    break;

  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup:
  case OverloadChoiceKind::KeyPathApplication:
  case OverloadChoiceKind::TupleIndex:
  case OverloadChoiceKind::MaterializePack:
  case OverloadChoiceKind::ExtractFunctionIsolation:
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

    auto getBaseObjectType = [&] () -> Type {
      return overload.getBaseType()
          ->getRValueType()
          ->getMetatypeInstanceType()
          ->lookThroughAllOptionalTypes();
    };

    auto withDynamicSelfResultReplaced = [&](Type type) {
      return type->replaceDynamicSelfType(
        getDynamicSelfReplacementType(
          getBaseObjectType(), decl, locator));
    };

    SmallVector<OpenedType, 4> emptyReplacements;
    if (auto subscript = dyn_cast<SubscriptDecl>(decl)) {
      auto elementTy = subscript->getElementInterfaceType();

      if (doesStorageProduceLValue(subscript, overload.getBaseType(),
                                   useDC, *this, locator))
        elementTy = LValueType::get(elementTy);
      else if (elementTy->hasDynamicSelfType()) {
        elementTy = withDynamicSelfResultReplaced(elementTy);
      }

      // See ConstraintSystem::resolveOverload() -- optional and dynamic
      // subscripts are a special case, because the optionality is
      // applied to the result type and not the type of the reference.
      if (subscript->getAttrs().hasAttribute<OptionalAttr>())
        elementTy = OptionalType::get(elementTy->getRValueType());

      auto indices = subscript->getInterfaceType()
                       ->castTo<AnyFunctionType>()->getParams();
      // FIXME: Verify ExtInfo state is correct, not working by accident.
      FunctionType::ExtInfo info;
      type = adjustFunctionTypeForConcurrency(
          FunctionType::get(indices, elementTy, info), overload.getBaseType(),
          subscript, useDC, /*numApplies=*/1, /*isMainDispatchQueue=*/false,
          /*openGlobalActorType=*/false, locator);
    } else if (auto var = dyn_cast<VarDecl>(decl)) {
      type = var->getValueInterfaceType();
      if (doesStorageProduceLValue(
              var, overload.getBaseType(), useDC, *this, locator)) {
        type = LValueType::get(type);
      } else if (type->hasDynamicSelfType()) {
        type = withDynamicSelfResultReplaced(type);
      }
      type = adjustVarTypeForConcurrency(
          type, var, useDC, GetClosureType{*this},
          ClosureIsolatedByPreconcurrency{*this});
    } else if (isa<AbstractFunctionDecl>(decl) || isa<EnumElementDecl>(decl)) {
      if (decl->isInstanceMember()) {
        auto baseTy = overload.getBaseType();
        if (!baseTy)
          return Type();

        baseTy = baseTy->getRValueType();
        if (!baseTy->getAnyNominal() && !baseTy->is<ExistentialType>() &&
            !baseTy->is<OpaqueTypeArchetypeType>())
          return Type();
      }

      if (isa<ConstructorDecl>(decl) &&
          decl->getDeclContext()->getSelfClassDecl()) {
        type = type->withCovariantResultType();
      }

      // Cope with 'Self' returns.
      if (type->hasDynamicSelfType()) {
        if (!overload.getBaseType())
          return Type();

        type = withDynamicSelfResultReplaced(type);
      }

      auto hasAppliedSelf =
          doesMemberRefApplyCurriedSelf(overload.getBaseType(), decl);
      unsigned numApplies = getNumApplications(hasAppliedSelf,
                                               overload.getFunctionRefInfo());

      type = adjustFunctionTypeForConcurrency(
                 type->castTo<FunctionType>(), overload.getBaseType(), decl,
                 useDC, numApplies, /*isMainDispatchQueue=*/false,
                 /*openGlobalActorType=*/false, locator)
                    ->getResult();
    }
  }

  // Handle "@objc optional" for non-subscripts; subscripts are handled above.
  if (decl->getAttrs().hasAttribute<OptionalAttr>() &&
      !isa<SubscriptDecl>(decl))
    type = OptionalType::get(type->getRValueType());

  return type;
}

void ConstraintSystem::bindOverloadType(const SelectedOverload &overload,
                                        Type boundType,
                                        ConstraintLocator *locator,
                                        DeclContext *useDC) {
  auto &ctx = getASTContext();
  auto choice = overload.choice;
  auto openedType = overload.adjustedOpenedType;

  auto bindTypeOrIUO = [&](Type ty) {
    if (choice.getIUOReferenceKind(*this) == IUOReferenceKind::Value) {
      // Build the disjunction to attempt binding both T? and T (or
      // function returning T? and function returning T).
      buildDisjunctionForImplicitlyUnwrappedOptional(boundType, ty, locator);
    } else {
      // Add the type binding constraint.
      addConstraint(ConstraintKind::Bind, boundType, ty, locator);
    }
  };
  auto addDynamicMemberSubscriptConstraints = [&](Type argTy, Type resultTy) {
    // DynamicMemberLookup results are always a (dynamicMember: T1) -> T2
    // subscript.
    auto *fnTy = openedType->castTo<FunctionType>();
    assert(fnTy->getParams().size() == 1 &&
           "subscript always has one argument");

    auto *callLoc = getConstraintLocator(
        locator, LocatorPathElt::ImplicitDynamicMemberSubscript());

    // Associate an argument list for the implicit x[dynamicMember:] subscript
    // if we haven't already.
    auto *argLoc = getArgumentInfoLocator(callLoc);
    if (ArgumentLists.find(argLoc) == ArgumentLists.end()) {
      auto *argList = ArgumentList::createImplicit(
          ctx, {Argument(SourceLoc(), ctx.Id_dynamicMember, /*expr*/ nullptr)},
          /*firstTrailingClosureIndex=*/std::nullopt,
          AllocationArena::ConstraintSolver);
      recordArgumentList(argLoc, argList);
    }

    auto *callerTy = FunctionType::get(
        {FunctionType::Param(argTy, ctx.Id_dynamicMember)}, resultTy);

    ConstraintLocatorBuilder builder(callLoc);
    addApplicationConstraint(
        callerTy, fnTy, /*trailingClosureMatching=*/std::nullopt, useDC,
        builder.withPathElement(ConstraintLocator::ApplyFunction));

    if (isExpr<KeyPathExpr>(locator->getAnchor())) {
      auto paramTy = fnTy->getParams()[0].getParameterType();
      verifyThatArgumentIsHashable(/*idx*/ 0, paramTy, locator,
                                   choice.getDecl()->getLoc());
    }
  };
  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::TupleIndex:
  case OverloadChoiceKind::MaterializePack:
  case OverloadChoiceKind::ExtractFunctionIsolation:
  case OverloadChoiceKind::KeyPathApplication:
    bindTypeOrIUO(openedType);
    return;
  case OverloadChoiceKind::DeclViaDynamic: {
    // Subscripts have optionality applied to their result type rather than
    // the type of their reference, so there's nothing to adjust here.
    if (isa<SubscriptDecl>(choice.getDecl())) {
      bindTypeOrIUO(openedType);
      return;
    }

    // The opened type of an unbound member reference has optionality applied
    // to the uncurried type.
    if (!doesMemberRefApplyCurriedSelf(choice.getBaseType(),
                                       choice.getDecl())) {
      bindTypeOrIUO(openedType);
      return;
    }

    // Build an outer disjunction to attempt binding both T? and T, then bind
    // as normal. This is needed to correctly handle e.g IUO properties which
    // may need two levels of optionality unwrapped T??.
    auto outerTy = createTypeVariable(locator, TVO_CanBindToLValue);
    buildDisjunctionForDynamicLookupResult(outerTy, openedType, locator);
    bindTypeOrIUO(outerTy);
    return;
  }
  case OverloadChoiceKind::DynamicMemberLookup: {
    auto stringLiteral =
        TypeChecker::getProtocol(getASTContext(), choice.getDecl()->getLoc(),
                                 KnownProtocolKind::ExpressibleByStringLiteral);
    if (!stringLiteral)
      return;

    // Form constraints for a x[dynamicMember:] subscript with a string literal
    // argument, where the overload type is bound to the result to model the
    // fact that this a property access in the source.
    auto argTy = createTypeVariable(locator, /*options*/ 0);
    addConstraint(ConstraintKind::LiteralConformsTo, argTy,
                  stringLiteral->getDeclaredInterfaceType(), locator);
    addDynamicMemberSubscriptConstraints(argTy, /*resultTy*/ boundType);
    return;
  }
  case OverloadChoiceKind::KeyPathDynamicMemberLookup: {
    auto *fnType = openedType->castTo<FunctionType>();
    assert(fnType->getParams().size() == 1 &&
           "subscript always has one argument");
    // Parameter type is KeyPath<T, U> where `T` is a root type
    // and U is a leaf type (aka member type).
    auto paramTy = fnType->getParams()[0].getPlainType();

    if (auto *existential = paramTy->getAs<ExistentialType>()) {
      paramTy = existential->getSuperclass();
      assert(isKnownKeyPathType(paramTy));
    }

    auto keyPathTy = paramTy->castTo<BoundGenericType>();

    auto *keyPathDecl = keyPathTy->getAnyNominal();
    assert(isKnownKeyPathType(keyPathTy) &&
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

    // Check the current depth of applied dynamic member lookups, if we've
    // exceeded the limit then record a fix and set a hole for the member.
    unsigned lookupDepth = [&]() {
      auto path = keyPathLoc->getPath();
      auto iter = path.begin();
      (void)keyPathLoc->findFirst<LocatorPathElt::KeyPathDynamicMember>(iter);
      return path.end() - iter;
    }();
    if (lookupDepth > ctx.TypeCheckerOpts.DynamicMemberLookupDepthLimit) {
      (void)recordFix(TooManyDynamicMemberLookups::create(
          *this, DeclNameRef(choice.getName()), locator));
      recordTypeVariablesAsHoles(memberTy);
    } else {
      addValueMemberConstraint(
          LValueType::get(rootTy), memberName, memberTy, useDC,
          isSubscriptRef ? FunctionRefInfo::doubleBaseNameApply()
                         : FunctionRefInfo::unappliedBaseName(),
          /*outerAlternatives=*/{}, keyPathLoc);
    }

    // In case of subscript things are more complicated comparing to "dot"
    // syntax, because we have to get "applicable function" constraint
    // associated with index expression and re-bind it to match "member type"
    // looked up by dynamically.
    if (isSubscriptRef) {
      // Make sure that regular subscript declarations (if any) are
      // preferred over key path dynamic member lookup.
      increaseScore(SK_KeyPathSubscript, locator);

      auto boundTypeVar = boundType->castTo<TypeVariableType>();
      auto constraints = getConstraintGraph().gatherNearbyConstraints(
          boundTypeVar,
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

      // The function type of the original call-site. We'll want to create a
      // new applicable fn constraint using its parameter along with a fresh
      // type variable for the result of the inner subscript.
      auto originalCallerTy =
          applicableFn->getFirstType()->castTo<FunctionType>();

      auto subscriptResultTy = createTypeVariable(
          getConstraintLocator(locator->getAnchor(),
                               ConstraintLocator::FunctionResult),
          TVO_CanBindToLValue | TVO_CanBindToNoEscape);

      // FIXME: Verify ExtInfo state is correct, not working by accident.
      FunctionType::ExtInfo info;
      auto adjustedFnTy = FunctionType::get(originalCallerTy->getParams(),
                                            subscriptResultTy, info);

      // Add a constraint for the inner application that uses the args of the
      // original call-site, and a fresh type var result equal to the leaf type.
      ConstraintLocatorBuilder kpLocBuilder(keyPathLoc);
      addApplicationConstraint(
          adjustedFnTy, memberTy, /*trailingClosureMatching=*/std::nullopt,
          useDC,
          kpLocBuilder.withPathElement(ConstraintLocator::ApplyFunction));

      addConstraint(ConstraintKind::Equal, subscriptResultTy, leafTy,
                    keyPathLoc);

      addDynamicMemberSubscriptConstraints(/*argTy*/ paramTy,
                                           originalCallerTy->getResult());

      // Bind the overload type to the opened type as usual to match the fact
      // that this is a subscript in the source.
      bindTypeOrIUO(fnType);
    } else {
      // Since member type is going to be bound to "leaf" generic parameter
      // of the keypath, it has to be an r-value always, so let's add a new
      // constraint to represent that conversion instead of loading member
      // type into "leaf" directly.
      addConstraint(ConstraintKind::Equal, memberTy, leafTy, keyPathLoc);

      // Form constraints for a x[dynamicMember:] subscript with a key path
      // argument, where the overload type is bound to the result to model the
      // fact that this a property access in the source.
      addDynamicMemberSubscriptConstraints(/*argTy*/ paramTy, boundType);
    }
    return;
  }
  }
  llvm_unreachable("Unhandled OverloadChoiceKind in switch.");
}

static unsigned getApplicationLevel(ConstraintSystem &CS, Type baseTy,
                                    UnresolvedDotExpr *UDE) {
  unsigned level = 0;

  // If base is a metatype it would be ignored (unless this is an initializer
  // call), but if it is some other type it means that we have a single
  // application level already.
  if (!baseTy->is<MetatypeType>())
    ++level;

  if (auto *call = dyn_cast_or_null<CallExpr>(CS.getParentExpr(UDE))) {
    // Reference is applied only if it appears in a function position
    // in the parent call expression - i.e. `x(...)` vs. `y(x)`,
    // the latter doesn't have `x` applied.
    if (UDE == call->getFn()->getSemanticsProvidingExpr())
      level += 1;
  }

  return level;
}

/// Try to identify and fix failures related to partial function application
/// e.g. partial application of `init` or 'mutating' instance methods.
static std::pair<bool, unsigned>
isInvalidPartialApplication(ConstraintSystem &cs,
                            const AbstractFunctionDecl *member,
                            ConstraintLocator *locator) {
  // If this is a compiler synthesized implicit conversion, let's skip
  // the check because the base of `UDE` is not the base of the injected
  // initializer.
  if (locator->isLastElement<LocatorPathElt::ConstructorMember>() &&
      locator->findFirst<LocatorPathElt::ImplicitConversion>())
    return {false, 0};

  auto *UDE = getAsExpr<UnresolvedDotExpr>(locator->getAnchor());
  if (UDE == nullptr)
    return {false,0};

  auto baseTy =
      cs.simplifyType(cs.getType(UDE->getBase()))->getWithoutSpecifierType();

  auto isInvalidIfPartiallyApplied = [&]() {
    if (auto *FD = dyn_cast<FuncDecl>(member)) {
      // 'mutating' instance methods cannot be partially applied.
      if (FD->isMutating())
        return true;

      // Instance methods cannot be referenced on 'super' from a static
      // context.
      if (UDE->getBase()->isSuperExpr() &&
          baseTy->is<MetatypeType>() &&
          !FD->isStatic())
        return true;
    }

    // Another unsupported partial application is related
    // to constructor delegation via 'self.init' or 'super.init'.
    //
    // Note that you can also write 'self.init' or 'super.init'
    // inside a static context -- since 'self' is a metatype there
    // it doesn't have the special delegation meaning that it does
    // in the body of a constructor.
    if (isa<ConstructorDecl>(member) && !baseTy->is<MetatypeType>()) {
      // Check for a `super.init` delegation...
      if (UDE->getBase()->isSuperExpr())
        return true;

      // ... and `self.init` delegation. Note that in a static context,
      // `self.init` is just an ordinary partial application; it's OK
      // because there's no associated instance for delegation.
      if (auto *DRE = dyn_cast<DeclRefExpr>(UDE->getBase())) {
        if (auto *baseDecl = DRE->getDecl()) {
          if (baseDecl->getBaseName() == cs.getASTContext().Id_self)
            return true;
        }
      }
    }

    return false;
  };

  if (!isInvalidIfPartiallyApplied())
    return {false,0};

  return {true, getApplicationLevel(cs, baseTy, UDE)};
}

/// If we're resolving an overload set with a decl that has special type
/// checking semantics, compute the type of the reference.  For now, follow
/// the lead of \c getTypeOfMemberReference and return a pair of
/// the full opened type and the reference's type.
static Type getTypeOfReferenceWithSpecialTypeCheckingSemantics(
    ConstraintSystem &CS, ConstraintLocator *locator,
    DeclTypeCheckingSemantics semantics,
    PreparedOverloadBuilder *preparedOverload) {
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
        TVO_CanBindToNoEscape, preparedOverload);
    auto output = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape, preparedOverload);

    FunctionType::Param inputArg(input,
                                 CS.getASTContext().getIdentifier("of"));

    CS.addConstraint(
        ConstraintKind::DynamicTypeOf, output, input,
        CS.getConstraintLocator(locator, ConstraintLocator::DynamicType),
        /*isFavored=*/false, preparedOverload);
    // FIXME: Verify ExtInfo state is correct, not working by accident.
    FunctionType::ExtInfo info;
    return FunctionType::get({inputArg}, output, info);
  }
  case DeclTypeCheckingSemantics::WithoutActuallyEscaping: {
    // Proceed with a "WithoutActuallyEscaping" operation. The body closure
    // receives a copy of the argument closure that is temporarily made
    // @escaping.
    auto noescapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape, preparedOverload);
    auto escapeClosure = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape, preparedOverload);
    CS.addConstraint(ConstraintKind::EscapableFunctionOf, escapeClosure,
                     noescapeClosure, CS.getConstraintLocator(locator),
                     /*isFavored=*/false, preparedOverload);
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape, preparedOverload);
    auto thrownError = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::ThrownErrorType),
        0, preparedOverload);
    FunctionType::Param arg(escapeClosure);

    auto bodyParamIsolation = FunctionTypeIsolation::forNonIsolated();
    if (CS.getASTContext().LangOpts.hasFeature(
            Feature::NonisolatedNonsendingByDefault)) {
      bodyParamIsolation = FunctionTypeIsolation::forNonIsolatedCaller();
    }

    auto bodyClosure = FunctionType::get(arg, result,
                                         FunctionType::ExtInfoBuilder()
                                             .withNoEscape(true)
                                             .withIsolation(bodyParamIsolation)
                                             .withAsync(true)
                                             .withThrows(true, thrownError)
                                             .build());
    FunctionType::Param args[] = {
      FunctionType::Param(noescapeClosure),
      FunctionType::Param(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };

    auto withoutEscapingIsolation = FunctionTypeIsolation::forNonIsolated();
    if (CS.getASTContext().LangOpts.hasFeature(
            Feature::NonisolatedNonsendingByDefault)) {
      withoutEscapingIsolation = FunctionTypeIsolation::forNonIsolatedCaller();
    }

    return FunctionType::get(args, result,
                             FunctionType::ExtInfoBuilder()
                                 .withNoEscape(false)
                                 .withIsolation(withoutEscapingIsolation)
                                 .withAsync(true)
                                 .withThrows(true, thrownError)
                                 .build());
  }
  case DeclTypeCheckingSemantics::OpenExistential: {
    // The body closure receives a freshly-opened archetype constrained by the
    // existential type as its input.
    auto openedTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape, preparedOverload);
    auto existentialTy = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionArgument),
        TVO_CanBindToNoEscape, preparedOverload);
    CS.addConstraint(ConstraintKind::OpenedExistentialOf, openedTy,
                     existentialTy, CS.getConstraintLocator(locator),
                     /*isFavored=*/false, preparedOverload);
    auto result = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::FunctionResult),
        TVO_CanBindToNoEscape, preparedOverload);
    auto thrownError = CS.createTypeVariable(
        CS.getConstraintLocator(locator, ConstraintLocator::ThrownErrorType),
        0, preparedOverload);
    FunctionType::Param bodyArgs[] = {FunctionType::Param(openedTy)};

    auto bodyParamIsolation = FunctionTypeIsolation::forNonIsolated();
    if (CS.getASTContext().LangOpts.hasFeature(
            Feature::NonisolatedNonsendingByDefault)) {
      bodyParamIsolation = FunctionTypeIsolation::forNonIsolatedCaller();
    }

    auto bodyClosure = FunctionType::get(bodyArgs, result,
                                         FunctionType::ExtInfoBuilder()
                                             .withNoEscape(true)
                                             .withThrows(true, thrownError)
                                             .withIsolation(bodyParamIsolation)
                                             .withAsync(true)
                                             .build());
    FunctionType::Param args[] = {
      FunctionType::Param(existentialTy),
      FunctionType::Param(bodyClosure, CS.getASTContext().getIdentifier("do")),
    };

    auto openExistentialIsolation = FunctionTypeIsolation::forNonIsolated();
    if (CS.getASTContext().LangOpts.hasFeature(
            Feature::NonisolatedNonsendingByDefault)) {
      openExistentialIsolation = FunctionTypeIsolation::forNonIsolatedCaller();
    }

    return FunctionType::get(args, result,
                             FunctionType::ExtInfoBuilder()
                                 .withNoEscape(false)
                                 .withThrows(true, thrownError)
                                 .withIsolation(openExistentialIsolation)
                                 .withAsync(true)
                                 .build());
  }
  }

  llvm_unreachable("Unhandled DeclTypeCheckingSemantics in switch.");
}

void ConstraintSystem::recordResolvedOverload(ConstraintLocator *locator,
                                              SelectedOverload overload) {
  bool inserted = ResolvedOverloads.insert({locator, overload}).second;
  ASSERT(inserted);

  if (solverState)
    recordChange(SolverTrail::Change::ResolvedOverload(locator));
}

void ConstraintSystem::replayChanges(
    ConstraintLocator *locator,
    PreparedOverload *preparedOverload) {
  for (auto change : preparedOverload->getChanges()) {
    switch (change.Kind) {
    case PreparedOverload::Change::AddedTypeVariable:
      addTypeVariable(change.TypeVar);
      break;

    case PreparedOverload::Change::AddedConstraint:
      simplifyDisjunctionChoice(change.TheConstraint);
      break;

    case PreparedOverload::Change::AddedBindConstraint:
      addConstraint(ConstraintKind::Bind,
                    change.Bind.FirstType,
                    change.Bind.SecondType,
                    locator, /*isFavored=*/false);
      break;

    case PreparedOverload::Change::OpenedTypes: {
      ArrayRef<OpenedType> replacements(
          change.Replacements.Data,
          change.Replacements.Count);

      // FIXME: Get rid of this conditional.
      if (getOpenedTypes(locator).empty())
        recordOpenedType(locator, replacements);
      break;
    }

    case PreparedOverload::Change::OpenedExistentialType: {
      recordOpenedExistentialType(locator,
                                  change.TheExistential);
      break;
    }

    case PreparedOverload::Change::OpenedPackExpansionType:
      recordOpenedPackExpansionType(
          change.PackExpansion.TheExpansion,
          change.PackExpansion.TypeVar);
      break;

    case PreparedOverload::Change::AppliedPropertyWrapper: {
      Expr *anchor = getAsExpr(locator->getAnchor());
      applyPropertyWrapper(anchor,
          { Type(change.PropertyWrapper.WrapperType),
            change.PropertyWrapper.InitKind });
      break;
    }

    case PreparedOverload::Change::AddedFix:
      recordFix(change.Fix.TheFix, change.Fix.Impact);
      break;

    case PreparedOverload::Change::RecordedNodeType:
      setType(change.Node.Node, change.Node.TheType);
      break;
    }
  }
}

/// Populate the prepared overload with all type variables and constraints
/// that are to be introduced into the constraint system when this choice
/// is taken.
///
/// Returns a pair consisting of the opened type, and the thrown error type.
///
/// FIXME: As a transitional mechanism, if preparedOverload is nullptr, this
/// immediately performs all operations.
std::pair<Type, Type>
ConstraintSystem::prepareOverloadImpl(OverloadChoice choice,
                                      DeclContext *useDC,
                                      ConstraintLocator *locator,
                                      PreparedOverloadBuilder *preparedOverload) {
  // If we refer to a top-level decl with special type-checking semantics,
  // handle it now.
  auto semantics =
      TypeChecker::getDeclTypeCheckingSemantics(choice.getDecl());
  if (semantics != DeclTypeCheckingSemantics::Normal) {
    auto openedType = getTypeOfReferenceWithSpecialTypeCheckingSemantics(
        *this, locator, semantics, preparedOverload);
    return {openedType, Type()};
  } else if (choice.getBaseType()) {
    return getTypeOfMemberReferencePre(choice, useDC, locator, preparedOverload);
  } else {
    return getTypeOfReferencePre(choice, useDC, locator, preparedOverload);
  }
}

PreparedOverload *ConstraintSystem::prepareOverload(OverloadChoice choice,
                                                    DeclContext *useDC,
                                                    ConstraintLocator *locator) {
  ASSERT(!PreparingOverload);
  PreparingOverload = true;

  PreparedOverloadBuilder builder;
  Type openedType;
  Type thrownErrorType;
  std::tie(openedType, thrownErrorType) = prepareOverloadImpl(
      choice, useDC, locator, &builder);

  PreparingOverload = false;

  size_t count = builder.Changes.size();
  auto size = PreparedOverload::totalSizeToAlloc<PreparedOverload::Change>(count);
  auto mem = Allocator.Allocate(size, alignof(PreparedOverload));

  return new (mem) PreparedOverload(openedType, thrownErrorType, builder.Changes);
}

void ConstraintSystem::resolveOverload(OverloadChoice choice, DeclContext *useDC,
                                       ConstraintLocator *locator, Type boundType,
                                       PreparedOverload *preparedOverload) {
  // Determine the type to which we'll bind the overload set's type.
  DeclReferenceType declRefType;

  switch (choice.getKind()) {
  case OverloadChoiceKind::Decl:
  case OverloadChoiceKind::DeclViaBridge:
  case OverloadChoiceKind::DeclViaDynamic:
  case OverloadChoiceKind::DeclViaUnwrappedOptional:
  case OverloadChoiceKind::DynamicMemberLookup:
  case OverloadChoiceKind::KeyPathDynamicMemberLookup: {
    Type openedType, thrownErrorType;

    if (preparedOverload) {
      replayChanges(locator, preparedOverload);

      openedType = preparedOverload->getOpenedType();
      thrownErrorType = preparedOverload->getThrownErrorType();
    } else {
      std::tie(openedType, thrownErrorType) = prepareOverloadImpl(
          choice, useDC, locator, nullptr);
    }

    auto semantics =
        TypeChecker::getDeclTypeCheckingSemantics(choice.getDecl());
    if (semantics != DeclTypeCheckingSemantics::Normal) {
      declRefType.openedType = openedType;
      declRefType.adjustedOpenedType = openedType;
      declRefType.referenceType = openedType;
      declRefType.adjustedReferenceType = openedType;
      declRefType.thrownErrorTypeOnAccess = thrownErrorType;
    } else {
      if (choice.getBaseType()) {
        declRefType = getTypeOfMemberReferencePost(
            choice, useDC, locator, openedType, thrownErrorType);
      } else {
        declRefType = getTypeOfReferencePost(
            choice, useDC, locator, openedType, thrownErrorType);
      }
    }

    break;
  }

  case OverloadChoiceKind::TupleIndex:
    if (auto lvalueTy = choice.getBaseType()->getAs<LValueType>()) {
      // When the base of a tuple lvalue, the member is always an lvalue.
      auto tuple = lvalueTy->getObjectType()->castTo<TupleType>();
      declRefType.adjustedReferenceType =
          LValueType::get(
              tuple->getElementType(choice.getTupleIndex())->getRValueType());
    } else {
      // When the base is a tuple rvalue, the member is always an rvalue.
      auto tuple = choice.getBaseType()->castTo<TupleType>();
      declRefType.adjustedReferenceType =
          tuple->getElementType(choice.getTupleIndex())->getRValueType();
    }
    declRefType.referenceType = declRefType.adjustedReferenceType;
    break;

  case OverloadChoiceKind::MaterializePack: {
    // Since pack expansion is only applicable to single element tuples at the
    // moment we can just look through l-value base to load it.
    //
    // In the future, _if_ the syntax allows for multiple expansions
    // this code would have to be adjusted to project l-value from the
    // base type just like TupleIndex does.
    declRefType.adjustedReferenceType =
        getPatternTypeOfSingleUnlabeledPackExpansionTuple(choice.getBaseType());
    declRefType.referenceType = declRefType.adjustedReferenceType;
    break;
  }

  case OverloadChoiceKind::ExtractFunctionIsolation: {
    // The type of `.isolation` is `(any Actor)?`
    auto actor = getASTContext().getProtocol(KnownProtocolKind::Actor);
    declRefType.adjustedReferenceType =
        OptionalType::get(actor->getDeclaredExistentialType());
    declRefType.referenceType = declRefType.adjustedReferenceType;
    break;
  }

  case OverloadChoiceKind::KeyPathApplication: {
    // Key path application looks like a subscript(keyPath: KeyPath<Base, T>).
    // The element type is T or @lvalue T based on the key path subtype and
    // the mutability of the base.
    auto *keyPathIndexLoc =
        getConstraintLocator(locator, ConstraintLocator::KeyPathSubscriptIndex);
    auto keyPathIndexTy = createTypeVariable(keyPathIndexLoc,
                                             /*options=*/0);
    auto elementTy = createTypeVariable(
        getConstraintLocator(keyPathIndexLoc, ConstraintLocator::KeyPathValue),
        TVO_CanBindToLValue | TVO_CanBindToNoEscape);

    // The element result is an lvalue or rvalue based on the key path class.
    addKeyPathApplicationConstraint(
                  keyPathIndexTy, choice.getBaseType(), elementTy, locator);

    FunctionType::Param indices[] = {
      FunctionType::Param(keyPathIndexTy, getASTContext().Id_keyPath),
    };
    // FIXME: Verify ExtInfo state is correct, not working by accident.
    FunctionType::ExtInfo subscriptInfo;
    auto subscriptTy = FunctionType::get(indices, elementTy, subscriptInfo);

    FunctionType::Param baseParam(choice.getBaseType());
    // FIXME: Verify ExtInfo state is correct, not working by accident.
    FunctionType::ExtInfo fullInfo;
    auto fullTy = FunctionType::get({baseParam}, subscriptTy, fullInfo);
    declRefType.openedType = fullTy;
    declRefType.adjustedOpenedType = fullTy;
    // FIXME: @preconcurrency
    declRefType.referenceType = subscriptTy;
    declRefType.adjustedReferenceType = subscriptTy;

    // Increase the score so that actual subscripts get preference.
    // ...except if we're solving for code completion and the index expression
    // contains the completion location
    auto SE = getAsExpr<SubscriptExpr>(locator->getAnchor());
    if (!isForCodeCompletion() ||
        (SE && !containsIDEInspectionTarget(SE->getArgs()))) {
      increaseScore(SK_KeyPathSubscript, locator);
    }
    break;
  }
  }
  assert(!declRefType.referenceType->hasTypeParameter() &&
         "Cannot have a dependent type here");
  assert(!declRefType.adjustedReferenceType->hasTypeParameter() &&
         "Cannot have a dependent type here");

  if (auto *decl = choice.getDeclOrNull()) {
    // If we're choosing an asynchronous declaration within a synchronous
    // context, or vice-versa, increase the async/async mismatch score.
    if (auto func = dyn_cast<AbstractFunctionDecl>(decl)) {
      if (!Options.contains(ConstraintSystemFlags::IgnoreAsyncSyncMismatch) &&
          !func->hasPolymorphicEffect(EffectKind::Async) &&
          func->isAsyncContext() != isAsynchronousContext(useDC)) {
        increaseScore(func->isAsyncContext() ? SK_AsyncInSyncMismatch
                                             : SK_SyncInAsync,
                      locator);
      }
    }

    if (isa<SubscriptDecl>(decl)) {
      if (locator->isResultOfKeyPathDynamicMemberLookup() ||
          locator->isKeyPathSubscriptComponent()) {
        // Subscript type has a format of (Self[.Type) -> (Arg...) -> Result
        auto declTy = declRefType.adjustedOpenedType->castTo<FunctionType>();
        auto subscriptTy = declTy->getResult()->castTo<FunctionType>();
        // If we have subscript, each of the arguments has to conform to
        // Hashable, because it would be used as a component inside key path.
        for (auto index : indices(subscriptTy->getParams())) {
          const auto &param = subscriptTy->getParams()[index];
          verifyThatArgumentIsHashable(index, param.getParameterType(), locator,
                                       choice.getDecl()->getLoc());
        }
      }
    }

    if (isa<AbstractFunctionDecl>(decl) || isa<TypeDecl>(decl)) {
      auto anchor = locator->getAnchor();
      // TODO: Instead of not increasing the score for arguments to #selector,
      // a better fix for this is to port over the #selector diagnostics from
      // CSApply to constraint fixes, and not attempt invalid disjunction
      // choices based on the selector kind on the valid code path.
      if (choice.getFunctionRefInfo().isUnappliedBaseName() &&
          !UnevaluatedRootExprs.contains(getAsExpr(anchor))) {
        increaseScore(SK_UnappliedFunction, locator);
      }
    }

    if (auto *afd = dyn_cast<AbstractFunctionDecl>(decl)) {
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
          isInvalidPartialApplication(*this, afd, locator);

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

    // If we have a macro, check for correct usage.
    if (auto macro = dyn_cast<MacroDecl>(decl)) {
      // Macro can only be used in an expansion. If we end up here, it's
      // because we found a macro but are missing the leading '#'.
      if (!locator->isForMacroExpansion()) {
        // Record a fix here
        (void)recordFix(MacroMissingPound::create(*this, macro, locator));
      }

      // The default type of the #isolation builtin macro is `(any Actor)?`
      if (macro->getBuiltinKind() == BuiltinMacroKind::IsolationMacro) {
        auto *fnType = declRefType.openedType->getAs<FunctionType>();
        auto actor = getASTContext().getProtocol(KnownProtocolKind::Actor);
        addConstraint(
            ConstraintKind::Defaultable, fnType->getResult(),
            OptionalType::get(actor->getDeclaredExistentialType()),
            locator);
      }
    }
  }

  // If accessing this declaration could throw an error, record this as a
  // potential throw site.
  if (declRefType.thrownErrorTypeOnAccess) {
    recordPotentialThrowSite(
        PotentialThrowSite::PropertyAccess,
        declRefType.thrownErrorTypeOnAccess,
        locator);
  }

  // Note that we have resolved this overload.
  auto overload = SelectedOverload{
      choice,
      declRefType.openedType,
      declRefType.adjustedOpenedType,
      declRefType.referenceType,
      declRefType.adjustedReferenceType,
      boundType};
  recordResolvedOverload(locator, overload);

  // Add the constraints necessary to bind the overload type.
  bindOverloadType(overload, boundType, locator, useDC);

  if (isDebugMode()) {
    PrintOptions PO;
    PO.PrintTypesForDebugging = true;

    auto &log = llvm::errs();
    log.indent(solverState ? solverState->getCurrentIndent() : 2);
    log << "(overload set choice binding ";
    boundType->print(log, PO);
    log << " := ";
    declRefType.adjustedReferenceType->print(log, PO);

    auto openedAtLoc = getOpenedTypes(locator);
    if (!openedAtLoc.empty()) {
      log << " [";
      llvm::interleave(
          openedAtLoc.begin(), openedAtLoc.end(),
          [&](OpenedType opened) {
            opened.second->getImpl().getGenericParameter()->print(log, PO);
            log << " := ";
            Type(opened.second).print(log, PO);
          },
          [&]() { log << ", "; });
      log << "]";
    }
    log << ")\n";
  }

  if (auto *decl = choice.getDeclOrNull()) {
    // If this is an existential member access and adjustments were made to the
    // member reference type, require that the constraint system is happy with
    // the ensuing conversion.
    if (auto baseTy = choice.getBaseType()) {
      baseTy = getFixedTypeRecursive(baseTy, /*wantRValue=*/true);
      const auto instanceTy = baseTy->getMetatypeInstanceType();

      if (isExistentialMemberAccessWithExplicitBaseExpression(
              instanceTy, decl, locator,
              /*isDynamicLookup=*/choice.getKind() ==
                  OverloadChoiceKind::DeclViaDynamic)) {

        // Strip curried 'self' parameters.
        auto fromTy = declRefType.openedType
            ->castTo<AnyFunctionType>()->getResult();
        auto toTy = declRefType.referenceType;
        if (!doesMemberRefApplyCurriedSelf(baseTy, decl)) {
          toTy = toTy->castTo<AnyFunctionType>()->getResult();
        }

        if (!fromTy->isEqual(toTy)) {
          ConstraintLocatorBuilder conversionLocator = locator;
          conversionLocator = conversionLocator.withPathElement(
              ConstraintLocator::ExistentialMemberAccessConversion);
          addConstraint(ConstraintKind::Conversion, fromTy, toTy,
                        conversionLocator);
        }
      }
    }

    // If the declaration is unavailable, note that in the score.
    if (isDeclUnavailable(decl, locator))
      increaseScore(SK_Unavailable, locator);

    // If the declaration is from a module that hasn't been imported, note that.
    if (getASTContext().LangOpts.hasFeature(Feature::MemberImportVisibility,
                                            /*allowMigration=*/true)) {
      if (!useDC->isDeclImported(decl))
        increaseScore(SK_MissingImport, locator);
    }

    // If this overload is disfavored, note that.
    if (decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>())
      increaseScore(SK_DisfavoredOverload, locator);
  }

  if (choice.isFallbackMemberOnUnwrappedBase()) {
    increaseScore(SK_UnresolvedMemberViaOptional, locator);
  }
}

void ConstraintSystem::verifyThatArgumentIsHashable(unsigned index,
                                                    Type argType,
                                                    ConstraintLocator *locator,
                                                    SourceLoc loc) {
  if (auto *hashable = TypeChecker::getProtocol(argType->getASTContext(), loc,
                                                KnownProtocolKind::Hashable)) {
    addConstraint(
        ConstraintKind::ConformsTo, argType,
        hashable->getDeclaredInterfaceType(),
        getConstraintLocator(locator, LocatorPathElt::TupleElement(index)));
  }
}
