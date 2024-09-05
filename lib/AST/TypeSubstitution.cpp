//===--- TypeSubstitution.cpp - Generic Type Substitution -----------------===//
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
// This file implements Type::subst(), TypeBase::getContextSubstitutionMap()
// and related functionality.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/InFlightSubstitution.h"
#include "swift/AST/Module.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeTransform.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"

using namespace swift;

//===----------------------------------------------------------------------===//
// Type::subst() and friends
//===----------------------------------------------------------------------===//

Type QueryReplacementTypeArray::operator()(SubstitutableType *type) const {
  auto *genericParam = cast<GenericTypeParamType>(type);
  auto genericParams = sig.getGenericParams();
  auto replacementIndex =
    GenericParamKey(genericParam).findIndexIn(genericParams);
  return types[replacementIndex];
}

Type QueryTypeSubstitutionMap::operator()(SubstitutableType *type) const {
  auto key = type->getCanonicalType()->castTo<SubstitutableType>();
  auto known = substitutions.find(key);
  if (known != substitutions.end() && known->second)
    return known->second;

  // Not known.
  return Type();
}

Type QuerySubstitutionMap::operator()(SubstitutableType *type) const {
  return subMap.lookupSubstitution(cast<GenericTypeParamType>(type));
}

FunctionType *
GenericFunctionType::substGenericArgs(SubstitutionMap subs,
                                      SubstOptions options) {
  return substGenericArgs(
    [=](Type t) { return t.subst(subs, options); });
}

FunctionType *GenericFunctionType::substGenericArgs(
    llvm::function_ref<Type(Type)> substFn) const {
  llvm::SmallVector<AnyFunctionType::Param, 4> params;
  params.reserve(getNumParams());

  llvm::transform(getParams(), std::back_inserter(params),
                  [&](const AnyFunctionType::Param &param) {
                    return param.withType(substFn(param.getPlainType()));
                  });

  auto resultTy = substFn(getResult());

  Type thrownError = getThrownError();
  if (thrownError)
    thrownError = substFn(thrownError);

  // Build the resulting (non-generic) function type.
  return FunctionType::get(params, resultTy,
                           getExtInfo().withThrows(isThrowing(), thrownError));
}

CanFunctionType
CanGenericFunctionType::substGenericArgs(SubstitutionMap subs) const {
  return cast<FunctionType>(
           getPointer()->substGenericArgs(subs)->getCanonicalType());
}

static Type getMemberForBaseType(InFlightSubstitution &IFS,
                                 Type origBase,
                                 Type substBase,
                                 AssociatedTypeDecl *assocType,
                                 Identifier name,
                                 unsigned level) {
  // Produce a dependent member type for the given base type.
  auto getDependentMemberType = [&](Type baseType) {
    if (assocType)
      return DependentMemberType::get(baseType, assocType);

    return DependentMemberType::get(baseType, name);
  };

  // Produce a failed result.
  auto failed = [&]() -> Type {
    Type baseType = ErrorType::get(substBase ? substBase : origBase);
    if (assocType)
      return DependentMemberType::get(baseType, assocType);

    return DependentMemberType::get(baseType, name);
  };

  if (auto *selfType = substBase->getAs<DynamicSelfType>())
    substBase = selfType->getSelfType();

  // If the parent is a type variable or a member rooted in a type variable,
  // or if the parent is a type parameter, we're done. Also handle
  // UnresolvedType here, which can come up in diagnostics.
  if (substBase->isTypeVariableOrMember() ||
      substBase->isTypeParameter() ||
      substBase->is<UnresolvedType>())
    return getDependentMemberType(substBase);

  // All remaining cases require an associated type declaration and not just
  // the name of a member type.
  if (!assocType)
    return failed();

  // If the parent is an archetype, extract the child archetype with the
  // given name.
  if (auto archetypeParent = substBase->getAs<ArchetypeType>()) {
    if (Type memberArchetypeByName = archetypeParent->getNestedType(assocType))
      return memberArchetypeByName;

    // If looking for an associated type and the archetype is constrained to a
    // class, continue to the default associated type lookup
    if (!assocType || !archetypeParent->getSuperclass())
      return failed();
  }

  auto proto = assocType->getProtocol();
  ProtocolConformanceRef conformance =
    IFS.lookupConformance(origBase->getCanonicalType(), substBase,
                          proto, level);

  if (conformance.isInvalid())
    return failed();

  Type witnessTy;

  // Retrieve the type witness.
  if (conformance.isPack()) {
    auto *packConformance = conformance.getPack();

    witnessTy = packConformance->getAssociatedType(
        assocType->getDeclaredInterfaceType());
  } else if (conformance.isConcrete()) {
    auto witness =
        conformance.getConcrete()->getTypeWitnessAndDecl(assocType,
                                                         IFS.getOptions());

    witnessTy = witness.getWitnessType();
    if (!witnessTy || witnessTy->hasError())
      return failed();

    // This is a hacky feature allowing code completion to migrate to
    // using Type::subst() without changing output.
    if (IFS.getOptions() & SubstFlags::DesugarMemberTypes) {
      if (auto *aliasType = dyn_cast<TypeAliasType>(witnessTy.getPointer()))
        witnessTy = aliasType->getSinglyDesugaredType();

      // Another hack. If the type witness is a opaque result type. They can
      // only be referred using the name of the associated type.
      if (witnessTy->is<OpaqueTypeArchetypeType>())
        witnessTy = witness.getWitnessDecl()->getDeclaredInterfaceType();
    }
  }

  if (!witnessTy || witnessTy->is<ErrorType>())
    return failed();

  return witnessTy;
}

ProtocolConformanceRef LookUpConformanceInModule::
operator()(CanType dependentType, Type conformingReplacementType,
           ProtocolDecl *conformedProtocol) const {
  return lookupConformance(conformingReplacementType,
                           conformedProtocol,
                           /*allowMissing=*/true);
}

ProtocolConformanceRef LookUpConformanceInSubstitutionMap::
operator()(CanType dependentType, Type conformingReplacementType,
           ProtocolDecl *conformedProtocol) const {
  // Lookup conformances for archetypes that conform concretely
  // via a superclass.
  if (auto archetypeType = conformingReplacementType->getAs<ArchetypeType>()) {
    return lookupConformance(
        conformingReplacementType, conformedProtocol,
        /*allowMissing=*/true);
  }

  if (isa<PrimaryArchetypeType>(dependentType) ||
      isa<PackArchetypeType>(dependentType))
    dependentType = dependentType->mapTypeOutOfContext()->getCanonicalType();

  auto result = Subs.lookupConformance(dependentType, conformedProtocol);
  if (!result.isInvalid())
    return result;

  // Otherwise, the original type might be fixed to a concrete type in
  // the substitution map's input generic signature.
  if (auto genericSig = Subs.getGenericSignature()) {
    if (genericSig->isValidTypeParameter(dependentType) &&
        genericSig->isConcreteType(dependentType)) {
      return lookupConformance(
          conformingReplacementType, conformedProtocol,
          /*allowMissing=*/true);
    }
  }

  return ProtocolConformanceRef::forInvalid();
}

ProtocolConformanceRef MakeAbstractConformanceForGenericType::
operator()(CanType dependentType, Type conformingReplacementType,
           ProtocolDecl *conformedProtocol) const {
  // The places that use this can also produce conformance packs, generally
  // just for singleton pack expansions.
  if (auto conformingPack = conformingReplacementType->getAs<PackType>()) {
    SmallVector<ProtocolConformanceRef, 4> conformances;
    for (auto conformingPackElt : conformingPack->getElementTypes()) {
      // Look through pack expansions; there's no equivalent conformance
      // expansion right now.
      auto expansion = conformingPackElt->getAs<PackExpansionType>();
      if (expansion) conformingPackElt = expansion->getPatternType();

      auto conformance =
        (*this)(dependentType, conformingPackElt, conformedProtocol);
      conformances.push_back(conformance);
    }
    return ProtocolConformanceRef(
        PackConformance::get(conformingPack, conformedProtocol, conformances));
  }

  assert((conformingReplacementType->is<ErrorType>() ||
          conformingReplacementType->is<SubstitutableType>() ||
          conformingReplacementType->is<DependentMemberType>() ||
          conformingReplacementType->hasTypeVariable()) &&
         "replacement requires looking up a concrete conformance");
  // A class-constrained archetype might conform to the protocol
  // concretely.
  if (auto *archetypeType = conformingReplacementType->getAs<ArchetypeType>()) {
    if (auto superclassType = archetypeType->getSuperclass()) {
      return lookupConformance(archetypeType, conformedProtocol);
    }
  }
  return ProtocolConformanceRef(conformedProtocol);
}

Type DependentMemberType::substBaseType(Type substBase) {
  return substBaseType(substBase, LookUpConformanceInModule(),
                       std::nullopt);
}

Type DependentMemberType::substBaseType(Type substBase,
                                        LookupConformanceFn lookupConformance,
                                        SubstOptions options) {
  if (substBase.getPointer() == getBase().getPointer() &&
      substBase->hasTypeParameter())
    return this;

  InFlightSubstitution IFS(nullptr, lookupConformance, options);
  return getMemberForBaseType(IFS, getBase(), substBase,
                              getAssocType(), getName(),
                              /*level=*/0);
}

Type DependentMemberType::substRootParam(Type newRoot,
                                         LookupConformanceFn lookupConformance,
                                         SubstOptions options) {
  auto base = getBase();
  if (base->is<GenericTypeParamType>()) {
    return substBaseType(newRoot, lookupConformance, options);
  }
  if (auto depMem = base->getAs<DependentMemberType>()) {
    return substBaseType(depMem->substRootParam(newRoot, lookupConformance, options),
                         lookupConformance, options);
  }
  return Type();
}

static Type substGenericFunctionType(GenericFunctionType *genericFnType,
                                     InFlightSubstitution &IFS) {
  // Substitute into the function type (without generic signature).
  auto *bareFnType = FunctionType::get(genericFnType->getParams(),
                                       genericFnType->getResult(),
                                       genericFnType->getExtInfo());
  Type result = Type(bareFnType).subst(IFS);
  if (!result || result->is<ErrorType>()) return result;

  auto *fnType = result->castTo<FunctionType>();
  // Substitute generic parameters.
  bool anySemanticChanges = false;
  SmallVector<GenericTypeParamType *, 2> genericParams;
  for (auto param : genericFnType->getGenericParams()) {
    Type paramTy = Type(param).subst(IFS);
    if (!paramTy)
      return Type();

    if (auto newParam = paramTy->getAs<GenericTypeParamType>()) {
      if (!newParam->isEqual(param))
        anySemanticChanges = true;

      genericParams.push_back(newParam);
    } else {
      anySemanticChanges = true;
    }
  }

  // If no generic parameters remain, this is a non-generic function type.
  if (genericParams.empty())
    return result;

  // Transform requirements.
  SmallVector<Requirement, 2> requirements;
  for (const auto &req : genericFnType->getRequirements()) {
    // Substitute into the requirement.
    auto substReqt = req.subst(IFS);

    // Did anything change?
    if (!anySemanticChanges &&
        (!req.getFirstType()->isEqual(substReqt.getFirstType()) ||
         (req.getKind() != RequirementKind::Layout &&
          !req.getSecondType()->isEqual(substReqt.getSecondType())))) {
      anySemanticChanges = true;
    }

    requirements.push_back(substReqt);
  }

  GenericSignature genericSig;
  if (anySemanticChanges) {
    // If there were semantic changes, we need to build a new generic
    // signature.
    ASTContext &ctx = genericFnType->getASTContext();
    genericSig = buildGenericSignature(ctx, GenericSignature(),
                                       genericParams, requirements,
                                       /*allowInverses=*/false);
  } else {
    // Use the mapped generic signature.
    genericSig = GenericSignature::get(genericParams, requirements);
  }

  // Produce the new generic function type.
  return GenericFunctionType::get(genericSig, fnType->getParams(),
                                  fnType->getResult(), fnType->getExtInfo());
}

InFlightSubstitution::InFlightSubstitution(TypeSubstitutionFn substType,
                                           LookupConformanceFn lookupConformance,
                                           SubstOptions options)
  : Options(options),
    BaselineSubstType(substType),
    BaselineLookupConformance(lookupConformance) {
  // FIXME: Don't substitute type parameters if one of the special flags is set.
  Props |= RecursiveTypeProperties::HasTypeParameter;

  // If none of the special flags are set, we substitute type parameters and
  // primary archetypes only.
  if (!Options.contains(SubstFlags::SubstitutePrimaryArchetypes) &&
      !Options.contains(SubstFlags::SubstituteLocalArchetypes) &&
      !Options.contains(SubstFlags::SubstituteOpaqueArchetypes)) {
    Props |= RecursiveTypeProperties::HasPrimaryArchetype;
  }

  if (Options.contains(SubstFlags::SubstitutePrimaryArchetypes))
    Props |= RecursiveTypeProperties::HasPrimaryArchetype;

  if (Options.contains(SubstFlags::SubstituteLocalArchetypes)) {
    Props |= RecursiveTypeProperties::HasOpenedExistential;
    Props |= RecursiveTypeProperties::HasElementArchetype;
  }

  if (Options.contains(SubstFlags::SubstituteOpaqueArchetypes))
    Props |= RecursiveTypeProperties::HasOpaqueArchetype;
}

bool InFlightSubstitution::isInvariant(Type derivedType) const {
  // If none of the bits are set, the type won't be changed by substitution.
  return !(derivedType->getRecursiveProperties().getBits() & Props.getBits());
}

void InFlightSubstitution::expandPackExpansionShape(Type origShape,
    llvm::function_ref<void(Type substComponentShape)> handleComponent) {

  // Substitute the shape using the baseline substitutions, not the
  // current elementwise projections.
  auto substShape = origShape.subst(BaselineSubstType,
                                    BaselineLookupConformance,
                                    Options);

  auto substPackShape = substShape->getAs<PackType>();
  if (!substPackShape) {
    ActivePackExpansions.push_back({/*is subst expansion*/true, 0});
    handleComponent(substShape);
    ActivePackExpansions.pop_back();
    return;
  }

  ActivePackExpansions.push_back({false, 0});
  for (auto substElt : substPackShape->getElementTypes()) {
    auto substExpansion = substElt->getAs<PackExpansionType>();
    auto substExpansionShape =
      (substExpansion ? substExpansion->getCountType() : Type());

    ActivePackExpansions.back().isSubstExpansion =
      (substExpansion != nullptr);
    handleComponent(substExpansionShape);
    ActivePackExpansions.back().expansionIndex++;
  }
  ActivePackExpansions.pop_back();
}

Type InFlightSubstitution::substType(SubstitutableType *origType,
                                     unsigned level) {
  auto substType = BaselineSubstType(origType);
  if (!substType)
    return Type();

  // FIXME: All the logic around 'level' is probably slightly wrong, and in
  // the unlikely event that it is correct, at the very least warrants a
  // detailed explanation.

  if (ActivePackExpansions.empty())
    return substType->increasePackElementLevel(level);

  auto outerExpansions = ArrayRef(ActivePackExpansions).drop_back(level);
  auto innerExpansions = ArrayRef(ActivePackExpansions).take_back(level);

  unsigned outerLevel = 0;
  if (!getOptions().contains(SubstFlags::PreservePackExpansionLevel)) {
    for (const auto &activeExpansion : outerExpansions) {
      if (activeExpansion.isSubstExpansion)
        ++outerLevel;
    }
  } else {
    outerLevel = level;
  }

  unsigned innerLevel = 0;
  for (const auto &activeExpansion : innerExpansions) {
    if (activeExpansion.isSubstExpansion)
      ++innerLevel;
  }

  auto substPackType = substType->getAs<PackType>();
  if (!substPackType)
    return substType->increasePackElementLevel(outerLevel);

  auto &activeExpansion = outerExpansions.back();

  auto index = activeExpansion.expansionIndex;
  assert(index < substPackType->getNumElements() &&
         "replacement for pack parameter did not have the right "
         "size for expansion");
  auto substEltType = substPackType->getElementType(index);
  if (activeExpansion.isSubstExpansion) {
    assert(substEltType->is<PackExpansionType>() &&
           "substituted shape mismatch: expected an expansion component");
    return substEltType->increasePackElementLevel(outerLevel)
                       ->castTo<PackExpansionType>()->getPatternType()
                       ->increasePackElementLevel(innerLevel);
  } else {
    assert(!substEltType->is<PackExpansionType>() &&
           "substituted shape mismatch: expected a scalar component");
    return substEltType->increasePackElementLevel(outerLevel);
  }
}

ProtocolConformanceRef
InFlightSubstitution::lookupConformance(CanType dependentType,
                                        Type conformingReplacementType,
                                        ProtocolDecl *conformedProtocol,
                                        unsigned level) {
  auto substConfRef = BaselineLookupConformance(dependentType,
                                                conformingReplacementType,
                                                conformedProtocol);
  if (!substConfRef ||
      ActivePackExpansions.empty() ||
      !substConfRef.isPack())
    return substConfRef;

  auto substPackConf = substConfRef.getPack();
  auto substPackPatterns = substPackConf->getPatternConformances();
  assert(level < ActivePackExpansions.size() && "too deep");
  auto index = ActivePackExpansions[ActivePackExpansions.size() - level - 1]
      .expansionIndex;
  assert(index < substPackPatterns.size() &&
         "replacement for pack parameter did not have the right "
         "size for expansion");
  return substPackPatterns[index];
}

namespace {

class TypeSubstituter : public TypeTransform<TypeSubstituter> {
  unsigned level;
  InFlightSubstitution &IFS;

public:
  TypeSubstituter(unsigned level, InFlightSubstitution &IFS)
    : level(level), IFS(IFS) {}

  std::optional<Type> transform(TypeBase *type, TypePosition pos);

  Type transformGenericTypeParamType(GenericTypeParamType *param,
                                     TypePosition pos);

  Type transformPackExpansionType(PackExpansionType *expand,
                                  TypePosition pos);

  Type transformPackElementType(PackElementType *element,
                                TypePosition pos);

  Type transformDependentMemberType(DependentMemberType *dependent,
                                    TypePosition pos);

  Type transformPrimaryArchetypeType(ArchetypeType *primary,
                                     TypePosition pos);

  std::optional<Type> transformOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *opaque,
                                                       TypePosition pos);

  std::optional<Type> transformLocalArchetypeType(LocalArchetypeType *local,
                                                  TypePosition pos);

  SubstitutionMap transformSubstitutionMap(SubstitutionMap subs);

  CanType transformSILField(CanType fieldTy, TypePosition pos);
};

}

std::optional<Type>
TypeSubstituter::transform(TypeBase *type, TypePosition position) {
  if (IFS.isInvariant(type))
    return Type(type);

  return std::nullopt;
}

Type TypeSubstituter::transformPrimaryArchetypeType(ArchetypeType *primary,
                                                    TypePosition position) {
  // If we're not in one of the special modes, map the primary archetype out
  // of context, and substitute that instead.
  if (!IFS.shouldSubstitutePrimaryArchetypes() &&
      !IFS.shouldSubstituteOpaqueArchetypes() &&
      !IFS.shouldSubstituteLocalArchetypes()) {
    return doIt(primary->getInterfaceType(), position);
  }

  // Primary types can't normally be directly substituted unless we
  // specifically were asked to substitute them.
  if (!IFS.shouldSubstitutePrimaryArchetypes())
    return primary;

  auto known = IFS.substType(primary, level);
  ASSERT(known && "Opaque type replacement shouldn't fail");

  return known;
}

std::optional<Type>
TypeSubstituter::transformOpaqueTypeArchetypeType(OpaqueTypeArchetypeType *opaque,
                                                  TypePosition position) {
  // Opaque types can't normally be directly substituted unless we
  // specifically were asked to substitute them.
  if (!IFS.shouldSubstituteOpaqueArchetypes())
    return std::nullopt;

  auto known = IFS.substType(opaque, level);
  ASSERT(known && "Opaque type replacement shouldn't fail");

  // If we return an opaque archetype unchanged, recurse into its substitutions
  // as a special case.
  if (known->getCanonicalType() == opaque->getCanonicalType())
    return std::nullopt; // Recursively process the substitutions of the
                         // opaque type archetype.
  return known;
}

std::optional<Type>
TypeSubstituter::transformLocalArchetypeType(LocalArchetypeType *local,
                                             TypePosition position) {
  // Local types can't normally be directly substituted unless we
  // specifically were asked to substitute them.
  if (!IFS.shouldSubstituteLocalArchetypes())
    return std::nullopt;

  auto known = IFS.substType(local, level);

  // FIXME: Change remaining callers to always substitute all local
  // archetypes.
  if (!known)
    return Type(local);

  return known;
}

Type TypeSubstituter::transformGenericTypeParamType(GenericTypeParamType *param,
                                                    TypePosition pos) {
  // If we have a substitution for this type, use it.
  if (auto known = IFS.substType(param, level))
    return known;

  // If we failed to substitute a generic type parameter, give up.
  return ErrorType::get(param);
}

Type TypeSubstituter::transformPackExpansionType(PackExpansionType *expand,
                                                 TypePosition pos) {
  auto eltTys = IFS.expandPackExpansionType(expand);
  if (eltTys.size() == 1)
    return eltTys[0];
  return Type(PackType::get(expand->getASTContext(), eltTys));
}

Type TypeSubstituter::transformPackElementType(PackElementType *element,
                                               TypePosition pos) {
  SWIFT_DEFER { level -= element->getLevel(); };
  level += element->getLevel();
  return doIt(element->getPackType(), pos);
}

Type TypeSubstituter::transformDependentMemberType(DependentMemberType *dependent,
                                                   TypePosition pos) {
  auto newBase = doIt(dependent->getBase(), TypePosition::Invariant);
  return getMemberForBaseType(IFS,
                              dependent->getBase(), newBase,
                              dependent->getAssocType(),
                              dependent->getName(),
                              level);
}

SubstitutionMap TypeSubstituter::transformSubstitutionMap(SubstitutionMap subs) {
  // FIXME: Take level into account? Move level down into IFS?
  return subs.subst(IFS);
}

CanType TypeSubstituter::transformSILField(CanType fieldTy, TypePosition pos) {
  // Type substitution does not walk into the SILBoxType's field types, because
  // that's written with respect to the generic signature of the box type,
  // and not the input generic signature of the substitution.
  return fieldTy;
}

Type Type::subst(SubstitutionMap substitutions,
                 SubstOptions options) const {
  InFlightSubstitutionViaSubMap IFS(substitutions, options);
  return subst(IFS);
}

Type Type::subst(TypeSubstitutionFn substitutions,
                 LookupConformanceFn conformances,
                 SubstOptions options) const {
  InFlightSubstitution IFS(substitutions, conformances, options);
  return subst(IFS);
}

Type Type::subst(InFlightSubstitution &IFS) const {
  // Handle substitutions into generic function types.
  // FIXME: This should be banned.
  if (auto genericFnType = getPointer()->getAs<GenericFunctionType>()) {
    return substGenericFunctionType(genericFnType, IFS);
  }

  if (IFS.isInvariant(*this))
    return *this;

  TypeSubstituter transform(/*level=*/0, IFS);
  return transform.doIt(*this, TypePosition::Invariant);
}

//===----------------------------------------------------------------------===//
// getContextSubstitutionMap() and friends
//===----------------------------------------------------------------------===//

static Type getConcreteTypeForSuperclassTraversing(Type t) {
  if (t->isExistentialType()) {
    return t->getExistentialLayout().getSuperclass();
  } if (auto archetype = t->getAs<ArchetypeType>()) {
    return archetype->getSuperclass();
  } else if (auto dynamicSelfTy = t->getAs<DynamicSelfType>()) {
    return dynamicSelfTy->getSelfType();
  }
  return t;
}

Type TypeBase::getSuperclassForDecl(const ClassDecl *baseClass,
                                    bool useArchetypes) {
  Type t = getConcreteTypeForSuperclassTraversing(this);

  while (t) {
    // If we have a class-constrained archetype or class-constrained
    // existential, get the underlying superclass constraint.
    auto *nominalDecl = t->getAnyNominal();
    assert(nominalDecl && "expected nominal type here");
    assert(isa<ClassDecl>(nominalDecl) && "expected a class here");

    if (nominalDecl == baseClass)
      return t;

    t = t->getSuperclass(useArchetypes);
  }

#ifndef NDEBUG
  auto *currentClass = getConcreteTypeForSuperclassTraversing(this)
      ->getClassOrBoundGenericClass();
  assert(baseClass->isSuperclassOf(currentClass) &&
         "no inheritance relationship between given classes");
#endif

  return ErrorType::get(this);
}

SubstitutionMap TypeBase::getContextSubstitutionMap() {
  // Fast path.
  auto *nominalTy = castTo<NominalOrBoundGenericNominalType>();
  if (nominalTy->ContextSubMap)
    return nominalTy->ContextSubMap;

  auto nominal = nominalTy->getDecl();
  auto genericSig = nominal->getGenericSignature();
  if (!genericSig)
    return SubstitutionMap();

  Type baseTy(this);

  assert(!baseTy->hasLValueType() &&
         !baseTy->is<AnyMetatypeType>() &&
         !baseTy->is<ErrorType>());

  // The resulting set of substitutions. Always use this to ensure we
  // don't miss out on NRVO anywhere.
  SmallVector<Type, 4> replacementTypes;

  // Gather all of the substitutions for all levels of generic arguments.
  auto params = genericSig.getGenericParams();
  bool first = true;

  while (baseTy) {
    // For a bound generic type, gather the generic parameter -> generic
    // argument substitutions.
    if (auto boundGeneric = baseTy->getAs<BoundGenericType>()) {
      auto args = boundGeneric->getGenericArgs();
      for (auto arg : llvm::reverse(args)) {
        replacementTypes.push_back(arg);
      }

      // Continue looking into the parent.
      baseTy = boundGeneric->getParent();
      first = false;
      continue;
    }

    // For an unbound generic type, fill in error types.
    if (auto unboundGeneric = baseTy->getAs<UnboundGenericType>()) {
      auto &ctx = getASTContext();
      auto decl = unboundGeneric->getDecl();
      for (auto *paramDecl : decl->getGenericParams()->getParams()) {
        replacementTypes.push_back(ErrorType::get(ctx));
        (void) paramDecl;
      }
      baseTy = unboundGeneric->getParent();
      first = false;
      continue;
    }

    // This case indicates we have invalid nesting of types.
    if (auto protocolTy = baseTy->getAs<ProtocolType>()) {
      if (!first)
        break;

      replacementTypes.push_back(getASTContext().TheErrorType);
      break;
    }

    // Continue looking into the parent.
    if (auto nominalTy = baseTy->getAs<NominalType>()) {
      baseTy = nominalTy->getParent();
      first = false;
      continue;
    }

    abort();
  }

  ASSERT(replacementTypes.size() <= params.size());

  // Add any outer generic parameters from the local context.
  while (replacementTypes.size() < params.size()) {
    replacementTypes.push_back(getASTContext().TheErrorType);
  }

  std::reverse(replacementTypes.begin(), replacementTypes.end());

  auto subMap = SubstitutionMap::get(
    genericSig, replacementTypes,
    LookUpConformanceInModule());

  nominalTy->ContextSubMap = subMap;
  return subMap;
}

TypeSubstitutionMap
TypeBase::getContextSubstitutions(const DeclContext *dc,
                                  GenericEnvironment *genericEnv) {
  assert(dc->isTypeContext());
  Type baseTy(this);

  assert(!baseTy->hasLValueType() &&
         !baseTy->is<AnyMetatypeType>() &&
         !baseTy->is<ErrorType>());

  // The resulting set of substitutions. Always use this to ensure we
  // don't miss out on NRVO anywhere.
  TypeSubstitutionMap substitutions;

  // If the member is part of a protocol or extension thereof, we need
  // to substitute in the type of Self.
  if (dc->getSelfProtocolDecl()) {
    // FIXME: This feels painfully inefficient. We're creating a dense map
    // for a single substitution.
    substitutions[dc->getSelfInterfaceType()
                    ->getCanonicalType()->castTo<GenericTypeParamType>()]
      = baseTy;
    return substitutions;
  }

  const auto genericSig = dc->getGenericSignatureOfContext();
  if (!genericSig)
    return substitutions;

  auto *ownerNominal = dc->getSelfNominalTypeDecl();

  // If the declaration context is Builtin.TheTupleType or an extension thereof,
  // the base type must be a tuple type. Build a pack type from the tuple's
  // elements and construct a substitution map replacing the generic parameter
  // of Builtin.TheTupleType with the pack.
  if (isa<BuiltinTupleDecl>(ownerNominal)) {
    SmallVector<Type, 2> packElts;
    for (auto type : castTo<TupleType>()->getElementTypes())
      packElts.push_back(type);

    auto *packType = PackType::get(dc->getASTContext(), packElts);

    assert(genericSig.getGenericParams().size() == 1);
    auto elementsParam = cast<SubstitutableType>(
        genericSig.getGenericParams()[0]->getCanonicalType());
    substitutions[elementsParam] = packType;
    return substitutions;
  }

  // If the declaration context is a class or an extension thereof, the base
  // type must be a class, class-constrained archetype, or self-conforming
  // existential with a superclass bound. Get the base type's superclass type
  // for the corresponding declaration context.
  if (auto *ownerClass = dyn_cast<ClassDecl>(ownerNominal)) {
    baseTy = baseTy->getSuperclassForDecl(ownerClass,
                                      /*usesArchetypes=*/genericEnv != nullptr);
  }

  // Gather all of the substitutions for all levels of generic arguments.
  auto params = genericSig.getGenericParams();
  unsigned n = params.size();

  while (baseTy && n > 0) {
    if (baseTy->is<ErrorType>())
      break;

    // For a bound generic type, gather the generic parameter -> generic
    // argument substitutions.
    if (auto boundGeneric = baseTy->getAs<BoundGenericType>()) {
      auto args = boundGeneric->getGenericArgs();
      for (unsigned i = 0, e = args.size(); i < e; ++i) {
        substitutions[params[n - e + i]->getCanonicalType()
                        ->castTo<GenericTypeParamType>()] = args[i];
      }

      // Continue looking into the parent.
      baseTy = boundGeneric->getParent();
      n -= args.size();
      continue;
    }

    // Continue looking into the parent.
    if (auto protocolTy = baseTy->getAs<ProtocolType>()) {
      baseTy = protocolTy->getParent();
      --n;
      continue;
    }

    // Continue looking into the parent.
    if (auto nominalTy = baseTy->getAs<NominalType>()) {
      baseTy = nominalTy->getParent();
      continue;
    }

    // There are no substitutions to apply if the type is still unbound,
    // continue looking into the parent.
    if (auto unboundGeneric = baseTy->getAs<UnboundGenericType>()) {
      baseTy = unboundGeneric->getParent();
      continue;
    }

    // Assert and break to avoid hanging if we get an unexpected baseTy.
    assert(0 && "Bad base type");
    break;
  }

  // Add any outer generic parameters from the local context.
  while (n > 0) {
    auto *gp = params[--n];
    Type substTy = gp;
    if (baseTy && baseTy->is<ErrorType>())
      substTy = ErrorType::get(baseTy->getASTContext());
    else if (genericEnv)
      substTy = genericEnv->mapTypeIntoContext(gp);

    if (gp->isParameterPack() && !substTy->hasError())
      substTy = PackType::getSingletonPackExpansion(substTy);

    auto result = substitutions.insert(
      {gp->getCanonicalType()->castTo<GenericTypeParamType>(),
       substTy});
    assert(result.second);
    (void) result;
  }

  return substitutions;
}

SubstitutionMap TypeBase::getContextSubstitutionMap(
    const DeclContext *dc,
    GenericEnvironment *genericEnv) {
  if (dc == getAnyNominal() && genericEnv == nullptr)
    return getContextSubstitutionMap();

  auto genericSig = dc->getGenericSignatureOfContext();
  if (genericSig.isNull())
    return SubstitutionMap();
  return SubstitutionMap::get(
    genericSig,
    QueryTypeSubstitutionMap{getContextSubstitutions(dc, genericEnv)},
    LookUpConformanceInModule());
}

TypeSubstitutionMap TypeBase::getMemberSubstitutions(
    const ValueDecl *member,
    GenericEnvironment *genericEnv) {
  auto *memberDC = member->getDeclContext();

  TypeSubstitutionMap substitutions;

  // Compute the set of member substitutions to apply.
  if (memberDC->isTypeContext())
    substitutions = getContextSubstitutions(memberDC, genericEnv);

  // If the member itself is generic, preserve its generic parameters.
  // We need this since code completion and diagnostics want to be able
  // to call getTypeOfMember() with functions and nested types.
  if (isa<AbstractFunctionDecl>(member) ||
      isa<GenericTypeDecl>(member) ||
      isa<SubscriptDecl>(member)) {
    auto *innerDC = member->getInnermostDeclContext();
    if (innerDC->isInnermostContextGeneric()) {
      if (auto sig = innerDC->getGenericSignatureOfContext()) {
        for (auto param : sig.getInnermostGenericParams()) {
          Type substGenericParam = param;
          if (param->isParameterPack()) {
            substGenericParam = PackType::getSingletonPackExpansion(
                param);
          }
          if (genericEnv) {
            substGenericParam = genericEnv->mapTypeIntoContext(
                substGenericParam);
          }

          auto *key = param->getCanonicalType()->castTo<GenericTypeParamType>();
          substitutions[key] = substGenericParam;
        }
      }
    }
  }

  return substitutions;
}

SubstitutionMap TypeBase::getMemberSubstitutionMap(
    const ValueDecl *member, GenericEnvironment *genericEnv) {
  auto genericSig = member->getInnermostDeclContext()
      ->getGenericSignatureOfContext();
  if (genericSig.isNull())
    return SubstitutionMap();
  auto subs = getMemberSubstitutions(member, genericEnv);
  return SubstitutionMap::get(
      genericSig,
      QueryTypeSubstitutionMap{subs},
      LookUpConformanceInModule());
}

Type TypeBase::getTypeOfMember(const VarDecl *member) {
  return getTypeOfMember(member, member->getInterfaceType());
}

Type TypeBase::getTypeOfMember(const ValueDecl *member,
                               Type memberType) {
  assert(memberType);
  assert(!memberType->is<GenericFunctionType>() &&
         "Generic function types are not supported");

  if (is<ErrorType>())
    return ErrorType::get(getASTContext());

  if (auto *lvalue = getAs<LValueType>()) {
    auto objectTy = lvalue->getObjectType();
    return objectTy->getTypeOfMember(member, memberType);
  }

  // Perform the substitution.
  auto substitutions = getMemberSubstitutionMap(member);
  return memberType.subst(substitutions);
}

Type TypeBase::adjustSuperclassMemberDeclType(const ValueDecl *baseDecl,
                                              const ValueDecl *derivedDecl,
                                              Type memberType) {
  auto subs = SubstitutionMap::getOverrideSubstitutions(
      baseDecl, derivedDecl);

  if (auto *genericMemberType = memberType->getAs<GenericFunctionType>()) {
    memberType = FunctionType::get(genericMemberType->getParams(),
                                   genericMemberType->getResult(),
                                   genericMemberType->getExtInfo());
  }

  auto type = memberType.subst(subs);
  if (baseDecl->getDeclContext()->getSelfProtocolDecl())
    return type;

  if (auto *afd = dyn_cast<AbstractFunctionDecl>(baseDecl)) {
    type = type->replaceSelfParameterType(this);
    if (afd->hasDynamicSelfResult())
      type = type->replaceCovariantResultType(this, /*uncurryLevel=*/2);
  } else if (auto *sd = dyn_cast<SubscriptDecl>(baseDecl)) {
    if (sd->getElementInterfaceType()->hasDynamicSelfType())
      type = type->replaceCovariantResultType(this, /*uncurryLevel=*/1);
  } else if (auto *vd = dyn_cast<VarDecl>(baseDecl)) {
    if (vd->getValueInterfaceType()->hasDynamicSelfType())
      type = type->replaceCovariantResultType(this, /*uncurryLevel=*/0);
  }

  return type;
}

//===----------------------------------------------------------------------===//
// Replacing opaque result archetypes with their underlying types
//===----------------------------------------------------------------------===//

OpaqueSubstitutionKind
ReplaceOpaqueTypesWithUnderlyingTypes::shouldPerformSubstitution(
    OpaqueTypeDecl *opaque) const {
  const auto *inContext = getContext();
  auto inModule = inContext ? inContext->getParentModule()
                            : opaque->getParentModule();
  return shouldPerformSubstitution(opaque, inModule, contextExpansion);
}
OpaqueSubstitutionKind
ReplaceOpaqueTypesWithUnderlyingTypes::shouldPerformSubstitution(
    OpaqueTypeDecl *opaque, ModuleDecl *contextModule,
    ResilienceExpansion contextExpansion) {
  auto namingDecl = opaque->getNamingDecl();
  
  // Don't allow replacement if the naming decl is dynamically replaceable.
  if (namingDecl && namingDecl->isDynamic())
    return OpaqueSubstitutionKind::DontSubstitute;

  // Allow replacement of opaque result types of inlineable function regardless
  // of resilience and in which context.
  if (auto *afd = dyn_cast<AbstractFunctionDecl>(namingDecl)) {
    if (afd->getResilienceExpansion() == ResilienceExpansion::Minimal) {
      return OpaqueSubstitutionKind::AlwaysSubstitute;
    }
  } else if (auto *asd = dyn_cast<AbstractStorageDecl>(namingDecl)) {
    auto *getter = asd->getOpaqueAccessor(AccessorKind::Get);
    if (getter &&
        getter->getResilienceExpansion() == ResilienceExpansion::Minimal) {
      return OpaqueSubstitutionKind::AlwaysSubstitute;
    }
  }

  // Allow replacement of opaque result types in the context of maximal
  // resilient expansion if the context's and the opaque type's module are the
  // same.
  auto module = namingDecl->getModuleContext();
  if (contextExpansion == ResilienceExpansion::Maximal &&
      module == contextModule)
    return OpaqueSubstitutionKind::SubstituteSameModuleMaximalResilience;

  // Allow replacement of opaque result types in the context of maximal
  // resilient expansion if the context's and the opaque type's module are in
  // the same package.
  if (contextExpansion == ResilienceExpansion::Maximal &&
      module->isResilient() && module->serializePackageEnabled() &&
      module->inSamePackage(contextModule))
    return OpaqueSubstitutionKind::SubstituteSamePackageMaximalResilience;

  // Allow general replacement from non resilient modules. Otherwise, disallow.
  if (module->isResilient())
    return OpaqueSubstitutionKind::DontSubstitute;

  return OpaqueSubstitutionKind::SubstituteNonResilientModule;
}

static Type substOpaqueTypesWithUnderlyingTypesRec(
    Type ty, const DeclContext *inContext, ResilienceExpansion contextExpansion,
    bool isWholeModuleContext,
    llvm::DenseSet<ReplaceOpaqueTypesWithUnderlyingTypes::SeenDecl> &decls) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(inContext, contextExpansion,
                                                 isWholeModuleContext, decls);
  return ty.subst(replacer, replacer,
                  SubstFlags::SubstituteOpaqueArchetypes |
                  SubstFlags::PreservePackExpansionLevel);
}

/// Checks that \p dc has access to \p ty for the purposes of an opaque
/// substitution described by \p kind.
///
/// This is purely an implementation detail check about whether type metadata
/// will be accessible. It's not intended to enforce any rules about what
/// opaque substitutions are or are not allowed.
static bool canSubstituteTypeInto(Type ty, const DeclContext *dc,
                                  OpaqueSubstitutionKind kind,
                                  ResilienceExpansion contextExpansion,
                                  bool isContextWholeModule) {
  TypeDecl *typeDecl = ty->getAnyNominal();
  if (!typeDecl) {
    // If the referenced type is a different opaque result type,
    // check that its descriptor is accessible.
    if (auto opaqueTy = ty->getAs<OpaqueTypeArchetypeType>())
      typeDecl = opaqueTy->getDecl();
  }
  if (!typeDecl) {
    return true;
  }

  switch (kind) {
  case OpaqueSubstitutionKind::DontSubstitute:
    return false;

  case OpaqueSubstitutionKind::AlwaysSubstitute:
    return true;

  case OpaqueSubstitutionKind::SubstituteSameModuleMaximalResilience:
    // In whole module compilation private types are okay.
    if (isContextWholeModule)
      return true;

    // In the same file any visibility is okay.
    if (!dc->isModuleContext() &&
        typeDecl->getDeclContext()->getOutermostParentSourceFile() ==
        dc->getOutermostParentSourceFile())
      return true;

    return typeDecl->getEffectiveAccess() > AccessLevel::FilePrivate;

  case OpaqueSubstitutionKind::SubstituteSamePackageMaximalResilience: {
    return typeDecl->getEffectiveAccess() >= AccessLevel::Package;
  }

  case OpaqueSubstitutionKind::SubstituteNonResilientModule:
    // Can't access types that are not public from a different module.
    if (dc->getParentModule() == typeDecl->getDeclContext()->getParentModule() &&
        contextExpansion != ResilienceExpansion::Minimal)
      return typeDecl->getEffectiveAccess() > AccessLevel::FilePrivate;

    return typeDecl->getEffectiveAccess() > AccessLevel::Internal;
  }
  llvm_unreachable("invalid substitution kind");
}

ReplaceOpaqueTypesWithUnderlyingTypes::ReplaceOpaqueTypesWithUnderlyingTypes(
    const DeclContext *inContext, ResilienceExpansion contextExpansion,
    bool isWholeModuleContext, llvm::DenseSet<SeenDecl> &seen)
    : contextExpansion(contextExpansion),
      inContextAndIsWholeModule(inContext, isWholeModuleContext),
      seenDecls(&seen) {}

Type ReplaceOpaqueTypesWithUnderlyingTypes::
operator()(SubstitutableType *maybeOpaqueType) const {
  auto *archetype = dyn_cast<OpaqueTypeArchetypeType>(maybeOpaqueType);
  if (!archetype)
    return maybeOpaqueType;

  auto *genericEnv = archetype->getGenericEnvironment();
  auto *decl = genericEnv->getOpaqueTypeDecl();
  auto outerSubs = genericEnv->getOuterSubstitutions();

  auto substitutionKind = shouldPerformSubstitution(decl);
  if (substitutionKind == OpaqueSubstitutionKind::DontSubstitute) {
    return maybeOpaqueType;
  }

  auto subs = decl->getUniqueUnderlyingTypeSubstitutions();
  // If the body of the opaque decl providing decl has not been type checked we
  // don't have a underlying substitution.
  if (!subs.has_value())
    return maybeOpaqueType;

  // Apply the underlying type substitutions to the interface type of the
  // archetype in question. This will map the inner generic signature of the
  // opaque type to its outer signature.
  auto partialSubstTy = archetype->getInterfaceType().subst(*subs);

  // Check that we are allowed to substitute the underlying type into the
  // context.
  auto inContext = this->getContext();
  auto isContextWholeModule = this->isWholeModule();
  auto contextExpansion = this->contextExpansion;
  if (inContext &&
      partialSubstTy.findIf(
          [inContext, substitutionKind, isContextWholeModule,
           contextExpansion](Type t) -> bool {
            if (!canSubstituteTypeInto(t, inContext, substitutionKind,
                                       contextExpansion,
                                       isContextWholeModule))
              return true;
            return false;
          }))
    return maybeOpaqueType;

  // Then apply the substitutions from the root opaque archetype, to specialize
  // for its type arguments. We perform this substitution after checking for
  // visibility, since we do not want the result of the visibility check to
  // depend on the substitutions previously applied.
  auto substTy = partialSubstTy.subst(outerSubs);

  // If the type changed, but still contains opaque types, recur.
  if (!substTy->isEqual(maybeOpaqueType) && substTy->hasOpaqueArchetype()) {
    SeenDecl seenKey(decl, outerSubs);
    if (auto *alreadySeen = this->seenDecls) {
      // Detect substitution loops. If we find one, just bounce the original
      // type back to the caller. This substitution will fail at runtime
      // instead.
      if (!alreadySeen->insert(seenKey).second) {
        return maybeOpaqueType;
      }

      auto res = ::substOpaqueTypesWithUnderlyingTypesRec(
          substTy, inContext, contextExpansion, isContextWholeModule,
          *alreadySeen);
      alreadySeen->erase(seenKey);
      return res;
    } else {
      // We're the top of the stack for the recursion check. Allocate a set of
      // opaque result type decls we've already seen for the rest of the check.
      llvm::DenseSet<SeenDecl> seenDecls;
      seenDecls.insert(seenKey);
      return ::substOpaqueTypesWithUnderlyingTypesRec(
          substTy, inContext, contextExpansion, isContextWholeModule,
          seenDecls);
    }
  }

  return substTy;
}

CanType swift::substOpaqueTypesWithUnderlyingTypes(CanType ty,
                                                   TypeExpansionContext context) {
  if (!context.shouldLookThroughOpaqueTypeArchetypes() ||
      !ty->hasOpaqueArchetype())
    return ty;

  ReplaceOpaqueTypesWithUnderlyingTypes replacer(
      context.getContext(), context.getResilienceExpansion(),
      context.isWholeModuleContext());
  SubstOptions flags = (SubstFlags::SubstituteOpaqueArchetypes |
                        SubstFlags::PreservePackExpansionLevel);
  return ty.subst(replacer, replacer, flags)->getCanonicalType();
}

static ProtocolConformanceRef substOpaqueTypesWithUnderlyingTypesRec(
    ProtocolConformanceRef ref, Type origType, const DeclContext *inContext,
    ResilienceExpansion contextExpansion, bool isWholeModuleContext,
    llvm::DenseSet<ReplaceOpaqueTypesWithUnderlyingTypes::SeenDecl> &decls) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(inContext, contextExpansion,
                                                 isWholeModuleContext, decls);
  return ref.subst(origType, replacer, replacer,
                   SubstFlags::SubstituteOpaqueArchetypes |
                   SubstFlags::PreservePackExpansionLevel);
}

ProtocolConformanceRef swift::substOpaqueTypesWithUnderlyingTypes(
    ProtocolConformanceRef ref, Type origType, TypeExpansionContext context) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(
      context.getContext(), context.getResilienceExpansion(),
      context.isWholeModuleContext());
  return ref.subst(origType, replacer, replacer,
                   SubstFlags::SubstituteOpaqueArchetypes);
}

ProtocolConformanceRef ReplaceOpaqueTypesWithUnderlyingTypes::
operator()(CanType maybeOpaqueType, Type replacementType,
           ProtocolDecl *protocol) const {
  auto abstractRef = ProtocolConformanceRef(protocol);
  
  auto archetype = dyn_cast<OpaqueTypeArchetypeType>(maybeOpaqueType);
  if (!archetype) {
    if (maybeOpaqueType->isTypeParameter() ||
        maybeOpaqueType->is<ArchetypeType>())
      return abstractRef;
    
    // SIL type lowering may have already substituted away the opaque type, in
    // which case we'll end up "substituting" the same type.
    if (maybeOpaqueType->isEqual(replacementType)) {
      return lookupConformance(replacementType, protocol);
    }
    
    llvm_unreachable("origType should have been an opaque type or type parameter");
  }

  auto *genericEnv = archetype->getGenericEnvironment();
  auto *decl = genericEnv->getOpaqueTypeDecl();
  auto outerSubs = genericEnv->getOuterSubstitutions();

  auto substitutionKind = shouldPerformSubstitution(decl);
  if (substitutionKind == OpaqueSubstitutionKind::DontSubstitute) {
    return abstractRef;
  }

  auto subs = decl->getUniqueUnderlyingTypeSubstitutions();
  // If the body of the opaque decl providing decl has not been type checked we
  // don't have a underlying substitution.
  if (!subs.has_value())
    return abstractRef;

  // Apply the underlying type substitutions to the interface type of the
  // archetype in question. This will map the inner generic signature of the
  // opaque type to its outer signature.
  auto partialSubstTy = archetype->getInterfaceType().subst(*subs);

  // Check that we are allowed to substitute the underlying type into the
  // context.
  auto inContext = this->getContext();
  auto isContextWholeModule = this->isWholeModule();
  auto contextExpansion = this->contextExpansion;
  if (partialSubstTy.findIf(
          [inContext, substitutionKind, isContextWholeModule,
          contextExpansion](Type t) -> bool {
            if (!canSubstituteTypeInto(t, inContext, substitutionKind,
                                       contextExpansion,
                                       isContextWholeModule))
              return true;
            return false;
          }))
    return abstractRef;

  // Then apply the substitutions from the root opaque archetype, to specialize
  // for its type arguments. We perform this substitution after checking for
  // visibility, since we do not want the result of the visibility check to
  // depend on the substitutions previously applied.
  auto substTy = partialSubstTy.subst(outerSubs);

  auto partialSubstRef =
      abstractRef.subst(archetype->getInterfaceType(), *subs);
  auto substRef =
      partialSubstRef.subst(partialSubstTy, outerSubs);

  // If the type still contains opaque types, recur.
  if (substTy->hasOpaqueArchetype()) {
    SeenDecl seenKey(decl, outerSubs);
    
    if (auto *alreadySeen = this->seenDecls) {
      // Detect substitution loops. If we find one, just bounce the original
      // type back to the caller. This substitution will fail at runtime
      // instead.
      if (!alreadySeen->insert(seenKey).second) {
        return abstractRef;
      }

      auto res = ::substOpaqueTypesWithUnderlyingTypesRec(
          substRef, substTy, inContext, contextExpansion, isContextWholeModule,
          *alreadySeen);
      alreadySeen->erase(seenKey);
      return res;
    } else {
      // We're the top of the stack for the recursion check. Allocate a set of
      // opaque result type decls we've already seen for the rest of the check.
      llvm::DenseSet<SeenDecl> seenDecls;
      seenDecls.insert(seenKey);
      return ::substOpaqueTypesWithUnderlyingTypesRec(
          substRef, substTy, inContext, contextExpansion, isContextWholeModule,
          seenDecls);
    }
  }
  return substRef;
}
