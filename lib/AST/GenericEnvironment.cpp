//===--- GenericEnvironment.cpp - GenericEnvironment AST ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the GenericEnvironment class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Defer.h"

using namespace swift;

size_t GenericEnvironment::numTrailingObjects(OverloadToken<Type>) const {
  return Signature.getGenericParams().size();
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
MutableArrayRef<Type> GenericEnvironment::getContextTypes() {
  return MutableArrayRef<Type>(getTrailingObjects<Type>(),
                               Signature.getGenericParams().size());
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
ArrayRef<Type> GenericEnvironment::getContextTypes() const {
  return ArrayRef<Type>(getTrailingObjects<Type>(),
                        Signature.getGenericParams().size());
}

TypeArrayView<GenericTypeParamType>
GenericEnvironment::getGenericParams() const {
  return Signature.getGenericParams();
}

GenericEnvironment::GenericEnvironment(GenericSignature signature)
  : Signature(signature)
{
  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

void GenericEnvironment::addMapping(GenericParamKey key,
                                    Type contextType) {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = Signature.getGenericParams();
  unsigned index = key.findIndexIn(genericParams);
  assert(genericParams[index] == key && "Bad generic parameter");

  // Add the mapping from the generic parameter to the context type.
  assert(getContextTypes()[index].isNull() ||
         getContextTypes()[index]->is<ErrorType>() &&
         "Already recoded this mapping");
  getContextTypes()[index] = contextType;
}

Optional<Type> GenericEnvironment::getMappingIfPresent(
                                                    GenericParamKey key) const {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = Signature.getGenericParams();
  unsigned index = key.findIndexIn(genericParams);
  assert(genericParams[index] == key && "Bad generic parameter");

  if (auto type = getContextTypes()[index])
    return type;

  return None;
}

Type GenericEnvironment::mapTypeIntoContext(GenericEnvironment *env,
                                            Type type) {
  assert(!type->hasArchetype() && "already have a contextual type");

  if (!env)
    return type.substDependentTypesWithErrorTypes();

  return env->mapTypeIntoContext(type);
}

Type MapTypeOutOfContext::operator()(SubstitutableType *type) const {
  auto archetype = cast<ArchetypeType>(type);
  if (isa<OpaqueTypeArchetypeType>(archetype->getRoot()))
    return Type();
  
  return archetype->getInterfaceType();
}

Type TypeBase::mapTypeOutOfContext() {
  assert(!hasTypeParameter() && "already have an interface type");
  return Type(this).subst(MapTypeOutOfContext(),
    MakeAbstractConformanceForGenericType(),
    SubstFlags::AllowLoweredTypes);
}

Type
GenericEnvironment::getOrCreateArchetypeFromInterfaceType(Type depType) {
  auto genericSig = getGenericSignature();
  LookUpConformanceInSignature conformanceLookupFn(genericSig.getPointer());

  auto requirements = genericSig->getLocalRequirements(depType);

  // FIXME: With the RequirementMachine, we will always have an anchor.
  if (requirements.concreteType && !requirements.anchor) {
    if (requirements.concreteType->is<ErrorType>())
      return requirements.concreteType;

    return mapTypeIntoContext(requirements.concreteType,
                              conformanceLookupFn);
  }

  assert(requirements.anchor && "No anchor or concrete type?");

  auto &ctx = genericSig->getASTContext();

  // First, write an ErrorType to the location where this type is cached,
  // to catch re-entrant lookups that might arise from an invalid generic
  // signature (eg, <X where X == Array<X>>).
  ArchetypeType *parentArchetype = nullptr;
  GenericTypeParamType *genericParam = nullptr;
  if (auto depMemTy = requirements.anchor->getAs<DependentMemberType>()) {
    parentArchetype =
      getOrCreateArchetypeFromInterfaceType(depMemTy->getBase())
        ->getAs<ArchetypeType>();
    if (!parentArchetype)
      return ErrorType::get(depMemTy);

    auto name = depMemTy->getName();
    if (auto type = parentArchetype->getNestedTypeIfKnown(name))
      return *type;

    parentArchetype->registerNestedType(name, ErrorType::get(ctx));
  } else {
    genericParam = requirements.anchor->castTo<GenericTypeParamType>();
    if (auto type = getMappingIfPresent(genericParam))
      return *type;
    addMapping(genericParam, ErrorType::get(ctx));
  }

  Type result;

  // If this equivalence class is mapped to a concrete type, produce that
  // type.
  if (requirements.concreteType) {
    result = mapTypeIntoContext(requirements.concreteType,
                                conformanceLookupFn);
  } else {
    // Substitute into the superclass.
    Type superclass = requirements.superclass;
    if (superclass && superclass->hasTypeParameter()) {
      superclass = mapTypeIntoContext(superclass,
                                      conformanceLookupFn);
      if (superclass->is<ErrorType>())
        superclass = Type();
    }

    if (parentArchetype) {
      auto *depMemTy = requirements.anchor->castTo<DependentMemberType>();
      result = NestedArchetypeType::getNew(ctx, parentArchetype, depMemTy,
                                           requirements.protos, superclass,
                                           requirements.layout);
    } else {
      result = PrimaryArchetypeType::getNew(ctx, this, genericParam,
                                            requirements.protos, superclass,
                                            requirements.layout);
    }
  }

  // Cache the new archetype for future lookups.
  if (auto depMemTy = requirements.anchor->getAs<DependentMemberType>()) {
    parentArchetype->registerNestedType(depMemTy->getName(), result);
  } else {
    addMapping(genericParam, result);
  }

  return result;
}

void ArchetypeType::resolveNestedType(
                                    std::pair<Identifier, Type> &nested) const {
  Type interfaceType = getInterfaceType();
  Type memberInterfaceType =
      DependentMemberType::get(interfaceType, nested.first);

  Type result = getGenericEnvironment()->getOrCreateArchetypeFromInterfaceType(
      memberInterfaceType);

  assert(!nested.second ||
         nested.second->isEqual(result) ||
         nested.second->is<ErrorType>());
  nested.second = result;
}

Type QueryInterfaceTypeSubstitutions::operator()(SubstitutableType *type) const{
  if (auto gp = type->getAs<GenericTypeParamType>()) {
    // Find the index into the parallel arrays of generic parameters and
    // context types.
    auto genericParams = self->Signature.getGenericParams();
    GenericParamKey key(gp);

    // Make sure that this generic parameter is from this environment.
    unsigned index = key.findIndexIn(genericParams);
    if (index == genericParams.size() || genericParams[index] != key)
      return Type();

    // If the context type isn't already known, lazily create it.
    auto mutableSelf = const_cast<GenericEnvironment *>(self);
    Type &contextType = mutableSelf->getContextTypes()[index];
    if (contextType)
      return contextType;

    auto result = mutableSelf->getOrCreateArchetypeFromInterfaceType(type);

    assert (!contextType ||
            contextType->isEqual(result) ||
            contextType->is<ErrorType>());
    contextType = result;
    return result;
  }

  return Type();
}

Type GenericEnvironment::mapTypeIntoContext(
                                Type type,
                                LookupConformanceFn lookupConformance) const {
  assert(!type->hasOpenedExistential() &&
         "Opened existentials are special and so are you");

  Type result = type.subst(QueryInterfaceTypeSubstitutions(this),
                           lookupConformance,
                           SubstFlags::AllowLoweredTypes);
  assert((!result->hasTypeParameter() || result->hasError()) &&
         "not fully substituted");
  return result;

}

Type GenericEnvironment::mapTypeIntoContext(Type type) const {
  auto sig = getGenericSignature();
  return mapTypeIntoContext(type, LookUpConformanceInSignature(sig.getPointer()));
}

Type GenericEnvironment::mapTypeIntoContext(GenericTypeParamType *type) const {
  auto self = const_cast<GenericEnvironment *>(this);
  Type result = QueryInterfaceTypeSubstitutions(self)(type);
  if (!result)
    return ErrorType::get(type);
  return result;
}

SubstitutionMap GenericEnvironment::getForwardingSubstitutionMap() const {
  auto genericSig = getGenericSignature();
  return SubstitutionMap::get(genericSig,
                              QueryInterfaceTypeSubstitutions(this),
                              MakeAbstractConformanceForGenericType());
}

std::pair<Type, ProtocolConformanceRef>
GenericEnvironment::mapConformanceRefIntoContext(GenericEnvironment *genericEnv,
                                           Type conformingType,
                                           ProtocolConformanceRef conformance) {
  if (!genericEnv)
    return {conformingType, conformance};
  
  return genericEnv->mapConformanceRefIntoContext(conformingType, conformance);
}

std::pair<Type, ProtocolConformanceRef>
GenericEnvironment::mapConformanceRefIntoContext(
                                     Type conformingInterfaceType,
                                     ProtocolConformanceRef conformance) const {
  auto contextConformance = conformance.subst(conformingInterfaceType,
    QueryInterfaceTypeSubstitutions(this),
    LookUpConformanceInSignature(getGenericSignature().getPointer()));
  
  auto contextType = mapTypeIntoContext(conformingInterfaceType);
  return {contextType, contextConformance};
}
