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

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<OpaqueTypeDecl *>) const {
  switch (getKind()) {
  case Kind::Normal:
  case Kind::OpenedExistential:
    return 0;

  case Kind::Opaque:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<SubstitutionMap>) const {
  switch (getKind()) {
  case Kind::Normal:
  case Kind::OpenedExistential:
    return 0;

  case Kind::Opaque:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(OverloadToken<Type>) const {
  return getGenericParams().size();
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
MutableArrayRef<Type> GenericEnvironment::getContextTypes() {
  return MutableArrayRef<Type>(getTrailingObjects<Type>(),
                               getGenericParams().size());
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
ArrayRef<Type> GenericEnvironment::getContextTypes() const {
  return ArrayRef<Type>(getTrailingObjects<Type>(),
                        getGenericParams().size());
}

TypeArrayView<GenericTypeParamType>
GenericEnvironment::getGenericParams() const {
  return getGenericSignature().getGenericParams();
}

OpaqueTypeDecl *GenericEnvironment::getOpaqueTypeDecl() const {
  assert(getKind() == Kind::Opaque);
  return *getTrailingObjects<OpaqueTypeDecl *>();
}

SubstitutionMap GenericEnvironment::getOpaqueSubstitutions() const {
  assert(getKind() == Kind::Opaque);
  return *getTrailingObjects<SubstitutionMap>();
}

GenericEnvironment::GenericEnvironment(GenericSignature signature, Kind kind)
  : SignatureAndKind(signature, kind)
{
  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericEnvironment::GenericEnvironment(
      GenericSignature signature, OpaqueTypeDecl *opaque, SubstitutionMap subs)
  : SignatureAndKind(signature, Kind::Opaque)
{
  *getTrailingObjects<OpaqueTypeDecl *>() = opaque;
  new (getTrailingObjects<SubstitutionMap>()) SubstitutionMap(subs);

  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

void GenericEnvironment::addMapping(GenericParamKey key,
                                    Type contextType) {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = getGenericParams();
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
  auto genericParams = getGenericParams();
  unsigned index = key.findIndexIn(genericParams);
  assert(genericParams[index] == key && "Bad generic parameter");

  if (auto type = getContextTypes()[index])
    return type;

  return None;
}

namespace {

/// Substitute the outer generic parameters from a substitution map, ignoring
/// innter generic parameters with a given depth.
struct SubstituteOuterFromSubstitutionMap {
  SubstitutionMap subs;
  unsigned depth;

  /// Whether this is a type parameter that should not be substituted.
  bool isUnsubstitutedTypeParameter(Type type) const {
    if (!type->isTypeParameter())
      return false;

    if (auto depMemTy = type->getAs<DependentMemberType>())
      return isUnsubstitutedTypeParameter(depMemTy->getBase());

    if (auto genericParam = type->getAs<GenericTypeParamType>())
      return genericParam->getDepth() >= depth;

    return false;
  }

  Type operator()(SubstitutableType *type) const {
    if (isUnsubstitutedTypeParameter(type))
      return Type(type);

    return QuerySubstitutionMap{subs}(type);
  }

  ProtocolConformanceRef operator()(CanType dependentType,
                                    Type conformingReplacementType,
                                    ProtocolDecl *conformedProtocol) const {
    if (isUnsubstitutedTypeParameter(dependentType))
      return ProtocolConformanceRef(conformedProtocol);

    return LookUpConformanceInSubstitutionMap(subs)(
        dependentType, conformingReplacementType, conformedProtocol);
  }
};

}

Type GenericEnvironment::maybeApplyOpaqueTypeSubstitutions(Type type) const {
  switch (getKind()) {
  case Kind::Normal:
  case Kind::OpenedExistential:
    return type;

  case Kind::Opaque: {
    // Substitute outer generic parameters of an opaque archetype environment.
    unsigned opaqueDepth =
      getOpaqueTypeDecl()->getOpaqueGenericParams().front()->getDepth();
    SubstituteOuterFromSubstitutionMap replacer{
        getOpaqueSubstitutions(), opaqueDepth};
    return type.subst(replacer, replacer);
  }
  }
}

Type GenericEnvironment::mapTypeIntoContext(GenericEnvironment *env,
                                            Type type) {
  assert((!type->hasArchetype() || type->hasOpenedExistential()) &&
         "already have a contextual type");
  assert((env || !type->hasTypeParameter()) &&
         "no generic environment provided for type with type parameters");

  if (!env) {
    return type;
  }

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

  /// Substitute a type for the purpose of requirements.
  auto substForRequirements = [&](Type type) {
    switch (getKind()) {
    case Kind::Normal:
    case Kind::OpenedExistential:
      return mapTypeIntoContext(type, conformanceLookupFn);

    case Kind::Opaque:
      return maybeApplyOpaqueTypeSubstitutions(type);
    }
  };

  if (requirements.concreteType) {
    return substForRequirements(requirements.concreteType);
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
        ->castTo<ArchetypeType>();

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

  // Substitute into the superclass.
  Type superclass = requirements.superclass;
  if (superclass && superclass->hasTypeParameter()) {
    superclass = substForRequirements(superclass);
    if (superclass->is<ErrorType>())
      superclass = Type();
  }

  Type result;

  if (parentArchetype) {
    auto *depMemTy = requirements.anchor->castTo<DependentMemberType>();
    result = NestedArchetypeType::getNew(ctx, parentArchetype, depMemTy,
                                         requirements.protos, superclass,
                                         requirements.layout);
    parentArchetype->registerNestedType(depMemTy->getName(), result);
  } else if (genericParam->isTypeSequence()) {
    result = SequenceArchetypeType::get(ctx, this, genericParam,
                                        requirements.protos, superclass,
                                        requirements.layout);
    addMapping(genericParam, result);
  } else {
    switch (getKind()) {
    case Kind::Normal:
    case Kind::OpenedExistential:
      result = PrimaryArchetypeType::getNew(ctx, this, genericParam,
                                            requirements.protos, superclass,
                                            requirements.layout);
      break;

    case Kind::Opaque:
      result = OpaqueTypeArchetypeType::getNew(this, genericParam,
                                               requirements.protos, superclass,
                                               requirements.layout);
      break;
    }

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
    auto genericParams = self->getGenericParams();
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
  assert((!type->hasArchetype() || type->hasOpenedExistential()) &&
         "already have a contextual type");

  type = maybeApplyOpaqueTypeSubstitutions(type);
  Type result = type.subst(QueryInterfaceTypeSubstitutions(this),
                           lookupConformance,
                           SubstFlags::AllowLoweredTypes);
  assert((!result->hasTypeParameter() || result->hasError() ||
          getKind() == Kind::Opaque) &&
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
