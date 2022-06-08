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
#include "swift/AST/ExistentialLayout.h"
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

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<OpenedGenericEnvironmentData>) const {
  switch (getKind()) {
  case Kind::Normal:
  case Kind::Opaque:
    return 0;

  case Kind::OpenedExistential:
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

Type GenericEnvironment::getOpenedExistentialType() const {
  assert(getKind() == Kind::OpenedExistential);
  return getTrailingObjects<OpenedGenericEnvironmentData>()->existential;
}

UUID GenericEnvironment::getOpenedExistentialUUID() const {
  assert(getKind() == Kind::OpenedExistential);
  return getTrailingObjects<OpenedGenericEnvironmentData>()->uuid;
}

GenericEnvironment::GenericEnvironment(GenericSignature signature)
  : SignatureAndKind(signature, Kind::Normal)
{
  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericEnvironment::GenericEnvironment(
    GenericSignature signature, Type existential, UUID uuid)
  : SignatureAndKind(signature, Kind::OpenedExistential)
{
  new (getTrailingObjects<OpenedGenericEnvironmentData>())
    OpenedGenericEnvironmentData{ existential, uuid };

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

  // Leave opened archetypes alone; they're handled contextually.
  if (isa<OpenedArchetypeType>(archetype))
    return Type(type);

  return archetype->getInterfaceType();
}

Type TypeBase::mapTypeOutOfContext() {
  assert(!hasTypeParameter() && "already have an interface type");
  return Type(this).subst(MapTypeOutOfContext(),
    MakeAbstractConformanceForGenericType(),
    SubstFlags::AllowLoweredTypes);
}

class GenericEnvironment::NestedTypeStorage
    : public llvm::DenseMap<CanType, Type> { };

auto GenericEnvironment::getOrCreateNestedTypeStorage() -> NestedTypeStorage & {
  if (nestedTypeStorage)
    return *nestedTypeStorage;

  nestedTypeStorage = new NestedTypeStorage();
  ASTContext &ctx = getGenericParams().front()->getASTContext();
  ctx.addCleanup([nestedTypeStorage=this->nestedTypeStorage]() {
    delete nestedTypeStorage;
  });

  return *nestedTypeStorage;
}

static Type stripBoundDependentMemberTypes(Type t) {
  if (auto *depMemTy = t->getAs<DependentMemberType>()) {
    return DependentMemberType::get(
      stripBoundDependentMemberTypes(depMemTy->getBase()),
      depMemTy->getName());
  }

  return t;
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
      if (type->hasTypeParameter()) {
        return mapTypeIntoContext(type, conformanceLookupFn);
      } else {
        return type;
      }
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
  CanDependentMemberType nestedDependentMemberType;
  GenericTypeParamType *genericParam = nullptr;
  if (auto depMemTy = requirements.anchor->getAs<DependentMemberType>()) {
    nestedDependentMemberType = cast<DependentMemberType>(
        stripBoundDependentMemberTypes(depMemTy)->getCanonicalType());
    auto &entry = getOrCreateNestedTypeStorage()[nestedDependentMemberType];
    if (entry)
      return entry;

    entry = ErrorType::get(ctx);
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

  if (requirements.anchor->getRootGenericParam()->isTypeSequence()) {
    result = SequenceArchetypeType::get(ctx, this, requirements.anchor,
                                        requirements.protos, superclass,
                                        requirements.layout);
  } else {
    switch (getKind()) {
    case Kind::Normal:
      result = PrimaryArchetypeType::getNew(ctx, this, requirements.anchor,
                                            requirements.protos, superclass,
                                            requirements.layout);
      break;

    case Kind::OpenedExistential: {
      // FIXME: The existential layout's protocols might differ from the
      // canonicalized set of protocols determined by the generic signature.
      // Before NestedArchetypeType was removed, we used the former when
      // building a root OpenedArchetypeType, and the latter when building
      // nested archetypes.
      // For compatibility, continue using the existential layout's version when
      // the interface type is a generic parameter. We should align these at
      // some point.
      if (depType->is<GenericTypeParamType>()) {
        auto layout = getOpenedExistentialType()->getExistentialLayout();
        SmallVector<ProtocolDecl *, 2> protos;
        for (auto proto : layout.getProtocols())
          protos.push_back(proto);

        result = OpenedArchetypeType::getNew(this, requirements.anchor, protos,
                                             superclass, requirements.layout);
      } else {
        result = OpenedArchetypeType::getNew(this, requirements.anchor,
                                             requirements.protos, superclass,
                                             requirements.layout);
      }

      break;
    }

    case Kind::Opaque: {
      // If the anchor type isn't rooted in a generic parameter that
      // represents an opaque declaration, then apply the outer substitutions.
      // It would be incorrect to build an opaque type archetype here.
      auto rootGP = requirements.anchor->getRootGenericParam();
      unsigned opaqueDepth =
          getOpaqueTypeDecl()->getOpaqueGenericParams().front()->getDepth();
      if (rootGP->getDepth() < opaqueDepth) {
        result = maybeApplyOpaqueTypeSubstitutions(requirements.anchor);
        break;
      }

      result = OpaqueTypeArchetypeType::getNew(this, requirements.anchor,
                                               requirements.protos, superclass,
                                               requirements.layout);
      break;
    }
    }
  }

  if (genericParam)
    addMapping(genericParam, result);
  else
    getOrCreateNestedTypeStorage()[nestedDependentMemberType] = result;

  return result;
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
