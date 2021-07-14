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
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/Basic/Defer.h"
#include "GenericSignatureBuilderImpl.h"

using namespace swift;

size_t GenericEnvironment::numTrailingObjects(OverloadToken<Type>) const {
  return Signature->getGenericParams().size();
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
MutableArrayRef<Type> GenericEnvironment::getContextTypes() {
  return MutableArrayRef<Type>(getTrailingObjects<Type>(),
                               Signature->getGenericParams().size());
}

/// Retrieve the array containing the context types associated with the
/// generic parameters, stored in parallel with the generic parameters of the
/// generic signature.
ArrayRef<Type> GenericEnvironment::getContextTypes() const {
  return ArrayRef<Type>(getTrailingObjects<Type>(),
                        Signature->getGenericParams().size());
}

TypeArrayView<GenericTypeParamType>
GenericEnvironment::getGenericParams() const {
  return Signature->getGenericParams();
}

GenericEnvironment::GenericEnvironment(GenericSignature signature)
  : Signature(signature)
{
  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericSignatureBuilder *GenericEnvironment::getGenericSignatureBuilder() const {
  if (Builder)
    return Builder;

  const_cast<GenericEnvironment *>(this)->Builder
      = Signature->getGenericSignatureBuilder();

  return Builder;
}

void GenericEnvironment::addMapping(GenericParamKey key,
                                    Type contextType) {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = Signature->getGenericParams();
  unsigned index = key.findIndexIn(genericParams);
  assert(genericParams[index] == key && "Bad generic parameter");

  // Add the mapping from the generic parameter to the context type.
  assert(getContextTypes()[index].isNull() && "Already recoded this mapping");
  getContextTypes()[index] = contextType;
}

Optional<Type> GenericEnvironment::getMappingIfPresent(
                                                    GenericParamKey key) const {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = Signature->getGenericParams();
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
GenericEnvironment::getOrCreateArchetypeFromInterfaceType(
    GenericSignatureBuilder::EquivalenceClass *equivClass) {
  auto genericParams = getGenericParams();

  auto &builder = *getGenericSignatureBuilder();
  Type anchor = equivClass->getAnchor(builder, genericParams);

  // If this equivalence class is mapped to a concrete type, produce that
  // type.
  if (equivClass->concreteType) {
    if (equivClass->recursiveConcreteType)
      return ErrorType::get(anchor);

    // Prevent recursive substitution.
    equivClass->recursiveConcreteType = true;
    SWIFT_DEFER {
      equivClass->recursiveConcreteType = false;
    };

    return mapTypeIntoContext(equivClass->concreteType,
                              builder.getLookupConformanceFn());
  }

  // Local function to check whether we have a generic parameter that has
  // already been recorded
  auto getAlreadyRecoveredGenericParam = [&]() -> Type {
    auto genericParam = anchor->getAs<GenericTypeParamType>();
    if (!genericParam) return Type();

    auto type = getMappingIfPresent(genericParam);
    if (!type) return Type();

    // We already have a mapping for this generic parameter in the generic
    // environment. Return it.
    return *type;
  };

  AssociatedTypeDecl *assocType = nullptr;
  ArchetypeType *parentArchetype = nullptr;
  if (auto depMemTy = anchor->getAs<DependentMemberType>()) {
    // Resolve the equivalence class of the parent.
    auto parentEquivClass =
      builder.resolveEquivalenceClass(
                          depMemTy->getBase(),
                          ArchetypeResolutionKind::CompleteWellFormed);
    if (!parentEquivClass)
      return ErrorType::get(anchor);

    // Map the parent type into this context.
    parentArchetype =
      getOrCreateArchetypeFromInterfaceType(parentEquivClass)
        ->castTo<ArchetypeType>();

    // If we already have a nested type with this name, return it.
    assocType = depMemTy->getAssocType();
    if (auto nested =
          parentArchetype->getNestedTypeIfKnown(assocType->getName())) {
      return *nested;
    }

    // We will build the archetype below.
  } else if (auto result = getAlreadyRecoveredGenericParam()) {
    // Return already-contextualized generic type parameter.
    return result;
  }

  // Substitute into the superclass.
  Type superclass = (equivClass->recursiveSuperclassType
                     ? Type() : equivClass->superclass);
  if (superclass && superclass->hasTypeParameter()) {
    // Prevent recursive substitution.
    equivClass->recursiveSuperclassType = true;
    SWIFT_DEFER {
      equivClass->recursiveSuperclassType = false;
    };

    superclass = mapTypeIntoContext(superclass,
                                    builder.getLookupConformanceFn());
    if (superclass->is<ErrorType>())
      superclass = Type();

    // We might have recursively recorded the archetype; if so, return early.
    // FIXME: This should be detectable before we end up building archetypes.
    if (auto result = getAlreadyRecoveredGenericParam())
      return result;
  }

  // Build a new archetype.

  // Collect the protocol conformances for the archetype.
  SmallVector<ProtocolDecl *, 4> protos;
  for (const auto &conforms : equivClass->conformsTo) {
    auto proto = conforms.first;

    if (!equivClass->isConformanceSatisfiedBySuperclass(proto))
      protos.push_back(proto);
  }

  ArchetypeType *archetype;
  ASTContext &ctx = builder.getASTContext();
  if (parentArchetype) {
    // Create a nested archetype.
    auto *depMemTy = anchor->castTo<DependentMemberType>();
    archetype = NestedArchetypeType::getNew(ctx, parentArchetype, depMemTy,
                                            protos, superclass,
                                            equivClass->layout);

    // Register this archetype with its parent.
    parentArchetype->registerNestedType(assocType->getName(), archetype);
  } else {
    // Create a top-level archetype.
    auto genericParam = anchor->castTo<GenericTypeParamType>();
    archetype = PrimaryArchetypeType::getNew(ctx, this, genericParam,
                                             protos, superclass,
                                             equivClass->layout);

    // Register the archetype with the generic environment.
    addMapping(genericParam, archetype);
  }

  return archetype;
}

void ArchetypeType::resolveNestedType(
                                    std::pair<Identifier, Type> &nested) const {
  auto genericEnv = getGenericEnvironment();
  auto &builder = *genericEnv->getGenericSignatureBuilder();

  Type interfaceType = getInterfaceType();
  Type memberInterfaceType =
    DependentMemberType::get(interfaceType, nested.first);
  auto resolved =
    builder.maybeResolveEquivalenceClass(
                                  memberInterfaceType,
                                  ArchetypeResolutionKind::CompleteWellFormed,
                                  /*wantExactPotentialArchetype=*/false);
  if (!resolved) {
    nested.second = ErrorType::get(interfaceType);
    return;
  }

  Type result;
  if (auto concrete = resolved.getAsConcreteType()) {
    result = concrete;
  } else {
    auto *equivClass = resolved.getEquivalenceClass(builder);
    result = genericEnv->getOrCreateArchetypeFromInterfaceType(equivClass);
  }

  assert(!nested.second ||
         nested.second->isEqual(result) ||
         (nested.second->hasError() && result->hasError()));
  nested.second = result;
}

Type QueryInterfaceTypeSubstitutions::operator()(SubstitutableType *type) const{
  if (auto gp = type->getAs<GenericTypeParamType>()) {
    // Find the index into the parallel arrays of generic parameters and
    // context types.
    auto genericParams = self->Signature->getGenericParams();
    GenericParamKey key(gp);

    // Make sure that this generic parameter is from this environment.
    unsigned index = key.findIndexIn(genericParams);
    if (index == genericParams.size() || genericParams[index] != key)
      return Type();

    // If the context type isn't already known, lazily create it.
    Type contextType = self->getContextTypes()[index];
    if (!contextType) {
      auto *builder = self->getGenericSignatureBuilder();
      auto equivClass =
        builder->resolveEquivalenceClass(
                                  type,
                                  ArchetypeResolutionKind::CompleteWellFormed);

      auto mutableSelf = const_cast<GenericEnvironment *>(self);
      contextType = mutableSelf->getOrCreateArchetypeFromInterfaceType(equivClass);

      // FIXME: Redundant mapping from key -> index.
      if (self->getContextTypes()[index].isNull())
        mutableSelf->addMapping(key, contextType);
    }

    return contextType;
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
