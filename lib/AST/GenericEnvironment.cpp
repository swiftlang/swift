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
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"

using namespace swift;

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<SubstitutionMap>) const {
  switch (getKind()) {
  case Kind::Primary:
    return 0;

  case Kind::OpenedExistential:
  case Kind::OpenedElement:
  case Kind::Opaque:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<OpaqueEnvironmentData>) const {
  switch (getKind()) {
  case Kind::Primary:
  case Kind::OpenedExistential:
  case Kind::OpenedElement:
    return 0;

  case Kind::Opaque:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<OpenedExistentialEnvironmentData>) const {
  switch (getKind()) {
  case Kind::Primary:
  case Kind::Opaque:
  case Kind::OpenedElement:
    return 0;

  case Kind::OpenedExistential:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(
    OverloadToken<OpenedElementEnvironmentData>) const {
  switch (getKind()) {
  case Kind::Primary:
  case Kind::Opaque:
  case Kind::OpenedExistential:
    return 0;

  case Kind::OpenedElement:
    return 1;
  }
}

size_t GenericEnvironment::numTrailingObjects(OverloadToken<Type>) const {
  return getGenericParams().size()
       + (getKind() == Kind::OpenedElement ? getNumOpenedPackParams() : 0);
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

unsigned GenericEnvironment::getNumOpenedPackParams() const {
  assert(getKind() == Kind::OpenedElement);
  return getGenericSignature().getInnermostGenericParams().size();
}

MutableArrayRef<Type> GenericEnvironment::getOpenedPackParams() {
  auto begin = getTrailingObjects<Type>() + getGenericParams().size();
  return MutableArrayRef<Type>(begin, getNumOpenedPackParams());
}

ArrayRef<Type> GenericEnvironment::getOpenedPackParams() const {
  auto begin = getTrailingObjects<Type>() + getGenericParams().size();
  return ArrayRef<Type>(begin, getNumOpenedPackParams());
}

ArrayRef<GenericTypeParamType *>
GenericEnvironment::getGenericParams() const {
  return getGenericSignature().getGenericParams();
}

SubstitutionMap GenericEnvironment::getOuterSubstitutions() const {
  assert(getKind() != Kind::Primary);
  return *getTrailingObjects<SubstitutionMap>();
}

OpaqueTypeDecl *GenericEnvironment::getOpaqueTypeDecl() const {
  assert(getKind() == Kind::Opaque);
  return getTrailingObjects<OpaqueEnvironmentData>()->decl;
}

CanGenericTypeParamType
GenericEnvironment::getOpenedElementShapeClass() const {
  assert(getKind() == Kind::OpenedElement);
  auto environmentData = getTrailingObjects<OpenedElementEnvironmentData>();
  return environmentData->shapeClass;
}

Type GenericEnvironment::getOpenedExistentialType() const {
  assert(getKind() == Kind::OpenedExistential);
  return getTrailingObjects<OpenedExistentialEnvironmentData>()->existential;
}

UUID GenericEnvironment::getOpenedExistentialUUID() const {
  assert(getKind() == Kind::OpenedExistential);
  return getTrailingObjects<OpenedExistentialEnvironmentData>()->uuid;
}

UUID GenericEnvironment::getOpenedElementUUID() const {
  assert(getKind() == Kind::OpenedElement);
  return getTrailingObjects<OpenedElementEnvironmentData>()->uuid;
}

void GenericEnvironment::forEachPackElementArchetype(
          llvm::function_ref<void(ElementArchetypeType *)> function) const {
  auto packElements = getGenericSignature().getInnermostGenericParams();
  for (auto eltInterfaceType: packElements) {
    auto *elementArchetype =
      mapTypeIntoContext(eltInterfaceType)->castTo<ElementArchetypeType>();
    function(elementArchetype);
  }
}

void GenericEnvironment::forEachPackElementGenericTypeParam(
    llvm::function_ref<void(GenericTypeParamType *)> function) const {
  auto sig = getGenericSignature();
  auto shapeClass = getOpenedElementShapeClass();
  auto packElements = sig.getInnermostGenericParams();
  auto packElementDepth = packElements.front()->getDepth();

  // Each parameter pack in the outer generic parameters has
  // a corresponding pack element parameter at the innermost
  // depth.
  for (auto *genericParam : getGenericParams()) {
    if (genericParam->getDepth() == packElementDepth)
      break;

    if (!genericParam->isParameterPack())
      continue;

    // Only include opened element parameters for packs in the given
    // shape equivalence class.
    if (!sig->haveSameShape(genericParam, shapeClass))
      continue;

    function(genericParam);
  }
}

void GenericEnvironment::forEachPackElementBinding(
    PackElementBindingCallback function) const {
  auto sig = getGenericSignature();
  auto packElements = sig.getInnermostGenericParams();
  auto elementIt = packElements.begin();
  forEachPackElementGenericTypeParam([&](auto *genericParam) {
    assert(elementIt != packElements.end());
    auto *elementArchetype =
        mapTypeIntoContext(*elementIt++)->castTo<ElementArchetypeType>();
    auto *packSubstitution = maybeApplyOuterContextSubstitutions(genericParam)
      ->getPackSubstitutionAsPackType();
    function(elementArchetype, packSubstitution);
  });

  assert(elementIt == packElements.end());
}

GenericEnvironment::GenericEnvironment(GenericSignature signature)
  : SignatureAndKind(signature, Kind::Primary)
{
  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericEnvironment::GenericEnvironment(
    GenericSignature signature,
    Type existential, SubstitutionMap subs, UUID uuid)
  : SignatureAndKind(signature, Kind::OpenedExistential)
{
  *getTrailingObjects<SubstitutionMap>() = subs;
  new (getTrailingObjects<OpenedExistentialEnvironmentData>())
    OpenedExistentialEnvironmentData{ existential, uuid };

  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericEnvironment::GenericEnvironment(
      GenericSignature signature, OpaqueTypeDecl *opaque, SubstitutionMap subs)
  : SignatureAndKind(signature, Kind::Opaque)
{
  *getTrailingObjects<SubstitutionMap>() = subs;
  new (getTrailingObjects<OpaqueEnvironmentData>())
    OpaqueEnvironmentData{opaque};

  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());
}

GenericEnvironment::GenericEnvironment(GenericSignature signature,
                                       UUID uuid,
                                       CanGenericTypeParamType shapeClass,
                                       SubstitutionMap outerSubs)
  : SignatureAndKind(signature, Kind::OpenedElement)
{
  *getTrailingObjects<SubstitutionMap>() = outerSubs;
  new (getTrailingObjects<OpenedElementEnvironmentData>())
    OpenedElementEnvironmentData{uuid, shapeClass};

  // Clear out the memory that holds the context types.
  std::uninitialized_fill(getContextTypes().begin(), getContextTypes().end(),
                          Type());

  // Fill in the array of opened pack parameters.
  auto openedPacksBuffer = getOpenedPackParams();
  unsigned i = 0;
  for (auto param : signature.getGenericParams()) {
    if (!param->isParameterPack()) continue;
    if (!signature->haveSameShape(param, shapeClass)) continue;
    openedPacksBuffer[i++] = param;
  }
  assert(i == openedPacksBuffer.size());
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

std::optional<Type>
GenericEnvironment::getMappingIfPresent(GenericParamKey key) const {
  // Find the index into the parallel arrays of generic parameters and
  // context types.
  auto genericParams = getGenericParams();
  unsigned index = key.findIndexIn(genericParams);
  assert(genericParams[index] == key && "Bad generic parameter");

  if (auto type = getContextTypes()[index])
    return type;

  return std::nullopt;
}

Type
GenericEnvironment::maybeApplyOuterContextSubstitutions(Type type) const {
  switch (getKind()) {
  case Kind::Primary:
    return type;

  case Kind::OpenedExistential:
  case Kind::OpenedElement:
  case Kind::Opaque: {
    OuterSubstitutions replacer{
        getOuterSubstitutions(), getGenericSignature()->getMaxDepth()};
    return type.subst(replacer, replacer);
  }
  }
}

Type GenericEnvironment::mapTypeIntoContext(GenericEnvironment *env,
                                            Type type) {
  assert(!type->hasPrimaryArchetype() && "already have a contextual type");

  if (!env) {
    assert(!type->hasTypeParameter() &&
           "no generic environment provided for type with type parameters");
    return type;
  }

  return env->mapTypeIntoContext(type);
}

Type MapTypeOutOfContext::operator()(SubstitutableType *type) const {
  if (isa<PrimaryArchetypeType>(type) ||
      isa<PackArchetypeType>(type)) {
    return cast<ArchetypeType>(type)->getInterfaceType();
  }

  return type;
}

Type TypeBase::mapTypeOutOfContext() {
  assert(!hasTypeParameter() && "already have an interface type");
  return Type(this).subst(MapTypeOutOfContext(),
    MakeAbstractConformanceForGenericType(),
    SubstFlags::PreservePackExpansionLevel |
    SubstFlags::SubstitutePrimaryArchetypes);
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

Type
GenericEnvironment::getOrCreateArchetypeFromInterfaceType(Type depType) {
  auto genericSig = getGenericSignature();
  auto requirements = genericSig->getLocalRequirements(depType);

  if (requirements.concreteType)
    return mapTypeIntoContext(requirements.concreteType);

  assert(requirements.anchor && "No anchor or concrete type?");

  auto &ctx = genericSig->getASTContext();

  // First, write an ErrorType to the location where this type is cached,
  // to catch re-entrant lookups that might arise from an invalid generic
  // signature (eg, <X where X == Array<X>>).
  CanDependentMemberType nestedType;
  GenericTypeParamType *genericParam = nullptr;
  if (auto depMemTy = requirements.anchor->getAs<DependentMemberType>()) {
    nestedType = cast<DependentMemberType>(depMemTy->getCanonicalType());
    auto &entry = getOrCreateNestedTypeStorage()[nestedType];
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
    superclass = mapTypeIntoContext(superclass);
    if (superclass->is<ErrorType>())
      superclass = Type();
  }

  Type result;

  auto rootGP = requirements.anchor->getRootGenericParam();
  switch (getKind()) {
  case Kind::Primary:
    if (rootGP->isParameterPack()) {
      result = PackArchetypeType::get(ctx, this, requirements.anchor,
                                      requirements.packShape,
                                      requirements.protos, superclass,
                                      requirements.layout);
    } else {
      result = PrimaryArchetypeType::getNew(ctx, this, requirements.anchor,
                                            requirements.protos, superclass,
                                            requirements.layout);
    }

    break;

  case Kind::Opaque: {
    // If the anchor type isn't rooted in a generic parameter that
    // represents an opaque declaration, then apply the outer substitutions.
    // It would be incorrect to build an opaque type archetype here.
    if (rootGP->getDepth() < genericSig->getMaxDepth()) {
      result = maybeApplyOuterContextSubstitutions(requirements.anchor);
      break;
    }

    result = OpaqueTypeArchetypeType::getNew(this, requirements.anchor,
                                             requirements.protos, superclass,
                                             requirements.layout);
    break;
  }

  case Kind::OpenedExistential: {
    if (rootGP->getDepth() < genericSig->getMaxDepth()) {
      result = maybeApplyOuterContextSubstitutions(requirements.anchor);
      break;
    }

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

  case Kind::OpenedElement: {
    if (rootGP->getDepth() < genericSig->getMaxDepth()) {
      result = maybeApplyOuterContextSubstitutions(requirements.anchor);
      break;
    }

    result = ElementArchetypeType::getNew(this, requirements.anchor,
                                          requirements.protos, superclass,
                                          requirements.layout);
    break;
  }
  }

  if (genericParam)
    addMapping(genericParam, result);
  else
    getOrCreateNestedTypeStorage()[nestedType] = result;

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
  assert(!type->hasPrimaryArchetype() && "already have a contextual type");

  Type result = type.subst(QueryInterfaceTypeSubstitutions(this),
                           lookupConformance,
                           SubstFlags::PreservePackExpansionLevel);
  ASSERT(getKind() != Kind::Primary || !result->hasTypeParameter());
  return result;

}

Type GenericEnvironment::mapTypeIntoContext(Type type) const {
  return mapTypeIntoContext(type, LookUpConformanceInModule());
}

Type GenericEnvironment::mapTypeIntoContext(GenericTypeParamType *type) const {
  auto self = const_cast<GenericEnvironment *>(this);
  Type result = QueryInterfaceTypeSubstitutions(self)(type);
  if (!result)
    return ErrorType::get(type);
  return result;
}

namespace {

struct FindElementArchetypeForOpenedPackParam {
  ArrayRef<Type> openedPacks;
  ArrayRef<GenericTypeParamType *> packElementParams;
  const GenericEnvironment *env;

  FindElementArchetypeForOpenedPackParam(const GenericEnvironment *env,
                                         ArrayRef<Type> openedPacks)
    : openedPacks(openedPacks),
      packElementParams(env->getGenericSignature().getInnermostGenericParams()),
      env(env) {}

  Type getInterfaceType(Type interfaceType) const {
    if (auto member = interfaceType->getAs<DependentMemberType>()) {
      return DependentMemberType::get(getInterfaceType(member->getBase()),
                                      member->getAssocType());
    }

    assert(interfaceType->is<GenericTypeParamType>());
    for (auto i : indices(openedPacks)) {
      if (openedPacks[i]->isEqual(interfaceType))
        return packElementParams[i];
    }

    llvm_unreachable("parameter was not an opened pack parameter");
  }

  Type operator()(Type interfaceType) const {
    return env->mapTypeIntoContext(getInterfaceType(interfaceType));
  }
};

}

/// So this expects a type written with the archetypes of the original generic
/// environment, not 'this', the opened element environment, because it is the
/// original PackArchetypes that become ElementArchetypes. Also this function
/// does not apply outer substitutions, which might not be what you expect.
Type
GenericEnvironment::mapContextualPackTypeIntoElementContext(Type type) const {
  assert(getKind() == Kind::OpenedElement);
  assert(!type->hasTypeParameter() && "expected contextual type");

  if (!type->hasPackArchetype()) return type;

  auto sig = getGenericSignature();
  auto shapeClass = getOpenedElementShapeClass();

  FindElementArchetypeForOpenedPackParam
    findElementArchetype(this, getOpenedPackParams());

  return type.transformTypeParameterPacks(
      [&](SubstitutableType *ty) -> std::optional<Type> {
        if (auto *packArchetype = dyn_cast<PackArchetypeType>(ty)) {
          auto interfaceType = packArchetype->getInterfaceType();
          if (sig->haveSameShape(interfaceType, shapeClass))
            return Type(findElementArchetype(interfaceType));
        }

        return std::nullopt;
      });
}

CanType
GenericEnvironment::mapContextualPackTypeIntoElementContext(CanType type) const {
  return CanType(mapContextualPackTypeIntoElementContext(Type(type)));
}

/// Unlike mapContextualPackTypeIntoElementContext(), this also applies outer
/// substitutions, so it behaves like mapTypeIntoContext() in that respect.
Type
GenericEnvironment::mapPackTypeIntoElementContext(Type type) const {
  assert(getKind() == Kind::OpenedElement);
  assert(!type->hasPackArchetype());

  if (!type->hasParameterPack()) return type;

  // Get a contextual type in the original generic environment, not the
  // substituted one, which is what mapContextualPackTypeIntoElementContext()
  // expects.
  auto contextualType = getOuterSubstitutions()
    .getGenericSignature().getGenericEnvironment()->mapTypeIntoContext(type);

  contextualType = mapContextualPackTypeIntoElementContext(contextualType);
  return maybeApplyOuterContextSubstitutions(contextualType);
}

Type
GenericEnvironment::mapElementTypeIntoPackContext(Type type) const {
  assert(getKind() == Kind::Primary);

  // We need to pass in an archetype to get the shape class from its
  // generic environment.
  assert(type->hasElementArchetype());

  GenericEnvironment *elementEnv = nullptr;

  // Map element archetypes to interface types in the element generic
  // environment's signature.
  type = type.subst(
    [&](SubstitutableType *type) -> Type {
      auto *archetype = cast<ArchetypeType>(type);

      if (isa<OpenedArchetypeType>(archetype))
        return archetype;

      if (isa<ElementArchetypeType>(archetype)) {
        assert(!elementEnv ||
               elementEnv == archetype->getGenericEnvironment());
        elementEnv = archetype->getGenericEnvironment();
      }

      return archetype->getInterfaceType();
    },
    MakeAbstractConformanceForGenericType(),
    SubstFlags::PreservePackExpansionLevel |
    SubstFlags::SubstitutePrimaryArchetypes |
    SubstFlags::SubstituteLocalArchetypes);

  auto shapeClass = elementEnv->getOpenedElementShapeClass();

  llvm::SmallVector<GenericTypeParamType *, 2> members;
  auto elementDepth = elementEnv->getGenericSignature()->getMaxDepth();

  auto sig = getGenericSignature();
  for (auto *genericParam : sig.getGenericParams()) {
    if (!genericParam->isParameterPack())
      continue;

    if (!sig->haveSameShape(genericParam, shapeClass))
      continue;

    members.push_back(genericParam);
  }

  // Map element interface types to pack archetypes.
  QueryInterfaceTypeSubstitutions mapIntoContext(this);
  return type.subst(
      [&](SubstitutableType *type) {
        auto *genericParam = cast<GenericTypeParamType>(type);
        if (genericParam->getDepth() == elementDepth) {
          genericParam = members[genericParam->getIndex()];
          assert(genericParam->isParameterPack());
        }
        return mapIntoContext(genericParam);
      },
      LookUpConformanceInModule(),
      SubstFlags::PreservePackExpansionLevel);
}

namespace {
/// A function suitable for use as a \c TypeSubstitutionFn that produces
/// correct forwarding substitutions for a generic environment.
///
/// This differs from QueryInterfaceTypeSubstitutions only in that it
/// always produces PackTypes for pack parameters.
class BuildForwardingSubstitutions {
  QueryInterfaceTypeSubstitutions Query;

public:
  BuildForwardingSubstitutions(const GenericEnvironment *self)
    : Query(self) { }

  Type operator()(SubstitutableType *type) const;
};
} // end anonymous namespace

Type BuildForwardingSubstitutions::operator()(SubstitutableType *type) const {
  if (auto resultType = Query(type)) {
    auto param = cast<GenericTypeParamType>(type);
    if (!param->isParameterPack())
      return resultType;
    if (resultType->is<PackType>())
      return resultType;
    return PackType::getSingletonPackExpansion(resultType);
  }
  return Type();
}

SubstitutionMap GenericEnvironment::getForwardingSubstitutionMap() const {
  auto genericSig = getGenericSignature();
  return SubstitutionMap::get(genericSig,
                              BuildForwardingSubstitutions(this),
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
    LookUpConformanceInModule());
  
  auto contextType = mapTypeIntoContext(conformingInterfaceType);
  return {contextType, contextConformance};
}

OpenedElementContext
OpenedElementContext::createForContextualExpansion(ASTContext &ctx,
                                       CanPackExpansionType expansionType) {
  assert(!expansionType->hasTypeParameter() &&
         "must be given a contextual type");

  // Get the outer generic signature and environment.
  auto countArchetype = cast<ArchetypeType>(expansionType.getCountType());
  auto *genericEnv = countArchetype->getGenericEnvironment();
  auto subMap = genericEnv->getForwardingSubstitutionMap();

  auto countType = cast<GenericTypeParamType>(
      countArchetype->getInterfaceType()->getCanonicalType());

  auto genericSig = genericEnv->getGenericSignature().getCanonicalSignature();
  // Create an opened element signature and environment.
  auto elementSig = ctx.getOpenedElementSignature(
      genericSig, countType);
  auto *elementEnv = GenericEnvironment::forOpenedElement(
      elementSig, UUID::fromTime(), countType, subMap);

  return {elementEnv, elementSig};
}
