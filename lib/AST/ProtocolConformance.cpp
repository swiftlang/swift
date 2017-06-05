//===--- ProtocolConformance.cpp - AST Protocol Conformance ---------------===//
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
// This file implements the protocol conformance data structures.
//
//===----------------------------------------------------------------------===//

#include "ConformanceLookupTable.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Substitution.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/PrettyStackTrace.h"
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

Witness::Witness(ValueDecl *decl, SubstitutionList substitutions,
                 GenericEnvironment *syntheticEnv,
                 SubstitutionList reqToSynthesizedEnvSubs) {
  auto &ctx = decl->getASTContext();

  auto declRef = ConcreteDeclRef(ctx, decl, substitutions);
  auto storedMem = ctx.Allocate(sizeof(StoredWitness), alignof(StoredWitness));
  auto stored = new (storedMem)
      StoredWitness{declRef, syntheticEnv,
                    ctx.AllocateCopy(reqToSynthesizedEnvSubs)};
  ctx.addDestructorCleanup(*stored);

  storage = stored;
}

void Witness::dump() const { dump(llvm::errs()); }

void Witness::dump(llvm::raw_ostream &out) const {
  // FIXME: Implement!
}

ProtocolConformanceRef::ProtocolConformanceRef(ProtocolDecl *protocol,
                                               ProtocolConformance *conf) {
  assert(protocol != nullptr &&
         "cannot construct ProtocolConformanceRef with null protocol");
  if (conf) {
    assert(protocol == conf->getProtocol() && "protocol conformance mismatch");
    Union = conf;
  } else {
    Union = protocol;
  }
}

ProtocolDecl *ProtocolConformanceRef::getRequirement() const {
  if (isConcrete()) {
    return getConcrete()->getProtocol();
  } else {
    return getAbstract();
  }
}

ProtocolConformanceRef
ProtocolConformanceRef::getInherited(ProtocolDecl *parent) const {
  assert((getRequirement() == parent ||
          getRequirement()->inheritsFrom(parent)) &&
         "not a parent of this protocol");
  
  if (parent == getRequirement())
    return *this;
  
  // For an abstract requirement, simply produce a new abstract requirement
  // for the parent.
  if (isAbstract()) {
    return ProtocolConformanceRef(parent);
  }
  
  // Navigate concrete conformances.
  if (isConcrete()) {
    return ProtocolConformanceRef(
      getConcrete()->getInheritedConformance(parent));
  }
  
  llvm_unreachable("unhandled ProtocolConformanceRef");
}

ProtocolConformanceRef
ProtocolConformanceRef::subst(Type origType,
                              TypeSubstitutionFn subs,
                              LookupConformanceFn conformances) const {
  auto substType = origType.subst(subs, conformances,
                                  SubstFlags::UseErrorType);

  // If we have a concrete conformance, we need to substitute the
  // conformance to apply to the new type.
  if (isConcrete())
    return ProtocolConformanceRef(
      getConcrete()->subst(substType, subs, conformances));

  // Opened existentials trivially conform and do not need to go through
  // substitution map lookup.
  if (substType->isOpenedExistential())
    return *this;

  // If the substituted type is an existential, we have a self-conforming
  // existential being substituted in place of itself. There's no
  // conformance information in this case, so just return.
  if (substType->isObjCExistentialType())
    return *this;

  auto *proto = getRequirement();

  // Check the conformance map.
  if (auto result = conformances(origType->getCanonicalType(),
                                 substType,
                                 proto->getDeclaredType())) {
    return *result;
  }

  llvm_unreachable("Invalid conformance substitution");
}

Type
ProtocolConformanceRef::getTypeWitnessByName(Type type,
                                             ProtocolConformanceRef conformance,
                                             Identifier name,
                                             LazyResolver *resolver) {
  // For an archetype, retrieve the nested type with the appropriate
  // name. There are no conformance tables.
  if (auto archetype = type->getAs<ArchetypeType>()) {
    return archetype->getNestedType(name);
  }

  // Find the named requirement.
  AssociatedTypeDecl *assocType = nullptr;
  auto members = conformance.getRequirement()->lookupDirect(name);
  for (auto member : members) {
    assocType = dyn_cast<AssociatedTypeDecl>(member);
    if (assocType)
      break;
  }

  // FIXME: Shouldn't this be a hard error?
  if (!assocType)
    return nullptr;

  if (conformance.isAbstract())
    return DependentMemberType::get(type, assocType);

  auto concrete = conformance.getConcrete();
  if (!concrete->hasTypeWitness(assocType, resolver)) {
    return nullptr;
  }
  return concrete->getTypeWitness(assocType, resolver);
}

void *ProtocolConformance::operator new(size_t bytes, ASTContext &context,
                                        AllocationArena arena,
                                        unsigned alignment) {
  return context.Allocate(bytes, alignment, arena);

}

#define CONFORMANCE_SUBCLASS_DISPATCH(Method, Args)                          \
switch (getKind()) {                                                         \
  case ProtocolConformanceKind::Normal:                                      \
    static_assert(&ProtocolConformance::Method !=                            \
                    &NormalProtocolConformance::Method,                      \
                  "Must override NormalProtocolConformance::" #Method);      \
    return cast<NormalProtocolConformance>(this)->Method Args;               \
  case ProtocolConformanceKind::Specialized:                                 \
    static_assert(&ProtocolConformance::Method !=                            \
                    &SpecializedProtocolConformance::Method,                 \
                  "Must override SpecializedProtocolConformance::" #Method); \
    return cast<SpecializedProtocolConformance>(this)->Method Args;          \
  case ProtocolConformanceKind::Inherited:                                   \
    static_assert(&ProtocolConformance::Method !=                            \
                    &InheritedProtocolConformance::Method,                   \
                  "Must override InheritedProtocolConformance::" #Method);   \
    return cast<InheritedProtocolConformance>(this)->Method Args;            \
}                                                                            \
llvm_unreachable("bad ProtocolConformanceKind");

/// Get the protocol being conformed to.
ProtocolDecl *ProtocolConformance::getProtocol() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getProtocol, ())
}

DeclContext *ProtocolConformance::getDeclContext() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getDeclContext, ())
}

/// Retrieve the state of this conformance.
ProtocolConformanceState ProtocolConformance::getState() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getState, ())
}

bool
ProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                    LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(hasTypeWitness, (assocType, resolver));
}

std::pair<Type, TypeDecl *>
ProtocolConformance::getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                                           LazyResolver *resolver,
                                           SubstOptions options) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitnessAndDecl,
                                (assocType, resolver, options))
}

Type ProtocolConformance::getTypeWitness(AssociatedTypeDecl *assocType,
                                         LazyResolver *resolver,
                                         SubstOptions options) const {
  return getTypeWitnessAndDecl(assocType, resolver, options).first;
}

ValueDecl *ProtocolConformance::getWitnessDecl(ValueDecl *requirement,
                                               LazyResolver *resolver) const {
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    return cast<NormalProtocolConformance>(this)->getWitness(requirement,
                                                             resolver)
      .getDecl();

  case ProtocolConformanceKind::Inherited:
    return cast<InheritedProtocolConformance>(this)
      ->getInheritedConformance()->getWitnessDecl(requirement, resolver);

  case ProtocolConformanceKind::Specialized:
    return cast<SpecializedProtocolConformance>(this)
      ->getGenericConformance()->getWitnessDecl(requirement, resolver);
  }
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::
usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
}

bool ProtocolConformance::hasFixedLayout() const {
  // A conformance/witness table has fixed layout if type has a fixed layout in
  // all resilience domains, and the conformance is externally visible.
  if (auto nominal = getType()->getAnyNominal())
    if (nominal->hasFixedLayout() &&
        getProtocol()->getEffectiveAccess() >= Accessibility::Public &&
        nominal->getEffectiveAccess() >= Accessibility::Public)
      return true;

  return false;
}

GenericEnvironment *ProtocolConformance::getGenericEnvironment() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic parameters.
    return getDeclContext()->getGenericEnvironmentOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it cannot have any open
    // type variables.
    //
    // FIXME: We could return a meaningful GenericEnvironment here
    return nullptr;
  }

  llvm_unreachable("Unhandled ProtocolConformanceKind in switch.");
}

GenericSignature *ProtocolConformance::getGenericSignature() const {
  switch (getKind()) {
  case ProtocolConformanceKind::Inherited:
  case ProtocolConformanceKind::Normal:
    // If we have a normal or inherited protocol conformance, look for its
    // generic signature.
    return getDeclContext()->getGenericSignatureOfContext();

  case ProtocolConformanceKind::Specialized:
    // If we have a specialized protocol conformance, since we do not support
    // currently partial specialization, we know that it cannot have any open
    // type variables.
    return nullptr;
  }

  llvm_unreachable("Unhandled ProtocolConformanceKind in switch.");
}

bool ProtocolConformance::isBehaviorConformance() const {
  return getRootNormalConformance()->isBehaviorConformance();
}

AbstractStorageDecl *ProtocolConformance::getBehaviorDecl() const {
  return getRootNormalConformance()->getBehaviorDecl();
}

void NormalProtocolConformance::setSignatureConformances(
                               ArrayRef<ProtocolConformanceRef> conformances) {
  auto &ctx = getProtocol()->getASTContext();
  SignatureConformances = ctx.AllocateCopy(conformances);

#if !NDEBUG
  unsigned idx = 0;
  for (auto req : getProtocol()->getRequirementSignature()->getRequirements()) {
    if (req.getKind() == RequirementKind::Conformance) {
      assert(idx < conformances.size());
      assert(conformances[idx].getRequirement() ==
               req.getSecondType()->castTo<ProtocolType>()->getDecl());
      ++idx;
    }
  }
  assert(idx == conformances.size() && "Too many conformances");
#endif
}

void NormalProtocolConformance::resolveLazyInfo() const {
  assert(Resolver);
  assert(isComplete());

  auto *resolver = Resolver;
  auto *mutableThis = const_cast<NormalProtocolConformance *>(this);
  mutableThis->Resolver = nullptr;
  mutableThis->setState(ProtocolConformanceState::Incomplete);
  resolver->finishNormalConformance(mutableThis, ResolverContextData);
  mutableThis->setState(ProtocolConformanceState::Complete);
}

void NormalProtocolConformance::setLazyLoader(LazyMemberLoader *resolver,
                                              uint64_t contextData) {
  assert(!Resolver && "already has a resolver");
  Resolver = resolver;
  ResolverContextData = contextData;
}

namespace {
  class PrettyStackTraceRequirement : public llvm::PrettyStackTraceEntry {
    const char *Action;
    const ProtocolConformance *Conformance;
    ValueDecl *Requirement;
  public:
    PrettyStackTraceRequirement(const char *action,
                                const ProtocolConformance *conformance,
                                ValueDecl *requirement)
      : Action(action), Conformance(conformance), Requirement(requirement) { }

    void print(llvm::raw_ostream &out) const override {
      out << "While " << Action << " requirement ";
      Requirement->dumpRef(out);
      out << " in conformance ";
      Conformance->printName(out);
      out << "\n";
    }
  };
} // end anonymous namespace

bool NormalProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                               LazyResolver *resolver) const {
  if (Resolver)
    resolveLazyInfo();

  if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
    return true;
  }
  if (resolver) {
    PrettyStackTraceRequirement trace("resolving", this, assocType);
    resolver->resolveTypeWitness(this, assocType);
    if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
      return true;
    }
  }
  return false;
}

/// Directly resolve type witnesses that are known to the compiler because they
/// were synthesized by the compiler.
///
/// FIXME: This is a hack to work around the fact that we don't have a
/// TypeChecker when we need one.
///
/// \returns true if we resolved the type witness.
static bool resolveKnownTypeWitness(NormalProtocolConformance *conformance,
                                    AssociatedTypeDecl *assocType) {
  auto nominal = conformance->getType()->getAnyNominal();
  if (!nominal) return false;

  if (!nominal->hasClangNode()) return false;

  auto proto = conformance->getProtocol();
  auto knownKind = proto->getKnownProtocolKind();
  if (!knownKind) return false;

  auto &ctx = nominal->getASTContext();
  (void)ctx;

  // Local function to handle resolution via lookup directly into the nominal
  // type.
  auto resolveViaLookup = [&] {
    for (auto member : nominal->lookupDirect(assocType->getFullName())) {
      auto memberType = dyn_cast<TypeDecl>(member);
      if (!memberType) continue;
      if (memberType->getDeclContext() != nominal) continue;

      conformance->setTypeWitness(assocType,
                                  nominal->mapTypeIntoContext(
                                    memberType->getDeclaredInterfaceType()),
                                  memberType);
      return true;
    }

    return false;
  };

  // RawRepresentable.RawValue.
  if (*knownKind == KnownProtocolKind::RawRepresentable) {
    assert(assocType->getName() == ctx.Id_RawValue);
    if (auto enumDecl = dyn_cast<EnumDecl>(nominal)) {
      // First, try to resolve via lookup, so we get the declaration.
      if (resolveViaLookup()) return true;

      // Otherwise, use the raw type.
      if (enumDecl->hasRawType()) {
        conformance->setTypeWitness(assocType, enumDecl->getRawType(), nullptr);
        return true;
      }

      return false;
    }

    // All other cases resolve via lookup.
    return resolveViaLookup();
  }

  // OptionSet.Element.
  if (*knownKind == KnownProtocolKind::OptionSet) {
    assert(assocType->getName() == ctx.Id_Element);
    return resolveViaLookup();
  }

  // _ObjectiveCBridgeable._ObjectiveCType
  if (*knownKind == KnownProtocolKind::ObjectiveCBridgeable) {
    assert(assocType->getName() == ctx.Id_ObjectiveCType);
    return resolveViaLookup();
  }

  // _BridgedStoredNSError.Code
  if (*knownKind == KnownProtocolKind::BridgedStoredNSError) {
    assert(assocType->getName() == ctx.Id_Code);
    return resolveViaLookup();
  }

  // ErrorCodeProtocol._ErrorType.
  if (*knownKind == KnownProtocolKind::ErrorCodeProtocol) {
    assert(assocType->getName() == ctx.Id_ErrorType);
    return resolveViaLookup();
  }

  return false;
}

std::pair<Type, TypeDecl *>
NormalProtocolConformance::getTypeWitnessAndDecl(AssociatedTypeDecl *assocType,
                                                 LazyResolver *resolver,
                                                 SubstOptions options) const {
  if (Resolver)
    resolveLazyInfo();

  // Check whether we already have a type witness.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end())
    return known->second;

  // If this conformance is in a state where it is inferring type witnesses,
  // check tentative witnesses.
  if (getState() == ProtocolConformanceState::CheckingTypeWitnesses) {
    // If there is a tentative-type-witness function, use it.
    if (options.getTentativeTypeWitness) {
     if (Type witnessType =
           Type(options.getTentativeTypeWitness(this, assocType)))
        return { witnessType, nullptr };
    }

    // Otherwise, we fail; this is the only case in which we can return a
    // null type.
    return { Type(), nullptr };
  }

  // Otherwise, resolve the type witness.
  PrettyStackTraceRequirement trace("resolving", this, assocType);
  if (!resolveKnownTypeWitness(const_cast<NormalProtocolConformance *>(this),
                               assocType)) {
    assert(resolver && "Unable to resolve type witness");
    resolver->resolveTypeWitness(this, assocType);
  }
  known = TypeWitnesses.find(assocType);
  assert(known != TypeWitnesses.end() && "Didn't resolve witness?");
  return known->second;
}

void NormalProtocolConformance::setTypeWitness(AssociatedTypeDecl *assocType,
                                               Type type,
                                               TypeDecl *typeDecl) const {
  assert(getProtocol() == cast<ProtocolDecl>(assocType->getDeclContext()) &&
         "associated type in wrong protocol");
  assert(TypeWitnesses.count(assocType) == 0 && "Type witness already known");
  assert((!isComplete() || isInvalid()) && "Conformance already complete?");
  TypeWitnesses[assocType] = std::make_pair(type, typeDecl);
}

Type ProtocolConformance::getAssociatedType(Type assocType,
                                            LazyResolver *resolver) const {
  assert(assocType->isTypeParameter() &&
         "associated type must be a type parameter");

  ProtocolConformanceRef ref(const_cast<ProtocolConformance*>(this));
  return ref.getAssociatedType(getType(), assocType, resolver);
}

Type ProtocolConformanceRef::getAssociatedType(Type conformingType,
                                               Type assocType,
                                               LazyResolver *resolver) const {
  assert(!isConcrete() || getConcrete()->getType()->isEqual(conformingType));

  auto type = assocType->getCanonicalType();
  auto proto = getRequirement();

#if false
  // Fast path for generic parameters.
  if (isa<GenericTypeParamType>(type)) {
    assert(type->isEqual(proto->getSelfInterfaceType()) &&
           "type parameter in protocol was not Self");
    return getType();
  }

  // Fast path for dependent member types on 'Self' of our associated types.
  auto memberType = cast<DependentMemberType>(type);
  if (memberType.getBase()->isEqual(proto->getProtocolSelfType()) &&
      memberType->getAssocType()->getProtocol() == proto)
    return getTypeWitness(memberType->getAssocType(), nullptr);
#endif

  // General case: consult the substitution map.
  auto substMap =
    SubstitutionMap::getProtocolSubstitutions(proto, conformingType, *this);
  return type.subst(substMap);
}

ProtocolConformanceRef
ProtocolConformanceRef::getAssociatedConformance(Type conformingType,
                                                 Type assocType,
                                                 ProtocolDecl *protocol,
                                                 LazyResolver *resolver) const {
  // If this is a concrete conformance, look up the associated conformance.
  if (isConcrete()) {
    auto conformance = getConcrete();
    assert(conformance->getType()->isEqual(conformingType));
    return conformance->getAssociatedConformance(assocType, protocol, resolver);
  }

  // Otherwise, apply the substitution {self -> conformingType}
  // to the abstract conformance requirement laid upon the dependent type
  // by the protocol.
  auto subMap =
    SubstitutionMap::getProtocolSubstitutions(getRequirement(),
                                              conformingType, *this);
  auto abstractConf = ProtocolConformanceRef(protocol);
  return abstractConf.subst(assocType,
                            QuerySubstitutionMap{subMap},
                            LookUpConformanceInSubstitutionMap(subMap));
}

ProtocolConformanceRef
ProtocolConformance::getAssociatedConformance(Type assocType,
                                               ProtocolDecl *protocol,
                                               LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getAssociatedConformance,
                                (assocType, protocol, resolver))
}

ProtocolConformanceRef
NormalProtocolConformance::getAssociatedConformance(Type assocType,
                                                    ProtocolDecl *protocol,
                                                LazyResolver *resolver) const {
  assert(assocType->isTypeParameter() &&
         "associated type must be a type parameter");
  assert(!getSignatureConformances().empty() &&
         "signature conformances not yet computed");

  unsigned conformanceIndex = 0;
  for (auto &reqt :
         getProtocol()->getRequirementSignature()->getRequirements()) {
    if (reqt.getKind() == RequirementKind::Conformance) {
      // Is this the conformance we're looking for?
      if (reqt.getFirstType()->isEqual(assocType) &&
          reqt.getSecondType()->castTo<ProtocolType>()->getDecl() == protocol)
        return getSignatureConformances()[conformanceIndex];

      ++conformanceIndex;
    }
  }

  llvm_unreachable(
    "requested conformance was not a direct requirement of the protocol");
}

  /// Retrieve the value witness corresponding to the given requirement.
Witness NormalProtocolConformance::getWitness(ValueDecl *requirement,
                                              LazyResolver *resolver) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(requirement->isProtocolRequirement() && "Not a requirement");

  if (Resolver)
    resolveLazyInfo();

  auto known = Mapping.find(requirement);
  if (known == Mapping.end()) {
    assert(resolver && "Unable to resolve witness without resolver");
    resolver->resolveWitness(this, requirement);
    known = Mapping.find(requirement);
  }
  if (known != Mapping.end()) {
    return known->second;
  } else {
    assert((!isComplete() || isInvalid()) &&
           "Resolver did not resolve requirement");
    return Witness();
  }
}

void NormalProtocolConformance::setWitness(ValueDecl *requirement,
                                           Witness witness) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
  assert(getProtocol() == cast<ProtocolDecl>(requirement->getDeclContext()) &&
         "requirement in wrong protocol");
  assert(Mapping.count(requirement) == 0 && "Witness already known");
  assert((!isComplete() || isInvalid() ||
          requirement->getAttrs().hasAttribute<OptionalAttr>() ||
          requirement->getAttrs().isUnavailable(
                                        requirement->getASTContext())) &&
         "Conformance already complete?");
  Mapping[requirement] = witness;
}

SpecializedProtocolConformance::SpecializedProtocolConformance(
    Type conformingType,
    ProtocolConformance *genericConformance,
    SubstitutionList substitutions)
  : ProtocolConformance(ProtocolConformanceKind::Specialized, conformingType),
    GenericConformance(genericConformance),
    GenericSubstitutions(substitutions)
{
  assert(genericConformance->getKind() != ProtocolConformanceKind::Specialized);
}

bool SpecializedProtocolConformance::hasTypeWitness(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  return TypeWitnesses.find(assocType) != TypeWitnesses.end() ||
         GenericConformance->hasTypeWitness(assocType, resolver);
}

std::pair<Type, TypeDecl *>
SpecializedProtocolConformance::getTypeWitnessAndDecl(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver,
                      SubstOptions options) const {
  // If we've already created this type witness, return it.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end()) {
    return known->second;
  }

  // Otherwise, perform substitutions to create this witness now.
  auto *genericSig = GenericConformance->getGenericSignature();

  auto substitutionMap =
      genericSig->getSubstitutionMap(GenericSubstitutions);

  // Local function to determine whether we will end up
  auto normal = GenericConformance->getRootNormalConformance();
  auto isTentativeWitness = [&] {
    if (normal->getState() != ProtocolConformanceState::CheckingTypeWitnesses)
      return false;

    return !normal->hasTypeWitness(assocType, nullptr);
  };

  auto genericWitnessAndDecl
    = GenericConformance->getTypeWitnessAndDecl(assocType, resolver, options);

  auto genericWitness = genericWitnessAndDecl.first;
  if (!genericWitness)
    return { Type(), nullptr };

  auto *typeDecl = genericWitnessAndDecl.second;

  // Apply the substitution we computed above
  auto specializedType
    = genericWitness.subst(substitutionMap, options);
  if (!specializedType) {
    if (isTentativeWitness())
      return { Type(), nullptr };

    specializedType = ErrorType::get(genericWitness);
  }

  // If we aren't in a case where we used the tentative type witness
  // information, cache the result.
  auto specializedWitnessAndDecl = std::make_pair(specializedType, typeDecl);
  if (!isTentativeWitness() && !specializedType->hasError())
    TypeWitnesses[assocType] = specializedWitnessAndDecl;

  return specializedWitnessAndDecl;
}

ProtocolConformanceRef
SpecializedProtocolConformance::getAssociatedConformance(Type assocType,
                                                ProtocolDecl *protocol,
                                                LazyResolver *resolver) const {
  ProtocolConformanceRef conformance =
    GenericConformance->getAssociatedConformance(assocType, protocol, resolver);

  auto genericSig = GenericConformance->getGenericSignature();
  auto subMap = genericSig->getSubstitutionMap(GenericSubstitutions);

  Type origType =
    (conformance.isConcrete()
       ? conformance.getConcrete()->getType()
       : GenericConformance->getAssociatedType(assocType, resolver));

  return conformance.subst(origType,
                           QuerySubstitutionMap{subMap},
                           LookUpConformanceInSubstitutionMap(subMap));
}

ProtocolConformanceRef
InheritedProtocolConformance::getAssociatedConformance(Type assocType,
                         ProtocolDecl *protocol,
                         LazyResolver *resolver) const {
  auto underlying =
    InheritedConformance->getAssociatedConformance(assocType, protocol,
                                                   resolver);


  // If the conformance is for Self, return an inherited conformance.
  if (underlying.isConcrete() &&
      assocType->isEqual(getProtocol()->getSelfInterfaceType())) {
    auto subclassType = getType();
    ASTContext &ctx = subclassType->getASTContext();
    return ProtocolConformanceRef(
             ctx.getInheritedConformance(subclassType,
                                         underlying.getConcrete()));
  }

  return underlying;
}

const NormalProtocolConformance *
ProtocolConformance::getRootNormalConformance() const {
  const ProtocolConformance *C = this;
  while (!isa<NormalProtocolConformance>(C)) {
    switch (C->getKind()) {
    case ProtocolConformanceKind::Normal:
      llvm_unreachable("should have broken out of loop");
    case ProtocolConformanceKind::Inherited:
      C = cast<InheritedProtocolConformance>(C)
          ->getInheritedConformance();
      break;
    case ProtocolConformanceKind::Specialized:
      C = cast<SpecializedProtocolConformance>(C)
        ->getGenericConformance();
      break;
    }
  }
  return cast<NormalProtocolConformance>(C);
}

bool ProtocolConformance::isVisibleFrom(const DeclContext *dc) const {
  // FIXME: Implement me!
  return true;
}

ProtocolConformance *
ProtocolConformance::subst(Type substType,
                           TypeSubstitutionFn subs,
                           LookupConformanceFn conformances) const {
  // ModuleDecl::lookupConformance() strips off dynamic Self, so
  // we should do the same here.
  if (auto selfType = substType->getAs<DynamicSelfType>())
    substType = selfType->getSelfType();

  if (getType()->isEqual(substType))
    return const_cast<ProtocolConformance *>(this);
  
  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    if (substType->isSpecialized()) {
      assert(getType()->isSpecialized()
             && "substitution mapped non-specialized to specialized?!");
      assert(getType()->getNominalOrBoundGenericNominal()
               == substType->getNominalOrBoundGenericNominal()
             && "substitution mapped to different nominal?!");

      SubstitutionMap subMap;
      if (getGenericSignature()) {
        auto *genericEnv = getGenericEnvironment();
        subMap = genericEnv->getSubstitutionMap(subs, conformances);
      }

      return substType->getASTContext()
        .getSpecializedConformance(substType,
                                   const_cast<ProtocolConformance *>(this),
                                   subMap);
    }

    assert(substType->isEqual(getType())
           && "substitution changed non-specialized type?!");
    return const_cast<ProtocolConformance *>(this);
  }
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this)->getInheritedConformance();
    ProtocolConformance *newBase;
    if (inheritedConformance->getType()->isSpecialized()) {
      newBase = inheritedConformance->subst(substType, subs, conformances);
    } else {
      newBase = inheritedConformance;
    }

    return substType->getASTContext()
      .getInheritedConformance(substType, newBase);
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance
      = cast<SpecializedProtocolConformance>(this)->getGenericConformance();
    auto subMap =
      genericConformance->getGenericSignature()
        ->getSubstitutionMap(spec->getGenericSubstitutions());

    return substType->getASTContext()
      .getSpecializedConformance(substType, genericConformance,
                                 subMap.subst(subs, conformances));
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

ProtocolConformance *
ProtocolConformance::getInheritedConformance(ProtocolDecl *protocol) const {
  auto result =
    getAssociatedConformance(getProtocol()->getSelfInterfaceType(), protocol);
  return result.isConcrete() ? result.getConcrete() : nullptr;
}

#pragma mark Protocol conformance lookup
void NominalTypeDecl::prepareConformanceTable() const {
  if (ConformanceTable)
    return;

  auto mutableThis = const_cast<NominalTypeDecl *>(this);
  ASTContext &ctx = getASTContext();
  auto resolver = ctx.getLazyResolver();
  ConformanceTable = new (ctx) ConformanceLookupTable(ctx, mutableThis,
                                                      resolver);

  // If this type declaration was not parsed from source code or introduced
  // via the Clang importer, don't add any synthesized conformances.
  auto *file = cast<FileUnit>(getModuleScopeContext());
  if (file->getKind() != FileUnitKind::Source &&
      file->getKind() != FileUnitKind::ClangModule) {
    return;
  }

  // Add any synthesized conformances.
  if (auto theEnum = dyn_cast<EnumDecl>(mutableThis)) {
    if (theEnum->hasCases() && theEnum->hasOnlyCasesWithoutAssociatedValues()) {
      // Simple enumerations conform to Equatable.
      if (auto equatable = ctx.getProtocol(KnownProtocolKind::Equatable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis, equatable);
      }

      // Simple enumerations conform to Hashable.
      if (auto hashable = getASTContext().getProtocol(
                            KnownProtocolKind::Hashable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis, hashable);
      }
    }

    // Enumerations with a raw type conform to RawRepresentable.
    if (resolver)
      resolver->resolveRawType(theEnum);
    if (theEnum->hasRawType()) {
      if (auto rawRepresentable =
            ctx.getProtocol(KnownProtocolKind::RawRepresentable)) {
        ConformanceTable->addSynthesizedConformance(mutableThis,
                                                    rawRepresentable);
      }
    }
  }

  // Add protocols for any synthesized protocol attributes.
  for (auto attr : getAttrs()) {
    if (auto synthesizedProto = dyn_cast<SynthesizedProtocolAttr>(attr)) {
      if (auto proto = getASTContext().getProtocol(
                         synthesizedProto->getProtocolKind())) {
        ConformanceTable->addSynthesizedConformance(mutableThis, proto);
      }
    }
  }
}

bool NominalTypeDecl::lookupConformance(
       ModuleDecl *module, ProtocolDecl *protocol,
       SmallVectorImpl<ProtocolConformance *> &conformances) const {
  prepareConformanceTable();
  return ConformanceTable->lookupConformance(
           module,
           const_cast<NominalTypeDecl *>(this),
           protocol,
           getASTContext().getLazyResolver(),
           conformances);
}

SmallVector<ProtocolDecl *, 2> NominalTypeDecl::getAllProtocols() const {
  prepareConformanceTable();
  SmallVector<ProtocolDecl *, 2> result;
  ConformanceTable->getAllProtocols(const_cast<NominalTypeDecl *>(this),
                                    getASTContext().getLazyResolver(),
                                    result);
  return result;
}

SmallVector<ProtocolConformance *, 2> NominalTypeDecl::getAllConformances(
                                        bool sorted) const
{
  prepareConformanceTable();
  SmallVector<ProtocolConformance *, 2> result;
  ConformanceTable->getAllConformances(const_cast<NominalTypeDecl *>(this),
                                       getASTContext().getLazyResolver(),
                                       sorted,
                                       result);
  return result;
}

void NominalTypeDecl::getImplicitProtocols(
       SmallVectorImpl<ProtocolDecl *> &protocols) {
  prepareConformanceTable();
  ConformanceTable->getImplicitProtocols(this, protocols);
}

void NominalTypeDecl::registerProtocolConformance(
       ProtocolConformance *conformance) {
  prepareConformanceTable();
  ConformanceTable->registerProtocolConformance(conformance);
}

ArrayRef<ValueDecl *>
NominalTypeDecl::getSatisfiedProtocolRequirementsForMember(
                                             const ValueDecl *member,
                                             bool sorted) const {
  assert(member->getDeclContext()->getAsNominalTypeOrNominalTypeExtensionContext()
           == this);
  assert(!isa<ProtocolDecl>(this));
  prepareConformanceTable();
  return ConformanceTable->getSatisfiedProtocolRequirementsForMember(member,
                                           const_cast<NominalTypeDecl *>(this),
                                           getASTContext().getLazyResolver(),
                                           sorted);
}

SmallVector<ProtocolDecl *, 2>
DeclContext::getLocalProtocols(
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics,
  bool sorted) const
{
  SmallVector<ProtocolDecl *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal)
    return result;

  // Update to record all potential conformances.
  nominal->prepareConformanceTable();
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    getASTContext().getLazyResolver(),
    lookupKind,
    &result,
    nullptr,
    diagnostics);

  // Sort if required.
  if (sorted) {
    llvm::array_pod_sort(result.begin(), result.end(),
                         &ProtocolType::compareProtocols);
  }

  return result;
}

SmallVector<ProtocolConformance *, 2>
DeclContext::getLocalConformances(
  ConformanceLookupKind lookupKind,
  SmallVectorImpl<ConformanceDiagnostic> *diagnostics,
  bool sorted) const
{
  SmallVector<ProtocolConformance *, 2> result;

  // Dig out the nominal type.
  NominalTypeDecl *nominal = getAsNominalTypeOrNominalTypeExtensionContext();
  if (!nominal)
    return result;

  // Protocols don't have conformances.
  if (isa<ProtocolDecl>(nominal))
    return { };

  // Update to record all potential conformances.
  nominal->prepareConformanceTable();
  nominal->ConformanceTable->lookupConformances(
    nominal,
    const_cast<DeclContext *>(this),
    nominal->getASTContext().getLazyResolver(),
    lookupKind,
    nullptr,
    &result,
    diagnostics);

  // If requested, sort the results.
  if (sorted) {
    llvm::array_pod_sort(result.begin(), result.end(),
                         &ConformanceLookupTable::compareProtocolConformances);
  }

  return result;
}

/// Check of all types used by the conformance are canonical.
bool ProtocolConformance::isCanonical() const {
  // Normal conformances are always canonical by construction.
  if (getKind() == ProtocolConformanceKind::Normal)
    return true;

  if (!getType()->isCanonical())
    return false;

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    return true;
  }
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this);
    return inheritedConformance->getInheritedConformance()->isCanonical();
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance = spec->getGenericConformance();
    if (!genericConformance->isCanonical())
      return false;
    auto specSubs = spec->getGenericSubstitutions();
    for (const auto &sub : specSubs) {
      if (!sub.isCanonical())
        return false;
    }
    return true;
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

Substitution Substitution::getCanonicalSubstitution(bool *wasCanonical) const {
  bool createdNewCanonicalConformances = false;
  bool createdCanReplacement = false;
  SmallVector<ProtocolConformanceRef, 4> newCanConformances;

  CanType canReplacement = getReplacement()->getCanonicalType();

  if (!getReplacement()->isCanonical()) {
    createdCanReplacement = true;
  }

  for (auto conf : getConformances()) {
    if (conf.isCanonical()) {
      newCanConformances.push_back(conf);
      continue;
    }
    newCanConformances.push_back(conf.getCanonicalConformanceRef());
    createdNewCanonicalConformances = true;
  }

  ArrayRef<ProtocolConformanceRef> canConformances = getConformances();
  if (createdNewCanonicalConformances) {
    auto &C = canReplacement->getASTContext();
    canConformances = C.AllocateCopy(newCanConformances);
  }

  if (createdCanReplacement || createdNewCanonicalConformances) {
    if (wasCanonical)
      *wasCanonical = false;
    return Substitution(canReplacement, canConformances);
  }
  if (wasCanonical)
    *wasCanonical = true;
  return *this;
}

SubstitutionList
swift::getCanonicalSubstitutionList(SubstitutionList subs,
                                    SmallVectorImpl<Substitution> &canSubs) {
  bool subListWasCanonical = true;
  for (auto &sub : subs) {
    bool subWasCanonical = false;
    auto canSub = sub.getCanonicalSubstitution(&subWasCanonical);
    if (!subWasCanonical)
      subListWasCanonical = false;
    canSubs.push_back(canSub);
  }

  if (subListWasCanonical) {
    canSubs.clear();
    return subs;
  }

  subs = canSubs;
  return subs;
}

/// Check of all types used by the conformance are canonical.
ProtocolConformance *ProtocolConformance::getCanonicalConformance() {
  if (isCanonical())
    return this;

  switch (getKind()) {
  case ProtocolConformanceKind::Normal: {
    // Normal conformances are always canonical by construction.
    return this;
  }

  case ProtocolConformanceKind::Inherited: {
    auto &Ctx = getType()->getASTContext();
    auto inheritedConformance = cast<InheritedProtocolConformance>(this);
    return Ctx.getInheritedConformance(
        getType()->getCanonicalType(),
        inheritedConformance->getInheritedConformance()
            ->getCanonicalConformance());
  }

  case ProtocolConformanceKind::Specialized: {
    auto &Ctx = getType()->getASTContext();
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto genericConformance = spec->getGenericConformance();
    auto specSubs = spec->getGenericSubstitutions();
    SmallVector<Substitution, 4> newSpecSubs;
    auto canSpecSubs = getCanonicalSubstitutionList(specSubs, newSpecSubs);
    return Ctx.getSpecializedConformance(
        getType()->getCanonicalType(),
        genericConformance->getCanonicalConformance(),
        newSpecSubs.empty() ? canSpecSubs : Ctx.AllocateCopy(canSpecSubs));
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

/// Check of all types used by the conformance are canonical.
bool ProtocolConformanceRef::isCanonical() const {
  if (isAbstract())
    return true;
  return getConcrete()->isCanonical();
}

ProtocolConformanceRef
ProtocolConformanceRef::getCanonicalConformanceRef() const {
  if (isAbstract())
    return *this;
  return ProtocolConformanceRef(getConcrete()->getCanonicalConformance());
}
