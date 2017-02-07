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
#include "swift/Basic/Fallthrough.h"
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
#include "llvm/Support/SaveAndRestore.h"

using namespace swift;

Witness::Witness(ValueDecl *decl, SubstitutionList substitutions,
                 GenericEnvironment *syntheticEnv,
                 SubstitutionMap reqToSynthesizedEnvMap) {
  auto &ctx = decl->getASTContext();

  auto declRef = ConcreteDeclRef(ctx, decl, substitutions);
  auto storedMem = ctx.Allocate(sizeof(StoredWitness), alignof(StoredWitness));
  auto stored = new (storedMem)
      StoredWitness{declRef, syntheticEnv, std::move(reqToSynthesizedEnvMap)};
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

std::pair<const Substitution &, TypeDecl *>
ProtocolConformance::getTypeWitnessSubstAndDecl(AssociatedTypeDecl *assocType,
                                                LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getTypeWitnessSubstAndDecl,
                                (assocType, resolver))
}

const Substitution &
ProtocolConformance::getTypeWitness(AssociatedTypeDecl *assocType, 
                                    LazyResolver *resolver) const {
  return getTypeWitnessSubstAndDecl(assocType, resolver).first;
}

Type
ProtocolConformance::getTypeWitnessByName(Type type,
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
  return concrete->getTypeWitness(assocType, resolver).getReplacement();
}

Witness ProtocolConformance::getWitness(ValueDecl *requirement,
                                        LazyResolver *resolver) const {
  CONFORMANCE_SUBCLASS_DISPATCH(getWitness, (requirement, resolver))
}

const InheritedConformanceMap &
ProtocolConformance::getInheritedConformances() const {
  CONFORMANCE_SUBCLASS_DISPATCH(getInheritedConformances, ())
}

/// Determine whether the witness for the given requirement
/// is either the default definition or was otherwise deduced.
bool ProtocolConformance::
usesDefaultDefinition(AssociatedTypeDecl *requirement) const {
  CONFORMANCE_SUBCLASS_DISPATCH(usesDefaultDefinition, (requirement))
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

bool NormalProtocolConformance::hasTypeWitness(AssociatedTypeDecl *assocType,
                                               LazyResolver *resolver) const {
  if (Resolver)
    resolveLazyInfo();

  if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
    return true;
  }
  if (resolver) {
    resolver->resolveTypeWitness(this, assocType);
    if (TypeWitnesses.find(assocType) != TypeWitnesses.end()) {
      return true;
    }
  }
  return false;
}

std::pair<const Substitution &, TypeDecl *>
NormalProtocolConformance::getTypeWitnessSubstAndDecl(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  if (Resolver)
    resolveLazyInfo();

  auto known = TypeWitnesses.find(assocType);
  if (known == TypeWitnesses.end()) {
    assert(resolver && "Unable to resolve type witness");
    resolver->resolveTypeWitness(this, assocType);
    known = TypeWitnesses.find(assocType);
    assert(known != TypeWitnesses.end() && "Didn't resolve witness?");
  }

  return known->second;
}

void NormalProtocolConformance::setTypeWitness(
       AssociatedTypeDecl *assocType,
       const Substitution &substitution,
       TypeDecl *typeDecl) const {
  assert(getProtocol() == cast<ProtocolDecl>(assocType->getDeclContext()) &&
         "associated type in wrong protocol");
  assert(TypeWitnesses.count(assocType) == 0 && "Type witness already known");
  assert((!isComplete() || isInvalid()) && "Conformance already complete?");
  TypeWitnesses[assocType] = std::make_pair(substitution, typeDecl);
}

  /// Retrieve the value witness corresponding to the given requirement.
Witness NormalProtocolConformance::getWitness(ValueDecl *requirement,
                                              LazyResolver *resolver) const {
  assert(!isa<AssociatedTypeDecl>(requirement) && "Request type witness");
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
  assert((!isComplete() || isInvalid()) && "Conformance already complete?");
  Mapping[requirement] = witness;
}

SpecializedProtocolConformance::SpecializedProtocolConformance(
    Type conformingType,
    ProtocolConformance *genericConformance,
    SubstitutionList substitutions)
  : ProtocolConformance(ProtocolConformanceKind::Specialized, conformingType,
                        // FIXME: interface type should be passed in.
                        // assumes specialized conformance is always fully
                        // specialized
                        conformingType),
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

std::pair<const Substitution &, TypeDecl *>
SpecializedProtocolConformance::getTypeWitnessSubstAndDecl(
                      AssociatedTypeDecl *assocType, 
                      LazyResolver *resolver) const {
  // If we've already created this type witness, return it.
  auto known = TypeWitnesses.find(assocType);
  if (known != TypeWitnesses.end()) {
    return known->second;
  }

  // Otherwise, perform substitutions to create this witness now.
  auto *genericEnv = GenericConformance->getGenericEnvironment();

  auto substitutionMap =
      genericEnv->getSubstitutionMap(GenericSubstitutions);

  auto genericWitnessAndDecl
    = GenericConformance->getTypeWitnessSubstAndDecl(assocType, resolver);

  auto &genericWitness = genericWitnessAndDecl.first;
  auto *typeDecl = genericWitnessAndDecl.second;

  // Apply the substitution we computed above
  auto specializedType
    = genericWitness.getReplacement().subst(substitutionMap, None);
  if (!specializedType)
    specializedType = ErrorType::get(genericWitness.getReplacement());

  // If the type witness was unchanged, just copy it directly.
  if (specializedType.getPointer() == genericWitness.getReplacement().getPointer()) {
    TypeWitnesses[assocType] = genericWitnessAndDecl;
    return TypeWitnesses[assocType];
  }

  auto conformingDC = getDeclContext();
  auto conformingModule = conformingDC->getParentModule();

  // Gather the conformances for the type witness. These should never fail.
  // FIXME: We should just be able to use the SubstitutionMap from above,
  // but we have no way to force inherited conformances to be filled in
  // through that mechanism.
  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto proto : assocType->getConformingProtocols()) {
    auto conforms = conformingModule->lookupConformance(specializedType, proto,
                                                        resolver);
    assert((conforms ||
            specializedType->isTypeVariableOrMember() ||
            specializedType->isTypeParameter() ||
            specializedType->hasError()) &&
           "Improperly checked substitution");
    conformances.push_back(conforms ? *conforms 
                                    : ProtocolConformanceRef(proto));
  }

  // Form the substitution.
  auto &ctx = assocType->getASTContext();
  TypeWitnesses[assocType] = std::make_pair(
                        Substitution{specializedType,
                                     ctx.AllocateCopy(conformances)},
                        typeDecl);
  return TypeWitnesses[assocType];
}

Witness
SpecializedProtocolConformance::getWitness(ValueDecl *requirement,
                                           LazyResolver *resolver) const {
  // FIXME: Apply substitutions here!
  return GenericConformance->getWitness(requirement, resolver);
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

ProtocolConformance *ProtocolConformance::subst(ModuleDecl *module,
                                       Type substType,
                                       TypeSubstitutionFn subs,
                                       LookupConformanceFn conformances) const {
  if (getType()->isEqual(substType))
    return const_cast<ProtocolConformance *>(this);
  
  switch (getKind()) {
  case ProtocolConformanceKind::Normal:
    if (substType->isSpecialized()) {
      assert(getType()->isSpecialized()
             && "substitution mapped non-specialized to specialized?!");
      assert(getType()->getNominalOrBoundGenericNominal()
               == substType->getNominalOrBoundGenericNominal()
             && "substitution mapped to different nominal?!");
      return substType->getASTContext()
        .getSpecializedConformance(substType,
                           const_cast<ProtocolConformance *>(this),
                           substType->gatherAllSubstitutions(module, nullptr));
    }
    assert(substType->isEqual(getType())
           && "substitution changed non-specialized type?!");
    return const_cast<ProtocolConformance *>(this);
      
  case ProtocolConformanceKind::Inherited: {
    // Substitute the base.
    auto inheritedConformance
      = cast<InheritedProtocolConformance>(this)->getInheritedConformance();
    ProtocolConformance *newBase;
    if (inheritedConformance->getType()->isSpecialized()) {
      newBase = inheritedConformance->subst(module, substType,
                                            subs, conformances);
    } else {
      newBase = inheritedConformance;
    }

    return substType->getASTContext()
      .getInheritedConformance(substType, newBase);
  }
  case ProtocolConformanceKind::Specialized: {
    // Substitute the substitutions in the specialized conformance.
    auto spec = cast<SpecializedProtocolConformance>(this);
    SmallVector<Substitution, 8> newSubs;
    newSubs.reserve(spec->getGenericSubstitutions().size());
    for (auto &sub : spec->getGenericSubstitutions())
      newSubs.push_back(sub.subst(module, subs, conformances));
    
    auto ctxNewSubs = substType->getASTContext().AllocateCopy(newSubs);
    
    return substType->getASTContext()
      .getSpecializedConformance(substType, spec->getGenericConformance(),
                                 ctxNewSubs);
  }
  }
  llvm_unreachable("bad ProtocolConformanceKind");
}

ProtocolConformance *
ProtocolConformance::getInheritedConformance(ProtocolDecl *protocol) const {
  auto &C = getProtocol()->getASTContext();
  // Preserve specialization and class inheritance through this operation by
  // reapplying them to the conformance we find.
  switch (getKind()) {
  case ProtocolConformanceKind::Specialized: {
    auto spec = cast<SpecializedProtocolConformance>(this);
    auto inherited = spec->getGenericConformance()
      ->getInheritedConformance(protocol);
    assert(inherited->getType()->isEqual(spec->getGenericConformance()->getType())
           && "inherited conformance doesn't match type?!");
    
    auto subs = spec->getGenericSubstitutions();

    auto *conformingDC = spec->getDeclContext();
    auto *conformingModule = conformingDC->getParentModule();

    auto *env = conformingDC->getGenericEnvironmentOfContext();

    auto subMap = env->getSubstitutionMap(subs);

    auto r = inherited->subst(conformingModule, getType(),
                              QuerySubstitutionMap{subMap},
                              LookUpConformanceInSubstitutionMap(subMap));
    assert(getType()->isEqual(r->getType())
           && "substitution didn't produce conformance for same type?!");
    return r;
  }
    
  case ProtocolConformanceKind::Inherited: {
    auto classInherited = cast<InheritedProtocolConformance>(this);
    auto protoInherited = classInherited->getInheritedConformance()
      ->getInheritedConformance(protocol);
    assert(protoInherited->getType()->isEqual(
                           classInherited->getInheritedConformance()->getType())
           && "inherited conformance doesn't match type?!");
    return C.getInheritedConformance(classInherited->getType(),
                                     protoInherited);
  }

  case ProtocolConformanceKind::Normal:
    // For a normal conformance, do the inheritance lookup.
    break;
  }

  // Search for the inherited conformance among our immediate parents.
  auto &inherited = getInheritedConformances();
  auto known = inherited.find(protocol);
  if (known != inherited.end())
    return known->second;

  // If not there, the inherited conformance must be available through one of
  // our parents.
  for (auto &inheritedMapping : inherited)
    if (inheritedMapping.first->inheritsFrom(protocol))
      return inheritedMapping.second->getInheritedConformance(protocol);

  // The conformance must not be complete; resolve the inherited conformance
  // and try again.
  assert(!isComplete() && "Missing inherited mapping in conformance");
  assert(C.getLazyResolver() && "Need a lazy resolver");
  return C.getLazyResolver()->resolveInheritedConformance(
    getRootNormalConformance(), protocol);
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
  if (!getParentSourceFile() && !hasClangNode())
    return;

  // Add any synthesized conformances.
  if (isa<ClassDecl>(this)) {
    if (auto anyObject = getASTContext().getProtocol(
                           KnownProtocolKind::AnyObject)) {
      ConformanceTable->addSynthesizedConformance(mutableThis, anyObject);
    }
  } else if (auto theEnum = dyn_cast<EnumDecl>(mutableThis)) {
    if (theEnum->hasOnlyCasesWithoutAssociatedValues()) {
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
      if (auto rawRepresentable = ctx.getProtocol(KnownProtocolKind::RawRepresentable)) {

        // The presence of a raw type is an explicit declaration that
        // the compiler should derive a RawRepresentable conformance.
        auto conformance = ctx.getConformance(mutableThis->getDeclaredTypeInContext(), rawRepresentable,
                                              mutableThis->getNameLoc(), mutableThis->getInnermostDeclContext(),
                                              ProtocolConformanceState::Incomplete);
        ConformanceTable->registerProtocolConformance(conformance);
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
