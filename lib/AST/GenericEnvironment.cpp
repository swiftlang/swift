//===--- GenericEnvironment.cpp - GenericEnvironment AST ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
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
#include "swift/AST/ProtocolConformance.h"

using namespace swift;

GenericEnvironment::GenericEnvironment(
    GenericSignature *signature,
    TypeSubstitutionMap interfaceToArchetypeMap)
  : Signature(signature)
{
  // Build a mapping in both directions, making sure to canonicalize the
  // interface type where it is used as a key, so that substitution can
  // find them, and to preserve sugar otherwise, so that
  // mapTypeOutOfContext() produces a human-readable type.
  for (auto entry : interfaceToArchetypeMap)
    addMapping(entry.first->castTo<GenericTypeParamType>(), entry.second);

  // Make sure this generic environment gets destroyed.
  signature->getASTContext().addDestructorCleanup(*this);
}

void GenericEnvironment::addMapping(GenericParamKey key,
                                    Type contextType) {
  // We're going to pass InterfaceToArchetypeMap to Type::subst(), which
  // expects the keys to be canonical, otherwise it won't be able to
  // find them.
  auto genericParams = Signature->getGenericParams();
  auto genericParam = genericParams[key.findIndexIn(genericParams)];
  auto canParamTy =
    cast<GenericTypeParamType>(genericParam->getCanonicalType());

  // Add the mapping form the generic parameter to the context type.
  assert(InterfaceToArchetypeMap.count(canParamTy) == 0 && "Duplicate entry");
  InterfaceToArchetypeMap[canParamTy] = contextType;

  // If we mapped the generic parameter to an archetype, add it to the
  // reverse mapping.
  auto *archetype = contextType->getAs<ArchetypeType>();
  if (!archetype) return;

  // Check whether we've already recorded an interface type for this archetype.
  // If not, record one and we're done.
  auto result = ArchetypeToInterfaceMap.insert({archetype, genericParam});
  if (result.second) return;

  // Multiple generic parameters map to the same archetype. If the
  // existing entry comes from a later generic parameter, replace it with
  // the earlier generic parameter. This gives us a deterministic reverse
  // mapping.
  auto otherGP = result.first->second->castTo<GenericTypeParamType>();
  if (std::make_pair(canParamTy->getDepth(), canParamTy->getIndex())
        < std::make_pair(otherGP->getDepth(), otherGP->getIndex()))
    result.first->second = genericParam;
}

Optional<Type> GenericEnvironment::getMappingIfPresent(
                                                    GenericParamKey key) const {
  auto genericParam = GenericTypeParamType::get(key.Depth, key.Index,
                                                Signature->getASTContext());
  auto canParamTy =
    cast<GenericTypeParamType>(genericParam->getCanonicalType());

  auto found = InterfaceToArchetypeMap.find(canParamTy);
  if (found == InterfaceToArchetypeMap.end()) return None;

  return found->second;
}

void *GenericEnvironment::operator new(size_t bytes, const ASTContext &ctx) {
  return ctx.Allocate(bytes, alignof(GenericEnvironment), AllocationArena::Permanent);
}

bool GenericEnvironment::containsPrimaryArchetype(
                                              ArchetypeType *archetype) const {
  return ArchetypeToInterfaceMap.count(archetype) > 0;
}

Type GenericEnvironment::mapTypeOutOfContext(ModuleDecl *M, Type type) const {
  type = type.subst(M, ArchetypeToInterfaceMap, SubstFlags::AllowLoweredTypes);
  assert(!type->hasArchetype() && "not fully substituted");
  return type;
}

Type GenericEnvironment::mapTypeIntoContext(ModuleDecl *M, Type type) const {
  type = type.subst(M, InterfaceToArchetypeMap, SubstFlags::AllowLoweredTypes);
  assert((!type->hasTypeParameter() || type->hasError()) &&
         "not fully substituted");
  return type;
}

Type GenericEnvironment::mapTypeIntoContext(GenericTypeParamType *type) const {
  auto canTy = type->getCanonicalType();
  auto found =
    InterfaceToArchetypeMap.find(canTy->castTo<GenericTypeParamType>());
  assert(found != InterfaceToArchetypeMap.end() &&
         "missing generic parameter");
  return found->second;
}

GenericTypeParamType *GenericEnvironment::getSugaredType(
    GenericTypeParamType *type) const {
  for (auto *sugaredType : getGenericParams())
    if (sugaredType->isEqual(type))
      return sugaredType;

  llvm_unreachable("missing generic parameter");
}

ArrayRef<Substitution>
GenericEnvironment::getForwardingSubstitutions(ModuleDecl *M) const {
  auto lookupConformanceFn =
      [&](CanType original, Type replacement, ProtocolType *protoType)
          -> ProtocolConformanceRef {
    return ProtocolConformanceRef(protoType->getDecl());
  };

  SmallVector<Substitution, 4> result;
  getGenericSignature()->getSubstitutions(*M, InterfaceToArchetypeMap,
                                          lookupConformanceFn, result);
  return getGenericSignature()->getASTContext().AllocateCopy(result);
}

SubstitutionMap GenericEnvironment::
getSubstitutionMap(ModuleDecl *mod,
                   ArrayRef<Substitution> subs) const {
  SubstitutionMap result;
  getSubstitutionMap(mod, subs, result);
  return result;
}

void GenericEnvironment::
getSubstitutionMap(ModuleDecl *mod,
                   ArrayRef<Substitution> subs,
                   SubstitutionMap &result) const {
  for (auto depTy : getGenericSignature()->getAllDependentTypes()) {

    // Map the interface type to a context type.
    auto contextTy = depTy.subst(mod, InterfaceToArchetypeMap, SubstOptions());
    auto *archetype = contextTy->castTo<ArchetypeType>();

    auto sub = subs.front();
    subs = subs.slice(1);

    // Record the replacement type and its conformances.
    result.addSubstitution(CanType(archetype), sub.getReplacement());
    result.addConformances(CanType(archetype), sub.getConformances());
  }

  for (auto reqt : getGenericSignature()->getRequirements()) {
    if (reqt.getKind() != RequirementKind::SameType)
      continue;

    auto first = reqt.getFirstType()->getAs<DependentMemberType>();
    auto second = reqt.getSecondType()->getAs<DependentMemberType>();

    if (!first || !second)
      continue;

    auto archetype = mapTypeIntoContext(mod, first)->getAs<ArchetypeType>();
    if (!archetype)
      continue;

    auto firstBase = first->getBase();
    auto secondBase = second->getBase();

    auto firstBaseArchetype = mapTypeIntoContext(mod, firstBase)->getAs<ArchetypeType>();
    auto secondBaseArchetype = mapTypeIntoContext(mod, secondBase)->getAs<ArchetypeType>();

    if (!firstBaseArchetype || !secondBaseArchetype)
      continue;

    if (archetype->getParent() != firstBaseArchetype)
      result.addParent(CanType(archetype),
                       CanType(firstBaseArchetype),
                       first->getAssocType());
    if (archetype->getParent() != secondBaseArchetype)
      result.addParent(CanType(archetype),
                       CanType(secondBaseArchetype),
                       second->getAssocType());
  }

  assert(subs.empty() && "did not use all substitutions?!");
}
