//===--- SubstitutionMap.cpp - Type substitution map ----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the SubstitutionMap class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"

using namespace swift;

Optional<ProtocolConformanceRef> SubstitutionMap::
lookupConformance(ProtocolDecl *proto,
                  ArrayRef<ProtocolConformanceRef> conformances) const {
  for (ProtocolConformanceRef found : conformances) {
    auto foundProto = found.getRequirement();
    if (foundProto == proto)
      return found;
    if (foundProto->inheritsFrom(proto))
      return found.getInherited(proto);
  }

  return None;
}

template<typename Fn>
Optional<ProtocolConformanceRef>
SubstitutionMap::forEachParent(CanType type, Fn fn) const {
  auto foundParents = parentMap.find(type.getPointer());
  if (foundParents != parentMap.end()) {
    for (auto parent : foundParents->second) {
      if (auto result = fn(parent.first, parent.second))
        return result;
    }
  }

  if (auto archetypeType = dyn_cast<ArchetypeType>(type))
    if (auto *parent = archetypeType->getParent())
      return fn(CanType(parent), archetypeType->getAssocType());

  if (auto memberType = dyn_cast<DependentMemberType>(type))
    return fn(CanType(memberType->getBase()), memberType->getAssocType());

  return None;
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(CanType type,
                                   ProtocolDecl *proto) const {
  // Check for conformances for the type that apply to the original
  // substituted archetype.
  auto foundReplacement = conformanceMap.find(type.getPointer());
  if (foundReplacement != conformanceMap.end()) {
    auto substReplacement = foundReplacement->second;
    if (auto conformance = lookupConformance(proto, substReplacement))
      return conformance;
  }

  // Check if we have substitutions from one of our parent types.
  return forEachParent(type, [&](CanType parent, AssociatedTypeDecl *assocType)
      -> Optional<ProtocolConformanceRef> {

    auto *parentProto = assocType->getProtocol();
    auto conformance = lookupConformance(parent, parentProto);

    if (!conformance)
      return None;

    if (!conformance->isConcrete())
      return ProtocolConformanceRef(proto);

    auto sub = conformance->getConcrete()->getTypeWitnessSubstAndDecl(
        assocType, nullptr).first;

    return lookupConformance(proto, sub.getConformances());
  });
}

void SubstitutionMap::
addSubstitution(CanType type, Type replacement) {
  auto result = subMap.insert(std::make_pair(type.getPointer(), replacement));
  assert(result.second);
  (void) result;
}

void SubstitutionMap::
addConformances(CanType type, ArrayRef<ProtocolConformanceRef> conformances) {
  if (conformances.empty())
    return;

  auto result = conformanceMap.insert(
      std::make_pair(type.getPointer(), conformances));
  assert(result.second);
  (void) result;
}

void SubstitutionMap::
addParent(CanType type, CanType parent, AssociatedTypeDecl *assocType) {
  assert(type && parent && assocType);
  parentMap[type.getPointer()].push_back(std::make_pair(parent, assocType));
}

void SubstitutionMap::removeType(CanType type) {
  subMap.erase(type.getPointer());
  conformanceMap.erase(type.getPointer());
  parentMap.erase(type.getPointer());
}
