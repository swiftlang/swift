//===--- SubstitutionMap.cpp - Type substitution map ----------------------===//
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
// This file defines the SubstitutionMap class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
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
addSubstitution(CanSubstitutableType type, Type replacement) {
  auto result = subMap.insert(std::make_pair(type, replacement));
  assert(result.second || result.first->second->isEqual(replacement));
  (void) result;
}

void SubstitutionMap::
addConformances(CanType type, ArrayRef<ProtocolConformanceRef> conformances) {
  if (conformances.empty())
    return;

  auto result = conformanceMap.insert(
      std::make_pair(type.getPointer(), conformances));
  assert(result.second ||
         result.first->getSecond().size() == conformances.size());
  (void) result;
}

ArrayRef<ProtocolConformanceRef> SubstitutionMap::
getConformances(CanType type) const {
  auto known = conformanceMap.find(type.getPointer());
  if (known == conformanceMap.end()) return { };
  return known->second;
}

void SubstitutionMap::
addParent(CanType type, CanType parent, AssociatedTypeDecl *assocType) {
  assert(type && parent && assocType);
  parentMap[type.getPointer()].push_back(std::make_pair(parent, assocType));
}

SubstitutionMap
SubstitutionMap::getOverrideSubstitutions(const ValueDecl *baseDecl,
                                          const ValueDecl *derivedDecl,
                                          Optional<SubstitutionMap> derivedSubs,
                                          LazyResolver *resolver) {
  auto *baseClass = baseDecl->getDeclContext()
      ->getAsClassOrClassExtensionContext();
  auto *derivedClass = derivedDecl->getDeclContext()
      ->getAsClassOrClassExtensionContext();

  auto *baseSig = baseDecl->getInnermostDeclContext()
      ->getGenericSignatureOfContext();
  auto *derivedSig = derivedDecl->getInnermostDeclContext()
      ->getGenericSignatureOfContext();

  return getOverrideSubstitutions(baseClass, derivedClass,
                                  baseSig, derivedSig,
                                  derivedSubs,
                                  resolver);
}

SubstitutionMap
SubstitutionMap::getOverrideSubstitutions(const ClassDecl *baseClass,
                                          const ClassDecl *derivedClass,
                                          GenericSignature *baseSig,
                                          GenericSignature *derivedSig,
                                          Optional<SubstitutionMap> derivedSubs,
                                          LazyResolver *resolver) {
  SubstitutionMap subMap;

  if (baseSig == nullptr)
    return subMap;

  unsigned minDepth = 0;

  // Get the substitutions for the self type.
  if (auto *genericSig = baseClass->getGenericSignature()) {
    auto derivedClassTy = derivedClass->getDeclaredInterfaceType();
    if (derivedSubs)
      derivedClassTy = derivedClassTy.subst(*derivedSubs);
    auto baseClassTy = derivedClassTy->getSuperclassForDecl(baseClass, resolver);

    auto *M = baseClass->getParentModule();
    auto subs = baseClassTy->gatherAllSubstitutions(M, resolver);
    genericSig->getSubstitutionMap(subs, subMap);

    minDepth = genericSig->getGenericParams().back()->getDepth() + 1;
  }

  // Map the innermost generic parameters of the derived function to
  // the base.
  auto &ctx = baseClass->getASTContext();

  auto baseParams = baseSig->getInnermostGenericParams();
  if (baseParams.back()->getDepth() >= minDepth) {
    assert(derivedSig);
    auto derivedParams = derivedSig->getInnermostGenericParams();

    assert(baseParams.size() == derivedParams.size());

    for (unsigned i = 0, e = derivedParams.size(); i < e; i++) {
      auto paramTy = cast<GenericTypeParamType>(baseParams[i]->getCanonicalType());
      assert(paramTy->getDepth() >= minDepth);
      Type replacementTy = derivedParams[i];
      if (derivedSubs)
        replacementTy = replacementTy.subst(*derivedSubs);
      subMap.addSubstitution(paramTy, replacementTy);
    }

    auto isRootedInInnermostParameter = [&](Type t) -> bool {
      while (auto *dmt = t->getAs<DependentMemberType>())
        t = dmt->getBase();
      return t->castTo<GenericTypeParamType>()->getDepth() >= minDepth;
    };

    // Add trivial conformances for the above.
    // FIXME: This should be less awkward.
    baseSig->enumeratePairedRequirements(
      [&](Type t, ArrayRef<Requirement> reqs) -> bool {
        auto canTy = t->getCanonicalType();

        if (isRootedInInnermostParameter(t)) {
          auto conformances =
              ctx.AllocateUninitialized<ProtocolConformanceRef>(
                  reqs.size());
          for (unsigned i = 0, e = reqs.size(); i < e; i++) {
            auto reqt = reqs[i];
            assert(reqt.getKind() == RequirementKind::Conformance);
            auto *proto = reqt.getSecondType()
                ->castTo<ProtocolType>()->getDecl();
            if (derivedSubs)
              conformances[i] = *derivedSubs->lookupConformance(canTy, proto);
            else
              conformances[i] = ProtocolConformanceRef(proto);
          }

          subMap.addConformances(canTy, conformances);
        }

        return false;
    });
  }

  return subMap;
}

SubstitutionMap
SubstitutionMap::combineSubstitutionMaps(const SubstitutionMap &baseSubMap,
                                         const SubstitutionMap &origSubMap,
                                         unsigned baseDepth,
                                         unsigned origDepth,
                                         GenericSignature *baseSig) {
  auto replaceGenericParameter = [&](Type type) -> Type {
    if (auto gp = type->getAs<GenericTypeParamType>()) {
      if (gp->getDepth() < baseDepth) return Type();
      return GenericTypeParamType::get(gp->getDepth() + origDepth - baseDepth,
                                       gp->getIndex(),
                                       baseSig->getASTContext());
    }

    return type;
  };

  return baseSig->getSubstitutionMap(
    [&](SubstitutableType *type) {
      auto replacement = replaceGenericParameter(type);
      if (replacement)
        return Type(replacement).subst(origSubMap);
      return Type(type).subst(baseSubMap);
    },
    [&](CanType type, Type substType, ProtocolType *conformedProtocol) {
      auto replacement = type.transform(replaceGenericParameter);
      if (replacement)
        return origSubMap.lookupConformance(replacement->getCanonicalType(),
                                            conformedProtocol->getDecl());
      return baseSubMap.lookupConformance(type,
                                          conformedProtocol->getDecl());
    });
}
