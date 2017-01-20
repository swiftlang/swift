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

template<typename T>
Optional<T> SubstitutionMap::forEachParent(
              CanType type,
              llvm::function_ref<Optional<T>(CanType,
                                             AssociatedTypeDecl *)> fn) const {
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

template<typename T>
Optional<T> SubstitutionMap::forEachConformance(
                CanType type,
                llvm::function_ref<Optional<T>(ProtocolConformanceRef)> fn)
              const {
  // Check for conformances for the type that apply to the original
  // substituted archetype.
  auto foundReplacement = conformanceMap.find(type.getPointer());
  if (foundReplacement != conformanceMap.end()) {
    for (auto conformance : foundReplacement->second) {
      if (auto found = fn(conformance))
        return found;
    }
  }

  // Check if we have conformances from one of our parent types.
  return forEachParent<ProtocolConformanceRef>(type,
      [&](CanType parent, AssociatedTypeDecl *assocType)
         -> Optional<ProtocolConformanceRef> {
    auto *parentProto = assocType->getProtocol();
    auto conformance = lookupConformance(parent, parentProto);

    if (!conformance)
      return None;

    if (!conformance->isConcrete()) {
      for (auto proto : assocType->getConformingProtocols()) {
        if (auto found = fn(ProtocolConformanceRef(proto)))
          return found;
      }

      return None;
    }

    auto sub = conformance->getConcrete()->getTypeWitnessSubstAndDecl(
        assocType, nullptr).first;
    for (auto conf : sub.getConformances())
      if (auto found = fn(conf))
        return found;

    return None;
  });
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(CanType type, ProtocolDecl *proto) const {
  return forEachConformance<ProtocolConformanceRef>(type,
             [&](ProtocolConformanceRef conformance)
               -> Optional<ProtocolConformanceRef> {
           if (conformance.getRequirement() == proto)
             return conformance;

           if (conformance.getRequirement()->inheritsFrom(proto))
             return conformance.getInherited(proto);

           return None;
         });
}

void SubstitutionMap::
addSubstitution(CanSubstitutableType type, Type replacement) {
  auto result = subMap.insert(std::make_pair(type, replacement));
  assert(result.second || result.first->second->isEqual(replacement));
  (void) result;
}

void SubstitutionMap::
addConformance(CanType type, ProtocolConformanceRef conformance) {
  conformanceMap[type.getPointer()].push_back(conformance);
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
          for (unsigned i = 0, e = reqs.size(); i < e; i++) {
            auto reqt = reqs[i];
            assert(reqt.getKind() == RequirementKind::Conformance);
            auto *proto = reqt.getSecondType()
                ->castTo<ProtocolType>()->getDecl();
            if (derivedSubs)
              subMap.addConformance(canTy, *derivedSubs->lookupConformance(
                                                                 canTy, proto));
            else
              subMap.addConformance(canTy, ProtocolConformanceRef(proto));
          }
        }

        return false;
    });
  }

  return subMap;
}

void SubstitutionMap::dump(llvm::raw_ostream &out) const {
  out << "Substitutions:\n";
  for (const auto &sub : subMap) {
    out.indent(2);
    sub.first->print(out);
    out << " -> ";
    sub.second->print(out);
    out << "\n";
  }

  out << "\nConformance map:\n";
  for (const auto &conformances : conformanceMap) {
    out.indent(2);
    conformances.first->print(out);
    out << " -> [";
    interleave(conformances.second.begin(), conformances.second.end(),
               [&](ProtocolConformanceRef conf) {
                 conf.dump(out);
               },
               [&] {
                 out << ", ";
               });
    out << "]\n";
  }

  out << "\nParent map:\n";
  for (const auto &parent : parentMap) {
    out.indent(2);
    parent.first->print(out);
    out << " -> [";
    interleave(parent.second.begin(), parent.second.end(),
               [&](SubstitutionMap::ParentType parentType) {
                 parentType.first->print(out);
                 out << " @ ";
                 out << parentType.second->getProtocol()->getName().str()
                     << "." << parentType.second->getName().str();
               },
               [&] {
                 out << ", ";
               });
    out << "]\n";
  }
}

void SubstitutionMap::dump() const {
  return dump(llvm::errs());
}
