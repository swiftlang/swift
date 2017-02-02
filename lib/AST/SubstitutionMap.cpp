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
              llvm::SmallPtrSetImpl<CanType> &visitedParents,
              llvm::function_ref<Optional<T>(CanType,
                                             AssociatedTypeDecl *)> fn) const {
  // If we've already visited the parents of this type, stop.
  if (!visitedParents.insert(type).second)
    return None;

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
              llvm::SmallPtrSetImpl<CanType> &visitedParents,
              llvm::function_ref<Optional<T>(ProtocolConformanceRef)> fn) const{
  // Check for conformances for the type that apply to the original
  // substituted archetype.
  auto foundReplacement = conformanceMap.find(type.getPointer());
  if (foundReplacement != conformanceMap.end()) {
    for (auto conformance : foundReplacement->second) {
      if (auto found = fn(conformance))
        return found;
    }
  }

  // Local function to performance a (recursive) search for an associated type
  // of the given name in the given conformance and all inherited conformances.
  std::function<Optional<T>(ProtocolConformanceRef, DeclName,
                                 llvm::SmallPtrSetImpl<ProtocolDecl *> &)>
    searchInConformance;
  searchInConformance =
      [&](ProtocolConformanceRef conformance,
          DeclName associatedTypeName,
          llvm::SmallPtrSetImpl<ProtocolDecl *> &visited) -> Optional<T> {
    // Only visit a particular protocol once.
    auto proto = conformance.getRequirement();
    if (!visited.insert(proto).second) return None;

    // Check whether this protocol has an associated type with the
    // same name as the one we're looking for.
    AssociatedTypeDecl *protoAssocType = nullptr;
    for (auto member : proto->lookupDirect(associatedTypeName)) {
      protoAssocType = dyn_cast<AssociatedTypeDecl>(member);
      if (protoAssocType) break;
    }

    if (protoAssocType) {
      if (conformance.isAbstract()) {
        for (auto assocProto : protoAssocType->getConformingProtocols()) {
          if (auto found = fn(ProtocolConformanceRef(assocProto)))
            return found;
        }
      } else {
       auto sub = conformance.getConcrete()->getTypeWitnessSubstAndDecl(
                                           protoAssocType, nullptr).first;
       for (auto subConformance : sub.getConformances()) {
         if (auto found = fn(subConformance))
           return found;
       }
      }
    }

    // Search inherited conformances.
    for (auto inherited : proto->getInheritedProtocols(nullptr)) {
      if (auto found = searchInConformance(conformance.getInherited(inherited),
                                           associatedTypeName,
                                           visited))
        return found;
    }
    return None;
  };

  // Check if we have conformances from one of our parent types.
  return forEachParent<ProtocolConformanceRef>(type, visitedParents,
      [&](CanType parent, AssociatedTypeDecl *assocType)
         -> Optional<ProtocolConformanceRef> {
    return forEachConformance<T>(parent, visitedParents,
        [&](ProtocolConformanceRef conformance) -> Optional<T> {
      llvm::SmallPtrSet<ProtocolDecl *, 4> visited;
      return searchInConformance(conformance, assocType->getFullName(),
                                 visited);
    });
  });
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(
                         CanType type, ProtocolDecl *proto,
                         llvm::SmallPtrSetImpl<CanType> *visitedParents) const {
  // Local function to either record an abstract conformance or return a
  // concrete conformance. This allows us to check multiple parents and
  // find the most specific conformance that applies.
  Optional<ProtocolConformanceRef> abstractConformance;
  auto recordOrReturn = [&](ProtocolConformanceRef conformance)
      -> Optional<ProtocolConformanceRef> {
    if (conformance.isAbstract()) {
      if (!abstractConformance)
        abstractConformance = conformance;

      return None;
    }

    return conformance;
  };

  llvm::SmallPtrSet<CanType, 4> visitedParentsStored;
  if (!visitedParents)
    visitedParents = &visitedParentsStored;

  auto concreteConformance =
    forEachConformance<ProtocolConformanceRef>(type, *visitedParents,
        [&](ProtocolConformanceRef conformance)
          -> Optional<ProtocolConformanceRef> {
      if (conformance.getRequirement() == proto)
        return recordOrReturn(conformance);

      if (conformance.getRequirement()->inheritsFrom(proto))
        return recordOrReturn(conformance.getInherited(proto));

       return None;
    });

  return concreteConformance ? concreteConformance : abstractConformance;
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
