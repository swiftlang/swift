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
// This file defines the SubstitutionMap class. A SubstitutionMap packages
// together a set of replacement types and protocol conformances for
// specializing generic types.
//
// SubstitutionMaps either have type parameters or archetypes as keys,
// based on whether they were built from a GenericSignature or a
// GenericEnvironment.
//
// To specialize a type, call Type::subst() with the right SubstitutionMap.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/Debug.h"

using namespace swift;

bool SubstitutionMap::hasArchetypes() const {
  for (auto &entry : subMap)
    if (entry.second->hasArchetype())
      return true;
  return false;
}

bool SubstitutionMap::hasOpenedExistential() const {
  for (auto &entry : subMap)
    if (entry.second->hasOpenedExistential())
      return true;
  return false;
}

bool SubstitutionMap::hasDynamicSelf() const {
  for (auto &entry : subMap)
    if (entry.second->hasDynamicSelfType())
      return true;
  return false;
}

Type SubstitutionMap::lookupSubstitution(CanSubstitutableType type) const {
  auto known = subMap.find(type);
  if (known != subMap.end() && known->second)
    return known->second;

  // Not known.
  return Type();
}

void SubstitutionMap::
addSubstitution(CanSubstitutableType type, Type replacement) {
  auto result = subMap.insert(std::make_pair(type, replacement));
  assert(result.second || result.first->second->isEqual(replacement));
  (void) result;
}

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
        // Narrow fix since this whole code path is going away soon
        // (if you find this code still here a year from now, I will be
        // very ashamed).
        //
        // FIXME: Should always have a valid generic environment.
        if (protoAssocType->getProtocol()->getGenericEnvironment()) {
          for (auto assocProto : protoAssocType->getConformingProtocols()) {
            if (auto found = fn(ProtocolConformanceRef(assocProto)))
              return found;
          }
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
    for (auto inherited : proto->getInheritedProtocols()) {
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
addConformance(CanType type, ProtocolConformanceRef conformance) {
  conformanceMap[type.getPointer()].push_back(conformance);
}

void SubstitutionMap::
addParent(CanType type, CanType parent, AssociatedTypeDecl *assocType) {
  assert(type && parent && assocType);
  parentMap[type.getPointer()].push_back(std::make_pair(parent, assocType));
}


SubstitutionMap SubstitutionMap::subst(const SubstitutionMap &subMap) const {
  return subst(QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

SubstitutionMap SubstitutionMap::subst(TypeSubstitutionFn subs,
                                       LookupConformanceFn conformances) const {
  SubstitutionMap result(*this);

  for (auto iter = result.subMap.begin(),
            end = result.subMap.end();
       iter != end; ++iter) {
    iter->second = iter->second.subst(subs, conformances,
                                      SubstFlags::UseErrorType);
  }

  for (auto iter = result.conformanceMap.begin(),
            end = result.conformanceMap.end();
       iter != end; ++iter) {
    auto origType = Type(iter->first).subst(
        *this, SubstFlags::UseErrorType);
    for (auto citer = iter->second.begin(),
              cend = iter->second.end();
         citer != cend; ++citer) {
      *citer = citer->subst(origType, subs, conformances);
    }
  }

  result.verify();

  return result;
}

SubstitutionMap
SubstitutionMap::getProtocolSubstitutions(ProtocolDecl *protocol,
                                          Type selfType,
                                          ProtocolConformanceRef conformance) {
  auto protocolSelfType = protocol->getSelfInterfaceType();

  return protocol->getGenericSignature()->getSubstitutionMap(
    [&](SubstitutableType *type) -> Type {
      if (type->isEqual(protocolSelfType))
        return selfType;

      // This will need to change if we ever support protocols
      // inside generic types.
      return Type();
    },
    [&](CanType origType, Type replacementType, ProtocolType *protoType)
      -> Optional<ProtocolConformanceRef> {
      if (origType->isEqual(protocolSelfType) &&
          protoType->getDecl() == protocol)
        return conformance;

      // This will need to change if we ever support protocols
      // inside generic types.
      return None;
    });
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
  if (baseSig == nullptr)
    return SubstitutionMap();

  auto *M = baseClass->getParentModule();

  unsigned baseDepth = 0;
  SubstitutionMap baseSubMap;
  if (auto *baseClassSig = baseClass->getGenericSignature()) {
    baseDepth = baseClassSig->getGenericParams().back()->getDepth() + 1;

    auto derivedClassTy = derivedClass->getDeclaredInterfaceType();
    if (derivedSubs)
      derivedClassTy = derivedClassTy.subst(*derivedSubs);
    auto baseClassTy = derivedClassTy->getSuperclassForDecl(baseClass, resolver);

    baseSubMap = baseClassTy->getContextSubstitutionMap(M, baseClass);
  }

  unsigned origDepth = 0;
  if (auto *derivedClassSig = derivedClass->getGenericSignature())
    origDepth = derivedClassSig->getGenericParams().back()->getDepth() + 1;

  SubstitutionMap origSubMap;
  if (derivedSubs)
    origSubMap = *derivedSubs;
  else if (derivedSig) {
    origSubMap = derivedSig->getSubstitutionMap(
        [](SubstitutableType *type) -> Type { return type; },
        MakeAbstractConformanceForGenericType());
  }

  return combineSubstitutionMaps(baseSubMap, origSubMap,
                                 CombineSubstitutionMaps::AtDepth,
                                 baseDepth, origDepth,
                                 baseSig);
}

SubstitutionMap
SubstitutionMap::combineSubstitutionMaps(const SubstitutionMap &firstSubMap,
                                         const SubstitutionMap &secondSubMap,
                                         CombineSubstitutionMaps how,
                                         unsigned firstDepthOrIndex,
                                         unsigned secondDepthOrIndex,
                                         GenericSignature *genericSig) {
  auto &ctx = genericSig->getASTContext();

  auto replaceGenericParameter = [&](Type type) -> Type {
    if (auto gp = type->getAs<GenericTypeParamType>()) {
      if (how == CombineSubstitutionMaps::AtDepth) {
        if (gp->getDepth() < firstDepthOrIndex)
          return Type();
        return GenericTypeParamType::get(
          gp->getDepth() + secondDepthOrIndex - firstDepthOrIndex,
          gp->getIndex(),
          ctx);
      }

      assert(how == CombineSubstitutionMaps::AtIndex);
      if (gp->getIndex() < firstDepthOrIndex)
        return Type();
      return GenericTypeParamType::get(
        gp->getDepth(),
        gp->getIndex() + secondDepthOrIndex - firstDepthOrIndex,
        ctx);
    }

    return type;
  };

  return genericSig->getSubstitutionMap(
    [&](SubstitutableType *type) {
      auto replacement = replaceGenericParameter(type);
      if (replacement)
        return Type(replacement).subst(secondSubMap);
      return Type(type).subst(firstSubMap);
    },
    [&](CanType type, Type substType, ProtocolType *conformedProtocol) {
      auto replacement = type.transform(replaceGenericParameter);
      if (replacement)
        return secondSubMap.lookupConformance(replacement->getCanonicalType(),
                                              conformedProtocol->getDecl());
      return firstSubMap.lookupConformance(type,
                                           conformedProtocol->getDecl());
    });
}

void SubstitutionMap::verify() const {
  // FIXME: Remove the conditional compilation once the substitutions
  // machinery and GenericSignatureBuilder always generate correct
  // SubstitutionMaps.
#if 0 && !defined(NDEBUG)
  for (auto iter = conformanceMap.begin(), end = conformanceMap.end();
       iter != end; ++iter) {
    auto replacement = Type(iter->first).subst(*this, SubstFlags::UseErrorType);
    if (replacement->isTypeParameter() || replacement->is<ArchetypeType>() ||
        replacement->isTypeVariableOrMember() ||
        replacement->is<UnresolvedType>() || replacement->hasError())
      continue;
    // Check conformances of a concrete replacement type.
    for (auto citer = iter->second.begin(), cend = iter->second.end();
         citer != cend; ++citer) {
      // An existential type can have an abstract conformance to
      // AnyObject or an @objc protocol.
      if (citer->isAbstract() && replacement->isExistentialType()) {
        auto *proto = citer->getRequirement();
        assert((proto->isSpecificProtocol(KnownProtocolKind::AnyObject) ||
                proto->isObjC()) &&
               "an existential type can conform only to AnyObject or an "
               "@objc-protocol");
        continue;
      }
      // All of the conformances should be concrete.
      if (!citer->isConcrete()) {
        llvm::dbgs() << "Concrete replacement type:\n";
        replacement->dump(llvm::dbgs());
        llvm::dbgs() << "SubstitutionMap:\n";
        dump(llvm::dbgs());
      }
      assert(citer->isConcrete() && "Conformance should be concrete");
    }
  }
#endif
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
