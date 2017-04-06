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

#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/Debug.h"

using namespace swift;

SubstitutionMap::SubstitutionMap(GenericEnvironment *genericEnv)
  : SubstitutionMap(genericEnv->getGenericSignature()) { }

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
  assert(!(type->isTypeParameter() && !getGenericSignature()) &&
         "type parameter substitution map without generic signature");
  auto result = subMap.insert(std::make_pair(type, replacement));
  assert(result.second || result.first->second->isEqual(replacement));
  (void) result;
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(CanType type, ProtocolDecl *proto) const {
  // If we have an archetype, map out of the context so we can compute a
  // conformance access path.
  GenericEnvironment *genericEnv = nullptr;
  if (auto archetype = type->getAs<ArchetypeType>()) {
    genericEnv = archetype->getGenericEnvironment();
    type = genericEnv->mapTypeOutOfContext(type)->getCanonicalType();
  }

  // Error path: if we don't have a type parameter, there is no conformance.
  // FIXME: Query concrete conformances in the generic signature?
  if (!type->isTypeParameter())
    return None;

  // Retrieve the starting conformance from the conformance map.
  auto getInitialConformance =
    [&](Type type, ProtocolDecl *proto) -> Optional<ProtocolConformanceRef> {
      // We're working relative to a generic environment, map into that
      // context before looking into the conformance map.
      if (genericEnv)
        type = genericEnv->mapTypeIntoContext(type);

      auto known = conformanceMap.find(type->getCanonicalType().getPointer());
      if (known == conformanceMap.end())
        return None;

      for (auto conformance : known->second) {
        if (conformance.getRequirement() == proto)
          return conformance;
      }

      return None;
    };

  auto genericSig = getGenericSignature();
  auto &mod = *proto->getModuleContext();

  // HACK: Deal with AnyObject conformances, which get magically dropped in
  // frustrating ways.
  // FIXME: This hack dies with AnyObject-as-a-protocol.
  if (proto->isSpecificProtocol(KnownProtocolKind::AnyObject) &&
      genericSig->requiresClass(type, mod))
    return ProtocolConformanceRef(proto);

  // If the type doesn't conform to this protocol, fail.
  if (!genericSig->conformsToProtocol(type, proto, mod))
    return None;

  auto canonType = genericSig->getCanonicalTypeInContext(type, mod);
  auto accessPath =
    genericSig->getConformanceAccessPath(canonType, proto, mod);

  // Fall through because we cannot yet evaluate an access path.
  Optional<ProtocolConformanceRef> conformance;
  for (const auto &step : accessPath) {
    // For the first step, grab the initial conformance.
    if (!conformance) {
      conformance = getInitialConformance(step.first, step.second);
      if (!conformance)
        return None;

      continue;
    }

    // If we've hit an abstract conformance, everything from here on out is
    // abstract.
    // FIXME: This may not always be true, but it holds for now.
    if (conformance->isAbstract())
      return ProtocolConformanceRef(proto);

    // For the second step, we're looking into the requirement signature for
    // this protocol.
    auto concrete = conformance->getConcrete();
    auto normal = concrete->getRootNormalConformance();

    // If we haven't set the signature conformances yet, force the issue now.
    if (normal->getSignatureConformances().empty()) {
      auto lazyResolver = canonType->getASTContext().getLazyResolver();
      lazyResolver->resolveTypeWitness(normal, nullptr);

      // Error case: the conformance is broken, so we cannot handle this
      // substitution.
      if (normal->getSignatureConformances().empty())
        return None;
    }

    // Get the associated conformance.
    conformance = concrete->getAssociatedConformance(step.first, step.second);
  }

  return conformance;
}

void SubstitutionMap::
addConformance(CanType type, ProtocolConformanceRef conformance) {
  conformanceMap[type.getPointer()].push_back(conformance);
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
  out << "Generic signature: ";
  getGenericSignature()->print(out);
  out << "\n";
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
}

void SubstitutionMap::dump() const {
  return dump(llvm::errs());
}
