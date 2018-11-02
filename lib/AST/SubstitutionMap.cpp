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
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/Debug.h"

using namespace swift;

ArrayRef<Type> SubstitutionMap::getReplacementTypes() const {
  if (empty()) return { };

  return llvm::makeArrayRef(replacementTypes.get(),
                            genericSig->getGenericParams().size());
}

MutableArrayRef<Type> SubstitutionMap::getReplacementTypes() {
  if (empty()) return { };

  return MutableArrayRef<Type>(replacementTypes.get(),
                               genericSig->getGenericParams().size());

}

SubstitutionMap::SubstitutionMap(GenericSignature *genericSig) : genericSig(genericSig) {
  if (genericSig) {
    replacementTypes.reset(new Type [genericSig->getGenericParams().size()]);
  }
}

SubstitutionMap::SubstitutionMap(GenericEnvironment *genericEnv)
  : SubstitutionMap(genericEnv->getGenericSignature()) { }

SubstitutionMap::SubstitutionMap(const SubstitutionMap &other)
  : SubstitutionMap(other.getGenericSignature())
{
  std::copy(other.getReplacementTypes().begin(),
            other.getReplacementTypes().end(),
            getReplacementTypes().begin());

  conformanceMap = other.conformanceMap;
}

SubstitutionMap &SubstitutionMap::operator=(const SubstitutionMap &other) {
  *this = SubstitutionMap(other);
  return *this;
}

SubstitutionMap::~SubstitutionMap() { }

bool SubstitutionMap::hasArchetypes() const {
  for (Type replacementTy : getReplacementTypes()) {
    if (replacementTy && replacementTy->hasArchetype())
      return true;
  }
  return false;
}

bool SubstitutionMap::hasOpenedExistential() const {
  for (Type replacementTy : getReplacementTypes()) {
    if (replacementTy && replacementTy->hasOpenedExistential())
      return true;
  }
  return false;
}

bool SubstitutionMap::hasDynamicSelf() const {
  for (Type replacementTy : getReplacementTypes()) {
    if (replacementTy && replacementTy->hasDynamicSelfType())
      return true;
  }
  return false;
}

Type SubstitutionMap::lookupSubstitution(CanSubstitutableType type) const {
  // If we have an archetype, map out of the context so we can compute a
  // conformance access path.
  if (auto archetype = dyn_cast<ArchetypeType>(type)) {
    if (archetype->isOpenedExistential() ||
        archetype->getParent() != nullptr)
      return Type();

    type = cast<GenericTypeParamType>(
      archetype->getInterfaceType()->getCanonicalType());
  }

  // Find the index of the replacement type based on the generic parameter we
  // have.
  auto genericParam = cast<GenericTypeParamType>(type);
  auto mutableThis = const_cast<SubstitutionMap *>(this);
  auto replacementTypes = mutableThis->getReplacementTypes();
  auto genericSig = getGenericSignature();
  assert(genericSig);
  auto genericParams = genericSig->getGenericParams();
  auto replacementIndex =
    GenericParamKey(genericParam).findIndexIn(genericParams);

  // If this generic parameter isn't represented, we don't have a replacement
  // type for it.
  if (replacementIndex == genericParams.size())
    return Type();

  // If we already have a replacement type, return it.
  Type &replacementType = replacementTypes[replacementIndex];
  if (replacementType)
    return replacementType;

  // The generic parameter may have been made concrete by the generic signature,
  // substitute into the concrete type.
  if (auto concreteType = genericSig->getConcreteType(genericParam)){
    // Set the replacement type to an error, to block infinite recursion.
    replacementType = ErrorType::get(concreteType);

    // Substitute into the replacement type.
    replacementType = concreteType.subst(*this);
    return replacementType;
  }

  // Not known.
  return Type();
}

void SubstitutionMap::
addSubstitution(CanGenericTypeParamType type, Type replacement) {
  assert(getGenericSignature() &&
         "cannot add entries to empty substitution map");

  auto replacementTypes = getReplacementTypes();
  auto genericParams = getGenericSignature()->getGenericParams();
  auto replacementIndex = GenericParamKey(type).findIndexIn(genericParams);

  assert((!replacementTypes[replacementIndex] ||
          replacementTypes[replacementIndex]->isEqual(replacement)));

  replacementTypes[replacementIndex] = replacement;
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(CanType type, ProtocolDecl *proto) const {
  // If we have an archetype, map out of the context so we can compute a
  // conformance access path.
  if (auto archetype = dyn_cast<ArchetypeType>(type)) {
    type = archetype->getInterfaceType()->getCanonicalType();
  }

  // Error path: if we don't have a type parameter, there is no conformance.
  // FIXME: Query concrete conformances in the generic signature?
  if (!type->isTypeParameter())
    return None;

  // Retrieve the starting conformance from the conformance map.
  auto getInitialConformance =
    [&](Type type, ProtocolDecl *proto) -> Optional<ProtocolConformanceRef> {
      auto known = conformanceMap.find(type->getCanonicalType());
      if (known == conformanceMap.end())
        return None;

      for (auto conformance : known->second) {
        if (conformance.getRequirement() == proto)
          return conformance;
      }

      return None;
    };

  auto genericSig = getGenericSignature();

  // If the type doesn't conform to this protocol, the result isn't formed
  // from these requirements.
  if (!genericSig->conformsToProtocol(type, proto)) {
    // Check whether the superclass conforms.
    if (auto superclass = genericSig->getSuperclassBound(type)) {
      return LookUpConformanceInSignature(*getGenericSignature())(
                                                 type->getCanonicalType(),
                                                 superclass,
                                                 proto->getDeclaredType());
    }

    return None;
  }

  auto accessPath =
    genericSig->getConformanceAccessPath(type, proto);

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
    if (conformance->isAbstract()) {
      // FIXME: Rip this out once we can get a concrete conformance from
      // an archetype.
      auto *M = proto->getParentModule();
      auto substType = type.subst(*this);
      if (substType &&
          (!substType->is<ArchetypeType>() ||
           substType->castTo<ArchetypeType>()->getSuperclass()) &&
          !substType->isTypeParameter() &&
          !substType->isExistentialType()) {
        return M->lookupConformance(substType, proto);
      }

      return ProtocolConformanceRef(proto);
    }

    // For the second step, we're looking into the requirement signature for
    // this protocol.
    auto concrete = conformance->getConcrete();
    auto normal = concrete->getRootNormalConformance();

    // If we haven't set the signature conformances yet, force the issue now.
    if (normal->getSignatureConformances().empty()) {
      // If we're in the process of checking the type witnesses, fail
      // gracefully.
      // FIXME: Seems like we should be able to get at the intermediate state
      // to use that.
      if (normal->getState() == ProtocolConformanceState::CheckingTypeWitnesses)
        return None;

      auto lazyResolver = type->getASTContext().getLazyResolver();
      if (lazyResolver == nullptr)
        return None;

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
  assert(!isa<ArchetypeType>(type));
  conformanceMap[type].push_back(conformance);
}

SubstitutionMap SubstitutionMap::mapReplacementTypesOutOfContext() const {
  return subst(MapTypeOutOfContext(), MakeAbstractConformanceForGenericType());
}

SubstitutionMap SubstitutionMap::subst(const SubstitutionMap &subMap) const {
  return subst(QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

SubstitutionMap SubstitutionMap::subst(TypeSubstitutionFn subs,
                                       LookupConformanceFn conformances) const {
  SubstitutionMap result(*this);

  for (auto &replacementType : result.getReplacementTypes()) {
    if (replacementType) {
      replacementType = replacementType.subst(subs, conformances,
                                              SubstFlags::UseErrorType);
    }
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
                                          Optional<SubstitutionMap> derivedSubs) {
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
                                  derivedSubs);
}

SubstitutionMap
SubstitutionMap::getOverrideSubstitutions(const ClassDecl *baseClass,
                                          const ClassDecl *derivedClass,
                                          GenericSignature *baseSig,
                                          GenericSignature *derivedSig,
                                          Optional<SubstitutionMap> derivedSubs) {
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
    auto baseClassTy = derivedClassTy->getSuperclassForDecl(baseClass);

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
        assert(proto->isObjC() &&
               "an existential type can conform only to an "
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
  auto *genericSig = getGenericSignature();
  if (genericSig == nullptr) {
    out << "Empty substitution map\n";
    return;
  }
  out << "Generic signature: ";
  genericSig->print(out);
  out << "\n";
  out << "Substitutions:\n";
  auto genericParams = genericSig->getGenericParams();
  auto replacementTypes = getReplacementTypes();
  for (unsigned i : indices(genericParams)) {
    out.indent(2);
    genericParams[i]->print(out);
    out << " -> ";
    if (replacementTypes[i])
      replacementTypes[i]->print(out);
    else
      out << "<<unresolved concrete type>>";
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

void SubstitutionMap::profile(llvm::FoldingSetNodeID &id) const {
  // Generic signature.
  id.AddPointer(genericSig);

  if (empty() || !genericSig) return;

  // Replacement types.
  for (Type gp : genericSig->getGenericParams()) {
    id.AddPointer(gp.subst(*this).getPointer());
  }

  // Conformance requirements.
  for (const auto &req : genericSig->getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    auto conformance =
      lookupConformance(req.getFirstType()->getCanonicalType(),
                        req.getSecondType()->castTo<ProtocolType>()->getDecl());
    id.AddPointer(conformance ? conformance->getOpaqueValue() : nullptr);
  }
}

