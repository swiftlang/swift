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
#include "SubstitutionMapStorage.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/Support/Debug.h"

using namespace swift;

SubstitutionMap::Storage::Storage(
                              GenericSignature *genericSig,
                              ArrayRef<Type> replacementTypes,
                              ArrayRef<ProtocolConformanceRef> conformances)
  : genericSig(genericSig),
    numConformanceRequirements(genericSig->getNumConformanceRequirements())
{
  assert(replacementTypes.size() == getNumReplacementTypes());
  assert(conformances.size() == numConformanceRequirements);

  std::copy(replacementTypes.begin(), replacementTypes.end(),
            getReplacementTypes().data());
  std::copy(conformances.begin(), conformances.end(),
            getConformances().data());
}

SubstitutionMap::SubstitutionMap(
                                GenericSignature *genericSig,
                                ArrayRef<Type> replacementTypes,
                                ArrayRef<ProtocolConformanceRef> conformances)
  : storage(Storage::get(genericSig, replacementTypes, conformances)) { }

ArrayRef<Type> SubstitutionMap::getReplacementTypesBuffer() const {
  return storage ? storage->getReplacementTypes() : ArrayRef<Type>();
}

MutableArrayRef<Type> SubstitutionMap::getReplacementTypesBuffer() {
  return storage ? storage->getReplacementTypes() : MutableArrayRef<Type>();
}

MutableArrayRef<ProtocolConformanceRef>
SubstitutionMap::getConformancesBuffer() {
  return storage ? storage->getConformances()
                 : MutableArrayRef<ProtocolConformanceRef>();
}

ArrayRef<ProtocolConformanceRef> SubstitutionMap::getConformances() const {
  return storage ? storage->getConformances()
                 : ArrayRef<ProtocolConformanceRef>();
}

ArrayRef<Type> SubstitutionMap::getReplacementTypes() const {
  if (empty()) return { };

  // Make sure we've filled in all of the replacement types.
  for (auto gp : getGenericSignature()->getGenericParams()) {
    (void)Type(gp).subst(*this);
  }

  return getReplacementTypesBuffer();
}

GenericSignature *SubstitutionMap::getGenericSignature() const {
  return storage ? storage->getGenericSignature() : nullptr;
}

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

bool SubstitutionMap::isCanonical() const {
  if (empty()) return true;

  if (!getGenericSignature()->isCanonical()) return false;

  for (Type replacementTy : getReplacementTypes()) {
    if (replacementTy && !replacementTy->isCanonical())
      return false;
  }

  for (auto conf : getConformances()) {
    if (!conf.isCanonical())
      return false;
  }

  return true;
}

SubstitutionMap SubstitutionMap::getCanonical() const {
  if (empty()) return *this;

  auto canonicalSig = getGenericSignature()->getCanonicalSignature();
  SmallVector<Type, 4> replacementTypes;
  for (Type replacementType : getReplacementTypes()) {
    if (replacementType)
      replacementTypes.push_back(replacementType->getCanonicalType());
    else
      replacementTypes.push_back(nullptr);
  }

  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto conf : getConformances()) {
    conformances.push_back(conf.getCanonicalConformanceRef());
  }

  return SubstitutionMap::get(canonicalSig,
                              ArrayRef<Type>(replacementTypes),
                              ArrayRef<ProtocolConformanceRef>(conformances));
}


SubstitutionMap SubstitutionMap::get(GenericSignature *genericSig,
                                     SubstitutionList substitutions) {
  if (!genericSig) {
    assert(substitutions.empty() && "Shouldn't have substitutions here");
    return SubstitutionMap();
  }

  SmallVector<Type, 4> replacementTypes(genericSig->getGenericParams().size(),
                                        Type());
  SmallVector<ProtocolConformanceRef, 4> storedConformances;

  genericSig->enumeratePairedRequirements(
    [&](Type depTy, ArrayRef<Requirement> reqts) -> bool {
      auto sub = substitutions.front();
      substitutions = substitutions.slice(1);

      auto canTy = depTy->getCanonicalType();
      if (auto paramTy = dyn_cast<GenericTypeParamType>(canTy)) {
        replacementTypes[genericSig->getGenericParamOrdinal(paramTy)] =
          sub.getReplacement();
      }

      auto conformances = sub.getConformances();
      assert(reqts.size() == conformances.size());

      for (auto i : indices(conformances)) {
        assert(reqts[i].getSecondType()->getAnyNominal() ==
               conformances[i].getRequirement());
        storedConformances.push_back(conformances[i]);
      }

      return false;
    });

  assert(substitutions.empty() && "did not use all substitutions?!");
  return SubstitutionMap(genericSig, replacementTypes, storedConformances);
}

/// Build an interface type substitution map for the given generic signature
/// from a type substitution function and conformance lookup function.
SubstitutionMap SubstitutionMap::get(GenericSignature *genericSig,
                                     TypeSubstitutionFn subs,
                                     LookupConformanceFn lookupConformance) {
  if (!genericSig) {
    return SubstitutionMap();
  }

  SmallVector<Type, 4> replacementTypes(genericSig->getGenericParams().size(),
                                        Type());
  SmallVector<ProtocolConformanceRef, 4> storedConformances;

  // Enumerate all of the requirements that require substitution.
  genericSig->enumeratePairedRequirements(
    [&](Type depTy, ArrayRef<Requirement> reqs) {
      auto canTy = depTy->getCanonicalType();

      // Compute the replacement type.
      Type currentReplacement = depTy.subst(subs, lookupConformance,
                                            SubstFlags::UseErrorType);
      if (auto paramTy = dyn_cast<GenericTypeParamType>(canTy)) {
        replacementTypes[genericSig->getGenericParamOrdinal(paramTy)] =
          currentReplacement;
      }

      // Collect the conformances.
      for (auto req: reqs) {
        assert(req.getKind() == RequirementKind::Conformance);
        auto protoType = req.getSecondType()->castTo<ProtocolType>();
        auto conformance =
          lookupConformance(canTy, currentReplacement, protoType)
            .getValueOr(ProtocolConformanceRef(protoType->getDecl()));
        storedConformances.push_back(conformance);
      }

      return false;
    });

  return SubstitutionMap(genericSig, replacementTypes, storedConformances);
}

Type SubstitutionMap::lookupSubstitution(CanSubstitutableType type) const {
  if (empty())
    return Type();

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
  auto replacementTypes = mutableThis->getReplacementTypesBuffer();
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

  // The generic parameter may not be canonical. Retrieve the canonical
  // type, which will be dependent.
  CanType canonicalType = genericSig->getCanonicalTypeInContext(genericParam);

  // If nothing changed, we don't have a replacement.
  if (canonicalType == type) return Type();

  // If we're left with a substitutable type, substitute into that.
  // First, set the replacement type to an error, to block infinite recursion.
  replacementType = ErrorType::get(type);

  replacementType = lookupSubstitution(cast<SubstitutableType>(canonicalType));
  return replacementType;
}

Optional<ProtocolConformanceRef>
SubstitutionMap::lookupConformance(CanType type, ProtocolDecl *proto) const {
  if (empty()) return None;

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
      unsigned conformanceIndex = 0;
      for (const auto &req : getGenericSignature()->getRequirements()) {
        if (req.getKind() != RequirementKind::Conformance)
          continue;

        // Is this the conformance we're looking for?
        if (req.getFirstType()->isEqual(type) &&
            req.getSecondType()->castTo<ProtocolType>()->getDecl() == proto) {
          return getConformances()[conformanceIndex];
        }

        ++conformanceIndex;
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

SubstitutionMap SubstitutionMap::mapReplacementTypesOutOfContext() const {
  return subst(MapTypeOutOfContext(), MakeAbstractConformanceForGenericType());
}

SubstitutionMap SubstitutionMap::subst(const SubstitutionMap &subMap) const {
  return subst(QuerySubstitutionMap{subMap},
               LookUpConformanceInSubstitutionMap(subMap));
}

SubstitutionMap SubstitutionMap::subst(TypeSubstitutionFn subs,
                                       LookupConformanceFn conformances) const {
  if (empty()) return SubstitutionMap();

  return getGenericSignature()->getSubstitutionMap(
    [&](SubstitutableType *type) {
      return Type(type).subst(*this, SubstFlags::UseErrorType)
               .subst(subs, conformances, SubstFlags::UseErrorType);
    },
    [&](CanType dependentType, Type replacementType,
        ProtocolType *conformedProtocol) ->Optional<ProtocolConformanceRef> {
      auto proto = conformedProtocol->getDecl();
      auto conformance =
        lookupConformance(dependentType, proto)
          .getValueOr(ProtocolConformanceRef(proto));
      auto substType = dependentType.subst(*this, SubstFlags::UseErrorType);
      return conformance.subst(substType, subs, conformances);
    });
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
  auto replacementTypes = getReplacementTypesBuffer();
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
  auto conformances = getConformances();
  for (const auto &req : genericSig->getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance) continue;

    out.indent(2);
    req.getFirstType()->print(out);
    out << " -> ";
    conformances.front().dump(out);
    out << "\n";

    conformances = conformances.slice(1);
  }
}

void SubstitutionMap::dump() const {
  return dump(llvm::errs());
}

void SubstitutionMap::profile(llvm::FoldingSetNodeID &id) const {
  id.AddPointer(storage);
}

SubstitutionList SubstitutionMap::toList() const {
  if (empty()) return { };

  // If we don't yet have cached flat substitutions, cache them now.
  if (storage->flatSubstitutions.empty()) {
    SmallVector<Substitution, 4> subs;
    getGenericSignature()->getSubstitutions(*this, subs);

    auto &ctx = getGenericSignature()->getASTContext();
    storage->flatSubstitutions = ctx.AllocateCopy(subs);
  }

  return storage->flatSubstitutions;
}

