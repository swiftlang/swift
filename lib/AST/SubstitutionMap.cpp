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
// together a set of replacement types and protocol conformances, given by
// the generic parameters and conformance requirements of the substitution map's
// input generic signature.
//
// To substitute a type, call Type::subst() with the right SubstitutionMap.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/SubstitutionMap.h"
#include "SubstitutionMapStorage.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericParamList.h"
#include "swift/AST/InFlightSubstitution.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Module.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "llvm/Support/Debug.h"

using namespace swift;

SubstitutionMap::Storage::Storage(
                              GenericSignature genericSig,
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
                                GenericSignature genericSig,
                                ArrayRef<Type> replacementTypes,
                                ArrayRef<ProtocolConformanceRef> conformances)
  : storage(Storage::get(genericSig, replacementTypes, conformances)) {
  if (genericSig->getASTContext().LangOpts.VerifyAllSubstitutionMaps)
    verify();
}

ArrayRef<ProtocolConformanceRef> SubstitutionMap::getConformances() const {
  return storage ? storage->getConformances()
                 : ArrayRef<ProtocolConformanceRef>();
}

ArrayRef<Type> SubstitutionMap::getReplacementTypes() const {
  if (empty()) return { };

  return storage->getReplacementTypes();
}

ArrayRef<Type> SubstitutionMap::getInnermostReplacementTypes() const {
  if (empty()) return { };

  return getReplacementTypes().take_back(
      getGenericSignature().getInnermostGenericParams().size());
}

GenericSignature SubstitutionMap::getGenericSignature() const {
  return storage ? storage->getGenericSignature() : nullptr;
}

bool SubstitutionMap::empty() const {
  return getGenericSignature().isNull();
}

bool SubstitutionMap::hasAnySubstitutableParams() const {
  auto genericSig = getGenericSignature();
  if (!genericSig) return false;

  return !genericSig->areAllParamsConcrete();
}

RecursiveTypeProperties SubstitutionMap::getRecursiveProperties() const {
  RecursiveTypeProperties properties;
  for (auto replacementTy : getReplacementTypes())
    properties |= replacementTy->getRecursiveProperties();
  return properties;
}

bool SubstitutionMap::isCanonical() const {
  if (empty()) return true;

  if (!getGenericSignature()->isCanonical()) return false;

  for (Type replacementTy : getReplacementTypes()) {
    if (!replacementTy->isCanonical())
      return false;
  }

  for (auto conf : getConformances()) {
    if (!conf.isCanonical())
      return false;
  }

  return true;
}

SubstitutionMap SubstitutionMap::getCanonical(bool canonicalizeSignature) const {
  if (empty()) return *this;

  auto sig = getGenericSignature();
  if (canonicalizeSignature) sig = sig.getCanonicalSignature();

  SmallVector<Type, 4> replacementTypes;
  for (Type replacementType : getReplacementTypes()) {
    replacementTypes.push_back(replacementType->getCanonicalType());
  }

  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (auto conf : getConformances()) {
    conformances.push_back(conf.getCanonicalConformanceRef());
  }

  return SubstitutionMap::get(sig,
                              ArrayRef<Type>(replacementTypes),
                              ArrayRef<ProtocolConformanceRef>(conformances));
}

SubstitutionMap SubstitutionMap::get(GenericSignature genericSig,
                                     SubstitutionMap substitutions) {
  if (!genericSig)
    return SubstitutionMap();

  return SubstitutionMap::get(genericSig,
           QuerySubstitutionMap{substitutions},
           LookUpConformanceInSubstitutionMap(substitutions));
}

SubstitutionMap SubstitutionMap::get(GenericSignature genericSig,
                                     TypeSubstitutionFn subs,
                                     LookupConformanceFn lookupConformance) {
  InFlightSubstitution IFS(subs, lookupConformance, std::nullopt);
  return get(genericSig, IFS);
}

SubstitutionMap SubstitutionMap::get(GenericSignature genericSig,
                                     ArrayRef<Type> types,
                                     LookupConformanceFn lookupConformance) {
  QueryReplacementTypeArray subs{genericSig, types};
  InFlightSubstitution IFS(subs, lookupConformance, std::nullopt);
  return get(genericSig, types, IFS);
}

SubstitutionMap SubstitutionMap::get(GenericSignature genericSig,
                                     ArrayRef<Type> types,
                                     InFlightSubstitution &IFS) {
  // Form the stored conformances.
  SmallVector<ProtocolConformanceRef, 4> conformances;
  for (const auto &req : genericSig.getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    conformances.push_back(
      IFS.lookupConformance(
        req.getFirstType(), req.getProtocolDecl(), /*level=*/0));
  }

  return SubstitutionMap(genericSig, types, conformances);
}

SubstitutionMap SubstitutionMap::get(GenericSignature genericSig,
                                     InFlightSubstitution &IFS) {
  if (!genericSig) {
    return SubstitutionMap();
  }

  // Form the replacement types.
  SmallVector<Type, 4> replacementTypes;
  replacementTypes.reserve(genericSig.getGenericParams().size());

  for (auto *gp : genericSig.getGenericParams()) {
    // Record the replacement.
    Type replacement = IFS.substType(gp, /*level=*/0);
    if (!replacement)
      replacement = ErrorType::get(gp->getASTContext());
    assert((replacement->hasError() ||
            gp->isParameterPack() == replacement->is<PackType>()) &&
           "replacement for pack parameter must be a pack type");

    replacementTypes.push_back(replacement);
  }

  return SubstitutionMap::get(genericSig, replacementTypes, IFS);
}

Type SubstitutionMap::lookupSubstitution(GenericTypeParamType *genericParam) const {
  if (empty())
    return Type();

  // Find the index of the replacement type based on the generic parameter we
  // have.
  GenericSignature genericSig = getGenericSignature();
  auto genericParams = genericSig.getGenericParams();
  auto replacementIndex =
    GenericParamKey(genericParam).findIndexIn(genericParams);

  // If this generic parameter isn't represented, we don't have a replacement
  // type for it.
  if (replacementIndex == genericParams.size())
    return Type();

  return getReplacementTypes()[replacementIndex];
}

ProtocolConformanceRef
SubstitutionMap::lookupConformance(CanType type, ProtocolDecl *proto) const {
  if (!type->isTypeParameter() || empty())
    return ProtocolConformanceRef::forInvalid();

  auto genericSig = getGenericSignature();

  auto getSignatureConformance =
      [&](Type type,
          ProtocolDecl *proto) -> std::optional<ProtocolConformanceRef> {
    unsigned index = 0;
    for (auto reqt : genericSig.getRequirements()) {
      if (reqt.getKind() == RequirementKind::Conformance) {
        if (reqt.getFirstType()->isEqual(type) &&
            reqt.getProtocolDecl() == proto)
          return getConformances()[index];

        ++index;
      }
    }

    return std::nullopt;
  };

  // Fast path -- check if the generic signature directly states the
  // conformance.
  if (auto directConformance = getSignatureConformance(type, proto))
    return *directConformance;

  // If the type doesn't conform to this protocol, the result isn't formed
  // from these requirements.
  if (!genericSig->requiresProtocol(type, proto)) {
    Type substType = type.subst(*this);
    return ProtocolConformanceRef::forMissingOrInvalid(substType, proto);
  }

  // If the protocol is invertible, fall back to a global lookup instead of
  // evaluating a conformance path, to avoid an infinite substitution issue.
  if (proto->getInvertibleProtocolKind())
    return swift::lookupConformance(type.subst(*this), proto);

  auto path = genericSig->getConformancePath(type, proto);

  // For the first step, grab the initial conformance.
  auto iter = path.begin();
  const auto step = *iter++;

  ProtocolConformanceRef conformance =
      *getSignatureConformance(step.first, step.second);

  // For each remaining step, project an associated conformance.
  while (iter != path.end()) {
    // FIXME: Remove this hack. It is unsound, because we may not have diagnosed
    // anything but still end up with an ErrorType in the AST.
    if (conformance.isConcrete()) {
      auto concrete = conformance.getConcrete();
      if (auto normal = dyn_cast<NormalProtocolConformance>(concrete->getRootConformance())) {
        if (!normal->hasComputedAssociatedConformances()) {
          if (proto->getASTContext().evaluator.hasActiveRequest(
                ResolveTypeWitnessesRequest{normal})) {
            return ProtocolConformanceRef::forInvalid();
          }
        }
      }
    }

    const auto step = *iter++;
    conformance = conformance.getAssociatedConformance(step.first, step.second);
  }

  return conformance;
}

SubstitutionMap SubstitutionMap::mapReplacementTypesOutOfContext() const {
  return subst(MapTypeOutOfContext(),
               LookUpConformanceInModule(),
               SubstFlags::PreservePackExpansionLevel |
               SubstFlags::SubstitutePrimaryArchetypes);
}

SubstitutionMap SubstitutionMap::subst(SubstitutionMap subMap,
                                       SubstOptions options) const {
  InFlightSubstitutionViaSubMap IFS(subMap, options);
  return subst(IFS);
}

SubstitutionMap SubstitutionMap::subst(TypeSubstitutionFn subs,
                                       LookupConformanceFn conformances,
                                       SubstOptions options) const {
  InFlightSubstitution IFS(subs, conformances, options);
  return subst(IFS);
}

SubstitutionMap SubstitutionMap::subst(InFlightSubstitution &IFS) const {
  if (empty()) return SubstitutionMap();

  SmallVector<Type, 4> newSubs;
  for (Type type : getReplacementTypes()) {
    newSubs.push_back(type.subst(IFS));
    assert(type->is<PackType>() == newSubs.back()->is<PackType>() &&
           "substitution changed the pack-ness of a replacement type");
  }

  SmallVector<ProtocolConformanceRef, 4> newConformances;
  auto oldConformances = getConformances();

  auto genericSig = getGenericSignature();
  for (const auto &req : genericSig.getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    newConformances.push_back(oldConformances[0].subst(IFS));
    oldConformances = oldConformances.slice(1);
  }

  assert(oldConformances.empty());
  return SubstitutionMap(genericSig, newSubs, newConformances);
}

SubstitutionMap
SubstitutionMap::getProtocolSubstitutions(ProtocolConformanceRef conformance) {
  return getProtocolSubstitutions(conformance.getProtocol(),
                                  conformance.getType(),
                                  conformance);
}

SubstitutionMap
SubstitutionMap::getProtocolSubstitutions(ProtocolDecl *protocol,
                                          Type selfType,
                                          ProtocolConformanceRef conformance) {
  return get(protocol->getGenericSignature(), llvm::ArrayRef<Type>(selfType),
             llvm::ArrayRef<ProtocolConformanceRef>(conformance));
}

SubstitutionMap
SubstitutionMap::getOverrideSubstitutions(
                                      const ValueDecl *baseDecl,
                                      const ValueDecl *derivedDecl) {
  // For overrides within a protocol hierarchy, substitute the Self type.
  if (baseDecl->getDeclContext()->getSelfProtocolDecl()) {
    auto baseSig = baseDecl->getInnermostDeclContext()
        ->getGenericSignatureOfContext();
    return baseSig->getIdentitySubstitutionMap();
  }

  auto *baseClass = baseDecl->getDeclContext()->getSelfClassDecl();
  auto *derivedClass = derivedDecl->getDeclContext()->getSelfClassDecl();

  auto baseSig = baseDecl->getInnermostDeclContext()
      ->getGenericSignatureOfContext();

  // If more kinds of overridable decls with generic parameter lists appear,
  // add them here.
  GenericParamList *derivedParams = nullptr;
  if (auto *funcDecl = dyn_cast<AbstractFunctionDecl>(derivedDecl))
    derivedParams = funcDecl->getGenericParams();
  else if (auto *subscriptDecl = dyn_cast<SubscriptDecl>(derivedDecl))
    derivedParams = subscriptDecl->getGenericParams();

  return getOverrideSubstitutions(baseClass, derivedClass, baseSig, derivedParams);
}

OverrideSubsInfo::OverrideSubsInfo(const NominalTypeDecl *baseNominal,
                                   const NominalTypeDecl *derivedNominal,
                                   GenericSignature baseSig,
                                   const GenericParamList *derivedParams)
  : BaseDepth(0),
    OrigDepth(0),
    DerivedParams(derivedParams) {

  if (auto baseNominalSig = baseNominal->getGenericSignature()) {
    BaseDepth = baseNominalSig.getNextDepth();

    auto *genericEnv = derivedNominal->getGenericEnvironment();
    auto derivedNominalTy = derivedNominal->getDeclaredInterfaceType();

    // FIXME: Map in and out of context to get more accurate
    // conformance information. If the base generic signature
    // is <T: P> and the derived generic signature is <T: C>
    // where C is a class that conforms to P, then we want the
    // substitution map to store the concrete conformance C: P
    // and not the abstract conformance T: P.
    if (genericEnv) {
      derivedNominalTy = genericEnv->mapTypeIntoContext(
          derivedNominalTy);
    }

    BaseSubMap = derivedNominalTy->getContextSubstitutionMap(
        baseNominal, genericEnv);

    BaseSubMap = BaseSubMap.mapReplacementTypesOutOfContext();
  }

  if (auto derivedNominalSig = derivedNominal->getGenericSignature())
    OrigDepth = derivedNominalSig.getNextDepth();
}

Type QueryOverrideSubs::operator()(SubstitutableType *type) const {
  if (auto gp = type->getAs<GenericTypeParamType>()) {
    if (gp->getDepth() >= info.BaseDepth) {
      assert(gp->getDepth() == info.BaseDepth);
      if (info.DerivedParams != nullptr) {
        return info.DerivedParams->getParams()[gp->getIndex()]
            ->getDeclaredInterfaceType();
      }

      return gp->withDepth(gp->getDepth() + info.OrigDepth - info.BaseDepth);
    }
  }

  return Type(type).subst(info.BaseSubMap);
}

ProtocolConformanceRef
LookUpConformanceInOverrideSubs::operator()(InFlightSubstitution &IFS,
                                            Type type,
                                            ProtocolDecl *proto) const {
  if (type->getRootGenericParam()->getDepth() >= info.BaseDepth)
    return ProtocolConformanceRef::forAbstract(type.subst(IFS), proto);

  if (auto conformance = info.BaseSubMap.lookupConformance(
        type->getCanonicalType(), proto)) {
    return conformance;
  }

  return lookupConformance(type.subst(IFS), proto);
}

SubstitutionMap
SubstitutionMap::getOverrideSubstitutions(const NominalTypeDecl *baseNominal,
                                          const NominalTypeDecl *derivedNominal,
                                          GenericSignature baseSig,
                                          const GenericParamList *derivedParams) {
  if (baseSig.isNull())
    return SubstitutionMap();

  OverrideSubsInfo info(baseNominal, derivedNominal, baseSig, derivedParams);

  return get(baseSig,
             QueryOverrideSubs(info),
             LookUpConformanceInOverrideSubs(info));
}

void SubstitutionMap::verify(bool allowInvalid) const {
  if (empty())
    return;

  auto genericSig = getGenericSignature();
  auto &ctx = genericSig->getASTContext();

  if (ctx.isRecursivelyConstructingRequirementMachine(
        genericSig.getCanonicalSignature()))
    return;

  unsigned conformanceIndex = 0;

  for (const auto &req : genericSig.getRequirements()) {
    if (req.getKind() != RequirementKind::Conformance)
      continue;

    SWIFT_DEFER { ++conformanceIndex; };
    auto conformance = getConformances()[conformanceIndex];

    auto substType = req.getFirstType().subst(*this);

    // Unwrap various strange things.
    substType = substType->getReferenceStorageReferent();
    if (auto *selfType = substType->getAs<DynamicSelfType>())
      substType = selfType->getSelfType();

    // Don't bother validating these cases.
    if (allowInvalid && substType->hasUnboundGenericType())
      return;

    if (conformance.isInvalid()) {
      if (!allowInvalid) {
        ABORT([&](auto &out) {
          out << "Unexpected invalid conformance in substitution map:\n";
          dump(out);
        });
      }

      continue;
    }

    if (conformance.isAbstract()) {
      if (!substType->isTypeParameter() &&
          !substType->is<PackElementType>() &&
          !substType->is<ArchetypeType>() &&
          !substType->isTypeVariableOrMember() &&
          !substType->is<UnresolvedType>() &&
          !substType->is<PlaceholderType>() &&
          !substType->is<ErrorType>()) {
        ABORT([&](auto &out) {
          out << "Unexpected abstract conformance in substitution map:\n";
          dump(out);
        });
      }

      continue;
    }

    if (conformance.isPack()) {
      // FIXME: Implement some kind of check here.
      continue;
    }

    auto *concrete = conformance.getConcrete();

    if (substType->isExistentialType()) {
      if (req.getProtocolDecl()->isSpecificProtocol(KnownProtocolKind::Sendable) &&
          isa<BuiltinProtocolConformance>(concrete)) {
        continue;
      }

      if (!isa<SelfProtocolConformance>(concrete)) {
        // A superclass-constrained self-conforming existential might conform
        // concretely.
        if (substType->getSuperclass())
          continue;

        ABORT([&](auto &out) {
          out << "Expected to find a self conformance:\n";
          substType->dump(out);
          out << "Substitution map:\n";
          dump(out);
        });
      }

      continue;
    }

    if (substType->isTypeParameter())
      continue;

    if (!concrete->getType()->isEqual(substType)) {
      ABORT([&](auto &out) {
        out << "Conformance with wrong conforming type:\n";
        concrete->getType()->dump(out);
        out << "Should be:\n";
        substType->dump(out);
        out << "Substitution map:\n";
        dump(out);
      });
    }
  }
}

void SubstitutionMap::profile(llvm::FoldingSetNodeID &id) const {
  id.AddPointer(storage);
}

bool SubstitutionMap::isIdentity() const {
  if (empty())
    return true;

  for (auto conf : getConformances()) {
    if (conf.isAbstract())
      continue;

    if (conf.isPack()) {
      auto patternConfs = conf.getPack()->getPatternConformances();
      if (patternConfs.size() == 1 && patternConfs[0].isAbstract())
        continue;
    }

    return false;
  }

  GenericSignature sig = getGenericSignature();
  bool hasNonIdentityReplacement = false;
  auto replacements = getReplacementTypes();

  sig->forEachParam([&](GenericTypeParamType *paramTy, bool isCanonical) {
    if (isCanonical) {
      Type wrappedParamTy = paramTy;
      if (paramTy->isParameterPack())
        wrappedParamTy = PackType::getSingletonPackExpansion(paramTy);
      if (!wrappedParamTy->isEqual(replacements[0]))
        hasNonIdentityReplacement = true;
    }

    replacements = replacements.slice(1);
  });

  assert(replacements.empty());

  return !hasNonIdentityReplacement;
}

SubstitutionMap swift::substOpaqueTypesWithUnderlyingTypes(
    SubstitutionMap subs, TypeExpansionContext context) {
  ReplaceOpaqueTypesWithUnderlyingTypes replacer(
      context.getContext(), context.getResilienceExpansion(),
      context.isWholeModuleContext());
  InFlightSubstitution IFS(replacer, replacer,
                           SubstFlags::SubstituteOpaqueArchetypes |
                           SubstFlags::PreservePackExpansionLevel);

  auto substSubs = subs.subst(IFS);

  if (IFS.wasLimitReached()) {
    ABORT([&](auto &out) {
      out << "Possible non-terminating type substitution detected\n\n";
      out << "Original substitution map:\n";
      subs.dump(out, SubstitutionMap::DumpStyle::NoConformances);
      out << "Substituted substitution map:\n";
      substSubs.dump(out, SubstitutionMap::DumpStyle::NoConformances);
    });
  }

  return substSubs;
}

Type OuterSubstitutions::operator()(SubstitutableType *type) const {
  if (cast<GenericTypeParamType>(type)->getDepth() >= depth)
    return Type(type);

  return QuerySubstitutionMap{subs}(type);
}

ProtocolConformanceRef OuterSubstitutions::operator()(
                                        InFlightSubstitution &IFS,
                                        Type dependentType,
                                        ProtocolDecl *conformedProtocol) const {
  auto sig = subs.getGenericSignature();
  if (!sig->isValidTypeParameter(dependentType) ||
      !sig->requiresProtocol(dependentType, conformedProtocol)) {
    // FIXME: We need the isValidTypeParameter() check instead of just looking
    // at the root generic parameter because in the case of an existential
    // environment, the reduced type of a member type of Self might be an outer
    // type parameter that is not formed from the outer generic signature's
    // conformance requirements. Ideally, we'd either add these supplementary
    // conformance requirements to the generalization signature, or we would
    // store the supplementary conformances directly in the generic environment
    // somehow.
    //
    // Once we check for that and handle it properly, the lookupConformance()
    // can become a forAbstract().
    return swift::lookupConformance(
      dependentType.subst(IFS), conformedProtocol);
  }

  return LookUpConformanceInSubstitutionMap(subs)(
      IFS, dependentType, conformedProtocol);
}

