//===--- GenericSignature.cpp - Generic Signature AST ---------------------===//
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
// This file implements the GenericSignature class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Module.h"
#include "swift/AST/Types.h"

using namespace swift;

GenericSignature::GenericSignature(ArrayRef<GenericTypeParamType *> params,
                                   ArrayRef<Requirement> requirements,
                                   bool isKnownCanonical)
  : NumGenericParams(params.size()), NumRequirements(requirements.size()),
    CanonicalSignatureOrASTContext()
{
  auto paramsBuffer = getGenericParamsBuffer();
  for (unsigned i = 0; i < NumGenericParams; ++i) {
    paramsBuffer[i] = params[i];
  }

  auto reqtsBuffer = getRequirementsBuffer();
  for (unsigned i = 0; i < NumRequirements; ++i) {
    reqtsBuffer[i] = requirements[i];
  }

  if (isKnownCanonical)
    CanonicalSignatureOrASTContext = &getASTContext(params, requirements);
}

ArrayRef<GenericTypeParamType *> 
GenericSignature::getInnermostGenericParams() const {
  auto params = getGenericParams();

  // Find the point at which the depth changes.
  unsigned depth = params.back()->getDepth();
  for (unsigned n = params.size(); n > 0; --n) {
    if (params[n-1]->getDepth() != depth) {
      return params.slice(n);
    }
  }

  // All parameters are at the same depth.
  return params;
}

ASTContext &GenericSignature::getASTContext(
                                ArrayRef<swift::GenericTypeParamType *> params,
                                ArrayRef<swift::Requirement> requirements) {
  // The params and requirements cannot both be empty.
  if (!params.empty())
    return params.front()->getASTContext();
  else
    return requirements.front().getFirstType()->getASTContext();
}

GenericEnvironment *
GenericSignature::getCanonicalGenericEnvironment(ModuleDecl &mod) {
  // The generic environment is associated with the canonical signature.
  if (!isCanonical())
    return getCanonicalSignature()->getCanonicalGenericEnvironment(mod);

  // Archetype builders are stored on the ASTContext.
  return getASTContext().getCanonicalGenericEnvironment(
      CanGenericSignature(this), &mod);
}

bool GenericSignature::isCanonical() const {
  if (CanonicalSignatureOrASTContext.is<ASTContext*>()) return true;

  return getCanonicalSignature() == this;
}

CanGenericSignature GenericSignature::getCanonical(
                                        ArrayRef<GenericTypeParamType *> params,
                                        ArrayRef<Requirement> requirements) {
  // Canonicalize the parameters and requirements.
  SmallVector<GenericTypeParamType*, 8> canonicalParams;
  canonicalParams.reserve(params.size());
  for (auto param : params) {
    canonicalParams.push_back(cast<GenericTypeParamType>(param->getCanonicalType()));
  }

  SmallVector<Requirement, 8> canonicalRequirements;
  canonicalRequirements.reserve(requirements.size());
  for (auto &reqt : requirements) {
    canonicalRequirements.push_back(Requirement(reqt.getKind(),
                              reqt.getFirstType()->getCanonicalType(),
                              reqt.getSecondType().getCanonicalTypeOrNull()));
  }
  auto canSig = get(canonicalParams, canonicalRequirements,
                    /*isKnownCanonical=*/true);
  return CanGenericSignature(canSig);
}

CanGenericSignature
GenericSignature::getCanonicalSignature() const {
  // If we haven't computed the canonical signature yet, do so now.
  if (CanonicalSignatureOrASTContext.isNull()) {
    // Compute the canonical signature.
    CanGenericSignature canSig = getCanonical(getGenericParams(),
                                              getRequirements());

    // Record either the canonical signature or an indication that
    // this is the canonical signature.
    if (canSig != this)
      CanonicalSignatureOrASTContext = canSig;
    else
      CanonicalSignatureOrASTContext = &getGenericParams()[0]->getASTContext();

    // Return the canonical signature.
    return canSig;
  }

  // A stored ASTContext indicates that this is the canonical
  // signature.
  if (CanonicalSignatureOrASTContext.is<ASTContext*>())
    // TODO: CanGenericSignature should be const-correct.
    return CanGenericSignature(const_cast<GenericSignature*>(this));
  
  // Otherwise, return the stored canonical signature.
  return CanGenericSignature(
           CanonicalSignatureOrASTContext.get<GenericSignature*>());
}

ASTContext &GenericSignature::getASTContext() const {
  // Canonical signatures store the ASTContext directly.
  if (auto ctx = CanonicalSignatureOrASTContext.dyn_cast<ASTContext *>())
    return *ctx;

  // For everything else, just get it from the generic parameter.
  return getASTContext(getGenericParams(), getRequirements());
}

SubstitutionMap
GenericSignature::getSubstitutionMap(ArrayRef<Substitution> subs) const {
  SubstitutionMap result;
  getSubstitutionMap(subs, result);
  return result;
}

void
GenericSignature::getSubstitutionMap(ArrayRef<Substitution> subs,
                                     SubstitutionMap &result) const {
  // An empty parameter list gives an empty map.
  if (subs.empty())
    assert(getGenericParams().empty());

  for (auto depTy : getAllDependentTypes()) {
    auto sub = subs.front();
    subs = subs.slice(1);

    auto canTy = depTy->getCanonicalType();
    result.addSubstitution(canTy, sub.getReplacement());
    result.addConformances(canTy, sub.getConformances());
  }

  // TODO: same-type constraints

  assert(subs.empty() && "did not use all substitutions?!");
}

void GenericSignature::
getSubstitutions(ModuleDecl &mod,
                 const TypeSubstitutionMap &subs,
                 GenericSignature::LookupConformanceFn lookupConformance,
                 SmallVectorImpl<Substitution> &result) const {
  auto &ctx = getASTContext();

  Type currentReplacement;
  SmallVector<ProtocolConformanceRef, 4> currentConformances;

  for (const auto &req : getRequirements()) {
    auto depTy = req.getFirstType()->getCanonicalType();

    switch (req.getKind()) {
    case RequirementKind::Conformance: {
      // Get the conformance and record it.
      auto protoType = req.getSecondType()->castTo<ProtocolType>();
      currentConformances.push_back(
          lookupConformance(depTy, currentReplacement, protoType));
      break;
    }

    case RequirementKind::Superclass:
      // Superclass requirements aren't recorded in substitutions.
      break;

    case RequirementKind::SameType:
      // Same-type requirements aren't recorded in substitutions.
      break;

    case RequirementKind::WitnessMarker:
      // Flush the current conformances.
      if (currentReplacement) {
        result.push_back({
          currentReplacement,
          ctx.AllocateCopy(currentConformances)
        });
        currentConformances.clear();
      }

      // Each witness marker starts a new substitution.
      currentReplacement = req.getFirstType().subst(&mod, subs);
      if (!currentReplacement)
        currentReplacement = ErrorType::get(req.getFirstType());

      break;
    }
  }

  // Flush the final conformances.
  if (currentReplacement) {
    result.push_back({
      currentReplacement,
      ctx.AllocateCopy(currentConformances),
    });
    currentConformances.clear();
  }
}

void GenericSignature::
getSubstitutions(ModuleDecl &mod,
                 const SubstitutionMap &subMap,
                 SmallVectorImpl<Substitution> &result) const {
  auto lookupConformanceFn =
      [&](CanType original, Type replacement, ProtocolType *protoType)
          -> ProtocolConformanceRef {
    return *subMap.lookupConformance(original, protoType->getDecl());
  };

  getSubstitutions(mod, subMap.getMap(), lookupConformanceFn, result);
}

bool GenericSignature::requiresClass(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return false;

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  if (auto *archetypeTy = contextTy->getAs<ArchetypeType>())
    return archetypeTy->requiresClass();
  return false;
}

/// Determine the superclass bound on the given dependent type.
Type GenericSignature::getSuperclassBound(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return nullptr;

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  if (auto *archetypeTy = contextTy->getAs<ArchetypeType>())
    return archetypeTy->getSuperclass();

  return Type();
}

/// Determine the set of protocols to which the given dependent type
/// must conform.
ArrayRef<ProtocolDecl *> GenericSignature::getConformsTo(Type type,
                                                         ModuleDecl &mod) {
  if (!type->isTypeParameter()) return { };

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  if (auto *archetypeTy = contextTy->getAs<ArchetypeType>())
    return archetypeTy->getConformsTo();

  return { };
}

/// Determine whether the given dependent type is equal to a concrete type.
bool GenericSignature::isConcreteType(Type type, ModuleDecl &mod) {
  return bool(getConcreteType(type, mod));
}

/// Return the concrete type that the given dependent type is constrained to,
/// or the null Type if it is not the subject of a concrete same-type
/// constraint.
Type GenericSignature::getConcreteType(Type type, ModuleDecl &mod) {
  if (!type->isTypeParameter()) return Type();

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  if (contextTy->is<ArchetypeType>())
    return Type();

  return contextTy;
}

Type GenericSignature::getRepresentative(Type type, ModuleDecl &mod) {
  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  return genericEnv->mapTypeOutOfContext(&mod, contextTy)->getCanonicalType();
}

bool GenericSignature::areSameTypeParameterInContext(Type type1, Type type2,
                                                     ModuleDecl &mod) {
  assert(type1->isTypeParameter());
  assert(type2->isTypeParameter());

  if (type1.getPointer() == type2.getPointer())
    return true;

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto first = genericEnv->mapTypeIntoContext(&mod, type1);
  auto second = genericEnv->mapTypeIntoContext(&mod, type2);
  return first->isEqual(second);
}

bool GenericSignature::isCanonicalTypeInContext(Type type, ModuleDecl &mod) {
  // If the type isn't independently canonical, it's certainly not canonical
  // in this context.
  if (!type->isCanonical())
    return false;

  // All the contextual canonicality rules apply to type parameters, so if the
  // type doesn't involve any type parameters, it's already canonical.
  if (!type->hasTypeParameter())
    return true;

  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  return type->isEqual(genericEnv->mapTypeOutOfContext(&mod, contextTy));
}

CanType GenericSignature::getCanonicalTypeInContext(Type type, ModuleDecl &mod) {
  auto genericEnv = getCanonicalGenericEnvironment(mod);
  auto contextTy = genericEnv->mapTypeIntoContext(&mod, type);
  return genericEnv->mapTypeOutOfContext(&mod, contextTy)->getCanonicalType();
}
