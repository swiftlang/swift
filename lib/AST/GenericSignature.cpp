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

bool GenericSignature::enumeratePairedRequirements(
               llvm::function_ref<bool(Type, ArrayRef<Requirement>)> fn) const {
  // We'll be walking through the list of requirements.
  ArrayRef<Requirement> reqs = getRequirements();
  unsigned curReqIdx = 0, numReqs = reqs.size();

  // ... and walking through the list of generic parameters.
  ArrayRef<GenericTypeParamType *> genericParams = getGenericParams();
  unsigned curGenericParamIdx = 0, numGenericParams = genericParams.size();

  /// Local function to 'catch up' to the next dependent type we're going to
  /// visit, calling the function for each of the generic parameters in the
  /// generic parameter list prior to this parameter.
  auto enumerateGenericParamsUpToDependentType = [&](CanType depTy) -> bool {
    // Figure out where we should stop when enumerating generic parameters.
    unsigned stopDepth, stopIndex;
    if (auto gp = dyn_cast_or_null<GenericTypeParamType>(depTy)) {
      stopDepth = gp->getDepth();
      stopIndex = gp->getIndex();
    } else {
      stopDepth = genericParams.back()->getDepth() + 1;
      stopIndex = 0;
    }

    // Enumerate generic parameters up to the stopping point, calling the
    // callback function for each one
    while (curGenericParamIdx != numGenericParams) {
      auto curGenericParam = genericParams[curGenericParamIdx];

      // If the current generic parameter is before our stopping point, call
      // the function.
      if (curGenericParam->getDepth() < stopDepth ||
          (curGenericParam->getDepth() == stopDepth &&
           curGenericParam->getIndex() < stopIndex)) {
        if (fn(curGenericParam, { })) return true;
        ++curGenericParamIdx;
        continue;
      }

      // If the current generic parameter is at our stopping point, we're
      // done.
      if (curGenericParam->getDepth() == stopDepth &&
          curGenericParam->getIndex() == stopIndex) {
        ++curGenericParamIdx;
        return false;
      }

      // Otherwise, there's nothing to do.
      break;
    }

    return false;
  };

  // Walk over all of the requirements.
  while (curReqIdx != numReqs) {
    // "Catch up" by enumerating generic parameters up to this dependent type.
    CanType depTy = reqs[curReqIdx].getFirstType()->getCanonicalType();
    if (enumerateGenericParamsUpToDependentType(depTy)) return true;

    // Utility to skip over non-conformance constraints that apply to this
    // type.
    bool sawSameTypeConstraint = false;
    auto skipNonConformanceConstraints = [&] {
      while (curReqIdx != numReqs &&
             reqs[curReqIdx].getKind() != RequirementKind::Conformance &&
             reqs[curReqIdx].getFirstType()->getCanonicalType() == depTy) {
        // Record whether we saw a same-type constraint mentioning this type.
        if (reqs[curReqIdx].getKind() == RequirementKind::SameType)
          sawSameTypeConstraint = true;

        ++curReqIdx;
      }
    };

    // First, skip past any non-conformance constraints on this type.
    skipNonConformanceConstraints();

    // Collect all of the conformance constraints for this dependent type.
    unsigned startIdx = curReqIdx;
    unsigned endIdx = curReqIdx;
    while (curReqIdx != numReqs &&
           reqs[curReqIdx].getKind() == RequirementKind::Conformance &&
           reqs[curReqIdx].getFirstType()->getCanonicalType() == depTy) {
      ++curReqIdx;
      endIdx = curReqIdx;
    }

    // Skip any trailing non-conformance constraints.
    skipNonConformanceConstraints();

    // If there were any conformance constraints, or we have a generic
    // parameter we can't skip, invoke the callback.
    if ((startIdx != endIdx ||
         (isa<GenericTypeParamType>(depTy) && !sawSameTypeConstraint)) &&
        fn(depTy, reqs.slice(startIdx, endIdx-startIdx)))
      return true;
  }

  // Catch up on any remaining generic parameters.
  return enumerateGenericParamsUpToDependentType(CanType());
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
    if (isa<GenericTypeParamType>(canTy))
      result.addSubstitution(canTy, sub.getReplacement());
    result.addConformances(canTy, sub.getConformances());
  }

  // TODO: same-type constraints

  assert(subs.empty() && "did not use all substitutions?!");
}

SmallVector<Type, 4> GenericSignature::getAllDependentTypes() const {
  SmallVector<Type, 4> result;
  enumeratePairedRequirements([&](Type type, ArrayRef<Requirement>) {
    result.push_back(type);
    return false;
  });

  return result;
}

void GenericSignature::
getSubstitutions(ModuleDecl &mod,
                 const TypeSubstitutionMap &subs,
                 GenericSignature::LookupConformanceFn lookupConformance,
                 SmallVectorImpl<Substitution> &result) const {
  // Enumerate all of the requirements that require substitution.
  enumeratePairedRequirements([&](Type depTy, ArrayRef<Requirement> reqs) {
    auto &ctx = getASTContext();

    // Compute the replacement type.
    Type currentReplacement = depTy.subst(&mod, subs);
    if (!currentReplacement)
      currentReplacement = ErrorType::get(depTy);

    // Collect the conformances.
    SmallVector<ProtocolConformanceRef, 4> currentConformances;
    for (auto req: reqs) {
      assert(req.getKind() == RequirementKind::Conformance);
      auto protoType = req.getSecondType()->castTo<ProtocolType>();
      currentConformances.push_back(
        lookupConformance(depTy->getCanonicalType(), currentReplacement,
                          protoType));
    }

    // Add it to the final substitution list.
    result.push_back({
      currentReplacement,
      ctx.AllocateCopy(currentConformances)
    });

    return false;
  });
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
