//===--- LocalArchetypeRequirementCollector.cpp ---------------------------===//1
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements the LocalArchetypeRequirementCollector class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/LocalArchetypeRequirementCollector.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Assertions.h"

using namespace swift;

LocalArchetypeRequirementCollector::LocalArchetypeRequirementCollector(
	const ASTContext &ctx, GenericSignature sig)
    : Context(ctx), OuterSig(sig), Depth(sig.getNextDepth()) {}

void LocalArchetypeRequirementCollector::addOpenedExistential(Type constraint) {
  if (auto existential = constraint->getAs<ExistentialType>())
    constraint = existential->getConstraintType();

  assert(constraint->isConstraintType() ||
         constraint->getClassOrBoundGenericClass());
  assert(OuterSig || !constraint->hasTypeParameter() &&
         "Interface type here requires a parent signature");

  auto param = addParameter();

  Requirements.emplace_back(RequirementKind::Conformance, param, constraint);

  ++Depth;
}

void LocalArchetypeRequirementCollector::addOpenedElement(
        CanGenericTypeParamType shapeClass) {

  size_t startingIndex = Params.size();

  /// Add a parameter for each of the opened elements in this shape class.
  SmallVector<GenericTypeParamType *, 2> packParams;
  for (auto paramType : OuterSig.getGenericParams()) {
    if (paramType->isParameterPack() &&
        OuterSig->haveSameShape(paramType, shapeClass)) {
      packParams.push_back(paramType);
      addParameter();
    }
  }

  assert(!packParams.empty());

  // Clone the element requirements.

  // Helper function: replace references to type parameter packs
  // with one of the opened element type parameters we just created for this
  // shape class.
  auto rewriteElementType = [=](Type type) {
    return type.transformTypeParameterPacks(
      [&](SubstitutableType *t) -> std::optional<Type> {
        auto *paramType = cast<GenericTypeParamType>(t);
        for (unsigned packElementIndex : indices(packParams)) {
          if (paramType == packParams[packElementIndex])
            return Params[startingIndex + packElementIndex];
        }

        return std::nullopt;
      });
  };

  // Clone the pack requirements that apply to this shape class.
  for (auto req : OuterSig.getRequirements()) {
    switch (req.getKind()) {
    case RequirementKind::SameShape:
      // These never involve element types.
      break;
    case RequirementKind::Conformance:
    case RequirementKind::Superclass:
    case RequirementKind::SameType: {
      auto substFirstType = rewriteElementType(req.getFirstType());
      auto substSecondType = rewriteElementType(req.getSecondType());
      if (!substFirstType->isEqual(req.getFirstType()) ||
          !substSecondType->isEqual(req.getSecondType())) {
        Requirements.emplace_back(req.getKind(), substFirstType, substSecondType);
      }
      break;
    }
    case RequirementKind::Layout: {
      auto substFirstType = rewriteElementType(req.getFirstType());
      if (!substFirstType->isEqual(req.getFirstType())) {
        Requirements.emplace_back(req.getKind(), substFirstType,
                                  req.getLayoutConstraint());
      }
      break;
    }
    }
  }

  ++Depth;
}

GenericTypeParamType *LocalArchetypeRequirementCollector::addParameter() {
  unsigned index = 0;
  if (!Params.empty() &&
      Params.back()->getDepth() == Depth) {
    index = Params.back()->getIndex() + 1;
  }

  auto *param = GenericTypeParamType::getType(Depth, index, Context);
  Params.push_back(param);
  return param;
}

GenericSignature swift::buildGenericSignatureWithCapturedEnvironments(
    ASTContext &ctx,
    GenericSignature sig,
    ArrayRef<GenericEnvironment *> capturedEnvs) {
  // Add new generic parameters to replace the local archetypes.
  LocalArchetypeRequirementCollector collector(ctx, sig);

  for (auto *genericEnv : capturedEnvs) {
    switch (genericEnv->getKind()) {
    case GenericEnvironment::Kind::Primary:
    case GenericEnvironment::Kind::Opaque:
      break;

    case GenericEnvironment::Kind::OpenedExistential: {
      auto existentialTy = genericEnv->maybeApplyOuterContextSubstitutions(
          genericEnv->getOpenedExistentialType())
              ->mapTypeOutOfContext();
      collector.addOpenedExistential(existentialTy);
      continue;
    }
    case GenericEnvironment::Kind::OpenedElement: {
      collector.addOpenedElement(
          genericEnv->getOpenedElementShapeClass());
      continue;
    }
    }

    llvm_unreachable("Cannot happen");
  }

  return buildGenericSignature(ctx,
                               collector.OuterSig,
                               collector.Params,
                               collector.Requirements,
                               /*allowInverses=*/false);
}

Type MapLocalArchetypesOutOfContext::getInterfaceType(
    Type interfaceTy, GenericEnvironment *genericEnv) const {

  if (auto *dmt = interfaceTy->getAs<DependentMemberType>()) {
    auto newBase = getInterfaceType(dmt->getBase(), genericEnv);
    return DependentMemberType::get(newBase, dmt->getAssocType());
  }

  auto rootParam = interfaceTy->castTo<GenericTypeParamType>();
  ASSERT(!rootParam->isParameterPack());
  ASSERT(rootParam->getDepth() == genericEnv->getGenericSignature()->getMaxDepth());

  // The new depth is determined by counting how many captured environments
  // precede this one.
  unsigned depth = baseGenericSig.getNextDepth();
  for (auto *capturedEnv : capturedEnvs) {
    if (capturedEnv == genericEnv) {
      return GenericTypeParamType::getType(depth, rootParam->getIndex(),
                                           rootParam->getASTContext());
    }

    ++depth;
  }

  ABORT([&](auto &out) {
    out << "Fell off the end:\n";
    interfaceTy->dump(out);
  });
}

Type MapLocalArchetypesOutOfContext::operator()(SubstitutableType *type) const {
  // Local archetypes can appear in interface types alongside generic param
  // types, ignore them here.
  auto *archetypeTy = dyn_cast<ArchetypeType>(type);
  if (!archetypeTy)
    return type;

  // Primary archetypes just map out of context.
  if (isa<PrimaryArchetypeType>(archetypeTy) ||
      isa<PackArchetypeType>(archetypeTy)) {
    return archetypeTy->getInterfaceType();
  }

  ASSERT(isa<LocalArchetypeType>(archetypeTy));

  // Root local archetypes change depth.
  auto *genericEnv = archetypeTy->getGenericEnvironment();
  return getInterfaceType(archetypeTy->getInterfaceType(), genericEnv);
}

Type swift::mapLocalArchetypesOutOfContext(
    Type type,
    GenericSignature baseGenericSig,
    ArrayRef<GenericEnvironment *> capturedEnvs) {
  return type.subst(MapLocalArchetypesOutOfContext(baseGenericSig, capturedEnvs),
                    MakeAbstractConformanceForGenericType(),
                    SubstFlags::PreservePackExpansionLevel |
                    SubstFlags::SubstitutePrimaryArchetypes |
                    SubstFlags::SubstituteLocalArchetypes);
}

static Type mapIntoLocalContext(GenericTypeParamType *param, unsigned baseDepth,
                                ArrayRef<GenericEnvironment *> capturedEnvs) {
  assert(!param->isParameterPack());
  unsigned envIndex = param->getDepth() - baseDepth;
  assert(envIndex < capturedEnvs.size());
  auto *capturedEnv = capturedEnvs[envIndex];
  auto localInterfaceType = capturedEnv->getGenericSignature()
      .getInnermostGenericParams()[param->getIndex()];
  assert(localInterfaceType->getIndex() == param->getIndex());
  return capturedEnvs[envIndex]->mapTypeIntoContext(localInterfaceType);
}

Type MapIntoLocalArchetypeContext::operator()(SubstitutableType *type) const {
  unsigned baseDepth = baseGenericEnv->getGenericSignature().getNextDepth();

  auto param = cast<GenericTypeParamType>(type);
  if (param->getDepth() >= baseDepth)
    return mapIntoLocalContext(param, baseDepth, capturedEnvs);

  return baseGenericEnv->mapTypeIntoContext(param);
}

/// Given a substitution map for a call to a local function or closure, extend
/// it to include all captured element archetypes; they become primary archetypes
/// inside the body of the function.
SubstitutionMap
swift::buildSubstitutionMapWithCapturedEnvironments(
    SubstitutionMap baseSubMap,
    GenericSignature genericSigWithCaptures,
    ArrayRef<GenericEnvironment *> capturedEnvs) {

  if (capturedEnvs.empty()) {
    assert((!baseSubMap && !genericSigWithCaptures) ||
           baseSubMap.getGenericSignature()->isEqual(genericSigWithCaptures));
    return baseSubMap;
  }

  unsigned baseDepth = genericSigWithCaptures.getNextDepth() - capturedEnvs.size();

  return SubstitutionMap::get(
    genericSigWithCaptures,
    [&](SubstitutableType *type) -> Type {
      auto param = cast<GenericTypeParamType>(type);
      if (param->getDepth() >= baseDepth)
        return mapIntoLocalContext(param, baseDepth, capturedEnvs);
      return Type(type).subst(baseSubMap);
    },
    [&](InFlightSubstitution &IFS, Type origType, ProtocolDecl *proto)
          -> ProtocolConformanceRef {
      if (origType->getRootGenericParam()->getDepth() >= baseDepth)
        return ProtocolConformanceRef::forAbstract(origType.subst(IFS), proto);
      return baseSubMap.lookupConformance(origType->getCanonicalType(), proto);
    });
}
