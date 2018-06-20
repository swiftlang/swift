//===--- RequirementEnvironment.cpp - Requirement Environments ------------===//
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
// This file implements the RequirementEnvironment class, which is used to
// capture how a witness to a protocol requirement maps type parameters.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/RequirementEnvironment.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/Statistic.h"

#define DEBUG_TYPE "Protocol conformance checking"

using namespace swift;

STATISTIC(NumRequirementEnvironments, "# of requirement environments");

RequirementEnvironment::RequirementEnvironment(
                                           DeclContext *conformanceDC,
                                           GenericSignature *reqSig,
                                           ProtocolDecl *proto,
                                           ClassDecl *covariantSelf,
                                           ProtocolConformance *conformance)
    : reqSig(reqSig) {
  ASTContext &ctx = conformanceDC->getASTContext();

  auto concreteType = conformanceDC->getSelfInterfaceType();
  auto *conformanceSig = conformanceDC->getGenericSignatureOfContext();

  // This is a substitution function from the generic parameters of the
  // conforming type to the synthetic environment.
  //
  // For structs, enums and protocols, this is a 1:1 mapping; for classes,
  // we increase the depth of each generic parameter by 1 so that we can
  // introduce a class-bound 'Self' parameter.
  //
  // This is a raw function rather than a substitution map because we need to
  // keep generic parameters as generic, even if the conformanceSig (the best
  // way to create the substitution map) equates them to concrete types.
  auto conformanceToSyntheticTypeFn = [&](SubstitutableType *type) {
    auto *genericParam = cast<GenericTypeParamType>(type);
    if (covariantSelf) {
      return GenericTypeParamType::get(genericParam->getDepth() + 1,
                                       genericParam->getIndex(), ctx);
    }

    return GenericTypeParamType::get(genericParam->getDepth(),
                                     genericParam->getIndex(), ctx);
  };
  auto conformanceToSyntheticConformanceFn =
      MakeAbstractConformanceForGenericType();

  auto substConcreteType = concreteType.subst(
      conformanceToSyntheticTypeFn, conformanceToSyntheticConformanceFn);

  // Calculate the depth at which the requirement's generic parameters
  // appear in the synthetic signature.
  unsigned depth = 0;
  if (covariantSelf) {
    depth++;
  }
  if (conformanceSig) {
    depth += conformanceSig->getGenericParams().back()->getDepth() + 1;
  }

  // Build a substitution map to replace the protocol's \c Self and the type
  // parameters of the requirement into a combined context that provides the
  // type parameters of the conformance context and the parameters of the
  // requirement.
  auto selfType = cast<GenericTypeParamType>(
      proto->getSelfInterfaceType()->getCanonicalType());

  reqToSyntheticEnvMap = SubstitutionMap::get(reqSig,
    [selfType, substConcreteType, depth, covariantSelf, &ctx]
    (SubstitutableType *type) -> Type {
      // If the conforming type is a class, the protocol 'Self' maps to
      // the class-constrained 'Self'. Otherwise, it maps to the concrete
      // type.
      if (type->isEqual(selfType)) {
        if (covariantSelf)
          return GenericTypeParamType::get(/*depth=*/0, /*index=*/0, ctx);
        return substConcreteType;
      }
      // Other requirement generic parameters map 1:1 with their depth
      // increased appropriately.
      auto *genericParam = cast<GenericTypeParamType>(type);
      // In a protocol requirement, the only generic parameter at depth 0
      // should be 'Self', and all others at depth 1. Anything else is
      // invalid code.
      if (genericParam->getDepth() != 1)
        return Type();
      auto substGenericParam =
        GenericTypeParamType::get(depth, genericParam->getIndex(), ctx);
      return substGenericParam;
    },
    [selfType, substConcreteType, conformance, conformanceDC, &ctx](
        CanType type, Type replacement, ProtocolDecl *proto)
          -> Optional<ProtocolConformanceRef> {
      // The protocol 'Self' conforms concretely to the conforming type.
      if (type->isEqual(selfType)) {
        ProtocolConformance *specialized = conformance;
        if (conformance && conformance->getGenericSignature()) {
          auto concreteSubs =
            substConcreteType->getContextSubstitutionMap(
              conformanceDC->getParentModule(), conformanceDC);
          specialized =
            ctx.getSpecializedConformance(substConcreteType, conformance,
                                          concreteSubs);
        }

        if (specialized)
          return ProtocolConformanceRef(specialized);
      }

      // All other generic parameters come from the requirement itself
      // and conform abstractly.
      return ProtocolConformanceRef(proto);
    });

  // If the requirement itself is non-generic, the synthetic signature
  // is that of the conformance context.
  if (!covariantSelf &&
      reqSig->getGenericParams().size() == 1 &&
      reqSig->getRequirements().size() == 1) {
    syntheticSignature = conformanceDC->getGenericSignatureOfContext();
    if (syntheticSignature) {
      syntheticSignature = syntheticSignature->getCanonicalSignature();
      syntheticEnvironment =
        syntheticSignature->createGenericEnvironment();
    }

    return;
  }

  // Construct a generic signature builder by collecting the constraints
  // from the requirement and the context of the conformance together,
  // because both define the capabilities of the requirement.
  GenericSignatureBuilder builder(ctx);

  auto source =
    GenericSignatureBuilder::FloatingRequirementSource::forAbstract();

  // If the conforming type is a class, add a class-constrained 'Self'
  // parameter.
  if (covariantSelf) {
    auto paramTy = GenericTypeParamType::get(/*depth=*/0, /*index=*/0, ctx);
    builder.addGenericParameter(paramTy);
  }

  // Now, add all generic parameters from the conforming type.
  if (conformanceSig) {
    for (auto param : conformanceSig->getGenericParams()) {
      auto substParam = Type(param).subst(conformanceToSyntheticTypeFn,
                                          conformanceToSyntheticConformanceFn);
      builder.addGenericParameter(substParam->castTo<GenericTypeParamType>());
    }
  }

  // Next, add requirements.
  if (covariantSelf) {
    auto paramTy = GenericTypeParamType::get(/*depth=*/0, /*index=*/0, ctx);
    Requirement reqt(RequirementKind::Superclass, paramTy, substConcreteType);
    builder.addRequirement(reqt, source, nullptr);
  }

  if (conformanceSig) {
    for (auto &rawReq : conformanceSig->getRequirements()) {
      if (auto req = rawReq.subst(conformanceToSyntheticTypeFn,
                                  conformanceToSyntheticConformanceFn))
        builder.addRequirement(*req, source, nullptr);
    }
  }

  // Finally, add the generic parameters from the requirement.
  for (auto genericParam : reqSig->getGenericParams().slice(1)) {
    // The only depth that makes sense is depth == 1, the generic parameters
    // of the requirement itself. Anything else is from invalid code.
    if (genericParam->getDepth() != 1) {
      return;
    }

    // Create an equivalent generic parameter at the next depth.
    auto substGenericParam =
      GenericTypeParamType::get(depth, genericParam->getIndex(), ctx);

    builder.addGenericParameter(substGenericParam);
  }

  ++NumRequirementEnvironments;

  // Next, add each of the requirements (mapped from the requirement's
  // interface types into the abstract type parameters).
  for (auto &rawReq : reqSig->getRequirements()) {
    if (auto req = rawReq.subst(reqToSyntheticEnvMap))
      builder.addRequirement(*req, source, conformanceDC->getParentModule());
  }

  // Produce the generic signature and environment.
  // FIXME: Pass in a source location for the conformance, perhaps? It seems
  // like this could fail.
  syntheticSignature =
    std::move(builder).computeGenericSignature(SourceLoc());
  syntheticEnvironment = syntheticSignature->createGenericEnvironment();
}
