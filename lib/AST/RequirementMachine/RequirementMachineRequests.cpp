//===--- RequirementMachineRequests.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the main entry points for computing minimized generic
// signatures using the requirement machine via the request evaluator.
//
// The actual logic for finding a minimal set of rewrite rules is implemented in
// HomotopyReduction.cpp and GeneratingConformances.cpp.
//
//===----------------------------------------------------------------------===//

#include "RequirementMachine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Statistic.h"
#include "RequirementLowering.h"
#include <memory>
#include <vector>

using namespace swift;
using namespace rewriting;

namespace {

/// Represents a set of types related by same-type requirements, and an
/// optional concrete type requirement.
struct ConnectedComponent {
  llvm::SmallVector<Type, 2> Members;
  Type ConcreteType;

  void buildRequirements(Type subjectType, std::vector<Requirement> &reqs);
};

/// Case 1: A set of rewrite rules of the form:
///
///   B => A
///   C => A
///   D => A
///
/// Become a series of same-type requirements
///
///   A == B, B == C, C == D
///
/// Case 2: A set of rewrite rules of the form:
///
///   A.[concrete: X] => A
///   B => A
///   C => A
///   D => A
///
/// Become a series of same-type requirements
///
///   A == X, B == X, C == X, D == X
void ConnectedComponent::buildRequirements(Type subjectType,
                                           std::vector<Requirement> &reqs) {
  std::sort(Members.begin(), Members.end(),
            [](Type first, Type second) -> bool {
              return compareDependentTypes(first, second) < 0;
            });

  if (!ConcreteType) {
    for (auto constraintType : Members) {
      reqs.emplace_back(RequirementKind::SameType,
                        subjectType, constraintType);
      subjectType = constraintType;
    }
  } else {
    reqs.emplace_back(RequirementKind::SameType,
                      subjectType, ConcreteType);

    for (auto constraintType : Members) {
      reqs.emplace_back(RequirementKind::SameType,
                        constraintType, ConcreteType);
    }
  }
}

}  // end namespace

/// Convert a list of non-permanent, non-redundant rewrite rules into a list of
/// requirements sorted in canonical order. The \p genericParams are used to
/// produce sugared types.
std::vector<Requirement>
RequirementMachine::buildRequirementsFromRules(
    ArrayRef<unsigned> rules,
    TypeArrayView<GenericTypeParamType> genericParams) const {
  std::vector<Requirement> reqs;
  llvm::SmallDenseMap<TypeBase *, ConnectedComponent> sameTypeReqs;

  // Convert a rewrite rule into a requirement.
  auto createRequirementFromRule = [&](const Rule &rule) {
    if (auto prop = rule.isPropertyRule()) {
      auto subjectType = Context.getTypeForTerm(rule.getRHS(), genericParams);

      switch (prop->getKind()) {
      case Symbol::Kind::Protocol:
        reqs.emplace_back(RequirementKind::Conformance,
                          subjectType,
                          prop->getProtocol()->getDeclaredInterfaceType());
        return;

      case Symbol::Kind::Layout:
        reqs.emplace_back(RequirementKind::Layout,
                          subjectType,
                          prop->getLayoutConstraint());
        return;

      case Symbol::Kind::Superclass:
        reqs.emplace_back(RequirementKind::Superclass,
                          subjectType,
                          Context.getTypeFromSubstitutionSchema(
                              prop->getSuperclass(),
                              prop->getSubstitutions(),
                              genericParams, MutableTerm()));
        return;

      case Symbol::Kind::ConcreteType: {
        auto concreteType = Context.getTypeFromSubstitutionSchema(
                                prop->getConcreteType(),
                                prop->getSubstitutions(),
                                genericParams, MutableTerm());

        auto &component = sameTypeReqs[subjectType.getPointer()];
        assert(!component.ConcreteType);
        component.ConcreteType = concreteType;
        return;
      }

      case Symbol::Kind::Name:
      case Symbol::Kind::AssociatedType:
      case Symbol::Kind::GenericParam:
        break;
      }

      llvm_unreachable("Invalid symbol kind");
    }

    assert(rule.getLHS().back().getKind() != Symbol::Kind::Protocol);
    auto constraintType = Context.getTypeForTerm(rule.getLHS(), genericParams);
    auto subjectType = Context.getTypeForTerm(rule.getRHS(), genericParams);

    sameTypeReqs[subjectType.getPointer()].Members.push_back(constraintType);
  };

  if (getDebugOptions().contains(DebugFlags::Minimization)) {
    llvm::dbgs() << "Minimized rules:\n";
  }

  // Build the list of requirements, storing same-type requirements off
  // to the side.
  for (unsigned ruleID : rules) {
    const auto &rule = System.getRule(ruleID);

    if (getDebugOptions().contains(DebugFlags::Minimization)) {
      llvm::dbgs() << "- " << rule << "\n";
    }

    createRequirementFromRule(rule);
  }

  // Now, convert each connected component into a series of same-type
  // requirements.
  for (auto &pair : sameTypeReqs) {
    pair.second.buildRequirements(pair.first, reqs);
  }

  if (getDebugOptions().contains(DebugFlags::Minimization)) {
    llvm::dbgs() << "Requirements:\n";
    for (const auto &req : reqs) {
      req.dump(llvm::dbgs());
      llvm::dbgs() << "\n";
    }
  }

  // Finally, sort the requirements in canonical order.
  llvm::array_pod_sort(reqs.begin(), reqs.end(),
                       [](const Requirement *lhs, const Requirement *rhs) -> int {
                         return lhs->compare(*rhs);
                       });

  return reqs;
}

/// Builds the requirement signatures for each protocol in this strongly
/// connected component.
llvm::DenseMap<const ProtocolDecl *, std::vector<Requirement>>
RequirementMachine::computeMinimalProtocolRequirements() {
  assert(Protos.size() > 0 &&
         "Not a protocol connected component rewrite system");
  assert(Params.empty() &&
         "Not a protocol connected component rewrite system");

  System.minimizeRewriteSystem();

  if (Dump) {
    llvm::dbgs() << "Minimized rewrite system:\n";
    dump(llvm::dbgs());
  }

  auto rules = System.getMinimizedProtocolRules(Protos);

  // Note that we build 'result' by iterating over 'Protos' rather than
  // 'rules'; this is intentional, so that even if a protocol has no
  // rules, we still end up creating an entry for it in 'result'.
  llvm::DenseMap<const ProtocolDecl *, std::vector<Requirement>> result;
  for (const auto *proto : Protos) {
    auto genericParams = proto->getGenericSignature().getGenericParams();
    result[proto] = buildRequirementsFromRules(rules[proto], genericParams);
  }

  return result;
}

ArrayRef<Requirement>
RequirementSignatureRequestRQM::evaluate(Evaluator &evaluator,
                                         ProtocolDecl *proto) const {
  ASTContext &ctx = proto->getASTContext();

  // First check if we have a deserializable requirement signature.
  assert(!proto->hasLazyRequirementSignature() &&
         "Should be handled in RequirementSignatureRequest");

  // We build requirement signatures for all protocols in a strongly connected
  // component at the same time.
  auto *machine = ctx.getRewriteContext().getRequirementMachine(proto);
  auto requirements = machine->computeMinimalProtocolRequirements();

  bool debug = machine->getDebugOptions().contains(DebugFlags::Minimization);

  // The requirement signature for the actual protocol that the result
  // was kicked off with.
  ArrayRef<Requirement> result;

  for (const auto &pair : requirements) {
    auto *otherProto = pair.first;
    const auto &reqs = pair.second;

    // setRequirementSignature() doesn't take ownership of the memory, so
    // we have to make a copy of the std::vector temporary.
    ArrayRef<Requirement> reqsCopy = ctx.AllocateCopy(reqs);

    // Dump the result if requested.
    if (debug) {
      llvm::dbgs() << "Protocol " << otherProto->getName() << ": ";

      auto sig = GenericSignature::get(
          otherProto->getGenericSignature().getGenericParams(),
          reqsCopy);
      llvm::dbgs() << sig << "\n";
    }

    // Don't call setRequirementSignature() on the original proto; the
    // request evaluator will do it for us.
    if (otherProto == proto)
      result = reqsCopy;
    else {
      ctx.evaluator.cacheOutput(
        RequirementSignatureRequestRQM{const_cast<ProtocolDecl *>(otherProto)},
        std::move(reqsCopy));
    }
  }

  // Return the result for the specific protocol this request was kicked off on.
  return result;
}

/// Builds the top-level generic signature requirements for this rewrite system.
std::vector<Requirement>
RequirementMachine::computeMinimalGenericSignatureRequirements() {
  assert(Protos.empty() &&
         "Not a top-level generic signature rewrite system");
  assert(!Params.empty() &&
         "Not a from-source top-level generic signature rewrite system");

  System.minimizeRewriteSystem();

  if (Dump) {
    llvm::dbgs() << "Minimized rewrite system:\n";
    dump(llvm::dbgs());
  }

  auto rules = System.getMinimizedGenericSignatureRules();
  return buildRequirementsFromRules(rules, getGenericParams());
}

GenericSignatureWithError
AbstractGenericSignatureRequestRQM::evaluate(
         Evaluator &evaluator,
         const GenericSignatureImpl *baseSignatureImpl,
         SmallVector<GenericTypeParamType *, 2> addedParameters,
         SmallVector<Requirement, 2> addedRequirements) const {
  GenericSignature baseSignature = GenericSignature{baseSignatureImpl};
  // If nothing is added to the base signature, just return the base
  // signature.
  if (addedParameters.empty() && addedRequirements.empty())
    return GenericSignatureWithError(baseSignature, /*hadError=*/false);

  ASTContext &ctx = addedParameters.empty()
      ? addedRequirements.front().getFirstType()->getASTContext()
      : addedParameters.front()->getASTContext();

  SmallVector<GenericTypeParamType *, 4> genericParams(
      baseSignature.getGenericParams().begin(),
      baseSignature.getGenericParams().end());
  genericParams.append(
      addedParameters.begin(),
      addedParameters.end());

  // If there are no added requirements, we can form the signature directly
  // with the added parameters.
  if (addedRequirements.empty()) {
    auto result = GenericSignature::get(genericParams,
                                        baseSignature.getRequirements());
    return GenericSignatureWithError(result, /*hadError=*/false);
  }

  SmallVector<Requirement, 4> requirements(
      baseSignature.getRequirements().begin(),
      baseSignature.getRequirements().end());

  // The requirements passed to this request may have been substituted,
  // meaning the subject type might be a concrete type and not a type
  // parameter.
  //
  // Also, the right hand side of conformance requirements here might be
  // a protocol composition.
  //
  // Desugaring converts these kinds of requirements into "proper"
  // requirements where the subject type is always a type parameter,
  // which is what the RuleBuilder expects.
  for (auto req : addedRequirements)
    desugarRequirement(req, requirements);

  // Heap-allocate the requirement machine to save stack space.
  std::unique_ptr<RequirementMachine> machine(new RequirementMachine(
      ctx.getRewriteContext()));

  machine->initWithAbstractRequirements(genericParams, requirements);

  auto minimalRequirements =
    machine->computeMinimalGenericSignatureRequirements();

  // FIXME: Implement this
  bool hadError = false;

  auto result = GenericSignature::get(genericParams, minimalRequirements);
  return GenericSignatureWithError(result, hadError);
}

GenericSignatureWithError
InferredGenericSignatureRequestRQM::evaluate(
        Evaluator &evaluator,
        ModuleDecl *parentModule,
        const GenericSignatureImpl *parentSigImpl,
        GenericParamList *genericParamList,
        WhereClauseOwner whereClause,
        SmallVector<Requirement, 2> addedRequirements,
        SmallVector<TypeLoc, 2> inferenceSources,
        bool allowConcreteGenericParams) const {
  GenericSignature parentSig(parentSigImpl);

  auto &ctx = parentModule->getASTContext();

  SmallVector<GenericTypeParamType *, 4> genericParams(
      parentSig.getGenericParams().begin(),
      parentSig.getGenericParams().end());

  SmallVector<StructuralRequirement, 4> requirements;
  for (const auto &req : parentSig.getRequirements())
    requirements.push_back({req, SourceLoc(), /*wasInferred=*/false});

  const auto visitRequirement = [&](const Requirement &req,
                                    RequirementRepr *reqRepr) {
    realizeRequirement(req, reqRepr, parentModule, requirements);
    return false;
  };

  if (genericParamList) {
    // Extensions never have a parent signature.
    assert(genericParamList->getOuterParameters() == nullptr || !parentSig);

    // Collect all outer generic parameter lists.
    SmallVector<GenericParamList *, 2> gpLists;
    for (auto *outerParamList = genericParamList;
         outerParamList != nullptr;
         outerParamList = outerParamList->getOuterParameters()) {
      gpLists.push_back(outerParamList);
    }

    // The generic parameter lists must appear from innermost to outermost.
    // We walk them backwards to order outer parameters before inner
    // parameters.
    for (auto *gpList : llvm::reverse(gpLists)) {
      assert(gpList->size() > 0 &&
             "Parsed an empty generic parameter list?");

      for (auto *gpDecl : *gpList) {
        auto *gpType = gpDecl->getDeclaredInterfaceType()
                             ->castTo<GenericTypeParamType>();
        genericParams.push_back(gpType);

        realizeInheritedRequirements(gpDecl, gpType, parentModule,
                                     requirements);
      }

      // Add the generic parameter list's 'where' clause to the builder.
      //
      // The only time generic parameter lists have a 'where' clause is
      // in SIL mode; all other generic declarations have a free-standing
      // 'where' clause, which will be visited below.
      WhereClauseOwner(parentModule, gpList)
        .visitRequirements(TypeResolutionStage::Structural,
                           visitRequirement);
    }
  }

  if (whereClause) {
    std::move(whereClause).visitRequirements(
        TypeResolutionStage::Structural,
        visitRequirement);
  }

  // Perform requirement inference from function parameter and result
  // types and such.
  for (auto sourcePair : inferenceSources) {
    auto *typeRepr = sourcePair.getTypeRepr();
    auto loc = typeRepr ? typeRepr->getStartLoc() : SourceLoc();

    inferRequirements(sourcePair.getType(), loc, parentModule, requirements);
  }

  // Finish by adding any remaining requirements. This is used to introduce
  // inferred same-type requirements when building the generic signature of
  // an extension whose extended type is a generic typealias.
  for (const auto &req : addedRequirements)
    requirements.push_back({req, SourceLoc(), /*wasInferred=*/true});

  // Heap-allocate the requirement machine to save stack space.
  std::unique_ptr<RequirementMachine> machine(new RequirementMachine(
      ctx.getRewriteContext()));

  machine->initWithWrittenRequirements(genericParams, requirements);

  auto minimalRequirements =
    machine->computeMinimalGenericSignatureRequirements();

  // FIXME: Implement this
  bool hadError = false;

  auto result = GenericSignature::get(genericParams, minimalRequirements);

  // FIXME: Handle allowConcreteGenericParams

  return GenericSignatureWithError(result, hadError);
}