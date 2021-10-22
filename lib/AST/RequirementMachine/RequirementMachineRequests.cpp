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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Statistic.h"
#include <vector>

#include "RequirementMachine.h"

using namespace swift;
using namespace rewriting;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyRequirementSignaturesLoaded,
          "# of lazily-deserialized requirement signatures loaded");

#undef DEBUG_TYPE

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

/// Convert a list of non-permanent, non-redundant rewrite rules into a minimal
/// protocol requirement signature for \p proto. The requirements are sorted in
/// canonical order, and same-type requirements are canonicalized.
std::vector<Requirement>
RequirementMachine::buildRequirementSignature(ArrayRef<unsigned> rules,
                                              const ProtocolDecl *proto) const {
  std::vector<Requirement> reqs;
  llvm::SmallDenseMap<TypeBase *, ConnectedComponent> sameTypeReqs;

  auto genericParams = proto->getGenericSignature().getGenericParams();

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
    } else if (rule.getLHS().back().getKind() != Symbol::Kind::Protocol) {
      auto constraintType = Context.getTypeForTerm(rule.getLHS(), genericParams);
      auto subjectType = Context.getTypeForTerm(rule.getRHS(), genericParams);

      sameTypeReqs[subjectType.getPointer()].Members.push_back(constraintType);
    }
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
  std::sort(reqs.begin(), reqs.end(),
            [](const Requirement &lhs, const Requirement &rhs) -> bool {
              return lhs.compare(rhs) < 0;
            });

  return reqs;
}

/// Builds the requirement signatures for each protocol in this strongly
/// connected component.
llvm::DenseMap<const ProtocolDecl *, std::vector<Requirement>>
RequirementMachine::computeMinimalRequirements() {
  assert(Protos.size() > 0);
  System.minimizeRewriteSystem();

  auto rules = System.getMinimizedRules(Protos);

  // Note that we build 'result' by iterating over 'Protos' rather than
  // 'rules'; this is intentional, so that even if a protocol has no
  // rules, we still end up creating an entry for it in 'result'.
  llvm::DenseMap<const ProtocolDecl *, std::vector<Requirement>> result;
  for (const auto *proto : Protos)
    result[proto] = buildRequirementSignature(rules[proto], proto);

  return result;
}

ArrayRef<Requirement>
RequirementSignatureRequest::evaluate(Evaluator &evaluator,
                                      ProtocolDecl *proto) const {
  ASTContext &ctx = proto->getASTContext();

  // First check if we have a deserializable requirement signature.
  if (proto->hasLazyRequirementSignature()) {
    ++NumLazyRequirementSignaturesLoaded;
    // FIXME: (transitional) increment the redundant "always-on" counter.
    if (ctx.Stats)
      ++ctx.Stats->getFrontendCounters().NumLazyRequirementSignaturesLoaded;

    auto contextData = static_cast<LazyProtocolData *>(
        ctx.getOrCreateLazyContextData(proto, nullptr));

    SmallVector<Requirement, 8> requirements;
    contextData->loader->loadRequirementSignature(
        proto, contextData->requirementSignatureData, requirements);
    if (requirements.empty())
      return None;
    return ctx.AllocateCopy(requirements);
  }

  auto buildViaGSB = [&]() {
    GenericSignatureBuilder builder(proto->getASTContext());

    // Add all of the generic parameters.
    for (auto gp : *proto->getGenericParams())
      builder.addGenericParameter(gp);

    // Add the conformance of 'self' to the protocol.
    auto selfType =
      proto->getSelfInterfaceType()->castTo<GenericTypeParamType>();
    auto requirement =
      Requirement(RequirementKind::Conformance, selfType,
                  proto->getDeclaredInterfaceType());

    builder.addRequirement(
            requirement,
            GenericSignatureBuilder::RequirementSource::forRequirementSignature(
                                                        builder, selfType, proto),
            nullptr);

    auto reqSignature = std::move(builder).computeGenericSignature(
                          /*allowConcreteGenericParams=*/false,
                          /*requirementSignatureSelfProto=*/proto);
    return reqSignature.getRequirements();
  };

  auto buildViaRQM = [&]() {
    // We build requirement signatures for all protocols in a strongly connected
    // component at the same time.
    auto *machine = ctx.getOrCreateRequirementMachine(proto);
    auto requirements = machine->computeMinimalRequirements();

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

      // Don't call setRequirementSignature() on the original proto; the
      // request evaluator will do it for us.
      if (otherProto == proto)
        result = reqsCopy;
      else
        const_cast<ProtocolDecl *>(otherProto)->setRequirementSignature(reqsCopy);

      // Dump the result if requested.
      if (debug) {
        llvm::dbgs() << "Protocol " << otherProto->getName() << ": ";

        auto sig = GenericSignature::get(
            otherProto->getGenericSignature().getGenericParams(),
            reqsCopy);
        llvm::dbgs() << sig << "\n";
      }
    }

    // Return the result for the specific protocol this request was kicked off on.
    return result;
  };

  switch (ctx.LangOpts.RequirementMachineProtocolSignatures) {
  case RequirementMachineMode::Disabled:
    return buildViaGSB();

  case RequirementMachineMode::Enabled:
    return buildViaRQM();

  case RequirementMachineMode::Verify:
    abort();
  }
}
