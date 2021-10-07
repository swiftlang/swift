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
// This file implements the main entry points into the requirement machine
// via the request evaluator.
//
//===----------------------------------------------------------------------===//

#include "RequirementMachine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignatureBuilder.h"
#include "swift/AST/LazyResolver.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Statistic.h"

using namespace swift;
using namespace rewriting;

#define DEBUG_TYPE "Serialization"

STATISTIC(NumLazyRequirementSignaturesLoaded,
          "# of lazily-deserialized requirement signatures loaded");

#undef DEBUG_TYPE

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
