//===--- RequirementMachine.cpp - Generics with term rewriting ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "RequirementMachine.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/Requirement.h"
#include "RequirementLowering.h"

using namespace swift;
using namespace rewriting;

RequirementMachine::RequirementMachine(RewriteContext &ctx)
    : Context(ctx), System(ctx), Map(System) {
  auto &langOpts = ctx.getASTContext().LangOpts;
  Dump = langOpts.DumpRequirementMachine;
  RequirementMachineStepLimit = langOpts.RequirementMachineStepLimit;
  RequirementMachineDepthLimit = langOpts.RequirementMachineDepthLimit;
  Stats = ctx.getASTContext().Stats;

  if (Stats)
    ++Stats->getFrontendCounters().NumRequirementMachines;
}

RequirementMachine::~RequirementMachine() {}

static void checkCompletionResult(const RequirementMachine &machine,
                                  CompletionResult result) {
  switch (result) {
  case CompletionResult::Success:
    break;

  case CompletionResult::MaxIterations:
    llvm::errs() << "Rewrite system exceeds maximum completion step count\n";
    machine.dump(llvm::errs());
    abort();

  case CompletionResult::MaxDepth:
    llvm::errs() << "Rewrite system exceeds maximum completion depth\n";
    machine.dump(llvm::errs());
    abort();
  }
}

/// Build a requirement machine for the requirements of a generic signature.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by ASTContext::getOrCreateRequirementMachine().
///
/// Asserts if completion fails within the configured number of steps.
void RequirementMachine::initWithGenericSignature(CanGenericSignature sig) {
  Sig = sig;
  Params.append(sig.getGenericParams().begin(),
                sig.getGenericParams().end());

  PrettyStackTraceGenericSignature debugStack("building rewrite system for", sig);

  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Dump) {
    llvm::dbgs() << "Adding generic signature " << sig << " {\n";
  }

  // Collect the top-level requirements, and all transtively-referenced
  // protocol requirement signatures.
  RuleBuilder builder(Context, Dump);
  builder.addRequirements(sig.getRequirements());

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/false,
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::DisallowInvalidRequirements);
  checkCompletionResult(*this, result);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }
}

/// Build a requirement machine for the structural requirements of a set
/// of protocols, which are understood to form a strongly-connected component
/// (SCC) of the protocol dependency graph.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by RequirementSignatureRequest.
///
/// Returns failure if completion fails within the configured number of steps.
CompletionResult
RequirementMachine::initWithProtocols(ArrayRef<const ProtocolDecl *> protos) {
  Protos = protos;

  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Dump) {
    llvm::dbgs() << "Adding protocols";
    for (auto *proto : protos) {
      llvm::dbgs() << " " << proto->getName();
    }
    llvm::dbgs() << " {\n";
  }

  RuleBuilder builder(Context, Dump);
  builder.addProtocols(protos);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true,
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::AllowInvalidRequirements);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }

  return result;
}

/// Build a requirement machine from a set of generic parameters and
/// (possibly non-canonical or non-minimal) abstract requirements.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by AbstractGenericSignatureRequest.
///
/// Asserts if completion fails within the configured number of steps.
void RequirementMachine::initWithAbstractRequirements(
    ArrayRef<GenericTypeParamType *> genericParams,
    ArrayRef<Requirement> requirements) {
  Params.append(genericParams.begin(), genericParams.end());

  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Dump) {
    llvm::dbgs() << "Adding generic parameters:";
    for (auto *paramTy : genericParams)
      llvm::dbgs() << " " << Type(paramTy);
    llvm::dbgs() << "\n";
  }

  // Collect the top-level requirements, and all transtively-referenced
  // protocol requirement signatures.
  RuleBuilder builder(Context, Dump);
  builder.addRequirements(requirements);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true,
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::AllowInvalidRequirements);
  checkCompletionResult(*this, result);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }
}

/// Build a requirement machine from a set of generic parameters and
/// structural requirements.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by InferredGenericSignatureRequest.
///
/// Returns failure if completion fails within the configured number of steps.
CompletionResult
RequirementMachine::initWithWrittenRequirements(
    ArrayRef<GenericTypeParamType *> genericParams,
    ArrayRef<StructuralRequirement> requirements) {
  Params.append(genericParams.begin(), genericParams.end());

  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Dump) {
    llvm::dbgs() << "Adding generic parameters:";
    for (auto *paramTy : genericParams)
      llvm::dbgs() << " " << Type(paramTy);
    llvm::dbgs() << "\n";
  }

  // Collect the top-level requirements, and all transtively-referenced
  // protocol requirement signatures.
  RuleBuilder builder(Context, Dump);
  builder.addRequirements(requirements);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true,
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::AllowInvalidRequirements);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }

  return result;
}

/// Attempt to obtain a confluent rewrite system by iterating the Knuth-Bendix
/// completion procedure together with property map construction until fixed
/// point.
CompletionResult
RequirementMachine::computeCompletion(RewriteSystem::ValidityPolicy policy) {
  while (true) {
    // First, run the Knuth-Bendix algorithm to resolve overlapping rules.
    auto result = System.computeConfluentCompletion(
        RequirementMachineStepLimit,
        RequirementMachineDepthLimit);

    if (Stats) {
      Stats->getFrontendCounters()
          .NumRequirementMachineCompletionSteps += result.second;
    }

    // Check for failure.
    if (result.first != CompletionResult::Success)
      return result.first;

    // Check invariants.
    System.verifyRewriteRules(policy);

    // Build the property map, which also performs concrete term
    // unification; if this added any new rules, run the completion
    // procedure again.
    result = Map.buildPropertyMap(
        RequirementMachineStepLimit,
        RequirementMachineDepthLimit);

    if (Stats) {
      Stats->getFrontendCounters()
        .NumRequirementMachineUnifiedConcreteTerms += result.second;
    }

    // Check for failure.
    if (result.first != CompletionResult::Success)
      return result.first;

    // If buildPropertyMap() added new rules, we run another round of
    // Knuth-Bendix, and build the property map again.
    if (result.second == 0)
      break;
  }

  if (Dump) {
    dump(llvm::dbgs());
  }

  assert(!Complete);
  Complete = true;

  return CompletionResult::Success;
}

bool RequirementMachine::isComplete() const {
  return Complete;
}

bool RequirementMachine::hadError() const {
  // FIXME: Implement other checks here
  // FIXME: Assert if hadError() is true but we didn't emit any diagnostics?
  return System.hadError();
}

void RequirementMachine::dump(llvm::raw_ostream &out) const {
  out << "Requirement machine for ";
  if (Sig)
    out << Sig;
  else if (!Params.empty()) {
    out << "fresh signature ";
    for (auto paramTy : Params)
      out << " " << Type(paramTy);
  } else {
    assert(!Protos.empty());
    out << "protocols [";
    for (auto *proto : Protos) {
      out << " " << proto->getName();
    }
    out << " ]";
  }
  out << "\n";

  System.dump(out);
  Map.dump(out);

  out << "Conformance access paths: {\n";
  for (auto pair : ConformanceAccessPaths) {
    out << "- " << pair.first.first << " : ";
    out << pair.first.second->getName() << " => ";
    pair.second.print(out);
    out << "\n";
  }
  out << "}\n";
}
