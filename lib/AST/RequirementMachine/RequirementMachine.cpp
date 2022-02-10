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
  MaxRuleCount = langOpts.RequirementMachineMaxRuleCount;
  MaxRuleLength = langOpts.RequirementMachineMaxRuleLength;
  MaxConcreteNesting = langOpts.RequirementMachineMaxConcreteNesting;
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

  case CompletionResult::MaxRuleCount:
    llvm::errs() << "Rewrite system exceeded maximum rule count\n";
    machine.dump(llvm::errs());
    abort();

  case CompletionResult::MaxRuleLength:
    llvm::errs() << "Rewrite system exceeded rule length limit\n";
    machine.dump(llvm::errs());
    abort();

  case CompletionResult::MaxConcreteNesting:
    llvm::errs() << "Rewrite system exceeded concrete type nesting depth limit\n";
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
  RuleBuilder builder(Context, System.getProtocolMap());
  builder.addRequirements(sig.getRequirements());

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/false,
                    /*protos=*/ArrayRef<const ProtocolDecl *>(),
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::DisallowInvalidRequirements);
  checkCompletionResult(*this, result.first);

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
std::pair<CompletionResult, unsigned>
RequirementMachine::initWithProtocols(ArrayRef<const ProtocolDecl *> protos) {
  FrontendStatsTracer tracer(Stats, "build-rewrite-system");

  if (Dump) {
    llvm::dbgs() << "Adding protocols";
    for (auto *proto : protos) {
      llvm::dbgs() << " " << proto->getName();
    }
    llvm::dbgs() << " {\n";
  }

  RuleBuilder builder(Context, System.getProtocolMap());
  builder.addProtocols(protos);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true, protos,
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
  RuleBuilder builder(Context, System.getProtocolMap());
  builder.addRequirements(requirements);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true,
                    /*protos=*/ArrayRef<const ProtocolDecl *>(),
                    std::move(builder.PermanentRules),
                    std::move(builder.RequirementRules));

  auto result = computeCompletion(RewriteSystem::AllowInvalidRequirements);
  checkCompletionResult(*this, result.first);

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
std::pair<CompletionResult, unsigned>
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
  RuleBuilder builder(Context, System.getProtocolMap());
  builder.addRequirements(requirements);

  // Add the initial set of rewrite rules to the rewrite system.
  System.initialize(/*recordLoops=*/true,
                    /*protos=*/ArrayRef<const ProtocolDecl *>(),
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
///
/// Returns a pair where the first element is the status. If the status is not
/// CompletionResult::Success, the second element of the pair is the rule ID
/// which triggered failure.
std::pair<CompletionResult, unsigned>
RequirementMachine::computeCompletion(RewriteSystem::ValidityPolicy policy) {
  while (true) {
    {
      unsigned ruleCount = System.getRules().size();

      // First, run the Knuth-Bendix algorithm to resolve overlapping rules.
      auto result = System.computeConfluentCompletion(MaxRuleCount, MaxRuleLength);

      unsigned rulesAdded = (System.getRules().size() - ruleCount);

      if (Stats) {
        Stats->getFrontendCounters()
            .NumRequirementMachineCompletionSteps += rulesAdded;
      }

      // Check for failure.
      if (result.first != CompletionResult::Success)
        return result;

      // Check invariants.
      System.verifyRewriteRules(policy);
    }

    {
      unsigned ruleCount = System.getRules().size();

      // Build the property map, which also performs concrete term
      // unification; if this added any new rules, run the completion
      // procedure again.
      Map.buildPropertyMap();

      unsigned rulesAdded = (System.getRules().size() - ruleCount);

      if (Stats) {
        Stats->getFrontendCounters()
          .NumRequirementMachineUnifiedConcreteTerms += rulesAdded;
      }

      // Check new rules added by the property map against configured limits.
      for (unsigned i = 0; i < rulesAdded; ++i) {
        const auto &newRule = System.getRule(ruleCount + i);
        if (newRule.getDepth() > MaxRuleLength) {
          return std::make_pair(CompletionResult::MaxRuleLength,
                                ruleCount + i);
        }
        if (newRule.getNesting() > MaxConcreteNesting) {
          return std::make_pair(CompletionResult::MaxConcreteNesting,
                                ruleCount + i);
        }
      }

      if (System.getRules().size() > MaxRuleCount) {
        return std::make_pair(CompletionResult::MaxRuleCount,
                              System.getRules().size() - 1);
      }

      // If buildPropertyMap() didn't add any new rules, we are done.
      if (rulesAdded == 0)
        break;
    }
  }

  if (Dump) {
    dump(llvm::dbgs());
  }

  assert(!Complete);
  Complete = true;

  return std::make_pair(CompletionResult::Success, 0);
}

std::string RequirementMachine::getRuleAsStringForDiagnostics(
    unsigned ruleID) const {
  const auto &rule = System.getRule(ruleID);

  std::string result;
  llvm::raw_string_ostream out(result);
  out << rule;
  return out.str();
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
    out << "fresh signature <";
    for (auto paramTy : Params)
      out << " " << Type(paramTy);
    out << " >";
  } else {
    auto protos = System.getProtocols();
    assert(!protos.empty());
    out << "protocols [";
    for (auto *proto : protos) {
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
