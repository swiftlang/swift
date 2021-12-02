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

void RequirementMachine::verify(const MutableTerm &term) const {
#ifndef NDEBUG
  // If the term is in the generic parameter domain, ensure we have a valid
  // generic parameter.
  if (term.begin()->getKind() == Symbol::Kind::GenericParam) {
    auto *genericParam = term.begin()->getGenericParam();
    TypeArrayView<GenericTypeParamType> genericParams = getGenericParams();
    auto found = std::find(genericParams.begin(),
                           genericParams.end(),
                           genericParam);
    if (found == genericParams.end()) {
      llvm::errs() << "Bad generic parameter in " << term << "\n";
      dump(llvm::errs());
      abort();
    }
  }

  MutableTerm erased;

  // First, "erase" resolved associated types from the term, and try
  // to simplify it again.
  for (auto symbol : term) {
    if (erased.empty()) {
      switch (symbol.getKind()) {
      case Symbol::Kind::Protocol:
      case Symbol::Kind::GenericParam:
        erased.add(symbol);
        continue;

      case Symbol::Kind::AssociatedType:
        erased.add(Symbol::forProtocol(symbol.getProtocols()[0], Context));
        break;

      case Symbol::Kind::Name:
      case Symbol::Kind::Layout:
      case Symbol::Kind::Superclass:
      case Symbol::Kind::ConcreteType:
        llvm::errs() << "Bad initial symbol in " << term << "\n";
        abort();
        break;
      }
    }

    switch (symbol.getKind()) {
    case Symbol::Kind::Name:
      assert(!erased.empty());
      erased.add(symbol);
      break;

    case Symbol::Kind::AssociatedType:
      erased.add(Symbol::forName(symbol.getName(), Context));
      break;

    case Symbol::Kind::Protocol:
    case Symbol::Kind::GenericParam:
    case Symbol::Kind::Layout:
    case Symbol::Kind::Superclass:
    case Symbol::Kind::ConcreteType:
      llvm::errs() << "Bad interior symbol " << symbol << " in " << term << "\n";
      abort();
      break;
    }
  }

  MutableTerm simplified = erased;
  System.simplify(simplified);

  // We should end up with the same term.
  if (simplified != term) {
    llvm::errs() << "Term verification failed\n";
    llvm::errs() << "Initial term:    " << term << "\n";
    llvm::errs() << "Erased term:     " << erased << "\n";
    llvm::errs() << "Simplified term: " << simplified << "\n";
    llvm::errs() << "\n";
    dump(llvm::errs());
    abort();
  }
#endif
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

/// Build a requirement machine for the requirements of a generic signature.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by ASTContext::getOrCreateRequirementMachine().
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

  computeCompletion(RewriteSystem::DisallowInvalidRequirements);

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
void RequirementMachine::initWithProtocols(ArrayRef<const ProtocolDecl *> protos) {
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

  // FIXME: Only if the protocols were written in source, though.
  computeCompletion(RewriteSystem::AllowInvalidRequirements);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }
}

/// Build a requirement machine from a set of generic parameters and
/// (possibly non-canonical or non-minimal) abstract requirements.
///
/// This must only be called exactly once, before any other operations are
/// performed on this requirement machine.
///
/// Used by AbstractGenericSignatureRequest.
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

  computeCompletion(RewriteSystem::AllowInvalidRequirements);

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
void RequirementMachine::initWithWrittenRequirements(
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

  computeCompletion(RewriteSystem::AllowInvalidRequirements);

  if (Dump) {
    llvm::dbgs() << "}\n";
  }
}

/// Attempt to obtain a confluent rewrite system by iterating the Knuth-Bendix
/// completion procedure together with property map construction until fixed
/// point.
void RequirementMachine::computeCompletion(RewriteSystem::ValidityPolicy policy) {
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
    auto checkCompletionResult = [&]() {
      switch (result.first) {
      case CompletionResult::Success:
        break;

      case CompletionResult::MaxIterations:
        llvm::errs() << "Generic signature " << Sig
                     << " exceeds maximum completion step count\n";
        System.dump(llvm::errs());
        abort();

      case CompletionResult::MaxDepth:
        llvm::errs() << "Generic signature " << Sig
                     << " exceeds maximum completion depth\n";
        System.dump(llvm::errs());
        abort();
      }
    };

    checkCompletionResult();

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

    checkCompletionResult();

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
}

bool RequirementMachine::isComplete() const {
  return Complete;
}
