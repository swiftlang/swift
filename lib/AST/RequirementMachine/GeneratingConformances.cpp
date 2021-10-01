//===--- GeneratingConformances.cpp - Reasoning about conformance rules ---===//
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
//
// This file implements an algorithm to find a minimal set of "generating
// conformances", which are rules (V1.[P1] => V1), ..., (Vn.[Pn] => Vn) such
// that any valid term of the form T.[P] can be written as a product of terms
// (Vi.[Pi]), where each Vi.[Pi] is a left hand side of a generating
// conformance.
//
// A "conformance-valid" rewrite system is one where if we can write
// T == U.V for arbitrary non-empty U and V, then U.[domain(V)] is joinable
// with U.
//
// If this holds, then starting with a term T.[P] that is joinable with T, we
// can reduce T to canonical form T', and find the unique rule (V.[P] => V) such
// that T' == U.V. Then we repeat this process with U.[domain(V)], which is
// known to be joinable with U, since T is conformance-valid.
//
// Iterating this process produces a decomposition of T.[P] as a product of
// left hand sides of conformance rules. Some of those rules are not minimal;
// they are added by completion, or they are redundant rules written by the
// user.
//
// Using the 3-cells that generate the homotopy relation on rewrite paths,
// decompositions can be found for all "derived" conformance rules, producing
// a minimal set of generating conformances.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Defer.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// Finds all protocol conformance rules appearing in a 3-cell, both without
/// context, and with a non-empty left context. Applications of rules with a
/// non-empty right context are ignored.
void HomotopyGenerator::findProtocolConformanceRules(
    SmallVectorImpl<unsigned> &notInContext,
    SmallVectorImpl<std::pair<MutableTerm, unsigned>> &inContext,
    const RewriteSystem &system) const {

  auto redundancyCandidates = Path.findRulesAppearingOnceInEmptyContext();
  if (redundancyCandidates.empty())
    return;

  for (const auto &step : Path) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      const auto &rule = system.getRule(step.RuleID);
      if (!rule.isProtocolConformanceRule())
        break;

      if (!step.isInContext() &&
          step.Inverse &&
          std::find(redundancyCandidates.begin(),
                    redundancyCandidates.end(),
                    step.RuleID) != redundancyCandidates.end()) {
        notInContext.push_back(step.RuleID);
      }

      break;
    }

    case RewriteStep::AdjustConcreteType:
      break;
    }
  }

  if (notInContext.empty())
    return;

  if (notInContext.size() > 1) {
    llvm::errs() << "Multiple conformance rules appear once without context:\n";
    for (unsigned ruleID : notInContext)
      llvm::errs() << system.getRule(ruleID) << "\n";
    dump(llvm::errs(), system);
    llvm::errs() << "\n";
    abort();
  }

  MutableTerm term = Basepoint;

  for (const auto &step : Path) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      const auto &rule = system.getRule(step.RuleID);
      if (!rule.isProtocolConformanceRule())
        break;

      if (step.StartOffset > 0 &&
          step.EndOffset == 0 &&
          rule.getLHS().back() == system.getRule(notInContext[0]).getLHS().back()) {
        MutableTerm prefix(term.begin(), term.begin() + step.StartOffset);
        inContext.emplace_back(prefix, step.RuleID);
      }
      break;
    }

    case RewriteStep::AdjustConcreteType:
      break;
    }

    step.apply(term, system);
  }

  if (inContext.empty()) {
    notInContext.clear();
    return;
  }

  if (inContext.size() > 1) {
    llvm::errs() << "Multiple candidate conformance rules in context?\n";
    dump(llvm::errs(), system);
    llvm::errs() << "\n";
    abort();
  }
}

/// Write the term as a product of left hand sides of protocol conformance
/// rules.
///
/// The term should be irreducible, except for a protocol symbol at the end.
void
RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(
    MutableTerm term, SmallVectorImpl<unsigned> &result) const {
  assert(term.back().getKind() == Symbol::Kind::Protocol);

  // If T is canonical and T.[P] => T, then by confluence, T.[P]
  // reduces to T in a single step, via a rule V.[P] => V, where
  // T == U.V.
  RewritePath steps;
  bool simplified = simplify(term, &steps);
  if (!simplified) {
    llvm::errs() << "Term does not conform to protocol: " << term << "\n";
    abort();
  }

  assert(steps.size() == 1 &&
         "Canonical conformance term should simplify in one step");

  const auto &step = *steps.begin();
  assert(step.Kind == RewriteStep::ApplyRewriteRule);
  assert(step.EndOffset == 0);
  assert(!step.Inverse);

  // If |U| > 0, recurse with the term U.[domain(V)]. Since T is
  // canonical, we know that U is canonical as well.
  if (step.StartOffset > 0) {
    // Build the term U.
    MutableTerm prefix(term.begin(), term.begin() + step.StartOffset);

    decomposeTermIntoConformanceRuleLeftHandSides(prefix, step.RuleID, result);
  } else {
    result.push_back(step.RuleID);
  }
}

/// Given a term U and a rule (V.[P] => V), write U.[domain(V)] as a
/// product of left hand sdies of conformance rules. The term U should
/// be irreducible.
void
RewriteSystem::decomposeTermIntoConformanceRuleLeftHandSides(
    MutableTerm term, unsigned ruleID,
    SmallVectorImpl<unsigned> &result) const {
  const auto &rule = getRule(ruleID);
  assert(rule.isProtocolConformanceRule());

  // Compute domain(V).
  const auto &lhs = rule.getLHS();
  auto protocols = lhs[0].getProtocols();
  assert(protocols.size() == 1);
  auto protocol = Symbol::forProtocol(protocols[0], Context);

  // A same-type requirement of the form 'Self.Foo == Self' can induce a
  // conformance rule [P].[P] => [P], and we can end up with a generating
  // conformance decomposition of the form
  //
  //   (V.[Q] => V) := [P].(V'.[Q] => V'),
  //
  // where domain(V) == [P]. Don't recurse on [P].[P] here since it won't
  // yield anything useful, instead just return with (V'.[Q] => V').
  if (term.size() == 1 && term[0] == protocol) {
    result.push_back(ruleID);
    return;
  }

  // Build the term U.[domain(V)].
  term.add(protocol);

  decomposeTermIntoConformanceRuleLeftHandSides(term, result);

  // Add the rule V => V.[P].
  result.push_back(ruleID);
}

/// Use homotopy information to discover all ways of writing the left hand side
/// of each conformance rule as a product of left hand sides of other conformance
/// rules.
///
/// Each conformance rule (Vi.[P] => Vi) can always be written in terms of itself,
/// so the first term of each disjunction is always (Vi.[P] => Vi).
///
/// Conformance rules can also be circular, so not every choice of disjunctions
/// produces a valid result; for example, if you have these definitions:
///
///   protocol P {
///     associatedtype T : P
///   }
///
///   struct G<X, Y> where X : P, X.T == Y, Y : P, Y.T == X {}
///
/// We have three conformance rules:
///
///   [P:T].[P] => [P:T]
///   <X>.[P] => <X>
///   <Y>.[P] => <Y>
///
/// The first rule, <X>.[P] => <X> has an alternate conformance path:
///
///   (<Y>.[P]).([P:T].[P])
///
/// The second rule similarly has an alternate conformance path:
///
///   (<X>.[P]).([P:T].[P])
///
/// This gives us the following initial set of candidate conformance paths:
///
///   [P:T].[P] := ([P:T].[P])
///   <X>.[P] := (<X>.[P]) ∨ (<Y>.[P]).([P:T].[P])
///   <Y>.[P] := (<Y>.[P]) ∨ (<X>.[P]).([P:T].[P])
///
/// One valid solution is the following set of assignments:
///
///   [P:T].[P] := ([P:T].[P])
///   <X>.[P] := (<X>.[P])
///   <Y>.[P] := (<X>.[P]).([P:T].[P])
///
/// That is, we can choose to eliminate <X>.[P], but not <Y>.[P], or vice
/// versa; but it is never valid to eliminate both.
void RewriteSystem::computeCandidateConformancePaths(
    llvm::MapVector<unsigned,
                    std::vector<SmallVector<unsigned, 2>>> &conformancePaths) const {
  for (const auto &loop : HomotopyGenerators) {
    if (loop.isDeleted())
      continue;

    SmallVector<unsigned, 2> notInContext;
    SmallVector<std::pair<MutableTerm, unsigned>, 2> inContext;

    loop.findProtocolConformanceRules(notInContext, inContext, *this);

    if (notInContext.empty())
      continue;

    // We must either have multiple conformance rules in empty context, or
    // at least one conformance rule in non-empty context. Otherwise, we have
    // a conformance rule which is written as a series of same-type rules,
    // which doesn't make sense.
    assert(inContext.size() > 0 || notInContext.size() > 1);

    if (Debug.contains(DebugFlags::GeneratingConformances)) {
      llvm::dbgs() << "Candidate homotopy generator: ";
      loop.dump(llvm::dbgs(), *this);
      llvm::dbgs() << "\n";

      llvm::dbgs() << "* Conformance rules not in context:\n";
      for (unsigned ruleID : notInContext) {
        llvm::dbgs() << "- (#" << ruleID << ") " << getRule(ruleID) << "\n";
      }

      llvm::dbgs() << "* Conformance rules in context:\n";
      for (auto pair : inContext) {
        llvm::dbgs() << "- " << pair.first;
        unsigned ruleID = pair.second;
        llvm::dbgs() << " (#" << ruleID << ") " << getRule(ruleID) << "\n";
      }

      llvm::dbgs() << "\n";
    }

    // Suppose a 3-cell contains a conformance rule (T.[P] => T) in an empty
    // context, and a conformance rule (V.[P] => V) with a possibly non-empty
    // left context U and empty right context.
    //
    // We can decompose U into a product of conformance rules:
    //
    //    (V1.[P1] => V1)...(Vn.[Pn] => Vn),
    //
    // Now, we can record a candidate decomposition of (T.[P] => T) as a
    // product of conformance rules:
    //
    //    (T.[P] => T) := (V1.[P1] => V1)...(Vn.[Pn] => Vn).(V.[P] => V)
    //
    // Now if U is empty, this becomes the trivial candidate:
    //
    //    (T.[P] => T) := (V.[P] => V)
    SmallVector<SmallVector<unsigned, 2>, 2> candidatePaths;
    for (auto pair : inContext) {
      // We have a term U, and a rule V.[P] => V.
      SmallVector<unsigned, 2> conformancePath;

      // Simplify U to get U'.
      MutableTerm term = pair.first;
      (void) simplify(term);

      // Write U'.[domain(V)] as a product of left hand sides of protocol
      // conformance rules.
      decomposeTermIntoConformanceRuleLeftHandSides(term, pair.second,
                                                    conformancePath);

      candidatePaths.push_back(conformancePath);
    }

    for (unsigned candidateRuleID : notInContext) {
      // If multiple conformance rules appear in an empty context, each one
      // can be replaced with any other conformance rule.
      for (unsigned otherRuleID : notInContext) {
        if (otherRuleID == candidateRuleID)
          continue;

        SmallVector<unsigned, 2> path;
        path.push_back(otherRuleID);
        conformancePaths[candidateRuleID].push_back(path);
      }

      // If conformance rules appear in non-empty context, they define a
      // conformance access path for each conformance rule in empty context.
      for (const auto &path : candidatePaths) {
        conformancePaths[candidateRuleID].push_back(path);
      }
    }
  }
}

/// Determines if \p path can be expressed without any of the conformance
/// rules appearing in \p redundantConformances, by possibly substituting
/// any occurrences of the redundant rules with alternate definitions
/// appearing in \p conformancePaths.
///
/// The \p conformancePaths map sends conformance rules to a list of
/// disjunctions, where each disjunction is a product of other conformance
/// rules.
bool RewriteSystem::isValidConformancePath(
    llvm::SmallDenseSet<unsigned, 4> &visited,
    llvm::DenseSet<unsigned> &redundantConformances,
    const llvm::SmallVectorImpl<unsigned> &path,
    const llvm::MapVector<unsigned,
                          std::vector<SmallVector<unsigned, 2>>>
        &conformancePaths) const {
  for (unsigned ruleID : path) {
    if (visited.count(ruleID) > 0)
      return false;

    if (!redundantConformances.count(ruleID))
      continue;

    SWIFT_DEFER {
      visited.erase(ruleID);
    };
    visited.insert(ruleID);

    auto found = conformancePaths.find(ruleID);
    assert(found != conformancePaths.end());

    bool foundValidConformancePath = false;
    for (const auto &otherPath : found->second) {
      if (isValidConformancePath(visited, redundantConformances,
                                 otherPath, conformancePaths)) {
        foundValidConformancePath = true;
        break;
      }
    }

    if (!foundValidConformancePath)
      return false;
  }

  return true;
}

void RewriteSystem::dumpGeneratingConformanceEquation(
    llvm::raw_ostream &out,
    unsigned baseRuleID,
    const std::vector<SmallVector<unsigned, 2>> &paths) const {
  out << getRule(baseRuleID).getLHS() << " := ";

  bool first = true;
  for (const auto &path : paths) {
    if (!first)
      out << " ∨ ";
    else
      first = false;
    for (unsigned ruleID : path)
      out << "(" << getRule(ruleID).getLHS() << ")";
  }
}

void RewriteSystem::verifyGeneratingConformanceEquations(
    const llvm::MapVector<unsigned,
                          std::vector<SmallVector<unsigned, 2>>>
        &conformancePaths) const {
#ifndef NDEBUG
  for (const auto &pair : conformancePaths) {
    const auto &rule = getRule(pair.first);
    auto *proto = rule.getLHS().back().getProtocol();

    MutableTerm baseTerm(rule.getLHS());
    (void) simplify(baseTerm);

    for (const auto &path : pair.second) {
      const auto &otherRule = getRule(path.back());
      auto *otherProto = otherRule.getLHS().back().getProtocol();

      if (proto != otherProto) {
        llvm::errs() << "Invalid equation: ";
        dumpGeneratingConformanceEquation(llvm::errs(),
                                          pair.first, pair.second);
        llvm::errs() << "\n";
        llvm::errs() << "Mismatched conformance:\n";
        llvm::errs() << "Base rule: " << rule << "\n";
        llvm::errs() << "Final rule: " << otherRule << "\n\n";
        dump(llvm::errs());
        abort();
      }

      MutableTerm otherTerm;
      for (unsigned otherRuleID : path) {
        otherTerm.append(getRule(otherRuleID).getLHS());
      }

      (void) simplify(otherTerm);

      if (baseTerm != otherTerm) {
        llvm::errs() << "Invalid equation: ";
        llvm::errs() << "\n";
        dumpGeneratingConformanceEquation(llvm::errs(),
                                          pair.first, pair.second);
        llvm::errs() << "Invalid conformance path:\n";
        llvm::errs() << "Expected: " << baseTerm << "\n";
        llvm::errs() << "Got: " << otherTerm << "\n\n";
        dump(llvm::errs());
        abort();
      }
    }
  }
#endif
}

/// Computes a minimal set of generating conformances, assuming that homotopy
/// reduction has already eliminated all redundant rewrite rules that are not
/// conformance rules.
void RewriteSystem::computeGeneratingConformances(
    llvm::DenseSet<unsigned> &redundantConformances) {
  llvm::MapVector<unsigned, std::vector<SmallVector<unsigned, 2>>> conformancePaths;

  // Prepare the initial set of equations: every non-redundant conformance rule
  // can be expressed as itself.
  for (unsigned ruleID : indices(Rules)) {
    const auto &rule = getRule(ruleID);
    if (rule.isRedundant())
      continue;

    if (!rule.isProtocolConformanceRule())
      continue;

    SmallVector<unsigned, 2> path;
    path.push_back(ruleID);
    conformancePaths[ruleID].push_back(path);
  }

  computeCandidateConformancePaths(conformancePaths);

  if (Debug.contains(DebugFlags::GeneratingConformances)) {
    llvm::dbgs() << "Initial set of equations:\n";
    for (const auto &pair : conformancePaths) {
      llvm::dbgs() << "- ";
      dumpGeneratingConformanceEquation(llvm::dbgs(),
                                        pair.first, pair.second);
      llvm::dbgs() << "\n";
    }
  }

  verifyGeneratingConformanceEquations(conformancePaths);

  // Find a minimal set of generating conformances.
  for (const auto &pair : conformancePaths) {
    for (const auto &path : pair.second) {
      llvm::SmallDenseSet<unsigned, 4> visited;
      visited.insert(pair.first);

      if (isValidConformancePath(visited, redundantConformances,
                                 path, conformancePaths)) {
        redundantConformances.insert(pair.first);
        break;
      }
    }
  }

  // Check invariants.
#ifndef NDEBUG
  for (const auto &pair : conformancePaths) {
    if (redundantConformances.count(pair.first) > 0)
      continue;

    const auto &rule = getRule(pair.first);

    if (rule.isRedundant()) {
      llvm::errs() << "Generating conformance is redundant: ";
      llvm::errs() << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }

    if (rule.containsUnresolvedSymbols()) {
      llvm::errs() << "Generating conformance contains unresolved symbols: ";
      llvm::errs() << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }
  }
#endif

  if (Debug.contains(DebugFlags::GeneratingConformances)) {
    llvm::dbgs() << "Generating conformances:\n";

    for (const auto &pair : conformancePaths) {
      if (redundantConformances.count(pair.first) > 0)
        continue;

      llvm::dbgs() << "- " << getRule(pair.first) << "\n";
    }
  }
}