//===--- HomotopyReduction.cpp - Higher-dimensional term rewriting --------===//
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
// This file implements the algorithm for computing a minimal set of rules from
// a confluent rewrite system. A minimal set of rules is:
//
// 1) Large enough that computing the confluent completion produces the original
//    rewrite system;
//
// 2) Small enough that no further rules can be deleted without changing the
//    resulting confluent rewrite system.
//
// Redundant rules that are not part of the minimal set are redundant are
// detected by analyzing the set of rewrite loops computed by the completion
// procedure. See RewriteLoop.cpp for a discussion of rewrite loops.
//
// If a rewrite rule appears exactly once in a loop and without context, the
// loop witnesses a redundancy; the rewrite rule is equivalent to traveling
// around the loop "in the other direction". This rewrite rule and the
// corresponding rewrite loop can be deleted.
//
// Any occurrence of the rule in the remaining loops is replaced with the
// alternate definition obtained by splitting the loop that witnessed the
// redundancy.
//
// Iterating this process eventually produces a minimal set of rewrite rules.
//
// For a description of the general algorithm, see "A Homotopical Completion
// Procedure with Applications to Coherence of Monoids",
// https://hal.inria.fr/hal-00818253.
//
// Note that in the world of Swift, rewrite rules for introducing associated
// type symbols are marked 'permanent'; they are always re-added when a new
// rewrite system is built from a minimal generic signature, so instead of
// deleting them it is better to leave them in place in case it allows other
// rules to be deleted instead.
//
// Also, for a conformance rule (V.[P] => V) to be redundant, a stronger
// condition is needed than appearing once in a loop and without context;
// the rule must not be a _minimal conformance_. The algorithm for computing
// minimal conformances is implemented in MinimalConformances.cpp.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Type.h"
#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// Recompute RulesInEmptyContext and DecomposeCount if needed.
void RewriteLoop::recompute(const RewriteSystem &system) {
  if (!Dirty)
    return;
  Dirty = 0;

  ProjectionCount = 0;
  DecomposeCount = 0;

  // Rules appearing in empty context (possibly more than once).
  llvm::SmallDenseSet<unsigned, 2> rulesInEmptyContext;

  // The number of times each rule appears (with or without context).
  llvm::SmallDenseMap<unsigned, unsigned, 2> ruleMultiplicity;

  RewritePathEvaluator evaluator(Basepoint);

  for (auto step : Path) {
    switch (step.Kind) {
    case RewriteStep::Rule: {
      if (!step.isInContext() && !evaluator.isInContext())
        rulesInEmptyContext.insert(step.getRuleID());

      ++ruleMultiplicity[step.getRuleID()];
      break;
    }

    case RewriteStep::LeftConcreteProjection:
      ++ProjectionCount;
      break;

    case RewriteStep::Decompose:
      ++DecomposeCount;
      break;

    case RewriteStep::PrefixSubstitutions:
    case RewriteStep::Shift:
    case RewriteStep::Relation:
    case RewriteStep::DecomposeConcrete:
    case RewriteStep::RightConcreteProjection:
      break;
    }

    evaluator.apply(step, system);
  }

  RulesInEmptyContext.clear();

  // Collect all rules that we saw exactly once in empty context.
  for (auto rule : rulesInEmptyContext) {
    auto found = ruleMultiplicity.find(rule);
    assert(found != ruleMultiplicity.end());

    if (found->second == 1)
      RulesInEmptyContext.push_back(rule);
  }
}

/// A rewrite rule is redundant if it appears exactly once in a loop
/// without context.
ArrayRef<unsigned>
RewriteLoop::findRulesAppearingOnceInEmptyContext(
    const RewriteSystem &system) const {
  const_cast<RewriteLoop *>(this)->recompute(system);
  return RulesInEmptyContext;
}

/// The number of LeftConcreteProjection steps, used by the elimination order to
/// prioritize loops that are not concrete unification projections.
unsigned RewriteLoop::getProjectionCount(
    const RewriteSystem &system) const {
  const_cast<RewriteLoop *>(this)->recompute(system);
  return ProjectionCount;
}

/// The number of Decompose steps, used by the elimination order to prioritize
/// loops that are not concrete simplifications.
unsigned RewriteLoop::getDecomposeCount(
    const RewriteSystem &system) const {
  const_cast<RewriteLoop *>(this)->recompute(system);
  return DecomposeCount;
}

/// If a rewrite loop contains an explicit rule in empty context, propagate the
/// explicit bit to all other rules appearing in empty context within the same
/// loop.
///
/// When computing minimal conformances we prefer to eliminate non-explicit
/// rules, as a heuristic to ensure that minimized conformance requirements
/// remain in the same protocol as originally written, in cases where they can
/// be moved between protocols.
///
/// However, conformance rules can also be written in a non-canonical way.
///
/// Most conformance requirements are non-canonical, since the original
/// requirements use unresolved types. For example, a requirement 'Self.X.Y : Q'
/// inside a protocol P will lower to a rewrite rule
///
///    [P].X.Y.[Q] => [P].X.Y
///
/// Completion will then add a new rule that looks something like this, using
/// associated type symbols:
///
///    [P:X].[P2:Y].[Q] => [P:X].[P2:Y]
///
/// Furthermore, if [P:X].[P2:Y] simplies to some other term, such as [P:Z],
/// there will be yet another rule added by completion:
///
///    [P:Z].[Q] => [P:Z]
///
/// The new rules are related to the original rule via rewrite loops where
/// both rules appear in empty context. This algorithm will propagate the
/// explicit bit from the original rule to the canonical rule.
void RewriteSystem::propagateExplicitBits() {
  for (const auto &loop : Loops) {
    auto rulesInEmptyContext =
      loop.findRulesAppearingOnceInEmptyContext(*this);

    bool sawExplicitRule = false;

    for (unsigned ruleID : rulesInEmptyContext) {
      const auto &rule = getRule(ruleID);
      if (rule.isExplicit())
        sawExplicitRule = true;
    }
    if (sawExplicitRule) {
      for (unsigned ruleID : rulesInEmptyContext) {
        auto &rule = getRule(ruleID);
        if (!rule.isPermanent() && !rule.isExplicit())
          rule.markExplicit();
      }
    }
  }
}

/// After propagating the 'explicit' bit on rules, process pairs of
/// conflicting rules, marking one or both of the rules as conflicting,
/// which instructs minimization to drop them.
void RewriteSystem::processConflicts() {
  for (auto pair : ConflictingRules) {
    auto existingRuleID = pair.first;
    auto newRuleID = pair.second;

    auto *existingRule = &getRule(existingRuleID);
    auto *newRule = &getRule(newRuleID);

    auto existingKind = existingRule->isPropertyRule()->getKind();
    auto newKind = newRule->isPropertyRule()->getKind();

    // The GSB preferred to drop an explicit rule in a conflict, but
    // only if the kinds were the same.
    if (existingRule->isExplicit() && !newRule->isExplicit() &&
        existingKind == newKind) {
      std::swap(existingRule, newRule);
    }

    if (newRule->getRHS().size() >= existingRule->getRHS().size()) {
      newRule->markConflicting();
    } else if (existingKind != Symbol::Kind::Superclass &&
               existingKind == newKind) {
      // The GSB only dropped the new rule in the case of a conflicting
      // superclass requirement, so maintain that behavior here.
      if (existingRule->getRHS().size() >= newRule->getRHS().size())
        existingRule->markConflicting();
    }

    // FIXME: Diagnose the conflict later.
  }
}

/// Given a rewrite rule which appears exactly once in a loop
/// without context, return a new definition for this rewrite rule.
/// The new definition is the path obtained by deleting the
/// rewrite rule from the loop.
RewritePath RewritePath::splitCycleAtRule(unsigned ruleID) const {
  // A cycle is a path from the basepoint to the basepoint.
  // Somewhere in this path, an application of \p ruleID
  // appears in an empty context.

  // First, we split the cycle into two paths:
  //
  // (1) A path from the basepoint to the rule's
  // left hand side,
  RewritePath basepointToLhs;
  // (2) And a path from the rule's right hand side
  // to the basepoint.
  RewritePath rhsToBasepoint;

  // Because the rule only appears once, we know that basepointToLhs
  // and rhsToBasepoint do not involve the rule itself.

  // If the rule is inverted, we have to invert the whole thing
  // again at the end.
  bool ruleWasInverted = false;

  bool sawRule = false;

  for (auto step : Steps) {
    switch (step.Kind) {
    case RewriteStep::Rule: {
      if (step.getRuleID() != ruleID)
        break;

      assert(!sawRule && "Rule appears more than once?");
      assert(!step.isInContext() && "Rule appears in context?");

      ruleWasInverted = step.Inverse;
      sawRule = true;
      continue;
    }
    case RewriteStep::PrefixSubstitutions:
    case RewriteStep::Shift:
    case RewriteStep::Decompose:
    case RewriteStep::Relation:
    case RewriteStep::DecomposeConcrete:
    case RewriteStep::LeftConcreteProjection:
    case RewriteStep::RightConcreteProjection:
      break;
    }

    if (sawRule)
      rhsToBasepoint.add(step);
    else
      basepointToLhs.add(step);
  }

  // Build a path from the rule's lhs to the rule's rhs via the
  // basepoint.
  RewritePath result = rhsToBasepoint;
  result.append(basepointToLhs);

  // We want a path from the lhs to the rhs, so invert it unless
  // the rewrite step was also inverted.
  if (!ruleWasInverted)
    result.invert();

  return result;
}

/// Replace every rewrite step involving the given rewrite rule with
/// either the replacement path (or its inverse, if the step was
/// inverted).
///
/// The replacement path is re-contextualized at each occurrence of a
/// rewrite step involving the given rule.
///
/// Returns true if any rewrite steps were replaced; false means the
/// rule did not appear in this path.
bool RewritePath::replaceRuleWithPath(unsigned ruleID,
                                      const RewritePath &path) {
  bool foundAny = false;

  for (const auto &step : Steps) {
    if (step.Kind == RewriteStep::Rule &&
        step.getRuleID() == ruleID) {
      foundAny = true;
      break;
    }
  }

  if (!foundAny)
    return false;

  SmallVector<RewriteStep, 4> newSteps;

  for (const auto &step : Steps) {
    switch (step.Kind) {
    case RewriteStep::Rule: {
      // All other rewrite rules remain unchanged.
      if (step.getRuleID() != ruleID) {
        newSteps.push_back(step);
        break;
      }

      // Ok, we found a rewrite step referencing the redundant rule.
      // Replace this step with the provided path. If this rewrite step has
      // context, the path's own steps must be re-contextualized.

      // Keep track of rewrite step pairs which push and pop the stack. Any
      // rewrite steps enclosed with a push/pop are not re-contextualized.
      unsigned pushCount = 0;

      auto recontextualizeStep = [&](RewriteStep newStep) {
        bool inverse = newStep.Inverse ^ step.Inverse;

        if (newStep.pushesTermsOnStack() && inverse) {
          assert(pushCount > 0);
          --pushCount;
        }

        if (pushCount == 0) {
          newStep.StartOffset += step.StartOffset;
          newStep.EndOffset += step.EndOffset;
        }

        newStep.Inverse = inverse;
        newSteps.push_back(newStep);

        if (newStep.pushesTermsOnStack() && !inverse) {
          ++pushCount;
        }
      };

      // If this rewrite step is inverted, invert the entire path.
      if (step.Inverse) {
        for (auto newStep : llvm::reverse(path))
          recontextualizeStep(newStep);
      } else {
        for (auto newStep : path)
          recontextualizeStep(newStep);
      }

      // Rewrite steps which push and pop the stack must come in balanced pairs.
      assert(pushCount == 0);

      break;
    }
    case RewriteStep::PrefixSubstitutions:
    case RewriteStep::Shift:
    case RewriteStep::Decompose:
    case RewriteStep::Relation:
    case RewriteStep::DecomposeConcrete:
    case RewriteStep::LeftConcreteProjection:
    case RewriteStep::RightConcreteProjection:
      newSteps.push_back(step);
      break;
    }
  }

  std::swap(newSteps, Steps);
  return true;
}

/// Find a rule to delete by looking through all loops for rewrite rules appearing
/// once in empty context. Returns a pair consisting of a loop ID and a rule ID,
/// otherwise returns None.
///
/// Minimization performs three passes over the rewrite system.
///
/// 1) First, rules that are not conformance rules are deleted, with
///    \p redundantConformances equal to nullptr.
///
/// 2) Second, minimal conformances are computed.
///
/// 3) Finally, redundant conformance rules are deleted, with
/// \p redundantConformances equal to the set of conformance rules that are
///    not minimal conformances.
Optional<std::pair<unsigned, unsigned>> RewriteSystem::
findRuleToDelete(llvm::function_ref<bool(unsigned)> isRedundantRuleFn) {
  SmallVector<std::pair<unsigned, unsigned>, 2> redundancyCandidates;
  for (unsigned loopID : indices(Loops)) {
    auto &loop = Loops[loopID];
    if (loop.isDeleted())
      continue;

    bool foundAny = false;
    for (unsigned ruleID : loop.findRulesAppearingOnceInEmptyContext(*this)) {
      redundancyCandidates.emplace_back(loopID, ruleID);
      foundAny = true;
    }

    // Delete loops that don't contain any rewrite rules in empty context,
    // since such loops do not give us useful information.
    if (!foundAny)
      loop.markDeleted();
  }

  Optional<std::pair<unsigned, unsigned>> found;

  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    llvm::dbgs() << "\n";
  }

  for (const auto &pair : redundancyCandidates) {
    unsigned ruleID = pair.second;
    const auto &rule = getRule(ruleID);

    // We should not find a rule that has already been marked redundant
    // here; it should have already been replaced with a rewrite path
    // in all homotopy generators.
    assert(!rule.isRedundant());

    // Associated type introduction rules are 'permanent'. They're
    // not worth eliminating since they are re-added every time; it
    // is better to find other candidates to eliminate in the same
    // loop instead.
    if (rule.isPermanent())
      continue;

    // Homotopy reduction runs multiple passes with different filters to
    // prioritize the deletion of certain rules ahead of others. Apply
    // the filter now.
    if (!isRedundantRuleFn(ruleID)) {
      if (Debug.contains(DebugFlags::HomotopyReductionDetail)) {
        llvm::dbgs() << "** Skipping rule " << rule << " from loop #"
                     << pair.first << "\n";
      }

      continue;
    }

    if (Debug.contains(DebugFlags::HomotopyReductionDetail)) {
      llvm::dbgs() << "** Candidate rule " << rule << " from loop #"
                   << pair.first << "\n";
    }

    if (!found) {
      found = pair;
      continue;
    }

    // 'rule' is the candidate rule; 'otherRule' is the best rule to eliminate
    // we've found so far.
    const auto &otherRule = getRule(found->second);

    const auto &loop = Loops[pair.first];
    const auto &otherLoop = Loops[found->first];

    {
      // If one of the rules was a concrete unification projection, prefer to
      // eliminate the *other* rule.
      //
      // For example, if 'X.T == G<U, V>' is implied by the conformance on X,
      // and the following three rules are defined in the current protocol:
      //
      //    a) X.T == G<Int, W>
      //    b) X.U == Int
      //    c) X.V == W
      //
      // Then we can either eliminate a) alone, or b) and c). Since b) and c)
      // are projections, they are "simpler", and we would rather keep both and
      // eliminate a).
      unsigned projectionCount = loop.getProjectionCount(*this);
      unsigned otherProjectionCount = otherLoop.getProjectionCount(*this);

      if (projectionCount != otherProjectionCount) {
        if (projectionCount < otherProjectionCount)
          found = pair;

        continue;
      }
    }

    {
      // If one of the rules is a concrete type requirement, prefer to
      // eliminate the *other* rule.
      bool ruleIsConcrete = rule.getLHS().back().hasSubstitutions();
      bool otherRuleIsConcrete = otherRule.getLHS().back().hasSubstitutions();

      if (ruleIsConcrete != otherRuleIsConcrete) {
        if (otherRuleIsConcrete)
          found = pair;

        continue;
      }
    }

    {
      // If both are concrete type requirements, prefer to eliminate the
      // one with the more deeply nested type.
      unsigned ruleNesting = rule.getNesting();
      unsigned otherRuleNesting = otherRule.getNesting();

      if (ruleNesting != otherRuleNesting) {
        if (ruleNesting > otherRuleNesting)
          found = pair;

        continue;
      }
    }

    {
      // Otherwise, perform a shortlex comparison on (LHS, RHS).
      Optional<int> comparison = rule.compare(otherRule, Context);

      if (!comparison.hasValue()) {
        // Two rules (T.[C] => T) and (T.[C'] => T) are incomparable if
        // C and C' are superclass, concrete type or concrete conformance
        // symbols.
        continue;
      }

      if (*comparison == 0) {
        // Given two rewrite loops that both eliminate the same rule, prefer
        // the one that was not recorded by substitution simplification;
        // substitution simplification rules contain the projections in
        // context, which then prevents the projections from being eliminated.
        //
        // An example is if you have two rules implied by conformances on X,
        //
        //     a) X.T == G<Y>
        //     b) X.T == G<Z>
        //
        // then the induced rule Y == Z is a projection.
        //
        // The rule X.T == G<Z> can be eliminated with a loop that begins at
        // X.T.[concrete: G<Y>] followed by a decomposition and rewrite of
        // Y into Z, finally followed by an inverse decomposition back to
        // X.T.[concrete: G<Z>].
        //
        // However, if we can eliminate G<Y> via some other loop, we prefer
        // to do that, since that might *also* allow us to eliminate Y == Z.
        unsigned decomposeCount = loop.getDecomposeCount(*this);
        unsigned otherDecomposeCount = otherLoop.getDecomposeCount(*this);

        if (decomposeCount != otherDecomposeCount) {
          if (decomposeCount < otherDecomposeCount)
            found = pair;

          continue;
        }
      }

      if (*comparison > 0) {
        // Otherwise, if the new rule is less canonical than the best one so
        // far, it becomes the new candidate for elimination.
        found = pair;
        continue;
      }
    }
  }

  return found;
}

/// Delete a rewrite rule that is known to be redundant, replacing all
/// occurrences of the rule in all loops with the replacement path.
void RewriteSystem::deleteRule(unsigned ruleID,
                               const RewritePath &replacementPath) {
  // Replace all occurrences of the rule with the replacement path in
  // all redundant rule paths recorded so far.
  for (auto &pair : RedundantRules) {
    (void) pair.second.replaceRuleWithPath(ruleID, replacementPath);
  }

  // Replace all occurrences of the rule with the replacement path in
  // all remaining rewrite loops.
  for (unsigned loopID : indices(Loops)) {
    auto &loop = Loops[loopID];
    if (loop.isDeleted())
      continue;

    bool changed = loop.Path.replaceRuleWithPath(ruleID, replacementPath);
    if (!changed)
      continue;

    // The loop's path has changed, so we must invalidate the cached
    // result of findRulesAppearingOnceInEmptyContext().
    loop.markDirty();

    if (Debug.contains(DebugFlags::HomotopyReductionDetail)) {
      llvm::dbgs() << "** Updated loop #" << loopID << ": ";
      loop.dump(llvm::dbgs(), *this);
      llvm::dbgs() << "\n";
    }
  }

  // Record the redundant rule along with its replacement path.
  RedundantRules.emplace_back(ruleID, replacementPath);
}

void RewriteSystem::performHomotopyReduction(
    llvm::function_ref<bool(unsigned)> isRedundantRuleFn) {
  while (true) {
    auto optPair = findRuleToDelete(isRedundantRuleFn);

    // If no redundant rules remain which can be eliminated by this pass, stop.
    if (!optPair)
      return;

    unsigned loopID = optPair->first;
    unsigned ruleID = optPair->second;

    auto &loop = Loops[loopID];
    auto replacementPath = loop.Path.splitCycleAtRule(ruleID);

    loop.markDeleted();

    auto &rule = getRule(ruleID);

    if (Debug.contains(DebugFlags::HomotopyReduction)) {
      llvm::dbgs() << "** Deleting rule " << rule << " from loop #"
                   << loopID << "\n";
      llvm::dbgs() << "* Replacement path: ";
      MutableTerm mutTerm(getRule(ruleID).getLHS());
      replacementPath.dump(llvm::dbgs(), mutTerm, *this);
      llvm::dbgs() << "\n";
    }

    rule.markRedundant();

    deleteRule(ruleID, replacementPath);
  }
}

/// Use the loops to delete redundant rewrite rules via a series of Tietze
/// transformations, updating and simplifying existing loops as each rule
/// is deleted.
///
/// Redundant rules are mutated to set their isRedundant() bit.
void RewriteSystem::minimizeRewriteSystem() {
  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    llvm::dbgs() << "-----------------------------\n";
    llvm::dbgs() << "- Minimizing rewrite system -\n";
    llvm::dbgs() << "-----------------------------\n";
  }

  assert(Complete);
  assert(!Minimized);
  Minimized = 1;

  propagateExplicitBits();
  processConflicts();

  // First pass:
  // - Eliminate all LHS-simplified non-conformance rules.
  // - Eliminate all RHS-simplified and substitution-simplified rules.
  // - Eliminate all rules with unresolved symbols.
  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    llvm::dbgs() << "---------------------------------------------\n";
    llvm::dbgs() << "First pass: simplified and unresolved rules -\n";
    llvm::dbgs() << "---------------------------------------------\n";
  }

  performHomotopyReduction([&](unsigned ruleID) -> bool {
    const auto &rule = getRule(ruleID);

    if (rule.isLHSSimplified() &&
        !rule.isAnyConformanceRule())
      return true;

    if (rule.isRHSSimplified() ||
        rule.isSubstitutionSimplified())
      return true;

    if (rule.containsUnresolvedSymbols() &&
        !rule.isProtocolTypeAliasRule())
      return true;

    return false;
  });

  // Now compute a set of minimal conformances.
  //
  // FIXME: For now this just produces a set of redundant conformances, but
  // it should actually output the canonical minimal conformance equation
  // for each non-minimal conformance. We can then use information to
  // compute conformance access paths, instead of the current "brute force"
  // algorithm used for that purpose.
  llvm::DenseSet<unsigned> redundantConformances;
  computeMinimalConformances(redundantConformances);

  // Second pass: Eliminate all non-minimal conformance rules.
  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    llvm::dbgs() << "--------------------------------------------\n";
    llvm::dbgs() << "Second pass: non-minimal conformance rules -\n";
    llvm::dbgs() << "--------------------------------------------\n";
  }

  performHomotopyReduction([&](unsigned ruleID) -> bool {
    const auto &rule = getRule(ruleID);

    if (rule.isAnyConformanceRule() &&
        redundantConformances.count(ruleID))
      return true;

    return false;
  });

  // Third pass: Eliminate all other redundant non-conformance rules.
  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    llvm::dbgs() << "---------------------------------------\n";
    llvm::dbgs() << "Third pass: all other redundant rules -\n";
    llvm::dbgs() << "---------------------------------------\n";
  }

  performHomotopyReduction([&](unsigned ruleID) -> bool {
    const auto &rule = getRule(ruleID);

    if (!rule.isAnyConformanceRule())
      return true;

    return false;
  });

  // Check invariants after homotopy reduction.
  verifyRewriteLoops();
  verifyRedundantConformances(redundantConformances);
  verifyMinimizedRules(redundantConformances);

  if (Debug.contains(DebugFlags::RedundantRules)) {
    llvm::dbgs() << "\nRedundant rules:\n";
    for (const auto &pair : RedundantRules) {
      const auto &rule = getRule(pair.first);
      llvm::dbgs() << "- " << rule << " ::== ";

      MutableTerm lhs(rule.getLHS());
      pair.second.dump(llvm::dbgs(), lhs, *this);

      llvm::dbgs() << "\n";

      if (Debug.contains(DebugFlags::RedundantRulesDetail)) {
        llvm::dbgs() << "\n";
        pair.second.dumpLong(llvm::dbgs(), lhs, *this);

        llvm::dbgs() << "\n\n";
      }
    }
  }
}

/// In a conformance-valid rewrite system, any rule with unresolved symbols on
/// the left or right hand side should be redundant. The presence of unresolved
/// non-redundant rules means one of the original requirements written by the
/// user was invalid.
bool RewriteSystem::hadError() const {
  assert(Complete);
  assert(Minimized);

  for (const auto &rule : Rules) {
    if (!isInMinimizationDomain(rule.getLHS().getRootProtocol()))
      continue;

    if (rule.isPermanent())
      continue;

    if (rule.isConflicting())
      return true;

    if (!rule.isRedundant() &&
        !rule.isProtocolTypeAliasRule() &&
        rule.containsUnresolvedSymbols())
      return true;
  }

  return false;
}

/// Collect all non-permanent, non-redundant rules whose domain is equal to
/// one of the protocols in the connected component represented by this
/// rewrite system.
///
/// These rules form the requirement signatures of these protocols.
llvm::DenseMap<const ProtocolDecl *, RewriteSystem::MinimizedProtocolRules>
RewriteSystem::getMinimizedProtocolRules() const {
  assert(Minimized);
  assert(!Protos.empty());

  llvm::DenseMap<const ProtocolDecl *, MinimizedProtocolRules> rules;
  for (unsigned ruleID : indices(Rules)) {
    const auto &rule = getRule(ruleID);

    if (rule.isPermanent() ||
        rule.isRedundant() ||
        rule.isConflicting())
      continue;

    const auto *proto = rule.getLHS().getRootProtocol();
    if (!isInMinimizationDomain(proto))
      continue;

    if (rule.isProtocolTypeAliasRule())
      rules[proto].TypeAliases.push_back(ruleID);
    else if (!rule.containsUnresolvedSymbols())
      rules[proto].Requirements.push_back(ruleID);
  }

  return rules;
}

/// Collect all non-permanent, non-redundant rules whose left hand side
/// begins with a generic parameter symbol.
///
/// These rules form the top-level generic signature for this rewrite system.
std::vector<unsigned>
RewriteSystem::getMinimizedGenericSignatureRules() const {
  assert(Minimized);
  assert(Protos.empty());

  std::vector<unsigned> rules;
  for (unsigned ruleID : indices(Rules)) {
    const auto &rule = getRule(ruleID);

    if (rule.isPermanent() ||
        rule.isRedundant() ||
        rule.isConflicting() ||
        rule.containsUnresolvedSymbols()) {
      continue;
    }

    if (rule.getLHS()[0].getKind() != Symbol::Kind::GenericParam)
      continue;

    rules.push_back(ruleID);
  }

  return rules;
}

/// Verify that each loop begins and ends at its basepoint.
void RewriteSystem::verifyRewriteLoops() const {
  for (const auto &loop : Loops) {
    loop.verify(*this);
  }
}

/// Assert if homotopy reduction failed to eliminate a redundant conformance,
/// since this suggests a misunderstanding on my part.
void RewriteSystem::verifyRedundantConformances(
    const llvm::DenseSet<unsigned> &redundantConformances) const {
  for (unsigned ruleID : redundantConformances) {
    const auto &rule = getRule(ruleID);
    assert(!rule.isPermanent() &&
           "Permanent rule cannot be redundant");
    assert(!rule.isIdentityConformanceRule() &&
           "Identity conformance cannot be redundant");
    assert(rule.isAnyConformanceRule() &&
           "Redundant conformance is not a conformance rule?");

    if (!rule.isRedundant()) {
      llvm::errs() << "Homotopy reduction did not eliminate redundant "
                   << "conformance?\n";
      llvm::errs() << "(#" << ruleID << ") " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }
  }
}

// Assert if homotopy reduction failed to eliminate a rewrite rule it was
// supposed to delete.
void RewriteSystem::verifyMinimizedRules(
    const llvm::DenseSet<unsigned> &redundantConformances) const {
  unsigned redundantRuleCount = 0;

  for (unsigned ruleID : indices(Rules)) {
    const auto &rule = getRule(ruleID);

    // Ignore the rewrite rule if it is not part of our minimization domain.
    if (!isInMinimizationDomain(rule.getLHS().getRootProtocol())) {
      if (rule.isRedundant()) {
        llvm::errs() << "Redundant rule outside minimization domain: "
                     << rule << "\n\n";
        dump(llvm::errs());
        abort();
      }

      continue;
    }

    // Note that sometimes permanent rules can be simplified, but they can never
    // be redundant.
    if (rule.isPermanent()) {
      if (rule.isRedundant()) {
        llvm::errs() << "Permanent rule is redundant: " << rule << "\n\n";
        dump(llvm::errs());
        abort();
      }

      continue;
    }

    if (rule.isRedundant())
      ++redundantRuleCount;

    // LHS-simplified rules should be redundant, unless they're protocol
    // conformance rules, which unfortunately might not be redundant, because
    // we try to keep them in the original protocol definition for
    // compatibility with the GenericSignatureBuilder's minimization algorithm.
    if (rule.isLHSSimplified() &&
        !rule.isRedundant() &&
        !rule.isProtocolConformanceRule()) {
      llvm::errs() << "Simplified rule is not redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }

    // RHS-simplified and substitution-simplified rules should be redundant.
    if ((rule.isRHSSimplified() ||
         rule.isSubstitutionSimplified()) &&
        !rule.isRedundant()) {
      llvm::errs() << "Simplified rule is not redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }

    if (rule.isRedundant() &&
        rule.isAnyConformanceRule() &&
        !rule.isRHSSimplified() &&
        !rule.isSubstitutionSimplified() &&
        !rule.containsUnresolvedSymbols() &&
        !redundantConformances.count(ruleID)) {
      llvm::errs() << "Minimal conformance is redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }
  }

  if (RedundantRules.size() != redundantRuleCount) {
    llvm::errs() << "Expected " << RedundantRules.size() << " redundant rules "
                 << "but counted " << redundantRuleCount << "\n";
    dump(llvm::errs());
    abort();
  }

  for (const auto &pair : RedundantRules) {
    const auto &rule = getRule(pair.first);
    if (!rule.isRedundant()) {
      llvm::errs() << "Recorded replacement path for non-redundant rule "
                   << rule << "\n";
      dump(llvm::errs());
      abort();
    }
  }
}
