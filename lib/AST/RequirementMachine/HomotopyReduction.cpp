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
// A confluent rewrite system together with a set of rewrite loops that generate
// the homotopy relation on rewrite paths is known as a 'coherent presentation'.
//
// If a rewrite rule appears exactly once in a loop and without context, the
// loop witnesses a redundancy; the rewrite rule is equivalent to traveling
// around the loop "in the other direction". This rewrite rule and the
// corresponding loop can be deleted from the coherent presentation via a
// Tietze transformation.
//
// Any occurrence of the rule in the remaining loops is replaced with the
// alternate definition obtained by splitting the loop that witnessed the
// redundancy. After substitution, every loop is normalized to a cyclically
// reduced left-canonical form. The loop witnessing the redundancy normalizes
// to the empty loop and is deleted.
//
// Iterating this process eventually produces a minimal presentation.
//
// For a description of the general algorithm, see "A Homotopical Completion
// Procedure with Applications to Coherence of Monoids",
// https://hal.inria.fr/hal-00818253.
//
// The idea of computing a left-canonical form for rewrite loops is from
// "Homotopy reduction systems for monoid presentations",
// https://www.sciencedirect.com/science/article/pii/S0022404997000959
//
// Note that in the world of Swift, rewrite rules for introducing associated
// type symbols are marked 'permanent'; they are always re-added when a new
// rewrite system is built from a minimal generic signature, so instead of
// deleting them it is better to leave them in place in case it allows other
// rules to be deleted instead.
//
// Also, for a conformance rule (V.[P] => V) to be redundant, a stronger
// condition is needed than appearing once in a loop and without context;
// the rule must not be a _generating conformance_. The algorithm for computing
// a minimal set of generating conformances is implemented in
// GeneratingConformances.cpp.
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

/// A rewrite rule is redundant if it appears exactly once in a loop
/// without context.
llvm::SmallVector<unsigned, 1>
RewriteLoop::findRulesAppearingOnceInEmptyContext(
    const RewriteSystem &system) const {
  llvm::SmallDenseSet<unsigned, 2> rulesInEmptyContext;
  llvm::SmallDenseMap<unsigned, unsigned, 2> ruleMultiplicity;

  RewritePathEvaluator evaluator(Basepoint);

  for (auto step : Path) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      if (!step.isInContext() && !evaluator.isInContext())
        rulesInEmptyContext.insert(step.RuleID);

      ++ruleMultiplicity[step.RuleID];
      break;
    }

    case RewriteStep::AdjustConcreteType:
    case RewriteStep::Shift:
    case RewriteStep::Decompose:
      break;
    }

    step.apply(evaluator, system);
  }

  SmallVector<unsigned, 1> result;
  for (auto rule : rulesInEmptyContext) {
    auto found = ruleMultiplicity.find(rule);
    assert(found != ruleMultiplicity.end());

    if (found->second == 1)
      result.push_back(rule);
  }

  return result;
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
    case RewriteStep::ApplyRewriteRule: {
      if (step.RuleID != ruleID)
        break;

      assert(!sawRule && "Rule appears more than once?");
      assert(!step.isInContext() && "Rule appears in context?");

      ruleWasInverted = step.Inverse;
      sawRule = true;
      continue;
    }
    case RewriteStep::AdjustConcreteType:
    case RewriteStep::Shift:
    case RewriteStep::Decompose:
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
    if (step.Kind == RewriteStep::ApplyRewriteRule &&
        step.RuleID == ruleID) {
      foundAny = true;
      break;
    }
  }

  if (!foundAny)
    return false;

  SmallVector<RewriteStep, 4> newSteps;

  // Keep track of Decompose/Compose pairs. Any rewrite steps in
  // between do not need to be re-contextualized, since they
  // operate on new terms that were pushed on the stack by the
  // Compose operation.
  unsigned decomposeCount = 0;

  for (const auto &step : Steps) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      if (step.RuleID != ruleID) {
        newSteps.push_back(step);
        break;
      }

      auto adjustStep = [&](RewriteStep newStep) {
        bool inverse = newStep.Inverse ^ step.Inverse;

        if (newStep.Kind == RewriteStep::Decompose && inverse) {
          assert(decomposeCount > 0);
          --decomposeCount;
        }

        if (decomposeCount == 0) {
          newStep.StartOffset += step.StartOffset;
          newStep.EndOffset += step.EndOffset;
        }

        newStep.Inverse = inverse;
        newSteps.push_back(newStep);

        if (newStep.Kind == RewriteStep::Decompose && !inverse) {
          ++decomposeCount;
        }
      };

      if (step.Inverse) {
        for (auto newStep : llvm::reverse(path))
          adjustStep(newStep);
      } else {
        for (auto newStep : path)
          adjustStep(newStep);
      }

      break;
    }
    case RewriteStep::AdjustConcreteType:
    case RewriteStep::Shift:
    case RewriteStep::Decompose:
      newSteps.push_back(step);
      break;
    }
  }

  std::swap(newSteps, Steps);
  return true;
}

/// Returns true if this rewrite step is an inverse of \p other
/// (and vice versa).
bool RewriteStep::isInverseOf(const RewriteStep &other) const {
  if (Kind != other.Kind)
    return false;

  if (StartOffset != other.StartOffset)
    return false;

  if (Inverse != !other.Inverse)
    return false;

  switch (Kind) {
  case RewriteStep::ApplyRewriteRule:
    return RuleID == other.RuleID;

  case RewriteStep::AdjustConcreteType:
    return true;

  case RewriteStep::Shift:
    return true;

  case RewriteStep::Decompose:
    return RuleID == other.RuleID;
  }

  assert(EndOffset == other.EndOffset && "Bad whiskering?");
  return true;
};

bool RewriteStep::maybeSwapRewriteSteps(RewriteStep &other,
                                        const RewriteSystem &system) {
  if (Kind != RewriteStep::ApplyRewriteRule ||
      other.Kind != RewriteStep::ApplyRewriteRule)
    return false;

  // Two rewrite steps are _orthogonal_ if they rewrite disjoint subterms
  // in context. Orthogonal rewrite steps commute, so we can canonicalize
  // a path by placing the left-most step first.
  //
  // Eg, A.U.B.(X => Y).C ⊗ A.(U => V).B.Y == A.(U => V).B.X ⊗ A.V.B.(X => Y).
  //
  // Or, in diagram form. We want to turn this:
  //
  //   ----- time ----->
  // +---------+---------+
  // |    A    |    A    |
  // +---------+---------+
  // |    U    | U ==> V |
  // +---------+---------+
  // |    B    |    B    |
  // +---------+---------+
  // | X ==> Y |    Y    |
  // +---------+---------+
  // |    C    |    C    |
  // +---------+---------+
  //
  // Into this:
  //
  // +---------+---------+
  // |    A    |    A    |
  // +---------+---------+
  // | U ==> V |    V    |
  // +---------+---------+
  // |    B    |    B    |
  // +---------+---------+
  // |    X    | X ==> Y |
  // +---------+---------+
  // |    C    |    C    |
  // +---------+---------+
  //
  // Note that
  //
  // StartOffset == |A|+|U|+|B|
  // EndOffset = |C|
  //
  // other.StartOffset = |A|
  // other.EndOffset = |B|+|Y|+|C|
  //
  // After interchange, we adjust:
  //
  // StartOffset = |A|
  // EndOffset = |B|+|X|+|C|
  //
  // other.StartOffset = |A|+|V|+|B|
  // other.EndOffset = |C|

  const auto &rule = system.getRule(RuleID);
  auto lhs = (Inverse ? rule.getRHS() : rule.getLHS());
  auto rhs = (Inverse ? rule.getLHS() : rule.getRHS());

  const auto &otherRule = system.getRule(other.RuleID);
  auto otherLHS = (other.Inverse ? otherRule.getRHS() : otherRule.getLHS());
  auto otherRHS = (other.Inverse ? otherRule.getLHS() : otherRule.getRHS());

  if (StartOffset < other.StartOffset + otherLHS.size())
    return false;

  std::swap(*this, other);
  EndOffset += (lhs.size() - rhs.size());
  other.StartOffset += (otherRHS.size() - otherLHS.size());

  return true;
}

/// Cancels out adjacent rewrite steps that are inverses of each other.
/// This does not change either endpoint of the path, and the path does
/// not necessarily need to be a loop.
bool RewritePath::computeFreelyReducedPath() {
  SmallVector<RewriteStep, 4> newSteps;
  bool changed = false;

  for (const auto &step : Steps) {
    if (!newSteps.empty() &&
        newSteps.back().isInverseOf(step)) {
      changed = true;
      newSteps.pop_back();
      continue;
    }

    newSteps.push_back(step);
  }

  std::swap(newSteps, Steps);
  return changed;
}

/// Given a path that is a loop around the given basepoint, cancels out
/// pairs of terms from the ends that are inverses of each other, applying
/// the corresponding translation to the basepoint.
///
/// For example, consider this loop with basepoint 'X':
///
///   (X => Y.A) * (Y.A => Y.B) * Y.(B => A) * (Y.A => X)
///
/// The first step is the inverse of the last step, so the cyclic
/// reduction is the loop (Y.A => Y.B) * Y.(B => A), with a new
/// basepoint 'Y.A'.
bool RewritePath::computeCyclicallyReducedLoop(MutableTerm &basepoint,
                                               const RewriteSystem &system) {
  RewritePathEvaluator evaluator(basepoint);
  unsigned count = 0;

  while (2 * count + 1 < size()) {
    auto left = Steps[count];
    auto right = Steps[Steps.size() - count - 1];
    if (!left.isInverseOf(right))
      break;

    // Update the basepoint by applying the first step in the path.
    left.apply(evaluator, system);

    ++count;
  }

  std::rotate(Steps.begin(), Steps.begin() + count, Steps.end() - count);
  Steps.erase(Steps.end() - 2 * count, Steps.end());

  basepoint = evaluator.getCurrentTerm();
  return count > 0;
}

/// Apply the interchange rule until fixed point (see maybeSwapRewriteSteps()).
bool RewritePath::computeLeftCanonicalForm(const RewriteSystem &system) {
  bool changed = false;

  for (unsigned i = 1, e = Steps.size(); i < e; ++i) {
    auto &prevStep = Steps[i - 1];
    auto &step = Steps[i];

    if (prevStep.maybeSwapRewriteSteps(step, system))
      changed = true;
  }

  return changed;
}

/// Compute cyclically-reduced left-canonical normal form of a loop.
void RewriteLoop::normalize(const RewriteSystem &system) {
  // FIXME: This can be more efficient.
  bool changed;
  do {
    changed = false;
    changed |= Path.computeFreelyReducedPath();
    changed |= Path.computeCyclicallyReducedLoop(Basepoint, system);
    changed |= Path.computeLeftCanonicalForm(system);
  } while (changed);
}

/// A loop is "in context" if every rewrite step has a left or right whisker.
bool RewriteLoop::isInContext(const RewriteSystem &system) const {
  RewritePathEvaluator evaluator(Basepoint);

  unsigned minStartOffset = (unsigned) -1;
  unsigned minEndOffset = (unsigned) -1;

  for (const auto &step : Path) {
    if (!evaluator.isInContext()) {
      switch (step.Kind) {
      case RewriteStep::ApplyRewriteRule:
        minStartOffset = std::min(minStartOffset, step.StartOffset);
        minEndOffset = std::min(minEndOffset, step.EndOffset);
        break;

      case RewriteStep::AdjustConcreteType:
      case RewriteStep::Shift:
      case RewriteStep::Decompose:
        break;
      }

      if (minStartOffset == 0 && minEndOffset == 0)
        break;
    }

    step.apply(evaluator, system);
  }

  return (minStartOffset > 0 || minEndOffset > 0);
}

/// Check if a rewrite rule is a candidate for deletion in this pass of the
/// minimization algorithm.
bool RewriteSystem::
isCandidateForDeletion(unsigned ruleID,
                       bool firstPass,
                       const llvm::DenseSet<unsigned> *redundantConformances) const {
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
    return false;

  // Other rules involving unresolved name symbols are derived from an
  // associated type introduction rule together with a conformance rule.
  // They are eliminated in the first pass.
  if (firstPass)
    return rule.getLHS().containsUnresolvedSymbols();

  // In the second and third pass we should not have any rules involving
  // unresolved name symbols, except for permanent rules which were
  // already skipped above.
  //
  // FIXME: This isn't true with invalid code.
  assert(!rule.getLHS().containsUnresolvedSymbols());

  // Protocol conformance rules are eliminated via a different
  // algorithm which computes "generating conformances".
  //
  // The first and second passes skip protocol conformance rules.
  //
  // The third pass eliminates any protocol conformance rule which is
  // redundant according to both homotopy reduction and the generating
  // conformances algorithm.
  //
  // Later on, we verify that any conformance redundant via generating
  // conformances was also redundant via homotopy reduction. This
  // means that the set of generating conformances is always a superset
  // (or equal to) of the set of minimal protocol conformance
  // requirements that homotopy reduction alone would produce.
  if (rule.isProtocolConformanceRule()) {
    if (!redundantConformances)
      return false;

    if (!redundantConformances->count(ruleID))
      return false;
  }

  return true;
}

/// Find a rule to delete by looking through all loops for rewrite rules appearing
/// once in empty context. Returns a redundant rule to delete if one was found,
/// otherwise returns None.
///
/// Minimization performs three passes over the rewrite system, with the
/// \p firstPass and \p redundantConformances parameters as follows:
///
/// 1) First, rules involving unresolved name symbols are deleted, with
///    \p firstPass equal to true and \p redundantConformances equal to nullptr.
///
/// 2) Second, rules that are not conformance rules are deleted, with
///    \p firstPass equal to false and \p redundantConformances equal to nullptr.
///
/// 3) Finally, conformance rules are deleted after computing a minimal set of
///    generating conformances, with \p firstPass equal to false and
///    \p redundantConformances equal to the set of conformance rules that are
///    not generating conformances.
Optional<unsigned> RewriteSystem::
findRuleToDelete(bool firstPass,
                 const llvm::DenseSet<unsigned> *redundantConformances,
                 RewritePath &replacementPath) {
  assert(!firstPass || redundantConformances == nullptr);

  SmallVector<std::pair<unsigned, unsigned>, 2> redundancyCandidates;
  for (unsigned loopID : indices(Loops)) {
    const auto &loop = Loops[loopID];
    if (loop.isDeleted())
      continue;

    for (unsigned ruleID : loop.findRulesAppearingOnceInEmptyContext(*this)) {
      redundancyCandidates.emplace_back(loopID, ruleID);
    }
  }

  Optional<std::pair<unsigned, unsigned>> found;

  for (const auto &pair : redundancyCandidates) {
    unsigned ruleID = pair.second;
    if (!isCandidateForDeletion(ruleID, firstPass, redundantConformances))
      continue;

    if (!found) {
      found = pair;
      continue;
    }

    const auto &rule = getRule(ruleID);
    const auto &otherRule = getRule(found->second);

    // Prefer to delete "less canonical" rules.
    if (rule.compare(otherRule, Context) > 0)
      found = pair;
  }

  if (!found)
    return None;

  unsigned loopID = found->first;
  unsigned ruleID = found->second;
  assert(replacementPath.empty());

  auto &loop = Loops[loopID];
  replacementPath = loop.Path.splitCycleAtRule(ruleID);

  loop.markDeleted();
  getRule(ruleID).markRedundant();

  return ruleID;
}

/// Delete a rewrite rule that is known to be redundant, replacing all
/// occurrences of the rule in all loops with the replacement path.
void RewriteSystem::deleteRule(unsigned ruleID,
                               const RewritePath &replacementPath) {
  if (Debug.contains(DebugFlags::HomotopyReduction)) {
    const auto &rule = getRule(ruleID);
    llvm::dbgs() << "* Deleting rule ";
    rule.dump(llvm::dbgs());
    llvm::dbgs() << " (#" << ruleID << ")\n";
    llvm::dbgs() << "* Replacement path: ";
    MutableTerm mutTerm(rule.getLHS());
    replacementPath.dump(llvm::dbgs(), mutTerm, *this);
    llvm::dbgs() << "\n";
  }

  // Replace all occurrences of the rule with the replacement path and
  // normalize all loops.
  for (auto &loop : Loops) {
    if (loop.isDeleted())
      continue;

    bool changed = loop.Path.replaceRuleWithPath(ruleID, replacementPath);
    if (!changed)
      continue;

    unsigned size = loop.Path.size();

    loop.normalize(*this);

    if (Debug.contains(DebugFlags::HomotopyReduction)) {
      if (size != loop.Path.size()) {
        llvm::dbgs() << "** Note: loop normalization eliminated "
                     << (size - loop.Path.size()) << " steps\n";
      }
    }

    if (loop.Path.empty()) {
      if (Debug.contains(DebugFlags::HomotopyReduction)) {
        llvm::dbgs() << "** Deleting trivial loop at basepoint ";
        llvm::dbgs() << loop.Basepoint << "\n";
      }

      loop.markDeleted();
      continue;
    }

    // FIXME: Is this correct?
    if (loop.isInContext(*this)) {
      if (Debug.contains(DebugFlags::HomotopyReduction)) {
        llvm::dbgs() << "** Deleting loop in context: ";
        loop.dump(llvm::dbgs(), *this);
        llvm::dbgs() << "\n";
      }

      loop.markDeleted();
      continue;
    }

    if (Debug.contains(DebugFlags::HomotopyReduction)) {
      llvm::dbgs() << "** Updated loop: ";
      loop.dump(llvm::dbgs(), *this);
      llvm::dbgs() << "\n";
    }
  }
}

void RewriteSystem::performHomotopyReduction(
    bool firstPass,
    const llvm::DenseSet<unsigned> *redundantConformances) {
  while (true) {
    RewritePath replacementPath;
    auto optRuleID = findRuleToDelete(firstPass,
                                      redundantConformances,
                                      replacementPath);

    // If no redundant rules remain which can be eliminated by this pass, stop.
    if (!optRuleID)
      return;

    deleteRule(*optRuleID, replacementPath);
  }
}

/// Use the loops to delete redundant rewrite rules via a series of Tietze
/// transformations, updating and simplifying existing loops as each rule
/// is deleted.
///
/// Redundant rules are mutated to set their isRedundant() bit.
void RewriteSystem::minimizeRewriteSystem() {
  assert(Complete);
  assert(!Minimized);
  Minimized = 1;

  /// Begin by normalizing all loops to cyclically-reduced left-canonical
  /// form.
  for (auto &loop : Loops) {
    if (loop.isDeleted())
      continue;

    loop.normalize(*this);
  }

  // Check invariants before homotopy reduction.
  verifyRewriteLoops();

  // First pass: Eliminate all redundant rules involving unresolved types.
  performHomotopyReduction(/*firstPass=*/true,
                           /*redundantConformances=*/nullptr);

  // Second pass: Eliminate all redundant rules that are not conformance rules.
  performHomotopyReduction(/*firstPass=*/false,
                           /*redundantConformances=*/nullptr);

  // Now find a minimal set of generating conformances.
  //
  // FIXME: For now this just produces a set of redundant conformances, but
  // it should actually compute the full generating conformance basis, since
  // we want to use the same information for finding conformance access paths.
  llvm::DenseSet<unsigned> redundantConformances;
  computeGeneratingConformances(redundantConformances);

  // Third pass: Eliminate all redundant conformance rules.
  performHomotopyReduction(/*firstPass=*/false,
                           /*redundantConformances=*/&redundantConformances);

  // Check invariants after homotopy reduction.
  verifyRewriteLoops();
  verifyRedundantConformances(redundantConformances);
  verifyMinimizedRules();
}

/// Collect all non-permanent, non-redundant rules whose domain is equal to
/// one of the protocols in \p proto. These rules form the requirement
/// signatures of these protocols.
llvm::DenseMap<const ProtocolDecl *, std::vector<unsigned>>
RewriteSystem::getMinimizedRules(ArrayRef<const ProtocolDecl *> protos) {
  assert(Minimized);

  llvm::DenseMap<const ProtocolDecl *, std::vector<unsigned>> rules;
  for (unsigned ruleID : indices(Rules)) {
    const auto &rule = getRule(ruleID);

    if (rule.isPermanent())
      continue;

    if (rule.isRedundant())
      continue;

    auto domain = rule.getLHS()[0].getProtocols();
    assert(domain.size() == 1);

    const auto *proto = domain[0];
    if (std::find(protos.begin(), protos.end(), proto) != protos.end())
      rules[proto].push_back(ruleID);
  }

  return rules;
}

/// Verify that each loop begins and ends at its basepoint.
void RewriteSystem::verifyRewriteLoops() const {
#ifndef NDEBUG
  for (const auto &loop : Loops) {
    RewritePathEvaluator evaluator(loop.Basepoint);

    for (const auto &step : loop.Path) {
      step.apply(evaluator, *this);
    }

    if (evaluator.getCurrentTerm() != loop.Basepoint) {
      llvm::errs() << "Not a loop: ";
      loop.dump(llvm::errs(), *this);
      llvm::errs() << "\n";
      abort();
    }

    if (evaluator.isInContext()) {
      llvm::errs() << "Leftover terms on evaluator stack\n";
      evaluator.dump(llvm::errs());
      abort();
    }
  }
#endif
}

/// Assert if homotopy reduction failed to eliminate a redundant conformance,
/// since this suggests a misunderstanding on my part.
void RewriteSystem::verifyRedundantConformances(
    llvm::DenseSet<unsigned> redundantConformances) const {
  for (unsigned ruleID : redundantConformances) {
    const auto &rule = getRule(ruleID);
    assert(!rule.isPermanent() &&
           "Permanent rule cannot be redundant");
    assert(!rule.isIdentityConformanceRule() &&
           "Identity conformance cannot be redundant");
    assert(rule.isProtocolConformanceRule() &&
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
void RewriteSystem::verifyMinimizedRules() const {
  for (const auto &rule : Rules) {
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
      continue;

    // Simplified rules should be redundant.
    if (rule.isSimplified()) {
      llvm::errs() << "Simplified rule is not redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }

    // Rules with unresolved name symbols (other than permanent rules for
    // associated type introduction) should be redundant.
    //
    // FIXME: What about invalid code?
    if (rule.getLHS().containsUnresolvedSymbols() ||
        rule.getRHS().containsUnresolvedSymbols()) {
      llvm::errs() << "Unresolved rule is not redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }
  }
}
