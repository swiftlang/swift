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
// A confluent rewrite system together with a set of 3-cells that generate the
// homotopy relation on 2-cells (rewrite paths) is known as a 'coherent
// presentation'.
//
// If a rewrite rule appears exactly once in a 3-cell and without context, the
// 3-cell witnesses a redundancy; the rewrite rule is equivalent to traveling
// around the loop "in the other direction". This rewrite rule and the
// corresponding 3-cell can be deleted from the coherent presentation via a
// Tietze transformation.
//
// Any occurrence of the rule in the remaining 3-cells is replaced with the
// alternate definition obtained by splitting the 3-cell that witnessed the
// redundancy. After substitution, every 3-cell is normalized to a cyclically
// reduced left-canonical form. The 3-cell witnessing the redundancy normalizes
// to the empty loop and is deleted.
//
// Iterating this process eventually produces a minimal presentation.
//
// For a description of the general algorithm, see "A Homotopical Completion
// Procedure with Applications to Coherence of Monoids",
// https://hal.inria.fr/hal-00818253.
//
// The idea of computing a left-canonical form for 2-cells is from
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
// condition is needed than appearing once in a 3-cell and without context;
// the rule must not be a _generating conformance_. The algorithm for computing
// a minimal set of generating conformances is implemented in
// GeneratingConformances.cpp.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Range.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// Invert a 2-cell.
void RewritePath::invert() {
  std::reverse(Steps.begin(), Steps.end());

  for (auto &step : Steps)
    step.invert();
}

AppliedRewriteStep
RewriteStep::applyRewriteRule(MutableTerm &term,
                              const RewriteSystem &system) const {
  assert(Kind == ApplyRewriteRule);

  const auto &rule = system.getRule(RuleID);

  auto lhs = (Inverse ? rule.getRHS() : rule.getLHS());
  auto rhs = (Inverse ? rule.getLHS() : rule.getRHS());

  auto bug = [&](StringRef msg) {
    llvm::errs() << msg << "\n";
    llvm::errs() << "- Term: " << term << "\n";
    llvm::errs() << "- StartOffset: " << StartOffset << "\n";
    llvm::errs() << "- EndOffset: " << EndOffset << "\n";
    llvm::errs() << "- Expected subterm: " << lhs << "\n";
    abort();
  };

  if (term.size() != StartOffset + lhs.size() + EndOffset) {
    bug("Invalid whiskering");
  }

  if (!std::equal(term.begin() + StartOffset,
                  term.begin() + StartOffset + lhs.size(),
                  lhs.begin())) {
    bug("Invalid subterm");
  }

  MutableTerm prefix(term.begin(), term.begin() + StartOffset);
  MutableTerm suffix(term.end() - EndOffset, term.end());

  term = prefix;
  term.append(rhs);
  term.append(suffix);

  return {lhs, rhs, prefix, suffix};
}

MutableTerm RewriteStep::applyAdjustment(MutableTerm &term,
                                         const RewriteSystem &system) const {
  assert(Kind == AdjustConcreteType);
  assert(EndOffset == 0);
  assert(RuleID == 0);

  auto &ctx = system.getRewriteContext();
  MutableTerm prefix(term.begin(), term.begin() + StartOffset);

  // We're either adding or removing the prefix to each concrete substitution.
  term.back() = term.back().transformConcreteSubstitutions(
    [&](Term t) -> Term {
      if (Inverse) {
        if (!std::equal(t.begin(),
                        t.begin() + StartOffset,
                        prefix.begin())) {
          llvm::errs() << "Invalid rewrite path\n";
          llvm::errs() << "- Term: " << term << "\n";
          llvm::errs() << "- Start offset: " << StartOffset << "\n";
          llvm::errs() << "- Expected subterm: " << prefix << "\n";
          abort();
        }

        MutableTerm mutTerm(t.begin() + StartOffset, t.end());
        return Term::get(mutTerm, ctx);
      } else {
        MutableTerm mutTerm(prefix);
        mutTerm.append(t);
        return Term::get(mutTerm, ctx);
      }
    }, ctx);

  return prefix;
}

void RewriteStep::apply(MutableTerm &term, const RewriteSystem &system) const {
  switch (Kind) {
  case ApplyRewriteRule:
    (void) applyRewriteRule(term, system);
    break;

  case AdjustConcreteType:
    (void) applyAdjustment(term, system);
    break;
  }
}

/// Dumps the rewrite step that was applied to \p term. Mutates \p term to
/// reflect the application of the rule.
void RewriteStep::dump(llvm::raw_ostream &out,
                       MutableTerm &term,
                       const RewriteSystem &system) const {
  switch (Kind) {
  case ApplyRewriteRule: {
    auto result = applyRewriteRule(term, system);

    if (!result.prefix.empty()) {
      out << result.prefix;
      out << ".";
    }
    out << "(" << result.lhs << " => " << result.rhs << ")";
    if (!result.suffix.empty()) {
      out << ".";
      out << result.suffix;
    }

    break;
  }
  case AdjustConcreteType: {
    auto result = applyAdjustment(term, system);

    out << "(σ";
    out << (Inverse ? " - " : " + ");
    out << result << ")";

    break;
  }
  }
}

/// A rewrite rule is redundant if it appears exactly once in a 3-cell
/// without context.
llvm::SmallVector<unsigned, 1>
RewritePath::findRulesAppearingOnceInEmptyContext() const {
  llvm::SmallDenseSet<unsigned, 2> rulesInEmptyContext;
  llvm::SmallDenseMap<unsigned, unsigned, 2> ruleMultiplicity;

  for (auto step : Steps) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      if (!step.isInContext())
        rulesInEmptyContext.insert(step.RuleID);

      ++ruleMultiplicity[step.RuleID];
      break;
    }
    case RewriteStep::AdjustConcreteType:
      break;
    }
  }

  SmallVector<unsigned, 1> result;
  for (auto rule : rulesInEmptyContext) {
    auto found = ruleMultiplicity.find(rule);
    assert(found != ruleMultiplicity.end());

    if (found->second == 1)
      result.push_back(rule);
  }

  std::sort(result.begin(), result.end());
  return result;
}

/// Given a rewrite rule which appears exactly once in a 3-cell
/// without context, return a new definition for this rewrite rule.
/// The new definition is the path obtained by deleting the
/// rewrite rule from the 3-cell.
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

  for (const auto &step : Steps) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule: {
      if (step.RuleID != ruleID) {
        newSteps.push_back(step);
        break;
      }

      auto adjustStep = [&](RewriteStep newStep) {
        newStep.StartOffset += step.StartOffset;

        if (newStep.Kind == RewriteStep::ApplyRewriteRule)
          newStep.EndOffset += step.EndOffset;

        newStep.Inverse ^= step.Inverse;
        newSteps.push_back(newStep);
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

  if (Kind == RewriteStep::ApplyRewriteRule &&
      RuleID != other.RuleID)
    return false;

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
/// For example, consider this 3-cell, forming a loop with basepoint 'X':
///
///   (X => Y.A) * (Y.A => Y.B) * Y.(B => A) * (Y.A => X)
///
/// The first step is the inverse of the last step, so the cyclic
/// reduction is the 3-cell (Y.A => Y.B) * Y.(B => A), with a new
/// basepoint 'Y.A'.
bool RewritePath::computeCyclicallyReducedLoop(MutableTerm &basepoint,
                                               const RewriteSystem &system) {
  unsigned count = 0;

  while (2 * count + 1 < size()) {
    auto left = Steps[count];
    auto right = Steps[Steps.size() - count - 1];
    if (!left.isInverseOf(right))
      break;

    // Update the basepoint by applying the first step in the path.
    left.apply(basepoint, system);

    ++count;
  }

  std::rotate(Steps.begin(), Steps.begin() + count, Steps.end() - count);
  Steps.erase(Steps.end() - 2 * count, Steps.end());

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

/// Dumps a series of rewrite steps applied to \p term.
void RewritePath::dump(llvm::raw_ostream &out,
                       MutableTerm term,
                       const RewriteSystem &system) const {
  bool first = true;

  for (const auto &step : Steps) {
    if (!first) {
      out << " ⊗ ";
    } else {
      first = false;
    }

    step.dump(out, term, system);
  }
}

/// Compute cyclically-reduced left-canonical normal form of a 3-cell.
void HomotopyGenerator::normalize(const RewriteSystem &system) {
  // FIXME: This can be more efficient.
  bool changed;
  do {
    changed = false;
    changed |= Path.computeFreelyReducedPath();
    changed |= Path.computeCyclicallyReducedLoop(Basepoint, system);
    changed |= Path.computeLeftCanonicalForm(system);
  } while (changed);
}

/// A 3-cell is "in context" if every rewrite step has a left or right whisker.
bool HomotopyGenerator::isInContext() const {
  unsigned minStartOffset = (unsigned) -1;
  unsigned minEndOffset = (unsigned) -1;

  for (const auto &step : Path) {
    switch (step.Kind) {
    case RewriteStep::ApplyRewriteRule:
      minStartOffset = std::min(minStartOffset, step.StartOffset);
      minEndOffset = std::min(minEndOffset, step.EndOffset);
      break;

    case RewriteStep::AdjustConcreteType:
      break;
    }

    if (minStartOffset == 0 && minEndOffset == 0)
      break;
  }

  return (minStartOffset > 0 || minEndOffset > 0);
}

void HomotopyGenerator::dump(llvm::raw_ostream &out,
                             const RewriteSystem &system) const {
  out << Basepoint << ": ";
  Path.dump(out, Basepoint, system);
  if (isDeleted())
    out << " [deleted]";
}

/// Find a rule to delete by looking through all 3-cells for rewrite rules appearing
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

  for (auto &loop : HomotopyGenerators) {
    if (loop.isDeleted())
      continue;

    SmallVector<unsigned> redundancyCandidates =
        loop.Path.findRulesAppearingOnceInEmptyContext();

    auto found = std::find_if(
        redundancyCandidates.begin(),
        redundancyCandidates.end(),
        [&](unsigned ruleID) -> bool {
          const auto &rule = getRule(ruleID);

          // We should not find a rule that has already been marked redundant
          // here; it should have already been replaced with a rewrite path
          // in all homotopy generators.
          assert(!rule.isRedundant());

          // Associated type introduction rules are 'permanent'. They're
          // not worth eliminating since they are re-added every time; it
          // is better to find other candidates to eliminate in the same
          // 3-cell instead.
          if (rule.isPermanent())
            return false;

          // Other rules involving unresolved name symbols are eliminated in
          // the first pass.
          if (firstPass)
            return rule.containsUnresolvedSymbols();

          assert(!rule.containsUnresolvedSymbols());

          // Protocol conformance rules are eliminated via a different
          // algorithm which computes "generating conformances".
          if (rule.isProtocolConformanceRule()) {
            if (!redundantConformances)
              return false;

            if (!redundantConformances->count(ruleID))
              return false;
          }

          return true;
        });

    if (found == redundancyCandidates.end())
      continue;

    auto ruleID = *found;
    assert(replacementPath.empty());
    replacementPath = loop.Path.splitCycleAtRule(ruleID);

    loop.markDeleted();
    getRule(ruleID).markRedundant();

    return ruleID;
  }

  return None;
}

/// Delete a rewrite rule that is known to be redundant, replacing all
/// occurrences of the rule in all 3-cells with the replacement path.
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
  // normalize all 3-cells.
  for (auto &loop : HomotopyGenerators) {
    if (loop.isDeleted())
      continue;

    bool changed = loop.Path.replaceRuleWithPath(ruleID, replacementPath);
    if (!changed)
      continue;

    unsigned size = loop.Path.size();

    loop.normalize(*this);

    if (Debug.contains(DebugFlags::HomotopyReduction)) {
      if (size != loop.Path.size()) {
        llvm::dbgs() << "** Note: 3-cell normalization eliminated "
                     << (size - loop.Path.size()) << " steps\n";
      }
    }

    if (loop.Path.empty()) {
      if (Debug.contains(DebugFlags::HomotopyReduction)) {
        llvm::dbgs() << "** Deleting trivial 3-cell at basepoint ";
        llvm::dbgs() << loop.Basepoint << "\n";
      }

      loop.markDeleted();
      continue;
    }

    // FIXME: Is this correct?
    if (loop.isInContext()) {
      if (Debug.contains(DebugFlags::HomotopyReduction)) {
        llvm::dbgs() << "** Deleting 3-cell in context: ";
        loop.dump(llvm::dbgs(), *this);
        llvm::dbgs() << "\n";
      }

      loop.markDeleted();
      continue;
    }

    if (Debug.contains(DebugFlags::HomotopyReduction)) {
      llvm::dbgs() << "** Updated 3-cell: ";
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

    // If there no redundant rules remain in this pass, stop.
    if (!optRuleID)
      return;

    deleteRule(*optRuleID, replacementPath);
  }
}

/// Use the 3-cells to delete redundant rewrite rules via a series of Tietze
/// transformations, updating and simplifying existing 3-cells as each rule
/// is deleted.
void RewriteSystem::minimizeRewriteSystem() {
  /// Begin by normalizing all 3-cells to cyclically-reduced left-canonical
  /// form.
  for (auto &loop : HomotopyGenerators) {
    if (loop.isDeleted())
      continue;

    loop.normalize(*this);
  }

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

  // Assert if homotopy reduction failed to eliminate a redundant conformance,
  // since this suggests a misunderstanding on my part.
  for (unsigned ruleID : redundantConformances) {
    const auto &rule = getRule(ruleID);
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

  // Assert if homotopy reduction failed to eliminate a rewrite rule which was
  // deleted because either it's left hand side can be reduced by some other
  // rule, or because it's right hand side can be reduced further.
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

    if (rule.isSimplified() && !rule.isRedundant()) {
      llvm::errs() << "Simplified rule is not redundant: " << rule << "\n\n";
      dump(llvm::errs());
      abort();
    }
  }
}

/// Verify that each 3-cell is a valid loop around its basepoint.
void RewriteSystem::verifyHomotopyGenerators() const {
#ifndef NDEBUG
  for (const auto &loop : HomotopyGenerators) {
    auto term = loop.Basepoint;

    for (const auto &step : loop.Path) {
      step.apply(term, *this);
    }

    if (term != loop.Basepoint) {
      llvm::errs() << "Not a loop: ";
      loop.dump(llvm::errs(), *this);
      llvm::errs() << "\n";
      abort();
    }
  }
#endif
}