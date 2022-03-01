//===--- LeftCanonicalForm.cpp - Left canonical form of a rewrite path ----===//
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
// Algorithm for reducing a rewrite path to left-canonical form:
//
// - Adjacent steps that are inverses of each other cancel out. for example
//   these two steps will be eliminated:
//
//     (A => B) ⊗ (B => A)
//
// - Interchange law moves rewrites "to the left", for example
//
//     X.(U => V) ⊗ (X => Y).V
//
//   becomes
//
//     (X => Y).U ⊗ Y.(U => V)
//
// These two transformations are iterated until fixed point to produce a
// equivalent rewrite path that is simpler.
//
// From "Homotopy reduction systems for monoid presentations",
// https://www.sciencedirect.com/science/article/pii/S0022404997000959
//
//===----------------------------------------------------------------------===//

#include "RewriteLoop.h"
#include "RewriteSystem.h"
#include "llvm/ADT/SmallVector.h"
#include <utility>

using namespace swift;
using namespace rewriting;

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
  case RewriteStep::Rule:
    return Arg == other.Arg;

  default:
    return false;
  }

  assert(EndOffset == other.EndOffset && "Bad whiskering?");
  return true;
}

bool RewriteStep::maybeSwapRewriteSteps(RewriteStep &other,
                                        const RewriteSystem &system) {
  if (Kind != RewriteStep::Rule ||
      other.Kind != RewriteStep::Rule)
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

  const auto &rule = system.getRule(Arg);
  auto lhs = (Inverse ? rule.getRHS() : rule.getLHS());
  auto rhs = (Inverse ? rule.getLHS() : rule.getRHS());

  const auto &otherRule = system.getRule(other.Arg);
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
bool RewritePath::computeFreelyReducedForm() {
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

  if (!changed)
    return false;
  std::swap(newSteps, Steps);
  return changed;
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

/// Compute freely-reduced left-canonical normal form of a path.
void RewritePath::computeNormalForm(const RewriteSystem &system) {
  // FIXME: This can be more efficient.
  bool changed;
  do {
    changed = false;
    changed |= computeFreelyReducedForm();
    changed |= computeLeftCanonicalForm(system);
  } while (changed);
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
bool RewritePath::computeCyclicallyReducedForm(MutableTerm &basepoint,
                                               const RewriteSystem &system) {
  RewritePathEvaluator evaluator(basepoint);
  unsigned count = 0;

  while (2 * count + 1 < size()) {
    auto left = Steps[count];
    auto right = Steps[Steps.size() - count - 1];
    if (!left.isInverseOf(right))
      break;

    // Update the basepoint by applying the first step in the path.
    evaluator.apply(left, system);

    ++count;
  }

  std::rotate(Steps.begin(), Steps.begin() + count, Steps.end() - count);
  Steps.erase(Steps.end() - 2 * count, Steps.end());

  basepoint = evaluator.getCurrentTerm();
  return count > 0;
}

/// Compute cyclically-reduced left-canonical normal form of a loop.
void RewriteLoop::computeNormalForm(const RewriteSystem &system) {
  // FIXME: This can be more efficient.
  bool changed;
  do {
    changed = false;
    changed |= Path.computeFreelyReducedForm();
    changed |= Path.computeCyclicallyReducedForm(Basepoint, system);
    changed |= Path.computeLeftCanonicalForm(system);

    if (changed)
      markDirty();
  } while (changed);
}