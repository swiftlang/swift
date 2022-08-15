//===--- KnuthBendix.cpp - Confluent completion procedure -----------------===//
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
// This implements completion in the rewriting system sense, not code
// completion.
//
// We use a variation of the Knuth-Bendix algorithm 
// (https://en.wikipedia.org/wiki/Knuth–Bendix_completion_algorithm).
//
// The goal is to find 'overlapping' rules which would allow the same term to
// be rewritten in two different ways. These two different irreducible
// reductions are called a 'critical pair'; the completion procedure introduces
// new rewrite rules to eliminate critical pairs by rewriting one side of the
// pair to the other. This can introduce more overlaps with existing rules, and
// the process iterates until fixed point.
//
// When completion records a new rewrite rule, it also constructs a rewrite loop
// describing how this rule is derived from existing rules. See RewriteLoop.cpp
// for a discussion of rewrite loops.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Range.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <deque>
#include <vector>

#include "RewriteContext.h"
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// For a superclass or concrete type symbol
///
///   [concrete: Foo<X1, ..., Xn>]
///   [superclass: Foo<X1, ..., Xn>]
///
/// Return a new symbol where the prefix T is prepended to each of the
/// substitutions:
///
///   [concrete: Foo<T.X1, ..., T.Xn>]
///   [superclass: Foo<T.X1, ..., T.Xn>]
///
/// Asserts if this is not a superclass or concrete type symbol.
Symbol Symbol::prependPrefixToConcreteSubstitutions(
    const MutableTerm &prefix,
    RewriteContext &ctx) const {
  if (prefix.empty())
    return *this;

  return transformConcreteSubstitutions(
    [&](Term term) -> Term {
      MutableTerm mutTerm;
      mutTerm.append(prefix);
      mutTerm.append(term);

      return Term::get(mutTerm, ctx);
    }, ctx);
}

/// Compute a critical pair from the left hand sides of two rewrite rules,
/// where \p rhs begins at \p from, which must be an iterator pointing
/// into \p lhs.
///
/// The resulting pair, together with a rewrite path relating them is
/// pushed onto \p pairs only if it is non-trivial, that is, the left
/// hand side and right hand side are not equal.
///
/// Otherwise, we record a rewrite loop in \p loops.
///
/// Returns true if the pair was non-trivial, false if it was trivial.
///
/// There are two cases:
///
/// 1) lhs == TUV -> X, rhs == U -> Y. The overlapped term is TUV;
///    applying lhs and rhs, respectively, yields the critical pair
///    (X, TYV).
///
/// 2) lhs == TU -> X, rhs == UV -> Y. The overlapped term is once
///    again TUV; applying lhs and rhs, respectively, yields the
///    critical pair (XV, TY).
///
/// If lhs and rhs have identical left hand sides, either case could
/// apply, but we arbitrarily pick case 1.
///
/// There is also an additional wrinkle. If we're in case 2, and the
/// last symbol of V is a superclass or concrete type symbol A, we prepend
/// T to each substitution of A.
///
/// For example, suppose we have the following two rules:
///
/// A.B -> C
/// B.[concrete: Foo<X>] -> B
///
/// The overlapped term is A.B.[concrete: Foo<X>], so the critical pair
/// is (C.[concrete: Foo<A.X>], A.B). We prepended 'A' to the
/// concrete substitution 'X' to get 'A.X'; the new concrete term
/// is now rooted at the same level as A.B in the rewrite system,
/// not just B.
bool
RewriteSystem::computeCriticalPair(ArrayRef<Symbol>::const_iterator from,
                                   const Rule &lhs, const Rule &rhs,
                                   std::vector<CriticalPair> &pairs,
                                   std::vector<RewriteLoop> &loops) const {
  auto end = lhs.getLHS().end();
  if (from + rhs.getLHS().size() < end) {
    // lhs == TUV -> X, rhs == U -> Y.

    // Note: This includes the case where the two rules have exactly
    // equal left hand sides; that is, lhs == U -> X, rhs == U -> Y.
    //
    // In this case, T and V are both empty.

    // Compute the terms T and V.
    MutableTerm t(lhs.getLHS().begin(), from);
    MutableTerm v(from + rhs.getLHS().size(), lhs.getLHS().end());

    // Compute the term TYV.
    MutableTerm tyv(t);
    tyv.append(rhs.getRHS());
    tyv.append(v);

    MutableTerm x(lhs.getRHS());

    // Compute a path from X to TYV: (X => TUV) ⊗ T.(U => Y).V
    RewritePath path;

    // (1) First, apply the left hand side rule in the reverse direction:
    //
    //     (X => TUV)
    path.add(RewriteStep::forRewriteRule(/*startOffset=*/0,
                                         /*endOffset=*/0,
                                         getRuleID(lhs),
                                         /*inverse=*/true));
    // (2) Now, apply the right hand side in the forward direction:
    //
    //     T.(U => Y).V 
    path.add(RewriteStep::forRewriteRule(t.size(), v.size(),
                                         getRuleID(rhs),
                                         /*inverse=*/false));

    // If X == TYV, we have a trivial overlap.
    if (x == tyv) {
      loops.emplace_back(x, path);
      return false;
    }

    // Add the pair (X, TYV).
    pairs.emplace_back(x, tyv, path);
  } else {
    // lhs == TU -> X, rhs == UV -> Y.

    // Compute the terms T and V.
    MutableTerm t(lhs.getLHS().begin(), from);
    MutableTerm v(rhs.getLHS().begin() + (lhs.getLHS().end() - from),
                  rhs.getLHS().end());

    // Compute the term XV.
    MutableTerm xv(lhs.getRHS());
    xv.append(v);

    // Compute the term TY.
    MutableTerm ty(t);
    ty.append(rhs.getRHS());

    // Compute a path from XV to TY: (X => TU).V ⊗ (σ - T) ⊗ T.(UV => Y)
    RewritePath path;

    // (1) First, apply the left hand side rule in the reverse direction:
    //
    //     (X => TU).V
    path.add(RewriteStep::forRewriteRule(/*startOffset=*/0, v.size(),
                                         getRuleID(lhs),
                                         /*inverse=*/true));

    // (2) Next, if the right hand side rule ends with a superclass or concrete
    // type symbol, remove the prefix 'T' from each substitution in the symbol.
    //
    //     (σ - T)
    if (xv.back().hasSubstitutions() &&
        !xv.back().getSubstitutions().empty() &&
        t.size() > 0) {
      path.add(RewriteStep::forPrefixSubstitutions(t.size(), /*endOffset=*/0,
                                                   /*inverse=*/true));

      xv.back() = xv.back().prependPrefixToConcreteSubstitutions(
          t, Context);
    }

    // (3) Finally, apply the right hand side in the forward direction:
    //
    //     T.(UV => Y)
    path.add(RewriteStep::forRewriteRule(t.size(), /*endOffset=*/0,
                                         getRuleID(rhs),
                                         /*inverse=*/false));

    // If XV == TY, we have a trivial overlap.
    if (xv == ty) {
      loops.emplace_back(xv, path);
      return false;
    }

    // Add the pair (XV, TY).
    pairs.emplace_back(xv, ty, path);
  }

  return true;
}

/// Computes the confluent completion using the Knuth-Bendix algorithm and
/// returns a status code.
///
/// The first element of the pair is a status.
///
/// The status is CompletionResult::MaxRuleCount if we add more than
/// \p maxRuleCount rules.
///
/// The status is CompletionResult::MaxRuleLength if we produce a rewrite rule
/// whose left hand side has a length exceeding \p maxRuleLength.
///
/// In the above two cases, the second element of the pair is a rule ID.
///
/// Otherwise, the status is CompletionResult::Success and the second element
/// is zero.
std::pair<CompletionResult, unsigned>
RewriteSystem::computeConfluentCompletion(unsigned maxRuleCount,
                                          unsigned maxRuleLength) {
  assert(Initialized);
  assert(!Minimized);
  assert(!Frozen);

  // 'Complete' might already be set, if we're re-running completion after
  // adding new rules in the property map's concrete type unification procedure.
  Complete = 1;

  unsigned ruleCount;

  std::vector<CriticalPair> resolvedCriticalPairs;
  std::vector<RewriteLoop> resolvedLoops;

  do {
    ruleCount = Rules.size();

    // For every rule, look for other rules that overlap with this rule.
    for (unsigned i = FirstLocalRule, e = Rules.size(); i < e; ++i) {
      const auto &lhs = getRule(i);
      if (lhs.isLHSSimplified() ||
          lhs.isRHSSimplified() ||
          lhs.isSubstitutionSimplified())
        continue;

      // Look up every suffix of this rule in the trie using findAll(). This
      // will find both kinds of overlap:
      //
      // 1) rules whose left hand side is fully contained in [from,to)
      // 2) rules whose left hand side has a prefix equal to [from,to)
      auto from = lhs.getLHS().begin();
      auto to = lhs.getLHS().end();
      while (from < to) {
        Trie.findAll(from, to, [&](unsigned j) {
          const auto &rhs = getRule(j);
          if (rhs.isLHSSimplified() ||
              rhs.isRHSSimplified() ||
              rhs.isSubstitutionSimplified())
            return;

          if (from == lhs.getLHS().begin()) {
            // While every rule will have an overlap of the first kind
            // with itself, it's not useful to consider since the
            // resulting critical pair is always trivial.
            if (i == j)
              return;

            // If the first rule's left hand side is a proper prefix
            // of the second rule's left hand side, don't do anything.
            //
            // We will find the 'opposite' overlap later, where the two
            // rules are swapped around. Then it becomes an overlap of
            // the first kind, and will be handled as such.
            if (rhs.getLHS().size() > lhs.getLHS().size())
              return;
          }

          // We don't have to consider the same pair of rules more than once,
          // since those critical pairs were already resolved.
          if (!CheckedOverlaps.insert(std::make_pair(i, j)).second)
            return;

          // Try to repair the confluence violation by adding a new rule.
          if (computeCriticalPair(from, lhs, rhs,
                                  resolvedCriticalPairs,
                                  resolvedLoops)) {
            if (Debug.contains(DebugFlags::Completion)) {
              const auto &pair = resolvedCriticalPairs.back();

              llvm::dbgs() << "$ Overlapping rules: (#" << i << ") ";
              llvm::dbgs() << lhs << "\n";
              llvm::dbgs() << "                -vs- (#" << j << ") ";
              llvm::dbgs() << rhs << ":\n";
              llvm::dbgs() << "$$ First term of critical pair is "
                           << pair.LHS << "\n";
              llvm::dbgs() << "$$ Second term of critical pair is "
                           << pair.RHS << "\n\n";

              llvm::dbgs() << "$$ Resolved via path: ";
              pair.Path.dump(llvm::dbgs(), pair.LHS, *this);
              llvm::dbgs() << "\n\n";
            }
          } else {
            if (Debug.contains(DebugFlags::Completion)) {
              const auto &loop = resolvedLoops.back();

              llvm::dbgs() << "$ Trivially overlapping rules: (#" << i << ") ";
              llvm::dbgs() << lhs << "\n";
              llvm::dbgs() << "                          -vs- (#" << j << ") ";
              llvm::dbgs() << rhs << ":\n";

              llvm::dbgs() << "$$ Loop: ";
              loop.dump(llvm::dbgs(), *this);
              llvm::dbgs() << "\n\n";
            }
          }
        });

        ++from;
      }
    }

    assert(ruleCount == Rules.size());

    simplifyLeftHandSides();

    for (const auto &pair : resolvedCriticalPairs) {
      // Check if we've already done too much work.
      if (getLocalRules().size() > maxRuleCount)
        return std::make_pair(CompletionResult::MaxRuleCount, Rules.size() - 1);

      if (!addRule(pair.LHS, pair.RHS, &pair.Path))
        continue;

      // Check if the new rule is too long.
      if (Rules.back().getDepth() > maxRuleLength + getLongestInitialRule())
        return std::make_pair(CompletionResult::MaxRuleLength, Rules.size() - 1);
    }

    for (const auto &loop : resolvedLoops) {
      recordRewriteLoop(loop.Basepoint, loop.Path);
    }

    resolvedCriticalPairs.clear();
    resolvedLoops.clear();

    simplifyRightHandSides();
    simplifyLeftHandSideSubstitutions(/*map=*/nullptr);
  } while (Rules.size() > ruleCount);

  return std::make_pair(CompletionResult::Success, 0);
}
