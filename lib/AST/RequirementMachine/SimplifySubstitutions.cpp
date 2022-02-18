//===--- PropertyUnification.cpp - Rules added w/ building property map ---===//
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

#include "PropertyMap.h"
#include "RewriteSystem.h"

using namespace swift;
using namespace rewriting;

/// Given a rule (V.[LHS] => V) and a rewrite path (T.[RHS] => T) where
/// T == U.V, build a rewrite path from T.[RHS] to T.[LHS].
void RewriteSystem::buildRewritePathForUnifier(Term key,
                                               unsigned lhsRuleID,
                                               const RewritePath &rhsPath,
                                               RewritePath *path) const {
  unsigned lhsLength = getRule(lhsRuleID).getRHS().size();
  unsigned lhsPrefix = key.size() - lhsLength;

  path->append(rhsPath);

  // Apply the inverted rule U.(V => V.[LHS]).
  path->add(RewriteStep::forRewriteRule(
      /*startOffset=*/lhsPrefix, /*endOffset=*/0,
      /*ruleID=*/lhsRuleID, /*inverse=*/true));

  // If the rule was actually (V.[LHS] => V) with T == U.V for some
  // |U| > 0, prefix each substitution of [LHS] with U.
  if (lhsPrefix > 0) {
    path->add(RewriteStep::forPrefixSubstitutions(/*prefix=*/lhsPrefix,
                                                  /*endOffset=*/0,
                                                  /*inverse=*/false));
  }
}

/// Build a rewrite path for a rule induced by concrete type unification.
///
/// Consider two concrete type rules (T.[LHS] => T) and (T.[RHS] => T), a
/// TypeDifference describing the transformation from LHS to RHS, and the
/// index of a substitution Xn from [C] which is transformed into its
/// replacement f(Xn).
///
/// The rewrite path should allow us to eliminate the induced rule
/// (f(Xn) => Xn), so the induced rule will appear without context, and
/// the concrete type rules (T.[LHS] => T) and (T.[RHS] => T) will appear
/// in context.
///
/// There are two cases:
///
/// a) The substitution Xn remains a type parameter in [RHS], but becomes
///    a canonical term Xn', so f(Xn) = Xn'.
///
///    In the first case, the induced rule (Xn => Xn'), described by a
///    rewrite path as follows:
///
///    Xn
///    Xn' T.[RHS] // RightConcreteProjection(n) pushes T.[RHS]
///    Xn' T       // Application of (T.[RHS] => T) in context
///    Xn' T.[LHS] // Application of (T => T.[LHS]) in context
///    Xn'         // LeftConcreteProjection(n) pops T.[LHS]
///
///    Now when this path is composed with a rewrite step for the inverted
///    induced rule (Xn' => Xn), we get a rewrite loop at Xn in which the
///    new rule appears in empty context.
///
/// b) The substitution Xn becomes a concrete type [D] in [C'], so
///    f(Xn) = Xn.[D].
///
///    In the second case, the induced rule is (Xn.[D] => Xn), described
///    by a rewrite path (going in the other direction) as follows:
///
///    Xn
///    Xn.[D] T.[RHS] // RightConcreteProjection(n) pushes T.[RHS]
///    Xn.[D] T       // Application of (T.[RHS] => T) in context
///    Xn.[D] T.[LHS] // Application of (T => T.[LHS]) in context
///    Xn.[D]         // LeftConcreteProjection(n) pops T.[LHS]
///
///    Now when this path is composed with a rewrite step for the induced
///    rule (Xn.[D] => Xn), we get a rewrite loop at Xn in which the
///    new rule appears in empty context.
///
/// There is a minor complication; the concrete type rules T.[LHS] and
/// T.[RHS] might actually be T.[LHS] and V.[RHS] where V is a suffix of
/// T, so T = U.V for some |U| > 0, (or vice versa). In this case we need
/// an additional step in the middle to prefix the concrete substitutions
/// of [LHS] (or [LHS]) with U.
static void buildRewritePathForInducedRule(Term key,
                                           unsigned differenceID,
                                           unsigned lhsRuleID,
                                           const RewritePath &rhsPath,
                                           unsigned substitutionIndex,
                                           const RewriteSystem &system,
                                           RewritePath *path) {
  // Replace f(Xn) with Xn and push T.[RHS] on the stack.
  path->add(RewriteStep::forRightConcreteProjection(
      differenceID, substitutionIndex, /*inverse=*/false));

  system.buildRewritePathForUnifier(key, lhsRuleID, rhsPath, path);

  // Pop T.[LHS] from the stack, leaving behind Xn.
  path->add(RewriteStep::forLeftConcreteProjection(
      differenceID, substitutionIndex, /*inverse=*/true));
}

/// Given that LHS and RHS are known to simplify to the same term, build a
/// rewrite path from RHS to LHS.
void RewriteSystem::buildRewritePathForJoiningTerms(MutableTerm lhsTerm,
                                                    MutableTerm rhsTerm,
                                                    RewritePath *path) const {
  (void) simplify(rhsTerm, path);

  RewritePath lhsPath;
  (void) simplify(lhsTerm, &lhsPath);
  lhsPath.invert();

  path->append(lhsPath);

  assert(lhsTerm == rhsTerm);
}

/// Given two concrete type rules (T.[LHS] => T) and (T.[RHS] => T) and
/// TypeDifference describing the transformation from LHS to RHS,
/// record rules for transforming each substitution of LHS into a
/// more canonical type parameter or concrete type from RHS.
///
/// This also records rewrite paths relating induced rules to the original
/// concrete type rules, since the concrete type rules imply the induced
/// rules and make them redundant.
///
/// Finally, builds a rewrite loop relating the two concrete type rules
/// via the induced rules.
void RewriteSystem::processTypeDifference(const TypeDifference &difference,
                                          unsigned differenceID,
                                          unsigned lhsRuleID,
                                          const RewritePath &rhsPath) {
  if (!CheckedDifferences.insert(differenceID).second)
    return;

  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  if (debug) {
    difference.dump(llvm::dbgs());
  }

  RewritePath unificationPath;

  auto substitutions = difference.LHS.getSubstitutions();

  // The term is at the top of the primary stack. Push all substitutions onto
  // the primary stack.
  unificationPath.add(RewriteStep::forDecompose(substitutions.size(),
                                                /*inverse=*/false));

  // Move all substitutions but the first one to the secondary stack.
  for (unsigned i = 1; i < substitutions.size(); ++i)
    unificationPath.add(RewriteStep::forShift(/*inverse=*/false));

  for (unsigned index : indices(substitutions)) {
    // Move the next substitution from the secondary stack to the primary stack.
    if (index != 0)
      unificationPath.add(RewriteStep::forShift(/*inverse=*/true));

    auto lhsTerm = difference.getReplacementSubstitution(index);
    auto rhsTerm = difference.getOriginalSubstitution(index);

    RewritePath inducedRulePath;
    buildRewritePathForInducedRule(difference.BaseTerm, differenceID,
                                   lhsRuleID, rhsPath, index,
                                   *this, &inducedRulePath);

    if (debug) {
      llvm::dbgs() << "%% Induced rule " << lhsTerm
                   << " => " << rhsTerm << " with path ";
      inducedRulePath.dump(llvm::dbgs(), lhsTerm, *this);
      llvm::dbgs() << "\n";
    }

    addRule(lhsTerm, rhsTerm, &inducedRulePath);
    buildRewritePathForJoiningTerms(lhsTerm, rhsTerm, &unificationPath);
  }

  // All simplified substitutions are now on the primary stack. Collect them to
  // produce the new term.
  unificationPath.add(RewriteStep::forDecomposeConcrete(differenceID,
                                                        /*inverse=*/true));

  // We now have a unification path from T.[RHS] to T.[LHS] using the
  // newly-recorded induced rules. Close the loop with a path from
  // T.[RHS] to R.[LHS] via the concrete type rules being unified.
  buildRewritePathForUnifier(difference.BaseTerm, lhsRuleID, rhsPath,
                             &unificationPath);

  // Record a rewrite loop at T.[LHS].
  MutableTerm basepoint(difference.BaseTerm);
  basepoint.add(difference.LHS);
  recordRewriteLoop(basepoint, unificationPath);

  // Optimization: If the LHS rule applies to the entire base term and not
  // a suffix, mark it substitution-simplified so that we can skip recording
  // the same rewrite loop in concretelySimplifyLeftHandSideSubstitutions().
  auto &lhsRule = getRule(lhsRuleID);
  if (lhsRule.getRHS() == difference.BaseTerm &&
      !lhsRule.isSubstitutionSimplified())
    lhsRule.markSubstitutionSimplified();
}

/// Simplify terms appearing in the substitutions of the last symbol of \p term,
/// which must be a superclass or concrete type symbol.
///
/// Additionally, if \p map is non-null, any terms which become concrete types
/// will cause the corresponding generic parameter in the concrete type symbol
/// to be replaced.
///
/// Returns None if the concrete type symbol cannot be simplified further.
///
/// Otherwise returns an index which can be passed to
/// RewriteSystem::getTypeDifference().
Optional<unsigned>
RewriteSystem::simplifySubstitutions(Term baseTerm, Symbol symbol,
                                     const PropertyMap *map,
                                     RewritePath *path) {
  assert(symbol.hasSubstitutions());

  // Fast path if the type is fully concrete.
  auto substitutions = symbol.getSubstitutions();
  if (substitutions.empty())
    return None;

  // Save the original rewrite path length so that we can reset if if we don't
  // find anything to simplify.
  unsigned oldSize = (path ? path->size() : 0);

  if (path) {
    // The term is at the top of the primary stack. Push all substitutions onto
    // the primary stack.
    path->add(RewriteStep::forDecompose(substitutions.size(),
                                        /*inverse=*/false));

    // Move all substitutions but the first one to the secondary stack.
    for (unsigned i = 1; i < substitutions.size(); ++i)
      path->add(RewriteStep::forShift(/*inverse=*/false));
  }

  // Simplify and collect substitutions.
  llvm::SmallVector<std::pair<unsigned, Term>, 1> sameTypes;
  llvm::SmallVector<std::pair<unsigned, Symbol>, 1> concreteTypes;

  for (unsigned index : indices(substitutions)) {
    // Move the next substitution from the secondary stack to the primary stack.
    if (index != 0 && path)
      path->add(RewriteStep::forShift(/*inverse=*/true));

    auto term = symbol.getSubstitutions()[index];
    MutableTerm mutTerm(term);

    // Note that it's of course possible that the term both requires
    // simplification, and the simplified term has a concrete type.
    //
    // This isn't handled with our current representation of
    // TypeDifference, but that should be fine since the caller
    // has to iterate until fixed point anyway.
    //
    // This should be rare in practice.
    if (simplify(mutTerm, path)) {
      // Record a mapping from this substitution to the simplified term.
      sameTypes.emplace_back(index, Term::get(mutTerm, Context));

    } else if (map) {
      auto *props = map->lookUpProperties(mutTerm);

      if (props && props->isConcreteType()) {
        auto concreteSymbol = props->concretelySimplifySubstitution(
            mutTerm, Context, path);

        // Record a mapping from this substitution to the concrete type.
        concreteTypes.emplace_back(index, concreteSymbol);
      }
    }
  }

  // If nothing changed, we don't have to build the type difference.
  if (sameTypes.empty() && concreteTypes.empty()) {
    if (path) {
      // The rewrite path should consist of a Decompose, followed by a number
      // of Shifts, followed by a Compose.
  #ifndef NDEBUG
      for (auto iter = path->begin() + oldSize; iter < path->end(); ++iter) {
        assert(iter->Kind == RewriteStep::Shift ||
               iter->Kind == RewriteStep::Decompose);
      }
  #endif

      path->resize(oldSize);
    }
    return None;
  }

  auto difference = buildTypeDifference(baseTerm, symbol,
                                        sameTypes, concreteTypes,
                                        Context);
  assert(difference.LHS != difference.RHS);

  unsigned differenceID = recordTypeDifference(difference);

  // All simplified substitutions are now on the primary stack. Collect them to
  // produce the new term.
  if (path) {
    path->add(RewriteStep::forDecomposeConcrete(differenceID,
                                                /*inverse=*/true));
  }

  return differenceID;
}

/// Simplify substitution terms in superclass, concrete type and concrete
/// conformance symbols.
///
/// During completion, \p map will be null. After completion, the property map
/// is built, and a final simplification pass is performed with \p map set to
/// the new property map.
void RewriteSystem::simplifyLeftHandSideSubstitutions(const PropertyMap *map) {
  for (unsigned ruleID = 0, e = Rules.size(); ruleID < e; ++ruleID) {
    auto &rule = getRule(ruleID);
    if (rule.isSubstitutionSimplified())
      continue;

    auto optSymbol = rule.isPropertyRule();
    if (!optSymbol || !optSymbol->hasSubstitutions())
      continue;

    auto symbol = *optSymbol;

    auto differenceID = simplifySubstitutions(rule.getRHS(), symbol, map);
    if (!differenceID)
      continue;

    auto difference = getTypeDifference(*differenceID);
    assert(difference.LHS == symbol);
    assert(difference.RHS != symbol);

    MutableTerm rhs(rule.getRHS());
    MutableTerm lhs(rhs);
    lhs.add(difference.RHS);

    addRule(lhs, rhs);

    RewritePath path;
    buildRewritePathForJoiningTerms(rhs, lhs, &path);

    processTypeDifference(difference, *differenceID, ruleID, path);
  }
}