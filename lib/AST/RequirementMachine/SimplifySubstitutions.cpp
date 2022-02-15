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

    RewritePath path;

    auto differenceID = simplifySubstitutions(rule.getRHS(), symbol, map, &path);
    if (!differenceID)
      continue;

    rule.markSubstitutionSimplified();

    auto difference = getTypeDifference(*differenceID);
    assert(difference.LHS == symbol);

    // If the original rule is (T.[concrete: C] => T) and [concrete: C'] is
    // the simplified symbol, then difference.LHS == [concrete: C] and
    // difference.RHS == [concrete: C'], and the rewrite path we just
    // built takes T.[concrete: C] to T.[concrete: C'].
    //
    // We want a path from T.[concrete: C'] to T, so invert the path to get
    // a path from T.[concrete: C'] to T.[concrete: C], and add a final step
    // applying the original rule (T.[concrete: C] => T).
    path.invert();
    path.add(RewriteStep::forRewriteRule(/*startOffset=*/0,
                                         /*endOffset=*/0,
                                         /*ruleID=*/ruleID,
                                         /*inverted=*/false));
    MutableTerm rhs(rule.getRHS());
    MutableTerm lhs(rhs);
    lhs.add(difference.RHS);

    addRule(lhs, rhs, &path);
  }
}