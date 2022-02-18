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
//
// This implements the PropertyBag::addProperty() method, which merges layout,
// superclass and concrete type requirements. This merging can create new rules;
// property map construction is iterated with the Knuth-Bendix completion
// procedure until fixed point.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/Types.h"
#include <algorithm>
#include <vector>
#include "PropertyMap.h"

using namespace swift;
using namespace rewriting;

/// Returns true if we have not processed this rule before.
bool PropertyMap::checkRuleOnce(unsigned ruleID) {
  return CheckedRules.insert(ruleID).second;
}

/// Returns true if we have not processed this pair of rules before.
bool PropertyMap::checkRulePairOnce(unsigned firstRuleID,
                                    unsigned secondRuleID) {
  return CheckedRulePairs.insert(
      std::make_pair(firstRuleID, secondRuleID)).second;
}

/// Given a key T, a rule (V.[p1] => V) where T == U.V, and a property [p2]
/// where [p1] < [p2], record a rule (T.[p2] => T) that is induced by
/// the original rule (V.[p1] => V).
///
/// This is used to define rewrite loops for relating pairs of rules where
/// one implies another:
///
/// - a more specific layout constraint implies a general layout constraint
/// - a more specific superclass bound implies a less specific superclass bound
/// - a superclass bound implies a layout constraint
/// - a concrete type that is a class implies a superclass bound
/// - a concrete type that is a class implies a layout constraint
///
static void recordRelation(Term key,
                           unsigned lhsRuleID,
                           Symbol rhsProperty,
                           RewriteSystem &system,
                           bool debug) {
  const auto &lhsRule = system.getRule(lhsRuleID);
  auto lhsProperty = lhsRule.getLHS().back();

  assert(key.size() >= lhsRule.getRHS().size());

  assert((lhsProperty.getKind() == Symbol::Kind::Layout &&
          rhsProperty.getKind() == Symbol::Kind::Layout) ||
         (lhsProperty.getKind() == Symbol::Kind::Superclass &&
          rhsProperty.getKind() == Symbol::Kind::Superclass) ||
         (lhsProperty.getKind() == Symbol::Kind::Superclass &&
          rhsProperty.getKind() == Symbol::Kind::Layout) ||
         (lhsProperty.getKind() == Symbol::Kind::ConcreteType &&
          rhsProperty.getKind() == Symbol::Kind::Superclass) ||
         (lhsProperty.getKind() == Symbol::Kind::ConcreteType &&
          rhsProperty.getKind() == Symbol::Kind::Layout));

  if (debug) {
    llvm::dbgs() << "%% Recording relation: ";
    llvm::dbgs() << lhsRule.getLHS() << " < " << rhsProperty << "\n";
  }

  unsigned relationID = system.recordRelation(lhsProperty, rhsProperty);

  // Build the following rewrite path:
  //
  //   U.(V => V.[p1]).[p2] ⊗ U.V.Relation([p1].[p2] => [p1]) ⊗ U.(V.[p1] => V).
  //
  RewritePath path;

  // Starting from U.V.[p2], apply the rule in reverse to get U.V.[p1].[p2].
  path.add(RewriteStep::forRewriteRule(
      /*startOffset=*/key.size() - lhsRule.getRHS().size(),
      /*endOffset=*/1,
      /*ruleID=*/lhsRuleID,
      /*inverse=*/true));

  // U.V.Relation([p1].[p2] => [p1]).
  path.add(RewriteStep::forRelation(/*startOffset=*/key.size(),
                                    relationID, /*inverse=*/false));

  // U.(V.[p1] => V).
  path.add(RewriteStep::forRewriteRule(
      /*startOffset=*/key.size() - lhsRule.getRHS().size(),
      /*endOffset=*/0,
      /*ruleID=*/lhsRuleID,
      /*inverse=*/false));

  // Add the rule (T.[p2] => T) with the above rewrite path.
  MutableTerm lhs(key);
  lhs.add(rhsProperty);

  MutableTerm rhs(key);

  (void) system.addRule(lhs, rhs, &path);
}

/// Given a term T == U.V, an existing rule (V.[C] => V) and a new rule
/// (U.V.[D] => U.V) where [C] and [D] are understood to be two property
/// symbols in conflict with each other, mark the new rule as conflicting,
/// and if the existing rule applies to the entire term T (that is, if
/// |U| == 0) also mark the existing rule as conflicting.
static void recordConflict(Term key,
                           unsigned existingRuleID,
                           unsigned newRuleID,
                           RewriteSystem &system) {
  auto &existingRule = system.getRule(existingRuleID);
  auto &newRule = system.getRule(newRuleID);

  auto existingKind = existingRule.isPropertyRule()->getKind();
  auto newKind = newRule.isPropertyRule()->getKind();

  // The GSB only dropped the new rule in the case of a conflicting
  // superclass requirement, so maintain that behavior here.
  if (existingKind != Symbol::Kind::Superclass &&
      existingKind == newKind) {
    if (existingRule.getRHS().size() == key.size())
      existingRule.markConflicting();
  }

  assert(newRule.getRHS().size() == key.size());
  newRule.markConflicting();
}

void PropertyMap::addConformanceProperty(
    Term key, Symbol property, unsigned ruleID) {
  auto *props = getOrCreateProperties(key);
  props->ConformsTo.push_back(property.getProtocol());
  props->ConformsToRules.push_back(ruleID);
}

void PropertyMap::addLayoutProperty(
    Term key, Symbol property, unsigned ruleID) {
  auto *props = getOrCreateProperties(key);
  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  auto newLayout = property.getLayoutConstraint();

  if (!props->Layout) {
    // If we haven't seen a layout requirement before, just record it.
    props->Layout = newLayout;
    props->LayoutRule = ruleID;
    return;
  }

  // Otherwise, compute the intersection.
  assert(props->LayoutRule.hasValue());
  auto mergedLayout = props->Layout.merge(property.getLayoutConstraint());

  // If the intersection is invalid, we have a conflict.
  if (!mergedLayout->isKnownLayout()) {
    recordConflict(key, *props->LayoutRule, ruleID, System);
    return;
  }

  // If the intersection is equal to the existing layout requirement,
  // the new layout requirement is redundant.
  if (mergedLayout == props->Layout) {
    if (checkRulePairOnce(*props->LayoutRule, ruleID)) {
      recordRelation(key, *props->LayoutRule, property, System, debug);
    }

  // If the intersection is equal to the new layout requirement, the
  // existing layout requirement is redundant.
  } else if (mergedLayout == newLayout) {
    if (checkRulePairOnce(ruleID, *props->LayoutRule)) {
      auto oldProperty = System.getRule(*props->LayoutRule).getLHS().back();
      recordRelation(key, ruleID, oldProperty, System, debug);
    }

    props->LayoutRule = ruleID;
  } else {
    llvm::errs() << "Arbitrary intersection of layout requirements is "
                 << "supported yet\n";
    abort();
  }
}

/// Given a term T == U.V, an existing rule (V.[superclass: C] => V), and
/// a superclass declaration D of C, record a new rule (T.[superclass: C'] => T)
/// where C' is the substituted superclass type of C for D.
///
/// For example, suppose we have
///
///    class Derived : Base<Int> {}
///    class Base<T> {}
///
/// Given C == Derived and D == Base, then C' == Base<Int>.
void PropertyMap::recordSuperclassRelation(Term key,
                                           Symbol superclassType,
                                           unsigned superclassRuleID,
                                           const ClassDecl *otherClass) {
  auto derivedType = superclassType.getConcreteType();
  assert(otherClass->isSuperclassOf(derivedType->getClassOrBoundGenericClass()));

  auto baseType = derivedType->getSuperclassForDecl(otherClass)
      ->getCanonicalType();

  SmallVector<Term, 3> baseSubstitutions;
  auto baseSchema = Context.getRelativeSubstitutionSchemaFromType(
      baseType, superclassType.getSubstitutions(),
      baseSubstitutions);

  auto baseSymbol = Symbol::forSuperclass(baseSchema, baseSubstitutions,
                                          Context);

  bool debug = Debug.contains(DebugFlags::ConcreteUnification);
  recordRelation(key, superclassRuleID, baseSymbol, System, debug);
}

/// When a type parameter has two superclasses, we have to both unify the
/// type constructor arguments, and record the most derived superclass.
///
/// For example, if we have this setup:
///
///   class Base<T, T> {}
///   class Middle<U> : Base<T, T> {}
///   class Derived : Middle<Int> {}
///
///   T : Base<U, V>
///   T : Derived
///
/// The most derived superclass requirement is 'T : Derived'.
///
/// The corresponding superclass of 'Derived' is 'Base<Int, Int>', so we
/// unify the type constructor arguments of 'Base<U, V>' and 'Base<Int, Int>',
/// which generates two induced rules:
///
///   U.[concrete: Int] => U
///   V.[concrete: Int] => V
void PropertyMap::addSuperclassProperty(
    Term key, Symbol property, unsigned ruleID) {
  auto *props = getOrCreateProperties(key);
  auto &newRule = System.getRule(ruleID);
  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  const auto *superclassDecl = property.getConcreteType()
      ->getClassOrBoundGenericClass();
  assert(superclassDecl != nullptr);

  if (checkRuleOnce(ruleID)) {
    // A rule (T.[superclass: C] => T) induces a rule (T.[layout: L] => T),
    // where L is either AnyObject or _NativeObject.
    auto layout =
        LayoutConstraint::getLayoutConstraint(
          superclassDecl->getLayoutConstraintKind(),
          Context.getASTContext());
    auto layoutSymbol = Symbol::forLayout(layout, Context);

    recordRelation(key, ruleID, layoutSymbol, System, debug);
  }

  // If this is the first superclass requirement we've seen for this term,
  // just record it and we're done.
  if (!props->SuperclassDecl) {
    if (debug) {
      llvm::dbgs() << "% New superclass " << superclassDecl->getName()
                   << " for " << key << "\n";
    }

    props->SuperclassDecl = superclassDecl;

    assert(props->Superclasses.empty());
    auto &req = props->Superclasses[superclassDecl];

    assert(!req.SuperclassType.hasValue());
    assert(!req.SuperclassRule.hasValue());

    req.SuperclassType = property;
    req.SuperclassRule = ruleID;
    return;
  }

  if (debug) {
    llvm::dbgs() << "% New superclass " << superclassDecl->getName()
                 << " for " << key << " is ";
  }

  // Otherwise, we compare it against the existing superclass requirement.
  assert(!props->Superclasses.empty());

  if (superclassDecl == props->SuperclassDecl) {
    if (debug) {
      llvm::dbgs() << "equal to existing superclass\n";
    }

    // Perform concrete type unification at this level of the class
    // hierarchy.
    auto &req = props->Superclasses[superclassDecl];
    assert(req.SuperclassType.hasValue());
    assert(req.SuperclassRule.hasValue());

    unifyConcreteTypes(key, req.SuperclassType, req.SuperclassRule,
                       property, ruleID);

  } else if (superclassDecl->isSuperclassOf(props->SuperclassDecl)) {
    if (debug) {
      llvm::dbgs() << "less specific than existing superclass "
                   << props->SuperclassDecl->getName() << "\n";
    }

    // Record a relation where existing superclass implies the new superclass.
    const auto &existingReq = props->Superclasses[props->SuperclassDecl];
    if (checkRulePairOnce(*existingReq.SuperclassRule, ruleID)) {
      recordSuperclassRelation(key,
                               *existingReq.SuperclassType,
                               *existingReq.SuperclassRule,
                               superclassDecl);
    }

    // Record the new rule at the less specific level of the class
    // hierarchy, performing concrete type unification if we've
    // already seen another rule at that level.
    auto &req = props->Superclasses[superclassDecl];

    unifyConcreteTypes(key, req.SuperclassType, req.SuperclassRule,
                       property, ruleID);

  } else if (props->SuperclassDecl->isSuperclassOf(superclassDecl)) {
    if (debug) {
      llvm::dbgs() << "more specific than existing superclass "
                   << props->SuperclassDecl->getName() << "\n";
    }

    // Record a relation where new superclass implies the existing superclass.
    const auto &existingReq = props->Superclasses[props->SuperclassDecl];
    if (checkRulePairOnce(*existingReq.SuperclassRule, ruleID)) {
      recordSuperclassRelation(key, property, ruleID,
                               props->SuperclassDecl);
    }

    // Record the new rule at the more specific level of the class
    // hierarchy.
    auto &req = props->Superclasses[superclassDecl];
    assert(!req.SuperclassType.hasValue());
    assert(!req.SuperclassRule.hasValue());

    req.SuperclassType = property;
    req.SuperclassRule = ruleID;

    props->SuperclassDecl = superclassDecl;

  } else {
    if (debug) {
      llvm::dbgs() << "not related to existing superclass "
                   << props->SuperclassDecl->getName() << "\n";
    }

    newRule.markConflicting();
  }
}


/// Utility used by addSuperclassProperty() and addConcreteTypeProperty().
void PropertyMap::unifyConcreteTypes(
    Term key,
    Optional<Symbol> &existingProperty,
    Optional<unsigned> &existingRuleID,
    Symbol property,
    unsigned ruleID) {
  auto &rule = System.getRule(ruleID);
  assert(rule.getRHS() == key);

  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  if (!existingProperty.hasValue()) {
    existingProperty = property;
    existingRuleID = ruleID;
    return;
  }

  assert(existingRuleID.hasValue());

  if (debug) {
    llvm::dbgs() << "% Unifying " << *existingProperty
                 << " with " << property << "\n";
  }

  Optional<unsigned> lhsDifferenceID;
  Optional<unsigned> rhsDifferenceID;

  bool conflict = System.computeTypeDifference(key,
                                               *existingProperty, property,
                                               lhsDifferenceID,
                                               rhsDifferenceID);

  if (conflict) {
    // FIXME: Diagnose the conflict
    if (debug) {
      llvm::dbgs() << "%% Concrete type conflict\n";
    }
    recordConflict(key, *existingRuleID, ruleID, System);
    return;
  }

  // Handle the case where (LHS ∧ RHS) is distinct from both LHS and RHS:
  // - First, record a new rule.
  // - Next, process the LHS -> (LHS ∧ RHS) difference.
  // - Finally, process the RHS -> (LHS ∧ RHS) difference.
  if (lhsDifferenceID && rhsDifferenceID) {
    const auto &lhsDifference = System.getTypeDifference(*lhsDifferenceID);
    const auto &rhsDifference = System.getTypeDifference(*rhsDifferenceID);

    auto newProperty = lhsDifference.RHS;
    assert(newProperty == rhsDifference.RHS);

    MutableTerm rhsTerm(key);
    MutableTerm lhsTerm(key);
    lhsTerm.add(newProperty);

    if (checkRulePairOnce(*existingRuleID, ruleID)) {
      assert(lhsDifference.RHS == rhsDifference.RHS);

      if (debug) {
        llvm::dbgs() << "%% Induced rule " << lhsTerm
                     << " == " << rhsTerm << "\n";
      }

      // This rule does not need a rewrite path because it will be related
      // to the two existing rules by the processTypeDifference() calls below.
      System.addRule(lhsTerm, rhsTerm);

      // Recover a rewrite path from T to T.[LHS ∧ RHS].
      RewritePath path;
      System.buildRewritePathForJoiningTerms(rhsTerm, lhsTerm, &path);

      // Process LHS -> (LHS ∧ RHS).
      System.processTypeDifference(lhsDifference, *lhsDifferenceID,
                                   *existingRuleID, path);

      // Process RHS -> (LHS ∧ RHS).
      System.processTypeDifference(rhsDifference, *rhsDifferenceID,
                                   ruleID, path);
    }

    // The new property is more specific, so update ConcreteType and
    // ConcreteTypeRule.
    existingProperty = newProperty;
    existingRuleID = ruleID;

    return;
  }

  // Handle the case where RHS == (LHS ∧ RHS) by processing LHS -> (LHS ∧ RHS).
  if (lhsDifferenceID) {
    assert(!rhsDifferenceID);

    const auto &lhsDifference = System.getTypeDifference(*lhsDifferenceID);
    assert(*existingProperty == lhsDifference.LHS);
    assert(property == lhsDifference.RHS);

    if (checkRulePairOnce(*existingRuleID, ruleID)) {
      // Build a rewrite path (T.[RHS] => T).
      RewritePath path;
      path.add(RewriteStep::forRewriteRule(
          /*startOffset=*/0, /*endOffset=*/0,
          /*ruleID=*/ruleID, /*inverse=*/false));

      System.processTypeDifference(lhsDifference, *lhsDifferenceID,
                                   *existingRuleID, path);
    }

    // The new property is more specific, so update existingProperty and
    // existingRuleID.
    existingProperty = property;
    existingRuleID = ruleID;

    return;
  }

  // Handle the case where LHS == (LHS ∧ RHS) by processing RHS -> (LHS ∧ RHS).
  if (rhsDifferenceID) {
    assert(!lhsDifferenceID);

    const auto &rhsDifference = System.getTypeDifference(*rhsDifferenceID);
    assert(property == rhsDifference.LHS);
    assert(*existingProperty == rhsDifference.RHS);

    if (checkRulePairOnce(*existingRuleID, ruleID)) {
      // Build a rewrite path (T.[LHS] => T).
      RewritePath path;
      unsigned lhsLength = System.getRule(*existingRuleID).getRHS().size();
      unsigned prefix = key.size() - lhsLength;

      if (prefix > 0) {
        path.add(RewriteStep::forPrefixSubstitutions(
            prefix, /*endOffset=*/0, /*inverse=*/true));
      }

      path.add(RewriteStep::forRewriteRule(
          /*startOffset=*/prefix, /*endOffset=*/0,
          /*ruleID=*/*existingRuleID, /*inverse=*/false));

      System.processTypeDifference(rhsDifference, *rhsDifferenceID,
                                   ruleID, path);
    }

    // The new property is less specific, so existingProperty and existingRuleID
    // remain unchanged.
    return;
  }

  assert(property == *existingProperty);

  if (*existingRuleID != ruleID) {
    // If the rules are different but the concrete types are identical, then
    // the key is some term U.V, the existing rule is a rule of the form:
    //
    //    V.[concrete: G<...> with <X, Y>]
    //
    // and the new rule is a rule of the form:
    //
    //    U.V.[concrete: G<...> with <U.X, U.Y>]
    //
    // Record a loop relating the two rules via a rewrite step to prefix 'U' to
    // the symbol's substitutions.
    //
    // Since the new rule appears without context, it becomes redundant.
    if (checkRulePairOnce(*existingRuleID, ruleID)) {
      RewritePath rhsPath;
      rhsPath.add(RewriteStep::forRewriteRule(
          /*startOffset=*/0, /*endOffset=*/0,
          /*ruleID=*/ruleID, /*inverse=*/false));

      RewritePath path;
      System.buildRewritePathForUnifier(key, *existingRuleID, rhsPath, &path);
      System.recordRewriteLoop(MutableTerm(rule.getLHS()), path);

      rule.markSubstitutionSimplified();
    }
  }
}

/// When a type parameter has two concrete types, we have to unify the
/// type constructor arguments.
///
/// For example, suppose that we have two concrete same-type requirements:
///
///   T == Foo<X.Y, Z, String>
///   T == Foo<Int, A.B, W>
///
/// These lower to the following two rules:
///
///   T.[concrete: Foo<τ_0_0, τ_0_1, String> with {X.Y, Z}] => T
///   T.[concrete: Foo<Int, τ_0_0, τ_0_1> with {A.B, W}] => T
///
/// The two concrete type symbols will be added to the property bag of 'T',
/// and we will eventually end up in this method, where we will generate three
/// induced rules:
///
///   X.Y.[concrete: Int] => X.Y
///   A.B => Z
///   W.[concrete: String] => W
void PropertyMap::addConcreteTypeProperty(
    Term key, Symbol property, unsigned ruleID) {
  auto *props = getOrCreateProperties(key);

  unifyConcreteTypes(key, props->ConcreteType, props->ConcreteTypeRule,
                     property, ruleID);
}

/// Record a protocol conformance, layout or superclass constraint on the given
/// key. Must be called in monotonically non-decreasing key order.
void PropertyMap::addProperty(
    Term key, Symbol property, unsigned ruleID) {
  assert(property.isProperty());
  assert(*System.getRule(ruleID).isPropertyRule() == property);

  switch (property.getKind()) {
  case Symbol::Kind::Protocol:
    addConformanceProperty(key, property, ruleID);
    return;

  case Symbol::Kind::Layout:
    addLayoutProperty(key, property, ruleID);
    return;

  case Symbol::Kind::Superclass:
    addSuperclassProperty(key, property, ruleID);
    return;

  case Symbol::Kind::ConcreteType:
    addConcreteTypeProperty(key, property, ruleID);
    return;

  case Symbol::Kind::ConcreteConformance:
    // Concrete conformance rules are not recorded in the property map, since
    // they're not needed for unification, and generic signature queries don't
    // care about them.
    return;

  case Symbol::Kind::Name:
  case Symbol::Kind::GenericParam:
  case Symbol::Kind::AssociatedType:
    break;
  }

  llvm_unreachable("Bad symbol kind");
}

/// Post-pass to handle unification and conflict checking between pairs of
/// rules of different kinds:
///
/// - concrete vs superclass
/// - concrete vs layout
void PropertyMap::checkConcreteTypeRequirements() {
  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  for (auto *props : Entries) {
    if (props->isConcreteType()) {
      auto concreteType = props->ConcreteType->getConcreteType();

      // A rule (T.[concrete: C] => T) where C is a class type induces a rule
      // (T.[superclass: C] => T).
      if (concreteType->getClassOrBoundGenericClass()) {
        auto superclassSymbol = Symbol::forSuperclass(
            concreteType, props->ConcreteType->getSubstitutions(),
            Context);

        recordRelation(props->getKey(), *props->ConcreteTypeRule,
                       superclassSymbol, System, debug);

      // If the concrete type is not a class and we have a superclass
      // requirement, we have a conflict.
      } else if (props->hasSuperclassBound()) {
        const auto &req = props->getSuperclassRequirement();
        recordConflict(props->getKey(),
                       *props->ConcreteTypeRule,
                       *req.SuperclassRule, System);
      }

      // A rule (T.[concrete: C] => T) where C is a class type induces a rule
      // (T.[layout: L] => T), where L is either AnyObject or _NativeObject.
      if (concreteType->satisfiesClassConstraint()) {
        Type superclassType = concreteType;
        if (!concreteType->getClassOrBoundGenericClass())
          superclassType = concreteType->getSuperclass();

        auto layoutConstraint = LayoutConstraintKind::Class;
        if (superclassType)
          if (auto *classDecl = superclassType->getClassOrBoundGenericClass())
            layoutConstraint = classDecl->getLayoutConstraintKind();

        auto layout =
            LayoutConstraint::getLayoutConstraint(
              layoutConstraint, Context.getASTContext());
        auto layoutSymbol = Symbol::forLayout(layout, Context);

        recordRelation(props->getKey(), *props->ConcreteTypeRule,
                       layoutSymbol, System, debug);

      // If the concrete type does not satisfy a class layout constraint and
      // we have such a layout requirement, we have a conflict.
      } else if (props->LayoutRule &&
                 props->Layout->isClass()) {
        recordConflict(props->getKey(),
                       *props->ConcreteTypeRule,
                       *props->LayoutRule, System);
      }
    }
  }
}
