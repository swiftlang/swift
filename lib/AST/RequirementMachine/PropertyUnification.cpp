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

namespace {
  /// Utility class used by unifyConcreteTypes() and unifySuperclasses()
  /// to walk two concrete types in parallel. Any time there is a mismatch,
  /// records a new induced rule.
  class ConcreteTypeMatcher : public TypeMatcher<ConcreteTypeMatcher> {
    ArrayRef<Term> lhsSubstitutions;
    ArrayRef<Term> rhsSubstitutions;
    RewriteContext &ctx;
    RewriteSystem &system;
    bool debug;

  public:
    ConcreteTypeMatcher(ArrayRef<Term> lhsSubstitutions,
                        ArrayRef<Term> rhsSubstitutions,
                        RewriteSystem &system,
                        bool debug)
        : lhsSubstitutions(lhsSubstitutions),
          rhsSubstitutions(rhsSubstitutions),
          ctx(system.getRewriteContext()), system(system),
          debug(debug) {}

    bool alwaysMismatchTypeParameters() const { return true; }

    bool mismatch(TypeBase *firstType, TypeBase *secondType,
                  Type sugaredFirstType) {
      bool firstAbstract = firstType->isTypeParameter();
      bool secondAbstract = secondType->isTypeParameter();

      if (firstAbstract && secondAbstract) {
        // Both sides are type parameters; add a same-type requirement.
        auto lhsTerm = ctx.getRelativeTermForType(CanType(firstType),
                                                  lhsSubstitutions);
        auto rhsTerm = ctx.getRelativeTermForType(CanType(secondType),
                                                  rhsSubstitutions);
        if (lhsTerm != rhsTerm) {
          if (debug) {
            llvm::dbgs() << "%% Induced rule " << lhsTerm
                         << " == " << rhsTerm << "\n";
          }

          // FIXME: Need a rewrite path here.
          (void) system.addRule(lhsTerm, rhsTerm);
        }
        return true;
      }

      if (firstAbstract && !secondAbstract) {
        // A type parameter is equated with a concrete type; add a concrete
        // type requirement.
        auto subjectTerm = ctx.getRelativeTermForType(CanType(firstType),
                                                      lhsSubstitutions);

        SmallVector<Term, 3> result;
        auto concreteType = ctx.getRelativeSubstitutionSchemaFromType(
            CanType(secondType), rhsSubstitutions, result);

        MutableTerm constraintTerm(subjectTerm);
        constraintTerm.add(Symbol::forConcreteType(concreteType, result, ctx));

        if (debug) {
          llvm::dbgs() << "%% Induced rule " << subjectTerm
                       << " == " << constraintTerm << "\n";
        }

        // FIXME: Need a rewrite path here.
        (void) system.addRule(subjectTerm, constraintTerm);
        return true;
      }

      if (!firstAbstract && secondAbstract) {
        // A concrete type is equated with a type parameter; add a concrete
        // type requirement.
        auto subjectTerm = ctx.getRelativeTermForType(CanType(secondType),
                                                      rhsSubstitutions);

        SmallVector<Term, 3> result;
        auto concreteType = ctx.getRelativeSubstitutionSchemaFromType(
            CanType(firstType), lhsSubstitutions, result);

        MutableTerm constraintTerm(subjectTerm);
        constraintTerm.add(Symbol::forConcreteType(concreteType, result, ctx));

        if (debug) {
          llvm::dbgs() << "%% Induced rule " << subjectTerm
                       << " == " << constraintTerm << "\n";
        }

        // FIXME: Need a rewrite path here.
        (void) system.addRule(subjectTerm, constraintTerm);
        return true;
      }

      // Any other kind of type mismatch involves conflicting concrete types on
      // both sides, which can only happen on invalid input.
      return false;
    }
  };
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
///
/// Returns the left hand side on success (it could also return the right hand
/// side; since we unified the type constructor arguments, it doesn't matter).
///
/// Returns true if a conflict was detected.
static bool unifyConcreteTypes(
    Symbol lhs, Symbol rhs, RewriteSystem &system,
    bool debug) {
  auto lhsType = lhs.getConcreteType();
  auto rhsType = rhs.getConcreteType();

  if (debug) {
    llvm::dbgs() << "% Unifying " << lhs << " with " << rhs << "\n";
  }

  ConcreteTypeMatcher matcher(lhs.getSubstitutions(),
                              rhs.getSubstitutions(),
                              system, debug);
  if (!matcher.match(lhsType, rhsType)) {
    // FIXME: Diagnose the conflict
    if (debug) {
      llvm::dbgs() << "%% Concrete type conflict\n";
    }
    return true;
  }

  return false;
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
///
/// Returns the most derived superclass, which becomes the new superclass
/// that gets recorded in the property map.
static std::pair<Symbol, bool> unifySuperclasses(
    Symbol lhs, Symbol rhs, RewriteSystem &system,
    bool debug) {
  if (debug) {
    llvm::dbgs() << "% Unifying " << lhs << " with " << rhs << "\n";
  }

  auto lhsType = lhs.getConcreteType();
  auto rhsType = rhs.getConcreteType();

  auto *lhsClass = lhsType.getClassOrBoundGenericClass();
  assert(lhsClass != nullptr);

  auto *rhsClass = rhsType.getClassOrBoundGenericClass();
  assert(rhsClass != nullptr);

  // First, establish the invariant that lhsClass is either equal to, or
  // is a superclass of rhsClass.
  if (lhsClass == rhsClass ||
      lhsClass->isSuperclassOf(rhsClass)) {
    // Keep going.
  } else if (rhsClass->isSuperclassOf(lhsClass)) {
    std::swap(rhs, lhs);
    std::swap(rhsType, lhsType);
    std::swap(rhsClass, lhsClass);
  } else {
    // FIXME: Diagnose the conflict.
    if (debug) {
      llvm::dbgs() << "%% Unrelated superclass types\n";
    }

    return std::make_pair(lhs, true);
  }

  if (lhsClass != rhsClass) {
    // Get the corresponding substitutions for the right hand side.
    assert(lhsClass->isSuperclassOf(rhsClass));
    rhsType = rhsType->getSuperclassForDecl(lhsClass)
                     ->getCanonicalType();
  }

  // Unify type contructor arguments.
  ConcreteTypeMatcher matcher(lhs.getSubstitutions(),
                              rhs.getSubstitutions(),
                              system, debug);
  if (!matcher.match(lhsType, rhsType)) {
    if (debug) {
      llvm::dbgs() << "%% Superclass conflict\n";
    }
    return std::make_pair(rhs, true);
  }

  // Record the more specific class.
  return std::make_pair(rhs, false);
}

/// Record a protocol conformance, layout or superclass constraint on the given
/// key. Must be called in monotonically non-decreasing key order.
void PropertyMap::addProperty(
    Term key, Symbol property, unsigned ruleID) {
  assert(property.isProperty());
  assert(*System.getRule(ruleID).isPropertyRule() == property);
  auto *props = getOrCreateProperties(key);
  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  switch (property.getKind()) {
  case Symbol::Kind::Protocol:
    props->ConformsTo.push_back(property.getProtocol());
    props->ConformsToRules.push_back(ruleID);
    return;

  case Symbol::Kind::Layout: {
    auto newLayout = property.getLayoutConstraint();

    if (!props->Layout) {
      // If we haven't seen a layout requirement before, just record it.
      props->Layout = newLayout;
      props->LayoutRule = ruleID;
    } else {
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

    return;
  }

  case Symbol::Kind::Superclass: {
    if (checkRuleOnce(ruleID)) {
      // A rule (T.[superclass: C] => T) induces a rule (T.[layout: L] => T),
      // where L is either AnyObject or _NativeObject.
      auto superclass =
          property.getConcreteType()->getClassOrBoundGenericClass();
      auto layout =
          LayoutConstraint::getLayoutConstraint(
            superclass->getLayoutConstraintKind(),
            Context.getASTContext());
      auto layoutSymbol = Symbol::forLayout(layout, Context);

      recordRelation(key, ruleID, layoutSymbol, System, debug);
    }

    if (!props->Superclass) {
      props->Superclass = property;
      props->SuperclassRule = ruleID;
    } else {
      assert(props->SuperclassRule.hasValue());
      auto pair = unifySuperclasses(*props->Superclass, property,
                                    System, debug);
      props->Superclass = pair.first;
      bool conflict = pair.second;
      if (conflict) {
        recordConflict(key, *props->SuperclassRule, ruleID, System);
        return;
      }
    }

    return;
  }

  case Symbol::Kind::ConcreteType: {
    if (!props->ConcreteType) {
      props->ConcreteType = property;
      props->ConcreteTypeRule = ruleID;
    } else {
      assert(props->ConcreteTypeRule.hasValue());
      bool conflict = unifyConcreteTypes(*props->ConcreteType, property,
                                         System, debug);
      if (conflict) {
        recordConflict(key, *props->ConcreteTypeRule, ruleID, System);
        return;
      }
    }

    return;
  }

  case Symbol::Kind::ConcreteConformance:
    // FIXME
    return;

  case Symbol::Kind::Name:
  case Symbol::Kind::GenericParam:
  case Symbol::Kind::AssociatedType:
    break;
  }

  llvm_unreachable("Bad symbol kind");
}

void PropertyMap::checkConcreteTypeRequirements() {
  bool debug = Debug.contains(DebugFlags::ConcreteUnification);

  for (auto *props : Entries) {
    if (props->ConcreteTypeRule) {
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
      } else if (props->SuperclassRule) {
        recordConflict(props->getKey(),
                       *props->ConcreteTypeRule,
                       *props->SuperclassRule, System);
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
