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
// This file also implements "nested type concretization", which introduces
// concrete type requirements on nested types of type parameters which are
// subject to both a protocol conformance and a concrete type (or superclass)
// requirement.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Decl.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/Module.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/TypeMatcher.h"
#include "swift/AST/Types.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>

#include "PropertyMap.h"

using namespace swift;
using namespace rewriting;

/// This method takes a concrete type that was derived from a concrete type
/// produced by RewriteSystemBuilder::getConcreteSubstitutionSchema(),
/// either by extracting a structural sub-component or performing a (Swift AST)
/// substitution using subst(). It returns a new concrete substitution schema
/// and a new list of substitution terms.
///
/// For example, suppose we start with the concrete type
///
///   Dictionary<τ_0_0, Array<τ_0_1>> with substitutions {X.Y, Z}
///
/// We can extract out the structural sub-component Array<τ_0_1>. If we wish
/// to build a new concrete substitution schema, we call this method with
/// Array<τ_0_1> and the original substitutions {X.Y, Z}. This will produce
/// the new schema Array<τ_0_0> with substitutions {Z}.
///
/// As another example, consider we start with the schema Bar<τ_0_0> with
/// original substitutions {X.Y}, and perform a Swift AST subst() to get
/// Foo<τ_0_0.A.B>. We can then call this method with Foo<τ_0_0.A.B> and
/// the original substitutions {X.Y} to produce the new schema Foo<τ_0_0>
/// with substitutions {X.Y.A.B}.
static CanType
remapConcreteSubstitutionSchema(CanType concreteType,
                                ArrayRef<Term> substitutions,
                                RewriteContext &ctx,
                                SmallVectorImpl<Term> &result) {
  assert(!concreteType->isTypeParameter() && "Must have a concrete type here");

  if (!concreteType->hasTypeParameter())
    return concreteType;

  return CanType(concreteType.transformRec(
    [&](Type t) -> Optional<Type> {
      if (!t->isTypeParameter())
        return None;

      auto term = ctx.getRelativeTermForType(CanType(t), substitutions);

      unsigned newIndex = result.size();
      result.push_back(Term::get(term, ctx));

      return CanGenericTypeParamType::get(/*depth=*/0, newIndex,
                                          ctx.getASTContext());
    }));
}

namespace {
  /// Utility class used by unifyConcreteTypes() and unifySuperclasses()
  /// to walk two concrete types in parallel. Any time there is a mismatch,
  /// records a new induced rule.
  class ConcreteTypeMatcher : public TypeMatcher<ConcreteTypeMatcher> {
    ArrayRef<Term> lhsSubstitutions;
    ArrayRef<Term> rhsSubstitutions;
    RewriteContext &ctx;
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules;
    bool debug;

  public:
    ConcreteTypeMatcher(ArrayRef<Term> lhsSubstitutions,
                        ArrayRef<Term> rhsSubstitutions,
                        RewriteContext &ctx,
                        SmallVectorImpl<std::pair<MutableTerm,
                                                  MutableTerm>> &inducedRules,
                        bool debug)
        : lhsSubstitutions(lhsSubstitutions),
          rhsSubstitutions(rhsSubstitutions),
          ctx(ctx), inducedRules(inducedRules), debug(debug) {}

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
          inducedRules.emplace_back(lhsTerm, rhsTerm);
        }
        return true;
      }

      if (firstAbstract && !secondAbstract) {
        // A type parameter is equated with a concrete type; add a concrete
        // type requirement.
        auto subjectTerm = ctx.getRelativeTermForType(CanType(firstType),
                                                      lhsSubstitutions);

        SmallVector<Term, 3> result;
        auto concreteType = remapConcreteSubstitutionSchema(CanType(secondType),
                                                            rhsSubstitutions,
                                                            ctx, result);

        MutableTerm constraintTerm(subjectTerm);
        constraintTerm.add(Symbol::forConcreteType(concreteType, result, ctx));

        if (debug) {
          llvm::dbgs() << "%% Induced rule " << subjectTerm
                       << " == " << constraintTerm << "\n";
        }
        inducedRules.emplace_back(subjectTerm, constraintTerm);
        return true;
      }

      if (!firstAbstract && secondAbstract) {
        // A concrete type is equated with a type parameter; add a concrete
        // type requirement.
        auto subjectTerm = ctx.getRelativeTermForType(CanType(secondType),
                                                      rhsSubstitutions);

        SmallVector<Term, 3> result;
        auto concreteType = remapConcreteSubstitutionSchema(CanType(firstType),
                                                            lhsSubstitutions,
                                                            ctx, result);

        MutableTerm constraintTerm(subjectTerm);
        constraintTerm.add(Symbol::forConcreteType(concreteType, result, ctx));

        if (debug) {
          llvm::dbgs() << "%% Induced rule " << subjectTerm
                       << " == " << constraintTerm << "\n";
        }
        inducedRules.emplace_back(subjectTerm, constraintTerm);
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
    Symbol lhs, Symbol rhs, RewriteContext &ctx,
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules,
    bool debug) {
  auto lhsType = lhs.getConcreteType();
  auto rhsType = rhs.getConcreteType();

  if (debug) {
    llvm::dbgs() << "% Unifying " << lhs << " with " << rhs << "\n";
  }

  ConcreteTypeMatcher matcher(lhs.getSubstitutions(),
                              rhs.getSubstitutions(),
                              ctx, inducedRules, debug);
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
static Symbol unifySuperclasses(
    Symbol lhs, Symbol rhs, RewriteContext &ctx,
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules,
    bool debug) {
  if (debug) {
    llvm::dbgs() << "% Unifying " << lhs << " with " << rhs << "\n";
  }

  auto lhsType = lhs.getSuperclass();
  auto rhsType = rhs.getSuperclass();

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

    return lhs;
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
                              ctx, inducedRules, debug);
  if (!matcher.match(lhsType, rhsType)) {
    if (debug) {
      llvm::dbgs() << "%% Superclass conflict\n";
    }
    return rhs;
  }

  // Record the more specific class.
  return rhs;
}

void PropertyBag::addProperty(
    Symbol property, RewriteContext &ctx,
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules,
    bool debug) {

  switch (property.getKind()) {
  case Symbol::Kind::Protocol:
    ConformsTo.push_back(property.getProtocol());
    return;

  case Symbol::Kind::Layout:
    if (!Layout)
      Layout = property.getLayoutConstraint();
    else
      Layout = Layout.merge(property.getLayoutConstraint());

    return;

  case Symbol::Kind::Superclass: {
    // FIXME: Also handle superclass vs concrete

    if (Superclass) {
      Superclass = unifySuperclasses(*Superclass, property,
                                     ctx, inducedRules, debug);
    } else {
      Superclass = property;
    }

    return;
  }

  case Symbol::Kind::ConcreteType: {
    if (ConcreteType) {
      (void) unifyConcreteTypes(*ConcreteType, property,
                                ctx, inducedRules, debug);
    } else {
      ConcreteType = property;
    }

    return;
  }

  case Symbol::Kind::Name:
  case Symbol::Kind::GenericParam:
  case Symbol::Kind::AssociatedType:
    break;
  }

  llvm_unreachable("Bad symbol kind");
}

/// For each fully-concrete type, find the shortest term having that concrete type.
/// This is later used by computeConstraintTermForTypeWitness().
void PropertyMap::computeConcreteTypeInDomainMap() {
  for (const auto &props : Entries) {
    if (!props->isConcreteType())
      continue;

    auto concreteType = props->ConcreteType->getConcreteType();
    if (concreteType->hasTypeParameter())
      continue;

    assert(props->ConcreteType->getSubstitutions().empty());

    auto domain = props->Key.getRootProtocols();
    auto concreteTypeKey = std::make_pair(concreteType, domain);

    auto found = ConcreteTypeInDomainMap.find(concreteTypeKey);
    if (found != ConcreteTypeInDomainMap.end())
      continue;

    auto inserted = ConcreteTypeInDomainMap.insert(
        std::make_pair(concreteTypeKey, props->Key));
    assert(inserted.second);
    (void) inserted;
  }
}

void PropertyMap::concretizeNestedTypesFromConcreteParents(
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules) const {
  for (const auto &props : Entries) {
    if (props->getConformsTo().empty())
      continue;

    if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
      if (props->isConcreteType() ||
          props->hasSuperclassBound()) {
        llvm::dbgs() << "^ Concretizing nested types of ";
        props->dump(llvm::dbgs());
        llvm::dbgs() << "\n";
      }
    }

    if (props->isConcreteType()) {
      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "- via concrete type requirement\n";
      }

      concretizeNestedTypesFromConcreteParent(
          props->getKey(),
          RequirementKind::SameType,
          props->ConcreteType->getConcreteType(),
          props->ConcreteType->getSubstitutions(),
          props->getConformsTo(),
          props->ConcreteConformances,
          inducedRules);
    }

    if (props->hasSuperclassBound()) {
      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "- via superclass requirement\n";
      }

      concretizeNestedTypesFromConcreteParent(
          props->getKey(),
          RequirementKind::Superclass,
          props->Superclass->getSuperclass(),
          props->Superclass->getSubstitutions(),
          props->getConformsTo(),
          props->SuperclassConformances,
          inducedRules);
    }
  }
}

/// Suppose a same-type requirement merges two property bags,
/// one of which has a conformance requirement to P and the other
/// one has a concrete type or superclass requirement.
///
/// If the concrete type or superclass conforms to P and P has an
/// associated type A, then we need to infer an equivalence between
/// T.[P:A] and whatever the type witness for 'A' is in the
/// concrete conformance.
///
/// For example, suppose we have a the following definitions,
///
///    protocol Q { associatedtype V }
///    protocol P { associatedtype A; associatedtype C }
///    struct Foo<A, B : Q> : P {
///      typealias C = B.V
///    }
///
/// together with the following property bag:
///
///    T => { conforms_to: [ P ], concrete: Foo<Int, τ_0_0> with <U> }
///
/// The type witness for A in the conformance Foo<Int, τ_0_0> : P is
/// the concrete type 'Int', which induces the following rule:
///
///    T.[P:A].[concrete: Int] => T.[P:A]
///
/// Whereas the type witness for B in the same conformance is the
/// abstract type 'τ_0_0.V', which via the substitutions <U> corresponds
/// to the term 'U.V', and therefore induces the following rule:
///
///    T.[P:B] => U.V
///
void PropertyMap::concretizeNestedTypesFromConcreteParent(
    Term key, RequirementKind requirementKind,
    CanType concreteType, ArrayRef<Term> substitutions,
    ArrayRef<const ProtocolDecl *> conformsTo,
    llvm::TinyPtrVector<ProtocolConformance *> &conformances,
    SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules) const {
  assert(requirementKind == RequirementKind::SameType ||
         requirementKind == RequirementKind::Superclass);

  for (auto *proto : conformsTo) {
    // FIXME: Either remove the ModuleDecl entirely from conformance lookup,
    // or pass the correct one down in here.
    auto *module = proto->getParentModule();

    auto conformance = module->lookupConformance(concreteType,
                                                 const_cast<ProtocolDecl *>(proto));
    if (conformance.isInvalid()) {
      // FIXME: Diagnose conflict
      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "^^ " << concreteType << " does not conform to "
                     << proto->getName() << "\n";
      }

      continue;
    }

    // FIXME: Maybe this can happen if the concrete type is an
    // opaque result type?
    assert(!conformance.isAbstract());

    auto *concrete = conformance.getConcrete();

    // Record the conformance for use by
    // PropertyBag::getConformsToExcludingSuperclassConformances().
    conformances.push_back(concrete);

    auto assocTypes = Protos.getProtocolInfo(proto).AssociatedTypes;
    if (assocTypes.empty())
      continue;

    for (auto *assocType : assocTypes) {
      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "^^ " << "Looking up type witness for "
                     << proto->getName() << ":" << assocType->getName()
                     << " on " << concreteType << "\n";
      }

      auto t = concrete->getTypeWitness(assocType);
      if (!t) {
        if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
          llvm::dbgs() << "^^ " << "Type witness for " << assocType->getName()
                       << " of " << concreteType << " could not be inferred\n";
        }

        t = ErrorType::get(concreteType);
      }

      auto typeWitness = t->getCanonicalType();

      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "^^ " << "Type witness for " << assocType->getName()
                     << " of " << concreteType << " is " << typeWitness << "\n";
      }

      MutableTerm subjectType(key);
      subjectType.add(Symbol::forAssociatedType(proto, assocType->getName(),
                                                Context));

      MutableTerm constraintType;

      auto simplify = [&](CanType t) -> CanType {
        return CanType(t.transformRec([&](Type t) -> Optional<Type> {
          if (!t->isTypeParameter())
            return None;

          auto term = Context.getRelativeTermForType(t->getCanonicalType(),
                                                     substitutions);
          System.simplify(term);
          return Context.getTypeForTerm(term, { }, Protos);
        }));
      };

      if (simplify(concreteType) == simplify(typeWitness) &&
          requirementKind == RequirementKind::SameType) {
        // FIXME: ConcreteTypeInDomainMap should support substitutions so
        // that we can remove this.

        if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
          llvm::dbgs() << "^^ Type witness is the same as the concrete type\n";
        }

        // Add a rule T.[P:A] => T.
        constraintType = MutableTerm(key);
      } else {
        constraintType = computeConstraintTermForTypeWitness(
            key, concreteType, typeWitness, subjectType,
            substitutions);
      }

      inducedRules.emplace_back(subjectType, constraintType);
      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "^^ Induced rule " << constraintType
                     << " => " << subjectType << "\n";
      }
    }
  }
}

/// Given the key of a property bag known to have \p concreteType,
/// together with a \p typeWitness from a conformance on that concrete
/// type, return the right hand side of a rewrite rule to relate
/// \p subjectType with a term representing the type witness.
///
/// Suppose the key is T and the subject type is T.[P:A].
///
/// If the type witness is an abstract type U, this produces a rewrite
/// rule
///
///     T.[P:A] => U
///
/// If the type witness is a concrete type Foo, this produces a rewrite
/// rule
///
///     T.[P:A].[concrete: Foo] => T.[P:A]
///
/// However, this also tries to tie off recursion first using a heuristic.
///
/// If the type witness is fully concrete and we've already seen some
/// term V in the same domain with the same concrete type, we produce a
/// rewrite rule:
///
///        T.[P:A] => V
MutableTerm PropertyMap::computeConstraintTermForTypeWitness(
    Term key, CanType concreteType, CanType typeWitness,
    const MutableTerm &subjectType, ArrayRef<Term> substitutions) const {
  if (!typeWitness->hasTypeParameter()) {
    // Check if we have a shorter representative we can use.
    auto domain = key.getRootProtocols();
    auto concreteTypeKey = std::make_pair(typeWitness, domain);

    auto found = ConcreteTypeInDomainMap.find(concreteTypeKey);
    if (found != ConcreteTypeInDomainMap.end()) {
      MutableTerm result(found->second);
      if (result != subjectType) {
        if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
          llvm::dbgs() << "^^ Type witness can re-use property bag of "
                       << found->second << "\n";
        }
        return result;
      }
    }
  }

  if (typeWitness->isTypeParameter()) {
    // The type witness is a type parameter of the form τ_0_n.X.Y...Z,
    // where 'n' is an index into the substitution array.
    //
    // Add a rule T => S.X.Y...Z, where S is the nth substitution term.
    return Context.getRelativeTermForType(typeWitness, substitutions);
  }

  // The type witness is a concrete type.
  MutableTerm constraintType = subjectType;

  SmallVector<Term, 3> result;
  auto typeWitnessSchema =
      remapConcreteSubstitutionSchema(typeWitness, substitutions,
                                      Context, result);

  // Add a rule T.[P:A].[concrete: Foo.A] => T.[P:A].
  constraintType.add(
      Symbol::forConcreteType(
          typeWitnessSchema, result, Context));

  return constraintType;
}