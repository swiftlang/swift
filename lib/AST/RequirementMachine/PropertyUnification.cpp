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

  return CanType(concreteType.transformRec([&](Type t) -> Optional<Type> {
    if (!t->isTypeParameter())
      return None;

    auto term = ctx.getRelativeTermForType(CanType(t), substitutions);

    unsigned newIndex = result.size();
    result.push_back(Term::get(term, ctx));

    return CanGenericTypeParamType::get(/*type sequence=*/ false,
                                        /*depth=*/ 0, newIndex,
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
    SmallVectorImpl<InducedRule> &inducedRules;
    bool debug;

  public:
    ConcreteTypeMatcher(ArrayRef<Term> lhsSubstitutions,
                        ArrayRef<Term> rhsSubstitutions,
                        RewriteContext &ctx,
                        SmallVectorImpl<InducedRule> &inducedRules,
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
    SmallVectorImpl<InducedRule> &inducedRules,
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
static std::pair<Symbol, bool> unifySuperclasses(
    Symbol lhs, Symbol rhs, RewriteContext &ctx,
    SmallVectorImpl<InducedRule> &inducedRules,
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
                              ctx, inducedRules, debug);
  if (!matcher.match(lhsType, rhsType)) {
    if (debug) {
      llvm::dbgs() << "%% Superclass conflict\n";
    }
    return std::make_pair(rhs, true);
  }

  // Record the more specific class.
  return std::make_pair(rhs, false);
}

/// Returns the old conflicting rule ID if there was a conflict,
/// otherwise returns None.
Optional<unsigned> PropertyBag::addProperty(
    Symbol property, unsigned ruleID, RewriteContext &ctx,
    SmallVectorImpl<InducedRule> &inducedRules,
    bool debug) {

  switch (property.getKind()) {
  case Symbol::Kind::Protocol:
    ConformsTo.push_back(property.getProtocol());
    ConformsToRules.push_back(ruleID);
    return None;

  case Symbol::Kind::Layout:
    if (!Layout) {
      Layout = property.getLayoutConstraint();
      LayoutRule = ruleID;
    } else {
      assert(LayoutRule.hasValue());
      Layout = Layout.merge(property.getLayoutConstraint());
      if (!Layout->isKnownLayout())
        return LayoutRule;
    }

    return None;

  case Symbol::Kind::Superclass: {
    // FIXME: Also handle superclass vs concrete

    if (!Superclass) {
      Superclass = property;
      SuperclassRule = ruleID;
    } else {
      assert(SuperclassRule.hasValue());
      auto pair = unifySuperclasses(*Superclass, property,
                                    ctx, inducedRules, debug);
      Superclass = pair.first;
      bool conflict = pair.second;
      if (conflict)
        return SuperclassRule;
    }

    return None;
  }

  case Symbol::Kind::ConcreteType: {
    if (!ConcreteType) {
      ConcreteType = property;
      ConcreteTypeRule = ruleID;
    } else {
      assert(ConcreteTypeRule.hasValue());
      bool conflict = unifyConcreteTypes(*ConcreteType, property,
                                         ctx, inducedRules, debug);
      if (conflict)
        return ConcreteTypeRule;
    }

    return None;
  }

  case Symbol::Kind::ConcreteConformance:
    // FIXME
    return None;

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
    SmallVectorImpl<InducedRule> &inducedRules) {
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
          *props->ConcreteTypeRule,
          props->ConcreteType->getConcreteType(),
          props->ConcreteType->getSubstitutions(),
          props->ConformsToRules,
          props->ConformsTo,
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
          *props->SuperclassRule,
          props->Superclass->getSuperclass(),
          props->Superclass->getSubstitutions(),
          props->ConformsToRules,
          props->ConformsTo,
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
    unsigned concreteRuleID,
    CanType concreteType,
    ArrayRef<Term> substitutions,
    ArrayRef<unsigned> conformsToRules,
    ArrayRef<const ProtocolDecl *> conformsTo,
    llvm::TinyPtrVector<ProtocolConformance *> &conformances,
    SmallVectorImpl<InducedRule> &inducedRules) {
  assert(requirementKind == RequirementKind::SameType ||
         requirementKind == RequirementKind::Superclass);
  assert(conformsTo.size() == conformsToRules.size());

  for (unsigned i : indices(conformsTo)) {
    auto *proto = conformsTo[i];
    unsigned conformanceRuleID = conformsToRules[i];

    // If we've already processed this pair of rules, record the conformance
    // and move on.
    //
    // This occurs when a pair of rules are inherited from the property map
    // entry for this key's suffix.
    auto pair = std::make_pair(concreteRuleID, conformanceRuleID);
    auto found = ConcreteConformances.find(pair);
    if (found != ConcreteConformances.end()) {
      conformances.push_back(found->second);
      continue;
    }

    // FIXME: Either remove the ModuleDecl entirely from conformance lookup,
    // or pass the correct one down in here.
    auto *module = proto->getParentModule();

    auto conformance = module->lookupConformance(concreteType,
                                                 const_cast<ProtocolDecl *>(proto));
    if (conformance.isInvalid()) {
      // For superclass rules, it is totally fine to have a signature like:
      //
      // protocol P {}
      // class C {}
      // <T  where T : P, T : C>
      //
      // There is no relation between P and C here.
      //
      // With concrete types, a missing conformance is a conflict.
      if (requirementKind == RequirementKind::SameType) {
        // FIXME: Diagnose conflict
        auto &concreteRule = System.getRule(concreteRuleID);
        if (concreteRule.getRHS().size() == key.size())
          concreteRule.markConflicting();

        auto &conformanceRule = System.getRule(conformanceRuleID);
        if (conformanceRule.getRHS().size() == key.size())
          conformanceRule.markConflicting();
      }

      if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
        llvm::dbgs() << "^^ " << concreteType << " does not conform to "
                     << proto->getName() << "\n";
      }

      continue;
    }

    // FIXME: Maybe this can happen if the concrete type is an
    // opaque result type?
    assert(!conformance.isAbstract());

    // Save this conformance for later.
    auto *concrete = conformance.getConcrete();
    auto inserted = ConcreteConformances.insert(
        std::make_pair(pair, concrete));
    assert(inserted.second);
    (void) inserted;

    // Record the conformance for use by
    // PropertyBag::getConformsToExcludingSuperclassConformances().
    conformances.push_back(concrete);

    auto concreteConformanceSymbol = Symbol::forConcreteConformance(
        concreteType, substitutions, proto, Context);

    recordConcreteConformanceRule(concreteRuleID, conformanceRuleID,
                                  requirementKind, concreteConformanceSymbol,
                                  inducedRules);

    auto assocTypes = proto->getAssociatedTypeMembers();
    if (assocTypes.empty())
      continue;

    for (auto *assocType : assocTypes) {
      concretizeTypeWitnessInConformance(key, requirementKind,
                                         concreteConformanceSymbol,
                                         concrete, assocType,
                                         inducedRules);
    }
  }
}

void PropertyMap::concretizeTypeWitnessInConformance(
    Term key, RequirementKind requirementKind,
    Symbol concreteConformanceSymbol,
    ProtocolConformance *concrete,
    AssociatedTypeDecl *assocType,
    SmallVectorImpl<InducedRule> &inducedRules) const {
  auto concreteType = concreteConformanceSymbol.getConcreteType();
  auto substitutions = concreteConformanceSymbol.getSubstitutions();
  auto *proto = concreteConformanceSymbol.getProtocol();

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

  // Build the term T.[concrete: C : P].[P:X].
  MutableTerm subjectType(key);
  subjectType.add(concreteConformanceSymbol);
  subjectType.add(Symbol::forAssociatedType(proto, assocType->getName(),
                                            Context));

  MutableTerm constraintType;

  RewritePath path;

  constraintType = computeConstraintTermForTypeWitness(
      key, requirementKind, concreteType, typeWitness, subjectType,
      substitutions, path);

  inducedRules.emplace_back(constraintType, subjectType, path);
  if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
    llvm::dbgs() << "^^ Induced rule " << constraintType
                 << " => " << subjectType << "\n";
  }
}

RewriteSystem::TypeWitness::TypeWitness(
    Term lhs, llvm::PointerUnion<Symbol, Term> rhs)
  : LHS(lhs), RHS(rhs) {
  assert(LHS.size() >= 2);
  assert(getConcreteConformance().getKind() ==
         Symbol::Kind::ConcreteConformance);
  assert(getAssocType().getKind() == Symbol::Kind::AssociatedType);
  if (RHS.is<Symbol>())
    assert(RHS.get<Symbol>().getKind() == Symbol::Kind::ConcreteType);
  assert(getAssocType().getProtocols().size() == 1);
  assert(getAssocType().getProtocols()[0] ==
         getConcreteConformance().getProtocol());
}

namespace swift {
namespace rewriting {
bool operator==(const RewriteSystem::TypeWitness &lhs,
                const RewriteSystem::TypeWitness &rhs) {
  return lhs.LHS == rhs.LHS && lhs.RHS == rhs.RHS;
}
}
}

void RewriteSystem::TypeWitness::dump(llvm::raw_ostream &out) const {
  out << "Subject type: " << LHS << "\n";
  if (RHS.is<Symbol>())
    out << "Concrete type witness: " << RHS.get<Symbol>() << "\n";
  else
    out << "Abstract type witness: " << RHS.get<Term>() << "\n";
}

unsigned RewriteSystem::recordTypeWitness(
    RewriteSystem::TypeWitness witness) {
  unsigned index = TypeWitnesses.size();
  auto inserted = TypeWitnessMap.insert(std::make_pair(witness.LHS, index));

  if (!inserted.second) {
    index = inserted.first->second;
  } else {
    TypeWitnesses.push_back(witness);
  }

  assert(TypeWitnesses[index] == witness);
  return index;
}

const RewriteSystem::TypeWitness &
RewriteSystem::getTypeWitness(unsigned index) const {
  return TypeWitnesses[index];
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
    Term key, RequirementKind requirementKind,
    CanType concreteType, CanType typeWitness,
    const MutableTerm &subjectType,
    ArrayRef<Term> substitutions,
    RewritePath &path) const {
  // If the type witness is abstract, introduce a same-type requirement
  // between two type parameters.
  if (typeWitness->isTypeParameter()) {
    // The type witness is a type parameter of the form τ_0_n.X.Y...Z,
    // where 'n' is an index into the substitution array.
    //
    // Add a rule:
    //
    // T.[concrete: C : P].[P:X] => S[n].X.Y...Z
    //
    // Where S[n] is the nth substitution term.

    auto result = Context.getRelativeTermForType(typeWitness, substitutions);

    RewriteSystem::TypeWitness witness(Term::get(subjectType, Context),
                                       Term::get(result, Context));
    unsigned witnessID = System.recordTypeWitness(witness);
    path.add(RewriteStep::forAbstractTypeWitness(
        witnessID, /*inverse=*/false));

    return result;
  }

  // Otherwise the type witness is concrete, but may contain type
  // parameters in structural position.

  // Compute the concrete type symbol [concrete: C.X].
  SmallVector<Term, 3> result;
  auto typeWitnessSchema =
      remapConcreteSubstitutionSchema(typeWitness, substitutions,
                                      Context, result);
  auto typeWitnessSymbol =
      Symbol::forConcreteType(typeWitnessSchema, result, Context);

  RewriteSystem::TypeWitness witness(Term::get(subjectType, Context),
                                     typeWitnessSymbol);
  unsigned witnessID = System.recordTypeWitness(witness);

  // Simplify the substitution terms in the type witness symbol.
  RewritePath substPath;
  System.simplifySubstitutions(typeWitnessSymbol, &substPath);
  substPath.invert();

  // If it is equal to the parent type, introduce a same-type requirement
  // between the two parameters.
  if (requirementKind == RequirementKind::SameType &&
      typeWitnessSymbol.getConcreteType() == concreteType &&
      typeWitnessSymbol.getSubstitutions() == substitutions) {
    // FIXME: ConcreteTypeInDomainMap should support substitutions so
    // that we can remove this.

    if (Debug.contains(DebugFlags::ConcretizeNestedTypes)) {
      llvm::dbgs() << "^^ Type witness is the same as the concrete type\n";
    }

    // Add a rule T.[concrete: C : P] => T.[concrete: C : P].[P:X].
    MutableTerm result(key);
    result.add(witness.getConcreteConformance());

    // ([concrete: C : P] => [concrete: C : P].[P:X].[concrete: C])
    path.add(RewriteStep::forSameTypeWitness(
        witnessID, /*inverse=*/true));

    // [concrete: C : P].[P:X].([concrete: C] => [concrete: C.X])
    path.append(substPath);

    // T.([concrete: C : P].[P:X].[concrete: C.X] => [concrete: C : P].[P:X])
    path.add(RewriteStep::forConcreteTypeWitness(
        witnessID, /*inverse=*/false));

    return result;
  }

  // If the type witness is completely concrete, try to introduce a
  // same-type requirement with another representative type parameter,
  // if we have one.
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

        // FIXME: Record a rewrite path.
        return result;
      }
    }
  }

  // Otherwise, add a concrete type requirement for the type witness.
  //
  // Add a rule:
  //
  // T.[concrete: C : P].[P:X].[concrete: C.X'] => T.[concrete: C : P].[P:X].
  //
  // Where C.X' is the canonical form of C.X.
  MutableTerm constraintType = subjectType;
  constraintType.add(typeWitnessSymbol);

  // T.[concrete: C : P].[P:X].([concrete: C.X'] => [concrete: C.X])
  path.append(substPath);

  // T.([concrete: C : P].[P:X].[concrete: C.X] => [concrete: C : P].[P:X])
  path.add(RewriteStep::forConcreteTypeWitness(
      witnessID, /*inverse=*/false));

  return constraintType;
}

void PropertyMap::recordConcreteConformanceRule(
    unsigned concreteRuleID,
    unsigned conformanceRuleID,
    RequirementKind requirementKind,
    Symbol concreteConformanceSymbol,
    SmallVectorImpl<InducedRule> &inducedRules) const {
  const auto &concreteRule = System.getRule(concreteRuleID);
  const auto &conformanceRule = System.getRule(conformanceRuleID);

#ifndef NDEBUG
  {
    auto conformanceSymbol = *conformanceRule.isPropertyRule();
    assert(conformanceSymbol.getKind() == Symbol::Kind::Protocol);
    assert(conformanceSymbol.getProtocol() ==
           concreteConformanceSymbol.getProtocol());

    auto concreteSymbol = *concreteRule.isPropertyRule();
    if (concreteSymbol.getKind() == Symbol::Kind::Superclass)
      assert(requirementKind == RequirementKind::Superclass);
    else {
      assert(concreteSymbol.getKind() == Symbol::Kind::ConcreteType);
      assert(requirementKind == RequirementKind::SameType);
    }
  }
#endif

  RewritePath path;

  // We have a pair of rules T.[P] and T'.[concrete: C].
  // Either T == T', or T is a prefix of T', or T' is a prefix of T.
  //
  // Let T'' be the longest of T and T'.
  MutableTerm rhs(concreteRule.getRHS().size() > conformanceRule.getRHS().size()
                  ? concreteRule.getRHS()
                  : conformanceRule.getRHS());

  // First, apply the conformance rule in reverse to obtain T''.[P].
  path.add(RewriteStep::forRewriteRule(
      /*startOffset=*/rhs.size() - conformanceRule.getRHS().size(),
      /*endOffset=*/0,
      /*ruleID=*/conformanceRuleID,
      /*inverse=*/true));

  // Now, apply the concrete type rule in reverse to obtain T''.[concrete: C].[P].
  path.add(RewriteStep::forRewriteRule(
      /*startOffset=*/rhs.size() - concreteRule.getRHS().size(),
      /*endOffset=*/1,
      /*ruleID=*/concreteRuleID,
      /*inverse=*/true));

  // Apply a concrete type adjustment to the concrete symbol if T' is shorter
  // than T.
  unsigned adjustment = rhs.size() - concreteRule.getRHS().size();
  if (adjustment > 0 &&
      !concreteConformanceSymbol.getSubstitutions().empty()) {
    path.add(RewriteStep::forAdjustment(adjustment, /*endOffset=*/1,
                                        /*inverse=*/false));
  }

  // Now, transform T''.[concrete: C].[P] into T''.[concrete: C : P].
  if (requirementKind == RequirementKind::Superclass) {
    path.add(RewriteStep::forSuperclassConformance(/*inverse=*/false));
  } else {
    assert(requirementKind == RequirementKind::SameType);
    path.add(RewriteStep::forConcreteConformance(/*inverse=*/false));
  }

  MutableTerm lhs(rhs);
  lhs.add(concreteConformanceSymbol);

  // The path turns T'' (RHS) into T''.[concrete: C : P] (LHS), but we need
  // it to go in the other direction.
  path.invert();

  inducedRules.emplace_back(std::move(lhs), std::move(rhs), std::move(path));
}
