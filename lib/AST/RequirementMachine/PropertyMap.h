//===--- PropertyMap.h - Properties of type parameter terms 0000-*- C++ -*-===//
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
// A description of this data structure and its purpose can be found in
// PropertyMap.cpp.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PROPERTYMAP_H
#define SWIFT_PROPERTYMAP_H

#include "swift/AST/LayoutConstraint.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include <algorithm>
#include <vector>

#include "Debug.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class ProtocolConformance;
class ProtocolDecl;
enum class RequirementKind : unsigned;

namespace rewriting {

class MutableTerm;
class Term;

/// A new rule introduced during property map construction, as a result of
/// unifying two property symbols that apply to the same common suffix term.
struct InducedRule {
  MutableTerm LHS;
  MutableTerm RHS;
  RewritePath Path;

  InducedRule(MutableTerm LHS, MutableTerm RHS, RewritePath Path)
    : LHS(LHS), RHS(RHS), Path(Path) {}

  // FIXME: Eventually all induced rules will have a rewrite path.
  InducedRule(MutableTerm LHS, MutableTerm RHS)
    : LHS(LHS), RHS(RHS) {}
};

/// Stores a convenient representation of all "property-like" rewrite rules of
/// the form T.[p] => T, where [p] is a property symbol, for some term 'T'.
class PropertyBag {
  friend class PropertyMap;

  /// The fully reduced term whose properties are recorded in this property bag.
  Term Key;

  /// All protocols this type conforms to.
  llvm::TinyPtrVector<const ProtocolDecl *> ConformsTo;

  /// The corresponding protocol conformance rules.
  llvm::SmallVector<unsigned, 1> ConformsToRules;

  /// The most specific layout constraint this type satisfies.
  LayoutConstraint Layout;

  /// The corresponding layout rule for the above.
  Optional<unsigned> LayoutRule;

  /// The most specific superclass constraint this type satisfies.
  Optional<Symbol> Superclass;

  /// The corresponding superclass rule for the above.
  Optional<unsigned> SuperclassRule;

  /// All concrete conformances of Superclass to the protocols in the
  /// ConformsTo list.
  llvm::TinyPtrVector<ProtocolConformance *> SuperclassConformances;

  /// The most specific concrete type constraint this type satisfies.
  Optional<Symbol> ConcreteType;

  /// The corresponding layout rule for the above.
  Optional<unsigned> ConcreteTypeRule;

  /// All concrete conformances of ConcreteType to the protocols in the
  /// ConformsTo list.
  llvm::TinyPtrVector<ProtocolConformance *> ConcreteConformances;

  /// Cache of associated type declarations.
  llvm::SmallDenseMap<Identifier, AssociatedTypeDecl *, 2> AssocTypes;

  explicit PropertyBag(Term key) : Key(key) {}

  void copyPropertiesFrom(const PropertyBag *next,
                          RewriteContext &ctx);

  PropertyBag(const PropertyBag &) = delete;
  PropertyBag(PropertyBag &&) = delete;
  PropertyBag &operator=(const PropertyBag &) = delete;
  PropertyBag &operator=(PropertyBag &&) = delete;

public:
  Term getKey() const { return Key; }
  void dump(llvm::raw_ostream &out) const;

  bool hasSuperclassBound() const {
    return Superclass.hasValue();
  }

  CanType getSuperclassBound() const {
    return Superclass->getSuperclass();
  }

  Type getSuperclassBound(
      TypeArrayView<GenericTypeParamType> genericParams,
      const MutableTerm &lookupTerm,
      const PropertyMap &map) const;

  bool isConcreteType() const {
    return ConcreteType.hasValue();
  }

  CanType getConcreteType() const {
    return ConcreteType->getConcreteType();
  }

  Type getConcreteType(
      TypeArrayView<GenericTypeParamType> genericParams,
      const MutableTerm &lookupTerm,
      const PropertyMap &map) const;

  LayoutConstraint getLayoutConstraint() const {
    return Layout;
  }

  ArrayRef<const ProtocolDecl *> getConformsTo() const {
    return ConformsTo;
  }

  llvm::TinyPtrVector<const ProtocolDecl *>
  getConformsToExcludingSuperclassConformances() const;

  MutableTerm getPrefixAfterStrippingKey(const MutableTerm &lookupTerm) const;

  AssociatedTypeDecl *getAssociatedType(Identifier name);

  void verify(const RewriteSystem &system) const;
};

/// Stores all rewrite rules of the form T.[p] => T, where [p] is a property
/// symbol, for all terms 'T'.
///
/// Out-of-line methods are documented in PropertyMap.cpp.
class PropertyMap {
  RewriteContext &Context;
  RewriteSystem &System;
  std::vector<PropertyBag *> Entries;
  Trie<PropertyBag *, MatchKind::Longest> Trie;

  // Building the property map introduces new induced rules, which
  // runs another round of Knuth-Bendix completion, which rebuilds the
  // property map again.
  //
  // To avoid wasted work from re-introducing the same induced rules,
  // we track the rules we've seen already on previous builds.

  /// Maps a pair of rules where the first is a conformance rule and the
  /// second is a superclass or concrete type rule, to a concrete
  /// conformance.
  llvm::DenseMap<std::pair<unsigned, unsigned>, ProtocolConformance *>
      ConcreteConformances;

  /// Superclass requirements always imply a layout requirement, and
  /// concrete type requirements where the type is a class imply a
  /// superclass requirement.
  ///
  /// Keep track of such rules to avoid wasted work from recording the
  /// same rewrite loop more than once.
  llvm::DenseSet<unsigned> CheckedRules;

  /// When a type parameter is subject to two requirements of the same
  /// kind, we have a pair of rewrite rules T.[p1] => T and T.[p2] => T.
  ///
  /// One of these rules might imply the other. Keep track of these pairs
  /// to avoid wasted work from recording the same rewrite loop more than
  /// once.
  llvm::DenseSet<std::pair<unsigned, unsigned>> CheckedRulePairs;

  DebugOptions Debug;

  PropertyBag *getOrCreateProperties(Term key);

  PropertyMap(const PropertyMap &) = delete;
  PropertyMap(PropertyMap &&) = delete;
  PropertyMap &operator=(const PropertyMap &) = delete;
  PropertyMap &operator=(PropertyMap &&) = delete;

public:
  explicit PropertyMap(RewriteSystem &system)
      : Context(system.getRewriteContext()),
        System(system) {
    Debug = Context.getDebugOptions();
  }

  ~PropertyMap();

  PropertyBag *lookUpProperties(std::reverse_iterator<const Symbol *> begin,
                                std::reverse_iterator<const Symbol *> end) const;
  PropertyBag *lookUpProperties(const MutableTerm &key) const;

  std::pair<CompletionResult, unsigned>
  buildPropertyMap(unsigned maxIterations,
                   unsigned maxDepth);

  void dump(llvm::raw_ostream &out) const;

  /// Return the rewrite context used for allocating memory.
  RewriteContext &getRewriteContext() const { return Context; }

  //////////////////////////////////////////////////////////////////////////////
  ///
  /// Term to type conversion. The opposite direction is implemented in
  /// RewriteContext because it does not depend on the current rewrite system.
  ///
  //////////////////////////////////////////////////////////////////////////////

  Type getTypeForTerm(Term term,
                      TypeArrayView<GenericTypeParamType> genericParams) const;

  Type getTypeForTerm(const MutableTerm &term,
                      TypeArrayView<GenericTypeParamType> genericParams) const;

  Type getRelativeTypeForTerm(
                      const MutableTerm &term, const MutableTerm &prefix) const;

  Type getTypeFromSubstitutionSchema(
                      Type schema,
                      ArrayRef<Term> substitutions,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const MutableTerm &prefix) const;

private:
  void clear();

  bool checkRuleOnce(unsigned ruleID);
  bool checkRulePairOnce(unsigned firstRuleID, unsigned secondRuleID);

  void addProperty(Term key, Symbol property, unsigned ruleID,
                   SmallVectorImpl<InducedRule> &inducedRules);

  void checkConcreteTypeRequirements(
                   SmallVectorImpl<InducedRule> &inducedRules);

  void concretizeNestedTypesFromConcreteParents(
                   SmallVectorImpl<InducedRule> &inducedRules);

  void concretizeNestedTypesFromConcreteParent(
                   Term key, RequirementKind requirementKind,
                   unsigned concreteRuleID,
                   CanType concreteType,
                   ArrayRef<Term> substitutions,
                   ArrayRef<unsigned> conformsToRules,
                   ArrayRef<const ProtocolDecl *> conformsTo,
                   llvm::TinyPtrVector<ProtocolConformance *> &conformances,
                   SmallVectorImpl<InducedRule> &inducedRules);

  void concretizeTypeWitnessInConformance(
                   Term key, RequirementKind requirementKind,
                   Symbol concreteConformanceSymbol,
                   ProtocolConformance *concrete,
                   AssociatedTypeDecl *assocType,
                   SmallVectorImpl<InducedRule> &inducedRules) const;

  MutableTerm computeConstraintTermForTypeWitness(
      Term key, RequirementKind requirementKind,
      CanType concreteType, CanType typeWitness,
      const MutableTerm &subjectType,
      ArrayRef<Term> substitutions,
      RewritePath &path) const;

  void recordConcreteConformanceRule(
    unsigned concreteRuleID,
    unsigned conformanceRuleID,
    RequirementKind requirementKind,
    Symbol concreteConformanceSymbol,
    SmallVectorImpl<InducedRule> &inducedRules) const;

  void verify() const;
};

} // end namespace rewriting

} // end namespace swift

#endif