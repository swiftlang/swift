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
#include <memory>
#include <vector>

#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "RewriteSystem.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {

class ProtocolDecl;
enum class RequirementKind : unsigned;

namespace rewriting {

class MutableTerm;
class RewriteContext;
class Term;

/// Stores a convenient representation of all "property-like" rewrite rules of
/// the form T.[p] => T, where [p] is a property symbol, for some term 'T'.
class PropertyBag {
  friend class PropertyMap;

  /// The fully reduced term whose properties are recorded in this property bag.
  MutableTerm Key;

  /// All protocols this type conforms to.
  llvm::TinyPtrVector<const ProtocolDecl *> ConformsTo;

  /// The most specific layout constraint this type satisfies.
  LayoutConstraint Layout;

  /// The most specific superclass constraint this type satisfies.
  Optional<Symbol> Superclass;

  /// All concrete conformances of Superclass to the protocols in the
  /// ConformsTo list.
  llvm::TinyPtrVector<ProtocolConformance *> SuperclassConformances;

  /// The most specific concrete type constraint this type satisfies.
  Optional<Symbol> ConcreteType;

  /// All concrete conformances of ConcreteType to the protocols in the
  /// ConformsTo list.
  llvm::TinyPtrVector<ProtocolConformance *> ConcreteConformances;

  explicit PropertyBag(const MutableTerm &key) : Key(key) {}

  void addProperty(Symbol property,
                   RewriteContext &ctx,
                   SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules,
                   bool debug);
  void copyPropertiesFrom(const PropertyBag *next,
                          RewriteContext &ctx);

  PropertyBag(const PropertyBag &) = delete;
  PropertyBag(PropertyBag &&) = delete;
  PropertyBag &operator=(const PropertyBag &) = delete;
  PropertyBag &operator=(PropertyBag &&) = delete;

public:
  const MutableTerm &getKey() const { return Key; }
  void dump(llvm::raw_ostream &out) const;

  bool hasSuperclassBound() const {
    return Superclass.hasValue();
  }

  Type getSuperclassBound() const {
    return Superclass->getSuperclass();
  }

  Type getSuperclassBound(
      TypeArrayView<GenericTypeParamType> genericParams,
      const ProtocolGraph &protos,
      RewriteContext &ctx) const;

  bool isConcreteType() const {
    return ConcreteType.hasValue();
  }

  Type getConcreteType() const {
    return ConcreteType->getConcreteType();
  }

  Type getConcreteType(
      TypeArrayView<GenericTypeParamType> genericParams,
      const ProtocolGraph &protos,
      RewriteContext &ctx) const;

  LayoutConstraint getLayoutConstraint() const {
    return Layout;
  }

  ArrayRef<const ProtocolDecl *> getConformsTo() const {
    return ConformsTo;
  }

  llvm::TinyPtrVector<const ProtocolDecl *>
  getConformsToExcludingSuperclassConformances() const;
};

/// Stores all rewrite rules of the form T.[p] => T, where [p] is a property
/// symbol, for all terms 'T'.
///
/// Out-of-line methods are documented in PropertyMap.cpp.
class PropertyMap {
  RewriteContext &Context;
  std::vector<std::unique_ptr<PropertyBag>> Map;

  using ConcreteTypeInDomain = std::pair<CanType, ArrayRef<const ProtocolDecl *>>;
  llvm::DenseMap<ConcreteTypeInDomain, MutableTerm> ConcreteTypeInDomainMap;

  const ProtocolGraph &Protos;
  unsigned DebugConcreteUnification : 1;
  unsigned DebugConcretizeNestedTypes : 1;

  PropertyBag *getPropertiesIfPresent(const MutableTerm &key) const;
  PropertyBag *getOrCreateProperties(const MutableTerm &key);

  PropertyMap(const PropertyMap &) = delete;
  PropertyMap(PropertyMap &&) = delete;
  PropertyMap &operator=(const PropertyMap &) = delete;
  PropertyMap &operator=(PropertyMap &&) = delete;

public:
  explicit PropertyMap(RewriteContext &ctx,
                       const ProtocolGraph &protos)
      : Context(ctx), Protos(protos) {
    DebugConcreteUnification = false;
    DebugConcretizeNestedTypes = false;
  }

  PropertyBag *lookUpProperties(const MutableTerm &key) const;

  void dump(llvm::raw_ostream &out) const;

  void clear();
  void addProperty(const MutableTerm &key, Symbol property,
                   SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules);

  void computeConcreteTypeInDomainMap();
  void concretizeNestedTypesFromConcreteParents(
                   SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules) const;

private:
  void concretizeNestedTypesFromConcreteParent(
                   const MutableTerm &key, RequirementKind requirementKind,
                   CanType concreteType, ArrayRef<Term> substitutions,
                   ArrayRef<const ProtocolDecl *> conformsTo,
                   llvm::TinyPtrVector<ProtocolConformance *> &conformances,
                   SmallVectorImpl<std::pair<MutableTerm, MutableTerm>> &inducedRules) const;

  MutableTerm computeConstraintTermForTypeWitness(
      const MutableTerm &key, CanType concreteType, CanType typeWitness,
      const MutableTerm &subjectType, ArrayRef<Term> substitutions) const;
};

} // end namespace rewriting

} // end namespace swift

#endif