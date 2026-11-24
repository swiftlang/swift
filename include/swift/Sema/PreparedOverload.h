//===--- PreparedOverload.h - A Choice from an Overload Set  ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_PREPAREDOVERLOAD_H
#define SWIFT_SEMA_PREPAREDOVERLOAD_H

#include "swift/AST/PropertyWrappers.h"
#include "swift/Basic/TaggedUnion.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/TrailingObjects.h"

namespace llvm {
class raw_ostream;
}

namespace swift {

class ExistentialArchetypeType;
class GenericTypeParamType;
class TypeVariableType;

namespace constraints {

class ConstraintLocatorBuilder;
class ConstraintSystem;

/// Describes the type produced when referencing a declaration.
struct DeclReferenceType {
  /// The "opened" type, which is the type of the declaration where any
  /// generic parameters have been replaced with type variables.
  ///
  /// The mapping from generic parameters to type variables will have been
  /// recorded by \c recordOpenedTypes when this type is produced.
  Type openedType;

  /// The opened type, after performing contextual type adjustments such as
  /// removing concurrency-related annotations for a `@preconcurrency`
  /// operation.
  Type adjustedOpenedType;

  /// The type of the reference, based on the original opened type. This is the
  /// type that the expression used to form the declaration reference would
  /// have if no adjustments had been applied.
  Type referenceType;

  /// The type of the reference, which is the adjusted opened type after
  /// (e.g.) applying the base of a member access. This is the type of the
  /// expression used to form the declaration reference.
  Type adjustedReferenceType;

  /// The type that could be thrown by accessing this declaration.
  Type thrownErrorTypeOnAccess;
};

/// Describes a dependent type that has been opened to a particular type
/// variable.
using OpenedType = std::pair<GenericTypeParamType *, TypeVariableType *>;

/// A change to be introduced into the constraint system when this
/// overload choice is chosen.
class PreparedOverloadChange {
public:
  enum class Kind : unsigned {
#define CHANGE(ID, BODY, VISIT) ID,
#include "PreparedOverloadChanges.def"
  };

#define CHANGE(ID, BODY, VISIT) \
  struct ID BODY;
#include "PreparedOverloadChanges.def"

private:
  using Members = ExternalUnionMembers<
#define CHANGE(ID, BODY, VISIT) ID,
#define LAST_CHANGE(ID, BODY, VISIT) ID
#include "PreparedOverloadChanges.def"
  >;

  static constexpr Members::Index getIndexForKind(Kind kind) {
    switch (kind) {
#define CHANGE(ID, BODY, VISIT) \
    case Kind::ID: return Members::indexOf<ID>();
#include "PreparedOverloadChanges.def"
    }
    llvm_unreachable("bad kind");
  }

  static constexpr Kind getDefaultKindForIndex(Members::Index index) {
    switch (index) {
#define CHANGE(ID, BODY, VISIT) \
    case Members::indexOf<ID>(): return Kind::ID;
#include "PreparedOverloadChanges.def"
    }
    llvm_unreachable("bad kind");
  }

  using StorageType = ExplicitTaggedUnion<Kind, Members, getIndexForKind,
                                          getDefaultKindForIndex>;
  StorageType storage;

public:
  template <class T,
            typename = StorageType::enable_if_constructible_t<T> >
  PreparedOverloadChange(T &&data) : storage(std::forward<T>(data)) {}

  Kind getKind() const {
    return storage.getKind();
  }

  template <Kind K>
  const StorageType::typeForKind<K> &get() const {
    assert(getKind() == K);
    return storage.get<StorageType::typeForKind<K>>();
  }

  template <Kind K>
  const StorageType::typeForKind<K> *dyn_cast() const {
    return (getKind() == K
              ? &storage.get<StorageType::typeForKind<K>>()
              : nullptr);
  }

  template <class V>
  auto visit(V &&visitor) const
      -> decltype(std::forward<V>(visitor).visitAddedTypeVariable(nullptr)) {
    switch (getKind()) {
#define CHANGE(ID, BODY, VISIT_ARGS)                          \
    case Kind::ID: {                                          \
      auto &self = get<Kind::ID>();                           \
      return std::forward<V>(visitor).visit##ID VISIT_ARGS;   \
    }
#include "PreparedOverloadChanges.def"
    }
    llvm_unreachable("bad kind");
  }

  void dump(llvm::raw_ostream &out,
            ConstraintSystem &cs,
            unsigned indent = 0) const;
};

/// A "pre-cooked" representation of all type variables and constraints
/// that are generated as part of an overload choice.
class PreparedOverload final :
  public llvm::TrailingObjects<PreparedOverload, PreparedOverloadChange> {

public:
  using Change = PreparedOverloadChange;

private:
  Type OpenedType;
  Type ThrownErrorType;
  unsigned Count : 31;

  /// A prepared overload for diagnostics is different than one without,
  /// because of fixes and such.
  unsigned ForDiagnostics : 1;

  size_t numTrailingObjects(OverloadToken<Change>) const {
    return Count;
  }

public:
  PreparedOverload(Type openedType, Type thrownErrorType,
                   ArrayRef<Change> changes, bool forDiagnostics)
    : OpenedType(openedType), ThrownErrorType(thrownErrorType),
      Count(changes.size()), ForDiagnostics(forDiagnostics) {
    std::uninitialized_copy(changes.begin(), changes.end(),
                            getTrailingObjects());
  }

  Type getOpenedType() const {
    return OpenedType;
  }

  Type getThrownErrorType() const {
    return ThrownErrorType;
  }

  bool wasForDiagnostics() const {
    return ForDiagnostics;
  }

  ArrayRef<Change> getChanges() const { return getTrailingObjects(Count); }
};

struct PreparedOverloadBuilder {
  using Change = PreparedOverload::Change;
  SmallVector<Change, 8> Changes;
  ConstraintLocator *Locator;

  PreparedOverloadBuilder(ConstraintLocator *locator)
    : Locator(locator) {}

  void addedTypeVariable(TypeVariableType *typeVar) {
    Changes.emplace_back(Change::AddedTypeVariable{typeVar});
  }

  void addedConstraint(Constraint *constraint) {
    Changes.emplace_back(Change::AddedConstraint{constraint});
  }

  void addedBindConstraint(Type firstType, Type secondType) {
    Changes.emplace_back(
      Change::AddedBindConstraint{firstType, secondType});
  }

  void openedTypes(ArrayRef<OpenedType> replacements) {
    Changes.emplace_back(Change::OpenedTypes{replacements});
  }

  void openedExistentialType(ExistentialArchetypeType *openedExistential) {
    Changes.emplace_back(Change::OpenedExistentialType{openedExistential});
  }

  void openedPackExpansionType(PackExpansionType *packExpansion,
                               TypeVariableType *typeVar) {
    Changes.emplace_back(
      Change::OpenedPackExpansionType{packExpansion, typeVar});
  }

  void appliedPropertyWrapper(AppliedPropertyWrapper wrapper) {
    Changes.emplace_back(
      Change::AppliedPropertyWrapper{wrapper.wrapperType, wrapper.initKind});
  }

  void addedFix(ConstraintFix *fix, FixImpact impact) {
    Changes.emplace_back(Change::AddedFix{fix, impact});
  }

  void recordedNodeType(ASTNode node, Type type) {
    Changes.emplace_back(Change::RecordedNodeType{node, type});
  }

  void dump(llvm::raw_ostream &out,
            ConstraintSystem &cs,
            unsigned indent = 0) const;
};

}  // end namespace constraints
}  // end namespace swift

#endif
