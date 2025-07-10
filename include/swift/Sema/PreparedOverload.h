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
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallVector.h"

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

/// A "pre-cooked" representation of all type variables and constraints
/// that are generated as part of an overload choice.
struct PreparedOverload {
  /// A change to be introduced into the constraint system when this
  /// overload choice is chosen.
  struct Change {
    enum ChangeKind : unsigned {
      /// A generic parameter was opened to a type variable.
      AddedTypeVariable,

      /// A generic requirement was opened to a constraint.
      AddedConstraint,

      /// A mapping of generic parameter types to type variables
      /// was recorded.
      OpenedTypes,

      /// An existential type was opened.
      OpenedExistentialType,

      /// A pack expansion type was opened.
      OpenedPackExpansionType,

      /// A property wrapper was applied to a parameter.
      AppliedPropertyWrapper,

      /// A fix was recorded because a property wrapper application failed.
      AddedFix
    };

    /// The kind of change.
    ChangeKind Kind;

    union {
      /// For ChangeKind::AddedTypeVariable.
      TypeVariableType *TypeVar;

      /// For ChangeKind::AddedConstraint.
      Constraint *TheConstraint;

      /// For ChangeKind::OpenedTypes.
      struct {
        const OpenedType *Data;
        size_t Count;
      } Replacements;

      /// For ChangeKind::OpenedExistentialType.
      ExistentialArchetypeType *TheExistential;

      /// For ChangeKind::OpenedPackExpansionType.
      struct {
        PackExpansionType *TheExpansion;
        TypeVariableType *TypeVar;
      } PackExpansion;

      /// For ChangeKind::AppliedPropertyWrapper.
      struct {
        TypeBase *WrapperType;
        PropertyWrapperInitKind InitKind;
      } PropertyWrapper;

      /// For ChangeKind::Fix.
      struct {
        ConstraintFix *TheFix;
        unsigned Impact;
      } Fix;
    };
  };

  ArrayRef<Change> Changes;
  DeclReferenceType DeclType;
};

struct PreparedOverloadBuilder {
  SmallVector<PreparedOverload::Change, 8> Changes;

  void addedTypeVariable(TypeVariableType *typeVar) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::AddedTypeVariable;
    change.TypeVar = typeVar;
    Changes.push_back(change);
  }

  void addedConstraint(Constraint *constraint) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::AddedConstraint;
    change.TheConstraint = constraint;
    Changes.push_back(change);
  }

  void openedTypes(ArrayRef<OpenedType> replacements) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::OpenedTypes;
    change.Replacements.Data = replacements.data();
    change.Replacements.Count = replacements.size();
    Changes.push_back(change);
  }

  void openedExistentialType(ExistentialArchetypeType *openedExistential) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::OpenedExistentialType;
    change.TheExistential = openedExistential;
    Changes.push_back(change);
  }

  void openedPackExpansionType(PackExpansionType *packExpansion,
                               TypeVariableType *typeVar) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::OpenedPackExpansionType;
    change.PackExpansion.TheExpansion = packExpansion;
    change.PackExpansion.TypeVar = typeVar;
    Changes.push_back(change);
  }

  void appliedPropertyWrapper(AppliedPropertyWrapper wrapper) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::AppliedPropertyWrapper;
    change.PropertyWrapper.WrapperType = wrapper.wrapperType.getPointer();
    change.PropertyWrapper.InitKind = wrapper.initKind;
    Changes.push_back(change);
  }

  void addedFix(ConstraintFix *fix, unsigned impact) {
    PreparedOverload::Change change;
    change.Kind = PreparedOverload::Change::AddedFix;
    change.Fix.TheFix = fix;
    change.Fix.Impact = impact;
    Changes.push_back(change);
  }
};

}  // end namespace constraints
}  // end namespace swift

#endif
