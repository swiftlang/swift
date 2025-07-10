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

  SmallVector<Change, 8> Changes;

  void addedTypeVariable(TypeVariableType *typeVar) {
    Change change;
    change.Kind = Change::AddedTypeVariable;
    change.TypeVar = typeVar;
    Changes.push_back(change);
  }

  void addedConstraint(Constraint *constraint) {
    Change change;
    change.Kind = Change::AddedConstraint;
    change.TheConstraint = constraint;
    Changes.push_back(change);
  }

  void openedTypes(ArrayRef<OpenedType> replacements) {
    Change change;
    change.Kind = Change::OpenedTypes;
    change.Replacements.Data = replacements.data();
    change.Replacements.Count = replacements.size();
    Changes.push_back(change);
  }

  void openedExistentialType(ExistentialArchetypeType *openedExistential) {
    Change change;
    change.Kind = Change::OpenedExistentialType;
    change.TheExistential = openedExistential;
    Changes.push_back(change);
  }

  void openedPackExpansionType(PackExpansionType *packExpansion,
                               TypeVariableType *typeVar) {
    Change change;
    change.Kind = Change::OpenedPackExpansionType;
    change.PackExpansion.TheExpansion = packExpansion;
    change.PackExpansion.TypeVar = typeVar;
    Changes.push_back(change);
  }

  void appliedPropertyWrapper(AppliedPropertyWrapper wrapper) {
    Change change;
    change.Kind = Change::AppliedPropertyWrapper;
    change.PropertyWrapper.WrapperType = wrapper.wrapperType.getPointer();
    change.PropertyWrapper.InitKind = wrapper.initKind;
    Changes.push_back(change);
  }

  void addedFix(ConstraintFix *fix, unsigned impact) {
    Change change;
    change.Kind = Change::AddedFix;
    change.Fix.TheFix = fix;
    change.Fix.Impact = impact;
    Changes.push_back(change);
  }

  void discharge(ConstraintSystem &cs, ConstraintLocatorBuilder locator) const;
};

}
}

#endif
