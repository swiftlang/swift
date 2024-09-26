//===--- CSTrail.h - Constraint Solver Trail --------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the \c SolverTrail class, which records the decisions taken
// while attempting to find a solution.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CSTRAIL_H
#define SWIFT_SEMA_CSTRAIL_H

#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {

class TypeBase;
class TypeVariableType;

namespace constraints {

class Constraint;
class ConstraintGraphScope;

class SolverTrail {
public:

  /// The kind of change made to the graph.
  enum class ChangeKind {
    /// Added a type variable to the constraint graph.
    AddedTypeVariable,
    /// Added a new constraint to the constraint graph.
    AddedConstraint,
    /// Removed an existing constraint from the constraint graph.
    RemovedConstraint,
    /// Extended the equivalence class of a type variable in the constraint graph.
    ExtendedEquivalenceClass,
    /// Added a fixed binding for a type variable in the constraint graph.
    BoundTypeVariable,
  };

  /// A change made to the constraint system.
  ///
  /// Each change can be undone (once, and in reverse order) by calling the
  /// undo() method.
  class Change {
  public:
    /// The kind of change.
    ChangeKind Kind;

    union {
      TypeVariableType *TypeVar;
      Constraint *TheConstraint;

      struct {
        /// The type variable whose equivalence class was extended.
        TypeVariableType *TypeVar;

        /// The previous size of the equivalence class.
        unsigned PrevSize;
      } EquivClass;

      struct {
        /// The type variable being bound to a fixed type.
        TypeVariableType *TypeVar;

        /// The fixed type to which the type variable was bound.
        TypeBase *FixedType;
      } Binding;
    };

    Change() : Kind(ChangeKind::AddedTypeVariable), TypeVar(nullptr) { }

    /// Create a change that added a type variable.
    static Change addedTypeVariable(TypeVariableType *typeVar);

    /// Create a change that added a constraint.
    static Change addedConstraint(Constraint *constraint);

    /// Create a change that removed a constraint.
    static Change removedConstraint(Constraint *constraint);

    /// Create a change that extended an equivalence class.
    static Change extendedEquivalenceClass(TypeVariableType *typeVar,
                                           unsigned prevSize);

    /// Create a change that bound a type variable to a fixed type.
    static Change boundTypeVariable(TypeVariableType *typeVar, Type fixed);

    /// Undo this change, reverting the constraint graph to the state it
    /// had prior to this change.
    ///
    /// Changes must be undone in stack order.
    void undo(ConstraintSystem &cs);
  };

  SolverTrail(ConstraintSystem &cs) : CS(cs) {}

  ~SolverTrail();

  SolverTrail(const SolverTrail &) = delete;
  SolverTrail &operator=(const SolverTrail &) = delete;

  void recordChange(Change change);

  void dumpActiveScopeChanges(llvm::raw_ostream &out,
                              unsigned indent = 0);

  unsigned size() const {
    return Changes.size();
  }

  void undo(unsigned toIndex);

private:
  ConstraintSystem &CS;

  /// The list of changes made to this constraint system.
  std::vector<Change> Changes;

  friend class ConstraintGraphScope;
};

} // namespace constraints
} // namespace swift

#endif // SWIFT_SEMA_CSTRAIL_H