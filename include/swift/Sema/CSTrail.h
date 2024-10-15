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

#include "swift/AST/AnyFunctionRef.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {

class TypeBase;
class TypeVariableType;

namespace constraints {

class Constraint;

class SolverTrail {
public:

  /// The kind of change made to the graph.
  enum class ChangeKind: unsigned {
    /// Added a new vertex to the constraint graph.
    AddedTypeVariable,
    /// Added a new constraint to the constraint graph.
    AddedConstraint,
    /// Removed an existing constraint from the constraint graph.
    RemovedConstraint,
    /// Extended the equivalence class of a type variable in the constraint graph.
    ExtendedEquivalenceClass,
    /// Added a new edge in the constraint graph.
    RelatedTypeVariables,
    /// Inferred potential bindings from a constraint.
    InferredBindings,
    /// Retracted potential bindings from a constraint.
    RetractedBindings,
    /// Set the fixed type or parent and flags for a type variable.
    UpdatedTypeVariable,
    /// Recorded a conversion restriction kind.
    AddedConversionRestriction,
    /// Recorded a fix.
    AddedFix,
    /// Recorded a fixed requirement.
    AddedFixedRequirement,
    /// Recorded a disjunction choice.
    RecordedDisjunctionChoice,
    /// Recorded an applied disjunction.
    RecordedAppliedDisjunction,
    /// Recorded an argument matching choice.
    RecordedMatchCallArgumentResult,
    /// Recorded a list of opened types at a locator.
    RecordedOpenedTypes,
    /// Recorded the opening of an existential type at a locator.
    RecordedOpenedExistentialType,
    /// Recorded the opening of a pack existential type.
    RecordedOpenedPackExpansionType,
    /// Recorded the creation of a generic environment for a pack expansion expression.
    RecordedPackExpansionEnvironment,
    /// Recorded the mapping from a pack element expression to its parent
    /// pack expansion expression.
    RecordedPackEnvironment,
    /// Recorded a defaulted constraint at a locator.
    RecordedDefaultedConstraint,
    /// Recorded an assignment of a type to an AST node.
    RecordedNodeType,
    /// Recorded an assignment of a type to a keypath component.
    RecordedKeyPathComponentType,
    /// Disabled a constraint.
    DisabledConstraint,
    /// Favored a constraint.
    FavoredConstraint,
    /// Recorded a result builder transform.
    RecordedResultBuilderTransform,
  };

  /// A change made to the constraint system.
  ///
  /// Each change can be undone (once, and in reverse order) by calling the
  /// undo() method.
  class Change {
  public:
    /// The kind of change.
    ChangeKind Kind;

    /// Extra storage.
    unsigned Options;

    union {
      TypeVariableType *TypeVar;

      struct {
        /// The type variable we're adding or removing a constraint from.
        TypeVariableType *TypeVar;

        /// The constraint.
        Constraint *Constraint;
      } TheConstraint;

      struct {
        /// The type variable whose equivalence class was extended.
        TypeVariableType *TypeVar;

        /// The previous size of the equivalence class.
        unsigned PrevSize;
      } EquivClass;

      struct {
        /// The first type variable.
        TypeVariableType *TypeVar;

        /// The second type variable.
        TypeVariableType *OtherTypeVar;
      } Relation;

      struct {
        /// The type variable being updated.
        TypeVariableType *TypeVar;

        /// The representative of the equivalence class, or the fixed type.
        llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;
      } Update;

      struct {
        /// The source type.
        Type SrcType;

        /// The destination type.
        Type DstType;
      } Restriction;

      ConstraintFix *Fix;

      struct {
        GenericTypeParamType *GP;
        Type ReqTy;
      } FixedRequirement;

      struct {
        ASTNode Node;
        Type OldType;
      } Node;

      struct {
        const KeyPathExpr *Expr;
        Type OldType;
      } KeyPath;

      ConstraintLocator *Locator;
      PackExpansionType *ExpansionTy;
      PackElementExpr *ElementExpr;
      AnyFunctionRef AFR;
    };

    Change() : Kind(ChangeKind::AddedTypeVariable), TypeVar(nullptr) { }

    /// Create a change that added a type variable.
    static Change addedTypeVariable(TypeVariableType *typeVar);

    /// Create a change that added a constraint.
    static Change addedConstraint(TypeVariableType *typeVar, Constraint *constraint);

    /// Create a change that removed a constraint.
    static Change removedConstraint(TypeVariableType *typeVar, Constraint *constraint);

    /// Create a change that extended an equivalence class.
    static Change extendedEquivalenceClass(TypeVariableType *typeVar,
                                           unsigned prevSize);

    /// Create a change that updated the references/referenced by sets of
    /// a type variable pair.
    static Change relatedTypeVariables(TypeVariableType *typeVar,
                                       TypeVariableType *otherTypeVar);

    /// Create a change that inferred bindings from a constraint.
    static Change inferredBindings(TypeVariableType *typeVar,
                                   Constraint *constraint);

    /// Create a change that retracted bindings from a constraint.
    static Change retractedBindings(TypeVariableType *typeVar,
                                    Constraint *constraint);

    /// Create a change that updated a type variable.
    static Change updatedTypeVariable(
               TypeVariableType *typeVar,
               llvm::PointerUnion<TypeVariableType *, TypeBase *> parentOrFixed,
               unsigned options);

    /// Create a change that recorded a restriction.
    static Change addedConversionRestriction(Type srcType, Type dstType);

    /// Create a change that recorded a fix.
    static Change addedFix(ConstraintFix *fix);

    /// Create a change that recorded a fixed requirement.
    static Change addedFixedRequirement(GenericTypeParamType *GP,
                                        unsigned reqKind,
                                        Type requirementTy);

    /// Create a change that recorded a disjunction choice.
    static Change recordedDisjunctionChoice(ConstraintLocator *locator,
                                            unsigned index);

    /// Create a change that recorded an applied disjunction.
    static Change recordedAppliedDisjunction(ConstraintLocator *locator);

    /// Create a change that recorded an applied disjunction.
    static Change recordedMatchCallArgumentResult(ConstraintLocator *locator);

    /// Create a change that recorded a list of opened types.
    static Change recordedOpenedTypes(ConstraintLocator *locator);

    /// Create a change that recorded the opening of an existential type.
    static Change recordedOpenedExistentialType(ConstraintLocator *locator);

    /// Create a change that recorded the opening of a pack expansion type.
    static Change recordedOpenedPackExpansionType(PackExpansionType *expansion);

    /// Create a change that recorded the opening of a pack expansion type.
    static Change recordedPackExpansionEnvironment(ConstraintLocator *locator);

    /// Create a change that recorded a mapping from a pack element expression
    /// to its parent expansion expression.
    static Change recordedPackEnvironment(PackElementExpr *packElement);

    /// Create a change that recorded a defaulted constraint at a locator.
    static Change recordedDefaultedConstraint(ConstraintLocator *locator);

    /// Create a change that recorded an assignment of a type to an AST node.
    static Change recordedNodeType(ASTNode node, Type oldType);

    /// Create a change that recorded an assignment of a type to an AST node.
    static Change recordedKeyPathComponentType(const KeyPathExpr *expr,
                                               unsigned component,
                                               Type oldType);

    /// Create a change that disabled a constraint.
    static Change disabledConstraint(Constraint *constraint);

    /// Create a change that favored a constraint.
    static Change favoredConstraint(Constraint *constraint);

    /// Create a change that recorded a result builder transform.
    static Change recordedResultBuilderTransform(AnyFunctionRef fn);

    /// Undo this change, reverting the constraint graph to the state it
    /// had prior to this change.
    ///
    /// Changes must be undone in stack order.
    void undo(ConstraintSystem &cs) const;

    void dump(llvm::raw_ostream &out, ConstraintSystem &cs,
              unsigned indent = 0) const;
  };

  SolverTrail(ConstraintSystem &cs) : CS(cs) {}

  ~SolverTrail();

  SolverTrail(const SolverTrail &) = delete;
  SolverTrail &operator=(const SolverTrail &) = delete;

  bool isUndoActive() const { return UndoActive; }

  void recordChange(Change change);

  void dumpActiveScopeChanges(llvm::raw_ostream &out,
                              unsigned fromIndex,
                              unsigned indent = 0) const;

  unsigned size() const {
    return Changes.size();
  }

  void undo(unsigned toIndex);

private:
  ConstraintSystem &CS;

  /// The list of changes made to this constraint system.
  std::vector<Change> Changes;

  bool UndoActive = false;
  unsigned Total = 0;
  unsigned Max = 0;
};

} // namespace constraints
} // namespace swift

#endif // SWIFT_SEMA_CSTRAIL_H