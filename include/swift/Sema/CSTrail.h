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
#include "swift/Sema/Constraint.h"
#include "llvm/ADT/ilist.h"
#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {

class TypeBase;
class TypeVariableType;

namespace constraints {

class Constraint;
struct SyntacticElementTargetKey;

namespace inference {
struct PotentialBinding;
}

class SolverTrail {
public:

  /// The kind of change made to the graph.
  enum class ChangeKind: unsigned {
#define CHANGE(Name) Name,
#define LAST_CHANGE(Name) Last = Name
#include "CSTrail.def"
#undef CHANGE
#undef LAST_CHANGE
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
        TypeVariableType *TypeVar;
        TypeVariableType *OtherTypeVar;
        Constraint *Constraint;
      } BindingRelation;

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

      struct {
        /// It's former position in the inactive constraints list.
        llvm::ilist<Constraint>::iterator Where;

        /// The constraint.
        Constraint *Constraint;
      } Retiree;

      struct {
        TypeVariableType *TypeVar;

        /// These two fields together with 'Options' above store the contents
        /// of a PotentialBinding.
        Type BindingType;
        PointerUnion<Constraint *, ConstraintLocator *> BindingSource;
      } Binding;

      ConstraintFix *TheFix;
      ConstraintLocator *TheLocator;
      PackExpansionType *TheExpansion;
      PackExpansionExpr *TheExpansionExpr;
      PackElementExpr *TheElement;
      Expr *TheExpr;
      Stmt *TheStmt;
      StmtConditionElement *TheCondElt;
      Pattern *ThePattern;
      PatternBindingDecl *ThePatternBinding;
      VarDecl *TheVar;
      AnyFunctionRef TheRef;
      ClosureExpr *TheClosure;
      DeclContext *TheDeclContext;
      CaseLabelItem *TheItem;
      CatchNode TheCatchNode;
      ParamDecl *TheParam;
    };

    Change() : Kind(ChangeKind::AddedTypeVariable), TypeVar(nullptr) { }

#define LOCATOR_CHANGE(Name, _) static Change Name(ConstraintLocator *locator);
#define EXPR_CHANGE(Name) static Change Name(Expr *expr);
#define CLOSURE_CHANGE(Name) static Change Name(ClosureExpr *closure);
#define CONSTRAINT_CHANGE(Name) static Change Name(Constraint *constraint);
#define SCORE_CHANGE(Name) static Change Name(ScoreKind kind, unsigned value);
#define GRAPH_NODE_CHANGE(Name) static Change Name(TypeVariableType *typeVar, \
                                                   Constraint *constraint);
#define BINDING_RELATION_CHANGE(Name) static Change Name(TypeVariableType *typeVar, \
                                                         TypeVariableType *otherTypeVar, \
                                                         Constraint *constraint);
#include "swift/Sema/CSTrail.def"

    /// Create a change that added a type variable.
    static Change AddedTypeVariable(TypeVariableType *typeVar);

    /// Create a change that extended an equivalence class.
    static Change ExtendedEquivalenceClass(TypeVariableType *typeVar,
                                           unsigned prevSize);

    /// Create a change that updated the references/referenced by sets of
    /// a type variable pair.
    static Change RelatedTypeVariables(TypeVariableType *typeVar,
                                       TypeVariableType *otherTypeVar);

    /// Create a change that updated a type variable.
    static Change UpdatedTypeVariable(
               TypeVariableType *typeVar,
               llvm::PointerUnion<TypeVariableType *, TypeBase *> parentOrFixed,
               unsigned options);

    /// Create a change that recorded a restriction.
    static Change AddedConversionRestriction(Type srcType, Type dstType);

    /// Create a change that recorded a fix.
    static Change AddedFix(ConstraintFix *fix);

    /// Create a change that recorded a fixed requirement.
    static Change AddedFixedRequirement(GenericTypeParamType *GP,
                                        unsigned reqKind,
                                        Type requirementTy);

    /// Create a change that recorded the opening of a pack expansion type.
    static Change RecordedOpenedPackExpansionType(PackExpansionType *expansion);

    /// Create a change that recorded a mapping from a pack element expression
    /// to its parent expansion expression.
    static Change RecordedPackElementExpansion(PackElementExpr *packElement);

    /// Create a change that records the GenericEnvironment for a given
    /// PackExpansionExpr.
    static Change RecordedPackExpansionEnvironment(PackExpansionExpr *expr);

    /// Create a change that recorded an assignment of a type to an AST node.
    static Change RecordedNodeType(ASTNode node, Type oldType);

    /// Create a change that recorded an assignment of a type to an AST node.
    static Change RecordedKeyPathComponentType(const KeyPathExpr *expr,
                                               unsigned component,
                                               Type oldType);

    /// Create a change that recorded a result builder transform.
    static Change RecordedResultBuilderTransform(AnyFunctionRef fn);

    /// Create a change that recorded the contextual type of an AST node.
    static Change RecordedContextualInfo(ASTNode node);

    /// Create a change that recorded a SyntacticElementTarget.
    static Change RecordedTarget(SyntacticElementTargetKey key);

    /// Create a change that recorded a SyntacticElementTarget.
    static Change RecordedCaseLabelItemInfo(CaseLabelItem *item);

    /// Create a change that recorded a potential throw site.
    static Change RecordedPotentialThrowSite(CatchNode catchNode);

    /// Create a change that recorded an isolated parameter.
    static Change RecordedIsolatedParam(ParamDecl *param);

    /// Create a change that recorded a key path expression.
    static Change RecordedKeyPath(KeyPathExpr *expr);

    /// Create a change that removed a constraint from the inactive constraint list.
    static Change RetiredConstraint(llvm::ilist<Constraint>::iterator where,
                                    Constraint *constraint);

    /// Create a change that removed a binding from a type variable's potential
    /// bindings.
    static Change RetractedBinding(TypeVariableType *typeVar,
                                   inference::PotentialBinding binding);

    /// Undo this change, reverting the constraint graph to the state it
    /// had prior to this change.
    ///
    /// Changes must be undone in stack order.
    void undo(ConstraintSystem &cs) const;

    void dump(llvm::raw_ostream &out, ConstraintSystem &cs,
              unsigned indent = 0) const;

  private:
    SyntacticElementTargetKey getSyntacticElementTargetKey() const;
  };

  SolverTrail(ConstraintSystem &cs);

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

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &OS, unsigned fromIndex = 0,
            unsigned indent = 0) const LLVM_ATTRIBUTE_USED;

private:
  ConstraintSystem &CS;

  /// The list of changes made to this constraint system.
  std::vector<Change> Changes;

  uint64_t Profile[unsigned(ChangeKind::Last) + 1];

  bool UndoActive = false;
  unsigned Total = 0;
  unsigned Max = 0;
};

} // namespace constraints
} // namespace swift

#endif // SWIFT_SEMA_CSTRAIL_H
