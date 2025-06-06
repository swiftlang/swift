//===--- ConstraintGraph.h - Constraint Graph -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the \c ConstraintGraph class, which describes the
// relationships among the type variables within a constraint system.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINT_GRAPH_H
#define SWIFT_SEMA_CONSTRAINT_GRAPH_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/Sema/CSBindings.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/TinyPtrVector.h"
#include "llvm/Support/Compiler.h"
#include <functional>
#include <utility>

namespace swift {

class Type;
class TypeBase;
class TypeVariableType;

namespace constraints {

class Constraint;
class ConstraintGraph;
class ConstraintSystem;
class TypeVariableBinding;

/// A single node in the constraint graph, which represents a type variable.
class ConstraintGraphNode {
public:
  explicit ConstraintGraphNode(ConstraintGraph &CG, TypeVariableType *typeVar)
      : CG(CG), TypeVar(typeVar) {}

  ConstraintGraphNode(const ConstraintGraphNode&) = delete;
  ConstraintGraphNode &operator=(const ConstraintGraphNode&) = delete;

  /// Retrieve the type variable this node represents.
  TypeVariableType *getTypeVariable() const { return TypeVar; }

  void reset();

  void initTypeVariable(TypeVariableType *typeVar) {
    ASSERT(!TypeVar);
    TypeVar = typeVar;
  }

  /// Retrieve the set of constraints that mention this type variable.
  ///
  /// These are the hyperedges of the graph, connecting this node to
  /// various other nodes.
  ArrayRef<Constraint *> getConstraints() const { return Constraints; }

  /// Retrieve the set of type variables that are adjacent due to fixed
  /// bindings.
  ArrayRef<TypeVariableType *> getReferencedVars() const {
    return References.getArrayRef();
  }

  ArrayRef<TypeVariableType *> getReferencedBy() const {
    return ReferencedBy.getArrayRef();
  }

  /// Retrieve all of the type variables in the same equivalence class
  /// as this type variable.
  ArrayRef<TypeVariableType *> getEquivalenceClass() const;

  inference::PotentialBindings &getPotentialBindings() {
    DEBUG_ASSERT(forRepresentativeVar());
    return Potential;
  }

  void initBindingSet();

  inference::BindingSet &getBindingSet() {
    ASSERT(hasBindingSet());
    return *Set;
  }

  bool hasBindingSet() const {
    return Set.has_value();
  }

  void resetBindingSet() {
    Set.reset();
  }

private:
  /// Determines whether the type variable associated with this node
  /// is a representative of an equivalence class.
  ///
  /// Note: The smallest equivalence class is of just one variable - itself.
  bool forRepresentativeVar() const;

  /// Retrieve all of the type variables in the same equivalence class
  /// as this type variable.
  ArrayRef<TypeVariableType *> getEquivalenceClassUnsafe() const;

  /// Add a constraint to the list of constraints.
  void addConstraint(Constraint *constraint);

  /// Remove a constraint from the list of constraints.
  ///
  /// Note that this only removes the constraint itself; it does not
  /// remove the corresponding adjacencies.
  void removeConstraint(Constraint *constraint);

  /// Add the given type variables to this node's equivalence class.
  void addToEquivalenceClass(ArrayRef<TypeVariableType *> typeVars);

  /// Remove N last members from equivalence class of the current type variable.
  void truncateEquivalenceClass(unsigned prevSize);

  /// Add a type variable related to this type variable through fixed
  /// binding.
  void addReferencedVar(TypeVariableType *typeVar);

  /// Add a type variable referencing this type variable - this type
  /// variable occurs in fixed type of the given type variable.
  void addReferencedBy(TypeVariableType *typeVar);

  /// Remove a type variable referenced by this node through a fixed binding.
  void removeReference(TypeVariableType *typeVar);

  /// Remove a type variable which used to reference this type variable.
  void removeReferencedBy(TypeVariableType *typeVar);

  /// Binding Inference {

  /// Perform graph updates that must be undone after we bind a fixed type
  /// to a type variable.
  void retractFromInference();

  /// Perform graph updates that must be undone before we bind a fixed type
  /// to a type variable.
  ///
  /// The reason why this can't simplify be a part of \c bindTypeVariable
  /// is related to the fact that it's sometimes expensive to re-compute
  /// bindings (i.e. if `DependentMemberType` is involved, because it requires
  /// a conformance lookup), so inference has to be delayed until its clear that
  /// type variable has been bound to a valid type and solver can make progress.
  void introduceToInference(Type fixedType);

  /// Notify all of the type variables that have this one (or any member of
  /// its equivalence class) referenced in their fixed type.
  ///
  /// This is a traversal up the reference change which triggers constraint
  /// re-introduction to affected type variables.
  ///
  /// This is useful in situations when type variable gets bound and unbound,
  /// or equivalence class changes.
  void notifyReferencingVars(
      llvm::function_ref<void(ConstraintGraphNode &,
                              Constraint *)> notification) const;

  /// Notify all of the type variables referenced by this one about a change.
  void notifyReferencedVars(
      llvm::function_ref<void(ConstraintGraphNode &)> notification) const;
  /// }

  /// The constraint graph this node belongs to.
  ConstraintGraph &CG;

  /// The type variable this node represents.
  TypeVariableType *TypeVar;

  /// The potential bindings for this type variable, updated incrementally by
  /// the constraint graph.
  inference::PotentialBindings Potential;

  /// The binding set for this type variable, computed by
  /// determineBestBindings().
  std::optional<inference::BindingSet> Set;

  /// The vector of constraints that mention this type variable, in a stable
  /// order for iteration.
  SmallVector<Constraint *, 2> Constraints;

  /// A mapping from the set of constraints that mention this type variable
  /// to the index within the vector of constraints.
  llvm::SmallDenseMap<Constraint *, unsigned, 2> ConstraintIndex;

  /// The set of type variables that reference type variable associated
  /// with this constraint graph node.
  llvm::SmallSetVector<TypeVariableType *, 2> ReferencedBy;

  /// The set of type variables that occur within the fixed binding of
  /// this type variable.
  llvm::SmallSetVector<TypeVariableType *, 2> References;

  /// All of the type variables in the same equivalence class as this
  /// representative type variable.
  ///
  /// Note that this field is only valid for type variables that
  /// are representatives of their equivalence classes.
  mutable SmallVector<TypeVariableType *, 2> EquivalenceClass;

  /// Print this graph node.
  void print(llvm::raw_ostream &out, unsigned indent,
             const PrintOptions &PO = PrintOptions()) const;

  SWIFT_DEBUG_DUMP;

  /// Verify the invariants of this node within the given constraint graph.
  void verify(ConstraintGraph &cg);

  friend class ConstraintGraph;
  friend class ConstraintSystem;
  friend class TypeVariableBinding;
  friend class SolverTrail;
};

/// A graph that describes the relationships among the various type variables
/// and constraints within a constraint system.
///
/// The constraint graph is a hypergraph where the nodes are type variables and
/// the edges are constraints. Any given constraint connects a type variable to
/// zero or more other type variables. Because these adjacencies are as
/// important as the edges themselves and are expensive to calculate from the
/// constraints, each node in the graph tracks both its edges (constraints) and
/// its adjacencies (the type variables) separately.
class ConstraintGraph {
public:
  /// Constraint a constraint graph for the given constraint system.
  ConstraintGraph(ConstraintSystem &cs);

  /// Destroy the given constraint graph.
  ~ConstraintGraph();

  ConstraintGraph(const ConstraintGraph &) = delete;
  ConstraintGraph &operator=(const ConstraintGraph &) = delete;


  /// Retrieve the constraint system this graph describes.
  ConstraintSystem &getConstraintSystem() const { return CS; }

  /// Add a new vertex to the graph.
  void addTypeVariable(TypeVariableType *typeVar);

  /// Look up the vertex associated with the given type variable.
  ConstraintGraphNode &operator[](TypeVariableType *typeVar);

  /// Add a new constraint to the graph.
  void addConstraint(Constraint *constraint);

  /// Primitive form for SolverTrail::Change::undo().
  void addConstraint(TypeVariableType *typeVar, Constraint *constraint);

  /// Remove a constraint from the graph.
  void removeConstraint(Constraint *constraint);

  /// Primitive form for SolverTrail::Change::undo().
  void removeConstraint(TypeVariableType *typeVar, Constraint *constraint);

  /// Prepare to merge the given node into some other node.
  ///
  /// This records graph changes that must be undone after the merge has
  /// been undone.
  void mergeNodesPre(TypeVariableType *typeVar2);

  /// Merge the two nodes for the two given type variables.
  ///
  /// The type variables must actually have been merged already; this
  /// operation merges the two nodes. This also records graph changes
  /// that must be undone before the merge can be undone.
  void mergeNodes(TypeVariableType *typeVar1, TypeVariableType *typeVar2);

  /// Bind the given type variable to the given fixed type.
  void bindTypeVariable(TypeVariableType *typeVar, Type fixedType);

  /// Perform graph updates that must be undone after we bind a fixed type
  /// to a type variable.
  void retractFromInference(TypeVariableType *typeVar);

  /// Perform graph updates that must be undone before we bind a fixed type
  /// to a type variable.
  void introduceToInference(TypeVariableType *typeVar, Type fixedType);

  /// Gather constraints associated with all of the variables within the
  /// same equivalence class as the given type variable, as well as its
  /// immediate fixed bindings.
  llvm::TinyPtrVector<Constraint *>
  gatherAllConstraints(TypeVariableType *typeVar);

  /// Gather all constraints that mention this type variable or type variables
  /// that it is a fixed binding of. Unlike EquivalenceClass, this looks
  /// through transitive fixed bindings. This can be used to find all the
  /// constraints that may be affected when binding a type variable.
  llvm::TinyPtrVector<Constraint *>
  gatherNearbyConstraints(TypeVariableType *typeVar,
                    llvm::function_ref<bool(Constraint *)> acceptConstraint =
                        [](Constraint *constraint) { return true; });

  /// Describes a single component, as produced by the connected components
  /// algorithm.
  struct Component {
    /// The type variables in this component.
    TinyPtrVector<TypeVariableType *> typeVars;

    /// The original index of this component in the list of components,
    /// used to provide the index of where the partial solutions will occur.
    /// FIXME: This is needed due to some ordering dependencies in the
    /// merging of partial solutions, which appears to also be related
    /// DisjunctionStep::pruneOverloads() short-circuiting. It should be
    /// removed.
    unsigned solutionIndex;

  private:
    /// The number of disjunctions in this component.
    unsigned numDisjunctions = 0;

    /// The constraints in this component.
    TinyPtrVector<Constraint *> constraints;

  public:
    Component(unsigned solutionIndex) : solutionIndex(solutionIndex) { }

    /// Whether this component represents an orphaned constraint.
    bool isOrphaned() const {
      return typeVars.empty();
    }

    /// Add a constraint.
    void addConstraint(Constraint *constraint);

    const TinyPtrVector<Constraint *> &getConstraints() const {
      return constraints;
    }

    unsigned getNumDisjunctions() const { return numDisjunctions; }
  };

  /// Compute the connected components of the graph.
  ///
  /// \param typeVars The type variables that should be included in the
  /// set of connected components that are returned.
  ///
  /// \returns the connected components of the graph, where each component
  /// contains the type variables and constraints specific to that component.
  SmallVector<Component, 1> computeConnectedComponents(
             ArrayRef<TypeVariableType *> typeVars);

  /// Retrieve the set of "orphaned" constraints, which are known to the
  /// constraint graph but have no type variables to anchor them.
  ArrayRef<Constraint *> getOrphanedConstraints() const {
    return OrphanedConstraints;
  }

  /// Replace the orphaned constraints with the constraints in the given list,
  /// returning the old set of orphaned constraints.
  SmallVector<Constraint *, 4> takeOrphanedConstraints() {
    auto result = std::move(OrphanedConstraints);
    OrphanedConstraints.clear();
    return result;
  }

  /// Set the orphaned constraints.
  void setOrphanedConstraints(SmallVector<Constraint *, 4> &&newConstraints) {
    OrphanedConstraints = std::move(newConstraints);
  }

  /// Set the list of orphaned constraints to a single constraint.
  ///
  /// If \c orphaned is null, just clear out the list.
  void setOrphanedConstraint(Constraint *orphaned) {
    OrphanedConstraints.clear();
    if (orphaned)
      OrphanedConstraints.push_back(orphaned);
  }

  /// Print the graph.
  void print(ArrayRef<TypeVariableType *> typeVars, llvm::raw_ostream &out);
  void dump(llvm::raw_ostream &out);

  // FIXME: Potentially side-effectful.
  SWIFT_DEBUG_HELPER(void dump());

  /// Print the connected components of the graph.
  void printConnectedComponents(ArrayRef<TypeVariableType *> typeVars,
                                llvm::raw_ostream &out);

  // FIXME: Potentially side-effectful.
  SWIFT_DEBUG_HELPER(void dumpConnectedComponents());

  /// Verify the invariants of the graph.
  void verify();

  /// Optimize the constraint graph by eliminating simple transitive
  /// connections between nodes.
  void optimize();

private:
  /// Remove the node corresponding to the given type variable.
  ///
  /// This operation assumes that the any constraints that refer to this type
  /// variable have been or will be removed before other graph queries are
  /// performed.
  ///
  /// Note that this it only meant to be called by SolverTrail::Change::undo().
  void removeNode(TypeVariableType *typeVar);

  /// Remove an edge from the constraint graph.
  ///
  ///
  /// Note that this it only meant to be called by SolverTrail::Change::undo().
  void unrelateTypeVariables(TypeVariableType *typeVar,
                             TypeVariableType *otherTypeVar);

  /// Retract bindings from the given constraint.
  ///
  /// Note that this it only meant to be called by SolverTrail::Change::undo().
  void retractBindings(TypeVariableType *typeVar, Constraint *constraint);

  /// Perform edge contraction on the constraint graph, merging equivalence
  /// classes until a fixed point is reached.
  bool contractEdges();

  /// The constraint system.
  ConstraintSystem &CS;

  /// Constraints that are "orphaned" because they contain no type variables.
  SmallVector<Constraint *, 4> OrphanedConstraints;

  /// Unused nodes.
  SmallVector<ConstraintGraphNode *> FreeList;

  /// Increment the number of constraints considered per attempt
  /// to contract constraint graph edges.
  void incrementConstraintsPerContractionCounter();

  friend class SolverTrail;
};

} // end namespace constraints

} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_GRAPH_H
