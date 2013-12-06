//===--- ConstraintGraph.h - Constraint Graph -------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the \c ConstraintGraph class, which describes the
// relationships among the type variables within a constraint system.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINT_GRAPH_H
#define SWIFT_SEMA_CONSTRAINT_GRAPH_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Compiler.h"
#include <utility>

namespace swift {
class TypeVariableType;

namespace constraints {

class Constraint;
class ConstraintSystem;

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
  /// A single node in the constraint graph, which represents a type variable.
  class Node {
  public:
    explicit Node(TypeVariableType *typeVar) : TypeVar(typeVar) { }

    Node(const Node&) = delete;
    Node &operator=(const Node&) = delete;

    /// Retrieve the type variable this node represents.
    TypeVariableType *getTypeVariable() const { return TypeVar; }

    /// Retrieve the set of constraints that mention this type variable.
    ///
    /// These are the hyperedges of the graph, connecting this node to
    /// various other nodes.
    ArrayRef<Constraint *> getConstraints() const { return Constraints; }

    /// Retrieve the set of type variables to which this node is adjacent.
    ArrayRef<TypeVariableType *> getAdjacencies() const {
      return Adjacencies;
    }

    /// Retrieve all of the type variables in the same equivalence class
    /// as this type variable.
    ArrayRef<TypeVariableType *> getEquivalenceClass() const;

    /// Retrieve the set of type variables related to this type variable
    /// through fixed bindings.
    ArrayRef<TypeVariableType *> getFixedBindings() const {
      return FixedBindings;
    }

  private:
    /// Add a constraint to the list of constraints.
    void addConstraint(Constraint *constraint);

    /// Remove a constraint from the list of constraints.
    ///
    /// Note that this only removes the constraint itself; it does not
    /// remove the corresponding adjacencies.
    void removeConstraint(Constraint *constraint);

    /// Add an adjacency to the list of adjacencies.
    void addAdjacency(TypeVariableType *typeVar);

    /// Remove an adjacency from the list of adjacencies.
    void removeAdjacency(TypeVariableType *typeVar);

    /// Add the given type variable to this node's equivalence class.
    void addToEquivalenceClass(TypeVariableType *otherTypeVar);

    /// Add a type variable related to this type variable through fixed
    /// bindings.
    void addFixedBinding(TypeVariableType *otherTypeVar) {
      FixedBindings.push_back(otherTypeVar);
    }

    /// The type variable this node represents.
    TypeVariableType *TypeVar;

    /// The vector of constraints that mention this type variable, in a stable
    /// order for iteration.
    SmallVector<Constraint *, 2> Constraints;

    /// A mapping from the set of constraints that mention this type variable
    /// to the index within the vector of constraints.
    llvm::SmallDenseMap<Constraint *, unsigned, 2> ConstraintIndex;

    /// The set of adjacent type variables, in a stable order.
    SmallVector<TypeVariableType *, 2> Adjacencies;

    /// Describes information about an adjacency between two type variables.
    struct Adjacency {
      /// Index into the vector of adjacent type variables, \c Adjacencies.
      unsigned Index;

      /// The number of constraints that link this type variable to the
      /// enclosing node.
      unsigned NumConstraints;
    };

    /// A mapping from each of the type variables adjacent to this
    /// type variable to the index of the adjacency information in
    /// \c Adjacencies.
    llvm::SmallDenseMap<TypeVariableType *, Adjacency, 2> AdjacencyInfo;

    /// All of the type variables in the same equivalence class as this
    /// representative type variable.
    ///
    /// Note that this field is only valid for type variables that
    /// are representatives of their equivalence classes.
    mutable SmallVector<TypeVariableType *, 2> EquivalenceClass;

    /// The type variables related to this type variable via fixed bindings.
    SmallVector<TypeVariableType *, 2> FixedBindings;

    /// Print this graph node.
    void print(llvm::raw_ostream &out, unsigned indent);

    LLVM_ATTRIBUTE_DEPRECATED(void dump() LLVM_ATTRIBUTE_USED,
                              "only for use within the debugger");

    /// Verify the invariants of this node within the given constraint graph.
    void verify(ConstraintGraph &cg);

    friend class ConstraintGraph;
  };

  /// Constraint a constraint graph for the given constraint system.
  ConstraintGraph(ConstraintSystem &cs);

  /// Destroy the given constraint graph.
  ~ConstraintGraph();

  ConstraintGraph(const ConstraintGraph &) = delete;
  ConstraintGraph &operator=(const ConstraintGraph &) = delete;

  /// Retrieve the constraint system this graph describes.
  ConstraintSystem &getConstraintSystem() const { return CS; }

  /// Access the node corresponding to the given type variable.
  Node &operator[](TypeVariableType *typeVar) {
    return lookupNode(typeVar).first;
  }

  /// Retrieve the node and index corresponding to the given type variable.
  std::pair<Node &, unsigned> lookupNode(TypeVariableType *typeVar);

  /// Add a new constraint to the graph.
  void addConstraint(Constraint *constraint);

  /// Remove a constraint from the graph.
  void removeConstraint(Constraint *constraint);

  /// Retrieve the type variables that correspond to nodes in the graph.
  ///
  /// The subscript operator can be used to retrieve the nodes that
  /// correspond to these type variables.
  ArrayRef<TypeVariableType *> getTypeVariables() const {
    return TypeVariables;
  }

  /// Compute the connected components of the graph.
  ///
  /// \param typeVars The type variables that occur within the
  /// connected components.
  ///
  /// \param components Receives the component numbers for each type variable
  /// in \c typeVars.
  ///
  /// \returns the number of connected components in the graph.
  unsigned computeConnectedComponents(
             SmallVectorImpl<TypeVariableType *> &typeVars,
             SmallVectorImpl<unsigned> &components);

  /// Print the graph.
  void print(llvm::raw_ostream &out);

  LLVM_ATTRIBUTE_DEPRECATED(void dump() LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");

  /// Verify the invariants of the graph.
  void verify();

private:
  /// The constraint system.
  ConstraintSystem &CS;

  /// The type variables in this graph, in stable order.
  SmallVector<TypeVariableType *, 4> TypeVariables;

  /// A stored node within the node mapping, containing both the node pointer
  /// and the
  struct StoredNode {
    /// \brief The node itself, stored as a pointer so we can efficiently
    /// copy/move \c StoredNodes.
    Node *NodePtr;

    /// \brief The index in the \c TypeVariables vector where the corresponding
    /// type variable is stored.
    unsigned Index;
  };

  /// A mapping from the type variables in the graph to their corresponding
  /// nodes along with the index
  llvm::DenseMap<TypeVariableType *, StoredNode> Nodes;
};

} // end namespace swift::constraints

} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_GRAPH_H
