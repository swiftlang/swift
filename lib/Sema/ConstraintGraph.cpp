//===--- ConstraintGraph.cpp - Constraint Graph ---------------------------===//
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
// This file implements the \c ConstraintGraph class, which describes the
// relationships among the type variables within a constraint system.
//
//===----------------------------------------------------------------------===//

#include "ConstraintGraph.h"
#include "ConstraintGraphScope.h"
#include "ConstraintSystem.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <memory>
#include <numeric>

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "ConstraintGraph"

#pragma mark Graph construction/destruction

ConstraintGraph::ConstraintGraph(ConstraintSystem &cs) : CS(cs) { }

ConstraintGraph::~ConstraintGraph() {
  assert(Changes.empty() && "Scope stack corrupted");
  for (unsigned i = 0, n = TypeVariables.size(); i != n; ++i) {
    auto &impl = TypeVariables[i]->getImpl();
    delete impl.getGraphNode();
    impl.setGraphNode(nullptr);
  }
}

#pragma mark Graph accessors

std::pair<ConstraintGraphNode &, unsigned>
ConstraintGraph::lookupNode(TypeVariableType *typeVar) {
  // Check whether we've already created a node for this type variable.
  auto &impl = typeVar->getImpl();
  if (auto nodePtr = impl.getGraphNode()) {
    assert(impl.getGraphIndex() < TypeVariables.size() && "Out-of-bounds index");
    assert(TypeVariables[impl.getGraphIndex()] == typeVar && 
           "Type variable mismatch");
    return { *nodePtr, impl.getGraphIndex() };
  }

  // Allocate the new node.
  auto nodePtr = new ConstraintGraphNode(typeVar);
  unsigned index = TypeVariables.size();
  impl.setGraphNode(nodePtr);
  impl.setGraphIndex(index);

  // Record this type variable.
  TypeVariables.push_back(typeVar);

  // Record the change, if there are active scopes.
  if (ActiveScope)
    Changes.push_back(Change::addedTypeVariable(typeVar));

  // If this type variable is not the representative of its equivalence class,
  // add it to its representative's set of equivalences.
  auto typeVarRep = CS.getRepresentative(typeVar);
  if (typeVar != typeVarRep)
    mergeNodes(typeVar, typeVarRep);
  else if (auto fixed = CS.getFixedType(typeVarRep)) {
    // Bind the type variable.
    bindTypeVariable(typeVar, fixed);
  }

  return { *nodePtr, index };
}

ArrayRef<TypeVariableType *> ConstraintGraphNode::getEquivalenceClass() const{
  assert(TypeVar == TypeVar->getImpl().getRepresentative(nullptr) &&
         "Can't request equivalence class from non-representative type var");
  return getEquivalenceClassUnsafe();
}

ArrayRef<TypeVariableType *>
ConstraintGraphNode::getEquivalenceClassUnsafe() const{
  if (EquivalenceClass.empty())
    EquivalenceClass.push_back(TypeVar);
  return EquivalenceClass;
}

#pragma mark Node mutation
void ConstraintGraphNode::addConstraint(Constraint *constraint) {
  assert(ConstraintIndex.count(constraint) == 0 && "Constraint re-insertion");
  ConstraintIndex[constraint] = Constraints.size();
  Constraints.push_back(constraint);
}

void ConstraintGraphNode::removeConstraint(Constraint *constraint) {
  auto pos = ConstraintIndex.find(constraint);
  assert(pos != ConstraintIndex.end());

  // Remove this constraint from the constraint mapping.
  auto index = pos->second;
  ConstraintIndex.erase(pos);
  assert(Constraints[index] == constraint && "Mismatched constraint");

  // If this is the last constraint, just pop it off the list and we're done.
  unsigned lastIndex = Constraints.size()-1;
  if (index == lastIndex) {
    Constraints.pop_back();
    return;
  }

  // This constraint is somewhere in the middle; swap it with the last
  // constraint, so we can remove the constraint from the vector in O(1)
  // time rather than O(n) time.
  auto lastConstraint = Constraints[lastIndex];
  Constraints[index] = lastConstraint;
  ConstraintIndex[lastConstraint] = index;
  Constraints.pop_back();
}

ConstraintGraphNode::Adjacency &
ConstraintGraphNode::getAdjacency(TypeVariableType *typeVar) {
  assert(typeVar != TypeVar && "Cannot be adjacent to oneself");

  // Look for existing adjacency information.
  auto pos = AdjacencyInfo.find(typeVar);

  if (pos != AdjacencyInfo.end())
    return pos->second;

  // If we weren't already adjacent to this type variable, add it to the
  // list of adjacencies.
  pos = AdjacencyInfo.insert(
          { typeVar, { static_cast<unsigned>(Adjacencies.size()), 0 } })
         .first;
  Adjacencies.push_back(typeVar);
  return pos->second;
}

void ConstraintGraphNode::modifyAdjacency(
       TypeVariableType *typeVar,
       llvm::function_ref<void(Adjacency& adj)> modify) {
   // Find the adjacency information.
  auto pos = AdjacencyInfo.find(typeVar);
  assert(pos != AdjacencyInfo.end() && "Type variables not adjacent");
  assert(Adjacencies[pos->second.Index] == typeVar && "Mismatched adjacency");

  // Perform the modification .
  modify(pos->second);

  // If the adjacency is not empty, leave the information in there.
  if (!pos->second.empty())
    return;

  // Remove this adjacency from the mapping.
  unsigned index = pos->second.Index;
  AdjacencyInfo.erase(pos);

  // If this adjacency is last in the vector, just pop it off.
  unsigned lastIndex = Adjacencies.size()-1;
  if (index == lastIndex) {
    Adjacencies.pop_back();
    return;
  }

  // This adjacency is somewhere in the middle; swap it with the last
  // adjacency so we can remove the adjacency from the vector in O(1) time
  // rather than O(n) time.
  auto lastTypeVar = Adjacencies[lastIndex];
  Adjacencies[index] = lastTypeVar;
  AdjacencyInfo[lastTypeVar].Index = index;
  Adjacencies.pop_back();
}

void ConstraintGraphNode::addAdjacency(TypeVariableType *typeVar) {
  auto &adjacency = getAdjacency(typeVar);

  // Bump the degree of the adjacency.
  ++adjacency.NumConstraints;
}

void ConstraintGraphNode::removeAdjacency(TypeVariableType *typeVar) {
  modifyAdjacency(typeVar, [](Adjacency &adj) {
    assert(adj.NumConstraints > 0 && "No adjacency to remove?");
    --adj.NumConstraints;
  });
}

void ConstraintGraphNode::addToEquivalenceClass(
       ArrayRef<TypeVariableType *> typeVars) {
  assert(TypeVar == TypeVar->getImpl().getRepresentative(nullptr) &&
         "Can't extend equivalence class of non-representative type var");
  if (EquivalenceClass.empty())
    EquivalenceClass.push_back(TypeVar);
  EquivalenceClass.append(typeVars.begin(), typeVars.end());
}

void ConstraintGraphNode::addFixedBinding(TypeVariableType *typeVar) {
  FixedBindings.push_back(typeVar);
}

void ConstraintGraphNode::removeFixedBinding(TypeVariableType *typeVar) {
  FixedBindings.pop_back();
}

#pragma mark Graph scope management
ConstraintGraphScope::ConstraintGraphScope(ConstraintGraph &CG)
  : CG(CG), ParentScope(CG.ActiveScope), NumChanges(CG.Changes.size())
{
  CG.ActiveScope = this;
}

ConstraintGraphScope::~ConstraintGraphScope() {
  // Pop changes off the stack until we hit the change could we had prior to
  // introducing this scope.
  assert(CG.Changes.size() >= NumChanges && "Scope stack corrupted");
  while (CG.Changes.size() > NumChanges) {
    CG.Changes.back().undo(CG);
    CG.Changes.pop_back();
  }

  // The active scope is now the parent scope.
  CG.ActiveScope = ParentScope;
}

ConstraintGraph::Change
ConstraintGraph::Change::addedTypeVariable(TypeVariableType *typeVar) {
  Change result;
  result.Kind = ChangeKind::AddedTypeVariable;
  result.TypeVar = typeVar;
  return result;
}

ConstraintGraph::Change
ConstraintGraph::Change::addedConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::AddedConstraint;
  result.TheConstraint = constraint;
  return result;
}

ConstraintGraph::Change
ConstraintGraph::Change::removedConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::RemovedConstraint;
  result.TheConstraint = constraint;
  return result;
}

ConstraintGraph::Change
ConstraintGraph::Change::extendedEquivalenceClass(TypeVariableType *typeVar,
                                                  unsigned prevSize) {
  Change result;
  result.Kind = ChangeKind::ExtendedEquivalenceClass;
  result.EquivClass.TypeVar = typeVar;
  result.EquivClass.PrevSize = prevSize;
  return result;
}

ConstraintGraph::Change
ConstraintGraph::Change::boundTypeVariable(TypeVariableType *typeVar,
                                           Type fixed) {
  Change result;
  result.Kind = ChangeKind::BoundTypeVariable;
  result.Binding.TypeVar = typeVar;
  result.Binding.FixedType = fixed.getPointer();
  return result;
}

void ConstraintGraph::Change::undo(ConstraintGraph &cg) {
  /// Temporarily change the active scope to null, so we don't record
  /// any changes made while performing the undo operation.
  llvm::SaveAndRestore<ConstraintGraphScope *> prevActiveScope(cg.ActiveScope,
                                                               nullptr);

  switch (Kind) {
  case ChangeKind::AddedTypeVariable:
    cg.removeNode(TypeVar);
    break;

  case ChangeKind::AddedConstraint:
    cg.removeConstraint(TheConstraint);
    break;

  case ChangeKind::RemovedConstraint:
    cg.addConstraint(TheConstraint);
    break;

  case ChangeKind::ExtendedEquivalenceClass: {
    auto &node = cg[EquivClass.TypeVar];
    node.EquivalenceClass.erase(
      node.EquivalenceClass.begin() + EquivClass.PrevSize,
      node.EquivalenceClass.end());
    break;
   }

  case ChangeKind::BoundTypeVariable:
    cg.unbindTypeVariable(Binding.TypeVar, Binding.FixedType);
    break;
  }
}

#pragma mark Graph mutation

void ConstraintGraph::removeNode(TypeVariableType *typeVar) {
  // Remove this node.
  auto &impl = typeVar->getImpl();
  unsigned index = impl.getGraphIndex();
  delete impl.getGraphNode();
  impl.setGraphNode(nullptr);

  // Remove this type variable from the list.
  unsigned lastIndex = TypeVariables.size()-1;
  if (index < lastIndex)
    TypeVariables[index] = TypeVariables[lastIndex];
  TypeVariables.pop_back();
}

/// Enumerate the adjacency edges for the given constraint.
static void enumerateAdjacencies(
    Constraint *constraint,
    llvm::function_ref<void(TypeVariableType *, TypeVariableType *)> visitor) {
  // Don't record adjacencies for one-way constraints.
  if (constraint->isOneWayConstraint())
    return;

  // O(N^2) update for all of the adjacent type variables.
  auto referencedTypeVars = constraint->getTypeVariables();
  for (auto typeVar : referencedTypeVars) {
    for (auto otherTypeVar : referencedTypeVars) {
      if (typeVar == otherTypeVar)
        continue;

      visitor(typeVar, otherTypeVar);
    }
  }
}

void ConstraintGraph::addConstraint(Constraint *constraint) {
  // Record the change, if there are active scopes.
  if (ActiveScope) {
    Changes.push_back(Change::addedConstraint(constraint));
  }

  if (constraint->getTypeVariables().empty()) {
    // A constraint that doesn't reference any type variables is orphaned;
    // track it as such.
    OrphanedConstraints.push_back(constraint);
    return;
  }

  // Record this constraint in each type variable.
  for (auto typeVar : constraint->getTypeVariables()) {
    (*this)[typeVar].addConstraint(constraint);
  }

  // Record adjacencies.
  enumerateAdjacencies(constraint,
                       [&](TypeVariableType *lhs, TypeVariableType *rhs) {
    assert(lhs != rhs);
    (*this)[lhs].addAdjacency(rhs);
  });
}

void ConstraintGraph::removeConstraint(Constraint *constraint) {
  // Record the change, if there are active scopes.
  if (ActiveScope)
    Changes.push_back(Change::removedConstraint(constraint));

  if (constraint->getTypeVariables().empty()) {
    // A constraint that doesn't reference any type variables is orphaned;
    // remove it from the list of orphaned constraints.
    auto known = std::find(OrphanedConstraints.begin(),
                           OrphanedConstraints.end(),
                           constraint);
    assert(known != OrphanedConstraints.end() && "missing orphaned constraint");
    *known = OrphanedConstraints.back();
    OrphanedConstraints.pop_back();
    return;
  }

  // Remove the constraint from each type variable.
  for (auto typeVar : constraint->getTypeVariables()) {
    (*this)[typeVar].removeConstraint(constraint);
  }

  // Remove all adjacencies for all type variables.
  enumerateAdjacencies(constraint,
                       [&](TypeVariableType *lhs, TypeVariableType *rhs) {
    assert(lhs != rhs);
    (*this)[lhs].removeAdjacency(rhs);
  });
}

void ConstraintGraph::mergeNodes(TypeVariableType *typeVar1, 
                                 TypeVariableType *typeVar2) {
  assert(CS.getRepresentative(typeVar1) == CS.getRepresentative(typeVar2) &&
         "type representatives don't match");
  
  // Retrieve the node for the representative that we're merging into.
  auto typeVarRep = CS.getRepresentative(typeVar1);
  auto &repNode = (*this)[typeVarRep];

  // Retrieve the node for the non-representative.
  assert((typeVar1 == typeVarRep || typeVar2 == typeVarRep) &&
         "neither type variable is the new representative?");
  auto typeVarNonRep = typeVar1 == typeVarRep? typeVar2 : typeVar1;

  // Record the change, if there are active scopes.
  if (ActiveScope)
    Changes.push_back(Change::extendedEquivalenceClass(
                        typeVarRep,
                        repNode.getEquivalenceClass().size()));

  // Merge equivalence class from the non-representative type variable.
  auto &nonRepNode = (*this)[typeVarNonRep];
  repNode.addToEquivalenceClass(nonRepNode.getEquivalenceClassUnsafe());
}

void ConstraintGraph::bindTypeVariable(TypeVariableType *typeVar, Type fixed) {
  // If there are no type variables in the fixed type, there's nothing to do.
  if (!fixed->hasTypeVariable())
    return;

  SmallVector<TypeVariableType *, 4> typeVars;
  llvm::SmallPtrSet<TypeVariableType *, 4> knownTypeVars;
  fixed->getTypeVariables(typeVars);
  auto &node = (*this)[typeVar];
  for (auto otherTypeVar : typeVars) {
    if (knownTypeVars.insert(otherTypeVar).second) {
      if (typeVar == otherTypeVar) continue;

      (*this)[otherTypeVar].addFixedBinding(typeVar);
      node.addFixedBinding(otherTypeVar);
    }
  }

  // Record the change, if there are active scopes.
  // Note: If we ever use this to undo the actual variable binding,
  // we'll need to store the change along the early-exit path as well.
  if (ActiveScope)
    Changes.push_back(Change::boundTypeVariable(typeVar, fixed));
}

void ConstraintGraph::unbindTypeVariable(TypeVariableType *typeVar, Type fixed){
  // If there are no type variables in the fixed type, there's nothing to do.
  if (!fixed->hasTypeVariable())
    return;

  SmallVector<TypeVariableType *, 4> typeVars;
  llvm::SmallPtrSet<TypeVariableType *, 4> knownTypeVars;
  fixed->getTypeVariables(typeVars);
  auto &node = (*this)[typeVar];
  for (auto otherTypeVar : typeVars) {
    if (knownTypeVars.insert(otherTypeVar).second) {
      (*this)[otherTypeVar].removeFixedBinding(typeVar);
      node.removeFixedBinding(otherTypeVar);
    }
  }
}

llvm::TinyPtrVector<Constraint *> ConstraintGraph::gatherConstraints(
    TypeVariableType *typeVar, GatheringKind kind,
    llvm::function_ref<bool(Constraint *)> acceptConstraintFn) {
  llvm::TinyPtrVector<Constraint *> constraints;
  // Whether we should consider this constraint at all.
  auto rep = CS.getRepresentative(typeVar);
  auto shouldConsiderConstraint = [&](Constraint *constraint) {
    // For a one-way constraint, only consider it when the type variable
    // is on the right-hand side of the the binding, and the left-hand side of
    // the binding is one of the type variables currently under consideration.
    if (constraint->isOneWayConstraint()) {
      auto lhsTypeVar =
          constraint->getFirstType()->castTo<TypeVariableType>();
      if (!CS.isActiveTypeVariable(lhsTypeVar))
        return false;

      SmallVector<TypeVariableType *, 2> rhsTypeVars;
      constraint->getSecondType()->getTypeVariables(rhsTypeVars);
      for (auto rhsTypeVar : rhsTypeVars) {
        if (CS.getRepresentative(rhsTypeVar) == rep)
          return true;
      }
      return false;
    }

    return true;
  };

  auto acceptConstraint = [&](Constraint *constraint) {
    return shouldConsiderConstraint(constraint) &&
        acceptConstraintFn(constraint);
  };

  // Add constraints for the given adjacent type variable.
  llvm::SmallPtrSet<TypeVariableType *, 4> typeVars;

  // Local function to add constraints
  llvm::SmallPtrSet<Constraint *, 4> visitedConstraints;
  auto addConstraintsOfAdjacency = [&](TypeVariableType *adjTypeVar) {
    ArrayRef<TypeVariableType *> adjTypeVarsToVisit;
    switch (kind) {
    case GatheringKind::EquivalenceClass:
      adjTypeVarsToVisit = adjTypeVar;
      break;

    case GatheringKind::AllMentions:
      adjTypeVarsToVisit
        = (*this)[CS.getRepresentative(adjTypeVar)].getEquivalenceClass();
      break;
    }

    for (auto adjTypeVarEquiv : adjTypeVarsToVisit) {
      if (!typeVars.insert(adjTypeVarEquiv).second)
        continue;

      for (auto constraint : (*this)[adjTypeVarEquiv].getConstraints()) {
        if (visitedConstraints.insert(constraint).second &&
            acceptConstraint(constraint))
          constraints.push_back(constraint);
      }
    }
  };

  auto &reprNode = (*this)[CS.getRepresentative(typeVar)];
  auto equivClass = reprNode.getEquivalenceClass();
  for (auto typeVar : equivClass) {
    if (!typeVars.insert(typeVar).second)
      continue;

    for (auto constraint : (*this)[typeVar].getConstraints()) {
      if (visitedConstraints.insert(constraint).second &&
          acceptConstraint(constraint))
        constraints.push_back(constraint);
    }

    auto &node = (*this)[typeVar];

    for (auto adjTypeVar : node.getFixedBindings()) {
      addConstraintsOfAdjacency(adjTypeVar);
    }

    switch (kind) {
    case GatheringKind::EquivalenceClass:
      break;

    case GatheringKind::AllMentions:
      // Retrieve the constraints from adjacent bindings.
      for (auto adjTypeVar : node.getAdjacencies()) {
        addConstraintsOfAdjacency(adjTypeVar);
      }

      break;
    }

  }

  return constraints;
}

#pragma mark Algorithms

/// Perform a depth-first search.
///
/// \param cg The constraint graph.
/// \param typeVar The type variable we're searching from.
/// \param preVisitNode Called before traversing a node. Must return \c
/// false when the node has already been visited.
/// \param visitConstraint Called before considering a constraint. If it
/// returns \c false, that constraint will be skipped.
/// \param visitedConstraints Set of already-visited constraints, used
/// internally to avoid duplicated work.
static void depthFirstSearch(
    ConstraintGraph &cg,
    TypeVariableType *typeVar,
    llvm::function_ref<bool(TypeVariableType *)> preVisitNode,
    llvm::function_ref<bool(Constraint *)> visitConstraint,
    llvm::SmallPtrSet<Constraint *, 8> &visitedConstraints) {
  // Visit this node. If we've already seen it, bail out.
  if (!preVisitNode(typeVar))
    return;

  // Local function to visit adjacent type variables.
  auto visitAdjacencies = [&](ArrayRef<TypeVariableType *> adjTypeVars) {
    for (auto adj : adjTypeVars) {
      if (adj == typeVar)
        continue;

      // Recurse into this node.
      depthFirstSearch(cg, adj, preVisitNode, visitConstraint,
                       visitedConstraints);
    }
  };

  // Walk all of the constraints associated with this node to find related
  // nodes.
  auto &node = cg[typeVar];
  for (auto constraint : node.getConstraints()) {
    // If we've already seen this constraint, skip it.
    if (!visitedConstraints.insert(constraint).second)
      continue;

    if (visitConstraint(constraint))
      visitAdjacencies(constraint->getTypeVariables());
  }

  // Visit all of the other nodes in the equivalence class.
  auto repTypeVar = cg.getConstraintSystem().getRepresentative(typeVar);
  if (typeVar == repTypeVar) {
    // We are the representative, so visit all of the other type variables
    // in this equivalence class.
    visitAdjacencies(node.getEquivalenceClass());
  } else {
    // We are not the representative; visit the representative.
    visitAdjacencies(repTypeVar);
  }

  // Walk any type variables related via fixed bindings.
  visitAdjacencies(node.getFixedBindings());
}

namespace {
  /// A union-find connected components algorithm used to find the connected
  /// components within a constraint graph.
  class ConnectedComponents {
    ConstraintGraph &cg;
    ArrayRef<TypeVariableType *> typeVars;

    /// A mapping from each type variable to its representative in a union-find
    /// data structure, excluding entries where the type variable is its own
    /// representative.
    mutable llvm::SmallDenseMap<TypeVariableType *, TypeVariableType *>
        representatives;

    /// The complete set of constraints that were visited while computing
    /// connected components.
    llvm::SmallPtrSet<Constraint *, 8> visitedConstraints;

    /// Describes the one-way incoming and outcoming adjacencies of
    /// a component within the directed graph of one-way constraints.
    struct OneWayComponent {
      /// The (uniqued) set of type variable representatives to which this
      /// component has an outgoing edge.
      TinyPtrVector<TypeVariableType *> outAdjacencies;

      /// The (uniqued) set of type variable representatives from which this
      /// component has an incoming edge.
      TinyPtrVector<TypeVariableType *> inAdjacencies;
    };

    // Adjacency list representation of the directed graph of edges for
    // one-way constraints, using type variable representatives as the
    // nodes.
    llvm::SmallDenseMap<TypeVariableType *, OneWayComponent> oneWayDigraph;

  public:
    using Component = ConstraintGraph::Component;

    /// Compute connected components for the given set of type variables
    /// in the constraint graph.
    ConnectedComponents(ConstraintGraph &cg,
                        ArrayRef<TypeVariableType *> typeVars)
        : cg(cg), typeVars(typeVars)
    {
      auto oneWayConstraints = connectedComponents();

      // If there were no one-way constraints, we're done.
      if (oneWayConstraints.empty())
        return;

      // Build the directed one-way constraint graph.
      buildOneWayConstraintGraph(oneWayConstraints);
    }

    /// Retrieve the set of components.
    SmallVector<Component, 1> getComponents() const {
      // Figure out which components have unbound type variables and/or
      // constraints. These are the only components we want to report.
      llvm::SmallDenseSet<TypeVariableType *> validComponents;
      auto &cs = cg.getConstraintSystem();
      for (auto typeVar : typeVars) {
        // If this type variable has a fixed type, skip it.
        if (cs.getFixedType(typeVar))
          continue;

        auto rep = findRepresentative(typeVar);
        validComponents.insert(rep);
      }

      for (auto &constraint : cs.getConstraints()) {
        for (auto typeVar : constraint.getTypeVariables()) {
          auto rep = findRepresentative(typeVar);
          validComponents.insert(rep);
        }
      }

      // Capture the type variables of each component.
      llvm::SmallDenseMap<TypeVariableType *, Component> components;
      SmallVector<TypeVariableType *, 4> representativeTypeVars;
      for (auto typeVar : typeVars) {
        // Find the representative. If we aren't creating a type variable
        // for this component, skip it.
        auto rep = findRepresentative(typeVar);
        if (validComponents.count(rep) == 0)
          continue;

        // If this type variable is the representative, add it to the list of
        // representatives.
        if (rep == typeVar) {
          representativeTypeVars.push_back(rep);
        }

        // Record this type variable in the set of type variables for its
        // component.
        auto &component = components.insert(
            {rep, Component(components.size())}).first->second;
        component.typeVars.push_back(typeVar);
      }

      // Retrieve the component for the given representative type variable.
      auto getComponent = [&](TypeVariableType *rep) -> Component& {
        auto component = components.find(rep);
        assert(component != components.end());
        return component->second;
      };

      // Assign each constraint to its appropriate component.
      // Note: we use the inactive constraints so that we maintain the
      // order of constraints when we re-introduce them.
      for (auto &constraint : cs.getConstraints()) {
        auto constraintTypeVars = constraint.getTypeVariables();
        if (constraintTypeVars.empty())
          continue;

        TypeVariableType *typeVar;
        if (constraint.isOneWayConstraint()) {
          // For one-way constraints, associate the constraint with the
          // left-hand type variable.
          typeVar = constraint.getFirstType()->castTo<TypeVariableType>();
        } else {
          typeVar = constraintTypeVars.front();
        }

        auto rep = findRepresentative(typeVar);
        getComponent(rep).addConstraint(&constraint);
      }

      // If we have any one-way constraint information, compute the ordering
      // of representative type variables needed to respect one-way
      // constraints while solving.
      if (!oneWayDigraph.empty()) {
        // Sort the representative type variables based on the disjunction
        // count, so
        std::sort(representativeTypeVars.begin(), representativeTypeVars.end(),
                  [&](TypeVariableType *lhs, TypeVariableType *rhs) {
                    return getComponent(lhs).getNumDisjunctions() >
                        getComponent(rhs).getNumDisjunctions();
                  });
        
        representativeTypeVars =
            computeOneWayComponentOrdering(representativeTypeVars,
                                           validComponents);

        // Fill in one-way dependency information for all of the components.
        for (auto typeVar : representativeTypeVars) {
          auto knownOneWayComponent = oneWayDigraph.find(typeVar);
          if (knownOneWayComponent == oneWayDigraph.end())
            continue;

          auto &oneWayComponent = knownOneWayComponent->second;
          auto &component = getComponent(typeVar);
          for (auto inAdj : oneWayComponent.inAdjacencies) {
            if (validComponents.count(inAdj) == 0)
              continue;

            component.dependsOn.push_back(getComponent(inAdj).solutionIndex);
          }
        }
      }

      // Flatten the set of components.
      SmallVector<Component, 1> flatComponents;
      flatComponents.reserve(
          representativeTypeVars.size() + cg.getOrphanedConstraints().size());
      for (auto rep: representativeTypeVars) {
        assert(components.count(rep) == 1);
        flatComponents.push_back(std::move(getComponent(rep)));
      }

      // Gather orphaned constraints; each gets its own component.
      for (auto orphaned : cg.getOrphanedConstraints()) {
        flatComponents.push_back(Component(flatComponents.size()));
        flatComponents.back().addConstraint(orphaned);
      }

      // Create component ordering based on the information associated
      // with constraints in each step - e.g. number of disjunctions,
      // since components are going to be executed in LIFO order, we'd
      // want to have smaller/faster components at the back of the list.
      // When there are one-way constraints, we can't reorder them, so only
      // sort the orphaned constraints at the back. In the absense of
      // one-way constraints, sort everything.
      if (components.size() > 1) {
        auto sortStart = oneWayDigraph.empty()
            ? flatComponents.begin()
            : flatComponents.end() - cg.getOrphanedConstraints().size();
        std::sort(sortStart, flatComponents.end(),
                  [&](const Component &lhs, const Component &rhs) {
                    return lhs.getNumDisjunctions() > rhs.getNumDisjunctions();
                  });
      }

      return flatComponents;
    }

    /// Find the representative for the given type variable within the set
    /// of representatives in a union-find data structure.
    TypeVariableType *findRepresentative(TypeVariableType *typeVar) const {
      // If we don't have a record of this type variable, it is it's own
      // representative.
      auto known = representatives.find(typeVar);
      if (known == representatives.end() || known->second == typeVar)
        return typeVar;

      // Find the representative of the parent.
      auto parent = known->second;
      auto rep = findRepresentative(parent);
      representatives[typeVar] = rep;

      return rep;
    }

  private:
    /// Perform the union of two type variables in a union-find data structure
    /// used for connected components.
    ///
    /// \returns true if the two components were separate and have now been
    /// joined, \c false if they were already in the same set.
    bool unionSets(TypeVariableType *typeVar1, TypeVariableType *typeVar2) {
      auto rep1 = findRepresentative(typeVar1);
      auto rep2 = findRepresentative(typeVar2);
      if (rep1 == rep2)
        return false;

      // Reparent the type variable with the higher ID. The actual choice doesn't
      // matter, but this makes debugging easier.
      if (rep1->getID() < rep2->getID())
        representatives[rep2] = rep1;
      else
        representatives[rep1] = rep2;
      return true;
    }

    /// Perform the connected components algorithm, skipping one-way
    /// constraints.
    ///
    /// \returns the set of one-way constraints that were skipped.
    TinyPtrVector<Constraint *> connectedComponents() {
      TinyPtrVector<Constraint *> oneWayConstraints;

      // Perform a depth-first search from each type variable to identify
      // what component it is in.
      for (auto typeVar : typeVars) {
        // If we've already assigned a representative to this type variable,
        // we're done.
        if (representatives.count(typeVar) > 0)
          continue;

        // Perform a depth-first search to mark those type variables that are
        // in the same component as this type variable.
        depthFirstSearch(
            cg, typeVar,
            [&](TypeVariableType *found) {
              // If we have already seen this node, we're done.
              auto inserted = representatives.insert({found, typeVar});
              assert((inserted.second || inserted.first->second == typeVar) &&
                     "Wrong component?");

              return inserted.second;
            },
            [&](Constraint *constraint) {
              // Record and skip one-way constraints.
              if (constraint->isOneWayConstraint()) {
                oneWayConstraints.push_back(constraint);
                return false;
              }

              return true;
            },
            visitedConstraints);
      }

      return oneWayConstraints;
    }

    /// Insert the given type variable into the given vector if it isn't
    /// already present.
    static void insertIfUnique(TinyPtrVector<TypeVariableType *> &vector,
                               TypeVariableType *typeVar) {
      if (std::find(vector.begin(), vector.end(), typeVar) == vector.end())
        vector.push_back(typeVar);
    }

    /// Retrieve the (uniqued) set of type variable representations that occur
    /// within the given type.
    TinyPtrVector<TypeVariableType *>
    getRepresentativesInType(Type type) const {
      TinyPtrVector<TypeVariableType *> results;

      SmallVector<TypeVariableType *, 2> typeVars;
      type->getTypeVariables(typeVars);
      for (auto typeVar : typeVars) {
        auto rep = findRepresentative(typeVar);
        insertIfUnique(results, rep);
      }

      return results;
    }

    /// Add all of the one-way constraints to the one-way digraph
    void addOneWayConstraintEdges(ArrayRef<Constraint *> oneWayConstraints) {
      for (auto constraint : oneWayConstraints) {
        auto lhsTypeReps =
            getRepresentativesInType(constraint->getFirstType());
        auto rhsTypeReps =
            getRepresentativesInType(constraint->getSecondType());

        // Add an edge from the type representatives on the right-hand side
        // of the one-way constraint to the type representatives on the
        // left-hand side, because the right-hand type variables need to
        // be solved before the left-hand type variables.
        for (auto lhsTypeRep : lhsTypeReps) {
          for (auto rhsTypeRep : rhsTypeReps) {
            if (lhsTypeRep == rhsTypeRep)
              break;

            insertIfUnique(oneWayDigraph[rhsTypeRep].outAdjacencies,lhsTypeRep);
            insertIfUnique(oneWayDigraph[lhsTypeRep].inAdjacencies,rhsTypeRep);
          }
        }
      }
    }

    using TypeVariablePair = std::pair<TypeVariableType *, TypeVariableType *>;

    /// Build the directed graph of one-way constraints among components.
    void buildOneWayConstraintGraph(ArrayRef<Constraint *> oneWayConstraints) {
      auto &cs = cg.getConstraintSystem();
      auto &ctx = cs.getASTContext();
      bool contractedCycle = false;
      do {
        // Construct the one-way digraph from scratch.
        oneWayDigraph.clear();
        addOneWayConstraintEdges(oneWayConstraints);

        // Minimize the in-adjacencies, detecting cycles along the way.
        SmallVector<TypeVariablePair, 4> cycleEdges;
        removeIndirectOneWayInAdjacencies(cycleEdges);

        // For any contractions we need to perform due to cycles, perform a
        // union the connected components based on the type variable pairs.
        contractedCycle = false;
        for (const auto &edge : cycleEdges) {
          if (unionSets(edge.first, edge.second)) {
            if (ctx.LangOpts.DebugConstraintSolver) {
              auto &log = ctx.TypeCheckerDebug->getStream();
              if (cs.solverState)
                log.indent(cs.solverState->depth * 2);

              log << "Collapsing one-way components for $T"
                  << edge.first->getID() << " and $T" << edge.second->getID()
                  << " due to cycle.\n";
            }

            if (ctx.Stats) {
              ctx.Stats->getFrontendCounters()
                  .NumCyclicOneWayComponentsCollapsed++;
            }

            contractedCycle = true;
          }
        }
      } while (contractedCycle);
    }

    /// Perform a depth-first search to produce a from the given type variable,
    /// notifying the function object.
    ///
    /// \param getAdjacencies Called to retrieve the set of type variables
    /// that are adjacent to the given type variable.
    ///
    /// \param preVisit Called before visiting the adjacencies of the given
    /// type variable. When it returns \c true, the adjacencies of this type
    /// variable will be visited. When \c false, the adjacencies will not be
    /// visited and \c postVisit will not be called.
    ///
    /// \param postVisit Called after visiting the adjacencies of the given
    /// type variable.
    static void postorderDepthFirstSearchRec(
        TypeVariableType *typeVar,
        llvm::function_ref<
          ArrayRef<TypeVariableType *>(TypeVariableType *)> getAdjacencies,
        llvm::function_ref<bool(TypeVariableType *)> preVisit,
        llvm::function_ref<void(TypeVariableType *)> postVisit) {
      if (!preVisit(typeVar))
        return;

      for (auto adj : getAdjacencies(typeVar)) {
        postorderDepthFirstSearchRec(adj, getAdjacencies, preVisit, postVisit);
      }

      postVisit(typeVar);
    }

    /// Minimize the incoming adjacencies for one of the nodes in the one-way
    /// directed graph by eliminating any in-adjacencies that can also be
    /// found indirectly.
    void removeIndirectOneWayInAdjacencies(
        TypeVariableType *typeVar,
        OneWayComponent &component,
        SmallVectorImpl<TypeVariablePair> &cycleEdges) {
      // Perform a depth-first search from each of the in adjacencies to
      // this type variable, traversing each of the one-way edges backwards
      // to find all of the components whose type variables must be
      // bound before this component can be solved.
      SmallPtrSet<TypeVariableType *, 4> visited;
      SmallPtrSet<TypeVariableType *, 4> indirectlyReachable;
      SmallVector<TypeVariableType *, 4> currentPath;
      for (auto inAdj : component.inAdjacencies) {
        postorderDepthFirstSearchRec(
            inAdj,
            [&](TypeVariableType *typeVar) -> ArrayRef<TypeVariableType *> {
              // Traverse the outgoing adjacencies for the subcomponent
              auto oneWayComponent = oneWayDigraph.find(typeVar);
              if (oneWayComponent == oneWayDigraph.end()) {
                return { };
              }

              return oneWayComponent->second.inAdjacencies;
            },
            [&](TypeVariableType *typeVar) {
              // If we haven't seen this type variable yet, add it to the
              // path.
              if (visited.insert(typeVar).second) {
                currentPath.push_back(typeVar);
                return true;
              }

              // Add edges between this type variable and every other type
              // variable in the path.
              for (auto otherTypeVar : llvm::reverse(currentPath)) {
                // When we run into our own type variable, we're done.
                if (otherTypeVar == typeVar)
                  break;

                cycleEdges.push_back({typeVar, otherTypeVar});
              }

              return false;
            },
            [&](TypeVariableType *dependsOn) {
              // Remove this type variable from the path.
              assert(currentPath.back() == dependsOn);
              currentPath.pop_back();

              // Don't record dependency on ourselves.
              if (dependsOn == inAdj)
                return;

              indirectlyReachable.insert(dependsOn);
            });

        // Remove any in-adjacency of this component that is indirectly
        // reachable.
        component.inAdjacencies.erase(
            std::remove_if(component.inAdjacencies.begin(),
                           component.inAdjacencies.end(),
                           [&](TypeVariableType *inAdj) {
                             return indirectlyReachable.count(inAdj) > 0;
                           }),
            component.inAdjacencies.end());
      }
    }

    /// Minimize the incoming adjacencies for all of the nodes in the one-way
    /// directed graph by eliminating any in-adjacencies that can also be
    /// found indirectly.
    void removeIndirectOneWayInAdjacencies(
        SmallVectorImpl<TypeVariablePair> &cycleEdges)  {
      for (auto &oneWayEntry : oneWayDigraph) {
        auto typeVar = oneWayEntry.first;
        auto &component = oneWayEntry.second;
        removeIndirectOneWayInAdjacencies(typeVar, component, cycleEdges);
      }
    }

    /// Compute the order in which the components should be visited to respect
    /// one-way constraints.
    ///
    /// \param representativeTypeVars the set of type variables that
    /// represent the components, in a preferred ordering that does not
    /// account for one-way constraints.
    /// \returns the set of type variables that represent the components, in
    /// an ordering that ensures that components containing type variables
    /// that occur on the left-hand side of a one-way constraint will be
    /// solved after the components for type variables on the right-hand
    /// side of that constraint.
    SmallVector<TypeVariableType *, 4> computeOneWayComponentOrdering(
        ArrayRef<TypeVariableType *> representativeTypeVars,
        llvm::SmallDenseSet<TypeVariableType *> &validComponents) const {
      SmallVector<TypeVariableType *, 4> orderedReps;
      orderedReps.reserve(representativeTypeVars.size());
      SmallPtrSet<TypeVariableType *, 4> visited;
      for (auto rep : llvm::reverse(representativeTypeVars)) {
        // Perform a postorder depth-first search through the one-way digraph,
        // starting at this representative, to establish the dependency
        // ordering amongst components that are reachable
        // to establish the dependency ordering for the representative type
        // variables.
        postorderDepthFirstSearchRec(
            rep,
            [&](TypeVariableType *typeVar) -> ArrayRef<TypeVariableType *> {
              // Traverse the outgoing adjacencies for the subcomponent
              assert(typeVar == findRepresentative(typeVar));
              auto oneWayComponent = oneWayDigraph.find(typeVar);
              if (oneWayComponent == oneWayDigraph.end()) {
                return { };
              }

              return oneWayComponent->second.outAdjacencies;
            },
            [&](TypeVariableType *typeVar) {
              return visited.insert(typeVar).second;
            },
            [&](TypeVariableType *typeVar) {
              // Record this type variable, if it's one of the representative
              // type variables.
              if (validComponents.count(typeVar) > 0)
                orderedReps.push_back(typeVar);
            });
      }

      assert(orderedReps.size() == representativeTypeVars.size());
      return orderedReps;
    }
  };
}

void ConstraintGraph::Component::addConstraint(Constraint *constraint) {
  if (constraint->getKind() == ConstraintKind::Disjunction)
    ++numDisjunctions;

  constraints.push_back(constraint);
}

SmallVector<ConstraintGraph::Component, 1>
ConstraintGraph::computeConnectedComponents(
           ArrayRef<TypeVariableType *> typeVars) {
  // Perform connected components via a union-find algorithm on all of the
  // constraints adjacent to these type variables.
  ConnectedComponents cc(*this, typeVars);
  return cc.getComponents();
}


/// For a given constraint kind, decide if we should attempt to eliminate its
/// edge in the graph.
static bool shouldContractEdge(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
    return true;

  default:
    return false;
  }
}

bool ConstraintGraph::contractEdges() {
  SmallVector<Constraint *, 16> constraints;
  CS.findConstraints(constraints, [&](const Constraint &constraint) {
    // Track how many constraints did contraction algorithm iterated over.
    incrementConstraintsPerContractionCounter();
    return shouldContractEdge(constraint.getKind());
  });

  bool didContractEdges = false;
  for (auto *constraint : constraints) {
    auto kind = constraint->getKind();

    // Contract binding edges between type variables.
    assert(shouldContractEdge(kind));

    auto t1 = constraint->getFirstType()->getDesugaredType();
    auto t2 = constraint->getSecondType()->getDesugaredType();

    auto tyvar1 = t1->getAs<TypeVariableType>();
    auto tyvar2 = t2->getAs<TypeVariableType>();

    if (!(tyvar1 && tyvar2))
      continue;

    auto isParamBindingConstraint = kind == ConstraintKind::BindParam;

    // If the argument is allowed to bind to `inout`, in general,
    // it's invalid to contract the edge between argument and parameter,
    // but if we can prove that there are no possible bindings
    // which result in attempt to bind `inout` type to argument
    // type variable, we should go ahead and allow (temporary)
    // contraction, because that greatly helps with performance.
    // Such action is valid because argument type variable can
    // only get its bindings from related overload, which gives
    // us enough information to decided on l-valueness.
    if (isParamBindingConstraint && tyvar1->getImpl().canBindToInOut()) {
      bool isNotContractable = true;
      if (auto bindings = CS.getPotentialBindings(tyvar1)) {
        for (auto &binding : bindings.Bindings) {
          auto type = binding.BindingType;
          isNotContractable = type.findIf([&](Type nestedType) -> bool {
            if (auto tv = nestedType->getAs<TypeVariableType>()) {
              if (tv->getImpl().canBindToInOut())
                return true;
            }

            return nestedType->is<InOutType>();
          });

          // If there is at least one non-contractable binding, let's
          // not risk contracting this edge.
          if (isNotContractable)
            break;
        }
      }

      if (isNotContractable)
        continue;
    }

    auto rep1 = CS.getRepresentative(tyvar1);
    auto rep2 = CS.getRepresentative(tyvar2);

    if (((rep1->getImpl().canBindToLValue() ==
          rep2->getImpl().canBindToLValue()) ||
         // Allow l-value contractions when binding parameter types.
         isParamBindingConstraint)) {
      if (CS.getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
        if (CS.solverState)
          log.indent(CS.solverState->depth * 2);

        log << "Contracting constraint ";
        constraint->print(log, &CS.getASTContext().SourceMgr);
        log << "\n";
      }

      // Merge the edges and remove the constraint.
      removeEdge(constraint);
      if (rep1 != rep2)
        CS.mergeEquivalenceClasses(rep1, rep2, /*updateWorkList*/ false);
      didContractEdges = true;
    }
  }
  return didContractEdges;
}

void ConstraintGraph::removeEdge(Constraint *constraint) {
  bool isExistingConstraint = false;

  for (auto &active : CS.ActiveConstraints) {
    if (&active == constraint) {
      CS.ActiveConstraints.erase(constraint);
      isExistingConstraint = true;
      break;
    }
  }

  for (auto &inactive : CS.InactiveConstraints) {
    if (&inactive == constraint) {
      CS.InactiveConstraints.erase(constraint);
      isExistingConstraint = true;
      break;
    }
  }

  if (CS.solverState) {
    if (isExistingConstraint)
      CS.solverState->retireConstraint(constraint);
    else
      CS.solverState->removeGeneratedConstraint(constraint);
  }

  removeConstraint(constraint);
}

void ConstraintGraph::optimize() {
  // Merge equivalence classes until a fixed point is reached.
  while (contractEdges()) {}
}

void ConstraintGraph::incrementConstraintsPerContractionCounter() {
  SWIFT_FUNC_STAT;
  auto &context = CS.getASTContext();
  if (context.Stats)
    context.Stats->getFrontendCounters()
        .NumConstraintsConsideredForEdgeContraction++;
}

#pragma mark Debugging output

void ConstraintGraphNode::print(llvm::raw_ostream &out, unsigned indent) const {
  out.indent(indent);
  TypeVar->print(out);
  out << ":\n";

  // Print constraints.
  if (!Constraints.empty()) {
    out.indent(indent + 2);
    out << "Constraints:\n";
    SmallVector<Constraint *, 4> sortedConstraints(Constraints.begin(),
                                                   Constraints.end());
    std::sort(sortedConstraints.begin(), sortedConstraints.end());
    for (auto constraint : sortedConstraints) {
      out.indent(indent + 4);
      constraint->print(out, &TypeVar->getASTContext().SourceMgr);
      out << "\n";
    }
  }

 if (!Adjacencies.empty()) {
   out.indent(indent + 2);
   out << "Adjacencies:";
   SmallVector<TypeVariableType *, 4> sortedAdjacencies(Adjacencies.begin(),
                                                        Adjacencies.end());
   std::sort(sortedAdjacencies.begin(), sortedAdjacencies.end(),
             [](TypeVariableType *lhs, TypeVariableType *rhs) {
               return lhs->getID() < rhs->getID();
             });
   for (auto adj : sortedAdjacencies) {
     out << ' ';
     adj->print(out);

     const auto info = AdjacencyInfo.lookup(adj);
     auto degree = info.NumConstraints;
     if (degree > 1) {
       out << " (" << degree << ")";
     }
   }
   out << "\n";
 }

  // Print fixed bindings.
  if (!FixedBindings.empty()) {
    out.indent(indent + 2);
    out << "Fixed bindings: ";
    SmallVector<TypeVariableType *, 4> sortedFixedBindings(
        FixedBindings.begin(), FixedBindings.end());
    std::sort(sortedFixedBindings.begin(), sortedFixedBindings.end(),
              [&](TypeVariableType *typeVar1, TypeVariableType *typeVar2) {
                return typeVar1->getID() < typeVar2->getID();
              });

    interleave(sortedFixedBindings,
               [&](TypeVariableType *typeVar) {
                 out << "$T" << typeVar->getID();
               },
               [&]() {
                 out << ", ";
               });
    out << "\n";
  }

  // Print equivalence class.
  if (TypeVar->getImpl().getRepresentative(nullptr) == TypeVar &&
      EquivalenceClass.size() > 1) {
    out.indent(indent + 2);
    out << "Equivalence class:";
    for (unsigned i = 1, n = EquivalenceClass.size(); i != n; ++i) {
      out << ' ';
      EquivalenceClass[i]->print(out);
    }
    out << "\n";
  }
}

void ConstraintGraphNode::dump() const {
  llvm::SaveAndRestore<bool>
    debug(TypeVar->getASTContext().LangOpts.DebugConstraintSolver, true);
  print(llvm::dbgs(), 0);
}

void ConstraintGraph::print(ArrayRef<TypeVariableType *> typeVars,
                            llvm::raw_ostream &out) {
  for (auto typeVar : typeVars) {
    (*this)[typeVar].print(out, 2);
    out << "\n";
  }
}

void ConstraintGraph::dump() {
  dump(llvm::dbgs());
}

void ConstraintGraph::dump(llvm::raw_ostream &out) {
  llvm::SaveAndRestore<bool>
    debug(CS.getASTContext().LangOpts.DebugConstraintSolver, true);
  print(CS.getTypeVariables(), out);
}

void ConstraintGraph::printConnectedComponents(
    ArrayRef<TypeVariableType *> typeVars,
    llvm::raw_ostream &out) {
  auto components = computeConnectedComponents(typeVars);
  for (const auto& component : components) {
    out.indent(2);
    out << component.solutionIndex << ": ";
    SWIFT_DEFER {
      out << '\n';
    };

    // Print all of the type variables in this connected component.
    interleave(component.typeVars,
               [&](TypeVariableType *typeVar) {
                 typeVar->print(out);
               },
               [&] {
                 out << ' ';
               });

    if (component.dependsOn.empty())
      continue;

    // Print all of the one-way components.
    out << " depends on ";
    interleave(
        component.dependsOn,
        [&](unsigned index) { out << index; },
        [&] { out << ", "; }
      );
  }
}

void ConstraintGraph::dumpConnectedComponents() {
  llvm::SaveAndRestore<bool>
    debug(CS.getASTContext().LangOpts.DebugConstraintSolver, true);
  printConnectedComponents(CS.getTypeVariables(), llvm::dbgs());
}

#pragma mark Verification of graph invariants

/// Require that the given condition evaluate true.
///
/// If the condition is not true, complain about the problem and abort.
///
/// \param condition The actual Boolean condition.
///
/// \param complaint A string that describes the problem.
///
/// \param cg The constraint graph that failed verification.
///
/// \param node If non-null, the graph node that failed verification.
///
/// \param extraContext If provided, a function that will be called to
/// provide extra, contextual information about the failure.
static void _require(bool condition, const Twine &complaint,
                     ConstraintGraph &cg,
                     ConstraintGraphNode *node,
                     const std::function<void()> &extraContext = nullptr) {
  if (condition)
    return;

  // Complain
  llvm::dbgs() << "Constraint graph verification failed: " << complaint << '\n';
  if (extraContext)
    extraContext();

  // Print the graph.
  // FIXME: Highlight the offending node/constraint/etc.
  cg.dump(llvm::dbgs());

  abort();
}

/// Print a type variable value.
static void printValue(llvm::raw_ostream &os, TypeVariableType *typeVar) {
  typeVar->print(os);
}

/// Print a constraint value.
static void printValue(llvm::raw_ostream &os, Constraint *constraint) {
  constraint->print(os, nullptr);
}

/// Print an unsigned value.
static void printValue(llvm::raw_ostream &os, unsigned value) {
  os << value;
}

void ConstraintGraphNode::verify(ConstraintGraph &cg) {
#define require(condition, complaint) _require(condition, complaint, cg, this)
#define requireWithContext(condition, complaint, context) \
  _require(condition, complaint, cg, this, context)
#define requireSameValue(value1, value2, complaint)             \
  _require(value1 == value2, complaint, cg, this, [&] {         \
    llvm::dbgs() << "  ";                                       \
    printValue(llvm::dbgs(), value1);                           \
    llvm::dbgs() << " != ";                                     \
    printValue(llvm::dbgs(), value2);                           \
    llvm::dbgs() << '\n';                                       \
  })

  // Verify that the constraint map/vector haven't gotten out of sync.
  requireSameValue(Constraints.size(), ConstraintIndex.size(),
                   "constraint vector and map have different sizes");
  for (auto info : ConstraintIndex) {
    require(info.second < Constraints.size(), "constraint index out-of-range");
    requireSameValue(info.first, Constraints[info.second],
                     "constraint map provides wrong index into vector");
  }

  // Verify that the adjacency map/vector haven't gotten out of sync.
  requireSameValue(Adjacencies.size(), AdjacencyInfo.size(),
                   "adjacency vector and map have different sizes");
  for (auto info : AdjacencyInfo) {
    require(info.second.Index < Adjacencies.size(),
            "adjacency index out-of-range");
    requireSameValue(info.first, Adjacencies[info.second.Index],
                     "adjacency map provides wrong index into vector");
    require(!info.second.empty(),
            "adjacency information should have been removed");
    require(info.second.NumConstraints <= Constraints.size(),
            "adjacency information has higher degree than # of constraints");
  }

  // Based on the constraints we have, build up a representation of what
  // we expect the adjacencies to look like.
  llvm::DenseMap<TypeVariableType *, unsigned> expectedAdjacencies;
  for (auto constraint : Constraints) {
    if (constraint->isOneWayConstraint())
      continue;

    for (auto adjTypeVar : constraint->getTypeVariables()) {
      if (adjTypeVar == TypeVar)
        continue;

      ++expectedAdjacencies[adjTypeVar];
    }
  }

  // Make sure that the adjacencies we expect are the adjacencies we have.
  for (auto adj : expectedAdjacencies) {
    auto knownAdj = AdjacencyInfo.find(adj.first);
    requireWithContext(knownAdj != AdjacencyInfo.end(),
                       "missing adjacency information for type variable",
                       [&] {
      llvm::dbgs() << "  type variable=" << adj.first->getString() << 'n';
    });

    requireWithContext(adj.second == knownAdj->second.NumConstraints,
                       "wrong number of adjacencies for type variable",
                       [&] {
       llvm::dbgs() << "  type variable=" << adj.first->getString()
                    << " (" << adj.second << " vs. "
                    << knownAdj->second.NumConstraints
                    << ")\n";
     });
  }

  if (AdjacencyInfo.size() != expectedAdjacencies.size()) {
    // The adjacency information has something extra in it. Find the
    // extraneous type variable.
    for (auto adj : AdjacencyInfo) {
      requireWithContext(AdjacencyInfo.count(adj.first) > 0,
                         "extraneous adjacency info for type variable",
                         [&] {
        llvm::dbgs() << "  type variable=" << adj.first->getString() << '\n';
      });
    }
  }

#undef requireSameValue
#undef requireWithContext
#undef require
}

void ConstraintGraph::verify() {
#define require(condition, complaint) \
  _require(condition, complaint, *this, nullptr)
#define requireWithContext(condition, complaint, context) \
  _require(condition, complaint, *this, nullptr, context)
#define requireSameValue(value1, value2, complaint)             \
  _require(value1 == value2, complaint, *this, nullptr, [&] {   \
    llvm::dbgs() << "  ";                                       \
    printValue(llvm::dbgs(), value1);                           \
    llvm::dbgs() << " != ";                                     \
    printValue(llvm::dbgs(), value2);                           \
    llvm::dbgs() << '\n';                                       \
  })

  // Verify that the type variables are either representatives or represented
  // within their representative's equivalence class.
  // FIXME: Also check to make sure the equivalence classes aren't too large?
  for (auto typeVar : TypeVariables) {
    auto typeVarRep = CS.getRepresentative(typeVar);
    auto &repNode = (*this)[typeVarRep];
    if (typeVar != typeVarRep) {
      // This type variable should be in the equivalence class of its
      // representative.
      require(std::find(repNode.getEquivalenceClass().begin(),
                        repNode.getEquivalenceClass().end(),
                        typeVar) != repNode.getEquivalenceClass().end(),
              "type variable not present in its representative's equiv class");
    } else {
      // Each of the type variables in the same equivalence class as this type
      // should have this type variable as their representative.
      for (auto equiv : repNode.getEquivalenceClass()) {
        requireSameValue(
          typeVar, equiv->getImpl().getRepresentative(nullptr),
          "representative and an equivalent type variable's representative");
      }
    }
  }

  // Verify that our type variable map/vector are in sync.
  for (unsigned i = 0, n = TypeVariables.size(); i != n; ++i) {
    auto typeVar = TypeVariables[i];
    auto &impl = typeVar->getImpl();
    requireSameValue(impl.getGraphIndex(), i, "wrong graph node index");
    require(impl.getGraphNode(), "null graph node");
  }

  // Verify consistency of all of the nodes in the graph.
  for (unsigned i = 0, n = TypeVariables.size(); i != n; ++i) {
    auto typeVar = TypeVariables[i];
    auto &impl = typeVar->getImpl();
    impl.getGraphNode()->verify(*this);
  }

  // Collect all of the constraints known to the constraint graph.
  llvm::SmallPtrSet<Constraint *, 4> knownConstraints;
  for (auto typeVar : getTypeVariables()) {
    for (auto constraint : (*this)[typeVar].getConstraints())
      knownConstraints.insert(constraint);
  }

  // Verify that all of the constraints in the constraint system
  // are accounted for.
  for (auto &constraint : CS.getConstraints()) {
    // Check whether the constraint graph knows about this constraint.
    auto referencedTypeVars = constraint.getTypeVariables();
    requireWithContext((knownConstraints.count(&constraint) ||
                        referencedTypeVars.empty()),
                       "constraint graph doesn't know about constraint",
                       [&] {
                         llvm::dbgs() << "constraint = ";
                         printValue(llvm::dbgs(), &constraint);
                         llvm::dbgs() << "\n";
                       });

    // Make sure each of the type variables referenced knows about this
    // constraint.
    for (auto typeVar : referencedTypeVars) {
      auto nodePtr = typeVar->getImpl().getGraphNode();
      requireWithContext(nodePtr,
                         "type variable in constraint not known",
                         [&] {
                           llvm::dbgs() << "type variable = ";
                           printValue(llvm::dbgs(), typeVar);
                           llvm::dbgs() << ", constraint = ";
                           printValue(llvm::dbgs(), &constraint);
                           llvm::dbgs() << "\n";
                         });

      auto &node = *nodePtr;
      auto constraintPos = node.ConstraintIndex.find(&constraint);
      requireWithContext(constraintPos != node.ConstraintIndex.end(),
                         "type variable doesn't know about constraint",
                         [&] {
                           llvm::dbgs() << "type variable = ";
                           printValue(llvm::dbgs(), typeVar);
                           llvm::dbgs() << ", constraint = ";
                           printValue(llvm::dbgs(), &constraint);
                           llvm::dbgs() << "\n";
                         });
    }
  }

#undef requireSameValue
#undef requireWithContext
#undef require
}


