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

#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/CSTrail.h"
#include "swift/Basic/Assertions.h"
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
#ifndef NDEBUG
  for (unsigned i = 0, n = CS.TypeVariables.size(); i != n; ++i) {
    auto &impl = CS.TypeVariables[i]->getImpl();
    ASSERT(impl.getGraphNode() == nullptr);
  }
#endif

  for (auto *node : FreeList) {
    delete node;
  }
}

#pragma mark Graph accessors

void ConstraintGraph::addTypeVariable(TypeVariableType *typeVar) {
  // Check whether we've already created a node for this type variable.
  auto &impl = typeVar->getImpl();

  // ComponentStep::Scope re-introduces type variables that are already
  // in the graph, but not in ConstraintSystem::TypeVariables.
  if (impl.getGraphNode())
    return;

  ASSERT(!impl.hasRepresentativeOrFixed());

  // Allocate the new node.
  ConstraintGraphNode *nodePtr;
  if (FreeList.empty())
    nodePtr = new ConstraintGraphNode(*this, typeVar);
  else {
    nodePtr = FreeList.back();
    FreeList.pop_back();
    nodePtr->initTypeVariable(typeVar);
  }
  impl.setGraphNode(nodePtr);

  if (CS.solverState)
    CS.recordChange(SolverTrail::Change::AddedTypeVariable(typeVar));
}

ConstraintGraphNode &
ConstraintGraph::operator[](TypeVariableType *typeVar) {
  auto *nodePtr = typeVar->getImpl().getGraphNode();
  ASSERT(nodePtr->TypeVar == typeVar && "Use-after-free");
  return *nodePtr;
}

void ConstraintGraphNode::reset() {
  if (CONDITIONAL_ASSERT_enabled()) {
    ASSERT(TypeVar);
    ASSERT(Constraints.empty());
    ASSERT(ConstraintIndex.empty());
    ASSERT(ReferencedBy.empty());
    ASSERT(References.empty());
    ASSERT(EquivalenceClass.size() <= 1);
  }

  TypeVar = nullptr;
  EquivalenceClass.clear();
  Potential.reset();
  Set.reset();
}

bool ConstraintGraphNode::forRepresentativeVar() const {
  auto *typeVar = getTypeVariable();
  return typeVar == typeVar->getImpl().getRepresentative(nullptr);
}

ArrayRef<TypeVariableType *> ConstraintGraphNode::getEquivalenceClass() const{
  assert(forRepresentativeVar() &&
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

static bool isUsefulForReferencedVars(Constraint *constraint) {
  switch (constraint->getKind()) {
    // Don't attempt to propagate information about `Bind`s and
    // `BindOverload`s to referenced variables since they are
    // adjacent through that binding already, and there is no
    // useful information in trying to process that kind of
    // constraint.
  case ConstraintKind::Bind:
  case ConstraintKind::BindOverload:
    return false;

  default:
    return true;
  }
}

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

void ConstraintGraphNode::notifyReferencingVars(
    llvm::function_ref<void(ConstraintGraphNode &,
                            Constraint *)> notification) const {
  SmallVector<TypeVariableType *, 4> stack;

  stack.push_back(TypeVar);

  auto updateAdjacencies = [&](TypeVariableType *typeVar) {
    for (auto *constraint : CG[typeVar].getConstraints()) {
      if (constraint->getClassification() !=
          ConstraintClassification::Relational)
        continue;

      auto lhsTy = constraint->getFirstType();
      auto rhsTy = constraint->getSecondType();

      Type affectedTy =
          ConstraintSystem::typeVarOccursInType(typeVar, lhsTy) ? rhsTy : lhsTy;

      if (auto *affectedVar = affectedTy->getAs<TypeVariableType>()) {
        auto *repr =
            affectedVar->getImpl().getRepresentative(/*record=*/nullptr);

        if (!repr->getImpl().getFixedType(/*record=*/nullptr))
          notification(CG[repr], constraint);
      }
    }
  };

  while (!stack.empty()) {
    auto *typeVar = stack.pop_back_val();

    // All of the relational constraints associated with this
    // variable need to get re-introduced to other mentioned
    // type variable to update their bindings.
    //
    // If variable is a representative of an equivalence class
    // it means that all members have been modified together
    // with their representative and their adjacencies have to
    // get updated as well.
    if (CG[typeVar].forRepresentativeVar()) {
      for (auto *eqVar : CG[typeVar].getEquivalenceClass()) {
        updateAdjacencies(eqVar);

        for (auto *referrer : CG[eqVar].getReferencedBy())
          stack.push_back(referrer);
      }
    } else {
      updateAdjacencies(typeVar);

      // If current type variable is referenced by some other
      // type variable as part of its fixed type it means that
      // all of the adjacencies of that variable have to be
      // notified as well otherwise they'll miss change in type.
      for (auto *referrer : CG[typeVar].getReferencedBy())
        stack.push_back(referrer);
    }
  }
}

void ConstraintGraphNode::notifyReferencedVars(
    llvm::function_ref<void(ConstraintGraphNode &)> notification) const {
  for (auto *referencedVar : getReferencedVars()) {
    auto *repr = referencedVar->getImpl().getRepresentative(/*record=*/nullptr);
    if (!repr->getImpl().getFixedType(/*record=*/nullptr))
      notification(CG[repr]);
  }
}

void ConstraintGraphNode::addToEquivalenceClass(
       ArrayRef<TypeVariableType *> typeVars) {
  assert(forRepresentativeVar() &&
         "Can't extend equivalence class of non-representative type var");
  if (EquivalenceClass.empty())
    EquivalenceClass.push_back(getTypeVariable());
  EquivalenceClass.append(typeVars.begin(), typeVars.end());
}

void ConstraintGraphNode::truncateEquivalenceClass(unsigned prevSize) {
  EquivalenceClass.erase(EquivalenceClass.begin() + prevSize,
                         EquivalenceClass.end());
}

void ConstraintGraphNode::addReferencedVar(TypeVariableType *typeVar) {
  bool inserted = References.insert(typeVar);
  if (!inserted) {
    llvm::errs() << "$T" << TypeVar->getImpl().getID() << " already "
                 << "references $T" << typeVar->getImpl().getID() << "\n";
    abort();
  }
}

void ConstraintGraphNode::addReferencedBy(TypeVariableType *typeVar) {
  bool inserted = ReferencedBy.insert(typeVar);
  if (!inserted) {
    llvm::errs() << "$T" << TypeVar->getImpl().getID() << " already "
                 << "referenced by $T" << typeVar->getImpl().getID() << "\n";
    abort();
  }
}

void ConstraintGraphNode::removeReference(TypeVariableType *typeVar) {
  auto removed = References.remove(typeVar);
  if (!removed) {
    llvm::errs() << "$T" << TypeVar->getImpl().getID() << " does not "
                 << "reference $T" << typeVar->getImpl().getID() << "\n";
    abort();
  }
}

void ConstraintGraphNode::removeReferencedBy(TypeVariableType *typeVar) {
  auto removed = ReferencedBy.remove(typeVar);
  if (!removed) {
    llvm::errs() << "$T" << TypeVar->getImpl().getID() << " not "
                 << "referenced by $T" << typeVar->getImpl().getID() << "\n";
    abort();
  }
}

void ConstraintGraphNode::retractFromInference() {
  auto &cs = CG.getConstraintSystem();

  // Notify all of the type variables that reference this one.
  //
  // Since this type variable is going to be replaced with a fixed type
  // all of the concrete types that reference it are going to change,
  // which means that all of the not-yet-attempted bindings should
  // change as well.
  return notifyReferencingVars(
      [&cs](ConstraintGraphNode &node, Constraint *constraint) {
        node.getPotentialBindings().retract(cs, node.getTypeVariable(), constraint);
      });
}

void ConstraintGraphNode::introduceToInference(Type fixedType) {
  auto &cs = CG.getConstraintSystem();
  
  // Notify all of the type variables that reference this one.
  //
  // Since this type variable has been replaced with a fixed type
  // all of the concrete types that reference it are going to change,
  // which means that all of the not-yet-attempted bindings should
  // change as well.
  notifyReferencingVars([&cs](ConstraintGraphNode &node, Constraint *constraint) {
    node.getPotentialBindings().infer(cs, node.getTypeVariable(), constraint);
  });

  if (!fixedType->hasTypeVariable())
    return;

  SmallPtrSet<TypeVariableType *, 4> referencedVars;
  fixedType->getTypeVariables(referencedVars);

  for (auto *referencedVar : referencedVars) {
    auto *repr = referencedVar->getImpl().getRepresentative(/*record=*/nullptr);
    if (repr->getImpl().getFixedType(/*record=*/nullptr))
      continue;

    auto &node = CG[repr];

    // Newly referred vars need to re-introduce all constraints associated
    // with this type variable since they are now going to be used in
    // all of the constraints that reference bound type variable.
    for (auto *constraint : getConstraints()) {
      if (isUsefulForReferencedVars(constraint))
        node.getPotentialBindings().infer(cs, node.getTypeVariable(), constraint);
    }
  }
}

#pragma mark Graph mutation

void ConstraintGraph::removeNode(TypeVariableType *typeVar) {
  // Remove this node.
  auto &impl = typeVar->getImpl();
  auto *node = impl.getGraphNode();
  node->reset();
  FreeList.push_back(node);
  impl.setGraphNode(nullptr);
}

void ConstraintGraph::addConstraint(Constraint *constraint) {
  // For the nodes corresponding to each type variable...
  auto referencedTypeVars = constraint->getTypeVariables();
  for (auto typeVar : referencedTypeVars) {
    // Record the change, if there are active scopes.
    if (CS.solverState)
      CS.recordChange(SolverTrail::Change::AddedConstraint(typeVar, constraint));

    addConstraint(typeVar, constraint);

    auto *repr = typeVar->getImpl().getRepresentative(/*record=*/nullptr);
    if (!repr->getImpl().getFixedType(/*record=*/nullptr))
      (*this)[repr].getPotentialBindings().infer(CS, repr, constraint);

    if (isUsefulForReferencedVars(constraint)) {
      (*this)[typeVar].notifyReferencedVars([&](ConstraintGraphNode &node) {
        node.getPotentialBindings().infer(CS, node.getTypeVariable(), constraint);
      });
    }
  }

  // If the constraint doesn't reference any type variables, it's orphaned;
  // track it as such.
  if (referencedTypeVars.empty()) {
    // Record the change, if there are active scopes.
    if (CS.solverState)
      CS.recordChange(SolverTrail::Change::AddedConstraint(nullptr, constraint));

    addConstraint(nullptr, constraint);
  }
}

void ConstraintGraph::addConstraint(TypeVariableType *typeVar,
                                    Constraint *constraint) {
  if (typeVar) {
    (*this)[typeVar].addConstraint(constraint);
    return;
  }

  // If the constraint doesn't reference any type variables, it's orphaned;
  // track it as such.
  OrphanedConstraints.push_back(constraint);
}

void ConstraintGraph::removeConstraint(Constraint *constraint) {
  // For the nodes corresponding to each type variable...
  auto referencedTypeVars = constraint->getTypeVariables();
  for (auto typeVar : referencedTypeVars) {
    auto *repr = typeVar->getImpl().getRepresentative(/*record=*/nullptr);
    if (!repr->getImpl().getFixedType(/*record=*/nullptr))
      (*this)[repr].getPotentialBindings().retract(CS, repr, constraint);

    if (isUsefulForReferencedVars(constraint)) {
      (*this)[typeVar].notifyReferencedVars([&](ConstraintGraphNode &node) {
        node.getPotentialBindings().retract(CS, node.getTypeVariable(), constraint);
      });
    }

    // Record the change, if there are active scopes.
    if (CS.solverState)
      CS.recordChange(SolverTrail::Change::RemovedConstraint(typeVar, constraint));

    removeConstraint(typeVar, constraint);
  }

  // If this is an orphaned constraint, remove it from the list.
  if (referencedTypeVars.empty()) {
    // Record the change, if there are active scopes.
    if (CS.solverState)
      CS.recordChange(SolverTrail::Change::RemovedConstraint(nullptr, constraint));

    removeConstraint(nullptr, constraint);
  }
}

void ConstraintGraph::removeConstraint(TypeVariableType *typeVar,
                                       Constraint *constraint) {
  if (typeVar) {
    (*this)[typeVar].removeConstraint(constraint);
    return;
  }

  // If this is an orphaned constraint, remove it from the list.
  auto known = std::find(OrphanedConstraints.begin(),
                         OrphanedConstraints.end(),
                         constraint);
  assert(known != OrphanedConstraints.end() && "missing orphaned constraint");
  *known = OrphanedConstraints.back();
  OrphanedConstraints.pop_back();
}

void ConstraintGraph::mergeNodesPre(TypeVariableType *typeVar2) {
  // Merge equivalence class from the non-representative type variable.
  auto &nonRepNode = (*this)[typeVar2];

  for (auto *newMember : nonRepNode.getEquivalenceClassUnsafe()) {
    auto &node = (*this)[newMember];

    node.notifyReferencingVars(
      [&](ConstraintGraphNode &node, Constraint *constraint) {
        node.getPotentialBindings().retract(CS, node.getTypeVariable(), constraint);
      });
  }
}

void ConstraintGraph::mergeNodes(TypeVariableType *typeVar1, 
                                 TypeVariableType *typeVar2) {
  // Retrieve the node for the representative that we're merging into.
  ASSERT(CS.getRepresentative(typeVar1) == typeVar1);

  auto &repNode = (*this)[typeVar1];

  // Record the change, if there are active scopes.
  if (CS.solverState) {
    CS.recordChange(
      SolverTrail::Change::ExtendedEquivalenceClass(
                        typeVar1,
                        repNode.getEquivalenceClass().size()));
  }

  // Merge equivalence class from the non-representative type variable.
  auto &nonRepNode = (*this)[typeVar2];

  auto typeVars = nonRepNode.getEquivalenceClassUnsafe();
  repNode.addToEquivalenceClass(typeVars);

  for (auto *newMember : typeVars) {
    auto &node = (*this)[newMember];

    for (auto *constraint : node.getConstraints()) {
      if (!typeVar1->getImpl().getFixedType(/*record=*/nullptr))
        repNode.getPotentialBindings().infer(CS, typeVar1, constraint);

      if (!isUsefulForReferencedVars(constraint))
        continue;

      repNode.notifyReferencedVars([&](ConstraintGraphNode &node) {
        node.getPotentialBindings().infer(CS, node.getTypeVariable(), constraint);
      });
    }

    node.notifyReferencingVars(
      [&](ConstraintGraphNode &node, Constraint *constraint) {
        node.getPotentialBindings().infer(CS, node.getTypeVariable(), constraint);
      });
  }
}

void ConstraintGraph::bindTypeVariable(TypeVariableType *typeVar, Type fixed) {
  assert(!fixed->is<TypeVariableType>() &&
         "Cannot bind to type variable; merge equivalence classes instead");

  auto &node = (*this)[typeVar];

  llvm::SmallPtrSet<TypeVariableType *, 4> referencedVars;
  fixed->getTypeVariables(referencedVars);

  for (auto otherTypeVar : referencedVars) {
    if (typeVar == otherTypeVar)
      continue;

    auto &otherNode = (*this)[otherTypeVar];

    otherNode.addReferencedBy(typeVar);
    node.addReferencedVar(otherTypeVar);

    // Record the change, if there are active scopes.
    if (CS.solverState)
      CS.recordChange(SolverTrail::Change::RelatedTypeVariables(typeVar, otherTypeVar));
  }
}

void ConstraintGraph::retractFromInference(TypeVariableType *typeVar) {
  (*this)[typeVar].retractFromInference();
}

void ConstraintGraph::introduceToInference(TypeVariableType *typeVar, Type fixed) {
  (*this)[typeVar].introduceToInference(fixed);
}

void ConstraintGraph::unrelateTypeVariables(TypeVariableType *typeVar,
                                            TypeVariableType *otherTypeVar) {
  auto &node = (*this)[typeVar];
  auto &otherNode = (*this)[otherTypeVar];

  otherNode.removeReferencedBy(typeVar);
  node.removeReference(otherTypeVar);
}

void ConstraintGraph::retractBindings(TypeVariableType *typeVar,
                                      Constraint *constraint) {
  (*this)[typeVar].getPotentialBindings().retract(CS, typeVar, constraint);
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
  // If we're not looking at this type variable right now because we're
  // solving a conjunction element, don't consider its adjacencies.
  if (!cg.getConstraintSystem().isActiveTypeVariable(typeVar))
    return;

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
  visitAdjacencies(node.getReferencedBy());
  visitAdjacencies(node.getReferencedVars());
}

llvm::TinyPtrVector<Constraint *> ConstraintGraph::gatherConstraints(
    TypeVariableType *typeVar, GatheringKind kind,
    llvm::function_ref<bool(Constraint *)> acceptConstraintFn) {
  llvm::TinyPtrVector<Constraint *> constraints;
  // Whether we should consider this constraint at all.
  auto shouldConsiderConstraint = [&](Constraint *constraint) {
    // For a one-way constraint, only consider it when the left-hand side of
    // the binding is one of the type variables currently under consideration,
    // as only such constraints need solving for this component. Note that we
    // don't perform any other filtering, as the constraint system should be
    // responsible for checking any other conditions.
    if (constraint->isOneWayConstraint()) {
      auto lhsTypeVar = constraint->getFirstType()->castTo<TypeVariableType>();
      return CS.isActiveTypeVariable(lhsTypeVar);
    }

    return true;
  };

  auto acceptConstraint = [&](Constraint *constraint) {
    return shouldConsiderConstraint(constraint) &&
        acceptConstraintFn(constraint);
  };

  llvm::SmallPtrSet<TypeVariableType *, 4> typeVars;
  llvm::SmallPtrSet<Constraint *, 8> visitedConstraints;

  if (kind == GatheringKind::AllMentions) {
    // If we've been asked for "all mentions" of a type variable, search for
    // constraints involving both it and its fixed bindings.
    depthFirstSearch(
        *this, typeVar,
        [&](TypeVariableType *typeVar) {
          return typeVars.insert(typeVar).second;
        },
        [&](Constraint *constraint) {
          if (acceptConstraint(constraint))
            constraints.push_back(constraint);

          // Don't recurse into the constraint's type variables.
          return false;
        },
        visitedConstraints);
    return constraints;
  }

  // Otherwise only search in the type var's equivalence class and immediate
  // fixed bindings.

  // Local function to add constraints.
  auto addTypeVarConstraints = [&](TypeVariableType *adjTypeVar) {
    if (!typeVars.insert(adjTypeVar).second)
      return;

    for (auto constraint : (*this)[adjTypeVar].getConstraints()) {
      if (visitedConstraints.insert(constraint).second &&
          acceptConstraint(constraint))
        constraints.push_back(constraint);
    }
  };

  auto &reprNode = (*this)[CS.getRepresentative(typeVar)];
  auto equivClass = reprNode.getEquivalenceClass();
  for (auto typeVar : equivClass) {
    if (!typeVars.insert(typeVar).second)
      continue;

    auto &node = (*this)[typeVar];

    for (auto constraint : node.getConstraints()) {
      if (visitedConstraints.insert(constraint).second &&
          acceptConstraint(constraint))
        constraints.push_back(constraint);
    }

    for (auto adjTypeVar : node.getReferencedBy()) {
      addTypeVarConstraints(adjTypeVar);
    }

    for (auto adjTypeVar : node.getReferencedVars()) {
      addTypeVarConstraints(adjTypeVar);
    }
  }

  return constraints;
}

namespace {
  /// A union-find connected components algorithm used to find the connected
  /// components within a constraint graph.
  class ConnectedComponents {
    ConstraintGraph &cg;
    ArrayRef<TypeVariableType *> typeVars;

    /// The number of connected components discovered so far. Decremented when
    /// we merge equivalence classes.
    unsigned validComponentCount = 0;

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
      // The final return value.
      SmallVector<Component, 1> flatComponents;

      // We don't actually need to partition the graph into components if
      // there are fewer than 2.
      if (validComponentCount < 2 && cg.getOrphanedConstraints().empty())
        return flatComponents;

      // Mapping from representatives to components.
      llvm::SmallDenseMap<TypeVariableType *, Component> components;
      SmallVector<TypeVariableType *, 4> representativeTypeVars;

      // Capture the type variables of each component.
      for (auto typeVar : typeVars) {
        // Find the representative. If we aren't creating a type variable
        // for this component, skip it.
        auto rep = typeVar->getImpl().getComponent();
        if (!rep->getImpl().isValidComponent())
          continue;

        auto pair = components.insert({rep, Component(components.size())});
        if (pair.second)
          representativeTypeVars.push_back(rep);

        // Record this type variable in the set of type variables for its
        // component.
        pair.first->second.typeVars.push_back(typeVar);
      }

      // Retrieve the component for the given representative type variable.
      auto getComponent = [&](TypeVariableType *rep) -> Component& {
        auto component = components.find(rep);
        assert(component != components.end());
        return component->second;
      };

      auto &cs = cg.getConstraintSystem();

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

        auto rep = typeVar->getImpl().getComponent();
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
            computeOneWayComponentOrdering(representativeTypeVars);

        // Fill in one-way dependency information for all of the components.
        for (auto typeVar : representativeTypeVars) {
          auto knownOneWayComponent = oneWayDigraph.find(typeVar);
          if (knownOneWayComponent == oneWayDigraph.end())
            continue;

          auto &oneWayComponent = knownOneWayComponent->second;
          auto &component = getComponent(typeVar);
          for (auto inAdj : oneWayComponent.inAdjacencies) {
            if (!inAdj->getImpl().isValidComponent())
              continue;

            component.recordDependency(getComponent(inAdj));
          }
        }
      }

      // Flatten the set of components.
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
      // sort the orphaned constraints at the back. In the absence of
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

  private:
    /// Perform the union of two type variables in a union-find data structure
    /// used for connected components.
    ///
    /// \returns true if the two components were separate and have now been
    /// joined, \c false if they were already in the same set.
    bool unionSets(TypeVariableType *typeVar1, TypeVariableType *typeVar2) {
      auto rep1 = typeVar1->getImpl().getComponent();
      auto rep2 = typeVar2->getImpl().getComponent();
      if (rep1 == rep2)
        return false;

      // Reparent the type variable with the higher ID. The actual choice doesn't
      // matter, but this makes debugging easier.
      if (rep1->getID() > rep2->getID())
        std::swap(rep1, rep2);

      if (rep2->getImpl().isValidComponent()) {
        // If both are valid components, decrement the valid component counter
        // by one. Otherwise, propagate the valid component flag.
        if (!rep1->getImpl().markValidComponent()) {
          ASSERT(validComponentCount > 0);
          --validComponentCount;
        }
      }

      rep2->getImpl().setComponent(rep1);

      return true;
    }

    /// Perform the connected components algorithm, skipping one-way
    /// constraints.
    ///
    /// \returns the set of one-way constraints that were skipped.
    TinyPtrVector<Constraint *> connectedComponents() {
      TinyPtrVector<Constraint *> oneWayConstraints;

      auto &cs = cg.getConstraintSystem();

      for (auto typeVar : typeVars) {
        auto &impl = typeVar->getImpl();
        if (auto *rep = impl.getRepresentativeOrFixed().dyn_cast<TypeVariableType *>()) {
          impl.setComponent(rep);
          if (typeVar == rep) {
            if (impl.markValidComponent())
              ++validComponentCount;
          }
        } else {
          impl.setComponent(typeVar);
        }
      }

      for (auto typeVar : typeVars) {
        auto &impl = typeVar->getImpl();
        if (impl.getRepresentativeOrFixed().is<TypeBase *>()) {
          auto &node = cg[typeVar];
          for (auto otherTypeVar : node.getReferencedVars()) {
            unionSets(typeVar, otherTypeVar);
          }
        }
      }

      for (auto &constraint : cs.getConstraints()) {
        if (constraint.isOneWayConstraint()) {
          oneWayConstraints.push_back(&constraint);
          auto *typeVar = constraint.getFirstType()->castTo<TypeVariableType>();
          typeVar = typeVar->getImpl().getComponent();
          if (typeVar->getImpl().markValidComponent())
            ++validComponentCount;
          continue;
        }

        auto typeVars = constraint.getTypeVariables();
        if (typeVars.empty())
          continue;

        auto *firstTypeVar = typeVars[0]->getImpl().getComponent();
        if (firstTypeVar->getImpl().markValidComponent())
          ++validComponentCount;

        for (auto *otherTypeVar : typeVars.slice(1))
          unionSets(firstTypeVar, otherTypeVar);
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

      SmallPtrSet<TypeVariableType *, 2> typeVars;
      type->getTypeVariables(typeVars);
      for (auto typeVar : typeVars) {
        auto rep = typeVar->getImpl().getComponent();
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
              continue;

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
            if (cs.isDebugMode()) {
              auto &log = llvm::errs();
              if (cs.solverState)
                log.indent(cs.solverState->getCurrentIndent());

              log << "Collapsing one-way components for $T"
                  << edge.first->getID() << " and $T" << edge.second->getID()
                  << " due to cycle.\n";
            }

            if (ctx.Stats) {
              ++ctx.Stats->getFrontendCounters()
                  .NumCyclicOneWayComponentsCollapsed;
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
        ArrayRef<TypeVariableType *> representativeTypeVars) const {
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
              assert(typeVar == typeVar->getImpl().getComponent());
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
              if (typeVar->getImpl().isValidComponent())
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

void ConstraintGraph::Component::recordDependency(const Component &component) {
  dependencies.push_back(component.solutionIndex);
}

SmallVector<ConstraintGraph::Component, 1>
ConstraintGraph::computeConnectedComponents(
           ArrayRef<TypeVariableType *> typeVars) {
  // Perform connected components via a union-find algorithm on all of the
  // constraints adjacent to these type variables.
  ConnectedComponents cc(*this, typeVars);
  return cc.getComponents();
}

bool ConstraintGraph::contractEdges() {
  // Current constraint system doesn't have any closure expressions
  // associated with it so there is nothing to here.
  if (CS.ClosureTypes.empty())
    return false;

  // For a given constraint kind, decide if we should attempt to eliminate its
  // edge in the graph.
  auto shouldContractEdge = [](ConstraintKind kind) {
    switch (kind) {
    case ConstraintKind::BindParam:
      return true;

    default:
      return false;
    }
  };

  SmallVector<Constraint *, 16> constraints;
  for (const auto &closure : CS.ClosureTypes) {
    for (const auto &param : closure.second->getParams()) {
      auto paramTy = param.getPlainType()->getAs<TypeVariableType>();
      if (!paramTy)
        continue;

      // This closure is not currently in scope.
      if (!CS.isActiveTypeVariable(paramTy))
        break;

      // Nothing to contract here since outside parameter
      // is already bound to a concrete type.
      if (CS.getFixedType(paramTy))
        continue;

      for (auto *constraint : (*this)[paramTy].getConstraints()) {
        // Track how many constraints did contraction algorithm iterated over.
        incrementConstraintsPerContractionCounter();

        if (shouldContractEdge(constraint->getKind()))
          constraints.push_back(constraint);
      }
    }
  }

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

    // If the argument is allowed to bind to `inout`, in general,
    // it's invalid to contract the edge between argument and parameter,
    // but if we can prove that there are no possible bindings
    // which result in attempt to bind `inout` type to argument
    // type variable, we should go ahead and allow (temporary)
    // contraction, because that greatly helps with performance.
    // Such action is valid because argument type variable can
    // only get its bindings from related overload, which gives
    // us enough information to decided on l-valueness.
    if (tyvar1->getImpl().canBindToInOut()) {
      bool isNotContractable = true;
      if (auto bindings = CS.getBindingsFor(tyvar1)) {
        // Holes can't be contracted.
        if (bindings.isHole())
          continue;

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

    if (CS.isDebugMode()) {
      auto indent = CS.solverState ? CS.solverState->getCurrentIndent() : 0;
      auto &log = llvm::errs().indent(indent);

      log << "Contracting constraint ";
      constraint->print(log.indent(indent), &CS.getASTContext().SourceMgr,
                        indent);
      log << "\n";
    }

    // Merge the edges and retire the constraint.
    CS.retireConstraint(constraint);
    if (rep1 != rep2)
      CS.mergeEquivalenceClasses(rep1, rep2, /*updateWorkList*/ false);
    didContractEdges = true;
  }
  return didContractEdges;
}

void ConstraintGraph::optimize() {
  // Merge equivalence classes until a fixed point is reached.
  while (contractEdges()) {}
}

void ConstraintGraph::incrementConstraintsPerContractionCounter() {
  SWIFT_FUNC_STAT;
  auto &context = CS.getASTContext();
  if (auto *Stats = context.Stats) {
    ++Stats->getFrontendCounters()
        .NumConstraintsConsideredForEdgeContraction;
  }
}

#pragma mark Debugging output

void ConstraintGraphNode::print(llvm::raw_ostream &out, unsigned indent,
                                PrintOptions PO) const {
  out.indent(indent);
  Type(TypeVar).print(out, PO);
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
      constraint->print(out, &TypeVar->getASTContext().SourceMgr, indent + 4);
      out << "\n";
    }
  }

  auto printVarList = [&](ArrayRef<TypeVariableType *> typeVars) {
    SmallVector<TypeVariableType *, 4> sorted(typeVars.begin(), typeVars.end());
    std::sort(sorted.begin(), sorted.end(),
              [&](TypeVariableType *typeVar1, TypeVariableType *typeVar2) {
                return typeVar1->getID() < typeVar2->getID();
              });

    interleave(
        sorted,
        [&](TypeVariableType *typeVar) { out << typeVar->getString(PO); },
        [&out] { out << ", "; });
  };

  // Print fixed bindings.
  if (!ReferencedBy.empty()) {
    out.indent(indent + 2);
    out << "Referenced By: ";
    printVarList(getReferencedBy());
    out << "\n";
  }

  if (!References.empty()) {
    out.indent(indent + 2);
    out << "References: ";
    printVarList(getReferencedVars());
    out << "\n";
  }

  // Print equivalence class.
  if (forRepresentativeVar() && EquivalenceClass.size() > 1) {
    out.indent(indent + 2);
    out << "Equivalence class:";
    for (unsigned i = 1, n = EquivalenceClass.size(); i != n; ++i) {
      out << ' ';
      EquivalenceClass[i]->print(out, PO);
    }
    out << "\n";
  }
}

void ConstraintGraphNode::dump() const {
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;
  print(llvm::dbgs(), 0, PO);
}

void ConstraintGraph::print(ArrayRef<TypeVariableType *> typeVars,
                            llvm::raw_ostream &out) {
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;

  for (auto typeVar : typeVars) {
    (*this)[typeVar].print(
        out, (CS.solverState ? CS.solverState->getCurrentIndent() : 0) + 2, PO);
    out << "\n";
  }
}

void ConstraintGraph::dump() {
  dump(llvm::dbgs());
}

void ConstraintGraph::dump(llvm::raw_ostream &out) {
  print(CS.getTypeVariables(), out);
}

void ConstraintGraph::printConnectedComponents(
    ArrayRef<TypeVariableType *> typeVars,
    llvm::raw_ostream &out) {
  auto components = computeConnectedComponents(typeVars);
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;
  for (const auto& component : components) {
    out.indent((CS.solverState ? CS.solverState->getCurrentIndent() : 0) + 2);
    out << component.solutionIndex << ": ";
    SWIFT_DEFER {
      out << '\n';
    };

    // Print all of the type variables in this connected component.
    interleave(component.typeVars,
               [&](TypeVariableType *typeVar) {
                 Type(typeVar).print(out, PO);
               },
               [&] {
                 out << ' ';
               });

    auto dependencies = component.getDependencies();
    if (dependencies.empty())
      continue;

    SmallVector<unsigned, 4> indices{dependencies.begin(), dependencies.end()};
    // Sort dependencies so output is stable.
    llvm::sort(indices);

    // Print all of the one-way components.
    out << " depends on ";
    llvm::interleave(
        indices, [&out](unsigned index) { out << index; },
        [&out] { out << ", "; });
  }
}

void ConstraintGraph::dumpConnectedComponents() {
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
  for (auto typeVar : CS.TypeVariables) {
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

  // Verify consistency of all of the nodes in the graph.
  for (auto typeVar : CS.TypeVariables) {
    auto &impl = typeVar->getImpl();
    impl.getGraphNode()->verify(*this);
  }

  // Collect all of the constraints known to the constraint graph.
  llvm::SmallPtrSet<Constraint *, 4> knownConstraints;
  for (auto typeVar : CS.TypeVariables) {
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


