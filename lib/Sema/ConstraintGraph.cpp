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
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <memory>
#include <numeric>

using namespace swift;
using namespace constraints;

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
          { typeVar, { static_cast<unsigned>(Adjacencies.size()), 0, 0 } })
         .first;
  Adjacencies.push_back(typeVar);
  return pos->second;
}

void ConstraintGraphNode::modifyAdjacency(
       TypeVariableType *typeVar,
       std::function<void(Adjacency& adj)> modify) {
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
  auto &adjacency = getAdjacency(typeVar);

  assert(!adjacency.FixedBinding && "Already marked as a fixed binding?");
  adjacency.FixedBinding = true;
}

void ConstraintGraphNode::removeFixedBinding(TypeVariableType *typeVar) {
  modifyAdjacency(typeVar, [](Adjacency &adj) {
    assert(adj.FixedBinding && "Not a fixed binding?");
    adj.FixedBinding = false;
  });
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

void ConstraintGraph::addConstraint(Constraint *constraint) {
  // For the nodes corresponding to each type variable...
  auto referencedTypeVars = constraint->getTypeVariables();
  for (auto typeVar : referencedTypeVars) {
    // Find the node for this type variable.
    auto &node = (*this)[typeVar];

    // Note the constraint within the node for that type variable.
    node.addConstraint(constraint);

    // Record the adjacent type variables.
    // This is O(N^2) in the number of referenced type variables, because
    // we're updating all of the adjacent type variables eagerly.
    for (auto otherTypeVar : referencedTypeVars) {
      if (typeVar == otherTypeVar)
        continue;

      node.addAdjacency(otherTypeVar);
    }
  }

  // If the constraint doesn't reference any type variables, it's orphaned;
  // track it as such.
  if (referencedTypeVars.empty()) {
    OrphanedConstraints.push_back(constraint);
  }

  // Record the change, if there are active scopes.
  if (ActiveScope)
    Changes.push_back(Change::addedConstraint(constraint));
}

void ConstraintGraph::removeConstraint(Constraint *constraint) {
  // For the nodes corresponding to each type variable...
  auto referencedTypeVars = constraint->getTypeVariables();
  for (auto typeVar : referencedTypeVars) {
    // Find the node for this type variable.
    auto &node = (*this)[typeVar];

    // Remove the constraint.
    node.removeConstraint(constraint);

    // Remove the adjacencies for all adjacent type variables.
    // This is O(N^2) in the number of referenced type variables, because
    // we're updating all of the adjacent type variables eagerly.
    for (auto otherTypeVar : referencedTypeVars) {
      if (typeVar == otherTypeVar)
        continue;

      node.removeAdjacency(otherTypeVar);
    }
  }

  // If this is an orphaned constraint, remove it from the list.
  if (referencedTypeVars.empty()) {
    auto known = std::find(OrphanedConstraints.begin(),
                           OrphanedConstraints.end(),
                           constraint);
    assert(known != OrphanedConstraints.end() && "missing orphaned constraint");
    *known = OrphanedConstraints.back();
    OrphanedConstraints.pop_back();
  }

  // Record the change, if there are active scopes.
  if (ActiveScope)
    Changes.push_back(Change::removedConstraint(constraint));
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

void ConstraintGraph::gatherConstraints(
       TypeVariableType *typeVar,
       SmallVectorImpl<Constraint *> &constraints,
       GatheringKind kind) {
  auto &node = (*this)[CS.getRepresentative(typeVar)];
  auto equivClass = node.getEquivalenceClass();
  llvm::SmallPtrSet<TypeVariableType *, 4> typeVars;
  for (auto typeVar : equivClass) {
    if (!typeVars.insert(typeVar).second)
      continue;

    for (auto constraint : (*this)[typeVar].getConstraints())
      constraints.push_back(constraint);
  }

  // Retrieve the constraints from adjacent bindings.
  for (auto adjTypeVar : node.getAdjacencies()) {
    switch (kind) {
    case GatheringKind::EquivalenceClass:
      if (!node.getAdjacency(adjTypeVar).FixedBinding)
        continue;
      break;

    case GatheringKind::AllMentions:
      break;
    }

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

      for (auto constraint : (*this)[adjTypeVarEquiv].getConstraints())
        constraints.push_back(constraint);
    }
  }
}

#pragma mark Algorithms

/// Depth-first search for connected components
static void connectedComponentsDFS(ConstraintGraph &cg,
                                   ConstraintGraphNode &node,
                                   unsigned component,
                                   SmallVectorImpl<unsigned> &components) {
  // Local function that recurses on the given set of type variables.
  auto visitAdjacencies = [&](ArrayRef<TypeVariableType *> typeVars) {
    for (auto adj : typeVars) {
      auto nodeAndIndex = cg.lookupNode(adj);
      // If we've already seen this node in this component, we're done.
      unsigned &curComponent = components[nodeAndIndex.second];
      if (curComponent == component)
        continue;

      // Mark this node as part of this connected component, then recurse.
      assert(curComponent == components.size() && "Already in a component?");
      curComponent = component;
      connectedComponentsDFS(cg, nodeAndIndex.first, component, components);
    }
  };

  // Recurse to mark adjacent nodes as part of this connected component.
  visitAdjacencies(node.getAdjacencies());

  // Figure out the representative for this type variable.
  auto &cs = cg.getConstraintSystem();
  auto typeVarRep = cs.getRepresentative(node.getTypeVariable());
  if (typeVarRep == node.getTypeVariable()) {
    // This type variable is the representative of its set; visit all of the
    // other type variables in the same equivalence class.
    visitAdjacencies(node.getEquivalenceClass().slice(1));
  } else {
    // Otherwise, visit the representative of the set.
    visitAdjacencies(typeVarRep);
  }
}

unsigned ConstraintGraph::computeConnectedComponents(
           SmallVectorImpl<TypeVariableType *> &typeVars,
           SmallVectorImpl<unsigned> &components) {
  // Track those type variables that the caller cares about.
  llvm::SmallPtrSet<TypeVariableType *, 4> typeVarSubset(typeVars.begin(),
                                                         typeVars.end());
  typeVars.clear();

  // Initialize the components with component == # of type variables,
  // a sentinel value indicating
  unsigned numTypeVariables = TypeVariables.size();
  components.assign(numTypeVariables, numTypeVariables);

  // Perform a depth-first search from each type variable to identify
  // what component it is in.
  unsigned numComponents = 0;
  for (unsigned i = 0; i != numTypeVariables; ++i) {
    auto typeVar = TypeVariables[i];

    // Look up the node for this type variable.
    auto nodeAndIndex = lookupNode(typeVar);

    // If we're already assigned a component for this node, skip it.
    unsigned &curComponent = components[nodeAndIndex.second];
    if (curComponent != numTypeVariables)
      continue;

    // Record this component.
    unsigned component = numComponents++;

    // Note that this node is part of this component, then visit it.
    curComponent = component;
    connectedComponentsDFS(*this, nodeAndIndex.first, component, components);
  }

  // Figure out which components have unbound type variables; these
  // are the only components and type variables we want to report.
  SmallVector<bool, 4> componentHasUnboundTypeVar(numComponents, false);
  for (unsigned i = 0; i != numTypeVariables; ++i) {
    // If this type variable has a fixed type, skip it.
    if (CS.getFixedType(TypeVariables[i]))
      continue;

    // If this type variable isn't in the subset of type variables we care
    // about, skip it.
    if (typeVarSubset.count(TypeVariables[i]) == 0)
      continue;

    componentHasUnboundTypeVar[components[i]] = true;
  }

  // Renumber the old components to the new components.
  SmallVector<unsigned, 4> componentRenumbering(numComponents, 0);
  numComponents = 0;
  for (unsigned i = 0, n = componentHasUnboundTypeVar.size(); i != n; ++i) {
    // Skip components that have no unbound type variables.
    if (!componentHasUnboundTypeVar[i])
      continue;

    componentRenumbering[i] = numComponents++;
  }

  // Copy over the type variables in the live components and remap
  // component numbers.
  unsigned outIndex = 0;
  for (unsigned i = 0, n = TypeVariables.size(); i != n; ++i) {
    // Skip type variables in dead components.
    if (!componentHasUnboundTypeVar[components[i]])
      continue;

    typeVars.push_back(TypeVariables[i]);
    components[outIndex] = componentRenumbering[components[i]];
    ++outIndex;
  }
  components.erase(components.begin() + outIndex, components.end());

  return numComponents + getOrphanedConstraints().size();
}


/// For a given constraint kind, decide if we should attempt to eliminate its
/// edge in the graph.
static bool shouldContractEdge(ConstraintKind kind) {
  switch (kind) {
  case ConstraintKind::Bind:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Equal:
  case ConstraintKind::BindOverload:

  // We currently only allow subtype contractions for the purpose of 
  // parameter binding constraints.
  // TODO: We do this because of how inout parameter bindings are handled
  // for implicit closure parameters. We should consider adjusting our
  // current approach to unlock more opportunities for subtype contractions.
  case ConstraintKind::Subtype:
    return true;

  default:
    return false;
  }
}

/// We use this function to determine if a subtype constraint is set
/// between two (possibly sugared) type variables, one of which is wrapped
/// in an inout type.
static bool isStrictInoutSubtypeConstraint(Constraint *constraint) {
  if (constraint->getKind() != ConstraintKind::Subtype)
    return false;

  auto t1 = constraint->getFirstType()->getDesugaredType();

  if (auto tt = t1->getAs<TupleType>()) {
    if (tt->getNumElements() != 1)
      return false;

    t1 = tt->getElementType(0).getPointer();
  }

  auto iot = t1->getAs<InOutType>();

  if (!iot)
    return false;

  return !iot->getObjectType()->isTypeVariableOrMember();
}

bool ConstraintGraph::contractEdges() {
  llvm::SetVector<std::pair<TypeVariableType *,
                            TypeVariableType *>> contractions;

  auto tyvars = getTypeVariables();
  auto didContractEdges = false;

  for (auto tyvar : tyvars) {
    SmallVector<Constraint *, 4> constraints;
    gatherConstraints(tyvar, constraints,
                      ConstraintGraph::GatheringKind::EquivalenceClass);

    for (auto constraint : constraints) {
      auto kind = constraint->getKind();
      // Contract binding edges between type variables.
      if (shouldContractEdge(kind)) {
        auto t1 = constraint->getFirstType()->getDesugaredType();
        auto t2 = constraint->getSecondType()->getDesugaredType();

        if (kind == ConstraintKind::Subtype) {
          if (auto iot1 = t1->getAs<InOutType>()) {
            t1 = iot1->getObjectType().getPointer();
          } else {
            continue;
          }
        }

        auto tyvar1 = t1->getAs<TypeVariableType>();
        auto tyvar2 = t2->getAs<TypeVariableType>();

        if (!(tyvar1 && tyvar2))
          continue;

        auto isParamBindingConstraint = kind == ConstraintKind::BindParam;

        // We need to take special care not to directly contract parameter
        // binding constraints if there is an inout subtype constraint on the
        // type variable. The constraint solver depends on multiple constraints
        // being present in this case, so it can generate the appropriate lvalue
        // wrapper for the argument type.
        if (isParamBindingConstraint) {
          auto node = tyvar1->getImpl().getGraphNode();
          auto hasDependentConstraint = false;

          for (auto t1Constraint : node->getConstraints()) {
            if (isStrictInoutSubtypeConstraint(t1Constraint)) {
              hasDependentConstraint = true;
              break;
            }
          }

          if (hasDependentConstraint)
            continue;
        }

        auto rep1 = CS.getRepresentative(tyvar1);
        auto rep2 = CS.getRepresentative(tyvar2);

        if (((rep1->getImpl().canBindToLValue() ==
              rep2->getImpl().canBindToLValue()) ||
              // Allow l-value contractions when binding parameter types.
              isParamBindingConstraint)) {
          if (CS.TC.getLangOpts().DebugConstraintSolver) {
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
    }
  }

  return didContractEdges;
}

void ConstraintGraph::removeEdge(Constraint *constraint) {

  for (auto &active : CS.ActiveConstraints) {
    if (&active == constraint) {
      CS.ActiveConstraints.erase(constraint);
      break;
    }
  }

  for (auto &inactive : CS.InactiveConstraints) {
    if (&inactive == constraint) {
      CS.InactiveConstraints.erase(constraint);
      break;
    }
  }

  if (CS.solverState)
    CS.solverState->removeGeneratedConstraint(constraint);

  removeConstraint(constraint);
}

void ConstraintGraph::optimize() {
  // Merge equivalence classes until a fixed point is reached.
  while (contractEdges()) {}
}

#pragma mark Debugging output

void ConstraintGraphNode::print(llvm::raw_ostream &out, unsigned indent) {
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

  // Print adjacencies.
  if (!Adjacencies.empty()) {
    out.indent(indent + 2);
    out << "Adjacencies:";
    SmallVector<TypeVariableType *, 4> sortedAdjacencies(Adjacencies.begin(),
                                                         Adjacencies.end());
    std::sort(sortedAdjacencies.begin(), sortedAdjacencies.end(),
              [&](TypeVariableType *typeVar1, TypeVariableType *typeVar2) {
                return typeVar1->getID() < typeVar2->getID();
              });

    for (auto adj : sortedAdjacencies) {
      out << ' ';
      adj->print(out);

      auto &info = AdjacencyInfo[adj];
      auto degree = info.NumConstraints;
      if (degree > 1 || info.FixedBinding) {
        out << " (";
        if (degree > 1) {
          out << degree;
          if (info.FixedBinding)
            out << ", fixed";
        } else {
          out << "fixed";
        }
        out << ")";
      }
    }
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

void ConstraintGraphNode::dump() {
  llvm::SaveAndRestore<bool>
    debug(TypeVar->getASTContext().LangOpts.DebugConstraintSolver, true);
  print(llvm::dbgs(), 0);
}

void ConstraintGraph::print(llvm::raw_ostream &out) {
  for (auto typeVar : TypeVariables) {
    (*this)[typeVar].print(out, 2);
    out << "\n";
  }
}

void ConstraintGraph::dump() {
  llvm::SaveAndRestore<bool>
    debug(CS.getASTContext().LangOpts.DebugConstraintSolver, true);
  print(llvm::dbgs());
}

void ConstraintGraph::printConnectedComponents(llvm::raw_ostream &out) {
  SmallVector<TypeVariableType *, 16> typeVars;
  typeVars.append(TypeVariables.begin(), TypeVariables.end());
  SmallVector<unsigned, 16> components;
  unsigned numComponents = computeConnectedComponents(typeVars, components);
  for (unsigned component = 0; component != numComponents; ++component) {
    out.indent(2);
    out << component << ":";
    for (unsigned i = 0, n = typeVars.size(); i != n; ++i) {
      if (components[i] == component) {
        out << ' ';
        typeVars[i]->print(out);
      }
    }
    out << '\n';
  }
}

void ConstraintGraph::dumpConnectedComponents() {
  printConnectedComponents(llvm::dbgs());
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
  // FIXME: Highlight the offending node/constraint/adjacency/etc.
  cg.print(llvm::dbgs());

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


