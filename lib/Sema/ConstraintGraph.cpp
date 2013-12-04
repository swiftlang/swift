//===--- ConstraintGraph.cpp - Constraint Graph ---------------------------===//
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
// This file implements the \c ConstraintGraph class, which describes the
// relationships among the type variables within a constraint system.
//
//===----------------------------------------------------------------------===//
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "swift/Basic/Fallthrough.h"
#include "llvm/Support/Debug.h"
#include <algorithm>
#include <memory>
#include <numeric>

using namespace swift;
using namespace constraints;

#pragma mark Graph construction/destruction

ConstraintGraph::ConstraintGraph(ConstraintSystem &cs) : CS(cs) { }

ConstraintGraph::~ConstraintGraph() {
  for (auto node : Nodes) {
    delete node.second.NodePtr;
  }
}

#pragma mark Helper functions

/// Recursively gather the set of type variable representatives referenced by
/// this constraint.
static void
gatherReferencedTypeVarsRec(ConstraintSystem &cs,
                            Constraint *constraint,
                            SmallVectorImpl<TypeVariableType *> &typeVars) {
  switch (constraint->getKind()) {
  case ConstraintKind::Conjunction:
  case ConstraintKind::Disjunction:
    for (auto nested : constraint->getNestedConstraints())
      gatherReferencedTypeVarsRec(cs, nested, typeVars);
    return;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::Bind:
  case ConstraintKind::Construction:
  case ConstraintKind::Conversion:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::Equal:
  case ConstraintKind::Subtype:
  case ConstraintKind::TrivialSubtype:
  case ConstraintKind::TypeMember:
  case ConstraintKind::ValueMember: {
    Type second = cs.simplifyType(constraint->getSecondType());
    second->getTypeVariables(typeVars);
  }
  SWIFT_FALLTHROUGH;

  case ConstraintKind::Archetype:
  case ConstraintKind::BindOverload:
  case ConstraintKind::Class:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::DynamicLookupValue:
  case ConstraintKind::SelfObjectOfProtocol: {
    Type first = cs.simplifyType(constraint->getFirstType());
    first->getTypeVariables(typeVars);

    // Special case: the base type of an overloading binding.
    if (constraint->getKind() == ConstraintKind::BindOverload) {
      if (auto baseType = constraint->getOverloadChoice().getBaseType()) {
        baseType = cs.simplifyType(baseType);
        baseType->getTypeVariables(typeVars);
      }
    }

    break;
  }
  }
}

/// Gather the set of type variable representatives referenced by this
/// constraint, mapped to the type representative and uniqued.
static void
gatherReferencedTypeVars(ConstraintSystem &cs,
                         Constraint *constraint,
                         SmallVectorImpl<TypeVariableType *> &typeVars) {
  // Gather all of the referenced type variables.
  gatherReferencedTypeVarsRec(cs, constraint, typeVars);

  // Map the referenced type variables to their representatives and remove
  // any duplicates.
  // Note: This is a standard erase/remove idiom, but we don't use the
  // standard library's algorithms because we also want to map type
  // variables to their representatives.
  llvm::SmallPtrSet<TypeVariableType *, 4> representatives;
  unsigned currentIndex = 0;
  for (unsigned i = 0, n = typeVars.size(); i != n; ++i) {
    auto typeVar = cs.getRepresentative(typeVars[i]);
    if (!representatives.insert(typeVar))
      continue;

    typeVars[currentIndex++] = typeVar;
  }
  typeVars.erase(typeVars.begin() + currentIndex, typeVars.end());
}

#pragma mark Graph accessors

ConstraintGraph::Node &ConstraintGraph::operator[](TypeVariableType *typeVar) {
  return lookupNode(typeVar).first;
}

std::pair<ConstraintGraph::Node &, unsigned>
ConstraintGraph::lookupNode(TypeVariableType *typeVar) {
  typeVar = CS.getRepresentative(typeVar);

  // Check whether we've already created a node for this type variable.
  auto known = Nodes.find(typeVar);
  if (known != Nodes.end()) {
    assert(known->second.NodePtr && "Missing node pointer?");
    return { *known->second.NodePtr, known->second.Index };
  }

  // Allocate the new node.
  StoredNode &stored = Nodes[typeVar];
  stored.NodePtr = new Node(typeVar);
  stored.Index = TypeVariables.size();

  // Record this type variable.
  TypeVariables.push_back(typeVar);

  return { *stored.NodePtr, stored.Index };
}

#pragma mark Node mutation
void ConstraintGraph::Node::addConstraint(Constraint *constraint) {
  assert(ConstraintIndex.count(constraint) == 0 && "Constraint re-insertion");
  ConstraintIndex[constraint] = Constraints.size();
  Constraints.push_back(constraint);
}

void ConstraintGraph::Node::removeConstraint(Constraint *constraint) {
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

void ConstraintGraph::Node::addAdjacency(TypeVariableType *typeVar, 
                                         unsigned degree) {
  assert(typeVar != TypeVar && "Cannot be adjacent to oneself");

  // Look for existing adjacency information.
  auto pos = AdjacencyInfo.find(typeVar);

  // If we weren't already adjacent to this type variable, add it to the
  // list of adjacencies.
  if (pos == AdjacencyInfo.end()) {
    pos = AdjacencyInfo.insert(
            { typeVar, { static_cast<unsigned>(Adjacencies.size()), 0 } })
            .first;
    Adjacencies.push_back(typeVar);
  }

  // Bump the degree of the adjacency.
  pos->second.NumConstraints += degree;
}

void ConstraintGraph::Node::removeAdjacency(TypeVariableType *typeVar,
                                            bool allAdjacencies) {
  // Find the adjacency information.
  auto pos = AdjacencyInfo.find(typeVar);
  assert(pos != AdjacencyInfo.end() && "Type variables not adjacent");
  assert(Adjacencies[pos->second.Index] == typeVar && "Mismatched adjacency");

  if (!allAdjacencies) {
    // Decrement the number of constraints that make these two type variables
    // adjacent.
    --pos->second.NumConstraints;
    
    // If there are other constraints that make these type variables
    // adjacent,
    if (pos->second.NumConstraints > 0)
      return;
  }

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

void ConstraintGraph::Node::collapseInto(ConstraintGraph &cg, Node &combined) {
  // Add each of the constraints into the combined node.
  for (auto constraint : Constraints) {
    if (combined.ConstraintIndex.count(constraint) == 0)
      combined.addConstraint(constraint);
  }

  // Update adjacency counts in the combined node and remap the
  // adjacent nodes.
  for (const auto &adj : AdjacencyInfo) {
    // If this is a newly-created self-adjacency, remove it from the
    // combined node and we're done.
    if (adj.first == combined.TypeVar) {
      combined.removeAdjacency(TypeVar, /*allAdjacencies=*/true);
      continue;
    }

    // Add this adjacency to the combined node.
    combined.addAdjacency(adj.first, adj.second.NumConstraints);

    // Replace the adjacency in the adjacent node with the combined
    // node.
    auto &adjNode = cg[adj.first];
    adjNode.removeAdjacency(TypeVar, /*allAdjacencies=*/true);
    adjNode.addAdjacency(combined.TypeVar, adj.second.NumConstraints);
  }
}

#pragma mark Graph mutation

void ConstraintGraph::addConstraint(Constraint *constraint) {
  // Gather the set of type variables referenced by this constraint.
  SmallVector<TypeVariableType *, 8> referencedTypeVars;
  gatherReferencedTypeVars(CS, constraint, referencedTypeVars);

  // For the nodes corresponding to each type variable...
  for (auto typeVar : referencedTypeVars) {
    // Find the node for this type variable.
    Node &node = (*this)[typeVar];

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
}

void ConstraintGraph::removeConstraint(Constraint *constraint) {
  // Gather the set of type variables referenced by this constraint.
  SmallVector<TypeVariableType *, 8> referencedTypeVars;
  gatherReferencedTypeVars(CS, constraint, referencedTypeVars);

  // For the nodes corresponding to each type variable...
  for (auto typeVar : referencedTypeVars) {
    // Find the node for this type variable.
    Node &node = (*this)[typeVar];

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
}

void ConstraintGraph::mergeNodes(TypeVariableType *typeVar1, 
                                 TypeVariableType *typeVar2) {
  assert(typeVar1 == CS.getRepresentative(typeVar1) && "non-representative 1");
  assert(typeVar2 == CS.getRepresentative(typeVar2) && "non-representative 2");
  assert(typeVar1 != typeVar2 && "Type variables aren't different");

  // Pull the first node out of storage.
  std::unique_ptr<Node> node1;
  {
    auto known1 = Nodes.find(typeVar1);
    assert(known1 != Nodes.end() && "Missing first node");
    auto storedNode1 = known1->second;
    Nodes.erase(known1);

    unsigned lastIndex = TypeVariables.size()-1;
    if (storedNode1.Index < lastIndex) {
      // Shuffle the last type variable to the index of the first
      // node's type variable.
      auto lastTypeVar = TypeVariables[lastIndex];
      TypeVariables[storedNode1.Index] = lastTypeVar;
      Nodes[lastTypeVar].Index = storedNode1.Index;
    } 

    // Pop the removed variable off the end.
    TypeVariables.pop_back();

    // Save the node pointer.
    node1.reset(storedNode1.NodePtr);
  }
  
  // Collapse the first type variable's node into the second.
  auto &node2 = (*this)[typeVar2];
  node1->collapseInto(*this, node2);
}

#pragma mark Algorithms

/// Depth-first search for connected components
static void connectedComponentsDFS(ConstraintGraph &cg,
                                   ConstraintGraph::Node &node,
                                   unsigned component,
                                   SmallVectorImpl<unsigned> &components) {
  // Recurse to mark adjacent nodes as part of this connected component.
  for (auto adj : node.getAdjacencies()) {
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
}

unsigned ConstraintGraph::computeConnectedComponents(
           SmallVectorImpl<unsigned> &components,
           SmallVectorImpl<unsigned> *componentSizes) {
  // Initialize the components with component == # of type variables,
  // a sentinel value indicating
  unsigned numTypeVariables = TypeVariables.size();
  components.assign(numTypeVariables, numTypeVariables);
  if (componentSizes)
    componentSizes->clear();

  // Perform a depth-first search from each type variable to identify
  // what component it is in.
  unsigned numComponents = 0;
  for (unsigned i = 0; i != numTypeVariables; ++i) {
    // Look up the node for this type variable.
    auto typeVar = TypeVariables[i];
    auto nodeAndIndex = lookupNode(typeVar);

    // If we're already assigned a component for this node, skip it.
    unsigned &curComponent = components[nodeAndIndex.second];
    if (curComponent != numTypeVariables) {
      if (componentSizes)
        ++(*componentSizes)[curComponent];
      continue;
    }

    // Record this component.
    unsigned component = numComponents++;
    if (componentSizes)
      componentSizes->push_back(1);

    // Note that this node is part of this component, then visit it.
    curComponent = component;
    connectedComponentsDFS(*this, nodeAndIndex.first, component, components);
  }

  // If we computed component sizes, make sure we did something sane.
  assert(!componentSizes ||
         (std::accumulate(componentSizes->begin(), componentSizes->end(),
                          0u) == numTypeVariables));
  return numComponents;
}

#pragma mark Debugging output

void ConstraintGraph::Node::print(llvm::raw_ostream &out, unsigned indent) {
  out.indent(indent);
  TypeVar->print(out);
  out << ":\n";

  // Print constraints.
  if (!Constraints.empty()) {
    out.indent(indent + 2);
    out << "Constraints:\n";
    for (auto constraint : Constraints) {
      out.indent(indent + 4);
      constraint->print(out, /*FIXME:*/nullptr);
      out << "\n";
    }
  }

  // Print adjacencies.
  if (!Adjacencies.empty()) {
    out.indent(indent + 2);
    out << "Adjacencies:";
    for (auto adj : Adjacencies) {
      out << ' ';
      adj->print(out);

      auto degree = AdjacencyInfo[adj].NumConstraints;
      if (degree > 1)
        out << " (" << degree << ")";
    }
    out << "\n";
  }
}

void ConstraintGraph::Node::dump() {
  print(llvm::dbgs(), 0);
}

void ConstraintGraph::print(llvm::raw_ostream &out) {
  for (auto typeVar : TypeVariables) {
    (*this)[typeVar].print(out, 2);
    out << "\n";
  }
}

void ConstraintGraph::dump() {
  print(llvm::dbgs());
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
                     ConstraintGraph::Node *node,
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

void ConstraintGraph::Node::verify(ConstraintGraph &cg) {
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
    require(info.second.NumConstraints > 0,
            "adjacency information should have been removed");
    require(info.second.NumConstraints <= Constraints.size(),
            "adjacency information has higher degree than # of constraints");
    requireSameValue(info.first, cg.CS.getRepresentative(info.first),
                     "adjacency with non-representative type");
  }

  // Based on the constraints we have, build up a representation of what
  // we expect the adjacencies to look like.
  llvm::DenseMap<TypeVariableType *, unsigned> expectedAdjacencies;
  for (auto constraint : Constraints) {
    SmallVector<TypeVariableType *, 4> referencedTypeVars;
    gatherReferencedTypeVars(cg.CS, constraint, referencedTypeVars);

    for (auto adjTypeVar : referencedTypeVars) {
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
    llvm::dbgs() << "  " << value1 << " != " << value2 << '\n'; \
  })

  // Verify that the type variables are all representatives.
  for (auto typeVar : TypeVariables) {
    requireSameValue(typeVar, CS.getRepresentative(typeVar),
                     "non-representative type variable in constraint graph");
  }

  // Verify that our type variable map/vector are in sync.
  requireSameValue(TypeVariables.size(), Nodes.size(),
                   "type variables vector and node map have different sizes");
  for (auto node : Nodes) {
    require(node.second.Index < TypeVariables.size(),
            "out of bounds node index");
    requireSameValue(node.first, TypeVariables[node.second.Index],
                     "node map provides wrong index into type variable vector");
  }

  // Verify consistency of all of the nodes in the graph.
  for (auto node : Nodes) {
    node.second.NodePtr->verify(*this);
  }

  // FIXME: Verify that all of the constraints in the constraint system
  // are accounted for. This requires a better abstraction for tracking
  // the set of constraints that are live.

#undef requireSameValue
#undef requireWithContext
#undef require
}


