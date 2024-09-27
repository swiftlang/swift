//===--- CSTrail.cpp -  Tracking changes that can be undone ---------------===//
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

#define DEBUG_TYPE "SolverTrail"

SolverTrail::~SolverTrail() {
  // If constraint system is in an invalid state, it's
  // possible that constraint graph is corrupted as well
  // so let's not attempt to check change log.
  if (!CS.inInvalidState())
    ASSERT(Changes.empty() && "Trail corrupted");
}

SolverTrail::Change
SolverTrail::Change::addedTypeVariable(TypeVariableType *typeVar) {
  Change result;
  result.Kind = ChangeKind::AddedTypeVariable;
  result.TypeVar = typeVar;
  return result;
}

SolverTrail::Change
SolverTrail::Change::addedConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::AddedConstraint;
  result.TheConstraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::removedConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::RemovedConstraint;
  result.TheConstraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::extendedEquivalenceClass(TypeVariableType *typeVar,
                                              unsigned prevSize) {
  Change result;
  result.Kind = ChangeKind::ExtendedEquivalenceClass;
  result.EquivClass.TypeVar = typeVar;
  result.EquivClass.PrevSize = prevSize;
  return result;
}

SolverTrail::Change
SolverTrail::Change::boundTypeVariable(TypeVariableType *typeVar,
                                       Type fixed) {
  Change result;
  result.Kind = ChangeKind::BoundTypeVariable;
  result.Binding.TypeVar = typeVar;
  result.Binding.FixedType = fixed.getPointer();
  return result;
}

SolverTrail::Change
SolverTrail::Change::updatedTypeVariable(
    TypeVariableType *typeVar,
    llvm::PointerUnion<TypeVariableType *, TypeBase *> parentOrFixed,
	  unsigned options) {
  Change result;
  result.Kind = ChangeKind::UpdatedTypeVariable;
  result.Update.TypeVar = typeVar;
  result.Update.ParentOrFixed = parentOrFixed;
  result.Update.Options = options;
  return result;
}

void SolverTrail::Change::undo(ConstraintSystem &cs) {
  auto &cg = cs.getConstraintGraph();

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
    node.truncateEquivalenceClass(EquivClass.PrevSize);
    break;
   }

  case ChangeKind::BoundTypeVariable:
    cg.unbindTypeVariable(Binding.TypeVar, Binding.FixedType);
    break;

  case ChangeKind::UpdatedTypeVariable:
    Update.TypeVar->getImpl().setRawOptions(Update.Options);
    Update.TypeVar->getImpl().ParentOrFixed = Update.ParentOrFixed;
    break;
  }
}

void SolverTrail::recordChange(Change change) {
  if (UndoActive)
    return;

  Changes.push_back(change);
}

void SolverTrail::undo(unsigned toIndex) {
  // Don't attempt to rollback if constraint system ended up
  // in an invalid state.
  if (CS.inInvalidState())
    return;

  ASSERT(Changes.size() >= toIndex && "Trail corrupted");
  ASSERT(!UndoActive);
  UndoActive = true;

  for (unsigned i = Changes.size(); i > toIndex; i--) {
    auto change = Changes[i - 1];
    if (change.Kind == ChangeKind::UpdatedTypeVariable)
      change.undo(CS);
  }

  for (unsigned i = Changes.size(); i > toIndex; i--) {
    auto change = Changes[i - 1];
    if (change.Kind != ChangeKind::UpdatedTypeVariable)
      change.undo(CS);
  }

  Changes.resize(toIndex);
  UndoActive = false;
}

void SolverTrail::dumpActiveScopeChanges(llvm::raw_ostream &out,
                                         unsigned fromIndex,
                                         unsigned indent) {
  if (Changes.empty())
    return;

  // Collect Changes for printing.
  std::map<TypeVariableType *, TypeBase *> tvWithboundTypes;
  std::vector<TypeVariableType *> addedTypeVars;
  std::vector<TypeVariableType *> equivTypeVars;
  std::set<Constraint *> addedConstraints;
  std::set<Constraint *> removedConstraints;
  for (unsigned int i = fromIndex; i < Changes.size(); i++) {
    auto change = Changes[i];
    switch (change.Kind) {
    case ChangeKind::BoundTypeVariable:
      tvWithboundTypes.insert(std::pair<TypeVariableType *, TypeBase *>(
          change.Binding.TypeVar, change.Binding.FixedType));
      break;
    case ChangeKind::AddedTypeVariable:
      addedTypeVars.push_back(change.TypeVar);
      break;
    case ChangeKind::ExtendedEquivalenceClass:
      equivTypeVars.push_back(change.EquivClass.TypeVar);
      break;
    case ChangeKind::AddedConstraint:
      addedConstraints.insert(change.TheConstraint);
      break;
    case ChangeKind::RemovedConstraint:
      removedConstraints.insert(change.TheConstraint);
      break;
    case ChangeKind::UpdatedTypeVariable:
      // Don't consider changes that don't affect the graph.
      break;
    }
  }

  // If there are any constraints that were both added and removed in this set
  // of Changes, remove them from both.
  std::set<Constraint *> intersects;
  set_intersection(addedConstraints.begin(), addedConstraints.end(),
                   removedConstraints.begin(), removedConstraints.end(),
                   std::inserter(intersects, intersects.begin()));
  llvm::set_subtract(addedConstraints, intersects);
  llvm::set_subtract(removedConstraints, intersects);

  // Print out Changes.
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;
  out.indent(indent);
  out << "(Changes:\n";
  if (!tvWithboundTypes.empty()) {
    out.indent(indent + 2);
    out << "(Newly Bound: \n";
    for (const auto &tvWithType : tvWithboundTypes) {
      out.indent(indent + 4);
      out << "> $T" << tvWithType.first->getImpl().getID() << " := ";
      tvWithType.second->print(out, PO);
      out << '\n';
    }
    out.indent(indent + 2);
    out << ")\n";
  }
  if (!addedTypeVars.empty()) {
    out.indent(indent + 2);
    auto heading = (addedTypeVars.size() > 1) ? "(New Type Variables: \n"
                                              : "(New Type Variable: \n";
    out << heading;
    for (const auto &typeVar : addedTypeVars) {
      out.indent(indent + 4);
      out << "> $T" << typeVar->getImpl().getID();
      out << '\n';
    }
    out.indent(indent + 2);
    out << ")\n";
  }
  if (!equivTypeVars.empty()) {
    out.indent(indent + 2);
    auto heading = (equivTypeVars.size() > 1) ? "(New Equivalences: \n"
                                              : "(New Equivalence: \n";
    out << heading;
    for (const auto &typeVar : equivTypeVars) {
      out.indent(indent + 4);
      out << "> $T" << typeVar->getImpl().getID();
      out << '\n';
    }
    out.indent(indent + 2);
    out << ")\n";
  }
  if (!addedConstraints.empty()) {
    out.indent(indent + 2);
    auto heading = (addedConstraints.size() > 1) ? "(Added Constraints: \n"
                                                 : "(Added Constraint: \n";
    out << heading;
    for (const auto &constraint : addedConstraints) {
      out.indent(indent + 4);
      out << "> ";
      constraint->print(out, &CS.getASTContext().SourceMgr, indent + 6);
      out << '\n';
    }
    out.indent(indent + 2);
    out << ")\n";
  }
  if (!removedConstraints.empty()) {
    out.indent(indent + 2);
    auto heading = (removedConstraints.size() > 1) ? "(Removed Constraints: \n"
                                                   : "(Removed Constraint: \n";
    out << heading;
    for (const auto &constraint : removedConstraints) {
      out.indent(indent + 4);
      out << "> ";
      constraint->print(out, &CS.getASTContext().SourceMgr, indent + 6);
      out << '\n';
    }
    out.indent(indent + 2);
    out << ")\n";
  }
  out.indent(indent);
  out << ")\n";
}