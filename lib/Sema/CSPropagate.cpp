//===--- CSPropagate.cpp - Constraint Propagation -------------------------===//
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
// This file implements the constraint propagation algorithm in the solver.
//
//===----------------------------------------------------------------------===//
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace constraints;

bool isBindOverloadDisjunction(Constraint *disjunction) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction &&
         "Expected disjunction constraint!");

  assert(!disjunction->getNestedConstraints().empty() &&
         "Unexpected empty disjunction!");

  auto *nested = disjunction->getNestedConstraints().front();
  return nested->getKind() == ConstraintKind::BindOverload;
}

// Find the disjunction of bind overload constraints related to this
// applicable function constraint, if it exists.
Constraint *
getBindOverloadDisjunction(ConstraintSystem &CS, Constraint *applicableFn) {
  assert(applicableFn->getKind() == ConstraintKind::ApplicableFunction
         && "Expected ApplicableFunction disjunction!");
  auto *tyvar = applicableFn->getSecondType()->getAs<TypeVariableType>();
  assert(tyvar && "Expected type variable!");

  Constraint *found = nullptr;
  for (auto *constraint : CS.getConstraintGraph()[tyvar].getConstraints()) {
    if (constraint->getKind() == ConstraintKind::Disjunction) {
      found = constraint;
      break;
    }
  }

  if (!found)
    return nullptr;

#if !defined(NDEBUG)
  for (auto *constraint : CS.getConstraintGraph()[tyvar].getConstraints()) {
    if (constraint == found)
      continue;

    assert(constraint->getKind() != ConstraintKind::Disjunction
           && "Type variable is involved in more than one disjunction!");
  }
#endif

  // Verify the disjunction consists of BindOverload constraints.
  assert(isBindOverloadDisjunction(found));

  return found;
}

void ConstraintSystem::collectNeighboringBindOverloadDisjunctions(
    llvm::SetVector<Constraint *> &neighbors) {

  while (!ActiveConstraints.empty()) {
    auto *constraint = &ActiveConstraints.front();
    ActiveConstraints.pop_front();

    assert(constraint->isActive() && "Expected constraints to be active?");
    assert(!constraint->isDisabled() && "Unexpected disabled constraint!");

    if (constraint->getKind() == ConstraintKind::Disjunction) {
      if (isBindOverloadDisjunction(constraint)) {
        neighbors.insert(constraint);
      }
    } else if (constraint->getKind() == ConstraintKind::ApplicableFunction) {
      if (auto *bindDisjunction =
              getBindOverloadDisjunction(*this, constraint)) {
        neighbors.insert(bindDisjunction);
      }
    }

    solverState->retireConstraint(constraint);
    CG.removeConstraint(constraint);
    constraint->setActive(false);
  }
}

// Simplify any active constraints, returning true on success, false
// on failure.
bool ConstraintSystem::simplifyForConstraintPropagation() {
  SmallVector<Constraint *, 4> conformsToConstraints;

  while (!ActiveConstraints.empty()) {
    auto *constraint = &ActiveConstraints.front();
    ActiveConstraints.pop_front();

    assert(constraint->isActive()
           && "Expected constraints to be active?");
    assert(!constraint->isDisabled() && "Unexpected disabled constraint!");

    if (constraint->getKind() == ConstraintKind::ConformsTo)
      conformsToConstraints.push_back(constraint);

    bool failed = false;

    // Simplify this constraint.
    switch (simplifyConstraint(*constraint)) {
    case SolutionKind::Error:
      failed = true;
      LLVM_FALLTHROUGH;

    case SolutionKind::Solved:
      solverState->retireConstraint(constraint);
      CG.removeConstraint(constraint);
      break;

    case SolutionKind::Unsolved:
      InactiveConstraints.push_back(constraint);
      break;
    }

    constraint->setActive(false);

    if (failed)
      return false;
  }

  return areProtocolConformancesConsistent(conformsToConstraints);
}

bool ConstraintSystem::areBindPairConsistent(Constraint *first,
                                             Constraint *second) {
  // Set up a scope that will be torn down when we're done testing
  // this constraint.
  ConstraintSystem::SolverScope scope(*this);

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "Testing constraints for consistency: ";
    first->print(log, &TC.Context.SourceMgr);
    log << "\nversus: ";
    second->print(log, &TC.Context.SourceMgr);
  }

  auto result = simplifyConstraint(*first);
  assert(result == ConstraintSystem::SolutionKind::Solved &&
         "Expected the first bind constraint to work!");

  solverState->retireConstraint(first);
  solverState->addGeneratedConstraint(first);

  result = simplifyConstraint(*second);
  assert(result == ConstraintSystem::SolutionKind::Solved &&
         "Expected the second bind constraint to work!");

  solverState->retireConstraint(second);
  solverState->addGeneratedConstraint(second);

  auto success = simplifyForConstraintPropagation();
  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    if (success)
      log << "Consistent!\n";
    else
      log << "Not consistent!\n";
  }

  return success;
}

// Test a bind overload constraint to see if it is consistent with the
// rest of the constraint system.
bool ConstraintSystem::isBindOverloadConsistent(
    Constraint *bindConstraint, llvm::SetVector<Constraint *> &workList) {

  llvm::SetVector<Constraint *> otherDisjunctions;

  {
    // Set up a scope that will be torn down when we're done testing
    // this constraint.
    ConstraintSystem::SolverScope scope(*this);

    assert(bindConstraint->getKind() == ConstraintKind::BindOverload &&
           "Expected a BindOverload constraint!");

    // Test this bind overload constraint, activating neighboring
    // constraints.
    auto result = simplifyConstraint(*bindConstraint);
    assert(result == ConstraintSystem::SolutionKind::Solved &&
           "Expected the bind constraint to work!");
    (void)result;

    solverState->retireConstraint(bindConstraint);
    solverState->addGeneratedConstraint(bindConstraint);

    collectNeighboringBindOverloadDisjunctions(otherDisjunctions);
  }

  // Test the our primary constraint against all of the members of
  // neighboring disjunctions. If this constraint fails with all
  // members of a neighboring disjunction, we'll disable it and queue
  // up these neighbors for further processing. If this constraint
  // works with any member of each of the disjunctions, we do not need
  // to test the remaining members.
  for (auto *disjunction : otherDisjunctions) {
    auto insertPt = InactiveConstraints.erase(disjunction);
    CG.removeConstraint(disjunction);

    bool foundConsistent = false;
    for (auto *nested : disjunction->getNestedConstraints()) {
      assert(nested->getKind() == ConstraintKind::BindOverload &&
             "Expected a BindOverload constraint");

      if (nested->isDisabled())
        continue;

      if (areBindPairConsistent(bindConstraint, nested)) {
        foundConsistent = true;
        break;
      }
    }

    CG.addConstraint(disjunction);
    InactiveConstraints.insert(insertPt, disjunction);

    // We failed to find a working pair between the bind overload
    // constraint we started with and the members of this
    // disjunction. We'll mark this bind overload as disabled and
    // queue up the neighboring disjunctions for re-processing.
    if (!foundConsistent) {
      if (TC.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log << "Disabling bind constraint: ";
        bindConstraint->print(log, &TC.Context.SourceMgr);
        log << "\n";
      }

      bindConstraint->setDisabled();
      for (auto *disjunction : otherDisjunctions)
        workList.insert(disjunction);

      return false;
    }
  }

  return true;
}

void ConstraintSystem::reviseBindOverloadDisjunction(
    Constraint *disjunction, llvm::SetVector<Constraint *> &workList,
    bool *foundConsistent) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction &&
         "Expected disjunction constraint on work list!");

  // Set up a scope that will be torn down when we're done testing
  // this constraint.
  ConstraintSystem::SolverScope scope(*this);

  // Temporarily remove the disjunction from the constraint system and
  // constraint graph.
  auto insertPt = InactiveConstraints.erase(disjunction);
  CG.removeConstraint(disjunction);

  *foundConsistent = false;
  for (auto *bindConstraint : disjunction->getNestedConstraints()) {
    assert(bindConstraint->getKind() == ConstraintKind::BindOverload
           && "Expected a BindOverload constraint!");

    if (bindConstraint->isDisabled())
      continue;

    if (isBindOverloadConsistent(bindConstraint, workList))
      *foundConsistent = true;
  }

  CG.addConstraint(disjunction);
  InactiveConstraints.insert(insertPt, disjunction);
}

// Do a form of constraint propagation consisting of examining
// applicable function constraints and their associated disjunction of
// bind overload constraints. Disable bind overload constraints in the
// disjunction if they are inconsistent with the rest of the
// constraint system. By doing this we can eliminate a lot of the work
// that we'll perform in the constraint solver.
bool ConstraintSystem::propagateConstraints() {
  assert(!failedConstraint && "Unexpected failed constraint!");
  assert(getActiveConstraints().empty() && "Expected no active constraints!");

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Propagating constraints---\n";
  }

  // Queue an initial list of bind overload disjunction constraints to
  // process.
  llvm::SetVector<Constraint *> workList;
  for (auto &constraint : getConstraints())
    if (constraint.getKind() == ConstraintKind::Disjunction)
      if (isBindOverloadDisjunction(&constraint))
        workList.insert(&constraint);

  // Process each disjunction in the work list. If we modify the
  // active constraints in the disjunction as a result of processing
  // it, we'll add it's neighbors back to the worklist for
  // reprocessing.
  while (!workList.empty()) {
    auto *disjunction = workList.pop_back_val();

    bool foundConsistent;
    reviseBindOverloadDisjunction(disjunction, workList, &foundConsistent);
    if (!foundConsistent)
      return true;
  }

  return false;
}
