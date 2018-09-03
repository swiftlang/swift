//===--- CSFix.cpp - Constraint Fixes -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the \c SolverStep class and its related types,
// which is used by constraint solver to do iterative constraint solving.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CSSTEP_H
#define SWIFT_SEMA_CSSTEP_H

#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include <list>
#include <memory>

using namespace llvm;

namespace swift {
namespace constraints {

class SolverStep {
  struct Scope;

  ConstraintSystem &CS;

  // Type variables and constraints "in scope" of this step.
  SmallVector<TypeVariableType *, 16> TypeVars;
  SmallVector<Constraint *, 16> Constraints;

  // If this step depends on other smaller steps to be solved first
  // we need to keep active scope until all of the work is done.
  Scope *ActiveScope = nullptr;

  /// Once step is complete this is a container to hold finalized solutions.
  SmallVectorImpl<Solution> &Solutions;

public:
  using StepResult =
      std::pair<ConstraintSystem::SolutionKind, std::list<SolverStep>>;

  explicit SolverStep(ConstraintSystem &cs,
                      SmallVectorImpl<Solution> &solutions)
      : CS(cs), Solutions(solutions) {}

  ~SolverStep() {
    if (!ActiveScope)
      return;

    // Since the step is no longer needed, it's same to rewind active scope.
    delete ActiveScope;
    ActiveScope = nullptr;
  }

  /// Record a type variable as associated with this step.
  void record(TypeVariableType *typeVar) { TypeVars.push_back(typeVar); }

  /// Record a constraint as associated with this step.
  void record(Constraint *constraint) { Constraints.push_back(constraint); }

  /// Try to move solver forward by simplifying constraints if possible.
  /// Such simplication might lead to either producing a solution, or
  /// creating a set of "follow-up" more granular steps to execute.
  StepResult advance();

  /// If current step needs follow-up steps to get completely solved,
  /// let's compute them using connected components algorithm.
  StepResult computeFollowupSteps() const;

  static SolverStep create(ConstraintSystem &cs,
                           ArrayRef<TypeVariableType *> typeVars,
                           ConstraintList &constraints,
                           SmallVectorImpl<Solution> &solutions);

  /// Once all of the follow-up steps are complete, let's try
  /// to merge resulting solutions together, to form final solution(s)
  /// for this step.
  ///
  /// \returns true if there are any solutions, false otherwise.
  bool mergePartialSolutions() const { return false; }

private:
  struct Scope {
    ConstraintSystem &CS;
    ConstraintSystem::SolverScope *SolverScope;

    SmallVector<TypeVariableType *, 16> TypeVars;
    ConstraintList Constraints;

    // Current disjunction choice index.
    unsigned CurrChoice = 0;

    // Partial solutions associated with given step, each element
    // of the array presents a disjoint component (or follow-up step)
    // that current step has been split into.
    std::unique_ptr<SmallVector<Solution, 4>[]> PartialSolutions = nullptr;

    Scope(SolverStep &step) : CS(step.CS) {
      TypeVars = std::move(CS.TypeVariables);

      for (auto *typeVar : step.TypeVars)
        CS.TypeVariables.push_back(typeVar);

      Constraints.splice(Constraints.end(), CS.InactiveConstraints);

      for (auto *constraint : step.Constraints)
        CS.InactiveConstraints.push_back(constraint);

      SolverScope = new ConstraintSystem::SolverScope(CS);
    }

    ~Scope() {
      delete SolverScope; // rewind back all of the changes.

      // return all of the saved type variables back to the system.
      CS.TypeVariables = std::move(TypeVars);
      // return all of the saved constraints back to the system.
      CS.InactiveConstraints.splice(CS.InactiveConstraints.end(), Constraints);
    }
  };
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSSTEP_H
