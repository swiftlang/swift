#include "CSStep.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include <list>

using namespace llvm;
using namespace swift;
using namespace constraints;

SolverStep SolverStep::create(ConstraintSystem &cs,
                              ArrayRef<TypeVariableType *> typeVars,
                              ConstraintList &constraints,
                              SmallVectorImpl<Solution> &solutions) {
  SolverStep step(cs, solutions);

  for (auto *typeVar : typeVars)
    step.record(typeVar);

  for (auto &constraint : constraints)
    step.record(&constraint);

  return step;
}

SolverStep::StepResult SolverStep::advance() {
  if (!ActiveScope)
    ActiveScope = new (CS.getAllocator()) Scope(*this);

  auto *disjunction = CS.selectDisjunction();
  auto bestBindings = CS.determineBestBindings();

  // TODO: Add expression too complex into the mix
  // TODO: This is where the generator comes in.

  if (bestBindings && (!disjunction || (!bestBindings->InvolvesTypeVariables &&
                                        !bestBindings->FullyBound))) {
    // Attempt given "best" binding and record the current scope,
    // we'll have to come back to this step later on when follow-up
    // steps are solved.
    return computeFollowupSteps();
  }

  // For disjunctions we need to figure out what all of the already
  // attempted choices are, and try the next one.
  auto choices = disjunction->getNestedConstraints();
  for (unsigned i = ActiveScope->CurrChoice, n = choices.size(); i != n; ++i) {
    auto *choice = choices[i];
    if (choice->isDisabled())
      continue;

    // Attempt the choice and record it in the scope.
    ActiveScope->CurrChoice = i;
    return computeFollowupSteps();
  }

  // Since we are done with this step, it's a good time to
  // roll-up all of the solutions produced by follow-up steps.
  auto result = mergePartialSolutions() ? ConstraintSystem::SolutionKind::Solved
                                        : ConstraintSystem::SolutionKind::Error;

  return {result, {}};
}

SolverStep::StepResult SolverStep::computeFollowupSteps() const {
  std::list<SolverStep> nextSteps;
  // Compute next steps based on that connected components
  // algorithm tells us is splittable.
  return {ConstraintSystem::SolutionKind::Unsolved, std::move(nextSteps)};
}
