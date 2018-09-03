#include "CSStep.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

using namespace llvm;
using namespace swift;
using namespace constraints;

SolverStep *SolverStep::create(ConstraintSystem &cs,
                               SmallVectorImpl<Solution> &solutions) {
  return new (cs.getAllocator()) SolverStep(cs, solutions);
}

SolverStep *SolverStep::create(ConstraintSystem &cs,
                               ArrayRef<TypeVariableType *> typeVars,
                               ConstraintList &constraints,
                               SmallVectorImpl<Solution> &solutions) {
  auto *step = SolverStep::create(cs, solutions);

  for (auto *typeVar : typeVars)
    step->record(typeVar);

  for (auto &constraint : constraints)
    step->record(&constraint);

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
  SmallVector<SolverStep *, 4> nextSteps;

  // Compute next steps based on that connected components
  // algorithm tells us is splittable.

  auto &CG = CS.getConstraintGraph();
  // Contract the edges of the constraint graph.
  CG.optimize();

  // Compute the connected components of the constraint graph.
  // FIXME: We're seeding typeVars with TypeVariables so that the
  // connected-components algorithm only considers those type variables within
  // our component. There are clearly better ways to do this.
  SmallVector<TypeVariableType *, 16> typeVars(CS.TypeVariables);
  SmallVector<unsigned, 16> components;
  unsigned numComponents = CG.computeConnectedComponents(typeVars, components);

  std::unique_ptr<SmallVector<Solution, 4>[]> partialSolutions(
      new SmallVector<Solution, 4>[numComponents]);

  ActiveScope->NumComponents = numComponents;
  ActiveScope->PartialSolutions = std::move(partialSolutions);

  for (unsigned i = 0, n = numComponents; i != n; ++i)
    nextSteps.push_back(SolverStep::create(CS, partialSolutions[i]));

  if (CS.getASTContext().LangOpts.DebugConstraintSolver) {
    auto &log = CS.getASTContext().TypeCheckerDebug->getStream();

    // Verify that the constraint graph is valid.
    CG.verify();

    log << "---Constraint graph---\n";
    CG.print(log);

    log << "---Connected components---\n";
    CG.printConnectedComponents(log);
  }

  // Map type variables and constraints into appropriate steps.
  for (unsigned i = 0, n = typeVars.size(); i != n; ++i) {
    auto *typeVar = typeVars[i];
    auto &step = *nextSteps[components[i]];

    step.record(typeVar);
    for (auto *constraint : CG[typeVar].getConstraints())
      step.record(constraint);
  }

  // Add the orphaned components to the mapping from constraints to components.
  unsigned firstOrphanedConstraint =
      numComponents - CG.getOrphanedConstraints().size();
  {
    unsigned component = firstOrphanedConstraint;
    for (auto constraint : CG.getOrphanedConstraints())
      nextSteps[component++]->record(constraint);
  }

  return {ConstraintSystem::SolutionKind::Unsolved, std::move(nextSteps)};
}

bool SolverStep::mergePartialSolutions() const {
  assert(ActiveScope);

  auto numComponents = ActiveScope->NumComponents;
  auto &partialSolutions = ActiveScope->PartialSolutions;

  // TODO: Optimize when there is only one component
  //       because it would be inefficient to create all
  //       these data structures and do nothing.

  // Produce all combinations of partial solutions.
  SmallVector<unsigned, 2> indices(numComponents, 0);
  bool done = false;
  bool anySolutions = false;
  do {
    // Create a new solver scope in which we apply all of the partial
    // solutions.
    ConstraintSystem::SolverScope scope(CS);
    for (unsigned i = 0; i != numComponents; ++i)
      CS.applySolution(partialSolutions[i][indices[i]]);

    // This solution might be worse than the best solution found so far.
    // If so, skip it.
    if (!CS.worseThanBestSolution()) {
      // Finalize this solution.
      auto solution = CS.finalize();
      if (CS.TC.getLangOpts().DebugConstraintSolver) {
        auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
        log.indent(CS.solverState->depth * 2)
            << "(composed solution " << CS.CurrentScore << ")\n";
      }

      // Save this solution.
      Solutions.push_back(std::move(solution));
      anySolutions = true;
    }

    // Find the next combination.
    for (unsigned n = numComponents; n > 0; --n) {
      ++indices[n - 1];

      // If we haven't run out of solutions yet, we're done.
      if (indices[n - 1] < partialSolutions[n - 1].size())
        break;

      // If we ran out of solutions at the first position, we're done.
      if (n == 1) {
        done = true;
        break;
      }

      // Zero out the indices from here to the end.
      for (unsigned i = n - 1; i != numComponents; ++i)
        indices[i] = 0;
    }
  } while (!done);

  return anySolutions;
}
