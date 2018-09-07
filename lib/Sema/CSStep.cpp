#include "CSStep.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"

using namespace llvm;
using namespace swift;
using namespace constraints;

ComponentStep::ComponentScope::ComponentScope(ComponentStep &component)
    : CS(component.CS) {
  TypeVars = std::move(CS.TypeVariables);

  for (auto *typeVar : component.TypeVars)
    CS.TypeVariables.push_back(typeVar);

  Constraints.splice(Constraints.end(), CS.InactiveConstraints);

  for (auto *constraint : component.Constraints)
    CS.InactiveConstraints.push_back(constraint);

  auto &CG = CS.getConstraintGraph();
  if (component.OrphanedConstraint)
    CG.setOrphanedConstraint(component.OrphanedConstraint);

  SolverScope = new ConstraintSystem::SolverScope(CS);
  PrevPartialScope = CS.solverState->PartialSolutionScope;
  CS.solverState->PartialSolutionScope = SolverScope;
}

StepResult SplitterStep::take(bool prevFailed) {
  // If we came back to this step and previous (one of the components)
  // failed, it means that we can't solve this step either.
  if (prevFailed)
    return StepResult::failure();

  switch (State) {
  case StepState::Split: {
    SmallVector<SolverStep *, 4> nextSteps;
    computeFollowupSteps(nextSteps);

    State = StepState::Merge;
    return StepResult::unsolved(nextSteps);
  }

  case StepState::Merge: {
    return mergePartialSolutions() ? StepResult::success()
                                   : StepResult::failure();
  }
  }
}

void SplitterStep::computeFollowupSteps(
    SmallVectorImpl<SolverStep *> &nextSteps) {
  SmallVector<ComponentStep *, 4> componentSteps;

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

  NumComponents = CG.computeConnectedComponents(typeVars, components);
  PartialSolutions = std::unique_ptr<SmallVector<Solution, 4>[]>(
      new SmallVector<Solution, 4>[NumComponents]);

  for (unsigned i = 0, n = NumComponents; i != n; ++i)
    componentSteps.push_back(ComponentStep::create(CS, i, PartialSolutions[i]));

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
    auto &step = *componentSteps[components[i]];

    step.record(typeVar);
    for (auto *constraint : CG[typeVar].getConstraints())
      step.record(constraint);
  }

  // Add the orphaned components to the mapping from constraints to components.
  unsigned firstOrphanedConstraint =
      NumComponents - CG.getOrphanedConstraints().size();
  {
    unsigned component = firstOrphanedConstraint;
    for (auto constraint : CG.getOrphanedConstraints())
      componentSteps[component++]->recordOrphan(constraint);
  }

  // Remove all of the orphaned constraints; they'll be re-introduced
  // by each component independently.
  OrphanedConstraints = CG.takeOrphanedConstraints();

  for (auto *step : componentSteps)
    nextSteps.push_back(step);
}

bool SplitterStep::mergePartialSolutions() const {
  // TODO: Optimize when there is only one component
  //       because it would be inefficient to create all
  //       these data structures and do nothing.

  // Produce all combinations of partial solutions.
  SmallVector<unsigned, 2> indices(NumComponents, 0);
  bool done = false;
  bool anySolutions = false;
  do {
    // Create a new solver scope in which we apply all of the partial
    // solutions.
    ConstraintSystem::SolverScope scope(CS);
    for (unsigned i = 0; i != NumComponents; ++i)
      CS.applySolution(PartialSolutions[i][indices[i]]);

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
    for (unsigned n = NumComponents; n > 0; --n) {
      ++indices[n - 1];

      // If we haven't run out of solutions yet, we're done.
      if (indices[n - 1] < PartialSolutions[n - 1].size())
        break;

      // If we ran out of solutions at the first position, we're done.
      if (n == 1) {
        done = true;
        break;
      }

      // Zero out the indices from here to the end.
      for (unsigned i = n - 1; i != NumComponents; ++i)
        indices[i] = 0;
    }
  } while (!done);

  return anySolutions;
}

StepResult ComponentStep::take(bool prevFailed) {
  // If we came either back to this step and previous
  // (either disjunction or type var) failed, or
  // one of the previous components created by "split"
  // failed, it means that we can't solve this component.
  if (prevFailed)
    return StepResult::failure();

  if (!Scope) {
    Scope = new (CS.getAllocator()) ComponentScope(*this);

    if (CS.TC.getLangOpts().DebugConstraintSolver) {
      auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
      log.indent(CS.solverState->depth * 2)
          << "(solving component #" << Index << "\n";
    }

    /// Try to figure out what this step is going to be,
    /// after the scope has been established.
    auto *disjunction = CS.selectDisjunction();
    auto bestBindings = CS.determineBestBindings();

    if (bestBindings &&
        (!disjunction ||
         (!bestBindings->InvolvesTypeVariables && !bestBindings->FullyBound))) {
      // Produce a type variable step.
      return StepResult::unsolved(
          TypeVariableStep::create(CS, *bestBindings, Solutions));
    } else if (disjunction) {
      // Produce a disjunction step.
      return StepResult::unsolved(
          DisjunctionStep::create(CS, disjunction, Solutions));
    }

    // If there are no disjunctions or type variables to bind
    // we can't solve this system unless we have free type variables
    // allowed in the solution.
    if (!CS.solverState->allowsFreeTypeVariables() ||
        !CS.hasFreeTypeVariables())
      return StepResult::failure();

    // If this solution is worse than the best solution we've seen so far,
    // skip it.
    if (CS.worseThanBestSolution())
      return StepResult::failure();

    // If we only have relational or member constraints and are allowing
    // free type variables, save the solution.
    for (auto &constraint : CS.InactiveConstraints) {
      switch (constraint.getClassification()) {
      case ConstraintClassification::Relational:
      case ConstraintClassification::Member:
        continue;
      default:
        return StepResult::failure();
      }
    }

    auto solution = CS.finalize();
    if (CS.TC.getLangOpts().DebugConstraintSolver) {
      auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
      log.indent(CS.solverState->depth * 2) << "(found solution)\n";
    }

    Solutions.push_back(std::move(solution));
    return StepResult::success();
  }

  // For each of the partial solutions, subtract off the current score.
  // It doesn't contribute.
  for (auto &solution : Solutions)
    solution.getFixedScore() -= OriginalScore;

  // When there are multiple partial solutions for a given connected component,
  // rank those solutions to pick the best ones. This limits the number of
  // combinations we need to produce; in the common case, down to a single
  // combination.
  filterSolutions(Solutions, /*minimize=*/true);
  return StepResult::success();
}

StepResult TypeVariableStep::take(bool prevFailed) {
  return StepResult::failure();
}

StepResult DisjunctionStep::take(bool prevFailed) {
  return StepResult::failure();
}

bool DisjunctionStep::shouldSkipChoice(DisjunctionChoice &choice) const {
  return false;
}
