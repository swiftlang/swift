//===--- CSStep.h - Constraint Solver Steps -------------------------------===//
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
// which is used by constraint solver to do iterative solving.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CSSTEP_H
#define SWIFT_SEMA_CSSTEP_H

#include "Constraint.h"
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

using namespace llvm;

namespace swift {
namespace constraints {

class SolverStep;
class ComponentStep;

/// Represents available states which every
/// given step could be in during it's lifetime.
enum class StepState { Setup, Ready, Running, Suspended, Done };

/// Represents result of the step execution,
/// and can only be constructed by `SolverStep`.
struct StepResult {
  using Kind = ConstraintSystem::SolutionKind;

  friend class SolverStep;

private:
  Kind ResultKind;
  SmallVector<std::unique_ptr<SolverStep>, 4> NextSteps;

  StepResult(Kind kind) : ResultKind(kind) {}

  StepResult(Kind kind, std::unique_ptr<SolverStep> step) : ResultKind(kind) {
    NextSteps.push_back(std::move(step));
  }

  StepResult(Kind kind, SmallVectorImpl<std::unique_ptr<SolverStep>> &followup)
      : ResultKind(kind), NextSteps(std::move(followup)) {}

public:
  StepResult() = delete;

  Kind getKind() const { return ResultKind; }

  void transfer(SmallVectorImpl<std::unique_ptr<SolverStep>> &workList) {
    workList.reserve(NextSteps.size());
    for (auto &step : NextSteps)
      workList.push_back(std::move(step));
  }

private:
  static StepResult success() { return StepResult(Kind::Solved); }
  static StepResult failure() { return StepResult(Kind::Error); }

  static StepResult unsolved(std::unique_ptr<SolverStep> singleStep) {
    return StepResult(Kind::Unsolved, std::move(singleStep));
  }

  static StepResult
  unsolved(SmallVectorImpl<std::unique_ptr<SolverStep>> &followup) {
    return StepResult(Kind::Unsolved, followup);
  }
};

/// Represents a single independently solvable part of
/// the constraint system. And is a base class for all
/// different types of steps there are.
class SolverStep {
  friend class ConstraintSystem;

protected:
  ConstraintSystem &CS;

  StepState State = StepState::Setup;

  /// Once step is complete this is a container to hold finalized solutions.
  SmallVectorImpl<Solution> &Solutions;

public:
  explicit SolverStep(ConstraintSystem &cs,
                      SmallVectorImpl<Solution> &solutions)
      : CS(cs), Solutions(solutions) {}

  virtual ~SolverStep() {}

  /// \returns The current state of this step.
  StepState getState() const { return State; }

  /// Run preliminary setup (if needed) right
  /// before taking this step for the first time.
  virtual void setup() {}

  /// Try to move solver forward by simplifying constraints if possible.
  /// Such simplication might lead to either producing a solution, or
  /// creating a set of "follow-up" more granular steps to execute.
  ///
  /// \param prevFailed Indicate whether previous step
  ///        has failed (returned StepResult::Kind = Error),
  ///        this is useful to propagate failures when
  ///        unsolved steps are re-taken.
  ///
  /// \returns status and any follow-up steps to take before considering
  ///          this step solved or failed.
  virtual StepResult take(bool prevFailed) = 0;

  /// Try to resume previously suspended step.
  ///
  /// This happens after "follow-up" steps are done
  /// and all of the required information should be
  /// available to re-take this step.
  ///
  /// \param prevFailed Indicate whether previous step
  ///        has failed (returned StepResult::Kind = Error),
  ///        this is useful to propagate failures when
  ///        unsolved steps are re-taken.
  ///
  /// \returns status and any follow-up steps to take before considering
  ///          this step solved or failed.
  virtual StepResult resume(bool prevFailed) = 0;

  virtual void print(llvm::raw_ostream &Out) = 0;

protected:
  /// Transition this step into one of the available states.
  ///
  /// This is primarily driven by execution of the step itself and
  /// the solver, while it executes the work list.
  ///
  /// \param newState The new state this step should be in.
  void transitionTo(StepState newState) {
#ifndef NDEBUG
    // Make sure that ordering of the state transitions is correct,
    // because `setup -> ready -> running [-> suspended]* -> done`
    // is the only reasonable state transition path.
    switch (State) {
    case StepState::Setup:
      assert(newState == StepState::Ready);
      break;

    case StepState::Ready:
      assert(newState == StepState::Running);
      break;

    case StepState::Running:
      assert(newState == StepState::Suspended || newState == StepState::Done);
      break;

    case StepState::Suspended:
      assert(newState == StepState::Running);
      break;

    case StepState::Done:
      llvm_unreachable("step is already done.");
    }
#endif

    State = newState;
  }

  StepResult done(bool isSuccess) {
    transitionTo(StepState::Done);
    return isSuccess ? StepResult::success() : StepResult::failure();
  }

  StepResult replaceWith(std::unique_ptr<SolverStep> replacement) {
    transitionTo(StepState::Done);
    return StepResult(StepResult::Kind::Solved, std::move(replacement));
  }

  StepResult suspend(std::unique_ptr<SolverStep> followup) {
    transitionTo(StepState::Suspended);
    return StepResult::unsolved(std::move(followup));
  }

  StepResult suspend(SmallVectorImpl<std::unique_ptr<SolverStep>> &followup) {
    transitionTo(StepState::Suspended);
    return StepResult::unsolved(followup);
  }

  /// Erase constraint from the constraint system (include constraint graph)
  /// and return the constraint which follows it.
  ConstraintList::iterator erase(Constraint *constraint) {
    CS.CG.removeConstraint(constraint);
    return CS.InactiveConstraints.erase(constraint);
  }

  void restore(ConstraintList::iterator &iterator, Constraint *constraint) {
    CS.InactiveConstraints.insert(iterator, constraint);
    CS.CG.addConstraint(constraint);
  }

  ResolvedOverloadSetListItem *getResolvedOverloads() const {
    return CS.resolvedOverloadSets;
  }

  void recordDisjunctionChoice(ConstraintLocator *disjunctionLocator,
                               unsigned index) const {
    CS.recordDisjunctionChoice(disjunctionLocator, index);
  }

  Score getCurrentScore() const { return CS.CurrentScore; }

  Optional<Score> getBestScore() const { return CS.solverState->BestScore; }

  void filterSolutions(SmallVectorImpl<Solution> &solutions, bool minimize) {
    if (!CS.retainAllSolutions())
      CS.filterSolutions(solutions, minimize);
  }

  /// Check whether constraint solver is running in "debug" mode,
  /// which should output diagnostic information.
  bool isDebugMode() const { return CS.TC.getLangOpts().DebugConstraintSolver; }

  llvm::raw_ostream &getDebugLogger(bool indent = true) const {
    auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
    return indent ? log.indent(CS.solverState->depth * 2) : log;
  }
};

/// `SplitterStep` is responsible for running connected components
/// algorithm to determine how many independent sub-systems there are.
/// Once that's done it would create one `ComponentStep` per such
/// sub-system, and move to try to solve each and then merge partial
/// solutions produced by components into complete solution(s).
class SplitterStep final : public SolverStep {
  // Set of constraints associated with each component, after
  // component steps are complete, all of the constraints are
  // returned back to the work-list in their original order.
  SmallVector<ConstraintList, 4> Components;
  // Partial solutions associated with given step, each element
  // of the array presents a disjoint component (or follow-up step)
  // that current step has been split into.
  std::unique_ptr<SmallVector<Solution, 4>[]> PartialSolutions = nullptr;

  SmallVector<Constraint *, 4> OrphanedConstraints;

public:
  SplitterStep(ConstraintSystem &cs, SmallVectorImpl<Solution> &solutions)
      : SolverStep(cs, solutions) {}

  StepResult take(bool prevFailed) override;
  StepResult resume(bool prevFailed) override;

  void print(llvm::raw_ostream &Out) override {
    Out << "SplitterStep with #" << Components.size() << " components\n";
  }

private:
  /// If current step needs follow-up steps to get completely solved,
  /// let's compute them using connected components algorithm.
  void computeFollowupSteps(
      SmallVectorImpl<std::unique_ptr<ComponentStep>> &componentSteps);

  /// Once all of the follow-up steps are complete, let's try
  /// to merge resulting solutions together, to form final solution(s)
  /// for this step.
  ///
  /// \returns true if there are any solutions, false otherwise.
  bool mergePartialSolutions() const;
};

/// `ComponentStep` represents a set of type variables and related
/// constraints which could be solved independently. It's further
/// simplified into "binding" steps which attempt type variable and
/// disjunction choices.
class ComponentStep final : public SolverStep {
  class Scope {
    ConstraintSystem &CS;
    ConstraintSystem::SolverScope *SolverScope;

    std::vector<TypeVariableType *> TypeVars;
    ConstraintSystem::SolverScope *PrevPartialScope = nullptr;

    // The component this scope is associated with.
    ComponentStep &Component;

  public:
    Scope(ComponentStep &component);

    ~Scope() {
      delete SolverScope; // rewind back all of the changes.
      CS.solverState->PartialSolutionScope = PrevPartialScope;

      // return all of the saved type variables back to the system.
      CS.TypeVariables = std::move(TypeVars);
      // return all of the saved constraints back to the component.
      auto &constraints = *Component.Constraints;
      constraints.splice(constraints.end(), CS.InactiveConstraints);
    }
  };

  /// The position of the component in the set of
  /// components produced by "split" step.
  unsigned Index;

  /// Indicates whether this is only component produced
  /// by "split" step. This information opens optimization
  /// opportunity, because if there are no other components,
  /// constraint system doesn't have to pruned from
  /// unrelated type variables and their constraints.
  bool IsSingle;

  /// The score associated with constraint system before
  /// the component step is taken.
  Score OriginalScore;

  /// The original best score computed before any of the
  /// component steps belonging to the same "split" are taken.
  Optional<Score> OriginalBestScore;

  /// If this step depends on other smaller steps to be solved first
  /// we need to keep active scope until all of the work is done.
  std::unique_ptr<Scope> ComponentScope = nullptr;

  /// Type variables and constraints "in scope" of this step.
  TinyPtrVector<TypeVariableType *> TypeVars;
  /// Constraints "in scope" of this step.
  ConstraintList *Constraints;

  /// Number of disjunction constraints associated with this step,
  /// used to aid in ordering of the components.
  unsigned NumDisjunctions = 0;

  /// Constraint which doesn't have any free type variables associated
  /// with it, which makes it disconnected in the graph.
  Constraint *OrphanedConstraint = nullptr;

public:
  /// Create a single component step.
  ComponentStep(ConstraintSystem &cs, unsigned index,
                ConstraintList *constraints,
                SmallVectorImpl<Solution> &solutions)
      : SolverStep(cs, solutions), Index(index), IsSingle(true),
        OriginalScore(getCurrentScore()), OriginalBestScore(getBestScore()),
        Constraints(constraints) {}

  /// Create a component step from a constraint graph component.
  ComponentStep(ConstraintSystem &cs, unsigned index,
                ConstraintList *constraints,
                ConstraintGraph::Component &&component,
                SmallVectorImpl<Solution> &solutions)
      : SolverStep(cs, solutions), Index(index), IsSingle(false),
        OriginalScore(getCurrentScore()), OriginalBestScore(getBestScore()),
        Constraints(constraints) {
    TypeVars = std::move(component.typeVars);

    for (auto constraint : component.constraints) {
      constraints->erase(constraint);
      record(constraint);
    }
  }

  /// Create a component step for an orphaned constraint.
  ComponentStep(ConstraintSystem &cs, unsigned index,
                ConstraintList *constraints,
                Constraint *orphaned,
                SmallVectorImpl<Solution> &solutions)
      : SolverStep(cs, solutions), Index(index), IsSingle(false),
        OriginalScore(getCurrentScore()), OriginalBestScore(getBestScore()),
        Constraints(constraints), OrphanedConstraint(orphaned) {
    constraints->erase(orphaned);
    record(orphaned);
  }

private:
  /// Record a constraint as associated with this step.
  void record(Constraint *constraint) {
    Constraints->push_back(constraint);
    if (constraint->getKind() == ConstraintKind::Disjunction)
      ++NumDisjunctions;
  }

public:

  StepResult take(bool prevFailed) override;

  StepResult resume(bool prevFailed) override { return finalize(!prevFailed); }

  // The number of disjunction constraints associated with this component.
  unsigned disjunctionCount() const { return NumDisjunctions; }

  void print(llvm::raw_ostream &Out) override {
    Out << "ComponentStep with at #" << Index << '\n';
  }

private:
  void setupScope() {
    // If this is a single component, there is no need
    // to preliminary modify constraint system or log anything.
    if (IsSingle)
      return;

    if (isDebugMode())
      getDebugLogger() << "(solving component #" << Index << '\n';

    ComponentScope = llvm::make_unique<Scope>(*this);
    // If this component has oprhaned constraint attached,
    // let's return it to the graph.
    CS.CG.setOrphanedConstraint(OrphanedConstraint);
  }

  /// Finalize current component by either cleanup if sub-tasks
  /// have failed, or solution generation and minimization.
  StepResult finalize(bool isSuccess);
};

template <typename P> class BindingStep : public SolverStep {
  using Scope = ConstraintSystem::SolverScope;

  P Producer;

protected:
  /// Indicates whether any of the attempted bindings
  /// produced a solution.
  bool AnySolved = false;

  /// Active binding (scope + choice) which is currently
  /// being attempted, helps to rewind state of the
  /// constraint system back to original before attempting
  /// next binding, if any.
  Optional<std::pair<std::unique_ptr<Scope>, typename P::Element>> ActiveChoice;

  BindingStep(ConstraintSystem &cs, P producer,
              SmallVectorImpl<Solution> &solutions)
      : SolverStep(cs, solutions), Producer(std::move(producer)) {}

public:
  StepResult take(bool prevFailed) override {
    while (auto choice = Producer()) {
      if (shouldSkip(*choice))
        continue;

      if (shouldStopAt(*choice))
        break;

      if (isDebugMode()) {
        auto &log = getDebugLogger();
        log << "(attempting ";
        choice->print(log, &CS.getASTContext().SourceMgr);
        log << '\n';
      }

      {
        auto scope = llvm::make_unique<Scope>(CS);
        if (attempt(*choice)) {
          ActiveChoice.emplace(std::move(scope), *choice);
          return suspend(llvm::make_unique<SplitterStep>(CS, Solutions));
        }
      }

      if (isDebugMode())
        getDebugLogger() << ")\n";

      // If this binding didn't match, let's check if we've attempted
      // enough bindings to stop, because some producers might need
      // to compute next step of bindings to try, which we'd want to avoid.
      if (shouldStopAfter(*choice))
        break;
    }

    return done(/*isSuccess=*/AnySolved);
  }

protected:
  /// Attempt to apply given binding choice to constraint system.
  /// This action is going to establish "active choice" of this step
  /// to point to a given choice.
  ///
  /// \param choice The choice to attempt.
  ///
  /// \return true if the choice has been accepted and system can be
  /// simplified further, false otherwise.
  virtual bool attempt(const typename P::Element &choice) = 0;

  /// Check whether attempting this choice could be avoided,
  /// which could speed-up solving.
  virtual bool shouldSkip(const typename P::Element &choice) const = 0;

  /// Check whether attempting binding choices should be stopped,
  /// because optimal solution has already been found.
  virtual bool shouldStopAt(const typename P::Element &choice) const = 0;

  /// Check whether attempting binding choices should be stopped,
  /// after current choice has been attempted, because optimal
  /// solution has already been found,
  virtual bool shouldStopAfter(const typename P::Element &choice) const {
    return false;
  }

  bool needsToComputeNext() const { return Producer.needsToComputeNext(); }

  ConstraintLocator *getLocator() const { return Producer.getLocator(); }
};

class TypeVariableStep final : public BindingStep<TypeVarBindingProducer> {
  using BindingContainer = ConstraintSystem::PotentialBindings;
  using Binding = ConstraintSystem::PotentialBinding;

  TypeVariableType *TypeVar;
  // A set of the initial bindings to consider, which is
  // also a source of follow-up "computed" bindings such
  // as supertypes, defaults etc.
  SmallVector<Binding, 4> InitialBindings;

  /// Indicates whether source of one of the previously
  /// attempted bindings was a literal constraint. This
  /// is useful for a performance optimization to stop
  /// attempting other bindings in certain conditions.
  bool SawFirstLiteralConstraint = false;

public:
  TypeVariableStep(ConstraintSystem &cs, BindingContainer &bindings,
                   SmallVectorImpl<Solution> &solutions)
      : BindingStep(cs, {cs, bindings}, solutions), TypeVar(bindings.TypeVar),
        InitialBindings(bindings.Bindings.begin(), bindings.Bindings.end()) {}

  void setup() override;

  StepResult resume(bool prevFailed) override;

  void print(llvm::raw_ostream &Out) override {
    Out << "TypeVariableStep for " << TypeVar->getString() << " with #"
        << InitialBindings.size() << " initial bindings\n";
  }

protected:
  bool attempt(const TypeVariableBinding &choice) override;

  bool shouldSkip(const TypeVariableBinding &choice) const override {
    // If this is a defaultable binding and we have found solutions,
    // don't explore the default binding.
    return AnySolved && choice.isDefaultable();
  }

  /// Check whether attempting type variable binding choices should
  /// be stopped, because optimal solution has already been found.
  bool shouldStopAt(const TypeVariableBinding &choice) const override {
    // If we were able to solve this without considering
    // default literals, don't bother looking at default literals.
    return AnySolved && choice.hasDefaultedProtocol() &&
           !SawFirstLiteralConstraint;
  }

  bool shouldStopAfter(const TypeVariableBinding &choice) const override {
    // If there has been at least one solution so far
    // at a current batch of bindings is done it's a
    // success because each new batch would be less
    // and less precise.
    return AnySolved && needsToComputeNext();
  }
};

class DisjunctionStep final : public BindingStep<DisjunctionChoiceProducer> {
  Constraint *Disjunction;
  SmallVector<Constraint *, 4> DisabledChoices;
  ConstraintList::iterator AfterDisjunction;

  Optional<Score> BestNonGenericScore;
  Optional<std::pair<Constraint *, Score>> LastSolvedChoice;

public:
  DisjunctionStep(ConstraintSystem &cs, Constraint *disjunction,
                  SmallVectorImpl<Solution> &solutions)
      : BindingStep(cs, {cs, disjunction}, solutions), Disjunction(disjunction),
        AfterDisjunction(erase(disjunction)) {
    assert(Disjunction->getKind() == ConstraintKind::Disjunction);
    pruneOverloadSet(Disjunction);
    ++cs.solverState->NumDisjunctions;
  }

  ~DisjunctionStep() override {
    // Rewind back any changes left after attempting last choice.
    ActiveChoice.reset();
    // Return disjunction constraint back to the system.
    restore(AfterDisjunction, Disjunction);
    // Re-enable previously disabled overload choices.
    for (auto *choice : DisabledChoices)
      choice->setEnabled();
  }

  StepResult resume(bool prevFailed) override;

  void print(llvm::raw_ostream &Out) override {
    Out << "DisjunctionStep for ";
    Disjunction->print(Out, &CS.getASTContext().SourceMgr);
    Out << '\n';
  }

private:
  bool shouldSkip(const DisjunctionChoice &choice) const override;

  /// Whether we should short-circuit a disjunction that already has a
  /// solution when we encounter the given choice.
  ///
  /// FIXME: This is performance hack, which should go away.
  ///
  /// \params choice The disjunction choice we are about to attempt.
  ///
  /// \returns true if disjunction step should be considered complete,
  ///          false otherwise.
  bool shouldStopAt(const DisjunctionChoice &choice) const override;
  bool shortCircuitDisjunctionAt(Constraint *currentChoice,
                                 Constraint *lastSuccessfulChoice) const;

  /// Attempt to apply given disjunction choice to constraint system.
  /// This action is going to establish "active choice" of this disjunction
  /// to point to a given choice.
  ///
  /// \param choice The choice to attempt.
  ///
  /// \return true if the choice has been accepted and system can be
  /// simplified further, false otherwise.
  bool attempt(const DisjunctionChoice &choice) override;

  // Check if selected disjunction has a representative
  // this might happen when there are multiple binary operators
  // chained together. If so, disable choices which differ
  // from currently selected representative.
  void pruneOverloadSet(Constraint *disjunction) {
    auto *choice = disjunction->getNestedConstraints().front();
    auto *typeVar = choice->getFirstType()->getAs<TypeVariableType>();
    if (!typeVar)
      return;

    auto *repr = typeVar->getImpl().getRepresentative(nullptr);
    if (!repr || repr == typeVar)
      return;

    for (auto *resolved = getResolvedOverloads(); resolved;
         resolved = resolved->Previous) {
      if (!resolved->BoundType->isEqual(repr))
        continue;

      auto &representative = resolved->Choice;
      if (!representative.isDecl())
        return;

      // Disable all of the overload choices which are different from
      // the one which is currently picked for representative.
      for (auto *constraint : disjunction->getNestedConstraints()) {
        auto choice = constraint->getOverloadChoice();
        if (!choice.isDecl() || choice.getDecl() == representative.getDecl())
          continue;

        constraint->setDisabled();
        DisabledChoices.push_back(constraint);
      }
      break;
    }
  };

  // Figure out which of the solutions has the smallest score.
  static Optional<Score> getBestScore(SmallVectorImpl<Solution> &solutions) {
    assert(!solutions.empty());
    Score bestScore = solutions.front().getFixedScore();
    if (solutions.size() == 1)
      return bestScore;

    for (unsigned i = 1, n = solutions.size(); i != n; ++i) {
      auto &score = solutions[i].getFixedScore();
      if (score < bestScore)
        bestScore = score;
    }
    return bestScore;
  }
};

} // end namespace constraints
} // end namespace swift

#endif // SWIFT_SEMA_CSSTEP_H
