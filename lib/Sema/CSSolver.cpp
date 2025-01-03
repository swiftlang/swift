//===--- CSSolver.cpp - Constraint Solver ---------------------------------===//
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
// This file implements the constraint solver used in the type checker.
//
//===----------------------------------------------------------------------===//
#include "CSStep.h"
#include "TypeCheckType.h"
#include "TypeChecker.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeWalker.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/SolutionResult.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <memory>
#include <tuple>

using namespace swift;
using namespace constraints;

//===----------------------------------------------------------------------===//
// Constraint solver statistics
//===----------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint solver overall"
#define JOIN2(X,Y) X##Y
STATISTIC(NumSolutionAttempts, "# of solution attempts");
STATISTIC(TotalNumTypeVariables, "# of type variables created");

#define CS_STATISTIC(Name, Description) \
  STATISTIC(Overall##Name, Description);
#include "swift/Sema/ConstraintSolverStats.def"

#undef DEBUG_TYPE
#define DEBUG_TYPE "Constraint solver largest system"
#define CS_STATISTIC(Name, Description) \
  STATISTIC(Largest##Name, Description);
#include "swift/Sema/ConstraintSolverStats.def"
STATISTIC(LargestSolutionAttemptNumber, "# of the largest solution attempt");

TypeVariableType *ConstraintSystem::createTypeVariable(
                                     ConstraintLocator *locator,
                                     unsigned options) {
  ++TotalNumTypeVariables;
  auto tv = TypeVariableType::getNew(getASTContext(), assignTypeVariableID(),
                                     locator, options);
  addTypeVariable(tv);
  return tv;
}

Solution ConstraintSystem::finalize() {
  assert(solverState);

  // Create the solution.
  Solution solution(*this, CurrentScore);

  // Update the best score we've seen so far.
  auto &ctx = getASTContext();
  assert(ctx.TypeCheckerOpts.DisableConstraintSolverPerformanceHacks ||
         !solverState->BestScore || CurrentScore <= *solverState->BestScore);

  if (!solverState->BestScore || CurrentScore <= *solverState->BestScore) {
    solverState->BestScore = CurrentScore;
  }

  for (auto tv : getTypeVariables()) {
    if (getFixedType(tv))
      continue;

    switch (solverState->AllowFreeTypeVariables) {
    case FreeTypeVariableBinding::Disallow:
      llvm_unreachable("Solver left free type variables");

    case FreeTypeVariableBinding::Allow:
      break;

    case FreeTypeVariableBinding::UnresolvedType:
      assignFixedType(tv, ctx.TheUnresolvedType);
      break;
    }
  }

  // For each of the type variables, get its fixed type.
  for (auto tv : getTypeVariables()) {
    // This type variable has no binding. Allowed only
    // when `FreeTypeVariableBinding::Allow` is set,
    // which is checked above.
    if (!getFixedType(tv)) {
      solution.typeBindings[tv] = Type();
      continue;
    }

    solution.typeBindings[tv] = simplifyType(tv)->reconstituteSugar(false);
  }

  // Copy over the resolved overloads.
  solution.overloadChoices.insert(ResolvedOverloads.begin(),
                                  ResolvedOverloads.end());

  // For each of the constraint restrictions, record it with simplified,
  // canonical types.
  if (solverState) {
    for (const auto &entry : ConstraintRestrictions) {
      const auto &types = entry.first;
      auto restriction = entry.second;

      CanType first = simplifyType(types.first)->getCanonicalType();
      CanType second = simplifyType(types.second)->getCanonicalType();
      solution.ConstraintRestrictions[{first, second}] = restriction;
    }
  }

  // For each of the fixes, record it as an operation on the affected
  // expression.
  unsigned firstFixIndex =
      (solverState ? solverState->numPartialSolutionFixes : 0);
  for (const auto &fix :
       llvm::make_range(Fixes.begin() + firstFixIndex, Fixes.end()))
    solution.Fixes.push_back(fix);

  for (const auto &fix : FixedRequirements) {
    solution.FixedRequirements.push_back(fix);
  }

  // Remember all the disjunction choices we made.
  for (auto &choice : DisjunctionChoices) {
    solution.DisjunctionChoices.insert(choice);
  }

  // Remember all the applied disjunctions.
  for (auto &choice : AppliedDisjunctions) {
    solution.AppliedDisjunctions.insert(choice);
  }

  // Remember all of the argument/parameter matching choices we made.
  for (auto &argumentMatch : argumentMatchingChoices) {
    auto inserted = solution.argumentMatchingChoices.insert(argumentMatch);
    assert(inserted.second || inserted.first->second == argumentMatch.second);
    (void)inserted;
  }

  // Remember implied results.
  for (auto impliedResult : ImpliedResults)
    solution.ImpliedResults.insert(impliedResult);

  // Remember the opened types.
  for (const auto &opened : OpenedTypes) {
    // We shouldn't ever register opened types multiple times,
    // but saving and re-applying solutions can cause us to get
    // multiple entries.  We should use an optimized PartialSolution
    // structure for that use case, which would optimize a lot of
    // stuff here.
#if false
    assert((solution.OpenedTypes.count(opened.first) == 0 ||
            solution.OpenedTypes[opened.first] == opened.second)
            && "Already recorded");
#endif
    solution.OpenedTypes.insert(opened);
  }

  // Remember the opened existential types.
  for (auto &openedExistential : OpenedExistentialTypes) {
    openedExistential.second = simplifyType(openedExistential.second)
        ->castTo<OpenedArchetypeType>();

    assert(solution.OpenedExistentialTypes.count(openedExistential.first) == 0||
           solution.OpenedExistentialTypes[openedExistential.first]
             == openedExistential.second &&
           "Already recorded");
    solution.OpenedExistentialTypes.insert(openedExistential);
  }

  for (const auto &expansion : OpenedPackExpansionTypes) {
    assert(solution.OpenedPackExpansionTypes.count(expansion.first) == 0 ||
           solution.OpenedPackExpansionTypes[expansion.first] ==
                   expansion.second &&
               "Already recorded");
    solution.OpenedPackExpansionTypes.insert(expansion);
  }

  // Remember the defaulted type variables.
  solution.DefaultedConstraints.insert(DefaultedConstraints.begin(),
                                       DefaultedConstraints.end());

  for (auto &nodeType : NodeTypes) {
    solution.nodeTypes.insert(nodeType);
  }
  for (auto &keyPathComponentType : KeyPathComponentTypes) {
    solution.keyPathComponentTypes.insert(keyPathComponentType);
  }

  // Remember key paths.
  for (const auto &keyPaths : KeyPaths) {
    solution.KeyPaths.insert(keyPaths);
  }

  // Remember contextual types.
  for (auto &entry : contextualTypes) {
    solution.contextualTypes.push_back({entry.first, entry.second.first});
  }

  for (auto &target : targets)
    solution.targets.insert(target);

  for (const auto &item : caseLabelItems)
    solution.caseLabelItems.insert(item);

  for (const auto &throwSite : potentialThrowSites)
    solution.potentialThrowSites.push_back(throwSite);

  for (const auto &pattern : exprPatterns)
    solution.exprPatterns.insert(pattern);

  for (const auto &param : isolatedParams)
    solution.isolatedParams.insert(param);

  for (auto closure : preconcurrencyClosures)
    solution.preconcurrencyClosures.insert(closure);

  for (const auto &transformed : resultBuilderTransformed) {
    solution.resultBuilderTransformed.insert(transformed);
  }

  for (const auto &appliedWrapper : appliedPropertyWrappers) {
    solution.appliedPropertyWrappers.insert(appliedWrapper);
  }

  // Remember implicit value conversions.
  for (const auto &valueConversion : ImplicitValueConversions) {
    solution.ImplicitValueConversions.push_back(valueConversion);
  }

  // Remember argument lists.
  for (const auto &argListMapping : ArgumentLists) {
    solution.argumentLists.insert(argListMapping);
  }

  for (const auto &implicitRoot : ImplicitCallAsFunctionRoots) {
    solution.ImplicitCallAsFunctionRoots.insert(implicitRoot);
  }

  for (const auto &env : PackExpansionEnvironments) {
    solution.PackExpansionEnvironments.insert(env);
  }

  for (const auto &packEnv : PackEnvironments)
    solution.PackEnvironments.insert(packEnv);

  for (const auto &synthesized : SynthesizedConformances) {
    solution.SynthesizedConformances.insert(synthesized);
  }

  return solution;
}

void ConstraintSystem::replaySolution(const Solution &solution,
                                      bool shouldIncreaseScore) {
  if (shouldIncreaseScore)
    replayScore(solution.getFixedScore());

  for (auto binding : solution.typeBindings) {
    // If we haven't seen this type variable before, record it now.
    addTypeVariable(binding.first);
  }

  // Assign fixed types to the type variables solved by this solution.
  for (auto binding : solution.typeBindings) {
    if (!binding.second)
      continue;

    // If we don't already have a fixed type for this type variable,
    // assign the fixed type from the solution.
    if (getFixedType(binding.first))
      continue;

    assignFixedType(binding.first, binding.second, /*updateState=*/false);
  }

  // Register overload choices.
  // FIXME: Copy these directly into some kind of partial solution?
  for (auto overload : solution.overloadChoices) {
    if (!ResolvedOverloads.count(overload.first))
      recordResolvedOverload(overload.first, overload.second);
  }

  // Register constraint restrictions.
  // FIXME: Copy these directly into some kind of partial solution?
  for ( auto &restriction : solution.ConstraintRestrictions) {
    auto type1 = restriction.first.first;
    auto type2 = restriction.first.second;

    addConversionRestriction(type1, type2, restriction.second);
  }

  // Register the solution's disjunction choices.
  for (auto &choice : solution.DisjunctionChoices) {
    if (DisjunctionChoices.count(choice.first) == 0)
      recordDisjunctionChoice(choice.first, choice.second);
  }

  // Register the solution's applied disjunctions.
  for (auto &choice : solution.AppliedDisjunctions) {
    if (AppliedDisjunctions.count(choice.first) == 0)
      recordAppliedDisjunction(choice.first, choice.second);
  }

  // Remember all of the argument/parameter matching choices we made.
  for (auto &argumentMatch : solution.argumentMatchingChoices) {
    if (argumentMatchingChoices.count(argumentMatch.first) == 0)
      recordMatchCallArgumentResult(argumentMatch.first, argumentMatch.second);
  }

  // Remember implied results.
  for (auto impliedResult : solution.ImpliedResults) {
    if (ImpliedResults.count(impliedResult.first) == 0)
      recordImpliedResult(impliedResult.first, impliedResult.second);
  }

  // Register the solution's opened types.
  for (const auto &opened : solution.OpenedTypes) {
    if (OpenedTypes.count(opened.first) == 0)
      recordOpenedType(opened.first, opened.second);
  }

  // Register the solution's opened existential types.
  for (const auto &openedExistential : solution.OpenedExistentialTypes) {
    if (OpenedExistentialTypes.count(openedExistential.first) == 0) {
      recordOpenedExistentialType(openedExistential.first,
                                  openedExistential.second);
    }
  }

  // Register the solution's opened pack expansion types.
  for (const auto &expansion : solution.OpenedPackExpansionTypes) {
    if (OpenedPackExpansionTypes.count(expansion.first) == 0)
      recordOpenedPackExpansionType(expansion.first, expansion.second);
  }

  // Register the solutions's pack expansion environments.
  for (const auto &expansion : solution.PackExpansionEnvironments) {
    if (PackExpansionEnvironments.count(expansion.first) == 0)
      recordPackExpansionEnvironment(expansion.first, expansion.second);
  }

  // Register the solutions's pack environments.
  for (auto &packEnvironment : solution.PackEnvironments) {
    if (PackEnvironments.count(packEnvironment.first) == 0)
      addPackEnvironment(packEnvironment.first, packEnvironment.second);
  }

  // Register the defaulted type variables.
  for (auto *locator : solution.DefaultedConstraints) {
    recordDefaultedConstraint(locator);
  }

  // Add the node types back.
  for (auto &nodeType : solution.nodeTypes) {
    setType(nodeType.first, nodeType.second);
  }

  for (auto &nodeType : solution.keyPathComponentTypes) {
    setType(nodeType.getFirst().first, nodeType.getFirst().second,
            nodeType.getSecond());
  }

  // Add key paths.
  for (const auto &keypath : solution.KeyPaths) {
    if (KeyPaths.count(keypath.first) == 0) {
      recordKeyPath(keypath.first,
                    std::get<0>(keypath.second),
                    std::get<1>(keypath.second),
                    std::get<2>(keypath.second));
    }
  }

  // Add the contextual types.
  for (const auto &contextualType : solution.contextualTypes) {
    if (!getContextualTypeInfo(contextualType.first))
      setContextualInfo(contextualType.first, contextualType.second);
  }

  // Register the statement condition targets.
  for (const auto &target : solution.targets) {
    if (!getTargetFor(target.first))
      setTargetFor(target.first, target.second);
  }

  // Register the statement condition targets.
  for (const auto &info : solution.caseLabelItems) {
    if (!getCaseLabelItemInfo(info.first))
      setCaseLabelItemInfo(info.first, info.second);
  }

  auto sites = ArrayRef(solution.potentialThrowSites);
  ASSERT(sites.size() >= potentialThrowSites.size());
  for (const auto &site : sites.slice(potentialThrowSites.size())) {
    potentialThrowSites.push_back(site);
  }

  for (auto param : solution.isolatedParams) {
    if (isolatedParams.count(param) == 0)
      recordIsolatedParam(param);
  }

  for (auto &pair : solution.exprPatterns) {
    if (exprPatterns.count(pair.first) == 0)
      setExprPatternFor(pair.first, pair.second);
  }

  for (auto closure : solution.preconcurrencyClosures) {
    if (preconcurrencyClosures.count(closure) == 0)
      recordPreconcurrencyClosure(closure);
  }

  for (const auto &transformed : solution.resultBuilderTransformed) {
    if (resultBuilderTransformed.count(transformed.first) == 0)
      recordResultBuilderTransform(transformed.first, transformed.second);
  }

  for (const auto &appliedWrapper : solution.appliedPropertyWrappers) {
    auto found = appliedPropertyWrappers.find(appliedWrapper.first);
    if (found == appliedPropertyWrappers.end()) {
      for (auto applied : appliedWrapper.second)
        applyPropertyWrapper(getAsExpr(appliedWrapper.first), applied);
    } else {
      ASSERT(found->second.size() == appliedWrapper.second.size());
    }
  }

  for (auto &valueConversion : solution.ImplicitValueConversions) {
    if (ImplicitValueConversions.count(valueConversion.first) == 0) {
      recordImplicitValueConversion(valueConversion.first,
                                    valueConversion.second);
    }
  }

  // Register the argument lists.
  for (auto &argListMapping : solution.argumentLists) {
    if (ArgumentLists.count(argListMapping.first) == 0)
      recordArgumentList(argListMapping.first, argListMapping.second);
  }

  for (auto &implicitRoot : solution.ImplicitCallAsFunctionRoots) {
    if (ImplicitCallAsFunctionRoots.count(implicitRoot.first) == 0)
      recordImplicitCallAsFunctionRoot(implicitRoot.first, implicitRoot.second);
  }

  for (auto &synthesized : solution.SynthesizedConformances) {
    if (SynthesizedConformances.count(synthesized.first) == 0)
      recordSynthesizedConformance(synthesized.first, synthesized.second);
  }

  // Register any fixes produced along this path.
  for (auto *fix : solution.Fixes) {
    if (Fixes.count(fix) == 0)
      addFix(fix);
  }

  // Register fixed requirements.
  for (auto fix : solution.FixedRequirements) {
    recordFixedRequirement(std::get<0>(fix),
                           std::get<1>(fix),
                           std::get<2>(fix));
  }
}
bool ConstraintSystem::simplify() {
  // While we have a constraint in the worklist, process it.
  while (!ActiveConstraints.empty()) {
    // Grab the next constraint from the worklist.
    auto *constraint = &ActiveConstraints.front();
    deactivateConstraint(constraint);

    auto isSimplifiable =
        constraint->getKind() != ConstraintKind::Disjunction &&
        constraint->getKind() != ConstraintKind::Conjunction;

    if (isDebugMode()) {
      auto &log = llvm::errs();
      log.indent(solverState->getCurrentIndent());
      log << "(considering: ";
      constraint->print(log, &getASTContext().SourceMgr,
                        solverState->getCurrentIndent());
      log << "\n";

      // {Dis, Con}junction are returned unsolved in \c simplifyConstraint() and
      // handled separately by solver steps.
      if (isSimplifiable) {
        log.indent(solverState->getCurrentIndent() + 2)
            << "(simplification result:\n";
      }
    }

    // Simplify this constraint.
    switch (simplifyConstraint(*constraint)) {
    case SolutionKind::Error:
      retireFailedConstraint(constraint);
      if (isDebugMode()) {
        auto &log = llvm::errs();
        if (isSimplifiable) {
          log.indent(solverState->getCurrentIndent() + 2) << ")\n";
        }
        log.indent(solverState->getCurrentIndent() + 2) << "(outcome: error)\n";
      }
      break;

    case SolutionKind::Solved:
      if (solverState)
        ++solverState->NumSimplifiedConstraints;
      retireConstraint(constraint);
      if (isDebugMode()) {
        auto &log = llvm::errs();
        if (isSimplifiable) {
          log.indent(solverState->getCurrentIndent() + 2) << ")\n";
        }
        log.indent(solverState->getCurrentIndent() + 2)
            << "(outcome: simplified)\n";
      }
      break;

    case SolutionKind::Unsolved:
      if (solverState)
        ++solverState->NumUnsimplifiedConstraints;
      if (isDebugMode()) {
        auto &log = llvm::errs();
        if (isSimplifiable) {
          log.indent(solverState->getCurrentIndent() + 2) << ")\n";
        }
        log.indent(solverState->getCurrentIndent() + 2)
            << "(outcome: unsolved)\n";
      }
      break;
    }

    if (isDebugMode()) {
      auto &log = llvm::errs();
      log.indent(solverState->getCurrentIndent()) << ")\n";
    }

    // Check whether a constraint failed. If so, we're done.
    if (failedConstraint) {
      return true;
    }

    // If the current score is worse than the best score we've seen so far,
    // there's no point in continuing. So don't.
    if (worseThanBestSolution()) {
      return true;
    }
  }

  return false;
}

namespace {

template<typename T>
void truncate(std::vector<T> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  vec.erase(vec.begin() + newSize, vec.end());
}

/// Truncate the given small vector to the given new size.
template<typename T>
void truncate(SmallVectorImpl<T> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  vec.erase(vec.begin() + newSize, vec.end());
}

template<typename T, unsigned N>
void truncate(llvm::SmallSetVector<T, N> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  for (unsigned i = 0, n = vec.size() - newSize; i != n; ++i)
    vec.pop_back();
}

template <typename K, typename V>
void truncate(llvm::MapVector<K, V> &map, unsigned newSize) {
  assert(newSize <= map.size() && "Not a truncation!");
  for (unsigned i = 0, n = map.size() - newSize; i != n; ++i)
    map.pop_back();
}

template <typename K, typename V, unsigned N>
void truncate(llvm::SmallMapVector<K, V, N> &map, unsigned newSize) {
  assert(newSize <= map.size() && "Not a truncation!");
  for (unsigned i = 0, n = map.size() - newSize; i != n; ++i)
    map.pop_back();
}

template <typename V>
void truncate(llvm::SetVector<V> &vector, unsigned newSize) {
  while (vector.size() > newSize)
    vector.pop_back();
}

} // end anonymous namespace

ConstraintSystem::SolverState::SolverState(
    ConstraintSystem &cs, FreeTypeVariableBinding allowFreeTypeVariables)
    : CS(cs), AllowFreeTypeVariables(allowFreeTypeVariables), Trail(cs) {
  assert(!CS.solverState &&
         "Constraint system should not already have solver state!");
  CS.solverState = this;

  ++NumSolutionAttempts;
  SolutionAttempt = NumSolutionAttempts;

  // Record active constraints for re-activation at the end of lifetime.
  for (auto &constraint : cs.ActiveConstraints)
    activeConstraints.push_back(&constraint);

  // If we're supposed to debug a specific constraint solver attempt,
  // turn on debugging now.
  ASTContext &ctx = CS.getASTContext();
  const auto &tyOpts = ctx.TypeCheckerOpts;
  if (tyOpts.DebugConstraintSolverAttempt &&
      tyOpts.DebugConstraintSolverAttempt == SolutionAttempt) {
    CS.Options |= ConstraintSystemFlags::DebugConstraints;
    llvm::errs().indent(CS.solverState->getCurrentIndent())
        << "---Constraint system #" << SolutionAttempt << "---\n";
    CS.print(llvm::errs());
  }
}

ConstraintSystem::SolverState::~SolverState() {
  assert((CS.solverState == this) &&
         "Expected constraint system to have this solver state!");
  CS.solverState = nullptr;

  // If constraint system ended up being in an invalid state
  // let's just drop the state without attempting to rollback.
  if (CS.inInvalidState())
    return;

  // Re-activate constraints which were initially marked as "active"
  // to restore original state of the constraint system.
  for (auto *constraint : activeConstraints) {
    // If the constraint is already active we can just move on.
    if (constraint->isActive())
      continue;

#ifndef NDEBUG
    // Make sure that constraint is present in the "inactive" set
    // before transferring it to "active".
    auto existing = llvm::find_if(CS.InactiveConstraints,
                                  [&constraint](const Constraint &inactive) {
                                    return &inactive == constraint;
                                  });
    assert(existing != CS.InactiveConstraints.end() &&
           "All constraints should be present in 'inactive' list");
#endif

    // Transfer the constraint to "active" set.
    CS.activateConstraint(constraint);
  }

  // If global constraint debugging is off and we are finished logging the
  // current solution attempt, switch debugging back off.
  const auto &tyOpts = CS.getASTContext().TypeCheckerOpts;
  if (!tyOpts.DebugConstraintSolver &&
      tyOpts.DebugConstraintSolverAttempt &&
      tyOpts.DebugConstraintSolverAttempt == SolutionAttempt) {
    CS.Options -= ConstraintSystemFlags::DebugConstraints;
  }

  // Write our local statistics back to the overall statistics.
  #define CS_STATISTIC(Name, Description) JOIN2(Overall,Name) += Name;
  #include "swift/Sema/ConstraintSolverStats.def"

#if LLVM_ENABLE_STATS
  // Update the "largest" statistics if this system is larger than the
  // previous one.  
  // FIXME: This is not at all thread-safe.
  if (NumSolverScopes > LargestNumSolverScopes.getValue()) {
    LargestSolutionAttemptNumber = SolutionAttempt-1;
    ++LargestSolutionAttemptNumber;
    #define CS_STATISTIC(Name, Description) \
      JOIN2(Largest,Name) = Name-1; \
      ++JOIN2(Largest,Name);
    #include "swift/Sema/ConstraintSolverStats.def"
  }
#endif
}

ConstraintSystem::SolverScope::SolverScope(ConstraintSystem &cs)
  : cs(cs),
    startTypeVariables(cs.TypeVariables.size()),
    startTrailSteps(cs.solverState->Trail.size()),
    scopeNumber(cs.solverState->beginScope()),
    moved(0) {
  ASSERT(!cs.failedConstraint && "Unexpected failed constraint!");
}

ConstraintSystem::SolverScope::SolverScope(SolverScope &&other)
  : cs(other.cs),
    startTypeVariables(other.startTypeVariables),
    startTrailSteps(other.startTrailSteps),
    scopeNumber(other.scopeNumber),
    moved(0) {
  other.moved = 1;
}

ConstraintSystem::SolverScope::~SolverScope() {
  if (moved)
    return;

  // Don't attempt to rollback from an incorrect state.
  if (cs.inInvalidState())
    return;

  // Roll back introduced type variables.
  truncate(cs.TypeVariables, startTypeVariables);

  // Move any remaining active constraints into the inactive list.
  if (!cs.ActiveConstraints.empty()) {
    for (auto &constraint : cs.ActiveConstraints) {
      constraint.setActive(false);
    }
    cs.InactiveConstraints.splice(cs.InactiveConstraints.end(),
                                  cs.ActiveConstraints);
  }

  uint64_t endTrailSteps = cs.solverState->Trail.size();

  // Roll back changes to the constraint system.
  cs.solverState->Trail.undo(startTrailSteps);

  // Update statistics.
  cs.solverState->endScope(scopeNumber,
                           startTrailSteps,
                           endTrailSteps);

  // Clear out other "failed" state.
  cs.failedConstraint = nullptr;
}

unsigned ConstraintSystem::SolverState::beginScope() {
  ++depth;
  maxDepth = std::max(maxDepth, depth);

  CS.incrementScopeCounter();

  return NumSolverScopes++;
}

/// Update statistics when a scope ends.
void ConstraintSystem::SolverState::endScope(unsigned scopeNumber,
                                             uint64_t startTrailSteps,
                                             uint64_t endTrailSteps) {
  ASSERT(depth > 0);
  --depth;

  NumTrailSteps += (endTrailSteps - startTrailSteps);

  unsigned countSolverScopes = NumSolverScopes - scopeNumber;
  if (countSolverScopes == 1)
    CS.incrementLeafScopes();
}

/// Solve the system of constraints.
///
/// \param allowFreeTypeVariables How to bind free type variables in
/// the solution.
///
/// \returns a solution if a single unambiguous one could be found, or None if
/// ambiguous or unsolvable.
std::optional<Solution>
ConstraintSystem::solveSingle(FreeTypeVariableBinding allowFreeTypeVariables,
                              bool allowFixes) {

  SolverState state(*this, allowFreeTypeVariables);
  state.recordFixes = allowFixes;

  SmallVector<Solution, 4> solutions;
  solveImpl(solutions);
  filterSolutions(solutions);

  if (solutions.size() != 1)
    return std::optional<Solution>();

  return std::move(solutions[0]);
}

static bool debugConstraintSolverForTarget(ASTContext &C,
                                           SyntacticElementTarget target) {
  if (C.TypeCheckerOpts.DebugConstraintSolver)
    return true;

  if (C.TypeCheckerOpts.DebugConstraintSolverOnLines.empty())
    // No need to compute the line number to find out it's not present.
    return false;

  // Get the lines on which the target starts and ends.
  unsigned startLine = 0, endLine = 0;
  SourceRange range = target.getSourceRange();
  if (range.isValid()) {
    auto charRange = Lexer::getCharSourceRangeFromSourceRange(C.SourceMgr, range);
    startLine =
        C.SourceMgr.getLineAndColumnInBuffer(charRange.getStart()).first;
    endLine = C.SourceMgr.getLineAndColumnInBuffer(charRange.getEnd()).first;
  }

  assert(startLine <= endLine && "expr ends before it starts?");

  auto &lines = C.TypeCheckerOpts.DebugConstraintSolverOnLines;
  assert(std::is_sorted(lines.begin(), lines.end()) &&
         "DebugConstraintSolverOnLines sorting invariant violated");

  // Check if `lines` contains at least one line `L` where
  // `startLine <= L <= endLine`. If it does, `lower_bound(startLine)` and
  // `upper_bound(endLine)` will be different.
  auto startBound = llvm::lower_bound(lines, startLine);
  auto endBound = std::upper_bound(startBound, lines.end(), endLine);

  return startBound != endBound;
}

std::optional<std::vector<Solution>>
ConstraintSystem::solve(SyntacticElementTarget &target,
                        FreeTypeVariableBinding allowFreeTypeVariables) {
  llvm::SaveAndRestore<ConstraintSystemOptions> debugForExpr(Options);
  if (debugConstraintSolverForTarget(getASTContext(), target)) {
    Options |= ConstraintSystemFlags::DebugConstraints;
  }

  /// Dump solutions for debugging purposes.
  auto dumpSolutions = [&](const SolutionResult &result) {
    // Debug-print the set of solutions.
    if (isDebugMode()) {
      auto &log = llvm::errs();
      auto indent = solverState ? solverState->getCurrentIndent() : 0;
      if (result.getKind() == SolutionResult::Success) {
        log << "\n";
        log.indent(indent) << "---Solution---\n";
        result.getSolution().dump(llvm::errs(), indent);
      } else if (result.getKind() == SolutionResult::Ambiguous) {
        auto solutions = result.getAmbiguousSolutions();
        for (unsigned i : indices(solutions)) {
          log << "\n";
          log.indent(indent) << "--- Solution #" << i << " ---\n";
          solutions[i].dump(llvm::errs(), indent);
        }
      }
    }
  };

  auto reportSolutionsToSolutionCallback = [&](const SolutionResult &result) {
    if (!getASTContext().SolutionCallback) {
      return;
    }
    switch (result.getKind()) {
    case SolutionResult::Success:
      getASTContext().SolutionCallback->sawSolution(result.getSolution());
      break;
    case SolutionResult::Ambiguous:
      for (auto &solution : result.getAmbiguousSolutions()) {
        getASTContext().SolutionCallback->sawSolution(solution);
      }
      break;
    default:
      break;
    }
  };

  // Take up to two attempts at solving the system. The first attempts to
  // solve a system that is expected to be well-formed, the second kicks in
  // when there is an error and attempts to salvage an ill-formed program.
  for (unsigned stage = 0; stage != 2; ++stage) {
    auto solution = (stage == 0)
        ? solveImpl(target, allowFreeTypeVariables)
        : salvage();

    switch (solution.getKind()) {
    case SolutionResult::Success: {
      // Return the successful solution.
      dumpSolutions(solution);
      reportSolutionsToSolutionCallback(solution);
      std::vector<Solution> result;
      result.push_back(std::move(solution).takeSolution());
      return std::move(result);
    }

    case SolutionResult::Error:
      maybeProduceFallbackDiagnostic(target);
      return std::nullopt;

    case SolutionResult::TooComplex: {
      auto affectedRange = solution.getTooComplexAt();

      // If affected range is unknown, let's use whole
      // target.
      if (!affectedRange)
        affectedRange = target.getSourceRange();

      getASTContext()
          .Diags.diagnose(affectedRange->Start, diag::expression_too_complex)
          .highlight(*affectedRange);

      solution.markAsDiagnosed();
      return std::nullopt;
    }

    case SolutionResult::Ambiguous:
      // If salvaging produced an ambiguous result, it has already been
      // diagnosed.
      // If we have found an ambiguous solution in the first stage, salvaging
      // won't produce more solutions, so we can inform the solution callback
      // about the current ambiguous solutions straight away.
      if (stage == 1 || Context.SolutionCallback) {
        reportSolutionsToSolutionCallback(solution);
        solution.markAsDiagnosed();
        return std::nullopt;
      }

      if (Options.contains(
            ConstraintSystemFlags::AllowUnresolvedTypeVariables)) {
        dumpSolutions(solution);
        auto ambiguousSolutions = std::move(solution).takeAmbiguousSolutions();
        std::vector<Solution> result(
            std::make_move_iterator(ambiguousSolutions.begin()),
            std::make_move_iterator(ambiguousSolutions.end()));
        return std::move(result);
      }

      LLVM_FALLTHROUGH;

    case SolutionResult::UndiagnosedError:
      if (stage == 1) {
        diagnoseFailureFor(target);
        reportSolutionsToSolutionCallback(solution);
        solution.markAsDiagnosed();
        return std::nullopt;
      }

      // Loop again to try to salvage.
      solution.markAsDiagnosed();
      continue;
    }
  }

  llvm_unreachable("Loop always returns");
}

SolutionResult
ConstraintSystem::solveImpl(SyntacticElementTarget &target,
                            FreeTypeVariableBinding allowFreeTypeVariables) {
  if (isDebugMode()) {
    auto &log = llvm::errs();
    log << "\n---Constraint solving at ";
    auto R = target.getSourceRange();
    if (R.isValid()) {
      R.print(log, getASTContext().SourceMgr, /*PrintText=*/ false);
    } else {
      log << "<invalid range>";
    }
    log << "---\n";
  }

  assert(!solverState && "cannot be used directly");

  // Set up the expression type checker timer.
  if (Expr *expr = target.getAsExpr())
    startExpressionTimer(expr);

  if (generateConstraints(target, allowFreeTypeVariables))
    return SolutionResult::forError();

  // Try to solve the constraint system using computed suggestions.
  SmallVector<Solution, 4> solutions;
  solve(solutions, allowFreeTypeVariables);

  if (isTooComplex(solutions))
    return SolutionResult::forTooComplex(getTooComplexRange());

  switch (solutions.size()) {
  case 0:
    return SolutionResult::forUndiagnosedError();

  case 1:
    return SolutionResult::forSolved(std::move(solutions.front()));

  default:
    return SolutionResult::forAmbiguous(solutions);
  }
}

bool ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions,
                             FreeTypeVariableBinding allowFreeTypeVariables) {
  // Set up solver state.
  SolverState state(*this, allowFreeTypeVariables);

  // Solve the system.
  solveImpl(solutions);

  if (isDebugMode()) {
    auto &log = llvm::errs();
    log << "\n---Solver statistics---\n";
    log << "Total number of scopes explored: " << solverState->NumSolverScopes << "\n";
    log << "Total number of trail steps: " << solverState->NumTrailSteps << "\n";
    log << "Maximum depth reached while exploring solutions: " << solverState->maxDepth << "\n";
    if (Timer) {
      auto timeInMillis =
        1000 * Timer->getElapsedProcessTimeInFractionalSeconds();
      log << "Time: " << timeInMillis << "ms\n";
    }
  }

  // Filter deduced solutions, try to figure out if there is
  // a single best solution to use, if not explicitly disabled
  // by constraint system options.
  filterSolutions(solutions);

  // We fail if there is no solution or the expression was too complex.
  return solutions.empty() || isTooComplex(solutions);
}

void ConstraintSystem::solveImpl(SmallVectorImpl<Solution> &solutions) {
  assert(solverState);

  setPhase(ConstraintSystemPhase::Solving);

  SWIFT_DEFER { setPhase(ConstraintSystemPhase::Finalization); };

  // If constraint system failed while trying to
  // genenerate constraints, let's stop right here.
  if (failedConstraint)
    return;

  // Attempt to solve a constraint system already in an invalid
  // state should be immediately aborted.
  if (inInvalidState()) {
    solutions.clear();
    return;
  }

  // Allocate new solver scope, so constraint system
  // could be restored to its original state afterwards.
  // Otherwise there is a risk that some of the constraints
  // are not going to be re-introduced to the system.
  SolverScope scope(*this);

  SmallVector<std::unique_ptr<SolverStep>, 16> workList;
  // First step is always wraps whole constraint system.
  workList.push_back(std::make_unique<SplitterStep>(*this, solutions));

  // Indicate whether previous step in the stack has failed
  // (returned StepResult::Kind = Error), this is useful to
  // propagate failures when unsolved steps are re-taken.
  bool prevFailed = false;

  // Advance the solver by taking a given step, which might involve
  // a preliminary "setup", if this is the first time this step is taken.
  auto advance = [](SolverStep *step, bool prevFailed) -> StepResult {
    auto currentState = step->getState();
    if (currentState == StepState::Setup) {
      step->setup();
      step->transitionTo(StepState::Ready);
    }

    currentState = step->getState();
    step->transitionTo(StepState::Running);
    return currentState == StepState::Ready ? step->take(prevFailed)
                                            : step->resume(prevFailed);
  };

  // Execute steps in LIFO order, which means that
  // each individual step would either end up producing
  // a solution, or producing another set of mergeable
  // steps to take before arriving to solution.
  while (!workList.empty()) {
    auto &step = workList.back();

    // Now let's try to advance to the next step or re-take previous,
    // which should produce another steps to follow,
    // or error, which means that current path is inconsistent.
    {
      auto result = advance(step.get(), prevFailed);

      // If execution of this step let constraint system in an
      // invalid state, let's drop all of the solutions and abort.
      if (inInvalidState()) {
        solutions.clear();
        return;
      }

      switch (result.getKind()) {
      // It was impossible to solve this step, let's note that
      // for followup steps, to propagate the error.
      case SolutionKind::Error:
        LLVM_FALLTHROUGH;

      // Step has been solved successfully by either
      // producing a partial solution, or more steps
      // toward that solution.
      case SolutionKind::Solved: {
        workList.pop_back();
        break;
      }

      // Keep this step in the work list to return to it
      // once all other steps are done, this could be a
      // disjunction which has to peek a new choice until
      // it completely runs out of choices, or type variable
      // binding.
      case SolutionKind::Unsolved:
        break;
      }

      prevFailed = result.getKind() == SolutionKind::Error;
      result.transfer(workList);
    }
  }
}

void ConstraintSystem::solveForCodeCompletion(
    SmallVectorImpl<Solution> &solutions) {
  {
    SolverState state(*this, FreeTypeVariableBinding::Disallow);

    // Enable "diagnostic mode" by default, this means that
    // solver would produce "fixed" solutions alongside valid
    // ones, which helps code completion to rank choices.
    state.recordFixes = true;

    solveImpl(solutions);
  }

  if (isDebugMode()) {
    auto &log = llvm::errs();
    auto indent = solverState ? solverState->getCurrentIndent() : 0;
    log.indent(indent) << "--- Discovered " << solutions.size()
                       << " solutions ---\n";
    for (const auto &solution : solutions) {
      log.indent(indent) << "--- Solution ---\n";
      solution.dump(log, indent);
    }
  }

  return;
}

bool ConstraintSystem::solveForCodeCompletion(
    SyntacticElementTarget &target, SmallVectorImpl<Solution> &solutions) {
  if (auto *expr = target.getAsExpr()) {
    // Tell the constraint system what the contextual type is.
    setContextualInfo(expr, target.getExprContextualTypeInfo());

    // Set up the expression type checker timer.
    startExpressionTimer(expr);
  }

  if (isDebugMode()) {
    auto &log = llvm::errs();
    log.indent(solverState ? solverState->getCurrentIndent() : 0)
        << "--- Code Completion ---\n";
  }

  if (generateConstraints(target))
    return false;

  solveForCodeCompletion(solutions);
  return true;
}

void ConstraintSystem::collectDisjunctions(
    SmallVectorImpl<Constraint *> &disjunctions) {
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() == ConstraintKind::Disjunction)
      disjunctions.push_back(&constraint);
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::filterDisjunction(
    Constraint *disjunction, bool restoreOnFail,
    llvm::function_ref<bool(Constraint *)> pred) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction);

  SmallVector<Constraint *, 4> constraintsToRestoreOnFail;
  unsigned choiceIdx = 0;
  unsigned numEnabledTerms = 0;
  ASTContext &ctx = getASTContext();
  for (unsigned constraintIdx : indices(disjunction->getNestedConstraints())) {
    auto constraint = disjunction->getNestedConstraints()[constraintIdx];

    // Skip already-disabled constraints. Let's treat disabled
    // choices which have a fix as "enabled" ones here, so we can
    // potentially infer some type information from them.
    if (constraint->isDisabled() && !constraint->getFix())
      continue;

    if (pred(constraint)) {
      ++numEnabledTerms;
      choiceIdx = constraintIdx;
      continue;
    }

    if (isDebugMode()) {
      auto indent = (solverState ? solverState->getCurrentIndent() : 0) + 4;
      llvm::errs().indent(indent) << "(disabled disjunction term ";
      constraint->print(llvm::errs(), &ctx.SourceMgr, indent);
      llvm::errs().indent(indent) << ")\n";
    }

    if (!constraint->isDisabled()) {
      if (restoreOnFail)
        constraintsToRestoreOnFail.push_back(constraint);
      else if (solverState)
        solverState->disableConstraint(constraint);
      else
        constraint->setDisabled();
    }
  }

  if (numEnabledTerms == 0)
    return SolutionKind::Error;

  if (restoreOnFail) {
    for (auto constraint : constraintsToRestoreOnFail) {
      if (solverState)
        solverState->disableConstraint(constraint);
      else
        constraint->setDisabled();
    }
  }

  if (numEnabledTerms == 1) {
    // Only a single constraint remains. Retire the disjunction and make
    // the remaining constraint active.
    auto choice = disjunction->getNestedConstraints()[choiceIdx];

    // This can only happen when subscript syntax is used to lookup
    // something which doesn't exist in type marked with
    // `@dynamicMemberLookup`.
    // Since filtering currently runs as part of the `applicable function`
    // constraint processing, "keypath dynamic member lookup" choice can't
    // be attempted in-place because that would also try to operate on that
    // constraint, so instead let's keep the disjunction, but disable all
    // unviable choices.
    if (choice->getOverloadChoice().isKeyPathDynamicMemberLookup()) {
      // Early simplification of the "keypath dynamic member lookup" choice
      // is impossible because it requires constraints associated with
      // subscript index expression to be present.
      if (Phase == ConstraintSystemPhase::ConstraintGeneration)
        return SolutionKind::Unsolved;

      for (auto *currentChoice : disjunction->getNestedConstraints()) {
        if (currentChoice->isDisabled())
          continue;

        if (currentChoice != choice)
          solverState->disableConstraint(currentChoice);
      }
      return SolutionKind::Solved;
    }

    // Retire the disjunction. It's been solved.
    retireConstraint(disjunction);

    // Note the choice we made and simplify it. This introduces the
    // new constraint into the system.
    if (disjunction->shouldRememberChoice()) {
      recordDisjunctionChoice(disjunction->getLocator(), choiceIdx);
    }

    if (isDebugMode()) {
      auto indent = (solverState ? solverState->getCurrentIndent() : 0) + 4;
      llvm::errs().indent(indent)
          << "(introducing single enabled disjunction term ";
      choice->print(llvm::errs(), &ctx.SourceMgr, indent);
      llvm::errs().indent(indent) << ")\n";
    }

    simplifyDisjunctionChoice(choice);

    return failedConstraint ? SolutionKind::Unsolved : SolutionKind::Solved;
  }

  return SolutionKind::Unsolved;
}


std::optional<std::pair<Constraint *, unsigned>>
ConstraintSystem::findConstraintThroughOptionals(
    TypeVariableType *typeVar, OptionalWrappingDirection optionalDirection,
    llvm::function_ref<bool(Constraint *, TypeVariableType *)> predicate) {
  unsigned numOptionals = 0;
  auto *rep = getRepresentative(typeVar);

  SmallPtrSet<TypeVariableType *, 4> visitedVars;
  while (visitedVars.insert(rep).second) {
    // Look for a disjunction that binds this type variable to an overload set.
    TypeVariableType *optionalObjectTypeVar = nullptr;
    auto constraints = getConstraintGraph().gatherConstraints(
        rep, ConstraintGraph::GatheringKind::EquivalenceClass,
        [&](Constraint *match) {
          // If we have an "optional object of" constraint, we may need to
          // look through it to find the constraint we're looking for.
          if (match->getKind() != ConstraintKind::OptionalObject)
            return predicate(match, rep);

          switch (optionalDirection) {
          case OptionalWrappingDirection::Promote: {
            // We want to go from T to T?, so check if we're on the RHS, and
            // move over to the LHS if we can.
            auto rhsTypeVar = match->getSecondType()->getAs<TypeVariableType>();
            if (rhsTypeVar && getRepresentative(rhsTypeVar) == rep) {
              optionalObjectTypeVar =
                  match->getFirstType()->getAs<TypeVariableType>();
            }
            break;
          }
          case OptionalWrappingDirection::Unwrap: {
            // We want to go from T? to T, so check if we're on the LHS, and
            // move over to the RHS if we can.
            auto lhsTypeVar = match->getFirstType()->getAs<TypeVariableType>();
            if (lhsTypeVar && getRepresentative(lhsTypeVar) == rep) {
              optionalObjectTypeVar =
                  match->getSecondType()->getAs<TypeVariableType>();
            }
            break;
          }
          }
          // Don't include the optional constraint in the results.
          return false;
        });

    // If we found a result, return it.
    if (!constraints.empty())
      return std::make_pair(constraints[0], numOptionals);

    // If we found an "optional object of" constraint, follow it.
    if (optionalObjectTypeVar && !getFixedType(optionalObjectTypeVar)) {
      numOptionals += 1;
      rep = getRepresentative(optionalObjectTypeVar);
      continue;
    }

    // Otherwise we're done.
    return std::nullopt;
  }
  return std::nullopt;
}

Constraint *ConstraintSystem::getUnboundBindOverloadDisjunction(
    TypeVariableType *tyvar, unsigned *numOptionalUnwraps) {
  assert(!getFixedType(tyvar));
  auto result = findConstraintThroughOptionals(
      tyvar, OptionalWrappingDirection::Promote,
      [&](Constraint *match, TypeVariableType *currentRep) {
        // Check to see if we have a bind overload disjunction that binds the
        // type var we need.
        if (match->getKind() != ConstraintKind::Disjunction ||
            match->getNestedConstraints().front()->getKind() !=
                ConstraintKind::BindOverload)
          return false;

        auto lhsTy = match->getNestedConstraints().front()->getFirstType();
        auto *lhsTyVar = lhsTy->getAs<TypeVariableType>();
        return lhsTyVar && currentRep == getRepresentative(lhsTyVar);
      });
  if (!result)
    return nullptr;

  if (numOptionalUnwraps)
    *numOptionalUnwraps = result->second;

  return result->first;
}

// Performance hack: if there are two generic overloads, and one is
// more specialized than the other, prefer the more-specialized one.
static Constraint *
tryOptimizeGenericDisjunction(ConstraintSystem &cs, Constraint *disjunction,
                              ArrayRef<Constraint *> constraints) {
  auto *dc = cs.DC;

  // If we're solving for code completion, and have a child completion token,
  // skip this optimization since the completion token being a placeholder can
  // allow us to prefer an unhelpful disjunction choice.
  if (cs.isForCodeCompletion()) {
    auto anchor = disjunction->getLocator()->getAnchor();
    if (cs.containsIDEInspectionTarget(cs.includingParentApply(anchor)))
      return nullptr;
  }

  // Don't attempt this optimization if call has number literals.
  // This is intended to narrowly fix situations like:
  //
  // func test<T: FloatingPoint>(_: T) { ... }
  // func test<T: Numeric>(_: T) { ... }
  //
  // test(42)
  //
  // The call should use `<T: Numeric>` overload even though the
  // `<T: FloatingPoint>` is a more specialized version because
  // selecting `<T: Numeric>` doesn't introduce non-default literal
  // types.
  if (auto *argFnType = cs.getAppliedDisjunctionArgumentFunction(disjunction)) {
    if (llvm::any_of(
            argFnType->getParams(), [](const AnyFunctionType::Param &param) {
              auto *typeVar = param.getPlainType()->getAs<TypeVariableType>();
              return typeVar && typeVar->getImpl().isNumberLiteralType();
            }))
      return nullptr;
  }

  llvm::SmallVector<Constraint *, 4> choices;
  for (auto *choice : constraints) {
    if (choices.size() > 2)
      return nullptr;

    if (!choice->isDisabled())
      choices.push_back(choice);
  }

  if (choices.size() != 2)
    return nullptr;

  if (choices[0]->getKind() != ConstraintKind::BindOverload ||
      choices[1]->getKind() != ConstraintKind::BindOverload ||
      choices[0]->isFavored() ||
      choices[1]->isFavored())
    return nullptr;

  OverloadChoice choiceA = choices[0]->getOverloadChoice();
  OverloadChoice choiceB = choices[1]->getOverloadChoice();

  if (!choiceA.isDecl() || !choiceB.isDecl())
    return nullptr;

  auto isViable = [](ValueDecl *decl) -> bool {
    assert(decl);

    auto *AFD = dyn_cast<AbstractFunctionDecl>(decl);
    if (!AFD || !AFD->isGeneric())
      return false;

    if (AFD->getAttrs().hasAttribute<DisfavoredOverloadAttr>())
      return false;

    auto funcType = AFD->getInterfaceType();
    auto hasAnyOrOptional = funcType.findIf([](Type type) -> bool {
      if (type->getOptionalObjectType())
        return true;

      return type->isAny();
    });

    // If function declaration references `Any` or an optional type,
    // let's not attempt it, because it's unclear
    // without solving which overload is going to be better.
    return !hasAnyOrOptional;
  };

  auto *declA = choiceA.getDecl();
  auto *declB = choiceB.getDecl();

  if (!isViable(declA) || !isViable(declB))
    return nullptr;

  switch (TypeChecker::compareDeclarations(dc, declA, declB)) {
  case Comparison::Better:
    return choices[0];

  case Comparison::Worse:
    return choices[1];

  case Comparison::Unordered:
    return nullptr;
  }
  llvm_unreachable("covered switch");
}

/// Populates the \c found vector with the indices of the given constraints
/// that have a matching type to an existing operator binding elsewhere in
/// the expression.
///
/// Operator bindings that have a matching type to an existing binding
/// are attempted first by the solver because it's very common to chain
/// operators of the same type together.
static void existingOperatorBindingsForDisjunction(ConstraintSystem &CS,
                                                   ArrayRef<Constraint *> constraints,
                                                   SmallVectorImpl<unsigned> &found) {
  auto *choice = constraints.front();
  if (choice->getKind() != ConstraintKind::BindOverload)
    return;

  auto overload = choice->getOverloadChoice();
  if (!overload.isDecl())
    return;
  auto decl = overload.getDecl();
  if (!decl->isOperator())
    return;

  // For concrete operators, consider overloads that have the same type as
  // an existing binding, because it's very common to write mixed operator
  // expressions where all operands have the same type, e.g. `(x + 10) / 2`.
  // For generic operators, only favor an exact overload that has already
  // been bound, because mixed operator expressions are far less common, and
  // computing generic canonical types is expensive.
  SmallSet<CanType, 4> concreteTypesFound;
  SmallSet<ValueDecl *, 4> genericDeclsFound;
  for (auto overload : CS.getResolvedOverloads()) {
    auto resolved = overload.second;
    if (!resolved.choice.isDecl())
      continue;

    auto representativeDecl = resolved.choice.getDecl();
    if (!representativeDecl->isOperator())
      continue;

    auto interfaceType = representativeDecl->getInterfaceType();
    if (interfaceType->is<GenericFunctionType>()) {
      genericDeclsFound.insert(representativeDecl);
    } else {
      concreteTypesFound.insert(interfaceType->getCanonicalType());
    }
  }

  for (auto index : indices(constraints)) {
    auto *constraint = constraints[index];
    if (constraint->isFavored())
      continue;

    auto *decl = constraint->getOverloadChoice().getDecl();
    auto interfaceType = decl->getInterfaceType();
    bool isGeneric = interfaceType->is<GenericFunctionType>();
    if ((isGeneric && genericDeclsFound.count(decl)) ||
        (!isGeneric && concreteTypesFound.count(interfaceType->getCanonicalType())))
      found.push_back(index);
  }
}

void DisjunctionChoiceProducer::partitionGenericOperators(
    SmallVectorImpl<unsigned>::iterator first,
    SmallVectorImpl<unsigned>::iterator last) {
  auto *argFnType = CS.getAppliedDisjunctionArgumentFunction(Disjunction);
  if (!isOperatorDisjunction(Disjunction) || !argFnType)
    return;

  auto operatorName = Choices[0]->getOverloadChoice().getName();
  if (!operatorName.getBaseIdentifier().isArithmeticOperator())
    return;

  SmallVector<unsigned, 4> concreteOverloads;
  SmallVector<unsigned, 4> numericOverloads;
  SmallVector<unsigned, 4> sequenceOverloads;
  SmallVector<unsigned, 4> simdOverloads;
  SmallVector<unsigned, 4> otherGenericOverloads;

  auto &ctx = CS.getASTContext();

  auto *additiveArithmeticProto = ctx.getProtocol(KnownProtocolKind::AdditiveArithmetic);
  auto *sequenceProto = ctx.getProtocol(KnownProtocolKind::Sequence);
  auto *simdProto = ctx.getProtocol(KnownProtocolKind::SIMD);

  auto conformsTo = [&](Type type, ProtocolDecl *protocol) -> bool {
    return protocol && bool(CS.lookupConformance(type, protocol));
  };

  auto refinesOrConformsTo = [&](NominalTypeDecl *nominal, ProtocolDecl *protocol) -> bool {
    if (!nominal || !protocol)
      return false;

    if (auto *refined = dyn_cast<ProtocolDecl>(nominal))
      return refined->inheritsFrom(protocol);

    return conformsTo(nominal->getDeclaredInterfaceType(), protocol);
  };

  // Gather Numeric and Sequence overloads into separate buckets.
  for (auto iter = first; iter != last; ++iter) {
    unsigned index = *iter;
    auto *decl = Choices[index]->getOverloadChoice().getDecl();
    auto *nominal = decl->getDeclContext()->getSelfNominalTypeDecl();

    if (isSIMDOperator(decl)) {
      simdOverloads.push_back(index);
    } else if (!decl->getInterfaceType()->is<GenericFunctionType>()) {
      concreteOverloads.push_back(index);
    } else if (refinesOrConformsTo(nominal, additiveArithmeticProto)) {
      numericOverloads.push_back(index);
    } else if (refinesOrConformsTo(nominal, sequenceProto)) {
      sequenceOverloads.push_back(index);
    } else {
      otherGenericOverloads.push_back(index);
    }
  }

  auto sortPartition = [&](SmallVectorImpl<unsigned> &partition) {
    llvm::sort(partition, [&](unsigned lhs, unsigned rhs) -> bool {
      auto *declA =
          dyn_cast<ValueDecl>(Choices[lhs]->getOverloadChoice().getDecl());
      auto *declB =
          dyn_cast<ValueDecl>(Choices[rhs]->getOverloadChoice().getDecl());

      return TypeChecker::isDeclRefinementOf(declA, declB);
    });
  };

  // Sort sequence overloads so that refinements are attempted first.
  // If the solver finds a solution with an overload, it can then skip
  // subsequent choices that the successful choice is a refinement of.
  sortPartition(sequenceOverloads);

  // Attempt concrete overloads first.
  first = std::copy(concreteOverloads.begin(), concreteOverloads.end(), first);

  // Check if any of the known argument types conform to one of the standard
  // arithmetic protocols. If so, the solver should attempt the corresponding
  // overload choices first.
  for (auto arg : argFnType->getParams()) {
    auto argType = arg.getPlainType();
    argType = CS.getFixedTypeRecursive(argType, /*wantRValue=*/true);

    if (argType->isTypeVariableOrMember())
      continue;

    if (conformsTo(argType, additiveArithmeticProto)) {
      first =
          std::copy(numericOverloads.begin(), numericOverloads.end(), first);
      numericOverloads.clear();
      break;
    }

    if (conformsTo(argType, sequenceProto)) {
      first =
          std::copy(sequenceOverloads.begin(), sequenceOverloads.end(), first);
      sequenceOverloads.clear();
      break;
    }

    if (conformsTo(argType, simdProto)) {
      first = std::copy(simdOverloads.begin(), simdOverloads.end(), first);
      simdOverloads.clear();
      break;
    }
  }

  first = std::copy(otherGenericOverloads.begin(), otherGenericOverloads.end(), first);
  first = std::copy(numericOverloads.begin(), numericOverloads.end(), first);
  first = std::copy(sequenceOverloads.begin(), sequenceOverloads.end(), first);
  first = std::copy(simdOverloads.begin(), simdOverloads.end(), first);
}

void DisjunctionChoiceProducer::partitionDisjunction(
    SmallVectorImpl<unsigned> &Ordering,
    SmallVectorImpl<unsigned> &PartitionBeginning) {
  // Apply a special-case rule for favoring one generic function over
  // another.
  if (auto favored = tryOptimizeGenericDisjunction(CS, Disjunction, Choices)) {
    CS.favorConstraint(favored);
  }

  SmallSet<Constraint *, 16> taken;

  using ConstraintMatcher = std::function<bool(unsigned index, Constraint *)>;
  using ConstraintMatchLoop =
      std::function<void(ArrayRef<Constraint *>, ConstraintMatcher)>;
  using PartitionAppendCallback =
      std::function<void(SmallVectorImpl<unsigned> & options)>;

  // Local function used to iterate over the untaken choices from the
  // disjunction and use a higher-order function to determine if they
  // should be part of a partition.
  ConstraintMatchLoop forEachChoice =
      [&](ArrayRef<Constraint *>,
          std::function<bool(unsigned index, Constraint *)> fn) {
        for (auto index : indices(Choices)) {
          auto *constraint = Choices[index];
          if (taken.count(constraint))
            continue;

          if (fn(index, constraint))
            taken.insert(constraint);
        }
      };

  // First collect some things that we'll generally put near the beginning or
  // end of the partitioning.
  SmallVector<unsigned, 4> favored;
  SmallVector<unsigned, 4> everythingElse;
  SmallVector<unsigned, 4> simdOperators;
  SmallVector<unsigned, 4> disabled;
  SmallVector<unsigned, 4> unavailable;

  // Add existing operator bindings to the main partition first. This often
  // helps the solver find a solution fast.
  existingOperatorBindingsForDisjunction(CS, Choices, everythingElse);
  for (auto index : everythingElse)
    taken.insert(Choices[index]);

  // First collect disabled and favored constraints.
  forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
    if (constraint->isDisabled()) {
      disabled.push_back(index);
      return true;
    }

    if (constraint->isFavored()) {
      favored.push_back(index);
      return true;
    }

    // Order VarDecls before other kinds of declarations because they are
    // effectively favored over functions when the two are in the same
    // overload set. This disjunction order allows SK_UnappliedFunction
    // to prune later overload choices that are functions when a solution
    // has already been found with a property.
    if (auto *decl = getOverloadChoiceDecl(constraint)) {
      if (isa<VarDecl>(decl)) {
        everythingElse.push_back(index);
        return true;
      }
    }

    return false;
  });

  // Then unavailable constraints if we're skipping them.
  if (!CS.shouldAttemptFixes()) {
    forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
      if (constraint->getKind() != ConstraintKind::BindOverload)
        return false;

      auto *decl = constraint->getOverloadChoice().getDeclOrNull();
      auto *funcDecl = dyn_cast_or_null<FuncDecl>(decl);
      if (!funcDecl)
        return false;

      if (!CS.isDeclUnavailable(funcDecl, constraint->getLocator()))
        return false;

      unavailable.push_back(index);
      return true;
    });
  }

  // Partition SIMD operators.
  if (isOperatorDisjunction(Disjunction) &&
      !Choices[0]->getOverloadChoice().getName().getBaseIdentifier().isArithmeticOperator()) {
    forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
      if (isSIMDOperator(constraint->getOverloadChoice().getDecl())) {
        simdOperators.push_back(index);
        return true;
      }

      return false;
    });
  }

  // Gather the remaining options.
  forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
    everythingElse.push_back(index);
    return true;
  });

  // Local function to create the next partition based on the options
  // passed in.
  PartitionAppendCallback appendPartition =
      [&](SmallVectorImpl<unsigned> &options) {
        if (options.size()) {
          PartitionBeginning.push_back(Ordering.size());
          Ordering.insert(Ordering.end(), options.begin(), options.end());
        }
      };

  appendPartition(favored);
  appendPartition(everythingElse);
  appendPartition(simdOperators);
  appendPartition(unavailable);
  appendPartition(disabled);

  assert(Ordering.size() == Choices.size());
}

Constraint *ConstraintSystem::selectConjunction() {
  SmallVector<Constraint *, 4> conjunctions;
  for (auto &constraint : InactiveConstraints) {
    if (constraint.isDisabled())
      continue;

    if (constraint.getKind() == ConstraintKind::Conjunction)
      conjunctions.push_back(&constraint);
  }

  if (conjunctions.empty())
    return nullptr;

  auto &SM = getASTContext().SourceMgr;

  // Conjunctions should be solved in order of their apperance in the source.
  // This is important because once a conjunction is solved, we don't re-visit
  // it, so we need to make sure we don't solve it before another conjuntion
  // that could provide it with necessary type information. Source order
  // provides an easy to reason about and quick way of establishing this.
  return *std::min_element(
      conjunctions.begin(), conjunctions.end(),
      [&](Constraint *conjunctionA, Constraint *conjunctionB) {
        auto *locA = conjunctionA->getLocator();
        auto *locB = conjunctionB->getLocator();
        if (!(locA && locB))
          return false;

        auto anchorA = locA->getAnchor();
        auto anchorB = locB->getAnchor();
        if (!(anchorA && anchorB))
          return false;

        auto slocA = anchorA.getStartLoc();
        auto slocB = anchorB.getStartLoc();
        if (!(slocA.isValid() && slocB.isValid()))
          return false;

        return SM.isBeforeInBuffer(slocA, slocB);
      });
}

bool DisjunctionChoice::attempt(ConstraintSystem &cs) const {
  cs.simplifyDisjunctionChoice(Choice);

  if (ExplicitConversion)
    propagateConversionInfo(cs);

  // Attempt to simplify current choice might result in
  // immediate failure, which is recorded in constraint system.
  return !cs.failedConstraint && !cs.simplify();
}

bool DisjunctionChoice::isGenericOperator() const {
  auto *decl = getOperatorDecl(Choice);
  if (!decl)
    return false;

  auto interfaceType = decl->getInterfaceType();
  return interfaceType->is<GenericFunctionType>();
}

bool DisjunctionChoice::isSymmetricOperator() const {
  auto *decl = getOperatorDecl(Choice);
  if (!decl)
    return false;

  auto func = dyn_cast<FuncDecl>(decl);
  auto paramList = func->getParameters();
  if (paramList->size() != 2)
    return true;

  auto firstType = paramList->get(0)->getInterfaceType();
  auto secondType = paramList->get(1)->getInterfaceType();
  return firstType->isEqual(secondType);
}

bool DisjunctionChoice::isUnaryOperator() const {
  auto *decl = getOperatorDecl(Choice);
  if (!decl)
    return false;

  auto func = cast<FuncDecl>(decl);
  return func->getParameters()->size() == 1;
}

void DisjunctionChoice::propagateConversionInfo(ConstraintSystem &cs) const {
  assert(ExplicitConversion);

  auto LHS = Choice->getFirstType();
  auto typeVar = LHS->getAs<TypeVariableType>();
  if (!typeVar)
    return;

  // Use the representative (if any) to lookup constraints
  // and potentially bind the coercion type to.
  typeVar = typeVar->getImpl().getRepresentative(nullptr);

  // If the representative already has a type assigned to it
  // we can't really do anything here.
  if (typeVar->getImpl().getFixedType(nullptr))
    return;

  auto bindings = cs.getBindingsFor(typeVar);

  auto numBindings =
      bindings.Bindings.size() + bindings.getNumViableLiteralBindings();
  if (bindings.isHole() || bindings.involvesTypeVariables() || numBindings != 1)
    return;

  Type conversionType;

  // There is either a single direct/transitive binding, or
  // a single literal default.
  if (!bindings.Bindings.empty()) {
    conversionType = bindings.Bindings[0].BindingType;
  } else {
    for (const auto &literal : bindings.Literals) {
      if (literal.second.viableAsBinding()) {
        conversionType = literal.second.getDefaultType();
        break;
      }
    }
  }

  auto constraints = cs.CG.gatherConstraints(
      typeVar,
      ConstraintGraph::GatheringKind::EquivalenceClass,
      [](Constraint *constraint) -> bool {
        switch (constraint->getKind()) {
        case ConstraintKind::Conversion:
        case ConstraintKind::Defaultable:
        case ConstraintKind::ConformsTo:
        case ConstraintKind::LiteralConformsTo:
        case ConstraintKind::TransitivelyConformsTo:
          return false;

        default:
          return true;
        }
      });

  if (constraints.empty())
    cs.addConstraint(ConstraintKind::Bind, typeVar, conversionType,
                     Choice->getLocator());
}

bool ConjunctionElement::attempt(ConstraintSystem &cs) const {
  // First, let's bring all referenced variables into scope.
  {
    llvm::SmallPtrSet<TypeVariableType *, 4> referencedVars;
    findReferencedVariables(cs, referencedVars);

    for (auto *typeVar : referencedVars)
      cs.addTypeVariable(typeVar);
  }

  auto result = cs.simplifyConstraint(*Element);
  return result != ConstraintSystem::SolutionKind::Error;
}
