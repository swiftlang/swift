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
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "TypeCheckType.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
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
#include "ConstraintSolverStats.def"

#undef DEBUG_TYPE
#define DEBUG_TYPE "Constraint solver largest system"
#define CS_STATISTIC(Name, Description) \
  STATISTIC(Largest##Name, Description);
#include "ConstraintSolverStats.def"
STATISTIC(LargestSolutionAttemptNumber, "# of the largest solution attempt");

TypeVariableType *ConstraintSystem::createTypeVariable(
                                     ConstraintLocator *locator,
                                     unsigned options) {
  ++TotalNumTypeVariables;
  auto tv = TypeVariableType::getNew(TC.Context, assignTypeVariableID(),
                                     locator, options);
  addTypeVariable(tv);
  return tv;
}

/// \brief Check whether the given type can be used as a binding for the given
/// type variable.
///
/// \returns the type to bind to, if the binding is okay.
Optional<Type> ConstraintSystem::checkTypeOfBinding(TypeVariableType *typeVar,
                                                    Type type,
                                                    bool *isNilLiteral) {
  if (!type)
    return None;

  // Simplify the type.
  type = simplifyType(type);

  // If the type references the type variable, don't permit the binding.
  SmallVector<TypeVariableType *, 4> referencedTypeVars;
  type->getTypeVariables(referencedTypeVars);
  if (count(referencedTypeVars, typeVar))
    return None;

  // If type variable is not allowed to bind to `lvalue`,
  // let's check if type of potential binding has any
  // type variables, which are allowed to bind to `lvalue`,
  // and postpone such type from consideration.
  if (!typeVar->getImpl().canBindToLValue()) {
    for (auto *typeVar : referencedTypeVars) {
      if (typeVar->getImpl().canBindToLValue())
        return None;
    }
  }

  // If the type is a type variable itself, don't permit the binding.
  if (auto bindingTypeVar = type->getRValueType()->getAs<TypeVariableType>()) {
    if (isNilLiteral) {
      *isNilLiteral = false;

      // Look for a literal-conformance constraint on the type variable.
      llvm::SetVector<Constraint *> constraints;
      getConstraintGraph().gatherConstraints(
          bindingTypeVar, constraints,
          ConstraintGraph::GatheringKind::EquivalenceClass,
          [](Constraint *constraint) -> bool {
            return constraint->getKind() == ConstraintKind::LiteralConformsTo &&
                   constraint->getProtocol()->isSpecificProtocol(
                       KnownProtocolKind::ExpressibleByNilLiteral);
          });

      for (auto constraint : constraints) {
        if (simplifyType(constraint->getFirstType())->isEqual(bindingTypeVar)) {
          *isNilLiteral = true;
          break;
        }
      }
    }
    
    return None;
  }

  // Don't bind to a dependent member type, even if it's currently
  // wrapped in any number of optionals, because binding producer
  // might unwrap and try to attempt it directly later.
  if (type->lookThroughAllOptionalTypes()->is<DependentMemberType>())
    return None;

  // Okay, allow the binding (with the simplified type).
  return type;
}

Solution ConstraintSystem::finalize() {
  assert(solverState);

  // Create the solution.
  Solution solution(*this, CurrentScore);

  // Update the best score we've seen so far.
  if (!retainAllSolutions()) {
    assert(TC.getLangOpts().DisableConstraintSolverPerformanceHacks ||
           !solverState->BestScore || CurrentScore <= *solverState->BestScore);

    if (!solverState->BestScore || CurrentScore <= *solverState->BestScore) {
      solverState->BestScore = CurrentScore;
    }
  }

  for (auto tv : TypeVariables) {
    if (getFixedType(tv))
      continue;

    switch (solverState->AllowFreeTypeVariables) {
    case FreeTypeVariableBinding::Disallow:
      llvm_unreachable("Solver left free type variables");

    case FreeTypeVariableBinding::Allow:
      break;
        
    case FreeTypeVariableBinding::UnresolvedType:
      assignFixedType(tv, TC.Context.TheUnresolvedType);
      break;
    }
  }

  // For each of the type variables, get its fixed type.
  for (auto tv : TypeVariables) {
    solution.typeBindings[tv] = simplifyType(tv)->reconstituteSugar(false);
  }

  // For each of the overload sets, get its overload choice.
  for (auto resolved = resolvedOverloadSets;
       resolved; resolved = resolved->Previous) {
    solution.overloadChoices[resolved->Locator]
      = { resolved->Choice, resolved->OpenedFullType, resolved->ImpliedType };
  }

  // For each of the constraint restrictions, record it with simplified,
  // canonical types.
  if (solverState) {
    for (auto &restriction : ConstraintRestrictions) {
      using std::get;
      CanType first = simplifyType(get<0>(restriction))->getCanonicalType();
      CanType second = simplifyType(get<1>(restriction))->getCanonicalType();
      solution.ConstraintRestrictions[{first, second}] = get<2>(restriction);
    }
  }

  // For each of the fixes, record it as an operation on the affected
  // expression.
  unsigned firstFixIndex = 0;
  if (solverState && solverState->PartialSolutionScope) {
    firstFixIndex = solverState->PartialSolutionScope->numFixes;
  }
  solution.Fixes.append(Fixes.begin() + firstFixIndex, Fixes.end());

  // Remember all the disjunction choices we made.
  for (auto &choice : DisjunctionChoices) {
    // We shouldn't ever register disjunction choices multiple times,
    // but saving and re-applying solutions can cause us to get
    // multiple entries.  We should use an optimized PartialSolution
    // structure for that use case, which would optimize a lot of
    // stuff here.
    assert(!solution.DisjunctionChoices.count(choice.first) ||
           solution.DisjunctionChoices[choice.first] == choice.second);
    solution.DisjunctionChoices.insert(choice);
  }

  // Remember the opened types.
  for (const auto &opened : OpenedTypes) {
    // We shouldn't ever register opened types multiple times,
    // but saving and re-applying solutions can cause us to get
    // multiple entries.  We should use an optimized PartialSolution
    // structure for that use case, which would optimize a lot of
    // stuff here.
    assert((solution.OpenedTypes.count(opened.first) == 0 ||
            solution.OpenedTypes[opened.first] == opened.second)
            && "Already recorded");
    solution.OpenedTypes.insert(opened);
  }

  // Remember the opened existential types.
  for (const auto &openedExistential : OpenedExistentialTypes) {
    assert(solution.OpenedExistentialTypes.count(openedExistential.first) == 0||
           solution.OpenedExistentialTypes[openedExistential.first]
             == openedExistential.second &&
           "Already recorded");
    solution.OpenedExistentialTypes.insert(openedExistential);
  }

  // Remember the defaulted type variables.
  solution.DefaultedConstraints.insert(DefaultedConstraints.begin(),
                                       DefaultedConstraints.end());

  for (auto &e : CheckedConformances)
    solution.Conformances.push_back({e.first, e.second});

  return solution;
}

void ConstraintSystem::applySolution(const Solution &solution) {
  // Update the score.
  CurrentScore += solution.getFixedScore();

  // Assign fixed types to the type variables solved by this solution.
  llvm::SmallPtrSet<TypeVariableType *, 4> 
    knownTypeVariables(TypeVariables.begin(), TypeVariables.end());
  for (auto binding : solution.typeBindings) {
    // If we haven't seen this type variable before, record it now.
    if (knownTypeVariables.insert(binding.first).second)
      TypeVariables.push_back(binding.first);

    // If we don't already have a fixed type for this type variable,
    // assign the fixed type from the solution.
    if (!getFixedType(binding.first) && !binding.second->hasTypeVariable())
      assignFixedType(binding.first, binding.second, /*updateState=*/false);
  }

  // Register overload choices.
  // FIXME: Copy these directly into some kind of partial solution?
  for (auto overload : solution.overloadChoices) {
    resolvedOverloadSets
      = new (*this) ResolvedOverloadSetListItem{resolvedOverloadSets,
                                                Type(),
                                                overload.second.choice,
                                                overload.first,
                                                overload.second.openedFullType,
                                                overload.second.openedType};    
  }

  // Register constraint restrictions.
  // FIXME: Copy these directly into some kind of partial solution?
  for (auto restriction : solution.ConstraintRestrictions) {
    ConstraintRestrictions.push_back(
        std::make_tuple(restriction.first.first, restriction.first.second,
                        restriction.second));
  }

  // Register the solution's disjunction choices.
  for (auto &choice : solution.DisjunctionChoices) {
    DisjunctionChoices.push_back(choice);
  }

  // Register the solution's opened types.
  for (const auto &opened : solution.OpenedTypes) {
    OpenedTypes.push_back(opened);
  }

  // Register the solution's opened existential types.
  for (const auto &openedExistential : solution.OpenedExistentialTypes) {
    OpenedExistentialTypes.push_back(openedExistential);
  }

  // Register the defaulted type variables.
  DefaultedConstraints.append(solution.DefaultedConstraints.begin(),
                              solution.DefaultedConstraints.end());

  // Register the conformances checked along the way to arrive to solution.
  for (auto &conformance : solution.Conformances)
    CheckedConformances.push_back(conformance);

  // Register any fixes produced along this path.
  Fixes.append(solution.Fixes.begin(), solution.Fixes.end());
}

/// \brief Restore the type variable bindings to what they were before
/// we attempted to solve this constraint system.
void ConstraintSystem::restoreTypeVariableBindings(unsigned numBindings) {
  auto &savedBindings = *getSavedBindings();
  std::for_each(savedBindings.rbegin(), savedBindings.rbegin() + numBindings,
                [](SavedTypeVariableBinding &saved) {
                  saved.restore();
                });
  savedBindings.erase(savedBindings.end() - numBindings,
                      savedBindings.end());
}

bool ConstraintSystem::simplify(bool ContinueAfterFailures) {
  // While we have a constraint in the worklist, process it.
  while (!ActiveConstraints.empty()) {
    // Grab the next constraint from the worklist.
    auto *constraint = &ActiveConstraints.front();
    ActiveConstraints.pop_front();
    assert(constraint->isActive() && "Worklist constraint is not active?");

    // Simplify this constraint.
    switch (simplifyConstraint(*constraint)) {
    case SolutionKind::Error:
      if (!failedConstraint) {
        failedConstraint = constraint;
      }

      if (TC.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(solverState ? solverState->depth * 2 : 0)
            << "(failed constraint ";
        constraint->print(log, &getASTContext().SourceMgr);
        log << ")\n";
      }

      if (solverState)
        solverState->retireConstraint(constraint);

      CG.removeConstraint(constraint);
      break;

    case SolutionKind::Solved:
      if (solverState) {
        ++solverState->NumSimplifiedConstraints;

        // This constraint has already been solved; retire it.
        solverState->retireConstraint(constraint);
      }

      // Remove the constraint from the constraint graph.
      CG.removeConstraint(constraint);
      break;

    case SolutionKind::Unsolved:
      if (solverState)
        ++solverState->NumUnsimplifiedConstraints;

      InactiveConstraints.push_back(constraint);
      break;
    }

    // This constraint is not active. We delay this operation until
    // after simplification to avoid re-insertion.
    constraint->setActive(false);

    // Check whether a constraint failed. If so, we're done.
    if (failedConstraint && !ContinueAfterFailures) {
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

/// \brief Truncate the given small vector to the given new size.
template<typename T>
void truncate(SmallVectorImpl<T> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  vec.erase(vec.begin() + newSize, vec.end());
}

} // end anonymous namespace

ConstraintSystem::SolverState::SolverState(
    Expr *const expr, ConstraintSystem &cs,
    FreeTypeVariableBinding allowFreeTypeVariables)
    : CS(cs), AllowFreeTypeVariables(allowFreeTypeVariables) {
  assert(!CS.solverState &&
         "Constraint system should not already have solver state!");
  CS.solverState = this;

  if (expr)
    ExprWeights = expr->getDepthMap();

  ++NumSolutionAttempts;
  SolutionAttempt = NumSolutionAttempts;

  // Record active constraints for re-activation at the end of lifetime.
  for (auto &constraint : cs.ActiveConstraints)
    activeConstraints.push_back(&constraint);

  // If we're supposed to debug a specific constraint solver attempt,
  // turn on debugging now.
  ASTContext &ctx = CS.getTypeChecker().Context;
  LangOptions &langOpts = ctx.LangOpts;
  OldDebugConstraintSolver = langOpts.DebugConstraintSolver;
  if (langOpts.DebugConstraintSolverAttempt &&
      langOpts.DebugConstraintSolverAttempt == SolutionAttempt) {
    langOpts.DebugConstraintSolver = true;
    llvm::raw_ostream &dbgOut = ctx.TypeCheckerDebug->getStream();
    dbgOut << "---Constraint system #" << SolutionAttempt << "---\n";
    CS.print(dbgOut);
  }
}

ConstraintSystem::SolverState::~SolverState() {
  assert((CS.solverState == this) &&
         "Expected constraint system to have this solver state!");
  CS.solverState = nullptr;

  // Make sure that all of the retired constraints have been returned
  // to constraint system.
  assert(!hasRetiredConstraints());

  // Make sure that all of the generated constraints have been handled.
  assert(generatedConstraints.empty());

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
    CS.ActiveConstraints.splice(CS.ActiveConstraints.end(),
                                CS.InactiveConstraints, constraint);

    constraint->setActive(true);
  }

  // Restore debugging state.
  LangOptions &langOpts = CS.getTypeChecker().Context.LangOpts;
  langOpts.DebugConstraintSolver = OldDebugConstraintSolver;

  // Write our local statistics back to the overall statistics.
  #define CS_STATISTIC(Name, Description) JOIN2(Overall,Name) += Name;
  #include "ConstraintSolverStats.def"

  // Update the "largest" statistics if this system is larger than the
  // previous one.  
  // FIXME: This is not at all thread-safe.
  if (NumStatesExplored > LargestNumStatesExplored.Value) {
    LargestSolutionAttemptNumber.Value = SolutionAttempt-1;
    ++LargestSolutionAttemptNumber;
    #define CS_STATISTIC(Name, Description) \
      JOIN2(Largest,Name).Value = Name-1; \
      ++JOIN2(Largest,Name);
    #include "ConstraintSolverStats.def"
  }
}

ConstraintSystem::SolverScope::SolverScope(ConstraintSystem &cs)
  : cs(cs), CGScope(cs.CG)
{
  resolvedOverloadSets = cs.resolvedOverloadSets;
  numTypeVariables = cs.TypeVariables.size();
  numSavedBindings = cs.solverState->savedBindings.size();
  numConstraintRestrictions = cs.ConstraintRestrictions.size();
  numFixes = cs.Fixes.size();
  numDisjunctionChoices = cs.DisjunctionChoices.size();
  numOpenedTypes = cs.OpenedTypes.size();
  numOpenedExistentialTypes = cs.OpenedExistentialTypes.size();
  numDefaultedConstraints = cs.DefaultedConstraints.size();
  numCheckedConformances = cs.CheckedConformances.size();
  PreviousScore = cs.CurrentScore;

  cs.solverState->registerScope(this);
  assert(!cs.failedConstraint && "Unexpected failed constraint!");
}

ConstraintSystem::SolverScope::~SolverScope() {
  // Erase the end of various lists.
  cs.resolvedOverloadSets = resolvedOverloadSets;
  truncate(cs.TypeVariables, numTypeVariables);

  // Restore bindings.
  cs.restoreTypeVariableBindings(cs.solverState->savedBindings.size() -
                                   numSavedBindings);

  // Move any remaining active constraints into the inactive list.
  if (!cs.ActiveConstraints.empty()) {
    for (auto &constraint : cs.ActiveConstraints) {
      constraint.setActive(false);
    }
    cs.InactiveConstraints.splice(cs.InactiveConstraints.end(),
                                  cs.ActiveConstraints);
  }

  // Rollback all of the changes done to constraints by the current scope,
  // e.g. add retired constraints back to the circulation and remove generated
  // constraints introduced by the current scope.
  cs.solverState->rollback(this);

  // Remove any constraint restrictions.
  truncate(cs.ConstraintRestrictions, numConstraintRestrictions);

  // Remove any fixes.
  truncate(cs.Fixes, numFixes);

  // Remove any disjunction choices.
  truncate(cs.DisjunctionChoices, numDisjunctionChoices);

  // Remove any opened types.
  truncate(cs.OpenedTypes, numOpenedTypes);

  // Remove any opened existential types.
  truncate(cs.OpenedExistentialTypes, numOpenedExistentialTypes);

  // Remove any defaulted type variables.
  truncate(cs.DefaultedConstraints, numDefaultedConstraints);

  // Remove any conformances checked along the current path.
  truncate(cs.CheckedConformances, numCheckedConformances);

  // Reset the previous score.
  cs.CurrentScore = PreviousScore;

  // Clear out other "failed" state.
  cs.failedConstraint = nullptr;
}

/// \brief Solve the system of constraints.
///
/// \param allowFreeTypeVariables How to bind free type variables in
/// the solution.
///
/// \returns a solution if a single unambiguous one could be found, or None if
/// ambiguous or unsolvable.
Optional<Solution>
ConstraintSystem::solveSingle(FreeTypeVariableBinding allowFreeTypeVariables,
                              bool allowFixes) {

  SolverState state(nullptr, *this, allowFreeTypeVariables);
  state.recordFixes = allowFixes;

  SmallVector<Solution, 4> solutions;
  solve(solutions);
  filterSolutions(solutions, state.ExprWeights);

  if (solutions.size() != 1)
    return Optional<Solution>();

  return std::move(solutions[0]);
}

bool ConstraintSystem::Candidate::solve(
    llvm::SmallDenseSet<OverloadSetRefExpr *> &shrunkExprs) {
  // Don't attempt to solve candidate if there is closure
  // expression involved, because it's handled specially
  // by parent constraint system (e.g. parameter lists).
  bool containsClosure = false;
  E->forEachChildExpr([&](Expr *childExpr) -> Expr * {
    if (isa<ClosureExpr>(childExpr)) {
      containsClosure = true;
      return nullptr;
    }
    return childExpr;
  });

  if (containsClosure)
    return false;

  auto cleanupImplicitExprs = [&](Expr *expr) {
    expr->forEachChildExpr([&](Expr *childExpr) -> Expr * {
      Type type = childExpr->getType();
      if (childExpr->isImplicit() && type && type->hasTypeVariable())
        childExpr->setType(Type());
      return childExpr;
    });
  };

  // Allocate new constraint system for sub-expression.
  ConstraintSystem cs(TC, DC, None);
  cs.baseCS = &BaseCS;

  // Set up expression type checker timer for the candidate.
  cs.Timer.emplace(E, cs);

  // Generate constraints for the new system.
  if (auto generatedExpr = cs.generateConstraints(E)) {
    E = generatedExpr;
  } else {
    // Failure to generate constraint system for sub-expression
    // means we can't continue solving sub-expressions.
    cleanupImplicitExprs(E);
    return true;
  }

  // If this candidate is too complex given the number
  // of the domains we have reduced so far, let's bail out early.
  if (isTooComplexGiven(&cs, shrunkExprs))
    return false;

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
    log << "--- Solving candidate for shrinking at ";
    auto R = E->getSourceRange();
    if (R.isValid()) {
      R.print(log, TC.Context.SourceMgr, /*PrintText=*/ false);
    } else {
      log << "<invalid range>";
    }
    log << " ---\n";

    E->dump(log);
    log << '\n';
    cs.print(log);
  }

  // If there is contextual type present, add an explicit "conversion"
  // constraint to the system.
  if (!CT.isNull()) {
    auto constraintKind = ConstraintKind::Conversion;
    if (CTP == CTP_CallArgument)
      constraintKind = ConstraintKind::ArgumentConversion;

    cs.addConstraint(constraintKind, cs.getType(E), CT,
                     cs.getConstraintLocator(E), /*isFavored=*/true);
  }

  // Try to solve the system and record all available solutions.
  llvm::SmallVector<Solution, 2> solutions;
  {
    SolverState state(E, cs, FreeTypeVariableBinding::Allow);

    // Use solve which doesn't try to filter solution list.
    // Because we want the whole set of possible domain choices.
    cs.solve(solutions);
  }

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
    if (solutions.empty()) {
      log << "--- No Solutions ---\n";
    } else {
      log << "--- Solutions ---\n";
      for (unsigned i = 0, n = solutions.size(); i != n; ++i) {
        auto &solution = solutions[i];
        log << "--- Solution #" << i << " ---\n";
        solution.dump(log);
      }
    }
  }

  // Record found solutions as suggestions.
  this->applySolutions(solutions, shrunkExprs);

  // Let's double-check if we have any implicit expressions
  // with type variables and nullify their types.
  cleanupImplicitExprs(E);

  // No solutions for the sub-expression means that either main expression
  // needs salvaging or it's inconsistent (read: doesn't have solutions).
  return solutions.empty();
}

void ConstraintSystem::Candidate::applySolutions(
    llvm::SmallVectorImpl<Solution> &solutions,
    llvm::SmallDenseSet<OverloadSetRefExpr *> &shrunkExprs) const {
  // A collection of OSRs with their newly reduced domains,
  // it's domains are sets because multiple solutions can have the same
  // choice for one of the type variables, and we want no duplication.
  llvm::SmallDenseMap<OverloadSetRefExpr *, llvm::SmallSet<ValueDecl *, 2>>
    domains;
  for (auto &solution : solutions) {
    for (auto choice : solution.overloadChoices) {
      // Some of the choices might not have locators.
      if (!choice.getFirst())
        continue;

      auto anchor = choice.getFirst()->getAnchor();
      // Anchor is not available or expression is not an overload set.
      if (!anchor || !isa<OverloadSetRefExpr>(anchor))
        continue;

      auto OSR = cast<OverloadSetRefExpr>(anchor);
      auto overload = choice.getSecond().choice;
      auto type = overload.getDecl()->getInterfaceType();

      // One of the solutions has polymorphic type assigned with one of it's
      // type variables. Such functions can only be properly resolved
      // via complete expression, so we'll have to forget solutions
      // we have already recorded. They might not include all viable overload
      // choices.
      if (type->is<GenericFunctionType>()) {
        return;
      }

      domains[OSR].insert(overload.getDecl());
    }
  }

  // Reduce the domains.
  for (auto &domain : domains) {
    auto OSR = domain.getFirst();
    auto &choices = domain.getSecond();

    // If the domain wasn't reduced, skip it.
    if (OSR->getDecls().size() == choices.size()) continue;

    // Update the expression with the reduced domain.
    MutableArrayRef<ValueDecl *> decls(
                                Allocator.Allocate<ValueDecl *>(choices.size()),
                                choices.size());

    std::uninitialized_copy(choices.begin(), choices.end(), decls.begin());
    OSR->setDecls(decls);

    // Record successfully shrunk expression.
    shrunkExprs.insert(OSR);
  }
}

void ConstraintSystem::shrink(Expr *expr) {
  if (TC.getLangOpts().SolverDisableShrink)
    return;

  using DomainMap = llvm::SmallDenseMap<Expr *, ArrayRef<ValueDecl *>>;

  // A collection of original domains of all of the expressions,
  // so they can be restored in case of failure.
  DomainMap domains;

  struct ExprCollector : public ASTWalker {
    Expr *PrimaryExpr;

    // The primary constraint system.
    ConstraintSystem &CS;

    // All of the sub-expressions which are suitable to be solved
    // separately from the main system e.g. binary expressions, collections,
    // function calls, coercions etc.
    llvm::SmallVector<Candidate, 4> Candidates;

    // Counts the number of overload sets present in the tree so far.
    // Note that the traversal is depth-first.
    llvm::SmallVector<std::pair<Expr *, unsigned>, 4> ApplyExprs;

    // A collection of original domains of all of the expressions,
    // so they can be restored in case of failure.
    DomainMap &Domains;

    ExprCollector(Expr *expr, ConstraintSystem &cs, DomainMap &domains)
        : PrimaryExpr(expr), CS(cs), Domains(domains) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *expr) override {
      // A dictionary expression is just a set of tuples; try to solve ones
      // that have overload sets.
      if (auto collectionExpr = dyn_cast<CollectionExpr>(expr)) {
        visitCollectionExpr(collectionExpr, CS.getContextualType(expr),
                            CS.getContextualTypePurpose());
        // Don't try to walk into the dictionary.
        return {false, expr};
      }

      // Let's not attempt to type-check closures or expressions
      // which constrain closures, because they require special handling
      // when dealing with context and parameters declarations.
      if (isa<ClosureExpr>(expr)) {
        return {false, expr};
      }

      if (auto coerceExpr = dyn_cast<CoerceExpr>(expr)) {
        if (coerceExpr->isLiteralInit())
          ApplyExprs.push_back({coerceExpr, 1});
        visitCoerceExpr(coerceExpr);
        return {false, expr};
      }

      if (auto OSR = dyn_cast<OverloadSetRefExpr>(expr)) {
        Domains[OSR] = OSR->getDecls();
      }

      if (auto applyExpr = dyn_cast<ApplyExpr>(expr)) {
        auto func = applyExpr->getFn();
        // Let's record this function application for post-processing
        // as well as if it contains overload set, see walkToExprPost.
        ApplyExprs.push_back(
            {applyExpr, isa<OverloadSetRefExpr>(func) || isa<TypeExpr>(func)});
      }

      return { true, expr };
    }

    /// Determine whether this is an arithmetic expression comprised entirely
    /// of literals.
    static bool isArithmeticExprOfLiterals(Expr *expr) {
      expr = expr->getSemanticsProvidingExpr();

      if (auto prefix = dyn_cast<PrefixUnaryExpr>(expr))
        return isArithmeticExprOfLiterals(prefix->getArg());

      if (auto postfix = dyn_cast<PostfixUnaryExpr>(expr))
        return isArithmeticExprOfLiterals(postfix->getArg());

      if (auto binary = dyn_cast<BinaryExpr>(expr))
        return isArithmeticExprOfLiterals(binary->getArg()->getElement(0)) &&
               isArithmeticExprOfLiterals(binary->getArg()->getElement(1));

      return isa<IntegerLiteralExpr>(expr) || isa<FloatLiteralExpr>(expr);
    }

    Expr *walkToExprPost(Expr *expr) override {
      auto isSrcOfPrimaryAssignment = [&](Expr *expr) -> bool {
        if (auto *AE = dyn_cast<AssignExpr>(PrimaryExpr))
          return expr == AE->getSrc();
        return false;
      };

      if (expr == PrimaryExpr || isSrcOfPrimaryAssignment(expr)) {
        // If this is primary expression and there are no candidates
        // to be solved, let's not record it, because it's going to be
        // solved regardless.
        if (Candidates.empty())
          return expr;

        auto contextualType = CS.getContextualType();
        // If there is a contextual type set for this expression.
        if (!contextualType.isNull()) {
          Candidates.push_back(Candidate(CS, PrimaryExpr, contextualType,
                                         CS.getContextualTypePurpose()));
          return expr;
        }

        // Or it's a function application with other candidates present.
        if (isa<ApplyExpr>(expr)) {
          Candidates.push_back(Candidate(CS, PrimaryExpr));
          return expr;
        }
      }

      if (!isa<ApplyExpr>(expr))
        return expr;

      unsigned numOverloadSets = 0;
      // Let's count how many overload sets do we have.
      while (!ApplyExprs.empty()) {
        auto &application = ApplyExprs.back();
        auto applyExpr = application.first;

        // Add overload sets tracked by current expression.
        numOverloadSets += application.second;
        ApplyExprs.pop_back();

        // We've found the current expression, so record the number of
        // overloads.
        if (expr == applyExpr) {
          ApplyExprs.push_back({applyExpr, numOverloadSets});
          break;
        }
      }

      // If there are fewer than two overloads in the chain
      // there is no point of solving this expression,
      // because we won't be able to reduce its domain.
      if (numOverloadSets > 1 && !isArithmeticExprOfLiterals(expr))
        Candidates.push_back(Candidate(CS, expr));

      return expr;
    }

  private:
    /// \brief Extract type of the element from given collection type.
    ///
    /// \param collection The type of the collection container.
    ///
    /// \returns Null type, ErrorType or UnresolvedType on failure,
    /// properly constructed type otherwise.
    Type extractElementType(Type collection) {
      auto &ctx = CS.getASTContext();
      if (!collection || collection->hasError())
        return collection;

      auto base = collection.getPointer();
      auto isInvalidType = [](Type type) -> bool {
        return type.isNull() || type->hasUnresolvedType() ||
               type->hasError();
      };

      // Array type.
      if (auto array = dyn_cast<ArraySliceType>(base)) {
        auto elementType = array->getBaseType();
        // If base type is invalid let's return error type.
        return elementType;
      }

      // Map or Set or any other associated collection type.
      if (auto boundGeneric = dyn_cast<BoundGenericType>(base)) {
        if (boundGeneric->hasUnresolvedType())
          return boundGeneric;

        llvm::SmallVector<TupleTypeElt, 2> params;
        for (auto &type : boundGeneric->getGenericArgs()) {
          // One of the generic arguments in invalid or unresolved.
          if (isInvalidType(type))
            return type;

          params.push_back(type);
        }

        // If there is just one parameter, let's return it directly.
        if (params.size() == 1)
          return params[0].getType();

        return TupleType::get(params, ctx);
      }

      return Type();
    }

    bool isSuitableCollection(TypeRepr *collectionTypeRepr) {
      // Only generic identifier, array or dictionary.
      switch (collectionTypeRepr->getKind()) {
      case TypeReprKind::GenericIdent:
      case TypeReprKind::Array:
      case TypeReprKind::Dictionary:
        return true;

      default:
        return false;
      }
    }

    void visitCoerceExpr(CoerceExpr *coerceExpr) {
      auto subExpr = coerceExpr->getSubExpr();
      // Coerce expression is valid only if it has sub-expression.
      if (!subExpr) return;

      unsigned numOverloadSets = 0;
      subExpr->forEachChildExpr([&](Expr *childExpr) -> Expr * {
        if (isa<OverloadSetRefExpr>(childExpr)) {
          ++numOverloadSets;
          return childExpr;
        }

        if (auto nestedCoerceExpr = dyn_cast<CoerceExpr>(childExpr)) {
          visitCoerceExpr(nestedCoerceExpr);
          // Don't walk inside of nested coercion expression directly,
          // that is be done by recursive call to visitCoerceExpr.
          return nullptr;
        }

        // If sub-expression we are trying to coerce to type is a collection,
        // let's allow collector discover it with assigned contextual type
        // of coercion, which allows collections to be solved in parts.
        if (auto collectionExpr = dyn_cast<CollectionExpr>(childExpr)) {
          auto castTypeLoc = coerceExpr->getCastTypeLoc();
          auto typeRepr = castTypeLoc.getTypeRepr();

          if (typeRepr && isSuitableCollection(typeRepr)) {
            // Clone representative to avoid modifying in-place,
            // FIXME: We should try and silently resolve the type here,
            // instead of cloning representative.
            auto coercionRepr = typeRepr->clone(CS.getASTContext());
            // Let's try to resolve coercion type from cloned representative.
            auto resolution = TypeResolution::forContextual(CS.DC);
            auto coercionType =
              resolution.resolveType(coercionRepr, None);

            // Looks like coercion type is invalid, let's skip this sub-tree.
            if (coercionType->hasError())
              return nullptr;

            // Visit collection expression inline.
            visitCollectionExpr(collectionExpr, coercionType,
                                CTP_CoerceOperand);
          }
        }

        return childExpr;
      });

      // It's going to be inefficient to try and solve
      // coercion in parts, so let's just make it a candidate directly,
      // if it contains at least a single overload set.

      if (numOverloadSets > 0)
        Candidates.push_back(Candidate(CS, coerceExpr));
    }

    void visitCollectionExpr(CollectionExpr *collectionExpr,
                             Type contextualType = Type(),
                             ContextualTypePurpose CTP = CTP_Unused) {
      // If there is a contextual type set for this collection,
      // let's propagate it to the candidate.
      if (!contextualType.isNull()) {
        auto elementType = extractElementType(contextualType);
        // If we couldn't deduce element type for the collection, let's
        // not attempt to solve it.
        if (!elementType ||
            elementType->hasError() ||
            elementType->hasUnresolvedType())
          return;

        contextualType = elementType;
      }

      for (auto element : collectionExpr->getElements()) {
        unsigned numOverloads = 0;
        element->walk(OverloadSetCounter(numOverloads));

        // There are no overload sets in the element; skip it.
        if (numOverloads == 0)
          continue;

        // Record each of the collection elements, which passed
        // number of overload sets rule, as a candidate for solving
        // with contextual type of the collection.
        Candidates.push_back(Candidate(CS, element, contextualType, CTP));
      }
    }
  };

  ExprCollector collector(expr, *this, domains);

  // Collect all of the binary/unary and call sub-expressions
  // so we can start solving them separately.
  expr->walk(collector);

  llvm::SmallDenseSet<OverloadSetRefExpr *> shrunkExprs;
  for (auto &candidate : collector.Candidates) {
    // If there are no results, let's forget everything we know about the
    // system so far. This actually is ok, because some of the expressions
    // might require manual salvaging.
    if (candidate.solve(shrunkExprs)) {
      // Let's restore all of the original OSR domains for this sub-expression,
      // this means that we can still make forward progress with solving of the
      // top sub-expressions.
      candidate.getExpr()->forEachChildExpr([&](Expr *childExpr) -> Expr * {
        if (auto OSR = dyn_cast<OverloadSetRefExpr>(childExpr)) {
          auto domain = domains.find(OSR);
          if (domain == domains.end())
            return childExpr;

          OSR->setDecls(domain->getSecond());
          shrunkExprs.erase(OSR);
        }

        return childExpr;
      });
    }
  }

  // Once "shrinking" is done let's re-allocate final version of
  // the candidate list to the permanent arena, so it could
  // survive even after primary constraint system is destroyed.
  for (auto &OSR : shrunkExprs) {
    auto choices = OSR->getDecls();
    auto decls = TC.Context.AllocateUninitialized<ValueDecl *>(choices.size());

    std::uninitialized_copy(choices.begin(), choices.end(), decls.begin());
    OSR->setDecls(decls);
  }
}

bool ConstraintSystem::solve(Expr *&expr,
                             Type convertType,
                             ExprTypeCheckListener *listener,
                             SmallVectorImpl<Solution> &solutions,
                             FreeTypeVariableBinding allowFreeTypeVariables) {
  // Attempt to solve the constraint system.
  auto solution = solveImpl(expr,
                            convertType,
                            listener,
                            solutions,
                            allowFreeTypeVariables);

  // The constraint system has failed
  if (solution == SolutionKind::Error)
    return true;

  // If the system is unsolved or there are multiple solutions present but
  // type checker options do not allow unresolved types, let's try to salvage
  if (solution == SolutionKind::Unsolved ||
      (solutions.size() != 1 &&
       !Options.contains(
           ConstraintSystemFlags::AllowUnresolvedTypeVariables))) {
    if (shouldSuppressDiagnostics())
      return true;

    // Try to provide a decent diagnostic.
    if (salvage(solutions, expr)) {
      // If salvage produced an error message, then it failed to salvage the
      // expression, just bail out having reported the error.
      return true;
    }

    // The system was salvaged; continue on as if nothing happened.
  }

  if (getExpressionTooComplex(solutions)) {
    TC.diagnose(expr->getLoc(), diag::expression_too_complex).
    highlight(expr->getSourceRange());
    return true;
  }

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    if (solutions.size() == 1) {
      log << "---Solution---\n";
      solutions[0].dump(log);
    } else {
      for (unsigned i = 0, e = solutions.size(); i != e; ++i) {
        log << "--- Solution #" << i << " ---\n";
        solutions[i].dump(log);
      }
    }
  }

  return false;
}

ConstraintSystem::SolutionKind
ConstraintSystem::solveImpl(Expr *&expr,
                            Type convertType,
                            ExprTypeCheckListener *listener,
                            SmallVectorImpl<Solution> &solutions,
                            FreeTypeVariableBinding allowFreeTypeVariables) {
  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Constraint solving for the expression at ";
    auto R = expr->getSourceRange();
    if (R.isValid()) {
      R.print(log, TC.Context.SourceMgr, /*PrintText=*/ false);
    } else {
      log << "<invalid range>";
    }
    log << "---\n";
  }

  assert(!solverState && "cannot be used directly");

  // Set up the expression type checker timer.
  Timer.emplace(expr, *this);

  // Try to shrink the system by reducing disjunction domains. This
  // goes through every sub-expression and generate its own sub-system, to
  // try to reduce the domains of those subexpressions.
  shrink(expr);

  // Generate constraints for the main system.
  if (auto generatedExpr = generateConstraints(expr))
    expr = generatedExpr;
  else {
    return SolutionKind::Error;
  }

  // If there is a type that we're expected to convert to, add the conversion
  // constraint.
  if (convertType) {
    auto constraintKind = ConstraintKind::Conversion;
    if (getContextualTypePurpose() == CTP_CallArgument)
      constraintKind = ConstraintKind::ArgumentConversion;

    // In a by-reference yield, we expect the contextual type to be an
    // l-value type, so the result must be bound to that.
    if (getContextualTypePurpose() == CTP_YieldByReference)
      constraintKind = ConstraintKind::Bind;

    auto *convertTypeLocator = getConstraintLocator(
        getConstraintLocator(expr), ConstraintLocator::ContextualType);

    if (allowFreeTypeVariables == FreeTypeVariableBinding::UnresolvedType) {
      convertType = convertType.transform([&](Type type) -> Type {
        if (type->is<UnresolvedType>())
          return createTypeVariable(convertTypeLocator);
        return type;
      });
    }

    addConstraint(constraintKind, getType(expr), convertType,
                  convertTypeLocator, /*isFavored*/ true);
  }

  // Notify the listener that we've built the constraint system.
  if (listener && listener->builtConstraints(*this, expr)) {
    return SolutionKind::Error;
  }

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto getTypeOfExpr = [&](const Expr *E) -> Type {
      if (hasType(E))
        return getType(E);
      return Type();
    };
    auto getTypeOfTypeLoc = [&](const TypeLoc &TL) -> Type {
      if (hasType(TL))
        return getType(TL);
      return Type();
    };

    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";

    expr->dump(log, getTypeOfExpr, getTypeOfTypeLoc);
    log << "\n";
    print(log);
  }

  // Try to solve the constraint system using computed suggestions.
  solve(expr, solutions, allowFreeTypeVariables);

  // If there are no solutions let's mark system as unsolved,
  // and solved otherwise even if there are multiple solutions still present.
  return solutions.empty() ? SolutionKind::Unsolved : SolutionKind::Solved;
}

bool ConstraintSystem::solve(Expr *const expr,
                             SmallVectorImpl<Solution> &solutions,
                             FreeTypeVariableBinding allowFreeTypeVariables) {
  // Set up solver state.
  SolverState state(expr, *this, allowFreeTypeVariables);

  // Solve the system.
  solve(solutions);

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Solver statistics---\n";
    log << "Total number of scopes explored: " << solverState->NumStatesExplored << "\n";
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
  if (!retainAllSolutions())
    filterSolutions(solutions, state.ExprWeights);

  // We fail if there is no solution or the expression was too complex.
  return solutions.empty() || getExpressionTooComplex(solutions);
}

void ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions) {
  assert(solverState);

  // If constraint system failed while trying to
  // genenerate constraints, let's stop right here.
  if (failedConstraint)
    return;

  // Allocate new solver scope, so constraint system
  // could be restored to its original state afterwards.
  // Otherwise there is a risk that some of the constraints
  // are not going to be re-introduced to the system.
  SolverScope scope(*this);

  SmallVector<std::unique_ptr<SolverStep>, 16> workList;
  // First step is always wraps whole constraint system.
  workList.push_back(llvm::make_unique<SplitterStep>(*this, solutions));

  // Indicate whether previous step in the stack has failed
  // (returned StepResult::Kind = Error), this is useful to
  // propagate failures when unsolved steps are re-taken.
  bool prevFailed = false;

  // Advance the solver by taking a given step, which might involve
  // a prelimilary "setup", if this is the first time this step is taken.
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
      switch (result.getKind()) {
      // It was impossible to solve this step, let's note that
      // for followup steps, to propogate the error.
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

void ConstraintSystem::collectDisjunctions(
    SmallVectorImpl<Constraint *> &disjunctions) {
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() == ConstraintKind::Disjunction)
      disjunctions.push_back(&constraint);
  }
}

// Attempt to find a disjunction of bind constraints where all options
// in the disjunction are binding the same type variable.
//
// Prefer disjunctions where the bound type variable is also the
// right-hand side of a conversion constraint, since having a concrete
// type that we're converting to can make it possible to split the
// constraint system into multiple ones.
static Constraint *selectBestBindingDisjunction(
    ConstraintSystem &cs, SmallVectorImpl<Constraint *> &disjunctions) {

  if (disjunctions.empty())
    return nullptr;

  auto getAsTypeVar = [&cs](Type type) {
    return cs.simplifyType(type)->getRValueType()->getAs<TypeVariableType>();
  };

  Constraint *firstBindDisjunction = nullptr;
  for (auto *disjunction : disjunctions) {
    auto choices = disjunction->getNestedConstraints();
    assert(!choices.empty());

    auto *choice = choices.front();
    if (choice->getKind() != ConstraintKind::Bind)
      continue;

    // We can judge disjunction based on the single choice
    // because all of choices (of bind overload set) should
    // have the same left-hand side.
    // Only do this for simple type variable bindings, not for
    // bindings like: ($T1) -> $T2 bind String -> Int
    auto *typeVar = getAsTypeVar(choice->getFirstType());
    if (!typeVar)
      continue;

    if (!firstBindDisjunction)
      firstBindDisjunction = disjunction;

    llvm::SetVector<Constraint *> constraints;
    cs.getConstraintGraph().gatherConstraints(
        typeVar, constraints, ConstraintGraph::GatheringKind::EquivalenceClass,
        [](Constraint *constraint) {
          return constraint->getKind() == ConstraintKind::Conversion;
        });

    for (auto *constraint : constraints) {
      if (typeVar == getAsTypeVar(constraint->getSecondType()))
        return disjunction;
    }
  }

  // If we had any binding disjunctions, return the first of
  // those. These ensure that we attempt to bind types earlier than
  // trying the elements of other disjunctions, which can often mean
  // we fail faster.
  return firstBindDisjunction;
}

// For a given type, collect any concrete types or literal
// conformances we can reach by walking the constraint graph starting
// from this point.
//
// For example, if the type is a type variable, we'll walk back
// through the constraints mentioning this type variable and find what
// types are converted to this type along with what literals are
// conformed-to by this type.
void ConstraintSystem::ArgumentInfoCollector::walk(Type argType) {
  llvm::SmallSet<TypeVariableType *, 4> visited;
  llvm::SmallVector<Type, 4> worklist;
  worklist.push_back(argType);

  while (!worklist.empty()) {
    auto itemTy = worklist.pop_back_val()->getRValueType();

    if (!itemTy->is<TypeVariableType>()) {
      addType(itemTy);
      continue;
    }

    auto tyvar = itemTy->castTo<TypeVariableType>();
    if (auto fixedTy = CS.getFixedType(tyvar)) {
      addType(fixedTy);
      continue;
    }

    auto *rep = CS.getRepresentative(tyvar);

    // FIXME: This can happen when we have two type variables that are
    // subtypes of each other. We would ideally merge those type
    // variables somewhere.
    if (visited.count(rep))
      continue;

    visited.insert(rep);

    llvm::SetVector<Constraint *> constraints;
    CS.getConstraintGraph().gatherConstraints(
        rep, constraints, ConstraintGraph::GatheringKind::EquivalenceClass);

    for (auto *constraint : constraints) {
      switch (constraint->getKind()) {
      case ConstraintKind::LiteralConformsTo:
        addLiteralProtocol(constraint->getProtocol());
        break;

      case ConstraintKind::Bind:
      case ConstraintKind::Equal: {
        auto firstTy = constraint->getFirstType();
        auto secondTy = constraint->getSecondType();
        if (firstTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(firstTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep))
            worklist.push_back(secondTy);
        }
        if (secondTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(secondTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep))
            worklist.push_back(firstTy);
        }
        break;
      }

      case ConstraintKind::Subtype:
      case ConstraintKind::OperatorArgumentConversion:
      case ConstraintKind::ArgumentConversion:
      case ConstraintKind::Conversion:
      case ConstraintKind::BridgingConversion:
      case ConstraintKind::BindParam: {
        auto secondTy = constraint->getSecondType();
        if (secondTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(secondTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep))
            worklist.push_back(constraint->getFirstType());
        }
        break;
      }

      case ConstraintKind::DynamicTypeOf:
      case ConstraintKind::EscapableFunctionOf: {
        auto firstTy = constraint->getFirstType();
        if (firstTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(firstTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep))
            worklist.push_back(constraint->getSecondType());
        }
        break;
      }

      case ConstraintKind::OptionalObject: {
        // Get the underlying object type.
        auto secondTy = constraint->getSecondType();
        if (secondTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(secondTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep)) {
            // See if we can actually determine what the underlying
            // type is.
            Type fixedTy;
            auto firstTy = constraint->getFirstType();
            if (!firstTy->is<TypeVariableType>()) {
              fixedTy = firstTy;
            } else {
              fixedTy = CS.getFixedType(firstTy->castTo<TypeVariableType>());
            }
            if (fixedTy && fixedTy->getOptionalObjectType())
              worklist.push_back(fixedTy->getOptionalObjectType());
          }
        }
        break;
      }

      case ConstraintKind::KeyPathApplication:
      case ConstraintKind::KeyPath: {
        auto firstTy = constraint->getFirstType();
        if (firstTy->is<TypeVariableType>()) {
          auto otherRep =
              CS.getRepresentative(firstTy->castTo<TypeVariableType>());
          if (otherRep->isEqual(rep))
            worklist.push_back(constraint->getThirdType());
        }
        break;
      }

      case ConstraintKind::BindToPointerType:
      case ConstraintKind::ValueMember:
      case ConstraintKind::UnresolvedValueMember:
      case ConstraintKind::Disjunction:
      case ConstraintKind::CheckedCast:
      case ConstraintKind::OpenedExistentialOf:
      case ConstraintKind::ApplicableFunction:
      case ConstraintKind::DynamicCallableApplicableFunction:
      case ConstraintKind::BindOverload:
      case ConstraintKind::FunctionInput:
      case ConstraintKind::FunctionResult:
      case ConstraintKind::SelfObjectOfProtocol:
      case ConstraintKind::ConformsTo:
      case ConstraintKind::Defaultable:
        break;
      }
    }
  }
}

void ConstraintSystem::ArgumentInfoCollector::minimizeLiteralProtocols() {
  if (LiteralProtocols.size() <= 1)
    return;

  llvm::SmallVector<Type, 2> defaultTypes;
  for (auto *protocol : LiteralProtocols)
    defaultTypes.push_back(CS.TC.getDefaultType(protocol, CS.DC));

  auto result = 0;
  for (unsigned long i = 1; i < LiteralProtocols.size(); ++i) {
    auto first =
        CS.TC.conformsToProtocol(defaultTypes[i], LiteralProtocols[result],
                                 CS.DC, ConformanceCheckFlags::InExpression);
    auto second =
        CS.TC.conformsToProtocol(defaultTypes[result], LiteralProtocols[i],
                                 CS.DC, ConformanceCheckFlags::InExpression);
    if ((first && second) || (!first && !second))
      return;

    if (first)
      result = i;
  }

  auto *protocol = LiteralProtocols[result];
  LiteralProtocols.clear();
  LiteralProtocols.insert(protocol);
}

void ConstraintSystem::ArgumentInfoCollector::dump() const {
  auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
  log << "types:\n";
  for (auto type : Types)
    type->print(log);
  log << "\n";

  log << "literal protocols:\n";
  for (auto *proto : LiteralProtocols)
    proto->print(log);
  log << "\n";
}

// Check to see if we know something about the types of all arguments
// in the given function type.
bool ConstraintSystem::haveTypeInformationForAllArguments(
    FunctionType *fnType) {
  llvm::SetVector<Constraint *> literalConformsTo;
  return llvm::all_of(fnType->getParams(),
                      [&](AnyFunctionType::Param param) -> bool {
                        ArgumentInfoCollector argInfo(*this, param);
                        auto countFacts = argInfo.getTypes().size() +
                                          argInfo.getLiteralProtocols().size();
                        return countFacts > 0;
                      });
}

// Given a type variable representing the RHS of an ApplicableFunction
// constraint, attempt to find the disjunction of bind overloads
// associated with it. This may return null in cases where have not
// yet created a disjunction because we need to resolve a base type,
// e.g.: [1].map{ ... } does not have a disjunction until we decide on
// a type for [1].
static Constraint *getUnboundBindOverloadDisjunction(TypeVariableType *tyvar,
                                                     ConstraintSystem &cs) {
  auto *rep = cs.getRepresentative(tyvar);
  assert(!cs.getFixedType(rep));

  llvm::SetVector<Constraint *> disjunctions;
  cs.getConstraintGraph().gatherConstraints(
      rep, disjunctions, ConstraintGraph::GatheringKind::EquivalenceClass,
      [](Constraint *match) {
        return match->getKind() == ConstraintKind::Disjunction &&
               match->getNestedConstraints().front()->getKind() ==
                   ConstraintKind::BindOverload;
      });

  if (disjunctions.empty())
    return nullptr;

  return disjunctions[0];
}

// Find a disjunction associated with an ApplicableFunction constraint
// where we have some information about all of the types of in the
// function application (even if we only know something about what the
// types conform to and not actually a concrete type).
Constraint *ConstraintSystem::selectApplyDisjunction() {
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() != ConstraintKind::ApplicableFunction)
      continue;

    auto *applicable = &constraint;
    if (haveTypeInformationForAllArguments(
            applicable->getFirstType()->castTo<FunctionType>())) {
      auto *tyvar = applicable->getSecondType()->castTo<TypeVariableType>();

      // If we have created the disjunction for this apply, find it.
      auto *disjunction = getUnboundBindOverloadDisjunction(tyvar, *this);
      if (disjunction)
        return disjunction;
    }
  }

  return nullptr;
}

static bool isOperatorBindOverload(Constraint *bindOverload) {
  if (bindOverload->getKind() != ConstraintKind::BindOverload)
    return false;

  auto choice = bindOverload->getOverloadChoice();
  if (!choice.isDecl())
    return false;

  auto *funcDecl = dyn_cast<FuncDecl>(choice.getDecl());
  return funcDecl && funcDecl->getOperatorDecl();
}

// Given a bind overload constraint for an operator, return the
// protocol designated as the first place to look for overloads of the
// operator.
static ArrayRef<NominalTypeDecl *>
getOperatorDesignatedNominalTypes(Constraint *bindOverload) {
  auto choice = bindOverload->getOverloadChoice();
  auto *funcDecl = cast<FuncDecl>(choice.getDecl());
  auto *operatorDecl = funcDecl->getOperatorDecl();
  return operatorDecl->getDesignatedNominalTypes();
}

void ConstraintSystem::sortDesignatedTypes(
    SmallVectorImpl<NominalTypeDecl *> &nominalTypes,
    Constraint *bindOverload) {
  auto *tyvar = bindOverload->getFirstType()->castTo<TypeVariableType>();
  llvm::SetVector<Constraint *> applicableFns;
  getConstraintGraph().gatherConstraints(
      tyvar, applicableFns, ConstraintGraph::GatheringKind::EquivalenceClass,
      [](Constraint *match) {
        return match->getKind() == ConstraintKind::ApplicableFunction;
      });

  // FIXME: This is not true when we run the constraint optimizer.
  // assert(applicableFns.size() <= 1);

  // We have a disjunction for an operator but no application of it,
  // so it's being passed as an argument.
  if (applicableFns.size() == 0)
    return;

  // FIXME: We have more than one applicable per disjunction as a
  //        result of merging disjunction type variables. We may want
  //        to rip that out at some point.
  Constraint *foundApplicable = nullptr;
  SmallVector<Optional<Type>, 2> argumentTypes;
  for (auto *applicableFn : applicableFns) {
    argumentTypes.clear();
    auto *fnTy = applicableFn->getFirstType()->castTo<FunctionType>();
    ArgumentInfoCollector argInfo(*this, fnTy);
    // Stop if we hit anything with concrete types or conformances to
    // literals.
    if (!argInfo.getTypes().empty() || !argInfo.getLiteralProtocols().empty()) {
      foundApplicable = applicableFn;
      break;
    }
  }

  if (!foundApplicable)
    return;

  // FIXME: It would be good to avoid this redundancy.
  auto *fnTy = foundApplicable->getFirstType()->castTo<FunctionType>();
  ArgumentInfoCollector argInfo(*this, fnTy);

  size_t nextType = 0;
  for (auto argType : argInfo.getTypes()) {
    auto *nominal = argType->getAnyNominal();
    for (size_t i = nextType + 1; i < nominalTypes.size(); ++i) {
      if (nominal == nominalTypes[i]) {
        std::swap(nominalTypes[nextType], nominalTypes[i]);
        ++nextType;
        break;
      }
    }
  }

  if (nextType + 1 >= nominalTypes.size())
    return;

  for (auto *protocol : argInfo.getLiteralProtocols()) {
    auto defaultType = TC.getDefaultType(protocol, DC);
    // ExpressibleByNilLiteral does not have a default type.
    if (!defaultType)
      continue;
    auto *nominal = defaultType->getAnyNominal();
    for (size_t i = nextType + 1; i < nominalTypes.size(); ++i) {
      if (nominal == nominalTypes[i]) {
        std::swap(nominalTypes[nextType], nominalTypes[i]);
        ++nextType;
        break;
      }
    }
  }
}

void ConstraintSystem::partitionForDesignatedTypes(
    ArrayRef<Constraint *> Choices, ConstraintMatchLoop forEachChoice,
    PartitionAppendCallback appendPartition) {

  auto types = getOperatorDesignatedNominalTypes(Choices[0]);
  if (types.empty())
    return;

  SmallVector<NominalTypeDecl *, 4> designatedNominalTypes(types.begin(),
                                                           types.end());

  if (designatedNominalTypes.size() > 1)
    sortDesignatedTypes(designatedNominalTypes, Choices[0]);

  SmallVector<SmallVector<unsigned, 4>, 4> definedInDesignatedType;
  SmallVector<SmallVector<unsigned, 4>, 4> definedInExtensionOfDesignatedType;

  auto examineConstraint =
    [&](unsigned constraintIndex, Constraint *constraint) -> bool {
    auto *decl = constraint->getOverloadChoice().getDecl();
    auto *funcDecl = cast<FuncDecl>(decl);

    auto *parentDC = funcDecl->getParent();
    auto *parentDecl = parentDC->getSelfNominalTypeDecl();

    for (auto designatedTypeIndex : indices(designatedNominalTypes)) {
      auto *designatedNominal =
        designatedNominalTypes[designatedTypeIndex];

      if (parentDecl != designatedNominal)
        continue;

      auto &constraints =
          isa<ExtensionDecl>(parentDC)
              ? definedInExtensionOfDesignatedType[designatedTypeIndex]
              : definedInDesignatedType[designatedTypeIndex];

      constraints.push_back(constraintIndex);
      return true;
    }

    return false;
  };

  definedInDesignatedType.resize(designatedNominalTypes.size());
  definedInExtensionOfDesignatedType.resize(designatedNominalTypes.size());

  forEachChoice(Choices, examineConstraint);

  // Now collect the overload choices that are defined within the type
  // that was designated in the operator declaration.
  // Add partitions for each of the overloads we found in types that
  // were designated as part of the operator declaration.
  for (auto designatedTypeIndex : indices(designatedNominalTypes)) {
    if (designatedTypeIndex < definedInDesignatedType.size()) {
      auto &primary = definedInDesignatedType[designatedTypeIndex];
      appendPartition(primary);
    }
    if (designatedTypeIndex < definedInExtensionOfDesignatedType.size()) {
      auto &secondary = definedInExtensionOfDesignatedType[designatedTypeIndex];
      appendPartition(secondary);
    }
  }
}

void ConstraintSystem::partitionDisjunction(
    ArrayRef<Constraint *> Choices, SmallVectorImpl<unsigned> &Ordering,
    SmallVectorImpl<unsigned> &PartitionBeginning) {
  // Maintain the original ordering, and make a single partition of
  // disjunction choices.
  auto originalOrdering = [&]() {
    for (unsigned long i = 0, e = Choices.size(); i != e; ++i)
      Ordering.push_back(i);

    PartitionBeginning.push_back(0);
  };

  if (!getASTContext().isSwiftVersionAtLeast(5) ||
      !TC.getLangOpts().SolverEnableOperatorDesignatedTypes ||
      !isOperatorBindOverload(Choices[0])) {
    originalOrdering();
    return;
  }

  SmallSet<Constraint *, 16> taken;

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

          assert(constraint->getKind() == ConstraintKind::BindOverload);
          assert(constraint->getOverloadChoice().isDecl());

          if (fn(index, constraint))
            taken.insert(constraint);
        }
      };

  // First collect some things that we'll generally put near the end
  // of the partitioning.

  SmallVector<unsigned, 4> disabled;
  SmallVector<unsigned, 4> unavailable;
  SmallVector<unsigned, 4> globalScope;

  // First collect disabled constraints.
  forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
    if (!constraint->isDisabled())
      return false;
    disabled.push_back(index);
    return true;
  });

  // Then unavailable constraints if we're skipping them.
  if (!shouldAttemptFixes()) {
    forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
      auto *decl = constraint->getOverloadChoice().getDecl();
      auto *funcDecl = cast<FuncDecl>(decl);

      if (!funcDecl->getAttrs().isUnavailable(getASTContext()))
        return false;

      unavailable.push_back(index);
      return true;
    });
  }

  // Collect everything at the global scope.
  forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
    auto *decl = constraint->getOverloadChoice().getDecl();
    auto *funcDecl = cast<FuncDecl>(decl);

    // Skip anything defined within a type.
    auto *parentDecl = funcDecl->getParent()->getAsDecl();
    if (parentDecl)
      return false;

    globalScope.push_back(index);
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

  partitionForDesignatedTypes(Choices, forEachChoice, appendPartition);

  SmallVector<unsigned, 4> everythingElse;
  // Gather the remaining options.
  forEachChoice(Choices, [&](unsigned index, Constraint *constraint) -> bool {
    everythingElse.push_back(index);
    return true;
  });
  appendPartition(everythingElse);

  // Now create the remaining partitions from what we previously collected.
  appendPartition(globalScope);
  appendPartition(unavailable);
  appendPartition(disabled);

  assert(Ordering.size() == Choices.size());
}

Constraint *ConstraintSystem::selectDisjunction() {
  SmallVector<Constraint *, 4> disjunctions;

  collectDisjunctions(disjunctions);
  if (disjunctions.empty())
    return nullptr;

  if (auto *disjunction = selectBestBindingDisjunction(*this, disjunctions))
    return disjunction;

  if (getASTContext().isSwiftVersionAtLeast(5))
    if (auto *disjunction = selectApplyDisjunction())
      return disjunction;

  // Pick the disjunction with the smallest number of active choices.
  auto minDisjunction =
      std::min_element(disjunctions.begin(), disjunctions.end(),
                       [&](Constraint *first, Constraint *second) -> bool {
                         return first->countActiveNestedConstraints() <
                                second->countActiveNestedConstraints();
                       });

  if (minDisjunction != disjunctions.end())
    return *minDisjunction;

  return nullptr;
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

  auto bindings = cs.getPotentialBindings(typeVar);
  if (bindings.InvolvesTypeVariables || bindings.Bindings.size() != 1)
    return;

  auto conversionType = bindings.Bindings[0].BindingType;
  llvm::SetVector<Constraint *> constraints;
  cs.CG.gatherConstraints(typeVar, constraints,
                          ConstraintGraph::GatheringKind::EquivalenceClass,
                          [](Constraint *constraint) -> bool {
                            switch (constraint->getKind()) {
                            case ConstraintKind::Conversion:
                            case ConstraintKind::Defaultable:
                            case ConstraintKind::ConformsTo:
                            case ConstraintKind::LiteralConformsTo:
                              return false;

                            default:
                              return true;
                            }
                          });

  if (constraints.empty())
    cs.addConstraint(ConstraintKind::Bind, typeVar, conversionType,
                     Choice->getLocator());
}
