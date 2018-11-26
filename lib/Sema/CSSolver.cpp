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
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/TypeWalker.h"
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

  // Don't bind to a dependent member type.
  if (type->is<DependentMemberType>()) return None;

  // Okay, allow the binding (with the simplified type).
  return type;
}

Solution ConstraintSystem::finalize(
           FreeTypeVariableBinding allowFreeTypeVariables) {
  // Create the solution.
  Solution solution(*this, CurrentScore);

  // Update the best score we've seen so far.
  if (solverState && !retainAllSolutions()) {
    assert(!solverState->BestScore || CurrentScore <= *solverState->BestScore);
    solverState->BestScore = CurrentScore;
  }

  for (auto tv : TypeVariables) {
    if (getFixedType(tv))
      continue;

    switch (allowFreeTypeVariables) {
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

/// \brief Enumerates all of the 'direct' supertypes of the given type.
///
/// The direct supertype S of a type T is a supertype of T (e.g., T < S)
/// such that there is no type U where T < U and U < S.
static SmallVector<Type, 4> 
enumerateDirectSupertypes(TypeChecker &tc, Type type) {
  SmallVector<Type, 4> result;

  if (auto tupleTy = type->getAs<TupleType>()) {
    // A tuple that can be constructed from a scalar has a value of that
    // scalar type as its supertype.
    // FIXME: There is a way more general property here, where we can drop
    // one label from the tuple, maintaining the rest.
    int scalarIdx = tupleTy->getElementForScalarInit();
    if (scalarIdx >= 0) {
      auto &elt = tupleTy->getElement(scalarIdx);
      if (elt.isVararg()) // FIXME: Should we keep the name?
        result.push_back(elt.getVarargBaseTy());
      else if (elt.hasName())
        result.push_back(elt.getType());
    }
  }

  if (type->mayHaveSuperclass()) {
    // FIXME: Can also weaken to the set of protocol constraints, but only
    // if there are any protocols that the type conforms to but the superclass
    // does not.

    // If there is a superclass, it is a direct supertype.
    if (auto superclass = tc.getSuperClassOf(type))
      result.push_back(superclass);
  }

  if (!type->isMaterializable())
    result.push_back(type->getWithoutSpecifierType());

  // FIXME: lots of other cases to consider!
  return result;
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

ConstraintSystem::SolverState::SolverState(Expr *const expr,
                                           ConstraintSystem &cs)
    : CS(cs) {
  assert(!CS.solverState &&
         "Constraint system should not already have solver state!");
  CS.solverState = this;

  if (expr)
    ExprWeights = expr->getDepthMap();

  ++NumSolutionAttempts;
  SolutionAttempt = NumSolutionAttempts;

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

/// \brief Try each of the given type variable bindings to find solutions
/// to the given constraint system.
///
/// \param depth The depth of the solution stack.
/// \param typeVar The type variable we're binding.
/// \param bindings The initial set of bindings to explore.
/// \param solutions The set of solutions.
///
/// \returns true if there are no solutions.
bool ConstraintSystem::tryTypeVariableBindings(
    unsigned depth, TypeVariableType *typeVar,
    ArrayRef<ConstraintSystem::PotentialBinding> bindings,
    SmallVectorImpl<Solution> &solutions,
    FreeTypeVariableBinding allowFreeTypeVariables) {
  bool anySolved = false;
  llvm::SmallPtrSet<CanType, 4> exploredTypes;
  llvm::SmallPtrSet<TypeBase *, 4> boundTypes;

  SmallVector<PotentialBinding, 4> storedBindings;
  auto &tc = getTypeChecker();
  ++solverState->NumTypeVariablesBound;

  for (unsigned tryCount = 0; !anySolved && !bindings.empty(); ++tryCount) {
    // Try each of the bindings in turn.
    ++solverState->NumTypeVariableBindings;
    bool sawFirstLiteralConstraint = false;

    if (tc.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(depth * 2) << "Active bindings: ";

      for (auto binding : bindings) {
        log << typeVar->getString() << " := "
            << binding.BindingType->getString() << " ";
      }

      log <<"\n";
    }

    for (const auto &binding : bindings) {
      // If this is a defaultable binding and we have found any solutions,
      // don't explore the default binding.
      if (binding.isDefaultableBinding() && anySolved)
        continue;

      auto type = binding.BindingType;

      // If the type variable can't bind to an lvalue, make sure the
      // type we pick isn't an lvalue.
      if (!typeVar->getImpl().canBindToLValue())
        type = type->getRValueType();

      // Remove parentheses. They're insignificant here.
      type = type->getWithoutParens();

      // If we've already tried this binding, move on.
      if (!boundTypes.insert(type.getPointer()).second)
        continue;

      // Prevent against checking against the same bound generic type
      // over and over again. Doing so means redundant work in the best
      // case. In the worst case, we'll produce lots of duplicate solutions
      // for this constraint system, which is problematic for overload
      // resolution.
      if (type->hasTypeVariable()) {
        auto triedBinding = false;
        if (auto BGT = type->getAs<BoundGenericType>()) {
          for (auto bt : boundTypes) {
            if (auto BBGT = bt->getAs<BoundGenericType>()) {
              if (BGT != BBGT &&
                  BGT->getDecl() == BBGT->getDecl()) {
                triedBinding = true;
                break;
              }
            }
          }
        }

        if (triedBinding)
          continue;
      }

      if (tc.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(depth * 2)
          << "(trying " << typeVar->getString() << " := " << type->getString()
          << "\n";
      }

      // Try to solve the system with typeVar := type
      ConstraintSystem::SolverScope scope(*this);
      if (binding.DefaultedProtocol) {
        // If we were able to solve this without considering
        // default literals, don't bother looking at default literals.
        if (!sawFirstLiteralConstraint) {
          sawFirstLiteralConstraint = true;
          if (anySolved)
            break;
        }
        type = openUnboundGenericType(type, typeVar->getImpl().getLocator());
        type = type->reconstituteSugar(/*recursive=*/false);
      } else if ((binding.BindingSource == ConstraintKind::ArgumentConversion ||
                  binding.BindingSource ==
                      ConstraintKind::ArgumentTupleConversion) &&
                 !type->hasTypeVariable() && isCollectionType(type)) {
        // If the type binding comes from the argument conversion, let's
        // instead of binding collection types directly let's try to
        // bind using temporary type variables substituted for element
        // types, that's going to ensure that subtype relationship is
        // always preserved.
        auto *BGT = type->castTo<BoundGenericType>();
        auto UGT = UnboundGenericType::get(BGT->getDecl(), BGT->getParent(),
                                           BGT->getASTContext());

        type = openUnboundGenericType(UGT, typeVar->getImpl().getLocator());
        type = type->reconstituteSugar(/*recursive=*/false);
      }

      // FIXME: We want the locator that indicates where the binding came
      // from.
      addConstraint(ConstraintKind::Bind, typeVar, type,
                    typeVar->getImpl().getLocator());

      // If this was from a defaultable binding note that.
      if (binding.isDefaultableBinding()) {
        DefaultedConstraints.push_back(binding.DefaultableBinding);
      }

      if (!solveRec(solutions, allowFreeTypeVariables))
        anySolved = true;

      if (tc.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(depth * 2) << ")\n";
      }
    }

    // If we found any solution, we're done.
    if (anySolved)
      break;

    // None of the children had solutions, enumerate supertypes and
    // try again.
    SmallVector<PotentialBinding, 4> newBindings;

    // Enumerate the supertypes of each of the types we tried.
    for (auto binding : bindings) {
      const auto type = binding.BindingType;
      if (type->hasError())
        continue;

      // After our first pass, note that we've explored these
      // types.
      if (tryCount == 0)
        exploredTypes.insert(type->getCanonicalType());

      // If we have a protocol with a default type, look for alternative
      // types to the default.
      if (tryCount == 0 && binding.DefaultedProtocol) {
        KnownProtocolKind knownKind =
            *(binding.DefaultedProtocol->getKnownProtocolKind());
        for (auto altType : getAlternativeLiteralTypes(knownKind)) {
          if (exploredTypes.insert(altType->getCanonicalType()).second)
            newBindings.push_back({altType, AllowedBindingKind::Subtypes,
                                   binding.BindingSource,
                                   binding.DefaultedProtocol});
        }
      }

      // Handle simple subtype bindings.
      if (binding.Kind == AllowedBindingKind::Subtypes &&
          typeVar->getImpl().canBindToLValue() &&
          !type->hasLValueType() &&
          !type->is<InOutType>()) {
        // Try lvalue qualification in addition to rvalue qualification.
        auto subtype = LValueType::get(type);
        if (exploredTypes.insert(subtype->getCanonicalType()).second)
          newBindings.push_back({subtype, binding.Kind, binding.BindingSource});
      }

      if (binding.Kind == AllowedBindingKind::Subtypes) {
        if (auto tupleTy = type->getAs<TupleType>()) {
          int scalarIdx = tupleTy->getElementForScalarInit();
          if (scalarIdx >= 0) {
            auto eltType = tupleTy->getElementType(scalarIdx);
            if (exploredTypes.insert(eltType->getCanonicalType()).second)
              newBindings.push_back(
                  {eltType, binding.Kind, binding.BindingSource});
          }
        }

        // If we were unsuccessful solving for T?, try solving for T.
        if (auto objTy = type->getOptionalObjectType()) {
          if (exploredTypes.insert(objTy->getCanonicalType()).second) {
            // If T is a type variable, only attempt this if both the
            // type variable we are trying bindings for, and the type
            // variable we will attempt to bind, both have the same
            // polarity with respect to being able to bind lvalues.
            if (auto otherTypeVar = objTy->getAs<TypeVariableType>()) {
              if (typeVar->getImpl().canBindToLValue() ==
                  otherTypeVar->getImpl().canBindToLValue()) {
                newBindings.push_back(
                    {objTy, binding.Kind, binding.BindingSource});
              }
            } else {
              newBindings.push_back(
                  {objTy, binding.Kind, binding.BindingSource});
            }
          }
        }
      }

      if (binding.Kind != AllowedBindingKind::Supertypes)
        continue;

      for (auto supertype : enumerateDirectSupertypes(getTypeChecker(), type)) {
        // If we're not allowed to try this binding, skip it.
        auto simpleSuper = checkTypeOfBinding(typeVar, supertype);
        if (!simpleSuper)
          continue;

        // If we haven't seen this supertype, add it.
        if (exploredTypes.insert((*simpleSuper)->getCanonicalType()).second)
          newBindings.push_back({
              *simpleSuper,
              binding.Kind,
              binding.BindingSource,
          });
      }
    }

    // If we didn't compute any new bindings, we're done.
    if (newBindings.empty())
      break;

    // We have a new set of bindings; use them for our next loop.
    storedBindings = std::move(newBindings);
    bindings = storedBindings;
  }

  return !anySolved;
}

/// \brief Solve the system of constraints.
///
/// \param allowFreeTypeVariables How to bind free type variables in
/// the solution.
///
/// \returns a solution if a single unambiguous one could be found, or None if
/// ambiguous or unsolvable.
Optional<Solution>
ConstraintSystem::solveSingle(FreeTypeVariableBinding allowFreeTypeVariables) {
  SmallVector<Solution, 4> solutions;
  if (solve(nullptr, solutions, allowFreeTypeVariables) ||
      solutions.size() != 1)
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

    E->print(log);
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
    SolverState state(E, cs);

    // Use solveRec() instead of solve() in here, because solve()
    // would try to deduce the best solution, which we don't
    // really want. Instead, we want the reduced set of domain choices.
    cs.solveRec(solutions, FreeTypeVariableBinding::Allow);
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
            auto coercionType = CS.TC.resolveType(coercionRepr, CS.DC,
                                                  TypeResolutionOptions());

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

  assert(!solverState && "use solveRec for recursive calls");

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

    expr->print(log, getTypeOfExpr, getTypeOfTypeLoc);
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
  SolverState state(expr, *this);

  // Solve the system.
  solveRec(solutions, allowFreeTypeVariables);

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Solver statistics---\n";
    log << "Total number of scopes explored: " << solverState->NumStatesExplored << "\n";
    log << "Number of leaf scopes explored: " << solverState->leafScopes << "\n";
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

bool ConstraintSystem::solveRec(SmallVectorImpl<Solution> &solutions,
                                FreeTypeVariableBinding allowFreeTypeVariables){
  // If we already failed, or simplification fails, we're done.
  if (failedConstraint || simplify()) {
    return true;
  } else {
    assert(ActiveConstraints.empty() && "Active constraints remain?");
  }

  // If there are no constraints remaining, we're done. Save this solution.
  if (InactiveConstraints.empty()) {
    // If this solution is worse than the best solution we've seen so far,
    // skip it.
    if (worseThanBestSolution())
      return true;

    // If any free type variables remain and we're not allowed to have them,
    // fail.
    if (allowFreeTypeVariables == FreeTypeVariableBinding::Disallow &&
        hasFreeTypeVariables())
      return true;

    auto solution = finalize(allowFreeTypeVariables);
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2)
        << "(found solution " << CurrentScore << ")\n";
    }

    solutions.push_back(std::move(solution));
    return false;
  }

  // Contract the edges of the constraint graph.
  CG.optimize();

  // Compute the connected components of the constraint graph.
  // FIXME: We're seeding typeVars with TypeVariables so that the
  // connected-components algorithm only considers those type variables within
  // our component. There are clearly better ways to do this.
  SmallVector<TypeVariableType *, 16> typeVars(TypeVariables);
  SmallVector<unsigned, 16> components;
  unsigned numComponents = CG.computeConnectedComponents(typeVars, components);

  // If we don't have more than one component, just solve the whole
  // system.
  if (numComponents < 2)
    return solveSimplified(solutions, allowFreeTypeVariables);

  if (TC.Context.LangOpts.DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();

    // Verify that the constraint graph is valid.
    CG.verify();

    log << "---Constraint graph---\n";
    CG.print(log);

    log << "---Connected components---\n";
    CG.printConnectedComponents(log);
  }

  // Construct a mapping from type variables and constraints to their
  // owning component.
  llvm::DenseMap<TypeVariableType *, unsigned> typeVarComponent;
  llvm::DenseMap<Constraint *, unsigned> constraintComponent;
  for (unsigned i = 0, n = typeVars.size(); i != n; ++i) {
    // Record the component of this type variable.
    typeVarComponent[typeVars[i]] = components[i];

    // Record the component of each of the constraints.
    for (auto constraint : CG[typeVars[i]].getConstraints())
      constraintComponent[constraint] = components[i];
  }

  // Add the orphaned components to the mapping from constraints to components.
  unsigned firstOrphanedConstraint =
    numComponents - CG.getOrphanedConstraints().size();
  {
    unsigned component = firstOrphanedConstraint;
    for (auto constraint : CG.getOrphanedConstraints())
      constraintComponent[constraint] = component++;
  }

  // Sort the constraints into component buckets based on component number.
  std::unique_ptr<Component[]> buckets(new Component[numComponents]);

  while (!InactiveConstraints.empty()) {
    auto *constraint = &InactiveConstraints.front();
    InactiveConstraints.pop_front();
    buckets[constraintComponent[constraint]].record(constraint);
  }

  // Create component ordering based on the information associated
  // with constraints in each bucket - e.g. number of disjunctions.
  std::vector<unsigned> componentOrdering;
  // First seed the ordering with original indexes of the components.
  for (unsigned component = 0; component != numComponents; ++component)
    componentOrdering.push_back(component);
  // Now sort the components in the ordering based on the scores of the buckets.
  std::sort(
      componentOrdering.begin(), componentOrdering.end(),
      [&](const unsigned &componentA, const unsigned &componentB) -> bool {
        return buckets[componentA] < buckets[componentB];
      });

  // Remove all of the orphaned constraints; we'll introduce them as needed.
  auto allOrphanedConstraints = CG.takeOrphanedConstraints();

  // Function object that returns all constraints placed into buckets
  // back to the list of constraints.
  auto returnAllConstraints = [&] {
    assert(InactiveConstraints.empty() && "Already have constraints?");
    for (unsigned component = 0; component != numComponents; ++component)
      buckets[component].reinstateTo(InactiveConstraints);
    CG.setOrphanedConstraints(std::move(allOrphanedConstraints));
  };

  // Compute the partial solutions produced for each connected component.
  std::unique_ptr<SmallVector<Solution, 4>[]> 
    partialSolutions(new SmallVector<Solution, 4>[numComponents]);
  Optional<Score> PreviousBestScore = solverState->BestScore;

  for (auto &component : componentOrdering) {
    assert(InactiveConstraints.empty() &&
           "Some constraints were not transferred?");
    ++solverState->NumComponentsSplit;

    auto &bucket = buckets[component];

    llvm::SmallVector<TypeVariableType *, 16> allTypeVariables
      = std::move(TypeVariables);

    Constraint *orphaned = nullptr;
    if (component < firstOrphanedConstraint) {
      // Collect the type variables that are not part of a different
      // component; this includes type variables that are part of the
      // component as well as already-resolved type variables.
      for (auto typeVar : allTypeVariables) {
        auto known = typeVarComponent.find(typeVar);
        if (known != typeVarComponent.end() && known->second != component)
          continue;

        TypeVariables.push_back(typeVar);
      }
    } else {
      // Get the orphaned constraint.
      orphaned = allOrphanedConstraints[component - firstOrphanedConstraint];
    }
    CG.setOrphanedConstraint(orphaned);

    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2)
          << "(solving component #" << component << "\n";
    }

    // Solve for this component. If it fails, we're done.
    bool failed = bucket.solve(*this, partialSolutions[component],
                               allowFreeTypeVariables);

    if (failed) {
      if (TC.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(solverState->depth * 2) << "failed component #" 
                                           << component << ")\n";
      }
      
      TypeVariables = std::move(allTypeVariables);
      returnAllConstraints();
      return true;
    }

    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2) << "finished component #" 
                                         << component << ")\n";
    }
    
    assert(!partialSolutions[component].empty() &&" No solutions?");

    // Move the type variables back, clear out constraints; we're
    // ready for the next component.
    TypeVariables = std::move(allTypeVariables);

    // For each of the partial solutions, subtract off the current score.
    // It doesn't contribute.
    for (auto &solution : partialSolutions[component])
      solution.getFixedScore() -= CurrentScore;

    // Restore the previous best score.
    solverState->BestScore = PreviousBestScore;
  }

  // Move the constraints back. The system is back in a normal state.
  returnAllConstraints();

  // When there are multiple partial solutions for a given connected component,
  // rank those solutions to pick the best ones. This limits the number of
  // combinations we need to produce; in the common case, down to a single
  // combination.
  for (unsigned component = 0; component != numComponents; ++component) {
    auto &solutions = partialSolutions[component];
    // If there's a single best solution, keep only that one.
    // Otherwise, the set of solutions will at least have been minimized.
    if (!retainAllSolutions())
      filterSolutions(solutions, solverState->ExprWeights, /*minimize=*/true);
  }

  // Produce all combinations of partial solutions.
  SmallVector<unsigned, 2> indices(numComponents, 0);
  bool done = false;
  bool anySolutions = false;
  do {
    // Create a new solver scope in which we apply all of the partial
    // solutions.
    SolverScope scope(*this);
    for (unsigned i = 0; i != numComponents; ++i)
      applySolution(partialSolutions[i][indices[i]]);

    // This solution might be worse than the best solution found so far. If so,
    // skip it.
    if (!worseThanBestSolution()) {
      // Finalize this solution.
      auto solution = finalize(allowFreeTypeVariables);
      if (TC.getLangOpts().DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(solverState->depth * 2)
          << "(composed solution " << CurrentScore << ")\n";
      }

      // Save this solution.
      solutions.push_back(std::move(solution));

      anySolutions = true;
    }
    
    // Find the next combination.
    for (unsigned n = numComponents; n > 0; --n) {
      ++indices[n-1];

      // If we haven't run out of solutions yet, we're done.
      if (indices[n-1] < partialSolutions[n-1].size())
        break;

      // If we ran out of solutions at the first position, we're done.
      if (n == 1) {
        done = true;
        break;
      } 

      // Zero out the indices from here to the end.
      for (unsigned i = n-1; i != numComponents; ++i)
        indices[i] = 0;
    }
  } while (!done);

  return !anySolutions;
}

/// Whether we should short-circuit a disjunction that already has a
/// solution when we encounter the given constraint.
static bool shortCircuitDisjunctionAt(Constraint *constraint,
                                      Constraint *successfulConstraint,
                                      ASTContext &ctx) {

  // If the successfully applied constraint is favored, we'll consider that to
  // be the "best".
  if (successfulConstraint->isFavored() && !constraint->isFavored()) {
#if !defined(NDEBUG)
    if (successfulConstraint->getKind() == ConstraintKind::BindOverload) {
      auto overloadChoice = successfulConstraint->getOverloadChoice();
      assert((!overloadChoice.isDecl() ||
              !overloadChoice.getDecl()->getAttrs().isUnavailable(ctx)) &&
             "Unavailable decl should not be favored!");
    }
#endif

    return true;
  }

  // Anything without a fix is better than anything with a fix.
  if (constraint->getFix() && !successfulConstraint->getFix())
    return true;

  if (auto restriction = constraint->getRestriction()) {
    // Non-optional conversions are better than optional-to-optional
    // conversions.
    if (*restriction == ConversionRestrictionKind::OptionalToOptional)
      return true;
    
    // Array-to-pointer conversions are better than inout-to-pointer conversions.
    if (auto successfulRestriction = successfulConstraint->getRestriction()) {
      if (*successfulRestriction == ConversionRestrictionKind::ArrayToPointer
          && *restriction == ConversionRestrictionKind::InoutToPointer)
        return true;
    }
  }

  // Implicit conversions are better than checked casts.
  if (constraint->getKind() == ConstraintKind::CheckedCast)
    return true;

  return false;
}

void ConstraintSystem::collectDisjunctions(
    SmallVectorImpl<Constraint *> &disjunctions) {
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() == ConstraintKind::Disjunction)
      disjunctions.push_back(&constraint);
  }
}

/// \brief Check if the given disjunction choice should be attempted by solver.
static bool shouldSkipDisjunctionChoice(ConstraintSystem &cs,
                                        DisjunctionChoice &choice,
                                        Optional<Score> &bestNonGenericScore) {
  if (choice->isDisabled()) {
    auto &TC = cs.TC;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
      log.indent(cs.solverState->depth)
      << "(skipping ";
      choice->print(log, &TC.Context.SourceMgr);
      log << '\n';
    }

    return true;
  }

  // Skip unavailable overloads unless solver is in the "diagnostic" mode.
  if (!cs.shouldAttemptFixes() && choice.isUnavailable())
    return true;

  // Don't attempt to solve for generic operators if we already have
  // a non-generic solution.

  // FIXME: Less-horrible but still horrible hack to attempt to
  //        speed things up. Skip the generic operators if we
  //        already have a solution involving non-generic operators,
  //        but continue looking for a better non-generic operator
  //        solution.
  if (bestNonGenericScore && choice.isGenericOperator()) {
    auto &score = bestNonGenericScore->Data;
    // Let's skip generic overload choices only in case if
    // non-generic score indicates that there were no forced
    // unwrappings of optional(s), no unavailable overload
    // choices present in the solution, no fixes required,
    // and there are no non-trivial function conversions.
    if (score[SK_ForceUnchecked] == 0 && score[SK_Unavailable] == 0 &&
        score[SK_Fix] == 0 && score[SK_FunctionConversion] == 0)
      return true;
  }

  return false;
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

  // Collect any disjunctions that simply attempt bindings for a
  // type variable.
  SmallVector<Constraint *, 8> bindingDisjunctions;
  for (auto *disjunction : disjunctions) {
    llvm::Optional<TypeVariableType *> commonTypeVariable;
    if (llvm::all_of(
            disjunction->getNestedConstraints(),
            [&](Constraint *bindingConstraint) {
              if (bindingConstraint->getKind() != ConstraintKind::Bind)
                return false;

              auto *tv =
                  bindingConstraint->getFirstType()->getAs<TypeVariableType>();
              // Only do this for simple type variable bindings, not for
              // bindings like: ($T1) -> $T2 bind String -> Int
              if (!tv)
                return false;

              if (!commonTypeVariable.hasValue())
                commonTypeVariable = tv;

              if (commonTypeVariable.getValue() != tv)
                return false;

              return true;
            })) {
      bindingDisjunctions.push_back(disjunction);
    }
  }

  for (auto *disjunction : bindingDisjunctions) {
    auto nested = disjunction->getNestedConstraints();
    assert(!nested.empty());
    auto *tv = cs.simplifyType(nested[0]->getFirstType())
                   ->getRValueType()
                   ->getAs<TypeVariableType>();
    assert(tv);

    llvm::SetVector<Constraint *> constraints;
    cs.getConstraintGraph().gatherConstraints(
        tv, constraints, ConstraintGraph::GatheringKind::EquivalenceClass,
        [](Constraint *constraint) {
          return constraint->getKind() == ConstraintKind::Conversion;
        });

    for (auto *constraint : constraints) {
      auto toType =
          cs.simplifyType(constraint->getSecondType())->getRValueType();
      auto *toTV = toType->getAs<TypeVariableType>();
      if (tv != toTV)
        continue;

      return disjunction;
    }
  }

  // If we had any binding disjunctions, return the first of
  // those. These ensure that we attempt to bind types earlier than
  // trying the elements of other disjunctions, which can often mean
  // we fail faster.
  if (!bindingDisjunctions.empty())
    return bindingDisjunctions[0];

  return nullptr;
}

Constraint *ConstraintSystem::selectDisjunction() {
  SmallVector<Constraint *, 4> disjunctions;

  collectDisjunctions(disjunctions);
  if (auto *disjunction = selectBestBindingDisjunction(*this, disjunctions))
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

bool ConstraintSystem::solveForDisjunctionChoices(
    ArrayRef<Constraint *> constraints, ConstraintLocator *disjunctionLocator,
    SmallVectorImpl<Solution> &solutions,
    FreeTypeVariableBinding allowFreeTypeVariables, bool explicitConversion) {
  Optional<Score> bestNonGenericScore;
  Optional<std::pair<DisjunctionChoice, Score>> lastSolvedChoice;

  // Try each of the constraints within the disjunction.
  for (auto index : indices(constraints)) {
    auto currentChoice =
        DisjunctionChoice(this, constraints[index], explicitConversion);
    if (shouldSkipDisjunctionChoice(*this, currentChoice, bestNonGenericScore))
      continue;

    // We already have a solution; check whether we should
    // short-circuit the disjunction.
    if (lastSolvedChoice) {
      Constraint *lastChoice = lastSolvedChoice->first;
      auto delta = lastSolvedChoice->second - CurrentScore;
      bool hasUnavailableOverloads = delta.Data[SK_Unavailable] > 0;
      bool hasFixes = delta.Data[SK_Fix] > 0;

      // Attempt to short-circuit evaluation of this disjunction only
      // if the disjunction choice we are comparing to did not involve
      // selecting unavailable overloads or result in fixes being
      // applied to reach a solution.
      if (!hasUnavailableOverloads && !hasFixes &&
          shortCircuitDisjunctionAt(currentChoice, lastChoice, getASTContext()))
        break;
    }

    // Try to solve the system with this option in the disjunction.
    SolverScope scope(*this);
    ++solverState->NumDisjunctionTerms;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth)
        << "(assuming ";
      currentChoice->print(log, &TC.Context.SourceMgr);
      log << '\n';
    }

    // If the disjunction requested us to, remember which choice we
    // took for it.

    if (disjunctionLocator) {
      DisjunctionChoices.push_back({disjunctionLocator, index});

      // Implicit unwraps of optionals are worse solutions than those
      // not involving implicit unwraps.
      if (!disjunctionLocator->getPath().empty()) {
        auto kind = disjunctionLocator->getPath().back().getKind();
        if (kind == ConstraintLocator::ImplicitlyUnwrappedDisjunctionChoice ||
            kind == ConstraintLocator::DynamicLookupResult) {
          assert(index == 0 || index == 1);
          if (index == 1)
            increaseScore(SK_ForceUnchecked);
        }
      }
    }

    if (auto score = currentChoice.solve(solutions, allowFreeTypeVariables)) {
      if (!currentChoice.isGenericOperator() &&
          currentChoice.isSymmetricOperator()) {
        if (!bestNonGenericScore || score < bestNonGenericScore)
          bestNonGenericScore = score;
      }

      lastSolvedChoice = {currentChoice, *score};
    }

    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth) << ")\n";
    }
  }

  return !bool(lastSolvedChoice.hasValue());
}

bool ConstraintSystem::solveForDisjunction(
    Constraint *disjunction, SmallVectorImpl<Solution> &solutions,
    FreeTypeVariableBinding allowFreeTypeVariables) {
  assert(disjunction->getKind() == ConstraintKind::Disjunction);

  // Remove this disjunction constraint from the list.
  auto afterDisjunction = InactiveConstraints.erase(disjunction);
  CG.removeConstraint(disjunction);

  // Check if selected disjunction has a representative
  // this might happen when there are multiple binary operators
  // chained together. If so, disable choices which differ
  // from currently selected representative.
  auto pruneOverloadSet = [&](Constraint *disjunction) -> bool {
    auto *choice = disjunction->getNestedConstraints().front();
    auto *typeVar = choice->getFirstType()->getAs<TypeVariableType>();
    if (!typeVar)
      return false;

    auto *repr = typeVar->getImpl().getRepresentative(nullptr);
    if (!repr || repr == typeVar)
      return false;

    bool isPruned = false;
    for (auto resolved = resolvedOverloadSets; resolved;
         resolved = resolved->Previous) {
      if (!resolved->BoundType->isEqual(repr))
        continue;

      auto &representative = resolved->Choice;
      if (!representative.isDecl())
        return false;

      // Disable all of the overload choices which are different from
      // the one which is currently picked for representative.
      for (auto *constraint : disjunction->getNestedConstraints()) {
        auto choice = constraint->getOverloadChoice();
        if (!choice.isDecl())
          continue;

        if (choice.getDecl() != representative.getDecl()) {
          constraint->setDisabled();
          isPruned = true;
        }
      }
      break;
    }

    return isPruned;
  };

  bool hasDisabledChoices = pruneOverloadSet(disjunction);

  ++solverState->NumDisjunctions;
  auto *locator =
      disjunction->shouldRememberChoice() ? disjunction->getLocator() : nullptr;
  assert(!disjunction->shouldRememberChoice() || disjunction->getLocator());

  auto noSolutions = solveForDisjunctionChoices(
      disjunction->getNestedConstraints(), locator, solutions,
      allowFreeTypeVariables, isExplicitConversionConstraint(disjunction));

  if (hasDisabledChoices) {
    // Re-enable previously disabled overload choices.
    for (auto *choice : disjunction->getNestedConstraints()) {
      if (choice->isDisabled())
        choice->setEnabled();
    }
  }

  // Put the disjunction constraint back in its place.
  InactiveConstraints.insert(afterDisjunction, disjunction);
  CG.addConstraint(disjunction);

  return noSolutions;
}

bool ConstraintSystem::solveSimplified(
    SmallVectorImpl<Solution> &solutions,
    FreeTypeVariableBinding allowFreeTypeVariables) {

  auto *disjunction = selectDisjunction();

  auto bestBindings = determineBestBindings();

  // If we've already explored a lot of potential solutions, bail.
  if (getExpressionTooComplex(solutions))
    return true;

  // If we have a binding that does not involve type variables, and is
  // not fully bound, or we have no disjunction to attempt instead,
  // go ahead and try the bindings for this type variable.
  if (bestBindings && (!disjunction || (!bestBindings->InvolvesTypeVariables &&
                                        !bestBindings->FullyBound))) {
    return tryTypeVariableBindings(solverState->depth, bestBindings->TypeVar,
                                   bestBindings->Bindings, solutions,
                                   allowFreeTypeVariables);
  }

  if (disjunction)
    return solveForDisjunction(disjunction, solutions, allowFreeTypeVariables);

  // If there are no disjunctions we can't solve this system unless we have
  // free type variables and are allowing them in the solution.
  if (allowFreeTypeVariables == FreeTypeVariableBinding::Disallow ||
      !hasFreeTypeVariables())
    return true;

  // If this solution is worse than the best solution we've seen so far,
  // skip it.
  if (worseThanBestSolution())
    return true;

  // If we only have relational or member constraints and are allowing
  // free type variables, save the solution.
  for (auto &constraint : InactiveConstraints) {
    switch (constraint.getClassification()) {
    case ConstraintClassification::Relational:
    case ConstraintClassification::Member:
      continue;
    default:
      return true;
    }
  }

  auto solution = finalize(allowFreeTypeVariables);
  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log.indent(solverState->depth * 2) << "(found solution)\n";
  }

  solutions.push_back(std::move(solution));
  return false;
}

Optional<Score>
DisjunctionChoice::solve(SmallVectorImpl<Solution> &solutions,
                         FreeTypeVariableBinding allowFreeTypeVariables) {
  CS->simplifyDisjunctionChoice(Choice);

  if (ExplicitConversion)
    propagateConversionInfo();

  if (CS->solveRec(solutions, allowFreeTypeVariables))
    return None;

  assert (!solutions.empty());

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

bool DisjunctionChoice::isGenericOperator() const {
  auto *decl = getOperatorDecl();
  if (!decl)
    return false;

  auto interfaceType = decl->getInterfaceType();
  return interfaceType->is<GenericFunctionType>();
}

bool DisjunctionChoice::isSymmetricOperator() const {
  auto *decl = getOperatorDecl();
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

void DisjunctionChoice::propagateConversionInfo() const {
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

  auto bindings = CS->getPotentialBindings(typeVar);
  if (bindings.InvolvesTypeVariables || bindings.Bindings.size() != 1)
    return;

  auto conversionType = bindings.Bindings[0].BindingType;
  llvm::SetVector<Constraint *> constraints;
  CS->CG.gatherConstraints(typeVar, constraints,
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
    CS->addConstraint(ConstraintKind::Bind, typeVar, conversionType,
                      Choice->getLocator());
}
