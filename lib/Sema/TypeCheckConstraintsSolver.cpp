//===--- TypeCheckConstraintsSolver.cpp - Constraint Solver ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the constraint solver used in the type checker.
//
//===----------------------------------------------------------------------===//
#include "ConstraintSystem.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>
using namespace swift;
using namespace constraints;

//===--------------------------------------------------------------------===//
// Constraint solver statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint solver"
STATISTIC(NumSimplifyIterations, "# of simplification iterations");
STATISTIC(NumSupertypeFallbacks, "# of supertype fallbacks");

/// \brief Check whether the given type can be used as a binding for the given
/// type variable.
///
/// \returns the type to bind to, if the binding is okay.
static Optional<Type> checkTypeOfBinding(ConstraintSystem &cs, 
                                         TypeVariableType *typeVar, Type type) {
  if (!type)
    return Nothing;

  // Simplify the type.
  type = cs.simplifyType(type);

  // If the type references the type variable, don't permit the binding.
  SmallVector<TypeVariableType *, 4> referencedTypeVars;
  type->getTypeVariables(referencedTypeVars);
  if (std::count(referencedTypeVars.begin(), referencedTypeVars.end(), typeVar))
    return Nothing;

  // If the type is a type variable itself, don't permit the binding.
  if (type->is<TypeVariableType>())
    return Nothing;

  // Okay, allow the binding (with the simplified type).
  return type;
}

Optional<Solution> ConstraintSystem::finalize() {
  switch (State) {
  case Unsolvable:
    if (!solverState) {
      removeInactiveChildren();
      restoreTypeVariableBindings();
    }
    return Nothing;

  case Solved:
    llvm_unreachable("Already finalized (?)");
    break;

  case Active:
    if (hasActiveChildren()) {
      // There is a solution below, but by itself this isn't a solution.
      if (!solverState) {
        removeInactiveChildren();
        clearIntermediateData();
        restoreTypeVariableBindings();
      }
      return Nothing;
    }

    // Check whether we have a proper solution.
    if (Constraints.empty() && UnresolvedOverloadSets.empty() &&
        !hasFreeTypeVariables()) {
      State = Solved;
      auto result = createSolution();
      if (!solverState) {
        removeInactiveChildren();
        restoreTypeVariableBindings();
      }
      return std::move(result);
    }

    // We don't have a solution;
    if (!solverState) {
      markUnsolvable();
      removeInactiveChildren();
      restoreTypeVariableBindings();
    }
    return Nothing;
  }
}

void ConstraintSystem::removeInactiveChildren() {
  Children.erase(std::remove_if(Children.begin(), Children.end(),
                   [&](std::unique_ptr<ConstraintSystem> &child) -> bool {
                     return !child->hasActiveChildren() &&
                            (!child->Children.empty() ||
                             !child->isSolved());
                   }),
                 Children.end());
}

void ConstraintSystem::markChildInactive(ConstraintSystem *childCS) {
  assert(NumActiveChildren > 0);
  --NumActiveChildren;

  // If the child system made an assumption about a type variable that
  // didn't pan out, try weaker assumptions.
  if (auto typeVar = childCS->assumedTypeVar) {
    auto boundTy = childCS->getFixedType(typeVar);
    auto supertypes = enumerateDirectSupertypes(boundTy);
    auto &explored = ExploredTypeBindings[typeVar];

    bool addedAny = false;
    for (auto supertype : supertypes) {
      // Make sure we can actually do the binding.
      if (auto bindingTy = checkTypeOfBinding(*this, typeVar, supertype))
        supertype = *bindingTy;
      else
        continue;

      if (explored.count(supertype->getCanonicalType()) > 0)
        continue;

      ++NumSupertypeFallbacks;
      PotentialBindings.push_back( { typeVar, supertype, false } );
      addedAny = true;
    }

    if (!addedAny) {
      // If we haven't added any constraints for this type variable, check
      // whether we can fall back to a default literal type.
      // FIXME: This would be far, far more efficient if we keep constraints
      // related to a type variable on-line.
      for (auto constraint : Constraints) {
        if (constraint->getClassification() !=ConstraintClassification::Literal)
          continue;

        if (auto constrainedVar
              = dyn_cast<TypeVariableType>(
                  constraint->getFirstType().getPointer())) {
          if (getRepresentative(constrainedVar) != typeVar)
            continue;

          if (auto literalType =
                TC.getDefaultLiteralType(constraint->getLiteralKind())) {
            if (explored.count(literalType->getCanonicalType()) == 0)
              PotentialBindings.push_back({typeVar, literalType, true});
          }
        }
      }
    }
  }
}

Solution ConstraintSystem::createSolution() {
  assert(State == Solved);

  decltype(ExploredTypeBindings)().swap(ExploredTypeBindings);
  PotentialBindings.clear();
  decltype(ExternallySolved)().swap(ExternallySolved);

  Solution solution(getTopConstraintSystem());
  for (auto cs = this; cs; cs = cs->Parent) {
    // For each of the type variables, get its fixed type.
    for (auto tv : cs->TypeVariables) {
      solution.typeBindings[tv] = simplifyType(tv);
    }

    // For each of the overload sets, get its overload choice.
    for (auto resolved : cs->ResolvedOverloads) {
      solution.overloadChoices[resolved.first->getLocator()]
        = { resolved.first->getChoices()[resolved.second.first],
            resolved.second.second };
    }
  }

  return solution;
}

void ConstraintSystem::clearIntermediateData() {
  decltype(ExploredTypeBindings)().swap(ExploredTypeBindings);
  PotentialBindings.clear();
  decltype(ExternallySolved)().swap(ExternallySolved);
}

/// \brief Restore the type variable bindings to what they were before
/// we attempted to solve this constraint system.
void ConstraintSystem::restoreTypeVariableBindings(unsigned numBindings) {
  std::for_each(SavedBindings.rbegin(), SavedBindings.rbegin() + numBindings,
                [](SavedTypeVariableBinding &saved) {
                  saved.restore();
                });
  SavedBindings.erase(SavedBindings.end() - numBindings,
                      SavedBindings.end());
}

SmallVector<Type, 4> ConstraintSystem::enumerateDirectSupertypes(Type type) {
  SmallVector<Type, 4> result;

  if (auto tupleTy = type->getAs<TupleType>()) {
    // A one-element tuple can be viewed as a scalar by dropping the label.
    // FIXME: There is a way more general property here, where we can drop
    // one label from the tuple, maintaining the rest.
    if (tupleTy->getFields().size() == 1) {
      auto &elt = tupleTy->getFields()[0];
      if (elt.isVararg()) // FIXME: Should we keep the name?
        result.push_back(elt.getVarargBaseTy());
      else if (!elt.getName().empty())
        result.push_back(elt.getType());
    }
  }

  if (auto functionTy = type->getAs<FunctionType>()) {
    // FIXME: Can weaken input type, but we really don't want to get in the
    // business of strengthening the result type.

    // An [auto_closure] function type can be viewed as scalar of the result
    // type.
    if (functionTy->isAutoClosure())
      result.push_back(functionTy->getResult());
  }

  if (type->mayHaveSuperclass()) {
    // FIXME: Can also weaken to the set of protocol constraints, but only
    // if there are any protocols that the type conforms to but the superclass
    // does not.

    // If there is a superclass, it is a direct supertype.
    if (auto superclass = TC.getSuperClassOf(type))
      result.push_back(superclass);
  }

  // FIXME: lots of other cases to consider!
  return result;
}

void ConstraintSystem::collectConstraintsForTypeVariables(
       SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {
  typeVarConstraints.clear();

  // Provide a mapping from type variable to its constraints. The getTVC
  // function ties together the SmallVector and the DenseMap.
  llvm::SmallDenseMap<TypeVariableType *, unsigned, 8> typeVarConstraintsMap;
  auto getTVC = [&](TypeVariableType *tv) -> TypeVariableConstraints& {
    tv = getRepresentative(tv);
    unsigned& constraintsIdx = typeVarConstraintsMap[tv];
    if (!constraintsIdx) {
      typeVarConstraints.push_back(TypeVariableConstraints(tv));
      constraintsIdx = typeVarConstraints.size();
    }
    return typeVarConstraints[constraintsIdx - 1];
  };

  // First, collect all of the constraints that relate directly to a
  // type variable.
  SmallVector<TypeVariableType *, 8> referencedTypeVars;
  for (auto constraint : Constraints) {
    auto first = simplifyType(constraint->getFirstType());
    switch (constraint->getClassification()) {
    case ConstraintClassification::Relational:
      // Handle this interesting case below.
      break;

    case ConstraintClassification::Archetype:
    case ConstraintClassification::Literal:
      if (auto firstTV = dyn_cast<TypeVariableType>(first.getPointer())) {
        // Record this constraint on the type variable.
        getTVC(firstTV).KindConstraints.push_back(constraint);
      } else {
        // Simply mark any type variables in the type as referenced.
        first->getTypeVariables(referencedTypeVars);
      }
      continue;

    case ConstraintClassification::Member:
      // Mark the referenced type variables for the left-hand side, unless
      // it is simply a type variable or a metatype of a type variable, in
      // which case it's not going to open up any more opportunities.
      // FIXME: Enable this for old solver?
      bool skipFirst = solverState;
      if (skipFirst) {
        if (auto metaFirst = first->getAs<MetaTypeType>())
          skipFirst = metaFirst->getInstanceType()->is<TypeVariableType>();
        else
          skipFirst = first->is<TypeVariableType>();
      }
      if (skipFirst)
        first->getTypeVariables(referencedTypeVars);
      simplifyType(constraint->getSecondType())
        ->getTypeVariables(referencedTypeVars);
      continue;
    }

    auto second = simplifyType(constraint->getSecondType());

    auto firstTV = first->getAs<TypeVariableType>();
    if (firstTV) {
      // Record the constraint.
      getTVC(firstTV).Above.push_back(std::make_pair(constraint, second));
    } else {
      // Collect any type variables represented in the first type.
      first->getTypeVariables(referencedTypeVars);
    }

    auto secondTV = second->getAs<TypeVariableType>();
    if (secondTV) {
      // Record the constraint.
      getTVC(secondTV).Below.push_back(std::make_pair(constraint, first));
    } else {
      // Collect any type variables represented in the second type.
      second->getTypeVariables(referencedTypeVars);
    }

    // If both types are type variables, mark both as referenced.
    if (firstTV && secondTV) {
      referencedTypeVars.push_back(firstTV);
      referencedTypeVars.push_back(secondTV);
    }
  }

  // Mark any type variables that specify the result of an unresolved overload
  // set as having non-concrete constraints.
  for (auto ovl : UnresolvedOverloadSets) {
    ovl->getBoundType()->getTypeVariables(referencedTypeVars);
  }

  // Mark any referenced type variables as having non-concrete constraints.
  llvm::SmallPtrSet<TypeVariableType *, 8> SeenVars;
  for (auto tv : referencedTypeVars) {
    // If this type variable is redundantly in the list, skip it.
    if (!SeenVars.insert(tv)) continue;

    tv = getRepresentative(tv);
    auto known = typeVarConstraintsMap.find(tv);
    if (known == typeVarConstraintsMap.end())
      continue;

    typeVarConstraints[known->second-1].HasNonConcreteConstraints = true;
  }
}

bool ConstraintSystem::simplify() {
  bool solvedAny;
  do {
    // Loop through all of the thus-far-unsolved constraints, attempting to
    // simplify each one.
    SmallVector<Constraint *, 16> existingConstraints;
    llvm::SmallPtrSet<Constraint *, 16> visited;
    existingConstraints.swap(Constraints);
    solvedAny = false;
    for (unsigned i = 0, n = existingConstraints.size(); i != n; ++i) {
      auto constraint = existingConstraints[i];
      if (ExternallySolved.count(constraint))
        continue;

      // FIXME: Temporary hack. We shouldn't ever end up revisiting a
      // constraint like this.
      if (!visited.insert(constraint))
        continue;
      
      if (addConstraint(constraint, false, true)) {
        solvedAny = true;

        if (TC.getLangOpts().DebugConstraintSolver && !solverState)
          SolvedConstraints.push_back(constraint);        
      }

      if (failedConstraint) {
        if (solverState) {
          solverState->retiredConstraints.append(
            existingConstraints.begin() + i,
            existingConstraints.end());
        }
        return true;
      }
    }

    ExternallySolved.clear();
    ++NumSimplifyIterations;
  } while (solvedAny);

  // We've simplified all of the constraints we can.
  return false;
}

namespace {

/// \brief Truncate the given small vector to the given new size.
template<typename T>
void truncate(SmallVectorImpl<T> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  vec.erase(vec.begin() + newSize, vec.end());
}

}

ConstraintSystem::SolverScope::SolverScope(ConstraintSystem &cs)
  : cs(cs)
{
  ++cs.solverState->depth;

  numResolvedOverloadSets = cs.solverState->resolvedOverloadSets.size();
  numTypeVariables = cs.TypeVariables.size();
  numUnresolvedOverloadSets = cs.UnresolvedOverloadSets.size();
  numGeneratedOverloadSets = cs.solverState->generatedOverloadSets.size();
  numSavedBindings = cs.SavedBindings.size();
  numGeneratedConstraints = cs.solverState->generatedConstraints.size();
  numRetiredConstraints = cs.solverState->retiredConstraints.size();
}

ConstraintSystem::SolverScope::~SolverScope() {
  --cs.solverState->depth;

  // Remove resolved overloads from the shared constraint system state.
  // FIXME: In the long run, we shouldn't need this.
  for (unsigned i = numResolvedOverloadSets,
                n = cs.solverState->resolvedOverloadSets.size();
       i != n; ++i) {
    cs.ResolvedOverloads.erase(cs.solverState->resolvedOverloadSets[i]);
  }

  // Remove generated overloads from the shared constraint system state.
  // FIXME: In the long run, we shouldn't need this.
  for (unsigned i = numGeneratedOverloadSets,
                n = cs.solverState->generatedOverloadSets.size();
       i != n; ++i) {
    cs.GeneratedOverloadSets.erase(cs.solverState->generatedOverloadSets[i]);
  }

  // Erase the end of various lists.
  truncate(cs.solverState->resolvedOverloadSets, numResolvedOverloadSets);
  truncate(cs.TypeVariables, numTypeVariables);
  truncate(cs.UnresolvedOverloadSets, numUnresolvedOverloadSets);
  truncate(cs.solverState->generatedOverloadSets, numGeneratedOverloadSets);

  // Restore bindings.
  cs.restoreTypeVariableBindings(cs.SavedBindings.size() - numSavedBindings);

  // Add the retired constraints back into circulation.
  for (unsigned i = numRetiredConstraints,
                n = cs.solverState->retiredConstraints.size();
       i != n; ++i) {
    assert(std::find(cs.Constraints.begin(), cs.Constraints.end(),
                     cs.solverState->retiredConstraints[i]) == cs.Constraints.end());
  }

  cs.Constraints.append(
    cs.solverState->retiredConstraints.begin() + numRetiredConstraints,
    cs.solverState->retiredConstraints.end());
  truncate(cs.solverState->retiredConstraints, numRetiredConstraints);

  // Remove any constraints that were generated here.
  llvm::SmallPtrSet<Constraint *, 4> generated;
  generated.insert(
    cs.solverState->generatedConstraints.begin() + numGeneratedConstraints,
    cs.solverState->generatedConstraints.end());
  cs.Constraints.erase(std::remove_if(cs.Constraints.begin(),
                                      cs.Constraints.end(),
                                      [&](Constraint *c) {
                                        return generated.count(c) > 0;
                                      }),
                       cs.Constraints.end());
  truncate(cs.solverState->generatedConstraints, numGeneratedConstraints);

  // Clear out other "failed" state.
  cs.failedConstraint = nullptr;
  cs.State = Active;
}

/// \brief Retrieve the set of potential type bindings for the given type
/// variable, along with flags indicating whether those types should be
/// opened.
static SmallVector<std::pair<Type, bool>, 4>
getPotentialBindings(ConstraintSystem &cs,
                     TypeVariableConstraints &tvc,
                     bool &involvesTypeVariables,
                     bool &hasLiteralBindings) {
  involvesTypeVariables = tvc.HasNonConcreteConstraints;
  hasLiteralBindings = false;

  llvm::SmallPtrSet<CanType, 4> exactTypes;
  SmallVector<std::pair<Type, bool>, 4> bindings;

  // Add the types below this type variable.
  for (auto arg : tvc.Below) {
    // Make sure we can perform this binding.
    auto type = arg.second;
    if (auto boundTy = checkTypeOfBinding(cs, tvc.TypeVar, type)) {
      type = *boundTy;

      // Check whether the type involves type variables.
      if (type->hasTypeVariable())
        involvesTypeVariables = true;
    } else {
      // If it's recursive, obviously it involves type variables.
      involvesTypeVariables = true;
      continue;
    }

    if (exactTypes.insert(type->getCanonicalType()))
      bindings.push_back({type, false});
  }

  // Add the types above this type variable.
  for (auto arg : tvc.Above) {
    // Make sure we can perform this binding.
    auto type = arg.second;
    if (auto boundTy = checkTypeOfBinding(cs, tvc.TypeVar, type)) {
      type = *boundTy;

      // Anything with a type variable in it is not definitive.
      if (type->hasTypeVariable())
        involvesTypeVariables = true;
    } else {
      // If it's recursive, obviously it involves type variables.
      involvesTypeVariables = true;
      continue;
    }

    if (exactTypes.insert(type->getCanonicalType()))
      bindings.push_back({type, false});
  }

  // Add the default literal types.
  auto &tc = cs.getTypeChecker();
  for (auto constraint : tvc.KindConstraints) {
    if (constraint->getKind() != ConstraintKind::Literal)
      continue;

    if (auto type = tc.getDefaultLiteralType(constraint->getLiteralKind())) {
      // For non-generic literal types, just check for exact types.
      if (!type->isUnspecializedGeneric()) {
        if (exactTypes.insert(type->getCanonicalType())) {
          hasLiteralBindings = true;
          bindings.push_back({type, true});
        }
        continue;
      }

      // For generic literal types, check whether we already have a
      // specialization of this generic within our list.
      auto nominal = type->getAnyNominal();
      bool matched = false;
      for (auto exactType : exactTypes) {
        if (auto exactNominal = exactType->getAnyNominal()) {
          // FIXME: Check parents?
          if (nominal == exactNominal) {
            matched = true;
            break;
          }
        }
      }

      if (!matched) {
        hasLiteralBindings = true;
        exactTypes.insert(type->getCanonicalType());
        bindings.push_back({type, true});
      }
    }
  }

  // FIXME: Minimize type bindings here by removing types that are supertypes
  // of others in the list.

  return bindings;
}

bool ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions) {
  // If there is no solver state, this is the top-level call. Create solver
  // state and begin recursion.
  if (!solverState) {
    // Set up solver state.
    SolverState state;
    this->solverState = &state;

    // Solve the system.
    solve(solutions);

    // If there is more than one viable system, attempt to pick the best
    // solution.
    if (solutions.size() > 1) {
      if (auto best = findBestSolution(solutions)) {
        if (best != &solutions[0])
          solutions[0] = std::move(*best);
        solutions.erase(solutions.begin() + 1, solutions.end());
      }
    }

    // Remove the solver state.
    this->solverState = nullptr;
    return solutions.size() != 1;
  }

  // Simplify this system.
  if (failedConstraint || simplify()) {
    return true;
  }

  // If there are no constraints remaining, we're done. Save this solution.
  if (Constraints.empty() && UnresolvedOverloadSets.empty()) {
    if (auto solution = finalize()) {
      if (TC.getLangOpts().DebugConstraintSolver) {
        llvm::errs().indent(solverState->depth * 2) << "(found solution)\n";
      }

      solutions.push_back(std::move(*solution));
      return false;
    }
    return true;
  }

  // Collect the type variable constraints.
  SmallVector<TypeVariableConstraints, 4> typeVarConstraints;
  collectConstraintsForTypeVariables(typeVarConstraints);
  if (!typeVarConstraints.empty()) {
    // Look for the best type variable to bind.
    unsigned bestTypeVarIndex = 0;
    bool bestInvolvesTypeVariables = false;
    bool bestHasLiteralBindings = false;
    SmallVector<std::pair<Type, bool>, 4> bestBindings
      = getPotentialBindings(*this,
                             typeVarConstraints[0],
                             bestInvolvesTypeVariables,
                             bestHasLiteralBindings);
    for (unsigned i = 1, n = typeVarConstraints.size(); i != n; ++i) {
      bool involvesTypeVariables = false;
      bool hasLiteralBindings = false;
      SmallVector<std::pair<Type, bool>, 4> bindings
        = getPotentialBindings(*this, typeVarConstraints[i],
                               involvesTypeVariables, hasLiteralBindings);
      if (bindings.empty())
        continue;
      
      // Prefer type variables whose bindings don't involve type variables or,
      // if neither involves type variables, those with fewer bindings.
      if (bestBindings.empty() ||
          std::make_tuple(involvesTypeVariables, hasLiteralBindings,
                          -bindings.size())
            <
          std::make_tuple(bestInvolvesTypeVariables, bestHasLiteralBindings,
                          -bestBindings.size())) {
        bestTypeVarIndex = i;
        bestInvolvesTypeVariables = involvesTypeVariables;
        bestBindings = std::move(bindings);
      }
    }

    // If we have a binding that does not involve type variables, or we have
    // no other option, go ahead and try the bindings for this type variable.
    if (!bestBindings.empty() &&
        (!bestInvolvesTypeVariables || UnresolvedOverloadSets.empty())) {
      auto typeVar = typeVarConstraints[bestTypeVarIndex].TypeVar;
      bool anySolved = false;
      unsigned tryCount = 0;
      llvm::SmallPtrSet<CanType, 4> exploredTypes;
      
    retry:
      anySolved = false;
      bool sawFirstLiteralConstraint = false;
      for (auto binding : bestBindings) {
        auto type = binding.first;
        if (TC.getLangOpts().DebugConstraintSolver) {
          llvm::errs().indent(solverState->depth * 2)
            << "(trying " << typeVar->getString()
            << " := " << type->getString() << "\n";
        }

        // Try to solve the system with typeVar := type
        SolverScope scope(*this);
        if (binding.second) {
          // FIXME: If we were able to solve this without considering
          // default literals.
          if (!sawFirstLiteralConstraint) {
            sawFirstLiteralConstraint = true;
            if (anySolved)
              break;
          }
          type = openType(type);
        }

        addConstraint(ConstraintKind::Equal, typeVar, type);
        if (!solve(solutions))
          anySolved = true;

        if (TC.getLangOpts().DebugConstraintSolver) {
          llvm::errs().indent(solverState->depth * 2) << ")\n";
        }
      }

      if (!anySolved) {
        // If none of the children had solutions, enumerate supertypes and
        // try again.
        SmallVector<std::pair<Type, bool>, 4> oldBindings;
        oldBindings.swap(bestBindings);

        // If this is our first attempt, note which bindings we already visited.
        if (tryCount == 0) {
          for (auto binding : oldBindings)
            exploredTypes.insert(binding.first->getCanonicalType());
        }

        // Enumerate the supertypes of each of the types we tried.
        bool handledLiteralConstraints = false;
        for (auto binding : oldBindings) {
          auto type = binding.first;

          // If this is a literal constraint, find all of the types that
          // conform to the literal constraint protocol.
          // FIXME: Except that we actually need said protocol :)
          if (binding.second) {
            if (handledLiteralConstraints)
              continue;

            for (auto constraint
                   : typeVarConstraints[bestTypeVarIndex].KindConstraints) {
              if (constraint->getKind() != ConstraintKind::Literal)
                continue;

              // FIXME: Total hack. integer literal -> default floating
              // literal kind.
              if (constraint->getLiteralKind() == LiteralKind::Int) {
                auto newType = TC.getDefaultLiteralType(LiteralKind::Float);
                if (exploredTypes.insert(newType->getCanonicalType()))
                  bestBindings.push_back({newType, false});
              }
            }

            handledLiteralConstraints = true;
            continue;
          }

          for (auto supertype : enumerateDirectSupertypes(type)) {
            // If we're not allowed to try this binding, skip it.
            auto simpleSuper = checkTypeOfBinding(*this, typeVar, supertype);
            if (!simpleSuper)
              continue;

            // If we haven't seen this supertype, add it.
            if (exploredTypes.insert((*simpleSuper)->getCanonicalType()))
              bestBindings.push_back({*simpleSuper, false});
          }
        }

        if (!bestBindings.empty()) {
          ++tryCount;
          goto retry;
        }
      }
      
      return !anySolved;
    }

    // Fall through to resolve an overload set.
  }

  // If there are no overload sets, we can't solve this system.
  if (UnresolvedOverloadSets.empty()) {
    return true;
  }

  // Find the overload set with the minimum number of overloads.
  unsigned bestSize = UnresolvedOverloadSets[0]->getChoices().size();
  unsigned bestIdx = 0;
  if (bestSize > 2) {
    for (unsigned i = 1, n = UnresolvedOverloadSets.size(); i < n; ++i) {
      unsigned newSize = UnresolvedOverloadSets[i]->getChoices().size();
      if (newSize < bestSize) {
        bestSize = newSize;
        bestIdx = i;

        if (bestSize == 2)
          break;
      }
    }
  }

  // Swap the best overload set to the end and pop it off the set of
  // unresolved overload sets. We'll restore it later.
  std::swap(UnresolvedOverloadSets[bestIdx], UnresolvedOverloadSets.back());
  OverloadSet *bestOvl = UnresolvedOverloadSets.back();
  UnresolvedOverloadSets.pop_back();

  // Try each of the overloads.
  bool anySolved = false;
  for (unsigned i = 0, n = bestOvl->getChoices().size(); i != n; ++i) {
    // Try to solve the system with this overload choice.
    SolverScope scope(*this);
    resolveOverload(bestOvl, i);
    if (!solve(solutions))
      anySolved = true;
  }

  // Put the best overload set back in place.
  UnresolvedOverloadSets.push_back(bestOvl);
  std::swap(UnresolvedOverloadSets[bestIdx], UnresolvedOverloadSets.back());

  return !anySolved;
}
