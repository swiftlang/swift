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
using namespace swift;
using namespace constraints;

//===--------------------------------------------------------------------===//
// Constraint solver statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint solver"
STATISTIC(NumSimplifyIterations, "# of simplification iterations");
STATISTIC(NumSupertypeFallbacks, "# of supertype fallbacks");
STATISTIC(NumLameNonDefinitive, "# of type variables lamely non-definitive");

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
    removeInactiveChildren();
    restoreTypeVariableBindings();
    return Nothing;

  case Solved:
    llvm_unreachable("Already finalized (?)");
    break;

  case Active:
    if (hasActiveChildren()) {
      // There is a solution below, but by itself this isn't a solution.
      removeInactiveChildren();
      clearIntermediateData();
      restoreTypeVariableBindings();
      return Nothing;
    }

    // Check whether we have a proper solution.
    if (Constraints.empty() && UnresolvedOverloadSets.empty() &&
        !hasFreeTypeVariables()) {
      State = Solved;
      auto result = createSolution();
      removeInactiveChildren();
      restoreTypeVariableBindings();
      return std::move(result);
    }

    // We don't have a solution;
    markUnsolvable();
    removeInactiveChildren();
      restoreTypeVariableBindings();
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

  Solution solution;
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
void ConstraintSystem::restoreTypeVariableBindings() {
  std::for_each(SavedBindings.rbegin(), SavedBindings.rend(),
                [](SavedTypeVariableBinding &saved) {
                  saved.restore();
                });
  SavedBindings.clear();
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
      // Mark the referenced type variables for both sides.
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
    existingConstraints.swap(Constraints);
    solvedAny = false;
    for (auto constraint : existingConstraints) {
      if (ExternallySolved.count(constraint))
        continue;

      if (addConstraint(constraint)) {
        solvedAny = true;

        if (TC.getLangOpts().DebugConstraintSolver)
          SolvedConstraints.push_back(constraint);        
      }

      if (failedConstraint) {
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
  /// \brief Describes the kind of step taking toward a potential
  /// solution.
  enum class SolutionStepKind : char {
    /// \brief Simplify the system again.
    Simplify,
    /// \brief Resolves an overload set by exploring each option.
    Overload, 
    /// \brief Binds a type variable to a given type.
    TypeBinding,
    /// \brief Explore potential bindings.
    ExploreBindings
  };

  /// \brief A step in the search for a solution, which may resolve a
  /// type variale or resolve an overload.
  class SolutionStep {
    /// \brief The kind of solution step this is.
    SolutionStepKind Kind;

    /// \brief Whether this solution step is "definitive".
    bool IsDefinitive;

    union {
      /// \brief The index of the overload set to be evaluated next.
      unsigned OvlSetIdx;

      /// \brief The type binding to perform. 
      struct {
        /// \brief The type variable to bind.
        TypeVariableType *TypeVar;
        /// \brief The type to which the type variable will be bound.
        TypeBase *BoundType;
        /// \brief Whether to open up the type to which the type variable
        /// will be bound.
        bool OpenType;
      } TypeBinding; 
    };

  public:
    /// \brief Construct a solution step for a simple kind, which needs no
    /// extra data.
    SolutionStep(SolutionStepKind kind) : Kind(kind) {
      assert(kind == SolutionStepKind::ExploreBindings ||
             kind == SolutionStepKind::Simplify);
    }

    /// \brief Construct a solution step that resolves an overload.
    ///
    /// Overload solution steps are always definitive.
    explicit SolutionStep(unsigned ovlSetIdx)
      : Kind(SolutionStepKind::Overload), IsDefinitive(true),
        OvlSetIdx(ovlSetIdx) { }

    /// \brief Construct a solution step that binds a type variable.
    SolutionStep(const PotentialBinding &binding,
                 bool isDefinitive)
      : Kind(SolutionStepKind::TypeBinding), IsDefinitive(isDefinitive),
        TypeBinding{binding.getTypeVariable(), binding.getType().getPointer(),
                    binding.shouldOpenType()} { }

    /// \brief Determine the kind of solution step to take.
    SolutionStepKind getKind() const { return Kind; }

    /// \brief Determine whether this solution step is definitive.
    ///
    /// A definitive solution step is a solution step that is guaranteed to
    /// be part of the solution, if a solution exists.
    bool isDefinitive() const { return IsDefinitive; }

    /// \brief Retrieve the overload set index.
    ///
    /// Only valid for an overload solution step.
    unsigned getOverloadSetIdx() const {
      assert(getKind() == SolutionStepKind::Overload);
      return OvlSetIdx;
    }

    /// \brief Retrieve the type variable and binding.
    ///
    /// Only valid for a type-binding solution step.
    PotentialBinding getTypeBinding() const {
      assert(getKind() == SolutionStepKind::TypeBinding);
      return { TypeBinding.TypeVar, Type(TypeBinding.BoundType),
               TypeBinding.OpenType };
    }
  };

  /// \brief Describes the kind of child system that we should build to take
  /// the next solution step.
  enum class ChildKind : char {
    /// \brief Don't build a child system at all; we're looking at the parent
    /// system.
    None,
    /// \brief Select a particular overload choice.
    OverloadChoice,
    /// \brief Bind a given type variable to a given type.
    TypeBinding
  };

  /// \brief Describes the child system that we should be building.
  class ChildDescription {
    ChildKind Kind;

    union {
      struct {
        unsigned SetIdx;
        unsigned ChoiceIdx;
      } Overload;

      PotentialBinding TypeBinding;
    };

  public:
    ChildDescription() : Kind(ChildKind::None) { }
    ChildDescription(unsigned ovlSetIdx, unsigned ovlChoiceIdx)
      : Kind(ChildKind::OverloadChoice), Overload{ovlSetIdx, ovlChoiceIdx} { }
    ChildDescription(const PotentialBinding &binding)
      : Kind(ChildKind::TypeBinding), TypeBinding(binding) { }

    ChildKind getKind() const { return Kind; }

    std::pair<unsigned, unsigned> getOverloadChoice() const {
      assert(getKind() == ChildKind::OverloadChoice);
      return {Overload.SetIdx, Overload.ChoiceIdx};
    }

    const PotentialBinding &getTypeBinding() const {
      assert(getKind() == ChildKind::TypeBinding);
      return TypeBinding;
    }
  };

  /// \brief Stack used to store the constraint systems to visit.
  typedef SmallVector<std::pair<ConstraintSystem *, ChildDescription>, 16>
    SolutionStack;
}

/// \brief Resolve an overload set in the given constraint system by
/// producing a set of child constraint systems, each of which picks a specific
/// overload from that set. Those child constraint systems that do not fail
/// during simplification will be added to the stack of constraint systems
/// being considered.
static void resolveOverloadSet(ConstraintSystem &cs,
                               unsigned ovlSetIdx,
                               SolutionStack &stack) {
  OverloadSet *ovl = cs.getUnresolvedOverloadSet(ovlSetIdx);
  auto choices = ovl->getChoices();
  for (unsigned i = 0, n = choices.size(); i != n; ++i) {
    stack.push_back({&cs, ChildDescription(ovlSetIdx, n-i-1)});
  }
}

/// \brief Find the lower and upper bounds on a type variable.
static std::pair<Type, Type>
findTypeVariableBounds(ConstraintSystem &cs, TypeVariableConstraints &tvc,
                       bool &isDefinitive) {
  isDefinitive = !tvc.HasNonConcreteConstraints && tvc.KindConstraints.empty();

  std::pair<Type, Type> bounds;
  for (auto arg : tvc.Below) {
    // Make sure we can perform this binding.
    auto type = arg.second;
    if (auto boundTy = checkTypeOfBinding(cs, tvc.TypeVar, type)) {
      type = *boundTy;

      // Anything with a type variable in it is not definitive.
      if (type->hasTypeVariable())
        isDefinitive = false;
    } else {
      // We can't perform this binding at all; the result will be
      // non-definitive.
      isDefinitive = false;
      continue;
    }

    if (bounds.first && !bounds.first->isEqual(type)) {
      // FIXME: Compute the meet of the types. We'll miss
      // potential solutions with the current approach.
      isDefinitive = false;
      ++NumLameNonDefinitive;
      break;
    }

    bounds.first = type;
  }

  for (auto arg : tvc.Above) {
    // Make sure we can perform this binding.
    auto type = arg.second;
    if (auto boundTy = checkTypeOfBinding(cs, tvc.TypeVar, type)) {
      type = *boundTy;

      // Anything with a type variable in it is not definitive.
      if (type->hasTypeVariable())
        isDefinitive = false;
    } else {
      // We can't perform this binding at all; the result will be
      // non-definitive.
      isDefinitive = false;
      continue;
    }

    if (bounds.second && !bounds.second->isEqual(type)) {
      // FIXME: Compute the join of the types. We'll miss
      // potential solutions with the current approach.
      isDefinitive = false;
      ++NumLameNonDefinitive;
      break;
    }

    bounds.second = type;
  }

  return bounds;
}

/// \brief Given a set of constraint/type pairs, find the unique concrete type.
///
/// \returns a pair (T, Conflict), where T is the unique concrete type
/// found (or null if there is no such type) and Conflict is a bool
/// indicating whether there was some kind of conflict that makes the
/// type variable we're solving for not have an obvious solution.
static std::pair<Type, bool> 
findUniqueConcreteType(ConstraintSystem &cs,
                       ArrayRef<std::pair<Constraint *, Type>> values) {
  Type result;
  for (const auto &value : values) {
    if (value.second->hasTypeVariable()) {
      return { Type(), true };
    }

    if (result && !result->isEqual(value.second)) {
      return { Type(), true };
    }


    result = value.second;
  }

  return { result, false };
}

/// \brief Given a set of 'kind' constraints, e.g., literal and
/// archetype constraints, find the unique type that satisfies these
/// constraints, if any.
///
/// \returns a pair (T, Conflict), where T is the unique concrete type
/// found (or null if there is no such type) and Conflict is a bool
/// indicating whether there was some kind of conflict that makes the
/// type variable we're solving for not have an obvious solution.
static std::pair<Type, bool> 
findUniqueKindType(ConstraintSystem &cs,
                   ArrayRef<Constraint *> constraints)  {
  auto &tc = cs.getTypeChecker();
  Type result;
  for (auto constraint : constraints) {
    if (constraint->getKind() != ConstraintKind::Literal) {
      return { Type(), true };
    }

    auto literalType = tc.getDefaultLiteralType(constraint->getLiteralKind());
    if (!literalType || (result && !result->isEqual(literalType))) {
      return { Type(), true };
    }

    result = literalType;
  }

  return { result, false };
}

/// \brief Identify type variables for which can we determine a
/// concrete binding, and immediately produce constraints describing
/// that binding.
///
/// Any binding produced is either part of all solutions to this
/// constraint system, or their are no solutions to this constraint
/// system.
///
/// \return true if any type variables were bound.
static bool bindDefinitiveTypeVariables(
              ConstraintSystem &cs,
              SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints) {
  bool foundAny = false;
  for (auto &tvc : typeVarConstraints) {
    if (tvc.HasNonConcreteConstraints)
      continue;

    // Find unique concrete type below.
    auto below = findUniqueConcreteType(cs, tvc.Below);
    if (below.second)
      continue;

    auto type = below.first;
    if (!type) {
      // Find unique concrete type above.
      auto above = findUniqueConcreteType(cs, tvc.Above);
      if (above.second)
        continue;

      type = above.first;
    }

    // Find unique kind type.
    auto kind = findUniqueKindType(cs, tvc.KindConstraints);
    if (kind.second)
      continue;

    if (kind.first) {
      if (type && !type->isEqual(kind.first))
        continue;

      // Open up the default literal type we chose.
      type = cs.openBindingType(kind.first);
    } else if (!type) {
      continue;
    }
    assert(type && "missing type?");

    // We found a type. Use it.
    cs.addConstraint(ConstraintKind::Equal, tvc.TypeVar, type);
    foundAny = true;

    // Directly solve the constraints associated with this type variable.
    for (const auto &below : tvc.Below) {
      cs.addConstraint(below.first, /*isExternallySolved=*/true);
    }
    for (const auto &above : tvc.Above) {
      cs.addConstraint(above.first, /*isExternallySolved=*/true);
    }
    for (auto constraint : tvc.KindConstraints) {
      cs.addConstraint(constraint, /*isExternallySolved=*/true);
    }
  }

  return foundAny;
}

/// \brief Given the direct constraints placed on the type variables within
/// a given constraint system, create potential bindings that resolve the
/// constraints for a type variable.
///
/// \returns If successful, a pair containing a potential binding and a bit
/// indicating whether this binding is definitive.
static Optional<std::pair<PotentialBinding, bool>>
resolveTypeVariable(
  ConstraintSystem &cs,
  SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints,
  bool onlyDefinitive)
{

  // Find a type variable with a lower bound, because those are the most
  // interesting.
  TypeVariableConstraints *tvcWithUpper = nullptr;
  Type tvcUpper;
  bool tvcWithUpperIsDefinitive;
  for (auto &tvc : typeVarConstraints) {
    // If we already explored type bindings for this type variable, skip it.
    if (cs.haveExploredTypeVar(tvc.TypeVar))
      continue;

    // If we're only allowed to look at type variables with concrete
    // constraints, and this type variable has non-concrete constraints,
    // skip it.
    if (onlyDefinitive && tvc.HasNonConcreteConstraints)
      continue;

    bool isDefinitive;
    auto bounds = findTypeVariableBounds(cs, tvc, isDefinitive);

    if (onlyDefinitive && !isDefinitive)
      continue;

    // If we have a lower bound, introduce a child constraint system binding
    // this type variable to that lower bound.
    if (bounds.first) {
      return { PotentialBinding(tvc.TypeVar, bounds.first, false),
               isDefinitive };
    }

    // If there is an upper bound, record that (but don't use it yet).
    if (bounds.second && !tvcWithUpper) {
      tvcWithUpper = &tvc;
      tvcUpper = bounds.second;
      tvcWithUpperIsDefinitive = isDefinitive;
    }
  }

  // If we had an upper bound, introduce a child constraint system binding
  // this type variable to that upper bound.
  // FIXME: This type may be too specific, but we'd really rather not
  // go on a random search for subtypes that might work.
  if (tvcWithUpper) {
    return { PotentialBinding(tvcWithUpper->TypeVar, tvcUpper, false),
             tvcWithUpperIsDefinitive };
  }

  // If we were only looking for concrete bindings, don't consider literal
  // bindings.
  if (onlyDefinitive)
    return Nothing;

  // If there are any literal constraints, add the default literal values as
  // potential bindings.
  auto &tc = cs.getTypeChecker();
  for (auto &tvc : typeVarConstraints) {
    // If we already explored type bindings for this type variable, skip it.
    if (cs.haveExploredTypeVar(tvc.TypeVar))
      continue;

    for (auto &constraint : tvc.KindConstraints) {
      if (constraint->getClassification() != ConstraintClassification::Literal)
        continue;

      if (auto type = tc.getDefaultLiteralType(constraint->getLiteralKind())) {
        return { PotentialBinding(tvc.TypeVar, type, true), false };
      }
    }
  }

  // We're out of ideas.
  return Nothing;
}

/// \brief Determine the next step to take toward finding a solution
/// based on the given constraint system.
///
/// \returns The next solution step, or an empty \c Optional if we've
/// run out of ideas.
static Optional<SolutionStep> getNextSolutionStep(ConstraintSystem &cs) {
  // If this constraint system has a failed constraint, there's nothing to do.
  if (cs.getFailedConstraint())
    return Nothing;

  // If this constraint system resolved overloads in a child system, there is
  // nothing more we can do with it.
  if (cs.hasResolvedOverloadsInChildSystems())
    return Nothing;

  // If there are any potential bindings to explore, do it now.
  if (cs.hasPotentialBindings()) {
    return SolutionStep(SolutionStepKind::ExploreBindings);
  }
  
  SmallVector<TypeVariableConstraints, 16> typeVarConstraints;
  cs.collectConstraintsForTypeVariables(typeVarConstraints);

  // If there are any type variables that we can definitively solve,
  // do so now.
  if (bindDefinitiveTypeVariables(cs, typeVarConstraints)) {
    return SolutionStep(SolutionStepKind::Simplify);
  }

  // If there are any unresolved overload sets, resolve one now.
  // FIXME: This is terrible for performance.
  if (cs.getNumUnresolvedOverloadSets() > 0) {
    // Find the overload set with the minimum number of overloads.
    unsigned minSize = cs.getUnresolvedOverloadSet(0)->getChoices().size();
    unsigned minIdx = 0;
    if (minSize > 2) {
      for (unsigned i = 1, n = cs.getNumUnresolvedOverloadSets(); i < n; ++i) {
        unsigned newSize = cs.getUnresolvedOverloadSet(i)->getChoices().size();
        if (newSize < minSize) {
          minSize = newSize;
          minIdx = i;

          if (minSize == 2)
            break;
        }
      }
    }

    // Resolve the unresolved overload set with the minimum number of overloads.
    return SolutionStep(minIdx);
  }

  // Try to determine a binding for a type variable.
  if (auto binding = resolveTypeVariable(cs, typeVarConstraints,
                                         /*onlyDefinitive=*/false)) {
    return SolutionStep(binding->first, binding->second);
  }

  // We're out of ideas.
  return Nothing;
}

bool ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions) {
  assert(!Parent &&"Can only solve at the top level");

  // Simplify this constraint system.
  if (TC.getLangOpts().DebugConstraintSolver) {
    llvm::errs() << "---Simplified constraints---\n";
  }
  bool error = simplify();
  if (TC.getLangOpts().DebugConstraintSolver) {
    dump();
  }
  if (error)
    return true;

  // Seed the constraint system stack with ourselves.
  SolutionStack stack;
  stack.push_back({this, ChildDescription()});

  // While there are still constraint systems to search, do so.
  while (!stack.empty()) {
    auto csAndChildDesc = stack.back();
    auto cs = csAndChildDesc.first;
    auto childDesc = csAndChildDesc.second;
    stack.pop_back();

    // If we're supposed to build a child system, do so now (and simplify it).
    switch (childDesc.getKind()) {
    case ChildKind::None:
      break;

    case ChildKind::OverloadChoice: {
      auto ovl = childDesc.getOverloadChoice();
      if (auto childCS = cs->createDerivedConstraintSystem(ovl.first,
                                                           ovl.second)) {
        cs = childCS;
        break;
      }

      continue;
    }

    case ChildKind::TypeBinding: {
      auto binding = childDesc.getTypeBinding();
      if (auto childCS = cs->createDerivedConstraintSystem(binding)) {
        cs = childCS;
        break;
      }
      continue;
    }
    }
    
    // If there are any children of this system that are still active,
    // then we found a potential solution. There is no need to explore
    // alternatives based on this constraint system.
    if (cs->hasActiveChildren()) {
      cs->finalize();
      continue;
    }

    // If there are no unsolved constraints and no unresolved overload sets,
    // this system is either a solution or it is underconstrained.
    if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
      if (auto solution = cs->finalize())
        solutions.push_back(std::move(*solution));
      continue;
    }

    // While we have something interesting to do with this constraint
    // system, do it.
    bool done = false;
    while (auto step = getNextSolutionStep(*cs)) {
      switch (step->getKind()) {
      case SolutionStepKind::Simplify:
        // Simplify the system again.
        if (cs->simplify()) {
          cs->finalize();
          done = true;
        }
        break;

      case SolutionStepKind::Overload: {
        // Resolve the overload set.
        assert(step->isDefinitive() && "Overload solutions are definitive");
        cs->ResolvedOverloadsInChildSystems = true;
        stack.push_back({cs, ChildDescription()});
        resolveOverloadSet(*cs, step->getOverloadSetIdx(), stack);
        done = true;
        break;
      }

      case SolutionStepKind::TypeBinding: {
        // We have a type binding.
        auto binding = step->getTypeBinding();

        // If we have a definitive type binding, apply it immediately;
        // there's no point in creating a child system.
        if (step->isDefinitive()) {
          auto type = binding.getType();
          if (binding.shouldOpenType())
            type = openBindingType(type);
          cs->addConstraint(ConstraintKind::Equal,
                            binding.getTypeVariable(), type);
          if (cs->simplify()) {
            cs->finalize();
            done = true;
          }
          break;
        }
        
        // Add this type binding as a potential binding; we'll
        // explore potential bindings below.
        cs->addPotentialBinding(binding);

        // Fall though to explore this binding.
      }
          
      case SolutionStepKind::ExploreBindings: {
        // If there are no potential bindings, we have nothing else we can
        // explore. Consider this system dead.
        if (cs->PotentialBindings.empty()) {
          cs->markUnsolvable();
          cs->finalize();
          done = true;
          break;
        }

        // Push this constraint system back onto the stack to be reconsidered if
        // none of the child systems created below succeed.
        stack.push_back({cs, ChildDescription()});

        // Create child systems for each of the potential bindings.
        auto potentialBindings = std::move(cs->PotentialBindings);
        cs->PotentialBindings.clear();
        for (auto binding : potentialBindings) {
          if (cs->exploreBinding(binding.getTypeVariable(), binding.getType())) {
            stack.push_back({cs, ChildDescription(binding)});
          }
        }
        done = true;
        break;
      }
      }

      if (done)
        break;

      if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
        if (auto solution = cs->finalize())
          solutions.push_back(std::move(*solution));
        done = true;
        break;
      }
    }

    if (!done) {
      // If there are no unsolved constraints and no unresolved overload sets,
      // this system is either a solution or it is underconstrained.
      if (cs->Constraints.empty() && cs->UnresolvedOverloadSets.empty()) {
        if (auto solution = cs->finalize())
          solutions.push_back(std::move(*solution));
        continue;
      }

      // We couldn't do anything with the system, so it's unsolvable.
      cs->markUnsolvable();
      cs->finalize();
    }
  }


  // If there is more than one viable system, attempt to pick the best solution.
  if (solutions.size() > 1) {
    if (auto best = findBestSolution(solutions)) {
      if (best != &solutions[0])
        solutions[0] = std::move(*best);
      solutions.erase(solutions.begin() + 1, solutions.end());
    }
  }

  return solutions.size() != 1;
}
