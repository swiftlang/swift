//===--- CSSolver.cpp - Constraint Solver ---------------------------------===//
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
#include "ConstraintGraph.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
#include <memory>
#include <tuple>
using namespace swift;
using namespace constraints;

//===--------------------------------------------------------------------===//
// Constraint solver statistics
//===--------------------------------------------------------------------===//
#define DEBUG_TYPE "Constraint solver overall"
#define JOIN(X,Y) JOIN2(X,Y)
#define JOIN2(X,Y) X##Y
STATISTIC(NumSolutionAttempts, "# of solution attempts");

#define CS_STATISTIC(Name, Description) \
  STATISTIC(JOIN2(Overall,Name), Description);
#include "ConstraintSolverStats.def"

#undef DEBUG_TYPE
#define DEBUG_TYPE "Constraint solver largest system"
#define CS_STATISTIC(Name, Description) \
  STATISTIC(JOIN2(Largest,Name), Description);
#include "ConstraintSolverStats.def"
STATISTIC(LargestSolutionAttemptNumber, "# of the largest solution attempt");

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
  // FIXME: This is a hack. We need to be smarter about whether there's enough
  // structure in the type to produce an interesting binding, or not.
  if (type->getRValueType()->is<TypeVariableType>())
    return Nothing;

  // Okay, allow the binding (with the simplified type).
  return type;
}

Solution ConstraintSystem::finalize(
           FreeTypeVariableBinding allowFreeTypeVariables) {
  // Create the solution.
  Solution solution(*this, CurrentScore);

  // Update the best score we've seen so far.
  if (solverState) {
    assert(!solverState->BestScore || CurrentScore <= *solverState->BestScore);
    solverState->BestScore = CurrentScore;
  }

  // For any of the type variables that has no associated fixed type, assign a
  // fresh generic type parameters.
  // FIXME: We could gather the requirements on these as well.
  unsigned index = 0;
  for (auto tv : TypeVariables) {
    if (getFixedType(tv))
      continue;

    switch (allowFreeTypeVariables) {
    case FreeTypeVariableBinding::Disallow:
      llvm_unreachable("Solver left free type variables");

    case FreeTypeVariableBinding::Allow:
      break;

    case FreeTypeVariableBinding::GenericParameters:
      assignFixedType(tv, GenericTypeParamType::get(0, index++, TC.Context));
      break;
    }
  }

  // For each of the type variables, get its fixed type.
  for (auto tv : TypeVariables) {
    solution.typeBindings[tv] = simplifyType(tv);
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
    for (auto &restriction : solverState->constraintRestrictions) {
      using std::get;
      CanType first = simplifyType(get<0>(restriction))->getCanonicalType();
      CanType second = simplifyType(get<1>(restriction))->getCanonicalType();
      solution.constraintRestrictions[{first, second}] = get<2>(restriction);
    }
  }

  return std::move(solution);
}

void ConstraintSystem::applySolution(const Solution &solution) {
  // Update the score.
  CurrentScore += solution.getFixedScore();

  // Assign fixed types to the type variables solved by this solution.
  llvm::SmallPtrSet<TypeVariableType *, 4> 
    knownTypeVariables(TypeVariables.begin(), TypeVariables.end());
  for (auto binding : solution.typeBindings) {
    // If we haven't seen this type variable before, record it now.
    if (knownTypeVariables.insert(binding.first))
      TypeVariables.push_back(binding.first);

    // If we don't already have a fixed type for this type variable,
    // assign the fixed type from the solution.
    if (!getFixedType(binding.first) && !binding.second->hasTypeVariable())
      assignFixedType(binding.first, binding.second, /*updateScore=*/false);
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
  for (auto restriction : solution.constraintRestrictions) {
    solverState->constraintRestrictions.push_back(
        std::make_tuple(restriction.first.first, restriction.first.second,
                        restriction.second));
  }
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
    int scalarIdx = tupleTy->getFieldForScalarInit();
    if (scalarIdx >= 0) {
      auto &elt = tupleTy->getFields()[scalarIdx];
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
    if (auto superclass = tc.getSuperClassOf(type))
      result.push_back(superclass);
  }

  if (auto lvalue = type->getAs<LValueType>()) {
    if (lvalue->getQualifiers().isImplicit()) {
      result.push_back(lvalue->getObjectType());
    }
  }

  // FIXME: lots of other cases to consider!
  return result;
}

bool ConstraintSystem::simplify() {
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

      if (solverState)
        solverState->retiredConstraints.push_front(constraint);

      break;

    case SolutionKind::Solved:
      ++solverState->NumSimplifiedConstraints;

      // This constraint has already been solved; retire it.
      if (solverState)
        solverState->retiredConstraints.push_front(constraint);

      // Remove the constraint from the constraint graph.
      CG.removeConstraint(constraint);
      break;

    case SolutionKind::Unsolved:
      ++solverState->NumUnsimplifiedConstraints;

      InactiveConstraints.push_back(constraint);
      break;
    }

    // This constraint is not active. We delay this operation until
    // after simplification to avoid re-insertion.
    constraint->setActive(false);

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

/// \brief Truncate the given small vector to the given new size.
template<typename T>
void truncate(SmallVectorImpl<T> &vec, unsigned newSize) {
  assert(newSize <= vec.size() && "Not a truncation!");
  vec.erase(vec.begin() + newSize, vec.end());
}

} // end anonymous namespace

ConstraintSystem::SolverState::SolverState(ConstraintSystem &cs) : CS(cs) {
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
    CS.dump(dbgOut);
  }
}

ConstraintSystem::SolverState::~SolverState() {
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
  ++cs.solverState->depth;

  resolvedOverloadSets = cs.resolvedOverloadSets;
  numTypeVariables = cs.TypeVariables.size();
  numSavedBindings = cs.solverState->savedBindings.size();
  firstRetired = cs.solverState->retiredConstraints.begin();
  numConstraintRestrictions = cs.solverState->constraintRestrictions.size();
  numGeneratedConstraints = cs.solverState->generatedConstraints.size();
  PreviousScore = cs.CurrentScore;

  ++cs.solverState->NumStatesExplored;
}

ConstraintSystem::SolverScope::~SolverScope() {
  --cs.solverState->depth;

  // Erase the end of various lists.
  cs.resolvedOverloadSets = resolvedOverloadSets;
  truncate(cs.TypeVariables, numTypeVariables);

  // Restore bindings.
  cs.restoreTypeVariableBindings(cs.solverState->savedBindings.size() -
                                   numSavedBindings);

  // Move any remaining active constraints into the inactive list.
  while (!cs.ActiveConstraints.empty()) {
    for (auto &constraint : cs.ActiveConstraints) {
      constraint.setActive(false);
    }
    cs.InactiveConstraints.splice(cs.InactiveConstraints.end(),
                                  cs.ActiveConstraints);
  }

  // Add the retired constraints back into circulation.
  cs.InactiveConstraints.splice(cs.InactiveConstraints.end(), 
                                cs.solverState->retiredConstraints,
                                cs.solverState->retiredConstraints.begin(),
                                firstRetired);

  // Remove any constraints that were generated here.
  auto &generatedConstraints = cs.solverState->generatedConstraints;
  auto genStart = generatedConstraints.begin() + numGeneratedConstraints,
       genEnd = generatedConstraints.end();
  for (auto genI = genStart; genI != genEnd; ++genI) {
    cs.InactiveConstraints.erase(ConstraintList::iterator(*genI));
  }
  generatedConstraints.erase(genStart, genEnd);

  // Remove any constraint restrictions.
  truncate(cs.solverState->constraintRestrictions, numConstraintRestrictions);

  // Reset the previous score.
  cs.CurrentScore = PreviousScore;

  // Clear out other "failed" state.
  cs.failedConstraint = nullptr;
}

namespace {
  /// The kind of bindings that are permitted.
  enum class AllowedBindingKind : unsigned char {
    /// Only the exact type.
    Exact,
    /// Supertypes of the specified type.
    Supertypes,
    /// Subtypes of the specified type.
    Subtypes
  };

  /// A potential binding from the type variable to a particular type,
  /// along with information that can be used to construct related
  /// bindings, e.g., the supertypes of a given type.
  struct PotentialBinding {
    /// The type to which the type variable can be bound.
    Type BindingType;

    /// The kind of bindings permitted.
    AllowedBindingKind Kind;

    /// The defaulted protocol associated with this binding.
    Optional<ProtocolDecl *> DefaultedProtocol;    
  };

  struct PotentialBindings {
    /// The set of potential bindings.
    SmallVector<PotentialBinding, 4> Bindings;

    /// Whether this type variable is fully bound by one of its constraints.
    bool FullyBound = false;

    /// Whether the bindings of this type involve other type variables.
    bool InvolvesTypeVariables = false;

    /// Whether this type variable has literal bindings.
    bool HasLiteralBindings = false;

    /// Determine whether the set of bindings is non-empty.
    explicit operator bool() const {
      return !Bindings.empty();
    }

    /// Compare two sets of bindings, where \c x < y indicates that
    /// \c x is a better set of bindings that \c y.
    friend bool operator<(const PotentialBindings &x, 
                          const PotentialBindings &y) {
      return std::make_tuple(x.FullyBound, x.InvolvesTypeVariables, 
                             x.HasLiteralBindings, -x.Bindings.size())
        < std::make_tuple(y.FullyBound, y.InvolvesTypeVariables, 
                          y.HasLiteralBindings, -y.Bindings.size());
    }
  };
}

/// Determine whether the given type variables occurs in the given type.
static bool typeVarOccursInType(ConstraintSystem &cs, TypeVariableType *typeVar,
                                Type type, bool &involvesOtherTypeVariables) {
  SmallVector<TypeVariableType *, 4> typeVars;
  type->getTypeVariables(typeVars);
  bool result = false;
  for (auto referencedTypeVar : typeVars) {
    if (cs.getRepresentative(referencedTypeVar) == typeVar) {
      result = true;
      if (involvesOtherTypeVariables)
        break;

      continue;
    }

    involvesOtherTypeVariables = true;
  }

  return result;
}

/// \brief Retrieve the set of potential type bindings for the given
/// representative type variable, along with flags indicating whether
/// those types should be opened.
static PotentialBindings getPotentialBindings(ConstraintSystem &cs,
                                              TypeVariableType *typeVar) {
  assert(typeVar->getImpl().getRepresentative(nullptr) == typeVar &&
         "not a representative");
  assert(!typeVar->getImpl().getFixedType(nullptr) && "has a fixed type");

  // Gather the constraints associated with this type variable.
  SmallVector<Constraint *, 8> constraints;
  llvm::SmallPtrSet<Constraint *, 4> visitedConstraints;
  cs.getConstraintGraph().gatherConstraints(typeVar, constraints);

  // Consider each of the constraints related to this type variable.
  PotentialBindings result;
  llvm::SmallPtrSet<CanType, 4> exactTypes;
  auto &tc = cs.getTypeChecker();
  for (auto constraint : constraints) {
    // Only visit each constraint once.
    if (!visitedConstraints.insert(constraint))
      continue;

    switch (constraint->getKind()) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
      // Relational constraints: break out to look for types above/below.
      break;

    case ConstraintKind::CheckedCast:
      // FIXME: Relational constraints for which we could perhaps do better
      // than the default.
      break;

    case ConstraintKind::Construction:
      // Can't infer anything from a construction constraint.
      if (result.InvolvesTypeVariables)
        continue;

      // Check whether the left-hand side involves any other type variables.
      // FIXME: Can we avoid simplification here?
      typeVarOccursInType(cs, typeVar, 
                          cs.simplifyType(constraint->getFirstType()),
                          result.InvolvesTypeVariables);
      if (result.InvolvesTypeVariables)
        continue;

      // Check whether the right-hand side involves any other type variables.
      // FIXME: Can we avoid simplification here?
      typeVarOccursInType(cs, typeVar, 
                          cs.simplifyType(constraint->getSecondType()),
                          result.InvolvesTypeVariables);
      continue;

    case ConstraintKind::Archetype:
    case ConstraintKind::Class:
    case ConstraintKind::DynamicLookupValue:
      // Constraints from which we can't do anything.
      // FIXME: Record this somehow?
      continue;

    case ConstraintKind::Conjunction:
      llvm_unreachable("conjunctions should have been unwrapped");

    case ConstraintKind::Disjunction:
      // FIXME: Recurse into these constraints to see whether this
      // type variable is fully bound by any of them.
      result.InvolvesTypeVariables = true;
      continue;

    case ConstraintKind::ConformsTo: 
    case ConstraintKind::SelfObjectOfProtocol: {
      // FIXME: Can we always assume that the type variable is the lower bound?
      TypeVariableType *lowerTypeVar = nullptr;
      cs.getFixedTypeRecursive(constraint->getFirstType(), lowerTypeVar,
                               /*wantRValue=*/false);
      if (lowerTypeVar != typeVar) {
        continue;
      }

      // If there is a default literal type for this protocol, it's a
      // potential binding.
      auto defaultType = tc.getDefaultType(constraint->getProtocol(), cs.DC);
      if (!defaultType)
        continue;

      // Handle unspecialized types directly.
      if (!defaultType->isUnspecializedGeneric()) {
        if (!exactTypes.insert(defaultType->getCanonicalType()))
          continue;

        result.HasLiteralBindings = true;
        result.Bindings.push_back({defaultType, AllowedBindingKind::Subtypes,
                                   constraint->getProtocol()});
        continue;
      }

      // For generic literal types, check whether we already have a
      // specialization of this generic within our list.
      // FIXME: This assumes that, e.g., the default literal
      // int/float/char/string types are never generic.
      auto nominal = defaultType->getAnyNominal();
      if (!nominal)
        continue;

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
        result.HasLiteralBindings = true;
        exactTypes.insert(defaultType->getCanonicalType());
        result.Bindings.push_back({defaultType, AllowedBindingKind::Subtypes,
                                   constraint->getProtocol()});
      }

      continue;
    }

    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload: {
      // If this variable is in the left-hand side, it is fully bound.
      // FIXME: Can we avoid simplification here by walking the graph? Is it
      // worthwhile?
      if (typeVarOccursInType(cs, typeVar,
                              cs.simplifyType(constraint->getFirstType()),
                              result.InvolvesTypeVariables)) {
        result.FullyBound = true;
      }
      continue;
    }

    case ConstraintKind::ValueMember:
    case ConstraintKind::TypeMember:
      // If our type variable shows up in the base type, there's
      // nothing to do.
      // FIXME: Can we avoid simplification here?
      if (typeVarOccursInType(cs, typeVar, 
                              cs.simplifyType(constraint->getFirstType()),
                              result.InvolvesTypeVariables)) {
        continue;
      }
      
      // If the type variable is in the list of member type
      // variables, it is fully bound.
      // FIXME: Can we avoid simplification here?
      if (typeVarOccursInType(cs, typeVar, 
                              cs.simplifyType(constraint->getSecondType()),
                              result.InvolvesTypeVariables)) {
        result.FullyBound = true;
      }
      continue;
    }

    // Handle relational constraints.
    assert(constraint->getClassification() 
             == ConstraintClassification::Relational && 
           "only relational constraints handled here");
    
    auto first = cs.simplifyType(constraint->getFirstType());
    auto second = cs.simplifyType(constraint->getSecondType());

    Type type;
    AllowedBindingKind kind;
    if (first->getAs<TypeVariableType>() == typeVar) {
      // Upper bound for this type variable.
      type = second;
      kind = AllowedBindingKind::Subtypes;
    } else if (second->getAs<TypeVariableType>() == typeVar) {
      // Lower bound for this type variable.
      type = first;
      kind = AllowedBindingKind::Supertypes;
    } else {
      // Can't infer anything.
      if (!result.InvolvesTypeVariables)
        typeVarOccursInType(cs, typeVar, first, result.InvolvesTypeVariables);
      if (!result.InvolvesTypeVariables)
        typeVarOccursInType(cs, typeVar, second, result.InvolvesTypeVariables);
      continue;
    }

    // Check whether we can perform this binding.
    // FIXME: this has a super-inefficient extraneous simplifyType() in it.
    if (auto boundType = checkTypeOfBinding(cs, typeVar, type)) {
      type = *boundType;
      if (type->hasTypeVariable())
        result.InvolvesTypeVariables = true;
    } else {
      result.InvolvesTypeVariables = true;
      continue;
    }

    // Don't deduce autoclosure types or single-element, non-variadic
    // tuples.
    if (constraint->getKind() == ConstraintKind::Conversion ||
        constraint->getKind() == ConstraintKind::Subtype ||
        constraint->getKind() == ConstraintKind::TrivialSubtype) {
      if (auto funcTy = type->getAs<FunctionType>()) {
        if (funcTy->isAutoClosure())
          type = funcTy->getResult();
      }

      if (auto tupleTy = type->getAs<TupleType>()) {
        if (tupleTy->getNumElements() == 1 &&
            !tupleTy->getFields()[0].isVararg())
          type = tupleTy->getElementType(0);
      }
    }

    // Make sure we aren't trying to equate type variables with different
    // lvalue-binding rules.
    if (auto otherTypeVar = type->getAs<TypeVariableType>()) {
      if (typeVar->getImpl().canBindToLValue() !=
            otherTypeVar->getImpl().canBindToLValue())
        continue;
    }

    if (exactTypes.insert(type->getCanonicalType()))
      result.Bindings.push_back({type, kind, Nothing});
  }

  return result;
}

/// \brief Try each of the given type variable bindings to find solutions
/// to the given constraint system.
///
/// \param cs The constraint system we're solving in.
/// \param depth The depth of the solution stack.
/// \param typeVar The type variable we're binding.
/// \param bindings The initial set of bindings to explore.
/// \param solutions The set of solutions.
///
/// \returns true if there are no solutions.
static bool tryTypeVariableBindings(
              ConstraintSystem &cs,
              unsigned depth,
              TypeVariableType *typeVar,
              ArrayRef<PotentialBinding> bindings,
              SmallVectorImpl<Solution> &solutions,
              FreeTypeVariableBinding allowFreeTypeVariables) {
  bool anySolved = false;
  llvm::SmallPtrSet<CanType, 4> exploredTypes;

  SmallVector<PotentialBinding, 4> storedBindings;
  auto &tc = cs.getTypeChecker();
  ++cs.solverState->NumTypeVariablesBound;

  for (unsigned tryCount = 0; !anySolved && !bindings.empty(); ++tryCount) {
    // Try each of the bindings in turn.
    ++cs.solverState->NumTypeVariableBindings;
    bool sawFirstLiteralConstraint = false;
    for (auto binding : bindings) {
      auto type = binding.BindingType;

      // If the type variable can't bind to an lvalue, make sure the
      // type we pick isn't an lvalue.
      if (!typeVar->getImpl().canBindToLValue())
        type = type->getRValueType();

      if (tc.getLangOpts().DebugConstraintSolver) {
        auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
        log.indent(depth * 2)
          << "(trying " << typeVar->getString()
          << " := " << type->getString() << "\n";
      }

      // Try to solve the system with typeVar := type
      ConstraintSystem::SolverScope scope(cs);
      if (binding.DefaultedProtocol) {
        // If we were able to solve this without considering
        // default literals, don't bother looking at default literals.
        if (!sawFirstLiteralConstraint) {
          sawFirstLiteralConstraint = true;
          if (anySolved)
            break;
        }
        type = cs.openBindingType(type);
      }

      cs.addConstraint(ConstraintKind::Bind, typeVar, type);
      if (!cs.solve(solutions, allowFreeTypeVariables))
        anySolved = true;

      if (tc.getLangOpts().DebugConstraintSolver) {
        auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
        log.indent(depth * 2) << ")\n";
      }
    }

    // If we found any solution, we're done.
    if (anySolved)
      break;

    // None of the children had solutions, enumerate supertypes and
    // try again.
    SmallVector<PotentialBinding, 4> newBindings;

    // Check whether this was our first attempt.
    if (tryCount == 0) {
      // Note which bindings we already visited.
      for (auto binding : bindings) {
        exploredTypes.insert(binding.BindingType->getCanonicalType());

        // If we have a protocol with a default type, look for alternative
        // types to the default.
        if (binding.DefaultedProtocol) {
          KnownProtocolKind knownKind 
            = *((*binding.DefaultedProtocol)->getKnownProtocolKind());
          for (auto type : cs.getAlternativeLiteralTypes(knownKind)) {
            if (exploredTypes.insert(type->getCanonicalType()))
              newBindings.push_back({type, AllowedBindingKind::Subtypes, 
                                     binding.DefaultedProtocol});
          }
        }
      }

      // If we found any new bindings, try them now.
      if (!newBindings.empty()) {
        // We have a new set of bindings; use them for our next loop.
        storedBindings = std::move(newBindings);
        bindings = storedBindings;
        continue;
      }
    }

    // Enumerate the supertypes of each of the types we tried.
    for (auto binding : bindings) {
      auto type = binding.BindingType;

      // Handle simple subtype bindings.
      if (binding.Kind == AllowedBindingKind::Subtypes &&
          typeVar->getImpl().canBindToLValue() &&
          !type->is<LValueType>()) {
        // FIXME: It's horrible that we have to enumerate different
        // bits on the lvalue type.
        auto subtype = LValueType::get(type,
                                       LValueType::Qual::DefaultForVar);
        if (exploredTypes.insert(subtype->getCanonicalType()))
          newBindings.push_back({subtype, binding.Kind, Nothing});

        subtype = LValueType::get(type,
                                  (LValueType::Qual::DefaultForVar|
                                   LValueType::Qual::NonSettable));
        if (exploredTypes.insert(subtype->getCanonicalType()))
          newBindings.push_back({subtype, binding.Kind, Nothing});
        
      }


      if (binding.Kind != AllowedBindingKind::Supertypes)
        continue;

      for (auto supertype : enumerateDirectSupertypes(cs.getTypeChecker(),
                                                      type)) {
        // If we're not allowed to try this binding, skip it.
        auto simpleSuper = checkTypeOfBinding(cs, typeVar, supertype);
        if (!simpleSuper)
          continue;

        // If we haven't seen this supertype, add it.
        if (exploredTypes.insert((*simpleSuper)->getCanonicalType()))
          newBindings.push_back({*simpleSuper, binding.Kind, Nothing});
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

bool ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions,
                             FreeTypeVariableBinding allowFreeTypeVariables) {
  // If there is no solver state, this is the top-level call. Create solver
  // state and begin recursion.
  if (!solverState) {
    // Set up solver state.
    SolverState state(*this);
    this->solverState = &state;

    // Solve the system.
    solve(solutions, allowFreeTypeVariables);

    // If there is more than one viable system, attempt to pick the best
    // solution.
    if (solutions.size() > 1) {
      if (auto best = findBestSolution(solutions, /*minimize=*/false)) {
        if (*best != 0)
          solutions[0] = std::move(solutions[*best]);
        solutions.erase(solutions.begin() + 1, solutions.end());
      }
    }

    // Remove the solver state.
    this->solverState = nullptr;
    return solutions.size() != 1;
  }

  // If we already failed, or simplification fails, we're done.
  if (failedConstraint || simplify()) {
    return true;
  } else {
    assert(ActiveConstraints.empty() && "Active constraints remain?");
  }

  // If there are no constraints remaining, we're done. Save this solution.
  if (InactiveConstraints.empty()) {
    // If this solution is worse than the best solution we've seen so far,
    // skipt it.
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

  // Compute the connected components of the constraint graph.
  // FIXME: We're seeding typeVars with TypeVariables so that the
  // connected-components algorithm only considers those type variables within
  // our component. There are clearly better ways to do this.
  SmallVector<TypeVariableType *, 16> typeVars(TypeVariables);
  SmallVector<unsigned, 16> components;
  unsigned numComponents = CG.computeConnectedComponents(typeVars, components);

  // If we don't have more than one component, just solve the whole
  // system.
  if (numComponents < 2) {
    return solveSimplified(solutions, allowFreeTypeVariables);
  }

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

  // Sort the constraints into buckets based on component number.
  std::unique_ptr<ConstraintList[]> constraintBuckets(
                                      new ConstraintList[numComponents]);
  while (!InactiveConstraints.empty()) {
    auto *constraint = &InactiveConstraints.front();
    InactiveConstraints.pop_front();
    constraintBuckets[constraintComponent[constraint]].push_back(constraint);
  }

  // Function object that returns all constraints placed into buckets
  // back to the list of constraints.
  auto returnAllConstraints = [&] {
    assert(InactiveConstraints.empty() && "Already have constraints?");
    for (unsigned component = 0; component != numComponents; ++component) {
      InactiveConstraints.splice(InactiveConstraints.end(), 
                                 constraintBuckets[component]);
    }
  };

  // Compute the partial solutions produced for each connected component.
  std::unique_ptr<SmallVector<Solution, 4>[]> 
    partialSolutions(new SmallVector<Solution, 4>[numComponents]);
  Optional<Score> PreviousBestScore = solverState->BestScore;
  for (unsigned component = 0; component != numComponents; ++component) {
    assert(InactiveConstraints.empty() && 
           "Some constraints were not transferred?");
    ++solverState->NumComponentsSplit;

    // Collect the constraints for this component.
    InactiveConstraints.splice(InactiveConstraints.end(), 
                               constraintBuckets[component]);

    // Collect the type variables that are not part of a different
    // component; this includes type variables that are part of the
    // component as well as already-resolved type variables.
    // FIXME: The latter could be avoided if we had already
    // substituted all of those other type variables through.
    llvm::SmallVector<TypeVariableType *, 16> allTypeVariables 
      = std::move(TypeVariables);
    for (auto typeVar : allTypeVariables) {
      auto known = typeVarComponent.find(typeVar);
      if (known != typeVarComponent.end() && known->second != component)
        continue;

      TypeVariables.push_back(typeVar);
    }
    
    // Solve for this component. If it fails, we're done.
    bool failed;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2) << "(solving component #" 
                                         << component << "\n";
    }
    {
      SolverScope scope(*this);
      failed = solveSimplified(partialSolutions[component], 
                               allowFreeTypeVariables);
    }

    // Put the constraints back into their original bucket.
    auto &bucket = constraintBuckets[component];
    bucket.splice(bucket.end(), InactiveConstraints);
    
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
      log.indent(solverState->depth * 2) << "finised component #" 
                                         << component << ")\n";
    }
    
    assert(!partialSolutions[component].empty() &&" No solutions?");

    // Move the type variables back, clear out constraints; we're
    // ready for the next component.
    TypeVariables = std::move(allTypeVariables);

    // For each of the partial solutions, substract off the current score.
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
    if (auto best = findBestSolution(solutions, /*minimize=*/true)) {
      if (*best > 0)
        solutions[0] = std::move(solutions[*best]);
      solutions.erase(solutions.begin() + 1, solutions.end());
    }
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
static bool shortCircuitDisjunctionAt(Constraint *constraint) {
  // Non-optional conversions are better than optional-to-optional
  // conversions.
  if (auto restriction = constraint->getRestriction()) {
    if (*restriction == ConversionRestrictionKind::OptionalToOptional)
      return true;
  }

  // Implicit conversions are better than checked casts.
  if (constraint->getKind() == ConstraintKind::CheckedCast)
    return true;

  // For a conjunction, check if any of the terms should be skipped.
  if (constraint->getKind() == ConstraintKind::Conjunction)
    for (auto nested : constraint->getNestedConstraints())
      if (shortCircuitDisjunctionAt(nested))
        return true;

  return false;
}

bool ConstraintSystem::solveSimplified(
       SmallVectorImpl<Solution> &solutions,
       FreeTypeVariableBinding allowFreeTypeVariables) {
  // Collect disjunctions.
  SmallVector<Constraint *, 4> disjunctions;
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() == ConstraintKind::Disjunction)
      disjunctions.push_back(&constraint);
  }

  // Look for potential type variable bindings.
  TypeVariableType *bestTypeVar = nullptr;
  PotentialBindings bestBindings;
  for (auto typeVar : TypeVariables) {
    // Skip any type variables that are bound.
    if (typeVar->getImpl().hasRepresentativeOrFixed())
      continue;

    // Get potential bindings.
    auto bindings = getPotentialBindings(*this, typeVar);
    if (!bindings)
      continue;

    // If these are the first bindings, or they are better than what
    // we saw before, use them instead.
    if (!bestTypeVar || bindings < bestBindings) {
      bestBindings = std::move(bindings);
      bestTypeVar = typeVar;
    }
  }

  // If we have a binding that does not involve type variables, or we have
  // no other option, go ahead and try the bindings for this type variable.
  if (bestBindings && 
      (disjunctions.empty() ||
       (!bestBindings.InvolvesTypeVariables && !bestBindings.FullyBound))) {
    return tryTypeVariableBindings(*this, solverState->depth, bestTypeVar,
                                   bestBindings.Bindings, solutions,
                                   allowFreeTypeVariables);
  }

  // If there are no disjunctions, we can't solve this system.
  if (disjunctions.empty()) {
    // If the only remaining constraints are conformance constraints
    // or member equality constraints, and we're allowed to have free
    // variables, we still have a solution.  FIXME: It seems like this
    // should be easier to detect. Aren't there other kinds of
    // constraints that could show up here?
    if (allowFreeTypeVariables != FreeTypeVariableBinding::Disallow &&
        hasFreeTypeVariables()) {
      bool anyNonConformanceConstraints = false;
      for (auto &constraint : InactiveConstraints) {
        if (constraint.getKind() == ConstraintKind::ConformsTo ||
            constraint.getKind() == ConstraintKind::SelfObjectOfProtocol)
          continue;

        if (constraint.getKind() == ConstraintKind::TypeMember)
          continue;

        anyNonConformanceConstraints = true;
        break;
      }

      // If this solution is worse than the best solution we've seen so far,
      // skipt it.
      if (worseThanBestSolution())
        return true;

      if (!anyNonConformanceConstraints) {
        auto solution = finalize(allowFreeTypeVariables);
        if (TC.getLangOpts().DebugConstraintSolver) {
          auto &log = getASTContext().TypeCheckerDebug->getStream();
          log.indent(solverState->depth * 2) << "(found solution)\n";
        }
        
        solutions.push_back(std::move(solution));
        return false;
      }
    }
    return true;
  }

  // Pick the smallest disjunction.
  // FIXME: This heuristic isn't great, but it helped somewhat for
  // overload sets.
  auto disjunction = disjunctions[0];
  auto bestSize = disjunction->getNestedConstraints().size();
  if (bestSize > 2) {
    for (auto contender : llvm::makeArrayRef(disjunctions).slice(1)) {
      unsigned newSize = contender->getNestedConstraints().size();
      if (newSize < bestSize) {
        bestSize = newSize;
        disjunction = contender;

        if (bestSize == 2)
          break;
      }
    }
  }

  // Remove this disjunction constraint from the list.
  auto afterDisjunction = InactiveConstraints.erase(disjunction);
  CG.removeConstraint(disjunction);

  // Try each of the constraints within the disjunction.
  bool anySolved = false;
  ++solverState->NumDisjunctions;
  for (auto constraint : disjunction->getNestedConstraints()) {
    // We already have a solution; check whether we should
    // short-circuit the disjunction.
    if (anySolved && shortCircuitDisjunctionAt(constraint))
      break;

    // Try to solve the system with this option in the disjunction.
    SolverScope scope(*this);
    ++solverState->NumDisjunctionTerms;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2)
        << "(assuming ";
      constraint->print(log, &TC.Context.SourceMgr);
      log << '\n';
    }

    // Simplify this term in the disjunction.
    switch (simplifyConstraint(*constraint)) {
    case SolutionKind::Error:
      if (!failedConstraint)
        failedConstraint = constraint;
      solverState->retiredConstraints.push_back(constraint);
      break;

    case SolutionKind::Solved:
      solverState->retiredConstraints.push_back(constraint);
      break;

    case SolutionKind::Unsolved:
      InactiveConstraints.push_back(constraint);
      CG.addConstraint(constraint);
      break;
    }

    // Record this as a generated constraint.
    solverState->generatedConstraints.push_back(constraint);

    if (!solve(solutions, allowFreeTypeVariables)) {
      anySolved = true;

      // If we see a tuple-to-tuple conversion that succeeded, we're done.
      // FIXME: This should be more general.
      if (auto restriction = constraint->getRestriction()) {
        if (*restriction == ConversionRestrictionKind::TupleToTuple)
          break;
      }

      // Or, if we see a conversion successfully applied to a string
      // interpolation argument, we're done.
      // FIXME: Probably should be more general, as mentioned above.
      if (auto locator = disjunction->getLocator()) {
        if (!locator->getPath().empty() &&
            locator->getPath().back().getKind()
              == ConstraintLocator::InterpolationArgument &&
            constraint->getKind() == ConstraintKind::Conversion)
          break;
      }
    }

    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2) << ")\n";
    }
  }

  // Put the disjunction constraint back in its place.
  InactiveConstraints.insert(afterDisjunction, disjunction);
  CG.addConstraint(disjunction);

  return !anySolved;
}
