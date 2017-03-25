//===--- CSSolver.cpp - Constraint Solver ---------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
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
#include "ConstraintSystem.h"
#include "ConstraintGraph.h"
#include "swift/AST/TypeWalker.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/SaveAndRestore.h"
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
static Optional<Type> checkTypeOfBinding(ConstraintSystem &cs, 
                                         TypeVariableType *typeVar, Type type,
                                         bool *isNilLiteral = nullptr) {
  if (!type)
    return None;

  // Simplify the type.
  type = cs.simplifyType(type);

  // If the type references the type variable, don't permit the binding.
  SmallVector<TypeVariableType *, 4> referencedTypeVars;
  type->getTypeVariables(referencedTypeVars);
  if (count(referencedTypeVars, typeVar))
    return None;

  // If the type is a type variable itself, don't permit the binding.
  if (auto bindingTypeVar = type->getRValueType()->getAs<TypeVariableType>()) {
    if (isNilLiteral) {
      *isNilLiteral = false;

      // Look for a literal-conformance constraint on the type variable.
      SmallVector<Constraint *, 8> constraints;
      cs.getConstraintGraph().gatherConstraints(
                              bindingTypeVar, constraints,
                              ConstraintGraph::GatheringKind::EquivalenceClass);
      for (auto constraint : constraints) {
        if (constraint->getKind() == ConstraintKind::LiteralConformsTo &&
            constraint->getProtocol()->isSpecificProtocol(
              KnownProtocolKind::ExpressibleByNilLiteral) &&
            cs.simplifyType(constraint->getFirstType())
              ->isEqual(bindingTypeVar)) {
          *isNilLiteral = true;
          break;
        }
      }
    }
    
    return None;
  }

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

  if (auto lvalue = type->getAs<LValueType>())
    result.push_back(lvalue->getObjectType());
  if (auto iot = type->getAs<InOutType>())
    result.push_back(iot->getObjectType());

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

ConstraintSystem::SolverState::SolverState(ConstraintSystem &cs) : CS(cs) {
  assert(!CS.solverState &&
         "Constraint system should not already have solver state!");
  CS.solverState = this;

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
  ++cs.solverState->depth;

  resolvedOverloadSets = cs.resolvedOverloadSets;
  numTypeVariables = cs.TypeVariables.size();
  numSavedBindings = cs.solverState->savedBindings.size();
  numConstraintRestrictions = cs.ConstraintRestrictions.size();
  numFixes = cs.Fixes.size();
  numDisjunctionChoices = cs.DisjunctionChoices.size();
  numOpenedTypes = cs.OpenedTypes.size();
  numOpenedExistentialTypes = cs.OpenedExistentialTypes.size();
  numDefaultedConstraints = cs.DefaultedConstraints.size();
  PreviousScore = cs.CurrentScore;

  cs.solverState->registerScope(this);
  assert(!cs.failedConstraint && "Unexpected failed constraint!");

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

  /// The kind of literal binding found.
  enum class LiteralBindingKind : unsigned char {
    None,
    Collection,
    Float,
    Atom,
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

    /// If this is a binding that comes from a \c Defaultable constraint,
    /// the locator of that constraint.
    ConstraintLocator *DefaultableBinding = nullptr;

    PotentialBinding(Type type, AllowedBindingKind kind,
                     Optional<ProtocolDecl *> defaultedProtocol = None,
                     ConstraintLocator *defaultableBinding = nullptr)
      : BindingType(type), Kind(kind), DefaultedProtocol(defaultedProtocol),
        DefaultableBinding(defaultableBinding) { }

    bool isDefaultableBinding() const { return DefaultableBinding != nullptr; }
  };

  struct PotentialBindings {
    /// The set of potential bindings.
    SmallVector<PotentialBinding, 4> Bindings;

    /// Whether this type variable is fully bound by one of its constraints.
    bool FullyBound = false;

    /// Whether the bindings of this type involve other type variables.
    bool InvolvesTypeVariables = false;

    /// Whether this type variable has literal bindings.
    LiteralBindingKind LiteralBinding = LiteralBindingKind::None;

    /// Whether this type variable is only bound above by existential types.
    bool SubtypeOfExistentialType = false;

    /// The number of defaultable bindings.
    unsigned NumDefaultableBindings = 0;

    /// Determine whether the set of bindings is non-empty.
    explicit operator bool() const {
      return !Bindings.empty();
    }

    /// Whether there are any non-defaultable bindings.
    bool hasNonDefaultableBindings() const {
      return Bindings.size() > NumDefaultableBindings;
    }

    /// Compare two sets of bindings, where \c x < y indicates that
    /// \c x is a better set of bindings that \c y.
    friend bool operator<(const PotentialBindings &x, 
                          const PotentialBindings &y) {
      return std::make_tuple(!x.hasNonDefaultableBindings(),
                             x.FullyBound,
                             x.SubtypeOfExistentialType,
                             static_cast<unsigned char>(x.LiteralBinding),
                             x.InvolvesTypeVariables,
                             -(x.Bindings.size() - x.NumDefaultableBindings))
        < std::make_tuple(!y.hasNonDefaultableBindings(),
                          y.FullyBound,
                          y.SubtypeOfExistentialType,
                          static_cast<unsigned char>(y.LiteralBinding),
                          y.InvolvesTypeVariables,
                          -(y.Bindings.size() - y.NumDefaultableBindings));
    }

    void foundLiteralBinding(ProtocolDecl *proto) {
      switch (*proto->getKnownProtocolKind()) {
      case KnownProtocolKind::ExpressibleByDictionaryLiteral:
      case KnownProtocolKind::ExpressibleByArrayLiteral:
      case KnownProtocolKind::ExpressibleByStringInterpolation:
        LiteralBinding = LiteralBindingKind::Collection;
        break;

      case KnownProtocolKind::ExpressibleByFloatLiteral:
        LiteralBinding = LiteralBindingKind::Float;
        break;

      default:
        if (LiteralBinding != LiteralBindingKind::Collection)
          LiteralBinding = LiteralBindingKind::Atom;
        break;
      }
    }

    void dump(TypeVariableType *typeVar, llvm::raw_ostream &out,
              unsigned indent) const {
      out.indent(indent);
      out << "(";
      if (typeVar)
        out << "$T" << typeVar->getImpl().getID();
      if (FullyBound)
        out << " fully_bound";
      if (SubtypeOfExistentialType)
        out << " subtype_of_existential";
      if (LiteralBinding != LiteralBindingKind::None)
        out << " literal=" << static_cast<int>(LiteralBinding);
      if (InvolvesTypeVariables)
        out << " involves_type_vars";
      if (NumDefaultableBindings > 0)
        out << " defaultable_bindings=" << NumDefaultableBindings;
      out << " bindings=";
      interleave(Bindings, [&](const PotentialBinding &binding) {
        auto type = binding.BindingType;
        auto &ctx = type->getASTContext();
        llvm::SaveAndRestore<bool>
          debugConstraints(ctx.LangOpts.DebugConstraintSolver, true);
        switch (binding.Kind) {
        case AllowedBindingKind::Exact:
          break;

        case AllowedBindingKind::Subtypes:
          out << "(subtypes of) ";
          break;

        case AllowedBindingKind::Supertypes:
          out << "(supertypes of) ";
          break;
        }
        if (binding.DefaultedProtocol)
          out << "(default from " << (*binding.DefaultedProtocol)->getName()
              << ") ";
        out << type.getString();
      }, [&]() { out << " "; });
      out << ")\n";
    }
  };
} // end anonymous namespace

/// \brief Return whether a relational constraint between a type variable and a
/// trivial wrapper type (autoclosure, unary tuple) should result in the type
/// variable being potentially bound to the value type, as opposed to the
/// wrapper type.
static bool shouldBindToValueType(Constraint *constraint)
{
  switch (constraint->getKind()) {
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::OperatorArgumentTupleConversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::ArgumentTupleConversion:
  case ConstraintKind::Conversion:
  case ConstraintKind::BridgingConversion:
  case ConstraintKind::Subtype:
    return true;
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::Layout:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::SelfObjectOfProtocol:
  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::BindOverload:
  case ConstraintKind::OptionalObject:
    return false;
  case ConstraintKind::DynamicTypeOf:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::Defaultable:
  case ConstraintKind::Disjunction:
    llvm_unreachable("shouldBindToValueType() may only be called on "
                     "relational constraints");
  }

  llvm_unreachable("Unhandled ConstraintKind in switch.");
}

/// Find the set of type variables that are inferable from the given type.
///
/// \param type The type to search.
/// \param typeVars Collects the type variables that are inferable from the
/// given type. This set is not cleared, so that multiple types can be explored
/// and introduce their results into the same set.
static void findInferableTypeVars(
              Type type,
              SmallPtrSetImpl<TypeVariableType *> &typeVars) {
  type = type->getCanonicalType();
  if (!type->hasTypeVariable()) return;

  class Walker : public TypeWalker {
    SmallPtrSetImpl<TypeVariableType *> &typeVars;
  public:
    explicit Walker(SmallPtrSetImpl<TypeVariableType *> &typeVars)
      : typeVars(typeVars) { }

    Action walkToTypePre(Type ty) override {
      if (ty->is<DependentMemberType>())
        return Action::SkipChildren;

      if (auto typeVar = ty->getAs<TypeVariableType>())
        typeVars.insert(typeVar);
      return Action::Continue;
    }
  };

  type.walk(Walker(typeVars));
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
  cs.getConstraintGraph().gatherConstraints(
                              typeVar, constraints,
                              ConstraintGraph::GatheringKind::EquivalenceClass);

  PotentialBindings result;
  Optional<unsigned> lastSupertypeIndex;

  // Local function to add a potential binding to the list of bindings,
  // coalescing supertype bounds when we are able to compute the meet.
  auto addPotentialBinding = [&](PotentialBinding binding,
                                 bool allowJoinMeet = true) {
    // If this is a non-defaulted supertype binding, check whether we can
    // combine it with another supertype binding by computing the 'join' of the
    // types.
    if (binding.Kind == AllowedBindingKind::Supertypes &&
        !binding.BindingType->hasTypeVariable() &&
        !binding.DefaultedProtocol &&
        !binding.isDefaultableBinding() &&
        allowJoinMeet) {
      if (lastSupertypeIndex) {
        // Can we compute a join?
        auto &lastBinding = result.Bindings[*lastSupertypeIndex];
        if (auto meet =
                Type::join(lastBinding.BindingType, binding.BindingType)) {
          // Replace the last supertype binding with the join. We're done.
          lastBinding.BindingType = meet;
          return;
        }
      }

      // Record this as the most recent supertype index.
      lastSupertypeIndex = result.Bindings.size();
    }

    result.Bindings.push_back(std::move(binding));
  };

  // Consider each of the constraints related to this type variable.
  llvm::SmallPtrSet<CanType, 4> exactTypes;
  llvm::SmallPtrSet<ProtocolDecl *, 4> literalProtocols;
  SmallVector<Constraint *, 2> defaultableConstraints;
  bool addOptionalSupertypeBindings = false;
  auto &tc = cs.getTypeChecker();
  bool hasNonDependentMemberRelationalConstraints = false;
  bool hasDependentMemberRelationalConstraints = false;
  for (auto constraint : constraints) {
    // Only visit each constraint once.
    if (!visitedConstraints.insert(constraint).second)
      continue;

    switch (constraint->getKind()) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::BindParam:
    case ConstraintKind::BindToPointerType:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::ArgumentTupleConversion:
    case ConstraintKind::OperatorArgumentTupleConversion:
    case ConstraintKind::OperatorArgumentConversion:
    case ConstraintKind::OptionalObject:
      // Relational constraints: break out to look for types above/below.
      break;

    case ConstraintKind::BridgingConversion:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::DynamicTypeOf:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::OpenedExistentialOf:
      // Constraints from which we can't do anything.
      continue;

    case ConstraintKind::Defaultable:
      // Do these in a separate pass.
      if (cs.getFixedTypeRecursive(constraint->getFirstType(), true)
            ->getAs<TypeVariableType>() == typeVar) {
        defaultableConstraints.push_back(constraint);
        hasNonDependentMemberRelationalConstraints = true;
      }
      continue;

    case ConstraintKind::Disjunction:
      // FIXME: Recurse into these constraints to see whether this
      // type variable is fully bound by any of them.
      result.InvolvesTypeVariables = true;
      continue;

    case ConstraintKind::ConformsTo:
    case ConstraintKind::SelfObjectOfProtocol:
      // Swift 3 allowed the use of default types for normal conformances
      // to expressible-by-literal protocols.
      if (tc.Context.LangOpts.EffectiveLanguageVersion[0] >= 4)
        continue;

      LLVM_FALLTHROUGH;
        
    case ConstraintKind::Layout:
    case ConstraintKind::LiteralConformsTo: {
      // If there is a 'nil' literal constraint, we might need optional
      // supertype bindings.
      if (constraint->getProtocol()->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByNilLiteral))
        addOptionalSupertypeBindings = true;

      // If there is a default literal type for this protocol, it's a
      // potential binding.
      auto defaultType = tc.getDefaultType(constraint->getProtocol(), cs.DC);
      if (!defaultType)
        continue;

      // Note that we have a literal constraint with this protocol.
      literalProtocols.insert(constraint->getProtocol());
      hasNonDependentMemberRelationalConstraints = true;

      // Handle unspecialized types directly.
      if (!defaultType->hasUnboundGenericType()) {
        if (!exactTypes.insert(defaultType->getCanonicalType()).second)
          continue;

        result.foundLiteralBinding(constraint->getProtocol());
        addPotentialBinding({defaultType, AllowedBindingKind::Subtypes,
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
        result.foundLiteralBinding(constraint->getProtocol());
        exactTypes.insert(defaultType->getCanonicalType());
        addPotentialBinding({defaultType, AllowedBindingKind::Subtypes,
                             constraint->getProtocol()});
      }

      continue;
    }

    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload: {
      if (result.FullyBound && result.InvolvesTypeVariables) continue;

      // If this variable is in the left-hand side, it is fully bound.
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      findInferableTypeVars(cs.simplifyType(constraint->getFirstType()),
                             typeVars);
      if (typeVars.count(typeVar))
        result.FullyBound = true;

      if (result.InvolvesTypeVariables) continue;

      // If this and another type variable occur, this result involves
      // type variables.
      findInferableTypeVars(cs.simplifyType(constraint->getSecondType()),
                             typeVars);
      if (typeVars.size() > 1 && typeVars.count(typeVar))
        result.InvolvesTypeVariables = true;
      continue;
    }

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
      // If our type variable shows up in the base type, there's
      // nothing to do.
      // FIXME: Can we avoid simplification here?
      if (ConstraintSystem::typeVarOccursInType(
            typeVar,
            cs.simplifyType(constraint->getFirstType()),
            &result.InvolvesTypeVariables)) {
        continue;
      }
      
      // If the type variable is in the list of member type
      // variables, it is fully bound.
      // FIXME: Can we avoid simplification here?
      if (ConstraintSystem::typeVarOccursInType(
            typeVar,
            cs.simplifyType(constraint->getSecondType()),
            &result.InvolvesTypeVariables)) {
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

    if (first->is<TypeVariableType>() && first->isEqual(second))
      continue;

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
      if (result.InvolvesTypeVariables) continue;

      // Check whether both this type and another type variable are
      // inferable.
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      findInferableTypeVars(first, typeVars);
      findInferableTypeVars(second, typeVars);
      if (typeVars.size() > 1 && typeVars.count(typeVar))
        result.InvolvesTypeVariables = true;
      continue;
    }

    // If the type we'd be binding to is a dependent member, don't try to
    // resolve this type variable yet.
    if (type->is<DependentMemberType>()) {
      if (!ConstraintSystem::typeVarOccursInType(
             typeVar, type, &result.InvolvesTypeVariables)) {
        hasDependentMemberRelationalConstraints = true;
      }
      continue;
    }
    hasNonDependentMemberRelationalConstraints = true;

    // Check whether we can perform this binding.
    // FIXME: this has a super-inefficient extraneous simplifyType() in it.
    bool isNilLiteral = false;
    bool *isNilLiteralPtr = nullptr;
    if (!addOptionalSupertypeBindings && kind == AllowedBindingKind::Supertypes)
      isNilLiteralPtr = &isNilLiteral;
    if (auto boundType = checkTypeOfBinding(cs, typeVar, type,
                                            isNilLiteralPtr)) {
      type = *boundType;
      if (type->hasTypeVariable())
        result.InvolvesTypeVariables = true;
    } else {
      // If the bound is a 'nil' literal type, add optional supertype bindings.
      if (isNilLiteral) {
        addOptionalSupertypeBindings = true;
        continue;
      }

      result.InvolvesTypeVariables = true;
      continue;
    }

    // Don't deduce autoclosure types or single-element, non-variadic
    // tuples.
    if (shouldBindToValueType(constraint)) {
      if (auto funcTy = type->getAs<FunctionType>()) {
        if (funcTy->isAutoClosure())
          type = funcTy->getResult();
      }

      type = type->getWithoutImmediateLabel();
    }

    // Don't deduce IUO types.
    Type alternateType;
    bool adjustedIUO = false;
    if (kind == AllowedBindingKind::Supertypes &&
        constraint->getKind() >= ConstraintKind::Conversion &&
        constraint->getKind() <= ConstraintKind::OperatorArgumentConversion) {
      auto innerType = type->getLValueOrInOutObjectType();
      if (auto objectType =
          cs.lookThroughImplicitlyUnwrappedOptionalType(innerType)) {
        type = OptionalType::get(objectType);
        alternateType = objectType;
        adjustedIUO = true;
      }
    }

    // Make sure we aren't trying to equate type variables with different
    // lvalue-binding rules.
    if (auto otherTypeVar = type->getAs<TypeVariableType>()) {
      if (typeVar->getImpl().canBindToLValue() !=
            otherTypeVar->getImpl().canBindToLValue())
        continue;
    }

    // BindParam constraints are not reflexive and must be treated specially.
    if (constraint->getKind() == ConstraintKind::BindParam) {
      if (kind == AllowedBindingKind::Subtypes) {
        if (auto *lvt = type->getAs<LValueType>()) {
          type = InOutType::get(lvt->getObjectType());
        }
      } else if (kind == AllowedBindingKind::Supertypes) {
        if (auto *iot = type->getAs<InOutType>()) {
          type = LValueType::get(iot->getObjectType());
        }
      }
      kind = AllowedBindingKind::Exact;
    }

    if (exactTypes.insert(type->getCanonicalType()).second)
      addPotentialBinding({type, kind, None}, /*allowJoinMeet=*/!adjustedIUO);
    if (alternateType &&
        exactTypes.insert(alternateType->getCanonicalType()).second)
      addPotentialBinding({alternateType, kind, None}, /*allowJoinMeet=*/false);
  }

  // If we have any literal constraints, check whether there is already a
  // binding that provides a type that conforms to that literal protocol. In
  // such cases, remove the default binding suggestion because the existing
  // suggestion is better.
  if (!literalProtocols.empty()) {
    SmallPtrSet<ProtocolDecl *, 5> coveredLiteralProtocols;
    for (auto &binding : result.Bindings) {
      // Skip defaulted-protocol constraints.
      if (binding.DefaultedProtocol)
        continue;

      Type testType;
      switch (binding.Kind) {
      case AllowedBindingKind::Exact:
        testType = binding.BindingType;
        break;

      case AllowedBindingKind::Subtypes:
      case AllowedBindingKind::Supertypes:
        testType = binding.BindingType->getRValueType();
        break;
      }

      // Check each non-covered literal protocol to determine which ones
      bool updatedBindingType = false;
      for (auto proto : literalProtocols) {
        do {
          // If the type conforms to this protocol, we're covered.
          if (tc.conformsToProtocol(testType, proto, cs.DC,
                                    ConformanceCheckFlags::InExpression)) {
            coveredLiteralProtocols.insert(proto);
            break;
          }

          // If we're allowed to bind to subtypes, look through optionals.
          // FIXME: This is really crappy special case of computing a reasonable
          // result based on the given constraints.
          if (binding.Kind == AllowedBindingKind::Subtypes) {
            if (auto objTy = testType->getAnyOptionalObjectType()) {
              updatedBindingType = true;
              testType = objTy;
              continue;
            }
          }

          updatedBindingType = false;
          break;
        } while (true);
      }

      if (updatedBindingType)
        binding.BindingType = testType;
    }

    // For any literal type that has been covered, remove the default literal
    // type.
    if (!coveredLiteralProtocols.empty()) {
      result.Bindings.erase(
        std::remove_if(result.Bindings.begin(),
                       result.Bindings.end(),
                       [&](PotentialBinding &binding) {
                         return binding.DefaultedProtocol &&
                         coveredLiteralProtocols.count(*binding.DefaultedProtocol) > 0;
                       }),
        result.Bindings.end());
    }
  }

  /// Add defaultable constraints last.
  for (auto constraint : defaultableConstraints) {
    Type type = constraint->getSecondType();
    if (!exactTypes.insert(type->getCanonicalType()).second)
      continue;

    ++result.NumDefaultableBindings;
    addPotentialBinding({type, AllowedBindingKind::Exact, None,
                         constraint->getLocator()});
  }

  // Determine if the bindings only constrain the type variable from above with
  // an existential type; such a binding is not very helpful because it's
  // impossible to enumerate the existential type's subtypes.
  result.SubtypeOfExistentialType =
    std::all_of(result.Bindings.begin(), result.Bindings.end(),
                [](const PotentialBinding &binding) {
                  return binding.BindingType->isExistentialType() &&
                         binding.Kind == AllowedBindingKind::Subtypes;
                });

  // If we're supposed to add optional supertype bindings, do so now.
  if (addOptionalSupertypeBindings) {
    for (unsigned i : indices(result.Bindings)) {
      // Only interested in supertype bindings.
      auto &binding = result.Bindings[i];
      if (binding.Kind != AllowedBindingKind::Supertypes) continue;

      // If the type doesn't conform to ExpressibleByNilLiteral,
      // produce an optional of that type as a potential binding. We
      // overwrite the binding in place because the non-optional type
      // will fail to type-check against the nil-literal conformance.
      auto nominalBindingDecl = binding.BindingType->getAnyNominal();
      bool conformsToExprByNilLiteral = false;
      if (nominalBindingDecl) {
        SmallVector<ProtocolConformance *, 2> conformances;
        conformsToExprByNilLiteral = nominalBindingDecl->lookupConformance(
                                       cs.DC->getParentModule(),
                                       cs.getASTContext().getProtocol(
                                         KnownProtocolKind::ExpressibleByNilLiteral),
                                       conformances);
      }

      if (!conformsToExprByNilLiteral) {
        binding.BindingType = OptionalType::get(binding.BindingType);
      }
    }
  }

  // If there were both dependent-member and non-dependent-member relational
  // constraints, consider this "fully bound"; we don't want to touch it.
  if (hasDependentMemberRelationalConstraints) {
    if (hasNonDependentMemberRelationalConstraints)
      result.FullyBound = true;
    else
      result.Bindings.clear();
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
  llvm::SmallPtrSet<TypeBase *, 4> boundTypes;

  SmallVector<PotentialBinding, 4> storedBindings;
  auto &tc = cs.getTypeChecker();
  ++cs.solverState->NumTypeVariablesBound;
  
  // If we've already explored a lot of potential solutions, bail.
  if (cs.getExpressionTooComplex(solutions))
    return true;

  for (unsigned tryCount = 0; !anySolved && !bindings.empty(); ++tryCount) {
    // Try each of the bindings in turn.
    ++cs.solverState->NumTypeVariableBindings;
    bool sawFirstLiteralConstraint = false;

    if (tc.getLangOpts().DebugConstraintSolver) {
      auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
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
        auto &log = cs.getASTContext().TypeCheckerDebug->getStream();
        log.indent(depth * 2)
          << "(trying " << typeVar->getString() << " := " << type->getString()
          << "\n";
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
        type = cs.openBindingType(type, typeVar->getImpl().getLocator());
      }

      // FIXME: We want the locator that indicates where the binding came
      // from.
      cs.addConstraint(ConstraintKind::Bind,
                       typeVar,
                       type,
                       typeVar->getImpl().getLocator());

      // If this was from a defaultable binding note that.
      if (binding.isDefaultableBinding()) {
        cs.DefaultedConstraints.push_back(binding.DefaultableBinding);
      }

      if (!cs.solveRec(solutions, allowFreeTypeVariables))
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
        KnownProtocolKind knownKind 
          = *((*binding.DefaultedProtocol)->getKnownProtocolKind());
        for (auto altType : cs.getAlternativeLiteralTypes(knownKind)) {
          if (exploredTypes.insert(altType->getCanonicalType()).second)
            newBindings.push_back({altType, AllowedBindingKind::Subtypes, 
                                   binding.DefaultedProtocol});
        }
      }

      // Handle simple subtype bindings.
      if (binding.Kind == AllowedBindingKind::Subtypes &&
          typeVar->getImpl().canBindToLValue() &&
          !type->isLValueType() &&
          !type->is<InOutType>()) {
        // Try lvalue qualification in addition to rvalue qualification.
        auto subtype = LValueType::get(type);
        if (exploredTypes.insert(subtype->getCanonicalType()).second)
          newBindings.push_back({subtype, binding.Kind, None});
      }

      if (binding.Kind == AllowedBindingKind::Subtypes) {
        if (auto tupleTy = type->getAs<TupleType>()) {
          int scalarIdx = tupleTy->getElementForScalarInit();
          if (scalarIdx >= 0) {
            auto eltType = tupleTy->getElementType(scalarIdx);
            if (exploredTypes.insert(eltType->getCanonicalType()).second)
              newBindings.push_back({eltType, binding.Kind, None});
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
                newBindings.push_back({objTy, binding.Kind, None});
              }
            } else {
              newBindings.push_back({objTy, binding.Kind, None});
            }
          }
        }
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
        if (exploredTypes.insert((*simpleSuper)->getCanonicalType()).second)
          newBindings.push_back({*simpleSuper, binding.Kind, None});
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
  if (solve(solutions, allowFreeTypeVariables) ||
      solutions.size() != 1)
    return Optional<Solution>();

  return std::move(solutions[0]);
}

bool ConstraintSystem::Candidate::solve() {
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

  // Cleanup after constraint system generation/solving,
  // because it would assign types to expressions, which
  // might interfere with solving higher-level expressions.
  ExprCleaner cleaner(E);

  // Generate constraints for the new system.
  if (auto generatedExpr = cs.generateConstraints(E)) {
    E = generatedExpr;
  } else {
    // Failure to generate constraint system for sub-expression
    // means we can't continue solving sub-expressions.
    cleanupImplicitExprs(E);
    return true;
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
    SolverState state(cs);

    // Use solveRec() instead of solve() in here, because solve()
    // would try to deduce the best solution, which we don't
    // really want. Instead, we want the reduced set of domain choices.
    cs.solveRec(solutions, FreeTypeVariableBinding::Allow);
  }

  // Record found solutions as suggestions.
  this->applySolutions(solutions);

  // Let's double-check if we have any implicit expressions
  // with type variables and nullify their types.
  cleanupImplicitExprs(E);

  // No solutions for the sub-expression means that either main expression
  // needs salvaging or it's inconsistent (read: doesn't have solutions).
  return solutions.empty();
}

void ConstraintSystem::Candidate::applySolutions(
                            llvm::SmallVectorImpl<Solution> &solutions) const {
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
    MutableArrayRef<ValueDecl *> decls
      = TC.Context.AllocateUninitialized<ValueDecl *>(choices.size());
    std::uninitialized_copy(choices.begin(), choices.end(), decls.begin());
    OSR->setDecls(decls);
  }
}

void ConstraintSystem::shrink(Expr *expr) {
  // Disable the shrink pass when constraint propagation is
  // enabled. They achieve similar effects, and the shrink pass is
  // known to have bad behavior in some cases.
  if (TC.Context.LangOpts.EnableConstraintPropagation)
    return;

  typedef llvm::SmallDenseMap<Expr *, ArrayRef<ValueDecl *>> DomainMap;

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
    llvm::SmallVector<std::pair<ApplyExpr *, unsigned>, 4> ApplyExprs;

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
        ApplyExprs.push_back({applyExpr, isa<OverloadSetRefExpr>(func)});
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
      if (expr == PrimaryExpr) {
        // If this is primary expression and there are no candidates
        // to be solved, let's not record it, because it's going to be
        // solved regardless.
        if (Candidates.empty())
          return expr;

        auto contextualType = CS.getContextualType();
        // If there is a contextual type set for this expression.
        if (!contextualType.isNull()) {
          Candidates.push_back(Candidate(CS, expr, contextualType,
                                         CS.getContextualTypePurpose()));
          return expr;
        }

        // Or it's a function application with other candidates present.
        if (isa<ApplyExpr>(expr)) {
          Candidates.push_back(Candidate(CS, expr));
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
    /// \returns ErrorType on failure, properly constructed type otherwise.
    Type extractElementType(Type collection) {
      auto &ctx = CS.getASTContext();
      if (collection.isNull() || collection->hasError())
        return ErrorType::get(ctx);

      auto base = collection.getPointer();
      auto isInvalidType = [](Type type) -> bool {
        return type.isNull() || type->hasUnresolvedType() ||
               type->hasError();
      };

      // Array type.
      if (auto array = dyn_cast<ArraySliceType>(base)) {
        auto elementType = array->getBaseType();
        // If base type is invalid let's return error type.
        return isInvalidType(elementType) ? ErrorType::get(ctx) : elementType;
      }

      // Map or Set or any other associated collection type.
      if (auto boundGeneric = dyn_cast<BoundGenericType>(base)) {
        if (boundGeneric->hasUnresolvedType())
          return ErrorType::get(ctx);

        llvm::SmallVector<TupleTypeElt, 2> params;
        for (auto &type : boundGeneric->getGenericArgs()) {
          // One of the generic arguments in invalid or unresolved.
          if (isInvalidType(type))
            return ErrorType::get(ctx);

          params.push_back(type);
        }

        // If there is just one parameter, let's return it directly.
        if (params.size() == 1)
          return params[0].getType();

        return TupleType::get(params, ctx);
      }

      return ErrorType::get(ctx);
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
        if (elementType->hasError())
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

  for (auto &candidate : collector.Candidates) {
    // If there are no results, let's forget everything we know about the
    // system so far. This actually is ok, because some of the expressions
    // might require manual salvaging.
    if (candidate.solve()) {
      // Let's restore all of the original OSR domains for this sub-expression,
      // this means that we can still make forward progress with solving of the
      // top sub-expressions.
      candidate.getExpr()->forEachChildExpr([&](Expr *childExpr) -> Expr * {
        if (auto OSR = dyn_cast<OverloadSetRefExpr>(childExpr)) {
          auto domain = domains.find(OSR);
          if (domain == domains.end())
            return childExpr;

          OSR->setDecls(domain->getSecond());
        }

        return childExpr;
      });
    }
  }
}

ConstraintSystem::SolutionKind
ConstraintSystem::solve(Expr *&expr,
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

    if (allowFreeTypeVariables == FreeTypeVariableBinding::UnresolvedType) {
      convertType = convertType.transform([&](Type type) -> Type {
        if (type->is<UnresolvedType>())
          return createTypeVariable(getConstraintLocator(expr), 0);
        return type;
      });
    }

    addConstraint(constraintKind, getType(expr), convertType,
                  getConstraintLocator(expr), /*isFavored*/ true);
  }

  // Notify the listener that we've built the constraint system.
  if (listener && listener->builtConstraints(*this, expr)) {
    return SolutionKind::Error;
  }

  if (TC.getLangOpts().DebugConstraintSolver) {
    auto &log = getASTContext().TypeCheckerDebug->getStream();
    log << "---Initial constraints for the given expression---\n";
    expr->print(log);
    log << "\n";
    print(log);
  }

  // Try to solve the constraint system using computed suggestions.
  solve(solutions, allowFreeTypeVariables);

  // If there are no solutions let's mark system as unsolved,
  // and solved otherwise even if there are multiple solutions still present.

  // There was a Swift 3 bug that allowed us to return Solved if we
  // had found at least one solution before deciding an expression was
  // "too complex". Maintain that behavior, but for Swift > 3 return
  // Unsolved in these cases.
  auto tooComplex = getExpressionTooComplex(solutions) &&
    !getASTContext().isSwiftVersion3();
  auto unsolved = tooComplex || solutions.empty();

  return unsolved ? SolutionKind::Unsolved : SolutionKind::Solved;
}

bool ConstraintSystem::solve(SmallVectorImpl<Solution> &solutions,
                             FreeTypeVariableBinding allowFreeTypeVariables) {
  // Set up solver state.
  SolverState state(*this);

  // Simplify any constraints left active after constraint generation
  // and optimization. Return if the resulting system has no
  // solutions.
  if (failedConstraint || simplify())
    return true;

  // If the experimental constraint propagation pass is enabled, run it.
  if (TC.Context.LangOpts.EnableConstraintPropagation)
    if (propagateConstraints())
      return true;

  // Solve the system.
  solveRec(solutions, allowFreeTypeVariables);

  // If there is more than one viable system, attempt to pick the best
  // solution.
  auto size = solutions.size();
  if (size > 1) {
    if (auto best = findBestSolution(solutions, /*minimize=*/false)) {
      if (*best != 0)
        solutions[0] = std::move(solutions[*best]);
      solutions.erase(solutions.begin() + 1, solutions.end());
    }
  }

  // We fail if there is no solution.
  return solutions.empty();
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

  // Add the orphaned components to the mapping from constraints to components.
  unsigned firstOrphanedConstraint =
    numComponents - CG.getOrphanedConstraints().size();
  {
    unsigned component = firstOrphanedConstraint;
    for (auto constraint : CG.getOrphanedConstraints())
      constraintComponent[constraint] = component++;
  }

  // Sort the constraints into buckets based on component number.
  std::unique_ptr<ConstraintList[]> constraintBuckets(
                                      new ConstraintList[numComponents]);
  while (!InactiveConstraints.empty()) {
    auto *constraint = &InactiveConstraints.front();
    InactiveConstraints.pop_front();
    constraintBuckets[constraintComponent[constraint]].push_back(constraint);
  }

  // Remove all of the orphaned constraints; we'll introduce them as needed.
  auto allOrphanedConstraints = CG.takeOrphanedConstraints();

  // Function object that returns all constraints placed into buckets
  // back to the list of constraints.
  auto returnAllConstraints = [&] {
    assert(InactiveConstraints.empty() && "Already have constraints?");
    for (unsigned component = 0; component != numComponents; ++component) {
      InactiveConstraints.splice(InactiveConstraints.end(), 
                                 constraintBuckets[component]);
    }
    CG.setOrphanedConstraints(std::move(allOrphanedConstraints));
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

    // Solve for this component. If it fails, we're done.
    bool failed;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2) << "(solving component #" 
                                         << component << "\n";
    }
    {
      // Introduce a scope for this partial solution.
      SolverScope scope(*this);
      llvm::SaveAndRestore<SolverScope *> 
        partialSolutionScope(solverState->PartialSolutionScope, &scope);

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
static bool shortCircuitDisjunctionAt(Constraint *constraint,
                                      Constraint *successfulConstraint) {
  
  // If the successfully applied constraint is favored, we'll consider that to
  // be the "best".
  if (successfulConstraint->isFavored() && !constraint->isFavored()) {
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

  // Binding an operator overloading to a generic operator is weaker than
  // binding to a non-generic operator, always.
  // Note: this is a hack to improve performance when we're dealing with
  // overloaded operators.
  if (constraint->getKind() == ConstraintKind::BindOverload &&
      constraint->getOverloadChoice().getKind() == OverloadChoiceKind::Decl &&
      constraint->getOverloadChoice().getDecl()->isOperator() &&
      successfulConstraint->getKind() == ConstraintKind::BindOverload &&
      successfulConstraint->getOverloadChoice().getKind()
        == OverloadChoiceKind::Decl &&
      successfulConstraint->getOverloadChoice().getDecl()->isOperator() &&
      constraint->getOverloadChoice().getDecl()->getInterfaceType()
        ->is<GenericFunctionType>() &&
      !successfulConstraint->getOverloadChoice().getDecl()->getInterfaceType()
         ->is<GenericFunctionType>()) {
    return true;
  }

  return false;
}

void ConstraintSystem::collectDisjunctions(
    SmallVectorImpl<Constraint *> &disjunctions) {
  for (auto &constraint : InactiveConstraints) {
    if (constraint.getKind() == ConstraintKind::Disjunction)
      disjunctions.push_back(&constraint);
  }
}

static std::pair<PotentialBindings, TypeVariableType *>
determineBestBindings(ConstraintSystem &CS) {
  // Look for potential type variable bindings.
  TypeVariableType *bestTypeVar = nullptr;
  PotentialBindings bestBindings;
  for (auto typeVar : CS.getTypeVariables()) {
    // Skip any type variables that are bound.
    if (typeVar->getImpl().hasRepresentativeOrFixed())
      continue;

    // Get potential bindings.
    auto bindings = getPotentialBindings(CS, typeVar);
    if (!bindings)
      continue;

    if (CS.TC.getLangOpts().DebugConstraintSolver) {
      auto &log = CS.getASTContext().TypeCheckerDebug->getStream();
      bindings.dump(typeVar, log, CS.solverState->depth * 2);
    }

    // If these are the first bindings, or they are better than what
    // we saw before, use them instead.
    if (!bestTypeVar || bindings < bestBindings) {
      bestBindings = std::move(bindings);
      bestTypeVar = typeVar;
    }
  }

  return std::make_pair(bestBindings, bestTypeVar);
}

bool ConstraintSystem::solveSimplified(
    SmallVectorImpl<Solution> &solutions,
    FreeTypeVariableBinding allowFreeTypeVariables) {

  SmallVector<Constraint *, 4> disjunctions;
  collectDisjunctions(disjunctions);

  TypeVariableType *bestTypeVar = nullptr;
  PotentialBindings bestBindings;
  std::tie(bestBindings, bestTypeVar) = determineBestBindings(*this);

  // If we have a binding that does not involve type variables, or we have
  // no other option, go ahead and try the bindings for this type variable.
  if (bestBindings && 
      (disjunctions.empty() ||
       (!bestBindings.InvolvesTypeVariables && !bestBindings.FullyBound &&
        bestBindings.LiteralBinding == LiteralBindingKind::None))) {
    return tryTypeVariableBindings(*this, solverState->depth, bestTypeVar,
                                   bestBindings.Bindings, solutions,
                                   allowFreeTypeVariables);
  }

  // If there are no disjunctions we can't solve this system unless we have
  // free type variables and are allowing them in the solution.
  if (disjunctions.empty()) {
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
  Constraint *firstSolvedConstraint = nullptr;
  ++solverState->NumDisjunctions;
  auto constraints = disjunction->getNestedConstraints();
  for (auto index : indices(constraints)) {
    auto constraint = constraints[index];

    // We already have a solution; check whether we should
    // short-circuit the disjunction.
    if (firstSolvedConstraint &&
        shortCircuitDisjunctionAt(constraint, firstSolvedConstraint))
      break;
    
    // If the expression was deemed "too complex", stop now and salvage.
    if (getExpressionTooComplex(solutions))
      break;

    // Try to solve the system with this option in the disjunction.
    SolverScope scope(*this);
    ++solverState->NumDisjunctionTerms;
    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth)
        << "(assuming ";
      constraint->print(log, &TC.Context.SourceMgr);
      log << '\n';
    }

    // If the disjunction requested us to, remember which choice we
    // took for it.
    if (disjunction->shouldRememberChoice()) {
      auto locator = disjunction->getLocator();
      assert(locator && "remembered disjunction doesn't have a locator?");
      DisjunctionChoices.push_back({locator, index});
    }

    // Simplify this term in the disjunction.
    switch (simplifyConstraint(*constraint)) {
    case SolutionKind::Error:
      if (!failedConstraint)
        failedConstraint = constraint;
      solverState->retireConstraint(constraint);
      break;

    case SolutionKind::Solved:
      solverState->retireConstraint(constraint);
      break;

    case SolutionKind::Unsolved:
      InactiveConstraints.push_back(constraint);
      CG.addConstraint(constraint);
      break;
    }

    // Record this as a generated constraint.
    solverState->addGeneratedConstraint(constraint);

    if (!solveRec(solutions, allowFreeTypeVariables)) {
      firstSolvedConstraint = constraint;

      // If we see a tuple-to-tuple conversion that succeeded, we're done.
      // FIXME: This should be more general.
      if (auto restriction = constraint->getRestriction()) {
        if (*restriction == ConversionRestrictionKind::TupleToTuple)
          break;
      }
    }

    if (TC.getLangOpts().DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth) << ")\n";
    }
  }

  // Put the disjunction constraint back in its place.
  InactiveConstraints.insert(afterDisjunction, disjunction);
  CG.addConstraint(disjunction);

  // If we are exiting due to an expression that is too complex, do
  // not allow our caller to continue as if we have been successful.
  // Maintain the broken behavior under Swift 3 mode though, to avoid
  // breaking code.
  auto tooComplex = getExpressionTooComplex(solutions) &&
    !getASTContext().isSwiftVersion3();

  return tooComplex || !firstSolvedConstraint;
}
