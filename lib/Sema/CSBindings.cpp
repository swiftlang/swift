//===--- CSBindings.cpp - Constraint Solver -------------------------------===//
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
// This file implements selection of bindings for type variables.
//
//===----------------------------------------------------------------------===//
#include "ConstraintGraph.h"
#include "ConstraintSystem.h"
#include "llvm/ADT/SetVector.h"
#include <tuple>

using namespace swift;
using namespace constraints;

void ConstraintSystem::inferTransitiveSupertypeBindings(
    const llvm::SmallDenseMap<TypeVariableType *, PotentialBindings>
        &inferredBindings,
    PotentialBindings &bindings) {
  auto *typeVar = bindings.TypeVar;

  llvm::SmallVector<Constraint *, 4> subtypeOf;
  // First, let's collect all of the `subtype` constraints associated
  // with this type variable.
  llvm::copy_if(bindings.Sources, std::back_inserter(subtypeOf),
                [&](const Constraint *constraint) -> bool {
                  if (constraint->getKind() != ConstraintKind::Subtype)
                    return false;

                  auto rhs = simplifyType(constraint->getSecondType());
                  return rhs->getAs<TypeVariableType>() == typeVar;
                });

  if (subtypeOf.empty())
    return;

  // We need to make sure that there are no duplicate bindings in the
  // set, other we'll produce multiple identical solutions.
  llvm::SmallPtrSet<CanType, 4> existingTypes;
  for (const auto &binding : bindings.Bindings)
    existingTypes.insert(binding.BindingType->getCanonicalType());

  for (auto *constraint : subtypeOf) {
    auto *tv =
        simplifyType(constraint->getFirstType())->getAs<TypeVariableType>();
    if (!tv)
      continue;

    auto relatedBindings = inferredBindings.find(tv);
    if (relatedBindings == inferredBindings.end())
      continue;

    for (auto &binding : relatedBindings->getSecond().Bindings) {
      // We need the binding kind for the potential binding to
      // either be Exact or Supertypes in order for it to make sense
      // to add Supertype bindings based on the relationship between
      // our type variables.
      if (binding.Kind != AllowedBindingKind::Exact &&
          binding.Kind != AllowedBindingKind::Supertypes)
        continue;

      auto type = binding.BindingType;

      if (type->isHole())
        continue;

      if (!existingTypes.insert(type->getCanonicalType()).second)
        continue;

      if (ConstraintSystem::typeVarOccursInType(typeVar, type))
        continue;

      bindings.addPotentialBinding(
          binding.withSameSource(type, AllowedBindingKind::Supertypes));
    }
  }
}

Optional<ConstraintSystem::PotentialBindings>
ConstraintSystem::determineBestBindings() {
  // Look for potential type variable bindings.
  Optional<PotentialBindings> bestBindings;
  llvm::SmallDenseMap<TypeVariableType *, PotentialBindings> cache;

  // First, let's collect all of the possible bindings.
  for (auto *typeVar : getTypeVariables()) {
    if (typeVar->getImpl().hasRepresentativeOrFixed())
      continue;

    if (auto bindings = getPotentialBindings(typeVar))
      cache.insert({typeVar, std::move(bindings)});
  }

  // Now let's see if we could infer something for related type
  // variables based on other bindings.
  for (auto *typeVar : getTypeVariables()) {
    auto cachedBindings = cache.find(typeVar);
    if (cachedBindings == cache.end())
      continue;

    auto &bindings = cachedBindings->getSecond();

    inferTransitiveSupertypeBindings(cache, bindings);

    if (getASTContext().TypeCheckerOpts.DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      bindings.dump(typeVar, log, solverState->depth * 2);
    }

    // If these are the first bindings, or they are better than what
    // we saw before, use them instead.
    if (!bestBindings || bindings < *bestBindings)
      bestBindings = bindings;
  }

  return bestBindings;
}

/// Find the set of type variables that are inferable from the given type.
///
/// \param type The type to search.
/// \param typeVars Collects the type variables that are inferable from the
/// given type. This set is not cleared, so that multiple types can be explored
/// and introduce their results into the same set.
static void
findInferableTypeVars(Type type,
                      SmallPtrSetImpl<TypeVariableType *> &typeVars) {
  type = type->getCanonicalType();
  if (!type->hasTypeVariable())
    return;

  class Walker : public TypeWalker {
    SmallPtrSetImpl<TypeVariableType *> &typeVars;

  public:
    explicit Walker(SmallPtrSetImpl<TypeVariableType *> &typeVars)
        : typeVars(typeVars) {}

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

void ConstraintSystem::PotentialBindings::addPotentialBinding(
    PotentialBinding binding, bool allowJoinMeet) {
  assert(!binding.BindingType->is<ErrorType>());

  // If this is a non-defaulted supertype binding,
  // check whether we can combine it with another
  // supertype binding by computing the 'join' of the types.
  if (binding.Kind == AllowedBindingKind::Supertypes &&
      !binding.BindingType->hasUnresolvedType() &&
      !binding.BindingType->hasTypeVariable() &&
      !binding.BindingType->hasUnboundGenericType() &&
      !binding.hasDefaultedLiteralProtocol() &&
      !binding.isDefaultableBinding() && allowJoinMeet) {
    if (lastSupertypeIndex) {
      auto &lastBinding = Bindings[*lastSupertypeIndex];
      auto lastType = lastBinding.BindingType->getWithoutSpecifierType();
      auto bindingType = binding.BindingType->getWithoutSpecifierType();

      auto join = Type::join(lastType, bindingType);
      if (join && !(*join)->isAny() &&
          (!(*join)->getOptionalObjectType()
           || !(*join)->getOptionalObjectType()->isAny())) {
        // Replace the last supertype binding with the join. We're done.
        lastBinding.BindingType = *join;
        return;
      }
    }

    // Record this as the most recent supertype index.
    lastSupertypeIndex = Bindings.size();
  }

  if (auto *literalProtocol = binding.getDefaultedLiteralProtocol())
    foundLiteralBinding(literalProtocol);

  // If the type variable can't bind to an lvalue, make sure the
  // type we pick isn't an lvalue.
  if (!TypeVar->getImpl().canBindToLValue() &&
      binding.BindingType->hasLValueType()) {
    binding = binding.withType(binding.BindingType->getRValueType());
  }

  if (!isViable(binding))
    return;

  if (binding.isDefaultableBinding())
    ++NumDefaultableBindings;

  Bindings.push_back(std::move(binding));
}

bool ConstraintSystem::PotentialBindings::isViable(
    PotentialBinding &binding) const {
  // Prevent against checking against the same opened nominal type
  // over and over again. Doing so means redundant work in the best
  // case. In the worst case, we'll produce lots of duplicate solutions
  // for this constraint system, which is problematic for overload
  // resolution.
  auto type = binding.BindingType;
  if (type->hasTypeVariable()) {
    auto *NTD = type->getAnyNominal();
    if (!NTD)
      return true;

    for (auto &existing : Bindings) {
      auto *existingNTD = existing.BindingType->getAnyNominal();
      if (existingNTD && NTD == existingNTD)
        return false;
    }
  }

  return true;
}

bool ConstraintSystem::PotentialBindings::favoredOverDisjunction(
    Constraint *disjunction) const {
  if (IsHole || FullyBound)
    return false;

  // If this bindings are for a closure and there are no holes,
  // it shouldn't matter whether it there are any type variables
  // or not because e.g. parameter type can have type variables,
  // but we still want to resolve closure body early (instead of
  // attempting any disjunction) to gain additional contextual
  // information.
  if (TypeVar->getImpl().isClosureType()) {
    auto boundType = disjunction->getNestedConstraints()[0]->getFirstType();
    // If disjunction is attempting to bind a type variable, let's
    // favor closure because it would add additional context, otherwise
    // if it's something like a collection (where it has to pick
    // between a conversion and bridging conversion) or concrete
    // type let's prefer the disjunction.
    //
    // We are looking through optionals here because it could be
    // a situation where disjunction is formed to match optionals
    // either as deep equality or optional-to-optional conversion.
    // Such type variables might be connected to closure as well
    // e.g. when result type is optional, so it makes sense to
    // open closure before attempting such disjunction.
    return boundType->lookThroughAllOptionalTypes()->is<TypeVariableType>();
  }

  return !InvolvesTypeVariables;
}

static bool hasNilLiteralConstraint(TypeVariableType *typeVar,
                                    const ConstraintSystem &CS) {
  // Look for a literal-conformance constraint on the type variable.
  auto constraints =
      CS.getConstraintGraph().gatherConstraints(
          typeVar, ConstraintGraph::GatheringKind::EquivalenceClass,
          [](Constraint *constraint) -> bool {
            return constraint->getKind() == ConstraintKind::LiteralConformsTo &&
                   constraint->getProtocol()->isSpecificProtocol(
                       KnownProtocolKind::ExpressibleByNilLiteral);
          });

  for (auto constraint : constraints)
    if (CS.simplifyType(constraint->getFirstType())->isEqual(typeVar))
      return true;

  return false;
}

Optional<ConstraintSystem::PotentialBinding>
ConstraintSystem::getPotentialBindingForRelationalConstraint(
    PotentialBindings &result, Constraint *constraint,
    bool &hasDependentMemberRelationalConstraints,
    bool &hasNonDependentMemberRelationalConstraints,
    bool &addOptionalSupertypeBindings) const {
  assert(constraint->getClassification() ==
             ConstraintClassification::Relational &&
         "only relational constraints handled here");

  auto *typeVar = result.TypeVar;

  // Record constraint which contributes to the
  // finding of potential bindings.
  result.Sources.insert(constraint);

  auto first = simplifyType(constraint->getFirstType());
  auto second = simplifyType(constraint->getSecondType());

  if (first->is<TypeVariableType>() && first->isEqual(second))
    return None;

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
    // If the left-hand side of a relational constraint is a
    // type variable representing a closure type, let's delay
    // attempting any bindings related to any type variables
    // on the other side since it could only be either a closure
    // parameter or a result type, and we can't get a full set
    // of bindings for them until closure's body is opened.
    if (auto *typeVar = first->getAs<TypeVariableType>()) {
      if (typeVar->getImpl().isClosureType()) {
        result.InvolvesTypeVariables = true;
        result.FullyBound = true;
        return None;
      }
    }

    // Can't infer anything.
    if (result.InvolvesTypeVariables)
      return None;

    // Check whether both this type and another type variable are
    // inferable.
    SmallPtrSet<TypeVariableType *, 4> typeVars;
    findInferableTypeVars(first, typeVars);
    findInferableTypeVars(second, typeVars);
    if (typeVars.size() > 1 && typeVars.count(typeVar))
      result.InvolvesTypeVariables = true;
    return None;
  }

  // Do not attempt to bind to ErrorType.
  if (type->hasError())
    return None;

  if (auto *locator = typeVar->getImpl().getLocator()) {
    if (locator->isKeyPathType()) {
      auto *BGT =
          type->lookThroughAllOptionalTypes()->getAs<BoundGenericType>();
      if (!BGT || !isKnownKeyPathDecl(getASTContext(), BGT->getDecl()))
        return None;
    }
  }

  // If the source of the binding is 'OptionalObject' constraint
  // and type variable is on the left-hand side, that means
  // that it _has_ to be of optional type, since the right-hand
  // side of the constraint is object type of the optional.
  if (constraint->getKind() == ConstraintKind::OptionalObject &&
      kind == AllowedBindingKind::Subtypes) {
    type = OptionalType::get(type);
  }

  // If the type we'd be binding to is a dependent member, don't try to
  // resolve this type variable yet.
  if (type->is<DependentMemberType>()) {
    if (!ConstraintSystem::typeVarOccursInType(typeVar, type,
                                               &result.InvolvesTypeVariables)) {
      hasDependentMemberRelationalConstraints = true;
    }
    return None;
  }
  hasNonDependentMemberRelationalConstraints = true;

  // If our binding choice is a function type and we're attempting
  // to bind to a type variable that is the result of opening a
  // generic parameter, strip the noescape bit so that we only allow
  // bindings of escaping functions in this position. We do this
  // because within the generic function we have no indication of
  // whether the parameter is a function type and if so whether it
  // should be allowed to escape. As a result we allow anything
  // passed in to escape.
  if (auto *fnTy = type->getAs<AnyFunctionType>())
    if (typeVar->getImpl().getGenericParameter() && !shouldAttemptFixes())
      type = fnTy->withExtInfo(fnTy->getExtInfo().withNoEscape(false));

  // Check whether we can perform this binding.
  // FIXME: this has a super-inefficient extraneous simplifyType() in it.
  if (auto boundType = checkTypeOfBinding(typeVar, type)) {
    type = *boundType;
    if (type->hasTypeVariable())
      result.InvolvesTypeVariables = true;
  } else {
    auto *bindingTypeVar = type->getRValueType()->getAs<TypeVariableType>();

    if (!bindingTypeVar)
      return None;

    result.InvolvesTypeVariables = true;

    if (constraint->getKind() == ConstraintKind::Subtype &&
        kind == AllowedBindingKind::Subtypes) {
      result.SubtypeOf.insert(bindingTypeVar);
    }

    // If we've already set addOptionalSupertypeBindings, or we aren't
    // allowing supertype bindings, we're done.
    if (addOptionalSupertypeBindings || kind != AllowedBindingKind::Supertypes)
      return None;

    // If the bound is a 'nil' literal type, add optional supertype bindings.
    if (hasNilLiteralConstraint(bindingTypeVar, *this))
      addOptionalSupertypeBindings = true;

    return None;
  }

  // Make sure we aren't trying to equate type variables with different
  // lvalue-binding rules.
  if (auto otherTypeVar =
          type->lookThroughAllOptionalTypes()->getAs<TypeVariableType>()) {
    if (typeVar->getImpl().canBindToLValue() !=
        otherTypeVar->getImpl().canBindToLValue())
      return None;
  }

  if (type->is<InOutType>() && !typeVar->getImpl().canBindToInOut())
    type = LValueType::get(type->getInOutObjectType());
  if (type->is<LValueType>() && !typeVar->getImpl().canBindToLValue())
    type = type->getRValueType();

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

  return PotentialBinding{type, kind, constraint};
}

/// Retrieve the set of potential type bindings for the given
/// representative type variable, along with flags indicating whether
/// those types should be opened.
ConstraintSystem::PotentialBindings
ConstraintSystem::getPotentialBindings(TypeVariableType *typeVar) const {
  assert(typeVar->getImpl().getRepresentative(nullptr) == typeVar &&
         "not a representative");
  assert(!typeVar->getImpl().getFixedType(nullptr) && "has a fixed type");

  // Determines whether this type variable represents an object
  // of the optional type extracted by force unwrap.
  bool isOptionalObject = false;
  if (auto *locator = typeVar->getImpl().getLocator()) {
    auto *anchor = locator->getAnchor();
    isOptionalObject = anchor && isa<ForceValueExpr>(anchor);
  }

  // Gather the constraints associated with this type variable.
  auto constraints =
      getConstraintGraph().gatherConstraints(
          typeVar, ConstraintGraph::GatheringKind::EquivalenceClass);

  PotentialBindings result(typeVar);

  // Consider each of the constraints related to this type variable.
  llvm::SmallPtrSet<CanType, 4> exactTypes;
  SmallVector<Constraint *, 2> defaultableConstraints;
  SmallVector<PotentialBinding, 4> literalBindings;
  bool addOptionalSupertypeBindings = false;
  bool hasNonDependentMemberRelationalConstraints = false;
  bool hasDependentMemberRelationalConstraints = false;
  for (auto constraint : constraints) {
    switch (constraint->getKind()) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::BindParam:
    case ConstraintKind::BindToPointerType:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentConversion:
    case ConstraintKind::OptionalObject: {
      // If there is a `bind param` constraint associated with
      // current type variable, result should be aware of that
      // fact. Binding set might be incomplete until
      // this constraint is resolved, because we currently don't
      // look-through constraints expect to `subtype` to try and
      // find related bindings.
      // This only affects type variable that appears one the
      // right-hand side of the `bind param` constraint and
      // represents result type of the closure body, because
      // left-hand side gets types from overload choices.
      if (constraint->getKind() == ConstraintKind::BindParam &&
          constraint->getSecondType()->isEqual(typeVar))
        result.PotentiallyIncomplete = true;

      auto binding = getPotentialBindingForRelationalConstraint(
          result, constraint, hasDependentMemberRelationalConstraints,
          hasNonDependentMemberRelationalConstraints,
          addOptionalSupertypeBindings);
      if (!binding)
        break;

      auto type = binding->BindingType;
      if (exactTypes.insert(type->getCanonicalType()).second) {
        result.addPotentialBinding(*binding);

        // Result of force unwrap is always connected to its base
        // optional type via `OptionalObject` constraint which
        // preserves l-valueness, so in case where object type got
        // inferred before optional type (because it got the
        // type from context e.g. parameter type of a function call),
        // we need to test type with and without l-value after
        // delaying bindings for as long as possible.
        if (isOptionalObject && !type->is<LValueType>()) {
          result.addPotentialBinding(binding->withType(LValueType::get(type)));
          result.FullyBound = true;
        }

        if (auto *locator = typeVar->getImpl().getLocator()) {
          auto path = locator->getPath();
          auto voidType = getASTContext().TheEmptyTupleType;

          // If this is a type variable representing closure result,
          // which is on the right-side of some relational constraint
          // let's have it try `Void` as well because there is an
          // implicit conversion `() -> T` to `() -> Void` and this
          // helps to avoid creating a thunk to support it.
          if (!path.empty() &&
              path.back().getKind() == ConstraintLocator::ClosureResult &&
              binding->Kind == AllowedBindingKind::Supertypes &&
              exactTypes.insert(voidType).second) {
            result.addPotentialBinding({voidType, binding->Kind, constraint},
                                       /*allowJoinMeet=*/false);
          }
        }
      }
      break;
    }

    case ConstraintKind::BridgingConversion:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::OpenedExistentialOf:
    case ConstraintKind::KeyPath:
    case ConstraintKind::KeyPathApplication:
    case ConstraintKind::FunctionInput:
    case ConstraintKind::FunctionResult:
    case ConstraintKind::OpaqueUnderlyingType:
      // Constraints from which we can't do anything.
      break;

    case ConstraintKind::DynamicTypeOf: {
      // Direct binding of the left-hand side could result
      // in `DynamicTypeOf` failure if right-hand side is
      // bound (because 'Bind' requires equal types to
      // succeed), or left is bound to Any which is not an
      // [existential] metatype.
      auto dynamicType = constraint->getFirstType();
      if (auto *tv = dynamicType->getAs<TypeVariableType>()) {
        if (tv->getImpl().getRepresentative(nullptr) == typeVar)
          return {typeVar};
      }

      // This is right-hand side, let's continue.
      break;
    }

    case ConstraintKind::Defaultable:
    case ConstraintKind::DefaultClosureType:
      // Do these in a separate pass.
      if (getFixedTypeRecursive(constraint->getFirstType(), true)
              ->getAs<TypeVariableType>() == typeVar) {
        defaultableConstraints.push_back(constraint);
        hasNonDependentMemberRelationalConstraints = true;
      }
      break;

    case ConstraintKind::Disjunction:
      // FIXME: Recurse into these constraints to see whether this
      // type variable is fully bound by any of them.
      result.InvolvesTypeVariables = true;

      // If there is additional context available via disjunction
      // associated with closure literal (e.g. coercion to some other
      // type) let's delay resolving the closure until the disjunction
      // is attempted.
      if (typeVar->getImpl().isClosureType())
        return {typeVar};

      break;

    case ConstraintKind::ConformsTo:
    case ConstraintKind::SelfObjectOfProtocol:
      // Swift 3 allowed the use of default types for normal conformances
      // to expressible-by-literal protocols.
      if (getASTContext().LangOpts.EffectiveLanguageVersion[0] >= 4)
        continue;

      if (!constraint->getSecondType()->is<ProtocolType>())
        continue;

      LLVM_FALLTHROUGH;

    case ConstraintKind::LiteralConformsTo: {
      // If there is a 'nil' literal constraint, we might need optional
      // supertype bindings.
      if (constraint->getProtocol()->isSpecificProtocol(
              KnownProtocolKind::ExpressibleByNilLiteral)) {
        addOptionalSupertypeBindings = true;
      }

      // If there is a default literal type for this protocol, it's a
      // potential binding.
      auto defaultType = TypeChecker::getDefaultType(constraint->getProtocol(), DC);
      if (!defaultType)
        continue;

      hasNonDependentMemberRelationalConstraints = true;

      // Handle unspecialized types directly.
      if (!defaultType->hasUnboundGenericType()) {
        if (!exactTypes.insert(defaultType->getCanonicalType()).second)
          continue;

        literalBindings.push_back(
            {defaultType, AllowedBindingKind::Subtypes, constraint});
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
        exactTypes.insert(defaultType->getCanonicalType());
        literalBindings.push_back(
            {defaultType, AllowedBindingKind::Subtypes, constraint});
      }

      break;
    }

    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::DynamicCallableApplicableFunction:
    case ConstraintKind::BindOverload: {
      if (result.FullyBound && result.InvolvesTypeVariables)
        continue;

      // If this variable is in the left-hand side, it is fully bound.
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      findInferableTypeVars(simplifyType(constraint->getFirstType()), typeVars);
      if (typeVars.count(typeVar))
        result.FullyBound = true;

      if (result.InvolvesTypeVariables)
        continue;

      // If this and another type variable occur, this result involves
      // type variables.
      findInferableTypeVars(simplifyType(constraint->getSecondType()),
                            typeVars);
      if (typeVars.size() > 1 && typeVars.count(typeVar))
        result.InvolvesTypeVariables = true;

      break;
    }

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
    case ConstraintKind::ValueWitness:
      // If our type variable shows up in the base type, there's
      // nothing to do.
      // FIXME: Can we avoid simplification here?
      if (ConstraintSystem::typeVarOccursInType(
              typeVar, simplifyType(constraint->getFirstType()),
              &result.InvolvesTypeVariables)) {
        continue;
      }

      // If the type variable is in the list of member type
      // variables, it is fully bound.
      // FIXME: Can we avoid simplification here?
      if (ConstraintSystem::typeVarOccursInType(
              typeVar, simplifyType(constraint->getSecondType()),
              &result.InvolvesTypeVariables)) {
        result.FullyBound = true;
      }
      break;

    case ConstraintKind::OneWayEqual:
    case ConstraintKind::OneWayBindParam: {
      // Don't produce any bindings if this type variable is on the left-hand
      // side of a one-way binding.
      auto firstType = constraint->getFirstType();
      if (auto *tv = firstType->getAs<TypeVariableType>()) {
        if (tv->getImpl().getRepresentative(nullptr) == typeVar)
          return {typeVar};
      }

      break;
    }
    }
  }

  // If we have any literal constraints, check whether there is already a
  // binding that provides a type that conforms to that literal protocol. In
  // such cases, remove the default binding suggestion because the existing
  // suggestion is better.
  if (!literalBindings.empty()) {
    SmallPtrSet<ProtocolDecl *, 5> coveredLiteralProtocols;
    for (auto &binding : result.Bindings) {
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

      // Attempting to check conformance of the type variable,
      // or unresolved type is invalid since it would result
      // in lose of viable literal bindings because that check
      // always returns trivial conformance.
      if (testType->isTypeVariableOrMember() || testType->is<UnresolvedType>())
        continue;

      // Check each non-covered literal protocol to determine which ones
      // might be covered by non-defaulted bindings.
      bool updatedBindingType = false;
      for (auto &literalBinding : literalBindings) {
        auto *protocol = literalBinding.getDefaultedLiteralProtocol();

        assert(protocol);

        // Has already been covered by one of the bindings.
        if (coveredLiteralProtocols.count(protocol))
          continue;

        do {
          // If the type conforms to this protocol, we're covered.
          if (TypeChecker::conformsToProtocol(
                  testType, protocol, DC,
                  (ConformanceCheckFlags::InExpression |
                   ConformanceCheckFlags::SkipConditionalRequirements))) {
            coveredLiteralProtocols.insert(protocol);
            break;
          }

          // If we're allowed to bind to subtypes, look through optionals.
          // FIXME: This is really crappy special case of computing a reasonable
          // result based on the given constraints.
          if (binding.Kind == AllowedBindingKind::Subtypes) {
            if (auto objTy = testType->getOptionalObjectType()) {
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

    for (auto &literalBinding : literalBindings) {
      auto *protocol = literalBinding.getDefaultedLiteralProtocol();
      // For any literal type that has been covered, skip them.
      if (coveredLiteralProtocols.count(protocol) == 0)
        result.addPotentialBinding(std::move(literalBinding));
    }
  }

  /// Add defaultable constraints last.
  for (auto constraint : defaultableConstraints) {
    Type type = constraint->getSecondType();
    if (!exactTypes.insert(type->getCanonicalType()).second)
      continue;

    if (constraint->getKind() == ConstraintKind::DefaultClosureType) {
      // If there are no other possible bindings for this closure
      // let's default it to the type inferred from its parameters/body,
      // otherwise we should only attempt contextual types as a
      // top-level closure type.
      if (!result.Bindings.empty())
        continue;
    }

    result.addPotentialBinding({type, AllowedBindingKind::Exact, constraint});
  }

  // If there are no bindings, typeVar may be a hole.
  if (shouldAttemptFixes() && result.Bindings.empty() &&
      typeVar->getImpl().canBindToHole()) {
    result.IsHole = true;
    // If the base of the unresolved member reference like `.foo`
    // couldn't be resolved we'd want to bind it to a hole at the
    // very last moment possible, just like generic parameters.
    auto *locator = typeVar->getImpl().getLocator();
    if (locator->isLastElement<LocatorPathElt::MemberRefBase>())
      result.PotentiallyIncomplete = true;

    result.addPotentialBinding(
        PotentialBinding::forHole(getASTContext(), locator));
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
      auto &binding = result.Bindings[i];
      bool wrapInOptional = false;

      if (binding.Kind == AllowedBindingKind::Supertypes) {
        // If the type doesn't conform to ExpressibleByNilLiteral,
        // produce an optional of that type as a potential binding. We
        // overwrite the binding in place because the non-optional type
        // will fail to type-check against the nil-literal conformance.
        auto nominalBindingDecl =
            binding.BindingType->getRValueType()->getAnyNominal();
        bool conformsToExprByNilLiteral = false;
        if (nominalBindingDecl) {
          SmallVector<ProtocolConformance *, 2> conformances;
          conformsToExprByNilLiteral = nominalBindingDecl->lookupConformance(
              DC->getParentModule(),
              getASTContext().getProtocol(
                  KnownProtocolKind::ExpressibleByNilLiteral),
              conformances);
        }
        wrapInOptional = !conformsToExprByNilLiteral;
      } else if (binding.isDefaultableBinding() &&
                 binding.BindingType->isAny()) {
        wrapInOptional = true;
      }

      if (wrapInOptional) {
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

/// Check whether the given type can be used as a binding for the given
/// type variable.
///
/// \returns the type to bind to, if the binding is okay.
Optional<Type> ConstraintSystem::checkTypeOfBinding(TypeVariableType *typeVar,
                                                    Type type) const {
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
  if (type->getRValueType()->is<TypeVariableType>())
    return None;

  // Don't bind to a dependent member type, even if it's currently
  // wrapped in any number of optionals, because binding producer
  // might unwrap and try to attempt it directly later.
  if (type->lookThroughAllOptionalTypes()->is<DependentMemberType>())
    return None;

  // Okay, allow the binding (with the simplified type).
  return type;
}

// Given a possibly-Optional type, return the direct superclass of the
// (underlying) type wrapped in the same number of optional levels as
// type.
static Type getOptionalSuperclass(Type type) {
  int optionalLevels = 0;
  while (auto underlying = type->getOptionalObjectType()) {
    ++optionalLevels;
    type = underlying;
  }

  if (!type->mayHaveSuperclass())
    return Type();

  auto superclass = type->getSuperclass();
  if (!superclass)
    return Type();

  while (optionalLevels--)
    superclass = OptionalType::get(superclass);

  return superclass;
}

/// Enumerates all of the 'direct' supertypes of the given type.
///
/// The direct supertype S of a type T is a supertype of T (e.g., T < S)
/// such that there is no type U where T < U and U < S.
static SmallVector<Type, 4> enumerateDirectSupertypes(Type type) {
  SmallVector<Type, 4> result;

  if (type->is<InOutType>() || type->is<LValueType>()) {
    type = type->getWithoutSpecifierType();
    result.push_back(type);
  }

  if (auto superclass = getOptionalSuperclass(type)) {
    // FIXME: Can also weaken to the set of protocol constraints, but only
    // if there are any protocols that the type conforms to but the superclass
    // does not.

    result.push_back(superclass);
  }

  // FIXME: lots of other cases to consider!
  return result;
}

bool TypeVarBindingProducer::computeNext() {
  SmallVector<Binding, 4> newBindings;
  auto addNewBinding = [&](Binding binding) {
    auto type = binding.BindingType;

    // If we've already tried this binding, move on.
    if (!BoundTypes.insert(type.getPointer()).second)
      return;

    if (!ExploredTypes.insert(type->getCanonicalType()).second)
      return;

    newBindings.push_back(std::move(binding));
  };

  for (auto &binding : Bindings) {
    const auto type = binding.BindingType;
    assert(!type->hasError());

    // After our first pass, note that we've explored these types.
    if (NumTries == 0)
      ExploredTypes.insert(type->getCanonicalType());

    // If we have a protocol with a default type, look for alternative
    // types to the default.
    if (NumTries == 0 && binding.hasDefaultedLiteralProtocol()) {
      auto knownKind =
          *(binding.getDefaultedLiteralProtocol()->getKnownProtocolKind());
      for (auto altType : CS.getAlternativeLiteralTypes(knownKind)) {
        addNewBinding(binding.withSameSource(altType, BindingKind::Subtypes));
      }
    }

    // Allow solving for T even for a binding kind where that's invalid
    // if fixes are allowed, because that gives us the opportunity to
    // match T? values to the T binding by adding an unwrap fix.
    if (binding.Kind == BindingKind::Subtypes || CS.shouldAttemptFixes()) {
      // If we were unsuccessful solving for T?, try solving for T.
      if (auto objTy = type->getOptionalObjectType()) {
        // If T is a type variable, only attempt this if both the
        // type variable we are trying bindings for, and the type
        // variable we will attempt to bind, both have the same
        // polarity with respect to being able to bind lvalues.
        if (auto otherTypeVar = objTy->getAs<TypeVariableType>()) {
          if (TypeVar->getImpl().canBindToLValue() ==
              otherTypeVar->getImpl().canBindToLValue()) {
            addNewBinding(binding.withSameSource(objTy, binding.Kind));
          }
        } else {
          addNewBinding(binding.withSameSource(objTy, binding.Kind));
        }
      }
    }

    auto srcLocator = binding.getLocator();
    if (srcLocator &&
        srcLocator->isLastElement<LocatorPathElt::ApplyArgToParam>() &&
        !type->hasTypeVariable() && CS.isCollectionType(type)) {
      // If the type binding comes from the argument conversion, let's
      // instead of binding collection types directly, try to bind
      // using temporary type variables substituted for element
      // types, that's going to ensure that subtype relationship is
      // always preserved.
      auto *BGT = type->castTo<BoundGenericType>();
      auto UGT = UnboundGenericType::get(BGT->getDecl(), BGT->getParent(),
                                         BGT->getASTContext());

      auto dstLocator = TypeVar->getImpl().getLocator();
      auto newType = CS.openUnboundGenericType(UGT, dstLocator)
                         ->reconstituteSugar(/*recursive=*/false);
      addNewBinding(binding.withType(newType));
    }

    if (binding.Kind == BindingKind::Supertypes) {
      for (auto supertype : enumerateDirectSupertypes(type)) {
        // If we're not allowed to try this binding, skip it.
        if (auto simplifiedSuper = CS.checkTypeOfBinding(TypeVar, supertype))
          addNewBinding(binding.withType(*simplifiedSuper));
      }
    }
  }

  if (newBindings.empty())
    return false;

  Index = 0;
  ++NumTries;
  Bindings = std::move(newBindings);
  return true;
}

bool TypeVariableBinding::attempt(ConstraintSystem &cs) const {
  auto type = Binding.BindingType;
  auto *srcLocator = Binding.getLocator();
  auto *dstLocator = TypeVar->getImpl().getLocator();

  if (Binding.hasDefaultedLiteralProtocol()) {
    type = cs.openUnboundGenericType(type, dstLocator);
    type = type->reconstituteSugar(/*recursive=*/false);
  }

  cs.addConstraint(ConstraintKind::Bind, TypeVar, type, srcLocator);

  // If this was from a defaultable binding note that.
  if (Binding.isDefaultableBinding()) {
    cs.DefaultedConstraints.push_back(srcLocator);

    if (type->isHole()) {
      // Reflect in the score that this type variable couldn't be
      // resolved and had to be bound to a placeholder "hole" type.
      cs.increaseScore(SK_Hole);

      ConstraintFix *fix = nullptr;
      if (auto *GP = TypeVar->getImpl().getGenericParameter()) {
        auto path = dstLocator->getPath();
        // Drop `generic parameter` locator element so that all missing
        // generic parameters related to the same path can be coalesced later.
        fix = DefaultGenericArgument::create(
            cs, GP,
            cs.getConstraintLocator(dstLocator->getAnchor(), path.drop_back()));
      } else if (TypeVar->getImpl().isClosureParameterType()) {
        fix = SpecifyClosureParameterType::create(cs, dstLocator);
      } else if (TypeVar->getImpl().isClosureResultType()) {
        fix = SpecifyClosureReturnType::create(cs, dstLocator);
      } else if (srcLocator->getAnchor() &&
                 isa<ObjectLiteralExpr>(srcLocator->getAnchor())) {
        fix = SpecifyObjectLiteralTypeImport::create(cs, dstLocator);
      } else if (srcLocator->isKeyPathRoot()) {
        fix = SpecifyKeyPathRootType::create(cs, dstLocator);
      }

      if (fix && cs.recordFix(fix))
        return true;
    }
  }

  return !cs.failedConstraint && !cs.simplify();
}
