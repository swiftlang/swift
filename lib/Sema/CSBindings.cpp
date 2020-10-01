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

void ConstraintSystem::PotentialBindings::inferTransitiveBindings(
    ConstraintSystem &cs, llvm::SmallPtrSetImpl<CanType> &existingTypes,
    const llvm::SmallDenseMap<TypeVariableType *,
                              ConstraintSystem::PotentialBindings>
        &inferredBindings) {
  using BindingKind = ConstraintSystem::AllowedBindingKind;

  llvm::SmallVector<Constraint *, 4> conversions;
  // First, let's collect all of the conversions associated
  // with this type variable.
  llvm::copy_if(
      Sources, std::back_inserter(conversions),
      [&](const Constraint *constraint) -> bool {
        if (constraint->getKind() != ConstraintKind::Subtype &&
            constraint->getKind() != ConstraintKind::Conversion &&
            constraint->getKind() != ConstraintKind::ArgumentConversion &&
            constraint->getKind() != ConstraintKind::OperatorArgumentConversion)
          return false;

        auto rhs = cs.simplifyType(constraint->getSecondType());
        return rhs->getAs<TypeVariableType>() == TypeVar;
      });

  for (auto *constraint : conversions) {
    auto *tv =
        cs.simplifyType(constraint->getFirstType())->getAs<TypeVariableType>();
    if (!tv || tv == TypeVar)
      continue;

    auto relatedBindings = inferredBindings.find(tv);
    if (relatedBindings == inferredBindings.end())
      continue;

    auto &bindings = relatedBindings->getSecond();

    // FIXME: This is a workaround necessary because solver doesn't filter
    // bindings based on protocol requirements placed on a type variable.
    //
    // Forward propagate (subtype -> supertype) only literal conformance
    // requirements since that helps solver to infer more types at
    // parameter positions.
    //
    // \code
    // func foo<T: ExpressibleByStringLiteral>(_: String, _: T) -> T {
    //   fatalError()
    // }
    //
    // func bar(_: Any?) {}
    //
    // func test() {
    //   bar(foo("", ""))
    // }
    // \endcode
    //
    // If one of the literal arguments doesn't propagate its
    // `ExpressibleByStringLiteral` conformance, we'd end up picking
    // `T` with only one type `Any?` which is incorrect.
    llvm::copy_if(bindings.Protocols, std::back_inserter(Protocols),
                  [](const Constraint *protocol) {
                    return protocol->getKind() ==
                           ConstraintKind::LiteralConformsTo;
                  });

    // Infer transitive defaults.
    llvm::copy(bindings.Defaults, std::back_inserter(Defaults));

    // TODO: We shouldn't need this in the future.
    if (constraint->getKind() != ConstraintKind::Subtype)
      continue;

    for (auto &binding : bindings.Bindings) {
      // We need the binding kind for the potential binding to
      // either be Exact or Supertypes in order for it to make sense
      // to add Supertype bindings based on the relationship between
      // our type variables.
      if (binding.Kind != BindingKind::Exact &&
          binding.Kind != BindingKind::Supertypes)
        continue;

      auto type = binding.BindingType;

      if (type->isHole())
        continue;

      if (!existingTypes.insert(type->getCanonicalType()).second)
        continue;

      if (ConstraintSystem::typeVarOccursInType(TypeVar, type))
        continue;

      addPotentialBinding(
          binding.withSameSource(type, BindingKind::Supertypes));
    }
  }
}

static bool
isUnviableDefaultType(Type defaultType,
                      llvm::SmallPtrSetImpl<CanType> &existingTypes) {
  auto canType = defaultType->getCanonicalType();

  if (!defaultType->hasUnboundGenericType())
    return !existingTypes.insert(canType).second;

  // For generic literal types, check whether we already have a
  // specialization of this generic within our list.
  // FIXME: This assumes that, e.g., the default literal
  // int/float/char/string types are never generic.
  auto nominal = defaultType->getAnyNominal();
  if (!nominal)
    return true;

  if (llvm::any_of(existingTypes, [&nominal](CanType existingType) {
        // FIXME: Check parents?
        return nominal == existingType->getAnyNominal();
      }))
    return true;

  existingTypes.insert(canType);
  return false;
}

void ConstraintSystem::PotentialBindings::inferDefaultTypes(
    ConstraintSystem &cs, llvm::SmallPtrSetImpl<CanType> &existingTypes) {
  auto isDirectRequirement = [&](Constraint *constraint) -> bool {
    if (auto *typeVar = constraint->getFirstType()->getAs<TypeVariableType>()) {
      auto *repr = cs.getRepresentative(typeVar);
      return repr == TypeVar;
    }

    return false;
  };

  // If we have any literal constraints, check whether there is already a
  // binding that provides a type that conforms to that literal protocol. In
  // such cases, don't add the default binding suggestion because the existing
  // suggestion is better.
  //
  // Note that ordering is important when it comes to bindings, we'd like to
  // add any "direct" default types first to attempt them before transitive
  // ones.
  //
  // Key is a literal protocol requirement, Value indicates whether (first)
  // given protocol is a direct requirement, and (second) whether it has been
  // covered by an existing binding.
  llvm::SmallMapVector<ProtocolDecl *, std::pair<bool, bool>, 4>
      literalProtocols;
  for (auto *constraint : Protocols) {
    if (constraint->getKind() == ConstraintKind::LiteralConformsTo)
      literalProtocols.insert({constraint->getProtocol(),
                               {isDirectRequirement(constraint), false}});
  }

  for (auto &binding : Bindings) {
    Type type;

    switch (binding.Kind) {
    case AllowedBindingKind::Exact:
      type = binding.BindingType;
      break;

    case AllowedBindingKind::Subtypes:
    case AllowedBindingKind::Supertypes:
      type = binding.BindingType->getRValueType();
      break;
    }

    if (type->isTypeVariableOrMember() || type->isHole())
      continue;

    bool requiresUnwrap = false;
    for (auto &entry : literalProtocols) {
      auto *protocol = entry.first;
      bool isDirectRequirement = entry.second.first;
      bool &isCovered = entry.second.second;

      if (isCovered)
        continue;

      // FIXME: This is a hack and it's incorrect because it depends
      // on ordering of the literal procotols e.g. if `ExpressibleByNilLiteral`
      // appears before e.g. `ExpressibleByIntegerLiteral` we'd drop
      // optionality although that would be incorrect.
      do {
        // If the type conforms to this protocol, we're covered.
        if (TypeChecker::conformsToProtocol(type, protocol, cs.DC)) {
          isCovered = true;
          break;
        }

        // If this literal protocol is not a direct requirement it
        // would be possible to change optionality while inferring
        // bindings for a supertype, so this hack doesn't apply.
        if (!isDirectRequirement)
          break;

        // If we're allowed to bind to subtypes, look through optionals.
        // FIXME: This is really crappy special case of computing a reasonable
        // result based on the given constraints.
        if (binding.Kind == AllowedBindingKind::Subtypes) {
          if (auto objTy = type->getOptionalObjectType()) {
            requiresUnwrap = true;
            type = objTy;
            continue;
          }
        }

        requiresUnwrap = false;
        break;
      } while (true);
    }

    if (requiresUnwrap)
      binding.BindingType = type;
  }

  // If this is not a literal protocol or it has been "covered" by an existing
  // binding it can't provide a default type.
  auto isUnviableForDefaulting = [&literalProtocols](ProtocolDecl *protocol) {
    auto literal = literalProtocols.find(protocol);
    return literal == literalProtocols.end() || literal->second.second;
  };

  for (auto *constraint : Protocols) {
    auto *protocol = constraint->getProtocol();

    if (isUnviableForDefaulting(protocol))
      continue;

    // Let's try to coalesce integer and floating point literal protocols
    // if they appear together because the only possible default type that
    // could satisfy both requirements is `Double`.
    if (protocol->isSpecificProtocol(
          KnownProtocolKind::ExpressibleByIntegerLiteral)) {
      auto *floatLiteral = cs.getASTContext().getProtocol(
          KnownProtocolKind::ExpressibleByFloatLiteral);
      // If `ExpressibleByFloatLiteral` is a requirement and it isn't
      // covered, let's skip `ExpressibleByIntegerLiteral` requirement.
      if (!isUnviableForDefaulting(floatLiteral))
        continue;
    }

    auto defaultType = TypeChecker::getDefaultType(protocol, cs.DC);
    if (!defaultType)
      continue;

    if (isUnviableDefaultType(defaultType, existingTypes))
      continue;

    // We need to figure out whether this is a direct conformance
    // requirement or inferred transitive one to identify binding
    // kind correctly.
    addPotentialBinding({defaultType,
                         isDirectRequirement(constraint)
                             ? AllowedBindingKind::Subtypes
                             : AllowedBindingKind::Supertypes,
                         constraint});
  }

  /// Add defaultable constraints.
  for (auto *constraint : Defaults) {
    Type type = constraint->getSecondType();
    if (!existingTypes.insert(type->getCanonicalType()).second)
      continue;

    if (constraint->getKind() == ConstraintKind::DefaultClosureType) {
      // If there are no other possible bindings for this closure
      // let's default it to the type inferred from its parameters/body,
      // otherwise we should only attempt contextual types as a
      // top-level closure type.
      if (!Bindings.empty())
        continue;
    }

    addPotentialBinding({type, AllowedBindingKind::Exact, constraint});
  }
}

void ConstraintSystem::PotentialBindings::finalize(
    ConstraintSystem &cs,
    const llvm::SmallDenseMap<TypeVariableType *,
                              ConstraintSystem::PotentialBindings>
        &inferredBindings) {
  // We need to make sure that there are no duplicate bindings in the
  // set, otherwise solver would produce multiple identical solutions.
  llvm::SmallPtrSet<CanType, 4> existingTypes;
  for (const auto &binding : Bindings)
    existingTypes.insert(binding.BindingType->getCanonicalType());

  inferTransitiveBindings(cs, existingTypes, inferredBindings);

  inferDefaultTypes(cs, existingTypes);

  // Adjust optionality of existing bindings based on presence of
  // `ExpressibleByNilLiteral` requirement.
  if (llvm::any_of(Protocols, [](Constraint *constraint) {
        auto *protocol = constraint->getProtocol();
        return protocol->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByNilLiteral);
      })) {
    for (auto &binding : Bindings) {
      bool wrapInOptional = false;
      if (binding.Kind == AllowedBindingKind::Supertypes) {
        auto type = binding.BindingType->getRValueType();
        // If the type doesn't conform to ExpressibleByNilLiteral,
        // produce an optional of that type as a potential binding. We
        // overwrite the binding in place because the non-optional type
        // will fail to type-check against the nil-literal conformance.
        bool conformsToExprByNilLiteral = false;
        if (auto *nominalBindingDecl = type->getAnyNominal()) {
          SmallVector<ProtocolConformance *, 2> conformances;
          conformsToExprByNilLiteral = nominalBindingDecl->lookupConformance(
              cs.DC->getParentModule(),
              cs.getASTContext().getProtocol(
                  KnownProtocolKind::ExpressibleByNilLiteral),
              conformances);
        }
        wrapInOptional = !conformsToExprByNilLiteral;
      } else if (binding.isDefaultableBinding() &&
                 binding.BindingType->isAny()) {
        wrapInOptional = true;
      }

      if (wrapInOptional)
        binding.BindingType = OptionalType::get(binding.BindingType);
    }
  }

  // If there are no bindings, typeVar may be a hole.
  if (cs.shouldAttemptFixes() && Bindings.empty() &&
      TypeVar->getImpl().canBindToHole()) {
    IsHole = true;
    // If the base of the unresolved member reference like `.foo`
    // couldn't be resolved we'd want to bind it to a hole at the
    // very last moment possible, just like generic parameters.
    auto *locator = TypeVar->getImpl().getLocator();
    if (locator->isLastElement<LocatorPathElt::MemberRefBase>())
      PotentiallyIncomplete = true;

    // Delay resolution of the code completion expression until
    // the very end to give it a chance to be bound to some
    // contextual type even if it's a hole.
    if (locator->directlyAt<CodeCompletionExpr>()) {
      FullyBound = true;
      PotentiallyIncomplete = true;
    }

    // Delay resolution of the `nil` literal to a hole until
    // the very end to give it a change to be bound to some
    // other type, just like code completion expression which
    // relies solely on contextual information.
    if (locator->directlyAt<NilLiteralExpr>()) {
      FullyBound = true;
      PotentiallyIncomplete = true;
    }

    // If this type variable is associated with a code completion token
    // and it failed to infer any bindings let's adjust hole's locator
    // to point to a code completion token to avoid attempting to "fix"
    // this problem since its rooted in the fact that constraint system
    // is under-constrained.
    if (AssociatedCodeCompletionToken) {
      locator = cs.getConstraintLocator(AssociatedCodeCompletionToken);
    }

    addPotentialBinding(PotentialBinding::forHole(TypeVar, locator));
  }

  // Let's always consider `Any` to be a last resort binding because
  // it's always better to infer concrete type and erase it if required
  // by the context.
  if (Bindings.size() > 1) {
    auto AnyTypePos =
        llvm::find_if(Bindings, [](const PotentialBinding &binding) {
          return binding.BindingType->isAny() &&
                 !binding.isDefaultableBinding();
        });

    if (AnyTypePos != Bindings.end()) {
      std::rotate(AnyTypePos, AnyTypePos + 1, Bindings.end());
    }
  }

  // Determine if the bindings only constrain the type variable from above with
  // an existential type; such a binding is not very helpful because it's
  // impossible to enumerate the existential type's subtypes.
  if (!Bindings.empty()) {
    SubtypeOfExistentialType =
        llvm::all_of(Bindings, [](const PotentialBinding &binding) {
          return binding.BindingType->isExistentialType() &&
                 binding.Kind == AllowedBindingKind::Subtypes;
        });
  }
}

Optional<ConstraintSystem::PotentialBindings>
ConstraintSystem::determineBestBindings() {
  // Look for potential type variable bindings.
  Optional<PotentialBindings> bestBindings;
  llvm::SmallDenseMap<TypeVariableType *, PotentialBindings> cache;

  // First, let's collect all of the possible bindings.
  for (auto *typeVar : getTypeVariables()) {
    if (!typeVar->getImpl().hasRepresentativeOrFixed())
      cache.insert({typeVar, inferBindingsFor(typeVar, /*finalize=*/false)});
  }

  // Determine whether given type variable with its set of bindings is
  // viable to be attempted on the next step of the solver. If type variable
  // has no "direct" bindings of any kind e.g. direct bindings to concrete
  // types, default types from "defaultable" constraints or literal
  // conformances, such type variable is not viable to be evaluated to be
  // attempted next.
  auto isViableForRanking =
      [this](const ConstraintSystem::PotentialBindings &bindings) -> bool {
    auto *typeVar = bindings.TypeVar;

    // If type variable is marked as a potential hole there is always going
    // to be at least one binding available for it.
    if (shouldAttemptFixes() && typeVar->getImpl().canBindToHole())
      return true;

    return !bindings.Bindings.empty() || !bindings.Defaults.empty() ||
           llvm::any_of(bindings.Protocols, [&](Constraint *constraint) {
             return bool(
                 TypeChecker::getDefaultType(constraint->getProtocol(), DC));
           });
  };

  // Now let's see if we could infer something for related type
  // variables based on other bindings.
  for (auto *typeVar : getTypeVariables()) {
    auto cachedBindings = cache.find(typeVar);
    if (cachedBindings == cache.end())
      continue;

    auto &bindings = cachedBindings->getSecond();

    // Before attempting to infer transitive bindings let's check
    // whether there are any viable "direct" bindings associated with
    // current type variable, if there are none - it means that this type
    // variable could only be used to transitively infer bindings for
    // other type variables and can't participate in ranking.
    //
    // Viable bindings include - any types inferred from constraints
    // associated with given type variable, any default constraints,
    // or any conformance requirements to literal protocols with can
    // produce a default type.
    bool isViable = isViableForRanking(bindings);

    bindings.finalize(*this, cache);

    if (!bindings || !isViable)
      continue;

    if (isDebugMode()) {
      bindings.dump(typeVar, llvm::errs(), solverState->depth * 2);
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
      !binding.BindingType->hasHole() &&
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

ConstraintSystem::PotentialBindings
ConstraintSystem::inferBindingsFor(TypeVariableType *typeVar, bool finalize) {
  assert(typeVar->getImpl().getRepresentative(nullptr) == typeVar &&
         "not a representative");
  assert(!typeVar->getImpl().getFixedType(nullptr) && "has a fixed type");

  PotentialBindings bindings(typeVar);

  // Gather the constraints associated with this type variable.
  auto constraints = CG.gatherConstraints(
      typeVar, ConstraintGraph::GatheringKind::EquivalenceClass);

  llvm::SmallPtrSet<CanType, 4> exactTypes;

  for (auto *constraint : constraints) {
    bool failed = bindings.infer(*this, exactTypes, constraint);

    // Upon inference failure let's produce an empty set of bindings.
    if (failed)
      return {typeVar};
  }

  if (finalize) {
    llvm::SmallDenseMap<TypeVariableType *, ConstraintSystem::PotentialBindings>
        inferred;

    bindings.finalize(*this, inferred);
  }

  return bindings;
}

Optional<ConstraintSystem::PotentialBinding>
ConstraintSystem::getPotentialBindingForRelationalConstraint(
    PotentialBindings &result, Constraint *constraint) const {
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
      result.FullyBound = true;
    }

    return None;
  }

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

    // If current type variable is associated with a code completion token
    // it's possible that it doesn't have enough contextual information
    // to be resolved to anything, so let's note that fact in the potential
    // bindings and use it when forming a hole if there are no other bindings
    // available.
    if (auto *locator = bindingTypeVar->getImpl().getLocator()) {
      if (locator->directlyAt<CodeCompletionExpr>()) {
        result.AssociatedCodeCompletionToken = locator->getAnchor();
        result.PotentiallyIncomplete = true;
      }
    }

    if (constraint->getKind() == ConstraintKind::Subtype &&
        kind == AllowedBindingKind::Subtypes) {
      result.SubtypeOf.insert(bindingTypeVar);
    }

    return None;
  }

  // Make sure we aren't trying to equate type variables with different
  // lvalue-binding rules.
  if (auto otherTypeVar = type->getAs<TypeVariableType>()) {
    if (typeVar->getImpl().canBindToLValue() !=
        otherTypeVar->getImpl().canBindToLValue())
      return None;
  }

  // If subtyping is allowed and this is a result of an implicit member chain,
  // let's delay binding it to an optional until its object type resolved too or
  // it has been determined that there is no possibility to resolve it. Otherwise
  // we might end up missing solutions since it's allowed to implicitly unwrap
  // base type of the chain but it can't be done early - type variable
  // representing chain's result type has a different l-valueness comparing
  // to generic parameter of the optional.
  if (kind == AllowedBindingKind::Subtypes) {
    auto *locator = typeVar->getImpl().getLocator();
    if (locator &&
        locator->isLastElement<LocatorPathElt::UnresolvedMemberChainResult>()) {
      auto objectType = type->getOptionalObjectType();
      if (objectType && objectType->isTypeVariableOrMember()) {
        result.PotentiallyIncomplete = true;
      }
    }
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
bool ConstraintSystem::PotentialBindings::infer(
    ConstraintSystem &cs, llvm::SmallPtrSetImpl<CanType> &exactTypes,
    Constraint *constraint) {
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
        constraint->getSecondType()->isEqual(TypeVar))
      PotentiallyIncomplete = true;

    auto binding =
        cs.getPotentialBindingForRelationalConstraint(*this, constraint);
    if (!binding)
      break;

    auto type = binding->BindingType;
    if (exactTypes.insert(type->getCanonicalType()).second) {
      addPotentialBinding(*binding);

      // Determines whether this type variable represents an object
      // of the optional type extracted by force unwrap.
      if (auto *locator = TypeVar->getImpl().getLocator()) {
        auto anchor = locator->getAnchor();
        // Result of force unwrap is always connected to its base
        // optional type via `OptionalObject` constraint which
        // preserves l-valueness, so in case where object type got
        // inferred before optional type (because it got the
        // type from context e.g. parameter type of a function call),
        // we need to test type with and without l-value after
        // delaying bindings for as long as possible.
        if (isExpr<ForceValueExpr>(anchor) && !type->is<LValueType>()) {
          addPotentialBinding(binding->withType(LValueType::get(type)));
          FullyBound = true;
        }

        // If this is a type variable representing closure result,
        // which is on the right-side of some relational constraint
        // let's have it try `Void` as well because there is an
        // implicit conversion `() -> T` to `() -> Void` and this
        // helps to avoid creating a thunk to support it.
        auto voidType = cs.getASTContext().TheEmptyTupleType;
        if (locator->isLastElement<LocatorPathElt::ClosureResult>() &&
            binding->Kind == AllowedBindingKind::Supertypes &&
            exactTypes.insert(voidType).second) {
          addPotentialBinding({voidType, binding->Kind, constraint},
                              /*allowJoinMeet=*/false);
        }
      }
    }
    break;
  }
  case ConstraintKind::KeyPathApplication: {
    if (FullyBound)
      return false;

    // If this variable is in the application projected result type, mark the
    // result as `FullyBound` to ensure we delay binding until we've bound
    // other type variables in the KeyPathApplication constraint. This ensures
    // we try to bind the key path type first, which can allow us to discover
    // additional bindings for the result type.
    SmallPtrSet<TypeVariableType *, 4> typeVars;
    findInferableTypeVars(cs.simplifyType(constraint->getThirdType()),
                          typeVars);
    if (typeVars.count(TypeVar))
      FullyBound = true;

    break;
  }

  case ConstraintKind::BridgingConversion:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::KeyPath:
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
      if (tv->getImpl().getRepresentative(nullptr) == TypeVar)
        return true;
    }

    // This is right-hand side, let's continue.
    break;
  }

  case ConstraintKind::Defaultable:
  case ConstraintKind::DefaultClosureType:
    // Do these in a separate pass.
    if (cs.getFixedTypeRecursive(constraint->getFirstType(), true)
            ->getAs<TypeVariableType>() == TypeVar) {
      Defaults.push_back(constraint);
    }
    break;

  case ConstraintKind::Disjunction:
    // FIXME: Recurse into these constraints to see whether this
    // type variable is fully bound by any of them.
    InvolvesTypeVariables = true;

    // If there is additional context available via disjunction
    // associated with closure literal (e.g. coercion to some other
    // type) let's delay resolving the closure until the disjunction
    // is attempted.
    if (TypeVar->getImpl().isClosureType())
      return true;

    break;

  case ConstraintKind::ConformsTo:
  case ConstraintKind::SelfObjectOfProtocol:
    return false;

  case ConstraintKind::LiteralConformsTo: {
    // Record constraint where protocol requirement originated
    // this is useful to use for the binding later.
    Protocols.push_back(constraint);
    break;
  }

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::DynamicCallableApplicableFunction:
  case ConstraintKind::BindOverload: {
    if (FullyBound && InvolvesTypeVariables)
      return false;

    // If this variable is in the left-hand side, it is fully bound.
    SmallPtrSet<TypeVariableType *, 4> typeVars;
    findInferableTypeVars(cs.simplifyType(constraint->getFirstType()),
                          typeVars);
    if (typeVars.count(TypeVar))
      FullyBound = true;

    if (InvolvesTypeVariables)
      return false;

    // If this and another type variable occur, this result involves
    // type variables.
    findInferableTypeVars(cs.simplifyType(constraint->getSecondType()),
                          typeVars);
    if (typeVars.size() > 1 && typeVars.count(TypeVar))
      InvolvesTypeVariables = true;

    break;
  }

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueWitness:
    // If our type variable shows up in the base type, there's
    // nothing to do.
    // FIXME: Can we avoid simplification here?
    if (ConstraintSystem::typeVarOccursInType(
            TypeVar, cs.simplifyType(constraint->getFirstType()),
            &InvolvesTypeVariables)) {
      return false;
    }

    // If the type variable is in the list of member type
    // variables, it is fully bound.
    // FIXME: Can we avoid simplification here?
    if (ConstraintSystem::typeVarOccursInType(
            TypeVar, cs.simplifyType(constraint->getSecondType()),
            &InvolvesTypeVariables)) {
      FullyBound = true;
    }
    break;

  case ConstraintKind::OneWayEqual:
  case ConstraintKind::OneWayBindParam: {
    // Don't produce any bindings if this type variable is on the left-hand
    // side of a one-way binding.
    auto firstType = constraint->getFirstType();
    if (auto *tv = firstType->getAs<TypeVariableType>()) {
      if (tv->getImpl().getRepresentative(nullptr) == TypeVar)
        return true;
    }

    break;
  }
  }

  return false;
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
        (srcLocator->isLastElement<LocatorPathElt::ApplyArgToParam>() ||
         srcLocator->isLastElement<LocatorPathElt::AutoclosureResult>()) &&
        !type->hasTypeVariable() && type->isKnownStdlibCollectionType()) {
      // If the type binding comes from the argument conversion, let's
      // instead of binding collection types directly, try to bind
      // using temporary type variables substituted for element
      // types, that's going to ensure that subtype relationship is
      // always preserved.
      auto *BGT = type->castTo<BoundGenericType>();
      auto dstLocator = TypeVar->getImpl().getLocator();
      auto newType = CS.openUnboundGenericType(BGT->getDecl(), BGT->getParent(),
                                               dstLocator)
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

Optional<std::pair<ConstraintFix *, unsigned>>
TypeVariableBinding::fixForHole(ConstraintSystem &cs) const {
  auto *dstLocator = TypeVar->getImpl().getLocator();
  auto *srcLocator = Binding.getLocator();

  // FIXME: This check could be turned into an assert once
  // all code completion kinds are ported to use
  // `TypeChecker::typeCheckForCodeCompletion` API.
  if (cs.isForCodeCompletion()) {
    // If the hole is originated from code completion expression
    // let's not try to fix this, anything connected to a
    // code completion is allowed to be a hole because presence
    // of a code completion token makes constraint system
    // under-constrained due to e.g. lack of expressions on the
    // right-hand side of the token, which are required for a
    // regular type-check.
    if (dstLocator->directlyAt<CodeCompletionExpr>())
      return None;
  }

  unsigned defaultImpact = 1;

  if (auto *GP = TypeVar->getImpl().getGenericParameter()) {
    // If it is represetative for a key path root, let's emit a more
    // specific diagnostic.
    auto *keyPathRoot =
        cs.isRepresentativeFor(TypeVar, ConstraintLocator::KeyPathRoot);
    if (keyPathRoot) {
      ConstraintFix *fix = SpecifyKeyPathRootType::create(
          cs, keyPathRoot->getImpl().getLocator());
      return std::make_pair(fix, defaultImpact);
    } else {
      auto path = dstLocator->getPath();
      // Drop `generic parameter` locator element so that all missing
      // generic parameters related to the same path can be coalesced later.
      ConstraintFix *fix = DefaultGenericArgument::create(
          cs, GP,
          cs.getConstraintLocator(dstLocator->getAnchor(), path.drop_back()));
      return std::make_pair(fix, defaultImpact);
    }
  }

  if (TypeVar->getImpl().isClosureParameterType()) {
    ConstraintFix *fix = SpecifyClosureParameterType::create(cs, dstLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (TypeVar->getImpl().isClosureResultType()) {
    auto *closure = castToExpr<ClosureExpr>(dstLocator->getAnchor());
    // If the whole body is being ignored due to a pre-check failure,
    // let's not record a fix about result type since there is
    // just not enough context to infer it without a body.
    if (cs.hasFixFor(cs.getConstraintLocator(closure->getBody()),
                     FixKind::IgnoreInvalidFunctionBuilderBody))
      return None;

    ConstraintFix *fix = SpecifyClosureReturnType::create(cs, dstLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (srcLocator->directlyAt<ObjectLiteralExpr>()) {
    ConstraintFix *fix = SpecifyObjectLiteralTypeImport::create(cs, dstLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (srcLocator->isKeyPathRoot()) {
    // If we recorded an invalid key path fix, let's skip this specify root
    // type fix because it wouldn't produce a useful diagnostic.
    auto *kpLocator = cs.getConstraintLocator(srcLocator->getAnchor());
    if (cs.hasFixFor(kpLocator, FixKind::AllowKeyPathWithoutComponents))
      return None;

    ConstraintFix *fix = SpecifyKeyPathRootType::create(cs, dstLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (dstLocator->directlyAt<NilLiteralExpr>()) {
    // This is a dramatic event, it means that there is absolutely
    // no contextual information to resolve type of `nil`.
    ConstraintFix *fix = SpecifyContextualTypeForNil::create(cs, dstLocator);
    return std::make_pair(fix, /*impact=*/(unsigned)10);
  }

  return None;
}

bool TypeVariableBinding::attempt(ConstraintSystem &cs) const {
  auto type = Binding.BindingType;
  auto *srcLocator = Binding.getLocator();
  auto *dstLocator = TypeVar->getImpl().getLocator();

  if (Binding.hasDefaultedLiteralProtocol()) {
    type = cs.openUnboundGenericTypes(type, dstLocator);
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

      if (auto fix = fixForHole(cs)) {
        if (cs.recordFix(/*fix=*/fix->first, /*impact=*/fix->second))
          return true;
      }
    }
  }

  return !cs.failedConstraint && !cs.simplify();
}
