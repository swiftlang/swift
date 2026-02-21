//===--- ConstraintSystem.cpp - Constraint-based Type Checking ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements various subclasses of BindingProducer to generate
// successive choices in a binding set, disjunction, or conjunction.
//
//===----------------------------------------------------------------------===//
#include "swift/Sema/BindingProducer.h"
#include "swift/Sema/Constraint.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/TypeVariableType.h"

using namespace swift;
using namespace constraints;
using namespace inference;

// Given a possibly-Optional type, return the direct superclass of the
// (underlying) type wrapped in the same number of optional levels as
// type.
static Type getOptionalSuperclass(Type type) {
  int optionalLevels = 0;
  while (auto underlying = type->getOptionalObjectType()) {
    ++optionalLevels;
    type = underlying;
  }

  Type superclass;
  if (auto *existential = type->getAs<ExistentialType>()) {
    auto constraintTy = existential->getConstraintType();
    if (auto *compositionTy = constraintTy->getAs<ProtocolCompositionType>()) {
      SmallVector<Type, 2> members;
      bool found = false;
      // Preserve all of the protocol requirements of the type i.e.
      // if the type was `any B & P` where `B : A` the supertype is
      // going to be `any A & P`.
      //
      // This is especially important for Sendable key paths because
      // to reserve sendability of the original type.
      for (auto member : compositionTy->getMembers()) {
        if (member->getClassOrBoundGenericClass()) {
          member = member->getSuperclass();
          if (!member)
            return Type();
          found = true;
        }
        members.push_back(member);
      }

      if (!found)
        return Type();

      superclass = ExistentialType::get(
          ProtocolCompositionType::get(type->getASTContext(), members,
                                       compositionTy->getInverses(),
                                       compositionTy->hasExplicitAnyObject()));
    } else {
      // Avoid producing superclass for situations like `any P` where `P` is
      // `protocol P : C`.
      return Type();
    }
  } else {
    superclass = type->getSuperclass();
  }

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

TypeVarBindingProducer::TypeVarBindingProducer(
    ConstraintSystem &cs,
    TypeVariableType *typeVar,
    const BindingSet &bindings)
    : BindingProducer(cs, typeVar->getImpl().getLocator()),
      TypeVar(typeVar), CanBeNil(bindings.canBeNil()) {
  if (bindings.isDirectHole()) {
    auto *locator = getLocator();
    // If this type variable is associated with a code completion token
    // and it failed to infer any bindings let's adjust holes's locator
    // to point to a code completion token to avoid attempting to "fix"
    // this problem since its rooted in the fact that constraint system
    // is under-constrained.
    if (bindings.getAssociatedCodeCompletionToken()) {
      locator =
          CS.getConstraintLocator(bindings.getAssociatedCodeCompletionToken());
    }

    Bindings.push_back(Binding::forHole(TypeVar, locator));
    return;
  }

  // A binding to `Any` which should always be considered as a last resort.
  std::optional<Binding> Any;

  auto addBinding = [&](const Binding &binding) {
    // Adjust optionality of existing bindings based on presence of
    // `ExpressibleByNilLiteral` requirement.
    if (requiresOptionalAdjustment(binding)) {
      Bindings.push_back(
          binding.withType(OptionalType::get(binding.BindingType)));
    } else if (binding.BindingType->isAny()) {
      Any.emplace(binding);
    } else {
      Bindings.push_back(binding);
    }
  };

  if (TypeVar->getImpl().isPackExpansion()) {
    SmallVector<Binding> viableBindings;

    // Collect possible contextual types (keep in mind that pack
    // expansion type variable gets bound to its "opened" type
    // regardless). To be viable the binding has to come from `bind`
    // or `equal` constraint (i.e. same-type constraint or explicit
    // generic argument) and be fully resolved.
    llvm::copy_if(bindings.Bindings, std::back_inserter(viableBindings),
                  [&](const Binding &binding) {
                    auto *source = binding.getSource();
                    if (source->getKind() == ConstraintKind::Bind ||
                        source->getKind() == ConstraintKind::Equal) {
                      auto type = binding.BindingType;
                      return type->is<PackExpansionType>() &&
                             !type->hasTypeVariable();
                    }
                    return false;
                  });

    // If there is a single fully resolved contextual type, let's
    // use it as a binding to help with performance and diagnostics.
    if (viableBindings.size() == 1) {
      addBinding(viableBindings.front());
    } else {
      for (auto *constraint : bindings.Defaults) {
        Bindings.push_back(getDefaultBinding(constraint));
      }
    }

    return;
  }

  for (const auto &binding : bindings.Bindings) {
    addBinding(binding);
  }

  // Infer defaults based on "uncovered" literal protocol requirements.
  for (const auto &literal : bindings.Literals) {
    if (!literal.viableAsBinding())
      continue;

    // We need to figure out whether this is a direct conformance
    // requirement or inferred transitive one to identify binding
    // kind correctly.
    addBinding({literal.getDefaultType(),
                literal.isDirectRequirement() ? BindingKind::Subtypes
                                              : BindingKind::Supertypes,
                literal.getSource()});
  }

  // Let's always consider `Any` to be a last resort binding because
  // it's always better to infer concrete type and erase it if required
  // by the context.
  if (Any) {
    Bindings.push_back(*Any);
  }

  {
    bool noBindings = Bindings.empty();

    for (auto *constraint : bindings.Defaults) {
      if (noBindings) {
        // If there are no direct or transitive bindings to attempt
        // let's add defaults to the list right away.
        Bindings.push_back(getDefaultBinding(constraint));
      } else {
        // Otherwise let's delay attempting default bindings
        // until all of the direct & transitive bindings and
        // their derivatives have been attempted.
        DelayedDefaults.push_back(constraint);
      }
    }
  }
}

bool TypeVarBindingProducer::requiresOptionalAdjustment(
    const Binding &binding) const {
  // If type variable can't be `nil` then adjustment is
  // not required.
  if (!CanBeNil)
    return false;

  if (binding.Kind == BindingKind::Supertypes) {
    auto type = binding.BindingType->getRValueType();
    // If the type doesn't conform to ExpressibleByNilLiteral,
    // produce an optional of that type as a potential binding. We
    // overwrite the binding in place because the non-optional type
    // will fail to type-check against the nil-literal conformance.
    auto *proto = CS.getASTContext().getProtocol(
         KnownProtocolKind::ExpressibleByNilLiteral);

    return !CS.lookupConformance(type, proto);
  } else if (binding.isDefaultableBinding() && binding.BindingType->isAny()) {
    return true;
  }

  return false;
}

PotentialBinding
TypeVarBindingProducer::getDefaultBinding(Constraint *constraint) const {
  assert(constraint->getKind() == ConstraintKind::Defaultable ||
         constraint->getKind() == ConstraintKind::FallbackType);

  auto type = constraint->getSecondType();
  Binding binding{type, BindingKind::Exact, constraint};
  return requiresOptionalAdjustment(binding)
             ? binding.withType(OptionalType::get(type))
             : binding;
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

  // Let's attempt only directly inferrable bindings for
  // a type variable representing a closure type because
  // such type variables are handled specially and only
  // bound to a type inferred from their expression, having
  // contextual bindings is just a trigger for that to
  // happen.
  if (TypeVar->getImpl().isClosureType())
    return false;

  for (auto &binding : Bindings) {
    const auto type = binding.BindingType;
    assert(!type->hasError());

    // If we have a protocol with a default type, look for alternative
    // types to the default.
    if (NumTries == 0 && binding.hasDefaultedLiteralProtocol()) {
      auto knownKind =
          *(binding.getDefaultedLiteralProtocol()->getKnownProtocolKind());
      SmallVector<Type, 2> scratch;
      for (auto altType : CS.getAlternativeLiteralTypes(knownKind, scratch)) {
        addNewBinding(binding.withSameSource(altType, BindingKind::Subtypes));
      }
    }

    if (getLocator()->directlyAt<ForceValueExpr>() &&
        TypeVar->getImpl().canBindToLValue() &&
        !binding.BindingType->is<LValueType>()) {
      // Result of force unwrap is always connected to its base
      // optional type via `OptionalObject` constraint which
      // preserves l-valueness, so in case where object type got
      // inferred before optional type (because it got the
      // type from context e.g. parameter type of a function call),
      // we need to test type with and without l-value after
      // delaying bindings for as long as possible.
      addNewBinding(binding.withType(LValueType::get(binding.BindingType)));
    }

    // There is a tailored fix for optional key path root references,
    // let's not create ambiguity by attempting unwrap when it's
    // not allowed.
    if (binding.Kind != BindingKind::Subtypes &&
        getLocator()->isKeyPathRoot() && type->getOptionalObjectType())
      continue;

    // Allow solving for T even for a binding kind where that's invalid
    // if fixes are allowed, because that gives us the opportunity to
    // match T? values to the T binding by adding an unwrap fix.
    if (binding.Kind == BindingKind::Subtypes || CS.shouldAttemptFixes()) {
      // If we were unsuccessful solving for T?, try solving for T.
      if (auto objTy = type->getOptionalObjectType()) {
        // TODO: This could be generalized in the future to cover all patterns
        // that have an intermediate type variable in subtype/conversion chain.
        //
        // Let's not perform $T? -> $T for closure result types to avoid having
        // to re-discover solutions that differ only in location of optional
        // injection. `Void` is a special case because in $T_result position
        // it has special semantics and enables T? -> Void conversions.
        //
        // The pattern with such type variables is:
        //
        // $T_body <conv/subtype> $T_result <conv/subtype> $T_contextual_result
        //
        // When $T_contextual_result is Optional<$U>, the optional injection
        // can either happen from $T_body or from $T_result (if `return`
        // expression is non-optional), if we allow  both the solver would
        // find two solutions that differ only in location of optional
        // injection.
        if (!TypeVar->getImpl().isClosureResultType() || objTy->isVoid() ||
            objTy->isTypeVariableOrMember()) {
          // If T is a type variable, only attempt this if both the
          // type variable we are trying bindings for, and the type
          // variable we will attempt to bind, both have the same
          // polarity with respect to being able to bind lvalues.
          if (auto otherTypeVar = objTy->getAs<TypeVariableType>()) {
            if (TypeVar->getImpl().canBindToLValue() ==
                otherTypeVar->getImpl().canBindToLValue()) {
              addNewBinding(binding.withType(objTy));
            }
          } else {
            addNewBinding(binding.withType(objTy));
          }
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
      auto newType =
          CS.openUnboundGenericType(BGT->getDecl(), BGT->getParent(),
                                    dstLocator, /*isTypeResolution=*/false)
              ->reconstituteSugar(/*recursive=*/false);
      addNewBinding(binding.withType(newType));
    }

    if (binding.Kind == BindingKind::Supertypes) {
      // If this is a type variable representing closure result,
      // which is on the right-side of some relational constraint
      // let's have it try `Void` as well because there is an
      // implicit conversion `() -> T` to `() -> Void` and this
      // helps to avoid creating a thunk to support it.
      // Avoid doing this is we already have a hole binding since
      // introducing Void will just cause local solution ambiguities.
      if (getLocator()->isLastElement<LocatorPathElt::ClosureResult>() &&
          binding.Kind == AllowedBindingKind::Supertypes &&
          !binding.BindingType->isPlaceholder()) {
        auto voidType = CS.getASTContext().TheEmptyTupleType;
        addNewBinding(binding.withSameSource(voidType, BindingKind::Exact));
      }

      for (auto supertype : enumerateDirectSupertypes(type)) {
        // If we're not allowed to try this binding, skip it.
        if (checkTypeOfBinding(TypeVar, supertype)) {
          // A key path type cannot be bound to type-erased key path variants.
          if (TypeVar->getImpl().isKeyPathType() &&
              isTypeErasedKeyPathType(supertype))
            continue;

          addNewBinding(binding.withType(supertype));
        }
      }
    }
  }

  if (newBindings.empty()) {
    // If key path type had contextual types, let's not attempt fallback.
    if (TypeVar->getImpl().isKeyPathType() && !ExploredTypes.empty())
      return false;

    // Add defaultable constraints (if any).
    for (auto *constraint : DelayedDefaults) {
      if (constraint->getKind() == ConstraintKind::FallbackType) {
        // If there are no other possible bindings for this variable
        // let's default it to the fallback type, otherwise we should
        // only attempt contextual types.
        if (!ExploredTypes.empty())
          continue;
      }

      addNewBinding(getDefaultBinding(constraint));
    }

    // Drop all of the default since we have converted them into bindings.
    DelayedDefaults.clear();
  }

  if (newBindings.empty())
    return false;

  Index = 0;
  ++NumTries;
  Bindings = std::move(newBindings);
  return true;
}

std::optional<std::pair<ConstraintFix *, unsigned>>
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
    if (dstLocator->directlyAt<CodeCompletionExpr>() ||
        srcLocator->directlyAt<CodeCompletionExpr>())
      return std::nullopt;
  }

  unsigned defaultImpact = 1;

  if (auto *GP = TypeVar->getImpl().getGenericParameter()) {
    // If it is representative for a key path root, let's emit a more
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
    auto *closureLoc = cs.getConstraintLocator(closure);
    if (cs.hasFixFor(closureLoc, FixKind::IgnoreInvalidResultBuilderBody) ||
        cs.hasFixFor(closureLoc, FixKind::IgnoreResultBuilderWithReturnStmts))
      return std::nullopt;

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
      return std::nullopt;

    // If key path has any invalid component, let's just skip fix because the
    // invalid component would be already diagnosed.
    auto keyPath = castToExpr<KeyPathExpr>(srcLocator->getAnchor());
    if (llvm::any_of(keyPath->getComponents(),
                     [](KeyPathExpr::Component component) {
                       return !component.isValid();
                     }))
      return std::nullopt;

    ConstraintFix *fix = SpecifyKeyPathRootType::create(cs, dstLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (srcLocator->isLastElement<LocatorPathElt::PlaceholderType>()) {
    // When a 'nil' has a placeholder as contextual type there is not enough
    // information to resolve it, so let's record a specify contextual type for
    // nil fix.
    if (isExpr<NilLiteralExpr>(srcLocator->getAnchor())) {
      ConstraintFix *fix = SpecifyContextualTypeForNil::create(cs, dstLocator);
      return std::make_pair(fix, /*impact=*/(unsigned)10);
    }

    // If the placeholder is in an invalid position, we'll have already
    // recorded a fix, and can skip recording another.
    if (cs.hasFixFor(dstLocator, FixKind::IgnoreInvalidPlaceholder))
      return std::nullopt;

    ConstraintFix *fix = SpecifyTypeForPlaceholder::create(cs, srcLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (dstLocator->directlyAt<NilLiteralExpr>()) {
    // This is a dramatic event, it means that there is absolutely
    // no contextual information to resolve type of `nil`.
    ConstraintFix *fix = SpecifyContextualTypeForNil::create(cs, dstLocator);
    return std::make_pair(fix, /*impact=*/(unsigned)10);
  }

  if (auto pattern = dstLocator->getPatternMatch()) {
    if (dstLocator->isLastElement<LocatorPathElt::PatternDecl>()) {
      // If this is the pattern in a for loop, and we have a mismatch of the
      // element type, then we don't have any useful contextual information
      // for the pattern, and can just bind to a hole without needing to penalize
      // the solution further.
      auto *seqLoc = cs.getConstraintLocator(
          dstLocator->getAnchor(), ConstraintLocator::SequenceElementType);
      if (cs.hasFixFor(seqLoc,
                       FixKind::IgnoreCollectionElementContextualMismatch)) {
        return std::nullopt;
      }
      if (dstLocator->getAnchor().isExpr(ExprKind::CodeCompletion)) {
        // Ignore the hole if it is because the right-hand-side of the pattern
        // match is a code completion token. Assigning a high fix score to this
        // mismatch won't help. In fact, it can harm because we might have a
        // different exploration path in the constraint system that gives up
        // earlier (eg. because code completion is in a closure that doesn't
        // match the expected parameter of a function call) and might thus get a
        // better score, despite not having any information about the code
        // completion token at all.
        return std::nullopt;
      }
      // Not being able to infer the type of a variable in a pattern binding
      // decl is more dramatic than anything that could happen inside the
      // expression because we want to preferrably point the diagnostic to a
      // part of the expression that caused us to be unable to infer the
      // variable's type.
      ConstraintFix *fix =
          IgnoreUnresolvedPatternVar::create(cs, pattern.get(), dstLocator);
      return std::make_pair(fix, /*impact=*/(unsigned)100);
    }
  }

  if (srcLocator->isLastElement<LocatorPathElt::MemberRefBase>()) {
    auto *baseExpr = castToExpr<UnresolvedMemberExpr>(srcLocator->getAnchor());
    ConstraintFix *fix = SpecifyBaseTypeForContextualMember::create(
        cs, baseExpr->getName(), srcLocator);
    return std::make_pair(fix, defaultImpact);
  }

  if (dstLocator->isLastElement<LocatorPathElt::PackElement>()) {
    // A hole appears as an element of generic pack params
    ConstraintFix *Fix = SpecifyPackElementType::create(cs, dstLocator);
    return std::make_pair(Fix, defaultImpact);
  }

  return std::nullopt;
}

static bool shouldIgnoreHoleForCodeCompletion(ConstraintSystem &cs,
                                              TypeVariableType *typeVar,
                                              ConstraintLocator *srcLocator) {
  if (!cs.isForCodeCompletion())
    return false;

  // Don't penalize solutions with unresolved generics.
  if (typeVar->getImpl().getGenericParameter())
    return true;

  // Don't penalize solutions if we couldn't determine the type of the code
  // completion token. We still want to examine the surrounding types in
  // that case.
  if (typeVar->getImpl().isCodeCompletionToken())
    return true;

  // When doing completion in a result builder, we avoid solving unrelated
  // expressions by replacing them with unbound placeholder variables.
  // As such, we need to avoid penalizing holes for references to
  // placeholder variables.
  if (srcLocator->isLastElement<LocatorPathElt::PlaceholderType>()) {
    if (auto *DRE = getAsExpr<DeclRefExpr>(srcLocator->getAnchor())) {
      if (auto *VD = dyn_cast_or_null<VarDecl>(DRE->getDecl())) {
        if (auto *PBD = VD->getParentPatternBinding()) {
          if (isPlaceholderVar(PBD))
            return true;
        }
      }
    }
  }

  // Don't penalize solutions with holes due to missing arguments after the
  // code completion position.
  auto argLoc = srcLocator->findLast<LocatorPathElt::SynthesizedArgument>();
  if (argLoc && argLoc->isAfterCodeCompletionLoc())
    return true;

  // Don't penalize solutions that have holes for ignored arguments.
  if (cs.hasArgumentsIgnoredForCodeCompletion()) {
    // Avoid simplifying the locator if the constraint system didn't ignore
    // any arguments.
    auto argExpr = simplifyLocatorToAnchor(typeVar->getImpl().getLocator());
    if (cs.isArgumentIgnoredForCodeCompletion(argExpr.dyn_cast<Expr *>())) {
      return true;
    }
  }
  return false;
}

bool TypeVariableBinding::attempt(ConstraintSystem &cs) const {
  auto type = Binding.BindingType;
  auto *srcLocator = Binding.getLocator();
  auto *dstLocator = TypeVar->getImpl().getLocator();

  if (Binding.hasDefaultedLiteralProtocol()) {
    type = cs.replaceInferableTypesWithTypeVars(type, dstLocator);
    type = type->reconstituteSugar(/*recursive=*/false);
  }

  // If type variable has been marked as a possible hole due to
  // e.g. reference to a missing member. Let's propagate that
  // information to the object type of the optional type it's
  // about to be bound to.
  //
  // In some situations like pattern bindings e.g. `if let x = base?.member`
  // - if `member` doesn't exist, `x` cannot be determined either, which
  // leaves `OptionalEvaluationExpr` representing outer type of `base?.member`
  // without any contextual information, so even though `x` would get
  // bound to result type of the chain, underlying type variable wouldn't
  // be resolved, so we need to propagate holes up the conversion chain.
  // Also propagate in code completion mode because in some cases code
  // completion relies on type variable being a potential hole.
  if (TypeVar->getImpl().canBindToHole()) {
    if (srcLocator->directlyAt<OptionalEvaluationExpr>() ||
        cs.isForCodeCompletion()) {
      if (auto objectTy = type->getOptionalObjectType()) {
        if (auto *typeVar = objectTy->getAs<TypeVariableType>()) {
          cs.recordPotentialHole(typeVar);
        }
      }
    }
  }

  ConstraintSystem::TypeMatchOptions options;

  options |= ConstraintSystem::TMF_GenerateConstraints;
  options |= ConstraintSystem::TMF_BindingTypeVariable;

  auto result =
      cs.matchTypes(TypeVar, type, ConstraintKind::Bind, options, srcLocator);

  if (result.isFailure()) {
    if (cs.isDebugMode()) {
      PrintOptions PO = PrintOptions::forDebugging();

      llvm::errs().indent(cs.solverState->getCurrentIndent())
          << "(failed to establish binding " << TypeVar->getString(PO)
          << " := " << type->getString(PO) << ")\n";
    }
    return false;
  }

  auto reportHole = [&]() {
    if (shouldIgnoreHoleForCodeCompletion(cs, TypeVar, srcLocator))
      return false;

    // Reflect in the score that this type variable couldn't be
    // resolved and had to be bound to a placeholder "hole" type.
    cs.increaseScore(SK_Hole, srcLocator);

    if (auto fix = fixForHole(cs)) {
      if (cs.recordFix(/*fix=*/fix->first, /*impact=*/fix->second))
        return true;
    }
    return false;
  };

  // If this was from a defaultable binding note that.
  if (Binding.isDefaultableBinding()) {
    cs.recordDefaultedConstraint(srcLocator);

    // Fail if hole reporting fails.
    if (type->isPlaceholder() && reportHole())
      return false;
  }

  if (cs.simplify())
    return false;

  // If all of the re-activated constraints where simplified,
  // let's notify binding inference about the fact that type
  // variable has been bound successfully.
  cs.getConstraintGraph().introduceToInference(TypeVar, type);

  return true;
}

namespace {

/// Find any type variable references inside of an AST node.
class TypeVariableRefFinder : public ASTWalker {
  /// A stack of all closures the walker encountered so far.
  SmallVector<DeclContext *> ClosureDCs;

  ConstraintSystem &CS;
  ASTNode Parent;

  llvm::SmallPtrSetImpl<TypeVariableType *> &ReferencedVars;

public:
  TypeVariableRefFinder(
      ConstraintSystem &cs, ASTNode parent, ContextualTypeInfo context,
      llvm::SmallPtrSetImpl<TypeVariableType *> &referencedVars)
      : CS(cs), Parent(parent), ReferencedVars(referencedVars) {
    if (auto ty = context.getType())
      inferVariables(ty);
    if (auto *closure = getAsExpr<ClosureExpr>(Parent))
      ClosureDCs.push_back(closure);
  }

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::Arguments;
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *expr) override {
    if (auto *closure = dyn_cast<ClosureExpr>(expr)) {
      ClosureDCs.push_back(closure);
    }

    if (auto *joinExpr = dyn_cast<TypeJoinExpr>(expr)) {
      // If this join is over a known type, let's
      // analyze it too because it can contain type
      // variables.
      if (!joinExpr->getVar())
        inferVariables(joinExpr->getType());
    }

    if (auto *DRE = dyn_cast<DeclRefExpr>(expr)) {
      auto *decl = DRE->getDecl();

      if (auto type = CS.getTypeIfAvailable(decl)) {
        auto &ctx = CS.getASTContext();
        // If this is not one of the closure parameters which
        // is inferrable from the body, let's replace type
        // variables with errors to avoid bringing external
        // information to the element component.
        if (type->hasTypeVariable() &&
            !(isa<ParamDecl>(decl) || decl->getName() == ctx.Id_builderSelf)) {
          // If there are type variables left in the simplified version,
          // it means that this is an invalid external declaration
          // relative to this element's context.
          if (CS.simplifyType(type)->hasTypeVariable()) {
            auto transformedTy = type.transformRec([&](Type type) -> std::optional<Type> {
              if (type->is<TypeVariableType>()) {
                return Type(ErrorType::get(CS.getASTContext()));
              }
              return std::nullopt;
            });

            CS.setType(decl, transformedTy);
            return Action::Continue(expr);
          }
        }

        inferVariables(type);
        return Action::Continue(expr);
      }

      auto var = dyn_cast<VarDecl>(decl);
      if (!var)
        return Action::Continue(expr);

      // If there is no type recorded yet, let's check whether
      // it is a placeholder variable implicitly generated by the
      // compiler.
      if (auto *PB = var->getParentPatternBinding()) {
        if (auto placeholderTy = isPlaceholderVar(PB)) {
          auto openedTy = CS.replaceInferableTypesWithTypeVars(
              placeholderTy, CS.getConstraintLocator(expr));
          inferVariables(openedTy);
          CS.setType(var, openedTy);
        }
      }
    }

    // If closure appears inside of a pack expansion, the elements
    // that reference pack elements have to bring expansion's shape
    // type in scope to make sure that the shapes match.
    if (auto *packElement = getAsExpr<PackElementExpr>(expr)) {
      if (auto *outerExpansion = CS.getPackElementExpansion(packElement)) {
        auto *expansionTy = CS.simplifyType(CS.getType(outerExpansion))
                                ->castTo<PackExpansionType>();
        expansionTy->getCountType()->getTypeVariables(ReferencedVars);
      }
    }

    return Action::Continue(expr);
  }

  PostWalkResult<Expr *> walkToExprPost(Expr *expr) override {
    if (isa<ClosureExpr>(expr)) {
      ClosureDCs.pop_back();
    }
    return Action::Continue(expr);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *stmt) override {
    // Return statements have to reference outside result type
    // since all of them are joined by it if it's not specified
    // explicitly.
    if (isa<ReturnStmt>(stmt)) {
      if (auto *closure = getAsExpr<ClosureExpr>(Parent)) {
        // Return is only viable if it belongs to a parent closure.
        if (currentClosureDC() == closure)
          inferVariables(CS.getClosureType(closure)->getResult());
      }
    }

    return Action::Continue(stmt);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    /// Decls get type-checked separately, except for PatternBindingDecls,
    /// whose initializers we want to walk into.
    return Action::VisitNodeIf(isa<PatternBindingDecl>(D));
  }

private:
  DeclContext *currentClosureDC() const {
    return ClosureDCs.empty() ? nullptr : ClosureDCs.back();
  }

  void inferVariables(Type type) {
    type = type->getWithoutSpecifierType();
    // Record the type variable itself because it has to
    // be in scope even when already bound.
    if (auto *typeVar = type->getAs<TypeVariableType>()) {
      ReferencedVars.insert(typeVar);

      // It is possible that contextual type of a parameter/result
      // has been assigned to e.g. an anonymous or named argument
      // early, to facilitate closure type checking. Such a
      // type can have type variables inside e.g.
      //
      // func test<T>(_: (UnsafePointer<T>) -> Void) {}
      //
      // test { ptr in
      //  ...
      // }
      //
      // Type variable representing `ptr` in the body of
      // this closure would be bound to `UnsafePointer<$T>`
      // in this case, where `$T` is a type variable for a
      // generic parameter `T`.
      type = CS.getFixedTypeRecursive(typeVar, /*wantRValue=*/false);

      if (type->isEqual(typeVar))
        return;
    }

    // Desugar type before collecting type variables, otherwise
    // we can bring in scope unrelated type variables passed
    // into the closure (via parameter/result) from contextual type.
    // For example `Typealias<$T, $U>.Context` which desugars into
    // `_Context<$U>` would bring in `$T` that could be inferrable
    // only after the body of the closure is solved.
    type = type->getCanonicalType();

    // Don't walk into the opaque archetypes because they are not
    // transparent in this context - `some P` could reference a
    // type variables as substitutions which are visible only to
    // the outer context.
    if (type->is<OpaqueTypeArchetypeType>())
      return;

    if (type->hasTypeVariable()) {
      SmallPtrSet<TypeVariableType *, 4> typeVars;
      type->getTypeVariables(typeVars);

      // Some of the type variables could be non-representative, so
      // we need to recurse into `inferTypeVariables` to property
      // handle them.
      for (auto *typeVar : typeVars)
        inferVariables(typeVar);
    }
  }
};

}

void ConjunctionElement::findReferencedVariables(
    ConstraintSystem &cs, SmallPtrSetImpl<TypeVariableType *> &typeVars) const {
  auto referencedVars = Element->getTypeVariables();
  typeVars.insert(referencedVars.begin(), referencedVars.end());

  if (Element->getKind() != ConstraintKind::SyntacticElement)
    return;

  ASTNode element = Element->getSyntacticElement();
  auto *locator = Element->getLocator();

  ASTNode parent = locator->getAnchor();
  if (auto *SVE = getAsExpr<SingleValueStmtExpr>(parent)) {
    // Use a parent closure if we have one. This is needed to correctly handle
    // return statements that refer to an outer closure.
    if (auto *CE = dyn_cast<ClosureExpr>(SVE->getDeclContext()))
      parent = CE;
  }

  TypeVariableRefFinder refFinder(cs, parent, Element->getElementContext(),
                                  typeVars);

  // If this is a pattern of `for-in` statement, let's walk into `for-in`
  // sequence expression because both elements are type-checked together.
  //
  // Correct expressions wouldn't have any type variables in sequence but
  // they could appear due to circular references or other incorrect syntax.
  if (isa<Pattern *>(element)) {
    if (auto parent =
            locator->getLastElementAs<LocatorPathElt::SyntacticElement>()) {
      if (auto *forEach = getAsStmt<ForEachStmt>(parent->getElement())) {
        if (auto *sequence = forEach->getSequence())
          sequence->walk(refFinder);
        return;
      }
    }
  }

  if (auto *patternBinding =
          dyn_cast_or_null<PatternBindingDecl>(element.dyn_cast<Decl *>())) {
    // Let's not walk into placeholder variable initializers, since they
    // are type-checked separately right now.
    if (isPlaceholderVar(patternBinding))
      return;

    if (auto patternBindingElt =
            locator
                ->getLastElementAs<LocatorPathElt::PatternBindingElement>()) {
      if (auto *init = patternBinding->getInit(patternBindingElt->getIndex()))
        init->walk(refFinder);
      return;
    }
  }

  if (isa<Decl *>(element) || isa<StmtConditionElement *>(element) ||
      isa<Expr *>(element) || element.isPattern(PatternKind::Expr) ||
      element.isStmt(StmtKind::Return)) {
    element.walk(refFinder);
  }
}