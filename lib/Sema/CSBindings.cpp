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
#include "swift/Sema/CSBindings.h"
#include "TypeChecker.h"
#include "swift/AST/ExistentialLayout.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/Basic/Assertions.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>

#define DEBUG_TYPE "PotentialBindings"

using namespace swift;
using namespace constraints;
using namespace inference;


void ConstraintGraphNode::initBindingSet() {
  ASSERT(!hasBindingSet());
  ASSERT(forRepresentativeVar());

  Set.emplace(CG.getConstraintSystem(), TypeVar, Potential);
}

static std::optional<Type> checkTypeOfBinding(TypeVariableType *typeVar,
                                              Type type);

BindingSet::BindingSet(ConstraintSystem &CS, TypeVariableType *TypeVar,
                       const PotentialBindings &info)
    : CS(CS), TypeVar(TypeVar), Info(info) {

  for (const auto &binding : info.Bindings)
    addBinding(binding, /*isTransitive=*/false);

  for (auto *constraint : info.Constraints) {
    switch (constraint->getKind()) {
    case ConstraintKind::NonisolatedConformsTo:
    case ConstraintKind::ConformsTo:
      if (constraint->getSecondType()->is<ProtocolType>())
        Protocols.push_back(constraint);
      break;

    case ConstraintKind::LiteralConformsTo:
      addLiteralRequirement(constraint);
      break;

    case ConstraintKind::Defaultable:
    case ConstraintKind::FallbackType:
      // Do these in a separate pass.
      if (CS.getFixedTypeRecursive(constraint->getFirstType(), true)
              ->getAs<TypeVariableType>() == TypeVar) {
        addDefault(constraint);
      }
      break;

    default:
      break;
    }
  }

  for (auto &entry : info.AdjacentVars)
    AdjacentVars.insert(entry.first);
}

bool BindingSet::forClosureResult() const {
  return TypeVar->getImpl().isClosureResultType();
}

bool BindingSet::forGenericParameter() const {
  return bool(TypeVar->getImpl().getGenericParameter());
}

bool BindingSet::canBeNil() const {
  auto &ctx = CS.getASTContext();
  return Literals.count(
      ctx.getProtocol(KnownProtocolKind::ExpressibleByNilLiteral));
}

bool BindingSet::isDirectHole() const {
  // Direct holes are only allowed in "diagnostic mode".
  if (!CS.shouldAttemptFixes())
    return false;

  return Bindings.empty() && getNumViableLiteralBindings() == 0 &&
         Defaults.empty() && TypeVar->getImpl().canBindToHole();
}

static bool isGenericParameter(TypeVariableType *TypeVar) {
  auto *locator = TypeVar->getImpl().getLocator();
  return locator && locator->isLastElement<LocatorPathElt::GenericParameter>();
}

bool PotentialBinding::isViableForJoin() const {
  return Kind == AllowedBindingKind::Supertypes &&
         !BindingType->hasLValueType() &&
         !BindingType->hasUnresolvedType() &&
         !BindingType->hasTypeVariable() &&
         !BindingType->hasPlaceholder() &&
         !BindingType->hasUnboundGenericType() &&
         !hasDefaultedLiteralProtocol() &&
         !isDefaultableBinding();
}

bool BindingSet::isDelayed() const {
  if (auto *locator = TypeVar->getImpl().getLocator()) {
    if (locator->isLastElement<LocatorPathElt::MemberRefBase>()) {
      // If first binding is a "fallback" to a protocol type,
      // it means that this type variable should be delayed
      // until it either gains more contextual information, or
      // there are no other type variables to attempt to make
      // forward progress.
      if (Bindings.empty())
        return true;

      if (Bindings[0].BindingType->is<ProtocolType>()) {
        auto *bindingLoc = Bindings[0].getLocator();
        // This set shouldn't be delayed because there won't be any
        // other inference sources when the protocol binding got
        // inferred from a contextual type and the leading-dot chain
        // this type variable is a base of, is connected directly to it.

        if (!bindingLoc->findLast<LocatorPathElt::ContextualType>())
          return true;

        auto *chainResult =
            getAsExpr<UnresolvedMemberChainResultExpr>(bindingLoc->getAnchor());
        if (!chainResult || CS.getParentExpr(chainResult) ||
            chainResult->getChainBase() != getAsExpr(locator->getAnchor()))
          return true;
      }
    }

    // Since force unwrap preserves l-valueness, resulting
    // type variable has to be delayed until either l-value
    // binding becomes available or there are no other
    // variables to attempt.
    if (locator->directlyAt<ForceValueExpr>() &&
        TypeVar->getImpl().canBindToLValue()) {
      return llvm::none_of(Bindings, [](const PotentialBinding &binding) {
        return binding.BindingType->is<LValueType>();
      });
    }
  }

  // Delay key path literal type binding until there is at least
  // one contextual binding (or default is promoted into a binding).
  if (TypeVar->getImpl().isKeyPathType() && !Defaults.empty())
    return true;

  if (isHole()) {
    auto *locator = TypeVar->getImpl().getLocator();
    assert(locator && "a hole without locator?");

    // Delay resolution of the code completion expression until
    // the very end to give it a chance to be bound to some
    // contextual type even if it's a hole.
    if (locator->directlyAt<CodeCompletionExpr>())
      return true;

    // Delay resolution of the `nil` literal to a hole until
    // the very end to give it a change to be bound to some
    // other type, just like code completion expression which
    // relies solely on contextual information.
    if (locator->directlyAt<NilLiteralExpr>())
      return true;

    // When inferring the type of a variable in a pattern, delay its resolution
    // so that we resolve type variables inside the expression as placeholders
    // instead of marking the type of the variable itself as a placeholder. This
    // allows us to produce more specific errors because the type variable in
    // the expression that introduced the placeholder might be diagnosable using
    // fixForHole.
    if (locator->isLastElement<LocatorPathElt::PatternDecl>()) {
      return true;
    }

    // It's possible that type of member couldn't be determined,
    // and if so it would be beneficial to bind member to a hole
    // early to propagate that information down to arguments,
    // result type of a call that references such a member.
    //
    // Note: This is done here instead of during binding inference,
    // because it's possible that variable is marked as a "hole"
    // (or that status is propagated to it) after constraints
    // mentioned below are recorded.
    return llvm::any_of(Info.DelayedBy, [&](Constraint *constraint) {
      switch (constraint->getKind()) {
      case ConstraintKind::ApplicableFunction:
      case ConstraintKind::DynamicCallableApplicableFunction:
      case ConstraintKind::BindOverload: {
        return !ConstraintSystem::typeVarOccursInType(
            TypeVar, CS.simplifyType(constraint->getSecondType()));
      }

      default:
        return true;
      }
    });
  }

  return !Info.DelayedBy.empty();
}

bool BindingSet::involvesTypeVariables() const {
  // This type variable always depends on a pack expansion variable
  // which should be inferred first if possible.
  if (TypeVar->getImpl().getGenericParameter() &&
      TypeVar->getImpl().canBindToPack())
    return true;

  // This is effectively O(1) right now since bindings are re-computed
  // on each step of the solver, but once bindings are computed
  // incrementally it becomes more important to double-check that
  // any adjacent type variables found previously are still unresolved.
  return llvm::any_of(AdjacentVars, [](TypeVariableType *typeVar) {
    return !typeVar->getImpl().getFixedType(/*record=*/nullptr);
  });
}

bool BindingSet::isPotentiallyIncomplete() const {
  // Generic parameters are always potentially incomplete.
  if (isGenericParameter(TypeVar))
    return true;

  // Key path literal type is incomplete until there is a
  // contextual type or key path is resolved enough to infer
  // capability and promote default into a binding.
  if (TypeVar->getImpl().isKeyPathType())
    return !Defaults.empty();

  // If current type variable is associated with a code completion token
  // it's possible that it doesn't have enough contextual information
  // to be resolved to anything so let's delay considering it until everything
  // else is resolved.
  if (Info.AssociatedCodeCompletionToken)
    return true;

  auto *locator = TypeVar->getImpl().getLocator();
  if (!locator)
    return false;

  if (locator->isLastElement<LocatorPathElt::MemberRefBase>() &&
      !Bindings.empty()) {
    // If the base of the unresolved member reference like `.foo`
    // couldn't be resolved we'd want to bind it to a hole at the
    // very last moment possible, just like generic parameters.
    if (isHole())
      return true;

    auto &binding = Bindings.front();
    // If base type of a member chain is inferred to be a protocol type,
    // let's consider this binding set to be potentially incomplete since
    // that's done as a last resort effort at resolving first member.
    if (auto *constraint = binding.getSource()) {
      if (binding.BindingType->is<ProtocolType>() &&
          (constraint->getKind() == ConstraintKind::ConformsTo ||
           constraint->getKind() == ConstraintKind::NonisolatedConformsTo))
        return true;
    }
  }

  if (locator->isLastElement<LocatorPathElt::UnresolvedMemberChainResult>()) {
    // If subtyping is allowed and this is a result of an implicit member chain,
    // let's delay binding it to an optional until its object type resolved too or
    // it has been determined that there is no possibility to resolve it. Otherwise
    // we might end up missing solutions since it's allowed to implicitly unwrap
    // base type of the chain but it can't be done early - type variable
    // representing chain's result type has a different l-valueness comparing
    // to generic parameter of the optional.
    if (llvm::any_of(Bindings, [&](const PotentialBinding &binding) {
          if (binding.Kind != AllowedBindingKind::Subtypes)
            return false;

          auto objectType = binding.BindingType->getOptionalObjectType();
          return objectType && objectType->isTypeVariableOrMember();
        }))
      return true;
  }

  if (isHole()) {
    // Delay resolution of the code completion expression until
    // the very end to give it a chance to be bound to some
    // contextual type even if it's a hole.
    if (locator->directlyAt<CodeCompletionExpr>())
      return true;

    // Delay resolution of the `nil` literal to a hole until
    // the very end to give it a change to be bound to some
    // other type, just like code completion expression which
    // relies solely on contextual information.
    if (locator->directlyAt<NilLiteralExpr>())
      return true;
  }

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
  if (llvm::any_of(
          Info.EquivalentTo,
          [&](const std::pair<TypeVariableType *, Constraint *> &equivalence) {
            auto *constraint = equivalence.second;
            return constraint->getKind() == ConstraintKind::BindParam &&
                   constraint->getSecondType()->isEqual(TypeVar);
          }))
    return true;

  return false;
}

void BindingSet::inferTransitiveProtocolRequirements() {
  if (TransitiveProtocols)
    return;

  llvm::SmallVector<std::pair<TypeVariableType *, TypeVariableType *>, 4>
      workList;
  llvm::SmallPtrSet<TypeVariableType *, 4> visitedRelations;

  llvm::SmallDenseMap<TypeVariableType *, SmallPtrSet<Constraint *, 4>, 4>
      protocols;

  auto addToWorkList = [&](TypeVariableType *parent,
                           TypeVariableType *typeVar) {
    if (visitedRelations.insert(typeVar).second)
      workList.push_back({parent, typeVar});
  };

  auto propagateProtocolsTo =
      [&protocols](TypeVariableType *dstVar,
                   const ArrayRef<Constraint *> &direct,
                   const SmallPtrSetImpl<Constraint *> &transitive) {
        auto &destination = protocols[dstVar];

        if (direct.size() > 0)
          destination.insert(direct.begin(), direct.end());

        if (transitive.size() > 0)
          destination.insert(transitive.begin(), transitive.end());
      };

  addToWorkList(nullptr, TypeVar);

  do {
    auto *currentVar = workList.back().second;

    auto &node = CS.getConstraintGraph()[currentVar];
    if (!node.hasBindingSet()) {
      workList.pop_back();
      continue;
    }

    auto &bindings = node.getBindingSet();

    // If current variable already has transitive protocol
    // conformances inferred, there is no need to look deeper
    // into subtype/equivalence chain.
    if (bindings.TransitiveProtocols) {
      TypeVariableType *parent = nullptr;
      std::tie(parent, currentVar) = workList.pop_back_val();
      assert(parent);
      propagateProtocolsTo(parent, bindings.getConformanceRequirements(),
                           *bindings.TransitiveProtocols);
      continue;
    }

    for (const auto &entry : bindings.Info.SubtypeOf)
      addToWorkList(currentVar, entry.first);

    // If current type variable is part of an equivalence
    // class, make it a "representative" and let it infer
    // supertypes and direct protocol requirements from
    // other members and their equivalence classes.
    llvm::SmallSetVector<TypeVariableType *, 4> equivalenceClass;
    {
      SmallVector<TypeVariableType *, 4> workList;
      workList.push_back(currentVar);

      do {
        auto *typeVar = workList.pop_back_val();

        if (!equivalenceClass.insert(typeVar))
          continue;

        auto &node = CS.getConstraintGraph()[typeVar];
        if (!node.hasBindingSet())
          continue;

        auto &equivalences = node.getBindingSet().Info.EquivalentTo;
        for (const auto &eqVar : equivalences) {
          workList.push_back(eqVar.first);
        }
      } while (!workList.empty());
    }

    for (const auto &memberVar : equivalenceClass) {
      if (memberVar == currentVar)
        continue;

      auto &node = CS.getConstraintGraph()[memberVar];
      if (!node.hasBindingSet())
        continue;

      const auto &bindings = node.getBindingSet();

      llvm::SmallPtrSet<Constraint *, 2> placeholder;
      // Add any direct protocols from members of the
      // equivalence class, so they could be propagated
      // to all of the members.
      propagateProtocolsTo(currentVar, bindings.getConformanceRequirements(),
                           placeholder);

      // Since type variables are equal, current type variable
      // becomes a subtype to any supertype found in the current
      // equivalence  class.
      for (const auto &eqEntry : bindings.Info.SubtypeOf)
        addToWorkList(currentVar, eqEntry.first);
    }

    // More subtype/equivalences relations have been added.
    if (workList.back().second != currentVar)
      continue;

    TypeVariableType *parent = nullptr;
    std::tie(parent, currentVar) = workList.pop_back_val();

    // At all of the protocols associated with current type variable
    // are transitive to its parent, propagate them down the subtype/equivalence
    // chain.
    if (parent) {
      propagateProtocolsTo(parent, bindings.getConformanceRequirements(),
                           protocols[currentVar]);
    }

    auto &inferredProtocols = protocols[currentVar];

    llvm::SmallPtrSet<Constraint *, 4> protocolsForEquivalence;

    // Equivalence class should contain both:
    // - direct protocol requirements of the current type
    //   variable;
    // - all of the transitive protocols inferred through
    //   the members of the equivalence class.
    {
      auto directRequirements = bindings.getConformanceRequirements();
      protocolsForEquivalence.insert(directRequirements.begin(),
                                     directRequirements.end());

      protocolsForEquivalence.insert(inferredProtocols.begin(),
                                     inferredProtocols.end());
    }

    // Propagate inferred protocols to all of the members of the
    // equivalence class.
    for (const auto &equivalence : bindings.Info.EquivalentTo) {
      auto &node = CS.getConstraintGraph()[equivalence.first];
      if (node.hasBindingSet()) {
        auto &bindings = node.getBindingSet();
        bindings.TransitiveProtocols.emplace(protocolsForEquivalence.begin(),
                                             protocolsForEquivalence.end());
      }
    }

    // Update the bindings associated with current type variable,
    // to avoid repeating this inference process.
    bindings.TransitiveProtocols.emplace(inferredProtocols.begin(),
                                         inferredProtocols.end());
  } while (!workList.empty());
}

void BindingSet::inferTransitiveBindings() {
  using BindingKind = AllowedBindingKind;

  // If the current type variable represents a key path root type
  // let's try to transitively infer its type through bindings of
  // a key path type.
  if (TypeVar->getImpl().isKeyPathRoot()) {
    auto *locator = TypeVar->getImpl().getLocator();
    if (auto *keyPathTy =
            CS.getType(locator->getAnchor())->getAs<TypeVariableType>()) {
      auto &node = CS.getConstraintGraph()[keyPathTy];
      if (node.hasBindingSet()) {
        auto &bindings = node.getBindingSet();

        for (auto &binding : bindings.Bindings) {
          auto bindingTy = binding.BindingType->lookThroughAllOptionalTypes();

          Type inferredRootTy;
          if (isKnownKeyPathType(bindingTy)) {
            // AnyKeyPath doesn't have a root type.
            if (bindingTy->isAnyKeyPath())
              continue;

            auto *BGT = bindingTy->castTo<BoundGenericType>();
            inferredRootTy = BGT->getGenericArgs()[0];
          } else if (auto *fnType = bindingTy->getAs<FunctionType>()) {
            if (fnType->getNumParams() == 1)
              inferredRootTy = fnType->getParams()[0].getParameterType();
          }

          if (inferredRootTy) {
            // If contextual root is not yet resolved, let's try to see if
            // there are any bindings in its set. The bindings could be
            // transitively used because conversions between generic arguments
            // are not allowed.
            if (auto *contextualRootVar = inferredRootTy->getAs<TypeVariableType>()) {
              auto &node = CS.getConstraintGraph()[contextualRootVar];
              if (node.hasBindingSet()) {
                auto &bindings = node.getBindingSet();

                // Don't infer if root is not yet fully resolved.
                if (bindings.isDelayed())
                  continue;

                // Copy the bindings over to the root.
                for (const auto &binding : bindings.Bindings)
                  addBinding(binding, /*isTransitive=*/true);

                // Make a note that the key path root is transitively adjacent
                // to contextual root type variable and all of its variables.
                // This is important for ranking.
                AdjacentVars.insert(contextualRootVar);
                AdjacentVars.insert(bindings.AdjacentVars.begin(),
                                    bindings.AdjacentVars.end());
              }
            } else {
              addBinding(
                  binding.withSameSource(inferredRootTy, BindingKind::Exact),
                  /*isTransitive=*/true);
            }
          }
        }
      }
    }
  }

  for (const auto &entry : Info.SupertypeOf) {
    auto &node = CS.getConstraintGraph()[entry.first];
    if (!node.hasBindingSet())
      continue;

    auto &bindings = node.getBindingSet();

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
    for (const auto &literal : bindings.Literals)
      addLiteralRequirement(literal.second.getSource());

    // Infer transitive defaults.
    for (const auto &def : bindings.Defaults) {
      if (def.getSecond()->getKind() == ConstraintKind::FallbackType)
        continue;

      addDefault(def.second);
    }

    // TODO: We shouldn't need this in the future.
    if (entry.second->getKind() != ConstraintKind::Subtype)
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

      if (type->isPlaceholder())
        continue;

      if (ConstraintSystem::typeVarOccursInType(TypeVar, type))
        continue;

      addBinding(binding.withSameSource(type, BindingKind::Supertypes),
                 /*isTransitive=*/true);
    }
  }
}

static Type getKeyPathType(ASTContext &ctx, KeyPathCapability capability,
                           Type rootType, Type valueType) {
  KeyPathMutability mutability;
  bool isSendable;

  std::tie(mutability, isSendable) = capability;

  Type keyPathTy;
  switch (mutability) {
  case KeyPathMutability::ReadOnly:
    keyPathTy = BoundGenericType::get(ctx.getKeyPathDecl(), /*parent=*/Type(),
                                      {rootType, valueType});
    break;

  case KeyPathMutability::Writable:
    keyPathTy = BoundGenericType::get(ctx.getWritableKeyPathDecl(),
                                      /*parent=*/Type(), {rootType, valueType});
    break;

  case KeyPathMutability::ReferenceWritable:
    keyPathTy = BoundGenericType::get(ctx.getReferenceWritableKeyPathDecl(),
                                      /*parent=*/Type(), {rootType, valueType});
    break;
  }

  if (isSendable &&
      ctx.LangOpts.hasFeature(Feature::InferSendableFromCaptures)) {
    auto *sendable = ctx.getProtocol(KnownProtocolKind::Sendable);
    keyPathTy = ProtocolCompositionType::get(
        ctx, {keyPathTy, sendable->getDeclaredInterfaceType()},
        /*inverses=*/{}, /*hasExplicitAnyObject=*/false);
    return ExistentialType::get(keyPathTy);
  }

  return keyPathTy;
}

bool BindingSet::finalize(bool transitive) {
  if (transitive)
    inferTransitiveBindings();

  determineLiteralCoverage();

  if (auto *locator = TypeVar->getImpl().getLocator()) {
    if (locator->isLastElement<LocatorPathElt::MemberRefBase>()) {
      // If this is a base of an unresolved member chain, as a last
      // resort effort let's infer base to be a protocol type based
      // on contextual conformance requirements.
      //
      // This allows us to find solutions in cases like this:
      //
      // \code
      // func foo<T: P>(_: T) {}
      // foo(.bar) <- `.bar` should be a static member of `P`.
      // \endcode
      if (transitive && !hasViableBindings()) {
        inferTransitiveProtocolRequirements();

        if (TransitiveProtocols.has_value()) {
          for (auto *constraint : *TransitiveProtocols) {
            Type protocolTy = constraint->getSecondType();

            // Compiler-known marker protocols cannot be extended with members,
            // so do not consider them.
            if (auto p = protocolTy->getAs<ProtocolType>()) {
              if (ProtocolDecl *decl = p->getDecl())
                if (decl->getKnownProtocolKind() && decl->isMarkerProtocol())
                  continue;
            }

            addBinding({protocolTy, AllowedBindingKind::Exact, constraint},
                       /*isTransitive=*/false);
          }
        }
      }
    }

    if (TypeVar->getImpl().isKeyPathType()) {
      auto &ctx = CS.getASTContext();

      auto *keyPathLoc = TypeVar->getImpl().getLocator();
      auto *keyPath = castToExpr<KeyPathExpr>(keyPathLoc->getAnchor());

      bool isValid;
      std::optional<KeyPathCapability> capability;

      std::tie(isValid, capability) = CS.inferKeyPathLiteralCapability(TypeVar);

      // Key path literal is not yet sufficiently resolved, this binding
      // set is not viable.
      if (isValid && !capability)
        return false;

      // If the key path is sufficiently resolved we can add inferred binding
      // to the set.
      SmallSetVector<PotentialBinding, 4> updatedBindings;
      for (const auto &binding : Bindings) {
        auto bindingTy = binding.BindingType->lookThroughAllOptionalTypes();

        assert(isKnownKeyPathType(bindingTy) || bindingTy->is<FunctionType>());

        // Functions don't have capability so we can simply add them.
        if (auto *fnType = bindingTy->getAs<FunctionType>()) {
          auto extInfo = fnType->getExtInfo();

          bool isKeyPathSendable = capability && capability->second;
          if (!isKeyPathSendable && extInfo.isSendable()) {
            fnType = FunctionType::get(fnType->getParams(), fnType->getResult(),
                                       extInfo.withSendable(false));
          }

          updatedBindings.insert(binding.withType(fnType));
        }
      }

      // Note that even though key path literal maybe be invalid it's
      // still the best course of action to use contextual function type
      // bindings because they allow to propagate type information from
      // the key path into the context, so key path bindings are addded
      // only if there is absolutely no other choice.
      if (updatedBindings.empty()) {
        auto rootTy = CS.getKeyPathRootType(keyPath);

        // A valid key path literal.
        if (capability) {
          // Note that the binding is formed using root & value
          // type variables produced during constraint generation
          // because at this point root is already known (otherwise
          // inference wouldn't been able to determine key path's
          // capability) and we always want to infer value from
          // the key path and match it to a contextual type to produce
          // better diagnostics.
          auto keyPathTy = getKeyPathType(ctx, *capability, rootTy,
                                          CS.getKeyPathValueType(keyPath));
          updatedBindings.insert(
              {keyPathTy, AllowedBindingKind::Exact, keyPathLoc});
        } else if (CS.shouldAttemptFixes()) {
          auto fixedRootTy = CS.getFixedType(rootTy);
          // If key path is structurally correct and has a resolved root
          // type, let's promote the fallback type into a binding because
          // root would have been inferred from explicit type already and
          // it's benefitial for diagnostics to assign a non-placeholder
          // type to key path literal to propagate root/value to the context.
          if (!keyPath->hasSingleInvalidComponent() &&
              (keyPath->getParsedRoot() ||
               (fixedRootTy && !fixedRootTy->isTypeVariableOrMember()))) {
            auto fallback = llvm::find_if(Defaults, [](const auto &entry) {
              return entry.second->getKind() == ConstraintKind::FallbackType;
            });
            assert(fallback != Defaults.end());
            updatedBindings.insert(
                {fallback->first, AllowedBindingKind::Exact, fallback->second});
          } else {
            updatedBindings.insert(PotentialBinding::forHole(
                TypeVar, CS.getConstraintLocator(
                             keyPath, ConstraintLocator::FallbackType)));
          }
        }
      }

      Bindings = std::move(updatedBindings);
      Defaults.clear();

      return true;
    }

    if (CS.shouldAttemptFixes() &&
        locator->isLastElement<LocatorPathElt::UnresolvedMemberChainResult>()) {
      // Let's see whether this chain is valid, if it isn't then to avoid
      // diagnosing the same issue multiple different ways, let's infer
      // result of the chain to be a hole.
      auto *resultExpr =
          castToExpr<UnresolvedMemberChainResultExpr>(locator->getAnchor());
      auto *baseLocator = CS.getConstraintLocator(
          resultExpr->getChainBase(), ConstraintLocator::UnresolvedMember);

      if (CS.hasFixFor(
              baseLocator,
              FixKind::AllowInvalidStaticMemberRefOnProtocolMetatype)) {
        CS.recordPotentialHole(TypeVar);
        // Clear all of the previously collected bindings which are inferred
        // from inside of a member chain.
        Bindings.remove_if([](const PotentialBinding &binding) {
          return binding.Kind == AllowedBindingKind::Supertypes;
        });
      }
    }
  }

  return true;
}

void BindingSet::addBinding(PotentialBinding binding, bool isTransitive) {
  if (Bindings.count(binding))
    return;

  if (!isViable(binding, isTransitive))
    return;

  SmallPtrSet<TypeVariableType *, 4> referencedTypeVars;
  binding.BindingType->getTypeVariables(referencedTypeVars);

  // If type variable is not allowed to bind to `lvalue`,
  // let's check if type of potential binding has any
  // type variables, which are allowed to bind to `lvalue`,
  // and postpone such type from consideration.
  //
  // This check is done here and not in `checkTypeOfBinding`
  // because the l-valueness of the variable might change during
  // solving and that would not be reflected in the graph.
  if (!TypeVar->getImpl().canBindToLValue()) {
    for (auto *typeVar : referencedTypeVars) {
      if (typeVar->getImpl().canBindToLValue())
        return;
    }
  }

  // Since Double and CGFloat are effectively the same type due to an
  // implicit conversion between them, always prefer Double over CGFloat
  // when possible.
  //
  // Note: This optimization can't be performed for closure parameters
  //       because their type could be converted only at the point of
  //       use in the closure body.
  if (!TypeVar->getImpl().isClosureParameterType()) {
    auto type = binding.BindingType;

    if (type->isCGFloat() &&
        llvm::any_of(Bindings, [](const PotentialBinding &binding) {
          return binding.BindingType->isDouble();
        }))
      return;

    if (type->isDouble()) {
      auto inferredCGFloat =
          llvm::find_if(Bindings, [](const PotentialBinding &binding) {
            return binding.BindingType->isCGFloat();
          });

      if (inferredCGFloat != Bindings.end()) {
        Bindings.erase(inferredCGFloat);
        Bindings.insert(inferredCGFloat->withType(type));
        return;
      }
    }
  }

  // If this is a non-defaulted supertype binding,
  // check whether we can combine it with another
  // supertype binding by computing the 'join' of the types.
  if (binding.isViableForJoin()) {
    auto isAcceptableJoin = [](Type type) {
      return !type->isAny() && (!type->getOptionalObjectType() ||
                                !type->getOptionalObjectType()->isAny());
    };

    SmallVector<PotentialBinding, 4> joined;
    for (auto existingBinding = Bindings.begin();
         existingBinding != Bindings.end();) {
      if (existingBinding->isViableForJoin()) {
        auto join =
            Type::join(existingBinding->BindingType, binding.BindingType);

        if (join && isAcceptableJoin(*join)) {
          // Result of the join has to use new binding because it refers
          // to the constraint that triggered the join that replaced the
          // existing binding.
          joined.push_back(binding.withType(*join));
          // Remove existing binding from the set.
          // It has to be re-introduced later, since its type has been changed.
          existingBinding = Bindings.erase(existingBinding);
          continue;
        }
      }

      ++existingBinding;
    }

    for (const auto &binding : joined)
      (void)Bindings.insert(binding);

    // If new binding has been joined with at least one of existing
    // bindings, there is no reason to include it into the set.
    if (!joined.empty())
      return;
  }

  for (auto *adjacentVar : referencedTypeVars)
    AdjacentVars.insert(adjacentVar);

  (void)Bindings.insert(std::move(binding));
}

void BindingSet::determineLiteralCoverage() {
  if (Literals.empty())
    return;

  bool allowsNil = canBeNil();

  for (auto &entry : Literals) {
    auto &literal = entry.second;

    if (!literal.viableAsBinding())
      continue;

    for (auto binding = Bindings.begin(); binding != Bindings.end();
         ++binding) {
      bool isCovered = false;
      Type adjustedTy;

      std::tie(isCovered, adjustedTy) =
          literal.isCoveredBy(*binding, allowsNil, CS);

      if (!isCovered)
        continue;

      literal.setCoveredBy(binding->getSource());

      if (adjustedTy) {
        Bindings.erase(binding);
        Bindings.insert(binding->withType(adjustedTy));
      }

      break;
    }
  }
}

void BindingSet::addLiteralRequirement(Constraint *constraint) {
  auto isDirectRequirement = [&](Constraint *constraint) -> bool {
    if (auto *typeVar = constraint->getFirstType()->getAs<TypeVariableType>()) {
      auto *repr = CS.getRepresentative(typeVar);
      return repr == TypeVar;
    }

    return false;
  };

  auto *protocol = constraint->getProtocol();

  // Let's try to coalesce integer and floating point literal protocols
  // if they appear together because the only possible default type that
  // could satisfy both requirements is `Double`.
  {
    if (protocol->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByIntegerLiteral)) {
      auto *floatLiteral = CS.getASTContext().getProtocol(
          KnownProtocolKind::ExpressibleByFloatLiteral);
      if (Literals.count(floatLiteral))
        return;
    }

    if (protocol->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByFloatLiteral)) {
      auto *intLiteral = CS.getASTContext().getProtocol(
          KnownProtocolKind::ExpressibleByIntegerLiteral);
      Literals.erase(intLiteral);
    }
  }

  if (Literals.count(protocol) > 0)
    return;

  bool isDirect = isDirectRequirement(constraint);

  // Coverage is not applicable to `ExpressibleByNilLiteral` since it
  // doesn't have a default type.
  if (protocol->isSpecificProtocol(
          KnownProtocolKind::ExpressibleByNilLiteral)) {
    Literals.insert(
        {protocol, LiteralRequirement(constraint,
                                      /*DefaultType=*/Type(), isDirect)});
    return;
  }

  // Check whether any of the existing bindings covers this literal
  // protocol.
  LiteralRequirement literal(
      constraint, TypeChecker::getDefaultType(protocol, CS.DC), isDirect);

  Literals.insert({protocol, std::move(literal)});
}

bool BindingSet::operator==(const BindingSet &other) {
  if (AdjacentVars != other.AdjacentVars)
    return false;

  if (Bindings.size() != other.Bindings.size())
    return false;

  for (auto i : indices(Bindings)) {
    const auto &x = Bindings[i];
    const auto &y = other.Bindings[i];

    if (x.BindingType.getPointer() != y.BindingType.getPointer() ||
        x.Kind != y.Kind)
      return false;
  }

  if (Literals.size() != other.Literals.size())
    return false;

  for (auto pair : Literals) {
    auto found = other.Literals.find(pair.first);
    if (found == other.Literals.end())
      return false;

    const auto &x = pair.second;
    const auto &y = found->second;

    if (x.Source != y.Source ||
        x.DefaultType.getPointer() != y.DefaultType.getPointer() ||
        x.IsDirectRequirement != y.IsDirectRequirement) {
      return false;
    }
  }

  if (Defaults.size() != other.Defaults.size())
    return false;

  for (auto pair : Defaults) {
    auto found = other.Defaults.find(pair.first);
    if (found == other.Defaults.end() ||
        pair.second != found->second)
      return false;
  }

  if (TransitiveProtocols != other.TransitiveProtocols)
    return false;

  return true;
}

BindingSet::BindingScore BindingSet::formBindingScore(const BindingSet &b) {
  // If there are no bindings available but this type
  // variable represents a closure - let's consider it
  // as having a single non-default binding - that would
  // be a type inferred based on context.
  // It's considered to be non-default for purposes of
  // ranking because we'd like to prioritize resolving
  // closures to gain more information from their bodies.
  unsigned numBindings = b.Bindings.size() + b.getNumViableLiteralBindings();
  auto numNonDefaultableBindings = numBindings > 0 ? numBindings
                                   : b.TypeVar->getImpl().isClosureType() ? 1
                                                                          : 0;

  return std::make_tuple(b.isHole(), numNonDefaultableBindings == 0,
                         b.isDelayed(), b.isSubtypeOfExistentialType(),
                         b.involvesTypeVariables(),
                         static_cast<unsigned char>(b.getLiteralForScore()),
                         -numNonDefaultableBindings);
}

bool BindingSet::operator<(const BindingSet &other) {
  auto xScore = formBindingScore(*this);
  auto yScore = formBindingScore(other);

  if (xScore < yScore)
    return true;

  if (yScore < xScore)
    return false;

  auto xDefaults = getNumViableDefaultableBindings();
  auto yDefaults = other.getNumViableDefaultableBindings();

  // If there is a difference in number of default types,
  // prioritize bindings with fewer of them.
  if (xDefaults != yDefaults)
    return xDefaults < yDefaults;

  // If neither type variable is a "hole" let's check whether
  // there is a subtype relationship between them and prefer
  // type variable which represents superclass first in order
  // for "subtype" type variable to attempt more bindings later.
  // This is required because algorithm can't currently infer
  // bindings for subtype transitively through superclass ones.
  if (!(std::get<0>(xScore) && std::get<0>(yScore))) {
    if (Info.isSubtypeOf(other.getTypeVariable()))
      return false;

    if (other.Info.isSubtypeOf(getTypeVariable()))
      return true;
  }

  // As a last resort, let's check if the bindings are
  // potentially incomplete, and if so, let's de-prioritize them.
  return isPotentiallyIncomplete() < other.isPotentiallyIncomplete();
}

std::optional<BindingSet> ConstraintSystem::determineBestBindings(
    llvm::function_ref<void(const BindingSet &)> onCandidate) {
  // Look for potential type variable bindings.
  BindingSet *bestBindings = nullptr;

  // First, let's collect all of the possible bindings.
  for (auto *typeVar : getTypeVariables()) {
    auto &node = CG[typeVar];
    node.resetBindingSet();
    if (!typeVar->getImpl().hasRepresentativeOrFixed())
      node.initBindingSet();
  }

  // Determine whether given type variable with its set of bindings is
  // viable to be attempted on the next step of the solver. If type variable
  // has no "direct" bindings of any kind e.g. direct bindings to concrete
  // types, default types from "defaultable" constraints or literal
  // conformances, such type variable is not viable to be evaluated to be
  // attempted next.
  auto isViableForRanking = [this](const BindingSet &bindings) -> bool {
    auto *typeVar = bindings.getTypeVariable();

    // Key path root type variable is always viable because it can be
    // transitively inferred from key path type during binding set
    // finalization.
    if (typeVar->getImpl().isKeyPathRoot())
      return true;

    // Type variable representing a base of unresolved member chain should
    // always be considered viable for ranking since it's allow to infer
    // types from transitive protocol requirements.
    if (auto *locator = typeVar->getImpl().getLocator()) {
      if (locator->isLastElement<LocatorPathElt::MemberRefBase>())
        return true;
    }

    // If type variable is marked as a potential hole there is always going
    // to be at least one binding available for it.
    if (shouldAttemptFixes() && typeVar->getImpl().canBindToHole())
      return true;

    return bool(bindings);
  };

  // Now let's see if we could infer something for related type
  // variables based on other bindings.
  for (auto *typeVar : getTypeVariables()) {
    auto &node = CG[typeVar];
    if (!node.hasBindingSet())
      continue;

    auto &bindings = node.getBindingSet();

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

    if (!bindings.finalize(true))
      continue;

    if (!bindings || !isViable)
      continue;

    onCandidate(bindings);

    // If these are the first bindings, or they are better than what
    // we saw before, use them instead.
    if (!bestBindings || bindings < *bestBindings)
      bestBindings = &bindings;
  }

  if (!bestBindings)
    return std::nullopt;

  return std::optional(*bestBindings);
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
        return Action::SkipNode;

      if (auto typeVar = ty->getAs<TypeVariableType>())
        typeVars.insert(typeVar);
      return Action::Continue;
    }
  };

  type.walk(Walker(typeVars));
}

void BindingSet::addDefault(Constraint *constraint) {
  auto defaultTy = constraint->getSecondType();
  Defaults.insert({defaultTy->getCanonicalType(), constraint});
}

bool LiteralRequirement::isCoveredBy(Type type, ConstraintSystem &CS) const {
  auto coversDefaultType = [](Type type, Type defaultType) -> bool {
    if (!defaultType->hasUnboundGenericType())
      return type->isEqual(defaultType);

    // For generic literal types, check whether we already have a
    // specialization of this generic within our list.
    // FIXME: This assumes that, e.g., the default literal
    // int/float/char/string types are never generic.
    auto nominal = defaultType->getAnyNominal();
    if (!nominal)
      return false;

    // FIXME: Check parents?
    return nominal == type->getAnyNominal();
  };

  if (hasDefaultType() && coversDefaultType(type, getDefaultType()))
    return true;

  return bool(CS.lookupConformance(type, getProtocol()));
}

std::pair<bool, Type>
LiteralRequirement::isCoveredBy(const PotentialBinding &binding, bool canBeNil,
                                ConstraintSystem &CS) const {
  auto type = binding.BindingType;
  switch (binding.Kind) {
  case AllowedBindingKind::Exact:
    type = binding.BindingType;
    break;

  case AllowedBindingKind::Subtypes:
  case AllowedBindingKind::Supertypes:
    type = binding.BindingType->getRValueType();
    break;
  }

  bool requiresUnwrap = false;
  do {
    // Conformance check on type variable would always return true,
    // but type variable can't cover anything until it's bound.
    if (type->isTypeVariableOrMember() || type->isPlaceholder())
      return std::make_pair(false, Type());

    if (isCoveredBy(type, CS)) {
      return std::make_pair(true, requiresUnwrap ? type : binding.BindingType);
    }

    // Can't unwrap optionals if there is `ExpressibleByNilLiteral`
    // conformance requirement placed on the type variable.
    if (canBeNil)
      return std::make_pair(false, Type());

    // If this literal protocol is not a direct requirement it
    // would not be possible to change optionality while inferring
    // bindings for a supertype, so this hack doesn't apply.
    if (!isDirectRequirement())
      return std::make_pair(false, Type());

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

    return std::make_pair(false, Type());
  } while (true);
}

void PotentialBindings::addPotentialBinding(TypeVariableType *TypeVar,
                                            PotentialBinding binding) {
  assert(!binding.BindingType->is<ErrorType>());

  // If the type variable can't bind to an lvalue, make sure the
  // type we pick isn't an lvalue.
  if (!TypeVar->getImpl().canBindToLValue() &&
      binding.BindingType->hasLValueType()) {
    binding = binding.withType(binding.BindingType->getRValueType());
  }

  Bindings.push_back(std::move(binding));
}

bool BindingSet::isViable(PotentialBinding &binding, bool isTransitive) {
  // Prevent against checking against the same opened nominal type
  // over and over again. Doing so means redundant work in the best
  // case. In the worst case, we'll produce lots of duplicate solutions
  // for this constraint system, which is problematic for overload
  // resolution.
  auto type = binding.BindingType;

  if (isTransitive && !checkTypeOfBinding(TypeVar, type))
    return false;

  auto *NTD = type->getAnyNominal();
  if (!NTD)
    return true;

  for (auto existing = Bindings.begin(); existing != Bindings.end();
       ++existing) {
    auto existingType = existing->BindingType;

    auto *existingNTD = existingType->getAnyNominal();
    if (!existingNTD || NTD != existingNTD)
      continue;

    // FIXME: What is going on here needs to be thoroughly re-evaluated.

    // If new type has a type variable it shouldn't
    // be considered  viable.
    if (type->hasTypeVariable())
      return false;

    // If new type doesn't have any type variables,
    // but existing binding does, let's replace existing
    // binding with new one.
    if (existingType->hasTypeVariable()) {
      // First, let's remove all of the adjacent type
      // variables associated with this binding.
      {
        SmallPtrSet<TypeVariableType *, 4> referencedVars;
        existingType->getTypeVariables(referencedVars);
        for (auto *var : referencedVars)
          AdjacentVars.erase(var);
      }

      // And now let's remove the binding itself.
      Bindings.erase(existing);
      break;
    }
  }

  return true;
}

static bool hasConversions(Type type) {
  if (type->isAnyHashable() || type->isDouble() || type->isCGFloat())
    return true;

  if (type->getAnyPointerElementType())
    return true;

  if (auto *structTy = type->getAs<BoundGenericStructType>()) {
    if (auto eltTy = structTy->getArrayElementType()) {
      return hasConversions(eltTy);
    } else if (auto pair = ConstraintSystem::isDictionaryType(structTy)) {
      return hasConversions(pair->second);
    } else if (auto eltTy = ConstraintSystem::isSetType(structTy)) {
      return hasConversions(*eltTy);
    }

    return false;
  }

  if (auto *enumTy = type->getAs<BoundGenericEnumType>()) {
    if (enumTy->getOptionalObjectType())
      return true;

    return false;
  }

  return !(type->is<StructType>() || type->is<EnumType>() ||
           type->is<BuiltinType>() || type->is<ArchetypeType>());
}

bool BindingSet::favoredOverDisjunction(Constraint *disjunction) const {
  if (isHole())
    return false;

  if (llvm::any_of(Bindings, [&](const PotentialBinding &binding) {
        if (binding.Kind == AllowedBindingKind::Supertypes)
          return false;

        if (CS.shouldAttemptFixes())
          return false;

        return !hasConversions(binding.BindingType);
      })) {
    // Result type of subscript could be l-value so we can't bind it early.
    if (!TypeVar->getImpl().isSubscriptResultType() &&
        llvm::none_of(Info.DelayedBy, [](const Constraint *constraint) {
          return constraint->getKind() == ConstraintKind::Disjunction ||
                 constraint->getKind() == ConstraintKind::ValueMember;
        }))
      return true;
  }

  if (isDelayed())
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

  // If this is a collection literal type, it's preferrable to bind it
  // early (unless it's delayed) to connect all of its elements even
  // if it doesn't have any bindings.
  if (TypeVar->getImpl().isCollectionLiteralType())
    return !involvesTypeVariables();

  // Don't prioritize type variables that don't have any direct bindings.
  if (Bindings.empty())
    return false;

  // Always prefer key path type if it has bindings and is not delayed
  // because that means that it was possible to infer its capability.
  if (TypeVar->getImpl().isKeyPathType())
    return true;

  return !involvesTypeVariables();
}

bool BindingSet::favoredOverConjunction(Constraint *conjunction) const {
  if (CS.shouldAttemptFixes() && isHole()) {
    if (forClosureResult() || forGenericParameter())
      return false;
  }

  auto *locator = conjunction->getLocator();
  if (locator->directlyAt<ClosureExpr>()) {
    auto *closure = castToExpr<ClosureExpr>(locator->getAnchor());

    if (auto transform = CS.getAppliedResultBuilderTransform(closure)) {
      // Conjunctions that represent closures with result builder transformed
      // bodies could be attempted right after their resolution if they meet
      // all of the following criteria:
      //
      // - Builder type doesn't have any unresolved generic parameters;
      // - Closure doesn't have any parameters;
      // - The contextual result type is either concrete or opaque type.
      auto contextualType = transform->contextualType;
      if (!(contextualType && contextualType->is<FunctionType>()))
        return true;

      auto *contextualFnType =
          CS.simplifyType(contextualType)->castTo<FunctionType>();
      {
        auto resultType = contextualFnType->getResult();
        if (resultType->hasTypeVariable()) {
          auto *typeVar = resultType->getAs<TypeVariableType>();
          // If contextual result type is represented by an opaque type,
          // it's a strong indication that body is self-contained, otherwise
          // closure might rely on external types flowing into the body for
          // disambiguation of `build{Partial}Block` or `buildFinalResult`
          // calls.
          if (!(typeVar && typeVar->getImpl().isOpaqueType()))
            return true;
        }
      }

      // If some of the closure parameters are unresolved, the conjunction
      // has to be delayed to give them a chance to be inferred.
      if (llvm::any_of(contextualFnType->getParams(), [](const auto &param) {
            return param.getPlainType()->hasTypeVariable();
          }))
        return true;

      // Check whether conjunction has any unresolved type variables
      // besides the variable that represents the closure.
      //
      // Conjunction could refer to declarations from outer context
      // (i.e. a variable declared in the outer closure) or generic
      // parameters of the builder type), if any of such references
      // are not yet inferred the conjunction has to be delayed.
      auto *closureType = CS.getType(closure)->castTo<TypeVariableType>();
      return llvm::any_of(
          conjunction->getTypeVariables(), [&](TypeVariableType *typeVar) {
            return !(typeVar == closureType || CS.getFixedType(typeVar));
          });
    }
  }

  // If key path capability is not yet determined it cannot be favored
  // over a conjunction because:
  // 1. There could be no other bindings and that would mean that
  //    key path would be selected even though it's not yet ready.
  // 2. A conjunction could be the source of type context for the key path.
  if (TypeVar->getImpl().isKeyPathType() && isDelayed())
    return false;

  return true;
}

BindingSet ConstraintSystem::getBindingsFor(TypeVariableType *typeVar) {
  assert(typeVar->getImpl().getRepresentative(nullptr) == typeVar &&
         "not a representative");
  assert(!typeVar->getImpl().getFixedType(nullptr) && "has a fixed type");

  BindingSet bindings(*this, typeVar, CG[typeVar].getPotentialBindings());
  bindings.finalize(false);

  return bindings;
}

/// Check whether the given type can be used as a binding for the given
/// type variable.
///
/// \returns the type to bind to, if the binding is okay.
static std::optional<Type> checkTypeOfBinding(TypeVariableType *typeVar,
                                              Type type) {
  // If the type references the type variable, don't permit the binding.
  if (type->hasTypeVariable()) {
    SmallPtrSet<TypeVariableType *, 4> referencedTypeVars;
    type->getTypeVariables(referencedTypeVars);
    if (referencedTypeVars.count(typeVar))
      return std::nullopt;
  }

  {
    auto objType = type->getWithoutSpecifierType();

    // If the type is a type variable itself, don't permit the binding.
    if (objType->is<TypeVariableType>())
      return std::nullopt;

    // Don't bind to a dependent member type, even if it's currently
    // wrapped in any number of optionals, because binding producer
    // might unwrap and try to attempt it directly later.
    if (objType->lookThroughAllOptionalTypes()->is<DependentMemberType>())
      return std::nullopt;
  }

  // Okay, allow the binding (with the simplified type).
  return type;
}

std::optional<PotentialBinding>
PotentialBindings::inferFromRelational(ConstraintSystem &CS,
                                       TypeVariableType *TypeVar,
                                       Constraint *constraint) {
  assert(constraint->getClassification() ==
             ConstraintClassification::Relational &&
         "only relational constraints handled here");

  auto first = CS.simplifyType(constraint->getFirstType());
  auto second = CS.simplifyType(constraint->getSecondType());

  if (first->is<TypeVariableType>() && first->isEqual(second))
    return std::nullopt;

  Type type;
  AllowedBindingKind kind;
  if (first->getAs<TypeVariableType>() == TypeVar) {
    // Upper bound for this type variable.
    type = second;
    kind = AllowedBindingKind::Subtypes;
  } else if (second->getAs<TypeVariableType>() == TypeVar) {
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
        DelayedBy.push_back(constraint);
        return std::nullopt;
      }
    }

    // Check whether both this type and another type variable are
    // inferable.
    SmallPtrSet<TypeVariableType *, 4> typeVars;
    findInferableTypeVars(first, typeVars);
    findInferableTypeVars(second, typeVars);

    if (typeVars.erase(TypeVar)) {
      for (auto *typeVar : typeVars) {
        AdjacentVars.emplace_back(typeVar, constraint);
      }
    }

    // Infer a binding from `inout $T <convertible to> Unsafe*Pointer<...>?`.
    if (first->is<InOutType>() &&
        first->getInOutObjectType()->isEqual(TypeVar)) {
      if (auto pointeeTy = second->lookThroughAllOptionalTypes()
                               ->getAnyPointerElementType()) {
        if (!pointeeTy->isTypeVariableOrMember()) {
          return PotentialBinding(pointeeTy, AllowedBindingKind::Exact,
                                  constraint);
        }
      }
    }

    return std::nullopt;
  }

  // Do not attempt to bind to ErrorType.
  if (type->hasError())
    return std::nullopt;

  if (TypeVar->getImpl().isKeyPathType()) {
    auto objectTy = type->lookThroughAllOptionalTypes();

    // If contextual type is an existential with a superclass
    // constraint, let's try to infer a key path type from it.
    if (kind == AllowedBindingKind::Subtypes) {
      if (type->isExistentialType()) {
        auto layout = type->getExistentialLayout();
        if (auto superclass = layout.explicitSuperclass) {
          if (isKnownKeyPathType(superclass)) {
            type = superclass;
            objectTy = superclass;
          }
        }
      }
    }

    if (!(isKnownKeyPathType(objectTy) || objectTy->is<AnyFunctionType>()))
      return std::nullopt;
  }

  if (TypeVar->getImpl().isKeyPathSubscriptIndex()) {
    // Key path subscript index can only be a r-value non-optional
    // type that is a subtype of a known KeyPath type.
    type = type->getRValueType()->lookThroughAllOptionalTypes();

    // If argument to a key path subscript is an existential,
    // we can erase it to superclass (if any) here and solver
    // will perform the opening if supertype turns out to be
    // a valid key path type of its subtype.
    if (kind == AllowedBindingKind::Supertypes) {
      if (type->isExistentialType()) {
        auto layout = type->getExistentialLayout();
        if (auto superclass = layout.explicitSuperclass) {
          type = superclass;
        } else if (!CS.shouldAttemptFixes()) {
          return std::nullopt;
        }
      }
    }
  }

  // Situations like `v.<member> = { ... }` where member is overloaded.
  // We need to wait until member is resolved otherwise there is a risk
  // of losing some of the contextual attributes important for the closure
  // such as @Sendable and global actor.
  if (TypeVar->getImpl().isClosureType() &&
      kind == AllowedBindingKind::Subtypes) {
    if (type->isTypeVariableOrMember() &&
        constraint->getLocator()->directlyAt<AssignExpr>()) {
      DelayedBy.push_back(constraint);
    }
  }

  if (TypeVar->getImpl().getLocator()) {
    // Don't allow a protocol type to get propagated from the base to the result
    // type of a chain, Result should always be a concrete type which conforms
    // to the protocol inferred for the base.
    if (constraint->getKind() == ConstraintKind::UnresolvedMemberChainBase &&
        kind == AllowedBindingKind::Subtypes && type->is<ProtocolType>())
      return std::nullopt;
  }

  if (constraint->getKind() == ConstraintKind::LValueObject) {
    // Allow l-value type inference from its object type, but
    // not the other way around, that would be handled by constraint
    // simplification.
    if (kind == AllowedBindingKind::Subtypes) {
      if (type->isTypeVariableOrMember())
        return std::nullopt;

      type = LValueType::get(type);
    } else {
      // Right-hand side of the l-value object constraint can only
      // be bound via constraint simplification when l-value type
      // is inferred or contextually from other constraints.
      DelayedBy.push_back(constraint);
      return std::nullopt;
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
  if (type->getWithoutSpecifierType()
          ->lookThroughAllOptionalTypes()
          ->is<DependentMemberType>()) {
    llvm::SmallPtrSet<TypeVariableType *, 4> referencedVars;
    type->getTypeVariables(referencedVars);

    bool containsSelf = false;
    for (auto *var : referencedVars) {
      // Add all type variables encountered in the type except
      // to the current type variable.
      if (var != TypeVar) {
        AdjacentVars.emplace_back(var, constraint);
        continue;
      }

      containsSelf = true;
    }

    // If inferred type doesn't contain the current type variable,
    // let's mark bindings as delayed until dependent member type
    // is resolved.
    if (!containsSelf)
      DelayedBy.push_back(constraint);

    return std::nullopt;
  }

  // If our binding choice is a function type and we're attempting
  // to bind to a type variable that is the result of opening a
  // generic parameter, strip the noescape bit so that we only allow
  // bindings of escaping functions in this position. We do this
  // because within the generic function we have no indication of
  // whether the parameter is a function type and if so whether it
  // should be allowed to escape. As a result we allow anything
  // passed in to escape.
  if (auto *fnTy = type->getAs<AnyFunctionType>()) {
    // Since inference now happens during constraint generation,
    // this hack should be allowed in both `Solving`
    // (during non-diagnostic mode) and `ConstraintGeneration` phases.
    if (isGenericParameter(TypeVar) &&
        (!CS.shouldAttemptFixes() ||
         CS.getPhase() == ConstraintSystemPhase::ConstraintGeneration)) {
      type = fnTy->withExtInfo(fnTy->getExtInfo().withNoEscape(false));
    }
  }

  // Check whether we can perform this binding.
  if (auto boundType = checkTypeOfBinding(TypeVar, type)) {
    type = *boundType;
  } else {
    auto *bindingTypeVar = type->getRValueType()->getAs<TypeVariableType>();

    if (!bindingTypeVar)
      return std::nullopt;

    // If current type variable is associated with a code completion token
    // it's possible that it doesn't have enough contextual information
    // to be resolved to anything, so let's note that fact in the potential
    // bindings and use it when forming a hole if there are no other bindings
    // available.
    if (auto *locator = bindingTypeVar->getImpl().getLocator()) {
      if (locator->directlyAt<CodeCompletionExpr>())
        AssociatedCodeCompletionToken = locator->getAnchor();
    }

    switch (constraint->getKind()) {
    case ConstraintKind::Subtype:
    case ConstraintKind::SubclassOf:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentConversion: {
      if (kind == AllowedBindingKind::Subtypes) {
        SubtypeOf.emplace_back(bindingTypeVar, constraint);
      } else {
        assert(kind == AllowedBindingKind::Supertypes);
        SupertypeOf.emplace_back(bindingTypeVar, constraint);
      }

      AdjacentVars.emplace_back(bindingTypeVar, constraint);
      break;
    }

    case ConstraintKind::Bind:
    case ConstraintKind::BindParam:
    case ConstraintKind::Equal: {
      EquivalentTo.emplace_back(bindingTypeVar, constraint);
      AdjacentVars.emplace_back(bindingTypeVar, constraint);
      break;
    }

    case ConstraintKind::UnresolvedMemberChainBase: {
      EquivalentTo.emplace_back(bindingTypeVar, constraint);

      // Don't record adjacency between base and result types,
      // this is just an auxiliary constraint to enforce ordering.
      break;
    }

    case ConstraintKind::OptionalObject: {
      // Type variable that represents an object type of
      // an un-inferred optional is adjacent to a type
      // variable that presents such optional (`bindingTypeVar`
      // in this case).
      if (kind == AllowedBindingKind::Supertypes) {
        AdjacentVars.emplace_back(bindingTypeVar, constraint);
      }
      break;
    }

    default:
      break;
    }

    return std::nullopt;
  }

  // Make sure we aren't trying to equate type variables with different
  // lvalue-binding rules.
  if (auto otherTypeVar = type->getAs<TypeVariableType>()) {
    if (TypeVar->getImpl().canBindToLValue() !=
        otherTypeVar->getImpl().canBindToLValue())
      return std::nullopt;
  }

  if (type->is<InOutType>() && !TypeVar->getImpl().canBindToInOut())
    type = LValueType::get(type->getInOutObjectType());
  if (type->is<LValueType>() && !TypeVar->getImpl().canBindToLValue())
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
void PotentialBindings::infer(ConstraintSystem &CS,
                              TypeVariableType *TypeVar,
                              Constraint *constraint) {
  if (!Constraints.insert(constraint))
    return;

  // Record the change, if there are active scopes.
  if (CS.solverState)
    CS.recordChange(SolverTrail::Change::InferredBindings(TypeVar, constraint));

  switch (constraint->getKind()) {
  case ConstraintKind::Bind:
  case ConstraintKind::Equal:
  case ConstraintKind::BindParam:
  case ConstraintKind::BindToPointerType:
  case ConstraintKind::Subtype:
  case ConstraintKind::SubclassOf:
  case ConstraintKind::Conversion:
  case ConstraintKind::ArgumentConversion:
  case ConstraintKind::OperatorArgumentConversion:
  case ConstraintKind::OptionalObject:
  case ConstraintKind::UnresolvedMemberChainBase:
  case ConstraintKind::LValueObject: {
    auto binding = inferFromRelational(CS, TypeVar, constraint);
    if (!binding)
      break;

    addPotentialBinding(TypeVar, *binding);
    break;
  }
  case ConstraintKind::KeyPathApplication: {
    // If this variable is in the application projected result type, delay
    // binding until we've bound other type variables in the key-path
    // application constraint. This ensures we try to bind the key path type
    // first, which can allow us to discover additional bindings for the result
    // type.
    SmallPtrSet<TypeVariableType *, 4> typeVars;
    findInferableTypeVars(CS.simplifyType(constraint->getThirdType()),
                          typeVars);
    if (typeVars.count(TypeVar)) {
      DelayedBy.push_back(constraint);
    }

    break;
  }

  case ConstraintKind::BridgingConversion:
  case ConstraintKind::CheckedCast:
  case ConstraintKind::EscapableFunctionOf:
  case ConstraintKind::OpenedExistentialOf:
  case ConstraintKind::KeyPath:
  case ConstraintKind::SyntacticElement:
  case ConstraintKind::Conjunction:
  case ConstraintKind::BindTupleOfFunctionParams:
  case ConstraintKind::ShapeOf:
  case ConstraintKind::ExplicitGenericArguments:
  case ConstraintKind::PackElementOf:
  case ConstraintKind::SameShape:
  case ConstraintKind::MaterializePackExpansion:
  case ConstraintKind::NonisolatedConformsTo:
  case ConstraintKind::ConformsTo:
  case ConstraintKind::LiteralConformsTo:
  case ConstraintKind::Defaultable:
  case ConstraintKind::FallbackType:
    // Constraints from which we can't do anything.
    break;

  // For now let's avoid inferring protocol requirements from
  // this constraint, but in the future we could do that to
  // to filter bindings.
  case ConstraintKind::TransitivelyConformsTo:
    break;

  case ConstraintKind::DynamicTypeOf: {
    // Direct binding of the left-hand side could result
    // in `DynamicTypeOf` failure if right-hand side is
    // bound (because 'Bind' requires equal types to
    // succeed), or left is bound to Any which is not an
    // [existential] metatype.
    auto dynamicType = constraint->getFirstType();
    if (auto *tv = dynamicType->getAs<TypeVariableType>()) {
      if (tv->getImpl().getRepresentative(nullptr) == TypeVar) {
        DelayedBy.push_back(constraint);
        break;
      }
    }

    // This is right-hand side, let's continue.
    break;
  }

  case ConstraintKind::Disjunction:
    DelayedBy.push_back(constraint);
    break;

  case ConstraintKind::ApplicableFunction:
  case ConstraintKind::DynamicCallableApplicableFunction: {
    auto overloadTy = constraint->getSecondType();
    // If current type variable represents an overload set
    // being applied to the arguments, it can't be delayed
    // by application constraints, because it doesn't
    // depend on argument/result types being resolved first.
    if (overloadTy->isEqual(TypeVar))
      break;

    LLVM_FALLTHROUGH;
  }

  case ConstraintKind::BindOverload: {
    DelayedBy.push_back(constraint);
    break;
  }

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
  case ConstraintKind::ValueWitness:
  case ConstraintKind::PropertyWrapper: {
    // If current type variable represents a member type of some reference,
    // it would be bound once member is resolved either to a actual member
    // type or to a hole if member couldn't be found.
    auto memberTy = constraint->getSecondType()->castTo<TypeVariableType>();

    if (memberTy->getImpl().hasRepresentativeOrFixed()) {
      if (auto type = memberTy->getImpl().getFixedType(/*record=*/nullptr)) {
        // It's possible that member has been bound to some other type variable
        // instead of merged with it because it's wrapped in an l-value type.
        if (type->getWithoutSpecifierType()->isEqual(TypeVar)) {
          DelayedBy.push_back(constraint);
          break;
        }
      } else {
        memberTy = memberTy->getImpl().getRepresentative(/*record=*/nullptr);
      }
    }

    if (memberTy == TypeVar)
      DelayedBy.push_back(constraint);

    break;
  }

  case ConstraintKind::OneWayEqual:{
    // Don't produce any bindings if this type variable is on the left-hand
    // side of a one-way binding.
    auto firstType = constraint->getFirstType();
    if (auto *tv = firstType->getAs<TypeVariableType>()) {
      if (tv->getImpl().getRepresentative(nullptr) == TypeVar) {
        DelayedBy.push_back(constraint);
        break;
      }
    }

    break;
  }
  }
}

void PotentialBindings::retract(ConstraintSystem &CS,
                                TypeVariableType *TypeVar,
                                Constraint *constraint) {
  if (!Constraints.remove(constraint))
    return;

  bool recordingChanges = CS.solverState && !CS.solverState->Trail.isUndoActive();

  // Record the change, if there are active scopes.
  if (recordingChanges)
    CS.recordChange(SolverTrail::Change::RetractedBindings(TypeVar, constraint));

  LLVM_DEBUG(
    llvm::dbgs() << Constraints.size() << " " << Bindings.size() << " "
                 << AdjacentVars.size() << " " << DelayedBy.size() << " "
                 << SubtypeOf.size() << " " << SupertypeOf.size() << " "
                 << EquivalentTo.size() << "\n");

  Bindings.erase(
      llvm::remove_if(Bindings,
                      [&](const PotentialBinding &binding) {
                        if (binding.getSource() == constraint) {
                          if (recordingChanges) {
                            CS.recordChange(SolverTrail::Change::RetractedBinding(
                                TypeVar, binding));
                          }
                          return true;
                        }
                        return false;
                      }),
      Bindings.end());

  DelayedBy.erase(
      llvm::remove_if(DelayedBy,
                      [&](Constraint *existing) {
                        if (existing == constraint) {
                          if (recordingChanges) {
                            CS.recordChange(SolverTrail::Change::RetractedDelayedBy(
                                TypeVar, constraint));
                          }
                          return true;
                        }
                        return false;
                      }),
      DelayedBy.end());

#define CALLBACK(ChangeKind)                                                   \
  [&](std::pair<TypeVariableType *, Constraint *> pair) {                      \
    if (pair.second == constraint) {                                           \
      if (recordingChanges) {                                                  \
        CS.recordChange(SolverTrail::Change::ChangeKind(                       \
            TypeVar, pair.first, pair.second));                                \
      }                                                                        \
      return true;                                                             \
    }                                                                          \
    return false;                                                              \
  }

  AdjacentVars.erase(
    llvm::remove_if(AdjacentVars, CALLBACK(RetractedAdjacentVar)),
    AdjacentVars.end());

  SubtypeOf.erase(
    llvm::remove_if(SubtypeOf, CALLBACK(RetractedSubtypeOf)),
    SubtypeOf.end());

  SupertypeOf.erase(
    llvm::remove_if(SupertypeOf, CALLBACK(RetractedSupertypeOf)),
    SupertypeOf.end());

  EquivalentTo.erase(
    llvm::remove_if(EquivalentTo, CALLBACK(RetractedEquivalentTo)),
    EquivalentTo.end());

#undef CALLBACK
}

void PotentialBindings::reset() {
  if (CONDITIONAL_ASSERT_enabled()) {
    ASSERT(Constraints.empty());
    ASSERT(Bindings.empty());
    ASSERT(DelayedBy.empty());
    ASSERT(AdjacentVars.empty());
    ASSERT(SubtypeOf.empty());
    ASSERT(SupertypeOf.empty());
    ASSERT(EquivalentTo.empty());
  }

  AssociatedCodeCompletionToken = ASTNode();
}

void PotentialBindings::dump(ConstraintSystem &cs,
                             TypeVariableType *typeVar,
                             llvm::raw_ostream &out,
                             unsigned indent) const {
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;

  out << "Potential bindings for ";
  typeVar->getImpl().print(out);
  out << "\n";

  out << "[constraints: ";
  interleave(Constraints,
             [&](Constraint *constraint) {
               constraint->print(out, &cs.getASTContext().SourceMgr, indent,
                                 /*skipLocator=*/true);
             },
             [&out]() { out << ", "; });
  out << "] ";

  if (!AdjacentVars.empty()) {
    out << "[adjacent to: ";
    SmallVector<std::pair<TypeVariableType *, Constraint *>> adjacentVars(
        AdjacentVars.begin(), AdjacentVars.end());
    llvm::sort(adjacentVars,
               [](auto lhs, auto rhs) {
                   return lhs.first->getID() < rhs.first->getID();
               });
    interleave(adjacentVars,
               [&](std::pair<TypeVariableType *, Constraint *> pair) {
                 out << pair.first->getString(PO);
                 if (pair.first->getImpl().getFixedType(/*record=*/nullptr))
                   out << " (fixed)";
                 out << " via ";
                 pair.second->print(out, &cs.getASTContext().SourceMgr, indent,
                                    /*skipLocator=*/true);
               },
               [&out]() { out << ", "; });
    out << "] ";
  }
}

void BindingSet::forEachLiteralRequirement(
    llvm::function_ref<void(KnownProtocolKind)> callback) const {
  for (const auto &literal : Literals) {
    auto *protocol = literal.first;
    const auto &info = literal.second;

    // Only uncovered defaultable literal protocols participate.
    if (!info.viableAsBinding())
      continue;
    
    if (auto protocolKind = protocol->getKnownProtocolKind())
      callback(*protocolKind);
  }
}

LiteralBindingKind BindingSet::getLiteralForScore() const {
  LiteralBindingKind kind = LiteralBindingKind::None;

  forEachLiteralRequirement([&](KnownProtocolKind protocolKind) {
    switch (protocolKind) {
    case KnownProtocolKind::ExpressibleByDictionaryLiteral:
    case KnownProtocolKind::ExpressibleByArrayLiteral:
    case KnownProtocolKind::ExpressibleByStringInterpolation:
      kind = LiteralBindingKind::Collection;
      break;

    case KnownProtocolKind::ExpressibleByFloatLiteral:
      kind = LiteralBindingKind::Float;
      break;

    default:
      if (kind != LiteralBindingKind::Collection)
        kind = LiteralBindingKind::Atom;
      break;
    }
  });
  return kind;
}

unsigned BindingSet::getNumViableLiteralBindings() const {
  return llvm::count_if(Literals, [&](const auto &literal) {
    return literal.second.viableAsBinding();
  });
}

/// Return string for atomic literal kinds (integer, string, & boolean) for
/// printing in debug output.
static std::string getAtomLiteralAsString(ExprKind EK) {
#define ENTRY(Kind, String)                                                    \
  case ExprKind::Kind:                                                         \
    return String
  switch (EK) {
    ENTRY(IntegerLiteral, "integer");
    ENTRY(StringLiteral, "string");
    ENTRY(BooleanLiteral, "boolean");
    ENTRY(NilLiteral, "nil");
  default:
    return "";
  }
#undef ENTRY
}

/// Return string for collection literal kinds (interpolated string, array,
/// dictionary) for printing in debug output.
static std::string getCollectionLiteralAsString(KnownProtocolKind KPK) {
#define ENTRY(Kind, String)                                                    \
  case KnownProtocolKind::Kind:                                                \
    return String
  switch (KPK) {
    ENTRY(ExpressibleByDictionaryLiteral, "dictionary");
    ENTRY(ExpressibleByArrayLiteral, "array");
    ENTRY(ExpressibleByStringInterpolation, "interpolated string");
  default:
    return "";
  }
#undef ENTRY
}

void BindingSet::dump(llvm::raw_ostream &out, unsigned indent) const {
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;

  if (auto typeVar = getTypeVariable()) {
    typeVar->getImpl().print(out);
    out << " ";
  }

  std::vector<std::string> attributes;
  if (isDirectHole())
    attributes.push_back("hole");
  if (isPotentiallyIncomplete())
    attributes.push_back("potentially_incomplete");
  if (isDelayed())
    attributes.push_back("delayed");
  if (isSubtypeOfExistentialType())
    attributes.push_back("subtype_of_existential");
  if (!attributes.empty()) {
    out << "[attributes: ";
    interleave(attributes, out, ", ");
  }
  
  auto literalKind = getLiteralForScore();
  if (literalKind != LiteralBindingKind::None) {
    if (!attributes.empty()) {
      out << ", ";
    } else {
      out << "[attributes: ";
    }
    out << "[literal: ";
    switch (literalKind) {
    case LiteralBindingKind::Atom: {
      if (auto atomKind = TypeVar->getImpl().getAtomicLiteralKind()) {
        out << getAtomLiteralAsString(*atomKind);
      }
      break;
    }
    case LiteralBindingKind::Collection: {
      std::vector<std::string> collectionLiterals;
      forEachLiteralRequirement([&](KnownProtocolKind protocolKind) {
        collectionLiterals.push_back(
            getCollectionLiteralAsString(protocolKind));
      });
      interleave(collectionLiterals, out, ", ");
      break;
    }
    case LiteralBindingKind::Float:
    case LiteralBindingKind::None:
      out << getLiteralBindingKind(literalKind).str();
      break;
    }
    if (attributes.empty()) {
      out << "]] ";
    } else {
      out << "]";
    }
  }
  if (!attributes.empty())
    out << "] ";

  if (!AdjacentVars.empty()) {
    out << "[adjacent to: ";
    SmallVector<TypeVariableType *> adjacentVars(AdjacentVars.begin(),
                                                 AdjacentVars.end());
    llvm::sort(adjacentVars,
               [](const TypeVariableType *lhs, const TypeVariableType *rhs) {
                   return lhs->getID() < rhs->getID();
               });
    interleave(adjacentVars,
               [&](auto *typeVar) {
                 out << typeVar->getString(PO);
                 if (typeVar->getImpl().getFixedType(/*record=*/nullptr))
                   out << " (fixed)";
               },
               [&out]() { out << ", "; });
    out << "] ";
  }

  auto numDefaultable = getNumViableDefaultableBindings();
  if (numDefaultable > 0)
    out << "[defaultable bindings: " << numDefaultable << "] ";

  struct PrintableBinding {
  private:
    enum class BindingKind { Exact, Subtypes, Supertypes, Literal };
    BindingKind Kind;
    Type BindingType;
    bool Viable;
    PrintableBinding(BindingKind kind, Type bindingType, bool viable)
        : Kind(kind), BindingType(bindingType), Viable(viable) {}

  public:
    static PrintableBinding supertypesOf(Type binding) {
      return PrintableBinding{BindingKind::Supertypes, binding, true};
    }
    
    static PrintableBinding subtypesOf(Type binding) {
      return PrintableBinding{BindingKind::Subtypes, binding, true};
    }
    
    static PrintableBinding exact(Type binding) {
      return PrintableBinding{BindingKind::Exact, binding, true};
    }
    
    static PrintableBinding literalDefaultType(Type binding, bool viable) {
      return PrintableBinding{BindingKind::Literal, binding, viable};
    }

    void print(llvm::raw_ostream &out, const PrintOptions &PO,
               unsigned indent = 0) const {
      switch (Kind) {
      case BindingKind::Exact:
        break;
      case BindingKind::Subtypes:
        out << "(subtypes of) ";
        break;
      case BindingKind::Supertypes:
        out << "(supertypes of) ";
        break;
      case BindingKind::Literal:
        out << "(default type of literal) ";
        break;
      }
      if (BindingType)
        BindingType.print(out, PO);
      if (!Viable)
        out << " [literal not viable]";
    }
  };

  out << "[potential bindings: ";
  SmallVector<PrintableBinding, 2> potentialBindings;
  for (const auto &binding : Bindings) {
    switch (binding.Kind) {
    case AllowedBindingKind::Exact:
      potentialBindings.push_back(PrintableBinding::exact(binding.BindingType));
      break;
    case AllowedBindingKind::Supertypes:
      potentialBindings.push_back(
          PrintableBinding::supertypesOf(binding.BindingType));
      break;
    case AllowedBindingKind::Subtypes:
      potentialBindings.push_back(
          PrintableBinding::subtypesOf(binding.BindingType));
      break;
    }
  }
  for (const auto &literal : Literals) {
    potentialBindings.push_back(PrintableBinding::literalDefaultType(
        literal.second.hasDefaultType()
        ? literal.second.getDefaultType()
        : Type(),
        literal.second.viableAsBinding()));
  }
  if (potentialBindings.empty()) {
    out << "<none>";
  } else {
    interleave(
        potentialBindings,
        [&](const PrintableBinding &binding) { binding.print(out, PO); },
        [&] { out << ", "; });
  }
  out << "]";

  if (!Defaults.empty()) {
    out << " [defaults: ";
    interleave(
        Defaults,
        [&](const auto &entry) {
          auto *constraint = entry.second;
          auto defaultBinding =
              PrintableBinding::exact(constraint->getSecondType());
          defaultBinding.print(out, PO);
        },
        [&] { out << ", "; });
    out << "]";
  }
  
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
      if (getLocator()->isLastElement<LocatorPathElt::ClosureResult>() &&
          binding.Kind == AllowedBindingKind::Supertypes) {
        auto voidType = CS.getASTContext().TheEmptyTupleType;
        addNewBinding(binding.withSameSource(voidType, BindingKind::Exact));
      }

      for (auto supertype : enumerateDirectSupertypes(type)) {
        // If we're not allowed to try this binding, skip it.
        if (auto simplifiedSuper = checkTypeOfBinding(TypeVar, supertype)) {
          auto supertype = *simplifiedSuper;
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
      PrintOptions PO;
      PO.PrintTypesForDebugging = true;

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
