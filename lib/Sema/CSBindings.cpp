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
#include "swift/Sema/Subtyping.h"
#include "swift/Sema/TypeVariableType.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <tuple>

#define DEBUG_TYPE "PotentialBindings"

STATISTIC(NumBindingSetsSkipped, "binding sets that did not need recomputation");
STATISTIC(NumBindingSetsRecomputed, "binding sets that required recomputation");

using namespace swift;
using namespace constraints;
using namespace inference;

void ConstraintGraphNode::initBindingSet() {
  ASSERT(!hasBindingSet());
  ASSERT(forRepresentativeVar());

  Set.emplace(CG.getConstraintSystem(), TypeVar, Potential);
}

static bool isDirectRequirement(ConstraintSystem &cs,
                                TypeVariableType *typeVar,
                                Constraint *constraint) {
  if (auto *other = constraint->getFirstType()->getAs<TypeVariableType>()) {
    return typeVar == cs.getRepresentative(other);
  }

  return false;
}

BindingSet::BindingSet(ConstraintSystem &CS, TypeVariableType *TypeVar,
                       const PotentialBindings &info)
    : CS(CS), TypeVar(TypeVar), Info(info) {
  GenerationNumber = Info.GenerationNumber;

  for (const auto &binding : info.Bindings)
    addBinding(binding);

  for (const auto &literal : info.Literals)
    Literals.push_back(literal);

  for (auto *constraint : info.Defaults) {
    // Do these in a separate pass.
    if (isDirectRequirement(CS, TypeVar, constraint))
      addDefault(constraint);
  }

  for (auto &entry : info.AdjacentVars)
    AdjacentVars.insert(entry.first);

  ASSERT(!IsDirty);
}

bool BindingSet::forClosureResult() const {
  return TypeVar->getImpl().isClosureResultType();
}

bool BindingSet::forGenericParameter() const {
  return bool(TypeVar->getImpl().getGenericParameter());
}

bool BindingSet::canBeNil() const {
  for (const auto &literal : Literals) {
    if (literal.getProtocol()->isSpecificProtocol(
        KnownProtocolKind::ExpressibleByNilLiteral))
      return true;
  }
  return false;
}

bool BindingSet::isDirectHole() const {
  // Direct holes are only allowed in "diagnostic mode".
  if (!CS.shouldAttemptFixes())
    return false;

  return !hasViableBindings() && TypeVar->getImpl().canBindToHole();
}

static bool isGenericParameter(TypeVariableType *TypeVar) {
  auto *locator = TypeVar->getImpl().getLocator();
  return locator && locator->isLastElement<LocatorPathElt::GenericParameter>();
}

bool PotentialBinding::isViableForJoin() const {
  return Kind == AllowedBindingKind::Supertypes &&
         !BindingType->hasLValueType() &&
         !BindingType->hasTypeVariable() &&
         !BindingType->hasPlaceholder() &&
         !BindingType->hasUnboundGenericType() &&
         !hasDefaultedLiteralProtocol() &&
         !isDefaultableBinding();
}

namespace {

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

}

void PotentialBinding::print(llvm::raw_ostream &out,
                             const PrintOptions &PO) const {
  switch (Kind) {
  case AllowedBindingKind::Exact:
    PrintableBinding::exact(BindingType).print(out, PO);
    break;
  case AllowedBindingKind::Supertypes:
    PrintableBinding::supertypesOf(BindingType).print(out, PO);
    break;
  case AllowedBindingKind::Subtypes:
    PrintableBinding::subtypesOf(BindingType).print(out, PO);
    break;
  }
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

  // This is effectively a no-op right now since bindings are re-computed
  // on each step of the solver and fixed types won't appear in AdjancentVars,
  // but once bindings are computed incrementally it becomes important
  // to double-check that any adjacent type variables found previously are
  // still unresolved.
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
    auto conformanceReqs =
        node.getPotentialBindings().getConformanceRequirements();

    // If current variable already has transitive protocol
    // conformances inferred, there is no need to look deeper
    // into subtype/equivalence chain.
    if (bindings.TransitiveProtocols) {
      TypeVariableType *parent = nullptr;
      std::tie(parent, currentVar) = workList.pop_back_val();
      assert(parent);
      propagateProtocolsTo(parent, conformanceReqs,
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

      auto conformanceReqs =
          node.getPotentialBindings().getConformanceRequirements();

      llvm::SmallPtrSet<Constraint *, 2> placeholder;
      // Add any direct protocols from members of the
      // equivalence class, so they could be propagated
      // to all of the members.
      propagateProtocolsTo(currentVar, conformanceReqs, placeholder);

      const auto &bindings = node.getBindingSet();

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
      propagateProtocolsTo(parent, conformanceReqs, protocols[currentVar]);
    }

    auto &inferredProtocols = protocols[currentVar];

    llvm::SmallPtrSet<Constraint *, 4> protocolsForEquivalence;

    // Equivalence class should contain both:
    // - direct protocol requirements of the current type
    //   variable;
    // - all of the transitive protocols inferred through
    //   the members of the equivalence class.
    {
      protocolsForEquivalence.insert(conformanceReqs.begin(),
                                     conformanceReqs.end());

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

void BindingSet::inferTransitiveKeyPathBindings() {
  // If the current type variable represents a key path root type
  // let's try to transitively infer its type through bindings of
  // a key path type.
  if (TypeVar->getImpl().isKeyPathRoot()) {
    auto *locator = TypeVar->getImpl().getLocator();
    if (auto *keyPathTy =
            CS.getType(locator->getAnchor())->getAs<TypeVariableType>()) {
      auto &node = CS.getConstraintGraph()[keyPathTy];
      if (node.hasBindingSet()) {
        const auto &bindings = node.getBindingSet();

        for (auto &binding : bindings.Bindings) {
          auto bindingTy = binding.BindingType->lookThroughAllOptionalTypes();

          Type inferredRootTy;
          if (bindingTy->isKnownKeyPathType()) {
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
                const auto &bindings = node.getBindingSet();

                // Don't infer if root is not yet fully resolved.
                if (bindings.isDelayed())
                  continue;

                // Copy the bindings over to the root.
                for (const auto &binding : bindings.Bindings)
                  addBinding(binding.asTransitiveFrom(contextualRootVar));

                // Make a note that the key path root is transitively adjacent
                // to contextual root type variable and all of its variables.
                // This is important for ranking.
                AdjacentVars.insert(contextualRootVar);
                AdjacentVars.insert(bindings.AdjacentVars.begin(),
                                    bindings.AdjacentVars.end());

                // Note the fact that we modified the binding set.
                markDirty();
              }
            } else {
              auto newBinding = binding.withSameSource(
                  inferredRootTy, AllowedBindingKind::Exact);
              addBinding(newBinding.asTransitiveFrom(keyPathTy));

              // Note the fact that we modified the binding set.
              markDirty();
            }
          }
        }
      }
    }
  }
}

void BindingSet::inferTransitiveSupertypeBindings() {
  llvm::SmallDenseSet<ProtocolDecl *> seenLiterals;
  for (const auto &literal : Literals) {
    bool inserted = seenLiterals.insert(literal.getProtocol()).second;
    ASSERT(inserted);
  }

  for (const auto &entry : Info.SupertypeOf) {
    auto &node = CS.getConstraintGraph()[entry.first];
    if (!node.hasBindingSet())
      continue;

    const auto &bindings = node.getBindingSet();

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
    for (auto literal : bindings.Literals) {
      auto *protocol = literal.getProtocol();

      if (!seenLiterals.insert(protocol).second)
        continue;

      literal.setDirectRequirement(false);
      Literals.push_back(literal);

      // Note the fact that we modified the binding set.
      markDirty();
    }

    // TODO: We shouldn't need this in the future.
    if (entry.second->getKind() != ConstraintKind::Subtype)
      continue;

    for (auto &binding : bindings.Bindings) {
      // We need the binding kind for the potential binding to
      // either be Exact or Supertypes in order for it to make sense
      // to add Supertype bindings based on the relationship between
      // our type variables.
      if (binding.Kind != AllowedBindingKind::Exact &&
          binding.Kind != AllowedBindingKind::Supertypes)
        continue;

      auto type = binding.BindingType;

      if (type->isPlaceholder())
        continue;

      if (ConstraintSystem::typeVarOccursInType(TypeVar, type))
        continue;

      auto newBinding =
          binding.withSameSource(type, AllowedBindingKind::Supertypes);
      addBinding(newBinding.asTransitiveFrom(entry.first));

      // Note the fact that we modified the binding set.
      markDirty();
    }
  }
}

void BindingSet::inferTransitiveUnresolvedMemberRefBindings() {
  if (!hasViableBindings()) {
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
        inferTransitiveProtocolRequirements();

        if (TransitiveProtocols.has_value()) {
          for (auto *constraint : *TransitiveProtocols) {
            Type protocolTy = constraint->getSecondType();

            // Compiler-known marker protocols cannot be extended with members,
            // so do not consider them.
            if (auto p = protocolTy->getAs<ProtocolType>()) {
              ProtocolDecl *decl = p->getDecl();
              if (decl->getKnownProtocolKind() && decl->isMarkerProtocol())
                continue;

              // During normal type-checking filter inferred protocols based on
              // whether they have the member or not. There is no reason to
              // attempt unrelated protocols and adding them as bindings affects
              // type variable selection as well. They can be attempted during
              // diagnostics mode in case the member is misspelled or
              // inaccessible.
              if (!CS.shouldAttemptFixes()) {
                auto memberRef =
                    castToExpr<UnresolvedMemberExpr>(locator->getAnchor());

                auto &results = CS.lookupMember(
                    protocolTy, memberRef->getName(), memberRef->getLoc());
                if (results.empty())
                  continue;
              }
            }

            addBinding({protocolTy, AllowedBindingKind::Exact, constraint});

            // Note the fact that we modified the binding set.
            markDirty();
          }
        }
      }
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

bool BindingSet::finalizeKeyPathBindings() {
  if (auto *locator = TypeVar->getImpl().getLocator()) {
    if (TypeVar->getImpl().isKeyPathType()) {
      auto &ctx = CS.getASTContext();
      auto *keyPath = castToExpr<KeyPathExpr>(locator->getAnchor());

      bool isValid;
      std::optional<KeyPathCapability> capability;

      std::tie(isValid, capability) = CS.inferKeyPathLiteralCapability(TypeVar);

      // Key path literal is not yet sufficiently resolved, this binding
      // set is not viable.
      if (isValid && !capability)
        return false;

      bool isContextualTypeReadOnly = false;
      // If the key path is sufficiently resolved we can add inferred binding
      // to the set.
      SmallSetVector<PotentialBinding, 4> updatedBindings;
      for (const auto &binding : Bindings) {
        auto bindingTy = binding.BindingType->lookThroughAllOptionalTypes();

        assert(bindingTy->isKnownKeyPathType() ||
               bindingTy->is<FunctionType>());

        // Functions don't have capability so we can simply add them.
        if (auto *fnType = bindingTy->getAs<FunctionType>()) {
          auto extInfo = fnType->getExtInfo();

          bool isKeyPathSendable = capability && capability->second;
          if (!isKeyPathSendable && extInfo.isSendable()) {
            fnType = FunctionType::get(fnType->getParams(), fnType->getResult(),
                                       extInfo.withSendable(false));
          }

          updatedBindings.insert(binding.withType(fnType));
          isContextualTypeReadOnly = true;
        } else if (!(bindingTy->isWritableKeyPath() ||
                     bindingTy->isReferenceWritableKeyPath())) {
          isContextualTypeReadOnly = true;
        }
      }

      // Note that even though key path literal maybe be invalid it's
      // still the best course of action to use contextual function type
      // bindings because they allow to propagate type information from
      // the key path into the context, so key path bindings are added
      // only if there is absolutely no other choice.
      if (updatedBindings.empty()) {
        auto rootTy = CS.getKeyPathRootType(keyPath);

        // A valid key path literal.
        if (capability) {
          // Capability inference always results in a maximum mutability
          // but if context is read-only it can be downgraded to avoid
          // conversions.
          if (isContextualTypeReadOnly)
            capability =
                std::make_pair(KeyPathMutability::ReadOnly, capability->second);

          // Note that the binding is formed using root & value
          // type variables produced during constraint generation
          // because at this point root is already known (otherwise
          // inference wouldn't been able to determine key path's
          // capability) and we always want to infer value from
          // the key path and match it to a contextual type to produce
          // better diagnostics.
          auto keyPathTy = getKeyPathType(ctx, *capability, rootTy,
                                          CS.getKeyPathValueType(keyPath));
          updatedBindings.insert({keyPathTy, AllowedBindingKind::Exact, locator,
                                  /*originator=*/nullptr});
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
              return entry->getKind() == ConstraintKind::FallbackType;
            });
            assert(fallback != Defaults.end());
            updatedBindings.insert(
                {(*fallback)->getSecondType(),
                 AllowedBindingKind::Exact,
                 *fallback});
          } else {
            updatedBindings.insert(PotentialBinding::forHole(
                TypeVar, CS.getConstraintLocator(
                             keyPath, ConstraintLocator::FallbackType)));
          }
        }
      }

      Bindings = std::move(updatedBindings);
      Defaults.clear();

      // Note the fact that we modified the binding set.
      markDirty();
    }
  }

  return true;
}

void BindingSet::finalizeUnresolvedMemberChainResult() {
  if (auto *locator = TypeVar->getImpl().getLocator()) {
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

        // Note the fact that we modified the binding set.
        markDirty();
      }
    }
  }
}

void BindingSet::addBinding(PotentialBinding binding) {
  if (Bindings.count(binding))
    return;

  if (binding.isTransitive() &&
      !checkTypeOfBinding(TypeVar, binding.BindingType))
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

  // Prevent against checking against the same opened nominal type
  // over and over again. Doing so means redundant work in the best
  // case. In the worst case, we'll produce lots of duplicate solutions
  // for this constraint system, which is problematic for overload
  // resolution.
  if (auto *NTD = binding.BindingType->getAnyNominal()) {
    for (auto existing = Bindings.begin(); existing != Bindings.end();
         ++existing) {
      auto existingType = existing->BindingType;

      auto *existingNTD = existingType->getAnyNominal();
      if (!existingNTD || NTD != existingNTD)
        continue;

      // A binding can subsume another binding. Suppose we have two
      // constraints:
      // - X conv $T0
      // - $T0 conv Y
      //
      // This gives us two potential bindings:
      // - (subtypes of) X
      // - (supertypes of) Y
      //
      // If X and Y are a match except that X has type variables and
      // Y does not, we replace the subtype binding type with Y, and
      // drop the supertype binding:
      // - (subtypes of) Y
      //
      // Two more combinations are supported. Suppose as above that
      // X is concrete and Y has type variables.
      //
      // Then:
      // - (exact / subtypes of / supertypes of) X
      // - (exact) Y
      // becomes:
      // - (exact) Y
      //
      // And finally:
      // - (supertypes of) X
      // - (supertypes of) Y
      // becomes:
      // - (supertypes of) Y
      //
      // This is unsound, but without it we get ambiguous solutions
      // and performance problems in a couple of instances where we
      // end up considering bindings like Int? vs $T0?, where $T0
      // is subsequently bound to Int.
      if (!(binding.Kind == AllowedBindingKind::Exact ||
            existing->Kind == AllowedBindingKind::Supertypes))
        continue;

      // If new type has a type variable it shouldn't
      // be considered viable.
      if (binding.BindingType->hasTypeVariable())
        return;

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
        auto newBinding = inferredCGFloat->withType(type);
        (void)Bindings.erase(inferredCGFloat);
        Bindings.insert(newBinding);
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
        auto joinType =
            Type::join(existingBinding->BindingType, binding.BindingType);

        if (joinType && isAcceptableJoin(*joinType)) {
          // Result of the join has to use new binding because it refers
          // to the constraint that triggered the join that replaced the
          // existing binding.
          //
          // For "join" to be transitive, both bindings have to be as
          // well, otherwise we consider it a refinement of a direct
          // binding.
          auto *origintor =
              binding.isTransitive() && existingBinding->isTransitive()
                  ? binding.Originator
                  : nullptr;

          PotentialBinding join(*joinType, binding.Kind, binding.BindingSource,
                                origintor);

          joined.push_back(join);
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

  for (auto &literal : Literals) {
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
        auto newBinding = binding->withType(adjustedTy);
        (void)Bindings.erase(binding);
        Bindings.insert(newBinding);

        // Note the fact that we modified the binding set.
        markDirty();
      }

      break;
    }
  }
}

void BindingSet::coalesceIntegerAndFloatLiteralRequirements() {
  decltype(Literals)::iterator intLiteral = Literals.end();
  decltype(Literals)::iterator floatLiteral = Literals.end();

  for (auto iter = Literals.begin(); iter != Literals.end(); ++iter) {
    auto *protocol = iter->getProtocol();

    if (protocol->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByIntegerLiteral)) {
      intLiteral = iter;
    }

    if (protocol->isSpecificProtocol(
            KnownProtocolKind::ExpressibleByFloatLiteral)) {
      floatLiteral = iter;
    }
  }

  if (intLiteral != Literals.end() &&
      floatLiteral != Literals.end()) {
    Literals.erase(intLiteral);
  }
}

void PotentialBindings::inferFromLiteral(Constraint *constraint) {
  ASSERT(TypeVar);
  ASSERT(isDirectRequirement(CS, TypeVar, constraint));

  auto *protocol = constraint->getProtocol();

  for (const auto &literal : Literals) {
    if (literal.getProtocol() == protocol)
      return;
  }
  
  Type defaultType;
  // `ExpressibleByNilLiteral` doesn't have a default type.
  if (!protocol->isSpecificProtocol(
          KnownProtocolKind::ExpressibleByNilLiteral)) {
    defaultType = TypeChecker::getDefaultType(protocol, CS.DC);
  }

  // "undo" check is necessary here because this method is called
  // from `Change::undo`, that's necessary because we only record
  // a constraint.
  if (CS.solverState && !CS.solverState->Trail.isUndoActive())
    CS.recordChange(SolverTrail::Change::AddedLiteral(TypeVar, constraint));

  Literals.emplace_back(protocol, constraint, defaultType, /*isDirect=*/true);
}

bool BindingSet::operator==(const BindingSet &other) const {
  if (AdjacentVars != other.AdjacentVars)
    return false;

  if (Bindings.size() != other.Bindings.size())
    return false;

  for (unsigned i : indices(Bindings)) {
    const auto &x = Bindings[i];
    const auto &y = other.Bindings[i];

    if (x.BindingType.getPointer() != y.BindingType.getPointer() ||
        x.Kind != y.Kind)
      return false;
  }

  if (Literals.size() != other.Literals.size())
    return false;

  for (unsigned i : indices(Literals)) {
    auto &x = Literals[i];
    auto &y = other.Literals[i];

    if (x.Source != y.Source ||
        x.DefaultType.getPointer() != y.DefaultType.getPointer() ||
        x.IsDirectRequirement != y.IsDirectRequirement) {
      return false;
    }
  }

  if (Defaults.size() != other.Defaults.size())
    return false;

  for (auto i : indices(Defaults)) {
    auto *x = Defaults[i];
    auto *y = other.Defaults[i];
    if (x != y)
      return false;
  }

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

#define COMMON_BINDING_INFORMATION_ADDITION(PropertyName, Storage)             \
  void PotentialBindings::record##PropertyName(Constraint *constraint) {       \
    if (CS.solverState)                                                        \
      CS.recordChange(                                                         \
          SolverTrail::Change::Added##PropertyName(TypeVar, constraint));      \
    Storage.push_back(constraint);                                             \
  }
#define BINDING_RELATION_ADDITION(RelationName, Storage)                       \
  void PotentialBindings::record##RelationName(TypeVariableType *typeVar,      \
                                               Constraint *originator) {       \
    if (CS.solverState)                                                        \
      CS.recordChange(SolverTrail::Change::Added##RelationName(                \
          TypeVar, typeVar, originator));                                      \
    Storage.emplace_back(typeVar, originator);                                 \
  }
#include "swift/Sema/CSTrail.def"

const BindingSet *ConstraintSystem::determineBestBindings() {
  // Look for potential type variable bindings.
  BindingSet *bestBindings = nullptr;

  // First, let's construct a BindingSet for each type variable,
  // from its PotentialBindings.
  for (auto *typeVar : getTypeVariables()) {
    auto &node = CG[typeVar];

    // If the type variable has been bound to a fixed type, we won't
    // be considering it any further. Clear its binding set if it
    // has one and move on.
    if (typeVar->getImpl().hasRepresentativeOrFixed()) {
      node.resetBindingSet();
      continue;
    }

    // If we don't have a binding set yet, compute one.
    if (!node.hasBindingSet()) {
      node.initBindingSet();
      continue;
    }

    // If the node has a binding set already, check if it needs to
    // be recomputed. This is the case if the PotentialBindings
    // changed since last time, or if any of the "transitive inference"
    // steps below changed the BindingSet for any reason.
    if (!node.getBindingSet().isUpToDate()) {
      ++NumBindingSetsRecomputed;
      node.resetBindingSet();
      node.initBindingSet();
    } else {
      ++NumBindingSetsSkipped;
      node.getBindingSet().resetTransitiveProtocols();
    }
  }

  // Now let's perform transitive inference and ranking.
  for (auto *typeVar : getTypeVariables()) {
    auto &node = CG[typeVar];
    if (!node.hasBindingSet())
      continue;

    auto &bindings = node.getBindingSet();

    // ****
    // If any of the below change the binding set, they must also call
    // markDirty().
    // ****

#define EXPENSIVE_CHECK 0
#if EXPENSIVE_CHECK
    BindingSet saved(*this, typeVar, node.getPotentialBindings());
    ASSERT(bindings.getGenerationNumber() == saved.getGenerationNumber());
    if (bindings != saved) {
      ABORT([&](auto &out) {
        out << "Binding set differs from freshly-recomputed one\n";
        out << "\nold: ";
        bindings.dump(out, 0);
        out << "\nnew: ";
        saved.dump(out, 0);
      });
    }
#endif

    // Special handling for key paths.
    bindings.inferTransitiveKeyPathBindings();
    if (!bindings.finalizeKeyPathBindings())
      continue;

    // Special handling for "leading-dot" unresolved member references,
    // like .foo.
    bindings.inferTransitiveUnresolvedMemberRefBindings();
    bindings.finalizeUnresolvedMemberChainResult();

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
    bool isViable = bindings.isViable();

    bindings.inferTransitiveSupertypeBindings();
    bindings.determineLiteralCoverage();

#if EXPENSIVE_CHECK
    if (!bindings.isDirty() && saved != bindings) {
      ABORT([&](auto &out) {
        out << "Binding set changed but wasn't dirty\n";
        out << "\nold: ";
        saved.dump(out, 0);
        out << "\nnew: ";
        bindings.dump(out, 0);
      });
    }
#endif

    if (!isViable)
      continue;

    // If these are the first bindings, or they are better than what
    // we saw before, use them instead.
    if (!bestBindings || bindings < *bestBindings)
      bestBindings = &bindings;
  }

  if (isDebugMode()) {
    bool first = true;

    for (auto *typeVar : getTypeVariables()) {
      auto &node = CG[typeVar];
      if (!node.hasBindingSet())
        continue;

      const auto &bindings = node.getBindingSet();

      if (isDebugMode() && bindings.hasViableBindings()) {
        if (first) {
          llvm::errs().indent(solverState->getCurrentIndent())
              << "(Potential Binding(s)\n";
          first = false;
        }
        auto &log = llvm::errs().indent(solverState->getCurrentIndent() + 2);
        bindings.dump(log, solverState->getCurrentIndent() + 2);
        log << "\n";
      }

      if (!first) {
        auto &log = llvm::errs().indent(solverState->getCurrentIndent());
        log << ")\n";
      }
    }
  }

  if (bestBindings)
    bestBindings->coalesceIntegerAndFloatLiteralRequirements();

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
        return Action::SkipNode;

      if (auto typeVar = ty->getAs<TypeVariableType>())
        typeVars.insert(typeVar);
      return Action::Continue;
    }
  };

  type.walk(Walker(typeVars));
}

void BindingSet::addDefault(Constraint *constraint) {
  if (CONDITIONAL_ASSERT_enabled()) {
    for (auto *other : Defaults) {
      ASSERT(other != constraint);
    }
  }
  Defaults.push_back(constraint);
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
      return std::make_pair(true, requiresUnwrap ? type : Type());
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

void PotentialBindings::addPotentialBinding(PotentialBinding binding) {
  ASSERT(TypeVar);
  assert(!binding.BindingType->is<ErrorType>());

  // If the type variable can't bind to an lvalue, make sure the
  // type we pick isn't an lvalue.
  if (!TypeVar->getImpl().canBindToLValue() &&
      binding.BindingType->hasLValueType()) {
    binding = binding.withType(binding.BindingType->getRValueType());
  }

  LLVM_DEBUG(
    PrintOptions PO = PrintOptions::forDebugging();
    llvm::dbgs() << "Recording ";
    TypeVar->print(llvm::dbgs(), PO);
    llvm::dbgs() << " ";
    binding.print(llvm::dbgs(), PO);
    llvm::dbgs() << " from ";
    if (auto *constraint = dyn_cast<Constraint *>(binding.BindingSource)) {
      constraint->print(llvm::dbgs(), &TypeVar->getASTContext().SourceMgr, 0);
    } else {
      auto *locator = cast<ConstraintLocator *>(binding.BindingSource);
      locator->dump(&TypeVar->getASTContext().SourceMgr, llvm::dbgs());
    }
    llvm::dbgs() << "\n");

  if (CS.solverState)
    CS.recordChange(SolverTrail::Change::AddedBinding(TypeVar, binding));

  Bindings.push_back(std::move(binding));
}

/// Check whether the given type can be used as a binding for the given
/// type variable.
///
/// \returns true if the binding is okay.
bool swift::constraints::inference::checkTypeOfBinding(
    TypeVariableType *typeVar, Type type) {
  // If the type references the type variable, don't permit the binding.
  if (type->hasTypeVariable()) {
    SmallPtrSet<TypeVariableType *, 4> referencedTypeVars;
    type->getTypeVariables(referencedTypeVars);
    if (referencedTypeVars.count(typeVar))
      return false;
  }

  {
    auto objType = type->getWithoutSpecifierType();

    // If the type is a type variable itself, don't permit the binding.
    if (objType->is<TypeVariableType>())
      return false;

    // Don't bind to a dependent member type, even if it's currently
    // wrapped in any number of optionals, because binding producer
    // might unwrap and try to attempt it directly later.
    if (objType->lookThroughAllOptionalTypes()->is<DependentMemberType>())
      return false;
  }

  // Okay, allow the binding.
  return true;
}

bool BindingSet::favoredOverDisjunction(Constraint *disjunction) const {
  if (isHole())
    return false;

  if (llvm::any_of(Bindings, [&](const PotentialBinding &binding) {
        if (binding.Kind == AllowedBindingKind::Supertypes)
          return false;

        if (CS.shouldAttemptFixes())
          return false;

        return !hasProperSubtypes(binding.BindingType);
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

  (void) bindings.finalizeKeyPathBindings();
  bindings.finalizeUnresolvedMemberChainResult();
  bindings.determineLiteralCoverage();
  bindings.coalesceIntegerAndFloatLiteralRequirements();

  return bindings;
}

std::optional<PotentialBinding>
PotentialBindings::inferFromRelational(Constraint *constraint) {
  ASSERT(TypeVar);
  assert(constraint->getClassification() ==
             ConstraintClassification::Relational &&
         "only relational constraints handled here");

  LLVM_DEBUG(
      llvm::dbgs() << "inferFromRelational(";
      TypeVar->print(llvm::dbgs(), PrintOptions::forDebugging());
      llvm::dbgs() << ", ";
      constraint->print(llvm::dbgs(), &CS.getASTContext().SourceMgr, 0);
      llvm::dbgs() << ")\n");

  auto first = CS.simplifyType(constraint->getFirstType());
  auto second = CS.simplifyType(constraint->getSecondType());

#define DEBUG_BAILOUT(msg)                                              \
  LLVM_DEBUG(                                                           \
      PrintOptions PO = PrintOptions::forDebugging();                   \
      llvm::dbgs() << msg << " ";                                       \
      TypeVar->print(llvm::dbgs(), PO);                                 \
      llvm::dbgs() << " from ";                                         \
      constraint->print(llvm::dbgs(), &CS.getASTContext().SourceMgr);   \
      llvm::dbgs() << "\n");

  if (first->is<TypeVariableType>() && first->isEqual(second)) {
    DEBUG_BAILOUT("First is second");
    return std::nullopt;
  }

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
        DEBUG_BAILOUT("Delayed (1)");
        recordDelayedBy(constraint);
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
        recordAdjacentVar(typeVar, constraint);
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
  if (type->hasError()) {
    DEBUG_BAILOUT("Has error");
    return std::nullopt;
  }

  if (TypeVar->getImpl().isKeyPathType()) {
    auto objectTy = type->lookThroughAllOptionalTypes();

    // If contextual type is an existential with a superclass
    // constraint, let's try to infer a key path type from it.
    if (kind == AllowedBindingKind::Subtypes) {
      if (type->isExistentialType()) {
        auto layout = type->getExistentialLayout();
        if (auto superclass = layout.explicitSuperclass) {
          if (superclass->isKnownKeyPathType()) {
            type = superclass;
            objectTy = superclass;
          }
        }
      }
    }

    if (!(objectTy->isKnownKeyPathType() || objectTy->is<AnyFunctionType>())) {
      DEBUG_BAILOUT("Bad key path type (1)");
      return std::nullopt;
    }
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
          DEBUG_BAILOUT("Bad key path type (2)");
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
      recordDelayedBy(constraint);
    }
  }

  if (TypeVar->getImpl().getLocator()) {
    // Don't allow a protocol type to get propagated from the base to the result
    // type of a chain, Result should always be a concrete type which conforms
    // to the protocol inferred for the base.
    if (constraint->getKind() == ConstraintKind::UnresolvedMemberChainBase &&
        kind == AllowedBindingKind::Subtypes && type->is<ProtocolType>()) {
      DEBUG_BAILOUT("Unresolved member chain base");
      return std::nullopt;
    }
  }

  if (constraint->getKind() == ConstraintKind::LValueObject) {
    // Allow l-value type inference from its object type, but
    // not the other way around, that would be handled by constraint
    // simplification.
    if (kind == AllowedBindingKind::Subtypes) {
      if (type->isTypeVariableOrMember()) {
        DEBUG_BAILOUT("Disallowed l-value inference");
        return std::nullopt;
      }

      type = LValueType::get(type);
    } else {
      // Right-hand side of the l-value object constraint can only
      // be bound via constraint simplification when l-value type
      // is inferred or contextually from other constraints.
      DEBUG_BAILOUT("Delayed (2)");
      recordDelayedBy(constraint);
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
        recordAdjacentVar(var, constraint);
        continue;
      }

      containsSelf = true;
    }

    // If inferred type doesn't contain the current type variable,
    // let's mark bindings as delayed until dependent member type
    // is resolved.
    if (!containsSelf)
      recordDelayedBy(constraint);

    DEBUG_BAILOUT("Dependent member");
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
    if (isGenericParameter(TypeVar) && !CS.inSalvageMode()) {
      type = fnTy->withExtInfo(fnTy->getExtInfo().withNoEscape(false));
    }
  }

  // Check whether we can perform this binding.
  if (!checkTypeOfBinding(TypeVar, type)) {
    auto *bindingTypeVar = type->getRValueType()->getAs<TypeVariableType>();

    if (!bindingTypeVar) {
      DEBUG_BAILOUT("Not a type variable");
      return std::nullopt;
    }

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
        recordSubtypeOf(bindingTypeVar, constraint);
      } else {
        assert(kind == AllowedBindingKind::Supertypes);
        recordSupertypeOf(bindingTypeVar, constraint);
      }

      recordAdjacentVar(bindingTypeVar, constraint);
      break;
    }

    case ConstraintKind::Bind:
    case ConstraintKind::BindParam:
    case ConstraintKind::Equal: {
      recordEquivalentTo(bindingTypeVar, constraint);
      recordAdjacentVar(bindingTypeVar, constraint);
      break;
    }

    case ConstraintKind::UnresolvedMemberChainBase: {
      recordEquivalentTo(bindingTypeVar, constraint);

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
        recordAdjacentVar(bindingTypeVar, constraint);
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
        otherTypeVar->getImpl().canBindToLValue()) {
      DEBUG_BAILOUT("LValue mismatch");
      return std::nullopt;
    }
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

#undef DEBUG_BAILOUT

/// Retrieve the set of potential type bindings for the given
/// representative type variable, along with flags indicating whether
/// those types should be opened.
void PotentialBindings::infer(Constraint *constraint) {
  ASSERT(TypeVar);

  if (!Constraints.insert(constraint))
    return;

  ++GenerationNumber;

  // Record the change, if there are active scopes.
  if (CS.solverState)
    CS.recordChange(
        SolverTrail::Change::AddedConstraintToInference(TypeVar, constraint));

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
    auto binding = inferFromRelational(constraint);
    if (!binding)
      break;

    addPotentialBinding(*binding);
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
      recordDelayedBy(constraint);
    }

    break;
  }

  case ConstraintKind::NonisolatedConformsTo:
  case ConstraintKind::ConformsTo:
    // Conformances are applicable only to the types they are
    // placed on. They could be transferred to supertypes
    // but that happens separately.
    if (!isDirectRequirement(CS, TypeVar, constraint))
      break;

    if (constraint->getSecondType()->is<ProtocolType>())
      recordProtocol(constraint);
    break;

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
  case ConstraintKind::ForEachElement:
    // Constraints from which we can't do anything.
    break;

  case ConstraintKind::LiteralConformsTo: {
    // Literal conformances are applicable only to the types they
    // are placed on. They could be transferred to supertypes
    // but that happens separately.
    if (!isDirectRequirement(CS, TypeVar, constraint))
      break;

    inferFromLiteral(constraint);
    break;
  }

  case ConstraintKind::Defaultable:
  case ConstraintKind::FallbackType: {
    // Defaults and fallbacks are applicable only to the types
    // they are associated with. Defaults could be transferred
    // to supertypes but that happens separately.
    if (!isDirectRequirement(CS, TypeVar, constraint))
      break;

    auto newDefault = constraint->getSecondType();
    // Don't record duplicate default types.
    if (llvm::any_of(Defaults, [&](Constraint *existingDefault) {
          return existingDefault->getSecondType()->isEqual(newDefault);
        }))
      break;

    recordDefault(constraint);
    break;
  }

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
        recordDelayedBy(constraint);
        break;
      }
    }

    // This is right-hand side, let's continue.
    break;
  }

  case ConstraintKind::Disjunction:
    recordDelayedBy(constraint);
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
    recordDelayedBy(constraint);
    break;
  }

  case ConstraintKind::ValueMember:
  case ConstraintKind::UnresolvedValueMember:
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
          recordDelayedBy(constraint);
          break;
        }
      } else {
        memberTy = memberTy->getImpl().getRepresentative(/*record=*/nullptr);
      }
    }

    if (memberTy == TypeVar)
      recordDelayedBy(constraint);

    break;
  }

  case ConstraintKind::OneWayEqual:{
    // Don't produce any bindings if this type variable is on the left-hand
    // side of a one-way binding.
    auto firstType = constraint->getFirstType();
    if (auto *tv = firstType->getAs<TypeVariableType>()) {
      if (tv->getImpl().getRepresentative(nullptr) == TypeVar) {
        recordDelayedBy(constraint);
        break;
      }
    }

    break;
  }
  }
}

void PotentialBindings::retract(Constraint *constraint) {
  ASSERT(TypeVar);

  if (!Constraints.remove(constraint))
    return;

  ++GenerationNumber;

  // Record the change, if there are active scopes.
  if (CS.solverState)
    CS.recordChange(SolverTrail::Change::RetractedConstraintFromInference(
        TypeVar, constraint));

  LLVM_DEBUG(
    llvm::dbgs() << Constraints.size() << " " << Bindings.size() << " "
                 << AdjacentVars.size() << " " << DelayedBy.size() << " "
                 << SubtypeOf.size() << " " << SupertypeOf.size() << " "
                 << EquivalentTo.size() << "\n");

  Bindings.erase(
      llvm::remove_if(Bindings,
                      [&](const PotentialBinding &binding) {
                        if (binding.getSource() == constraint) {
                          if (CS.solverState) {
                            CS.recordChange(SolverTrail::Change::RetractedBinding(
                                TypeVar, binding));
                          }
                          return true;
                        }
                        return false;
                      }),
      Bindings.end());

  Literals.erase(
      llvm::remove_if(Literals,
                      [&](const LiteralRequirement &literal) {
                        if (literal.getSource() == constraint) {
                          if (CS.solverState) {
                            CS.recordChange(SolverTrail::Change::RetractedLiteral(
                                TypeVar, constraint));
                          }
                          return true;
                        }
                        return false;
                      }),
      Literals.end());

#define COMMON_BINDING_INFORMATION_RETRACTION(PropertyName, Storage)           \
  Storage.erase(                                                               \
      llvm::remove_if(Storage,                                                 \
                      [&](Constraint *other) {                                 \
                        if (other == constraint) {                             \
                          if (CS.solverState) {                                \
                            CS.recordChange(                                   \
                                SolverTrail::Change::Retracted##PropertyName(  \
                                    TypeVar, constraint));                     \
                          }                                                    \
                          return true;                                         \
                        }                                                      \
                        return false;                                          \
                      }),                                                      \
      Storage.end());

#define BINDING_RELATION_RETRACTION(RelationName, Storage)                     \
  Storage.erase(                                                               \
      llvm::remove_if(Storage,                                                 \
                      [&](std::pair<TypeVariableType *, Constraint *> pair) {  \
                        if (pair.second == constraint) {                       \
                          if (CS.solverState) {                                \
                            CS.recordChange(                                   \
                                SolverTrail::Change::Retracted##RelationName(  \
                                    TypeVar, pair.first, pair.second));        \
                          }                                                    \
                          return true;                                         \
                        }                                                      \
                        return false;                                          \
                      }),                                                      \
      Storage.end());
#include "swift/Sema/CSTrail.def"
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

  TypeVar = nullptr;
  AssociatedCodeCompletionToken = ASTNode();
}

void PotentialBindings::dump(llvm::raw_ostream &out, unsigned indent) const {
  if (TypeVar) {
    out << "Potential bindings for ";
    TypeVar->getImpl().print(out);
    out << "\n";
  } else {
    out << "<<No type variable assigned>>\n";
  }

  out << "generation: " << GenerationNumber << " ";

  out << "[constraints: ";
  interleave(
      Constraints,
      [&](Constraint *constraint) {
        constraint->print(out, &CS.getASTContext().SourceMgr, indent,
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
    interleave(
        adjacentVars,
        [&](std::pair<TypeVariableType *, Constraint *> pair) {
          out << pair.first->getString(PrintOptions::forDebugging());
          if (pair.first->getImpl().getFixedType(/*record=*/nullptr))
            out << " (fixed)";
          out << " via ";
          pair.second->print(out, &CS.getASTContext().SourceMgr, indent,
                             /*skipLocator=*/true);
        },
        [&out]() { out << ", "; });
    out << "] ";
  }
}

void BindingSet::forEachLiteralRequirement(
    llvm::function_ref<void(KnownProtocolKind)> callback) const {
  for (const auto &info : Literals) {
    // Only uncovered defaultable literal protocols participate.
    if (!info.viableAsBinding())
      continue;
    
    if (auto protocolKind = info.getProtocol()->getKnownProtocolKind())
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
    return literal.viableAsBinding();
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
  PrintOptions PO = PrintOptions::forDebugging();

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
  if (isDirty())
    attributes.push_back("dirty");
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
        literal.hasDefaultType()
        ? literal.getDefaultType()
        : Type(),
        literal.viableAsBinding()));
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
        [&](Constraint *constraint) {
          auto defaultBinding =
              PrintableBinding::exact(constraint->getSecondType());
          defaultBinding.print(out, PO);
        },
        [&] { out << ", "; });
    out << "]";
  }
  
}
