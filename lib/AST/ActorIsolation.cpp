//===--- ActorIsolation.cpp -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ActorIsolation.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"

using namespace swift;

ActorIsolation ActorIsolation::forMainActor(ASTContext &ctx) {
  return ActorIsolation::forGlobalActor(
      ctx.getMainActorType()->mapTypeOutOfContext());
}

// These constructors are defined out-of-line so that including ActorIsolation.h
// doesn't require a bunch of other headers to be included.

ActorIsolation::ActorIsolation(Kind kind, NominalTypeDecl *actor,
                               EncodedParameterIndex parameterIndex)
    : actorInstance(actor), kind(kind), isolatedByPreconcurrency(false),
      silParsed(false), encodedParameterIndex(parameterIndex.getOpaqueValue()) {}

ActorIsolation::ActorIsolation(Kind kind, VarDecl *actor,
                               EncodedParameterIndex parameterIndex)
    : actorInstance(actor), kind(kind), isolatedByPreconcurrency(false),
      silParsed(false), encodedParameterIndex(parameterIndex.getOpaqueValue()) {}

ActorIsolation::ActorIsolation(Kind kind, Expr *actor,
                               EncodedParameterIndex parameterIndex)
    : actorInstance(actor), kind(kind), isolatedByPreconcurrency(false),
      silParsed(false), encodedParameterIndex(parameterIndex.getOpaqueValue()) {}

ActorIsolation::ActorIsolation(Kind kind, Type globalActor)
    : globalActor(globalActor), kind(kind), isolatedByPreconcurrency(false),
      silParsed(false), encodedParameterIndex(0) {}

ActorIsolation
ActorIsolation::forActorInstanceParameter(Expr *actor,
                                          unsigned parameterIndex) {
  auto &ctx = actor->getType()->getASTContext();

  // An isolated value of `nil` is statically nonisolated.
  // FIXME: Also allow 'Optional.none'
  if (isa<NilLiteralExpr>(actor))
    return ActorIsolation::forNonisolated(/*unsafe*/ false);

  // An isolated value of `<global actor type>.shared` is statically
  // global actor isolated.
  if (auto *memberRef = dyn_cast<MemberRefExpr>(actor)) {
    // Check that the member declaration witnesses the `shared`
    // requirement of the `GlobalActor` protocol.
    auto declRef = memberRef->getDecl();
    auto baseType = memberRef->getBase()->getType()->getMetatypeInstanceType();
    if (auto globalActor = ctx.getProtocol(KnownProtocolKind::GlobalActor)) {
      auto conformance = checkConformance(baseType, globalActor);
      if (conformance &&
          conformance.getWitnessByName(ctx.Id_shared) == declRef) {
        return ActorIsolation::forGlobalActor(baseType);
      }
    }
  }

  return ActorIsolation(ActorInstance, actor,
                        EncodedParameterIndex::parameter(parameterIndex));
}

ActorIsolation ActorIsolation::forActorInstanceSelf(ValueDecl *decl) {
  if (auto *fn = dyn_cast<AbstractFunctionDecl>(decl))
    return ActorIsolation(ActorInstance, fn->getImplicitSelfDecl(),
                          EncodedParameterIndex::self());

  if (auto *storage = dyn_cast<AbstractStorageDecl>(decl)) {
    if (auto *fn = storage->getAccessor(AccessorKind::Get)) {
      return ActorIsolation(ActorInstance, fn->getImplicitSelfDecl(),
                            EncodedParameterIndex::self());
    }
  }

  auto *dc = decl->getDeclContext();
  return ActorIsolation(ActorInstance, dc->getSelfNominalTypeDecl(),
                        EncodedParameterIndex::self());
}

ActorIsolation ActorIsolation::forActorInstanceSelf(NominalTypeDecl *selfDecl) {
  return ActorIsolation(ActorInstance, selfDecl, EncodedParameterIndex::self());
}

NominalTypeDecl *ActorIsolation::getActor() const {
  assert(getKind() == ActorInstance || getKind() == GlobalActor);

  if (silParsed)
    return nullptr;

  if (getKind() == GlobalActor) {
    return getGlobalActor()->getAnyNominal();
  }

  Type actorType;

  if (auto *instance = actorInstance.dyn_cast<VarDecl *>()) {
    actorType = instance->getTypeInContext();
  } else if (auto *expr = actorInstance.dyn_cast<Expr *>()) {
    actorType = expr->getType()->getRValueType();
  }

  if (actorType) {
    if (auto wrapped = actorType->getOptionalObjectType()) {
      actorType = wrapped;
    }
    return actorType->getReferenceStorageReferent()->getAnyActor();
  }

  return cast<NominalTypeDecl *>(actorInstance);
}

VarDecl *ActorIsolation::getActorInstance() const {
  assert(getKind() == ActorInstance);

  if (silParsed)
    return nullptr;

  return actorInstance.dyn_cast<VarDecl *>();
}

Expr *ActorIsolation::getActorInstanceExpr() const {
  assert(getKind() == ActorInstance);

  if (silParsed)
    return nullptr;

  return actorInstance.dyn_cast<Expr *>();
}

bool ActorIsolation::isMainActor() const {
  if (silParsed)
    return false;

  if (isGlobalActor()) {
    if (auto *nominal = getGlobalActor()->getAnyNominal())
      return nominal->isMainActor();
  }

  return false;
}

bool ActorIsolation::isDistributedActor() const {
  if (silParsed)
    return false;

  if (getKind() != ActorInstance)
    return false;

  return getActor()->isDistributedActor();
}

bool ActorIsolation::isEqual(const ActorIsolation &lhs,
                             const ActorIsolation &rhs) {
  if (lhs.getKind() != rhs.getKind())
    return false;

  switch (lhs.getKind()) {
  case Nonisolated:
  case NonisolatedUnsafe:
  case Unspecified:
    return true;

  case Erased:
    // Different functions with erased isolation have the same *kind* of
    // isolation, but we must generally assume that they're not isolated
    // the *same way*, which is what this function is apparently supposed
    // to answer.
    return false;

  case CallerIsolationInheriting:
    // This returns false for the same reason as erased. The caller has to check
    // against the actual caller isolation.
    return false;

  case ActorInstance: {
    auto *lhsActor = lhs.getActorInstance();
    auto *rhsActor = rhs.getActorInstance();
    if (lhsActor && rhsActor) {
      if (lhsActor == rhsActor)
        return true;

      // FIXME: This won't work for arbitrary isolated parameter captures.
      if ((lhsActor->isSelfParameter() && rhsActor->isSelfParamCapture()) ||
          (lhsActor->isSelfParamCapture() && rhsActor->isSelfParameter())) {
        return true;
      }
    }

    // The parameter index doesn't matter; only the actor instance
    // values must be equal.
    return (lhs.getActor() == rhs.getActor() &&
            lhs.actorInstance == rhs.actorInstance);
  }

  case GlobalActor:
    return areTypesEqual(lhs.globalActor, rhs.globalActor);
  }
}
