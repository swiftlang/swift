//===--- DerivedConformanceActor.cpp - Derived Actor Conformance ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements implicit derivation of the Actor protocol.
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}
// ==== ------------------------------------------------------------------------

// TODO: deduplicate with 'declareDerivedProperty' from DerivedConformance...
std::pair<VarDecl *, PatternBindingDecl *>
createStoredProperty(ClassDecl *classDecl, ASTContext &ctx,
                     VarDecl::Introducer introducer, Identifier name,
                     Type propertyInterfaceType, Type propertyContextType,
                     bool isStatic, bool isFinal) {
  auto parentDC = classDecl;

  VarDecl *propDecl = new (ctx)
      VarDecl(/*IsStatic*/ isStatic, introducer,
                           SourceLoc(), name, parentDC);
  propDecl->setImplicit();
  propDecl->setSynthesized();
  propDecl->copyFormalAccessFrom(classDecl, /*sourceIsParentContext*/ true);
  propDecl->setInterfaceType(propertyInterfaceType);

  Pattern *propPat = NamedPattern::createImplicit(ctx, propDecl);
  propPat->setType(propertyContextType);

  propPat = TypedPattern::createImplicit(ctx, propPat, propertyContextType);
  propPat->setType(propertyContextType);

  auto *pbDecl = PatternBindingDecl::createImplicit(
      ctx, StaticSpellingKind::None, propPat, /*InitExpr*/ nullptr,
      parentDC);
  return {propDecl, pbDecl};
}

static ValueDecl *deriveDistributedActor_id(DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // @_distributedActorIndependent
  // let id: AnyActorIdentity
  // ```
  auto propertyType = C.getAnyActorIdentityDecl()->getDeclaredInterfaceType();

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_id,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as @_distributedActorIndependent, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) DistributedActorIndependentAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}

static ValueDecl *deriveDistributedActor_actorTransport(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // @_distributedActorIndependent
  // let actorTransport: ActorTransport
  // ```
  // (no need for @actorIndependent because it is an immutable let)
  auto propertyType = C.getActorTransportDecl()->getDeclaredInterfaceType();

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_actorTransport,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as @_distributedActorIndependent, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) DistributedActorIndependentAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}


// ==== ------------------------------------------------------------------------

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  if (auto var = dyn_cast<VarDecl>(requirement)) {
    if (var->getName() == Context.Id_id)
      return deriveDistributedActor_id(*this);

    if (var->getName() == Context.Id_actorTransport)
      return deriveDistributedActor_actorTransport(*this);
  }

  return nullptr;
}
