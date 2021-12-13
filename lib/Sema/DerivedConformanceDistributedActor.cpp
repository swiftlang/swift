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
#include "TypeCheckDistributed.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}

/******************************************************************************/
/******************************* RESOLVE FUNCTION *****************************/
/******************************************************************************/

/// Synthesizes the
///
/// \verbatim
/// static resolve(_ address: ActorAddress,
///                using system: DistributedActorSystem) throws -> Self {
///   <filled in by SILGenDistributed>
/// }
/// \endverbatim
///
/// factory function in the AST, with an empty body. Its body is
/// expected to be filled-in during SILGen.
static FuncDecl *deriveDistributedActor_resolve(DerivedConformance &derived) {
  auto decl = dyn_cast<ClassDecl>(derived.Nominal);
  assert(decl->isDistributedActor());
  auto &C = decl->getASTContext();

  auto mkParam = [&](Identifier argName, Identifier paramName, Type ty) -> ParamDecl* {
    auto *param = new (C) ParamDecl(SourceLoc(),
                                    SourceLoc(), argName,
                                    SourceLoc(), paramName, decl);
    param->setImplicit();
    param->setSpecifier(ParamSpecifier::Default);
    param->setInterfaceType(ty);
    return param;
  };

  auto idType = getDistributedActorIDType(decl);
  auto actorSystemType = getDistributedActorSystemType(decl);

  // (id: Self.ID, using system: Self.ActorSystem)
  auto *params = ParameterList::create(
      C,
      /*LParenLoc=*/SourceLoc(),
      /*params=*/{ mkParam(C.Id_id, C.Id_id, idType),
                   mkParam(C.Id_using, C.Id_system, actorSystemType)
      },
      /*RParenLoc=*/SourceLoc()
  );

  // Func name: resolve(id:using:)
  DeclName name(C, C.Id_resolve, params);

  // Expected type: (Self) -> (Self.ID, Self.ActorSystem) throws -> (Self)
  auto *factoryDecl =
      FuncDecl::createImplicit(C, StaticSpellingKind::KeywordStatic,
                               name, SourceLoc(),
                               /*async=*/false,
                               /*throws=*/true,
                               /*genericParams=*/nullptr,
                               params,
                               /*returnType*/decl->getDeclaredInterfaceType(),
                               decl);

  factoryDecl->setDistributedActorFactory(); // TODO(distributed): should we mark this specifically as the resolve factory?
  factoryDecl->copyFormalAccessFrom(decl, /*sourceIsParentContext=*/true);

  derived.addMembersToConformanceContext({factoryDecl});
  return factoryDecl;
}

/******************************************************************************/
/******************************* PROPERTIES ***********************************/
/******************************************************************************/

// TODO(distributed): make use of this after all, but FORCE it?
static ValueDecl *deriveDistributedActor_id(DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // nonisolated
  // let id: Self.ID // Self.ActorSystem.ActorID
  // ```
  auto propertyType = getDistributedActorIDType(derived.Nominal);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_id,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}

static ValueDecl *deriveDistributedActor_actorSystem(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // ```
  // nonisolated
  // let actorSystem: ActorSystem
  // ```
  // (no need for @actorIndependent because it is an immutable let)
  auto propertyType = getDistributedActorSystemType(derived.Nominal);

  VarDecl *propDecl;
  PatternBindingDecl *pbDecl;
  std::tie(propDecl, pbDecl) = derived.declareDerivedProperty(
      C.Id_actorSystem,
      propertyType, propertyType,
      /*isStatic=*/false, /*isFinal=*/true);

  propDecl->setIntroducer(VarDecl::Introducer::Let);

  // mark as nonisolated, allowing access to it from everywhere
  propDecl->getAttrs().add(
      new (C) NonisolatedAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({ propDecl, pbDecl });
  return propDecl;
}

/******************************************************************************/
/***************************** ASSOC TYPES ************************************/
/******************************************************************************/

static Type
deriveDistributedActorType_ActorSystem(
    DerivedConformance &derived) {
  assert(derived.Nominal->isDistributedActor());
  auto &C = derived.Context;

  // Look for a type DefaultDistributedActorSystem within the parent context.
  auto defaultDistributedActorSystemLookup = TypeChecker::lookupUnqualified(
      derived.getConformanceContext()->getModuleScopeContext(),
      DeclNameRef(C.Id_DefaultDistributedActorSystem),
      derived.ConformanceDecl->getLoc());
  TypeDecl *defaultDistributedActorSystemTypeDecl = nullptr;
  for (const auto &found : defaultDistributedActorSystemLookup) {
    if (auto foundType = dyn_cast_or_null<TypeDecl>(found.getValueDecl())) {
      if (defaultDistributedActorSystemTypeDecl) {
        // Note: ambiguity, for now just fail.
        return nullptr;
      }

      defaultDistributedActorSystemTypeDecl = foundType;
      continue;
    }
  }

  // There is no default, so fail to synthesize.
  if (!defaultDistributedActorSystemTypeDecl)
    return nullptr;

  // Return the default system type.
  return defaultDistributedActorSystemTypeDecl->getDeclaredInterfaceType();
}

/******************************************************************************/
/**************************** ENTRY POINTS ************************************/
/******************************************************************************/

// IMPORTANT: Remember to update DerivedConformance::getDerivableRequirement
//            any time the signatures or list of derived requirements change.

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  if (auto var = dyn_cast<VarDecl>(requirement)) {
    if (var->getName() == Context.Id_id)
      return deriveDistributedActor_id(*this);

    if (var->getName() == Context.Id_actorSystem)
      return deriveDistributedActor_actorSystem(*this);
  }

  if (auto func = dyn_cast<FuncDecl>(requirement)) {
    // just a simple name check is enough here,
    // if we are invoked here we know for sure it is for the "right" function
    if (func->getName().getBaseName() == Context.Id_resolve) {
      return deriveDistributedActor_resolve(*this);
    }
  }

  return nullptr;
}

std::pair<Type, TypeDecl *> DerivedConformance::deriveDistributedActor(
    AssociatedTypeDecl *assocType) {
  if (!canDeriveDistributedActor(Nominal, cast<DeclContext>(ConformanceDecl)))
    return std::make_pair(Type(), nullptr);

  if (assocType->getName() == Context.Id_ActorSystem) {
    return std::make_pair(deriveDistributedActorType_ActorSystem(*this), nullptr);
  }

  // TODO(distributed): maybe also do Self.ID here?

  Context.Diags.diagnose(assocType->getLoc(),
                         diag::broken_distributed_actor_requirement);
  return std::make_pair(Type(), nullptr);
}

/******************************************************************************/
/*************************** ERRORS & DIAGNOSTICS *****************************/
/******************************************************************************/

void DerivedConformance::tryDiagnoseFailedDistributedActorDerivation(
        DeclContext *DC, NominalTypeDecl *nominal) {
  // TODO: offer better diagnosis for error scenarios here
}
