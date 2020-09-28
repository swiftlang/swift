//===--- DerivedConformanceEquatableHashable.cpp - Derived Equatable & co -===//
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
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "swift/AST/ParameterList.h"

using namespace swift;

bool DerivedConformance::canDeriveActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isActor() && dc == nominal;
}

static DeclName getEnqueuePartialTaskName(ASTContext &ctx) {
  return DeclName(ctx, ctx.Id_enqueue, { ctx.Id_partialTask });
}

static Type getPartialAsyncTaskType(ASTContext &ctx) {
  auto concurrencyModule = ctx.getLoadedModule(ctx.Id_Concurrency);
  if (!concurrencyModule)
    return Type();

  SmallVector<ValueDecl *, 2> decls;
  concurrencyModule->lookupQualified(
      concurrencyModule, DeclNameRef(ctx.Id_PartialAsyncTask),
      NL_QualifiedDefault, decls);
  for (auto decl : decls) {
    if (auto typeDecl = dyn_cast<TypeDecl>(decl))
      return typeDecl->getDeclaredInterfaceType();
  }

  return Type();
}

static std::pair<BraceStmt *, bool>
deriveBodyActor_enqueuePartialTask(
  AbstractFunctionDecl *enqueuePartialTask, void *) {
  ASTContext &ctx = enqueuePartialTask->getASTContext();

  // FIXME: Call into runtime API to enqueue the task, once we figure out
  // what that runtime API should look like.

  auto body = BraceStmt::create(
      ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

/// Derive the declaration of Actor's enqueue(partialTask:).
static ValueDecl *deriveActor_enqueuePartialTask(DerivedConformance &derived) {
  ASTContext &ctx = derived.Context;

  Type partialTaskType = getPartialAsyncTaskType(ctx);
  if (!partialTaskType) {
    derived.Nominal->diagnose(diag::partial_task_type_missing);
    return nullptr;
  }

  auto parentDC = derived.getConformanceContext();
  auto partialTaskParamDecl = new (ctx) ParamDecl(
      SourceLoc(), SourceLoc(), ctx.Id_partialTask,
      SourceLoc(), ctx.Id_partialTask, parentDC);
  partialTaskParamDecl->setInterfaceType(partialTaskType);
  partialTaskParamDecl->setSpecifier(ParamSpecifier::Default);

  ParameterList *params = ParameterList::createWithoutLoc(partialTaskParamDecl);
  auto func = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, getEnqueuePartialTaskName(ctx),
      SourceLoc(), /*Async=*/false, /*Throws=*/false, /*GenericParams=*/nullptr,
      params, TupleType::getEmpty(ctx), parentDC);
  func->copyFormalAccessFrom(derived.Nominal);
  func->setBodySynthesizer(deriveBodyActor_enqueuePartialTask);

  // FIXME: This function should be "actor-unsafe", not "actor-independent", but
  // the latter is all we have at the moment.
  func->getAttrs().add(new (ctx) ActorIndependentAttr(/*IsImplicit=*/true));

  derived.addMembersToConformanceContext({func});
  return func;
}

ValueDecl *DerivedConformance::deriveActor(ValueDecl *requirement) {
  auto func = dyn_cast<FuncDecl>(requirement);
  if (!func)
    return nullptr;

  if (func->getName() == getEnqueuePartialTaskName(Context))
    return deriveActor_enqueuePartialTask(*this);

  return nullptr;
}
