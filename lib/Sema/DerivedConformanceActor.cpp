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
#include "DerivedConformances.h"
#include "TypeChecker.h"
#include "TypeCheckConcurrency.h"
#include "swift/AST/NameLookupRequests.h"
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

/// Look for the default enqueue operation.
static FuncDecl *getDefaultActorEnqueue(DeclContext *dc, SourceLoc loc) {
  ASTContext &ctx = dc->getASTContext();
  auto desc = UnqualifiedLookupDescriptor(
      DeclNameRef(ctx.Id__defaultActorEnqueue),
      dc, loc, UnqualifiedLookupOptions());
  auto lookup =
      evaluateOrDefault(ctx.evaluator, UnqualifiedLookupRequest{desc}, {});
  for (const auto &result : lookup) {
    // FIXME: Validate this further, because we're assuming the exact type.
    if (auto func = dyn_cast<FuncDecl>(result.getValueDecl()))
      return func;
  }

  return nullptr;
}

static std::pair<BraceStmt *, bool>
deriveBodyActor_enqueuePartialTask(
  AbstractFunctionDecl *enqueuePartialTask, void *) {
  // func enqueue(partialTask: PartialAsyncTask) {
  //   _defaultActorEnqueue(partialTask: partialTask, actor: self)
  // }
  ASTContext &ctx = enqueuePartialTask->getASTContext();
  auto classDecl = enqueuePartialTask->getDeclContext()->getSelfClassDecl();

  // Produce an empty brace statement on failure.
  auto failure = [&]() -> std::pair<BraceStmt *, bool> {
    auto body = BraceStmt::create(
        ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
    return { body, /*isTypeChecked=*/true };
  };

  // Call into the runtime to enqueue the task.
  auto fn = getDefaultActorEnqueue(classDecl, classDecl->getLoc());
  if (!fn) {
    classDecl->diagnose(
        diag::concurrency_lib_missing, ctx.Id__defaultActorEnqueue.str());
    return failure();
  }

  // Reference to _defaultActorEnqueue.
  auto fnRef = new (ctx) DeclRefExpr(fn, DeclNameLoc(), /*Implicit=*/true);
  fnRef->setType(fn->getInterfaceType());

  // self argument to the function.
  auto selfDecl = enqueuePartialTask->getImplicitSelfDecl();
  Type selfType = enqueuePartialTask->mapTypeIntoContext(
      selfDecl->getValueInterfaceType());
  Expr *selfArg = new (ctx) DeclRefExpr(
      selfDecl, DeclNameLoc(), /*Implicit=*/true, AccessSemantics::Ordinary,
      selfType);
  selfArg = ErasureExpr::create(ctx, selfArg, ctx.getAnyObjectType(), { });
  selfArg->setImplicit();

  // The partial asynchronous task.
  auto partialTaskParam = enqueuePartialTask->getParameters()->get(0);
  Expr *partialTask = new (ctx) DeclRefExpr(
      partialTaskParam, DeclNameLoc(), /*Implicit=*/true,
      AccessSemantics::Ordinary,
      enqueuePartialTask->mapTypeIntoContext(
        partialTaskParam->getValueInterfaceType()));

  // Form the call itself.
  auto call = CallExpr::createImplicit(
      ctx, fnRef, { partialTask, selfArg },
      { ctx.Id_partialTask, ctx.getIdentifier("actor") });
  call->setType(fn->getResultInterfaceType());
  call->setThrows(false);

  auto body = BraceStmt::create(
      ctx, SourceLoc(), { call }, SourceLoc(), /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

/// Derive the declaration of Actor's enqueue(partialTask:).
static ValueDecl *deriveActor_enqueuePartialTask(DerivedConformance &derived) {
  ASTContext &ctx = derived.Context;

  // Retrieve the types and declarations we'll need to form this operation.
  Type partialTaskType = getPartialAsyncTaskType(ctx);
  if (!partialTaskType) {
    derived.Nominal->diagnose(
        diag::concurrency_lib_missing, ctx.Id_PartialAsyncTask.str());
    return nullptr;
  }

  auto parentDC = derived.getConformanceContext();

  // Partial task parameter to enqueue(partialTask:).
  auto partialTaskParamDecl = new (ctx) ParamDecl(
      SourceLoc(), SourceLoc(), ctx.Id_partialTask,
      SourceLoc(), ctx.Id_partialTask, parentDC);
  partialTaskParamDecl->setInterfaceType(partialTaskType);
  partialTaskParamDecl->setSpecifier(ParamSpecifier::Default);

  // enqueue(partialTask:) method.
  ParameterList *params = ParameterList::createWithoutLoc(partialTaskParamDecl);
  auto func = FuncDecl::createImplicit(
      ctx, StaticSpellingKind::None, getEnqueuePartialTaskName(ctx),
      SourceLoc(), /*Async=*/false, /*Throws=*/false, /*GenericParams=*/nullptr,
      params, TupleType::getEmpty(ctx), parentDC);
  func->copyFormalAccessFrom(derived.Nominal);
  func->setBodySynthesizer(deriveBodyActor_enqueuePartialTask);
  func->setSynthesized();
  // mark as @actorIndependent(unsafe)
  func->getAttrs().add(new (ctx) ActorIndependentAttr(
                            ActorIndependentKind::Unsafe, /*IsImplicit=*/true));

  derived.addMembersToConformanceContext(
      { func });
  return func;
}

ValueDecl *DerivedConformance::deriveActor(ValueDecl *requirement) {
  auto func = dyn_cast<FuncDecl>(requirement);
  if (!func)
    return nullptr;

  if (FuncDecl::isEnqueuePartialTaskName(Context, func->getName()))
    return deriveActor_enqueuePartialTask(*this);

  return nullptr;
}
