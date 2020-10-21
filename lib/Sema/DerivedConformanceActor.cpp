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

/// Look for the default actor queue type.
static Type getDefaultActorQueueType(DeclContext *dc, SourceLoc loc) {
  ASTContext &ctx = dc->getASTContext();
  UnqualifiedLookupOptions options;
  options |= UnqualifiedLookupFlags::TypeLookup;
  auto desc = UnqualifiedLookupDescriptor(
      DeclNameRef(ctx.getIdentifier("_DefaultActorQueue")), dc, loc, options);
  auto lookup =
      evaluateOrDefault(ctx.evaluator, UnqualifiedLookupRequest{desc}, {});
  for (const auto &result : lookup) {
    if (auto typeDecl = dyn_cast<TypeDecl>(result.getValueDecl()))
      return typeDecl->getDeclaredInterfaceType();
  }

  return Type();
}

/// Look for the initialization function for the default actor storage.
static FuncDecl *getDefaultActorQueueCreate(DeclContext *dc, SourceLoc loc) {
  ASTContext &ctx = dc->getASTContext();
  auto desc = UnqualifiedLookupDescriptor(
      DeclNameRef(ctx.getIdentifier("_defaultActorQueueCreate")), dc, loc,
      UnqualifiedLookupOptions());
  auto lookup =
      evaluateOrDefault(ctx.evaluator, UnqualifiedLookupRequest{desc}, {});
  for (const auto &result : lookup) {
    // FIXME: Validate this further, because we're assuming the exact type.
    if (auto func = dyn_cast<FuncDecl>(result.getValueDecl()))
      return func;
  }

  return nullptr;
}

/// Look for the default enqueue operation.
static FuncDecl *getDefaultActorQueueEnqueue(DeclContext *dc, SourceLoc loc) {
  ASTContext &ctx = dc->getASTContext();
  auto desc = UnqualifiedLookupDescriptor(
      DeclNameRef(ctx.getIdentifier("_defaultActorQueueEnqueuePartialTask")),
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
  //   _defaultActorQueueEnqueuePartialTask(
  //     actor: self, queue: &self.$__actor_storage, partialTask: partialTask)
  // }
  ASTContext &ctx = enqueuePartialTask->getASTContext();

  // Dig out the $__actor_storage property.
  auto classDecl = enqueuePartialTask->getDeclContext()->getSelfClassDecl();
  VarDecl *storageVar = nullptr;
  for (auto decl : classDecl->lookupDirect(ctx.Id_actorStorage)) {
    storageVar = dyn_cast<VarDecl>(decl);
    if (storageVar)
      break;
  }

  // Produce an empty brace statement on failure.
  auto failure = [&]() -> std::pair<BraceStmt *, bool> {
    auto body = BraceStmt::create(
        ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
    return { body, /*isTypeChecked=*/true };
  };

  if (!storageVar) {
    classDecl->diagnose(
        diag::concurrency_lib_missing, ctx.Id_actorStorage.str());
    return failure();
  }

  // Call into the runtime to enqueue the task.
  auto fn = getDefaultActorQueueEnqueue(classDecl, classDecl->getLoc());
  if (!fn) {
    classDecl->diagnose(
        diag::concurrency_lib_missing, "_defaultActorQueueEnqueuePartialTask");
    return failure();
  }

  // Reference to _defaultActorQueueEnqueuePartialTask.
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

  // Address of the actor storage.
  auto module = classDecl->getModuleContext();
  Expr *selfBase = new (ctx) DeclRefExpr(
      selfDecl, DeclNameLoc(), /*Implicit=*/true, AccessSemantics::Ordinary,
      selfType);
  SubstitutionMap storageVarSubs = classDecl->getDeclaredTypeInContext()
      ->getMemberSubstitutionMap(module, storageVar);
  ConcreteDeclRef storageVarDeclRef(storageVar, storageVarSubs);
  Type storageVarType = classDecl->mapTypeIntoContext(
      storageVar->getValueInterfaceType());
  Type storageVarRefType = LValueType::get(storageVarType);
  Expr *storageVarRefExpr = new (ctx) MemberRefExpr(
      selfBase, SourceLoc(), storageVarDeclRef, DeclNameLoc(),
      /*Implicit=*/true);
  storageVarRefExpr->setType(storageVarRefType);
  storageVarRefExpr = new (ctx) InOutExpr(
      SourceLoc(), storageVarRefExpr, storageVarType, /*isImplicit=*/true);

  // The partial asynchronous task.
  auto partialTaskParam = enqueuePartialTask->getParameters()->get(0);
  Expr *partialTask = new (ctx) DeclRefExpr(
      partialTaskParam, DeclNameLoc(), /*Implicit=*/true,
      AccessSemantics::Ordinary,
      enqueuePartialTask->mapTypeIntoContext(
        partialTaskParam->getValueInterfaceType()));

  // Form the call itself.
  auto call = CallExpr::createImplicit(
      ctx, fnRef, { selfArg, storageVarRefExpr, partialTask },
      { ctx.getIdentifier("actor"), ctx.getIdentifier("queue"),
        ctx.Id_partialTask });
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
  Type defaultActorQueueType = getDefaultActorQueueType(
      parentDC, derived.ConformanceDecl->getLoc());
  if (!defaultActorQueueType) {
    derived.Nominal->diagnose(
        diag::concurrency_lib_missing, "_DefaultActorQueue");
    return nullptr;
  }

  auto actorStorageCreateFn = getDefaultActorQueueCreate(
      parentDC, derived.ConformanceDecl->getLoc());
  if (!actorStorageCreateFn) {
    derived.Nominal->diagnose(
        diag::concurrency_lib_missing, "_defaultActorQueueCreate");
    return nullptr;
  }

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

  // Actor storage property and its initialization.
  auto actorStorage = new (ctx) VarDecl(
      /*isStatic=*/false, VarDecl::Introducer::Var, SourceLoc(),
      ctx.Id_actorStorage, parentDC);
  actorStorage->setInterfaceType(defaultActorQueueType);
  actorStorage->setImplicit();
  actorStorage->setAccess(AccessLevel::Private);
  actorStorage->getAttrs().add(new (ctx) FinalAttr(/*Implicit=*/true));

  // Pattern binding to initialize the actor storage.
  Pattern *actorStoragePattern = NamedPattern::createImplicit(
      ctx, actorStorage);
  actorStoragePattern = TypedPattern::createImplicit(
      ctx, actorStoragePattern, defaultActorQueueType);

  // Initialization expression.
  // FIXME: We want the equivalent of type(of: self) here, but we cannot refer
  // to self, so for now we use the static type instead.
  Type nominalType = derived.Nominal->getDeclaredTypeInContext();
  Expr *metatypeArg = TypeExpr::createImplicit(nominalType, ctx);
  Type anyObjectMetatype = ExistentialMetatypeType::get(ctx.getAnyObjectType());
  metatypeArg = ErasureExpr::create(ctx, metatypeArg, anyObjectMetatype, { });
  Expr *actorStorageCreateFnRef = new (ctx) DeclRefExpr(
      actorStorageCreateFn, DeclNameLoc(), /*Implicit=*/true);
  actorStorageCreateFnRef->setType(actorStorageCreateFn->getInterfaceType());

  auto actorStorageInit = CallExpr::createImplicit(
      ctx, actorStorageCreateFnRef, { metatypeArg}, { Identifier() });
  actorStorageInit->setType(actorStorageCreateFn->getResultInterfaceType());
  actorStorageInit->setThrows(false);

  auto actorStoragePatternBinding = PatternBindingDecl::createImplicit(
      ctx, StaticSpellingKind::None, actorStoragePattern, actorStorageInit,
      parentDC);
  actorStoragePatternBinding->setInitializerChecked(0);

  derived.addMembersToConformanceContext(
      { func, actorStorage, actorStoragePatternBinding });
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
