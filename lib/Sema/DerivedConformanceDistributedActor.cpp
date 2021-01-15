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

bool DerivedConformance::canDeriveDistributedActor(
    NominalTypeDecl *nominal, DeclContext *dc) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  return classDecl && classDecl->isDistributedActor() && dc == nominal;
}

//static DeclName getEnqueuePartialTaskName(ASTContext &ctx) {
//  return DeclName(ctx, ctx.Id_enqueue, { ctx.Id_partialTask });
//}
//
//static Type getPartialAsyncTaskType(ASTContext &ctx) {
//  auto concurrencyModule = ctx.getLoadedModule(ctx.Id_Concurrency);
//  if (!concurrencyModule)
//    return Type();
//
//  SmallVector<ValueDecl *, 2> decls;
//  concurrencyModule->lookupQualified(
//      concurrencyModule, DeclNameRef(ctx.Id_PartialAsyncTask),
//      NL_QualifiedDefault, decls);
//  for (auto decl : decls) {
//    if (auto typeDecl = dyn_cast<TypeDecl>(decl))
//      return typeDecl->getDeclaredInterfaceType();
//  }
//
//  return Type();
//}
//
///// Look for the default enqueue operation.
//static FuncDecl *getDefaultActorEnqueue(DeclContext *dc, SourceLoc loc) {
//  ASTContext &ctx = dc->getASTContext();
//  auto desc = UnqualifiedLookupDescriptor(
//      DeclNameRef(ctx.Id__defaultActorEnqueue),
//      dc, loc, UnqualifiedLookupOptions());
//  auto lookup =
//      evaluateOrDefault(ctx.evaluator, UnqualifiedLookupRequest{desc}, {});
//  for (const auto &result : lookup) {
//    // FIXME: Validate this further, because we're assuming the exact type.
//    if (auto func = dyn_cast<FuncDecl>(result.getValueDecl()))
//      return func;
//  }
//
//  return nullptr;
//}
//
//static std::pair<BraceStmt *, bool>
//deriveBodyActor_enqueuePartialTask(
//  AbstractFunctionDecl *enqueuePartialTask, void *) {
//  // func enqueue(partialTask: PartialAsyncTask) {
//  //   _defaultActorEnqueue(partialTask: partialTask, actor: self)
//  // }
//  ASTContext &ctx = enqueuePartialTask->getASTContext();
//  auto classDecl = enqueuePartialTask->getDeclContext()->getSelfClassDecl();
//
//  // Produce an empty brace statement on failure.
//  auto failure = [&]() -> std::pair<BraceStmt *, bool> {
//    auto body = BraceStmt::create(
//        ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
//    return { body, /*isTypeChecked=*/true };
//  };
//
//  // Call into the runtime to enqueue the task.
//  auto fn = getDefaultActorEnqueue(classDecl, classDecl->getLoc());
//  if (!fn) {
//    classDecl->diagnose(
//        diag::concurrency_lib_missing, ctx.Id__defaultActorEnqueue.str());
//    return failure();
//  }
//
//  // Reference to _defaultActorEnqueue.
//  auto fnRef = new (ctx) DeclRefExpr(fn, DeclNameLoc(), /*Implicit=*/true);
//  fnRef->setType(fn->getInterfaceType());
//
//  // self argument to the function.
//  auto selfDecl = enqueuePartialTask->getImplicitSelfDecl();
//  Type selfType = enqueuePartialTask->mapTypeIntoContext(
//      selfDecl->getValueInterfaceType());
//  Expr *selfArg = new (ctx) DeclRefExpr(
//      selfDecl, DeclNameLoc(), /*Implicit=*/true, AccessSemantics::Ordinary,
//      selfType);
//  selfArg = ErasureExpr::create(ctx, selfArg, ctx.getAnyObjectType(), { });
//  selfArg->setImplicit();
//
//  // The partial asynchronous task.
//  auto partialTaskParam = enqueuePartialTask->getParameters()->get(0);
//  Expr *partialTask = new (ctx) DeclRefExpr(
//      partialTaskParam, DeclNameLoc(), /*Implicit=*/true,
//      AccessSemantics::Ordinary,
//      enqueuePartialTask->mapTypeIntoContext(
//        partialTaskParam->getValueInterfaceType()));
//
//  // Form the call itself.
//  auto call = CallExpr::createImplicit(
//      ctx, fnRef, { partialTask, selfArg },
//      { ctx.Id_partialTask, ctx.getIdentifier("actor") });
//  call->setType(fn->getResultInterfaceType());
//  call->setThrows(false);
//
//  auto body = BraceStmt::create(
//      ctx, SourceLoc(), { call }, SourceLoc(), /*implicit=*/true);
//  return { body, /*isTypeChecked=*/true };
//}
//

/// Derive the declaration of Actor's actorTransport.
static ValueDecl *deriveDistributedActor_initializer(DerivedConformance &derived) {
  ASTContext &ctx = derived.Context;

  fprintf(stderr, "[%s:%d] >> TODO: SYNTHESIZE (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
  // TODO: synthesize the initializer accepting the transport,
  // - store the transport as actorTransport
  // - invoke the transport to allocate an address, store it as actorAddress

  return nullptr;
}

/// Derive the declaration of Actor's actorTransport.
static ValueDecl *deriveDistributedActor_actorTransport(DerivedConformance &derived) {
  ASTContext &ctx = derived.Context;

  fprintf(stderr, "[%s:%d] >> TODO: SYNTHESIZE (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
  // TODO: actually implement the transport field
  return nullptr;
}

/// Derive the declaration of Actor's actorAddress.
static ValueDecl *deriveDistributedActor_actorAddress(DerivedConformance &derived) {
  ASTContext &ctx = derived.Context;

  fprintf(stderr, "[%s:%d] >> TODO: SYNTHESIZE (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
  // TODO: actually implement the address field
  return nullptr;
}

ValueDecl *DerivedConformance::deriveDistributedActor(ValueDecl *requirement) {
  // Synthesize properties
  auto var = dyn_cast<VarDecl>(requirement);
  if (var) {
    requirement->dump();

    if (VarDecl::isDistributedActorTransportName(Context, var->getName())) {
      fprintf(stderr, "[%s:%d] >> (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
      return deriveDistributedActor_actorTransport(*this);
    }

    if (VarDecl::isDistributedActorAddressName(Context, var->getName())) {
      fprintf(stderr, "[%s:%d] >> (%s)  \n", __FILE__, __LINE__, __FUNCTION__);
      return deriveDistributedActor_actorAddress(*this);
    }
  }

  // Synthesize functions
  auto func = dyn_cast<FuncDecl>(requirement);
  if (func) {
    // TODO: derive encode impl
  }

  // Synthesize initializers
  // TODO: derive init(from:) impl

   return nullptr;
}
