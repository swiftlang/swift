//===--- DerivedConformanceActor.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements explicit derivation of the Actor protocol
// for actor types.
//
// Swift Evolution pitch thread:
// https://forums.swift.org/t/support-custom-executors-in-swift-concurrency/44425
//
//===----------------------------------------------------------------------===//

#include "CodeSynthesis.h"
#include "TypeChecker.h"
#include "swift/Strings.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/Module.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "DerivedConformances.h"

using namespace swift;

bool DerivedConformance::canDeriveActor(DeclContext *dc,
                                        NominalTypeDecl *nominal) {
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  auto result = classDecl && classDecl->isActor() && dc == nominal;
  ; // NOTE: we always synthesize, but we may find an existing member
//  &&
  fprintf(stderr, "[%s:%d](%s) can derive actor? [%s]: %s\n", __FILE_NAME__, __LINE__, __FUNCTION__, nominal->getName().str().str().c_str(),
          result ? "yes" : "no");
//        !classDecl->getUnownedExecutorProperty();
  return result;
}

/// Turn a Builtin.Executor value into an UnownedSerialExecutor.
static Expr *constructUnownedSerialExecutor(ASTContext &ctx,
                                            Expr *arg) {
  auto executorDecl = ctx.getUnownedSerialExecutorDecl();
  if (!executorDecl) return nullptr;

  for (auto member: executorDecl->getAllMembers()) {
    auto ctor = dyn_cast<ConstructorDecl>(member);
    if (!ctor) continue;
    auto params = ctor->getParameters();
    if (params->size() != 1 ||
        !params->get(0)->getInterfaceType()->is<BuiltinExecutorType>())
      continue;

    Type executorType = executorDecl->getDeclaredInterfaceType();

    Type ctorType = ctor->getInterfaceType();

    // We have the right initializer. Build a reference to it of type:
    //   (UnownedSerialExecutor.Type)
    //      -> (Builtin.Executor) -> UnownedSerialExecutor
    auto initRef = new (ctx) DeclRefExpr(ctor, DeclNameLoc(), /*implicit*/true,
                                         AccessSemantics::Ordinary,
                                         ctorType);

    // Apply the initializer to the metatype, building an expression of type:
    //   (Builtin.Executor) -> UnownedSerialExecutor
    auto metatypeRef = TypeExpr::createImplicit(executorType, ctx);
    Type ctorAppliedType = ctorType->getAs<FunctionType>()->getResult();
    auto selfApply = ConstructorRefCallExpr::create(ctx, initRef, metatypeRef,
                                                    ctorAppliedType);
    selfApply->setImplicit(true);
    selfApply->setThrows(false);

    // Call the constructor, building an expression of type
    // UnownedSerialExecutor.
    auto *argList = ArgumentList::forImplicitUnlabeled(ctx, {arg});
    auto call = CallExpr::createImplicit(ctx, selfApply, argList);
    call->setType(executorType);
    call->setThrows(false);
    return call;
  }

  return nullptr;
}

static std::pair<BraceStmt *, bool>
deriveBodyActor_unownedExecutor(AbstractFunctionDecl *getter, void *) {
  // var unownedExecutor: UnownedSerialExecutor {
  //   get {
  //     return Builtin.buildDefaultActorExecutorRef(self)
  //   }
  // }
  ASTContext &ctx = getter->getASTContext();

  // Produce an empty brace statement on failure.
  auto failure = [&]() -> std::pair<BraceStmt *, bool> {
    auto body = BraceStmt::create(
      ctx, SourceLoc(), { }, SourceLoc(), /*implicit=*/true);
    return { body, /*isTypeChecked=*/true };
  };

  // Build a reference to self.
  Type selfType = getter->getImplicitSelfDecl()->getType();
  Expr *selfArg = DerivedConformance::createSelfDeclRef(getter);
  selfArg->setType(selfType);

  // The builtin call gives us a Builtin.Executor.
  auto builtinCall =
    DerivedConformance::createBuiltinCall(ctx,
                        BuiltinValueKind::BuildDefaultActorExecutorRef,
                                          {selfType}, {}, {selfArg});

  // Turn that into an UnownedSerialExecutor.
  auto initCall = constructUnownedSerialExecutor(ctx, builtinCall);
  if (!initCall) return failure();

  auto ret = new (ctx) ReturnStmt(SourceLoc(), initCall, /*implicit*/ true);

  auto body = BraceStmt::create(
    ctx, SourceLoc(), { ret }, SourceLoc(), /*implicit=*/true);
  return { body, /*isTypeChecked=*/true };
}

/// Derive the declaration of Actor's unownedExecutor property.
static ValueDecl *deriveActor_unownedExecutor(DerivedConformance &derived) {
  fprintf(stderr, "[%s:%d](%s) deriveActor_unownedExecutor\n", __FILE_NAME__, __LINE__, __FUNCTION__);
  ASTContext &ctx = derived.Context;

  // Retrieve the types and declarations we'll need to form this operation.
  auto executorDecl = ctx.getUnownedSerialExecutorDecl();
  if (!executorDecl) {
    derived.Nominal->diagnose(
      diag::concurrency_lib_missing, "UnownedSerialExecutor");
    return nullptr;
  }
  Type executorType = executorDecl->getDeclaredInterfaceType();

  auto propertyPair = derived.declareDerivedProperty(
      DerivedConformance::SynthesizedIntroducer::Var, ctx.Id_unownedExecutor,
      executorType, executorType,
      /*static*/ false, /*final*/ false);
  auto property = propertyPair.first;
  property->setSynthesized(true);
  property->getAttrs().add(new (ctx) SemanticsAttr(SEMANTICS_DEFAULT_ACTOR,
                                                   SourceLoc(), SourceRange(),
                                                   /*implicit*/ true));
  property->getAttrs().add(new (ctx) NonisolatedAttr(/*IsImplicit=*/true));

  // Make the property implicitly final.
  property->getAttrs().add(new (ctx) FinalAttr(/*IsImplicit=*/true));
  if (property->getFormalAccess() == AccessLevel::Open)
    property->overwriteAccess(AccessLevel::Public);

  // Infer availability.
  SmallVector<const Decl *, 2> asAvailableAs;
  asAvailableAs.push_back(executorDecl);
  if (auto enclosingDecl = property->getInnermostDeclWithAvailability())
    asAvailableAs.push_back(enclosingDecl);

  AvailabilityInference::applyInferredAvailableAttrs(
      property, asAvailableAs, ctx);

  auto getter =
    derived.addGetterToReadOnlyDerivedProperty(property, executorType);
  getter->setBodySynthesizer(deriveBodyActor_unownedExecutor);

  derived.addMembersToConformanceContext(
    { property, propertyPair.second, });
  return property;
}

static ValueDecl* checkUserDeclared(DerivedConformance &derived, ValueDecl *requirement) {
  auto proto = derived.Protocol;
  auto *classDecl = dyn_cast<ClassDecl>(derived.Nominal);

  DeclName memberName = requirement->getName();
  fprintf(stderr, "[%s:%d](%s) member name:\n", __FILE_NAME__, __LINE__, __FUNCTION__);
  memberName.dump();
  auto unownedExecutorDecl = derived.Context.getUnownedSerialExecutorDecl();
  auto result = TypeChecker::lookupMember(classDecl, unownedExecutorDecl->getDeclaredInterfaceType(),
                                          DeclNameRef(memberName));

  if (result.empty()) {
    return nullptr;
  }

  return result.front().getValueDecl();
}

ValueDecl *DerivedConformance::deriveActor(ValueDecl *requirement) {
  auto var = dyn_cast<VarDecl>(requirement);
  if (!var)
    return nullptr;

  if (var->getName() == Context.Id_unownedExecutor) {
//    fprintf(stderr, "[%s:%d](%s) check if we can derive\n", __FILE_NAME__, __LINE__, __FUNCTION__);
//    if (auto value = checkUserDeclared(*this, requirement)) {
//      fprintf(stderr, "[%s:%d](%s) found existing\n", __FILE_NAME__, __LINE__, __FUNCTION__);
//      return value;
//    } else {
//      fprintf(stderr, "[%s:%d](%s) call deriveActor_unownedExecutor\n", __FILE_NAME__, __LINE__, __FUNCTION__);
      return deriveActor_unownedExecutor(*this);
//    }
  }

  return nullptr;
}
