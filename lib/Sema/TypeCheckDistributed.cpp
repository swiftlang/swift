//===--- TypeCheckDistributed.cpp - Distributed ---------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements type checking support for Swift's concurrency model.
//
//===----------------------------------------------------------------------===//
#include "TypeCheckConcurrency.h"
#include "TypeCheckDistributed.h"
#include "TypeChecker.h"
#include "TypeCheckType.h"
#include "swift/Strings.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Initializer.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/AST/TypeVisitor.h"
#include "swift/AST/ExistentialLayout.h"

using namespace swift;

// ==== ------------------------------------------------------------------------

bool IsDistributedActorRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // Protocols are actors if their `Self` type conforms to `DistributedActor`.
  if (auto protocol = dyn_cast<ProtocolDecl>(nominal)) {
    // Simple case: we have the `DistributedActor` protocol itself.
    if (protocol->isSpecificProtocol(KnownProtocolKind::DistributedActor))
      return true;

    auto actorProto = nominal->getASTContext().getProtocol(
        KnownProtocolKind::DistributedActor);
    if (!actorProto)
      return false;

    auto selfType = Type(protocol->getProtocolSelfType());
    auto genericSig = protocol->getGenericSignature();
    if (!genericSig)
      return false;

    return genericSig->requiresProtocol(selfType, actorProto);
  }

  // Class declarations are 'distributed actors' if they are declared with 'distributed actor'
  if(!dyn_cast<ClassDecl>(nominal))
    return false;

  auto distributedAttr = nominal->getAttrs()
      .getAttribute<DistributedActorAttr>();
  return distributedAttr != nullptr;
}

bool IsDistributedFuncRequest::evaluate(
    Evaluator &evaluator, FuncDecl *func) const {
  // Check whether the attribute was explicitly specified.
  if (auto attr = func->getAttrs().getAttribute<DistributedActorAttr>()) {
    return true;
  } else {
    return false;
  }
}

// ==== ------------------------------------------------------------------------

/// Check whether the function is a proper distributed function
///
/// \param diagnose Whether to emit a diagnostic when a problem is encountered.
///
/// \returns \c true if there was a problem with adding the attribute, \c false
/// otherwise.
bool swift::checkDistributedFunction(FuncDecl *func, bool diagnose) {
  // === All parameters and the result type must be Codable

  auto &C = func->getASTContext();
  auto encodableType = C.getProtocol(KnownProtocolKind::Encodable);
  auto decodableType = C.getProtocol(KnownProtocolKind::Decodable);

  auto module = func->getParentModule();

  // --- Check parameters for 'Codable' conformance
  for (auto param : *func->getParameters()) {
    auto paramType = func->mapTypeIntoContext(param->getInterfaceType());
    if (TypeChecker::conformsToProtocol(paramType, encodableType, module).isInvalid() ||
        TypeChecker::conformsToProtocol(paramType, decodableType, module).isInvalid()) {
      if (diagnose)
        func->diagnose(
            diag::distributed_actor_func_param_not_codable,
            param->getArgumentName().str(),
            param->getInterfaceType()
        );
      // TODO: suggest a fixit to add Codable to the type?
      return true;
    }
  }

  // --- Result type must be either void or a codable type
  auto resultType = func->mapTypeIntoContext(func->getResultInterfaceType());
  if (!resultType->isVoid()) {
    if (TypeChecker::conformsToProtocol(resultType, decodableType, module).isInvalid() ||
        TypeChecker::conformsToProtocol(resultType, encodableType, module).isInvalid()) {
      if (diagnose)
        func->diagnose(
            diag::distributed_actor_func_result_not_codable,
            func->getResultInterfaceType()
        );
      // TODO: suggest a fixit to add Codable to the type?
      return true;
    }
  }

  // === Each distributed function must have a static _remote_<func_name> counterpart
  ClassDecl *actorDecl = dyn_cast<ClassDecl>(func->getParent());
  assert(actorDecl && actorDecl->isDistributedActor());

  auto remoteFuncDecl = actorDecl->lookupDirectRemoteFunc(func);
  if (!remoteFuncDecl) {
    if (diagnose) {
      auto localFuncName = func->getBaseIdentifier().str().str();
      func->diagnose(
          diag::distributed_actor_func_missing_remote_func,
          C.getIdentifier("_remote_" + localFuncName));
    }
    return true;
  }

  if (!remoteFuncDecl->isStatic()) {
    if (diagnose)
      func->diagnose(
          diag::distributed_actor_remote_func_is_not_static,
          remoteFuncDecl->getName());
    return true;
  }

  if (!remoteFuncDecl->hasAsync() || !remoteFuncDecl->hasThrows()) {
    if (diagnose)
      func->diagnose(
          diag::distributed_actor_remote_func_is_not_async_throws,
          remoteFuncDecl->getName());
    return true;
  }

  if (remoteFuncDecl->isDistributed()) {
    if (diagnose)
      func->diagnose(
          diag::distributed_actor_remote_func_must_not_be_distributed,
          remoteFuncDecl->getName());
    return true;
  }

  return false;
}

void swift::checkDistributedActorConstructor(ClassDecl *decl, ConstructorDecl *ctor) {
  // bail out unless distributed actor, only those have special rules to check here
  if (!decl->isDistributedActor())
    return;

  // bail out for synthesized constructors
  if (ctor->isSynthesized())
    return;

  if (ctor->isDistributedActorLocalInit()) {
    // it is not legal to manually define init(transport:)
    // TODO: we want to lift this restriction but it is tricky
    ctor->diagnose(diag::distributed_actor_local_init_explicitly_defined)
        .fixItRemove(SourceRange(ctor->getStartLoc(), decl->getStartLoc()));
    // TODO: we should be able to allow this, but then we need to inject
    //       code or force users to "do the right thing"
    return;
  }

  if (ctor->isDistributedActorResolveInit()) {
    // It is illegal for users to attempt defining a resolve initializer;
    // Suggest removing it entirely, there is no way users can implement this init.
    ctor->diagnose(diag::distributed_actor_init_resolve_must_not_be_user_defined)
        .fixItRemove(SourceRange(ctor->getStartLoc(), decl->getStartLoc()));
    return;
  }

  // All user defined initializers on distributed actors must be 'convenience'.
  //
  // The only initializer that is allowed to be designated is init(transport:)
  // which we synthesize on behalf of a distributed actor.
  //
  // When checking ctor bodies we'll check
  if (!ctor->isConvenienceInit()) {
    ctor->diagnose(diag::distributed_actor_init_user_defined_must_be_convenience,
                   ctor->getName())
        .fixItInsert(ctor->getConstructorLoc(), "convenience ");
    return;
  }
}

