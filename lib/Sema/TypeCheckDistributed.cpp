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

bool swift::ensureDistributedModuleLoaded(Decl *decl) {
  auto &C = decl->getASTContext();
  auto moduleAvailable = evaluateOrDefault(
      C.evaluator, DistributedModuleIsAvailableRequest{decl}, false);
  return moduleAvailable;
}

bool
DistributedModuleIsAvailableRequest::evaluate(Evaluator &evaluator,
                                              Decl *decl) const {
  auto &C = decl->getASTContext();

  if (C.getLoadedModule(C.Id_Distributed))
    return true;

  // seems we're missing the _Distributed module, ask to import it explicitly
  decl->diagnose(diag::distributed_actor_needs_explicit_distributed_import);
  return false;
}

// ==== ------------------------------------------------------------------------

bool IsDistributedActorRequest::evaluate(
    Evaluator &evaluator, NominalTypeDecl *nominal) const {
  // Protocols are actors if they inherit from `DistributedActor`.
  if (auto protocol = dyn_cast<ProtocolDecl>(nominal)) {
    auto &ctx = protocol->getASTContext();
    auto *distributedActorProtocol = ctx.getDistributedActorDecl();
    return (protocol == distributedActorProtocol ||
            protocol->inheritsFrom(distributedActorProtocol));
  }

  // Class declarations are 'distributed actors' if they are declared with 'distributed actor'
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if(!classDecl)
    return false;

  return classDecl->isExplicitDistributedActor();
}

AbstractFunctionDecl *GetDistributedRemoteFuncRequest::evaluate(
    Evaluator &evaluator, AbstractFunctionDecl *func) const {

  if (!func->isDistributed())
    return nullptr;

  auto &C = func->getASTContext();
  DeclContext *DC = func->getDeclContext();

  // not via `ensureDistributedModuleLoaded` to avoid generating a warning,
  // we won't be emitting the offending decl after all.
  if (!C.getLoadedModule(C.Id_Distributed))
    return nullptr;

  // Locate the actor decl that the member must be synthesized to.
  // TODO(distributed): should this just be added to the extension instead when we're in one?
  ClassDecl *decl = dyn_cast<ClassDecl>(DC);
  if (!decl) {
    if (auto ED = dyn_cast<ExtensionDecl>(DC)) {
      decl = dyn_cast<ClassDecl>(ED->getExtendedNominal());
    }
  }

  /// A distributed func cannot be added to a non-distributed actor;
  /// If the 'decl' was not a distributed actor we must have declared and
  /// requested it from a illegal context, let's just ignore the synthesis.
  assert(decl && "Can't find actor detect to add implicit _remote function to");
  return TypeChecker::addImplicitDistributedActorRemoteFunction(decl, func);
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
    auto paramTy = func->mapTypeIntoContext(param->getInterfaceType());
    if (TypeChecker::conformsToProtocol(paramTy, encodableType, module).isInvalid() ||
        TypeChecker::conformsToProtocol(paramTy, decodableType, module).isInvalid()) {
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

  // === Check _remote functions
  ClassDecl *actorDecl = dyn_cast<ClassDecl>(func->getParent());
  assert(actorDecl && actorDecl->isDistributedActor());

  // _remote function for a distributed function must not be implemented by end-users,
  // it must be the specific implementation synthesized by the compiler.
  auto remoteFuncDecl = actorDecl->lookupDirectRemoteFunc(func);
  if (remoteFuncDecl && !remoteFuncDecl->isSynthesized()) {
    if (diagnose) {
      func->diagnose(diag::distributed_actor_remote_func_implemented_manually,
                     func->getBaseIdentifier(),
                     // TODO: make general function to get the _remote identifier
                     C.getIdentifier("_remote_" + func->getBaseIdentifier().str().str()));
    }
    return true;
  }

  return false;
}

void swift::checkDistributedActorProperties(const ClassDecl *decl) {
  auto &C = decl->getASTContext();

  for (auto member : decl->getMembers()) {
    if (auto prop = dyn_cast<VarDecl>(member)) {
      if (prop->isSynthesized())
        continue;

      auto id = prop->getName();
      if (id == C.Id_actorTransport || id == C.Id_id) {
        prop->diagnose(diag::distributed_actor_user_defined_special_property,
                      id);
      }
    }
  }
}

void swift::checkDistributedActorConstructor(const ClassDecl *decl, ConstructorDecl *ctor) {
  // bail out unless distributed actor, only those have special rules to check here
  if (!decl->isDistributedActor())
    return;

  // Only designated initializers need extra checks
  if (!ctor->isDesignatedInit())
    return;

  // === Designated initializers must accept exactly one ActorTransport
  auto &C = ctor->getASTContext();
  auto module = ctor->getParentModule();

  SmallVector<ParamDecl*, 2> transportParams;
  int transportParamsCount = 0;
  auto protocolDecl = C.getProtocol(KnownProtocolKind::ActorTransport);
  auto protocolTy = protocolDecl->getDeclaredInterfaceType();

  for (auto param : *ctor->getParameters()) {
    auto paramTy = ctor->mapTypeIntoContext(param->getInterfaceType());
    auto conformance = TypeChecker::conformsToProtocol(paramTy, protocolDecl, module);

    if (paramTy->isEqual(protocolTy) || !conformance.isInvalid()) {
      transportParamsCount += 1;
      transportParams.push_back(param);
    }
  }

  // missing transport parameter
  if (transportParamsCount == 0) {
    ctor->diagnose(diag::distributed_actor_designated_ctor_missing_transport_param,
                   ctor->getName());
    // TODO(distributed): offer fixit to insert 'transport: ActorTransport'
    return;
  }

  // ok! We found exactly one transport parameter
  if (transportParamsCount == 1)
    return;

  // TODO(distributed): rdar://81824959 report the error on the offending (2nd) matching parameter
  //                    Or maybe we can issue a note about the other offending params?
  ctor->diagnose(diag::distributed_actor_designated_ctor_must_have_one_transport_param,
                 ctor->getName(), transportParamsCount);
}

// ==== ------------------------------------------------------------------------

void TypeChecker::checkDistributedActor(ClassDecl *decl) {
  if (!decl)
    return;

  // ==== Ensure the _Distributed module is available,
  // without it there's no reason to check the decl in more detail anyway.
  if (!swift::ensureDistributedModuleLoaded(decl))
    return;

  // ==== Constructors
  // --- Get the default initializer
  // If applicable, this will create the default 'init(transport:)' initializer
  (void)decl->getDefaultInitializer();

  for (auto member : decl->getMembers()) {
    // --- Check all constructors
    if (auto ctor = dyn_cast<ConstructorDecl>(member))
      checkDistributedActorConstructor(decl, ctor);

    // --- synthesize _remote functions for distributed functions
    if (auto func = dyn_cast<FuncDecl>(member))
      (void)addImplicitDistributedActorRemoteFunction(decl, func);
  }

  // ==== Properties
  // --- Check for any illegal re-definitions
  checkDistributedActorProperties(decl);
}

