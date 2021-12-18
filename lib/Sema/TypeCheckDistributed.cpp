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

/// Add Fix-It text for the given protocol type to inherit DistributedActor.
void swift::diagnoseDistributedFunctionInNonDistributedActorProtocol(
    const ProtocolDecl *proto, InFlightDiagnostic &diag) {
  if (proto->getInherited().empty()) {
    SourceLoc fixItLoc = proto->getBraces().Start;
    diag.fixItInsert(fixItLoc, ": DistributedActor");
  } else {
    // Similar to how Sendable FitIts do this, we insert at the end of
    // the inherited types.
    ASTContext &ctx = proto->getASTContext();
    SourceLoc fixItLoc = proto->getInherited().back().getSourceRange().End;
    fixItLoc = Lexer::getLocForEndOfToken(ctx.SourceMgr, fixItLoc);
    diag.fixItInsert(fixItLoc, ", DistributedActor");
  }
}


/// Add Fix-It text for the given nominal type to adopt Codable.
///
/// Useful when 'Codable' is the 'SerializationRequirement' and a non-Codable
/// function parameter or return value type is detected.
void swift::addCodableFixIt(
    const NominalTypeDecl *nominal, InFlightDiagnostic &diag) {
  if (nominal->getInherited().empty()) {
    SourceLoc fixItLoc = nominal->getBraces().Start;
    diag.fixItInsert(fixItLoc, ": Codable");
  } else {
    ASTContext &ctx = nominal->getASTContext();
    SourceLoc fixItLoc = nominal->getInherited().back().getSourceRange().End;
    fixItLoc = Lexer::getLocForEndOfToken(ctx.SourceMgr, fixItLoc);
    diag.fixItInsert(fixItLoc, ", Codable");
  }
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

  // Class declarations are 'distributed actors' if they are declared with
  // 'distributed actor'
  auto classDecl = dyn_cast<ClassDecl>(nominal);
  if(!classDecl)
    return false;

  return classDecl->isExplicitDistributedActor();
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
      if (diagnose) {
        auto diag = func->diagnose(
            diag::distributed_actor_func_param_not_codable,
            param->getArgumentName().str(), param->getInterfaceType(),
            func->getDescriptiveKind(), "Codable");
        if (auto paramNominalTy = paramTy->getAnyNominal()) {
          addCodableFixIt(paramNominalTy, diag);
        } // else, no nominal type to suggest the fixit for, e.g. a closure
      }
      return true;
    }

    if (param->isInOut()) {
      param->diagnose(
          diag::distributed_actor_func_inout,
          param->getName(),
          func->getDescriptiveKind(), func->getName()
      ).fixItRemove(SourceRange(param->getTypeSourceRangeForDiagnostics().Start,
                                param->getTypeSourceRangeForDiagnostics().Start.getAdvancedLoc(1)));
      // FIXME(distributed): the fixIt should be on param->getSpecifierLoc(), but that Loc is invalid for some reason?
      return true;
    }

    if (param->isVariadic()) {
      param->diagnose(
          diag::distributed_actor_func_variadic,
          param->getName(),
          func->getDescriptiveKind(), func->getName()
      );
    }
  }

  // --- Result type must be either void or a codable type
  auto resultType = func->mapTypeIntoContext(func->getResultInterfaceType());
  if (!resultType->isVoid()) {
    if (TypeChecker::conformsToProtocol(resultType, decodableType, module).isInvalid() ||
        TypeChecker::conformsToProtocol(resultType, encodableType, module).isInvalid()) {
      if (diagnose) {
        auto diag = func->diagnose(
            diag::distributed_actor_func_result_not_codable,
            func->getResultInterfaceType(), func->getDescriptiveKind(),
            "Codable" // Codable is a typealias, easier to diagnose like that
        );
        if (auto resultNominalType = resultType->getAnyNominal()) {
          addCodableFixIt(resultNominalType, diag);
        }
      }
      return true;
    }
  }

  // === Check _remote functions
  auto actorDecl = func->getParent()->getSelfNominalTypeDecl();
  assert(actorDecl && actorDecl->isDistributedActor());

  // _remote function for a distributed instance method must not be implemented by end-users,
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

  // === Designated initializers must accept exactly one actor transport that
  // matches the actor transport type of the actor.
  SmallVector<ParamDecl*, 2> transportParams;
  int transportParamsCount = 0;
  Type transportTy = ctor->mapTypeIntoContext(
      getDistributedActorTransportType(const_cast<ClassDecl *>(decl)));
  for (auto param : *ctor->getParameters()) {
    auto paramTy = ctor->mapTypeIntoContext(param->getInterfaceType());
    if (paramTy->isEqual(transportTy)) {
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
  }

  // ==== Properties
  // --- Check for any illegal re-definitions
  checkDistributedActorProperties(decl);
}

Type swift::getDistributedActorTransportType(NominalTypeDecl *actor) {
  assert(actor->isDistributedActor());
  auto &ctx = actor->getASTContext();

  auto protocol = ctx.getProtocol(KnownProtocolKind::DistributedActor);
  if (!protocol)
    return ErrorType::get(ctx);

  // Dig out the actor transport type.
  auto module = actor->getParentModule();
  Type selfType = actor->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, protocol);
  return conformance.getTypeWitnessByName(selfType, ctx.Id_Transport);
}

Type swift::getDistributedActorIdentityType(NominalTypeDecl *actor) {
  assert(actor->isDistributedActor());
  auto &ctx = actor->getASTContext();

  auto actorProtocol = ctx.getProtocol(KnownProtocolKind::DistributedActor);
  if (!actorProtocol)
    return ErrorType::get(ctx);

  AssociatedTypeDecl *transportDecl =
      actorProtocol->getAssociatedType(ctx.Id_Transport);
  if (!transportDecl)
    return ErrorType::get(ctx);

  auto transportProtocol = ctx.getProtocol(KnownProtocolKind::ActorTransport);
  if (!transportProtocol)
    return ErrorType::get(ctx);

  AssociatedTypeDecl *identityDecl =
      transportProtocol->getAssociatedType(ctx.getIdentifier("Identity"));
  if (!identityDecl)
    return ErrorType::get(ctx);

  auto module = actor->getParentModule();
  Type selfType = actor->getSelfInterfaceType();
  auto conformance = module->lookupConformance(selfType, actorProtocol);
  Type dependentType = actorProtocol->getSelfInterfaceType();
  dependentType = DependentMemberType::get(dependentType, transportDecl);
  dependentType = DependentMemberType::get(dependentType, identityDecl);
  return dependentType.subst(
      SubstitutionMap::getProtocolSubstitutions(
        actorProtocol, selfType, conformance));
}
