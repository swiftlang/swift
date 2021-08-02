//===--- SILGenConstructor.cpp - SILGen for constructors ------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "ArgumentSource.h"
#include "Conversion.h"
#include "ExecutorBreadcrumb.h"
#include "Initialization.h"
#include "LValue.h"
#include "RValue.h"
#include "SILGenFunction.h"
#include "SILGenFunctionBuilder.h"
#include "Scope.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/ForeignErrorConvention.h"
#include "swift/AST/GenericEnvironment.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/PropertyWrappers.h"
#include "swift/Basic/Defer.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"

using namespace swift;
using namespace Lowering;

static InitializationPtr
emitDistributedActorTransportInit(SILGenFunction &SGF, VarDecl *selfDecl,
                                  Pattern *transportPattern, VarDecl *transportVar) {
  auto self = SGF.emitSelfForMemberInit(SGF, transportPattern, selfDecl);
  InitializationPtr initialization =
      emitPatternBindingInitialization(transportPattern, SOMEDEST);

  //
  FullExpr Scope(SGF.Cleanups, CleanupLocation(transportVar->getInitializer()));
  emitExprInto(transportVar->getInitializer());
}


void SILGenFunction::initializeDistributedActorImplicitStorageInit(
    ConstructorDecl *ctor, ManagedValue selfArg) {
  VarDecl *selfDecl = ctor->getImplicitSelfDecl();
  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();
  auto &C = classDecl->getASTContext();

  SILLocation prologueLoc = RegularLocation(ctor);
  prologueLoc.markAsPrologue(); // TODO: no idea if this is necessary or makes sense

  fprintf(stderr, "[%s:%d] (%s) EMIT initializeDistributedActorImplicitStorageInit\n", __FILE__, __LINE__, __FUNCTION__);
  ctor->dump();
  fprintf(stderr, "[%s:%d] (%s) CLASS---------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  classDecl->dump();
  fprintf(stderr, "[%s:%d] (%s) --------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);

  // find the transport parameter
  SILValue transportArgValue = F.getArgument(0);
  ManagedValue transportArgManaged = ManagedValue::forUnmanaged(transportArgValue);

  auto transportTy = C.getActorTransportType(); // getProtocol(KnownProtocolKind::ActorTransport);
  auto identityProtoTy = C.getActorIdentityType(); //getProtocol(KnownProtocolKind::ActorIdentity);
  auto anyIdentityTy = C.getAnyActorIdentityType();

  VarDecl *transportMember;
  VarDecl *idMember;

  for (auto member : classDecl->getMembers()) {
    PatternBindingDecl *pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd) continue;
    if (pbd->isStatic()) continue;

    fprintf(stderr, "[%s:%d] (%s) MEMBER\n", __FILE__, __LINE__, __FUNCTION__);
    member->dump();

    Pattern *pattern = pbd->getPattern(0);
    VarDecl *var = pbd->getSingleVar();
    if (!var) continue;

    if (var->getName() == C.Id_actorTransport &&
        var->getInterfaceType()->isEqual(transportTy)) {
      transportMember = var;
      fprintf(stderr, "[%s:%d] (%s) FOUND TRANSPORT MEMBER\n", __FILE__, __LINE__, __FUNCTION__);
      auto transportInit = emitDistributedActorTransportInit(*this, selfDecl, pattern, var);
    } else if (var->getName() == C.Id_id &&
               (var->getInterfaceType()->isEqual(identityProtoTy) ||
                var->getInterfaceType()->isEqual(anyIdentityTy))) { // TODO(distributed): stick one way to store, but today we can't yet store the existential
      idMember = var;
      fprintf(stderr, "[%s:%d] (%s) FOUND ID MEMBER\n", __FILE__, __LINE__, __FUNCTION__);
      auto transportInit = emitDistributedActorTransportInit(*this, selfDecl, pattern, var);

    }
    if (transportMember && idMember)
      break; // we found all properties we care about, break out of the loop early
  }

  assert(transportMember && "Missing DistributedActor.actorTransport member");
  assert(idMember && "Missing DistributedActor.id member");

}