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

void SILGenFunction::emitDistributedThunk(SILDeclRef thunk) {
  // Check if actor is local or remote and call respective function
  //
  // func X_distributedThunk(...) async throws -> T {
  //   if __isRemoteActor(self) {
  //     return try await self._remote_X(...)
  //   } else {
  //     return try await self.X(...)
  //   }
  // }
  //

  assert(thunk.isDistributed);
  SILDeclRef native = thunk.asDistributed(false);
  auto fd = cast<AbstractFunctionDecl>(thunk.getDecl());

  ASTContext &ctx = getASTContext();

  // Use the same generic environment as the native entry point.
  F.setGenericEnvironment(SGM.Types.getConstantGenericEnvironment(native));

  auto loc = thunk.getAsRegularLocation();
  loc.markAutoGenerated();
  Scope scope(Cleanups, CleanupLocation(loc));

  auto isRemoteBB = createBasicBlock();
  auto isLocalBB = createBasicBlock();
  auto localErrorBB = createBasicBlock();
  auto remoteErrorBB = createBasicBlock();
  auto localReturnBB = createBasicBlock();
  auto remoteReturnBB = createBasicBlock();
  auto errorBB = createBasicBlock();
  auto returnBB = createBasicBlock();

  auto methodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                    thunk);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
  auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);
  auto resultType = fnConv.getSILResultType(getTypeExpansionContext());

  auto *selfDecl = fd->getImplicitSelfDecl();

  SmallVector<SILValue, 8> params;

  bindParametersForForwarding(fd->getParameters(), params);
  bindParameterForForwarding(selfDecl, params);
  auto selfValue = ManagedValue::forUnmanaged(params[params.size() - 1]);
  auto selfType = selfDecl->getType();

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  {
    FuncDecl* isRemoteFn = ctx.getIsRemoteDistributedActor();
    assert(isRemoteFn &&
    "Could not find 'is remote' function, is the '_Distributed' module available?");

    ManagedValue selfAnyObject = B.createInitExistentialRef(loc, getLoweredType(ctx.getAnyObjectType()),
                                                            CanType(selfType),
                                                            selfValue, {});
    auto result = emitApplyOfLibraryIntrinsic(loc, isRemoteFn, SubstitutionMap(),
                                              {selfAnyObject}, SGFContext());

    SILValue isRemoteResult = std::move(result).forwardAsSingleValue(*this, loc);
    SILValue isRemoteResultUnwrapped = emitUnwrapIntegerResult(loc, isRemoteResult);

    B.createCondBranch(loc, isRemoteResultUnwrapped, isRemoteBB, isLocalBB);
  }

  // // if __isRemoteActor(self)
  // {
  //   return try await self._remote_X(...)
  // }
  {
    B.emitBlock(isRemoteBB);

    auto *selfTyDecl = FunctionDC->getParent()->getSelfNominalTypeDecl();
    assert(selfTyDecl && "distributed function declared outside of actor");

    auto remoteFnDecl = selfTyDecl->lookupDirectRemoteFunc(fd);
    assert(remoteFnDecl && "Could not find _remote_<dist_func_name> function");
    auto remoteFnRef = SILDeclRef(remoteFnDecl);

    SILGenFunctionBuilder builder(SGM);
    auto remoteFnSIL = builder.getOrCreateFunction(loc, remoteFnRef, ForDefinition);
    SILValue remoteFn = B.createFunctionRefFor(loc, remoteFnSIL);

    auto subs = F.getForwardingSubstitutionMap();

    SmallVector<SILValue, 8> remoteParams(params);

    B.createTryApply(loc, remoteFn, subs, remoteParams, remoteReturnBB, remoteErrorBB);
  }

  // // else
  // {
  //   return (try)? (await)? self.X(...)
  // }
  {
    B.emitBlock(isLocalBB);

    auto nativeMethodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                            native);
    auto nativeFnSILTy = SILType::getPrimitiveObjectType(nativeMethodTy);
    auto nativeSilFnType = nativeFnSILTy.castTo<SILFunctionType>();

    SILValue nativeFn = emitClassMethodRef(
        loc, params[params.size() - 1], native, nativeMethodTy);
    auto subs = F.getForwardingSubstitutionMap();

    if (nativeSilFnType->hasErrorResult()) {
      B.createTryApply(loc, nativeFn, subs, params, localReturnBB, localErrorBB);
    } else {
      auto result = B.createApply(loc, nativeFn, subs, params);
      B.createBranch(loc, returnBB, {result});
    }
  }

  {
    B.emitBlock(remoteErrorBB);
    SILValue error = remoteErrorBB->createPhiArgument(
        fnConv.getSILErrorType(getTypeExpansionContext()),
        OwnershipKind::Owned);

    B.createBranch(loc, errorBB, {error});
  }

  {
    B.emitBlock(localErrorBB);
    SILValue error = localErrorBB->createPhiArgument(
        fnConv.getSILErrorType(getTypeExpansionContext()),
        OwnershipKind::Owned);

    B.createBranch(loc, errorBB, {error});
  }

  {
    B.emitBlock(remoteReturnBB);
    SILValue result = remoteReturnBB->createPhiArgument(
        resultType, OwnershipKind::Owned);
    B.createBranch(loc, returnBB, {result});
  }

  {
    B.emitBlock(localReturnBB);
    SILValue result = localReturnBB->createPhiArgument(
        resultType, OwnershipKind::Owned);
    B.createBranch(loc, returnBB, {result});
  }

  // Emit return logic
  {
    B.emitBlock(returnBB);
    SILValue resArg = returnBB->createPhiArgument(
        resultType, OwnershipKind::Owned);
    B.createReturn(loc, resArg);
  }

  // Emit the rethrow logic.
  {
    B.emitBlock(errorBB);
    SILValue error = errorBB->createPhiArgument(
        fnConv.getSILErrorType(getTypeExpansionContext()),
        OwnershipKind::Owned);

    Cleanups.emitCleanupsForReturn(CleanupLocation(loc), IsForUnwind);
    B.createThrow(loc, error);
  }
}