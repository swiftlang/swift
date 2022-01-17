//===--- SILGenDistributed.cpp - SILGen for distributed -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
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
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILUndef.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/Utils/DistributedActor.h"

using namespace swift;
using namespace Lowering;

// MARK: utility functions

/// Obtain a nominal type's member by name, as a VarDecl.
/// \returns nullptr if the name lookup doesn't resolve to exactly one member,
///          or the subsequent cast to VarDecl failed.
static VarDecl* lookupProperty(NominalTypeDecl *ty, DeclName name) {
  auto refs = ty->lookupDirect(name);
  if (refs.size() != 1)
    return nullptr;
  return dyn_cast<VarDecl>(refs.front());
}

/// Emit a reference to a specific stored property of the actor.
static SILValue emitActorPropertyReference(
    SILGenFunction &SGF, SILLocation loc, SILValue actorSelf,
    VarDecl *property) {
  Type formalType = SGF.F.mapTypeIntoContext(property->getInterfaceType());
  SILType loweredType = SGF.getLoweredType(formalType).getAddressType();
  return SGF.B.createRefElementAddr(loc, actorSelf, property, loweredType);
}

/// Perform an initializing store to the given property using the value
/// \param actorSelf the value representing `self` for the actor instance.
/// \param prop the property to be initialized.
/// \param value the value to use when initializing the property.
static void initializeProperty(SILGenFunction &SGF, SILLocation loc,
                               SILValue actorSelf,
                               VarDecl* prop, SILValue value) {
  Type formalType = SGF.F.mapTypeIntoContext(prop->getInterfaceType());
  SILType loweredType = SGF.getLoweredType(formalType);

  auto fieldAddr = emitActorPropertyReference(SGF, loc, actorSelf, prop);

  if (loweredType.isAddressOnly(SGF.F)) {
    SGF.B.createCopyAddr(loc, value, fieldAddr, IsNotTake, IsInitialization);
  } else {
    if (value->getType().isAddress()) {
      value = SGF.B.createTrivialLoadOr(
          loc, value, LoadOwnershipQualifier::Take);
    } else {
      value = SGF.B.emitCopyValueOperation(loc, value);
    }

    SGF.B.emitStoreValueOperation(
        loc, value, fieldAddr, StoreOwnershipQualifier::Init);

    if (value->getType().isAddress()) {
      SGF.B.createDestroyAddr(loc, value);
    }
  }
}

/******************************************************************************/
/******************* COMMON (DISTRIBUTED) SIL PATTERNS ************************/
/******************************************************************************/

/// Emit the following branch SIL instruction:
/// \verbatim
/// if __isRemoteActor(self) {
///   <isRemoteBB>
/// } else {
///   <isLocalBB>
/// }
/// \endverbatim
static void emitDistributedIfRemoteBranch(SILGenFunction &SGF,
                                          SILLocation Loc,
                                          ManagedValue selfValue, Type selfTy,
                                          SILBasicBlock *isRemoteBB,
                                          SILBasicBlock *isLocalBB) {
  ASTContext &ctx = SGF.getASTContext();
  auto &B = SGF.B;

  FuncDecl *isRemoteFn = ctx.getIsRemoteDistributedActor();
  assert(isRemoteFn && "Could not find 'is remote' function, is the "
                       "'_Distributed' module available?");

  ManagedValue selfAnyObject =
      B.createInitExistentialRef(Loc, SGF.getLoweredType(ctx.getAnyObjectType()),
                                 CanType(selfTy), selfValue, {});
  auto result = SGF.emitApplyOfLibraryIntrinsic(
      Loc, isRemoteFn, SubstitutionMap(), {selfAnyObject}, SGFContext());

  SILValue isRemoteResult =
      std::move(result).forwardAsSingleValue(SGF, Loc);
  SILValue isRemoteResultUnwrapped =
      SGF.emitUnwrapIntegerResult(Loc, isRemoteResult);

  B.createCondBranch(Loc, isRemoteResultUnwrapped, isRemoteBB, isLocalBB);
}

/// Emit a value of `RemoteCallTarget' with the appropriate information for this
/// distributed method.
static SILValue *emitDistributedRemoteCallTargetValue(SILGenFunction &SGF,
                                                      SILLocation loc) {
  ASTContext &ctx = SGF.getASTContext();
  auto &B = SGF.B;

  auto &F = SGF.F;
  assert(false); // FIXME(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!
//
//  F.decl
}

// MARK: local instance initialization

/// For the initialization of a local distributed actor instance, emits code to initialize the instance's
/// stored property corresponding to the system.
static void emitTransportInit(SILGenFunction &SGF,
                                        ConstructorDecl *ctor,
                                        SILLocation loc,
                                        ManagedValue actorSelf) {
  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();
  auto &C = ctor->getASTContext();

  // Sema has already guaranteed that there is exactly one DistributedActorSystem
  // argument to the constructor, so we grab the first one from the params.
  SILValue systemArg = findFirstDistributedActorSystemArg(SGF.F);
  VarDecl *var = lookupProperty(classDecl, C.Id_actorSystem);
  assert(var);
      
  initializeProperty(SGF, loc, actorSelf.getValue(), var, systemArg);
}

/// Emits the distributed actor's identity (`id`) initialization.
///
/// Specifically, it performs:
/// \verbatim
///     self.id = system.assignID(Self.self)
/// \endverbatim
static void emitIdentityInit(SILGenFunction &SGF, ConstructorDecl *ctor,
                             SILLocation loc, ManagedValue borrowedSelfArg) {
  auto &C = ctor->getASTContext();
  auto &B = SGF.B;
  auto &F = SGF.F;
  
  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();

  // --- prepare `Self.self` metatype
  auto *selfTyDecl = ctor->getParent()->getSelfNominalTypeDecl();
  auto selfTy = F.mapTypeIntoContext(selfTyDecl->getDeclaredInterfaceType());
  auto selfMetatype = SGF.getLoweredType(MetatypeType::get(selfTy));
  SILValue selfMetatypeValue = B.createMetatype(loc, selfMetatype);

  SILValue actorSystem = findFirstDistributedActorSystemArg(F);

  // --- create a temporary storage for the result of the call
  // it will be deallocated automatically as we exit this scope
  VarDecl *var = lookupProperty(classDecl, C.Id_id);
  auto resultTy = SGF.getLoweredType(
      F.mapTypeIntoContext(var->getInterfaceType()));
  auto temp = SGF.emitTemporaryAllocation(loc, resultTy);

  // --- emit the call itself.
  emitDistributedActorSystemWitnessCall(
      B, loc, C.Id_assignID,
      actorSystem, SGF.getLoweredType(selfTy),
      { temp, selfMetatypeValue });

  // --- initialize the property.
  initializeProperty(SGF, loc, borrowedSelfArg.getValue(), var, temp);
}

namespace {
/// Cleanup to resign the identity of a distributed actor if an abnormal exit happens.
class ResignIdentity : public Cleanup {
  ClassDecl *actorDecl;
  SILValue self;
public:
  ResignIdentity(ClassDecl *actorDecl, SILValue self)
    : actorDecl(actorDecl), self(self) {
      assert(actorDecl->isDistributedActor());
    }

  void emit(SILGenFunction &SGF, CleanupLocation l, ForUnwind_t forUnwind) override {
    if (forUnwind == IsForUnwind) {
      l.markAutoGenerated();
      SGF.emitDistributedActorSystemResignIDCall(l, actorDecl,
                                 ManagedValue::forUnmanaged(self));
    }
  }

  void dump(SILGenFunction &SGF) const override {
#ifndef NDEBUG
    llvm::errs() << "ResignIdentity "
                 << "State:" << getState() << " "
                 << "Self: " << self << "\n";
#endif
  }
};
} // end anonymous namespace

void SILGenFunction::emitDistActorImplicitPropertyInits(
    ConstructorDecl *ctor, ManagedValue selfArg) {
  // Only designated initializers should perform this initialization.
  assert(ctor->isDesignatedInit());

  auto loc = SILLocation(ctor);
  loc.markAutoGenerated();

  selfArg = selfArg.borrow(*this, loc);
  emitTransportInit(*this, ctor, loc, selfArg);
  emitIdentityInit(*this, ctor, loc, selfArg);

  // register a clean-up to resign the identity upon abnormal exit
  auto *actorDecl = cast<ClassDecl>(ctor->getParent()->getAsDecl());
  Cleanups.pushCleanup<ResignIdentity>(actorDecl, selfArg.getValue());
}

void SILGenFunction::emitDistributedActorReady(
    SILLocation loc, ConstructorDecl *ctor, ManagedValue actorSelf) {

  // Only designated initializers get the lifecycle handling injected
  assert(ctor->isDesignatedInit());

  SILValue transport = findFirstDistributedActorSystemArg(F);

  FullExpr scope(Cleanups, CleanupLocation(loc));
  auto borrowedSelf = actorSelf.borrow(*this, loc);

  emitActorReadyCall(B, loc, borrowedSelf.getValue(), transport);
}

// MARK: remote instance initialization

/// Synthesize the distributed actor's identity (`id`) initialization:
///
/// \verbatim
///     system.resolve(id:as:)
/// \endverbatim
static void createDistributedActorFactory_resolve(
    SILGenFunction &SGF, ASTContext &C, FuncDecl *fd, SILValue idValue,
    SILValue actorSystemValue, Type selfTy, SILValue selfMetatypeValue,
    SILType resultTy, SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  auto &B = SGF.B;

  auto loc = SILLocation(fd);
  loc.markAutoGenerated();

  // // ---- actually call system.resolve(id: id, as: Self.self)
  emitDistributedActorSystemWitnessCall(
      B, loc, C.Id_resolve, actorSystemValue, SGF.getLoweredType(selfTy),
      { idValue, selfMetatypeValue },
      std::make_pair(normalBB, errorBB));
}

/// Function body of:
/// \verbatim
/// DistributedActor.resolve(
///     id: Self.ID,
///     using system: Self.ActorSystem
/// ) throws -> Self
/// \endverbatim
void SILGenFunction::emitDistributedActorFactory(FuncDecl *fd) { // TODO(distributed): rename
  /// NOTE: this will only be reached if the resolve function is actually
  ///       demanded. For example, by declaring the actor as `public` or
  ///       having at least one call to the resolve function.

  auto &C = getASTContext();
  SILLocation loc = fd;

  // ==== Prepare argument references
  // --- Parameter: id
  SILArgument *idArg = F.getArgument(0);

  // --- Parameter: system
  SILArgument *actorSystemArg = F.getArgument(1);

  SILValue selfArgValue = F.getSelfArgument();
  ManagedValue selfArg = ManagedValue::forUnmanaged(selfArgValue);

  // type: SpecificDistributedActor.Type
  auto selfArgType = F.mapTypeIntoContext(selfArg.getType().getASTType());
  auto selfMetatype = getLoweredType(selfArgType);
  SILValue selfMetatypeValue = B.createMetatype(loc, selfMetatype);

  // type: SpecificDistributedActor
  auto *selfTyDecl = fd->getParent()->getSelfNominalTypeDecl();
  assert(selfTyDecl->isDistributedActor());
  auto selfTy = F.mapTypeIntoContext(selfTyDecl->getDeclaredInterfaceType());
  auto returnTy = getLoweredType(selfTy);

  // ==== Prepare all the basic blocks
  auto returnBB = createBasicBlock();
  auto resolvedBB = createBasicBlock();
  auto makeProxyBB = createBasicBlock();
  auto switchBB = createBasicBlock();
  auto errorBB = createBasicBlock();

  SILFunctionConventions fnConv = F.getConventions();

  // --- get the uninitialized allocation from the runtime system.
  FullExpr scope(Cleanups, CleanupLocation(fd));

  auto optionalReturnTy = SILType::getOptionalType(returnTy);

  // ==== Call `try system.resolve(id: id, as: Self.self)`
  {
    createDistributedActorFactory_resolve(
        *this, C, fd, idArg, actorSystemArg, selfTy, selfMetatypeValue,
        optionalReturnTy, switchBB, errorBB);
  }

  // ==== switch resolved { ... }
  {
    B.emitBlock(switchBB);
    auto resolve =
        switchBB->createPhiArgument(optionalReturnTy, OwnershipKind::Owned);

    auto *switchEnum = B.createSwitchEnum(
        loc, resolve, nullptr,
        {{C.getOptionalSomeDecl(), resolvedBB},
         {std::make_pair(C.getOptionalNoneDecl(), makeProxyBB)}});
    switchEnum->createOptionalSomeResult();
  }

  // ==== Case 'some') return the resolved instance
  {
    B.emitBlock(resolvedBB);

    B.createBranch(loc, returnBB, {resolvedBB->getArgument(0)});
  }

  // ==== Case 'none') Create the remote instance
  {
    B.emitBlock(makeProxyBB);
    // ==== Create 'remote' distributed actor instance

    // --- Call: _distributedActorRemoteInitialize(Self.self)
    auto builtinName = C.getIdentifier(
        getBuiltinName(BuiltinValueKind::InitializeDistributedRemoteActor));
    auto *remote = B.createBuiltin(
        loc, builtinName,
        /*returnTy*/returnTy,
        /*subs*/ {},
        {selfMetatypeValue});

    // ==== Initialize distributed actor properties
    loc.markAutoGenerated();
    auto *dc = fd->getDeclContext();
    auto classDecl = dc->getSelfClassDecl();
    
    initializeProperty(*this, loc, remote,
                       lookupProperty(classDecl, C.Id_id),
                       idArg);

    initializeProperty(*this, loc, remote,
                       lookupProperty(classDecl, C.Id_actorSystem),
                       actorSystemArg);

    // ==== Branch to return the fully initialized remote instance
    B.createBranch(loc, returnBB, {remote});
  }

  // --- Emit return logic
  // return <remote>
  {
    B.emitBlock(returnBB);

    auto local = returnBB->createPhiArgument(returnTy, OwnershipKind::Owned);

    Cleanups.emitCleanupsForReturn(CleanupLocation(loc), NotForUnwind);
    B.createReturn(loc, local);
  }

  // --- Emit rethrow logic
  // throw error
  {
    B.emitBlock(errorBB);

    auto error = errorBB->createPhiArgument(
        fnConv.getSILErrorType(F.getTypeExpansionContext()),
        OwnershipKind::Owned);

    Cleanups.emitCleanupsForReturn(CleanupLocation(loc), IsForUnwind);
    B.createThrow(loc, error);
  }
}

// MARK: system.resignID()

void SILGenFunction::emitDistributedActorSystemResignIDCall(
    SILLocation loc, ClassDecl *actorDecl, ManagedValue actorSelf) {
  ASTContext &ctx = getASTContext();
  
  FormalEvaluationScope scope(*this);

  // ==== locate: self.id
  auto idRef = emitActorPropertyReference(
      *this, loc, actorSelf.getValue(), lookupProperty(actorDecl, ctx.Id_id));

  // ==== locate: self.actorSystem
  auto systemRef = emitActorPropertyReference(
      *this, loc, actorSelf.getValue(),
      lookupProperty(actorDecl, ctx.Id_actorSystem));

  // Perform the call.
  emitDistributedActorSystemWitnessCall(
      B, loc, ctx.Id_resignID,
      systemRef,
      SILType(),
      { idRef });
}

void
SILGenFunction::emitConditionalResignIdentityCall(SILLocation loc,
                                                  ClassDecl *actorDecl,
                                                  ManagedValue actorSelf,
                                                  SILBasicBlock *continueBB) {
  assert(actorDecl->isDistributedActor() &&
  "only distributed actors have transport lifecycle hooks in deinit");

  auto selfTy = actorDecl->getDeclaredInterfaceType();
  
  // we only system.resignID if we are a local actor,
  // and thus the address was created by system.assignID.
  auto isRemoteBB = createBasicBlock();
  auto isLocalBB = createBasicBlock();

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  emitDistributedIfRemoteBranch(*this, loc,
                                actorSelf, selfTy,
                                /*if remote*/isRemoteBB,
                                /*if local*/isLocalBB);

  // if remote, do nothing.
  {
    B.emitBlock(isRemoteBB);
    B.createBranch(loc, continueBB);
  }

  // if local, resign identity.
  {
    B.emitBlock(isLocalBB);

    emitDistributedActorSystemResignIDCall(loc, actorDecl, actorSelf);
    
    B.createBranch(loc, continueBB);
  }
}

// MARK: system.makeInvocation()

void SILGenFunction::emitDistributedActorSystemMakeInvocationCall(
    SILLocation loc, ClassDecl *actorDecl, ManagedValue actorSelf,
    SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  ASTContext &ctx = getASTContext();

  FormalEvaluationScope scope(*this);

  // ==== locate: self.actorSystem
  auto systemRef = emitActorPropertyReference(
      *this, loc, actorSelf.getValue(),
      lookupProperty(actorDecl, ctx.Id_actorSystem));

  // Perform the call.
  emitDistributedActorSystemWitnessCall(
      B, loc, ctx.Id_makeInvocationEncoder,
      systemRef,
      SILType(),
      {},
      std::make_pair(normalBB, errorBB));
}

/******************************************************************************/
/******************* DISTRIBUTED DEINIT: class memberwise destruction *********/
/******************************************************************************/

void SILGenFunction::emitDistributedActorClassMemberDestruction(
    SILLocation cleanupLoc, ManagedValue selfValue, ClassDecl *cd,
    SILBasicBlock *normalMemberDestroyBB, SILBasicBlock *finishBB) {
  auto selfTy = cd->getDeclaredInterfaceType();

  Scope scope(Cleanups, CleanupLocation(cleanupLoc));

  auto isLocalBB = createBasicBlock();
  auto remoteMemberDestroyBB = createBasicBlock();

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  emitDistributedIfRemoteBranch(*this, cleanupLoc,
                                selfValue, selfTy,
                                /*if remote*/remoteMemberDestroyBB,
                                /*if local*/isLocalBB);

  // // if __isRemoteActor(self)
  // {
  //  // destroy only self.id and self.actorSystem
  // }
  {
    B.emitBlock(remoteMemberDestroyBB);

    for (VarDecl *vd : cd->getStoredProperties()) {
      if (getActorIsolation(vd) == ActorIsolation::DistributedActorInstance)
        continue;

      destroyClassMember(cleanupLoc, selfValue, vd);
    }

    B.createBranch(cleanupLoc, finishBB);
  }

  // // else (local distributed actor)
  // {
  //   <continue normal deinit>
  // }
  {
    B.emitBlock(isLocalBB);

    B.createBranch(cleanupLoc, normalMemberDestroyBB);
  }
}

/******************************************************************************/
/***************************** DISTRIBUTED THUNKS *****************************/
/******************************************************************************/

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
  auto &M = getModule();

  // Use the same generic environment as the native entry point.
  F.setGenericEnvironment(SGM.Types.getConstantGenericEnvironment(native));

  auto loc = thunk.getAsRegularLocation();
  loc.markAutoGenerated();
  Scope scope(Cleanups, CleanupLocation(loc));

  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//  auto OLDSYNTHESIS = true; // FIXME(distributed): remove when ready !!!!!!!!!!!!
  auto OLDSYNTHESIS = false; // FIXME(distributed): remove when ready !!!!!!!!!!!!
  // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  auto isRemoteBB = createBasicBlock();
  auto isLocalBB = createBasicBlock();
  auto localErrorBB = createBasicBlock();
//  auto makeInvocationNormalBB = OLDSYNTHESIS ? nullptr : createBasicBlock();
//  auto makeInvocationErrorBB = OLDSYNTHESIS ? nullptr : createBasicBlock();
  auto remoteErrorBB = createBasicBlock();
  auto localReturnBB = createBasicBlock();
  auto remoteReturnBB = createBasicBlock();
  auto errorBB = createBasicBlock();
  auto returnBB = createBasicBlock();
  // Basic blocks used as error edges for all the throwing calls we make
  // during the preparation of the distributed invocation. All those
  // calls can throw and have their own error block, all those blocks
  // then directly branch out to the remoteErrorBB.
  SmallVector<SILBasicBlock*, 2> dynamicBasicErrorBlocks;

  auto methodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                    thunk);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
  auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);
  auto resultType = fnConv.getSILResultType(getTypeExpansionContext());

  auto *selfVarDecl = fd->getImplicitSelfDecl();

  SmallVector<SILValue, 8> params;

  bindParametersForForwarding(fd->getParameters(), params);
  bindParameterForForwarding(selfVarDecl, params);
  auto selfValue = ManagedValue::forUnmanaged(params[params.size() - 1]); // TODO(distributed): getSelfArgument instead
  auto selfTy = selfVarDecl->getType();

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  {
    FuncDecl* isRemoteFn = ctx.getIsRemoteDistributedActor();
    assert(isRemoteFn &&
    "Could not find 'is remote' function, is the '_Distributed' module available?");

    ManagedValue selfAnyObject = B.createInitExistentialRef(
        loc, getLoweredType(ctx.getAnyObjectType()),
        CanType(selfTy), selfValue, {});
    auto result = emitApplyOfLibraryIntrinsic(
        loc, isRemoteFn, SubstitutionMap(),
        {selfAnyObject}, SGFContext());

    SILValue isRemoteResult = std::move(result).forwardAsSingleValue(*this, loc);
    SILValue isRemoteResultUnwrapped = emitUnwrapIntegerResult(loc, isRemoteResult);

    B.createCondBranch(loc, isRemoteResultUnwrapped, isRemoteBB, isLocalBB);
  }

  // // if __isRemoteActor(self)
  // {
  //   var invocation = try self.actorSystem.makeInvocation()
  // }
  {
    B.emitBlock(isRemoteBB);
    fprintf(stderr, "[%s:%d] (%s) remoteBB\n", __FILE__, __LINE__, __FUNCTION__);

    auto *selfTyDecl = FunctionDC->getParent()->getSelfNominalTypeDecl();
    assert(selfTyDecl &&
           "distributed instance method declared outside of actor");

    if (OLDSYNTHESIS) {
      // return try await self._remote_X(...)
      auto remoteFnDecl = selfTyDecl->lookupDirectRemoteFunc(fd);
      assert(remoteFnDecl &&
             "Could not find _remote_<dist_func_name> function");
      auto remoteFnRef = SILDeclRef(remoteFnDecl);

      SILGenFunctionBuilder builder(SGM);
      auto remoteFnSIL =
          builder.getOrCreateFunction(loc, remoteFnRef, ForDefinition);
      SILValue remoteFn = B.createFunctionRefFor(loc, remoteFnSIL);

      auto subs = F.getForwardingSubstitutionMap();

      SmallVector<SILValue, 8> remoteParams(params);

      B.createTryApply(loc, remoteFn, subs, remoteParams, remoteReturnBB,
                       remoteErrorBB);
    } else {
      // -----------------------------------------------------------------------
      auto remoteCallFnDecl =
          selfTyDecl->getDistributedActorSystemRemoteCallFunction();
      auto remoteCallFnRef = SILDeclRef(remoteCallFnDecl);
      assert(remoteCallFnDecl && "no remoteCall func found!");

      // === get the actorSystem property
      auto systemRef = emitActorPropertyReference(
          *this, loc, selfValue.getValue(),
          lookupProperty(selfTyDecl, ctx.Id_actorSystem));

      AbstractFunctionDecl *makeInvocationFnDecl =
          selfTyDecl->getDistributedActorSystemMakeInvocationFunction();
      auto makeInvocationFnRef = SILDeclRef(makeInvocationFnDecl);
      assert(makeInvocationFnDecl && "no remoteCall func found!");

      ProtocolDecl *invocationEncoderProto =
          ctx.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);

      auto makeInvocationMethodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                        makeInvocationFnRef);
      auto makeInvocationDerivativeFnSILTy = SILType::getPrimitiveObjectType(makeInvocationMethodTy);
      auto makeInvocationSilFnType = makeInvocationDerivativeFnSILTy.castTo<SILFunctionType>();

      auto invocationEncoderResultInfo =
          makeInvocationSilFnType->getResults().begin();
      auto invocationEncoderCanTy = invocationEncoderResultInfo->getInterfaceType();
      auto invocationEncoderTy = getLoweredType(invocationEncoderCanTy);

      SILGenFunctionBuilder builder(SGM);

      // === -------------------------------------------------------------------
      // var encoder = actorSystem.makeInvocationEncoder()
      auto makeInvocationFnSIL =
          builder.getOrCreateFunction(loc, makeInvocationFnRef, ForDefinition);
      SILValue makeInvocationFn = B.createFunctionRefFor(loc, makeInvocationFnSIL);

      ApplyInst *invocationValue;
      BeginAccessInst *invocationValueAccess;
      SILValue encoderTemp = emitTemporaryAllocation(loc, invocationEncoderTy);
      {
//        emitDistributedActorSystemWitnessCall(
//              B, loc, ctx.Id_makeInvocationEncoder,
//              /*base=*/systemRef,
//              /*actorTypeSub=*/SILType(),
//              /*args=*/ { });

        invocationValue = B.createApply(
            loc, makeInvocationFn,
            /*subs=*/F.getForwardingSubstitutionMap(),
            /*args=*/{systemRef});

        B.createStore(loc, invocationValue, encoderTemp,
                      StoreOwnershipQualifier::Trivial);

        invocationValueAccess =
            B.createBeginAccess(loc, encoderTemp,
                                SILAccessKind::Modify,
                                SILAccessEnforcement::Static,
                                false,
                                false);

        fprintf(stderr, "[%s:%d] (%s) invocation Value: \n", __FILE__, __LINE__, __FUNCTION__);
        invocationValue->dump();
        fprintf(stderr, "[%s:%d] (%s) invocation Value access: \n", __FILE__, __LINE__, __FUNCTION__);
        invocationValueAccess->dump();
      }

      // We need to maintain a "next normal basic block" pointer because
      // we cannot just emit a bunch of tryApply right after one another
      // but each subsequent call must be in its own basic block on the
      // 'normal' path.
      SILBasicBlock *nextNormalBB = nullptr;

      // === -------------------------------------------------------------------
      // populate the invocation:
      // - recordGenericSubstitution // TODO(distributed): implement
      // - recordArgument // TODO(distributed): implement
      // - recordErrorType // TODO(distributed): implement
      // - recordReturnType // TODO(distributed): implement
      // === recordGenericSubstitution
      {
        // TODO(distributed): record substitutions
      }

      // === encoder.recordArgument(s)
      {
//          auto recordArgumentFnDecl =
//              selfTyDecl->getDistributedActorInvocationRecordArgumentFunction();
//          auto recordArgumentFnRef = SILDeclRef(recordArgumentFnDecl);
//          assert(remoteCallFnDecl && "no recordArgument func found!");
//
//          auto recordArgumentFnSIL =
//              builder.getOrCreateFunction(loc, recordArgumentFnRef, ForDefinition);
//          SILValue recordArgumentFn = B.createFunctionRefFor(loc, recordArgumentFnSIL);
//
//          for (const auto &param : params) {
//            if (nextNormalBB)
//              B.emitBlock(nextNormalBB);
//
//            auto recordArgumentNormalBB = createBasicBlock();
//            auto recordArgumentErrorBB = createBasicBlock();
//            nextNormalBB = recordArgumentNormalBB;
//
//            emitDistributedWitnessCall(
//                B, loc, ctx.Id_recordArgument,
//                invocationValue,
//                invocationEncoderProto, getLoweredType(selfTy),
//                /*args=*/{},
//                std::make_pair(nextNormalBB, recordArgumentErrorBB)
//                );
//          }
      }

      {
        // TODO(distributed): record error type
      }

      {
        // TODO(distributed): record return type
      }


      // === doneRecording
      auto doneRecordingNormalBB = createBasicBlock();
      auto doneRecordingErrorBB = createBasicBlock();
      dynamicBasicErrorBlocks.push_back(doneRecordingErrorBB);
      {
        if (nextNormalBB) {
          B.emitBlock(nextNormalBB);
        }
        nextNormalBB = doneRecordingNormalBB;

//          auto doneRecordingFnDecl =
//              selfTyDecl->getDistributedActorSystemMakeInvocationFunction();

        NominalTypeDecl *invocationEncoderNominal =
            invocationEncoderTy.getNominalOrBoundGenericNominal();
        assert(invocationEncoderNominal);
        FuncDecl *doneRecordingFnDecl =
            ctx.getDoneRecordingOnDistributedInvocationEncoder(
                invocationEncoderNominal);
        assert(doneRecordingFnDecl);
        auto doneRecordingFnRef = SILDeclRef(doneRecordingFnDecl);
        assert(doneRecordingFnDecl && "no remoteCall func found!");

        auto doneRecordingFnSIL =
            builder.getOrCreateFunction(loc, doneRecordingFnRef, ForDefinition);
        SILValue doneRecordingFn = B.createFunctionRefFor(loc, doneRecordingFnSIL);
        doneRecordingFn->dump();

        B.createTryApply(
            loc, doneRecordingFn,
            /*subs=*/F.getForwardingSubstitutionMap(),
            /*args=*/{invocationValueAccess},
            /*normalBB=*/doneRecordingNormalBB,
            /*errorBB*/doneRecordingErrorBB);

        //          emitDistributedActorSystemWitnessCall(
//              B, loc, ctx.Id_doneRecording,
//              encoderTemp, /*actorTypeSubs*/SILType(),
//              /*args=*/ { encoderTemp },
//              std::make_pair(doneRecordingNormalBB, doneRecordingErrorBB));
//          nextNormalBB = doneRecordingNormalBB;
      }

      // === create the RemoteCallTarget
      {
        B.emitBlock(nextNormalBB);
        B.createEndAccess(loc, invocationValueAccess, /*aborted=*/false);

        // ---------------------------------------------------------------------
        // ---------------------------------------------------------------------
        // ---------------------------------------------------------------------
        auto mangledName = thunk.mangle(SILDeclRef::ManglingKind::Default);
        auto mangledNameRef = llvm::StringRef(mangledName.c_str(), mangledName.size()); // FIXME(distributed): can just pass the mangledName?

        // --- Get the `RemoteCallTarget` type
//        auto remoteCallTargetDecl = ctx.getRemoteCallTargetDecl();
//        auto remoteCallTargetTy =
//            getLoweredType(remoteCallTargetDecl->getInterfaceType());

//        auto remoteCallTargetTy = getLoweredType(ctx.getRemoteCallTargetType()); // WAS OK

        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        auto remoteCallTargetDecl = ctx.getRemoteCallTargetDecl();
        auto remoteCallTargetTy = F.mapTypeIntoContext(remoteCallTargetDecl->getDeclaredInterfaceType());
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %28 = alloc_stack $RemoteCallTarget, let, name "target" // users: %58, %57, %50, %77, %76, %37
        auto remoteCallTargetValue = B.createAllocStack(loc, getLoweredType(remoteCallTargetTy));
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

//        // %29 = metatype $@thin RemoteCallTarget.Type // user: %37
//        auto remoteCallTargetMetaTy = B.createMetatype(loc, remoteCallTargetTy);
//
        auto remoteCallTargetMetatype = getLoweredType(MetatypeType::get(remoteCallTargetTy));
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);
        auto remoteCallTargetMetatypeValue = B.createMetatype(loc, remoteCallTargetMetatype);
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %30 = string_literal utf8 "MANGLED_NAME" // user: %35
        auto mangledNameLiteral =
            B.createStringLiteral(loc, mangledNameRef,
                                  StringLiteralInst::Encoding::UTF8);
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %31 = integer_literal $Builtin.Word, 12 // user: %35
        auto codeUnitCountLiteral =
            B.createIntegerLiteral(loc,
                                   SILType::getBuiltinWordType(ctx),
                                   mangledName.size());
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %32 = integer_literal $Builtin.Int1, -1 // user: %35
        auto isAsciiLiteral =
            B.createIntegerLiteral(loc,
                                   SILType::getBuiltinIntegerType(1, ctx),
                                   -1);
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %33 = metatype $@thin String.Type // user: %35
        auto StringNominalTy = ctx.getStringDecl();
//        auto StringTy = getLoweredType(ctx.getStringType());
//        auto StringMetaTy = B.createMetatype(loc, StringTy);
        auto StringMetaTy = CanMetatypeType::get(CanType(ctx.getStringType()), MetatypeRepresentation::Thin);
        auto stringSelf =
            B.createMetatype(loc,
                             SILType::getPrimitiveObjectType(StringMetaTy));
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);
        F.dump();
        fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);


        // // function_ref String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)
        // %34 = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String // user: %35
        fprintf(stderr, "[%s:%d] (%s) HERE 1\n", __FILE__, __LINE__, __FUNCTION__);
        auto stringInitDeclRef = ctx.getStringBuiltinInitDecl(StringNominalTy); // FIXME ????
        stringInitDeclRef.dump();
        fprintf(stderr, "[%s:%d] (%s) HERE 1\n", __FILE__, __LINE__, __FUNCTION__);
        auto stringInitRef = SILDeclRef(stringInitDeclRef.getDecl(), SILDeclRef::Kind::Allocator);
        stringInitRef.dump();
        fprintf(stderr, "[%s:%d] (%s) HERE 1\n", __FILE__, __LINE__, __FUNCTION__);
         auto stringInitFn = M.findFunction(stringInitRef.mangle(), SILLinkage::PublicExternal);
        fprintf(stderr, "[%s:%d] (%s) HERE 1\n", __FILE__, __LINE__, __FUNCTION__);
        auto stringInitFnRef = B.createFunctionRef(loc, stringInitFn);
        stringInitFnRef->dump();
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

        // %35 = apply %34(%30, %31, %32, %33) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String // user: %37
        auto mangledNameString =
            B.createApply(loc, stringInitFnRef, {},
                          /*args*/{mangledNameLiteral, codeUnitCountLiteral,
                           isAsciiLiteral, stringSelf});
        fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);


        fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);
        F.dump();
        fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);

        // ---------------------------------------------------------------------
        // ---------------------------------------------------------------------
        // ---------------------------------------------------------------------
      }

      // === Call the remoteCall on the actor system
      {

      }

      fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);
      F.dump();
      fprintf(stderr, "[%s:%d] (%s) ---------------------------\n", __FILE__, __LINE__, __FUNCTION__);

      // Emit all basic error blocks which handle errors thrown by invocation
      // preparing calls; All those blocks just branch to errorBB.
      for (const auto &dynamicErrorBB : dynamicBasicErrorBlocks) {
        B.emitBlock(dynamicErrorBB);
        SILValue error = dynamicErrorBB->createPhiArgument(
            fnConv.getSILErrorType(getTypeExpansionContext()),
            OwnershipKind::Owned);

        B.createBranch(loc, errorBB, {error});
      }
    }
  } // end of `if isRemote { ... }`

  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  F.dump();
  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ----------------------------------------------------------------\n", __FILE__, __LINE__, __FUNCTION__);

  // // else
  // {
  //   return (try)? (await)? self.X(...)
  // }
  {
    B.setInsertionPoint(isLocalBB);
    B.emitBlock(isLocalBB);

    auto nativeMethodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                            native);
    auto nativeFnSILTy = SILType::getPrimitiveObjectType(nativeMethodTy);
    auto nativeSilFnType = nativeFnSILTy.castTo<SILFunctionType>();

    bool isClassMethod = false;
    if (auto classDecl = dyn_cast<ClassDecl>(fd->getDeclContext())) {
      if (!classDecl->isFinal() && !fd->isFinal() &&
          !fd->hasForcedStaticDispatch())
        isClassMethod = true;
    }

    SILValue nativeFn;
    if (isClassMethod) {
      nativeFn = emitClassMethodRef(
        loc, params[params.size() - 1], native, nativeMethodTy);
    } else {
      nativeFn = emitGlobalFunctionRef(loc, native);
    }
    auto subs = F.getForwardingSubstitutionMap();

    if (nativeSilFnType->hasErrorResult()) {
      B.createTryApply(loc, nativeFn, subs, params, localReturnBB, localErrorBB);
    } else {
      auto result = B.createApply(loc, nativeFn, subs, params);
      B.createBranch(loc, returnBB, {result});
    }
  }

  // TODO(distributed): to use a emitAndBranch local function, since these four blocks are so similar

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

  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  F.dump();
  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ================================================================\n", __FILE__, __LINE__, __FUNCTION__);
}
