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
  assert(false); // FIXME(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!(!!)!!!!
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

// MARK: system.makeInvocationEncoder()

void SILGenFunction::emitDistributedActorSystemMakeInvocationEncoderCall(
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

/// Emit a basic block that accepts an error, emits pending cleanups
static void emitThrowWithCleanupBasicBlock(SILGenFunction &SGF, SILLocation loc,
                                           SILDeclRef thunk,
                                           SILBasicBlock *errorBB,
                                           SILBasicBlock *throwBB) {
  if (!errorBB)
    return;

  auto &B = SGF.B;
  auto &SGM = SGF.SGM;

  auto methodTy =
      SGM.Types.getConstantOverrideType(SGF.getTypeExpansionContext(), thunk);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
  auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);

  B.emitBlock(errorBB);

  SILValue error = errorBB->createPhiArgument(
      fnConv.getSILErrorType(SGF.getTypeExpansionContext()),
      OwnershipKind::Owned);

  SGF.Cleanups.emitCleanupsForReturn(CleanupLocation(loc), IsForUnwind);

  B.createBranch(loc, throwBB, {error});

  fprintf(stderr, "[%s:%d] (%s) EMIT THROW VVVVVVVV\n", __FILE__, __LINE__, __FUNCTION__);
  errorBB->dump();
  fprintf(stderr, "[%s:%d] (%s) EMIT THROW ^^^^^^^^\n", __FILE__, __LINE__, __FUNCTION__);
}

static void emitEncoderRecordErrorTypeCall(SILGenFunction &SGF, SILLocation loc,
                                           SILBasicBlock *block,
                                           SILBasicBlock *nextNormalBlock) {
  auto &B = SGF.B;
  auto &SGM = SGF.SGM;

  assert(false);
}

void SILGenFunction::emitDistributedThunk(SILDeclRef thunk) {
  // Check if actor is local or remote and call respective logic
  //
  // func X_distributedThunk(...) async throws -> T {
  //   if __isRemoteActor(self) {
  //     // ... prepare args ...
  //     return try await actorSystem.remoteCall(<args>)
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

  auto methodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                    thunk);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
  auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);
  auto resultType = fnConv.getSILResultType(getTypeExpansionContext());

  auto shouldRecordGenericSubstitutions = false;
  auto shouldRecordArguments = fd->getParameters()->size() > 0;
  auto shouldRecordErrorType = fd->hasThrows();
  auto shouldRecordReturnType = !resultType.isVoid();
  auto shouldDoneRecordingInBB =
      shouldRecordGenericSubstitutions ||
      shouldRecordArguments ||
      shouldRecordErrorType ||
      shouldRecordReturnType;

//  auto remoteCallBB = createBasicBlock();
//  auto recordReturnTypeBB = shouldRecordGenericSubstitutions || shouldRecordArguments ? createBasicBlock() : nullptr;
////  auto recordReturnTypeNormalBB = resultType.isVoid() ? nullptr : createBasicBlock();
////  auto recordReturnTypeErrorBB = resultType.isVoid() ? nullptr : createBasicBlock();
////  auto recordReturnTypeBB = createBasicBlock();
////  auto recordReturnTypeNormalBB = createBasicBlock();
////  auto recordReturnTypeErrorBB = createBasicBlock();
//  auto doneRecordingBB = shouldDoneRecordingInBB ? createBasicBlock() : nullptr;
////  auto doneRecordingNormalBB = createBasicBlock();
//  auto doneRecordingErrorBB = createBasicBlock();
////  auto makeInvocationNormalBB = OLDSYNTHESIS ? nullptr : createBasicBlock();
////  auto makeInvocationErrorBB = OLDSYNTHESIS ? nullptr : createBasicBlock();
//  auto remoteCallNormalBB = createBasicBlock();
//  auto makeRemoteCallTargetBB = createBasicBlock();
//  auto remoteCallErrorBB = createBasicBlock();
//  auto remoteErrorBB = createBasicBlock();
//  auto localReturnBB = createBasicBlock();
//  auto remoteReturnBB = createBasicBlock();
  auto errorBB = createBasicBlock();
  auto returnBB = createBasicBlock();

  auto *selfVarDecl = fd->getImplicitSelfDecl();

  SmallVector<SILValue, 8> params;
  bindParametersForForwarding(fd->getParameters(), params);
  bindParameterForForwarding(selfVarDecl, params);

  // === `Self` types
  auto selfValue = ManagedValue::forUnmanaged(params[params.size() - 1]); // TODO(distributed): getSelfArgument instead
  auto selfTy = selfVarDecl->getType();
  auto *selfTyDecl = FunctionDC->getParent()->getSelfNominalTypeDecl();
  assert(selfTyDecl && "distributed instance method declared outside of actor");

  // === `InvocationEncoder` types
  AbstractFunctionDecl *makeInvocationEncoderFnDecl =
      selfTyDecl->getDistributedActorSystemMakeInvocationEncoderFunction();
  assert(makeInvocationEncoderFnDecl && "no remoteCall func found!");
  auto makeInvocationEncoderFnRef = SILDeclRef(makeInvocationEncoderFnDecl);

  ProtocolDecl *invocationEncoderProto =
      ctx.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);

  auto makeInvocationEncoderMethodTy = SGM.Types.getConstantOverrideType(
      getTypeExpansionContext(), makeInvocationEncoderFnRef);
  auto makeInvocationEncoderDerivativeFnSILTy = SILType::getPrimitiveObjectType(makeInvocationEncoderMethodTy);
  auto makeInvocationEncoderSilFnType = makeInvocationEncoderDerivativeFnSILTy.castTo<SILFunctionType>();

  auto invocationEncoderResultInfo =
      makeInvocationEncoderSilFnType->getResults().begin();
  auto invocationEncoderCanTy = invocationEncoderResultInfo->getInterfaceType();
  auto invocationEncoderTy = getLoweredType(invocationEncoderCanTy);

  NominalTypeDecl *invocationEncoderNominal =
      invocationEncoderTy.getNominalOrBoundGenericNominal();

  // ==== ----------------------------------------------------------------------

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  auto isLocalBB = createBasicBlock();
  auto isRemoteBB = createBasicBlock();
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

  // === Local Call ------------------------------------------------------------
  // {
  //   return (try)? (await)? self.X(...)
  // }
  SILBasicBlock *localReturnBB;
  SILBasicBlock *localCallErrorBB;
//  // Prepare some variables that we need to destroy etc. from returnBB/errorBB:
//  BeginAccessInst *invocationValueAccess = nullptr;
  {
    B.emitBlock(isLocalBB);
    fprintf(stderr, "[%s:%d] (%s) isLocalBB\n", __FILE__, __LINE__, __FUNCTION__);
    isLocalBB->dump();

    auto nativeMethodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                            native);
    auto nativeFnSILTy = SILType::getPrimitiveObjectType(nativeMethodTy);
    auto nativeSilFnType = nativeFnSILTy.castTo<SILFunctionType>();

    localReturnBB = createBasicBlock();
    localCallErrorBB = nativeSilFnType->hasErrorResult() ? createBasicBlock() : nullptr;

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

    if (localCallErrorBB) {
      B.createTryApply(loc, nativeFn, subs, params, localReturnBB, localCallErrorBB);
    } else {
      auto result = B.createApply(loc, nativeFn, subs, params);
      B.createBranch(loc, localReturnBB, {result});
    }
  }
  {
    B.emitBlock(localReturnBB);

    SILValue result = localReturnBB->createPhiArgument(
        resultType, OwnershipKind::Owned);
    B.createBranch(loc, returnBB, {result});
  }
  { // local error
    fprintf( stderr, "[%s:%d] (%s) CALL emitThrowWithCleanupBasicBlock: localCallErrorBB\n", __FILE__, __LINE__, __FUNCTION__);
    emitThrowWithCleanupBasicBlock(*this, loc, thunk, localCallErrorBB, errorBB);
  }


  // === Remote Call -----------------------------------------------------------
  // {
  //   var invocation = try self.actorSystem.makeInvocation()
  //   // ...
  // }
  {
    B.emitBlock(isRemoteBB);
    fprintf(stderr, "[%s:%d] (%s) isRemoteBB\n", __FILE__, __LINE__, __FUNCTION__);
    isRemoteBB->dump();
//
//    if (OLDSYNTHESIS) {
//      // return try await self._remote_X(...)
//      auto remoteFnDecl = selfTyDecl->lookupDirectRemoteFunc(fd);
//      assert(remoteFnDecl &&
//             "Could not find _remote_<dist_func_name> function");
//      auto remoteFnRef = SILDeclRef(remoteFnDecl);
//
//      SILGenFunctionBuilder builder(SGM);
//      auto remoteFnSIL =
//          builder.getOrCreateFunction(loc, remoteFnRef, ForDefinition);
//      SILValue remoteFn = B.createFunctionRefFor(loc, remoteFnSIL);
//
//      auto subs = F.getForwardingSubstitutionMap();
//
//      SmallVector<SILValue, 8> remoteParams(params);
//
//      B.createTryApply(loc, remoteFn, subs, remoteParams, remoteReturnBB,
//                       remoteErrorBB);
//    }

    // -----------------------------------------------------------------------
    // === get the actorSystem property
    auto systemRef = emitActorPropertyReference(
        *this, loc, selfValue.getValue(),
        lookupProperty(selfTyDecl, ctx.Id_actorSystem));

    SILGenFunctionBuilder builder(SGM);

    // We need to maintain a "next normal basic block" pointer because
    // we cannot just emit a bunch of tryApply right after one another
    // but each subsequent call must be in its own basic block on the
    // 'normal' path.
    SILBasicBlock *nextNormalBB = nullptr;

//    // TODO: jump to record generics or arguments
//    if (recordErrorTypeBB) {
//      nextNormalBB = recordErrorTypeBB;
//    } else if (recordReturnTypeBB) {
//      nextNormalBB = recordReturnTypeBB;
//    } else {
//      nextNormalBB = doneRecordingBB;
//    }

    // === -------------------------------------------------------------------
    // var encoder = actorSystem.makeInvocationEncoder()
    auto makeInvocationEncoderFnSIL =
        builder.getOrCreateFunction(loc, makeInvocationEncoderFnRef, ForDefinition);
    SILValue makeInvocationEncoderFn =
        B.createFunctionRefFor(loc, makeInvocationEncoderFnSIL);

    SILValue encoderBuf = emitTemporaryAllocation(loc, invocationEncoderTy);
    ManagedValue encoderTemp = emitManagedBufferWithCleanup(encoderBuf); // FIXME: rename
    {
      ApplyInst *invocationValue = B.createApply(
          loc, makeInvocationEncoderFn,
          /*subs=*/SubstitutionMap(),
          /*args=*/{systemRef});

      B.createStore(loc, invocationValue, encoderTemp.getValue(),
                    StoreOwnershipQualifier::Trivial);

//        invocationValueAccess =
//            B.createBeginAccess(loc, encoderTemp.getValue(),
//                                SILAccessKind::Modify,
//                                SILAccessEnforcement::Static,
//                                false,
//                                false);
//      B.createBranch(loc, nextNormalBB);
    }

    // === -------------------------------------------------------------------
    // populate the invocation:

    auto firstOfThrowingApplyBBs = true;

    // TODO: generics

    SILBasicBlock *recordErrorTypeBB = nullptr;
    SILBasicBlock *recordErrorTypeErrorBB = nullptr;
    if (shouldRecordErrorType) {
      if (!firstOfThrowingApplyBBs)
        recordErrorTypeBB = createBasicBlock();
      recordErrorTypeErrorBB = createBasicBlock();
      firstOfThrowingApplyBBs = false;
      nextNormalBB = recordErrorTypeBB;
    }

    SILBasicBlock *recordReturnTypeBB = nullptr;
    SILBasicBlock *recordReturnTypeErrorBB = nullptr;
    if (shouldRecordReturnType) {
      if (!firstOfThrowingApplyBBs)
        recordReturnTypeBB = createBasicBlock();
      recordReturnTypeErrorBB = createBasicBlock();
      firstOfThrowingApplyBBs = false;
      nextNormalBB = recordReturnTypeBB;
    }

    SILBasicBlock *recordingDoneBB = nullptr;
    SILBasicBlock *recordingDoneErrorBB = nullptr;
    if (!firstOfThrowingApplyBBs)
      recordingDoneBB = createBasicBlock();
    firstOfThrowingApplyBBs = false;
    recordingDoneErrorBB = createBasicBlock();
    if (!nextNormalBB) {
      nextNormalBB = recordReturnTypeBB;
    }

    // === recordGenericSubstitution
    {
      // TODO(distributed): record substitutions

    }

    // === encoder.recordArgument(s)
    if (shouldRecordArguments) {
      assert(false && "record arguments");
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

    // === encoder.recordErrorType
    if (shouldRecordErrorType) {
      if (recordErrorTypeBB)
        B.emitBlock(recordErrorTypeBB);

      if (recordReturnTypeBB) {
        nextNormalBB = recordReturnTypeBB;
      } else if (recordingDoneBB) {
        nextNormalBB = recordingDoneBB;
      }

      // Get the error type.
      // If we ever did typed-throws we'd get the error type from fd here...
      auto errorTy = F.mapTypeIntoContext(
          ctx.getErrorDecl()->getInterfaceType());
      auto errorMetatype = getLoweredType(MetatypeType::get(errorTy));
      auto errorMetatypeValue = B.createMetatype(loc, errorMetatype); // TODO(distributed): we create this in two places, optimize a bit

      // Get the function
      FuncDecl *recordErrorTyFnDecl =
          ctx.getRecordErrorTypeOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      assert(recordErrorTyFnDecl);
      auto recordErrorTyFnRef = SILDeclRef(recordErrorTyFnDecl);
      auto recordErrorFnSIL =
          builder.getOrCreateFunction(loc, recordErrorTyFnRef, ForDefinition);
      SILValue recordErrorTyFn = B.createFunctionRefFor(loc, recordErrorFnSIL);
      recordErrorTyFn->dump();

      // Prepare the <E: Error> substitution,
      // but just fill it with Error anyway.
      auto errorSubs = SubstitutionMap();

      B.createTryApply(
          loc, recordErrorTyFn,
          /*subs*/errorSubs,
          /*args*/{ errorMetatypeValue, encoderTemp.getValue() },
          /*normalBB*/nextNormalBB,
          /*errorBB*/errorBB);
    }
    {
      emitThrowWithCleanupBasicBlock(*this, loc, thunk, recordErrorTypeErrorBB, errorBB);
    }

    if (recordReturnTypeBB) {
      // TODO(distributed): record return type
      assert(false);
    }

    // === try doneRecording
//      SILValue encoderTempAccess; // 222222
//    dynamicBasicErrorBlocks.push_back(doneRecordingErrorBB);
    SILBasicBlock *makeRemoteCallTargetBB = createBasicBlock();
    {
      if (recordingDoneBB)
        B.emitBlock(recordingDoneBB);

      assert(invocationEncoderNominal);
      FuncDecl *doneRecordingFnDecl =
          ctx.getDoneRecordingOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      assert(doneRecordingFnDecl);
      auto doneRecordingFnRef = SILDeclRef(doneRecordingFnDecl);

      auto doneRecordingFnSIL =
          builder.getOrCreateFunction(loc, doneRecordingFnRef, ForDefinition);
      SILValue doneRecordingFn = B.createFunctionRefFor(loc, doneRecordingFnSIL);

//        encoderTempAccess =
//            B.createBeginAccess(loc, encoderTemp.getValue(),
//                                SILAccessKind::Modify,
//                                SILAccessEnforcement::Static,
//                                false,
//                                false); // 22222

      B.createTryApply(
          loc, doneRecordingFn,
          /*subs=*/SubstitutionMap(), // FIXME(distributed): (!!!)
          /*args=*/{encoderTemp.getValue()},
          /*normalBB=*/makeRemoteCallTargetBB,
          /*errorBB*/recordingDoneErrorBB);

      //          emitDistributedActorSystemWitnessCall(
//              B, loc, ctx.Id_doneRecording,
//              encoderTemp, /*actorTypeSubs*/SILType(),
//              /*args=*/ { encoderTemp },
//              std::make_pair(doneRecordingNormalBB, doneRecordingErrorBB));
//          nextNormalBB = doneRecordingNormalBB;
    }
    {
      emitThrowWithCleanupBasicBlock(*this, loc, thunk, recordingDoneErrorBB, errorBB);
    }

    // === create the RemoteCallTarget
    auto remoteCallTargetDecl = ctx.getRemoteCallTargetDecl();
    auto remoteCallTargetTy = F.mapTypeIntoContext(remoteCallTargetDecl->getDeclaredInterfaceType());
    ManagedValue remoteCallTargetValue;
    SILBasicBlock *remoteCallReturnBB = createBasicBlock();
    SILBasicBlock *remoteCallErrorBB = createBasicBlock();
    {
      if (makeRemoteCallTargetBB)
        B.emitBlock(makeRemoteCallTargetBB);
//        B.createEndAccess(loc, encoderTemp.getValue(), /*aborted=*/false);

      auto mangledName = thunk.mangle(SILDeclRef::ManglingKind::Default);
      auto mangledNameRef = llvm::StringRef(mangledName.c_str(), mangledName.size()); // FIXME(distributed): can just pass the mangledName?

      // --- Get the `RemoteCallTarget` type
//        auto remoteCallTargetDecl = ctx.getRemoteCallTargetDecl();
//        auto remoteCallTargetTy =
//            getLoweredType(remoteCallTargetDecl->getInterfaceType());

//        auto remoteCallTargetTy = getLoweredType(ctx.getRemoteCallTargetType()); // WAS OK

      // %28 = alloc_stack $RemoteCallTarget, let, name "target" // users: %58, %57, %50, %77, %76, %37
//        remoteCallTargetValue = B.createAllocStack(loc, getLoweredType(remoteCallTargetTy));
      auto remoteCallTargetBuf = emitTemporaryAllocation(loc, getLoweredType(remoteCallTargetTy));
      // native.forwardInto(SGF, loc, buf);
      remoteCallTargetValue = emitManagedBufferWithCleanup(remoteCallTargetBuf);

//        // %29 = metatype $@thin RemoteCallTarget.Type // user: %37
      auto remoteCallTargetMetatype = getLoweredType(MetatypeType::get(remoteCallTargetTy));
      auto remoteCallTargetMetatypeValue = B.createMetatype(loc, remoteCallTargetMetatype);

      // %30 = string_literal utf8 "MANGLED_NAME" // user: %35
      // TODO: ManagedValue ... = emitStringLiteral
      auto mangledNameLiteral =
          B.createStringLiteral(loc, mangledNameRef,
                                StringLiteralInst::Encoding::UTF8);
      fprintf(stderr, "[%s:%d] (%s) HERE\n", __FILE__, __LINE__, __FUNCTION__);

      // %31 = integer_literal $Builtin.Word, 12 // user: %35
      auto codeUnitCountLiteral =
          B.createIntegerLiteral(loc,
                                 SILType::getBuiltinWordType(ctx),
                                 mangledName.size());

      // %32 = integer_literal $Builtin.Int1, -1 // user: %35
      auto isAsciiLiteral =
          B.createIntegerLiteral(loc,
                                 SILType::getBuiltinIntegerType(1, ctx),
                                 -1);

      // %33 = metatype $@thin String.Type // user: %35
//        auto StringNominalTy = ctx.getStringDecl();
//        auto StringTy = getLoweredType(ctx.getStringType());
//        auto StringMetaTy = B.createMetatype(loc, StringTy);
      auto StringMetaTy = CanMetatypeType::get(CanType(ctx.getStringType()), MetatypeRepresentation::Thin);
      auto stringSelf =
          B.createMetatype(loc,
                           SILType::getPrimitiveObjectType(StringMetaTy));

      // // function_ref String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)
      // %34 = function_ref @$sSS21_builtinStringLiteral17utf8CodeUnitCount7isASCIISSBp_BwBi1_tcfC : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String // user: %35
      auto stringInitDeclRef = ctx.getStringBuiltinInitDecl(ctx.getStringDecl());
      auto stringInitRef = SILDeclRef(stringInitDeclRef.getDecl(), SILDeclRef::Kind::Allocator);
      // NotForDefinition since it seems the body of the init function is not there yet:
       auto stringInitFn = builder.getOrCreateFunction(loc, stringInitRef, NotForDefinition);
      auto stringInitFnRef = B.createFunctionRef(loc, stringInitFn);

      // %35 = apply %34(%30, %31, %32, %33) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String // user: %37
      // auto x = std::move(mangledNameLiteral).getScalarValue()
      auto mangledNameStringValue =
          B.createApply(loc, stringInitFnRef, {},
                        /*args*/{mangledNameLiteral, codeUnitCountLiteral,
                         isAsciiLiteral, stringSelf});

      // --- Create the RemoteCallTarget instance, passing the mangledNameString
      // function_ref RemoteCallTarget.init(_mangledName:)
      // %36 = function_ref @$s12_Distributed16RemoteCallTargetV12_mangledNameACSS_tcfC : $@convention(method) (@owned String, @thin RemoteCallTarget.Type) -> @out RemoteCallTarget // user: %37
      auto remoteCallTargetInitDecl = remoteCallTargetDecl->getDistributedRemoteCallTargetInitFunction();
      assert(remoteCallTargetInitDecl && "no 'RemoteCallTarget.init' found!");
      auto remoteCallTargetInitRef = SILDeclRef(remoteCallTargetInitDecl);
      auto remoteCallTargetInitFnSIL =
          builder.getOrCreateFunction(loc, remoteCallTargetInitRef, ForDefinition);
      SILValue remoteCallTargetInitFn = B.createFunctionRefFor(loc, remoteCallTargetInitFnSIL);

      // %37 = apply %36(%28, %35, %29) : $@convention(method) (@owned String, @thin RemoteCallTarget.Type) -> @out RemoteCallTarget
      B.createApply(
          loc, remoteCallTargetInitFn, {},
          {/*out*/ remoteCallTargetValue.getValue(), mangledNameStringValue,
           remoteCallTargetMetatypeValue});

      // B.createBranch(loc, remoteCallBB, {remoteCallTargetValue.getValue()}); /// 1111111
//      B.createBranch(loc, remoteCallBB);
//    }
//
//    // === Call the remoteCall on the actor system
//    {
//      B.emitBlock(remoteCallBB);
//        SILValue remoteCallTargetValue = remoteCallBB->createPhiArgument( // TODO: do we need this?????????????????????????
//            getLoweredType(remoteCallTargetTy), OwnershipKind::Owned); // 111111111

      // --- Prepare storage for the return value
      // %38 = alloc_stack $String // users: %54, %56, %50, %75
      // auto remoteCallReturnValue = B.createAllocStack(loc, resultType);
      auto remoteCallReturnBuf = emitTemporaryAllocation(loc, resultType);
      auto remoteCallReturnValue = emitManagedBufferWithCleanup(remoteCallReturnBuf);

      // --- Prepare 'throwing' type, Error or Never depending on throws of the target
      SILValue thrownErrorMetatypeValue;
      if (fd->hasThrows()) {
        auto errorTy = F.mapTypeIntoContext(
            ctx.getErrorDecl()->getInterfaceType());
        auto errorMetatype = getLoweredType(MetatypeType::get(errorTy));
        thrownErrorMetatypeValue = B.createMetatype(loc, errorMetatype);
      } else {
        auto neverTy = F.mapTypeIntoContext(
            ctx.getNeverType());
        auto neverMetatype = getLoweredType(MetatypeType::get(neverTy));
        thrownErrorMetatypeValue = B.createMetatype(loc, neverMetatype);
      }
      assert(thrownErrorMetatypeValue);

      // --- Prepare 'returning' type, can be 'Void' or specific type
      SILValue returnMetatypeValue;
      switch (methodTy->getNumResults()) {
      case 0: {
        auto voidTy = ctx.getVoidType();
        auto voidMetatype =
            getLoweredType(MetatypeType::get(voidTy, MetatypeRepresentation::Thick));
        // %42 = metatype $@thin Never.Type
        // %43 = metatype $@thick Never.Type /// we just have this one
        returnMetatypeValue = B.createMetatype(loc, voidMetatype);
        break;
      }
      case 1: {
        CanType returnType = methodTy->getSingleResult().getInterfaceType();
        auto returnMetatype = getLoweredType(MetatypeType::get(returnType, MetatypeRepresentation::Thick));
        returnMetatypeValue = B.createMetatype(loc, returnMetatype);
        break;
      }
      default:
        llvm_unreachable("Can't support more results than one.");
      }
      assert(returnMetatypeValue);

      // function_ref FakeActorSystem.remoteCall<A, B, C>(on:target:invocationDecoder:throwing:returning:)
      // %49 = function_ref @$s27FakeDistributedActorSystems0aC6SystemV10remoteCall2on6target17invocationDecoder8throwing9returningq0_x_01_B006RemoteG6TargetVAA0A10InvocationVzq_mq0_mSgtYaKAJ0bC0RzSeR0_SER0_AA0C7AddressV2IDRtzr1_lF : $@convention(method) @async <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : DistributedActor, τ_0_2 : Decodable, τ_0_2 : Encodable, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @in_guaranteed RemoteCallTarget, @inout FakeInvocation, @thick τ_0_1.Type, Optional<@thick τ_0_2.Type>, @guaranteed FakeActorSystem) -> (@out τ_0_2, @error Error) // user: %50
      auto remoteCallFnDecl =
          selfTyDecl->getDistributedActorSystemRemoteCallFunction();
      assert(remoteCallFnDecl && "no remoteCall func found!");
      auto remoteCallFnRef = SILDeclRef(remoteCallFnDecl);
      auto remoteCallFnSIL =
          builder.getOrCreateFunction(loc, remoteCallFnRef, ForDefinition);
      SILValue remoteCallFn = B.createFunctionRefFor(loc, remoteCallFnSIL);

      // --- prepare subs for the 'remoteCall'
      // <MyDistActor, ErrorType, ReturnType>
      SubstitutionMap remoteCallSubs = SubstitutionMap();
      if (false) { // FIXME(distributed): implement the signatures !!!!!!
        auto remoteCallGenericSig = remoteCallFnDecl->getGenericSignature();
        SmallVector<Type, 3> subTypes;
        SmallVector<ProtocolConformanceRef, 3> subConformances;
        subTypes.push_back(getLoweredType(selfTy).getASTType());

        // FIXME(distributed): get the conformances right here...

        remoteCallSubs =
            SubstitutionMap::get(remoteCallGenericSig,
                                 subTypes, subConformances);
      }

      // try_apply %49<MyDistActor, Never, String>(%38, %2, %28, %48, %43, %46, %40) : $@convention(method) @async <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : DistributedActor, τ_0_2 : Decodable, τ_0_2 : Encodable, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @in_guaranteed RemoteCallTarget, @inout FakeInvocation, @thick τ_0_1.Type, Optional<@thick τ_0_2.Type>, @guaranteed FakeActorSystem) -> (@out τ_0_2, @error Error), normal bb5, error bb10 // id: %50
      B.createTryApply(loc, remoteCallFn,
                       remoteCallSubs,
                       /*args=*/{
                           /*out*/ remoteCallReturnValue.getValue(), // out, return value
                           selfValue.getValue(), // self
                           remoteCallTargetValue.getValue(), // target
                           encoderTemp.getValue(), // invocation encoder
                           thrownErrorMetatypeValue, // throwing type
                           returnMetatypeValue // returning type
                       },
                       /*normalBB=*/remoteCallReturnBB,
                       /*errorBB=*/remoteCallErrorBB);
    }

  //    // Emit all basic error blocks which handle errors thrown by invocation
  //    // preparing calls; All those blocks just branch to errorBB.
  //    for (const auto &dynamicErrorBB : dynamicBasicErrorBlocks) {
  //      B.emitBlock(dynamicErrorBB);
  //      SILValue error = dynamicErrorBB->createPhiArgument(
  //          fnConv.getSILErrorType(getTypeExpansionContext()),
  //          OwnershipKind::Owned);
  //
  //      B.createBranch(loc, errorBB, {error});
  //    }
//    {
//      B.emitBlock(remoteCallReturnBB);
//      SILValue result = remoteCallReturnBB->createPhiArgument(
//          resultType, OwnershipKind::Owned);
//      B.createBranch(loc, returnBB, {result});
//    }
    {
      B.emitBlock(remoteCallReturnBB);

      SILValue result = remoteCallReturnBB->createPhiArgument(
          resultType, OwnershipKind::Owned);

      Cleanups.emitCleanupsForReturn(CleanupLocation(loc), NotForUnwind);
      B.createBranch(loc, returnBB, {result});
    }
    {
      emitThrowWithCleanupBasicBlock(*this, loc, thunk, remoteCallErrorBB, errorBB);
    }
  } // end of `if isRemote { ... }`

  // Emit return logic
  {
    B.emitBlock(returnBB);

    SILValue result =
        returnBB->createPhiArgument(resultType, OwnershipKind::Owned);
    B.createReturn(loc, result);
  }

  // Emit the rethrow logic.
  {
    B.emitBlock(errorBB);

    SILValue error = errorBB->createPhiArgument(
        fnConv.getSILErrorType(getTypeExpansionContext()),
        OwnershipKind::Owned);
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
