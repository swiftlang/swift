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
          loc, value, LoadOwnershipQualifier::Copy);
    }

    value = SGF.B.emitCopyValueOperation(loc, value);
    SGF.B.emitStoreValueOperation(
        loc, value, fieldAddr, StoreOwnershipQualifier::Init);
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

// MARK: local instance initialization

/// For the initialization of a local distributed actor instance, emits code to initialize the instance's
/// stored property corresponding to the transport.
static void emitTransportInit(SILGenFunction &SGF,
                                        ConstructorDecl *ctor,
                                        SILLocation loc,
                                        ManagedValue actorSelf) {
  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();
  auto &C = ctor->getASTContext();

  // Sema has already guaranteed that there is exactly one ActorTransport
  // argument to the constructor, so we grab the first one from the params.
  SILValue transportArg = findFirstActorTransportArg(SGF.F);
  VarDecl *var = lookupProperty(classDecl, C.Id_actorTransport);
  assert(var);
      
  initializeProperty(SGF, loc, actorSelf.getValue(), var, transportArg);
}

/// Emits the distributed actor's identity (`id`) initialization.
///
/// Specifically, it performs:
/// \verbatim
///     self.id = transport.assignIdentity(Self.self)
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

  SILValue transport = findFirstActorTransportArg(F);

  // --- create a temporary storage for the result of the call
  // it will be deallocated automatically as we exit this scope
  VarDecl *var = lookupProperty(classDecl, C.Id_id);
  auto resultTy = SGF.getLoweredType(
      F.mapTypeIntoContext(var->getInterfaceType()));
  auto temp = SGF.emitTemporaryAllocation(loc, resultTy);

  // --- emit the call itself.
  emitActorTransportWitnessCall(
      B, loc, C.Id_assignIdentity, transport, SGF.getLoweredType(selfTy),
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
      SGF.emitResignIdentityCall(l, actorDecl,
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

  SILValue transport = findFirstActorTransportArg(F);

  FullExpr scope(Cleanups, CleanupLocation(loc));
  auto borrowedSelf = actorSelf.borrow(*this, loc);

  emitActorReadyCall(B, loc, borrowedSelf.getValue(), transport);
}

// MARK: remote instance initialization

/// Synthesize the distributed actor's identity (`id`) initialization:
///
/// \verbatim
///     transport.resolve(_, as:)
/// \endverbatim
static void createDistributedActorFactory_resolve(
    SILGenFunction &SGF, ASTContext &C, FuncDecl *fd, SILValue identityValue,
    SILValue transportValue, Type selfTy, SILValue selfMetatypeValue,
    SILType resultTy, SILBasicBlock *normalBB, SILBasicBlock *errorBB) {
  auto &B = SGF.B;

  auto loc = SILLocation(fd);
  loc.markAutoGenerated();

  // // ---- actually call transport.resolve(id, as: Self.self)
  emitActorTransportWitnessCall(
      B, loc, C.Id_resolve, transportValue, SGF.getLoweredType(selfTy),
      { identityValue, selfMetatypeValue },
      std::make_pair(normalBB, errorBB));
}

/// Function body of:
/// \verbatim
/// DistributedActor.resolve(
///     _ identity: Identity,
///     using transport: ActorTransport
/// ) throws -> Self where Identity: ActorIdentity
/// \endverbatim
void SILGenFunction::emitDistributedActorFactory(FuncDecl *fd) {
  /// NOTE: this will only be reached if the resolve function is actually
  ///       demanded. For example, by declaring the actor as `public` or
  ///       having at least one call to the resolve function.

  auto &C = getASTContext();
  SILLocation loc = fd;

  // ==== Prepare argument references
  // --- Parameter: identity
  SILArgument *identityArg = F.getArgument(0);

  // --- Parameter: transport
  SILArgument *transportArg = F.getArgument(1);

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

  SILFunctionConventions fnConv = F.getConventions(); // TODO: no idea?

  // --- get the uninitialized allocation from the runtime system.
  FullExpr scope(Cleanups, CleanupLocation(fd));

  auto optionalReturnTy = SILType::getOptionalType(returnTy);

  // ==== Call `try transport.resolve(id, as: Self.self)`
  {
    createDistributedActorFactory_resolve(
        *this, C, fd, identityArg, transportArg, selfTy, selfMetatypeValue,
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
                       identityArg);

    initializeProperty(*this, loc, remote,
                       lookupProperty(classDecl, C.Id_actorTransport),
                       transportArg);

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

// MARK: transport.resignIdentity()

void SILGenFunction::emitResignIdentityCall(SILLocation loc,
                                            ClassDecl *actorDecl,
                                            ManagedValue actorSelf) {
  ASTContext &ctx = getASTContext();
  
  FormalEvaluationScope scope(*this);

  // ==== locate: self.id
  auto idRef = emitActorPropertyReference(
      *this, loc, actorSelf.getValue(), lookupProperty(actorDecl, ctx.Id_id));

  // ==== locate: self.actorTransport
  auto transportRef = emitActorPropertyReference(
      *this, loc, actorSelf.getValue(),
      lookupProperty(actorDecl, ctx.Id_actorTransport));

  // Perform the call.
  emitActorTransportWitnessCall(
      B, loc, ctx.Id_resignIdentity,
      transportRef,
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
  
  // we only transport.resignIdentity if we are a local actor,
  // and thus the address was created by transport.assignIdentity.
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

    emitResignIdentityCall(loc, actorDecl, actorSelf);
    
    B.createBranch(loc, continueBB);
  }
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
  //  // destroy only self.id and self.actorTransport
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
  //   return try await self._remote_X(...)
  // }
  {
    B.emitBlock(isRemoteBB);

    auto *selfTyDecl = FunctionDC->getParent()->getSelfNominalTypeDecl();
    assert(selfTyDecl && "distributed instance method declared outside of actor");

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
}
