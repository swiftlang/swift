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
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"
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
/********************* COMMON CONFORMANCE SIL PATTERNS ************************/
/******************************************************************************/

static void getSerializationRequirements(
    SILGenFunction &SGF, ASTContext &ctx, NominalTypeDecl actorDecl,
    SmallVectorImpl<ProtocolDecl *> &requirementProtocolDecls) {
  auto encodableDecl =
      ctx.getProtocol(KnownProtocolKind::Encodable); // FIXME: actually use SerializationRequirement
  requirementProtocolDecls.push_back(encodableDecl);

  auto decodableDecl =
      ctx.getProtocol(KnownProtocolKind::Decodable); // FIXME: actually use SerializationRequirement
  requirementProtocolDecls.push_back(decodableDecl);
}

// FIXME: this isn't actually implemented
static void pushSerializationRequirementConformance(
    SILGenFunction &SGF, ASTContext &ctx,
    SILType type,
    SmallVectorImpl<Type> &subTypes,
    SmallVectorImpl<ProtocolConformanceRef> &subConformances) {
  auto &B = SGF.B;
  auto module = B.getModule().getSwiftModule();

  //        auto resultEncodableTypeConfRef =
  //            module->lookupConformance(resultType.getASTType(),
  //                                      encodableRequirementTy);
  //        assert(!distributedActorConfRef.isInvalid() &&
  //               "Missing conformance to `SerializationRequirement`");
  subTypes.push_back(type.getASTType());
  //        subConformances.push_back(resultEncodableTypeConfRef);

  //        auto decodableRequirementTy =
  //            ctx.getProtocol(KnownProtocolKind::Decodable);
  //        auto resultDecodableTypeConfRef =
  //            module->lookupConformance(resultType.getASTType(),
  //                                      encodableRequirementTy);
  //        subTypes.push_back(resultType.getASTType());
  //        subConformances.push_back(resultDecodableTypeConfRef);
}

/// Push `Type` and `ProtocolConformanceRef` for the `Error` protocol to
/// `subTypes` and `subConformances`.
static void pushErrorConformance(
    SILGenFunction &SGF, ASTContext &ctx,
    SmallVectorImpl<Type> &subTypes,
    SmallVectorImpl<ProtocolConformanceRef> &subConformances) {
  auto &B = SGF.B;
  auto module = B.getModule().getSwiftModule();

  auto errorDecl = ctx.getErrorDecl();
  auto errorTy = errorDecl->getInterfaceType();
  auto thrownErrorConfRef =
      module->lookupConformance(errorDecl->getDeclaredInterfaceType(), ctx.getErrorDecl());

  assert(!thrownErrorConfRef.isInvalid() && "Missing conformance to `Error`");
  subTypes.push_back(errorTy);
  subConformances.push_back(thrownErrorConfRef);
}

static void pushNeverErrorConformance(
    SILGenFunction &SGF, ASTContext &ctx,
    SmallVectorImpl<Type> &subTypes,
    SmallVectorImpl<ProtocolConformanceRef> &subConformances) {
  auto &B = SGF.B;
  auto module = B.getModule().getSwiftModule();

  auto neverDecl = ctx.getNeverDecl();
  auto neverTy = neverDecl->getDeclaredInterfaceType();
  auto thrownErrorConfRef =
      module->lookupConformance(neverTy, ctx.getErrorDecl());

//  assert(!thrownErrorConfRef.isInvalid() && "Missing conformance to `Error`"); // FIXME(distributed): how to deal with the `: Error` !!!!
  subTypes.push_back(neverTy);
  subConformances.push_back(thrownErrorConfRef); // FIXME(distributed): how to deal with the `: Error` !!!!
}

/// Push `Type` and `ProtocolConformanceRef` for the `Self` of this distributed
/// actor to `subTypes` and `subConformances`.
static void pushDistributedActorConformance(
    SILGenFunction &SGF, ASTContext &ctx,
    SILType distributedActorSelfTy,
    SmallVectorImpl<Type> &subTypes,
    SmallVectorImpl<ProtocolConformanceRef> &subConformances) {
  auto &B = SGF.B;
  auto module = B.getModule().getSwiftModule();

  ProtocolDecl *distributedActorProto =
      ctx.getProtocol(KnownProtocolKind::DistributedActor);

  auto confRef =
      module->lookupConformance(distributedActorSelfTy.getASTType(), distributedActorProto);
  assert(!confRef.isInvalid() && "Missing conformance to `DistributedActor`");

  subTypes.push_back(distributedActorSelfTy.getASTType());
  subConformances.push_back(confRef);
}


/******************************************************************************/
/******************* COMMON (DISTRIBUTED) SIL PATTERNS ************************/
/******************************************************************************/

static void createVoidPhiArgument(SILGenFunction &SGF,
                                  ASTContext &ctx,
                                  SILBasicBlock *block) {
  block->createPhiArgument(
      SGF.getLoweredType(ctx.getVoidType()), OwnershipKind::Owned);
}

static SILValue emitSimpleLoad(SILGenFunction &SGF,
                               SILLocation loc,
                               SILValue valueRef) {
  ASTContext &ctx = SGF.getASTContext();

  if (valueRef->getType().isAddress()) {
    auto value = SGF.B.createTrivialLoadOr(
        loc, valueRef, LoadOwnershipQualifier::Unqualified, true);
    // TODO: push a release cleanup?
    return value;
  } else {
    auto value = SGF.B.emitCopyValueOperation(loc, valueRef);
    return value;
  }
}

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
static void emitActorSystemInit(SILGenFunction &SGF,
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
static void emitIDInit(SILGenFunction &SGF, ConstructorDecl *ctor,
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
  emitActorSystemInit(*this, ctor, loc, selfArg);
  emitIDInit(*this, ctor, loc, selfArg);

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
  auto returnBB = createBasicBlock("returnBB");
  auto resolvedBB = createBasicBlock("resolvedBB");
  auto makeProxyBB = createBasicBlock("makeProxyBB");
  auto switchBB = createBasicBlock("switchBB");
  auto errorBB = createBasicBlock("errorBB");

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
  auto isRemoteBB = createBasicBlock("isRemoteBB");
  auto isLocalBB = createBasicBlock("isLocalBB");

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

/******************************************************************************/
/******************* DISTRIBUTED DEINIT: class memberwise destruction *********/
/******************************************************************************/

void SILGenFunction::emitDistributedActorClassMemberDestruction(
    SILLocation cleanupLoc, ManagedValue selfValue, ClassDecl *cd,
    SILBasicBlock *normalMemberDestroyBB, SILBasicBlock *finishBB) {
  auto selfTy = cd->getDeclaredInterfaceType();

  Scope scope(Cleanups, CleanupLocation(cleanupLoc));

  auto isLocalBB = createBasicBlock("isLocalBB");
  auto remoteMemberDestroyBB = createBasicBlock("remoteMemberDestroyBB");

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
                                           SILBasicBlock *throwBB,
                                           ArrayRef<SILValue> endAccesses = {},
                                           ArrayRef<SILValue> endLifetimes = {}) {
  if (!errorBB)
    return;

  auto &B = SGF.B;
  auto &SGM = SGF.SGM;

  B.emitBlock(errorBB);

  auto methodTy =
      SGM.Types.getConstantOverrideType(SGF.getTypeExpansionContext(), thunk);
  auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
  auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
  SILFunctionConventions fnConv(silFnType, SGM.M);

  SILValue error = errorBB->createPhiArgument(
      fnConv.getSILErrorType(SGF.getTypeExpansionContext()),
      OwnershipKind::Owned);

  for (const auto &value : endAccesses) {
    B.createEndAccess(loc, value, /*aborted=*/false);
  }
  for (const auto &value : endLifetimes) { // TODO(distributed): use enterCleanup for ending lifetimes
    if (value->getType().isAddress())
      B.createEndLifetime(loc, value);
  }

  SGF.Cleanups.emitCleanupsForReturn(CleanupLocation(loc), IsForUnwind);

  B.createBranch(loc, throwBB, {error});
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

  auto errorBB = createBasicBlock("errorBB");
  auto returnBB = createBasicBlock("returnBB");

  auto *selfVarDecl = fd->getImplicitSelfDecl();

  SmallVector<SILValue, 8> paramsForForwarding;
  bindParametersForForwarding(fd->getParameters(), paramsForForwarding);
  bindParameterForForwarding(selfVarDecl, paramsForForwarding);

  // === `Self` types
  auto selfValue = ManagedValue::forUnmanaged(F.getSelfArgument());
  auto selfTy = selfVarDecl->getType();
  auto selfSILTy = getLoweredType(selfTy);
  auto *selfTyDecl = FunctionDC->getParent()->getSelfNominalTypeDecl();
  assert(selfTyDecl && "distributed instance method declared outside of actor");

  // === Thrown 'Err' type
  auto errorTy = F.mapTypeIntoContext(ctx.getErrorDecl()->getInterfaceType());
  auto neverTy = F.mapTypeIntoContext(ctx.getNeverType());
  auto errTy = fd->hasThrows() ? errorTy : neverTy;

  // === `InvocationEncoder` types
  AbstractFunctionDecl *makeInvocationEncoderFnDecl =
      selfTyDecl->getDistributedActorSystemMakeInvocationEncoderFunction();
  assert(makeInvocationEncoderFnDecl && "no remoteCall func found!");
  auto makeInvocationEncoderFnRef = SILDeclRef(makeInvocationEncoderFnDecl);

  ProtocolDecl *invocationEncoderProto =
      ctx.getProtocol(KnownProtocolKind::DistributedTargetInvocationEncoder);
  ProtocolDecl *distributedActorProto =
      ctx.getProtocol(KnownProtocolKind::DistributedActor);

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
  auto isLocalBB = createBasicBlock("isLocalBB");
  auto isRemoteBB = createBasicBlock("isRemoteBB");
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
  {
    B.emitBlock(isLocalBB);

    auto nativeMethodTy = SGM.Types.getConstantOverrideType(getTypeExpansionContext(),
                                                            native);
    auto nativeFnSILTy = SILType::getPrimitiveObjectType(nativeMethodTy);
    auto nativeSilFnType = nativeFnSILTy.castTo<SILFunctionType>();

    localReturnBB = createBasicBlock("localReturnBB");
    localCallErrorBB = nativeSilFnType->hasErrorResult() ? createBasicBlock("localCalLErrorBB") : nullptr;

    bool isClassMethod = false;
    if (auto classDecl = dyn_cast<ClassDecl>(fd->getDeclContext())) {
      if (!classDecl->isFinal() && !fd->isFinal() &&
          !fd->hasForcedStaticDispatch())
        isClassMethod = true;
    }

    SILValue nativeFn;
    if (isClassMethod) {
      nativeFn = emitClassMethodRef(
          loc, selfValue.getValue(), native, nativeMethodTy);
    } else {
      nativeFn = emitGlobalFunctionRef(loc, native);
    }
    auto subs = F.getForwardingSubstitutionMap();

    if (localCallErrorBB) {
      B.createTryApply(loc, nativeFn, subs, paramsForForwarding, localReturnBB, localCallErrorBB);
    } else {
      auto result = B.createApply(loc, nativeFn, subs, paramsForForwarding);
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
    emitThrowWithCleanupBasicBlock(*this, loc, thunk, localCallErrorBB, errorBB);
  }


  // === Remote Call -----------------------------------------------------------
  SILGenFunctionBuilder builder(SGM);
  // {
  //   var invocation = try self.actorSystem.makeInvocationEncoder()
  //   // ...
  // }
  {
    B.emitBlock(isRemoteBB);

    // We need to maintain a "next normal basic block" pointer because
    // we cannot just emit a bunch of tryApply right after one another
    // but each subsequent call must be in its own basic block on the
    // 'normal' path.
    SILBasicBlock *nextNormalBB = nullptr;

    // === -------------------------------------------------------------------
    // var encoder = actorSystem.makeInvocationEncoder()
    SILValue invocationEncoderBuf;
    ManagedValue invocationEncoder;
    {
      invocationEncoderBuf = emitTemporaryAllocation(loc, invocationEncoderTy);
      invocationEncoder = emitManagedBufferWithCleanup(invocationEncoderBuf);
      // === get the actorSystem property
      // %16 = ref_element_addr %2 : $MyDistActor, #MyDistActor.actorSystem // user: %17
      auto systemRef = emitActorPropertyReference(
          *this, loc, selfValue.getValue(),
          lookupProperty(selfTyDecl, ctx.Id_actorSystem));
      // %17 = load %16 : $*FakeActorSystem // users: %21, %20, %18
//      auto systemLoaded = emitSimpleLoad(*this, loc, systemRef);
      // auto systemLoaded = emitLoad(loc, systemRef, getTypeLowering(invocationEncoderTy), SGFContext(), IsNotTake);
//      auto systemLoaded = B.createLoad(loc, systemRef, );
      auto systemLoaded = B.createTrivialLoadOr(loc, systemRef, LoadOwnershipQualifier::Copy, true);

      // retain_value %17 : $FakeActorSystem // id: %18
      // B.createRetainValue(loc, systemLoaded, RefCountingInst::Atomicity::Atomic);

      // function_ref FakeActorSystem.makeInvocationEncoder()
      // %19 = function_ref @$s27FakeDistributedActorSystems0aC6SystemV21makeInvocationEncoderAA0aG0VyF : $@convention(method) (@guaranteed FakeActorSystem) -> FakeInvocation // user: %20
      auto makeInvocationEncoderFnSIL =
          builder.getOrCreateFunction(loc, makeInvocationEncoderFnRef, NotForDefinition);
      SILValue makeInvocationEncoderFn =
          B.createFunctionRefFor(loc, makeInvocationEncoderFnSIL);

      //  %20 = apply %19(%17) : $@convention(method) (@guaranteed FakeActorSystem) -> FakeInvocation // user: %22
      ApplyInst *invocationEncoderValue = B.createApply(
          loc, makeInvocationEncoderFn,
          /*subs=*/SubstitutionMap(),
          /*args=*/{systemLoaded});

      // B.emitReleaseValue(loc, systemLoaded);
      if (systemLoaded->getType().isAddress())
        B.createEndLifetime(loc, systemLoaded);

      B.createStore(loc, invocationEncoderValue, invocationEncoder.getValue(),
                    StoreOwnershipQualifier::Init);
    }

    // === -------------------------------------------------------------------
    // populate the invocation:

    // The graph of basic blocks depends
    // test()
    // [...] -> [doneRecording]
    //           \-...ErrorBB
    //
    // test() throws
    // [...] -> [recordErrorType] -> [doneRecordingBB]
    //           \-...ErrorBB         \-...ErrorBB
    //
    // test() -> T
    // [...] -> [recordReturnTypeBB] -> [doneRecordingBB]
    //           \-...ErrorBB            \-...ErrorBB
    //
    // test() throws -> T
    // [...] -> [recordErrorType] -> [recordReturnTypeBB] -> [doneRecordingBB]
    //           \-...ErrorBB         \-...ErrorBB          \-...ErrorBB
    //
    // test(p: P1)
    // [...] -> [recordArgument] -> [doneRecordingBB]
    //           \-...ErrorBB        \-...ErrorBB
    //
    // test(p: P1) throws
    // [...] -> [recordArgument] -> [recordErrorTypeBB] -> [doneRecordingBB]
    //           \-...ErrorBB        \-...ErrorBB           \-...ErrorBB
    //
    // test(p: P1) throws -> P1
    // [...] -> [recordArgument] -> [recordErrorTypeBB] -> [recordReturnTypeBB] -> [doneRecordingBB]
    //           \-...ErrorBB        \-...ErrorBB           \-...ErrorBB            \-...ErrorBB
    //
    // test(p: P1, p: P2, ...)
    // [...] -> [recordArgument] (-> [recordArgumentNBB])* -> [doneRecordingBB]
    //           \-...ErrorBB         \-...ErrorBB             \-...ErrorBB
    //
    // test(p: P1, p: P2, ...) throws
    // [...] -> [recordArgument] (-> [recordArgumentNBB])* -> [recordErrorType] -> [doneRecording]
    //           \-...ErrorBB         \-...ErrorBB             \-...ErrorBB         \-...ErrorBB
    //
    // test(p: P1, p: P2, ...) throws -> T
    // [...] -> [recordArgument] (-> [recordArgumentNBB])* -> [recordErrorType] -> [recordReturnType] -> [doneRecording]
    //           \-...ErrorBB         \-...ErrorBB             \-...ErrorBB         \-...ErrorBB          \-...ErrorBB
    auto firstOfThrowingApplyBBs = true;
    auto anyRecordBlocks = false;

    SILBasicBlock *recordGenericSubstitutionsBB = nullptr;
    SILBasicBlock *recordErrorGenericSubstitutionsBB = nullptr;
    if (shouldRecordGenericSubstitutions) {
      anyRecordBlocks = true;
      assert(false);
      if (!firstOfThrowingApplyBBs) {
        recordGenericSubstitutionsBB = createBasicBlock("recordGenericSubstitutionsBB");
        firstOfThrowingApplyBBs = false;
      }
      recordErrorGenericSubstitutionsBB = createBasicBlock("recordErrorGenericSubstitutionsBB");
    }

    if (shouldRecordArguments) {
      anyRecordBlocks = true;
      firstOfThrowingApplyBBs = false;
    }

    SILBasicBlock *recordErrorTypeBB = nullptr;
    SILBasicBlock *recordErrorTypeErrorBB = nullptr;
    if (shouldRecordErrorType) {
      anyRecordBlocks = true;
      if (!firstOfThrowingApplyBBs) {
        recordErrorTypeBB = createBasicBlock("recordErrorTypeBB");
      }
      firstOfThrowingApplyBBs = false;
      recordErrorTypeErrorBB = createBasicBlock("recordErrorTypeErrorBB");
    }

    SILBasicBlock *recordReturnTypeBB = nullptr;
    SILBasicBlock *recordReturnTypeErrorBB = nullptr;
    if (shouldRecordReturnType) {
      anyRecordBlocks = true;
      if (!firstOfThrowingApplyBBs) {
        recordReturnTypeBB = createBasicBlock("recordReturnTypeBB");
      }
      firstOfThrowingApplyBBs = false;
      recordReturnTypeErrorBB = createBasicBlock("recordReturnTypeErrorBB");
    }

    firstOfThrowingApplyBBs = false;
    SILBasicBlock *recordingDoneBB = nullptr;
    SILBasicBlock *recordingDoneErrorBB = createBasicBlock("recordingDoneErrorBB");
    if (anyRecordBlocks) {
      // If any previous record* calls have been made, we need a BB to jump to
      // on the normal path to the recordingDone call:
      recordingDoneBB = createBasicBlock("recordingDoneBB");
    }

    // === All calls on invocationEncoder need Access:
    SILValue invocationEncoderAccess;
    {
      invocationEncoderAccess = B.createBeginAccess(
          loc, invocationEncoder.getValue(), SILAccessKind::Modify,
          SILAccessEnforcement::Static, false, false);
    }

    // === encoder.recordGenericSubstitution() ---------------------------------
    SILBasicBlock *recordGenericSubstitutionsErrorBB = nullptr;
    if (shouldRecordGenericSubstitutions) {
      // TODO(distributed): record substitutions
      assert(false);
    }

    // === encoder.recordArgument() --------------------------------------------
    if (shouldRecordArguments) {
      if (nextNormalBB) {
        B.emitBlock(nextNormalBB);
      }
      nextNormalBB = nullptr;

      assert(invocationEncoderNominal);
      FuncDecl *recordArgumentFnDecl =
          ctx.getRecordArgumentOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      auto recordArgumentFnRef = SILDeclRef(recordArgumentFnDecl);
      assert(recordArgumentFnRef && "no 'recordArgument' func found!");

      auto recordArgumentFnSIL =
          builder.getOrCreateFunction(loc, recordArgumentFnRef, NotForDefinition);
      SILValue recordArgumentFn =
          B.createFunctionRefFor(loc, recordArgumentFnSIL);

      // --- invoke recordArgument for every parameter
      auto funcDeclParamsNum = fd->getParameters()->size();
      assert(funcDeclParamsNum > 0 &&
             "attempted recording arguments but no actual parameters declared "
             "on distributed method");

      assert(paramsForForwarding.size() == funcDeclParamsNum + 1);
      assert(paramsForForwarding.back()->getType().getNominalOrBoundGenericNominal()->isDistributedActor());
      // the parameters for forwarding include `self`, but here we should not
      // copy that self, so we just drop it.
      paramsForForwarding.pop_back();

      unsigned long paramIdx = 0;
      Optional<SILValue> previousArgumentToDestroy;
      for (SILValue paramValue : paramsForForwarding) {
        auto isLastParam = ++paramIdx == funcDeclParamsNum;

        auto paramTy = paramValue->getType().getASTType();
        if (nextNormalBB) {
          // this will be `nextRecordArgumentNormalBB` from the previous
          // iteration i.e. if we're the first parameter, we just emit directly;
          // if we're the second (or later) parameter, we need to emit a basic
          // block here.
          B.emitBlock(nextNormalBB);
          createVoidPhiArgument(*this, ctx, nextNormalBB);
        }

        if (previousArgumentToDestroy.hasValue()) {
          B.createDestroyAddr(loc, previousArgumentToDestroy.getValue());
        }

        // Prepare the next normalBB we need to jump to on successful recordArgument call;
        // If this is the last parameter we need to record though, then we always
        // go to the following record* function type, which need to be the
        // 'nextNormalBB'.
        SILBasicBlock *nextRecordArgumentNormalBB;
        if (!isLastParam) {
          nextRecordArgumentNormalBB = createBasicBlock("nextRecordArgumentNormalBB");
        } else if (shouldRecordErrorType) {
          nextRecordArgumentNormalBB = recordErrorTypeBB;
        } else if (shouldRecordReturnType) {
          nextRecordArgumentNormalBB = recordReturnTypeBB;
        } else {
          nextRecordArgumentNormalBB = recordingDoneBB;
        }
        nextNormalBB = nextRecordArgumentNormalBB;
        auto recordArgumentErrorBB = createBasicBlock("recordArgumentErrorBB");

        // === Prepare the argument
        SILType argType = paramValue->getType();
        if (paramValue->getType().hasArchetype()) {
          fprintf(stderr, "[%s:%d] (%s) HAS ARCHETYPE\n", __FILE__, __LINE__, __FUNCTION__);
          auto out = paramValue->getType().mapTypeOutOfContext();
          out.dump();
          argType = out;
        }

        llvm::Optional<ManagedValue> argValue;
        {
          fprintf(stderr, "[%s:%d] (%s) paramValue->getType()\n", __FILE__, __LINE__, __FUNCTION__);
          paramValue->getType().dump();
          paramValue->getType().getASTType().dump();
          getLoweredType(paramValue->getType().getASTType()).dump();

          auto argTemp = emitTemporaryAllocation(loc, paramValue->getType());
          argValue = emitManagedBufferWithCleanup(argTemp);

          fprintf(stderr, "[%s:%d] (%s) ----------------------------\n", __FILE__, __LINE__, __FUNCTION__);
          F.dump();
          fprintf(stderr, "[%s:%d] (%s) ----------------------------\n", __FILE__, __LINE__, __FUNCTION__);

          if (paramValue->getType().isAddressOnly(F)) {
            fprintf(stderr, "[%s:%d] (%s) createCopyAddr\n", __FILE__, __LINE__, __FUNCTION__);
            B.createCopyAddr(loc, paramValue, argTemp, IsNotTake, IsInitialization);
          } else {
            if (paramValue->getType().isAddress()) {
              fprintf(stderr, "[%s:%d] (%s) createTrivialLoadOr\n", __FILE__, __LINE__, __FUNCTION__);
              paramValue = B.createTrivialLoadOr(loc, paramValue,
                                                 LoadOwnershipQualifier::Take);
            } else {
              fprintf(stderr, "[%s:%d] (%s) emitCopyValueOperation\n", __FILE__, __LINE__, __FUNCTION__);
              paramValue = B.emitCopyValueOperation(loc, paramValue);
            }

            B.emitStoreValueOperation(loc, paramValue, argTemp,
                                      StoreOwnershipQualifier::Init);
          }

        }

        // === Prepare generic signature
        auto recordArgumentGenericSig = recordArgumentFnDecl->getGenericSignature();
        SmallVector<Type, 1> subTypes;
        SmallVector<ProtocolConformanceRef, 2> subConformances;
        {
          auto module = B.getModule().getSwiftModule();
          subTypes.push_back(paramTy);

          // --- Codable: Decodable
          auto decodableRequirementTy =
              ctx.getProtocol(KnownProtocolKind::Decodable); // FIXME: actually use SerializatioNRequirement
          auto paramDecodableTypeConfRef = module->lookupConformance(
              paramTy, decodableRequirementTy);
          subConformances.push_back(paramDecodableTypeConfRef);

          // --- Codable: Encodable
          auto encodableRequirementTy = ctx.getProtocol(
              KnownProtocolKind::Encodable); // FIXME: actually use SerializatioNRequirement
          auto paramEncodableTypeConfRef = module->lookupConformance(
              paramTy, encodableRequirementTy);
          subConformances.push_back(paramEncodableTypeConfRef);

        }
        auto subs = SubstitutionMap::get(
            recordArgumentGenericSig,
            subTypes, subConformances);

        B.createTryApply(
            loc, recordArgumentFn,
            subs,
            {
                argValue.hasValue() ? argValue->getValue() : paramValue,
                invocationEncoderAccess
            },
            /*normalBB=*/nextNormalBB,
            /*errorBB=*/recordArgumentErrorBB);
        {
          emitThrowWithCleanupBasicBlock(
              *this, loc, thunk, recordArgumentErrorBB, errorBB,
              /*endAccesses=*/{invocationEncoderAccess});
        }
      }
    }

    // === encoder.recordErrorType() -------------------------------------------
    if (shouldRecordErrorType) {
      if (recordErrorTypeBB) {
        B.emitBlock(recordErrorTypeBB);
        createVoidPhiArgument(*this, ctx, recordErrorTypeBB);
      }

      if (recordReturnTypeBB) {
        nextNormalBB = recordReturnTypeBB;
      } else {
        nextNormalBB = recordingDoneBB;
      }

      // Get the error type.
      // If we ever did typed-throws we'd get the error type from fd here...
      auto errorMetatype = getLoweredType(MetatypeType::get(errorTy, MetatypeRepresentation::Thick));
      auto errorMetatypeValue = B.createMetatype(loc, errorMetatype);

      // Get the function
      FuncDecl *recordErrorTypeFnDecl =
          ctx.getRecordErrorTypeOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      assert(recordErrorTypeFnDecl);
      auto recordErrorTyFnRef = SILDeclRef(recordErrorTypeFnDecl);
      auto recordErrorTypeFnSIL =
          builder.getOrCreateFunction(loc, recordErrorTyFnRef, NotForDefinition);
      SILValue recordErrorTyFn = B.createFunctionRefFor(loc, recordErrorTypeFnSIL);

      // Prepare the <E: Error> substitution,
      // but just fill it with Error anyway.
      auto recordErrorTypeGenericSig = recordErrorTypeFnDecl->getGenericSignature();
      SmallVector<Type, 1> subTypes;
      SmallVector<ProtocolConformanceRef, 1> subConformances;
      {
        // <Err: Error>
        pushErrorConformance(*this, ctx, subTypes, subConformances);
      }
      auto errorSubs = SubstitutionMap::get(
          recordErrorTypeGenericSig,
          subTypes, subConformances);

      B.createTryApply(
          loc, recordErrorTyFn,
          /*subs*/errorSubs,
          /*args*/{ errorMetatypeValue, invocationEncoder.getValue() },
          /*normalBB*/nextNormalBB,
          /*errorBB*/recordErrorTypeErrorBB);
    }
    if (shouldRecordErrorType) {
      emitThrowWithCleanupBasicBlock(
          *this, loc, thunk, recordErrorTypeErrorBB, errorBB,
          /*endAccesses=*/{invocationEncoderAccess});
    }

    // === encoder.recordReturnType() ------------------------------------------
    if (shouldRecordReturnType) {
      if (recordReturnTypeBB) {
        B.emitBlock(recordReturnTypeBB);
        createVoidPhiArgument(*this, ctx, recordReturnTypeBB);
      }

      // Get the return meta type.
      // If we ever did typed-throws we'd get the error type from fd here...
      auto returnMetatype = getLoweredType(MetatypeType::get(resultType.getASTType(), MetatypeRepresentation::Thick));
      auto returnMetatypeValue = B.createMetatype(loc, returnMetatype);

      // Get the function
      FuncDecl *recordReturnTypeFnDecl =
          ctx.getRecordReturnTypeOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      assert(recordReturnTypeFnDecl);
      auto recordErrorTyFnRef = SILDeclRef(recordReturnTypeFnDecl);
      auto recordReturnTypeFnSIL =
          builder.getOrCreateFunction(loc, recordErrorTyFnRef, NotForDefinition);
      SILValue recordErrorTyFn = B.createFunctionRefFor(loc, recordReturnTypeFnSIL);

      // Prepare the <Res: SerializationRequirement> substitution,
      // but just fill it with Error anyway.
      auto recordReturnTypeGenericSig = recordReturnTypeFnDecl->getGenericSignature();
      SmallVector<Type, 1> subTypes;
      SmallVector<ProtocolConformanceRef, 2> subConformances;
      {
        auto module = B.getModule().getSwiftModule();

        // <Res: SerializationRequirement>
        subTypes.push_back(resultType.getASTType());

        // pushSerializationRequirementConformance(*this, ctx, resultType, subTypes, subConformances); // FIXME(distributed): use this

        // FIXME: actually use SerializationRequirement
        subConformances.push_back(module->lookupConformance(
            resultType.getASTType(),
            ctx.getProtocol(KnownProtocolKind::Decodable)));

        // FIXME: actually use SerializationRequirement
        subConformances.push_back(module->lookupConformance(
            resultType.getASTType(),
            ctx.getProtocol(KnownProtocolKind::Encodable)));
      }
      auto errorSubs = SubstitutionMap::get(
          recordReturnTypeGenericSig,
          subTypes, subConformances);

      B.createTryApply(
          loc, recordErrorTyFn,
          /*subs*/errorSubs,
          /*args*/{ returnMetatypeValue, invocationEncoder.getValue() },
          /*normalBB*/recordingDoneBB,
          /*errorBB*/recordReturnTypeErrorBB);
    }
    if (shouldRecordReturnType) {
      emitThrowWithCleanupBasicBlock(
          *this, loc, thunk, recordReturnTypeErrorBB, errorBB,
          /*endAccesses=*/{invocationEncoderAccess});
    }

    // === encoder.doneRecording() ---------------------------------------------
    SILBasicBlock *makeRemoteCallTargetBB = createBasicBlock("makeRemoteCallTargetBB");
    {
      if (recordingDoneBB) {
        B.emitBlock(recordingDoneBB);

        createVoidPhiArgument(*this, ctx, recordingDoneBB);
      }

      assert(invocationEncoderNominal);
      FuncDecl *doneRecordingFnDecl =
          ctx.getDoneRecordingOnDistributedInvocationEncoder(
              invocationEncoderNominal);
      assert(doneRecordingFnDecl);
      auto doneRecordingFnRef = SILDeclRef(doneRecordingFnDecl);

      auto doneRecordingFnSIL =
          builder.getOrCreateFunction(loc, doneRecordingFnRef, NotForDefinition);
      SILValue doneRecordingFn = B.createFunctionRefFor(loc, doneRecordingFnSIL);

      B.createTryApply(
          loc, doneRecordingFn,
          /*subs=*/SubstitutionMap(),
          /*args=*/{invocationEncoderAccess},
          /*normalBB=*/makeRemoteCallTargetBB,
          /*errorBB*/recordingDoneErrorBB);
    }
    {
      emitThrowWithCleanupBasicBlock(*this, loc, thunk, recordingDoneErrorBB,
                                     errorBB, /*endAccesses=*/{invocationEncoderAccess});
    }

    // === create the RemoteCallTarget -----------------------------------------
    auto remoteCallTargetDecl = ctx.getRemoteCallTargetDecl();
    auto remoteCallTargetTy = F.mapTypeIntoContext(remoteCallTargetDecl->getDeclaredInterfaceType());
    ManagedValue remoteCallTargetValue;
    LoadInst *remoteCallSystemSelf = nullptr;
    SILBasicBlock *remoteCallReturnBB = createBasicBlock("remoteCallReturnBB");
    SILBasicBlock *remoteCallErrorBB = createBasicBlock("remoteCallErrorBB");
    ManagedValue remoteCallReturnValue;
    {
      B.emitBlock(makeRemoteCallTargetBB);
      createVoidPhiArgument(*this, ctx, makeRemoteCallTargetBB);

      auto mangledName = thunk.mangle(SILDeclRef::ManglingKind::Default);
      auto mangledNameRef = llvm::StringRef(mangledName.c_str(), mangledName.size()); // FIXME(distributed): can just pass the mangledName?

      // --- Get the `RemoteCallTarget` type

      // %28 = alloc_stack $RemoteCallTarget, let, name "target" // users: %58, %57, %50, %77, %76, %37
      auto remoteCallTargetBuf = emitTemporaryAllocation(loc, getLoweredType(remoteCallTargetTy));
      remoteCallTargetValue = emitManagedBufferWithCleanup(remoteCallTargetBuf);

      // %29 = metatype $@thin RemoteCallTarget.Type // user: %37
      auto remoteCallTargetMetatype = getLoweredType(MetatypeType::get(remoteCallTargetTy));
      auto remoteCallTargetMetatypeValue = B.createMetatype(loc, remoteCallTargetMetatype);

      // %30 = string_literal utf8 "MANGLED_NAME" // user: %35
      auto mangledNameLiteral =
          B.createStringLiteral(loc, mangledNameRef,
                                StringLiteralInst::Encoding::UTF8);

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
          builder.getOrCreateFunction(loc, remoteCallTargetInitRef, NotForDefinition);
      SILValue remoteCallTargetInitFn = B.createFunctionRefFor(loc, remoteCallTargetInitFnSIL);

      // %37 = apply %36(%28, %35, %29) : $@convention(method) (@owned String, @thin RemoteCallTarget.Type) -> @out RemoteCallTarget
      B.createApply(
          loc, remoteCallTargetInitFn, {},
          {/*out*/ remoteCallTargetValue.getValue(), mangledNameStringValue,
           remoteCallTargetMetatypeValue});

      // === Prepare `actorSystem.remoteCall()` --------------------------------
      // --- Prepare storage for the return value
      // %38 = alloc_stack $String // users: %54, %56, %50, %75
      auto remoteCallReturnBuf = emitTemporaryAllocation(loc, resultType);
//      remoteCallReturnValue = emitManagedBufferWithCleanup(remoteCallReturnBuf); // FIXME: !!!!!!!!!!!!!!!!!!!!!!!!!!
      remoteCallReturnValue = ManagedValue::forUnmanaged(remoteCallReturnBuf); // FIXME: !!!!!!!!!!!!!!!!!!!!!!!!!!

      auto systemRef = emitActorPropertyReference(
          *this, loc, selfValue.getValue(),
          lookupProperty(selfTyDecl, ctx.Id_actorSystem));
      remoteCallSystemSelf = B.createTrivialLoadOr(loc, systemRef, LoadOwnershipQualifier::Copy, true);

      // --- Prepare 'throwing' type, Error or Never depending on throws of the target
      SILValue thrownErrorMetatypeValue;
      if (fd->hasThrows()) {
        auto errorMetatype = getLoweredType(MetatypeType::get(errorTy, MetatypeRepresentation::Thick));
        thrownErrorMetatypeValue = B.createMetatype(loc, errorMetatype);
      } else {
        auto neverMetatype = getLoweredType(MetatypeType::get(neverTy, MetatypeRepresentation::Thick));
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
          ctx.getRemoteCallOnDistributedActorSystem(selfTyDecl, /*isVoid=*/resultType.isVoid());
      assert(remoteCallFnDecl && "no remoteCall func found!");
      auto remoteCallFnRef = SILDeclRef(remoteCallFnDecl);
      auto remoteCallFnSIL =
          builder.getOrCreateFunction(loc, remoteCallFnRef, NotForDefinition);
      SILValue remoteCallFn = B.createFunctionRefFor(loc, remoteCallFnSIL);

      // --- prepare subs for the 'remoteCall'
      // <MyDistActor, ErrorType, ReturnType>
      auto remoteCallGenericSig = remoteCallFnDecl->getGenericSignature();
      SmallVector<Type, 3> subTypes;
      SmallVector<ProtocolConformanceRef, 3> subConformances;
      // <τ_0_0,
      //  τ_0_1,
      //  τ_0_2 // only if resultTy != Void
      //  where
      //  τ_0_0 : DistributedActor,
      //  τ_0_2 : Decodable, // only if resultTy != Void
      //  τ_0_2 : Encodable, // only if resultTy != Void
      //  τ_0_0.ID == ActorAddress
      // >
      // (
      //  @guaranteed τ_0_0,
      //  @in_guaranteed RemoteCallTarget,
      //  @inout FakeInvocation,
      //  @thick τ_0_1.Type,
      // @thick τ_0_2.Type, // only if resultTy != Void
      //  @guaranteed FakeActorSystem)
      {
        auto module = B.getModule().getSwiftModule();

        // <Self: DistributedActor>
        pushDistributedActorConformance(*this, ctx, selfSILTy, subTypes, subConformances);

        // <Err: Error>
        if (fd->hasThrows()) {
          pushErrorConformance(*this, ctx, subTypes, subConformances);
        } else {
          pushNeverErrorConformance(*this, ctx, subTypes, subConformances);
        }

        if (!resultType.isVoid()) {
          // <Res: SerializationRequirement>
          //        pushSerializationRequirementConformance(*this, ctx,
          //                                                resultType,
          //                                                subTypes, subConformances);
          subTypes.push_back(resultType.getASTType());

          // FIXME(distributed): get the types from SerializationRequirement
          subConformances.push_back(module->lookupConformance(
              resultType.getASTType(),
              ctx.getProtocol(KnownProtocolKind::Decodable)));

          subConformances.push_back(module->lookupConformance(
              resultType.getASTType(),
              ctx.getProtocol(KnownProtocolKind::Encodable)));
        }
      }

      SubstitutionMap remoteCallSubs =
          SubstitutionMap::get(remoteCallGenericSig,
                               subTypes, subConformances);

      SmallVector<SILValue, 7> remoteCallArgs;
      // 'out' arguments:
      if (!resultType.isVoid())
        remoteCallArgs.push_back(remoteCallReturnValue.getValue()); // return value buffer
      // function arguments:
      remoteCallArgs.push_back(selfValue.getValue()); // on actor
      remoteCallArgs.push_back(remoteCallTargetValue.getValue()); // target
      remoteCallArgs.push_back(invocationEncoderAccess); // invocation encoder
      remoteCallArgs.push_back(thrownErrorMetatypeValue); // throwing type
      if (!resultType.isVoid())
        remoteCallArgs.push_back(returnMetatypeValue); // returning type, only if non-void
      // self:
      remoteCallArgs.push_back(remoteCallSystemSelf); // ActorSystem

      // try_apply %49<MyDistActor, Never, String>(%38, %2, %28, %48, %43, %46, %40) : $@convention(method) @async <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : DistributedActor, τ_0_2 : Decodable, τ_0_2 : Encodable, τ_0_0.ID == ActorAddress> (@guaranteed τ_0_0, @in_guaranteed RemoteCallTarget, @inout FakeInvocation, @thick τ_0_1.Type, Optional<@thick τ_0_2.Type>, @guaranteed FakeActorSystem) -> (@out τ_0_2, @error Error), normal bb5, error bb10 // id: %50
      B.createTryApply(loc, remoteCallFn,
                       remoteCallSubs,
                       remoteCallArgs,
                       /*normalBB=*/remoteCallReturnBB,
                       /*errorBB=*/remoteCallErrorBB);
    }

    // === return <result of remote call> --------------------------------------
    {
      B.emitBlock(remoteCallReturnBB);
      createVoidPhiArgument(*this, ctx, remoteCallReturnBB);

      auto result = remoteCallReturnValue.getValue();
      auto resultLoaded = B.createTrivialLoadOr(loc, result, LoadOwnershipQualifier::Copy, true);

      // FIXME(distributed): manual since I could not figure out how to NOT destroy_addr in the error path, where the memory is not initialized, so the destroy would fail SIL verification
      B.createDestroyAddr(loc, result);
//      B.createDeallocStack(loc, result);

      if (remoteCallSystemSelf->getType().isAddress())
        B.createEndLifetime(loc, remoteCallSystemSelf);
      B.createEndAccess(loc, invocationEncoderAccess, /*aborted=*/false);
      Cleanups.emitCleanupsForReturn(CleanupLocation(loc), NotForUnwind);
      B.createBranch(loc, returnBB, {resultLoaded});
    }
    {
      // FIXME(distributed): manual since I could not figure out how to NOT destroy_addr in the error path, where the memory is not initialized, so the destroy would fail SIL verification
//      emitThrowWithCleanupBasicBlock(*this, loc, thunk, remoteCallErrorBB, errorBB,
//                                     /*endAccesses*/{invocationEncoderAccess},
//                                     /*endLifetimes*/{remoteCallSystemSelf});
      B.emitBlock(remoteCallErrorBB);
      SILValue error = remoteCallErrorBB->createPhiArgument(
          fnConv.getSILErrorType(getTypeExpansionContext()),
          OwnershipKind::Owned);

      auto result = remoteCallReturnValue.getValue();

      auto methodTy =
          SGM.Types.getConstantOverrideType(getTypeExpansionContext(), thunk);
      auto derivativeFnSILTy = SILType::getPrimitiveObjectType(methodTy);
      auto silFnType = derivativeFnSILTy.castTo<SILFunctionType>();
      SILFunctionConventions fnConv(silFnType, SGM.M);


//      B.createDeallocStack(loc, result); // FIXME: the reason this is manual is because I could not figure out how to NOT destroy_addr the result on this error exit path

      B.createEndAccess(loc, invocationEncoderAccess, /*aborted=*/false);

      if (remoteCallSystemSelf->getType().isAddress())
        B.createEndLifetime(loc, remoteCallSystemSelf);

      Cleanups.emitCleanupsForReturn(CleanupLocation(loc), IsForUnwind);

      B.createBranch(loc, errorBB, {error});
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

  fprintf(stderr, "[%s:%d] (%s) ===========================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ===========================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  F.dump();
  fprintf(stderr, "[%s:%d] (%s) ===========================================================================\n", __FILE__, __LINE__, __FUNCTION__);
  fprintf(stderr, "[%s:%d] (%s) ===========================================================================\n", __FILE__, __LINE__, __FUNCTION__);
}
