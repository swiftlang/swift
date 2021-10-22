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

/// Perform an initializing store to the given property using the value
/// \param actorSelf the value representing `self` for the actor instance.
/// \param prop the property to be initialized.
/// \param value the value to use when initializing the property.
static void initializeProperty(SILGenFunction &SGF, SILLocation loc,
                               SILValue actorSelf,
                               VarDecl* prop, SILValue value) {
  auto fieldAddr = SGF.B.createRefElementAddr(loc, actorSelf, prop,
                                  SGF.getLoweredType(prop->getInterfaceType()));
  SGF.B.createCopyAddr(loc,
                   /*src*/value,
                   /*dest*/fieldAddr,
                   IsNotTake, IsInitialization);
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
      
  // If the argument is not existential, it will be a concrete type
  // that can be erased to that existential.
  SILValue transportValue = transportArg;
  if (!transportArg->getType().isExistentialType()) {
    auto &existentialTL = SGF.getTypeLowering(var->getInterfaceType());
    auto concreteFormalType = transportArg->getType().getASTType();

    auto archetype = OpenedArchetypeType::getAny(var->getInterfaceType());
    AbstractionPattern abstractionPattern(archetype);
    auto &concreteTL = SGF.getTypeLowering(abstractionPattern,
                                           concreteFormalType);

    auto module = dc->getParentModule();
    auto actorTransportProto = C.getProtocol(KnownProtocolKind::ActorTransport);
    ProtocolConformanceRef conformances[] = {
        module->lookupConformance(concreteFormalType, actorTransportProto) };
    ManagedValue mv =
      SGF.emitExistentialErasure(loc, concreteFormalType,
                                 concreteTL, existentialTL,
                                 C.AllocateCopy(conformances),
                                 SGFContext(),
                 [&](SGFContext C) -> ManagedValue {
                   return ManagedValue::forBorrowedRValue(transportArg);
                 });
    transportValue = mv.getValue();
  }
  
  initializeProperty(SGF, loc, actorSelf.getValue(), var, transportValue);
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

  ProtocolDecl *transportProto = C.getProtocol(KnownProtocolKind::ActorTransport);
  ProtocolDecl *distributedActorProto = C.getProtocol(KnownProtocolKind::DistributedActor);

  assert(distributedActorProto);
  assert(transportProto);

  SILValue transportValue = findFirstActorTransportArg(F);

  // --- Open the transport existential, if needed.
  auto transportASTType = transportValue->getType().getASTType();
  if (transportASTType->isAnyExistentialType()) {
    OpenedArchetypeType *Opened;
    transportASTType =
        transportASTType->openAnyExistentialType(Opened)->getCanonicalType();
    transportValue = B.createOpenExistentialAddr(
        loc, transportValue, F.getLoweredType(transportASTType),
        OpenedExistentialAccess::Immutable);
  }

  // --- prepare `Self.self` metatype
  auto *selfTyDecl = ctor->getParent()->getSelfNominalTypeDecl();
  // This would be bad: since not ok for generic
  // auto selfMetatype = SGF.getLoweredType(selfTyDecl->getInterfaceType());
  auto selfMetatype =
       SGF.getLoweredType(F.mapTypeIntoContext(selfTyDecl->getInterfaceType()));
  // selfVarDecl.getType() // TODO: do this; then dont need the self type decl
  SILValue selfMetatypeValue = B.createMetatype(loc, selfMetatype);

  // === Make the transport.assignIdentity call
  // --- prepare the witness_method
  // Note: it does not matter on what module we perform the lookup,
  // it is currently ignored. So the Stdlib module is good enough.
  auto *module = SGF.getModule().getSwiftModule();

  // the conformance here is just an abstract thing so we can simplify
  auto transportConfRef = ProtocolConformanceRef(transportProto);
  assert(!transportConfRef.isInvalid() && "Missing conformance to `ActorTransport`");

  auto selfTy = F.mapTypeIntoContext(selfTyDecl->getDeclaredInterfaceType()); // TODO: thats just self var devl getType

  auto distributedActorConfRef = module->lookupConformance(
      selfTy,
      distributedActorProto);
  assert(!distributedActorConfRef.isInvalid() && "Missing conformance to `DistributedActor`");

  auto assignIdentityMethod =
      cast<FuncDecl>(transportProto->getSingleRequirement(C.Id_assignIdentity));
  auto assignIdentityRef = SILDeclRef(assignIdentityMethod, SILDeclRef::Kind::Func);
  auto assignIdentitySILTy =
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), assignIdentityRef)
          .getSILType();

  auto assignWitnessMethod = B.createWitnessMethod(
      loc,
      /*lookupTy*/transportASTType,
      /*Conformance*/transportConfRef,
      /*member*/assignIdentityRef,
      /*methodTy*/assignIdentitySILTy);

  // --- prepare conformance subs
  auto genericSig = assignIdentityMethod->getGenericSignature();

  SubstitutionMap subs =
      SubstitutionMap::get(genericSig,
                           {transportASTType, selfTy},
                           {transportConfRef, distributedActorConfRef});

  VarDecl *var = lookupProperty(classDecl, C.Id_id);

  // --- create a temporary storage for the result of the call
  // it will be deallocated automatically as we exit this scope
  auto resultTy = SGF.getLoweredType(var->getInterfaceType());
  auto temp = SGF.emitTemporaryAllocation(loc, resultTy);

  // ---- actually call transport.assignIdentity(Self.self)
  B.createApply(
      loc, assignWitnessMethod, subs,
      { temp, selfMetatypeValue, transportValue});

  initializeProperty(SGF, loc, borrowedSelfArg.getValue(), var, temp);
}

void SILGenFunction::emitDistActorImplicitPropertyInits(
    ConstructorDecl *ctor, ManagedValue selfArg) {
  // Only designated initializers should perform this initialization.
  assert(ctor->isDesignatedInit());

  auto loc = SILLocation(ctor);
  loc.markAutoGenerated();

  selfArg = selfArg.borrow(*this, loc);
  emitTransportInit(*this, ctor, loc, selfArg);
  emitIdentityInit(*this, ctor, loc, selfArg);
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
  auto &SGM = SGF.SGM;
  auto &F = SGF.F;
  SILGenFunctionBuilder builder(SGM);

  auto loc = SILLocation(fd);
  loc.markAutoGenerated();

  ProtocolDecl *distributedActorProto =
      C.getProtocol(KnownProtocolKind::DistributedActor);
  ProtocolDecl *transportProto =
      C.getProtocol(KnownProtocolKind::ActorTransport);
  assert(distributedActorProto);
  assert(transportProto);

  // // --- Open the transport existential
  OpenedArchetypeType *Opened;
  auto transportASTType = transportValue->getType().getASTType();
  auto openedTransportType =
      transportASTType->openAnyExistentialType(Opened)->getCanonicalType();
  auto openedTransportSILType = F.getLoweredType(openedTransportType);
  auto transportArchetypeValue =
      B.createOpenExistentialAddr(loc, transportValue, openedTransportSILType,
                                  OpenedExistentialAccess::Immutable);

  // --- prepare the witness_method
  // Note: it does not matter on what module we perform the lookup,
  // it is currently ignored. So the Stdlib module is good enough.
  auto *module = SGF.getModule().getSwiftModule();

  // the conformance here is just an abstract thing so we can simplify
  auto transportConfRef = ProtocolConformanceRef(transportProto);
  assert(!transportConfRef.isInvalid() &&
         "Missing conformance to `ActorTransport`");

  auto distributedActorConfRef =
      module->lookupConformance(selfTy, distributedActorProto);
  assert(!distributedActorConfRef.isInvalid() &&
         "Missing conformance to `DistributedActor`");

  auto resolveMethod =
      cast<FuncDecl>(transportProto->getSingleRequirement(C.Id_resolve));
  auto resolveRef = SILDeclRef(resolveMethod, SILDeclRef::Kind::Func);
  auto constantInfo =
      SGF.getConstantInfo(SGF.getTypeExpansionContext(), resolveRef);
  auto resolveSILTy = constantInfo.getSILType();

  auto resolveWitnessMethod =
      B.createWitnessMethod(loc,
                            /*lookupTy*/ openedTransportType,
                            /*Conformance*/ transportConfRef,
                            /*member*/ resolveRef,
                            /*methodTy*/ resolveSILTy);

  // // --- prepare conformance subs
  auto genericSig = resolveMethod->getGenericSignature();

  SubstitutionMap subs =
      SubstitutionMap::get(genericSig, {openedTransportType, selfTy},
                           {transportConfRef, distributedActorConfRef});

  // // ---- actually call transport.resolve(id, as: Self.self)

  SmallVector<SILValue, 3> params;
  params.push_back(identityValue);
  params.push_back(selfMetatypeValue);
  params.push_back(transportArchetypeValue); // self for the call, as last param

  B.createTryApply(loc, resolveWitnessMethod, subs, params, normalBB, errorBB);
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
  assert(identityArg->getType().getASTType()->isEqual(C.getAnyActorIdentityType()));

  // --- Parameter: transport
  SILArgument *transportArg = F.getArgument(1);
  assert(
      transportArg->getType().getASTType()->isEqual(C.getActorTransportType()));

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
  auto *idVarDeclRef = lookupProperty(actorDecl, ctx.Id_id);
  auto idRef =
  B.createRefElementAddr(loc, actorSelf, idVarDeclRef,
                         getLoweredType(idVarDeclRef->getType()));
  
  // ==== locate: self.actorTransport
  auto transportVarDeclRef = lookupProperty(actorDecl, ctx.Id_actorTransport);
  auto transportRef =
  B.createRefElementAddr(loc, actorSelf, transportVarDeclRef,
                         getLoweredType(transportVarDeclRef->getType()));
  
  // ==== locate: self.transport.resignIdentity(...)
  auto *transportDecl = ctx.getActorTransportDecl();
  auto resignFnDecls = transportDecl->lookupDirect(ctx.Id_resignIdentity);
  assert(resignFnDecls.size() == 1);
  auto *resignFnDecl = resignFnDecls.front();
  auto resignFnRef = SILDeclRef(resignFnDecl);
  
  // ==== perform the call
  auto openedTransport =
  OpenedArchetypeType::get(transportVarDeclRef->getType());
  auto transportAddr =
    B.createOpenExistentialAddr(loc, /*operand=*/transportRef.getValue(),
                                getLoweredType(openedTransport),
                                OpenedExistentialAccess::Immutable);
  
  auto resignFnType =
    SGM.M.Types.getConstantFunctionType(TypeExpansionContext::minimal(),
                                        resignFnRef);
  
  auto conformance = ProtocolConformanceRef(transportDecl);
  auto witness =
    B.createWitnessMethod(loc, openedTransport,
                          conformance, resignFnRef,
                          SILType::getPrimitiveObjectType(resignFnType));
  
  auto subs = SubstitutionMap::getProtocolSubstitutions(transportDecl,
                                                        openedTransport,
                                                        conformance);
  
  SmallVector<SILValue, 2> params;
  params.push_back(idRef.getValue());
  params.push_back(transportAddr); // self for the call, as last param
  
  B.createApply(loc, witness, subs, params);
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
