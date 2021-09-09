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

using namespace swift;
using namespace Lowering;

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

static VarDecl *lookupActorTransportProperty(ASTContext &C, ClassDecl *cd,
                                             SILValue selfValue) {
  auto transportVarDeclRefs = cd->lookupDirect(C.Id_actorTransport);
  assert(transportVarDeclRefs.size() == 1);
  return dyn_cast<VarDecl>(transportVarDeclRefs.front());
}

/******************************************************************************/
/****************** DISTRIBUTED ACTOR STORAGE INITIALIZATION ******************/
/******************************************************************************/

/// Get the `ActorTransport` parameter of the constructor.
/// Sema should have guaranteed that there is exactly one of them for any
/// designated initializer of a distributed actor.
static SILArgument*
getActorTransportArgument(ASTContext& C, SILFunction& F, ConstructorDecl *ctor) {
  auto *DC = cast<DeclContext>(ctor);
  auto module = DC->getParentModule();

  auto *transportProto =
      C.getProtocol(KnownProtocolKind::ActorTransport);
  Type transportTy = transportProto->getDeclaredInterfaceType();

  for (auto arg : F.getArguments()) {
    // TODO(distributed): also be able to locate a generic transport
    Type argTy = arg->getType().getASTType();
    auto argDecl = arg->getDecl();

    auto conformsToTransport = module->lookupConformance(
        argDecl->getInterfaceType(), transportProto);

    // Is it a protocol that conforms to ActorTransport?
    if (argTy->isEqual(transportTy) || conformsToTransport) {
      return arg;
    }

    // Is it some specific ActorTransport?
    auto result = module->lookupConformance(argTy, transportProto);
    if (!result.isInvalid()) {
      return arg;
    }
  }

  // did not find argument of ActorTransport type!
  llvm_unreachable("Missing required ActorTransport argument!");
}

/// Synthesize the actorTransport initialization:
///
/// \verbatim
///     self.actorTransport = <<constructor parameter:ActorTransport>>
/// \endverbatim
static void emitDistributedActorStore_transport(
    ASTContext& C, SILGenFunction &SGF,
    SILValue actorSelf, AbstractFunctionDecl *func,
    SILArgument *transportArg) {
  auto &B = SGF.B;

  auto &SGM = SGF.SGM;
  SILGenFunctionBuilder builder(SGM);

  auto *dc = func->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();

  auto loc = SILLocation(func);
  loc.markAutoGenerated();

  // ==== Prepare the property reference: self.id
  auto vars = classDecl->lookupDirect(C.Id_actorTransport);
  assert(vars.size() == 1);
  auto *var = dyn_cast<VarDecl>(vars.front());

  // ----

  auto fieldAddr = B.createRefElementAddr(
      loc, actorSelf, var,
      SGF.getLoweredType(var->getInterfaceType()));

  // ==== Store the transport
  B.createCopyAddr(loc,
                   /*src*/transportArg,
                   /*dest*/fieldAddr,
                   IsNotTake, IsInitialization);
}

// TODO(distributed): remove this store impl and reuse Store_transport
static void
emitDistributedActor_init_transportStore(
    SILGenFunction &SGF,
    ManagedValue borrowedSelfArg, VarDecl *selfDecl,
    ConstructorDecl *ctor,
    Pattern *pattern, VarDecl *var) {
  auto &C = selfDecl->getASTContext();
  auto &B = SGF.B;
  auto &F = SGF.F;
  auto &SGM = SGF.SGM;
  SILGenFunctionBuilder builder(SGM);

  auto loc = SILLocation(ctor);
  loc.markAutoGenerated();

  // ==== Prepare assignment: get the self.transport address
  SILValue transportArgValue = getActorTransportArgument(C, F, ctor);

  // ----

  auto transportFieldAddr = B.createRefElementAddr(
      loc, borrowedSelfArg.getValue(), var,
      SGF.getLoweredType(var->getInterfaceType()));

  // ==== Store the transport
  B.createCopyAddr(loc,
                   /*src*/transportArgValue,
                   /*dest*/transportFieldAddr,
                   IsNotTake, IsInitialization);
}

/// Synthesize storing the passed in managed identity to the `id` property:
///
/// \verbatim
///     self.id = <<parameter:identity>>
/// \endverbatim
static void emitDistributedActorStore_id(
    ASTContext& C, SILGenFunction &SGF,
    SILValue actorSelf, AbstractFunctionDecl *func,
    SILArgument *identityArg) {
  auto &B = SGF.B;

  auto &SGM = SGF.SGM;
  SILGenFunctionBuilder builder(SGM);

  auto *dc = func->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();

  auto loc = SILLocation(func);
  loc.markAutoGenerated();

  // ==== Prepare the property reference: self.id
  auto vars = classDecl->lookupDirect(C.Id_id);
  assert(vars.size() == 1);
  auto *var = dyn_cast<VarDecl>(vars.front());

  // ==== Prepare assignment
  auto fieldAddr = B.createRefElementAddr(
      loc, actorSelf, var,
      SGF.getLoweredType(var->getInterfaceType()));

  // ==== Store the transport
  B.createCopyAddr(loc,
                   /*src*/identityArg,
                   /*dest*/fieldAddr,
                   IsNotTake, IsInitialization);
}

/// Synthesize the distributed actor's identity (`id`) initialization:
///
/// \verbatim
///     self.id = transport.assignIdentity(Self.self)
/// \endverbatim
static void emitDistributedActorStore_init_assignIdentity(
    SILGenFunction &SGF,
    ManagedValue borrowedSelfArg, VarDecl *selfVarDecl,
    ConstructorDecl *ctor,
    Pattern *pattern, VarDecl *var) {
  auto &C = selfVarDecl->getASTContext();
  auto &B = SGF.B;
  auto &F = SGF.F;
  auto &SGM = SGF.SGM;
  SILGenFunctionBuilder builder(SGM);

  auto loc = SILLocation(ctor);
  loc.markAutoGenerated();

  // ==== prepare the transport.assignIdentity(_:) function
  ProtocolDecl *transportProto = C.getProtocol(KnownProtocolKind::ActorTransport);

  // --- Prepare the arguments
  SILValue transportArgValue = getActorTransportArgument(C, F, ctor);

  ProtocolDecl *distributedActorProto = C.getProtocol(KnownProtocolKind::DistributedActor);

  assert(distributedActorProto);
  assert(transportProto);

  // --- Open the transport existential
  OpenedArchetypeType *Opened;
  auto transportASTType = transportArgValue->getType().getASTType();
  auto openedTransportType =
      transportASTType->openAnyExistentialType(Opened)->getCanonicalType();
  auto openedTransportSILType = F.getLoweredType(openedTransportType);
  auto transportArchetypeValue = B.createOpenExistentialAddr(
      loc, transportArgValue, openedTransportSILType, OpenedExistentialAccess::Immutable);

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
      /*lookupTy*/openedTransportType,
      /*Conformance*/transportConfRef,
      /*member*/assignIdentityRef,
      /*methodTy*/assignIdentitySILTy);

  // --- prepare conformance subs
  auto genericSig = assignIdentityMethod->getGenericSignature();

  SubstitutionMap subs =
      SubstitutionMap::get(genericSig,
                           {openedTransportType, selfTy},
                           {transportConfRef, distributedActorConfRef});

  // --- create a temporary storage for the result of the call
  // it will be deallocated automatically as we exit this scope
  auto resultTy = SGF.getLoweredType(var->getInterfaceType());
  auto temp = SGF.emitTemporaryAllocation(loc, resultTy);

  // ---- actually call transport.assignIdentity(Self.self)
  B.createApply(
      loc, assignWitnessMethod, subs,
      { temp, selfMetatypeValue, transportArchetypeValue});

  // ==== Assign the identity to stored property
  // TODO(distributed): reuse emitDistributedActorStore_id here, pass the SILValue
  // --- Prepare address of self.id
  auto idFieldAddr = B.createRefElementAddr(
      loc, borrowedSelfArg.getValue(), var,
      SGF.getLoweredType(var->getInterfaceType()));

  // --- assign to the property
  B.createCopyAddr(loc, /*src*/temp, /*dest*/idFieldAddr,
                   IsTake, IsInitialization);
}

/******************************************************************************/
/******************* DISTRIBUTED ACTOR LOCAL INIT *****************************/
/******************************************************************************/

void SILGenFunction::initializeDistributedActorImplicitStorageInit(
    ConstructorDecl *ctor, ManagedValue selfArg) {
  VarDecl *selfVarDecl = ctor->getImplicitSelfDecl();
  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();
  auto &C = classDecl->getASTContext();

  // Only designated initializers get the lifecycle handling injected
  if (!ctor->isDesignatedInit())
    return;

  SILLocation prologueLoc = RegularLocation(ctor);
  prologueLoc.markAsPrologue(); // TODO: no idea if this is necessary or makes sense

  auto transportTy = C.getActorTransportType();
  auto identityProtoTy = C.getActorIdentityType();
  auto anyIdentityTy = C.getAnyActorIdentityType();

  // ==== Find the stored properties we will initialize
  VarDecl *transportMember = nullptr;
  VarDecl *idMember = nullptr;

  auto borrowedSelfArg = selfArg.borrow(*this, prologueLoc);

  // TODO(distributed): getStoredProperties might be better here, avoid the `break;`
  for (auto member : classDecl->getMembers()) {
    PatternBindingDecl *pbd = dyn_cast<PatternBindingDecl>(member);
    if (!pbd) continue;
    if (pbd->isStatic()) continue;

    Pattern *pattern = pbd->getPattern(0);
    VarDecl *var = pbd->getSingleVar();
    if (!var) continue;

    if (var->getName() == C.Id_actorTransport &&
        var->getInterfaceType()->isEqual(transportTy)) {
      transportMember = var;
      // TODO(distributed): reuse emitDistributedActorStore_transport
      emitDistributedActor_init_transportStore(
          *this, borrowedSelfArg, selfVarDecl, ctor, pattern, var);
    } else if (var->getName() == C.Id_id &&
               (var->getInterfaceType()->isEqual(identityProtoTy) ||
                var->getInterfaceType()->isEqual(anyIdentityTy))) { // TODO(distributed): stick one way to store, but today we can't yet store the existential
      idMember = var;
      emitDistributedActorStore_init_assignIdentity(
          *this, borrowedSelfArg, selfVarDecl, ctor, pattern, var);
    }
    if (transportMember && idMember) {
      break; // we found all properties we care about, break out of the loop early
    }
  }

  assert(transportMember && "Missing DistributedActor.actorTransport member");
  assert(idMember && "Missing DistributedActor.id member");
}

void SILGenFunction::emitDistributedActorReady(
    ConstructorDecl *ctor, ManagedValue selfArg) {

  auto *dc = ctor->getDeclContext();
  auto classDecl = dc->getSelfClassDecl();
  auto &C = classDecl->getASTContext();

  // Only designated initializers get the lifecycle handling injected
  if (!ctor->isDesignatedInit())
    return;

  SILLocation loc = RegularLocation(ctor);
  loc.markAutoGenerated();

  // === Prepare the arguments
  SILValue transportArgValue = getActorTransportArgument(C, F, ctor);
  SILValue selfArgValue = F.getSelfArgument();

  ProtocolDecl *distributedActorProto = C.getProtocol(KnownProtocolKind::DistributedActor);
  ProtocolDecl *transportProto = C.getProtocol(KnownProtocolKind::ActorTransport);
  assert(distributedActorProto);
  assert(transportProto);

  // --- Open the transport existential
  OpenedArchetypeType *Opened;
  auto transportASTType = transportArgValue->getType().getASTType();
  auto openedTransportType =
      transportASTType->openAnyExistentialType(Opened)->getCanonicalType();
  auto openedTransportSILType = F.getLoweredType(openedTransportType);
  auto transportArchetypeValue = B.createOpenExistentialAddr(
      loc, transportArgValue, openedTransportSILType, OpenedExistentialAccess::Immutable);

  // === Make the transport.actorReady call
  // --- prepare the witness_method
  // Note: it does not matter on what module we perform the lookup,
  // it is currently ignored. So the Stdlib module is good enough.
  auto *module = getModule().getSwiftModule();

  // the conformance here is just an abstract thing so we can simplify
  auto transportConfRef = ProtocolConformanceRef(transportProto);
  assert(!transportConfRef.isInvalid() && "Missing conformance to `ActorTransport`");

  auto *selfTyDecl = ctor->getParent()->getSelfNominalTypeDecl();
  auto selfTy = F.mapTypeIntoContext(selfTyDecl->getDeclaredInterfaceType()); // TODO: thats just self var devl getType

  auto distributedActorConfRef = module->lookupConformance(
      selfTy,
      distributedActorProto);
  assert(!distributedActorConfRef.isInvalid() && "Missing conformance to `DistributedActor`");

  // === Prepare the actorReady function
  auto actorReadyMethod =
      cast<FuncDecl>(transportProto->getSingleRequirement(C.Id_actorReady));
  auto actorReadyRef = SILDeclRef(actorReadyMethod, SILDeclRef::Kind::Func);
  auto actorReadySILTy =
      getConstantInfo(getTypeExpansionContext(), actorReadyRef)
      .getSILType();

  auto readyWitnessMethod = B.createWitnessMethod(
      loc,
      /*lookupTy*/openedTransportType,
      /*Conformance*/transportConfRef,
      /*member*/actorReadyRef,
      /*methodTy*/actorReadySILTy);

  // --- prepare conformance subs
  auto genericSig = actorReadyMethod->getGenericSignature();

  SubstitutionMap subs =
      SubstitutionMap::get(genericSig,
                           {openedTransportType, selfTy},
                           {transportConfRef, distributedActorConfRef});

  // ---- actually call transport.actorReady(self)
  B.createApply(
      loc, readyWitnessMethod, subs,
      { selfArgValue, transportArchetypeValue});
}

/******************************************************************************/
/******************* DISTRIBUTED ACTOR RESOLVE FUNCTION ***********************/
/******************************************************************************/

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
    // --- Store the identity: self.id = identity
    emitDistributedActorStore_id(
        C, *this, /*actorSelf*/remote, fd, identityArg);

    // --- Store the transport: self.actorTransport = transport
    emitDistributedActorStore_transport(
        C, *this, /*actorSelf*/remote, fd, transportArg);

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

/******************************************************************************/
/******************* DISTRIBUTED DEINIT: resignAddress ************************/
/******************************************************************************/

void SILGenFunction::emitDistributedActor_resignAddress(
    DestructorDecl *dd, SILValue selfValue, SILBasicBlock *continueBB) {
  ASTContext &ctx = getASTContext();

  auto cd = cast<ClassDecl>(dd->getDeclContext());
  assert(cd->isDistributedActor() &&
  "only distributed actors have transport lifecycle hooks in deinit");

  RegularLocation Loc(dd);
  if (dd->isImplicit())
    Loc.markAutoGenerated();

  auto selfDecl = dd->getImplicitSelfDecl();
  auto selfManagedValue = ManagedValue::forUnmanaged(selfValue);
  auto selfTy = selfDecl->getType();

  // ==== locate: self.id
  auto idVarDeclRefs = cd->lookupDirect(ctx.Id_id);
  assert(idVarDeclRefs.size() == 1);
  auto *idVarDeclRef = dyn_cast<VarDecl>(idVarDeclRefs.front());
  assert(idVarDeclRef);
  auto idRef =
      B.createRefElementAddr(Loc, selfValue, idVarDeclRef,
                             getLoweredType(idVarDeclRef->getType()));

  // ==== locate: self.actorTransport
  auto transportVarDeclRef = lookupActorTransportProperty(ctx, cd, selfValue);

  auto transportRef =
      B.createRefElementAddr(Loc, selfValue, transportVarDeclRef,
                             getLoweredType(transportVarDeclRef->getType()));

  // locate: self.transport.resignIdentity(...)
  auto *transportDecl = ctx.getActorTransportDecl();
  auto resignFnDecls = transportDecl->lookupDirect(ctx.Id_resignIdentity);
  assert(resignFnDecls.size() == 1);
  auto *resignFnDecl = resignFnDecls.front();
  auto resignFnRef = SILDeclRef(resignFnDecl);

  // we only transport.resignIdentity if we are a local actor,
  // and thus the address was created by transport.assignIdentity.
  auto isRemoteBB = createBasicBlock();
  auto isLocalBB = createBasicBlock();

  // if __isRemoteActor(self) {
  //   ...
  // } else {
  //   ...
  // }
  emitDistributedIfRemoteBranch(*this, Loc,
                                selfManagedValue, selfTy,
                                /*if remote*/isRemoteBB,
                                /*if local*/isLocalBB);

  // if remote
  // === <noop>
  {
    B.emitBlock(isRemoteBB);
    // noop, remote actors don't do anything in their deinit
    B.createBranch(Loc, continueBB);
  }

  // if local
  // === self.transport.resignIdentity(self.address)
  {
    B.emitBlock(isLocalBB);

    auto openedTransport =
        OpenedArchetypeType::get(transportVarDeclRef->getType());
    auto transportAddr = B.createOpenExistentialAddr(
        Loc, /*operand=*/transportRef, getLoweredType(openedTransport),
        OpenedExistentialAccess::Immutable);

    auto resignFnType = SGM.M.Types.getConstantFunctionType(
        TypeExpansionContext::minimal(), resignFnRef);

    auto witness = B.createWitnessMethod(
        Loc, openedTransport, ProtocolConformanceRef(transportDecl),
        resignFnRef, SILType::getPrimitiveObjectType(resignFnType));

    auto subs = SubstitutionMap::getProtocolSubstitutions(
        transportDecl, openedTransport, ProtocolConformanceRef(transportDecl));

    SmallVector<SILValue, 2> params;
    params.push_back(idRef);
    params.push_back(transportAddr); // self for the call, as last param

    B.createApply(Loc, witness, subs, params);
    B.createBranch(Loc, continueBB);
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
      if (!vd->getAttrs().hasAttribute<DistributedActorIndependentAttr>())
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
