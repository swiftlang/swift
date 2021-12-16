//===-- DistributedActor.cpp - SIL utils for distributed actors -*- C++ -*-===//
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

#include "swift/SILOptimizer/Utils/DistributedActor.h"

#include "swift/AST/Decl.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILLocation.h"

namespace swift {

SILArgument *findFirstDistributedActorSystemArg(SILFunction &F) {
  auto *module = F.getModule().getSwiftModule();
  auto &C = F.getASTContext();

  auto *transportProto = C.getProtocol(KnownProtocolKind::DistributedActorSystem);
  Type transportTy = transportProto->getDeclaredInterfaceType();

  for (auto arg : F.getArguments()) {
    // TODO(distributed): also be able to locate a generic transport
    Type argTy = arg->getType().getASTType();
    auto argDecl = arg->getDecl();

    auto conformsToTransport =
        module->lookupConformance(argDecl->getInterfaceType(), transportProto);

    // Is it a protocol that conforms to DistributedActorSystem?
    if (argTy->isEqual(transportTy) || conformsToTransport) {
      return arg;
    }

    // Is it some specific DistributedActorSystem?
    auto result = module->lookupConformance(argTy, transportProto);
    if (!result.isInvalid()) {
      return arg;
    }
  }

#ifndef NDEBUG
  llvm_unreachable("Missing required DistributedActorSystem argument!");
#endif

  return nullptr;
}

void emitDistributedActorSystemWitnessCall(
    SILBuilder &B, SILLocation loc, DeclName methodName,
    SILValue actorSystem, SILType actorType, ArrayRef<SILValue> args,
    Optional<std::pair<SILBasicBlock *, SILBasicBlock *>> tryTargets) {
  auto &F = B.getFunction();
  auto &M = B.getModule();
  auto &C = F.getASTContext();

  // Dig out the conformance to DistributedActorSystem.
  ProtocolDecl *systemProto = C.getProtocol(KnownProtocolKind::DistributedActorSystem);
  assert(systemProto);
  auto systemASTType = actorSystem->getType().getASTType();
  auto *module = M.getSwiftModule();
  ProtocolConformanceRef systemConfRef;

  // If the actorSystem is an existential open it.
  if (systemASTType->isAnyExistentialType()) {
    OpenedArchetypeType *opened;
    systemASTType =
        systemASTType->openAnyExistentialType(opened)->getCanonicalType();
    actorSystem = B.createOpenExistentialAddr(
        loc, actorSystem, F.getLoweredType(systemASTType),
        OpenedExistentialAccess::Immutable);
  }

  if (systemASTType->isTypeParameter() || systemASTType->is<ArchetypeType>()) {
    systemConfRef = ProtocolConformanceRef(systemProto);
  } else {
    systemConfRef = module->lookupConformance(systemASTType, systemProto);
  }

  assert(!systemConfRef.isInvalid() &&
         "Missing conformance to `DistributedActorSystem`");

  // Dig out the method.
  auto method = cast<FuncDecl>(systemProto->getSingleRequirement(methodName));
  auto methodRef = SILDeclRef(method, SILDeclRef::Kind::Func);
  auto methodSILTy =
      M.Types.getConstantInfo(B.getTypeExpansionContext(), methodRef)
          .getSILType();

  auto witnessMethod = B.createWitnessMethod(
      loc, systemASTType, systemConfRef, methodRef, methodSILTy);

  // prepare conformance substitutions
  SubstitutionMap subs;
  {
    auto genericSig = method->getGenericSignature();
    SmallVector<Type, 2> subTypes;
    SmallVector<ProtocolConformanceRef, 2> subConformances;
    subTypes.push_back(systemASTType);
    subConformances.push_back(systemConfRef);
    if (actorType) {
      ProtocolDecl *actorProto = C.getProtocol(
          KnownProtocolKind::DistributedActor);
      assert(actorProto);

      ProtocolConformanceRef conformance;
      auto distributedActorConfRef = module->lookupConformance(
          actorType.getASTType(), actorProto);
      assert(!distributedActorConfRef.isInvalid() &&
             "Missing conformance to `DistributedActor`");
      subTypes.push_back(actorType.getASTType());
      subConformances.push_back(distributedActorConfRef);
    }

    subs = SubstitutionMap::get(genericSig, subTypes, subConformances);
  }

  Optional<SILValue> temporaryActorIDBuffer;

  // If the self parameter is indirect but the actorSystem is a value, put it
  // into a temporary allocation.
  auto methodSILFnTy = methodSILTy.castTo<SILFunctionType>();
  Optional<SILValue> temporaryActorSystemBuffer;
  if (methodSILFnTy->getSelfParameter().isFormalIndirect() &&
      !actorSystem->getType().isAddress()) {
    auto buf = B.createAllocStack(loc, actorSystem->getType(), None);
    actorSystem = B.emitCopyValueOperation(loc, actorSystem);
    B.emitStoreValueOperation(
        loc, actorSystem, buf, StoreOwnershipQualifier::Init);
    temporaryActorSystemBuffer = SILValue(buf);
  }

  // === Call the method.
  // --- Push the arguments
  SmallVector<SILValue, 2> allArgs;
  auto params = methodSILFnTy->getParameters();
  for (size_t i = 0; i < args.size(); ++i) {
    auto arg = args[i];
    if (params[i].isFormalIndirect() &&
        !arg->getType().isAddress() &&
        !dyn_cast<AnyMetatypeType>(arg->getType().getASTType())) {
      auto buf = B.createAllocStack(loc, arg->getType(), None);
      auto argCopy = B.emitCopyValueOperation(loc, arg);
      B.emitStoreValueOperation(
          loc, argCopy, buf, StoreOwnershipQualifier::Init);
      temporaryActorIDBuffer = SILValue(buf);
      allArgs.push_back(*temporaryActorIDBuffer);
    } else {
      allArgs.push_back(arg);
    }
  }
  // Push the self argument
  auto selfArg = temporaryActorSystemBuffer ? *temporaryActorSystemBuffer : actorSystem;
  allArgs.push_back(selfArg);

  SILInstruction *apply;
  if (tryTargets) {
    apply = B.createTryApply(
        loc, witnessMethod, subs, allArgs, tryTargets->first,
        tryTargets->second);
  } else {
    apply = B.createApply(loc, witnessMethod, subs, allArgs);
  }

  // Local function to emit a cleanup after the call.
  auto emitCleanup = [&](llvm::function_ref<void(SILBuilder &builder)> fn) {
    if (tryTargets) {
      {
        SILBuilderWithScope normalBuilder(tryTargets->first, apply);
        fn(normalBuilder);
      }
      {
        SILBuilderWithScope errorBuilder(tryTargets->second, apply);
        fn(errorBuilder);
      }
    } else {
      fn(B);
    }
  };

  // ==== If we had to create a buffers we need to clean them up
  // --- Cleanup id buffer
  if (temporaryActorIDBuffer) {
    emitCleanup([&](SILBuilder & builder) {
      auto value = builder.emitLoadValueOperation(
          loc, *temporaryActorIDBuffer, LoadOwnershipQualifier::Take);
      builder.emitDestroyValueOperation(loc, value);
      builder.createDeallocStack(loc, *temporaryActorIDBuffer);
    });
  }
  // --- Cleanup actorSystem buffer
  if (temporaryActorSystemBuffer) {
    emitCleanup([&](SILBuilder & builder) {
      auto value = builder.emitLoadValueOperation(
          loc, *temporaryActorSystemBuffer, LoadOwnershipQualifier::Take);
      builder.emitDestroyValueOperation(loc, value);
      builder.createDeallocStack(loc, *temporaryActorSystemBuffer);
    });
  }
}

void emitActorReadyCall(SILBuilder &B, SILLocation loc, SILValue actor,
                        SILValue actorSystem) {

  auto &F = B.getFunction();
  auto &C = F.getASTContext();
  emitDistributedActorSystemWitnessCall(
      B, loc, C.Id_actorReady, actorSystem,
      F.mapTypeIntoContext(actor->getType()), { actor });
}

} // namespace swift
