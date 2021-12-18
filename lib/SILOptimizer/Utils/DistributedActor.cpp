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

SILArgument *findFirstActorTransportArg(SILFunction &F) {
  auto *module = F.getModule().getSwiftModule();
  auto &C = F.getASTContext();

  auto *transportProto = C.getProtocol(KnownProtocolKind::ActorTransport);
  Type transportTy = transportProto->getDeclaredInterfaceType();

  for (auto arg : F.getArguments()) {
    // TODO(distributed): also be able to locate a generic transport
    Type argTy = arg->getType().getASTType();
    auto argDecl = arg->getDecl();

    auto conformsToTransport =
        module->lookupConformance(argDecl->getInterfaceType(), transportProto);

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

#ifndef NDEBUG
  llvm_unreachable("Missing required ActorTransport argument!");
#endif

  return nullptr;
}

void emitActorTransportWitnessCall(
    SILBuilder &B, SILLocation loc, DeclName methodName,
    SILValue transport, SILType actorType, ArrayRef<SILValue> args,
    Optional<std::pair<SILBasicBlock *, SILBasicBlock *>> tryTargets) {
  auto &F = B.getFunction();
  auto &M = B.getModule();
  auto &C = F.getASTContext();

  // Dig out the conformance to ActorTransport.
  ProtocolDecl *transProto = C.getProtocol(KnownProtocolKind::ActorTransport);
  assert(transProto);
  auto transportASTType = transport->getType().getASTType();
  auto *module = M.getSwiftModule();
  ProtocolConformanceRef transportConfRef;

  // If the transport is an existential open it.
  if (transportASTType->isAnyExistentialType()) {
    OpenedArchetypeType *opened;
    transportASTType =
        transportASTType->openAnyExistentialType(opened)->getCanonicalType();
    transport = B.createOpenExistentialAddr(
        loc, transport, F.getLoweredType(transportASTType),
        OpenedExistentialAccess::Immutable);
  }

  if (transportASTType->isTypeParameter() ||
      transportASTType->is<ArchetypeType>()) {
    transportConfRef = ProtocolConformanceRef(transProto);
  } else {
    transportConfRef = module->lookupConformance(transportASTType, transProto);
  }

  assert(!transportConfRef.isInvalid() &&
         "Missing conformance to `ActorTransport`");

  // Dig out the method.
  auto method = cast<FuncDecl>(transProto->getSingleRequirement(methodName));
  auto methodRef = SILDeclRef(method, SILDeclRef::Kind::Func);
  auto methodSILTy =
      M.Types.getConstantInfo(B.getTypeExpansionContext(), methodRef)
          .getSILType();

  auto witnessMethod = B.createWitnessMethod(
      loc, transportASTType, transportConfRef, methodRef, methodSILTy);

  // prepare conformance substitutions
  SubstitutionMap subs;
  {
    auto genericSig = method->getGenericSignature();
    SmallVector<Type, 2> subTypes;
    SmallVector<ProtocolConformanceRef, 2> subConformances;
    subTypes.push_back(transportASTType);
    subConformances.push_back(transportConfRef);
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

  // If the self parameter is indirect but the transport is a value, put it
  // into a temporary allocation.
  auto methodSILFnTy = methodSILTy.castTo<SILFunctionType>();
  Optional<SILValue> temporaryTransportBuffer;
  if (methodSILFnTy->getSelfParameter().isFormalIndirect() &&
      !transport->getType().isAddress()) {
    auto buf = B.createAllocStack(loc, transport->getType(), None);
    transport = B.emitCopyValueOperation(loc, transport);
    B.emitStoreValueOperation(
        loc, transport, buf, StoreOwnershipQualifier::Init);
    temporaryTransportBuffer = SILValue(buf);
  }

  // Call the method.
  SmallVector<SILValue, 2> allArgs(args.begin(), args.end());
  allArgs.push_back(
      temporaryTransportBuffer ? *temporaryTransportBuffer : transport);

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

  // If we had to create a buffer to pass the transport
  if (temporaryTransportBuffer) {
    emitCleanup([&](SILBuilder & builder) {
      auto value = builder.emitLoadValueOperation(
          loc, *temporaryTransportBuffer, LoadOwnershipQualifier::Take);
      builder.emitDestroyValueOperation(loc, value);
      builder.createDeallocStack(loc, *temporaryTransportBuffer);
    });
  }
}

void emitActorReadyCall(SILBuilder &B, SILLocation loc, SILValue actor,
                        SILValue transport) {

  auto &F = B.getFunction();
  auto &C = F.getASTContext();
  emitActorTransportWitnessCall(
      B, loc, C.Id_actorReady, transport,
      F.mapTypeIntoContext(actor->getType()), { actor });
}

} // namespace swift
