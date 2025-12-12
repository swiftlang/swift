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

#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILLocation.h"

namespace swift {

/// \returns the result of the call, if returned directly.
std::optional<SILValue> emitDistributedActorSystemWitnessCall(
    SILBuilder &B, SILLocation loc, DeclName methodName, SILValue base,
    // types to be passed through to SubstitutionMap:
    SILType actorType,
    // call arguments, except the base which will be passed last
    ArrayRef<SILValue> args,
    // pre-allocated, uninitialized indirect result storage, if needed
    std::optional<SILValue> indirectResult,
    std::optional<std::pair<SILBasicBlock *, SILBasicBlock *>> tryTargets) {
  auto &F = B.getFunction();
  auto &M = B.getModule();
  auto &C = F.getASTContext();

  // Dig out the conformance to DistributedActorSystem.
  ProtocolDecl *DAS = C.getDistributedActorSystemDecl();
  assert(DAS);
  auto systemASTType = base->getType().getASTType();
  ProtocolConformanceRef systemConfRef;

  // If the base is an existential open it.
  if (systemASTType->isAnyExistentialType()) {
    systemASTType = ExistentialArchetypeType::getAny(systemASTType)
        ->getCanonicalType();
    base = B.createOpenExistentialAddr(
        loc, base, F.getLoweredType(systemASTType),
        OpenedExistentialAccess::Immutable);
  }

  systemConfRef = lookupConformance(systemASTType, DAS);
  assert(!systemConfRef.isInvalid() &&
         "Missing conformance to `DistributedActorSystem`");

  // Dig out the method.
  auto method = cast<FuncDecl>(DAS->getSingleRequirement(methodName));
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

      auto distributedActorConfRef = lookupConformance(
          actorType.getASTType(), actorProto);
      assert(!distributedActorConfRef.isInvalid() &&
             "Missing conformance to `DistributedActor`");
      subTypes.push_back(actorType.getASTType());
      subConformances.push_back(distributedActorConfRef);
    }

    subs = SubstitutionMap::get(genericSig, subTypes, subConformances);
  }

  auto methodSILFnTy = methodSILTy.castTo<SILFunctionType>();
  SILFunctionConventions conv(methodSILFnTy, M);

  // Since this code lives outside of SILGen, manage our clean-ups manually.
  SmallVector<SILInstruction *, 2> cleanups;

  auto prepareArgument = [&](SILParameterInfo param, SILValue arg) -> SILValue {
    if (conv.isSILIndirect(param)) {
      // Does it need temporary stack storage?
      if (!arg->getType().isAddress() &&
          !dyn_cast<AnyMetatypeType>(arg->getType().getASTType())) {
        auto buf = B.createAllocStack(loc, arg->getType(), std::nullopt);
        cleanups.push_back(buf);

        auto copy = B.emitCopyValueOperation(loc, arg);
        B.emitStoreValueOperation(
            loc, copy, buf, StoreOwnershipQualifier::Init);

        return buf;
      }
      return arg; // no temporary storage needed
    }

    // Otherwise, it's a direct convention. Borrow if needed.
    if (arg->getType().isAddress()) {
      arg = B.emitLoadBorrowOperation(loc, arg);
      cleanups.push_back(arg.getDefiningInstruction());
    }
    return arg;
  };

  SILValue selfArg = prepareArgument(methodSILFnTy->getSelfParameter(), base);

  // === Call the method.
  // --- Push the arguments
  SmallVector<SILValue, 2> allArgs;

  const bool hasIndirectResult = conv.getNumIndirectSILResults() > 0;
  ASSERT(hasIndirectResult == indirectResult.has_value() && "no indirectResult storage given!");
  ASSERT(conv.getNumIndirectSILResults() <= 1);

  const bool hasDirectResult = conv.getNumDirectSILResults() > 0;
  ASSERT(!(hasIndirectResult && hasDirectResult) && "indirect AND direct results aren't supported");
  ASSERT(conv.getNumDirectSILResults() <= 1);

  if (hasIndirectResult) {
    allArgs.push_back(*indirectResult);
  }

  auto params = methodSILFnTy->getParameters();
  for (size_t i = 0; i < args.size(); ++i) {
    allArgs.push_back(prepareArgument(params[i], args[i]));
  }

  // Push the self argument
  allArgs.push_back(selfArg);

  SILInstruction *apply;
  if (tryTargets) {
    apply = B.createTryApply(
        loc, witnessMethod, subs, allArgs, tryTargets->first,
        tryTargets->second);
  } else {
    apply = B.createApply(loc, witnessMethod, subs, allArgs);
  }

  // Local function to emit cleanups after the call in successor blocks.
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

  // Emit clean-ups in reverse order, to preserve stack nesting, etc.
  for (auto inst : reverse(cleanups)) {
    if (auto asi = dyn_cast<AllocStackInst>(inst)) {
      auto buf = asi->getResult(0);
      emitCleanup([&](SILBuilder & builder) {
        // FIXME: could do destroy_addr rather than take + destroy_value
        auto value = builder.emitLoadValueOperation(
            loc, buf, LoadOwnershipQualifier::Take);
        builder.emitDestroyValueOperation(loc, value);
        builder.createDeallocStack(loc, buf);
      });
      continue;
    }

    if (auto lb = dyn_cast<LoadBorrowInst>(inst)) {
      auto borrow = lb->getResult(0);
      emitCleanup([&](SILBuilder & builder) {
        builder.emitEndBorrowOperation(loc, borrow);
      });
      continue;
    }

    if (isa<LoadInst>(inst)) {
      // no clean-ups required
      continue;
    }

    llvm_unreachable("unknown instruction kind to clean-up!");
  }

  // If this was a try_apply, then the result is the BB argument of the
  // successor block. We let our caller figure that out themselves.
  //
  // Otherwise, the apply had a single direct result, so we return that.
  if (hasDirectResult && !tryTargets) {
    return apply->getResult(0);
  }

  return std::nullopt;
}

void emitActorReadyCall(SILBuilder &B, SILLocation loc, SILValue actor,
                        SILValue actorSystem) {

  auto &F = B.getFunction();
  auto &C = F.getASTContext();
  emitDistributedActorSystemWitnessCall(
      B, loc, C.Id_actorReady, actorSystem,
      actor->getType(), { actor });
}

void emitResignIdentityCall(SILBuilder &B, SILLocation loc,
                            ClassDecl* actorDecl,
                            SILValue actor, SILValue idRef) {
  auto &F = B.getFunction();
  auto &C = F.getASTContext();

  SILValue systemRef = refDistributedActorSystem(B, loc, actorDecl, actor);

  emitDistributedActorSystemWitnessCall(
      B, loc, C.Id_resignID,
      systemRef,
      SILType(),
      { idRef });
}

/// Creates a reference to the distributed actor's \p actorSystem
/// stored property.
SILValue refDistributedActorSystem(SILBuilder &b,
                                   SILLocation loc,
                                   ClassDecl *actDecl,
                                   SILValue actorInstance) {
  assert(actDecl);
  assert(actDecl->isDistributedActor());

  // get the VarDecl corresponding to the actorSystem.
  auto refs = actDecl->lookupDirect(actDecl->getASTContext().Id_actorSystem);
  assert(refs.size() == 1);
  VarDecl *actorSystemVar = dyn_cast<VarDecl>(refs.front());

  return b.createRefElementAddr(loc, actorInstance, actorSystemVar);
}

} // namespace swift
