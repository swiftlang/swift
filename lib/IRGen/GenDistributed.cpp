//===--- GenDistributed.cpp - IRGen for distributed features --------------===//
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
//
//  This file implements IR generation for distributed features.
//
//===----------------------------------------------------------------------===//

#include "GenDistributed.h"

#include "BitPatternBuilder.h"
#include "CallEmission.h"
#include "Callee.h"
#include "ClassTypeInfo.h"
#include "ExtraInhabitants.h"
#include "GenCall.h"
#include "GenDecl.h"
#include "GenMeta.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ExtInfo.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/IRGen/Linking.h"
#include "swift/SIL/SILFunction.h"

using namespace swift;
using namespace irgen;

llvm::Value *irgen::emitDistributedActorInitializeRemote(
    IRGenFunction &IGF, SILType selfType, llvm::Value *actorMetatype, Explosion &out) {
  auto &classTI = IGF.getTypeInfo(selfType).as<ClassTypeInfo>();
  auto &classLayout = classTI.getClassLayout(IGF.IGM, selfType,
                                             /*forBackwardDeployment=*/false);
  llvm::Type *destType = classLayout.getType()->getPointerTo();

  auto fn = IGF.IGM.getDistributedActorInitializeRemoteFn();
  actorMetatype =
      IGF.Builder.CreateBitCast(actorMetatype, IGF.IGM.TypeMetadataPtrTy);

  auto call = IGF.Builder.CreateCall(fn, {actorMetatype});
  call->setCallingConv(IGF.IGM.SwiftCC);
  call->setDoesNotThrow();

  auto result = IGF.Builder.CreateBitCast(call, destType);

  out.add(result);

  return result;
}

namespace {

class DistributedAccessor {
  IRGenModule &IGM;
  IRGenFunction &IGF;

  /// Underlying distributed method for this accessor.
  SILFunction *Method;

  /// The interface type of this accessor function.
  CanSILFunctionType AccessorType;
  /// The asynchronous context associated with this accessor.
  AsyncContextLayout AsyncLayout;

public:
  DistributedAccessor(IRGenFunction &IGF, SILFunction *method,
                      CanSILFunctionType accessorTy);

  void emit();

private:
  void computeArguments(llvm::Value *argumentBuffer, Explosion &arguments);

  FunctionPointer getPointerToMethod() const;

  Callee getCalleeForDistributedMethod(llvm::Value *self) const;
};

} // end namespace

/// Compute a type of a distributed method accessor function based
/// on the provided distributed method.
static CanSILFunctionType getAccessorType(IRGenModule &IGM,
                                          SILFunction *DistMethod) {
  auto &Context = IGM.Context;

  auto getParamForArguments = [&]() {
    auto ptrType = Context.getUnsafeRawPointerType();
    return SILParameterInfo(ptrType->getCanonicalType(),
                            ParameterConvention::Direct_Guaranteed);
  };

  // `self` of the distributed actor is going to be passed as an argument
  // to this accessor function.
  auto extInfo = SILExtInfoBuilder()
                     .withRepresentation(SILFunctionTypeRepresentation::Thick)
                     .withAsync()
                     .build();

  auto methodTy = DistMethod->getLoweredFunctionType();

  assert(methodTy->isAsync());
  assert(methodTy->hasErrorResult());

  // Accessor gets argument value buffer and a reference to `self` of
  // the actor and produces a call to the distributed thunk forwarding
  // its result(s) out.
  return SILFunctionType::get(
      /*genericSignature=*/nullptr, extInfo, SILCoroutineKind::None,
      ParameterConvention::Direct_Guaranteed, {getParamForArguments()},
      /*Yields=*/{},
      /*Results=*/methodTy->getResults(),
      /*ErrorResult=*/methodTy->getErrorResult(),
      /*patternSubs=*/SubstitutionMap(),
      /*invocationSubs=*/SubstitutionMap(), Context);
}

llvm::Function *
IRGenModule::getAddrOfDistributedMethodAccessor(SILFunction *F,
                                                ForDefinition_t forDefinition) {
  auto entity = LinkEntity::forDistributedMethodAccessor(F);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition)
      updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  Signature signature = getSignature(getAccessorType(*this, F));
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);

  return createFunction(*this, link, signature);
}

void IRGenModule::emitDistributedMethodAccessor(SILFunction *method) {
  assert(method->isDistributed());

  auto *f = getAddrOfDistributedMethodAccessor(method, ForDefinition);
  if (!f->isDeclaration())
    return;

  IRGenFunction IGF(*this, f);
  DistributedAccessor(IGF, method, getAccessorType(*this, method)).emit();
}

DistributedAccessor::DistributedAccessor(IRGenFunction &IGF,
                                         SILFunction *method,
                                         CanSILFunctionType accessorTy)
    : IGM(IGF.IGM), IGF(IGF), Method(method), AccessorType(accessorTy),
      AsyncLayout(getAsyncContextLayout(
          IGM, AccessorType, AccessorType, SubstitutionMap(),
          /*suppress generics*/ true,
          FunctionPointer::Kind(
              FunctionPointer::BasicKind::AsyncFunctionPointer))) {}

void DistributedAccessor::computeArguments(llvm::Value *argumentBuffer,
                                           Explosion &arguments) {
  auto fnType = Method->getLoweredFunctionType();

  auto offset =
      IGF.createAlloca(IGM.Int8PtrTy, IGM.getPointerAlignment(), "offset");
  IGF.Builder.CreateLifetimeStart(offset, IGM.getPointerSize());

  // Initialize "offset" with the address of the base of the argument buffer.
  IGF.Builder.CreateStore(argumentBuffer, offset);

  // Cover all of the arguments except to `self` of the actor.
  for (auto &param : fnType->getParameters().drop_back()) {
    auto paramTy = param.getSILStorageInterfaceType();
    const TypeInfo &typeInfo = IGF.getTypeInfo(paramTy);

    // 1. Load current offset.
    llvm::Value *currentOffset = IGF.Builder.CreateLoad(offset, "elt_offset");

    // 2. Cast the pointer to the type of the element.
    Address eltPtr = IGF.Builder.CreateBitCast(
        Address(currentOffset, IGM.getPointerAlignment()),
        IGM.getStoragePointerType(paramTy));

    // 3. Adjust typed pointer to the alignement of the type.
    auto alignedOffset = typeInfo.roundUpToTypeAlignment(IGF, eltPtr, paramTy);

    // 4. Create an exploded version of the type to pass as an
    //    argument to distributed method.

    if (paramTy.isObject()) {
      auto &nativeSchema = typeInfo.nativeParameterValueSchema(IGM);
      auto expandedTy = nativeSchema.getExpandedType(IGM);

      if (nativeSchema.requiresIndirect()) {
        llvm_unreachable("indirect parameters are not supported");
      }

      // If schema is empty, skip to the next argument.
      if (nativeSchema.empty())
        continue;

      // 5. Load argument value from the element pointer.
      auto *argValue = IGF.Builder.CreateLoad(alignedOffset, "argval");

      Explosion nonNativeParam;
      nonNativeParam.add(argValue);

      // Convert SIL type into a native representation.
      auto nativeParam = nativeSchema.mapIntoNative(
          IGM, IGF, nonNativeParam, paramTy, /*isOutlined=*/false);

      // If expanded type is a struct we need to explode it to match
      // expected argument schema.
      if (auto *ST = dyn_cast<llvm::StructType>(expandedTy)) {
        IGF.emitAllExtractValues(nativeParam.claimNext(), ST, arguments);
      } else {
        arguments.add(nativeParam.claimNext());
      }
    } else {
      // 5. Load argument value from the element pointer.
      arguments.add(IGF.Builder.CreateLoad(alignedOffset, "argval"));
    }

    // 6. Move the offset to the beginning of the next element.
    {
      llvm::Value *typeSize = typeInfo.getSize(IGF, paramTy);

      llvm::Value *addr = alignedOffset.getAddress();
      addr = IGF.Builder.CreatePtrToInt(addr, IGM.IntPtrTy);
      llvm::Value *nextOffset = IGF.Builder.CreateIntToPtr(
          IGF.Builder.CreateAdd(addr, typeSize), IGM.Int8PtrTy);

      IGF.Builder.CreateStore(nextOffset, offset);
    }
  }

  IGF.Builder.CreateLifetimeEnd(offset, IGM.getPointerSize());
}

void DistributedAccessor::emit() {
  auto methodTy = Method->getLoweredFunctionType();
  SILFunctionConventions conv(methodTy, IGF.getSILModule());
  TypeExpansionContext expansionContext = IGM.getMaximalTypeExpansionContext();

  auto params = IGF.collectParameters();

  auto directResultTy = conv.getSILResultType(expansionContext);
  const auto &directResultTI = IGM.getTypeInfo(directResultTy);
  auto &resultSchema = directResultTI.nativeReturnValueSchema(IGM);
  llvm::Value *indirectResultSlot = nullptr;

  if (resultSchema.requiresIndirect())
    indirectResultSlot = params.claimNext();

  Explosion arguments;
  // Claim indirect results first, they are going to be passed
  // through to the distributed method.
  params.transferInto(arguments, conv.getNumIndirectSILResults());

  unsigned numAsyncContextParams =
      (unsigned)AsyncFunctionArgumentIndex::Context + 1;
  (void)params.claim(numAsyncContextParams);

  // UnsafeRawPointer that holds all of the argument values.
  auto *argBuffer = params.claimNext();
  // Reference to a `self` of the actor to be called.
  auto *actorSelf = params.claimNext();

  GenericContextScope scope(IGM, methodTy->getInvocationGenericSignature());

  // Preliminary: Setup async context for this accessor.
  {
    auto asyncContextIdx =
        Signature::forAsyncEntry(IGM, AccessorType,
                                 /*useSpecialConvention*/ false)
            .getAsyncContextIndex();

    auto entity = LinkEntity::forDistributedMethodAccessor(Method);
    emitAsyncFunctionEntry(IGF, AsyncLayout, entity, asyncContextIdx);
    emitAsyncFunctionPointer(IGM, IGF.CurFn, entity, AsyncLayout.getSize());
  }

  // Step one is to load all of the data from argument buffer,
  // so it could be forwarded to the distributed method.
  computeArguments(argBuffer, arguments);

  // Step two, let's form and emit a call to the distributed method
  // using computed argument explosion.
  {
    Explosion result;
    Explosion error;

    auto callee = getCalleeForDistributedMethod(actorSelf);
    auto emission =
        getCallEmission(IGF, callee.getSwiftContext(), std::move(callee));

    emission->begin();
    emission->setArgs(arguments, /*isOutlined=*/false,
                      /*witnessMetadata=*/nullptr);

    if (resultSchema.requiresIndirect()) {
      Address resultAddr(indirectResultSlot,
                         directResultTI.getBestKnownAlignment());
      emission->emitToMemory(resultAddr, cast<LoadableTypeInfo>(directResultTI),
                             /*isOutlined=*/false);
    } else {
      emission->emitToExplosion(result, /*isOutlined=*/false);
    }

    // Both accessor and distributed method are always `async throws`
    // so we need to load error value (if any) from the slot.
    {
      assert(methodTy->hasErrorResult());

      SILType errorType = conv.getSILErrorType(expansionContext);
      Address calleeErrorSlot =
          emission->getCalleeErrorSlot(errorType, /*isCalleeAsync=*/true);
      error.add(IGF.Builder.CreateLoad(calleeErrorSlot));
    }

    emission->end();

    emitAsyncReturn(IGF, AsyncLayout, directResultTy, AccessorType, result,
                    error);
  }
}

FunctionPointer DistributedAccessor::getPointerToMethod() const {
  auto fnType = Method->getLoweredFunctionType();
  auto fpKind = classifyFunctionPointerKind(Method);
  auto signature = IGM.getSignature(fnType, fpKind.useSpecialConvention());

  auto *fnPtr =
      IGM.getAddrOfSILFunction(Method, NotForDefinition,
                               /*isDynamicallyReplaceable=*/false,
                               /*shouldCallPreviousImplementation=*/false);

  return FunctionPointer::forDirect(fpKind, fnPtr, /*secondary=*/nullptr,
                                    signature);
}

Callee
DistributedAccessor::getCalleeForDistributedMethod(llvm::Value *self) const {
  auto fnType = Method->getLoweredFunctionType();
  CalleeInfo info{fnType, fnType, SubstitutionMap()};
  return {std::move(info), getPointerToMethod(), self};
}
