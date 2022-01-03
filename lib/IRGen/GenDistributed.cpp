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

struct AllocationInfo {
  SILType Type;
  const TypeInfo &TI;
  StackAddress Addr;
};

class DistributedAccessor {
  IRGenModule &IGM;
  IRGenFunction &IGF;

  /// Underlying distributed method for this accessor.
  SILFunction *Method;

  /// The interface type of this accessor function.
  CanSILFunctionType AccessorType;
  /// The asynchronous context associated with this accessor.
  AsyncContextLayout AsyncLayout;

  /// The list of all arguments that were allocated on the stack.
  SmallVector<AllocationInfo, 4> AllocatedArguments;

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

  auto getRawPointerParmeter = [&]() {
    auto ptrType = Context.getUnsafeRawPointerType();
    return SILParameterInfo(ptrType->getCanonicalType(),
                            ParameterConvention::Direct_Guaranteed);
  };

  // `self` of the distributed actor is going to be passed as an argument
  // to this accessor function.
  auto extInfo = SILExtInfoBuilder()
                     .withRepresentation(SILFunctionTypeRepresentation::Thin)
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
      ParameterConvention::Direct_Guaranteed,
      {/*argumentBuffer=*/getRawPointerParmeter(),
       /*resultBuffer=*/getRawPointerParmeter(),
       /*actor=*/methodTy->getParameters().back()},
      /*Yields=*/{},
      /*Results=*/{},
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

  // Cover all of the arguments except to `self` of the actor.
  auto parameters = fnType->getParameters().drop_back();

  // If there are no parameters to extract, we are done.
  if (parameters.empty())
    return;

  auto offset =
      IGF.createAlloca(IGM.Int8PtrTy, IGM.getPointerAlignment(), "offset");
  IGF.Builder.CreateLifetimeStart(offset, IGM.getPointerSize());

  // Initialize "offset" with the address of the base of the argument buffer.
  IGF.Builder.CreateStore(argumentBuffer, offset);

  for (const auto &param : parameters) {
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

    if (paramTy.isObject()) {
      auto &nativeSchema = typeInfo.nativeParameterValueSchema(IGM);
      // If schema is empty, skip to the next argument.
      if (nativeSchema.empty())
        continue;
    }

    // 4. Create an exploded version of the type to pass as an
    //    argument to distributed method.

    switch (param.getConvention()) {
    case ParameterConvention::Indirect_In:
    case ParameterConvention::Indirect_In_Constant: {
      // The +1 argument is passed indirectly, so we need to copy it into
      // a temporary.

      auto stackAddr = typeInfo.allocateStack(IGF, paramTy, "arg.temp");
      auto argPtr = stackAddr.getAddress().getAddress();

      typeInfo.initializeWithCopy(IGF, stackAddr.getAddress(), alignedOffset,
                                  paramTy, /*isOutlined=*/false);
      arguments.add(argPtr);

      // Remember to deallocate later.
      AllocatedArguments.push_back({paramTy, typeInfo, stackAddr});
      break;
    }

    case ParameterConvention::Indirect_In_Guaranteed: {
      // The argument is +0, so we can use the address of the param in
      // the context directly.
      arguments.add(alignedOffset.getAddress());
      break;
    }

    case ParameterConvention::Indirect_Inout:
    case ParameterConvention::Indirect_InoutAliasable:
      llvm_unreachable("indirect parameters are not supported");

    case ParameterConvention::Direct_Guaranteed:
    case ParameterConvention::Direct_Unowned: {
      cast<LoadableTypeInfo>(typeInfo).loadAsTake(IGF, alignedOffset,
                                                  arguments);
      break;
    }

    case ParameterConvention::Direct_Owned:
      // Copy the value out at +1.
      cast<LoadableTypeInfo>(typeInfo).loadAsCopy(IGF, alignedOffset,
                                                  arguments);
    }

    // 6. Move the offset to the beginning of the next element, unless
    //    this is the last element.
    if (param != parameters.back()) {
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
  SILFunctionConventions targetConv(methodTy, IGF.getSILModule());
  SILFunctionConventions accessorConv(AccessorType, IGF.getSILModule());
  TypeExpansionContext expansionContext = IGM.getMaximalTypeExpansionContext();

  auto params = IGF.collectParameters();

  auto directResultTy = targetConv.getSILResultType(expansionContext);
  const auto &directResultTI = IGM.getTypeInfo(directResultTy);

  Explosion arguments;

  unsigned numAsyncContextParams =
      (unsigned)AsyncFunctionArgumentIndex::Context + 1;
  (void)params.claim(numAsyncContextParams);

  // UnsafeRawPointer that holds all of the argument values.
  auto *argBuffer = params.claimNext();
  // UnsafeRawPointer that is used to store the result.
  auto *resultBuffer = params.claimNext();
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

  auto *typedResultBuffer = IGF.Builder.CreateBitCast(
    resultBuffer, IGM.getStoragePointerType(directResultTy));

  if (targetConv.getNumIndirectSILResults()) {
    // Since tuples are not allowed as valid result types (because they cannot
    // conform to protocols), there could be only a single indirect result type
    // associated with distributed method.
    assert(targetConv.getNumIndirectSILResults() == 1);
    arguments.add(typedResultBuffer);
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

    // Load result of the thunk into the location provided by the caller.
    // This would only generate code for direct results, if thunk has an
    // indirect result (e.g. large struct) it result buffer would be passed
    // as an argument.
    {
      Address resultAddr(typedResultBuffer,
                         directResultTI.getBestKnownAlignment());
      emission->emitToMemory(resultAddr, cast<LoadableTypeInfo>(directResultTI),
                             /*isOutlined=*/false);
    }

    // Both accessor and distributed method are always `async throws`
    // so we need to load error value (if any) from the slot.
    {
      assert(methodTy->hasErrorResult());

      SILType errorType = accessorConv.getSILErrorType(expansionContext);
      Address calleeErrorSlot =
          emission->getCalleeErrorSlot(errorType, /*isCalleeAsync=*/true);
      error.add(IGF.Builder.CreateLoad(calleeErrorSlot));
    }

    emission->end();

    // Deallocate all of the copied arguments.
    {
      for (auto &entry : AllocatedArguments)
        entry.TI.deallocateStack(IGF, entry.Addr, entry.Type);
    }

    Explosion voidResult;
    emitAsyncReturn(IGF, AsyncLayout,
                    accessorConv.getSILResultType(expansionContext),
                    AccessorType, voidResult, error);
  }
}

FunctionPointer DistributedAccessor::getPointerToMethod() const {
  auto fnType = Method->getLoweredFunctionType();
  auto fpKind = classifyFunctionPointerKind(Method);
  auto signature = IGM.getSignature(fnType, fpKind.useSpecialConvention());

  auto *fnPtr =
    llvm::ConstantExpr::getBitCast(IGM.getAddrOfAsyncFunctionPointer(Method),
                                   signature.getType()->getPointerTo());

  return FunctionPointer::forDirect(
      FunctionPointer::Kind(fnType), fnPtr,
      IGM.getAddrOfSILFunction(Method, NotForDefinition), signature);
}

Callee
DistributedAccessor::getCalleeForDistributedMethod(llvm::Value *self) const {
  auto fnType = Method->getLoweredFunctionType();
  CalleeInfo info{fnType, fnType, SubstitutionMap()};
  return {std::move(info), getPointerToMethod(), self};
}
