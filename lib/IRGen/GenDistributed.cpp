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
#include "GenOpaque.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ExtInfo.h"
#include "swift/AST/GenericEnvironment.h"
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
  unsigned ArgumentIdx;
  StackAddress Addr;
};

class DistributedAccessor {
  IRGenModule &IGM;
  IRGenFunction &IGF;

  /// Underlying distributed method for this accessor.
  SILFunction *Target;

  /// The interface type of this accessor function.
  CanSILFunctionType AccessorType;
  /// The asynchronous context associated with this accessor.
  AsyncContextLayout AsyncLayout;

  /// The list of all arguments that were allocated on the stack.
  SmallVector<AllocationInfo, 4> AllocatedArguments;

public:
  DistributedAccessor(IRGenFunction &IGF, SILFunction *target,
                      CanSILFunctionType accessorTy);

  void emit();

private:
  void computeArguments(llvm::Value *argumentBuffer,
                        llvm::Value *argumentTypes,
                        Explosion &arguments);

  /// Load an argument value from the given buffer \c argumentSlot
  /// to the given explosion \c arguments. Type of the argument is
  /// the same as parameter type.
  ///
  /// Returns a pair of aligned offset and value size.
  std::pair<llvm::Value *, llvm::Value *>
  loadArgument(unsigned argumentIdx,
               llvm::Value *argumentSlot,
               SILType paramType,
               ParameterConvention convention,
               Explosion &arguments);

  /// Load an argument value from the given buffer \c argumentSlot
  /// to the given explosion \c arguments. Information describing
  /// the type of argument comes from runtime metadata.
  ///
  /// Returns a pair of aligned offset and value size.
  std::pair<llvm::Value *, llvm::Value *>
  loadArgument(unsigned argumentIdx,
               llvm::Value *argumentSlot,
               llvm::Value *argumentType,
               const SILParameterInfo &param,
               Explosion &arguments);

  /// Load witness table addresses (if any) from the given buffer
  /// into the given argument explosion.
  ///
  /// Number of witnesses to load is provided by \c numTables but
  /// it's checked against the number of \c expectedWitnessTables.
  void emitLoadOfWitnessTables(llvm::Value *witnessTables,
                               llvm::Value *numTables,
                               unsigned expectedWitnessTables,
                               Explosion &arguments);

  FunctionPointer getPointerToTarget() const;

  Callee getCalleeForDistributedTarget(llvm::Value *self) const;
};

} // end namespace

/// Compute a type of a distributed method accessor function based
/// on the provided distributed method.
static CanSILFunctionType getAccessorType(IRGenModule &IGM,
                                          SILFunction *Target) {
  auto &Context = IGM.Context;

  auto getInvocationDecoderParameter = [&]() {
    auto *actor = Target->getDeclContext()
                      ->getInnermostTypeContext()
                      ->getSelfNominalTypeDecl();
    auto *decoder = Context.getDistributedActorInvocationDecoder(actor);
    auto decoderTy = decoder->getInterfaceType()->getMetatypeInstanceType();
    auto paramType = IGM.getLoweredType(decoderTy);
    return SILParameterInfo(paramType.getASTType(),
                            ParameterConvention::Direct_Guaranteed);
  };

  auto getRawPointerParameter = [&]() {
    auto ptrType = Context.getUnsafeRawPointerType();
    return SILParameterInfo(ptrType->getCanonicalType(),
                            ParameterConvention::Direct_Unowned);
  };

  auto getUIntParameter = [&]() {
    return SILParameterInfo(Context.getUIntType()->getCanonicalType(),
                            ParameterConvention::Direct_Unowned);
  };

  // `self` of the distributed actor is going to be passed as an argument
  // to this accessor function.
  auto extInfo = SILExtInfoBuilder()
                     .withRepresentation(SILFunctionTypeRepresentation::Thin)
                     .withAsync()
                     .build();

  auto targetTy = Target->getLoweredFunctionType();

  assert(targetTy->isAsync());
  assert(targetTy->hasErrorResult());

  // Accessor gets argument/result value buffer and a reference to `self` of
  // the actor and produces a call to the distributed thunk forwarding
  // its result(s) out.
  return SILFunctionType::get(
      /*genericSignature=*/nullptr, extInfo, SILCoroutineKind::None,
      ParameterConvention::Direct_Guaranteed,
      {/*argumentDecoder=*/getInvocationDecoderParameter(),
       /*argumentTypes=*/getRawPointerParameter(),
       /*resultBuffer=*/getRawPointerParameter(),
       /*substitutions=*/getRawPointerParameter(),
       /*witnessTables=*/getRawPointerParameter(),
       /*numWitnessTables=*/getUIntParameter(),
       /*actor=*/targetTy->getParameters().back()},
      /*Yields=*/{},
      /*Results=*/{},
      /*ErrorResult=*/targetTy->getErrorResult(),
      /*patternSubs=*/SubstitutionMap(),
      /*invocationSubs=*/SubstitutionMap(), Context);
}

llvm::Function *
IRGenModule::getAddrOfDistributedTargetAccessor(SILFunction *F,
                                                ForDefinition_t forDefinition) {
  auto entity = LinkEntity::forDistributedTargetAccessor(F);

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

void IRGenModule::emitDistributedTargetAccessor(SILFunction *target) {
  assert(target->isDistributed());

  auto *f = getAddrOfDistributedTargetAccessor(target, ForDefinition);
  if (!f->isDeclaration())
    return;

  IRGenFunction IGF(*this, f);
  DistributedAccessor(IGF, target, getAccessorType(*this, target)).emit();
}

DistributedAccessor::DistributedAccessor(IRGenFunction &IGF,
                                         SILFunction *target,
                                         CanSILFunctionType accessorTy)
    : IGM(IGF.IGM), IGF(IGF), Target(target), AccessorType(accessorTy),
      AsyncLayout(getAsyncContextLayout(
          IGM, AccessorType, AccessorType, SubstitutionMap(),
          /*suppress generics*/ true,
          FunctionPointer::Kind(
              FunctionPointer::BasicKind::AsyncFunctionPointer))) {}

void DistributedAccessor::computeArguments(llvm::Value *argumentBuffer,
                                           llvm::Value *argumentTypes,
                                           Explosion &arguments) {
  auto fnType = Target->getLoweredFunctionType();

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

  // Cast type buffer to `swift.type**`
  argumentTypes =
      IGF.Builder.CreateBitCast(argumentTypes, IGM.TypeMetadataPtrPtrTy);

  for (unsigned i = 0, n = parameters.size(); i != n; ++i) {
    const auto &param = parameters[i];
    auto paramTy = param.getSILStorageInterfaceType();

    // The size of the loaded argument value
    llvm::Value *valueSize;

    // Load current offset
    llvm::Value *currentOffset = IGF.Builder.CreateLoad(offset, "elt_offset");

    if (paramTy.hasTypeParameter()) {
      auto offset =
          Size(i * IGM.DataLayout.getTypeAllocSize(IGM.TypeMetadataPtrTy));
      auto alignment =
          IGM.DataLayout.getABITypeAlignment(IGM.TypeMetadataPtrTy);

      // Load metadata describing argument value from argument types buffer.
      auto typeLoc = IGF.emitAddressAtOffset(
          argumentTypes, Offset(offset), IGM.TypeMetadataPtrTy,
          Alignment(alignment), "arg_type_loc");

      auto *argumentTy = IGF.Builder.CreateLoad(typeLoc, "arg_type");

      // Load argument value based using loaded type metadata.
      std::tie(currentOffset, valueSize) =
          loadArgument(i, currentOffset, argumentTy, param, arguments);
    } else {
      std::tie(currentOffset, valueSize) = loadArgument(
          i, currentOffset, paramTy, param.getConvention(), arguments);
    }

    // Move the offset to the beginning of the next element, unless
    // this is the last element
    if (param != parameters.back()) {
      llvm::Value *addr = IGF.Builder.CreatePtrToInt(currentOffset, IGM.IntPtrTy);
      llvm::Value *nextOffset = IGF.Builder.CreateIntToPtr(
          IGF.Builder.CreateAdd(addr, valueSize), IGM.Int8PtrTy);
      IGF.Builder.CreateStore(nextOffset, offset);
    }
  }

  IGF.Builder.CreateLifetimeEnd(offset, IGM.getPointerSize());
}

std::pair<llvm::Value *, llvm::Value *>
DistributedAccessor::loadArgument(unsigned argumentIdx,
                                  llvm::Value *argumentSlot,
                                  llvm::Value *argumentType,
                                  const SILParameterInfo &param,
                                  Explosion &arguments) {
  // TODO: `emitLoad*` would actually load value witness table every
  // time it's called, which is sub-optimal but all of the APIs that
  // deal with value witness tables are currently hidden in GenOpaque.cpp
  llvm::Value *valueSize = emitLoadOfSize(IGF, argumentType);

  llvm::Value *isInline, *flags;
  std::tie(isInline, flags) = emitLoadOfIsInline(IGF, argumentType);

  llvm::Value *alignmentMask = emitAlignMaskFromFlags(IGF, flags);

  // Align argument value offset as required by its type metadata.
  llvm::Value *alignedSlot =
      IGF.Builder.CreatePtrToInt(argumentSlot, IGM.IntPtrTy);
  {
    alignedSlot = IGF.Builder.CreateNUWAdd(alignedSlot, alignmentMask);

    llvm::Value *invertedMask = IGF.Builder.CreateNot(alignmentMask);
    alignedSlot = IGF.Builder.CreateAnd(alignedSlot, invertedMask);
    alignedSlot =
        IGF.Builder.CreateIntToPtr(alignedSlot, IGM.OpaquePtrTy);
  }

  Address argumentAddr(alignedSlot, IGM.getPointerAlignment());

  switch (param.getConvention()) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant: {
    // The only way to load opaque type is to allocate a temporary
    // variable on the stack for it and initialize from the given address
    // either at +0 or +1 depending on convention.

    auto stackAddr =
        IGF.emitDynamicAlloca(IGM.Int8Ty, valueSize, Alignment(16));
    auto argPtr = stackAddr.getAddress().getAddress();

    emitInitializeWithCopyCall(IGF, argumentType, stackAddr.getAddress(),
                               argumentAddr);

    arguments.add(argPtr);

    // Remember to deallocate later.
    AllocatedArguments.push_back({argumentIdx, stackAddr});
    break;
  }

  case ParameterConvention::Indirect_In_Guaranteed: {
    // The argument is +0, so we can use the address of the param in
    // the context directly.
    arguments.add(alignedSlot);
    break;
  }

  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
    llvm_unreachable("indirect 'inout' parameters are not supported");

  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Unowned: {
    auto paramTy = param.getSILStorageInterfaceType();
    auto &typeInfo = IGM.getTypeInfo(paramTy);
    Address eltPtr = IGF.Builder.CreateBitCast(
        argumentAddr, IGM.getStoragePointerType(paramTy));

    cast<LoadableTypeInfo>(typeInfo).loadAsTake(IGF, eltPtr, arguments);
    break;
  }

  case ParameterConvention::Direct_Owned: {
    auto &typeInfo = IGM.getTypeInfo(param.getSILStorageInterfaceType());
    // Copy the value out at +1.
    cast<LoadableTypeInfo>(typeInfo).loadAsCopy(IGF, argumentAddr, arguments);
    break;
  }
  }

  return {alignedSlot, valueSize};
}

std::pair<llvm::Value *, llvm::Value *>
DistributedAccessor::loadArgument(unsigned argumentIdx,
                                  llvm::Value *argumentSlot,
                                  SILType paramTy,
                                  ParameterConvention convention,
                                  Explosion &arguments) {
  const TypeInfo &typeInfo = IGF.getTypeInfo(paramTy);

  // 1. Check whether the native representation is empty e.g.
  //    this happens for empty enums.
  if (paramTy.isObject()) {
    auto &nativeSchema = typeInfo.nativeParameterValueSchema(IGM);
    // If schema is empty, skip to the next argument.
    if (nativeSchema.empty())
      return {argumentSlot, typeInfo.getSize(IGF, paramTy)};
  }

  // 2. Cast the pointer to the type of the element.
  Address eltPtr = IGF.Builder.CreateBitCast(
      Address(argumentSlot, IGM.getPointerAlignment()),
      IGM.getStoragePointerType(paramTy));

  // 3. Adjust typed pointer to the alignment of the type.
  auto alignedOffset = typeInfo.roundUpToTypeAlignment(IGF, eltPtr, paramTy);

  // 4. Create an exploded version of the type to pass as an
  //    argument to distributed method.

  switch (convention) {
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
    AllocatedArguments.push_back({argumentIdx, stackAddr});
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
    llvm_unreachable("indirect 'inout' parameters are not supported");

  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Unowned: {
    cast<LoadableTypeInfo>(typeInfo).loadAsTake(IGF, alignedOffset, arguments);
    break;
  }

  case ParameterConvention::Direct_Owned:
    // Copy the value out at +1.
    cast<LoadableTypeInfo>(typeInfo).loadAsCopy(IGF, alignedOffset, arguments);
    break;
  }

  return {alignedOffset.getAddress(), typeInfo.getSize(IGF, paramTy)};
}

void DistributedAccessor::emitLoadOfWitnessTables(llvm::Value *witnessTables,
                                                  llvm::Value *numTables,
                                                  unsigned expectedWitnessTables,
                                                  Explosion &arguments) {
  auto contBB = IGF.createBasicBlock("");
  auto unreachableBB = IGF.createBasicBlock("incorrect-witness-tables");

  auto incorrectNum = IGF.Builder.CreateICmpNE(
      numTables, llvm::ConstantInt::get(IGM.SizeTy, expectedWitnessTables));

  // Make sure that we have a correct number of witness tables provided to us.
  IGF.Builder.CreateCondBr(incorrectNum, unreachableBB, contBB);
  {
    IGF.Builder.emitBlock(unreachableBB);
    IGF.Builder.CreateUnreachable();
  }

  IGF.Builder.emitBlock(contBB);

  witnessTables = IGF.Builder.CreateBitCast(witnessTables, IGM.Int8PtrPtrTy);

  for (unsigned i = 0, n = expectedWitnessTables; i != n; ++i) {
    auto offset = Size(i * IGM.getPointerSize());
    auto alignment = IGM.getPointerAlignment();

    auto witnessTableAddr = IGF.emitAddressAtOffset(
        witnessTables, Offset(offset), IGM.Int8PtrTy, Alignment(alignment));
    arguments.add(witnessTableAddr.getAddress());
  }
}

void DistributedAccessor::emit() {
  auto targetTy = Target->getLoweredFunctionType();
  SILFunctionConventions targetConv(targetTy, IGF.getSILModule());
  SILFunctionConventions accessorConv(AccessorType, IGF.getSILModule());
  TypeExpansionContext expansionContext = IGM.getMaximalTypeExpansionContext();

  auto params = IGF.collectParameters();

  auto directResultTy = targetConv.getSILResultType(expansionContext);
  const auto &directResultTI = IGM.getTypeInfo(directResultTy);

  Explosion arguments;

  unsigned numAsyncContextParams =
      (unsigned)AsyncFunctionArgumentIndex::Context + 1;
  (void)params.claim(numAsyncContextParams);

  // A container that produces argument values based on the given set of
  // argument types (supplied as a next argument).
  auto *argDecoder = params.claimNext();
  // `swift.type**` that holds the argument types that correspond to values.
  auto *argTypes = params.claimNext();
  // UnsafeRawPointer that is used to store the result.
  auto *resultBuffer = params.claimNext();
  // UnsafeRawPointer that represents a list of substitutions
  auto *substitutions = params.claimNext();
  // UnsafeRawPointer that represents a list of witness tables
  auto *witnessTables = params.claimNext();
  // Integer that represented the number of witness tables
  auto *numWitnessTables = params.claimNext();
  // Reference to a `self` of the actor to be called.
  auto *actorSelf = params.claimNext();

  GenericContextScope scope(IGM, targetTy->getInvocationGenericSignature());

  // Preliminary: Setup async context for this accessor.
  {
    auto asyncContextIdx =
        Signature::forAsyncEntry(IGM, AccessorType,
                                 /*useSpecialConvention*/ false)
            .getAsyncContextIndex();

    auto entity = LinkEntity::forDistributedTargetAccessor(Target);
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
  computeArguments(argBuffer, argTypes, arguments);

  // Add all of the substitutions to the explosion
  if (auto *genericEnvironment = Target->getGenericEnvironment()) {
    // swift.type **
    llvm::Value *substitutionBuffer =
        IGF.Builder.CreateBitCast(substitutions, IGM.TypeMetadataPtrPtrTy);

    // Collect the generic arguments expected by the distributed thunk.
    // We need this to determine the expected number of witness tables
    // to load from the buffer provided by the caller.
    llvm::SmallVector<llvm::Type *, 4> targetGenericArguments;
    expandPolymorphicSignature(IGM, targetTy, targetGenericArguments);

    unsigned numGenericArgs = genericEnvironment->getGenericParams().size();
    unsigned expectedWitnessTables =
        targetGenericArguments.size() - numGenericArgs;

    for (unsigned index = 0; index < numGenericArgs; ++index) {
      auto offset =
          Size(index * IGM.DataLayout.getTypeAllocSize(IGM.TypeMetadataPtrTy));
      auto alignment =
          IGM.DataLayout.getABITypeAlignment(IGM.TypeMetadataPtrTy);

      auto substitution =
          IGF.emitAddressAtOffset(substitutionBuffer, Offset(offset),
                                  IGM.TypeMetadataPtrTy, Alignment(alignment));
      arguments.add(IGF.Builder.CreateLoad(substitution, "substitution"));
    }

    emitLoadOfWitnessTables(witnessTables, numWitnessTables,
                            expectedWitnessTables, arguments);
  }

  // Step two, let's form and emit a call to the distributed method
  // using computed argument explosion.
  {
    Explosion result;
    Explosion error;

    auto callee = getCalleeForDistributedTarget(actorSelf);
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
      assert(targetTy->hasErrorResult());

      SILType errorType = accessorConv.getSILErrorType(expansionContext);
      Address calleeErrorSlot =
          emission->getCalleeErrorSlot(errorType, /*isCalleeAsync=*/true);
      error.add(IGF.Builder.CreateLoad(calleeErrorSlot));
    }

    emission->end();

    // Deallocate all of the copied arguments.
    {
      auto targetType = Target->getLoweredFunctionType();
      for (auto &alloca : AllocatedArguments) {
        const auto &param = targetType->getParameters()[alloca.ArgumentIdx];
        auto paramTy = param.getSILStorageInterfaceType();

        if (paramTy.hasTypeParameter()) {
          IGF.emitDeallocateDynamicAlloca(alloca.Addr);
        } else {
          auto &typeInfo = IGF.getTypeInfo(paramTy);
          typeInfo.deallocateStack(IGF, alloca.Addr, paramTy);
        }
      }
    }

    Explosion voidResult;
    emitAsyncReturn(IGF, AsyncLayout,
                    accessorConv.getSILResultType(expansionContext),
                    AccessorType, voidResult, error);
  }
}

FunctionPointer DistributedAccessor::getPointerToTarget() const {
  auto fnType = Target->getLoweredFunctionType();
  auto fpKind = classifyFunctionPointerKind(Target);
  auto signature = IGM.getSignature(fnType, fpKind.useSpecialConvention());

  auto *fnPtr =
    llvm::ConstantExpr::getBitCast(IGM.getAddrOfAsyncFunctionPointer(Target),
                                   signature.getType()->getPointerTo());

  return FunctionPointer::forDirect(
      FunctionPointer::Kind(fnType), fnPtr,
      IGM.getAddrOfSILFunction(Target, NotForDefinition), signature);
}

Callee
DistributedAccessor::getCalleeForDistributedTarget(llvm::Value *self) const {
  auto fnType = Target->getLoweredFunctionType();
  CalleeInfo info{fnType, fnType, SubstitutionMap()};
  return {std::move(info), getPointerToTarget(), self};
}
