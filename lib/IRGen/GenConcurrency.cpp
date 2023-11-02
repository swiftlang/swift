//===--- GenConcurrency.cpp - IRGen for concurrency features --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for concurrency features (other than
//  basic async function lowering, which is more spread out).
//
//===----------------------------------------------------------------------===//

#include "GenConcurrency.h"

#include "BitPatternBuilder.h"
#include "ExtraInhabitants.h"
#include "GenCall.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/ABI/MetadataValues.h"

using namespace swift;
using namespace irgen;

namespace {

/// A TypeInfo implementation for Builtin.Executor.
class ExecutorTypeInfo :
  public TrivialScalarPairTypeInfo<ExecutorTypeInfo, LoadableTypeInfo> {

public:
  ExecutorTypeInfo(llvm::StructType *storageType,
                   Size size, Alignment align, SpareBitVector &&spareBits)
      : TrivialScalarPairTypeInfo(storageType, size, std::move(spareBits),
                                  align, IsTriviallyDestroyable,
                                  IsCopyable, IsFixedSize) {}

  static Size getFirstElementSize(IRGenModule &IGM) {
    return IGM.getPointerSize();
  }
  static StringRef getFirstElementLabel() {
    return ".identity";
  }

  TypeLayoutEntry
  *buildTypeLayoutEntry(IRGenModule &IGM,
                        SILType T,
                        bool useStructLayouts) const override {
    if (!useStructLayouts) {
      return IGM.typeLayoutCache.getOrCreateTypeInfoBasedEntry(*this, T);
    }
    return IGM.typeLayoutCache.getOrCreateScalarEntry(*this, T,
                                            ScalarKind::TriviallyDestroyable);
  }

  static Size getSecondElementOffset(IRGenModule &IGM) {
    return IGM.getPointerSize();
  }
  static Size getSecondElementSize(IRGenModule &IGM) {
    return IGM.getPointerSize();
  }
  static StringRef getSecondElementLabel() {
    return ".impl";
  }

  // The identity pointer is a heap object reference.
  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }
  PointerInfo getPointerInfo(IRGenModule &IGM) const {
    return PointerInfo::forHeapObject(IGM);
  }
  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return getPointerInfo(IGM).getExtraInhabitantCount(IGM);
  }
  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                     unsigned bits,
                                     unsigned index) const override {
    return getPointerInfo(IGM)
          .getFixedExtraInhabitantValue(IGM, bits, index, 0);
  }
  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                       SILType T,
                                       bool isOutlined) const override {
    src = projectFirstElement(IGF, src);
    return getPointerInfo(IGF.IGM).getExtraInhabitantIndex(IGF, src);
  }
  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    // Store the extra-inhabitant value in the first (identity) word.
    auto first = projectFirstElement(IGF, dest);
    getPointerInfo(IGF.IGM).storeExtraInhabitant(IGF, index, first);

    // Zero the second word.
    auto second = projectSecondElement(IGF, dest);
    IGF.Builder.CreateStore(llvm::ConstantInt::get(IGF.IGM.ExecutorSecondTy, 0),
                            second);
  }
};

} // end anonymous namespace

const LoadableTypeInfo &IRGenModule::getExecutorTypeInfo() {
  return Types.getExecutorTypeInfo();
}

const LoadableTypeInfo &TypeConverter::getExecutorTypeInfo() {
  if (ExecutorTI) return *ExecutorTI;

  auto ty = IGM.SwiftExecutorTy;

  SpareBitVector spareBits;
  spareBits.append(IGM.getHeapObjectSpareBits());
  spareBits.appendClearBits(IGM.getPointerSize().getValueInBits());

  ExecutorTI =
    new ExecutorTypeInfo(ty, IGM.getPointerSize() * 2,
                         IGM.getPointerAlignment(),
                         std::move(spareBits));
  ExecutorTI->NextConverted = FirstType;
  FirstType = ExecutorTI;
  return *ExecutorTI;
}

void irgen::emitBuildMainActorExecutorRef(IRGenFunction &IGF,
                                          Explosion &out) {
  auto call = IGF.Builder.CreateCall(
      IGF.IGM.getTaskGetMainExecutorFunctionPointer(), {});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  IGF.emitAllExtractValues(call, IGF.IGM.SwiftExecutorTy, out);
}

void irgen::emitBuildDefaultActorExecutorRef(IRGenFunction &IGF,
                                             llvm::Value *actor,
                                             Explosion &out) {
  // The implementation word of a default actor is just a null pointer.
  llvm::Value *identity =
    IGF.Builder.CreatePtrToInt(actor, IGF.IGM.ExecutorFirstTy);
  llvm::Value *impl = llvm::ConstantInt::get(IGF.IGM.ExecutorSecondTy, 0);

  out.add(identity);
  out.add(impl);
}

void irgen::emitBuildOrdinarySerialExecutorRef(IRGenFunction &IGF,
                                               llvm::Value *executor,
                                               CanType executorType,
                                               ProtocolConformanceRef executorConf,
                                               Explosion &out) {
  // The implementation word of an "ordinary" serial executor is
  // just the witness table pointer with no flags set.
  llvm::Value *identity =
    IGF.Builder.CreatePtrToInt(executor, IGF.IGM.ExecutorFirstTy);
  llvm::Value *impl =
    emitWitnessTableRef(IGF, executorType, executorConf);
  impl = IGF.Builder.CreatePtrToInt(impl, IGF.IGM.ExecutorSecondTy);

  out.add(identity);
  out.add(impl);
}

void irgen::emitBuildComplexEqualitySerialExecutorRef(IRGenFunction &IGF,
                                               llvm::Value *executor,
                                               CanType executorType,
                                               ProtocolConformanceRef executorConf,
                                               Explosion &out) {
  llvm::Value *identity =
    IGF.Builder.CreatePtrToInt(executor, IGF.IGM.ExecutorFirstTy);

  // The implementation word of an "complex equality" serial executor is
  // the witness table pointer with the ExecutorKind::ComplexEquality flag set.
  llvm::Value *impl =
    emitWitnessTableRef(IGF, executorType, executorConf);
  impl = IGF.Builder.CreatePtrToInt(impl, IGF.IGM.ExecutorSecondTy);

  // NOTE: Refer to SerialExecutorRef::ExecutorKind for the flag values.
  llvm::IntegerType *IntPtrTy = IGF.IGM.IntPtrTy;
  auto complexEqualityExecutorKindFlag =
      llvm::Constant::getIntegerValue(IntPtrTy, APInt(IntPtrTy->getBitWidth(),
                                                      0b01));
  impl = IGF.Builder.CreateOr(impl, complexEqualityExecutorKindFlag);

  out.add(identity);
  out.add(impl);
}

void irgen::emitGetCurrentExecutor(IRGenFunction &IGF, Explosion &out) {
  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getTaskGetCurrentExecutorFunctionPointer(), {});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  IGF.emitAllExtractValues(call, IGF.IGM.SwiftExecutorTy, out);
}

llvm::Value *irgen::emitBuiltinStartAsyncLet(IRGenFunction &IGF,
                                             llvm::Value *taskOptions,
                                             llvm::Value *taskFunction,
                                             llvm::Value *localContextInfo,
                                             llvm::Value *localResultBuffer,
                                             SubstitutionMap subs) {
  localContextInfo = IGF.Builder.CreateBitCast(localContextInfo,
                                               IGF.IGM.OpaquePtrTy);
  
  // stack allocate AsyncLet, and begin lifetime for it (until EndAsyncLet)
  auto ty = llvm::ArrayType::get(IGF.IGM.Int8PtrTy, NumWords_AsyncLet);
  auto address = IGF.createAlloca(ty, Alignment(Alignment_AsyncLet));
  auto alet = IGF.Builder.CreateBitCast(address.getAddress(),
                                        IGF.IGM.Int8PtrTy);
  IGF.Builder.CreateLifetimeStart(alet);

  assert(subs.getReplacementTypes().size() == 1 &&
         "startAsyncLet should have a type substitution");
  auto futureResultType = subs.getReplacementTypes()[0]->getCanonicalType();

  llvm::Value *futureResultTypeMetadata =
      llvm::ConstantPointerNull::get(IGF.IGM.Int8PtrTy);
  if (!IGF.IGM.Context.LangOpts.hasFeature(Feature::Embedded)) {
    futureResultTypeMetadata =
        IGF.emitAbstractTypeMetadataRef(futureResultType);
  }

  // The concurrency runtime for older Apple OSes has a bug in task formation
  // for `async let`s that may manifest when trying to use room in the
  // parent task's preallocated `async let` buffer for the child task's
  // initial task allocator slab. If targeting those older OSes, pad the
  // context size for async let entry points to never fit in the preallocated
  // space, so that we don't run into that bug. We leave a note on the
  // declaration so that coroutine splitting can pad out the final context
  // size after splitting.
  auto deploymentAvailability
    = AvailabilityContext::forDeploymentTarget(IGF.IGM.Context);
  if (!deploymentAvailability.isContainedIn(
                                   IGF.IGM.Context.getSwift57Availability()))
  {
    auto taskAsyncFunctionPointer
                = cast<llvm::GlobalVariable>(taskFunction->stripPointerCasts());

    if (auto taskAsyncID
          = IGF.IGM.getAsyncCoroIDMapping(taskAsyncFunctionPointer)) {
      // If the entry point function has already been emitted, retroactively
      // pad out the initial context size in the async function pointer record
      // and ID intrinsic so that it will never fit in the preallocated space.
      uint64_t origSize = cast<llvm::ConstantInt>(taskAsyncID->getArgOperand(0))
        ->getValue().getLimitedValue();
      
      uint64_t paddedSize = std::max(origSize,
                     (NumWords_AsyncLet * IGF.IGM.getPointerSize()).getValue());
      auto paddedSizeVal = llvm::ConstantInt::get(IGF.IGM.Int32Ty, paddedSize);
      taskAsyncID->setArgOperand(0, paddedSizeVal);
      
      auto origInit = taskAsyncFunctionPointer->getInitializer();
      auto newInit = llvm::ConstantStruct::get(
                                   cast<llvm::StructType>(origInit->getType()),
                                   origInit->getAggregateElement(0u),
                                   paddedSizeVal);
      taskAsyncFunctionPointer->setInitializer(newInit);
    } else {
      // If it hasn't been emitted yet, mark it to get the padding when it does
      // get emitted.
      IGF.IGM.markAsyncFunctionPointerForPadding(taskAsyncFunctionPointer);
    }
  }

  // In embedded Swift, create and pass result type info.
  taskOptions = addEmbeddedSwiftResultTypeInfo(IGF, taskOptions, subs);
  
  llvm::CallInst *call;
  if (localResultBuffer) {
    // This is @_silgen_name("swift_asyncLet_begin")
    call = IGF.Builder.CreateCall(IGF.IGM.getAsyncLetBeginFunctionPointer(),
                                  {alet, taskOptions, futureResultTypeMetadata,
                                   taskFunction, localContextInfo,
                                   localResultBuffer});
  } else {
    // This is @_silgen_name("swift_asyncLet_start")
    call = IGF.Builder.CreateCall(IGF.IGM.getAsyncLetStartFunctionPointer(),
                                  {alet, taskOptions, futureResultTypeMetadata,
                                   taskFunction, localContextInfo});
  }
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  return alet;
}

void irgen::emitEndAsyncLet(IRGenFunction &IGF, llvm::Value *alet) {
  auto *call =
      IGF.Builder.CreateCall(IGF.IGM.getEndAsyncLetFunctionPointer(), {alet});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  IGF.Builder.CreateLifetimeEnd(alet);
}

llvm::Value *irgen::emitCreateTaskGroup(IRGenFunction &IGF,
                                        SubstitutionMap subs,
                                        llvm::Value *groupFlags) {
  auto ty = llvm::ArrayType::get(IGF.IGM.Int8PtrTy, NumWords_TaskGroup);
  auto address = IGF.createAlloca(ty, Alignment(Alignment_TaskGroup));
  auto group = IGF.Builder.CreateBitCast(address.getAddress(),
                                         IGF.IGM.Int8PtrTy);
  IGF.Builder.CreateLifetimeStart(group);
  assert(subs.getReplacementTypes().size() == 1 &&
         "createTaskGroup should have a type substitution");
  auto resultType = subs.getReplacementTypes()[0]->getCanonicalType();
  auto resultTypeMetadata = IGF.emitAbstractTypeMetadataRef(resultType);

  llvm::CallInst *call;
  if (groupFlags) {
    call = IGF.Builder.CreateCall(IGF.IGM.getTaskGroupInitializeWithFlagsFunctionPointer(),
                                  {groupFlags, group, resultTypeMetadata});
  } else {
    call = IGF.Builder.CreateCall(IGF.IGM.getTaskGroupInitializeFunctionPointer(),
                                  {group, resultTypeMetadata});
  }
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  return group;
}

void irgen::emitDestroyTaskGroup(IRGenFunction &IGF, llvm::Value *group) {
  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getTaskGroupDestroyFunctionPointer(), {group});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  IGF.Builder.CreateLifetimeEnd(group);
}

llvm::Function *IRGenModule::getAwaitAsyncContinuationFn() {
  StringRef name = "__swift_continuation_await_point";
  if (llvm::GlobalValue *F = Module.getNamedValue(name))
    return cast<llvm::Function>(F);

  // The parameters here match the extra arguments passed to
  // @llvm.coro.suspend.async by emitAwaitAsyncContinuation.
  llvm::Type *argTys[] = { ContinuationAsyncContextPtrTy };
  auto *suspendFnTy =
    llvm::FunctionType::get(VoidTy, argTys, false /*vaargs*/);

  llvm::Function *suspendFn =
      llvm::Function::Create(suspendFnTy, llvm::Function::InternalLinkage,
                             name, &Module);
  suspendFn->setCallingConv(SwiftAsyncCC);
  suspendFn->setDoesNotThrow();
  IRGenFunction suspendIGF(*this, suspendFn);
  if (DebugInfo)
    DebugInfo->emitArtificialFunction(suspendIGF, suspendFn);
  auto &Builder = suspendIGF.Builder;

  llvm::Value *context = suspendFn->getArg(0);
  auto *call =
      Builder.CreateCall(getContinuationAwaitFunctionPointer(), {context});
  call->setCallingConv(SwiftAsyncCC);
  call->setDoesNotThrow();
  call->setTailCallKind(AsyncTailCallKind);

  Builder.CreateRetVoid();
  return suspendFn;
}

void irgen::emitTaskRunInline(IRGenFunction &IGF, SubstitutionMap subs,
                              llvm::Value *result, llvm::Value *closure,
                              llvm::Value *closureContext) {
  assert(subs.getReplacementTypes().size() == 1 &&
         "taskRunInline should have a type substitution");
  auto resultType = subs.getReplacementTypes()[0]->getCanonicalType();
  auto resultTypeMetadata = IGF.emitAbstractTypeMetadataRef(resultType);

  auto *call = IGF.Builder.CreateCall(
      IGF.IGM.getTaskRunInlineFunctionPointer(),
      {result, closure, closureContext, resultTypeMetadata});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);
}
