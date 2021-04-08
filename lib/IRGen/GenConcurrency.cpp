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
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
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
                                  align, IsPOD, IsFixedSize) {}

  static Size getFirstElementSize(IRGenModule &IGM) {
    return IGM.getPointerSize();
  }
  static StringRef getFirstElementLabel() {
    return ".identity";
  }

  TypeLayoutEntry *buildTypeLayoutEntry(IRGenModule &IGM,
                                        SILType T) const override {
    return IGM.typeLayoutCache.getOrCreateScalarEntry(*this, T);
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

  // The identity pointer is a heap object reference, but it's
  // nullable because of the generic executor.
  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }

  PointerInfo getPointerInfo(IRGenModule &IGM) const {
    return PointerInfo::forHeapObject(IGM).withNullable(IsNullable);
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
  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    auto pointerSize = IGM.getPointerSize();
    auto mask = BitPatternBuilder(IGM.Triple.isLittleEndian());
    mask.appendSetBits(pointerSize.getValueInBits());
    mask.appendClearBits(pointerSize.getValueInBits());
    return mask.build().getValue();
  }
  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T,
                            bool isOutlined) const override {
    dest = projectFirstElement(IGF, dest);
    getPointerInfo(IGF.IGM).storeExtraInhabitant(IGF, index, dest);
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

void irgen::emitBuildSerialExecutorRef(IRGenFunction &IGF,
                                       llvm::Value *actor,
                                       SILType actorType,
                                       Explosion &out) {
  auto cls = actorType.getClassOrBoundGenericClass();

  // HACK: if the actor type is Swift.MainActor, treat it specially.
  if (cls && cls->getNameStr() == "MainActor" &&
      cls->getDeclContext()->isModuleScopeContext() &&
      cls->getModuleContext()->getName().str() == SWIFT_CONCURRENCY_NAME) {
    llvm::Value *identity = llvm::ConstantInt::get(IGF.IGM.SizeTy,
                              unsigned(ExecutorRefFlags::MainActorIdentity));
    identity = IGF.Builder.CreateIntToPtr(identity, IGF.IGM.ExecutorFirstTy);
    out.add(identity);

    llvm::Value *impl = IGF.IGM.getSize(Size(0));
    out.add(impl);
    return;
  }

  llvm::Value *identity =
    IGF.Builder.CreateBitCast(actor, IGF.IGM.ExecutorFirstTy);
  llvm::Value *impl = IGF.IGM.getSize(Size(0));

  unsigned flags = 0;

  // FIXME: this isn't how we should be doing any of this
  flags |= unsigned(ExecutorRefFlags::DefaultActor);

  if (flags) {
    impl = IGF.Builder.CreateOr(impl, IGF.IGM.getSize(Size(flags)));
  }
  out.add(identity);
  out.add(impl);
}

void irgen::emitGetCurrentExecutor(IRGenFunction &IGF, Explosion &out) {
  auto *call = IGF.Builder.CreateCall(IGF.IGM.getTaskGetCurrentExecutorFn(),
                                      {});
  call->setDoesNotThrow();
  call->setCallingConv(IGF.IGM.SwiftCC);

  IGF.emitAllExtractValues(call, IGF.IGM.SwiftExecutorTy, out);
}
