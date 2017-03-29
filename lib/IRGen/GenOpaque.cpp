//===--- GenOpaque.cpp - Swift IR-generation for opaque values ------------===//
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
//  This file implements IR generation for opaque values and value
//  witness operations.
//
//  In the comments throughout this file, three type names are used:
//    'B' is the type of a fixed-size buffer
//    'T' is the type which implements a protocol
//    'W' is the type of a witness to the protocol
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "swift/IRGen/ValueWitness.h"

#include "FixedTypeInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ProtocolInfo.h"

#include "GenOpaque.h"

using namespace swift;
using namespace irgen;

/// A fixed-size buffer is always 16 bytes and pointer-aligned.
/// If we align them more, we'll need to introduce padding to
/// make protocol types work.
Size irgen::getFixedBufferSize(IRGenModule &IGM) {
  return 3 * IGM.getPointerSize();
}
Alignment irgen::getFixedBufferAlignment(IRGenModule &IGM) {
  return IGM.getPointerAlignment();
}

/// Lazily create the standard fixed-buffer type.
llvm::Type *IRGenModule::getFixedBufferTy() {
  if (FixedBufferTy) return FixedBufferTy;

  auto size = getFixedBufferSize(*this).getValue();
  FixedBufferTy = llvm::ArrayType::get(Int8Ty, size);
  return FixedBufferTy;
}

static llvm::Type *createWitnessType(IRGenModule &IGM, ValueWitness index) {
  switch (index) {
  // void (*deallocateBuffer)(B *buffer, M *self);
  // void (*destroyBuffer)(B *buffer, M *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // void (*destroyArray)(T *object, size_t n, witness_t *self);
  case ValueWitness::DestroyArray: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.SizeTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, M *self);
  // T *(*initializeBufferWithTakeOfBuffer)(B *dest, B *src, M *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithTakeOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // T *(*allocateBuffer)(B *buffer, M *self);
  // T *(*projectBuffer)(B *buffer, M *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, M *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, M *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  // T *(*assignWithCopy)(T *dest, T *src, M *self);
  // T *(*assignWithTake)(T *dest, T *src, M *self);
  // T *(*initializeWithCopy)(T *dest, T *src, M *self);
  // T *(*initializeWithTake)(T *dest, T *src, M *self);
  case ValueWitness::AssignWithCopy:
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(ptrTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }
      
  // T *(*initializeArrayWithCopy)(T *dest, T *src, size_t n, M *self);
  // T *(*initializeArrayWithTakeFrontToBack)(T *dest, T *src, size_t n, M *self);
  // T *(*initializeArrayWithTakeBackToFront)(T *dest, T *src, size_t n, M *self);
  case ValueWitness::InitializeArrayWithCopy:
  case ValueWitness::InitializeArrayWithTakeFrontToBack:
  case ValueWitness::InitializeArrayWithTakeBackToFront: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, IGM.SizeTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(ptrTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }
      
  /// void (*storeExtraInhabitant)(T *obj, unsigned index, M *self);
  case ValueWitness::StoreExtraInhabitant: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *args[] = {ptrTy, indexTy, metaTy};
    
    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }
      
  /// int (*getExtraInhabitantIndex)(T *obj, M *self);
  case ValueWitness::GetExtraInhabitantIndex: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    
    llvm::Type *args[] = {ptrTy, metaTy};
    
    return llvm::FunctionType::get(indexTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }
  
  /// int (*getEnumTag)(T *obj, M *self);
  case ValueWitness::GetEnumTag: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;

    llvm::Type *args[] = {ptrTy, metaTy};

    return llvm::FunctionType::get(indexTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  /// void (*destructiveProjectEnumData)(T *obj, M *self);
  case ValueWitness::DestructiveProjectEnumData: {
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = {ptrTy, metaTy};

    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  /// void (*destructiveInjectEnumTag)(T *obj, int tag, M *self);
  case ValueWitness::DestructiveInjectEnumTag: {
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = {ptrTy, indexTy, metaTy};

    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }

  case ValueWitness::Size:
  case ValueWitness::Flags:
  case ValueWitness::Stride:
  case ValueWitness::ExtraInhabitantFlags:
    // Non-function witnesses all have type size_t.
    return IGM.SizeTy;
  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::Type *IRGenModule::getValueWitnessTy(ValueWitness index) {
  assert(unsigned(index) < MaxNumValueWitnesses);
  auto &ty = ValueWitnessTys[unsigned(index)];
  if (ty) return ty;

  ty = createWitnessType(*this, index);
  return ty;
}

static StringRef getValueWitnessLabel(ValueWitness index) {
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    return "deallocateBuffer";
  case ValueWitness::DestroyBuffer:
    return "destroyBuffer";
  case ValueWitness::Destroy:
    return "destroy";
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
    return "initializeBufferWithCopyOfBuffer";
  case ValueWitness::AllocateBuffer:
    return "allocateBuffer";
  case ValueWitness::ProjectBuffer:
    return "projectBuffer";
  case ValueWitness::InitializeBufferWithCopy:
    return "initializeBufferWithCopy";
  case ValueWitness::InitializeBufferWithTake:
    return "initializeBufferWithTake";
  case ValueWitness::AssignWithCopy:
    return "assignWithCopy";
  case ValueWitness::AssignWithTake:
    return "assignWithTake";
  case ValueWitness::InitializeWithCopy:
    return "initializeWithCopy";
  case ValueWitness::InitializeWithTake:
    return "initializeWithTake";
  case ValueWitness::InitializeBufferWithTakeOfBuffer:
    return "initializeBufferWithTakeOfBuffer";
  case ValueWitness::Size:
    return "size";
  case ValueWitness::Flags:
    return "flags";
  case ValueWitness::Stride:
    return "stride";
  case ValueWitness::DestroyArray:
    return "destroyArray";
  case ValueWitness::InitializeArrayWithCopy:
    return "initializeArrayWithCopy";
  case ValueWitness::InitializeArrayWithTakeFrontToBack:
    return "initializeArrayWithTakeFrontToBack";
  case ValueWitness::InitializeArrayWithTakeBackToFront:
    return "initializeArrayWithTakeBackToFront";
  case ValueWitness::StoreExtraInhabitant:
    return "storeExtraInhabitant";
  case ValueWitness::GetExtraInhabitantIndex:
    return "getExtraInhabitantIndex";
  case ValueWitness::ExtraInhabitantFlags:
    return "extraInhabitantFlags";
  case ValueWitness::GetEnumTag:
    return "getEnumTag";
  case ValueWitness::DestructiveProjectEnumData:
    return "destructiveProjectEnumData";
  case ValueWitness::DestructiveInjectEnumTag:
    return "destructiveInjectEnumTag";
  }
  llvm_unreachable("bad value witness index");
}

/// Load a specific witness from a known table.  The result is
/// always an i8*.
llvm::Value *irgen::emitInvariantLoadOfOpaqueWitness(IRGenFunction &IGF,
                                                     llvm::Value *table,
                                                     WitnessIndex index) {
  assert(table->getType() == IGF.IGM.WitnessTablePtrTy);

  // GEP to the appropriate index, avoiding spurious IR in the trivial case.
  llvm::Value *slot = table;
  if (index.getValue() != 0)
    slot = IGF.Builder.CreateConstInBoundsGEP1_32(
        /*Ty=*/nullptr, table, index.getValue());

  auto witness =
    IGF.Builder.CreateLoad(Address(slot, IGF.IGM.getPointerAlignment()));
  IGF.setInvariantLoad(witness);
  return witness;
}

/// Given a value witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
static llvm::Value *emitLoadOfValueWitness(IRGenFunction &IGF,
                                           llvm::Value *table,
                                           ValueWitness index) {
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, table, index);
  auto label = getValueWitnessLabel(index);
  auto type = IGF.IGM.getValueWitnessTy(index);
  if (isValueWitnessFunction(index)) {
    return IGF.Builder.CreateBitCast(witness, type, label);
  } else {
    return IGF.Builder.CreatePtrToInt(witness, type, label);
  }
}

/// Given a type metadata pointer, load one of the value witnesses from its
/// value witness table.
static llvm::Value *emitLoadOfValueWitnessFromMetadata(IRGenFunction &IGF,
                                                       llvm::Value *metadata,
                                                       ValueWitness index) {
  llvm::Value *vwtable = IGF.emitValueWitnessTableRefForMetadata(metadata);
  return emitLoadOfValueWitness(IGF, vwtable, index);
}

llvm::Value *IRGenFunction::emitValueWitness(CanType type, ValueWitness index) {
  if (auto witness =
        tryGetLocalTypeData(type, LocalTypeDataKind::forValueWitness(index)))
    return witness;
  
  auto vwtable = emitValueWitnessTableRef(type);
  auto witness = emitLoadOfValueWitness(*this, vwtable, index);
  setScopedLocalTypeData(type, LocalTypeDataKind::forValueWitness(index),
                         witness);
  return witness;
}

llvm::Value *IRGenFunction::emitValueWitnessForLayout(SILType type,
                                                      ValueWitness index) {
  if (auto witness = tryGetLocalTypeDataForLayout(type,
                                    LocalTypeDataKind::forValueWitness(index)))
    return witness;
  
  auto vwtable = emitValueWitnessTableRefForLayout(type);
  auto witness = emitLoadOfValueWitness(*this, vwtable, index);
  setScopedLocalTypeDataForLayout(type,
                           LocalTypeDataKind::forValueWitness(index), witness);
  return witness;
}

/// Given a call to a helper function that produces a result
/// into its first argument, set attributes appropriately.
static void setHelperAttributesForAggResult(llvm::CallInst *call,
                                            bool isFormalResult = true) {
  // Set as nounwind.
  auto attrs = llvm::AttributeSet::get(call->getContext(),
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  attrs = attrs.addAttribute(call->getContext(), 1, llvm::Attribute::NoAlias);

  // Only set 'sret' if this is also the formal result.
  if (isFormalResult) {
    attrs = attrs.addAttribute(call->getContext(), 1,
                               llvm::Attribute::StructRet);
  }

  call->setAttributes(attrs);
}

/// Given a call to a helper function, set attributes appropriately.
static void setHelperAttributes(llvm::CallInst *call) {
  // Set as nounwind.
  auto attrs = llvm::AttributeSet::get(call->getContext(),
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);

  call->setAttributes(attrs);
}

/// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
llvm::Value *irgen::emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                     llvm::Value *metadata,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

llvm::Value *irgen::emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                     SILType T,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                ValueWitness::InitializeBufferWithTakeOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
  return call;
}

/// Emit a call to do an 'initializeBufferWithTakeOfBuffer' operation.
llvm::Value *irgen::emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                     llvm::Value *metadata,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                             ValueWitness::InitializeBufferWithTakeOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

llvm::Value *irgen::emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                     SILType T,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
  return call;
}

/// Emit a dynamic alloca call to allocate enough memory to hold an object of
/// type 'T' and an optional llvm.stackrestore point if 'isInEntryBlock' is
/// false.
DynamicAlloca irgen::emitDynamicAlloca(IRGenFunction &IGF, SILType T,
                                       bool isInEntryBlock) {
  llvm::Value *stackRestorePoint = nullptr;

  // Save the stack pointer if we are not in the entry block (we could be
  // executed more than once).
  if (!isInEntryBlock) {
    auto *stackSaveFn = llvm::Intrinsic::getDeclaration(
        &IGF.IGM.Module, llvm::Intrinsic::ID::stacksave);

    stackRestorePoint =  IGF.Builder.CreateCall(stackSaveFn, {}, "spsave");
  }

  // Emit the dynamic alloca.
  llvm::Value *size = emitLoadOfSize(IGF, T);
  auto *alloca = IGF.Builder.CreateAlloca(IGF.IGM.Int8Ty, size, "alloca");
  alloca->setAlignment(16);
  assert(!isInEntryBlock ||
         IGF.getActiveDominancePoint().isUniversal() &&
             "Must be in entry block if we insert dynamic alloca's without "
             "stackrestores");
  return {alloca, stackRestorePoint};
}

/// Deallocate dynamic alloca's memory if requested by restoring the stack
/// location before the dynamic alloca's call.
void irgen::emitDeallocateDynamicAlloca(IRGenFunction &IGF,
                                        StackAddress address) {
  if (!address.needsSPRestore())
    return;
  auto *stackRestoreFn = llvm::Intrinsic::getDeclaration(
      &IGF.IGM.Module, llvm::Intrinsic::ID::stackrestore);
  IGF.Builder.CreateCall(stackRestoreFn, address.getSavedSP());
}

/// Emit a call to do an 'allocateBuffer' operation.
llvm::Value *irgen::emitAllocateBufferCall(IRGenFunction &IGF,
                                           SILType T,
                                           Address buffer) {
  llvm::Value *metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *allocateFn
    = IGF.emitValueWitnessForLayout(T, ValueWitness::AllocateBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall(allocateFn, {buffer.getAddress(), metadata});
  result->setCallingConv(IGF.IGM.DefaultCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do a 'projectBuffer' operation.
llvm::Value *irgen::emitProjectBufferCall(IRGenFunction &IGF,
                                          SILType T,
                                          Address buffer) {
  llvm::Value *metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn
    = IGF.emitValueWitnessForLayout(T, ValueWitness::ProjectBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall(fn, {buffer.getAddress(), metadata});
  result->setCallingConv(IGF.IGM.DefaultCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do a 'projectBuffer' operation.
llvm::Value *irgen::emitProjectBufferCall(IRGenFunction &IGF,
                                          llvm::Value *metadata,
                                          Address buffer) {
  llvm::Value *projectFn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                            ValueWitness::ProjectBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall(projectFn, {buffer.getAddress(), metadata});
  result->setCallingConv(IGF.IGM.DefaultCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do an 'initializeWithCopy' operation.
void irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address destObject,
                                       Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::InitializeWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

llvm::Value *irgen::emitInitializeBufferWithTakeCall(IRGenFunction &IGF,
                                                     SILType T,
                                                     Address destObject,
                                                     Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                       ValueWitness::InitializeBufferWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
  return call;
}

llvm::Value *irgen::emitInitializeBufferWithCopyCall(IRGenFunction &IGF,
                                                     SILType T,
                                                     Address destObject,
                                                     Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                       ValueWitness::InitializeBufferWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
  return call;
}

/// Emit a call to do an 'initializeArrayWithCopy' operation.
void irgen::emitInitializeArrayWithCopyCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::InitializeArrayWithCopy);

  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), count, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeWithTake' operation.
void irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address destObject,
                                       Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                            ValueWitness::InitializeWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeArrayWithTakeFrontToBack' operation.
void irgen::emitInitializeArrayWithTakeFrontToBackCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                             ValueWitness::InitializeArrayWithTakeFrontToBack);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), count, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeArrayWithTakeBackToFront' operation.
void irgen::emitInitializeArrayWithTakeBackToFrontCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                             ValueWitness::InitializeArrayWithTakeBackToFront);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), count, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithCopy' operation.
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *metadata,
                                   Address destObject,
                                   Address srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   SILType T,
                                   Address destObject,
                                   Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithTake' operation.
void irgen::emitAssignWithTakeCall(IRGenFunction &IGF,
                                   SILType T,
                                   Address destObject,
                                   Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::AssignWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}

/// Emit a call to do a 'destroy' operation.
void irgen::emitDestroyCall(IRGenFunction &IGF,
                            SILType T,
                            Address object) {
  // If T is a trivial/POD type, nothing needs to be done.
  if (T.getObjectType().isTrivial(IGF.getSILModule()))
    return;
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::Destroy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {object.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyArray' operation.
void irgen::emitDestroyArrayCall(IRGenFunction &IGF,
                                 SILType T,
                                 Address object,
                                 llvm::Value *count) {
  // If T is a trivial/POD type, nothing needs to be done.
  if (T.getObjectType().isTrivial(IGF.getSILModule()))
    return;
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::DestroyArray);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {object.getAddress(), count, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyBuffer' operation.
void irgen::emitDestroyBufferCall(IRGenFunction &IGF,
                                  SILType T,
                                  Address buffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {buffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}
void irgen::emitDestroyBufferCall(IRGenFunction &IGF,
                                  llvm::Value *metadata,
                                  Address buffer) {
  auto fn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                   ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {buffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'deallocateBuffer' operation.
void irgen::emitDeallocateBufferCall(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     Address buffer) {
  auto fn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                   ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {buffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}
void irgen::emitDeallocateBufferCall(IRGenFunction &IGF,
                                     SILType T,
                                     Address buffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {buffer.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Emit a call to the 'getExtraInhabitantIndex' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitGetExtraInhabitantIndexCall(IRGenFunction &IGF,
                                                    SILType T,
                                                    Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::GetExtraInhabitantIndex);
  
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
  return call;
}

/// Emit a call to the 'storeExtraInhabitant' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitStoreExtraInhabitantCall(IRGenFunction &IGF,
                                                 SILType T,
                                                 llvm::Value *index,
                                                 Address destObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                       ValueWitness::StoreExtraInhabitant);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {destObject.getAddress(), index, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
  return call;
}

/// Emit a call to the 'getEnumTag' operation.
llvm::Value *irgen::emitGetEnumTagCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                       ValueWitness::GetEnumTag);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
  return call;
}

/// Emit a call to the 'destructiveProjectEnumData' operation.
/// The type must be dynamically known to have enum witnesses.
void irgen::emitDestructiveProjectEnumDataCall(IRGenFunction &IGF,
                                               SILType T,
                                               Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                      ValueWitness::DestructiveProjectEnumData);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Emit a call to the 'destructiveInjectEnumTag' operation.
/// The type must be dynamically known to have enum witnesses.
void irgen::emitDestructiveInjectEnumTagCall(IRGenFunction &IGF,
                                             SILType T,
                                             unsigned tag,
                                             Address srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                      ValueWitness::DestructiveInjectEnumTag);
  llvm::Value *tagValue =
    llvm::ConstantInt::get(IGF.IGM.Int32Ty, tag);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), tagValue, metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

/// Load the 'size' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfSize(IRGenFunction &IGF, SILType T) {
  return IGF.emitValueWitnessForLayout(T, ValueWitness::Size);
}

/// Load the 'alignmentMask' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfAlignmentMask(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessForLayout(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::AlignmentMask));
  return IGF.Builder.CreateAnd(flags, mask,
                               flags->getName() + ".alignmentMask");
}

/// Load the 'isPOD' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsPOD(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessForLayout(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonPOD));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isPOD");
}

/// Load the 'isBitwiseTakable' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsBitwiseTakable(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessForLayout(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonBitwiseTakable));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isBitwiseTakable");
}

/// Load the 'isInline' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsInline(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessForLayout(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonInline));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isInline");
}

/// Load the 'hasExtraInhabitants' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfHasExtraInhabitants(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessForLayout(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::Enum_HasExtraInhabitants));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpNE(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".hasExtraInhabitants");
}

/// Load the 'stride' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfStride(IRGenFunction &IGF, SILType T) {
  return IGF.emitValueWitnessForLayout(T, ValueWitness::Stride);
}

llvm::Value *irgen::emitLoadOfExtraInhabitantCount(IRGenFunction &IGF,
                                                   SILType T) {
  auto xiFlags = IGF.emitValueWitnessForLayout(T,
                                           ValueWitness::ExtraInhabitantFlags);
  auto mask = IGF.IGM.getSize(
                          Size(ExtraInhabitantFlags::NumExtraInhabitantsMask));
  return IGF.Builder.CreateAnd(xiFlags, mask,
                               xiFlags->getName() + ".extraInhabitantCount");
}

std::pair<llvm::Value *, llvm::Value *>
irgen::emitLoadOfIsInline(IRGenFunction &IGF, llvm::Value *metadata) {
  auto *flags =
      emitLoadOfValueWitnessFromMetadata(IGF, metadata, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonInline));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return std::make_pair(
      IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                               flags->getName() + ".isInline"),
      flags);
}

llvm::Value *irgen::emitLoadOfSize(IRGenFunction &IGF, llvm::Value *metadata) {
  auto *size =
      emitLoadOfValueWitnessFromMetadata(IGF, metadata, ValueWitness::Size);
  return size;
}

llvm::Value *irgen::emitAlignMaskFromFlags(IRGenFunction &IGF,
                                           llvm::Value *flags) {
  auto *alignMask = IGF.IGM.getSize(Size(ValueWitnessFlags::AlignmentMask));
  return IGF.Builder.CreateAnd(flags, alignMask,
                               flags->getName() + ".alignmentMask");
}

llvm::Value *irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                               llvm::Value *metadata,
                                               Address dest, Address src) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(
      IGF, metadata, ValueWitness::InitializeWithCopy);
  llvm::CallInst *call = IGF.Builder.CreateCall(
      copyFn, {dest.getAddress(), src.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();

  return call;
}

llvm::Value *irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                               llvm::Value *metadata,
                                               Address dest, Address src) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(
      IGF, metadata, ValueWitness::InitializeWithTake);
  llvm::CallInst *call = IGF.Builder.CreateCall(
      copyFn, {dest.getAddress(), src.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();

  return call;
}

void irgen::emitDestroyCall(IRGenFunction &IGF, llvm::Value *metadata,
                            Address object) {
  llvm::Value *fn =
      emitLoadOfValueWitnessFromMetadata(IGF, metadata, ValueWitness::Destroy);
  llvm::CallInst *call =
      IGF.Builder.CreateCall(fn, {object.getAddress(), metadata});
  call->setCallingConv(IGF.IGM.DefaultCC);
  setHelperAttributes(call);
}

static llvm::Constant *getAllocateValueBufferFunction(IRGenModule &IGM) {

  llvm::Type *argTys[] = {IGM.TypeMetadataPtrTy, IGM.OpaquePtrTy};

  llvm::SmallString<40> fnName("__swift_allocate_value_buffer");

  return IGM.getOrCreateHelperFunction(
      fnName, IGM.OpaquePtrTy, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        auto *metadata = &*(it++);
        auto buffer = Address(&*(it++), Alignment(1));

        // Dynamically check whether this type is inline or needs an allocation.
        llvm::Value *isInline, *flags;
        std::tie(isInline, flags) = emitLoadOfIsInline(IGF, metadata);

        auto *outlineBB = IGF.createBasicBlock("outline.allocateValueInBuffer");
        auto *doneBB = IGF.createBasicBlock("done");
        llvm::Value *addressInline, *addressOutline;
        addressInline = buffer.getAddress();
        auto *origBB = IGF.Builder.GetInsertBlock();
        IGF.Builder.CreateCondBr(isInline, doneBB, outlineBB);

        IGF.Builder.emitBlock(outlineBB);
        {
          auto *size = emitLoadOfSize(IGF, metadata);
          auto *alignMask = emitAlignMaskFromFlags(IGF, flags);
          auto valueAddr =
              IGF.emitAllocRawCall(size, alignMask, "outline.ValueBuffer");
          IGF.Builder.CreateStore(
              valueAddr, Address(IGF.Builder.CreateBitCast(
                                     buffer.getAddress(),
                                     valueAddr->getType()->getPointerTo()),
                                 Alignment(1)));
          addressOutline =
              IGF.Builder.CreateBitCast(valueAddr, IGM.OpaquePtrTy);
          IGF.Builder.CreateBr(doneBB);
        }

        IGF.Builder.emitBlock(doneBB);
        auto *addressOfValue = IGF.Builder.CreatePHI(IGM.OpaquePtrTy, 2);
        addressOfValue->addIncoming(addressInline, origBB);
        addressOfValue->addIncoming(addressOutline, outlineBB);
        IGF.Builder.CreateRet(addressOfValue);
      },
      true /*noinline*/);
}

Address irgen::emitAllocateValueInBuffer(IRGenFunction &IGF, SILType type,
                                         Address buffer) {
  // Handle FixedSize types.
  auto &IGM = IGF.IGM;
  auto storagePtrTy = IGM.getStoragePointerType(type);
  auto &Builder = IGF.Builder;
  if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&IGF.getTypeInfo(type))) {
    auto packing = fixedTI->getFixedPacking(IGM);

    // Inline representation.
    if (packing == FixedPacking::OffsetZero) {
      return Address(Builder.CreateBitCast(buffer.getAddress(), storagePtrTy),
                     buffer.getAlignment());
    }

    // Outline representation.
    assert(packing == FixedPacking::Allocate && "Expect non dynamic packing");
    auto size = fixedTI->getStaticSize(IGM);
    auto alignMask = fixedTI->getStaticAlignmentMask(IGM);
    auto valueAddr =
        IGF.emitAllocRawCall(size, alignMask, "outline.ValueBuffer");
    Builder.CreateStore(
        valueAddr,
        Address(Builder.CreateBitCast(buffer.getAddress(),
                                      valueAddr->getType()->getPointerTo()),
                buffer.getAlignment()));
    return Address(Builder.CreateBitCast(valueAddr, storagePtrTy),
                   buffer.getAlignment());
  }

  // Dynamic packing.

  /// Call a function to handle the non-fixed case.
  auto *allocateFun = getAllocateValueBufferFunction(IGF.IGM);
  auto *metadata = IGF.emitTypeMetadataRefForLayout(type);
  auto *call = Builder.CreateCall(
      allocateFun,
      {metadata, Builder.CreateBitCast(buffer.getAddress(), IGM.OpaquePtrTy)});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();

  auto addressOfValue = Builder.CreateBitCast(call, storagePtrTy);
  return Address(addressOfValue, Alignment(1));
}

static llvm::Constant *getProjectValueInBufferFunction(IRGenModule &IGM) {

  llvm::Type *argTys[] = {IGM.TypeMetadataPtrTy, IGM.OpaquePtrTy};

  llvm::SmallString<40> fnName("__swift_project_value_buffer");

  return IGM.getOrCreateHelperFunction(
      fnName, IGM.OpaquePtrTy, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        auto *metadata = &*(it++);
        auto buffer = Address(&*(it++), Alignment(1));
        auto &Builder = IGF.Builder;

        // Dynamically check whether this type is inline or needs an allocation.
        llvm::Value *isInline, *flags;
        std::tie(isInline, flags) = emitLoadOfIsInline(IGF, metadata);

        auto *outlineBB = IGF.createBasicBlock("outline.projectValueInBuffer");
        auto *doneBB = IGF.createBasicBlock("done");
        llvm::Value *addressInline, *addressOutline;
        auto *origBB = Builder.GetInsertBlock();
        addressInline = buffer.getAddress();

        Builder.CreateCondBr(isInline, doneBB, outlineBB);

        Builder.emitBlock(outlineBB);
        {
          addressOutline = Builder.CreateLoad(
              Address(Builder.CreateBitCast(buffer.getAddress(),
                                            IGM.OpaquePtrTy->getPointerTo()),
                      Alignment(1)));
          Builder.CreateBr(doneBB);
        }

        Builder.emitBlock(doneBB);
        auto *addressOfValue = Builder.CreatePHI(IGM.OpaquePtrTy, 2);
        addressOfValue->addIncoming(addressInline, origBB);
        addressOfValue->addIncoming(addressOutline, outlineBB);

        Builder.CreateRet(addressOfValue);
      },
      true /*noinline*/);
}

Address irgen::emitProjectValueInBuffer(IRGenFunction &IGF, SILType type,
                                        Address buffer) {
  // Handle FixedSize types.
  auto &IGM = IGF.IGM;
  auto storagePtrTy = IGM.getStoragePointerType(type);
  auto &Builder = IGF.Builder;
  if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&IGF.getTypeInfo(type))) {
    auto packing = fixedTI->getFixedPacking(IGM);

    // Inline representation.
    if (packing == FixedPacking::OffsetZero) {
      return Address(Builder.CreateBitCast(buffer.getAddress(), storagePtrTy),
                     buffer.getAlignment());
    }

    // Outline representation.
    assert(packing == FixedPacking::Allocate && "Expect non dynamic packing");
    auto valueAddr = Builder.CreateLoad(
        Address(Builder.CreateBitCast(buffer.getAddress(),
                                      storagePtrTy->getPointerTo()),
                buffer.getAlignment()));
    return Address(Builder.CreateBitCast(valueAddr, storagePtrTy),
                   buffer.getAlignment());
  }

  // Dynamic packing.
  auto *projectFun = getProjectValueInBufferFunction(IGF.IGM);
  auto *metadata = IGF.emitTypeMetadataRefForLayout(type);
  auto *call = Builder.CreateCall(
      projectFun,
      {metadata, Builder.CreateBitCast(buffer.getAddress(), IGM.OpaquePtrTy)});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();

  auto addressOfValue = Builder.CreateBitCast(call, storagePtrTy);
  return Address(addressOfValue, Alignment(1));
}

static llvm::Constant *getDeallocateValueInBufferFunction(IRGenModule &IGM) {

  llvm::Type *argTys[] = {IGM.TypeMetadataPtrTy, IGM.OpaquePtrTy};

  llvm::SmallString<40> fnName("__swift_deallocate_value_buffer");

  return IGM.getOrCreateHelperFunction(
      fnName, IGM.VoidTy, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        auto *metadata = &*(it++);
        auto buffer = Address(&*(it++), Alignment(1));
        auto &Builder = IGF.Builder;

        // Dynamically check whether this type is inline or needs an allocation.
        llvm::Value *isInline, *flags;
        std::tie(isInline, flags) = emitLoadOfIsInline(IGF, metadata);
        auto *outlineBB = IGF.createBasicBlock("outline.deallocateValueInBuffer");
        auto *doneBB = IGF.createBasicBlock("done");

        Builder.CreateCondBr(isInline, doneBB, outlineBB);

        Builder.emitBlock(outlineBB);
        {
          auto *size = emitLoadOfSize(IGF, metadata);
          auto *alignMask = emitAlignMaskFromFlags(IGF, flags);
          auto *ptr = Builder.CreateLoad(Address(
              Builder.CreateBitCast(buffer.getAddress(), IGM.Int8PtrPtrTy),
              buffer.getAlignment()));
          IGF.emitDeallocRawCall(ptr, size, alignMask);
          Builder.CreateBr(doneBB);
        }

        Builder.emitBlock(doneBB);
        Builder.CreateRetVoid();
      },
      true /*noinline*/);
}

void irgen::emitDeallocateValueInBuffer(IRGenFunction &IGF,
                                 SILType type,
                                 Address buffer) {
  // Handle FixedSize types.
  auto &IGM = IGF.IGM;
  auto &Builder = IGF.Builder;
  if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&IGF.getTypeInfo(type))) {
    auto packing = fixedTI->getFixedPacking(IGM);

    // Inline representation.
    if (packing == FixedPacking::OffsetZero)
      return;

    // Outline representation.
    assert(packing == FixedPacking::Allocate && "Expect non dynamic packing");
    auto size = fixedTI->getStaticSize(IGM);
    auto alignMask = fixedTI->getStaticAlignmentMask(IGM);
    auto *ptr = Builder.CreateLoad(Address(
        Builder.CreateBitCast(buffer.getAddress(), IGM.Int8PtrPtrTy),
        buffer.getAlignment()));
    IGF.emitDeallocRawCall(ptr, size, alignMask);
    return;
  }

  // Dynamic packing.
  auto *projectFun = getDeallocateValueInBufferFunction(IGF.IGM);
  auto *metadata = IGF.emitTypeMetadataRefForLayout(type);
  auto *call = Builder.CreateCall(
      projectFun,
      {metadata, Builder.CreateBitCast(buffer.getAddress(), IGM.OpaquePtrTy)});
  call->setCallingConv(IGF.IGM.DefaultCC);
  call->setDoesNotThrow();
}
