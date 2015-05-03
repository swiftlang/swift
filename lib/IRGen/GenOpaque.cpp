//===--- GenOpaque.cpp - Swift IR-generation for opaque values ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ProtocolInfo.h"
#include "ValueWitness.h"

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
  
  /// unsigned (*getEnumTag)(T *obj, M *self);
  case ValueWitness::GetEnumTag: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    
    llvm::Type *args[] = {ptrTy, metaTy};
    
    return llvm::FunctionType::get(indexTy, args, /*isVarArg*/ false)
      ->getPointerTo();
  }
    
  /// U *(*inplaceProjectEnumData)(T *obj, unsigned tag, M *self);
  case ValueWitness::InplaceProjectEnumData: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    
    llvm::Type *args[] = {ptrTy, indexTy, metaTy};
    
    return llvm::FunctionType::get(ptrTy, args, /*isVarArg*/ false)
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
  case ValueWitness::InplaceProjectEnumData:
    return "inplaceProjectEnumData";
  }
  llvm_unreachable("bad value witness index");
}

/// Load a specific witness from a known table.  The result is
/// always an i8*.
llvm::Value *irgen::emitLoadOfOpaqueWitness(IRGenFunction &IGF,
                                            llvm::Value *table,
                                            WitnessIndex index) {
  assert(table->getType() == IGF.IGM.WitnessTablePtrTy);

  // GEP to the appropriate index, avoiding spurious IR in the trivial case.
  llvm::Value *slot = table;
  if (index.getValue() != 0)
    slot = IGF.Builder.CreateConstInBoundsGEP1_32(table, index.getValue());

  llvm::Value *witness =
    IGF.Builder.CreateLoad(Address(slot, IGF.IGM.getPointerAlignment()));
  return witness;
}

/// Given a value witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
static llvm::Value *emitLoadOfValueWitness(IRGenFunction &IGF,
                                           llvm::Value *table,
                                           ValueWitness index) {
  llvm::Value *witness = emitLoadOfOpaqueWitness(IGF, table, index);
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
        tryGetLocalTypeData(type, LocalTypeData::forValueWitness(index)))
    return witness;
  
  auto vwtable = emitValueWitnessTableRef(type);
  auto witness = emitLoadOfValueWitness(*this, vwtable, index);
  setScopedLocalTypeData(type, LocalTypeData::forValueWitness(index), witness);
  return witness;
}

llvm::Value *IRGenFunction::emitValueWitnessForLayout(SILType type,
                                                      ValueWitness index) {
  if (auto witness = tryGetLocalTypeDataForLayout(type,
                                         LocalTypeData::forValueWitness(index)))
    return witness;
  
  auto vwtable = emitValueWitnessTableRefForLayout(type);
  auto witness = emitLoadOfValueWitness(*this, vwtable, index);
  setScopedLocalTypeDataForLayout(type,
                                LocalTypeData::forValueWitness(index), witness);
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
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributesForAggResult(call, false);

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
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

/// Emit a call to do an 'allocateBuffer' operation.
llvm::Value *irgen::emitAllocateBufferCall(IRGenFunction &IGF,
                                           SILType T,
                                           Address buffer) {
  llvm::Value *metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *allocateFn
    = IGF.emitValueWitnessForLayout(T, ValueWitness::AllocateBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(allocateFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
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
    IGF.Builder.CreateCall2(projectFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do an 'initializeWithCopy' operation.
void irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       SILType T,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::InitializeWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeArrayWithCopy' operation.
void irgen::emitInitializeArrayWithCopyCall(IRGenFunction &IGF,
                                            SILType T,
                                            llvm::Value *destObject,
                                            llvm::Value *srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::InitializeArrayWithCopy);

  llvm::CallInst *call =
    IGF.Builder.CreateCall4(copyFn, destObject, srcObject, count, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeWithTake' operation.
void irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       SILType T,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                            ValueWitness::InitializeWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeArrayWithTakeFrontToBack' operation.
void irgen::emitInitializeArrayWithTakeFrontToBackCall(IRGenFunction &IGF,
                                            SILType T,
                                            llvm::Value *destObject,
                                            llvm::Value *srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                             ValueWitness::InitializeArrayWithTakeFrontToBack);
  llvm::CallInst *call =
    IGF.Builder.CreateCall4(copyFn, destObject, srcObject, count, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeArrayWithTakeBackToFront' operation.
void irgen::emitInitializeArrayWithTakeBackToFrontCall(IRGenFunction &IGF,
                                            SILType T,
                                            llvm::Value *destObject,
                                            llvm::Value *srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                             ValueWitness::InitializeArrayWithTakeBackToFront);
  llvm::CallInst *call =
    IGF.Builder.CreateCall4(copyFn, destObject, srcObject, count, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithCopy' operation.
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *metadata,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   SILType T,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithTake' operation.
void irgen::emitAssignWithTakeCall(IRGenFunction &IGF,
                                   SILType T,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *copyFn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::AssignWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do a 'destroy' operation.
void irgen::emitDestroyCall(IRGenFunction &IGF,
                            SILType T,
                            llvm::Value *object) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::Destroy);
  llvm::CallInst *call = IGF.Builder.CreateCall2(fn, object, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyArray' operation.
void irgen::emitDestroyArrayCall(IRGenFunction &IGF,
                                 SILType T,
                                 llvm::Value *object,
                                 llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::DestroyArray);
  llvm::CallInst *call = IGF.Builder.CreateCall3(fn, object, count, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyBuffer' operation.
void irgen::emitDestroyBufferCall(IRGenFunction &IGF,
                                  llvm::Value *metadata,
                                  Address buffer) {
  auto fn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                   ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'deallocateBuffer' operation.
void irgen::emitDeallocateBufferCall(IRGenFunction &IGF,
                                     llvm::Value *metadata,
                                     Address buffer) {
  auto fn = emitLoadOfValueWitnessFromMetadata(IGF, metadata,
                                   ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}
void irgen::emitDeallocateBufferCall(IRGenFunction &IGF,
                                     SILType T,
                                     Address buffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                   ValueWitness::DeallocateBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to the 'getExtraInhabitantIndex' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitGetExtraInhabitantIndexCall(IRGenFunction &IGF,
                                                    SILType T,
                                                    llvm::Value *srcObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                         ValueWitness::GetExtraInhabitantIndex);
  
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
  return call;
}

/// Emit a call to the 'storeExtraInhabitant' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitStoreExtraInhabitantCall(IRGenFunction &IGF,
                                                 SILType T,
                                                 llvm::Value *index,
                                                 llvm::Value *destObject) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  llvm::Value *fn = IGF.emitValueWitnessForLayout(T,
                                       ValueWitness::StoreExtraInhabitant);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(fn, destObject, index, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
  return call;
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
