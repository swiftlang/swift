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

static llvm::FunctionType *createWitnessFunctionType(IRGenModule &IGM,
                                                     ValueWitness index) {
  switch (index) {

  // void (*deallocateBuffer)(B *buffer, M *self);
  // void (*destroyBuffer)(B *buffer, M *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, M *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*allocateBuffer)(B *buffer, M *self);
  // T *(*projectBuffer)(B *buffer, M *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, M *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, M *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, false);
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
    return llvm::FunctionType::get(ptrTy, args, false);
  }

  // M *(*typeof)(T *src, M *self);
  case ValueWitness::TypeOf: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *args[] = { ptrTy, metaTy };
    return llvm::FunctionType::get(metaTy, args, false);
  }
      
  case ValueWitness::Size:
  case ValueWitness::Alignment:
  case ValueWitness::Stride:
    llvm_unreachable("these witnesses aren't value witnesses!");
  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::Type *IRGenModule::getValueWitnessTy(ValueWitness index) {
  static_assert(IRGenModule::NumValueWitnessFunctions
                  == ::NumValueWitnessFunctions,
                "array on IGM has the wrong size");

  /// All the non-function values are size_t's.
  if (!isValueWitnessFunction(index))
    return SizeTy;

  assert(unsigned(index) < NumValueWitnessFunctions);
  auto &ty = ValueWitnessTys[unsigned(index)];
  if (ty) return ty;

  ty = createWitnessFunctionType(*this, index)->getPointerTo(0);
  return ty;
}

static llvm::StringRef getValueWitnessLabel(ValueWitness index) {
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
  case ValueWitness::TypeOf:
    return "typeof";
  case ValueWitness::Size:
    return "size";
  case ValueWitness::Alignment:
    return "alignment";
  case ValueWitness::Stride:
    return "stride";
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

/// Given a witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
llvm::Value *irgen::emitLoadOfValueWitness(IRGenFunction &IGF,
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
                                                     llvm::Value *witnessTable,
                                                     llvm::Value *metadata,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  llvm::Value *copyFn = emitLoadOfValueWitness(IGF, witnessTable,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributesForAggResult(call, false);

  return call;
}

/// Emit a call to do an 'allocateBuffer' operation.
llvm::Value *irgen::emitAllocateBufferCall(IRGenFunction &IGF,
                                           llvm::Value *witnessTable,
                                           llvm::Value *metadata,
                                           Address buffer) {
  llvm::Value *allocateFn = emitLoadOfValueWitness(IGF, witnessTable,
                                             ValueWitness::AllocateBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(allocateFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do a 'projectBuffer' operation.
llvm::Value *irgen::emitProjectBufferCall(IRGenFunction &IGF,
                                          llvm::Value *witnessTable,
                                           llvm::Value *metadata,
                                          Address buffer) {
  llvm::Value *projectFn = emitLoadOfValueWitness(IGF, witnessTable,
                                            ValueWitness::ProjectBuffer);
  llvm::CallInst *result =
    IGF.Builder.CreateCall2(projectFn, buffer.getAddress(), metadata);
  result->setCallingConv(IGF.IGM.RuntimeCC);
  result->setDoesNotThrow();
  return result;
}

/// Emit a call to do an 'initializeWithCopy' operation.
void irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *metadata,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'initializeWithTake' operation.
void irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       llvm::Value *witnessTable,
                                       llvm::Value *metadata,
                                       llvm::Value *destObject,
                                       llvm::Value *srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitness(IGF, witnessTable,
                                         ValueWitness::InitializeWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithCopy' operation.
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithCopy);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do an 'assignWithTake' operation.
void irgen::emitAssignWithTakeCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                   llvm::Value *destObject,
                                   llvm::Value *srcObject) {
  llvm::Value *copyFn = emitLoadOfValueWitness(IGF, witnessTable,
                                         ValueWitness::AssignWithTake);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destObject, srcObject, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a call to do a 'destroy' operation.
void irgen::emitDestroyCall(IRGenFunction &IGF,
                            llvm::Value *witnessTable,
                            llvm::Value *metadata,
                            llvm::Value *object) {
  auto fn = emitLoadOfValueWitness(IGF, witnessTable, ValueWitness::Destroy);
  llvm::CallInst *call = IGF.Builder.CreateCall2(fn, object, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to do a 'destroyBuffer' operation.
void irgen::emitDestroyBufferCall(IRGenFunction &IGF,
                                  llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                  Address buffer) {
  auto fn = emitLoadOfValueWitness(IGF, witnessTable,
                                   ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
}

/// Emit a call to the 'typeof' operation.
llvm::Value *irgen::emitTypeofCall(IRGenFunction &IGF,
                                   llvm::Value *witnessTable,
                                   llvm::Value *metadata,
                                   llvm::Value *object) {
  auto fn = emitLoadOfValueWitness(IGF, witnessTable,
                                   ValueWitness::TypeOf);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, object, metadata);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  setHelperAttributes(call);
  return call;
}

/// Emit a load of the 'size' value witness.
llvm::Value *irgen::emitLoadOfSize(IRGenFunction &IGF, llvm::Value *wtable) {
  return emitLoadOfValueWitness(IGF, wtable, ValueWitness::Size);
}

/// Emit a load of the 'alignment' value witness.
llvm::Value *irgen::emitLoadOfAlignment(IRGenFunction &IGF,
                                        llvm::Value *wtable) {
  return emitLoadOfValueWitness(IGF, wtable, ValueWitness::Alignment);
}

/// Emit a load of the 'stride' value witness.
llvm::Value *irgen::emitLoadOfStride(IRGenFunction &IGF, llvm::Value *wtable) {
  return emitLoadOfValueWitness(IGF, wtable, ValueWitness::Stride);
}
