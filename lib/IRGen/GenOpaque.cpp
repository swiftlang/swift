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
#include "swift/AST/IRGenOptions.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/IRGen/ValueWitness.h"

#include "Callee.h"
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
  return NumWords_ValueBuffer * IGM.getPointerSize();
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
  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.OpaquePtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.VoidTy, args, /*isVarArg*/ false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, M *self);
  // T *(*initializeBufferWithTakeOfBuffer)(B *dest, B *src, M *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithTakeOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, IGM.TypeMetadataPtrTy };
    return llvm::FunctionType::get(IGM.OpaquePtrTy, args, /*isVarArg*/ false);
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
    return llvm::FunctionType::get(ptrTy, args, /*isVarArg*/ false);
  }
      
  /// void (*storeExtraInhabitant)(T *obj, unsigned index, M *self);
  case ValueWitness::StoreExtraInhabitant: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *args[] = {ptrTy, indexTy, metaTy};
    
    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false);
  }
      
  /// int (*getExtraInhabitantIndex)(T *obj, M *self);
  case ValueWitness::GetExtraInhabitantIndex: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    
    llvm::Type *args[] = {ptrTy, metaTy};
    
    return llvm::FunctionType::get(indexTy, args, /*isVarArg*/ false);
  }
  
  /// unsigned (*getEnumTag)(T *obj, M *self);
  case ValueWitness::GetEnumTag: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;

    llvm::Type *args[] = {ptrTy, metaTy};

    return llvm::FunctionType::get(indexTy, args, /*isVarArg*/ false);
  }

  /// void (*destructiveProjectEnumData)(T *obj, M *self);
  case ValueWitness::DestructiveProjectEnumData: {
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = {ptrTy, metaTy};

    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false);
  }

  /// void (*destructiveInjectEnumTag)(T *obj, unsigned tag, M *self);
  case ValueWitness::DestructiveInjectEnumTag: {
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = {ptrTy, indexTy, metaTy};

    return llvm::FunctionType::get(voidTy, args, /*isVarArg*/ false);
  }

  /// unsigned (*getEnumTagSinglePayload)(const T* enum, UINT_TYPE emptyCases,
  ///                                     M *self)
  case ValueWitness::GetEnumTagSinglePayload: {
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = { ptrTy, indexTy, metaTy };
    return llvm::FunctionType::get(indexTy, args, false);
  }

  /// void (*storeEnumTagSinglePayload)(T* enum, UINT_TYPE whichCase,
  ///                                   UINT_TYPE emptyCases,
  ///                                   M *self)
  case ValueWitness::StoreEnumTagSinglePayload: {
    llvm::Type *voidTy = IGM.VoidTy;
    llvm::Type *ptrTy = IGM.OpaquePtrTy;
    llvm::Type *indexTy = IGM.Int32Ty;
    llvm::Type *metaTy = IGM.TypeMetadataPtrTy;

    llvm::Type *args[] = { ptrTy, indexTy, indexTy, metaTy };
    return llvm::FunctionType::get(voidTy, args, false);
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

static llvm::AttributeList getValueWitnessAttrs(IRGenModule &IGM,
                                                ValueWitness index) {
  assert(isValueWitnessFunction(index));

  auto &ctx = IGM.getLLVMContext();

  // All value witnesses are nounwind.
  auto attrs = llvm::AttributeList::get(ctx, llvm::AttributeList::FunctionIndex,
                                        llvm::Attribute::NoUnwind);

  switch (index) {
  // These have two arguments, but they can alias.
  case ValueWitness::AssignWithCopy:
    return attrs;

  // These have one argument.
  case ValueWitness::Destroy:
  case ValueWitness::DestructiveInjectEnumTag:
  case ValueWitness::DestructiveProjectEnumData:
  case ValueWitness::GetEnumTag:
  case ValueWitness::GetExtraInhabitantIndex:
  case ValueWitness::StoreExtraInhabitant:
  case ValueWitness::StoreEnumTagSinglePayload:
    return attrs.addAttribute(ctx, 1, llvm::Attribute::NoAlias);

  case ValueWitness::GetEnumTagSinglePayload:
    return attrs
        .addAttribute(ctx, llvm::AttributeList::FunctionIndex,
                      llvm::Attribute::ReadOnly)
        .addAttribute(ctx, 1, llvm::Attribute::NoAlias);

  // These have two arguments and they don't alias each other.
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithTakeOfBuffer:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake:
    return attrs.addAttribute(ctx, 1, llvm::Attribute::NoAlias)
                .addAttribute(ctx, 2, llvm::Attribute::NoAlias);

  case ValueWitness::Size:
  case ValueWitness::Flags:
  case ValueWitness::Stride:
  case ValueWitness::ExtraInhabitantFlags:
    llvm_unreachable("not a function value witness");
  }
  llvm_unreachable("bad witness");
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

Signature IRGenModule::getValueWitnessSignature(ValueWitness index) {
  assert(isValueWitnessFunction(index));
  auto fnTy = cast<llvm::FunctionType>(getValueWitnessTy(index));
  auto attrs = getValueWitnessAttrs(*this, index);
  return Signature(fnTy, attrs, DefaultCC);
}

static StringRef getValueWitnessLabel(ValueWitness index) {
  switch (index) {
  case ValueWitness::Destroy:
    return "destroy";
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
    return "initializeBufferWithCopyOfBuffer";
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
  case ValueWitness::GetEnumTagSinglePayload:
    return "getEnumTagSinglePayload";
  case ValueWitness::StoreEnumTagSinglePayload:
    return "storeEnumTagSinglePayload";
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
static llvm::Value *emitLoadOfValueWitnessValue(IRGenFunction &IGF,
                                                llvm::Value *table,
                                                ValueWitness index) {
  assert(!isValueWitnessFunction(index));
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, table, index);
  auto label = getValueWitnessLabel(index);
  auto type = IGF.IGM.getValueWitnessTy(index);
  return IGF.Builder.CreatePtrToInt(witness, type, label);
}

/// Given a type metadata pointer, load one of the value witnesses from its
/// value witness table.
static llvm::Value *
emitLoadOfValueWitnessValueFromMetadata(IRGenFunction &IGF,
                                        llvm::Value *metadata,
                                        ValueWitness index) {
  llvm::Value *vwtable = IGF.emitValueWitnessTableRefForMetadata(metadata);
  return emitLoadOfValueWitnessValue(IGF, vwtable, index);
}

/// Given a value witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
static FunctionPointer emitLoadOfValueWitnessFunction(IRGenFunction &IGF,
                                                      llvm::Value *table,
                                                      ValueWitness index) {
  assert(isValueWitnessFunction(index));
  llvm::Value *witness = emitInvariantLoadOfOpaqueWitness(IGF, table, index);
  auto label = getValueWitnessLabel(index);
  auto signature = IGF.IGM.getValueWitnessSignature(index);

  auto type = signature.getType()->getPointerTo();
  witness = IGF.Builder.CreateBitCast(witness, type, label);

  return FunctionPointer(witness, signature);
}

/// Given a type metadata pointer, load one of the function
/// value witnesses from its value witness table.
static FunctionPointer
emitLoadOfValueWitnessFunctionFromMetadata(IRGenFunction &IGF,
                                           llvm::Value *metadata,
                                           ValueWitness index) {
  llvm::Value *vwtable = IGF.emitValueWitnessTableRefForMetadata(metadata);
  return emitLoadOfValueWitnessFunction(IGF, vwtable, index);
}

llvm::Value *IRGenFunction::emitValueWitnessValue(SILType type,
                                                  ValueWitness index) {
  assert(!isValueWitnessFunction(index));

  auto key = LocalTypeDataKind::forValueWitness(index);
  if (auto witness = tryGetLocalTypeDataForLayout(type, key)) {
    return witness;
  }
  
  auto vwtable = emitValueWitnessTableRef(type);
  auto witness = emitLoadOfValueWitnessValue(*this, vwtable, index);
  setScopedLocalTypeDataForLayout(type, key, witness);
  return witness;
}

FunctionPointer
IRGenFunction::emitValueWitnessFunctionRef(SILType type,
                                           llvm::Value *&metadataSlot,
                                           ValueWitness index) {
  assert(isValueWitnessFunction(index));

  auto key = LocalTypeDataKind::forValueWitness(index);
  if (auto witness = tryGetLocalTypeDataForLayout(type, key)) {
    metadataSlot = emitTypeMetadataRefForLayout(type);
    auto signature = IGM.getValueWitnessSignature(index);
    return FunctionPointer(witness, signature);
  }
  
  auto vwtable = emitValueWitnessTableRef(type, &metadataSlot);
  auto witness = emitLoadOfValueWitnessFunction(*this, vwtable, index);
  setScopedLocalTypeDataForLayout(type, key, witness.getPointer());
  return witness;
}

llvm::Value *irgen::emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                     SILType T,
                                                     Address destBuffer,
                                                     Address srcBuffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  return emitInitializeBufferWithCopyOfBufferCall(IGF, metadata,
                                                  destBuffer, srcBuffer);
}

/// Emit a call to do an 'initializeBufferWithCopyOfBuffer' operation.
llvm::Value *
irgen::emitInitializeBufferWithCopyOfBufferCall(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                Address destBuffer,
                                                Address srcBuffer) {
  auto copyFn = emitLoadOfValueWitnessFunctionFromMetadata(IGF, metadata,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});

  return call;
}

llvm::Value *
irgen::emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                SILType T,
                                                Address destBuffer,
                                                Address srcBuffer) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  return emitInitializeBufferWithTakeOfBufferCall(IGF, metadata,
                                                  destBuffer, srcBuffer);
}

/// Emit a call to do an 'initializeBufferWithTakeOfBuffer' operation.
llvm::Value *
irgen::emitInitializeBufferWithTakeOfBufferCall(IRGenFunction &IGF,
                                                llvm::Value *metadata,
                                                Address destBuffer,
                                                Address srcBuffer) {
  auto copyFn = emitLoadOfValueWitnessFunctionFromMetadata(IGF, metadata,
                             ValueWitness::InitializeBufferWithTakeOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(copyFn,
      {destBuffer.getAddress(), srcBuffer.getAddress(), metadata});

  return call;
}

/// Emit a dynamic alloca call to allocate enough memory to hold an object of
/// type 'T' and an optional llvm.stackrestore point if 'isInEntryBlock' is
/// false.
StackAddress IRGenFunction::emitDynamicAlloca(SILType T,
                                              const llvm::Twine &name) {
  llvm::Value *size = emitLoadOfSize(*this, T);
  return emitDynamicAlloca(IGM.Int8Ty, size, Alignment(16), name);
}

StackAddress IRGenFunction::emitDynamicAlloca(llvm::Type *eltTy,
                                              llvm::Value *arraySize,
                                              Alignment align,
                                              const llvm::Twine &name) {
  // In coroutines, call llvm.coro.alloca.alloc.
  if (isCoroutine()) {
    // Compute the number of bytes to allocate.
    llvm::Value *byteCount;
    auto eltSize = IGM.DataLayout.getTypeAllocSize(eltTy);
    if (eltSize == 1) {
      byteCount = arraySize;
    } else {
      byteCount = Builder.CreateMul(arraySize, IGM.getSize(Size(eltSize)));
    }

    auto alignment = llvm::ConstantInt::get(IGM.Int32Ty, align.getValue());

    // Allocate memory.  This produces an abstract token.
    auto allocFn = llvm::Intrinsic::getDeclaration(
        &IGM.Module, llvm::Intrinsic::ID::coro_alloca_alloc, { IGM.SizeTy });
    auto allocToken = Builder.CreateCall(allocFn, { byteCount, alignment });

    // Get the allocation result.
    auto getFn = llvm::Intrinsic::getDeclaration(
        &IGM.Module, llvm::Intrinsic::ID::coro_alloca_get);
    auto ptr = Builder.CreateCall(getFn, { allocToken });

    return {Address(ptr, align), allocToken};
  }

  // Otherwise, use a dynamic alloca.
  llvm::Value *stackRestorePoint = nullptr;

  // Save the stack pointer if we are not in the entry block (we could be
  // executed more than once).
  bool isInEntryBlock = (Builder.GetInsertBlock() == &*CurFn->begin());
  if (!isInEntryBlock) {
    auto *stackSaveFn = llvm::Intrinsic::getDeclaration(
        &IGM.Module, llvm::Intrinsic::ID::stacksave);

    stackRestorePoint =  Builder.CreateCall(stackSaveFn, {}, "spsave");
  }

  // Emit the dynamic alloca.
  auto *alloca = Builder.IRBuilderBase::CreateAlloca(eltTy, arraySize, name);
  alloca->setAlignment(align.getValue());

  assert(!isInEntryBlock ||
         getActiveDominancePoint().isUniversal() &&
             "Must be in entry block if we insert dynamic alloca's without "
             "stackrestores");
  return {Address(alloca, align), stackRestorePoint};
}

/// Deallocate dynamic alloca's memory if requested by restoring the stack
/// location before the dynamic alloca's call.
void IRGenFunction::emitDeallocateDynamicAlloca(StackAddress address) {
  // In coroutines, unconditionally call llvm.coro.alloca.free.
  if (isCoroutine()) {
    auto allocToken = address.getExtraInfo();
    assert(allocToken && "dynamic alloca in coroutine without alloc token?");
    auto freeFn = llvm::Intrinsic::getDeclaration(
        &IGM.Module, llvm::Intrinsic::ID::coro_alloca_free);
    Builder.CreateCall(freeFn, address.getAddressPointer());
    return;
  }

  // Otherwise, call llvm.stackrestore if an address was saved.
  auto savedSP = address.getExtraInfo();
  if (savedSP == nullptr)
    return;
  auto *stackRestoreFn = llvm::Intrinsic::getDeclaration(
      &IGM.Module, llvm::Intrinsic::ID::stackrestore);
  Builder.CreateCall(stackRestoreFn, savedSP);
}

/// Emit a call to do an 'initializeArrayWithCopy' operation.
void irgen::emitInitializeArrayWithCopyCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayInitWithCopyFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'initializeArrayWithTakeNoAlias' operation.
void irgen::emitInitializeArrayWithTakeNoAliasCall(IRGenFunction &IGF,
                                                   SILType T,
                                                   Address destObject,
                                                   Address srcObject,
                                                   llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayInitWithTakeNoAliasFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'initializeArrayWithTakeFrontToBack' operation.
void irgen::emitInitializeArrayWithTakeFrontToBackCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayInitWithTakeFrontToBackFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'initializeArrayWithTakeBackToFront' operation.
void irgen::emitInitializeArrayWithTakeBackToFrontCall(IRGenFunction &IGF,
                                            SILType T,
                                            Address destObject,
                                            Address srcObject,
                                            llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayInitWithTakeBackToFrontFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'assignWithCopy' operation.
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   SILType T,
                                   Address destObject,
                                   Address srcObject) {
  llvm::Value *metadata;
  auto copyFn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                                ValueWitness::AssignWithCopy);
  IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
}

/// Emit a call to do an 'assignWithCopy' operation.
void irgen::emitAssignWithCopyCall(IRGenFunction &IGF,
                                   llvm::Value *metadata,
                                   Address destObject,
                                   Address srcObject) {
  auto copyFn = emitLoadOfValueWitnessFunctionFromMetadata(IGF, metadata,
                                         ValueWitness::AssignWithCopy);
  IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
}

/// Emit a call to do an 'arrayAssignWithCopyNoAlias' operation.
void irgen::emitAssignArrayWithCopyNoAliasCall(IRGenFunction &IGF, SILType T,
                                          Address destObject, Address srcObject,
                                          llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayAssignWithCopyNoAliasFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'arrayAssignWithCopyFrontToBack' operation.
void irgen::emitAssignArrayWithCopyFrontToBackCall(IRGenFunction &IGF,
                                                   SILType T,
                                                   Address destObject,
                                                   Address srcObject,
                                                   llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayAssignWithCopyFrontToBackFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'arrayAssignWithCopyBackToFront' operation.
void irgen::emitAssignArrayWithCopyBackToFrontCall(IRGenFunction &IGF,
                                                   SILType T,
                                                   Address destObject,
                                                   Address srcObject,
                                                   llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayAssignWithCopyBackToFrontFn(),
                         {dest, src, count, metadata});
}

/// Emit a call to do an 'assignWithTake' operation.
void irgen::emitAssignWithTakeCall(IRGenFunction &IGF,
                                   SILType T,
                                   Address destObject,
                                   Address srcObject) {
  llvm::Value *metadata;
  auto copyFn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                                ValueWitness::AssignWithTake);
  IGF.Builder.CreateCall(copyFn,
      {destObject.getAddress(), srcObject.getAddress(), metadata});
}

/// Emit a call to do an 'arrayAssignWithTake' operation.
void irgen::emitAssignArrayWithTakeCall(IRGenFunction &IGF, SILType T,
                                        Address destObject, Address srcObject,
                                        llvm::Value *count) {
  auto metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto dest =
      IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy);
  auto src =
      IGF.Builder.CreateBitCast(srcObject.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayAssignWithTakeFn(),
                         {dest, src, count, metadata});
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
  auto obj =
      IGF.Builder.CreateBitCast(object.getAddress(), IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateCall(IGF.IGM.getArrayDestroyFn(), {obj, count, metadata});
}

/// Emit a call to the 'getExtraInhabitantIndex' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitGetExtraInhabitantIndexCall(IRGenFunction &IGF,
                                                    SILType T,
                                                    Address srcObject) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                       ValueWitness::GetExtraInhabitantIndex);
  
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
  return call;
}

/// Emit a call to the 'storeExtraInhabitant' operation.
/// The type must be dynamically known to have extra inhabitant witnesses.
llvm::Value *irgen::emitStoreExtraInhabitantCall(IRGenFunction &IGF,
                                                 SILType T,
                                                 llvm::Value *index,
                                                 Address destObject) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                          ValueWitness::StoreExtraInhabitant);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {destObject.getAddress(), index, metadata});
  return call;
}

/// Emit a trampoline to call the getEnumTagSinglePayload witness. API:
/// UINT_TYPE (const T* enum, UINT_TYPE emptyCases, M *self)
static llvm::Constant *
getGetEnumTagSinglePayloadTrampolineFn(IRGenModule &IGM) {

  llvm::Type *argTys[] = {IGM.OpaquePtrTy, IGM.Int32Ty, IGM.TypeMetadataPtrTy};

  llvm::SmallString<40> fnName("__swift_getEnumTagSinglePayload");

  auto func = IGM.getOrCreateHelperFunction(
      fnName, IGM.Int32Ty, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        auto *enumAddr = &*(it++);
        auto *numEmptyCases = &*(it++);
        auto *metadata = &*(it++);
        auto &Builder = IGF.Builder;
        auto witnessFunc = emitLoadOfValueWitnessFunctionFromMetadata(
            IGF, metadata, ValueWitness::GetEnumTagSinglePayload);
        auto *result = Builder.CreateCall(witnessFunc,
                                          {enumAddr, numEmptyCases, metadata});
        Builder.CreateRet(result);
      },
      true /*noinline*/);

  // This function is readonly.
  cast<llvm::Function>(func)->addFnAttr(llvm::Attribute::ReadOnly);
  return func;
}

/// Emit a trampoline to call the storeEnumTagSinglePayload witness. API:
/// VOID_TYPE (const T* enum, UINT_TYPE whichCase, UINT_TYPE emptyCases,
///            M *self)
static llvm::Constant *
getStoreEnumTagSinglePayloadTrampolineFn(IRGenModule &IGM) {

  llvm::Type *argTys[] = {IGM.OpaquePtrTy, IGM.Int32Ty, IGM.Int32Ty,
                          IGM.TypeMetadataPtrTy};

  llvm::SmallString<40> fnName("__swift_storeEnumTagSinglePayload");

  return IGM.getOrCreateHelperFunction(
      fnName, IGM.VoidTy, argTys,
      [&](IRGenFunction &IGF) {
        auto it = IGF.CurFn->arg_begin();
        auto *enumAddr = &*(it++);
        auto *whichCase = &*(it++);
        auto *numEmptyCases = &*(it++);
        auto *metadata = &*(it++);
        auto &Builder = IGF.Builder;
        auto witnessFunc = emitLoadOfValueWitnessFunctionFromMetadata(
            IGF, metadata, ValueWitness::StoreEnumTagSinglePayload);
        Builder.CreateCall(witnessFunc,
                           {enumAddr, whichCase, numEmptyCases, metadata});
        Builder.CreateRetVoid();
      },
      true /*noinline*/);
}

llvm::Value *irgen::emitGetEnumTagSinglePayloadCall(IRGenFunction &IGF,
                                                    SILType T,
                                                    llvm::Value *numEmptyCases,
                                                    Address destObject) {
  if (!IGF.optimizeForSize()) {
    llvm::Value *metadata;
    auto fn = IGF.emitValueWitnessFunctionRef(
        T, metadata, ValueWitness::GetEnumTagSinglePayload);
    llvm::CallInst *call = IGF.Builder.CreateCall(
        fn, {destObject.getAddress(), numEmptyCases, metadata});
    return call;
  }
  auto *metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto *func = getGetEnumTagSinglePayloadTrampolineFn(IGF.IGM);
  auto *result = IGF.Builder.CreateCall(
      func,
      {IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy),
       numEmptyCases, metadata});
  return result;
}

llvm::Value *irgen::emitStoreEnumTagSinglePayloadCall(
    IRGenFunction &IGF, SILType T, llvm::Value *whichCase,
    llvm::Value *numEmptyCases, Address destObject) {
  if (!IGF.optimizeForSize()) {
    llvm::Value *metadata;
    auto fn = IGF.emitValueWitnessFunctionRef(
        T, metadata, ValueWitness::StoreEnumTagSinglePayload);
    llvm::CallInst *call = IGF.Builder.CreateCall(
        fn, {destObject.getAddress(), whichCase, numEmptyCases, metadata});
    return call;
  }

  auto *metadata = IGF.emitTypeMetadataRefForLayout(T);
  auto *func = getStoreEnumTagSinglePayloadTrampolineFn(IGF.IGM);
  auto *result = IGF.Builder.CreateCall(
      func,
      {IGF.Builder.CreateBitCast(destObject.getAddress(), IGF.IGM.OpaquePtrTy),
       whichCase, numEmptyCases, metadata});
  return result;
}

/// Emit a call to the 'getEnumTag' operation.
llvm::Value *irgen::emitGetEnumTagCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address srcObject) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                            ValueWitness::GetEnumTag);

  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
  return call;
}

/// Emit a call to the 'destructiveProjectEnumData' operation.
/// The type must be dynamically known to have enum witnesses.
void irgen::emitDestructiveProjectEnumDataCall(IRGenFunction &IGF,
                                               SILType T,
                                               Address srcObject) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                    ValueWitness::DestructiveProjectEnumData);
  IGF.Builder.CreateCall(fn, {srcObject.getAddress(), metadata});
}

/// Emit a call to the 'destructiveInjectEnumTag' operation.
/// The type must be dynamically known to have enum witnesses.
void irgen::emitDestructiveInjectEnumTagCall(IRGenFunction &IGF,
                                             SILType T,
                                             llvm::Value *tagValue,
                                             Address srcObject) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                      ValueWitness::DestructiveInjectEnumTag);
  IGF.Builder.CreateCall(fn, {srcObject.getAddress(), tagValue, metadata});
}

/// Load the 'size' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfSize(IRGenFunction &IGF, SILType T) {
  return IGF.emitValueWitnessValue(T, ValueWitness::Size);
}

/// Load the 'alignmentMask' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfAlignmentMask(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessValue(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::AlignmentMask));
  return IGF.Builder.CreateAnd(flags, mask,
                               flags->getName() + ".alignmentMask");
}

/// Load the 'isPOD' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsPOD(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessValue(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonPOD));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isPOD");
}

/// Load the 'isBitwiseTakable' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsBitwiseTakable(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessValue(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonBitwiseTakable));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isBitwiseTakable");
}

/// Load the 'isInline' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfIsInline(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessValue(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonInline));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".isInline");
}

/// Load the 'hasExtraInhabitants' valueWitness from the given table as an i1.
llvm::Value *irgen::emitLoadOfHasExtraInhabitants(IRGenFunction &IGF, SILType T) {
  auto flags = IGF.emitValueWitnessValue(T, ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::HasExtraInhabitants));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return IGF.Builder.CreateICmpNE(masked, IGF.IGM.getSize(Size(0)),
                                  flags->getName() + ".hasExtraInhabitants");
}

/// Load the 'stride' value witness from the given table as a size_t.
llvm::Value *irgen::emitLoadOfStride(IRGenFunction &IGF, SILType T) {
  return IGF.emitValueWitnessValue(T, ValueWitness::Stride);
}

llvm::Value *irgen::emitLoadOfExtraInhabitantCount(IRGenFunction &IGF,
                                                   SILType T) {
  auto xiFlags =
    IGF.emitValueWitnessValue(T, ValueWitness::ExtraInhabitantFlags);
  auto mask = IGF.IGM.getSize(
                          Size(ExtraInhabitantFlags::NumExtraInhabitantsMask));
  return IGF.Builder.CreateAnd(xiFlags, mask,
                               xiFlags->getName() + ".extraInhabitantCount");
}

std::pair<llvm::Value *, llvm::Value *>
irgen::emitLoadOfIsInline(IRGenFunction &IGF, llvm::Value *metadata) {
  auto *flags = emitLoadOfValueWitnessValueFromMetadata(IGF, metadata,
                                                        ValueWitness::Flags);
  auto mask = IGF.IGM.getSize(Size(ValueWitnessFlags::IsNonInline));
  auto masked = IGF.Builder.CreateAnd(flags, mask);
  return std::make_pair(
      IGF.Builder.CreateICmpEQ(masked, IGF.IGM.getSize(Size(0)),
                               flags->getName() + ".isInline"),
      flags);
}

llvm::Value *irgen::emitLoadOfSize(IRGenFunction &IGF, llvm::Value *metadata) {
  auto *size = emitLoadOfValueWitnessValueFromMetadata(IGF, metadata,
                                                       ValueWitness::Size);
  return size;
}

llvm::Value *irgen::emitAlignMaskFromFlags(IRGenFunction &IGF,
                                           llvm::Value *flags) {
  auto *alignMask = IGF.IGM.getSize(Size(ValueWitnessFlags::AlignmentMask));
  return IGF.Builder.CreateAnd(flags, alignMask,
                               flags->getName() + ".alignmentMask");
}

/// Emit a call to do an 'initializeWithCopy' operation.
void irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address dest,
                                       Address src) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                            ValueWitness::InitializeWithCopy);
  IGF.Builder.CreateCall(fn, {dest.getAddress(), src.getAddress(), metadata});
}

llvm::Value *irgen::emitInitializeWithCopyCall(IRGenFunction &IGF,
                                               llvm::Value *metadata,
                                               Address dest, Address src) {
  auto copyFn = emitLoadOfValueWitnessFunctionFromMetadata(
      IGF, metadata, ValueWitness::InitializeWithCopy);
  llvm::CallInst *call = IGF.Builder.CreateCall(
      copyFn, {dest.getAddress(), src.getAddress(), metadata});

  return call;
}

/// Emit a call to do an 'initializeWithTake' operation.
void irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                       SILType T,
                                       Address dest,
                                       Address src) {
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                            ValueWitness::InitializeWithTake);
  IGF.Builder.CreateCall(fn, {dest.getAddress(), src.getAddress(), metadata});
}

llvm::Value *irgen::emitInitializeWithTakeCall(IRGenFunction &IGF,
                                               llvm::Value *metadata,
                                               Address dest, Address src) {
  auto copyFn = emitLoadOfValueWitnessFunctionFromMetadata(
      IGF, metadata, ValueWitness::InitializeWithTake);
  llvm::CallInst *call = IGF.Builder.CreateCall(
      copyFn, {dest.getAddress(), src.getAddress(), metadata});

  return call;
}

/// Emit a call to do a 'destroy' operation.
void irgen::emitDestroyCall(IRGenFunction &IGF,
                            SILType T,
                            Address object) {
  // If T is a trivial/POD type, nothing needs to be done.
  if (T.getObjectType().isTrivial(IGF.getSILModule()))
    return;
  llvm::Value *metadata;
  auto fn = IGF.emitValueWitnessFunctionRef(T, metadata,
                                            ValueWitness::Destroy);
  IGF.Builder.CreateCall(fn, {object.getAddress(), metadata});
}

void irgen::emitDestroyCall(IRGenFunction &IGF, llvm::Value *metadata,
                            Address object) {
  auto fn = emitLoadOfValueWitnessFunctionFromMetadata(IGF, metadata,
                                                       ValueWitness::Destroy);
  IGF.Builder.CreateCall(fn, {object.getAddress(), metadata});
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
