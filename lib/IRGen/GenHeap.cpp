//===--- GenHeap.cpp - Layout of heap objects and their metadata ----------===//
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
//  This file implements layout for heap metadata.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Intrinsics.h"

#include "swift/Basic/SourceLoc.h"
#include "swift/ABI/MetadataValues.h"

#include "Explosion.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "HeapTypeInfo.h"

#include "GenHeap.h"

using namespace swift;
using namespace irgen;

/// Produce a constant to place in a metatype's isa field
/// corresponding to the given metadata kind.
static llvm::ConstantInt *getMetadataKind(IRGenModule &IGM,
                                          MetadataKind kind) {
  return llvm::ConstantInt::get(IGM.MetadataKindTy, uint8_t(kind));
}

static llvm::ConstantInt *getSize(IRGenFunction &IGF,
                                  const llvm::APInt &value) {
  return cast<llvm::ConstantInt>(llvm::ConstantInt::get(IGF.IGM.SizeTy, value));
}

static llvm::ConstantInt *getSizeMax(IRGenFunction &IGF) {
  return llvm::ConstantInt::getSigned(IGF.IGM.SizeTy, -1);
}

/// Perform the layout required for a heap object.
HeapLayout::HeapLayout(IRGenModule &IGM, LayoutStrategy strategy,
                       llvm::ArrayRef<const TypeInfo *> fields,
                       llvm::StructType *typeToFill)
  : StructLayout(IGM, LayoutKind::HeapObject, strategy, fields, typeToFill) {
}

/// Create the destructor function for a layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Function *createDtorFn(IRGenModule &IGM,
                                    const HeapLayout &layout) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DeallocatingDtorTy,
                           llvm::Function::InternalLinkage,
                           "objectdestroy", &IGM.Module);

  IRGenFunction IGF(IGM, CanType(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  Address structAddr = layout.emitCastTo(IGF, fn->arg_begin());

  for (auto &field : layout.getElements()) {
    if (field.Type->isPOD(ResilienceScope::Local))
      continue;

    field.Type->destroy(IGF, field.project(IGF, structAddr));
  }

  llvm::Value *size = layout.emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

/// Create the size function for a layout.
/// TODO: give this some reasonable name and possibly linkage.
llvm::Constant *HeapLayout::createSizeFn(IRGenModule &IGM) const {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DeallocatingDtorTy,
                           llvm::Function::InternalLinkage,
                           "objectsize", &IGM.Module);

  IRGenFunction IGF(IGM, CanType(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  // Ignore the object pointer; we aren't a dynamically-sized array,
  // so it's pointless.

  llvm::Value *size = emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

static llvm::Constant *buildPrivateMetadata(IRGenModule &IGM,
                                            llvm::Constant *dtorFn,
                                            MetadataKind kind) {
  // Build the fields of the private metadata.
  llvm::SmallVector<llvm::Constant*, 4> fields;
  fields.push_back(dtorFn);
  fields.push_back(llvm::ConstantPointerNull::get(IGM.WitnessTablePtrTy));
  fields.push_back(llvm::ConstantStruct::get(IGM.TypeMetadataStructTy,
                                             getMetadataKind(IGM, kind)));

  llvm::Constant *init =
    llvm::ConstantStruct::get(IGM.FullHeapMetadataStructTy, fields);

  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, IGM.FullHeapMetadataStructTy,
                             /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage, init,
                             "metadata");

  llvm::Constant *indices[] = {
    llvm::ConstantInt::get(IGM.Int32Ty, 0),
    llvm::ConstantInt::get(IGM.Int32Ty, 2)
  };
  return llvm::ConstantExpr::getInBoundsGetElementPtr(var, indices);
}

llvm::Constant *HeapLayout::getPrivateMetadata(IRGenModule &IGM) const {
  return buildPrivateMetadata(IGM, createDtorFn(IGM, *this),
                              MetadataKind::HeapLocalVariable);
}

/// Return the size of the array heap header, minus the array of
/// stored archetypes.
static Size getArrayHeapHeaderSize(IRGenModule &IGM) {
  return getHeapHeaderSize(IGM) + IGM.getPointerSize();
}

/// Given an array allocation, project down to the necessary-bindings
/// buffer.
static Address projectBindingsBuffer(IRGenFunction &IGF,
                                     Address alloc) {
  Size headerSize = getArrayHeapHeaderSize(IGF.IGM);

  Address slot = alloc;
  slot = IGF.Builder.CreateBitCast(slot, IGF.IGM.Int8PtrTy);
  slot = IGF.Builder.CreateConstByteArrayGEP(slot, headerSize);
  return slot;
}

/// Do the work necessary to bind the necessary bindings for the given
/// array allocation.
static void bindNecessaryBindings(IRGenFunction &IGF,
                                  const NecessaryBindings &bindings,
                                  Address allocation) {
  if (bindings.empty()) return;
  Address bindingsBuffer = projectBindingsBuffer(IGF, allocation);
  bindings.restore(IGF, bindingsBuffer);
}

/// Compute the basic information for how to lay out a heap array.
HeapArrayInfo::HeapArrayInfo(IRGenFunction &IGF, CanType T)
  : ElementTI(IGF.getFragileTypeInfo(T)), Bindings(IGF.IGM, T) {}

/// Lay out the allocation in this IGF.
HeapArrayInfo::Layout HeapArrayInfo::getLayout(IRGenFunction &IGF) const {
  // Start with the heap header.
  Size headerSize(0);
  Alignment headerAlign(1);
  SmallVector<llvm::Type*, 4> fields;
  addHeapHeaderToLayout(IGF.IGM, headerSize, headerAlign, fields);
  assert((headerSize % headerAlign).isZero());
  assert(headerAlign >= IGF.IGM.getPointerAlignment());

  // Add the length field.
  headerSize += IGF.IGM.getPointerSize();
  assert(headerSize == getArrayHeapHeaderSize(IGF.IGM));

  // Add the necessary bindings size.
  headerSize += Bindings.getBufferSize(IGF.IGM);

  // The easy case is when we know the layout of the element.
  if (auto fixedElementTI = dyn_cast<FixedTypeInfo>(&ElementTI)) {
    // Update the required alignment.
    if (fixedElementTI->getFixedAlignment() > headerAlign)
      headerAlign = fixedElementTI->getFixedAlignment();

    // Round the size up to the alignment of the element type.
    // FIXME: resilient types.
    headerSize = headerSize.roundUpToAlignment(
                                        fixedElementTI->getFixedAlignment());

    return {
      IGF.IGM.getSize(headerSize),
      IGF.IGM.getSize(headerAlign.asSize()),
      headerAlign
    };
  }

  // Otherwise, we need to do this computation at runtime.

  // Read the alignment of the element type.
  llvm::Value *eltAlign = ElementTI.getAlignment(IGF);

  // Round the header size up to the element alignment.
  llvm::Value *headerSizeV = IGF.IGM.getSize(headerSize);

  // mask = alignment - 1
  // headerSize = (headerSize + mask) & ~mask
  auto eltAlignMask = IGF.Builder.CreateSub(eltAlign, IGF.IGM.getSize(Size(1)));
  headerSizeV = IGF.Builder.CreateAdd(headerSizeV, eltAlignMask);
  llvm::Value *eltAlignMaskInverted = IGF.Builder.CreateNot(eltAlignMask);
  headerSizeV = IGF.Builder.CreateAnd(headerSizeV, eltAlignMaskInverted,
                                      "array-header-size");

  // allocAlign = max(headerAlign, alignment)
  llvm::Value *headerAlignV = IGF.IGM.getSize(headerAlign.asSize());
  llvm::Value *overaligned =
    IGF.Builder.CreateICmpUGT(eltAlign, headerAlignV, "overaligned");
  llvm::Value *allocAlign =
    IGF.Builder.CreateSelect(overaligned, eltAlign, headerAlignV);

  return { headerSizeV, allocAlign, headerAlign };
}

/// Destroy all the elements of an array.
static void emitArrayDestroy(IRGenFunction &IGF,
                             llvm::Value *begin, llvm::Value *end,
                             const TypeInfo &elementTI,
                             llvm::Value *elementSize) {
  assert(!elementTI.isPOD(ResilienceScope::Local));

  llvm::BasicBlock *endBB = IGF.createBasicBlock("end");
  llvm::BasicBlock *bodyBB = IGF.createBasicBlock("loop-body");

  // First, check whether the array is empty.  This is possible unless
  // we ban allocating a zero-length array (or use a static allocation
  // for one) and therefore never enter this code.
  llvm::Value *isEmpty = IGF.Builder.CreateICmpEQ(begin, end, "empty");
  IGF.Builder.CreateCondBr(isEmpty, endBB, bodyBB);

  llvm::BasicBlock *entryBB = IGF.Builder.GetInsertBlock();
  IGF.Builder.emitBlock(bodyBB);

  // Destroy things in reverse order for no particular reason.
  // Slightly better cache locality, maybe.
  llvm::PHINode *prev = IGF.Builder.CreatePHI(begin->getType(), 2, "prev");
  prev->addIncoming(end, entryBB);

  // 'prev' points one past the end of the valid array; make something
  // that points at the end.
  llvm::Value *cur;
  if (elementTI.StorageType->isSized()) {
    cur = IGF.Builder.CreateInBoundsGEP(prev, getSizeMax(IGF), "cur");
  } else {
    cur = IGF.Builder.CreateBitCast(prev, IGF.IGM.Int8PtrTy);
    llvm::Value *strideBytes = IGF.Builder.CreateNeg(elementSize);
    cur = IGF.Builder.CreateInBoundsGEP(cur, strideBytes);
    cur = IGF.Builder.CreateBitCast(cur, prev->getType());
  }

  // Destroy this element.
  elementTI.destroy(IGF, elementTI.getAddressForPointer(cur));

  // Loop if we haven't reached the end.
  prev->addIncoming(cur, IGF.Builder.GetInsertBlock());
  llvm::Value *done = IGF.Builder.CreateICmpEQ(cur, end, "done");
  IGF.Builder.CreateCondBr(done, endBB, bodyBB);

  // Done.
  IGF.Builder.emitBlock(endBB);
}

/// Create the destructor function for an array layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Constant *
createArrayDtorFn(IRGenModule &IGM,
                  const HeapArrayInfo &arrayInfo,
                  const NecessaryBindings &bindings) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DeallocatingDtorTy,
                           llvm::Function::InternalLinkage,
                           "arraydestroy", &IGM.Module);

  IRGenFunction IGF(IGM, CanType(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  // Bind the necessary archetypes.  This is required before we can
  // lay out the array in this IGF.
  llvm::Value *header = fn->arg_begin();
  bindNecessaryBindings(IGF, bindings,
                        Address(header, IGM.getPointerAlignment()));

  auto layout = arrayInfo.getLayout(IGF);

  Address lengthPtr = arrayInfo.getLengthPointer(IGF, layout, header);
  llvm::Value *length = IGF.Builder.CreateLoad(lengthPtr, "length");

  auto &eltTI = arrayInfo.getElementTypeInfo();

  // If the layout isn't known to be POD, we actually have to do work here.
  if (!eltTI.isPOD(ResilienceScope::Local)) {
    llvm::Value *elementSize = eltTI.getStride(IGF);

    llvm::Value *begin = arrayInfo.getBeginPointer(IGF, layout, header);
    llvm::Value *end;
    if (isa<FixedTypeInfo>(eltTI)) {
      end = IGF.Builder.CreateInBoundsGEP(begin, length, "end");
    } else {
      end = IGF.Builder.CreateBitCast(begin, IGF.IGM.Int8PtrTy);
      llvm::Value *lengthBytes = IGF.Builder.CreateMul(elementSize, length);
      end = IGF.Builder.CreateInBoundsGEP(end, lengthBytes);
      end = IGF.Builder.CreateBitCast(end, begin->getType());
    }

    emitArrayDestroy(IGF, begin, end, eltTI, elementSize);
  }

  llvm::Value *size =
    arrayInfo.getAllocationSize(IGF, layout, length, false, false);
  IGF.Builder.CreateRet(size);

  return fn;
}

llvm::Constant *HeapArrayInfo::getPrivateMetadata(IRGenModule &IGM) const {
  return buildPrivateMetadata(IGM, createArrayDtorFn(IGM, *this, Bindings),
                              MetadataKind::HeapArray);
}

/// Perform an arithmetic operation which saturates at SIZE_MAX.
static llvm::Value *checkOverflow(IRGenFunction &IGF,
                                  llvm::Intrinsic::ID intrinsicID,
                                  llvm::Value *lhs, llvm::Value *rhs) {
  llvm::Function *intrinsic =
    llvm::Intrinsic::getDeclaration(&IGF.IGM.Module, intrinsicID,
                                    lhs->getType());
  llvm::CallInst *resultWithOverflow =
    IGF.Builder.CreateCall2(intrinsic, lhs, rhs);
  resultWithOverflow->setAttributes(
                           llvm::Intrinsic::getAttributes(IGF.IGM.LLVMContext,
                                                          intrinsicID));

  llvm::Value *result =
    IGF.Builder.CreateExtractValue(resultWithOverflow, 0);
  llvm::Value *hasOverflow =
    IGF.Builder.CreateExtractValue(resultWithOverflow, 1);

  return IGF.Builder.CreateSelect(hasOverflow, getSizeMax(IGF), result);
}

/// Compute the size of an array allocation.
///
/// \param length - the requested length; must be a size_t unless
///   canOverflow is set
/// \param canOverflow - whether the size computation can overflow;
///   this is false for computations involving a known-good length
/// \param updateLength - whether to update the 'length' parameter
///   with the proper length, i.e. the length as a size_t
llvm::Value *HeapArrayInfo::getAllocationSize(IRGenFunction &IGF,
                                              const Layout &layout,
                                              llvm::Value *&length,
                                              bool canOverflow,
                                              bool updateLength) const {
  // We're computing HeaderSize + length * sizeof(element).

  // Easy case: the length is a static constant.
  llvm::ConstantInt *clength = dyn_cast<llvm::ConstantInt>(length);
  if (clength && ElementTI.isFixedSize()) {
    auto &fixedElementTI = cast<FixedTypeInfo>(ElementTI);
    unsigned sizeWidth = IGF.IGM.SizeTy->getBitWidth();

    // Get the length to size_t, making sure it isn't too large.
    if (canOverflow && !clength->getValue().isIntN(sizeWidth)) {
      llvm::Value *result = getSizeMax(IGF);
      if (updateLength) length = result;
      return result;
    }

    llvm::APInt lenval = clength->getValue().zextOrTrunc(sizeWidth);
    if (updateLength && length->getType() != IGF.IGM.SizeTy)
      length = getSize(IGF, lenval);

    bool overflow = false;

    // Scale the length by the element stride.
    llvm::APInt elementStride(sizeWidth,
                              fixedElementTI.getFixedStride().getValue());
    assert(elementStride);
    auto scaledLength = lenval.umul_ov(elementStride, overflow);
    if (overflow) return getSizeMax(IGF);

    // Add the header size in.
    assert(isa<llvm::ConstantInt>(layout.HeaderSize) &&
           "fixed-size array element type without constant header size?");
    auto &headerSize = cast<llvm::ConstantInt>(layout.HeaderSize)->getValue();
    auto lengthWithHeader = scaledLength.uadd_ov(headerSize, overflow);
    if (overflow) return getSizeMax(IGF);

    // All done.
    return getSize(IGF, lengthWithHeader);
  }

  // First things first: coerce the length to the right bit-width.
  llvm::Value *properLength = length;
  if (canOverflow && length->getType() != IGF.IGM.SizeTy) {
    llvm::IntegerType *lengthTy = cast<llvm::IntegerType>(length->getType());
    unsigned lengthWidth = lengthTy->getBitWidth();
    unsigned sizeWidth = IGF.IGM.SizeTy->getBitWidth();

    assert(lengthWidth != sizeWidth);

    // If the length is narrower than the width, just zext.  We always
    // treat the input type as having unsigned semantics.
    if (lengthWidth < sizeWidth) {
      properLength = IGF.Builder.CreateZExt(length, IGF.IGM.SizeTy);

    // Otherwise, we need to truncate.
    } else {
      properLength = IGF.Builder.CreateTrunc(length, IGF.IGM.SizeTy);
      llvm::Value *zext = IGF.Builder.CreateZExt(properLength, lengthTy,
                                                 "overflow.zext");
      llvm::Value *hasOverflow = IGF.Builder.CreateICmpNE(length, zext);

      properLength = IGF.Builder.CreateSelect(hasOverflow, getSizeMax(IGF),
                                              properLength);
    }
  }
  assert(properLength->getType() == IGF.IGM.SizeTy);
  if (updateLength) length = properLength;

  // If the element size is known to be zero, we don't need to do
  // anything further.
  if (ElementTI.isKnownEmpty())
    return layout.HeaderSize;

  llvm::Value *size = properLength;

  // Scale that by the element stride, saturating at SIZE_MAX.
  llvm::Value *elementStride = ElementTI.getStride(IGF);
  if (canOverflow) {
    size = checkOverflow(IGF, llvm::Intrinsic::umul_with_overflow,
                         size, elementStride);
  } else {
    size = IGF.Builder.CreateMul(size, elementStride);
  }

  // Increase that by the header size, saturating at SIZE_MAX.
  if (canOverflow) {
    size = checkOverflow(IGF, llvm::Intrinsic::uadd_with_overflow,
                         size, layout.HeaderSize);
  } else {
    size = IGF.Builder.CreateAdd(size, layout.HeaderSize);
  }

  return size;
}

/// Returns a pointer to the 'length' field of an array allocation.
Address HeapArrayInfo::getLengthPointer(IRGenFunction &IGF,
                                        const Layout &layout,
                                        llvm::Value *alloc) const {
  assert(alloc->getType() == IGF.IGM.RefCountedPtrTy);
  llvm::Value *addr = IGF.Builder.CreateConstInBoundsGEP1_32(alloc, 1);
  addr = IGF.Builder.CreateBitCast(addr, IGF.IGM.SizeTy->getPointerTo());

  return Address(addr, IGF.IGM.getPointerAlignment());
}

llvm::Value *HeapArrayInfo::getBeginPointer(IRGenFunction &IGF,
                                            const Layout &layout,
                                            llvm::Value *alloc) const {
  assert(alloc->getType() == IGF.IGM.RefCountedPtrTy);
  alloc = IGF.Builder.CreateBitCast(alloc, IGF.IGM.Int8PtrTy);
  llvm::Value *begin = IGF.Builder.CreateInBoundsGEP(alloc, layout.HeaderSize);
  return IGF.Builder.CreateBitCast(begin,
                                 ElementTI.getStorageType()->getPointerTo());
}

llvm::Value *HeapArrayInfo::emitUnmanagedAlloc(IRGenFunction &IGF,
                                               llvm::Value *length,
                                               Address &begin,
                                               const llvm::Twine &name) const {
  Layout layout = getLayout(IGF);

  llvm::Constant *metadata = getPrivateMetadata(IGF.IGM);
  llvm::Value *size = getAllocationSize(IGF, layout, length, true, true);
  llvm::Value *align = layout.AllocAlign;

  // Perform the allocation.
  llvm::Value *alloc =
    IGF.emitAllocObjectCall(metadata, size, align, "array.alloc");

  if (!Bindings.empty()) {
    Address bindingsBuffer =
      projectBindingsBuffer(IGF, Address(alloc, layout.BestStaticAlignment));
    Bindings.save(IGF, bindingsBuffer);
  }

  // Store the length pointer to the array.
  Address lengthPtr = getLengthPointer(IGF, layout, alloc);
  // FIXME: storing the actual length here doesn't seem to work.
  IGF.Builder.CreateStore(IGF.IGM.getSize(Size(0)), lengthPtr);

  // Find the begin pointer.
  llvm::Value *beginPtr = getBeginPointer(IGF, layout, alloc);
  begin = ElementTI.getAddressForPointer(beginPtr);

  // Zero-initialize and immediately enter a release cleanup.
  llvm::Value *sizeToMemset = IGF.Builder.CreateSub(size, layout.HeaderSize);

  Alignment arrayAlignment = layout.BestStaticAlignment;
  if (auto offset = dyn_cast<llvm::ConstantInt>(layout.HeaderSize))
    arrayAlignment =
      arrayAlignment.alignmentAtOffset(Size(offset->getZExtValue()));

  IGF.Builder.CreateMemSet(
                   IGF.Builder.CreateBitCast(beginPtr, IGF.IGM.Int8PtrTy),
                           llvm::ConstantInt::get(IGF.IGM.Int8Ty, 0),
                           sizeToMemset,
                           arrayAlignment.getValue(),
                           /*volatile*/ false);

  return alloc;
}

llvm::Value *IRGenFunction::emitUnmanagedAlloc(const HeapLayout &layout,
                                               const llvm::Twine &name) {
  llvm::Value *metadata = layout.getPrivateMetadata(IGM);
  llvm::Value *size = layout.emitSize(*this);
  llvm::Value *align = layout.emitAlign(*this);

  return emitAllocObjectCall(metadata, size, align, name);
}

namespace {
  class BuiltinObjectPointerTypeInfo
    : public HeapTypeInfo<BuiltinObjectPointerTypeInfo> {
  public:
    BuiltinObjectPointerTypeInfo(llvm::PointerType *storage,
                                 Size size, Alignment align)
    : HeapTypeInfo(storage, size, align) {}

    /// Builtin.ObjectPointer uses Swift reference-counting.
    bool hasSwiftRefcount() const { return true; }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjectPointer() {
  return new BuiltinObjectPointerTypeInfo(IGM.RefCountedPtrTy,
                                          IGM.getPointerSize(),
                                          IGM.getPointerAlignment());
}

/// Does the given value superficially not require reference-counting?
static bool doesNotRequireRefCounting(llvm::Value *value) {
  // Constants never require reference-counting.
  return isa<llvm::Constant>(value);
}

/// Emit a call to swift_retain_noresult.  In general, you should not be using
/// this routine; instead you should use emitRetain, which properly
/// balances the retain.
void IRGenFunction::emitRetainCall(llvm::Value *value) {
  // Make sure the input pointer is the right type.
  if (value->getType() != IGM.RefCountedPtrTy)
    value = Builder.CreateBitCast(value, IGM.RefCountedPtrTy);
  
  // Emit the call.
  llvm::CallInst *call = Builder.CreateCall(IGM.getRetainNoResultFn(), value);
  call->setCallingConv(IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a retain of a value.  This is usually not required because
/// values in explosions are typically "live", i.e. have a +1 owned by
/// the explosion.
void IRGenFunction::emitRetain(llvm::Value *value, Explosion &out) {
  if (doesNotRequireRefCounting(value)) {
    out.add(value);
    return;
  }

  emitRetainCall(value);
  out.add(value);
}

/// Emit a load of a live value from the given retaining variable.
void IRGenFunction::emitLoadAndRetain(Address address, Explosion &out) {
  llvm::Value *value = Builder.CreateLoad(address);
  emitRetainCall(value);
  out.add(value);
}

/// Emit a store of a live value to the given retaining variable.
void IRGenFunction::emitAssignRetained(llvm::Value *newValue, Address address) {
  // Pull the old value out of the address.
  llvm::Value *oldValue = Builder.CreateLoad(address);

  // We assume the new value is already retained.
  Builder.CreateStore(newValue, address);

  // Release the old value.
  emitRelease(oldValue);
}

/// Emit an initialize of a live value to the given retaining variable.
void IRGenFunction::emitInitializeRetained(llvm::Value *newValue,
                                           Address address) {
  // We assume the new value is already retained.
  Builder.CreateStore(newValue, address);
}

/// Emit a call to swift_release for the given value.
static void emitReleaseCall(IRGenFunction &IGF, llvm::Value *value) {
  // Instead of casting the input to %swift.refcounted*, we cast the
  // function type.  This tends to produce less IR, but might be evil.
  llvm::Constant *fn = IGF.IGM.getReleaseFn();
  if (value->getType() != IGF.IGM.RefCountedPtrTy) {
    llvm::FunctionType *fnType =
    llvm::FunctionType::get(IGF.IGM.VoidTy, value->getType(), false);
    fn = llvm::ConstantExpr::getBitCast(fn, fnType->getPointerTo());
  }
  
  // The call itself can never throw.
  llvm::CallInst *call = IGF.Builder.CreateCall(fn, value);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a release of a live value.
void IRGenFunction::emitRelease(llvm::Value *value) {
  if (doesNotRequireRefCounting(value)) return;
  return emitReleaseCall(*this, value);
}


