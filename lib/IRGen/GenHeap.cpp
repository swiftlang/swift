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
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"
#include "llvm/Intrinsics.h"

#include "swift/Basic/SourceLoc.h"

#include "Cleanup.h"
#include "Explosion.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "HeapTypeInfo.h"

#include "GenHeap.h"

using namespace swift;
using namespace irgen;

static llvm::ConstantInt *getSize(IRGenFunction &IGF, Size value) {
  return llvm::ConstantInt::get(IGF.IGM.SizeTy, value.getValue());
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
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "objectdestroy", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  Address structAddr = layout.emitCastOfAlloc(IGF, fn->arg_begin());

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
static llvm::Function *createSizeFn(IRGenModule &IGM,
                                    const HeapLayout &layout) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "objectsize", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  // Ignore the object pointer; we aren't a dynamically-sized array,
  // so it's pointless.

  llvm::Value *size = layout.emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

void HeapLayout::buildMetadataInto(IRGenModule &IGM,
                    llvm::SmallVectorImpl<llvm::Constant*> &metadata) const {

  metadata.push_back(createDtorFn(IGM, *this));
  metadata.push_back(createSizeFn(IGM, *this));
}

static llvm::Constant *buildPrivateMetadataVar(IRGenModule &IGM,
                                          ArrayRef<llvm::Constant*> fields) {
  llvm::Constant *init =
    llvm::ConstantStruct::get(IGM.HeapMetadataStructTy, fields);

  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, IGM.HeapMetadataStructTy,
                             /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage, init,
                             "metadata");
  return var;
}

llvm::Constant *HeapLayout::getPrivateMetadata(IRGenModule &IGM) const {
  llvm::SmallVector<llvm::Constant*, 2> fields;
  buildMetadataInto(IGM, fields);
  return buildPrivateMetadataVar(IGM, fields);
}

/// Lay out an array on the heap.
ArrayHeapLayout::ArrayHeapLayout(IRGenFunction &IGF, Type T)
  : ElementTI(IGF.getFragileTypeInfo(T)) {

  // Add the heap header.
  Size size(0);
  Alignment align(1);
  SmallVector<llvm::Type*, 4> fields;
  addHeapHeaderToLayout(IGF.IGM, size, align, fields);
  assert((size % align).isZero());
  assert(align >= IGF.IGM.getPointerAlignment());

  // Add the length field.
  size += IGF.IGM.getPointerSize();

  // Add value witness tables.
  getValueWitnessTableElements(T, WitnessTypes);
  size += IGF.IGM.getPointerSize() * WitnessTypes.size();

  // Update the required alignment.
  if (ElementTI.StorageAlignment > align)
    align = ElementTI.StorageAlignment;

  // Round the size up to the alignment of the element type.
  // FIXME: resilient types.
  size = size.roundUpToAlignment(ElementTI.StorageAlignment);

  HeaderSize = size;
  Align = align;
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
  elementTI.destroy(IGF, Address(cur, elementTI.StorageAlignment));

  // Loop if we haven't reached the end.
  prev->addIncoming(cur, IGF.Builder.GetInsertBlock());
  llvm::Value *done = IGF.Builder.CreateICmpEQ(cur, end, "done");
  IGF.Builder.CreateCondBr(done, endBB, bodyBB);

  // Done.
  IGF.Builder.emitBlock(endBB);
}

static void
loadValueWitnessTables(IRGenFunction &IGF,
                       llvm::Value *header,
                       const llvm::SetVector<ArchetypeType*> &witnessTypes) {
  if (witnessTypes.empty())
    return;

  Explosion tables(ExplosionKind::Maximal);
  llvm::Value *tablesPtr;
  tablesPtr = IGF.Builder.CreateBitCast(header, IGF.IGM.Int8PtrTy);
  tablesPtr = IGF.Builder.CreateConstInBoundsGEP1_32(tablesPtr, 24);
  llvm::Type *tablesPtrTy = IGF.IGM.WitnessTablePtrTy->getPointerTo();
  tablesPtr = IGF.Builder.CreateBitCast(tablesPtr, tablesPtrTy);

  for (unsigned i = 0, e = witnessTypes.size(); i != e; ++i) {
    llvm::Value *table = IGF.Builder.CreateConstInBoundsGEP1_32(tablesPtr, i);
    table = IGF.Builder.CreateLoad(table, IGF.IGM.getPointerAlignment());
    tables.addUnmanaged(table);
  }
  setValueWitnessTables(
      IGF,
      llvm::makeArrayRef(&*witnessTypes.begin(), &*witnessTypes.end()),
      tables);
}

static void
storeValueWitnessTables(IRGenFunction &IGF,
                        llvm::Value *header,
                        const llvm::SetVector<ArchetypeType*> &witnessTypes) {
  if (witnessTypes.empty())
    return;

  Explosion tables(ExplosionKind::Maximal);
  llvm::Value *tablesPtr;
  tablesPtr = IGF.Builder.CreateBitCast(header, IGF.IGM.Int8PtrTy);
  tablesPtr = IGF.Builder.CreateConstInBoundsGEP1_32(tablesPtr, 24);
  llvm::Type *tablesPtrTy = IGF.IGM.WitnessTablePtrTy->getPointerTo();
  tablesPtr = IGF.Builder.CreateBitCast(tablesPtr, tablesPtrTy);

  getValueWitnessTables(
      IGF,
      llvm::makeArrayRef(&*witnessTypes.begin(), &*witnessTypes.end()),
      tables);
  for (unsigned i = 0, e = witnessTypes.size(); i != e; ++i) {
    llvm::Value *table = IGF.Builder.CreateConstInBoundsGEP1_32(tablesPtr, i);
    llvm::Value *tableVal = tables.claimUnmanagedNext();
    IGF.Builder.CreateStore(tableVal, table, IGF.IGM.getPointerAlignment());
  }
}

/// Create the destructor function for an array layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Constant *
createArrayDtorFn(IRGenModule &IGM,
                  const ArrayHeapLayout &layout,
                  const llvm::SetVector<ArchetypeType*> &witnessTypes) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "arraydestroy", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  llvm::Value *header = fn->arg_begin();
  Address lengthPtr = layout.getLengthPointer(IGF, header);
  llvm::Value *length = IGF.Builder.CreateLoad(lengthPtr, "length");

  loadValueWitnessTables(IGF, header, witnessTypes);

  llvm::Value *elementSize = layout.getElementTypeInfo().getStride(IGF);
  if (!layout.getElementTypeInfo().isPOD(ResilienceScope::Local)) {
    llvm::Value *begin = layout.getBeginPointer(IGF, header);
    llvm::Value *end;
    if (layout.getElementTypeInfo().StorageType->isSized()) {
      end = IGF.Builder.CreateInBoundsGEP(begin, length, "end");
    } else {
      end = IGF.Builder.CreateBitCast(begin, IGF.IGM.Int8PtrTy);
      llvm::Value *lengthBytes = IGF.Builder.CreateMul(elementSize, length);
      end = IGF.Builder.CreateInBoundsGEP(end, lengthBytes);
      end = IGF.Builder.CreateBitCast(end, begin->getType());
    }
    emitArrayDestroy(IGF, begin, end, layout.getElementTypeInfo(), elementSize);
  }

  llvm::Value *size = layout.getAllocationSize(IGF, length, false, false);
  IGF.Builder.CreateRet(size);

  return fn;
}

/// Create the size function for an array layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Function *
createArraySizeFn(IRGenModule &IGM,
                  const ArrayHeapLayout &layout,
                  const llvm::SetVector<ArchetypeType*> &witnessTypes) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "arraysize", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  llvm::Value *header = fn->arg_begin();
  Address lengthPtr = layout.getLengthPointer(IGF, header);
  llvm::Value *length = IGF.Builder.CreateLoad(lengthPtr, "length");

  loadValueWitnessTables(IGF, header, witnessTypes);

  llvm::Value *size = layout.getAllocationSize(IGF, length, false, false);
  IGF.Builder.CreateRet(size);

  return fn;
}

void ArrayHeapLayout::buildMetadataInto(IRGenModule &IGM,
                      llvm::SmallVectorImpl<llvm::Constant*> &fields) const {
  fields.push_back(createArrayDtorFn(IGM, *this, WitnessTypes));
  fields.push_back(createArraySizeFn(IGM, *this, WitnessTypes));
}

llvm::Constant *ArrayHeapLayout::getPrivateMetadata(IRGenModule &IGM) const {
  llvm::SmallVector<llvm::Constant*, 2> fields;
  buildMetadataInto(IGM, fields);
  return buildPrivateMetadataVar(IGM, fields);
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
                           llvm::Intrinsic::getAttributes(intrinsicID));

  llvm::Value *result =
    IGF.Builder.CreateExtractValue(resultWithOverflow, 0);
  llvm::Value *hasOverflow =
    IGF.Builder.CreateExtractValue(resultWithOverflow, 1);

  return IGF.Builder.CreateSelect(hasOverflow, getSizeMax(IGF), result);
}

/// Return the array stride for the given type.
static Size getElementStride(const TypeInfo &type) {
  return type.StorageSize.roundUpToAlignment(type.StorageAlignment);
}

/// Compute the size of an array allocation.
///
/// \param length - the requested length; must be a size_t unless
///   canOverflow is set
/// \param canOverflow - whether the size computation can overflow;
///   this is false for computations involving a known-good length
/// \param updateLength - whether to update the 'length' parameter
///   with the proper length, i.e. the length as a size_t
llvm::Value *ArrayHeapLayout::getAllocationSize(IRGenFunction &IGF,
                                                llvm::Value *&length,
                                                bool canOverflow,
                                                bool updateLength) const {
  // We're computing HeaderSize + length * sizeof(element).

  // Easy case: the length is a static constant.
  llvm::ConstantInt *clength = dyn_cast<llvm::ConstantInt>(length);
  if (clength && ElementTI.StorageType->isSized()) {
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
                              getElementStride(ElementTI).getValue());
    assert(elementStride);
    auto scaledLength = lenval.umul_ov(elementStride, overflow);
    if (overflow) return getSizeMax(IGF);

    // Add the header size in.
    llvm::APInt headerSize(sizeWidth, HeaderSize.getValue());
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
  llvm::Value *headerSize = getSize(IGF, HeaderSize);
  if (ElementTI.isEmpty(ResilienceScope::Local))
    return headerSize;

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
                         size, headerSize);
  } else {
    size = IGF.Builder.CreateAdd(size, headerSize);
  }

  return size;
}

/// Returns a pointer to the 'length' field of an array allocation.
Address ArrayHeapLayout::getLengthPointer(IRGenFunction &IGF,
                                          llvm::Value *alloc) const {
  assert(alloc->getType() == IGF.IGM.RefCountedPtrTy);
  llvm::Value *addr = IGF.Builder.CreateConstInBoundsGEP1_32(alloc, 1);
  addr = IGF.Builder.CreateBitCast(addr, IGF.IGM.SizeTy->getPointerTo());

  return Address(addr, IGF.IGM.getPointerAlignment());
}

llvm::Value *ArrayHeapLayout::getBeginPointer(IRGenFunction &IGF,
                                              llvm::Value *alloc) const {
  assert(alloc->getType() == IGF.IGM.RefCountedPtrTy);
  alloc = IGF.Builder.CreateBitCast(alloc, IGF.IGM.Int8PtrTy);
  llvm::Value *begin =
    IGF.Builder.CreateConstInBoundsGEP1_32(alloc, HeaderSize.getValue());
  return IGF.Builder.CreateBitCast(begin,
                                 ElementTI.getStorageType()->getPointerTo());
}

ManagedValue ArrayHeapLayout::emitAlloc(IRGenFunction &IGF,
                                        llvm::Value *length,
                                        Address &begin,
                                        Expr *init,
                                        const llvm::Twine &name) const {
  llvm::Constant *metadata = getPrivateMetadata(IGF.IGM);
  llvm::Value *size = getAllocationSize(IGF, length, true, true);
  llvm::Value *align = getSize(IGF, Size(Align.getValue()));

  // Perform the allocation.
  llvm::Value *alloc =
    IGF.emitAllocObjectCall(metadata, size, align, "array.alloc");

  storeValueWitnessTables(IGF, alloc, WitnessTypes);

  // Find the begin pointer.
  llvm::Value *beginPtr = getBeginPointer(IGF, alloc);
  begin = Address(beginPtr, ElementTI.StorageAlignment);

  // If we don't have an initializer, just zero-initialize and
  // immediately enter a release cleanup.
  if (!init) {
    llvm::Value *sizeToMemset =
      IGF.Builder.CreateSub(size, getSize(IGF, HeaderSize));
    IGF.Builder.CreateMemSet(
                     IGF.Builder.CreateBitCast(beginPtr, IGF.IGM.Int8PtrTy),
                             llvm::ConstantInt::get(IGF.IGM.Int8Ty, 0),
                             sizeToMemset,
                             Align.alignmentAtOffset(HeaderSize).getValue(),
                             /*volatile*/ false);

  // Otherwise, repeatedly evaluate the initializer into successive
  // elements, with a cleanup around to deallocate the object if necessary.
  } else {
    llvm_unreachable("unimplemented: array alloc with nontrivial initializer");
  }

  return IGF.enterReleaseCleanup(alloc);
}

llvm::Value *IRGenFunction::emitUnmanagedAlloc(const HeapLayout &layout,
                                               const llvm::Twine &name) {
  llvm::Value *metadata = layout.getPrivateMetadata(IGM);
  llvm::Value *size = layout.emitSize(*this);
  llvm::Value *align = layout.emitAlign(*this);

  return emitAllocObjectCall(metadata, size, align, name);
}

ManagedValue IRGenFunction::emitAlloc(const HeapLayout &layout,
                                      const llvm::Twine &name) {
  llvm::Value *alloc = emitUnmanagedAlloc(layout, name);
  return enterReleaseCleanup(alloc);
}

/// Given that an object has been allocated, cast the result to the
/// appropriate type.
Address HeapLayout::emitCastOfAlloc(IRGenFunction &IGF,
                                    llvm::Value *alloc,
                                    const llvm::Twine &name) const {
  llvm::Value *addr =
    IGF.Builder.CreateBitCast(alloc, getType()->getPointerTo(), name);
  return Address(addr, getAlignment());
}

bool HeapTypeInfo::isSingleRetainablePointer(ResilienceScope scope) const {
  return true;
}

const TypeInfo *TypeConverter::convertBuiltinObjectPointer() {
  return new HeapTypeInfo(IGM.RefCountedPtrTy, IGM.getPointerSize(),
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

static void emitRetainAndManage(IRGenFunction &IGF, llvm::Value *value,
                                Explosion &out) {
  IGF.emitRetainCall(value);
  out.add(IGF.enterReleaseCleanup(value));
}

/// Emit a retain of a value.  This is usually not required because
/// values in explosions are typically "live", i.e. have a +1 owned by
/// the explosion.
void IRGenFunction::emitRetain(llvm::Value *value, Explosion &out) {
  if (doesNotRequireRefCounting(value)) {
    out.addUnmanaged(value);
    return;
  }

  emitRetainAndManage(*this, value, out);
}

/// Emit a load of a live value from the given retaining variable.
void IRGenFunction::emitLoadAndRetain(Address address, Explosion &out) {
  llvm::Value *value = Builder.CreateLoad(address);
  emitRetainAndManage(*this, value, out);
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

namespace {
  struct CallRelease : Cleanup {
    llvm::Value *Value;
    CallRelease(llvm::Value *value) : Value(value) {}

    void emit(IRGenFunction &IGF) const {
      emitReleaseCall(IGF, Value);
    }
  };
}

/// Enter a cleanup to release an object.
ManagedValue IRGenFunction::enterReleaseCleanup(llvm::Value *value) {
  if (doesNotRequireRefCounting(value))
    return ManagedValue(value);

  pushFullExprCleanup<CallRelease>(value);
  return ManagedValue(value, getCleanupsDepth());
}

namespace {
  class CallDealloc : public Cleanup {
    llvm::Value *Allocation;
    llvm::Value *Size;
  public:
    CallDealloc(llvm::Value *allocation, llvm::Value *size)
      : Allocation(allocation), Size(size) {}
    void emit(IRGenFunction &IGF) const {
      IGF.emitDeallocObjectCall(Allocation, Size);
    }
  };
}

/// Enter a cleanup to call swift_dealloc on the given pointer.
/// This cleanup will usually be deactivated as soon as the
/// initializer completes.
CleanupsDepth
IRGenFunction::pushDeallocCleanup(llvm::Value *allocation,
                                  llvm::Value *size) {
  pushFullExprCleanup<CallDealloc>(allocation, size);
  return getCleanupsDepth();
}
