//===--- IRGenFunction.cpp - Swift Per-Function IR Generation -------------===//
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
//  This file implements basic setup and teardown for the class which
//  performs IR generation for function bodies.
//
//===----------------------------------------------------------------------===//

#include "llvm/Instructions.h"
#include "llvm/Support/SourceMgr.h"
#include "swift/Basic/SourceLoc.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

IRGenFunction::IRGenFunction(IRGenModule &IGM, CanType t, ArrayRef<Pattern*> p,
                             ExplosionKind explosionLevel,
                             unsigned uncurryLevel, llvm::Function *Fn,
                             Prologue prologue)
  : IGM(IGM), Builder(IGM.getLLVMContext()), CurFuncType(t),
    CurFuncParamPatterns(p), CurFn(Fn),
    CurExplosionLevel(explosionLevel), CurUncurryLevel(uncurryLevel),
    CurPrologue(prologue), ContextPtr(nullptr),
    UnreachableBB(nullptr), JumpDestSlot(nullptr),
    InnermostScope(Cleanups.stable_end()) {
  emitPrologue();
}

IRGenFunction::~IRGenFunction() {
  emitEpilogue();
}

/// Call the llvm.memcpy intrinsic.  The arguments need not already
/// be of i8* type.
void IRGenFunction::emitMemCpy(llvm::Value *dest, llvm::Value *src,
                               Size size, Alignment align) {
  Builder.CreateMemCpy(dest, src, size.getValue(), align.getValue(), false);
}

void IRGenFunction::emitMemCpy(Address dest, Address src, Size size) {
  // Map over to the inferior design of the LLVM intrinsic.
  emitMemCpy(dest.getAddress(), src.getAddress(), size,
             std::min(dest.getAlignment(), src.getAlignment()));
}

/// Given a size, try to turn it into an alloc token.
typedef unsigned AllocToken;
const AllocToken InvalidAllocToken = ~0U;
static AllocToken getAllocToken(IRGenModule &IGM, uint64_t size) {
  // This is meant to exactly match the algorithm in the runtime's Alloc.h.
  if (size >= 0x1000) return InvalidAllocToken;
  if (size == 0) return 0;
  --size;
  if (IGM.getPointerSize() == Size(8)) {
    if      (size < 0x80)   return (size >> 3);
    else if (size < 0x100)  return (size >> 4) + 0x8;
    else if (size < 0x200)  return (size >> 5) + 0x10;
    else if (size < 0x400)  return (size >> 6) + 0x18;
    else if (size < 0x800)  return (size >> 7) + 0x20;
    else if (size < 0x1000) return (size >> 8) + 0x28;
  } else {
    assert(IGM.getPointerSize() == Size(4));
    if      (size < 0x40)   return (size >> 2);
    else if (size < 0x80)   return (size >> 3) + 0x8;
    else if (size < 0x100)  return (size >> 4) + 0x10;
    else if (size < 0x200)  return (size >> 5) + 0x18;
    else if (size < 0x400)  return (size >> 6) + 0x20;
    else if (size < 0x800)  return (size >> 7) + 0x28;
    else if (size < 0x1000) return (size >> 8) + 0x30;
  }
  llvm_unreachable("everything is terrible");
}

static llvm::AttributeSet getAllocAttrs(llvm::LLVMContext &ctx) {
  llvm::AttributeWithIndex attrValues[] = {
    llvm::AttributeWithIndex::get(ctx, 0, llvm::Attribute::NoAlias),
    llvm::AttributeWithIndex::get(ctx, ~0, llvm::Attribute::NoUnwind)
  };
  return llvm::AttributeSet::get(ctx, attrValues);
}

static llvm::Value *emitAllocatingCall(IRGenFunction &IGF,
                                       llvm::Value *fn,
                                       std::initializer_list<llvm::Value*> args,
                                       const llvm::Twine &name) {
  static auto allocAttrs = getAllocAttrs(IGF.IGM.LLVMContext);
  llvm::CallInst *call =
    IGF.Builder.CreateCall(fn, makeArrayRef(args.begin(), args.size()));
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setAttributes(allocAttrs);
  return call;
}

/// Emit a 'raw' allocation, which has no heap pointer and is
/// not guaranteed to be zero-initialized.
llvm::Value *IRGenFunction::emitAllocRawCall(llvm::Value *size,
                                             llvm::Value *align,
                                             const llvm::Twine &name) {

  // Try to use swift_rawAlloc.
  if (auto csize = dyn_cast<llvm::ConstantInt>(size)) {
    AllocToken allocToken = getAllocToken(IGM, csize->getZExtValue());
    if (allocToken != InvalidAllocToken) {
      return emitAllocatingCall(*this, IGM.getRawAllocFn(),
                          { llvm::ConstantInt::get(IGM.SizeTy, allocToken) },
                                name);
    }

    assert(isa<llvm::ConstantInt>(align));
    assert(csize->getZExtValue() %cast<llvm::ConstantInt>(align)->getZExtValue()
             == 0 && "size not a multiple of alignment!");
  }

  // Okay, fall back to swift_slowAlloc.  The flags here are:
  //  0x1 - 'try', i.e. returning null is acceptable
  //  0x2 - 'raw', i.e. returning uninitialized memory is acceptable
  return emitAllocatingCall(*this, IGM.getSlowAllocFn(),
                            { size, llvm::ConstantInt::get(IGM.SizeTy, 2) },
                            name);
}

/// Emit a heap allocation.
llvm::Value *IRGenFunction::emitAllocObjectCall(llvm::Value *metadata,
                                                llvm::Value *size,
                                                llvm::Value *align,
                                                const llvm::Twine &name) {
  // For now, all we have is swift_allocObject.
  return emitAllocatingCall(*this, IGM.getAllocObjectFn(),
                            { metadata, size, align }, name);
}

static void emitDeallocatingCall(IRGenFunction &IGF, llvm::Constant *fn,
                                 llvm::Value *pointer, llvm::Value *arg) {
  llvm::CallInst *call = IGF.Builder.CreateCall2(fn, pointer, arg);
  call->setCallingConv(IGF.IGM.RuntimeCC);
  call->setDoesNotThrow();
}

/// Emit a 'raw' deallocation, which has no heap pointer and is not
/// guaranteed to be zero-initialized.
void IRGenFunction::emitDeallocRawCall(llvm::Value *pointer,
                                       llvm::Value *size) {
  // Try to use swift_rawDealloc.
  if (auto csize = dyn_cast<llvm::ConstantInt>(size)) {
    AllocToken allocToken = getAllocToken(IGM, csize->getZExtValue());
    if (allocToken != InvalidAllocToken) {
      return emitDeallocatingCall(*this, IGM.getRawDeallocFn(), pointer,
                             llvm::ConstantInt::get(IGM.SizeTy, allocToken));
    }
  }

  // Okay, fall back to swift_slowRawDealloc.
  return emitDeallocatingCall(*this, IGM.getSlowRawDeallocFn(), pointer, size);
}

/// Deallocate an object which was allocated but has not actually been
/// initialized.
void IRGenFunction::emitDeallocObjectCall(llvm::Value *ptr, llvm::Value *size) {
  // For now, all we have is swift_deallocObject.
  return emitDeallocatingCall(*this, IGM.getDeallocObjectFn(), ptr, size);
}

void IRGenFunction::unimplemented(SourceLoc Loc, StringRef Message) {
  return IGM.unimplemented(Loc, Message);
}
