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

#include "llvm/IR/Instructions.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"
#include "swift/Basic/SourceLoc.h"

#include "Explosion.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"

using namespace swift;
using namespace irgen;

IRGenFunction::IRGenFunction(IRGenModule &IGM,
                             ExplosionKind explosionLevel,
                             llvm::Function *Fn)
  : IGM(IGM), Builder(IGM.getLLVMContext()),
    CurFn(Fn), CurExplosionLevel(explosionLevel),
    ContextPtr(nullptr) {
  emitPrologue();
}

IRGenFunction::~IRGenFunction() {
  emitEpilogue();
}

/// Call the llvm.memcpy intrinsic.  The arguments need not already
/// be of i8* type.
void IRGenFunction::emitMemCpy(llvm::Value *dest, llvm::Value *src,
                               Size size, Alignment align) {
  emitMemCpy(dest, src, IGM.getSize(size), align);
}

void IRGenFunction::emitMemCpy(llvm::Value *dest, llvm::Value *src,
                               llvm::Value *size, Alignment align) {
  Builder.CreateMemCpy(dest, src, size, align.getValue(), false);
}

void IRGenFunction::emitMemCpy(Address dest, Address src, Size size) {
  emitMemCpy(dest, src, IGM.getSize(size));
}

void IRGenFunction::emitMemCpy(Address dest, Address src, llvm::Value *size) {
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
  auto attrs = llvm::AttributeSet::get(ctx,
                                       llvm::AttributeSet::ReturnIndex,
                                       llvm::Attribute::NoAlias);
  attrs = attrs.addAttribute(ctx,
                             llvm::AttributeSet::FunctionIndex,
                             llvm::Attribute::NoUnwind);
  return attrs;
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
                                             llvm::Value *alignMask,
                                             const llvm::Twine &name) {

  // Try to use swift_rawAlloc.
  if (auto csize = dyn_cast<llvm::ConstantInt>(size)) {
    AllocToken allocToken = getAllocToken(IGM, csize->getZExtValue());
    if (allocToken != InvalidAllocToken) {
      return emitAllocatingCall(*this, IGM.getRawAllocFn(),
                          { llvm::ConstantInt::get(IGM.SizeTy, allocToken) },
                                name);
    }

    assert(isa<llvm::ConstantInt>(alignMask));
    assert(csize->getZExtValue() %
               (cast<llvm::ConstantInt>(alignMask)->getZExtValue() + 1)
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
                                                llvm::Value *alignMask,
                                                const llvm::Twine &name) {
  // For now, all we have is swift_allocObject.
  return emitAllocatingCall(*this, IGM.getAllocObjectFn(),
                            { metadata, size, alignMask }, name);
}

void IRGenFunction::emitAllocBoxCall(llvm::Value *typeMetadata,
                                     llvm::Value *&box,
                                     llvm::Value *&valueAddress) {
  auto attrs = llvm::AttributeSet::get(IGM.LLVMContext,
                                       llvm::AttributeSet::FunctionIndex,
                                       llvm::Attribute::NoUnwind);
  
  llvm::CallInst *call =
    Builder.CreateCall(IGM.getAllocBoxFn(), typeMetadata);
  call->setCallingConv(IGM.RuntimeCC);
  call->setAttributes(attrs);

  box = Builder.CreateExtractValue(call, 0);
  valueAddress = Builder.CreateExtractValue(call, 1);
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


void IRGenFunction::unimplemented(SourceLoc Loc, StringRef Message) {
  return IGM.unimplemented(Loc, Message);
}

// Debug output for Explosions.

void Explosion::print(llvm::raw_ostream &OS) {
  for (auto value : makeArrayRef(Values).slice(NextValue)) {
    value->print(OS);
    OS << '\n';
  }
}

void Explosion::dump() {
  print(llvm::errs());
}
