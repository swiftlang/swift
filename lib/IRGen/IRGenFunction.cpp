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

IRGenFunction::IRGenFunction(IRGenModule &IGM, Type t, ArrayRef<Pattern*> p,
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

llvm::Value *IRGenFunction::emitAllocRawCall(llvm::Value *size,
                                             llvm::Value *align) {
  // For now, always emit the most general call.  Eventually
  // we should probably have specialized entry points for specific
  // small allocations.
  llvm::CallInst *call =
    Builder.CreateCall2(IGM.getAllocRawFn(), size, align);

  // Set as nounwind noalias.
  llvm::SmallVector<llvm::AttributeWithIndex, 1> attrs;
  attrs.push_back(llvm::AttributeWithIndex::get(0, llvm::Attribute::NoAlias));
  attrs.push_back(llvm::AttributeWithIndex::get(~0, llvm::Attribute::NoUnwind));
  call->setAttributes(llvm::AttrListPtr::get(attrs.data(), attrs.size()));  

  return call;  
}

void IRGenFunction::emitDeallocRawCall(llvm::Value *pointer,
                                       llvm::Value *align) {
  // For now, always emit the most general call.  Eventually
  // we should probably have specialized entry points for specific
  // small allocations.
  llvm::CallInst *call =
    Builder.CreateCall2(IGM.getDeallocRawFn(), pointer, align);
  call->setDoesNotThrow();
}

void IRGenFunction::unimplemented(SourceLoc Loc, StringRef Message) {
  return IGM.unimplemented(Loc, Message);
}
