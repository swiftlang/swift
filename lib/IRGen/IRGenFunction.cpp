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

using namespace swift;
using namespace irgen;

IRGenFunction::IRGenFunction(IRGenModule &IGM, Type t, ArrayRef<Pattern*> p,
                             ExplosionKind explosionLevel,
                             unsigned uncurryLevel, llvm::Function *Fn,
                             Prologue prologue)
  : IGM(IGM), Builder(IGM.getLLVMContext()), CurFuncType(t),
    CurFuncParamPatterns(p), CurFn(Fn),
    CurExplosionLevel(explosionLevel), CurUncurryLevel(uncurryLevel),
  CurPrologue(prologue), ContextPtr(nullptr) {
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

void IRGenFunction::unimplemented(SourceLoc Loc, StringRef Message) {
  return IGM.unimplemented(Loc, Message);
}
