//===--- GenClosure.cpp - Miscellaneous IR Generation for Expressions -----===//
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
//  This file implements IR generation for closures, e.g. ImplicitClosureExpr.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

#include "GenClosure.h"

using namespace swift;
using namespace irgen;

void swift::irgen::emitClosure(IRGenFunction &IGF, ClosureExpr *E,
                               Explosion &explosion) {
  if (!E->getCaptures().empty()) {
    IGF.unimplemented(E->getLoc(), "cannot capture local vars yet");
    return IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                                 explosion);
  }

  // Create the IR function.
  llvm::FunctionType *fnType =
      IGF.IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0, false);
  llvm::Function *fn =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGF.IGM.Module);

  IRGenFunction innerIGF(IGF.IGM, E->getType(), E->getParamPatterns(),
                         ExplosionKind::Minimal, /*uncurry level*/ 0, fn);

  llvm::Value *ContextPtr = IGF.IGM.RefCountedNull;

  // There are three places we need to generate code for captures: in the
  // current function, to store the captures to a capture block; in the inner
  // function, to load the captures from the capture block; and the destructor
  // for the capture block.
  // FIXME: Not generating the destructor for the capture block yet; figure out
  // how to do that once we actually have destructors.
  if (!E->getCaptures().empty()) {
    // First, compute the size of the capture block
    // FIXME: This is completely hacky
    unsigned NumPointers = E->getCaptures().size();
    llvm::Value *size =
      llvm::ConstantInt::get(IGF.IGM.SizeTy, NumPointers * 16);

    // Allocate the capture block
    llvm::CallInst *allocation =
      IGF.Builder.CreateCall(IGF.IGM.getAllocFn(), size);
    allocation->setDoesNotThrow();
    allocation->setName(".capturealloc");
    ContextPtr = allocation;
    llvm::Type *CapturesType = IGF.IGM.RefCountedPtrTy->getPointerTo();
    llvm::Value *CaptureArray =
        IGF.Builder.CreateBitCast(allocation, CapturesType);

    // Emit stores and loads for capture block
    for (unsigned i = 0, e = E->getCaptures().size(); i != e; ++i) {
      ValueDecl *D = E->getCaptures()[i];
      OwnedAddress Var = IGF.getLocal(D);
      llvm::Value *CaptureOwnerAddr =
          IGF.Builder.CreateConstInBoundsGEP1_32(CaptureArray, 2 * i);
      llvm::Value *CaptureValueAddr =
          IGF.Builder.CreateConstInBoundsGEP1_32(CaptureArray, 2 * i + 1);
      llvm::Type *ValueAddrTy = Var.getAddress()->getType()->getPointerTo();
      CaptureValueAddr = IGF.Builder.CreateBitCast(CaptureValueAddr, ValueAddrTy);
      IGF.Builder.CreateStore(Var.getOwner(), CaptureOwnerAddr);
      IGF.Builder.CreateStore(Var.getAddressPointer(), CaptureValueAddr);

      // FIXME: Stub out inner var for the moment.
      llvm::Value *InnerOwnerAddr = 0;
      llvm::Value *InnerValueAddr =
          llvm::Constant::getNullValue(Var.getAddress()->getType());
      Address InnerValue(InnerValueAddr, Var.getAddress().getAlignment());
      OwnedAddress InnerLocal = OwnedAddress(InnerValue, InnerOwnerAddr);
      innerIGF.setLocal(D, InnerLocal);
    }
  }

  // Emit the body of the closure as if it were a single return
  // statement.
  ReturnStmt ret(SourceLoc(), E->getBody());
  innerIGF.emitStmt(&ret);

  // Build the explosion result.
  explosion.add(IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy));
  explosion.add(ContextPtr);
}
