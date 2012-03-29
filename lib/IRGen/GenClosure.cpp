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

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "StructLayout.h"

#include "GenClosure.h"

using namespace swift;
using namespace irgen;

void swift::irgen::emitClosure(IRGenFunction &IGF, CapturingExpr *E,
                               Explosion &explosion) {
  assert(isa<FuncExpr>(E) || isa<ClosureExpr>(E));

  ArrayRef<Pattern*> Patterns;
  if (FuncExpr *FE = dyn_cast<FuncExpr>(E))
    Patterns = FE->getParamPatterns();
  else
    Patterns = cast<ClosureExpr>(E)->getParamPatterns();

  // Create the IR function.
  llvm::FunctionType *fnType =
      IGF.IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0, true);
  llvm::Function *fn =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGF.IGM.Module);

  IRGenFunction innerIGF(IGF.IGM, E->getType(), Patterns,
                         ExplosionKind::Minimal, /*uncurry level*/ 0, fn,
                         Prologue::StandardWithContext);

  llvm::Value *ContextPtr = IGF.IGM.RefCountedNull;

  // There are three places we need to generate code for captures: in the
  // current function, to store the captures to a capture block; in the inner
  // function, to load the captures from the capture block; and the destructor
  // for the capture block.
  // FIXME: Not generating the destructor for the capture block yet; figure out
  // how to do that once we actually have destructors.
  if (!E->getCaptures().empty()) {
    SmallVector<const TypeInfo *, 4> Fields;
    for (ValueDecl *D : E->getCaptures()) {
      Type RefTy = LValueType::get(D->getType(),
                                   LValueType::Qual::DefaultForVar,
                                   IGF.IGM.Context);
      const TypeInfo &typeInfo = IGF.getFragileTypeInfo(RefTy);
      Fields.push_back(&typeInfo);
    }
    StructLayout layout(IGF.IGM, LayoutKind::HeapObject,
                        LayoutStrategy::Optimal, Fields);

    // Allocate the capture block
    llvm::Value *size =
        llvm::ConstantInt::get(IGF.IGM.SizeTy, layout.getSize().getValue());
    llvm::CallInst *allocation =
        IGF.Builder.CreateCall(IGF.IGM.getAllocFn(), size);
    allocation->setDoesNotThrow();
    allocation->setName(".capturealloc");
    ContextPtr = allocation;
    llvm::Type *CapturesType = layout.getType()->getPointerTo();
    llvm::Value *CaptureStruct =
        IGF.Builder.CreateBitCast(allocation, CapturesType);
    llvm::Value *InnerStruct =
        innerIGF.Builder.CreateBitCast(innerIGF.ContextPtr, CapturesType);

    // Emit stores and loads for capture block
    for (unsigned i = 0, e = E->getCaptures().size(); i != e; ++i) {
      ValueDecl *D = E->getCaptures()[i];
      OwnedAddress Var = IGF.getLocal(D);
      unsigned StructIdx = layout.getElements()[i].StructIndex;
      llvm::Value *CaptureAddr =
        IGF.Builder.CreateStructGEP(CaptureStruct, StructIdx);
      llvm::Value *CaptureValueAddr =
        IGF.Builder.CreateStructGEP(CaptureAddr, 0);
      llvm::Value *CaptureOwnerAddr =
          IGF.Builder.CreateStructGEP(CaptureAddr, 1);
      IGF.Builder.CreateStore(Var.getOwner(), CaptureOwnerAddr);
      IGF.Builder.CreateStore(Var.getAddressPointer(), CaptureValueAddr);

      llvm::Value *InnerAddr =
        innerIGF.Builder.CreateStructGEP(InnerStruct, StructIdx);
      llvm::Value *InnerValueAddr =
        innerIGF.Builder.CreateStructGEP(InnerAddr, 0);
      llvm::Value *InnerOwnerAddr =
          innerIGF.Builder.CreateStructGEP(InnerAddr, 1);
      Address InnerValue(innerIGF.Builder.CreateLoad(InnerValueAddr),
                         Var.getAddress().getAlignment());
      OwnedAddress InnerLocal(InnerValue,
                              innerIGF.Builder.CreateLoad(InnerOwnerAddr));
      innerIGF.setLocal(D, InnerLocal);
    }
  }

  if (FuncExpr *FE = dyn_cast<FuncExpr>(E)) {
    innerIGF.emitFunctionTopLevel(FE->getBody());
  } else {
    // Emit the body of the closure as if it were a single return
    // statement.
    ReturnStmt ret(SourceLoc(), cast<ClosureExpr>(E)->getBody());
    innerIGF.emitStmt(&ret);
  }

  // Build the explosion result.
  explosion.add(IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy));
  explosion.add(ContextPtr);
}
