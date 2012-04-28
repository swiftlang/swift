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

#include "GenClosure.h"

#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "GenHeap.h"
#include "GenInit.h"
#include "GenType.h"
#include "LValue.h"

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

  if (Patterns.size() != 1) {
    IGF.unimplemented(E->getLoc(), "curried local functions");
    return IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                                 explosion);
  }

  for (ValueDecl *D : E->getCaptures()) {
    if (!isa<VarDecl>(D) && !isa<FuncDecl>(D)) {
      IGF.unimplemented(E->getLoc(), "capturing non-variables");
      return IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                                  explosion);
    }
  }

  bool HasCaptures = !E->getCaptures().empty();

  // Create the IR function.
  llvm::FunctionType *fnType =
      IGF.IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0,
                              HasCaptures);
  llvm::Function *fn =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGF.IGM.Module);

  IRGenFunction innerIGF(IGF.IGM, E->getType(), Patterns,
                         ExplosionKind::Minimal, /*uncurry level*/ 0, fn,
                         HasCaptures ? Prologue::StandardWithContext :
                                       Prologue::Standard);

  ManagedValue contextPtr(IGF.IGM.RefCountedNull);

  // There are two places we need to generate code for captures: in the
  // current function, to store the captures to a capture block, and in the
  // inner function, to load the captures from the capture block.
  if (HasCaptures) {
    SmallVector<const TypeInfo *, 4> Fields;
    for (ValueDecl *D : E->getCaptures()) {
      Type RefTy = D->getTypeOfReference();
      const TypeInfo &typeInfo = IGF.getFragileTypeInfo(RefTy);
      Fields.push_back(&typeInfo);
    }
    HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, Fields);

    // Allocate the capture block.
    contextPtr = IGF.emitAlloc(layout, "closure-data.alloc");
    
    Address CaptureStruct =
      layout.emitCastOfAlloc(IGF, contextPtr.getValue(), "closure-data");
    Address InnerStruct =
      layout.emitCastOfAlloc(innerIGF, innerIGF.ContextPtr, "closure-data");

    // Emit stores and loads for capture block
    for (unsigned i = 0, e = E->getCaptures().size(); i != e; ++i) {
      // FIXME: avoid capturing owner when this is obviously derivable.

      ValueDecl *D = E->getCaptures()[i];
      auto &elt = layout.getElements()[i];

      if (isa<FuncDecl>(D)) {
        Explosion OuterExplosion(ExplosionKind::Maximal);
        Address Func = IGF.getLocalFunc(cast<FuncDecl>(D));
        elt.Type->load(IGF, Func, OuterExplosion);
        Address CaptureAddr = elt.project(IGF, CaptureStruct);
        elt.Type->initialize(IGF, OuterExplosion, CaptureAddr);

        Address InnerAddr = elt.project(innerIGF, InnerStruct);
        innerIGF.setLocalFunc(cast<FuncDecl>(D), InnerAddr);
        continue;
      }

      Explosion OuterExplosion(ExplosionKind::Maximal);
      OwnedAddress Var = IGF.getLocalVar(cast<VarDecl>(D));
      IGF.emitLValueAsScalar(IGF.emitAddressLValue(Var),
                             OnHeap, OuterExplosion);
      Address CaptureAddr = elt.project(IGF, CaptureStruct);
      elt.Type->initialize(IGF, OuterExplosion, CaptureAddr);

      Address InnerAddr = elt.project(innerIGF, InnerStruct);
      Address InnerValueAddr =
        innerIGF.Builder.CreateStructGEP(InnerAddr, 0, Size(0));
      Address InnerOwnerAddr =
        innerIGF.Builder.CreateStructGEP(InnerAddr, 1, IGF.IGM.getPointerSize());
      Address InnerValue(innerIGF.Builder.CreateLoad(InnerValueAddr),
                         Var.getAddress().getAlignment());
      OwnedAddress InnerLocal(InnerValue,
                              innerIGF.Builder.CreateLoad(InnerOwnerAddr));
      innerIGF.setLocalVar(cast<VarDecl>(D), InnerLocal);
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
  explosion.addUnmanaged(IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy));
  explosion.add(contextPtr);
}

/// Emit a function declaration, starting at the given uncurry level.
void IRGenFunction::emitLocalFunction(FuncDecl *func) {
  // Nothing to do if the function has no body.
  if (!func->getBody()) return;
  FuncExpr *funcExpr = func->getBody();

  Initialization I;
  auto object = I.getObjectForDecl(func);
  const TypeInfo &type = getFragileTypeInfo(func->getType());
  I.registerObject(*this, object, NotOnHeap, type);
  OwnedAddress addr =
    I.emitLocalAllocation(*this, object, NotOnHeap, type,
                          func->getName().str());

  Explosion e(ExplosionKind::Maximal);
  emitClosure(*this, funcExpr, e);
  type.initialize(*this, e, addr);

  setLocalFunc(func, addr);
}

