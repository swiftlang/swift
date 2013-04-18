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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "swift/AST/Types.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "CallingConvention.h"
#include "Explosion.h"
#include "FunctionRef.h"
#include "GenHeap.h"
#include "GenInit.h"
#include "TypeInfo.h"

using namespace swift;
using namespace irgen;

namespace {
  class CaptureInfo {
  public:
    // Captured functions which do not require data pointers.
    SmallVector<FuncDecl*, 4> TrivialFunctions;

    // Captures that do require interesting work, and (in parallel)
    // the types by which they need to be captured.
    SmallVector<Decl*, 4> Captures;
    SmallVector<const TypeInfo *, 4> CaptureFields;

    CaptureInfo(IRGenFunction &IGF, CapturingExpr *E);

    bool requiresData() const { return (!Captures.empty()); }

  private:
    void captureFunc(IRGenFunction &IGF, FuncDecl *fn);
    void captureVar(IRGenFunction &IGF, VarDecl *var);

    void addCapture(IRGenModule &IGM, Decl *capture, Type storageType) {
      addCapture(capture, IGM.getFragileTypeInfo(storageType));
    }

    void addCapture(Decl *capture, const TypeInfo &storageTI) {
      Captures.push_back(capture);
      CaptureFields.push_back(&storageTI);
    }
  };
}

CaptureInfo::CaptureInfo(IRGenFunction &outerIGF, CapturingExpr *E) {
  // Come up with a capturing strategy for all the captures.
  for (auto capture : E->getCaptures()) {
    if (auto var = dyn_cast<VarDecl>(capture)) {
      captureVar(outerIGF, var);
    } else if (auto fn = dyn_cast<FuncDecl>(capture)) {
      captureFunc(outerIGF, fn);
    } else {
      outerIGF.unimplemented(E->getLoc(), "capturing non-variables");
      // Just ignore it.  This is probably not best.
    }
  }
}

void CaptureInfo::captureFunc(IRGenFunction &outerIGF, FuncDecl *fn) {
  // Functions can be trivial if they don't require data.
  llvm::Value *fnData = outerIGF.getLocalFuncData(fn);
  if (isa<llvm::ConstantPointerNull>(fnData)) {
    TrivialFunctions.push_back(fn);
    return;
  }

  // Otherwise we need to capture the data (but that's it; no need to
  // save the function pointer as well).
  IRGenModule &IGM = outerIGF.IGM;
  addCapture(IGM, fn, IGM.Context.TheObjectPointerType);
}

void CaptureInfo::captureVar(IRGenFunction &outerIGF, VarDecl *var) {
  // Everything else gets captured according to its type.
  // TODO: capture variables by value when feasible.
  // TODO: avoid capturing the 'owner' of variables when this
  // is obviously derivable.
  addCapture(outerIGF.IGM, var, var->getTypeOfReference());
}

/// Emit the data pointer for a capturing expression.
static ManagedValue emitClosureData(IRGenFunction &IGF, CapturingExpr *E,
                                    const CaptureInfo &info,
                                    const HeapLayout &layout) {
  assert(info.requiresData());
  ManagedValue data;

  // Allocate the capture block.
  if (E->isNotCaptured()) {
    // FIXME: This doesn't compute the heap metadata pointer; do we need it?
    // FIXME: This should be refactored if anything else has a use for it.
    Address allocatedPtr = IGF.createAlloca(layout.getType(),
                                            layout.getAlignment(),
                                            "closuretemp");
    Address refCntPtr =
      IGF.Builder.CreateStructGEP(allocatedPtr, 1, IGF.IGM.getPointerSize());
    IGF.Builder.CreateStore(IGF.Builder.getInt64(2), refCntPtr);
    data = ManagedValue(IGF.Builder.CreateBitCast(allocatedPtr.getAddress(),
                                                  IGF.IGM.RefCountedPtrTy));
  } else {
    data = IGF.emitAlloc(layout, "closure-data.alloc");
  }
    
  Address object = layout.emitCastTo(IGF, data.getValue(), "closure-data");

  // Emit stores and loads for objects actually captured.
  for (unsigned i = 0, e = info.Captures.size(); i != e; ++i) {
    Decl *D = info.Captures[i];
    auto &elt = layout.getElements()[i];

    Address capture = elt.project(IGF, object);

    // If this a function, map the data pointer.
    if (auto capturedFunc = dyn_cast<FuncDecl>(D)) {
      llvm::Value *data = IGF.getLocalFuncData(capturedFunc);
      assert(!isa<llvm::ConstantPointerNull>(data));
        
      // In the outer function, retain the data and use that to
      // initialize the capture.
      Explosion temp(ExplosionKind::Maximal);
      IGF.emitRetain(data, temp);
      elt.Type->initialize(IGF, temp, capture);

      continue;
    }

    // Otherwise, we have a variable.
    VarDecl *capturedVar = cast<VarDecl>(D);

    // In the outer function, take the address and copy it into place.
    OwnedAddress outerVar = IGF.getLocalVar(capturedVar);
    Explosion temp(ExplosionKind::Maximal);
    temp.addUnmanaged(outerVar.getAddressPointer());
    IGF.emitRetain(outerVar.getOwner(), temp);
    elt.Type->initialize(IGF, temp, capture);
  }

  return data;
}

static HeapLayout getHeapLayout(IRGenModule &IGM, const CaptureInfo &info) {
  return HeapLayout(IGM, LayoutStrategy::Optimal, info.CaptureFields);
}

/// Emit the body of a local function.
///
/// \param definingIGF - the IGF for the function in which this local
///   function is directly defined
/// \param fn - the function for which to create the body
static void emitLocalFunctionBody(IRGenFunction &definingIGF,
                                  llvm::Function *fn,
                                  CapturingExpr *E,
                                  ExplosionKind explosionLevel,
                                  unsigned uncurryLevel,
                                  const CaptureInfo &info,
                                  const HeapLayout *layout) {
  assert(0);
  IRGenModule &IGM = definingIGF.IGM;
  assert(info.requiresData() == (layout != nullptr));

  ArrayRef<Pattern*> patterns;
  if (FuncExpr *func = dyn_cast<FuncExpr>(E))
    patterns = func->getBodyParamPatterns();
  else
    patterns = cast<ClosureExpr>(E)->getParamPatterns();

  if (patterns.size() != uncurryLevel + 1) {
    // Just leave it as a forward declaration.
    IGM.unimplemented(E->getLoc(), "curried entrypoints for local function");
    return;
  }

  IRGenFunction IGF(IGM, E->getType()->getCanonicalType(), patterns,
                    explosionLevel, uncurryLevel, fn,
                    info.requiresData() ? Prologue::StandardWithContext
                                        : Prologue::Standard);

  // Map all the trivial captured functions.
  for (auto capturedFunc : info.TrivialFunctions) {
    IGF.setLocalFuncData(capturedFunc, IGM.RefCountedNull, nullptr);
  }

  // Map all the non-trivial captures.
  if (info.requiresData()) {
    Address data = layout->emitCastTo(IGF, IGF.ContextPtr, "closure-data");

    // Emit stores and loads for objects actually captured.
    for (unsigned i = 0, e = info.Captures.size(); i != e; ++i) {
      Decl *D = info.Captures[i];
      auto &elt = layout->getElements()[i];

      Address capture = elt.project(IGF, data);

      // If this a function, map the data pointer.
      if (auto capturedFunc = dyn_cast<FuncDecl>(D)) {
        // In the inner function, load unretained.
        // FIXME: This probably needs to be retained so that the optimizer
        // won't get confused.
        auto value = IGF.Builder.CreateLoad(capture,
                                    capturedFunc->getName().str() + ".data");
        IGF.setLocalFuncData(capturedFunc, value,
                             definingIGF.getLocalFuncDefiner(capturedFunc));
        continue;
      }

      // Otherwise, we have a variable.
      VarDecl *capturedVar = cast<VarDecl>(D);

      // In the inner function, load unretained.
      // FIXME: This probably needs to be retained so that the optimizer
      // won't get confused.
      llvm::Value *addr =
          IGF.Builder.CreateLoad(
            IGF.Builder.CreateStructGEP(capture, 0, Size(0)));
      llvm::Value *owner =
          IGF.Builder.CreateLoad(
            IGF.Builder.CreateStructGEP(capture, 1, IGM.getPointerSize()));
      Alignment alignment =
        definingIGF.getLocalVar(capturedVar).getAlignment();
      OwnedAddress innerVar(Address(addr, alignment), owner);
      IGF.setLocalVar(capturedVar, innerVar);
    }
  }
}

/// Emit an anonymous closure expression.
void irgen::emitClosure(IRGenFunction &IGF, CapturingExpr *E, Explosion &out) {
  assert(isa<FuncExpr>(E) || isa<ClosureExpr>(E));

  // Compute the closure information.
  CaptureInfo info(IGF, E);

  // The specs for an indirect call.
  ExplosionKind explosionLevel = ExplosionKind::Minimal;
  unsigned uncurryLevel = 0;

  // Create the LLVM function declaration.
  llvm::AttributeSet attrs;
  llvm::FunctionType *fnType =
    IGF.IGM.getFunctionType(AbstractCC::Freestanding,
                            E->getType()->getCanonicalType(),
                            explosionLevel, uncurryLevel,
                            info.requiresData() ? ExtraData::Retainable
                                                : ExtraData::None,
                            attrs);
  llvm::Function *fn =
    llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                           "closure", &IGF.IGM.Module);
  fn->setAttributes(attrs);
  
  ManagedValue data;
  if (!info.requiresData()) {
    emitLocalFunctionBody(IGF, fn, E, explosionLevel, uncurryLevel,
                          info, /*layout*/ nullptr);
    data = ManagedValue(IGF.IGM.RefCountedNull);
  } else {
    HeapLayout layout = getHeapLayout(IGF.IGM, info);
    emitLocalFunctionBody(IGF, fn, E, explosionLevel, uncurryLevel,
                          info, &layout);
    data = emitClosureData(IGF, E, info, layout);
  }

  out.addUnmanaged(IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy));
  out.add(data);
}


/// Get or create a definition for the given local function, emitting
/// it if necessary.  The IGF doesn't need to be for the function
/// which actually declares the function; it can also be for any
/// function which has the function and all its captured declarations
/// mapped.
llvm::Function *
IRGenFunction::getAddrOfLocalFunction(FunctionRef fnRef) {
  FuncDecl *func = cast<FuncDecl>(fnRef.getDecl());
  assert(func->getBody() && "local function without a body!");

  llvm::Value *data = getLocalFuncData(func);
  bool needsData = !isa<llvm::ConstantPointerNull>(data);
  ExtraData extraData = (needsData ? ExtraData::Retainable : ExtraData::None);

  // Find the function pointer.
  llvm::Function *fn = IGM.getAddrOfFunction(fnRef, extraData);

  // If we already have a function body, we're set.
  if (!fn->empty()) return fn;

  // Find the defining IGF.
  IRGenFunction *definingIGF = getLocalFuncDefiner(func);
  assert(definingIGF && "no defining IGF for function that requires data!");

  // Recompute capture information in that context.
  CaptureInfo info(*definingIGF, func->getBody());
  assert(info.requiresData() == needsData);

  if (!needsData) {
    emitLocalFunctionBody(*definingIGF, fn, func->getBody(),
                          fnRef.getExplosionLevel(), fnRef.getUncurryLevel(),
                          info, nullptr);
  } else {
    HeapLayout layout = getHeapLayout(IGM, info);

    // Emit the function body.
    emitLocalFunctionBody(*definingIGF, fn, func->getBody(),
                          fnRef.getExplosionLevel(), fnRef.getUncurryLevel(),
                          info, &layout);
  }

  return fn;
}
