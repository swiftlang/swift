//===--- GenRefCount.cpp - IR Generation for Reference-Counting -----------===//
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
//  This file implements IR generation to support the
//  reference-counting model.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/LLVM.h"
#include "llvm/DerivedTypes.h"
#include "Cleanup.h"
#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

namespace {
  class RefCountedTypeInfo : public TypeInfo {
  public:
    RefCountedTypeInfo(llvm::PointerType *storage, Size size, Alignment align)
      : TypeInfo(storage, size, align, IsNotPOD) {}

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    void load(IRGenFunction &IGF, Address addr, Explosion &e) const {
      IGF.emitLoadAndRetain(addr, e);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.emitAssignRetained(e.forwardNext(IGF), addr);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.emitInitializeRetained(e.forwardNext(IGF), addr);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      llvm::Value *value = IGF.Builder.CreateLoad(addr);
      IGF.emitRelease(value);
    }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjectPointer(IRGenModule &IGM) {
  return new RefCountedTypeInfo(IGM.RefCountedPtrTy,
                                IGM.getPointerSize(),
                                IGM.getPointerAlignment());
}

/// Does the given value superficially not require reference-counting?
static bool doesNotRequireRefCounting(llvm::Value *value) {
  // Constants never require reference-counting.
  return isa<llvm::Constant>(value);
}

/// Emit a call to swift_retain.
static void emitRetainCall(IRGenFunction &IGF, llvm::Value *value,
                           Explosion &out) {
  // Instead of casting the input, retaining, and casting back, we
  // cast the function to the right type.  This tends to produce less
  // IR, but it might be evil.
  llvm::Constant *fn = IGF.IGM.getRetainFn();
  if (value->getType() != IGF.IGM.RefCountedPtrTy) {
    llvm::FunctionType *fnType =
      llvm::FunctionType::get(value->getType(), value->getType(), false);
    fn = llvm::ConstantExpr::getBitCast(fn, fnType->getPointerTo());
  }

  // Emit the call.
  llvm::CallInst *call = IGF.Builder.CreateCall(fn, value);
  call->setDoesNotThrow();

  out.add(IGF.enterReleaseCleanup(call));
}

/// Emit a retain of a value.  This is usually not required because
/// values in explosions are typically "live", i.e. have a +1 owned by
/// the explosion.
void IRGenFunction::emitRetain(llvm::Value *value, Explosion &out) {
  if (doesNotRequireRefCounting(value)) return;

  emitRetainCall(*this, value, out);
}

/// Emit a load of a live value from the given retaining variable.
void IRGenFunction::emitLoadAndRetain(Address address, Explosion &out) {
  llvm::Value *value = Builder.CreateLoad(address);
  emitRetainCall(*this, value, out);
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

/// Emit a call to swift_dealloc.
static void emitDeallocCall(IRGenFunction &IGF, llvm::Value *allocation) {
  llvm::Constant *fn = IGF.IGM.getDeallocFn();
  if (allocation->getType() != IGF.IGM.RefCountedPtrTy) {
    llvm::FunctionType *fnType =
      llvm::FunctionType::get(IGF.IGM.VoidTy, allocation->getType(), false);
    fn = llvm::ConstantExpr::getBitCast(fn, fnType->getPointerTo());
  }

  llvm::CallInst *call = IGF.Builder.CreateCall(fn, allocation);
  call->setDoesNotThrow();
}

namespace {
  class CallDealloc : public Cleanup {
    llvm::Value *Allocation;
  public:
    CallDealloc(llvm::Value *allocation) : Allocation(allocation) {}
    void emit(IRGenFunction &IGF) const {
      emitDeallocCall(IGF, Allocation);
    }
  };
}

/// Enter a cleanup to call swift_dealloc on the given pointer.
/// This cleanup will usually be deactivated as soon as the
/// initializer completes.
IRGenFunction::CleanupsDepth
IRGenFunction::pushDeallocCleanup(llvm::Value *allocation) {
  pushFullExprCleanup<CallDealloc>(allocation);
  return getCleanupsDepth();
}
