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
#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "RValue.h"

using namespace swift;
using namespace irgen;

namespace {
  class RefCountedTypeInfo : public TypeInfo {
  public:
    RefCountedTypeInfo(llvm::PointerType *storage, Size size, Alignment align)
      : TypeInfo(storage, size, align) {}

    RValueSchema getSchema() const {
      return RValueSchema::forScalars(getStorageType());
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getExplosionSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    RValue load(IRGenFunction &IGF, Address addr) const {
      return RValue::forScalars(IGF.emitLoadRetained(addr));
    }

    void loadExplosion(IRGenFunction &IGF, Address addr, Explosion &e) const {
      e.add(IGF.emitLoadRetained(addr));
    }

    void store(IRGenFunction &IGF, const RValue &RV, Address addr) const {
      assert(RV.isScalar() && RV.getScalars().size() == 1);
      IGF.emitStoreRetained(RV.getScalars()[0], addr);
    }

    void storeExplosion(IRGenFunction &IGF, Explosion &e, Address addr) const {
      IGF.emitStoreRetained(e.claimNext(), addr);
    }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjectPointer(IRGenModule &IGM) {
  return new RefCountedTypeInfo(IGM.RefCountedPtrTy,
                                IGM.getPointerSize(),
                                IGM.getPointerAlignment());
}

/// Emit a load of a live value from the given retaining variable.
llvm::Value *IRGenFunction::emitLoadRetained(Address address) {
  llvm::Value *value = Builder.CreateLoad(address);

  llvm::Constant *fn = IGM.getRetainFn();
  if (value->getType() != IGM.RefCountedPtrTy) {
    llvm::FunctionType *fnType =
      llvm::FunctionType::get(value->getType(), value->getType(), false);
    fn = llvm::ConstantExpr::getBitCast(fn, fnType->getPointerTo());
  }

  llvm::CallInst *call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();

  // FIXME: enter an EH cleanup to balance this out!
  return call;
}

/// Emit a store of a live value to the given retaining variable.
void IRGenFunction::emitStoreRetained(llvm::Value *newValue, Address address) {
  // Pull the old value out of the address.
  llvm::Value *oldValue = Builder.CreateLoad(address);

  // We assume the new value is already retained.
  Builder.CreateStore(newValue, address);

  // Release the old value.
  emitRelease(oldValue);
}

/// Emit a release of a live value.
void IRGenFunction::emitRelease(llvm::Value *value) {
  llvm::Constant *fn = IGM.getReleaseFn();
  if (value->getType() != IGM.RefCountedPtrTy) {
    llvm::FunctionType *fnType =
      llvm::FunctionType::get(IGM.VoidTy, value->getType(), false);
    fn = llvm::ConstantExpr::getBitCast(fn, fnType->getPointerTo());
  }

  llvm::CallInst *call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
}

/// Enter a cleanup to release an object.
void IRGenFunction::enterReleaseCleanup(llvm::Value *value) {
  // FIXME: implement
}

void IRGenFunction::popCleanup() {
  // FIXME: implement
}
