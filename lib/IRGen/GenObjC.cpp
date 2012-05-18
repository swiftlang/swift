//===--- GenObjC.cpp - Objective-C interaction ----------------------------===//
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
//  This file implements bridging to Objective-C.
//
//===----------------------------------------------------------------------===//

#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"

#include "Cleanup.h"
#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

using namespace swift;
using namespace irgen;

llvm::Constant *IRGenModule::getObjCRetainFn() {
  if (ObjCRetainFn) return ObjCRetainFn;

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(ObjCPtrTy, ObjCPtrTy, false);
  ObjCRetainFn = Module.getOrInsertFunction("objc_retain", fnType);
  return ObjCRetainFn;
}

llvm::Constant *IRGenModule::getObjCReleaseFn() {
  if (ObjCReleaseFn) return ObjCReleaseFn;

  llvm::FunctionType *fnType =
    llvm::FunctionType::get(VoidTy, ObjCPtrTy, false);
  ObjCReleaseFn = Module.getOrInsertFunction("objc_release", fnType);
  return ObjCReleaseFn;
}

static void emitObjCReleaseCall(IRGenFunction &IGF, llvm::Value *value) {
  auto call = IGF.Builder.CreateCall(IGF.IGM.getObjCReleaseFn(), value);
  call->setDoesNotThrow();
}

static llvm::Value *emitObjCRetainCall(IRGenFunction &IGF, llvm::Value *value) {
  auto call = IGF.Builder.CreateCall(IGF.IGM.getObjCRetainFn(), value);
  call->setDoesNotThrow();
  return call;
}

namespace {
  struct CallObjCRelease : Cleanup {
    llvm::Value *Value;
    CallObjCRelease(llvm::Value *value) : Value(value) {}

    void emit(IRGenFunction &IGF) const {
      emitObjCReleaseCall(IGF, Value);
    }
  };
}

static void enterObjCReleaseCleanup(IRGenFunction &IGF, llvm::Value *value,
                                    Explosion &out) {
  IGF.pushFullExprCleanup<CallObjCRelease>(value);
  out.add(ManagedValue(value, IGF.getCleanupsDepth()));
}

static void emitObjCRetain(IRGenFunction &IGF, llvm::Value *value,
                           Explosion &out) {
  enterObjCReleaseCleanup(IGF, emitObjCRetainCall(IGF, value), out);
}

static void emitObjCLoadRetained(IRGenFunction &IGF, Address address,
                                 Explosion &out) {
  llvm::Value *value = IGF.Builder.CreateLoad(address);
  emitObjCRetain(IGF, value, out);
}

static void emitObjCAssignRetained(IRGenFunction &IGF, llvm::Value *newValue,
                                   Address address) {
  llvm::Value *oldValue = IGF.Builder.CreateLoad(address);
  IGF.Builder.CreateStore(newValue, address);
  emitObjCReleaseCall(IGF, oldValue);
}

namespace {
  /// A type-info implementation suitable for an ObjC pointer type.
  class ObjCTypeInfo : public TypeInfo {
  public:
    ObjCTypeInfo(llvm::PointerType *storageType, Size size, Alignment align)
      : TypeInfo(storageType, size, align, IsNotPOD) {
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 1;
    }

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forScalar(getStorageType()));
    }

    void load(IRGenFunction &IGF, Address address, Explosion &out) const {
      emitObjCLoadRetained(IGF, address, out);
    }

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &out) const {
      enterObjCReleaseCleanup(IGF, IGF.Builder.CreateLoad(addr), out);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      emitObjCAssignRetained(IGF, e.forwardNext(IGF), address);
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      IGF.Builder.CreateStore(e.forwardNext(IGF), address);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      emitObjCRetain(IGF, src.claimNext().getValue(), dest);
    }

    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitObjCReleaseCall(IGF, IGF.Builder.CreateLoad(addr));
    }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjCPointer(IRGenModule &IGM) {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                          IGM.getPointerAlignment());
}

