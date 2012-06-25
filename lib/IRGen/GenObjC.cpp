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
#include "FixedTypeInfo.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ScalarTypeInfo.h"

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

namespace {
  /// A type-info implementation suitable for an ObjC pointer type.
  class ObjCTypeInfo : public SingleScalarTypeInfo<ObjCTypeInfo, FixedTypeInfo> {
  public:
    ObjCTypeInfo(llvm::PointerType *storageType, Size size, Alignment align)
      : SingleScalarTypeInfo(storageType, size, align, IsNotPOD) {
    }

    static const bool IsScalarPOD = false;

    void emitScalarRelease(IRGenFunction &IGF, llvm::Value *value) const {
      emitObjCReleaseCall(IGF, value);
    }

    llvm::Value *emitScalarRetain(IRGenFunction &IGF, llvm::Value *value) const {
      return emitObjCRetainCall(IGF, value);
    }

    void enterScalarCleanup(IRGenFunction &IGF, llvm::Value *value,
                            Explosion &out) const {
      return enterObjCReleaseCleanup(IGF, value, out);
    }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjCPointer() {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                          IGM.getPointerAlignment());
}

