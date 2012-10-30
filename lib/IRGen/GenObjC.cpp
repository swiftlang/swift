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
#include "HeapTypeInfo.h"
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

void IRGenFunction::emitObjCRelease(llvm::Value *value) {
  // Get an appropriately-casted function pointer.
  auto fn = IGM.getObjCReleaseFn();
  if (value->getType() != IGM.ObjCPtrTy) {
    auto fnTy = llvm::FunctionType::get(IGM.VoidTy, value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }

  auto call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
}

llvm::Value *IRGenFunction::emitObjCRetainCall(llvm::Value *value) {
  // Get an appropriately-casted function pointer.
  auto fn = IGM.getObjCRetainFn();
  if (value->getType() != IGM.ObjCPtrTy) {
    auto fnTy = llvm::FunctionType::get(value->getType(), value->getType(),
                                        false)->getPointerTo();
    fn = llvm::ConstantExpr::getBitCast(fn, fnTy);
  }

  auto call = Builder.CreateCall(fn, value);
  call->setDoesNotThrow();
  return call;
}

namespace {
  struct CallObjCRelease : Cleanup {
    llvm::Value *Value;
    CallObjCRelease(llvm::Value *value) : Value(value) {}

    void emit(IRGenFunction &IGF) const {
      IGF.emitObjCRelease(Value);
    }
  };
}

ManagedValue IRGenFunction::enterObjCReleaseCleanup(llvm::Value *value) {
  pushFullExprCleanup<CallObjCRelease>(value);
  return ManagedValue(value, getCleanupsDepth());
}

namespace {
  /// A type-info implementation suitable for an ObjC pointer type.
  class ObjCTypeInfo : public HeapTypeInfo<ObjCTypeInfo> {
  public:
    ObjCTypeInfo(llvm::PointerType *storageType, Size size, Alignment align)
      : HeapTypeInfo(storageType, size, align) {
    }

    /// Builtin.ObjCPointer requires ObjC reference-counting.
    bool hasSwiftRefcount() const { return false; }
  };
}

const TypeInfo *TypeConverter::convertBuiltinObjCPointer() {
  return new ObjCTypeInfo(IGM.ObjCPtrTy, IGM.getPointerSize(),
                          IGM.getPointerAlignment());
}

