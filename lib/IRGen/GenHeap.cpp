//===--- GenHeap.cpp - Layout of heap objects and their metadata ----------===//
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
//  This file implements layout for heap metadata.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/GlobalVariable.h"

#include "Cleanup.h"
#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

#include "GenHeap.h"

using namespace swift;
using namespace irgen;

/// Perform the layout required for a heap object.
HeapLayout::HeapLayout(IRGenModule &IGM, LayoutStrategy strategy,
                       llvm::ArrayRef<const TypeInfo *> fields)
  : StructLayout(IGM, LayoutKind::HeapObject, strategy, fields) {
}

/// Create the destructor function for a layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Function *createDtorFn(IRGenModule &IGM,
                                    const HeapLayout &layout) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "objectdestroy", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  Address structAddr = layout.emitCastOfAlloc(IGF, fn->arg_begin());

  for (auto &field : layout.getElements()) {
    if (field.Type->isPOD(ResilienceScope::Local))
      continue;

    field.Type->destroy(IGF, field.project(IGF, structAddr));
  }

  llvm::Value *size = layout.emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

/// Create the size function for a layout.
/// TODO: give this some reasonable name and possibly linkage.
static llvm::Function *createSizeFn(IRGenModule &IGM,
                                    const HeapLayout &layout) {
  llvm::Function *fn =
    llvm::Function::Create(IGM.DtorTy, llvm::Function::InternalLinkage,
                           "objectsize", &IGM.Module);

  IRGenFunction IGF(IGM, Type(), llvm::ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  // Ignore the object pointer; we aren't a dynamically-sized array,
  // so it's pointless.

  llvm::Value *size = layout.emitSize(IGF);
  IGF.Builder.CreateRet(size);

  return fn;
}

void HeapLayout::buildMetadataInto(IRGenModule &IGM,
                    llvm::SmallVectorImpl<llvm::Constant*> &metadata) const {

  metadata.push_back(createDtorFn(IGM, *this));
  metadata.push_back(createSizeFn(IGM, *this));
}

llvm::Constant *HeapLayout::getPrivateMetadata(IRGenModule &IGM) const {
  llvm::SmallVector<llvm::Constant*, 2> metadata;
  buildMetadataInto(IGM, metadata);

  llvm::Constant *init =
    llvm::ConstantStruct::get(IGM.HeapMetadataStructTy, metadata);

  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, IGM.HeapMetadataStructTy,
                             /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage, init,
                             "metadata");
  return var;
}

llvm::Value *IRGenFunction::emitUnmanagedAlloc(const HeapLayout &layout,
                                               const llvm::Twine &name) {
  llvm::Value *metadata = layout.getPrivateMetadata(IGM);
  llvm::Value *size = layout.emitSize(*this);
  llvm::Value *align = layout.emitAlign(*this);

  llvm::Value *args[] = { metadata, size, align };
  llvm::CallInst *alloc = Builder.CreateCall(IGM.getAllocFn(), args, name);
  alloc->setDoesNotThrow();

  return alloc;
}

ManagedValue IRGenFunction::emitAlloc(const HeapLayout &layout,
                                      const llvm::Twine &name) {
  llvm::Value *alloc = emitUnmanagedAlloc(layout, name);
  return enterReleaseCleanup(alloc);
}

/// Given that an object has been allocated, cast the result to the
/// appropriate type.
Address HeapLayout::emitCastOfAlloc(IRGenFunction &IGF,
                                    llvm::Value *alloc,
                                    const llvm::Twine &name) const {
  llvm::Value *addr =
    IGF.Builder.CreateBitCast(alloc, getType()->getPointerTo(), name);
  return Address(addr, getAlignment());
}

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

    void loadAsTake(IRGenFunction &IGF, Address addr, Explosion &e) const {
      e.addUnmanaged(IGF.Builder.CreateLoad(addr));
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
  if (doesNotRequireRefCounting(value)) {
    out.addUnmanaged(value);
    return;
  }

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
