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

#include "Explosion.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "GenType.h"

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
