//===--- GenInit.cpp - IR Generation for Initialization -------------------===//
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
//  This file implements IR generation for the initialization of
//  local and global variables.
//
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Optional.h"
#include "swift/AST/Pattern.h"
#include "llvm/IR/GlobalVariable.h"

#include "ASTVisitor.h"
#include "Cleanup.h"
#include "Explosion.h"
#include "GenHeap.h"
#include "GenTuple.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;


/// Emit a global variable.
Address IRGenModule::emitGlobalVariable(VarDecl *var,
                                        const TypeInfo &type) {
  // If the variable is empty, don't actually emit it; just return undef.
  // FIXME: global destructors?
  if (type.isKnownEmpty()) {
    auto undef = llvm::UndefValue::get(type.StorageType->getPointerTo());
    return type.getAddressForPointer(undef);
  }
  
  /// Get the global variable.
  Address addr = getAddrOfGlobalVariable(var);
  
  // Add a zero-initializer.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.getStorageType()));

  return addr;
}

/// Create an allocation for an empty object.
static OwnedAddress createEmptyAlloca(IRGenModule &IGM, const TypeInfo &type) {
  llvm::Value *badPointer =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return OwnedAddress(type.getAddressForPointer(badPointer),
                      IGM.RefCountedNull);
}


/// Allocate an object with fixed layout.
OwnedAddress FixedTypeInfo::allocate(IRGenFunction &IGF, OnHeap_t onHeap,
                                     const Twine &name) const {
  // If the type is known to be empty, don't actually allocate anything.
  if (isKnownEmpty())
    return createEmptyAlloca(IGF.IGM, *this);

  // If the object does not need to be allocated on the heap,
  // allocate it on the stack.
  if (!onHeap) {
    Address rawAddr =
      IGF.createAlloca(getStorageType(), getFixedAlignment(), name);
    // TODO: lifetime intrinsics?

    OwnedAddress addr(rawAddr, IGF.IGM.RefCountedNull);
    return addr;
  }

  // Lay out the type as a heap object.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, this);
  assert(!layout.empty() && "non-empty type had empty layout?");
  auto &elt = layout.getElements()[0];

  // Allocate a new object.
  // TODO: lifetime intrinsics?
  llvm::Value *allocation = IGF.emitUnmanagedAlloc(layout, name + ".alloc");

  // Cast and GEP down to the element.
  Address rawAddr = layout.emitCastTo(IGF, allocation);
  rawAddr = elt.project(IGF, rawAddr, name);

  OwnedAddress addr(rawAddr, allocation);
  return addr;
}

