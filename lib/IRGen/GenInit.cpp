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

#include "swift/AST/Pattern.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/IR/GlobalVariable.h"

#include "Explosion.h"
#include "GenHeap.h"
#include "GenTuple.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;

/// Emit a global variable.
Address IRGenModule::emitSILGlobalVariable(SILGlobalVariable *var) {
  auto &type = getTypeInfo(var->getLoweredType());
  
  // If the variable is empty, don't actually emit it; just return undef.
  if (type.isKnownEmpty()) {
    return type.getUndefAddress();
  }
  
  /// Get the global variable.
  Address addr = getAddrOfSILGlobalVariable(var,
                     var->isDefinition() ? ForDefinition : NotForDefinition);
  
  /// Add a zero initializer.
  if (var->isDefinition()) {
    auto gvar = cast<llvm::GlobalVariable>(addr.getAddress());
    gvar->setInitializer(llvm::Constant::getNullValue(type.getStorageType()));
  }
  return addr;
}

ContainedAddress FixedTypeInfo::allocateStack(IRGenFunction &IGF, SILType T,
                                              const Twine &name) const {
  // If the type is known to be empty, don't actually allocate anything.
  if (isKnownEmpty()) {
    auto addr = getUndefAddress();
    return { addr, addr };
  }

  Address alloca =
    IGF.createAlloca(getStorageType(), getFixedAlignment(), name);
  // TODO: lifetime intrinsics?

  return { alloca, alloca };
}

void FixedTypeInfo::deallocateStack(IRGenFunction &IGF, Address addr,
                                    SILType T) const {
  // TODO: lifetime intrinsics?
}

/// Allocate an object with fixed layout.
OwnedAddress FixedTypeInfo::allocateBox(IRGenFunction &IGF, SILType T,
                                        const Twine &name) const {
  // If the type is known to be empty, don't actually allocate anything.
  if (isKnownEmpty())
    return OwnedAddress(getUndefAddress(), IGF.IGM.RefCountedNull);

  // Lay out the type as a heap object.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, T, this);
  assert(!layout.isKnownEmpty() && "non-empty type had empty layout?");
  auto &elt = layout.getElement(0);

  // Allocate a new object.
  // TODO: lifetime intrinsics?
  llvm::Value *allocation = IGF.emitUnmanagedAlloc(layout, name + ".alloc");

  // FIXME: provide non-fixed offsets
  NonFixedOffsets offsets = None;

  // Cast and GEP down to the element.
  Address rawAddr = layout.emitCastTo(IGF, allocation);
  rawAddr = elt.project(IGF, rawAddr, offsets, name);

  OwnedAddress addr(rawAddr, allocation);
  return addr;
}

// Deallocate a fixed-layout box that is uninitialized.
void FixedTypeInfo::deallocateBox(IRGenFunction &IGF, llvm::Value *boxOwner,
                                  SILType T) const {
  // If the type is known to be empty, a box isn't actually allocated.
  if (isKnownEmpty())
    return;
  
  // Lay out the type as a heap object.
  HeapLayout layout(IGF.IGM, LayoutStrategy::Optimal, T, this);
  assert(!layout.isKnownEmpty() && "non-empty type had empty layout?");

  auto size = layout.emitSize(IGF.IGM);
  auto alignMask = layout.emitAlignMask(IGF.IGM);
  
  emitDeallocateHeapObject(IGF, boxOwner, size, alignMask);
}
