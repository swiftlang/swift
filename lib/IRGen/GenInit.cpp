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

#include "GenInit.h"

using namespace swift;
using namespace irgen;

/// Enter a cleanup to destroy an object of arbitrary type.  Adds the
/// address value to the given explosion, along with the appropriate
/// cleanup.
void IRGenFunction::enterDestroyCleanup(Address addr,
                                        const TypeInfo &addrTI,
                                        Explosion &out) {
  out.add(ManagedValue(addr.getAddress(), getCleanupsDepth()));
}

/// Register an object with the initialization process.
CleanupsDepth Initialization::registerObject(IRGenFunction &IGF,
                                             InitializedObject object,
                                             OnHeap_t onHeap,
                                             const TypeInfo &objectTI) {
  // Create the appropriate destroy cleanup.
  registerObject(object, CleanupsDepth::invalid());

  return CleanupsDepth::invalid();
}

void Initialization::registerObjectWithoutDestroy(InitializedObject object) {
  registerObject(object, CleanupsDepth::invalid());
}

/// Register an object with the initialization process.
void Initialization::registerObject(InitializedObject object,
                                    CleanupsDepth destroy) {
  // The invariant is that the cleanup has to be an
  // UnboundDestroy if it's valid.

  ValueRecord record = {
    CleanupsDepth::invalid(), destroy
  };
  Records.insert(std::make_pair(object.Opaque, record));
}

/// Mark that an object has been allocated.
void Initialization::markAllocated(IRGenFunction &IGF,
                                   InitializedObject object,
                                   OwnedAddress address,
                                   CleanupsDepth dealloc) {
  assert(Records.find(object.Opaque) != Records.end() &&
         "object was not registered with initialization");
  ValueRecord &record = Records.find(object.Opaque)->second;
  record.DeallocCleanup = dealloc;
}

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

/// Allocate an object in local scope.
OwnedAddress
Initialization::emitLocalAllocation(IRGenFunction &IGF,
                                    InitializedObject object,
                                    OnHeap_t allocateOnHeap,
                                    const TypeInfo &type, const Twine &name) {
  return type.allocate(IGF, *this, object, allocateOnHeap, name);
}

/// Allocate an object with fixed layout.
OwnedAddress FixedTypeInfo::allocate(IRGenFunction &IGF, Initialization &init,
                                     InitializedObject object,
                                     OnHeap_t onHeap,
                                     const Twine &name) const {
  // If the type is known to be empty, don't actually allocate anything.
  if (isKnownEmpty()) {
    OwnedAddress addr = createEmptyAlloca(IGF.IGM, *this);
    init.markAllocated(IGF, object, addr, CleanupsDepth::invalid());
    return addr;
  }

  // If the object does not need to be allocated on the heap,
  // allocate it on the stack.
  if (!onHeap) {
    Address rawAddr =
      IGF.createAlloca(getStorageType(), getFixedAlignment(), name);
    // TODO: lifetime intrinsics?

    OwnedAddress addr(rawAddr, IGF.IGM.RefCountedNull);
    init.markAllocated(IGF, object, addr, CleanupsDepth::invalid());
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
  init.markAllocated(IGF, object, addr, CleanupsDepth::invalid());
  return addr;
}

static void maybeSetCleanupState(IRGenFunction &IGF,
                                 CleanupsDepth maybeCleanup,
                                 CleanupState newState) {
  if (maybeCleanup.isValid())
    IGF.setCleanupState(maybeCleanup, newState);
}

/// Mark that a value has reached its initialization point.
void Initialization::markInitialized(IRGenFunction &IGF,
                                     InitializedObject object) {
  auto it = Records.find(object.Opaque);
  assert(it != Records.end());

  // Deactivate the dealloc cleanup.
  maybeSetCleanupState(IGF, it->second.DeallocCleanup, CleanupState::Dead);

  // Activate the destroy cleanup.
  maybeSetCleanupState(IGF, it->second.DestroyCleanup, CleanupState::Active);
}
