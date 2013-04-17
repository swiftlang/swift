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
#include "Scope.h"
#include "FixedTypeInfo.h"

#include "GenInit.h"

using namespace swift;
using namespace irgen;

namespace {
  /// A cleanup to destroy an object whose address isn't actually known yet.
  class UnboundDestroy : public Cleanup {
    const TypeInfo &TI;
    OwnedAddress Addr;

  public:
    UnboundDestroy(const TypeInfo &TI) : TI(TI) {}

    void setAddress(OwnedAddress addr) {
      assert(!Addr.isValid());
      Addr = addr;
    }

    void emit(IRGenFunction &IGF) const {
      assert(Addr.isValid());
      llvm::Value *owner = Addr.getOwner();
      if (!isa<llvm::ConstantPointerNull>(owner)) {
        IGF.emitRelease(owner);
      } else {
        TI.destroy(IGF, Addr);
      }
    }
  };
}

/// Enter a cleanup to destroy an object of arbitrary type.  Adds the
/// address value to the given explosion, along with the appropriate
/// cleanup.
void IRGenFunction::enterDestroyCleanup(Address addr,
                                        const TypeInfo &addrTI,
                                        Explosion &out) {
  enterDestroyCleanup(addr, addrTI);
  out.add(ManagedValue(addr.getAddress(), getCleanupsDepth()));
}

/// Enter a cleanup to destroy an object of arbitrary type.
void IRGenFunction::enterDestroyCleanup(Address addr,
                                        const TypeInfo &addrTI) {
  assert(!addrTI.isPOD(ResilienceScope::Local) &&
         "destroying something known to be POD");

  // The use of UnboundDestroy here is not important.
  UnboundDestroy &destroy = pushCleanup<UnboundDestroy>(addrTI);
  destroy.setAddress(OwnedAddress(addr, IGM.RefCountedNull));
}

/// Should the given variable be allocated on the heap?
static OnHeap_t isOnHeap(VarDecl *var) {
  return (var->hasFixedLifetime() ? NotOnHeap : OnHeap);
}

/// Register an object with the initialization process.
CleanupsDepth Initialization::registerObject(IRGenFunction &IGF,
                                             InitializedObject object,
                                             OnHeap_t onHeap,
                                             const TypeInfo &objectTI) {
  // Create the appropriate destroy cleanup.
  CleanupsDepth destroy;

  // We need a destroy cleanup if the object is on the heap or non-POD.
  if (onHeap || !objectTI.isPOD(ResilienceScope::Local)) {
    IGF.pushFullExprCleanupInState<UnboundDestroy>(CleanupState::Dormant,
                                                   objectTI);
    destroy = IGF.getCleanupsDepth();
  } else {
    destroy = CleanupsDepth::invalid();
  }

  registerObject(object, destroy);

  return destroy;
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

  // Update the destroy cleanup if present.
  if (record.DestroyCleanup.isValid()) {
    UnboundDestroy &destroy =
      static_cast<UnboundDestroy&>(IGF.findCleanup(record.DestroyCleanup));
    destroy.setAddress(address);
  }
}

/// Create a variable in the current scope.  Equivalent to either
/// emitLocalVariable or emitGlobalVariable, depending.
OwnedAddress Initialization::emitVariable(IRGenFunction &IGF, VarDecl *var,
                                          const TypeInfo &type) {
  if (!var->getDeclContext()->isLocalContext())
    return emitGlobalVariable(IGF, var, type);

  OwnedAddress addr =
    emitLocalAllocation(IGF, getObjectForDecl(var), isOnHeap(var), type,
                        var->getName().str());
  IGF.setLocalVar(var, addr);
  return addr;
}

/// Emit a global variable.
Address IRGenModule::emitGlobalVariable(VarDecl *var,
                                        const TypeInfo &type) {
  // If the variable is empty, don't actually emit it; just return undef.
  // FIXME: fragility?  global destructors?
  if (type.isEmpty(ResilienceScope::Local)) {
    auto undef = llvm::UndefValue::get(type.StorageType->getPointerTo());
    return Address(undef, Alignment(1));
  }
  
  /// Get the global variable.
  Address addr = getAddrOfGlobalVariable(var);
  
  // Add a zero-initializer.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.getStorageType()));

  return addr;
}

/// Emit a global variable.
OwnedAddress Initialization::emitGlobalVariable(IRGenFunction &IGF,
                                                VarDecl *var,
                                                const TypeInfo &type) {
  // If the variable is empty, don't actually emit it; just return undef.
  // FIXME: fragility?  global destructors?
  if (type.isEmpty(ResilienceScope::Local)) {
    auto undef = llvm::UndefValue::get(type.StorageType->getPointerTo());
    auto addr = Address(undef, Alignment(1));
    return OwnedAddress(addr, IGF.IGM.RefCountedNull);
  }

  /// Get the global variable.
  Address addr = IGF.IGM.getAddrOfGlobalVariable(var);

  // Add a zero-initializer.
  llvm::GlobalVariable *gvar = cast<llvm::GlobalVariable>(addr.getAddress());
  gvar->setInitializer(llvm::Constant::getNullValue(type.getStorageType()));

  // TODO: global destructors?
  return OwnedAddress(addr, IGF.IGM.RefCountedNull);
}

/// Create an allocation for an empty object.
static OwnedAddress createEmptyAlloca(IRGenModule &IGM, const TypeInfo &type) {
  llvm::Value *badPointer =
    llvm::UndefValue::get(type.getStorageType()->getPointerTo());
  return OwnedAddress(Address(badPointer, type.StorageAlignment),
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
  if (isEmpty(ResilienceScope::Local)) {
    OwnedAddress addr = createEmptyAlloca(IGF.IGM, *this);
    init.markAllocated(IGF, object, addr, CleanupsDepth::invalid());
    return addr;
  }

  // If the object does not need to be allocated on the heap,
  // allocate it on the stack.
  if (!onHeap) {
    Address rawAddr =
      IGF.createAlloca(getStorageType(), StorageAlignment, name);
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

  // Push a cleanup to dealloc the allocation.
  // FIXME: don't emit the size twice!
  CleanupsDepth deallocCleanup
    = IGF.pushDeallocCleanup(allocation, layout.emitSize(IGF));

  OwnedAddress addr(rawAddr, allocation);
  init.markAllocated(IGF, object, addr, deallocCleanup);
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

/// Emit an expression as an initializer for the given location.
void Initialization::emitInit(IRGenFunction &IGF, InitializedObject object,
                              Address addr, Expr *E, const TypeInfo &type) {
  IGF.emitRValueAsInit(E, addr, type);

  // Mark as initialized.  This assumes that calls to
  // TypeInfo::initialize are atomic w.r.t. exceptions and
  // other control flow.
  markInitialized(IGF, object);
}

/// Emit an r-value directly into memory as an initialization.
/// Enable a cleanup for it as soon as it's complete.
void IRGenFunction::emitInit(Expr *E, Address addr, const TypeInfo &type) {
  Initialization I;
  auto object = I.getObjectForTemporary();
  I.registerObjectWithoutDestroy(object);
  I.emitInit(*this, object, addr, E, type);
}

/// Zero-initialize the given memory location.
void Initialization::emitZeroInit(IRGenFunction &IGF, InitializedObject object,
                                  Address addr, const TypeInfo &type) {
  // Zero-initialization always has trivial outwards control flow; go
  // ahead and immediately switch the cleanups.
  markInitialized(IGF, object);

  // No work is necessary if the type is empty or the address is global.
  if (type.isEmpty(ResilienceScope::Local) ||
      isa<llvm::Constant>(addr.getAddress()))
    return;

  ExplosionSchema schema(ExplosionKind::Maximal);
  type.getSchema(schema);

  // Try to fill the value in with stores if that doesn't make for a
  // ridiculous amount of IR.  This is impossible if the schema
  // contains an aggregate;  otherwise, 4 is just a number.
  if (!schema.containsAggregate() && schema.size() <= 4) {
    Explosion explosion(schema.getKind());
    for (auto elt : schema) {
      explosion.addUnmanaged(llvm::Constant::getNullValue(elt.getScalarType()));
    }
    type.initialize(IGF, explosion, addr);
    return;
  }

  // Otherwise, just do a memset.
  IGF.Builder.CreateMemSet(IGF.Builder.CreateBitCast(addr.getAddress(),
                                                     IGF.IGM.Int8PtrTy),
                           IGF.Builder.getInt8(0),
                           IGF.Builder.getInt64(type.StorageSize.getValue()),
                           addr.getAlignment().getValue(),
                           /*volatile*/ false);
}

