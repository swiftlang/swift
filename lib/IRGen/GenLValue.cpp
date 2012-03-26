//===--- GenLValue.cpp - IR Generation for Operations on L-Values ---------===//
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
//  This file implements IR generation for store and, conceivably,
//  compound store operations on l-values.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Optional.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "LValue.h"

#include "GenLValue.h"

using namespace swift;
using namespace irgen;

void PathComponent::_anchor() {}
void LogicalPathComponent::_anchor() {}
void PhysicalPathComponent::_anchor() {}

namespace {
  /// A path component with a fixed address.
  class FixedAddress : public PhysicalPathComponent {
    OwnedAddress Addr;

  public:
    FixedAddress(OwnedAddress addr)
      : PhysicalPathComponent(sizeof(FixedAddress)), Addr(addr) {}

    OwnedAddress offset(IRGenFunction &IGF, OwnedAddress base) const {
      assert(!base.isValid());
      return Addr;
    }
  };
}

/// Create an l-value which resolves exactly to the given address.
LValue IRGenFunction::emitAddressLValue(OwnedAddress address) {
  LValue lvalue;
  lvalue.add<FixedAddress>(address);
  return lvalue;
}

/// Load this l-value to create an exploded r-value.
void IRGenFunction::emitLoad(const LValue &lvalue, const TypeInfo &type,
                             Explosion &explosion) {
  OwnedAddress address;

  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ) {
    const PathComponent &component = *i++;

    // If this is a physical component, just compute it relative to the
    // previous component.  The address is initialized on the first pass,
    // but that's okay, because the first component should never care.
    if (component.isPhysical()) {
      address = component.asPhysical().offset(*this, address);
      continue;
    }

    // If this is the last component, load it and return that as the result.
    if (i == e)
      return component.asLogical().loadExplosion(*this, address, explosion);

    // Otherwise, load and materialize the result into memory.
    address = component.asLogical().loadAndMaterialize(*this, NotOnHeap,
                                                       address);
  }

  return type.load(*this, address, explosion);
}

/// Perform a store into the given path, given the base of the first
/// component.
static void emitAssignRecursive(IRGenFunction &IGF,
                                Address base,
                                const TypeInfo &finalType,
                                Explosion &finalValue,
                                LValue::const_iterator pathStart,
                                LValue::const_iterator pathEnd) {
  // Drill into any physical components.
  while (true) {
    assert(pathStart != pathEnd);

    const PathComponent &component = *pathStart;
    if (component.isLogical()) break;
    base = component.asPhysical().offset(IGF,
                                   OwnedAddress(base, IGF.IGM.RefCountedNull));

    // If we reach the end, do an assignment and we're done.
    if (++pathStart == pathEnd) {
      return finalType.assign(IGF, finalValue, base);
    }
  }

  // Okay, we have a logical component.
  assert(pathStart != pathEnd);
  const LogicalPathComponent &component = pathStart->asLogical();
  ++pathStart;
  
  // If this is the final component, just do a logical store.
  if (pathStart == pathEnd) {
    return component.storeExplosion(IGF, finalValue, base);
  }

  // Otherwise, load and materialize into a temporary.
  Address temp = component.loadAndMaterialize(IGF, OnHeap, base);

  // Recursively perform the store.
  emitAssignRecursive(IGF, temp, finalType, finalValue, pathStart, pathEnd);

  // Store the temporary back.
  component.storeMaterialized(IGF, temp, base);
}
                           

void IRGenFunction::emitAssign(Explosion &rvalue, const LValue &lvalue,
                              const TypeInfo &type) {
  emitAssignRecursive(*this, Address(), type, rvalue,
                      lvalue.begin(), lvalue.end());
}

/// Given an l-value which is known to be physical, load from it.
OwnedAddress IRGenFunction::emitAddressForPhysicalLValue(const LValue &lvalue) {
  OwnedAddress address;
  for (auto &component : lvalue) {
    address = component.asPhysical().offset(*this, address);
  }
  return address;
}

void IRGenFunction::emitLValueAsScalar(const LValue &lvalue, OnHeap_t onHeap,
                                       Explosion &explosion) {
  OwnedAddress address;
  for (auto &component : lvalue) {
    if (component.isLogical()) {
      // FIXME: we only need to materialize the *final* logical value
      // to the heap.
      address = component.asLogical().loadAndMaterialize(*this, onHeap,
                                                         address);
    } else {
      address = component.asPhysical().offset(*this, address);
    }
  }

  // FIXME: writebacks
  // FIXME: rematerialize if inadequate alignment

  // Add the address.
  explosion.add(address.getAddressPointer());

  // If we're emitting a heap l-value, also emit the owner pointer.
  if (onHeap == OnHeap) {
    // We need to retain.  We're optimistically delaying the retain
    // until here, but that may not be safe in general.
    emitRetain(address.getOwner(), explosion);
  }
}

void IRGenFunction::emitAssign(Expr *rhs, const LValue &lhs,
                                   const TypeInfo &type) {
  // We don't expect that l-value emission is generally going to admit
  // maximally-exploded calls.
  Explosion explosion(ExplosionKind::Minimal);
  emitRValue(rhs, explosion);
  emitAssign(explosion, lhs, type);
}

namespace {
  /// The type layout for [byref(heap)] types.
  class HeapLValueTypeInfo : public TypeInfo {
  public:
    HeapLValueTypeInfo(llvm::StructType *type, Size s, Alignment a)
      : TypeInfo(type, s, a) {}

    llvm::StructType *getStorageType() const {
      return cast<llvm::StructType>(TypeInfo::getStorageType());
    }

    unsigned getExplosionSize(ExplosionKind kind) const {
      return 2;
    }

    void getSchema(ExplosionSchema &schema) const {
      llvm::StructType *ty = getStorageType();
      assert(ty->getNumElements() == 2);
      schema.add(ExplosionSchema::Element::forScalar(ty->getElementType(0)));
      schema.add(ExplosionSchema::Element::forScalar(ty->getElementType(1)));
    }

    static Address projectReference(IRGenFunction &IGF, Address address) {
      return IGF.Builder.CreateStructGEP(address, 0, Size(0),
                                         address->getName() + ".reference");
    }

    static Address projectOwner(IRGenFunction &IGF, Address address) {
      return IGF.Builder.CreateStructGEP(address, 1, IGF.IGM.getPointerSize(),
                                         address->getName() + ".owner");
    }

    void load(IRGenFunction &IGF, Address address, Explosion &e) const {
      // Load the reference.
      Address refAddr = projectReference(IGF, address);
      e.add(IGF.Builder.CreateLoad(refAddr, refAddr->getName() + ".load"));

      // Load the owner.
      IGF.emitLoadAndRetain(projectOwner(IGF, address), e);
    }

    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the reference.
      IGF.Builder.CreateStore(e.claimNext(), projectReference(IGF, address));

      // Store the owner.
      IGF.emitAssignRetained(e.claimNext(), projectOwner(IGF, address));
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the reference.
      IGF.Builder.CreateStore(e.claimNext(), projectReference(IGF, address));

      // Store the owner, transferring the +1.
      IGF.emitInitializeRetained(e.claimNext(), projectOwner(IGF, address));
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(src.claimNext());
      dest.add(src.claimNext());
    }
  };
}

/// Convert an l-value type.  For non-heap l-values, this is always
/// just a bare pointer.  For heap l-values, this is a pair of a bare
/// pointer with an object reference.
const TypeInfo *TypeConverter::convertLValueType(IRGenModule &IGM,
                                                 LValueType *T) {
  const TypeInfo &objectTI = IGM.getFragileTypeInfo(T->getObjectType());
  llvm::PointerType *referenceType = objectTI.StorageType->getPointerTo();

  // If it's not a heap l-value, just use the reference type as a
  // primitive pointer.
  if (!T->isHeap()) {
    return createPrimitive(referenceType, IGM.getPointerSize(),
                           IGM.getPointerAlignment());
  }

  // Otherwise, pair up the reference with an owner pointer.  We put
  // the reference before the owner because, if we do ever see one of
  // these held in memory, it's more likely that we'll want the
  // reference by itself than that we'd want the owner by itself, so
  // it makes sense to give it the trivial offset.  Generally that's
  // not going to matter too much, though, because the offset will
  // probably get combined with some base offset.
  llvm::Type *elts[] = { referenceType, IGM.RefCountedPtrTy };
  llvm::StructType *pairTy =
    llvm::StructType::get(IGM.getLLVMContext(), elts, /*packed*/ false);
  return new HeapLValueTypeInfo(pairTy, IGM.getPointerSize() * 2,
                                IGM.getPointerAlignment());
}

/// Emit a change in the qualification of an l-value.  The only change
/// that we need to handle here explicitly is the shift of a heap
/// l-value to a non-heap l-value.
void swift::irgen::emitRequalify(IRGenFunction &IGF, RequalifyExpr *E,
                                 Explosion &explosion) {
  LValueType *srcType = E->getSubExpr()->getType()->castTo<LValueType>();
  LValueType::Qual srcQs = srcType->getQualifiers();
  LValueType::Qual destQs = E->getType()->castTo<LValueType>()->getQualifiers();

  // If we're losing heap-qualification, this involves a representation change.
  if (srcQs.isHeap() && !destQs.isHeap()) {
    const TypeInfo &heapTI = IGF.getFragileTypeInfo(srcType->getObjectType());

    // Try to just figure out an address and use that.
    if (Optional<Address> addr = IGF.tryEmitAsAddress(E->getSubExpr(), heapTI))
      return explosion.add(addr.getValue().getAddress());

    // Otherwise, emit as a heap l-value and project out the reference.
    Explosion subExplosion(explosion.getKind());
    IGF.emitRValue(E->getSubExpr(), subExplosion);
    explosion.add(subExplosion.claimNext());

    // FIXME: decide how we want to handle the owner value.
    return;
  }

  // Otherwise, it doesn't, and we can just emit the underlying
  // expression directly.
  return IGF.emitRValue(E->getSubExpr(), explosion);
}
