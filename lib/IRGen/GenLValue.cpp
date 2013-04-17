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

#include "llvm/IR/Function.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Optional.h"
#include "ASTVisitor.h"
#include "CallEmission.h"
#include "GenClass.h"
#include "GenInit.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenStruct.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
#include "FormalType.h"
#include "LValue.h"
#include "FixedTypeInfo.h"
#include "ScalarTypeInfo.h"

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
    FixedAddress(OwnedAddress addr) : Addr(addr) {}

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
      return component.asLogical().loadExplosion(*this, address, explosion,
                                                 ConsumeValues);

    // Otherwise, load and materialize the result into memory.
    address = component.asLogical().loadAndMaterialize(*this, NotOnHeap,
                                                       address,
                                                       ConsumeValues);
  }

  return type.load(*this, address, explosion);
}

namespace {
  /// A visitor for emitting an assignment to a physical object.
  class AssignEmitter : public irgen::ASTVisitor<AssignEmitter> {
    IRGenFunction &IGF;
    Address Dest;
    const TypeInfo &ObjectTI;

  public:
    AssignEmitter(IRGenFunction &IGF, Address dest, const TypeInfo &objectTI)
      : IGF(IGF), Dest(dest), ObjectTI(objectTI) {}

    /// If the expression is a load from something, try to emit that
    /// as an address and then do a copy-assign.
    void visitLoadExpr(LoadExpr *E) {
      abort();
      
      return visitExpr(E);
    }

    /// Default case.
    void visitExpr(Expr *E) {
      // TODO: if's natural to emit this r-value into memory, do so
      // and emit a take-assign.

      Explosion value(ExplosionKind::Maximal);
      IGF.emitRValue(E, value);
      ObjectTI.assign(IGF, value, Dest);
    }
  };
}

/// Emit the given expression for assignment into the given physical object.
static void emitRValueAsAssign(IRGenFunction &IGF,
                               Expr *E, Address object,
                               const TypeInfo &objectTI) {
  AssignEmitter(IGF, object, objectTI).visit(E);
}

/// Perform a store into the given path, given the base of the first
/// component.
static void emitAssignRecursive(IRGenFunction &IGF,
                                Address base,
                                const TypeInfo &finalType,
                                Expr *finalExpr,
                                Explosion *finalExplosion,
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
      if (finalExpr)
        return emitRValueAsAssign(IGF, finalExpr, base, finalType);
      return finalType.assign(IGF, *finalExplosion, base);
    }
  }

  // Okay, we have a logical component.
  assert(pathStart != pathEnd);
  const LogicalPathComponent &component = pathStart->asLogical();
  ++pathStart;
  
  // If this is the final component, just do a logical store.
  if (pathStart == pathEnd) {
    if (finalExpr)
      return component.storeRValue(IGF, finalExpr, base, ConsumeValues);
    return component.storeExplosion(IGF, *finalExplosion, base,
                                    ConsumeValues);
  }

  // Otherwise, load and materialize into a temporary.
  Address temp = component.loadAndMaterialize(IGF, NotOnHeap, base,
                                              PreserveValues);

  // Recursively perform the store.
  emitAssignRecursive(IGF, temp, finalType, finalExpr, finalExplosion,
                      pathStart, pathEnd);

  // Store the temporary back.
  component.storeMaterialized(IGF, temp, base, ConsumeValues);
}
                           

void IRGenFunction::emitAssign(Expr *E, const LValue &lvalue,
                              const TypeInfo &type) {
  emitAssignRecursive(*this, Address(), type, E, nullptr,
                      lvalue.begin(), lvalue.end());
}

void IRGenFunction::emitAssign(Explosion &explosion, const LValue &lvalue,
                              const TypeInfo &type) {
  emitAssignRecursive(*this, Address(), type, nullptr, &explosion,
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

static OwnedAddress emitMaterializeWithWriteback(IRGenFunction &IGF,
                                                 LValue &&lvalue,
                                                 OnHeap_t onHeap) {
  OwnedAddress address;
  for (auto &component : lvalue) {
    if (component.isLogical()) {
      // FIXME: we only need to materialize the *final* logical value
      // to the heap.
      address = component.asLogical().loadAndMaterialize(IGF, onHeap, address,
                                                         ConsumeValues);
    } else {
      address = component.asPhysical().offset(IGF, address);
    }
  }

  // FIXME: writebacks
  // FIXME: rematerialize if inadequate alignment
  return address;
}

void IRGenFunction::emitLValueAsScalar(LValue &&lvalue, OnHeap_t onHeap,
                                       Explosion &explosion) {
  OwnedAddress address =
    ::emitMaterializeWithWriteback(*this, std::move(lvalue), onHeap);

  // Add the address.
  explosion.addUnmanaged(address.getAddressPointer());

  // If we're emitting a heap l-value, also emit the owner pointer.
  if (onHeap == OnHeap) {
    // We need to retain.  We're optimistically delaying the retain
    // until here, but that may not be safe in general.
    emitRetain(address.getOwner(), explosion);
  }
}

/// Given an l-value, locate it in memory, using the appropriate writeback.
Address IRGenFunction::emitMaterializeWithWriteback(LValue &&lvalue,
                                                    OnHeap_t onHeap) {
  return ::emitMaterializeWithWriteback(*this, std::move(lvalue), onHeap);
}

namespace {
  /// The type layout for [byref(heap)] types.
  class HeapLValueTypeInfo :
    public ScalarTypeInfo<HeapLValueTypeInfo,FixedTypeInfo> {
  public:
    HeapLValueTypeInfo(llvm::StructType *type, Size s, Alignment a)
      : ScalarTypeInfo(type, s, a, IsNotPOD) {}

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
      e.addUnmanaged(
            IGF.Builder.CreateLoad(refAddr, refAddr->getName() + ".load"));

      // Load the owner.
      IGF.emitLoadAndRetain(projectOwner(IGF, address), e);
    }

    void loadAsTake(IRGenFunction &IGF, Address address, Explosion &e) const {
      // Load the reference.
      Address refAddr = projectReference(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(refAddr));

      // Load the owner.
      Address ownerAddr = projectOwner(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(ownerAddr));
    }

    void loadUnmanaged(IRGenFunction &IGF, Address address,
                       Explosion &e) const {
      // Load the reference.
      Address refAddr = projectReference(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(refAddr));
      
      // Load the owner.
      Address ownerAddr = projectOwner(IGF, address);
      e.addUnmanaged(IGF.Builder.CreateLoad(ownerAddr));
    }
    
    void assign(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the reference.
      IGF.Builder.CreateStore(e.claimUnmanagedNext(),
                              projectReference(IGF, address));

      // Store the owner.
      IGF.emitAssignRetained(e.forwardNext(IGF),
                             projectOwner(IGF, address));
    }

    void initialize(IRGenFunction &IGF, Explosion &e, Address address) const {
      // Store the reference.
      IGF.Builder.CreateStore(e.claimUnmanagedNext(),
                              projectReference(IGF, address));

      // Store the owner, transferring the +1.
      IGF.emitInitializeRetained(e.forwardNext(IGF),
                                 projectOwner(IGF, address));
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
      IGF.emitRetain(src.claimNext().getValue(), dest);
    }

    void retain(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRetainCall(e.claimNext().getValue());
    }
      
    void release(IRGenFunction &IGF, Explosion &e) const {
      e.claimNext();
      IGF.emitRelease(e.claimNext().getValue());
    }
      
    void manage(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      src.transferInto(dest, 1);
      dest.add(IGF.enterReleaseCleanup(src.claimUnmanagedNext()));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      IGF.emitRelease(IGF.Builder.CreateLoad(projectOwner(IGF, addr)));
    }
  };
}

/// Convert an l-value type.  For non-heap l-values, this is always
/// just a bare pointer.  For heap l-values, this is a pair of a bare
/// pointer with an object reference.
const TypeInfo *TypeConverter::convertLValueType(LValueType *T) {
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

