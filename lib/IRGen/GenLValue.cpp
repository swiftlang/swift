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

#include "GenLValue.h"

#include "llvm/Function.h"
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
#include "GetterSetter.h"
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

/// Emit a load from the given l-value as an initializer.
void swift::irgen::emitLoadAsInit(IRGenFunction &IGF, const LValue &lvalue,
                                  Address dest, const TypeInfo &destTI) {
  OwnedAddress baseAddress;

  for (auto i = lvalue.begin(), e = lvalue.end(); i != e; ) {
    const PathComponent &component = *i++;

    // If this is a physical component, just compute it relative to the
    // previous component.  The address is initialized on the first pass,
    // but that's okay, because the first component should never care.
    if (component.isPhysical()) {
      baseAddress = component.asPhysical().offset(IGF, baseAddress);
      continue;
    }

    // If this is the last component, load it and return that as the result.
    if (i == e)
      return component.asLogical().loadMaterialized(IGF, baseAddress, dest,
                                                    ConsumeValues);

    // Otherwise, load and materialize the result into memory.
    baseAddress = component.asLogical().loadAndMaterialize(IGF, NotOnHeap,
                                                           baseAddress,
                                                           ConsumeValues);
  }

  return destTI.initializeWithCopy(IGF, dest, baseAddress);  
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
      if (Optional<Address> src
            = IGF.tryEmitAsAddress(E->getSubExpr(), ObjectTI))
        return ObjectTI.assignWithCopy(IGF, Dest, src.getValue());
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
      return explosion.addUnmanaged(addr.getValue().getAddress());

    // Otherwise, emit as a heap l-value and project out the reference.
    Explosion subExplosion(explosion.getKind());
    IGF.emitRValue(E->getSubExpr(), subExplosion);
    subExplosion.transferInto(explosion, 1);
    subExplosion.ignoreAndDestroy(IGF, 1);
    return;
  }

  // Otherwise, it doesn't, and we can just emit the underlying
  // expression directly.
  return IGF.emitRValue(E->getSubExpr(), explosion);
}

/// Must accesses to the given variable be performed as a
/// logical access?
static bool isVarAccessLogical(IRGenFunction &IGF, VarDecl *var) {
  return var->isProperty() || IGF.IGM.isResilient(var);
}

void GetterSetter::storeRValue(IRGenFunction &IGF, Expr *rvalue, Address base,
                               ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareSetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addArg(rvalue);
  emission.emitVoid();
}

void GetterSetter::storeMaterialized(IRGenFunction &IGF, Address temp,
                                     Address base,
                                 ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareSetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addMaterializedArg(temp, /*asTake*/ false);
  emission.emitVoid();
}

void GetterSetter::storeExplosion(IRGenFunction &IGF,
                                  Explosion &value, Address base,
                                  ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareSetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addArg(value);
  emission.emitVoid();
}

void GetterSetter::loadExplosion(IRGenFunction &IGF, Address base,
                                 Explosion &out,
                                 ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareGetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addEmptyArg();
  emission.emitToExplosion(out);
}

void GetterSetter::loadMaterialized(IRGenFunction &IGF, Address base,
                                    Address temp,
                                    ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareGetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addEmptyArg();

  const TypeInfo &valueTI =
    IGF.getFragileTypeInfo(emission.getCallee().getSubstResultType());
  emission.emitToMemory(temp, valueTI);
}

OwnedAddress
GetterSetter::loadAndMaterialize(IRGenFunction &IGF,
                                 OnHeap_t onHeap, Address base,
                                 ShouldPreserveValues shouldPreserve) const {
  CallEmission emission = prepareGetter(IGF, base, ExplosionKind::Maximal,
                                        shouldPreserve);
  emission.addEmptyArg();

  Initialization init;
  InitializedObject object = init.getObjectForTemporary();

  const TypeInfo &valueTI =
    IGF.getFragileTypeInfo(emission.getCallee().getSubstResultType());
  init.registerObject(IGF, object, onHeap, valueTI);

  // Emit a local allocation for the object.
  OwnedAddress addr = init.emitLocalAllocation(IGF, object, onHeap,
                                               valueTI, "temporary");

  // Load into the new memory.
  emission.emitToMemory(addr, valueTI);

  // The load is complete;  mark the memory as initialized.
  init.markInitialized(IGF, object);

  return addr;
}

void MemberGetterSetterBase::initBaseValues(Explosion &values) {
  unsigned numValues = values.size();
  assert(numValues == NumBaseValues);
  std::memcpy(getBaseValuesBuffer(), values.claimAll().data(),
              sizeof(ManagedValue) * numValues);
}

void MemberGetterSetterBase::initIndexValues(Explosion &values) {
  unsigned numValues = values.size();
  assert(numValues == NumIndexValues);
  std::memcpy(getIndexValuesBuffer(), values.claimAll().data(),
              sizeof(ManagedValue) * numValues);
}

/// Given a bunch of stored values of the substituted type, emitted at
/// maximal explosion, add them to the given explosion as values of
/// the original type.
static void addStored(IRGenFunction &IGF, ArrayRef<ManagedValue> storedValues,
                      Explosion &out, ShouldPreserveValues shouldPreserve,
                      CanType origType, CanType substType,
                      ArrayRef<Substitution> subs) {
  // In the simplest case, just add them straight.
  if (!shouldPreserve &&
      out.getKind() == ExplosionKind::Maximal &&
      origType == substType) {
    return out.add(storedValues);
  }

  // Otherwise, we need to reexplode and potentially map.
  Explosion storedExplosion(ExplosionKind::Maximal);
  storedExplosion.add(storedValues);

  // FIXME: re-emit under substitution!
  const TypeInfo &byvalTI = IGF.getFragileTypeInfo(substType);
  if (shouldPreserve) {
    byvalTI.copy(IGF, storedExplosion, out);
  } else {
    byvalTI.reexplode(IGF, storedExplosion, out);
  }
}

/// Add the base values to the given explosion.
void MemberGetterSetterBase::addBaseValues(IRGenFunction &IGF,
                                           Explosion &out,
                                           ShouldPreserveValues shouldPreserve,
                                           CanType origBaseType,
                                           CanType substBaseType,
                                           ArrayRef<Substitution> subs) const {
  // The stored index values come from a maximal explosion.
  ArrayRef<ManagedValue> storedValues(getBaseValuesBuffer(),
                                      NumBaseValues);

  addStored(IGF, storedValues, out, shouldPreserve,
            origBaseType, substBaseType, subs);
}

/// Add the index values to the given explosion.
void MemberGetterSetterBase::addIndexValues(IRGenFunction &IGF,
                                            Explosion &out,
                                            ShouldPreserveValues shouldPreserve,
                                            CanType origIndexType,
                                            CanType substIndexType,
                                            ArrayRef<Substitution> subs) const {
  // The stored index values come from a maximal explosion.
  ArrayRef<ManagedValue> storedValues(getIndexValuesBuffer(),
                                      NumIndexValues);

  addStored(IGF, storedValues, out, shouldPreserve,
            origIndexType, substIndexType, subs);
}

namespace {
  /// A common base class for logical Vars or Subscripts that provide
  /// known, global accessors.
  class ConcreteMemberGetterSetterBase : public MemberGetterSetterBase {
  protected:
    ConcreteMemberGetterSetterBase(ValueDecl *target,
                                   unsigned numBaseValues,
                                   unsigned numIndexValues,
                                   size_t thisSize)
      : MemberGetterSetterBase(target, numBaseValues, numIndexValues,
                               thisSize) {
    }

    /// Returns the substituted object type.
    virtual CanType getSubstObjectType() const = 0;

    /// Returns the substituted index type.
    virtual CanType getSubstIndexType() const = 0;

    /// Returns the substitutions in effect.
    virtual ArrayRef<Substitution> getSubstitutions() const = 0;

    /// Add the 'self' argument.
    virtual void addSelfArg(CallEmission &emission, Address base,
                            ShouldPreserveValues shouldPreserve) const = 0;

    /// Prepare the getter.  Subclasses shouldn't need to override this.
    CallEmission prepareGetter(IRGenFunction &IGF,
                               Address base,
                               ExplosionKind maxExplosion,
                               ShouldPreserveValues shouldPreserve) const {
      // For now, always be pessimistic.
      ExplosionKind explosionLevel = ExplosionKind::Minimal;
      Callee callee = getGetter(IGF, explosionLevel);

      CallEmission emission(IGF, callee);
      addArgs(emission, base, shouldPreserve);
      return emission;
    }

    /// Prepare the setter.  Subclasses shouldn't need to override this.
    CallEmission prepareSetter(IRGenFunction &IGF,
                               Address base,
                               ExplosionKind maxExplosion,
                               ShouldPreserveValues shouldPreserve) const {
      // For now, always be pessimistic.
      ExplosionKind explosionLevel = ExplosionKind::Minimal;
      Callee callee = getSetter(IGF, explosionLevel);

      CallEmission emission(IGF, callee);
      addArgs(emission, base, shouldPreserve);
      return emission;
    }

  private:
    void addArgs(CallEmission &emission, Address base,
                 ShouldPreserveValues shouldPreserve) const {
      // Defer to the derived class for how to emit the 'self' argument.
      addSelfArg(emission, base, shouldPreserve);

      // Add the index argument if necessary.
      addIndexArg(emission, shouldPreserve);
    }

    Callee getGetter(IRGenFunction &IGF, ExplosionKind explosionLevel) const {
      auto target = getTarget();
      FormalType formal = IGF.IGM.getTypeOfGetter(target);
      llvm::Constant *fn =
        IGF.IGM.getAddrOfGetter(target, formal, explosionLevel);

      return Callee::forKnownFunction(formal.getCC(), formal.getType(),
                                      getSubstObjectType(),
                                      getSubstitutions(),
                                      fn, ManagedValue(nullptr),
                                      explosionLevel,
                                      formal.getNaturalUncurryLevel());
    }

    Callee getSetter(IRGenFunction &IGF, ExplosionKind explosionLevel) const {
      auto target = getTarget();
      FormalType formal = IGF.IGM.getTypeOfSetter(target);
      llvm::Constant *fn =
        IGF.IGM.getAddrOfSetter(target, formal, explosionLevel);

      CanType substResultType = CanType(TupleType::getEmpty(IGF.IGM.Context));
      return Callee::forKnownFunction(formal.getCC(), formal.getType(),
                                      substResultType,
                                      getSubstitutions(),
                                      fn, ManagedValue(nullptr),
                                      explosionLevel,
                                      formal.getNaturalUncurryLevel());
    }

    /// Add the index argument.
    void addIndexArg(CallEmission &emission,
                     ShouldPreserveValues shouldPreserve) const {
      auto subscript = dyn_cast<SubscriptDecl>(getTarget());
      if (!subscript) return;

      CanType origIndexType =
        subscript->getIndices()->getType()->getCanonicalType();

      Explosion index(emission.getCurExplosionLevel());
      addIndexValues(emission.IGF, index, shouldPreserve,
                     origIndexType,
                     getSubstIndexType(),
                     getSubstitutions());
      emission.addArg(index);
    }
  };

  /// A common base class for logical members of non-generic types.
  class NonGenericMemberGetterSetterBase
      : public ConcreteMemberGetterSetterBase {
  protected:
    NonGenericMemberGetterSetterBase(ValueDecl *target,
                                     unsigned numBaseValues,
                                     unsigned numIndexValues,
                                     size_t thisSize)
      : ConcreteMemberGetterSetterBase(target, numBaseValues, numIndexValues,
                                       thisSize) {}

    ArrayRef<Substitution> getSubstitutions() const final {
      return ArrayRef<Substitution>();
    }

    CanType getSubstObjectType() const final {
      ValueDecl *target = getTarget();
      if (auto subscript = dyn_cast<SubscriptDecl>(target))
        return subscript->getElementType()->getCanonicalType();
      return cast<VarDecl>(target)->getType()->getCanonicalType();
    }

    CanType getSubstIndexType() const final {
      auto subscript = cast<SubscriptDecl>(getTarget());
      return subscript->getIndices()->getType()->getCanonicalType();
    }
  };

  /// A getter/setter lvalue for accesses into non-generic reference
  /// types (or, more generally, anything where we're meant to pass
  /// the self object by value).
  class DirectMemberGetterSetter
      : public MemberGetterSetter<DirectMemberGetterSetter,
                                  NonGenericMemberGetterSetterBase> {
  public:
    DirectMemberGetterSetter(SubscriptDecl *target, Explosion &baseArgs,
                             Explosion &indexArgs)
        : MemberGetterSetter(target, baseArgs.size(), indexArgs.size()) {
      initBaseValues(baseArgs);
      initIndexValues(indexArgs);
    }

    DirectMemberGetterSetter(VarDecl *target, Explosion &baseArgs)
        : MemberGetterSetter(target, baseArgs.size()) {
      initBaseValues(baseArgs);
    }

  protected:
    void addSelfArg(CallEmission &emission, Address base,
                    ShouldPreserveValues shouldPreserve) const {
      Explosion selfArg(emission.getCurExplosionLevel());

      CanType origBaseType =
        getTarget()->getDeclContext()->getDeclaredTypeInContext()
                   ->getCanonicalType();
      addBaseValues(emission.IGF, selfArg, shouldPreserve,
                    origBaseType, origBaseType, ArrayRef<Substitution>());
      emission.addArg(selfArg);      
    }
  };

  /// A getter/setter lvalue for accesses into non-generic struct types
  /// (or, more generally, anything where we're meant to pass the self
  /// object by reference).
  class IndirectMemberGetterSetter
      : public MemberGetterSetter<IndirectMemberGetterSetter,
                                  NonGenericMemberGetterSetterBase> {
  public:
    IndirectMemberGetterSetter(SubscriptDecl *target, Explosion &indexArgs)
        : MemberGetterSetter(target, 0, indexArgs.size()) {
      initIndexValues(indexArgs);
    }

    IndirectMemberGetterSetter(VarDecl *target)
        : MemberGetterSetter(target, 0) {
    }

  protected:
    void addSelfArg(CallEmission &emission, Address base,
                    ShouldPreserveValues shouldPreserve) const {
      Explosion selfArg(emission.getCurExplosionLevel());
      selfArg.addUnmanaged(base.getAddress());
      emission.addArg(selfArg);
    }
  };

  /// A common base class for logical members of generic types.
  class GenericMemberGetterSetterBase : public ConcreteMemberGetterSetterBase {
    ArrayRef<Substitution> Subs;
    CanType SubstBaseType;
    CanType SubstIndexType;
    CanType SubstResultType;

  public:
    void setExpr(GenericMemberRefExpr *E) {
      Subs = E->getSubstitutions();
      SubstBaseType = E->getBase()->getType()->getCanonicalType();
      SubstResultType = E->getType()->castTo<LValueType>()->getObjectType()
                         ->getCanonicalType();
    }

    void setExpr(GenericSubscriptExpr *E) {
      Subs = E->getSubstitutions();
      SubstBaseType = E->getBase()->getType()->getCanonicalType();
      SubstResultType = E->getType()->castTo<LValueType>()->getObjectType()
                         ->getCanonicalType();
      SubstIndexType = E->getIndex()->getType()->getCanonicalType();
    }
    
  protected:
    GenericMemberGetterSetterBase(ValueDecl *target,
                                  unsigned numBaseValues,
                                  unsigned numIndexValues,
                                  size_t thisSize)
      : ConcreteMemberGetterSetterBase(target, numBaseValues, numIndexValues,
                                       thisSize) {}

    void emitGenericArguments(CallEmission &emission, Explosion &out) const {
      auto polyFn = cast<PolymorphicFunctionType>(
                                   emission.getCallee().getOrigFormalType());
      return emitPolymorphicArguments(emission.IGF, polyFn,
                                      getSubstitutions(),
                                      out);
    }

    ArrayRef<Substitution> getSubstitutions() const final {
      assert(!Subs.empty());
      return Subs;
    }

    CanType getSubstObjectType() const final {
      assert(SubstResultType);
      return SubstResultType;
    }

    CanType getSubstIndexType() const final {
      assert(SubstIndexType);
      return SubstIndexType;
    }

    CanType getSubstBaseType() const {
      assert(SubstBaseType);
      return SubstBaseType;
    }
  };

  /// A getter/setter lvalue for accesses into generic reference types.
  ///
  /// Clients must call setExpr() in order to fully construct an
  /// instance of this class.
  class GenericDirectMemberGetterSetter
      : public MemberGetterSetter<GenericDirectMemberGetterSetter,
                                  GenericMemberGetterSetterBase> {
  public:
    GenericDirectMemberGetterSetter(SubscriptDecl *target, Explosion &baseArgs,
                                    Explosion &indexArgs)
        : MemberGetterSetter(target, baseArgs.size(), indexArgs.size()) {
      initBaseValues(baseArgs);
      initIndexValues(indexArgs);
    }

    GenericDirectMemberGetterSetter(VarDecl *target, Explosion &baseArgs)
        : MemberGetterSetter(target, baseArgs.size()) {
      initBaseValues(baseArgs);
    }

  protected:
    void addSelfArg(CallEmission &emission, Address base,
                    ShouldPreserveValues shouldPreserve) const {
      Explosion selfArg(emission.getCurExplosionLevel());

      // Collect the new base values.
      CanType origBaseType =
        getTarget()->getDeclContext()->getDeclaredTypeInContext()
                   ->getCanonicalType();
      addBaseValues(emission.IGF, selfArg, shouldPreserve,
                    origBaseType, getSubstBaseType(),
                    getSubstitutions());

      // The generic arguments are bound at this level.
      emitGenericArguments(emission, selfArg);

      emission.addArg(selfArg);
    }
  };

  /// A getter/setter lvalue for accesses into generic value types.
  ///
  /// Clients must call setExpr() in order to fully construct an
  /// instance of this class.
  class GenericIndirectMemberGetterSetter
      : public MemberGetterSetter<GenericIndirectMemberGetterSetter,
                                  GenericMemberGetterSetterBase> {
  public:
    GenericIndirectMemberGetterSetter(SubscriptDecl *target,
                                      Explosion &indexArgs)
        : MemberGetterSetter(target, 0, indexArgs.size()) {
      initIndexValues(indexArgs);
    }

    GenericIndirectMemberGetterSetter(VarDecl *target)
        : MemberGetterSetter(target, 0) {
    }

  protected:
    void addSelfArg(CallEmission &emission, Address base,
                    ShouldPreserveValues shouldPreserve) const {
      Explosion selfArg(emission.getCurExplosionLevel());

#ifndef NDEBUG
      CanType origBaseType =
        cast<PolymorphicFunctionType>(emission.getCallee().getOrigFormalType())
          ->getInput()->getCanonicalType();
      assert(!differsByAbstractionInExplosion(emission.IGF.IGM,
                                              origBaseType,
                                              getSubstBaseType(),
                                              selfArg.getKind()));
#endif
      // No transformation required on l-value.
      selfArg.addUnmanaged(base.getAddress());

      // The generic arguments are bound at this level.
      emitGenericArguments(emission, selfArg);

      emission.addArg(selfArg);
    }
  };

  /// A getter/setter lvalue for accesses to global variables with
  /// setters or getters.
  class GlobalGetterSetter
      : public MemberGetterSetter<GlobalGetterSetter,
                                  NonGenericMemberGetterSetterBase> {
  public:
    GlobalGetterSetter(SubscriptDecl *target, Explosion &indexArgs)
        : MemberGetterSetter(target, 0, indexArgs.size()) {
      initIndexValues(indexArgs);
    }

    GlobalGetterSetter(VarDecl *target)
        : MemberGetterSetter(target, 0) {
    }

  protected:
    void addSelfArg(CallEmission &emission, Address base,
                    ShouldPreserveValues shouldPreserve) const {
    }
  };
}

static LValue emitLogicalClassMemberLValue(IRGenFunction &IGF,
                                           MemberRefExpr *E) {
  // Emit the base value.
  Explosion base(ExplosionKind::Maximal);
  IGF.emitRValue(E->getBase(), base);

  // Build the logical lvalue.
  LValue lvalue;
  lvalue.addWithExtra<DirectMemberGetterSetter>(E->getDecl(), base);
  return lvalue;
}

static LValue emitLogicalStructMemberLValue(IRGenFunction &IGF,
                                            MemberRefExpr *E) {

  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Push a logical member reference.
  lvalue.add<IndirectMemberGetterSetter>(E->getDecl());

  return lvalue;
}

static LValue emitLogicalClassMemberLValue(IRGenFunction &IGF,
                                           GenericMemberRefExpr *E) {
  // Emit the base value.
  Explosion base(ExplosionKind::Maximal);
  IGF.emitRValue(E->getBase(), base);

  // Build the logical lvalue.
  LValue lvalue;
  lvalue.addWithExtra<GenericDirectMemberGetterSetter>(
                                                 cast<VarDecl>(E->getDecl()),
                                                       base)
    .setExpr(E);
  return lvalue;
}

static LValue emitLogicalStructMemberLValue(IRGenFunction &IGF,
                                            GenericMemberRefExpr *E) {

  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Push a logical member reference.
  lvalue.add<GenericIndirectMemberGetterSetter>(cast<VarDecl>(E->getDecl()))
    .setExpr(E);

  return lvalue;
}

template <class T>
static LValue emitAnyMemberRefLValue(IRGenFunction &IGF, T *E) {
  VarDecl *var = cast<VarDecl>(E->getDecl());
  Type baseType = E->getBase()->getType();
  if (!baseType->is<LValueType>()) {
    if (!isVarAccessLogical(IGF, var)) {
      return emitPhysicalClassMemberLValue(IGF, E);
    } else {
      return emitLogicalClassMemberLValue(IGF, E);
    }
  } else {
    if (!isVarAccessLogical(IGF, var)) {
      return emitPhysicalStructMemberLValue(IGF, E);
    } else {
      return emitLogicalStructMemberLValue(IGF, E);
    }
  }
}

/// Emit an l-value for the given member reference.
LValue irgen::emitMemberRefLValue(IRGenFunction &IGF, MemberRefExpr *E) {
  return emitAnyMemberRefLValue(IGF, E);
}

/// Emit an l-value which accesses a member out of a generic type.
LValue irgen::emitGenericMemberRefLValue(IRGenFunction &IGF,
                                         GenericMemberRefExpr *E) {
  return emitAnyMemberRefLValue(IGF, E);
}

/// Try to emit the given member-reference as an address.
Optional<Address> irgen::tryEmitMemberRefAsAddress(IRGenFunction &IGF,
                                                   MemberRefExpr *E) {
  // Can't do anything if the member reference is logical.
  if (isVarAccessLogical(IGF, E->getDecl()))
    return Nothing;

  // FIXME: actually implement this.
  return Nothing;
}

/// Emit a reference to a global variable.
LValue IRGenFunction::getGlobal(VarDecl *var) {
  // If we need to access this variable logically, use a
  // GetterSetterComponent.
  if (isVarAccessLogical(*this, var)) {
    LValue lvalue;
    lvalue.add<GlobalGetterSetter>(var);
    return lvalue;
  }

  const TypeInfo &type = getFragileTypeInfo(var->getType());
  if (type.isEmpty(ResilienceScope::Local)) {
    auto undef = llvm::UndefValue::get(type.StorageType->getPointerTo());
    auto addr = Address(undef, Alignment(1));
    return emitAddressLValue(OwnedAddress(addr, IGM.RefCountedNull));
  }

  // Otherwise we can use a physical-address component.
  OwnedAddress addr(IGM.getAddrOfGlobalVariable(var), IGM.RefCountedNull);
  return emitAddressLValue(addr);
}

static LValue emitLogicalClassSubscriptLValue(IRGenFunction &IGF,
                                              SubscriptExpr *E) {
  // Emit the base value.
  Explosion base(ExplosionKind::Maximal);
  IGF.emitRValue(E->getBase(), base);

  // Emit the index.
  Explosion index(ExplosionKind::Maximal);
  IGF.emitRValue(E->getIndex(), index);

  // Build the logical lvalue.
  LValue lvalue;
  lvalue.addWithExtra<DirectMemberGetterSetter>(E->getDecl(), base, index);
  return lvalue;
}

static LValue emitLogicalStructSubscriptLValue(IRGenFunction &IGF,
                                               SubscriptExpr *E) {
  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Emit the index.  IndirectMemberGetterSetter requires this to be
  // maximal; in theory it might be better to try to match the
  // getter/setter, but that would require some significant
  // complexity.
  Explosion index(ExplosionKind::Maximal);
  IGF.emitRValue(E->getIndex(), index);

  // Subscript accesses are always logical (for now).
  lvalue.addWithExtra<IndirectMemberGetterSetter>(E->getDecl(), index);

  return lvalue;
}

static LValue emitLogicalClassSubscriptLValue(IRGenFunction &IGF,
                                              GenericSubscriptExpr *E) {
  // Emit the base value.
  Explosion base(ExplosionKind::Maximal);
  IGF.emitRValue(E->getBase(), base);

  // Emit the index.
  Explosion index(ExplosionKind::Maximal);
  IGF.emitRValue(E->getIndex(), index);

  // Build the logical lvalue.
  LValue lvalue;
  lvalue.addWithExtra<GenericDirectMemberGetterSetter>(E->getDecl(),
                                                       base, index)
    .setExpr(E);
  return lvalue;
}

static LValue emitLogicalStructSubscriptLValue(IRGenFunction &IGF,
                                               GenericSubscriptExpr *E) {
  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Emit the index.  IndirectMemberGetterSetter requires this to be
  // maximal; in theory it might be better to try to match the
  // getter/setter, but that would require some significant
  // complexity.
  Explosion index(ExplosionKind::Maximal);
  IGF.emitRValue(E->getIndex(), index);

  // Subscript accesses are always logical (for now).
  lvalue.addWithExtra<GenericIndirectMemberGetterSetter>(E->getDecl(), index)
    .setExpr(E);

  return lvalue;
}


template <class T>
static LValue emitAnySubscriptLValue(IRGenFunction &IGF, T *E) {
  if (!E->getBase()->getType()->template is<LValueType>()) {
    return emitLogicalClassSubscriptLValue(IGF, E);
  } else {
    return emitLogicalStructSubscriptLValue(IGF, E);
  }
}

/// Emit an l-value for the given subscripted reference.
LValue irgen::emitSubscriptLValue(IRGenFunction &IGF, SubscriptExpr *E) {
  return emitAnySubscriptLValue(IGF, E);
}

/// Emit an l-value for the given subscripted reference.
LValue irgen::emitGenericSubscriptLValue(IRGenFunction &IGF,
                                         GenericSubscriptExpr *E) {
  return emitAnySubscriptLValue(IGF, E);
}

/// Emit an expression which accesses a member out of a generic type.
void irgen::emitGenericMemberRef(IRGenFunction &IGF,
                                 GenericMemberRefExpr *E,
                                 Explosion &out) {
  // The l-value case should have been weeded out.
  assert(!E->getType()->is<LValueType>());

  // The remaining case is to construct an implicit closure.
  // Just refuse to do this for now.
  assert(E->getType()->is<AnyFunctionType>());
  IGF.unimplemented(E->getLoc(),
              "forming implicit closure over generic member reference");
  IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()), out);  
}
