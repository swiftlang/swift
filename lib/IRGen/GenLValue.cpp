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
#include "GenClass.h"
#include "GenFunc.h"
#include "GenInit.h"
#include "GenStruct.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"
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
                                                 /*preserve*/ false);

    // Otherwise, load and materialize the result into memory.
    address = component.asLogical().loadAndMaterialize(*this, NotOnHeap,
                                                       address,
                                                       /*preserve*/ false);
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
                                                    /*preserve*/ false);

    // Otherwise, load and materialize the result into memory.
    baseAddress = component.asLogical().loadAndMaterialize(IGF, NotOnHeap,
                                                           baseAddress,
                                                           /*preserve*/ false);
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
      return component.storeRValue(IGF, finalExpr, base, /*preserve*/ false);
    return component.storeExplosion(IGF, *finalExplosion, base,
                                    /*preserve*/ false);
  }

  // Otherwise, load and materialize into a temporary.
  Address temp = component.loadAndMaterialize(IGF, NotOnHeap, base,
                                              /*preserve*/ true);

  // Recursively perform the store.
  emitAssignRecursive(IGF, temp, finalType, finalExpr, finalExplosion,
                      pathStart, pathEnd);

  // Store the temporary back.
  component.storeMaterialized(IGF, temp, base, /*preserve*/ false);
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
                                                         /*preserve*/ false);
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
  // For now, the answer is yes only if the variable is a property.
  // FIXME: resilience
  return var->isProperty();
}

namespace {
  class GetterSetterTarget {
  public:
    enum Kind {
      IndependentVar, ByrefMemberVar, ByvalMemberVar,
      ByrefMemberSubscript, ByvalMemberSubscript
    };
    static bool isSubscriptKind(Kind kind) {
      return kind == ByrefMemberSubscript ||
             kind == ByvalMemberSubscript;
    }
    static bool isByrefMemberKind(Kind kind) {
      return kind == ByrefMemberVar ||
             kind == ByrefMemberSubscript;
    }
    static bool isByvalMemberKind(Kind kind) {
      return kind == ByvalMemberVar ||
             kind == ByvalMemberSubscript;
    }
    static bool isMemberKind(Kind kind) {
      return kind != IndependentVar;
    }

  private:
    GetterSetterTarget(Kind kind, ValueDecl *decl, unsigned baseSize = 0,
                       unsigned indexSize = 0)
      : Target(decl), TargetKind(kind), BaseExplosionSize(baseSize),
        IndexExplosionSize(indexSize) {
      assert(!isSubscript() || isa<SubscriptDecl>(decl));
      assert(!isVar() || isa<VarDecl>(decl));
      assert(isByvalMember() || baseSize == 0);
      assert(isSubscript() || indexSize == 0);
    }

  public:
    static GetterSetterTarget forByrefMemberVar(VarDecl *var) {
      return GetterSetterTarget(Kind::ByrefMemberVar, var);
    }
    static GetterSetterTarget forByvalMemberVar(VarDecl *var,
                                                unsigned baseSize) {
      return GetterSetterTarget(Kind::ByvalMemberVar, var, baseSize);
    }
    static GetterSetterTarget forIndependentVar(VarDecl *var) {
      return GetterSetterTarget(Kind::IndependentVar, var);
    }
    static GetterSetterTarget
    forByrefMemberSubscript(SubscriptDecl *subscript,
                            unsigned indexSize) {
      return GetterSetterTarget(Kind::ByrefMemberSubscript,
                                subscript, /*baseSize*/ 0, indexSize);
    }
    static GetterSetterTarget
    forByvalMemberSubscript(SubscriptDecl *subscript,
                            unsigned baseSize,
                            unsigned indexSize) {
      return GetterSetterTarget(Kind::ByvalMemberSubscript,
                                subscript, baseSize, indexSize);
    }

    Kind getKind() const { return TargetKind; }
    bool isSubscript() const { return isSubscriptKind(getKind()); }
    bool isVar() const { return !isSubscript(); }
    bool isByrefMember() const { return isByrefMemberKind(getKind()); }
    bool isByvalMember() const { return isByvalMemberKind(getKind()); }
    bool isMember() const { return isMemberKind(getKind()); }

    ValueDecl *getDecl() const { return Target; }
    VarDecl *getVarDecl() const {
      assert(!isSubscript());
      return cast<VarDecl>(getDecl());
    }
    SubscriptDecl *getSubscriptDecl() const {
      assert(isSubscript());
      return cast<SubscriptDecl>(getDecl());
    }

    /// Returns the size of the byval base as a maximal explosion.
    unsigned getBaseExplosionSize() const {
      assert(isByvalMember());
      return BaseExplosionSize;
    }

    /// Returns the size of the index value as a maximal explosion.
    unsigned getIndexExplosionSize() const {
      assert(isSubscript());
      return IndexExplosionSize;
    }

    /// Find the formal type of this object.
    Type getType() const {
      if (isSubscript())
        return getSubscriptDecl()->getElementType();
      return getVarDecl()->getType();
    }

    Type getByvalType() const {
      return getDecl()->getDeclContext()->getDeclaredTypeOfContext();
    }

    Type getIndexType() const {
      return getSubscriptDecl()->getIndices()->getType();
    }

    /// Find the address of the getter function.
    Callee getGetter(IRGenFunction &IGF, ExplosionKind explosionLevel) const {
      // For now, always be pessimistic.
      explosionLevel = ExplosionKind::Minimal;

      // The uncurry level is 0 unless we have a 'this' argument.
      llvm::Constant *fn = IGF.IGM.getAddrOfGetter(getDecl(), explosionLevel);
      if (isMember()) {
        return Callee::forMethod(Type(), fn, explosionLevel, 1);
      } else {
        return Callee::forFreestandingFunction(Type(), fn, explosionLevel, 0);
      }
    }

    /// Find the address of the getter function.
    Callee getSetter(IRGenFunction &IGF, ExplosionKind explosionLevel) const {
      // For now, always be pessimistic.
      explosionLevel = ExplosionKind::Minimal;

      // The uncurry level is 0 unless we have a 'this' argument.
      llvm::Constant *fn = IGF.IGM.getAddrOfSetter(getDecl(), explosionLevel);
      if (isMember()) {
        return Callee::forMethod(Type(), fn, explosionLevel, 1);
      } else {
        return Callee::forFreestandingFunction(Type(), fn, explosionLevel, 0);
      }
    }

  private:
    ValueDecl* Target;
    Kind TargetKind;
    unsigned BaseExplosionSize;
    unsigned IndexExplosionSize;
  };

  class GetterSetterComponent : public LogicalPathComponent {
    GetterSetterTarget Target;

    const ManagedValue *getBaseValuesBuffer() const {
      return reinterpret_cast<const ManagedValue*>(this + 1);
    }
    ManagedValue *getBaseValuesBuffer() {
      return reinterpret_cast<ManagedValue*>(this + 1);
    }
    const ManagedValue *getIndexValuesBuffer() const {
      auto values = reinterpret_cast<const ManagedValue*>(this + 1);
      if (Target.isByvalMember())
        values += Target.getBaseExplosionSize();
      return values;
    }
    ManagedValue *getIndexValuesBuffer() {
      auto values = reinterpret_cast<ManagedValue*>(this + 1);
      if (Target.isByvalMember())
        values += Target.getBaseExplosionSize();
      return values;
    }

  public:
    GetterSetterComponent(GetterSetterTarget &&target) : Target(target) {}

    /// Required by LValue::addWithExtra.
    static size_t extra_storage_size(const GetterSetterTarget &target) {
      assert(target.isByvalMember() || target.isSubscript());
      size_t size = 0;
      if (target.isByvalMember())
        size += sizeof(ManagedValue) * target.getBaseExplosionSize();
      if (target.isSubscript())
        size += sizeof(ManagedValue) * target.getIndexExplosionSize();
      return size;
    }

    void initBaseValues(Explosion &values) {
      assert(values.getKind() == ExplosionKind::Maximal &&
             "cannot store non-maximal index explosion in "
             "GetterSetterComponent!");
      assert(values.size() == Target.getBaseExplosionSize());
      std::memcpy(getBaseValuesBuffer(), values.claimAll().data(),
                  sizeof(ManagedValue) * Target.getBaseExplosionSize());
    }

    void initIndexValues(Explosion &values) {
      assert(values.getKind() == ExplosionKind::Maximal &&
             "cannot store non-maximal index explosion in "
             "GetterSetterComponent!");
      assert(values.size() == Target.getIndexExplosionSize());
      std::memcpy(getIndexValuesBuffer(), values.claimAll().data(),
                  sizeof(ManagedValue) * Target.getIndexExplosionSize());
    }

    /// Add the base values to the given explosion.
    void addBaseValues(IRGenFunction &IGF, Explosion &out,
                       bool preserve) const {
      // The stored index values come from a maximal explosion.
      ArrayRef<ManagedValue> storedValues(getBaseValuesBuffer(),
                                          Target.getBaseExplosionSize());

      // Just copy them in straight if the target is maximal.
      if (!preserve && out.getKind() == ExplosionKind::Maximal)
        return out.add(storedValues);

      // Otherwise, reexplode.
      Explosion storedExplosion(ExplosionKind::Maximal);
      storedExplosion.add(storedValues);

      Type byvalType = Target.getByvalType();
      const TypeInfo &byvalTI = IGF.getFragileTypeInfo(byvalType);
      if (preserve) {
        byvalTI.copy(IGF, storedExplosion, out);
      } else {
        byvalTI.reexplode(IGF, storedExplosion, out);
      }
    }

    /// Add the index values to the given explosion.
    void addIndexValues(IRGenFunction &IGF, Explosion &out,
                        bool preserve) const {
      // The stored index values come from a maximal explosion.
      ArrayRef<ManagedValue> storedValues(getIndexValuesBuffer(),
                                          Target.getIndexExplosionSize());

      // Just copy them in straight if the target is maximal.
      if (!preserve && out.getKind() == ExplosionKind::Maximal)
        return out.add(storedValues);

      // Otherwise, reexplode.
      Explosion storedExplosion(ExplosionKind::Maximal);
      storedExplosion.add(storedValues);

      Type indexType = Target.getIndexType();
      const TypeInfo &indexTI = IGF.getFragileTypeInfo(indexType);
      if (preserve) {
        indexTI.copy(IGF, storedExplosion, out);
      } else {
        indexTI.reexplode(IGF, storedExplosion, out);
      }
    }

    /// A helper routine for performing code common to all store paths.
    void store(IRGenFunction &IGF, const Callee &setter, Address base,
               Explosion &rawValue, const TypeInfo &valueTI,
               bool preserve) const {
      // An explosion for the 'self' argument, if required.
      Explosion selfArg(setter.getExplosionLevel());

      SmallVector<Arg, 2> args;

      // A byref member requires 'self' as its first argument.
      if (Target.isByrefMember()) {
        selfArg.addUnmanaged(base.getAddress());
        args.push_back(Arg::forUnowned(selfArg));
      } else if (Target.isByvalMember()) {
        addBaseValues(IGF, selfArg, preserve);
        args.push_back(Arg::forUnowned(selfArg));
      }

      // Add the value argument.
      // In a subscript, this is a tuple of the subscript and the value.
      Explosion value(setter.getExplosionLevel());
      if (Target.isSubscript()) {
        addIndexValues(IGF, value, preserve);
        valueTI.reexplode(IGF, rawValue, value);
        args.push_back(Arg::forUnowned(value));

      // Otherwise, it's just the original value.
      } else {
        args.push_back(Arg::forUnowned(rawValue, valueTI));
      }

      // Make the call.
      emitVoidCall(IGF, setter, args);
    }

    void storeRValue(IRGenFunction &IGF, Expr *rvalue,
                     Address base, bool preserve) const {
      Callee setter = Target.getSetter(IGF, ExplosionKind::Maximal);

      // Emit the r-value at the natural level of the setter we found.
      Explosion value(setter.getExplosionLevel());
      IGF.emitRValue(rvalue, value);
      
      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      store(IGF, setter, base, value, valueTI, preserve);
    }

    void storeMaterialized(IRGenFunction &IGF, Address temp,
                           Address base, bool preserve) const {
      Callee setter = Target.getSetter(IGF, ExplosionKind::Maximal);

      // Explode the value at the natural level of the setter we found.
      Explosion value(setter.getExplosionLevel());
      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      valueTI.load(IGF, temp, value);

      store(IGF, setter, base, value, valueTI, preserve);
    }

    void storeExplosion(IRGenFunction &IGF, Explosion &value,
                        Address base, bool preserve) const {
      Callee setter = Target.getSetter(IGF, ExplosionKind::Maximal);
      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      store(IGF, setter, base, value, valueTI, preserve);
    }

    void loadExplosion(IRGenFunction &IGF, Address base, Explosion &out,
                       bool preserve) const {
      Callee getter = Target.getGetter(IGF, out.getKind());

      // Build the arguments.
      SmallVector<Arg, 2> args;

      // Byref members need a 'self' argument.
      Explosion selfArg(getter.getExplosionLevel());
      if (Target.isByrefMember()) {
        selfArg.addUnmanaged(base.getAddress());

        // We can use an untyped Arg because we perfectly match the
        // getter's explosion level.
        args.push_back(Arg::forUnowned(selfArg));
      } else if (Target.isByvalMember()) {
        addBaseValues(IGF, selfArg, preserve);
        args.push_back(Arg::forUnowned(selfArg));
      }

      // The next argument is the "index".
      // In a subscript, this is a tuple of the subscript and the value.
      Explosion index(getter.getExplosionLevel());
      if (Target.isSubscript()) {
        addIndexValues(IGF, index, preserve);
        args.push_back(Arg::forUnowned(index));

      // Otherwise, it's an empty tuple.
      } else {
        args.push_back(Arg());
      }

      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      emitCall(IGF, getter, args, valueTI, out);
    }

    void loadMaterialized(IRGenFunction &IGF, Address base, Address temp,
                          bool preserve) const {
      Callee getter = Target.getGetter(IGF, ExplosionKind::Maximal);

      // Build the arguments.
      SmallVector<Arg, 2> args;

      // Byref members need a 'self' argument.
      Explosion selfArg(getter.getExplosionLevel());
      if (Target.isByrefMember()) {
        selfArg.addUnmanaged(base.getAddress());

        // We can use an untyped Arg because we perfectly match the
        // getter's explosion level.
        args.push_back(Arg::forUnowned(selfArg));
      } else if (Target.isByvalMember()) {
        addBaseValues(IGF, selfArg, preserve);
        args.push_back(Arg::forUnowned(selfArg));
      }

      // The next argument is the "index".
      // In a subscript, this is a tuple of the subscript and the value.
      Explosion index(getter.getExplosionLevel());
      if (Target.isSubscript()) {
        addIndexValues(IGF, index, preserve);
        args.push_back(Arg::forUnowned(index));

      // Otherwise, it's an empty tuple.
      } else {
        args.push_back(Arg());
      }

      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      emitCallToMemory(IGF, getter, args, valueTI, temp);
    }

    OwnedAddress loadAndMaterialize(IRGenFunction &IGF, OnHeap_t onHeap,
                                    Address base, bool preserve) const {
      Initialization init;
      InitializedObject object = init.getObjectForTemporary();

      const TypeInfo &valueTI = IGF.getFragileTypeInfo(Target.getType());
      init.registerObject(IGF, object, onHeap, valueTI);

      // Emit a local allocation for the object.
      OwnedAddress addr = init.emitLocalAllocation(IGF, object, onHeap,
                                                   valueTI, "temporary");

      // Load into the new memory.
      GetterSetterComponent::loadMaterialized(IGF, base, addr, preserve);

      // The load is complete;  mark the memory as initialized.
      init.markInitialized(IGF, object);

      return addr;
    }
  };
}

/// Emit an l-value for the given member reference.
LValue swift::irgen::emitMemberRefLValue(IRGenFunction &IGF, MemberRefExpr *E) {
  VarDecl *var = E->getDecl();

  if (E->getBase()->getType()->hasReferenceSemantics()) {
    if (!isVarAccessLogical(IGF, var)) {
      if (isa<ClassDecl>(var->getDeclContext()))
        return emitPhysicalClassMemberLValue(IGF, E);
      llvm_unreachable("Unexpected physical lvalue MemberRefExpr");
    }

    // Emit the base value.
    Explosion base(ExplosionKind::Maximal);
    IGF.emitRValue(E->getBase(), base);

    // Build the logical lvalue.
    LValue lvalue;
    GetterSetterComponent &component =
      lvalue.addWithExtra<GetterSetterComponent>(
              GetterSetterTarget::forByvalMemberVar(var, base.size()));
    component.initBaseValues(base);
    return lvalue;
  }

  if (!isVarAccessLogical(IGF, var)) {
    if (isa<StructDecl>(var->getDeclContext()))
      return emitPhysicalStructMemberLValue(IGF, E);

    llvm_unreachable("Unexpected physical lvalue MemberRefExpr");
  }

  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Push a logical member reference.
  lvalue.add<GetterSetterComponent>(GetterSetterTarget::forByrefMemberVar(var));

  return lvalue;
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
    lvalue.add<GetterSetterComponent>(
                                 GetterSetterTarget::forIndependentVar(var));
    return lvalue;
  }

  // Otherwise we can use a physical-address component.
  OwnedAddress addr(IGM.getAddrOfGlobalVariable(var), IGM.RefCountedNull);
  return emitAddressLValue(addr);
}

/// Emit an l-value for the given subscripted reference.
LValue swift::irgen::emitSubscriptLValue(IRGenFunction &IGF, SubscriptExpr *E) {
  if (E->getBase()->getType()->hasReferenceSemantics()) {
    // Emit the base value.
    Explosion base(ExplosionKind::Maximal);
    IGF.emitRValue(E->getBase(), base);

    // Emit the index.
    Explosion index(ExplosionKind::Maximal);
    IGF.emitRValue(E->getIndex(), index);

    // Build the logical lvalue.
    LValue lvalue;
    GetterSetterComponent &component =
      lvalue.addWithExtra<GetterSetterComponent>(
                GetterSetterTarget::forByvalMemberSubscript(E->getDecl(),
                                                            base.size(),
                                                            index.size()));
    component.initBaseValues(base);
    component.initIndexValues(index);
    return lvalue;
  }
  
  // Emit the base l-value.
  LValue lvalue = IGF.emitLValue(E->getBase());

  // Emit the index.  GetterSetterComponent requires this to be
  // maximal; in theory it might be better to try to match the
  // getter/setter, but that would require some significant
  // complexity.
  Explosion index(ExplosionKind::Maximal);
  IGF.emitRValue(E->getIndex(), index);

  // Subscript accesses are always logical (for now).
  GetterSetterComponent &component =
    lvalue.addWithExtra<GetterSetterComponent>(
                GetterSetterTarget::forByrefMemberSubscript(E->getDecl(),
                                                            index.size()));
  component.initIndexValues(index);

  return lvalue;
}
