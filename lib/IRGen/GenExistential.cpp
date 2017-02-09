//===--- GenExistential.cpp - Swift IR Generation for Existential Types ---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for existential types in Swift.
//
//===----------------------------------------------------------------------===//

#include "GenExistential.h"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/Types.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/TypeLowering.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "EnumPayload.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenProto.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IndirectTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "NonFixedTypeInfo.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"
#include "WeakTypeInfo.h"

using namespace swift;
using namespace irgen;

namespace {
  /// The layout of an existential buffer.  This is intended to be a
  /// small, easily-computed type that can be passed around by value.
  class OpaqueExistentialLayout {
  private:
    unsigned NumTables;
    // If you add anything to the layout computation, you might need
    // to update certain uses;  check the external uses of getNumTables().
    // For example, getAssignExistentialsFunction relies on being uniqued
    // for different layout kinds.

  public:
    explicit OpaqueExistentialLayout(unsigned numTables)
      : NumTables(numTables) {}

    unsigned getNumTables() const { return NumTables; }

    Size getSize(IRGenModule &IGM) const {
      return getFixedBufferSize(IGM)
           + IGM.getPointerSize() * (getNumTables() + 1);
    }

    Alignment getAlignment(IRGenModule &IGM) const {
      return getFixedBufferAlignment(IGM);
    }

    // friend bool operator==(ExistentialLayout a, ExistentialLayout b) {
    //   return a.NumTables == b.NumTables;
    // }

    /// Given the address of an existential object, drill down to the
    /// buffer.
    Address projectExistentialBuffer(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateStructGEP(addr, 0, Size(0));

    }

    /// Given the address of an existential object, drill down to the
    /// witness-table field.
    Address projectWitnessTable(IRGenFunction &IGF, Address addr,
                                unsigned which) const {
      assert(which < getNumTables());
      return IGF.Builder.CreateStructGEP(addr, which + 2,
                                         getFixedBufferSize(IGF.IGM)
                                         + IGF.IGM.getPointerSize() * (which + 1));
    }

    /// Given the address of an existential object, load its witness table.
    llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address addr,
                                  unsigned which) const {
      return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr, which));
    }

    /// Given the address of an existential object, drill down to the
    /// metadata field.
    Address projectMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 1, getFixedBufferSize(IGF.IGM));
    }

    /// Given the address of an existential object, load its metadata
    /// object.
    llvm::Value *loadMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateLoad(projectMetadataRef(IGF, addr));
    }
  };
} // end anonymous namespace


/// Given the address of an existential object, destroy it.
static void emitDestroyExistential(IRGenFunction &IGF, Address addr,
                                   OpaqueExistentialLayout layout) {
  llvm::Value *metadata = layout.loadMetadataRef(IGF, addr);

  Address object = layout.projectExistentialBuffer(IGF, addr);
  emitDestroyBufferCall(IGF, metadata, object);
}

static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                               llvm::Type *objectPtrTy,
                                               OpaqueExistentialLayout layout);


namespace {

/// A helper class for implementing existential type infos that
/// store an existential value of some sort.
template <class Derived, class Base>
class ExistentialTypeInfoBase : public Base,
    private llvm::TrailingObjects<Derived, ProtocolEntry> {
  friend class llvm::TrailingObjects<Derived, ProtocolEntry>;

  /// The number of non-trivial protocols for this existential.
  unsigned NumStoredProtocols;

protected:
  const Derived &asDerived() const {
    return *static_cast<const Derived*>(this);
  }
  Derived &asDerived() {
    return *static_cast<Derived*>(this);
  }

  template <class... As>
  ExistentialTypeInfoBase(ArrayRef<ProtocolEntry> protocols,
                          As &&...args)
      : Base(std::forward<As>(args)...),
        NumStoredProtocols(protocols.size()) {
    std::uninitialized_copy(protocols.begin(), protocols.end(),
                            this->template getTrailingObjects<ProtocolEntry>());
  }

public:
  template <class... As>
  static const Derived *
  create(ArrayRef<ProtocolEntry> protocols, As &&...args)
  {
    void *buffer =
        operator new(ExistentialTypeInfoBase::template totalSizeToAlloc<ProtocolEntry>(protocols.size()));
    return new (buffer) Derived(protocols, std::forward<As>(args)...);
  }

  /// Returns the number of protocol witness tables directly carried
  /// by values of this type.
  unsigned getNumStoredProtocols() const { return NumStoredProtocols; }

  /// Returns the protocols that values of this type are known to
  /// implement.  This can be empty, meaning that values of this
  /// type are not know to implement any protocols, although we do
  /// still know how to manipulate them.
  ArrayRef<ProtocolEntry> getStoredProtocols() const {
    return {this->template getTrailingObjects<ProtocolEntry>(),
            NumStoredProtocols};
  }

  /// Given an existential object, find the witness table
  /// corresponding to the given protocol.
  llvm::Value *findWitnessTable(IRGenFunction &IGF,
                                Explosion &container,
                                ProtocolDecl *protocol) const {
    assert(NumStoredProtocols != 0 &&
           "finding a witness table in a trivial existential");

    return emitImpliedWitnessTableRef(IGF, getStoredProtocols(), protocol,
      [&](unsigned originIndex) {
        return asDerived().extractWitnessTable(IGF, container, originIndex);
      });
  }

  /// Given the address of an existential object, find the witness
  /// table corresponding to the given protocol.
  llvm::Value *findWitnessTable(IRGenFunction &IGF, Address obj,
                                ProtocolDecl *protocol) const {
    assert(NumStoredProtocols != 0 &&
           "finding a witness table in a trivial existential");

    return emitImpliedWitnessTableRef(IGF, getStoredProtocols(), protocol,
      [&](unsigned originIndex) {
        return asDerived().loadWitnessTable(IGF, obj, originIndex);
      });
  }

  /// Given the witness table vector from an existential object, find the
  /// witness table corresponding to the given protocol.
  llvm::Value *findWitnessTable(IRGenFunction &IGF,
                                ArrayRef<llvm::Value *> witnesses,
                                ProtocolDecl *protocol) const {
    return emitImpliedWitnessTableRef(IGF, getStoredProtocols(), protocol,
      [&](unsigned originIndex) {
        return witnesses[originIndex];
      });
  }

  /// Given the address of an existential object, find the witness
  /// table of a directly-stored witness table.
  llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address obj,
                                unsigned which) const {
    return IGF.Builder.CreateLoad(
                         asDerived().projectWitnessTable(IGF, obj, which));
  }

  void emitCopyOfTables(IRGenFunction &IGF, Address dest, Address src) const {
    if (NumStoredProtocols == 0) return;

    Explosion temp;
    asDerived().emitLoadOfTables(IGF, src, temp);
    asDerived().emitStoreOfTables(IGF, temp, dest);
  }

  void emitLoadOfTables(IRGenFunction &IGF, Address existential,
                        Explosion &out) const {
    for (unsigned i = 0; i != NumStoredProtocols; ++i) {
      auto tableAddr = asDerived().projectWitnessTable(IGF, existential, i);
      out.add(IGF.Builder.CreateLoad(tableAddr));
    }
  }

  void emitStoreOfTables(IRGenFunction &IGF, Explosion &in,
                         Address existential) const {
    for (unsigned i = 0; i != NumStoredProtocols; ++i) {
      auto tableAddr = asDerived().projectWitnessTable(IGF, existential, i);
      IGF.Builder.CreateStore(in.claimNext(), tableAddr);
    }
  }
};

/// A TypeInfo implementation for existential types, i.e., types like:
///   Printable
///   Printable & Serializable
///   Any
/// with the semantic translation:
///   \exists t : Printable . t
/// t here is an ArchetypeType.
///
/// This is used for both ProtocolTypes and ProtocolCompositionTypes.
class OpaqueExistentialTypeInfo final :
    public ExistentialTypeInfoBase<OpaqueExistentialTypeInfo,
             IndirectTypeInfo<OpaqueExistentialTypeInfo, FixedTypeInfo>> {

  using super =
           ExistentialTypeInfoBase<OpaqueExistentialTypeInfo,
             IndirectTypeInfo<OpaqueExistentialTypeInfo, FixedTypeInfo>>;
  friend super;

  // FIXME: We could get spare bits out of the metadata and/or witness
  // pointers.
  OpaqueExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                            llvm::Type *ty, Size size, Alignment align)
    : super(protocols, ty, size,
            SpareBitVector::getConstant(size.getValueInBits(), false), align,
            IsNotPOD, IsNotBitwiseTakable, IsFixedSize) {}

public:
  OpaqueExistentialLayout getLayout() const {
    return OpaqueExistentialLayout(getNumStoredProtocols());
  }

  Address projectWitnessTable(IRGenFunction &IGF, Address obj,
                              unsigned index) const {
    return getLayout().projectWitnessTable(IGF, obj, index);
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    auto objPtrTy = dest.getAddress()->getType();
    auto fn = getAssignExistentialsFunction(IGF.IGM, objPtrTy, getLayout());
    auto call = IGF.Builder.CreateCall(
        fn, {dest.getAddress(), src.getAddress()});
    call->setCallingConv(IGF.IGM.DefaultCC);
    call->setDoesNotThrow();
  }

  llvm::Value *copyType(IRGenFunction &IGF, Address dest, Address src) const {
    auto layout = getLayout();

    llvm::Value *metadata = layout.loadMetadataRef(IGF, src);
    IGF.Builder.CreateStore(metadata, layout.projectMetadataRef(IGF, dest));

    // Load the witness tables and copy them into the new object.
    emitCopyOfTables(IGF, dest, src);

    return metadata;
  }

  void initializeWithCopy(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    llvm::Value *metadata = copyType(IGF, dest, src);

    auto layout = getLayout();

    // Project down to the buffers and ask the witnesses to do a
    // copy-initialize.
    Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
    Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
    emitInitializeBufferWithCopyOfBufferCall(IGF, metadata,
                                             destBuffer,
                                             srcBuffer);
  }

  void initializeWithTake(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    llvm::Value *metadata = copyType(IGF, dest, src);

    auto layout = getLayout();

    // Project down to the buffers and ask the witnesses to do a
    // take-initialize.
    Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
    Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
    emitInitializeBufferWithTakeOfBufferCall(IGF, metadata,
                                             destBuffer,
                                             srcBuffer);
  }

  void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
    emitDestroyExistential(IGF, addr, getLayout());
  }
};

/// A type implementation for address-only reference storage of
/// class existential types.
template <class Impl, class Base>
class AddressOnlyClassExistentialTypeInfoBase :
    public ExistentialTypeInfoBase<Impl, IndirectTypeInfo<Impl, Base>> {
  using super = ExistentialTypeInfoBase<Impl, IndirectTypeInfo<Impl, Base>>;

  using super::asDerived;
  using super::emitCopyOfTables;
  using super::getNumStoredProtocols;

protected:
  const ReferenceCounting Refcounting;

  template <class... As>
  AddressOnlyClassExistentialTypeInfoBase(ArrayRef<ProtocolEntry> protocols,
                                          ReferenceCounting refcounting,
                                          As &&...args)
    : super(protocols, std::forward<As>(args)...),
      Refcounting(refcounting) {
  }

public:
  Address projectWitnessTable(IRGenFunction &IGF, Address container,
                              unsigned index) const {
    assert(index < getNumStoredProtocols());
    return IGF.Builder.CreateStructGEP(container, index + 1,
                                  (index + 1) * IGF.IGM.getPointerSize());
  }

  Address projectValue(IRGenFunction &IGF, Address existential) const {
    return IGF.Builder.CreateStructGEP(existential, 0, Size(0),
                          existential.getAddress()->getName() + ".weakref");
  }

  void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                      SILType T) const override {
    Address destValue = projectValue(IGF, dest);
    Address srcValue = projectValue(IGF, src);
    asDerived().emitValueAssignWithCopy(IGF, destValue, srcValue);
    emitCopyOfTables(IGF, dest, src);
  }

  void initializeWithCopy(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    Address destValue = projectValue(IGF, dest);
    Address srcValue = projectValue(IGF, src);
    asDerived().emitValueInitializeWithCopy(IGF, destValue, srcValue);
    emitCopyOfTables(IGF, dest, src);
  }

  void assignWithTake(IRGenFunction &IGF,
                      Address dest, Address src,
                      SILType T) const override {
    Address destValue = projectValue(IGF, dest);
    Address srcValue = projectValue(IGF, src);
    asDerived().emitValueAssignWithTake(IGF, destValue, srcValue);
    emitCopyOfTables(IGF, dest, src);
  }

  void initializeWithTake(IRGenFunction &IGF,
                          Address dest, Address src,
                          SILType T) const override {
    Address destValue = projectValue(IGF, dest);
    Address srcValue = projectValue(IGF, src);
    asDerived().emitValueInitializeWithTake(IGF, destValue, srcValue);
    emitCopyOfTables(IGF, dest, src);
  }

  void destroy(IRGenFunction &IGF, Address existential,
               SILType T) const override {
    Address valueAddr = projectValue(IGF, existential);
    asDerived().emitValueDestroy(IGF, valueAddr);
  }

  /// Given an explosion with multiple pointer elements in them, pack them
  /// into an enum payload explosion.
  /// FIXME: Assumes the explosion is broken into word-sized integer chunks.
  /// Should use EnumPayload.
  void mergeExplosion(Explosion &In, Explosion &Out, IRGenFunction &IGF)
  const {
    // We always have at least one entry.
    auto *part = In.claimNext();
    Out.add(IGF.Builder.CreatePtrToInt(part, IGF.IGM.IntPtrTy));

    for (unsigned i = 0; i != getNumStoredProtocols(); ++i) {
      part = In.claimNext();
      Out.add(IGF.Builder.CreatePtrToInt(part, IGF.IGM.IntPtrTy));
    }
  }

  // Given an exploded enum payload consisting of consecutive word-sized
  // chunks, cast them to their underlying component types.
  // FIXME: Assumes the payload is word-chunked. Should use
  void decomposeExplosion(Explosion &InE, Explosion &OutE,
                          IRGenFunction &IGF) const {
    // The first entry is always the weak*.
    llvm::Value *weak = InE.claimNext();
    if (Refcounting == ReferenceCounting::Native)
      OutE.add(IGF.Builder.CreateBitOrPointerCast(weak,
                                          IGF.IGM.RefCountedPtrTy));
    else
      OutE.add(IGF.Builder.CreateBitOrPointerCast(weak,
                                          IGF.IGM.UnknownRefCountedPtrTy));

    // Collect the witness tables.
    for (unsigned i = 0, e = getNumStoredProtocols(); i != e; ++i) {
      llvm::Value *witness = InE.claimNext();
      OutE.add(IGF.Builder.CreateBitOrPointerCast(witness,
                                                  IGF.IGM.WitnessTablePtrTy));
    }
  }
};

/// A type implementation for 'weak' existential types.
class WeakClassExistentialTypeInfo final :
    public AddressOnlyClassExistentialTypeInfoBase<WeakClassExistentialTypeInfo,
                                                   WeakTypeInfo> {
public:
  WeakClassExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                               llvm::Type *ty, Size size, Alignment align,
                               SpareBitVector &&spareBits,
                               ReferenceCounting refcounting)
    : AddressOnlyClassExistentialTypeInfoBase(protocols, refcounting,
                                              ty, size, align,
                                              std::move(spareBits)) {
  }

  void emitValueAssignWithCopy(IRGenFunction &IGF,
                               Address dest, Address src) const {
    IGF.emitWeakCopyAssign(dest, src, Refcounting);
  }

  void emitValueInitializeWithCopy(IRGenFunction &IGF,
                                   Address dest, Address src) const {
    IGF.emitWeakCopyInit(dest, src, Refcounting);
  }

  void emitValueAssignWithTake(IRGenFunction &IGF,
                               Address dest, Address src) const {
    IGF.emitWeakTakeAssign(dest, src, Refcounting);
  }

  void emitValueInitializeWithTake(IRGenFunction &IGF,
                                   Address dest, Address src) const {
    IGF.emitWeakTakeInit(dest, src, Refcounting);
  }

  void emitValueDestroy(IRGenFunction &IGF, Address addr) const {
    IGF.emitWeakDestroy(addr, Refcounting);
  }

  // These explosions must follow the same schema as
  // ClassExistentialTypeInfo, i.e. first the value, then the tables.

  void weakLoadStrong(IRGenFunction &IGF, Address existential,
                      Explosion &out) const override {
    Explosion temp;
    Address valueAddr = projectValue(IGF, existential);
    llvm::Type *resultType = IGF.IGM.getReferenceType(Refcounting);
    temp.add(IGF.emitWeakLoadStrong(valueAddr, resultType, Refcounting));
    emitLoadOfTables(IGF, existential, temp);
    mergeExplosion(temp, out, IGF);
  }

  void weakTakeStrong(IRGenFunction &IGF, Address existential,
                      Explosion &out) const override {
    Explosion temp;
    Address valueAddr = projectValue(IGF, existential);
    llvm::Type *resultType = IGF.IGM.getReferenceType(Refcounting);
    temp.add(IGF.emitWeakTakeStrong(valueAddr, resultType, Refcounting));
    emitLoadOfTables(IGF, existential, temp);
    mergeExplosion(temp, out, IGF);
  }

  void weakInit(IRGenFunction &IGF, Explosion &in,
                Address existential) const override {
    Explosion temp;
    decomposeExplosion(in, temp, IGF);

    llvm::Value *value = temp.claimNext();
    assert(value->getType() == IGF.IGM.getReferenceType(Refcounting));
    emitStoreOfTables(IGF, temp, existential);
    Address valueAddr = projectValue(IGF, existential);
    IGF.emitWeakInit(value, valueAddr, Refcounting);
  }

  void weakAssign(IRGenFunction &IGF, Explosion &in,
                  Address existential) const override {
    Explosion temp;
    decomposeExplosion(in, temp, IGF);

    llvm::Value *value = temp.claimNext();
    assert(value->getType() == IGF.IGM.getReferenceType(Refcounting));
    emitStoreOfTables(IGF, temp, existential);
    Address valueAddr = projectValue(IGF, existential);
    IGF.emitWeakAssign(value, valueAddr, Refcounting);
  }
};

/// A type implementation for address-only @unowned existential types.
class AddressOnlyUnownedClassExistentialTypeInfo final :
    public AddressOnlyClassExistentialTypeInfoBase<
                                    AddressOnlyUnownedClassExistentialTypeInfo,
                                                   FixedTypeInfo> {
public:
  AddressOnlyUnownedClassExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                                             llvm::Type *ty,
                                             SpareBitVector &&spareBits,
                                             Size size, Alignment align,
                                             ReferenceCounting refcounting)
    : AddressOnlyClassExistentialTypeInfoBase(protocols, refcounting,
                                              ty, size, std::move(spareBits),
                                              align, IsNotPOD,
                                              IsNotBitwiseTakable,
                                              IsFixedSize) {
  }

  void emitValueAssignWithCopy(IRGenFunction &IGF,
                               Address dest, Address src) const {
    IGF.emitUnownedCopyAssign(dest, src, Refcounting);
  }

  void emitValueInitializeWithCopy(IRGenFunction &IGF,
                                   Address dest, Address src) const {
    IGF.emitUnownedCopyInit(dest, src, Refcounting);
  }

  void emitValueAssignWithTake(IRGenFunction &IGF,
                              Address dest, Address src) const {
    IGF.emitUnownedTakeAssign(dest, src, Refcounting);
  }

  void emitValueInitializeWithTake(IRGenFunction &IGF,
                                  Address dest, Address src) const {
    IGF.emitUnownedTakeInit(dest, src, Refcounting);
  }

  void emitValueDestroy(IRGenFunction &IGF, Address addr) const {
    IGF.emitUnownedDestroy(addr, Refcounting);
  }

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return IGM.getUnownedExtraInhabitantCount(Refcounting);
  }

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                     unsigned bits,
                                     unsigned index) const override {
    return IGM.getUnownedExtraInhabitantValue(bits, index, Refcounting);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                       SILType T) const override {
    Address valueSrc = projectValue(IGF, src);
    return IGF.getUnownedExtraInhabitantIndex(valueSrc, Refcounting);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T) const override {
    Address valueDest = projectValue(IGF, dest);
    return IGF.storeUnownedExtraInhabitant(index, valueDest, Refcounting);
  }

  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    APInt bits = IGM.getUnownedExtraInhabitantMask(Refcounting);

    // Zext out to the size of the existential.
    bits = bits.zextOrTrunc(getFixedSize().getValueInBits());
    return bits;
  }
};

/// A helper class for working with existential types that can be
/// exploded into scalars.
///
/// The subclass must provide:
///   void emitValueRetain(IRGenFunction &IGF, llvm::Value *value) const;
///   void emitValueRelease(IRGenFunction &IGF, llvm::Value *value) const;
///   void emitValueFixLifetime(IRGenFunction &IGF,
///                               llvm::Value *value) const;
///   const LoadableTypeInfo &
///       getValueTypeInfoForExtraInhabitants(IRGenModule &IGM) const;
/// The value type info is only used to manage extra inhabitants, so it's
/// okay for it to implement different semantics.
template <class Derived, class Base>
class ScalarExistentialTypeInfoBase :
  public ExistentialTypeInfoBase<Derived, ScalarTypeInfo<Derived, Base>> {

  using super =
         ExistentialTypeInfoBase<Derived, ScalarTypeInfo<Derived, Base>>;

protected:
  template <class... T>
  ScalarExistentialTypeInfoBase(T &&...args)
    : super(std::forward<T>(args)...) {}

  using super::asDerived;

public:
  /// The storage type of a class existential is a struct containing
  /// a refcounted pointer to the class instance value followed by
  /// witness table pointers for each conformed-to protocol. Unlike opaque
  /// existentials, a class existential does not need to store type
  /// metadata as an additional element, since it can be derived from the
  /// class instance.
  llvm::StructType *getStorageType() const {
    return cast<llvm::StructType>(TypeInfo::getStorageType());
  }

  using super::getNumStoredProtocols;

  unsigned getExplosionSize() const final {
    return 1 + getNumStoredProtocols();
  }

  void getSchema(ExplosionSchema &schema) const override {
    schema.add(ExplosionSchema::Element::forScalar(asDerived().getValueType()));

    llvm::StructType *ty = getStorageType();
    for (unsigned i = 1, e = getExplosionSize(); i != e; ++i)
      schema.add(ExplosionSchema::Element::forScalar(ty->getElementType(i)));
  }

  void addToAggLowering(IRGenModule &IGM, SwiftAggLowering &lowering,
                        Size offset) const override {
    auto ptrSize = IGM.getPointerSize();
    LoadableTypeInfo::addScalarToAggLowering(IGM, lowering,
                                             asDerived().getValueType(),
                                             offset, ptrSize);

    llvm::StructType *ty = getStorageType();
    for (unsigned i = 1, e = getExplosionSize(); i != e; ++i)
      LoadableTypeInfo::addScalarToAggLowering(IGM, lowering,
                                               ty->getElementType(i),
                                               offset + i * ptrSize, ptrSize);
  }

  /// Given the address of a class existential container, returns
  /// the address of a witness table pointer.
  Address projectWitnessTable(IRGenFunction &IGF, Address address,
                              unsigned n) const {
    assert(n < getNumStoredProtocols() && "witness table index out of bounds");
    return IGF.Builder.CreateStructGEP(address, n+1,
                                       IGF.IGM.getPointerSize() * (n+1));
  }

  /// Return the type of the instance value.
  llvm::Type *getValueType() const {
    return getStorageType()->getElementType(0);
  }

  /// Given the address of a class existential container, returns
  /// the address of its instance pointer.
  Address projectValue(IRGenFunction &IGF, Address address) const {
    return IGF.Builder.CreateStructGEP(address, 0, Size(0));
  }

  llvm::Value *loadValue(IRGenFunction &IGF, Address addr) const {
    return IGF.Builder.CreateLoad(asDerived().projectValue(IGF, addr));
  }

  /// Given a class existential container, returns a witness table
  /// pointer out of the container, and the type metadata pointer for the
  /// value.
  llvm::Value *
  extractWitnessTable(IRGenFunction &IGF, Explosion &container,
                      unsigned which) const {
    assert(which < getNumStoredProtocols() && "witness table index out of bounds");
    ArrayRef<llvm::Value *> values = container.claim(getExplosionSize());
    return values[which+1];
  }

  /// Deconstruct an existential object into witness tables and instance
  /// pointer.
  std::pair<ArrayRef<llvm::Value*>, llvm::Value*>
  getWitnessTablesAndValue(Explosion &container) const {
    llvm::Value *instance = container.claimNext();
    ArrayRef<llvm::Value*> witnesses = container.claim(getNumStoredProtocols());
    return {witnesses, instance};
  }

  /// Given an existential object, returns the payload value.
  llvm::Value *getValue(IRGenFunction &IGF, Explosion &container) const {
    llvm::Value *instance = container.claimNext();
    (void)container.claim(getNumStoredProtocols());
    return instance;
  }

  void loadAsCopy(IRGenFunction &IGF, Address address,
                  Explosion &out) const override {
    // Load the instance pointer, which is unknown-refcounted.
    llvm::Value *instance = asDerived().loadValue(IGF, address);
    asDerived().emitValueRetain(IGF, instance, Atomicity::Atomic);
    out.add(instance);

    // Load the witness table pointers.
    asDerived().emitLoadOfTables(IGF, address, out);
  }

  void loadAsTake(IRGenFunction &IGF, Address address,
                  Explosion &e) const override {
    // Load the instance pointer.
    e.add(asDerived().loadValue(IGF, address));

    // Load the witness table pointers.
    asDerived().emitLoadOfTables(IGF, address, e);
  }

  void assign(IRGenFunction &IGF, Explosion &e,
              Address address) const override {
    // Assign the value.
    Address instanceAddr = asDerived().projectValue(IGF, address);
    llvm::Value *old = IGF.Builder.CreateLoad(instanceAddr);
    IGF.Builder.CreateStore(e.claimNext(), instanceAddr);
    asDerived().emitValueRelease(IGF, old, Atomicity::Atomic);

    // Store the witness table pointers.
    asDerived().emitStoreOfTables(IGF, e, address);
  }

  void initialize(IRGenFunction &IGF, Explosion &e,
                  Address address) const override {
    // Store the instance pointer.
    IGF.Builder.CreateStore(e.claimNext(),
                            asDerived().projectValue(IGF, address));

    // Store the witness table pointers.
    asDerived().emitStoreOfTables(IGF, e, address);
  }

  void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest,
            Atomicity atomicity)
  const override {
    // Copy the instance pointer.
    llvm::Value *value = src.claimNext();
    dest.add(value);
    asDerived().emitValueRetain(IGF, value, atomicity);

    // Transfer the witness table pointers.
    src.transferInto(dest, getNumStoredProtocols());
  }

  void consume(IRGenFunction &IGF, Explosion &src, Atomicity atomicity)
  const override {
    // Copy the instance pointer.
    llvm::Value *value = src.claimNext();
    asDerived().emitValueRelease(IGF, value, atomicity);

    // Throw out the witness table pointers.
    (void)src.claim(getNumStoredProtocols());
  }

  void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {
    // Copy the instance pointer.
    llvm::Value *value = src.claimNext();
    asDerived().emitValueFixLifetime(IGF, value);

    // Throw out the witness table pointers.
    (void)src.claim(getNumStoredProtocols());
  }

  void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
    llvm::Value *value = asDerived().loadValue(IGF, addr);
    asDerived().emitValueRelease(IGF, value, Atomicity::Atomic);
  }

  void packIntoEnumPayload(IRGenFunction &IGF,
                           EnumPayload &payload,
                           Explosion &src,
                           unsigned offset) const override {
    payload.insertValue(IGF, src.claimNext(), offset);
    auto wordSize = IGF.IGM.getPointerSize().getValueInBits();
    for (unsigned i = 0; i < getNumStoredProtocols(); ++i) {
      offset += wordSize;
      payload.insertValue(IGF, src.claimNext(), offset);
    }
  }

  void unpackFromEnumPayload(IRGenFunction &IGF,
                             const EnumPayload &payload,
                             Explosion &dest,
                             unsigned offset) const override {
    ExplosionSchema schema;
    getSchema(schema);
    dest.add(payload.extractValue(IGF, schema[0].getScalarType(), offset));
    auto wordSize = IGF.IGM.getPointerSize().getValueInBits();
    for (unsigned i = 0; i < getNumStoredProtocols(); ++i) {
      offset += wordSize;
      dest.add(payload.extractValue(IGF, IGF.IGM.WitnessTablePtrTy, offset));
    }
  }


  // Extra inhabitants of the various scalar existential containers.
  // We use the heap object extra inhabitants over the class pointer value.
  // We could get even more extra inhabitants from the witness table
  // pointer(s), but it's unlikely we would ever need to.

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    assert(asDerived().getValueTypeInfoForExtraInhabitants(IGM)
                      .mayHaveExtraInhabitants(IGM));
    return true;
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return asDerived().getValueTypeInfoForExtraInhabitants(IGM)
                      .getFixedExtraInhabitantCount(IGM);
  }

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                     unsigned bits,
                                     unsigned index) const override {
    // Note that we pass down the original bit-width.
    return asDerived().getValueTypeInfoForExtraInhabitants(IGM)
                      .getFixedExtraInhabitantValue(IGM, bits, index);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                       SILType T)
  const override {
    // NB: We assume that the witness table slots are zero if an extra
    // inhabitant is stored in the container.
    src = projectValue(IGF, src);
    return asDerived().getValueTypeInfoForExtraInhabitants(IGF.IGM)
                      .getExtraInhabitantIndex(IGF, src, SILType());
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T) const override {
    Address valueDest = projectValue(IGF, dest);
    asDerived().getValueTypeInfoForExtraInhabitants(IGF.IGM)
               .storeExtraInhabitant(IGF, index, valueDest, SILType());
  }

  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    // Ask the value type for its mask.
    APInt bits = asDerived().getValueTypeInfoForExtraInhabitants(IGM)
                            .getFixedExtraInhabitantMask(IGM);
    
    // Zext out to the size of the existential.
    bits = bits.zextOrTrunc(asDerived().getFixedSize().getValueInBits());
    return bits;
  }
};

/// A type implementation for loadable [unowned] class existential types.
class LoadableUnownedClassExistentialTypeInfo final
  : public ScalarExistentialTypeInfoBase<
                                      LoadableUnownedClassExistentialTypeInfo,
                                         LoadableTypeInfo> {
  ReferenceCounting Refcounting;
  llvm::Type *ValueType;

public:
  LoadableUnownedClassExistentialTypeInfo(
                                  ArrayRef<ProtocolEntry> storedProtocols,
                                  llvm::Type *valueTy,
                                  llvm::Type *ty,
                                  const SpareBitVector &spareBits,
                                  Size size, Alignment align,
                                  ReferenceCounting refcounting)
    : ScalarExistentialTypeInfoBase(storedProtocols, ty, size,
                                    spareBits, align, IsNotPOD, IsFixedSize),
      Refcounting(refcounting), ValueType(valueTy) {
    assert(refcounting == ReferenceCounting::Native ||
           refcounting == ReferenceCounting::Unknown);
  }

  llvm::Type *getValueType() const {
    return ValueType;
  }

  Address projectValue(IRGenFunction &IGF, Address addr) const {
    Address valueAddr = ScalarExistentialTypeInfoBase::projectValue(IGF, addr);
    return IGF.Builder.CreateBitCast(valueAddr, ValueType->getPointerTo());
  }

  void emitValueRetain(IRGenFunction &IGF, llvm::Value *value,
                       Atomicity atomicity) const {
    IGF.emitUnownedRetain(value, Refcounting);
  }

  void emitValueRelease(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {
    IGF.emitUnownedRelease(value, Refcounting);
  }

  void emitValueFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitFixLifetime(value);
  }

  const LoadableTypeInfo &
  getValueTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
    llvm_unreachable("should have overridden all actual uses of this");
  }

  bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
    return true;
  }

  unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
    return IGM.getUnownedExtraInhabitantCount(Refcounting);
  }

  APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                     unsigned bits,
                                     unsigned index) const override {
    return IGM.getUnownedExtraInhabitantValue(bits, index, Refcounting);
  }

  llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                       SILType T) const override {
    Address valueSrc = projectValue(IGF, src);
    return IGF.getUnownedExtraInhabitantIndex(valueSrc, Refcounting);
  }

  void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                            Address dest, SILType T) const override {
    Address valueDest = projectValue(IGF, dest);
    return IGF.storeUnownedExtraInhabitant(index, valueDest, Refcounting);
  }

  APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
    APInt bits = IGM.getUnownedExtraInhabitantMask(Refcounting);

    // Zext out to the size of the existential.
    bits = bits.zextOrTrunc(asDerived().getFixedSize().getValueInBits());
    return bits;
  }
};

/// A type implementation for @unowned(unsafe) class existential types.
class UnmanagedClassExistentialTypeInfo final
  : public ScalarExistentialTypeInfoBase<UnmanagedClassExistentialTypeInfo,
                                         LoadableTypeInfo> {
public:
  UnmanagedClassExistentialTypeInfo(ArrayRef<ProtocolEntry> storedProtocols,
                                    llvm::Type *ty,
                                    const SpareBitVector &spareBits,
                                    Size size, Alignment align)
    : ScalarExistentialTypeInfoBase(storedProtocols, ty, size,
                                    spareBits, align, IsPOD, IsFixedSize) {}

  const LoadableTypeInfo &
  getValueTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
    if (!IGM.ObjCInterop)
      return IGM.getNativeObjectTypeInfo();
    else
      return IGM.getUnknownObjectTypeInfo();
  }

  void emitValueRetain(IRGenFunction &IGF, llvm::Value *value,
                       Atomicity atomicity) const {
    // do nothing
  }

  void emitValueRelease(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {
    // do nothing
  }

  void emitValueFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    // do nothing
  }
};

/// A type info implementation for class existential types, that is,
/// an existential type known to conform to one or more class protocols.
/// Class existentials can be represented directly as an aggregation
/// of a refcounted pointer plus witness tables instead of using an indirect
/// buffer.
class ClassExistentialTypeInfo final
  : public ScalarExistentialTypeInfoBase<ClassExistentialTypeInfo,
                                         ReferenceTypeInfo>
{
  ReferenceCounting Refcounting;
 
  friend ExistentialTypeInfoBase;
  ClassExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                           llvm::Type *ty,
                           Size size,
                           SpareBitVector &&spareBits,
                           Alignment align,
                           ReferenceCounting refcounting)
    : ScalarExistentialTypeInfoBase(protocols, ty, size,
                                    std::move(spareBits), align),
      Refcounting(refcounting) {
    assert(refcounting == ReferenceCounting::Native ||
           refcounting == ReferenceCounting::Unknown);
  }

public:

  bool isSingleRetainablePointer(ResilienceExpansion expansion,
                                 ReferenceCounting *refcounting) const override{
    if (refcounting) *refcounting = Refcounting;
    return getNumStoredProtocols() == 0;
  }

  const LoadableTypeInfo &
  getValueTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
    if (Refcounting == ReferenceCounting::Native)
      return IGM.getNativeObjectTypeInfo();
    else
      return IGM.getUnknownObjectTypeInfo();
  }

  void strongRetain(IRGenFunction &IGF, Explosion &e,
                    Atomicity atomicity) const override {
    IGF.emitStrongRetain(e.claimNext(), Refcounting, atomicity);
    (void)e.claim(getNumStoredProtocols());
  }

  void strongRelease(IRGenFunction &IGF, Explosion &e,
                     Atomicity atomicity) const override {
    IGF.emitStrongRelease(e.claimNext(), Refcounting, atomicity);
    (void)e.claim(getNumStoredProtocols());
  }

  void strongRetainUnowned(IRGenFunction &IGF, Explosion &e) const override {
    IGF.emitStrongRetainUnowned(e.claimNext(), Refcounting);
    (void)e.claim(getNumStoredProtocols());
  }

  void strongRetainUnownedRelease(IRGenFunction &IGF,
                                  Explosion &e) const override {
    IGF.emitStrongRetainAndUnownedRelease(e.claimNext(), Refcounting);
    (void)e.claim(getNumStoredProtocols());
  }

  void unownedRetain(IRGenFunction &IGF, Explosion &e) const override {
    IGF.emitUnownedRetain(e.claimNext(), Refcounting);
    (void)e.claim(getNumStoredProtocols());
  }

  void unownedRelease(IRGenFunction &IGF, Explosion &e) const override {
    IGF.emitUnownedRelease(e.claimNext(), Refcounting);
    (void)e.claim(getNumStoredProtocols());
  }

  void unownedLoadStrong(IRGenFunction &IGF, Address existential,
                         Explosion &out) const override {
    Address valueAddr = projectValue(IGF, existential);
    out.add(IGF.emitUnownedLoadStrong(valueAddr,
                                      IGF.IGM.getReferenceType(Refcounting),
                                      Refcounting));
    emitLoadOfTables(IGF, existential, out);
  }

  void unownedTakeStrong(IRGenFunction &IGF, Address existential,
                         Explosion &out) const override {
    Address valueAddr = projectValue(IGF, existential);
    out.add(IGF.emitUnownedTakeStrong(valueAddr,
                                      IGF.IGM.getReferenceType(Refcounting),
                                      Refcounting));
    emitLoadOfTables(IGF, existential, out);
  }

  void unownedInit(IRGenFunction &IGF, Explosion &in,
                   Address existential) const override {
    llvm::Value *value = in.claimNext();
    emitStoreOfTables(IGF, in, existential);
    Address valueAddr = projectValue(IGF, existential);
    IGF.emitUnownedInit(value, valueAddr, Refcounting);
  }

  void unownedAssign(IRGenFunction &IGF, Explosion &in,
                     Address existential) const override {
    llvm::Value *value = in.claimNext();
    emitStoreOfTables(IGF, in, existential);
    Address valueAddr = projectValue(IGF, existential);
    IGF.emitUnownedAssign(value, valueAddr, Refcounting);
  }

  void emitValueRetain(IRGenFunction &IGF, llvm::Value *value,
                       Atomicity atomicity) const {
    IGF.emitStrongRetain(value, Refcounting, atomicity);
  }

  void emitValueRelease(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {
    IGF.emitStrongRelease(value, Refcounting, atomicity);
  }

  void emitValueFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    IGF.emitFixLifetime(value);
  }

  LoadedRef loadRefcountedPtr(IRGenFunction &IGF, SourceLoc loc,
                              Address existential) const override {
    Address valueAddr = projectValue(IGF, existential);
    return LoadedRef(IGF.emitLoadRefcountedPtr(valueAddr, Refcounting), true);
  }

  const TypeInfo *
  createUnownedStorageType(TypeConverter &TC) const override {
    // We can just re-use the storage type for the @unowned(safe) type.

    SpareBitVector spareBits =
      TC.IGM.getUnownedReferenceSpareBits(Refcounting);
    for (unsigned i = 0, e = getNumStoredProtocols(); i != e; ++i)
      spareBits.append(TC.IGM.getWitnessTablePtrSpareBits());

    auto storageTy = buildReferenceStorageType(TC.IGM,
                              TC.IGM.UnownedReferencePtrTy->getElementType());

    if (TC.IGM.isUnownedReferenceAddressOnly(Refcounting)) {
      return AddressOnlyUnownedClassExistentialTypeInfo::create(
                                                   getStoredProtocols(),
                                                   storageTy,
                                                   std::move(spareBits),
                                                   getFixedSize(),
                                                   getFixedAlignment(),
                                                   Refcounting);
    } else {
      return LoadableUnownedClassExistentialTypeInfo::create(
                                                   getStoredProtocols(),
                                                   getValueType(),
                                                   storageTy,
                                                   std::move(spareBits),
                                                   getFixedSize(),
                                                   getFixedAlignment(),
                                                   Refcounting);
    }
  }

  const TypeInfo *
  createUnmanagedStorageType(TypeConverter &TC) const override {
    // We can just re-use the storage type for the @unowned(unsafe) type.
    return UnmanagedClassExistentialTypeInfo::create(getStoredProtocols(),
                                                     getStorageType(),
                                                     getSpareBits(),
                                                     getFixedSize(),
                                                     getFixedAlignment());
  }

  const WeakTypeInfo *
  createWeakStorageType(TypeConverter &TC) const override {
    Size size = TC.IGM.getWeakReferenceSize()
              + getNumStoredProtocols() * TC.IGM.getPointerSize();

    Alignment align = TC.IGM.getWeakReferenceAlignment();
    assert(align == TC.IGM.getPointerAlignment() &&
           "[weak] alignment not pointer alignment; fix existential layout");
    (void)align;

    auto storageTy = buildReferenceStorageType(TC.IGM,
                                  TC.IGM.WeakReferencePtrTy->getElementType());

    SpareBitVector spareBits = TC.IGM.getWeakReferenceSpareBits();
    for (unsigned i = 0, e = getNumStoredProtocols(); i != e; ++i)
      spareBits.append(TC.IGM.getWitnessTablePtrSpareBits());

    return WeakClassExistentialTypeInfo::create(getStoredProtocols(),
                                                storageTy, size, align,
                                                std::move(spareBits),
                                                Refcounting);
  }

  llvm::StructType *buildReferenceStorageType(IRGenModule &IGM,
                                              llvm::Type *refStorageTy) const {
    SmallVector<llvm::Type*, 8> fieldTys;
    fieldTys.push_back(refStorageTy);
    fieldTys.resize(getNumStoredProtocols() + 1, IGM.WitnessTablePtrTy);
    auto storageTy = llvm::StructType::get(IGM.getLLVMContext(), fieldTys);
    return storageTy;
  }
};

/// A type implementation for existential metatypes.
class ExistentialMetatypeTypeInfo final
  : public ScalarExistentialTypeInfoBase<ExistentialMetatypeTypeInfo,
                                         LoadableTypeInfo> {
  const LoadableTypeInfo &MetatypeTI;

  friend ExistentialTypeInfoBase;
  ExistentialMetatypeTypeInfo(ArrayRef<ProtocolEntry> storedProtocols,
                              llvm::Type *ty, Size size,
                              SpareBitVector &&spareBits,
                              Alignment align,
                              const LoadableTypeInfo &metatypeTI)
    : ScalarExistentialTypeInfoBase(storedProtocols, ty, size,
                                    std::move(spareBits), align, IsPOD,
                                    IsFixedSize),
      MetatypeTI(metatypeTI) {}

public:
  const LoadableTypeInfo &
  getValueTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
    return MetatypeTI;
  }

  void emitValueRetain(IRGenFunction &IGF, llvm::Value *value,
                       Atomicity atomicity) const {
    // do nothing
  }

  void emitValueRelease(IRGenFunction &IGF, llvm::Value *value,
                        Atomicity atomicity) const {
    // do nothing
  }

  void emitValueFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
    // do nothing
  }
};

/// Type info for error existentials, currently the only kind of boxed
/// existential.
class ErrorExistentialTypeInfo : public HeapTypeInfo<ErrorExistentialTypeInfo>
{
  ProtocolEntry ErrorEntry;
  ReferenceCounting Refcounting;

public:
  ErrorExistentialTypeInfo(llvm::PointerType *storage,
                           Size size, SpareBitVector spareBits,
                           Alignment align,
                           const ProtocolEntry &errorProtocolEntry,
                           ReferenceCounting refcounting)
    : HeapTypeInfo(storage, size, spareBits, align),
      ErrorEntry(errorProtocolEntry),
      Refcounting(refcounting) {}

  ReferenceCounting getReferenceCounting() const {
    // Error uses its own RC entry points.
    return Refcounting;
  }
  
  ArrayRef<ProtocolEntry> getStoredProtocols() const {
    return ErrorEntry;
  }
};
  
} // end anonymous namespace

static const TypeInfo *createErrorExistentialTypeInfo(IRGenModule &IGM,
                                            ArrayRef<ProtocolDecl*> protocols) {
  // The Error existential has a special boxed representation. It has
  // space only for witnesses to the Error protocol.
  assert(protocols.size() == 1
     && *protocols[0]->getKnownProtocolKind() == KnownProtocolKind::Error);
  
  const ProtocolInfo &impl = IGM.getProtocolInfo(protocols[0]);
  auto refcounting = (!IGM.ObjCInterop
                      ? ReferenceCounting::Native
                      : ReferenceCounting::Error);

  return new ErrorExistentialTypeInfo(IGM.ErrorPtrTy,
                                      IGM.getPointerSize(),
                                      IGM.getHeapObjectSpareBits(),
                                      IGM.getPointerAlignment(),
                                      ProtocolEntry(protocols[0], impl),
                                      refcounting);
}

static const TypeInfo *createExistentialTypeInfo(IRGenModule &IGM, CanType T,
                                            ArrayRef<ProtocolDecl*> protocols) {
  SmallVector<llvm::Type*, 5> fields;
  SmallVector<ProtocolEntry, 4> entries;

  // Check for special existentials.
  if (protocols.size() == 1) {
    switch (getSpecialProtocolID(protocols[0])) {
    case SpecialProtocol::Error:
      // Error has a special runtime representation.
      return createErrorExistentialTypeInfo(IGM, protocols);
    // Other existentials have standard representations.
    case SpecialProtocol::AnyObject:
    case SpecialProtocol::None:
      break;
    }
  }

  llvm::StructType *type;
  if (isa<ProtocolType>(T))
    type = IGM.createNominalType(T);
  else if (auto compT = dyn_cast<ProtocolCompositionType>(T))
    // Protocol composition types are not nominal, but we name them anyway.
    type = IGM.createNominalType(compT.getPointer());
  else
    llvm_unreachable("unknown existential type kind");
    
  assert(type->isOpaque() && "creating existential type in concrete struct");

  // In an opaque metadata, the first two fields are the fixed buffer
  // followed by the metadata reference.  In a class metadata, the
  // first field is the class instance.
  //
  // Leave space in the buffer for both, but make sure we set it up later.
  fields.push_back(nullptr);
  fields.push_back(nullptr);

  bool requiresClass = false;
  bool allowsTaggedPointers = true;

  for (auto protocol : protocols) {
    // The existential container is class-constrained if any of its protocol
    // constraints are.
    requiresClass |= protocol->requiresClass();

    if (protocol->getAttrs().hasAttribute<UnsafeNoObjCTaggedPointerAttr>())
      allowsTaggedPointers = false;

    // ObjC protocols need no layout or witness table info. All dispatch is done
    // through objc_msgSend.
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
      continue;

    // Find the protocol layout.
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    entries.push_back(ProtocolEntry(protocol, impl));

    // Each protocol gets a witness table.
    fields.push_back(IGM.WitnessTablePtrTy);
  }

  // If the existential is class, lower it to a class
  // existential representation.
  if (requiresClass) {
    // If we're not using the Objective-C runtime, we can use the
    // native reference counting entry points.
    ReferenceCounting refcounting;

    // Replace the type metadata pointer with the class instance.
    if (!IGM.ObjCInterop) {
      refcounting = ReferenceCounting::Native;
      fields[1] = IGM.RefCountedPtrTy;
    } else {
      refcounting = ReferenceCounting::Unknown;
      fields[1] = IGM.UnknownRefCountedPtrTy;
    }

    auto classFields = llvm::makeArrayRef(fields).slice(1);
    type->setBody(classFields);

    Alignment align = IGM.getPointerAlignment();
    Size size = classFields.size() * IGM.getPointerSize();

    SpareBitVector spareBits;

    // The class pointer is an unknown heap object, so it may be a tagged
    // pointer, if the platform has those.
    if (allowsTaggedPointers && IGM.TargetInfo.hasObjCTaggedPointers()) {
      spareBits.appendClearBits(IGM.getPointerSize().getValueInBits());
    } else {
      // If the platform doesn't use ObjC tagged pointers, we can go crazy.
      spareBits.append(IGM.getHeapObjectSpareBits());
    }

    for (unsigned i = 1, e = classFields.size(); i < e; ++i) {
      spareBits.append(IGM.getWitnessTablePtrSpareBits());
    }

    return ClassExistentialTypeInfo::create(entries, type,
                                            size, std::move(spareBits), align,
                                            refcounting);
  }

  // Set up the first two fields.
  fields[0] = IGM.getFixedBufferTy();
  fields[1] = IGM.TypeMetadataPtrTy;
  type->setBody(fields);

  OpaqueExistentialLayout layout(entries.size());
  Alignment align = layout.getAlignment(IGM);
  Size size = layout.getSize(IGM);
  return OpaqueExistentialTypeInfo::create(entries, type, size, align);
}

const TypeInfo *TypeConverter::convertProtocolType(ProtocolType *T) {
  // Protocol types are nominal.
  return createExistentialTypeInfo(IGM, CanType(T), T->getDecl());
}

const TypeInfo *
TypeConverter::convertProtocolCompositionType(ProtocolCompositionType *T) {
  // Find the canonical protocols.  There might not be any.
  SmallVector<ProtocolDecl*, 4> protocols;
  T->getAnyExistentialTypeProtocols(protocols);

  return createExistentialTypeInfo(IGM, CanType(T), protocols);
}

const TypeInfo *
TypeConverter::convertExistentialMetatypeType(ExistentialMetatypeType *T) {
  assert(T->hasRepresentation() &&
         "metatype should have been assigned a representation by SIL");

  SmallVector<ProtocolDecl*, 4> protocols;
  T->getAnyExistentialTypeProtocols(protocols);

  SmallVector<ProtocolEntry, 4> entries;
  SmallVector<llvm::Type*, 4> fields;

  SpareBitVector spareBits;

  assert(T->getRepresentation() != MetatypeRepresentation::Thin &&
         "existential metatypes cannot have thin representation");
  auto &baseTI = getMetatypeTypeInfo(T->getRepresentation());
  fields.push_back(baseTI.getStorageType());
  spareBits.append(baseTI.getSpareBits());

  for (auto protocol : protocols) {
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
      continue;

    // Find the protocol layout.
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    entries.push_back(ProtocolEntry(protocol, impl));

    // Each protocol gets a witness table.
    fields.push_back(IGM.WitnessTablePtrTy);
    spareBits.append(IGM.getWitnessTablePtrSpareBits());
  }

  llvm::StructType *type = llvm::StructType::get(IGM.getLLVMContext(), fields);

  Size size = IGM.getPointerSize() * fields.size();
  Alignment align = IGM.getPointerAlignment();

  return ExistentialMetatypeTypeInfo::create(entries, type, size,
                                             std::move(spareBits),
                                             align, baseTI);
}

/// Return a function which performs an assignment operation on two
/// existentials.
///
/// Existential types are nominal, so we potentially need to cast the
/// function to the appropriate object-pointer type.
static llvm::Constant *getAssignExistentialsFunction(IRGenModule &IGM,
                                                     llvm::Type *objectPtrTy,
                                                     OpaqueExistentialLayout layout) {
  llvm::Type *argTys[] = { objectPtrTy, objectPtrTy };

  // __swift_assign_existentials_N is the well-known function for
  // assigning existential types with N witness tables.
  llvm::SmallString<40> fnName;
  llvm::raw_svector_ostream(fnName)
    << "__swift_assign_existentials_" << layout.getNumTables();

  return IGM.getOrCreateHelperFunction(fnName, IGM.VoidTy, argTys,
                                       [&](IRGenFunction &IGF) {
    auto it = IGF.CurFn->arg_begin();
    Address dest(&*(it++), getFixedBufferAlignment(IGM));
    Address src(&*(it++), getFixedBufferAlignment(IGM));

    // If doing a self-assignment, we're done.
    llvm::BasicBlock *doneBB = IGF.createBasicBlock("done");
    llvm::BasicBlock *contBB = IGF.createBasicBlock("cont");
    llvm::Value *isSelfAssign =
      IGF.Builder.CreateICmpEQ(dest.getAddress(), src.getAddress(),
                               "isSelfAssign");
    IGF.Builder.CreateCondBr(isSelfAssign, doneBB, contBB);

    // Project down to the buffers.
    IGF.Builder.emitBlock(contBB);
    // We don't need a ConditionalDominanceScope here because (1) there's no
    // code in the other condition and (2) we immediately return.
    Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
    Address srcBuffer = layout.projectExistentialBuffer(IGF, src);

    // Load the metadata tables.
    Address destMetadataSlot = layout.projectMetadataRef(IGF, dest);
    llvm::Value *destMetadata = IGF.Builder.CreateLoad(destMetadataSlot);
    llvm::Value *srcMetadata = layout.loadMetadataRef(IGF, src);

    // Check whether the metadata match.
    llvm::BasicBlock *matchBB = IGF.createBasicBlock("match");
    llvm::BasicBlock *noMatchBB = IGF.createBasicBlock("no-match");
    llvm::Value *sameMetadata =
      IGF.Builder.CreateICmpEQ(destMetadata, srcMetadata, "sameMetadata");
    IGF.Builder.CreateCondBr(sameMetadata, matchBB, noMatchBB);

    // If so, do a direct assignment.
    IGF.Builder.emitBlock(matchBB);
    {
      ConditionalDominanceScope matchCondition(IGF);

      llvm::Value *destObject =
        emitProjectBufferCall(IGF, destMetadata, destBuffer);
      llvm::Value *srcObject =
        emitProjectBufferCall(IGF, destMetadata, srcBuffer);
      emitAssignWithCopyCall(IGF, destMetadata,
                             Address(destObject, Alignment(1)),
                             Address(srcObject, Alignment(1)));
      IGF.Builder.CreateBr(doneBB);
    }

    // Otherwise, destroy and copy-initialize.
    // TODO: should we copy-initialize and then destroy?  That's
    // possible if we copy aside, which is a small expense but
    // always safe.  Otherwise the destroy (which can invoke user code)
    // could see invalid memory at this address.  These are basically
    // the madnesses that boost::variant has to go through, with the
    // advantage of address-invariance.
    IGF.Builder.emitBlock(noMatchBB);
    {
      ConditionalDominanceScope noMatchCondition(IGF);

      // Store the metadata ref.
      IGF.Builder.CreateStore(srcMetadata, destMetadataSlot);

      // Store the protocol witness tables.
      unsigned numTables = layout.getNumTables();
      for (unsigned i = 0, e = numTables; i != e; ++i) {
        Address destTableSlot = layout.projectWitnessTable(IGF, dest, i);
        llvm::Value *srcTable = layout.loadWitnessTable(IGF, src, i);

        // Overwrite the old witness table.
        IGF.Builder.CreateStore(srcTable, destTableSlot);
      }

      // Destroy the old value.
      emitDestroyBufferCall(IGF, destMetadata, destBuffer);

      // Copy-initialize with the new value.  Again, pull a value
      // witness table from the source metadata if we can't use a
      // protocol witness table.
      emitInitializeBufferWithCopyOfBufferCall(IGF, srcMetadata,
                                               destBuffer,
                                               srcBuffer);
      IGF.Builder.CreateBr(doneBB);
    }

    // All done.
    IGF.Builder.emitBlock(doneBB);
    IGF.Builder.CreateRetVoid();
  });
}

/// Emit protocol witness table pointers for the given protocol conformances,
/// passing each emitted witness table index into the given function body.
static void forEachProtocolWitnessTable(IRGenFunction &IGF,
                          CanType srcType, llvm::Value **srcMetadataCache,
                          CanType destType,
                          ArrayRef<ProtocolEntry> protocols,
                          ArrayRef<ProtocolConformanceRef> conformances,
                          std::function<void (unsigned, llvm::Value*)> body) {
  // Collect the conformances that need witness tables.
  SmallVector<ProtocolDecl*, 2> destProtocols;
  destType.getAnyExistentialTypeProtocols(destProtocols);

  SmallVector<ProtocolConformanceRef, 2> witnessConformances;
  assert(destProtocols.size() == conformances.size() &&
         "mismatched protocol conformances");
  for (unsigned i = 0, size = destProtocols.size(); i < size; ++i)
    if (Lowering::TypeConverter::protocolRequiresWitnessTable(destProtocols[i]))
      witnessConformances.push_back(conformances[i]);

  assert(protocols.size() == witnessConformances.size() &&
         "mismatched protocol conformances");

  for (unsigned i = 0, e = protocols.size(); i < e; ++i) {
    assert(protocols[i].getProtocol()
             == witnessConformances[i].getRequirement());
    auto table = emitWitnessTableRef(IGF, srcType, srcMetadataCache,
                                     witnessConformances[i]);
    body(i, table);
  }
}

#ifndef NDEBUG
static bool _isError(SILType baseTy) {
  llvm::SmallVector<ProtocolDecl*, 1> protos;
  return baseTy.getSwiftRValueType()->isExistentialType(protos)
    && protos.size() == 1
    && protos[0]->getKnownProtocolKind()
    && *protos[0]->getKnownProtocolKind() == KnownProtocolKind::Error;
}
#endif

/// Project the address of the value inside a boxed existential container.
ContainedAddress irgen::emitBoxedExistentialProjection(IRGenFunction &IGF,
                                              Explosion &base,
                                              SILType baseTy,
                                              CanType projectedType) {
  // TODO: Non-ErrorType boxed existentials.
  assert(_isError(baseTy));
  
  // Get the reference to the existential box.
  llvm::Value *box = base.claimNext();
  // Allocate scratch space to invoke the runtime.
  Address scratch = IGF.createAlloca(IGF.IGM.Int8PtrTy,
                                     IGF.IGM.getPointerAlignment(),
                                     "project_error_scratch");
  Address out = IGF.createAlloca(IGF.IGM.OpenedErrorTripleTy,
                                 IGF.IGM.getPointerAlignment(),
                                 "project_error_out");
  
  IGF.Builder.CreateCall(IGF.IGM.getGetErrorValueFn(), {box,
                         scratch.getAddress(),
                         out.getAddress()});
  // Load the 'out' values.
  auto &projectedTI = IGF.getTypeInfoForLowered(projectedType);
  auto projectedPtrAddr = IGF.Builder.CreateStructGEP(out, 0, Size(0));
  llvm::Value *projectedPtr = IGF.Builder.CreateLoad(projectedPtrAddr);
  projectedPtr = IGF.Builder.CreateBitCast(projectedPtr,
                               projectedTI.getStorageType()->getPointerTo());
  auto projected = projectedTI.getAddressForPointer(projectedPtr);
  return ContainedAddress(out, projected);
}

/// Project the address of the value inside a boxed existential container,
/// and open an archetype to its contained type.
Address irgen::emitOpenExistentialBox(IRGenFunction &IGF,
                                      Explosion &base,
                                      SILType baseTy,
                                      CanArchetypeType openedArchetype) {
  ContainedAddress box = emitBoxedExistentialProjection(IGF, base, baseTy,
                                                        openedArchetype);
  Address out = box.getContainer();
  auto metadataAddr = IGF.Builder.CreateStructGEP(out, 1,
                                                  IGF.IGM.getPointerSize());
  auto metadata = IGF.Builder.CreateLoad(metadataAddr);
  auto witnessAddr = IGF.Builder.CreateStructGEP(out, 2,
                                                 2 * IGF.IGM.getPointerSize());
  auto witness = IGF.Builder.CreateLoad(witnessAddr);
  
  IGF.bindArchetype(openedArchetype, metadata, witness);
  return box.getAddress();
}

/// Allocate a boxed existential container with uninitialized space to hold a
/// value of a given type.
OwnedAddress irgen::emitBoxedExistentialContainerAllocation(IRGenFunction &IGF,
                                  SILType destType,
                                  CanType formalSrcType,
                                ArrayRef<ProtocolConformanceRef> conformances) {
  // TODO: Non-Error boxed existentials.
  assert(_isError(destType));

  auto &destTI = IGF.getTypeInfo(destType).as<ErrorExistentialTypeInfo>();
  auto srcMetadata = IGF.emitTypeMetadataRef(formalSrcType);
  // Should only be one conformance, for the Error protocol.
  assert(conformances.size() == 1 && destTI.getStoredProtocols().size() == 1);
  const ProtocolEntry &entry = destTI.getStoredProtocols()[0];
  (void) entry;
  assert(entry.getProtocol() == conformances[0].getRequirement());
  auto witness = emitWitnessTableRef(IGF, formalSrcType, &srcMetadata,
                                     conformances[0]);
  
  // Call the runtime to allocate the box.
  // TODO: When there's a store or copy_addr immediately into the box, peephole
  // it into the initializer parameter to allocError.
  auto result = IGF.Builder.CreateCall(IGF.IGM.getAllocErrorFn(),
                         {srcMetadata, witness,
                           llvm::ConstantPointerNull::get(IGF.IGM.OpaquePtrTy),
                           llvm::ConstantInt::get(IGF.IGM.Int1Ty, 0)});
  
  // Extract the box and value address from the result.
  auto box = IGF.Builder.CreateExtractValue(result, 0);
  auto addr = IGF.Builder.CreateExtractValue(result, 1);

  auto archetype = ArchetypeType::getOpened(destType.getSwiftRValueType());
  auto &srcTI = IGF.getTypeInfoForUnlowered(AbstractionPattern(archetype),
                                            formalSrcType);
  addr = IGF.Builder.CreateBitCast(addr,
                                   srcTI.getStorageType()->getPointerTo());
  return OwnedAddress(srcTI.getAddressForPointer(addr), box);
}

/// Deallocate a boxed existential container with uninitialized space to hold a
/// value of a given type.
void irgen::emitBoxedExistentialContainerDeallocation(IRGenFunction &IGF,
                                                      Explosion &container,
                                                      SILType containerType,
                                                      CanType valueType) {
  // TODO: Non-Error boxed existentials.
  assert(_isError(containerType));

  auto box = container.claimNext();
  auto srcMetadata = IGF.emitTypeMetadataRef(valueType);
  
  IGF.Builder.CreateCall(IGF.IGM.getDeallocErrorFn(), {box, srcMetadata});
}

/// "Deinitialize" an existential container whose contained value is allocated
/// but uninitialized, by deallocating the buffer owned by the container if any.
void irgen::emitOpaqueExistentialContainerDeinit(IRGenFunction &IGF,
                                                 Address container,
                                                 SILType type) {
  assert(type.isExistentialType());
  assert(!type.isClassExistentialType());
  auto &ti = IGF.getTypeInfo(type).as<OpaqueExistentialTypeInfo>();
  auto layout = ti.getLayout();

  llvm::Value *metadata = layout.loadMetadataRef(IGF, container);
  Address buffer = layout.projectExistentialBuffer(IGF, container);
  emitDeallocateBufferCall(IGF, metadata, buffer);
}

/// Emit a class existential container from a class instance value
/// as an explosion.
void irgen::emitClassExistentialContainer(IRGenFunction &IGF,
                               Explosion &out,
                               SILType outType,
                               llvm::Value *instance,
                               CanType instanceFormalType,
                               SILType instanceLoweredType,
                               ArrayRef<ProtocolConformanceRef> conformances) {
  // As a special case, an Error existential can be represented as a
  // reference to an already existing NSError or CFError instance.
  SmallVector<ProtocolDecl*, 4> protocols;
  
  if (outType.getSwiftRValueType()->isExistentialType(protocols)
      && protocols.size() == 1) {
    switch (getSpecialProtocolID(protocols[0])) {
    case SpecialProtocol::Error: {
      // Bitcast the incoming class reference to Error.
      out.add(IGF.Builder.CreateBitCast(instance, IGF.IGM.ErrorPtrTy));
      return;
    }

    case SpecialProtocol::AnyObject:
    case SpecialProtocol::None:
      break;
    }
  }
  
  assert(outType.isClassExistentialType() &&
         "creating a non-class existential type");

  auto &destTI = IGF.getTypeInfo(outType).as<ClassExistentialTypeInfo>();

  // Cast the instance pointer to an opaque refcounted pointer.
  llvm::Value *opaqueInstance;
  if (!IGF.IGM.ObjCInterop)
    opaqueInstance = IGF.Builder.CreateBitCast(instance,
                                               IGF.IGM.RefCountedPtrTy);
  else
    opaqueInstance = IGF.Builder.CreateBitCast(instance,
                                               IGF.IGM.UnknownRefCountedPtrTy);
  out.add(opaqueInstance);

  // Emit the witness table pointers.
  llvm::Value *instanceMetadata = nullptr;
  forEachProtocolWitnessTable(IGF, instanceFormalType, &instanceMetadata,
                              outType.getSwiftRValueType(),
                              destTI.getStoredProtocols(),
                              conformances,
                              [&](unsigned i, llvm::Value *ptable) {
    out.add(ptable);
  });
}

/// Emit an existential container initialization operation for a concrete type.
/// Returns the address of the uninitialized fixed-size buffer for the concrete
/// value.
Address irgen::emitOpaqueExistentialContainerInit(IRGenFunction &IGF,
                                  Address dest,
                                  SILType destType,
                                  CanType formalSrcType,
                                  SILType loweredSrcType,
                                  ArrayRef<ProtocolConformanceRef> conformances) {
  assert(!destType.isClassExistentialType() &&
         "initializing a class existential container as opaque");
  auto &destTI = IGF.getTypeInfo(destType).as<OpaqueExistentialTypeInfo>();
  OpaqueExistentialLayout destLayout = destTI.getLayout();
  assert(destTI.getStoredProtocols().size() == conformances.size());

  // First, write out the metadata.
  llvm::Value *metadata = IGF.emitTypeMetadataRef(formalSrcType);
  IGF.Builder.CreateStore(metadata, destLayout.projectMetadataRef(IGF, dest));


  // Next, write the protocol witness tables.
  forEachProtocolWitnessTable(IGF, formalSrcType, &metadata,
                              destType.getSwiftRValueType(),
                              destTI.getStoredProtocols(), conformances,
                              [&](unsigned i, llvm::Value *ptable) {
    Address ptableSlot = destLayout.projectWitnessTable(IGF, dest, i);
    IGF.Builder.CreateStore(ptable, ptableSlot);
  });

  // Finally, evaluate into the buffer.

  // Project down to the destination fixed-size buffer.
  return destLayout.projectExistentialBuffer(IGF, dest);
}

/// Emit an existential metatype container from a metatype value
/// as an explosion.
void irgen::emitExistentialMetatypeContainer(IRGenFunction &IGF,
                               Explosion &out, SILType outType,
                               llvm::Value *metatype, SILType metatypeType,
                               ArrayRef<ProtocolConformanceRef> conformances) {
  assert(outType.is<ExistentialMetatypeType>());
  auto &destTI = IGF.getTypeInfo(outType).as<ExistentialMetatypeTypeInfo>();
  out.add(metatype);

  auto srcType = metatypeType.castTo<MetatypeType>().getInstanceType();
  auto destType = outType.castTo<ExistentialMetatypeType>().getInstanceType();
  while (auto destMetatypeType = dyn_cast<ExistentialMetatypeType>(destType)) {
    destType = destMetatypeType.getInstanceType();
    srcType = cast<AnyMetatypeType>(srcType).getInstanceType();
  }

  // Emit the witness table pointers.
  llvm::Value *srcMetadata = nullptr;
  forEachProtocolWitnessTable(IGF, srcType, &srcMetadata, destType,
                              destTI.getStoredProtocols(),
                              conformances,
                              [&](unsigned i, llvm::Value *ptable) {
                                out.add(ptable);
                              });
}

void irgen::emitMetatypeOfOpaqueExistential(IRGenFunction &IGF, Address addr,
                                            SILType type, Explosion &out) {
  assert(type.isExistentialType());
  assert(!type.isClassExistentialType());
  auto &baseTI = IGF.getTypeInfo(type).as<OpaqueExistentialTypeInfo>();

  // Get the static metadata.
  auto existLayout = baseTI.getLayout();
  llvm::Value *metadata = existLayout.loadMetadataRef(IGF, addr);

  // Project the buffer and apply the 'typeof' value witness.
  Address buffer = existLayout.projectExistentialBuffer(IGF, addr);
  llvm::Value *object =
    emitProjectBufferCall(IGF, metadata, buffer);
  llvm::Value *dynamicType =
    IGF.Builder.CreateCall(IGF.IGM.getGetDynamicTypeFn(),
                           {object, metadata,
                            llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1)});
  out.add(dynamicType);

  // Get the witness tables.
  baseTI.emitLoadOfTables(IGF, addr, out);
}

void irgen::emitMetatypeOfBoxedExistential(IRGenFunction &IGF, Explosion &value,
                                           SILType type, Explosion &out) {
  // TODO: Non-Error boxed existentials.
  assert(_isError(type));

  // Get the reference to the existential box.
  llvm::Value *box = value.claimNext();

  // Allocate scratch space to invoke the runtime.
  Address scratchAddr = IGF.createAlloca(IGF.IGM.Int8PtrTy,
                                         IGF.IGM.getPointerAlignment(),
                                         "project_error_scratch");
  Address outAddr = IGF.createAlloca(IGF.IGM.OpenedErrorTripleTy,
                                     IGF.IGM.getPointerAlignment(),
                                     "project_error_out");

  IGF.Builder.CreateCall(IGF.IGM.getGetErrorValueFn(), {box,
                         scratchAddr.getAddress(),
                         outAddr.getAddress()});

  auto projectedPtrAddr = IGF.Builder.CreateStructGEP(outAddr, 0, Size(0));
  auto projectedPtr = IGF.Builder.CreateLoad(projectedPtrAddr);

  auto metadataAddr = IGF.Builder.CreateStructGEP(outAddr, 1,
                                                  IGF.IGM.getPointerSize());
  auto metadata = IGF.Builder.CreateLoad(metadataAddr);

  auto dynamicType =
    IGF.Builder.CreateCall(IGF.IGM.getGetDynamicTypeFn(),
                           {projectedPtr, metadata,
                            llvm::ConstantInt::get(IGF.IGM.Int1Ty, 1)});

  auto witnessAddr = IGF.Builder.CreateStructGEP(outAddr, 2,
                                                 2 * IGF.IGM.getPointerSize());
  auto witness = IGF.Builder.CreateLoad(witnessAddr);

  out.add(dynamicType);
  out.add(witness);
}

void irgen::emitMetatypeOfClassExistential(IRGenFunction &IGF, Explosion &value,
                                           SILType metatypeTy,
                                           SILType existentialTy,
                                           Explosion &out) {
  assert(existentialTy.isClassExistentialType());
  auto &baseTI = IGF.getTypeInfo(existentialTy).as<ClassExistentialTypeInfo>();

  // Extract the class instance pointer.
  auto tablesAndValue = baseTI.getWitnessTablesAndValue(value);

  // Get the type metadata.
  llvm::Value *instance = tablesAndValue.second;

  auto metaTy = metatypeTy.castTo<ExistentialMetatypeType>();
  auto repr = metaTy->getRepresentation();
  assert(repr != MetatypeRepresentation::Thin &&
         "Class metatypes should have a thin representation");
  assert((IGF.IGM.ObjCInterop || repr != MetatypeRepresentation::ObjC) &&
         "Class metatypes should not have ObjC representation without runtime");

  if (repr == MetatypeRepresentation::Thick) {
    auto dynamicType = emitDynamicTypeOfOpaqueHeapObject(IGF, instance);
    out.add(dynamicType);
  } else if (repr == MetatypeRepresentation::ObjC) {
    auto dynamicType = emitHeapMetadataRefForUnknownHeapObject(IGF, instance);
    out.add(dynamicType);
  } else {
    llvm_unreachable("Unknown metatype representation");
  }

  // Get the witness tables.
  out.add(tablesAndValue.first);
}

void irgen::emitMetatypeOfMetatype(IRGenFunction &IGF, Explosion &value,
                                           SILType existentialTy,
                                           Explosion &out) {
  assert(existentialTy.is<ExistentialMetatypeType>());
  auto &baseTI = IGF.getTypeInfo(existentialTy).as<ExistentialMetatypeTypeInfo>();

  auto tablesAndValue = baseTI.getWitnessTablesAndValue(value);

  llvm::Value *dynamicType = IGF.Builder.CreateCall(
                    IGF.IGM.getGetMetatypeMetadataFn(), tablesAndValue.second);
  out.add(dynamicType);
  out.add(tablesAndValue.first);
}

/// Emit a projection from an existential container to its concrete value
/// buffer with the type metadata for the contained value.
///
/// \param _openedArchetype When non-null, the opened archetype
/// that captures the details of this existential.
std::pair<Address, llvm::Value*>
irgen::emitIndirectExistentialProjectionWithMetadata(IRGenFunction &IGF,
                                                     Address base,
                                                     SILType baseTy,
                                                     CanType _openedArchetype) {
  CanArchetypeType openedArchetype;
  if (_openedArchetype) openedArchetype = cast<ArchetypeType>(_openedArchetype);

  assert(baseTy.isExistentialType());
  if (baseTy.isClassExistentialType()) {
    auto &baseTI = IGF.getTypeInfo(baseTy).as<ClassExistentialTypeInfo>();
    auto valueAddr = baseTI.projectValue(IGF, base);
    auto value = IGF.Builder.CreateLoad(valueAddr);
    auto metadata = emitDynamicTypeOfOpaqueHeapObject(IGF, value);

    // If we are projecting into an opened archetype, capture the
    // witness tables.
    if (openedArchetype) {
      SmallVector<llvm::Value *, 4> wtables;
      for (unsigned i = 0, n = baseTI.getNumStoredProtocols(); i != n; ++i) {
        auto wtableAddr = baseTI.projectWitnessTable(IGF, base, i);
        wtables.push_back(IGF.Builder.CreateLoad(wtableAddr));
      }

      IGF.bindArchetype(openedArchetype, metadata, wtables);
    }

    return {valueAddr, metadata};
  } else {
    auto &baseTI = IGF.getTypeInfo(baseTy).as<OpaqueExistentialTypeInfo>();
    auto layout = baseTI.getLayout();

    llvm::Value *metadata = layout.loadMetadataRef(IGF, base);
    Address buffer = layout.projectExistentialBuffer(IGF, base);
    llvm::Value *object =
      emitProjectBufferCall(IGF, metadata, buffer);

    // If we are projecting into an opened archetype, capture the
    // witness tables.
    if (openedArchetype) {
      SmallVector<llvm::Value *, 4> wtables;
      for (unsigned i = 0, n = layout.getNumTables(); i != n; ++i) {
        wtables.push_back(layout.loadWitnessTable(IGF, base, i));
      }
      IGF.bindArchetype(openedArchetype, metadata, wtables);
    }

    return {Address(object, Alignment(1)), metadata};
  }
}

/// Emit a projection from an existential container to its concrete value
/// buffer.
Address irgen::emitOpaqueExistentialProjection(IRGenFunction &IGF,
                                               Address base,
                                               SILType baseTy,
                                               CanArchetypeType openedArchetype)
{
  return emitIndirectExistentialProjectionWithMetadata(IGF, base, baseTy,
                                                       openedArchetype)
    .first;
}

/// Extract the instance pointer from a class existential value.
llvm::Value *
irgen::emitClassExistentialProjection(IRGenFunction &IGF,
                                      Explosion &base,
                                      SILType baseTy,
                                      CanArchetypeType openedArchetype) {
  assert(baseTy.isClassExistentialType());
  auto &baseTI = IGF.getTypeInfo(baseTy).as<ClassExistentialTypeInfo>();

  if (!openedArchetype)
    return baseTI.getValue(IGF, base);

  // Capture the metadata and witness tables from this existential
  // into the given archetype.
  ArrayRef<llvm::Value*> wtables;
  llvm::Value *value;
  std::tie(wtables, value) = baseTI.getWitnessTablesAndValue(base);
  auto metadata = emitDynamicTypeOfOpaqueHeapObject(IGF, value);
  IGF.bindArchetype(openedArchetype, metadata, wtables);

  return value;
}

/// Extract the metatype pointer from a class existential value.
llvm::Value *
irgen::emitExistentialMetatypeProjection(IRGenFunction &IGF,
                                         Explosion &base,
                                         SILType baseTy,
                                         CanType openedTy) {
  assert(baseTy.is<ExistentialMetatypeType>());
  auto &baseTI = IGF.getTypeInfo(baseTy).as<ExistentialMetatypeTypeInfo>();

  if (!openedTy)
    return baseTI.getValue(IGF, base);

  // Capture the metadata and witness tables from this existential
  // into the given archetype.
  ArrayRef<llvm::Value*> wtables;
  llvm::Value *value;
  std::tie(wtables, value) = baseTI.getWitnessTablesAndValue(base);

  auto existentialType = baseTy.castTo<ExistentialMetatypeType>();
  auto targetType = cast<MetatypeType>(openedTy);

  // If we're starting with an ObjC representation, convert it to a
  // class type and let's go.
  llvm::Value *metatype;
  if (existentialType->getRepresentation() == MetatypeRepresentation::ObjC) {
    metatype = emitObjCMetadataRefForMetadata(IGF, value);

  // Otherwise, we have type metadata.
  } else {
    assert(existentialType->getRepresentation()
             == MetatypeRepresentation::Thick);
    metatype = value;

    // The type we need to bind to the archetype is the one that's
    // deep in the type.
    while (!isa<ArchetypeType>(targetType.getInstanceType())) {
      targetType = cast<MetatypeType>(targetType.getInstanceType());
      existentialType =
        cast<ExistentialMetatypeType>(existentialType.getInstanceType());
      metatype = emitMetatypeInstanceType(IGF, metatype);
    }
  }

  auto openedArchetype = cast<ArchetypeType>(targetType.getInstanceType());
  IGF.bindArchetype(openedArchetype, metatype, wtables);

  return value;
}
