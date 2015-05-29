//===--- GenProto.cpp - Swift IR Generation for Protocols -----------------===//
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
//  This file implements IR generation for protocols in Swift.
//
//  Protocols serve two masters: generic algorithms and existential
//  types.  In either case, the size and structure of a type is opaque
//  to the code manipulating a value.  Local values of the type must
//  be stored in fixed-size buffers (which can overflow to use heap
//  allocation), and basic operations on the type must be dynamically
//  delegated to a collection of information that "witnesses" the
//  truth that a particular type implements the protocol.
//
//  In the comments throughout this file, three type names are used:
//    'B' is the type of a fixed-size buffer
//    'T' is the type which implements a protocol
//    'W' is the type of a witness to the protocol
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/CanTypeVisitor.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/IRGenOptions.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILWitnessVisitor.h"
#include "swift/SIL/TypeLowering.h"
#include "clang/AST/DeclObjC.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"

#include "CallEmission.h"
#include "EnumPayload.h"
#include "Explosion.h"
#include "FixedTypeInfo.h"
#include "GenClass.h"
#include "GenHeap.h"
#include "GenMeta.h"
#include "GenOpaque.h"
#include "GenPoly.h"
#include "GenType.h"
#include "HeapTypeInfo.h"
#include "IndirectTypeInfo.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Linking.h"
#include "NecessaryBindings.h"
#include "NonFixedTypeInfo.h"
#include "ProtocolInfo.h"
#include "TypeInfo.h"
#include "UnownedTypeInfo.h"
#include "WeakTypeInfo.h"

#include "GenProto.h"

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

    /*
    friend bool operator==(ExistentialLayout a, ExistentialLayout b) {
      return a.NumTables == b.NumTables;
    }*/

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
      return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr, which),
                                    "witness-table");
    }

    /// Given the address of an existential object, drill down to the
    /// metadata field.
    Address projectMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateStructGEP(addr, 1, getFixedBufferSize(IGF.IGM));
    }

    /// Given the address of an existential object, load its metadata
    /// object.
    llvm::Value *loadMetadataRef(IRGenFunction &IGF, Address addr) {
      return IGF.Builder.CreateLoad(projectMetadataRef(IGF, addr),
                               addr.getAddress()->getName() + ".metadata");
    }
  };

  /// A concrete witness table, together with its known layout.
  class WitnessTable {
    llvm::Value *Table;
    const ProtocolInfo &Info;
  public:
    WitnessTable(llvm::Value *wtable, const ProtocolInfo &info)
      : Table(wtable), Info(info) {}

    llvm::Value *getTable() const { return Table; }
    const ProtocolInfo &getInfo() const { return Info; }
  };
}

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

  /// A class which lays out a witness table in the abstract.
  class WitnessTableLayout : public SILWitnessVisitor<WitnessTableLayout> {
    unsigned NumWitnesses = 0;
    SmallVector<WitnessTableEntry, 16> Entries;

    WitnessIndex getNextIndex() {
      return WitnessIndex(NumWitnesses++, /*isPrefix=*/false);
    }

  public:
    /// The next witness is an out-of-line base protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
      Entries.push_back(
             WitnessTableEntry::forOutOfLineBase(baseProto, getNextIndex()));
    }

    void addMethod(FuncDecl *func) {
      Entries.push_back(WitnessTableEntry::forFunction(func, getNextIndex()));
    }

    void addConstructor(ConstructorDecl *ctor) {
      Entries.push_back(WitnessTableEntry::forFunction(ctor, getNextIndex()));
    }

    void addAssociatedType(AssociatedTypeDecl *ty,
                           ArrayRef<ProtocolDecl *> protos) {
      // An associated type takes up a spot for the type metadata and for the
      // witnesses to all its conformances.
      Entries.push_back(
                      WitnessTableEntry::forAssociatedType(ty, getNextIndex()));
      for (auto *proto : protos)
        if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
          ++NumWitnesses;
    }

    unsigned getNumWitnesses() const { return NumWitnesses; }
    ArrayRef<WitnessTableEntry> getEntries() const { return Entries; }
  };

  /// A path through a protocol hierarchy.
  class ProtocolPath {
    IRGenModule &IGM;

    /// The destination protocol.
    ProtocolDecl *Dest;

    /// The path from the selected origin down to the destination
    /// protocol.
    SmallVector<WitnessIndex, 8> ReversePath;

    /// The origin index to use.
    unsigned OriginIndex;

    /// The best path length we found.
    unsigned BestPathLength;

  public:
    /// Find a path from the given set of origins to the destination
    /// protocol.
    ///
    /// T needs to provide a couple of member functions:
    ///   ProtocolDecl *getProtocol() const;
    ///   const ProtocolInfo &getInfo() const;
    template <class T>
    ProtocolPath(IRGenModule &IGM, ArrayRef<T> origins, ProtocolDecl *dest)
      : IGM(IGM), Dest(dest), BestPathLength(~0U) {

      // Consider each of the origins in turn, breaking out if any of
      // them yields a zero-length path.
      for (unsigned i = 0, e = origins.size(); i != e; ++i) {
        auto &origin = origins[i];
        if (considerOrigin(origin.getProtocol(), origin.getInfo(), i))
          break;
      }

      // Sanity check that we actually found a path at all.
      assert(BestPathLength != ~0U);
      assert(BestPathLength == ReversePath.size());
    }

    /// Returns the index of the origin protocol we chose.
    unsigned getOriginIndex() const { return OriginIndex; }

    /// Apply the path to the given witness table.
    llvm::Value *apply(IRGenFunction &IGF, llvm::Value *wtable) const {
      for (unsigned i = ReversePath.size(); i != 0; --i) {
        wtable = emitLoadOfOpaqueWitness(IGF, wtable, ReversePath[i-1]);
        wtable = IGF.Builder.CreateBitCast(wtable, IGF.IGM.WitnessTablePtrTy);
      }
      return wtable;
    }

  private:
    /// Consider paths starting from a new origin protocol.
    /// Returns true if there's no point in considering other origins.
    bool considerOrigin(ProtocolDecl *origin, const ProtocolInfo &originInfo,
                        unsigned originIndex) {
      assert(BestPathLength != 0);

      // If the origin *is* the destination, we can stop here.
      if (origin == Dest) {
        OriginIndex = originIndex;
        BestPathLength = 0;
        ReversePath.clear();
        return true;
      }

      // Otherwise, if the origin gives rise to a better path, that's
      // also cool.
      if (findBetterPath(origin, originInfo, 0)) {
        OriginIndex = originIndex;
        return BestPathLength == 0;
      }

      return false;
    }

    /// Consider paths starting at the given protocol.
    bool findBetterPath(ProtocolDecl *proto, const ProtocolInfo &protoInfo,
                        unsigned lengthSoFar) {
      assert(lengthSoFar < BestPathLength);
      assert(proto != Dest);

      // Keep track of whether we found a better path than the
      // previous best.
      bool foundBetter = false;
      for (auto base : proto->getInheritedProtocols(nullptr)) {
        // ObjC protocols do not have witnesses.
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(base))
          continue;

        auto &baseEntry = protoInfo.getWitnessEntry(base);
        assert(baseEntry.isBase());

        // Compute the length down to this base.
        unsigned lengthToBase = lengthSoFar;
        if (baseEntry.isOutOfLineBase()) {
          lengthToBase++;

          // Don't consider this path if we reach a length that can't
          // possibly be better than the best so far.
          if (lengthToBase == BestPathLength) continue;
        }
        assert(lengthToBase < BestPathLength);

        // If this base *is* the destination, go ahead and start
        // building the path into ReversePath.
        if (base == Dest) {
          // Reset the collected best-path information.
          BestPathLength = lengthToBase;
          ReversePath.clear();

        // Otherwise, if there isn't a better path through this base,
        // don't accumulate anything in the path.
        } else if (!findBetterPath(base, IGM.getProtocolInfo(base),
                                   lengthToBase)) {
          continue;
        }

        // Okay, we've found a better path, and ReversePath contains a
        // path leading from base to Dest.
        assert(BestPathLength >= lengthToBase);
        foundBetter = true;

        // Add the link from proto to base if necessary.
        if (baseEntry.isOutOfLineBase()) {
          ReversePath.push_back(baseEntry.getOutOfLineBaseIndex());

        // If it isn't necessary, then we might be able to
        // short-circuit considering the bases of this protocol.
        } else {
          if (lengthSoFar == BestPathLength)
            return true;
        }
      }

      return foundBetter;
    }
  };

  /// An entry in an existential type's list of known protocols.
  class ProtocolEntry {
    ProtocolDecl *Protocol;
    const ProtocolInfo &Impl;

  public:
    explicit ProtocolEntry(ProtocolDecl *proto, const ProtocolInfo &impl)
      : Protocol(proto), Impl(impl) {}

    ProtocolDecl *getProtocol() const { return Protocol; }
    const ProtocolInfo &getInfo() const { return Impl; }
  };

  /// A helper class for implementing existential type infos that
  /// store an existential value of some sort.
  template <class Derived, class Base>
  class ExistentialTypeInfoBase : public Base {
    /// The number of non-trivial protocols for this existential.
    unsigned NumStoredProtocols;

    ProtocolEntry *getStoredProtocolsBuffer() {
      return reinterpret_cast<ProtocolEntry *>(&asDerived() + 1);
    }
    const ProtocolEntry *getStoredProtocolsBuffer() const {
      return reinterpret_cast<const ProtocolEntry *>(&asDerived() + 1);
    }

  protected:
    const ExistentialTypeInfoBase<Derived, Base> &asExistentialTI() const {
      return *this;
    }

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
          NumStoredProtocols(protocols.size())  {
      for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
        new (&getStoredProtocolsBuffer()[i]) ProtocolEntry(protocols[i]);
      }
    }

  public:
    template <class... As>
    static const Derived *
    create(ArrayRef<ProtocolEntry> protocols, As &&...args)
    {
      void *buffer = operator new(sizeof(Derived) +
                                  protocols.size() * sizeof(ProtocolEntry));
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
      return ArrayRef<ProtocolEntry>(getStoredProtocolsBuffer(),
                                     NumStoredProtocols);
    }

    /// Given an existential object, find the witness table
    /// corresponding to the given protocol.
    llvm::Value *findWitnessTable(IRGenFunction &IGF,
                                  Explosion &container,
                                  ProtocolDecl *protocol) const {
      assert(NumStoredProtocols != 0 &&
             "finding a witness table in a trivial existential");

      ProtocolPath path(IGF.IGM, getStoredProtocols(), protocol);
      llvm::Value *witness
        = asDerived().extractWitnessTable(IGF, container,
                                          path.getOriginIndex());
      return path.apply(IGF, witness);
    }

    /// Given the address of an existential object, find the witness
    /// table corresponding to the given protocol.
    llvm::Value *findWitnessTable(IRGenFunction &IGF, Address obj,
                                  ProtocolDecl *protocol) const {
      assert(NumStoredProtocols != 0 &&
             "finding a witness table in a trivial existential");

      ProtocolPath path(IGF.IGM, getStoredProtocols(), protocol);
      llvm::Value *originTable =
        asDerived().loadWitnessTable(IGF, obj, path.getOriginIndex());
      return path.apply(IGF, originTable);
    }

    /// Given the witness table vector from an existential object, find the
    /// witness table corresponding to the given protocol.
    llvm::Value *findWitnessTable(IRGenFunction &IGF,
                                  ArrayRef<llvm::Value *> witnesses,
                                  ProtocolDecl *protocol) const {
      ProtocolPath path(IGF.IGM, getStoredProtocols(), protocol);
      return path.apply(IGF, witnesses[path.getOriginIndex()]);
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
  ///   protocol<Printable, Serializable>
  /// with the semantic translation:
  ///   \exists t : Printable . t
  /// t here is an ArchetypeType.
  ///
  /// This is used for both ProtocolTypes and ProtocolCompositionTypes.
  class OpaqueExistentialTypeInfo :
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
              IsNotPOD, IsNotBitwiseTakable) {}

  public:
    OpaqueExistentialLayout getLayout() const {
      return OpaqueExistentialLayout(getNumStoredProtocols());
    }

    Address projectWitnessTable(IRGenFunction &IGF, Address obj,
                                unsigned index) const {
      return getLayout().projectWitnessTable(IGF, obj, index);
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                        SILType T) const {
      auto objPtrTy = dest.getAddress()->getType();
      auto fn = getAssignExistentialsFunction(IGF.IGM, objPtrTy, getLayout());
      auto call = IGF.Builder.CreateCall2(fn, dest.getAddress(),
                                          src.getAddress());
      call->setCallingConv(IGF.IGM.RuntimeCC);
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
                            SILType T) const {
      llvm::Value *metadata = copyType(IGF, dest, src);

      auto layout = getLayout();

      // Project down to the buffers and ask the witnesses to do a
      // copy-initialize.
      Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
      Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithCopyOfBufferCall(IGF, metadata,
                                               destBuffer, srcBuffer);
    }

    void initializeWithTake(IRGenFunction &IGF,
                            Address dest, Address src,
                            SILType T) const {
      llvm::Value *metadata = copyType(IGF, dest, src);

      auto layout = getLayout();

      // Project down to the buffers and ask the witnesses to do a
      // take-initialize.
      Address srcBuffer = layout.projectExistentialBuffer(IGF, src);
      Address destBuffer = layout.projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithTakeOfBufferCall(IGF, metadata,
                                               destBuffer, srcBuffer);
    }

    void destroy(IRGenFunction &IGF, Address addr, SILType T) const {
      emitDestroyExistential(IGF, addr, getLayout());
    }
  };

  /// A type implementation for 'weak' existential types.
  class WeakClassExistentialTypeInfo :
      public ExistentialTypeInfoBase<WeakClassExistentialTypeInfo,
               IndirectTypeInfo<WeakClassExistentialTypeInfo, WeakTypeInfo>> {
    using super =
             ExistentialTypeInfoBase<WeakClassExistentialTypeInfo,
               IndirectTypeInfo<WeakClassExistentialTypeInfo, WeakTypeInfo>>;
  public:
    WeakClassExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                                 llvm::Type *ty, Size size, Alignment align,
                                 SpareBitVector &&spareBits)
      : super(protocols, ty, size, align, std::move(spareBits)) {
    }

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
      IGF.emitUnknownWeakCopyAssign(destValue, srcValue);
      emitCopyOfTables(IGF, dest, src);
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src,
                            SILType T) const override {
      Address destValue = projectValue(IGF, dest);
      Address srcValue = projectValue(IGF, src);
      IGF.emitUnknownWeakCopyInit(destValue, srcValue);
      emitCopyOfTables(IGF, dest, src);
    }

    void assignWithTake(IRGenFunction &IGF,
                        Address dest, Address src,
                        SILType T) const override {
      Address destValue = projectValue(IGF, dest);
      Address srcValue = projectValue(IGF, src);
      IGF.emitUnknownWeakTakeAssign(destValue, srcValue);
      emitCopyOfTables(IGF, dest, src);
    }

    void initializeWithTake(IRGenFunction &IGF,
                            Address dest, Address src,
                            SILType T) const override {
      Address destValue = projectValue(IGF, dest);
      Address srcValue = projectValue(IGF, src);
      IGF.emitUnknownWeakTakeInit(destValue, srcValue);
      emitCopyOfTables(IGF, dest, src);
    }

    void destroy(IRGenFunction &IGF, Address existential,
                 SILType T) const override {
      Address valueAddr = projectValue(IGF, existential);
      IGF.emitUnknownWeakDestroy(valueAddr);
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
      OutE.add(IGF.Builder.CreateBitOrPointerCast(weak,
                                          IGF.IGM.UnknownRefCountedPtrTy));

      // Collect the witness tables.
      for (unsigned i = 0, e = getNumStoredProtocols(); i != e; ++i) {
        llvm::Value *witness = InE.claimNext();
        OutE.add(IGF.Builder.CreateBitOrPointerCast(witness,
                                                    IGF.IGM.WitnessTablePtrTy));
      }
    }

    // These explosions must follow the same schema as
    // ClassExistentialTypeInfo, i.e. first the value, then the tables.

    void weakLoadStrong(IRGenFunction &IGF, Address existential,
                        Explosion &out) const override {
      Explosion temp;
      Address valueAddr = projectValue(IGF, existential);
      temp.add(IGF.emitUnknownWeakLoadStrong(valueAddr,
                                            IGF.IGM.UnknownRefCountedPtrTy));
      emitLoadOfTables(IGF, existential, temp);
      mergeExplosion(temp, out, IGF);
    }

    void weakTakeStrong(IRGenFunction &IGF, Address existential,
                        Explosion &out) const override {
      Explosion temp;
      Address valueAddr = projectValue(IGF, existential);
      temp.add(IGF.emitUnknownWeakTakeStrong(valueAddr,
                                            IGF.IGM.UnknownRefCountedPtrTy));
      emitLoadOfTables(IGF, existential, temp);
      mergeExplosion(temp, out, IGF);
    }

    void weakInit(IRGenFunction &IGF, Explosion &in,
                  Address existential) const override {
      Explosion temp;
      decomposeExplosion(in, temp, IGF);

      llvm::Value *value = temp.claimNext();
      assert(value->getType() == IGF.IGM.UnknownRefCountedPtrTy);
      emitStoreOfTables(IGF, temp, existential);
      Address valueAddr = projectValue(IGF, existential);
      IGF.emitUnknownWeakInit(value, valueAddr);
    }

    void weakAssign(IRGenFunction &IGF, Explosion &in,
                    Address existential) const override {
      Explosion temp;
      decomposeExplosion(in, temp, IGF);

      llvm::Value *value = temp.claimNext();
      assert(value->getType() == IGF.IGM.UnknownRefCountedPtrTy);
      emitStoreOfTables(IGF, temp, existential);
      Address valueAddr = projectValue(IGF, existential);
      IGF.emitUnknownWeakAssign(value, valueAddr);
    }
  };

  /// A helper class for working with existential types that can be
  /// exploded into scalars.
  ///
  /// The subclass must provide:
  ///   void emitPayloadRetain(IRGenFunction &IGF, llvm::Value *payload) const;
  ///   void emitPayloadRelease(IRGenFunction &IGF, llvm::Value *payload) const;
  ///   void emitPayloadFixLifetime(IRGenFunction &IGF,
  ///                               llvm::Value *payload) const;
  ///   const LoadableTypeInfo &
  ///       getPayloadTypeInfoForExtraInhabitants(IRGenModule &IGM) const;
  /// The payload type info is only used to manage extra inhabitants, so it's
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

    unsigned getExplosionSize() const override {
      return 1 + getNumStoredProtocols();
    }

    void getSchema(ExplosionSchema &schema) const override {
      llvm::StructType *ty = getStorageType();
      for (unsigned i = 0, e = getExplosionSize(); i != e; ++i)
        schema.add(ExplosionSchema::Element::forScalar(ty->getElementType(i)));
    }

    /// Given the address of a class existential container, returns
    /// the address of a witness table pointer.
    Address projectWitnessTable(IRGenFunction &IGF, Address address,
                                unsigned n) const {
      assert(n < getNumStoredProtocols() && "witness table index out of bounds");
      return IGF.Builder.CreateStructGEP(address, n+1,
                                         IGF.IGM.getPointerSize() * (n+1));
    }

    /// Given the address of a class existential container, returns
    /// the address of its instance pointer.
    Address projectValue(IRGenFunction &IGF, Address address) const {
      return IGF.Builder.CreateStructGEP(address, 0, Size(0));
    }

    llvm::Value *loadValue(IRGenFunction &IGF, Address addr) const {
      return IGF.Builder.CreateLoad(projectValue(IGF, addr));
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
      container.claim(getNumStoredProtocols());
      return instance;
    }

    void loadAsCopy(IRGenFunction &IGF, Address address,
                    Explosion &out) const override {
      // Load the instance pointer, which is unknown-refcounted.
      llvm::Value *instance
        = IGF.Builder.CreateLoad(projectValue(IGF, address));
      asDerived().emitPayloadRetain(IGF, instance);
      out.add(instance);

      // Load the witness table pointers.
      asDerived().emitLoadOfTables(IGF, address, out);
    }

    void loadAsTake(IRGenFunction &IGF, Address address,
                    Explosion &e) const override {
      // Load the instance pointer.
      e.add(IGF.Builder.CreateLoad(projectValue(IGF, address)));

      // Load the witness table pointers.
      asDerived().emitLoadOfTables(IGF, address, e);
    }

    void assign(IRGenFunction &IGF, Explosion &e,
                Address address) const override {
      // Assign the value.
      Address instanceAddr = projectValue(IGF, address);
      llvm::Value *old = IGF.Builder.CreateLoad(instanceAddr);
      IGF.Builder.CreateStore(e.claimNext(), instanceAddr);
      asDerived().emitPayloadRelease(IGF, old);

      // Store the witness table pointers.
      asDerived().emitStoreOfTables(IGF, e, address);
    }

    void initialize(IRGenFunction &IGF, Explosion &e,
                    Address address) const override {
      // Store the instance pointer.
      IGF.Builder.CreateStore(e.claimNext(),
                              projectValue(IGF, address));

      // Store the witness table pointers.
      asDerived().emitStoreOfTables(IGF, e, address);
    }

    void copy(IRGenFunction &IGF, Explosion &src, Explosion &dest)
    const override {
      // Copy the instance pointer.
      llvm::Value *value = src.claimNext();
      dest.add(value);
      asDerived().emitPayloadRetain(IGF, value);

      // Transfer the witness table pointers.
      src.transferInto(dest, getNumStoredProtocols());
    }

    void consume(IRGenFunction &IGF, Explosion &src)
    const override {
      // Copy the instance pointer.
      llvm::Value *value = src.claimNext();
      asDerived().emitPayloadRelease(IGF, value);

      // Throw out the witness table pointers.
      src.claim(getNumStoredProtocols());
    }

    void fixLifetime(IRGenFunction &IGF, Explosion &src) const override {
      // Copy the instance pointer.
      llvm::Value *value = src.claimNext();
      asDerived().emitPayloadFixLifetime(IGF, value);

      // Throw out the witness table pointers.
      src.claim(getNumStoredProtocols());
    }

    void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
      llvm::Value *value = IGF.Builder.CreateLoad(projectValue(IGF, addr));
      asDerived().emitPayloadRelease(IGF, value);
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
      assert(asDerived().getPayloadTypeInfoForExtraInhabitants(IGM)
                        .mayHaveExtraInhabitants(IGM));
      return true;
    }

    unsigned getFixedExtraInhabitantCount(IRGenModule &IGM) const override {
      return asDerived().getPayloadTypeInfoForExtraInhabitants(IGM)
                        .getFixedExtraInhabitantCount(IGM);
    }

    APInt getFixedExtraInhabitantValue(IRGenModule &IGM,
                                       unsigned bits,
                                       unsigned index) const override {
      // Note that we pass down the original bit-width.
      return asDerived().getPayloadTypeInfoForExtraInhabitants(IGM)
                        .getFixedExtraInhabitantValue(IGM, bits, index);
    }

    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF, Address src,
                                         SILType T)
    const override {
      // NB: We assume that the witness table slots are zero if an extra
      // inhabitant is stored in the container.
      src = projectValue(IGF, src);
      return asDerived().getPayloadTypeInfoForExtraInhabitants(IGF.IGM)
                        .getExtraInhabitantIndex(IGF, src, SILType());
    }

    void storeExtraInhabitant(IRGenFunction &IGF, llvm::Value *index,
                              Address dest, SILType T) const override {
      Address valueDest = projectValue(IGF, dest);
      asDerived().getPayloadTypeInfoForExtraInhabitants(IGF.IGM)
                 .storeExtraInhabitant(IGF, index, valueDest, SILType());
    }

    APInt getFixedExtraInhabitantMask(IRGenModule &IGM) const override {
      // Ask the payload type for its mask.
      APInt bits = asDerived().getPayloadTypeInfoForExtraInhabitants(IGM)
                              .getFixedExtraInhabitantMask(IGM);
      
      // Zext out to the size of the existential.
      bits = bits.zextOrTrunc(asDerived().getFixedSize().getValueInBits());
      return bits;
    }
  };

  /// A type implementation for [unowned] class existential types.
  class UnownedClassExistentialTypeInfo
    : public ScalarExistentialTypeInfoBase<UnownedClassExistentialTypeInfo,
                                           UnownedTypeInfo> {
  public:
    UnownedClassExistentialTypeInfo(ArrayRef<ProtocolEntry> storedProtocols,
                                    llvm::Type *ty,
                                    const SpareBitVector &spareBits,
                                    Size size, Alignment align)
      : ScalarExistentialTypeInfoBase(storedProtocols, ty, size,
                                      spareBits, align) {}

    const LoadableTypeInfo &
    getPayloadTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
      return IGM.getUnknownObjectTypeInfo();
    }

    void emitPayloadRetain(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitUnknownUnownedRetain(value);
    }

    void emitPayloadRelease(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitUnknownUnownedRelease(value);
    }

    void emitPayloadFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitFixLifetime(value);
    }
  };

  /// A type implementation for @unowned(unsafe) class existential types.
  class UnmanagedClassExistentialTypeInfo
    : public ScalarExistentialTypeInfoBase<UnmanagedClassExistentialTypeInfo,
                                           LoadableTypeInfo> {
  public:
    UnmanagedClassExistentialTypeInfo(ArrayRef<ProtocolEntry> storedProtocols,
                                      llvm::Type *ty,
                                      const SpareBitVector &spareBits,
                                      Size size, Alignment align)
      : ScalarExistentialTypeInfoBase(storedProtocols, ty, size,
                                      spareBits, align, IsPOD) {}

    const LoadableTypeInfo &
    getPayloadTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
      return IGM.getUnknownObjectTypeInfo();
    }

    void emitPayloadRetain(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }

    void emitPayloadRelease(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }

    void emitPayloadFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }
  };

  /// A type info implementation for class existential types, that is,
  /// an existential type known to conform to one or more class protocols.
  /// Class existentials can be represented directly as an aggregation
  /// of a refcounted pointer plus witness tables instead of using an indirect
  /// buffer.
  class ClassExistentialTypeInfo
    : public ScalarExistentialTypeInfoBase<ClassExistentialTypeInfo,
                                           ReferenceTypeInfo>
  {
    friend ExistentialTypeInfoBase;
    ClassExistentialTypeInfo(ArrayRef<ProtocolEntry> protocols,
                             llvm::Type *ty,
                             Size size,
                             SpareBitVector &&spareBits,
                             Alignment align)
      : ScalarExistentialTypeInfoBase(protocols, ty, size,
                                      std::move(spareBits), align)
    {}
  public:

    /// Class existentials are single refcounted pointers if they have no
    /// witness tables. Right now we have no way of constraining an existential
    /// to Swift-refcounted types.
    bool isSingleSwiftRetainablePointer(ResilienceScope scope) const override {
      return false;
    }
    bool isSingleUnknownRetainablePointer(ResilienceScope scope) const override{
      return getNumStoredProtocols() == 0;
    }

    const LoadableTypeInfo &
    getPayloadTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
      return IGM.getUnknownObjectTypeInfo();
    }

    void retain(IRGenFunction &IGF, Explosion &e) const override {
      // The instance is treated as unknown-refcounted.
      IGF.emitUnknownRetainCall(e.claimNext());
      e.claim(getNumStoredProtocols());
    }

    void release(IRGenFunction &IGF, Explosion &e) const override {
      // The instance is treated as unknown-refcounted.
      IGF.emitUnknownRelease(e.claimNext());
      e.claim(getNumStoredProtocols());
    }

    void retainUnowned(IRGenFunction &IGF, Explosion &e) const override {
      // The instance is treated as unknown-refcounted.
      IGF.emitUnknownRetainUnowned(e.claimNext());
      e.claim(getNumStoredProtocols());
    }

    void unownedRetain(IRGenFunction &IGF, Explosion &e) const override {
      // The instance is treated as unknown-refcounted.
      IGF.emitUnknownUnownedRetain(e.claimNext());
      e.claim(getNumStoredProtocols());
    }

    void unownedRelease(IRGenFunction &IGF, Explosion &e) const override {
      // The instance is treated as unknown-refcounted.
      IGF.emitUnknownUnownedRelease(e.claimNext());
      e.claim(getNumStoredProtocols());
    }

    void emitPayloadRetain(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitUnknownRetainCall(value);
    }

    void emitPayloadRelease(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitUnknownRelease(value);
    }

    void emitPayloadFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
      IGF.emitFixLifetime(value);
    }

    LoadedRef loadRefcountedPtr(IRGenFunction &IGF, SourceLoc loc,
                                Address addr) const override {
      return LoadedRef(IGF.emitLoadUnknownRefcountedPtr(addr), true);
    }

    const UnownedTypeInfo *
    createUnownedStorageType(TypeConverter &TC) const override {
      // We can just re-use the storage type for the @unowned(safe) type.
      return UnownedClassExistentialTypeInfo::create(getStoredProtocols(),
                                                     getStorageType(),
                                                     getSpareBits(),
                                                     getFixedSize(),
                                                     getFixedAlignment());
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

      // We need to build a new struct for the [weak] type because the weak
      // component is not necessarily pointer-sized.
      SmallVector<llvm::Type*, 8> fieldTys;
      fieldTys.push_back(TC.IGM.WeakReferencePtrTy->getElementType());
      fieldTys.resize(getNumStoredProtocols() + 1, TC.IGM.WitnessTablePtrTy);
      auto storageTy = llvm::StructType::get(TC.IGM.getLLVMContext(), fieldTys);

      SpareBitVector spareBits = TC.IGM.getWeakReferenceSpareBits();
      for (unsigned i = 0, e = getNumStoredProtocols(); i != e; ++i)
        spareBits.append(TC.IGM.getWitnessTablePtrSpareBits());

      return WeakClassExistentialTypeInfo::create(getStoredProtocols(),
                                                  storageTy, size, align,
                                                  std::move(spareBits));
    }
  };

  /// A type implementation for existential metatypes.
  class ExistentialMetatypeTypeInfo
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
                                      std::move(spareBits), align, IsPOD),
        MetatypeTI(metatypeTI) {}

  public:
    const LoadableTypeInfo &
    getPayloadTypeInfoForExtraInhabitants(IRGenModule &IGM) const {
      return MetatypeTI;
    }

    void emitPayloadRetain(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }

    void emitPayloadRelease(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }

    void emitPayloadFixLifetime(IRGenFunction &IGF, llvm::Value *value) const {
      // do nothing
    }
  };

  /// Common type implementation details for all archetypes.
  class ArchetypeTypeInfoBase {
  protected:
    unsigned NumStoredProtocols;
    ProtocolEntry *StoredProtocolsBuffer;

    ArchetypeTypeInfoBase(void *protocolsBuffer,
                          ArrayRef<ProtocolEntry> protocols)
      : NumStoredProtocols(protocols.size()),
        StoredProtocolsBuffer(reinterpret_cast<ProtocolEntry*>(protocolsBuffer))
    {
      for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
        ::new (&StoredProtocolsBuffer[i]) ProtocolEntry(protocols[i]);
      }
    }

  public:
    unsigned getNumStoredProtocols() const {
      return NumStoredProtocols;
    }

    ArrayRef<ProtocolEntry> getStoredProtocols() const {
      return llvm::makeArrayRef(StoredProtocolsBuffer, getNumStoredProtocols());
    }

    /// Return the witness table that's been set for this type.
    llvm::Value *getWitnessTable(IRGenFunction &IGF,
                                 CanArchetypeType archetype,
                                 unsigned which) const {
      assert(which < getNumStoredProtocols());
      return IGF.getLocalTypeData(archetype,
                            LocalTypeData::forArchetypeProtocolWitness(which));
    }
  };

  /// A type implementation for an ArchetypeType, otherwise known as a
  /// type variable: for example, This in a protocol declaration, or T
  /// in a generic declaration like foo<T>(x : T) -> T.  The critical
  /// thing here is that performing an operation involving archetypes
  /// is dependent on the witness binding we can see.
  class OpaqueArchetypeTypeInfo
    : public IndirectTypeInfo<OpaqueArchetypeTypeInfo,
                              WitnessSizedTypeInfo<OpaqueArchetypeTypeInfo>>,
      public ArchetypeTypeInfoBase
  {
    OpaqueArchetypeTypeInfo(llvm::Type *type,
                            ArrayRef<ProtocolEntry> protocols)
      : IndirectTypeInfo(type, Alignment(1), IsNotPOD, IsNotBitwiseTakable),
        ArchetypeTypeInfoBase(this + 1, protocols)
    {}

  public:
    static const OpaqueArchetypeTypeInfo *create(llvm::Type *type,
                                           ArrayRef<ProtocolEntry> protocols) {
      void *buffer = operator new(sizeof(OpaqueArchetypeTypeInfo)
                                  + protocols.size() * sizeof(ProtocolEntry));
      return ::new (buffer) OpaqueArchetypeTypeInfo(type, protocols);
    }

    void assignWithCopy(IRGenFunction &IGF, Address dest, Address src,
                        SILType T) const override {
      emitAssignWithCopyCall(IGF, T,
                             dest.getAddress(), src.getAddress());
    }

    void assignWithTake(IRGenFunction &IGF, Address dest, Address src,
                        SILType T) const override {
      emitAssignWithTakeCall(IGF, T,
                             dest.getAddress(), src.getAddress());
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src, SILType T) const override {
      emitInitializeWithCopyCall(IGF, T,
                                 dest.getAddress(), src.getAddress());
    }

    void initializeArrayWithCopy(IRGenFunction &IGF,
                                 Address dest, Address src, llvm::Value *count,
                                 SILType T) const override {
      emitInitializeArrayWithCopyCall(IGF, T,
                                 dest.getAddress(), src.getAddress(), count);
    }

    void initializeWithTake(IRGenFunction &IGF,
                            Address dest, Address src, SILType T) const override {
      emitInitializeWithTakeCall(IGF, T,
                                 dest.getAddress(), src.getAddress());
    }

    void initializeArrayWithTakeFrontToBack(IRGenFunction &IGF,
                                            Address dest, Address src,
                                            llvm::Value *count,
                                            SILType T) const override {
      emitInitializeArrayWithTakeFrontToBackCall(IGF, T,
                                    dest.getAddress(), src.getAddress(), count);
    }

    void initializeArrayWithTakeBackToFront(IRGenFunction &IGF,
                                            Address dest, Address src,
                                            llvm::Value *count,
                                            SILType T) const override {
      emitInitializeArrayWithTakeBackToFrontCall(IGF, T,
                                    dest.getAddress(), src.getAddress(), count);
    }

    void destroy(IRGenFunction &IGF, Address addr, SILType T) const override {
      emitDestroyCall(IGF, T, addr.getAddress());
    }

    void destroyArray(IRGenFunction &IGF, Address addr, llvm::Value *count,
                      SILType T) const override {
      emitDestroyArrayCall(IGF, T, addr.getAddress(), count);
    }

    std::pair<llvm::Value*,llvm::Value*>
    getSizeAndAlignment(IRGenFunction &IGF, SILType T) const {
      auto size = emitLoadOfSize(IGF, T);
      auto align = emitLoadOfAlignmentMask(IGF, T);
      return std::make_pair(size, align);
    }

    llvm::Value *getSize(IRGenFunction &IGF, SILType T) const override {
      return emitLoadOfSize(IGF, T);
    }

    llvm::Value *getAlignment(IRGenFunction &IGF, SILType T) const {
      return emitLoadOfAlignmentMask(IGF, T);
    }

    llvm::Value *getStride(IRGenFunction &IGF, SILType T) const override {
      return emitLoadOfStride(IGF, T);
    }

    llvm::Constant *getStaticSize(IRGenModule &IGM) const override { return nullptr; }
    llvm::Constant *getStaticAlignment(IRGenModule &IGM) const { return nullptr; }
    llvm::Constant *getStaticStride(IRGenModule &IGM) const override { return nullptr; }

    void initializeMetadata(IRGenFunction &IGF,
                            llvm::Value *metadata,
                            llvm::Value *vwtable,
                            SILType T) const override {
      // Archetypes always refer to an existing type. A witness table should
      // never be independently initialized for one.
      llvm_unreachable("initializing value witness table for archetype?!");
    }

    bool mayHaveExtraInhabitants(IRGenModule &IGM) const override {
      return true;
    }
    llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                         Address src,
                                         SILType T) const override {
      return emitGetExtraInhabitantIndexCall(IGF, T, src.getAddress());
    }
    void storeExtraInhabitant(IRGenFunction &IGF,
                              llvm::Value *index,
                              Address dest,
                              SILType T) const override {
      emitStoreExtraInhabitantCall(IGF, T, index, dest.getAddress());
    }
  };

  /// A type implementation for a class archetype, that is, an archetype
  /// bounded by a class protocol constraint. These archetypes can be
  /// represented by a refcounted pointer instead of an opaque value buffer.
  /// We use an unknown-refcounted pointer in order to allow ObjC or Swift
  /// classes to conform to the type variable.
  class ClassArchetypeTypeInfo
    : public HeapTypeInfo<ClassArchetypeTypeInfo>,
      public ArchetypeTypeInfoBase
  {
    ReferenceCounting RefCount;

    ClassArchetypeTypeInfo(llvm::PointerType *storageType,
                           Size size, const SpareBitVector &spareBits,
                           Alignment align,
                           ArrayRef<ProtocolEntry> protocols,
                           ReferenceCounting refCount)
      : HeapTypeInfo(storageType, size, spareBits, align),
        ArchetypeTypeInfoBase(this + 1, protocols),
        RefCount(refCount)
    {}

  public:
    static const ClassArchetypeTypeInfo *create(llvm::PointerType *storageType,
                                           Size size, const SpareBitVector &spareBits,
                                           Alignment align,
                                           ArrayRef<ProtocolEntry> protocols,
                                           ReferenceCounting refCount) {
      void *buffer = operator new(sizeof(ClassArchetypeTypeInfo)
                                    + protocols.size() * sizeof(ProtocolEntry));
      return ::new (buffer)
        ClassArchetypeTypeInfo(storageType, size, spareBits, align,
                               protocols, refCount);
    }

    ReferenceCounting getReferenceCounting() const {
      return RefCount;
    }
  };

  /// Return the ArchetypeTypeInfoBase information from the TypeInfo for any
  /// archetype.
  static const ArchetypeTypeInfoBase &
  getArchetypeInfo(IRGenFunction &IGF, CanArchetypeType t, const TypeInfo &ti) {
    if (t->requiresClass())
      return ti.as<ClassArchetypeTypeInfo>();
    return ti.as<OpaqueArchetypeTypeInfo>();
  }
}

static void setMetadataRef(IRGenFunction &IGF,
                           ArchetypeType *archetype,
                           llvm::Value *metadata) {
  assert(metadata->getType() == IGF.IGM.TypeMetadataPtrTy);
  IGF.setUnscopedLocalTypeData(CanType(archetype),
                               LocalTypeData::forMetatype(),
                               metadata);

  // Create a shadow copy of the metadata in an alloca for the debug info.
  StringRef Name = metadata->getName();
  if (!IGF.IGM.Opts.Optimize) {
    auto Alloca = IGF.createAlloca(metadata->getType(),
                                   IGF.IGM.getPointerAlignment(), Name);
    IGF.Builder.CreateAlignedStore(metadata, Alloca.getAddress(),
                                   IGF.IGM.getPointerAlignment().getValue());
    metadata = Alloca.getAddress();
  }

  // Emit debug info for the metadata.
  if (IGF.IGM.DebugInfo)
    IGF.IGM.DebugInfo->emitTypeMetadata(IGF, metadata, Name);
}

static void setWitnessTable(IRGenFunction &IGF,
                            ArchetypeType *archetype,
                            unsigned protocolIndex,
                            llvm::Value *wtable) {
  assert(wtable->getType() == IGF.IGM.WitnessTablePtrTy);
  assert(protocolIndex < archetype->getConformsTo().size());
  IGF.setUnscopedLocalTypeData(CanType(archetype),
             LocalTypeData::forArchetypeProtocolWitness(protocolIndex), wtable);
}

/// Detail about how an object conforms to a protocol.
class irgen::ConformanceInfo {
  friend class ProtocolInfo;
public:
  virtual ~ConformanceInfo() {}
  virtual llvm::Value *getTable(IRGenFunction &IGF) const = 0;
  /// Try to get this table as a constant pointer.  This might just
  /// not be supportable at all.
  virtual llvm::Constant *tryGetConstantTable(IRGenModule &IGM) const = 0;
};

namespace {

/// Conformance info for a witness table that can be directly generated.
class DirectConformanceInfo : public ConformanceInfo {
  friend class ProtocolInfo;

  const NormalProtocolConformance *RootConformance;
public:
  DirectConformanceInfo(const NormalProtocolConformance *C)
    : RootConformance(C) {}

  llvm::Value *getTable(IRGenFunction &IGF) const override {
    return IGF.IGM.getAddrOfWitnessTable(RootConformance);
  }

  llvm::Constant *tryGetConstantTable(IRGenModule &IGM) const override {
    return IGM.getAddrOfWitnessTable(RootConformance);
  }
};

} //end anonymous namespace

static bool isNeverAllocated(FixedPacking packing) {
  switch (packing) {
  case FixedPacking::OffsetZero: return true;
  case FixedPacking::Allocate: return false;
  case FixedPacking::Dynamic: return false;
  }
  llvm_unreachable("bad FixedPacking value");
}

namespace {
  /// An operation to be peformed for various kinds of packing.
  struct DynamicPackingOperation {
    virtual ~DynamicPackingOperation() = default;

    /// Emit the operation at a concrete packing kind.
    ///
    /// Immediately after this call, there will be an unconditional
    /// branch to the continuation block.
    virtual void emitForPacking(IRGenFunction &IGF,
                                SILType T,
                                const TypeInfo &type,
                                FixedPacking packing) = 0;

    /// Given that we are currently at the beginning of the
    /// continuation block, complete the operation.
    virtual void complete(IRGenFunction &IGF,
                          SILType T,
                          const TypeInfo &type) = 0;
  };

  /// A class for merging a particular kind of value across control flow.
  template <class T> class DynamicPackingPHIMapping;

  /// An implementation of DynamicPackingPHIMapping for a single LLVM value.
  template <> class DynamicPackingPHIMapping<llvm::Value*> {
    llvm::PHINode *PHI = nullptr;
  public:
    void collect(IRGenFunction &IGF, SILType T,
                 const TypeInfo &type, llvm::Value *value) {
      // Add the result to the phi, creating it (unparented) if necessary.
      if (!PHI) PHI = llvm::PHINode::Create(value->getType(), 2,
                                            "dynamic-packing.result");
      PHI->addIncoming(value, IGF.Builder.GetInsertBlock());
    }
    void complete(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      assert(PHI);
      IGF.Builder.Insert(PHI);
    }
    llvm::Value *get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      assert(PHI);
      return PHI;
    }
  };

  /// An implementation of DynamicPackingPHIMapping for Addresses.
  template <> class DynamicPackingPHIMapping<Address>
      : private DynamicPackingPHIMapping<llvm::Value*> {
    typedef DynamicPackingPHIMapping<llvm::Value*> super;
  public:
    void collect(IRGenFunction &IGF, SILType T,
                 const TypeInfo &type, Address value) {
      super::collect(IGF, T, type, value.getAddress());
    }
    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) {
      super::complete(IGF, T, type);
    }
    Address get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      return type.getAddressForPointer(super::get(IGF, T, type));
    }
  };

  /// An implementation of packing operations based around a lambda.
  template <class ResultTy, class FnTy>
  class LambdaDynamicPackingOperation : public DynamicPackingOperation {
    FnTy Fn;
    DynamicPackingPHIMapping<ResultTy> Mapping;
  public:
    explicit LambdaDynamicPackingOperation(FnTy &&fn) : Fn(fn) {}
    void emitForPacking(IRGenFunction &IGF, SILType T, const TypeInfo &type,
                        FixedPacking packing) override {
      Mapping.collect(IGF, T, type, Fn(IGF, T, type, packing));
    }

    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) override {
      Mapping.complete(IGF, T, type);
    }

    ResultTy get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {
      return Mapping.get(IGF, T, type);
    }
  };

  /// A partial specialization for lambda-based packing operations
  /// that return 'void'.
  template <class FnTy>
  class LambdaDynamicPackingOperation<void, FnTy>
      : public DynamicPackingOperation {
    FnTy Fn;
  public:
    explicit LambdaDynamicPackingOperation(FnTy &&fn) : Fn(fn) {}
    void emitForPacking(IRGenFunction &IGF, SILType T, const TypeInfo &type,
                        FixedPacking packing) override {
      Fn(IGF, T, type, packing);
    }
    void complete(IRGenFunction &IGF, SILType T,
                  const TypeInfo &type) override {}
    void get(IRGenFunction &IGF, SILType T, const TypeInfo &type) {}
  };
}

/// Dynamic check for the enabling conditions of different kinds of
/// packing into a fixed-size buffer, and perform an operation at each
/// of them.
static void emitDynamicPackingOperation(IRGenFunction &IGF,
                                        SILType T,
                                        const TypeInfo &type,
                                        DynamicPackingOperation &operation) {
  auto indirectBB = IGF.createBasicBlock("dynamic-packing.indirect");
  auto directBB = IGF.createBasicBlock("dynamic-packing.direct");
  auto contBB = IGF.createBasicBlock("dynamic-packing.cont");

  // Branch.
  auto isInline = type.isDynamicallyPackedInline(IGF, T);
  IGF.Builder.CreateCondBr(isInline, directBB, indirectBB);

  // Emit the indirect path.
  IGF.Builder.emitBlock(indirectBB);
  operation.emitForPacking(IGF, T, type, FixedPacking::Allocate);
  IGF.Builder.CreateBr(contBB);

  // Emit the direct path.
  IGF.Builder.emitBlock(directBB);
  operation.emitForPacking(IGF, T, type, FixedPacking::OffsetZero);
  IGF.Builder.CreateBr(contBB);

  // Enter the continuation block and add the PHI if required.
  IGF.Builder.emitBlock(contBB);
  operation.complete(IGF, T, type);
}

/// A helper function for creating a lambda-based DynamicPackingOperation.
template <class ResultTy, class FnTy>
LambdaDynamicPackingOperation<ResultTy, FnTy>
makeLambdaDynamicPackingOperation(FnTy &&fn) {
  return LambdaDynamicPackingOperation<ResultTy, FnTy>(std::move(fn));
}

/// Perform an operation on a type that requires dynamic packing.
template <class ResultTy, class... ArgTys>
static ResultTy emitForDynamicPacking(IRGenFunction &IGF,
                                      ResultTy (*fn)(IRGenFunction &IGF,
                                                     SILType T,
                                                     const TypeInfo &type,
                                                     FixedPacking packing,
                                                     ArgTys... args),
                                      SILType T,
                                      const TypeInfo &type,
                        // using enable_if to block template argument deduction
                        typename std::enable_if<true,ArgTys>::type... args) {
  auto operation = makeLambdaDynamicPackingOperation<ResultTy>(
    [&](IRGenFunction &IGF, SILType T, const TypeInfo &type, FixedPacking packing) {
      return fn(IGF, T, type, packing, args...);
    });
  emitDynamicPackingOperation(IGF, T, type, operation);
  return operation.get(IGF, T, type);
}

/// Emit a 'projectBuffer' operation.  Always returns a T*.
static Address emitProjectBuffer(IRGenFunction &IGF,
                                 SILType T,
                                 const TypeInfo &type,
                                 FixedPacking packing,
                                 Address buffer) {
  llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot = IGF.Builder.CreateBitCast(buffer, resultTy->getPointerTo(),
                                             "storage-slot");
    llvm::Value *address = IGF.Builder.CreateLoad(slot);
    return type.getAddressForPointer(address);
  }

  case FixedPacking::OffsetZero: {
    return IGF.Builder.CreateBitCast(buffer, resultTy, "object");
  }

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitProjectBuffer, T, type, buffer);

  }
  llvm_unreachable("bad packing!");

}
namespace swift { namespace irgen { using ::emitProjectBuffer; } }

/// Project to the address of a value in a value buffer.
Address irgen::emitProjectBuffer(IRGenFunction &IGF, SILType valueType,
                                 Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  return ::emitProjectBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit an 'allocateBuffer' operation.  Always returns a T*.
static Address emitAllocateBuffer(IRGenFunction &IGF,
                                  SILType T,
                                  const TypeInfo &type,
                                  FixedPacking packing,
                                  Address buffer) {
  switch (packing) {
  case FixedPacking::Allocate: {
    auto sizeAndAlign = type.getSizeAndAlignmentMask(IGF, T);
    llvm::Value *addr =
      IGF.emitAllocRawCall(sizeAndAlign.first, sizeAndAlign.second);
    buffer = IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
    IGF.Builder.CreateStore(addr, buffer);

    addr = IGF.Builder.CreateBitCast(addr,
                                     type.getStorageType()->getPointerTo());
    return type.getAddressForPointer(addr);
  }

  case FixedPacking::OffsetZero:
    return emitProjectBuffer(IGF, T, type, packing, buffer);

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitAllocateBuffer, T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}
namespace swift { namespace irgen { using ::emitAllocateBuffer; } }

/// Allocate space for a value in a value buffer.
Address irgen::emitAllocateBuffer(IRGenFunction &IGF, SILType valueType,
                                  Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  return emitAllocateBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit a 'deallocateBuffer' operation.
static void emitDeallocateBuffer(IRGenFunction &IGF,
                                 SILType T,
                                 const TypeInfo &type,
                                 FixedPacking packing,
                                 Address buffer) {
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot =
      IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrPtrTy);
    llvm::Value *addr = IGF.Builder.CreateLoad(slot, "storage");
    auto sizeAndAlignMask = type.getSizeAndAlignmentMask(IGF, T);
    IGF.emitDeallocRawCall(addr, sizeAndAlignMask.first,
                           sizeAndAlignMask.second);
    return;
  }

  case FixedPacking::OffsetZero:
    return;

  case FixedPacking::Dynamic:
    return emitForDynamicPacking(IGF, &emitDeallocateBuffer, T, type, buffer);
  }
  llvm_unreachable("bad packing!");
}
namespace swift { namespace irgen { using ::emitDeallocateBuffer; } }

/// Deallocate space for a value in a value buffer.
void irgen::emitDeallocateBuffer(IRGenFunction &IGF, SILType valueType,
                                 Address buffer) {
  const TypeInfo &valueTI = IGF.getTypeInfo(valueType);
  FixedPacking packing = valueTI.getFixedPacking(IGF.IGM);
  emitDeallocateBuffer(IGF, valueType, valueTI, packing, buffer);
}

/// Emit a 'destroyBuffer' operation.
static void emitDestroyBuffer(IRGenFunction &IGF,
                              SILType T,
                              const TypeInfo &type,
                              FixedPacking packing,
                              Address buffer) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF, &emitDestroyBuffer, T, type, buffer);

  Address object = emitProjectBuffer(IGF, T, type, packing, buffer);
  type.destroy(IGF, object, T);
  emitDeallocateBuffer(IGF, T, type, packing, buffer);
}

/// Emit an 'initializeWithCopy' operation.
static void emitInitializeWithCopy(IRGenFunction &IGF,
                                   SILType T,
                                   const TypeInfo &type,
                                   Address dest, Address src) {
  type.initializeWithCopy(IGF, dest, src, T);
}

/// Emit an 'initializeWithTake' operation.
static void emitInitializeWithTake(IRGenFunction &IGF,
                                   SILType T,
                                   const TypeInfo &type,
                                   Address dest, Address src) {
  type.initializeWithTake(IGF, dest, src, T);
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                                    SILType T,
                                                    const TypeInfo &type,
                                                    FixedPacking packing,
                                                    Address dest,
                                                    Address src) {
  // Special-case dynamic packing in order to thread the jumps.
  if (packing == FixedPacking::Dynamic)
    return emitForDynamicPacking(IGF, &emitInitializeBufferWithCopyOfBuffer,
                                 T, type, dest, src);

  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  Address srcObject = emitProjectBuffer(IGF, T, type, packing, src);
  emitInitializeWithCopy(IGF, T, type, destObject, srcObject);
  return destObject;
}

/// Emit an 'initializeBufferWithTakeOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTakeOfBuffer(IRGenFunction &IGF,
                                                    SILType T,
                                                    const TypeInfo &type,
                                                    FixedPacking packing,
                                                    Address dest,
                                                    Address src) {
  switch (packing) {

  case FixedPacking::Dynamic:
    // Special-case dynamic packing in order to thread the jumps.
    return emitForDynamicPacking(IGF, &emitInitializeBufferWithTakeOfBuffer,
                                 T, type, dest, src);

  case FixedPacking::OffsetZero: {
    // Both of these allocations/projections should be no-ops.
    Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
    Address srcObject = emitProjectBuffer(IGF, T, type, packing, src);
    emitInitializeWithTake(IGF, T, type, destObject, srcObject);
    return destObject;
  }

  case FixedPacking::Allocate: {
    // Just copy the out-of-line storage pointers.
    llvm::Type *ptrTy = type.getStorageType()->getPointerTo()->getPointerTo();
    src = IGF.Builder.CreateBitCast(src, ptrTy);
    llvm::Value *addr = IGF.Builder.CreateLoad(src);
    dest = IGF.Builder.CreateBitCast(dest, ptrTy);
    IGF.Builder.CreateStore(addr, dest);
    return type.getAddressForPointer(addr);
  }
  }
  llvm_unreachable("bad fixed packing");
}

/// Emit an 'initializeBufferWithCopy' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopy(IRGenFunction &IGF,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing,
                                            Address dest,
                                            Address srcObject) {
  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  emitInitializeWithCopy(IGF, T, type, destObject, srcObject);
  return destObject;
}

/// Emit an 'initializeBufferWithTake' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTake(IRGenFunction &IGF,
                                            SILType T,
                                            const TypeInfo &type,
                                            FixedPacking packing,
                                            Address dest,
                                            Address srcObject) {
  Address destObject = emitAllocateBuffer(IGF, T, type, packing, dest);
  emitInitializeWithTake(IGF, T, type, destObject, srcObject);
  return destObject;
}

static llvm::Value *getArg(llvm::Function::arg_iterator &it,
                           StringRef name) {
  llvm::Value *arg = it++;
  arg->setName(name);
  return arg;
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAs(IRGenFunction &IGF,
                        llvm::Function::arg_iterator &it,
                        const TypeInfo &type,
                        StringRef name) {
  llvm::Value *arg = getArg(it, name);
  llvm::Value *result =
    IGF.Builder.CreateBitCast(arg, type.getStorageType()->getPointerTo());
  return type.getAddressForPointer(result);
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAsBuffer(IRGenFunction &IGF,
                              llvm::Function::arg_iterator &it,
                              StringRef name) {
  llvm::Value *arg = getArg(it, name);
  return Address(arg, getFixedBufferAlignment(IGF.IGM));
}

/// Get the next argument and use it as the 'self' type metadata.
static void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF,
                                          llvm::Function::arg_iterator &it,
                                          CanType abstractType);

/// Build a value witness that initializes an array front-to-back.
static void emitInitializeArrayFrontToBackWitness(IRGenFunction &IGF,
                                           llvm::Function::arg_iterator argv,
                                           CanType abstractType,
                                           SILType concreteType,
                                           const TypeInfo &type,
                                           IsTake_t take) {
  Address destArray = getArgAs(IGF, argv, type, "dest");
  Address srcArray = getArgAs(IGF, argv, type, "src");
  llvm::Value *count = getArg(argv, "count");
  getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

  emitInitializeArrayFrontToBack(IGF, type, destArray, srcArray, count,
                                 concreteType, take);

  destArray = IGF.Builder.CreateBitCast(destArray, IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateRet(destArray.getAddress());
}

/// Build a value witness that initializes an array back-to-front.
static void emitInitializeArrayBackToFrontWitness(IRGenFunction &IGF,
                                           llvm::Function::arg_iterator argv,
                                           CanType abstractType,
                                           SILType concreteType,
                                           const TypeInfo &type,
                                           IsTake_t take) {
  Address destArray = getArgAs(IGF, argv, type, "dest");
  Address srcArray = getArgAs(IGF, argv, type, "src");
  llvm::Value *count = getArg(argv, "count");
  getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

  emitInitializeArrayBackToFront(IGF, type, destArray, srcArray, count,
                                 concreteType, take);

  destArray = IGF.Builder.CreateBitCast(destArray, IGF.IGM.OpaquePtrTy);
  IGF.Builder.CreateRet(destArray.getAddress());
}

/// Build a specific value-witness function.
static void buildValueWitnessFunction(IRGenModule &IGM,
                                      llvm::Function *fn,
                                      ValueWitness index,
                                      FixedPacking packing,
                                      CanType abstractType,
                                      SILType concreteType,
                                      const TypeInfo &type) {
  assert(isValueWitnessFunction(index));

  IRGenFunction IGF(IGM, fn);
  if (IGM.DebugInfo)
    IGM.DebugInfo->emitArtificialFunction(IGF, fn);

  auto argv = fn->arg_begin();
  switch (index) {
  case ValueWitness::AllocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    Address result = emitAllocateBuffer(IGF, concreteType, type, packing, buffer);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::AssignWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.assignWithCopy(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::AssignWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.assignWithTake(IGF, dest, src, concreteType);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::DeallocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    emitDeallocateBuffer(IGF, concreteType, type, packing, buffer);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::Destroy: {
    Address object = getArgAs(IGF, argv, type, "object");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    type.destroy(IGF, object, concreteType);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::DestroyArray: {
    Address array = getArgAs(IGF, argv, type, "array");
    llvm::Value *count = getArg(argv, "count");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    auto entry = IGF.Builder.GetInsertBlock();
    auto iter = IGF.createBasicBlock("iter");
    auto loop = IGF.createBasicBlock("loop");
    auto exit = IGF.createBasicBlock("exit");
    IGF.Builder.CreateBr(iter);
    IGF.Builder.emitBlock(iter);

    auto counter = IGF.Builder.CreatePHI(IGM.SizeTy, 2);
    counter->addIncoming(count, entry);
    auto elementVal = IGF.Builder.CreatePHI(array.getType(), 2);
    elementVal->addIncoming(array.getAddress(), entry);
    Address element(elementVal, array.getAlignment());

    auto done = IGF.Builder.CreateICmpEQ(counter,
                                         llvm::ConstantInt::get(IGM.SizeTy, 0));
    IGF.Builder.CreateCondBr(done, exit, loop);

    IGF.Builder.emitBlock(loop);
    type.destroy(IGF, element, concreteType);
    auto nextCounter = IGF.Builder.CreateSub(counter,
                                     llvm::ConstantInt::get(IGM.SizeTy, 1));
    auto nextElement = type.indexArray(IGF, element,
                                       llvm::ConstantInt::get(IGM.SizeTy, 1),
                                       concreteType);
    auto loopEnd = IGF.Builder.GetInsertBlock();
    counter->addIncoming(nextCounter, loopEnd);
    elementVal->addIncoming(nextElement.getAddress(), loopEnd);
    IGF.Builder.CreateBr(iter);

    IGF.Builder.emitBlock(exit);
    IGF.Builder.CreateRetVoid();

    return;
  }

  case ValueWitness::DestroyBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);
    emitDestroyBuffer(IGF, concreteType, type, packing, buffer);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopyOfBuffer(IGF, concreteType,
                                           type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTakeOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTakeOfBuffer(IGF, concreteType,
                                           type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithCopy(IGF, concreteType, type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result =
      emitInitializeBufferWithTake(IGF, concreteType, type, packing, dest, src);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    emitInitializeWithCopy(IGF, concreteType, type, dest, src);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeArrayWithCopy: {
    emitInitializeArrayFrontToBackWitness(IGF, argv, abstractType, concreteType,
                                          type, IsNotTake);
    return;
  }

  case ValueWitness::InitializeWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    emitInitializeWithTake(IGF, concreteType, type, dest, src);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeArrayWithTakeFrontToBack: {
    emitInitializeArrayFrontToBackWitness(IGF, argv, abstractType, concreteType,
                                          type, IsTake);
    return;
  }

  case ValueWitness::InitializeArrayWithTakeBackToFront: {
    emitInitializeArrayBackToFrontWitness(IGF, argv, abstractType, concreteType,
                                          type, IsTake);
    return;
  }

  case ValueWitness::ProjectBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    Address result = emitProjectBuffer(IGF, concreteType, type, packing, buffer);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.OpaquePtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::StoreExtraInhabitant: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    llvm::Value *index = getArg(argv, "index");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    type.storeExtraInhabitant(IGF, index, dest, concreteType);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::GetExtraInhabitantIndex: {
    Address src = getArgAs(IGF, argv, type, "src");
    getArgAsLocalSelfTypeMetadata(IGF, argv, abstractType);

    llvm::Value *idx = type.getExtraInhabitantIndex(IGF, src, concreteType);
    IGF.Builder.CreateRet(idx);
    return;
  }

  // TODO
  case ValueWitness::GetEnumTag:
  case ValueWitness::InplaceProjectEnumData: {
    IGF.Builder.CreateUnreachable();
    return;
  }

  case ValueWitness::Size:
  case ValueWitness::Flags:
  case ValueWitness::Stride:
  case ValueWitness::ExtraInhabitantFlags:
    llvm_unreachable("these value witnesses aren't functions");
  }
  llvm_unreachable("bad value witness kind!");
}

static llvm::Constant *asOpaquePtr(IRGenModule &IGM, llvm::Constant *in) {
  return llvm::ConstantExpr::getBitCast(in, IGM.Int8PtrTy);
}

/// Should we be defining the given helper function?
static llvm::Function *shouldDefineHelper(IRGenModule &IGM,
                                          llvm::Constant *fn) {
  llvm::Function *def = dyn_cast<llvm::Function>(fn);
  if (!def) return nullptr;
  if (!def->empty()) return nullptr;

  def->setLinkage(llvm::Function::LinkOnceODRLinkage);
  def->setVisibility(llvm::Function::HiddenVisibility);
  def->setDoesNotThrow();
  def->setCallingConv(IGM.RuntimeCC);
  return def;
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
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);

  // __swift_assign_existentials_N is the well-known function for
  // assigning existential types with N witness tables.
  llvm::SmallString<40> fnName;
  llvm::raw_svector_ostream(fnName)
    << "__swift_assign_existentials_" << layout.getNumTables();
  llvm::Constant *fn = IGM.Module.getOrInsertFunction(fnName, fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);

    auto it = def->arg_begin();
    Address dest(it++, getFixedBufferAlignment(IGM));
    Address src(it++, getFixedBufferAlignment(IGM));

    // If doing a self-assignment, we're done.
    llvm::BasicBlock *doneBB = IGF.createBasicBlock("done");
    llvm::BasicBlock *contBB = IGF.createBasicBlock("cont");
    llvm::Value *isSelfAssign =
      IGF.Builder.CreateICmpEQ(dest.getAddress(), src.getAddress(),
                               "isSelfAssign");
    IGF.Builder.CreateCondBr(isSelfAssign, doneBB, contBB);

    // Project down to the buffers.
    IGF.Builder.emitBlock(contBB);
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

    { // (scope to avoid contaminating other branches with these values)

      // If so, do a direct assignment.
      IGF.Builder.emitBlock(matchBB);

      llvm::Value *destObject =
        emitProjectBufferCall(IGF, destMetadata, destBuffer);
      llvm::Value *srcObject =
        emitProjectBufferCall(IGF, destMetadata, srcBuffer);
      emitAssignWithCopyCall(IGF, destMetadata, destObject, srcObject);
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
                                             destBuffer, srcBuffer);
    IGF.Builder.CreateBr(doneBB);

    // All done.
    IGF.Builder.emitBlock(doneBB);
    IGF.Builder.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes two pointer arguments and returns
/// void immediately.
static llvm::Constant *getNoOpVoidFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_void_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    IRBuilder B(IGM.getLLVMContext());
    B.SetInsertPoint(entry);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(*IGM.SILMod, B, def);
    B.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes two pointer arguments and returns
/// the first one immediately.
static llvm::Constant *getReturnSelfFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_self_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    IRBuilder B(IGM.getLLVMContext());
    B.SetInsertPoint(entry);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(*IGM.SILMod, B, def);
    B.CreateRet(def->arg_begin());
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithCopy on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitRetainCall(newValue);
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitRelease(oldValue);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithTake on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithTakeStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithTake_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);

    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    llvm::Value *oldValue = IGF.Builder.CreateLoad(dest, "old");
    IGF.Builder.CreateStore(newValue, dest);
    IGF.emitRelease(oldValue);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining initWithCopy on the first two: it loads a pointer from
/// the second, retains it, and stores that in the first.
static llvm::Constant *getInitWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_initWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    IGF.emitRetainCall(newValue);
    IGF.Builder.CreateStore(newValue, dest);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes two pointer arguments, loads a
/// pointer from the first, and calls swift_release on it immediately.
static llvm::Constant *getDestroyStrongFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.WitnessTablePtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_destroy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);
    Address arg(def->arg_begin(), IGM.getPointerAlignment());
    IGF.emitRelease(IGF.Builder.CreateLoad(arg));
    IGF.Builder.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes two pointer arguments, memcpys
/// from the second to the first, and returns the first argument.
static llvm::Constant *getMemCpyFunction(IRGenModule &IGM,
                                         const TypeInfo &objectTI) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.TypeMetadataPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);

  // If we don't have a fixed type, use the standard copy-opaque-POD
  // routine.  It's not quite clear how in practice we'll be able to
  // conclude that something is known-POD without knowing its size,
  // but it's (1) conceivable and (2) needed as a general export anyway.
  auto *fixedTI = dyn_cast<FixedTypeInfo>(&objectTI);
  if (!fixedTI) return IGM.getCopyPODFn();

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_memcpy";
    nameStream << fixedTI->getFixedSize().getValue();
    nameStream << '_';
    nameStream << fixedTI->getFixedAlignment().getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);

    auto it = def->arg_begin();
    Address dest(it++, fixedTI->getFixedAlignment());
    Address src(it++, fixedTI->getFixedAlignment());
    IGF.emitMemCpy(dest, src, fixedTI->getFixedSize());
    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes two buffer arguments, copies
/// a pointer from the second to the first, and returns the pointer.
static llvm::Constant *getCopyOutOfLinePointerFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrPtrTy, IGM.Int8PtrPtrTy,
                           IGM.TypeMetadataPtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);

  StringRef name = "__swift_copy_outline_pointer";
  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);

    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());
    auto ptr = IGF.Builder.CreateLoad(src);
    IGF.Builder.CreateStore(ptr, dest);
    IGF.Builder.CreateRet(ptr);
  }
  return fn;
}

namespace {
  enum class MemMoveOrCpy { MemMove, MemCpy };
}

/// Return a function which takes two pointer arguments and a count, memmoves
/// or memcpys from the second to the first, and returns the first argument.
static llvm::Constant *getMemOpArrayFunction(IRGenModule &IGM,
                                             const TypeInfo &objectTI,
                                             MemMoveOrCpy kind) {
  llvm::Type *argTys[] = {
    IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.SizeTy,
    IGM.TypeMetadataPtrTy
  };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);

  // TODO: Add a copyPODArray runtime entry point for bitwise-takable but non-
  // fixed-size types. Currently only fixed-layout types should be known
  // bitwise-takable.
  auto &fixedTI = cast<FixedTypeInfo>(objectTI);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    switch (kind) {
    case MemMoveOrCpy::MemCpy:
      nameStream << "__swift_memcpy_array";
      break;
    case MemMoveOrCpy::MemMove:
      nameStream << "__swift_memmove_array";
      break;
    }
    nameStream << fixedTI.getFixedStride().getValue();
    nameStream << '_';
    nameStream << fixedTI.getFixedAlignment().getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(IGM, fn)) {
    IRGenFunction IGF(IGM, def);
    if (IGM.DebugInfo)
      IGM.DebugInfo->emitArtificialFunction(IGF, def);

    auto it = def->arg_begin();
    Address dest(it++, fixedTI.getFixedAlignment());
    Address src(it++, fixedTI.getFixedAlignment());
    llvm::Value *count = it++;
    llvm::Value *stride
      = llvm::ConstantInt::get(IGM.SizeTy, fixedTI.getFixedStride().getValue());
    llvm::Value *totalCount = IGF.Builder.CreateNUWMul(count, stride);
    switch (kind) {
    case MemMoveOrCpy::MemMove:
      IGF.Builder.CreateMemMove(dest.getAddress(), src.getAddress(), totalCount,
                                fixedTI.getFixedAlignment().getValue());
      break;
    case MemMoveOrCpy::MemCpy:
      IGF.Builder.CreateMemCpy(dest.getAddress(), src.getAddress(), totalCount,
                               fixedTI.getFixedAlignment().getValue());
      break;
    }
    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

static llvm::Constant *getMemMoveArrayFunction(IRGenModule &IGM,
                                               const TypeInfo &objectTI) {
  return getMemOpArrayFunction(IGM, objectTI, MemMoveOrCpy::MemMove);
}
static llvm::Constant *getMemCpyArrayFunction(IRGenModule &IGM,
                                               const TypeInfo &objectTI) {
  return getMemOpArrayFunction(IGM, objectTI, MemMoveOrCpy::MemCpy);
}

/// Find a witness to the fact that a type is a value type.
/// Always returns an i8*.
static llvm::Constant *getValueWitness(IRGenModule &IGM,
                                       ValueWitness index,
                                       FixedPacking packing,
                                       CanType abstractType,
                                       SILType concreteType,
                                       const TypeInfo &concreteTI) {
  // Try to use a standard function.
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    if (isNeverAllocated(packing))
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    goto standard;

  case ValueWitness::DestroyBuffer:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      if (isNeverAllocated(packing))
        return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      assert(isNeverAllocated(packing));
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::Destroy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::DestroyArray:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    }
    // TODO: A standard "destroy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithCopy:
    if (packing == FixedPacking::OffsetZero) {
      if (concreteTI.isPOD(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
      } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
      }
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTakeOfBuffer:
    if (packing == FixedPacking::Allocate) {
      return asOpaquePtr(IGM, getCopyOutOfLinePointerFunction(IGM));
    } else if (packing == FixedPacking::OffsetZero &&
               concreteTI.isBitwiseTakable(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Local)
        && packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    goto standard;

  case ValueWitness::InitializeWithTake:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeFrontToBack:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithTakeBackToFront:
    if (concreteTI.isBitwiseTakable(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemMoveArrayFunction(IGM, concreteTI));
    }
    goto standard;

  case ValueWitness::AssignWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AssignWithTake:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithTakeStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleSwiftRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeArrayWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyArrayFunction(IGM, concreteTI));
    }
    // TODO: A standard "copy strong array" entrypoint for arrays of single
    // refcounted pointer types.
    goto standard;

  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer:
    if (packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getReturnSelfFunction(IGM));
    goto standard;

  case ValueWitness::Size: {
    if (auto value = concreteTI.getStaticSize(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::Flags: {
    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      uint64_t flags = fixedTI->getFixedAlignment().getValue() - 1;
      if (!fixedTI->isPOD(ResilienceScope::Local))
        flags |= ValueWitnessFlags::IsNonPOD;
      assert(packing == FixedPacking::OffsetZero ||
             packing == FixedPacking::Allocate);
      if (packing != FixedPacking::OffsetZero)
        flags |= ValueWitnessFlags::IsNonInline;

      if (fixedTI->getFixedExtraInhabitantCount(IGM) > 0)
        flags |= ValueWitnessFlags::Enum_HasExtraInhabitants;

      if (!fixedTI->isBitwiseTakable(ResilienceScope::Local))
        flags |= ValueWitnessFlags::IsNonBitwiseTakable;

      auto value = IGM.getSize(Size(flags));
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);
    }

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::Stride: {
    if (auto value = concreteTI.getStaticStride(IGM))
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);

    // Just fill in null here if the type can't be statically laid out.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  case ValueWitness::StoreExtraInhabitant:
  case ValueWitness::GetExtraInhabitantIndex: {
    assert(concreteTI.mayHaveExtraInhabitants(IGM));

    goto standard;
  }

  case ValueWitness::ExtraInhabitantFlags: {
    assert(concreteTI.mayHaveExtraInhabitants(IGM));

    // If we locally know that the type has fixed layout, we can emit
    // meaningful flags for it.
    if (auto *fixedTI = dyn_cast<FixedTypeInfo>(&concreteTI)) {
      uint64_t numExtraInhabitants = fixedTI->getFixedExtraInhabitantCount(IGM);
      assert(numExtraInhabitants <= ExtraInhabitantFlags::NumExtraInhabitantsMask);
      auto value = IGM.getSize(Size(numExtraInhabitants));
      return llvm::ConstantExpr::getIntToPtr(value, IGM.Int8PtrTy);
    }

    // Otherwise, just fill in null here if the type can't be statically
    // queried for extra inhabitants.
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }

  /// TODO:
  case ValueWitness::GetEnumTag:
  case ValueWitness::InplaceProjectEnumData:
    return llvm::ConstantPointerNull::get(IGM.Int8PtrTy);
  }
  llvm_unreachable("bad value witness kind");

 standard:
  llvm::Function *fn =
    IGM.getAddrOfValueWitness(abstractType, index, ForDefinition);
  if (fn->empty())
    buildValueWitnessFunction(IGM, fn, index, packing, abstractType,
                              concreteType, concreteTI);
  return asOpaquePtr(IGM, fn);
}

namespace {
  /// A class which lays out a specific conformance to a protocol.
  class WitnessTableBuilder : public SILWitnessVisitor<WitnessTableBuilder> {
    IRGenModule &IGM;
    SmallVectorImpl<llvm::Constant*> &Table;
    CanType ConcreteType;
    GenericParamList *ConcreteGenerics = nullptr;
    const TypeInfo &ConcreteTI;
    const ProtocolConformance &Conformance;
    ArrayRef<Substitution> Substitutions;
    ArrayRef<SILWitnessTable::Entry> SILEntries;
#ifndef NDEBUG
    const ProtocolInfo &PI;
#endif

    void computeSubstitutionsForType() {
      // FIXME: This is a bit of a hack; the AST doesn't directly encode
      // substitutions for the conformance of a generic type to a
      // protocol, so we have to dig them out.
      Type ty = ConcreteType;
      while (ty) {
        if (auto nomTy = ty->getAs<NominalType>())
          ty = nomTy->getParent();
        else
          break;
      }
      if (ty) {
        if (auto boundTy = ty->getAs<BoundGenericType>()) {
          ConcreteGenerics = boundTy->getDecl()->getGenericParams();
          Substitutions = boundTy->getSubstitutions(/*FIXME:*/nullptr, nullptr);
        } else {
          assert(!ty || !ty->isSpecialized());
        }
      }
    }

  public:
    WitnessTableBuilder(IRGenModule &IGM,
                        SmallVectorImpl<llvm::Constant*> &table,
                        SILWitnessTable *SILWT)
      : IGM(IGM), Table(table),
        ConcreteType(SILWT->getConformance()->getType()->getCanonicalType()),
        ConcreteTI(
               IGM.getTypeInfoForUnlowered(SILWT->getConformance()->getType())),
        Conformance(*SILWT->getConformance()),
        SILEntries(SILWT->getEntries())
#ifndef NDEBUG
        , PI(IGM.getProtocolInfo(SILWT->getConformance()->getProtocol()))
#endif
    {
      computeSubstitutionsForType();
    }

    /// A base protocol is witnessed by a pointer to the conformance
    /// of this type to that protocol.
    void addOutOfLineBaseProtocol(ProtocolDecl *baseProto) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::BaseProtocol
             && "sil witness table does not match protocol");
      assert(entry.getBaseProtocolWitness().Requirement == baseProto
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(baseProto);
      assert(piEntry.getOutOfLineBaseIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif
      
      SILEntries = SILEntries.slice(1);

      // TODO: Use the witness entry instead of falling through here.

      // Look for a protocol type info.
      const ProtocolInfo &basePI = IGM.getProtocolInfo(baseProto);
      const ProtocolConformance *astConf
        = Conformance.getInheritedConformance(baseProto);
      const ConformanceInfo &conf =
        basePI.getConformance(IGM, ConcreteType, ConcreteTI,
                              baseProto, *astConf);

      llvm::Constant *baseWitness = conf.tryGetConstantTable(IGM);
      assert(baseWitness && "couldn't get a constant table!");
      Table.push_back(asOpaquePtr(IGM, baseWitness));
    }

    void addMethodFromSILWitnessTable(AbstractFunctionDecl *iface) {
      auto &entry = SILEntries.front();
      SILEntries = SILEntries.slice(1);

      // Handle missing optional requirements.
      if (entry.getKind() == SILWitnessTable::MissingOptional) {
        Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
        return;
      }

#ifndef NDEBUG
      assert(entry.getKind() == SILWitnessTable::Method
             && "sil witness table does not match protocol");
      assert(entry.getMethodWitness().Requirement.getDecl() == iface
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(iface);
      assert(piEntry.getFunctionIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILFunction *Func = entry.getMethodWitness().Witness;
      llvm::Constant *witness = nullptr;
      if (Func) {
        witness = IGM.getAddrOfSILFunction(Func, NotForDefinition);
        witness = llvm::ConstantExpr::getBitCast(witness, IGM.Int8PtrTy);
      } else {
        // The method is removed by dead method elimination.
        // It should be never called. We add a pointer to an error function.
        witness = llvm::ConstantExpr::getBitCast(IGM.getDeadMethodErrorFn(),
                                                 IGM.Int8PtrTy);
      }
      Table.push_back(witness);
      return;
    }

    void addMethod(FuncDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addConstructor(ConstructorDecl *iface) {
      return addMethodFromSILWitnessTable(iface);
    }

    void addAssociatedType(AssociatedTypeDecl *ty,
                           ArrayRef<ProtocolDecl *> protos) {
#ifndef NDEBUG
      auto &entry = SILEntries.front();
      assert(entry.getKind() == SILWitnessTable::AssociatedType
             && "sil witness table does not match protocol");
      assert(entry.getAssociatedTypeWitness().Requirement == ty
             && "sil witness table does not match protocol");
      auto piEntry = PI.getWitnessEntry(ty);
      assert(piEntry.getAssociatedTypeIndex().getValue() == Table.size()
             && "offset doesn't match ProtocolInfo layout");
#endif

      SILEntries = SILEntries.slice(1);

      // FIXME: Use info from SILWitnessTable instead of falling through.

      // Determine whether the associated type has static metadata. If it
      // doesn't, then this witness table is a template that requires runtime
      // instantiation.

      // FIXME: Add static type metadata.
      Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));

      // FIXME: Add static witness tables for type conformances.
      for (auto protocol : protos) {
        if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
          continue;

        auto &entry = SILEntries.front();
        (void)entry;
        assert(entry.getKind() == SILWitnessTable::AssociatedTypeProtocol
               && "sil witness table does not match protocol");
        assert(entry.getAssociatedTypeProtocolWitness().Requirement == ty
               && "sil witness table does not match protocol");
        assert(entry.getAssociatedTypeProtocolWitness().Protocol == protocol
               && "sil witness table does not match protocol");

        SILEntries = SILEntries.slice(1);

        // FIXME: Use info from SILWitnessTable instead of falling through.
        // FIXME: Add static witness table reference.
        Table.push_back(llvm::ConstantPointerNull::get(IGM.Int8PtrTy));
      }
    }
  };
}

/// Collect the value witnesses for a particular type.
static void addValueWitnesses(IRGenModule &IGM, FixedPacking packing,
                              CanType abstractType,
                              SILType concreteType, const TypeInfo &concreteTI,
                              SmallVectorImpl<llvm::Constant*> &table) {
  for (unsigned i = 0; i != NumRequiredValueWitnesses; ++i) {
    table.push_back(getValueWitness(IGM, ValueWitness(i),
                                    packing, abstractType, concreteType,
                                    concreteTI));
  }
  if (concreteTI.mayHaveExtraInhabitants(IGM)) {
    for (auto i = unsigned(ValueWitness::First_ExtraInhabitantValueWitness);
         i <= unsigned(ValueWitness::Last_ExtraInhabitantValueWitness);
         ++i) {
      table.push_back(getValueWitness(IGM, ValueWitness(i), packing,
                                      abstractType, concreteType, concreteTI));
    }
  }
}

/// True if a type has a generic-parameter-dependent value witness table.
/// Currently, this is true if the size and/or alignment of the type is
/// dependent on its generic parameters.
bool irgen::hasDependentValueWitnessTable(IRGenModule &IGM, CanType ty) {
  if (auto ugt = dyn_cast<UnboundGenericType>(ty))
    ty = ugt->getDecl()->getDeclaredTypeInContext()->getCanonicalType();

  return !IGM.getTypeInfoForUnlowered(ty).isFixedSize();
}

static void addValueWitnessesForAbstractType(IRGenModule &IGM,
                                 CanType abstractType,
                                 SmallVectorImpl<llvm::Constant*> &witnesses) {
  // Instantiate unbound generic types on their context archetypes.
  CanType concreteFormalType = abstractType;
  if (auto ugt = dyn_cast<UnboundGenericType>(abstractType)) {
    concreteFormalType = ugt->getDecl()->getDeclaredTypeInContext()->getCanonicalType();
  }

  auto concreteLoweredType = IGM.SILMod->Types.getLoweredType(concreteFormalType);
  auto &concreteTI = IGM.getTypeInfo(concreteLoweredType);
  FixedPacking packing = concreteTI.getFixedPacking(IGM);

  addValueWitnesses(IGM, packing, abstractType,
                    concreteLoweredType, concreteTI, witnesses);
}

/// Emit a value-witness table for the given type, which is assumed to
/// be non-dependent.
llvm::Constant *irgen::emitValueWitnessTable(IRGenModule &IGM,
                                             CanType abstractType) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  // We shouldn't emit global value witness tables for non-fixed-layout types.
  assert(!hasDependentValueWitnessTable(IGM, abstractType) &&
         "emitting global VWT for dynamic-layout type");

  SmallVector<llvm::Constant*, MaxNumValueWitnesses> witnesses;
  addValueWitnessesForAbstractType(IGM, abstractType, witnesses);

  auto tableTy = llvm::ArrayType::get(IGM.Int8PtrTy, witnesses.size());
  auto table = llvm::ConstantArray::get(tableTy, witnesses);

  auto addr = IGM.getAddrOfValueWitnessTable(abstractType, table->getType());
  auto global = cast<llvm::GlobalVariable>(addr);
  global->setConstant(true);
  global->setInitializer(table);

  return llvm::ConstantExpr::getBitCast(global, IGM.WitnessTablePtrTy);
}

/// Emit the elements of a dependent value witness table template into a
/// vector.
void irgen::emitDependentValueWitnessTablePattern(IRGenModule &IGM,
                                    CanType abstractType,
                                    SmallVectorImpl<llvm::Constant*> &fields) {
  // We shouldn't emit global value witness tables for generic type instances.
  assert(!isa<BoundGenericType>(abstractType) &&
         "emitting VWT for generic instance");

  // We shouldn't emit global value witness tables for fixed-layout types.
  assert(hasDependentValueWitnessTable(IGM, abstractType) &&
         "emitting VWT pattern for fixed-layout type");

  addValueWitnessesForAbstractType(IGM, abstractType, fields);
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &IRGenModule::getProtocolInfo(ProtocolDecl *protocol) {
  return Types.getProtocolInfo(protocol);
}

/// Do a memoized witness-table layout for a protocol.
const ProtocolInfo &TypeConverter::getProtocolInfo(ProtocolDecl *protocol) {
  // Check whether we've already translated this protocol.
  auto it = Protocols.find(protocol);
  if (it != Protocols.end()) return *it->second;

  // If not, lay out the protocol's witness table, if it needs one.
  WitnessTableLayout layout;
  if (Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
    layout.visitProtocolDecl(protocol);

  // Create a ProtocolInfo object from the layout.
  ProtocolInfo *info = ProtocolInfo::create(layout.getNumWitnesses(),
                                            layout.getEntries());
  info->NextConverted = FirstProtocol;
  FirstProtocol = info;

  // Memoize.
  Protocols.insert(std::make_pair(protocol, info));

  // Done.
  return *info;
}

/// Allocate a new ProtocolInfo.
ProtocolInfo *ProtocolInfo::create(unsigned numWitnesses,
                                   ArrayRef<WitnessTableEntry> table) {
  unsigned numEntries = table.size();
  size_t bufferSize =
    sizeof(ProtocolInfo) + numEntries * sizeof(WitnessTableEntry);
  void *buffer = ::operator new(bufferSize);
  return new(buffer) ProtocolInfo(numWitnesses, table);
}

ProtocolInfo::~ProtocolInfo() {
  for (auto &conf : Conformances) {
    delete conf.second;
  }
}

/// Find the conformance information for a protocol.
const ConformanceInfo &
ProtocolInfo::getConformance(IRGenModule &IGM, CanType concreteType,
                             const TypeInfo &concreteTI,
                             ProtocolDecl *protocol,
                             const ProtocolConformance &conformance) const {
  // Check whether we've already cached this.
  auto it = Conformances.find(&conformance);
  if (it != Conformances.end()) return *it->second;

  // Drill down to the root normal conformance.
  auto normalConformance = conformance.getRootNormalConformance();

  // Emit a direct-referencing conformance.
  // FIXME: For some conformances we need to do lazy initialization or runtime
  // instantiation.
  ConformanceInfo *info = new DirectConformanceInfo(normalConformance);
  auto res = Conformances.insert(std::make_pair(&conformance, info));
  return *res.first->second;
}

std::pair<unsigned, llvm::Constant*>
getTypeReferenceForProtocolConformanceRecord(IRGenModule &IGM,
                                             ProtocolConformance *conformance) {
  ProtocolConformanceTypeKind typeKind;
  llvm::Constant *typeRef;

  // TODO: Should use accessor kind for lazy conformances
  ProtocolConformanceReferenceKind conformanceKind
    = ProtocolConformanceReferenceKind::WitnessTable;

  auto conformingType = conformance->getType()->getCanonicalType();
  if (auto bgt = dyn_cast<BoundGenericType>(conformingType)) {
    // Conformances for generics are represented by referencing the metadata
    // pattern for the generic type.
    typeKind = ProtocolConformanceTypeKind::UniqueGenericPattern;
    typeRef = IGM.getAddrOfTypeMetadata(bgt->getDecl()->getDeclaredType()
                                          ->getCanonicalType(),
                                        /* indirect */ false,
                                        /* pattern */ true);
  } else if (auto ct = dyn_cast<ClassType>(conformingType)) {
    auto clas = ct->getDecl();
    if (clas->isForeign()) {
      typeKind = ProtocolConformanceTypeKind::NonuniqueDirectType;
      typeRef = IGM.getAddrOfForeignTypeMetadataCandidate(ct);
    } else {
      // TODO: We should indirectly reference classes. For now directly
      // reference the class object, which is totally wrong for ObjC interop.

      typeKind = ProtocolConformanceTypeKind::UniqueDirectClass;
      typeRef = IGM.getAddrOfTypeMetadata(ct,
                                          /* indirect */ false,
                                          /* pattern */ false);
    }
  } else if (auto nom = conformingType->getNominalOrBoundGenericNominal()) {
    // TODO: Metadata for Clang types should be uniqued like foreign classes.
    if (nom->hasClangNode()) {
      typeKind = ProtocolConformanceTypeKind::NonuniqueDirectType;
      typeRef = IGM.getAddrOfForeignTypeMetadataCandidate(conformingType);
    } else {
      // We can reference the canonical metadata for native value types
      // directly.
      typeKind = ProtocolConformanceTypeKind::UniqueDirectType;
      typeRef = IGM.getAddrOfTypeMetadata(nom->getDeclaredType()
                                            ->getCanonicalType(),
                                          /* indirect */ false,
                                          /* pattern */ false);
    }
  } else {
    // TODO: Universal and/or structural conformances
    llvm_unreachable("unhandled protocol conformance");
  }

  // Cast the type reference to OpaquePtrTy.
  typeRef = llvm::ConstantExpr::getBitCast(typeRef, IGM.OpaquePtrTy);

  auto flags = ProtocolConformanceFlags()
    .withTypeKind(typeKind)
    .withConformanceKind(conformanceKind);

  return {flags.getValue(), typeRef};
}

void IRGenModule::emitSILWitnessTable(SILWitnessTable *wt) {
  // Don't emit a witness table if it is a declaration.
  if (wt->isDeclaration())
    return;
  // Don't emit a witness table that is available externally if we are emitting
  // code for the JIT. We do not do any optimization for the JIT and it has
  // problems with external symbols that get merged with non-external symbols.
  if (Opts.UseJIT && isAvailableExternally(wt->getLinkage()))
    return;

  // Build the witnesses.
  SmallVector<llvm::Constant*, 32> witnesses;
  WitnessTableBuilder(*this, witnesses, wt)
    .visitProtocolDecl(wt->getConformance()->getProtocol());
  
  assert(getProtocolInfo(wt->getConformance()->getProtocol())
           .getNumWitnesses() == witnesses.size()
         && "witness table size doesn't match ProtocolInfo");

  // Produce the initializer value.
  auto tableTy = llvm::ArrayType::get(FunctionPtrTy, witnesses.size());
  auto initializer = llvm::ConstantArray::get(tableTy, witnesses);

  auto global = cast<llvm::GlobalVariable>(
                         getAddrOfWitnessTable(wt->getConformance(), tableTy));
  global->setConstant(true);
  global->setInitializer(initializer);
  global->setAlignment(getWitnessTableAlignment().getValue());

  // Build the conformance record, if it lives in this TU.
  if (isAvailableExternally(wt->getLinkage()))
    return;

  unsigned flags;
  llvm::Constant *typeRef;
  std::tie(flags, typeRef)
    = getTypeReferenceForProtocolConformanceRecord(*this, wt->getConformance());

  llvm::Constant *recordFields[] = {
    // Protocol descriptor
    getAddrOfProtocolDescriptor(wt->getConformance()->getProtocol(),
                                NotForDefinition),
    // Type reference
    typeRef,
    // Witness table
    // TODO: This needs to be a generator function if the witness table requires
    // lazy initialization or instantiation.
    llvm::ConstantExpr::getBitCast(global, OpaquePtrTy),
    // Flags
    llvm::ConstantInt::get(Int32Ty, flags),
  };

  auto record = llvm::ConstantStruct::get(ProtocolConformanceRecordTy,
                                          recordFields);
  addProtocolConformanceRecord(record);

  // TODO: We should record what access mode the witness table requires:
  // direct, lazily initialized, or runtime instantiated template.
}

namespace {

/// Type info for error existentials, currently the only kind of boxed
/// existential.
class ErrorExistentialTypeInfo : public HeapTypeInfo<ErrorExistentialTypeInfo>
{
  ProtocolEntry ErrorProtocolEntry;
public:
  ErrorExistentialTypeInfo(llvm::PointerType *storage,
                           Size size, SpareBitVector spareBits,
                           Alignment align,
                           const ProtocolEntry &errorProtocolEntry)
    : HeapTypeInfo(storage, size, spareBits, align),
      ErrorProtocolEntry(errorProtocolEntry) {}

  ReferenceCounting getReferenceCounting() const {
    // ErrorType uses its own rc entry points.
    return ReferenceCounting::Error;
  }
  
  ArrayRef<ProtocolEntry> getStoredProtocols() const {
    return ErrorProtocolEntry;
  }
};
  
} // end anonymous namespace

static const TypeInfo *createErrorExistentialTypeInfo(IRGenModule &IGM,
                                            ArrayRef<ProtocolDecl*> protocols) {
  // The ErrorType existential has a special boxed representation. It has space
  // only for witnesses to the ErrorType protocol.
  assert(protocols.size() == 1
     && *protocols[0]->getKnownProtocolKind() == KnownProtocolKind::ErrorType);
  
  const ProtocolInfo &impl = IGM.getProtocolInfo(protocols[0]);
  
  return new ErrorExistentialTypeInfo(IGM.ErrorPtrTy,
                                      IGM.getPointerSize(),
                                      IGM.getHeapObjectSpareBits(),
                                      IGM.getPointerAlignment(),
                                      ProtocolEntry(protocols[0], impl));
}

static const TypeInfo *createExistentialTypeInfo(IRGenModule &IGM,
                                            TypeBase *T,
                                            ArrayRef<ProtocolDecl*> protocols) {
  SmallVector<llvm::Type*, 5> fields;
  SmallVector<ProtocolEntry, 4> entries;

  // Check for special existentials.
  if (protocols.size() == 1) {
    switch (getSpecialProtocolID(protocols[0])) {
    case SpecialProtocol::ErrorType:
      // ErrorType has a special runtime representation.
      return createErrorExistentialTypeInfo(IGM, protocols);
    // Other existentials have standard representations.
    case SpecialProtocol::AnyObject:
    case SpecialProtocol::None:
      break;
    }
  }

  llvm::StructType *type;
  if (auto *protoT = T->getAs<ProtocolType>())
    type = IGM.createNominalType(protoT->getDecl());
  else if (auto *compT = T->getAs<ProtocolCompositionType>())
    // Protocol composition types are not nominal, but we name them anyway.
    type = IGM.createNominalType(compT);
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
    // Replace the type metadata pointer with the class instance.
    fields[1] = IGM.UnknownRefCountedPtrTy;
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
                                            size, std::move(spareBits), align);
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
  return createExistentialTypeInfo(IGM, T, T->getDecl());
}

const TypeInfo *
TypeConverter::convertProtocolCompositionType(ProtocolCompositionType *T) {
  // Find the canonical protocols.  There might not be any.
  SmallVector<ProtocolDecl*, 4> protocols;
  T->getAnyExistentialTypeProtocols(protocols);

  return createExistentialTypeInfo(IGM, T, protocols);
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

const TypeInfo *TypeConverter::convertArchetypeType(ArchetypeType *archetype) {
  assert(isExemplarArchetype(archetype) && "lowering non-exemplary archetype");

  // Compute layouts for the protocols we ascribe to.
  SmallVector<ProtocolEntry, 4> protocols;
  for (auto protocol : archetype->getConformsTo()) {
    const ProtocolInfo &impl = IGM.getProtocolInfo(protocol);
    protocols.push_back(ProtocolEntry(protocol, impl));
  }

  // If the archetype is class-constrained, use a class pointer
  // representation.
  if (archetype->requiresClass()) {
    // Fully general archetypes can't be assumed to have any particular
    // refcounting scheme.
    ReferenceCounting refcount = ReferenceCounting::Unknown;
    llvm::PointerType *reprTy = IGM.UnknownRefCountedPtrTy;

    // If the archetype has a superclass constraint, it has at least the
    // retain semantics of its superclass, and it can be represented with
    // the supertype's pointer type.
    if (Type super = archetype->getSuperclass()) {
      ClassDecl *superClass = super->getClassOrBoundGenericClass();
      refcount = getReferenceCountingForClass(IGM, superClass);

      auto &superTI = IGM.getTypeInfoForUnlowered(super);
      reprTy = cast<llvm::PointerType>(superTI.StorageType);
    }

    // As a hack, assume class archetypes never have spare bits. There's a
    // corresponding hack in MultiPayloadEnumImplStrategy::completeEnumTypeLayout
    // to ignore spare bits of dependent-typed payloads.
    auto spareBits =
      SpareBitVector::getConstant(IGM.getPointerSize().getValueInBits(), false);

    return ClassArchetypeTypeInfo::create(reprTy,
                                      IGM.getPointerSize(),
                                      spareBits,
                                      IGM.getPointerAlignment(),
                                      protocols, refcount);
  }

  // Otherwise, for now, always use an opaque indirect type.
  llvm::Type *storageType = IGM.OpaquePtrTy->getElementType();
  return OpaqueArchetypeTypeInfo::create(storageType, protocols);
}

/// Inform IRGenFunction that the given archetype has the given value
/// witness value within this scope.
void IRGenFunction::bindArchetype(ArchetypeType *archetype,
                                  llvm::Value *metadata,
                                  ArrayRef<llvm::Value*> wtables) {
  // Set the metadata pointer.
  bool setNames = !archetype->getOpenedExistentialType();
  if (setNames)
    metadata->setName(archetype->getFullName());
  setMetadataRef(*this, archetype, metadata);

  // Set the protocol witness tables.

  unsigned wtableI = 0;
  for (unsigned i = 0, e = archetype->getConformsTo().size(); i != e; ++i) {
    auto proto = archetype->getConformsTo()[i];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;
    auto wtable = wtables[wtableI++];
    if (setNames) {
      wtable->setName(Twine(archetype->getFullName()) + "." +
                      proto->getName().str());
    }
    setWitnessTable(*this, archetype, i, wtable);
  }
  assert(wtableI == wtables.size());
}

/// True if a function's signature in LLVM carries polymorphic parameters.
/// Generic functions and protocol witnesses carry polymorphic parameters.
bool irgen::hasPolymorphicParameters(CanSILFunctionType ty) {
  switch (ty->getRepresentation()) {
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    // Should never be polymorphic.
    assert(!ty->isPolymorphic() && "polymorphic C function?!");
    return false;
  case SILFunctionTypeRepresentation::ObjCMethod:
    // An ObjC witness_method reference will notionally have polymorphic type
    // <Self: P> (...) -> (...), but there are no polymorphic parameters that
    // can't be solved from the usual ObjC metadata.
    return false;

  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
    return ty->isPolymorphic();

  case SILFunctionTypeRepresentation::WitnessMethod:
    // Always carries polymorphic parameters for the Self type.
    return true;
  }
}

namespace {
  struct Fulfillment {
    Fulfillment() = default;
    Fulfillment(unsigned sourceIndex, unsigned depth, unsigned index)
      : SourceIndex(sourceIndex), Depth(depth), Index(index) {}

    /// The source index.
    unsigned SourceIndex;

    /// The distance up the metadata chain.
    /// 0 is the origin metadata, 1 is the parent of that, etc.
    unsigned Depth;

    /// The generic argument index.
    unsigned Index;
  };
  typedef std::pair<Type, ProtocolDecl*> FulfillmentKey;

  /// A class for computing how to pass arguments to a polymorphic
  /// function.  The subclasses of this are the places which need to
  /// be updated if the convention changes.
  class PolymorphicConvention {
  public:
    enum class SourceKind {
      /// The polymorphic arguments are derived from a source class
      /// pointer.
      ClassPointer,

      /// The polymorphic arguments are derived from a type metadata
      /// pointer.
      Metadata,

      /// The polymorphic arguments are passed from generic type
      /// metadata for the origin type.
      GenericLValueMetadata,

      /// The polymorphic arguments are derived from a Self type binding
      /// passed via the WitnessMethod convention.
      WitnessSelf,

      /// The polymorphic arguments are derived from a Self type binding
      /// embedded in a thick WitnessMethod function value.
      WitnessExtraData,
    };

    static bool requiresSourceIndex(SourceKind kind) {
      return (kind == SourceKind::ClassPointer ||
              kind == SourceKind::Metadata ||
              kind == SourceKind::GenericLValueMetadata);
    }

    enum : unsigned { InvalidSourceIndex = ~0U };

    class Source {
      /// The kind of source this is.
      SourceKind Kind;

      /// The parameter index, for ClassPointer and Metadata sources.
      unsigned Index;

    public:
      SmallVector<NominalTypeDecl*, 2> TypesForDepths;

      Source(SourceKind kind, unsigned index) : Kind(kind), Index(index) {
        assert(index != InvalidSourceIndex || !requiresSourceIndex(kind));
      }

      SourceKind getKind() const { return Kind; }
      unsigned getParamIndex() const {
        assert(requiresSourceIndex(getKind()));
        return Index;
      }
    };

  protected:
    CanSILFunctionType FnType;
    std::vector<Source> Sources;

    llvm::DenseMap<FulfillmentKey, Fulfillment> Fulfillments;

    ArchetypeBuilder ParamArchetypes;

    // Retrieve a representative archetype for a dependent type if it refers to
    // a generic parameter or a member of a generic parameter, or return null
    // if it does not.
    ArchetypeType::NestedType getRepresentativeArchetype(Type depType) {
      assert(depType->isDependentType()
             && "considering non-dependent type?!");

      auto *potential = ParamArchetypes.resolveArchetype(depType);
      if (!potential)
        return ArchetypeType::NestedType();

      return potential->getType(ParamArchetypes);
    }

  public:
    PolymorphicConvention(CanSILFunctionType fnType, Module &M)
        : FnType(fnType), ParamArchetypes(M, M.getASTContext().Diags) {
      assert(hasPolymorphicParameters(fnType));

      // Build archetypes from the generic signature so we can consult the
      // protocol requirements on the parameters and dependent types.
      //
      // TODO: The ArchetypeBuilder should be cached in the generic signature.
      ParamArchetypes.addGenericSignature(fnType->getGenericSignature(),
                                          false);

      // Protocol witnesses always derive all polymorphic parameter information
      // from the Self argument. We also *cannot* consider other arguments;
      // doing so would potentially make the signature incompatible with other
      // witnesses for the same method.
      if (fnType->getRepresentation()
            == SILFunctionTypeRepresentation::WitnessMethod) {
        // The metadata for a witness is provided from the type of the
        // self argument.
        Sources.emplace_back(SourceKind::WitnessSelf,
                             InvalidSourceIndex);

        // Testify to generic parameters in the Self type.
        CanType selfTy = fnType->getSelfParameter().getType();
        if (auto metaTy = dyn_cast<AnyMetatypeType>(selfTy))
          selfTy = metaTy.getInstanceType();

        if (auto nomTy = dyn_cast<NominalType>(selfTy))
          considerNominalType(nomTy, 0);
        else if (auto bgTy = dyn_cast<BoundGenericType>(selfTy))
          considerBoundGenericType(bgTy, 0);
        else if (auto param = dyn_cast<GenericTypeParamType>(selfTy))
          considerWitnessParamType(param);
        else if (isa<ArchetypeType>(selfTy))
          // A bound Self archetype from a protocol. Nothing to do.
          (void)0;
        else
          llvm_unreachable("witness for non-nominal type?!");

        return;
      }

      // We don't need to pass anything extra as long as all of the
      // archetypes (and their requirements) are producible from
      // arguments.
      auto params = fnType->getParameters();
      unsigned selfIndex = ~0U;

      // Consider 'self' first.
      if (fnType->hasSelfParam()) {
        selfIndex = params.size() - 1;
        considerParameter(params[selfIndex], selfIndex, true);
      }

      // Now consider the rest of the parameters.
      for (auto index : indices(params)) {
        if (index != selfIndex)
          considerParameter(params[index], index, false);
      }
    }

    /// Extract dependent type metadata for a value witness function of the given
    /// type.
    PolymorphicConvention(NominalTypeDecl *ntd, Module &M)
      : FnType(getNotionalFunctionType(ntd)),
        ParamArchetypes(M, M.getASTContext().Diags)
    {
      Sources.emplace_back(SourceKind::Metadata, 0);

      // Build archetypes from the generic signature so we can consult the
      // protocol requirements on the parameters and dependent types.
      //
      // TODO: The ArchetypeBuilder should be cached in the generic signature.
      ParamArchetypes.addGenericSignature(FnType->getGenericSignature(),
                                          false);

      auto paramType = FnType->getParameters()[0].getType();
      considerBoundGenericType(cast<BoundGenericType>(paramType), 0);
    }

    ArrayRef<Source> getSources() const { return Sources; }

    GenericSignatureWitnessIterator getAllDependentTypes() const {
      if (auto gs = FnType->getGenericSignature())
        return gs->getAllDependentTypes();
      return GenericSignatureWitnessIterator::emptyRange();
    }

  private:
    static CanSILFunctionType getNotionalFunctionType(NominalTypeDecl *D) {
      ASTContext &ctx = D->getASTContext();
      SILFunctionType::ExtInfo extInfo(SILFunctionType::Representation::Method,
                                       /*noreturn*/ false);
      SILResultInfo result(TupleType::getEmpty(ctx),
                           ResultConvention::Unowned);
      SILParameterInfo param(D->getDeclaredInterfaceType()->getCanonicalType(),
                              ParameterConvention::Direct_Owned);

      CanGenericSignature sig = D->getGenericSignatureOfContext()
        ? D->getGenericSignatureOfContext()->getCanonicalSignature()
        : nullptr;

      return SILFunctionType::get(sig, extInfo,
                                  ParameterConvention::Direct_Unowned,
                                  param, result, None, ctx);
    }

    template <class T>
    void considerNewSource(SourceKind kind, unsigned paramIndex,
                           const T &consider) {
      // Remember how many fulfillments we currently have.
      auto numFulfillments = Fulfillments.size();

      // Prospectively add a source.
      Sources.emplace_back(kind, paramIndex);

      // Consider the source.
      consider();

      // If we didn't add anything, remove the source.
      if (Fulfillments.size() == numFulfillments)
        Sources.pop_back();
    }

    void considerNewTypeSource(SourceKind kind, unsigned paramIndex,
                               CanType type) {
      if (auto nomTy = dyn_cast<NominalType>(type)) {
        considerNewSource(kind, paramIndex,
                          [&] { considerNominalType(nomTy, 0); });
      } else if (auto boundTy = dyn_cast<BoundGenericType>(type)) {
        considerNewSource(kind, paramIndex,
                          [&] { considerBoundGenericType(boundTy, 0); });
      }
    }

    void considerParameter(SILParameterInfo param, unsigned paramIndex,
                           bool isSelfParameter) {
      auto type = param.getType();
      switch (param.getConvention()) {
      // Out-parameters don't give us a value we can use.
      case ParameterConvention::Indirect_Out:
        return;

      // In-parameters do, but right now we don't bother, for no good
      // reason. But if this is 'self', consider passing an extra
      // metatype.
      case ParameterConvention::Indirect_In:
      case ParameterConvention::Indirect_In_Guaranteed:
      case ParameterConvention::Indirect_Inout:
        if (!isSelfParameter) return;
        considerNewTypeSource(SourceKind::GenericLValueMetadata,
                              paramIndex, type);
        return;

      case ParameterConvention::Direct_Owned:
      case ParameterConvention::Direct_Unowned:
      case ParameterConvention::Direct_Guaranteed:
      case ParameterConvention::Direct_Deallocating:
        // Classes are sources of metadata.
        if (auto classTy = dyn_cast<ClassType>(type)) {
          considerNewSource(SourceKind::ClassPointer, paramIndex,
                            [&] { considerNominalType(classTy, 0); });
          return;
        } else if (auto boundTy = dyn_cast<BoundGenericClassType>(type)) {
          considerNewSource(SourceKind::ClassPointer, paramIndex,
                            [&] { considerBoundGenericType(boundTy, 0); });
          return;
        }

        // Thick metatypes are sources of metadata.
        if (auto metatypeTy = dyn_cast<MetatypeType>(type)) {
          if (metatypeTy->getRepresentation() != MetatypeRepresentation::Thick)
            return;

          CanType objTy = metatypeTy.getInstanceType();
          considerNewTypeSource(SourceKind::Metadata, paramIndex, objTy);
          return;
        }

        return;
      }
      llvm_unreachable("bad parameter convention");
    }

    void considerParentType(CanType parent, unsigned depth) {
      // We might not have a parent type.
      if (!parent) return;

      // If we do, it has to be nominal one way or another.
      depth++;
      if (auto nom = dyn_cast<NominalType>(parent))
        considerNominalType(nom, depth);
      else
        considerBoundGenericType(cast<BoundGenericType>(parent), depth);
    }

    void considerNominalType(NominalType *type, unsigned depth) {
      assert(Sources.back().TypesForDepths.size() == depth);
      Sources.back().TypesForDepths.push_back(type->getDecl());

      // Nominal types add no generic arguments themselves, but they
      // may have the arguments of their parents.
      considerParentType(CanType(type->getParent()), depth);
    }

    void considerBoundGenericType(BoundGenericType *type, unsigned depth) {
      assert(Sources.back().TypesForDepths.size() == depth);
      Sources.back().TypesForDepths.push_back(type->getDecl());

      auto params = type->getDecl()->getGenericParams()->getAllArchetypes();
      auto substitutions = type->getSubstitutions(/*FIXME:*/nullptr, nullptr);
      assert(params.size() >= substitutions.size() &&
             "generic decl archetypes should parallel generic type subs");

      for (unsigned i = 0, e = substitutions.size(); i != e; ++i) {
        auto sub = substitutions[i];
        CanType arg = sub.getReplacement()->getCanonicalType();

        // Right now, we can only pull things out of the direct
        // arguments, not out of nested metadata.  For example, this
        // prevents us from realizing that we can rederive T and U in the
        // following:
        //   \forall T U . Vector<T->U> -> ()
        if (arg->isDependentType()) {
          // Find the archetype from the dependent type.
          considerDependentType(arg, params[i], depth, i);
        }
      }

      // Match against the parent first.  The polymorphic type
      // will start with any arguments from the parent.
      considerParentType(CanType(type->getParent()), depth);
    }

    /// We found a reference to the dependent arg type at the given depth
    /// and index.  Add any fulfillments this gives us.
    void considerDependentType(Type arg,
                               ArchetypeType *param,
                               unsigned depth,
                               unsigned index) {
      // If we don't have a representative archetype for this dependent type,
      // don't try to fulfill it. It doesn't directly correspond to one of our
      // parameters or their associated types.
      auto representative = getRepresentativeArchetype(arg);
      if (!representative)
        return;
      auto arch = representative.getAsArchetype();
      if (!arch)
        return;

      // First, record that we can find this dependent type at this point.
      addFulfillment(arg, nullptr, depth, index);

      // Now consider each of the protocols that the parameter guarantees.
      for (auto protocol : param->getConformsTo()) {
        if (requiresFulfillment(arch, protocol))
          addFulfillment(arg, protocol, depth, index);
      }
    }

    /// We're binding an archetype for a protocol witness.
    void considerWitnessParamType(CanGenericTypeParamType arg) {
      assert(arg->getDepth() == 0 && arg->getIndex() == 0);
      auto representative = getRepresentativeArchetype(arg);
      assert(representative && "no representative for dependent type?!");

      // First of all, the archetype or concrete type fulfills its own
      // requirements.
      if (auto arch = representative.getAsArchetype())
        considerDependentType(arg, arch, 0, 0);

      // FIXME: We can't pass associated types of Self through the witness
      // CC, so as a hack, fake up impossible fulfillments for the associated
      // types. For now all conformances are concrete, so the associated types
      // can be recovered by substitution on the implementation side. For
      // default implementations, we will need to get associated types from
      // witness tables anyway.
      for (auto depTy : getAllDependentTypes()) {
        // Is this a dependent member?
        auto depMemTy = dyn_cast<DependentMemberType>(CanType(depTy));
        if (!depMemTy)
          continue;

        // Is it rooted in a generic parameter?
        CanType rootTy;
        do {
          rootTy = depMemTy.getBase();
        } while ((depMemTy = dyn_cast<DependentMemberType>(rootTy)));

        auto rootParamTy = dyn_cast<GenericTypeParamType>(rootTy);
        if (!rootParamTy)
          continue;

        // If so, suppress providing metadata for the type by making up a bogus
        // fulfillment.
        if (rootParamTy == arg) {
          auto depRep = getRepresentativeArchetype(depTy);
          assert(depRep && "no representative for dependent type?!");
          if (auto depArch = depRep.getAsArchetype())
            considerDependentType(depTy, depArch, ~0u, ~0u);
        }
      }
    }

    /// Does the given archetype require the given protocol to be fulfilled?
    static bool requiresFulfillment(ArchetypeType *representative,
                                    ProtocolDecl *proto) {
      // TODO: protocol inheritance should be considered here somehow.
      for (auto argProto : representative->getConformsTo()) {
        if (argProto == proto)
          return true;
      }
      return false;
    }

    /// Testify that there's a fulfillment at the given depth and level.
    void addFulfillment(Type arg, ProtocolDecl *proto,
                        unsigned depth, unsigned index) {
      assert(!Sources.empty() && "adding fulfillment without source?");
      auto sourceIndex = Sources.size() - 1;

      // Only add a fulfillment if we don't have any previous
      // fulfillment for that value.
      assert(arg->isDependentType() && "fulfilling non-dependent type?!");
      auto key = FulfillmentKey(arg, proto);
      Fulfillments.insert(std::make_pair(key,
                                   Fulfillment(sourceIndex, depth, index)));
    }
  };

  /// A class for binding type parameters of a generic function.
  class EmitPolymorphicParameters : public PolymorphicConvention {
    IRGenFunction &IGF;
    GenericParamList *ContextParams;

    struct SourceValue {
      SmallVector<llvm::Value*, 4> MetadataForDepths;
    };

    std::vector<SourceValue> SourceValues;

  public:
    EmitPolymorphicParameters(IRGenFunction &IGF,
                              SILFunction &Fn)
      : PolymorphicConvention(Fn.getLoweredFunctionType(),
                              *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF), ContextParams(Fn.getContextGenericParams()) {}

    void emit(Explosion &in, WitnessMetadata *witnessMetadata,
              const GetParameterFn &getParameter);

    /// Emit polymorphic parameters for a generic value witness.
    EmitPolymorphicParameters(IRGenFunction &IGF, NominalTypeDecl *ntd)
      : PolymorphicConvention(ntd, *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF), ContextParams(ntd->getGenericParams()) {}

    void emitForGenericValueWitness(llvm::Value *selfMeta);

  private:
    // Emit metadata bindings after the source, if any, has been bound.
    void emitWithSourcesBound(Explosion &in);

    CanType getArgTypeInContext(unsigned paramIndex) const {
      return ArchetypeBuilder::mapTypeIntoContext(
                            IGF.IGM.SILMod->getSwiftModule(), ContextParams,
                            FnType->getParameters()[paramIndex].getType())
        ->getCanonicalType();
    }

    /// Emit the source value for parameters.
    llvm::Value *emitSourceForParameters(const Source &source,
                                         Explosion &in,
                                         WitnessMetadata *witnessMetadata,
                                         const GetParameterFn &getParameter) {
      switch (source.getKind()) {
      case SourceKind::Metadata:
        return getParameter(source.getParamIndex());

      case SourceKind::ClassPointer: {
        unsigned paramIndex = source.getParamIndex();
        llvm::Value *instanceRef = getParameter(paramIndex);
        SILType instanceType =
          SILType::getPrimitiveObjectType(getArgTypeInContext(paramIndex));
        return emitDynamicTypeOfHeapObject(IGF, instanceRef, instanceType);
      }

      case SourceKind::GenericLValueMetadata: {
        llvm::Value *metatype = in.claimNext();
        metatype->setName("Self");

        // Mark this as the cached metatype for the l-value's object type.
        CanType argTy = getArgTypeInContext(source.getParamIndex());
        IGF.setUnscopedLocalTypeData(argTy, LocalTypeData::forMetatype(),
                                     metatype);
        return metatype;
      }

      case SourceKind::WitnessSelf: {
        assert(witnessMetadata && "no metadata for witness method");
        llvm::Value *metatype = witnessMetadata->SelfMetadata;
        assert(metatype && "no Self metadata for witness method");
        
        // Mark this as the cached metatype for Self.
        CanType argTy = getArgTypeInContext(FnType->getParameters().size() - 1);
        IGF.setUnscopedLocalTypeData(argTy,
                                     LocalTypeData::forMetatype(), metatype);
        return metatype;
      }
          
      case SourceKind::WitnessExtraData: {
        // The 'Self' parameter is provided last.
        // TODO: For default implementations, the witness table pointer for
        // the 'Self : P' conformance must be provided last along with the
        // metatype.
        llvm::Value *metatype = in.takeLast();
        metatype->setName("Self");
        return metatype;
      }
      }
      llvm_unreachable("bad source kind!");
    }

    /// Produce the metadata value for the given depth, using the
    /// given cache.
    llvm::Value *getMetadataForDepth(unsigned sourceIndex, unsigned depth) {
      auto &source = getSources()[sourceIndex];
      auto &sourceValue = SourceValues[sourceIndex];
      assert(!sourceValue.MetadataForDepths.empty());

      // Drill down until we get to that depth.
      while (depth >= sourceValue.MetadataForDepths.size()) {
        auto child = sourceValue.MetadataForDepths.back();
        auto childDecl =
          source.TypesForDepths[sourceValue.MetadataForDepths.size()];
        auto parent = emitParentMetadataRef(IGF, childDecl, child);
        sourceValue.MetadataForDepths.push_back(parent);
      }
      return sourceValue.MetadataForDepths[depth];
    }

    /// Produce the base metadata value and declaration for the given
    /// fulfillment.
    std::pair<llvm::Value*, NominalTypeDecl *>
    getAncestorForFulfillment(const Fulfillment &fulfillment) {
      auto ancestor = getMetadataForDepth(fulfillment.SourceIndex,
                                          fulfillment.Depth);
      auto ancestorDecl =
        Sources[fulfillment.SourceIndex].TypesForDepths[fulfillment.Depth];
      return std::make_pair(ancestor, ancestorDecl);
    }
  };
};

/// Emit a polymorphic parameters clause, binding all the metadata necessary.
void EmitPolymorphicParameters::emit(Explosion &in,
                                     WitnessMetadata *witnessMetadata,
                                     const GetParameterFn &getParameter) {
  SourceValues.reserve(getSources().size());
  for (const Source &source : getSources()) {
    llvm::Value *value =
      emitSourceForParameters(source, in, witnessMetadata, getParameter);
    SourceValues.emplace_back();
    SourceValues.back().MetadataForDepths.push_back(value);
  }

  emitWithSourcesBound(in);
}

/// Emit a polymorphic parameters clause for a generic value witness, binding
/// all the metadata necessary.
void
EmitPolymorphicParameters::emitForGenericValueWitness(llvm::Value *selfMeta) {
  // We get the source metadata verbatim from the value witness signature.
  assert(getSources().size() == 1);
  SourceValues.emplace_back();
  SourceValues.back().MetadataForDepths.push_back(selfMeta);

  // All our archetypes should be satisfiable from the source.
  Explosion empty;
  emitWithSourcesBound(empty);
}

void
EmitPolymorphicParameters::emitWithSourcesBound(Explosion &in) {
  for (auto depTy : getAllDependentTypes()) {
    // Get the corresponding context archetype.
    auto contextTy
      = ArchetypeBuilder::mapTypeIntoContext(IGF.IGM.SILMod->getSwiftModule(),
                                             ContextParams, depTy)
        ->getAs<ArchetypeType>();
    if (!contextTy)
      continue;

    // Derive the appropriate metadata reference.
    llvm::Value *metadata;

    // If the reference is fulfilled by the source, go for it.
    auto it = Fulfillments.find(FulfillmentKey(depTy, nullptr));
    if (it != Fulfillments.end()) {
      auto &fulfillment = it->second;
      auto ancestorAndDecl = getAncestorForFulfillment(fulfillment);
      metadata = emitArgumentMetadataRef(IGF, ancestorAndDecl.second,
                                         fulfillment.Index,
                                         ancestorAndDecl.first);

    // Otherwise, it's just next in line.
    } else {
      metadata = in.claimNext();
    }

    // Collect all the witness tables.
    SmallVector<llvm::Value *, 8> wtables;
    for (auto protocol : contextTy->getConformsTo()) {
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      llvm::Value *wtable;

      // If the protocol witness table is fulfilled by the source, go for it.
      auto it = Fulfillments.find(FulfillmentKey(depTy, protocol));
      if (it != Fulfillments.end()) {
        auto &fulfillment = it->second;
        auto ancestorAndDecl = getAncestorForFulfillment(fulfillment);
        wtable = emitArgumentWitnessTableRef(IGF, ancestorAndDecl.second,
                                             fulfillment.Index, protocol,
                                             ancestorAndDecl.first);

      // Otherwise, it's just next in line.
      } else {
        wtable = in.claimNext();
      }
      wtables.push_back(wtable);
    }
    IGF.bindArchetype(contextTy, metadata, wtables);
  }
}

/// Collect any required metadata for a witness method from the end of
/// the given parameter list.
void irgen::collectTrailingWitnessMetadata(IRGenFunction &IGF,
                                           SILFunction &fn,
                                           Explosion &params,
                                           WitnessMetadata &witnessMetadata) {
  assert(fn.getLoweredFunctionType()->getRepresentation()
           == SILFunctionTypeRepresentation::WitnessMethod);

  llvm::Value *metatype = params.takeLast();
  assert(metatype->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "parameter signature mismatch: witness metadata didn't "
         "end in metatype?");
  metatype->setName("Self");
  witnessMetadata.SelfMetadata = metatype;
}

/// Perform all the bindings necessary to emit the given declaration.
void irgen::emitPolymorphicParameters(IRGenFunction &IGF,
                                      SILFunction &Fn,
                                      Explosion &in,
                                      WitnessMetadata *witnessMetadata,
                                      const GetParameterFn &getParameter) {
  EmitPolymorphicParameters(IGF, Fn).emit(in, witnessMetadata, getParameter);
}

/// Perform the metadata bindings necessary to emit a generic value witness.
void irgen::emitPolymorphicParametersForGenericValueWitness(IRGenFunction &IGF,
                                                        NominalTypeDecl *ntd,
                                                        llvm::Value *selfMeta) {
  // Nothing to do if the type isn't generic.
  if (!ntd->getGenericParamsOfContext())
    return;

  EmitPolymorphicParameters(IGF, ntd).emitForGenericValueWitness(selfMeta);
  // Register the 'Self' argument as generic metadata for the type.
  IGF.setUnscopedLocalTypeData(ntd->getDeclaredTypeInContext()->getCanonicalType(),
                               LocalTypeData::forMetatype(), selfMeta);
}

/// Get the next argument and use it as the 'self' type metadata.
static void getArgAsLocalSelfTypeMetadata(IRGenFunction &IGF,
                                          llvm::Function::arg_iterator &it,
                                          CanType abstractType) {
  llvm::Value *arg = getArg(it, "Self");
  assert(arg->getType() == IGF.IGM.TypeMetadataPtrTy &&
         "Self argument is not a type?!");
  if (auto ugt = dyn_cast<UnboundGenericType>(abstractType)) {
    emitPolymorphicParametersForGenericValueWitness(IGF, ugt->getDecl(), arg);
  }
}

namespace {
  /// A CRTP class for finding the archetypes we need to bind in order
  /// to perform value operations on the given type.
  struct FindArchetypesForValueOperations
    : CanTypeVisitor<FindArchetypesForValueOperations>
  {
    NecessaryBindings &Bindings;
  public:
    FindArchetypesForValueOperations(NecessaryBindings &bindings)
      : Bindings(bindings) {}

    // We're collecting archetypes.
    void visitArchetypeType(CanArchetypeType type) {
      Bindings.addArchetype(type);
    }

    // We need to walk into tuples.
    void visitTupleType(CanTupleType tuple) {
      for (auto eltType : tuple.getElementTypes()) {
        visit(eltType);
      }
    }

    // Walk into on-stack block storage.
    void visitSILBlockStorageType(CanSILBlockStorageType t) {
      visit(t->getCaptureType());
    }

    // We do not need to walk into any of these types, because their
    // value operations do not depend on the specifics of their
    // sub-structure (or they have none).
    void visitAnyFunctionType(CanAnyFunctionType fn) {}
    void visitSILFunctionType(CanSILFunctionType fn) {}
    void visitBuiltinType(CanBuiltinType type) {}
    void visitAnyMetatypeType(CanAnyMetatypeType type) {}
    void visitModuleType(CanModuleType type) {}
    void visitDynamicSelfType(CanDynamicSelfType type) {}
    void visitProtocolCompositionType(CanProtocolCompositionType type) {}
    void visitReferenceStorageType(CanReferenceStorageType type) {}

    // L-values are impossible.
    void visitLValueType(CanLValueType type) {
      llvm_unreachable("cannot store l-value type directly");
    }
    void visitInOutType(CanInOutType type) {
      llvm_unreachable("cannot store inout type directly");
    }

    // Bind archetypes from the parent of nominal types.
    void visitNominalType(CanNominalType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
    }
    // Bind archetypes from bound generic types and their parents.
    void visitBoundGenericType(CanBoundGenericType type) {
      if (auto parent = CanType(type->getParent()))
        visit(parent);
      for (auto arg : type->getGenericArgs())
        visit(CanType(arg));
    }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitGenericTypeParamType(CanGenericTypeParamType type) { }

    // FIXME: Will need to bind the archetype that this eventually refers to.
    void visitDependentMemberType(CanDependentMemberType type) { }
  };
}

NecessaryBindings
NecessaryBindings::forFunctionInvocations(IRGenModule &IGM,
                                          CanSILFunctionType origType,
                                          CanSILFunctionType substType,
                                          ArrayRef<Substitution> subs) {
  NecessaryBindings bindings;
  // Collect bindings required by the polymorphic parameters to the function.
  for (auto &sub : subs) {
    sub.getReplacement().findIf([&](Type t) -> bool {
      if (auto archetype = dyn_cast<ArchetypeType>(t->getCanonicalType())) {
        bindings.addArchetype(archetype);
      }
      return false;
    });
  }
  return bindings;
}

NecessaryBindings
NecessaryBindings::forValueOperations(IRGenModule &IGM, CanType type) {
  NecessaryBindings bindings;
  FindArchetypesForValueOperations(bindings).visit(type);
  return bindings;
}

Size NecessaryBindings::getBufferSize(IRGenModule &IGM) const {
  unsigned numPointers = 0;

  // We need one pointer for each archetype and witness table.
  for (auto type : Types) {
    numPointers++;
    for (auto proto : type->getConformsTo())
      if (Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
        numPointers++;
  }

  return IGM.getPointerSize() * numPointers;
}

void NecessaryBindings::restore(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned archetypeI = 0, e = Types.size(), metadataI = 0;
       archetypeI != e; ++archetypeI) {
    auto archetype = Types[archetypeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Load the archetype's metatype.
    llvm::Value *metatype = IGF.Builder.CreateLoad(slot);
    metatype->setName(archetype->getFullName());
    setMetadataRef(IGF, archetype, metatype);

    // Load the witness tables for the archetype's protocol constraints.
    for (unsigned protocolI : indices(archetype->getConformsTo())) {
      auto protocol = archetype->getConformsTo()[protocolI];
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness = IGF.Builder.CreateLoad(witnessSlot);

      setWitnessTable(IGF, archetype, protocolI, witness);
    }
  }
}

void NecessaryBindings::save(IRGenFunction &IGF, Address buffer) const {
  if (Types.empty()) return;

  // Cast the buffer to %type**.
  auto metatypePtrPtrTy = IGF.IGM.TypeMetadataPtrTy->getPointerTo();
  buffer = IGF.Builder.CreateBitCast(buffer, metatypePtrPtrTy);

  for (unsigned typeI = 0, typeE = Types.size(),
                metadataI = 0; typeI != typeE; ++typeI) {
    auto archetype = Types[typeI];

    // GEP to the appropriate slot.
    Address slot = buffer;
    if (metadataI) slot = IGF.Builder.CreateConstArrayGEP(slot, metadataI,
                                                  IGF.IGM.getPointerSize());
    ++metadataI;

    // Find the metatype for the appropriate archetype and store it in
    // the slot.
    llvm::Value *metatype =
      IGF.getLocalTypeData(CanType(archetype), LocalTypeData::forMetatype());
    IGF.Builder.CreateStore(metatype, slot);

    // Find the witness tables for the archetype's protocol constraints and
    // store them in the slot.
    for (unsigned protocolI : indices(archetype->getConformsTo())) {
      auto protocol = archetype->getConformsTo()[protocolI];
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;
      Address witnessSlot = IGF.Builder.CreateConstArrayGEP(buffer, metadataI,
                                                      IGF.IGM.getPointerSize());
      witnessSlot = IGF.Builder.CreateBitCast(witnessSlot,
                                    IGF.IGM.WitnessTablePtrTy->getPointerTo());
      ++metadataI;
      llvm::Value *witness =
        IGF.getLocalTypeData(CanType(archetype),
                         LocalTypeData::forArchetypeProtocolWitness(protocolI));
      IGF.Builder.CreateStore(witness, witnessSlot);
    }
  }
}

void NecessaryBindings::addArchetype(CanArchetypeType type) {
  if (Types.insert(type))
    // Collect the associated archetypes.
    for (auto nested : type->getNestedTypes())
      if (auto assocArchetype = nested.second.getAsArchetype())
        addArchetype(CanArchetypeType(assocArchetype));
}

/// Emit a single protocol witness table reference.
llvm::Value *irgen::emitWitnessTableRef(IRGenFunction &IGF,
                                        CanArchetypeType archetype,
                                        ProtocolDecl *proto) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(proto) &&
         "looking up witness table for protocol that doesn't have one");

  auto &archTI = getArchetypeInfo(IGF, archetype,
                                  IGF.getTypeInfoForLowered(archetype));
  ProtocolPath path(IGF.IGM, archTI.getStoredProtocols(), proto);
  auto wtable = archTI.getWitnessTable(IGF, archetype,
                                       path.getOriginIndex());
  wtable = path.apply(IGF, wtable);
  return wtable;
}

/// Emit a protocol witness table for a conformance.
llvm::Value *irgen::emitWitnessTableRef(IRGenFunction &IGF,
                                        CanType srcType,
                                        const TypeInfo &srcTI,
                                        ProtocolDecl *proto,
                                        const ProtocolInfo &protoI,
                                        ProtocolConformance *conformance) {
  assert(Lowering::TypeConverter::protocolRequiresWitnessTable(proto)
         && "protocol does not have witness tables?!");

  // If the source type is an archetype and we don't have concrete conformance
  // info, the conformance must be via one of the protocol requirements of the
  // archetype. Look at what's locally bound.
  if (!conformance) {
    auto archetype = cast<ArchetypeType>(srcType);
    return emitWitnessTableRef(IGF, archetype, proto);
  }

  // All other source types should be concrete enough that we have conformance
  // info for them.
  auto &conformanceI = protoI.getConformance(IGF.IGM, srcType,
                                             srcTI, proto, *conformance);
  return conformanceI.getTable(IGF);
}

/// Emit the witness table references required for the given type
/// substitution.
void irgen::emitWitnessTableRefs(IRGenFunction &IGF,
                                 const Substitution &sub,
                                 SmallVectorImpl<llvm::Value*> &out) {
  auto conformances = sub.getConformances();

  // We don't need to do anything if we have no protocols to conform to.
  auto archetypeProtos = sub.getArchetype()->getConformsTo();
  assert(!conformances.size() || archetypeProtos.size() == conformances.size());

  if (archetypeProtos.empty()) return;

  // Look at the replacement type.
  CanType replType = sub.getReplacement()->getCanonicalType();
  auto &replTI = IGF.getTypeInfoForUnlowered(replType);

  for (unsigned j = 0, je = archetypeProtos.size(); j != je; ++j) {
    auto proto = archetypeProtos[j];
    if (!Lowering::TypeConverter::protocolRequiresWitnessTable(proto))
      continue;

    auto conformance = conformances.size() ? conformances[j] : nullptr;
    auto wtable = emitWitnessTableRef(IGF, replType, replTI, proto,
                                      IGF.IGM.getProtocolInfo(proto),
                                      conformance);

    out.push_back(wtable);
  }
}

namespace {
  class EmitPolymorphicArguments : public PolymorphicConvention {
    IRGenFunction &IGF;
  public:
    EmitPolymorphicArguments(IRGenFunction &IGF,
                             CanSILFunctionType polyFn)
      : PolymorphicConvention(polyFn, *IGF.IGM.SILMod->getSwiftModule()),
        IGF(IGF) {}

    void emit(CanType substInputType, ArrayRef<Substitution> subs,
              WitnessMetadata *witnessMetadata, Explosion &out);

  private:
    void emitEarlySources(CanType substInputType, Explosion &out) {
      for (auto &source : getSources()) {
        switch (source.getKind()) {
        // Already accounted for in the parameters.
        case SourceKind::ClassPointer:
        case SourceKind::Metadata:
          continue;

        // Needs a special argument.
        case SourceKind::GenericLValueMetadata: {
          out.add(IGF.emitTypeMetadataRef(substInputType));
          continue;
        }

        // Witness 'Self' argument(s) are added as a special case in
        // EmitPolymorphicArguments::emit.
        case SourceKind::WitnessSelf:
          continue;

        // The 'Self' argument(s) are added implicitly from ExtraData
        // of the function value.
        case SourceKind::WitnessExtraData:
          continue;
        }
        llvm_unreachable("bad source kind!");
      }
    }
  };
}

void irgen::emitTrailingWitnessArguments(IRGenFunction &IGF,
                                         WitnessMetadata &witnessMetadata,
                                         Explosion &args) {
  llvm::Value *self = witnessMetadata.SelfMetadata;
  assert(self && "no Self value bound");
  args.add(self);
}

/// Pass all the arguments necessary for the given function.
void irgen::emitPolymorphicArguments(IRGenFunction &IGF,
                                     CanSILFunctionType origFnType,
                                     CanSILFunctionType substFnType,
                                     ArrayRef<Substitution> subs,
                                     WitnessMetadata *witnessMetadata,
                                     Explosion &out) {
  // Grab the apparent 'self' type.  If there isn't a 'self' type,
  // we're not going to try to access this anyway.
  CanType substInputType;
  if (!substFnType->getParameters().empty()) {
    auto selfParam = substFnType->getParameters().back();
    substInputType = selfParam.getType();
    // If the parameter is a direct metatype parameter, this is a static method
    // of the instance type. We can assume this because:
    // - metatypes cannot directly conform to protocols
    // - even if they could, they would conform as a value type 'self' and thus
    //   be passed indirectly as an @in or @inout parameter.
    if (auto meta = dyn_cast<MetatypeType>(substInputType)) {
      if (!selfParam.isIndirect())
        substInputType = meta.getInstanceType();
    }
  }

  EmitPolymorphicArguments(IGF, origFnType).emit(substInputType, subs,
                                                 witnessMetadata, out);
}

void EmitPolymorphicArguments::emit(CanType substInputType,
                                    ArrayRef<Substitution> subs,
                                    WitnessMetadata *witnessMetadata,
                                    Explosion &out) {
  // Add all the early sources.
  emitEarlySources(substInputType, out);

  // For now, treat all archetypes independently.
  // FIXME: Later, we'll want to emit only the minimal set of archetypes,
  // because non-primary archetypes (which correspond to associated types)
  // will have their witness tables embedded in the witness table corresponding
  // to their parent.
  for (auto depTy : getAllDependentTypes()) {
    // The substitutions should be in the same order.
    const Substitution &sub = subs.front();
    subs = subs.slice(1);

    CanType argType = sub.getReplacement()->getCanonicalType();

    // If same-type constraints have eliminated the genericity of this
    // parameter, it doesn't need an independent metadata parameter.
    auto type = getRepresentativeArchetype(depTy);
    assert(type && "no potential archetype for dependent type?!");
    auto arch = type.getAsArchetype();
    if (!arch)
      continue;

    // Add the metadata reference unless it's fulfilled.
    if (!Fulfillments.count(FulfillmentKey(depTy, nullptr))) {
      out.add(IGF.emitTypeMetadataRef(argType));
    }

    // Nothing else to do if there aren't any protocols to witness.
    auto protocols = arch->getConformsTo();
    auto conformances = sub.getConformances();
    assert(!conformances.size() || protocols.size() == conformances.size());

    if (protocols.empty()) continue;

    auto &argTI = IGF.getTypeInfoForUnlowered(argType);

    // Add witness tables for each of the required protocols.
    for (unsigned i = 0, e = protocols.size(); i != e; ++i) {
      auto protocol = protocols[i];

      // Skip this if the protocol doesn't require a witness table.
      if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
        continue;

      // Skip this if it's fulfilled by the source.
      if (Fulfillments.count(FulfillmentKey(depTy, protocol)))
        continue;

      auto conformance = conformances.size() ? conformances[i] : nullptr;
      auto wtable = emitWitnessTableRef(IGF,
                                        argType, argTI,
                                        protocol,
                                        IGF.IGM.getProtocolInfo(protocol),
                                        conformance);
      out.add(wtable);
    }
  }
  assert(subs.empty()
         && "did not use all substitutions?!");

  // For a witness call, add the Self argument metadata arguments last.
  for (auto &source : getSources()) {
    switch (source.getKind()) {
    case SourceKind::Metadata:
    case SourceKind::ClassPointer:
      // Already accounted for in the arguments.
      continue;

    case SourceKind::GenericLValueMetadata:
      // Added in the early phase.
      continue;

    case SourceKind::WitnessSelf: {
      assert(witnessMetadata && "no metadata structure for witness method");
      auto self = IGF.emitTypeMetadataRef(substInputType);
      witnessMetadata->SelfMetadata = self;
      continue;
    }

    case SourceKind::WitnessExtraData:
      // The 'Self' argument(s) are added implicitly from ExtraData of the
      // function value.
      continue;
    }
    llvm_unreachable("bad source kind");
  }
}

namespace {
  /// A class for expanding a polymorphic signature.
  class ExpandPolymorphicSignature : public PolymorphicConvention {
    IRGenModule &IGM;
  public:
    ExpandPolymorphicSignature(IRGenModule &IGM, CanSILFunctionType fn)
      : PolymorphicConvention(fn, *IGM.SILMod->getSwiftModule()), IGM(IGM) {}

    void expand(SmallVectorImpl<llvm::Type*> &out) {
      for (auto &source : getSources())
        addEarlySource(source, out);

      for (auto depTy : getAllDependentTypes()) {
        // Only emit parameters for independent parameters that haven't been
        // constrained to concrete types.
        auto representative = getRepresentativeArchetype(depTy);
        assert(representative && "no representative archetype for param?!");
        auto arch = representative.getAsArchetype();
        if (!arch)
          continue;

        // Pass the type argument if not fulfilled.
        if (!Fulfillments.count(FulfillmentKey(depTy, nullptr)))
          out.push_back(IGM.TypeMetadataPtrTy);

        // Pass each signature requirement that needs a witness table
        // separately (unless fulfilled).
        for (auto protocol : arch->getConformsTo()) {
          if (!Lowering::TypeConverter::protocolRequiresWitnessTable(protocol))
            continue;

          if (!Fulfillments.count(FulfillmentKey(depTy, protocol)))
            out.push_back(IGM.WitnessTablePtrTy);
        }
      }
    }

  private:
    /// Add signature elements for the source metadata.
    void addEarlySource(const Source &source,
                        SmallVectorImpl<llvm::Type*> &out) {
      switch (source.getKind()) {
      case SourceKind::ClassPointer: return; // already accounted for
      case SourceKind::Metadata: return; // already accounted for
      case SourceKind::GenericLValueMetadata:
        return out.push_back(IGM.TypeMetadataPtrTy);
      case SourceKind::WitnessSelf:
        return; // handled as a special case in expand()
      case SourceKind::WitnessExtraData:
        return; // added implicitly as ExtraData
      }
      llvm_unreachable("bad source kind");
    }
  };
}

/// Given a generic signature, add the argument types required in order to call it.
void irgen::expandPolymorphicSignature(IRGenModule &IGM,
                                       CanSILFunctionType polyFn,
                                       SmallVectorImpl<llvm::Type*> &out) {
  ExpandPolymorphicSignature(IGM, polyFn).expand(out);
}

void irgen::expandTrailingWitnessSignature(IRGenModule &IGM,
                                           CanSILFunctionType polyFn,
                                           SmallVectorImpl<llvm::Type*> &out) {
  assert(polyFn->getRepresentation()
          == SILFunctionTypeRepresentation::WitnessMethod);

  assert(getTrailingWitnessSignatureLength(IGM, polyFn) == 1);

  // A witness method always provides Self.
  out.push_back(IGM.TypeMetadataPtrTy);

  // TODO: Should also provide the protocol witness table,
  // for default implementations.
}

/// Retrieve the protocol witness table for a conformance.
static llvm::Value *getProtocolWitnessTable(IRGenFunction &IGF,
                                            CanType srcType,
                                            const TypeInfo &srcTI,
                                            ProtocolEntry protoEntry,
                                            ProtocolConformance *conformance) {
  return emitWitnessTableRef(IGF, srcType, srcTI,
                             protoEntry.getProtocol(),
                             protoEntry.getInfo(),
                             conformance);
}

/// Emit protocol witness table pointers for the given protocol conformances,
/// passing each emitted witness table index into the given function body.
static void forEachProtocolWitnessTable(IRGenFunction &IGF,
                          CanType srcType, CanType destType,
                          ArrayRef<ProtocolEntry> protocols,
                          ArrayRef<ProtocolConformance*> conformances,
                          std::function<void (unsigned, llvm::Value*)> body) {
  // Collect the conformances that need witness tables.
  SmallVector<ProtocolDecl*, 2> destProtocols;
  destType.getAnyExistentialTypeProtocols(destProtocols);

  SmallVector<ProtocolConformance*, 2> witnessConformances;
  assert(destProtocols.size() == conformances.size() &&
         "mismatched protocol conformances");
  for (unsigned i = 0, size = destProtocols.size(); i < size; ++i)
    if (Lowering::TypeConverter::protocolRequiresWitnessTable(destProtocols[i]))
      witnessConformances.push_back(conformances[i]);

  assert(protocols.size() == witnessConformances.size() &&
         "mismatched protocol conformances");

  auto &srcTI = IGF.getTypeInfoForUnlowered(srcType);
  for (unsigned i = 0, e = protocols.size(); i < e; ++i) {
    auto table = getProtocolWitnessTable(IGF, srcType, srcTI,
                                         protocols[i], witnessConformances[i]);
    body(i, table);
  }
}

#ifndef NDEBUG
static bool _isErrorType(SILType baseTy) {
  llvm::SmallVector<ProtocolDecl*, 1> protos;
  return baseTy.getSwiftRValueType()->isExistentialType(protos)
    && protos.size() == 1
    && protos[0]->getKnownProtocolKind()
    && *protos[0]->getKnownProtocolKind() == KnownProtocolKind::ErrorType;
}
#endif

/// Project the address of the value inside a boxed existential container,
/// and open an archetype to its contained type.
Address irgen::emitBoxedExistentialProjection(IRGenFunction &IGF,
                                              Explosion &base,
                                              SILType baseTy,
                                              CanArchetypeType openedArchetype){
  // TODO: Non-ErrorType boxed existentials.
  assert(_isErrorType(baseTy));
  
  // Get the reference to the existential box.
  llvm::Value *box = base.claimNext();
  // Allocate scratch space to invoke the runtime.
  Address scratch = IGF.createAlloca(IGF.IGM.Int8PtrTy,
                                     IGF.IGM.getPointerAlignment(),
                                     "project_error_scratch");
  Address out = IGF.createAlloca(IGF.IGM.OpenedErrorTripleTy,
                                 IGF.IGM.getPointerAlignment(),
                                 "project_error_out");
  
  IGF.Builder.CreateCall3(IGF.IGM.getGetErrorValueFn(), box,
                          scratch.getAddress(),
                          out.getAddress());
  // Load the 'out' values.
  auto &openedTI = IGF.getTypeInfoForLowered(openedArchetype);
  auto projectedPtrAddr = IGF.Builder.CreateStructGEP(out, 0, Size(0));
  auto projectedPtr = IGF.Builder.CreateLoad(projectedPtrAddr);
  auto projected = openedTI.getAddressForPointer(projectedPtr);
  
  auto metadataAddr = IGF.Builder.CreateStructGEP(out, 1,
                                                  IGF.IGM.getPointerSize());
  auto metadata = IGF.Builder.CreateLoad(metadataAddr);
  auto witnessAddr = IGF.Builder.CreateStructGEP(out, 2,
                                                 2 * IGF.IGM.getPointerSize());
  auto witness = IGF.Builder.CreateLoad(witnessAddr);
  
  IGF.bindArchetype(openedArchetype, metadata, witness);
  
  return projected;
}

/// Allocate a boxed existential container with uninitialized space to hold a
/// value of a given type.
Address irgen::emitBoxedExistentialContainerAllocation(IRGenFunction &IGF,
                                  Explosion &dest,
                                  SILType destType,
                                  CanType formalSrcType,
                                  SILType loweredSrcType,
                                  ArrayRef<ProtocolConformance *> conformances){
  // TODO: Non-ErrorType boxed existentials.
  assert(_isErrorType(destType));

  auto &destTI = IGF.getTypeInfo(destType).as<ErrorExistentialTypeInfo>();
  auto &srcTI = IGF.getTypeInfo(loweredSrcType);
  
  auto srcMetadata = IGF.emitTypeMetadataRef(formalSrcType);
  // Should only be one conformance, for the ErrorType protocol.
  assert(conformances.size() == 1 && destTI.getStoredProtocols().size() == 1);
  const ProtocolEntry &entry = destTI.getStoredProtocols()[0];
  auto witness = getProtocolWitnessTable(IGF, formalSrcType, srcTI,
                                         entry, conformances[0]);
  
  // Call the runtime to allocate the box.
  auto result = IGF.Builder.CreateCall2(IGF.IGM.getAllocErrorFn(),
                                        srcMetadata, witness);
  
  // Extract the box and value address from the result.
  auto box = IGF.Builder.CreateExtractValue(result, 0);
  auto addr = IGF.Builder.CreateExtractValue(result, 1);
  dest.add(box);
  
  addr = IGF.Builder.CreateBitCast(addr,
                                   srcTI.getStorageType()->getPointerTo());
  return srcTI.getAddressForPointer(addr);
}

/// Deallocate a boxed existential container with uninitialized space to hold a
/// value of a given type.
void irgen::emitBoxedExistentialContainerDeallocation(IRGenFunction &IGF,
                                                      Explosion &container,
                                                      SILType containerType,
                                                      CanType valueType) {
  // TODO: Non-ErrorType boxed existentials.
  assert(_isErrorType(containerType));

  auto box = container.claimNext();
  auto srcMetadata = IGF.emitTypeMetadataRef(valueType);
  
  IGF.Builder.CreateCall2(IGF.IGM.getDeallocErrorFn(), box, srcMetadata);
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
                               ArrayRef<ProtocolConformance*> conformances) {
  // As a special case, an ErrorType existential can represented as a reference
  // to an already existing NSError or CFError instance.
  SmallVector<ProtocolDecl*, 4> protocols;
  
  if (outType.getSwiftRValueType()->isExistentialType(protocols)
      && protocols.size() == 1) {
    switch (getSpecialProtocolID(protocols[0])) {
    case SpecialProtocol::ErrorType: {
      // Bitcast the incoming class reference to ErrorType.
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
  llvm::Value *opaqueInstance
    = IGF.Builder.CreateBitCast(instance, IGF.IGM.UnknownRefCountedPtrTy);
  out.add(opaqueInstance);

  // Emit the witness table pointers.
  forEachProtocolWitnessTable(IGF, instanceFormalType,
                              outType.getSwiftRValueType(),
                              destTI.getStoredProtocols(),
                              conformances,
                              [&](unsigned i, llvm::Value *ptable) {
    out.add(ptable);
  });
}

/// Emit an existential container initialization operation for a concrete type.
/// Returns the address of the uninitialized buffer for the concrete value.
Address irgen::emitOpaqueExistentialContainerInit(IRGenFunction &IGF,
                                  Address dest,
                                  SILType destType,
                                  CanType formalSrcType,
                                  SILType loweredSrcType,
                                  ArrayRef<ProtocolConformance*> conformances) {
  assert(!destType.isClassExistentialType() &&
         "initializing a class existential container as opaque");
  auto &destTI = IGF.getTypeInfo(destType).as<OpaqueExistentialTypeInfo>();
  auto &srcTI = IGF.getTypeInfo(loweredSrcType);
  OpaqueExistentialLayout destLayout = destTI.getLayout();
  assert(destTI.getStoredProtocols().size() == conformances.size());

  // First, write out the metadata.
  llvm::Value *metadata = IGF.emitTypeMetadataRef(formalSrcType);
  IGF.Builder.CreateStore(metadata, destLayout.projectMetadataRef(IGF, dest));

  // Compute basic layout information about the type.  If we have a
  // concrete type, we need to know how it packs into a fixed-size
  // buffer.  If we don't, we need a value witness table.
  FixedPacking packing;
  bool needValueWitnessToAllocate;
  if (!isa<FixedTypeInfo>(srcTI)) {
    packing = (FixedPacking) -1;
    needValueWitnessToAllocate = true;
  } else {
    packing = srcTI.getFixedPacking(IGF.IGM);
    needValueWitnessToAllocate = false;
  }

  // Next, write the protocol witness tables.
  forEachProtocolWitnessTable(IGF, formalSrcType, destType.getSwiftRValueType(),
                              destTI.getStoredProtocols(), conformances,
                              [&](unsigned i, llvm::Value *ptable) {
    Address ptableSlot = destLayout.projectWitnessTable(IGF, dest, i);
    IGF.Builder.CreateStore(ptable, ptableSlot);
  });

  // Finally, evaluate into the buffer.

  // Project down to the destination fixed-size buffer.
  Address buffer = destLayout.projectExistentialBuffer(IGF, dest);

  // If the type is provably empty, we're done.
  if (srcTI.isKnownEmpty()) {
    assert(packing == FixedPacking::OffsetZero);
    return buffer;
  }

  // Otherwise, allocate if necessary.

  if (needValueWitnessToAllocate) {
    // If we're using a witness-table to do this, we need to emit a
    // value-witness call to allocate the fixed-size buffer.
    return Address(emitAllocateBufferCall(IGF, loweredSrcType, buffer),
                   Alignment(1));
  } else {
    // Otherwise, allocate using what we know statically about the type.
    return emitAllocateBuffer(IGF, loweredSrcType,
                              srcTI, packing, buffer);
  }
}

/// Emit an existential metatype container from a metatype value
/// as an explosion.
void irgen::emitExistentialMetatypeContainer(IRGenFunction &IGF,
                               Explosion &out, SILType outType,
                               llvm::Value *metatype, SILType metatypeType,
                               ArrayRef<ProtocolConformance*> conformances) {
  assert(outType.is<ExistentialMetatypeType>());
  auto &destTI = IGF.getTypeInfo(outType).as<ExistentialMetatypeTypeInfo>();
  out.add(metatype);

  auto srcType = metatypeType.castTo<MetatypeType>().getInstanceType();
  auto destType = outType.castTo<ExistentialMetatypeType>().getInstanceType();
  while (auto destMetatypeType = dyn_cast<ExistentialMetatypeType>(destType)) {
    destType = destMetatypeType.getInstanceType();
    srcType = cast<MetatypeType>(srcType).getInstanceType();
  }

  // Emit the witness table pointers.
  forEachProtocolWitnessTable(IGF, srcType, destType,
                              destTI.getStoredProtocols(),
                              conformances,
                              [&](unsigned i, llvm::Value *ptable) {
                                out.add(ptable);
                              });
}

void
irgen::emitWitnessMethodValue(IRGenFunction &IGF,
                              CanType baseTy,
                              SILDeclRef member,
                              ProtocolConformance *conformance,
                              Explosion &out) {
  auto fn = cast<AbstractFunctionDecl>(member.getDecl());

  // The protocol we're calling on.
  ProtocolDecl *fnProto = cast<ProtocolDecl>(fn->getDeclContext());

  // Find the witness table.
  // FIXME conformance for concrete type
  auto &baseTI = IGF.getTypeInfoForUnlowered(baseTy);
  auto &fnProtoInfo = IGF.IGM.getProtocolInfo(fnProto);
  llvm::Value *wtable = emitWitnessTableRef(IGF, baseTy, baseTI,
                                            fnProto,
                                            fnProtoInfo,
                                            conformance);

  // Find the witness we're interested in.
  auto index = fnProtoInfo.getWitnessEntry(fn).getFunctionIndex();
  llvm::Value *witness = emitLoadOfOpaqueWitness(IGF, wtable, index);
  
  // Cast the witness pointer to i8*.
  witness = IGF.Builder.CreateBitCast(witness, IGF.IGM.Int8PtrTy);
  
  // Build the value.
  out.add(witness);
}

llvm::Value *irgen::emitDynamicTypeOfOpaqueArchetype(IRGenFunction &IGF,
                                                     Address addr,
                                                     SILType type) {
  auto archetype = type.castTo<ArchetypeType>();

  // Acquire the archetype's static metadata.
  llvm::Value *metadata = IGF.getLocalTypeData(archetype,
                                               LocalTypeData::forMetatype());
  return IGF.Builder.CreateCall2(IGF.IGM.getGetDynamicTypeFn(),
                                 addr.getAddress(), metadata);
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
  llvm::Value *object = emitProjectBufferCall(IGF, metadata, buffer);
  llvm::Value *dynamicType =
    IGF.Builder.CreateCall2(IGF.IGM.getGetDynamicTypeFn(),
                            object, metadata);
  out.add(dynamicType);

  // Get the witness tables.
  baseTI.emitLoadOfTables(IGF, addr, out);
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
    llvm::Value *object = emitProjectBufferCall(IGF, metadata, buffer);

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
