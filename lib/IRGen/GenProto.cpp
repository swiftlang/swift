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

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Module.h"

#include "Cleanup.h"
#include "Explosion.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"

#include "GenProto.h"

using namespace swift;
using namespace irgen;

/// A fixed-size buffer is always 16 bytes and pointer-aligned.
/// If we align them more, we'll need to introduce padding to
/// make protocol types work.
static Size getFixedBufferSize(IRGenModule &IGM) {
  return Size(16);
}
static Alignment getFixedBufferAlignment(IRGenModule &IGM) {
  return IGM.getPointerAlignment();
}

/// Lazily create the standard fixed-buffer type.
llvm::Type *IRGenModule::getFixedBufferTy() {
  if (FixedBufferTy) return FixedBufferTy;

  auto size = getFixedBufferSize(*this).getValue();
  FixedBufferTy = llvm::ArrayType::get(Int8Ty, size);
  return FixedBufferTy;
}

/// A witness table is an i8**.
static llvm::PointerType *getWitnessTableTy(IRGenModule &IGM) {
  return IGM.Int8PtrTy->getPointerTo(0);
}

static llvm::StructType *getSizeAndAlignmentResultType(IRGenModule &IGM) {
  llvm::Type *resultElts[] = { IGM.SizeTy, IGM.SizeTy };
  return llvm::StructType::get(IGM.getLLVMContext(), resultElts, false);
}

static llvm::FunctionType *createWitnessFunctionType(IRGenModule &IGM,
                                                     ValueWitness index) {
  switch (index) {

  // void (*deallocateBuffer)(B *buffer, W *self);
  // void (*destroyBuffer)(B *buffer, W *self);
  case ValueWitness::DeallocateBuffer:
  case ValueWitness::DestroyBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // void (*destroy)(T *object, witness_t *self);
  case ValueWitness::Destroy: {
    llvm::Type *args[] = { IGM.Int8PtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.VoidTy, args, false);
  }

  // T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, W *self);
  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*allocateBuffer)(B *buffer, W *self);
  // T *(*projectBuffer)(B *buffer, W *self);
  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*initializeBufferWithCopy)(B *dest, T *src, W *self);
  // T *(*initializeBufferWithTake)(B *dest, T *src, W *self);
  case ValueWitness::InitializeBufferWithCopy:
  case ValueWitness::InitializeBufferWithTake: {
    llvm::Type *bufPtrTy = IGM.getFixedBufferTy()->getPointerTo(0);
    llvm::Type *args[] = { bufPtrTy, IGM.Int8PtrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(IGM.Int8PtrTy, args, false);
  }

  // T *(*assignWithCopy)(T *dest, T *src, W *self);
  // T *(*assignWithTake)(T *dest, T *src, W *self);
  // T *(*initializeWithCopy)(T *dest, T *src, W *self);
  // T *(*initializeWithTake)(T *dest, T *src, W *self);
  case ValueWitness::AssignWithCopy:
  case ValueWitness::AssignWithTake:
  case ValueWitness::InitializeWithCopy:
  case ValueWitness::InitializeWithTake: {
    llvm::Type *ptrTy = IGM.Int8PtrTy;
    llvm::Type *args[] = { ptrTy, ptrTy, getWitnessTableTy(IGM) };
    return llvm::FunctionType::get(ptrTy, args, false);
  }

  // typedef struct { size_t Size; size_t Align } layout_t;
  // layout_t (*sizeAndAlignment)(W *self);
  case ValueWitness::SizeAndAlignment: {
    llvm::StructType *resultTy = getSizeAndAlignmentResultType(IGM);
    return llvm::FunctionType::get(resultTy, getWitnessTableTy(IGM), false);
  }

  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::PointerType *IRGenModule::getValueWitnessTy(ValueWitness index) {
  static_assert(IRGenModule::NumValueWitnesses == ::NumValueWitnesses,
                "array on IGM has the wrong size");

  auto &ty = ValueWitnessTys[unsigned(index)];
  if (ty) return ty;

  ty = createWitnessFunctionType(*this, index)->getPointerTo(0);
  return ty;
}

static llvm::StringRef getValueWitnessLabel(ValueWitness index) {
  switch (index) {
  case ValueWitness::DeallocateBuffer:
    return "deallocateBuffer";
  case ValueWitness::DestroyBuffer:
    return "destroyBuffer";
  case ValueWitness::Destroy:
    return "destroy";
  case ValueWitness::InitializeBufferWithCopyOfBuffer:
    return "initializeBufferWithCopyOfBuffer";
  case ValueWitness::AllocateBuffer:
    return "allocateBuffer";
  case ValueWitness::ProjectBuffer:
    return "projectBuffer";
  case ValueWitness::InitializeBufferWithCopy:
    return "initializeBufferWithCopy";
  case ValueWitness::InitializeBufferWithTake:
    return "initializeBufferWithTake";
  case ValueWitness::AssignWithCopy:
    return "assignWithCopy";
  case ValueWitness::AssignWithTake:
    return "assignWithTake";
  case ValueWitness::InitializeWithCopy:
    return "initializeWithCopy";
  case ValueWitness::InitializeWithTake:
    return "initializeWithTake";
  case ValueWitness::SizeAndAlignment:
    return "sizeAndAlignment";
  }
  llvm_unreachable("bad value witness index");
}

namespace {
  /// A class which encapsulates an index into a witness table.
  class WitnessIndex {
    unsigned Value;
  public:
    WitnessIndex() = default;
    WitnessIndex(ValueWitness index) : Value(unsigned(index)) {}
    explicit WitnessIndex(unsigned index) : Value(index) {}

    unsigned getValue() const { return Value; }
  };
}

/// Given the offset of the buffer within an existential type.
static Size getBufferOffset(IRGenModule &IGM) {
  return IGM.getPointerSize();
}

/// Given the address of an existential object, drill down to the
/// buffer.
static Address projectExistentialBuffer(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateStructGEP(addr, 1, getBufferOffset(IGF.IGM));
}

/// Given the address of an existential object, drill down to the
/// witness-table field.
static Address projectWitnessTable(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateStructGEP(addr, 0, Size(0));
}

/// Given the address of an existential object, load its witness table.
static llvm::Value *loadWitnessTable(IRGenFunction &IGF, Address addr) {
  return IGF.Builder.CreateLoad(projectWitnessTable(IGF, addr),
                                "witness-table");
}

/// Load a specific witness from a known table.  The result is
/// always an i8*.
static llvm::Value *loadOpaqueWitness(IRGenFunction &IGF,
                                      llvm::Value *table,
                                      WitnessIndex index) {
  assert(table->getType() == IGF.IGM.Int8PtrTy->getPointerTo());

  // GEP to the appropriate index, avoiding spurious IR in the trivial case.
  llvm::Value *slot = table;
  if (index.getValue() != 0)
    slot = IGF.Builder.CreateConstInBoundsGEP1_32(table, index.getValue());

  llvm::Value *witness =
    IGF.Builder.CreateLoad(Address(slot, IGF.IGM.getPointerAlignment()));
  return witness;
}

/// Given a witness table, load one of the value witnesses.
/// The result has the appropriate type for the witness.
static llvm::Value *loadValueWitness(IRGenFunction &IGF,
                                     llvm::Value *table,
                                     ValueWitness index) {
  llvm::Value *witness = loadOpaqueWitness(IGF, table, index);
  return IGF.Builder.CreateBitCast(witness, IGF.IGM.getValueWitnessTy(index),
                                   getValueWitnessLabel(index));
}

/// Given a call to a helper function that produces a result
/// into its first argument, set attributes appropriately.
static void setHelperAttributesForAggResult(llvm::CallInst *call,
                                            bool isFormalResult = true) {
  llvm::SmallVector<llvm::AttributeWithIndex, 2> attrs;

  // Don't set 'sret' unless this is also the formal result.
  auto resultAttrs = llvm::Attribute::NoAlias;
  if (isFormalResult) resultAttrs = resultAttrs | llvm::Attribute::StructRet;
  attrs.push_back(llvm::AttributeWithIndex::get(1, resultAttrs));

  // Set as nounwind.
  attrs.push_back(llvm::AttributeWithIndex::get(~0,
                                llvm::Attribute::NoUnwind));

  call->setAttributes(llvm::AttrListPtr::get(attrs.data(), attrs.size()));
}

/// Given a call to a helper function, set attributes appropriately.
static void setHelperAttributes(llvm::CallInst *call) {
  // Set as nounwind.
  llvm::SmallVector<llvm::AttributeWithIndex, 1> attrs;
  attrs.push_back(llvm::AttributeWithIndex::get(~0,
                                llvm::Attribute::NoUnwind));

  call->setAttributes(llvm::AttrListPtr::get(attrs.data(), attrs.size()));
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
static void emitInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                                 llvm::Value *witnessTable,
                                                 Address destBuffer,
                                                 Address srcBuffer) {
  llvm::Value *copyFn = loadValueWitness(IGF, witnessTable,
                             ValueWitness::InitializeBufferWithCopyOfBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall3(copyFn, destBuffer.getAddress(),
                            srcBuffer.getAddress(), witnessTable);
  setHelperAttributesForAggResult(call, false);
}                                            

/// Emit a 'destroyBuffer' operation.
static void emitDestroyBuffer(IRGenFunction &IGF,
                              llvm::Value *witnessTable,
                              Address buffer) {
  llvm::Value *fn = loadValueWitness(IGF, witnessTable,
                                     ValueWitness::DestroyBuffer);
  llvm::CallInst *call =
    IGF.Builder.CreateCall2(fn, buffer.getAddress(), witnessTable);
  setHelperAttributes(call);
}

/// Given the address of an existential object, destroy it.
static void emitDestroyExistential(IRGenFunction &IGF, Address addr) {
  llvm::Value *table = loadWitnessTable(IGF, addr);
  emitDestroyBuffer(IGF, table, projectExistentialBuffer(IGF, addr));
}

namespace {
  struct DestroyExistential : Cleanup {
    Address Addr;
    DestroyExistential(Address addr) : Addr(addr) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyExistential(IGF, Addr);
    }
  };

  class ConformanceInfo;

  /// A witness to a specific element of a protocol.  Every
  /// ProtocolTypeInfo stores one of these for each declaration in the
  /// protocol.
  /// 
  /// The structure of a witness varies by the type of declaration:
  ///   - a function requires a single witness, the function;
  ///   - a variable requires two witnesses, a getter and a setter;
  ///   - a subscript requires two witnesses, a getter and a setter;
  ///   - a type requires a pointer to a witness for that type and
  ///     the protocols it obeys.
  struct WitnessTableEntry {
    Decl *Member;
    WitnessIndex BeginIndex;
  };

  /// A class for laying out a witness table.
  class WitnessTableLayout {
    unsigned NumWitnesses;
    SmallVector<WitnessTableEntry, 16> Entries;

  public:
    WitnessTableLayout(ProtocolDecl *P) : NumWitnesses(NumValueWitnesses) {
      for (Decl *member : P->getMembers())
        addMember(member);
      finalize();
    }

    unsigned getNumWitnesses() const { return NumWitnesses; }
    ArrayRef<WitnessTableEntry> getEntries() const { return Entries; }

  private:
    void addMember(Decl *member);
    void finalize() { /*do nothing, for now*/ }
  };

  class ProtocolTypeInfo : public TypeInfo {
    /// The number of witnesses in this protocol.
    unsigned NumWitnesses;

    /// The number of WitnessTableEntry objects stored here.
    unsigned NumTableEntries;

    /// A table of all the conformance infos we've registered with
    /// this type.
    mutable llvm::DenseMap<ProtocolConformance*, ConformanceInfo*>
      Conformances;

    ProtocolTypeInfo(llvm::Type *ty, Size size, Alignment align,
                     unsigned numWitnesses, unsigned numEntries)
      : TypeInfo(ty, size, align, IsNotPOD),
        NumWitnesses(numWitnesses), NumTableEntries(numEntries) {}

    WitnessTableEntry *getEntriesBuffer() {
      return reinterpret_cast<WitnessTableEntry*>(this+1);
    }
    const WitnessTableEntry *getEntriesBuffer() const {
      return reinterpret_cast<const WitnessTableEntry*>(this+1);
    }

  public:
    ~ProtocolTypeInfo();

    static const ProtocolTypeInfo *create(llvm::Type *ty, Size size,
                                          Alignment align,
                                          const WitnessTableLayout &layout) {
      unsigned numEntries = layout.getEntries().size();
      size_t bufferSize = sizeof(ProtocolTypeInfo)
                        + numEntries * sizeof(WitnessTableEntry);
      void *buffer = ::operator new(bufferSize);
      auto result = new(buffer) ProtocolTypeInfo(ty, size, align,
                                                 layout.getNumWitnesses(),
                                                 numEntries);

      memcpy(result->getEntriesBuffer(), layout.getEntries().data(),
             numEntries * sizeof(WitnessTableEntry));

      return result;
    }

    const ConformanceInfo &getConformance(IRGenModule &IGM,
                                          Type concreteType,
                                          const TypeInfo &concreteTI,
                                          ProtocolConformance *conf) const;

    /// Return the number of witnesses required by this protocol
    /// beyond the basic value-type witnesses.
    unsigned getNumTableEntries() const { return NumTableEntries; }

    ArrayRef<WitnessTableEntry> getTableEntries() const {
      return ArrayRef<WitnessTableEntry>(getEntriesBuffer(),
                                         getNumTableEntries());
    }

    const WitnessTableEntry &getTableEntry(Decl *member) {
      // FIXME: do a binary search if the number of witnesses is large
      // enough.
      for (auto &witness : getTableEntries())
        if (witness.Member == member)
          return witness;
      llvm_unreachable("didn't find entry for member!");
    }

    /// Create an uninitialized existential object.
    Address createTemporary(IRGenFunction &IGF) const {
      return IGF.createAlloca(getStorageType(), StorageAlignment,
                              "protocol.temporary");
    }

    static ManagedValue enterCleanupForTemporary(IRGenFunction &IGF,
                                                 Address dest) {
      IGF.pushFullExprCleanup<DestroyExistential>(dest);
      return ManagedValue(dest.getAddress(), IGF.getCleanupsDepth());
    }

    void getSchema(ExplosionSchema &schema) const {
      schema.add(ExplosionSchema::Element::forAggregate(getStorageType(),
                                                        StorageAlignment));
    }
    unsigned getExplosionSize(ExplosionKind kind) const { return 1; }

    void load(IRGenFunction &IGF, Address src, Explosion &out) const {
      // Create a temporary.
      Address dest = createTemporary(IGF);

      // Initialize it with a copy of the source.
      ProtocolTypeInfo::initializeWithCopy(IGF, dest, src);

      // Enter a cleanup for the temporary.
      out.add(enterCleanupForTemporary(IGF, dest));
    }

    void loadAsTake(IRGenFunction &IGF, Address src, Explosion &out) const {
      // Create a temporary and memcpy into it, then enter a cleanup
      // to destroy that.
      Address dest = createTemporary(IGF);
      IGF.emitMemCpy(dest, src, StorageSize);
      out.add(enterCleanupForTemporary(IGF, dest));
    }

    void assign(IRGenFunction &IGF, Explosion &in, Address dest) const {
      // TODO: by this point, we've aleady lost some interesting
      // dynamic optimization opportunities because of the implicit
      // copy into a temporary to form the explosion.

      // Destroy the old value.  This is safe because the value in the
      // explosion is already +1, so even if there's any aliasing
      // going on, we're totally fine.
      emitDestroyExistential(IGF, dest);

      // Take the new value.
      ProtocolTypeInfo::initialize(IGF, in, dest);
    }

    void initialize(IRGenFunction &IGF, Explosion &in, Address dest) const {
      // Take ownership of the temporary and memcpy it into place.
      llvm::Value *src = in.forwardNext(IGF);
      IGF.emitMemCpy(dest, Address(src, StorageAlignment), StorageSize);
    }

    void initializeWithCopy(IRGenFunction &IGF,
                            Address dest, Address src) const {
      // Load the witness table and copy it into the new object.
      llvm::Value *table = loadWitnessTable(IGF, src);
      IGF.Builder.CreateStore(table, projectWitnessTable(IGF, dest));

      // Project down to the buffers and ask the witnesses to do a
      // copy-initialize.
      Address srcBuffer = projectExistentialBuffer(IGF, src);
      Address destBuffer = projectExistentialBuffer(IGF, dest);
      emitInitializeBufferWithCopyOfBuffer(IGF, table, destBuffer, srcBuffer);
    }

    void reexplode(IRGenFunction &IGF, Explosion &src, Explosion &dest) const {
      dest.add(src.claimNext());
    }

    void copy(IRGenFunction &IGF, Explosion &in, Explosion &out) const {
      auto srcManaged = in.claimNext();
      Address src(srcManaged.getValue(), StorageAlignment);
      ProtocolTypeInfo::load(IGF, src, out);
      // Force the cleanup here?
    }

    void manage(IRGenFunction &IGF, Explosion &in, Explosion &out) const {
      llvm::Value *srcAddr = in.claimUnmanagedNext();
      out.add(enterCleanupForTemporary(IGF,
                                       Address(srcAddr, StorageAlignment)));
    }

    void destroy(IRGenFunction &IGF, Address addr) const {
      emitDestroyExistential(IGF, addr);
    }
  };

  /// Ways in which an object can fit into a fixed-size buffer.
  enum class FixedPacking {
    /// It fits at offset zero.
    OffsetZero,

    /// It'll fit if it gets realigned.
    Realign,

    /// It doesn't fit and needs to be side-allocated.
    Allocate

    // Resilience: it needs to be checked dynamically.
  };

  /// Detail about how an object conforms to a protocol.
  class ConformanceInfo {
    friend class ProtocolTypeInfo;

    /// The pointer to the table.  In practice, it's not really
    /// reasonable for this to be always be a constant!  It probably
    /// needs to be managed by the runtime, and the information stored
    /// here would just indicate how to find the actual thing.
    llvm::Constant *Table;

    FixedPacking Packing;

  public:
    llvm::Value *getTable(IRGenFunction &IGF) const {
      return Table;
    }

    FixedPacking getPacking() const {
      return Packing;
    }
  };
}

ProtocolTypeInfo::~ProtocolTypeInfo() {
  for (auto &conf : Conformances) {
    delete conf.second;
  }
}

/// Add a member to the witness-table layout.
void WitnessTableLayout::addMember(Decl *member) {
  // FIXME
}

static FixedPacking computePacking(IRGenModule &IGM,
                                   const TypeInfo &concreteTI) {
  Size bufferSize = getFixedBufferSize(IGM);
  Size requiredSize = concreteTI.StorageSize;

  // Flat out, if we need more space than the buffer provides,
  // we always have to allocate.
  // FIXME: there might be some interesting cases where this
  // is suboptimal for oneofs.
  if (requiredSize > bufferSize)
    return FixedPacking::Allocate;

  Alignment bufferAlign = getFixedBufferAlignment(IGM);
  Alignment requiredAlign = concreteTI.StorageAlignment;

  // If the buffer alignment is good enough for the type, great.
  if (bufferAlign >= requiredAlign)
    return FixedPacking::OffsetZero;

  // We can always reliably realign as long as the max difference
  // between the alignments doesn't exceed the spare room.
  if (requiredSize + Size(requiredAlign.getValue() - bufferAlign.getValue())
        <= bufferSize)
    return FixedPacking::Realign;

  // TODO: consider using a slower mode that dynamically checks
  // whether the buffer size is small enough.

  // Otherwise we're stuck and have to separately allocate.
  return FixedPacking::Allocate;
}

static bool isNeverAllocated(FixedPacking packing) {
  switch (packing) {
  case FixedPacking::OffsetZero: return true;
  case FixedPacking::Realign: return true;
  case FixedPacking::Allocate: return false;
  }
  llvm_unreachable("bad FixedPacking value");
}

/// Emit a 'projectBuffer' operation.  Always returns a T*.
static Address emitProjectBuffer(IRGenFunction &IGF,
                                 Address buffer,
                                 FixedPacking packing,
                                 const TypeInfo &type) {
  llvm::PointerType *resultTy = type.getStorageType()->getPointerTo();
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot = IGF.Builder.CreateBitCast(buffer, resultTy->getPointerTo(),
                                             "storage-slot");
    llvm::Value *address = IGF.Builder.CreateLoad(slot);
    return Address(address, type.StorageAlignment);
  }

  case FixedPacking::OffsetZero: {
    return IGF.Builder.CreateBitCast(buffer, resultTy, "object");
  }
    
  case FixedPacking::Realign: {
    // Round the buffer pointer up to the required alignment:

    // Convert the buffer to an integer.
    llvm::Value *addr = buffer.getAddress();
    addr = IGF.Builder.CreatePtrToInt(addr, IGF.IGM.SizeTy);

    // Add one less than the alignment.
    llvm::Value *align = type.getAlignmentOnly(IGF);
    llvm::Value *alignMask =
      IGF.Builder.CreateSub(align,
                            llvm::ConstantInt::get(IGF.IGM.SizeTy, 1),
                            "alignMask");
    addr = IGF.Builder.CreateAdd(addr, alignMask);

    // Mask off the low bits.
    alignMask = IGF.Builder.CreateNot(alignMask);
    addr = IGF.Builder.CreateAnd(addr, alignMask, "addr-rounded");

    addr = IGF.Builder.CreateIntToPtr(addr, resultTy, "object");
    return Address(addr, type.StorageAlignment);
  }
  }
  llvm_unreachable("bad packing!");
  
}

/// Emit an 'allocateBuffer' operation.  Always returns a T*.
static Address emitAllocateBuffer(IRGenFunction &IGF,
                                  Address buffer,
                                  FixedPacking packing,
                                  const TypeInfo &type) {
  switch (packing) {
  case FixedPacking::Allocate: {
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);
    llvm::Value *addr =
      IGF.emitAllocRawCall(sizeAndAlign.first, sizeAndAlign.second);
    buffer = IGF.Builder.CreateBitCast(buffer,
                                       IGF.IGM.Int8PtrTy->getPointerTo());
    IGF.Builder.CreateStore(addr, buffer);
    return IGF.Builder.CreateBitCast(Address(addr, type.StorageAlignment),
                                     type.getStorageType()->getPointerTo());
  }

  case FixedPacking::OffsetZero:
  case FixedPacking::Realign:
    return emitProjectBuffer(IGF, buffer, packing, type);
  }
  llvm_unreachable("bad packing!");
}

/// Emit an 'assignWithCopy' operation.
static void emitAssignWithCopy(IRGenFunction &IGF,
                               Address src, Address dest,
                               const TypeInfo &type) {
  Explosion value(ExplosionKind::Maximal);
  type.load(IGF, src, value);
  type.assign(IGF, value, dest);
}

/// Emit an 'assignWithTake' operation.
static void emitAssignWithTake(IRGenFunction &IGF,
                               Address src, Address dest,
                               const TypeInfo &type) {
  Explosion value(ExplosionKind::Maximal);
  type.loadAsTake(IGF, src, value);
  type.assign(IGF, value, dest);
}

/// Emit a 'deallocateBuffer' operation.
static void emitDeallocateBuffer(IRGenFunction &IGF,
                                 Address buffer,
                                 FixedPacking packing,
                                 const TypeInfo &type) {
  switch (packing) {
  case FixedPacking::Allocate: {
    Address slot =
      IGF.Builder.CreateBitCast(buffer, IGF.IGM.Int8PtrTy->getPointerTo());
    llvm::Value *addr = IGF.Builder.CreateLoad(slot, "storage");
    IGF.emitDeallocRawCall(addr, type.getSizeOnly(IGF));
    return;
  }

  case FixedPacking::OffsetZero:
  case FixedPacking::Realign:
    return;
  }
  llvm_unreachable("bad packing!");
}

/// Emit a 'destroyObject' operation.
static void emitDestroyObject(IRGenFunction &IGF,
                              Address object,
                              const TypeInfo &type) {
  if (!type.isPOD(ResilienceScope::Local))
    type.destroy(IGF, object);
}

/// Emit a 'destroyBuffer' operation.
static void emitDestroyBuffer(IRGenFunction &IGF,
                              Address buffer,
                              FixedPacking packing,
                              const TypeInfo &type) {
  Address object = emitProjectBuffer(IGF, buffer, packing, type);
  emitDestroyObject(IGF, object, type);
  emitDeallocateBuffer(IGF, buffer, packing, type);
}

/// Emit an 'initializeWithCopy' operation.
static void emitInitializeWithCopy(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   const TypeInfo &type) {
  type.initializeWithCopy(IGF, dest, src);
}

/// Emit an 'initializeWithTake' operation.
static void emitInitializeWithTake(IRGenFunction &IGF,
                                   Address dest, Address src,
                                   const TypeInfo &type) {
  type.initializeWithTake(IGF, dest, src);
}

/// Emit an 'initializeBufferWithCopyOfBuffer' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopyOfBuffer(IRGenFunction &IGF,
                                                    Address dest,
                                                    Address src,
                                                    FixedPacking packing,
                                                    const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  Address srcObject = emitProjectBuffer(IGF, src, packing, type);
  emitInitializeWithCopy(IGF, destObject, srcObject, type);
  return destObject;
}

/// Emit an 'initializeBufferWithCopy' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithCopy(IRGenFunction &IGF,
                                            Address dest,
                                            Address srcObject,
                                            FixedPacking packing,
                                            const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  emitInitializeWithCopy(IGF, destObject, srcObject, type);
  return destObject;
}

/// Emit an 'initializeBufferWithTake' operation.
/// Returns the address of the destination object.
static Address emitInitializeBufferWithTake(IRGenFunction &IGF,
                                            Address dest,
                                            Address srcObject,
                                            FixedPacking packing,
                                            const TypeInfo &type) {
  Address destObject = emitAllocateBuffer(IGF, dest, packing, type);
  emitInitializeWithTake(IGF, destObject, srcObject, type);
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
  return Address(result, type.StorageAlignment);
}

/// Get the next argument as a pointer to the given storage type.
static Address getArgAsBuffer(IRGenFunction &IGF,
                              llvm::Function::arg_iterator &it,
                              StringRef name) {
  llvm::Value *arg = getArg(it, name);
  return Address(arg, getFixedBufferAlignment(IGF.IGM));
}

/// Build a specific value-witness function.
static void buildValueWitness(IRGenModule &IGM,
                              llvm::Function *fn,
                              ValueWitness index,
                              FixedPacking packing,
                              const TypeInfo &type) {
  IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                    ExplosionKind::Minimal, 0, fn, Prologue::Bare);

  auto argv = fn->arg_begin();
  switch (index) {
  case ValueWitness::AllocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitAllocateBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::AssignWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithCopy(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::AssignWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitAssignWithTake(IGF, src, dest, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::DeallocateBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    emitDeallocateBuffer(IGF, buffer, packing, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::Destroy: {
    Address object = getArgAs(IGF, argv, type, "object");
    emitDestroyObject(IGF, object, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::DestroyBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    emitDestroyBuffer(IGF, buffer, packing, type);
    IGF.Builder.CreateRetVoid();
    return;
  }

  case ValueWitness::InitializeBufferWithCopyOfBuffer: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAsBuffer(IGF, argv, "src");
    Address result =
      emitInitializeBufferWithCopyOfBuffer(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithCopy: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithCopy(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeBufferWithTake: {
    Address dest = getArgAsBuffer(IGF, argv, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    Address result =
      emitInitializeBufferWithTake(IGF, dest, src, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::InitializeWithCopy: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithCopy(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::InitializeWithTake: {
    Address dest = getArgAs(IGF, argv, type, "dest");
    Address src = getArgAs(IGF, argv, type, "src");
    emitInitializeWithTake(IGF, dest, src, type);
    dest = IGF.Builder.CreateBitCast(dest, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(dest.getAddress());
    return;
  }

  case ValueWitness::ProjectBuffer: {
    Address buffer = getArgAsBuffer(IGF, argv, "buffer");
    Address result = emitProjectBuffer(IGF, buffer, packing, type);
    result = IGF.Builder.CreateBitCast(result, IGF.IGM.Int8PtrTy);
    IGF.Builder.CreateRet(result.getAddress());
    return;
  }

  case ValueWitness::SizeAndAlignment: {
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);

    llvm::Type *pairTy = getSizeAndAlignmentResultType(IGF.IGM);
    llvm::Value *result = llvm::UndefValue::get(pairTy);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.first, 0);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.second, 1);
    IGF.Builder.CreateRet(result);
    return;
  }

  }
  llvm_unreachable("bad value witness kind!");
}

static llvm::Constant *asOpaquePtr(IRGenModule &IGM, llvm::Constant *in) {
  return llvm::ConstantExpr::getBitCast(in, IGM.Int8PtrTy);
}

/// Should we be defining the given helper function?
static llvm::Function *shouldDefineHelper(llvm::Constant *fn) {
  llvm::Function *def = dyn_cast<llvm::Function>(fn);
  if (!def) return nullptr;
  if (!def->empty()) return nullptr;

  def->setLinkage(llvm::Function::LinkOnceODRLinkage);
  def->setVisibility(llvm::Function::HiddenVisibility);
  return def;
}

/// Return a function which takes two pointer arguments and returns
/// void immediately.
static llvm::Constant *getNoOpVoidFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_void_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    llvm::ReturnInst::Create(IGM.getLLVMContext(), entry);
  }
  return fn;
}

/// Return a function which takes two pointer arguments and returns
/// the first one immediately.
static llvm::Constant *getReturnSelfFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_noop_self_return", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(IGM.getLLVMContext(), "entry", def);
    llvm::ReturnInst::Create(IGM.getLLVMContext(),
                             def->arg_begin(), entry);
  }
  return fn;
}

/// Return a function which takes three pointer arguments and does a
/// retaining assignWithCopy on the first two: it loads a pointer from
/// the second, retains it, loads a pointer from the first, stores the
/// new pointer in the first, and releases the old pointer.
static llvm::Constant *getAssignWithCopyStrongFunction(IRGenModule &IGM) {
  llvm::Type *ptrPtrTy = IGM.RefCountedPtrTy->getPointerTo();
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    newValue = IGF.emitRetainCall(newValue);
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
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_assignWithTake_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
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
  llvm::Type *argTys[] = { ptrPtrTy, ptrPtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(ptrPtrTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_initWithCopy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, IGM.getPointerAlignment());
    Address src(it++, IGM.getPointerAlignment());

    llvm::Value *newValue = IGF.Builder.CreateLoad(src, "new");
    newValue = IGF.emitRetainCall(newValue);
    IGF.Builder.CreateStore(newValue, dest);

    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes two pointer arguments, loads a
/// pointer from the first, and calls swift_release on it immediately.
static llvm::Constant *getDestroyStrongFunction(IRGenModule &IGM) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy->getPointerTo(), IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.VoidTy, argTys, false);
  llvm::Constant *fn =
    IGM.Module.getOrInsertFunction("__swift_destroy_strong", fnTy);

  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    Address arg(def->arg_begin(), IGM.getPointerAlignment());
    IGF.emitRelease(IGF.Builder.CreateLoad(arg));
    IGF.Builder.CreateRetVoid();
  }
  return fn;
}

/// Return a function which takes three pointer arguments, memcpys
/// from the second to the first, and returns the first argument.
static llvm::Constant *getMemCpyFunction(IRGenModule &IGM,
                                         const TypeInfo &type) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy, IGM.Int8PtrTy, IGM.Int8PtrTy };
  llvm::FunctionType *fnTy =
    llvm::FunctionType::get(IGM.Int8PtrTy, argTys, false);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_memcpy";
    nameStream << type.StorageSize.getValue();
    nameStream << '_';
    nameStream << type.StorageAlignment.getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto it = def->arg_begin();
    Address dest(it++, type.StorageAlignment);
    Address src(it++, type.StorageAlignment);
    IGF.emitMemCpy(dest, src, type.StorageSize);
    IGF.Builder.CreateRet(dest.getAddress());
  }
  return fn;
}

/// Return a function which takes one pointer argument and returns a
/// constant size and alignment.
static llvm::Constant *getSizeAndAlignmentFunction(IRGenModule &IGM,
                                                   const TypeInfo &type) {
  llvm::Type *argTys[] = { IGM.Int8PtrTy };
  llvm::Type *resultTy = getSizeAndAlignmentResultType(IGM);
  llvm::FunctionType *fnTy = llvm::FunctionType::get(resultTy, argTys, false);

  // We need to unique by both size and alignment.  Note that we're
  // assuming that it's safe to call a function that returns a pointer
  // at a site that assumes the function returns void.
  llvm::SmallString<40> name;
  {
    llvm::raw_svector_ostream nameStream(name);
    nameStream << "__swift_sizeAndAlignment_";
    nameStream << type.StorageSize.getValue();
    nameStream << '_';
    nameStream << type.StorageAlignment.getValue();
  }

  llvm::Constant *fn = IGM.Module.getOrInsertFunction(name, fnTy);
  if (llvm::Function *def = shouldDefineHelper(fn)) {
    IRGenFunction IGF(IGM, Type(), ArrayRef<Pattern*>(),
                      ExplosionKind::Minimal, 0, def, Prologue::Bare);
    auto sizeAndAlign = type.getSizeAndAlignment(IGF);
    assert(isa<llvm::ConstantInt>(sizeAndAlign.first));
    assert(isa<llvm::ConstantInt>(sizeAndAlign.second));

    llvm::Value *result = llvm::UndefValue::get(resultTy);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.first, 0);
    result = IGF.Builder.CreateInsertValue(result, sizeAndAlign.second, 1);
    IGF.Builder.CreateRet(result);
  }
  return fn;
}


/// Find a witness to the fact that a type is a value type.
/// Always returns an i8*.
static llvm::Constant *getValueWitness(IRGenModule &IGM,
                                       ValueWitness index,
                                       FixedPacking packing,
                                       Type concreteType,
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
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      assert(isNeverAllocated(packing));
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::Destroy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getNoOpVoidFunction(IGM));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getDestroyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeBufferWithCopyOfBuffer:
  case ValueWitness::InitializeBufferWithCopy:
    if (packing == FixedPacking::OffsetZero) {
      if (concreteTI.isPOD(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
      } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
        return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
      }
    }
    goto standard;

  case ValueWitness::InitializeBufferWithTake:
    if (packing == FixedPacking::OffsetZero)    
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    goto standard;

  case ValueWitness::InitializeWithTake:
    return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));

  case ValueWitness::AssignWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AssignWithTake:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getAssignWithTakeStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::InitializeWithCopy:
    if (concreteTI.isPOD(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getMemCpyFunction(IGM, concreteTI));
    } else if (concreteTI.isSingleRetainablePointer(ResilienceScope::Local)) {
      return asOpaquePtr(IGM, getInitWithCopyStrongFunction(IGM));
    }
    goto standard;

  case ValueWitness::AllocateBuffer:
  case ValueWitness::ProjectBuffer:
    if (packing == FixedPacking::OffsetZero)
      return asOpaquePtr(IGM, getReturnSelfFunction(IGM));
    goto standard;

  case ValueWitness::SizeAndAlignment:
    return asOpaquePtr(IGM, getSizeAndAlignmentFunction(IGM, concreteTI));
  }
  llvm_unreachable("bad value witness kind");

 standard:
  llvm::Function *fn =
    IGM.getAddrOfValueWitness(concreteType, index);
  if (fn->empty())
    buildValueWitness(IGM, fn, index, packing, concreteTI);
  return asOpaquePtr(IGM, fn);
}

/// Find the conformance information for a protocol.
const ConformanceInfo &
ProtocolTypeInfo::getConformance(IRGenModule &IGM, Type concreteType,
                                 const TypeInfo &concreteTI,
                                 ProtocolConformance *conformance) const {
  // Check whether we've already cached this.
  auto it = Conformances.find(conformance);
  if (it != Conformances.end()) return *it->second;

  // We haven't.  First things first:  compute the packing.
  FixedPacking packing = computePacking(IGM, concreteTI);

  // Build the witnesses:
  SmallVector<llvm::Constant*, 32> witnesses;

  // First, build the value witnesses.
  for (unsigned i = 0; i != NumValueWitnesses; ++i) {
    witnesses.push_back(getValueWitness(IGM, ValueWitness(i),
                                        packing, concreteType,
                                        concreteTI));
  }

  // Next, build the protocol witnesses.
  // FIXME

  // We've got our global initializer.
  llvm::ArrayType *tableTy =
    llvm::ArrayType::get(IGM.Int8PtrTy, witnesses.size());
  llvm::Constant *initializer =
    llvm::ConstantArray::get(tableTy, witnesses);

  // Construct a variable for that.
  // FIXME: linkage, better name, agreement across t-units,
  // interaction with runtime, etc.
  llvm::GlobalVariable *var =
    new llvm::GlobalVariable(IGM.Module, tableTy, /*constant*/ true,
                             llvm::GlobalVariable::InternalLinkage,
                             initializer, "witness_table");

  // Abstract away the length.
  llvm::ConstantInt *zero = llvm::ConstantInt::get(IGM.SizeTy, 0);
  llvm::Constant *indices[] = { zero, zero };
  llvm::Constant *table =
    llvm::ConstantExpr::getInBoundsGetElementPtr(var, indices);

  ConformanceInfo *info = new ConformanceInfo;
  info->Table = table;
  info->Packing = packing;

  auto res = Conformances.insert(std::make_pair(conformance, info));
  return *res.first->second;
}
                                 

const TypeInfo *
TypeConverter::convertProtocolType(IRGenModule &IGM, ProtocolType *T) {
  // Protocol types are nominal.
  llvm::StructType *type = IGM.createNominalType(T->getDecl());
  llvm::Type *fields[] = {
    IGM.Int8PtrTy->getPointerTo(0),  // witness table
    IGM.getFixedBufferTy()           // value buffer
  };
  type->setBody(fields);

  WitnessTableLayout layout(T->getDecl());

  Alignment align = getFixedBufferAlignment(IGM);

  Size size = getBufferOffset(IGM);
  assert(size.roundUpToAlignment(align) == size);
  size += getFixedBufferSize(IGM);

  return ProtocolTypeInfo::create(type, size, align, layout);
}

namespace {
  class DeallocateBuffer : public Cleanup {
    Address Buffer;
    FixedPacking Packing;
    const TypeInfo &ConcreteTI;

  public:
    DeallocateBuffer(Address buffer, FixedPacking packing,
                     const TypeInfo &concreteTI)
      : Buffer(buffer), Packing(packing), ConcreteTI(concreteTI) {}

    void emit(IRGenFunction &IGF) const {
      emitDeallocateBuffer(IGF, Buffer, Packing, ConcreteTI);
    }
  };
}

/// Emit an erasure expression as an initializer for the given memory.
void irgen::emitErasureAsInit(IRGenFunction &IGF, ErasureExpr *E,
                              Address dest, const TypeInfo &rawTI) {
  const ProtocolTypeInfo &protoTI = rawTI.as<ProtocolTypeInfo>();

  // Find the concrete type.
  Type concreteType = E->getSubExpr()->getType();
  const TypeInfo &concreteTI = IGF.getFragileTypeInfo(concreteType);

  // Compute the conformance information.
  const ConformanceInfo &conformance =
    protoTI.getConformance(IGF.IGM, concreteType, concreteTI,
                           E->getConformance());

  // First things first: compute the table and store that into the
  // destination.

  llvm::Value *table = conformance.getTable(IGF);
  Address tableSlot = projectWitnessTable(IGF, dest);
  IGF.Builder.CreateStore(table, tableSlot);

  FixedPacking packing = conformance.getPacking();

  // If the type is provably empty, just emit the operand as ignored.
  if (concreteTI.isEmpty(ResilienceScope::Local)) {
    assert(packing == FixedPacking::OffsetZero);
    return IGF.emitIgnored(E->getSubExpr());
  }

  // Otherwise, allocate if necessary.
  Address buffer = projectExistentialBuffer(IGF, dest);
  Address object = emitAllocateBuffer(IGF, buffer, packing, concreteTI);

  // Push a cleanup to destroy that.
  IRGenFunction::CleanupsDepth deallocCleanup;
  bool needsDeallocCleanup = !isNeverAllocated(packing);
  if (needsDeallocCleanup) {
    IGF.pushFullExprCleanup<DeallocateBuffer>(buffer, packing, concreteTI);
    deallocCleanup = IGF.getCleanupsDepth();
  }

  // Emit the object in-place.
  IGF.emitRValueAsInit(E->getSubExpr(), object, concreteTI);

  // Deactivate the dealloc cleanup.
  if (needsDeallocCleanup) {
    IGF.setCleanupState(deallocCleanup, CleanupState::Dead);
  }
}

/// Emit an expression which erases the concrete type of its value and
/// replaces it with a generic type.
void irgen::emitErasure(IRGenFunction &IGF, ErasureExpr *E, Explosion &out) {
  const ProtocolTypeInfo &protoTI =
    IGF.getFragileTypeInfo(E->getType()).as<ProtocolTypeInfo>();

  // Create a temporary of the appropriate type.
  Address temp = protoTI.createTemporary(IGF);

  // Initialize the temporary.
  emitErasureAsInit(IGF, E, temp, protoTI);

  // Add that as something to destroy.
  out.add(protoTI.enterCleanupForTemporary(IGF, temp));
}
