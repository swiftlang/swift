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

#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "llvm/DerivedTypes.h"

#include "Cleanup.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

/// The members required to attest that a type is a value type.
///
/// Logically, there are three basic data operations we must support
/// on arbitrary types:
///   - initializing an object by copying another
///   - changing an object to be a copy of another
///   - destroying an object
///
/// As an optimization to permit efficient transfers of data, the
/// "copy" operations each have an analogous "take" operation which
/// implicitly destroys the source object.
///
/// Therefore there are five basic data operations:
///   initWithCopy(T*, T*)
///   initWithTake(T*, T*)
///   assignWithCopy(T*, T*)
///   assignWithTake(T*, T*)
///   destroy(T*)
///
/// As a further optimization, for every T*, there is a related
/// operation which replaces that T* with a B*, combinatorially.  This
/// makes 18 operations, except that some of these operations are
/// fairly unlikely and so do not merit optimized entries, due to
/// the common code patterns of the two use cases:
///   - Existential code usually doesn't work directly with T*s
///     because pointers into existential objects are not generally
///     reliable.
///   - Generic code works with T*s a fair amount, but it usually
///     doesn't have to deal with B*s after initialization
///     because initialization returns a reliable pointer.
/// This leads us to the following conclusions:
//    - Operations to copy a B* to a T* are very unlikely
///     to be used (-4 operations).
///   - Assignments involving two B*s are only likely in
///     existential code, where we won't have the right
///     typing guarantees to use them (-2 operations).
/// Furthermore, take-initializing a buffer from a buffer is just a
/// memcpy of the buffer (-1), and take-assigning a buffer from a
/// buffer is just a destroy and a memcpy (-1).
///
/// This leaves us with 12 data operations, to which we add the
/// meta-operation 'sizeAndAlign' for a total of 13.
enum class swift::irgen::ValueWitness : unsigned {
  // destroyBuffer comes first because I expect it to be the most
  // common operation (both by code size and occurrence), since it's
  // the optimal way to destroy an individual local/temporary.
  //
  // Several other candidates that are likely to see use in
  // existential code are then grouped together for cache-locality
  // reasons.

  ///   void (*destroyBuffer)(B *buffer, W *self);
  ///
  /// Given a valid buffer which owns a valid object of this type,
  /// destroy it.  This can be decomposed as
  ///   self->destroy(self->projectBuffer(buffer), self);
  ///   self->deallocateBuffer(buffer), self);
  DestroyBuffer,

  ///   T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, W *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// object in the source buffer.  This can be decomposed as:
  ///   initalizeBufferWithCopy(dest, self->projectBuffer(src), self)
  InitializeBufferWithCopyOfBuffer,

  ///   T *(*projectBuffer)(B *buffer, W *self);
  ///
  /// Given an initialized fixed-size buffer, find its allocated
  /// storage.
  ProjectBuffer,

  ///   void (*deallocateBuffer)(B *buffer, W *self);
  ///
  /// Given a buffer owning storage for an uninitialized object of this
  /// type, deallocate the storage, putting the buffer in an invalid
  /// state.
  DeallocateBuffer, // likely along exception edges of initializers

  ///   void (*destroy)(T *object, witness_t *self);
  ///
  /// Given a valid object of this type, destroy it, leaving it as an
  /// invalid object.  This is useful when generically destroying
  /// an object which has been allocated in-line, such as an array,
  /// struct, or tuple element.
  Destroy,

  ///   T *(*initializeBufferWithCopy)(B *dest, T *src, W *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// source object.  This can be decomposed as:
  ///   initializeWithCopy(self->allocateBuffer(dest, self), src, self)
  InitializeBufferWithCopy,

  ///   T *(*initializeWithCopy)(T *dest, T *src, W *self);
  ///
  /// Given an invalid object of this type, initialize it as a copy of
  /// the source object.  Returns the dest object.
  InitializeWithCopy,

  ///   T *(*assignWithCopy)(T *dest, T *src, W *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  Returns the dest object.
  AssignWithCopy,

  ///   T *(*initializeBufferWithTake)(B *dest, T *src, W *self);
  ///
  /// Given an invalid buffer, initialize it by taking the value
  /// of the source object.  The source object becomes invalid.
  /// Returns the dest object.  
  InitializeBufferWithTake,

  ///   T *(*initializeWithTake)(T *dest, T *src, W *self);
  ///
  /// Given an invalid object of this type, initialize it by taking
  /// the value of the source object.  The source object becomes
  /// invalid.  Returns the dest object.
  InitializeWithTake,

  ///   T *(*assignWithTake)(T *dest, T *src, W *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  The source object becomes invalid.  Returns the
  /// dest object.
  AssignWithTake,

  ///   T *(*allocateBuffer)(B *buffer, W *self);
  /// 
  /// Given a buffer in an invalid state, make it the owner of storage
  /// for an uninitialized object of this type.  Return the address of
  /// that object.
  AllocateBuffer,

  ///   typedef struct { size_t Size; size_t Align } layout_t;
  ///   layout_t (*sizeAndAlignment)(W *self);
  ///
  /// Returns the required storage size and alignment of an object of
  /// this type.
  SizeAndAlignment,
};
const unsigned NumValueWitnesses = unsigned(ValueWitness::SizeAndAlignment) + 1;

static llvm::PointerType *getWitnessTableTy(IRGenModule &IGM) {
  return IGM.Int8PtrTy->getPointerTo(0);
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
    llvm::Type *resultElts[] = { IGM.SizeTy, IGM.SizeTy };
    llvm::StructType *resultTy =
      llvm::StructType::get(IGM.getLLVMContext(), resultElts);
    return llvm::FunctionType::get(resultTy, getWitnessTableTy(IGM), false);
  }

  }
  llvm_unreachable("bad value witness!");
}

/// Return the cached pointer-to-function type for the given value
/// witness index.
llvm::PointerType *IRGenModule::getValueWitnessTy(ValueWitness index) {
  static_assert(sizeof(ValueWitnessTys)/sizeof(*ValueWitnessTys)
                  == NumValueWitnesses,
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
static Address projectBuffer(IRGenFunction &IGF, Address addr) {
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
  emitDestroyBuffer(IGF, table, projectBuffer(IGF, addr));
}

namespace {
  struct DestroyExistential : Cleanup {
    Address Addr;
    DestroyExistential(Address addr) : Addr(addr) {}

    void emit(IRGenFunction &IGF) const {
      emitDestroyExistential(IGF, Addr);
    }
  };

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
    unsigned NumWitnesses;
    unsigned NumTableEntries;

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
      Address srcBuffer = projectBuffer(IGF, src);
      Address destBuffer = projectBuffer(IGF, dest);
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
}

/// Add a member to the witness-table layout.
void WitnessTableLayout::addMember(Decl *member) {
  // FIXME
}

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

