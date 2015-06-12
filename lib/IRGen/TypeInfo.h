//===--- TypeInfo.h - Abstract primitive operations on values ---*- C++ -*-===//
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
// This file defines the interface used to perform primitive
// operations on swift values and objects.
//
// This interface is supplemented in two ways:
//   - FixedTypeInfo provides a number of operations meaningful only
//     for types with a fixed-size representation
//   - ReferenceTypeInfo is a further refinement of FixedTypeInfo
//     which provides operations meaningful only for types with
//     reference semantics
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_TYPEINFO_H
#define SWIFT_IRGEN_TYPEINFO_H

#include "IRGen.h"

namespace llvm {
  class Constant;
  class Twine;
  class Type;
}

namespace swift {
namespace irgen {
  class Address;
  class ContainedAddress;
  class IRGenFunction;
  class IRGenModule;
  class Explosion;
  class ExplosionSchema;
  enum OnHeap_t : unsigned char;
  class OwnedAddress;
  class RValue;
  class RValueSchema;

/// Ways in which an object can fit into a fixed-size buffer.
enum class FixedPacking {
  /// It fits at offset zero.
  OffsetZero,
  
  /// It doesn't fit and needs to be side-allocated.
  Allocate,
  
  /// It needs to be checked dynamically.
  Dynamic
};

/// Information about the IR representation and generation of the
/// given type.
class TypeInfo {
  TypeInfo(const TypeInfo &) = delete;
  TypeInfo &operator=(const TypeInfo &) = delete;

  friend class TypeConverter;
  mutable const TypeInfo *NextConverted;

protected:
  enum SpecialTypeInfoKind {
    STIK_Unimplemented,
    
    STIK_None,

    /// Everything after this is statically fixed-size.
    STIK_Fixed,
    STIK_Weak,

    /// Everything after this is loadable.
    STIK_Loadable,
    STIK_Reference,
    STIK_Unowned,
  };

  TypeInfo(llvm::Type *Type, Alignment A, IsPOD_t pod,
           IsBitwiseTakable_t bitwiseTakable,
           SpecialTypeInfoKind stik)
    : NextConverted(0), StorageType(Type), StorageAlignment(A),
      POD(pod), BitwiseTakable(bitwiseTakable), STIK(stik),
      SubclassKind(InvalidSubclassKind) {}

  /// Change the minimum alignment of a stored value of this type.
  void setStorageAlignment(Alignment alignment) {
    StorageAlignment = alignment;
  }

public:
  virtual ~TypeInfo() = default;

  /// Unsafely cast this to the given subtype.
  template <class T> const T &as() const {
    // FIXME: maybe do an assert somehow if we have RTTI enabled.
    return static_cast<const T &>(*this);
  }

  /// The LLVM representation of a stored value of this type.  For
  /// non-fixed types, this is really useful only for forming pointers to it.
  llvm::Type *StorageType;

private:
  /// The storage alignment of this type in bytes.  This is never zero
  /// for a completely-converted type.
  Alignment StorageAlignment;

  /// Whether this type is known to be POD.
  unsigned POD : 1;
  
  /// Whether this type is known to be bitwise-takable.
  unsigned BitwiseTakable : 1;

  /// The kind of supplemental API this type has, if any.
  unsigned STIK : 3;

  /// An arbitrary discriminator for the subclass.  This is useful for
  /// e.g. distinguishing between different TypeInfos that all
  /// implement the same kind of type.
  unsigned SubclassKind : 3;
  enum { InvalidSubclassKind = 0x7 };

protected:
  void setSubclassKind(unsigned kind) {
    assert(kind != InvalidSubclassKind);
    SubclassKind = kind;
    assert(SubclassKind == kind && "kind was truncated?");
  }

public:
  /// Sets whether this type is POD.  Should only be called during
  /// completion of a forward-declaration.
  void setPOD(IsPOD_t isPOD) { POD = unsigned(isPOD); }

  /// Sets whether this type is bitwise-takable.  Should only be called during
  /// completion of a forward-declaration.
  void setBitwiseTakable(IsBitwiseTakable_t takable) {
    BitwiseTakable = unsigned(takable);
  }
  
  /// Whether this type info has been completely converted.
  bool isComplete() const { return !StorageAlignment.isZero(); }

  /// Whether this type is known to be empty.
  bool isKnownEmpty() const;

  /// Whether this type is known to be POD, i.e. to not require any
  /// particular action on copy or destroy.
  IsPOD_t isPOD(ResilienceScope scope) const { return IsPOD_t(POD); }
  
  /// Whether this type is known to be bitwise-takable, i.e. "initializeWithTake"
  /// is equivalent to a memcpy.
  IsBitwiseTakable_t isBitwiseTakable(ResilienceScope scope) const {
    return IsBitwiseTakable_t(BitwiseTakable);
  }
  
  /// Returns the type of special interface followed by this TypeInfo.
  /// It is important for our design that this depends only on
  /// immediate type structure and not on, say, properties that can
  /// vary by resilience.  Of course, generics can obscure these
  /// properties on their parameter types, but then the program
  /// can rely on them.
  SpecialTypeInfoKind getSpecialTypeInfoKind() const {
    return SpecialTypeInfoKind(STIK);
  }

  /// Returns whatever arbitrary data has been stash in the subclass
  /// kind field.  This mechanism allows an orthogonal dimension of
  /// distinguishing between TypeInfos, which is useful when multiple
  /// TypeInfo subclasses are used to implement the same kind of type.
  unsigned getSubclassKind() const {
    assert(SubclassKind != InvalidSubclassKind &&
           "subclass kind has not been initialized!");
    return SubclassKind;
  }

  /// Whether this type is known to be fixed-size in the local
  /// resilience domain.  If true, this TypeInfo can be cast to
  /// FixedTypeInfo.
  IsFixedSize_t isFixedSize() const {
    return IsFixedSize_t(STIK >= STIK_Fixed);
  }

  /// Whether this type is known to be loadable in the local
  /// resilience domain.  If true, this TypeInfo can be cast to
  /// LoadableTypeInfo.
  IsLoadable_t isLoadable() const {
    return IsLoadable_t(STIK >= STIK_Loadable);
  }

  llvm::Type *getStorageType() const { return StorageType; }

  Alignment getBestKnownAlignment() const {
    return StorageAlignment;
  }

  /// Given a generic pointer to this type, produce an Address for it.
  Address getAddressForPointer(llvm::Value *ptr) const;

  /// Produce an undefined pointer to an object of this type.
  Address getUndefAddress() const;
    
  /// Return the size and alignment of this type.
  virtual std::pair<llvm::Value*,llvm::Value*>
    getSizeAndAlignmentMask(IRGenFunction &IGF, SILType T) const = 0;
  virtual std::tuple<llvm::Value*,llvm::Value*,llvm::Value*>
    getSizeAndAlignmentMaskAndStride(IRGenFunction &IGF, SILType T) const = 0;
  virtual llvm::Value *getSize(IRGenFunction &IGF, SILType T) const = 0;
  virtual llvm::Value *getAlignmentMask(IRGenFunction &IGF, SILType T) const = 0;
  virtual llvm::Value *getStride(IRGenFunction &IGF, SILType T) const = 0;
  virtual llvm::Value *isDynamicallyPackedInline(IRGenFunction &IGF,
                                                 SILType T) const = 0;

  /// Return the statically-known size of this type, or null if it is
  /// not known.
  virtual llvm::Constant *getStaticSize(IRGenModule &IGM) const = 0;

  /// Return the statically-known alignment mask for this type, or
  /// null if it is not known.
  virtual llvm::Constant *getStaticAlignmentMask(IRGenModule &IGM) const = 0;

  /// Return the statically-known stride size of this type, or null if
  /// it is not known.
  virtual llvm::Constant *getStaticStride(IRGenModule &IGM) const = 0;

  /// Add the information for exploding values of this type to the
  /// given schema.
  virtual void getSchema(ExplosionSchema &schema) const = 0;

  /// A convenience for getting the schema of a single type.
  ExplosionSchema getSchema() const;

  /// Allocate a variable of this type on the stack.
  virtual ContainedAddress allocateStack(IRGenFunction &IGF,
                                         SILType T,
                                         const llvm::Twine &name) const = 0;

  /// Deallocate a variable of this type.
  virtual void deallocateStack(IRGenFunction &IGF, Address addr,
                               SILType T) const = 0;

  /// Allocate a box of this type on the heap.
  virtual OwnedAddress allocateBox(IRGenFunction &IGF, SILType T,
                                   const llvm::Twine &name) const = 0;

  /// Deallocate an uninitialized box of this type on the heap.
  virtual void deallocateBox(IRGenFunction &IGF,
                             llvm::Value *boxOwner,
                             SILType T) const = 0;
  
  /// Copy a value out of an object and into another, destroying the
  /// old value in the destination.
  virtual void assignWithCopy(IRGenFunction &IGF, Address dest,
                              Address src, SILType T) const = 0;

  /// Move a value out of an object and into another, destroying the
  /// old value there and leaving the source object in an invalid state.
  virtual void assignWithTake(IRGenFunction &IGF, Address dest,
                              Address src, SILType T) const = 0;

  /// Perform a "take-initialization" from the given object.  A
  /// take-initialization is like a C++ move-initialization, except that
  /// the old object is actually no longer permitted to be destroyed.
  virtual void initializeWithTake(IRGenFunction &IGF, Address destAddr,
                                  Address srcAddr, SILType T) const = 0;

  /// Perform a copy-initialization from the given object.
  virtual void initializeWithCopy(IRGenFunction &IGF, Address destAddr,
                                  Address srcAddr, SILType T) const = 0;

  /// Perform a "take-initialization" from the given object into an
  /// uninitialized fixed-size buffer, allocating the buffer if necessary.
  /// Returns the address of the value inside the buffer.
  ///
  /// This is equivalent to:
  ///   auto destAddress = allocateBuffer(IGF, destBuffer, T);
  ///   initializeWithTake(IGF, destAddr, srcAddr, T);
  ///   return destAddress;
  /// but will be more efficient for dynamic types, since it uses a single
  /// value witness call.
  virtual Address initializeBufferWithTake(IRGenFunction &IGF,
                                           Address destBuffer,
                                           Address srcAddr,
                                           SILType T) const;

  /// Perform a copy-initialization from the given object into an
  /// uninitialized fixed-size buffer, allocating the buffer if necessary.
  /// Returns the address of the value inside the buffer.
  ///
  /// This is equivalent to:
  ///   auto destAddress = allocateBuffer(IGF, destBuffer, T);
  ///   initializeWithCopy(IGF, destAddr, srcAddr, T);
  ///   return destAddress;
  /// but will be more efficient for dynamic types, since it uses a single
  /// value witness call.
  virtual Address initializeBufferWithCopy(IRGenFunction &IGF,
                                           Address destBuffer,
                                           Address srcAddr,
                                           SILType T) const;

  /// Take-initialize an address from a parameter explosion.
  virtual void initializeFromParams(IRGenFunction &IGF, Explosion &params,
                                    Address src, SILType T) const = 0;

  /// Destroy an object of this type in memory.
  virtual void destroy(IRGenFunction &IGF, Address address, SILType T) const = 0;
  
  /// Should optimizations be enabled which rely on the representation
  /// for this type being a single Swift-retainable object pointer?
  ///
  /// \return false by default
  virtual bool isSingleSwiftRetainablePointer(ResilienceScope scope) const;

  /// Should optimizations be enabled which rely on the representation
  /// for this type being a single, potentially Objective-C, retainable
  /// object pointer?
  ///
  /// \return false by default. isSingleSwiftRetainablePointer implies this.
  virtual bool isSingleUnknownRetainablePointer(ResilienceScope scope) const;

  /// Does this type statically have extra inhabitants, or may it dynamically
  /// have extra inhabitants based on type arguments?
  virtual bool mayHaveExtraInhabitants(IRGenModule &IGM) const = 0;
  
  /// Map an extra inhabitant representation in memory to a unique 31-bit
  /// identifier, and map a valid representation of the type to -1.
  ///
  /// Calls to this witness must be dominated by a runtime check that the type
  /// has extra inhabitants.
  virtual llvm::Value *getExtraInhabitantIndex(IRGenFunction &IGF,
                                               Address src,
                                               SILType T) const = 0;
  
  /// Store the extra inhabitant representation indexed by a 31-bit identifier
  /// to memory.
  ///
  /// Calls to this witness must be dominated by a runtime check that the type
  /// has extra inhabitants.
  virtual void storeExtraInhabitant(IRGenFunction &IGF,
                                    llvm::Value *index,
                                    Address dest,
                                    SILType T) const = 0;
  
  /// Initialize a freshly instantiated value witness table. Should be a no-op
  /// for fixed-size types.
  virtual void initializeMetadata(IRGenFunction &IGF,
                                  llvm::Value *metadata,
                                  llvm::Value *vwtable,
                                  SILType T) const = 0;
  
  /// Compute the packing of values of this type into a fixed-size buffer.
  FixedPacking getFixedPacking(IRGenModule &IGM) const;
  
  /// Index into an array of objects of this type.
  Address indexArray(IRGenFunction &IGF, Address base, llvm::Value *offset,
                     SILType T) const;
  
  /// Destroy an array of objects of this type in memory.
  virtual void destroyArray(IRGenFunction &IGF, Address base,
                            llvm::Value *count, SILType T) const;
  
  /// Initialize an array of objects of this type in memory by copying the
  /// values from another array. The arrays must not overlap.
  virtual void initializeArrayWithCopy(IRGenFunction &IGF,
                                       Address dest,
                                       Address src,
                                       llvm::Value *count, SILType T) const;
  
  /// Initialize an array of objects of this type in memory by taking the
  /// values from another array. The destination array may overlap the head of
  /// the source array because the elements are taken as if in front-to-back
  /// order.
  virtual void initializeArrayWithTakeFrontToBack(IRGenFunction &IGF,
                                       Address dest, Address src,
                                       llvm::Value *count, SILType T) const;
  
  /// Initialize an array of objects of this type in memory by taking the
  /// values from another array. The destination array may overlap the tail of
  /// the source array because the elements are taken as if in back-to-front
  /// order.
  virtual void initializeArrayWithTakeBackToFront(IRGenFunction &IGF,
                                       Address dest, Address src,
                                       llvm::Value *count, SILType T) const;
};

} // end namespace irgen
} // end namespace swift

#endif
