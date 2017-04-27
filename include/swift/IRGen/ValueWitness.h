//===--- ValueWitness.h - Enumeration of value witnesses --------*- C++ -*-===//
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
// This file defines the list of witnesses required to attest that a
// type is a value type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_VALUEWITNESS_H
#define SWIFT_IRGEN_VALUEWITNESS_H

namespace swift {
namespace irgen {

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
enum class ValueWitness : unsigned {
  // destroyBuffer comes first because I expect it to be the most
  // common operation (both by code size and occurrence), since it's
  // the optimal way to destroy an individual local/temporary.
  //
  // Several other candidates that are likely to see use in
  // existential code are then grouped together for cache-locality
  // reasons.
  
  ///   void (*destroyBuffer)(B *buffer, M *self);
  ///
  /// Given a valid buffer which owns a valid object of this type,
  /// destroy it.  This can be decomposed as
  ///   self->destroy(self->projectBuffer(buffer), self);
  ///   self->deallocateBuffer(buffer), self);
  DestroyBuffer,

  ///   T *(*initializeBufferWithCopyOfBuffer)(B *dest, B *src, M *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// object in the source buffer.  This can be decomposed as:
  ///   initializeBufferWithCopy(dest, self->projectBuffer(src), self)
  InitializeBufferWithCopyOfBuffer,
  
  ///   T *(*projectBuffer)(B *buffer, M *self);
  ///
  /// Given an initialized fixed-size buffer, find its allocated
  /// storage.
  ProjectBuffer,

  ///   void (*deallocateBuffer)(B *buffer, M *self);
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

  ///   T *(*initializeBufferWithCopy)(B *dest, T *src, M *self);
  /// Given an invalid buffer, initialize it as a copy of the
  /// source object.  This can be decomposed as:
  ///   initializeWithCopy(self->allocateBuffer(dest, self), src, self)
  InitializeBufferWithCopy,

  ///   T *(*initializeWithCopy)(T *dest, T *src, M *self);
  ///
  /// Given an invalid object of this type, initialize it as a copy of
  /// the source object.  Returns the dest object.
  InitializeWithCopy,

  ///   T *(*assignWithCopy)(T *dest, T *src, M *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  Returns the dest object.
  AssignWithCopy,

  ///   T *(*initializeBufferWithTake)(B *dest, T *src, M *self);
  ///
  /// Given an invalid buffer, initialize it by taking the value
  /// of the source object.  The source object becomes invalid.
  /// Returns the dest object.  
  InitializeBufferWithTake,

  ///   T *(*initializeWithTake)(T *dest, T *src, M *self);
  ///
  /// Given an invalid object of this type, initialize it by taking
  /// the value of the source object.  The source object becomes
  /// invalid.  Returns the dest object.
  InitializeWithTake,

  ///   T *(*assignWithTake)(T *dest, T *src, M *self);
  ///
  /// Given a valid object of this type, change it to be a copy of the
  /// source object.  The source object becomes invalid.  Returns the
  /// dest object.
  AssignWithTake,

  ///   T *(*allocateBuffer)(B *buffer, M *self);
  /// 
  /// Given a buffer in an invalid state, make it the owner of storage
  /// for an uninitialized object of this type.  Return the address of
  /// that object.
  AllocateBuffer,
  
  ///   T *(*initializeBufferWithTakeOfBuffer)(B *dest, B *src, M *self);
  /// Given an invalid buffer, initialize it by taking the value out of
  /// the source buffer.  This can be (inefficiently) decomposed as:
  ///   initializeBufferWithTake(dest, self->projectBuffer(src), self)
  ///   deallocateBuffer(src, self)
  InitializeBufferWithTakeOfBuffer,
  
  ///   void (*destroyArray)(T *object, size_t n, witness_t *self);
  ///
  /// Given a valid array of n objects of this type, destroy the object, leaving
  /// the array invalid. This is useful when generically destroying an array of
  /// objects to avoid calling the scalar 'destroy' witness in a loop.
  DestroyArray,
  
  ///   T *(*initializeArrayWithCopy)(T *dest, T *src, size_t n, M *self);
  ///
  /// Given an invalid array of n objects of this type, initialize the objects
  /// as a copy of the source array.  Returns the dest array.
  InitializeArrayWithCopy,
  
  ///   T *(*initializeArrayWithTakeFrontToBack)(T *dest, T *src, size_t n, M *self);
  ///
  /// Given an invalid array of n objects of this type, initialize the objects
  /// by taking them from the source array in front-to-back order.
  /// The source array becomes invalid.
  /// Returns the dest array.
  InitializeArrayWithTakeFrontToBack,

  ///   T *(*initializeArrayWithTakeBackToFront)(T *dest, T *src, size_t n, M *self);
  ///
  /// Given an invalid array of n objects of this type, initialize the objects
  /// by taking them from the source array in back-to-front order.
  /// The source array becomes invalid.
  /// Returns the dest array.
  InitializeArrayWithTakeBackToFront,

  Last_RequiredValueWitnessFunction = InitializeArrayWithTakeBackToFront,

  /// The offset at which type layout witnesses begin.
  First_TypeLayoutWitness,

  ///   size_t size;
  ///
  /// The required storage size of a single object of this type.
  Size = First_TypeLayoutWitness,

  ///   size_t flags;
  ///
  /// The ValueWitnessAlignmentMask bits represent the required
  /// alignment of the first byte of an object of this type, expressed
  /// as a mask of the low bits that must not be set in the pointer.
  /// This representation can be easily converted to the 'alignof'
  /// result by merely adding 1, but it is more directly useful for
  /// performing dynamic structure layouts, and it grants an
  /// additional bit of precision in a compact field without needing
  /// to switch to an exponent representation.
  ///
  /// The ValueWitnessIsNonPOD bit is set if the type is not POD.
  ///
  /// The ValueWitnessIsNonInline bit is set if the type cannot be
  /// represented in a fixed-size buffer.
  ///
  /// The Enum_HasExtraInhabitants bit is set if the type's binary
  /// representation has "extra inhabitants" that do not form valid values of
  /// the type, and the value witness table contains the ExtraInhabitantWitness
  /// entries.
  ///
  /// The Enum_HasSpareBits bit is set if the type's binary representation
  /// has unused bits.
  ///
  /// The HasEnumWitnesses bit is set if the type is an enum type.
  Flags,

  ///   size_t stride;
  ///
  /// The required size per element of an array of this type. It is at least
  /// one, even for zero-sized types, like the empty tuple.
  Stride,
  
  Last_RequiredValueWitness = Stride,
  Last_RequiredTypeLayoutWitness = Last_RequiredValueWitness,

  /// The following value witnesses are conditionally present based on
  /// the Enum_HasExtraInhabitants bit of the flags.
  First_ExtraInhabitantValueWitness,
  
  ///   size_t extraInhabitantFlags;
  ///
  /// These bits are always present if the extra inhabitants witnesses are:
  ///
  /// - The NumExtraInhabitantsMask bits contain the number of extra
  ///   inhabitants of the type representation.
  ///
  /// If the Enum_HasSpareBits flag is set in the value witness flags, these
  /// additional flags are available:
  ///
  /// - The NumSpareBitsMask bits contain the number of (host-endian) contiguous
  ///   spare bits in the type representation.
  /// - The SpareBitsShiftMask bits contain the (host-endian) bit offset of the
  ///   lowest spare bit.
  ExtraInhabitantFlags = First_ExtraInhabitantValueWitness,

  Last_TypeLayoutWitness = ExtraInhabitantFlags,

  First_ExtraInhabitantValueWitnessFunction,

  ///   void (*storeExtraInhabitant)(T *obj, unsigned index, M *self);
  ///
  /// Given an invalid object of this type, store the representation of an
  /// extra inhabitant of the type. The object will remain invalid, because
  /// an extra inhabitant is by definition an invalid representation of the
  /// type. index must be less than numExtraInhabitants.
  StoreExtraInhabitant = First_ExtraInhabitantValueWitnessFunction,
  
  ///   int (*getExtraInhabitantIndex)(T *obj, M *self);
  ///
  /// Given an invalid object of this type with an extra inhabitant
  /// representation, returns the index of the extra inhabitant representation.
  /// Returns -1 if the object is a valid value of the type. If non-negative,
  /// the return value is the same index that can be passed to
  /// storeExtraInhabitant to reproduce the representation.
  GetExtraInhabitantIndex,
  
  Last_ExtraInhabitantValueWitnessFunction = GetExtraInhabitantIndex,
  Last_ExtraInhabitantValueWitness = Last_ExtraInhabitantValueWitnessFunction,

  /// The following value witnesses are conditionally present if the witnessed
  /// type is an enum.
  First_EnumValueWitness,

  ///   int (*getEnumTag)(T *obj, M *self);
  /// Given a valid object of this enum type, extracts the tag value indicating
  /// which case of the enum is inhabited. Returned values are in the range
  /// [-ElementsWithPayload..ElementsWithNoPayload-1].
  GetEnumTag = First_EnumValueWitness,
  ///   void (*destructiveProjectEnumData)(T *obj, M *self);
  /// Given a valid object of this enum type, destructively extracts the
  /// associated payload.
  DestructiveProjectEnumData,
  ///   void (*destructiveInjectEnumTag)(T *obj, int tag, M *self);
  /// Given an enum case tag and a valid object of case's payload type,
  /// destructively inserts the tag into the payload. The given tag value
  /// must be in the range [-ElementsWithPayload..ElementsWithNoPayload-1].
  DestructiveInjectEnumTag,

  Last_EnumValueWitness = DestructiveInjectEnumTag,

  Last_ValueWitness = Last_EnumValueWitness,
};

// The namespaces here are to force the enumerators to be scoped.  We don't
// use 'enum class' because we want the enumerators to convert freely
// to uint64_t.
namespace ValueWitnessFlags {
  enum : uint64_t {
    AlignmentMask       = 0x0FFFF,
    IsNonPOD            = 0x10000,
    IsNonInline         = 0x20000,
    
    /// Flags pertaining to enum representation.
    Enum_FlagMask = 0xC0000,
    
    /// If Flags & Enum_FlagMask == Enum_IsOpaque, then the type does not
    /// support any optimized representation in enums.
    Enum_IsOpaque = 0x00000,
    /// If Flags & Enum_FlagMask == Enum_HasExtraInhabitants, then the type
    /// has "extra inhabitants" of its binary representation which do not form
    /// valid values of the type, such as null in a class type. The
    /// ExtraInhabitants value witnesses are present in the value witness table.
    Enum_HasExtraInhabitants = 0x40000,
    /// If Flags & Enum_FlagMask == Enum_HasSpareBits, then the type has
    /// unused bits in its binary representation. This implies
    /// HasExtraInhabitants. Both the ExtraInhabitants and SpareBits value
    /// witnesses are present in the value witness table.
    Enum_HasSpareBits = 0xC0000,

    IsNonBitwiseTakable = 0x100000,

    /// If Flags & HasEnumWitnesses, then enum value witnesses are present in
    /// the value witness table.
    HasEnumWitnesses = 0x00200000,
  };
}
  
namespace ExtraInhabitantFlags {
  enum : uint64_t {
    NumExtraInhabitantsMask = 0x7FFFFFFFULL,
    
    NumSpareBitsMask   = 0x0000FFFF00000000ULL,
    NumSpareBitsShift  = 32,
    
    SpareBitsShiftMask = 0xFFFF000000000000ULL,
    SpareBitsShiftShift = 48,
  };
}
 
enum {
  NumRequiredValueWitnesses
    = unsigned(ValueWitness::Last_RequiredValueWitness) + 1,
  NumRequiredValueWitnessFunctions
    = unsigned(ValueWitness::Last_RequiredValueWitnessFunction) + 1,
  
  MaxNumValueWitnesses
    = unsigned(ValueWitness::Last_ValueWitness) + 1,
  MaxNumTypeLayoutWitnesses
    = unsigned(ValueWitness::Last_TypeLayoutWitness)
      - unsigned(ValueWitness::First_TypeLayoutWitness)
      + 1,
};

static inline bool isValueWitnessFunction(ValueWitness witness) {
  auto ord = unsigned(witness);
  return ord < NumRequiredValueWitnessFunctions
    || (ord >= unsigned(ValueWitness::First_ExtraInhabitantValueWitness)
        && ord <= unsigned(
                       ValueWitness::Last_ExtraInhabitantValueWitnessFunction))
    || (ord >= unsigned(ValueWitness::First_EnumValueWitness)
        && ord <= unsigned(ValueWitness::Last_EnumValueWitness));
}

const char *getValueWitnessName(ValueWitness witness);

} // end namespace irgen
} // end namespace swift

#endif
