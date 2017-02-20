//===--- Metadata.h - Swift Language ABI Metadata Support -------*- C++ -*-===//
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
// Swift ABI for generating and uniquing metadata.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_METADATA_H
#define SWIFT_RUNTIME_METADATA_H

#include <atomic>
#include <cassert>
#include <climits>
#include <cstddef>
#include <cstdint>
#include <string>
#include <type_traits>
#include <utility>
#include <string.h>
#include "swift/Runtime/Config.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/System.h"
#include "swift/Basic/Malloc.h"
#include "swift/Basic/FlaggedPointer.h"
#include "swift/Basic/RelativePointer.h"
#include "swift/Basic/ManglingMacros.h"
#include "swift/Runtime/Unreachable.h"
#include "../../../stdlib/public/SwiftShims/HeapObject.h"

namespace swift {

template <unsigned PointerSize>
struct RuntimeTarget;

template <>
struct RuntimeTarget<4> {
  using StoredPointer = uint32_t;
  using StoredSize = uint32_t;
  static constexpr size_t PointerSize = 4;
};

template <>
struct RuntimeTarget<8> {
  using StoredPointer = uint64_t;
  using StoredSize = uint64_t;
  static constexpr size_t PointerSize = 8;
};

/// In-process native runtime target.
///
/// For interactions in the runtime, this should be the equivalent of working
/// with a plain old pointer type.
struct InProcess {
  static constexpr size_t PointerSize = sizeof(uintptr_t);
  using StoredPointer = uintptr_t;
  using StoredSize = size_t;
  
  template <typename T>
  using Pointer = T*;
  
  template <typename T, bool Nullable = false>
  using FarRelativeDirectPointer = FarRelativeDirectPointer<T, Nullable>;

  template <typename T, bool Nullable = false>
  using FarRelativeIndirectablePointer =
    FarRelativeIndirectablePointer<T, Nullable>;
  
  template <typename T, bool Nullable = true>
  using RelativeDirectPointer = RelativeDirectPointer<T, Nullable>;
};

/// Represents a pointer in another address space.
///
/// This type should not have * or -> operators -- you must as a memory reader
/// to read the data at the stored address on your behalf.
template <typename Runtime, typename Pointee>
struct ExternalPointer {
  using StoredPointer = typename Runtime::StoredPointer;
  StoredPointer PointerValue;
};

/// An external process's runtime target, which may be a different architecture.
template <typename Runtime>
struct External {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;
  static constexpr size_t PointerSize = Runtime::PointerSize;
  const StoredPointer PointerValue;
  
  template <typename T>
  using Pointer = StoredPointer;
  
  template <typename T, bool Nullable = false>
  using FarRelativeDirectPointer = StoredPointer;

  template <typename T, bool Nullable = false>
  using FarRelativeIndirectablePointer = StoredSize;
  
  template <typename T, bool Nullable = true>
  using RelativeDirectPointer = int32_t;
};

/// Template for branching on native pointer types versus external ones
template <typename Runtime, template <typename> class Pointee>
using TargetMetadataPointer
  = typename Runtime::template Pointer<Pointee<Runtime>>;
  
template <typename Runtime, template <typename> class Pointee>
using ConstTargetMetadataPointer
  = typename Runtime::template Pointer<const Pointee<Runtime>>;
  
template <typename Runtime, typename T>
using TargetPointer = typename Runtime::template Pointer<T>;
  
template <typename Runtime, template <typename> class Pointee,
          bool Nullable = true>
using ConstTargetFarRelativeDirectPointer
  = typename Runtime::template FarRelativeDirectPointer<const Pointee<Runtime>,
                                                        Nullable>;

template <typename Runtime, typename Pointee, bool Nullable = true>
using TargetRelativeDirectPointer
  = typename Runtime::template RelativeDirectPointer<Pointee, Nullable>;

template <typename Runtime, typename Pointee, bool Nullable = true>
using TargetFarRelativeIndirectablePointer
  = typename Runtime::template FarRelativeIndirectablePointer<Pointee,Nullable>;

struct HeapObject;
struct WeakReference;
  
template <typename Runtime> struct TargetMetadata;
using Metadata = TargetMetadata<InProcess>;

/// Storage for an arbitrary value.  In C/C++ terms, this is an
/// 'object', because it is rooted in memory.
///
/// The context dictates what type is actually stored in this object,
/// and so this type is intentionally incomplete.
///
/// An object can be in one of two states:
///  - An uninitialized object has a completely unspecified state.
///  - An initialized object holds a valid value of the type.
struct OpaqueValue;

/// A fixed-size buffer for local values.  It is capable of owning
/// (possibly in side-allocated memory) the storage necessary
/// to hold a value of an arbitrary type.  Because it is fixed-size,
/// it can be allocated in places that must be agnostic to the
/// actual type: for example, within objects of existential type,
/// or for local variables in generic functions.
///
/// The context dictates its type, which ultimately means providing
/// access to a value witness table by which the value can be
/// accessed and manipulated.
///
/// A buffer can directly store three pointers and is pointer-aligned.
/// Three pointers is a sweet spot for Swift, because it means we can
/// store a structure containing a pointer, a size, and an owning
/// object, which is a common pattern in code due to ARC.  In a GC
/// environment, this could be reduced to two pointers without much loss.
///
/// A buffer can be in one of three states:
///  - An unallocated buffer has a completely unspecified state.
///  - An allocated buffer has been initialized so that it
///    owns uninitialized value storage for the stored type.
///  - An initialized buffer is an allocated buffer whose value
///    storage has been initialized.
struct ValueBuffer {
  void *PrivateData[3];
};

/// Can a value with the given size and alignment be allocated inline?
constexpr inline bool canBeInline(size_t size, size_t alignment) {
  return size <= sizeof(ValueBuffer) && alignment <= alignof(ValueBuffer);
}

template <class T>
constexpr inline bool canBeInline() {
  return canBeInline(sizeof(T), alignof(T));
}

struct ValueWitnessTable;

/// Flags stored in the value-witness table.
class ValueWitnessFlags {
  typedef size_t int_type;
  
  // The polarity of these bits is chosen so that, when doing struct layout, the
  // flags of the field types can be mostly bitwise-or'ed together to derive the
  // flags for the struct. (The "non-inline" and "has-extra-inhabitants" bits
  // still require additional fixup.)
  enum : int_type {
    AlignmentMask = 0x0000FFFF,
    IsNonPOD =      0x00010000,
    IsNonInline =   0x00020000,
    HasExtraInhabitants = 0x00040000,
    HasSpareBits =  0x00080000,
    IsNonBitwiseTakable = 0x00100000,
    HasEnumWitnesses = 0x00200000,
    // Everything else is reserved.
  };
  int_type Data;

  constexpr ValueWitnessFlags(int_type data) : Data(data) {}
public:
  constexpr ValueWitnessFlags() : Data(0) {}

  /// The required alignment of the first byte of an object of this
  /// type, expressed as a mask of the low bits that must not be set
  /// in the pointer.
  ///
  /// This representation can be easily converted to the 'alignof'
  /// result by merely adding 1, but it is more directly useful for
  /// performing dynamic structure layouts, and it grants an
  /// additional bit of precision in a compact field without needing
  /// to switch to an exponent representation.
  ///
  /// For example, if the type needs to be 8-byte aligned, the
  /// appropriate alignment mask should be 0x7.
  size_t getAlignmentMask() const {
    return (Data & AlignmentMask);
  }
  constexpr ValueWitnessFlags withAlignmentMask(size_t alignMask) const {
    return ValueWitnessFlags((Data & ~AlignmentMask) | alignMask);
  }

  size_t getAlignment() const { return getAlignmentMask() + 1; }
  constexpr ValueWitnessFlags withAlignment(size_t alignment) const {
    return withAlignmentMask(alignment - 1);
  }

  /// True if the type requires out-of-line allocation of its storage.
  bool isInlineStorage() const { return !(Data & IsNonInline); }
  constexpr ValueWitnessFlags withInlineStorage(bool isInline) const {
    return ValueWitnessFlags((Data & ~IsNonInline) |
                               (isInline ? 0 : IsNonInline));
  }

  /// True if values of this type can be copied with memcpy and
  /// destroyed with a no-op.
  bool isPOD() const { return !(Data & IsNonPOD); }
  constexpr ValueWitnessFlags withPOD(bool isPOD) const {
    return ValueWitnessFlags((Data & ~IsNonPOD) |
                               (isPOD ? 0 : IsNonPOD));
  }
  
  /// True if values of this type can be taken with memcpy. Unlike C++ 'move',
  /// 'take' is a destructive operation that invalidates the source object, so
  /// most types can be taken with a simple bitwise copy. Only types with side
  /// table references, like @weak references, or types with opaque value
  /// semantics, like imported C++ types, are not bitwise-takable.
  bool isBitwiseTakable() const { return !(Data & IsNonBitwiseTakable); }
  constexpr ValueWitnessFlags withBitwiseTakable(bool isBT) const {
    return ValueWitnessFlags((Data & ~IsNonBitwiseTakable) |
                               (isBT ? 0 : IsNonBitwiseTakable));
  }
  /// True if this type's binary representation has extra inhabitants, that is,
  /// bit patterns that do not form valid values of the type.
  ///
  /// If true, then the extra inhabitant value witness table entries are
  /// available in this type's value witness table.
  bool hasExtraInhabitants() const { return Data & HasExtraInhabitants; }
  /// True if this type's binary representation is that of an enum, and the
  /// enum value witness table entries are available in this type's value
  /// witness table.
  bool hasEnumWitnesses() const { return Data & HasEnumWitnesses; }
  constexpr ValueWitnessFlags
  withExtraInhabitants(bool hasExtraInhabitants) const {
    return ValueWitnessFlags((Data & ~HasExtraInhabitants) |
                               (hasExtraInhabitants ? HasExtraInhabitants : 0));
  }
  constexpr ValueWitnessFlags
  withEnumWitnesses(bool hasEnumWitnesses) const {
    return ValueWitnessFlags((Data & ~HasEnumWitnesses) |
                             (hasEnumWitnesses ? HasEnumWitnesses : 0));
  }
};
  
/// Flags stored in a value-witness table with extra inhabitants.
class ExtraInhabitantFlags {
  typedef size_t int_type;
  enum : int_type {
    NumExtraInhabitantsMask = 0x7FFFFFFFU,
  };
  int_type Data;
  
  constexpr ExtraInhabitantFlags(int_type data) : Data(data) {}

public:
  constexpr ExtraInhabitantFlags() : Data(0) {}
  
  /// The number of extra inhabitants in the type's representation.
  int getNumExtraInhabitants() const { return Data & NumExtraInhabitantsMask; }
  
  constexpr ExtraInhabitantFlags
  withNumExtraInhabitants(unsigned numExtraInhabitants) const {
    return ExtraInhabitantFlags((Data & ~NumExtraInhabitantsMask) |
                                  numExtraInhabitants);
  }
};

namespace value_witness_types {

/// Given an initialized buffer, destroy its value and deallocate
/// the buffer.  This can be decomposed as:
///
///   self->destroy(self->projectBuffer(buffer), self);
///   self->deallocateBuffer(buffer), self);
///
/// Preconditions:
///   'buffer' is an initialized buffer
/// Postconditions:
///   'buffer' is an unallocated buffer
typedef void destroyBuffer(ValueBuffer *buffer, const Metadata *self);

/// Given an unallocated buffer, initialize it as a copy of the
/// object in the source buffer.  This can be decomposed as:
///
///   self->initializeBufferWithCopy(dest, self->projectBuffer(src), self)
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// 
/// Preconditions:
///   'dest' is an unallocated buffer
/// Postconditions:
///   'dest' is an initialized buffer
/// Invariants:
///   'src' is an initialized buffer
typedef OpaqueValue *initializeBufferWithCopyOfBuffer(ValueBuffer *dest,
                                                      ValueBuffer *src,
                                                      const Metadata *self);

/// Given an allocated or initialized buffer, derive a pointer to
/// the object.
/// 
/// Invariants:
///   'buffer' is an allocated or initialized buffer
typedef OpaqueValue *projectBuffer(ValueBuffer *buffer,
                                   const Metadata *self);

/// Given an allocated buffer, deallocate the object.
///
/// Preconditions:
///   'buffer' is an allocated buffer
/// Postconditions:
///   'buffer' is an unallocated buffer
typedef void deallocateBuffer(ValueBuffer *buffer,
                              const Metadata *self);

/// Given an initialized object, destroy it.
///
/// Preconditions:
///   'object' is an initialized object
/// Postconditions:
///   'object' is an uninitialized object
typedef void destroy(OpaqueValue *object,
                     const Metadata *self);

/// Given an uninitialized buffer and an initialized object, allocate
/// storage in the buffer and copy the value there.
///
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized buffer
/// Postconditions:
///   'dest' is an initialized buffer
/// Invariants:
///   'src' is an initialized object
typedef OpaqueValue *initializeBufferWithCopy(ValueBuffer *dest,
                                              OpaqueValue *src,
                                              const Metadata *self);

/// Given an uninitialized object and an initialized object, copy
/// the value.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized object
/// Postconditions:
///   'dest' is an initialized object
/// Invariants:
///   'src' is an initialized object
typedef OpaqueValue *initializeWithCopy(OpaqueValue *dest,
                                        OpaqueValue *src,
                                        const Metadata *self);

/// Given two initialized objects, copy the value from one to the
/// other.
///
/// This operation must be safe against 'dest' and 'src' aliasing.
/// 
/// Returns the dest object.
///
/// Invariants:
///   'dest' is an initialized object
///   'src' is an initialized object
typedef OpaqueValue *assignWithCopy(OpaqueValue *dest,
                                    OpaqueValue *src,
                                    const Metadata *self);

/// Given an uninitialized buffer and an initialized object, move
/// the value from the object to the buffer, leaving the source object
/// uninitialized.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized buffer
///   'src' is an initialized object
/// Postconditions:
///   'dest' is an initialized buffer
///   'src' is an uninitialized object
typedef OpaqueValue *initializeBufferWithTake(ValueBuffer *dest,
                                              OpaqueValue *src,
                                              const Metadata *self);

/// Given an uninitialized object and an initialized object, move
/// the value from one to the other, leaving the source object
/// uninitialized.
///
/// There is no need for an initializeBufferWithTakeOfBuffer, because that
/// can simply be a pointer-aligned memcpy of sizeof(ValueBuffer)
/// bytes.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized object
///   'src' is an initialized object
/// Postconditions:
///   'dest' is an initialized object
///   'src' is an uninitialized object
typedef OpaqueValue *initializeWithTake(OpaqueValue *dest,
                                        OpaqueValue *src,
                                        const Metadata *self);

/// Given an initialized object and an initialized object, move
/// the value from one to the other, leaving the source object
/// uninitialized.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// Therefore this can be decomposed as:
///
///   self->destroy(dest, self);
///   self->initializeWithTake(dest, src, self);
///
/// Returns the dest object.
///
/// Preconditions:
///   'src' is an initialized object
/// Postconditions:
///   'src' is an uninitialized object
/// Invariants:
///   'dest' is an initialized object
typedef OpaqueValue *assignWithTake(OpaqueValue *dest,
                                    OpaqueValue *src,
                                    const Metadata *self);

/// Given an uninitialized buffer, allocate an object.
///
/// Returns the uninitialized object.
///
/// Preconditions:
///   'buffer' is an uninitialized buffer
/// Postconditions:
///   'buffer' is an allocated buffer
typedef OpaqueValue *allocateBuffer(ValueBuffer *buffer,
                                    const Metadata *self);

  
/// Given an unallocated buffer and an initialized buffer, move the
/// value from one buffer to the other, leaving the source buffer
/// unallocated.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// Therefore this can be decomposed as:
///
///   self->initializeBufferWithTake(dest, self->projectBuffer(src), self)
///   self->deallocateBuffer(src, self)
///
/// However, it may be more efficient because values stored out-of-line
/// may be moved by simply moving the buffer.
///
/// If the value is bitwise-takable or stored out of line, this is
/// equivalent to a memcpy of the buffers.
///
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an unallocated buffer
///   'src' is an initialized buffer
/// Postconditions:
///   'dest' is an initialized buffer
///   'src' is an unallocated buffer
typedef OpaqueValue *initializeBufferWithTakeOfBuffer(ValueBuffer *dest,
                                                      ValueBuffer *src,
                                                      const Metadata *self);
  
/// Given an initialized array of objects, destroy it.
///
/// Preconditions:
///   'object' is an initialized array of n objects
/// Postconditions:
///   'object' is an uninitialized array of n objects
typedef void destroyArray(OpaqueValue *array, size_t n,
                          const Metadata *self);
  
/// Given an uninitialized array and an initialized array, copy
/// the value.
///
/// This operation does not need to be safe against 'dest' and 'src' aliasing.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized array of n objects
/// Postconditions:
///   'dest' is an initialized array of n objects
/// Invariants:
///   'src' is an initialized array of n objects
typedef OpaqueValue *initializeArrayWithCopy(OpaqueValue *dest,
                                             OpaqueValue *src,
                                             size_t n,
                                             const Metadata *self);
  
/// Given an uninitialized array and an initialized array, move
/// the values from one to the other, leaving the source array
/// uninitialized.
///
/// This operation does not need to be safe against 'dest' and 'src' fully
/// overlapping. 'dest' may partially overlap the head of 'src', because the
/// values are taken as if in front-to-back order.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized array of n objects
///   'src' is an initialized array of n objects
/// Postconditions:
///   'dest' is an initialized array of n objects
///   'src' is an uninitialized array of n objects
typedef OpaqueValue *initializeArrayWithTakeFrontToBack(OpaqueValue *dest,
                                                        OpaqueValue *src,
                                                        size_t n,
                                                        const Metadata *self);
  
/// Given an uninitialized array and an initialized array, move
/// the values from one to the other, leaving the source array
/// uninitialized.
///
/// This operation does not need to be safe against 'dest' and 'src' fully
/// overlapping. 'dest' may partially overlap the tail of 'src', because the
/// values are taken as if in back-to-front order.
/// 
/// Returns the dest object.
///
/// Preconditions:
///   'dest' is an uninitialized array of n objects
///   'src' is an initialized array of n objects
/// Postconditions:
///   'dest' is an initialized array of n objects
///   'src' is an uninitialized array of n objects
typedef OpaqueValue *initializeArrayWithTakeBackToFront(OpaqueValue *dest,
                                                        OpaqueValue *src,
                                                        size_t n,
                                                        const Metadata *self);
  
/// The number of bytes required to store an object of this type.
/// This value may be zero.  This value is not necessarily a
/// multiple of the alignment.
typedef size_t size;

/// Flags which apply to the type here.
typedef ValueWitnessFlags flags;

/// When allocating an array of objects of this type, the number of bytes
/// between array elements.  This value may be zero.  This value is always
/// a multiple of the alignment.
typedef size_t stride;

/// Flags which describe extra inhabitants.
typedef ExtraInhabitantFlags extraInhabitantFlags;
  
/// Store an extra inhabitant, named by a unique positive or zero index,
/// into the given uninitialized storage for the type.
typedef void storeExtraInhabitant(OpaqueValue *dest,
                                  int index,
                                  const Metadata *self);
  
/// Get the extra inhabitant index for the bit pattern stored at the given
/// address, or return -1 if there is a valid value at the address.
typedef int getExtraInhabitantIndex(const OpaqueValue *src,
                                    const Metadata *self);

/// Given a valid object of this enum type, extracts the tag value indicating
/// which case of the enum is inhabited. Returned values are in the range
/// [-ElementsWithPayload..ElementsWithNoPayload-1].
///
/// The tag value can be used to index into the array returned by the
/// NominalTypeDescriptor's GetCaseTypes function to get the payload type
/// and check if the payload is indirect.
typedef int getEnumTag(const OpaqueValue *src,
                       const Metadata *self);

/// Given a valid object of this enum type, destructively strips the tag
/// bits, leaving behind a value of the inhabited case payload type.
/// If the case is indirect, the payload can then be projected from the box
/// with swift_projectBox().
typedef void destructiveProjectEnumData(OpaqueValue *src,
                                        const Metadata *self);

/// Given a valid object of an enum case payload's type, destructively add
/// the tag bits for the given case, leaving behind a fully-formed value of
/// the enum type. If the enum case does not have a payload, the initial
/// state of the value can be undefined. The given tag value must be in
/// the range [-ElementsWithPayload..ElementsWithNoPayload-1].
typedef void destructiveInjectEnumTag(OpaqueValue *src,
                                      int tag,
                                      const Metadata *self);

} // end namespace value_witness_types

/// A standard routine, suitable for placement in the value witness
/// table, for copying an opaque POD object.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_copyPOD(OpaqueValue *dest,
                           OpaqueValue *src,
                           const Metadata *self);

#define FOR_ALL_FUNCTION_VALUE_WITNESSES(MACRO) \
  MACRO(destroyBuffer) \
  MACRO(initializeBufferWithCopyOfBuffer) \
  MACRO(projectBuffer) \
  MACRO(deallocateBuffer) \
  MACRO(destroy) \
  MACRO(initializeBufferWithCopy) \
  MACRO(initializeWithCopy) \
  MACRO(assignWithCopy) \
  MACRO(initializeBufferWithTake) \
  MACRO(initializeWithTake) \
  MACRO(assignWithTake) \
  MACRO(allocateBuffer) \
  MACRO(initializeBufferWithTakeOfBuffer) \
  MACRO(destroyArray) \
  MACRO(initializeArrayWithCopy) \
  MACRO(initializeArrayWithTakeFrontToBack) \
  MACRO(initializeArrayWithTakeBackToFront)

struct TypeLayout;

/// A value-witness table.  A value witness table is built around
/// the requirements of some specific type.  The information in
/// a value-witness table is intended to be sufficient to lay out
/// and manipulate values of an arbitrary type.
struct ValueWitnessTable {
  // For the meaning of all of these witnesses, consult the comments
  // on their associated typedefs, above.

#define DECLARE_WITNESS(NAME) \
  value_witness_types::NAME *NAME;
  FOR_ALL_FUNCTION_VALUE_WITNESSES(DECLARE_WITNESS)
#undef DECLARE_WITNESS

  value_witness_types::size size;
  value_witness_types::flags flags;
  value_witness_types::stride stride;

  /// Would values of a type with the given layout requirements be
  /// allocated inline?
  static bool isValueInline(size_t size, size_t alignment) {
    return (size <= sizeof(ValueBuffer) &&
            alignment <= alignof(ValueBuffer));
  }

  /// Are values of this type allocated inline?
  bool isValueInline() const {
    return flags.isInlineStorage();
  }

  /// Is this type POD?
  bool isPOD() const {
    return flags.isPOD();
  }

  /// Is this type bitwise-takable?
  bool isBitwiseTakable() const {
    return flags.isBitwiseTakable();
  }

  /// Return the size of this type.  Unlike in C, this has not been
  /// padded up to the alignment; that value is maintained as
  /// 'stride'.
  size_t getSize() const {
    return size;
  }

  /// Return the stride of this type.  This is the size rounded up to
  /// be a multiple of the alignment.
  size_t getStride() const {
    return stride;
  }

  /// Return the alignment required by this type, in bytes.
  size_t getAlignment() const {
    return flags.getAlignment();
  }

  /// The alignment mask of this type.  An offset may be rounded up to
  /// the required alignment by adding this mask and masking by its
  /// bit-negation.
  ///
  /// For example, if the type needs to be 8-byte aligned, the value
  /// of this witness is 0x7.
  size_t getAlignmentMask() const {
    return flags.getAlignmentMask();
  }
  
  /// The number of extra inhabitants, that is, bit patterns that do not form
  /// valid values of the type, in this type's binary representation.
  unsigned getNumExtraInhabitants() const;

  /// Assert that this value witness table is an extra-inhabitants
  /// value witness table and return it as such.
  ///
  /// This has an awful name because it's supposed to be internal to
  /// this file.  Code outside this file should use LLVM's cast/dyn_cast.
  /// We don't want to use those here because we need to avoid accidentally
  /// introducing ABI dependencies on LLVM structures.
  const struct ExtraInhabitantsValueWitnessTable *_asXIVWT() const;

  /// Assert that this value witness table is an enum value witness table
  /// and return it as such.
  ///
  /// This has an awful name because it's supposed to be internal to
  /// this file.  Code outside this file should use LLVM's cast/dyn_cast.
  /// We don't want to use those here because we need to avoid accidentally
  /// introducing ABI dependencies on LLVM structures.
  const struct EnumValueWitnessTable *_asEVWT() const;

  /// Get the type layout record within this value witness table.
  const TypeLayout *getTypeLayout() const {
    return reinterpret_cast<const TypeLayout *>(&size);
  }
};
  
/// A value-witness table with extra inhabitants entry points.
/// These entry points are available only if the HasExtraInhabitants flag bit is
/// set in the 'flags' field.
struct ExtraInhabitantsValueWitnessTable : ValueWitnessTable {
  value_witness_types::extraInhabitantFlags extraInhabitantFlags;
  value_witness_types::storeExtraInhabitant *storeExtraInhabitant;
  value_witness_types::getExtraInhabitantIndex *getExtraInhabitantIndex;

  constexpr ExtraInhabitantsValueWitnessTable()
    : ValueWitnessTable{}, extraInhabitantFlags(),
      storeExtraInhabitant(nullptr),
      getExtraInhabitantIndex(nullptr) {}
  constexpr ExtraInhabitantsValueWitnessTable(
                            const ValueWitnessTable &base,
                            value_witness_types::extraInhabitantFlags eif,
                            value_witness_types::storeExtraInhabitant *sei,
                            value_witness_types::getExtraInhabitantIndex *geii)
    : ValueWitnessTable(base), extraInhabitantFlags(eif),
      storeExtraInhabitant(sei),
      getExtraInhabitantIndex(geii) {}

  static bool classof(const ValueWitnessTable *table) {
    return table->flags.hasExtraInhabitants();
  }
};

/// A value-witness table with enum entry points.
/// These entry points are available only if the HasEnumWitnesses flag bit is
/// set in the 'flags' field.
struct EnumValueWitnessTable : ExtraInhabitantsValueWitnessTable {
  value_witness_types::getEnumTag *getEnumTag;
  value_witness_types::destructiveProjectEnumData *destructiveProjectEnumData;
  value_witness_types::destructiveInjectEnumTag *destructiveInjectEnumTag;

  constexpr EnumValueWitnessTable()
    : ExtraInhabitantsValueWitnessTable(),
      getEnumTag(nullptr),
      destructiveProjectEnumData(nullptr),
      destructiveInjectEnumTag(nullptr) {}
  constexpr EnumValueWitnessTable(
          const ExtraInhabitantsValueWitnessTable &base,
          value_witness_types::getEnumTag *getEnumTag,
          value_witness_types::destructiveProjectEnumData *destructiveProjectEnumData,
          value_witness_types::destructiveInjectEnumTag *destructiveInjectEnumTag)
    : ExtraInhabitantsValueWitnessTable(base),
      getEnumTag(getEnumTag),
      destructiveProjectEnumData(destructiveProjectEnumData),
      destructiveInjectEnumTag(destructiveInjectEnumTag) {}

  static bool classof(const ValueWitnessTable *table) {
    return table->flags.hasEnumWitnesses();
  }
};

/// A type layout record. This is the subset of the value witness table that is
/// necessary to perform dependent layout of generic value types. It excludes
/// the value witness functions and includes only the size, alignment,
/// extra inhabitants, and miscellaneous flags about the type.
struct TypeLayout {
  value_witness_types::size size;
  value_witness_types::flags flags;
  value_witness_types::stride stride;

private:
  // Only available if the "hasExtraInhabitants" flag is set.
  value_witness_types::extraInhabitantFlags extraInhabitantFlags;

  void _static_assert_layout();
public:
  value_witness_types::extraInhabitantFlags getExtraInhabitantFlags() const {
    assert(flags.hasExtraInhabitants());
    return extraInhabitantFlags;
  }

  const TypeLayout *getTypeLayout() const { return this; }

  /// The number of extra inhabitants, that is, bit patterns that do not form
  /// valid values of the type, in this type's binary representation.
  unsigned getNumExtraInhabitants() const;
};

inline void TypeLayout::_static_assert_layout() {
  #define CHECK_TYPE_LAYOUT_OFFSET(FIELD)                               \
    static_assert(offsetof(ExtraInhabitantsValueWitnessTable, FIELD)    \
                    - offsetof(ExtraInhabitantsValueWitnessTable, size) \
                  == offsetof(TypeLayout, FIELD),                       \
                  "layout of " #FIELD " in TypeLayout doesn't match "   \
                  "value witness table")
  CHECK_TYPE_LAYOUT_OFFSET(size);
  CHECK_TYPE_LAYOUT_OFFSET(flags);
  CHECK_TYPE_LAYOUT_OFFSET(stride);
  CHECK_TYPE_LAYOUT_OFFSET(extraInhabitantFlags);

  #undef CHECK_TYPE_LAYOUT_OFFSET
}

inline const ExtraInhabitantsValueWitnessTable *
ValueWitnessTable::_asXIVWT() const {
  assert(ExtraInhabitantsValueWitnessTable::classof(this));
  return static_cast<const ExtraInhabitantsValueWitnessTable *>(this);
}
  
inline const EnumValueWitnessTable *
ValueWitnessTable::_asEVWT() const {
  assert(EnumValueWitnessTable::classof(this));
  return static_cast<const EnumValueWitnessTable *>(this);
}

inline unsigned ValueWitnessTable::getNumExtraInhabitants() const {
  // If the table does not have extra inhabitant witnesses, then there are zero.
  if (!flags.hasExtraInhabitants())
    return 0;
  return this->_asXIVWT()->extraInhabitantFlags.getNumExtraInhabitants();
}

inline unsigned TypeLayout::getNumExtraInhabitants() const {
  // If the table does not have extra inhabitant witnesses, then there are zero.
  if (!flags.hasExtraInhabitants())
    return 0;
  return extraInhabitantFlags.getNumExtraInhabitants();
}

// Standard value-witness tables.

// The "Int" tables are used for arbitrary POD data with the matching
// size/alignment characteristics.
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi8_);   // Builtin.Int8
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi16_);  // Builtin.Int16
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi32_);  // Builtin.Int32
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi64_);  // Builtin.Int64
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi128_); // Builtin.Int128
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(Bi256_); // Builtin.Int256

// The object-pointer table can be used for arbitrary Swift refcounted
// pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bo); // Builtin.NativeObject
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable UNOWNED_VALUE_WITNESS_SYM(Bo); // unowned Builtin.NativeObject
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable WEAK_VALUE_WITNESS_SYM(Bo); // weak Builtin.NativeObject?

SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bb); // Builtin.BridgeObject

SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(Bp); // Builtin.RawPointer

#if SWIFT_OBJC_INTEROP
// The ObjC-pointer table can be used for arbitrary ObjC pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable VALUE_WITNESS_SYM(BO); // Builtin.UnknownObject
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable UNOWNED_VALUE_WITNESS_SYM(BO); // unowned Builtin.UnknownObject
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable WEAK_VALUE_WITNESS_SYM(BO); // weak Builtin.UnknownObject?
#endif

// The () -> () table can be used for arbitrary function types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable
  VALUE_WITNESS_SYM(FUNCTION_MANGLING);     // () -> ()

// The @convention(thin) () -> () table can be used for arbitrary thin function types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable
  VALUE_WITNESS_SYM(THIN_FUNCTION_MANGLING);    // @convention(thin) () -> ()

// The () table can be used for arbitrary empty types.
SWIFT_RUNTIME_EXPORT
const ValueWitnessTable VALUE_WITNESS_SYM(EMPTY_TUPLE_MANGLING);        // ()

// The table for aligned-pointer-to-pointer types.
SWIFT_RUNTIME_EXPORT
const ExtraInhabitantsValueWitnessTable METATYPE_VALUE_WITNESS_SYM(Bo); // Builtin.NativeObject.Type

/// Return the value witnesses for unmanaged pointers.
static inline const ValueWitnessTable &getUnmanagedPointerValueWitnesses() {
#ifdef __LP64__
  return VALUE_WITNESS_SYM(Bi64_);
#else
  return VALUE_WITNESS_SYM(Bi32_);
#endif
}

/// Return value witnesses for a pointer-aligned pointer type.
static inline
const ExtraInhabitantsValueWitnessTable &
getUnmanagedPointerPointerValueWitnesses() {
  return METATYPE_VALUE_WITNESS_SYM(Bo);
}

/// The header before a metadata object which appears on all type
/// metadata.  Note that heap metadata are not necessarily type
/// metadata, even for objects of a heap type: for example, objects of
/// Objective-C type possess a form of heap metadata (an Objective-C
/// Class pointer), but this metadata lacks the type metadata header.
/// This case can be distinguished using the isTypeMetadata() flag
/// on ClassMetadata.
struct TypeMetadataHeader {
  /// A pointer to the value-witnesses for this type.  This is only
  /// present for type metadata.
  const ValueWitnessTable *ValueWitnesses;
};

/// A "full" metadata pointer is simply an adjusted address point on a
/// metadata object; it points to the beginning of the metadata's
/// allocation, rather than to the canonical address point of the
/// metadata object.
template <class T> struct FullMetadata : T::HeaderType, T {
  typedef typename T::HeaderType HeaderType;

  FullMetadata() = default;
  constexpr FullMetadata(const HeaderType &header, const T &metadata)
    : HeaderType(header), T(metadata) {}
};

/// Given a canonical metadata pointer, produce the adjusted metadata pointer.
template <class T>
static inline FullMetadata<T> *asFullMetadata(T *metadata) {
  return (FullMetadata<T>*) (((typename T::HeaderType*) metadata) - 1);
}
template <class T>
static inline const FullMetadata<T> *asFullMetadata(const T *metadata) {
  return asFullMetadata(const_cast<T*>(metadata));
}

// std::result_of is busted in Xcode 5. This is a simplified reimplementation
// that isn't SFINAE-safe.
namespace {
  template<typename T> struct _ResultOf;
  
  template<typename R, typename...A>
  struct _ResultOf<R(A...)> {
    using type = R;
  };
}
  
namespace heap_object_abi {
  
// The extra inhabitants and spare bits of heap object pointers.
// These must align with the values in IRGen's SwiftTargetInfo.cpp.
#if defined(__x86_64__)

# ifdef __APPLE__
static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DARWIN_X86_64_LEAST_VALID_POINTER;
# else
static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER;
# endif
static const uintptr_t SwiftSpareBitsMask =
  SWIFT_ABI_X86_64_SWIFT_SPARE_BITS_MASK;
static const uintptr_t ObjCReservedBitsMask =
  SWIFT_ABI_X86_64_OBJC_RESERVED_BITS_MASK;
static const unsigned ObjCReservedLowBits =
  SWIFT_ABI_X86_64_OBJC_NUM_RESERVED_LOW_BITS;

#elif defined(__arm64__)

# ifdef __APPLE__
static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DARWIN_ARM64_LEAST_VALID_POINTER;
# else
static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER;
# endif
static const uintptr_t SwiftSpareBitsMask =
  SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK;
static const uintptr_t ObjCReservedBitsMask =
  SWIFT_ABI_ARM64_OBJC_RESERVED_BITS_MASK;
static const unsigned ObjCReservedLowBits =
  SWIFT_ABI_ARM64_OBJC_NUM_RESERVED_LOW_BITS;

#elif defined(__powerpc64__)

static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER;
static const uintptr_t SwiftSpareBitsMask =
  SWIFT_ABI_POWERPC64_SWIFT_SPARE_BITS_MASK;
static const uintptr_t ObjCReservedBitsMask =
  SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK;
static const unsigned ObjCReservedLowBits =
  SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS;

#elif defined(__s390x__)

static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER;
static const uintptr_t SwiftSpareBitsMask =
  SWIFT_ABI_S390X_SWIFT_SPARE_BITS_MASK;
static const uintptr_t ObjCReservedBitsMask =
  SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK;
static const unsigned ObjCReservedLowBits =
  SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS;

#else

static const uintptr_t LeastValidPointerValue =
  SWIFT_ABI_DEFAULT_LEAST_VALID_POINTER;
static const uintptr_t SwiftSpareBitsMask =
# if __i386__
  SWIFT_ABI_I386_SWIFT_SPARE_BITS_MASK
# elif __arm__
  SWIFT_ABI_ARM_SWIFT_SPARE_BITS_MASK
# else
  SWIFT_ABI_DEFAULT_SWIFT_SPARE_BITS_MASK
# endif
  ;
static const uintptr_t ObjCReservedBitsMask =
  SWIFT_ABI_DEFAULT_OBJC_RESERVED_BITS_MASK;
static const unsigned ObjCReservedLowBits =
  SWIFT_ABI_DEFAULT_OBJC_NUM_RESERVED_LOW_BITS;

#endif

}
  
template <typename Runtime> struct TargetNominalTypeDescriptor;
template <typename Runtime> struct TargetGenericMetadata;
template <typename Runtime> struct TargetClassMetadata;
template <typename Runtime> struct TargetStructMetadata;
template <typename Runtime> struct TargetOpaqueMetadata;

// FIXME: https://bugs.swift.org/browse/SR-1155
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"

extern uint64_t RelativeDirectPointerNullPtr;

#define RelativeDirectPointerNullPtrRef                                        \
  *reinterpret_cast<ConstTargetFarRelativeDirectPointer<                       \
      Runtime, TargetNominalTypeDescriptor, /*nullable*/ true> *>(             \
      &RelativeDirectPointerNullPtr)

/// The common structure of all type metadata.
template <typename Runtime>
struct TargetMetadata {
  using StoredPointer = typename Runtime::StoredPointer;

  constexpr TargetMetadata()
    : Kind(static_cast<StoredPointer>(MetadataKind::Class)) {}
  constexpr TargetMetadata(MetadataKind Kind)
    : Kind(static_cast<StoredPointer>(Kind)) {}
  
  /// The basic header type.
  typedef TypeMetadataHeader HeaderType;

private:
  /// The kind. Only valid for non-class metadata; getKind() must be used to get
  /// the kind value.
  StoredPointer Kind;
public:
  /// Get the metadata kind.
  MetadataKind getKind() const {
    return getEnumeratedMetadataKind(Kind);
  }
  
  /// Set the metadata kind.
  void setKind(MetadataKind kind) {
    Kind = static_cast<StoredPointer>(kind);
  }

  /// Is this a class object--the metadata record for a Swift class (which also
  /// serves as the class object), or the class object for an ObjC class (which
  /// is not metadata)?
  bool isClassObject() const {
    return static_cast<MetadataKind>(getKind()) == MetadataKind::Class;
  }
  
  /// Does the given metadata kind represent metadata for some kind of class?
  static bool isAnyKindOfClass(MetadataKind k) {
    switch (k) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
      return true;

    case MetadataKind::Function:
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
    case MetadataKind::Existential:
    case MetadataKind::Metatype:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
      return false;
    }
    
    swift_runtime_unreachable("Unhandled MetadataKind in switch.");
  }
  
  /// Is this metadata for an existential type?
  bool isAnyExistentialType() const {
    switch (getKind()) {
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Existential:
      return true;
        
    case MetadataKind::Metatype:
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
    case MetadataKind::Function:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
      return false;
    }

    swift_runtime_unreachable("Unhandled MetadataKind in switch.");
  }
  
  /// Is this either type metadata or a class object for any kind of class?
  bool isAnyClass() const {
    return isAnyKindOfClass(getKind());
  }

  const ValueWitnessTable *getValueWitnesses() const {
    return asFullMetadata(this)->ValueWitnesses;
  }

  const TypeLayout *getTypeLayout() const {
    return getValueWitnesses()->getTypeLayout();
  }

  void setValueWitnesses(const ValueWitnessTable *table) {
    asFullMetadata(this)->ValueWitnesses = table;
  }
  
  // Define forwarders for value witnesses. These invoke this metadata's value
  // witness table with itself as the 'self' parameter.
  #define FORWARD_WITNESS(WITNESS)                                         \
    template<typename...A>                                                 \
    _ResultOf<value_witness_types::WITNESS>::type                          \
    vw_##WITNESS(A &&...args) const {                                      \
      return getValueWitnesses()->WITNESS(std::forward<A>(args)..., this); \
    }
  FOR_ALL_FUNCTION_VALUE_WITNESSES(FORWARD_WITNESS)
  #undef FORWARD_WITNESS

  int vw_getExtraInhabitantIndex(const OpaqueValue *value) const  {
    return getValueWitnesses()->_asXIVWT()->getExtraInhabitantIndex(value, this);
  }
  void vw_storeExtraInhabitant(OpaqueValue *value, int index) const {
    getValueWitnesses()->_asXIVWT()->storeExtraInhabitant(value, index, this);
  }

  int vw_getEnumTag(const OpaqueValue *value) const {
    return getValueWitnesses()->_asEVWT()->getEnumTag(value, this);
  }
  void vw_destructiveProjectEnumData(OpaqueValue *value) const {
    getValueWitnesses()->_asEVWT()->destructiveProjectEnumData(value, this);
  }
  void vw_destructiveInjectEnumTag(OpaqueValue *value, unsigned tag) const {
    getValueWitnesses()->_asEVWT()->destructiveInjectEnumTag(value, tag, this);
  }
  
  /// Get the nominal type descriptor if this metadata describes a nominal type,
  /// or return null if it does not.
  const ConstTargetFarRelativeDirectPointer<Runtime,
                                            TargetNominalTypeDescriptor,
                                            /*nullable*/ true> &
  getNominalTypeDescriptor() const {
    switch (getKind()) {
    case MetadataKind::Class: {
      const auto cls = static_cast<const TargetClassMetadata<Runtime> *>(this);
      if (!cls->isTypeMetadata())
        return RelativeDirectPointerNullPtrRef;
      if (cls->isArtificialSubclass())
        return RelativeDirectPointerNullPtrRef;
      return cls->getDescription();
    }
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      return static_cast<const TargetStructMetadata<Runtime> *>(this)->Description;
    case MetadataKind::ForeignClass:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
    case MetadataKind::Function:
    case MetadataKind::Existential:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::Metatype:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
      return RelativeDirectPointerNullPtrRef;
    }

    swift_runtime_unreachable("Unhandled MetadataKind in switch.");
  }
  
  /// Get the generic metadata pattern from which this generic type instance was
  /// instantiated, or null if the type is not generic.
  const TargetGenericMetadata<Runtime> *getGenericPattern() const;
  
  /// Get the class object for this type if it has one, or return null if the
  /// type is not a class (or not a class with a class object).
  const TargetClassMetadata<Runtime> *getClassObject() const;
  
protected:
  friend struct TargetOpaqueMetadata<Runtime>;
  
  /// Metadata should not be publicly copied or moved.
  constexpr TargetMetadata(const TargetMetadata &) = default;
  TargetMetadata &operator=(const TargetMetadata &) = default;
  constexpr TargetMetadata(TargetMetadata &&) = default;
  TargetMetadata &operator=(TargetMetadata &&) = default;
};

/// The common structure of opaque metadata.  Adds nothing.
template <typename Runtime>
struct TargetOpaqueMetadata {
  typedef TypeMetadataHeader HeaderType;

  // We have to represent this as a member so we can list-initialize it.
  TargetMetadata<Runtime> base;
};
using OpaqueMetadata = TargetOpaqueMetadata<InProcess>;

// Standard POD opaque metadata.
// The "Int" metadata are used for arbitrary POD data with the
// matching characteristics.
using FullOpaqueMetadata = FullMetadata<OpaqueMetadata>;
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi8_);      // Builtin.Int8
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi16_);     // Builtin.Int16
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi32_);     // Builtin.Int32
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi64_);     // Builtin.Int64
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi128_);    // Builtin.Int128
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bi256_);    // Builtin.Int256
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bo);        // Builtin.NativeObject
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bb);        // Builtin.BridgeObject
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(Bp);        // Builtin.RawPointer
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(BB);        // Builtin.UnsafeValueBuffer
#if SWIFT_OBJC_INTEROP
SWIFT_RUNTIME_EXPORT
const FullOpaqueMetadata METADATA_SYM(BO);        // Builtin.UnknownObject
#endif

/// The prefix on a heap metadata.
struct HeapMetadataHeaderPrefix {
  /// Destroy the object, returning the allocated size of the object
  /// or 0 if the object shouldn't be deallocated.
  SWIFT_CC(swift) void (*destroy)(SWIFT_CONTEXT HeapObject *);
};

/// The header present on all heap metadata.
struct HeapMetadataHeader : HeapMetadataHeaderPrefix, TypeMetadataHeader {
  constexpr HeapMetadataHeader(const HeapMetadataHeaderPrefix &heapPrefix,
                               const TypeMetadataHeader &typePrefix)
    : HeapMetadataHeaderPrefix(heapPrefix), TypeMetadataHeader(typePrefix) {}
};

/// The common structure of all metadata for heap-allocated types.  A
/// pointer to one of these can be retrieved by loading the 'isa'
/// field of any heap object, whether it was managed by Swift or by
/// Objective-C.  However, when loading from an Objective-C object,
/// this metadata may not have the heap-metadata header, and it may
/// not be the Swift type metadata for the object's dynamic type.
template <typename Runtime>
struct TargetHeapMetadata : TargetMetadata<Runtime> {
  typedef HeapMetadataHeader HeaderType;

  TargetHeapMetadata() = default;
  constexpr TargetHeapMetadata(const TargetMetadata<Runtime> &base)
    : TargetMetadata<Runtime>(base) {}
};
using HeapMetadata = TargetHeapMetadata<InProcess>;

/// Header for a generic parameter descriptor. This is a variable-sized
/// structure that describes how to find and parse a generic parameter vector
/// within the type metadata for an instance of a nominal type.
struct GenericParameterDescriptor {
  /// The offset to the first generic argument from the start of
  /// metadata record.
  ///
  /// This is meaningful if either NumGenericRequirements is nonzero or
  /// (for classes) if Flags.hasParent() is true.
  uint32_t Offset;

  /// The amount of generic requirement data in the metadata record, in
  /// words, excluding the lexical parent type.  A value of zero means
  /// there is no generic requirement data.
  ///
  /// This may include protocol witness tables for type parameters or
  /// their associated types.
  uint32_t NumGenericRequirements;

  /// The number of primary type parameters. This is always less than or equal
  /// to NumGenericRequirements; it counts only the type parameters
  /// and not any required witness tables.
  uint32_t NumPrimaryParams;

  /// Flags for this generic parameter descriptor.
  GenericParameterDescriptorFlags Flags;

  /// True if the nominal type has generic requirements other than its
  /// parent metadata.
  bool hasGenericRequirements() const { return NumGenericRequirements > 0; }

  /// True if the nominal type is generic in any way.
  bool isGeneric() const {
    return hasGenericRequirements() || Flags.hasGenericParent();
  }

  // TODO: add meaningful descriptions of the generic requirements.
};
  
struct ClassTypeDescriptor;
struct StructTypeDescriptor;
struct EnumTypeDescriptor;

/// Common information about all nominal types. For generic types, this
/// descriptor is shared for all instantiations of the generic type.
template <typename Runtime>
struct TargetNominalTypeDescriptor {
  using StoredPointer = typename Runtime::StoredPointer;
  /// The mangled name of the nominal type.
  TargetRelativeDirectPointer<Runtime, const char> Name;
  
  /// The following fields are kind-dependent.
  union {
    /// Information about class types.
    struct {
      /// The number of stored properties in the class, not including its
      /// superclasses. If there is a field offset vector, this is its length.
      uint32_t NumFields;
      /// The offset of the field offset vector for this class's stored
      /// properties in its metadata, if any. 0 means there is no field offset
      /// vector.
      ///
      /// To deal with resilient superclasses correctly, this will
      /// eventually need to be relative to the start of this class's
      /// metadata area.
      uint32_t FieldOffsetVectorOffset;
      
      /// The field names. A doubly-null-terminated list of strings, whose
      /// length and order is consistent with that of the field offset vector.
      RelativeDirectPointer<const char, /*nullable*/ true> FieldNames;
      
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// field offset vector.
      RelativeDirectPointer<const FieldType *
        (const TargetMetadata<Runtime> *)> GetFieldTypes;

      /// True if metadata records for this type have a field offset vector for
      /// its stored properties.
      bool hasFieldOffsetVector() const { return FieldOffsetVectorOffset != 0; }      
    } Class;
    
    /// Information about struct types.
    struct {
      /// The number of stored properties in the class, not including its
      /// superclasses. If there is a field offset vector, this is its length.
      uint32_t NumFields;
      /// The offset of the field offset vector for this class's stored
      /// properties in its metadata, if any. 0 means there is no field offset
      /// vector.
      uint32_t FieldOffsetVectorOffset;
      
      /// The field names. A doubly-null-terminated list of strings, whose
      /// length and order is consistent with that of the field offset vector.
      RelativeDirectPointer<const char, /*nullable*/ true> FieldNames;
      
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// field offset vector.
      RelativeDirectPointer<const FieldType *
        (const TargetMetadata<Runtime> *)> GetFieldTypes;

      /// True if metadata records for this type have a field offset vector for
      /// its stored properties.
      bool hasFieldOffsetVector() const { return FieldOffsetVectorOffset != 0; }
    } Struct;
    
    /// Information about enum types.
    struct {
      /// The number of non-empty cases in the enum are in the low 24 bits;
      /// the offset of the payload size in the metadata record in words,
      /// if any, is stored in the high 8 bits.
      uint32_t NumPayloadCasesAndPayloadSizeOffset;
      /// The number of empty cases in the enum.
      uint32_t NumEmptyCases;
      /// The names of the cases. A doubly-null-terminated list of strings,
      /// whose length is NumNonEmptyCases + NumEmptyCases. Cases are named in
      /// tag order, non-empty cases first, followed by empty cases.
      RelativeDirectPointer<const char, /*nullable*/ true> CaseNames;
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// CaseNames. Only types for payload cases are provided.
      RelativeDirectPointer<
        const FieldType * (const TargetMetadata<Runtime> *)>
        GetCaseTypes;

      uint32_t getNumPayloadCases() const {
        return NumPayloadCasesAndPayloadSizeOffset & 0x00FFFFFFU;
      }
      uint32_t getNumEmptyCases() const {
        return NumEmptyCases;
      }
      uint32_t getNumCases() const {
        return getNumPayloadCases() + NumEmptyCases;
      }
      size_t getPayloadSizeOffset() const {
        return ((NumPayloadCasesAndPayloadSizeOffset & 0xFF000000U) >> 24);
      }
      
      bool hasPayloadSizeOffset() const {
        return getPayloadSizeOffset() != 0;
      }
    } Enum;
  };
  
  RelativeDirectPointerIntPair<TargetGenericMetadata<Runtime>,
                               NominalTypeKind, /*Nullable*/ true>
    GenericMetadataPatternAndKind;

  using NonGenericMetadataAccessFunction = const Metadata *();

  /// A pointer to the metadata access function for this type.
  ///
  /// The type of the returned function is speculative; in reality, it
  /// takes one argument for each of the generic requirements, in the order
  /// they are listed.  Therefore, the function type is correct only if
  /// this type is non-generic.
  ///
  /// Not all type metadata have access functions.
  TargetRelativeDirectPointer<Runtime, NonGenericMetadataAccessFunction,
                              /*nullable*/ true> AccessFunction;

  /// A pointer to the generic metadata pattern that is used to instantiate
  /// instances of this type. Zero if the type is not generic.
  TargetGenericMetadata<Runtime> *getGenericMetadataPattern() const {
    return const_cast<TargetGenericMetadata<Runtime>*>(
                                    GenericMetadataPatternAndKind.getPointer());
  }

  NonGenericMetadataAccessFunction *getAccessFunction() const {
    return AccessFunction.get();
  }

  NominalTypeKind getKind() const {
    return GenericMetadataPatternAndKind.getInt();
  }

  int32_t offsetToNameOffset() const {
    return offsetof(TargetNominalTypeDescriptor<Runtime>, Name);
  }

  /// The generic parameter descriptor header. This describes how to find and
  /// parse the generic parameter vector in metadata records for this nominal
  /// type.
  GenericParameterDescriptor GenericParams;
  
  // NOTE: GenericParams ends with a tail-allocated array, so it cannot be
  // followed by additional fields.
};
using NominalTypeDescriptor = TargetNominalTypeDescriptor<InProcess>;

typedef SWIFT_CC(swift) void (*ClassIVarDestroyer)(SWIFT_CONTEXT HeapObject *);

/// The structure of all class metadata.  This structure is embedded
/// directly within the class's heap metadata structure and therefore
/// cannot be extended without an ABI break.
///
/// Note that the layout of this type is compatible with the layout of
/// an Objective-C class.
template <typename Runtime>
struct TargetClassMetadata : public TargetHeapMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;
  friend class ReflectionContext;
  TargetClassMetadata() = default;
  constexpr TargetClassMetadata(const TargetHeapMetadata<Runtime> &base,
             ConstTargetMetadataPointer<Runtime, swift::TargetClassMetadata> superClass,
             StoredPointer data,
             ClassFlags flags,
             ClassIVarDestroyer ivarDestroyer,
             StoredPointer size, StoredPointer addressPoint,
             StoredPointer alignMask,
             StoredPointer classSize, StoredPointer classAddressPoint)
    : TargetHeapMetadata<Runtime>(base), SuperClass(superClass),
      CacheData {0, 0}, Data(data),
      Flags(flags), InstanceAddressPoint(addressPoint),
      InstanceSize(size), InstanceAlignMask(alignMask),
      Reserved(0), ClassSize(classSize), ClassAddressPoint(classAddressPoint),
      Description(nullptr), IVarDestroyer(ivarDestroyer) {}

  // Description's copy ctor is deleted so we have to do this the hard way.
  TargetClassMetadata(const TargetClassMetadata& other)
    : TargetHeapMetadata<Runtime>(other),
      SuperClass(other.SuperClass),
      CacheData{other.CacheData[0], other.CacheData[1]},
      Data(other.Data),
      Flags(other.Flags),
      InstanceAddressPoint(other.InstanceAddressPoint),
      InstanceSize(other.InstanceSize),
      InstanceAlignMask(other.InstanceAlignMask),
      Reserved(other.Reserved),
      ClassSize(other.ClassSize),
      ClassAddressPoint(other.ClassAddressPoint),
      Description(other.Description.get()),
      IVarDestroyer(other.IVarDestroyer) {}

  /// The metadata for the superclass.  This is null for the root class.
  ConstTargetMetadataPointer<Runtime, swift::TargetClassMetadata> SuperClass;

  /// The cache data is used for certain dynamic lookups; it is owned
  /// by the runtime and generally needs to interoperate with
  /// Objective-C's use.
  StoredPointer CacheData[2];

  /// The data pointer is used for out-of-line metadata and is
  /// generally opaque, except that the compiler sets the low bit in
  /// order to indicate that this is a Swift metatype and therefore
  /// that the type metadata header is present.
  StoredPointer Data;

  static constexpr StoredPointer offsetToData() {
    return offsetof(TargetClassMetadata, Data);
  }

  /// Is this object a valid swift type metadata?
  bool isTypeMetadata() const {
    return (Data & 1);
  }
  /// A different perspective on the same bit
  bool isPureObjC() const {
    return !isTypeMetadata();
  }

private:
  // The remaining fields are valid only when isTypeMetadata().
  // The Objective-C runtime knows the offsets to some of these fields.
  // Be careful when changing them.

  /// Swift-specific class flags.
  ClassFlags Flags;

  /// The address point of instances of this type.
  uint32_t InstanceAddressPoint;

  /// The required size of instances of this type.
  /// 'InstanceAddressPoint' bytes go before the address point;
  /// 'InstanceSize - InstanceAddressPoint' bytes go after it.
  uint32_t InstanceSize;

  /// The alignment mask of the address point of instances of this type.
  uint16_t InstanceAlignMask;

  /// Reserved for runtime use.
  uint16_t Reserved;

  /// The total size of the class object, including prefix and suffix
  /// extents.
  uint32_t ClassSize;

  /// The offset of the address point within the class object.
  uint32_t ClassAddressPoint;

  /// An out-of-line Swift-specific description of the type, or null
  /// if this is an artificial subclass.  We currently provide no
  /// supported mechanism for making a non-artificial subclass
  /// dynamically.
  ConstTargetFarRelativeDirectPointer<Runtime, TargetNominalTypeDescriptor,
                                      /*nullable*/ true> Description;

  /// A function for destroying instance variables, used to clean up
  /// after an early return from a constructor.
  ClassIVarDestroyer IVarDestroyer; // TODO: Make target-agnostic size

  // After this come the class members, laid out as follows:
  //   - class members for the superclass (recursively)
  //   - metadata reference for the parent, if applicable
  //   - generic parameters for this class
  //   - class variables (if we choose to support these)
  //   - "tabulated" virtual methods

public:
  const ConstTargetFarRelativeDirectPointer<Runtime,
                                            TargetNominalTypeDescriptor,
                                            /*nullable*/ true> &
  getDescription() const {
    assert(isTypeMetadata());
    assert(!isArtificialSubclass());
    return Description;
  }
  
  void setDescription(const TargetNominalTypeDescriptor<Runtime> *
                      description) {
    Description = description;
  }

  ClassIVarDestroyer getIVarDestroyer() const {
    assert(isTypeMetadata());
    return IVarDestroyer;
  }

  /// Is this class an artificial subclass, such as one dynamically
  /// created for various dynamic purposes like KVO?
  bool isArtificialSubclass() const {
    assert(isTypeMetadata());
    return Description == 0;
  }
  void setArtificialSubclass() {
    assert(isTypeMetadata());
    Description = 0;
  }

  ClassFlags getFlags() const {
    assert(isTypeMetadata());
    return Flags;
  }
  void setFlags(ClassFlags flags) {
    assert(isTypeMetadata());
    Flags = flags;
  }

  StoredSize getInstanceSize() const {
    assert(isTypeMetadata());
    return InstanceSize;
  }
  void setInstanceSize(StoredSize size) {
    assert(isTypeMetadata());
    InstanceSize = size;
  }

  StoredPointer getInstanceAddressPoint() const {
    assert(isTypeMetadata());
    return InstanceAddressPoint;
  }
  void setInstanceAddressPoint(StoredSize size) {
    assert(isTypeMetadata());
    InstanceAddressPoint = size;
  }

  StoredPointer getInstanceAlignMask() const {
    assert(isTypeMetadata());
    return InstanceAlignMask;
  }
  void setInstanceAlignMask(StoredSize mask) {
    assert(isTypeMetadata());
    InstanceAlignMask = mask;
  }

  StoredPointer getClassSize() const {
    assert(isTypeMetadata());
    return ClassSize;
  }
  void setClassSize(StoredSize size) {
    assert(isTypeMetadata());
    ClassSize = size;
  }

  StoredPointer getClassAddressPoint() const {
    assert(isTypeMetadata());
    return ClassAddressPoint;
  }
  void setClassAddressPoint(StoredSize offset) {
    assert(isTypeMetadata());
    ClassAddressPoint = offset;
  }

  uint16_t getRuntimeReservedData() const {
    assert(isTypeMetadata());
    return Reserved;
  }
  void setRuntimeReservedData(uint16_t data) {
    assert(isTypeMetadata());
    Reserved = data;
  }

  /// Get a pointer to the field offset vector, if present, or null.
  const StoredPointer *getFieldOffsets() const {
    assert(isTypeMetadata());
    auto offset = getDescription()->Class.FieldOffsetVectorOffset;
    if (offset == 0)
      return nullptr;
    auto asWords = reinterpret_cast<const void * const*>(this);
    return reinterpret_cast<const StoredPointer *>(asWords + offset);
  }
  
  /// Get a pointer to the field type vector, if present, or null.
  const FieldType *getFieldTypes() const {
    assert(isTypeMetadata());
    auto *getter = getDescription()->Class.GetFieldTypes.get();
    if (!getter)
      return nullptr;
    
    return getter(this);
  }

  /// Return the parent type for a given level in the class hierarchy, or
  /// null if that level does not have a parent type.
  const TargetMetadata<Runtime> *
  getParentType(const TargetNominalTypeDescriptor<Runtime> *theClass) const {
    if (!theClass->GenericParams.Flags.hasParent())
      return nullptr;

    auto metadataAsWords = reinterpret_cast<const Metadata * const *>(this);
    return metadataAsWords[theClass->GenericParams.Offset - 1];
  }

  StoredPointer offsetToDescriptorOffset() const {
    return offsetof(TargetClassMetadata<Runtime>, Description);
  }

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Class;
  }
};
using ClassMetadata = TargetClassMetadata<InProcess>;

/// The structure of metadata for heap-allocated local variables.
/// This is non-type metadata.
template <typename Runtime>
struct TargetHeapLocalVariableMetadata
  : public TargetHeapMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  uint32_t OffsetToFirstCapture;
  TargetPointer<Runtime, const char> CaptureDescription;

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::HeapLocalVariable;
  }
};
using HeapLocalVariableMetadata
  = TargetHeapLocalVariableMetadata<InProcess>;

/// The structure of wrapper metadata for Objective-C classes.  This
/// is used as a type metadata pointer when the actual class isn't
/// Swift-compiled.
template <typename Runtime>
struct TargetObjCClassWrapperMetadata : public TargetMetadata<Runtime> {
  ConstTargetMetadataPointer<Runtime, TargetClassMetadata> Class;

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::ObjCClassWrapper;
  }
};
using ObjCClassWrapperMetadata
  = TargetObjCClassWrapperMetadata<InProcess>;

// FIXME: Workaround for rdar://problem/18889711. 'Consume' does not require
// a barrier on ARM64, but LLVM doesn't know that. Although 'relaxed'
// is formally UB by C++11 language rules, we should be OK because neither
// the processor model nor the optimizer can realistically reorder our uses
// of 'consume'.
#if __arm64__ || __arm__
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_relaxed)
#else
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_consume)
#endif

/// The structure of metadata for foreign types where the source
/// language doesn't provide any sort of more interesting metadata for
/// us to use.
template <typename Runtime>
struct TargetForeignTypeMetadata : public TargetMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;
  using InitializationFunction_t =
    void (*)(TargetForeignTypeMetadata<Runtime> *selectedMetadata);
  using RuntimeMetadataPointer =
      ConstTargetMetadataPointer<Runtime, swift::TargetForeignTypeMetadata>;

  /// Foreign type metadata may have extra header fields depending on
  /// the flags.
  struct HeaderPrefix {
    /// An optional callback performed when a particular metadata object
    /// is chosen as the unique structure.
    /// If there is no initialization function, this metadata record can be
    /// assumed to be immutable (except for the \c Unique invasive cache
    /// field).
    InitializationFunction_t InitializationFunction;
    
    /// The Swift-mangled name of the type. This is the uniquing key for the
    /// type.
    TargetPointer<Runtime, const char> Name;

    /// A pointer to the actual, runtime-uniqued metadata for this
    /// type.  This is essentially an invasive cache for the lookup
    /// structure.
    mutable std::atomic<RuntimeMetadataPointer> Unique;

    /// Various flags.
    enum : StoredSize {
      /// This metadata has an initialization callback function.  If
      /// this flag is not set, the metadata object needn't actually
      /// have a InitializationFunction field.
      HasInitializationFunction = 0x1,
    } Flags;
  };

  struct HeaderType : HeaderPrefix, TypeMetadataHeader {};

  static constexpr int OffsetToName =
    (int) offsetof(HeaderType, Name) - (int) sizeof(HeaderType);

  TargetPointer<Runtime, const char> getName() const {
    return reinterpret_cast<TargetPointer<Runtime, const char>>(
      asFullMetadata(this)->Name);
  }

  RuntimeMetadataPointer getCachedUniqueMetadata() const {
#if __alpha__
    // TODO: This can be a relaxed-order load if there is no initialization
    // function. On platforms we care about, consume is no more expensive than
    // relaxed, so there's no reason to branch here (and LLVM isn't smart
    // enough to eliminate it when it's not needed).
    if (!hasInitializationFunction())
      return asFullMetadata(this)->Unique.load(std::memory_order_relaxed);
#endif
    return asFullMetadata(this)->Unique.load(SWIFT_MEMORY_ORDER_CONSUME);
  }

  void setCachedUniqueMetadata(RuntimeMetadataPointer unique) const {
    assert((static_cast<RuntimeMetadataPointer>(asFullMetadata(this)->Unique) ==
                nullptr ||
            asFullMetadata(this)->Unique == unique) &&
           "already set unique metadata");

    // If there is no initialization function, this can be a relaxed store.
    if (!hasInitializationFunction())
      asFullMetadata(this)->Unique.store(unique, std::memory_order_relaxed);
    
    // Otherwise, we need a release store to publish the result of
    // initialization
    else
      asFullMetadata(this)->Unique.store(unique, std::memory_order_release);
  }
  
  StoredSize getFlags() const {
    return asFullMetadata(this)->Flags;
  }

  bool hasInitializationFunction() const {
    return getFlags() & HeaderPrefix::HasInitializationFunction;
  }

  InitializationFunction_t getInitializationFunction() const {
    assert(hasInitializationFunction());
    return asFullMetadata(this)->InitializationFunction;
  }
};
using ForeignTypeMetadata = TargetForeignTypeMetadata<InProcess>;

/// The structure of metadata objects for foreign class types.
/// A foreign class is a foreign type with reference semantics and
/// Swift-supported reference counting.  Generally this requires
/// special logic in the importer.
///
/// We assume for now that foreign classes are entirely opaque
/// to Swift introspection.
template <typename Runtime>
struct TargetForeignClassMetadata
  : public TargetForeignTypeMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;

  /// The superclass of the foreign class, if any.
  ConstTargetMetadataPointer<Runtime, swift::TargetForeignClassMetadata>
  SuperClass;

  /// Reserved space.  For now, these should be zero-initialized.
  StoredPointer Reserved[3];

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::ForeignClass;
  }
};
using ForeignClassMetadata = TargetForeignClassMetadata<InProcess>;

/// The common structure of metadata for structs and enums.
template <typename Runtime>
struct TargetValueMetadata : public TargetMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  TargetValueMetadata(MetadataKind Kind,
    ConstTargetMetadataPointer<Runtime, TargetNominalTypeDescriptor>
                      description,
    ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> parent)
    : TargetMetadata<Runtime>(Kind),
      Description(description),
      Parent(parent)
  {}

  /// An out-of-line description of the type.
  ConstTargetFarRelativeDirectPointer<Runtime, TargetNominalTypeDescriptor>
  Description;

  /// The parent type of this member type, or null if this is not a
  /// member type.  It's acceptable to make this a direct pointer because
  /// parent types are relatively uncommon.
  TargetPointer<Runtime, const TargetMetadata<Runtime>> Parent;

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Struct
      || metadata->getKind() == MetadataKind::Enum
      || metadata->getKind() == MetadataKind::Optional;
  }
  
  /// Retrieve the generic arguments of this type.
  ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> const *
  getGenericArgs() const {
    if (!Description->GenericParams.hasGenericRequirements())
      return nullptr;

    auto asWords = reinterpret_cast<
      ConstTargetMetadataPointer<Runtime, TargetMetadata> const *>(this);
    return (asWords + Description->GenericParams.Offset);
  }

  const TargetNominalTypeDescriptor<Runtime> *getDescription() const {
    return Description.get();
  }

  StoredPointer offsetToDescriptorOffset() const {
    return offsetof(TargetValueMetadata<Runtime>, Description);
  }

  StoredPointer offsetToParentOffset() const {
    return offsetof(TargetValueMetadata<Runtime>, Parent);
  }
  
};
using ValueMetadata = TargetValueMetadata<InProcess>;

/// The structure of type metadata for structs.
template <typename Runtime>
struct TargetStructMetadata : public TargetValueMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  using TargetValueMetadata<Runtime>::TargetValueMetadata;
  
  /// Get a pointer to the field offset vector, if present, or null.
  const StoredPointer *getFieldOffsets() const {
    auto offset = this->Description->Struct.FieldOffsetVectorOffset;
    if (offset == 0)
      return nullptr;
    auto asWords = reinterpret_cast<const void * const*>(this);
    return reinterpret_cast<const StoredPointer *>(asWords + offset);
  }
  
  /// Get a pointer to the field type vector, if present, or null.
  const FieldType *getFieldTypes() const {
    auto *getter = this->Description->Struct.GetFieldTypes.get();
    if (!getter)
      return nullptr;
    
    return getter(this);
  }

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Struct;
  }
};
using StructMetadata = TargetStructMetadata<InProcess>;

/// The structure of type metadata for enums.
template <typename Runtime>
struct TargetEnumMetadata : public TargetValueMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;
  using TargetValueMetadata<Runtime>::TargetValueMetadata;

  /// True if the metadata records the size of the payload area.
  bool hasPayloadSize() const {
    return this->Description->Enum.hasPayloadSizeOffset();
  }

  /// Retrieve the size of the payload area.
  ///
  /// `hasPayloadSize` must be true for this to be valid.
  StoredSize getPayloadSize() const {
    assert(hasPayloadSize());
    auto offset = this->Description->Enum.getPayloadSizeOffset();
    const StoredSize *asWords = reinterpret_cast<const StoredSize *>(this);
    asWords += offset;
    return *asWords;
  }

  StoredSize &getPayloadSize() {
    assert(hasPayloadSize());
    auto offset = this->Description->Enum.getPayloadSizeOffset();
    StoredSize *asWords = reinterpret_cast<StoredSize *>(this);
    asWords += offset;
    return *asWords;
  }

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Enum
      || metadata->getKind() == MetadataKind::Optional;
  }
};
using EnumMetadata = TargetEnumMetadata<InProcess>;

/// The structure of function type metadata.
template <typename Runtime>
struct TargetFunctionTypeMetadata : public TargetMetadata<Runtime> {
  using StoredSize = typename Runtime::StoredSize;

  // TODO: Make this target agnostic
  using Argument = FlaggedPointer<const TargetMetadata<Runtime> *, 0>;

  TargetFunctionTypeFlags<StoredSize> Flags;

  /// The type metadata for the result type.
  ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> ResultType;

  TargetPointer<Runtime, Argument> getArguments() {
    return reinterpret_cast<TargetPointer<Runtime, Argument>>(this + 1);
  }

  TargetPointer<Runtime, const Argument> getArguments() const {
    return reinterpret_cast<TargetPointer<Runtime, const Argument>>(this + 1);
  }
  
  StoredSize getNumArguments() const {
    return Flags.getNumArguments();
  }
  FunctionMetadataConvention getConvention() const {
    return Flags.getConvention();
  }
  bool throws() const { return Flags.throws(); }

  static constexpr StoredSize OffsetToFlags = sizeof(TargetMetadata<Runtime>);

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Function;
  }
};
using FunctionTypeMetadata = TargetFunctionTypeMetadata<InProcess>;

/// The structure of metadata for metatypes.
template <typename Runtime>
struct TargetMetatypeMetadata : public TargetMetadata<Runtime> {
  /// The type metadata for the element.
  ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> InstanceType;

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Metatype;
  }
};
using MetatypeMetadata = TargetMetatypeMetadata<InProcess>;

/// The structure of tuple type metadata.
template <typename Runtime>
struct TargetTupleTypeMetadata : public TargetMetadata<Runtime> {
  using StoredSize = typename Runtime::StoredSize;
  TargetTupleTypeMetadata() = default;
  constexpr TargetTupleTypeMetadata(const TargetMetadata<Runtime> &base,
                                    StoredSize numElements,
                                    TargetPointer<Runtime, const char> labels)
    : TargetMetadata<Runtime>(base),
      NumElements(numElements),
      Labels(labels) {}

  /// The number of elements.
  StoredSize NumElements;

  /// The labels string;  see swift_getTupleTypeMetadata.
  TargetPointer<Runtime, const char> Labels;

  struct Element {
    /// The type of the element.
    ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> Type;

    /// The offset of the tuple element within the tuple.
    StoredSize Offset;

    OpaqueValue *findIn(OpaqueValue *tuple) const {
      return (OpaqueValue*) (((char*) tuple) + Offset);
    }
  };

  Element *getElements() {
    return reinterpret_cast<Element*>(this + 1);
  }

  const Element *getElements() const {
    return reinterpret_cast<const Element*>(this + 1);
  }

  const Element &getElement(unsigned i) const {
    return getElements()[i];
  }

  Element &getElement(unsigned i) {
    return getElements()[i];
  }

  static constexpr StoredSize OffsetToNumElements = sizeof(TargetMetadata<Runtime>);

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Tuple;
  }
};
using TupleTypeMetadata = TargetTupleTypeMetadata<InProcess>;
  
/// The standard metadata for the empty tuple type.
SWIFT_RUNTIME_EXPORT
const
  FullMetadata<TupleTypeMetadata> METADATA_SYM(EMPTY_TUPLE_MANGLING);

template <typename Runtime> struct TargetProtocolDescriptor;
  
/// An array of protocol descriptors with a header and tail-allocated elements.
template <typename Runtime>
struct TargetProtocolDescriptorList {
  using StoredPointer = typename Runtime::StoredPointer;
  StoredPointer NumProtocols;

  ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor> *
  getProtocols() {
    return reinterpret_cast<
      ConstTargetMetadataPointer<
        Runtime, TargetProtocolDescriptor> *>(this + 1);
  }
  
  ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor> const *
  getProtocols() const {
    return reinterpret_cast<
      ConstTargetMetadataPointer<
        Runtime, TargetProtocolDescriptor> const *>(this + 1);
  }
  
  ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor> const &
  operator[](size_t i) const {
    return getProtocols()[i];
  }
  
  ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor> &
  operator[](size_t i) {
    return getProtocols()[i];
  }

  constexpr TargetProtocolDescriptorList() : NumProtocols(0) {}
  
protected:
  constexpr TargetProtocolDescriptorList(StoredPointer NumProtocols)
    : NumProtocols(NumProtocols) {}
};
using ProtocolDescriptorList = TargetProtocolDescriptorList<InProcess>;
  
/// A literal class for creating constant protocol descriptors in the runtime.
template<typename Runtime, uintptr_t NUM_PROTOCOLS>
struct TargetLiteralProtocolDescriptorList
  : TargetProtocolDescriptorList<Runtime> {
  const TargetProtocolDescriptorList<Runtime> *Protocols[NUM_PROTOCOLS];
  
  template<typename...DescriptorPointers>
  constexpr TargetLiteralProtocolDescriptorList(DescriptorPointers...elements)
    : TargetProtocolDescriptorList<Runtime>(NUM_PROTOCOLS),
      Protocols{elements...}
  {}
};
using LiteralProtocolDescriptorList = TargetProtocolDescriptorList<InProcess>;
  
/// A protocol descriptor. This is not type metadata, but is referenced by
/// existential type metadata records to describe a protocol constraint.
/// Its layout is compatible with the Objective-C runtime's 'protocol_t' record
/// layout.
template <typename Runtime>
struct TargetProtocolDescriptor {
  using StoredPointer = typename Runtime::StoredPointer;
  /// Unused by the Swift runtime.
  TargetPointer<Runtime, const void> _ObjC_Isa;
  
  /// The mangled name of the protocol.
  TargetPointer<Runtime, const char> Name;
  
  /// The list of protocols this protocol refines.
  ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptorList>
  InheritedProtocols;
  
  /// Unused by the Swift runtime.
  TargetPointer<Runtime, const void>
    _ObjC_InstanceMethods,
    _ObjC_ClassMethods,
    _ObjC_OptionalInstanceMethods,
    _ObjC_OptionalClassMethods,
    _ObjC_InstanceProperties;
  
  /// Size of the descriptor record.
  uint32_t DescriptorSize;
  
  /// Additional flags.
  ProtocolDescriptorFlags Flags;

  /// The minimum size of any conforming witness table, in words.
  ///
  /// When a conformance is ultimately instantiated from a GenericWitnessTable,
  /// this value must be greater than or equal to the GenericWitnessTable's
  /// WitnessTableSizeInWords.
  ///
  /// Only meaningful if ProtocolDescriptorFlags::IsResilient is set.
  uint16_t MinimumWitnessTableSizeInWords;

  /// The maximum amount to copy from the default requirements in words.
  /// If any requirements beyond MinimumWitnessTableSizeInWords are present
  /// in the witness table template, they will be not be overwritten with
  /// defaults.
  ///
  /// Only meaningful if ProtocolDescriptorFlags::IsResilient is set.
  uint16_t DefaultWitnessTableSizeInWords;

  /// Reserved. Really just here to zero-pad the structure on 64-bit.
  uint32_t Reserved;

  /// Default requirements are tail-allocated here.
  void **getDefaultWitnesses() const {
    return (void **) (this + 1);
  }

  constexpr TargetProtocolDescriptor<Runtime>(const char *Name,
                        const TargetProtocolDescriptorList<Runtime> *Inherited,
                        ProtocolDescriptorFlags Flags)
    : _ObjC_Isa(nullptr), Name(Name), InheritedProtocols(Inherited),
      _ObjC_InstanceMethods(nullptr), _ObjC_ClassMethods(nullptr),
      _ObjC_OptionalInstanceMethods(nullptr),
      _ObjC_OptionalClassMethods(nullptr),
      _ObjC_InstanceProperties(nullptr),
      DescriptorSize(sizeof(TargetProtocolDescriptor<Runtime>)),
      Flags(Flags),
      MinimumWitnessTableSizeInWords(0),
      DefaultWitnessTableSizeInWords(0)
  {}
};
using ProtocolDescriptor = TargetProtocolDescriptor<InProcess>;
  
/// A witness table for a protocol. This type is intentionally opaque because
/// the layout of a witness table is dependent on the protocol being
/// represented.
struct WitnessTable;

/// The basic layout of an opaque (non-class-bounded) existential type.
template <typename Runtime>
struct TargetOpaqueExistentialContainer {
  ValueBuffer Buffer;
  const TargetMetadata<Runtime> *Type;
  // const void *WitnessTables[];

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable **>(this + 1);
  }

  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable * const *>(this + 1);
  }

  void copyTypeInto(swift::TargetOpaqueExistentialContainer<Runtime> *dest,
                    unsigned numTables) const {
    dest->Type = Type;
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};
using OpaqueExistentialContainer
  = TargetOpaqueExistentialContainer<InProcess>;

/// The basic layout of a class-bounded existential type.
template <typename ContainedValue>
struct ClassExistentialContainerImpl {
  ContainedValue Value;

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable**>(this + 1);
  }
  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable* const *>(this + 1);
  }

  void copyTypeInto(ClassExistentialContainerImpl *dest,
                    unsigned numTables) const {
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};
using ClassExistentialContainer = ClassExistentialContainerImpl<void *>;
using WeakClassExistentialContainer =
  ClassExistentialContainerImpl<WeakReference>;

/// The possible physical representations of existential types.
enum class ExistentialTypeRepresentation {
  /// The type uses an opaque existential representation.
  Opaque,
  /// The type uses a class existential representation.
  Class,
  /// The type uses the Error boxed existential representation.
  Error,
};

/// The structure of existential type metadata.
template <typename Runtime>
struct TargetExistentialTypeMetadata : public TargetMetadata<Runtime> {
  using StoredPointer = typename Runtime::StoredPointer;
  /// The number of witness tables and class-constrained-ness of the type.
  ExistentialTypeFlags Flags;
  /// The protocol constraints.
  TargetProtocolDescriptorList<Runtime> Protocols;
  
  /// NB: Protocols has a tail-emplaced array; additional fields cannot follow.
  
  constexpr TargetExistentialTypeMetadata()
    : TargetMetadata<Runtime>(MetadataKind::Existential),
      Flags(ExistentialTypeFlags()), Protocols() {}
  
  /// Get the representation form this existential type uses.
  ExistentialTypeRepresentation getRepresentation() const;
  
  /// True if it's valid to take ownership of the value in the existential
  /// container if we own the container.
  bool mayTakeValue(const OpaqueValue *container) const;
  
  /// Clean up an existential container whose value is uninitialized.
  void deinitExistentialContainer(OpaqueValue *container) const;
  
  /// Project the value pointer from an existential container of the type
  /// described by this metadata.
  const OpaqueValue *projectValue(const OpaqueValue *container) const;
  
  OpaqueValue *projectValue(OpaqueValue *container) const {
    return const_cast<OpaqueValue *>(projectValue((const OpaqueValue*)container));
  }
  /// Get the dynamic type from an existential container of the type described
  /// by this metadata.
  const TargetMetadata<Runtime> *
  getDynamicType(const OpaqueValue *container) const;
  
  /// Get a witness table from an existential container of the type described
  /// by this metadata.
  const WitnessTable * getWitnessTable(const OpaqueValue *container,
                                       unsigned i) const;

  /// Return true iff all the protocol constraints are @objc.
  bool isObjC() const {
    return isClassBounded() && Flags.getNumWitnessTables() == 0;
  }

  bool isClassBounded() const {
    return Flags.getClassConstraint() == ProtocolClassConstraint::Class;
  }

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::Existential;
  }

  static constexpr StoredPointer
  OffsetToNumProtocols = sizeof(TargetMetadata<Runtime>) + sizeof(ExistentialTypeFlags);

};
using ExistentialTypeMetadata
  = TargetExistentialTypeMetadata<InProcess>;

/// The basic layout of an existential metatype type.
template <typename Runtime>
struct TargetExistentialMetatypeContainer {
  const TargetMetadata<Runtime> *Value;

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable**>(this + 1);
  }
  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable* const *>(this + 1);
  }

  void copyTypeInto(TargetExistentialMetatypeContainer *dest,
                    unsigned numTables) const {
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};
using ExistentialMetatypeContainer
  = TargetExistentialMetatypeContainer<InProcess>;

/// The structure of metadata for existential metatypes.
template <typename Runtime>
struct TargetExistentialMetatypeMetadata
  : public TargetMetadata<Runtime> {
  /// The type metadata for the element.
  ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> InstanceType;

  /// The number of witness tables and class-constrained-ness of the
  /// underlying type.
  ExistentialTypeFlags Flags;

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::ExistentialMetatype;
  }

  /// Return true iff all the protocol constraints are @objc.
  bool isObjC() const {
    return isClassBounded() && Flags.getNumWitnessTables() == 0;
  }

  bool isClassBounded() const {
    return Flags.getClassConstraint() == ProtocolClassConstraint::Class;
  }
};
using ExistentialMetatypeMetadata
  = TargetExistentialMetatypeMetadata<InProcess>;

/// \brief The header in front of a generic metadata template.
///
/// This is optimized so that the code generation pattern
/// requires the minimal number of independent arguments.
/// For example, we want to be able to allocate a generic class
/// Dictionary<T,U> like so:
///   extern GenericMetadata Dictionary_metadata_header;
///   void *arguments[] = { typeid(T), typeid(U) };
///   void *metadata = swift_getGenericMetadata(&Dictionary_metadata_header,
///                                             &arguments);
///   void *object = swift_allocObject(metadata);
///
/// Note that the metadata header is *not* const data; it includes 8
/// pointers worth of implementation-private data.
///
/// Both the metadata header and the arguments buffer are guaranteed
/// to be pointer-aligned.
template <typename Runtime>
struct TargetGenericMetadata {
  /// The fill function. Receives a pointer to the instantiated metadata and
  /// the argument pointer passed to swift_getGenericMetadata.
  TargetMetadata<Runtime> *(*CreateFunction)
  (TargetGenericMetadata<Runtime> *pattern, const void *arguments);
  
  /// The size of the template in bytes.
  uint32_t MetadataSize;

  /// The number of generic arguments that we need to unique on,
  /// in words.  The first 'NumArguments * sizeof(void*)' bytes of
  /// the arguments buffer are the key. There may be additional private-contract
  /// data used by FillFunction not used for uniquing.
  uint16_t NumKeyArguments;

  /// The offset of the address point in the template in bytes.
  uint16_t AddressPoint;

  /// Data that the runtime can use for its own purposes.  It is guaranteed
  /// to be zero-filled by the compiler.
  TargetPointer<Runtime, void>
  PrivateData[swift::NumGenericMetadataPrivateDataWords];

  // Here there is a variably-sized field:
  // char alignas(void*) MetadataTemplate[MetadataSize];

  /// Return the starting address of the metadata template data.
  TargetPointer<Runtime, const void> getMetadataTemplate() const {
    return reinterpret_cast<TargetPointer<Runtime, const void>>(this + 1);
  }

  /// Return the nominal type descriptor for the template metadata
  ConstTargetMetadataPointer<Runtime, TargetNominalTypeDescriptor>
  getTemplateDescription() const {
    auto bytes = reinterpret_cast<const uint8_t *>(getMetadataTemplate());
    auto metadata = reinterpret_cast<
      const TargetMetadata<Runtime> *>(bytes + AddressPoint);
    return metadata->getNominalTypeDescriptor();
  }
};
using GenericMetadata = TargetGenericMetadata<InProcess>;

/// Heap metadata for a box, which may have been generated statically by the
/// compiler or by the runtime.
template <typename Runtime>
struct TargetBoxHeapMetadata : public TargetHeapMetadata<Runtime> {
  /// The offset from the beginning of a box to its value.
  unsigned Offset;

  constexpr TargetBoxHeapMetadata(MetadataKind kind, unsigned offset)
  : TargetHeapMetadata<Runtime>(kind), Offset(offset) {}
};
using BoxHeapMetadata = TargetBoxHeapMetadata<InProcess>;

/// Heap metadata for runtime-instantiated generic boxes.
template <typename Runtime>
struct TargetGenericBoxHeapMetadata : public TargetBoxHeapMetadata<Runtime> {
  using super = TargetBoxHeapMetadata<Runtime>;
  using super::Offset;

  /// The type inside the box.
  ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> BoxedType;

  constexpr
  TargetGenericBoxHeapMetadata(MetadataKind kind, unsigned offset,
    ConstTargetMetadataPointer<Runtime, swift::TargetMetadata> boxedType)
  : TargetBoxHeapMetadata<Runtime>(kind, offset), BoxedType(boxedType)
  {}

  static unsigned getHeaderOffset(const Metadata *boxedType) {
    // Round up the header size to alignment.
    unsigned alignMask = boxedType->getValueWitnesses()->getAlignmentMask();
    return (sizeof(HeapObject) + alignMask) & ~alignMask;
  }

  /// Project the value out of a box of this type.
  OpaqueValue *project(HeapObject *box) const {
    auto bytes = reinterpret_cast<char*>(box);
    return reinterpret_cast<OpaqueValue *>(bytes + Offset);
  }

  /// Get the allocation size of this box.
  unsigned getAllocSize() const {
    return Offset + BoxedType->getValueWitnesses()->getSize();
  }

  /// Get the allocation alignment of this box.
  unsigned getAllocAlignMask() const {
    // Heap allocations are at least pointer aligned.
    return BoxedType->getValueWitnesses()->getAlignmentMask()
      | (alignof(void*) - 1);
  }

  static bool classof(const TargetMetadata<Runtime> *metadata) {
    return metadata->getKind() == MetadataKind::HeapGenericLocalVariable;
  }
};
using GenericBoxHeapMetadata = TargetGenericBoxHeapMetadata<InProcess>;

/// \brief The control structure of a generic or resilient protocol
/// conformance.
///
/// Witness tables need to be instantiated at runtime in these cases:
/// - For a generic conforming type, associated type requirements might be
///   dependent on the conforming type.
/// - For a type conforming to a resilient protocol, the runtime size of
///   the witness table is not known because default requirements can be
///   added resiliently.
///
/// One per conformance.
template <typename Runtime>
struct TargetGenericWitnessTable {
  /// The size of the witness table in words.  This amount is copied from
  /// the witness table template into the instantiated witness table.
  uint16_t WitnessTableSizeInWords;

  /// The amount of private storage to allocate before the address point,
  /// in words. This memory is zeroed out in the instantiated witness table
  /// template.
  uint16_t WitnessTablePrivateSizeInWords;

  /// The protocol descriptor. Only used for resilient conformances.
  RelativeIndirectablePointer<ProtocolDescriptor,
                              /*nullable*/ true> Protocol;

  /// The pattern.
  RelativeDirectPointer<const WitnessTable> Pattern;

  /// The instantiation function, which is called after the template is copied.
  RelativeDirectPointer<void(WitnessTable *instantiatedTable,
                             const TargetMetadata<Runtime> *type,
                             void * const *instantiationArgs),
                        /*nullable*/ true> Instantiator;

  void *PrivateData[swift::NumGenericMetadataPrivateDataWords];
};
using GenericWitnessTable = TargetGenericWitnessTable<InProcess>;

/// The structure of a type metadata record.
///
/// This contains enough static information to recover type metadata from a
/// name. It is only emitted for types that do not have an explicit protocol
/// conformance record. 
///
/// This structure is notionally a subtype of a protocol conformance record
/// but as we cannot change the conformance record layout we have to make do
/// with some duplicated code.
template <typename Runtime>
struct TargetTypeMetadataRecord {
private:
  // Some description of the type that is resolvable at runtime.
  union {
    /// A direct reference to the metadata.
    RelativeDirectPointer<const TargetMetadata<Runtime>> DirectType;

    /// The nominal type descriptor for a resilient or generic type.
    RelativeDirectPointer<TargetNominalTypeDescriptor<Runtime>>
    TypeDescriptor;
  };

  /// Flags describing the type metadata record.
  TypeMetadataRecordFlags Flags;
  
public:
  TypeMetadataRecordKind getTypeKind() const {
    return Flags.getTypeKind();
  }
  
  const TargetMetadata<Runtime> *getDirectType() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;

    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
    case TypeMetadataRecordKind::UniqueDirectClass:
      break;
        
    case TypeMetadataRecordKind::UniqueIndirectClass:
    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      assert(false && "not direct type metadata");
    }

    return this->DirectType;
  }

  const TargetNominalTypeDescriptor<Runtime> *
  getNominalTypeDescriptor() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;

    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      break;
        
    case TypeMetadataRecordKind::UniqueDirectClass:
    case TypeMetadataRecordKind::UniqueIndirectClass:
    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
      assert(false && "not generic metadata pattern");
    }
    
    return this->TypeDescriptor;
  }

  /// Get the canonical metadata for the type referenced by this record, or
  /// return null if the record references a generic or universal type.
  const TargetMetadata<Runtime> *getCanonicalTypeMetadata() const;
};
using TypeMetadataRecord = TargetTypeMetadataRecord<InProcess>;

/// The structure of a protocol conformance record.
///
/// This contains enough static information to recover the witness table for a
/// type's conformance to a protocol.
template <typename Runtime>
struct TargetProtocolConformanceRecord {
public:
  using WitnessTableAccessorFn
    = const WitnessTable *(const TargetMetadata<Runtime>*);

private:
  /// The protocol being conformed to.
  RelativeIndirectablePointer<ProtocolDescriptor> Protocol;
  
  // Some description of the type that conforms to the protocol.
  union {
    /// A direct reference to the metadata.
    ///
    /// Depending on the conformance kind, this may not be usable
    /// metadata without being first processed by the runtime.
    RelativeIndirectablePointer<TargetMetadata<Runtime>> DirectType;
    
    /// An indirect reference to the metadata.
    RelativeIndirectablePointer<const TargetClassMetadata<Runtime> *>
    IndirectClass;
    
    /// The nominal type descriptor for a resilient or generic type which has
    /// instances that conform to the protocol.
    RelativeIndirectablePointer<TargetNominalTypeDescriptor<Runtime>>
    TypeDescriptor;
  };
  
  
  // The conformance, or a generator function for the conformance.
  union {
    /// A direct reference to the witness table for the conformance.
    RelativeDirectPointer<const WitnessTable> WitnessTable;
    
    /// A function that produces the witness table given an instance of the
    /// type. The function may return null if a specific instance does not
    /// conform to the protocol.
    RelativeDirectPointer<WitnessTableAccessorFn> WitnessTableAccessor;
  };
  
  /// Flags describing the protocol conformance.
  ProtocolConformanceFlags Flags;
  
public:
  const ProtocolDescriptor *getProtocol() const {
    return Protocol;
  }
  
  ProtocolConformanceFlags getFlags() const {
    return Flags;
  }
  
  TypeMetadataRecordKind getTypeKind() const {
    return Flags.getTypeKind();
  }
  ProtocolConformanceReferenceKind getConformanceKind() const {
    return Flags.getConformanceKind();
  }
  
  const TargetMetadata<Runtime> *getDirectType() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;

    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
      break;
        
    case TypeMetadataRecordKind::UniqueDirectClass:
    case TypeMetadataRecordKind::UniqueIndirectClass:
    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      assert(false && "not direct type metadata");
    }

    return DirectType;
  }
  
  // FIXME: This shouldn't exist
  const TargetClassMetadata<Runtime> *getDirectClass() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;
    case TypeMetadataRecordKind::UniqueDirectClass:
      break;
        
    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
    case TypeMetadataRecordKind::UniqueIndirectClass:
      assert(false && "not direct class object");
    }

    const TargetMetadata<Runtime> *metadata = DirectType;
    return static_cast<const TargetClassMetadata<Runtime>*>(metadata);
    
  }
  
  const TargetClassMetadata<Runtime> * const *getIndirectClass() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;

    case TypeMetadataRecordKind::UniqueIndirectClass:
      break;
        
    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::UniqueDirectClass:
    case TypeMetadataRecordKind::NonuniqueDirectType:
    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      assert(false && "not indirect class object");
    }
    
    return IndirectClass;
  }
  
  const TargetNominalTypeDescriptor<Runtime> *
  getNominalTypeDescriptor() const {
    switch (Flags.getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      return nullptr;

    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      break;
        
    case TypeMetadataRecordKind::UniqueDirectClass:
    case TypeMetadataRecordKind::UniqueIndirectClass:
    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
      assert(false && "not generic metadata pattern");
    }
    
    return TypeDescriptor;
  }
  
  /// Get the directly-referenced static witness table.
  const swift::WitnessTable *getStaticWitnessTable() const {
    switch (Flags.getConformanceKind()) {
    case ProtocolConformanceReferenceKind::WitnessTable:
      break;
        
    case ProtocolConformanceReferenceKind::WitnessTableAccessor:
      assert(false && "not witness table");
    }
    return WitnessTable;
  }
  
  WitnessTableAccessorFn *getWitnessTableAccessor() const {
    switch (Flags.getConformanceKind()) {
    case ProtocolConformanceReferenceKind::WitnessTableAccessor:
      break;
        
    case ProtocolConformanceReferenceKind::WitnessTable:
      assert(false && "not witness table accessor");
    }
    return WitnessTableAccessor;
  }
  
  /// Get the canonical metadata for the type referenced by this record, or
  /// return null if the record references a generic or universal type.
  const TargetMetadata<Runtime> *getCanonicalTypeMetadata() const;
  
  /// Get the witness table for the specified type, realizing it if
  /// necessary, or return null if the conformance does not apply to the
  /// type.
  const swift::WitnessTable *
  getWitnessTable(const TargetMetadata<Runtime> *type) const;
  
#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
  void dump() const;
#endif
};
using ProtocolConformanceRecord
  = TargetProtocolConformanceRecord<InProcess>;

/// \brief Fetch a uniqued metadata object for a generic nominal type.
///
/// The basic algorithm for fetching a metadata object is:
///   func swift_getGenericMetadata(header, arguments) {
///     if (metadata = getExistingMetadata(&header.PrivateData,
///                                        arguments[0..header.NumArguments]))
///       return metadata
///     metadata = malloc(header.MetadataSize)
///     memcpy(metadata, header.MetadataTemplate, header.MetadataSize)
///     for (i in 0..header.NumFillInstructions)
///       metadata[header.FillInstructions[i].ToIndex]
///         = arguments[header.FillInstructions[i].FromIndex]
///     setExistingMetadata(&header.PrivateData,
///                         arguments[0..header.NumArguments],
///                         metadata)
///     return metadata
///   }
SWIFT_RT_ENTRY_VISIBILITY
const Metadata *
swift_getGenericMetadata(GenericMetadata *pattern,
                         const void *arguments)
    SWIFT_CC(RegisterPreservingCC);

// Callback to allocate a generic class metadata object.
SWIFT_RUNTIME_EXPORT
ClassMetadata *
swift_allocateGenericClassMetadata(GenericMetadata *pattern,
                                   const void *arguments,
                                   ClassMetadata *superclass);

// Callback to allocate a generic struct/enum metadata object.
SWIFT_RUNTIME_EXPORT
ValueMetadata *
swift_allocateGenericValueMetadata(GenericMetadata *pattern,
                                   const void *arguments);

/// Instantiate a resilient or generic protocol witness table.
///
/// \param genericTable - The witness table template for the
///   conformance. It may either have fields that require runtime
///   initialization, or be missing requirements at the end for
///   which default witnesses are available.
///
/// \param type - The conforming type, used to form a uniquing key
///   for the conformance. If the witness table is not dependent on
///   the substituted type of the conformance, this can be set to
///   nullptr, in which case there will only be one instantiated
///   witness table per witness table template.
///
/// \param instantiationArgs - An opaque pointer that's forwarded to
///   the instantiation function, used for conditional conformances.
///   This API implicitly embeds an assumption that these arguments
///   never form part of the uniquing key for the conformance, which
///   is ultimately a statement about the user model of overlapping
///   conformances.
SWIFT_RT_ENTRY_VISIBILITY
const WitnessTable *
swift_getGenericWitnessTable(GenericWitnessTable *genericTable,
                             const Metadata *type,
                             void * const *instantiationArgs)
    SWIFT_CC(RegisterPreservingCC);

/// \brief Fetch a uniqued metadata for a function type.
SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata(const void *flagsArgsAndResult[]);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata1(FunctionTypeFlags flags,
                               const void *arg0,
                               const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata2(FunctionTypeFlags flags,
                               const void *arg0,
                               const void *arg1,
                               const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getFunctionTypeMetadata3(FunctionTypeFlags flags,
                               const void *arg0,
                               const void *arg1,
                               const void *arg2,
                               const Metadata *resultMetadata);

/// \brief Fetch a uniqued metadata for a thin function type.
SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata(size_t numArguments,
                                  const void * argsAndResult []);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata0(const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata1(const void *arg0,
                                   const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata2(const void *arg0,
                                   const void *arg1,
                                   const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata3(const void *arg0,
                                   const void *arg1,
                                   const void *arg2,
                                   const Metadata *resultMetadata);

/// \brief Fetch a uniqued metadata for a C function type.
SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getCFunctionTypeMetadata(size_t numArguments,
                               const void * argsAndResult []);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getCFunctionTypeMetadata0(const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getCFunctionTypeMetadata1(const void *arg0,
                                const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getCFunctionTypeMetadata2(const void *arg0,
                                const void *arg1,
                                const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getCFunctionTypeMetadata3(const void *arg0,
                                const void *arg1,
                                const void *arg2,
                                const Metadata *resultMetadata);

#if SWIFT_OBJC_INTEROP
/// \brief Fetch a uniqued metadata for a block type.
SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getBlockTypeMetadata(size_t numArguments,
                           const void *argsAndResult []);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getBlockTypeMetadata0(const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getBlockTypeMetadata1(const void *arg0,
                            const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getBlockTypeMetadata2(const void *arg0,
                            const void *arg1,
                            const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
const FunctionTypeMetadata *
swift_getBlockTypeMetadata3(const void *arg0,
                            const void *arg1,
                            const void *arg2,
                            const Metadata *resultMetadata);

SWIFT_RUNTIME_EXPORT
void
swift_instantiateObjCClass(const ClassMetadata *theClass);
#endif

/// \brief Fetch a uniqued type metadata for an ObjC class.
SWIFT_RUNTIME_EXPORT
const Metadata *
swift_getObjCClassMetadata(const ClassMetadata *theClass);

/// \brief Fetch a unique type metadata object for a foreign type.
SWIFT_RUNTIME_EXPORT
const ForeignTypeMetadata *
swift_getForeignTypeMetadata(ForeignTypeMetadata *nonUnique);

/// \brief Fetch a uniqued metadata for a tuple type.
///
/// The labels argument is null if and only if there are no element
/// labels in the tuple.  Otherwise, it is a null-terminated
/// concatenation of space-terminated NFC-normalized UTF-8 strings,
/// assumed to point to constant global memory.
///
/// That is, for the tuple type (a : Int, Int, c : Int), this
/// argument should be:
///   "a  c \0"
///
/// This representation allows label strings to be efficiently
/// (1) uniqued within a linkage unit and (2) compared with strcmp.
/// In other words, it's optimized for code size and uniquing
/// efficiency, not for the convenience of actually consuming
/// these strings.
///
/// \param elements - potentially invalid if numElements is zero;
///   otherwise, an array of metadata pointers.
/// \param labels - the labels string
/// \param proposedWitnesses - an optional proposed set of value witnesses.
///   This is useful when working with a non-dependent tuple type
///   where the entrypoint is just being used to unique the metadata.
SWIFT_RUNTIME_EXPORT
const TupleTypeMetadata *
swift_getTupleTypeMetadata(size_t numElements,
                           const Metadata * const *elements,
                           const char *labels,
                           const ValueWitnessTable *proposedWitnesses);

SWIFT_RUNTIME_EXPORT
const TupleTypeMetadata *
swift_getTupleTypeMetadata2(const Metadata *elt0, const Metadata *elt1,
                            const char *labels,
                            const ValueWitnessTable *proposedWitnesses);
SWIFT_RUNTIME_EXPORT
const TupleTypeMetadata *
swift_getTupleTypeMetadata3(const Metadata *elt0, const Metadata *elt1,
                            const Metadata *elt2, const char *labels,
                            const ValueWitnessTable *proposedWitnesses);

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
SWIFT_RUNTIME_EXPORT
void swift_initStructMetadata_UniversalStrategy(size_t numFields,
                                         const TypeLayout * const *fieldTypes,
                                         size_t *fieldOffsets,
                                         ValueWitnessTable *vwtable);

struct ClassFieldLayout {
  size_t Size;
  size_t AlignMask;
};

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
///
/// This will relocate the metadata if it doesn't have enough space
/// for its superclass.  Note that swift_allocateGenericClassMetadata will
/// never produce a metadata that requires relocation.
SWIFT_RUNTIME_EXPORT
ClassMetadata *
swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                          size_t numFields,
                                          const ClassFieldLayout *fieldLayouts,
                                          size_t *fieldOffsets);

/// \brief Fetch a uniqued metadata for a metatype type.
SWIFT_RUNTIME_EXPORT
const MetatypeMetadata *
swift_getMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential metatype type.
SWIFT_RUNTIME_EXPORT
const ExistentialMetatypeMetadata *
swift_getExistentialMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
SWIFT_RT_ENTRY_VISIBILITY
const ExistentialTypeMetadata *
swift_getExistentialTypeMetadata(size_t numProtocols,
                                 const ProtocolDescriptor **protocols)
    SWIFT_CC(RegisterPreservingCC);

/// \brief Perform a checked dynamic cast of a value to a target type.
///
/// \param dest A buffer into which to write the destination value.
/// In all cases, this will be left uninitialized if the cast fails.
///
/// \param src Pointer to the source value to cast.  This may be left
///   uninitialized after the operation, depending on the flags.
///
/// \param targetType The type to which we are casting.
///
/// \param srcType The static type of the source value.
///
/// \param flags Flags to control the operation.
///
/// \return true if the cast succeeded. Depending on the flags,
///   swift_dynamicCast may fail rather than return false.
SWIFT_RT_ENTRY_VISIBILITY
bool
swift_dynamicCast(OpaqueValue *dest, OpaqueValue *src,
                  const Metadata *srcType,
                  const Metadata *targetType,
                  DynamicCastFlags flags)
    SWIFT_CC(RegisterPreservingCC);

/// \brief Checked dynamic cast to a Swift class type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a Swift class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
SWIFT_RT_ENTRY_VISIBILITY
const void *
swift_dynamicCastClass(const void *object, const ClassMetadata *targetType)
    SWIFT_CC(RegisterPreservingCC);

/// \brief Unconditional, checked dynamic cast to a Swift class type.
///
/// Aborts if the object isn't of the target type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a Swift class type.
///
/// \returns the object.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastClassUnconditional(const void *object,
                                    const ClassMetadata *targetType);

#if SWIFT_OBJC_INTEROP
/// \brief Checked Objective-C-style dynamic cast to a class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a class type, but not necessarily valid type metadata.
///
/// \returns the object if the cast succeeds, or null otherwise.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastObjCClass(const void *object, const ClassMetadata *targetType);

/// \brief Checked dynamic cast to a foreign class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a foreign class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastForeignClass(const void *object,
                              const ForeignClassMetadata *targetType);

/// \brief Unconditional, checked, Objective-C-style dynamic cast to a class
/// type.
///
/// Aborts if the object isn't of the target type.
/// Note that unlike swift_dynamicCastClassUnconditional, this does not abort
/// if the object is 'nil'.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a class type, but not necessarily valid type metadata.
///
/// \returns the object.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastObjCClassUnconditional(const void *object,
                                        const ClassMetadata *targetType);

/// \brief Unconditional, checked dynamic cast to a foreign class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a foreign class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastForeignClassUnconditional(
  const void *object,
  const ForeignClassMetadata *targetType);
#endif

/// \brief Checked dynamic cast of a class instance pointer to the given type.
///
/// \param object The class instance to cast.
///
/// \param targetType The type to which we are casting, which may be either a
/// class type or a wrapped Objective-C class type.
///
/// \returns the object, or null if it doesn't have the given target type.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastUnknownClass(const void *object, const Metadata *targetType);

/// \brief Unconditional checked dynamic cast of a class instance pointer to
/// the given type.
///
/// Aborts if the object isn't of the target type.
///
/// \param object The class instance to cast.
///
/// \param targetType The type to which we are casting, which may be either a
/// class type or a wrapped Objective-C class type.
///
/// \returns the object.
SWIFT_RUNTIME_EXPORT
const void *
swift_dynamicCastUnknownClassUnconditional(const void *object,
                                           const Metadata *targetType);

SWIFT_RUNTIME_EXPORT
const Metadata *
swift_dynamicCastMetatype(const Metadata *sourceType,
                          const Metadata *targetType);
SWIFT_RUNTIME_EXPORT
const Metadata *
swift_dynamicCastMetatypeUnconditional(const Metadata *sourceType,
                                       const Metadata *targetType);
#if SWIFT_OBJC_INTEROP
SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_dynamicCastObjCClassMetatype(const ClassMetadata *sourceType,
                                   const ClassMetadata *targetType);
SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_dynamicCastObjCClassMetatypeUnconditional(const ClassMetadata *sourceType,
                                                const ClassMetadata *targetType);
#endif

SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_dynamicCastForeignClassMetatype(const ClassMetadata *sourceType,
                                   const ClassMetadata *targetType);
SWIFT_RUNTIME_EXPORT
const ClassMetadata *
swift_dynamicCastForeignClassMetatypeUnconditional(
  const ClassMetadata *sourceType,
  const ClassMetadata *targetType);

/// \brief Return the dynamic type of an opaque value.
///
/// \param value An opaque value.
/// \param self  The static type metadata for the opaque value and the result
///              type value.
/// \param existentialMetatype Whether the result type value is an existential
///                            metatype. If `self` is an existential type,
///                            then a `false` value indicates that the result
///                            is of concrete metatype type `self.Protocol`,
///                            and existential containers will not be projected
///                            through. A `true` value indicates that the result
///                            is of existential metatype type `self.Type`,
///                            so existential containers can be projected
///                            through as long as a subtype relationship holds
///                            from `self` to the contained dynamic type.
SWIFT_RUNTIME_EXPORT
const Metadata *
swift_getDynamicType(OpaqueValue *value, const Metadata *self,
                     bool existentialMetatype);

/// \brief Fetch the type metadata associated with the formal dynamic
/// type of the given (possibly Objective-C) object.  The formal
/// dynamic type ignores dynamic subclasses such as those introduced
/// by KVO.
///
/// The object pointer may be a tagged pointer, but cannot be null.
SWIFT_RUNTIME_EXPORT
const Metadata *swift_getObjectType(HeapObject *object);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with the
/// same number of witness tables.
SWIFT_RUNTIME_EXPORT
OpaqueValue *swift_assignExistentialWithCopy(OpaqueValue *dest,
                                             const OpaqueValue *src,
                                             const Metadata *type);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with no
/// witness tables.
OpaqueValue *swift_assignExistentialWithCopy0(OpaqueValue *dest,
                                              const OpaqueValue *src,
                                              const Metadata *type);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with one
/// witness table.
OpaqueValue *swift_assignExistentialWithCopy1(OpaqueValue *dest,
                                              const OpaqueValue *src,
                                              const Metadata *type);

/// Calculate the numeric index of an extra inhabitant of a heap object
/// pointer in memory.
inline int swift_getHeapObjectExtraInhabitantIndex(HeapObject * const* src) {
  // This must be consistent with the getHeapObjectExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  uintptr_t value = reinterpret_cast<uintptr_t>(*src);
  if (value >= LeastValidPointerValue)
    return -1;

  // Check for tagged pointers on appropriate platforms.  Knowing that
  // value < LeastValidPointerValue tells us a lot.
#if SWIFT_OBJC_INTEROP
  if (value & ((uintptr_t(1) << ObjCReservedLowBits) - 1))
    return -1;
#endif

  return (int) (value >> ObjCReservedLowBits);
}
  
/// Store an extra inhabitant of a heap object pointer to memory,
/// in the style of a value witness.
inline void swift_storeHeapObjectExtraInhabitant(HeapObject **dest, int index) {
  // This must be consistent with the storeHeapObjectExtraInhabitant
  // implementation in IRGen's ExtraInhabitants.cpp.

  auto value = uintptr_t(index) << heap_object_abi::ObjCReservedLowBits;
  *dest = reinterpret_cast<HeapObject*>(value);
}

/// Return the number of extra inhabitants in a heap object pointer.
inline constexpr unsigned swift_getHeapObjectExtraInhabitantCount() {
  // This must be consistent with the getHeapObjectExtraInhabitantCount
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  // The runtime needs no more than INT_MAX inhabitants.
  return (LeastValidPointerValue >> ObjCReservedLowBits) > INT_MAX
    ? (unsigned)INT_MAX
    : (unsigned)(LeastValidPointerValue >> ObjCReservedLowBits);
}  

/// Calculate the numeric index of an extra inhabitant of a function
/// pointer in memory.
inline int swift_getFunctionPointerExtraInhabitantIndex(void * const* src) {
  // This must be consistent with the getFunctionPointerExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.
  uintptr_t value = reinterpret_cast<uintptr_t>(*src);
  return (value < heap_object_abi::LeastValidPointerValue
            ? (int) value : -1);
}
  
/// Store an extra inhabitant of a function pointer to memory, in the
/// style of a value witness.
inline void swift_storeFunctionPointerExtraInhabitant(void **dest, int index) {
  // This must be consistent with the storeFunctionPointerExtraInhabitantIndex
  // implementation in IRGen's ExtraInhabitants.cpp.
  *dest = reinterpret_cast<void*>(static_cast<uintptr_t>(index));
}

/// Return the number of extra inhabitants in a function pointer.
inline constexpr unsigned swift_getFunctionPointerExtraInhabitantCount() {
  // This must be consistent with the getFunctionPointerExtraInhabitantCount
  // implementation in IRGen's ExtraInhabitants.cpp.

  using namespace heap_object_abi;

  // The runtime needs no more than INT_MAX inhabitants.
  return (LeastValidPointerValue) > INT_MAX
    ? (unsigned)INT_MAX
    : (unsigned)(LeastValidPointerValue);
}
  
/// \brief Check whether a type conforms to a given native Swift protocol,
/// visible from the named module.
///
/// If so, returns a pointer to the witness table for its conformance.
/// Returns void if the type does not conform to the protocol.
///
/// \param type The metadata for the type for which to do the conformance
///             check.
/// \param protocol The protocol descriptor for the protocol to check
///                 conformance for.
SWIFT_RUNTIME_EXPORT
const WitnessTable *swift_conformsToProtocol(const Metadata *type,
                                            const ProtocolDescriptor *protocol);

/// Register a block of protocol conformance records for dynamic lookup.
SWIFT_RUNTIME_EXPORT
void swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                        const ProtocolConformanceRecord *end);

/// Register a block of type metadata records dynamic lookup.
SWIFT_RUNTIME_EXPORT
void swift_registerTypeMetadataRecords(const TypeMetadataRecord *begin,
                                       const TypeMetadataRecord *end);

/// Return the type name for a given type metadata.
std::string nameForMetadata(const Metadata *type,
                            bool qualified = true);

/// Return the superclass, if any.  The result is nullptr for root
/// classes and class protocol types.
SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_INTERFACE
const Metadata *_swift_class_getSuperclass(const Metadata *theClass);

} // end namespace swift

#pragma clang diagnostic pop

#endif /* SWIFT_RUNTIME_METADATA_H */
