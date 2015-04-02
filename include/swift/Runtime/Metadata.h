//===--- Metadata.h - Swift Language ABI Metadata Support -------*- C++ -*-===//
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
#include "swift/Runtime/Config.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/System.h"
#include "swift/Basic/FlaggedPointer.h"

namespace swift {

struct HeapObject;
struct Metadata;

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
///    owns unintialized value storage for the stored type.
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
  constexpr ValueWitnessFlags
  withExtraInhabitants(bool hasExtraInhabitants) const {
    return ValueWitnessFlags((Data & ~HasExtraInhabitants) |
                               (hasExtraInhabitants ? HasExtraInhabitants : 0));
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
///   self->initalizeBufferWithCopy(dest, self->projectBuffer(src), self)
///
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
/// This operation must be safe aginst 'dest' and 'src' aliasing.
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
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
/// There is no need for a initializeBufferWithTakeOfBuffer, because that
/// can simply be a pointer-aligned memcpy of sizeof(ValueBuffer)
/// bytes.
///
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
/// Therefore this can be decomposed as:
///
///   self->initalizeBufferWithTake(dest, self->projectBuffer(src), self)
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
/// This operation does not need to be safe aginst 'dest' and 'src' aliasing.
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
  
/// Store an extra inhabitant, named by a unique positive or zero index,
/// into the given uninitialized storage for the type.
typedef void storeExtraInhabitant(OpaqueValue *dest,
                                  int index,
                                  const Metadata *self);
  
/// Get the extra inhabitant index for the bit pattern stored at the given
/// address, or return -1 if there is a valid value at the address.
typedef int getExtraInhabitantIndex(const OpaqueValue *src,
                                    const Metadata *self);
  
/// Flags which describe extra inhabitants.
typedef ExtraInhabitantFlags extraInhabitantFlags;

} // end namespace value_witness_types

/// A standard routine, suitable for placement in the value witness
/// table, for copying an opaque POD object.
extern "C" OpaqueValue *swift_copyPOD(OpaqueValue *dest,
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
};
  
/// A value-witness table with extra inhabitants entry points.
/// These entry points are available only if the HasExtraInhabitants flag bit is
/// set in the 'flags' field.
struct ExtraInhabitantsValueWitnessTable : ValueWitnessTable {
  value_witness_types::storeExtraInhabitant *storeExtraInhabitant;
  value_witness_types::getExtraInhabitantIndex *getExtraInhabitantIndex;
  value_witness_types::extraInhabitantFlags extraInhabitantFlags;
  
  constexpr ExtraInhabitantsValueWitnessTable()
    : ValueWitnessTable{}, storeExtraInhabitant(nullptr),
      getExtraInhabitantIndex(nullptr), extraInhabitantFlags() {}
  constexpr ExtraInhabitantsValueWitnessTable(const ValueWitnessTable &base,
                            value_witness_types::storeExtraInhabitant *sei,
                            value_witness_types::getExtraInhabitantIndex *geii,
                            value_witness_types::extraInhabitantFlags eif)
    : ValueWitnessTable(base), storeExtraInhabitant(sei),
      getExtraInhabitantIndex(geii), extraInhabitantFlags(eif) {}

  static bool classof(const ValueWitnessTable *table) {
    return table->flags.hasExtraInhabitants();
  }
};

inline const ExtraInhabitantsValueWitnessTable *
ValueWitnessTable::_asXIVWT() const {
  assert(ExtraInhabitantsValueWitnessTable::classof(this));
  return static_cast<const ExtraInhabitantsValueWitnessTable *>(this);
}
  
inline unsigned ValueWitnessTable::getNumExtraInhabitants() const {
  // If the table does not have extra inhabitant witnesses, then there are zero.
  if (!flags.hasExtraInhabitants())
    return 0;
  return this->_asXIVWT()->extraInhabitantFlags.getNumExtraInhabitants();
}

// Standard value-witness tables.

// The "Int" tables are used for arbitrary POD data with the matching
// size/alignment characteristics.
extern "C" const ValueWitnessTable _TWVBi8_;      // Builtin.Int8
extern "C" const ValueWitnessTable _TWVBi16_;     // Builtin.Int16
extern "C" const ValueWitnessTable _TWVBi32_;     // Builtin.Int32
extern "C" const ValueWitnessTable _TWVBi64_;     // Builtin.Int64
extern "C" const ValueWitnessTable _TWVBi128_;    // Builtin.Int128
  
// The object-pointer table can be used for arbitrary Swift refcounted
// pointer types.
extern "C" const ExtraInhabitantsValueWitnessTable _TWVBo; // Builtin.NativeObject

extern "C" const ExtraInhabitantsValueWitnessTable _TWVBb; // Builtin.BridgeObject

#if SWIFT_OBJC_INTEROP
// The ObjC-pointer table can be used for arbitrary ObjC pointer types.
extern "C" const ExtraInhabitantsValueWitnessTable _TWVBO; // Builtin.UnknownObject
#endif

// The () -> () table can be used for arbitrary function types.
extern "C" const ExtraInhabitantsValueWitnessTable _TWVFT_T_;     // () -> ()

// The @thin () -> () table can be used for arbitrary thin function types.
extern "C" const ExtraInhabitantsValueWitnessTable _TWVXfT_T_;     // @thin () -> ()

// The () table can be used for arbitrary empty types.
extern "C" const ValueWitnessTable _TWVT_;        // ()

// The table for aligned-pointer-to-pointer types.
extern "C" const ExtraInhabitantsValueWitnessTable _TWVMBo; // Builtin.NativeObject.Type

/// Return the value witnesses for unmanaged pointers.
static inline const ValueWitnessTable &getUnmanagedPointerValueWitnesses() {
#ifdef __LP64__
  return _TWVBi64_;
#else
  return _TWVBi32_;
#endif
}

/// Return value witnesses for a pointer-aligned pointer type.
static inline
const ExtraInhabitantsValueWitnessTable &
getUnmanagedPointerPointerValueWitnesses() {
  return _TWVMBo;
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
  
struct NominalTypeDescriptor;
struct GenericMetadata;
struct ClassMetadata;

/// The common structure of all type metadata.
struct Metadata {
  constexpr Metadata() : Kind(MetadataKind::Class) {}
  constexpr Metadata(MetadataKind Kind) : Kind(Kind) {}
  
  /// The basic header type.
  typedef TypeMetadataHeader HeaderType;

private:
  /// The kind. Only valid for non-class metadata; getKind() must be used to get
  /// the kind value.
  MetadataKind Kind;
public:
  /// Get the metadata kind.
  MetadataKind getKind() const {
    if (Kind > MetadataKind::MetadataKind_Last)
      return MetadataKind::Class;
    return Kind;
  }
  
  /// Set the metadata kind.
  void setKind(MetadataKind kind) {
    Kind = kind;
  }

  /// Is this a class object--the metadata record for a Swift class (which also
  /// serves as the class object), or the class object for an ObjC class (which
  /// is not metadata)?
  bool isClassObject() const {
    return Kind > MetadataKind::MetadataKind_Last
      || Kind == MetadataKind::Class;
  }
  
  /// Does the given metadata kind represent metadata for some kind of class?
  static bool isAnyKindOfClass(MetadataKind k) {
    switch (k) {
    case MetadataKind::Class:
    case MetadataKind::ObjCClassWrapper:
    case MetadataKind::ForeignClass:
    case MetadataKind::Block:
      return true;
        
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
    case MetadataKind::Function:
    case MetadataKind::ThinFunction:
    case MetadataKind::PolyFunction:
    case MetadataKind::Existential:
    case MetadataKind::Metatype:
    case MetadataKind::ExistentialMetatype:
    case MetadataKind::HeapLocalVariable:
      return false;
    }
    assert(false && "not a metadata kind");
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
    case MetadataKind::Block:
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Opaque:
    case MetadataKind::Tuple:
    case MetadataKind::Function:
    case MetadataKind::ThinFunction:
    case MetadataKind::PolyFunction:
    case MetadataKind::HeapLocalVariable:
      return false;
    }
    assert(false && "not a metadata kind");
  }
  
  /// Is this either type metadata or a class object for any kind of class?
  bool isAnyClass() const {
    return isAnyKindOfClass(getKind());
  }

  const ValueWitnessTable *getValueWitnesses() const {
    return asFullMetadata(this)->ValueWitnesses;
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
  
  /// Get the nominal type descriptor if this metadata describes a nominal type,
  /// or return null if it does not.
  const NominalTypeDescriptor *getNominalTypeDescriptor() const;
  
  /// Get the generic metadata pattern from which this generic type instance was
  /// instantiated, or null if the type is not generic.
  const GenericMetadata *getGenericPattern() const;
  
  /// Get the class object for this type if it has one, or return null if the
  /// type is not a class (or not a class with a class object).
  const ClassMetadata *getClassObject() const;
  
protected:
  friend struct OpaqueMetadata;
  
  /// Metadata should not be publicly copied or moved.
  constexpr Metadata(const Metadata &) = default;
  Metadata &operator=(const Metadata &) = default;
  constexpr Metadata(Metadata &&) = default;
  Metadata &operator=(Metadata &&) = default;
};

/// The common structure of opaque metadata.  Adds nothing.
struct OpaqueMetadata {
  typedef TypeMetadataHeader HeaderType;

  // We have to represent this as a member so we can list-initialize it.
  Metadata base;
};

// Standard POD opaque metadata.
// The "Int" metadata are used for arbitrary POD data with the
// matching characteristics.
typedef FullMetadata<OpaqueMetadata> FullOpaqueMetadata;
extern "C" const FullOpaqueMetadata _TMdBi8_;      // Builtin.Int8
extern "C" const FullOpaqueMetadata _TMdBi16_;     // Builtin.Int16
extern "C" const FullOpaqueMetadata _TMdBi32_;     // Builtin.Int32
extern "C" const FullOpaqueMetadata _TMdBi64_;     // Builtin.Int64
extern "C" const FullOpaqueMetadata _TMdBi128_;    // Builtin.Int128
extern "C" const FullOpaqueMetadata _TMdBo;        // Builtin.NativeObject
extern "C" const FullOpaqueMetadata _TMdBb;        // Builtin.BridgeObject
extern "C" const FullOpaqueMetadata _TMdBB;        // Builtin.UnsafeValueBuffer
#if SWIFT_OBJC_INTEROP
extern "C" const FullOpaqueMetadata _TMdBO;        // Builtin.UnknownObject
#endif

/// The prefix on a heap metadata.
struct HeapMetadataHeaderPrefix {
  /// Destroy the object, returning the allocated size of the object
  /// or 0 if the object shouldn't be deallocated.
  void (*destroy)(HeapObject *);
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
struct HeapMetadata : Metadata {
  typedef HeapMetadataHeader HeaderType;

  HeapMetadata() = default;
  constexpr HeapMetadata(const Metadata &base) : Metadata(base) {}
};

/// Header for a generic parameter descriptor. This is a variable-sized
/// structure that describes how to find and parse a generic parameter vector
/// within
struct GenericParameterDescriptor {
  /// The offset of the descriptor in the metadata record. If NumParams is zero,
  /// this value is meaningless.
  uint32_t Offset;
  /// The number of type parameters. A value of zero means there is no generic
  /// parameter vector. This includes associated types of the primary type
  /// parameters.
  uint32_t NumParams;
  /// The number of primary type parameters. This is always less than or equal
  /// to NumParams; it counts only the primary type parameters and not their
  /// associated types.
  uint32_t NumPrimaryParams;
  
  /// True if the nominal type has generic parameters.
  bool hasGenericParams() const { return NumParams > 0; }
  
  /// A type parameter.
  struct Parameter {
    /// The number of protocol witness tables required by this type parameter.
    uint32_t NumWitnessTables;
    
    // TODO: This is the bare minimum to be able to parse an opaque generic
    // parameter vector. Should we include additional info, such as the
    // required protocols?
  };

  /// The parameter descriptors are in a tail-emplaced array of NumParams
  /// elements.
  Parameter Parameters[1];
};
  
struct ClassTypeDescriptor;
struct StructTypeDescriptor;
struct EnumTypeDescriptor;

/// Common information about all nominal types. For generic types, this
/// descriptor is shared for all instantiations of the generic type.
struct NominalTypeDescriptor {
  /// The kind of nominal type descriptor.
  NominalTypeKind Kind;
  /// The mangled name of the nominal type, with no generic parameters.
  const char *Name;
  
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
      
      /// True if metadata records for this type have a field offset vector for
      /// its stored properties.
      bool hasFieldOffsetVector() const { return FieldOffsetVectorOffset != 0; }
      
      /// The field names. A doubly-null-terminated list of strings, whose
      /// length and order is consistent with that of the field offset vector.
      const char *FieldNames;
      
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// field offset vector.
      const Metadata * const *(*GetFieldTypes)(const Metadata *Self);
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
      
      /// True if metadata records for this type have a field offset vector for
      /// its stored properties.
      bool hasFieldOffsetVector() const { return FieldOffsetVectorOffset != 0; }
      
      /// The field names. A doubly-null-terminated list of strings, whose
      /// length and order is consistent with that of the field offset vector.
      const char *FieldNames;
      
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// field offset vector.
      const Metadata * const *(*GetFieldTypes)(const Metadata *Self);
    } Struct;
    
    /// Information about enum types.
    struct {
      /// The number of non-empty cases in the enum.
      uint32_t NumNonEmptyCases;
      /// The number of empty cases in the enum.
      uint32_t NumEmptyCases;
      /// The names of the cases. A doubly-null-terminated list of strings,
      /// whose length is NumNonEmptyCases + NumEmptyCases. Cases are named in
      /// tag order, non-empty cases first, followed by empty cases.
      const char *CaseNames;
      /// The field type vector accessor. Returns a pointer to an array of
      /// type metadata references whose order is consistent with that of the
      /// CaseNames.
      const Metadata * const *(*GetCaseTypes)(const Metadata *Self);
    } Enum;
  };
  
  /// A pointer to the generic metadata pattern that is used to instantiate
  /// instances of this type. Null if the type is not generic.
  GenericMetadata *GenericMetadataPattern;
  
  /// The generic parameter descriptor header. This describes how to find and
  /// parse the generic parameter vector in metadata records for this nominal
  /// type.
  GenericParameterDescriptor GenericParams;
  
  // NOTE: GenericParams ends with a tail-allocated array, so it cannot be
  // followed by additional fields.
};

/// The structure of all class metadata.  This structure is embedded
/// directly within the class's heap metadata structure and therefore
/// cannot be extended without an ABI break.
///
/// Note that the layout of this type is compatible with the layout of
/// an Objective-C class.
struct ClassMetadata : public HeapMetadata {
  ClassMetadata() = default;
  constexpr ClassMetadata(const HeapMetadata &base,
                          const ClassMetadata *superClass,
                          uintptr_t data,
                          ClassFlags flags,
                          const NominalTypeDescriptor *description,
                          uintptr_t size, uintptr_t addressPoint,
                          uintptr_t alignMask,
                          uintptr_t classSize, uintptr_t classAddressPoint)
    : HeapMetadata(base), SuperClass(superClass),
      CacheData{nullptr, nullptr}, Data(data),
      Flags(flags), InstanceAddressPoint(addressPoint),
      InstanceSize(size), InstanceAlignMask(alignMask),
      Reserved(0), ClassSize(classSize), ClassAddressPoint(classAddressPoint),
      Description(description) {}

  /// The metadata for the superclass.  This is null for the root class.
  const ClassMetadata *SuperClass;

  /// The cache data is used for certain dynamic lookups; it is owned
  /// by the runtime and generally needs to interoperate with
  /// Objective-C's use.
  void *CacheData[2];

  /// The data pointer is used for out-of-line metadata and is
  /// generally opaque, except that the compiler sets the low bit in
  /// order to indicate that this is a Swift metatype and therefore
  /// that the type metadata header is present.
  uintptr_t Data;

  /// Is this object a valid swift type metadata?
  bool isTypeMetadata() const {
    return Data & 1;
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
  /// supported mechanism for making a non-artifical subclass
  /// dynamically.
  const NominalTypeDescriptor *Description;

  // After this come the class members, laid out as follows:
  //   - class members for the superclass (recursively)
  //   - metadata reference for the parent, if applicable
  //   - generic parameters for this class
  //   - class variables (if we choose to support these)
  //   - "tabulated" virtual methods

public:
  const NominalTypeDescriptor *getDescription() const {
    assert(isTypeMetadata());
    assert(!isArtificialSubclass());
    return Description;
  }

  /// Is this class an artificial subclass, such as one dynamically
  /// created for various dynamic purposes like KVO?
  bool isArtificialSubclass() const {
    assert(isTypeMetadata());
    return Description == nullptr;
  }
  void setArtificialSubclass() {
    assert(isTypeMetadata());
    Description = nullptr;
  }

  ClassFlags getFlags() const {
    assert(isTypeMetadata());
    return Flags;
  }
  void setFlags(ClassFlags flags) {
    assert(isTypeMetadata());
    Flags = flags;
  }

  uintptr_t getInstanceSize() const {
    assert(isTypeMetadata());
    return InstanceSize;
  }
  void setInstanceSize(uintptr_t size) {
    assert(isTypeMetadata());
    InstanceSize = size;
  }

  uintptr_t getInstanceAddressPoint() const {
    assert(isTypeMetadata());
    return InstanceAddressPoint;
  }
  void setInstanceAddressPoint(uintptr_t size) {
    assert(isTypeMetadata());
    InstanceAddressPoint = size;
  }

  uintptr_t getInstanceAlignMask() const {
    assert(isTypeMetadata());
    return InstanceAlignMask;
  }
  void setInstanceAlignMask(uintptr_t mask) {
    assert(isTypeMetadata());
    InstanceAlignMask = mask;
  }

  uintptr_t getClassSize() const {
    assert(isTypeMetadata());
    return ClassSize;
  }
  void setClassSize(uintptr_t size) {
    assert(isTypeMetadata());
    ClassSize = size;
  }

  uintptr_t getClassAddressPoint() const {
    assert(isTypeMetadata());
    return ClassAddressPoint;
  }
  void setClassAddressPoint(uintptr_t offset) {
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
  const uintptr_t *getFieldOffsets() const {
    assert(isTypeMetadata());
    auto offset = Description->Class.FieldOffsetVectorOffset;
    if (offset == 0)
      return nullptr;
    auto asWords = reinterpret_cast<const void * const*>(this);
    return reinterpret_cast<const uintptr_t *>(asWords + offset);
  }
  
  /// Get a pointer to the field type vector, if present, or null.
  const Metadata * const *getFieldTypes() const {
    assert(isTypeMetadata());
    auto *getter = Description->Class.GetFieldTypes;
    if (!getter)
      return nullptr;
    
    return getter(this);
  }

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Class;
  }
};

/// The structure of metadata for heap-allocated local variables.
/// This is non-type metadata.
///
/// It would be nice for tools to be able to dynamically discover the
/// type of a heap-allocated local variable.  This should not require
/// us to aggressively produce metadata for the type, though.  The
/// obvious solution is to simply place the mangling of the type after
/// the variable metadata.
///
/// One complication is that, in generic code, we don't want something
/// as low-priority (sorry!) as the convenience of tools to force us
/// to generate per-instantiation metadata for capturing variables.
/// In these cases, the heap-destructor function will be using
/// information stored in the allocated object (rather than in
/// metadata) to actually do the work of destruction, but even then,
/// that information needn't be metadata for the actual variable type;
/// consider the case of local variable of type (T, Int).
///
/// Anyway, that's all something to consider later.
struct HeapLocalVariableMetadata : public HeapMetadata {
  // No extra fields for now.
};

/// The structure of wrapper metadata for Objective-C classes.  This
/// is used as a type metadata pointer when the actual class isn't
/// Swift-compiled.
struct ObjCClassWrapperMetadata : public Metadata {
  const ClassMetadata *Class;

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::ObjCClassWrapper;
  }
};

/// The structure of metadata for foreign types where the source
/// language doesn't provide any sort of more interesting metadata for
/// us to use.
struct ForeignTypeMetadata : public Metadata {
  using InitializationFunction_t =
    void (*)(ForeignTypeMetadata *selectedMetadata);

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
    const char *Name;
    
    /// A pointer to the actual, runtime-uniqued metadata for this
    /// type.  This is essentially an invasive cache for the lookup
    /// structure.
    mutable std::atomic<const ForeignTypeMetadata *> Unique;
    
    /// Various flags.
    enum : size_t {
      /// This metadata has an initialization callback function.  If
      /// this flag is not set, the metadata object needn't actually
      /// have a InitializationFunction field.
      HasInitializationFunction = 0x1,
    } Flags;
  };

  struct HeaderType : HeaderPrefix, TypeMetadataHeader {};

  const char *getName() const {
    return asFullMetadata(this)->Name;
  }
  
  const ForeignTypeMetadata *getCachedUniqueMetadata() const {
#if __alpha__
    // TODO: This can be a relaxed-order load if there is no initialization
    // function. On platforms we care about, consume is no more expensive than
    // relaxed, so there's no reason to branch here (and LLVM isn't smart
    // enough to eliminate it when it's not needed).
    if (!hasInitializationFunction())
      return asFullMetadata(this)->Unique.load(std::memory_order_relaxed);
#endif
#if __arm64__
    // FIXME: Workaround for rdar://problem/18889711. 'Consume' does not require
    // a barrier on ARM64, but LLVM doesn't know that. Although 'relaxed'
    // is formally UB by C++11 language rules, we should be OK because neither
    // the processor model nor the optimizer can realistically reorder this.
    return asFullMetadata(this)->Unique.load(std::memory_order_relaxed);
#else
    return asFullMetadata(this)->Unique.load(std::memory_order_consume);
#endif
  }
  
  void setCachedUniqueMetadata(const ForeignTypeMetadata *unique) const {
    assert((asFullMetadata(this)->Unique == nullptr
            || asFullMetadata(this)->Unique == unique)
           && "already set unique metadata");
    
    // If there is no initialization function, this can be a relaxed store.
    if (!hasInitializationFunction())
      asFullMetadata(this)->Unique.store(unique, std::memory_order_relaxed);
    
    // Otherwise, we need a release store to publish the result of
    // initialization
    else
      asFullMetadata(this)->Unique.store(unique, std::memory_order_release);
  }
  
  size_t getFlags() const {
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

/// The structure of metadata objects for foreign class types.
/// A foreign class is a foreign type with reference semantics and
/// Swift-supported reference counting.  Generally this requires
/// special logic in the importer.
///
/// We assume for now that foreign classes are entirely opaque
/// to Swift introspection.
struct ForeignClassMetadata : public ForeignTypeMetadata {
  /// The superclass of the foreign class, if any.
  const ForeignClassMetadata *SuperClass;

  /// Reserved space.  For now, these should be zero-initialized.
  void *Reserved[3];

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::ForeignClass;
  }
};

/// The structure of type metadata for structs.
struct StructMetadata : public Metadata {
  /// An out-of-line description of the type.
  const NominalTypeDescriptor *Description;

  /// The parent type of this member type, or null if this is not a
  /// member type.
  const Metadata *Parent;
  
  /// Get a pointer to the field offset vector, if present, or null.
  const uintptr_t *getFieldOffsets() const {
    auto offset = Description->Struct.FieldOffsetVectorOffset;
    if (offset == 0)
      return nullptr;
    auto asWords = reinterpret_cast<const void * const*>(this);
    return reinterpret_cast<const uintptr_t *>(asWords + offset);
  }
  
  /// Get a pointer to the field type vector, if present, or null.
  const Metadata * const *getFieldTypes() const {
    auto *getter = Description->Struct.GetFieldTypes;
    if (!getter)
      return nullptr;
    
    return getter(this);
  }

  /// Retrieve the generic arguments of this struct.
  const Metadata * const *getGenericArgs() const {
    const void* const *asWords = reinterpret_cast<const void * const *>(this);
    if (Description->GenericParams.NumParams == 0)
      return nullptr;

    asWords += Description->GenericParams.Offset;
    return reinterpret_cast<const Metadata * const *>(asWords);
  }

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Struct;
  }
};

/// The structure of function type metadata.
struct FunctionTypeMetadata : public Metadata {
  using Argument = FlaggedPointer<const Metadata *, 0>;

  /// The number of arguments to the function.
  size_t NumArguments;

  /// The type metadata for the result type.
  const Metadata *ResultType;

  Argument *getArguments() {
    return reinterpret_cast<Argument *>(this + 1);
  }

  const Argument *getArguments() const {
    return reinterpret_cast<const Argument *>(this + 1);
  }

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Function ||
           metadata->getKind() == MetadataKind::ThinFunction ||
           metadata->getKind() == MetadataKind::Block;
  }
};

/// The structure of metadata for metatypes.
struct MetatypeMetadata : public Metadata {
  /// The type metadata for the element.
  const Metadata *InstanceType;

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Metatype;
  }
};

/// The structure of tuple type metadata.
struct TupleTypeMetadata : public Metadata {
  TupleTypeMetadata() = default;
  constexpr TupleTypeMetadata(const Metadata &base,
                              size_t numElements,
                              const char *labels)
    : Metadata(base), NumElements(numElements), Labels(labels) {}

  /// The number of elements.
  size_t NumElements;

  /// The labels string;  see swift_getTupleTypeMetadata.
  const char *Labels;

  struct Element {
    /// The type of the element.
    const Metadata *Type;

    /// The offset of the tuple element within the tuple.
    size_t Offset;

    OpaqueValue *findIn(OpaqueValue *tuple) const {
      return (OpaqueValue*) (((char*) tuple) + Offset);
    }
  };

  Element *getElements() {
    return reinterpret_cast<Element*>(this+1);
  }
  const Element *getElements() const {
    return reinterpret_cast<const Element *>(this+1);
  }

  const Element &getElement(unsigned i) const { return getElements()[i]; }
  Element &getElement(unsigned i) { return getElements()[i]; }

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Tuple;
  }
};
  
/// The standard metadata for the empty tuple type.
extern "C" const FullMetadata<TupleTypeMetadata> _TMdT_;

struct ProtocolDescriptor;
  
/// An array of protocol descriptors with a header and tail-allocated elements.
struct ProtocolDescriptorList {
  uintptr_t NumProtocols;

  const ProtocolDescriptor **getProtocols() {
    return reinterpret_cast<const ProtocolDescriptor **>(this + 1);
  }
  
  const ProtocolDescriptor * const *getProtocols() const {
    return reinterpret_cast<const ProtocolDescriptor * const *>(this + 1);
  }
  
  const ProtocolDescriptor *operator[](size_t i) const {
    return getProtocols()[i];
  }
  
  const ProtocolDescriptor *&operator[](size_t i) {
    return getProtocols()[i];
  }

  constexpr ProtocolDescriptorList() : NumProtocols(0) {}
  
protected:
  constexpr ProtocolDescriptorList(uintptr_t NumProtocols)
    : NumProtocols(NumProtocols) {}
};
  
/// A literal class for creating constant protocol descriptors in the runtime.
template<uintptr_t NUM_PROTOCOLS>
struct LiteralProtocolDescriptorList : ProtocolDescriptorList {
  const ProtocolDescriptorList *Protocols[NUM_PROTOCOLS];
  
  template<typename...DescriptorPointers>
  constexpr LiteralProtocolDescriptorList(DescriptorPointers...elements)
    : ProtocolDescriptorList(NUM_PROTOCOLS), Protocols{elements...}
  {}
};
  
/// A protocol descriptor. This is not type metadata, but is referenced by
/// existential type metadata records to describe a protocol constraint.
/// Its layout is compatible with the Objective-C runtime's 'protocol_t' record
/// layout.
struct ProtocolDescriptor {
  /// Unused by the Swift runtime.
  const void *_ObjC_Isa;
  
  /// The mangled name of the protocol.
  const char *Name;
  
  /// The list of protocols this protocol refines.
  const ProtocolDescriptorList *InheritedProtocols;
  
  /// Unused by the Swift runtime.
  const void *_ObjC_InstanceMethods, *_ObjC_ClassMethods,
             *_ObjC_OptionalInstanceMethods, *_ObjC_OptionalClassMethods,
             *_ObjC_InstanceProperties;
  
  /// Size of the descriptor record.
  uint32_t DescriptorSize;
  
  /// Additional flags.
  ProtocolDescriptorFlags Flags;
  
  constexpr ProtocolDescriptor(const char *Name,
                               const ProtocolDescriptorList *Inherited,
                               ProtocolDescriptorFlags Flags)
    : _ObjC_Isa(nullptr), Name(Name), InheritedProtocols(Inherited),
      _ObjC_InstanceMethods(nullptr), _ObjC_ClassMethods(nullptr),
      _ObjC_OptionalInstanceMethods(nullptr),
      _ObjC_OptionalClassMethods(nullptr),
      _ObjC_InstanceProperties(nullptr),
      DescriptorSize(sizeof(ProtocolDescriptor)),
      Flags(Flags)
  {}
};
  
/// A witness table for a protocol. This type is intentionally opaque because
/// the layout of a witness table is dependent on the protocol being
/// represented.
struct WitnessTable;

/// The basic layout of an opaque (non-class-bounded) existential type.
struct OpaqueExistentialContainer {
  ValueBuffer Buffer;
  const Metadata *Type;
  // const void *WitnessTables[];

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable**>(this + 1);
  }
  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable* const *>(this + 1);
  }

  void copyTypeInto(OpaqueExistentialContainer *dest, unsigned numTables) const {
    dest->Type = Type;
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};

/// The basic layout of a class-bounded existential type.
struct ClassExistentialContainer {
  void *Value;

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable**>(this + 1);
  }
  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable* const *>(this + 1);
  }

  void copyTypeInto(ClassExistentialContainer *dest, unsigned numTables) const {
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};

/// The possible physical representations of existential types.
enum class ExistentialTypeRepresentation {
  /// The type uses an opaque existential representation.
  Opaque,
  /// The type uses a class existential representation.
  Class,
  /// The type uses the ErrorType boxed existential representation.
  ErrorType,
};

/// The structure of existential type metadata.
struct ExistentialTypeMetadata : public Metadata {
  /// The number of witness tables and class-constrained-ness of the type.
  ExistentialTypeFlags Flags;
  /// The protocol constraints.
  ProtocolDescriptorList Protocols;
  
  /// NB: Protocols has a tail-emplaced array; additional fields cannot follow.
  
  constexpr ExistentialTypeMetadata()
    : Metadata{MetadataKind::Existential},
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
  const Metadata *getDynamicType(const OpaqueValue *container) const;
  
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

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::Existential;
  }
};

/// The basic layout of an existential metatype type.
struct ExistentialMetatypeContainer {
  const Metadata *Value;

  const WitnessTable **getWitnessTables() {
    return reinterpret_cast<const WitnessTable**>(this + 1);
  }
  const WitnessTable * const *getWitnessTables() const {
    return reinterpret_cast<const WitnessTable* const *>(this + 1);
  }

  void copyTypeInto(ExistentialMetatypeContainer *dest,
                    unsigned numTables) const {
    for (unsigned i = 0; i != numTables; ++i)
      dest->getWitnessTables()[i] = getWitnessTables()[i];
  }
};

/// The structure of metadata for existential metatypes.
struct ExistentialMetatypeMetadata : public Metadata {
  /// The type metadata for the element.
  const Metadata *InstanceType;

  /// The number of witness tables and class-constrained-ness of the
  /// underlying type.
  ExistentialTypeFlags Flags;

  static bool classof(const Metadata *metadata) {
    return metadata->getKind() == MetadataKind::ExistentialMetatype;
  }
};

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
struct GenericMetadata {
  /// The fill function. Receives a pointer to the instantiated metadata and
  /// the argument pointer passed to swift_getGenericMetadata.
  Metadata *(*CreateFunction)(GenericMetadata *pattern, const void *arguments);
  
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
  void *PrivateData[swift::NumGenericMetadataPrivateDataWords];

  // Here there is a variably-sized field:
  // char alignas(void*) MetadataTemplate[MetadataSize];

  /// Return the starting address of the metadata template data.
  const void *getMetadataTemplate() const {
    return reinterpret_cast<const void *>(this + 1);
  }
};

/// The structure of a protocol conformance record.
///
/// This contains enough static information to recover the witness table for a
/// type's conformance to a protocol.
struct ProtocolConformanceRecord {
public:
  using WitnessTableAccessor_t = const WitnessTable *(*)(const Metadata*);

private:
  /// The protocol being conformed to.
  const ProtocolDescriptor *Protocol;
  
  // Some description of the type that conforms to the protocol.
  union {
    /// A direct reference to the metadata.
    ///
    /// Depending on the conformance kind, this may not be usable
    /// metadata without being first processed by the runtime.
    const Metadata *DirectType;
    
    /// An indirect reference to the metadata.
    const ClassMetadata * const *IndirectClass;
    
    /// The generic metadata pattern for a generic type which has instances that
    /// conform to the protocol.
    const GenericMetadata *GenericPattern;
  };
  
  
  // The conformance, or a generator function for the conformance.
  union {
    /// A direct reference to the witness table for the conformance.
    const WitnessTable *WitnessTable;
    
    /// A function that produces the witness table given an instance of the
    /// type. The function may return null if a specific instance does not
    /// conform to the protocol.
    WitnessTableAccessor_t WitnessTableAccessor;
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
  
  ProtocolConformanceTypeKind getTypeKind() const {
    return Flags.getTypeKind();
  }
  ProtocolConformanceReferenceKind getConformanceKind() const {
    return Flags.getConformanceKind();
  }
  
  const Metadata *getDirectType() const {
    switch (Flags.getTypeKind()) {
    case ProtocolConformanceTypeKind::Universal: // will be null in this case
    case ProtocolConformanceTypeKind::UniqueDirectType:
    case ProtocolConformanceTypeKind::NonuniqueDirectType:
      break;
        
    case ProtocolConformanceTypeKind::UniqueDirectClass:
    case ProtocolConformanceTypeKind::UniqueIndirectClass:
    case ProtocolConformanceTypeKind::UniqueGenericPattern:
      assert(false && "not direct type metadata");
    }

    return DirectType;
  }
  
  // FIXME: This shouldn't exist
  const ClassMetadata *getDirectClass() const {
    switch (Flags.getTypeKind()) {
    case ProtocolConformanceTypeKind::Universal: // will be null in this case
    case ProtocolConformanceTypeKind::UniqueDirectClass:
      break;
        
    case ProtocolConformanceTypeKind::UniqueDirectType:
    case ProtocolConformanceTypeKind::NonuniqueDirectType:
    case ProtocolConformanceTypeKind::UniqueGenericPattern:
    case ProtocolConformanceTypeKind::UniqueIndirectClass:
      assert(false && "not direct class object");
    }
    
    return static_cast<const ClassMetadata*>(DirectType);
    
  }
  
  const ClassMetadata * const *getIndirectClass() const {
    switch (Flags.getTypeKind()) {
    case ProtocolConformanceTypeKind::Universal: // will be null in this case
    case ProtocolConformanceTypeKind::UniqueIndirectClass:
      break;
        
    case ProtocolConformanceTypeKind::UniqueDirectType:
    case ProtocolConformanceTypeKind::UniqueDirectClass:
    case ProtocolConformanceTypeKind::NonuniqueDirectType:
    case ProtocolConformanceTypeKind::UniqueGenericPattern:
      assert(false && "not indirect class object");
    }
    
    return IndirectClass;
  }
  
  const GenericMetadata *getGenericPattern() const {
    switch (Flags.getTypeKind()) {
    case ProtocolConformanceTypeKind::Universal: // will be null in this case
    case ProtocolConformanceTypeKind::UniqueGenericPattern:
      break;
        
    case ProtocolConformanceTypeKind::UniqueDirectClass:
    case ProtocolConformanceTypeKind::UniqueIndirectClass:
    case ProtocolConformanceTypeKind::UniqueDirectType:
    case ProtocolConformanceTypeKind::NonuniqueDirectType:
      assert(false && "not generic metadata pattern");
    }
    
    return GenericPattern;
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
  
  WitnessTableAccessor_t getWitnessTableAccessor() const {
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
  const Metadata *getCanonicalTypeMetadata() const;
  
  /// Get the witness table for the specified type, realizing it if
  /// necessary, or return null if the conformance does not apply to the
  /// type.
  const swift::WitnessTable *getWitnessTable(const Metadata *type) const;
  
#if defined(NDEBUG) && SWIFT_OBJC_INTEROP
  void dump() const;
#endif
};

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
extern "C" const Metadata *
swift_getGenericMetadata(GenericMetadata *pattern,
                         const void *arguments);

// Fast entry points for swift_getGenericMetadata with a small number of
// template arguments.
extern "C" const Metadata *
swift_getGenericMetadata1(GenericMetadata *pattern,
                          const void *arg0);
extern "C" const Metadata *
swift_getGenericMetadata2(GenericMetadata *pattern,
                          const void *arg0,
                          const void *arg1);
extern "C" const Metadata *
swift_getGenericMetadata3(GenericMetadata *pattern,
                          const void *arg0,
                          const void *arg1,
                          const void *arg2);
extern "C" const Metadata *
swift_getGenericMetadata4(GenericMetadata *pattern,
                          const void *arg0,
                          const void *arg1,
                          const void *arg2,
                          const void *arg3);

// Callback to allocate a generic class metadata object.
extern "C" ClassMetadata *
swift_allocateGenericClassMetadata(GenericMetadata *pattern,
                                   const void *arguments,
                                   ClassMetadata *superclass);

// Callback to allocate a generic struct/enum metadata object.
extern "C" Metadata *
swift_allocateGenericValueMetadata(GenericMetadata *pattern,
                                   const void *arguments);
  
/// \brief Fetch a uniqued metadata for a function type.
extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata(size_t numArguments,
                              const void * argsAndResult []);

extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata0(const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata1(const void *arg0,
                               const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata2(const void *arg0,
                               const void *arg1,
                               const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata3(const void *arg0,
                               const void *arg1,
                               const void *arg2,
                               const Metadata *resultMetadata);

/// \brief Fetch a uniqued metadata for a thin function type.
extern "C" const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata(size_t numArguments,
                                  const void * argsAndResult []);

extern "C" const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata0(const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata1(const void *arg0,
                                   const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata2(const void *arg0,
                                   const void *arg1,
                                   const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getThinFunctionTypeMetadata3(const void *arg0,
                                   const void *arg1,
                                   const void *arg2,
                                   const Metadata *resultMetadata);


#if SWIFT_OBJC_INTEROP
/// \brief Fetch a uniqued metadata for a block type.
extern "C" const FunctionTypeMetadata *
swift_getBlockTypeMetadata(size_t numArguments,
                           const void *argsAndResult []);

extern "C" const FunctionTypeMetadata *
swift_getBlockTypeMetadata0(const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getBlockTypeMetadata1(const void *arg0,
                            const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getBlockTypeMetadata2(const void *arg0,
                            const void *arg1,
                            const Metadata *resultMetadata);

extern "C" const FunctionTypeMetadata *
swift_getBlockTypeMetadata3(const void *arg0,
                            const void *arg1,
                            const void *arg2,
                            const Metadata *resultMetadata);

extern "C" void
swift_instantiateObjCClass(const ClassMetadata *theClass);
#endif

/// \brief Fetch a uniqued type metadata for an ObjC class.
extern "C" const Metadata *
swift_getObjCClassMetadata(const ClassMetadata *theClass);

/// \brief Fetch a unique type metadata object for a foreign type.
extern "C" const ForeignTypeMetadata *
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
extern "C" const TupleTypeMetadata *
swift_getTupleTypeMetadata(size_t numElements,
                           const Metadata * const *elements,
                           const char *labels,
                           const ValueWitnessTable *proposedWitnesses);

extern "C" const TupleTypeMetadata *
swift_getTupleTypeMetadata2(const Metadata *elt0, const Metadata *elt1,
                            const char *labels,
                            const ValueWitnessTable *proposedWitnesses);
extern "C" const TupleTypeMetadata *
swift_getTupleTypeMetadata3(const Metadata *elt0, const Metadata *elt1,
                            const Metadata *elt2, const char *labels,
                            const ValueWitnessTable *proposedWitnesses);

/// Initialize the value witness table and struct field offset vector for a
/// struct, using the "Universal" layout strategy.
extern "C" void swift_initStructMetadata_UniversalStrategy(size_t numFields,
                                         const Metadata * const *fieldTypes,
                                         size_t *fieldOffsets,
                                         ValueWitnessTable *vwtable);

struct ClassFieldLayout {
  size_t Size;
  size_t AlignMask;
};

/// Initialize the field offset vector for a dependent-layout class, using the
/// "Universal" layout strategy.
extern "C" void swift_initClassMetadata_UniversalStrategy(ClassMetadata *self,
                                            const ClassMetadata *super,
                                            size_t numFields,
                                      const ClassFieldLayout *fieldLayouts,
                                            size_t *fieldOffsets);
  
/// \brief Fetch a uniqued metadata for a metatype type.
extern "C" const MetatypeMetadata *
swift_getMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential metatype type.
extern "C" const ExistentialMetatypeMetadata *
swift_getExistentialMetatypeMetadata(const Metadata *instanceType);

/// \brief Fetch a uniqued metadata for an existential type. The array
/// referenced by \c protocols will be sorted in-place.
extern "C" const ExistentialTypeMetadata *
swift_getExistentialTypeMetadata(size_t numProtocols,
                                 const ProtocolDescriptor **protocols);

// Mask and left shift for reading the isa directly from Swift objects when
// we know that dynamic subclassing (CoreData, KVO, etc) doesn't happen.
extern "C" size_t swift_classMask;
extern "C" uint8_t swift_classShift;

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
extern "C" bool
swift_dynamicCast(OpaqueValue *dest, OpaqueValue *src,
                  const Metadata *srcType,
                  const Metadata *targetType,
                  DynamicCastFlags flags);

/// \brief Checked dynamic cast to a Swift class type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a Swift class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
extern "C" const void *
swift_dynamicCastClass(const void *object, const ClassMetadata *targetType);

/// \brief Unconditional, checked dynamic cast to a Swift class type.
///
/// Aborts if the object isn't of the target type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a Swift class type.
///
/// \returns the object.
extern "C" const void *
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
extern "C" const void *
swift_dynamicCastObjCClass(const void *object, const ClassMetadata *targetType);

/// \brief Checked dynamic cast to a foreign class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a foreign class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
extern "C" const void *
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
extern "C" const void *
swift_dynamicCastObjCClassUnconditional(const void *object,
                                        const ClassMetadata *targetType);

/// \brief Unconditional, checked dynamic cast to a foreign class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a foreign class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
extern "C" const void *
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
extern "C" const void *
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
extern "C" const void *
swift_dynamicCastUnknownClassUnconditional(const void *object,
                                           const Metadata *targetType);

#if SWIFT_OBJC_INTEROP
extern "C" const Metadata *
swift_dynamicCastMetatype(const Metadata *sourceType,
                          const Metadata *targetType);
extern "C" const Metadata *
swift_dynamicCastMetatypeUnconditional(const Metadata *sourceType,
                                       const Metadata *targetType);
extern "C" const ClassMetadata *
swift_dynamicCastObjCClassMetatype(const ClassMetadata *sourceType,
                                   const ClassMetadata *targetType);
extern "C" const ClassMetadata *
swift_dynamicCastObjCClassMetatypeUnconditional(const ClassMetadata *sourceType,
                                                const ClassMetadata *targetType);

extern "C" const ClassMetadata *
swift_dynamicCastForeignClassMetatype(const ClassMetadata *sourceType,
                                   const ClassMetadata *targetType);
extern "C" const ClassMetadata *
swift_dynamicCastForeignClassMetatypeUnconditional(
  const ClassMetadata *sourceType,
  const ClassMetadata *targetType);
#endif

/// \brief Return the dynamic type of an opaque value.
///
/// \param value An opaque value.
/// \param self  The static type metadata for the opaque value.
extern "C" const Metadata *
swift_getDynamicType(OpaqueValue *value, const Metadata *self);

/// \brief Fetch the type metadata associated with the formal dynamic
/// type of the given (possibly Objective-C) object.  The formal
/// dynamic type ignores dynamic subclasses such as those introduced
/// by KVO.
///
/// The object pointer may be a tagged pointer, but cannot be null.
extern "C" const Metadata *swift_getObjectType(HeapObject *object);

/// \brief Perform a copy-assignment from one existential container to another.
/// Both containers must be of the same existential type representable with the
/// same number of witness tables.
extern "C" OpaqueValue *swift_assignExistentialWithCopy(OpaqueValue *dest,
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
  *dest = reinterpret_cast<void*>((unsigned) index);
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
extern "C"
const WitnessTable *swift_conformsToProtocol(const Metadata *type,
                                            const ProtocolDescriptor *protocol);

/// Register a block of protocol conformance records for dynamic lookup.
extern "C"
void swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                        const ProtocolConformanceRecord *end);
  
/// FIXME: This doesn't belong in the runtime.
extern "C" void swift_printAny(OpaqueValue *value, const Metadata *type);

/// \brief Demangle a mangled class name into module+class.
/// Returns true if the name was successfully decoded.
/// On success, *outModule and *outClass must be freed with free().
extern "C" bool
swift_demangleSimpleClass(const char *mangledName, 
                          char **outModule, char **outClass);
  

/// Return the type name for a given type metadata.
std::string nameForMetadata(const Metadata *type);

} // end namespace swift

#endif /* SWIFT_RUNTIME_METADATA_H */
