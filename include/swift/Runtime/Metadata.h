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

#include <cstddef>
#include <cstdint>
#include "swift/ABI/MetadataValues.h"

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

struct ValueWitnessTable;

/// Types stored in the value-witness table.
class ValueWitnessFlags {
  typedef size_t int_type;
  enum : int_type {
    AlignmentMask = 0x0000FFFF,
    IsNonPOD =      0x00010000,
    IsNonInline =   0x00020000,
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
  ///
  /// Unlike C++, non-POD types in Swift are still required to be
  /// address-invariant, so a value can always be "moved" from place
  /// to place with a memcpy.
  bool isPOD() const { return !(Data & IsNonPOD); }
  constexpr ValueWitnessFlags withPOD(bool isPOD) const {
    return ValueWitnessFlags((Data & ~IsNonPOD) |
                               (isPOD ? 0 : IsNonPOD));
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
/// Guaranteed to be equivalent to a memcpy of self->size bytes.
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

  
/// Given an initialized object, return the metadata pointer for its dynamic
/// type.
///
/// Preconditions:
///   'src' is an initialized object
typedef const Metadata *typeOf(OpaqueValue *src,
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
  MACRO(typeOf)

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
};

// Standard value-witness tables.

// The "Int" tables are used for arbitrary POD data with the matching
// size/alignment characteristics.
extern "C" const ValueWitnessTable _TWVBi8_;      // Builtin.Int8
extern "C" const ValueWitnessTable _TWVBi16_;     // Builtin.Int16
extern "C" const ValueWitnessTable _TWVBi32_;     // Builtin.Int32
extern "C" const ValueWitnessTable _TWVBi64_;     // Builtin.Int64

// The object-pointer table can be used for arbitrary Swift refcounted
// pointer types.
extern "C" const ValueWitnessTable _TWVBo;        // Builtin.ObjectPointer

// The ObjC-pointer table can be used for arbitrary ObjC pointer types.
extern "C" const ValueWitnessTable _TWVBO;        // Builtin.ObjCPointer

// The () -> () table can be used for arbitrary function types.
extern "C" const ValueWitnessTable _TWVFT_T_;     // () -> ()

// The () table can be used for arbitrary empty types.
extern "C" const ValueWitnessTable _TWVT_;        // ()
  
// The Bool value witness table.
// FIXME: The compiler should generate this.
extern "C" const ValueWitnessTable _TWVSb;        // swift.Bool

/// Return the value witnesses for unmanaged pointers.
static inline const ValueWitnessTable &getUnmanagedPointerValueWitnesses() {
#ifdef __LP64__
  return _TWVBi64_;
#else
  return _TWVBi32_;
#endif
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

  /// Is this metadata for a class type?
  bool isClassType() const {
    return Kind > MetadataKind::MetadataKind_Last
      || Kind == MetadataKind::Class;
  }

  const ValueWitnessTable *getValueWitnesses() const {
    return asFullMetadata(this)->ValueWitnesses;
  }

  void setValueWitnesses(const ValueWitnessTable *table) {
    asFullMetadata(this)->ValueWitnesses = table;
  }

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
extern "C" const FullOpaqueMetadata _TMdBo;        // Builtin.ObjectPointer
extern "C" const FullOpaqueMetadata _TMdBO;        // Builtin.ObjCPointer
  
// FIXME: The compiler should generate this.
extern "C" const FullOpaqueMetadata _TMdSb;        // swift.Bool

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

/// Information about a nominal type.  Not described for now.
struct NominalTypeDescriptor;

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
                          uintptr_t data)
    : HeapMetadata(base), SuperClass(superClass),
      CacheData{ nullptr, nullptr }, Data(data) {}

  /// The metadata for the super class.  This is null for the root class.
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

  // After this come the class members, laid out as follows:
  //   - class members for the base class (recursively)
  //   - metadata reference for the parent, if applicable
  //   - generic parameters for this class
  //   - class variables (if we choose to support these)
  //   - "tabulated" virtual methods
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

/// The structure of metadata for heap-allocated arrays.
/// This is non-type metadata.
///
/// The comments on HeapLocalVariableMetadata about tools wanting type
/// discovery apply equally here.
struct HeapArrayMetadata : public HeapMetadata {
  // No extra fields for now.
};

/// The structure of wrapper metadata for Objective-C classes.  This
/// is used as a type metadata pointer when the actual class isn't
/// Swift-compiled.
struct ObjCClassWrapperMetadata : public Metadata {
  const ClassMetadata *Class;
};

/// The structure of type metadata for structs.
struct StructMetadata : public Metadata {
  /// An out-of-line description of the type.
  const NominalTypeDescriptor *Description;

  /// The parent type of this member type, or null if this is not a
  /// member type.
  const Metadata *Parent;

  // This is followed by the generics information, if this type is generic.
};

/// The structure of function type metadata.
struct FunctionTypeMetadata : public Metadata {
  /// The type metadata for the argument type.
  const Metadata *ArgumentType;

  /// The type metadata for the result type.
  const Metadata *ResultType;
};

/// The structure of metadata for metatypes.
struct MetatypeMetadata : public Metadata {
  /// The type metadata for the element.
  const Metadata *InstanceType;
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
};

/// The standard metadata for the empty tuple type.
extern "C" const FullMetadata<TupleTypeMetadata> _TMdT_;

/// \brief The header in front of a generic metadata template.
///
/// This is optimized so that the code generation pattern
/// requires the minimal number of independent arguments.
/// For example, we want to be able to allocate a generic class
/// Dictionary<T,U> like so:
///   extern GenericMetadata Dictionary_metadata_header;
///   void *arguments[] = { typeid(T), typeid(U) };
///   void *metadata = swift_fetchGenericMetadata(&Dictionary_metadata_header,
///                                               &arguments);
///   void *object = swift_allocObject(metadata);
///
/// Note that the metadata header is *not* const data; it includes 8
/// pointers worth of implementation-private data.
///
/// Both the metadata header and the arguments buffer are guaranteed
/// to be pointer-aligned.
struct GenericMetadata {
  /// The number of generic arguments that we need to unique on,
  /// in words.  The first 'NumArguments * sizeof(void*)' bytes of
  /// the arguments buffer are the key.
  uint32_t NumArguments;

  /// The number of fill operations following this header.
  /// See the 
  uint32_t NumFillOps;

  /// The size of the template in bytes.
  uint32_t MetadataSize;

  /// The offset of the address point in the template in bytes.
  uint32_t AddressPoint;

  /// Data that the runtime can use for its own purposes.  It is guaranteed
  /// to be zero-filled by the compiler.
  void *PrivateData[8];

  // Here there is a variably-sized field:
  // GenericMetadataFillOp FillOps[NumArguments];

  // Here there is a variably-sized field:
  // char MetadataTemplate[MetadataSize];

  /// A heap-metadata fill operation is an instruction to copy a
  /// pointer's worth of data from the arguments into a particular
  /// position in the allocated metadata.
  ///
  /// 'ToIndex' is expressed relative to the start of the full
  /// metadata, not relative to the address point.
  struct FillOp {
    uint32_t FromIndex;
    int32_t ToIndex;
  };

  typedef const FillOp *fill_ops_const_iterator;
  fill_ops_const_iterator fill_ops_begin() const {
    return reinterpret_cast<const FillOp *>(this + 1);
  }
  fill_ops_const_iterator fill_ops_end() const {
    return fill_ops_begin() + NumFillOps;
  }

  /// Return the starting address of the metadata template data.
  const void *getMetadataTemplate() const {
    return fill_ops_end();
  }
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

/// \brief Fetch a uniqued metadata for a function type.
extern "C" const FunctionTypeMetadata *
swift_getFunctionTypeMetadata(const Metadata *argMetadata,
                              const Metadata *resultMetadata);

/// \brief Fetch a uniqued type metadata for an ObjC class.
extern "C" const Metadata *
swift_getObjCClassMetadata(const ClassMetadata *theClass);

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


/// \brief Fetch a uniqued metadata for a metatype type.
extern "C" const MetatypeMetadata *
swift_getMetatypeMetadata(const Metadata *instanceType);

/// \brief Checked dynamic cast to a class type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
extern "C" const void *
swift_dynamicCastClass(const void *object, const ClassMetadata *targetType);

/// \brief Unconditional, checked dynamic cast to a class type.
///
/// Aborts if the object isn't of the target type.
///
/// \param object The object to cast.
/// \param targetType The type to which we are casting, which is known to be
/// a class type.
///
/// \returns the object.
extern "C" const void *
swift_dynamicCastClassUnconditional(const void *object,
                                    const ClassMetadata *targetType);

/// \brief Checked Objective-C-style dynamic cast to a class type.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a class type.
///
/// \returns the object if the cast succeeds, or null otherwise.
extern "C" const void *
swift_dynamicCastObjCClass(const void *object, const ClassMetadata *targetType);

/// \brief Unconditional, checked, Objective-C-style dynamic cast to a class
/// type.
///
/// Aborts if the object isn't of the target type.
/// Note that unlike swift_dynamicCastClassUnconditional, this does not abort
/// if the object is 'nil'.
///
/// \param object The object to cast, or nil.
/// \param targetType The type to which we are casting, which is known to be
/// a class type.
///
/// \returns the object.
extern "C" const void *
swift_dynamicCastObjCClassUnconditional(const void *object,
                                        const ClassMetadata *targetType);

/// \brief Checked dynamic cast to the given type.
///
/// \param object The object to cast.
///
/// \param targetType The type to which we are casting, which may be either a
/// class type or a wrapped Objective-C class type.
///
/// \returns the object, or null if it doesn't have the given target type.
extern "C" const void *
swift_dynamicCast(const void *object, const Metadata *targetType);

/// \brief Unconditional checked dynamic cast to the given type.
///
/// Aborts if the object isn't of the target type.
///
/// \param object The object to cast.
///
/// \param targetType The type to which we are casting, which may be either a
/// class type or a wrapped Objective-C class type.
///
/// \returns the object.
extern "C" const void *
swift_dynamicCastUnconditional(const void *object,
                               const Metadata *targetType);

/// \brief Standard 'typeof' value witness for types with static metatypes.
///
/// \param obj  A pointer to the object. Ignored.
/// \param self The type metadata for the object.
///
/// \returns self.
extern "C" const Metadata *
swift_staticTypeof(OpaqueValue *obj, const Metadata *self);

/// \brief Standard 'typeof' value witness for heap object references.
///
/// \param obj  A pointer to the object reference.
/// \param self The static type metadata for the object. Ignored.
///
/// \returns The dynamic type metadata for the object.
extern "C" const Metadata *
swift_objectTypeof(OpaqueValue *obj, const Metadata *self);

/// \brief Standard 'typeof' value witness for ObjC object references.
///
/// \param obj  A pointer to the object reference.
/// \param self The static type metadata for the object. Ignored.
///
/// \returns The dynamic type metadata for the object.
extern "C" const Metadata *
swift_objcTypeof(OpaqueValue *obj, const Metadata *self);
  
} // end namespace swift

#endif /* SWIFT_RUNTIME_METADATA_H */
