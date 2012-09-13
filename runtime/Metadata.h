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

#ifndef SWIFT_ABI_METADATA_H
#define SWIFT_ABI_METADATA_H

#include <cstddef>
#include <cstdint>

struct SwiftHeapMetadata;

namespace swift {

struct HeapObject;

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
typedef void destroyBuffer(ValueBuffer *buffer, ValueWitnessTable *self);

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
                                                      ValueWitnessTable *self);

/// Given an allocated or initialized buffer, derive a pointer to
/// the object.
/// 
/// Invariants:
///   'buffer' is an allocated or initialized buffer
typedef OpaqueValue *projectBuffer(ValueBuffer *buffer,
                                   ValueWitnessTable *self);

/// Given an allocated buffer, deallocate the object.
///
/// Preconditions:
///   'buffer' is an allocated buffer
/// Postconditions:
///   'buffer' is an unallocated buffer
typedef void deallocateBuffer(ValueBuffer *buffer,
                              ValueWitnessTable *self);

/// Given an initialized object, destroy it.
///
/// Preconditions:
///   'object' is an initialized object
/// Postconditions:
///   'object' is an uninitialized object
typedef void destroy(OpaqueValue *object,
                     ValueWitnessTable *self);

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
                                              ValueWitnessTable *self);

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
                                        ValueWitnessTable *self);

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
                                    ValueWitnessTable *self);

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
                                              ValueWitnessTable *self);

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
                                        ValueWitnessTable *self);

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
                                    ValueWitnessTable *self);

/// Given an uninitialized buffer, allocate an object.
///
/// Returns the uninitialized object.
///
/// Preconditions:
///   'buffer' is an uninitialized buffer
/// Postconditions:
///   'buffer' is an allocated buffer
typedef OpaqueValue *allocateBuffer(ValueBuffer *buffer,
                                    ValueWitnessTable *self);

/// The number of bytes required to store an object of this type.
/// This value may be zero.  This value is not necessarily a
/// multiple of the alignment.
typedef size_t size;

/// The required alignment for the first byte of an object of this type.
typedef size_t alignment;

/// When allocating an array of objects of this type, the number of bytes
/// between array elements.  This value may be zero.  This value is always
/// a multiple of the alignment.
typedef size_t stride;

} // end namespace value_witness_types

/// A value-witness table.  A value witness table is built around
/// the requirements of some specific type.  The information in
/// a value-witness table is intended to be sufficient to lay out
/// and manipulate values of an arbitrary type.
struct ValueWitnessTable {
  value_witness_types::destroyBuffer *destroyBuffer;
  value_witness_types::initializeBufferWithCopyOfBuffer *
    initializeBufferWithCopyOfBuffer;
  value_witness_types::projectBuffer *projectBuffer;
  value_witness_types::deallocateBuffer *deallocateBuffer;
  value_witness_types::destroy *destroy;
  value_witness_types::initializeBufferWithCopy *initializeBufferWithCopy;
  value_witness_types::initializeWithCopy *initializeWithCopy;
  value_witness_types::assignWithCopy *assignWithCopy;
  value_witness_types::initializeBufferWithTake *initializeBufferWithTake;
  value_witness_types::initializeWithTake *initializeWithTake;
  value_witness_types::assignWithTake *assignWithTake;
  value_witness_types::allocateBuffer *allocateBuffer;
  value_witness_types::size size;
  value_witness_types::alignment alignment;
  value_witness_types::stride stride;
};

// Standard POD value-witness tables.
extern "C" ValueWitnessTable _TWVBi8_;      // Builtin.Int8
extern "C" ValueWitnessTable _TWVBi16_;     // Builtin.Int16
extern "C" ValueWitnessTable _TWVBi32_;     // Builtin.Int32
extern "C" ValueWitnessTable _TWVBi64_;     // Builtin.Int64
extern "C" ValueWitnessTable _TWVBo;        // Builtin.ObjectPointer
extern "C" ValueWitnessTable _TWVBO;        // Builtin.ObjCPointer

enum : uint8_t { GenericTypeFlag = 0x80 };

/// Kinds of Swift metadata records.  Some of these are types, some
/// aren't.
enum class MetadataKind : uint8_t {
  /// A class type.
  Class         = 0,
  GenericClass  = unsigned(Class)  | GenericTypeFlag,

  /// A struct type.
  Struct        = 1,
  GenericStruct = unsigned(Struct) | GenericTypeFlag,

  /// A oneof type.
  /// If we add reference oneofs, that needs to go here.
  Oneof         = 2,
  GenericOneof  = unsigned(Oneof)  | GenericTypeFlag,

  /// A type whose value is not exposed in the metadata system.
  Opaque        = 8,

  /// A tuple.
  Tuple         = 9,

  /// A monomorphic function.
  Function      = 10,

  /// A polymorphic function.
  PolyFunction  = 11,

  /// An existential type.
  Existential   = 12

  // Array types?
  // L-value types?
};

/// The common structure of all type metadata.
struct Metadata {
  /// The kind.
  MetadataKind Kind;

  // The rest of the first pointer-sized storage unit is reserved.

  /// A pointer to the value-witnesses for this type.
  ValueWitnessTable *ValueWitnesses;
};

/// The common structure of all metadata for heap-allocated types.
struct HeapMetadata { // FIXME: make subclass of Metadata
  /// Returns the allocated size of the object, or 0 if the object
  /// shouldn't be deallocated.
  size_t (*destroy)(HeapObject *);

  /// Returns the allocated size of the object.
  size_t (*getSize)(HeapObject *);
};

/// The descriptor for a nominal type.  This should be sharable
/// between generic instantiations.
struct NominalTypeDescriptor {
  /// The number of generic arguments.
  uint32_t NumGenericArguments;

  /// The offset, in bytes, to the first generic argument
  /// relative to the address of the metadata.
  uint32_t GenericArgumentsOffset;

  // Name
  // Component descriptor.
};

/// The structure of all class metadata.  This structure
/// is embedded directly within the class's heap metadata
/// structure and therefore cannot be extended.
struct ClassMetadata : public HeapMetadata {
  /// An out-of-line description of the type.
  NominalTypeDescriptor *Description;

  /// The metadata for the parent class.  This is null for the root class.
  ClassMetadata *ParentClass;
};

/// \brief The header in front of a generic metadata template.
///
/// This is optimized so that the code generation pattern
/// requires the minimal number of independent arguments.
/// For example, we want to be able to allocate a generic class
/// Dictionary<T,U> like so:
///   extern GenericHeapMetadata Dictionary_metadata_header;
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
struct GenericHeapMetadata {
  /// The number of generic arguments that we need to unique on,
  /// in words.  The first 'NumArguments * sizeof(void*)' bytes of
  /// the arguments buffer are the key.
  uint32_t NumArguments;

  /// The number of fill operations following this header.
  /// See the 
  uint32_t NumFillOps;

  /// The size of the template in bytes.
  size_t MetadataSize;

  /// Data that the runtime can use for its own purposes.  It is guaranteed
  /// to be zero-filled by the compiler.
  void *PrivateData[8];

  // Here there is a variably-sized field:
  // GenericHeapMetadataFillOp FillOps[NumArguments];

  // Here there is a variably-sized field:
  // char MetadataTemplate[MetadataSize];

  /// A heap-metadata fill operation is an instruction to copy a
  /// pointer's worth of data from the arguments into a particular
  /// position in the allocated metadata.
  struct FillOp {
    uint32_t FromIndex;
    uint32_t ToIndex;
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

/// \brief Fetch a uniqued metadata object for a class.
///
/// The basic algorithm for fetching a metadata object is:
///   func swift_fetchGenericMetadata(header, arguments) {
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
extern "C" const SwiftHeapMetadata *
swift_getGenericMetadata(GenericHeapMetadata *pattern,
                         const void *arguments);

} // end namespace swift

#endif /* SWIFT_ABI_METADATA_H */
