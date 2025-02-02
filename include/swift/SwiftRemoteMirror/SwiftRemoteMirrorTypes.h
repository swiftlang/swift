//===--- SwiftRemoteMirrorTypes.h - Remote reflection types -----*- C++ -*-===//
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
///
/// \file
/// This header declares types in the libswiftReflection library.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MIRROR_TYPES_H
#define SWIFT_REMOTE_MIRROR_TYPES_H

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Pointers used here need to be pointer-sized on watchOS for binary
// compatibility. Everywhere else, they are 64-bit so 32-bit processes can
// potentially read from 64-bit processes.
#if defined(__APPLE__) && defined(__MACH__)
#include <TargetConditionals.h>
#if TARGET_OS_WATCH
#define SWIFT_REFLECTION_NATIVE_POINTERS 1
#endif
#endif

#if SWIFT_REFLECTION_NATIVE_POINTERS
typedef uintptr_t swift_reflection_ptr_t;
#else
typedef uint64_t swift_reflection_ptr_t;
#endif

typedef swift_reflection_ptr_t swift_typeref_t;

/// Represents one of the Swift reflection sections of an image.
typedef struct swift_reflection_section {
  void *Begin;
  void *End;
} swift_reflection_section_t;

/// Represents the remote address and size of an image's section
typedef struct swift_remote_reflection_section {
    uintptr_t StartAddress;
    uintptr_t Size;
} swift_remote_reflection_section_t;

typedef struct swift_reflection_section_pair {
  swift_reflection_section_t section;
  swift_reflection_ptr_t offset; ///< DEPRECATED. Must be zero
} swift_reflection_section_pair_t;

/// Represents the mapping between an image sections's local and remote addresses
typedef struct swift_reflection_section_mapping {
  swift_reflection_section_t local_section;
  swift_remote_reflection_section_t remote_section;
} swift_reflection_section_mapping_t;

/// Represents the set of Swift reflection sections of an image.
/// Not all sections may be present.
///
/// DEPRECATED. New RemoteMirror clients should use
/// \c swift_reflection_addImage .
typedef struct swift_reflection_info {
  swift_reflection_section_pair_t field;
  swift_reflection_section_pair_t associated_types;
  swift_reflection_section_pair_t builtin_types;
  swift_reflection_section_pair_t capture;
  swift_reflection_section_pair_t type_references;
  swift_reflection_section_pair_t reflection_strings;

  // Start address in local and remote address spaces.
  swift_reflection_ptr_t LocalStartAddress;
  swift_reflection_ptr_t RemoteStartAddress;
} swift_reflection_info_t;

/// Represents the set of Swift reflection sections of an image,
typedef struct swift_reflection_mapping_info {
  swift_reflection_section_mapping_t field;
  swift_reflection_section_mapping_t associated_types;
  swift_reflection_section_mapping_t builtin_types;
  swift_reflection_section_mapping_t capture;
  swift_reflection_section_mapping_t type_references;
  swift_reflection_section_mapping_t reflection_strings;
  // New fields cannot be added here without breaking ABI.
  // Use swift_reflection_addImage instead.
} swift_reflection_mapping_info_t;

/// The layout kind of a Swift type.
typedef enum swift_layout_kind {
  // Nothing is known about the size or contents of this value.
  SWIFT_UNKNOWN,

  // An opaque value with known size and alignment but no specific
  // interpretation.
  SWIFT_BUILTIN,

  // A pointer-size value that is known to contain the address of
  // another heap allocation, or NULL.
  SWIFT_RAW_POINTER,

  // Value types consisting of zero or more fields.
  SWIFT_TUPLE,
  SWIFT_STRUCT,

  // An enum with no payload cases. The record will have no fields, but
  // will have the correct size.
  SWIFT_NO_PAYLOAD_ENUM,

  // An enum with a single payload case. The record consists of a single
  // field, being the enum payload.
  SWIFT_SINGLE_PAYLOAD_ENUM,

  // An enum with multiple payload cases. The record consists of a multiple
  // fields, one for each enum payload.
  SWIFT_MULTI_PAYLOAD_ENUM,

  SWIFT_THICK_FUNCTION,

  SWIFT_OPAQUE_EXISTENTIAL,
  SWIFT_CLASS_EXISTENTIAL,
  SWIFT_ERROR_EXISTENTIAL,
  SWIFT_EXISTENTIAL_METATYPE,

  // References to other objects in the heap.
  SWIFT_STRONG_REFERENCE,
  SWIFT_UNOWNED_REFERENCE,
  SWIFT_WEAK_REFERENCE,
  SWIFT_UNMANAGED_REFERENCE,

  // Layouts of heap objects. These are only ever returned from
  // swift_reflection_infoFor{Instance,Metadata}(), and not
  // swift_reflection_infoForTypeRef().
  SWIFT_CLASS_INSTANCE,
  SWIFT_CLOSURE_CONTEXT,

  // A contiguous list of N Ts, typically for Builtin.FixedArray<N, T>.
  SWIFT_ARRAY,
} swift_layout_kind_t;

struct swift_childinfo;

/// A description of the memory layout of a type or field of a type.
typedef struct swift_typeinfo {
  swift_layout_kind_t Kind;

  unsigned Size;
  unsigned Alignment;
  unsigned Stride;

  unsigned NumFields;
} swift_typeinfo_t;

typedef struct swift_childinfo {
  /// The memory for Name is owned by the reflection context.
  const char *Name;
  unsigned Offset;
  swift_layout_kind_t Kind;
  swift_typeref_t TR;
} swift_childinfo_t;

// Values here match the values from MetadataAllocatorTags in Metadata.h.
enum swift_metadata_allocation_tag {
  SWIFT_GENERIC_METADATA_CACHE_ALLOCATION = 14,
};

typedef int swift_metadata_allocation_tag_t;

/// A metadata allocation made by the Swift runtime.
typedef struct swift_metadata_allocation {
  /// The allocation's tag, which describes what kind of allocation it is. This
  /// may be one of the values in swift_metadata_allocation_tag, or something
  /// else, in which case the tag should be considered unknown.
  swift_metadata_allocation_tag_t Tag;

  /// A pointer to the start of the allocation in the remote process.
  swift_reflection_ptr_t Ptr;

  /// The size of the allocation in bytes.
  unsigned Size;
} swift_metadata_allocation_t;

typedef struct swift_metadata_cache_node {
  swift_reflection_ptr_t Left;
  swift_reflection_ptr_t Right;
} swift_metadata_cache_node_t;

/// The return value when getting an async task's slab pointer.
typedef struct swift_async_task_slab_return {
  /// On failure, a pointer to a string describing the error. On success, NULL.
  /// This pointer remains valid until the next
  /// swift_reflection call on the given context.
  const char *Error;

  /// The task's slab pointer, if no error occurred.
  swift_reflection_ptr_t SlabPtr;
} swift_async_task_slab_return_t;

typedef struct swift_async_task_allocation_chunk {
  swift_reflection_ptr_t Start;
  unsigned Length;
  swift_layout_kind_t Kind;
} swift_async_task_allocation_chunk_t;

typedef struct swift_async_task_slab_allocations_return {
  /// On failure, a pointer to a string describing the error. On success, NULL.
  /// This pointer remains valid until the next
  /// swift_reflection call on the given context.
  const char *Error;

  /// The remote pointer to the next slab, or NULL/0 if none.
  swift_reflection_ptr_t NextSlab;

  /// The size of the entire slab, in bytes.
  unsigned SlabSize;

  /// The number of chunks pointed to by Chunks.
  unsigned ChunkCount;

  /// A pointer to the chunks, if no error occurred.
  swift_async_task_allocation_chunk_t *Chunks;
} swift_async_task_slab_allocations_return_t;

typedef struct swift_async_task_info {
  /// On failure, a pointer to a string describing the error. On success, NULL.
  /// This pointer remains valid until the next
  /// swift_reflection call on the given context.
  const char *Error;

  unsigned Kind;
  unsigned EnqueuePriority;
  bool IsChildTask;
  bool IsFuture;
  bool IsGroupChildTask;
  bool IsAsyncLetTask;

  unsigned MaxPriority;
  bool IsCancelled;
  bool IsStatusRecordLocked;
  bool IsEscalated;
  bool HasIsRunning; // If false, the IsRunning flag is not valid.
  bool IsRunning;
  bool IsEnqueued;

  bool HasThreadPort;
  uint32_t ThreadPort;

  uint64_t Id;
  swift_reflection_ptr_t RunJob;
  swift_reflection_ptr_t AllocatorSlabPtr;

  unsigned ChildTaskCount;
  swift_reflection_ptr_t *ChildTasks;

  unsigned AsyncBacktraceFramesCount;
  swift_reflection_ptr_t *AsyncBacktraceFrames;
} swift_async_task_info_t;

typedef struct swift_actor_info {
  /// On failure, a pointer to a string describing the error. On success, NULL.
  /// This pointer remains valid until the next
  /// swift_reflection call on the given context.
  const char *Error;

  uint8_t State;
  bool IsDistributedRemote;
  bool IsPriorityEscalated;
  uint8_t MaxPriority;

  swift_reflection_ptr_t FirstJob;

  bool HasThreadPort;
  uint32_t ThreadPort;
} swift_actor_info_t;

/// An opaque pointer to a context which maintains state and
/// caching of reflection structure for heap instances.
typedef struct SwiftReflectionContext *SwiftReflectionContextRef;

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REMOTE_MIRROR_TYPES_H
