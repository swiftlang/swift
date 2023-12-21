//===--- SwiftRemoteMirror.h - Public remote reflection interf. -*- C++ -*-===//
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
/// This header declares functions in the libswiftReflection library,
/// which provides mechanisms for reflecting heap information in a
/// remote Swift process.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MIRROR_H
#define SWIFT_REMOTE_MIRROR_H

#include "Platform.h"
#include "MemoryReaderInterface.h"
#include "SwiftRemoteMirrorTypes.h"

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

SWIFT_REMOTE_MIRROR_LINKAGE
#if !defined(_WIN32)
__attribute__((__weak_import__))
#endif
extern unsigned long long swift_reflection_classIsSwiftMask;

/// An arbitrary version number for this library. Incremented to indicate the
/// presence of a bug fix or feature that can't be detected from the outside
/// otherwise. The currently used version numbers are:
///
/// 0 - Indicates that swift_reflection_iterateAsyncTaskAllocations has the
///     first attempted fix to use the right AsyncTask layout.
/// 1 - Indicates that swift_reflection_iterateAsyncTaskAllocations has been
///     actually fixed to use the right AsyncTask layout.
/// 2 - swift_reflection_iterateAsyncTaskAllocations has been replaced by
///     swift_reflection_asyncTaskSlabPointer and
///     swift_reflection_asyncTaskSlabAllocations.
/// 3 - The async task slab size calculation is fixed to account for alignment,
///     no longer short by 8 bytes.
SWIFT_REMOTE_MIRROR_LINKAGE extern uint32_t swift_reflection_libraryVersion;

/// Get the metadata version supported by the Remote Mirror library.
SWIFT_REMOTE_MIRROR_LINKAGE
uint16_t swift_reflection_getSupportedMetadataVersion(void);

/// \returns An opaque reflection context.
SWIFT_REMOTE_MIRROR_LINKAGE
SwiftReflectionContextRef
swift_reflection_createReflectionContext(void *ReaderContext,
                                         uint8_t PointerSize,
                                         FreeBytesFunction Free,
                                         ReadBytesFunction ReadBytes,
                                         GetStringLengthFunction GetStringLength,
                                         GetSymbolAddressFunction GetSymbolAddress);

/// \returns An opaque reflection context.
SWIFT_REMOTE_MIRROR_LINKAGE
SwiftReflectionContextRef
swift_reflection_createReflectionContextWithDataLayout(void *ReaderContext,
                                     QueryDataLayoutFunction DataLayout,
                                     FreeBytesFunction Free,
                                     ReadBytesFunction ReadBytes,
                                     GetStringLengthFunction GetStringLength,
                                     GetSymbolAddressFunction GetSymbolAddress);

/// Destroys an opaque reflection context.
SWIFT_REMOTE_MIRROR_LINKAGE
void
swift_reflection_destroyReflectionContext(SwiftReflectionContextRef Context);

/// DEPRECATED. Add reflection sections for a loaded Swift image.
///
/// You probably want to use \c swift_reflection_addImage instead.
SWIFT_REMOTE_MIRROR_LINKAGE
void
swift_reflection_addReflectionInfo(SwiftReflectionContextRef ContextRef,
                                   swift_reflection_info_t Info);

/// Add reflection sections from a loaded Swift image.
SWIFT_REMOTE_MIRROR_LINKAGE
void swift_reflection_addReflectionMappingInfo(
    SwiftReflectionContextRef ContextRef, swift_reflection_mapping_info_t Info);

/// Add reflection information from a loaded Swift image.
/// Returns true on success, false if the image's memory couldn't be read.
SWIFT_REMOTE_MIRROR_LINKAGE
int
swift_reflection_addImage(SwiftReflectionContextRef ContextRef,
                          swift_addr_t imageStart);

/// Returns a boolean indicating if the isa mask was successfully
/// read, in which case it is stored in the isaMask out parameter.
SWIFT_REMOTE_MIRROR_LINKAGE
int swift_reflection_readIsaMask(SwiftReflectionContextRef ContextRef,
                                 uintptr_t *outIsaMask);

/// Returns an opaque type reference for a metadata pointer, or
/// NULL if one can't be constructed.
///
/// This function loses information; in particular, passing the
/// result to swift_reflection_infoForTypeRef() will not give
/// the same result as calling swift_reflection_infoForMetadata().
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeref_t
swift_reflection_typeRefForMetadata(SwiftReflectionContextRef ContextRef,
                                    uintptr_t Metadata);

/// Returns whether the given object appears to have metadata understood
/// by this library. Images must be added using
/// swift_reflection_addImage, not swift_reflection_addReflectionInfo,
/// for this function to work properly. If addImage is used, a negative
/// result is always correct, but a positive result may be a false
/// positive if the address in question is not really a Swift or
/// Objective-C object. If addReflectionInfo is used, the return value
/// will always be false.
SWIFT_REMOTE_MIRROR_LINKAGE
int
swift_reflection_ownsObject(SwiftReflectionContextRef ContextRef, uintptr_t Object);

/// Returns whether the given address is associated with an image added to this
/// library. Images must be added using swift_reflection_addImage, not
/// swift_reflection_addReflectionInfo, for this function to work
/// properly. If addReflectionInfo is used, the return value will always
/// be false.
SWIFT_REMOTE_MIRROR_LINKAGE
int
swift_reflection_ownsAddress(SwiftReflectionContextRef ContextRef, uintptr_t Address);

/// Returns whether the given address is strictly in the DATA, AUTH or TEXT
/// segments of the image added to this library. Images must be added using
/// swift_reflection_addImage, not swift_reflection_addReflectionInfo, for this
/// function to work properly. If addReflectionInfo is used, the return value
/// will always be false.
SWIFT_REMOTE_MIRROR_LINKAGE
int
swift_reflection_ownsAddressStrict(SwiftReflectionContextRef ContextRef, uintptr_t Address);

/// Returns the metadata pointer for a given object.
SWIFT_REMOTE_MIRROR_LINKAGE
uintptr_t
swift_reflection_metadataForObject(SwiftReflectionContextRef ContextRef,
                                   uintptr_t Object);

/// Returns the nominal type descriptor given the metadata
SWIFT_REMOTE_MIRROR_LINKAGE
swift_reflection_ptr_t
swift_reflection_metadataNominalTypeDescriptor(SwiftReflectionContextRef ContextRef,
																							 swift_reflection_ptr_t Metadata);


SWIFT_REMOTE_MIRROR_LINKAGE
int
swift_reflection_metadataIsActor(SwiftReflectionContextRef ContextRef,
                                 swift_reflection_ptr_t Metadata);

/// Returns an opaque type reference for a class or closure context
/// instance pointer, or NULL if one can't be constructed.
///
/// This function loses information; in particular, passing the
/// result to swift_reflection_infoForTypeRef() will not give
/// the same result as calling swift_reflection_infoForInstance().
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeref_t
swift_reflection_typeRefForInstance(SwiftReflectionContextRef ContextRef,
                                    uintptr_t Object);

/// Returns a typeref from a mangled type name string.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeref_t
swift_reflection_typeRefForMangledTypeName(SwiftReflectionContextRef ContextRef,
                                           const char *MangledName,
                                           uint64_t Length);

/// Returns the demangled name for a typeref, or NULL if the name couldn't be
/// created.
///
/// The returned string is heap allocated and the caller must free() it when
/// done.
SWIFT_REMOTE_MIRROR_DEPRECATED(
    "Please use swift_reflection_copyNameForTypeRef()",
    "swift_reflection_copyNameForTypeRef")
SWIFT_REMOTE_MIRROR_LINKAGE
char *
swift_reflection_copyDemangledNameForTypeRef(
  SwiftReflectionContextRef ContextRef, swift_typeref_t OpaqueTypeRef);

SWIFT_REMOTE_MIRROR_LINKAGE
char *
swift_reflection_copyDemangledNameForProtocolDescriptor(
  SwiftReflectionContextRef ContextRef, swift_reflection_ptr_t Proto);

/// Returns the mangled or demangled name for a typeref, or NULL if the name
/// couldn't be created.
///
/// The returned string is heap allocated and the caller must free() it when
/// done.

SWIFT_REMOTE_MIRROR_LINKAGE
char*
swift_reflection_copyNameForTypeRef(SwiftReflectionContextRef ContextRef,
                                    swift_typeref_t OpaqueTypeRef,
                                    bool mangled);

/// Returns a structure describing the layout of a value of a typeref.
/// For classes, this returns the reference value itself.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeinfo_t
swift_reflection_infoForTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef);

/// Returns the information about a child field of a value by index.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_childinfo_t
swift_reflection_childOfTypeRef(SwiftReflectionContextRef ContextRef,
                                swift_typeref_t OpaqueTypeRef, unsigned Index);

/// Returns a structure describing the layout of a class instance
/// from the isa pointer of a class.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeinfo_t
swift_reflection_infoForMetadata(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Metadata);

/// Returns the information about a child field of a class instance
/// by index.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_childinfo_t
swift_reflection_childOfMetadata(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Metadata, unsigned Index);

/// Returns a structure describing the layout of a class or closure
/// context instance, from a pointer to the object itself.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeinfo_t
swift_reflection_infoForInstance(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Object);

/// Returns the information about a child field of a class or closure
/// context instance.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_childinfo_t
swift_reflection_childOfInstance(SwiftReflectionContextRef ContextRef,
                                 uintptr_t Object, unsigned Index);

/// Returns the number of generic arguments of a typeref.
SWIFT_REMOTE_MIRROR_LINKAGE
unsigned
swift_reflection_genericArgumentCountOfTypeRef(swift_typeref_t OpaqueTypeRef);

/// Returns a fully instantiated typeref for a generic argument by index.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_typeref_t
swift_reflection_genericArgumentOfTypeRef(swift_typeref_t OpaqueTypeRef,
                                          unsigned Index);

/// Projects the type inside of an existential container.
///
/// Takes the address of the start of an existential container and the typeref
/// for the existential, and sets two out parameters:
///
/// - InstanceTypeRef: A type reference for the type inside of the existential
///   container.
/// - StartOfInstanceData: The address to the start of the inner type's instance
///   data, which may or may not be inside the container directly.
///   If the type is a reference type, the pointer to the instance is the first
///   word in the container.
///
///   If the existential contains a value type that can fit in the first three
///   spare words of the container, *StartOfInstanceData == InstanceAddress.
///   If it's a value type that can't fit in three words, the data will be in
///   its own heap box, so *StartOfInstanceData will be the address of that box.
///
/// Returns true if InstanceTypeRef and StartOfInstanceData contain valid
/// valid values.
SWIFT_REMOTE_MIRROR_LINKAGE
int swift_reflection_projectExistential(SwiftReflectionContextRef ContextRef,
                                        swift_addr_t ExistentialAddress,
                                        swift_typeref_t ExistentialTypeRef,
                                        swift_typeref_t *OutInstanceTypeRef,
                                        swift_addr_t *OutStartOfInstanceData);

/// Like swift_reflection_projectExistential, with 2 differences:
///
/// - When dealing with an error existential, this version will dereference 
///   the ExistentialAddress before proceeding.
/// - After setting OutInstanceTypeRef and OutStartOfInstanceData this version
///   may dereference and set OutStartOfInstanceData if OutInstanceTypeRef is a 
///   class TypeRef.
SWIFT_REMOTE_MIRROR_LINKAGE
int swift_reflection_projectExistentialAndUnwrapClass(
    SwiftReflectionContextRef ContextRef, swift_addr_t ExistentialAddress,
    swift_typeref_t ExistentialTypeRef, swift_typeref_t *OutInstanceTypeRef,
    swift_addr_t *OutStartOfInstanceData);

/// Projects the value of an enum.
///
/// Takes the address and typeref for an enum and determines the
/// index of the currently-selected case within the enum.
/// You can use this index with `swift_reflection_childOfTypeRef`
/// to get detailed information about the specific case.
///
/// Returns true if the enum case could be successfully determined.
/// In particular, note that this code may fail for valid in-memory data
/// if the compiler is using a strategy we do not yet understand.
SWIFT_REMOTE_MIRROR_LINKAGE
int swift_reflection_projectEnumValue(SwiftReflectionContextRef ContextRef,
                                      swift_addr_t EnumAddress,
                                      swift_typeref_t EnumTypeRef,
                                      int *CaseIndex);

/// Dump a brief description of the typeref as a tree to stderr.
SWIFT_REMOTE_MIRROR_LINKAGE
void swift_reflection_dumpTypeRef(swift_typeref_t OpaqueTypeRef);

/// Dump information about the layout for a typeref.
SWIFT_REMOTE_MIRROR_LINKAGE
void swift_reflection_dumpInfoForTypeRef(SwiftReflectionContextRef ContextRef,
                                         swift_typeref_t OpaqueTypeRef);

/// Dump information about the layout of a class instance from its isa pointer.
SWIFT_REMOTE_MIRROR_LINKAGE
void swift_reflection_dumpInfoForMetadata(SwiftReflectionContextRef ContextRef,
                                          uintptr_t Metadata);

/// Dump information about the layout of a class or closure context instance.
SWIFT_REMOTE_MIRROR_LINKAGE
void swift_reflection_dumpInfoForInstance(SwiftReflectionContextRef ContextRef,
                                          uintptr_t Object);

/// Demangle a type name.
///
/// Copies at most `MaxLength` bytes from the demangled name string into
/// `OutDemangledName`.
///
/// Returns the length of the demangled string this function tried to copy
/// into `OutDemangledName`.
SWIFT_REMOTE_MIRROR_LINKAGE
size_t swift_reflection_demangle(const char *MangledName, size_t Length,
                                 char *OutDemangledName, size_t MaxLength);

/// Iterate over the process's protocol conformance cache.
///
/// Calls the passed in Call function for each protocol conformance found in
/// the conformance cache. The function is passed the type which conforms and
/// the protocol it conforms to. The ContextPtr is passed through unchanged.
///
/// Returns NULL on success. On error, returns a pointer to a C string
/// describing the error. This pointer remains valid until the next
/// swift_reflection call on the given context.
SWIFT_REMOTE_MIRROR_LINKAGE
const char *swift_reflection_iterateConformanceCache(
  SwiftReflectionContextRef ContextRef,
  void (*Call)(swift_reflection_ptr_t Type,
               swift_reflection_ptr_t Proto,
               void *ContextPtr),
  void *ContextPtr);

/// Iterate over the process's metadata allocations.
///
/// Calls the passed in Call function for each metadata allocation. The function
/// is passed a structure that describes the allocation. The ContextPtr is
/// passed through unchanged.
///
/// Returns NULL on success. On error, returns a pointer to a C string
/// describing the error. This pointer remains valid until the next
/// swift_reflection call on the given context.
SWIFT_REMOTE_MIRROR_LINKAGE
const char *swift_reflection_iterateMetadataAllocations(
  SwiftReflectionContextRef ContextRef,
  void (*Call)(swift_metadata_allocation_t Allocation,
               void *ContextPtr),
  void *ContextPtr);

/// Given a metadata allocation, return the metadata it points to. Returns NULL
/// on failure. Despite the name, not all allocations point to metadata.
/// Currently, this will return a metadata only for allocations with tag
/// GenericMetadataCache. Support for additional tags may be added in the
/// future. The caller must gracefully handle failure.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_reflection_ptr_t swift_reflection_allocationMetadataPointer(
  SwiftReflectionContextRef ContextRef,
  swift_metadata_allocation_t Allocation);

/// Return the name of a metadata allocation tag, or NULL if the tag is unknown.
/// This pointer remains valid until the next swift_reflection call on the given
/// context.
SWIFT_REMOTE_MIRROR_LINKAGE
const char *
swift_reflection_metadataAllocationTagName(SwiftReflectionContextRef ContextRef,
                                           swift_metadata_allocation_tag_t Tag);

SWIFT_REMOTE_MIRROR_LINKAGE
int swift_reflection_metadataAllocationCacheNode(
    SwiftReflectionContextRef ContextRef,
    swift_metadata_allocation_t Allocation,
    swift_metadata_cache_node_t *OutNode);

/// Backtrace iterator callback passed to
/// swift_reflection_iterateMetadataAllocationBacktraces
typedef void (*swift_metadataAllocationBacktraceIterator)(
    swift_reflection_ptr_t AllocationPtr, size_t Count,
    const swift_reflection_ptr_t Ptrs[], void *ContextPtr);

/// Iterate over all recorded metadata allocation backtraces in the process.
///
/// Calls the passed in Call function for each recorded backtrace. The function
/// is passed the number of backtrace entries and an array of those entries, as
/// pointers. The array is stored from deepest to shallowest, so main() will be
/// somewhere near the end. This array is valid only for the duration of the
/// call.
///
/// Returns NULL on success. On error, returns a pointer to a C string
/// describing the error. This pointer remains valid until the next
/// swift_reflection call on the given context.
SWIFT_REMOTE_MIRROR_LINKAGE
const char *swift_reflection_iterateMetadataAllocationBacktraces(
    SwiftReflectionContextRef ContextRef,
    swift_metadataAllocationBacktraceIterator Call, void *ContextPtr);

/// Get the first allocation slab for a given async task object.
/// This object must have an isa value equal to
/// _swift_concurrency_debug_asyncTaskMetadata.
///
/// It is possible that the async task object hasn't allocated a slab yet, in
/// which case the slab pointer will be NULL. If non-NULL, the returned slab
/// pointer may be a separate heap allocation, or it may be interior to some
/// allocation used by the task.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_async_task_slab_return_t
swift_reflection_asyncTaskSlabPointer(SwiftReflectionContextRef ContextRef,
                                      swift_reflection_ptr_t AsyncTaskPtr);

/// Iterate over the allocations in the given async task allocator slab.
/// This allocation must have an "isa" value (scare quotes because it's not a
/// real object) equal to _swift_concurrency_debug_asyncTaskSlabMetadata.
///
/// Calls the passed in Call function for each allocation in the slab. The
/// function is passed the allocation pointer and an array of chunks. Each chunk
/// consists of a start, length, and kind for that chunk of the allocated
/// memory. Any regions of the allocation that are not covered by a chunk are
/// unallocated or garbage. The chunk array is valid only for the duration of
/// the call.
///
/// A slab may be part of a chain of slabs, so the
/// function may be called more than once.
///
/// Returns NULL on success. On error, returns a pointer to a C string
/// describing the error. This pointer remains valid until the next
/// swift_reflection call on the given context.
SWIFT_REMOTE_MIRROR_LINKAGE
swift_async_task_slab_allocations_return_t
swift_reflection_asyncTaskSlabAllocations(SwiftReflectionContextRef ContextRef,
                                          swift_reflection_ptr_t SlabPtr);

SWIFT_REMOTE_MIRROR_LINKAGE
swift_async_task_info_t
swift_reflection_asyncTaskInfo(SwiftReflectionContextRef ContextRef,
                               swift_reflection_ptr_t AsyncTaskPtr);

SWIFT_REMOTE_MIRROR_LINKAGE
swift_actor_info_t
swift_reflection_actorInfo(SwiftReflectionContextRef ContextRef,
                           swift_reflection_ptr_t ActorPtr);

SWIFT_REMOTE_MIRROR_LINKAGE
swift_reflection_ptr_t
swift_reflection_nextJob(SwiftReflectionContextRef ContextRef,
                         swift_reflection_ptr_t JobPtr);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_REFLECTION_SWIFT_REFLECTION_H

