//===--- EmbeddedShims.h - shims for embedded Swift -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Shims for embedded Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H
#define SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H

#include "SwiftStddef.h"
#include "Visibility.h"

#if __has_feature(nullability)
#pragma clang assume_nonnull begin
#endif

#ifdef __cplusplus
extern "C" {
#endif

// TODO: __has_feature(swiftasynccc) is just for older clang. Remove this
// when we no longer support older clang.
#if __has_extension(swiftcc) || __has_feature(swiftasynccc)
#define SWIFT_CC_swift __attribute__((swiftcall))
#define SWIFT_CONTEXT __attribute__((swift_context))
#define SWIFT_ERROR_RESULT __attribute__((swift_error_result))
#define SWIFT_INDIRECT_RESULT __attribute__((swift_indirect_result))
#else
#define SWIFT_CC_swift
#define SWIFT_CONTEXT
#define SWIFT_ERROR_RESULT
#define SWIFT_INDIRECT_RESULT
#endif

typedef void SWIFT_CC_swift (*HeapObjectDestroyer)(SWIFT_CONTEXT void *object);

typedef struct EmbeddedHeapObject {
#if __has_feature(ptrauth_calls)
  void * __ptrauth(2, 1, 0x6ae1) metadata;
#else
  void *metadata;
#endif
} EmbeddedHeapObject;

static inline void
_swift_embedded_invoke_heap_object_destroy(void *object) {
  void *metadata = ((EmbeddedHeapObject *)object)->metadata;
  void **destroy_location = &((void **)metadata)[1];
#if __has_feature(ptrauth_calls)
  (*(HeapObjectDestroyer __ptrauth(0, 1, 0xbbbf) *)destroy_location)(object);
#else
  (*(HeapObjectDestroyer *)destroy_location)(object);
#endif
}

static inline void
_swift_embedded_invoke_heap_object_optional_ivardestroyer(void *object, void *metadata) {
  void **ivardestroyer_location = &((void **)metadata)[2];
  if (*ivardestroyer_location) {
#if __has_feature(ptrauth_calls)
    (*(HeapObjectDestroyer __ptrauth(0, 1, 0xbbbf) *)ivardestroyer_location)(object);
#else
    (*(HeapObjectDestroyer *)ivardestroyer_location)(object);
#endif
  }
}

static inline void *_swift_embedded_get_heap_object_metadata_pointer(void *object) {
  return ((EmbeddedHeapObject *)object)->metadata;
}

static inline void _swift_embedded_set_heap_object_metadata_pointer(void *object, void *metadata) {
  ((EmbeddedHeapObject *)object)->metadata = metadata;
}

typedef struct {
  void  *initializeBufferWithCopyOfBufferFn;
#if __has_feature(ptrauth_calls)
  void  (* __ptrauth(0, 1, 0x04f8)  destroyFn)(void *, void*);
#else
  void  (*destroyFn)(void *, void*);
#endif
#if __has_feature(ptrauth_calls)
  void* (* __ptrauth(0, 1, 0xe3ba) initializeWithCopyFn)(void*, void*, void*);
#else
  void* (*initializeWithCopyFn)(void*, void*, void*);
#endif
  void  *assignWithCopyFn;
#if __has_feature(ptrauth_calls)
  void* (* __ptrauth(0, 1, 0x48d8) initializeWithTakeFn)(void*, void*, void*);
#else
  void* (*initializeWithTakeFn)(void *, void*, void*);
#endif
  void  *assignWithTakeFn;
  void  *getEnumTagSinglePayloadFn;
  void  *storeEnumTagSinglePayload;
  __swift_size_t size;
  __swift_size_t stride;
  unsigned flags;
} EmbeddedValueWitnessTable;

typedef struct {
#if __has_feature(ptrauth_calls)
  EmbeddedValueWitnessTable * __ptrauth(2, 1, 0x2e3f) vwt;
#else
  EmbeddedValueWitnessTable *vwt;
#endif
} EmbeddedMetaDataPrefix;

typedef enum {
  AlignmentMask =                0x000000FF,
  IsNonInline =                  0x00020000,
} ValueWitnessTableFlags;

static inline
EmbeddedMetaDataPrefix *_swift_embedded_get_full_metadata(void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = (EmbeddedMetaDataPrefix*)&((void **)metadata)[-1];
  return fullmeta;
}

static inline __swift_size_t
_swift_embedded_metadata_get_size(void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  return fullmeta->vwt->size;
}

static inline __swift_size_t
_swift_embedded_metadata_get_align_mask_impl(EmbeddedMetaDataPrefix *fullMetadata) {
  unsigned flags =  fullMetadata->vwt->flags;
  ValueWitnessTableFlags alignMask = AlignmentMask;
  return flags & alignMask;
}

static inline __swift_size_t
_swift_embedded_metadata_get_align_mask(void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  return _swift_embedded_metadata_get_align_mask_impl(fullmeta);
}

static inline void *
_swift_embedded_box_project(void *object, EmbeddedMetaDataPrefix *fullmeta) {
  __swift_size_t alignMask = _swift_embedded_metadata_get_align_mask_impl(fullmeta);
  __swift_size_t headerSize = sizeof(void*) + sizeof(__swift_size_t);
  __swift_size_t startOfBoxedValue = (headerSize + alignMask) & ~alignMask;
  void *addrInBox = (void *)(((unsigned char *)object) + startOfBoxedValue);
  return addrInBox;
}
static inline void
_swift_embedded_invoke_box_destroy(void *object) {
  void *metadata = ((EmbeddedHeapObject *)object)->metadata;
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  void *addrInBox = _swift_embedded_box_project(object, fullmeta);
  fullmeta->vwt->destroyFn(addrInBox, metadata);
}

static inline void
_swift_embedded_initialize_box(void *metadata, void *newObjectAddr, void *oldObjectAddr) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  fullmeta->vwt->initializeWithCopyFn(newObjectAddr, oldObjectAddr, metadata);
}

typedef struct {
  void *inlineBuffer[3];
  void *metadata;
} ExistentialValue;

static inline void
_swift_embedded_existential_destroy(void *exist, void (*releaseBoxFn) (void *)) {
  ExistentialValue* existVal = (ExistentialValue*)exist;
  void *metadata = existVal->metadata;
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  ValueWitnessTableFlags isNonInlineMask = IsNonInline;
  if (fullmeta->vwt->flags & IsNonInline) {
    releaseBoxFn(existVal->inlineBuffer[0]);
  } else {
    fullmeta->vwt->destroyFn(&(existVal->inlineBuffer[0]), metadata);
  }
}

static inline void
_swift_embedded_existential_init_with_take(void *dst, void *srcExist,
                                           void (*releaseBoxFn) (void *)) {
  ExistentialValue* existVal = (ExistentialValue*)srcExist;
  void *metadata = existVal->metadata;
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  ValueWitnessTableFlags isNonInlineMask = IsNonInline;
  if (fullmeta->vwt->flags & IsNonInline) {
    void *addrInBox = _swift_embedded_box_project(existVal->inlineBuffer[0], fullmeta);
    // Need to call initWithCopy (instead of initWithTake) so that we can call
    // swift_releaseBox which will also destroy the value in the box (if the
    // refcount == 1).
    fullmeta->vwt->initializeWithCopyFn(dst, addrInBox, metadata);
    releaseBoxFn(existVal->inlineBuffer[0]);
  } else {
    fullmeta->vwt->initializeWithTakeFn(dst, &(existVal->inlineBuffer[0]), metadata);
  }
}

static inline void
_swift_embedded_existential_init_with_copy(void *dst, void *srcExist) {
  ExistentialValue* existVal = (ExistentialValue*)srcExist;
  void *metadata = existVal->metadata;
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  ValueWitnessTableFlags isNonInlineMask = IsNonInline;
  if (fullmeta->vwt->flags & IsNonInline) {
    void *addrInBox = _swift_embedded_box_project(existVal->inlineBuffer[0], fullmeta);
    fullmeta->vwt->initializeWithCopyFn(dst, addrInBox, metadata);
  } else {
    fullmeta->vwt->initializeWithCopyFn(dst, &(existVal->inlineBuffer[0]), metadata);
  }
}

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H
