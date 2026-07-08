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
  const void * __ptrauth(2, 1, 0x6ae1) metadata;
#else
  const void *metadata;
#endif
} EmbeddedHeapObject;

static inline void
_swift_embedded_invoke_heap_object_destroy(void *object) {
  const void *metadata = ((EmbeddedHeapObject *)object)->metadata;
  void **destroy_location = &((void **)metadata)[1];
#if __has_feature(ptrauth_calls)
  (*(HeapObjectDestroyer __ptrauth(0, 1, 0xbbbf) *)destroy_location)(object);
#else
  (*(HeapObjectDestroyer *)destroy_location)(object);
#endif
}

static inline void
_swift_embedded_invoke_heap_object_optional_ivardestroyer(void *object, const void *metadata) {
  void **ivardestroyer_location = &((void **)metadata)[2];
  if (*ivardestroyer_location) {
#if __has_feature(ptrauth_calls)
    (*(HeapObjectDestroyer __ptrauth(0, 1, 0xbbbf) *)ivardestroyer_location)(object);
#else
    (*(HeapObjectDestroyer *)ivardestroyer_location)(object);
#endif
  }
}

static inline const void *_swift_embedded_get_heap_object_metadata_pointer(void *object) {
  return ((EmbeddedHeapObject *)object)->metadata;
}

static inline void _swift_embedded_set_heap_object_metadata_pointer(void *object, const void *metadata) {
  ((EmbeddedHeapObject *)object)->metadata = metadata;
}

typedef struct {
  void  *initializeBufferWithCopyOfBufferFn;
#if __has_feature(ptrauth_calls)
  void  (* __ptrauth(0, 1, 0x04f8)  destroyFn)(void *, const void*);
#else
  void  (*destroyFn)(void *, const void*);
#endif
#if __has_feature(ptrauth_calls)
  void* (* __ptrauth(0, 1, 0xe3ba) initializeWithCopyFn)(void*, const void*, const void*);
#else
  void* _Nonnull (* _Nonnull initializeWithCopyFn)(void*, void*, const void*);
#endif
  void  *assignWithCopyFn;
#if __has_feature(ptrauth_calls)
  void* (* __ptrauth(0, 1, 0x48d8) initializeWithTakeFn)(void*, void*, const void*);
#else
  void* _Nonnull (* _Nonnull initializeWithTakeFn)(void *, void*, const void*);
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
EmbeddedMetaDataPrefix *_swift_embedded_get_full_metadata(const void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = (EmbeddedMetaDataPrefix*)&((void **)metadata)[-1];
  return fullmeta;
}

static inline __swift_size_t
_swift_embedded_metadata_get_size(const void *metadata) {
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
_swift_embedded_metadata_get_align_mask(const void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  return _swift_embedded_metadata_get_align_mask_impl(fullmeta);
}

static inline unsigned
_swift_embedded_metadata_get_vwt_flags(const void *metadata) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  return fullmeta->vwt->flags;
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
  const void *metadata = ((EmbeddedHeapObject *)object)->metadata;
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
  void * _Nonnull inlineBuffer[3];
  const void *metadata;
} ExistentialValue;

static inline void
_swift_embedded_existential_destroy(void *exist, void (*releaseBoxFn) (void *)) {
  ExistentialValue* existVal = (ExistentialValue*)exist;
  const void *metadata = existVal->metadata;
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
  const void *metadata = existVal->metadata;
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
  const void *metadata = existVal->metadata;
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  ValueWitnessTableFlags isNonInlineMask = IsNonInline;
  if (fullmeta->vwt->flags & IsNonInline) {
    void *addrInBox = _swift_embedded_box_project(existVal->inlineBuffer[0], fullmeta);
    fullmeta->vwt->initializeWithCopyFn(dst, addrInBox, metadata);
  } else {
    fullmeta->vwt->initializeWithCopyFn(dst, &(existVal->inlineBuffer[0]), metadata);
  }
}

// Helpers for value-witness operations used by the embedded error runtime.
static inline void
_swift_embedded_metadata_initialize_with_copy(const void *metadata, void *dst, const void *src) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  fullmeta->vwt->initializeWithCopyFn(dst, (void*)src, metadata);
}

static inline void
_swift_embedded_metadata_initialize_with_take(const void *metadata, void *dst, void *src) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  fullmeta->vwt->initializeWithTakeFn(dst, src, metadata);
}

static inline void
_swift_embedded_metadata_destroy(const void *metadata, void *value) {
  EmbeddedMetaDataPrefix *fullmeta = _swift_embedded_get_full_metadata(metadata);
  fullmeta->vwt->destroyFn(value, metadata);
}

// Calling-convention bridge for invoking a key-path accessor thunk from
// the embedded runtime walker.  A key-path getter thunk has SIL type
// `@convention(keypath_accessor_getter) (@in_guaranteed T) -> @out T`,
// which at the LLVM level lowers to
// `void SWIFT_CC(swift)(sret out, in base, args ptr, args size)`.  From
// Swift we only have a raw function pointer and untyped memory pointers,
// so we can't express that ABI directly in Swift's type system.  This
// shim takes the pointers as regular C args and internally casts the
// function pointer to a swiftcc type with an `SWIFT_INDIRECT_RESULT`
// parameter — clang then rewrites the call so `out` is passed via the
// architecture's indirect-result register (x8 on arm64, rdi on x86_64),
// matching the callee's expectations.
//
// `arg` / `argSize` correspond to the KP thunk's trailing "index
// argument" pair — always `(NULL, 0)` for the components the embedded
// static-instance emitter produces (no captured subscript indices).
static inline void
_swift_embedded_kp_invokeGetter(void *_Nonnull fn,
                                void *_Nonnull out,
                                const void *_Nonnull base,
                                const void *_Nullable arg,
                                __swift_size_t argSize) {
  typedef void SWIFT_CC_swift (*Getter)(void *SWIFT_INDIRECT_RESULT,
                                        const void *, const void *,
                                        __swift_size_t);
  ((Getter)fn)(out, base, arg, argSize);
}

// Companion to `_swift_embedded_kp_invokeGetter` for the setter side of
// a KP writeback.  A KP setter thunk has SIL type
// `@convention(keypath_accessor_setter) (@in_guaranteed NewValue,
//                                        @{in_guaranteed|inout} CurValue) -> ()`,
// which at the LLVM level lowers to
// `void SWIFT_CC(swift)(ptr value_in, ptr base_in_or_inout,
//                       ptr args, size_t args_size)`.  Both mutating and
// nonmutating setters produce the same LLVM signature — the mutating-vs-
// nonmutating distinction is purely a source-level statement about
// aliasing of the base parameter.  Neither uses sret (no return value),
// so we can call it via a plain-pointer C signature: on arm64/x86_64 the
// register assignment (x0-x3 / rdi-rcx) matches swiftcc for four
// pointer/integer args with no return.
static inline void
_swift_embedded_kp_invokeSetter(void *_Nonnull fn,
                                const void *_Nonnull value,
                                void *_Nonnull base,
                                const void *_Nullable arg,
                                __swift_size_t argSize) {
  typedef void SWIFT_CC_swift (*Setter)(const void *, void *,
                                        const void *, __swift_size_t);
  ((Setter)fn)(value, base, arg, argSize);
}

// Swift implementation of error box destroy logic (defined in EmbeddedRuntime.swift).
// Takes the object as a regular swiftcc parameter (x0/rdi on arm64/x86_64).
SWIFT_CC_swift extern void _swift_embedded_error_destroy_impl(void * _Nonnull object);

// Calling convention bridge: HeapObjectDestroyer passes the object via swiftself
// (x20 on arm64, r13 on x86_64), but @_silgen_name Swift functions receive it as a
// regular parameter (x0/rdi). This wrapper receives via SWIFT_CONTEXT (swiftself)
// and forwards as a regular parameter to the Swift implementation.
// Named _swift_embedded_error_box_destroy (not _swift_embedded_error_destroy) to
// avoid SIL name collision — Swift IRGen would otherwise shadow the clang-compiled
// version, losing the swiftcall/swiftself attributes.
SWIFT_CC_swift static inline void
_swift_embedded_error_box_destroy(SWIFT_CONTEXT void * _Nonnull object) {
  _swift_embedded_error_destroy_impl(object);
}

// Returns the address of the calling convention bridge for metadata storage init.
static inline void * _Nonnull _swift_embedded_error_destroy_ptr(void) {
  return (void *)_swift_embedded_error_box_destroy;
}

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H
