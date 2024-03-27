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

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H
