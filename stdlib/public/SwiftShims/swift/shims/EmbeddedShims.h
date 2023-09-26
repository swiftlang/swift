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

#define SWIFT_CC __attribute__((swiftcall))
#define SWIFT_CONTEXT __attribute__((swift_context))

typedef void SWIFT_CC (*HeapObjectDestroyer)(SWIFT_CONTEXT void *object);

static inline void _swift_runtime_invoke_heap_object_destroy(
    const void *destroy, void *self) {
  ((HeapObjectDestroyer)destroy)(self);
}

#ifdef __cplusplus
} // extern "C"
#endif

#if __has_feature(nullability)
#pragma clang assume_nonnull end
#endif

#endif // SWIFT_STDLIB_SHIMS_EMBEDDEDSHIMS_H
