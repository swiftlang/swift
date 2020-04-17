//===--- Metadata.h -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_STDLIB_SHIMS_METADATA_H
#define SWIFT_STDLIB_SHIMS_METADATA_H

#include "SwiftStddef.h"
#include "Visibility.h"

#ifndef __swift__

#include "swift/ABI/Metadata.h"

#else // ifndef __swift__

typedef struct {
  const void *type;
  __swift_size_t state;
} MetadataResponse;

#endif

#ifdef __cplusplus
namespace swift { extern "C" {
#endif

// Shims to call a metadata accessor in Swift.
SWIFT_RUNTIME_STDLIB_API
MetadataResponse _swift_metadataAccessorCall(void *accessor,
                                             __swift_size_t request,
                                             const void * const *args,
                                             __swift_size_t size);
#ifdef __cplusplus
}} // extern "C", namespace swift
#endif

#endif
