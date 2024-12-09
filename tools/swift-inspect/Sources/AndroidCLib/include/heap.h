//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#pragma once

#include <stdbool.h>
#include <stdint.h>

#if defined(__cplusplus)
extern "C" {
#endif

// Location of the heap_iterate callback.
void* heap_iterate_callback_start();

// Size of the heap_iterate callback.
size_t heap_iterate_callback_len();

// Initialize the provided buffer to receive heap iteration metadata.
bool heap_iterate_metadata_init(void* data, size_t len);

// Callback invoked by heap_iterate_data_process for each heap entry .
typedef void (*heap_iterate_entry_callback_t)(void* context, uint64_t base, uint64_t len);

// Process all heap iteration entries in the provided buffer.
bool heap_iterate_metadata_process(
    void* data, size_t len, void* callback_context, heap_iterate_entry_callback_t callback);

#if defined(__cplusplus)
}
#endif
