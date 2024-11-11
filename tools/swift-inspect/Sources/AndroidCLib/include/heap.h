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
#include <unistd.h>

#if defined(__cplusplus)
extern "C" {
#endif

#define HEAP_ITERATE_DATA_MAX_VALID_IDX 0
#define HEAP_ITERATE_DATA_NEXT_FREE_IDX 1
#define HEAP_ITERATE_DATA_HEADER_SIZE 2
#define HEAP_ITERATE_DATA_ENTRY_SIZE  2

typedef void (*heap_iterate_callback_t)(void* context, uint64_t base, uint64_t len);
bool heap_iterate(pid_t pid, void* callback_context, heap_iterate_callback_t callback);

void* heap_callback_start();
size_t heap_callback_len();

#if defined(__cplusplus)
}
#endif
