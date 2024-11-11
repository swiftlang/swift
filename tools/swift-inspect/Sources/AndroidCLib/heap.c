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

#include "heap.h"

#if defined(__aarch64__) || defined(__ARM64__) || defined(_M_ARM64)
#define DEBUG_BREAK() asm("brk #0x0; nop")
#elif defined(_M_X64) || defined(__amd64__) || defined(__x86_64__) || defined(_M_AMD64)
#define DEBUG_BREAK() asm("int3; nop")
#else
#error("only aarch64 and x86_64 are supported")
#endif

/* We allocate a buffer in the remote process that it populates with metadata
 * describing each heap entry it enumerates. We then read the contents of the
 * buffer, and individual heap entry contents, with process_vm_readv.
 *
 * The buffer is interpreted as an array of 8-byte pairs. The first pair
 * contains metadata describing the buffer itself: max valid index (e.g. size of
 * the buffer) and next index (e.g. write cursor/position). Each subsequent pair
 * describes the address and length of a heap entry in the remote process.
 *
 * ------------
 * | uint64_t | max valid index (e.g. sizeof(buffer) / sizeof(uint64_t))
 * ------------
 * | uint64_t | next free index (starts at 2)
 * ------------
 * | uint64_t | heap item 1 address
 * ------------
 * | uint64_t | heap item 1 size
 * ------------
 * | uint64_t | heap item 2 address
 * ------------
 * | uint64_t | heap item 2 size
 * ------------
 * | uint64_t | ...
 * ------------
 * | uint64_t | ...
 * ------------
 * | uint64_t | heap item N address
 * ------------
 * | uint64_t | heap item N size
 * ------------
 */

// NOTE: this function cannot call any other functions and must only use
// relative branches. We could inline assembly instead, but C is more reabable
// and maintainable.
static void remote_callback_start(unsigned long base, unsigned long size, void *arg) {
  volatile uint64_t *data = (uint64_t*)arg;
  while (data[HEAP_ITERATE_DATA_NEXT_FREE_IDX] >= data[HEAP_ITERATE_DATA_MAX_VALID_IDX]) {
    DEBUG_BREAK();
  }
  data[data[HEAP_ITERATE_DATA_NEXT_FREE_IDX]++] = base;
  data[data[HEAP_ITERATE_DATA_NEXT_FREE_IDX]++] = size;
}

// NOTE: this function is here to mark the end of remote_callback_start and is never
// called.
static void remote_callback_end() {}

void* heap_callback_start() {
  return (void*)remote_callback_start;
}

size_t heap_callback_len() {
  return (size_t)(remote_callback_end - remote_callback_start);
}
