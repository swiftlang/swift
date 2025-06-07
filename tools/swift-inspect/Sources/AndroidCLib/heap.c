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

#include <string.h>

#include "heap.h"

/* The heap metadata buffer is interpreted as an array of 8-byte pairs. The
 * first pair contains metadata describing the buffer itself: max valid index
 * (e.g. size of the buffer) and next index (e.g. write cursor/position). Each
 * subsequent pair describes the address and length of a heap entry in the
 * remote process. A 4KiB page provides sufficient space for the header and
 * 255 (address, length) pairs.
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

#if !__has_builtin(__builtin_debugtrap)
#error("compiler support for __builtin_debugtrap is required")
#endif

#define MAX_VALID_IDX 0
#define NEXT_FREE_IDX 1
#define HEADER_SIZE 2
#define ENTRY_SIZE  2

// Callback for malloc_iterate. Because this function is meant to be copied to
// a different process for execution, it must not make any function calls to
// ensure compiles to simple, position-independent code. It is implemented in C
// for readability/maintainability. It is placed in its own code section to
// simplify calculating its size.
__attribute__((noinline, used, section("heap_iterator")))
static void heap_iterate_callback(unsigned long base, unsigned long size, void *arg) {
  volatile uint64_t *data = (uint64_t*)arg;
  while (data[NEXT_FREE_IDX] >= data[MAX_VALID_IDX]) {
    // SIGTRAP indicates the buffer is full and needs to be drained before more
    // entries can be written.
    __builtin_debugtrap();

    // After the SIGTRAP, the signal handler advances the instruction pointer
    // (PC) to the next instruction. Inserting a nop instruction here ensures
    // the CPU has a clear, executable instruction to process, which avoids
    // potential speculative execution or pipeline issues that could arise if
    // the next instruction were a control transfer like a branch or jump.
    __asm__ __volatile__("nop");
  }
  data[data[NEXT_FREE_IDX]++] = base;
  data[data[NEXT_FREE_IDX]++] = size;
}

// The linker implicitly defines __start- and __stop- prefixed symbols that mark
// the start and end of user defined sections.
extern char __stop_heap_iterator[];

void* heap_iterate_callback_start() {
  return (void*)heap_iterate_callback;
}

size_t heap_iterate_callback_len() {
  return (uintptr_t)__stop_heap_iterator - (uintptr_t)heap_iterate_callback;
}

bool heap_iterate_metadata_init(void* data, size_t len) {
  uint64_t *metadata = data;
  const uint64_t max_entries = len / sizeof(uint64_t);
  if (max_entries < HEADER_SIZE + ENTRY_SIZE)
    return false;

  memset(data, 0, len);
  metadata[MAX_VALID_IDX] = max_entries;
  metadata[NEXT_FREE_IDX] = HEADER_SIZE;
  return true;
}

bool heap_iterate_metadata_process(
  void* data, size_t len, void* callback_context, heap_iterate_entry_callback_t callback) {
  uint64_t *metadata = data;
  const uint64_t max_entries = len / sizeof(uint64_t);
  const uint64_t end_index = metadata[NEXT_FREE_IDX];

  if (metadata[MAX_VALID_IDX] != max_entries || end_index > max_entries)
    return false;

  for (size_t i = HEADER_SIZE; i < end_index; i += ENTRY_SIZE) {
    const uint64_t base = metadata[i];
    const uint64_t size = metadata[i + 1];
    callback(callback_context, base, size);
  }

  return true;
}
