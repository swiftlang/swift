//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors.
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include <stdint.h>
#include <stddef.h>

int putchar(int c);

int puts(const char *p) {
  while (*p) {
    putchar(*(p++));
  }
  return 0;
}

#define HEAP_SIZE (1 * 1024)

__attribute__((aligned(4)))
char heap[HEAP_SIZE] = {};

size_t next_heap_index = 0;

void *calloc(size_t count, size_t size) {
  if (next_heap_index + count * size > HEAP_SIZE) {
    puts("HEAP EXHAUSTED\n");
    __builtin_trap();
  }
  void *p = &heap[next_heap_index];
  next_heap_index += count * size;
  return p;
}

void *malloc(size_t count) {
  return calloc(count, 1);
}

int posix_memalign(void **memptr, size_t alignment, size_t size) {
  *memptr = calloc(size + alignment, 1);
  if (((uintptr_t)*memptr) % alignment == 0) return 0;
  *(uintptr_t *)memptr += alignment - ((uintptr_t)*memptr % alignment);
  return 0;
}

void free(void *ptr) {
  // never free
}

__attribute__((used))
void *memset(void *b, int c, size_t len) {
  for (int i = 0; i < len; i++) {
    ((char *)b)[i] = c;
  }
  return b;
}

__attribute__((used))
void *memcpy(void *restrict dst, const void *restrict src, size_t n) {
  for (int i = 0; i < n; i++) {
    ((char *)dst)[i] = ((char *)src)[i];
  }
  return dst;
}

__attribute__((used))
int memcmp(const void *s1, const void *s2, size_t n) {
  for (int i = 0; i < n; i++) {
    char diff = ((char *)s2)[i] - ((char *)s1)[i];
    if (diff != 0) return diff;
  }
  return 0;
}

__attribute__((used))
void *memmove(void *dst, const void *src, size_t n) {
  if ((uintptr_t)dst < (uintptr_t)src) {
    for (int i = 0; i < n; i++) {
      ((char *)dst)[i] = ((char *)src)[i];
    }
    return dst;
  } else {
    for (int i = n - 1; i >= 0; i--) {
      ((char *)dst)[i] = ((char *)src)[i];
    }
    return dst;
  }
}

void *__aeabi_memmove(void *dst, const void *src, size_t n) {
  return memmove(dst, src, n);
}

int rand(void) {
#if UINTPTR_MAX == UINT16_MAX
  static int seed = 0x3691;
  seed *= 0x3691;
  seed ^= 0x3692;
#else
  static int seed = 0x19283691;
  seed *= 0x19283691;
  seed ^= 0x81723692;
#endif
  return seed;
}

void arc4random_buf(void *buf, size_t nbytes) {
  for (int i = 0; i < nbytes; i++) { ((char *)buf)[i] = rand(); }
}
