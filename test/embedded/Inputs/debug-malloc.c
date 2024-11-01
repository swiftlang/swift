#include <stdlib.h>
#include <stdio.h>

#define HEAP_SIZE (8 * 1024)

__attribute__((aligned(16)))
char heap[HEAP_SIZE] = {};

size_t next_heap_index = 0;

void *calloc(size_t count, size_t size) {
  printf("malloc(%ld)", count);

  if (next_heap_index + count * size > HEAP_SIZE) {
    puts("HEAP EXHAUSTED\n");
    __builtin_trap();
  }
  void *p = &heap[next_heap_index];
  next_heap_index += count * size;
  printf("-> %p\n", p);
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
  // don't actually free
  printf("free(%p)\n", ptr);
}
