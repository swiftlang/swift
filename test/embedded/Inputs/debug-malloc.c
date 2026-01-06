#include <stdlib.h>
#include <stdio.h>

#define HEAP_SIZE (128 * 1024)

__attribute__((aligned(16)))
char heap[HEAP_SIZE] = {};

size_t next_heap_index = 0;

void *calloc(size_t count, size_t size) {
  size_t total_size = count * size;
  printf("malloc(%ld)", total_size);

  if (total_size % 16 != 0)
    total_size = total_size + (16 - total_size % 16);

  if (next_heap_index + total_size > HEAP_SIZE) {
    puts("HEAP EXHAUSTED\n");
    __builtin_trap();
  }
  void *p = &heap[next_heap_index];
  next_heap_index += total_size;
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
