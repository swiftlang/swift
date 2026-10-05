// An implementation of swift_slowAlloc and swift_slowDealloc to check that
// swift_slowDealloc is called with the same size and alignment that
// swift_slowAlloc was called with. Allocations are stored in a fixed-size table
// that's checked in dealloc. Alloc prints the size and alignment. Dealloc
// prints "dealloc MATCH" or "dealloc MISMATCH" accordingly, along with the
// actual and expected size and alignment.
//
// Implemented in C in order to override the implementations in
// EmbeddedRuntime.swift. The Swift implementations are emitted linkonce_odr,
// which these override. This is dicey, but it's only for testing.

#include <stdio.h>
#include <stdlib.h>

#define MAX_LIVE 64

struct entry {
  void *ptr;
  long size;
  long alignMask;
};

static struct entry live[MAX_LIVE];

static void emit(const char *verdict, long size, long alignMask, long wantSize,
                 long wantAlignMask) {
  printf("%s size=%ld alignMask=%ld want size=%ld alignMask=%ld\n", verdict,
         size, alignMask, wantSize, wantAlignMask);
}

static void fail(const char *msg) {
  printf("SHIM FAILURE: %s\n", msg);
  abort();
}

void *swift_slowAlloc(long size, long alignMask) {
  // A test that checks a size survived the trip proves nothing if the size going
  // in was also zero.
  if (size <= 0)
    fail("allocation with non-positive size");

  long alignment = (alignMask == -1) ? sizeof(void *) : alignMask + 1;
  if (alignment < sizeof(void *))
    alignment = sizeof(void *);

  void *p = NULL;
  if (posix_memalign(&p, alignment, size) != 0)
    fail("posix_memalign failed");

  for (int i = 0; i < MAX_LIVE; i++) {
    if (live[i].ptr == NULL) {
      live[i].ptr = p;
      live[i].size = size;
      live[i].alignMask = alignMask;
      printf("alloc size=%ld alignMask=%ld\n", size, alignMask);
      return p;
    }
  }

  fail("allocation table full");
  return NULL;
}

void swift_slowDealloc(void *ptr, long size, long alignMask) {
  for (int i = 0; i < MAX_LIVE; i++) {
    if (live[i].ptr == ptr) {
      if (size == live[i].size && alignMask == live[i].alignMask)
        emit("dealloc MATCH", size, alignMask, live[i].size, live[i].alignMask);
      else
        emit("dealloc MISMATCH", size, alignMask, live[i].size,
             live[i].alignMask);
      // Clear the slot so a recycled pointer can't match a stale entry.
      live[i].ptr = NULL;
      free(ptr);
      return;
    }
  }

  printf("dealloc UNTRACKED size=%ld alignMask=%ld\n", size, alignMask);
}
