// Fills every new heap allocation with a nonzero byte pattern, so that code
// reading memory before initializing it fails loudly instead of observing the
// zeros that the system allocator usually provides.
//
// Usage: DYLD_INSERT_LIBRARIES=<this dylib> <executable>

#include <malloc/malloc.h>
#include <stdlib.h>
#include <string.h>

// The malloc_type_* entry points are weakly referenced before macOS 14, where
// the Swift runtime uses malloc instead.
#pragma clang diagnostic ignored "-Wunguarded-availability-new"

// The pattern has its top bit clear, so that swift_release crashes loudly.
#define SCRIBBLE_BYTE 0x5A

static void scribble(void *p, size_t n) {
  if (p)
    memset(p, SCRIBBLE_BYTE, n);
}

static void *scribble_malloc(size_t n) {
  void *p = malloc(n);
  scribble(p, n);
  return p;
}

static void *scribble_malloc_zone_malloc(malloc_zone_t *z, size_t n) {
  void *p = malloc_zone_malloc(z, n);
  scribble(p, n);
  return p;
}

static int scribble_posix_memalign(void **p, size_t align, size_t n) {
  int err = posix_memalign(p, align, n);
  if (!err)
    scribble(*p, n);
  return err;
}

static void *scribble_malloc_type_malloc(size_t n, malloc_type_id_t t) {
  void *p = malloc_type_malloc(n, t);
  scribble(p, n);
  return p;
}

static void *scribble_malloc_type_zone_malloc(malloc_zone_t *z, size_t n,
                                              malloc_type_id_t t) {
  void *p = malloc_type_zone_malloc(z, n, t);
  scribble(p, n);
  return p;
}

static int scribble_malloc_type_posix_memalign(void **p, size_t align,
                                               size_t n, malloc_type_id_t t) {
  int err = malloc_type_posix_memalign(p, align, n, t);
  if (!err)
    scribble(*p, n);
  return err;
}

#define INTERPOSE(replacement, original)                                       \
  __attribute__((used)) static const struct {                                  \
    const void *replacement;                                                   \
    const void *original;                                                      \
  } interpose_##original __attribute__((section("__DATA,__interpose"))) = {    \
      (const void *)replacement, (const void *)original};

INTERPOSE(scribble_malloc, malloc)
INTERPOSE(scribble_malloc_zone_malloc, malloc_zone_malloc)
INTERPOSE(scribble_posix_memalign, posix_memalign)
INTERPOSE(scribble_malloc_type_malloc, malloc_type_malloc)
INTERPOSE(scribble_malloc_type_zone_malloc, malloc_type_zone_malloc)
INTERPOSE(scribble_malloc_type_posix_memalign, malloc_type_posix_memalign)
