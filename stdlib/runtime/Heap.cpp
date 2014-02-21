//===--- Heap.cpp - Swift Language Heap Logic -----------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations of the Swift heap
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Heap.h"
#include <stdlib.h>

__attribute__((visibility("hidden")))
malloc_zone_t _swift_zone = {
  NULL, // ignore -- for CF
  NULL, // ignore -- for CF
  swift::_swift_zone_size,
  swift::_swift_zone_malloc,
  swift::_swift_zone_calloc,
  swift::_swift_zone_valloc,
  swift::_swift_zone_free,
  swift::_swift_zone_realloc,
  swift::_swift_zone_destroy,
  "SwiftZone", // name
  NULL, // optional batch malloc
  NULL, // optional batch free
  NULL, // FIXME -- struct malloc_introspection_t *
  0, // version
  NULL, // XXX -- add support for memalign and free_definite_size?
};

size_t swift::_swift_zone_size(malloc_zone_t *zone, const void *pointer) {
  auto z = malloc_default_zone();
  return z->size(z, pointer);
}

void *swift::_swift_zone_malloc(malloc_zone_t *zone, size_t size) {
  auto z = malloc_default_zone();
  return z->malloc(z, size);
}

void *swift::_swift_zone_calloc(malloc_zone_t *zone,
                                size_t count, size_t size) {
  auto z = malloc_default_zone();
  return z->calloc(z, count, size);
}

void *swift::_swift_zone_valloc(malloc_zone_t *zone, size_t size) {
  auto z = malloc_default_zone();
  return z->valloc(z, size);
}

void swift::_swift_zone_free(malloc_zone_t *zone, void *pointer) {
  auto z = malloc_default_zone();
  return z->free(z, pointer);
}

void *swift::_swift_zone_realloc(malloc_zone_t *zone,
                                 void *pointer, size_t size) {
  auto z = malloc_default_zone();
  return z->realloc(z, pointer, size);
}

void swift::_swift_zone_destroy(malloc_zone_t *zone) {
  // nobody should ever destroy this zone
  abort();
}
