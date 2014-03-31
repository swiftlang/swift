//===--- Heap.h - Swift Language Heap ABI ----------------------*- C++ -*--===//
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
// Swift Heap ABI
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_RUNTIME_HEAP_H
#define SWIFT_RUNTIME_HEAP_H

#include <llvm/Support/Compiler.h>
#include <malloc/malloc.h>

namespace swift {

LLVM_LIBRARY_VISIBILITY
extern malloc_zone_t zoneShims;

LLVM_LIBRARY_VISIBILITY
size_t _swift_zone_size   (malloc_zone_t *zone, const void *pointer);
LLVM_LIBRARY_VISIBILITY
void  *_swift_zone_malloc (malloc_zone_t *zone, size_t size);
LLVM_LIBRARY_VISIBILITY
void  *_swift_zone_calloc (malloc_zone_t *zone, size_t count, size_t size);
LLVM_LIBRARY_VISIBILITY
void  *_swift_zone_valloc (malloc_zone_t *zone, size_t size);
LLVM_LIBRARY_VISIBILITY
void   _swift_zone_free   (malloc_zone_t *zone, void *pointer);
LLVM_LIBRARY_VISIBILITY
void  *_swift_zone_realloc(malloc_zone_t *zone, void *pointer, size_t size);
LLVM_LIBRARY_VISIBILITY
void   _swift_zone_destroy(malloc_zone_t *zone);

}

#endif /* SWIFT_RUNTIME_HEAP_H */
