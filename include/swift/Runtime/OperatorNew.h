//===-- OperatorNew.h - Definitions of operators new and delete -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Never use the global definitions of operators new and delete.
// The app may replace them and we don't trust their implementations.

#include <new>
#include <stdlib.h>

#define SWIFT_PRIVATE_INLINE \
  __private_extern__ inline __attribute__((always_inline))

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winline-new-delete"

SWIFT_PRIVATE_INLINE void*
operator new(std::size_t size) throw (std::bad_alloc) {
  return malloc(size);
}

SWIFT_PRIVATE_INLINE void*
operator new[](std::size_t size) throw (std::bad_alloc) {
  return malloc(size);
}

SWIFT_PRIVATE_INLINE void*
operator new(std::size_t size, const std::nothrow_t&) throw() {
  return malloc(size);
}

SWIFT_PRIVATE_INLINE void*
operator new[](std::size_t size, const std::nothrow_t&) throw() {
  return malloc(size);
}

SWIFT_PRIVATE_INLINE void
operator delete(void* p) throw() {
  free(p);
}

SWIFT_PRIVATE_INLINE void
operator delete[](void* p) throw() {
  free(p);
}

SWIFT_PRIVATE_INLINE void
operator delete(void* p, const std::nothrow_t&) throw() {
  free(p);
}

SWIFT_PRIVATE_INLINE void
operator delete[](void* p, const std::nothrow_t&) throw() {
  free(p);
}

#pragma clang diagnostic pop
