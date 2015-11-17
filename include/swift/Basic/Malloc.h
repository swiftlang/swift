//===- Malloc.h - Aligned malloc interface ----------------------*- C++ -*-===//
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
//  This file provides an implementation of C11 aligned_alloc(3) for platforms
//  that don't have it yet, using posix_memalign(3).
//
//===----------------------------------------------------------------------===//

#ifndef __SWIFT_MALLOC_H__
#define __SWIFT_MALLOC_H__

#include <cassert>
#include <cstdlib>

namespace swift {
  enum InPlace_t { InPlace };
}

/// A slightly more efficient version of the global placement operator new.
///
/// Unlike the standard global placement operator new, this operator
/// is not declared noexcept, which means it is not allowed to return
/// null under the language semantics, which eliminates an unnecessary
/// null check that the compiler would otherwise have to insert.
/// (C++ is dumb.)
inline void *operator new(size_t size, void *ptr, swift::InPlace_t) {
  return ptr;
}

namespace swift {

// FIXME: Use C11 aligned_alloc or Windows _aligned_malloc if available.
inline void *AlignedAlloc(size_t size, size_t align) {
  // posix_memalign only accepts alignments greater than sizeof(void*).
  // 
  if (align < sizeof(void*))
    align = sizeof(void*);
  
  void *r;
  int res = posix_memalign(&r, align, size);
  assert(res == 0 && "posix_memalign failed");
  (void)res; // Silence the unused variable warning
  return r;
}
  
// FIXME: Use Windows _aligned_free if available.
inline void AlignedFree(void *p) {
  free(p);
}
  
}

#endif
