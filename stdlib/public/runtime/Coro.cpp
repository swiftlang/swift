//===------------- Array.cpp - Swift Array Operations Support -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Coro.h"
#include "swift/ABI/Coro.h"
#include "swift/Basic/FlagSet.h"

using namespace swift;

void swift::swift_coro_dealloc(CoroAllocator *allocator, void *ptr) {
  assert(allocator);
  // Calls to swift_coro_dealloc are emitted in resume funclets for every
  // live-across dynamic allocation.  Whether such calls immediately deallocate
  // memory depends on the allocator.
  if (!allocator->shouldDeallocateImmediately()) {
    return;
  }
  allocator->deallocate(ptr);
}
