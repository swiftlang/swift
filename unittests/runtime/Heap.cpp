//===--- Heap.cpp - Heap tests --------------------------------------------===//
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

#include "swift/Runtime/Heap.h"

#include "gtest/gtest.h"

void shouldAlloc(size_t size, size_t alignMask) {
  void *ptr = swift::swift_slowAlloc(size, alignMask);
  EXPECT_NE(ptr, (void *)NULL)
    << "Allocation failed for size " << size << " and alignment mask "
    << alignMask << ".";
  swift::swift_slowDealloc(ptr, size, alignMask);
}

void shouldAlloc(size_t size) {
  shouldAlloc(size, 0);
  shouldAlloc(size, 1);
  shouldAlloc(size, 3);
  shouldAlloc(size, 7);
  shouldAlloc(size, 15);
  shouldAlloc(size, 31);
  shouldAlloc(size, 63);
  shouldAlloc(size, 4095);
}

TEST(HeapTest, slowAlloc) {
  shouldAlloc(1);
  shouldAlloc(8);
  shouldAlloc(32);
  shouldAlloc(1093);
}

void shouldAllocTyped(size_t size, size_t alignMask, swift::MallocTypeId typeId) {
  void *ptr = swift::swift_slowAllocTyped(size, alignMask, typeId);
  EXPECT_NE(ptr, (void *)NULL)
    << "Typed allocation failed for size " << size << " and alignment mask "
    << alignMask << ".";
  swift::swift_slowDealloc(ptr, size, alignMask);
}

void shouldAllocTyped(size_t size, swift::MallocTypeId typeId) {
  shouldAlloc(size, 0);
  shouldAlloc(size, 1);
  shouldAlloc(size, 3);
  shouldAlloc(size, 7);
  shouldAlloc(size, 15);
  shouldAlloc(size, 31);
  shouldAlloc(size, 63);
  shouldAlloc(size, 4095);
}

void shouldAllocTyped(size_t size) {
  shouldAllocTyped(size, 42);
}

TEST(HeapTest, slowAllocTyped) {
  shouldAllocTyped(1);
  shouldAllocTyped(8);
  shouldAllocTyped(32);
  shouldAllocTyped(1093);
}

