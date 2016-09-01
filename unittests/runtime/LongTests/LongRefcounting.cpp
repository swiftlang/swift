//===--- LongRefcounting.cpp - Slow reference-counting tests for Swift ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "gtest/gtest.h"

using namespace swift;

struct TestObject : HeapObject {
  size_t *Addr;
  size_t Value;
};

static void destroyTestObject(HeapObject *_object) {
  auto object = static_cast<TestObject*>(_object);
  assert(object->Addr && "object already deallocated");
  *object->Addr = object->Value;
  object->Addr = nullptr;
  swift_deallocObject(object, sizeof(TestObject), alignof(TestObject) - 1);
}

static const FullMetadata<ClassMetadata> TestClassObjectMetadata = {
  { { &destroyTestObject }, { &_TWVBo } },
  { { { MetadataKind::Class } }, 0, /*rodata*/ 1,
  ClassFlags::UsesSwift1Refcounting, nullptr, 0, 0, 0, 0, 0 }
};

/// Create an object that, when deallocated, stores the given value to
/// the given pointer.
static TestObject *allocTestObject(size_t *addr, size_t value) {
  auto result =
    static_cast<TestObject *>(swift_allocObject(&TestClassObjectMetadata,
                                                sizeof(TestObject),
                                                alignof(TestObject) - 1));
  result->Addr = addr;
  result->Value = value;
  return result;
}


////////////////////////////////////////////
// Max retain count and overflow checking //
////////////////////////////////////////////


template <bool atomic>
static void retainALot(TestObject *object, size_t &deallocated,
                       uint64_t count) {
  for (uint64_t i = 0; i < count; i++) {
    if (atomic) swift_retain(object);
    else swift_nonatomic_retain(object);
    EXPECT_EQ(0u, deallocated);
  }
}

template <bool atomic>
static void releaseALot(TestObject *object, size_t &deallocated,
                        uint64_t count) {
  for (uint64_t i = 0; i < count; i++) {
    if (atomic) swift_release(object);
    else swift_nonatomic_release(object);
    EXPECT_EQ(0u, deallocated);
  }
}

// 32-3 bits of extra retain count, plus 1 for the implicit retain
const uint64_t maxRC = 1ULL << (32 - 3);

TEST(LongRefcountingTest, retain_max) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  retainALot<true>(object, deallocated, maxRC - 1);
  releaseALot<true>(object, deallocated, maxRC - 1);
  EXPECT_EQ(0u, deallocated);
  swift_release(object);
  EXPECT_EQ(1u, deallocated);
}

TEST(LongRefcountingTest, retain_overflow_DeathTest) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow error.
  retainALot<true>(object, deallocated, maxRC - 1);
  EXPECT_EQ(0u, deallocated);
  ASSERT_DEATH(swift_retain(object), "swift_abortRetainOverflow");
}

TEST(LongRefcountingTest, nonatomic_retain_max) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  retainALot<false>(object, deallocated, maxRC - 1);
  releaseALot<false>(object, deallocated, maxRC - 1);
  EXPECT_EQ(0u, deallocated);
  swift_nonatomic_release(object);
  EXPECT_EQ(1u, deallocated);
}

TEST(LongRefcountingTest, nonatomic_retain_overflow_DeathTest) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow error.
  retainALot<false>(object, deallocated, maxRC - 1);
  EXPECT_EQ(0u, deallocated);
  ASSERT_DEATH(swift_nonatomic_retain(object), "swift_abortRetainOverflow");
}

