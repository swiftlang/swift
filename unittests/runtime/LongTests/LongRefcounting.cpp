//===--- LongRefcounting.cpp - Slow reference-counting tests for Swift ----===//
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

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Basic/ManglingMacros.h"
#include "gtest/gtest.h"

using namespace swift;

struct TestObject : HeapObject {
  size_t *Addr;
  size_t Value;
};

static SWIFT_CC(swift) void destroyTestObject(SWIFT_CONTEXT HeapObject *_object) {
  auto object = static_cast<TestObject*>(_object);
  assert(object->Addr && "object already deallocated");
  *object->Addr = object->Value;
  object->Addr = nullptr;
  swift_deallocObject(object, sizeof(TestObject), alignof(TestObject) - 1);
}

static const FullMetadata<ClassMetadata> TestClassObjectMetadata = {
  { { &destroyTestObject }, { &VALUE_WITNESS_SYM(Bo) } },
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

// 32-2 bits of retain count.
const uint64_t maxRC = (1ULL << (32 - 2)) - 1;

TEST(LongRefcountingTest, retain_max) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  retainALot<true>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  releaseALot<true>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(0u, deallocated);
  swift_release(object);
  EXPECT_EQ(1u, deallocated);
}

TEST(LongRefcountingTest, nonatomic_retain_max) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  retainALot<false>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  releaseALot<false>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(0u, deallocated);
  swift_nonatomic_release(object);
  EXPECT_EQ(1u, deallocated);
}

TEST(RefcountingTest, retain_overflow) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow.
  retainALot<true>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  EXPECT_EQ(0u, deallocated);

  // There is no overflow enforcement in the runtime today.
  // Instead we check that the retain count wrapped around.
  swift_retain(object);
  EXPECT_EQ(swift_retainCount(object), 0u);
  EXPECT_EQ(0u, deallocated);
}

TEST(RefcountingTest, nonatomic_retain_overflow) {
  size_t deallocated = 0;
  auto object = allocTestObject(&deallocated, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow.
  retainALot<false>(object, deallocated, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  EXPECT_EQ(0u, deallocated);

  // There is no overflow enforcement in the runtime today.
  // Instead we check that the retain count wrapped around.
  swift_nonatomic_retain(object);
  EXPECT_EQ(swift_retainCount(object), 0u);
  EXPECT_EQ(0u, deallocated);
}
