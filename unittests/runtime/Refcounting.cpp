//===- swift/unittests/runtime/Refcounting.cpp - Reference-counting -------===//
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

static const FullMetadata<HeapMetadata> TestObjectMetadata{
  HeapMetadataHeader{{destroyTestObject}, {nullptr}},
  HeapMetadata{{MetadataKind::HeapLocalVariable}}
};

/// Create an object that, when deallocated, stores the given value to
/// the given pointer.
static TestObject *allocTestObject(size_t *addr, size_t value) {
  auto result =
    static_cast<TestObject*>(swift_allocObject(&TestObjectMetadata,
                                               sizeof(TestObject),
                                               alignof(TestObject) - 1));
  result->Addr = addr;
  result->Value = value;
  return result;
}

TEST(RefcountingTest, release) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_release(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, retain_release) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  auto retainResult = swift_retain(object);
  EXPECT_EQ(object, retainResult);
  EXPECT_EQ(0u, value);
  swift_release(object);
  EXPECT_EQ(0u, value);
  swift_release(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, pin_unpin) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  auto pinResult = swift_tryPin(object);
  EXPECT_EQ(object, pinResult);
  EXPECT_EQ(0u, value);
  swift_release(object);
  EXPECT_EQ(0u, value);
  swift_unpin(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, pin_pin_unpin_unpin) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  auto pinResult = swift_tryPin(object);
  EXPECT_EQ(object, pinResult);
  EXPECT_EQ(0u, value);
  auto pinResult2 = swift_tryPin(object);
  EXPECT_EQ(nullptr, pinResult2);
  EXPECT_EQ(0u, value);
  swift_unpin(pinResult2);
  EXPECT_EQ(0u, value);
  swift_release(object);
  EXPECT_EQ(0u, value);
  swift_unpin(object);
  EXPECT_EQ(1u, value);
}
