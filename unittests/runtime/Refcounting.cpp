//===--- Refcounting.cpp - Reference-counting for Swift -------------------===//
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
  swift_retain(object);
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

TEST(RefcountingTest, retain_release_n) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_retain_n(object, 32);
  swift_retain(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(34u, swift_retainCount(object));
  swift_release_n(object, 31);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(3u, swift_retainCount(object));
  swift_release(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(2u, swift_retainCount(object));
  swift_release_n(object, 1);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(1u, swift_retainCount(object));
}

TEST(RefcountingTest, unknown_retain_release_n) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_unknownRetain_n(object, 32);
  swift_unknownRetain(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(34u, swift_retainCount(object));
  swift_unknownRelease_n(object, 31);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(3u, swift_retainCount(object));
  swift_unknownRelease(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(2u, swift_retainCount(object));
  swift_unknownRelease_n(object, 1);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(1u, swift_retainCount(object));
}

TEST(RefcountingTest, unowned_retain_release_n) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_unownedRetain_n(object, 32);
  swift_unownedRetain(object);
  EXPECT_EQ(34u, swift_unownedRetainCount(object));
  swift_unownedRelease_n(object, 31);
  EXPECT_EQ(3u, swift_unownedRetainCount(object));
  swift_unownedRelease(object);
  EXPECT_EQ(2u, swift_unownedRetainCount(object));
  swift_unownedRelease_n(object, 1);
  EXPECT_EQ(1u, swift_unownedRetainCount(object));
  swift_release(object);
  EXPECT_EQ(1u, value);
}

/////////////////////////////////////////
// Non-atomic reference counting tests //
/////////////////////////////////////////

TEST(RefcountingTest, nonatomic_release) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_nonatomic_release(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, nonatomic_retain_release) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_nonatomic_retain(object);
  EXPECT_EQ(0u, value);
  swift_nonatomic_release(object);
  EXPECT_EQ(0u, value);
  swift_nonatomic_release(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, nonatomic_pin_unpin) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  auto pinResult = swift_nonatomic_tryPin(object);
  EXPECT_EQ(object, pinResult);
  EXPECT_EQ(0u, value);
  swift_nonatomic_release(object);
  EXPECT_EQ(0u, value);
  swift_nonatomic_unpin(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, nonatomic_pin_pin_unpin_unpin) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  auto pinResult = swift_nonatomic_tryPin(object);
  EXPECT_EQ(object, pinResult);
  EXPECT_EQ(0u, value);
  auto pinResult2 = swift_nonatomic_tryPin(object);
  EXPECT_EQ(nullptr, pinResult2);
  EXPECT_EQ(0u, value);
  swift_nonatomic_unpin(pinResult2);
  EXPECT_EQ(0u, value);
  swift_nonatomic_release(object);
  EXPECT_EQ(0u, value);
  swift_nonatomic_unpin(object);
  EXPECT_EQ(1u, value);
}

TEST(RefcountingTest, nonatomic_retain_release_n) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_nonatomic_retain_n(object, 32);
  swift_nonatomic_retain(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(34u, swift_retainCount(object));
  swift_nonatomic_release_n(object, 31);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(3u, swift_retainCount(object));
  swift_nonatomic_release(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(2u, swift_retainCount(object));
  swift_nonatomic_release_n(object, 1);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(1u, swift_retainCount(object));
}

TEST(RefcountingTest, nonatomic_unknown_retain_release_n) {
  size_t value = 0;
  auto object = allocTestObject(&value, 1);
  EXPECT_EQ(0u, value);
  swift_nonatomic_unknownRetain_n(object, 32);
  swift_nonatomic_unknownRetain(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(34u, swift_retainCount(object));
  swift_nonatomic_unknownRelease_n(object, 31);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(3u, swift_retainCount(object));
  swift_nonatomic_unknownRelease(object);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(2u, swift_retainCount(object));
  swift_nonatomic_unknownRelease_n(object, 1);
  EXPECT_EQ(0u, value);
  EXPECT_EQ(1u, swift_retainCount(object));
}

