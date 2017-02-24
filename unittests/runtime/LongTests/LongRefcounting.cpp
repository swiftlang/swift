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

#ifdef __APPLE__
// FIXME: is EXPECT_UNALLOCATED reliable enough for CI?
// EXPECT_ALLOCATED may fail falsely if the memory is re-allocated.
# include <malloc/malloc.h>
# define EXPECT_ALLOCATED(p)    EXPECT_NE(0u, malloc_size(p))
# define EXPECT_UNALLOCATED(p)  EXPECT_EQ(0u, malloc_size(p))
#else
// FIXME: heap assertion for other platforms?
# define EXPECT_ALLOCATED(p) do {} while (0)
# define EXPECT_UNALLOCATED(p) do {} while (0)
#endif

using namespace swift;

struct TestObject : HeapObject {
  // *Addr = Value during deinit
  size_t *Addr;
  size_t Value;
  
  // Check lifecycle state DEINITING during deinit
  bool CheckLifecycle;

  // Weak variable to check in CheckLifecycle. nullptr skips the check.
  // On entry to deinit: must point to object
  // On exit from deinit: is destroyed
  WeakReference *WeakRef;
  
  TestObject(size_t *addr, size_t value)
    : Addr(addr), Value(value), CheckLifecycle(false), WeakRef(nullptr)
  { }
};

static SWIFT_CC(swift) void deinitTestObject(SWIFT_CONTEXT HeapObject *_object) {
  auto object = static_cast<TestObject*>(_object);
  assert(object->Addr && "object already deallocated");

  if (object->CheckLifecycle) {
    // RC ok
    swift_retain(object);
    swift_retain(object);
    swift_release(object);
    swift_release(object);
    // FIXME: RC underflow during deinit?

    // URC load crashes
    // URC increment OK
    // URC decrement OK
    ASSERT_DEATH(swift_unownedCheck(object),
                 "attempted to read an unowned reference");
    swift_unownedRetain(object);
    swift_unownedRetain(object);
    swift_unownedRelease(object);
    swift_unownedRelease(object);
    
    if (object->WeakRef) {
      // WRC load is nil
      // WRC increment is nil
      // WRC decrement OK

      // WRC -1
      auto weak_value = swift_weakLoadStrong(object->WeakRef);
      EXPECT_EQ(nullptr, weak_value);
      swift_weakDestroy(object->WeakRef);

      // WRC no change
      swift_weakInit(object->WeakRef, object);
      weak_value = swift_weakLoadStrong(object->WeakRef);
      EXPECT_EQ(nullptr, weak_value);

      // WRC no change
      swift_weakInit(object->WeakRef, object);
      weak_value = swift_weakLoadStrong(object->WeakRef);
      EXPECT_EQ(nullptr, weak_value);
    }
  }

  *object->Addr = object->Value;
  object->Addr = nullptr;
  swift_deallocObject(object, sizeof(TestObject), alignof(TestObject) - 1);
}

static const FullMetadata<ClassMetadata> TestClassObjectMetadata = {
  { { &deinitTestObject }, { &VALUE_WITNESS_SYM(Bo) } },
  { { { MetadataKind::Class } }, 0, /*rodata*/ 1,
  ClassFlags::UsesSwift1Refcounting, nullptr, 0, 0, 0, 0, 0 }
};

/// Create an object that, when deallocated, stores the given value to
/// the given pointer.
static TestObject *allocTestObject(size_t *addr, size_t value) {
  auto buf = swift_allocObject(&TestClassObjectMetadata,
                               sizeof(TestObject),
                               alignof(TestObject) - 1);

  return new (buf) TestObject(addr, value);
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

// Maximum legal retain count.
// 32-2 bits of extra retain count, plus 1 for the implicit retain.
const uint64_t maxRC = 1ULL << (32 - 2);

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
  ASSERT_DEATH(swift_retain(object),
               "object was retained too many times");
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
  ASSERT_DEATH(swift_nonatomic_retain(object),
               "object was retained too many times");
}


//////////////////////
// Object lifecycle //
//////////////////////

// FIXME: use the real WeakReference definition
namespace swift {

class WeakReference {
  uintptr_t value;
  
 public:
  void *getSideTable() {
    return (void*)(value & ~3ULL);
  }
};

}

// Lifecycle paths. One test each.
// 
// LIVE -> DEINITING                      -> DEAD, no side table
// LIVE -> DEINITING -> DEINITED          -> DEAD, no side table
// 
// LIVE -> DEINITING                      -> DEAD, with side table
// LIVE -> DEINITING -> DEINITED          -> DEAD, with side table
// LIVE -> DEINITING             -> FREED -> DEAD, with side table
// LIVE -> DEINITING -> DEINITED -> FREED -> DEAD, with side table


// LIVE -> DEINITING -> DEAD, no side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_no_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load can't happen
  // WRC increment adds side table which is tested elsewhere
  // WRC decrement can't happen

  // RC == 1
  // URC == 1
  // WRC == 1

  swift_release(object);  // DEINITING is in here
  
  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(object);
}


// LIVE -> DEINITING -> DEINITED -> DEAD, no side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_deinited_no_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load can't happen
  // WRC increment adds side table which is tested elsewhere
  // WRC decrement can't happen

  // RC == 1
  // URC == 3
  // WRC == 1

  swift_release(object);  // DEINITING is in here
  
  // Object is DEINITED
  // RC == 0
  // URC == 2
  // WRC == 1

  EXPECT_EQ(1u, deinited);
  EXPECT_ALLOCATED(object);

  // RC can't happen

  // WRC can't happen

  // URC load crashes
  // URC increment can't happen
  // URC decrement OK
  ASSERT_DEATH(swift_unownedCheck(object),
               "attempted to read an unowned reference");
  swift_unownedRelease(object);
  EXPECT_ALLOCATED(object);

  // RC == 0
  // URC == 1
  // WRC == 1

  swift_unownedRelease(object);
  
  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(object);
}


// LIVE -> DEINITING -> DEAD, with side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_with_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  // Remaining releases are performed after the side table is allocated.

  // WRC load can't happen
  // WRC increment adds side table
  // WRC decrement can't happen

  WeakReference w;
  swift_weakInit(&w, object);
  
  // Object is LIVE with side table

  void *side = w.getSideTable();
  EXPECT_ALLOCATED(side);

  WeakReference w_deinit;
  swift_weakInit(&w_deinit, object);
  object->WeakRef = &w_deinit;
  // destroyed during deinit
  
  // RC increment ok
  // RC decrement ok
  swift_retain(object);
  swift_retain(object);
  swift_retain(object);
  swift_release(object);
  swift_release(object);
  swift_release(object);
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  // ...and balancing from previously...
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load OK
  // WRC increment OK
  // WRC decrement OK

  WeakReference w2;
  swift_weakInit(&w2, object);
  HeapObject *weakValue = swift_weakTakeStrong(&w2);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);

  weakValue = swift_weakTakeStrong(&w);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);
  
  // RC == 1
  // URC == 1
  // WRC == 1

  swift_release(object);  // DEINITING is in here
  
  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(side);
  EXPECT_UNALLOCATED(object);
}


// LIVE -> DEINITING -> DEINITED -> DEAD, with side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_deinited_with_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  // Remaining releases are performed during DEINITED.

  // WRC load can't happen
  // WRC increment adds side table
  // WRC decrement can't happen

  WeakReference w;
  swift_weakInit(&w, object);
  
  // Object is LIVE with side table

  void *side = w.getSideTable();
  EXPECT_ALLOCATED(side);

  WeakReference w_deinit;
  swift_weakInit(&w_deinit, object);
  object->WeakRef = &w_deinit;
  // destroyed during deinit

  // RC increment ok
  // RC decrement ok
  swift_retain(object);
  swift_retain(object);
  swift_retain(object);
  swift_release(object);
  swift_release(object);
  swift_release(object);
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load OK
  // WRC increment OK
  // WRC decrement OK

  WeakReference w2;
  swift_weakInit(&w2, object);
  HeapObject *weakValue = swift_weakTakeStrong(&w2);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);

  weakValue = swift_weakTakeStrong(&w);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);
  
  // RC == 1
  // URC == 3
  // WRC == 1

  swift_release(object);  // DEINITING is in here

  // Object is DEINITED
  // RC == 0
  // URC == 2
  // WRC == 1
  
  EXPECT_EQ(1u, deinited);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);

  // RC can't happen

  // WRC can't happen

  // URC load crashes
  // URC increment can't happen
  // URC decrement OK
  ASSERT_DEATH(swift_unownedCheck(object),
               "attempted to read an unowned reference");
  swift_unownedRelease(object);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);
  
  // RC == 0
  // URC == 1
  // WRC == 1

  swift_unownedRelease(object);

  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(object);
  EXPECT_UNALLOCATED(side);
}


// LIVE -> DEINITING -> FREED -> DEAD, with side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_freed_with_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load can't happen
  // WRC increment adds side table
  // WRC decrement can't happen

  WeakReference w;
  swift_weakInit(&w, object);

  // Object is LIVE with side table
  
  void *side = w.getSideTable();
  EXPECT_ALLOCATED(side);

  WeakReference w_deinit;
  swift_weakInit(&w_deinit, object);
  object->WeakRef = &w_deinit;
  // destroyed during deinit

  // RC increment ok
  // RC decrement ok
  swift_retain(object);
  swift_retain(object);
  swift_retain(object);
  swift_release(object);
  swift_release(object);
  swift_release(object);
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load OK
  // WRC increment OK
  // WRC decrement OK

  WeakReference w2;
  swift_weakInit(&w2, object);
  HeapObject *weakValue = swift_weakLoadStrong(&w2);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);

  weakValue = swift_weakLoadStrong(&w);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);
  
  // RC == 1
  // URC == 1
  // WRC == 3

  swift_release(object);  // DEINITING is in here

  // Object is FREED
  // RC == 0
  // URC == 0
  // WRC == 2
  
  EXPECT_EQ(1u, deinited);
  EXPECT_UNALLOCATED(object);
  EXPECT_ALLOCATED(side);

  // RC can't happen

  // URC can't happen

  // WRC load is nil
  // WRC increment can't happen
  // WRC decrement OK

  weakValue = swift_weakTakeStrong(&w2);
  EXPECT_EQ(0, weakValue);
  
  // RC == 0
  // URC == 0
  // WRC == 1

  weakValue = swift_weakTakeStrong(&w);

  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(side);
  EXPECT_EQ(0, weakValue);
}


// LIVE -> DEINITING -> DEINITED -> FREED -> DEAD, with side table
TEST(LongRefcountingTest, lifecycle_live_deiniting_deinited_freed_with_side_DeathTest) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  object->CheckLifecycle = true;

  // Object is LIVE
  
  EXPECT_ALLOCATED(object);
  // RC tested elsewhere
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  // Remaining releases are performed during DEINITED.
  
  // WRC load can't happen
  // WRC increment adds side table
  // WRC decrement can't happen

  WeakReference w;
  swift_weakInit(&w, object);

  // Object is LIVE with side table

  void *side = w.getSideTable();
  EXPECT_ALLOCATED(side);
  
  WeakReference w_deinit;
  swift_weakInit(&w_deinit, object);
  object->WeakRef = &w_deinit;
  // destroyed during deinit  

  // RC increment ok
  // RC decrement ok
  swift_retain(object);
  swift_retain(object);
  swift_retain(object);
  swift_release(object);
  swift_release(object);
  swift_release(object);
  
  // URC load OK
  // URC increment OK
  // URC decrement OK
  swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRetain(object);   swift_unownedCheck(object);  
  swift_unownedRetain(object);   swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);
  swift_unownedRelease(object);  swift_unownedCheck(object);

  // WRC load OK
  // WRC increment OK
  // WRC decrement OK

  WeakReference w2;
  swift_weakInit(&w2, object);
  HeapObject *weakValue = swift_weakLoadStrong(&w2);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);

  weakValue = swift_weakLoadStrong(&w);
  EXPECT_EQ(weakValue, object);
  swift_release(weakValue);
  
  // RC == 1
  // URC == 3
  // WRC == 3

  swift_release(object);  // DEINITING is in here

  // Object is DEINITED
  // RC == 0
  // URC == 2
  // WRC == 3

  EXPECT_EQ(1u, deinited);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);

  // RC can't happen

  // WRC load is nil
  // WRC increment can't happen
  // WRC decrement OK

  weakValue = swift_weakTakeStrong(&w2);
  EXPECT_EQ(0, weakValue);

  // URC load crashes
  // URC increment can't happen
  // URC decrement OK
  ASSERT_DEATH(swift_unownedCheck(object),
               "attempted to read an unowned reference");
  swift_unownedRelease(object);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);

  // RC == 0
  // URC == 1
  // WRC == 2

  swift_unownedRelease(object);

  // Object is FREED
  // RC == 0
  // URC == 0
  // WRC == 1
  
  EXPECT_EQ(1u, deinited);
  EXPECT_UNALLOCATED(object);
  EXPECT_ALLOCATED(side);

  // RC can't happen

  // URC can't happen

  // WRC load is nil
  // WRC increment can't happen
  // WRC decrement OK
  
  // RC == 0
  // URC == 0
  // WRC == 1

  weakValue = swift_weakTakeStrong(&w);

  // Object is DEAD
  // RC == 0
  // URC == 0
  // WRC == 0

  EXPECT_UNALLOCATED(side);
  EXPECT_EQ(0, weakValue);
}
