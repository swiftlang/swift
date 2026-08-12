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

#include <algorithm>
#include <cstdint>
#include <functional>

#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Demangling/ManglingMacros.h"
#include "llvm/ADT/bit.h"
#include "llvm/Support/MathExtras.h"
#include "gtest/gtest.h"

#ifdef __APPLE__
// FIXME: is EXPECT_UNALLOCATED reliable enough for CI?
// EXPECT_ALLOCATED may fail falsely if the memory is re-allocated.
# include <malloc/malloc.h>
# define EXPECT_ALLOCATED(p)    EXPECT_NE(0u, malloc_size(p))
# define EXPECT_UNALLOCATED(p)  EXPECT_EQ(0u, malloc_size(p))
#else
// FIXME: heap assertion for other platforms?
# define EXPECT_ALLOCATED(p) (void)(p)
# define EXPECT_UNALLOCATED(p) (void)(p)
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
  
  // Callback invoked during the object's deinit.
  std::function<void()> DeinitCallback;

  TestObject(size_t *addr, size_t value)
    : Addr(addr), Value(value), CheckLifecycle(false), WeakRef(nullptr), DeinitCallback(nullptr)
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
                 "Attempted to read an unowned reference");
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

  if (object->DeinitCallback != nullptr) {
    object->DeinitCallback();
  }

  *object->Addr = object->Value;
  object->Addr = nullptr;
  object->~TestObject();
  swift_deallocObject(object, sizeof(TestObject), alignof(TestObject) - 1);
}

static const FullMetadata<ClassMetadata> TestClassObjectMetadata = {
  { { nullptr }, { &deinitTestObject }, { &VALUE_WITNESS_SYM(Bo) } },
  { { nullptr }, ClassFlags::UsesSwiftRefcounting, 0, 0, 0, 0, 0, 0 }
};

/// Create an object that, when deinited, stores the given value to
/// the given pointer.
static TestObject *allocTestObject(size_t *addr, size_t value) {
  auto buf = swift_allocObject(&TestClassObjectMetadata,
                               sizeof(TestObject),
                               alignof(TestObject) - 1);

  return new (buf) TestObject(addr, value);
}


///////////////////////////////////////////////////
// Max strong retain count and overflow checking //
///////////////////////////////////////////////////

// Perform most of a range with a single _n operation, switching to individual
// operations within this many steps of a power of two. Inline counts overflow
// into the side table near a power of two.
static const uint64_t singleStepWindow = 4;

// Operation indices count from 1. The indices within singleStepWindow of a
// power of two are performed one at a time; the rest are performed in bulk.
static bool isSingleStep(uint64_t index) {
  return index - llvm::bit_floor(index) <= singleStepWindow ||
         llvm::NextPowerOf2(index) - index <= singleStepWindow;
}

// The smallest single-step index greater than `index`, which must not itself be
// a single-step index.
static uint64_t nextSingleStep(uint64_t index) {
  assert(!isSingleStep(index));
  return llvm::NextPowerOf2(index) - singleStepWindow;
}

// The largest single-step index less than `index`, which must not itself be a
// single-step index.
static uint64_t previousSingleStep(uint64_t index) {
  assert(!isSingleStep(index));
  return llvm::bit_floor(index) + singleStepWindow;
}

// Perform `count` operations on `object`, counting upwards. `single` performs
// one operation and `bulk` performs n at once. `check` runs after each of them.
template <typename Object, typename SingleOp, typename BulkOp, typename CheckOp>
static void runUpward(Object *object, uint64_t count,
                      SingleOp single, BulkOp bulk, CheckOp check) {
  for (uint64_t done = 0; done < count; ) {
    uint64_t n = 1;
    if (!isSingleStep(done + 1))
      n = std::min(nextSingleStep(done + 1) - 1, count) - done;

    if (n == 1)
      single(object);
    else
      bulk(object, static_cast<uint32_t>(n));
    check(object);

    done += n;
  }
}

// Perform `count` operations on `object`, counting downwards, covering the same
// indices one at a time as runUpward does.
template <typename Object, typename SingleOp, typename BulkOp, typename CheckOp>
static void runDownward(Object *object, uint64_t count,
                        SingleOp single, BulkOp bulk, CheckOp check) {
  for (uint64_t remaining = count; remaining > 0; ) {
    uint64_t n = 1;
    if (!isSingleStep(remaining))
      n = remaining - previousSingleStep(remaining);

    if (n == 1)
      single(object);
    else
      bulk(object, static_cast<uint32_t>(n));
    check(object);

    remaining -= n;
  }
}

// A `check` for run{Upward,Downward} that checks nothing.
static void checkNothing(const void *) {}

template <bool atomic>
static void retainALot(TestObject *object, size_t &deinited,
                       uint64_t count) {
  auto check = [&](TestObject *) { EXPECT_EQ(0u, deinited); };
  if (atomic)
    runUpward(object, count, swift_retain, swift_retain_n, check);
  else
    runUpward(object, count, swift_nonatomic_retain, swift_nonatomic_retain_n,
              check);
}

template <bool atomic>
static void releaseALot(TestObject *object, size_t &deinited,
                        uint64_t count) {
  auto check = [&](TestObject *) { EXPECT_EQ(0u, deinited); };
  if (atomic)
    runDownward(object, count, swift_release, swift_release_n, check);
  else
    runDownward(object, count, swift_nonatomic_release,
                swift_nonatomic_release_n, check);
}

// Maximum legal retain count.
// 32-2 bits of extra retain count, plus 1 for the implicit retain.
const uint64_t maxRC = 1ULL << (32 - 2);

TEST(LongRefcountingTest, retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  EXPECT_EQ(swift_retainCount(object), 1u);
  retainALot<true>(object, deinited, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  releaseALot<true>(object, deinited, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(0u, deinited);
  swift_release(object);
  EXPECT_EQ(1u, deinited);
}

TEST(LongRefcountingTest, retain_overflow_DeathTest) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow error.
  retainALot<true>(object, deinited, maxRC - 1);
  EXPECT_EQ(0u, deinited);
  ASSERT_DEATH(swift_retain(object),
               "Object was retained too many times");
}

TEST(LongRefcountingTest, nonatomic_retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1.
  // Retain to maxRC, release back to 1, then release and verify deallocation.
  EXPECT_EQ(swift_retainCount(object), 1u);
  retainALot<false>(object, deinited, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), maxRC);
  releaseALot<false>(object, deinited, maxRC - 1);
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(0u, deinited);
  swift_nonatomic_release(object);
  EXPECT_EQ(1u, deinited);
}

TEST(LongRefcountingTest, nonatomic_retain_overflow_DeathTest) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. Retain to maxRC, then retain again and verify overflow error.
  retainALot<false>(object, deinited, maxRC - 1);
  EXPECT_EQ(0u, deinited);
  ASSERT_DEATH(swift_nonatomic_retain(object),
               "Object was retained too many times");
}


///////////////////////////////////////////////////
// Max unowned retain count and overflow checking //
///////////////////////////////////////////////////

static void checkAllocated(TestObject *object) {
  EXPECT_ALLOCATED(object);
}

template <bool atomic>
static void unownedRetainALot(TestObject *object, uint64_t count) {
  if (atomic)
    runUpward(object, count, swift_unownedRetain, swift_unownedRetain_n,
              checkAllocated);
  else
    runUpward(object, count, swift_nonatomic_unownedRetain,
              swift_nonatomic_unownedRetain_n, checkAllocated);
}

template <bool atomic>
static void unownedReleaseALot(TestObject *object, uint64_t count) {
  if (atomic)
    runDownward(object, count, swift_unownedRelease, swift_unownedRelease_n,
                checkAllocated);
  else
    runDownward(object, count, swift_nonatomic_unownedRelease,
                swift_nonatomic_unownedRelease_n, checkAllocated);
}

// Maximum legal unowned retain count. 31 bits minus one with no implicit +1.
const uint64_t maxURC = (1ULL << (32 - 1)) - 2;

TEST(LongRefcountingTest, unowned_retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. URC is 1.
  // Unowned-retain to maxURC.
  // Release and verify deinited and not deallocated.
  // Unowned-release back to 1, then unowned-release and verify deallocated.
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(swift_unownedRetainCount(object), 1u);
  unownedRetainALot<true>(object, maxURC - 1);
  EXPECT_EQ(swift_unownedRetainCount(object), maxURC);

  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  swift_release(object);
  EXPECT_EQ(1u, deinited);
  EXPECT_ALLOCATED(object);
  // Strong release decremented unowned count by 1.
  EXPECT_EQ(swift_unownedRetainCount(object), maxURC - 1);

  unownedReleaseALot<true>(object, maxURC - 2);
  EXPECT_EQ(swift_unownedRetainCount(object), 1u);

  EXPECT_ALLOCATED(object);
  swift_unownedRelease(object);
  EXPECT_UNALLOCATED(object);
}

TEST(LongRefcountingTest, unowned_retain_overflow_DeathTest) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // URC is 1. Retain to maxURC, then retain again and verify overflow error.
  unownedRetainALot<true>(object, maxURC - 1);
  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  ASSERT_DEATH(swift_unownedRetain(object),
               "Object's unowned reference was retained too many times");
}

TEST(LongRefcountingTest, nonatomic_unowned_retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. URC is 1.
  // Unowned-retain to maxURC.
  // Release and verify deinited and not deallocated.
  // Unowned-release back to 1, then unowned-release and verify deallocated.
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(swift_unownedRetainCount(object), 1u);
  unownedRetainALot<false>(object, maxURC - 1);
  EXPECT_EQ(swift_unownedRetainCount(object), maxURC);

  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  swift_release(object);
  EXPECT_EQ(1u, deinited);
  EXPECT_ALLOCATED(object);
  // Strong release decremented unowned count by 1.
  EXPECT_EQ(swift_unownedRetainCount(object), maxURC - 1);

  unownedReleaseALot<false>(object, maxURC - 2);
  EXPECT_EQ(swift_unownedRetainCount(object), 1u);

  EXPECT_ALLOCATED(object);
  swift_unownedRelease(object);
  EXPECT_UNALLOCATED(object);
}

TEST(LongRefcountingTest, nonatomic_unowned_retain_overflow_DeathTest) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // URC is 1. Retain to maxURC, then retain again and verify overflow error.
  unownedRetainALot<false>(object, maxURC - 1);
  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  ASSERT_DEATH(swift_nonatomic_unownedRetain(object),
               "Object's unowned reference was retained too many times");
}


/////////////////////////////////////////////////
// Max weak retain count and overflow checking //
/////////////////////////////////////////////////

static void weakRetain(HeapObjectSideTableEntry *side) {
  (void)side->incrementWeak();
}

static void weakRetain_n(HeapObjectSideTableEntry *side, uint32_t n) {
  (void)side->incrementWeak(n);
}

static void weakRelease(HeapObjectSideTableEntry *side) {
  side->decrementWeak();
}

static void weakRelease_n(HeapObjectSideTableEntry *side, uint32_t n) {
  side->decrementWeak(n);
}

static void weakReleaseNonAtomic(HeapObjectSideTableEntry *side) {
  side->decrementWeakNonAtomic();
}

static void weakReleaseNonAtomic_n(HeapObjectSideTableEntry *side, uint32_t n) {
  side->decrementWeakNonAtomic(n);
}

static HeapObjectSideTableEntry *weakRetainALot(TestObject *object, uint64_t count) {
  if (count == 0) return nullptr;

  auto side = object->refCounts.formWeakReference();
  runUpward(side, count - 1, weakRetain, weakRetain_n,
            [&](HeapObjectSideTableEntry *) { EXPECT_ALLOCATED(object); });
  return side;
}

template <bool atomic>
static void weakReleaseALot(HeapObjectSideTableEntry *side, uint64_t count) {
  if (atomic)
    runDownward(side, count, weakRelease, weakRelease_n, checkNothing);
  else
    runDownward(side, count, weakReleaseNonAtomic, weakReleaseNonAtomic_n,
                checkNothing);
}

// Maximum legal weak retain count. 32 bits with no implicit +1.
const uint64_t maxWRC = (1ULL << 32) - 1;

TEST(LongRefcountingTest, weak_retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. WRC is 1.
  // Weak-retain to maxWRC.
  // Release and verify deallocated object and live side table.
  // Weak-release back to 1, then weak-release and verify deallocated.
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(object->refCounts.getWeakCount(), 1u);
  auto side = weakRetainALot(object, maxWRC - 1);
  EXPECT_EQ(side->getWeakCount(), maxWRC);
  
  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);
  swift_release(object);
  EXPECT_EQ(1u, deinited);
  EXPECT_UNALLOCATED(object);
  EXPECT_ALLOCATED(side);

  weakReleaseALot<true>(side, maxWRC - 2);
  EXPECT_EQ(side->getWeakCount(), 1u);

  EXPECT_ALLOCATED(side);
  side->decrementWeak();
  EXPECT_UNALLOCATED(side);
}

TEST(LongRefcountingTest, weak_retain_overflow_DeathTest) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // URC is 1. Retain to maxURC, then retain again and verify overflow error.
  weakRetainALot(object, maxWRC - 1);
  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  ASSERT_DEATH(weakRetainALot(object, 1),
               "Object's weak reference was retained too many times");
}

TEST(LongRefcountingTest, nonatomic_weak_retain_max) {
  // Don't generate millions of failures if something goes wrong.
  ::testing::FLAGS_gtest_break_on_failure = true;

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);

  // RC is 1. WRC is 1.
  // Weak-retain to maxWRC.
  // Release and verify deallocated object and live side table.
  // Weak-release back to 1, then weak-release and verify deallocated.
  EXPECT_EQ(swift_retainCount(object), 1u);
  EXPECT_EQ(object->refCounts.getWeakCount(), 1u);
  auto side = weakRetainALot(object, maxWRC - 1);
  EXPECT_EQ(side->getWeakCount(), maxWRC);
  
  EXPECT_EQ(0u, deinited);
  EXPECT_ALLOCATED(object);
  EXPECT_ALLOCATED(side);
  swift_release(object);
  EXPECT_EQ(1u, deinited);
  EXPECT_UNALLOCATED(object);
  EXPECT_ALLOCATED(side);

  weakReleaseALot<false>(side, maxWRC - 2);
  EXPECT_EQ(side->getWeakCount(), 1u);

  EXPECT_ALLOCATED(side);
  side->decrementWeak();
  EXPECT_UNALLOCATED(side);
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

} // namespace swift

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
               "Attempted to read an unowned reference");
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
               "Attempted to read an unowned reference");
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
               "Attempted to read an unowned reference");
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

TEST(LongRefcountingTest, lifecycle_live_deiniting_urc_overflow_to_side) {
  ::testing::FLAGS_gtest_death_test_style = "threadsafe";

  uint64_t urcOverflowCount;
  switch(sizeof(void *)) {
  // 32-bit has a 7-bit inline refcount that overflows into the side table.
  case 4: urcOverflowCount = 1 << 7; break;

  // 64-bit can't store any extra count in the side table, so there's nothing to test.
  case 8: return;

  // We should never see any other bitness.
  default: FAIL(); break;
  }

  size_t deinited = 0;
  auto object = allocTestObject(&deinited, 1);
  HeapObjectSideTableEntry *side = nullptr;
  object->DeinitCallback = [&]() {
    for (uint64_t i = 0; i < urcOverflowCount; i++) {
      swift_unownedRetain(object);
    }

    side = reinterpret_cast<HeapObjectSideTableEntry *>(object->refCounts.getSideTable());
    EXPECT_ALLOCATED(side);

    for (uint64_t i = 0; i < urcOverflowCount; i++) {
      swift_unownedRelease(object);
    }
  };

  swift_release(object);
  EXPECT_UNALLOCATED(object);
  EXPECT_UNALLOCATED(side);
}
