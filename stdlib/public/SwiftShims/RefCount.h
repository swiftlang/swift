//===--- RefCount.h ---------------------------------------------*- C++ -*-===//
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
#ifndef SWIFT_STDLIB_SHIMS_REFCOUNT_H
#define SWIFT_STDLIB_SHIMS_REFCOUNT_H

#if !defined(__cplusplus)

// These definitions are placeholders for importing into Swift.
// They provide size and alignment but cannot be manipulated safely there.

#include "SwiftStdint.h"

typedef struct {
  __swift_uint32_t refCount __attribute__((__unavailable__));
} StrongRefCount;

typedef struct {
  __swift_uint32_t weakRefCount __attribute__((__unavailable__));
} WeakRefCount;

// not __cplusplus
#else
// __cplusplus

#include <type_traits>
#include <stdint.h>
#include <assert.h>

#include "swift/Basic/type_traits.h"

// Strong reference count.

// Barriers
//
// Strong refcount increment is unordered with respect to other memory locations
//
// Strong refcount decrement is a release operation with respect to other
// memory locations. When an object's reference count becomes zero,
// an acquire fence is performed before beginning Swift deinit or ObjC
// dealloc code. This ensures that the deinit code sees all modifications
// of the object's contents that were made before the object was released.

class StrongRefCount {
  uint32_t refCount;

  // The low bit is the pinned marker.
  // The next bit is the deallocating marker.
  // The remaining bits are the reference count.
  // refCount == RC_ONE means reference count == 1.
  enum : uint32_t {
    RC_PINNED_FLAG = 0x1,
    RC_DEALLOCATING_FLAG = 0x2,

    RC_FLAGS_COUNT = 2,
    RC_FLAGS_MASK = 3,
    RC_COUNT_MASK = ~RC_FLAGS_MASK,

    RC_ONE = RC_FLAGS_MASK + 1
  };

  static_assert(RC_ONE == RC_DEALLOCATING_FLAG << 1,
                "deallocating bit must be adjacent to refcount bits");
  static_assert(RC_ONE == 1 << RC_FLAGS_COUNT,
                "inconsistent refcount flags");
  static_assert(RC_ONE == 1 + RC_FLAGS_MASK,
                "inconsistent refcount flags");

 public:
  enum Initialized_t { Initialized };

  // StrongRefCount must be trivially constructible to avoid ObjC++
  // destruction overhead at runtime. Use StrongRefCount(Initialized) to produce
  // an initialized instance.
  StrongRefCount() = default;
  
  // Refcount of a new object is 1.
  constexpr StrongRefCount(Initialized_t)
    : refCount(RC_ONE) { }

  void init() {
    refCount = RC_ONE;
  }

  // Increment the reference count.
  void increment() {
    __atomic_fetch_add(&refCount, RC_ONE, __ATOMIC_RELAXED);
  }

  void incrementNonAtomic() {
    uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    val += RC_ONE;
    __atomic_store_n(&refCount, val, __ATOMIC_RELAXED);
  }

  // Increment the reference count by n.
  void increment(uint32_t n) {
    __atomic_fetch_add(&refCount, n << RC_FLAGS_COUNT, __ATOMIC_RELAXED);
  }

  void incrementNonAtomic(uint32_t n) {
    uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    val += n << RC_FLAGS_COUNT;
    __atomic_store_n(&refCount, val, __ATOMIC_RELAXED);
 }

  // Try to simultaneously set the pinned flag and increment the
  // reference count.  If the flag is already set, don't increment the
  // reference count.
  //
  // This is only a sensible protocol for strictly-nested modifications.
  //
  // Returns true if the flag was set by this operation.
  //
  // Postcondition: the flag is set.
  bool tryIncrementAndPin() {
    uint32_t oldval = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    while (true) {
      // If the flag is already set, just fail.
      if (oldval & RC_PINNED_FLAG) {
        return false;
      }

      // Try to simultaneously set the flag and increment the reference count.
      uint32_t newval = oldval + (RC_PINNED_FLAG + RC_ONE);
      if (__atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                    __ATOMIC_RELAXED, __ATOMIC_RELAXED)) {
        return true;
      }

      // Try again; oldval has been updated with the value we saw.
    }
  }

  bool tryIncrementAndPinNonAtomic() {
    uint32_t oldval = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    // If the flag is already set, just fail.
    if (oldval & RC_PINNED_FLAG) {
      return false;
    }

    // Try to simultaneously set the flag and increment the reference count.
    uint32_t newval = oldval + (RC_PINNED_FLAG + RC_ONE);
    __atomic_store_n(&refCount, newval, __ATOMIC_RELAXED);
    return true;
  }

  // Increment the reference count, unless the object is deallocating.
  bool tryIncrement() {
    // FIXME: this could be better on LL/SC architectures like arm64
    uint32_t oldval = __atomic_fetch_add(&refCount, RC_ONE, __ATOMIC_RELAXED);
    if (oldval & RC_DEALLOCATING_FLAG) {
      __atomic_fetch_sub(&refCount, RC_ONE, __ATOMIC_RELAXED);
      return false;
    } else {
      return true;
    }
  }

  // Simultaneously clear the pinned flag and decrement the reference
  // count.
  //
  // Precondition: the pinned flag is set.
  bool decrementAndUnpinShouldDeallocate() {
    return doDecrementShouldDeallocate<true>();
  }

  bool decrementAndUnpinShouldDeallocateNonAtomic() {
    return doDecrementShouldDeallocateNonAtomic<true>();
  }

  // Decrement the reference count.
  // Return true if the caller should now deallocate the object.
  bool decrementShouldDeallocate() {
    return doDecrementShouldDeallocate<false>();
  }

  bool decrementShouldDeallocateNonAtomic() {
    return doDecrementShouldDeallocateNonAtomic<false>();
  }

  bool decrementShouldDeallocateN(uint32_t n) {
    return doDecrementShouldDeallocateN<false>(n);
  }

  // Set the RC_DEALLOCATING_FLAG flag non-atomically.
  //
  // Precondition: the reference count must be 1
  void decrementFromOneAndDeallocateNonAtomic() {
    assert(refCount == RC_ONE && "Expect a count of 1");
    __atomic_store_n(&refCount, RC_DEALLOCATING_FLAG, __ATOMIC_RELAXED);
  }

  bool decrementShouldDeallocateNNonAtomic(uint32_t n) {
    return doDecrementShouldDeallocateNNonAtomic<false>(n);
  }

  // Return the reference count.
  // During deallocation the reference count is undefined.
  uint32_t getCount() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
  }

  // Return whether the reference count is exactly 1.
  // During deallocation the reference count is undefined.
  bool isUniquelyReferenced() const {
    return getCount() == 1;
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set.  During deallocation the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const {
    auto value = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    // Rotating right by one sets the sign bit to the pinned bit. After
    // rotation, the dealloc flag is the least significant bit followed by the
    // reference count. A reference count of two or higher means that our value
    // is bigger than 3 if the pinned bit is not set. If the pinned bit is set
    // the value is negative.
    // Note: Because we are using the sign bit for testing pinnedness it
    // is important to do a signed comparison below.
    static_assert(RC_PINNED_FLAG == 1,
                  "The pinned flag must be the lowest bit");
    auto rotateRightByOne = ((value >> 1) | (value << 31));
    return (int32_t)rotateRightByOne < (int32_t)RC_ONE;
  }

  // Return true if the object is inside deallocation.
  bool isDeallocating() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) & RC_DEALLOCATING_FLAG;
  }

private:
  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocate() {
    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    constexpr uint32_t quantum =
      (ClearPinnedFlag ? RC_ONE + RC_PINNED_FLAG : RC_ONE);
    uint32_t newval = __atomic_sub_fetch(&refCount, quantum, __ATOMIC_RELEASE);

    assert((!ClearPinnedFlag || !(newval & RC_PINNED_FLAG)) &&
           "unpinning reference that was not pinned");
    assert(newval + quantum >= RC_ONE &&
           "releasing reference with a refcount of zero");

    // If we didn't drop the reference count to zero, or if the
    // deallocating flag is already set, we're done; don't start
    // deallocation.  We can assume that the pinned flag isn't set
    // unless the refcount is nonzero, and or'ing it in gives us a
    // more efficient mask: the check just becomes "is newval nonzero".
    if ((newval & (RC_COUNT_MASK | RC_PINNED_FLAG | RC_DEALLOCATING_FLAG))
          != 0) {
      // Refcount is not zero. We definitely do not need to deallocate.
      return false;
    }

    // Refcount is now 0 and is not already deallocating.  Try to set
    // the deallocating flag.  This must be atomic because it can race
    // with weak retains.
    //
    // This also performs the before-deinit acquire barrier if we set the flag.
    static_assert(RC_FLAGS_COUNT == 2,
                  "fix decrementShouldDeallocate() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateNonAtomic() {
    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    constexpr uint32_t quantum =
      (ClearPinnedFlag ? RC_ONE + RC_PINNED_FLAG : RC_ONE);
    uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    val -= quantum;
    __atomic_store_n(&refCount, val, __ATOMIC_RELEASE);
    uint32_t newval = refCount;

    assert((!ClearPinnedFlag || !(newval & RC_PINNED_FLAG)) &&
           "unpinning reference that was not pinned");
    assert(newval + quantum >= RC_ONE &&
           "releasing reference with a refcount of zero");

    // If we didn't drop the reference count to zero, or if the
    // deallocating flag is already set, we're done; don't start
    // deallocation.  We can assume that the pinned flag isn't set
    // unless the refcount is nonzero, and or'ing it in gives us a
    // more efficient mask: the check just becomes "is newval nonzero".
    if ((newval & (RC_COUNT_MASK | RC_PINNED_FLAG | RC_DEALLOCATING_FLAG))
          != 0) {
      // Refcount is not zero. We definitely do not need to deallocate.
      return false;
    }

    // Refcount is now 0 and is not already deallocating.  Try to set
    // the deallocating flag.  This must be atomic because it can race
    // with weak retains.
    //
    // This also performs the before-deinit acquire barrier if we set the flag.
    static_assert(RC_FLAGS_COUNT == 2,
                  "fix decrementShouldDeallocate() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateN(uint32_t n) {
    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    uint32_t delta = (n << RC_FLAGS_COUNT) + (ClearPinnedFlag ? RC_PINNED_FLAG : 0);
    uint32_t newval = __atomic_sub_fetch(&refCount, delta, __ATOMIC_RELEASE);

    assert((!ClearPinnedFlag || !(newval & RC_PINNED_FLAG)) &&
           "unpinning reference that was not pinned");
    assert(newval + delta >= RC_ONE &&
           "releasing reference with a refcount of zero");

    // If we didn't drop the reference count to zero, or if the
    // deallocating flag is already set, we're done; don't start
    // deallocation.  We can assume that the pinned flag isn't set
    // unless the refcount is nonzero, and or'ing it in gives us a
    // more efficient mask: the check just becomes "is newval nonzero".
    if ((newval & (RC_COUNT_MASK | RC_PINNED_FLAG | RC_DEALLOCATING_FLAG))
          != 0) {
      // Refcount is not zero. We definitely do not need to deallocate.
      return false;
    }

    // Refcount is now 0 and is not already deallocating.  Try to set
    // the deallocating flag.  This must be atomic because it can race
    // with weak retains.
    //
    // This also performs the before-deinit acquire barrier if we set the flag.
    static_assert(RC_FLAGS_COUNT == 2,
                  "fix decrementShouldDeallocate() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateNNonAtomic(uint32_t n) {
    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    uint32_t delta = (n << RC_FLAGS_COUNT) + (ClearPinnedFlag ? RC_PINNED_FLAG : 0);
    uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
    val -= delta;
    __atomic_store_n(&refCount, val, __ATOMIC_RELEASE);
    uint32_t newval = val;

    assert((!ClearPinnedFlag || !(newval & RC_PINNED_FLAG)) &&
           "unpinning reference that was not pinned");
    assert(newval + delta >= RC_ONE &&
           "releasing reference with a refcount of zero");

    // If we didn't drop the reference count to zero, or if the
    // deallocating flag is already set, we're done; don't start
    // deallocation.  We can assume that the pinned flag isn't set
    // unless the refcount is nonzero, and or'ing it in gives us a
    // more efficient mask: the check just becomes "is newval nonzero".
    if ((newval & (RC_COUNT_MASK | RC_PINNED_FLAG | RC_DEALLOCATING_FLAG))
          != 0) {
      // Refcount is not zero. We definitely do not need to deallocate.
      return false;
    }

    // Refcount is now 0 and is not already deallocating.  Try to set
    // the deallocating flag.  This must be atomic because it can race
    // with weak retains.
    //
    // This also performs the before-deinit acquire barrier if we set the flag.
    static_assert(RC_FLAGS_COUNT == 2,
                  "fix decrementShouldDeallocate() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
  }
};


// Weak reference count.

class WeakRefCount {
  uint32_t refCount;

  enum : uint32_t {
    // There isn't really a flag here.
    RC_UNUSED_FLAG = 1,

    RC_FLAGS_COUNT = 1,
    RC_FLAGS_MASK = 1,
    RC_COUNT_MASK = ~RC_FLAGS_MASK,

    RC_ONE = RC_FLAGS_MASK + 1
  };

  static_assert(RC_ONE == 1 << RC_FLAGS_COUNT,
                "inconsistent refcount flags");
  static_assert(RC_ONE == 1 + RC_FLAGS_MASK,
                "inconsistent refcount flags");

 public:
  enum Initialized_t { Initialized };

  // WeakRefCount must be trivially constructible to avoid ObjC++
  // destruction overhead at runtime. Use WeakRefCount(Initialized) to produce
  // an initialized instance.
  WeakRefCount() = default;
  
  // Weak refcount of a new object is 1.
  constexpr WeakRefCount(Initialized_t)
    : refCount(RC_ONE) { }

  void init() {
    refCount = RC_ONE;
  }

  /// Initialize for a stack promoted object. This prevents that the final
  /// release frees the memory of the object.
  void initForNotDeallocating() {
    refCount = RC_ONE + RC_ONE;
  }

  // Increment the weak reference count.
  void increment() {
    uint32_t newval = __atomic_add_fetch(&refCount, RC_ONE, __ATOMIC_RELAXED);
    assert(newval >= RC_ONE  &&  "weak refcount overflow");
    (void)newval;
  }

  /// Increment the weak reference count by n.
  void increment(uint32_t n) {
    uint32_t addval = (n << RC_FLAGS_COUNT);
    uint32_t newval = __atomic_add_fetch(&refCount, addval, __ATOMIC_RELAXED);
    assert(newval >= addval  &&  "weak refcount overflow");
    (void)newval;
  }

  // Decrement the weak reference count.
  // Return true if the caller should deallocate the object.
  bool decrementShouldDeallocate() {
    uint32_t oldval = __atomic_fetch_sub(&refCount, RC_ONE, __ATOMIC_RELAXED);
    assert(oldval >= RC_ONE  &&  "weak refcount underflow");

    // Should dealloc if count was 1 before decrementing (i.e. it is zero now)
    return (oldval & RC_COUNT_MASK) == RC_ONE;
  }

  /// Decrement the weak reference count.
  /// Return true if the caller should deallocate the object.
  bool decrementShouldDeallocateN(uint32_t n) {
    uint32_t subval = (n << RC_FLAGS_COUNT);
    uint32_t oldval = __atomic_fetch_sub(&refCount, subval, __ATOMIC_RELAXED);
    assert(oldval >= subval  &&  "weak refcount underflow");

    // Should dealloc if count was subval before decrementing (i.e. it is zero now)
    return (oldval & RC_COUNT_MASK) == subval;
  }

  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  uint32_t getCount() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
  }
};

static_assert(swift::IsTriviallyConstructible<StrongRefCount>::value,
              "StrongRefCount must be trivially initializable");
static_assert(swift::IsTriviallyConstructible<WeakRefCount>::value,
              "WeakRefCount must be trivially initializable");
static_assert(std::is_trivially_destructible<StrongRefCount>::value,
              "StrongRefCount must be trivially destructible");
static_assert(std::is_trivially_destructible<WeakRefCount>::value,
              "WeakRefCount must be trivially destructible");

// __cplusplus
#endif

#endif
