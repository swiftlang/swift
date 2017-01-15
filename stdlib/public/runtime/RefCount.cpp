//===--- RefCount.cpp - Swift Language Reference Counting Support ---------===//
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
//
// Reference Counting Shims While the Language is Bootstrapped
//
//===----------------------------------------------------------------------===//

#include "../../../stdlib/public/SwiftShims/HeapObject.h"

#include <type_traits>
#include <stdint.h>
#include <assert.h>

#include "swift/Basic/type_traits.h"

void StrongRefCount::increment() {
  __atomic_fetch_add(&refCount, RC_ONE, __ATOMIC_RELAXED);
}

void StrongRefCount::incrementNonAtomic() {
  uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
  val += RC_ONE;
  __atomic_store_n(&refCount, val, __ATOMIC_RELAXED);
}

void StrongRefCount::increment(uint32_t n) {
  __atomic_fetch_add(&refCount, n << RC_FLAGS_COUNT, __ATOMIC_RELAXED);
}

void StrongRefCount::incrementNonAtomic(uint32_t n) {
  uint32_t val = __atomic_load_n(&refCount, __ATOMIC_RELAXED);
  val += n << RC_FLAGS_COUNT;
  __atomic_store_n(&refCount, val, __ATOMIC_RELAXED);
}

bool StrongRefCount::tryIncrementAndPin() {
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

bool StrongRefCount::tryIncrementAndPinNonAtomic() {
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

bool StrongRefCount::tryIncrement() {
  // FIXME: this could be better on LL/SC architectures like arm64
  uint32_t oldval = __atomic_fetch_add(&refCount, RC_ONE, __ATOMIC_RELAXED);
  if (oldval & RC_DEALLOCATING_FLAG) {
    __atomic_fetch_sub(&refCount, RC_ONE, __ATOMIC_RELAXED);
    return false;
  } else {
    return true;
  }
}

bool StrongRefCount::decrementAndUnpinShouldDeallocate() {
  return doDecrementShouldDeallocate<true>();
}

bool StrongRefCount::decrementAndUnpinShouldDeallocateNonAtomic() {
  return doDecrementShouldDeallocateNonAtomic<true>();
}

bool StrongRefCount::decrementShouldDeallocate() {
  return doDecrementShouldDeallocate<false>();
}

bool StrongRefCount::decrementShouldDeallocateNonAtomic() {
  return doDecrementShouldDeallocateNonAtomic<false>();
}

bool StrongRefCount::decrementShouldDeallocateN(uint32_t n) {
  return doDecrementShouldDeallocateN<false>(n);
}
  
void StrongRefCount::decrementFromOneAndDeallocateNonAtomic() {
  assert(refCount == RC_ONE && "Expect a count of 1");
  __atomic_store_n(&refCount, RC_DEALLOCATING_FLAG, __ATOMIC_RELAXED);
}

bool StrongRefCount::decrementShouldDeallocateNNonAtomic(uint32_t n) {
  return doDecrementShouldDeallocateNNonAtomic<false>(n);
}

uint32_t StrongRefCount::getCount() const {
  return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
}

bool StrongRefCount::isUniquelyReferencedOrPinned() const {
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

bool StrongRefCount::isDeallocating() const {
  return __atomic_load_n(&refCount, __ATOMIC_RELAXED) & RC_DEALLOCATING_FLAG;
}

template <bool ClearPinnedFlag>
bool StrongRefCount::doDecrementShouldDeallocate() {
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
bool StrongRefCount::doDecrementShouldDeallocateNonAtomic() {
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
bool StrongRefCount::doDecrementShouldDeallocateN(uint32_t n) {
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
bool StrongRefCount::doDecrementShouldDeallocateNNonAtomic(uint32_t n) {
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

void WeakRefCount::increment() {
  uint32_t newval = __atomic_add_fetch(&refCount, RC_ONE, __ATOMIC_RELAXED);
  assert(newval >= RC_ONE  &&  "weak refcount overflow");
  (void)newval;
}

void WeakRefCount::increment(uint32_t n) {
  uint32_t addval = (n << RC_FLAGS_COUNT);
  uint32_t newval = __atomic_add_fetch(&refCount, addval, __ATOMIC_RELAXED);
  assert(newval >= addval  &&  "weak refcount overflow");
  (void)newval;
}

// Decrement the weak reference count.
// Return true if the caller should deallocate the object.
bool WeakRefCount::decrementShouldDeallocate() {
  uint32_t oldval = __atomic_fetch_sub(&refCount, RC_ONE, __ATOMIC_RELAXED);
  assert(oldval >= RC_ONE  &&  "weak refcount underflow");

  // Should dealloc if count was 1 before decrementing (i.e. it is zero now)
  return (oldval & RC_COUNT_MASK) == RC_ONE;
}

/// Decrement the weak reference count.
/// Return true if the caller should deallocate the object.
bool WeakRefCount::decrementShouldDeallocateN(uint32_t n) {
  uint32_t subval = (n << RC_FLAGS_COUNT);
  uint32_t oldval = __atomic_fetch_sub(&refCount, subval, __ATOMIC_RELAXED);
  assert(oldval >= subval  &&  "weak refcount underflow");

  // Should dealloc if count was subval before decrementing (i.e. it is zero now)
  return (oldval & RC_COUNT_MASK) == subval;
}

// Return weak reference count.
// Note that this is not equal to the number of outstanding weak pointers.
uint32_t WeakRefCount::getCount() const {
  return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
}
