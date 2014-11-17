//===--- RefCount.h -------------------------------------------------------===//
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
#ifndef SWIFT_STDLIB_SHIMS_REFCOUNT_H
#define SWIFT_STDLIB_SHIMS_REFCOUNT_H

#if !defined(__cplusplus)

// These definitions are placeholders for importing into Swift.
// They provide size and alignment but cannot be manipulated safely there.

#include "SwiftStdint.h"

typedef struct {
  __swift_uint32_t refCount __attribute__((unavailable));
} StrongRefCount;

typedef struct {
  __swift_uint32_t weakRefCount __attribute__((unavailable));
} WeakRefCount;

// not __cplusplus
#else
// __cplusplus

#include <type_traits>
#include <stdint.h>
#include <assert.h>

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

  // The low bit is the deallocating marker.
  // The remaining bits are the reference count.
  // refCount == RC_ONE means reference count == 1.
  enum : uint32_t {
    RC_DEALLOCATING_FLAG = 1,

    RC_FLAGS_COUNT = 1,
    RC_FLAGS_MASK = 1,
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


  // Decrement the reference count.
  // Return true if the caller should now deallocate the object.
  bool decrementShouldDeallocate() {
    uint32_t newval = __atomic_sub_fetch(&refCount, RC_ONE, __ATOMIC_RELEASE);
    if ((newval & (RC_COUNT_MASK | RC_DEALLOCATING_FLAG)) != 0) {
      // Refcount is not zero. We definitely do not need to deallocate.
      return false;
    }

    // Refcount is now 0 and is not already deallocating.
    // Try to set the deallocating flag.
    // This also performs the before-deinit acquire barrier if we set the flag.
    static_assert(RC_FLAGS_COUNT == 1,
                  "fix decrementShouldDeallocate() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&refCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
  }


  // Return the reference count.
  // During deallocation the reference count is undefined.
  uint32_t getCount() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
  }


  // Return true if the object is inside deallocation.
  bool isDeallocating() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) & RC_DEALLOCATING_FLAG;
  }
};


// Weak reference count.

class WeakRefCount {
  uint32_t refCount;

  enum : uint32_t {
    // There isn't really a flag here.
    // Making weak RC_ONE == strong RC_ONE saves an
    // instruction in allocation on arm64.
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


  // Increment the weak reference count.
  void increment() {
    uint32_t newval = __atomic_add_fetch(&refCount, RC_ONE, __ATOMIC_RELAXED);
    assert(newval >= RC_ONE  &&  "weak refcount overflow");
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


  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  uint32_t getCount() const {
    return __atomic_load_n(&refCount, __ATOMIC_RELAXED) >> RC_FLAGS_COUNT;
  }
};

#if !defined(__GLIBCXX__)
// Currently not implemented by libstdc++
static_assert(std::is_trivially_constructible<StrongRefCount>::value,
              "StrongRefCount must be trivially initializable");
static_assert(std::is_trivially_constructible<WeakRefCount>::value,
              "WeakRefCount must be trivially initializable");
#endif
static_assert(std::is_trivially_destructible<StrongRefCount>::value,
              "StrongRefCount must be trivially destructible");
static_assert(std::is_trivially_destructible<WeakRefCount>::value,
              "WeakRefCount must be trivially destructible");

// __cplusplus
#endif

#endif
