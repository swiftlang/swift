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
  void increment();

  void incrementNonAtomic();

  // Increment the reference count by n.
  void increment(uint32_t n);

  void incrementNonAtomic(uint32_t n);

  // Try to simultaneously set the pinned flag and increment the
  // reference count.  If the flag is already set, don't increment the
  // reference count.
  //
  // This is only a sensible protocol for strictly-nested modifications.
  //
  // Returns true if the flag was set by this operation.
  //
  // Postcondition: the flag is set.
  bool tryIncrementAndPin();

  bool tryIncrementAndPinNonAtomic();

  // Increment the reference count, unless the object is deallocating.
  bool tryIncrement();

  // Simultaneously clear the pinned flag and decrement the reference
  // count.
  //
  // Precondition: the pinned flag is set.
  bool decrementAndUnpinShouldDeallocate();

  bool decrementAndUnpinShouldDeallocateNonAtomic();

  // Decrement the reference count.
  // Return true if the caller should now deallocate the object.
  bool decrementShouldDeallocate();

  bool decrementShouldDeallocateNonAtomic();

  bool decrementShouldDeallocateN(uint32_t n);

  // Set the RC_DEALLOCATING_FLAG flag non-atomically.
  //
  // Precondition: the reference count must be 1
  void decrementFromOneAndDeallocateNonAtomic();

  bool decrementShouldDeallocateNNonAtomic(uint32_t n);

  // Return the reference count.
  // During deallocation the reference count is undefined.
  uint32_t getCount() const;

  // Return whether the reference count is exactly 1.
  // During deallocation the reference count is undefined.
  bool isUniquelyReferenced() const {
    return getCount() == 1;
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set.  During deallocation the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const;

  // Return true if the object is inside deallocation.
  bool isDeallocating() const;

private:
  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocate();

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateNonAtomic();

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateN(uint32_t n);

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeallocateNNonAtomic(uint32_t n);
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
  void increment();

  /// Increment the weak reference count by n.
  void increment(uint32_t n);

  // Decrement the weak reference count.
  // Return true if the caller should deallocate the object.
  bool decrementShouldDeallocate();

  /// Decrement the weak reference count.
  /// Return true if the caller should deallocate the object.
  bool decrementShouldDeallocateN(uint32_t n);

  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  uint32_t getCount() const;
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
