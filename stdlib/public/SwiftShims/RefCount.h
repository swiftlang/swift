//===--- RefCount.h ---------------------------------------------*- C++ -*-===//
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
#ifndef SWIFT_STDLIB_SHIMS_REFCOUNT_H
#define SWIFT_STDLIB_SHIMS_REFCOUNT_H

#if !defined(__cplusplus)

// These definitions are placeholders for importing into Swift.
// They provide size and alignment but cannot be manipulated safely there.

#include "SwiftStdint.h"

typedef struct {
  __swift_uint64_t refCounts __attribute__((__unavailable__));
} InlineRefCounts;

// not __cplusplus
#else
// __cplusplus

#include <type_traits>
#include <atomic>
#include <stdint.h>
#include <assert.h>

#include "llvm/Support/Compiler.h"
#include "swift/Basic/type_traits.h"

#define relaxed std::memory_order_relaxed
#define acquire std::memory_order_acquire
#define release std::memory_order_release

// Basic encoding of refcount and flag data into the object's header.
// FIXME: Specialize this for a 32-bit field on 32-bit architectures.
class InlineRefCountBits {
  uint64_t bits;

  // Layout of bits.
  // field value = (bits & mask) >> shift
  
# define MaskForField(name) (((1UL<<name##BitCount)-1) << name##Shift)
# define ShiftAfterField(name) (name##Shift + name##BitCount)

  enum : uint64_t {
    UnownedRefCountShift = 0,
    UnownedRefCountBitCount = 32,
    UnownedRefCountMask = MaskForField(UnownedRefCount),

    IsPinnedShift = ShiftAfterField(UnownedRefCount), 
    IsPinnedBitCount = 1, 
    IsPinnedMask = MaskForField(IsPinned),

    IsDeinitingShift = ShiftAfterField(IsPinned),
    IsDeinitingBitCount = 1,
    IsDeinitingMask = MaskForField(IsDeiniting),

    StrongRefCountShift = ShiftAfterField(IsDeiniting),
    StrongRefCountBitCount = 28,
    StrongRefCountMask = MaskForField(StrongRefCount),

    DiscriminatorShift = ShiftAfterField(StrongRefCount),
    DiscriminatorBitCount = 2,
    DiscriminatorMask = MaskForField(Discriminator),

    SideTableShift = 0,
    SideTableBitCount = 62,
    SideTableMask = MaskForField(SideTable)
  };

  static_assert(StrongRefCountShift == IsDeinitingShift + 1, 
                "IsDeiniting must be LSB-wards of StrongRefCount");
  static_assert(DiscriminatorShift + DiscriminatorBitCount == sizeof(bits)*8,
                "Discriminator must be MSB");
  static_assert(SideTableBitCount + DiscriminatorBitCount == sizeof(bits)*8,
               "wrong bit count for InlineRefCountBits side table version");
  static_assert(UnownedRefCountBitCount + IsPinnedBitCount +
                IsDeinitingBitCount + StrongRefCountBitCount +
                DiscriminatorBitCount == sizeof(bits)*8,
                "wrong bit count for InlineRefCountBits refcount version");
# undef MaskForField
# undef ShiftAfterField

# define GetField(name) \
    ((bits & name##Mask) >> name##Shift)
# define SetField(name, val) \
  bits = (bits & ~name##Mask) | (((uint64_t(val) << name##Shift) & name##Mask))

  enum class DiscriminatorValue {
    /* 00 */ Normal = 0,
    /* 01 */ Unused = 1,
    /* 10 */ HasSideTable = 2,
    /* 11 */ Unusual = 3, 
  };

  // InlineRefCountBits uses always_inline everywhere
  // to improve performance of debug builds.
  
 private:
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  DiscriminatorValue getDiscriminator() const {
    return DiscriminatorValue(GetField(Discriminator));
  }

 public:

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  InlineRefCountBits() = default;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  constexpr InlineRefCountBits(uint32_t strongCount, uint32_t unownedCount)
    : bits((uint64_t(strongCount)  <<  StrongRefCountShift) |
           (uint64_t(unownedCount) << UnownedRefCountShift))
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool hasSideTable() const {
    return getDiscriminator() == DiscriminatorValue::HasSideTable;
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uintptr_t getSideTable() const {
    assert(hasSideTable());
    // Stored value is a shifted pointer.
    return uintptr_t(GetField(SideTable)) << DiscriminatorBitCount;
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uint32_t getUnownedRefCount() const {
    assert(!hasSideTable());
    return uint32_t(GetField(UnownedRefCount));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getIsPinned() const {
    assert(!hasSideTable());
    return bool(GetField(IsPinned));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getIsDeiniting() const {
    assert(!hasSideTable());
    return bool(GetField(IsDeiniting));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uint32_t getStrongRefCount() const {
    assert(!hasSideTable());
    return uint32_t(GetField(StrongRefCount));
  }


  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setHasSideTable(bool value) {
    bits = 0;
    SetField(Discriminator,
             value ? uint32_t(DiscriminatorValue::HasSideTable)
                   : uint32_t(DiscriminatorValue::Normal));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setSideTable(uintptr_t value) {
    assert(hasSideTable());
    // Stored value is a shifted pointer
    uintptr_t storedValue = value >> DiscriminatorShift;
    assert(storedValue << DiscriminatorShift == value);
    SetField(SideTable, storedValue);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setUnownedRefCount(uint32_t value) {
    assert(!hasSideTable());
    SetField(UnownedRefCount, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setIsPinned(bool value) {
    assert(!hasSideTable());
    SetField(IsPinned, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setIsDeiniting(bool value) {
    assert(!hasSideTable());
    SetField(IsDeiniting, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setStrongRefCount(uint32_t value) {
    assert(!hasSideTable());
    SetField(StrongRefCount, value);
  }
  

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void incrementStrongRefCount(uint32_t inc = 1) {
    setStrongRefCount(getStrongRefCount() + inc);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void incrementUnownedRefCount(uint32_t inc = 1) {
    setUnownedRefCount(getUnownedRefCount() + inc);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementStrongRefCount(uint32_t dec = 1) {    
    setStrongRefCount(getStrongRefCount() - dec);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementUnownedRefCount(uint32_t dec = 1) {
    setUnownedRefCount(getUnownedRefCount() - dec);
  }
  
# undef GetField
# undef SetField
};

// Barriers
//
// Strong refcount increment is unordered with respect to other memory locations
//
// Strong refcount decrement is a release operation with respect to other
// memory locations. When an object's reference count becomes zero,
// an acquire fence is performed before beginning Swift deinit or ObjC
// -dealloc code. This ensures that the deinit code sees all modifications
// of the object's contents that were made before the object was released.

class InlineRefCounts {
  std::atomic<InlineRefCountBits> refCounts;

 public:
  enum Initialized_t { Initialized };

  // InlineRefCounts must be trivially constructible to avoid ObjC++
  // destruction overhead at runtime. Use InlineRefCounts(Initialized)
  // to produce an initialized instance.
  InlineRefCounts() = default;
  
  // Refcount of a new object is 1.
  constexpr InlineRefCounts(Initialized_t)
    : refCounts(InlineRefCountBits(1, 1)) { }

  void init() {
    refCounts.store(InlineRefCountBits(1, 1), relaxed);
  }

  /// Initialize for a stack promoted object. This prevents that the final
  /// release frees the memory of the object.
  void initForNotFreeing() {
    refCounts.store(InlineRefCountBits(1, 2), relaxed);
  }


  // Increment the reference count.
  void increment() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      newbits.incrementStrongRefCount();
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  void incrementNonAtomic() {
    auto bits = refCounts.load(relaxed);
    bits.incrementStrongRefCount();
    refCounts.store(bits, relaxed);
  }

  // Increment the reference count by n.
  void increment(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      newbits.incrementStrongRefCount(n);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  void incrementNonAtomic(uint32_t n) {
    auto bits = refCounts.load(relaxed);
    bits.incrementStrongRefCount(n);
    refCounts.store(bits, relaxed);
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
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      // If the flag is already set, just fail.
      if (oldbits.getIsPinned()) {
        return false;
      }

      // Try to simultaneously set the flag and increment the reference count.
      newbits = oldbits;
      newbits.setIsPinned(true);
      newbits.incrementStrongRefCount();
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  bool tryIncrementAndPinNonAtomic() {
    auto bits = refCounts.load(relaxed);

    // If the flag is already set, just fail.
    if (bits.getIsPinned()) {
      return false;
    }

    // Try to simultaneously set the flag and increment the reference count.
    bits.setIsPinned(true);
    bits.incrementStrongRefCount();
    refCounts.store(bits, relaxed);
    return true;
  }

  // Increment the reference count, unless the object is deiniting.
  bool tryIncrement() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;    
    do {
      if (oldbits.getIsDeiniting()) {
        return false;
      }

      newbits = oldbits;
      newbits.incrementStrongRefCount();
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  // Simultaneously clear the pinned flag and decrement the reference
  // count.
  //
  // Precondition: the pinned flag is set.
  bool decrementAndUnpinShouldDeinit() {
    return doDecrementShouldDeinit<true>();
  }

  bool decrementAndUnpinShouldDeinitNonAtomic() {
    return doDecrementShouldDeinitNonAtomic<true>();
  }

  // Decrement the reference count.
  // Return true if the caller should now deinit the object.
  bool decrementShouldDeinit() {
    return doDecrementShouldDeinit<false>();
  }

  bool decrementShouldDeinitNonAtomic() {
    return doDecrementShouldDeinitNonAtomic<false>();
  }

  bool decrementShouldDeinitN(uint32_t n) {
    return doDecrementShouldDeinitN<false>(n);
  }

  // Non-atomically release the last strong reference and mark the
  // object as deiniting.
  //
  // Precondition: the reference count must be 1
  void decrementFromOneAndDeinitNonAtomic() {
    auto bits = refCounts.load(relaxed);
    assert(bits.getStrongRefCount() == 1 && "Expect a count of 1");

    bits.setStrongRefCount(0);
    bits.setIsDeiniting(true);
    refCounts.store(bits, relaxed);
  }

  bool decrementShouldDeinitNNonAtomic(uint32_t n) {
    return doDecrementShouldDeinitNNonAtomic<false>(n);
  }

  // Return the reference count.
  // Once deinit begins the reference count is undefined.
  uint32_t getCount() const {
    auto bits = refCounts.load(relaxed);
    return bits.getStrongRefCount();
  }

  // Return whether the reference count is exactly 1.
  // Once deinit begins the reference count is undefined.
  bool isUniquelyReferenced() const {
    return getCount() == 1;
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set. Once deinit begins the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const {
    auto bits = refCounts.load(relaxed);
    return (bits.getStrongRefCount() == 1 || bits.getIsPinned());
    // FIXME: rewrite with rotate if compiler doesn't do it.

    // Rotating right by one sets the sign bit to the pinned bit. After
    // rotation, the dealloc flag is the least significant bit followed by the
    // reference count. A reference count of two or higher means that our value
    // is bigger than 3 if the pinned bit is not set. If the pinned bit is set
    // the value is negative.
    // Note: Because we are using the sign bit for testing pinnedness it
    // is important to do a signed comparison below.
    // static_assert(RC_PINNED_FLAG == 1,
    // "The pinned flag must be the lowest bit");
    // auto rotateRightByOne = ((value >> 1) | (value << 31));
    // return (int32_t)rotateRightByOne < (int32_t)RC_ONE;
  }

  // Return true if the object has started deiniting.
  bool isDeiniting() const {
    auto bits = refCounts.load(relaxed);
    return bits.getIsDeiniting();
  }

private:
  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeinit() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    
    bool performDeinit;
    do {
      newbits = oldbits;

      // ClearPinnedFlag assumes the pinned flag is already set.
      assert(!(ClearPinnedFlag && !oldbits.getIsPinned()) &&
             "unpinning reference that was not pinned");
      assert(oldbits.getStrongRefCount() >= 1 &&
             "releasing reference with a refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      if (ClearPinnedFlag) {
        newbits.setIsPinned(false);
      }
      newbits.decrementStrongRefCount();
      performDeinit = (newbits.getStrongRefCount() == 0 &&
                       !newbits.getIsDeiniting());
      if (performDeinit) {
        newbits.setIsDeiniting(true);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit) {
      std::atomic_thread_fence(acquire);
    }
    return performDeinit;
    
    /*  Old implementation for optimization reference:

    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    constexpr uint32_t quantum =
      (ClearPinnedFlag ? RC_ONE + RC_PINNED_FLAG : RC_ONE);
    uint32_t newval = __atomic_sub_fetch(&strongRefCount, quantum, __ATOMIC_RELEASE);

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
                  "fix decrementShouldDeinit() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&strongRefCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
    */
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeinitNonAtomic() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;

    // FIXME: can probably do this without CAS once zeroing weak references
    // are pushed to the side table. Then the presence of inline RC will
    // prove that the object is not already weakly-referenced so there can't
    // be a race vs a weak load; and presumably no other thread can see
    // the object so there can't be a race vs a weak store.
    
    bool performDeinit;
    do {
      newbits = oldbits;

      // ClearPinnedFlag assumes the pinned flag is already set.
      assert(!(ClearPinnedFlag && !oldbits.getIsPinned()) &&
             "unpinning reference that was not pinned");
      assert(oldbits.getStrongRefCount() >= 1 &&
             "releasing reference with a refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      if (ClearPinnedFlag) {
        newbits.setIsPinned(false);
      }
      newbits.decrementStrongRefCount();
      performDeinit = (newbits.getStrongRefCount() == 0 &&
                       !newbits.getIsDeiniting());
      if (performDeinit) {
        newbits.setIsDeiniting(true);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit) {
      std::atomic_thread_fence(acquire);
    }
    return performDeinit;
    
    /* Old implementation for optimization reference:

    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    constexpr uint32_t quantum =
      (ClearPinnedFlag ? RC_ONE + RC_PINNED_FLAG : RC_ONE);
    uint32_t val = __atomic_load_n(&strongRefCount, __ATOMIC_RELAXED);
    val -= quantum;
    __atomic_store_n(&strongRefCount, val, __ATOMIC_RELEASE);
    uint32_t newval = strongRefCount;

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
                  "fix decrementShouldDeinit() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&strongRefCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
    */
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeinitN(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    
    bool performDeinit;
    do {
      newbits = oldbits;

      // ClearPinnedFlag assumes the pinned flag is already set.
      assert(!(ClearPinnedFlag && !oldbits.getIsPinned()) &&
             "unpinning reference that was not pinned");
      assert(oldbits.getStrongRefCount() >= n &&
             "releasing reference with a refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      if (ClearPinnedFlag) {
        newbits.setIsPinned(false);
      }
      newbits.decrementStrongRefCount(n);
      performDeinit = (newbits.getStrongRefCount() == 0 &&
                       !newbits.getIsDeiniting());
      if (performDeinit) {
        newbits.setIsDeiniting(true);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit) {
      std::atomic_thread_fence(acquire);
    }
    return performDeinit;
    

    /* Old implementation for optimization reference:

    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    uint32_t delta = (n << RC_FLAGS_COUNT) + (ClearPinnedFlag ? RC_PINNED_FLAG : 0);
    uint32_t newval = __atomic_sub_fetch(&strongRefCount, delta, __ATOMIC_RELEASE);

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
                  "fix decrementShouldDeinit() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&strongRefCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
    */
  }

  template <bool ClearPinnedFlag>
  bool doDecrementShouldDeinitNNonAtomic(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;

    // FIXME: can probably do this without CAS once zeroing weak references
    // are pushed to the side table. Then the presence of inline RC will
    // prove that the object is not already weakly-referenced so there can't
    // be a race vs a weak load; and presumably no other thread can see
    // the object so there can't be a race vs a weak store.
    
    bool performDeinit;
    do {
      newbits = oldbits;
      
      // ClearPinnedFlag assumes the pinned flag is already set.
      assert(!(ClearPinnedFlag && !oldbits.getIsPinned()) &&
             "unpinning reference that was not pinned");
      assert(oldbits.getStrongRefCount() >= n &&
             "releasing reference with a refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      if (ClearPinnedFlag) {
        newbits.setIsPinned(false);
      }
      newbits.decrementStrongRefCount(n);
      performDeinit = (newbits.getStrongRefCount() == 0 &&
                       !newbits.getIsDeiniting());
      if (performDeinit) {
        newbits.setIsDeiniting(true);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit) {
      std::atomic_thread_fence(acquire);
    }
    return performDeinit;

    /* Old implementation for optimization reference:

    // If we're being asked to clear the pinned flag, we can assume
    // it's already set.
    uint32_t delta = (n << RC_FLAGS_COUNT) + (ClearPinnedFlag ? RC_PINNED_FLAG : 0);
    uint32_t val = __atomic_load_n(&strongRefCount, __ATOMIC_RELAXED);
    val -= delta;
    __atomic_store_n(&strongRefCount, val, __ATOMIC_RELEASE);
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
                  "fix decrementShouldDeinit() if you add more flags");
    uint32_t oldval = 0;
    newval = RC_DEALLOCATING_FLAG;
    return __atomic_compare_exchange(&strongRefCount, &oldval, &newval, 0,
                                     __ATOMIC_ACQUIRE, __ATOMIC_RELAXED);
    */
  }


  // UNOWNED
  
 public:
  // Increment the unowned reference count.
  void incrementUnowned() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      newbits.incrementUnownedRefCount();
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  // Increment the unowned reference count by n.
  void incrementUnowned(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      newbits.incrementUnownedRefCount(n);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  // Decrement the unowned reference count.
  // Return true if the caller should free the object.
  bool decrementUnownedShouldFree() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    
    bool performFree;
    do {
      newbits = oldbits;

      assert(oldbits.getUnownedRefCount() >= 1 &&
             "releasing reference with an unowned refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      newbits.decrementUnownedRefCount();
      performFree = (newbits.getUnownedRefCount() == 0);
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    return performFree;
  }

  // Decrement the unowned reference count.
  // Return true if the caller should free the object.
  bool decrementUnownedShouldFreeN(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    
    bool performFree;
    do {
      newbits = oldbits;

      assert(oldbits.getUnownedRefCount() >= n &&
             "releasing reference with an unowned refcount of zero");

      // FIXME: hand optimize these bit operations if necessary
      newbits.decrementUnownedRefCount(n);
      performFree = (newbits.getUnownedRefCount() == 0);
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    return performFree;
  }

  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  uint32_t getUnownedCount() const {
    auto bits = refCounts.load(relaxed);
    return bits.getUnownedRefCount();
  }
};

static_assert(swift::IsTriviallyConstructible<InlineRefCounts>::value,
              "InlineRefCounts must be trivially initializable");
static_assert(std::is_trivially_destructible<InlineRefCounts>::value,
              "InlineRefCounts must be trivially destructible");

#undef relaxed
#undef acquire
#undef release

// __cplusplus
#endif

#endif
