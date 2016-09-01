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

// Error reporting functions
namespace swift {
  LLVM_ATTRIBUTE_NORETURN LLVM_ATTRIBUTE_NOINLINE
  void swift_abortRetainOverflow();
};

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

    StrongExtraRefCountShift = ShiftAfterField(IsDeiniting),
    StrongExtraRefCountBitCount = 29,
    StrongExtraRefCountMask = MaskForField(StrongExtraRefCount),

    UseSlowRCShift = ShiftAfterField(StrongExtraRefCount),
    UseSlowRCBitCount = 1,
    UseSlowRCMask = MaskForField(UseSlowRC),

    // FIXME: 63 bits but MSB must always be zero. Handle that differently.
    SideTableShift = 0,
    SideTableBitCount = 63,
    SideTableMask = MaskForField(SideTable)
  };

  static_assert(StrongExtraRefCountShift == IsDeinitingShift + 1, 
                "IsDeiniting must be LSB-wards of StrongExtraRefCount");
  static_assert(UseSlowRCShift + UseSlowRCBitCount == sizeof(bits)*8,
                "UseSlowRC must be MSB");
  static_assert(SideTableBitCount + UseSlowRCBitCount == sizeof(bits)*8,
               "wrong bit count for InlineRefCountBits side table version");
  static_assert(UnownedRefCountBitCount + IsPinnedBitCount +
                IsDeinitingBitCount + StrongExtraRefCountBitCount +
                UseSlowRCBitCount == sizeof(bits)*8,
                "wrong bit count for InlineRefCountBits refcount version");
# undef MaskForField
# undef ShiftAfterField

# define GetField(name) \
    ((bits & name##Mask) >> name##Shift)
# define SetField(name, val) \
  bits = (bits & ~name##Mask) | (((uint64_t(val) << name##Shift) & name##Mask))

  // InlineRefCountBits uses always_inline everywhere
  // to improve performance of debug builds.
  
 private:
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getUseSlowRC() const {
    return bool(GetField(UseSlowRC));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setUseSlowRC(bool value) {
    SetField(UseSlowRC, value);
  }

 public:

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  InlineRefCountBits() = default;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  constexpr InlineRefCountBits(uint32_t strongExtraCount, uint32_t unownedCount)
    : bits((uint64_t(strongExtraCount) << StrongExtraRefCountShift) |
           (uint64_t(unownedCount)     << UnownedRefCountShift))
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool hasSideTable() const {
    // FIXME: change this when introducing immutable RC objects
    return getUseSlowRC();
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uintptr_t getSideTable() const {
    assert(hasSideTable());
    // Stored value is a shifted pointer.
    // FIXME: Don't hard-code this shift amount?
    return uintptr_t(GetField(SideTable)) << 2;
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
  uint32_t getStrongExtraRefCount() const {
    assert(!hasSideTable());
    return uint32_t(GetField(StrongExtraRefCount));
  }


  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setHasSideTable(bool value) {
    bits = 0;
    setUseSlowRC(value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setSideTable(uintptr_t value) {
    assert(hasSideTable());
    // Stored value is a shifted pointer.
    // FIXME: Don't hard-code this shift amount?
    uintptr_t storedValue = value >> 2;
    assert(storedValue << 2 == value);
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
  void setStrongExtraRefCount(uint32_t value) {
    assert(!hasSideTable());
    SetField(StrongExtraRefCount, value);
  }
  

  // Returns true if the increment is a fast-path result.
  // Returns false if the increment should fall back to some slow path
  // (for example, because UseSlowRC is set or because the refcount overflowed).
  LLVM_ATTRIBUTE_ALWAYS_INLINE LLVM_ATTRIBUTE_UNUSED_RESULT
  bool incrementStrongExtraRefCount(uint32_t inc = 1) {
    // This deliberately overflows into the UseSlowRC field.
    bits += uint64_t(inc) << StrongExtraRefCountShift;
    return (int64_t(bits) >= 0);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void incrementUnownedRefCount(uint32_t inc = 1) {
    setUnownedRefCount(getUnownedRefCount() + inc);
  }
  
  // Returns true if the decrement is a fast-path result.
  // Returns false if the decrement should fall back to some slow path
  // (for example, because UseSlowRC is set
  // or because the refcount is now zero and should deinit).
  template <bool ClearPinnedFlag>
  LLVM_ATTRIBUTE_ALWAYS_INLINE LLVM_ATTRIBUTE_UNUSED_RESULT
  bool decrementStrongExtraRefCount(uint32_t dec = 1) {
    // ClearPinnedFlag assumes the flag is already set.
    if (ClearPinnedFlag)
      assert(getIsPinned() && "unpinning reference that was not pinned");
    
    if (getIsDeiniting())
      assert(getStrongExtraRefCount() >= dec  &&  
             "releasing reference whose refcount is already zero");
    else 
      assert(getStrongExtraRefCount() + 1 >= dec  &&  
             "releasing reference whose refcount is already zero");

    uint64_t unpin = ClearPinnedFlag ? (uint64_t(1) << IsPinnedShift) : 0;
    // This deliberately underflows by borrowing from the UseSlowRC field.
    bits -= unpin + (uint64_t(dec) << StrongExtraRefCountShift);
    return (int64_t(bits) >= 0);
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

  // Template parameters.
  enum ClearPinnedFlag { DontClearPinnedFlag = false,
                         DoClearPinnedFlag = true };
  enum Atomicity { NonAtomic = false, Atomic = true }; 

  // Out-of-line slow paths.
  
  LLVM_ATTRIBUTE_NOINLINE
  void incrementSlow(InlineRefCountBits oldbits, uint32_t n = 1) {
    if (oldbits.hasSideTable()) {
      // Out-of-line slow path.
      abort();
    } else {
      swift::swift_abortRetainOverflow();
    }
  }

  LLVM_ATTRIBUTE_NOINLINE
  void incrementNonAtomicSlow(InlineRefCountBits oldbits, uint32_t n = 1) {
    if (oldbits.hasSideTable()) {
      // Out-of-line slow path.
      abort();
    } else {
      swift::swift_abortRetainOverflow();
    }
  }

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinSlow() {
    abort();
  }

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinNonAtomicSlow() {
    abort();
  }

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementSlow() {
    abort();
  }

 public:
  enum Initialized_t { Initialized };

  // InlineRefCounts must be trivially constructible to avoid ObjC++
  // destruction overhead at runtime. Use InlineRefCounts(Initialized)
  // to produce an initialized instance.
  InlineRefCounts() = default;
  
  // Refcount of a new object is 1.
  constexpr InlineRefCounts(Initialized_t)
    : refCounts(InlineRefCountBits(0, 1)) { }

  void init() {
    refCounts.store(InlineRefCountBits(0, 1), relaxed);
  }

  /// Initialize for a stack promoted object. This prevents that the final
  /// release frees the memory of the object.
  void initForNotFreeing() {
    refCounts.store(InlineRefCountBits(0, 2), relaxed);
  }


  // Increment the reference count.
  void increment() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount();
      if (!fast)
        return incrementSlow(oldbits); 
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  void incrementNonAtomic() {
    auto oldbits = refCounts.load(relaxed);
    auto newbits = oldbits;
    bool fast = newbits.incrementStrongExtraRefCount();
    if (!fast)
      return incrementNonAtomicSlow(oldbits);
    refCounts.store(newbits, relaxed);
  }

  // Increment the reference count by n.
  void increment(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    do {
      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount(n);
      if (!fast)
        return incrementSlow(oldbits, n);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  void incrementNonAtomic(uint32_t n) {
    auto oldbits = refCounts.load(relaxed);
    auto newbits = oldbits;
    bool fast = newbits.incrementStrongExtraRefCount(n);
    if (!fast)
      return incrementNonAtomicSlow(oldbits, n);
    refCounts.store(newbits, relaxed);
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
      if (oldbits.getIsPinned())
        return false;

      // Try to simultaneously set the flag and increment the reference count.
      newbits = oldbits;
      newbits.setIsPinned(true);
      bool fast = newbits.incrementStrongExtraRefCount();
      if (!fast)
        return tryIncrementAndPinSlow();
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  bool tryIncrementAndPinNonAtomic() {
    auto bits = refCounts.load(relaxed);

    // If the flag is already set, just fail.
    if (bits.getIsPinned())
      return false;

    // Try to simultaneously set the flag and increment the reference count.
    bits.setIsPinned(true);
    bool fast = bits.incrementStrongExtraRefCount();
    if (!fast)
      return tryIncrementAndPinNonAtomicSlow();
    refCounts.store(bits, relaxed);
    return true;
  }

  // Increment the reference count, unless the object is deiniting.
  bool tryIncrement() {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;    
    do {
      if (oldbits.getIsDeiniting())
        return false;

      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount();
      if (!fast)
        return tryIncrementSlow();
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  // Simultaneously clear the pinned flag and decrement the reference
  // count.
  //
  // Precondition: the pinned flag is set.
  bool decrementAndUnpinShouldDeinit() {
    return doDecrementShouldDeinit<DoClearPinnedFlag>();
  }

  bool decrementAndUnpinShouldDeinitNonAtomic() {
    return doDecrementShouldDeinitNonAtomic<DoClearPinnedFlag>();
  }

  // Decrement the reference count.
  // Return true if the caller should now deinit the object.
  bool decrementShouldDeinit() {
    return doDecrementShouldDeinit<DontClearPinnedFlag>();
  }

  bool decrementShouldDeinitNonAtomic() {
    return doDecrementShouldDeinitNonAtomic<DontClearPinnedFlag>();
  }

  bool decrementShouldDeinitN(uint32_t n) {
    return doDecrementShouldDeinit<DontClearPinnedFlag>(n);
  }

  bool decrementShouldDeinitNNonAtomic(uint32_t n) {
    return doDecrementShouldDeinitNonAtomic<DontClearPinnedFlag>(n);
  }

  // Non-atomically release the last strong reference and mark the
  // object as deiniting.
  //
  // Precondition: the reference count must be 1
  void decrementFromOneAndDeinitNonAtomic() {
    auto bits = refCounts.load(relaxed);
    assert(bits.getStrongExtraRefCount() == 0 && "Expect a refcount of 1");

    bits.setStrongExtraRefCount(0);
    bits.setIsDeiniting(true);
    refCounts.store(bits, relaxed);
  }

  // Return the reference count.
  // Once deinit begins the reference count is undefined.
  uint32_t getCount() const {
    auto bits = refCounts.load(relaxed);
    return bits.getStrongExtraRefCount() + 1;
  }

  // Return whether the reference count is exactly 1.
  // Once deinit begins the reference count is undefined.
  bool isUniquelyReferenced() const {
    auto bits = refCounts.load(relaxed);
    assert(!bits.getIsDeiniting());
    if (bits.hasSideTable())
      abort();
    return bits.getStrongExtraRefCount() == 0;
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set. Once deinit begins the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const {
    auto bits = refCounts.load(relaxed);
    assert(!bits.getIsDeiniting());
    if (bits.hasSideTable())
      abort();
    return (bits.getStrongExtraRefCount() == 0 || bits.getIsPinned());
    
    // FIXME: check if generated code is efficient.
    // We can't use the old rotate implementation below because
    // the strong refcount field is now biased.

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

  // Second slow path of doDecrementShouldDeinit, where the
  // object may have a side table entry.
  template <ClearPinnedFlag clearPinnedFlag>
  bool doDecrementShouldDeinitSlow2(InlineRefCountBits oldbits) {
    abort();
  }
  
  // First slow path of doDecrementShouldDeinit, where the object
  // may need to be deinited.
  // Side table paths are handled in doDecrementShouldDeinitSlow2().
  // FIXME: can we do the non-atomic thing here?
  template <Atomicity isAtomic, ClearPinnedFlag clearPinnedFlag>
  bool doDecrementShouldDeinitSlow1(InlineRefCountBits oldbits,
                                    uint32_t dec = 1) {
    InlineRefCountBits newbits;
    
    bool performDeinit;
    do {
      newbits = oldbits;
      
      bool fast = newbits.decrementStrongExtraRefCount<clearPinnedFlag>(dec);
      if (fast) {
        // Decrement completed normally. New refcount is not zero.
        performDeinit = false;
      }
      else if (oldbits.hasSideTable()) {
        // Decrement failed because we're on some other slow path.
        return doDecrementShouldDeinitSlow2<clearPinnedFlag>(oldbits);
      } else {
        // Decrement underflowed. Begin deinit.
        newbits = oldbits;  // Undo failed decrement of newbits.
        performDeinit = true;
        newbits.setStrongExtraRefCount(0);
        newbits.setIsDeiniting(true);
        if (clearPinnedFlag)
          newbits.setIsPinned(false);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit)
      std::atomic_thread_fence(acquire);

    return performDeinit;
  }
  
  template <ClearPinnedFlag clearPinnedFlag>
  bool doDecrementShouldDeinit(uint32_t n = 1) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;
    
    do {
      newbits = oldbits;
      bool fast = newbits.decrementStrongExtraRefCount<clearPinnedFlag>(n);
      if (!fast)
        return
          doDecrementShouldDeinitSlow1<Atomic, clearPinnedFlag>(oldbits, n);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              release, relaxed));

    return false;  // don't deinit
  }

  template <ClearPinnedFlag clearPinnedFlag>
  bool doDecrementShouldDeinitNonAtomic(uint32_t n = 1) {
    auto oldbits = refCounts.load(relaxed);
    InlineRefCountBits newbits;

    // FIXME: can probably do this without CAS once zeroing weak references
    // are pushed to the side table. Then the presence of inline RC will
    // prove that the object is not already weakly-referenced so there can't
    // be a race vs a weak load; and presumably no other thread can see
    // the object so there can't be a race vs a weak store.
    
    do {
      newbits = oldbits;
      bool fast = newbits.decrementStrongExtraRefCount<clearPinnedFlag>(n);
      if (!fast)
        return
          doDecrementShouldDeinitSlow1<NonAtomic, clearPinnedFlag>(oldbits);
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));

    return false;  // don't deinit
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
