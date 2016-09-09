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
#include "swift/Runtime/Config.h"

namespace swift {
  struct HeapObject;
  class HeapObjectSideTableEntry;
}

// FIXME: HACK copied from HeapObject.cpp
extern "C" LLVM_LIBRARY_VISIBILITY void
_swift_release_dealloc(swift::HeapObject *object)
  SWIFT_CC(RegisterPreservingCC_IMPL)
    __attribute__((__noinline__, __used__));

namespace swift {

// FIXME: There are lots of asserts here which hurts Assert performance a lot.

// FIXME: many `relaxed` in this file should be `consume`,
// but (1) the compiler doesn't support `consume` directly,
// and (2) the compiler promotes `consume` to `acquire` instead which
// is overkill on our CPUs. But this might leave us vulnerable to
// compiler optimizations that `relaxed` allows but `consume` ought not.
#define relaxed std::memory_order_relaxed
#define acquire std::memory_order_acquire
#define release std::memory_order_release
#define consume std::memory_order_consume


// RefCountIsInline: refcount stored in an object
// RefCountNotInline: refcount stored in an object's side table entry
enum RefCountInlinedness { RefCountNotInline = false, RefCountIsInline = true };

enum ClearPinnedFlag { DontClearPinnedFlag = false, DoClearPinnedFlag = true };

enum PerformDeinit { DontPerformDeinit = false, DoPerformDeinit = true };


// Basic encoding of refcount and flag data into the object's header.
// FIXME: Specialize this for a 32-bit field on 32-bit architectures.
template <RefCountInlinedness refcountIsInline>
class RefCountBitsT {

  friend class RefCountBitsT<RefCountIsInline>;
  friend class RefCountBitsT<RefCountNotInline>;
  
  static const RefCountInlinedness Inlinedness = refcountIsInline;
  
  uint64_t bits;

  // Layout of bits.
  // field value = (bits & mask) >> shift
  
# define MaskForField(name) (((1UL<<name##BitCount)-1) << name##Shift)
# define ShiftAfterField(name) (name##Shift + name##BitCount)

  enum : uint64_t {
    // FIXME: isFreeing bit
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

    SideTableShift = 0,
    SideTableBitCount = 62,
    SideTableMask = MaskForField(SideTable),

    SideTableMarkShift = SideTableBitCount,
    SideTableMarkBitCount = 1,
    SideTableMarkMask = MaskForField(SideTableMark)
  };

  static_assert(StrongExtraRefCountShift == IsDeinitingShift + 1, 
                "IsDeiniting must be LSB-wards of StrongExtraRefCount");
  static_assert(UseSlowRCShift + UseSlowRCBitCount == sizeof(bits)*8,
                "UseSlowRC must be MSB");
  static_assert(SideTableBitCount + SideTableMarkBitCount +
                UseSlowRCBitCount == sizeof(bits)*8,
               "wrong bit count for RefCountBits side table encoding");
  static_assert(UnownedRefCountBitCount + IsPinnedBitCount +
                IsDeinitingBitCount + StrongExtraRefCountBitCount +
                UseSlowRCBitCount == sizeof(bits)*8,
                "wrong bit count for RefCountBits refcount encoding");
# undef MaskForField
# undef ShiftAfterField

# define GetField(name) \
    ((bits & name##Mask) >> name##Shift)
# define SetField(name, val) \
  bits = (bits & ~name##Mask) | (((uint64_t(val) << name##Shift) & name##Mask))

  // RefCountBits uses always_inline everywhere
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

    
  // Returns true if the decrement is a fast-path result.
  // Returns false if the decrement should fall back to some slow path
  // (for example, because UseSlowRC is set
  // or because the refcount is now zero and should deinit).
  template <ClearPinnedFlag clearPinnedFlag>
  LLVM_ATTRIBUTE_ALWAYS_INLINE LLVM_ATTRIBUTE_UNUSED_RESULT
  bool doDecrementStrongExtraRefCount(uint32_t dec) {
    if (!hasSideTable()) {
      // Can't check these assertions with side table present.
      
      // clearPinnedFlag assumes the flag is already set.
      if (clearPinnedFlag)
        assert(getIsPinned() && "unpinning reference that was not pinned");

      if (getIsDeiniting())
        assert(getStrongExtraRefCount() >= dec  &&  
               "releasing reference whose refcount is already zero");
      else 
        assert(getStrongExtraRefCount() + 1 >= dec  &&  
               "releasing reference whose refcount is already zero");
    }

    uint64_t unpin = clearPinnedFlag ? (uint64_t(1) << IsPinnedShift) : 0;
    // This deliberately underflows by borrowing from the UseSlowRC field.
    bits -= unpin + (uint64_t(dec) << StrongExtraRefCountShift);
    return (int64_t(bits) >= 0);
  }

 public:

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT() = default;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  constexpr
  RefCountBitsT(uint32_t strongExtraCount, uint32_t unownedCount)
    : bits((uint64_t(strongExtraCount) << StrongExtraRefCountShift) |
           (uint64_t(unownedCount)     << UnownedRefCountShift))
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT(HeapObjectSideTableEntry* side)
    : bits((reinterpret_cast<uint64_t>(side) >> 3) |
           (1ULL << UseSlowRCShift) |
           (1ULL << SideTableMarkShift))
  {
    assert(refcountIsInline);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT(RefCountBitsT<RefCountIsInline> newbits) : bits(newbits.bits) { }
  
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool hasSideTable() const {
    // FIXME: change this when introducing immutable RC objects
    bool hasSide = getUseSlowRC();

    // Side table refcount must not point to another side table.
    assert((refcountIsInline || !hasSide)  &&
           "side table refcount must not have a side table entry of its own");
    
    return hasSide;
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  HeapObjectSideTableEntry *getSideTable() const {
    assert(hasSideTable());
    // FIXME: overkill barrier? Otherwise technically need
    // a consume re-load of the bits before dereferencing.
    std::atomic_thread_fence(std::memory_order_acquire);
    
    // Stored value is a shifted pointer.
    // FIXME: Don't hard-code this shift amount?
    return reinterpret_cast<HeapObjectSideTableEntry *>
      (uintptr_t(GetField(SideTable)) << 3);
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
  void setSideTable(HeapObjectSideTableEntry *side) {
    assert(hasSideTable());
    // Stored value is a shifted pointer.
    // FIXME: Don't hard-code this shift amount?
    uintptr_t value = reinterpret_cast<uintptr_t>(side);
    uintptr_t storedValue = value >> 3;
    assert(storedValue << 3 == value);
    SetField(SideTable, storedValue);
    SetField(SideTableMark, 1);
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
  bool incrementStrongExtraRefCount(uint32_t inc) {
    // This deliberately overflows into the UseSlowRC field.
    bits += uint64_t(inc) << StrongExtraRefCountShift;
    return (int64_t(bits) >= 0);
  }

  // FIXME: I don't understand why I can't make clearPinned a template argument
  // (compiler balks at calls from class RefCounts that way)
  LLVM_ATTRIBUTE_ALWAYS_INLINE LLVM_ATTRIBUTE_UNUSED_RESULT
  bool decrementStrongExtraRefCount(uint32_t dec, bool clearPinned = false) {
    if (clearPinned) 
      return doDecrementStrongExtraRefCount<DoClearPinnedFlag>(dec);
    else
      return doDecrementStrongExtraRefCount<DontClearPinnedFlag>(dec);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void incrementUnownedRefCount(uint32_t inc) {
    setUnownedRefCount(getUnownedRefCount() + inc);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementUnownedRefCount(uint32_t dec) {
    setUnownedRefCount(getUnownedRefCount() - dec);
  }
  
# undef GetField
# undef SetField
};

typedef RefCountBitsT<RefCountIsInline> InlineRefCountBits;

class SideTableRefCountBits : public RefCountBitsT<RefCountNotInline>
{
  uint32_t weakBits;

 public:
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SideTableRefCountBits() = default;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  constexpr
  SideTableRefCountBits(uint32_t strongExtraCount, uint32_t unownedCount)
    : RefCountBitsT<RefCountNotInline>(strongExtraCount, unownedCount)
    , weakBits(0)
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SideTableRefCountBits(HeapObjectSideTableEntry* side) = delete;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SideTableRefCountBits(InlineRefCountBits newbits)
    : RefCountBitsT<RefCountNotInline>(newbits), weakBits(0)
  { }

  
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void incrementWeakRefCount() {
    weakBits++;
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool decrementWeakRefCount() {
    assert(weakBits > 0);
    weakBits--;
    return weakBits == 0;
  }

  // Side table ref count never has a side table of its own.
  bool hasSideTable() {
    return false;
  }
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

template <typename RefCountBits>
class RefCounts {
  std::atomic<RefCountBits> refCounts;

  // Out-of-line slow paths.
  
  LLVM_ATTRIBUTE_NOINLINE
  void incrementSlow(RefCountBits oldbits, uint32_t inc);

  LLVM_ATTRIBUTE_NOINLINE
  void incrementNonAtomicSlow(RefCountBits oldbits, uint32_t inc);

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinSlow();

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinNonAtomicSlow();

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementSlow(RefCountBits oldbits);

 public:
  enum Initialized_t { Initialized };

  // RefCounts must be trivially constructible to avoid ObjC++
  // destruction overhead at runtime. Use RefCounts(Initialized)
  // to produce an initialized instance.
  RefCounts() = default;
  
  // Refcount of a new object is 1.
  constexpr RefCounts(Initialized_t)
    : refCounts(RefCountBits(0, 1)) { }

  void init() {
    refCounts.store(RefCountBits(0, 1), relaxed);
  }

  /// Initialize for a stack promoted object. This prevents that the final
  /// release frees the memory of the object.
  void initForNotFreeing() {
    refCounts.store(RefCountBits(0, 2), relaxed);
  }

  // Initialize from another refcount bits.
  // Only inline -> out-of-line is allowed (used for new side table entries).
  void init(InlineRefCountBits newBits) {
    refCounts.store(newBits, relaxed);
  }

  // Increment the reference count.
  void increment(uint32_t inc = 1) {
    auto oldbits = refCounts.load(relaxed);
    RefCountBits newbits;
    do {
      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount(inc);
      if (!fast)
        return incrementSlow(oldbits, inc);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  void incrementNonAtomic(uint32_t inc = 1) {
    auto oldbits = refCounts.load(relaxed);
    auto newbits = oldbits;
    bool fast = newbits.incrementStrongExtraRefCount(inc);
    if (!fast)
      return incrementNonAtomicSlow(oldbits, inc);
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
    RefCountBits newbits;
    do {
      // If the flag is already set, just fail.
      if (!oldbits.hasSideTable() && oldbits.getIsPinned())
        return false;

      // Try to simultaneously set the flag and increment the reference count.
      newbits = oldbits;
      newbits.setIsPinned(true);
      bool fast = newbits.incrementStrongExtraRefCount(1);
      if (!fast)
        return tryIncrementAndPinSlow();
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  bool tryIncrementAndPinNonAtomic() {
    auto bits = refCounts.load(relaxed);

    // If the flag is already set, just fail.
    if (!bits.hasSideTable() && bits.getIsPinned())
      return false;

    // Try to simultaneously set the flag and increment the reference count.
    bits.setIsPinned(true);
    bool fast = bits.incrementStrongExtraRefCount(1);
    if (!fast)
      return tryIncrementAndPinNonAtomicSlow();
    refCounts.store(bits, relaxed);
    return true;
  }

  // Increment the reference count, unless the object is deiniting.
  bool tryIncrement() {
    auto oldbits = refCounts.load(relaxed);
    RefCountBits newbits;
    do {
      if (!oldbits.hasSideTable() && oldbits.getIsDeiniting())
        return false;

      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount(1);
      if (!fast)
        return tryIncrementSlow(oldbits);
    } while (! refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
    return true;
  }

  // Simultaneously clear the pinned flag and decrement the reference
  // count. Call _swift_release_dealloc() if the reference count goes to zero.
  //
  // Precondition: the pinned flag is set.
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementAndUnpinAndMaybeDeinit() {
    doDecrement<DoClearPinnedFlag, DoPerformDeinit>(1);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementAndUnpinAndMaybeDeinitNonAtomic() {
    doDecrementNonAtomic<DoClearPinnedFlag, DoPerformDeinit>(1);
  }

  // Decrement the reference count.
  // Return true if the caller should now deinit the object.
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool decrementShouldDeinit(uint32_t dec) {
    return doDecrement<DontClearPinnedFlag, DontPerformDeinit>(dec);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementAndMaybeDeinit(uint32_t dec) {
    doDecrement<DontClearPinnedFlag, DoPerformDeinit>(dec);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void decrementAndMaybeDeinitNonAtomic(uint32_t dec) {
    doDecrementNonAtomic<DontClearPinnedFlag, DoPerformDeinit>(dec);
  }

  // Non-atomically release the last strong reference and mark the
  // object as deiniting.
  //
  // Precondition: the reference count must be 1
  void decrementFromOneAndDeinitNonAtomic() {
    auto bits = refCounts.load(relaxed);
    if (bits.hasSideTable())
      abort();
    
    assert(!bits.getIsDeiniting());
    assert(bits.getStrongExtraRefCount() == 0 && "Expect a refcount of 1");
    bits.setStrongExtraRefCount(0);
    bits.setIsDeiniting(true);
    refCounts.store(bits, relaxed);
  }

  // Return the reference count.
  // Once deinit begins the reference count is undefined.
  uint32_t getCount() const {
    auto bits = refCounts.load(relaxed);
    if (bits.hasSideTable())
      abort();
    assert(!bits.getIsDeiniting());  // FIXME: can we assert this?
    return bits.getStrongExtraRefCount() + 1;
  }

  // Return whether the reference count is exactly 1.
  // Once deinit begins the reference count is undefined.
  bool isUniquelyReferenced() const {
    auto bits = refCounts.load(relaxed);
    if (bits.hasSideTable())
      abort();
    assert(!bits.getIsDeiniting());
    return bits.getStrongExtraRefCount() == 0;
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set. Once deinit begins the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const {
    auto bits = refCounts.load(relaxed);
    if (bits.hasSideTable())
      abort();
    assert(!bits.getIsDeiniting());
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
    if (bits.hasSideTable())
      return bits.getSideTable()->isDeiniting();
    else
      return bits.getIsDeiniting();
  }

  /// Return true if the object can be freed directly right now.
  /// This is used in swift_deallocObject().
  /// Can be freed now means:  
  ///   no side table
  ///   unowned reference count is 1
  /// The object is assumed to be deiniting with no strong references already.
  bool canBeFreedNow() const {
    auto bits = refCounts.load(relaxed);
    return (!bits.hasSideTable() && bits.getIsDeiniting() && bits.getStrongExtraRefCount() == 0 && bits.getUnownedRefCount() == 1);
    // FIXME: make sure no-assert build optimizes this
  }

 private:

  // Second slow path of doDecrement, where the
  // object may have a side table entry.
  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool doDecrementSideTable(RefCountBits oldbits, uint32_t dec);
  
  // First slow path of doDecrement, where the object may need to be deinited.
  // Side table is handled in the second slow path, doDecrementSideTable().
  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool doDecrementSlow(RefCountBits oldbits, uint32_t dec) {
    RefCountBits newbits;
    
    bool deinitNow;
    do {
      newbits = oldbits;
      
      bool fast = newbits.decrementStrongExtraRefCount(dec, clearPinnedFlag);
      if (fast) {
        // Decrement completed normally. New refcount is not zero.
        deinitNow = false;
      }
      else if (oldbits.hasSideTable()) {
        // Decrement failed because we're on some other slow path.
        return doDecrementSideTable<clearPinnedFlag,
                                    performDeinit>(oldbits, dec);
      }
      else {
        // Decrement underflowed. Begin deinit.
        deinitNow = true;
        assert(!oldbits.getIsDeiniting());
        newbits = oldbits;  // Undo failed decrement of newbits.
        newbits.setStrongExtraRefCount(0);
        newbits.setIsDeiniting(true);
        if (clearPinnedFlag)
          newbits.setIsPinned(false);
      }
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    if (performDeinit && deinitNow) {
      std::atomic_thread_fence(acquire);
      _swift_release_dealloc(getHeapObject());
    }

    return deinitNow;
  }
  
  public:  // FIXME: access control hack

  // Fast path of atomic strong decrement.
  // 
  // Deinit is optionally handled directly instead of always deferring to 
  // the caller because the compiler can optimize this arrangement better.
  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool doDecrement(uint32_t dec) {
    auto oldbits = refCounts.load(relaxed);
    RefCountBits newbits;
    
    do {
      newbits = oldbits;
      bool fast = newbits.decrementStrongExtraRefCount(dec, clearPinnedFlag);
      if (!fast)
        // Slow paths include side table; deinit; underflow
        return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              release, relaxed));

    return false;  // don't deinit
  }
  
 private:

  // This is independently specialized below for inline and out-of-line use.
  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool doDecrementNonAtomic(uint32_t dec);


  // UNOWNED
  
 public:
  // Increment the unowned reference count.
  void incrementUnowned(uint32_t inc) {
    auto oldbits = refCounts.load(relaxed);
    RefCountBits newbits;
    do {
      if (oldbits.hasSideTable())
        return oldbits.getSideTable()->incrementUnowned(inc);

      newbits = oldbits;
      newbits.incrementUnownedRefCount(inc);
      // FIXME: overflow check?
    } while (!refCounts.compare_exchange_weak(oldbits, newbits, relaxed));
  }

  // Decrement the unowned reference count.
  // Return true if the caller should free the object.
  bool decrementUnownedShouldFree(uint32_t dec) {
    auto oldbits = refCounts.load(relaxed);
    RefCountBits newbits;
    
    bool performFree;
    do {
      if (oldbits.hasSideTable())
        return oldbits.getSideTable()->decrementUnownedShouldFree(dec);

      newbits = oldbits;
      newbits.decrementUnownedRefCount(dec);
      performFree = (newbits.getUnownedRefCount() == 0);
      // FIXME: underflow check?
    } while (! refCounts.compare_exchange_weak(oldbits, newbits,
                                               release, relaxed));
    return performFree;
  }

  // Return unowned reference count.
  // Note that this is not equal to the number of outstanding unowned pointers.
  uint32_t getUnownedCount() const {
    auto bits = refCounts.load(relaxed);
    if (bits.hasSideTable())
      return bits.getSideTable()->getUnownedCount();
    else 
      return bits.getUnownedRefCount();
  }


  // WEAK
  
 public:
  // Returns the object's side table entry (creating it if necessary) with
  // its weak ref count incremented.
  // Returns nullptr if the object is already deiniting.
  // Use this when creating a new weak reference to an object.
  HeapObjectSideTableEntry* formWeakReference();

  // Increment the weak reference count.
  void incrementWeak() {
    // FIXME
  }
  bool decrementWeakShouldCleanUp() {
    // FIXME
    return false;
  }
  
  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  // FIXME: inline fast path when there are no weak references outstanding
  uint32_t getWeakCount() const;


 private:
  HeapObject *getHeapObject() const;
  
  HeapObjectSideTableEntry* allocateSideTable();
};

typedef RefCounts<InlineRefCountBits> InlineRefCounts;
typedef RefCounts<SideTableRefCountBits> SideTableRefCounts;

static_assert(swift::IsTriviallyConstructible<InlineRefCounts>::value,
              "InlineRefCounts must be trivially initializable");
static_assert(std::is_trivially_destructible<InlineRefCounts>::value,
              "InlineRefCounts must be trivially destructible");


class HeapObjectSideTableEntry {
  std::atomic<HeapObject*> object;
  SideTableRefCounts refCounts;

 public:
  HeapObjectSideTableEntry(HeapObject *newObject)
    : object(newObject), refCounts()
  { }

  HeapObject* tryRetain() {
    if (refCounts.tryIncrement())
      return object.load();  // FIXME barrier
    else
      return nullptr;
  }

  void initRefCounts(InlineRefCountBits newbits) {
    refCounts.init(newbits);
  }

  HeapObject *unsafeGetObject() const {
    return object.load(relaxed);
  }
  
  void incrementStrong(uint32_t inc) {
    refCounts.increment(inc);
  }

  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool decrementStrong(uint32_t dec) {
    return refCounts.doDecrement<clearPinnedFlag, performDeinit>(dec);
  }

  bool isDeiniting() const {
    return refCounts.isDeiniting();
  }

  bool tryIncrement() {
    return refCounts.tryIncrement();
  }

  // UNOWNED

  void incrementUnowned(uint32_t inc) {
    return refCounts.incrementUnowned(inc);
  }

  bool decrementUnownedShouldFree(uint32_t dec) {
    bool shouldFree = refCounts.decrementUnownedShouldFree(dec);
    if (shouldFree) {
      // FIXME: Delete the side table if the weak count is zero.
    }

    return shouldFree;
  }

  uint32_t getUnownedCount() const {
    return refCounts.getUnownedCount();
  }

  
  // WEAK
  
  LLVM_ATTRIBUTE_UNUSED_RESULT
  HeapObjectSideTableEntry* incrementWeak() {
    // incrementWeak need not be atomic w.r.t. concurrent deinit initiation.
    // The client can't actually get a reference to the object without
    // going through tryRetain(). tryRetain is the one that needs to be
    // atomic w.r.t. concurrent deinit initiation.
    // The check here is merely an optimization.
    if (refCounts.isDeiniting())
      return nullptr;
    refCounts.incrementWeak();
    return this;
  }

  void decrementWeak() {
    // FIXME: assertions
    // FIXME: optimize barriers
    bool cleanup = refCounts.decrementWeakShouldCleanUp();
    if (!cleanup)
      return;

    // Weak ref count is now zero. Maybe delete the side table entry.
    abort();
  }
};


// Inline version of non-atomic strong decrement.
// This version can actually be non-atomic.
template <>
template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
LLVM_ATTRIBUTE_ALWAYS_INLINE
inline bool RefCounts<InlineRefCountBits>::doDecrementNonAtomic(uint32_t dec) {
  
  // We can get away without atomicity here.
  // The caller claims that there are no other threads with strong references 
  // to this object.
  // We can non-atomically check that there are no outstanding unowned or
  // weak references, and if nobody else has a strong reference then
  // nobody else can form a new unowned or weak reference.
  // Therefore there is no other thread that can be concurrently
  // manipulating this object's retain counts.

  auto oldbits = refCounts.load(relaxed);

  // Use slow path if we can't guarantee atomicity.
  if (oldbits.hasSideTable() || oldbits.getUnownedRefCount() != 1)
    return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);

  auto newbits = oldbits;
  bool fast = newbits.decrementStrongExtraRefCount(dec, clearPinnedFlag);
  if (!fast)
    return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);

  refCounts.store(newbits, relaxed);  
  return false;  // don't deinit
}

// Out-of-line version of non-atomic strong decrement.
// This version needs to be atomic because of the 
// threat of concurrent read of a weak reference.
template <>
template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
inline bool RefCounts<SideTableRefCountBits>::
doDecrementNonAtomic(uint32_t dec) {
  return doDecrement<clearPinnedFlag, performDeinit>(dec);
}

// SideTableRefCountBits specialization intentionally does not exist.
template <>
template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
inline bool RefCounts<InlineRefCountBits>::
doDecrementSideTable(InlineRefCountBits oldbits, uint32_t dec) {
  auto side = oldbits.getSideTable();
  return side->decrementStrong<clearPinnedFlag, performDeinit>(dec);
}

template <> inline
HeapObject* RefCounts<InlineRefCountBits>::getHeapObject() const {
  auto prefix = ((char *)this - sizeof(void*));
  return (HeapObject *)prefix;
}

template <> inline
HeapObject* RefCounts<SideTableRefCountBits>::getHeapObject() const {
  auto prefix = ((char *)this - sizeof(void*));
  return *(HeapObject **)prefix;
}


// namespace swift
}

// for use by SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS
typedef swift::InlineRefCounts InlineRefCounts;

#undef relaxed
#undef acquire
#undef release
#undef consume

// __cplusplus
#endif

#endif
