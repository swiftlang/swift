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
  __swift_uint64_t refCounts __attribute__((__unavailable__));
} InlineRefCounts;

// not __cplusplus
#else
// __cplusplus

#include <type_traits>
#include <atomic>
#include <stddef.h>
#include <stdint.h>
#include <assert.h>

#include "llvm/Support/Compiler.h"
#include "swift/Basic/type_traits.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/Debug.h"

// FIXME: Workaround for rdar://problem/18889711. 'Consume' does not require
// a barrier on ARM64, but LLVM doesn't know that. Although 'relaxed'
// is formally UB by C++11 language rules, we should be OK because neither
// the processor model nor the optimizer can realistically reorder our uses
// of 'consume'.
#if __arm64__ || __arm__
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_relaxed)
#else
#  define SWIFT_MEMORY_ORDER_CONSUME (std::memory_order_consume)
#endif

/*
  An object conceptually has three refcounts. These refcounts 
  are stored either "inline" in the field following the isa
  or in a "side table entry" pointed to by the field following the isa.
  
  The strong RC counts strong references to the object. When the strong RC 
  reaches zero the object is deinited, unowned reference reads become errors, 
  and weak reference reads become nil. The strong RC is stored as an extra
  count: when the physical field is 0 the logical value is 1.

  The unowned RC counts unowned references to the object. The unowned RC 
  also has an extra +1 on behalf of the strong references; this +1 is 
  decremented after deinit completes. When the unowned RC reaches zero 
  the object's allocation is freed.

  The weak RC counts weak references to the object. The weak RC also has an 
  extra +1 on behalf of the unowned references; this +1 is decremented 
  after the object's allocation is freed. When the weak RC reaches zero 
  the object's side table entry is freed.

  Objects initially start with no side table. They can gain a side table when:
  * a weak reference is formed 
  and pending future implementation:
  * strong RC or unowned RC overflows (inline RCs will be small on 32-bit)
  * associated object storage is needed on an object
  * etc
  Gaining a side table entry is a one-way operation; an object with a side 
  table entry never loses it. This prevents some thread races.

  Strong and unowned variables point at the object.
  Weak variables point at the object's side table.


  Storage layout:

  HeapObject {
    isa
    InlineRefCounts {
      atomic<InlineRefCountBits> {
        strong RC + unowned RC + flags
        OR
        HeapObjectSideTableEntry*
      }
    }
  }

  HeapObjectSideTableEntry {
    SideTableRefCounts {
      object pointer
      atomic<SideTableRefCountBits> {
        strong RC + unowned RC + weak RC + flags
      }
    }   
  }

  InlineRefCounts and SideTableRefCounts share some implementation
  via RefCounts<T>.

  InlineRefCountBits and SideTableRefCountBits share some implementation
  via RefCountBitsT<bool>.

  In general: The InlineRefCounts implementation tries to perform the 
  operation inline. If the object has a side table it calls the 
  HeapObjectSideTableEntry implementation which in turn calls the 
  SideTableRefCounts implementation. 
  Downside: this code is a bit twisted.
  Upside: this code has less duplication than it might otherwise


  Object lifecycle state machine:

  LIVE without side table
  The object is alive.
  Object's refcounts are initialized as 1 strong, 1 unowned, 1 weak.
  No side table. No weak RC storage.
  Strong variable operations work normally. 
  Unowned variable operations work normally.
  Weak variable load can't happen.
  Weak variable store adds the side table, becoming LIVE with side table.
  When the strong RC reaches zero deinit() is called and the object 
    becomes DEINITING.

  LIVE with side table
  Weak variable operations work normally.
  Everything else is the same as LIVE.

  DEINITING without side table
  deinit() is in progress on the object.
  Strong variable operations have no effect.
  Unowned variable load halts in swift_abortRetainUnowned().
  Unowned variable store works normally.
  Weak variable load can't happen.
  Weak variable store stores nil.
  When deinit() completes, the generated code calls swift_deallocObject. 
    swift_deallocObject calls canBeFreedNow() checking for the fast path 
    of no weak or unowned references. 
    If canBeFreedNow() the object is freed and it becomes DEAD. 
    Otherwise, it decrements the unowned RC and the object becomes DEINITED.

  DEINITING with side table
  Weak variable load returns nil. 
  Weak variable store stores nil.
  canBeFreedNow() is always false, so it never transitions directly to DEAD.
  Everything else is the same as DEINITING.

  DEINITED without side table
  deinit() has completed but there are unowned references outstanding.
  Strong variable operations can't happen.
  Unowned variable store can't happen.
  Unowned variable load halts in swift_abortRetainUnowned().
  Weak variable operations can't happen.
  When the unowned RC reaches zero, the object is freed and it becomes DEAD.

  DEINITED with side table
  Weak variable load returns nil.
  Weak variable store can't happen.
  When the unowned RC reaches zero, the object is freed, the weak RC is 
    decremented, and the object becomes FREED.
  Everything else is the same as DEINITED.

  FREED without side table
  This state never happens.

  FREED with side table
  The object is freed but there are weak refs to the side table outstanding.
  Strong variable operations can't happen.
  Unowned variable operations can't happen.
  Weak variable load returns nil.
  Weak variable store can't happen.
  When the weak RC reaches zero, the side table entry is freed and 
    the object becomes DEAD.

  DEAD
  The object and its side table are gone.
*/

namespace swift {
  struct HeapObject;
  class HeapObjectSideTableEntry;
}

// FIXME: HACK: copied from HeapObject.cpp
extern "C" LLVM_LIBRARY_VISIBILITY void
_swift_release_dealloc(swift::HeapObject *object)
  SWIFT_CC(RegisterPreservingCC_IMPL)
    __attribute__((__noinline__, __used__));

namespace swift {

// RefCountIsInline: refcount stored in an object
// RefCountNotInline: refcount stored in an object's side table entry
enum RefCountInlinedness { RefCountNotInline = false, RefCountIsInline = true };

enum ClearPinnedFlag { DontClearPinnedFlag = false, DoClearPinnedFlag = true };

enum PerformDeinit { DontPerformDeinit = false, DoPerformDeinit = true };


// Raw storage of refcount bits, depending on pointer size and inlinedness.
// 32-bit inline refcount is 32-bits. All others are 64-bits.

template <RefCountInlinedness refcountIsInline, size_t sizeofPointer>
struct RefCountBitsInt;

// 64-bit inline
// 64-bit out of line
template <RefCountInlinedness refcountIsInline>
struct RefCountBitsInt<refcountIsInline, 8> {
  typedef uint64_t Type;
  typedef int64_t SignedType;
};

// 32-bit out of line
template <>
struct RefCountBitsInt<RefCountNotInline, 4> {
  typedef uint64_t Type;
  typedef int64_t SignedType;
};

// 32-bit inline
template <>
struct RefCountBitsInt<RefCountIsInline, 4> {
  typedef uint32_t Type;
  typedef int32_t SignedType;  
};


// Layout of refcount bits.
// field value = (bits & mask) >> shift
// FIXME: redo this abstraction more cleanly
  
# define maskForField(name) (((uint64_t(1)<<name##BitCount)-1) << name##Shift)
# define shiftAfterField(name) (name##Shift + name##BitCount)

template <size_t sizeofPointer>
struct RefCountBitOffsets;

// 64-bit inline
// 64-bit out of line
// 32-bit out of line
template <>
struct RefCountBitOffsets<8> {
  static const size_t IsPinnedShift = 0;
  static const size_t IsPinnedBitCount = 1; 
  static const uint64_t IsPinnedMask = maskForField(IsPinned);

  static const size_t UnownedRefCountShift = shiftAfterField(IsPinned);
  static const size_t UnownedRefCountBitCount = 31;
  static const uint64_t UnownedRefCountMask = maskForField(UnownedRefCount);

  static const size_t IsDeinitingShift = shiftAfterField(UnownedRefCount);
  static const size_t IsDeinitingBitCount = 1;
  static const uint64_t IsDeinitingMask = maskForField(IsDeiniting);

  static const size_t StrongExtraRefCountShift = shiftAfterField(IsDeiniting);
  static const size_t StrongExtraRefCountBitCount = 30;
  static const uint64_t StrongExtraRefCountMask = maskForField(StrongExtraRefCount);

  static const size_t UseSlowRCShift = shiftAfterField(StrongExtraRefCount);
  static const size_t UseSlowRCBitCount = 1;
  static const uint64_t UseSlowRCMask = maskForField(UseSlowRC);

  static const size_t SideTableShift = 0;
  static const size_t SideTableBitCount = 62;
  static const uint64_t SideTableMask = maskForField(SideTable);
  static const size_t SideTableUnusedLowBits = 3;

  static const size_t SideTableMarkShift = SideTableBitCount;
  static const size_t SideTableMarkBitCount = 1;
  static const uint64_t SideTableMarkMask = maskForField(SideTableMark);
};

// 32-bit inline
template <>
struct RefCountBitOffsets<4> {
  static const size_t IsPinnedShift = 0;
  static const size_t IsPinnedBitCount = 1; 
  static const uint32_t IsPinnedMask = maskForField(IsPinned);

  static const size_t UnownedRefCountShift = shiftAfterField(IsPinned);
  static const size_t UnownedRefCountBitCount = 7;
  static const uint32_t UnownedRefCountMask = maskForField(UnownedRefCount);

  static const size_t IsDeinitingShift = shiftAfterField(UnownedRefCount);
  static const size_t IsDeinitingBitCount = 1;
  static const uint32_t IsDeinitingMask = maskForField(IsDeiniting);

  static const size_t StrongExtraRefCountShift = shiftAfterField(IsDeiniting);
  static const size_t StrongExtraRefCountBitCount = 22;
  static const uint32_t StrongExtraRefCountMask = maskForField(StrongExtraRefCount);

  static const size_t UseSlowRCShift = shiftAfterField(StrongExtraRefCount);
  static const size_t UseSlowRCBitCount = 1;
  static const uint32_t UseSlowRCMask = maskForField(UseSlowRC);

  static const size_t SideTableShift = 0;
  static const size_t SideTableBitCount = 30;
  static const uint32_t SideTableMask = maskForField(SideTable);
  static const size_t SideTableUnusedLowBits = 2;

  static const size_t SideTableMarkShift = SideTableBitCount;
  static const size_t SideTableMarkBitCount = 1;
  static const uint32_t SideTableMarkMask = maskForField(SideTableMark);
};


/*
  FIXME: reinstate these assertions
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
*/


// Basic encoding of refcount and flag data into the object's header.
template <RefCountInlinedness refcountIsInline>
class RefCountBitsT {

  friend class RefCountBitsT<RefCountIsInline>;
  friend class RefCountBitsT<RefCountNotInline>;
  
  static const RefCountInlinedness Inlinedness = refcountIsInline;

  typedef typename RefCountBitsInt<refcountIsInline, sizeof(void*)>::Type
    BitsType;
  typedef typename RefCountBitsInt<refcountIsInline, sizeof(void*)>::SignedType
    SignedBitsType;
  typedef RefCountBitOffsets<sizeof(BitsType)>
    Offsets;

  BitsType bits;

  // "Bitfield" accessors.

# define getFieldIn(bits, offsets, name)                                \
    ((bits & offsets::name##Mask) >> offsets::name##Shift)
# define setFieldIn(bits, offsets, name, val)                           \
    bits = ((bits & ~offsets::name##Mask) |                             \
            (((BitsType(val) << offsets::name##Shift) & offsets::name##Mask)))
  
# define getField(name) getFieldIn(bits, Offsets, name)
# define setField(name, val) setFieldIn(bits, Offsets, name, val)  
# define copyFieldFrom(src, name)                                       \
    setFieldIn(bits, Offsets, name,                                     \
               getFieldIn(src.bits, decltype(src)::Offsets, name))

  // RefCountBits uses always_inline everywhere
  // to improve performance of debug builds.
  
  private:
  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getUseSlowRC() const {
    return bool(getField(UseSlowRC));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setUseSlowRC(bool value) {
    setField(UseSlowRC, value);
  }

    
  // Returns true if the decrement is a fast-path result.
  // Returns false if the decrement should fall back to some slow path
  // (for example, because UseSlowRC is set
  // or because the refcount is now zero and should deinit).
  template <ClearPinnedFlag clearPinnedFlag>
  LLVM_NODISCARD LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool doDecrementStrongExtraRefCount(uint32_t dec) {
#ifndef NDEBUG
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
#endif

    BitsType unpin = (clearPinnedFlag
                      ? (BitsType(1) << Offsets::IsPinnedShift)
                      : 0);
    // This deliberately underflows by borrowing from the UseSlowRC field.
    bits -= unpin + (BitsType(dec) << Offsets::StrongExtraRefCountShift);
    return (SignedBitsType(bits) >= 0);
  }

  public:

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT() = default;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  constexpr
  RefCountBitsT(uint32_t strongExtraCount, uint32_t unownedCount)
    : bits((BitsType(strongExtraCount) << Offsets::StrongExtraRefCountShift) |
           (BitsType(unownedCount)     << Offsets::UnownedRefCountShift))
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT(HeapObjectSideTableEntry* side)
    : bits((reinterpret_cast<BitsType>(side) >> Offsets::SideTableUnusedLowBits)
           | (BitsType(1) << Offsets::UseSlowRCShift)
           | (BitsType(1) << Offsets::SideTableMarkShift))
  {
    assert(refcountIsInline);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  RefCountBitsT(RefCountBitsT<RefCountIsInline> newbits) {
    bits = 0;
    
    if (refcountIsInline  ||  sizeof(newbits) == sizeof(*this)) {
      // this and newbits are both inline
      // OR this is out-of-line but the same layout as inline.
      // (FIXME: use something cleaner than sizeof for same-layout test)
      // Copy the bits directly.
      bits = newbits.bits;
    }
    else {
      // this is out-of-line and not the same layout as inline newbits.
      // Copy field-by-field.
      copyFieldFrom(newbits, UnownedRefCount);
      copyFieldFrom(newbits, IsPinned);
      copyFieldFrom(newbits, IsDeiniting);
      copyFieldFrom(newbits, StrongExtraRefCount);
      copyFieldFrom(newbits, UseSlowRC);
    }
  }
  
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

    // Stored value is a shifted pointer.
    return reinterpret_cast<HeapObjectSideTableEntry *>
      (uintptr_t(getField(SideTable)) << Offsets::SideTableUnusedLowBits);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uint32_t getUnownedRefCount() const {
    assert(!hasSideTable());
    return uint32_t(getField(UnownedRefCount));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getIsPinned() const {
    assert(!hasSideTable());
    return bool(getField(IsPinned));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool getIsDeiniting() const {
    assert(!hasSideTable());
    return bool(getField(IsDeiniting));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uint32_t getStrongExtraRefCount() const {
    assert(!hasSideTable());
    return uint32_t(getField(StrongExtraRefCount));
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
    uintptr_t value = reinterpret_cast<uintptr_t>(side);
    uintptr_t storedValue = value >> Offsets::SideTableUnusedLowBits;
    assert(storedValue << Offsets::SideTableUnusedLowBits == value);
    setField(SideTable, storedValue);
    setField(SideTableMark, 1);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setUnownedRefCount(uint32_t value) {
    assert(!hasSideTable());
    setField(UnownedRefCount, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setIsPinned(bool value) {
    assert(!hasSideTable());
    setField(IsPinned, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setIsDeiniting(bool value) {
    assert(!hasSideTable());
    setField(IsDeiniting, value);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  void setStrongExtraRefCount(uint32_t value) {
    assert(!hasSideTable());
    setField(StrongExtraRefCount, value);
  }
  

  // Returns true if the increment is a fast-path result.
  // Returns false if the increment should fall back to some slow path
  // (for example, because UseSlowRC is set or because the refcount overflowed).
  LLVM_NODISCARD LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool incrementStrongExtraRefCount(uint32_t inc) {
    // This deliberately overflows into the UseSlowRC field.
    bits += BitsType(inc) << Offsets::StrongExtraRefCountShift;
    return (SignedBitsType(bits) >= 0);
  }

  // FIXME: I don't understand why I can't make clearPinned a template argument
  // (compiler balks at calls from class RefCounts that way)
  LLVM_NODISCARD LLVM_ATTRIBUTE_ALWAYS_INLINE
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

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool isUniquelyReferenced() {
    static_assert(Offsets::IsPinnedBitCount +
                  Offsets::UnownedRefCountBitCount +
                  Offsets::IsDeinitingBitCount +
                  Offsets::StrongExtraRefCountBitCount +
                  Offsets::UseSlowRCBitCount == sizeof(bits)*8,
                  "inspect isUniquelyReferenced after adding fields");

    // isPinned: don't care
    // Unowned: don't care (FIXME: should care and redo initForNotFreeing)
    // IsDeiniting: false
    // StrongExtra: 0
    // UseSlowRC: false

    // Compiler is clever enough to optimize this.
    return
      !getUseSlowRC() && !getIsDeiniting() && getStrongExtraRefCount() == 0;
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  bool isUniquelyReferencedOrPinned() {
    static_assert(Offsets::IsPinnedBitCount +
                  Offsets::UnownedRefCountBitCount +
                  Offsets::IsDeinitingBitCount +
                  Offsets::StrongExtraRefCountBitCount +
                  Offsets::UseSlowRCBitCount == sizeof(bits)*8,
                  "inspect isUniquelyReferencedOrPinned after adding fields");

    // isPinned: don't care
    // Unowned: don't care (FIXME: should care and redo initForNotFreeing)
    // IsDeiniting: false
    // isPinned/StrongExtra: true/any OR false/0
    // UseSlowRC: false

    // Compiler is not clever enough to optimize this.
    // return (isUniquelyReferenced() ||
    //         (!getUseSlowRC() && !getIsDeiniting() && getIsPinned()));

    // Bit twiddling solution:
    // 1. Define the fields in this order:
    //    bits that must be zero when not pinned | bits to ignore | IsPinned
    // 2. Rotate IsPinned into the sign bit:
    //    IsPinned | bits that must be zero when not pinned | bits to ignore
    // 3. Perform a signed comparison against X = (1 << count of ignored bits).
    //    IsPinned makes the value negative and thus less than X.
    //    Zero in the must-be-zero bits makes the value less than X.
    //    Non-zero and not pinned makes the value greater or equal to X.

    // Count the ignored fields.
    constexpr auto ignoredBitsCount =
      Offsets::UnownedRefCountBitCount + Offsets::IsDeinitingBitCount;
    // Make sure all fields are positioned as expected.
    // -1 compensates for the rotation.
    static_assert(Offsets::IsPinnedShift == 0, "IsPinned must be the LSB bit");
    static_assert(
      shiftAfterField(Offsets::UnownedRefCount)-1 <= ignoredBitsCount &&
      shiftAfterField(Offsets::IsDeiniting)-1 <= ignoredBitsCount &&
      Offsets::StrongExtraRefCountShift-1 >= ignoredBitsCount &&
      Offsets::UseSlowRCShift-1 >= ignoredBitsCount,
      "refcount bit layout incorrect for isUniquelyReferencedOrPinned");

    BitsType X = BitsType(1) << ignoredBitsCount;
    BitsType rotatedBits = ((bits >> 1) | (bits << (8*sizeof(bits) - 1)));
    return SignedBitsType(rotatedBits) < SignedBitsType(X);
  }

# undef getFieldIn
# undef setFieldIn
# undef getField
# undef setField
# undef copyFieldFrom
};

# undef maskForField
# undef shiftAfterField

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
    // weak refcount starts at 1 on behalf of the unowned count
    , weakBits(1)
  { }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SideTableRefCountBits(HeapObjectSideTableEntry* side) = delete;

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  SideTableRefCountBits(InlineRefCountBits newbits)
    : RefCountBitsT<RefCountNotInline>(newbits), weakBits(1)
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

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  uint32_t getWeakRefCount() {
    return weakBits;
  }

  // Side table ref count never has a side table of its own.
  LLVM_ATTRIBUTE_ALWAYS_INLINE
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
//
// Unowned and weak increment and decrement are all unordered.
// There is no deinit equivalent for these counts so no fence is needed.
//
// Accessing the side table requires that refCounts be accessed with
// a load-consume. Only code that is guaranteed not to try dereferencing
// the side table may perform a load-relaxed of refCounts.
// Similarly, storing the new side table pointer into refCounts is a
// store-release, but most other stores into refCounts are store-relaxed.

template <typename RefCountBits>
class RefCounts {
  std::atomic<RefCountBits> refCounts;
#if !__LP64__
  // FIXME: hack - something somewhere is assuming a 3-word header on 32-bit
  // See also other fixmes marked "small header for 32-bit"
  uintptr_t unused __attribute__((unavailable));
#endif

  // Out-of-line slow paths.
  
  LLVM_ATTRIBUTE_NOINLINE
  void incrementSlow(RefCountBits oldbits, uint32_t inc);

  LLVM_ATTRIBUTE_NOINLINE
  void incrementNonAtomicSlow(RefCountBits oldbits, uint32_t inc);

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinSlow(RefCountBits oldbits);

  LLVM_ATTRIBUTE_NOINLINE
  bool tryIncrementAndPinNonAtomicSlow(RefCountBits);

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
    refCounts.store(RefCountBits(0, 1), std::memory_order_relaxed);
  }

  // Initialize for a stack promoted object. This prevents that the final
  // release frees the memory of the object.
  // FIXME: need to mark these and assert they never get a side table,
  // because the extra unowned ref will keep the side table alive forever
  void initForNotFreeing() {
    refCounts.store(RefCountBits(0, 2), std::memory_order_relaxed);
  }

  // Initialize from another refcount bits.
  // Only inline -> out-of-line is allowed (used for new side table entries).
  void init(InlineRefCountBits newBits) {
    refCounts.store(newBits, std::memory_order_relaxed);
  }

  // Increment the reference count.
  void increment(uint32_t inc = 1) {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    do {
      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount(inc);
      if (!fast)
        return incrementSlow(oldbits, inc);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
  }

  void incrementNonAtomic(uint32_t inc = 1) {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    auto newbits = oldbits;
    bool fast = newbits.incrementStrongExtraRefCount(inc);
    if (!fast)
      return incrementNonAtomicSlow(oldbits, inc);
    refCounts.store(newbits, std::memory_order_relaxed);
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
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
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
        return tryIncrementAndPinSlow(oldbits);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
    return true;
  }

  bool tryIncrementAndPinNonAtomic() {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);

    // If the flag is already set, just fail.
    if (!oldbits.hasSideTable() && oldbits.getIsPinned())
      return false;

    // Try to simultaneously set the flag and increment the reference count.
    auto newbits = oldbits;
    newbits.setIsPinned(true);
    bool fast = newbits.incrementStrongExtraRefCount(1);
    if (!fast)
      return tryIncrementAndPinNonAtomicSlow(oldbits);
    refCounts.store(newbits, std::memory_order_relaxed);
    return true;
  }

  // Increment the reference count, unless the object is deiniting.
  bool tryIncrement() {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    do {
      if (!oldbits.hasSideTable() && oldbits.getIsDeiniting())
        return false;

      newbits = oldbits;
      bool fast = newbits.incrementStrongExtraRefCount(1);
      if (!fast)
        return tryIncrementSlow(oldbits);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
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
  void decrementFromOneNonAtomic() {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (bits.hasSideTable())
      return bits.getSideTable()->decrementFromOneNonAtomic();
    
    assert(!bits.getIsDeiniting());
    assert(bits.getStrongExtraRefCount() == 0 && "Expect a refcount of 1");
    bits.setStrongExtraRefCount(0);
    bits.setIsDeiniting(true);
    refCounts.store(bits, std::memory_order_relaxed);
  }

  // Return the reference count.
  // Once deinit begins the reference count is undefined.
  uint32_t getCount() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (bits.hasSideTable())
      return bits.getSideTable()->getCount();
    
    assert(!bits.getIsDeiniting());  // FIXME: can we assert this?
    return bits.getStrongExtraRefCount() + 1;
  }

  // Return whether the reference count is exactly 1.
  // Once deinit begins the reference count is undefined.
  bool isUniquelyReferenced() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (bits.hasSideTable())
      return false;  // FIXME: implement side table path if useful
    
    assert(!bits.getIsDeiniting());
    return bits.isUniquelyReferenced();
  }

  // Return whether the reference count is exactly 1 or the pin flag
  // is set. Once deinit begins the reference count is undefined.
  bool isUniquelyReferencedOrPinned() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    // FIXME: implement side table path if useful
    // In the meantime we don't check it here.
    // bits.isUniquelyReferencedOrPinned() checks it too,
    // and the compiler optimizer does better if this check is not here.
    // if (bits.hasSideTable())
    //   return false;
    
    assert(!bits.getIsDeiniting());

    // bits.isUniquelyReferencedOrPinned() also checks the side table bit
    // and this path is optimized better if we don't check it here first.
    if (bits.isUniquelyReferencedOrPinned()) return true;
    if (!bits.hasSideTable())
      return false;
    return bits.getSideTable()->isUniquelyReferencedOrPinned();
  }

  // Return true if the object has started deiniting.
  bool isDeiniting() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (bits.hasSideTable())
      return bits.getSideTable()->isDeiniting();
    else
      return bits.getIsDeiniting();
  }

  /// Return true if the object can be freed directly right now.
  /// (transition DEINITING -> DEAD)
  /// This is used in swift_deallocObject().
  /// Can be freed now means:  
  ///   no side table
  ///   unowned reference count is 1
  /// The object is assumed to be deiniting with no strong references already.
  bool canBeFreedNow() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    return (!bits.hasSideTable() &&
            bits.getIsDeiniting() &&
            bits.getStrongExtraRefCount() == 0 &&
            bits.getUnownedRefCount() == 1);
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
        // LIVE -> DEINITING
        deinitNow = true;
        assert(!oldbits.getIsDeiniting());  // FIXME: make this an error?
        newbits = oldbits;  // Undo failed decrement of newbits.
        newbits.setStrongExtraRefCount(0);
        newbits.setIsDeiniting(true);
        if (clearPinnedFlag)
          newbits.setIsPinned(false);
      }
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_release,
                                              std::memory_order_relaxed));
    if (performDeinit && deinitNow) {
      std::atomic_thread_fence(std::memory_order_acquire);
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
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    
    do {
      newbits = oldbits;
      bool fast = newbits.decrementStrongExtraRefCount(dec, clearPinnedFlag);
      if (!fast)
        // Slow paths include side table; deinit; underflow
        return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_release,
                                              std::memory_order_relaxed));

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
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    do {
      if (oldbits.hasSideTable())
        return oldbits.getSideTable()->incrementUnowned(inc);

      newbits = oldbits;
      assert(newbits.getUnownedRefCount() != 0);
      newbits.incrementUnownedRefCount(inc);
      // FIXME: overflow check?
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
  }

  // Decrement the unowned reference count.
  // Return true if the caller should free the object.
  bool decrementUnownedShouldFree(uint32_t dec) {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    
    bool performFree;
    do {
      if (oldbits.hasSideTable())
        return oldbits.getSideTable()->decrementUnownedShouldFree(dec);

      newbits = oldbits;
      newbits.decrementUnownedRefCount(dec);
      if (newbits.getUnownedRefCount() == 0) {
        // DEINITED -> FREED  or  DEINITED -> DEAD
        // Caller will free the object. Weak decrement is handled by
        // HeapObjectSideTableEntry::decrementUnownedShouldFree.
        assert(newbits.getIsDeiniting());
        performFree = true;
      } else {
        performFree = false;
      }
      // FIXME: underflow check?
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
    return performFree;
  }

  // Return unowned reference count.
  // Note that this is not equal to the number of outstanding unowned pointers.
  uint32_t getUnownedCount() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
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
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;
    do {
      newbits = oldbits;
      assert(newbits.getWeakRefCount() != 0);
      newbits.incrementWeakRefCount();
      // FIXME: overflow check
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));
  }
  
  bool decrementWeakShouldCleanUp() {
    auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    RefCountBits newbits;

    bool performFree;
    do {
      newbits = oldbits;
      performFree = newbits.decrementWeakRefCount();
    } while (!refCounts.compare_exchange_weak(oldbits, newbits,
                                              std::memory_order_relaxed));

    return performFree;
  }
  
  // Return weak reference count.
  // Note that this is not equal to the number of outstanding weak pointers.
  uint32_t getWeakCount() const {
    auto bits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);
    if (bits.hasSideTable()) {
      return bits.getSideTable()->getWeakCount();
    } else {
      // No weak refcount storage. Return only the weak increment held
      // on behalf of the unowned count.
      return bits.getUnownedRefCount() ? 1 : 0;
    }
  }


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

/* FIXME: small header for 32-bit
static_assert(sizeof(InlineRefCounts) == sizeof(uintptr_t),
  "InlineRefCounts must be pointer-sized");
static_assert(alignof(InlineRefCounts) == alignof(uintptr_t),
"InlineRefCounts must be pointer-aligned");
*/


class HeapObjectSideTableEntry {
  // FIXME: does object need to be atomic?
  std::atomic<HeapObject*> object;
  SideTableRefCounts refCounts;

  public:
  HeapObjectSideTableEntry(HeapObject *newObject)
    : object(newObject), refCounts()
  { }

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winvalid-offsetof"
  static ptrdiff_t refCountsOffset() {
    return offsetof(HeapObjectSideTableEntry, refCounts);
  }
#pragma clang diagnostic pop

  HeapObject* tryRetain() {
    if (refCounts.tryIncrement())
      return object.load(std::memory_order_relaxed);
    else
      return nullptr;
  }

  void initRefCounts(InlineRefCountBits newbits) {
    refCounts.init(newbits);
  }

  HeapObject *unsafeGetObject() const {
    return object.load(std::memory_order_relaxed);
  }

  // STRONG
  
  void incrementStrong(uint32_t inc) {
    refCounts.increment(inc);
  }

  template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
  bool decrementStrong(uint32_t dec) {
    return refCounts.doDecrement<clearPinnedFlag, performDeinit>(dec);
  }

  void decrementFromOneNonAtomic() {
    // FIXME: can there be a non-atomic implementation?
    decrementStrong<DontClearPinnedFlag, DontPerformDeinit>(1);
  }
  
  bool isDeiniting() const {
    return refCounts.isDeiniting();
  }

  bool tryIncrement() {
    return refCounts.tryIncrement();
  }

  bool tryIncrementAndPin() {
    return refCounts.tryIncrementAndPin();
  }

  uint32_t getCount() const {
    return refCounts.getCount();
  }

  bool isUniquelyReferencedOrPinned() const {
    return refCounts.isUniquelyReferencedOrPinned();
  }

  // UNOWNED

  void incrementUnowned(uint32_t inc) {
    return refCounts.incrementUnowned(inc);
  }

  bool decrementUnownedShouldFree(uint32_t dec) {
    bool shouldFree = refCounts.decrementUnownedShouldFree(dec);
    if (shouldFree) {
      // DEINITED -> FREED
      // Caller will free the object.
      decrementWeak();
    }

    return shouldFree;
  }

  uint32_t getUnownedCount() const {
    return refCounts.getUnownedCount();
  }

  
  // WEAK
  
  LLVM_NODISCARD
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

    // Weak ref count is now zero. Delete the side table entry.
    // FREED -> DEAD
    assert(refCounts.getUnownedCount() == 0);
    delete this;
  }

  uint32_t getWeakCount() const {
    return refCounts.getWeakCount();
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

  auto oldbits = refCounts.load(SWIFT_MEMORY_ORDER_CONSUME);

  // Use slow path if we can't guarantee atomicity.
  if (oldbits.hasSideTable() || oldbits.getUnownedRefCount() != 1)
    return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);

  auto newbits = oldbits;
  bool fast = newbits.decrementStrongExtraRefCount(dec, clearPinnedFlag);
  if (!fast)
    return doDecrementSlow<clearPinnedFlag, performDeinit>(oldbits, dec);

  refCounts.store(newbits, std::memory_order_relaxed);
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


template <>
template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
inline bool RefCounts<InlineRefCountBits>::
doDecrementSideTable(InlineRefCountBits oldbits, uint32_t dec) {
  auto side = oldbits.getSideTable();
  return side->decrementStrong<clearPinnedFlag, performDeinit>(dec);
}

template <>
template <ClearPinnedFlag clearPinnedFlag, PerformDeinit performDeinit>
inline bool RefCounts<SideTableRefCountBits>::
doDecrementSideTable(SideTableRefCountBits oldbits, uint32_t dec) {
  swift::crash("side table refcount must not have "
               "a side table entry of its own");
}


template <> inline
HeapObject* RefCounts<InlineRefCountBits>::getHeapObject() const {
  auto offset = sizeof(void *);
  auto prefix = ((char *)this - offset);
  return (HeapObject *)prefix;
}

template <> inline
HeapObject* RefCounts<SideTableRefCountBits>::getHeapObject() const {
  auto offset = HeapObjectSideTableEntry::refCountsOffset();
  auto prefix = ((char *)this - offset);
  return *(HeapObject **)prefix;
}


// namespace swift
}

// for use by SWIFT_HEAPOBJECT_NON_OBJC_MEMBERS
typedef swift::InlineRefCounts InlineRefCounts;

// __cplusplus
#endif

#endif
