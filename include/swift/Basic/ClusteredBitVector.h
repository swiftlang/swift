//===--- ClusteredBitVector.h - A size-optimized bit vector -----*- C++ -*-===//
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
// This file defines the ClusteredBitVector class, a bitset data
// structure appropriate for situations meeting two criteria:
//
//   - Many vectors are no larger than a particular constant size, and
//     such vectors should be stored as compactly as possible.  This
//     constant size should be at least as large as any reasonable
//     target's pointer size in bits (i.e. at least 64).
//
//   - Most vectors have no bits set, and those that do tend to have
//     them set in coherent ranges.
//
// For example, this would be reasonable to use to describe the
// unoccupied bits in a memory layout.
//
// Primary mutators:
//   - appending another vector to this vector
//   - appending a constant vector (<0,0,...,0> or <1,1,...,1>) to this vector
//
// Primary observers:
//   - testing a specific bit
//   - searching for set bits from the start
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_CLUSTEREDBITVECTOR_H
#define SWIFT_BASIC_CLUSTEREDBITVECTOR_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include <algorithm>
#include <cassert>
#include <climits>
#include <cstdlib>
#include <type_traits>

namespace llvm {
  class APInt;
}

namespace swift {

/// A vector of bits.  This data structure is optimized to store an
/// empty vector of any size without doing any allocation.
class ClusteredBitVector {
  using ChunkType = uint64_t;
  static_assert(std::is_unsigned<ChunkType>::value, "ChunkType must be unsigned");
  enum {
    ChunkSizeInBits = sizeof(ChunkType) * CHAR_BIT
  };
  static_assert(sizeof(ChunkType) >= sizeof(ChunkType*),
                "ChunkType must be large enough to store a pointer");

  /// Return the number of chunks required to store a vector of the
  /// given number of bits.
  static size_t getNumChunksForBits(size_t value) { 
    return (value + ChunkSizeInBits - 1) / ChunkSizeInBits;
  }

  /// Either:
  ///   - a uint64_t * with at least enough storage for
  ///     getNumChunksForBits(LengthInBits) or
  ///   - inline storage for a single chunk
  /// as determined by HasOutOfLineData.
  ///
  /// 1) When using out-of-line storage:
  ///
  /// Suppose chunk size = 8, length = 13, capacity = 24.
  ///
  ///   11010101 00011010 101010010
  ///   ~~~~~~~~  ^ ~~~~~        ^
  ///     data    |  data        |
  ///             |              +---- bits in other chunks
  ///    high bits in last chunk         are uninitialized
  ///      are guaranteed zero
  /// 
  /// The capacity (in bits) is stored at index -1.
  ///
  /// 2) When using inline storage:
  ///
  ///  a) LengthInBits >= ChunkSizeInBits.  In this case, Data must be 0.
  ///     All bits are considered to be zero in this case.
  ///
  ///  b) 0 == LengthInBits.  In this case, Data must be 0.
  ///
  ///  c) 0 < LengthInBits < ChunkSizeInBits.  In this case, Data contains
  ///     a single chunk, with its unused high bits zeroed like in the
  ///     out-of-line case.
  ///
  /// Therefore, an efficient way to test whether all bits are zero:
  /// Data != 0.  (isInlineAndAllClear())  Not *guaranteed* to find
  /// something, but still efficient.
  ChunkType Data;
  
  size_t LengthInBits : sizeof(size_t) * CHAR_BIT - 1;
  size_t HasOutOfLineData : 1;

  /// Is this vector using out-of-line storage?
  bool hasOutOfLineData() const { return HasOutOfLineData; }

  /// Return true if this vector is not using out-of-line storage and
  /// does not have any bits set.  This is a special-case representation
  /// where the capacity can be smaller than the length.
  ///
  /// This is a necessary condition for hasSufficientChunkStorage(),
  /// and it's quicker to test, so a lot of routines in this class
  /// that need to work on chunk data in the general case test this
  /// first.
  bool isInlineAndAllClear() const {
    assert(!hasOutOfLineData() || Data != 0);
    return Data == 0;
  }

  /// Return true if this vector is not in the special case where the
  /// capacity is smaller than the length.  If this is true, then
  /// it's safe to call routines like getChunks().
  bool hasSufficientChunkStorage() const {
    return !(isInlineAndAllClear() && LengthInBits > ChunkSizeInBits);
  }

  /// Return the number of chunks required in order to store the full
  /// length (not capacity) of this bit vector.  This may be greater
  /// than the capacity in exactly one case, (2a), i.e.
  /// !hasSufficientChunkStorage().
  size_t getLengthInChunks() const {
    return getNumChunksForBits(LengthInBits);
  }

  /// Return the current capacity of this bit vector, in bits.  This
  /// is a relatively important operation because it's needed on every
  /// append.
  size_t getCapacityInBits() const {
    return hasOutOfLineData() ? getOutOfLineCapacityInBits() : ChunkSizeInBits;
  }

  /// Return the current capacity of this bit vector, in chunks.
  size_t getCapacityInChunks() const {
    return getCapacityInBits() / ChunkSizeInBits;
  }

  /// Return the current capacity of this bit vector, in bits, given
  /// that it's using out-of-line storage.
  size_t getOutOfLineCapacityInBits() const {
    assert(hasOutOfLineData());
    return (size_t) getOutOfLineChunksPtr()[-1];
  }

  /// Return the current capacity of this bit vector, in chunks, given
  /// that it's using out-of-line storage.
  size_t getOutOfLineCapacityInChunks() const {
    return getOutOfLineCapacityInBits() / ChunkSizeInBits;
  }

  /// Return a pointer to the data storage of this bit vector.
  ChunkType *getChunksPtr() {
    assert(hasSufficientChunkStorage());
    return hasOutOfLineData() ? getOutOfLineChunksPtr() : &Data;
  }
  const ChunkType *getChunksPtr() const {
    assert(hasSufficientChunkStorage());
    return hasOutOfLineData() ? getOutOfLineChunksPtr() : &Data;
  }

  MutableArrayRef<ChunkType> getChunks() {
    assert(hasSufficientChunkStorage());
    return { getChunksPtr(), getLengthInChunks() };
  }
  ArrayRef<ChunkType> getChunks() const {
    assert(hasSufficientChunkStorage());
    return { getChunksPtr(), getLengthInChunks() };
  }

  MutableArrayRef<ChunkType> getOutOfLineChunks() {
    return { getOutOfLineChunksPtr(), getLengthInChunks() };
  }
  ArrayRef<ChunkType> getOutOfLineChunks() const {
    return { getOutOfLineChunksPtr(), getLengthInChunks() };
  }

  /// Return a pointer to the data storage of this bit vector, given
  /// that it's using out-of-line storage.
  ChunkType *getOutOfLineChunksPtr() {
    assert(hasOutOfLineData());
    return reinterpret_cast<ChunkType*>(Data);
  }
  const ChunkType *getOutOfLineChunksPtr() const {
    assert(hasOutOfLineData());
    return reinterpret_cast<const ChunkType*>(Data);
  }

public:
  /// Create a new bit vector of zero length.  This does not perform
  /// any allocations.
  ClusteredBitVector() : Data(0), LengthInBits(0), HasOutOfLineData(0) {}

  /// Return a constant bit-vector of the given size.
  static ClusteredBitVector getConstant(size_t numBits, bool value) {
    ClusteredBitVector result;
    if (value) {
      result.reserve(numBits);
      result.appendSetBits(numBits);
    } else {
      result.appendClearBits(numBits);
    }
    return result;
  }

  ClusteredBitVector(const ClusteredBitVector &other)
    : Data(other.Data),
      LengthInBits(other.LengthInBits),
      HasOutOfLineData(other.HasOutOfLineData) {
    if (hasOutOfLineData()) {
      makeIndependentCopy();
    }
  }

  ClusteredBitVector(ClusteredBitVector &&other)
    : Data(other.Data),
      LengthInBits(other.LengthInBits),
      HasOutOfLineData(other.HasOutOfLineData) {
    other.dropData();
  }

  ClusteredBitVector &operator=(const ClusteredBitVector &other) {
    // Do something with our current out-of-line storage.
    if (hasOutOfLineData()) {
      // Copy into our current storage if its capacity is adequate.
      auto otherLengthInChunks = other.getLengthInChunks();
      if (otherLengthInChunks <= getOutOfLineCapacityInChunks()) {
        LengthInBits = other.LengthInBits;
        if (other.isInlineAndAllClear()) {
          memset(getOutOfLineChunksPtr(), 0,
                 otherLengthInChunks * sizeof(ChunkType));
        } else {
          memcpy(getOutOfLineChunksPtr(), other.getChunksPtr(),
                 otherLengthInChunks * sizeof(ChunkType));
        }
        return *this;
      }

      // Otherwise, destroy our current storage.
      destroy();
    }

    Data = other.Data;
    LengthInBits = other.LengthInBits;
    HasOutOfLineData = other.HasOutOfLineData;
    if (HasOutOfLineData) {
      makeIndependentCopy();
    }

    return *this;
  }

  ClusteredBitVector &operator=(ClusteredBitVector &&other) {
    // Just drop our current out-of-line storage.
    if (hasOutOfLineData()) {
      destroy();
    }

    Data = other.Data;
    LengthInBits = other.LengthInBits;
    HasOutOfLineData = other.HasOutOfLineData;
    other.dropData();
    return *this;
  }

  ~ClusteredBitVector() {
    if (hasOutOfLineData()) {
      destroy();
    }
  }

  /// Return true if this vector is zero-length (*not* if it does not
  /// contain any set bits).
  bool empty() const {
    return LengthInBits == 0;
  }

  /// Return the length of this bit-vector.
  size_t size() const {
    return LengthInBits;
  }

  /// Reserve space for an extra N bits.  This may unnecessarily force
  /// the vector to use an out-of-line representation.
  void reserveExtra(size_t numBits) {
    auto requiredBits = LengthInBits + numBits;
    if (requiredBits > getCapacityInBits()) {
      auto requiredChunks = getNumChunksForBits(requiredBits);
      auto chunkCount = getCapacityInChunks();
      assert(requiredChunks > chunkCount);
      do {
        // Growth curve: 1 (inline) -> 3 -> 7 -> 15 -> 31 -> ...
        // This is a particularly nice sequence because we store the
        // capacity in the chunk at index -1, so the actual allocation
        // size is a power of 2.
        chunkCount = chunkCount * 2 + 1;
      } while (requiredChunks > chunkCount);

      reallocate(chunkCount);
    }
    // Postcondition: hasSufficientChunkStorage().
  }

  /// Reserve space for a total of N bits.  This may unnecessarily
  /// force the vector to use an out-of-line representation.
  void reserve(size_t requiredSize) {
    if (requiredSize > getCapacityInBits()) {
      reallocate(getNumChunksForBits(requiredSize));
    }
    // Postcondition: hasSufficientChunkStorage().
  }

  /// Append the bits from the given vector to this one.
  void append(const ClusteredBitVector &other) {
    // Nothing to do if the other vector is empty.
    if (other.empty()) return;

    // Special case: don't allocate space for zero bits.
    if (isInlineAndAllClear() && other.isInlineAndAllClear()) {
      LengthInBits += other.LengthInBits;
      return;
    }

    // Okay, one or the other of these is using out-of-line storage.
    // Assume that bits might be set.
    reserveExtra(other.size());

    if (other.isInlineAndAllClear()) {
      appendConstantBitsReserved(other.size(), 0);
    } else {
      appendReserved(other.size(), other.getChunksPtr());
    }
  }

  /// Append the bits from the given vector to this one.
  void append(ClusteredBitVector &&other) {
    // If this vector is empty, just move the other.
    if (empty()) {
      *this = std::move(other);
      return;
    }

    // Otherwise, use copy-append.
    append(other);
  }

  /// Add the low N bits from the given value to the vector.
  void add(size_t numBits, uint64_t value) {
    assert(numBits <= 64);
    if (numBits == 0) return;

    if (value == 0 && isInlineAndAllClear()) {
      LengthInBits += numBits;
      return;
    }

    reserveExtra(numBits);
    static_assert(sizeof(value) <= sizeof(ChunkType),
                  "chunk too small for this, break 'value' up into "
                  "multiple parts");
    const ChunkType chunks[] = { value };
    appendReserved(numBits, chunks);
  }

  /// Append a number of clear bits to this vector.
  void appendClearBits(size_t numBits) {
    if (numBits == 0) return;

    if (isInlineAndAllClear()) {
      LengthInBits += numBits;
      return;
    }

    reserveExtra(numBits);
    appendConstantBitsReserved(numBits, 0);
  }

  /// Extend the vector out to the given length with clear bits.
  void extendWithClearBits(size_t newSize) {
    assert(newSize >= size());
    appendClearBits(newSize - size());
  }


  /// Append a number of set bits to this vector.
  void appendSetBits(size_t numBits) {
    if (numBits == 0) return;
    reserveExtra(numBits);
    appendConstantBitsReserved(numBits, 1);
  }

  /// Extend the vector out to the given length with set bits.
  void extendWithSetBits(size_t newSize) {
    assert(newSize >= size());
    appendSetBits(newSize - size());
  }

  /// Test whether a particular bit is set.
  bool operator[](size_t i) const {
    assert(i < size());
    if (isInlineAndAllClear()) return false;
    return getChunks()[i / ChunkSizeInBits]
             & (ChunkType(1) << (i % ChunkSizeInBits));
  }

  /// Intersect a bit-vector of the same size into this vector.
  ClusteredBitVector &operator&=(const ClusteredBitVector &other) {
    assert(size() == other.size());

    // If this vector is all-clear, this is a no-op.
    if (isInlineAndAllClear())
      return *this;

    // If the other vector is all-clear, we need to wipe this one.
    if (other.isInlineAndAllClear()) {
      for (auto &chunk : getChunks())
        chunk = 0;
      return *this;
    }

    // Otherwise, &= the chunks pairwise.
    auto chunks = getChunks();
    auto oi = other.getChunksPtr();
    for (auto i = chunks.begin(), e = chunks.end(); i != e; ++i, ++oi) {
      *i &= *oi;
    }
    return *this;
  }

  /// Union a bit-vector of the same size into this vector.
  ClusteredBitVector &operator|=(const ClusteredBitVector &other) {
    assert(size() == other.size());

    // If the other vector is all-clear, this is a no-op.
    if (other.isInlineAndAllClear())
      return *this;

    // If this vector is all-clear, we just copy the other.
    if (isInlineAndAllClear()) {
      return (*this = other);
    }

    // Otherwise, |= the chunks pairwise.
    auto chunks = getChunks();
    auto oi = other.getChunksPtr();
    for (auto i = chunks.begin(), e = chunks.end(); i != e; ++i, ++oi) {
      *i |= *oi;
    }
    return *this;
  }

  /// Set bit i.
  void setBit(size_t i) {
    assert(i < size());
    if (isInlineAndAllClear()) {
      reserve(LengthInBits);
    }
    getChunks()[i / ChunkSizeInBits] |= (ChunkType(1) << (i % ChunkSizeInBits));
  }

  /// Clear bit i.
  void clearBit(size_t i) {
    assert(i < size());
    if (isInlineAndAllClear()) return;
    getChunksPtr()[i / ChunkSizeInBits] &= ~(ChunkType(1) << (i % ChunkSizeInBits));
  }

  /// Toggle bit i.
  void flipBit(size_t i) {
    assert(i < size());
    if (isInlineAndAllClear()) {
      reserve(LengthInBits);
    }
    getChunksPtr()[i / ChunkSizeInBits] ^= (ChunkType(1) << (i % ChunkSizeInBits));
  }

  /// Toggle all the bits in this vector.
  void flipAll() {
    if (empty()) return;
    if (isInlineAndAllClear()) {
      reserve(LengthInBits);
    }
    for (auto &chunk : getChunks()) {
      chunk = ~chunk;
    }
    if (auto tailBits = size() % ChunkSizeInBits) {
      getChunks().back() &= ((ChunkType(1) << tailBits) - 1);
    }
  }

  /// Set the length of this vector to zero, but do not release any capacity.
  void clear() {
    LengthInBits = 0;
    if (!hasOutOfLineData())
      Data = 0;
  }

  /// Count the number of set bits in this vector.
  size_t count() const {
    if (isInlineAndAllClear()) return 0;
    size_t count = 0;
    for (ChunkType chunk : getChunks()) {
      count += llvm::countPopulation(chunk);
    }
    return count;
  }

  /// Determine if there are any bits set in this vector.
  bool any() const {
    if (isInlineAndAllClear()) return false;
    for (ChunkType chunk : getChunks()) {
      if (chunk) return true;
    }
    return false;
  }

  /// Determine if there are no bits set in this vector.
  ///
  /// \return \c !any()
  bool none() const {
    return !any();
  }

  /// A class for scanning for set bits, from low indices to high ones.
  class SetBitEnumerator {
    ChunkType CurChunk;
    const ChunkType *Chunks;
    unsigned CurChunkIndex;
    unsigned NumChunks;
  public:
    explicit SetBitEnumerator(const ClusteredBitVector &vector) {
      if (vector.isInlineAndAllClear()) {
        CurChunkIndex = 0;
        NumChunks = 0;
      } else {
        Chunks = vector.getChunksPtr();
        CurChunk = Chunks[0];
        CurChunkIndex = 0;
        NumChunks = vector.getLengthInChunks();
      }
    }

    /// Search for another bit.  Returns false if it can't find one.
    Optional<size_t> findNext() {
      if (CurChunkIndex == NumChunks) return None;
      auto cur = CurChunk;
      while (!cur) {
        if (++CurChunkIndex == NumChunks) return None;
        cur = Chunks[CurChunkIndex];
      }

      // Find the index of the lowest set bit.
      size_t bitIndex = llvm::countTrailingZeros(cur, llvm::ZB_Undefined);

      // Clear that bit in the current chunk.
      CurChunk = cur ^ (ChunkType(1) << bitIndex);
      assert(!(CurChunk & (ChunkType(1) << bitIndex)));

      return (CurChunkIndex * ChunkSizeInBits + bitIndex);
    }
  };
  SetBitEnumerator enumerateSetBits() const {
    return SetBitEnumerator(*this);
  }

  friend bool operator==(const ClusteredBitVector &lhs,
                         const ClusteredBitVector &rhs) {
    if (lhs.size() != rhs.size())
      return false;
    if (lhs.empty())
      return true;

    if (!lhs.hasOutOfLineData() && !rhs.hasOutOfLineData()) {
      return lhs.Data == rhs.Data;
    } else {
      return equalsSlowCase(lhs, rhs);
    }
  }
  friend bool operator!=(const ClusteredBitVector &lhs,
                         const ClusteredBitVector &rhs) {
    return !(lhs == rhs);
  }

  /// Return this bit-vector as an APInt, with low indices becoming
  /// the least significant bits of the number.
  llvm::APInt asAPInt() const;

  /// Construct a bit-vector from an APInt.
  static ClusteredBitVector fromAPInt(const llvm::APInt &value);

  /// Pretty-print the vector.
  void print(llvm::raw_ostream &out) const;
  void dump() const;

private:
  /// Make this object store an independent copy of the out of line
  /// data it currently stores, simply overwriting the current pointer
  /// without deleting it.
  void makeIndependentCopy() {
    assert(hasOutOfLineData());
    auto lengthToCopy = getLengthInChunks();
    allocateAndCopyFrom(getOutOfLineChunksPtr(), lengthToCopy, lengthToCopy);
  }

  /// Reallocate this vector, copying the current data into the new space.
  void reallocate(size_t newCapacityInChunks);

  ChunkType *allocate(size_t newCapacityInChunks) {
    assert(HasOutOfLineData && "bit should already be set");
    ChunkType *newData = new ChunkType[newCapacityInChunks + 1] + 1;
    newData[-1] = newCapacityInChunks * ChunkSizeInBits;
    Data = reinterpret_cast<ChunkType>(newData);
    assert(!isInlineAndAllClear());
    assert(getCapacityInChunks() == newCapacityInChunks);
    return newData;
  }

  void allocateAndCopyFrom(const ChunkType *oldData,
                           size_t newCapacityInChunks,
                           size_t numChunksToCopy) {
    auto newData = allocate(newCapacityInChunks);
    memcpy(newData, oldData, numChunksToCopy * sizeof(ChunkType));
  }

  /// Drop references to the current data.
  void dropData() {
    LengthInBits = 0;
    HasOutOfLineData = false;
    Data = 0;
  }

  /// Destroy the out of line data currently stored in this object.
  void destroy() {
    assert(hasOutOfLineData());
    delete[] (getOutOfLineChunksPtr() - 1);
  }

  /// Append a certain number of constant bits to this vector, given
  /// that it's known to contain enough capacity for them.
  void appendConstantBitsReserved(size_t numBits, bool addOnes);

  /// Append bits from the given array to this vector.
  void appendReserved(size_t numBits, const ChunkType *nextChunk);

  /// Append bits to this vector, given that it's known to contain
  /// enough capacity for them all.
  void appendReserved(size_t numBits,
                llvm::function_ref<ChunkType(size_t numBitsWanted)> generator);

  /// The slow case of equality-checking.
  static bool equalsSlowCase(const ClusteredBitVector &lhs,
                             const ClusteredBitVector &rhs);
};

} // end namespace swift

#endif // SWIFT_BASIC_CLUSTEREDBITVECTOR_H
