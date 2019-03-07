//===--- ClusteredBitVector.cpp - Out-of-line code for the bit vector -----===//
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
//  This file implements support code for ClusteredBitVector.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/ClusteredBitVector.h"

#include "llvm/ADT/APInt.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

ClusteredBitVector ClusteredBitVector::fromAPInt(const llvm::APInt &bits) {
  // This is not a very efficient algorithm.
  ClusteredBitVector result;
  for (unsigned i = 0, e = bits.getBitWidth(); i != e; ++i) {
    if (bits[i]) {
      result.appendSetBits(1);
    } else {
      result.appendClearBits(1);
    }
  }
  return result;
}

llvm::APInt ClusteredBitVector::asAPInt() const {
  if (size() == 0) {
    // APInt doesn't like zero-bit values.
    return llvm::APInt(1, 0);
  }

  if (isInlineAndAllClear()) {
    return llvm::APInt(size(), 0);
  } else {
    // This assumes that the chunk size is the same as APInt's.
    // TODO: it'd be nice to be able to do this without copying.
    return llvm::APInt(size(), getChunks());
  }
}

void ClusteredBitVector::reallocate(size_t newCapacityInChunks) {
  // If we already have out-of-line storage, the padding invariants
  // will still apply, and we just need to copy the old data into
  // the new allocation.
  if (hasOutOfLineData()) {
    auto oldData = getOutOfLineChunksPtr();
    allocateAndCopyFrom(oldData, newCapacityInChunks, getLengthInChunks());
    delete[] (oldData - 1);
    return;
  }

  // Otherwise, we might need to establish the invariants.  If we
  // were in inline-and-all-clear mode, the vector might logically
  // be much longer than a single chunk, but all-zero.
  HasOutOfLineData = true;
  auto oldDataValue = Data;
  auto newData = allocate(newCapacityInChunks);

  // All of these cases initialize 'length' chunks in newData.
  switch (auto length = getLengthInChunks()) {
  case 0:
    break;
  case 1:
    newData[0] = oldDataValue;
    break;
  default:
    assert(oldDataValue == 0 && "not previously in inline-and-all-clear?");
    memset(newData, 0, length * sizeof(ChunkType));
    break;
  }
}

void ClusteredBitVector::appendReserved(size_t numBits,
                llvm::function_ref<ChunkType(size_t numBitsWanted)> generator) {
  assert(LengthInBits + numBits <= getCapacityInBits());
  assert(numBits > 0);

  auto getMoreBits =
    [&](size_t numBitsWanted) -> ChunkType {
    auto result = generator(numBitsWanted);
    assert((numBitsWanted == ChunkSizeInBits ||
            result <= (ChunkType(1) << numBitsWanted)) &&
           "generator returned out-of-range value!");
    return result;
  };

  // Check whether the current end of the vector is a clean multiple
  // of the chunk size.
  auto offset = LengthInBits % ChunkSizeInBits;
  ChunkType *nextChunk = &getChunksPtr()[LengthInBits / ChunkSizeInBits];

  // Now we can go ahead and add in the right number of extra bits.
  LengthInBits += numBits;

  // If not, we need to combine the generator result with that last chunk.
  if (offset) {
    auto claimedBits = std::min(numBits, size_t(ChunkSizeInBits - offset));

    // The extra bits in data[chunkIndex] are guaranteed to be zero.
    *nextChunk++ |= (getMoreBits(claimedBits) << offset);
    
    numBits -= claimedBits;
    if (numBits == 0) return;
  }

  // For the rest, just generator chunks one at a time.
  do {
    auto claimedBits = std::min(numBits, size_t(ChunkSizeInBits));
    *nextChunk++ = getMoreBits(claimedBits);
    numBits -= claimedBits;
  } while (numBits);
}

void ClusteredBitVector::appendConstantBitsReserved(size_t numBits,
                                                    bool addOnes) {
  assert(LengthInBits + numBits <= getCapacityInBits());
  assert(numBits > 0);

  ChunkType pattern = (addOnes ? ~ChunkType(0) : ChunkType(0));
  appendReserved(numBits, [=](size_t numBitsWanted) -> ChunkType {
    return (pattern >> (ChunkSizeInBits - numBitsWanted));
  });
}

void ClusteredBitVector::appendReserved(size_t numBits,
                                        const ChunkType *nextChunk) {
  // This is easy if we're not currently at an offset.
  // (Note that this special case generator relies on the exact
  // implementation of the main appendReserved routine.)
  auto offset = LengthInBits % ChunkSizeInBits;
  if (!offset) {
    appendReserved(numBits, [&](size_t numBitsWanted) -> ChunkType {
      return *nextChunk++;
    });
    return;
  }

  // But if we are, we need to be constantly mixing values.
  ChunkType prevChunk = 0;
  size_t bitsRemaining = 0;
  appendReserved(numBits, [&](size_t numBitsWanted) -> ChunkType {
    auto resultMask = (numBitsWanted == ChunkSizeInBits
                         ? ~ChunkType(0)
                         : ((ChunkType(1) << numBitsWanted) - 1));

    // If we can resolve the desired bits out of the current chunk,
    // all the better.
    if (numBitsWanted <= bitsRemaining) {
      assert(numBitsWanted != ChunkSizeInBits);
      auto result = prevChunk & resultMask;
      bitsRemaining -= numBitsWanted;
      prevChunk >>= numBitsWanted;
      return result;
    }

    // |-- bitsRemaining --|-------- ChunkSizeInBits --------|
    // |     prevChunk     |             nextChunk           |
    // |------ numBitsWanted ------|----- bitsRemaining' ----|
    //                             |        prevChunk'       |

    auto newChunk = *nextChunk++;
    auto result = (prevChunk | (newChunk << bitsRemaining)) & resultMask;
    prevChunk = newChunk >> (numBitsWanted - bitsRemaining);
    bitsRemaining = ChunkSizeInBits + bitsRemaining - numBitsWanted;
    return result;
  });
}

bool ClusteredBitVector::equalsSlowCase(const ClusteredBitVector &lhs,
                                        const ClusteredBitVector &rhs) {
  assert(lhs.size() == rhs.size());
  assert(!lhs.empty() && !rhs.empty());
  assert(lhs.hasOutOfLineData() || rhs.hasOutOfLineData());

  if (!lhs.hasOutOfLineData()) {
    assert(lhs.Data == 0 || lhs.getLengthInChunks() == 1);
    for (auto chunk : rhs.getOutOfLineChunks())
      if (chunk != lhs.Data)
        return false;
    return true;
  } else if (!rhs.hasOutOfLineData()) {
    assert(rhs.Data == 0 || rhs.getLengthInChunks() == 1);
    for (auto chunk : lhs.getOutOfLineChunks())
      if (chunk != rhs.Data)
        return false;
    return true;
  } else {
    auto lhsChunks = lhs.getOutOfLineChunks();
    auto rhsChunks = rhs.getOutOfLineChunks();
    assert(lhsChunks.size() == rhsChunks.size());
    return lhsChunks == rhsChunks;
  }
}

void ClusteredBitVector::dump() const {
  print(llvm::errs());
}

/// Pretty-print the vector.
void ClusteredBitVector::print(llvm::raw_ostream &out) const {
  // Print in 8 clusters of 8 bits per row.
  for (size_t i = 0, e = size(); ; ) {
    out << ((*this)[i++] ? '1' : '0');
    if (i == e) {
      return;
    } else if ((i & 64) == 0) {
      out << '\n';
    } else if ((i & 8) == 0) {
      out << ' ';
    }
  }
}
