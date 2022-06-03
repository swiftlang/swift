//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "SwiftShims/UnicodeData.h"
#include <limits>

// Every 4 byte chunks of data that we need to hash (in this case only ever
// scalars and levels who are all uint32), we need to calculate K. At the end
// of this scramble sequence to get K, directly apply this to the current hash.
static inline __swift_uint32_t scramble(__swift_uint32_t scalar) {
  scalar *= 0xCC9E2D51;
  scalar = (scalar << 15) | (scalar >> 17);
  scalar *= 0x1B873593;
  return scalar;
}

// This is a reimplementation of MurMur3 hash with a modulo at the end.
static __swift_uint32_t hash(__swift_uint32_t scalar, __swift_uint32_t level,
                             __swift_uint32_t seed) {
  __swift_uint32_t hash = seed;

  hash ^= scramble(scalar);
  hash = (hash << 13) | (hash >> 19);
  hash = hash * 5 + 0xE6546B64;
  
  hash ^= scramble(level);
  hash = (hash << 13) | (hash >> 19);
  hash = hash * 5 + 0xE6546B64;

  hash ^= 8;
  hash ^= hash >> 16;
  hash *= 0x85EBCA6B;
  hash ^= hash >> 13;
  hash *= 0xC2B2AE35;
  hash ^= hash >> 16;

  return hash % level;
}

// This implementation is based on the minimal perfect hashing strategy found
// here: https://arxiv.org/pdf/1702.03154.pdf
__swift_intptr_t _swift_stdlib_getMphIdx(__swift_uint32_t scalar,
                                         __swift_intptr_t levels,
                                         const __swift_uint64_t * const *keys,
                                         const __swift_uint16_t * const *ranks,
                                         const __swift_uint16_t * const sizes) {
  __swift_intptr_t resultIdx = 0;

  // Here, levels represent the numbers of bit arrays used for this hash table.
  for (int i = 0; i != levels; i += 1) {
    auto bitArray = keys[i];

    // Get the specific bit that this scalar hashes to in the bit array.
    auto idx = (__swift_uint64_t) hash(scalar, sizes[i], i);

    auto word = bitArray[idx / 64];
    auto mask = (__swift_uint64_t) 1 << (idx % 64);

    // If our scalar's bit is turned on in the bit array, it means we no longer
    // need to iterate the bit arrays to find where our scalar is located...
    // its in this one.
    if (word & mask) {
      // Our initial rank corresponds to our current level and there are ranks
      // within each bit array every 512 bits. Say our level (bit array)
      // contains 16 uint64 integers to represent all of the required bits.
      // There would be a total of 1024 bits, so our rankings for this level
      // would contain two values for precomputed counted bits for both halfs
      // of this bit array (1024 / 512 = 2).
      auto rank = ranks[i][idx / 512];

      // Because ranks are provided every 512 bits (8 uint64s), we still need to
      // count the bits of the uints64s before us in our 8 uint64 sequence. So
      // for example, if we are bit 576, we are larger than 512, so there is a
      // provided rank for the first 8 uint64s, however we're in the second
      // 8 uint64 sequence and within said sequence we are the #2 uint64. This
      // loop will count the bits set for the first uint64 and terminate.
      for (int j = (idx / 64) & ~7; j != idx / 64; j += 1) {
        rank += __builtin_popcountll(bitArray[j]);
      }

      // After counting the other bits set in the uint64s before, its time to
      // count our word itself and the bits before us.
      if (idx % 64 > 0) {
        rank += __builtin_popcountll(word << (64 - (idx % 64)));
      }

      // Our result is the built up rank value from all of the provided ranks
      // and the ones we've manually counted ourselves.
      resultIdx = rank;
      break;
    }
  }

  return resultIdx;
}

__swift_intptr_t _swift_stdlib_getScalarBitArrayIdx(__swift_uint32_t scalar,
                                              const __swift_uint64_t *bitArrays,
                                              const __swift_uint16_t *ranks) {
  auto chunkSize = 0x110000 / 64 / 64;
  auto base = scalar / chunkSize;
  auto idx = base / 64;
  auto chunkBit = base % 64;
  
  auto quickLookSize = bitArrays[0];
  
  // If our chunk index is larger than the quick look indices, then it means
  // our scalar appears in chunks who are all 0 and trailing.
  if ((__swift_uint64_t) idx > quickLookSize - 1) {
    return std::numeric_limits<__swift_intptr_t>::max();
  }
  
  auto quickLook = bitArrays[idx + 1];
  
  if ((quickLook & ((__swift_uint64_t) 1 << chunkBit)) == 0) {
    return std::numeric_limits<__swift_intptr_t>::max();
  }
  
  // Ok, our scalar failed the quick look check. Go lookup our scalar in the
  // chunk specific bit array.
  auto chunkRank = ranks[idx];
  
  if (chunkBit != 0) {
    chunkRank += __builtin_popcountll(quickLook << (64 - chunkBit));
  }
  
  auto chunkBA = bitArrays + 1 + quickLookSize + (chunkRank * 5);
  
  auto scalarOverallBit = scalar - (base * chunkSize);
  auto scalarSpecificBit = scalarOverallBit % 64;
  auto scalarWord = scalarOverallBit / 64;
  
  auto chunkWord = chunkBA[scalarWord];
  
  // If our scalar specifically is not turned on, then we're done.
  if ((chunkWord & ((__swift_uint64_t) 1 << scalarSpecificBit)) == 0) {
    return std::numeric_limits<__swift_intptr_t>::max();
  }
  
  auto scalarRank = ranks[quickLookSize + (chunkRank * 5) + scalarWord];
  
  if (scalarSpecificBit != 0) {
    scalarRank += __builtin_popcountll(chunkWord << (64 - scalarSpecificBit));
  }
  
  auto chunkDataIdx = chunkBA[4] >> 16;

  return chunkDataIdx + scalarRank;
}
