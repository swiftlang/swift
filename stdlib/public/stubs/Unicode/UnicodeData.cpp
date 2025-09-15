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

#include "swift/shims/UnicodeData.h"
#include <stdint.h>

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
      // would contain two values for precomputed counted bits for both halves
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

// A scalar bit array is represented using a combination of quick look bit
// arrays and specific bit arrays expanding these quick look arrays. There's
// usually a few data structures accompanying these bit arrays like ranks, data
// indices, and an actual data array.
//
// The bit arrays are constructed to look somewhat like the following:
//
//     [quickLookSize, {uint64 * quickLookSize}, {5 * uint64}, {5 * uint64},
//      {5 * uint64}...]
//
// where the number of {5 * uint64} (a specific bit array) is equal to the
// number of bits turned on within the {uint64 * quickLookSize}. This can be
// easily calculated using the passed in ranks arrays who looks like the
// following:
//
//     [{uint16 * quickLookSize}, {5 * uint16}, {5 * uint16}, {5 * uint16}...]
//
// which is the same exact scheme as the bit arrays. Ranks contain the number of
// previously turned on bits according their respectful {}. For instance, each
// chunk, {5 * uint16}, begins with 0x0 and continuously grows as the number of
// bits within the chunk turn on. An example sequence of this looks like:
// [0x0, 0x0, 0x30, 0x70, 0xB0] where the first uint64 obviously doesn't have a
// previous uint64 to look at, so its rank is 0. The second uint64's rank will
// be the number of bits turned on in the first uint64, which in this case is
// also 0. The third uint64's rank is 0x30 meaning there were 48 bits turned on
// from the first uint64 through the second uint64.
__swift_intptr_t _swift_stdlib_getScalarBitArrayIdx(__swift_uint32_t scalar,
                                              const __swift_uint64_t *bitArrays,
                                              const __swift_uint16_t *ranks) {
  // Chunk size indicates the number of scalars in a singular bit in our quick
  // look arrays. Currently, a chunk consists of 272 scalars being represented
  // in a bit. 0x110000 represents the maximum scalar value that Unicode will
  // never go over (or at least promised to never go over), 0x10FFFF, plus 1.
  // There are 64 bit arrays allocated for the quick look search and within
  // each bit array is an allocated 64 bits (8 bytes). Assuming the whole quick
  // search array is allocated and used, this would mean 512 bytes are used
  // solely for these arrays.
  auto chunkSize = 0x110000 / 64 / 64;

  // Our base is the specific bit in the context of all of the bit arrays that
  // holds our scalar. Considering there are 64 bit arrays of 64 bits, that
  // would mean there are 64 * 64 = 4096 total bits to represent all scalars.
  auto base = scalar / chunkSize;

  // Index is our specific bit array that holds our bit.
  auto idx = base / 64;

  // Chunk bit is the specific bit within the bit array for our scalar.
  auto chunkBit = base % 64;

  // At the beginning our bit arrays is a number indicating the number of
  // actually implemented quick look bit arrays. We do this to save a little bit
  // of code size for bit arrays towards the end that usually contain no
  // properties, thus their bit arrays are most likely 0 or null.
  auto quickLookSize = bitArrays[0];

  // If our chunk index is larger than the quick look indices, then it means
  // our scalar appears in chunks who are all 0 and trailing.
  if ((__swift_uint64_t) idx > quickLookSize - 1) {
    return INTPTR_MAX;
  }

  // Our scalar actually exists in a quick look bit array that was implemented.
  auto quickLook = bitArrays[idx + 1];

  // If the quick look array has our chunk bit not set, that means all 272
  // (chunkSize) of the scalars being represented have no property and ours is
  // one of them.
  if ((quickLook & ((__swift_uint64_t) 1 << chunkBit)) == 0) {
    return INTPTR_MAX;
  }

  // Ok, our scalar failed the quick look check. Go lookup our scalar in the
  // chunk specific bit array. Ranks keeps track of the previous bit array's
  // number of non zero bits and is iterative.
  //
  // For example, [1, 3, 10] are bit arrays who have certain number of bits
  // turned on. The generated ranks array would look like [0, 1, 3] because
  // the first value, 1, does not have any previous bit array to look at so its
  // number of ranks are 0. 3 on the other hand will see its rank value as 1
  // because the previous value had 1 bit turned on. 10 will see 3 because it is
  // seeing both 1 and 3's number of turned on bits (3 has 2 bits on and
  // 1 + 2 = 3).
  auto chunkRank = ranks[idx];

  // If our specific bit within the chunk isn't the first bit, then count the
  // number of bits turned on preceding our chunk bit.
  if (chunkBit != 0) {
    chunkRank += __builtin_popcountll(quickLook << (64 - chunkBit));
  }

  // Each bit that is turned on in the quick look arrays is given a bit array
  // that consists of 5 64 bit integers (5 * 64 = 320 which is enough to house
  // at least 272 specific bits dedicated to each scalar within a chunk). Our
  // specific chunk's array is located at:
  // 1 (quick look count)
  // +
  // quickLookSize (number of actually implemented quick look arrays)
  // +
  // chunkRank * 5 (where chunkRank is the total number of bits turned on
  // before ours and each chunk is given 5 uint64s)
  auto chunkBA = bitArrays + 1 + quickLookSize + (chunkRank * 5);

  // Our overall bit represents the bit within 0 - 271 (272 total, our
  // chunkSize) that houses our scalar.
  auto scalarOverallBit = scalar - (base * chunkSize);

  // And our specific bit here represents the bit that houses our scalar inside
  // a specific uint64 in our overall bit array.
  auto scalarSpecificBit = scalarOverallBit % 64;

  // Our word here is the index into the chunk's bit array to grab the specific
  // uint64 who houses a bit representing our scalar.
  auto scalarWord = scalarOverallBit / 64;

  auto chunkWord = chunkBA[scalarWord];

  // If our scalar specifically is not turned on within our chunk's bit array,
  // then we know for sure that our scalar does not inhibit this property.
  if ((chunkWord & ((__swift_uint64_t) 1 << scalarSpecificBit)) == 0) {
    return INTPTR_MAX;
  }

  // Otherwise, this scalar does have whatever property this scalar array is
  // representing. Our ranks also holds bit information for a chunk's bit array,
  // so each chunk is given 5 uint16 in our ranks to count its own bits.
  auto scalarRank = ranks[quickLookSize + (chunkRank * 5) + scalarWord];

  // Again, if our scalar isn't the first bit in its uint64, then count the
  // proceeding number of bits turned on in our uint64.
  if (scalarSpecificBit != 0) {
    scalarRank += __builtin_popcountll(chunkWord << (64 - scalarSpecificBit));
  }

  // In our last uint64 in our bit array, there is an index into our data index
  // array. Because we only need 272 bits for the scalars, any remaining bits
  // can be used for essentially whatever. 5 * 64 bits = 320 bits and we only
  // allocate 16 bits in the last uint64 for the remaining scalars
  // (4 * 64 bits = 256 + 16 = 272 (chunkSize)) leaving us with 48 spare bits.
  auto chunkDataIdx = chunkBA[4] >> 16;

  // Finally, our index (or rather whatever value is stored in our spare bits)
  // is simply the start of our chunk's index plus the specific rank for our
  // scalar.
  return chunkDataIdx + scalarRank;
}
