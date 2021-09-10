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

#include "../SwiftShims/UnicodeData.h"

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

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_intptr_t _swift_stdlib_getMphIdx(__swift_uint32_t scalar,
                                         __swift_intptr_t levels,
                                         const __swift_uint64_t * const *keys,
                                         const __swift_uint16_t * const *ranks,
                                         const __swift_uint16_t * const sizes) {
  __swift_intptr_t resultIdx = 0;

  for (int i = 0; i != levels; i += 1) {
    auto bitArray = keys[i];

    auto idx = (__swift_uint64_t) hash(scalar, sizes[i], i);

    auto word = bitArray[idx / 64];
    auto mask = (__swift_uint64_t) 1 << (idx % 64);

    if (word & mask) {
      auto rank = ranks[i][idx / 512];

      for (int j = (idx / 64) & ~7; j != idx / 64; j += 1) {
        rank += __builtin_popcountll(bitArray[j]);
      }

      auto finalWord = bitArray[idx / 64];

      if (idx % 64 > 0) {
        rank += __builtin_popcountll(finalWord << (64 - (idx % 64)));
      }

      resultIdx = rank;
      break;
    }
  }

  return resultIdx;
}
