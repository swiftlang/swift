//===--- StringComparison.cpp ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/StringComparison.h"
#include "swift/Runtime/Config.h"
#include <cassert>
#include <cstdint>

#ifdef __APPLE__

#include <simd/simd.h>

static int findDiffIdxSmall(uint8_t *left, uint16_t *right, int count);

namespace {
static constexpr simd_packed_uchar16 zero = {0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                                             0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
                                             0x0, 0x0, 0x0, 0x0};
} // end anonymous namespace

/// This is equivalent to _mm_unpacklo_epi8(x, zero) but written in a manner
/// that should work across platform.
static simd_packed_ushort8 unpackLower(simd_packed_uchar16 input) {
  return (simd_packed_ushort8)__builtin_shufflevector(
      input, zero, 0, 16, 1, 17, 2, 18, 3, 19, 4, 20, 5, 21, 6, 22, 7, 23);
}

/// This is equivalent to _mm_unpackhi_epi8(x, zero) but written in a manner
/// that should work across platform.
static simd_packed_ushort8 unpackUpper(simd_packed_uchar16 input) {
  return (simd_packed_ushort8)__builtin_shufflevector(input, zero, 8, 24, 9, 25,
                                                      10, 26, 11, 27, 12, 28,
                                                      13, 29, 14, 30, 15, 31);
}

/// This is equivalent to _mm_unpacklo_epi8(lhs, zero) == rhs
static simd_packed_short8 performComparisonLow(simd_packed_uchar16 lhs,
                                               simd_packed_ushort8 rhs) {
  return unpackLower(lhs) != rhs;
}

/// This is equivalent to _mm_unpackhi_epi8(lhs, zero) == rhs
static simd_packed_short8 performComparisonHigh(simd_packed_uchar16 lhs,
                                                simd_packed_ushort8 rhs) {
  return unpackUpper(lhs) != rhs;
}

namespace {
static constexpr int ELTS_PER_ROUND = 64;

// We only want to vectorize if we have more than a page in
// size. NUM_ELTS_PER_VECTORIZATION provides that.
static constexpr int NUM_ELTS_FOR_VECTORIZATION = ELTS_PER_ROUND * 4;
} // end anonymous namespace

// *TODO* use a template to make this autoupgrade to use AVX when -mavx is
// *passed in.
static int swift_stdlib_findDiffIdx_UInt8UInt16_vector(uint8_t *left,
                                                       uint16_t *right,
                                                       int initialCount) {
  int count = initialCount;
  auto *leftVPtr = reinterpret_cast<simd_packed_uchar16 *>(left);
  auto *rightVPtr = reinterpret_cast<simd_packed_ushort8 *>(right);

  // Our main loop is going to be processing 64 elements at a time. This can be
  // seen by us processing 4 vectors of 16 elements and 8 vector of 8 elements.
  //
  // TODO: Do page size blocking.
  while (count >= ELTS_PER_ROUND) {
    auto leftV0 = leftVPtr[0];
    auto leftV1 = leftVPtr[1];
    auto leftV2 = leftVPtr[2];
    auto leftV3 = leftVPtr[3];

    auto rightV0 = rightVPtr[0];
    auto rightV1 = rightVPtr[1];
    auto rightV2 = rightVPtr[2];
    auto rightV3 = rightVPtr[3];
    auto rightV4 = rightVPtr[4];
    auto rightV5 = rightVPtr[5];
    auto rightV6 = rightVPtr[6];
    auto rightV7 = rightVPtr[7];

    // Then compare the upper/lower halves of our v16u8 vectors with v8u16
    // vectors.
    auto V0 = performComparisonLow(leftV0, rightV0);
    auto V1 = performComparisonHigh(leftV0, rightV1);
    auto V2 = performComparisonLow(leftV1, rightV2);
    auto V3 = performComparisonHigh(leftV1, rightV3);
    auto V4 = performComparisonLow(leftV2, rightV4);
    auto V5 = performComparisonHigh(leftV2, rightV5);
    auto V6 = performComparisonLow(leftV3, rightV6);
    auto V7 = performComparisonHigh(leftV3, rightV7);

    // Then reduce over vectors to see if any of our vectors were found to
    // compare not equal.
    V0 |= V1;
    V2 |= V3;
    V4 |= V5;
    V6 |= V7;

    V0 |= V2;
    V4 |= V6;

    V0 |= V4;

    // Finally if any of our vectors were found to compare not equal, then back
    // track and find the actual index. This should almost always predict false,
    // so we should only mispredict on the last iteration.
    if (simd_any(V0)) {
      // We are assumed to return the offset from the beginning of the array
      // where the error happened. We have already processed (initialCount -
      // count) at this point. So we just need to add in the index found by
      // findDiffIdxSmall.
      unsigned offset = initialCount - count;
      offset += findDiffIdxSmall(reinterpret_cast<uint8_t *>(leftVPtr),
                                 reinterpret_cast<uint16_t *>(rightVPtr),
                                 ELTS_PER_ROUND);
      return offset;
    }

    count -= 64;
    leftVPtr += 4;
    rightVPtr += 8;
  }

  return initialCount;
}

#endif

static int findDiffIdxSmall(uint8_t *left, uint16_t *right, int count) {
  for (int i = 0; i != count; ++i) {
    if (uint16_t(left[i]) != right[i]) {
      return i;
    }
  }
  return count;
}

SWIFT_RUNTIME_EXPORT
int swift_stdlib_findDiffIdx_UInt8UInt16(uint8_t *left, uint16_t *right,
                                         int count) {
  assert(count >= 0 && "Can not have a negative count here");
#ifdef __APPLE__
  // We only use the vectorized code for cases where we are working with strings
  // that are at least 1 page in size. For now lets assume 4k pages.
  if (count >= NUM_ELTS_FOR_VECTORIZATION) {
    return swift_stdlib_findDiffIdx_UInt8UInt16_vector(left, right, count);
  }
#endif
  return findDiffIdxSmall(left, right, count);
}
