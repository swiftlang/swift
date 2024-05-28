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

#if SWIFT_STDLIB_ENABLE_UNICODE_DATA

#if defined(__APPLE__)
#include "Apple/NormalizationData.h"
#else
#include "Common/NormalizationData.h"
#endif

#else
#include "swift/Runtime/Debug.h"
#endif

#include "swift/shims/UnicodeData.h"
#include <stdint.h>

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint16_t _swift_stdlib_getNormData(__swift_uint32_t scalar) {
  // Fast Path: ASCII and some latiny scalars are very basic and have no
  // normalization properties.
  if (scalar < 0xC0) {
    return 0;
  }
  
#if !SWIFT_STDLIB_ENABLE_UNICODE_DATA
  swift::swift_abortDisabledUnicodeSupport();
#else
  auto dataIdx = _swift_stdlib_getScalarBitArrayIdx(scalar,
                                                    _swift_stdlib_normData,
                                                  _swift_stdlib_normData_ranks);

  // If we don't have an index into the data indices, then this scalar has no
  // normalization information.
  if (dataIdx == INTPTR_MAX) {
    return 0;
  }

  auto scalarDataIdx = _swift_stdlib_normData_data_indices[dataIdx];
  return _swift_stdlib_normData_data[scalarDataIdx];
#endif
}

#if SWIFT_STDLIB_ENABLE_UNICODE_DATA
SWIFT_RUNTIME_STDLIB_INTERNAL
const __swift_uint8_t * const _swift_stdlib_nfd_decompositions = _swift_stdlib_nfd_decomp;
#else
SWIFT_RUNTIME_STDLIB_INTERNAL
const __swift_uint8_t * const _swift_stdlib_nfd_decompositions = nullptr;
#endif

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _swift_stdlib_getDecompositionEntry(__swift_uint32_t scalar) {
#if !SWIFT_STDLIB_ENABLE_UNICODE_DATA
  swift::swift_abortDisabledUnicodeSupport();
#else
  auto levelCount = NFD_DECOMP_LEVEL_COUNT;
  __swift_intptr_t decompIdx = _swift_stdlib_getMphIdx(scalar, levelCount,
                                                  _swift_stdlib_nfd_decomp_keys,
                                                  _swift_stdlib_nfd_decomp_ranks,
                                                  _swift_stdlib_nfd_decomp_sizes);

  return _swift_stdlib_nfd_decomp_indices[decompIdx];
#endif
}

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint32_t _swift_stdlib_getComposition(__swift_uint32_t x,
                                            __swift_uint32_t y) {
#if !SWIFT_STDLIB_ENABLE_UNICODE_DATA
  swift::swift_abortDisabledUnicodeSupport();
#else
  auto levelCount = NFC_COMP_LEVEL_COUNT;
  __swift_intptr_t compIdx = _swift_stdlib_getMphIdx(y, levelCount,
                                                  _swift_stdlib_nfc_comp_keys,
                                                  _swift_stdlib_nfc_comp_ranks,
                                                  _swift_stdlib_nfc_comp_sizes);
  auto array = _swift_stdlib_nfc_comp_indices[compIdx];

  // Ensure that the first element in this array is equal to our y scalar.
  auto realY = (array[0] << 11) >> 11;

  if (y != realY) {
    return UINT32_MAX;
  }

  auto count = array[0] >> 21;

  __swift_uint32_t low = 1;
  __swift_uint32_t high = count - 1;

  while (high >= low) {
    auto idx = low + (high - low) / 2;
  
    auto entry = array[idx];
  
    // Shift the range count out of the scalar.
    auto lower = (entry << 15) >> 15;
  
    bool isNegative = entry >> 31;
    auto rangeCount = (entry << 1) >> 18;
  
    if (isNegative) {
      rangeCount = -rangeCount;
    }
  
    auto composed = lower + rangeCount;
  
    if (x == lower) {
      return composed;
    }
  
    if (x > lower) {
      low = idx + 1;
      continue;
    }
  
    if (x < lower) {
      high = idx - 1;
      continue;
    }
  }

  // If we made it out here, then our scalar was not found in the composition
  // array.
  // Return the max here to indicate that we couldn't find one.
  return UINT32_MAX;
#endif
}
