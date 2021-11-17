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

#include "UnicodeScalarProps.h"
#include "../SwiftShims/UnicodeData.h"
#include <limits>
#include <iostream>

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint64_t _swift_stdlib_getBinaryProperties(__swift_uint32_t scalar) {

  auto lowerBoundIndex = 0;
  auto endIndex = 4855;
  auto upperBoundIndex = endIndex - 1;

  while (upperBoundIndex >= lowerBoundIndex) {
    auto index = lowerBoundIndex + (upperBoundIndex - lowerBoundIndex) / 2;

    auto entry = _swift_stdlib_scalar_binProps[index];

    // Shift the ccc value out of the scalar.
    auto lowerBoundScalar = (entry << 11) >> 11;

    __swift_uint32_t upperBoundScalar = 0;

    // If we're not at the end of the array, the range count is simply the
    // distance to the next element.
    if (index != endIndex - 1) {
      auto nextEntry = _swift_stdlib_scalar_binProps[index + 1];

      auto nextLower = (nextEntry << 11) >> 11;

      upperBoundScalar = nextLower - 1;
    } else {
      // Otherwise, the range count is the distance to 0x10FFFF
      upperBoundScalar = 0x10FFFF;
    }

    // Shift everything out.
    auto dataIndex = entry >> 21;

    if (scalar >= lowerBoundScalar && scalar <= upperBoundScalar) {
      return  _swift_stdlib_scalar_binProps_data[dataIndex];
    }

    if (scalar > upperBoundScalar) {
      lowerBoundIndex = index + 1;
      continue;
    }

    if (scalar < lowerBoundScalar) {
      upperBoundIndex = index - 1;
      continue;
    }
  }

  // If we make it out of this loop, then it means the scalar was not found at
  // all in the array. This should never happen because the array represents all
  // scalars from 0x0 to 0x10FFFF, but if somehow this branch gets reached,
  // return 0 to indicate no properties.
  return 0;
}

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_uint8_t _swift_stdlib_getNumericType(__swift_uint32_t scalar) {
  auto lowerBoundIndex = 0;
  auto endIndex = 233;
  auto upperBoundIndex = endIndex - 1;

  while (upperBoundIndex >= lowerBoundIndex) {
    auto idx = lowerBoundIndex + (upperBoundIndex - lowerBoundIndex) / 2;

    auto entry = _swift_stdlib_numeric_type[idx];

    auto lowerBoundScalar = (entry << 11) >> 11;
    auto rangeCount = (entry << 3) >> 24;
    auto upperBoundScalar = lowerBoundScalar + rangeCount;

    auto numericType = (__swift_uint8_t)(entry >> 29);

    if (scalar >= lowerBoundScalar && scalar <= upperBoundScalar) {
      return numericType;
    }

    if (scalar > upperBoundScalar) {
      lowerBoundIndex = idx + 1;
      continue;
    }

    if (scalar < lowerBoundScalar) {
      upperBoundIndex = idx - 1;
      continue;
    }
  }

  // If we made it out here, then our scalar was not found in the composition
  // array.
  // Return the max here to indicate that we couldn't find one.
  return std::numeric_limits<__swift_uint8_t>::max();
}

SWIFT_RUNTIME_STDLIB_INTERNAL
double _swift_stdlib_getNumericValue(__swift_uint32_t scalar) {
  __swift_intptr_t scalarIdx = _swift_stdlib_getMphIdx(scalar, 11,
                                                  _swift_stdlib_numeric_values_keys,
                                                  _swift_stdlib_numeric_values_ranks,
                                                  _swift_stdlib_numeric_values_sizes);

  auto valueIdx = _swift_stdlib_numeric_values_indices[scalarIdx];
  return _swift_stdlib_numeric_values[valueIdx];
}

SWIFT_RUNTIME_STDLIB_INTERNAL
const char *_swift_stdlib_getNameAlias(__swift_uint32_t scalar) {
  auto dataIdx = _swift_stdlib_getScalarBitArrayIdx(scalar,
                                                    _swift_stdlib_nameAlias,
                                                  _swift_stdlib_nameAlias_ranks);

  if (dataIdx == std::numeric_limits<__swift_intptr_t>::max()) {
    return nullptr;
  }

  return _swift_stdlib_nameAlias_data[dataIdx];
}

SWIFT_RUNTIME_STDLIB_INTERNAL
__swift_int32_t _swift_stdlib_getMapping(__swift_uint32_t scalar,
                                         __swift_uint8_t mapping) {
  auto dataIdx = _swift_stdlib_getScalarBitArrayIdx(scalar,
                                                    _swift_stdlib_mappings,
                                                  _swift_stdlib_mappings_ranks);

  if (dataIdx == std::numeric_limits<__swift_intptr_t>::max()) {
    return 0;
  }

  auto mappings = _swift_stdlib_mappings_data_indices[dataIdx];

  __swift_uint8_t mappingIdx;

  switch (mapping) {
    // Uppercase
    case 0:
      mappingIdx = mappings & 0xFF;
      break;

    // Lowercase
    case 1:
      mappingIdx = (mappings & 0xFF00) >> 8;
      break;

    // Titlecase
    case 2:
      mappingIdx = (mappings & 0xFF0000) >> 16;
      break;

    // Unknown mapping
    default:
      return 0;
  }

  if (mappingIdx == 0xFF) {
    return 0;
  }

  return _swift_stdlib_mappings_data[mappingIdx];
}

SWIFT_RUNTIME_STDLIB_INTERNAL
const __swift_uint32_t *_swift_stdlib_getSpecialMapping(__swift_uint32_t scalar) {
  auto dataIdx = _swift_stdlib_getScalarBitArrayIdx(scalar,
                                                 _swift_stdlib_special_mappings,
                                          _swift_stdlib_special_mappings_ranks);

  if (dataIdx == std::numeric_limits<__swift_intptr_t>::max()) {
    return nullptr;
  }

  auto index = _swift_stdlib_special_mappings_data_indices[dataIdx];

  return _swift_stdlib_special_mappings_data + index;
}
