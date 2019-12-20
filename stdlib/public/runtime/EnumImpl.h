//===--- EnumImpl.h - Enum implementation runtime declarations --*- C++ -*-===//
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
// Enum implementation details declarations of the Swift runtime.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_RUNTIME_ENUMIMPL_H
#define SWIFT_RUNTIME_ENUMIMPL_H

#include "swift/ABI/Enum.h"
#include "swift/Runtime/Enum.h"

namespace swift {

/// Store the given 4-byte unsigned integer value into the variable-
/// length destination buffer. The value will be zero extended or
/// truncated to fit in the buffer.
static inline void storeEnumElement(uint8_t *dst,
                                    uint32_t value,
                                    size_t size) {
  // Note: we use fixed size memcpys to encourage the compiler to
  // optimize them into unaligned stores.
  switch (size) {
  case 0:
    return;
  case 1:
    dst[0] = uint8_t(value);
    return;
  case 2:
#if defined(__BIG_ENDIAN__)
    value <<= 16;
#endif
    memcpy(dst, &value, 2);
    return;
  case 3:
#if defined(__BIG_ENDIAN__)
    value <<= 8;
#endif
    memcpy(dst, &value, 3);
    return;
  case 4:
    memcpy(dst, &value, 4);
    return;
  }
  // Store zero extended value in the destination.
#if defined(__BIG_ENDIAN__)
  memset(&dst[0], 0, size - 4);
  memcpy(&dst[size - 4], &value, 4);
#else
  memcpy(&dst[0], &value, 4);
  memset(&dst[4], 0, size - 4);
#endif
}

/// Load a 4-byte unsigned integer value from the variable-length
/// source buffer. The value will be zero-extended or truncated to fit
/// into the returned value.
static inline uint32_t loadEnumElement(const uint8_t *src,
                                       size_t size) {
  // Note: we use fixed size memcpys to encourage the compiler to
  // optimize them into unaligned loads.
  uint32_t result = 0;
  switch (size) {
  case 0:
    return 0;
  case 1:
    return uint32_t(src[0]);
  case 2:
    memcpy(&result, src, 2);
#if defined(__BIG_ENDIAN__)
    result >>= 16;
#endif
    return result;
  case 3:
    memcpy(&result, src, 3);
#if defined(__BIG_ENDIAN__)
    result >>= 8;
#endif
    return result;
  case 4:
    memcpy(&result, src, 4);
    return result;
  }
  // Load value by truncating the source to 4 bytes.
#if defined(__BIG_ENDIAN__)
  memcpy(&result, &src[size - 4], 4);
#else
  memcpy(&result, &src[0], 4);
#endif
  return result;
}

inline unsigned getEnumTagSinglePayloadImpl(
    const OpaqueValue *enumAddr, unsigned emptyCases, const Metadata *payload,
    size_t payloadSize, unsigned payloadNumExtraInhabitants,
    getExtraInhabitantTag_t *getExtraInhabitantTag) {

  // If there are extra tag bits, check them.
  if (emptyCases > payloadNumExtraInhabitants) {
    auto *valueAddr = reinterpret_cast<const uint8_t *>(enumAddr);
    auto *extraTagBitAddr = valueAddr + payloadSize;
    unsigned numBytes =
        getEnumTagCounts(payloadSize,
                         emptyCases - payloadNumExtraInhabitants,
                         1 /*payload case*/).numTagBytes;

    unsigned extraTagBits = loadEnumElement(extraTagBitAddr, numBytes);

    // If the extra tag bits are zero, we have a valid payload or
    // extra inhabitant (checked below). If nonzero, form the case index from
    // the extra tag value and the value stored in the payload.
    if (extraTagBits > 0) {
      unsigned caseIndexFromExtraTagBits =
          payloadSize >= 4 ? 0 : (extraTagBits - 1U) << (payloadSize * 8U);
      unsigned caseIndexFromValue = loadEnumElement(valueAddr, payloadSize);
      unsigned noPayloadIndex =
          (caseIndexFromExtraTagBits | caseIndexFromValue) +
          payloadNumExtraInhabitants;
      return noPayloadIndex + 1;
    }
  }

  // If there are extra inhabitants, see whether the payload is valid.
  if (payloadNumExtraInhabitants > 0) {
    return getExtraInhabitantTag(enumAddr, payloadNumExtraInhabitants, payload);
  }

  // Otherwise, we have always have a valid payload.
  return 0;
}

inline void storeEnumTagSinglePayloadImpl(
    OpaqueValue *value, unsigned whichCase, unsigned emptyCases,
    const Metadata *payload, size_t payloadSize,
    unsigned payloadNumExtraInhabitants,
    storeExtraInhabitantTag_t *storeExtraInhabitantTag) {

  auto *valueAddr = reinterpret_cast<uint8_t *>(value);
  auto *extraTagBitAddr = valueAddr + payloadSize;
  unsigned numExtraTagBytes =
      emptyCases > payloadNumExtraInhabitants
          ? getEnumTagCounts(payloadSize,
                             emptyCases - payloadNumExtraInhabitants,
                             1 /*payload case*/).numTagBytes
          : 0;

  // For payload or extra inhabitant cases, zero-initialize the extra tag bits,
  // if any.
  if (whichCase <= payloadNumExtraInhabitants) {
    if (numExtraTagBytes != 0)
      storeEnumElement(extraTagBitAddr, 0, numExtraTagBytes);

    // If this is the payload case, we're done.
    if (whichCase == 0)
      return;

    // Store the extra inhabitant.
    storeExtraInhabitantTag(value, whichCase, payloadNumExtraInhabitants,
                            payload);
    return;
  }

  // Factor the case index into payload and extra tag parts.
  unsigned noPayloadIndex = whichCase - 1;
  unsigned caseIndex = noPayloadIndex - payloadNumExtraInhabitants;
  unsigned payloadIndex, extraTagIndex;
  if (payloadSize >= 4) {
    extraTagIndex = 1;
    payloadIndex = caseIndex;
  } else {
    unsigned payloadBits = payloadSize * 8U;
    extraTagIndex = 1U + (caseIndex >> payloadBits);
    payloadIndex = caseIndex & ((1U << payloadBits) - 1U);
  }

    // Store into the value.
  if (payloadSize)
    storeEnumElement(valueAddr, payloadIndex, payloadSize);
  if (numExtraTagBytes)
    storeEnumElement(extraTagBitAddr, extraTagIndex, numExtraTagBytes);
}

} /* end namespace swift */

#endif /* SWIFT_RUNTIME_ENUMIMPL_H */
