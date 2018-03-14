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

namespace swift {

/// This is a small and fast implementation of memcpy with a constant count. It
/// should be a performance win for small constant values where the function
/// can be inlined, the loop unrolled and the memory accesses merged.
template <unsigned count> static void small_memcpy(void *dest, const void *src) {
  uint8_t *d8 = (uint8_t*)dest;
  const uint8_t *s8 = (const uint8_t*)src;
  for (unsigned i = 0; i < count; i++) {
    *d8++ = *s8++;
  }
}

static inline void small_memcpy(void *dest, const void *src, unsigned count,
                                bool countMaybeThree = false) {
  // This is specialization of the memcpy line below with
  // specialization for values of 1, 2 and 4.
  // memcpy(dst, src, count)
  if (count == 1) {
    small_memcpy<1>(dest, src);
  } else if (count == 2) {
    small_memcpy<2>(dest, src);
  } else if (countMaybeThree && count == 3) {
    small_memcpy<3>(dest, src);
  } else if (count == 4) {
    small_memcpy<4>(dest, src);
  } else {
    swift::crash("Tagbyte values should be 1, 2 or 4.");
  }
}

static inline void small_memset(void *dest, uint8_t value, unsigned count) {
  if (count == 1) {
    memset(dest, value, 1);
  } else if (count == 2) {
    memset(dest, value, 2);
  } else if (count == 4) {
    memset(dest, value, 4);
  } else {
    swift::crash("Tagbyte values should be 1, 2 or 4.");
  }
}

inline unsigned getNumTagBytes(size_t size, unsigned emptyCases,
                               unsigned payloadCases) {
  // We can use the payload area with a tag bit set somewhere outside of the
  // payload area to represent cases. See how many bytes we need to cover
  // all the empty cases.

  unsigned numTags = payloadCases;
  if (emptyCases > 0) {
    if (size >= 4)
      // Assume that one tag bit is enough if the precise calculation overflows
      // an int32.
      numTags += 1;
    else {
      unsigned bits = size * 8U;
      unsigned casesPerTagBitValue = 1U << bits;
      numTags += ((emptyCases + (casesPerTagBitValue-1U)) >> bits);
    }
  }
  return (numTags <=    1 ? 0 :
          numTags <   256 ? 1 :
          numTags < 65536 ? 2 : 4);
}

inline unsigned getEnumTagSinglePayloadImpl(
    const OpaqueValue *enumAddr, unsigned emptyCases, const Metadata *payload,
    size_t payloadSize, size_t payloadNumExtraInhabitants,
    int (*getExtraInhabitantIndex)(const OpaqueValue *, const Metadata *)) {

  // If there are extra tag bits, check them.
  if (emptyCases > payloadNumExtraInhabitants) {
    auto *valueAddr = reinterpret_cast<const uint8_t *>(enumAddr);
    auto *extraTagBitAddr = valueAddr + payloadSize;
    unsigned extraTagBits = 0;
    unsigned numBytes =
        getNumTagBytes(payloadSize, emptyCases - payloadNumExtraInhabitants,
                       1 /*payload case*/);

#if defined(__BIG_ENDIAN__)
    small_memcpy(reinterpret_cast<uint8_t *>(&extraTagBits) + 4 - numBytes,
                 extraTagBitAddr, numBytes);
#else
    small_memcpy(&extraTagBits, extraTagBitAddr, numBytes);
#endif

    // If the extra tag bits are zero, we have a valid payload or
    // extra inhabitant (checked below). If nonzero, form the case index from
    // the extra tag value and the value stored in the payload.
    if (extraTagBits > 0) {
      unsigned caseIndexFromExtraTagBits =
          payloadSize >= 4 ? 0 : (extraTagBits - 1U) << (payloadSize * 8U);

      // In practice we should need no more than four bytes from the payload
      // area.
      unsigned caseIndexFromValue = 0;
      unsigned numPayloadTagBytes = std::min(size_t(4), payloadSize);
#if defined(__BIG_ENDIAN__)
      if (numPayloadTagBytes)
        small_memcpy(reinterpret_cast<uint8_t *>(&caseIndexFromValue) + 4 -
                         numPayloadTagBytes,
                     valueAddr, numPayloadTagBytes, true);
#else
      if (numPayloadTagBytes)
        small_memcpy(&caseIndexFromValue, valueAddr,
                     numPayloadTagBytes, true);
#endif
      unsigned noPayloadIndex =
          (caseIndexFromExtraTagBits | caseIndexFromValue) +
          payloadNumExtraInhabitants;
      return noPayloadIndex + 1;
    }
  }

  // If there are extra inhabitants, see whether the payload is valid.
  if (payloadNumExtraInhabitants > 0) {
    return getExtraInhabitantIndex(enumAddr, payload) + 1;
  }

  // Otherwise, we have always have a valid payload.
  return 0;
}

inline void storeEnumTagSinglePayloadImpl(
    OpaqueValue *value, unsigned whichCase, unsigned emptyCases,
    const Metadata *payload, size_t payloadSize,
    size_t payloadNumExtraInhabitants,
    void (*storeExtraInhabitant)(OpaqueValue *, int whichCase,
                                 const Metadata *)) {

  auto *valueAddr = reinterpret_cast<uint8_t *>(value);
  auto *extraTagBitAddr = valueAddr + payloadSize;
  unsigned numExtraTagBytes =
      emptyCases > payloadNumExtraInhabitants
          ? getNumTagBytes(payloadSize, emptyCases - payloadNumExtraInhabitants,
                           1 /*payload case*/)
          : 0;

  // For payload or extra inhabitant cases, zero-initialize the extra tag bits,
  // if any.
  if (whichCase <= payloadNumExtraInhabitants) {
    if (numExtraTagBytes != 0)
      small_memset(extraTagBitAddr, 0, numExtraTagBytes);

    // If this is the payload case, we're done.
    if (whichCase == 0)
      return;

    // Store the extra inhabitant.
    unsigned noPayloadIndex = whichCase - 1;
    storeExtraInhabitant(value, noPayloadIndex, payload);
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
#if defined(__BIG_ENDIAN__)
  unsigned numPayloadTagBytes = std::min(size_t(4), payloadSize);
  if (numPayloadTagBytes)
    small_memcpy(valueAddr,
                 reinterpret_cast<uint8_t *>(&payloadIndex) + 4 -
                     numPayloadTagBytes,
                 numPayloadTagBytes, true);
  if (numExtraTagBytes)
    small_memcpy(extraTagBitAddr,
                 reinterpret_cast<uint8_t *>(&extraTagIndex) + 4 -
                     numExtraTagBytes,
                 numExtraTagBytes);
#else
  unsigned numPayloadTagBytes = std::min(size_t(4), payloadSize);
  if (numPayloadTagBytes)
    small_memcpy(valueAddr, &payloadIndex, numPayloadTagBytes, true);
  if (payloadSize > 4)
    memset(valueAddr + 4, 0, payloadSize - 4);
  if (numExtraTagBytes)
    small_memcpy(extraTagBitAddr, &extraTagIndex, numExtraTagBytes);
#endif
}

} /* end namespace swift */

#endif /* SWIFT_RUNTIME_ENUMIMPL_H */
