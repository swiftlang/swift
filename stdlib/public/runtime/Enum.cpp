//===--- Enum.cpp - Runtime declarations for enums ------------------------===//
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
// Swift runtime functions in support of enums.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/MathExtras.h"
#include "swift/Basic/Fallthrough.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"
#include <cstring>
#include <algorithm>

using namespace swift;

static unsigned getNumTagBytes(size_t size, unsigned emptyCases,
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

/// This is a small and fast implementation of memcpy with a constant count. It
/// should be a performance win for small constant values where the function
/// can be inlined, the loop unrolled and the memory accesses merged.
template <unsigned count> static void small_memcpy(void *dest, const void *src) {
  uint8_t *d8 = (uint8_t*)dest, *s8 = (uint8_t*)src;
  for (unsigned i = 0; i < count; i++) {
    *d8++ = *s8++;
  }
}

static inline void small_memcpy(void *dest, const void *src, unsigned count) {
  // This is specialization of the memcpy line below with
  // specialization for values of 1, 2 and 4.
  // memcpy(dst, src, count)
  if (count == 1) {
    small_memcpy<1>(dest, src);
  } else if (count == 2) {
    small_memcpy<2>(dest, src);
  } else if (count == 4) {
    small_memcpy<4>(dest, src);
  } else {
    crash("Tagbyte values should be 1, 2 or 4.");
  }
}

void
swift::swift_initEnumValueWitnessTableSinglePayload(ValueWitnessTable *vwtable,
                                                const TypeLayout *payloadLayout,
                                                unsigned emptyCases) {
  size_t payloadSize = payloadLayout->size;
  unsigned payloadNumExtraInhabitants
    = payloadLayout->getNumExtraInhabitants();
  
  unsigned unusedExtraInhabitants = 0;
  
  // If there are enough extra inhabitants for all of the cases, then the size
  // of the enum is the same as its payload.
  size_t size;
  if (payloadNumExtraInhabitants >= emptyCases) {
    size = payloadSize;
    unusedExtraInhabitants = payloadNumExtraInhabitants - emptyCases;
  } else {
    size = payloadSize + getNumTagBytes(payloadSize,
                                      emptyCases - payloadNumExtraInhabitants,
                                      1 /*payload case*/);
  }
  
  size_t align = payloadLayout->flags.getAlignment();
  vwtable->size = size;
  vwtable->flags = payloadLayout->flags
    .withExtraInhabitants(unusedExtraInhabitants > 0)
    .withEnumWitnesses(true)
    .withInlineStorage(ValueWitnessTable::isValueInline(size, align));
  auto rawStride = llvm::alignTo(size, align);
  vwtable->stride = rawStride == 0 ? 1 : rawStride;
  
  // Substitute in better common value witnesses if we have them.
  // If the payload type is a single-refcounted pointer, and the enum has
  // a single empty case, then we can borrow the witnesses of the single
  // refcounted pointer type, since swift_retain and objc_retain are both
  // nil-aware. Most single-refcounted types will use the standard
  // value witness tables for NativeObject or UnknownObject. This isn't
  // foolproof but should catch the common case of optional class types.
#if OPTIONAL_OBJECT_OPTIMIZATION
  auto payloadVWT = payload->getValueWitnesses();
  if (emptyCases == 1
      && (payloadVWT == &VALUE_WITNESS_SYM(Bo)
#if SWIFT_OBJC_INTEROP
          || payloadVWT == &VALUE_WITNESS_SYM(BO)
#endif
          )) {
#define COPY_PAYLOAD_WITNESS(NAME) vwtable->NAME = payloadVWT->NAME;
    FOR_ALL_FUNCTION_VALUE_WITNESSES(COPY_PAYLOAD_WITNESS)
#undef COPY_PAYLOAD_WITNESS
  } else {
#endif
    installCommonValueWitnesses(vwtable);
#if OPTIONAL_OBJECT_OPTIMIZATION
  }
#endif


  // If the payload has extra inhabitants left over after the ones we used,
  // forward them as our own.
  if (unusedExtraInhabitants > 0) {
    auto xiVWTable = static_cast<ExtraInhabitantsValueWitnessTable*>(vwtable);
    xiVWTable->extraInhabitantFlags = ExtraInhabitantFlags()
      .withNumExtraInhabitants(unusedExtraInhabitants);
  }
}

int swift::swift_getEnumCaseSinglePayload(const OpaqueValue *value,
                                          const Metadata *payload,
                                          unsigned emptyCases)
  SWIFT_CC(RegisterPreservingCC_IMPL) {
  auto *payloadWitnesses = payload->getValueWitnesses();
  auto payloadSize = payloadWitnesses->getSize();
  auto payloadNumExtraInhabitants = payloadWitnesses->getNumExtraInhabitants();

  // If there are extra tag bits, check them.
  if (emptyCases > payloadNumExtraInhabitants) {
    auto *valueAddr = reinterpret_cast<const uint8_t*>(value);
    auto *extraTagBitAddr = valueAddr + payloadSize;
    unsigned extraTagBits = 0;
    unsigned numBytes = getNumTagBytes(payloadSize,
                                       emptyCases-payloadNumExtraInhabitants,
                                       1 /*payload case*/);

#if defined(__BIG_ENDIAN__)
    small_memcpy(reinterpret_cast<uint8_t*>(&extraTagBits) + 4 - numBytes,
                 extraTagBitAddr, numBytes);
#else
    small_memcpy(&extraTagBits, extraTagBitAddr, numBytes);
#endif

    // If the extra tag bits are zero, we have a valid payload or
    // extra inhabitant (checked below). If nonzero, form the case index from
    // the extra tag value and the value stored in the payload.
    if (extraTagBits > 0) {
      unsigned caseIndexFromExtraTagBits = payloadSize >= 4
        ? 0 : (extraTagBits - 1U) << (payloadSize*8U);

      // In practice we should need no more than four bytes from the payload
      // area.
      unsigned caseIndexFromValue = 0;
#if defined(__BIG_ENDIAN__)
      unsigned numPayloadTagBytes = std::min(size_t(4), payloadSize);
      memcpy(reinterpret_cast<uint8_t*>(&caseIndexFromValue) + 4 -
             numPayloadTagBytes, valueAddr, numPayloadTagBytes);
#else
      memcpy(&caseIndexFromValue, valueAddr,
             std::min(size_t(4), payloadSize));
#endif
      return (caseIndexFromExtraTagBits | caseIndexFromValue)
        + payloadNumExtraInhabitants;
    }
  }

  // If there are extra inhabitants, see whether the payload is valid.
  if (payloadNumExtraInhabitants > 0) {
    return
      static_cast<const ExtraInhabitantsValueWitnessTable*>(payloadWitnesses)
      ->getExtraInhabitantIndex(value, payload);
  }

  // Otherwise, we have always have a valid payload.
  return -1;
}

void swift::swift_storeEnumTagSinglePayload(OpaqueValue *value,
                                            const Metadata *payload,
                                            int whichCase, unsigned emptyCases)
  SWIFT_CC(RegisterPreservingCC_IMPL) {
  auto *payloadWitnesses = payload->getValueWitnesses();
  auto payloadSize = payloadWitnesses->getSize();
  unsigned payloadNumExtraInhabitants
    = payloadWitnesses->getNumExtraInhabitants();

  auto *valueAddr = reinterpret_cast<uint8_t*>(value);
  auto *extraTagBitAddr = valueAddr + payloadSize;
  unsigned numExtraTagBytes = emptyCases > payloadNumExtraInhabitants
    ? getNumTagBytes(payloadSize, emptyCases - payloadNumExtraInhabitants,
                     1 /*payload case*/)
    : 0;

  // For payload or extra inhabitant cases, zero-initialize the extra tag bits,
  // if any.
  if (whichCase < (int)payloadNumExtraInhabitants) {
    // The two most common values for numExtraTagBytes are zero and one.
    // Try to avoid calling bzero by specializing for these values.
    if (numExtraTagBytes != 0) {
      if (numExtraTagBytes == 1) {
        // Zero a single byte.
        *((char*)(extraTagBitAddr)) = 0;
      } else {
        // Zero the buffer.
        memset(extraTagBitAddr, 0, numExtraTagBytes);
      }
    }

    // If this is the payload case, we're done.
    if (whichCase == -1)
      return;
    
    // Store the extra inhabitant.
    static_cast<const ExtraInhabitantsValueWitnessTable*>(payloadWitnesses)
      ->storeExtraInhabitant(value, whichCase, payload);
    return;
  }
  
  // Factor the case index into payload and extra tag parts.
  unsigned caseIndex = whichCase - payloadNumExtraInhabitants;
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
  memcpy(valueAddr,
         reinterpret_cast<uint8_t*>(&payloadIndex) + 4 - numPayloadTagBytes,
         numPayloadTagBytes);
  if (payloadSize > 4)
    memset(valueAddr + 4, 0, payloadSize - 4);
  memcpy(extraTagBitAddr,
         reinterpret_cast<uint8_t*>(&extraTagIndex) + 4 - numExtraTagBytes,
         numExtraTagBytes);
#else
  memcpy(valueAddr, &payloadIndex, std::min(size_t(4), payloadSize));
  if (payloadSize > 4)
    memset(valueAddr + 4, 0, payloadSize - 4);
  memcpy(extraTagBitAddr, &extraTagIndex, numExtraTagBytes);
#endif
}

void
swift::swift_initEnumMetadataMultiPayload(ValueWitnessTable *vwtable,
                                     EnumMetadata *enumType,
                                     unsigned numPayloads,
                                     const TypeLayout * const *payloadLayouts) {
  // Accumulate the layout requirements of the payloads.
  size_t payloadSize = 0, alignMask = 0;
  bool isPOD = true, isBT = true;
  for (unsigned i = 0; i < numPayloads; ++i) {
    const TypeLayout *payloadLayout = payloadLayouts[i];
    payloadSize
      = std::max(payloadSize, (size_t)payloadLayout->size);
    alignMask |= payloadLayout->flags.getAlignmentMask();
    isPOD &= payloadLayout->flags.isPOD();
    isBT &= payloadLayout->flags.isBitwiseTakable();
  }
  
  // Store the max payload size in the metadata.
  assignUnlessEqual(enumType->getPayloadSize(), payloadSize);
  
  // The total size includes space for the tag.
  unsigned totalSize = payloadSize + getNumTagBytes(payloadSize,
                                enumType->Description->Enum.getNumEmptyCases(),
                                numPayloads);
  
  // Set up the layout info in the vwtable.
  vwtable->size = totalSize;
  vwtable->flags = ValueWitnessFlags()
    .withAlignmentMask(alignMask)
    .withPOD(isPOD)
    .withBitwiseTakable(isBT)
    // TODO: Extra inhabitants
    .withExtraInhabitants(false)
    .withEnumWitnesses(true)
    .withInlineStorage(ValueWitnessTable::isValueInline(totalSize, alignMask+1))
    ;
  auto rawStride = (totalSize + alignMask) & ~alignMask;
  vwtable->stride = rawStride == 0 ? 1 : rawStride;
  
  installCommonValueWitnesses(vwtable);
}

namespace {
struct MultiPayloadLayout {
  size_t payloadSize;
  size_t numTagBytes;
};
} // end anonymous namespace

static MultiPayloadLayout getMultiPayloadLayout(const EnumMetadata *enumType) {
  size_t payloadSize = enumType->getPayloadSize();
  size_t totalSize = enumType->getValueWitnesses()->size;
  return {payloadSize, totalSize - payloadSize};
}

static void storeMultiPayloadTag(OpaqueValue *value,
                                 MultiPayloadLayout layout,
                                 unsigned tag) {
  auto tagBytes = reinterpret_cast<char *>(value) + layout.payloadSize;
#if defined(__BIG_ENDIAN__)
  small_memcpy(tagBytes,
               reinterpret_cast<char *>(&tag) + 4 - layout.numTagBytes,
               layout.numTagBytes);
#else
  small_memcpy(tagBytes, &tag, layout.numTagBytes);
#endif
}

static void storeMultiPayloadValue(OpaqueValue *value,
                                   MultiPayloadLayout layout,
                                   unsigned payloadValue) {
  auto bytes = reinterpret_cast<char *>(value);
#if defined(__BIG_ENDIAN__)
  unsigned numPayloadValueBytes =
      std::min(layout.payloadSize, sizeof(payloadValue));
  memcpy(bytes + sizeof(OpaqueValue *) - numPayloadValueBytes,
         reinterpret_cast<char *>(&payloadValue) + 4 - numPayloadValueBytes,
         numPayloadValueBytes);
  if (layout.payloadSize > sizeof(payloadValue) &&
      layout.payloadSize > sizeof(OpaqueValue *)) {
    memset(bytes, 0,
           sizeof(OpaqueValue *) - numPayloadValueBytes);
    memset(bytes + sizeof(OpaqueValue *), 0,
           layout.payloadSize - sizeof(OpaqueValue *));
  }
#else
  memcpy(bytes, &payloadValue,
         std::min(layout.payloadSize, sizeof(payloadValue)));

  // If the payload is larger than the value, zero out the rest.
  if (layout.payloadSize > sizeof(payloadValue))
    memset(bytes + sizeof(payloadValue), 0,
           layout.payloadSize - sizeof(payloadValue));
#endif
}

static unsigned loadMultiPayloadTag(const OpaqueValue *value,
                                    MultiPayloadLayout layout) {
  auto tagBytes = reinterpret_cast<const char *>(value) + layout.payloadSize;

  unsigned tag = 0;
#if defined(__BIG_ENDIAN__)
  small_memcpy(reinterpret_cast<char *>(&tag) + 4 - layout.numTagBytes,
               tagBytes, layout.numTagBytes);
#else
  small_memcpy(&tag, tagBytes, layout.numTagBytes);
#endif

  return tag;
}

static unsigned loadMultiPayloadValue(const OpaqueValue *value,
                                      MultiPayloadLayout layout) {
  auto bytes = reinterpret_cast<const char *>(value);
  unsigned payloadValue = 0;
#if defined(__BIG_ENDIAN__)
  unsigned numPayloadValueBytes =
      std::min(layout.payloadSize, sizeof(payloadValue));
  memcpy(reinterpret_cast<char *>(&payloadValue) + 4 - numPayloadValueBytes,
         bytes + sizeof(OpaqueValue *) - numPayloadValueBytes, numPayloadValueBytes);
#else
  memcpy(&payloadValue, bytes,
         std::min(layout.payloadSize, sizeof(payloadValue)));
#endif
  return payloadValue;
}

void
swift::swift_storeEnumTagMultiPayload(OpaqueValue *value,
                                      const EnumMetadata *enumType,
                                      unsigned whichCase) {
  auto layout = getMultiPayloadLayout(enumType);
  unsigned numPayloads = enumType->Description->Enum.getNumPayloadCases();
  if (whichCase < numPayloads) {
    // For a payload case, store the tag after the payload area.
    storeMultiPayloadTag(value, layout, whichCase);
  } else {
    // For an empty case, factor out the parts that go in the payload and
    // tag areas.
    unsigned whichEmptyCase = whichCase - numPayloads;
    unsigned whichTag, whichPayloadValue;
    if (layout.payloadSize >= 4) {
      whichTag = numPayloads;
      whichPayloadValue = whichEmptyCase;
    } else {
      unsigned numPayloadBits = layout.payloadSize * CHAR_BIT;
      whichTag = numPayloads + (whichEmptyCase >> numPayloadBits);
      whichPayloadValue = whichEmptyCase & ((1U << numPayloads) - 1U);
    }
    storeMultiPayloadTag(value, layout, whichTag);
    storeMultiPayloadValue(value, layout, whichPayloadValue);
  }
}

unsigned
swift::swift_getEnumCaseMultiPayload(const OpaqueValue *value,
                                     const EnumMetadata *enumType) {
  auto layout = getMultiPayloadLayout(enumType);
  unsigned numPayloads = enumType->Description->Enum.getNumPayloadCases();

  unsigned tag = loadMultiPayloadTag(value, layout);
  if (tag < numPayloads) {
    // If the tag indicates a payload, then we're done.
    return tag;
  } else {
    // Otherwise, the other part of the discriminator is in the payload.
    unsigned payloadValue = loadMultiPayloadValue(value, layout);
    
    if (layout.payloadSize >= 4) {
      return numPayloads + payloadValue;
    } else {
      unsigned numPayloadBits = layout.payloadSize * CHAR_BIT;
      return (payloadValue | (tag - numPayloads) << numPayloadBits)
             + numPayloads;
    }
  }
}
