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
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Enum.h"
#include "swift/Runtime/Debug.h"
#include "Private.h"
#include "EnumImpl.h"
#include <cstring>
#include <algorithm>

using namespace swift;

static EnumValueWitnessTable *getMutableVWTableForInit(EnumMetadata *self,
                                                       EnumLayoutFlags flags) {
  auto oldTable =
    static_cast<const EnumValueWitnessTable *>(self->getValueWitnesses());

  // If we can alter the existing table in-place, do so.
  if (isValueWitnessTableMutable(flags))
    return const_cast<EnumValueWitnessTable*>(oldTable);

  // Otherwise, allocate permanent memory for it and copy the existing table.
  void *memory = allocateMetadata(sizeof(EnumValueWitnessTable),
                                  alignof(EnumValueWitnessTable));
  auto newTable = new (memory) EnumValueWitnessTable(*oldTable);
  self->setValueWitnesses(newTable);

  return newTable;
}

void
swift::swift_initEnumMetadataSingleCase(EnumMetadata *self,
                                        EnumLayoutFlags layoutFlags,
                                        const TypeLayout *payloadLayout) {
  auto vwtable = getMutableVWTableForInit(self, layoutFlags);

  TypeLayout layout;
  layout.size = payloadLayout->size;
  layout.stride = payloadLayout->stride;
  layout.flags = payloadLayout->flags.withEnumWitnesses(true);

  if (payloadLayout->flags.hasExtraInhabitants()) {
    auto ew = static_cast<ExtraInhabitantsValueWitnessTable*>(vwtable);
    ew->extraInhabitantFlags = payloadLayout->getExtraInhabitantFlags();
  }

  vwtable->publishLayout(layout);
}

void
swift::swift_initEnumMetadataSinglePayload(EnumMetadata *self,
                                           EnumLayoutFlags layoutFlags,
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

  auto vwtable = getMutableVWTableForInit(self, layoutFlags);
  
  size_t align = payloadLayout->flags.getAlignment();
  bool isBT = payloadLayout->flags.isBitwiseTakable();
  TypeLayout layout;
  layout.size = size;
  layout.flags =
      payloadLayout->flags.withExtraInhabitants(unusedExtraInhabitants > 0)
          .withEnumWitnesses(true)
          .withInlineStorage(
              ValueWitnessTable::isValueInline(isBT, size, align));
  auto rawStride = llvm::alignTo(size, align);
  layout.stride = rawStride == 0 ? 1 : rawStride;
  
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
#define WANT_ONLY_REQUIRED_VALUE_WITNESSES
#define VALUE_WITNESS(LOWER_ID, UPPER_ID) \
    vwtable->LOWER_ID = payloadVWT->LOWER_ID;
#define DATA_VALUE_WITNESS(LOWER_ID, UPPER_ID, TYPE)
#include "swift/ABI/ValueWitness.def"
  } else {
#endif
    installCommonValueWitnesses(layout, vwtable);
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

  vwtable->publishLayout(layout);
}

unsigned swift::swift_getEnumCaseSinglePayload(const OpaqueValue *value,
                                               const Metadata *payload,
                                               unsigned emptyCases) {

  auto *payloadWitnesses = payload->getValueWitnesses();
  auto size = payloadWitnesses->getSize();
  auto numExtraInhabitants = payloadWitnesses->getNumExtraInhabitants();
  auto EIVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(payloadWitnesses);
  auto getExtraInhabitantIndex = EIVWT ? EIVWT->getExtraInhabitantIndex : nullptr;

  return getEnumTagSinglePayloadImpl(value, emptyCases, payload, size,
                                     numExtraInhabitants,
                                     getExtraInhabitantIndex);
}

void swift::swift_storeEnumTagSinglePayload(OpaqueValue *value,
                                            const Metadata *payload,
                                            unsigned whichCase,
                                            unsigned emptyCases) {

  auto *payloadWitnesses = payload->getValueWitnesses();
  auto size = payloadWitnesses->getSize();
  auto numExtraInhabitants = payloadWitnesses->getNumExtraInhabitants();
  auto EIVWT = dyn_cast<ExtraInhabitantsValueWitnessTable>(payloadWitnesses);
  auto storeExtraInhabitant = EIVWT ? EIVWT->storeExtraInhabitant : nullptr;

  storeEnumTagSinglePayloadImpl(value, whichCase, emptyCases, payload, size,
                                numExtraInhabitants, storeExtraInhabitant);
}

void
swift::swift_initEnumMetadataMultiPayload(EnumMetadata *enumType,
                                     EnumLayoutFlags layoutFlags,
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
                                enumType->getDescription()->getNumEmptyCases(),
                                numPayloads);

  auto vwtable = getMutableVWTableForInit(enumType, layoutFlags);

  // Set up the layout info in the vwtable.
  TypeLayout layout;
  layout.size = totalSize;
  layout.flags = ValueWitnessFlags()
                     .withAlignmentMask(alignMask)
                     .withPOD(isPOD)
                     .withBitwiseTakable(isBT)
                     // TODO: Extra inhabitants
                     .withExtraInhabitants(false)
                     .withEnumWitnesses(true)
                     .withInlineStorage(ValueWitnessTable::isValueInline(
                         isBT, totalSize, alignMask + 1));
  auto rawStride = (totalSize + alignMask) & ~alignMask;
  layout.stride = rawStride == 0 ? 1 : rawStride;
  
  installCommonValueWitnesses(layout, vwtable);

  vwtable->publishLayout(layout);
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
  unsigned numPayloads = enumType->getDescription()->getNumPayloadCases();
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
  unsigned numPayloads = enumType->getDescription()->getNumPayloadCases();

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
