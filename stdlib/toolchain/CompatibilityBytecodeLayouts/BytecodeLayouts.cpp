//===--- RuntimeValueWitness.cpp - Value Witness Runtime Implementation---===//
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
// Implementations of runtime determined value witness functions
// This file is intended to be statically linked into executables until it is
// fully added to the runtime.
//
//===----------------------------------------------------------------------===//

#include "BytecodeLayouts.h"
#include "../../public/SwiftShims/swift/shims/HeapObject.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/ABI/System.h"
#include "swift/Runtime/Error.h"
#include "swift/Runtime/HeapObject.h"
#include "llvm/Support/SwapByteOrder.h"
#include <cstdint>
#if SWIFT_OBJC_INTEROP
#include "swift/Runtime/ObjCBridge.h"
#include <Block.h>
#endif

using namespace swift;

// Pointers my have spare bits stored in the high and low bits. Mask them out
// before we pass them to retain/release functions
#define MASK_PTR(x)                                                            \
  ((__swift_uintptr_t)x & ~heap_object_abi::SwiftSpareBitsMask)

/// Get the generic argument vector for the passed in metadata
///
/// NB: We manually compute the offset instead of using Metadata::getGenericArgs
/// because Metadata::getGenericArgs checks the isSwift bit which we cannot do
/// in a compatibility library. Once we merge this into the runtime, we can
/// safely use Metadata::getGenericArgs. For our purposes right now anyways,
/// struct and enums always have their generic argument vector at offset + 2 so
/// we can hard code that.
const Metadata **getGenericArgs(Metadata *metadata) {
  return ((const Metadata **)metadata) + 2;
}

/// The minimum number of bits used for a value.
/// e.g. bitsToRepresent(5(0b101)) -> 3
static uint8_t bitsToRepresent(uint32_t numValues) {
  if (numValues == 0)
    return 0;
  unsigned int r = 1;
  while (numValues >>= 1)
    r++;
  return r;
}

/// The minimum number of bytes (rounded up) used for a value.
/// e.g. bytesToRepresent(5(0b101)) -> 1
static uint8_t bytesToRepresent(uint32_t numValues) {
  return (bitsToRepresent(numValues) + 7) / 8;
}

BitVector::BitVector(std::vector<uint8_t> values) { add(values); }
BitVector::BitVector(size_t bits) { data = std::vector<bool>(bits, 0); }

size_t BitVector::count() const {
  size_t total = 0;
  for (auto bit : data)
    total += bit;
  return total;
}

void BitVector::add(uint64_t byte) {
  for (size_t i = 0; i < 64; i++)
    data.push_back(byte >> (63 - i) & 0x1);
}

void BitVector::add(uint32_t byte) {
  for (size_t i = 0; i < 32; i++)
    data.push_back(byte >> (31 - i) & 0x1);
}

void BitVector::add(uint8_t byte) {
  for (size_t i = 0; i < 8; i++)
    data.push_back(byte >> (7 - i) & 0x1);
}

void BitVector::add(std::vector<uint8_t> values) {
  for (auto value : values)
    add(value);
}

void BitVector::add(BitVector v) {
  data.insert(data.end(), v.data.begin(), v.data.end());
}

bool BitVector::none() const {
  for (bool bit : data) {
    if (bit)
      return false;
  }
  return true;
}

void BitVector::zextTo(size_t numBits) {
  if (numBits <= size())
    return;
  auto zeros = std::vector<bool>((numBits - size()), 0);
  data.insert(data.begin(), zeros.begin(), zeros.end());
}

void BitVector::truncateTo(size_t numBits) {
  auto other = std::vector<bool>(data.begin(), data.begin() + numBits);
  data = other;
}

void BitVector::zextOrTruncTo(size_t numBits) {
  if (numBits > size()) {
    truncateTo(numBits);
  } else {
    zextTo(numBits);
  }
}

bool BitVector::any() const { return !none(); }

size_t BitVector::size() const { return data.size(); }

BitVector &BitVector::operator&=(const BitVector &other) {
  for (size_t i = 0; i < size(); i++) {
    data[i] = data[i] && other.data[i];
  }
  return *this;
}

BitVector BitVector::operator+(const BitVector &other) const {
  BitVector bv = *this;
  bv.data.insert(bv.data.end(), other.data.begin(), other.data.end());
  return bv;
}

uint32_t BitVector::getAsU32() const {
  uint32_t output = 0;
  for (auto bit : data) {
    output <<= 1;
    output |= bit;
  }
  return output;
}

uint32_t BitVector::gatherBits(BitVector mask) {
  auto masked = *this;
  masked &= mask;
  assert(mask.size() == size());

  uint32_t output = 0;
  for (size_t i = 0; i < mask.size(); i++) {
    auto maskBit = mask.data[i];
    auto valueBit = data[i];
    if (maskBit) {
      output <<= 1;
      output |= valueBit;
    }
  }
  return output;
}

uint32_t indexFromValue(BitVector mask, BitVector payload,
                        BitVector extraTagBits) {
  unsigned numSpareBits = mask.count();
  uint32_t tag = 0;

  // Get the tag bits from spare bits, if any.
  if (numSpareBits > 0) {
    tag = payload.gatherBits(mask);
  }

  // Merge the extra tag bits, if any.
  if (extraTagBits.size() > 0) {
    tag = (extraTagBits.getAsU32() << numSpareBits) | tag;
  }
  return tag;
}

BitVector BitVector::getBitsSetFrom(uint32_t numBits, uint32_t lobits) {
  BitVector bv;
  uint8_t byte = 0;
  while (numBits > 0) {
    byte <<= 1;
    if (numBits > lobits)
      byte |= 1;
    numBits--;
    if (numBits % 8 == 0) {
      bv.add(byte);
      byte = 0;
    }
  }
  return bv;
}

BitVector BitVector::operator~() const {
  BitVector result = *this;
  result.data.flip();
  return result;
}

uint32_t BitVector::countExtraInhabitants() const {
  // Calculating Extra Inhabitants from Spare Bits:
  // If any of the spare bits is set, we have an extra inhabitant.
  //
  // Thus we have an extra inhabitant for each combination of spare bits
  // (2^#sparebits), minus 1, since all zeros indicates we have a valid value.
  //
  // Then for each combination, we can set the remaining non spare bits.
  //
  // ((2^#sparebits)-1) * (2^#nonsparebits)
  // ==> ((1 << #sparebits)-1) << #nonsparebits
  if (this->none())
    return 0;
  // Make sure the arithmetic below doesn't overflow.
  if (this->size() >= 32)
    // Max Num Extra Inhabitants: 0x7FFFFFFF (i.e. we need at least one
    // inhabited bit)
    return ValueWitnessFlags::MaxNumExtraInhabitants;
  uint32_t spareBitCount = this->count();
  uint32_t nonSpareCount = this->size() - this->count();
  uint32_t rawCount = ((1U << spareBitCount) - 1U) << nonSpareCount;
  return std::min(rawCount,
                  unsigned(ValueWitnessFlags::MaxNumExtraInhabitants));
}

/// Given a pointer and an offset, read the requested data and increment the
/// offset
template <typename T>
T readBytes(const uint8_t *typeLayout, size_t &i) {
  T returnVal = *(const T *)(typeLayout + i);
  for (size_t j = 0; j < sizeof(T); j++) {
    returnVal <<= 8;
    returnVal |= *(typeLayout + i + j);
  }
  i += sizeof(T);
  return returnVal;
}

template <>
uint8_t readBytes<uint8_t>(const uint8_t *typeLayout, size_t &i) {
  uint8_t returnVal = *(const uint8_t *)(typeLayout + i);
  i += 1;
  return returnVal;
}

AlignedGroup readAlignedGroup(const uint8_t *typeLayout, Metadata *metadata) {
  // a numFields (alignment,fieldLength,field)+
  // ALIGNED_GROUP:= 'a' SIZE (ALIGNMENT SIZE VALUE)+

  // Read 'a'
  size_t offset = 0;
  readBytes<uint8_t>(typeLayout, offset);
  uint32_t numFields = readBytes<uint32_t>(typeLayout, offset);
  std::vector<AlignedGroup::Field> fields;
  for (size_t i = 0; i < numFields; i++) {
    uint8_t alignment = readBytes<uint8_t>(typeLayout, offset);
    uint32_t layoutLen = readBytes<uint32_t>(typeLayout, offset);
    fields.emplace_back(alignment, layoutLen, typeLayout + offset);
    offset += layoutLen;
  }
  return AlignedGroup(fields, metadata);
}

SinglePayloadEnum readSinglePayloadEnum(const uint8_t *typeLayout,
                                        Metadata *metadata) {
  // SINGLEENUM := 'e' SIZE SIZE VALUE
  // e NumEmptyPayloads LengthOfPayload Payload
  // Read 'e'
  size_t offset = 0;
  readBytes<uint8_t>(typeLayout, offset);
  uint32_t numEmptyPayloads = readBytes<uint32_t>(typeLayout, offset);
  uint32_t payloadLayoutLength = readBytes<uint32_t>(typeLayout, offset);
  const uint8_t *payloadLayoutPtr = typeLayout + offset;
  uint32_t payloadSize = computeSize(payloadLayoutPtr, metadata);

  BitVector payloadSpareBits = spareBits(payloadLayoutPtr, metadata);
  uint32_t payloadExtraInhabitants =
      numExtraInhabitants(payloadLayoutPtr, metadata);

  uint32_t tagsInExtraInhabitants =
      std::min(payloadExtraInhabitants, numEmptyPayloads);
  uint32_t spilledTags = numEmptyPayloads - tagsInExtraInhabitants;

  uint8_t tagSize = spilledTags == 0           ? 0
                    : spilledTags < UINT8_MAX  ? 1
                    : spilledTags < UINT16_MAX ? 2
                                               : 4;
  BitVector enumSpareBits = payloadSpareBits;
  enumSpareBits.add(BitVector(tagSize * 8));
  return SinglePayloadEnum(numEmptyPayloads, payloadLayoutLength, payloadSize,
                           tagsInExtraInhabitants, enumSpareBits,
                           payloadSpareBits, payloadLayoutPtr, tagSize,
                           metadata);
}

uint32_t MultiPayloadEnum::payloadSize() const {
  size_t maxSize = 0;
  for (size_t i = 0; i < payloadLayoutPtr.size(); i++) {
    maxSize = std::max(maxSize, computeSize(payloadLayoutPtr[i], metadata));
  }
  return maxSize;
}

MultiPayloadEnum readMultiPayloadEnum(const uint8_t *typeLayout,
                                      Metadata *metadata) {
  size_t offset = 0;
  // // E numEmptyPayloads numPayloads lengthOfEachPayload payloads
  // MULTIENUM := 'E' SIZE SIZE SIZE+ VALUE+
  // Read 'E'
  readBytes<uint8_t>(typeLayout, offset);
  uint32_t numEmptyPayloads = readBytes<uint32_t>(typeLayout, offset);
  uint32_t numPayloads = readBytes<uint32_t>(typeLayout, offset);
  std::vector<uint32_t> payloadLengths;
  std::vector<const uint8_t *> payloadOffsets;
  std::vector<BitVector> casesSpareBits;
  for (size_t i = 0; i < numPayloads; i++) {
    payloadLengths.push_back(readBytes<uint32_t>(typeLayout, offset));
  }
  for (auto payloadLength : payloadLengths) {
    payloadOffsets.push_back(typeLayout + offset);
    casesSpareBits.push_back(spareBits(typeLayout + offset, metadata));
    offset += payloadLength;
  }

  BitVector payloadSpareBits;
  for (auto vec : casesSpareBits) {
    if (vec.size() > payloadSpareBits.size()) {
      payloadSpareBits.data.insert(
          payloadSpareBits.data.end(),
          vec.data.begin() + payloadSpareBits.data.size(), vec.data.end());
    }
    payloadSpareBits &= vec;
  }

  uint32_t numExtraInhabitants = payloadSpareBits.countExtraInhabitants();
  uint32_t tagsInExtraInhabitants =
      std::min(numExtraInhabitants, numPayloads + numEmptyPayloads);
  uint32_t spilledTags =
      numPayloads + numEmptyPayloads - tagsInExtraInhabitants;

  // round up to the nearest byte
  uint8_t extraTagBytes = bytesToRepresent(spilledTags);

  BitVector extraTagBitsSpareBits(8 * extraTagBytes);
  // If we rounded up, those extra tag bits are spare.
  for (int i = 0; i < extraTagBytes * 8 - bitsToRepresent(spilledTags); i++) {
    extraTagBitsSpareBits.data[i] = 1;
  }

  return MultiPayloadEnum(numEmptyPayloads, payloadLengths,
                          extraTagBitsSpareBits, payloadOffsets, metadata);
}

const BitVector MultiPayloadEnum::commonSpareBits() const {
  BitVector bits = ~BitVector(payloadSize() * 8);
  for (size_t i = 0; i < payloadLayoutPtr.size(); i++) {
    auto caseSpareBits = ::spareBits(payloadLayoutPtr[i], metadata);
    auto numTrailingZeros = payloadSize() * 8 - caseSpareBits.size();
    auto extended = caseSpareBits + ~BitVector(numTrailingZeros);
    bits &= extended;
  }
  return bits;
}

uint32_t MultiPayloadEnum::tagsInExtraInhabitants() const {
  return std::min(commonSpareBits().countExtraInhabitants(), numEmptyPayloads);
}

uint32_t MultiPayloadEnum::size() const {
  return payloadSize() + extraTagBitsSpareBits.size() / 8;
}

uint32_t MultiPayloadEnum::tagSize() const {
  return extraTagBitsSpareBits.size() / 8;
}

BitVector MultiPayloadEnum::spareBits() const {
  return commonSpareBits() + extraTagBitsSpareBits;
}

static BitVector pointerSpareBitMask() {
  BitVector bv;
  bv.add(heap_object_abi::SwiftSpareBitsMask);
  return bv;
}

size_t computeSize(const uint8_t *typeLayout, Metadata *metadata) {
  switch ((LayoutType)typeLayout[0]) {
  case LayoutType::I8:
    return 1;
  case LayoutType::I16:
    return 2;
  case LayoutType::I32:
    return 4;
  case LayoutType::I64:
    return 8;
  case LayoutType::ErrorReference:
  case LayoutType::NativeStrongReference:
  case LayoutType::NativeUnownedReference:
  case LayoutType::NativeWeakReference:
  case LayoutType::UnknownUnownedReference:
  case LayoutType::UnknownWeakReference:
  case LayoutType::BlockReference:
  case LayoutType::BridgeReference:
  case LayoutType::ObjCReference:
    return sizeof(__swift_uintptr_t);
  case LayoutType::AlignedGroup:
    return readAlignedGroup(typeLayout, metadata).size();
  case LayoutType::MultiPayloadEnum:
    return readMultiPayloadEnum(typeLayout, metadata).size();
  case LayoutType::SinglePayloadEnum:
    return readSinglePayloadEnum(typeLayout, metadata).size;
  case LayoutType::ArcheType: {
    size_t offset = 0;
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t index = readBytes<uint32_t>(typeLayout, offset);
    return getGenericArgs(metadata)[index]->getValueWitnesses()->getSize();
  }
  case LayoutType::ResilientType: {
    size_t offset = 0;
    // Read 'R'
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t mangledNameLength = readBytes<uint32_t>(typeLayout, offset);
    const Metadata *resilientMetadata = swift_getTypeByMangledNameInContext(
        (const char *)(typeLayout + offset), mangledNameLength, nullptr,
        (const void *const *)getGenericArgs(metadata));
    return resilientMetadata->getValueWitnesses()->getSize();
  }
  }
}

BitVector spareBits(const uint8_t *typeLayout, Metadata *metadata) {
  // In an aligned group, we use the field with the most extra inhabitants,
  // favouring the earliest field in a tie.
  switch ((LayoutType)typeLayout[0]) {
  case LayoutType::AlignedGroup:
    return readAlignedGroup(typeLayout, metadata).spareBits();
  case LayoutType::I8:
    return BitVector(8);
  case LayoutType::I16:
    return BitVector(16);
  case LayoutType::I32:
    return BitVector(32);
  case LayoutType::I64:
    return BitVector(64);
  case LayoutType::NativeStrongReference:
  case LayoutType::NativeUnownedReference:
  case LayoutType::NativeWeakReference:
    return pointerSpareBitMask();
  case LayoutType::UnknownUnownedReference:
  case LayoutType::UnknownWeakReference:
  case LayoutType::BlockReference:
  case LayoutType::BridgeReference:
  case LayoutType::ObjCReference:
  case LayoutType::ErrorReference:
    return BitVector(sizeof(__swift_uintptr_t) * 8);
  case LayoutType::SinglePayloadEnum:
    return readSinglePayloadEnum(typeLayout, metadata).spareBits;
  case LayoutType::MultiPayloadEnum:
    return readMultiPayloadEnum(typeLayout, metadata).spareBits();
  case LayoutType::ArcheType: {
    // No spare bits stored in archetypes
    size_t offset = 0;
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t index = readBytes<uint32_t>(typeLayout, offset);
    auto size = getGenericArgs(metadata)[index]->getValueWitnesses()->getSize();
    return BitVector(size * 8);
  }
  case LayoutType::ResilientType: {
    // No spare bits stored in resilient types
    // Read 'R'
    size_t offset;
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t mangledNameLength = readBytes<uint32_t>(typeLayout, offset);

    const Metadata *resilientMetadata = swift_getTypeByMangledNameInContext(
        (const char *)(typeLayout + offset), mangledNameLength, nullptr,
        (const void *const *)getGenericArgs(metadata));

    offset += mangledNameLength;
    auto size = resilientMetadata->getValueWitnesses()->getSize();
    return BitVector(size * 8);
    break;
  }
  }
}

size_t AlignedGroup::size() const {
  uint64_t addr = 0;
  for (auto field : fields) {
    if (field.alignment == '?') {
      // We'd have to hit the vw table for the field's alignment, so let's just
      // get the size of the whole thing.
      return metadata->vw_size();
    }
    uint64_t shiftValue = uint64_t(1) << (field.alignment - '0');
    uint64_t alignMask = shiftValue - 1;
    addr = ((addr + alignMask) & (~alignMask));
    addr += computeSize(field.fieldPtr, metadata);
  }
  return addr;
}

BitVector AlignedGroup::spareBits() const {
  // The spare bits of an aligned group is the field that has the most number of
  // spare bits's spare bits vector, padded with zeros to the size of the struct
  //
  // In the case of a tie, we take the first field encountered.
  size_t offset = 0;
  size_t offsetOfBest = 0;
  BitVector maskOfBest;

  for (size_t fieldIdx = 0; fieldIdx < fields.size(); fieldIdx++) {
    BitVector fieldSpareBits = ::spareBits(fields[fieldIdx].fieldPtr, metadata);
    // We're supposed to pick the first field (see
    // RecordTypeInfoImpl::getFixedExtraInhabitantProvidingField), but what
    // "first" seems to be gets reversed somewhere? So we pick the last field
    // instead, thus ">=".
    if (fieldSpareBits.count() >= maskOfBest.count()) {
      offsetOfBest = offset;
      maskOfBest = fieldSpareBits;
    }

    uint64_t alignMask = 0;
    uint64_t size = 0;
    if (fields[fieldIdx].alignment == '?') {
      // This must be an archetype or resilient type.
      switch ((LayoutType)fields[fieldIdx].fieldPtr[0]) {
      case LayoutType::ArcheType: {
        // Grab the index from the
        // archetype and consult the metadata
        size_t fieldOffset = 1;
        // 'A' <index: UINT32>
        uint32_t archetypeIndex =
            readBytes<uint32_t>(fields[fieldIdx].fieldPtr, fieldOffset);
        const Metadata *archetypeMetadata =
            getGenericArgs((Metadata *)metadata)[archetypeIndex];
        alignMask = archetypeMetadata->getValueWitnesses()->getAlignmentMask();
        offset = ((uint64_t)offset + alignMask) & (~alignMask);
        size = archetypeMetadata->getValueWitnesses()->getSize();
        break;
      }
      case LayoutType::ResilientType: {
        // 'R' <nameLength: uint32_t> <mangledName...>
        size_t fieldOffset = 1;
        const uint8_t *fieldLayout = fields[fieldIdx].fieldPtr;
        uint32_t mangledNameLength = readBytes<uint32_t>(fieldLayout, fieldOffset);
        const Metadata *resilientMetadata = swift_getTypeByMangledNameInContext(
            (const char *)(fieldLayout + offset), mangledNameLength, nullptr,
            (const void *const *)getGenericArgs(metadata));
        alignMask = resilientMetadata->getValueWitnesses()->getAlignmentMask();
        size += resilientMetadata->getValueWitnesses()->getSize();
        break;
      }
      default:
        assert(false &&
               "Only Archetypes and Resilient types should have '?' alignment");
      }
    } else {
      uint64_t shiftValue = uint64_t(1) << (fields[fieldIdx].alignment - '0');
      alignMask = shiftValue - 1;
      size = computeSize(fields[fieldIdx].fieldPtr, metadata);
    }
    // When we align, all the bits are spare and are considered a field we can
    // put spare bits into
    size_t oldOffset = offset;
    offset = ((uint64_t)offset + alignMask) & (~alignMask);

    BitVector alignmentSpareBits = ~BitVector(8 * (offset - oldOffset));
    if (alignmentSpareBits.count() >= maskOfBest.count()) {
      offsetOfBest = oldOffset;
      maskOfBest = alignmentSpareBits;
    }
    offset += size;
  }

  return BitVector(offsetOfBest * 8) + maskOfBest +
         BitVector((size() - offsetOfBest) * 8 - maskOfBest.size());
}

uint32_t MultiPayloadEnum::gatherSpareBits(const uint8_t *data,
                                           unsigned resultBitWidth) const {
  // Use the enum's spare bit vector to mask out a value, and accumlate the
  // resulting masked value into an int of the given size.
  //
  // For example:
  // Enum Mask: 0xFF0FFF0F
  // DATA:      0x12345678
  // Produces 124568
  //
  // Enum Mask: 0xFF 0F FF 07
  // DATA:      0x12 34 56 78
  // Produces 0001 0010 0010 0100 0101 0110 111
  // ->       000 1001 0001 0010 0010 1011 0111
  // ->       1001 0001 0010 0010 1011 0111
  // ->  0xB122D9
  //
  // We are given a result bitwidth, as if we gather the required bits without
  // using all of the mask bits, we will stop.
  //
  // For example, to store the values 0-255 in a mask of 0xFFFF0000, the layout
  // algorithm only uses the minimum number of bits to represent the values.
  // 0x01000000
  // 0x02000000
  // ..
  // 0xFF000000
  // Even though we have additional bytes in the mask.
  if (!resultBitWidth) {
    return 0;
  }

  uint32_t result = 0;
  uint32_t width = 0;

  const uint8_t *addr = data;
  size_t currByteIdx = 0;
  size_t currWordIdx = 0;
  uint32_t currBitIdx = 0;
  size_t wordSize = sizeof(__swift_uintptr_t);

  for (auto bit : spareBits().data) {
    __swift_uintptr_t currWord =
        *(((const __swift_uintptr_t *)addr) + currWordIdx);
    uint8_t currByte = currWord >> (8 * (wordSize - 1 - currByteIdx));
    if (bit) {
      result <<= 1;
      if ((bit << (7 - currBitIdx)) & currByte) {
        result |= 1;
      }
      width += 1;
      if (width == resultBitWidth) {
        return result;
      }
    }
    currBitIdx += 1;
    if (currBitIdx == 8) {
      currBitIdx = 0;
      currByteIdx += 1;
    }

    if (currByteIdx == wordSize) {
      currByteIdx = 0;
      currWordIdx += 1;
    }
  }

  return result;
}

uint32_t getEnumTag(void *addr, const uint8_t *layoutString,
                    Metadata *metadata) {
  switch ((LayoutType)layoutString[0]) {
  case LayoutType::SinglePayloadEnum:
    return getEnumTagSinglePayload(
        addr, layoutString,
        readSinglePayloadEnum(layoutString, metadata).numEmptyPayloads,
        metadata);
  case LayoutType::MultiPayloadEnum:
    return getEnumTagMultiPayload(addr, layoutString, metadata);
  default:
    assert(false && "getEnumTag called on non enum");
    return UINT32_MAX;
  }
}

uint32_t getEnumTagMultiPayload(void *addr, const uint8_t *layoutString,
                                Metadata *metadata) {
  return 0;
}

uint32_t getEnumTagSinglePayload(void *addr, const uint8_t *layoutString,
                                 uint32_t numEmptyPayloads,
                                 Metadata *metadata) {
  // In an aligned group, we use the field with the most extra inhabitants,
  // favouring the earliest field in a tie.
  switch ((LayoutType)layoutString[0]) {
  case LayoutType::I8:
  case LayoutType::I16:
  case LayoutType::I32:
  case LayoutType::I64:
    assert(false && "cannot get enum tag from Int types");
    return UINT32_MAX;
  case LayoutType::AlignedGroup: {
    uint32_t offset = 0;
    // Pick the field with the most number of extra inhabitants and return that
    uint32_t tag = UINT32_MAX;
    uint32_t maxXICount = 0;
    auto group = readAlignedGroup(layoutString, metadata);
    for (auto field : group.fields) {
      uint32_t fieldXI = numExtraInhabitants(field.fieldPtr, metadata);
      if (fieldXI && fieldXI >= maxXICount) {
        tag = getEnumTagSinglePayload((void **)addr + offset, field.fieldPtr,
                                      numEmptyPayloads, metadata);
        maxXICount = fieldXI;
      }
      offset += computeSize(field.fieldPtr, metadata);
    }
    return tag;
  }
  case LayoutType::NativeStrongReference:
  case LayoutType::NativeUnownedReference:
  case LayoutType::NativeWeakReference: {
    // Native heap references may have extra inhabitants of the high and low
    // bits pointers.
    uint32_t index =
        swift_getHeapObjectExtraInhabitantIndex((HeapObject **)addr);
    if (index == UINT32_MAX)
      // We have a valid value, thus, we have index 0
      return 0;
    return index;
  }
  case LayoutType::UnknownUnownedReference:
  case LayoutType::UnknownWeakReference:
  case LayoutType::BlockReference:
  case LayoutType::BridgeReference:
  case LayoutType::ObjCReference:
  case LayoutType::ErrorReference:
    // Non native references only have one safe extra inhabitant: 0
    return *(uint32_t **)addr == nullptr ? 1 : 0;
  case LayoutType::SinglePayloadEnum: {
    auto e = readSinglePayloadEnum(layoutString, metadata);
    auto payloadIndex = getEnumTagSinglePayload(addr, e.payloadLayoutPtr,
                                                e.numEmptyPayloads, metadata);
    if (payloadIndex >= e.numEmptyPayloads)
      return payloadIndex - e.numEmptyPayloads;
    return payloadIndex;
  }
  case LayoutType::ArcheType: {
    // Read 'A'
    // Kind is a pointer sized int at offset 0 of metadata pointer
    size_t offset = 0;
    readBytes<uint8_t>(layoutString, offset);
    uint32_t index = readBytes<uint32_t>(layoutString, offset);
    return getGenericArgs(metadata)[index]
        ->getValueWitnesses()
        ->getEnumTagSinglePayload((const OpaqueValue *)addr, numEmptyPayloads,
                                  getGenericArgs(metadata)[index]);
  }
  case LayoutType::MultiPayloadEnum:
  case LayoutType::ResilientType:
    // We don't store extra inhabitants in these types
    return UINT32_MAX;
  }
}

uint32_t numExtraInhabitants(const uint8_t *layoutString, Metadata *metadata) {
  // In an aligned group, we use the field with the most extra inhabitants,
  // favouring the earliest field in a tie.
  switch ((LayoutType)layoutString[0]) {
  case LayoutType::I8:
  case LayoutType::I16:
  case LayoutType::I32:
  case LayoutType::I64:
    return 0;
  case LayoutType::AlignedGroup: {
    // Pick the field with the most number of extra inhabitants and return that
    uint32_t maxXICount = 0;
    auto group = readAlignedGroup(layoutString, metadata);
    for (auto field : group.fields) {
      uint32_t fieldXI = numExtraInhabitants(field.fieldPtr, metadata);
      if (fieldXI >= maxXICount) {
        maxXICount = fieldXI;
      }
    }
    return maxXICount;
  }
  case LayoutType::NativeStrongReference:
  case LayoutType::NativeUnownedReference:
  case LayoutType::NativeWeakReference: {
    // Native heap references may have extra inhabitants of the high and low
    // bits pointers.
    uint64_t rawCount = heap_object_abi::LeastValidPointerValue >>
                        heap_object_abi::ObjCReservedLowBits;
    // The runtime limits the count.
    return std::min(uint64_t(ValueWitnessFlags::MaxNumExtraInhabitants),
                    rawCount);
  }
  case LayoutType::UnknownUnownedReference:
  case LayoutType::UnknownWeakReference:
  case LayoutType::BlockReference:
  case LayoutType::ObjCReference:
  case LayoutType::BridgeReference:
  case LayoutType::ErrorReference:
    // Non native references only have one safe extra inhabitant: 0
    return 1;
  case LayoutType::SinglePayloadEnum: {
    // The number of extra inhabitants in a single payload enum is the number of
    // extra inhabitants of the payload less the number of extra inhabitants we
    // use up for the enum's tag.
    auto e = readSinglePayloadEnum(layoutString, metadata);
    uint32_t payloadXI = numExtraInhabitants(e.payloadLayoutPtr, metadata);
    return payloadXI - e.tagsInExtraInhabitants;
  }
  case LayoutType::MultiPayloadEnum: {
    auto e = readMultiPayloadEnum(layoutString, metadata);
    return e.spareBits().countExtraInhabitants();
  }
  case LayoutType::ArcheType: {
    // Read 'A'
    // Kind is a pointer sized int at offset 0 of metadata pointer
    size_t offset = 0;
    readBytes<uint8_t>(layoutString, offset);
    uint32_t index = readBytes<uint32_t>(layoutString, offset);
    return getGenericArgs(metadata)[index]
        ->getValueWitnesses()
        ->extraInhabitantCount;
  }
  case LayoutType::ResilientType:
    return UINT32_MAX;
  }
}

uint32_t extractPayloadTag(const MultiPayloadEnum e, const uint8_t *data) {
  unsigned numPayloads = e.payloadLayoutPtr.size();
  unsigned casesPerTag = (~e.spareBits()).count() >= 32
                             ? UINT_MAX
                             : 1U << (~e.spareBits().count());
  if (e.numEmptyPayloads != 0) {
    numPayloads += (e.numEmptyPayloads / casesPerTag) + 1;
  }

  unsigned requiredSpareBits = bitsToRepresent(numPayloads);
  unsigned numSpareBits = e.spareBits().count();
  uint32_t tag = 0;
  // Get the tag bits from spare bits, if any.
  if (numSpareBits > 0) {
    tag = e.gatherSpareBits(data, requiredSpareBits);
  }

  // Get the extra tag bits, if any.
  if (e.extraTagBitsSpareBits.size() > 0) {
    uint32_t extraTagValue = 0;
    if (e.extraTagBitsSpareBits.size() == 8) {
      extraTagValue = *(const uint8_t *)data + e.payloadSize();
    } else if (e.extraTagBitsSpareBits.size() == 16) {
      extraTagValue = *(const uint16_t *)data + e.payloadSize();
    } else if (e.extraTagBitsSpareBits.size() == 32) {
      extraTagValue = *(const uint32_t *)data + e.payloadSize();
    }
    extraTagValue <<= numSpareBits;
    tag |= extraTagValue;
  }
  return tag;
}

__attribute__((weak)) extern "C" void
swift_generic_destroy(void *address, void *metadata,
                      const uint8_t *typeLayout) {
  uint8_t *addr = (uint8_t *)address;
  Metadata *typedMetadata = (Metadata *)metadata;

  switch ((LayoutType)typeLayout[0]) {
  case LayoutType::AlignedGroup: {
    AlignedGroup group = readAlignedGroup(typeLayout, typedMetadata);
    auto fields = group.fields;
    for (unsigned fieldIdx = 0; fieldIdx < fields.size(); fieldIdx++) {
      if (fields[fieldIdx].alignment == '?') {
        switch ((LayoutType)fields[fieldIdx].fieldPtr[0]) {
        case LayoutType::ArcheType: {
          // Grab the index from the
          // archetype and consult the metadata
          size_t offset = 1;
          // 'A' <index: UINT32>
          uint32_t archetypeIndex =
              readBytes<uint32_t>(fields[fieldIdx].fieldPtr, offset);
          const Metadata *archetypeMetadata =
              getGenericArgs((Metadata *)metadata)[archetypeIndex];
          uint64_t alignMask =
              archetypeMetadata->getValueWitnesses()->getAlignmentMask();
          addr = (uint8_t *)((((uint64_t)addr) + alignMask) & (~alignMask));
          archetypeMetadata->getValueWitnesses()->destroy((OpaqueValue *)addr,
                                                          archetypeMetadata);
          addr += archetypeMetadata->getValueWitnesses()->getSize();
          break;
        }
        case LayoutType::ResilientType: {
          // 'R' <nameLength: uint32_t> <mangledName...>
          size_t offset = 1;
          const uint8_t *fieldLayout = fields[fieldIdx].fieldPtr;
          uint32_t mangledNameLength = readBytes<uint32_t>(fieldLayout, offset);
          const Metadata *resilientMetadata =
              swift_getTypeByMangledNameInContext(
                  (const char *)(fieldLayout + offset), mangledNameLength,
                  nullptr,
                  (const void *const *)getGenericArgs((Metadata *)metadata));
          uint64_t alignMask =
              resilientMetadata->getValueWitnesses()->getAlignmentMask();
          addr = (uint8_t *)((((uint64_t)addr) + alignMask) & (~alignMask));
          resilientMetadata->getValueWitnesses()->destroy((OpaqueValue *)addr,
                                                          resilientMetadata);
          addr += resilientMetadata->getValueWitnesses()->getSize();
          break;
        }
        default:
          assert(
              false &&
              "Only Archetypes and Resilient types should have '?' alignment");
        }
      } else {
        uint64_t shiftValue = uint64_t(1) << (fields[fieldIdx].alignment - '0');
        uint64_t alignMask = shiftValue - 1;
        addr = (uint8_t *)(((uint64_t)addr + alignMask) & (~alignMask));
        swift_generic_destroy(addr, metadata, fields[fieldIdx].fieldPtr);
        addr += computeSize(fields[fieldIdx].fieldPtr, typedMetadata);
      }
    }
    return;
  }
  case LayoutType::I8:
  case LayoutType::I16:
  case LayoutType::I32:
  case LayoutType::I64:
    return;
  case LayoutType::ErrorReference:
    swift_errorRelease(*(SwiftError **)addr);
    return;
  case LayoutType::NativeStrongReference:
    swift_release((HeapObject *)MASK_PTR(*(void **)addr));
    return;
  case LayoutType::NativeUnownedReference:
    swift_release((HeapObject *)MASK_PTR(*(void **)addr));
    return;
  case LayoutType::NativeWeakReference:
    swift_weakDestroy((WeakReference *)MASK_PTR(*(void **)addr));
    return;
  case LayoutType::UnknownUnownedReference:
    swift_unknownObjectUnownedDestroy(
        (UnownedReference *)MASK_PTR(*(void **)addr));
    return;
  case LayoutType::UnknownWeakReference:
    swift_unknownObjectWeakDestroy((WeakReference *)MASK_PTR(*(void **)addr));
    return;
  case LayoutType::BridgeReference:
    swift_bridgeObjectRelease(*(HeapObject **)addr);
    return;
  case LayoutType::ObjCReference:
#if SWIFT_OBJC_INTEROP
    objc_release((*(id *)addr));
    return;
#else
    assert(false && "Got ObjCReference, but ObjCInterop is Disabled");
    return;
#endif
  case LayoutType::BlockReference:
#if SWIFT_OBJC_INTEROP
    Block_release(*(void **)addr);
    return;
#else
    assert(false && "Got ObjCBlock, but ObjCInterop is Disabled");
    return;
#endif
  case LayoutType::SinglePayloadEnum: {
    // A Single Payload Enum has a payload iff its index is 0
    uint32_t enumTag = getEnumTag(addr, typeLayout, typedMetadata);
    if (enumTag == 0) {
      auto e = readSinglePayloadEnum(typeLayout, typedMetadata);
      swift::swift_generic_destroy((void *)addr, metadata, e.payloadLayoutPtr);
    }
    return;
  }
  case LayoutType::MultiPayloadEnum: {
    MultiPayloadEnum e = readMultiPayloadEnum(typeLayout, typedMetadata);
    uint32_t index = extractPayloadTag(e, addr);

    // Enum indices count payload cases first, then non payload cases. Thus a
    // payload index will always be in 0...numPayloadCases-1
    if (index < e.payloadLayoutPtr.size()) {
      swift::swift_generic_destroy((void *)addr, metadata,
                                   e.payloadLayoutPtr[index]);
    }
    return;
  }
  case LayoutType::ArcheType: {
    // Read 'A' <index: uint32_t>
    // Kind is a pointer sized int at offset 0 of metadata pointer
    size_t offset = 1;
    uint32_t index = readBytes<uint32_t>(typeLayout, offset);
    getGenericArgs(typedMetadata)[index]->getValueWitnesses()->destroy(
        (OpaqueValue *)addr, getGenericArgs(typedMetadata)[index]);
    return;
  }
  case LayoutType::ResilientType: {
    // Read 'R'
    size_t offset = 0;
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t mangledNameLength = readBytes<uint32_t>(typeLayout, offset);
    const Metadata *resilientMetadata = swift_getTypeByMangledNameInContext(
        (const char *)(typeLayout + offset), mangledNameLength, nullptr,
        (const void *const *)getGenericArgs(typedMetadata));

    resilientMetadata->getValueWitnesses()->destroy((OpaqueValue *)addr,
                                                    resilientMetadata);
    return;
  }
  }
}

__attribute__((weak)) extern "C" void
swift_generic_initialize(void *dest, void *src, void *metadata,
                         const uint8_t *typeLayout, bool isTake) {
  uint8_t *destAddr = (uint8_t *)dest;
  uint8_t *srcAddr = (uint8_t *)src;
  size_t offset = 0;
  Metadata *typedMetadata = (Metadata *)metadata;

  switch ((LayoutType)typeLayout[offset]) {
  case LayoutType::AlignedGroup: {
    AlignedGroup group = readAlignedGroup(typeLayout, typedMetadata);
    auto fields = group.fields;
    for (unsigned fieldIdx = 0; fieldIdx < fields.size(); fieldIdx++) {
      if (fields[fieldIdx].alignment == '?') {
        switch ((LayoutType)fields[fieldIdx].fieldPtr[0]) {
        case LayoutType::ArcheType: {
          // Grab the index from the
          // archetype and consult the metadata
          // 'A' <index: UINT32>
          size_t offset = 1;
          uint32_t archetypeIndex =
              readBytes<uint32_t>(fields[fieldIdx].fieldPtr, offset);
          const Metadata *archetypeMetadata =
              getGenericArgs((Metadata *)metadata)[archetypeIndex];
          uint64_t alignMask =
              archetypeMetadata->getValueWitnesses()->getAlignmentMask();
          destAddr =
              (uint8_t *)((((uint64_t)destAddr) + alignMask) & (~alignMask));
          srcAddr =
              (uint8_t *)((((uint64_t)srcAddr) + alignMask) & (~alignMask));
          if (isTake) {
            archetypeMetadata->getValueWitnesses()->initializeWithTake(
                (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr,
                archetypeMetadata);
          } else {
            archetypeMetadata->getValueWitnesses()->initializeWithCopy(
                (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr,
                archetypeMetadata);
          }
          uint32_t size = archetypeMetadata->getValueWitnesses()->getSize();
          srcAddr += size;
          destAddr += size;
          break;
        }
        case LayoutType::ResilientType: {
          // 'R' <nameLength: uint32_t> <mangledName...>
          size_t offset = 1;
          const uint8_t *fieldLayout = fields[fieldIdx].fieldPtr;
          uint32_t mangledNameLength = readBytes<uint32_t>(fieldLayout, offset);
          const Metadata *resilientMetadata =
              swift_getTypeByMangledNameInContext(
                  (const char *)(fieldLayout + offset), mangledNameLength,
                  nullptr,
                  (const void *const *)getGenericArgs((Metadata *)metadata));
          uint64_t alignMask =
              resilientMetadata->getValueWitnesses()->getAlignmentMask();
          destAddr =
              (uint8_t *)((((uint64_t)destAddr) + alignMask) & (~alignMask));
          srcAddr =
              (uint8_t *)((((uint64_t)srcAddr) + alignMask) & (~alignMask));
          if (isTake) {
            resilientMetadata->getValueWitnesses()->initializeWithTake(
                (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr,
                resilientMetadata);
          } else {
            resilientMetadata->getValueWitnesses()->initializeWithCopy(
                (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr,
                resilientMetadata);
          }
          uint32_t size = resilientMetadata->getValueWitnesses()->getSize();
          srcAddr += size;
          destAddr += size;
          break;
        }
        default:
          assert(
              false &&
              "Only Archetypes and Resilient types should have '?' alignment");
        }
      } else {
        uint64_t shiftValue = uint64_t(1) << (fields[fieldIdx].alignment - '0');
        uint64_t alignMask = shiftValue - 1;
        srcAddr = (uint8_t *)(((uint64_t)srcAddr + alignMask) & (~alignMask));
        destAddr = (uint8_t *)(((uint64_t)destAddr + alignMask) & (~alignMask));
        swift_generic_initialize(destAddr, srcAddr, metadata,
                                 fields[fieldIdx].fieldPtr, isTake);
        unsigned size = computeSize(fields[fieldIdx].fieldPtr, typedMetadata);
        srcAddr += size;
        destAddr += size;
      }
    }
    return;
  }
  case LayoutType::I8:
    memcpy(dest, src, 1);
    return;
  case LayoutType::I16:
    memcpy(dest, src, 2);
    return;
  case LayoutType::I32:
    memcpy(dest, src, 4);
    return;
  case LayoutType::I64:
    memcpy(dest, src, 8);
    return;
  case LayoutType::ErrorReference:
    *(void **)dest =
        isTake ? *(void **)src : swift_errorRetain((SwiftError *)*(void **)src);
    return;
  case LayoutType::NativeStrongReference:
  case LayoutType::NativeUnownedReference:
    memcpy(dest, src, sizeof(void *));
    if (!isTake)
      swift_retain((HeapObject *)MASK_PTR(*(void **)destAddr));
    return;
  case LayoutType::NativeWeakReference:
    swift_weakInit((WeakReference *)MASK_PTR(*(void **)srcAddr),
                   *(HeapObject **)dest);
    memcpy(dest, src, sizeof(void *));
    return;
  case LayoutType::UnknownUnownedReference:
    swift_unknownObjectUnownedInit(
        (UnownedReference *)MASK_PTR(*(void **)srcAddr),
        *(HeapObject **)destAddr);
    memcpy(dest, src, sizeof(void *));
    return;
  case LayoutType::UnknownWeakReference:
    swift_unknownObjectWeakInit((WeakReference *)MASK_PTR(*(void **)srcAddr),
                                *(HeapObject **)destAddr);
    memcpy(dest, src, sizeof(void *));
    return;
  case LayoutType::BlockReference:
#if SWIFT_OBJC_INTEROP
    if (!isTake) {
      *(void **)dest = Block_copy(*(void **)src);
    } else {
      memcpy(dest, src, sizeof(void *));
    }
    return;
#else
    assert(false && "Got ObjCBlock, but ObjCInterop is Disabled");
    return;
#endif
  case LayoutType::BridgeReference: {
    *(void **)dest =
        isTake ? *(void **)src
               : swift_bridgeObjectRetain((HeapObject *)*(void **)src);
    return;
  }
  case LayoutType::ObjCReference:
#if SWIFT_OBJC_INTEROP
    if (!isTake)
      objc_retain((id) * (void **)src);
    memcpy(dest, src, sizeof(void *));
    return;
#else
    assert(false && "Got ObjCReference, but ObjCInterop is Disabled");
    return;
#endif
  case LayoutType::SinglePayloadEnum: {
    // We have a payload iff the enum tag is 0
    auto e = readSinglePayloadEnum(typeLayout, typedMetadata);
    if (getEnumTag(src, typeLayout, typedMetadata) == 0) {
      swift::swift_generic_initialize(destAddr, srcAddr, metadata,
                                      e.payloadLayoutPtr, isTake);
    }
    memcpy(destAddr, srcAddr, e.size);
    return;
  }
  case LayoutType::MultiPayloadEnum: {
    MultiPayloadEnum e = readMultiPayloadEnum(typeLayout, typedMetadata);
    // numExtraInhabitants

    BitVector payloadBits;
    std::vector<uint8_t> wordVec;
    size_t wordSize = sizeof(__swift_uintptr_t);
    for (size_t i = 0; i < e.payloadSize(); i++) {
      wordVec.push_back(srcAddr[i]);
      if (wordVec.size() == wordSize) {
        // The data we read will be in little endian, so we need to reverse it
        // byte wise.
        for (auto j = wordVec.rbegin(); j != wordVec.rend(); j++) {
          payloadBits.add(*j);
        }
        wordVec.clear();
      }
    }

    // If we don't have a multiple of 8 bytes, we need to top off
    if (!wordVec.empty()) {
      while (wordVec.size() < wordSize) {
        wordVec.push_back(0);
      }
      for (auto i = wordVec.rbegin(); i != wordVec.rend(); i++) {
        payloadBits.add(*i);
      }
    }

    BitVector tagBits;
    for (size_t i = 0; i < (e.extraTagBitsSpareBits.count() / 8); i++) {
      tagBits.add(srcAddr[i]);
    }

    uint32_t index = extractPayloadTag(e, srcAddr);

    // Enum indices count payload cases first, then non payload cases. Thus a
    // payload index will always be in 0...numPayloadCases-1
    if (index < e.payloadLayoutPtr.size()) {
      swift::swift_generic_initialize(destAddr, srcAddr, metadata,
                                      e.payloadLayoutPtr[index], isTake);
    }
    // The initialize is only going to copy the contained payload. If the enum
    // is actually larger than that because e.g. it has another case that is
    // larger, those bytes, which may contain the enum index, will not be
    // copied. We _could_ use the found layout string to compute the size of
    // the runtime payload, then copy from the end if it to the size of the
    // enum, however, it's probably faster to just re memcpy the whole thing.
    memcpy(destAddr, srcAddr, e.size());
    return;
  }
  case LayoutType::ArcheType: {
    // Read 'A'
    // Kind is a pointer sized int at offset 0 of metadata pointer
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t index = readBytes<uint32_t>(typeLayout, offset);
    const Metadata *fieldMetadata = getGenericArgs((Metadata *)metadata)[index];

    if (isTake) {
      fieldMetadata->getValueWitnesses()->initializeWithTake(
          (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr, fieldMetadata);
    } else {
      fieldMetadata->getValueWitnesses()->initializeWithCopy(
          (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr, fieldMetadata);
    }
    uint32_t size = fieldMetadata->getValueWitnesses()->getSize();
    srcAddr += size;
    destAddr += size;
    break;
  }
  case LayoutType::ResilientType: {
    // Read 'R'
    readBytes<uint8_t>(typeLayout, offset);
    uint32_t mangledNameLength = readBytes<uint32_t>(typeLayout, offset);
    const Metadata *resilientMetadata = swift_getTypeByMangledNameInContext(
        (const char *)(typeLayout + offset), mangledNameLength, nullptr,
        (const void *const *)getGenericArgs(typedMetadata));

    if (isTake) {
      resilientMetadata->getValueWitnesses()->initializeWithTake(
          (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr, resilientMetadata);
    } else {
      resilientMetadata->getValueWitnesses()->initializeWithCopy(
          (OpaqueValue *)destAddr, (OpaqueValue *)srcAddr, resilientMetadata);
    }
    offset += mangledNameLength;
    uint32_t size = resilientMetadata->getValueWitnesses()->getSize();
    srcAddr += size;
    destAddr += size;
    break;
  }
  }
}

__attribute__((weak)) extern "C" void
swift_generic_assign(void *dest, void *src, void *metadata,
                     const uint8_t *typeLayout, bool isTake) {
  swift_generic_destroy(dest, metadata, typeLayout);
  swift_generic_initialize(dest, src, metadata, typeLayout, isTake);
}

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden"))) extern "C" char
    _swift_FORCE_LOAD_$_swiftCompatibilityBytecodeLayouts = 0;
