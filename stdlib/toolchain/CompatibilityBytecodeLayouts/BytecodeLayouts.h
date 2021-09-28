//===--- RuntimeValueWitness.h                                         ---===//
// Swift Language Bytecode Layouts Runtime Implementation
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
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BYTECODE_LAYOUTS_H
#define SWIFT_BYTECODE_LAYOUTS_H

#include "swift/Runtime/Metadata.h"
#include <cstdint>
#include <vector>

// Layouts
//
// enum class LayoutType: char {
//   // Scalars
//   I8 = 'c',
//   I16 = 's',
//   I32 = 'l',
//   I64 = 'L',
//   ErrorReference = 'r',
//   NativeStrongReference = 'N',
//   NativeUnownedReference = 'n',
//   NativeWeakReference = 'W',
//   UnknownUnownedReference = 'u',
//   UnknownWeakReference = 'w',
//   BlockReference = 'b',
//   BridgeReference = 'B',
//   ObjCReference = 'o',
//   ExistentialReference = 'x',
//
//   // Enums
//   SinglePayloadEnum = 'e',
//   MultiPayloadEnum = 'E',
// };
//
// VALUE := STRUCT | ENUM | SCALAR
// SCALAR := 'c'|'s'|'l'|'L'|'C'|'r'|'N'|'n'|'W'|'u'|'w'|'b'|'B'|'o'|'f'|'x'
// ALIGNED_GROUP:= 'a' UINT32 (ALIGNMENT,UINT32,VALUE)+
// ALIGNMENT := '0'|'1'|'2'|'3'|'?'
// ENUM := SINGLEENUM | MULTIENUM
//
// SIZE := uint32 (does network order this need to be specified here?)
//
// // e numEmptyPayloads lengthOfPayload payload
// SINGLEENUM := 'e' SIZE SIZE VALUE
//
// // E numEmptyPayloads numPayloads lengthOfEachPayload payloads
// MULTIENUM := 'E' SIZE SIZE SIZE+ VALUE+
//
// OFFSETS := int32+
//
// Examples:
// struct SomeStruct {
//  let a : int8
//  let b : int16
//  let c : int16
//  let d : SomeClass
// }
//
// '1c2s2s3N'
// byte aligned int8
// 2 byte aligned int16
// 2 byte aligned int16
// 4 byte aligned Native Pointer
//
// enum SampleEnum {
//    case Payload(s: SomeStruct)
//    case None
//    case ReallyNone
// }
// 'e(0x00000002)(0x00000008)1c2s2s3N'
// An enum with:
// - 2 empty cases
// - a payload stringlength of 8
// - a struct payload as in the previous example

namespace swift {
enum class LayoutType : char {
  // Scalars
  I8 = 'c',
  I16 = 's',
  I32 = 'l',
  I64 = 'L',
  ErrorReference = 'r',
  NativeStrongReference = 'N',
  NativeUnownedReference = 'n',
  NativeWeakReference = 'W',
  UnknownUnownedReference = 'u',
  UnknownWeakReference = 'w',
  BlockReference = 'b',
  BridgeReference = 'B',
  ObjCReference = 'o',
  // Enums
  // Single
  // e{emptycases}{payload}
  //
  // Multi
  // e{emptycases}{num_cases}{payload1}{payload2}{...}{payloadn}
  SinglePayloadEnum = 'e',
  MultiPayloadEnum = 'E',
  AlignedGroup = 'a',
  ArcheType = 'A',
  ResilientType = 'R',
};

// The implemenation of this should be provided by the stdlib when we link this
// into an executable/library.
SWIFT_RUNTIME_EXPORT
SWIFT_CC(swift)
const Metadata *swift_getTypeByMangledNameInContext(
    const char *typeNameStart, size_t typeNameLength,
    const TargetContextDescriptor<InProcess> *context,
    const void *const *genericArgs);

SWIFT_RUNTIME_EXPORT
void swift_generic_destroy(void *address, void *metadata,
                           const uint8_t *typeLayout);
SWIFT_RUNTIME_EXPORT
void swift_generic_assign(void *dest, void *src, void *metadata,
                          const uint8_t *typeLayout, bool isTake);
SWIFT_RUNTIME_EXPORT
void swift_generic_initialize(void *dest, void *src, void *metadata,
                              const uint8_t *typeLayout, bool isTake);
} // namespace swift

/// A simple version of swift/Basic/ClusteredBitVector that doesn't use
/// llvm::APInt. Were we to uses ClustedBitVector, we would have to link the
/// implementation into every Swift binary which would be unideal.
struct BitVector {
  std::vector<bool> data;
  BitVector() = default;
  BitVector(std::vector<uint8_t> values);

  /// Return a bitvector which the given number of bytes set to all 0s
  BitVector(size_t bits);

  /// Return the number of set bits in the bit vector
  size_t count() const;

  /// Append on a byte
  void add(uint8_t values);

  /// Append on a 32bit value
  void add(uint32_t values);

  /// Append on a 64bit value
  void add(uint64_t values);

  /// Append on a vector of bytes
  void add(std::vector<uint8_t> values);

  /// Append on another bitvector
  void add(BitVector v);

  /// True if all bits are 0
  bool none() const;

  /// True if any bit is a 1
  bool any() const;

  /// Number of bits in the vector
  size_t size() const;

  /// Zero extend a bit vector to a given number of bits
  void zextTo(size_t numBits);

  /// Truncate a bitvector to a given number of bits
  void truncateTo(size_t numBits);

  /// Extend or truncate a bitvector to a given number of bits
  void zextOrTruncTo(size_t numBits);

  /// Convert to a uint32_t
  uint32_t getAsU32() const;

  /// Bitwise &= with another vector of the same length
  BitVector &operator&=(const BitVector &other);
  BitVector operator+(const BitVector &other) const;

  /// Return a new bitvector that is the bitwise not of this one
  BitVector operator~() const;

  /// Number of extra inhabitants that fit into this vector
  uint32_t countExtraInhabitants() const;

  /// Pack masked bits into the low bits of an integer value.
  /// Equivalent to a parallel bit extract instruction (PEXT)
  uint32_t gatherBits(BitVector mask);

  static BitVector getBitsSetFrom(uint32_t numBits, uint32_t loBits);
};

uint32_t indexFromValue(BitVector mask, BitVector value, BitVector tagBits);

uint32_t extractBits(BitVector mask, BitVector value);
/// Get the sparebits mask and the offset that it's located at
BitVector spareBits(const uint8_t *typeLayout, swift::Metadata *metadata);
size_t computeSize(const uint8_t *typeLayout, swift::Metadata *metadata);

uint32_t getEnumTag(void *addr, const uint8_t *layoutString,
                    swift::Metadata *metadata);
uint32_t numExtraInhabitants(const uint8_t *layoutString,
                             swift::Metadata *metadata);

uint32_t getEnumTagMultiPayload(void *addr, const uint8_t *layoutString,
                                swift::Metadata *metadata);
uint32_t getEnumTagSinglePayload(void *addr, const uint8_t *layoutString,
                                 uint32_t numEmptyPayloads,
                                 swift::Metadata *metadata);

struct AlignedGroup {
  struct Field {
    Field(uint8_t alignment, uint32_t fieldLength, const uint8_t *fieldPtr)
        : alignment(alignment), fieldLength(fieldLength), fieldPtr(fieldPtr) {}
    uint8_t alignment;
    uint32_t fieldLength;
    const uint8_t *fieldPtr;
  };
  AlignedGroup(std::vector<AlignedGroup::Field> fields,
               swift::Metadata *metadata)
      : fields(fields), metadata(metadata) {}
  std::vector<Field> fields;
  swift::Metadata *metadata;

  BitVector spareBits() const;
  size_t size() const;
};

struct SinglePayloadEnum {
  SinglePayloadEnum(uint32_t numEmptyPayloads, uint32_t payloadLayoutLength,
                    uint32_t payloadSize, uint32_t tagsInExtraInhabitants,
                    BitVector spareBits, BitVector payloadSpareBits,
                    const uint8_t *payloadLayoutPtr, uint8_t tagSize,
                    swift::Metadata *metadata)
      : numEmptyPayloads(numEmptyPayloads),
        payloadLayoutLength(payloadLayoutLength),
        payloadLayoutPtr(payloadLayoutPtr), payloadSize(payloadSize),
        tagsInExtraInhabitants(tagsInExtraInhabitants), spareBits(spareBits),
        payloadSpareBits(payloadSpareBits), tagSize(tagSize),
        size(payloadSize + tagSize), metadata(metadata) {}
  uint32_t numEmptyPayloads;
  uint32_t payloadLayoutLength;
  const uint8_t *payloadLayoutPtr;
  uint32_t payloadSize;
  uint32_t tagsInExtraInhabitants;
  BitVector spareBits;
  BitVector payloadSpareBits;
  uint8_t tagSize;
  uint8_t size;
  swift::Metadata *metadata;
};

struct MultiPayloadEnum {
  MultiPayloadEnum(uint32_t numEmptyPayloads,
                   std::vector<uint32_t> payloadLayoutLength,
                   BitVector extraTagBitsSpareBits,
                   std::vector<const uint8_t *> payloadLayoutPtr,
                   swift::Metadata *metadata)
      : numEmptyPayloads(numEmptyPayloads),
        payloadLayoutLength(payloadLayoutLength),
        payloadLayoutPtr(payloadLayoutPtr),
        extraTagBitsSpareBits(extraTagBitsSpareBits), metadata(metadata) {}
  const uint32_t numEmptyPayloads;
  const std::vector<uint32_t> payloadLayoutLength;
  const std::vector<const uint8_t *> payloadLayoutPtr;
  const BitVector extraTagBitsSpareBits;
  swift::Metadata *metadata;

  // The spare bits shared by all payloads, if any.
  // Invariant: The size of the bit vector is the size of the payload in bits,
  // rounded up to a byte boundary.
  const BitVector commonSpareBits() const;

  uint32_t payloadSize() const;
  BitVector spareBits() const;
  uint32_t size() const;
  uint32_t tagSize() const;
  uint32_t tagsInExtraInhabitants() const;
  uint32_t gatherSpareBits(const uint8_t *data, unsigned resultBitWidth) const;
};
AlignedGroup readAlignedGroup(const uint8_t *typeLayout,
                              swift::Metadata *metadata);
MultiPayloadEnum readMultiPayloadEnum(const uint8_t *typeLayout,
                                      swift::Metadata *metadata);
SinglePayloadEnum readSinglePayloadEnum(const uint8_t *typeLayout,
                                        swift::Metadata *metadata);

#endif // SWIFT_BYTECODE_LAYOUTS_H
