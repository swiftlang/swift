//===--- Enum.cpp - Enum tests --------------------------------------------===//
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

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Enum.h"
#include "gtest/gtest.h"

using namespace swift;

// Mock up a value witness table for Builtin.Int8 will 254 and 255 as extra
// inhabitants.
ExtraInhabitantsValueWitnessTable Int8WithExtraInhabitantValueWitness
= {
  // ValueWitnessTable
  ValueWitnessTable{
#define STEAL_INT8_WITNESS(witness) VALUE_WITNESS_SYM(Bi8_).witness,
    FOR_ALL_FUNCTION_VALUE_WITNESSES(STEAL_INT8_WITNESS)
#undef STEAL_INT8_WITNESS
    VALUE_WITNESS_SYM(Bi8_).size,
    VALUE_WITNESS_SYM(Bi8_).flags.withExtraInhabitants(true),
    VALUE_WITNESS_SYM(Bi8_).stride
  },
  // extraInhabitantFlags
  ExtraInhabitantFlags().withNumExtraInhabitants(2),
  // storeExtraInhabitant
  [](OpaqueValue *dest, int index, const Metadata *self) {
    *reinterpret_cast<uint8_t*>(dest) = 254 + index;
  },
  // getExtraInhabitantIndex
  [](const OpaqueValue *src, const Metadata *self) -> int {
    uint8_t byte = *reinterpret_cast<const uint8_t*>(src);
    if (byte >= 254)
      return byte - 254;
    return -1;
  }
};

FullMetadata<OpaqueMetadata> XI_TMBi8_ = {
  {&Int8WithExtraInhabitantValueWitness},
  {{MetadataKind::Opaque}}
};

const OpaqueValue *asOpaque(const void *v) {
  return reinterpret_cast<const OpaqueValue*>(v);
}
OpaqueValue *asOpaque(void *v) {
  return reinterpret_cast<OpaqueValue*>(v);
}

int test_getEnumCaseSinglePayload(std::initializer_list<uint8_t> repr,
                                   const FullOpaqueMetadata &metadata,
                                   unsigned numEmptyCases) {
  return swift_getEnumCaseSinglePayload(asOpaque(repr.begin()),
                                         &metadata.base, numEmptyCases);
}

TEST(EnumTest, getEnumCaseSinglePayload) {
  // Test with no XI.
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({0, 0}, METADATA_SYM(Bi8_), 512));
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({255, 0}, METADATA_SYM(Bi8_), 512));

  ASSERT_EQ(0, test_getEnumCaseSinglePayload({0, 1}, METADATA_SYM(Bi8_), 512));
  ASSERT_EQ(255, test_getEnumCaseSinglePayload({255, 1}, METADATA_SYM(Bi8_), 512));
  ASSERT_EQ(511, test_getEnumCaseSinglePayload({255, 2}, METADATA_SYM(Bi8_), 512));

  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({0, 0, 0}, METADATA_SYM(Bi8_),
                                               128*1024));
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({255, 0, 0}, METADATA_SYM(Bi8_),
                                               128*1024));
#if defined(__BIG_ENDIAN__)
  ASSERT_EQ(65535 - 255,
            test_getEnumCaseSinglePayload({0, 1, 0}, METADATA_SYM(Bi8_),
                                           128*1024));
#else
  ASSERT_EQ(65535 - 255,
            test_getEnumCaseSinglePayload({0, 0, 1}, METADATA_SYM(Bi8_),
                                           128*1024));
#endif

  // Test with XI.
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({0}, XI_TMBi8_, 2));
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({253}, XI_TMBi8_, 2));
  ASSERT_EQ(0, test_getEnumCaseSinglePayload({254}, XI_TMBi8_, 2));
  ASSERT_EQ(1, test_getEnumCaseSinglePayload({255}, XI_TMBi8_, 2));

  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({0, 0}, XI_TMBi8_, 4));
  ASSERT_EQ(-1, test_getEnumCaseSinglePayload({253, 0}, XI_TMBi8_, 4));
  ASSERT_EQ(0, test_getEnumCaseSinglePayload({254, 0}, XI_TMBi8_, 4));
  ASSERT_EQ(1, test_getEnumCaseSinglePayload({255, 0}, XI_TMBi8_, 4));
  ASSERT_EQ(2, test_getEnumCaseSinglePayload({0, 1}, XI_TMBi8_, 4));
  ASSERT_EQ(3, test_getEnumCaseSinglePayload({1, 1}, XI_TMBi8_, 4));
}

bool test_storeEnumTagSinglePayload(std::initializer_list<uint8_t> after,
                                     std::initializer_list<uint8_t> before,
                                     const FullOpaqueMetadata &metadata,
                                     unsigned whichCase,
                                     unsigned numEmptyCases) {
  assert(after.size() == before.size());

  std::vector<uint8_t> buf;
  buf.resize(before.size());
  memcpy(buf.data(), before.begin(), before.size());

  swift_storeEnumTagSinglePayload(asOpaque(buf.data()),
                                   &metadata.base,
                                   whichCase,
                                   numEmptyCases);

  return memcmp(buf.data(), after.begin(), after.size()) == 0;
}

TEST(EnumTest, storeEnumTagSinglePayload) {
  // Test with no XI.
  ASSERT_TRUE(test_storeEnumTagSinglePayload({219, 0}, {219, 123},
                                              METADATA_SYM(Bi8_), -1, 512));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 1}, {219, 123},
                                              METADATA_SYM(Bi8_), 0, 512));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 1}, {219, 123},
                                              METADATA_SYM(Bi8_), 255, 512));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 2}, {219, 123},
                                              METADATA_SYM(Bi8_), 511, 512));

  ASSERT_TRUE(test_storeEnumTagSinglePayload({219, 0, 0}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), -1, 128*1024));
#if defined(__BIG_ENDIAN__)
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 0, 1}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 0, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 0, 1}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 255, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 0, 2}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 256, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 2, 0}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 128*1024 - 1, 128*1024));
#else
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 1, 0}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 0, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 1, 0}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 255, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 2, 0}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 256, 128*1024));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 0, 2}, {219, 123, 77},
                                              METADATA_SYM(Bi8_), 128*1024 - 1, 128*1024));
#endif

  // Test with XI.
  ASSERT_TRUE(test_storeEnumTagSinglePayload({219}, {219},
                                              XI_TMBi8_, -1, 2));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({254}, {219},
                                              XI_TMBi8_, 0, 2));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255}, {219},
                                              XI_TMBi8_, 1, 2));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({219, 0}, {219, 123},
                                              XI_TMBi8_, -1, 4));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({254, 0}, {219, 123},
                                              XI_TMBi8_, 0, 4));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({255, 0}, {219, 123},
                                              XI_TMBi8_, 1, 4));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({0, 1}, {219, 123},
                                              XI_TMBi8_, 2, 4));
  ASSERT_TRUE(test_storeEnumTagSinglePayload({1, 1}, {219, 123},
                                              XI_TMBi8_, 3, 4));
}
