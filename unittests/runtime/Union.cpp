//===- swift/unittests/runtime/Union.cpp - Union tests --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Union.h"
#include "gtest/gtest.h"

using namespace swift;

// Mock up a value witness table for Builtin.Int8 will 254 and 255 as extra
// inhabitants.
ExtraInhabitantsValueWitnessTable Int8WithExtraInhabitantValueWitness
= {
  // ValueWitnessTable
  ValueWitnessTable{
#define STEAL_INT8_WITNESS(witness) _TWVBi8_.witness,
    FOR_ALL_FUNCTION_VALUE_WITNESSES(STEAL_INT8_WITNESS)
#undef STEAL_INT8_WITNESS
    _TWVBi8_.size,
    _TWVBi8_.flags.withExtraInhabitants(true),
    _TWVBi8_.stride
  },
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
  },
  // extraInhabitantFlags
  ExtraInhabitantFlags().withNumExtraInhabitants(2)
};

FullMetadata<OpaqueMetadata> XI_TMdBi8_ = {
  {&Int8WithExtraInhabitantValueWitness},
  {{MetadataKind::Opaque}}
};

const OpaqueValue *asOpaque(const void *v) {
  return reinterpret_cast<const OpaqueValue*>(v);
}
OpaqueValue *asOpaque(void *v) {
  return reinterpret_cast<OpaqueValue*>(v);
}

int test_getUnionCaseSinglePayload(std::initializer_list<uint8_t> repr,
                                   const FullOpaqueMetadata &metadata,
                                   unsigned numEmptyCases) {
  return swift_getUnionCaseSinglePayload(asOpaque(repr.begin()),
                                         &metadata.base, numEmptyCases);
}

TEST(UnionTest, getUnionCaseSinglePayload) {
  // Test with no XI.
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({0, 0}, _TMdBi8_, 512));
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({255, 0}, _TMdBi8_, 512));

  ASSERT_EQ(0, test_getUnionCaseSinglePayload({0, 1}, _TMdBi8_, 512));
  ASSERT_EQ(255, test_getUnionCaseSinglePayload({255, 1}, _TMdBi8_, 512));
  ASSERT_EQ(511, test_getUnionCaseSinglePayload({255, 2}, _TMdBi8_, 512));

  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({0, 0, 0}, _TMdBi8_,
                                               128*1024));
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({255, 0, 0}, _TMdBi8_,
                                               128*1024));
  ASSERT_EQ(65535 - 255,
            test_getUnionCaseSinglePayload({0, 0, 1}, _TMdBi8_,
                                           128*1024));

  // Test with XI.
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({0}, XI_TMdBi8_, 2));
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({253}, XI_TMdBi8_, 2));
  ASSERT_EQ(0, test_getUnionCaseSinglePayload({254}, XI_TMdBi8_, 2));
  ASSERT_EQ(1, test_getUnionCaseSinglePayload({255}, XI_TMdBi8_, 2));

  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({0, 0}, XI_TMdBi8_, 4));
  ASSERT_EQ(-1, test_getUnionCaseSinglePayload({253, 0}, XI_TMdBi8_, 4));
  ASSERT_EQ(0, test_getUnionCaseSinglePayload({254, 0}, XI_TMdBi8_, 4));
  ASSERT_EQ(1, test_getUnionCaseSinglePayload({255, 0}, XI_TMdBi8_, 4));
  ASSERT_EQ(2, test_getUnionCaseSinglePayload({0, 1}, XI_TMdBi8_, 4));
  ASSERT_EQ(3, test_getUnionCaseSinglePayload({1, 1}, XI_TMdBi8_, 4));
}

bool test_storeUnionTagSinglePayload(std::initializer_list<uint8_t> after,
                                     std::initializer_list<uint8_t> before,
                                     const FullOpaqueMetadata &metadata,
                                     unsigned whichCase,
                                     unsigned numEmptyCases) {
  assert(after.size() == before.size());

  std::vector<uint8_t> buf;
  buf.resize(before.size());
  memcpy(buf.data(), before.begin(), before.size());

  swift_storeUnionTagSinglePayload(asOpaque(buf.data()),
                                   &metadata.base,
                                   whichCase,
                                   numEmptyCases);

  return memcmp(buf.data(), after.begin(), after.size()) == 0;
}

TEST(UnionTest, storeUnionTagSinglePayload) {
  // Test with no XI.
  ASSERT_TRUE(test_storeUnionTagSinglePayload({219, 0}, {219, 123},
                                              _TMdBi8_, -1, 512));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({0, 1}, {219, 123},
                                              _TMdBi8_, 0, 512));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255, 1}, {219, 123},
                                              _TMdBi8_, 255, 512));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255, 2}, {219, 123},
                                              _TMdBi8_, 511, 512));

  ASSERT_TRUE(test_storeUnionTagSinglePayload({219, 0, 0}, {219, 123, 77},
                                              _TMdBi8_, -1, 128*1024));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({0, 1, 0}, {219, 123, 77},
                                              _TMdBi8_, 0, 128*1024));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255, 1, 0}, {219, 123, 77},
                                              _TMdBi8_, 255, 128*1024));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({0, 2, 0}, {219, 123, 77},
                                              _TMdBi8_, 256, 128*1024));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255, 0, 2}, {219, 123, 77},
                                              _TMdBi8_, 128*1024 - 1, 128*1024));

  // Test with XI.
  ASSERT_TRUE(test_storeUnionTagSinglePayload({219}, {219},
                                              XI_TMdBi8_, -1, 2));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({254}, {219},
                                              XI_TMdBi8_, 0, 2));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255}, {219},
                                              XI_TMdBi8_, 1, 2));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({219, 0}, {219, 123},
                                              XI_TMdBi8_, -1, 4));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({254, 0}, {219, 123},
                                              XI_TMdBi8_, 0, 4));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({255, 0}, {219, 123},
                                              XI_TMdBi8_, 1, 4));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({0, 1}, {219, 123},
                                              XI_TMdBi8_, 2, 4));
  ASSERT_TRUE(test_storeUnionTagSinglePayload({1, 1}, {219, 123},
                                              XI_TMdBi8_, 3, 4));
}
