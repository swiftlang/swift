//===--- CompressionTests.cpp - for swift/ABI/Compression.h ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#include "swift/ABI/Compression.h"
#include "gtest/gtest.h"

using namespace swift::Compress;

const char* TestValues[] = {"AA", "r", "J", "Swift","A", "ArrayStringPrintable",
  "AB","JA","YA","encodeCBCString", "HelloWorld", "long", "_TThisIsATestString"
  "Done", "Smile","_S_S_S", "________", "_TSLZ","Lempel_Ziv", "Ziv_and_Lempel",
  "JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ",
  "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ",
  "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY",
  "AllDone", "UnderWaterCordlessDrill","UnderWaterAngleGrinder", "Printable", "Clone", "Collection"
  "__TwxxV14StdlibUnittest24MinimalForwardCollection",
  "__TMPVs15ContiguousArray",
  "__TIF14StdlibUnittest13checkSequenceu0_Rxs14CollectionType_s12SequenceTypeWx9Generator7Element_zW_9GeneratorS3__rFTxq",
  "__TTSg5VSS13CharacterViewS_s14CollectionTypes_GVs17IndexingGeneratorS__GS1_S__s13GeneratorTypes_VS_5IndexS3_s16Forwar",
  "__TMANameWith$dollar$inIt",
  ""};

// Test that the code book compression round trips.
TEST(Compression, RoundTripCBC) {
    for (const char* N : TestValues) {
      std::string encoded = EncodeCBCString(N);
      std::string decoded = DecodeCBCString(encoded);
      EXPECT_EQ(decoded, std::string(N));
    }
}

// Test flat (non variable length) encoding.
TEST(Compression, FlatEncoding) {
    for (const char* input : TestValues) {
      llvm::APInt flat_code = EncodeStringAsNumber(input, EncodingKind::Fixed);
      std::string flat_input = DecodeStringFromNumber(flat_code, EncodingKind::Fixed);
      EXPECT_EQ(flat_input, input);
    }

    // Check that we can encode and decode all numbers and that we get the
    // correct value after round trips.
    for (int i = 0; i < 10000; i++) {
      llvm::APInt num = llvm::APInt(64, i);
      std::string encoded = DecodeStringFromNumber(num, EncodingKind::Fixed);
      llvm::APInt decoded_num = EncodeStringAsNumber(encoded, EncodingKind::Fixed);
      EXPECT_EQ(num.getZExtValue(), decoded_num.getZExtValue());
    }
}

// Test variable length encoding.
TEST(Compression, VarEncoding) {
    for (const char* input : TestValues) {
      llvm::APInt var_code = EncodeStringAsNumber(input, EncodingKind::Variable);
      std::string var_input = DecodeStringFromNumber(var_code, EncodingKind::Variable);
      EXPECT_EQ(var_input, input);
    }
}

TEST(Compression, VariableLength) {
    for (const char* input : TestValues) {
      llvm::APInt code = EncodeStringAsNumber(input, EncodingKind::Variable);

      std::string encoded = DecodeStringFromNumber(code, EncodingKind::Fixed);
      llvm::APInt code2 =   EncodeStringAsNumber(encoded, EncodingKind::Fixed);

      std::string encoded2 = DecodeStringFromNumber(code2, EncodingKind::Fixed);
      llvm::APInt code3 =    EncodeStringAsNumber(encoded2, EncodingKind::Fixed);
      EXPECT_EQ(code.toString(10, false), code2.toString(10, false));
      EXPECT_EQ(code3, code2);

      std::string decoded = DecodeStringFromNumber(code2, EncodingKind::Variable);
      EXPECT_EQ(decoded, input);
    }
}

TEST(Compression, FullCompression) {
    for (const char* input : TestValues) {
      std::string compressed = CompressName(input);
      std::string decompressed = DecompressName(compressed);
      EXPECT_EQ(std::string(input), decompressed);
    }
}
