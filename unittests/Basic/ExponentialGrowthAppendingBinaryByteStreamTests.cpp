//===--- ExponentialGrowthAppendingBinaryByteStreamTests.cpp -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/ExponentialGrowthAppendingBinaryByteStream.h"
#include "llvm/Support/BinaryStreamReader.h"
#include "llvm/Support/BinaryStreamWriter.h"
#include "llvm/Testing/Support/Error.h"

#include "gtest/gtest.h"

using namespace llvm;
using namespace swift;

class ExponentialGrowthAppendingBinaryByteStreamTest : public testing::Test {};

// This test case has been taken and adapted from
// unittests/BinaryStreamTests.cpp in the LLVM project
TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, ReadAndWrite) {
  StringRef Strings[] = {"1", "2", "3", "4"};
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();

  BinaryStreamWriter Writer(Stream);
  BinaryStreamReader Reader(Stream);
  const uint8_t *Byte;
  StringRef Str;

  // When the stream is empty, it should report a 0 length and we should get an
  // error trying to read even 1 byte from it.
  BinaryStreamRef ConstRef(Stream);
  EXPECT_EQ(0U, ConstRef.getLength());
  EXPECT_THAT_ERROR(Reader.readObject(Byte), Failed());

  // But if we write to it, its size should increase and we should be able to
  // read not just a byte, but the string that was written.
  EXPECT_THAT_ERROR(Writer.writeCString(Strings[0]), Succeeded());
  EXPECT_EQ(2U, ConstRef.getLength());
  EXPECT_THAT_ERROR(Reader.readObject(Byte), Succeeded());

  Reader.setOffset(0);
  EXPECT_THAT_ERROR(Reader.readCString(Str), Succeeded());
  EXPECT_EQ(Str, Strings[0]);

  // If we drop some bytes from the front, we should still track the length as
  // the
  // underlying stream grows.
  BinaryStreamRef Dropped = ConstRef.drop_front(1);
  EXPECT_EQ(1U, Dropped.getLength());

  EXPECT_THAT_ERROR(Writer.writeCString(Strings[1]), Succeeded());
  EXPECT_EQ(4U, ConstRef.getLength());
  EXPECT_EQ(3U, Dropped.getLength());

  // If we drop zero bytes from the back, we should continue tracking the
  // length.
  Dropped = Dropped.drop_back(0);
  EXPECT_THAT_ERROR(Writer.writeCString(Strings[2]), Succeeded());
  EXPECT_EQ(6U, ConstRef.getLength());
  EXPECT_EQ(5U, Dropped.getLength());

  // If we drop non-zero bytes from the back, we should stop tracking the
  // length.
  Dropped = Dropped.drop_back(1);
  EXPECT_THAT_ERROR(Writer.writeCString(Strings[3]), Succeeded());
  EXPECT_EQ(8U, ConstRef.getLength());
  EXPECT_EQ(4U, Dropped.getLength());
}

TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, WriteAtInvalidOffset) {
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();
  EXPECT_EQ(0U, Stream.getLength());

  std::vector<uint8_t> InputData = {'T', 'e', 's', 't', 'T', 'e', 's', 't'};
  auto Test = llvm::ArrayRef(InputData).take_front(4);
  // Writing past the end of the stream is an error.
  EXPECT_THAT_ERROR(Stream.writeBytes(4, Test), Failed());

  // Writing exactly at the end of the stream is ok.
  EXPECT_THAT_ERROR(Stream.writeBytes(0, Test), Succeeded());
  EXPECT_EQ(Test, Stream.data());

  // And now that the end of the stream is where we couldn't write before, now
  // we can write.
  EXPECT_THAT_ERROR(Stream.writeBytes(4, Test), Succeeded());
  EXPECT_EQ(MutableArrayRef<uint8_t>(InputData), Stream.data());
}

TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, InitialSizeZero) {
  // Test that the stream also works with an initial size of 0, which doesn't
  // grow when doubled.
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();

  std::vector<uint8_t> InputData = {'T', 'e', 's', 't'};
  auto Test = llvm::ArrayRef(InputData).take_front(4);
  EXPECT_THAT_ERROR(Stream.writeBytes(0, Test), Succeeded());
  EXPECT_EQ(Test, Stream.data());
}

TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, GrowMultipleSteps) {
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();

  // Test that the buffer can grow multiple steps at once, e.g. 1 -> 2 -> 4
  std::vector<uint8_t> InputData = {'T', 'e', 's', 't'};
  auto Test = llvm::ArrayRef(InputData).take_front(4);
  EXPECT_THAT_ERROR(Stream.writeBytes(0, Test), Succeeded());
  EXPECT_EQ(Test, Stream.data());
}

TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, WriteIntoMiddle) {
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();

  // Test that the stream resizes correctly if we write into its middle
  std::vector<uint8_t> InitialData = {'T', 'e', 's', 't'};
  auto InitialDataRef = llvm::ArrayRef(InitialData);
  EXPECT_THAT_ERROR(Stream.writeBytes(0, InitialDataRef), Succeeded());
  EXPECT_EQ(InitialDataRef, Stream.data());

  std::vector<uint8_t> UpdateData = {'u'};
  auto UpdateDataRef = llvm::ArrayRef(UpdateData);
  EXPECT_THAT_ERROR(Stream.writeBytes(1, UpdateDataRef), Succeeded());

  std::vector<uint8_t> DataAfterUpdate = {'T', 'u', 's', 't'};
  auto DataAfterUpdateRef = llvm::ArrayRef(DataAfterUpdate);
  EXPECT_EQ(DataAfterUpdateRef, Stream.data());
  EXPECT_EQ(4u, Stream.getLength());

  std::vector<uint8_t> InsertData = {'e', 'r'};
  auto InsertDataRef = llvm::ArrayRef(InsertData);
  EXPECT_THAT_ERROR(Stream.writeBytes(4, InsertDataRef), Succeeded());

  std::vector<uint8_t> DataAfterInsert = {'T', 'u', 's', 't', 'e', 'r'};
  auto DataAfterInsertRef = llvm::ArrayRef(DataAfterInsert);
  EXPECT_EQ(DataAfterInsertRef, Stream.data());
  EXPECT_EQ(6u, Stream.getLength());
}

TEST_F(ExponentialGrowthAppendingBinaryByteStreamTest, WriteInteger) {
  auto Stream = ExponentialGrowthAppendingBinaryByteStream();

  // Test the writeInteger method
  std::vector<uint8_t> InitialData = {'H', 'e', 'l', 'l', 'o'};
  auto InitialDataRef = llvm::ArrayRef(InitialData);
  EXPECT_THAT_ERROR(Stream.writeBytes(0, InitialDataRef), Succeeded());
  EXPECT_EQ(InitialDataRef, Stream.data());

  EXPECT_THAT_ERROR(Stream.writeInteger(5, (uint8_t)' '), Succeeded());
  std::vector<uint8_t> AfterFirstInsert = {'H', 'e', 'l', 'l', 'o', ' '};
  auto AfterFirstInsertRef = llvm::ArrayRef(AfterFirstInsert);
  EXPECT_EQ(AfterFirstInsertRef, Stream.data());
  EXPECT_EQ(6u, Stream.getLength());

  uint32_t ToInsert = 'w' | 
                      'o' << 8 |
                      'r' << 16 |
                      'l' << 24;
  EXPECT_THAT_ERROR(Stream.writeInteger(6, ToInsert), Succeeded());
  std::vector<uint8_t> AfterSecondInsert = {'H', 'e', 'l', 'l', 'o', ' ',
                                            'w', 'o', 'r', 'l'};
  auto AfterSecondInsertRef = llvm::ArrayRef(AfterSecondInsert);
  EXPECT_EQ(AfterSecondInsertRef, Stream.data());
  EXPECT_EQ(10u, Stream.getLength());
}
