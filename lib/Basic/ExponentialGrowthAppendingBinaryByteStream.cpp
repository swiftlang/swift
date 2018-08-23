//===-------- ExponentialGrowthAppendingBinaryByteStream.cpp --------------===//
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

using namespace llvm;
using namespace swift;

Error ExponentialGrowthAppendingBinaryByteStream::readBytes(
    uint32_t Offset, uint32_t Size, ArrayRef<uint8_t> &Buffer) {
  if (Offset > getLength())
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  if (Offset + Size > getLength())
    return make_error<BinaryStreamError>(stream_error_code::stream_too_short);

  Buffer = ArrayRef<uint8_t>(Data.data() + Offset, Size);
  return Error::success();
}

Error ExponentialGrowthAppendingBinaryByteStream::readLongestContiguousChunk(
    uint32_t Offset, ArrayRef<uint8_t> &Buffer) {
  if (Offset > getLength())
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  Buffer = ArrayRef<uint8_t>(Data.data() + Offset, Data.size() - Offset);
  return Error::success();
}

void ExponentialGrowthAppendingBinaryByteStream::reserve(size_t Size) {
  Data.reserve(Size);
}

Error ExponentialGrowthAppendingBinaryByteStream::writeBytes(
    uint32_t Offset, ArrayRef<uint8_t> Buffer) {
  if (Buffer.empty())
    return Error::success();

  if (Offset > getLength())
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  // Resize the internal buffer if needed.
  uint32_t RequiredSize = Offset + Buffer.size();
  if (RequiredSize > Data.size()) {
    Data.resize(RequiredSize);
  }

  ::memcpy(Data.data() + Offset, Buffer.data(), Buffer.size());

  return Error::success();
}
