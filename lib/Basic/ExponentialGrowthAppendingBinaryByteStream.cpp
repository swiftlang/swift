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
  if (Offset > this->Size)
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  if (Offset + Size > this->Size)
    return make_error<BinaryStreamError>(stream_error_code::stream_too_short);

  Buffer = ArrayRef<uint8_t>(Data + Offset, Size);
  return Error::success();
}

Error ExponentialGrowthAppendingBinaryByteStream::readLongestContiguousChunk(
    uint32_t Offset, ArrayRef<uint8_t> &Buffer) {
  if (Offset > this->Size)
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  Buffer = ArrayRef<uint8_t>(Data + Offset, Size - Offset);
  return Error::success();
}

void ExponentialGrowthAppendingBinaryByteStream::reserve(size_t Size) {
  if (Size <= ReservedBufferSize) {
    // The buffer is already large enough. Nothing to do.
    return;
  }
  Data = (uint8_t *)realloc(Data, Size);
  ReservedBufferSize = Size;
}

Error ExponentialGrowthAppendingBinaryByteStream::writeBytes(
    uint32_t Offset, ArrayRef<uint8_t> Buffer) {
  if (Buffer.empty())
    return Error::success();

  if (Offset > Size)
    return make_error<BinaryStreamError>(stream_error_code::invalid_offset);

  // Resize the internal buffer if needed.
  uint32_t RequiredSize = Offset + Buffer.size();
  uint32_t NewBufferSize = ReservedBufferSize;
  // Double the buffer size until its size is sufficient to hold the new data.
  while (RequiredSize > NewBufferSize) {
    NewBufferSize *= 2;
  }
  reserve(NewBufferSize);

  Size += Buffer.size();
  ::memcpy(Data + Offset, Buffer.data(), Buffer.size());

  return Error::success();
}
