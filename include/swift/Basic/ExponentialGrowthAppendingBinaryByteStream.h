//===--- ExponentialGrowthAppendingBinaryByteStream.h -----------*- C++ -*-===//
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
//
/// Defines an \c llvm::WritableBinaryStream whose internal buffer grows
/// exponentially in size as data is written to it. It is thus more efficient
/// than llvm::AppendingBinaryByteStream if a lot of small data gets appended to
/// it.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_EXPONENTIALGROWTHAPPENDINGBINARYBYTESTREAM_H
#define SWIFT_BASIC_EXPONENTIALGROWTHAPPENDINGBINARYBYTESTREAM_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/BinaryByteStream.h"

namespace swift {

/// \brief An implementation of WritableBinaryStream which can write at its end
/// causing the underlying data to grow.  This class owns the underlying data.
class ExponentialGrowthAppendingBinaryByteStream
    : public llvm::WritableBinaryStream {
  /// The buffer holding the data.
  uint8_t *Data;

  /// The size of the buffer that has been reserved. The buffer needs to get
  /// resized if more data shall be written.
  size_t ReservedBufferSize;

  /// The size of the buffer that has been initialized by the user using
  /// \c writeBytes. Since will always be less than or equal to
  /// \c ReservedBufferSize, but will most likely be strictly less becuase the
  /// internal buffer grows exponentially.
  size_t Size = 0;

  llvm::support::endianness Endian;

  void reserve(size_t Size);

public:
  ExponentialGrowthAppendingBinaryByteStream()
      : ExponentialGrowthAppendingBinaryByteStream(
            llvm::support::endianness::little) {}
  ExponentialGrowthAppendingBinaryByteStream(llvm::support::endianness Endian)
      : ExponentialGrowthAppendingBinaryByteStream(/*InitialSize=*/32, Endian) {
  }
  ExponentialGrowthAppendingBinaryByteStream(size_t InitialSize,
                                             llvm::support::endianness Endian)
      : Endian(Endian) {
    if (InitialSize == 0) {
      // The initial buffer needs to be at least one byte large so that doubling
      // its size has an effect.
      InitialSize = 1;
    }
    Data = (uint8_t *)malloc(InitialSize);
    ReservedBufferSize = InitialSize;
  }

  ~ExponentialGrowthAppendingBinaryByteStream() { free(Data); }

  llvm::support::endianness getEndian() const override { return Endian; }

  llvm::Error readBytes(uint32_t Offset, uint32_t Size,
                        ArrayRef<uint8_t> &Buffer) override;

  llvm::Error readLongestContiguousChunk(uint32_t Offset,
                                         ArrayRef<uint8_t> &Buffer) override;

  MutableArrayRef<uint8_t> data() { return {Data, Size}; }

  uint32_t getLength() override { return Size; }

  llvm::Error writeBytes(uint32_t Offset, ArrayRef<uint8_t> Buffer) override;

  llvm::Error commit() override { return llvm::Error::success(); }

  virtual llvm::BinaryStreamFlags getFlags() const override {
    return llvm::BSF_Write | llvm::BSF_Append;
  }
};

} // end namespace swift

#endif /* SWIFT_BASIC_EXPONENTIALGROWTHAPPENDINGBINARYBYTESTREAM_H */
