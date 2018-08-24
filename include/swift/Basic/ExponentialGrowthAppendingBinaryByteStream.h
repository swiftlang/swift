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
  SmallVector<uint8_t, 0> Data;

  llvm::support::endianness Endian;
public:
  ExponentialGrowthAppendingBinaryByteStream()
      : ExponentialGrowthAppendingBinaryByteStream(
            llvm::support::endianness::little) {}
  ExponentialGrowthAppendingBinaryByteStream(llvm::support::endianness Endian)
      : Endian(Endian) {}

  void reserve(size_t Size);

  llvm::support::endianness getEndian() const override { return Endian; }

  llvm::Error readBytes(uint32_t Offset, uint32_t Size,
                        ArrayRef<uint8_t> &Buffer) override;

  llvm::Error readLongestContiguousChunk(uint32_t Offset,
                                         ArrayRef<uint8_t> &Buffer) override;

  MutableArrayRef<uint8_t> data() { return Data; }

  uint32_t getLength() override { return Data.size(); }

  llvm::Error writeBytes(uint32_t Offset, ArrayRef<uint8_t> Buffer) override;

  llvm::Error commit() override { return llvm::Error::success(); }

  virtual llvm::BinaryStreamFlags getFlags() const override {
    return llvm::BSF_Write | llvm::BSF_Append;
  }
};

} // end namespace swift

#endif /* SWIFT_BASIC_EXPONENTIALGROWTHAPPENDINGBINARYBYTESTREAM_H */
