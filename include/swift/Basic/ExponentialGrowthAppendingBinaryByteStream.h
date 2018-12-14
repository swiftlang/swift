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

/// An implementation of WritableBinaryStream which can write at its end
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

  /// This is an optimized version of \c writeBytes that assumes we know the
  /// size of \p Value at compile time (which in particular holds for integers).
  /// It does so by exposing the memcpy to the optimizer along with the size 
  /// of the value being assigned; the compiler can then optimize the memcpy
  /// into a fixed set of instructions.
  /// This assumes that the endianess of this steam is the same as the native
  /// endianess on the executing machine. No endianess transformations are
  /// performed.
  template<typename T>
  llvm::Error writeRaw(uint32_t Offset, T Value) {
    if (auto Error = checkOffsetForWrite(Offset, sizeof(T))) {
      return Error;
    }

    // Resize the internal buffer if needed.
    uint32_t RequiredSize = Offset + sizeof(T);
    if (RequiredSize > Data.size()) {
      Data.resize(RequiredSize);
    }

    ::memcpy(Data.data() + Offset, &Value, sizeof Value);

    return llvm::Error::success();
  }

  llvm::Error commit() override { return llvm::Error::success(); }

  virtual llvm::BinaryStreamFlags getFlags() const override {
    return llvm::BSF_Write | llvm::BSF_Append;
  }
};

} // end namespace swift

#endif /* SWIFT_BASIC_EXPONENTIALGROWTHAPPENDINGBINARYBYTESTREAM_H */
