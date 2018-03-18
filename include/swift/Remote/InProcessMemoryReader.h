//===--- InProcessMemoryReader.h - Access to local memory -------*- C++ -*-===//
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
//  This file declares an abstract interface for working with remote memory.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_INPROCESSMEMORYREADER_H
#define SWIFT_REMOTE_INPROCESSMEMORYREADER_H

#include "swift/Remote/MemoryReader.h"

#include <cstring>

namespace swift {
namespace remote {

/// An implementation of MemoryReader which simply reads from the current
/// address space.
class InProcessMemoryReader final : public MemoryReader {
  bool queryDataLayout(DataLayoutQueryType type, void *inBuffer,
                       void *outBuffer) override {
    switch (type) {
      case DLQ_GetPointerSize: {
        auto result = static_cast<uint8_t *>(outBuffer);
        *result = sizeof(void *);
        return true;
      }
      case DLQ_GetSizeSize: {
        auto result = static_cast<uint8_t *>(outBuffer);
        *result = sizeof(size_t);
        return true;
      }
    }

    return false;
  }

  RemoteAddress getSymbolAddress(const std::string &name) override;

  bool readString(RemoteAddress address, std::string &dest) override {
    dest = address.getLocalPointer<char>();
    return true;
  }

  ReadBytesResult readBytes(RemoteAddress address, uint64_t size) override {
    return ReadBytesResult(address.getLocalPointer<void>(), [](const void *) {});
  }
};
 
}
}

#endif
