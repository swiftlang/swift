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
  uint8_t getPointerSize() override {
    return sizeof(uintptr_t);
  }

  uint8_t getSizeSize() override {
    return sizeof(size_t);
  }

  RemoteAddress getSymbolAddress(const std::string &name) override;

  bool readString(RemoteAddress address, std::string &dest) override {
    dest = address.getLocalPointer<char>();
    return true;
  }

  bool readBytes(RemoteAddress address, uint8_t *dest, uint64_t size) override {
    std::memcpy(dest, address.getLocalPointer<void>(), (size_t) size);
    return true;
  }
};
 
}
}

#endif
