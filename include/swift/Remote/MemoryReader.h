//===--- MemoryReader.h - Abstract access to remote memory ------*- C++ -*-===//
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
//  This file declares an abstract interface for working with the memory
//  of a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_MEMORYREADER_H
#define SWIFT_REMOTE_MEMORYREADER_H

#include "swift/Remote/RemoteAddress.h"
#include "swift/SwiftRemoteMirror/MemoryReaderInterface.h"

#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <tuple>

namespace swift {
namespace remote {

/// An abstract interface for reading memory.
///
/// This abstraction presents memory as if it were a read-only
/// representation of the address space of a remote process.
class MemoryReader {
public:
  /// A convenient name for the return type from readBytes.
  using ReadBytesResult = std::unique_ptr<const void, std::function<void(const void *)>>;

  virtual bool queryDataLayout(DataLayoutQueryType type, void *inBuffer,
                               void *outBuffer) = 0;

  /// Look up the given public symbol name in the remote process.
  virtual RemoteAddress getSymbolAddress(const std::string &name) = 0;

  /// Attempts to read a C string from the given address in the remote
  /// process.
  ///
  /// Returns false if the operation failed.
  virtual bool readString(RemoteAddress address, std::string &dest) = 0;
  
  /// Attempts to read an integer from the given address in the remote
  /// process.
  ///
  /// Returns false if the operation failed.
  template <typename IntegerType>
  bool readInteger(RemoteAddress address, IntegerType *dest) {
    return readBytes(address, reinterpret_cast<uint8_t*>(dest),
                     sizeof(IntegerType));
  }

  /// Attempts to read 'size' bytes from the given address in the remote process.
  ///
  /// Returns a pointer to the requested data and a function that must be called to
  /// free that data when done. The pointer will be NULL if the operation failed.
  ///
  /// NOTE: subclasses MUST override at least one of the readBytes functions. The default
  /// implementation calls through to the other one.
  virtual ReadBytesResult
    readBytes(RemoteAddress address, uint64_t size) {
    auto *Buf = malloc(size);
    ReadBytesResult Result(Buf, [](const void *ptr) {
      free(const_cast<void *>(ptr));
    });
    bool success = readBytes(address, reinterpret_cast<uint8_t *>(Buf), size);
    if (!success) {
      Result.reset();
    }
    return Result;
  }

  /// Attempts to read 'size' bytes from the given address in the
  /// remote process.
  ///
  /// Returns false if the operation failed.
  ///
  /// NOTE: subclasses MUST override at least one of the readBytes functions. The default
  /// implementation calls through to the other one.
  virtual bool readBytes(RemoteAddress address, uint8_t *dest, uint64_t size) {
    auto Ptr = readBytes(address, size);
    if (!Ptr)
      return false;
    
    memcpy(dest, Ptr.get(), size);
    return true;
  }
          
  virtual ~MemoryReader() = default;
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

