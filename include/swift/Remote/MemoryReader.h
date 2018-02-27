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

#include <functional>
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
  /// Return the size of an ordinary pointer in the remote process, in bytes.
  virtual uint8_t getPointerSize() = 0;

  /// Return the size of size_t in the remote process, in bytes.
  virtual uint8_t getSizeSize() = 0;

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
  virtual std::tuple<const void *, std::function<void()>>
    readBytes(RemoteAddress address, uint64_t size) {
    void *buffer = malloc(size);
    bool success = readBytes(address, reinterpret_cast<uint8_t *>(buffer), size);
    if (!success) {
      free(buffer);
      return std::make_tuple(nullptr, []{});
    }
    return std::make_tuple(buffer, [=]{ free(buffer); });
  }

  /// Attempts to read 'size' bytes from the given address in the
  /// remote process.
  ///
  /// Returns false if the operation failed.
  ///
  /// NOTE: subclasses MUST override at least one of the readBytes functions. The default
  /// implementation calls through to the other one.
  virtual bool readBytes(RemoteAddress address, uint8_t *dest, uint64_t size) {
    const void *ptr;
    std::function<void()> freeFunc;
    std::tie(ptr, freeFunc) = readBytes(address, size);
    if (ptr != nullptr) {
      memcpy(dest, ptr, size);
      freeFunc();
      return true;
    } else {
      return false;
    }
  }
          
  virtual ~MemoryReader() = default;
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

