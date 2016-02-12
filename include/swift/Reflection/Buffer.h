//===--- Buffer.h - Swift Type Reflection Buffers ---------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Abstracts indirecting pointers in the current address space or a remote
// address space.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_BUFFER_H
#define SWIFT_REFLECTION_BUFFER_H

#include "swift/Reflection/Reader.h"

namespace swift {
namespace reflection {

/// An abstract impl interface for getting the head of what may be a buffer
/// copied from an external address space.
class BufferImpl {
public:
  virtual const void *getPointer() = 0;
  virtual ~BufferImpl() {}
};

/// A trivial wrapper for pointers in the current address space.
template <typename T>
class InternalBuffer final : public BufferImpl {
  const T *Source;

public:
  InternalBuffer(const T *Source) : Source(Source) {}
  virtual const void *getPointer() override {
    return reinterpret_cast<const void *>(Source);
  }
};

/// A buffer of data copied out of an external address space.
template <typename T>
class ExternalBuffer final : public BufferImpl {
  uint8_t Buf[sizeof(T)] = {0};

public:
  virtual const void *getPointer() override {
    return reinterpret_cast<const void *>(Buf);
  }
};

/// An abstraction over a pointer which may be in another
/// address space.
///
/// These will always be backed by real data, but it is
/// important not to indirect twice out of this type.
/// Ask the MemoryReader to read each pointer. For example:
/// Buffer<MyStruct> MS = Reader.read(APointer);
/// int i = MS->i; // OK.
///
/// If MyStruct has a nested MyStruct *, you read it like so:
/// Buffer<MyStruct> MS = Reader.read(MS->nestedMyStruct);
template <typename T>
class Buffer {
  friend class MemoryReader;

  std::unique_ptr<BufferImpl> Impl;

  Buffer() : Impl(nullptr) {}

  Buffer(std::unique_ptr<BufferImpl> Impl)
  : Impl(std::move(Impl)) {}

public:
  const T& operator*() {
    return *reinterpret_cast<const T*>(Impl->getPointer());
  }

  const T* operator->() {
    return reinterpret_cast<const T*>(Impl->getPointer());
  }

  operator bool() const {
    return bool(Impl);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_BUFFER_H