//===--- Reader.h - Swift Type Reflection Memory Reader ---------*- C++ -*-===//
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
// Implements reading memory from the current or external process
// for reflection.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_READER_H
#define SWIFT_REFLECTION_READER_H

#include "swift/SwiftReflection/MemoryReaderInterface.h"

#include <cstdint>
#include <memory>
#include <vector>

namespace swift {
namespace reflection {

template <typename Iterator>
class ReflectionSection {
  using const_iterator = Iterator;
  const void * const Begin;
  const void * const End;

public:
  ReflectionSection(const void * const Begin,
                    const void * const End)
  : Begin(Begin), End(End) {}

  ReflectionSection(uint64_t Begin, uint64_t End)
  : Begin(reinterpret_cast<const void * const>(Begin)),
  End(reinterpret_cast<const void * const>(End)) {}

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }
};

class MemoryReader {
  std::unique_ptr<MemoryReaderImpl> Impl;

public:
  MemoryReader() : Impl(nullptr) {}

  MemoryReader(std::unique_ptr<MemoryReaderImpl> &&Impl)
    : Impl(std::move(Impl)) {
      assert(this->Impl && "No Memory reader implementation given!");
      assert(this->Impl->getPointerSize && "No getPointerSize implementation");
      assert(this->Impl->readInteger && "No readInteger implementation");
      assert(this->Impl->getStringLength && "No stringLength implementation");
      assert(this->Impl->readBytes && "No readBytes implementation");
      assert(this->Impl->getPointerSize() != 0 && "Invalid target pointer size");
  }

  uint8_t getPointerSize() {
    return Impl->getPointerSize();
  }

  template <typename IntegerType>
  bool readInteger(addr_t Address, IntegerType *Dest,
                   size_t Size = sizeof(IntegerType)) {
    assert(sizeof(IntegerType) == Size);
    uint64_t Temp;
    if (Impl->readInteger(Address, &Temp, Size)) {
      *Dest = (IntegerType)Temp;
      return true;
    }
    return false;
  }

  size_t getStringLength(addr_t Address) {
    return Impl->getStringLength(Address);
  }

  std::string readString(addr_t Address) {
    auto NameSize = getStringLength(Address);
    if (!NameSize)
      return "";

    auto NameBuffer = std::unique_ptr<uint8_t>(new uint8_t[NameSize + 1]);
    if (!readBytes(Address, NameBuffer.get(), NameSize))
      return "";
    return std::string(reinterpret_cast<const char *>(NameBuffer.get()));
  }

  bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) {
    return Impl->readBytes(Address, Dest, Size);
  }

};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

