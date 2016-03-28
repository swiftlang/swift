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

#include <dlfcn.h>
#include <cstdint>
#include <cstring>
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

  void *startAddress() {
    return const_cast<void *>(Begin);
  }

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }

  size_t size() const {
    return (char *)End - (char *)Begin;
  }
};

class MemoryReader {
public:
  virtual uint8_t getPointerSize() = 0;
  virtual uint8_t getSizeSize() = 0;
  virtual addr_t getSymbolAddress(const std::string &Name) = 0;
  virtual std::string readString(addr_t Address) = 0;
  virtual bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) = 0;

  template <typename IntegerType>
  bool readInteger(addr_t Address, IntegerType *Dest) {
    return readBytes(Address, (uint8_t*)Dest, sizeof(IntegerType));
  }

  virtual ~MemoryReader() = default;
};

class InProcessMemoryReader final : public MemoryReader {
  uint8_t getPointerSize() override {
    return sizeof(uintptr_t);
  }

  uint8_t getSizeSize() override {
    return sizeof(size_t);
  }

  addr_t getSymbolAddress(const std::string &Name) override {
    auto Symbol = dlsym(RTLD_DEFAULT, Name.c_str());
    auto Pointer = reinterpret_cast<uintptr_t>(Symbol);
    return static_cast<addr_t>(Pointer);
  }

  std::string readString(addr_t Address) override {
    return std::string((char *)Address);
  }

  bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) override {
    return memmove(Dest, reinterpret_cast<const void *>(Address), Size);
  }
};

class CMemoryReader final : public MemoryReader {
  std::unique_ptr<MemoryReaderImpl> Impl;

public:
  CMemoryReader() : Impl(nullptr) {}

  CMemoryReader(std::unique_ptr<MemoryReaderImpl> &&Impl)
    : Impl(std::move(Impl)) {
      assert(this->Impl && "No Memory reader implementation given!");
      assert(this->Impl->getPointerSize && "No getPointerSize implementation");
      assert(this->Impl->readInteger && "No readInteger implementation");
      assert(this->Impl->getStringLength && "No stringLength implementation");
      assert(this->Impl->readBytes && "No readBytes implementation");
      assert(this->Impl->getPointerSize() != 0 && "Invalid target pointer size");
  }

  uint8_t getPointerSize() override {
    return Impl->getPointerSize();
  }

  uint8_t getSizeSize() override {
    return Impl->getSizeSize();
  }

  addr_t getSymbolAddress(const std::string &Name) override {
    auto Address = Impl->getSymbolAddress(Name.c_str(), Name.size());
    return static_cast<addr_t>(Address);
  }

  uint64_t getStringLength(addr_t Address) {
    return Impl->getStringLength(Address);
  }

  std::string readString(addr_t Address) override {
    auto NameSize = getStringLength(Address);
    if (!NameSize)
      return "";

    auto NameBuffer = std::unique_ptr<uint8_t>(new uint8_t[NameSize + 1]);
    if (!readBytes(Address, NameBuffer.get(), NameSize + 1))
      return "";
    return std::string(reinterpret_cast<const char *>(NameBuffer.get()));
  }

  bool readBytes(addr_t Address, uint8_t *Dest, uint64_t Size) override {
    return Impl->readBytes(Address, Dest, Size);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

