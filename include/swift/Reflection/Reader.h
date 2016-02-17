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

#include "swift/Reflection/Buffer.h"
#include "swift/Reflection/Records.h"

#include <cstdint>
#include <memory>
#include <vector>

class BufferImpl;

namespace swift {
namespace reflection {

using CopyFunction = size_t (*)(uintptr_t Source, void *Dest, size_t Size);

template <typename Iterator>
class ReflectionSection {
  using const_iterator = Iterator;
  const std::string ImageFilename;
  const void * const Begin;
  const void * const End;

public:
  ReflectionSection(const char *ImageFilename, const void * const Begin,
                    const void * const End)
  : ImageFilename(ImageFilename), Begin(Begin), End(End) {}

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }

  const std::string getImageFilename() const {
    return ImageFilename;
  }
};

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;

struct ReflectionInfo {
  FieldSection Fields;
  AssociatedTypeSection AssociatedTypes;
};

class MemoryReader {
  CopyFunction copy;
  std::vector<ReflectionInfo> Info;

public:
  MemoryReader() : copy(nullptr) {}

  template <typename T>
  Buffer<T> read(const T *Source) {
    using External = ExternalBuffer<T>;
    using Internal = InternalBuffer<T>;
    if (copy) {
      auto external = std::unique_ptr<External>(new External());
      auto bytesCopied = copy(reinterpret_cast<uintptr_t>(Source),
                              const_cast<void *>(external->getPointer()),
                              sizeof(T));

      if (bytesCopied != sizeof(T))
        return Buffer<T>();

      return Buffer<T>(std::move(external));
    } else {
      auto internal = new Internal(Source);
      return Buffer<T>(std::unique_ptr<Internal>(internal));
    }
  }

  void addReflectionInfo(ReflectionInfo I) {
    Info.push_back(I);
  }

  const std::vector<ReflectionInfo> &getInfo() const {
    return Info;
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

