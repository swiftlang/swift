//===--- ReflectionContext.h - Swift Type Reflection Context ----*- C++ -*-===//
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
// Implements the context for allocations and management of structures related
// to reflection, such as TypeRefs.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONCONTEXT_H
#define SWIFT_REFLECTION_REFLECTIONCONTEXT_H

#include "swift/Runtime/Metadata.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"

class NodePointer;

namespace swift {
namespace reflection {

class TypeRef;

using llvm::ArrayRef;
using llvm::StringRef;

class FieldRecord {
  const RelativeDirectPointer<const char> MangledTypeName;
  const RelativeDirectPointer<const char> FieldName;

public:
  StringRef getMangledTypeName() const {
    return MangledTypeName.get();
  }

  StringRef getFieldName()  const {
    return FieldName.get();
  }
};

struct FieldDescriptor {
  const FieldRecord *getFieldRecordBuffer() const {
    return reinterpret_cast<const FieldRecord *>(this + 1);
  }

  const RelativeDirectPointer<const char> MangledTypeName;

public:
  const uint32_t NumFields;
  const uint32_t FieldRecordSize;

  const ArrayRef<FieldRecord> getFieldRecords() const {
    auto Begin = reinterpret_cast<const FieldRecord *>(this + 1);
    return ArrayRef<FieldRecord>(Begin, NumFields);
  }

  const char *getMangledTypeName() const {
    return MangledTypeName.get();
  }
};

class FieldDescriptorIterator
  : public std::iterator<std::forward_iterator_tag, FieldDescriptor> {
public:
  const void *Cur;
  const void * const End;
  FieldDescriptorIterator(const void *Cur, const void * const End)
    : Cur(Cur), End(End) {}

  const FieldDescriptor &operator*() const {
    return *reinterpret_cast<const FieldDescriptor *>(Cur);
  }

  FieldDescriptorIterator &operator++() {
    const auto &FR = this->operator*();
    const void *Next = reinterpret_cast<const char *>(Cur)
      + sizeof(FieldDescriptor) + FR.NumFields * FR.FieldRecordSize;
    Cur = Next;
    return *this;
  }

  bool operator==(FieldDescriptorIterator const &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(FieldDescriptorIterator const &other) const {
    return !(*this == other);
  }
};

class ReflectionSection {
  using const_iterator = FieldDescriptorIterator;
  const std::string ImageFilename;
  const void * const Begin;
  const void * const End;

public:
  ReflectionSection(const std::string ImageFilename, const void * const Begin,
                    const void * const End)
    : ImageFilename(ImageFilename), Begin(Begin), End(End) {}

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }

  const std::string &getImageFilename() const {
    return ImageFilename;
  }
};

class ReflectionReader {
public:
  virtual std::vector<const ReflectionSection> getSections() const = 0;
  virtual size_t read(void *dst, void *src, size_t count) const = 0;
  virtual ~ReflectionReader() {}
};

class ReflectionContext {
  llvm::BumpPtrAllocator Allocator;

public:
  void *allocate(size_t Bytes, size_t Alignment) {
    return Allocator.Allocate(Bytes, Alignment);
  }

  template <typename T, typename It>
  T *allocateCopy(It Begin, It End) {
    T *Result = static_cast<T *>(allocate(sizeof(T) * (End - Begin),
                                          alignof(T)));
    for (size_t i = 0; Begin != End; ++Begin, ++i)
      new (Result + i) T(*Begin);
    return Result;
  }

  template <typename T>
  llvm::MutableArrayRef<T> allocateCopy(llvm::ArrayRef<T> Array) {
    return llvm::MutableArrayRef<T>(allocateCopy<T>(Array.begin(), Array.end()),
                              Array.size());
  }

  llvm::StringRef allocateCopy(llvm::StringRef Str) {
    llvm::ArrayRef<char> Result =
    allocateCopy(llvm::makeArrayRef(Str.data(), Str.size()));
    return llvm::StringRef(Result.data(), Result.size());
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
