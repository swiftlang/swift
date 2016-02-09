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

#include "swift/Reflection/TypeRef.h"
#include "swift/Runtime/Metadata.h"

#include <iostream>

class NodePointer;

namespace swift {
namespace reflection {

class FieldRecord {
  const RelativeDirectPointer<const char> MangledTypeName;
  const RelativeDirectPointer<const char> FieldName;

public:
  std::string getMangledTypeName() const {
    return MangledTypeName.get();
  }

  std::string getFieldName()  const {
    return FieldName.get();
  }
};

struct FieldRecordIterator {
  const FieldRecord *Cur;
  const FieldRecord * const End;

  FieldRecordIterator(const FieldRecord *Cur, const FieldRecord * const End)
    : Cur(Cur), End(End) {}

  const FieldRecord &operator*() const {
    return *Cur;
  }

  FieldRecordIterator &operator++() {
    ++Cur;
    return *this;
  }

  bool operator==(const FieldRecordIterator &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(const FieldRecordIterator &other) const {
    return !(*this == other);
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

  using const_iterator = FieldRecordIterator;

  const_iterator begin() const {
    auto Begin = getFieldRecordBuffer();
    auto End = Begin + NumFields;
    return const_iterator { Begin, End };
  }

  const_iterator end() const {
    auto Begin = getFieldRecordBuffer();
    auto End = Begin + NumFields;
    return const_iterator { End, End };
  }

  std::string getMangledTypeName() const {
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

class ReflectionReader {
public:
  virtual std::vector<const ReflectionSection> getSections() const = 0;
  virtual size_t read(void *dst, void *src, size_t count) const = 0;
  virtual ~ReflectionReader() {}
};

class ReflectionContext {
  ReflectionReader &Reader;
public:
  ReflectionContext(ReflectionReader &Reader) : Reader(Reader) {}

  void dumpSections(std::ostream &OS) const {
    for (const auto &section : Reader.getSections()) {
      for (const auto &descriptor : section) {
        auto mangledTypeName = descriptor.getMangledTypeName();
        auto typeName = Demangle::demangleTypeAsString(mangledTypeName);
        auto demangleTree = Demangle::demangleTypeAsNode(mangledTypeName);
        auto TR = decodeDemangleNode(demangleTree);
        OS << typeName << '\n';
        for (size_t i = 0; i < typeName.size(); ++i)
          OS << '=';
        OS << '\n';
        TR->dump(OS);
        std::cout << std::endl;

        for (auto &field : descriptor) {
          auto fieldDemangleTree = Demangle::demangleTypeAsNode(
              field.getMangledTypeName());
          auto fieldTR = decodeDemangleNode(fieldDemangleTree);
          auto fieldName = field.getFieldName();

          OS << "- " << fieldName << ":\n";
          fieldTR->dump(OS);
          OS << std::endl;
        }
      }
    }
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
