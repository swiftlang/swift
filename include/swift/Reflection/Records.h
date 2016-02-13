//===--- Records.h - Swift Type Reflection Records --------------*- C++ -*-===//
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
// Implements the structures of type reflection records.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_RECORDS_H
#define SWIFT_REFLECTION_RECORDS_H

#include "swift/Basic/RelativePointer.h"

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

class AssociatedTypeRecord {
  const RelativeDirectPointer<const char> Name;
  const RelativeDirectPointer<const char> SubstitutedTypeName;

public:
  std::string getName() const {
    return Name.get();
  }

  std::string getMangledSubstitutedTypeName() const {
    return SubstitutedTypeName.get();
  }
};

struct AssociatedTypeRecordIterator {
  const AssociatedTypeRecord *Cur;
  const AssociatedTypeRecord * const End;

  AssociatedTypeRecordIterator(const AssociatedTypeRecord *Cur,
                               const AssociatedTypeRecord * const End)
  : Cur(Cur), End(End) {}

  const AssociatedTypeRecord &operator*() const {
    return *Cur;
  }

  AssociatedTypeRecordIterator &operator++() {
    ++Cur;
    return *this;
  }

  bool operator==(const AssociatedTypeRecordIterator &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(const AssociatedTypeRecordIterator &other) const {
    return !(*this == other);
  }
};

struct AssociatedTypeDescriptor {
  const RelativeDirectPointer<const char> ConformingTypeName;
  const RelativeDirectPointer<const char> ProtocolTypeName;
  uint32_t NumAssociatedTypes;
  uint32_t AssociatedTypeRecordSize;

  const AssociatedTypeRecord *getAssociatedTypeRecordBuffer() const {
    return reinterpret_cast<const AssociatedTypeRecord *>(this + 1);
  }

public:
  using const_iterator = AssociatedTypeRecordIterator;

  const_iterator begin() const {
    auto Begin = getAssociatedTypeRecordBuffer();
    auto End = Begin + NumAssociatedTypes;
    return const_iterator { Begin, End };
  }

  const_iterator end() const {
    auto Begin = getAssociatedTypeRecordBuffer();
    auto End = Begin + NumAssociatedTypes;
    return const_iterator { End, End };
  }

  std::string getMangledProtocolTypeName() const {
    return ProtocolTypeName.get();
  }

  std::string getMangledConformingTypeName() const {
    return ConformingTypeName.get();
  }
};

class AssociatedTypeIterator
  : public std::iterator<std::forward_iterator_tag, AssociatedTypeDescriptor> {
public:
  const void *Cur;
  const void * const End;
  AssociatedTypeIterator(const void *Cur, const void * const End)
    : Cur(Cur), End(End) {}

  const AssociatedTypeDescriptor &operator*() const {
    return *reinterpret_cast<const AssociatedTypeDescriptor *>(Cur);
  }

  AssociatedTypeIterator &operator++() {
    const auto &ATR = this->operator*();
    size_t Size = sizeof(AssociatedTypeDescriptor) +
      ATR.NumAssociatedTypes * ATR.AssociatedTypeRecordSize;
    const void *Next = reinterpret_cast<const char *>(Cur) + Size;
    Cur = Next;
    return *this;
  }

  bool operator==(AssociatedTypeIterator const &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(AssociatedTypeIterator const &other) const {
    return !(*this == other);
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


} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_RECORDS_H
