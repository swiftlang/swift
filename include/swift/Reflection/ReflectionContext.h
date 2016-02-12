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

struct ReflectionSectionReader {
  std::vector<FieldSection> FieldSections;
  std::vector<AssociatedTypeSection> AssociatedTypeSections;

  ReflectionSectionReader(std::vector<FieldSection> FieldSections,
                          std::vector<AssociatedTypeSection>
                            AssociatedTypeSections)
    : FieldSections(FieldSections),
      AssociatedTypeSections(AssociatedTypeSections) {}
};

class ReflectionContext {
  ReflectionSectionReader &Reader;

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false) const {
    auto TypeName = Demangle::demangleTypeAsString(MangledName);
    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = decodeDemangleNode(DemangleTree);
    OS << TypeName << '\n';
    TR->dump(OS);
    std::cout << std::endl;
  }

public:
  ReflectionContext(ReflectionSectionReader &Reader) : Reader(Reader) {}

  void dumpFieldSection(std::ostream &OS) const {
    for (const auto &section : Reader.FieldSections) {
      for (const auto &descriptor : section) {
        dumpTypeRef(descriptor.getMangledTypeName(), OS);
        for (auto &field : descriptor) {
          OS << field.getFieldName() << ": ";
          dumpTypeRef(field.getMangledTypeName(), OS);
        }
      }
    }
  }

  void dumpAssociatedTypeSection(std::ostream &OS) const {
    for (const auto &section : Reader.AssociatedTypeSections) {
      for (const auto &descriptor : section) {
        auto conformingTypeName = Demangle::demangleTypeAsString(
          descriptor.getMangledConformingTypeName());
        auto protocolName = Demangle::demangleTypeAsString(
          descriptor.getMangledProtocolTypeName());

        OS << conformingTypeName << " : " << protocolName;
        OS << std::endl;

        for (const auto &associatedType : descriptor) {
          OS << "typealias " << associatedType.getName() << " = ";
          dumpTypeRef(associatedType.getMangledSubstitutedTypeName(), OS);
        }
      }
    }
  }

  void dumpAllSections(std::ostream &OS) const {
    OS << "FIELDS:\n";
    for (size_t i = 0; i < 7; ++i) OS << '=';
    OS << std::endl;
    dumpFieldSection(OS);
    OS << "\nASSOCIATED TYPES:\n";
    for (size_t i = 0; i < 17; ++i) OS << '=';
    OS << std::endl;
    dumpAssociatedTypeSection(OS);
    OS << std::endl;
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
