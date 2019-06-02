//===--- ReflectionInfo.h --------------------------------*- C++ -*--------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_REFLECTIONINFO_H
#define SWIFT_REFLECTION_REFLECTIONINFO_H

#include "swift/Reflection/MetadataSource.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Remote/MetadataReader.h"

namespace swift {
namespace reflection {

template <typename IteratorTy> class ReflectionSection {
  using const_iterator = IteratorTy;
  uintptr_t startAddress;
  uintptr_t endAddress;

public:
  ReflectionSection(const void *startAddress, const void *endAddress)
      : startAddress(uintptr_t(startAddress)),
        endAddress(uintptr_t(endAddress)) {}

  ReflectionSection(uintptr_t startAddress, uintptr_t endAddress)
      : startAddress(startAddress), endAddress(endAddress) {}

  void *getStartAddress() { return reinterpret_cast<void *>(startAddress); }

  const void *getStartAddress() const {
    return reinterpret_cast<const void *>(startAddress);
  }

  const void *getEndAddress() const {
    return reinterpret_cast<const void *>(endAddress);
  }

  const_iterator begin() const {
    return const_iterator(getStartAddress(), getEndAddress());
  }

  const_iterator end() const {
    return const_iterator(getEndAddress(), getEndAddress());
  }

  size_t size() const { return endAddress - startAddress; }
};

/// A section of packed void * pointers.
using GenericSection = ReflectionSection<const void *>;

template <typename Runtime> class ReflectionContext;

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;

struct ReflectionInfo {
  struct {
    FieldSection Metadata;
    uint64_t SectionOffset;
  } Field;

  struct {
    AssociatedTypeSection Metadata;
    uint64_t SectionOffset;
  } AssociatedType;

  struct {
    BuiltinTypeSection Metadata;
    uint64_t SectionOffset;
  } Builtin;

  struct {
    CaptureSection Metadata;
    uint64_t SectionOffset;
  } Capture;

  struct {
    GenericSection Metadata;
    uint64_t SectionOffset;
  } TypeReference;

  struct {
    GenericSection Metadata;
    uint64_t SectionOffset;
  } ReflectionString;

  uint64_t LocalStartAddress;
  uint64_t RemoteStartAddress;
};

struct ClosureContextInfo {
  std::vector<const TypeRef *> CaptureTypes;
  std::vector<std::pair<const TypeRef *, const MetadataSource *>>
      MetadataSources;
  unsigned NumBindings = 0;

  void dump() const;
  void dump(std::ostream &OS) const;
};

struct FieldTypeInfo {
  std::string Name;
  const TypeRef *TR;
  bool Indirect;

  FieldTypeInfo() : Name(""), TR(nullptr), Indirect(false) {}
  FieldTypeInfo(const std::string &Name, const TypeRef *TR, bool Indirect)
      : Name(Name), TR(TR), Indirect(Indirect) {}

  static FieldTypeInfo forEmptyCase(std::string Name) {
    return FieldTypeInfo(Name, nullptr, false);
  }

  static FieldTypeInfo forIndirectCase(std::string Name, const TypeRef *TR) {
    return FieldTypeInfo(Name, TR, true);
  }

  static FieldTypeInfo forField(std::string Name, const TypeRef *TR) {
    return FieldTypeInfo(Name, TR, false);
  }
};

} // namespace reflection
} // namespace swift

#endif
