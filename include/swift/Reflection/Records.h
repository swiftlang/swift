//===--- Records.h - Swift Type Reflection Records --------------*- C++ -*-===//
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
// Implements the structures of type reflection records.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_RECORDS_H
#define SWIFT_REFLECTION_RECORDS_H

#include "swift/Basic/RelativePointer.h"
#include "swift/Demangling/Demangle.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {

const uint16_t SWIFT_REFLECTION_METADATA_VERSION = 3; // superclass field

namespace reflection {

// Field records describe the type of a single stored property or case member
// of a class, struct or enum.
class FieldRecordFlags {
  using int_type = uint32_t;
  enum : int_type {
    // Is this an indirect enum case?
    IsIndirectCase = 0x1,
    
    // Is this a mutable `var` property?
    IsVar = 0x2,
  };
  int_type Data = 0;

public:
  bool isIndirectCase() const {
    return (Data & IsIndirectCase) == IsIndirectCase;
  }

  bool isVar() const {
    return (Data & IsVar) == IsVar;
  }

  void setIsIndirectCase(bool IndirectCase=true) {
    if (IndirectCase)
      Data |= IsIndirectCase;
    else
      Data &= ~IsIndirectCase;
  }

  void setIsVar(bool Var=true) {
    if (Var)
      Data |= IsVar;
    else
      Data &= ~IsVar;
  }

  int_type getRawValue() const {
    return Data;
  }
};

class FieldRecord {
  const FieldRecordFlags Flags;
  const RelativeDirectPointer<const char> MangledTypeName;
  const RelativeDirectPointer<const char> FieldName;

public:
  FieldRecord() = delete;

  bool hasMangledTypeName() const {
    return MangledTypeName;
  }

  StringRef getMangledTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char *)((uintptr_t)MangledTypeName.get() + Offset));
  }

  StringRef getFieldName(uintptr_t Offset)  const {
    if (FieldName)
      return (const char *)((uintptr_t)FieldName.get() + Offset);
    return "";
  }

  bool isIndirectCase() const {
    return Flags.isIndirectCase();
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

  const FieldRecord *operator->() const {
    return Cur;
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

enum class FieldDescriptorKind : uint16_t {
  // Swift nominal types.
  Struct,
  Class,
  Enum,

  // Fixed-size multi-payload enums have a special descriptor format that
  // encodes spare bits.
  //
  // FIXME: Actually implement this. For now, a descriptor with this kind
  // just means we also have a builtin descriptor from which we get the
  // size and alignment.
  MultiPayloadEnum,

  // A Swift opaque protocol. There are no fields, just a record for the
  // type itself.
  Protocol,

  // A Swift class-bound protocol.
  ClassProtocol,

  // An Objective-C protocol, which may be imported or defined in Swift.
  ObjCProtocol,

  // An Objective-C class, which may be imported or defined in Swift.
  // In the former case, field type metadata is not emitted, and
  // must be obtained from the Objective-C runtime.
  ObjCClass
};

// Field descriptors contain a collection of field records for a single
// class, struct or enum declaration.
class FieldDescriptor {
  const FieldRecord *getFieldRecordBuffer() const {
    return reinterpret_cast<const FieldRecord *>(this + 1);
  }

  const RelativeDirectPointer<const char> MangledTypeName;
  const RelativeDirectPointer<const char> Superclass;

public:
  FieldDescriptor() = delete;

  const FieldDescriptorKind Kind;
  const uint16_t FieldRecordSize;
  const uint32_t NumFields;

  using const_iterator = FieldRecordIterator;

  bool isEnum() const {
    return (Kind == FieldDescriptorKind::Enum ||
            Kind == FieldDescriptorKind::MultiPayloadEnum);
  }

  bool isClass() const {
    return (Kind == FieldDescriptorKind::Class ||
            Kind == FieldDescriptorKind::ObjCClass);
  }

  bool isProtocol() const {
    return (Kind == FieldDescriptorKind::Protocol ||
            Kind == FieldDescriptorKind::ClassProtocol ||
            Kind == FieldDescriptorKind::ObjCProtocol);
  }

  bool isStruct() const {
    return Kind == FieldDescriptorKind::Struct;
  }

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

  llvm::ArrayRef<FieldRecord> getFields() const {
    return {getFieldRecordBuffer(), NumFields};
  }

  bool hasMangledTypeName() const {
    return MangledTypeName;
  }

  StringRef getMangledTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char *)((uintptr_t)MangledTypeName.get() + Offset));
  }

  bool hasSuperclass() const {
    return Superclass;
  }

  StringRef getSuperclass(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)Superclass.get() + Offset));
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

  const FieldDescriptor *operator->() const {
    return reinterpret_cast<const FieldDescriptor *>(Cur);
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

// Associated type records describe the mapping from an associated
// type to the type witness of a conformance.
class AssociatedTypeRecord {
  const RelativeDirectPointer<const char> Name;
  const RelativeDirectPointer<const char> SubstitutedTypeName;

public:
  StringRef getName(uintptr_t Offset) const {
    return (const char*)((uintptr_t)Name.get() + Offset);
  }

  StringRef getMangledSubstitutedTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)SubstitutedTypeName.get() + Offset));
  }
};

struct AssociatedTypeRecordIterator {
  const AssociatedTypeRecord *Cur;
  const AssociatedTypeRecord * const End;

  AssociatedTypeRecordIterator()
    : Cur(nullptr), End(nullptr) {}

  AssociatedTypeRecordIterator(const AssociatedTypeRecord *Cur,
                               const AssociatedTypeRecord * const End)
    : Cur(Cur), End(End) {}

  const AssociatedTypeRecord &operator*() const {
    return *Cur;
  }

  const AssociatedTypeRecord *operator->() const {
    return Cur;
  }

  AssociatedTypeRecordIterator &operator++() {
    ++Cur;
    return *this;
  }

  AssociatedTypeRecordIterator
  operator=(const AssociatedTypeRecordIterator &Other) {
    return { Other.Cur, Other.End };
  }

  bool operator==(const AssociatedTypeRecordIterator &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(const AssociatedTypeRecordIterator &other) const {
    return !(*this == other);
  }

  operator bool() const {
    return Cur && End;
  }
};

// An associated type descriptor contains a collection of associated
// type records for a conformance.
struct AssociatedTypeDescriptor {
private:
  const RelativeDirectPointer<const char> ConformingTypeName;
  const RelativeDirectPointer<const char> ProtocolTypeName;
public:

  uint32_t NumAssociatedTypes;
  uint32_t AssociatedTypeRecordSize;

  const AssociatedTypeRecord *getAssociatedTypeRecordBuffer() const {
    return reinterpret_cast<const AssociatedTypeRecord *>(this + 1);
  }

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

  StringRef getMangledProtocolTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)ProtocolTypeName.get() + Offset));
  }

  StringRef getMangledConformingTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)ConformingTypeName.get() + Offset));
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

  const AssociatedTypeDescriptor *operator->() const {
    return reinterpret_cast<const AssociatedTypeDescriptor *>(Cur);
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

// Builtin type records describe basic layout information about
// any builtin types referenced from the other sections.
class BuiltinTypeDescriptor {
  const RelativeDirectPointer<const char> TypeName;

public:
  uint32_t Size;

  // - Least significant 16 bits are the alignment.
  // - Bit 16 is 'bitwise takable'.
  // - Remaining bits are reserved.
  uint32_t AlignmentAndFlags;

  uint32_t Stride;
  uint32_t NumExtraInhabitants;

  bool isBitwiseTakable() const {
    return (AlignmentAndFlags >> 16) & 1;
  }

  uint32_t getAlignment() const {
    return AlignmentAndFlags & 0xffff;
  }

  bool hasMangledTypeName() const {
    return TypeName;
  }

  StringRef getMangledTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)TypeName.get() + Offset));
  }
};

class BuiltinTypeDescriptorIterator
  : public std::iterator<std::forward_iterator_tag, BuiltinTypeDescriptor> {
public:
  const void *Cur;
  const void * const End;
  BuiltinTypeDescriptorIterator(const void *Cur, const void * const End)
    : Cur(Cur), End(End) {}

  const BuiltinTypeDescriptor &operator*() const {
    return *reinterpret_cast<const BuiltinTypeDescriptor *>(Cur);
  }

  const BuiltinTypeDescriptor *operator->() const {
    return reinterpret_cast<const BuiltinTypeDescriptor *>(Cur);;
  }

  BuiltinTypeDescriptorIterator &operator++() {
    const void *Next = reinterpret_cast<const char *>(Cur)
      + sizeof(BuiltinTypeDescriptor);
    Cur = Next;
    return *this;
  }

  bool operator==(BuiltinTypeDescriptorIterator const &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(BuiltinTypeDescriptorIterator const &other) const {
    return !(*this == other);
  }
};

class CaptureTypeRecord {
  const RelativeDirectPointer<const char> MangledTypeName;

public:
  CaptureTypeRecord() = delete;

  bool hasMangledTypeName() const {
    return MangledTypeName;
  }

  StringRef getMangledTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)MangledTypeName.get() + Offset));
  }
};

struct CaptureTypeRecordIterator {
  const CaptureTypeRecord *Cur;
  const CaptureTypeRecord * const End;

  CaptureTypeRecordIterator(const CaptureTypeRecord *Cur,
                            const CaptureTypeRecord * const End)
    : Cur(Cur), End(End) {}

  const CaptureTypeRecord &operator*() const {
    return *Cur;
  }

  const CaptureTypeRecord *operator->() const {
    return Cur;
  }

  CaptureTypeRecordIterator &operator++() {
    ++Cur;
    return *this;
  }

  bool operator==(const CaptureTypeRecordIterator &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(const CaptureTypeRecordIterator &other) const {
    return !(*this == other);
  }
};

class MetadataSourceRecord {
  const RelativeDirectPointer<const char> MangledTypeName;
  const RelativeDirectPointer<const char> MangledMetadataSource;

public:
  MetadataSourceRecord() = delete;

  bool hasMangledTypeName() const {
    return MangledTypeName;
  }

  StringRef getMangledTypeName(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)MangledTypeName.get() + Offset));
  }

  bool hasMangledMetadataSource() const {
    return MangledMetadataSource;
  }

  StringRef getMangledMetadataSource(uintptr_t Offset) const {
    return Demangle::makeSymbolicMangledNameStringRef(
      (const char*)((uintptr_t)MangledMetadataSource.get() + Offset));
  }
};

struct MetadataSourceRecordIterator {
  const MetadataSourceRecord *Cur;
  const MetadataSourceRecord * const End;

  MetadataSourceRecordIterator(const MetadataSourceRecord *Cur,
                            const MetadataSourceRecord * const End)
    : Cur(Cur), End(End) {}

  const MetadataSourceRecord &operator*() const {
    return *Cur;
  }

  const MetadataSourceRecord *operator->() const {
    return Cur;
  }

  MetadataSourceRecordIterator &operator++() {
    ++Cur;
    return *this;
  }

  bool operator==(const MetadataSourceRecordIterator &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(const MetadataSourceRecordIterator &other) const {
    return !(*this == other);
  }
};

// Capture descriptors describe the layout of a closure context
// object. Unlike nominal types, the generic substitutions for a
// closure context come from the object, and not the metadata.
class CaptureDescriptor {
  const CaptureTypeRecord *getCaptureTypeRecordBuffer() const {
    return reinterpret_cast<const CaptureTypeRecord *>(this + 1);
  }

  const MetadataSourceRecord *getMetadataSourceRecordBuffer() const {
    return reinterpret_cast<const MetadataSourceRecord *>(capture_end().End);
  }

public:
  /// The number of captures in the closure and the number of typerefs that
  /// immediately follow this struct.
  uint32_t NumCaptureTypes;

  /// The number of sources of metadata available in the MetadataSourceMap
  /// directly following the list of capture's typerefs.
  uint32_t NumMetadataSources;

  /// The number of items in the NecessaryBindings structure at the head of
  /// the closure.
  uint32_t NumBindings;

  using const_iterator = FieldRecordIterator;

  CaptureTypeRecordIterator capture_begin() const {
    auto Begin = getCaptureTypeRecordBuffer();
    auto End = Begin + NumCaptureTypes;
    return { Begin, End };
  }

  CaptureTypeRecordIterator capture_end() const {
    auto Begin = getCaptureTypeRecordBuffer();
    auto End = Begin + NumCaptureTypes;
    return { End, End };
  }

  MetadataSourceRecordIterator source_begin() const {
    auto Begin = getMetadataSourceRecordBuffer();
    auto End = Begin + NumMetadataSources;
    return { Begin, End };
  }

  MetadataSourceRecordIterator source_end() const {
    auto Begin = getMetadataSourceRecordBuffer();
    auto End = Begin + NumMetadataSources;
    return { End, End };
  }
};

class CaptureDescriptorIterator
  : public std::iterator<std::forward_iterator_tag, CaptureDescriptor> {
public:
  const void *Cur;
  const void * const End;
  CaptureDescriptorIterator(const void *Cur, const void * const End)
    : Cur(Cur), End(End) {}

  const CaptureDescriptor &operator*() const {
    return *reinterpret_cast<const CaptureDescriptor *>(Cur);
  }

  const CaptureDescriptor *operator->() const {
    return reinterpret_cast<const CaptureDescriptor *>(Cur);
  }

  CaptureDescriptorIterator &operator++() {
    const auto &CR = this->operator*();
    const void *Next = reinterpret_cast<const char *>(Cur)
      + sizeof(CaptureDescriptor)
      + CR.NumCaptureTypes * sizeof(CaptureTypeRecord)
      + CR.NumMetadataSources * sizeof(MetadataSourceRecord);
    Cur = Next;
    return *this;
  }

  bool operator==(CaptureDescriptorIterator const &other) const {
    return Cur == other.Cur && End == other.End;
  }

  bool operator!=(CaptureDescriptorIterator const &other) const {
    return !(*this == other);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_RECORDS_H
