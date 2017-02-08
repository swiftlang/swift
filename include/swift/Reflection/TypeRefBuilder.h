//===--- TypeRefBuilder.h - Swift Type Reference Builder --------*- C++ -*-===//
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
// Implements utilities for constructing TypeRefs and looking up field and
// enum case types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TYPEREFBUILDER_H
#define SWIFT_REFLECTION_TYPEREFBUILDER_H

#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/MetadataSourceBuilder.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "llvm/ADT/Optional.h"

#include <iostream>
#include <vector>
#include <unordered_map>

class NodePointer;

namespace swift {
namespace reflection {

template <typename Iterator>
class ReflectionSection {
  using const_iterator = Iterator;
  const void * Begin;
  const void * End;

public:
  ReflectionSection(const void * Begin,
                    const void * End)
  : Begin(Begin), End(End) {}

  ReflectionSection(uint64_t Begin, uint64_t End)
  : Begin(reinterpret_cast<const void *>(Begin)),
    End(reinterpret_cast<const void *>(End)) {}

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

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  FieldSection fieldmd;
  AssociatedTypeSection assocty;
  BuiltinTypeSection builtin;
  CaptureSection capture;
  GenericSection typeref;
  GenericSection reflstr;
  uintptr_t LocalStartAddress;
  uintptr_t RemoteStartAddress;
};

struct ClosureContextInfo {
  std::vector<const TypeRef *> CaptureTypes;
  std::vector<std::pair<const TypeRef *, const MetadataSource *>> MetadataSources;
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

/// An implementation of MetadataReader's BuilderType concept for
/// building TypeRefs, and parsing field metadata from any images
/// it has been made aware of.
///
/// Note that the TypeRefBuilder owns the memory for all TypeRefs
/// it vends.
class TypeRefBuilder {
#define TYPEREF(Id, Parent) friend class Id##TypeRef;
#include "swift/Reflection/TypeRefs.def"

public:
  using BuiltType = const TypeRef *;
  using BuiltNominalTypeDecl = Optional<std::string>;

  TypeRefBuilder();

  TypeRefBuilder(const TypeRefBuilder &other) = delete;
  TypeRefBuilder &operator=(const TypeRefBuilder &other) = delete;

private:
  /// Makes sure dynamically allocated TypeRefs stick around for the life of
  /// this TypeRefBuilder and are automatically released.
  std::vector<std::unique_ptr<const TypeRef>> TypeRefPool;

  /// Cache for associated type lookups.
  std::unordered_map<TypeRefID, const TypeRef *,
                     TypeRefID::Hash, TypeRefID::Equal> AssociatedTypeCache;

  TypeConverter TC;
  MetadataSourceBuilder MSB;

#define TYPEREF(Id, Parent) \
  std::unordered_map<TypeRefID, const Id##TypeRef *, \
                     TypeRefID::Hash, TypeRefID::Equal> Id##TypeRefs;
#include "swift/Reflection/TypeRefs.def"

public:
  template <typename TypeRefTy, typename... Args>
  const TypeRefTy *makeTypeRef(Args... args) {
    const auto TR = new TypeRefTy(::std::forward<Args>(args)...);
    TypeRefPool.push_back(std::unique_ptr<const TypeRef>(TR));
    return TR;
  }

  ///
  /// Factory methods for all TypeRef kinds
  ///

  const BuiltinTypeRef *createBuiltinType(const std::string &mangledName) {
    return BuiltinTypeRef::create(*this, mangledName);
  }

  Optional<std::string>
  createNominalTypeDecl(const Demangle::NodePointer &node) {
    return Demangle::mangleNode(node);
  }

  Optional<std::string> createNominalTypeDecl(std::string &&mangledName) {
    return std::move(mangledName);
  }

  const NominalTypeRef *createNominalType(
                                    const Optional<std::string> &mangledName,
                                    const TypeRef *parent) {
    return NominalTypeRef::create(*this, *mangledName, parent);
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const Optional<std::string> &mangledName,
                         const std::vector<const TypeRef *> &args,
                         const TypeRef *parent) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, parent);
  }

  const TupleTypeRef *
  createTupleType(const std::vector<const TypeRef *> &elements,
                  std::string &&labels, bool isVariadic) {
    // FIXME: Add uniqueness checks in TupleTypeRef::Profile and
    // unittests/Reflection/TypeRef.cpp if using labels for identity.
    return TupleTypeRef::create(*this, elements, isVariadic);
  }

  const FunctionTypeRef *
  createFunctionType(const std::vector<const TypeRef *> &args,
                     const std::vector<bool> &inOutArgs,
                     const TypeRef *result,
                     FunctionTypeFlags flags) {
    // FIXME: don't ignore inOutArgs
    // and add test to unittests/Reflection/TypeRef.cpp
    return FunctionTypeRef::create(*this, args, result, flags);
  }

  const ProtocolTypeRef *createProtocolType(const std::string &mangledName,
                                            const std::string &moduleName,
                                            const std::string &name) {
    return ProtocolTypeRef::create(*this, mangledName);
  }

  const ProtocolCompositionTypeRef *
  createProtocolCompositionType(const std::vector<const TypeRef*> &protocols) {
    for (auto protocol : protocols) {
      if (!isa<ProtocolTypeRef>(protocol))
        return nullptr;
    }
    return ProtocolCompositionTypeRef::create(*this, protocols);
  }

  const ExistentialMetatypeTypeRef *
  createExistentialMetatypeType(const TypeRef *instance) {
    return ExistentialMetatypeTypeRef::create(*this, instance);
  }

  const MetatypeTypeRef *createMetatypeType(const TypeRef *instance,
                                            bool WasAbstract = false) {
    return MetatypeTypeRef::create(*this, instance, WasAbstract);
  }

  const GenericTypeParameterTypeRef *
  createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParameterTypeRef::create(*this, depth, index);
  }

  const DependentMemberTypeRef *
  createDependentMemberType(const std::string &member,
                            const TypeRef *base,
                            const TypeRef *protocol) {
    if (!isa<ProtocolTypeRef>(protocol))
      return nullptr;
    return DependentMemberTypeRef::create(*this, member, base, protocol);
  }

  const UnownedStorageTypeRef *createUnownedStorageType(const TypeRef *base) {
    return UnownedStorageTypeRef::create(*this, base);
  }

  const UnmanagedStorageTypeRef *
  createUnmanagedStorageType(const TypeRef *base) {
    return UnmanagedStorageTypeRef::create(*this, base);
  }

  const WeakStorageTypeRef *createWeakStorageType(const TypeRef *base) {
    return WeakStorageTypeRef::create(*this, base);
  }

  const SILBoxTypeRef *createSILBoxType(const TypeRef *base) {
    return SILBoxTypeRef::create(*this, base);
  }

  const ObjCClassTypeRef *
  createObjCClassType(const std::string &mangledName) {
    return ObjCClassTypeRef::create(*this, mangledName);
  }

  const ObjCClassTypeRef *getUnnamedObjCClassType() {
    return createObjCClassType("");
  }

  const ForeignClassTypeRef *
  createForeignClassType(const std::string &mangledName) {
    return ForeignClassTypeRef::create(*this, mangledName);
  }

  const ForeignClassTypeRef *
  getUnnamedForeignClassType() {
    return createForeignClassType("");
  }

  const OpaqueTypeRef *getOpaqueType() {
    return OpaqueTypeRef::get();
  }

  ///
  /// Parsing reflection metadata
  ///

  void addReflectionInfo(ReflectionInfo I) {
    ReflectionInfos.push_back(I);
  }

private:
  std::vector<ReflectionInfo> ReflectionInfos;

public:
  TypeConverter &getTypeConverter() { return TC; }

  const TypeRef *
  lookupTypeWitness(const std::string &MangledTypeName,
                    const std::string &Member,
                    const TypeRef *Protocol);

  const TypeRef *
  lookupSuperclass(const std::string &MangledTypeName);

  const TypeRef *
  lookupSuperclass(const TypeRef *TR);

  /// Load unsubstituted field types for a nominal type.
  const FieldDescriptor *getFieldTypeInfo(const TypeRef *TR);

  /// Get the parsed and substituted field types for a nominal type.
  std::vector<FieldTypeInfo>
  getFieldTypeRefs(const TypeRef *TR, const FieldDescriptor *FD);

  /// Get the primitive type lowering for a builtin type.
  const BuiltinTypeDescriptor *getBuiltinTypeInfo(const TypeRef *TR);

  /// Get the raw capture descriptor for a remote capture descriptor
  /// address.
  const CaptureDescriptor *getCaptureDescriptor(uintptr_t RemoteAddress);

  /// Get the unsubstituted capture types for a closure context.
  ClosureContextInfo getClosureContextInfo(const CaptureDescriptor &CD);

  ///
  /// Dumping typerefs, field declarations, associated types
  ///

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false);
  void dumpFieldSection(std::ostream &OS);
  void dumpAssociatedTypeSection(std::ostream &OS);
  void dumpBuiltinTypeSection(std::ostream &OS);
  void dumpCaptureSection(std::ostream &OS);
  void dumpAllSections(std::ostream &OS);
};


} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREFBUILDER_H
