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

template <typename Runtime> class ReflectionContext;

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
  const void *startAddress() const {
    return Begin;
  }
  
  const void *endAddress() const {
    return End;
  }

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }

  size_t size() const {
    return (const char *)End - (const char *)Begin;
  }
};

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  struct {
    FieldSection Metadata;
    uintptr_t SectionOffset;
  } Field;

  struct {
    AssociatedTypeSection Metadata;
    uintptr_t SectionOffset;
  } AssociatedType;

  struct {
    BuiltinTypeSection Metadata;
    uintptr_t SectionOffset;
  } Builtin;

  struct {
    CaptureSection Metadata;
    uintptr_t SectionOffset;
  } Capture;

  struct {
    GenericSection Metadata;
    uintptr_t SectionOffset;
  } TypeReference;

  struct {
    GenericSection Metadata;
    uintptr_t SectionOffset;
  } ReflectionString;

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
  using BuiltProtocolDecl = Optional<std::pair<std::string, bool /*isObjC*/>>;

  TypeRefBuilder();

  TypeRefBuilder(const TypeRefBuilder &other) = delete;
  TypeRefBuilder &operator=(const TypeRefBuilder &other) = delete;

private:
  Demangle::Demangler Dem;

  /// Makes sure dynamically allocated TypeRefs stick around for the life of
  /// this TypeRefBuilder and are automatically released.
  std::vector<std::unique_ptr<const TypeRef>> TypeRefPool;

  /// Cache for associated type lookups.
  std::unordered_map<TypeRefID, const TypeRef *,
                     TypeRefID::Hash, TypeRefID::Equal> AssociatedTypeCache;

  /// Cache for field info lookups.
  std::unordered_map<std::string,
                     std::pair<const FieldDescriptor *, const ReflectionInfo *>>
                     FieldTypeInfoCache;

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

  Demangle::NodeFactory &getNodeFactory() { return Dem; }

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

  BuiltProtocolDecl
  createProtocolDecl(const Demangle::NodePointer &node) {
    return std::make_pair(Demangle::mangleNode(node), false);
  }

  BuiltProtocolDecl
  createObjCProtocolDecl(std::string &&name) {
    return std::make_pair(name, true);
  }

  Optional<std::string> createNominalTypeDecl(std::string &&mangledName) {
    return std::move(mangledName);
  }
  
  const NominalTypeRef *createNominalType(
                                    const Optional<std::string> &mangledName) {
    return NominalTypeRef::create(*this, *mangledName, nullptr);
  }

  const NominalTypeRef *createNominalType(
                                    const Optional<std::string> &mangledName,
                                    const TypeRef *parent) {
    return NominalTypeRef::create(*this, *mangledName, parent);
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const Optional<std::string> &mangledName,
                         const std::vector<const TypeRef *> &args) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, nullptr);
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

  const FunctionTypeRef *createFunctionType(
      const std::vector<remote::FunctionParam<const TypeRef *>> &params,
      const TypeRef *result, FunctionTypeFlags flags) {
    return FunctionTypeRef::create(*this, params, result, flags);
  }

  const ProtocolCompositionTypeRef *
  createProtocolCompositionType(ArrayRef<BuiltProtocolDecl> protocols,
                                BuiltType superclass,
                                bool isClassBound) {
    std::vector<const TypeRef *> protocolRefs;
    for (const auto &protocol : protocols) {
      if (!protocol)
        continue;

      if (protocol->second)
        protocolRefs.push_back(createObjCProtocolType(protocol->first));
      else
        protocolRefs.push_back(createNominalType(protocol->first));
    }

    return ProtocolCompositionTypeRef::create(*this, protocolRefs, superclass,
                                              isClassBound);
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
                            BuiltProtocolDecl protocol) {
    // Objective-C protocols don't have dependent types.
    if (protocol->second)
      return nullptr;
    return DependentMemberTypeRef::create(*this, member, base,
                                          protocol->first);
  }

#define REF_STORAGE(Name, ...) \
  const Name##StorageTypeRef *create##Name##StorageType(const TypeRef *base) { \
    return Name##StorageTypeRef::create(*this, base); \
  }
#include "swift/AST/ReferenceStorage.def"

  const SILBoxTypeRef *createSILBoxType(const TypeRef *base) {
    return SILBoxTypeRef::create(*this, base);
  }

  const ObjCClassTypeRef *getUnnamedObjCClassType() {
    return createObjCClassType("");
  }

  const ObjCClassTypeRef *
  createObjCClassType(const std::string &name) {
    return ObjCClassTypeRef::create(*this, name);
  }

  const ObjCProtocolTypeRef *
  createObjCProtocolType(const std::string &name) {
    return ObjCProtocolTypeRef::create(*this, name);
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
  
  const std::vector<ReflectionInfo> &getReflectionInfos() {
    return ReflectionInfos;
  }

private:
  std::vector<ReflectionInfo> ReflectionInfos;
  
  uint64_t getRemoteAddrOfTypeRefPointer(const void *pointer);

public:
  template<typename Runtime>
  void setSymbolicReferenceResolverReader(
                      remote::MetadataReader<Runtime, TypeRefBuilder> &reader) {
    // Have the TypeRefBuilder demangle symbolic references by reading their
    // demangling out of the referenced context descriptors in the target
    // process.
    Dem.setSymbolicReferenceResolver(
    [this, &reader](SymbolicReferenceKind kind,
                    Directness directness,
                    int32_t offset, const void *base) -> Demangle::NodePointer {
      // Resolve the reference to a remote address.
      auto remoteAddress = getRemoteAddrOfTypeRefPointer(base);
      if (remoteAddress == 0)
        return nullptr;
      
      auto address = remoteAddress + offset;
      if (directness == Directness::Indirect) {
        if (auto indirectAddress = reader.readPointerValue(address)) {
          address = *indirectAddress;
        } else {
          return nullptr;
        }
      }
      
      switch (kind) {
      case Demangle::SymbolicReferenceKind::Context:
        return reader.readDemanglingForContextDescriptor(address, Dem);
      }
      
      return nullptr;
    });
  }

  TypeConverter &getTypeConverter() { return TC; }

  const TypeRef *
  lookupTypeWitness(const std::string &MangledTypeName,
                    const std::string &Member,
                    StringRef Protocol);

  const TypeRef *
  lookupSuperclass(const TypeRef *TR);

  /// Load unsubstituted field types for a nominal type.
  std::pair<const FieldDescriptor *, const ReflectionInfo *>
  getFieldTypeInfo(const TypeRef *TR);

  /// Get the parsed and substituted field types for a nominal type.
  bool getFieldTypeRefs(const TypeRef *TR,
           const std::pair<const FieldDescriptor *, const ReflectionInfo *> &FD,
           std::vector<FieldTypeInfo> &Fields);

  /// Get the primitive type lowering for a builtin type.
  const BuiltinTypeDescriptor *getBuiltinTypeInfo(const TypeRef *TR);

  /// Get the raw capture descriptor for a remote capture descriptor
  /// address.
  const CaptureDescriptor *getCaptureDescriptor(uintptr_t RemoteAddress);

  /// Get the unsubstituted capture types for a closure context.
  ClosureContextInfo getClosureContextInfo(const CaptureDescriptor &CD,
                                           uintptr_t Offset);

  ///
  /// Dumping typerefs, field declarations, associated types
  ///

  void dumpTypeRef(llvm::StringRef MangledName,
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
