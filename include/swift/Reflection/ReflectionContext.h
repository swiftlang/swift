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

#include "swift/Remote/MemoryReader.h"
#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/SwiftRemoteMirror/SwiftRemoteMirrorTypes.h"

#include <iostream>
#include <vector>
#include <unordered_map>

class NodePointer;

namespace swift {
namespace reflection {

using swift::remote::MemoryReader;
using swift::remote::RemoteAddress;

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

/// An implementation of MetadataReader's BuilderType concept for
/// building TypeRefs.
class TypeRefBuilder {
public:
  using Type = TypeRef*;

private:
  std::vector<std::unique_ptr<TypeRef>> TypeRefPool;

public:
  template <typename TypeRefTy, typename... Args>
  TypeRefTy *make_typeref(Args... args) {
    auto TR = new TypeRefTy(::std::forward<Args>(args)...);
    TypeRefPool.push_back(std::unique_ptr<TypeRef>(TR));
    return TR;
  }

  BuiltinTypeRef *createBuiltinType(const std::string &mangledName) {
    return BuiltinTypeRef::create(*this, mangledName);
  }

  NominalTypeRef *createNominalType(const std::string &mangledName,
                                    TypeRef *parent) {
    return NominalTypeRef::create(*this, mangledName, parent);
  }

  BoundGenericTypeRef *createBoundGenericType(const std::string &mangledName,
                                        const std::vector<TypeRef *> &args,
                                              TypeRef *parent) {
    return BoundGenericTypeRef::create(*this, mangledName, args, parent);
  }

  TupleTypeRef *createTupleType(const std::vector<TypeRef *> &elements,
                                bool isVariadic) {
    return TupleTypeRef::create(*this, elements, isVariadic);
  }

  FunctionTypeRef *createFunctionType(const std::vector<TypeRef *> &args,
                                      const std::vector<bool> &inOutArgs,
                                      TypeRef *result,
                                      FunctionTypeFlags flags) {
    // FIXME: don't ignore inOutArgs
    // FIXME: don't ignore flags
    return FunctionTypeRef::create(*this, args, result);
  }

  ProtocolTypeRef *createProtocolType(const std::string &moduleName,
                                      const std::string &protocolName) {
    return ProtocolTypeRef::create(*this, moduleName, protocolName);
  }

  ProtocolCompositionTypeRef *
  createProtocolCompositionType(const std::vector<TypeRef*> &protocols) {
    for (auto protocol : protocols) {
      if (!isa<ProtocolTypeRef>(protocol))
        return nullptr;
    }
    return ProtocolCompositionTypeRef::create(*this, protocols);
  }

  ExistentialMetatypeTypeRef *
  createExistentialMetatypeType(TypeRef *instance) {
    return ExistentialMetatypeTypeRef::create(*this, instance);
  }

  MetatypeTypeRef *createMetatypeType(TypeRef *instance) {
    return MetatypeTypeRef::create(*this, instance);
  }

  GenericTypeParameterTypeRef *
  createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParameterTypeRef::create(*this, depth, index);
  }

  DependentMemberTypeRef *createDependentMemberType(const std::string &member,
                                                    TypeRef *base,
                                                    TypeRef *protocol) {
    if (!isa<ProtocolTypeRef>(protocol))
      return nullptr;
    return DependentMemberTypeRef::create(*this, member, base, protocol);
  }

  UnownedStorageTypeRef *createUnownedStorageType(TypeRef *base) {
    return UnownedStorageTypeRef::create(*this, base);
  }

  UnmanagedStorageTypeRef *createUnmanagedStorageType(TypeRef *base) {
    return UnmanagedStorageTypeRef::create(*this, base);
  }

  WeakStorageTypeRef *createWeakStorageType(TypeRef *base) {
    return WeakStorageTypeRef::create(*this, base);
  }

  ObjCClassTypeRef *getUnnamedObjCClassType() {
    return ObjCClassTypeRef::getUnnamed();
  }

  ForeignClassTypeRef *getUnnamedForeignClassType() {
    return ForeignClassTypeRef::getUnnamed();
  }

  OpaqueTypeRef *getOpaqueType() {
    return OpaqueTypeRef::get();
  }
};

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  std::string ImageName;
  FieldSection fieldmd;
  AssociatedTypeSection assocty;
  GenericSection reflstr;
  GenericSection typeref;
};



template <typename Runtime>
class ReflectionContext
       : public remote::MetadataReader<Runtime, TypeRefBuilder> {
  using super = remote::MetadataReader<Runtime, TypeRefBuilder>;

public:
  using super::decodeMangledType;
  using super::readTypeFromMetadata;
  using typename super::StoredPointer;

private:

  std::vector<ReflectionInfo> ReflectionInfos;

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false) {
    auto TypeName = Demangle::demangleTypeAsString(MangledName);
    OS << TypeName << std::endl;

    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = decodeMangledType(DemangleTree);
    if (!TR) {
      OS << "!!! Invalid typeref: " << MangledName << std::endl;
      return;
    }
    TR->dump(OS);
    OS << std::endl;
  }

  const AssociatedTypeDescriptor *
  lookupAssociatedTypes(const std::string &MangledTypeName,
                        const DependentMemberTypeRef *DependentMember) {
    // Cache missed - we need to look through all of the assocty sections
    // for all images that we've been notified about.
    for (auto &Info : ReflectionInfos) {
      for (const auto &AssocTyDescriptor : Info.assocty) {
        std::string ConformingTypeName(AssocTyDescriptor.ConformingTypeName);
        if (ConformingTypeName.compare(MangledTypeName) != 0)
          continue;
        std::string ProtocolMangledName(AssocTyDescriptor.ProtocolTypeName);
        auto DemangledProto = Demangle::demangleTypeAsNode(ProtocolMangledName);
        auto TR = decodeMangledType(DemangledProto);

        auto &Conformance = *DependentMember->getProtocol();
        if (auto Protocol = dyn_cast<ProtocolTypeRef>(TR)) {
          if (*Protocol != Conformance)
            continue;
          return &AssocTyDescriptor;
        }
      }
    }
    return nullptr;
  }

public:

  explicit ReflectionContext(std::shared_ptr<MemoryReader> reader)
    : super(std::move(reader)) {}

  ReflectionContext(const ReflectionContext&) = delete;

  MemoryReader &getReader() {
    return *this->Reader;
  }

  void dumpFieldSection(std::ostream &OS) {
    for (const auto &sections : ReflectionInfos) {
      for (const auto &descriptor : sections.fieldmd) {
        auto TypeName
          = Demangle::demangleTypeAsString(descriptor.getMangledTypeName());
        OS << TypeName << std::endl;
        for (size_t i = 0; i < TypeName.size(); ++i)
          OS << '-';
        OS << std::endl;
        for (auto &field : descriptor) {
          OS << field.getFieldName() << ": ";
          dumpTypeRef(field.getMangledTypeName(), OS);
        }
      }
    }
  }

  void dumpAssociatedTypeSection(std::ostream &OS) {
    for (const auto &sections : ReflectionInfos) {
      for (const auto &descriptor : sections.assocty) {
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

  void dumpAllSections(std::ostream &OS) {
    OS << "FIELDS:" << std::endl;
    for (size_t i = 0; i < 7; ++i) OS << '=';
    OS << std::endl;
    dumpFieldSection(OS);
    OS << "\nASSOCIATED TYPES:" << std::endl;
    for (size_t i = 0; i < 17; ++i) OS << '=';
    OS << std::endl;
    dumpAssociatedTypeSection(OS);
    OS << std::endl;
  }

  TypeRef *
  getDependentMemberTypeRef(const std::string &MangledTypeName,
                            const DependentMemberTypeRef *DependentMember) {

    if (auto AssocTys = lookupAssociatedTypes(MangledTypeName, DependentMember)) {
      for (auto &AssocTy : *AssocTys) {
        if (DependentMember->getMember().compare(AssocTy.getName()) != 0)
          continue;

        auto SubstitutedTypeName = AssocTy.getMangledSubstitutedTypeName();
        auto Demangled = Demangle::demangleTypeAsNode(SubstitutedTypeName);
        return decodeMangledType(Demangled);
      }
    }
    return nullptr;
  }

  std::vector<std::pair<std::string, TypeRef *>>
  getFieldTypeRefs(TypeRef *TR) {
    std::string MangledName;
    if (auto N = dyn_cast<NominalTypeRef>(TR))
      MangledName = N->getMangledName();
    else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR))
      MangledName = BG->getMangledName();
    else
      return {};

    auto Subs = TR->getSubstMap();

    std::vector<std::pair<std::string, TypeRef *>> Fields;
    for (auto Info : ReflectionInfos) {
      for (auto &FieldDescriptor : Info.fieldmd) {
        auto CandidateMangledName = FieldDescriptor.MangledTypeName.get();
        if (!CandidateMangledName)
          continue;
        if (MangledName.compare(CandidateMangledName) != 0)
          continue;
        for (auto &Field : FieldDescriptor) {
          auto Demangled
          = Demangle::demangleTypeAsNode(Field.getMangledTypeName());
          auto Unsubstituted = decodeMangledType(Demangled);
          if (!Unsubstituted)
            return {};
          auto Substituted = Unsubstituted->subst(*this, Subs);
          auto FieldName = Field.getFieldName();
          if (FieldName.empty())
            FieldName = "<Redacted Field Name>";
          Fields.push_back({FieldName, Substituted});
        }
      }
    }
    return Fields;
  }

  std::vector<std::pair<std::string, TypeRef *>>
  getFieldTypeRefs(StoredPointer MetadataAddress) {
    auto TR = readTypeFromMetadata(MetadataAddress);
    return getFieldTypeRefs(TR);
  }

  void addReflectionInfo(ReflectionInfo I) {
    ReflectionInfos.push_back(I);
  }

  swift_typeinfo_t getInfoForTypeRef(const TypeRef *TR) {
    // TODO
    return {
      SWIFT_UNKNOWN,
      NULL,
      0,
      0,
      0
    };
  }

  swift_childinfo_t getInfoForChild(const TypeRef *TR, unsigned Index) {
    // TODO
    return {
      0,
      NULL,
      0,
    };
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
