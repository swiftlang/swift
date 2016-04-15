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

/// An implementation of MetadataReader's BuilderType concept for
/// building TypeRefs.
class TypeRefBuilder {
public:
  using Type = const TypeRef *;

private:
  std::vector<std::unique_ptr<const TypeRef>> TypeRefPool;

public:
  template <typename TypeRefTy, typename... Args>
  TypeRefTy *make_typeref(Args... args) {
    auto TR = new TypeRefTy(::std::forward<Args>(args)...);
    TypeRefPool.push_back(std::unique_ptr<const TypeRef>(TR));
    return TR;
  }

  const BuiltinTypeRef *createBuiltinType(const std::string &mangledName) {
    return BuiltinTypeRef::create(*this, mangledName);
  }

  const NominalTypeRef *createNominalType(const std::string &mangledName,
                                    const TypeRef *parent) {
    return NominalTypeRef::create(*this, mangledName, parent);
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const std::string &mangledName,
                         const std::vector<const TypeRef *> &args,
                         const TypeRef *parent) {
    return BoundGenericTypeRef::create(*this, mangledName, args, parent);
  }

  const TupleTypeRef *
  createTupleType(const std::vector<const TypeRef *> &elements,
                  bool isVariadic) {
    return TupleTypeRef::create(*this, elements, isVariadic);
  }

  const FunctionTypeRef *
  createFunctionType(const std::vector<const TypeRef *> &args,
                     const std::vector<bool> &inOutArgs,
                     const TypeRef *result,
                     FunctionTypeFlags flags) {
    // FIXME: don't ignore inOutArgs
    // FIXME: don't ignore flags
    return FunctionTypeRef::create(*this, args, result);
  }

  const ProtocolTypeRef *createProtocolType(const std::string &moduleName,
                                      const std::string &protocolName) {
    return ProtocolTypeRef::create(*this, moduleName, protocolName);
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

  const MetatypeTypeRef *createMetatypeType(const TypeRef *instance) {
    return MetatypeTypeRef::create(*this, instance);
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

  const ObjCClassTypeRef *getUnnamedObjCClassType() {
    return ObjCClassTypeRef::getUnnamed();
  }

  const ForeignClassTypeRef *getUnnamedForeignClassType() {
    return ForeignClassTypeRef::getUnnamed();
  }

  const OpaqueTypeRef *getOpaqueType() {
    return OpaqueTypeRef::get();
  }
};

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  std::string ImageName;
  FieldSection fieldmd;
  AssociatedTypeSection assocty;
  BuiltinTypeSection builtin;
  GenericSection typeref;
  GenericSection reflstr;
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
    OS << TypeName << '\n';

    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = decodeMangledType(DemangleTree);
    if (!TR) {
      OS << "!!! Invalid typeref: " << MangledName << '\n';
      return;
    }
    TR->dump(OS);
    OS << '\n';
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
        OS << TypeName << '\n';
        for (size_t i = 0; i < TypeName.size(); ++i)
          OS << '-';
        OS << '\n';
        for (auto &field : descriptor) {
          OS << field.getFieldName();
          if (field.hasMangledTypeName()) {
            OS << ": ";
            dumpTypeRef(field.getMangledTypeName(), OS);
          } else {
            OS << "\n\n";
          }
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

        OS << "- " << conformingTypeName << " : " << protocolName;
        OS << '\n';

        for (const auto &associatedType : descriptor) {
          OS << "typealias " << associatedType.getName() << " = ";
          dumpTypeRef(associatedType.getMangledSubstitutedTypeName(), OS);
        }
      }
    }
  }

  void dumpBuiltinTypeSection(std::ostream &OS) {
    for (const auto &sections : ReflectionInfos) {
      for (const auto &descriptor : sections.builtin) {
        auto typeName = Demangle::demangleTypeAsString(
          descriptor.getMangledTypeName());

        OS << "\n- " << typeName << ":\n";
        OS << "Size: " << descriptor.Size << "\n";
        OS << "Alignment: " << descriptor.Alignment << "\n";
        OS << "Stride: " << descriptor.Stride << "\n";
        OS << "NumExtraInhabitants: " << descriptor.NumExtraInhabitants << "\n";
      }
    }
  }

  void dumpAllSections(std::ostream &OS) {
    OS << "FIELDS:\n";
    OS << "=======\n";
    dumpFieldSection(OS);
    OS << '\n';
    OS << "ASSOCIATED TYPES:\n";
    OS << "=================\n";
    dumpAssociatedTypeSection(OS);
    OS << '\n';
    OS << "BUILTIN TYPES:\n";
    OS << "==============\n";
    dumpBuiltinTypeSection(OS);
    OS << '\n';
  }

  const TypeRef *
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

  std::vector<std::pair<std::string, const TypeRef *>>
  getFieldTypeRefs(const TypeRef *TR) {
    std::string MangledName;
    if (auto N = dyn_cast<NominalTypeRef>(TR))
      MangledName = N->getMangledName();
    else if (auto BG = dyn_cast<BoundGenericTypeRef>(TR))
      MangledName = BG->getMangledName();
    else
      return {};

    auto Subs = TR->getSubstMap();

    std::vector<std::pair<std::string, const TypeRef *>> Fields;
    for (auto Info : ReflectionInfos) {
      for (auto &FieldDescriptor : Info.fieldmd) {
        auto CandidateMangledName = FieldDescriptor.MangledTypeName.get();
        if (!CandidateMangledName)
          continue;
        if (MangledName.compare(CandidateMangledName) != 0)
          continue;
        for (auto &Field : FieldDescriptor) {
          auto FieldName = Field.getFieldName();

          // Empty cases of enums do not have a type
          if (!Field.hasMangledTypeName()) {
            Fields.push_back({FieldName, nullptr});
            continue;
          }

          auto Demangled
            = Demangle::demangleTypeAsNode(Field.getMangledTypeName());
          auto Unsubstituted = decodeMangledType(Demangled);
          if (!Unsubstituted)
            return {};

          auto Substituted = Unsubstituted->subst(*this, Subs);
          if (FieldName.empty())
            FieldName = "<Redacted Field Name>";
          Fields.push_back({FieldName, Substituted});
        }
      }
    }
    return Fields;
  }

  std::vector<std::pair<std::string, const TypeRef *>>
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
