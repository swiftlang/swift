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
#include "swift/Reflection/Reader.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeRef.h"

#include <iostream>
#include <vector>
#include <unordered_map>

class NodePointer;

namespace swift {
namespace reflection {

template <typename Runtime>
using SharedTargetMetadataRef = std::shared_ptr<TargetMetadata<Runtime>>;

template <typename Runtime>
using SharedTargetNominalTypeDescriptorRef = std::shared_ptr<TargetNominalTypeDescriptor<Runtime>>;

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
class ReflectionContext {
  using StoredPointer = typename Runtime::StoredPointer;

  std::vector<ReflectionInfo> ReflectionInfos;
  std::unordered_map<StoredPointer, TypeRefPointer> TypeRefCache;
  MemoryReader &Reader;

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false) const {
    auto TypeName = Demangle::demangleTypeAsString(MangledName);
    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = TypeRef::fromDemangleNode(DemangleTree);
    OS << TypeName << '\n';
    TR->dump(OS);
    std::cout << std::endl;
  }

  template <typename M>
  SharedTargetMetadataRef<Runtime> _readMetadata(StoredPointer Address) {
    auto Size = sizeof(M);
    uint8_t *Buffer = (uint8_t *)malloc(Size);
    if (!Reader.readBytes(Address, Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }

    auto Casted = reinterpret_cast<TargetMetadata<Runtime> *>(Buffer);
    return SharedTargetMetadataRef<Runtime>(Casted,
                                            [](void *Meta){
                                              free((void*)Meta);
                                            });
  }

public:
  ReflectionContext(MemoryReader &Reader) : Reader(Reader) {}

  void dumpFieldSection(std::ostream &OS) const {
    for (const auto &sections : ReflectionInfos) {
      for (const auto &descriptor : sections.fieldmd) {
        dumpTypeRef(descriptor.getMangledTypeName(), OS);
        for (auto &field : descriptor) {
          OS << field.getFieldName() << ": ";
          dumpTypeRef(field.getMangledTypeName(), OS);
        }
      }
    }
  }

  void dumpAssociatedTypeSection(std::ostream &OS) const {
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

  SharedTargetMetadataRef<Runtime> readMetadata(StoredPointer Address) {
    StoredPointer KindValue = 0;
    if (!Reader.readInteger(Address, &KindValue))
      return nullptr;

    auto Kind = static_cast<MetadataKind>(KindValue);

    if (metadataKindIsClass(Kind)) {
      return _readMetadata<TargetClassMetadata<Runtime>>(Address);
    } else {
      switch (Kind) {
      case MetadataKind::Enum:
        return _readMetadata<TargetEnumMetadata<Runtime>>(Address);
      case MetadataKind::ErrorObject:
        return _readMetadata<TargetEnumMetadata<Runtime>>(Address);
      case MetadataKind::Existential:
        return _readMetadata<TargetClassMetadata<Runtime>>(Address);
      case MetadataKind::ExistentialMetatype:
        return _readMetadata<TargetClassMetadata<Runtime>>(Address);
      case MetadataKind::ForeignClass:
        return _readMetadata<TargetForeignClassMetadata<Runtime>>(Address);
      case MetadataKind::Function:
        return _readMetadata<TargetFunctionTypeMetadata<Runtime>>(Address);
      case MetadataKind::HeapGenericLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata<Runtime>>(Address);
      case MetadataKind::HeapLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata<Runtime>>(Address);
      case MetadataKind::Metatype:
        return _readMetadata<TargetMetatypeMetadata<Runtime>>(Address);
      case MetadataKind::ObjCClassWrapper:
        return _readMetadata<TargetObjCClassWrapperMetadata<Runtime>>(Address);
      case MetadataKind::Opaque:
        return _readMetadata<TargetOpaqueMetadata<Runtime>>(Address);
      case MetadataKind::Optional:
        return _readMetadata<TargetEnumMetadata<Runtime>>(Address);
      case MetadataKind::Struct:
        return _readMetadata<TargetStructMetadata<Runtime>>(Address);
      case MetadataKind::Tuple:
        return _readMetadata<TargetTupleTypeMetadata<Runtime>>(Address);
      default:
        return nullptr;
      }
    }
  }

  template<typename Offset>
  StoredPointer resolveRelativeOffset(StoredPointer targetAddress) {
    Offset relative;
    if (!Reader.readInteger(targetAddress, &relative))
      return 0;
    using SignedOffset = typename std::make_signed<Offset>::type;
    using SignedPointer = typename std::make_signed<StoredPointer>::type;
    auto signext = (SignedPointer)(SignedOffset)relative;
    return targetAddress + signext;
  }

  SharedTargetNominalTypeDescriptorRef<Runtime>
  readNominalTypeDescriptor(StoredPointer Address) {
    auto Size = sizeof(TargetNominalTypeDescriptor<Runtime>);
    auto Buffer = (uint8_t *)malloc(Size);
    if (!Reader.readBytes(Address, Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }

    auto Casted
      = reinterpret_cast<TargetNominalTypeDescriptor<Runtime> *>(Buffer);
    return SharedTargetNominalTypeDescriptorRef<Runtime>(Casted,
                                            [](void *NTD){
                                              free(NTD);
                                            });
  }

  TypeRefVector
  getGenericSubstitutions(TypeRefPointer Unsubstituted,
                          StoredPointer MetadataAddress) {
    if (!Unsubstituted || !MetadataAddress)
      return {};

    if (!llvm::isa<BoundGenericTypeRef>(Unsubstituted.get()))
      return {};

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta)
      return {};

    TypeRefVector GenericArgTypeRefs;
    llvm_unreachable("todo");
    return GenericArgTypeRefs;
  }

  TypeRefPointer
  resolveDependentMembers(TypeRefPointer Unresolved,
                          StoredPointer MetadataAddress) {
    llvm_unreachable("todo");
    return Unresolved;
  }

  TypeRefPointer getTypeRef(StoredPointer MetadataAddress) {
    auto Cached = TypeRefCache.find(MetadataAddress);
    if (Cached != TypeRefCache.end())
      return Cached->second;

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta) return nullptr;

    switch (Meta->getKind()) {
    case MetadataKind::Class: {
      auto ClassMeta = llvm::cast<TargetClassMetadata<Runtime>>(Meta.get());
      if (ClassMeta->isPureObjC())
        return ObjCClassTypeRef::Unnamed;

      auto DescriptorAddress
        = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                         ClassMeta->offsetToDescriptorOffset());
      auto Descriptor = readNominalTypeDescriptor(DescriptorAddress);
      if (!Descriptor)
        return nullptr;

      auto NameAddress
        = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                         Descriptor->offsetToNameOffset());
      auto MangledName = Reader.readString(NameAddress);
      if (MangledName.empty())
        return nullptr;

      auto DemangleNode = Demangle::demangleTypeAsNode(MangledName);
      if (!DemangleNode)
        return nullptr;

      auto Unsubstituted = TypeRef::fromDemangleNode(DemangleNode);
      auto Substitutions = getGenericSubstitutions(Unsubstituted,
                                                   MetadataAddress);
      auto Substituted = Unsubstituted->substituteGenerics(Substitutions);
      auto Resolved = resolveDependentMembers(Substituted, MetadataAddress);
      TypeRefCache.insert({MetadataAddress, Resolved});
      return Resolved;
    }
    case MetadataKind::Struct: {
      auto StructMeta = cast<TargetStructMetadata<Runtime>>(Meta.get());

      auto DescriptorAddress
        = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                        StructMeta->offsetToDescriptorOffset());
      auto Descriptor = readNominalTypeDescriptor(DescriptorAddress);
      if (!Descriptor)
        return nullptr;

      auto NameAddress
        = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                         Descriptor->offsetToNameOffset());
      auto MangledName = Reader.readString(NameAddress);
      if (MangledName.empty())
        return nullptr;

      auto DemangleNode = Demangle::demangleTypeAsNode(MangledName);
      if (!DemangleNode)
        return nullptr;

      auto Unsubstituted = TypeRef::fromDemangleNode(DemangleNode);
      auto Substitutions = getGenericSubstitutions(Unsubstituted,
                                                   MetadataAddress);
      auto Substituted = Unsubstituted->substituteGenerics(Substitutions);
      auto Resolved = resolveDependentMembers(Substituted, MetadataAddress);
      TypeRefCache.insert({MetadataAddress, Resolved});
      return Resolved;
    }
    case MetadataKind::Enum:
    case MetadataKind::Optional: {
      auto EnumMeta = cast<TargetEnumMetadata<Runtime>>(Meta.get());
      auto DescriptorAddress
        = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                          EnumMeta->offsetToDescriptorOffset());
      auto Descriptor = readNominalTypeDescriptor(DescriptorAddress);
      if (!Descriptor)
        return nullptr;

      auto NameAddress
        = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                         Descriptor->offsetToNameOffset());
      auto MangledName = Reader.readString(NameAddress);
      if (MangledName.empty()) return nullptr;

      auto DemangleNode = Demangle::demangleTypeAsNode(MangledName);
      if (!DemangleNode) return nullptr;

      auto Unsubstituted = TypeRef::fromDemangleNode(DemangleNode);
      auto Substitutions = getGenericSubstitutions(Unsubstituted,
                                                   MetadataAddress);
      auto Substituted = Unsubstituted->substituteGenerics(Substitutions);
      auto Resolved = resolveDependentMembers(Substituted, MetadataAddress);
      TypeRefCache.insert({MetadataAddress, Resolved});
      return Resolved;
    }
    case MetadataKind::Tuple: {
      auto TupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta.get());
      TypeRefVector Elements;
      llvm_unreachable("todo");
      return TupleTypeRef::create(Elements);
    }
    case MetadataKind::Function: {
      auto Function = cast<TargetFunctionTypeMetadata<Runtime>>(Meta.get());
      TypeRefVector Arguments;
      llvm_unreachable("todo");
      auto Result = getTypeRef(Function->ResultType);
      return FunctionTypeRef::create(Arguments, Result);
    }
    case MetadataKind::Existential: {
      auto Exist = cast<TargetExistentialTypeMetadata<Runtime>>(Meta.get());
      TypeRefVector Protocols;
      llvm_unreachable("todo");
      return ProtocolCompositionTypeRef::create(Protocols);
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = getTypeRef(Metatype->InstanceType);
      return MetatypeTypeRef::create(Instance);
    }
    case MetadataKind::ObjCClassWrapper:
      return ObjCClassTypeRef::Unnamed;
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = getTypeRef(Exist->InstanceType);
      return ExistentialMetatypeTypeRef::create(Instance);
    }
    case MetadataKind::ForeignClass:
      return ForeignClassTypeRef::Unnamed;
    case MetadataKind::HeapLocalVariable:
      return ForeignClassTypeRef::Unnamed;
    case MetadataKind::HeapGenericLocalVariable:
      return ForeignClassTypeRef::Unnamed;
    case MetadataKind::ErrorObject:
      return ForeignClassTypeRef::Unnamed;
    case MetadataKind::Opaque:
      return OpaqueTypeRef::Opaque;
    }
  }

  void clear() {
    TypeRefCache.clear();
  }

  void addReflectionInfo(ReflectionInfo I) {
    ReflectionInfos.push_back(I);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_REFLECTIONCONTEXT_H
