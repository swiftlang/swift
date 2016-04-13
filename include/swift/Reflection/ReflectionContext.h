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
#include "swift/Remote/MemoryReader.h"
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

template <typename Runtime>
using SharedTargetMetadataRef = std::shared_ptr<TargetMetadata<Runtime>>;

template <typename Runtime>
using SharedTargetNominalTypeDescriptorRef
  = std::shared_ptr<TargetNominalTypeDescriptor<Runtime>>;

template <typename Runtime>
using SharedProtocolDescriptorRef
  = std::shared_ptr<TargetProtocolDescriptor<Runtime>>;

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
  using StoredSize = typename Runtime::StoredSize;

  std::vector<std::unique_ptr<TypeRef>> TypeRefPool;

  std::vector<ReflectionInfo> ReflectionInfos;
  std::unordered_map<StoredPointer, TypeRef *> TypeRefCache;
  std::unordered_map<StoredPointer, SharedTargetMetadataRef<Runtime>>
  MetadataCache;

  std::unordered_map<StoredPointer,
                     std::pair<SharedTargetNominalTypeDescriptorRef<Runtime>,
                               StoredPointer>>
  NominalTypeDescriptorCache;

  std::shared_ptr<MemoryReader> Reader;

  void dumpTypeRef(const std::string &MangledName,
                   std::ostream &OS, bool printTypeName = false) {
    auto TypeName = Demangle::demangleTypeAsString(MangledName);
    OS << TypeName << std::endl;

    auto DemangleTree = Demangle::demangleTypeAsNode(MangledName);
    auto TR = decodeTypeRef(DemangleTree);
    if (!TR) {
      OS << "!!! Invalid typeref: " << MangledName << std::endl;
      return;
    }
    TR->dump(OS);
    OS << std::endl;
  }

  template <typename M>
  SharedTargetMetadataRef<Runtime> _readMetadata(StoredPointer Address,
                                                 size_t Size = sizeof(M)) {
    uint8_t *Buffer = (uint8_t *)malloc(Size);
    if (!Reader->readBytes(RemoteAddress(Address), Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }

    auto Casted = reinterpret_cast<TargetMetadata<Runtime> *>(Buffer);
    auto Meta = SharedTargetMetadataRef<Runtime>(Casted, free);
    MetadataCache.insert({Address, Meta});
    return Meta;
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
        auto TR = decodeTypeRef(DemangledProto);

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

  ReflectionContext(std::shared_ptr<MemoryReader> Reader)
    : Reader(Reader) {}

  MemoryReader &getReader() {
    return *Reader;
  }

  template <typename TypeRefTy, typename... Args>
  TypeRefTy *make_typeref(Args... args) {
    auto TR = new TypeRefTy(::std::forward<Args>(args)...);
    TypeRefPool.push_back(std::unique_ptr<TypeRef>(TR));
    return TR;
  }

  TypeRef *decodeTypeRef(Demangle::NodePointer Node) {
    using NodeKind = Demangle::Node::Kind;
    switch (Node->getKind()) {
      case NodeKind::Global:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::TypeMangling:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::Type:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::BoundGenericClass:
      case NodeKind::BoundGenericEnum:
      case NodeKind::BoundGenericStructure: {
        auto mangledName = Demangle::mangleNode(Node->getChild(0));
        auto genericArgs = Node->getChild(1);
        std::vector<TypeRef *> Args;
        for (auto genericArg : *genericArgs) {
          auto paramTypeRef = decodeTypeRef(genericArg);
          if (!paramTypeRef)
            return nullptr;
          Args.push_back(paramTypeRef);
        }

        return BoundGenericTypeRef::create(*this, mangledName, Args);
      }
      case NodeKind::Class:
      case NodeKind::Enum:
      case NodeKind::Structure: {
        auto mangledName = Demangle::mangleNode(Node);
        return NominalTypeRef::create(*this, mangledName);
      }
      case NodeKind::BuiltinTypeName: {
        auto mangledName = Demangle::mangleNode(Node);
        return BuiltinTypeRef::create(*this, mangledName);
      }
      case NodeKind::ExistentialMetatype: {
        auto instance = decodeTypeRef(Node->getChild(0));
        if (!instance)
          return nullptr;
        return ExistentialMetatypeTypeRef::create(*this, instance);
      }
      case NodeKind::Metatype: {
        auto instance = decodeTypeRef(Node->getChild(0));
        if (!instance)
          return nullptr;
        return MetatypeTypeRef::create(*this, instance);
      }
      case NodeKind::ProtocolList: {
        std::vector<TypeRef *> Protocols;
        auto TypeList = Node->getChild(0);
        for (auto Type : *TypeList) {
          if (auto Protocol = decodeTypeRef(Type))
            Protocols.push_back(Protocol);
          else
            return nullptr;
        }
        if (Protocols.size() == 1)
          return Protocols.front();
        else
          return ProtocolCompositionTypeRef::create(*this, Protocols);
      }
      case NodeKind::Protocol: {
        auto moduleName = Node->getChild(0)->getText();
        auto name = Node->getChild(1)->getText();
        return ProtocolTypeRef::create(*this, moduleName, name);
      }
      case NodeKind::DependentGenericParamType: {
        auto depth = Node->getChild(0)->getIndex();
        auto index = Node->getChild(1)->getIndex();
        return GenericTypeParameterTypeRef::create(*this, depth, index);
      }
      case NodeKind::FunctionType: {
        std::vector<TypeRef *> arguments;
        auto input = decodeTypeRef(Node->getChild(0));
        if (!input)
          return nullptr;
        if (auto tuple = dyn_cast<TupleTypeRef>(input))
          arguments = tuple->getElements();
        else
          arguments = { input };
        auto result = decodeTypeRef(Node->getChild(1));
        if (!result)
          return nullptr;
        return FunctionTypeRef::create(*this, arguments, result);
      }
      case NodeKind::ArgumentTuple:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::ReturnType:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::NonVariadicTuple:
      case NodeKind::VariadicTuple: {
        std::vector<TypeRef *> Elements;
        for (auto element : *Node) {
          auto elementType = decodeTypeRef(element);
          if (!elementType)
            return nullptr;
          Elements.push_back(elementType);
        }
        bool Variadic = (Node->getKind() == NodeKind::VariadicTuple);
        return TupleTypeRef::create(*this, Elements, Variadic);
      }
      case NodeKind::TupleElement:
        if (Node->getChild(0)->getKind() == NodeKind::TupleElementName)
          return decodeTypeRef(Node->getChild(1));
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::DependentGenericType: {
        return decodeTypeRef(Node->getChild(1));
      }
      case NodeKind::DependentMemberType: {
        auto base = decodeTypeRef(Node->getChild(0));
        if (!base)
          return nullptr;
        auto member = Node->getChild(1)->getText();
        auto protocol = decodeTypeRef(Node->getChild(1));
        if (!protocol)
          return nullptr;
        assert(llvm::isa<ProtocolTypeRef>(protocol));
        return DependentMemberTypeRef::create(*this, member, base, protocol);
      }
      case NodeKind::DependentAssociatedTypeRef:
        return decodeTypeRef(Node->getChild(0));
      case NodeKind::Unowned: {
        auto base = decodeTypeRef(Node->getChild(0));
        if (!base)
          return nullptr;
        return UnownedStorageTypeRef::create(*this, base);
      }
      case NodeKind::Unmanaged: {
        auto base = decodeTypeRef(Node->getChild(0));
        if (!base)
          return nullptr;
        return UnmanagedStorageTypeRef::create(*this, base);
      }
      case NodeKind::Weak: {
        auto base = decodeTypeRef(Node->getChild(0));
        if (!base)
          return nullptr;
        return WeakStorageTypeRef::create(*this, base);
      }
      default:
        return nullptr;
    }
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
        return decodeTypeRef(Demangled);
      }
    }
    return nullptr;
  }

  SharedTargetMetadataRef<Runtime> readMetadata(StoredPointer Address) {
    auto Cached = MetadataCache.find(Address);
    if (Cached != MetadataCache.end())
      return Cached->second;

    StoredPointer KindValue = 0;
    if (!Reader->readInteger(RemoteAddress(Address), &KindValue))
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
      case MetadataKind::Existential: {
        StoredPointer NumProtocolsAddress = Address +
          TargetExistentialTypeMetadata<Runtime>::OffsetToNumProtocols;
        StoredPointer NumProtocols;
        if (!Reader->readInteger(RemoteAddress(NumProtocolsAddress),
                                 &NumProtocols))
          return nullptr;

        auto TotalSize = sizeof(TargetExistentialTypeMetadata<Runtime>) +
          NumProtocols *
            sizeof(ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor>);
        
        return _readMetadata<TargetExistentialTypeMetadata<Runtime>>(Address,
                                                                     TotalSize);
      }
      case MetadataKind::ExistentialMetatype:
        return _readMetadata<
          TargetExistentialMetatypeMetadata<Runtime>>(Address);
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
      case MetadataKind::Tuple: {
        auto NumElementsAddress = Address +
          TargetTupleTypeMetadata<Runtime>::OffsetToNumElements;
        StoredSize NumElements;
        if (!Reader->readInteger(RemoteAddress(NumElementsAddress),
                                 &NumElements))
          return nullptr;
        auto TotalSize = sizeof(TargetTupleTypeMetadata<Runtime>) +
          NumElements * sizeof(StoredPointer);
        return _readMetadata<TargetTupleTypeMetadata<Runtime>>(Address,
                                                               TotalSize);
      }
      default:
        return nullptr;
      }
    }
  }

  template<typename Offset>
  StoredPointer resolveRelativeOffset(StoredPointer targetAddress) {
    Offset relative;
    if (!Reader->readInteger(RemoteAddress(targetAddress), &relative))
      return 0;
    using SignedOffset = typename std::make_signed<Offset>::type;
    using SignedPointer = typename std::make_signed<StoredPointer>::type;
    auto signext = (SignedPointer)(SignedOffset)relative;
    return targetAddress + signext;
  }

  std::pair<SharedTargetNominalTypeDescriptorRef<Runtime>, StoredPointer>
  readNominalTypeDescriptor(StoredPointer MetadataAddress) {
    auto Cached = NominalTypeDescriptorCache.find(MetadataAddress);
    if (Cached != NominalTypeDescriptorCache.end())
      return Cached->second;

    auto Meta = readMetadata(MetadataAddress);
    StoredPointer DescriptorAddress;

    switch (Meta->getKind()) {
      case MetadataKind::Class: {
        auto ClassMeta = cast<TargetClassMetadata<Runtime>>(Meta.get());
        DescriptorAddress
          = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                         ClassMeta->offsetToDescriptorOffset());
        break;
      }
      case MetadataKind::Struct: {
        auto StructMeta = cast<TargetStructMetadata<Runtime>>(Meta.get());
        DescriptorAddress
          = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                        StructMeta->offsetToDescriptorOffset());
        break;
      }
      case MetadataKind::Optional:
      case MetadataKind::Enum: {
        auto EnumMeta = cast<TargetEnumMetadata<Runtime>>(Meta.get());
        DescriptorAddress
          = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                          EnumMeta->offsetToDescriptorOffset());
        break;
      }
      default:
        return {nullptr, 0};
    }

    auto Size = sizeof(TargetNominalTypeDescriptor<Runtime>);
    auto Buffer = (uint8_t *)malloc(Size);
    if (!Reader->readBytes(RemoteAddress(DescriptorAddress), Buffer, Size)) {
      free(Buffer);
      return {nullptr, 0};
    }

    auto Casted
      = reinterpret_cast<TargetNominalTypeDescriptor<Runtime> *>(Buffer);

    auto Descriptor
      = SharedTargetNominalTypeDescriptorRef<Runtime>(Casted, free);

    std::pair<SharedTargetNominalTypeDescriptorRef<Runtime>, StoredPointer>
    Result = {
      Descriptor,
      DescriptorAddress
    };
    NominalTypeDescriptorCache.insert({MetadataAddress, Result});
    return Result;
  }

  SharedProtocolDescriptorRef<Runtime>
  readProtocolDescriptor(StoredPointer Address) {
    auto Size = sizeof(TargetProtocolDescriptor<Runtime>);
    auto Buffer = (uint8_t *)malloc(Size);
    if (!Reader->readBytes(RemoteAddress(Address), Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }
    auto Casted
      = reinterpret_cast<TargetProtocolDescriptor<Runtime> *>(Buffer);
    return SharedProtocolDescriptorRef<Runtime>(Casted, free);
  }

  StoredPointer getParentAddress(StoredPointer MetadataAddress) {
    auto Meta = readMetadata(MetadataAddress);
    StoredPointer ParentAddress = 0;
    if (auto ValueMeta = dyn_cast<TargetValueMetadata<Runtime>>(Meta.get())) {
      auto AddressOfParentAddress
        = resolveRelativeOffset<StoredPointer>(MetadataAddress +
                                             ValueMeta->offsetToParentOffset());
      if (!Reader->readInteger(RemoteAddress(AddressOfParentAddress),
                               &ParentAddress))
        return 0;
    } else if (auto Class = dyn_cast<TargetClassMetadata<Runtime>>(Meta.get())){
      StoredPointer DescriptorAddress;
      SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
      std::tie(Descriptor, DescriptorAddress)
        = readNominalTypeDescriptor(MetadataAddress);
      std::vector<TypeRef *> Substitutions;
      auto OffsetToParent
        = sizeof(StoredPointer) * (Descriptor->GenericParams.Offset - 1);
      if (!Reader->readInteger(RemoteAddress(MetadataAddress + OffsetToParent),
                               &ParentAddress))
        return 0;
    }
    return ParentAddress;
  }

  unsigned getNominalTypeDepth(StoredPointer MetadataAddress) {
    if (auto ParentAddress = getParentAddress(MetadataAddress))
      return 1 + getNominalTypeDepth(ParentAddress);
    return 0;
  }

  std::vector<TypeRef *> getGenericSubst(StoredPointer MetadataAddress) {
    StoredPointer DescriptorAddress;
    SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
    std::tie(Descriptor, DescriptorAddress)
      = readNominalTypeDescriptor(MetadataAddress);
    std::vector<TypeRef *> Substitutions;
    auto NumGenericParams = Descriptor->GenericParams.NumPrimaryParams;
    auto OffsetToGenericArgs
      = sizeof(StoredPointer) * (Descriptor->GenericParams.Offset);
    auto AddressOfGenericArgAddress = MetadataAddress + OffsetToGenericArgs;

    using ArgIndex = decltype(Descriptor->GenericParams.NumPrimaryParams);
    for (ArgIndex i = 0; i < NumGenericParams; ++i,
         AddressOfGenericArgAddress += sizeof(StoredPointer)) {
        StoredPointer GenericArgAddress;
        if (!Reader->readInteger(RemoteAddress(AddressOfGenericArgAddress),
                                 &GenericArgAddress))
          return {};
      if (auto GenericArg = getTypeRef(GenericArgAddress))
        Substitutions.push_back(GenericArg);
      else
        return {};
    }
    return Substitutions;
  }

  TypeRef *
  getNominalTypeRef(StoredPointer MetadataAddress) {
    auto Meta = readMetadata(MetadataAddress);

    StoredPointer DescriptorAddress;
    SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
    std::tie(Descriptor, DescriptorAddress)
      = readNominalTypeDescriptor(MetadataAddress);
    if (!Descriptor)
      return nullptr;

    auto NameAddress
      = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                       Descriptor->offsetToNameOffset());
    std::string MangledName;
    if (!Reader->readString(RemoteAddress(NameAddress), MangledName))
      return nullptr;

    auto DemangleNode = Demangle::demangleTypeAsNode(MangledName);
    if (!DemangleNode)
      return nullptr;

    TypeRef *Parent;
    if (auto ParentAddress = getParentAddress(MetadataAddress))
      Parent = getTypeRef(ParentAddress);

    TypeRef *Nominal;
    if (Descriptor->GenericParams.NumPrimaryParams) {
      auto Args = getGenericSubst(MetadataAddress);
      auto BG = BoundGenericTypeRef::create(*this, MangledName, Args);
      BG->setParent(Parent);
      Nominal = BG;
    } else {
      auto TR = decodeTypeRef(DemangleNode);
      auto N = cast<NominalTypeRef>(TR);
      N->setParent(Parent);
      Nominal = TR;
    }
    TypeRefCache.insert({MetadataAddress, Nominal});
    return Nominal;
  }

  TypeRef *getTypeRef(StoredPointer MetadataAddress) {
    auto Cached = TypeRefCache.find(MetadataAddress);
    if (Cached != TypeRefCache.end())
      return Cached->second;

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta) return nullptr;

    switch (Meta->getKind()) {
    case MetadataKind::Class:
      return getNominalTypeRef(MetadataAddress);
    case MetadataKind::Struct:
      return getNominalTypeRef(MetadataAddress);
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      return getNominalTypeRef(MetadataAddress);
    case MetadataKind::Tuple: {
      auto TupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta.get());
      std::vector<TypeRef *> Elements;
      StoredPointer ElementAddress = MetadataAddress +
        sizeof(TargetTupleTypeMetadata<Runtime>);
      using Element = typename TargetTupleTypeMetadata<Runtime>::Element;
      for (StoredPointer i = 0; i < TupleMeta->NumElements; ++i,
           ElementAddress += sizeof(Element)) {
        Element E;
        if (!Reader->readBytes(RemoteAddress(ElementAddress),
                               (uint8_t*)&E, sizeof(Element)))
          return nullptr;

        if (auto ElementTypeRef = getTypeRef(E.Type))
          Elements.push_back(ElementTypeRef);
        else
          return nullptr;
      }
      return TupleTypeRef::create(*this, Elements);
    }
    case MetadataKind::Function: {
      auto Function = cast<TargetFunctionTypeMetadata<Runtime>>(Meta.get());
      StoredPointer FlagsAddress = MetadataAddress +
        TargetFunctionTypeMetadata<Runtime>::OffsetToFlags;
      TargetFunctionTypeFlags<Runtime> Flags;
      if (!Reader->readBytes(RemoteAddress(FlagsAddress),
                             (uint8_t*)&Flags, sizeof(Flags)))
        return nullptr;
      std::vector<TypeRef *> Arguments;
      StoredPointer ArgumentAddress = MetadataAddress +
        sizeof(TargetFunctionTypeMetadata<Runtime>);
      for (StoredPointer i = 0; i < Function->getNumArguments(); ++i,
           ArgumentAddress += sizeof(StoredPointer)) {
        StoredPointer FlaggedArgumentAddress;
        if (!Reader->readInteger(RemoteAddress(ArgumentAddress),
                                 &FlaggedArgumentAddress))
          return nullptr;
        // TODO: Use target-agnostic FlaggedPointer to mask this!
        FlaggedArgumentAddress &= ~((StoredPointer)1);
        if (auto ArgumentTypeRef = getTypeRef(FlaggedArgumentAddress))
          Arguments.push_back(ArgumentTypeRef);
        else
          return nullptr;
      }

      auto Result = getTypeRef(Function->ResultType);
      if (!Result)
        return nullptr;
      return FunctionTypeRef::create(*this, Arguments, Result);
    }
    case MetadataKind::Existential: {
      auto Exist = cast<TargetExistentialTypeMetadata<Runtime>>(Meta.get());
      std::vector<TypeRef *> Protocols;
      for (size_t i = 0; i < Exist->Protocols.NumProtocols; ++i) {
        auto ProtocolAddress = Exist->Protocols[i];
        auto ProtocolDescriptor = readProtocolDescriptor(ProtocolAddress);
        if (!ProtocolDescriptor)
          return nullptr;
        
        std::string MangledName;
        if (!Reader->readString(RemoteAddress(ProtocolDescriptor->Name),
                                MangledName))
          return nullptr;
        auto Demangled = Demangle::demangleSymbolAsNode(MangledName);
        auto Protocol = decodeTypeRef(Demangled);
        if (!llvm::isa<ProtocolTypeRef>(Protocol))
          return nullptr;

        Protocols.push_back(Protocol);
      }
      return ProtocolCompositionTypeRef::create(*this, Protocols);
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = getTypeRef(Metatype->InstanceType);
      return MetatypeTypeRef::create(*this, Instance);
    }
    case MetadataKind::ObjCClassWrapper:
      return ObjCClassTypeRef::getUnnamed();
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = getTypeRef(Exist->InstanceType);
      return ExistentialMetatypeTypeRef::create(*this, Instance);
    }
    case MetadataKind::ForeignClass:
      return ForeignClassTypeRef::getUnnamed();
    case MetadataKind::HeapLocalVariable:
      return ForeignClassTypeRef::getUnnamed();
    case MetadataKind::HeapGenericLocalVariable:
      return ForeignClassTypeRef::getUnnamed();
    case MetadataKind::ErrorObject:
      return ForeignClassTypeRef::getUnnamed();
    case MetadataKind::Opaque:
      return OpaqueTypeRef::get();
    }
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
          auto Unsubstituted = decodeTypeRef(Demangled);
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
    auto Meta = readMetadata(MetadataAddress);
    if (!Meta)
      return {};

    StoredPointer DescriptorAddress;
    SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
    std::tie(Descriptor, DescriptorAddress)
      = readNominalTypeDescriptor(MetadataAddress);
    if (!Descriptor)
      return {};

    auto NameAddress
      = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                       Descriptor->offsetToNameOffset());
    std::string MangledName;
    if (!Reader->readString(RemoteAddress(NameAddress), MangledName))
      return {};

    auto TR = getTypeRef(MetadataAddress);
    return getFieldTypeRefs(TR);
  }

  void clear() {
    TypeRefCache.clear();
    MetadataCache.clear();
    NominalTypeDescriptorCache.clear();
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
