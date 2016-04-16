//===--- MetadataReader.h - Abstract access to remote metadata --*- C++ -*-===//
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
//  This file defines operations for reading metadata from a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_METADATAREADER_H
#define SWIFT_REMOTE_METADATAREADER_H

#include "swift/Runtime/Metadata.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"

#include <vector>
#include <unordered_map>

namespace swift {
namespace remote {

template <typename Runtime>
using SharedTargetMetadataRef = std::shared_ptr<TargetMetadata<Runtime>>;

template <typename Runtime>
using SharedTargetNominalTypeDescriptorRef
  = std::shared_ptr<TargetNominalTypeDescriptor<Runtime>>;

template <typename Runtime>
using SharedProtocolDescriptorRef
  = std::shared_ptr<TargetProtocolDescriptor<Runtime>>;

/// A utility class for constructing abstract types from
/// a textual mangling.
template <typename BuilderType>
class TypeDecoder {
  using Type = typename BuilderType::Type;
  using NodeKind = Demangle::Node::Kind;

  BuilderType &Builder;

 public:
  explicit TypeDecoder(BuilderType &Builder)
    : Builder(Builder) {}

  /// Given a demangle tree, attempt to turn it into a type.
  Type decodeMangledType(const Demangle::NodePointer &Node) {
    if (!Node) return Type();

    using NodeKind = Demangle::Node::Kind;
    switch (Node->getKind()) {
      case NodeKind::Global:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::TypeMangling:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::Type:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::Class:
      case NodeKind::Enum:
      case NodeKind::Structure: {
        std::string mangledName;
        Type parent = Type();
        if (!decodeMangledNominalType(Node, mangledName, parent))
          return Type();

        return Builder.createNominalType(mangledName, parent);
      }
      case NodeKind::BoundGenericClass:
      case NodeKind::BoundGenericEnum:
      case NodeKind::BoundGenericStructure: {
        assert(Node->getNumChildren() == 2);
        std::string mangledName;
        Type parent = Type();
        if (!decodeMangledNominalType(Node->getChild(0), mangledName, parent))
          return Type();

        std::vector<Type> args;

        const auto &genericArgs = Node->getChild(1);
        assert(genericArgs->getKind() == NodeKind::TypeList);

        for (auto genericArg : *genericArgs) {
          auto paramType = decodeMangledType(genericArg);
          if (!paramType)
            return Type();
          args.push_back(paramType);
        }

        return Builder.createBoundGenericType(mangledName, args, parent);
      }
      case NodeKind::BuiltinTypeName: {
        auto mangledName = Demangle::mangleNode(Node);
        return Builder.createBuiltinType(mangledName);
      }
      case NodeKind::ExistentialMetatype: {
        auto instance = decodeMangledType(Node->getChild(0));
        if (!instance)
          return Type();
        return Builder.createExistentialMetatypeType(instance);
      }
      case NodeKind::Metatype: {
        auto instance = decodeMangledType(Node->getChild(0));
        if (!instance)
          return Type();
        return Builder.createMetatypeType(instance);
      }
      case NodeKind::ProtocolList: {
        std::vector<Type> protocols;
        auto TypeList = Node->getChild(0);
        for (auto componentType : *TypeList) {
          if (auto protocol = decodeMangledType(componentType))
            protocols.push_back(protocol);
          else
            return Type();
        }
        if (protocols.size() == 1)
          return protocols.front();
        else
          return Builder.createProtocolCompositionType(protocols);
      }
      case NodeKind::Protocol: {
        auto moduleName = Node->getChild(0)->getText();
        auto name = Node->getChild(1)->getText();
        return Builder.createProtocolType(moduleName, name);
      }
      case NodeKind::DependentGenericParamType: {
        auto depth = Node->getChild(0)->getIndex();
        auto index = Node->getChild(1)->getIndex();
        return Builder.createGenericTypeParameterType(depth, index);
      }
      case NodeKind::ObjCBlock:
      case NodeKind::CFunctionPointer:
      case NodeKind::ThinFunctionType:
      case NodeKind::FunctionType: {
        FunctionTypeFlags flags;
        if (Node->getKind() == NodeKind::ObjCBlock) {
          flags = flags.withConvention(FunctionMetadataConvention::Block);
        } else if (Node->getKind() == NodeKind::CFunctionPointer) {
          flags =
            flags.withConvention(FunctionMetadataConvention::CFunctionPointer);
        } else if (Node->getKind() == NodeKind::ThinFunctionType) {
          flags = flags.withConvention(FunctionMetadataConvention::Thin);
        }

        bool isThrow =
          Node->getChild(0)->getKind() == NodeKind::ThrowsAnnotation;
        flags = flags.withThrows(true);

        std::vector<Type> arguments;
        std::vector<bool> argsAreInOut;
        if (!decodeMangledFunctionInputType(Node->getChild(isThrow ? 1 : 0),
                                            arguments, argsAreInOut, flags))
          return Type();

        auto result = decodeMangledType(Node->getChild(isThrow ? 2 : 1));
        if (!result) return Type();
        return Builder.createFunctionType(arguments, argsAreInOut,
                                          result, flags);
      }
      case NodeKind::ArgumentTuple:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::ReturnType:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::NonVariadicTuple:
      case NodeKind::VariadicTuple: {
        std::vector<Type> Elements;
        for (auto element : *Node) {
          auto elementType = decodeMangledType(element);
          if (!elementType)
            return Type();
          Elements.push_back(elementType);
        }
        bool Variadic = (Node->getKind() == NodeKind::VariadicTuple);
        return Builder.createTupleType(Elements, Variadic);
      }
      case NodeKind::TupleElement:
        if (Node->getChild(0)->getKind() == NodeKind::TupleElementName)
          return decodeMangledType(Node->getChild(1));
        return decodeMangledType(Node->getChild(0));
      case NodeKind::DependentGenericType: {
        return decodeMangledType(Node->getChild(1));
      }
      case NodeKind::DependentMemberType: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return Type();
        auto member = Node->getChild(1)->getText();
        auto protocol = decodeMangledType(Node->getChild(1));
        if (!protocol)
          return Type();
        return Builder.createDependentMemberType(member, base, protocol);
      }
      case NodeKind::DependentAssociatedTypeRef:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::Unowned: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return Type();
        return Builder.createUnownedStorageType(base);
      }
      case NodeKind::Unmanaged: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return Type();
        return Builder.createUnmanagedStorageType(base);
      }
      case NodeKind::Weak: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return Type();
        return Builder.createWeakStorageType(base);
      }
      default:
        return Type();
    }
  }

private:
  bool decodeMangledNominalType(const Demangle::NodePointer &node,
                                std::string &mangledName,
                                Type &parent) {
    if (node->getKind() == NodeKind::Type)
      return decodeMangledNominalType(node->getChild(0), mangledName, parent);

    assert(node->getNumChildren() == 2);
    auto moduleOrParentType = node->getChild(0);

    // Nested types are handled a bit funny here because a
    // nominal typeref always stores its full mangled name,
    // in addition to a reference to the parent type. The
    // mangled name already includes the module and parent
    // types, if any.
    if (moduleOrParentType->getKind() != NodeKind::Module) {
      parent = decodeMangledType(moduleOrParentType);
      if (!parent) return false;
    }

    mangledName = Demangle::mangleNode(node);
    return true;
  }

  bool decodeMangledFunctionInputType(const Demangle::NodePointer &node,
                                      std::vector<Type> &args,
                                      std::vector<bool> &argsAreInOut,
                                      FunctionTypeFlags &flags) {
    // Look through a couple of sugar nodes.
    if (node->getKind() == NodeKind::Type ||
        node->getKind() == NodeKind::ArgumentTuple) {
      return decodeMangledFunctionInputType(node->getFirstChild(),
                                            args, argsAreInOut, flags);
    }

    auto decodeSingleHelper =
    [&](const Demangle::NodePointer &typeNode, bool argIsInOut) -> bool {
      Type argType = decodeMangledType(typeNode);
      if (!argType) return false;

      args.push_back(argType);
      argsAreInOut.push_back(argIsInOut);
      return true;
    };
    auto decodeSingle =
    [&](const Demangle::NodePointer &typeNode) -> bool {
      if (typeNode->getKind() == NodeKind::InOut) {
        return decodeSingleHelper(typeNode->getFirstChild(), true);
      } else {
        return decodeSingleHelper(typeNode, false);
      }
    };

    // Expand a single level of tuple.
    if (node->getKind() == NodeKind::VariadicTuple ||
        node->getKind() == NodeKind::NonVariadicTuple) {
      // TODO: preserve variadic somewhere?

      // Decode all the elements as separate arguments.
      for (const auto &elt : *node) {
        if (elt->getKind() != NodeKind::TupleElement)
          return false;
        auto typeNode = elt->getChild(elt->getNumChildren() - 1);
        if (typeNode->getKind() != NodeKind::Type)
          return false;
        if (!decodeSingle(typeNode->getFirstChild()))
          return false;
      }

      return true;
    }

    // Otherwise, handle the type as a single argument.
    return decodeSingle(node);
  }
};

template<typename BuilderType>
static inline typename BuilderType::Type
decodeMangledType(BuilderType &Builder,
                  const Demangle::NodePointer &Node) {
  return TypeDecoder<BuilderType>(Builder).decodeMangledType(Node);
}

/// A generic reader of metadata.
///
/// BuilderType must implement a particular interface which is currently
/// too fluid to allow useful documentation; consult the actual
/// implementations.  The chief thing is that it provides several member
/// types which should obey the following constraints:
///   - T() yields a value which is false when contextually converted to bool
///   - a false value signals that an error occurred when building a value
template <typename Runtime, typename BuilderType>
class MetadataReader {
public:
  using Type = typename BuilderType::Type;
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;

private:
  std::unordered_map<StoredPointer, Type> TypeCache;
  std::unordered_map<StoredPointer, SharedTargetMetadataRef<Runtime>>
  MetadataCache;

  std::unordered_map<StoredPointer,
                     std::pair<SharedTargetNominalTypeDescriptorRef<Runtime>,
                               StoredPointer>>
    NominalTypeDescriptorCache;

public:
  BuilderType Builder;

  BuilderType &getBuilder() {
    return this->Builder;
  }

  std::shared_ptr<MemoryReader> Reader;

  template <class... T>
  MetadataReader(std::shared_ptr<MemoryReader> reader, T &&... args)
    : Builder(std::forward<T>(args)...),
      Reader(std::move(reader)) {

  }

  MetadataReader(const MetadataReader &other) = delete;
  MetadataReader &operator=(const MetadataReader &other) = delete;

  /// Clear all of the caches in this reader.
  void clear() {
    TypeCache.clear();
    MetadataCache.clear();
    NominalTypeDescriptorCache.clear();
  }

  /// Given a demangle tree, attempt to turn it into a type.
  Type decodeMangledType(const Demangle::NodePointer &Node) {
    return swift::remote::decodeMangledType(Builder, Node);
  }

  /// Given a remote pointer to metadata, attempt to turn it into a type.
  Type readTypeFromMetadata(StoredPointer MetadataAddress) {
    auto Cached = TypeCache.find(MetadataAddress);
    if (Cached != TypeCache.end())
      return Cached->second;

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta) return Type();

    switch (Meta->getKind()) {
    case MetadataKind::Class:
      return readNominalTypeFromMetadata(MetadataAddress);
    case MetadataKind::Struct:
      return readNominalTypeFromMetadata(MetadataAddress);
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      return readNominalTypeFromMetadata(MetadataAddress);
    case MetadataKind::Tuple: {
      auto TupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta.get());
      std::vector<Type> Elements;
      StoredPointer ElementAddress = MetadataAddress +
        sizeof(TargetTupleTypeMetadata<Runtime>);
      using Element = typename TargetTupleTypeMetadata<Runtime>::Element;
      for (StoredPointer i = 0; i < TupleMeta->NumElements; ++i,
           ElementAddress += sizeof(Element)) {
        Element E;
        if (!Reader->readBytes(RemoteAddress(ElementAddress),
                               (uint8_t*)&E, sizeof(Element)))
          return Type();

        if (auto ElementTypeRef = readTypeFromMetadata(E.Type))
          Elements.push_back(ElementTypeRef);
        else
          return Type();
      }
      return Builder.createTupleType(Elements, /*variadic*/ false);
    }
    case MetadataKind::Function: {
      auto Function = cast<TargetFunctionTypeMetadata<Runtime>>(Meta.get());

      std::vector<Type> Arguments;
      std::vector<bool> ArgumentIsInOut;
      StoredPointer ArgumentAddress = MetadataAddress +
        sizeof(TargetFunctionTypeMetadata<Runtime>);
      for (StoredPointer i = 0; i < Function->getNumArguments(); ++i,
           ArgumentAddress += sizeof(StoredPointer)) {
        StoredPointer FlaggedArgumentAddress;
        if (!Reader->readInteger(RemoteAddress(ArgumentAddress),
                                 &FlaggedArgumentAddress))
          return Type();

        // TODO: Use target-agnostic FlaggedPointer to mask this!
        const auto InOutMask = (StoredPointer) 1;
        ArgumentIsInOut.push_back((FlaggedArgumentAddress & InOutMask) != 0);
        FlaggedArgumentAddress &= ~InOutMask;

        if (auto ArgumentTypeRef = readTypeFromMetadata(FlaggedArgumentAddress))
          Arguments.push_back(ArgumentTypeRef);
        else
          return Type();
      }

      auto Result = readTypeFromMetadata(Function->ResultType);
      if (!Result)
        return Type();

      auto flags = FunctionTypeFlags().withConvention(Function->getConvention())
                                      .withThrows(Function->throws());
      return Builder.createFunctionType(Arguments, ArgumentIsInOut,
                                        Result, flags);
    }
    case MetadataKind::Existential: {
      auto Exist = cast<TargetExistentialTypeMetadata<Runtime>>(Meta.get());
      std::vector<Type> Protocols;
      for (size_t i = 0; i < Exist->Protocols.NumProtocols; ++i) {
        auto ProtocolAddress = Exist->Protocols[i];
        auto ProtocolDescriptor = readProtocolDescriptor(ProtocolAddress);
        if (!ProtocolDescriptor)
          return Type();
        
        std::string MangledName;
        if (!Reader->readString(RemoteAddress(ProtocolDescriptor->Name),
                                MangledName))
          return Type();
        auto Demangled = Demangle::demangleSymbolAsNode(MangledName);
        auto Protocol = decodeMangledType(Demangled);
        if (!Protocol)
          return Type();

        Protocols.push_back(Protocol);
      }
      return Builder.createProtocolCompositionType(Protocols);
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = readTypeFromMetadata(Metatype->InstanceType);
      if (!Instance) return Type();
      return Builder.createMetatypeType(Instance);
    }
    case MetadataKind::ObjCClassWrapper:
      return Builder.getUnnamedObjCClassType();
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta.get());
      auto Instance = readTypeFromMetadata(Exist->InstanceType);
      if (!Instance) return Type();
      return Builder.createExistentialMetatypeType(Instance);
    }
    case MetadataKind::ForeignClass:
      return Builder.getUnnamedForeignClassType();
    case MetadataKind::HeapLocalVariable:
      return Builder.getUnnamedForeignClassType(); // FIXME?
    case MetadataKind::HeapGenericLocalVariable:
      return Builder.getUnnamedForeignClassType(); // FIXME?
    case MetadataKind::ErrorObject:
      return Builder.getUnnamedForeignClassType(); // FIXME?
    case MetadataKind::Opaque:
      return Builder.getOpaqueType(); // FIXME?
    }
  }

protected:
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

private:
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
      std::vector<Type> Substitutions;
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

  std::vector<Type> getGenericSubst(StoredPointer MetadataAddress) {
    StoredPointer DescriptorAddress;
    SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
    std::tie(Descriptor, DescriptorAddress)
      = readNominalTypeDescriptor(MetadataAddress);
    std::vector<Type> Substitutions;
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
      if (auto GenericArg = readTypeFromMetadata(GenericArgAddress))
        Substitutions.push_back(GenericArg);
      else
        return {};
    }
    return Substitutions;
  }

  Type readNominalTypeFromMetadata(StoredPointer MetadataAddress) {
    auto Meta = readMetadata(MetadataAddress);

    StoredPointer DescriptorAddress;
    SharedTargetNominalTypeDescriptorRef<Runtime> Descriptor;
    std::tie(Descriptor, DescriptorAddress)
      = readNominalTypeDescriptor(MetadataAddress);
    if (!Descriptor)
      return Type();

    auto NameAddress
      = resolveRelativeOffset<int32_t>(DescriptorAddress +
                                       Descriptor->offsetToNameOffset());
    std::string MangledName;
    if (!Reader->readString(RemoteAddress(NameAddress), MangledName))
      return Type();

    Type Parent;
    if (auto ParentAddress = getParentAddress(MetadataAddress)) {
      Parent = readTypeFromMetadata(ParentAddress);
      if (!Parent) return Type();
    }

    Type Nominal;
    if (Descriptor->GenericParams.NumPrimaryParams) {
      auto Args = getGenericSubst(MetadataAddress);
      if (Args.empty()) return Type();
      Nominal = Builder.createBoundGenericType(MangledName, Args, Parent);
    } else {
      Nominal = Builder.createNominalType(MangledName, Parent);
    }
    if (!Nominal) return Type();

    TypeCache.insert({MetadataAddress, Nominal});
    return Nominal;
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_READER_H

