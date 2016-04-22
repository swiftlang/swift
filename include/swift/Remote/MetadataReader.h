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
  using BuiltType = typename BuilderType::BuiltType;
  using BuiltNominalTypeDecl = typename BuilderType::BuiltNominalTypeDecl;
  using NodeKind = Demangle::Node::Kind;

  BuilderType &Builder;

 public:
  explicit TypeDecoder(BuilderType &Builder)
    : Builder(Builder) {}

  /// Given a demangle tree, attempt to turn it into a type.
  BuiltType decodeMangledType(const Demangle::NodePointer &Node) {
    if (!Node) return BuiltType();

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
        BuiltNominalTypeDecl typeDecl = BuiltNominalTypeDecl();
        BuiltType parent = BuiltType();
        if (!decodeMangledNominalType(Node, typeDecl, parent))
          return BuiltType();

        return Builder.createNominalType(typeDecl, parent);
      }
      case NodeKind::BoundGenericClass:
      case NodeKind::BoundGenericEnum:
      case NodeKind::BoundGenericStructure: {
        assert(Node->getNumChildren() == 2);
        BuiltNominalTypeDecl typeDecl = BuiltNominalTypeDecl();
        BuiltType parent = BuiltType();
        if (!decodeMangledNominalType(Node->getChild(0), typeDecl, parent))
          return BuiltType();

        std::vector<BuiltType> args;

        const auto &genericArgs = Node->getChild(1);
        assert(genericArgs->getKind() == NodeKind::TypeList);

        for (auto genericArg : *genericArgs) {
          auto paramType = decodeMangledType(genericArg);
          if (!paramType)
            return BuiltType();
          args.push_back(paramType);
        }

        return Builder.createBoundGenericType(typeDecl, args, parent);
      }
      case NodeKind::BuiltinTypeName: {
        auto mangledName = Demangle::mangleNode(Node);
        return Builder.createBuiltinType(mangledName);
      }
      case NodeKind::ExistentialMetatype: {
        auto instance = decodeMangledType(Node->getChild(0));
        if (!instance)
          return BuiltType();
        return Builder.createExistentialMetatypeType(instance);
      }
      case NodeKind::Metatype: {
        auto instance = decodeMangledType(Node->getChild(0));
        if (!instance)
          return BuiltType();
        return Builder.createMetatypeType(instance);
      }
      case NodeKind::ProtocolList: {
        std::vector<BuiltType> protocols;
        auto TypeList = Node->getChild(0);
        for (auto componentType : *TypeList) {
          if (auto protocol = decodeMangledType(componentType))
            protocols.push_back(protocol);
          else
            return BuiltType();
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

        std::vector<BuiltType> arguments;
        std::vector<bool> argsAreInOut;
        if (!decodeMangledFunctionInputType(Node->getChild(isThrow ? 1 : 0),
                                            arguments, argsAreInOut, flags))
          return BuiltType();

        auto result = decodeMangledType(Node->getChild(isThrow ? 2 : 1));
        if (!result) return BuiltType();
        return Builder.createFunctionType(arguments, argsAreInOut,
                                          result, flags);
      }
      case NodeKind::ArgumentTuple:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::ReturnType:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::NonVariadicTuple:
      case NodeKind::VariadicTuple: {
        std::vector<BuiltType> Elements;
        for (auto element : *Node) {
          auto elementType = decodeMangledType(element);
          if (!elementType)
            return BuiltType();
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
          return BuiltType();
        auto member = Node->getChild(1)->getText();
        auto protocol = decodeMangledType(Node->getChild(1));
        if (!protocol)
          return BuiltType();
        return Builder.createDependentMemberType(member, base, protocol);
      }
      case NodeKind::DependentAssociatedTypeRef:
        return decodeMangledType(Node->getChild(0));
      case NodeKind::Unowned: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return BuiltType();
        return Builder.createUnownedStorageType(base);
      }
      case NodeKind::Unmanaged: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return BuiltType();
        return Builder.createUnmanagedStorageType(base);
      }
      case NodeKind::Weak: {
        auto base = decodeMangledType(Node->getChild(0));
        if (!base)
          return BuiltType();
        return Builder.createWeakStorageType(base);
      }
      default:
        return BuiltType();
    }
  }

private:
  bool decodeMangledNominalType(const Demangle::NodePointer &node,
                                BuiltNominalTypeDecl &typeDecl,
                                BuiltType &parent) {
    if (node->getKind() == NodeKind::Type)
      return decodeMangledNominalType(node->getChild(0), typeDecl, parent);

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

    typeDecl = Builder.createNominalTypeDecl(node);
    if (!typeDecl) return false;

    return true;
  }

  bool decodeMangledFunctionInputType(const Demangle::NodePointer &node,
                                      std::vector<BuiltType> &args,
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
      BuiltType argType = decodeMangledType(typeNode);
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
static inline typename BuilderType::BuiltType
decodeMangledType(BuilderType &Builder,
                  const Demangle::NodePointer &Node) {
  return TypeDecoder<BuilderType>(Builder).decodeMangledType(Node);
}

/// A pointer to the local buffer of an object that also remembers the
/// address at which it was stored remotely.
template <typename Runtime, typename T>
class RemoteRef {
public:
  using StoredPointer = typename Runtime::StoredPointer;

private:
  StoredPointer Address;
  const T *LocalBuffer;

public:
  /*implicit*/
  RemoteRef(std::nullptr_t _)
    : Address(0), LocalBuffer(nullptr) {}

  explicit RemoteRef(StoredPointer address, const T *localBuffer)
    : Address(address), LocalBuffer(localBuffer) {}

  StoredPointer getAddress() const {
    return Address;
  }

  const T *getLocalBuffer() const {
    return LocalBuffer;
  }

  explicit operator bool() const {
    return LocalBuffer != nullptr;
  }

  const T *operator->() const {
    assert(LocalBuffer);
    return LocalBuffer;
  }
};

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
  using BuiltType = typename BuilderType::BuiltType;
  using BuiltNominalTypeDecl = typename BuilderType::BuiltNominalTypeDecl;
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;

private:
  /// A cache of built types, keyed by the address of the type.
  std::unordered_map<StoredPointer, BuiltType> TypeCache;

  using MetadataRef =
    RemoteRef<Runtime, TargetMetadata<Runtime>>;
  using SharedMetadataRef =
    SharedTargetMetadataRef<Runtime>;

  /// A cache of read type metadata, keyed by the address of the metadata.
  std::unordered_map<StoredPointer, SharedMetadataRef> MetadataCache;

  using NominalTypeDescriptorRef =
    RemoteRef<Runtime, TargetNominalTypeDescriptor<Runtime>>;
  using SharedNominalTypeDescriptorRef =
    SharedTargetNominalTypeDescriptorRef<Runtime>;

  /// A cache of read nominal type descriptors, keyed by the address of the
  /// nominal type descriptor.
  std::unordered_map<StoredPointer, SharedNominalTypeDescriptorRef>
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
  BuiltType decodeMangledType(const Demangle::NodePointer &Node) {
    return swift::remote::decodeMangledType(Builder, Node);
  }

  /// Given a remote pointer to metadata, attempt to turn it into a type.
  BuiltType readTypeFromMetadata(StoredPointer MetadataAddress) {
    auto Cached = TypeCache.find(MetadataAddress);
    if (Cached != TypeCache.end())
      return Cached->second;

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta) return BuiltType();

    switch (Meta->getKind()) {
    case MetadataKind::Class:
      return readNominalTypeFromMetadata(Meta);
    case MetadataKind::Struct:
      return readNominalTypeFromMetadata(Meta);
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      return readNominalTypeFromMetadata(Meta);
    case MetadataKind::Tuple: {
      auto TupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta);
      std::vector<BuiltType> Elements;
      StoredPointer ElementAddress = MetadataAddress +
        sizeof(TargetTupleTypeMetadata<Runtime>);
      using Element = typename TargetTupleTypeMetadata<Runtime>::Element;
      for (StoredPointer i = 0; i < TupleMeta->NumElements; ++i,
           ElementAddress += sizeof(Element)) {
        Element E;
        if (!Reader->readBytes(RemoteAddress(ElementAddress),
                               (uint8_t*)&E, sizeof(Element)))
          return BuiltType();

        if (auto ElementTypeRef = readTypeFromMetadata(E.Type))
          Elements.push_back(ElementTypeRef);
        else
          return BuiltType();
      }
      return Builder.createTupleType(Elements, /*variadic*/ false);
    }
    case MetadataKind::Function: {
      auto Function = cast<TargetFunctionTypeMetadata<Runtime>>(Meta);

      std::vector<BuiltType> Arguments;
      std::vector<bool> ArgumentIsInOut;
      StoredPointer ArgumentAddress = MetadataAddress +
        sizeof(TargetFunctionTypeMetadata<Runtime>);
      for (StoredPointer i = 0; i < Function->getNumArguments(); ++i,
           ArgumentAddress += sizeof(StoredPointer)) {
        StoredPointer FlaggedArgumentAddress;
        if (!Reader->readInteger(RemoteAddress(ArgumentAddress),
                                 &FlaggedArgumentAddress))
          return BuiltType();

        // TODO: Use target-agnostic FlaggedPointer to mask this!
        const auto InOutMask = (StoredPointer) 1;
        ArgumentIsInOut.push_back((FlaggedArgumentAddress & InOutMask) != 0);
        FlaggedArgumentAddress &= ~InOutMask;

        if (auto ArgumentTypeRef = readTypeFromMetadata(FlaggedArgumentAddress))
          Arguments.push_back(ArgumentTypeRef);
        else
          return BuiltType();
      }

      auto Result = readTypeFromMetadata(Function->ResultType);
      if (!Result)
        return BuiltType();

      auto flags = FunctionTypeFlags().withConvention(Function->getConvention())
                                      .withThrows(Function->throws());
      return Builder.createFunctionType(Arguments, ArgumentIsInOut,
                                        Result, flags);
    }
    case MetadataKind::Existential: {
      auto Exist = cast<TargetExistentialTypeMetadata<Runtime>>(Meta);
      std::vector<BuiltType> Protocols;
      for (size_t i = 0; i < Exist->Protocols.NumProtocols; ++i) {
        auto ProtocolAddress = Exist->Protocols[i];
        auto ProtocolDescriptor = readProtocolDescriptor(ProtocolAddress);
        if (!ProtocolDescriptor)
          return BuiltType();
        
        std::string MangledName;
        if (!Reader->readString(RemoteAddress(ProtocolDescriptor->Name),
                                MangledName))
          return BuiltType();
        auto Demangled = Demangle::demangleSymbolAsNode(MangledName);
        auto Protocol = decodeMangledType(Demangled);
        if (!Protocol)
          return BuiltType();

        Protocols.push_back(Protocol);
      }
      return Builder.createProtocolCompositionType(Protocols);
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Metatype->InstanceType);
      if (!Instance) return BuiltType();
      return Builder.createMetatypeType(Instance);
    }
    case MetadataKind::ObjCClassWrapper:
      return Builder.getUnnamedObjCClassType();
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Exist->InstanceType);
      if (!Instance) return BuiltType();
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

  /// Given the address of a nominal type descriptor, attempt to resolve
  /// its nominal type declaration.
  BuiltNominalTypeDecl readNominalTypeFromDescriptor(StoredPointer address) {
    auto descriptor = readNominalTypeDescriptor(address);
    if (!descriptor)
      return BuiltNominalTypeDecl();

    auto nameAddress
      = resolveRelativeOffset<int32_t>(address +
                                       descriptor->offsetToNameOffset());
    std::string mangledName;
    if (!Reader->readString(RemoteAddress(nameAddress), mangledName))
      return BuiltNominalTypeDecl();

    BuiltNominalTypeDecl decl =
      Builder.createNominalTypeDecl(std::move(mangledName));
    return decl;
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
  MetadataRef _readMetadata(StoredPointer address, size_t size = sizeof(M)) {
    uint8_t *buffer = (uint8_t *)malloc(size);
    if (!Reader->readBytes(RemoteAddress(address), buffer, size)) {
      free(buffer);
      return nullptr;
    }

    auto metadata = reinterpret_cast<TargetMetadata<Runtime>*>(buffer);
    MetadataCache.insert({address, SharedMetadataRef(metadata, free)});
    return MetadataRef(address, metadata);
  }

  MetadataRef readMetadata(StoredPointer address) {
    auto cached = MetadataCache.find(address);
    if (cached != MetadataCache.end())
      return MetadataRef(address, cached->second.get());

    StoredPointer KindValue = 0;
    if (!Reader->readInteger(RemoteAddress(address), &KindValue))
      return nullptr;

    auto Kind = static_cast<MetadataKind>(KindValue);

    if (metadataKindIsClass(Kind)) {
      return _readMetadata<TargetClassMetadata<Runtime>>(address);
    } else {
      switch (Kind) {
      case MetadataKind::Enum:
        return _readMetadata<TargetEnumMetadata<Runtime>>(address);
      case MetadataKind::ErrorObject:
        return _readMetadata<TargetEnumMetadata<Runtime>>(address);
      case MetadataKind::Existential: {
        StoredPointer NumProtocolsAddress = address +
          TargetExistentialTypeMetadata<Runtime>::OffsetToNumProtocols;
        StoredPointer NumProtocols;
        if (!Reader->readInteger(RemoteAddress(NumProtocolsAddress),
                                 &NumProtocols))
          return nullptr;

        auto TotalSize = sizeof(TargetExistentialTypeMetadata<Runtime>) +
          NumProtocols *
            sizeof(ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor>);
        
        return _readMetadata<TargetExistentialTypeMetadata<Runtime>>(address,
                                                                     TotalSize);
      }
      case MetadataKind::ExistentialMetatype:
        return _readMetadata<
          TargetExistentialMetatypeMetadata<Runtime>>(address);
      case MetadataKind::ForeignClass:
        return _readMetadata<TargetForeignClassMetadata<Runtime>>(address);
      case MetadataKind::Function:
        return _readMetadata<TargetFunctionTypeMetadata<Runtime>>(address);
      case MetadataKind::HeapGenericLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata<Runtime>>(address);
      case MetadataKind::HeapLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata<Runtime>>(address);
      case MetadataKind::Metatype:
        return _readMetadata<TargetMetatypeMetadata<Runtime>>(address);
      case MetadataKind::ObjCClassWrapper:
        return _readMetadata<TargetObjCClassWrapperMetadata<Runtime>>(address);
      case MetadataKind::Opaque:
        return _readMetadata<TargetOpaqueMetadata<Runtime>>(address);
      case MetadataKind::Optional:
        return _readMetadata<TargetEnumMetadata<Runtime>>(address);
      case MetadataKind::Struct:
        return _readMetadata<TargetStructMetadata<Runtime>>(address);
      case MetadataKind::Tuple: {
        auto NumElementsAddress = address +
          TargetTupleTypeMetadata<Runtime>::OffsetToNumElements;
        StoredSize NumElements;
        if (!Reader->readInteger(RemoteAddress(NumElementsAddress),
                                 &NumElements))
          return nullptr;
        auto TotalSize = sizeof(TargetTupleTypeMetadata<Runtime>) +
          NumElements * sizeof(StoredPointer);
        return _readMetadata<TargetTupleTypeMetadata<Runtime>>(address,
                                                               TotalSize);
      }
      default:
        return nullptr;
      }
    }
  }

  NominalTypeDescriptorRef
  readNominalTypeDescriptorFromMetadata(MetadataRef metadata) {
    StoredPointer descriptorAddress;

    switch (metadata->getKind()) {
    case MetadataKind::Class: {
      auto ClassMeta = cast<TargetClassMetadata<Runtime>>(metadata);
      descriptorAddress
        = resolveRelativeOffset<StoredPointer>(metadata.getAddress() +
                                       ClassMeta->offsetToDescriptorOffset());
      break;
    }
    case MetadataKind::Struct: {
      auto StructMeta = cast<TargetStructMetadata<Runtime>>(metadata);
      descriptorAddress
        = resolveRelativeOffset<StoredPointer>(metadata.getAddress() +
                                      StructMeta->offsetToDescriptorOffset());
      break;
    }
    case MetadataKind::Optional:
    case MetadataKind::Enum: {
      auto EnumMeta = cast<TargetEnumMetadata<Runtime>>(metadata);
      descriptorAddress
        = resolveRelativeOffset<StoredPointer>(metadata.getAddress() +
                                        EnumMeta->offsetToDescriptorOffset());
      break;
    }
    default:
      return nullptr;
    }

    return readNominalTypeDescriptor(descriptorAddress);
  }

  /// Given the address of a nominal type descriptor, attempt to read it.
  NominalTypeDescriptorRef
  readNominalTypeDescriptor(StoredPointer address) {
    auto cached = NominalTypeDescriptorCache.find(address);
    if (cached != NominalTypeDescriptorCache.end())
      return NominalTypeDescriptorRef(address, cached->second.get());

    auto size = sizeof(TargetNominalTypeDescriptor<Runtime>);
    auto buffer = (uint8_t *)malloc(size);
    if (!Reader->readBytes(RemoteAddress(address), buffer, size)) {
      free(buffer);
      return nullptr;
    }

    auto descriptor
      = reinterpret_cast<TargetNominalTypeDescriptor<Runtime> *>(buffer);

    NominalTypeDescriptorCache.insert({address,
                            SharedNominalTypeDescriptorRef(descriptor, free)});
    return NominalTypeDescriptorRef(address, descriptor);
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

  bool getParentAddress(MetadataRef metadata,
                        NominalTypeDescriptorRef descriptor,
                        StoredPointer &parentAddress) {
    // If this is metadata for some sort of value type, the parent type
    // is at a fixed offset.
    if (auto valueMetadata = dyn_cast<TargetValueMetadata<Runtime>>(metadata)) {
      parentAddress = valueMetadata->Parent;
      return true;
    }

    // If this is metadata for a class type, the parent type for the
    // most-derived class is at a offset stored in the most-derived
    // nominal type descriptor.
    if (auto classMetadata = dyn_cast<TargetClassMetadata<Runtime>>(metadata)) {
      // The class might not have a parent.
      if (!descriptor->GenericParams.Flags.hasParent()) {
        parentAddress = StoredPointer();
        return true;
      }

      // If it does, it's immediately before the generic parameters.
      auto offsetToParent
        = sizeof(StoredPointer) * (descriptor->GenericParams.Offset - 1);
      RemoteAddress addressOfParent(metadata.getAddress() + offsetToParent);
      return Reader->readInteger(addressOfParent, &parentAddress);
    }

    // Otherwise, assume it just doesn't have a parent.
    // This is still a success.
    parentAddress = StoredPointer();
    return true;
  }

  std::vector<BuiltType> getGenericSubst(MetadataRef metadata) {
    auto descriptor = readNominalTypeDescriptorFromMetadata(metadata);
    std::vector<BuiltType> substitutions;
    auto numGenericParams = descriptor->GenericParams.NumPrimaryParams;
    auto offsetToGenericArgs =
      sizeof(StoredPointer) * (descriptor->GenericParams.Offset);
    auto addressOfGenericArgAddress =
      metadata.getAddress() + offsetToGenericArgs;

    using ArgIndex = decltype(descriptor->GenericParams.NumPrimaryParams);
    for (ArgIndex i = 0; i < numGenericParams;
         ++i, addressOfGenericArgAddress += sizeof(StoredPointer)) {
      StoredPointer genericArgAddress;
      if (!Reader->readInteger(RemoteAddress(addressOfGenericArgAddress),
                               &genericArgAddress))
        return {};
      if (auto genericArg = readTypeFromMetadata(genericArgAddress))
        substitutions.push_back(genericArg);
      else
        return {};
    }
    return substitutions;
  }

  BuiltType readNominalTypeFromMetadata(MetadataRef metadata) {
    auto descriptor = readNominalTypeDescriptorFromMetadata(metadata);
    if (!descriptor)
      return BuiltType();

    auto nameAddress
      = resolveRelativeOffset<int32_t>(descriptor.getAddress() +
                                       descriptor->offsetToNameOffset());
    std::string MangledName;
    if (!Reader->readString(RemoteAddress(nameAddress), MangledName))
      return BuiltType();

    BuiltNominalTypeDecl typeDecl =
      Builder.createNominalTypeDecl(std::move(MangledName));
    if (!typeDecl)
      return BuiltType();

    StoredPointer parentAddress;
    if (!getParentAddress(metadata, descriptor, parentAddress))
      return BuiltType();
    BuiltType parent = BuiltType();
    if (parentAddress) {
      parent = readTypeFromMetadata(parentAddress);
      if (!parent) return BuiltType();
    }

    BuiltType nominal;
    if (descriptor->GenericParams.NumPrimaryParams) {
      auto args = getGenericSubst(metadata);
      if (args.empty()) return BuiltType();
      nominal = Builder.createBoundGenericType(typeDecl, args, parent);
    } else {
      nominal = Builder.createNominalType(typeDecl, parent);
    }
    if (!nominal) return BuiltType();

    TypeCache.insert({metadata.getAddress(), nominal});
    return nominal;
  }
};

} // end namespace remote
} // end namespace swift

namespace llvm {
  template<typename Runtime, typename T>
  struct simplify_type<swift::remote::RemoteRef<Runtime, T>> {
    typedef const T *SimpleType;
    static SimpleType
    getSimplifiedValue(swift::remote::RemoteRef<Runtime, T> value) {
      return value.getLocalBuffer();
    }
  };
}

#endif // SWIFT_REFLECTION_READER_H

