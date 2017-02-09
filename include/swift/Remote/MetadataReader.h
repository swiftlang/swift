//===--- MetadataReader.h - Abstract access to remote metadata --*- C++ -*-===//
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
//  This file defines operations for reading metadata from a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_METADATAREADER_H
#define SWIFT_REMOTE_METADATAREADER_H

#include "swift/Runtime/Metadata.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Basic/Demangle.h"
#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Unreachable.h"

#include <vector>
#include <unordered_map>

namespace swift {
namespace remote {

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
    case NodeKind::Metatype:
    case NodeKind::ExistentialMetatype: {
      unsigned i = 0;
      bool wasAbstract = false;

      // Handle lowered metatypes in a hackish way. If the representation
      // was not thin, force the resulting typeref to have a non-empty
      // representation.
      if (Node->getNumChildren() == 2) {
        auto repr = Node->getChild(i++);
        if (repr->getKind() != NodeKind::MetatypeRepresentation ||
            !repr->hasText())
          return BuiltType();
        auto &str = repr->getText();
        if (str != "@thin")
          wasAbstract = true;
      }
      auto instance = decodeMangledType(Node->getChild(i));
      if (!instance)
        return BuiltType();
      if (Node->getKind() == NodeKind::Metatype) {
        return Builder.createMetatypeType(instance, wasAbstract);
      } else if (Node->getKind() == NodeKind::ExistentialMetatype) {
        // FIXME: Ignore representation of existential metatype
        // completely for now
        return Builder.createExistentialMetatypeType(instance);
      } else {
        assert(false);
        return nullptr;
      }
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

      // Consistent handling of protocols and protocol compositions
      auto protocolList = Demangle::NodeFactory::create(NodeKind::ProtocolList);
      auto typeList = Demangle::NodeFactory::create(NodeKind::TypeList);
      auto type = Demangle::NodeFactory::create(NodeKind::Type);
      type->addChild(Node);
      typeList->addChild(type);
      protocolList->addChild(typeList);

      auto mangledName = Demangle::mangleNode(protocolList);
      return Builder.createProtocolType(mangledName, moduleName, name);
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
    case NodeKind::ImplFunctionType: {
      // Minimal support for lowered function types. These come up in
      // reflection as capture types. For the reflection library's
      // purposes, the only part that matters is the convention.
      FunctionTypeFlags flags;

      for (unsigned i = 0; i < Node->getNumChildren(); i++) {
        auto child = Node->getChild(i);

        if (child->getKind() == NodeKind::ImplConvention) {
          if (!child->hasText())
            return BuiltType();

          auto &text = child->getText();

          if (text == "@convention(thin)") {
            flags =
              flags.withConvention(FunctionMetadataConvention::Thin);
          }
        } else if (child->getKind() == NodeKind::ImplFunctionAttribute) {
          if (!child->hasText())
            return BuiltType();

          auto &text = child->getText();
          if (text == "@convention(c)") {
            flags =
              flags.withConvention(FunctionMetadataConvention::CFunctionPointer);
          } else if (text == "@convention(block)") {
            flags =
              flags.withConvention(FunctionMetadataConvention::Block);
          }
        }
      }

      // Completely punt on argument types and results.
      std::vector<BuiltType> arguments;
      std::vector<bool> argsAreInOut;

      std::vector<BuiltType> elements;
      std::string labels;
      auto result = Builder.createTupleType(elements, std::move(labels), false);

      return Builder.createFunctionType(arguments, argsAreInOut,
                                        result, flags);
    }
    case NodeKind::ArgumentTuple:
      return decodeMangledType(Node->getChild(0));
    case NodeKind::ReturnType:
      return decodeMangledType(Node->getChild(0));
    case NodeKind::NonVariadicTuple:
    case NodeKind::VariadicTuple: {
      std::vector<BuiltType> elements;
      std::string labels;
      for (auto &element : *Node) {
        if (element->getKind() != NodeKind::TupleElement)
          return BuiltType();

        // If the tuple element is labeled, add its label to 'labels'.
        unsigned typeChildIndex = 0;
        if (element->getChild(0)->getKind() == NodeKind::TupleElementName) {
          // Add spaces to terminate all the previous labels if this
          // is the first we've seen.
          if (labels.empty()) labels.append(elements.size(), ' ');

          // Add the label and its terminator.
          labels += element->getChild(0)->getText();
          labels += ' ';
          typeChildIndex = 1;

        // Otherwise, add a space if a previous element had a label.
        } else if (!labels.empty()) {
          labels += ' ';
        }

        // Decode the element type.
        BuiltType elementType =
          decodeMangledType(element->getChild(typeChildIndex));
        if (!elementType)
          return BuiltType();

        elements.push_back(elementType);
      }
      bool variadic = (Node->getKind() == NodeKind::VariadicTuple);
      return Builder.createTupleType(elements, std::move(labels), variadic);
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
    case NodeKind::SILBoxType: {
      auto base = decodeMangledType(Node->getChild(0));
      if (!base)
        return BuiltType();
      return Builder.createSILBoxType(base);
    }
    case NodeKind::SILBoxTypeWithLayout: {
      // TODO: Implement SILBoxTypeRefs with layout. As a stopgap, specify the
      // NativeObject type ref.
      return Builder.createBuiltinType("Bo");
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

/// A structure, designed for use with std::unique_ptr, which destroys
/// a pointer by calling free on it (and not trying to call a destructor).
struct delete_with_free {
  void operator()(const void *memory) {
    free(const_cast<void*>(memory));
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
  using OwnedMetadataRef =
    std::unique_ptr<const TargetMetadata<Runtime>, delete_with_free>;

  /// A cache of read type metadata, keyed by the address of the metadata.
  std::unordered_map<StoredPointer, OwnedMetadataRef>
    MetadataCache;

  using NominalTypeDescriptorRef =
    RemoteRef<Runtime, TargetNominalTypeDescriptor<Runtime>>;
  using OwnedNominalTypeDescriptorRef =
    std::unique_ptr<const TargetNominalTypeDescriptor<Runtime>,
                    delete_with_free>;

  /// A cache of read nominal type descriptors, keyed by the address of the
  /// nominal type descriptor.
  std::unordered_map<StoredPointer, OwnedNominalTypeDescriptorRef>
    NominalTypeDescriptorCache;

  using OwnedProtocolDescriptorRef =
    std::unique_ptr<const TargetProtocolDescriptor<Runtime>, delete_with_free>;

  enum class IsaEncodingKind {
    /// We haven't checked yet.
    Unknown,

    /// There was an error trying to find out the isa encoding.
    Error,

    /// There's no special isa encoding.
    None,

    /// There's an unconditional mask to apply to the isa pointer.
    ///   - IsaMask stores the mask.
    Masked,

    /// Isa pointers are indexed.  If applying a mask yields a magic value,
    /// applying a different mask and shifting yields an index into a global
    /// array of class pointers.  Otherwise, the isa pointer is just a raw
    /// class pointer.
    ///  - IsaIndexMask stores the index mask.
    ///  - IsaIndexShift stores the index shift.
    ///  - IsaMagicMask stores the magic value mask.
    ///  - IsaMagicValue stores the magic value.
    ///  - IndexedClassesPointer stores the pointer to the start of the
    ///    indexed classes array; this is constant throughout the program.
    ///  - IndexedClassesCountPointer stores a pointer to the number
    ///    of elements in the indexed classes array.
    Indexed
  };

  IsaEncodingKind IsaEncoding = IsaEncodingKind::Unknown;
  union {
    StoredPointer IsaMask;
    StoredPointer IsaIndexMask;
  };
  StoredPointer IsaIndexShift;
  StoredPointer IsaMagicMask;
  StoredPointer IsaMagicValue;
  StoredPointer IndexedClassesPointer;
  StoredPointer IndexedClassesCountPointer;
  StoredPointer LastIndexedClassesCount = 0;

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

  /// Get the remote process's swift_isaMask.
  std::pair<bool, StoredPointer> readIsaMask() {
    auto encoding = getIsaEncoding();
    if (encoding != IsaEncodingKind::Masked)
      // Still return success if there's no isa encoding at all.
      return {encoding == IsaEncodingKind::None, 0};

    return {true, IsaMask};
  }

  /// Given a remote pointer to metadata, attempt to discover its MetadataKind.
  std::pair<bool, MetadataKind>
  readKindFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta) return {false, MetadataKind::Opaque};

    return {true, meta->getKind()};
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  StoredPointer
  readSuperClassFromClassMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::Class)
      return StoredPointer();

    auto classMeta = cast<TargetClassMetadata<Runtime>>(meta);
    return classMeta->SuperClass;
  }

  /// Given a remote pointer to class metadata, attempt to discover its class
  /// instance size and whether fields should use the resilient layout strategy.
  std::pair<bool, unsigned>
  readInstanceStartAndAlignmentFromClassMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::Class)
      return std::make_pair(false, 0);

    // The following algorithm only works on the non-fragile Apple runtime.

    // Grab the RO-data pointer.  This part is not ABI.
    StoredPointer roDataPtr = readObjCRODataPtr(MetadataAddress);
    if (!roDataPtr)
      return std::make_pair(false, 0);

    // Get the address of the InstanceStart field.
    auto address = roDataPtr + sizeof(uint32_t) * 1;

    unsigned start;
    if (!Reader->readInteger(RemoteAddress(address), &start))
      return std::make_pair(false, 0);

    return std::make_pair(true, start);
  }

  /// Given a remote pointer to metadata, attempt to turn it into a type.
  BuiltType readTypeFromMetadata(StoredPointer MetadataAddress) {
    auto Cached = TypeCache.find(MetadataAddress);
    if (Cached != TypeCache.end())
      return Cached->second;

    // If we see garbage data in the process of building a BuiltType, and get
    // the same metadata address again, we will hit an infinite loop.
    // Insert a negative result into the cache now so that, if we recur with
    // the same address, we will return the negative result with the check
    // just above.
    TypeCache.insert({MetadataAddress, BuiltType()});

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
      auto tupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta);

      std::vector<BuiltType> elementTypes;
      elementTypes.reserve(tupleMeta->NumElements);

      StoredPointer elementAddress = MetadataAddress +
        sizeof(TargetTupleTypeMetadata<Runtime>);
      using Element = typename TargetTupleTypeMetadata<Runtime>::Element;
      for (StoredPointer i = 0; i < tupleMeta->NumElements; ++i,
           elementAddress += sizeof(Element)) {
        Element element;
        if (!Reader->readBytes(RemoteAddress(elementAddress),
                               (uint8_t*)&element, sizeof(Element)))
          return BuiltType();

        if (auto elementType = readTypeFromMetadata(element.Type))
          elementTypes.push_back(elementType);
        else
          return BuiltType();
      }

      // Read the labels string.
      std::string labels;
      if (tupleMeta->Labels &&
          !Reader->readString(RemoteAddress(tupleMeta->Labels), labels))
        return BuiltType();

      auto BuiltTuple = Builder.createTupleType(elementTypes, std::move(labels),
                                                /*variadic*/ false);
      TypeCache[MetadataAddress] = BuiltTuple;
      return BuiltTuple;
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
      auto BuiltFunction = Builder.createFunctionType(Arguments,
                                                      ArgumentIsInOut,
                                                      Result, flags);
      TypeCache[MetadataAddress] = BuiltFunction;
      return BuiltFunction;
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
      auto BuiltExist = Builder.createProtocolCompositionType(Protocols);
      TypeCache[MetadataAddress] = BuiltExist;
      return BuiltExist;
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Metatype->InstanceType);
      if (!Instance) return BuiltType();
      auto BuiltMetatype = Builder.createMetatypeType(Instance);
      TypeCache[MetadataAddress] = BuiltMetatype;
      return BuiltMetatype;
    }
    case MetadataKind::ObjCClassWrapper: {
      auto objcWrapper = cast<TargetObjCClassWrapperMetadata<Runtime>>(Meta);
      auto classAddress = objcWrapper->Class;

      std::string className;
      if (!readObjCClassName(classAddress, className))
        return BuiltType();

      auto BuiltObjCClass = Builder.createObjCClassType(std::move(className));
      TypeCache[MetadataAddress] = BuiltObjCClass;
      return BuiltObjCClass;
    }
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Exist->InstanceType);
      if (!Instance) return BuiltType();
      auto BuiltExist = Builder.createExistentialMetatypeType(Instance);
      TypeCache[MetadataAddress] = BuiltExist;
      return BuiltExist;
    }
    case MetadataKind::ForeignClass: {
      auto namePtrAddress =
        Meta.getAddress() + TargetForeignClassMetadata<Runtime>::OffsetToName;
      StoredPointer namePtr;
      if (!Reader->readInteger(RemoteAddress(namePtrAddress), &namePtr) ||
          namePtr == 0)
        return BuiltType();
      std::string name;
      if (!Reader->readString(RemoteAddress(namePtr), name))
        return BuiltType();
      auto BuiltForeign = Builder.createForeignClassType(std::move(name));
      TypeCache[MetadataAddress] = BuiltForeign;
      return BuiltForeign;
    }
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
      // Treat these all as Builtin.NativeObject for type lowering purposes.
      return Builder.createBuiltinType("Bo");
    case MetadataKind::Opaque: {
      auto BuiltOpaque = Builder.getOpaqueType();
      TypeCache[MetadataAddress] = BuiltOpaque;
      return BuiltOpaque;
    }
    }

    swift_runtime_unreachable("Unhandled MetadataKind in switch");
  }

  BuiltType readTypeFromMangledName(const char *MangledTypeName,
                                    size_t Length) {
    auto Demangled = Demangle::demangleSymbolAsNode(MangledTypeName, Length);
    return decodeMangledType(Demangled);
  }

  /// Read the isa pointer of a class or closure context instance and apply
  /// the isa mask.
  std::pair<bool, StoredPointer>
  readMetadataFromInstance(StoredPointer objectAddress) {
    StoredPointer isa;
    if (!Reader->readInteger(RemoteAddress(objectAddress), &isa))
      return {false, 0};

    switch (getIsaEncoding()) {
    case IsaEncodingKind::Unknown:
    case IsaEncodingKind::Error:
      return {false, 0};

    case IsaEncodingKind::None:
      return {true, isa};

    case IsaEncodingKind::Masked:
      return {true, isa & IsaMask};

    case IsaEncodingKind::Indexed: {
      // If applying the magic mask doesn't give us the magic value,
      // it's not an indexed isa.
      if ((isa & IsaMagicMask) != IsaMagicValue)
        return {true, isa};

      // Extract the index.
      auto classIndex = (isa & IsaIndexMask) >> IsaIndexShift;

      // 0 is never a valid index.
      if (classIndex == 0) {
        return {false, 0};

      // If the index is out of range, it's an error; but check for an
      // update first.  (This will also trigger the first time because
      // we initialize LastIndexedClassesCount to 0).
      } else if (classIndex >= LastIndexedClassesCount) {
        StoredPointer count;
        if (!Reader->readInteger(RemoteAddress(IndexedClassesCountPointer),
                                 &count)) {
          return {false, 0};
        }

        LastIndexedClassesCount = count;
        if (classIndex >= count) {
          return {false, 0};
        }
      }

      // Find the address of the appropriate array element.
      RemoteAddress eltPointer =
        RemoteAddress(IndexedClassesPointer
                        + classIndex * sizeof(StoredPointer));
      StoredPointer metadataPointer;
      if (!Reader->readInteger(eltPointer, &metadataPointer)) {
        return {false, 0};
      }

      return {true, metadataPointer};
    }
    }
  }

  /// Read the parent type metadata from a nested nominal type metadata.
  std::pair<bool, StoredPointer>
  readParentFromMetadata(StoredPointer metadata) {
    auto Meta = readMetadata(metadata);
    if (!Meta)
      return std::make_pair(false, 0);

    auto descriptorAddress = readAddressOfNominalTypeDescriptor(Meta);
    if (!descriptorAddress)
      return std::make_pair(false, 0);

    // Read the nominal type descriptor.
    auto descriptor = readNominalTypeDescriptor(descriptorAddress);
    if (!descriptor)
      return std::make_pair(false, 0);

    // Read the parent type if the type has one.
    if (descriptor->GenericParams.Flags.hasParent()) {
      StoredPointer parentAddress = getNominalParent(Meta, descriptor);
      if (!parentAddress)
        return std::make_pair(false, 0);
      return std::make_pair(true, parentAddress);
    }

    return std::make_pair(false, 0);
  }

  /// Read a single generic type argument from a bound generic type
  /// metadata.
  std::pair<bool, StoredPointer>
  readGenericArgFromMetadata(StoredPointer metadata, unsigned index) {
    auto Meta = readMetadata(metadata);
    if (!Meta)
      return std::make_pair(false, 0);

    auto descriptorAddress = readAddressOfNominalTypeDescriptor(Meta);
    if (!descriptorAddress)
      return std::make_pair(false, 0);

    // Read the nominal type descriptor.
    auto descriptor = readNominalTypeDescriptor(descriptorAddress);
    if (!descriptor)
      return std::make_pair(false, 0);

    auto numGenericParams = descriptor->GenericParams.NumPrimaryParams;
    auto offsetToGenericArgs =
      sizeof(StoredPointer) * (descriptor->GenericParams.Offset);
    auto addressOfGenericArgAddress =
      Meta.getAddress() + offsetToGenericArgs +
      index * sizeof(StoredPointer);

    if (index >= numGenericParams)
      return std::make_pair(false, 0);

    StoredPointer genericArgAddress;
    if (!Reader->readInteger(RemoteAddress(addressOfGenericArgAddress),
                             &genericArgAddress))
      return std::make_pair(false, 0);

    return std::make_pair(true, genericArgAddress);
  }

  /// Given the address of a nominal type descriptor, attempt to resolve
  /// its nominal type declaration.
  BuiltNominalTypeDecl readNominalTypeFromDescriptor(StoredPointer address) {
    auto descriptor = readNominalTypeDescriptor(address);
    if (!descriptor)
      return BuiltNominalTypeDecl();

    return buildNominalTypeDecl(descriptor);
  }

  /// Try to read the offset of a tuple element from a tuple metadata.
  bool readTupleElementOffset(StoredPointer metadataAddress, unsigned eltIndex,
                              StoredSize *offset) {
    // Read the metadata.
    auto metadata = readMetadata(metadataAddress);
    if (!metadata)
      return false;

    // Ensure that the metadata actually is tuple metadata.
    auto tupleMetadata = dyn_cast<TargetTupleTypeMetadata<Runtime>>(metadata);
    if (!tupleMetadata)
      return false;

    // Ensure that the element is in-bounds.
    if (eltIndex >= tupleMetadata->NumElements)
      return false;

    // Read the offset.
    const auto &element = tupleMetadata->getElement(eltIndex);
    *offset = element.Offset;
    return true;
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  std::pair<bool, StoredPointer>
  readOffsetToFirstCaptureFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::HeapLocalVariable)
      return std::make_pair(false, 0);

    auto heapMeta = cast<TargetHeapLocalVariableMetadata<Runtime>>(meta);
    return std::make_pair(true, heapMeta->OffsetToFirstCapture);
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  std::pair<bool, StoredPointer>
  readCaptureDescriptorFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::HeapLocalVariable)
      return std::make_pair(false, 0);

    auto heapMeta = cast<TargetHeapLocalVariableMetadata<Runtime>>(meta);
    return std::make_pair(true, heapMeta->CaptureDescription);
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

  /// Given a pointer to an Objective-C class, try to read its class name.
  bool readObjCClassName(StoredPointer classAddress, std::string &className) {
    // The following algorithm only works on the non-fragile Apple runtime.

    // Grab the RO-data pointer.  This part is not ABI.
    StoredPointer roDataPtr = readObjCRODataPtr(classAddress);
    if (!roDataPtr) return false;

    // This is ABI.
    static constexpr auto OffsetToName =
    roundUpToAlignment(size_t(12), sizeof(StoredPointer))
      + sizeof(StoredPointer);

    // Read the name pointer.
    StoredPointer namePtr;
    if (!Reader->readInteger(RemoteAddress(roDataPtr + OffsetToName), &namePtr))
      return false;

    // If the name pointer is null, treat that as an error.
    if (!namePtr)
      return false;

    return Reader->readString(RemoteAddress(namePtr), className);
  }

  MetadataRef readMetadata(StoredPointer address) {
    auto cached = MetadataCache.find(address);
    if (cached != MetadataCache.end())
      return MetadataRef(address, cached->second.get());

    StoredPointer KindValue = 0;
    if (!Reader->readInteger(RemoteAddress(address), &KindValue))
      return nullptr;

    switch (getEnumeratedMetadataKind(KindValue)) {
      case MetadataKind::Class:
        return _readMetadata<TargetClassMetadata>(address);
      case MetadataKind::Enum:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::ErrorObject:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::Existential: {
        StoredPointer numProtocolsAddress = address +
          TargetExistentialTypeMetadata<Runtime>::OffsetToNumProtocols;
        StoredPointer numProtocols;
        if (!Reader->readInteger(RemoteAddress(numProtocolsAddress),
                                 &numProtocols))
          return nullptr;

        // Make sure the number of protocols is reasonable
        if (numProtocols >= 256)
          return nullptr;

        auto totalSize = sizeof(TargetExistentialTypeMetadata<Runtime>)
          + numProtocols *
          sizeof(ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor>);

        return _readMetadata(address, totalSize);
      }
      case MetadataKind::ExistentialMetatype:
        return _readMetadata<TargetExistentialMetatypeMetadata>(address);
      case MetadataKind::ForeignClass:
        return _readMetadata<TargetForeignClassMetadata>(address);
      case MetadataKind::Function:
        return _readMetadata<TargetFunctionTypeMetadata>(address);
      case MetadataKind::HeapGenericLocalVariable:
        return _readMetadata<TargetGenericBoxHeapMetadata>(address);
      case MetadataKind::HeapLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata>(address);
      case MetadataKind::Metatype:
        return _readMetadata<TargetMetatypeMetadata>(address);
      case MetadataKind::ObjCClassWrapper:
        return _readMetadata<TargetObjCClassWrapperMetadata>(address);
      case MetadataKind::Opaque:
        return _readMetadata<TargetOpaqueMetadata>(address);
      case MetadataKind::Optional:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::Struct:
        return _readMetadata<TargetStructMetadata>(address);
      case MetadataKind::Tuple: {
        auto numElementsAddress = address +
          TargetTupleTypeMetadata<Runtime>::OffsetToNumElements;
        StoredSize numElements;
        if (!Reader->readInteger(RemoteAddress(numElementsAddress),
                                 &numElements))
          return nullptr;
        auto totalSize = sizeof(TargetTupleTypeMetadata<Runtime>)
          + numElements * sizeof(StoredPointer);

        // Make sure the number of elements is reasonable
        if (numElements >= 256)
          return nullptr;

        return _readMetadata(address, totalSize);
      }
    }

    // We can fall out here if the value wasn't actually a valid
    // MetadataKind.
    return nullptr;
  }

private:
  template <template <class R> class M>
  MetadataRef _readMetadata(StoredPointer address) {
    return _readMetadata(address, sizeof(M<Runtime>));
  }

  MetadataRef _readMetadata(StoredPointer address, size_t sizeAfter) {
    auto size = sizeAfter;
    uint8_t *buffer = (uint8_t *) malloc(size);
    if (!Reader->readBytes(RemoteAddress(address), buffer, size)) {
      free(buffer);
      return nullptr;
    }

    auto metadata = reinterpret_cast<TargetMetadata<Runtime>*>(buffer);
    MetadataCache.insert(std::make_pair(address, OwnedMetadataRef(metadata)));
    return MetadataRef(address, metadata);
  }

  StoredPointer readAddressOfNominalTypeDescriptor(MetadataRef metadata) {
    switch (metadata->getKind()) {
    case MetadataKind::Class: {
      auto classMeta = cast<TargetClassMetadata<Runtime>>(metadata);
      return resolveRelativeOffset<StoredPointer>(metadata.getAddress() +
                                       classMeta->offsetToDescriptorOffset());
    }

    case MetadataKind::Struct:
    case MetadataKind::Optional:
    case MetadataKind::Enum: {
      auto valueMeta = cast<TargetValueMetadata<Runtime>>(metadata);
      return resolveRelativeOffset<StoredPointer>(metadata.getAddress() +
                                       valueMeta->offsetToDescriptorOffset());
    }

    default:
      return 0;
    }
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

    NominalTypeDescriptorCache.insert(
      std::make_pair(address, OwnedNominalTypeDescriptorRef(descriptor)));
    return NominalTypeDescriptorRef(address, descriptor);
  }

  /// Given a read nominal type descriptor, attempt to build a
  /// nominal type decl from it.
  BuiltNominalTypeDecl
  buildNominalTypeDecl(NominalTypeDescriptorRef descriptor) {
    auto nameAddress
      = resolveRelativeOffset<int32_t>(descriptor.getAddress() +
                                       descriptor->offsetToNameOffset());
    std::string mangledName;
    if (!Reader->readString(RemoteAddress(nameAddress), mangledName))
      return BuiltNominalTypeDecl();

    BuiltNominalTypeDecl decl =
      Builder.createNominalTypeDecl(std::move(mangledName));
    return decl;
  }

  OwnedProtocolDescriptorRef
  readProtocolDescriptor(StoredPointer Address) {
    auto Size = sizeof(TargetProtocolDescriptor<Runtime>);
    auto Buffer = (uint8_t *)malloc(Size);
    if (!Reader->readBytes(RemoteAddress(Address), Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }
    auto Casted
      = reinterpret_cast<TargetProtocolDescriptor<Runtime> *>(Buffer);
    return OwnedProtocolDescriptorRef(Casted);
  }

  StoredPointer getNominalParent(MetadataRef metadata,
                                 NominalTypeDescriptorRef descriptor) {
    // If this is metadata for some sort of value type, the parent type
    // is at a fixed offset.
    if (auto valueMetadata = dyn_cast<TargetValueMetadata<Runtime>>(metadata)) {
      return valueMetadata->Parent;
    }

    // If this is metadata for a class type, the parent type for the
    // most-derived class is at an offset stored in the most-derived
    // nominal type descriptor.
    if (auto classMetadata = dyn_cast<TargetClassMetadata<Runtime>>(metadata)) {
      // If it does, it's immediately before the generic parameters.
      auto offsetToParent
        = sizeof(StoredPointer) * (descriptor->GenericParams.Offset - 1);
      RemoteAddress addressOfParent(metadata.getAddress() + offsetToParent);
      StoredPointer parentAddress;
      if (!Reader->readInteger(addressOfParent, &parentAddress))
        return StoredPointer();
      return parentAddress;
    }

    // Otherwise, we don't know how to access its parent.  This is a failure.
    return StoredPointer();
  }

  std::vector<BuiltType>
  getGenericSubst(MetadataRef metadata, NominalTypeDescriptorRef descriptor) {
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
    auto descriptorAddress = readAddressOfNominalTypeDescriptor(metadata);
    if (!descriptorAddress)
      return BuiltType();

    // Read the nominal type descriptor.
    auto descriptor = readNominalTypeDescriptor(descriptorAddress);
    if (!descriptor)
      return BuiltType();

    // From that, attempt to resolve a nominal type.
    BuiltNominalTypeDecl typeDecl = buildNominalTypeDecl(descriptor);
    if (!typeDecl)
      return BuiltType();

    // Read the parent type if the type has one.
    BuiltType parent = BuiltType();
    if (descriptor->GenericParams.Flags.hasParent()) {
      StoredPointer parentAddress = getNominalParent(metadata, descriptor);
      if (!parentAddress)
        return BuiltType();
      parent = readTypeFromMetadata(parentAddress);
      if (!parent) return BuiltType();
    }

    BuiltType nominal;
    if (descriptor->GenericParams.NumPrimaryParams) {
      auto args = getGenericSubst(metadata, descriptor);
      if (args.empty()) return BuiltType();
      nominal = Builder.createBoundGenericType(typeDecl, args, parent);
    } else {
      nominal = Builder.createNominalType(typeDecl, parent);
    }
    if (!nominal) return BuiltType();

    TypeCache[metadata.getAddress()] = nominal;
    return nominal;
  }

  /// Given that the remote process is running the non-fragile Apple runtime,
  /// grab the ro-data from a class pointer.
  StoredPointer readObjCRODataPtr(StoredPointer classAddress) {
    // WARNING: the following algorithm works on current modern Apple
    // runtimes but is not actually ABI.  But it is pretty reliable.

    StoredPointer dataPtr;
    if (!Reader->readInteger(RemoteAddress(classAddress +
                               TargetClassMetadata<Runtime>::offsetToData()),
                             &dataPtr))
      return StoredPointer();

    // Apply the data-pointer mask.
    // These values have been stolen from the runtime source.
    static constexpr uint64_t DataPtrMask =
      (Runtime::PointerSize == 8 ? 0x00007ffffffffff8ULL : 0xfffffffcULL);
    dataPtr &= StoredPointer(DataPtrMask);
    if (!dataPtr)
      return StoredPointer();

    // Read the flags, which is a 32-bit header on both formats.
    uint32_t flags;
    if (!Reader->readInteger(RemoteAddress(dataPtr), &flags))
      return StoredPointer();

    // If the type is not realized, this is the RO-data.
    static constexpr uint32_t RO_REALIZED = 0x80000000U;
    if (!(flags & RO_REALIZED))
      return dataPtr;

    // Otherwise, it's the RW-data; read the RO-data pointer from a
    // well-known position within the RW-data.
    static constexpr uint32_t OffsetToROPtr = 8;
    if (!Reader->readInteger(RemoteAddress(dataPtr + OffsetToROPtr), &dataPtr))
      return StoredPointer();

    return dataPtr;
  }

  IsaEncodingKind getIsaEncoding() {
    if (IsaEncoding != IsaEncodingKind::Unknown)
      return IsaEncoding;

    auto finish = [&](IsaEncodingKind result) -> IsaEncodingKind {
      IsaEncoding = result;
      return result;
    };

    /// Look up the given global symbol and bind 'varname' to its
    /// address if its exists.
#   define tryFindSymbol(varname, symbolName)                \
      auto varname = Reader->getSymbolAddress(symbolName);   \
      if (!varname)                                          \
        return finish(IsaEncodingKind::Error)
    /// Read from the given pointer into 'dest'.
#   define tryReadSymbol(varname, dest) do {                 \
      if (!Reader->readInteger(varname, &dest))              \
        return finish(IsaEncodingKind::Error);               \
    } while (0)
    /// Read from the given global symbol into 'dest'.
#   define tryFindAndReadSymbol(dest, symbolName) do {       \
      tryFindSymbol(_address, symbolName);                    \
      tryReadSymbol(_address, dest);                          \
    } while (0)

    // Check for the magic-mask symbol that indicates that the ObjC
    // runtime is using indexed ISAs.
    if (auto magicMaskAddress =
          Reader->getSymbolAddress("objc_debug_indexed_isa_magic_mask")) {
      tryReadSymbol(magicMaskAddress, IsaMagicMask);
      if (IsaMagicMask != 0) {
        tryFindAndReadSymbol(IsaMagicValue,
                             "objc_debug_indexed_isa_magic_value");
        tryFindAndReadSymbol(IsaIndexMask,
                             "objc_debug_indexed_isa_index_mask");
        tryFindAndReadSymbol(IsaIndexShift,
                             "objc_debug_indexed_isa_index_shift");
        tryFindSymbol(indexedClasses, "objc_indexed_classes");
        IndexedClassesPointer = indexedClasses.getAddressData();
        tryFindSymbol(indexedClassesCount, "objc_indexed_classes_count");
        IndexedClassesCountPointer = indexedClassesCount.getAddressData();

        return finish(IsaEncodingKind::Indexed);
      }
    }

    // Check for the ISA mask symbol.  This has to come second because
    // the standard library will define this even if the ObjC runtime
    // doesn't use it.
    if (auto maskAddress = Reader->getSymbolAddress("swift_isaMask")) {
      tryReadSymbol(maskAddress, IsaMask);
      if (IsaMask != 0) {
        return finish(IsaEncodingKind::Masked);
      }
    }

    return finish(IsaEncodingKind::None);
  }

  template <class T>
  static constexpr T roundUpToAlignment(T offset, T alignment) {
    return (offset + alignment - 1) & ~(alignment - 1);
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
