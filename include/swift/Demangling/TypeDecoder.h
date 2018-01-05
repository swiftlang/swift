//===--- TypeDecoder.h - Decode mangled type names --------------*- C++ -*-===//
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
// This file provides facilities to \c TypeDecoder, which decodes a mangled
// type name into a structured type representation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEMANGLING_TYPEDECODER_H
#define SWIFT_DEMANGLING_TYPEDECODER_H

#include "swift/ABI/MetadataValues.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Unreachable.h"

namespace swift {
namespace Demangle {

/// Describe a function parameter, parameterized on the type
/// representation.
template <typename BuiltType>
class FunctionParam {
  StringRef Label;
  BuiltType Type;
  ParameterFlags Flags;

  FunctionParam(StringRef label, BuiltType type, ParameterFlags flags)
      : Label(label), Type(type), Flags(flags) {}

public:
  explicit FunctionParam() {}

  FunctionParam(BuiltType type) : Type(type) {}

  StringRef getLabel() const { return Label; }
  BuiltType getType() const { return Type; }
  ParameterFlags getFlags() const { return Flags; }

  void setLabel(StringRef label) { Label = label; }
  void setType(BuiltType type) { Type = type; }

  void setVariadic() { Flags = Flags.withVariadic(true); }
  void setShared() { Flags = Flags.withShared(true); }
  void setInOut() { Flags = Flags.withInOut(true); }
  void setFlags(ParameterFlags flags) { Flags = flags; };

  FunctionParam withLabel(StringRef label) const {
    return FunctionParam(label, Type, Flags);
  }

  FunctionParam withType(BuiltType type) const {
    return FunctionParam(Label, type, Flags);
  }

  FunctionParam withFlags(ParameterFlags flags) const {
    return FunctionParam(Label, Type, flags);
  }
};

/// Decode a mangled type to construct an abstract type, forming such
/// types by invoking a custom builder.
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
        if (repr->getText() != "@thin")
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
      return Builder.createProtocolCompositionType(
          protocols,
          /*hasExplicitAnyObject=*/false);
    }
    case NodeKind::ProtocolListWithAnyObject: {
      std::vector<BuiltType> protocols;
      auto ProtocolList = Node->getChild(0);
      auto TypeList = ProtocolList->getChild(0);
      for (auto componentType : *TypeList) {
        if (auto protocol = decodeMangledType(componentType))
          protocols.push_back(protocol);
        else
          return BuiltType();
      }
      return Builder.createProtocolCompositionType(
          protocols,
          /*hasExplicitAnyObject=*/true);
    }
    case NodeKind::ProtocolListWithClass: {
      std::vector<BuiltType> members;
      auto ProtocolList = Node->getChild(0);
      auto TypeList = ProtocolList->getChild(0);
      for (auto componentType : *TypeList) {
        if (auto protocol = decodeMangledType(componentType))
          members.push_back(protocol);
        else
          return BuiltType();
      }

      auto SuperclassNode = Node->getChild(1);
      if (auto superclass = decodeMangledType(SuperclassNode))
        members.push_back(superclass);

      return Builder.createProtocolCompositionType(
          members,
          /*hasExplicitAnyObject=*/true);
    }
    case NodeKind::Protocol: {
      auto moduleName = Node->getChild(0)->getText();
      auto nameNode = Node->getChild(1);
      std::string privateDiscriminator, name;
      if (nameNode->getKind() == NodeKind::PrivateDeclName) {
        privateDiscriminator = nameNode->getChild(0)->getText();
        name = nameNode->getChild(1)->getText();
      } else if (nameNode->getKind() == NodeKind::Identifier) {
        name = Node->getChild(1)->getText();
      } else {
        return BuiltType();
      }

      // Consistent handling of protocols and protocol compositions
      Demangle::Demangler Dem;
      auto protocolList = Dem.createNode(NodeKind::ProtocolList);
      auto typeList = Dem.createNode(NodeKind::TypeList);
      auto type = Dem.createNode(NodeKind::Type);
      type->addChild(Node, Dem);
      typeList->addChild(type, Dem);
      protocolList->addChild(typeList, Dem);

      auto mangledName = Demangle::mangleNode(protocolList);
      return Builder.createProtocolType(mangledName, moduleName,
                                        privateDiscriminator, name);
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

      std::vector<FunctionParam<BuiltType>> parameters;
      if (!decodeMangledFunctionInputType(Node->getChild(isThrow ? 1 : 0),
                                          parameters))
        return BuiltType();

      auto result = decodeMangledType(Node->getChild(isThrow ? 2 : 1));
      if (!result) return BuiltType();
      return Builder.createFunctionType(parameters, result, flags);
    }
    case NodeKind::ImplFunctionType: {
      // Minimal support for lowered function types. These come up in
      // reflection as capture types. For the reflection library's
      // purposes, the only part that matters is the convention.
      //
      // TODO: Do we want to reflect @escaping?
      FunctionTypeFlags flags;

      for (unsigned i = 0; i < Node->getNumChildren(); i++) {
        auto child = Node->getChild(i);

        if (child->getKind() == NodeKind::ImplConvention) {
          if (!child->hasText())
            return BuiltType();

          if (child->getText() == "@convention(thin)") {
            flags =
              flags.withConvention(FunctionMetadataConvention::Thin);
          }
        } else if (child->getKind() == NodeKind::ImplFunctionAttribute) {
          if (!child->hasText())
            return BuiltType();

          StringRef text = child->getText();
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
      std::vector<FunctionParam<BuiltType>> parameters;

      std::vector<BuiltType> elements;
      std::string labels;
      auto result = Builder.createTupleType(elements, std::move(labels), false);

      return Builder.createFunctionType(parameters, result, flags);
    }
    case NodeKind::ArgumentTuple:
      return decodeMangledType(Node->getChild(0));
    case NodeKind::ReturnType:
      return decodeMangledType(Node->getChild(0));
    case NodeKind::Tuple: {
      std::vector<BuiltType> elements;
      std::string labels;
      bool variadic = false;
      for (auto &element : *Node) {
        if (element->getKind() != NodeKind::TupleElement)
          return BuiltType();

        // If the tuple element is labeled, add its label to 'labels'.
        unsigned typeChildIndex = 0;
        unsigned nameIdx = 0;
        if (element->getChild(nameIdx)->getKind() == NodeKind::VariadicMarker) {
          variadic = true;
          nameIdx = 1;
          typeChildIndex = 1;
        }
        if (element->getChild(nameIdx)->getKind() == NodeKind::TupleElementName) {
          // Add spaces to terminate all the previous labels if this
          // is the first we've seen.
          if (labels.empty()) labels.append(elements.size(), ' ');

          // Add the label and its terminator.
          labels += element->getChild(0)->getText();
          labels += ' ';
          typeChildIndex++;

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

  bool decodeMangledFunctionInputType(
      const Demangle::NodePointer &node,
      std::vector<FunctionParam<BuiltType>> &params) {
    // Look through a couple of sugar nodes.
    if (node->getKind() == NodeKind::Type ||
        node->getKind() == NodeKind::ArgumentTuple) {
      return decodeMangledFunctionInputType(node->getFirstChild(), params);
    }

    auto decodeParamTypeAndFlags =
        [&](const Demangle::NodePointer &typeNode,
            FunctionParam<BuiltType> &param) -> bool {
      Demangle::NodePointer node = typeNode;
      switch (node->getKind()) {
      case NodeKind::InOut:
        param.setInOut();
        node = node->getFirstChild();
        break;

      case NodeKind::Shared:
        param.setShared();
        node = node->getFirstChild();
        break;

      default:
        break;
      }

      auto paramType = decodeMangledType(node);
      if (!paramType)
        return false;

      param.setType(paramType);
      return true;
    };

    auto decodeParam = [&](const Demangle::NodePointer &paramNode)
        -> Optional<FunctionParam<BuiltType>> {
      if (paramNode->getKind() != NodeKind::TupleElement)
        return None;

      FunctionParam<BuiltType> param;
      for (const auto &child : *paramNode) {
        switch (child->getKind()) {
        case NodeKind::TupleElementName:
          param.setLabel(child->getText());
          break;

        case NodeKind::VariadicMarker:
          param.setVariadic();
          break;

        case NodeKind::Type:
          if (!decodeParamTypeAndFlags(child->getFirstChild(), param))
            return None;
          break;

        default:
          return None;
        }
      }

      return param;
    };

    // Expand a single level of tuple.
    if (node->getKind() == NodeKind::Tuple) {
      // Decode all the elements as separate arguments.
      for (const auto &elt : *node) {
        auto param = decodeParam(elt);
        if (!param)
          return false;

        params.push_back(std::move(*param));
      }

      return true;
    }

    // Otherwise, handle the type as a single argument.
    FunctionParam<BuiltType> param;
    if (!decodeParamTypeAndFlags(node, param))
      return false;

    params.push_back(std::move(param));
    return true;
  }
};

template<typename BuilderType>
inline typename BuilderType::BuiltType
decodeMangledType(BuilderType &Builder,
                  const Demangle::NodePointer &Node) {
  return TypeDecoder<BuilderType>(Builder).decodeMangledType(Node);
}


} // end namespace Demangle
} // end namespace swift

#endif // SWIFT_DEMANGLING_TYPEDECODER_H
