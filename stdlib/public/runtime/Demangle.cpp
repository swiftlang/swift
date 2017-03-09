//===----------------------------------------------------------------------===//
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

#include "../../../lib/Basic/Demangle.cpp"
#include "../../../lib/Basic/Demangler.cpp"
#include "../../../lib/Basic/ManglingUtils.cpp"
#include "../../../lib/Basic/Punycode.cpp"
#include "swift/Runtime/Metadata.h"
#include "Private.h"

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#endif

// FIXME: This stuff should be merged with the existing logic in
// include/swift/Reflection/TypeRefBuilder.h as part of the rewrite
// to change stdlib reflection over to using remote mirrors.

Demangle::NodePointer
swift::_swift_buildDemanglingForMetadata(const Metadata *type,
                                         Demangle::Demangler &Dem);

// Build a demangled type tree for a nominal type.
static Demangle::NodePointer
_buildDemanglingForNominalType(const Metadata *type, Demangle::Demangler &Dem) {
  using namespace Demangle;

  const Metadata *parent;
  Node::Kind boundGenericKind;
  const NominalTypeDescriptor *description;

  // Demangle the parent type, if any.
  switch (type->getKind()) {
  case MetadataKind::Class: {
    auto classType = static_cast<const ClassMetadata *>(type);
#if SWIFT_OBJC_INTEROP
    // Peek through artificial subclasses.
    while (classType->isTypeMetadata() && classType->isArtificialSubclass())
      classType = classType->SuperClass;
#endif
    parent = classType->getParentType(classType->getDescription());
    boundGenericKind = Node::Kind::BoundGenericClass;
    description = classType->getDescription();
    break;
  }
  case MetadataKind::Enum:
  case MetadataKind::Optional: {
    auto enumType = static_cast<const EnumMetadata *>(type);
    parent = enumType->Parent;
    boundGenericKind = Node::Kind::BoundGenericEnum;
    description = enumType->Description;
    break;
  }
  case MetadataKind::Struct: {
    auto structType = static_cast<const StructMetadata *>(type);
    parent = structType->Parent;
    boundGenericKind = Node::Kind::BoundGenericStructure;
    description = structType->Description;
    break;
  }
  default:
    return nullptr;
  }

  // Demangle the base name.
  auto node = Dem.demangleType(StringRef(description->Name));
  assert(node->getKind() == Node::Kind::Type);

  // Demangle the parent.
  if (parent) {
    auto parentNode = _swift_buildDemanglingForMetadata(parent, Dem);
    if (parentNode->getKind() == Node::Kind::Type)
      parentNode = parentNode->getChild(0);

    auto typeNode = node->getChild(0);
    auto newTypeNode = Dem.createNode(typeNode->getKind());
    newTypeNode->addChild(parentNode, Dem);
    newTypeNode->addChild(typeNode->getChild(1), Dem);

    auto newNode = Dem.createNode(Node::Kind::Type);
    newNode->addChild(newTypeNode, Dem);
    node = newNode;
  }

  // If generic, demangle the type parameters.
  if (description->GenericParams.NumPrimaryParams > 0) {
    auto typeParams = Dem.createNode(Node::Kind::TypeList);
    auto typeBytes = reinterpret_cast<const char *>(type);
    auto genericParam = reinterpret_cast<const Metadata * const *>(
                 typeBytes + sizeof(void*) * description->GenericParams.Offset);
    for (unsigned i = 0, e = description->GenericParams.NumPrimaryParams;
         i < e; ++i, ++genericParam) {
      auto demangling = _swift_buildDemanglingForMetadata(*genericParam, Dem);
      if (demangling == nullptr)
        return nullptr;
      typeParams->addChild(demangling, Dem);
    }

    auto genericNode = Dem.createNode(boundGenericKind);
    genericNode->addChild(node, Dem);
    genericNode->addChild(typeParams, Dem);
    return genericNode;
  }
  return node;
}

// Build a demangled type tree for a type.
Demangle::NodePointer
swift::_swift_buildDemanglingForMetadata(const Metadata *type,
                                         Demangle::Demangler &Dem) {
  using namespace Demangle;

  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::Struct:
    return _buildDemanglingForNominalType(type, Dem);
  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto objcWrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    const char *className = class_getName((Class)objcWrapper->Class);
    
    // ObjC classes mangle as being in the magic "__ObjC" module.
    auto module = Dem.createNode(Node::Kind::Module, "__ObjC");
    
    auto node = Dem.createNode(Node::Kind::Class);
    node->addChild(module, Dem);
    node->addChild(Dem.createNode(Node::Kind::Identifier,
                                       llvm::StringRef(className)), Dem);
    
    return node;
#else
    assert(false && "no ObjC interop");
    return nullptr;
#endif
  }
  case MetadataKind::ForeignClass: {
    auto foreign = static_cast<const ForeignClassMetadata *>(type);
    return Dem.demangleType(foreign->getName());
  }
  case MetadataKind::Existential: {
    auto exis = static_cast<const ExistentialTypeMetadata *>(type);
    NodePointer proto_list = Dem.createNode(Node::Kind::ProtocolList);
    NodePointer type_list = Dem.createNode(Node::Kind::TypeList);

    proto_list->addChild(type_list, Dem);
    
    std::vector<const ProtocolDescriptor *> protocols;
    protocols.reserve(exis->Protocols.NumProtocols);
    for (unsigned i = 0, e = exis->Protocols.NumProtocols; i < e; ++i)
      protocols.push_back(exis->Protocols[i]);
    
    // Sort the protocols by their mangled names.
    // The ordering in the existential type metadata is by metadata pointer,
    // which isn't necessarily stable across invocations.
    std::sort(protocols.begin(), protocols.end(),
          [](const ProtocolDescriptor *a, const ProtocolDescriptor *b) -> bool {
            return strcmp(a->Name, b->Name) < 0;
          });
    
    for (auto *protocol : protocols) {
      // The protocol name is mangled as a type symbol, with the _Tt prefix.
      StringRef ProtoName(protocol->Name);
      NodePointer protocolNode = Dem.demangleSymbol(ProtoName);

      // ObjC protocol names aren't mangled.
      if (!protocolNode) {
        auto module = Dem.createNode(Node::Kind::Module,
                                          MANGLING_MODULE_OBJC);
        auto node = Dem.createNode(Node::Kind::Protocol);
        node->addChild(module, Dem);
        node->addChild(Dem.createNode(Node::Kind::Identifier,
                                        llvm::StringRef(protocol->Name)), Dem);
        auto typeNode = Dem.createNode(Node::Kind::Type);
        typeNode->addChild(node, Dem);
        type_list->addChild(typeNode, Dem);
        continue;
      }

      // FIXME: We have to dig through a ridiculous number of nodes to get
      // to the Protocol node here.
      protocolNode = protocolNode->getChild(0); // Global -> TypeMangling
      protocolNode = protocolNode->getChild(0); // TypeMangling -> Type
      protocolNode = protocolNode->getChild(0); // Type -> ProtocolList
      protocolNode = protocolNode->getChild(0); // ProtocolList -> TypeList
      protocolNode = protocolNode->getChild(0); // TypeList -> Type
      
      assert(protocolNode->getKind() == Node::Kind::Type);
      assert(protocolNode->getChild(0)->getKind() == Node::Kind::Protocol);
      type_list->addChild(protocolNode, Dem);
    }
    
    return proto_list;
  }
  case MetadataKind::ExistentialMetatype: {
    auto metatype = static_cast<const ExistentialMetatypeMetadata *>(type);
    auto instance = _swift_buildDemanglingForMetadata(metatype->InstanceType,
                                                      Dem);
    auto node = Dem.createNode(Node::Kind::ExistentialMetatype);
    node->addChild(instance, Dem);
    return node;
  }
  case MetadataKind::Function: {
    auto func = static_cast<const FunctionTypeMetadata *>(type);

    Node::Kind kind;
    switch (func->getConvention()) {
    case FunctionMetadataConvention::Swift:
      kind = Node::Kind::FunctionType;
      break;
    case FunctionMetadataConvention::Block:
      kind = Node::Kind::ObjCBlock;
      break;
    case FunctionMetadataConvention::CFunctionPointer:
      kind = Node::Kind::CFunctionPointer;
      break;
    case FunctionMetadataConvention::Thin:
      kind = Node::Kind::ThinFunctionType;
      break;
    }
    
    std::vector<NodePointer> inputs;
    for (unsigned i = 0, e = func->getNumArguments(); i < e; ++i) {
      auto arg = func->getArguments()[i];
      auto input = _swift_buildDemanglingForMetadata(arg.getPointer(), Dem);
      if (arg.getFlag()) {
        NodePointer inout = Dem.createNode(Node::Kind::InOut);
        inout->addChild(input, Dem);
        input = inout;
      }
      inputs.push_back(input);
    }

    NodePointer totalInput = nullptr;
    if (inputs.size() > 1) {
      auto tuple = Dem.createNode(Node::Kind::NonVariadicTuple);
      for (auto &input : inputs)
        tuple->addChild(input, Dem);
      totalInput = tuple;
    } else {
      totalInput = inputs.front();
    }
    
    NodePointer args = Dem.createNode(Node::Kind::ArgumentTuple);
    args->addChild(totalInput, Dem);
    
    NodePointer resultTy = _swift_buildDemanglingForMetadata(func->ResultType,
                                                             Dem);
    NodePointer result = Dem.createNode(Node::Kind::ReturnType);
    result->addChild(resultTy, Dem);
    
    auto funcNode = Dem.createNode(kind);
    if (func->throws())
      funcNode->addChild(Dem.createNode(Node::Kind::ThrowsAnnotation), Dem);
    funcNode->addChild(args, Dem);
    funcNode->addChild(result, Dem);
    return funcNode;
  }
  case MetadataKind::Metatype: {
    auto metatype = static_cast<const MetatypeMetadata *>(type);
    auto instance = _swift_buildDemanglingForMetadata(metatype->InstanceType,
                                                      Dem);
    auto typeNode = Dem.createNode(Node::Kind::Type);
    typeNode->addChild(instance, Dem);
    auto node = Dem.createNode(Node::Kind::Metatype);
    node->addChild(typeNode, Dem);
    return node;
  }
  case MetadataKind::Tuple: {
    auto tuple = static_cast<const TupleTypeMetadata *>(type);
    const char *labels = tuple->Labels;
    auto tupleNode = Dem.createNode(Node::Kind::NonVariadicTuple);
    for (unsigned i = 0, e = tuple->NumElements; i < e; ++i) {
      auto elt = Dem.createNode(Node::Kind::TupleElement);

      // Add a label child if applicable:
      if (labels) {
        // Look for the next space in the labels string.
        if (const char *space = strchr(labels, ' ')) {
          // If there is one, and the label isn't empty, add a label child.
          if (labels != space) {
            auto eltName =
              Dem.createNode(Node::Kind::TupleElementName,
                                  llvm::StringRef(labels, space - labels));
            elt->addChild(eltName, Dem);
          }

          // Skip past the space.
          labels = space + 1;
        }
      }

      // Add the element type child.
      auto eltType =
        _swift_buildDemanglingForMetadata(tuple->getElement(i).Type, Dem);
      elt->addChild(eltType, Dem);

      // Add the completed element to the tuple.
      tupleNode->addChild(elt, Dem);
    }
    return tupleNode;
  }
  case MetadataKind::Opaque:
    // FIXME: Some opaque types do have manglings, but we don't have enough info
    // to figure them out.
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    break;
  }
  // Not a type.
  return nullptr;
}
