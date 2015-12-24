#include "../../../lib/Basic/Demangle.cpp"
#include "../../../lib/Basic/Punycode.cpp"
#include "swift/Runtime/Metadata.h"
#include "Private.h"

#if SWIFT_OBJC_INTEROP

#include <objc/runtime.h>

// Build a demangled type tree for a nominal type.
static Demangle::NodePointer
_buildDemanglingForNominalType(Demangle::Node::Kind boundGenericKind,
                               const Metadata *type,
                               const NominalTypeDescriptor *description) {
  using namespace Demangle;
  
  // Demangle the base name.
  auto node = demangleTypeAsNode(description->Name,
                                     strlen(description->Name));
  // If generic, demangle the type parameters.
  if (description->GenericParams.NumPrimaryParams > 0) {
    auto typeParams = NodeFactory::create(Node::Kind::TypeList);
    auto typeBytes = reinterpret_cast<const char *>(type);
    auto genericParam = reinterpret_cast<const Metadata * const *>(
                 typeBytes + sizeof(void*) * description->GenericParams.Offset);
    for (unsigned i = 0, e = description->GenericParams.NumPrimaryParams;
         i < e; ++i, ++genericParam) {
      typeParams->addChild(_swift_buildDemanglingForMetadata(*genericParam));
    }

    auto genericNode = NodeFactory::create(boundGenericKind);
    genericNode->addChild(node);
    genericNode->addChild(typeParams);
    return genericNode;
  }
  return node;
}

// Build a demangled type tree for a type.
Demangle::NodePointer swift::_swift_buildDemanglingForMetadata(const Metadata *type) {
  using namespace Demangle;

  switch (type->getKind()) {
  case MetadataKind::Class: {
    auto classType = static_cast<const ClassMetadata *>(type);
    return _buildDemanglingForNominalType(Node::Kind::BoundGenericClass,
                                          type, classType->getDescription());
  }
  case MetadataKind::Enum:
  case MetadataKind::Optional: {
    auto structType = static_cast<const EnumMetadata *>(type);
    return _buildDemanglingForNominalType(Node::Kind::BoundGenericEnum,
                                          type, structType->Description);
  }
  case MetadataKind::Struct: {
    auto structType = static_cast<const StructMetadata *>(type);
    return _buildDemanglingForNominalType(Node::Kind::BoundGenericStructure,
                                          type, structType->Description);
  }
  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto objcWrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    const char *className = class_getName((Class)objcWrapper->Class);
    
    // ObjC classes mangle as being in the magic "__ObjC" module.
    auto module = NodeFactory::create(Node::Kind::Module, "__ObjC");
    
    auto node = NodeFactory::create(Node::Kind::Class);
    node->addChild(module);
    node->addChild(NodeFactory::create(Node::Kind::Identifier,
                                       llvm::StringRef(className)));
    
    return node;
#else
    assert(false && "no ObjC interop");
    return nullptr;
#endif
  }
  case MetadataKind::ForeignClass: {
    auto foreign = static_cast<const ForeignClassMetadata *>(type);
    return Demangle::demangleTypeAsNode(foreign->getName(),
                                        strlen(foreign->getName()));
  }
  case MetadataKind::Existential: {
    auto exis = static_cast<const ExistentialTypeMetadata *>(type);
    NodePointer proto_list = NodeFactory::create(Node::Kind::ProtocolList);
    NodePointer type_list = NodeFactory::create(Node::Kind::TypeList);

    proto_list->addChild(type_list);
    
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
      auto protocolNode = demangleSymbolAsNode(protocol->Name,
                                               strlen(protocol->Name));
      
      // ObjC protocol names aren't mangled.
      if (!protocolNode) {
        auto module = NodeFactory::create(Node::Kind::Module,
                                          MANGLING_MODULE_OBJC);
        auto node = NodeFactory::create(Node::Kind::Protocol);
        node->addChild(module);
        node->addChild(NodeFactory::create(Node::Kind::Identifier,
                                           llvm::StringRef(protocol->Name)));
        auto typeNode = NodeFactory::create(Node::Kind::Type);
        typeNode->addChild(node);
        type_list->addChild(typeNode);
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
      type_list->addChild(protocolNode);
    }
    
    return proto_list;
  }
  case MetadataKind::ExistentialMetatype: {
    auto metatype = static_cast<const ExistentialMetatypeMetadata *>(type);
    auto instance = _swift_buildDemanglingForMetadata(metatype->InstanceType);
    auto node = NodeFactory::create(Node::Kind::ExistentialMetatype);
    node->addChild(instance);
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
      auto input = _swift_buildDemanglingForMetadata(arg.getPointer());
      if (arg.getFlag()) {
        NodePointer inout = NodeFactory::create(Node::Kind::InOut);
        inout->addChild(input);
        input = inout;
      }
      inputs.push_back(input);
    }

    NodePointer totalInput;
    if (inputs.size() > 1) {
      auto tuple = NodeFactory::create(Node::Kind::NonVariadicTuple);
      for (auto &input : inputs)
        tuple->addChild(input);
      totalInput = tuple;
    } else {
      totalInput = inputs.front();
    }
    
    NodePointer args = NodeFactory::create(Node::Kind::ArgumentTuple);
    args->addChild(totalInput);
    
    NodePointer resultTy = _swift_buildDemanglingForMetadata(func->ResultType);
    NodePointer result = NodeFactory::create(Node::Kind::ReturnType);
    result->addChild(resultTy);
    
    auto funcNode = NodeFactory::create(kind);
    if (func->throws())
      funcNode->addChild(NodeFactory::create(Node::Kind::ThrowsAnnotation));
    funcNode->addChild(args);
    funcNode->addChild(result);
    return funcNode;
  }
  case MetadataKind::Metatype: {
    auto metatype = static_cast<const MetatypeMetadata *>(type);
    auto instance = _swift_buildDemanglingForMetadata(metatype->InstanceType);
    auto node = NodeFactory::create(Node::Kind::Metatype);
    node->addChild(instance);
    return node;
  }
  case MetadataKind::Tuple: {
    auto tuple = static_cast<const TupleTypeMetadata *>(type);
    auto tupleNode = NodeFactory::create(Node::Kind::NonVariadicTuple);
    for (unsigned i = 0, e = tuple->NumElements; i < e; ++i) {
      auto elt = _swift_buildDemanglingForMetadata(tuple->getElement(i).Type);
      tupleNode->addChild(elt);
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

#endif
