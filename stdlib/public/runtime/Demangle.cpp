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

#include "swift/Basic/Range.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Portability.h"
#include "swift/Strings.h"
#include "Private.h"

#include <vector>

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#endif

using namespace swift;

Demangle::NodePointer
swift::_buildDemanglingForContext(const ContextDescriptor *context,
                                  llvm::ArrayRef<NodePointer> demangledGenerics,
                                  bool concretizedGenerics,
                                  Demangle::Demangler &Dem) {
  unsigned usedDemangledGenerics = 0;
  NodePointer node = nullptr;

  // Walk up the context tree.
  std::vector<const ContextDescriptor *> descriptorPath;
  {
    const ContextDescriptor *parent = context;
    while (parent) {
      descriptorPath.push_back(parent);
      parent = parent->Parent;
    }
  }

  auto getGenericArgsTypeListForContext =
    [&](const ContextDescriptor *context) -> NodePointer {
      // ABI TODO: As a hack to maintain existing broken behavior,
      // if there were any generic arguments eliminated by same type
      // constraints, we don't mangle any of them into intermediate contexts,
      // and pile all of the non-concrete arguments into the innermost context.
      if (concretizedGenerics)
        return nullptr;
      
      if (demangledGenerics.empty())
        return nullptr;
      
      auto generics = context->getGenericContext();
      if (!generics)
        return nullptr;
      
      auto numParams = generics->getGenericContextHeader().NumParams;
      if (numParams <= usedDemangledGenerics)
        return nullptr;
      
      auto genericArgsList = Dem.createNode(Node::Kind::TypeList);
      for (unsigned e = generics->getGenericContextHeader().NumParams;
           usedDemangledGenerics < e;
           ++usedDemangledGenerics) {
        genericArgsList->addChild(demangledGenerics[usedDemangledGenerics],
                                  Dem);
      }
      return genericArgsList;
    };
  
  auto innermostComponent = descriptorPath.front();
  for (auto component : reversed(descriptorPath)) {
    switch (auto kind = component->getKind()) {
    case ContextDescriptorKind::Module: {
      assert(node == nullptr && "module should be top level");
      auto name = llvm::cast<ModuleContextDescriptor>(component)->Name.get();
      node = Dem.createNode(Node::Kind::Module, name);
      break;
    }
    
    case ContextDescriptorKind::Extension: {
      auto extension = llvm::cast<ExtensionContextDescriptor>(component);
      // Demangle the extension self type.
      auto selfType = Dem.demangleType(extension->getMangledExtendedContext());
      assert(selfType->getKind() == Node::Kind::Type);
      selfType = selfType->getChild(0);
      
      // Substitute in the generic arguments.
      // TODO: This kludge only kinda works if there are no same-type
      // constraints. We'd need to handle those correctly everywhere else too
      // though.
      auto genericArgsList = getGenericArgsTypeListForContext(component);
      
      if (selfType->getKind() == Node::Kind::BoundGenericEnum
          || selfType->getKind() == Node::Kind::BoundGenericStructure
          || selfType->getKind() == Node::Kind::BoundGenericClass
          || selfType->getKind() == Node::Kind::BoundGenericOtherNominalType) {
        if (genericArgsList) {
          auto substSelfType = Dem.createNode(selfType->getKind());
          substSelfType->addChild(selfType->getChild(0), Dem);
          substSelfType->addChild(genericArgsList, Dem);
          selfType = substSelfType;
        } else {
          // TODO: Use the unsubstituted type if we can't handle the
          // substitutions yet.
          selfType = selfType->getChild(0)->getChild(0);
        }
      }
      
      auto extNode = Dem.createNode(Node::Kind::Extension);
      extNode->addChild(node, Dem);
      extNode->addChild(selfType, Dem);
      
      // TODO: Turn the generic signature into a demangling as the third
      // generic argument.
      
      node = extNode;
      break;
    }

    case ContextDescriptorKind::Protocol: {
      auto protocol = llvm::cast<ProtocolDescriptor>(component);
      auto name = protocol->Name.get();

      auto protocolNode = Dem.createNode(Node::Kind::Protocol);
      protocolNode->addChild(node, Dem);
      auto nameNode = Dem.createNode(Node::Kind::Identifier, name);
      protocolNode->addChild(nameNode, Dem);

      node = protocolNode;
      break;
    }

    default:
      // Form a type context demangling for type contexts.
      if (auto type = llvm::dyn_cast<TypeContextDescriptor>(component)) {
        auto name = type->Name.get();
        Node::Kind nodeKind;
        Node::Kind genericNodeKind;
        switch (kind) {
        case ContextDescriptorKind::Class:
          nodeKind = Node::Kind::Class;
          genericNodeKind = Node::Kind::BoundGenericClass;
          break;
        case ContextDescriptorKind::Struct:
          nodeKind = Node::Kind::Structure;
          genericNodeKind = Node::Kind::BoundGenericStructure;
          break;
        case ContextDescriptorKind::Enum:
          nodeKind = Node::Kind::Enum;
          genericNodeKind = Node::Kind::BoundGenericEnum;
          break;
        default:
          // We don't know about this kind of type. Use an "other type" mangling
          // for it.
          nodeKind = Node::Kind::OtherNominalType;
          genericNodeKind = Node::Kind::BoundGenericOtherNominalType;
          break;
        }
        
        // Override the node kind if this is a Clang-imported type so we give it
        // a stable mangling.
        auto typeFlags = type->getTypeContextDescriptorFlags();
        if (typeFlags.isCTypedef()) {
          nodeKind = Node::Kind::TypeAlias;
        } else if (nodeKind != Node::Kind::Structure &&
                   isCImportedTagType(type)) {
          nodeKind = Node::Kind::Structure;
        }
        
        auto typeNode = Dem.createNode(nodeKind);
        typeNode->addChild(node, Dem);
        auto nameNode = Dem.createNode(Node::Kind::Identifier, name);
        if (type->isSynthesizedRelatedEntity()) {
          auto relatedName = Dem.createNode(Node::Kind::RelatedEntityDeclName,
                                    type->getSynthesizedDeclRelatedEntityTag());
          relatedName->addChild(nameNode, Dem);
          nameNode = relatedName;
        }
        typeNode->addChild(nameNode, Dem);
        node = typeNode;
        
        // Apply generic arguments if the context is generic.
        if (auto genericArgsList = getGenericArgsTypeListForContext(component)){
          auto unspecializedType = Dem.createNode(Node::Kind::Type);
          unspecializedType->addChild(node, Dem);

          auto genericNode = Dem.createNode(genericNodeKind);
          genericNode->addChild(unspecializedType, Dem);
          genericNode->addChild(genericArgsList, Dem);
          node = genericNode;
        }
        
        // ABI TODO: If there were concretized generic arguments, just pile
        // all the non-concretized generic arguments into the innermost context.
        if (concretizedGenerics
            && !demangledGenerics.empty()
            && component == innermostComponent) {
          auto unspecializedType = Dem.createNode(Node::Kind::Type);
          unspecializedType->addChild(node, Dem);

          auto genericTypeList = Dem.createNode(Node::Kind::TypeList);
          for (auto arg : demangledGenerics) {
            if (!arg) continue;
            genericTypeList->addChild(arg, Dem);
          }
          
          if (genericTypeList->getNumChildren() > 0) {
            auto genericNode = Dem.createNode(genericNodeKind);
            genericNode->addChild(unspecializedType, Dem);
            genericNode->addChild(genericTypeList, Dem);
            node = genericNode;
          }
        }
        
        break;
      }

      // This runtime doesn't understand this context, or it's a context with
      // no richer runtime information available about it (such as an anonymous
      // context). Use an unstable mangling to represent the context by its
      // pointer identity.
      char addressBuf[sizeof(void*) * 2 + 2 + 1];
      snprintf(addressBuf, sizeof(addressBuf), "0x%" PRIxPTR, (uintptr_t)component);
      
      auto anonNode = Dem.createNode(Node::Kind::AnonymousContext);
      CharVector addressStr;
      addressStr.append(addressBuf, Dem);
      auto name = Dem.createNode(Node::Kind::Identifier, addressStr);
      anonNode->addChild(name, Dem);
      anonNode->addChild(node, Dem);
      
      // Collect generic arguments if the context is generic.
      auto genericArgsList = getGenericArgsTypeListForContext(component);
      if (!genericArgsList)
        genericArgsList = Dem.createNode(Node::Kind::TypeList);
      anonNode->addChild(genericArgsList, Dem);
      
      node = anonNode;
      
      break;
    }
  }
  
  // Wrap the final result in a top-level Type node.
  auto top = Dem.createNode(Node::Kind::Type);
  top->addChild(node, Dem);
  return top;
}

// FIXME: This stuff should be merged with the existing logic in
// include/swift/Reflection/TypeRefBuilder.h as part of the rewrite
// to change stdlib reflection over to using remote mirrors.

Demangle::NodePointer
swift::_swift_buildDemanglingForMetadata(const Metadata *type,
                                         Demangle::Demangler &Dem);

static Demangle::NodePointer
_buildDemanglerForBuiltinType(const Metadata *type, Demangle::Demangler &Dem) {
#define BUILTIN_TYPE(Symbol, Name) \
  if (type == &METADATA_SYM(Symbol).base) \
    return Dem.createNode(Node::Kind::BuiltinTypeName, Name);
#include "swift/Runtime/BuiltinTypes.def"
  return nullptr;
}

/// Build a demangled type tree for a nominal type.
static Demangle::NodePointer
_buildDemanglingForNominalType(const Metadata *type, Demangle::Demangler &Dem) {
  using namespace Demangle;

  // Get the context descriptor from the type metadata.
  const TypeContextDescriptor *description;

  switch (type->getKind()) {
  case MetadataKind::Class: {
    auto classType = static_cast<const ClassMetadata *>(type);
#if SWIFT_OBJC_INTEROP
    // Peek through artificial subclasses.
    while (classType->isTypeMetadata() && classType->isArtificialSubclass())
      classType = classType->Superclass;
#endif
    description = classType->getDescription();
    break;
  }
  case MetadataKind::Enum:
  case MetadataKind::Optional: {
    auto enumType = static_cast<const EnumMetadata *>(type);
    description = enumType->Description;
    break;
  }
  case MetadataKind::Struct: {
    auto structType = static_cast<const StructMetadata *>(type);
    description = structType->Description;
    break;
  }
  case MetadataKind::ForeignClass: {
    auto foreignType = static_cast<const ForeignClassMetadata *>(type);
    description = foreignType->Description;
    break;
  }
  default:
    return nullptr;
  }
  
  // Demangle the generic arguments.
  std::vector<NodePointer> demangledGenerics;
  bool concretizedGenerics = false;
  if (auto generics = description->getGenericContext()) {
    auto genericArgs = description->getGenericArguments(type);
    for (auto param : generics->getGenericParams()) {
      switch (param.getKind()) {
      case GenericParamKind::Type:
        // We don't know about type parameters with extra arguments.
        if (param.hasExtraArgument()) {
          genericArgs += param.hasExtraArgument() + param.hasKeyArgument();
          goto unknown_param;
        }

        // The type should have a key argument unless it's been same-typed to
        // another type.
        if (param.hasKeyArgument()) {
          auto paramType = *genericArgs++;
          auto paramDemangling =
            _swift_buildDemanglingForMetadata(paramType, Dem);
          if (!paramDemangling)
            return nullptr;
          demangledGenerics.push_back(paramDemangling);
        } else {
          // Leave a gap for us to fill in by looking at same type info.
          demangledGenerics.push_back(nullptr);
          concretizedGenerics = true;
        }
        break;
       
      unknown_param:
      default: {
        // We don't know about this kind of parameter. Create a placeholder
        // mangling.
        // ABI TODO: Mangle some kind of unique "unknown parameter"
        // representation here.
        auto placeholder = Dem.createNode(Node::Kind::Tuple);
        auto emptyList = Dem.createNode(Node::Kind::TypeList);
        placeholder->addChild(emptyList, Dem);
        auto type = Dem.createNode(Node::Kind::Type);
        type->addChild(placeholder, Dem);
        demangledGenerics.push_back(type);
      }
      }
    }
    
    // If we have concretized generic arguments, check for same type
    // requirements to get the argument values.
    // ABI TODO
  }
  
  return _buildDemanglingForContext(description, demangledGenerics,
                                    concretizedGenerics, Dem);
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
  case MetadataKind::ForeignClass:
    return _buildDemanglingForNominalType(type, Dem);
  case MetadataKind::ObjCClassWrapper: {
#if SWIFT_OBJC_INTEROP
    auto objcWrapper = static_cast<const ObjCClassWrapperMetadata *>(type);
    const char *className = class_getName(objcWrapper->getObjCClassObject());
    
    auto module = Dem.createNode(Node::Kind::Module, MANGLING_MODULE_OBJC);
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
  case MetadataKind::Existential: {
    auto exis = static_cast<const ExistentialTypeMetadata *>(type);
    auto protocols = exis->getProtocols();

    auto type_list = Dem.createNode(Node::Kind::TypeList);
    auto proto_list = Dem.createNode(Node::Kind::ProtocolList);
    proto_list->addChild(type_list, Dem);

    // The protocol descriptors should be pre-sorted since the compiler will
    // only ever make a swift_getExistentialTypeMetadata invocation using
    // its canonical ordering of protocols.

    for (auto protocol : protocols) {
#if SWIFT_OBJC_INTEROP
      if (protocol.isObjC()) {
        // The protocol name is mangled as a type symbol, with the _Tt prefix.
        StringRef ProtoName(protocol.getName());
        NodePointer protocolNode = Dem.demangleSymbol(ProtoName);

        // ObjC protocol names aren't mangled.
        if (!protocolNode) {
          auto module = Dem.createNode(Node::Kind::Module,
                                            MANGLING_MODULE_OBJC);
          auto node = Dem.createNode(Node::Kind::Protocol);
          node->addChild(module, Dem);
          node->addChild(Dem.createNode(Node::Kind::Identifier, ProtoName),
                         Dem);
          auto typeNode = Dem.createNode(Node::Kind::Type);
          typeNode->addChild(node, Dem);
          type_list->addChild(typeNode, Dem);
          continue;
        }

        // Dig out the protocol node.
        // Global -> (Protocol|TypeMangling)
        protocolNode = protocolNode->getChild(0);
        if (protocolNode->getKind() == Node::Kind::TypeMangling) {
          protocolNode = protocolNode->getChild(0); // TypeMangling -> Type
          protocolNode = protocolNode->getChild(0); // Type -> ProtocolList
          protocolNode = protocolNode->getChild(0); // ProtocolList -> TypeList
          protocolNode = protocolNode->getChild(0); // TypeList -> Type

          assert(protocolNode->getKind() == Node::Kind::Type);
          assert(protocolNode->getChild(0)->getKind() == Node::Kind::Protocol);
        } else {
          assert(protocolNode->getKind() == Node::Kind::Protocol);
        }

        type_list->addChild(protocolNode, Dem);
        continue;
      }
#endif

      auto protocolNode =
          _buildDemanglingForContext(protocol.getSwiftProtocol(), { }, false,
                                     Dem);
      if (!protocolNode)
        return nullptr;

      type_list->addChild(protocolNode, Dem);
    }

    if (auto superclass = exis->getSuperclassConstraint()) {
      // If there is a superclass constraint, we mangle it specially.
      auto result = Dem.createNode(Node::Kind::ProtocolListWithClass);
      auto superclassNode = _swift_buildDemanglingForMetadata(superclass, Dem);

      result->addChild(proto_list, Dem);
      result->addChild(superclassNode, Dem);
      return result;
    }

    if (exis->isClassBounded()) {
      // Check if the class constraint is implied by any of our
      // protocols.
      bool requiresClassImplicit = false;

      for (auto protocol : protocols) {
        if (protocol.getClassConstraint() == ProtocolClassConstraint::Class)
          requiresClassImplicit = true;
      }

      // If it was implied, we don't do anything special.
      if (requiresClassImplicit)
        return proto_list;

      // If the existential type has an explicit AnyObject constraint,
      // we must mangle it as such.
      auto result = Dem.createNode(Node::Kind::ProtocolListWithAnyObject);
      result->addChild(proto_list, Dem);
      return result;
    }

    // Just a simple composition of protocols.
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
      if (!func->isEscaping())
        kind = Node::Kind::NoEscapeFunctionType;
      else
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

    std::vector<std::pair<NodePointer, bool>> inputs;
    for (unsigned i = 0, e = func->getNumParameters(); i < e; ++i) {
      auto param = func->getParameter(i);
      auto flags = func->getParameterFlags(i);
      auto input = _swift_buildDemanglingForMetadata(param, Dem);

      auto wrapInput = [&](Node::Kind kind) {
        auto parent = Dem.createNode(kind);
        parent->addChild(input, Dem);
        input = parent;
      };
      switch (flags.getValueOwnership()) {
      case ValueOwnership::Default:
        /* nothing */
        break;
      case ValueOwnership::InOut:
        wrapInput(Node::Kind::InOut);
        break;
      case ValueOwnership::Shared:
        wrapInput(Node::Kind::Shared);
        break;
      case ValueOwnership::Owned:
        wrapInput(Node::Kind::Owned);
        break;
      }

      inputs.push_back({input, flags.isVariadic()});
    }

    NodePointer totalInput = nullptr;
    switch (inputs.size()) {
    case 1: {
      auto &singleParam = inputs.front();
      if (!singleParam.second) {
        totalInput = singleParam.first;
        break;
      }

      // If single parameter has a variadic marker it
      // requires a tuple wrapper.
      LLVM_FALLTHROUGH;
    }

    // This covers both none and multiple parameters.
    default:
      auto tuple = Dem.createNode(Node::Kind::Tuple);
      for (auto &input : inputs) {
        NodePointer eltType;
        bool isVariadic;
        std::tie(eltType, isVariadic) = input;

        // Tuple element := variadic-marker label? type
        auto tupleElt = Dem.createNode(Node::Kind::TupleElement);

        if (isVariadic)
          tupleElt->addChild(Dem.createNode(Node::Kind::VariadicMarker), Dem);

        if (eltType->getKind() == Node::Kind::Type) {
          tupleElt->addChild(eltType, Dem);
        } else {
          auto type = Dem.createNode(Node::Kind::Type);
          type->addChild(eltType, Dem);
          tupleElt->addChild(type, Dem);
        }

        tuple->addChild(tupleElt, Dem);
      }
      totalInput = tuple;
      break;
    }

    NodePointer parameters = Dem.createNode(Node::Kind::ArgumentTuple);
    NodePointer paramType = Dem.createNode(Node::Kind::Type);

    paramType->addChild(totalInput, Dem);
    parameters->addChild(paramType, Dem);

    NodePointer resultTy = _swift_buildDemanglingForMetadata(func->ResultType,
                                                             Dem);
    NodePointer result = Dem.createNode(Node::Kind::ReturnType);
    result->addChild(resultTy, Dem);
    
    auto funcNode = Dem.createNode(kind);
    if (func->throws())
      funcNode->addChild(Dem.createNode(Node::Kind::ThrowsAnnotation), Dem);
    funcNode->addChild(parameters, Dem);
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
    auto tupleNode = Dem.createNode(Node::Kind::Tuple);
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

      if (eltType->getKind() == Node::Kind::Type) {
        elt->addChild(eltType, Dem);
      } else {
        auto type = Dem.createNode(Node::Kind::Type);
        type->addChild(eltType, Dem);
        elt->addChild(type, Dem);
      }

      // Add the completed element to the tuple.
      tupleNode->addChild(elt, Dem);
    }
    return tupleNode;
  }
  case MetadataKind::HeapLocalVariable:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
    break;
  case MetadataKind::Opaque:
  default: {
    if (auto builtinType = _buildDemanglerForBuiltinType(type, Dem))
      return builtinType;
    
    // FIXME: Some opaque types do have manglings, but we don't have enough info
    // to figure them out.
    break;
  }
  }
  // Not a type.
  return nullptr;
}

// NB: This function is not used directly in the Swift codebase, but is
// exported for Xcode support and is used by the sanitizers. Please coordinate
// before changing.
char *swift_demangle(const char *mangledName,
                     size_t mangledNameLength,
                     char *outputBuffer,
                     size_t *outputBufferSize,
                     uint32_t flags) {
  if (flags != 0) {
    swift::fatalError(0, "Only 'flags' value of '0' is currently supported.");
  }
  if (outputBuffer != nullptr && outputBufferSize == nullptr) {
    swift::fatalError(0, "'outputBuffer' is passed but the size is 'nullptr'.");
  }

  // Check if we are dealing with Swift mangled name, otherwise, don't try
  // to demangle and send indication to the user.
  if (!Demangle::isSwiftSymbol(mangledName))
    return nullptr; // Not a mangled name

  // Demangle the name.
  auto options = Demangle::DemangleOptions();
  options.DisplayDebuggerGeneratedModule = false;
  auto result =
      Demangle::demangleSymbolAsString(mangledName,
                                       mangledNameLength,
                                       options);

  // If the output buffer is not provided, malloc memory ourselves.
  if (outputBuffer == nullptr || *outputBufferSize == 0) {
    return strdup(result.c_str());
  }

  // Indicate a failure if the result does not fit and will be truncated
  // and set the required outputBufferSize.
  if (*outputBufferSize < result.length() + 1) {
    *outputBufferSize = result.length() + 1;
  }

  // Copy into the provided buffer.
  _swift_strlcpy(outputBuffer, result.c_str(), *outputBufferSize);
  return outputBuffer;
}
