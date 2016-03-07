//===--- TypeRef.cpp - Swift Type References for Reflection ---------------===//
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
// Implements the structures of type references for property and enum
// case reflection.
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/TypeRef.h"

using namespace swift;
using namespace reflection;

const std::shared_ptr<ForeignClassTypeRef>
ForeignClassTypeRef::Unnamed = std::make_shared<ForeignClassTypeRef>("");

const std::shared_ptr<ObjCClassTypeRef>
ObjCClassTypeRef::Unnamed = std::make_shared<ObjCClassTypeRef>("");

const std::shared_ptr<OpaqueTypeRef>
OpaqueTypeRef::Opaque = std::make_shared<OpaqueTypeRef>();

void TypeRef::dump() const {
  dump(std::cerr);
}

void TypeRef::dump(std::ostream &OS, unsigned Indent) const {
  PrintTypeRef(OS, Indent).visit(this);
  OS << std::endl;
}

TypeRefPointer TypeRef::substituteGenerics(ConstTypeRefVector &Substitutions) {
  return TypeRefSubstitution(Substitutions).visit(this);
}

TypeRefPointer TypeRef::fromDemangleNode(Demangle::NodePointer Node) {
  using NodeKind = Demangle::Node::Kind;
  switch (Node->getKind()) {
    case NodeKind::Global:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::TypeMangling:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::Type:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::BoundGenericClass:
    case NodeKind::BoundGenericEnum:
    case NodeKind::BoundGenericStructure: {
      auto mangledName = Demangle::mangleNode(Node->getChild(0));
      auto genericArgs = Node->getChild(1);
      TypeRefVector Params;
      for (auto genericArg : *genericArgs)
        Params.push_back(fromDemangleNode(genericArg));

      return BoundGenericTypeRef::create(mangledName, Params);
    }
    case NodeKind::Class:
    case NodeKind::Enum:
    case NodeKind::Structure: {
      auto mangledName = Demangle::mangleNode(Node);
      return NominalTypeRef::create(mangledName);
    }
    case NodeKind::BuiltinTypeName: {
      auto mangledName = Demangle::mangleNode(Node);
      return BuiltinTypeRef::create(mangledName);
    }
    case NodeKind::ExistentialMetatype: {
      auto instance = fromDemangleNode(Node->getChild(0));
      return ExistentialMetatypeTypeRef::create(instance);
    }
    case NodeKind::Metatype: {
      auto instance = fromDemangleNode(Node->getChild(0));
      return MetatypeTypeRef::create(instance);
    }
    case NodeKind::ProtocolList: {
      TypeRefVector Protocols;
      auto TypeList = Node->getChild(0);
      for (auto Type : *TypeList) {
        if (auto Protocol = fromDemangleNode(Type))
          Protocols.push_back(Protocol);
        else
          return nullptr;
      }
      if (Protocols.size() == 1)
        return Protocols.front();
      else
        return ProtocolCompositionTypeRef::create(Protocols);
    }
    case NodeKind::Protocol: {
      auto moduleName = Node->getChild(0)->getText();
      auto name = Node->getChild(1)->getText();
      return ProtocolTypeRef::create(moduleName, name);
    }
    case NodeKind::DependentGenericParamType: {
      auto depth = Node->getChild(0)->getIndex();
      auto index = Node->getChild(1)->getIndex();
      return GenericTypeParameterTypeRef::create(index, depth);
    }
    case NodeKind::FunctionType: {
      TypeRefVector arguments;
      auto input = fromDemangleNode(Node->getChild(0));
      if (auto tuple = dyn_cast<TupleTypeRef>(input.get()))
        arguments = tuple->getElements();
      else
        arguments = { input };
      auto result = fromDemangleNode(Node->getChild(1));
      return FunctionTypeRef::create(arguments, result);
    }
    case NodeKind::ArgumentTuple:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::ReturnType:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::NonVariadicTuple: {
      TypeRefVector Elements;
      for (auto element : *Node)
        Elements.push_back(fromDemangleNode(element));
      return TupleTypeRef::create(Elements);
    }
    case NodeKind::TupleElement:
      return fromDemangleNode(Node->getChild(0));
    case NodeKind::DependentGenericType: {
      return fromDemangleNode(Node->getChild(1));
    }
    case NodeKind::DependentMemberType: {
      auto member = fromDemangleNode(Node->getChild(0));
      auto base = fromDemangleNode(Node->getChild(1));
      return DependentMemberTypeRef::create(member, base);
    }
    case NodeKind::DependentAssociatedTypeRef:
      return AssociatedTypeRef::create(Node->getText());
    default:
      return nullptr;
  }
}
