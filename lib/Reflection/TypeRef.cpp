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

#include "swift/Reflection/ReflectionContext.h"
#include "swift/Reflection/TypeRef.h"

using namespace swift;
using namespace reflection;

class PrintTypeRef : public TypeRefVisitor<PrintTypeRef, void> {
  std::ostream &OS;
  unsigned Indent;

  std::ostream &indent(unsigned Amount) {
    for (unsigned i = 0; i < Amount; ++i)
      OS << ' ';
    return OS;
  }

  std::ostream &printHeader(std::string Name) {
    indent(Indent) << '(' << Name;
    return OS;
  }

  template<typename T>
  std::ostream &printField(std::string name, const T &value) {
    if (!name.empty())
      OS << " " << name << "=" << value;
    else
      OS << " " << value;
    return OS;
  }

  void printRec(const TypeRef *typeRef) {
    OS << "\n";

    if (typeRef == nullptr)
      OS << "<<null>>";
    else {
      Indent += 2;
      visit(typeRef);
      Indent -=2;
    }
  }

public:
  PrintTypeRef(std::ostream &OS, unsigned Indent)
  : OS(OS), Indent(Indent) {}

  void visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    printHeader("builtin");
    auto demangled = Demangle::demangleTypeAsString(B->getMangledName());
    printField("", demangled);
    OS << ')';
  }

  void visitNominalTypeRef(const NominalTypeRef *N) {
    printHeader("nominal");
    auto demangled = Demangle::demangleTypeAsString(N->getMangledName());
    printField("", demangled);
    OS << ')';
  }

  void visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    printHeader("bound-generic");
    auto demangled = Demangle::demangleTypeAsString(BG->getMangledName());
    printField("", demangled);
    for (auto param : BG->getGenericParams())
      printRec(param.get());
    OS << ')';
  }

  void visitTupleTypeRef(const TupleTypeRef *T) {
    printHeader("tuple");
    for (auto element : T->getElements())
      printRec(element.get());
    OS << ')';
  }

  void visitFunctionTypeRef(const FunctionTypeRef *F) {
    printHeader("function");
    for (auto Arg : F->getArguments())
      printRec(Arg.get());
    printRec(F->getResult().get());
    OS << ')';
  }

  void visitProtocolTypeRef(const ProtocolTypeRef *P) {
    printHeader("protocol");
    printField("module", P->getModuleName());
    printField("name", P->getName());
    OS << ')';
  }

  void visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    printHeader("protocol-composition");
    for (auto protocol : PC->getProtocols())
      printRec(protocol.get());
    OS << ')';
  }

  void visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    printHeader("metatype");
    printRec(M->getInstanceType().get());
    OS << ')';
  }

  void visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    printHeader("existential-metatype");
    printRec(EM->getInstanceType().get());
    OS << ')';
  }

  void visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP){
    printHeader("generic-type-parameter");
    printField("index", GTP->getIndex());
    printField("depth", GTP->getDepth());
    OS << ')';
  }

  void visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    printHeader("dependent-member");
    printRec(DM->getBase().get());
    printField("member", DM->getMember());
    OS << ')';
  }

  void visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    printHeader("foreign");
    if (!F->getName().empty())
      printField("name", F->getName());
    OS << ')';
  }

  void visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    printHeader("objective-c-class");
    if (!OC->getName().empty())
      printField("name", OC->getName());
    OS << ')';
  }

  void visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    printHeader("opaque");
    OS << ')';
  }
};

struct TypeRefIsConcrete
  : public TypeRefVisitor<TypeRefIsConcrete, bool> {
  bool visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return true;
  }

  bool visitNominalTypeRef(const NominalTypeRef *N) {
    return true;
  }

  bool visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    TypeRefVector GenericParams;
    for (auto Param : BG->getGenericParams())
      if (!visit(Param.get()))
        return false;
    return true;
  }

  bool visitTupleTypeRef(const TupleTypeRef *T) {
    for (auto Element : T->getElements()) {
      if (!visit(Element.get()))
        return false;
    }
    return true;
  }

  bool visitFunctionTypeRef(const FunctionTypeRef *F) {
    TypeRefVector SubstitutedArguments;
    for (auto Argument : F->getArguments())
      if (!visit(Argument.get()))
        return false;
    return visit(F->getResult().get());
  }

  bool visitProtocolTypeRef(const ProtocolTypeRef *P) {
    return true;
  }

  bool
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    for (auto Protocol : PC->getProtocols())
      if (!visit(Protocol.get()))
        return false;
    return true;
  }

  bool visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    return visit(M->getInstanceType().get());
  }

  bool
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    return visit(EM->getInstanceType().get());
  }

  bool
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP){
    return false;
  }

  bool
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    return visit(DM->getBase().get());
  }

  bool visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return true;
  }

  bool visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return true;
  }
  
  bool visitOpaqueTypeRef(const OpaqueTypeRef *Op) {
    return true;
  }
};

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
      auto base = fromDemangleNode(Node->getChild(0));
      auto member = Node->getChild(1)->getText();
      return DependentMemberTypeRef::create(member, base);
    }
    default:
      return nullptr;
  }
}

bool TypeRef::isConcrete() const {
  return TypeRefIsConcrete().visit(this);
}
