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

#include "swift/Basic/Demangle.h"
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

  void printRec(TypeRef *typeRef) {
    OS << "\n";

    Indent += 2;
    visit(typeRef);
    Indent -= 2;
  }

public:
  PrintTypeRef(std::ostream &OS, unsigned Indent)
  : OS(OS), Indent(Indent) {}

  void visitBuiltinTypeRef(BuiltinTypeRef *B) {
    printHeader("builtin");
    auto demangled = Demangle::demangleTypeAsString(B->getMangledName());
    printField("", demangled);
    OS << ')';
  }

  void visitNominalTypeRef(NominalTypeRef *N) {
    if (N->isStruct())
      printHeader("struct");
    else if (N->isEnum())
      printHeader("enum");
    else if (N->isClass())
      printHeader("class");
    else
      printHeader("nominal");
    auto demangled = Demangle::demangleTypeAsString(N->getMangledName());
    printField("", demangled);
    if (auto parent = N->getParent())
      printRec(parent);
    OS << ')';
  }

  void visitBoundGenericTypeRef(BoundGenericTypeRef *BG) {
    if (BG->isStruct())
      printHeader("bound-generic struct");
    else if (BG->isEnum())
      printHeader("bound-generic enum");
    else if (BG->isClass())
      printHeader("bound-generic class");
    else
      printHeader("bound-generic");

    auto demangled = Demangle::demangleTypeAsString(BG->getMangledName());
    printField("", demangled);
    for (auto param : BG->getGenericParams())
      printRec(param);
    if (auto parent = BG->getParent())
      printRec(parent);
    OS << ')';
  }

  void visitTupleTypeRef(TupleTypeRef *T) {
    printHeader("tuple");
    for (auto element : T->getElements())
      printRec(element);
    OS << ')';
  }

  void visitFunctionTypeRef(FunctionTypeRef *F) {
    printHeader("function");
    for (auto Arg : F->getArguments())
      printRec(Arg);
    printRec(F->getResult());
    OS << ')';
  }

  void visitProtocolTypeRef(ProtocolTypeRef *P) {
    printHeader("protocol");
    printField("module", P->getModuleName());
    printField("name", P->getName());
    OS << ')';
  }

  void visitProtocolCompositionTypeRef(ProtocolCompositionTypeRef *PC) {
    printHeader("protocol-composition");
    for (auto protocol : PC->getProtocols())
      printRec(protocol);
    OS << ')';
  }

  void visitMetatypeTypeRef(MetatypeTypeRef *M) {
    printHeader("metatype");
    printRec(M->getInstanceType());
    OS << ')';
  }

  void visitExistentialMetatypeTypeRef(ExistentialMetatypeTypeRef *EM) {
    printHeader("existential-metatype");
    printRec(EM->getInstanceType());
    OS << ')';
  }

  void visitGenericTypeParameterTypeRef(GenericTypeParameterTypeRef *GTP){
    printHeader("generic-type-parameter");
    printField("depth", GTP->getDepth());
    printField("index", GTP->getIndex());
    OS << ')';
  }

  void visitDependentMemberTypeRef(DependentMemberTypeRef *DM) {
    printHeader("dependent-member");
    printRec(DM->getProtocol());
    printRec(DM->getBase());
    printField("member", DM->getMember());
    OS << ')';
  }

  void visitForeignClassTypeRef(ForeignClassTypeRef *F) {
    printHeader("foreign");
    if (!F->getName().empty())
      printField("name", F->getName());
    OS << ')';
  }

  void visitObjCClassTypeRef(ObjCClassTypeRef *OC) {
    printHeader("objective-c-class");
    if (!OC->getName().empty())
      printField("name", OC->getName());
    OS << ')';
  }

  void visitUnownedStorageTypeRef(UnownedStorageTypeRef *US) {
    printHeader("unowned-storage");
    printRec(US->getType());
    OS << ')';
  }

  void visitWeakStorageTypeRef(WeakStorageTypeRef *WS) {
    printHeader("weak-storage");
    printRec(WS->getType());
    OS << ')';
  }

  void visitUnmanagedStorageTypeRef(UnmanagedStorageTypeRef *US) {
    printHeader("weak-storage");
    printRec(US->getType());
    OS << ')';
  }

  void visitOpaqueTypeRef(OpaqueTypeRef *O) {
    printHeader("opaque");
    OS << ')';
  }
};

struct TypeRefIsConcrete
  : public TypeRefVisitor<TypeRefIsConcrete, bool> {
  bool visitBuiltinTypeRef(BuiltinTypeRef *B) {
    return true;
  }

  bool visitNominalTypeRef(NominalTypeRef *N) {
    return true;
  }

  bool visitBoundGenericTypeRef(BoundGenericTypeRef *BG) {
    std::vector<TypeRef *> GenericParams;
    for (auto Param : BG->getGenericParams())
      if (!visit(Param))
        return false;
    return true;
  }

  bool visitTupleTypeRef(TupleTypeRef *T) {
    for (auto Element : T->getElements()) {
      if (!visit(Element))
        return false;
    }
    return true;
  }

  bool visitFunctionTypeRef(FunctionTypeRef *F) {
    std::vector<TypeRef *> SubstitutedArguments;
    for (auto Argument : F->getArguments())
      if (!visit(Argument))
        return false;
    return visit(F->getResult());
  }

  bool visitProtocolTypeRef(ProtocolTypeRef *P) {
    return true;
  }

  bool
  visitProtocolCompositionTypeRef(ProtocolCompositionTypeRef *PC) {
    for (auto Protocol : PC->getProtocols())
      if (!visit(Protocol))
        return false;
    return true;
  }

  bool visitMetatypeTypeRef(MetatypeTypeRef *M) {
    return visit(M->getInstanceType());
  }

  bool
  visitExistentialMetatypeTypeRef(ExistentialMetatypeTypeRef *EM) {
    return visit(EM->getInstanceType());
  }

  bool
  visitGenericTypeParameterTypeRef(GenericTypeParameterTypeRef *GTP){
    return false;
  }

  bool
  visitDependentMemberTypeRef(DependentMemberTypeRef *DM) {
    return visit(DM->getBase());
  }

  bool visitForeignClassTypeRef(ForeignClassTypeRef *F) {
    return true;
  }

  bool visitObjCClassTypeRef(ObjCClassTypeRef *OC) {
    return true;
  }
  
  bool visitOpaqueTypeRef(OpaqueTypeRef *Op) {
    return true;
  }

  bool visitUnownedStorageTypeRef(UnownedStorageTypeRef *US) {
    return visit(US->getType());
  }

  bool visitWeakStorageTypeRef(WeakStorageTypeRef *WS) {
    return visit(WS->getType());
  }

  bool visitUnmanagedStorageTypeRef(UnmanagedStorageTypeRef *US) {
    return visit(US->getType());
  }
};

ForeignClassTypeRef *
ForeignClassTypeRef::UnnamedSingleton = new ForeignClassTypeRef("");

ForeignClassTypeRef *ForeignClassTypeRef::getUnnamed() {
  return UnnamedSingleton;
}

ObjCClassTypeRef *
ObjCClassTypeRef::UnnamedSingleton = new ObjCClassTypeRef("");

ObjCClassTypeRef *ObjCClassTypeRef::getUnnamed() {
  return UnnamedSingleton;
}

OpaqueTypeRef *
OpaqueTypeRef::Singleton = new OpaqueTypeRef();

OpaqueTypeRef *OpaqueTypeRef::get() {
  return Singleton;
}

void TypeRef::dump() {
  dump(std::cerr);
}

void TypeRef::dump(std::ostream &OS, unsigned Indent) {
  PrintTypeRef(OS, Indent).visit(this);
  OS << std::endl;
}

bool TypeRef::isConcrete() {
  return TypeRefIsConcrete().visit(this);
}

unsigned NominalTypeTrait::getDepth() const {
  if (auto P = Parent) {
    switch (P->getKind()) {
    case TypeRefKind::Nominal:
      return 1 + cast<NominalTypeRef>(P)->getDepth();
    case TypeRefKind::BoundGeneric:
      return 1 + cast<BoundGenericTypeRef>(P)->getDepth();
    default:
      assert(false && "Asked for depth on non-nominal typeref");
    }
  }

  return 0;
}

GenericArgumentMap TypeRef::getSubstMap() const {
  GenericArgumentMap Substitutions;
  switch (getKind()) {
    case TypeRefKind::Nominal: {
      auto Nom = cast<NominalTypeRef>(this);
      if (auto Parent = Nom->getParent())
        return Parent->getSubstMap();
      return GenericArgumentMap();
    }
    case TypeRefKind::BoundGeneric: {
      auto BG = cast<BoundGenericTypeRef>(this);
      auto Depth = BG->getDepth();
      unsigned Index = 0;
      for (auto Param : BG->getGenericParams())
        Substitutions.insert({{Depth, Index++}, Param});
      if (auto Parent = BG->getParent()) {
        auto ParentSubs = Parent->getSubstMap();
        Substitutions.insert(ParentSubs.begin(), ParentSubs.end());
      }
      break;
    }
    default:
      break;
  }
  return Substitutions;
}

namespace {
bool isStruct(Demangle::NodePointer Node) {
  switch (Node->getKind()) {
    case Demangle::Node::Kind::Type:
      return isStruct(Node->getChild(0));
    case Demangle::Node::Kind::Structure:
    case Demangle::Node::Kind::BoundGenericStructure:
      return true;
    default:
      return false;
  }
}
bool isEnum(Demangle::NodePointer Node) {
  switch (Node->getKind()) {
    case Demangle::Node::Kind::Type:
      return isEnum(Node->getChild(0));
    case Demangle::Node::Kind::Enum:
    case Demangle::Node::Kind::BoundGenericEnum:
      return true;
    default:
      return false;
  }
}
bool isClass(Demangle::NodePointer Node) {
  switch (Node->getKind()) {
    case Demangle::Node::Kind::Type:
      return isClass(Node->getChild(0));
    case Demangle::Node::Kind::Class:
    case Demangle::Node::Kind::BoundGenericClass:
      return true;
    default:
      return false;
  }
}
}

bool NominalTypeTrait::isStruct() const {
  auto Demangled = Demangle::demangleTypeAsNode(MangledName);
  return ::isStruct(Demangled);
}


bool NominalTypeTrait::isEnum() const {
  auto Demangled = Demangle::demangleTypeAsNode(MangledName);
  return ::isEnum(Demangled);
}


bool NominalTypeTrait::isClass() const {
  auto Demangled = Demangle::demangleTypeAsNode(MangledName);
  return ::isClass(Demangled);
}
