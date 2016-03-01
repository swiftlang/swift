//===--- TypeRef.h - Swift Type References for Reflection -------*- C++ -*-===//
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

#ifndef SWIFT_REFLECTION_TYPEREF_H
#define SWIFT_REFLECTION_TYPEREF_H

#include "swift/Basic/Demangle.h"
#include "llvm/Support/Casting.h"

#include <iostream>

class NodePointer;

namespace swift {
namespace reflection {

using llvm::cast;

enum class TypeRefKind {
#define TYPEREF(Id, Parent) Id,
#include "swift/Reflection/TypeRefs.def"
#undef TYPEREF
};

class TypeRef;
using TypeRefPointer = std::shared_ptr<TypeRef>;
using ConstTypeRefPointer = std::shared_ptr<const TypeRef>;
using TypeRefVector = std::vector<TypeRefPointer>;
using ConstTypeRefVector = const std::vector<TypeRefPointer>;

class TypeRef : public std::enable_shared_from_this<TypeRef> {
  TypeRefKind Kind;

public:
  TypeRef(TypeRefKind Kind) : Kind(Kind) {}

  TypeRefKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;

  TypeRefPointer substituteGenerics(ConstTypeRefVector &Substitutions);
  static TypeRefPointer fromDemangleNode(Demangle::NodePointer Node);
};

class BuiltinTypeRef final : public TypeRef {
  std::string MangledName;

public:
  BuiltinTypeRef(std::string MangledName)
    : TypeRef(TypeRefKind::Builtin), MangledName(MangledName) {}

  static std::shared_ptr<BuiltinTypeRef> create(std::string MangledName) {
    return std::make_shared<BuiltinTypeRef>(MangledName);
  }

  std::string getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Builtin;
  }
};

class NominalTypeRef final : public TypeRef {
  std::string MangledName;

public:
  NominalTypeRef(std::string MangledName)
    : TypeRef(TypeRefKind::Nominal), MangledName(MangledName) {}

  static std::shared_ptr<NominalTypeRef> create(std::string MangledName) {
    return std::make_shared<NominalTypeRef>(MangledName);
  }

  std::string getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Nominal;
  }
};

class BoundGenericTypeRef final : public TypeRef {
  std::string MangledName;
  TypeRefVector GenericParams;

public:
  BoundGenericTypeRef(std::string MangledName, TypeRefVector GenericParams)
    : TypeRef(TypeRefKind::BoundGeneric),
      MangledName(MangledName),
      GenericParams(GenericParams) {}

  static std::shared_ptr<BoundGenericTypeRef>
  create(std::string MangledName, TypeRefVector GenericParams) {
    return std::make_shared<BoundGenericTypeRef>(MangledName, GenericParams);
  }

  std::string getMangledName() const {
    return MangledName;
  }

  TypeRefVector getGenericParams() {
    return GenericParams;
  }

  ConstTypeRefVector getGenericParams() const {
    return GenericParams;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::BoundGeneric;
  }
};

class TupleTypeRef final : public TypeRef {
  TypeRefVector Elements;

public:
  TupleTypeRef(TypeRefVector Elements)
    : TypeRef(TypeRefKind::Tuple), Elements(Elements) {}

  static std::shared_ptr<TupleTypeRef> create(TypeRefVector Elements) {
    return std::make_shared<TupleTypeRef>(Elements);
  }

  TypeRefVector getElements() {
    return Elements;
  };

  ConstTypeRefVector getElements() const {
    return Elements;
  };

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Tuple;
  }
};

class FunctionTypeRef final : public TypeRef {
  TypeRefVector Arguments;
  TypeRefPointer Result;

public:
  FunctionTypeRef(TypeRefVector Arguments, TypeRefPointer Result)
    : TypeRef(TypeRefKind::Function), Arguments(Arguments), Result(Result) {}

  static std::shared_ptr<FunctionTypeRef> create(TypeRefVector Arguments,
                                                 TypeRefPointer Result) {
    return std::make_shared<FunctionTypeRef>(Arguments, Result);
  }

  TypeRefVector getArguments() {
    return Arguments;
  };

  ConstTypeRefVector getArguments() const {
    return Arguments;
  };

  TypeRefPointer getResult() {
    return Result;
  }

  ConstTypeRefPointer getResult() const {
    return Result;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Function;
  }
};

class ProtocolTypeRef final : public TypeRef {
  std::string ModuleName;
  std::string Name;

public:
  ProtocolTypeRef(std::string ModuleName, std::string Name)
    : TypeRef(TypeRefKind::Protocol), ModuleName(ModuleName), Name(Name) {}

  static std::shared_ptr<ProtocolTypeRef>
  create(std::string ModuleName, std::string Name) {
    return std::make_shared<ProtocolTypeRef>(ModuleName, Name);
  }

  std::string getName() const {
    return Name;
  }

  std::string getModuleName() const {
    return ModuleName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Protocol;
  }
};

class ProtocolCompositionTypeRef final : public TypeRef {
  TypeRefVector Protocols;

public:
  ProtocolCompositionTypeRef(TypeRefVector Protocols)
    : TypeRef(TypeRefKind::ProtocolComposition), Protocols(Protocols) {}

  static std::shared_ptr<ProtocolCompositionTypeRef>
  create(TypeRefVector Protocols) {
    return std::make_shared<ProtocolCompositionTypeRef>(Protocols);
  }

  TypeRefVector getProtocols() {
    return Protocols;
  }

  ConstTypeRefVector getProtocols() const {
    return Protocols;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ProtocolComposition;
  }
};

class MetatypeTypeRef final : public TypeRef {
  TypeRefPointer InstanceType;

public:
  MetatypeTypeRef(TypeRefPointer InstanceType)
    : TypeRef(TypeRefKind::Metatype), InstanceType(InstanceType) {}

  static std::shared_ptr<MetatypeTypeRef> create(TypeRefPointer InstanceType) {
    return std::make_shared<MetatypeTypeRef>(InstanceType);
  }

  TypeRefPointer getInstanceType() {
    return InstanceType;
  }

  ConstTypeRefPointer getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Metatype;
  }
};

class ExistentialMetatypeTypeRef final : public TypeRef {
  TypeRefPointer InstanceType;

public:
  ExistentialMetatypeTypeRef(TypeRefPointer InstanceType)
    : TypeRef(TypeRefKind::ExistentialMetatype), InstanceType(InstanceType) {}
  static std::shared_ptr<ExistentialMetatypeTypeRef>
  create(TypeRefPointer InstanceType) {
    return std::make_shared<ExistentialMetatypeTypeRef>(InstanceType);
  }

  TypeRefPointer getInstanceType() {
    return InstanceType;
  }

  ConstTypeRefPointer getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ExistentialMetatype;
  }
};

class GenericTypeParameterTypeRef final : public TypeRef {
  const uint32_t Index;
  const uint32_t Depth;

public:
  GenericTypeParameterTypeRef(uint32_t Index, uint32_t Depth)
    : TypeRef(TypeRefKind::GenericTypeParameter), Index(Index), Depth(Depth) {}

  static std::shared_ptr<GenericTypeParameterTypeRef>
  create(uint32_t Index, uint32_t Depth) {
    return std::make_shared<GenericTypeParameterTypeRef>(Index, Depth);
  }

  uint32_t getIndex() const {
    return Index;
  }

  uint32_t getDepth() const {
    return Depth;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::GenericTypeParameter;
  }
};

class DependentMemberTypeRef final : public TypeRef {
  TypeRefPointer Member;
  TypeRefPointer Base;

public:
  DependentMemberTypeRef(TypeRefPointer Member, TypeRefPointer Base)
    : TypeRef(TypeRefKind::DependentMember), Member(Member), Base(Base) {}
  static std::shared_ptr<DependentMemberTypeRef>
  create(TypeRefPointer Member, TypeRefPointer Base) {
    return std::make_shared<DependentMemberTypeRef>(Member, Base);
  }

  TypeRefPointer getMember() {
    return Member;
  }

  ConstTypeRefPointer getMember() const {
    return Member;
  }

  TypeRefPointer getBase() {
    return Base;
  }

  ConstTypeRefPointer getBase() const {
    return Base;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::DependentMember;
  }
};

class AssociatedTypeRef final : public TypeRef {
  std::string Name;

public:
  AssociatedTypeRef(std::string Name)
    : TypeRef(TypeRefKind::Associated), Name(Name) {}

  static std::shared_ptr<AssociatedTypeRef> create(std::string Name) {
    return std::make_shared<AssociatedTypeRef>(Name);
  }

  std::string getName() const {
    return Name;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Associated;
  }
};

class ForeignClassTypeRef final : public TypeRef {
  std::string Name;
public:
  ForeignClassTypeRef(std::string Name)
    : TypeRef(TypeRefKind::ForeignClass), Name(Name) {}
  static const std::shared_ptr<ForeignClassTypeRef> Unnamed;

  std::string getName() const {
    return Name;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ForeignClass;
  }
};

class ObjCClassTypeRef final : public TypeRef {
  std::string Name;
public:
  ObjCClassTypeRef(std::string Name)
    : TypeRef(TypeRefKind::ObjCClass), Name(Name) {}
  static const std::shared_ptr<ObjCClassTypeRef> Unnamed;

  std::string getName() const {
    return Name;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ObjCClass;
  }
};

class OpaqueTypeRef final : public TypeRef {
public:
  OpaqueTypeRef() : TypeRef(TypeRefKind::Opaque) {}
  static const std::shared_ptr<OpaqueTypeRef> Opaque;
  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Opaque;
  }
};

template <typename ImplClass, typename RetTy = void, typename... Args>
class TypeRefVisitor {
public:

  RetTy visit(const TypeRef *typeRef, Args... args) {
    switch (typeRef->getKind()) {
#define TYPEREF(Id, Parent) \
    case TypeRefKind::Id: \
      return static_cast<ImplClass*>(this) \
        ->visit##Id##TypeRef(cast<Id##TypeRef>(typeRef), \
                           ::std::forward<Args>(args)...);
#include "swift/Reflection/TypeRefs.def"
    }
  }
};

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
    printField("input", "");
    for (auto Arg : F->getArguments())
      printRec(Arg.get());
    printField("result", "");
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
    printRec(DM->getMember().get());
    OS << ')';
  }

  void visitAssociatedTypeRef(const AssociatedTypeRef *AT) {
    printHeader("associated-type");
    printField("name", AT->getName());
    OS << ')';
  }

  void visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    printHeader("foreign");
    printField("name", F->getName());
    OS << ')';
  }

  void visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    printHeader("objective-c-class");
    printField("name", OC->getName());
    OS << ')';
  }

  void visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    printHeader("opaque");
    OS << ')';
  }
};

class TypeRefSubstitution
  : public TypeRefVisitor<TypeRefSubstitution, TypeRefPointer> {
  ConstTypeRefVector &Substitutions;
public:
  TypeRefSubstitution(ConstTypeRefVector &Substitutions)
  : Substitutions(Substitutions) {}

  TypeRefPointer visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return std::make_shared<BuiltinTypeRef>(*B);
  }

  TypeRefPointer visitNominalTypeRef(const NominalTypeRef *N) {
    return std::make_shared<NominalTypeRef>(*N);
  }

  TypeRefPointer visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    TypeRefVector GenericParams;
    for (auto Param : BG->getGenericParams())
      if (auto Substituted = visit(Param.get()))
        GenericParams.push_back(Substituted);
      else return nullptr;
    return std::make_shared<BoundGenericTypeRef>(BG->getMangledName(),
                                                 GenericParams);
  }

  TypeRefPointer visitTupleTypeRef(const TupleTypeRef *T) {
    TypeRefVector Elements;
    for (auto Element : T->getElements()) {
      if (auto SubstitutedElement = visit(Element.get()))
        Elements.push_back(SubstitutedElement);
      else
        return nullptr;
    }
    return std::make_shared<TupleTypeRef>(Elements);
  }

  TypeRefPointer visitFunctionTypeRef(const FunctionTypeRef *F) {
    TypeRefVector SubstitutedArguments;
    for (auto Argument : F->getArguments())
      if (auto SubstitutedArgument = visit(Argument.get()))
        SubstitutedArguments.push_back(SubstitutedArgument);
      else
        return nullptr;

    auto SubstitutedResult = visit(F->getResult().get());
    if (!SubstitutedResult)
      return nullptr;

    return std::make_shared<FunctionTypeRef>(SubstitutedArguments,
                                             SubstitutedResult);
  }

  TypeRefPointer visitProtocolTypeRef(const ProtocolTypeRef *P) {
    return std::make_shared<ProtocolTypeRef>(*P);
  }

  TypeRefPointer
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    return std::make_shared<ProtocolCompositionTypeRef>(*PC);
  }

  TypeRefPointer visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    if (auto SubstitutedInstance = visit(M->getInstanceType().get()))
      return std::make_shared<MetatypeTypeRef>(SubstitutedInstance);
    else
      return nullptr;
  }

  TypeRefPointer
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    if (auto SubstitutedInstance = visit(EM->getInstanceType().get()))
      return std::make_shared<MetatypeTypeRef>(SubstitutedInstance);
    else
      return nullptr;
  }

  TypeRefPointer
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP){
    if (GTP->getIndex() < Substitutions.size())
      return Substitutions[GTP->getIndex()];
    return nullptr;
  }

  TypeRefPointer
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    return std::make_shared<DependentMemberTypeRef>(*DM);
  }

  TypeRefPointer visitAssociatedTypeRef(const AssociatedTypeRef *AT) {
    return std::make_shared<AssociatedTypeRef>(*AT);
  }

  TypeRefPointer visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return std::make_shared<ForeignClassTypeRef>(*F);
  }

  TypeRefPointer visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return std::make_shared<ObjCClassTypeRef>(*OC);
  }

  TypeRefPointer visitOpaqueTypeRef(const OpaqueTypeRef *Op) {
    return std::make_shared<OpaqueTypeRef>(*Op);
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF_H
