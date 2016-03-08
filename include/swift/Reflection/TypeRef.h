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
#include <map>

class NodePointer;

namespace swift {
namespace reflection {

template <typename Runtime>
class ReflectionContext;

struct ReflectionInfo;

using llvm::cast;
using llvm::dyn_cast;

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

using GenericArgumentMap = std::map<std::pair<unsigned, unsigned>,
                                    TypeRefPointer>;

class TypeRef : public std::enable_shared_from_this<TypeRef> {
  TypeRefKind Kind;

public:
  TypeRef(TypeRefKind Kind) : Kind(Kind) {}

  TypeRefKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;

  bool isConcrete() const;

  template <typename Runtime>
  TypeRefPointer
  substituteGenerics(ReflectionContext<Runtime> &RC,
                     typename Runtime::StoredPointer MetadatAddress);
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
  GenericArgumentMap GenericParams;

public:
  BoundGenericTypeRef(std::string MangledName, GenericArgumentMap GenericParams)
    : TypeRef(TypeRefKind::BoundGeneric),
      MangledName(MangledName),
      GenericParams(GenericParams) {}

  static std::shared_ptr<BoundGenericTypeRef>
  create(std::string MangledName, GenericArgumentMap GenericParams) {
    return std::make_shared<BoundGenericTypeRef>(MangledName, GenericParams);
  }

  std::string getMangledName() const {
    return MangledName;
  }

  const GenericArgumentMap &getGenericParams() const {
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
  std::string Member;
  TypeRefPointer Base;

public:
  DependentMemberTypeRef(std::string Member, TypeRefPointer Base)
    : TypeRef(TypeRefKind::DependentMember), Member(Member), Base(Base) {}
  static std::shared_ptr<DependentMemberTypeRef>
  create(std::string Member, TypeRefPointer Base) {
    return std::make_shared<DependentMemberTypeRef>(Member, Base);
  }

  const std::string &getMember() const {
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

template <typename Runtime>
class TypeRefSubstitution
  : public TypeRefVisitor<TypeRefSubstitution<Runtime>, TypeRefPointer> {
  using StoredPointer = typename Runtime::StoredPointer;
  ReflectionContext<Runtime> &RC;
  GenericArgumentMap Substitutions;
  StoredPointer MetadataAddress;
public:
  using TypeRefVisitor<TypeRefSubstitution<Runtime>, TypeRefPointer>::visit;
  TypeRefSubstitution(ReflectionContext<Runtime> &RC,
                      StoredPointer MetadataAddress)
  : RC(RC), Substitutions(RC.getGenericArguments(MetadataAddress)),
    MetadataAddress(MetadataAddress) {}

  TypeRefPointer visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return std::make_shared<BuiltinTypeRef>(*B);
  }

  TypeRefPointer visitNominalTypeRef(const NominalTypeRef *N) {
    return std::make_shared<NominalTypeRef>(*N);
  }

  TypeRefPointer visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    GenericArgumentMap GenericParams;
    for (auto Param : BG->getGenericParams())
      if (auto Substituted = visit(Param.second.get()))
        GenericParams.insert({Param.first, Substituted});
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
    auto Sub = Substitutions.find({GTP->getIndex(), GTP->getDepth()});
    if (Sub == Substitutions.end())
      return nullptr;
    return Sub->second;
  }

  TypeRefPointer
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    auto SubstBase = visit(DM->getBase().get());
    if (!SubstBase || !SubstBase->isConcrete())
      return nullptr;

    TypeRefPointer TypeWitness;

    switch (SubstBase->getKind()) {
    case TypeRefKind::Nominal: {
      auto Nominal = cast<NominalTypeRef>(SubstBase.get());
      TypeWitness = RC.getDependentMemberTypeRef(MetadataAddress,
                                                 Nominal->getMangledName(),
                                                 DM->getMember());
      break;
    }
    case TypeRefKind::BoundGeneric: {
      auto BG = cast<BoundGenericTypeRef>(SubstBase.get());
      TypeWitness = RC.getDependentMemberTypeRef(MetadataAddress,
                                                 BG->getMangledName(),
                                                 DM->getMember());
      break;
    }
    default:
      return nullptr;
    }
    if (!TypeWitness)
      return nullptr;

    return visit(TypeWitness.get());
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

template <typename Runtime>
TypeRefPointer TypeRef::substituteGenerics(ReflectionContext<Runtime> &RC,
                                           typename Runtime::StoredPointer MetadataAddress) {
  return TypeRefSubstitution<Runtime>(RC, MetadataAddress).visit(this);
}

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF_H
