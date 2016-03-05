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

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF_H
