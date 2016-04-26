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

#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/Casting.h"
#include "swift/ABI/MetadataValues.h"

#include <iostream>

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
class TypeRefBuilder;
using DepthAndIndex = std::pair<unsigned, unsigned>;
using GenericArgumentMap = llvm::DenseMap<DepthAndIndex, const TypeRef *>;

class alignas(void *) TypeRef {
  TypeRefKind Kind;

public:
  TypeRef(TypeRefKind Kind) : Kind(Kind) {}

  TypeRefKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(std::ostream &OS, unsigned Indent = 0) const;

  bool isConcrete() const;

  const TypeRef *
  subst(TypeRefBuilder &Builder, GenericArgumentMap Subs) const;

  GenericArgumentMap getSubstMap() const;

  virtual ~TypeRef() = default;
};

class BuiltinTypeRef final : public TypeRef {
  std::string MangledName;

public:
  BuiltinTypeRef(const std::string &MangledName)
    : TypeRef(TypeRefKind::Builtin), MangledName(MangledName) {}

  template <typename Allocator>
  static const BuiltinTypeRef *create(Allocator &A, std::string MangledName) {
    return A.template makeTypeRef<BuiltinTypeRef>(MangledName);
  }

  const std::string &getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Builtin;
  }
};

class NominalTypeTrait {
  std::string MangledName;
  const TypeRef *Parent;

protected:
  NominalTypeTrait(const std::string &MangledName, const TypeRef *Parent)
      : MangledName(MangledName), Parent(Parent) {}

public:
  const std::string &getMangledName() const {
    return MangledName;
  }

  bool isStruct() const;
  bool isEnum() const;
  bool isClass() const;

  const TypeRef *getParent() const {
    return Parent;
  }

  unsigned getDepth() const;
};

class NominalTypeRef final : public TypeRef, public NominalTypeTrait {
public:
  NominalTypeRef(const std::string &MangledName,
                 const TypeRef *Parent = nullptr)
    : TypeRef(TypeRefKind::Nominal), NominalTypeTrait(MangledName, Parent) {}

  template <typename Allocator>
  static const NominalTypeRef *create(Allocator &A,
                                      const std::string &MangledName,
                                      const TypeRef *Parent = nullptr) {
    return A.template makeTypeRef<NominalTypeRef>(MangledName, Parent);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Nominal;
  }
};

class BoundGenericTypeRef final : public TypeRef, public NominalTypeTrait {
  std::vector<const TypeRef *> GenericParams;

public:
  BoundGenericTypeRef(const std::string &MangledName,
                      std::vector<const TypeRef *> GenericParams,
                      const TypeRef *Parent = nullptr)
    : TypeRef(TypeRefKind::BoundGeneric),
      NominalTypeTrait(MangledName, Parent),
      GenericParams(GenericParams) {}

  template <typename Allocator>
  static const BoundGenericTypeRef *
  create(Allocator &A, const std::string &MangledName,
         std::vector<const TypeRef *> GenericParams,
         const TypeRef *Parent = nullptr) {
    return A.template makeTypeRef<BoundGenericTypeRef>(MangledName,
                                                       GenericParams,
                                                       Parent);
  }

  const std::vector<const TypeRef *> &getGenericParams() const {
    return GenericParams;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::BoundGeneric;
  }
};

class TupleTypeRef final : public TypeRef {
  std::vector<const TypeRef *> Elements;
  bool Variadic;

public:
  TupleTypeRef(std::vector<const TypeRef *> Elements, bool Variadic=false)
    : TypeRef(TypeRefKind::Tuple), Elements(Elements), Variadic(Variadic) {}

  template <typename Allocator>
  static TupleTypeRef *create(Allocator &A,
                              std::vector<const TypeRef *> Elements,
                              bool Variadic = false) {
    return A.template makeTypeRef<TupleTypeRef>(Elements, Variadic);
  }

  const std::vector<const TypeRef *> &getElements() const {
    return Elements;
  };

  bool isVariadic() const {
    return Variadic;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Tuple;
  }
};

class FunctionTypeRef final : public TypeRef {
  std::vector<const TypeRef *> Arguments;
  const TypeRef *Result;
  FunctionTypeFlags Flags;

public:
  FunctionTypeRef(std::vector<const TypeRef *> Arguments, const TypeRef *Result,
                  FunctionTypeFlags Flags)
    : TypeRef(TypeRefKind::Function), Arguments(Arguments), Result(Result),
      Flags(Flags) {}

  template <typename Allocator>
  static FunctionTypeRef *create(Allocator &A,
                                 std::vector<const TypeRef *> Arguments,
                                 const TypeRef *Result,
                                 FunctionTypeFlags Flags) {
    return A.template makeTypeRef<FunctionTypeRef>(Arguments, Result, Flags);
  }

  const std::vector<const TypeRef *> &getArguments() const {
    return Arguments;
  };

  const TypeRef *getResult() const {
    return Result;
  }

  FunctionTypeFlags getFlags() const {
    return Flags;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Function;
  }
};

class ProtocolTypeRef final : public TypeRef {
  std::string MangledName;

public:
  ProtocolTypeRef(const std::string &MangledName)
    : TypeRef(TypeRefKind::Protocol), MangledName(MangledName) {}

  template <typename Allocator>
  static const ProtocolTypeRef *
  create(Allocator &A, const std::string &MangledName) {
    return A.template makeTypeRef<ProtocolTypeRef>(MangledName);
  }

  const std::string &getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Protocol;
  }

  bool operator==(const ProtocolTypeRef &Other) const {
    return MangledName == Other.MangledName;
  }
  bool operator!=(const ProtocolTypeRef &Other) const {
    return !(*this == Other);
  }
};

class ProtocolCompositionTypeRef final : public TypeRef {
  std::vector<const TypeRef *> Protocols;

public:
  ProtocolCompositionTypeRef(std::vector<const TypeRef *> Protocols)
    : TypeRef(TypeRefKind::ProtocolComposition), Protocols(Protocols) {}

  template <typename Allocator>
  static const ProtocolCompositionTypeRef *
  create(Allocator &A, std::vector<const TypeRef *> Protocols) {
    return A.template makeTypeRef<ProtocolCompositionTypeRef>(Protocols);
  }

  const std::vector<const TypeRef *> &getProtocols() const {
    return Protocols;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ProtocolComposition;
  }
};

class MetatypeTypeRef final : public TypeRef {
  const TypeRef *InstanceType;
  bool WasAbstract;

public:
  MetatypeTypeRef(const TypeRef *InstanceType, bool WasAbstract)
    : TypeRef(TypeRefKind::Metatype), InstanceType(InstanceType),
      WasAbstract(WasAbstract) {}

  template <typename Allocator>
  static const MetatypeTypeRef *create(Allocator &A,
                                 const TypeRef *InstanceType,
                                 bool WasAbstract = false) {
    return A.template makeTypeRef<MetatypeTypeRef>(InstanceType, WasAbstract);
  }

  bool wasAbstract() const {
    return WasAbstract;
  }

  const TypeRef *getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Metatype;
  }
};

class ExistentialMetatypeTypeRef final : public TypeRef {
  const TypeRef *InstanceType;

public:
  ExistentialMetatypeTypeRef(const TypeRef *InstanceType)
    : TypeRef(TypeRefKind::ExistentialMetatype), InstanceType(InstanceType) {}

  template <typename Allocator>
  static const ExistentialMetatypeTypeRef *
  create(Allocator &A, const TypeRef *InstanceType) {
    return A.template makeTypeRef<ExistentialMetatypeTypeRef>(InstanceType);
  }

  const TypeRef *getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ExistentialMetatype;
  }
};

class GenericTypeParameterTypeRef final : public TypeRef {
  const uint32_t Depth;
  const uint32_t Index;

public:
  GenericTypeParameterTypeRef(uint32_t Depth, uint32_t Index)
    : TypeRef(TypeRefKind::GenericTypeParameter), Depth(Depth), Index(Index) {}

  template <typename Allocator>
  static const GenericTypeParameterTypeRef *
  create(Allocator &A, uint32_t Depth, uint32_t Index) {
    return A.template makeTypeRef<GenericTypeParameterTypeRef>(Depth, Index);
  }

  uint32_t getDepth() const {
    return Depth;
  }

  uint32_t getIndex() const {
    return Index;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::GenericTypeParameter;
  }
};

class DependentMemberTypeRef final : public TypeRef {
  std::string Member;
  const TypeRef *Base;
  const TypeRef *Protocol;

public:
  DependentMemberTypeRef(const std::string &Member, const TypeRef *Base,
                         const TypeRef *Protocol)
    : TypeRef(TypeRefKind::DependentMember), Member(Member), Base(Base),
      Protocol(Protocol) {}

  template <typename Allocator>
  static const DependentMemberTypeRef *
  create(Allocator &A, const std::string &Member,
         const TypeRef *Base, const TypeRef *Protocol) {
    return A.template makeTypeRef<DependentMemberTypeRef>(Member, Base,
                                                          Protocol);
  }

  const std::string &getMember() const {
    return Member;
  }

  const TypeRef *getBase() const {
    return Base;
  }

  const ProtocolTypeRef *getProtocol() const {
    return cast<ProtocolTypeRef>(Protocol);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::DependentMember;
  }
};

class ForeignClassTypeRef final : public TypeRef {
  std::string Name;
  static const ForeignClassTypeRef *UnnamedSingleton;
public:
  ForeignClassTypeRef(const std::string &Name)
    : TypeRef(TypeRefKind::ForeignClass), Name(Name) {}

  static const ForeignClassTypeRef *getUnnamed();


  template <typename Allocator>
  static ForeignClassTypeRef *create(Allocator &A,
                                     const std::string &Name) {
    return A.template makeTypeRef<ForeignClassTypeRef>(Name);
  }

  const std::string &getName() const {
    return Name;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ForeignClass;
  }
};

class ObjCClassTypeRef final : public TypeRef {
  std::string Name;
  static const ObjCClassTypeRef *UnnamedSingleton;
public:
  ObjCClassTypeRef(const std::string &Name)
    : TypeRef(TypeRefKind::ObjCClass), Name(Name) {}

  static const ObjCClassTypeRef *getUnnamed();

  template <typename Allocator>
  static const ObjCClassTypeRef *create(Allocator &A, const std::string &Name) {
    return A.template makeTypeRef<ObjCClassTypeRef>(Name);
  }

  const std::string &getName() const {
    return Name;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ObjCClass;
  }
};

class OpaqueTypeRef final : public TypeRef {
  static const OpaqueTypeRef *Singleton;
public:
  OpaqueTypeRef() : TypeRef(TypeRefKind::Opaque) {}

  static const OpaqueTypeRef *get();

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Opaque;
  }
};

class ReferenceStorageTypeRef : public TypeRef {
  const TypeRef *Type;

protected:
  ReferenceStorageTypeRef(TypeRefKind Kind, const TypeRef *Type)
    : TypeRef(Kind), Type(Type) {}

public:
  const TypeRef *getType() const {
    return Type;
  }
};

class UnownedStorageTypeRef final : public ReferenceStorageTypeRef {
public:
  UnownedStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::UnownedStorage, Type) {}

  template <typename Allocator>
  static const UnownedStorageTypeRef *create(Allocator &A,
                                       const TypeRef *Type) {
    return A.template makeTypeRef<UnownedStorageTypeRef>(Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::UnownedStorage;
  }
};

class WeakStorageTypeRef final : public ReferenceStorageTypeRef {
public:
  WeakStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::WeakStorage, Type) {}

  template <typename Allocator>
  static const WeakStorageTypeRef *create(Allocator &A,
                                    const TypeRef *Type) {
    return A.template makeTypeRef<WeakStorageTypeRef>(Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::WeakStorage;
  }
};

class UnmanagedStorageTypeRef final : public ReferenceStorageTypeRef {
public:
  UnmanagedStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::UnmanagedStorage, Type) {}

  template <typename Allocator>
  static const UnmanagedStorageTypeRef *create(Allocator &A,
                                               const TypeRef *Type) {
    return A.template makeTypeRef<UnmanagedStorageTypeRef>(Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::UnmanagedStorage;
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
