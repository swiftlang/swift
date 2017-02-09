//===--- TypeRef.h - Swift Type References for Reflection -------*- C++ -*-===//
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
#include "swift/Runtime/Unreachable.h"

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

// MSVC reports an error if we use "template"
// Clang reports an error if we don't use "template"
#if defined(__clang__) || defined(__GNUC__)
#define DEPENDENT_TEMPLATE template
#else
#define DEPENDENT_TEMPLATE
#endif

#define FIND_OR_CREATE_TYPEREF(Allocator, TypeRefTy, ...)                      \
  auto ID = Profile(__VA_ARGS__);                                              \
  const auto Entry = Allocator.DEPENDENT_TEMPLATE TypeRefTy##s.find(ID);       \
  if (Entry != Allocator.DEPENDENT_TEMPLATE TypeRefTy##s.end())                \
    return Entry->second;                                                      \
  const auto TR =                                                              \
      Allocator.DEPENDENT_TEMPLATE makeTypeRef<TypeRefTy>(__VA_ARGS__);        \
  Allocator.DEPENDENT_TEMPLATE TypeRefTy##s.insert({ID, TR});                  \
  return TR;

/// An identifier containing the unique bit pattern made up of all of the
/// instance data needed to uniquely identify a TypeRef.
///
/// This allows for uniquing (via Equal) and for keying into a dictionary for
/// caching.
///
/// TypeRefs should be comparable by pointers, so if the TypeRefBuilder
/// gets a request to build a TypeRef with the same constructor arguments,
/// it should return the one already created with those arguments, not a fresh
/// copy. This allows for fast identity comparisons and substitutions, for
/// example. We use a similar strategy for Types in the full AST.
class TypeRefID {

  std::vector<uint32_t> Bits;

public:
  TypeRefID() = default;

  template <typename T>
  void addPointer(const T *Pointer) {
    auto Raw = reinterpret_cast<uint32_t *>(&Pointer);
    Bits.push_back(Raw[0]);
    if (sizeof(const T *) > 4) {
      Bits.push_back(Raw[1]);
    }
  }

  void addInteger(uint32_t Integer) {
    Bits.push_back(Integer);
  }

  void addInteger(uint64_t Integer) {
    Bits.push_back((uint32_t)Integer);
    Bits.push_back(Integer >> 32);
  }

  void addString(const std::string &String) {
    if (String.empty()) {
      Bits.push_back(0);
    } else {
      size_t i = 0;
      size_t chunks = String.size() / 4;
      for (size_t chunk = 0; chunk < chunks; ++chunk, i+=4) {
        uint32_t entry = ((uint32_t) String[i]) +
                         (((uint32_t) String[i+1]) << 8) +
                         (((uint32_t) String[i+2]) << 16) +
                         (((uint32_t) String[i+3]) << 24);
        Bits.push_back(entry);
      }
      for (; i < String.size(); ++i) {
        Bits.push_back(String[i]);
      }
    }
  }

  struct Hash {
    std::size_t operator()(TypeRefID const &ID) const {
      size_t Hash = 0;
      std::hash<uint32_t> h;
      for (auto x : ID.Bits) {
        Hash ^= h(x) + 0x9e3779b9 + (Hash << 6) + (Hash >> 2);
      }
      return Hash;
    }
  };

  struct Equal {
    bool operator()(const TypeRefID &lhs, const TypeRefID &rhs) const {
      return lhs.Bits == rhs.Bits;
    }
  };


  bool operator==(const TypeRefID &Other) {
    return Bits == Other.Bits;
  }
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
  bool isConcreteAfterSubstitutions(const GenericArgumentMap &Subs) const;

  const TypeRef *
  subst(TypeRefBuilder &Builder, const GenericArgumentMap &Subs) const;

  GenericArgumentMap getSubstMap() const;

  virtual ~TypeRef() = default;

  /// Given an original type and substituted type, decompose them in
  /// parallel to derive substitutions that produced the substituted
  /// type.
  ///
  /// This will fail if the resulting substitutions contradict already
  /// known substitutions, or if the original and substituted types
  /// have a structural mismatch.
  static bool deriveSubstitutions(GenericArgumentMap &Subs,
                                  const TypeRef *OrigTR,
                                  const TypeRef *SubstTR);
};

class BuiltinTypeRef final : public TypeRef {
  std::string MangledName;

  static TypeRefID Profile(const std::string &MangledName) {
    TypeRefID ID;
    ID.addString(MangledName);
    return ID;
  }

public:
  BuiltinTypeRef(const std::string &MangledName)
    : TypeRef(TypeRefKind::Builtin), MangledName(MangledName) {}

  template <typename Allocator>
  static const BuiltinTypeRef *create(Allocator &A, std::string MangledName) {
    FIND_OR_CREATE_TYPEREF(A, BuiltinTypeRef, MangledName);
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
  static TypeRefID Profile(const std::string &MangledName,
                           const TypeRef *Parent) {
    TypeRefID ID;
    ID.addPointer(Parent);
    ID.addString(MangledName);
    return ID;
  }

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
  using NominalTypeTrait::Profile;
public:
  NominalTypeRef(const std::string &MangledName,
                 const TypeRef *Parent = nullptr)
    : TypeRef(TypeRefKind::Nominal), NominalTypeTrait(MangledName, Parent) {}

  template <typename Allocator>
  static const NominalTypeRef *create(Allocator &A,
                                      const std::string &MangledName,
                                      const TypeRef *Parent = nullptr) {
    FIND_OR_CREATE_TYPEREF(A, NominalTypeRef, MangledName, Parent);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Nominal;
  }
};

class BoundGenericTypeRef final : public TypeRef, public NominalTypeTrait {
  std::vector<const TypeRef *> GenericParams;

  static TypeRefID Profile(const std::string &MangledName,
                           const std::vector<const TypeRef *> &GenericParams,
                           const TypeRef *Parent) {
    TypeRefID ID;
    ID.addPointer(Parent);
    ID.addString(MangledName);
    for (auto Param : GenericParams)
      ID.addPointer(Param);
    return ID;
  }

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
    FIND_OR_CREATE_TYPEREF(A, BoundGenericTypeRef, MangledName, GenericParams,
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

  static TypeRefID Profile(const std::vector<const TypeRef *> &Elements,
                           bool Variadic) {
    TypeRefID ID;
    for (auto Element : Elements)
      ID.addPointer(Element);

    ID.addInteger(static_cast<uint32_t>(Variadic));
    return ID;
  }

public:
  TupleTypeRef(std::vector<const TypeRef *> Elements, bool Variadic=false)
    : TypeRef(TypeRefKind::Tuple), Elements(Elements), Variadic(Variadic) {}

  template <typename Allocator>
  static const TupleTypeRef *create(Allocator &A,
                                    std::vector<const TypeRef *> Elements,
                                    bool Variadic = false) {
    FIND_OR_CREATE_TYPEREF(A, TupleTypeRef, Elements, Variadic);
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

  static TypeRefID Profile(const std::vector<const TypeRef *> &Arguments,
                           const TypeRef *Result,
                           FunctionTypeFlags Flags) {
    TypeRefID ID;
    for (auto Argument : Arguments) {
      ID.addPointer(Argument);
    }
    ID.addPointer(Result);
    ID.addInteger(static_cast<uint64_t>(Flags.getIntValue()));
    return ID;
  }

public:
  FunctionTypeRef(std::vector<const TypeRef *> Arguments, const TypeRef *Result,
                  FunctionTypeFlags Flags)
    : TypeRef(TypeRefKind::Function), Arguments(Arguments), Result(Result),
      Flags(Flags) {}

  template <typename Allocator>
  static const FunctionTypeRef *create(Allocator &A,
                                       std::vector<const TypeRef *> Arguments,
                                       const TypeRef *Result,
                                       FunctionTypeFlags Flags) {
    FIND_OR_CREATE_TYPEREF(A, FunctionTypeRef, Arguments, Result, Flags);
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

  static TypeRefID Profile(const std::string &MangledName) {
    TypeRefID ID;
    ID.addString(MangledName);
    return ID;
  }
public:
  ProtocolTypeRef(const std::string &MangledName)
    : TypeRef(TypeRefKind::Protocol), MangledName(MangledName) {}

  template <typename Allocator>
  static const ProtocolTypeRef *
  create(Allocator &A, const std::string &MangledName) {
    FIND_OR_CREATE_TYPEREF(A, ProtocolTypeRef, MangledName);
  }

  bool isAnyObject() const {
    return MangledName == "Ps9AnyObject_";
  }

  bool isError() const {
    return MangledName == "Ps5Error_";
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

  static TypeRefID Profile(const std::vector<const TypeRef *> &Protocols) {
    TypeRefID ID;
    for (auto Protocol : Protocols) {
      ID.addPointer(Protocol);
    }
    return ID;
  }

public:
  ProtocolCompositionTypeRef(std::vector<const TypeRef *> Protocols)
    : TypeRef(TypeRefKind::ProtocolComposition), Protocols(Protocols) {}

  template <typename Allocator>
  static const ProtocolCompositionTypeRef *
  create(Allocator &A, std::vector<const TypeRef *> Protocols) {
    FIND_OR_CREATE_TYPEREF(A, ProtocolCompositionTypeRef, Protocols);\
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

  static TypeRefID Profile(const TypeRef *InstanceType, bool WasAbstract) {
    TypeRefID ID;
    ID.addPointer(InstanceType);
    ID.addInteger(static_cast<uint32_t>(WasAbstract));
    return ID;
  }
public:
  MetatypeTypeRef(const TypeRef *InstanceType, bool WasAbstract)
    : TypeRef(TypeRefKind::Metatype), InstanceType(InstanceType),
      WasAbstract(WasAbstract) {}

  template <typename Allocator>
  static const MetatypeTypeRef *create(Allocator &A,
                                       const TypeRef *InstanceType,
                                       bool WasAbstract = false) {
    FIND_OR_CREATE_TYPEREF(A, MetatypeTypeRef, InstanceType, WasAbstract);
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

  static TypeRefID Profile(const TypeRef *InstanceType) {
    TypeRefID ID;
    ID.addPointer(InstanceType);
    return ID;
  }

public:
  ExistentialMetatypeTypeRef(const TypeRef *InstanceType)
    : TypeRef(TypeRefKind::ExistentialMetatype), InstanceType(InstanceType) {}

  template <typename Allocator>
  static const ExistentialMetatypeTypeRef *
  create(Allocator &A, const TypeRef *InstanceType) {
    FIND_OR_CREATE_TYPEREF(A, ExistentialMetatypeTypeRef, InstanceType);
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

  static TypeRefID Profile(uint32_t Depth, uint32_t Index) {
    TypeRefID ID;
    ID.addInteger(Depth);
    ID.addInteger(Index);
    return ID;
  }

public:
  GenericTypeParameterTypeRef(uint32_t Depth, uint32_t Index)
    : TypeRef(TypeRefKind::GenericTypeParameter), Depth(Depth), Index(Index) {}

  template <typename Allocator>
  static const GenericTypeParameterTypeRef *
  create(Allocator &A, uint32_t Depth, uint32_t Index) {
    FIND_OR_CREATE_TYPEREF(A, GenericTypeParameterTypeRef, Depth, Index);
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

  static TypeRefID Profile(const std::string &Member, const TypeRef *Base,
                           const TypeRef *Protocol) {
    TypeRefID ID;
    ID.addString(Member);
    ID.addPointer(Base);
    ID.addPointer(Protocol);
    return ID;
  }

public:

  DependentMemberTypeRef(const std::string &Member, const TypeRef *Base,
                         const TypeRef *Protocol)
    : TypeRef(TypeRefKind::DependentMember), Member(Member), Base(Base),
      Protocol(Protocol) {}

  template <typename Allocator>
  static const DependentMemberTypeRef *
  create(Allocator &A, const std::string &Member,
         const TypeRef *Base, const TypeRef *Protocol) {
    FIND_OR_CREATE_TYPEREF(A, DependentMemberTypeRef, Member, Base, Protocol);
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

  static TypeRefID Profile(const std::string &Name) {
    TypeRefID ID;
    ID.addString(Name);
    return ID;
  }

public:
  ForeignClassTypeRef(const std::string &Name)
    : TypeRef(TypeRefKind::ForeignClass), Name(Name) {}

  template <typename Allocator>
  static const ForeignClassTypeRef *create(Allocator &A,
                                           const std::string &Name) {
    FIND_OR_CREATE_TYPEREF(A, ForeignClassTypeRef, Name);
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

  static TypeRefID Profile(const std::string &Name) {
    TypeRefID ID;
    ID.addString(Name);
    return ID;
  }
public:
  ObjCClassTypeRef(const std::string &Name)
    : TypeRef(TypeRefKind::ObjCClass), Name(Name) {}

  static const ObjCClassTypeRef *getUnnamed();

  template <typename Allocator>
  static const ObjCClassTypeRef *create(Allocator &A, const std::string &Name) {
    FIND_OR_CREATE_TYPEREF(A, ObjCClassTypeRef, Name);
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

  OpaqueTypeRef() : TypeRef(TypeRefKind::Opaque) {}

  static TypeRefID Profile() {
    return TypeRefID();
  }
public:
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

  static TypeRefID Profile(const TypeRef *Type) {
    TypeRefID ID;
    ID.addPointer(Type);
    return ID;
  }
public:
  const TypeRef *getType() const {
    return Type;
  }

  static bool classof(const TypeRef *TR) {
    auto Kind = TR->getKind();
    return (Kind == TypeRefKind::UnownedStorage &&
            Kind == TypeRefKind::WeakStorage &&
            Kind == TypeRefKind::UnmanagedStorage);
  }
};

class UnownedStorageTypeRef final : public ReferenceStorageTypeRef {
  using ReferenceStorageTypeRef::Profile;
public:
  UnownedStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::UnownedStorage, Type) {}

  template <typename Allocator>
  static const UnownedStorageTypeRef *create(Allocator &A,
                                             const TypeRef *Type) {
    FIND_OR_CREATE_TYPEREF(A, UnownedStorageTypeRef, Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::UnownedStorage;
  }
};

class WeakStorageTypeRef final : public ReferenceStorageTypeRef {
  using ReferenceStorageTypeRef::Profile;
public:
  WeakStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::WeakStorage, Type) {}

  template <typename Allocator>
  static const WeakStorageTypeRef *create(Allocator &A,
                                          const TypeRef *Type) {
    FIND_OR_CREATE_TYPEREF(A, WeakStorageTypeRef, Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::WeakStorage;
  }
};

class UnmanagedStorageTypeRef final : public ReferenceStorageTypeRef {
  using ReferenceStorageTypeRef::Profile;
public:
  UnmanagedStorageTypeRef(const TypeRef *Type)
    : ReferenceStorageTypeRef(TypeRefKind::UnmanagedStorage, Type) {}

  template <typename Allocator>
  static const UnmanagedStorageTypeRef *create(Allocator &A,
                                               const TypeRef *Type) {
    FIND_OR_CREATE_TYPEREF(A, UnmanagedStorageTypeRef, Type);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::UnmanagedStorage;
  }
};

class SILBoxTypeRef final : public TypeRef {
  const TypeRef *BoxedType;

  static TypeRefID Profile(const TypeRef *BoxedType) {
    TypeRefID ID;
    ID.addPointer(BoxedType);
    return ID;
  }
public:
  SILBoxTypeRef(const TypeRef *BoxedType)
    : TypeRef(TypeRefKind::SILBox), BoxedType(BoxedType) {}

  template <typename Allocator>
  static const SILBoxTypeRef *create(Allocator &A,
                                     const TypeRef *BoxedType) {
    FIND_OR_CREATE_TYPEREF(A, SILBoxTypeRef, BoxedType);
  }

  const TypeRef *getBoxedType() const {
    return BoxedType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::SILBox;
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

    swift_runtime_unreachable("Unhandled TypeRefKind in switch.");
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF_H
