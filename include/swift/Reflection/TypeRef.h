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

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

class NodePointer;

namespace swift {
namespace reflection {

class ReflectionContext;

using llvm::ArrayRef;
using llvm::StringRef;
using llvm::cast;

enum class TypeRefKind {
#define TYPEREF(Id, Parent) Id,
#include "swift/Reflection/TypeRefs.def"
#undef TYPEREF
};

class TypeRef {
  TypeRefKind Kind;

public:
  TypeRef(TypeRefKind Kind) : Kind(Kind) {}

  TypeRefKind getKind() const {
    return Kind;
  }

  void dump() const;
  void dump(llvm::raw_ostream &os, unsigned indent = 0) const;
};

class BuiltinTypeRef : public TypeRef {
  StringRef MangledName;

  BuiltinTypeRef(StringRef MangledName);

public:
  static BuiltinTypeRef *create(ReflectionContext &RC, StringRef MangledName);

  StringRef getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Builtin;
  }
};

class NominalTypeRef : public TypeRef {
  StringRef MangledName;

  NominalTypeRef(StringRef MangledName);
public:
  static NominalTypeRef *create(ReflectionContext &RC, StringRef MangledName);

  StringRef getMangledName() const {
    return MangledName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Nominal;
  }
};

class BoundGenericTypeRef : public TypeRef {
  StringRef MangledName;
  uint32_t NumGenericParams;

  TypeRef **getGenericParameterBuffer() {
    return reinterpret_cast<TypeRef**>(this + 1);
  }

  const TypeRef * const *getGenericParameterBuffer() const {
    return reinterpret_cast<const TypeRef * const *>(this + 1);
  }

  BoundGenericTypeRef(StringRef MangledName,
                      ArrayRef<TypeRef *> GenericParams);

public:
  static BoundGenericTypeRef *create(ReflectionContext &RC,
                                     StringRef MangledName,
                                     ArrayRef<TypeRef *> GenericParams);

  StringRef getMangledName() const {
    return MangledName;
  }

  ArrayRef<TypeRef *> getGenericParams() {
    return ArrayRef<TypeRef *>(getGenericParameterBuffer(), NumGenericParams);
  };

  ArrayRef<const TypeRef *> getGenericParams() const {
    return ArrayRef<const TypeRef *>(getGenericParameterBuffer(),
                                     NumGenericParams);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::BoundGeneric;
  }
};

class TupleTypeRef : public TypeRef {
  uint32_t NumElements;

  TypeRef **getElementBuffer() {
    return reinterpret_cast<TypeRef**>(this + 1);
  }

  const TypeRef * const *getElementBuffer() const {
    return reinterpret_cast<const TypeRef * const *>(this + 1);
  }

  TupleTypeRef(ArrayRef<TypeRef *> Elements);

public:
  static TupleTypeRef *create(ReflectionContext &RC,
                              ArrayRef<TypeRef *> Elements);

  ArrayRef<TypeRef *> getElements() {
    return ArrayRef<TypeRef *>(getElementBuffer(), NumElements);
  };

  ArrayRef<const TypeRef *> getElements() const {
    return ArrayRef<const TypeRef *>(getElementBuffer(), NumElements);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Tuple;
  }
};

class FunctionTypeRef : public TypeRef {
  TypeRef *Input;
  TypeRef *Result;

  FunctionTypeRef(TypeRef *Input, TypeRef *Result);
public:
  static FunctionTypeRef *create(ReflectionContext &RC, TypeRef *Input,
                          TypeRef *Result);

  TypeRef *getInput() {
    return Input;
  };

  const TypeRef *getInput() const {
    return Input;
  }

  TypeRef *getResult() {
    return Result;
  }

  const TypeRef *getResult() const {
    return Result;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Function;
  }
};

class ProtocolTypeRef : public TypeRef {
  StringRef ModuleName;
  StringRef Name;

  ProtocolTypeRef(StringRef ModuleName, StringRef Name);

public:
  static ProtocolTypeRef *create(ReflectionContext &RC, StringRef ModuleName,
                                 StringRef Name);

  StringRef getName() const {
    return Name;
  }

  StringRef getModuleName() const {
    return ModuleName;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Protocol;
  }
};

class ProtocolCompositionTypeRef : public TypeRef {
  uint32_t NumProtocols;

  ProtocolCompositionTypeRef(ArrayRef<TypeRef *> Protocols);

  TypeRef **getProtocolBuffer() {
    return reinterpret_cast<TypeRef **>(this + 1);
  }

  const TypeRef * const *getProtocolBuffer() const {
    return reinterpret_cast<const TypeRef * const *>(this + 1);
  }

public:
  static ProtocolCompositionTypeRef *create(ReflectionContext &RC,
                                     ArrayRef<TypeRef *> Protocols);

  ArrayRef<TypeRef *>getProtocols() {
    return ArrayRef<TypeRef *>(getProtocolBuffer(), NumProtocols);
  }

  ArrayRef<const TypeRef *> getProtocols() const {
    return ArrayRef<const TypeRef *>(getProtocolBuffer(), NumProtocols);
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ProtocolComposition;
  }
};

class MetatypeTypeRef : public TypeRef {
  TypeRef *InstanceType;

  MetatypeTypeRef(TypeRef *InstanceType);

public:
  static MetatypeTypeRef *create(ReflectionContext &RC, TypeRef *InstanceType);

  TypeRef *getInstanceType() {
    return InstanceType;
  }

  const TypeRef *getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::Metatype;
  }
};

class ExistentialMetatypeTypeRef : public TypeRef {
  TypeRef *InstanceType;

  ExistentialMetatypeTypeRef(TypeRef *InstanceType);
public:
  static ExistentialMetatypeTypeRef *create(ReflectionContext &RC,
                                     TypeRef *InstanceType);

  TypeRef *getInstanceType() {
    return InstanceType;
  }

  const TypeRef *getInstanceType() const {
    return InstanceType;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::ExistentialMetatype;
  }
};

class GenericTypeParameterTypeRef : public TypeRef {
  const uint32_t Index;
  const uint32_t Depth;

  GenericTypeParameterTypeRef(uint32_t Index, uint32_t Depth);
public:

  static GenericTypeParameterTypeRef *create(ReflectionContext &RC,
                                             uint32_t Index, uint32_t Depth);

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

class DependentMemberTypeRef : public TypeRef {
  TypeRef *Member;
  TypeRef *Base;

  DependentMemberTypeRef(TypeRef *Member, TypeRef *Base);
public:
  static DependentMemberTypeRef *create(ReflectionContext &RC,
                                 TypeRef *Member,
                                 TypeRef *Base);
  TypeRef *getMember() {
    return Member;
  }

  const TypeRef *getMember() const {
    return Member;
  }

  TypeRef *getBase() {
    return Base;
  }

  const TypeRef *getBase() const {
    return Base;
  }

  static bool classof(const TypeRef *TR) {
    return TR->getKind() == TypeRefKind::DependentMember;
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
  llvm::raw_ostream &OS;
  unsigned Indent;

  llvm::raw_ostream &printHeader(StringRef Name) {
    OS.indent(Indent) << '(' << Name;
    return OS;
  }

  template<typename T>
  llvm::raw_ostream &printField(StringRef name, const T &value) {
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
  PrintTypeRef(llvm::raw_ostream &OS, unsigned Indent)
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
      printRec(param);
    OS << ')';
  }

  void visitTupleTypeRef(const TupleTypeRef *T) {
    printHeader("tuple");
    for (auto element : T->getElements())
      printRec(element);
    OS << ')';
  }

  void visitFunctionTypeRef(const FunctionTypeRef *F) {
    printHeader("function");
    printRec(F->getInput());
    printRec(F->getResult());
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
      printRec(protocol);
    OS << ')';
  }

  void visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    printHeader("metatype");
    printRec(M->getInstanceType());
    OS << ')';
  }

  void visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    printHeader("existential-metatype");
    printRec(EM->getInstanceType());
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
    printRec(DM->getMember());
    printRec(DM->getBase());
    OS << ')';
  }
};

TypeRef *decodeDemangleNode(ReflectionContext &RC,
                            Demangle::NodePointer Node);

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF_H
