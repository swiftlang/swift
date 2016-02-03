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

#ifndef SWIFT_REFLECTION_TYPEREF
#define SWIFT_REFLECTION_TYPEREF

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"

namespace swift {
namespace reflection {

using llvm::ArrayRef;
using llvm::StringRef;
using llvm::cast;

enum class TypeRefKind {
#define TYPEREF(Id, Parent) Id,
#include "swift/Reflection/TypeRefs.def"
#undef TYPEREF
};

class ReflectionContext {
  llvm::BumpPtrAllocator Allocator;

public:
  void *allocate(size_t Bytes, size_t Alignment) {
    return Allocator.Allocate(Bytes, Alignment);
  }

  template <typename T, typename It>
  T *allocateCopy(It Begin, It End) {
    T *Result = static_cast<T *>(allocate(sizeof(T) * (End - Begin),
                                          alignof(T)));
    for (size_t i = 0; Begin != End; ++Begin, ++i)
      new (Result + i) T(*Begin);
    return Result;
  }

  template <typename T>
  llvm::MutableArrayRef<T> allocateCopy(llvm::ArrayRef<T> Array) {
    return llvm::MutableArrayRef<T>(allocateCopy<T>(Array.begin(), Array.end()),
                              Array.size());
  }

  StringRef allocateCopy(StringRef Str) {
    llvm::ArrayRef<char> Result =
    allocateCopy(llvm::makeArrayRef(Str.data(), Str.size()));
    return StringRef(Result.data(), Result.size());
  }
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
  BuiltinTypeRef *create(ReflectionContext &RC, StringRef MangledName);

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
  NominalTypeRef *create(ReflectionContext &RC, StringRef MangledName);

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
  BoundGenericTypeRef *create(ReflectionContext &RC, StringRef MangledName,
                              ArrayRef<TypeRef *> GenericParams);

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
  TupleTypeRef *create(ReflectionContext &RC, ArrayRef<TypeRef *> Elements);

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
  FunctionTypeRef *create(ReflectionContext &RC, TypeRef *Input,
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
  StringRef MangledName;

  ProtocolTypeRef(StringRef MangledName);

public:
  ProtocolTypeRef *create(ReflectionContext &RC, StringRef MangledName);

  StringRef getMangledName() const {
    return MangledName;
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
  ProtocolCompositionTypeRef *create(ReflectionContext &RC,
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
  MetatypeTypeRef *create(ReflectionContext &RC, TypeRef *InstanceType);

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
  ExistentialMetatypeTypeRef *create(ReflectionContext &RC,
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

  GenericTypeParameterTypeRef *create(ReflectionContext &RC, uint32_t Index,
                                      uint32_t Depth);

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
  StringRef Name;
  TypeRef *Base;

  DependentMemberTypeRef(StringRef Name, TypeRef *Base);
public:
  DependentMemberTypeRef *create(ReflectionContext &RC, StringRef Name,
                                 TypeRef *Base);

  StringRef getName() const {
    return Name;
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
    OS << " " << name << ": " << value;
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
    printField("type", B->getMangledName());
  }

  void visitNominalTypeRef(const NominalTypeRef *N) {
    printHeader("nominal");
    printField("type", N->getMangledName());
  }

  void visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    printHeader("bound-generic");
    for (auto param : BG->getGenericParams())
      printRec(param);
  }

  void visitTupleTypeRef(const TupleTypeRef *T) {
    printHeader("tuple");
    for (auto element : T->getElements())
      printRec(element);
  }

  void visitFunctionTypeRef(const FunctionTypeRef *F) {
    printHeader("function");
    printRec(F->getInput());
    printRec(F->getResult());
  }

  void visitProtocolTypeRef(const ProtocolTypeRef *P) {
    printHeader("protocol");
    printField("type", P->getMangledName());
  }

  void visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    printHeader("protocol-composition");
    for (auto protocol : PC->getProtocols())
      printRec(protocol);
  }

  void visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    printHeader("metatype");
    printRec(M->getInstanceType());
  }

  void visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    printHeader("existential-metatype");
    printRec(EM->getInstanceType());
  }

  void visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP){
    printHeader("generic-type-parameter");
    printField("index", GTP->getIndex());
    printField("depth", GTP->getDepth());
  }

  void visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    printHeader("dependent-member");
    printRec(DM->getBase());
    printField("name", DM->getName());
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREF
