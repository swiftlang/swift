//===--- TypeRef.cpp - Swift Type References for Reflection -----*- C++ -*-===//
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

BuiltinTypeRef::BuiltinTypeRef(StringRef MangledName)
  : TypeRef(TypeRefKind::Builtin), MangledName(MangledName) {}

BuiltinTypeRef *BuiltinTypeRef::create(ReflectionContext &RC, StringRef MangledName) {
  void *Mem = RC.allocate(sizeof(BuiltinTypeRef), alignof(BuiltinTypeRef));
  return new (Mem) BuiltinTypeRef(MangledName);
}

NominalTypeRef::NominalTypeRef(StringRef MangledName)
  : TypeRef(TypeRefKind::Nominal), MangledName(MangledName) {}

NominalTypeRef *NominalTypeRef::create(ReflectionContext &RC, StringRef MangledName) {
  void *Mem = RC.allocate(sizeof(NominalTypeRef), alignof(NominalTypeRef));
  return new (Mem) NominalTypeRef(MangledName);
}

BoundGenericTypeRef::BoundGenericTypeRef(StringRef MangledName,
                    ArrayRef<TypeRef *> GenericParams)
  : TypeRef(TypeRefKind::BoundGeneric), MangledName(MangledName), NumGenericParams(GenericParams.size()) {
    std::uninitialized_copy(GenericParams.begin(), GenericParams.end(), getGenericParameterBuffer());
  }

BoundGenericTypeRef *BoundGenericTypeRef::create(ReflectionContext &RC, StringRef MangledName, ArrayRef<TypeRef *> GenericParams) {
  void *Mem = RC.allocate(sizeof(BoundGenericTypeRef) + GenericParams.size() * sizeof(TypeRef *), alignof(BoundGenericTypeRef));
  return new (Mem) BoundGenericTypeRef(MangledName, GenericParams);
}

TupleTypeRef::TupleTypeRef(ArrayRef<TypeRef *> Elements)
  : TypeRef(TypeRefKind::Tuple), NumElements(Elements.size()) {
    std::uninitialized_copy(Elements.begin(), Elements.end(), getElementBuffer());
}

TupleTypeRef *TupleTypeRef::create(swift::reflection::ReflectionContext &RC, ArrayRef<swift::reflection::TypeRef *> Elements) {
  void *Mem = RC.allocate(sizeof(TupleTypeRef) + Elements.size() * sizeof(TypeRef *), alignof(TupleTypeRef));
  return new (Mem) TupleTypeRef(Elements);
}

FunctionTypeRef::FunctionTypeRef(TypeRef *Input, TypeRef *Result)
  : TypeRef(TypeRefKind::Function), Input(Input), Result(Result) {}

FunctionTypeRef *FunctionTypeRef::create(ReflectionContext &RC, TypeRef *Input, TypeRef *Result) {
  void *Mem = RC.allocate(sizeof(FunctionTypeRef), alignof(FunctionTypeRef));
  return new (Mem) FunctionTypeRef(Input, Result);
}

ProtocolTypeRef::ProtocolTypeRef(StringRef MangledName)
  : TypeRef(TypeRefKind::Protocol),
    MangledName(MangledName) {}

ProtocolTypeRef *ProtocolTypeRef::create(ReflectionContext &RC, StringRef MangledName) {
  void *Mem = RC.allocate(sizeof(ProtocolTypeRef), alignof(ProtocolTypeRef));
  return new (Mem) ProtocolTypeRef(MangledName);
}

ProtocolCompositionTypeRef::ProtocolCompositionTypeRef(ArrayRef<TypeRef *> Protocols) : TypeRef(TypeRefKind::ProtocolComposition) {
  std::uninitialized_copy(Protocols.begin(), Protocols.end(), getProtocolBuffer());
}

ProtocolCompositionTypeRef *ProtocolCompositionTypeRef::create(ReflectionContext &RC, ArrayRef<TypeRef *> Protocols) {
  void *Mem = RC.allocate(sizeof(ProtocolCompositionTypeRef) + Protocols.size() * sizeof(TypeRef *), alignof(ProtocolCompositionTypeRef));
  return new (Mem) ProtocolCompositionTypeRef(Protocols);
}

MetatypeTypeRef::MetatypeTypeRef(TypeRef *InstanceType)
  : TypeRef(TypeRefKind::Metatype), InstanceType(InstanceType) {}

MetatypeTypeRef *MetatypeTypeRef::create(ReflectionContext &RC, TypeRef *InstanceType) {
  void *Mem = RC.allocate(sizeof(MetatypeTypeRef), alignof(MetatypeTypeRef));
  return new (Mem) MetatypeTypeRef(InstanceType);
}

ExistentialMetatypeTypeRef::ExistentialMetatypeTypeRef(TypeRef *InstanceType)
  : TypeRef(TypeRefKind::ExistentialMetatype), InstanceType(InstanceType) {}

ExistentialMetatypeTypeRef *ExistentialMetatypeTypeRef::create(ReflectionContext &RC, TypeRef *InstanceType) {
  void *Mem = RC.allocate(sizeof(ExistentialMetatypeTypeRef), alignof(ExistentialMetatypeTypeRef));
  return new (Mem) ExistentialMetatypeTypeRef(InstanceType);
}

GenericTypeParameterTypeRef::GenericTypeParameterTypeRef(uint32_t Index, uint32_t Depth)
  : TypeRef(TypeRefKind::GenericTypeParameter), Index(Index), Depth(Depth) {}

GenericTypeParameterTypeRef *GenericTypeParameterTypeRef::create(ReflectionContext &RC, uint32_t Index, uint32_t Depth) {
  void *Mem = RC.allocate(sizeof(GenericTypeParameterTypeRef), alignof(GenericTypeParameterTypeRef));
  return new (Mem) GenericTypeParameterTypeRef(Index, Depth);
}

DependentMemberTypeRef::DependentMemberTypeRef(StringRef Name, TypeRef *Base)
  : TypeRef(TypeRefKind::DependentMember), Name(Name), Base(Base) {}

DependentMemberTypeRef *DependentMemberTypeRef::create(ReflectionContext &RC, llvm::StringRef Name, TypeRef *Base) {
  void *Mem = RC.allocate(sizeof(DependentMemberTypeRef), alignof(DependentMemberTypeRef));
  return new (Mem) DependentMemberTypeRef(Name, Base);
}

void TypeRef::dump() const {
  dump(llvm::errs());
}

void TypeRef::dump(llvm::raw_ostream &os, unsigned indent) const {
  PrintTypeRef(os, indent).visit(this);
}
