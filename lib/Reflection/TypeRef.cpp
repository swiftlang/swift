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
#include "llvm/Support/ErrorHandling.h"

using namespace swift;
using namespace reflection;

BuiltinTypeRef::BuiltinTypeRef(StringRef MangledName)
  : TypeRef(TypeRefKind::Builtin), MangledName(MangledName) {}

BuiltinTypeRef *BuiltinTypeRef::create(ReflectionContext &RC,
                                       StringRef MangledName) {
  void *Mem = RC.allocate(sizeof(BuiltinTypeRef), alignof(BuiltinTypeRef));
  return new (Mem) BuiltinTypeRef(RC.allocateCopy(MangledName));
}

NominalTypeRef::NominalTypeRef(StringRef MangledName)
  : TypeRef(TypeRefKind::Nominal), MangledName(MangledName) {}

NominalTypeRef *NominalTypeRef::create(ReflectionContext &RC,
                                       StringRef MangledName) {
  void *Mem = RC.allocate(sizeof(NominalTypeRef), alignof(NominalTypeRef));
  return new (Mem) NominalTypeRef(RC.allocateCopy(MangledName));
}

BoundGenericTypeRef::BoundGenericTypeRef(StringRef MangledName,
                    ArrayRef<TypeRef *> GenericParams)
  : TypeRef(TypeRefKind::BoundGeneric), MangledName(MangledName),
    NumGenericParams(GenericParams.size()) {
    std::uninitialized_copy(GenericParams.begin(), GenericParams.end(),
                            getGenericParameterBuffer());
  }

BoundGenericTypeRef *BoundGenericTypeRef::create(ReflectionContext &RC,
  StringRef MangledName, ArrayRef<TypeRef *> GenericParams) {
  void *Mem = RC.allocate(sizeof(BoundGenericTypeRef) + GenericParams.size()
                          * sizeof(TypeRef *), alignof(BoundGenericTypeRef));
  return new (Mem) BoundGenericTypeRef(RC.allocateCopy(MangledName),
                                       GenericParams);
}

TupleTypeRef::TupleTypeRef(ArrayRef<TypeRef *> Elements)
  : TypeRef(TypeRefKind::Tuple), NumElements(Elements.size()) {
    std::uninitialized_copy(Elements.begin(), Elements.end(),
                            getElementBuffer());
}

TupleTypeRef *TupleTypeRef::create(swift::reflection::ReflectionContext &RC,
  ArrayRef<swift::reflection::TypeRef *> Elements) {
  void *Mem = RC.allocate(sizeof(TupleTypeRef) + Elements.size()
                          * sizeof(TypeRef *), alignof(TupleTypeRef));
  return new (Mem) TupleTypeRef(Elements);
}

FunctionTypeRef::FunctionTypeRef(TypeRef *Input, TypeRef *Result)
  : TypeRef(TypeRefKind::Function), Input(Input), Result(Result) {}

FunctionTypeRef *FunctionTypeRef::create(ReflectionContext &RC, TypeRef *Input,
                                         TypeRef *Result) {
  void *Mem = RC.allocate(sizeof(FunctionTypeRef), alignof(FunctionTypeRef));
  return new (Mem) FunctionTypeRef(Input, Result);
}

ProtocolTypeRef::ProtocolTypeRef(StringRef ModuleName, StringRef Name)
  : TypeRef(TypeRefKind::Protocol),
    ModuleName(ModuleName), Name(Name) {}

ProtocolTypeRef *ProtocolTypeRef::create(ReflectionContext &RC,
                                         StringRef ModuleName, StringRef Name) {
  void *Mem = RC.allocate(sizeof(ProtocolTypeRef), alignof(ProtocolTypeRef));
  return new (Mem) ProtocolTypeRef(RC.allocateCopy(ModuleName),
                                   RC.allocateCopy(Name));
}

ProtocolCompositionTypeRef::ProtocolCompositionTypeRef(
  ArrayRef<TypeRef *> Protocols) : TypeRef(TypeRefKind::ProtocolComposition) {
  std::uninitialized_copy(Protocols.begin(), Protocols.end(),
                          getProtocolBuffer());
}

ProtocolCompositionTypeRef *
ProtocolCompositionTypeRef::create(ReflectionContext &RC,
                                   ArrayRef<TypeRef *> Protocols) {
  void *Mem = RC.allocate(sizeof(ProtocolCompositionTypeRef) + Protocols.size()
                          * sizeof(TypeRef *),
                          alignof(ProtocolCompositionTypeRef));
  return new (Mem) ProtocolCompositionTypeRef(Protocols);
}

MetatypeTypeRef::MetatypeTypeRef(TypeRef *InstanceType)
  : TypeRef(TypeRefKind::Metatype), InstanceType(InstanceType) {}

MetatypeTypeRef *MetatypeTypeRef::create(ReflectionContext &RC,
                                         TypeRef *InstanceType) {
  void *Mem = RC.allocate(sizeof(MetatypeTypeRef), alignof(MetatypeTypeRef));
  return new (Mem) MetatypeTypeRef(InstanceType);
}

ExistentialMetatypeTypeRef::ExistentialMetatypeTypeRef(TypeRef *InstanceType)
  : TypeRef(TypeRefKind::ExistentialMetatype), InstanceType(InstanceType) {}

ExistentialMetatypeTypeRef *
ExistentialMetatypeTypeRef::create(ReflectionContext &RC,
                                   TypeRef *InstanceType) {
  void *Mem = RC.allocate(sizeof(ExistentialMetatypeTypeRef),
                          alignof(ExistentialMetatypeTypeRef));
  return new (Mem) ExistentialMetatypeTypeRef(InstanceType);
}

GenericTypeParameterTypeRef::GenericTypeParameterTypeRef(uint32_t Index,
                                                         uint32_t Depth)
  : TypeRef(TypeRefKind::GenericTypeParameter), Index(Index), Depth(Depth) {}

GenericTypeParameterTypeRef *
GenericTypeParameterTypeRef::create(ReflectionContext &RC, uint32_t Index,
                                    uint32_t Depth) {
  void *Mem = RC.allocate(sizeof(GenericTypeParameterTypeRef),
                          alignof(GenericTypeParameterTypeRef));
  return new (Mem) GenericTypeParameterTypeRef(Index, Depth);
}

DependentMemberTypeRef::DependentMemberTypeRef(TypeRef *Member, TypeRef *Base)
  : TypeRef(TypeRefKind::DependentMember), Member(Member), Base(Base) {}

DependentMemberTypeRef *DependentMemberTypeRef::create(ReflectionContext &RC,
                                                       TypeRef *Member,
                                                       TypeRef *Base) {
  void *Mem = RC.allocate(sizeof(DependentMemberTypeRef),
                          alignof(DependentMemberTypeRef));
  return new (Mem) DependentMemberTypeRef(Member, Base);
}

void TypeRef::dump() const {
  dump(llvm::errs());
}

void TypeRef::dump(llvm::raw_ostream &os, unsigned indent) const {
  PrintTypeRef(os, indent).visit(this);
  os << "\n";
}

TypeRef *reflection::decodeDemangleNode(ReflectionContext &RC,
                                        Demangle::NodePointer Node) {
  using NodeKind = Demangle::Node::Kind;
  switch (Node->getKind()) {
  case NodeKind::Type:
    return decodeDemangleNode(RC, Node->getChild(0));
  case NodeKind::BoundGenericClass:
  case NodeKind::BoundGenericEnum:
  case NodeKind::BoundGenericStructure: {
    auto mangledName = Demangle::mangleNode(Node->getChild(0));
    auto genericArgs = Node->getChild(1);
    llvm::SmallVector<TypeRef *, 4> Params;
    for (auto genericArg : *genericArgs) {
      Params.push_back(decodeDemangleNode(RC, genericArg));
    }
    return BoundGenericTypeRef::create(RC, mangledName, Params);
  }
  case NodeKind::Class:
  case NodeKind::Enum:
  case NodeKind::Structure: {
    auto mangledName = Demangle::mangleNode(Node);
    return NominalTypeRef::create(RC, mangledName);
  }
  case NodeKind::BuiltinTypeName: {
    auto mangledName = Demangle::mangleNode(Node);
    return BuiltinTypeRef::create(RC, mangledName);
  }
  case NodeKind::ExistentialMetatype: {
    auto instance = decodeDemangleNode(RC, Node->getChild(0));
    return ExistentialMetatypeTypeRef::create(RC, instance);
  }
  case NodeKind::Metatype: {
    auto instance = decodeDemangleNode(RC, Node->getChild(0));
    return MetatypeTypeRef::create(RC, instance);
  }
  case NodeKind::Protocol: {
    auto moduleName = Node->getChild(0)->getText();
    auto name = Node->getChild(1)->getText();
    return ProtocolTypeRef::create(RC, moduleName, name);
  }
  case NodeKind::DependentGenericParamType: {
    auto index = Node->getChild(0)->getIndex();
    auto depth = Node->getChild(1)->getIndex();
    return GenericTypeParameterTypeRef::create(RC, index, depth);
  }
  case NodeKind::FunctionType: {
    auto input = decodeDemangleNode(RC, Node->getChild(0));
    auto result = decodeDemangleNode(RC, Node->getChild(1));
    return FunctionTypeRef::create(RC, input, result);
  }
  case NodeKind::ArgumentTuple:
    return decodeDemangleNode(RC, Node->getChild(0));
  case NodeKind::ReturnType:
    return decodeDemangleNode(RC, Node->getChild(0));
  case NodeKind::NonVariadicTuple: {
    llvm::SmallVector<TypeRef *, 4> Elements;
    for (auto element : *Node) {
      Elements.push_back(decodeDemangleNode(RC, element));
    }
    return TupleTypeRef::create(RC, Elements);
  }
  case NodeKind::TupleElement:
    return decodeDemangleNode(RC, Node->getChild(0));
  case NodeKind::DependentGenericType: {
    return decodeDemangleNode(RC, Node->getChild(1));
  }
  case NodeKind::DependentMemberType: {
    auto member = decodeDemangleNode(RC, Node->getChild(0));
    auto base = decodeDemangleNode(RC, Node->getChild(1));
    return DependentMemberTypeRef::create(RC, member, base);
  }
  case NodeKind::DependentAssociatedTypeRef:
    return decodeDemangleNode(RC, Node->getChild(0));
  default:
    llvm_unreachable("Can't decode demangle node of this type");
  }
}
