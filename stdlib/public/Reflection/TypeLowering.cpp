//===--- TypeLowering.cpp - Swift Type Lowering for Reflection ------------===//
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
// Implements logic for computing in-memory layouts from TypeRefs loaded from
// reflection metadata.
//
//===----------------------------------------------------------------------===//

#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"

#include <iostream>

using namespace swift;
using namespace reflection;

void TypeInfo::dump() const {
  dump(std::cerr);
}

namespace {

class PrintTypeInfo {
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

  void printRec(const TypeInfo &TI) {
    OS << "\n";

    Indent += 2;
    print(TI);
    Indent -= 2;
  }

  void printBasic(const TypeInfo &TI) {
    printField("size", TI.getSize());
    printField("alignment", TI.getAlignment());
    printField("stride", TI.getStride());
    printField("num_extra_inhabitants", TI.getNumExtraInhabitants());
  }

  void printFields(const RecordTypeInfo &TI) {
    Indent += 2;
    for (auto Field : TI.getFields()) {
      OS << "\n";
      printHeader("field");
      if (!Field.Name.empty())
        printField("name", Field.Name);
      printField("offset", Field.Offset);
      printRec(Field.TI);
      OS << ")";
    }
    Indent -= 2;
  }

public:
  PrintTypeInfo(std::ostream &OS, unsigned Indent)
    : OS(OS), Indent(Indent) {}

  void print(const TypeInfo &TI) {
    switch (TI.getKind()) {
    case TypeInfoKind::Builtin:
      printHeader("builtin");
      printBasic(TI);
      OS << ")";
      return;

    case TypeInfoKind::Record: {
      auto &RecordTI = cast<RecordTypeInfo>(TI);
      switch (RecordTI.getRecordKind()) {
      case RecordKind::Struct:
        printHeader("struct");
        break;
      case RecordKind::Tuple:
        printHeader("tuple");
        break;
      case RecordKind::ThickFunction:
        printHeader("thick_function");
        break;
      }
      printBasic(TI);
      printFields(RecordTI);
      OS << ")";
      return;
    }

    case TypeInfoKind::Reference: {
      printHeader("reference");
      auto &ReferenceTI = cast<ReferenceTypeInfo>(TI);
      switch (ReferenceTI.getReferenceKind()) {
      case ReferenceKind::Strong:
        printField("kind", "strong");
        break;
      case ReferenceKind::Unowned:
        printField("kind", "unowned");
        break;
      case ReferenceKind::Weak:
        printField("kind", "weak");
        break;
      case ReferenceKind::Unmanaged:
        printField("kind", "unmanaged");
        break;
      }

      switch (ReferenceTI.getReferenceCounting()) {
      case ReferenceCounting::Native:
        printField("refcounting", "native");
        break;
      case ReferenceCounting::Unknown:
        printField("refcounting", "unknown");
        break;
      }

      OS << ")";
      return;
    }
    }

    assert(false && "Bad TypeInfo kind");
  }
};

}

void TypeInfo::dump(std::ostream &OS, unsigned Indent) const {
  PrintTypeInfo(OS, Indent).print(*this);
}

namespace {

class BuiltinTypeInfo : public TypeInfo {
public:
  BuiltinTypeInfo(const BuiltinTypeDescriptor *descriptor)
    : TypeInfo(TypeInfoKind::Builtin,
               descriptor->Size, descriptor->Alignment,
               descriptor->Stride, descriptor->NumExtraInhabitants) {}

  static bool classof(const TypeInfo *TI) {
    return TI->getKind() == TypeInfoKind::Builtin;
  }
};

class RecordTypeInfoBuilder {
  TypeConverter &TC;
  unsigned Size, Alignment, Stride, NumExtraInhabitants;
  RecordKind Kind;
  std::vector<FieldInfo> Fields;
  bool Invalid;

public:
  RecordTypeInfoBuilder(TypeConverter &TC, RecordKind Kind)
    : TC(TC), Size(0), Alignment(1), Stride(0), NumExtraInhabitants(0),
      Kind(Kind), Invalid(false) {}

  void addField(const std::string &Name, const TypeRef *TR) {
    const TypeInfo *TI = TC.getTypeInfo(TR);
    if (TI == nullptr) {
      Invalid = true;
      return;
    }

    // FIXME: I just made this up
    if (Size == 0)
      NumExtraInhabitants = TI->getNumExtraInhabitants();
    else
      NumExtraInhabitants = 0;

    unsigned fieldSize = TI->getSize();
    unsigned fieldAlignment = TI->getAlignment();

    // Align the current size appropriately
    Size = ((Size + fieldAlignment - 1) & ~(fieldAlignment - 1));

    // Add the field at the aligned offset
    Fields.push_back({Name, Size, TR, *TI});

    // Update the aggregate size
    Size += fieldSize;

    // Update the aggregate alignment
    Alignment = std::max(Alignment, fieldAlignment);

    // Re-calculate the stride
    Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
  }

  const RecordTypeInfo *build() {
    if (Invalid)
      return nullptr;

    return TC.makeTypeInfo<RecordTypeInfo>(
        Size, Alignment, Stride,
        NumExtraInhabitants, Kind, Fields);
  }
};

}

const ReferenceTypeInfo *
TypeConverter::getReferenceTypeInfo(ReferenceKind Kind,
                                    ReferenceCounting Refcounting) {
  auto key = std::make_pair(unsigned(Kind), unsigned(Refcounting));
  auto found = ReferenceCache.find(key);
  if (found != ReferenceCache.end())
    return found->second;

  const TypeRef *TR;
  switch (Refcounting) {
  case ReferenceCounting::Native:
    TR = getNativeObjectTypeRef();
    break;
  case ReferenceCounting::Unknown:
    TR = getUnknownObjectTypeRef();
    break;
  }

  // FIXME: Weak, Unowned references have different extra inhabitants
  auto *BuiltinTI = Builder.getBuiltinTypeInfo(TR);
  if (BuiltinTI == nullptr)
    return nullptr;

  auto *TI = makeTypeInfo<ReferenceTypeInfo>(BuiltinTI->Size,
                                             BuiltinTI->Alignment,
                                             BuiltinTI->Stride,
                                             BuiltinTI->NumExtraInhabitants,
                                             Kind, Refcounting);
  ReferenceCache[key] = TI;
  return TI;
}

const TypeInfo *
TypeConverter::getThickFunctionTypeInfo() {
  if (ThickFunctionTI != nullptr)
    return ThickFunctionTI;

  RecordTypeInfoBuilder builder(*this, RecordKind::ThickFunction);
  builder.addField("function", getRawPointerTypeRef());
  builder.addField("context", getNativeObjectTypeRef());
  ThickFunctionTI = builder.build();

  return ThickFunctionTI;
}

const TypeRef *TypeConverter::getRawPointerTypeRef() {
  if (RawPointerTR != nullptr)
    return RawPointerTR;

  RawPointerTR = BuiltinTypeRef::create(Builder, "Bp");
  return RawPointerTR;
}

const TypeRef *TypeConverter::getNativeObjectTypeRef() {
  if (NativeObjectTR != nullptr)
    return NativeObjectTR;

  NativeObjectTR = BuiltinTypeRef::create(Builder, "Bo");
  return NativeObjectTR;
}

const TypeRef *TypeConverter::getUnknownObjectTypeRef() {
  if (UnknownObjectTR != nullptr)
    return UnknownObjectTR;

  UnknownObjectTR = BuiltinTypeRef::create(Builder, "BO");
  return UnknownObjectTR;
}

class LowerType
  : public TypeRefVisitor<LowerType, const TypeInfo *> {
  TypeConverter &TC;

public:
  using TypeRefVisitor<LowerType, const TypeInfo *>::visit;

  LowerType(TypeConverter &TC) : TC(TC) {}

  const TypeInfo *visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    if (B->getMangledName() == "Bo") {
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Native);
    } else if (B->getMangledName() == "BO") {
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Unknown);
    }

    auto *descriptor = TC.getBuilder().getBuiltinTypeInfo(B);
    assert(descriptor != nullptr);
    return TC.makeTypeInfo<BuiltinTypeInfo>(descriptor);
  }

  const TypeInfo *visitAnyNominalTypeRef(const TypeRef *TR) {
    const FieldDescriptor *FD = TC.getBuilder().getFieldTypeInfo(TR);
    if (FD == nullptr)
      return nullptr;

    switch (FD->Kind) {
    case FieldDescriptorKind::Class:
      // FIXME: need to know if this is a NativeObject or UnknownObject
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Native);
    case FieldDescriptorKind::Struct: {
      RecordTypeInfoBuilder builder(TC, RecordKind::Struct);
      for (auto Field : TC.getBuilder().getFieldTypeRefs(TR, FD))
        builder.addField(Field.first, Field.second);
      return builder.build();
    }
    case FieldDescriptorKind::Enum: {
      unsigned NoPayloadCases = 0;
      unsigned PayloadCases = 0;
      const TypeInfo *PayloadTI = nullptr;

      for (auto Field : TC.getBuilder().getFieldTypeRefs(TR, FD)) {
        if (Field.second == nullptr) {
          NoPayloadCases++;
          continue;
        }

        PayloadCases++;
        PayloadTI = TC.getTypeInfo(Field.second);
        if (PayloadTI == nullptr)
          return nullptr;
      }

      // FIXME: Implement remaining cases
      //
      // Also this is wrong if we wrap a reference in multiple levels of
      // optionality
      if (NoPayloadCases == 1 &&
          PayloadCases == 1 &&
          isa<ReferenceTypeInfo>(PayloadTI))
        return PayloadTI;

      return nullptr;
    }
    case FieldDescriptorKind::ObjCProtocol:
    case FieldDescriptorKind::ClassProtocol:
    case FieldDescriptorKind::Protocol:
      return nullptr;
    }
  }

  const TypeInfo *visitNominalTypeRef(const NominalTypeRef *N) {
    return visitAnyNominalTypeRef(N);
  }

  const TypeInfo *visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    return visitAnyNominalTypeRef(BG);
  }

  const TypeInfo *visitTupleTypeRef(const TupleTypeRef *T) {
    RecordTypeInfoBuilder builder(TC, RecordKind::Tuple);
    for (auto Element : T->getElements())
      builder.addField("", Element);
    return builder.build();
  }

  const TypeInfo *visitFunctionTypeRef(const FunctionTypeRef *F) {
    switch (F->getFlags().getConvention()) {
    case FunctionMetadataConvention::Swift:
      return TC.getThickFunctionTypeInfo();
    case FunctionMetadataConvention::Block:
      // FIXME: Native convention if blocks are ever supported on Linux?
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Unknown);
    case FunctionMetadataConvention::Thin:
    case FunctionMetadataConvention::CFunctionPointer:
      return TC.getTypeInfo(TC.getRawPointerTypeRef());
    }
  }

  const TypeInfo *visitProtocolTypeRef(const ProtocolTypeRef *P) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    assert(false && "Must have concrete TypeRef");
    return nullptr;
  }

  const TypeInfo *
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    assert(false && "Must have concrete TypeRef");
    return nullptr;
  }

  const TypeInfo *visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                   ReferenceCounting::Unknown);
  }

  const TypeInfo *visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                   ReferenceCounting::Unknown);
  }

  template<typename StorageTypeRef>
  const TypeInfo *
  visitAnyStorageTypeRef(const StorageTypeRef *S, ReferenceKind Kind) {
    auto *TI = TC.getTypeInfo(S->getType());
    if (TI == nullptr)
      return nullptr;

    // FIXME: This might also be a class existential
    return TC.getReferenceTypeInfo(Kind,
        cast<ReferenceTypeInfo>(TI)->getReferenceCounting());
  }

  const TypeInfo *
  visitUnownedStorageTypeRef(const UnownedStorageTypeRef *US) {
    return visitAnyStorageTypeRef(US, ReferenceKind::Unowned);
  }

  const TypeInfo *visitWeakStorageTypeRef(const WeakStorageTypeRef *WS) {
    return visitAnyStorageTypeRef(WS, ReferenceKind::Weak);
  }

  const TypeInfo *
  visitUnmanagedStorageTypeRef(const UnmanagedStorageTypeRef *US) {
    return visitAnyStorageTypeRef(US, ReferenceKind::Unmanaged);
  }

  const TypeInfo *visitOpaqueTypeRef(const OpaqueTypeRef *Op) {
    assert(false && "Can't lower opaque TypeRef");
    return nullptr;
  }
};

const TypeInfo *TypeConverter::getTypeInfo(const TypeRef *TR) {
  auto found = Cache.find(TR);
  if (found != Cache.end()) {
    auto *TI = found->second;
    assert(TI != nullptr && "TypeRef recursion detected");
    return TI;
  }

  // Detect recursion
  Cache[TR] = nullptr;

  auto *TI = LowerType(*this).visit(TR);

  // Cache the result
  if (TI != nullptr)
    Cache[TR] = TI;

  return TI;
}
