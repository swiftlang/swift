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
    case TypeInfoKind::Struct:
      printHeader("struct");
      printBasic(TI);
      // FIXME: Use LLVM casts
      printFields(static_cast<const RecordTypeInfo &>(TI));
      OS << ")";
      return;
    case TypeInfoKind::Tuple:
      printHeader("tuple");
      printBasic(TI);
      // FIXME: Use LLVM casts
      printFields(static_cast<const RecordTypeInfo &>(TI));
      OS << ")";
      return;
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
};

class RecordTypeInfoBuilder {
  TypeInfoKind Kind;
  unsigned Size, Alignment, Stride, NumExtraInhabitants;
  std::vector<FieldInfo> Fields;

public:
  RecordTypeInfoBuilder(TypeInfoKind Kind)
    : Kind(Kind), Size(0), Alignment(1), Stride(0),
      NumExtraInhabitants(0) {}

  void addField(const std::string &Name, const TypeInfo &TI) {
    // FIXME: I just made this up
    if (Size == 0)
      NumExtraInhabitants = TI.getNumExtraInhabitants();
    else
      NumExtraInhabitants = 0;

    unsigned fieldSize = TI.getSize();
    unsigned fieldAlignment = TI.getAlignment();

    // Align the current size appropriately
    Size = ((Size + fieldAlignment - 1) & ~(fieldAlignment - 1));

    // Add the field at the aligned offset
    Fields.push_back({Name, Size, TI});

    // Update the aggregate size
    Size += fieldSize;

    // Update the aggregate alignment
    Alignment = std::max(Alignment, fieldAlignment);

    // Re-calculate the stride
    Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
  }

  const RecordTypeInfo *build() {
    return new RecordTypeInfo(Kind, Size, Alignment, Stride,
                              NumExtraInhabitants, Fields);
  }
};

}

class LowerType
  : public TypeRefVisitor<LowerType, const TypeInfo *> {
  TypeConverter &TC;

public:
  using TypeRefVisitor<LowerType, const TypeInfo *>::visit;

  LowerType(TypeConverter &TC) : TC(TC) {}

  const TypeInfo *visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    auto *descriptor = TC.getBuilder().getBuiltinTypeInfo(B);
    assert(descriptor != nullptr);
    return new BuiltinTypeInfo(descriptor);
  }

  const TypeInfo *visitAnyNominalTypeRef(const TypeRef *TR) {
    const FieldDescriptor *FD = TC.getBuilder().getFieldTypeInfo(TR);
    if (FD == nullptr)
      return nullptr;

    switch (FD->Kind) {
    case FieldDescriptorKind::Class:
      // FIXME
      return nullptr;
    case FieldDescriptorKind::Struct: {
      RecordTypeInfoBuilder builder(TypeInfoKind::Struct);
      for (auto Field : TC.getBuilder().getFieldTypeRefs(TR, FD)) {
        auto *FieldTI = TC.getTypeInfo(Field.second);
        if (FieldTI == nullptr)
          return nullptr;
        builder.addField(Field.first, *FieldTI);
      }
      return builder.build();
    }
    case FieldDescriptorKind::Enum:
      // FIXME
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
    RecordTypeInfoBuilder builder(TypeInfoKind::Tuple);
    for (auto Element : T->getElements()) {
      auto *FieldTI = TC.getTypeInfo(Element);
      if (FieldTI == nullptr)
        return nullptr;
      builder.addField("", *FieldTI);
    }
    return builder.build();
  }

  const TypeInfo *visitFunctionTypeRef(const FunctionTypeRef *F) {
    // FIXME
    return nullptr;
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
    // FIXME
    return nullptr;
  }

  const TypeInfo *visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *
  visitUnownedStorageTypeRef(const UnownedStorageTypeRef *US) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *visitWeakStorageTypeRef(const WeakStorageTypeRef *WS) {
    // FIXME
    return nullptr;
  }

  const TypeInfo *
  visitUnmanagedStorageTypeRef(const UnmanagedStorageTypeRef *US) {
    // FIXME
    return nullptr;
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
    Pool.push_back(std::unique_ptr<const TypeInfo>(TI));
  Cache[TR] = TI;

  return TI;
}
