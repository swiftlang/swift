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
// This has to match up with layout algorithms used in IRGen and the runtime,
// and a bit of SIL type lowering to boot.
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

  std::ostream &printHeader(const std::string &name) {
    indent(Indent) << '(' << name;
    return OS;
  }

  template<typename T>
  std::ostream &printField(const std::string &name, const T &value) {
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
      case RecordKind::Existential:
        printHeader("existential");
        break;
      case RecordKind::ClassExistential:
        printHeader("class_existential");
        break;
      case RecordKind::ExistentialMetatype:
        printHeader("existential_metatype");
        break;
      case RecordKind::ClassInstance:
        printHeader("class_instance");
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
  OS << '\n';
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

/// Utility class for performing universal layout for types such as
/// tuples, structs, thick functions, etc.
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

  unsigned addField(unsigned fieldSize, unsigned fieldAlignment) {
    // Align the current size appropriately
    Size = ((Size + fieldAlignment - 1) & ~(fieldAlignment - 1));

    // Record the offset
    unsigned offset = Size;

    // Update the aggregate size
    Size += fieldSize;

    // Update the aggregate alignment
    Alignment = std::max(Alignment, fieldAlignment);

    // Re-calculate the stride
    Stride = ((Size + Alignment - 1) & ~(Alignment - 1));

    return offset;
  }

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

    unsigned fieldOffset = addField(fieldSize, fieldAlignment);
    Fields.push_back({Name, fieldOffset, TR, *TI});
  }

  const RecordTypeInfo *build() {
    if (Invalid)
      return nullptr;

    return TC.makeTypeInfo<RecordTypeInfo>(
        Size, Alignment, Stride,
        NumExtraInhabitants, Kind, Fields);
  }
};

/// Utility class for building values that contain witness tables.
class ExistentialTypeInfoBuilder {
  TypeConverter &TC;
  bool ObjC;
  bool Class;
  unsigned WitnessTableCount;
  bool Invalid;

public:
  ExistentialTypeInfoBuilder(TypeConverter &TC)
    : TC(TC), ObjC(false), Class(false), WitnessTableCount(0),
      Invalid(false) {}

  void addProtocol(const TypeRef *TR) {
    auto *P = dyn_cast<ProtocolTypeRef>(TR);
    if (P == nullptr) {
      Invalid = true;
      return;
    }

    // FIXME: AnyObject should go away
    if (P->getMangledName() == "Ps9AnyObject_") {
      // No witness table for AnyObject
      Class = true;
      return;
    }

    const FieldDescriptor *FD = TC.getBuilder().getFieldTypeInfo(TR);
    if (FD == nullptr) {
      Invalid = true;
      return;
    }

    switch (FD->Kind) {
    case FieldDescriptorKind::ObjCProtocol:
      // Objective-C protocols do not have any witness tables.
      ObjC = true;
      break;
    case FieldDescriptorKind::ClassProtocol:
      Class = true;
      WitnessTableCount++;
      break;
    case FieldDescriptorKind::Protocol:
      WitnessTableCount++;
      break;
    case FieldDescriptorKind::Struct:
    case FieldDescriptorKind::Enum:
    case FieldDescriptorKind::Class:
      Invalid = true;
      break;
    }
  }

  const TypeInfo *build() {
    if (Invalid)
      return nullptr;

    if (ObjC) {
      if (WitnessTableCount > 0)
        return nullptr;

      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Unknown);
    }

    RecordTypeInfoBuilder builder(TC,
                                  Class
                                    ? RecordKind::ClassExistential
                                    : RecordKind::Existential);

    if (Class) {
      // Class existentials consist of a single retainable pointer
      // followed by witness tables.
      builder.addField("object", TC.getUnknownObjectTypeRef());
    } else {
      // Non-class existentials consist of a three-word buffer and
      // value metadata, followed by witness tables.
      builder.addField("value", TC.getRawPointerTypeRef());
      builder.addField("value", TC.getRawPointerTypeRef());
      builder.addField("value", TC.getRawPointerTypeRef());
      builder.addField("metadata", TC.getRawPointerTypeRef());
    }

    for (unsigned i = 0; i < WitnessTableCount; i++)
      builder.addField("wtable", TC.getRawPointerTypeRef());

    return builder.build();
  }

  const TypeInfo *buildMetatype() {
    if (Invalid)
      return nullptr;

    if (ObjC) {
      if (WitnessTableCount > 0)
        return nullptr;

      return TC.getTypeInfo(TC.getRawPointerTypeRef());
    }

    RecordTypeInfoBuilder builder(TC, RecordKind::ExistentialMetatype);

    builder.addField("metadata", TC.getRawPointerTypeRef());
    for (unsigned i = 0; i < WitnessTableCount; i++)
      builder.addField("wtable", TC.getRawPointerTypeRef());

    return builder.build();
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

/// Thick functions consist of a function pointer and nullable retainable
/// context pointer. The context is modeled exactly like a native Swift
/// class reference.
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

const TypeInfo *TypeConverter::getEmptyTypeInfo() {
  if (EmptyTI != nullptr)
    return EmptyTI;

  EmptyTI = makeTypeInfo<TypeInfo>(TypeInfoKind::Builtin, 0, 1, 0, 0);
  return EmptyTI;
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

enum class MetatypeRepresentation : unsigned {
  /// Singleton metatype values are empty.
  Thin,

  /// Metatypes containing classes, or where the original unsubstituted
  /// type contains a type parameter, must be represented as pointers
  /// to metadata structures.
  Thick,

  /// Insufficient information to determine which.
  Unknown
};

MetatypeRepresentation combineRepresentations(MetatypeRepresentation rep1,
                                              MetatypeRepresentation rep2) {
  if (rep1 == rep2)
    return rep1;

  if (rep1 == MetatypeRepresentation::Unknown ||
      rep2 == MetatypeRepresentation::Unknown)
    return MetatypeRepresentation::Unknown;

  if (rep1 == MetatypeRepresentation::Thick ||
      rep2 == MetatypeRepresentation::Thick)
    return MetatypeRepresentation::Thick;

  return MetatypeRepresentation::Thin;
}

/// Visitor class to determine if a metatype should use the empty
/// representation.
///
/// This relies on substitution correctly setting wasAbstract() on
/// MetatypeTypeRefs.
class HasSingletonMetatype
  : public TypeRefVisitor<HasSingletonMetatype, MetatypeRepresentation> {
  TypeRefBuilder &Builder;

public:
  HasSingletonMetatype(TypeRefBuilder &Builder) : Builder(Builder) {}

  using TypeRefVisitor<HasSingletonMetatype, MetatypeRepresentation>::visit;

  MetatypeRepresentation visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation visitAnyNominalTypeRef(const TypeRef *TR) {
    const FieldDescriptor *FD = Builder.getFieldTypeInfo(TR);
    if (FD == nullptr)
      return MetatypeRepresentation::Unknown;

    switch (FD->Kind) {
    case FieldDescriptorKind::Class:
      // Classes can have subclasses, so the metatype is always thick.
      return MetatypeRepresentation::Thick;

    case FieldDescriptorKind::Struct:
    case FieldDescriptorKind::Enum:
      return MetatypeRepresentation::Thin;

    case FieldDescriptorKind::ObjCProtocol:
    case FieldDescriptorKind::ClassProtocol:
    case FieldDescriptorKind::Protocol:
      // Invalid field descriptor.
      return MetatypeRepresentation::Unknown;
    }
  }

  MetatypeRepresentation visitNominalTypeRef(const NominalTypeRef *N) {
    return visitAnyNominalTypeRef(N);
  }

  MetatypeRepresentation visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    return visitAnyNominalTypeRef(BG);
  }

  MetatypeRepresentation visitTupleTypeRef(const TupleTypeRef *T) {
    auto result = MetatypeRepresentation::Thin;
    for (auto Element : T->getElements())
      result = combineRepresentations(result, visit(Element));
    return result;
  }

  MetatypeRepresentation visitFunctionTypeRef(const FunctionTypeRef *F) {
    auto result = visit(F->getResult());
    for (auto Arg : F->getArguments())
      result = combineRepresentations(result, visit(Arg));
    return result;
  }

  MetatypeRepresentation visitProtocolTypeRef(const ProtocolTypeRef *P) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    if (M->wasAbstract())
      return MetatypeRepresentation::Thick;
    return visit(M->getInstanceType());
  }

  MetatypeRepresentation
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation
  visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    assert(false && "Must have concrete TypeRef");
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    assert(false && "Must have concrete TypeRef");
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitUnownedStorageTypeRef(const UnownedStorageTypeRef *US) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation visitWeakStorageTypeRef(const WeakStorageTypeRef *WS) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitUnmanagedStorageTypeRef(const UnmanagedStorageTypeRef *US) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return MetatypeRepresentation::Unknown;
  }
};

class LowerType
  : public TypeRefVisitor<LowerType, const TypeInfo *> {
  TypeConverter &TC;

public:
  using TypeRefVisitor<LowerType, const TypeInfo *>::visit;

  LowerType(TypeConverter &TC) : TC(TC) {}

  const TypeInfo *visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    /// The context field of a thick function is a Builtin.NativeObject.
    /// Since we want this to round-trip, lower these as reference
    /// types.
    if (B->getMangledName() == "Bo") {
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Native);
    } else if (B->getMangledName() == "BO") {
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Unknown);
    }

    /// Otherwise, get the fixed layout information from reflection
    /// metadata.
    auto *descriptor = TC.getBuilder().getBuiltinTypeInfo(B);
    if (descriptor == nullptr)
      return nullptr;
    return TC.makeTypeInfo<BuiltinTypeInfo>(descriptor);
  }

  const TypeInfo *visitAnyNominalTypeRef(const TypeRef *TR) {
    const FieldDescriptor *FD = TC.getBuilder().getFieldTypeInfo(TR);
    if (FD == nullptr)
      return nullptr;

    switch (FD->Kind) {
    case FieldDescriptorKind::Class:
      // A value of class type is a single retainable pointer.
      //
      // FIXME: need to know if this is a NativeObject or UnknownObject
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Native);
    case FieldDescriptorKind::Struct: {
      // Lower the struct's fields using substitutions from the
      // TypeRef to make field types concrete.
      RecordTypeInfoBuilder builder(TC, RecordKind::Struct);
      for (auto Field : TC.getBuilder().getFieldTypeRefs(TR, FD))
        builder.addField(Field.first, Field.second);
      return builder.build();
    }
    case FieldDescriptorKind::Enum: {
      // Sort enum into payload and no-payload cases.
      unsigned NoPayloadCases = 0;
      unsigned PayloadCases = 0;
      const TypeInfo *PayloadTI = nullptr;

      for (auto Field : TC.getBuilder().getFieldTypeRefs(TR, FD)) {
        if (Field.second == nullptr) {
          NoPayloadCases++;
          continue;
        }

        // FIXME: If the unsubstituted payload type is empty, but not
        // resilient, we treat the case as a no-payload case.
        //
        // This should be handled by IRGen emitting the enum strategy
        // explicitly.
        PayloadCases++;
        PayloadTI = TC.getTypeInfo(Field.second);
        if (PayloadTI == nullptr)
          return nullptr;
      }

      // FIXME: Implement remaining enum layout strategies
      //
      // Also this is wrong if we wrap a reference in multiple levels of
      // optionality
      if (NoPayloadCases == 1 && PayloadCases == 1) {
        if (isa<ReferenceTypeInfo>(PayloadTI))
          return PayloadTI;

        if (auto *RecordTI = dyn_cast<RecordTypeInfo>(PayloadTI)) {
          auto SubKind = RecordTI->getRecordKind();
          if (SubKind == RecordKind::ClassExistential)
            return PayloadTI;
        }
      }

      return nullptr;
    }
    case FieldDescriptorKind::ObjCProtocol:
    case FieldDescriptorKind::ClassProtocol:
    case FieldDescriptorKind::Protocol:
      // Invalid field descriptor
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
    ExistentialTypeInfoBuilder builder(TC);
    builder.addProtocol(P);
    return builder.build();
  }

  const TypeInfo *
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    ExistentialTypeInfoBuilder builder(TC);
    for (auto *P : PC->getProtocols())
      builder.addProtocol(P);
    return builder.build();
  }

  const TypeInfo *visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    switch (HasSingletonMetatype(TC.getBuilder()).visit(M)) {
    case MetatypeRepresentation::Unknown:
      return nullptr;
    case MetatypeRepresentation::Thin:
      return TC.getEmptyTypeInfo();
    case MetatypeRepresentation::Thick:
      return TC.getTypeInfo(TC.getRawPointerTypeRef());
    }
  }

  const TypeInfo *
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    ExistentialTypeInfoBuilder builder(TC);
    auto *TR = EM->getInstanceType();

    if (auto *P = dyn_cast<ProtocolTypeRef>(TR)) {
      builder.addProtocol(P);
    } else if (auto *PC = dyn_cast<ProtocolCompositionTypeRef>(TR)) {
      for (auto *P : PC->getProtocols())
        builder.addProtocol(P);
    } else {
      // Invalid TypeRef
      return nullptr;
    }

    return builder.buildMetatype();
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

  // Apply a storage qualifier, like 'weak', 'unowned' or 'unowned(unsafe)'
  // to a type with reference semantics, such as a class reference or
  // class-bound existential.
  const TypeInfo *
  rebuildStorageTypeInfo(const TypeInfo *TI, ReferenceKind Kind) {
    // If we can't lower the original storage type, give up.
    if (TI == nullptr)
      return nullptr;

    // Simple case: Just change the reference kind
    if (auto *ReferenceTI = dyn_cast<ReferenceTypeInfo>(TI))
      return TC.getReferenceTypeInfo(Kind, ReferenceTI->getReferenceCounting());

    // Class existentials are represented as record types.
    // Destructure the existential and replace the "object"
    // field with the right reference kind.
    if (auto *RecordTI = dyn_cast<RecordTypeInfo>(TI)) {
      auto SubKind = RecordTI->getRecordKind();
      if (SubKind == RecordKind::ClassExistential) {
        std::vector<FieldInfo> Fields;
        for (auto &Field : RecordTI->getFields()) {
          if (Field.Name == "object") {
            auto *FieldTI = rebuildStorageTypeInfo(&Field.TI, Kind);
            Fields.push_back({Field.Name, Field.Offset, Field.TR, *FieldTI});
            continue;
          }
          Fields.push_back(Field);
        }

        return TC.makeTypeInfo<RecordTypeInfo>(
            RecordTI->getSize(),
            RecordTI->getAlignment(),
            RecordTI->getStride(),
            RecordTI->getNumExtraInhabitants(),
            SubKind, Fields);
      }
    }

    // Anything else -- give up
    return nullptr;
  }

  const TypeInfo *
  visitAnyStorageTypeRef(const TypeRef *TR, ReferenceKind Kind) {
    return rebuildStorageTypeInfo(TC.getTypeInfo(TR), Kind);
  }

  const TypeInfo *
  visitUnownedStorageTypeRef(const UnownedStorageTypeRef *US) {
    return visitAnyStorageTypeRef(US->getType(), ReferenceKind::Unowned);
  }

  const TypeInfo *visitWeakStorageTypeRef(const WeakStorageTypeRef *WS) {
    return visitAnyStorageTypeRef(WS->getType(), ReferenceKind::Weak);
  }

  const TypeInfo *
  visitUnmanagedStorageTypeRef(const UnmanagedStorageTypeRef *US) {
    return visitAnyStorageTypeRef(US->getType(), ReferenceKind::Unmanaged);
  }

  const TypeInfo *visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                   ReferenceCounting::Native);
  }

  const TypeInfo *visitOpaqueTypeRef(const OpaqueTypeRef *O) {
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

const TypeInfo *TypeConverter::getClassInstanceTypeInfo(const TypeRef *TR,
                                                        unsigned start,
                                                        unsigned align) {
  const FieldDescriptor *FD = getBuilder().getFieldTypeInfo(TR);
  if (FD == nullptr)
    return nullptr;

  switch (FD->Kind) {
  case FieldDescriptorKind::Class: {
    // Lower the class's fields using substitutions from the
    // TypeRef to make field types concrete.
    RecordTypeInfoBuilder builder(*this, RecordKind::ClassInstance);

    // Start layout from the given instance start offset.
    builder.addField(start, align);

    for (auto Field : getBuilder().getFieldTypeRefs(TR, FD))
      builder.addField(Field.first, Field.second);
    return builder.build();
  }
  case FieldDescriptorKind::Struct:
  case FieldDescriptorKind::Enum:
  case FieldDescriptorKind::ObjCProtocol:
  case FieldDescriptorKind::ClassProtocol:
  case FieldDescriptorKind::Protocol:
    // Invalid field descriptor.
    return nullptr;
  }
}
