//===--- TypeLowering.cpp - Swift Type Lowering for Reflection ------------===//
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
// Implements logic for computing in-memory layouts from TypeRefs loaded from
// reflection metadata.
//
// This has to match up with layout algorithms used in IRGen and the runtime,
// and a bit of SIL type lowering to boot.
//
//===----------------------------------------------------------------------===//

#include "swift/ABI/Enum.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "swift/Reflection/TypeRefBuilder.h"
#include "swift/Runtime/Unreachable.h"

#include <iostream>

#ifdef DEBUG_TYPE_LOWERING
  #define DEBUG_LOG(expr) expr;
#else
  #define DEBUG_LOG(expr)
#endif

namespace swift {
namespace reflection {

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
    printField("bitwise_takable", TI.isBitwiseTakable());
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
      case RecordKind::Invalid:
        printHeader("invalid");
        break;
      case RecordKind::Struct:
        printHeader("struct");
        break;
      case RecordKind::NoPayloadEnum:
        printHeader("no_payload_enum");
        break;
      case RecordKind::SinglePayloadEnum:
        printHeader("single_payload_enum");
        break;
      case RecordKind::MultiPayloadEnum:
        printHeader("multi_payload_enum");
        break;
      case RecordKind::Tuple:
        printHeader("tuple");
        break;
      case RecordKind::ThickFunction:
        printHeader("thick_function");
        break;
      case RecordKind::OpaqueExistential:
        printHeader("opaque_existential");
        break;
      case RecordKind::ClassExistential:
        printHeader("class_existential");
        break;
      case RecordKind::ErrorExistential:
        printHeader("error_existential");
        break;
      case RecordKind::ExistentialMetatype:
        printHeader("existential_metatype");
        break;
      case RecordKind::ClassInstance:
        printHeader("class_instance");
        break;
      case RecordKind::ClosureContext:
        printHeader("closure_context");
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
      case ReferenceKind::Strong: printField("kind", "strong"); break;
#define REF_STORAGE(Name, name, ...) \
      case ReferenceKind::Name: printField("kind", #name); break;
#include "swift/AST/ReferenceStorage.def"
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

    swift_runtime_unreachable("Bad TypeInfo kind");
  }
};

} // end anonymous namespace

void TypeInfo::dump(std::ostream &OS, unsigned Indent) const {
  PrintTypeInfo(OS, Indent).print(*this);
  OS << '\n';
}

BuiltinTypeInfo::BuiltinTypeInfo(const BuiltinTypeDescriptor *descriptor)
    : TypeInfo(TypeInfoKind::Builtin,
               descriptor->Size,
               descriptor->getAlignment(),
               descriptor->Stride,
               descriptor->NumExtraInhabitants,
               descriptor->isBitwiseTakable()),
      Name(descriptor->getMangledTypeName(0)) {}

/// Utility class for building values that contain witness tables.
class ExistentialTypeInfoBuilder {
  TypeConverter &TC;
  std::vector<const TypeRef *> Protocols;
  const TypeRef *Superclass = nullptr;
  ExistentialTypeRepresentation Representation;
  ReferenceCounting Refcounting;
  bool ObjC;
  unsigned WitnessTableCount;
  bool Invalid;

  bool isSingleError() const {
    // If we changed representation, it means we added a
    // superclass constraint or an AnyObject member.
    if (Representation != ExistentialTypeRepresentation::Opaque)
      return false;

    if (Protocols.size() != 1)
      return false;

    if (Superclass)
      return false;

    for (auto *P : Protocols) {
      if (auto *NTD = dyn_cast<NominalTypeRef>(P))
        if (NTD->isErrorProtocol())
          return true;
    }
    return false;
  }

  void examineProtocols() {
    if (isSingleError()) {
      Representation = ExistentialTypeRepresentation::Error;
      // No extra witness table for protocol<Error>
      return;
    }

    for (auto *P : Protocols) {
      auto *NTD = dyn_cast<NominalTypeRef>(P);
      auto *OP = dyn_cast<ObjCProtocolTypeRef>(P);
      if (!NTD && !OP) {
        DEBUG_LOG(std::cerr << "Bad protocol: "; P->dump())
        Invalid = true;
        continue;
      }

      // Don't look up field info for imported Objective-C protocols.
      if (OP) {
        ObjC = true;
        continue;
      }

      std::pair<const FieldDescriptor *, const ReflectionInfo *> FD =
          TC.getBuilder().getFieldTypeInfo(P);
      if (FD.first == nullptr) {
        DEBUG_LOG(std::cerr << "No field descriptor: "; P->dump())
        Invalid = true;
        continue;
      }

      switch (FD.first->Kind) {
        case FieldDescriptorKind::ObjCProtocol:
          // Objective-C protocols do not have any witness tables.
          ObjC = true;
          continue;
        case FieldDescriptorKind::ClassProtocol:
          Representation = ExistentialTypeRepresentation::Class;
          WitnessTableCount++;

          if (auto *Superclass = TC.getBuilder().lookupSuperclass(P)) {
            auto *SuperclassTI = TC.getTypeInfo(Superclass);
            if (SuperclassTI == nullptr) {
              DEBUG_LOG(std::cerr << "No TypeInfo for superclass: ";
                        Superclass->dump());
              Invalid = true;
              continue;
            }

            if (!isa<ReferenceTypeInfo>(SuperclassTI)) {
              DEBUG_LOG(std::cerr << "Superclass not a reference type: ";
                        SuperclassTI->dump());
              Invalid = true;
              continue;
            }

            if (cast<ReferenceTypeInfo>(SuperclassTI)->getReferenceCounting()
                == ReferenceCounting::Native) {
              Refcounting = ReferenceCounting::Native;
            }
          }

          continue;
        case FieldDescriptorKind::Protocol:
          WitnessTableCount++;
          continue;
        case FieldDescriptorKind::ObjCClass:
        case FieldDescriptorKind::Struct:
        case FieldDescriptorKind::Enum:
        case FieldDescriptorKind::MultiPayloadEnum:
        case FieldDescriptorKind::Class:
          Invalid = true;
          continue;
      }
    }
  }

public:
  ExistentialTypeInfoBuilder(TypeConverter &TC)
    : TC(TC), Representation(ExistentialTypeRepresentation::Opaque),
      Refcounting(ReferenceCounting::Unknown),
      ObjC(false), WitnessTableCount(0),
      Invalid(false) {}

  void addProtocol(const TypeRef *P) {
    Protocols.push_back(P);
  }

  void addProtocolComposition(const ProtocolCompositionTypeRef *PC) {
    for (auto *T : PC->getProtocols()) {
      addProtocol(T);
    }

    if (PC->hasExplicitAnyObject())
      addAnyObject();

    if (auto *T = PC->getSuperclass()) {
      // Anything else should either be a superclass constraint, or
      // we have an invalid typeref.
      if (!isa<NominalTypeRef>(T) &&
          !isa<BoundGenericTypeRef>(T) &&
          !isa<ObjCClassTypeRef>(T)) {
        DEBUG_LOG(std::cerr << "Bad existential member: "; T->dump())
        Invalid = true;
        return;
      }

      // Don't look up field info for imported Objective-C classes.
      if (auto *OC = dyn_cast<ObjCClassTypeRef>(T)) {
        addAnyObject();
        return;
      }

      const auto &FD = TC.getBuilder().getFieldTypeInfo(T);
      if (FD.first == nullptr) {
        DEBUG_LOG(std::cerr << "No field descriptor: "; T->dump())
        Invalid = true;
        return;
      }

      // We have a valid superclass constraint. It only affects
      // lowering by class-constraining the entire existential.
      switch (FD.first->Kind) {
      case FieldDescriptorKind::Class:
        Refcounting = ReferenceCounting::Native;
        LLVM_FALLTHROUGH;

      case FieldDescriptorKind::ObjCClass:
        addAnyObject();
        break;

      default:
        DEBUG_LOG(std::cerr << "Bad existential member: "; T->dump())
        Invalid = true;
        return;
      }
    }
  }

  void addAnyObject() {
    Representation = ExistentialTypeRepresentation::Class;
  }

  void markInvalid() {
    Invalid = true;
  }

  const TypeInfo *build() {
    examineProtocols();

    if (Invalid)
      return nullptr;

    if (ObjC) {
      if (WitnessTableCount > 0) {
        DEBUG_LOG(std::cerr << "@objc existential with witness tables\n");
        return nullptr;
      }

      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     Refcounting);
    }

    RecordKind Kind;
    switch (Representation) {
    case ExistentialTypeRepresentation::Class:
      Kind = RecordKind::ClassExistential;
      break;
    case ExistentialTypeRepresentation::Opaque:
      Kind = RecordKind::OpaqueExistential;
      break;
    case ExistentialTypeRepresentation::Error:
      Kind = RecordKind::ErrorExistential;
      break;
    }

    RecordTypeInfoBuilder builder(TC, Kind);

    switch (Representation) {
    case ExistentialTypeRepresentation::Class:
      // Class existentials consist of a single retainable pointer
      // followed by witness tables.
      if (Refcounting == ReferenceCounting::Unknown)
        builder.addField("object", TC.getUnknownObjectTypeRef());
      else
        builder.addField("object", TC.getNativeObjectTypeRef());
      break;
    case ExistentialTypeRepresentation::Opaque: {
      auto *TI = TC.getTypeInfo(TC.getRawPointerTypeRef());
      if (TI == nullptr) {
        DEBUG_LOG(std::cerr << "No TypeInfo for RawPointer\n");
        return nullptr;
      }

      // Non-class existentials consist of a three-word buffer,
      // value metadata, and finally zero or more witness tables.
      // The buffer is always bitwise takable, since non-bitwise
      // takable payloads are stored out of line.
      builder.addField(TI->getSize() * 3,
                       TI->getAlignment(),
                       /*numExtraInhabitants=*/0,
                       /*bitwiseTakable=*/true);
      builder.addField("metadata", TC.getAnyMetatypeTypeRef());
      break;
    }
    case ExistentialTypeRepresentation::Error:
      builder.addField("error", TC.getUnknownObjectTypeRef());
      break;
    }

    for (unsigned i = 0; i < WitnessTableCount; i++)
      builder.addField("wtable", TC.getRawPointerTypeRef());

    return builder.build();
  }

  const TypeInfo *buildMetatype() {
    examineProtocols();

    if (Invalid)
      return nullptr;

    if (ObjC) {
      if (WitnessTableCount > 0) {
        DEBUG_LOG(std::cerr << "@objc existential with witness tables\n");
        return nullptr;
      }

      return TC.getAnyMetatypeTypeInfo();
    }

    RecordTypeInfoBuilder builder(TC, RecordKind::ExistentialMetatype);

    builder.addField("metadata", TC.getAnyMetatypeTypeRef());
    for (unsigned i = 0; i < WitnessTableCount; i++)
      builder.addField("wtable", TC.getRawPointerTypeRef());

    return builder.build();
  }
};

unsigned RecordTypeInfoBuilder::addField(unsigned fieldSize,
                                         unsigned fieldAlignment,
                                         unsigned numExtraInhabitants,
                                         bool bitwiseTakable) {
  assert(fieldAlignment > 0);

  // Align the current size appropriately
  Size = ((Size + fieldAlignment - 1) & ~(fieldAlignment - 1));

  // Record the offset
  unsigned offset = Size;

  // Update the aggregate size
  Size += fieldSize;

  // Update the aggregate alignment
  Alignment = std::max(Alignment, fieldAlignment);

  // The aggregate is bitwise takable if all elements are.
  BitwiseTakable &= bitwiseTakable;

  switch (Kind) {
  // The extra inhabitants of a struct or tuple are the same as the extra
  // inhabitants of the field that has the most.
  // Opaque existentials pick up the extra inhabitants of their type metadata
  // field.
  case RecordKind::Struct:
  case RecordKind::OpaqueExistential:
  case RecordKind::Tuple:
    NumExtraInhabitants = std::max(NumExtraInhabitants, numExtraInhabitants);
    break;
  
  // For other kinds of records, we only use the extra inhabitants of the
  // first field.
  case RecordKind::ClassExistential:
  case RecordKind::ClassInstance:
  case RecordKind::ClosureContext:
  case RecordKind::ErrorExistential:
  case RecordKind::ExistentialMetatype:
  case RecordKind::Invalid:
  case RecordKind::MultiPayloadEnum:
  case RecordKind::NoPayloadEnum:
  case RecordKind::SinglePayloadEnum:
  case RecordKind::ThickFunction:
    if (Empty) {
      NumExtraInhabitants = numExtraInhabitants;
    }
    break;
  }
  Empty = false;

  return offset;
}

void RecordTypeInfoBuilder::addField(const std::string &Name,
                                     const TypeRef *TR) {
  const TypeInfo *TI = TC.getTypeInfo(TR);
  if (TI == nullptr) {
    DEBUG_LOG(std::cerr << "No TypeInfo for field type: "; TR->dump());
    Invalid = true;
    return;
  }

  unsigned offset = addField(TI->getSize(),
                             TI->getAlignment(),
                             TI->getNumExtraInhabitants(),
                             TI->isBitwiseTakable());
  Fields.push_back({Name, offset, TR, *TI});
}

const RecordTypeInfo *RecordTypeInfoBuilder::build() {
  if (Invalid)
    return nullptr;

  // Calculate the stride
  unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
  if (Stride == 0)
    Stride = 1;

  return TC.makeTypeInfo<RecordTypeInfo>(
      Size, Alignment, Stride,
      NumExtraInhabitants, BitwiseTakable,
      Kind, Fields);
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

  // Unowned and unmanaged references have the same extra inhabitants
  // as the underlying type.
  //
  // Weak references do not have any extra inhabitants.

  auto *BuiltinTI = Builder.getBuiltinTypeInfo(TR);
  if (BuiltinTI == nullptr) {
    DEBUG_LOG(std::cerr << "No TypeInfo for reference type: "; TR->dump());
    return nullptr;
  }

  unsigned numExtraInhabitants = BuiltinTI->NumExtraInhabitants;
  bool bitwiseTakable = true;

  switch (Kind) {
  case ReferenceKind::Strong:
    break;
  case ReferenceKind::Weak:
    numExtraInhabitants = 0;
    bitwiseTakable = false;
    break;
  case ReferenceKind::Unowned:
    if (Refcounting == ReferenceCounting::Unknown)
      bitwiseTakable = false;
    break;
  case ReferenceKind::Unmanaged:
    break;
  }

  auto *TI = makeTypeInfo<ReferenceTypeInfo>(BuiltinTI->Size,
                                             BuiltinTI->getAlignment(),
                                             BuiltinTI->Stride,
                                             numExtraInhabitants,
                                             bitwiseTakable,
                                             Kind, Refcounting);
  ReferenceCache[key] = TI;
  return TI;
}

/// Thick functions consist of a function pointer. We do not use
/// Builtin.RawPointer here, since the extra inhabitants differ.
const TypeInfo *
TypeConverter::getThinFunctionTypeInfo() {
  if (ThinFunctionTI != nullptr)
    return ThinFunctionTI;

  auto *descriptor = getBuilder().getBuiltinTypeInfo(
      getThinFunctionTypeRef());
  if (descriptor == nullptr) {
    DEBUG_LOG(std::cerr << "No TypeInfo for function type\n");
    return nullptr;
  }

  ThinFunctionTI = makeTypeInfo<BuiltinTypeInfo>(descriptor);

  return ThinFunctionTI;
}

/// Thick functions consist of a function pointer and nullable retainable
/// context pointer. The context is modeled exactly like a native Swift
/// class reference.
const TypeInfo *
TypeConverter::getThickFunctionTypeInfo() {
  if (ThickFunctionTI != nullptr)
    return ThickFunctionTI;

  RecordTypeInfoBuilder builder(*this, RecordKind::ThickFunction);
  builder.addField("function", getThinFunctionTypeRef());
  builder.addField("context", getNativeObjectTypeRef());
  ThickFunctionTI = builder.build();

  return ThickFunctionTI;
}

/// Thick metatypes consist of a single pointer, possibly followed
/// by witness tables. We do not use Builtin.RawPointer here, since
/// the extra inhabitants differ.
const TypeInfo *
TypeConverter::getAnyMetatypeTypeInfo() {
  if (AnyMetatypeTI != nullptr)
    return AnyMetatypeTI;

  auto *descriptor = getBuilder().getBuiltinTypeInfo(
      getAnyMetatypeTypeRef());
  if (descriptor == nullptr) {
    DEBUG_LOG(std::cerr << "No TypeInfo for metatype type\n");
    return nullptr;
  }

  AnyMetatypeTI = makeTypeInfo<BuiltinTypeInfo>(descriptor);

  return AnyMetatypeTI;
}

const TypeInfo *TypeConverter::getEmptyTypeInfo() {
  if (EmptyTI != nullptr)
    return EmptyTI;

  EmptyTI = makeTypeInfo<TypeInfo>(TypeInfoKind::Builtin,
                                   /*Size=*/0,
                                   /*Alignment=*/1,
                                   /*Stride=*/1,
                                   /*ExtraInhabitants=*/0,
                                   /*BitwiseTakable=*/true);
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

const TypeRef *TypeConverter::getThinFunctionTypeRef() {
  if (ThinFunctionTR != nullptr)
    return ThinFunctionTR;

  ThinFunctionTR = BuiltinTypeRef::create(Builder, "yyXf");
  return ThinFunctionTR;
}

const TypeRef *TypeConverter::getAnyMetatypeTypeRef() {
  if (AnyMetatypeTR != nullptr)
    return AnyMetatypeTR;

  AnyMetatypeTR = BuiltinTypeRef::create(Builder, "ypXp");
  return AnyMetatypeTR;
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

/// Visitor class to determine if a type has a fixed size.
///
/// Conservative approximation.
class HasFixedSize
  : public TypeRefVisitor<HasFixedSize, bool> {

public:
  HasFixedSize() {}

  using TypeRefVisitor<HasFixedSize, bool>::visit;

  bool visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return true;
  }

  bool visitNominalTypeRef(const NominalTypeRef *N) {
    return true;
  }

  bool visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    if (BG->isClass())
      return true;
    for (auto Arg : BG->getGenericParams()) {
      if (!visit(Arg))
        return false;
    }
    return true;
  }

  bool visitTupleTypeRef(const TupleTypeRef *T) {
    for (auto Element : T->getElements())
      if (!visit(Element))
        return false;
    return true;
  }

  bool visitFunctionTypeRef(const FunctionTypeRef *F) {
    return true;
  }

  bool
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    return true;
  }

  bool visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    return true;
  }

  bool
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    return true;
  }

  bool
  visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return true;
  }

  bool
  visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return true;
  }

  bool visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return true;
  }

  bool visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OP) {
    return true;
  }

#define REF_STORAGE(Name, ...) \
  bool \
  visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return true; \
  }
#include "swift/AST/ReferenceStorage.def"

  bool
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    return false;
  }

  bool
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    return false;
  }

  bool visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return false;
  }
};

bool TypeConverter::hasFixedSize(const TypeRef *TR) {
  return HasFixedSize().visit(TR);
}

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

public:
  HasSingletonMetatype() {}

  using TypeRefVisitor<HasSingletonMetatype, MetatypeRepresentation>::visit;

  MetatypeRepresentation visitBuiltinTypeRef(const BuiltinTypeRef *B) {
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation visitNominalTypeRef(const NominalTypeRef *N) {
    if (N->isClass())
      return MetatypeRepresentation::Thick;
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation visitBoundGenericTypeRef(const BoundGenericTypeRef *BG) {
    if (BG->isClass())
      return MetatypeRepresentation::Thick;
    return MetatypeRepresentation::Thin;
  }

  MetatypeRepresentation visitTupleTypeRef(const TupleTypeRef *T) {
    auto result = MetatypeRepresentation::Thin;
    for (auto Element : T->getElements())
      result = combineRepresentations(result, visit(Element));
    return result;
  }

  MetatypeRepresentation visitFunctionTypeRef(const FunctionTypeRef *F) {
    auto result = visit(F->getResult());
    for (const auto &Param : F->getParameters())
      result = combineRepresentations(result, visit(Param.getType()));
    return result;
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
    DEBUG_LOG(std::cerr << "Unresolved generic TypeRef: "; GTP->dump());
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    DEBUG_LOG(std::cerr << "Unresolved generic TypeRef: "; DM->dump());
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation
  visitForeignClassTypeRef(const ForeignClassTypeRef *F) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation visitObjCClassTypeRef(const ObjCClassTypeRef *OC) {
    return MetatypeRepresentation::Unknown;
  }

  MetatypeRepresentation visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OP) {
    return MetatypeRepresentation::Unknown;
  }

#define REF_STORAGE(Name, ...) \
  MetatypeRepresentation \
  visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return MetatypeRepresentation::Unknown; \
  }
#include "swift/AST/ReferenceStorage.def"

  MetatypeRepresentation visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    return MetatypeRepresentation::Unknown;
  }
};

class EnumTypeInfoBuilder {
  TypeConverter &TC;
  unsigned Size, Alignment, NumExtraInhabitants;
  bool BitwiseTakable;
  RecordKind Kind;
  std::vector<FieldInfo> Cases;
  bool Invalid;

  const TypeRef *getCaseTypeRef(FieldTypeInfo Case) {
    // An indirect case is like a payload case with an argument type
    // of Builtin.NativeObject.
    if (Case.Indirect)
      return TC.getNativeObjectTypeRef();

    return Case.TR;
  }

  void addCase(const std::string &Name, const TypeRef *TR,
               const TypeInfo *TI) {
    if (TI == nullptr) {
      DEBUG_LOG(std::cerr << "No TypeInfo for case type: "; TR->dump());
      Invalid = true;
      return;
    }

    Size = std::max(Size, TI->getSize());
    Alignment = std::max(Alignment, TI->getAlignment());
    BitwiseTakable &= TI->isBitwiseTakable();

    Cases.push_back({Name, /*offset=*/0, TR, *TI});
  }

public:
  EnumTypeInfoBuilder(TypeConverter &TC)
    : TC(TC), Size(0), Alignment(1), NumExtraInhabitants(0),
      BitwiseTakable(true), Kind(RecordKind::Invalid), Invalid(false) {}

  const TypeInfo *
  build(const TypeRef *TR,
        const std::pair<const FieldDescriptor *, const ReflectionInfo *> &FD) {
    // Sort enum into payload and no-payload cases.
    unsigned NoPayloadCases = 0;
    std::vector<FieldTypeInfo> PayloadCases;

    std::vector<FieldTypeInfo> Fields;
    if (!TC.getBuilder().getFieldTypeRefs(TR, FD, Fields)) {
      Invalid = true;
      return nullptr;
    }

    for (auto Case : Fields) {
      if (Case.TR == nullptr) {
        NoPayloadCases++;
        continue;
      }

      PayloadCases.push_back(Case);
    }

    // NoPayloadEnumImplStrategy
    if (PayloadCases.empty()) {
      Kind = RecordKind::NoPayloadEnum;
      Size += getEnumTagCounts(/*size=*/0,
                               NoPayloadCases,
                               /*payloadCases=*/0).numTagBytes;

    // SinglePayloadEnumImplStrategy
    } else if (PayloadCases.size() == 1) {
      auto *CaseTR = getCaseTypeRef(PayloadCases[0]);
      auto *CaseTI = TC.getTypeInfo(CaseTR);

      // An enum consisting of a single payload case and nothing else
      // is lowered as the payload type.
      if (NoPayloadCases == 0)
        return CaseTI;

      Kind = RecordKind::SinglePayloadEnum;
      addCase(PayloadCases[0].Name, CaseTR, CaseTI);

      // If we were unable to lower the payload type, do not proceed
      // further.
      if (CaseTI != nullptr) {
        // Below logic should match the runtime function
        // swift_initEnumMetadataSinglePayload().
        NumExtraInhabitants = CaseTI->getNumExtraInhabitants();
        if (NumExtraInhabitants >= NoPayloadCases) {
          // Extra inhabitants can encode all no-payload cases.
          NumExtraInhabitants -= NoPayloadCases;
        } else {
          // Not enough extra inhabitants for all cases. We have to add an
          // extra tag field.
          NumExtraInhabitants = 0;
          Size += getEnumTagCounts(Size,
                                   NoPayloadCases - NumExtraInhabitants,
                                   /*payloadCases=*/1).numTagBytes;
        }
      }

    // MultiPayloadEnumImplStrategy
    } else {
      Kind = RecordKind::MultiPayloadEnum;

      // Check if this is a dynamic or static multi-payload enum
      for (auto Case : PayloadCases) {
        auto *CaseTR = getCaseTypeRef(Case);
        auto *CaseTI = TC.getTypeInfo(CaseTR);
        addCase(Case.Name, CaseTR, CaseTI);
      }

      // If we have a fixed descriptor for this type, it is a fixed-size
      // multi-payload enum that possibly uses payload spare bits.
      auto *FixedDescriptor = TC.getBuilder().getBuiltinTypeInfo(TR);
      if (FixedDescriptor) {
        Size = FixedDescriptor->Size;
        Alignment = FixedDescriptor->getAlignment();
        NumExtraInhabitants = FixedDescriptor->NumExtraInhabitants;
        BitwiseTakable = FixedDescriptor->isBitwiseTakable();
      } else {
        // Dynamic multi-payload enums always use an extra tag to differentiate
        // between cases
        auto tagCounts = getEnumTagCounts(Size, NoPayloadCases,
                                          PayloadCases.size());
        
        Size += tagCounts.numTagBytes;
        // Dynamic multi-payload enums use the tag representations not assigned
        // to cases for extra inhabitants.
        if (tagCounts.numTagBytes >= 32) {
          NumExtraInhabitants = INT_MAX;
        } else {
          NumExtraInhabitants =
            (1 << (tagCounts.numTagBytes * 8)) - tagCounts.numTags;
        }
      }
    }

    if (Invalid)
      return nullptr;

    // Calculate the stride
    unsigned Stride = ((Size + Alignment - 1) & ~(Alignment - 1));
    if (Stride == 0)
      Stride = 1;

    return TC.makeTypeInfo<RecordTypeInfo>(
        Size, Alignment, Stride,
        NumExtraInhabitants, BitwiseTakable,
        Kind, Cases);
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
    if (descriptor == nullptr) {
      DEBUG_LOG(std::cerr << "No TypeInfo for builtin type: "; B->dump());
      return nullptr;
    }
    return TC.makeTypeInfo<BuiltinTypeInfo>(descriptor);
  }

  const TypeInfo *visitAnyNominalTypeRef(const TypeRef *TR) {
    const auto &FD = TC.getBuilder().getFieldTypeInfo(TR);
    if (FD.first == nullptr || FD.first->isStruct()) {
      // Maybe this type is opaque -- look for a builtin
      // descriptor to see if we at least know its size
      // and alignment.
      if (auto ImportedTypeDescriptor = TC.getBuilder().getBuiltinTypeInfo(TR))
        return TC.makeTypeInfo<BuiltinTypeInfo>(ImportedTypeDescriptor);

      // Otherwise, we're out of luck.
      if (FD.first == nullptr) {
        DEBUG_LOG(std::cerr << "No TypeInfo for nominal type: "; TR->dump());
        return nullptr;
      }
    }

    switch (FD.first->Kind) {
    case FieldDescriptorKind::Class:
      // A value of class type is a single retainable pointer.
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Native);
    case FieldDescriptorKind::Struct: {
      // Lower the struct's fields using substitutions from the
      // TypeRef to make field types concrete.
      RecordTypeInfoBuilder builder(TC, RecordKind::Struct);

      std::vector<FieldTypeInfo> Fields;
      if (!TC.getBuilder().getFieldTypeRefs(TR, FD, Fields))
        return nullptr;

      for (auto Field : Fields)
        builder.addField(Field.Name, Field.TR);
      return builder.build();
    }
    case FieldDescriptorKind::Enum:
    case FieldDescriptorKind::MultiPayloadEnum: {
      EnumTypeInfoBuilder builder(TC);
      return builder.build(TR, FD);
    }
    case FieldDescriptorKind::ObjCClass:
      return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                     ReferenceCounting::Unknown);
    case FieldDescriptorKind::ObjCProtocol:
    case FieldDescriptorKind::ClassProtocol:
    case FieldDescriptorKind::Protocol:
      DEBUG_LOG(std::cerr << "Invalid field descriptor: "; TR->dump());
      return nullptr;
    }

    swift_runtime_unreachable("Unhandled FieldDescriptorKind in switch.");
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
      return TC.getTypeInfo(TC.getThinFunctionTypeRef());
    }

    swift_runtime_unreachable("Unhandled FunctionMetadataConvention in switch.");
  }

  const TypeInfo *
  visitProtocolCompositionTypeRef(const ProtocolCompositionTypeRef *PC) {
    ExistentialTypeInfoBuilder builder(TC);
    builder.addProtocolComposition(PC);
    return builder.build();
  }

  const TypeInfo *visitMetatypeTypeRef(const MetatypeTypeRef *M) {
    switch (HasSingletonMetatype().visit(M)) {
    case MetatypeRepresentation::Unknown:
      DEBUG_LOG(std::cerr << "Unknown metatype representation: "; M->dump());
      return nullptr;
    case MetatypeRepresentation::Thin:
      return TC.getEmptyTypeInfo();
    case MetatypeRepresentation::Thick:
      return TC.getTypeInfo(TC.getAnyMetatypeTypeRef());
    }

    swift_runtime_unreachable("Unhandled MetatypeRepresentation in switch.");
  }

  const TypeInfo *
  visitExistentialMetatypeTypeRef(const ExistentialMetatypeTypeRef *EM) {
    ExistentialTypeInfoBuilder builder(TC);
    auto *TR = EM->getInstanceType();

    if (auto *PC = dyn_cast<ProtocolCompositionTypeRef>(TR)) {
      builder.addProtocolComposition(PC);
    } else {
      DEBUG_LOG(std::cerr << "Invalid existential metatype: "; EM->dump());
      return nullptr;
    }

    return builder.buildMetatype();
  }

  const TypeInfo *
  visitGenericTypeParameterTypeRef(const GenericTypeParameterTypeRef *GTP) {
    DEBUG_LOG(std::cerr << "Unresolved generic TypeRef: "; GTP->dump());
    return nullptr;
  }

  const TypeInfo *
  visitDependentMemberTypeRef(const DependentMemberTypeRef *DM) {
    DEBUG_LOG(std::cerr << "Unresolved generic TypeRef: "; DM->dump());
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

  const TypeInfo *visitObjCProtocolTypeRef(const ObjCProtocolTypeRef *OP) {
    return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                   ReferenceCounting::Unknown);
  }

  // Apply a storage qualifier, like 'weak', 'unowned' or 'unowned(unsafe)'
  // to a type with reference semantics, such as a class reference or
  // class-bound existential.
  const TypeInfo *
  rebuildStorageTypeInfo(const TypeInfo *TI, ReferenceKind Kind) {
    // If we can't lower the original storage type, give up.
    if (TI == nullptr) {
      DEBUG_LOG(std::cerr << "Invalid reference type");
      return nullptr;
    }

    // Simple case: Just change the reference kind
    if (auto *ReferenceTI = dyn_cast<ReferenceTypeInfo>(TI))
      return TC.getReferenceTypeInfo(Kind, ReferenceTI->getReferenceCounting());

    if (auto *RecordTI = dyn_cast<RecordTypeInfo>(TI)) {
      auto SubKind = RecordTI->getRecordKind();

      // Look through optionals.
      if (SubKind == RecordKind::SinglePayloadEnum) {

        if (Kind == ReferenceKind::Weak) {
          auto *TI = TC.getTypeInfo(RecordTI->getFields()[0].TR);
          return rebuildStorageTypeInfo(TI, Kind);
        }

      // Class existentials are represented as record types.
      // Destructure the existential and replace the "object"
      // field with the right reference kind.
      } else if (SubKind == RecordKind::ClassExistential) {
        bool BitwiseTakable = RecordTI->isBitwiseTakable();
        std::vector<FieldInfo> Fields;
        for (auto &Field : RecordTI->getFields()) {
          if (Field.Name == "object") {
            auto *FieldTI = rebuildStorageTypeInfo(&Field.TI, Kind);
            BitwiseTakable &= FieldTI->isBitwiseTakable();
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
            BitwiseTakable,
            SubKind, Fields);
      }
    }

    // Anything else -- give up
    DEBUG_LOG(std::cerr << "Invalid reference type");
    return nullptr;
  }

  const TypeInfo *
  visitAnyStorageTypeRef(const TypeRef *TR, ReferenceKind Kind) {
    return rebuildStorageTypeInfo(TC.getTypeInfo(TR), Kind);
  }

#define REF_STORAGE(Name, name, ...) \
  const TypeInfo * \
  visit##Name##StorageTypeRef(const Name##StorageTypeRef *US) { \
    return visitAnyStorageTypeRef(US->getType(), ReferenceKind::Name); \
  }
#include "swift/AST/ReferenceStorage.def"

  const TypeInfo *visitSILBoxTypeRef(const SILBoxTypeRef *SB) {
    return TC.getReferenceTypeInfo(ReferenceKind::Strong,
                                   ReferenceCounting::Native);
  }

  const TypeInfo *visitOpaqueTypeRef(const OpaqueTypeRef *O) {
    DEBUG_LOG(std::cerr << "Can't lower opaque TypeRef");
    return nullptr;
  }
};

const TypeInfo *TypeConverter::getTypeInfo(const TypeRef *TR) {
  // See if we already computed the result
  auto found = Cache.find(TR);
  if (found != Cache.end())
    return found->second;

  // Detect invalid recursive value types (IRGen should not emit
  // them in the first place, but there might be bugs)
  if (!RecursionCheck.insert(TR).second) {
    DEBUG_LOG(std::cerr << "TypeRef recursion detected");
    return nullptr;
  }

  // Compute the result and cache it
  auto *TI = LowerType(*this).visit(TR);
  Cache[TR] = TI;

  RecursionCheck.erase(TR);

  return TI;
}

const TypeInfo *TypeConverter::getClassInstanceTypeInfo(const TypeRef *TR,
                                                        unsigned start) {
  std::pair<const FieldDescriptor *, const ReflectionInfo *> FD =
      getBuilder().getFieldTypeInfo(TR);
  if (FD.first == nullptr) {
    DEBUG_LOG(std::cerr << "No field descriptor: "; TR->dump());
    return nullptr;
  }

  switch (FD.first->Kind) {
  case FieldDescriptorKind::Class:
  case FieldDescriptorKind::ObjCClass: {
    // Lower the class's fields using substitutions from the
    // TypeRef to make field types concrete.
    RecordTypeInfoBuilder builder(*this, RecordKind::ClassInstance);

    std::vector<FieldTypeInfo> Fields;
    if (!getBuilder().getFieldTypeRefs(TR, FD, Fields))
      return nullptr;

    // Start layout from the given instance start offset. This should
    // be the superclass instance size.
    builder.addField(/*size=*/start,
                     /*alignment=*/1,
                     /*numExtraInhabitants=*/0,
                     /*bitwiseTakable=*/true);

    for (auto Field : Fields)
      builder.addField(Field.Name, Field.TR);
    return builder.build();
  }
  case FieldDescriptorKind::Struct:
  case FieldDescriptorKind::Enum:
  case FieldDescriptorKind::MultiPayloadEnum:
  case FieldDescriptorKind::ObjCProtocol:
  case FieldDescriptorKind::ClassProtocol:
  case FieldDescriptorKind::Protocol:
    // Invalid field descriptor.
    DEBUG_LOG(std::cerr << "Invalid field descriptor: "; TR->dump());
    return nullptr;
  }

  swift_runtime_unreachable("Unhandled FieldDescriptorKind in switch.");
}

} // namespace reflection
} // namespace swift
