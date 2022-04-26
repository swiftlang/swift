//===--- TypeRefBuilder.h - Swift Type Reference Builder --------*- C++ -*-===//
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
// Implements utilities for constructing TypeRefs and looking up field and
// enum case types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REFLECTION_TYPEREFBUILDER_H
#define SWIFT_REFLECTION_TYPEREFBUILDER_H

#include "swift/Remote/MetadataReader.h"
#include "swift/Reflection/MetadataSourceBuilder.h"
#include "swift/Reflection/Records.h"
#include "swift/Reflection/TypeLowering.h"
#include "swift/Reflection/TypeRef.h"
#include "llvm/ADT/Optional.h"
#include <iomanip>
#include <iostream>
#include <ostream>
#include <unordered_map>
#include <vector>

namespace swift {
namespace reflection {

using remote::RemoteRef;

template <typename Runtime> class ReflectionContext;

template <typename Iterator>
class ReflectionSection {
  using const_iterator = Iterator;
  RemoteRef<void> Start;
  uint64_t Size;

public:
  ReflectionSection(RemoteRef<void> Start, uint64_t Size)
    : Start(Start), Size(Size) {}

  RemoteRef<void> startAddress() const {
    return Start;
  }

  RemoteRef<void> endAddress() const {
    return Start.atByteOffset(Size);
  }

  const_iterator begin() const {
    return const_iterator(Start, Size);
  }

  const_iterator end() const {
    return const_iterator(endAddress(), 0);
  }

  size_t size() const {
    return Size;
  }
  
  bool containsRemoteAddress(uint64_t remoteAddr,
                             uint64_t size) const {
    return Start.getAddressData() <= remoteAddr
      && remoteAddr + size <= Start.getAddressData() + Size;
  }
  
  template<typename U>
  RemoteRef<U> getRemoteRef(uint64_t remoteAddr) const {
    assert(containsRemoteAddress(remoteAddr, sizeof(U)));
    auto localAddr = (uint64_t)(uintptr_t)Start.getLocalBuffer()
      + (remoteAddr - Start.getAddressData());
    
    return RemoteRef<U>(remoteAddr, (const U*)localAddr);
  }
};

template<typename Self, typename Descriptor>
class ReflectionSectionIteratorBase
  : public std::iterator<std::forward_iterator_tag, Descriptor> {
  uint64_t OriginalSize;
protected:
  Self &asImpl() {
    return *static_cast<Self *>(this);
  }
public:
  RemoteRef<void> Cur;
  uint64_t Size;
  std::string Name;
    
  ReflectionSectionIteratorBase(RemoteRef<void> Cur, uint64_t Size, std::string Name)
    : OriginalSize(Size), Cur(Cur), Size(Size), Name(Name) {
    if (Size != 0) {
      auto NextRecord = this->operator*();
      auto NextSize = Self::getCurrentRecordSize(NextRecord);
      if (NextSize > Size) {
        std::cerr << "!!! Reflection section too small to contain first record\n" << std::endl;
        std::cerr << "Section Type: " << Name << std::endl;
        std::cerr << "Section size: "
                  << Size
                  << ", size of first record: "
                  << NextSize
                  << std::endl;
        abort();
      }
    }
  }

  RemoteRef<Descriptor> operator*() const {
    assert(Size > 0);
    return RemoteRef<Descriptor>(Cur.getAddressData(),
                                 (const Descriptor*)Cur.getLocalBuffer());
  }

  Self &operator++() {
    auto CurRecord = this->operator*();
    auto CurSize = Self::getCurrentRecordSize(CurRecord);
    Cur = Cur.atByteOffset(CurSize);
    Size -= CurSize;
    
    if (Size > 0) {
      auto NextRecord = this->operator*();
      auto NextSize = Self::getCurrentRecordSize(NextRecord);
      if (NextSize > Size) {
        int offset = (int)(OriginalSize - Size);
        std::cerr << "!!! Reflection section too small to contain next record\n" << std::endl;
        std::cerr << "Section Type: " << Name << std::endl;
        std::cerr << "Remaining section size: " << Size
                  << ", total section size: " << OriginalSize
                  << ", offset in section: " << offset
                  << ", size of next record: " << NextSize
                  << std::endl;
        const uint8_t *p = reinterpret_cast<const uint8_t *>(Cur.getLocalBuffer());
        std::cerr << "Last bytes of previous record: ";
        for (int i = std::max(-8, -offset); i < 0; i++) {
          std::cerr << std::hex << std::setw(2) << (int)p[i] << " ";
        }
        std::cerr << std::endl;
        std::cerr << "Next bytes in section: ";
        for (unsigned i = 0; i < Size && i < 16; i++) {
          std::cerr << std::hex << std::setw(2) << (int)p[i] << " ";
        }
        std::cerr << std::endl;
        abort();
      }
    }

    return asImpl();
  }

  bool operator==(const Self &other) const {
    return Cur == other.Cur && Size == other.Size;
  }

  bool operator!=(const Self &other) const {
    return !(*this == other);
  }
};

class FieldDescriptorIterator
  : public ReflectionSectionIteratorBase<FieldDescriptorIterator,
                                         FieldDescriptor>
{
public:
  FieldDescriptorIterator(RemoteRef<void> Cur, uint64_t Size)
    : ReflectionSectionIteratorBase(Cur, Size, "FieldDescriptor")
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<FieldDescriptor> FR) {
    return sizeof(FieldDescriptor) + FR->NumFields * FR->FieldRecordSize;
  }
};
using FieldSection = ReflectionSection<FieldDescriptorIterator>;

class AssociatedTypeIterator
  : public ReflectionSectionIteratorBase<AssociatedTypeIterator,
                                         AssociatedTypeDescriptor>
{
public:
  AssociatedTypeIterator(RemoteRef<void> Cur, uint64_t Size)
    : ReflectionSectionIteratorBase(Cur, Size, "AssociatedType")
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<AssociatedTypeDescriptor> ATR){
    return sizeof(AssociatedTypeDescriptor)
      + ATR->NumAssociatedTypes * ATR->AssociatedTypeRecordSize;
  }
};
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;

class BuiltinTypeDescriptorIterator
  : public ReflectionSectionIteratorBase<BuiltinTypeDescriptorIterator,
                                         BuiltinTypeDescriptor> {
public:
  BuiltinTypeDescriptorIterator(RemoteRef<void> Cur, uint64_t Size)
    : ReflectionSectionIteratorBase(Cur, Size, "BuiltinTypeDescriptor")
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<BuiltinTypeDescriptor> ATR){
    return sizeof(BuiltinTypeDescriptor);
  }
};
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;

class CaptureDescriptorIterator
  : public ReflectionSectionIteratorBase<CaptureDescriptorIterator,
                                         CaptureDescriptor> {
public:
  CaptureDescriptorIterator(RemoteRef<void> Cur, uint64_t Size)
    : ReflectionSectionIteratorBase(Cur, Size, "CaptureDescriptor")
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<CaptureDescriptor> CR){
    return sizeof(CaptureDescriptor)
      + CR->NumCaptureTypes * sizeof(CaptureTypeRecord)
      + CR->NumMetadataSources * sizeof(MetadataSourceRecord);
  }
};
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;

class MultiPayloadEnumDescriptorIterator
  : public ReflectionSectionIteratorBase<MultiPayloadEnumDescriptorIterator,
                                         MultiPayloadEnumDescriptor> {
public:
  MultiPayloadEnumDescriptorIterator(RemoteRef<void> Cur, uint64_t Size)
    : ReflectionSectionIteratorBase(Cur, Size, "MultiPayloadEnum")
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<MultiPayloadEnumDescriptor> MPER) {
    return MPER->getSizeInBytes();
  }
};
using MultiPayloadEnumSection = ReflectionSection<MultiPayloadEnumDescriptorIterator>;

using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  FieldSection Field;
  AssociatedTypeSection AssociatedType;
  BuiltinTypeSection Builtin;
  CaptureSection Capture;
  GenericSection TypeReference;
  GenericSection ReflectionString;
  GenericSection Conformance;
  MultiPayloadEnumSection MultiPayloadEnum;
};

struct ClosureContextInfo {
  std::vector<const TypeRef *> CaptureTypes;
  std::vector<std::pair<const TypeRef *, const MetadataSource *>> MetadataSources;
  unsigned NumBindings = 0;

  void dump() const;
  void dump(std::ostream &stream) const;
};

struct FieldTypeInfo {
  std::string Name;
  int Value;
  const TypeRef *TR;
  bool Indirect;

  FieldTypeInfo() : Name(""), Value(0), TR(nullptr), Indirect(false) {}
  FieldTypeInfo(const std::string &Name, int Value, const TypeRef *TR, bool Indirect)
    : Name(Name), Value(Value), TR(TR), Indirect(Indirect) {}

  static FieldTypeInfo forEmptyCase(std::string Name, int Value) {
    return FieldTypeInfo(Name, Value, nullptr, false);
  }

  static FieldTypeInfo forIndirectCase(std::string Name, int Value, const TypeRef *TR) {
    return FieldTypeInfo(Name, Value, TR, true);
  }

  static FieldTypeInfo forField(std::string Name, int Value, const TypeRef *TR) {
    return FieldTypeInfo(Name, Value, TR, false);
  }
};

/// Info about a protocol conformance read out from an Image
struct ProtocolConformanceInfo {
  std::string TypeName;
  std::string ProtocolName;
  std::string MangledTypeName;
};

struct ConformanceCollectionResult {
  std::vector<ProtocolConformanceInfo> Conformances;
  std::vector<std::string> Errors;
};

struct AssociatedType {
  std::string TypeAliasName;
  std::string SubstitutedTypeMangledName;
  std::string SubstitutedTypeFullyQualifiedName;
  std::string SubstitutedTypeDiagnosticPrintName;
};

/// Info about all of a given type's associated types, as read out from an Image
struct AssociatedTypeInfo {
  std::string MangledTypeName;
  std::string FullyQualifiedName;
  std::string ProtocolFullyQualifiedName;
  std::vector<AssociatedType> AssociatedTypes;
};

struct AssociatedTypeCollectionResult {
  std::vector<AssociatedTypeInfo> AssociatedTypeInfos;
  std::vector<std::string> Errors;
};

struct PropertyTypeInfo {
  std::string Label;
  std::string TypeMangledName;
  std::string TypeFullyQualifiedName;
  std::string TypeDiagnosticPrintName;
};

struct EnumCaseInfo {
  std::string Label;
};

/// Info about all of a given type's fields, as read out from an Image
struct FieldMetadata {
  std::string MangledTypeName;
  std::string FullyQualifiedName;
  std::vector<PropertyTypeInfo> Properties;
  std::vector<EnumCaseInfo> EnumCases;
};

struct FieldTypeCollectionResult {
  std::vector<FieldMetadata> FieldInfos;
  std::vector<std::string> Errors;
};

/// An implementation of MetadataReader's BuilderType concept for
/// building TypeRefs, and parsing field metadata from any images
/// it has been made aware of.
///
/// Note that the TypeRefBuilder owns the memory for all TypeRefs
/// it vends.
class TypeRefBuilder {
#define TYPEREF(Id, Parent) friend class Id##TypeRef;
#include "swift/Reflection/TypeRefs.def"

public:
  using BuiltType = const TypeRef *;
  using BuiltTypeDecl = llvm::Optional<std::string>;
  using BuiltProtocolDecl =
      llvm::Optional<std::pair<std::string, bool /*isObjC*/>>;

  TypeRefBuilder(const TypeRefBuilder &other) = delete;
  TypeRefBuilder &operator=(const TypeRefBuilder &other) = delete;

private:
  Demangle::Demangler Dem;

  /// Makes sure dynamically allocated TypeRefs stick around for the life of
  /// this TypeRefBuilder and are automatically released.
  std::vector<std::unique_ptr<const TypeRef>> TypeRefPool;

  /// Cache for associated type lookups.
  std::unordered_map<TypeRefID, const TypeRef *,
                     TypeRefID::Hash, TypeRefID::Equal> AssociatedTypeCache;

  /// Cache for field info lookups.
  std::unordered_map<std::string, RemoteRef<FieldDescriptor>> FieldTypeInfoCache;

  TypeConverter TC;
  MetadataSourceBuilder MSB;

#define TYPEREF(Id, Parent) \
  std::unordered_map<TypeRefID, const Id##TypeRef *, \
                     TypeRefID::Hash, TypeRefID::Equal> Id##TypeRefs;
#include "swift/Reflection/TypeRefs.def"

public:
  template <typename TypeRefTy, typename... Args>
  const TypeRefTy *makeTypeRef(Args... args) {
    const auto TR = new TypeRefTy(::std::forward<Args>(args)...);
    TypeRefPool.push_back(std::unique_ptr<const TypeRef>(TR));
    return TR;
  }

  Demangle::NodeFactory &getNodeFactory() { return Dem; }

  void clearNodeFactory() { Dem.clear(); }

  BuiltType decodeMangledType(Node *node, bool forRequirement = true);

  ///
  /// Factory methods for all TypeRef kinds
  ///

  const BuiltinTypeRef *createBuiltinType(const std::string &builtinName,
                                          const std::string &mangledName) {
    return BuiltinTypeRef::create(*this, mangledName);
  }

  llvm::Optional<std::string> createTypeDecl(Node *node, bool &typeAlias) {
    auto mangling = Demangle::mangleNode(node);
    if (!mangling.isSuccess()) {
      return llvm::None;
    }
    return mangling.result();
  }

  BuiltProtocolDecl
  createProtocolDecl(Node *node) {
    auto mangling = Demangle::mangleNode(node);
    if (!mangling.isSuccess()) {
      return llvm::None;
    }
    return std::make_pair(mangling.result(), false);
  }

  BuiltProtocolDecl
  createObjCProtocolDecl(std::string &&name) {
    return std::make_pair(name, true);
  }

  llvm::Optional<std::string> createTypeDecl(std::string &&mangledName,
                                             bool &typeAlias) {
    return std::move(mangledName);
  }

  const NominalTypeRef *
  createNominalType(const llvm::Optional<std::string> &mangledName) {
    return NominalTypeRef::create(*this, *mangledName, nullptr);
  }

  const NominalTypeRef *
  createNominalType(const llvm::Optional<std::string> &mangledName,
                    const TypeRef *parent) {
    return NominalTypeRef::create(*this, *mangledName, parent);
  }

  const TypeRef *
  createTypeAliasType(const llvm::Optional<std::string> &mangledName,
                      const TypeRef *parent) {
    // TypeRefs don't contain sugared types
    return nullptr;
  }

  const TypeRef *createOptionalType(const TypeRef *base) {
    // TypeRefs don't contain sugared types
    return nullptr;
  }

  const TypeRef *createArrayType(const TypeRef *base) {
    // TypeRefs don't contain sugared types
    return nullptr;
  }

  const TypeRef *createDictionaryType(const TypeRef *key, const TypeRef *value) {
    // TypeRefs don't contain sugared types
    return nullptr;
  }

  const TypeRef *createParenType(const TypeRef *base) {
    // TypeRefs don't contain sugared types
    return nullptr;
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const llvm::Optional<std::string> &mangledName,
                         const std::vector<const TypeRef *> &args) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, nullptr);
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const llvm::Optional<std::string> &mangledName,
                         llvm::ArrayRef<const TypeRef *> args,
                         const TypeRef *parent) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, parent);
  }

  const TypeRef *
  resolveOpaqueType(NodePointer opaqueDescriptor,
                    llvm::ArrayRef<llvm::ArrayRef<const TypeRef *>> genericArgs,
                    unsigned ordinal) {
    // TODO: Produce a type ref for the opaque type if the underlying type isn't
    // available.

    // Try to resolve to the underlying type, if we can.
    if (opaqueDescriptor->getKind() ==
                            Node::Kind::OpaqueTypeDescriptorSymbolicReference) {
      auto underlyingTy = OpaqueUnderlyingTypeReader(
                                         opaqueDescriptor->getIndex(), ordinal);
      
      if (!underlyingTy)
        return nullptr;
      
      GenericArgumentMap subs;
      for (unsigned d = 0, de = genericArgs.size(); d < de; ++d) {
        auto argsForDepth = genericArgs[d];
        for (unsigned i = 0, ie = argsForDepth.size(); i < ie; ++i) {
          subs.insert({{d, i}, argsForDepth[i]});
        }
      }
      
      return underlyingTy->subst(*this, subs);
    }

    auto mangling = mangleNode(opaqueDescriptor,
                               SymbolicResolver(),
                               Dem);
    if (!mangling.isSuccess())
      return nullptr;

    // Otherwise, build a type ref that represents the opaque type.
    return OpaqueArchetypeTypeRef::create(*this,
                                          mangling.result(),
                                          nodeToString(opaqueDescriptor),
                                          ordinal,
                                          genericArgs);
  }

  const TupleTypeRef *createTupleType(llvm::ArrayRef<const TypeRef *> elements,
                                      std::string &&labels) {
    return TupleTypeRef::create(*this, elements, std::move(labels));
  }

  const FunctionTypeRef *createFunctionType(
      llvm::ArrayRef<remote::FunctionParam<const TypeRef *>> params,
      const TypeRef *result, FunctionTypeFlags flags,
      FunctionMetadataDifferentiabilityKind diffKind,
      const TypeRef *globalActor) {
    return FunctionTypeRef::create(
        *this, params, result, flags, diffKind, globalActor);
  }

  const FunctionTypeRef *createImplFunctionType(
      Demangle::ImplParameterConvention calleeConvention,
      llvm::ArrayRef<Demangle::ImplFunctionParam<const TypeRef *>> params,
      llvm::ArrayRef<Demangle::ImplFunctionResult<const TypeRef *>> results,
      llvm::Optional<Demangle::ImplFunctionResult<const TypeRef *>> errorResult,
      ImplFunctionTypeFlags flags) {
    // Minimal support for lowered function types. These come up in
    // reflection as capture types. For the reflection library's
    // purposes, the only part that matters is the convention.
    FunctionTypeFlags funcFlags;
    switch (flags.getRepresentation()) {
    case Demangle::ImplFunctionRepresentation::Thick:
    case Demangle::ImplFunctionRepresentation::Closure:
      funcFlags = funcFlags.withConvention(FunctionMetadataConvention::Swift);
      break;
    case Demangle::ImplFunctionRepresentation::Thin:
    case Demangle::ImplFunctionRepresentation::Method:
    case Demangle::ImplFunctionRepresentation::ObjCMethod:
    case Demangle::ImplFunctionRepresentation::WitnessMethod:
      funcFlags = funcFlags.withConvention(FunctionMetadataConvention::Thin);
      break;
    case Demangle::ImplFunctionRepresentation::CFunctionPointer:
      funcFlags = funcFlags.withConvention(FunctionMetadataConvention::CFunctionPointer);
      break;
    case Demangle::ImplFunctionRepresentation::Block:
      funcFlags = funcFlags.withConvention(FunctionMetadataConvention::Block);
      break;
    }

    funcFlags = funcFlags.withConcurrent(flags.isSendable());
    funcFlags = funcFlags.withAsync(flags.isAsync());
    funcFlags = funcFlags.withDifferentiable(flags.isDifferentiable());

    FunctionMetadataDifferentiabilityKind diffKind;
    switch (flags.getDifferentiabilityKind()) {
    case ImplFunctionDifferentiabilityKind::NonDifferentiable:
      diffKind = FunctionMetadataDifferentiabilityKind::NonDifferentiable;
      break;
    case ImplFunctionDifferentiabilityKind::Forward:
      diffKind = FunctionMetadataDifferentiabilityKind::Forward;
      break;
    case ImplFunctionDifferentiabilityKind::Reverse:
      diffKind = FunctionMetadataDifferentiabilityKind::Reverse;
      break;
    case ImplFunctionDifferentiabilityKind::Normal:
      diffKind = FunctionMetadataDifferentiabilityKind::Normal;
      break;
    case ImplFunctionDifferentiabilityKind::Linear:
      diffKind = FunctionMetadataDifferentiabilityKind::Linear;
      break;
    }

    auto result = createTupleType({}, "");
    return FunctionTypeRef::create(
        *this, {}, result, funcFlags, diffKind, nullptr);
  }

  const ProtocolCompositionTypeRef *
  createProtocolCompositionType(llvm::ArrayRef<BuiltProtocolDecl> protocols,
                                BuiltType superclass, bool isClassBound,
                                bool forRequirement = true) {
    std::vector<const TypeRef *> protocolRefs;
    for (const auto &protocol : protocols) {
      if (!protocol)
        continue;

      if (protocol->second)
        protocolRefs.push_back(createObjCProtocolType(protocol->first));
      else
        protocolRefs.push_back(createNominalType(protocol->first));
    }

    return ProtocolCompositionTypeRef::create(*this, protocolRefs, superclass,
                                              isClassBound);
  }

  const ParameterizedProtocolTypeRef *
  createParameterizedProtocolType(const TypeRef *base,
                                  llvm::ArrayRef<const TypeRef *> args) {
    auto *baseProto = llvm::dyn_cast<ProtocolCompositionTypeRef>(base);
    if (!baseProto)
      return nullptr;
    return ParameterizedProtocolTypeRef::create(*this, baseProto, args);
  }

  const ExistentialMetatypeTypeRef *createExistentialMetatypeType(
      const TypeRef *instance,
      llvm::Optional<Demangle::ImplMetatypeRepresentation> repr = None) {
    return ExistentialMetatypeTypeRef::create(*this, instance);
  }

  const MetatypeTypeRef *createMetatypeType(
      const TypeRef *instance,
      llvm::Optional<Demangle::ImplMetatypeRepresentation> repr = None) {
    bool WasAbstract = (repr && *repr != ImplMetatypeRepresentation::Thin);
    return MetatypeTypeRef::create(*this, instance, WasAbstract);
  }

  const GenericTypeParameterTypeRef *
  createGenericTypeParameterType(unsigned depth, unsigned index) {
    return GenericTypeParameterTypeRef::create(*this, depth, index);
  }

  const DependentMemberTypeRef *
  createDependentMemberType(const std::string &member,
                            const TypeRef *base) {
    // Should not have unresolved dependent member types here.
    return nullptr;
  }

  const DependentMemberTypeRef *
  createDependentMemberType(const std::string &member,
                            const TypeRef *base,
                            BuiltProtocolDecl protocol) {
    // Objective-C protocols don't have dependent types.
    if (protocol->second)
      return nullptr;
    return DependentMemberTypeRef::create(*this, member, base,
                                          protocol->first);
  }

#define REF_STORAGE(Name, ...) \
  const Name##StorageTypeRef *create##Name##StorageType(const TypeRef *base) { \
    return Name##StorageTypeRef::create(*this, base); \
  }
#include "swift/AST/ReferenceStorage.def"

  const SILBoxTypeRef *createSILBoxType(const TypeRef *base) {
    return SILBoxTypeRef::create(*this, base);
  }

  using BuiltSILBoxField = typename SILBoxTypeWithLayoutTypeRef::Field;
  using BuiltSubstitution = std::pair<const TypeRef *, const TypeRef *>;
  using BuiltRequirement = TypeRefRequirement;
  using BuiltLayoutConstraint = TypeRefLayoutConstraint;
  BuiltLayoutConstraint getLayoutConstraint(LayoutConstraintKind kind) {
    // FIXME: Implement this.
    return {};
  }
  BuiltLayoutConstraint
  getLayoutConstraintWithSizeAlign(LayoutConstraintKind kind, unsigned size,
                                   unsigned alignment) {
    // FIXME: Implement this.
    return {};
  }

  const SILBoxTypeWithLayoutTypeRef *createSILBoxTypeWithLayout(
      const llvm::SmallVectorImpl<BuiltSILBoxField> &Fields,
      const llvm::SmallVectorImpl<BuiltSubstitution> &Substitutions,
      const llvm::SmallVectorImpl<BuiltRequirement> &Requirements) {
    return SILBoxTypeWithLayoutTypeRef::create(*this, Fields, Substitutions,
                                               Requirements);
  }

  bool isExistential(const TypeRef *) {
    // FIXME: Implement this.
    return true;
  }

  const TypeRef *createDynamicSelfType(const TypeRef *selfType) {
    // TypeRefs should not contain DynamicSelfType.
    return nullptr;
  }

  const ObjCClassTypeRef *getUnnamedObjCClassType() {
    return createObjCClassType("");
  }

  const ObjCClassTypeRef *
  createObjCClassType(const std::string &name) {
    return ObjCClassTypeRef::create(*this, name);
  }

  const ObjCClassTypeRef *
  createBoundGenericObjCClassType(const std::string &name,
                                  llvm::ArrayRef<const TypeRef *> args) {
    // Remote reflection just ignores generic arguments for Objective-C
    // lightweight generic types, since they don't affect layout.
    return createObjCClassType(name);
  }

  const ObjCProtocolTypeRef *
  createObjCProtocolType(const std::string &name) {
    return ObjCProtocolTypeRef::create(*this, name);
  }

  const ForeignClassTypeRef *
  createForeignClassType(const std::string &mangledName) {
    return ForeignClassTypeRef::create(*this, mangledName);
  }

  const ForeignClassTypeRef *
  getUnnamedForeignClassType() {
    return createForeignClassType("");
  }

  const OpaqueTypeRef *getOpaqueType() {
    return OpaqueTypeRef::get();
  }

  ///
  /// Parsing reflection metadata
  ///

  void addReflectionInfo(ReflectionInfo I) {
    ReflectionInfos.push_back(I);
  }
  
  const std::vector<ReflectionInfo> &getReflectionInfos() {
    return ReflectionInfos;
  }

public:
  enum ForTesting_t { ForTesting };
  
  // Only for testing. A TypeRefBuilder built this way will not be able to
  // decode records in remote memory.
  explicit TypeRefBuilder(ForTesting_t) : TC(*this) {}

private:
  std::vector<ReflectionInfo> ReflectionInfos;

  /// Index of the next Reflection Info that should be processed.
  /// This assumes that Reflection Infos are never removed from the vector.
  size_t FirstUnprocessedReflectionInfoIndex = 0;
    
  llvm::Optional<std::string> normalizeReflectionName(RemoteRef<char> name);
  bool reflectionNameMatches(RemoteRef<char> reflectionName,
                             StringRef searchName);

public:
  RemoteRef<char> readTypeRef(uint64_t remoteAddr);
  
  template<typename Record, typename Field>
  RemoteRef<char> readTypeRef(RemoteRef<Record> record,
                              const Field &field) {
    uint64_t remoteAddr = record.resolveRelativeFieldData(field);
    
    return readTypeRef(remoteAddr);
  }

  StringRef getTypeRefString(RemoteRef<char> record) {
    return Demangle::makeSymbolicMangledNameStringRef(record.getLocalBuffer());
  }
  
private:
  using RefDemangler = std::function<Demangle::Node * (RemoteRef<char>, bool)>;
  using UnderlyingTypeReader = std::function<const TypeRef* (uint64_t, unsigned)>;
  using ByteReader = std::function<remote::MemoryReader::ReadBytesResult (remote::RemoteAddress, unsigned)>;
  using StringReader = std::function<bool (remote::RemoteAddress, std::string &)>;
  using PointerReader = std::function<llvm::Optional<remote::RemoteAbsolutePointer> (remote::RemoteAddress, unsigned)>;
  using IntVariableReader = std::function<llvm::Optional<uint64_t> (std::string, unsigned)>;

  // These fields are captured from the MetadataReader template passed into the
  // TypeRefBuilder struct, to isolate its template-ness from the rest of
  // TypeRefBuilder.
  unsigned PointerSize;
  RefDemangler TypeRefDemangler;
  UnderlyingTypeReader OpaqueUnderlyingTypeReader;

  // Opaque fields captured from the MetadataReader's MemoryReader
  ByteReader OpaqueByteReader;
  StringReader OpaqueStringReader;
  PointerReader OpaquePointerReader;
  IntVariableReader OpaqueIntVariableReader;

public:
  template<typename Runtime>
  TypeRefBuilder(remote::MetadataReader<Runtime, TypeRefBuilder> &reader)
    : TC(*this),
      PointerSize(sizeof(typename Runtime::StoredPointer)),
      TypeRefDemangler(
      [this, &reader](RemoteRef<char> string, bool useOpaqueTypeSymbolicReferences) -> Demangle::Node * {
        return reader.demangle(string,
                               remote::MangledNameKind::Type,
                               Dem, useOpaqueTypeSymbolicReferences);
      }),
      OpaqueUnderlyingTypeReader(
      [&reader](uint64_t descriptorAddr, unsigned ordinal) -> const TypeRef* {
        return reader.readUnderlyingTypeForOpaqueTypeDescriptor(
          descriptorAddr, ordinal).getType();
      }),
      OpaqueByteReader([&reader](remote::RemoteAddress address, unsigned size) -> remote::MemoryReader::ReadBytesResult {
        return reader.Reader->readBytes(address, size);
      }),
      OpaqueStringReader([&reader](remote::RemoteAddress address, std::string &dest) -> bool {
        return reader.Reader->readString(address, dest);
      }),
      OpaquePointerReader([&reader](remote::RemoteAddress address, unsigned size) -> llvm::Optional<remote::RemoteAbsolutePointer> {
        return reader.Reader->readPointer(address, size);
      }),
      OpaqueIntVariableReader(
        [&reader](std::string symbol, unsigned size) -> llvm::Optional<uint64_t> {
          llvm::Optional<uint64_t> result;
          if (auto Reader = reader.Reader) {
            auto Addr = Reader->getSymbolAddress(symbol);
            if (Addr) {
              switch (size) {
              case 8: {
                uint64_t i;
                if (Reader->readInteger(Addr, &i)) {
                  result = i;
                }
                break;
              }
              case 4: {
                uint32_t i;
                if (Reader->readInteger(Addr, &i)) {
                  result = i;
                }
                break;
              }
              default: {
                assert(false && "Can only read 4- or 8-byte integer variables from image");
              }
              }
            }
          }
          return result;
      })
  { }

  Demangle::Node *demangleTypeRef(RemoteRef<char> string,
                                  bool useOpaqueTypeSymbolicReferences = true) {
    return TypeRefDemangler(string, useOpaqueTypeSymbolicReferences);
  }

  TypeConverter &getTypeConverter() { return TC; }

  const TypeRef *
  lookupTypeWitness(const std::string &MangledTypeName,
                    const std::string &Member,
                    StringRef Protocol);

  const TypeRef *lookupSuperclass(const TypeRef *TR);

  /// Load unsubstituted field types for a nominal type.
  RemoteRef<FieldDescriptor> getFieldTypeInfo(const TypeRef *TR);

  /// Get the parsed and substituted field types for a nominal type.
  bool getFieldTypeRefs(const TypeRef *TR, RemoteRef<FieldDescriptor> FD,
                        remote::TypeInfoProvider *ExternalTypeInfo,
                        std::vector<FieldTypeInfo> &Fields);

  /// Get the primitive type lowering for a builtin type.
  RemoteRef<BuiltinTypeDescriptor> getBuiltinTypeInfo(const TypeRef *TR);

  /// Get the raw capture descriptor for a remote capture descriptor
  /// address.
  RemoteRef<CaptureDescriptor> getCaptureDescriptor(uint64_t RemoteAddress);

  /// Get the unsubstituted capture types for a closure context.
  ClosureContextInfo getClosureContextInfo(RemoteRef<CaptureDescriptor> CD);

  /// Get the multipayload enum projection information for a given TR
  RemoteRef<MultiPayloadEnumDescriptor> getMultiPayloadEnumInfo(const TypeRef *TR);

private:
  llvm::Optional<uint64_t> multiPayloadEnumPointerMask;

public:
  /// Retrieve the MPE pointer mask from the target
  // If it can't read it, it will make an educated guess
  // Note: This is a pointer-sized value stored in a uint64_t
  // If the target is 32 bits, the mask is in the lower 32 bits
  uint64_t getMultiPayloadEnumPointerMask() {
    unsigned pointerSize = TC.targetPointerSize();
    if (!multiPayloadEnumPointerMask.hasValue()) {
      // Ask the target for the spare bits mask
      multiPayloadEnumPointerMask
        = OpaqueIntVariableReader("_swift_debug_multiPayloadEnumPointerSpareBitsMask", pointerSize);
    }
    if (!multiPayloadEnumPointerMask.hasValue()) {
      if (pointerSize == sizeof(void *)) {
        // Most reflection tools run on the target machine,
        // in which case, they use the same configuration:
        multiPayloadEnumPointerMask = _swift_abi_SwiftSpareBitsMask;
      } else if (pointerSize == 4) {
        // All 32-bit platforms are the same, so this is always correct.
        multiPayloadEnumPointerMask = SWIFT_ABI_ARM_SWIFT_SPARE_BITS_MASK;
      } else {
        // This is not always correct.  But we can't do any better?
        multiPayloadEnumPointerMask = SWIFT_ABI_ARM64_SWIFT_SPARE_BITS_MASK;
      }
    }
    return multiPayloadEnumPointerMask.getValue();
  }

  ///
  /// Dumping typerefs, field declarations, associated types
  ///

  void dumpTypeRef(RemoteRef<char> MangledName, std::ostream &stream,
                   bool printTypeName = false);
  FieldTypeCollectionResult collectFieldTypes(llvm::Optional<std::string> forMangledTypeName);
  void dumpFieldSection(std::ostream &stream);
  AssociatedTypeCollectionResult collectAssociatedTypes(llvm::Optional<std::string> forMangledTypeName);
  void dumpAssociatedTypeSection(std::ostream &stream);
  void dumpBuiltinTypeSection(std::ostream &stream);
  void dumpCaptureSection(std::ostream &stream);
  void dumpMultiPayloadEnumSection(std::ostream &stream);

  ///
  /// Extraction of protocol conformances
  ///

private:
  /// Reader of protocol descriptors from Images
  template <template <typename Runtime> class ObjCInteropKind,
            unsigned PointerSize>
  struct ProtocolConformanceDescriptorReader {
    std::string Error;
    ByteReader OpaqueByteReader;
    StringReader OpaqueStringReader;
    PointerReader OpaquePointerReader;

    ProtocolConformanceDescriptorReader(ByteReader byteReader,
                                        StringReader stringReader,
                                        PointerReader pointerReader)
        : Error(""), OpaqueByteReader(byteReader),
          OpaqueStringReader(stringReader), OpaquePointerReader(pointerReader) {
    }

    llvm::Optional<std::string>
    getParentContextName(uintptr_t contextDescriptorAddress) {
      llvm::Optional<std::string> optionalParentContextName;
      auto contextTypeDescriptorBytes = OpaqueByteReader(
          remote::RemoteAddress(contextDescriptorAddress),
          sizeof(ExternalContextDescriptor<ObjCInteropKind, PointerSize>));
      if (!contextTypeDescriptorBytes.get()) {
        Error = "Failed to read context descriptor.";
        return llvm::None;
      }
      const ExternalContextDescriptor<ObjCInteropKind, PointerSize>
          *contextDescriptor =
              (const ExternalContextDescriptor<ObjCInteropKind, PointerSize> *)
                  contextTypeDescriptorBytes.get();

      auto parentOffsetAddress = detail::applyRelativeOffset(
          (const char *)contextDescriptorAddress,
          (int32_t)contextDescriptor->getParentOffset());
      auto parentOfsetBytes = OpaqueByteReader(
          remote::RemoteAddress(parentOffsetAddress), sizeof(uint32_t));
      if (!parentOfsetBytes.get()) {
        Error = "Failed to parent offset in a type descriptor.";
        return llvm::None;
      }
      auto parentFieldOffset = (const int32_t *)parentOfsetBytes.get();
      auto parentTargetAddress = detail::applyRelativeOffset(
          (const char *)parentOffsetAddress, *parentFieldOffset);

      //
      auto readContextParentName =
          [&](uintptr_t descriptorAddress) -> llvm::Optional<std::string> {
        llvm::Optional<std::string> optionalParentName;
        auto parentContextDescriptorBytes = OpaqueByteReader(
            remote::RemoteAddress(descriptorAddress),
            sizeof(ExternalContextDescriptor<ObjCInteropKind, PointerSize>));
        if (!parentContextDescriptorBytes.get()) {
          Error = "Failed to read context descriptor.";
          return llvm::None;
        }
        const ExternalContextDescriptor<ObjCInteropKind,
                                        PointerSize> *parentContextDescriptor =
            (const ExternalContextDescriptor<ObjCInteropKind, PointerSize> *)
                parentContextDescriptorBytes.get();

        if (auto moduleDescriptor = dyn_cast<
                ExternalModuleContextDescriptor<ObjCInteropKind, PointerSize>>(
                parentContextDescriptor)) {
          auto moduleDescriptorName = readModuleNameFromModuleDescriptor(
              moduleDescriptor, parentTargetAddress);
          if (!moduleDescriptorName.hasValue())
            return llvm::None;
          else
            optionalParentName = moduleDescriptorName;
        } else if (auto typeDescriptor =
                       dyn_cast<ExternalTypeContextDescriptor<ObjCInteropKind,
                                                              PointerSize>>(
                           parentContextDescriptor)) {
          auto typeDescriptorName = readTypeNameFromTypeDescriptor(
              typeDescriptor, parentTargetAddress);
          if (!typeDescriptorName.hasValue())
            return llvm::None;
          else
            optionalParentName = typeDescriptorName;
          // Recurse to get this type's parent.
          auto optionalParentParentName =
              getParentContextName(descriptorAddress);
          if (optionalParentParentName.hasValue()) {
            optionalParentName = optionalParentParentName.getValue() + "." +
                                 optionalParentName.getValue();
          }
        } else {
          Error = "Unexpected type of parent context descriptor.";
          return llvm::None;
        }

        return optionalParentName;
      };

      // Set low bit indicates that this is an indirect
      // reference
      if (parentTargetAddress & 0x1) {
        auto adjustedParentTargetAddress = parentTargetAddress & ~0x1;
        if (auto symbol = OpaquePointerReader(
                remote::RemoteAddress(adjustedParentTargetAddress),
                PointerSize)) {
          if (!symbol->getSymbol().empty()) {
            Demangle::Context Ctx;
            auto demangledRoot =
                Ctx.demangleSymbolAsNode(symbol->getSymbol().str());
            assert(demangledRoot->getKind() == Node::Kind::Global);
            optionalParentContextName =
                nodeToString(demangledRoot->getChild(0)->getChild(0));
          } else {
            optionalParentContextName =
                readContextParentName(adjustedParentTargetAddress);
          }
        } else {
          Error = "Error reading external symbol address.";
          return llvm::None;
        }
      } else {
        optionalParentContextName = readContextParentName(parentTargetAddress);
      }
      return optionalParentContextName;
    }

    llvm::Optional<std::string> readTypeNameFromTypeDescriptor(
        const ExternalTypeContextDescriptor<ObjCInteropKind, PointerSize>
            *typeDescriptor,
        uintptr_t typeDescriptorAddress) {
      auto typeNameOffsetAddress =
          detail::applyRelativeOffset((const char *)typeDescriptorAddress,
                                      (int32_t)typeDescriptor->getNameOffset());
      auto typeNameOfsetBytes = OpaqueByteReader(
          remote::RemoteAddress(typeNameOffsetAddress), sizeof(uint32_t));
      if (!typeNameOfsetBytes.get()) {
        Error = "Failed to read type name offset in a type descriptor.";
        return llvm::None;
      }
      auto typeNameOffset = (const uint32_t *)typeNameOfsetBytes.get();
      auto typeNameAddress = detail::applyRelativeOffset(
          (const char *)typeNameOffsetAddress, (int32_t)*typeNameOffset);
      std::string typeName;
      OpaqueStringReader(remote::RemoteAddress(typeNameAddress), typeName);
      return typeName;
    }

    llvm::Optional<std::string> readModuleNameFromModuleDescriptor(
        const ExternalModuleContextDescriptor<ObjCInteropKind, PointerSize>
            *moduleDescriptor,
        uintptr_t moduleDescriptorAddress) {
      auto parentNameOffsetAddress = detail::applyRelativeOffset(
          (const char *)moduleDescriptorAddress,
          (int32_t)moduleDescriptor->getNameOffset());
      auto parentNameOfsetBytes = OpaqueByteReader(
          remote::RemoteAddress(parentNameOffsetAddress), sizeof(uint32_t));
      if (!parentNameOfsetBytes.get()) {
        Error = "Failed to read parent name offset in a module descriptor.";
        return llvm::None;
      }
      auto parentNameOfset = (const uint32_t *)parentNameOfsetBytes.get();
      auto parentNameAddress = detail::applyRelativeOffset(
          (const char *)parentNameOffsetAddress, (int32_t)*parentNameOfset);
      std::string parentName;
      OpaqueStringReader(remote::RemoteAddress(parentNameAddress), parentName);
      return parentName;
    }

    llvm::Optional<std::string> readProtocolNameFromProtocolDescriptor(
        uintptr_t protocolDescriptorAddress) {
      std::string protocolName;
      auto protocolDescriptorBytes = OpaqueByteReader(
          remote::RemoteAddress(protocolDescriptorAddress),
          sizeof(ExternalProtocolDescriptor<ObjCInteropKind, PointerSize>));
      if (!protocolDescriptorBytes.get()) {
        Error = "Error reading protocol descriptor.";
        return llvm::None;
      }
      const ExternalProtocolDescriptor<ObjCInteropKind, PointerSize>
          *protocolDescriptor =
              (const ExternalProtocolDescriptor<ObjCInteropKind, PointerSize> *)
                  protocolDescriptorBytes.get();

      // Compute the address of the protocol descriptor's name field and read
      // the offset
      auto protocolNameOffsetAddress = detail::applyRelativeOffset(
          (const char *)protocolDescriptorAddress,
          (int32_t)protocolDescriptor->getNameOffset());
      auto protocolNameOfsetBytes = OpaqueByteReader(
          remote::RemoteAddress(protocolNameOffsetAddress), sizeof(uint32_t));
      if (!protocolNameOfsetBytes.get()) {
        Error = "Failed to read type name offset in a protocol descriptor.";
        return llvm::None;
      }
      auto protocolNameOffset = (const uint32_t *)protocolNameOfsetBytes.get();

      // Using the offset above, compute the address of the name field itsel
      // and read it.
      auto protocolNameAddress =
          detail::applyRelativeOffset((const char *)protocolNameOffsetAddress,
                                      (int32_t)*protocolNameOffset);
      OpaqueStringReader(remote::RemoteAddress(protocolNameAddress),
                         protocolName);
      return protocolName;
    }

    /// Extract conforming type's name from a Conformance Descriptor
    /// Returns a pair of (mangledTypeName, fullyQualifiedTypeName)
    llvm::Optional<std::pair<std::string, std::string>> getConformingTypeName(
        const uintptr_t conformanceDescriptorAddress,
        const ExternalProtocolConformanceDescriptor<
            ObjCInteropKind, PointerSize> &conformanceDescriptor) {
      std::string typeName;
      std::string mangledTypeName = "";

      // If this is a conformance added to an ObjC class, detect that here and return class name
      if (conformanceDescriptor.getTypeKind() == TypeReferenceKind::DirectObjCClassName) {
        auto className = conformanceDescriptor.getDirectObjCClassName();
        typeName = MANGLING_MODULE_OBJC.str() + std::string(".") + className;
        return std::make_pair(mangledTypeName, typeName);
      }

      // Compute the address of the type descriptor as follows:
      //    - Compute the address of the TypeRef field in the protocol
      //    descriptor
      //    - Read the TypeRef field to compute the offset to the type
      //    descriptor
      //    - Address of the type descriptor is found at the (2) offset from the
      //      conformance descriptor address
      auto contextDescriptorFieldAddress = detail::applyRelativeOffset(
          (const char *)conformanceDescriptorAddress,
          (int32_t)conformanceDescriptor.getTypeRefDescriptorOffset());
      auto contextDescriptorOffsetBytes =
          OpaqueByteReader(remote::RemoteAddress(contextDescriptorFieldAddress),
                           sizeof(uint32_t));
      if (!contextDescriptorOffsetBytes.get()) {
        Error =
            "Failed to read type descriptor field in conformance descriptor.";
        return llvm::None;
      }
      auto contextDescriptorOffset =
          (const uint32_t *)contextDescriptorOffsetBytes.get();

      // Read the type descriptor itself using the address computed above
      auto contextTypeDescriptorAddress = detail::applyRelativeOffset(
          (const char *)contextDescriptorFieldAddress,
          (int32_t)*contextDescriptorOffset);

      // Instead of a type descriptor this may just be a symbol reference, check that first
      if (auto symbol = OpaquePointerReader(remote::RemoteAddress(contextTypeDescriptorAddress),
                                            PointerSize)) {
        if (!symbol->getSymbol().empty()) {
          mangledTypeName = symbol->getSymbol().str();
          Demangle::Context Ctx;
          auto demangledRoot =
              Ctx.demangleSymbolAsNode(mangledTypeName);
          assert(demangledRoot->getKind() == Node::Kind::Global);
          typeName =
              nodeToString(demangledRoot->getChild(0)->getChild(0));
          return std::make_pair(mangledTypeName, typeName);
        }
      }

      auto contextTypeDescriptorBytes = OpaqueByteReader(
          remote::RemoteAddress(contextTypeDescriptorAddress),
          sizeof(ExternalContextDescriptor<ObjCInteropKind, PointerSize>));
      if (!contextTypeDescriptorBytes.get()) {
        Error = "Failed to read context descriptor.";
        return llvm::None;
      }
      const ExternalContextDescriptor<ObjCInteropKind, PointerSize>
          *contextDescriptor =
              (const ExternalContextDescriptor<ObjCInteropKind, PointerSize> *)
                  contextTypeDescriptorBytes.get();

      auto typeDescriptor =
          dyn_cast<ExternalTypeContextDescriptor<ObjCInteropKind, PointerSize>>(
              contextDescriptor);
      if (!typeDescriptor) {
        Error = "Unexpected type of context descriptor.";
        return llvm::None;
      }

      auto optionalTypeName = readTypeNameFromTypeDescriptor(
          typeDescriptor, contextTypeDescriptorAddress);
      if (!optionalTypeName.hasValue())
        return llvm::None;
      else
        typeName = optionalTypeName.getValue();

      // Prepend the parent context name
      auto optionalParentName =
          getParentContextName(contextTypeDescriptorAddress);
      if (optionalParentName.hasValue()) {
        typeName = optionalParentName.getValue() + "." + typeName;
      }

      return std::make_pair(mangledTypeName, typeName);
    }

    /// Extract protocol name from a Conformance Descriptor
    llvm::Optional<std::string> getConformanceProtocolName(
        const uintptr_t conformanceDescriptorAddress,
        const ExternalProtocolConformanceDescriptor<
            ObjCInteropKind, PointerSize> &conformanceDescriptor) {
      llvm::Optional<std::string> protocolName;
      auto protocolDescriptorFieldAddress = detail::applyRelativeOffset(
          (const char *)conformanceDescriptorAddress,
          (int32_t)conformanceDescriptor.getProtocolDescriptorOffset());

      auto protocolDescriptorOffsetBytes = OpaqueByteReader(
          remote::RemoteAddress(protocolDescriptorFieldAddress),
          sizeof(uint32_t));
      if (!protocolDescriptorOffsetBytes.get()) {
        Error = "Error reading protocol descriptor field in conformance "
                "descriptor.";
        return llvm::None;
      }
      auto protocolDescriptorOffset =
          (const uint32_t *)protocolDescriptorOffsetBytes.get();

      auto protocolDescriptorTarget = detail::applyRelativeOffset(
          (const char *)protocolDescriptorFieldAddress,
          (int32_t)*protocolDescriptorOffset);

      // Set low bit indicates that this is an indirect
      // reference
      if (protocolDescriptorTarget & 0x1) {
        auto adjustedProtocolDescriptorTarget = protocolDescriptorTarget & ~0x1;
        if (auto symbol = OpaquePointerReader(
                remote::RemoteAddress(adjustedProtocolDescriptorTarget),
                PointerSize)) {
          if (!symbol->getSymbol().empty()) {
            Demangle::Context Ctx;
            auto demangledRoot =
                Ctx.demangleSymbolAsNode(symbol->getSymbol().str());
            assert(demangledRoot->getKind() == Node::Kind::Global);
            assert(demangledRoot->getChild(0)->getKind() ==
                   Node::Kind::ProtocolDescriptor);
            protocolName =
                nodeToString(demangledRoot->getChild(0)->getChild(0));
          } else {
            // This is an absolute address of a protocol descriptor
            auto protocolDescriptorAddress = symbol->getOffset();
            protocolName = readProtocolNameFromProtocolDescriptor(
                protocolDescriptorAddress);
            // Prepend the parent context name
            auto optionalParentName =
                getParentContextName(protocolDescriptorAddress);
            if (optionalParentName.hasValue()) {
              protocolName =
                  optionalParentName.getValue() + "." + *protocolName;
            }
          }
        } else {
          Error = "Error reading external protocol address.";
          return llvm::None;
        }
      } else {
        // If this is a direct reference, get symbol name from the protocol
        // descriptor.
        protocolName =
            readProtocolNameFromProtocolDescriptor(protocolDescriptorTarget);
        // Prepend the parent context name
        auto optionalParentName =
            getParentContextName(protocolDescriptorTarget);
        if (optionalParentName.hasValue()) {
          protocolName = optionalParentName.getValue() + "." + *protocolName;
        }
      }

      return protocolName;
    }

    /// Given the address of a conformance descriptor, attempt to read it.
    llvm::Optional<ProtocolConformanceInfo>
    readConformanceDescriptor(RemoteRef<void> conformanceRecordRef,
                              const std::unordered_map<std::string, std::string>
                                  &typeNameToManglingMap) {
      const ExternalProtocolConformanceRecord<ObjCInteropKind,
                                              PointerSize> *CD =
          (const ExternalProtocolConformanceRecord<ObjCInteropKind, PointerSize>
               *)conformanceRecordRef.getLocalBuffer();
      // Read the Protocol Conformance Descriptor by getting its address from
      // the conformance record.
      auto conformanceDescriptorAddress = (uintptr_t)CD->getRelative(
          (void *)conformanceRecordRef.getAddressData());

      auto descriptorBytes = OpaqueByteReader(
          remote::RemoteAddress(conformanceDescriptorAddress),
          sizeof(ExternalProtocolConformanceDescriptor<ObjCInteropKind,
                                                       PointerSize>));
      if (!descriptorBytes.get()) {
        Error = "Failed to read protocol conformance descriptor.";
        return llvm::None;
      }
      const ExternalProtocolConformanceDescriptor<ObjCInteropKind, PointerSize>
          *conformanceDescriptorPtr =
              (const ExternalProtocolConformanceDescriptor<
                  ObjCInteropKind, PointerSize> *)descriptorBytes.get();

      auto optionalConformingTypeNamePair = getConformingTypeName(
          conformanceDescriptorAddress, *conformanceDescriptorPtr);
      if (!optionalConformingTypeNamePair.hasValue())
        return llvm::None;

      auto optionalConformanceProtocol = getConformanceProtocolName(
          conformanceDescriptorAddress, *conformanceDescriptorPtr);
      if (!optionalConformanceProtocol.hasValue())
        return llvm::None;

      std::string mangledTypeName;
      if (optionalConformingTypeNamePair.getValue().first.empty()) {
        auto it = typeNameToManglingMap.find(optionalConformingTypeNamePair.getValue().second);
        if (it != typeNameToManglingMap.end()) {
          mangledTypeName = it->second;
        } else {
          mangledTypeName = "";
        }
      } else {
        mangledTypeName = optionalConformingTypeNamePair.getValue().first;
      }

      return ProtocolConformanceInfo{optionalConformingTypeNamePair.getValue().second,
                                     optionalConformanceProtocol.getValue(),
                                     mangledTypeName};
    }
  };

public:
  template <template <typename Runtime> class ObjCInteropKind,
            unsigned PointerSize>
  ConformanceCollectionResult collectAllConformances() {
    ConformanceCollectionResult result;

    // The Fields section has gathered info on types that includes their mangled
    // names. Use that to build a dictionary from a type's demangled name to its
    // mangled name
    std::unordered_map<std::string, std::string> typeNameToManglingMap;
    for (const auto &section : ReflectionInfos) {
      for (auto descriptor : section.Field) {
        auto TypeRef = readTypeRef(descriptor, descriptor->MangledTypeName);
        auto OptionalMangledTypeName = normalizeReflectionName(TypeRef);
        auto TypeName = nodeToString(demangleTypeRef(TypeRef));
        clearNodeFactory();
        if (OptionalMangledTypeName.hasValue()) {
          typeNameToManglingMap[TypeName] = OptionalMangledTypeName.getValue();
        }
      }
    }

    // Collect all conformances and aggregate them per-conforming-type.
    std::unordered_map<std::string, std::vector<std::string>> typeConformances;
    ProtocolConformanceDescriptorReader<ObjCInteropKind, PointerSize>
        conformanceReader(OpaqueByteReader, OpaqueStringReader,
                          OpaquePointerReader);
    for (const auto &section : ReflectionInfos) {
      auto ConformanceBegin = section.Conformance.startAddress();
      auto ConformanceEnd = section.Conformance.endAddress();
      for (auto conformanceAddr = ConformanceBegin;
           conformanceAddr != ConformanceEnd;
           conformanceAddr = conformanceAddr.atByteOffset(4)) {
        auto optionalConformanceInfo =
            conformanceReader.readConformanceDescriptor(conformanceAddr,
                                                        typeNameToManglingMap);
        if (!optionalConformanceInfo.hasValue())
          result.Errors.push_back(conformanceReader.Error);
        else
          result.Conformances.push_back(optionalConformanceInfo.getValue());
      }
    }
    return result;
  }

  template <template <typename Runtime> class ObjCInteropKind,
            unsigned PointerSize>
  void dumpConformanceSection(std::ostream &stream) {
    auto conformanceCollectionResult = collectAllConformances<ObjCInteropKind, PointerSize>();

    // Collect all conformances and aggregate them per-conforming-type.
    std::unordered_map<std::string, std::vector<std::string>> typeConformances;
    for (auto &conformanceInfo : conformanceCollectionResult.Conformances) {
      auto typeConformancesKey = conformanceInfo.MangledTypeName + " (" +
                                 conformanceInfo.TypeName + ")";
      if (typeConformances.count(typeConformancesKey) != 0) {
        typeConformances[typeConformancesKey].push_back(
            conformanceInfo.ProtocolName);
      } else {
        typeConformances.emplace(
            typeConformancesKey,
            std::vector<std::string>{conformanceInfo.ProtocolName});
      }
    }
    for (auto &pair : typeConformances) {
      stream << pair.first << " : ";
      bool first = true;
      for (auto &protocol : pair.second) {
        if (!first) {
          stream << ", ";
        }
        first = false;
        stream << protocol;
      }
      stream << "\n";
    }

    // Report encountered errors
    for (auto &error : conformanceCollectionResult.Errors) {
      stream << "Error reading conformance descriptor: "
             << error << "\n";
    }
  }

  template <template <typename Runtime> class ObjCInteropKind,
            unsigned PointerSize>
  void dumpAllSections(std::ostream &stream) {
    stream << "FIELDS:\n";
    stream << "=======\n";
    dumpFieldSection(stream);
    stream << "\n";
    stream << "ASSOCIATED TYPES:\n";
    stream << "=================\n";
    dumpAssociatedTypeSection(stream);
    stream << "\n";
    stream << "BUILTIN TYPES:\n";
    stream << "==============\n";
    dumpBuiltinTypeSection(stream);
    stream << "\n";
    stream << "CAPTURE DESCRIPTORS:\n";
    stream << "====================\n";
    dumpCaptureSection(stream);
    stream << "\n";
    stream << "CONFORMANCES:\n";
    stream << "=============\n";
    dumpConformanceSection<ObjCInteropKind, PointerSize>(stream);
    stream << "\n";
    stream << "MULTI-PAYLOAD ENUM DESCRIPTORS:\n";
    stream << "===============================\n";
    dumpMultiPayloadEnumSection(stream);
    stream << "\n";
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREFBUILDER_H
