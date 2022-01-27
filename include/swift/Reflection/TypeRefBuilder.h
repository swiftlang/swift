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
protected:
  Self &asImpl() {
    return *static_cast<Self *>(this);
  }
public:
  RemoteRef<void> Cur;
  uint64_t Size;
    
  ReflectionSectionIteratorBase(RemoteRef<void> Cur, uint64_t Size)
    : Cur(Cur), Size(Size) {
    if (Size != 0 && Self::getCurrentRecordSize(this->operator*()) > Size) {
      fputs("reflection section too small!\n", stderr);
      abort();
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
        fputs("reflection section too small!\n", stderr);
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
    : ReflectionSectionIteratorBase(Cur, Size)
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
    : ReflectionSectionIteratorBase(Cur, Size)
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
    : ReflectionSectionIteratorBase(Cur, Size)
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
    : ReflectionSectionIteratorBase(Cur, Size)
  {}

  static uint64_t getCurrentRecordSize(RemoteRef<CaptureDescriptor> CR){
    return sizeof(CaptureDescriptor)
      + CR->NumCaptureTypes * sizeof(CaptureTypeRecord)
      + CR->NumMetadataSources * sizeof(MetadataSourceRecord);
  }
};
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  FieldSection Field;
  AssociatedTypeSection AssociatedType;
  BuiltinTypeSection Builtin;
  CaptureSection Capture;
  GenericSection TypeReference;
  GenericSection ReflectionString;
  GenericSection Conformance;
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
  std::string typeName;
  std::string protocolName;

  // TODO:
  // const TypeRef *type;
  // std::string mangledTypeName;
  // std::string mangledProtocolName;
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
      })
  {}

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

  ///
  /// Dumping typerefs, field declarations, associated types
  ///

  void dumpTypeRef(RemoteRef<char> MangledName, std::ostream &stream,
                   bool printTypeName = false);
  void dumpFieldSection(std::ostream &stream);
  void dumpAssociatedTypeSection(std::ostream &stream);
  void dumpBuiltinTypeSection(std::ostream &stream);
  void dumpCaptureSection(std::ostream &stream);

  ///
  /// Extraction of protocol conformances
  ///

private:
  /// Reader of protocol descriptors from Images
  template <unsigned PointerSize>
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

    std::string getConformingTypeName(
        const char *conformanceDescriptorAddress,
        const ExternalProtocolConformanceDescriptor<PointerSize>
            &conformanceDescriptor) {
      std::string typeName;
      // Compute the address of the type descriptor as follows:
      //    - Compute the address of the TypeRef field in the protocol
      //    descriptor
      //    - Read the TypeRef field to compute the offset to the type
      //    descriptor
      //    - Address of the type descriptor is found at the (2) offset from the
      //      conformance descriptor address
      auto contextDescriptorFieldAddress = detail::applyRelativeOffset(
          conformanceDescriptorAddress,
          (int32_t)conformanceDescriptor.getTypeDescriptorOffset());
      auto contextDescriptorOffsetBytes =
          OpaqueByteReader(remote::RemoteAddress(contextDescriptorFieldAddress),
                           sizeof(uint32_t));
      if (!contextDescriptorOffsetBytes.get()) {
        Error = "Failed to read type descriptor field in conformance descriptor.";
        return typeName;
      }
      auto contextDescriptorOffset =
          (const uint32_t *)contextDescriptorOffsetBytes.get();

      // Read the type descriptor itself using the address computed above
      auto contextDescriptorAddress = detail::applyRelativeOffset(
          (const char *)contextDescriptorFieldAddress,
          (int32_t)*contextDescriptorOffset);
      auto contextDescriptorBytes =
          OpaqueByteReader(remote::RemoteAddress(contextDescriptorAddress),
                           sizeof(ExternalContextDescriptor<PointerSize>));
      if (!contextDescriptorBytes.get()) {
        Error = "Failed to read context descriptor.";
        return typeName;
      }
      const ExternalContextDescriptor<PointerSize> *contextDescriptor =
          (const ExternalContextDescriptor<PointerSize> *)
              contextDescriptorBytes.get();

      auto typeDescriptor =
          dyn_cast<ExternalTypeContextDescriptor<PointerSize>>(
              contextDescriptor);
      if (!typeDescriptor) {
        Error = "Unexpected type of context descriptor.";
        return typeName;
      }

      // Compute the address of the type descriptor's name field and read the
      // offset
      auto typeNameOffsetAddress =
          detail::applyRelativeOffset((const char *)contextDescriptorAddress,
                                      (int32_t)typeDescriptor->getNameOffset());
      auto typeNameOfsetBytes = OpaqueByteReader(
          remote::RemoteAddress(typeNameOffsetAddress), sizeof(uint32_t));
      if (!typeNameOfsetBytes.get()) {
        Error = "Failed to read type name offset in a type descriptor.";
        return typeName;
      }
      auto typeNameOffset = (const uint32_t *)typeNameOfsetBytes.get();

      // Using the offset above, compute the address of the name field itsel
      // and read it.
      auto typeNameAddress = detail::applyRelativeOffset(
          (const char *)typeNameOffsetAddress, (int32_t)*typeNameOffset);
      OpaqueStringReader(remote::RemoteAddress(typeNameAddress), typeName);
      return typeName;
    }

    std::string readProtocolNameFromProtocolDescriptor(const char *protocolDescriptorAddress) {
      std::string protocolName;
      auto protocolDescriptorBytes =
          OpaqueByteReader(remote::RemoteAddress(protocolDescriptorAddress),
                           sizeof(ExternalProtocolDescriptor<PointerSize>));
      if (!protocolDescriptorBytes.get()) {
        Error = "Error reading protocol descriptor.";
        return protocolName;
      }
      const ExternalProtocolDescriptor<PointerSize> *protocolDescriptor =
          (const ExternalProtocolDescriptor<PointerSize> *)
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
        return protocolName;
      }
      auto protocolNameOffset =
          (const uint32_t *)protocolNameOfsetBytes.get();

      // Using the offset above, compute the address of the name field itsel
      // and read it.
      auto protocolNameAddress =
          detail::applyRelativeOffset((const char *)protocolNameOffsetAddress,
                                      (int32_t)*protocolNameOffset);
      OpaqueStringReader(remote::RemoteAddress(protocolNameAddress),
                         protocolName);
      return protocolName;
    }

    std::string getConformanceProtocol(
        const char *conformanceDescriptorAddress,
        const ExternalProtocolConformanceDescriptor<PointerSize>
            &conformanceDescriptor) {
      std::string protocolName = "";
      auto protocolDescriptorFieldAddress = detail::applyRelativeOffset(
          conformanceDescriptorAddress,
          (int32_t)conformanceDescriptor.getProtocolDescriptorOffset());

      auto protocolDescriptorOffsetBytes = OpaqueByteReader(
          remote::RemoteAddress(protocolDescriptorFieldAddress),
          sizeof(uint32_t));
      if (!protocolDescriptorOffsetBytes.get()) {
        Error = "Error reading protocol descriptor field in conformance descriptor.";
        return protocolName;
      }
      auto protocolDescriptorOffset =
          (const uint32_t *)protocolDescriptorOffsetBytes.get();

      auto protocolDescriptorTarget = detail::applyRelativeOffset(
          (const char *)protocolDescriptorFieldAddress,
          (int32_t)*protocolDescriptorOffset);

      // If indirect, attempt to get symbol name from external bindings
      if ((uint64_t)protocolDescriptorTarget & 0x1) {
        auto adjustedProtocolDescriptorTarget =
            (const void *)((uint64_t)protocolDescriptorTarget & ~(0x1));
        if (auto symbol = OpaquePointerReader(
                remote::RemoteAddress(adjustedProtocolDescriptorTarget), 8)) {
          if (!symbol->getSymbol().empty()) {
            Demangle::Context Ctx;
            auto demangledRoot =
                Ctx.demangleSymbolAsNode(symbol->getSymbol().str());
            assert(demangledRoot->getKind() == Node::Kind::Global);
            assert(demangledRoot->getChild(0)->getKind() ==
                   Node::Kind::ProtocolDescriptor);
            protocolName = nodeToString(demangledRoot->getChild(0)->getChild(0));
          } else {
            // This is an absolute address offset.
            auto protocolDescriptorAddress = symbol->getOffset();
            protocolName = readProtocolNameFromProtocolDescriptor((const char*)protocolDescriptorAddress);
          }
        } else {
          Error = "Error reading external protocol address.";
          return protocolName;
        }
        // If direct, read the protocol descriptor and get symbol name
      } else {
        protocolName = readProtocolNameFromProtocolDescriptor((const char*)protocolDescriptorTarget);
      }

      return protocolName;
    }

    /// Given the address of a conformance descriptor, attempt to read it.
    ProtocolConformanceInfo
    readConformanceDescriptor(RemoteRef<void> conformanceRecordRef) {
      const ExternalProtocolConformanceRecord<PointerSize> *CD =
          (const ExternalProtocolConformanceRecord<PointerSize> *)
              conformanceRecordRef.getLocalBuffer();
      // Read the Protocol Conformance Descriptor by getting its address from
      // the conformance record.
      auto conformanceDescriptorAddress = (const char *)CD->getRelative(
          (void *)conformanceRecordRef.getAddressData());

      auto descriptorBytes = OpaqueByteReader(
          remote::RemoteAddress(conformanceDescriptorAddress),
          sizeof(ExternalProtocolConformanceDescriptor<PointerSize>));
      if (!descriptorBytes.get()) {
        Error = "Failed to read protocol conformance descriptor.";
        return ProtocolConformanceInfo();
      }
      const ExternalProtocolConformanceDescriptor<PointerSize>
          *conformanceDescriptorPtr =
              (const ExternalProtocolConformanceDescriptor<PointerSize> *)
                  descriptorBytes.get();

      auto conformingTypeName = getConformingTypeName(
          conformanceDescriptorAddress, *conformanceDescriptorPtr);
      auto conformanceProtocol = getConformanceProtocol(
          conformanceDescriptorAddress, *conformanceDescriptorPtr);

      return ProtocolConformanceInfo{conformingTypeName,
                                     conformanceProtocol};
    }

    bool hasError() {
      return !Error.empty();
    }
  };

public:
  template <unsigned PointerSize>
  void dumpConformanceSection(std::ostream &stream) {
    // Collect all conformances and aggregate them per-conforming-type.
    std::unordered_map<std::string, std::vector<std::string>> typeConformances;
    ProtocolConformanceDescriptorReader<PointerSize> conformanceReader(
        OpaqueByteReader, OpaqueStringReader, OpaquePointerReader);

    for (const auto &section : ReflectionInfos) {
      auto ConformanceBegin = section.Conformance.startAddress();
      auto ConformanceEnd = section.Conformance.endAddress();
      for (auto conformanceAddr = ConformanceBegin;
           conformanceAddr != ConformanceEnd;
           conformanceAddr = conformanceAddr.atByteOffset(4)) {
        auto conformanceInfo =
            conformanceReader.readConformanceDescriptor(conformanceAddr);
        if (conformanceReader.hasError()) {
          stream << "Error reading conformance descriptor: "
                 << conformanceReader.Error << "\n";
          continue;
        }

        if (typeConformances.count(conformanceInfo.typeName) != 0) {
          typeConformances[conformanceInfo.typeName].push_back(
              conformanceInfo.protocolName);
        } else {
          typeConformances.emplace(
              conformanceInfo.typeName,
              std::vector<std::string>{conformanceInfo.protocolName});
        }
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
  }

  template <unsigned PointerSize>
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
    dumpConformanceSection<PointerSize>(stream);
    stream << "\n";
  }
};

} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREFBUILDER_H
