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

#include <iostream>
#include <vector>
#include <unordered_map>

namespace swift {
namespace reflection {

template <typename Runtime> class ReflectionContext;

template <typename Iterator>
class ReflectionSection {
  using const_iterator = Iterator;
  const void * Begin;
  const void * End;

public:
  ReflectionSection(const void * Begin,
                    const void * End)
  : Begin(Begin), End(End) {}

  ReflectionSection(uint64_t Begin, uint64_t End)
  : Begin(reinterpret_cast<const void *>(Begin)),
    End(reinterpret_cast<const void *>(End)) {}

  void *startAddress() {
    return const_cast<void *>(Begin);
  }
  const void *startAddress() const {
    return Begin;
  }
  
  const void *endAddress() const {
    return End;
  }

  const_iterator begin() const {
    return const_iterator(Begin, End);
  }

  const_iterator end() const {
    return const_iterator(End, End);
  }

  size_t size() const {
    return (const char *)End - (const char *)Begin;
  }
};

using FieldSection = ReflectionSection<FieldDescriptorIterator>;
using AssociatedTypeSection = ReflectionSection<AssociatedTypeIterator>;
using BuiltinTypeSection = ReflectionSection<BuiltinTypeDescriptorIterator>;
using CaptureSection = ReflectionSection<CaptureDescriptorIterator>;
using GenericSection = ReflectionSection<const void *>;

struct ReflectionInfo {
  struct {
    FieldSection Metadata;
    uint64_t SectionOffset;
  } Field;

  struct {
    AssociatedTypeSection Metadata;
    uint64_t SectionOffset;
  } AssociatedType;

  struct {
    BuiltinTypeSection Metadata;
    uint64_t SectionOffset;
  } Builtin;

  struct {
    CaptureSection Metadata;
    uint64_t SectionOffset;
  } Capture;

  struct {
    GenericSection Metadata;
    uint64_t SectionOffset;
  } TypeReference;

  struct {
    GenericSection Metadata;
    uint64_t SectionOffset;
  } ReflectionString;

  uint64_t LocalStartAddress;
  uint64_t RemoteStartAddress;
};

struct ClosureContextInfo {
  std::vector<const TypeRef *> CaptureTypes;
  std::vector<std::pair<const TypeRef *, const MetadataSource *>> MetadataSources;
  unsigned NumBindings = 0;

  void dump() const;
  void dump(std::ostream &OS) const;
};

struct FieldTypeInfo {
  std::string Name;
  const TypeRef *TR;
  bool Indirect;

  FieldTypeInfo() : Name(""), TR(nullptr), Indirect(false) {}
  FieldTypeInfo(const std::string &Name, const TypeRef *TR, bool Indirect)
      : Name(Name), TR(TR), Indirect(Indirect) {}

  static FieldTypeInfo forEmptyCase(std::string Name) {
    return FieldTypeInfo(Name, nullptr, false);
  }

  static FieldTypeInfo forIndirectCase(std::string Name, const TypeRef *TR) {
    return FieldTypeInfo(Name, TR, true);
  }

  static FieldTypeInfo forField(std::string Name, const TypeRef *TR) {
    return FieldTypeInfo(Name, TR, false);
  }
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
  using BuiltTypeDecl = Optional<std::string>;
  using BuiltProtocolDecl = Optional<std::pair<std::string, bool /*isObjC*/>>;

  TypeRefBuilder();

  TypeRefBuilder(const TypeRefBuilder &other) = delete;
  TypeRefBuilder &operator=(const TypeRefBuilder &other) = delete;

private:
  Demangle::Demangler Dem;

  std::function<const TypeRef* (const void*, unsigned)>
    OpaqueUnderlyingTypeReader;
  
  /// Makes sure dynamically allocated TypeRefs stick around for the life of
  /// this TypeRefBuilder and are automatically released.
  std::vector<std::unique_ptr<const TypeRef>> TypeRefPool;

  /// Cache for associated type lookups.
  std::unordered_map<TypeRefID, const TypeRef *,
                     TypeRefID::Hash, TypeRefID::Equal> AssociatedTypeCache;

  /// Cache for field info lookups.
  std::unordered_map<std::string,
                     std::pair<const FieldDescriptor *, const ReflectionInfo *>>
                     FieldTypeInfoCache;

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

  ///
  /// Factory methods for all TypeRef kinds
  ///

  const BuiltinTypeRef *createBuiltinType(const std::string &builtinName,
                                          const std::string &mangledName) {
    return BuiltinTypeRef::create(*this, mangledName);
  }

  Optional<std::string>
  createTypeDecl(Node *node, bool &typeAlias) {
    return Demangle::mangleNode(node);
  }

  BuiltProtocolDecl
  createProtocolDecl(Node *node) {
    return std::make_pair(Demangle::mangleNode(node), false);
  }

  BuiltProtocolDecl
  createObjCProtocolDecl(std::string &&name) {
    return std::make_pair(name, true);
  }

  Optional<std::string> createTypeDecl(std::string &&mangledName,
                                       bool &typeAlias) {
    return std::move(mangledName);
  }
  
  const NominalTypeRef *createNominalType(
                                    const Optional<std::string> &mangledName) {
    return NominalTypeRef::create(*this, *mangledName, nullptr);
  }

  const NominalTypeRef *createNominalType(
                                    const Optional<std::string> &mangledName,
                                    const TypeRef *parent) {
    return NominalTypeRef::create(*this, *mangledName, parent);
  }

  const TypeRef *createTypeAliasType(
                                    const Optional<std::string> &mangledName,
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
  createBoundGenericType(const Optional<std::string> &mangledName,
                         const std::vector<const TypeRef *> &args) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, nullptr);
  }

  const BoundGenericTypeRef *
  createBoundGenericType(const Optional<std::string> &mangledName,
                         ArrayRef<const TypeRef *> args,
                         const TypeRef *parent) {
    return BoundGenericTypeRef::create(*this, *mangledName, args, parent);
  }
  
  const TypeRef *
  resolveOpaqueType(NodePointer opaqueDescriptor,
                    ArrayRef<ArrayRef<const TypeRef *>> genericArgs,
                    unsigned ordinal) {
    // TODO: Produce a type ref for the opaque type if the underlying type isn't
    // available.
    
    // Try to resolve to the underlying type, if we can.
    if (opaqueDescriptor->getKind() ==
                            Node::Kind::OpaqueTypeDescriptorSymbolicReference) {
      if (!OpaqueUnderlyingTypeReader)
        return nullptr;
      
      auto underlyingTy = OpaqueUnderlyingTypeReader(
                           (const void *)opaqueDescriptor->getIndex(), ordinal);
      
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
    return nullptr;
  }

  const TupleTypeRef *
  createTupleType(ArrayRef<const TypeRef *> elements,
                  std::string &&labels, bool isVariadic) {
    // FIXME: Add uniqueness checks in TupleTypeRef::Profile and
    // unittests/Reflection/TypeRef.cpp if using labels for identity.
    return TupleTypeRef::create(*this, elements, isVariadic);
  }

  const FunctionTypeRef *createFunctionType(
      ArrayRef<remote::FunctionParam<const TypeRef *>> params,
      const TypeRef *result, FunctionTypeFlags flags) {
    return FunctionTypeRef::create(*this, params, result, flags);
  }

  const FunctionTypeRef *createImplFunctionType(
    Demangle::ImplParameterConvention calleeConvention,
    ArrayRef<Demangle::ImplFunctionParam<const TypeRef *>> params,
    ArrayRef<Demangle::ImplFunctionResult<const TypeRef *>> results,
    Optional<Demangle::ImplFunctionResult<const TypeRef *>> errorResult,
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

    auto result = createTupleType({}, "", false);
    return FunctionTypeRef::create(*this, {}, result, funcFlags);
  }

  const ProtocolCompositionTypeRef *
  createProtocolCompositionType(ArrayRef<BuiltProtocolDecl> protocols,
                                BuiltType superclass,
                                bool isClassBound) {
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

  const ExistentialMetatypeTypeRef *
  createExistentialMetatypeType(const TypeRef *instance,
                    Optional<Demangle::ImplMetatypeRepresentation> repr=None) {
    return ExistentialMetatypeTypeRef::create(*this, instance);
  }

  const MetatypeTypeRef *createMetatypeType(const TypeRef *instance,
                    Optional<Demangle::ImplMetatypeRepresentation> repr=None) {
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
                                  ArrayRef<const TypeRef *> args) {
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

private:
  std::vector<ReflectionInfo> ReflectionInfos;
  
  uint64_t getRemoteAddrOfTypeRefPointer(const void *pointer);

  std::function<auto (SymbolicReferenceKind kind,
                      Directness directness,
                      int32_t offset, const void *base) -> Demangle::Node *>
    SymbolicReferenceResolver;
  
  std::string normalizeReflectionName(StringRef name);
  bool reflectionNameMatches(StringRef reflectionName,
                             StringRef searchName);
  
  Demangle::Node *demangleTypeRef(StringRef mangledName) {
    return Dem.demangleType(mangledName, SymbolicReferenceResolver);
  }
  
public:
  template<typename Runtime>
  void setMetadataReader(
                      remote::MetadataReader<Runtime, TypeRefBuilder> &reader) {
    // Have the TypeRefBuilder demangle symbolic references by reading their
    // demangling out of the referenced context descriptors in the target
    // process.
    SymbolicReferenceResolver =
    [this, &reader](SymbolicReferenceKind kind,
                    Directness directness,
                    int32_t offset, const void *base) -> Demangle::Node * {
      // Resolve the reference to a remote address.
      auto remoteAddress = getRemoteAddrOfTypeRefPointer(base);
      if (remoteAddress == 0)
        return nullptr;
      
      auto address = remoteAddress + offset;
      if (directness == Directness::Indirect) {
        if (auto indirectAddress = reader.readPointerValue(address)) {
          address = *indirectAddress;
        } else {
          return nullptr;
        }
      }
      
      switch (kind) {
      case Demangle::SymbolicReferenceKind::Context: {
        auto context = reader.readContextDescriptor(address);
        if (!context)
          return nullptr;
        // Try to preserve a reference to an OpaqueTypeDescriptor symbolically,
        // since we'd like to read out and resolve the type ref to the
        // underlying type if available.
        if (context->getKind() == ContextDescriptorKind::OpaqueType) {
          return Dem.createNode(
                            Node::Kind::OpaqueTypeDescriptorSymbolicReference,
                            context.getAddressData());
        }
          
        return reader.buildContextMangling(context, Dem);
      }
      case Demangle::SymbolicReferenceKind::AccessorFunctionReference:
        // The symbolic reference points at a resolver function, but we can't
        // execute code in the target process to resolve it from here.
        return nullptr;
      }
      
      return nullptr;
    };
    
    OpaqueUnderlyingTypeReader =
    [&reader](const void *descriptor, unsigned ordinal) -> const TypeRef* {
      auto context = (typename Runtime::StoredPointer)descriptor;
      return reader.readUnderlyingTypeForOpaqueTypeDescriptor(context, ordinal);
    };
  }

  TypeConverter &getTypeConverter() { return TC; }

  const TypeRef *
  lookupTypeWitness(const std::string &MangledTypeName,
                    const std::string &Member,
                    StringRef Protocol);

  const TypeRef *
  lookupSuperclass(const TypeRef *TR);

  /// Load unsubstituted field types for a nominal type.
  std::pair<const FieldDescriptor *, const ReflectionInfo *>
  getFieldTypeInfo(const TypeRef *TR);

  /// Get the parsed and substituted field types for a nominal type.
  bool getFieldTypeRefs(const TypeRef *TR,
           const std::pair<const FieldDescriptor *, const ReflectionInfo *> &FD,
           std::vector<FieldTypeInfo> &Fields);

  /// Get the primitive type lowering for a builtin type.
  const BuiltinTypeDescriptor *getBuiltinTypeInfo(const TypeRef *TR);

  /// Get the raw capture descriptor for a remote capture descriptor
  /// address.
  const CaptureDescriptor *getCaptureDescriptor(uint64_t RemoteAddress);

  /// Get the unsubstituted capture types for a closure context.
  ClosureContextInfo getClosureContextInfo(const CaptureDescriptor &CD,
                                           uint64_t Offset);

  ///
  /// Dumping typerefs, field declarations, associated types
  ///

  void dumpTypeRef(llvm::StringRef MangledName,
                   std::ostream &OS, bool printTypeName = false);
  void dumpFieldSection(std::ostream &OS);
  void dumpAssociatedTypeSection(std::ostream &OS);
  void dumpBuiltinTypeSection(std::ostream &OS);
  void dumpCaptureSection(std::ostream &OS);
  void dumpAllSections(std::ostream &OS);
};


} // end namespace reflection
} // end namespace swift

#endif // SWIFT_REFLECTION_TYPEREFBUILDER_H
