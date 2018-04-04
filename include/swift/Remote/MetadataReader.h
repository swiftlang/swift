//===--- MetadataReader.h - Abstract access to remote metadata --*- C++ -*-===//
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
//  This file defines operations for reading metadata from a remote process.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_REMOTE_METADATAREADER_H
#define SWIFT_REMOTE_METADATAREADER_H

#include "swift/Runtime/Metadata.h"
#include "swift/Remote/MemoryReader.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/TypeDecoder.h"
#include "swift/Basic/Range.h"
#include "swift/Basic/LLVM.h"
#include "swift/Runtime/Unreachable.h"

#include <vector>
#include <unordered_map>

namespace swift {
namespace remote {

template <typename BuiltType>
using FunctionParam = swift::Demangle::FunctionParam<BuiltType>;

template <typename BuilderType>
using TypeDecoder = swift::Demangle::TypeDecoder<BuilderType>;

/// A pointer to the local buffer of an object that also remembers the
/// address at which it was stored remotely.
template <typename Runtime, typename T>
class RemoteRef {
public:
  using StoredPointer = typename Runtime::StoredPointer;

private:
  StoredPointer Address;
  const T *LocalBuffer;

public:
  /*implicit*/
  RemoteRef(std::nullptr_t _)
    : Address(0), LocalBuffer(nullptr) {}

  explicit RemoteRef(StoredPointer address, const T *localBuffer)
    : Address(address), LocalBuffer(localBuffer) {}

  StoredPointer getAddress() const {
    return Address;
  }

  const T *getLocalBuffer() const {
    return LocalBuffer;
  }

  explicit operator bool() const {
    return LocalBuffer != nullptr;
  }

  const T *operator->() const {
    assert(LocalBuffer);
    return LocalBuffer;
  }
};

/// A structure, designed for use with std::unique_ptr, which destroys
/// a pointer by calling free on it (and not trying to call a destructor).
struct delete_with_free {
  void operator()(const void *memory) {
    free(const_cast<void*>(memory));
  }
};

/// A generic reader of metadata.
///
/// BuilderType must implement a particular interface which is currently
/// too fluid to allow useful documentation; consult the actual
/// implementations.  The chief thing is that it provides several member
/// types which should obey the following constraints:
///   - T() yields a value which is false when contextually converted to bool
///   - a false value signals that an error occurred when building a value
template <typename Runtime, typename BuilderType>
class MetadataReader {
public:
  using BuiltType = typename BuilderType::BuiltType;
  using BuiltNominalTypeDecl = typename BuilderType::BuiltNominalTypeDecl;
  using BuiltProtocolDecl = typename BuilderType::BuiltProtocolDecl;
  using StoredPointer = typename Runtime::StoredPointer;
  using StoredSize = typename Runtime::StoredSize;

private:
  /// A cache of built types, keyed by the address of the type.
  std::unordered_map<StoredPointer, BuiltType> TypeCache;

  using MetadataRef =
    RemoteRef<Runtime, TargetMetadata<Runtime>>;
  using OwnedMetadataRef =
    std::unique_ptr<const TargetMetadata<Runtime>, delete_with_free>;

  /// A cache of read type metadata, keyed by the address of the metadata.
  std::unordered_map<StoredPointer, OwnedMetadataRef>
    MetadataCache;

  using ContextDescriptorRef =
    RemoteRef<Runtime, TargetContextDescriptor<Runtime>>;
  using OwnedContextDescriptorRef =
    std::unique_ptr<const TargetContextDescriptor<Runtime>,
                    delete_with_free>;

  /// A cache of read nominal type descriptors, keyed by the address of the
  /// nominal type descriptor.
  std::unordered_map<StoredPointer, OwnedContextDescriptorRef>
    ContextDescriptorCache;

  using OwnedProtocolDescriptorRef =
    std::unique_ptr<const TargetProtocolDescriptor<Runtime>, delete_with_free>;

  enum class IsaEncodingKind {
    /// We haven't checked yet.
    Unknown,

    /// There was an error trying to find out the isa encoding.
    Error,

    /// There's no special isa encoding.
    None,

    /// There's an unconditional mask to apply to the isa pointer.
    ///   - IsaMask stores the mask.
    Masked,

    /// Isa pointers are indexed.  If applying a mask yields a magic value,
    /// applying a different mask and shifting yields an index into a global
    /// array of class pointers.  Otherwise, the isa pointer is just a raw
    /// class pointer.
    ///  - IsaIndexMask stores the index mask.
    ///  - IsaIndexShift stores the index shift.
    ///  - IsaMagicMask stores the magic value mask.
    ///  - IsaMagicValue stores the magic value.
    ///  - IndexedClassesPointer stores the pointer to the start of the
    ///    indexed classes array; this is constant throughout the program.
    ///  - IndexedClassesCountPointer stores a pointer to the number
    ///    of elements in the indexed classes array.
    Indexed
  };

  IsaEncodingKind IsaEncoding = IsaEncodingKind::Unknown;
  union {
    StoredPointer IsaMask;
    StoredPointer IsaIndexMask;
  };
  StoredPointer IsaIndexShift;
  StoredPointer IsaMagicMask;
  StoredPointer IsaMagicValue;
  StoredPointer IndexedClassesPointer;
  StoredPointer IndexedClassesCountPointer;
  StoredPointer LastIndexedClassesCount = 0;

  Demangle::NodeFactory Factory;

  Demangle::NodeFactory &getNodeFactory() { return Factory; }

public:
  BuilderType Builder;

  BuilderType &getBuilder() {
    return this->Builder;
  }

  std::shared_ptr<MemoryReader> Reader;

  template <class... T>
  MetadataReader(std::shared_ptr<MemoryReader> reader, T &&... args)
    : Builder(std::forward<T>(args)...),
      Reader(std::move(reader)) {

  }

  MetadataReader(const MetadataReader &other) = delete;
  MetadataReader &operator=(const MetadataReader &other) = delete;

  /// Clear all of the caches in this reader.
  void clear() {
    TypeCache.clear();
    MetadataCache.clear();
    ContextDescriptorCache.clear();
  }

  /// Given a demangle tree, attempt to turn it into a type.
  BuiltType decodeMangledType(const Demangle::NodePointer &Node) {
    return swift::Demangle::decodeMangledType(Builder, Node);
  }

  /// Get the remote process's swift_isaMask.
  llvm::Optional<StoredPointer> readIsaMask() {
    auto encoding = getIsaEncoding();
    if (encoding != IsaEncodingKind::Masked) {
      // Still return success if there's no isa encoding at all.
      if (encoding == IsaEncodingKind::None)
        return 0;
      else
        return llvm::None;
    }

    return IsaMask;
  }

  /// Given a remote pointer to metadata, attempt to discover its MetadataKind.
  llvm::Optional<MetadataKind>
  readKindFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta) return llvm::None;

    return meta->getKind();
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  StoredPointer
  readSuperClassFromClassMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::Class)
      return StoredPointer();

    auto classMeta = cast<TargetClassMetadata<Runtime>>(meta);
    return classMeta->Superclass;
  }

  /// Given a remote pointer to class metadata, attempt to discover its class
  /// instance size and whether fields should use the resilient layout strategy.
  llvm::Optional<unsigned>
  readInstanceStartAndAlignmentFromClassMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::Class)
      return llvm::None;

    // The following algorithm only works on the non-fragile Apple runtime.

    // Grab the RO-data pointer.  This part is not ABI.
    StoredPointer roDataPtr = readObjCRODataPtr(MetadataAddress);
    if (!roDataPtr)
      return llvm::None;

    // Get the address of the InstanceStart field.
    auto address = roDataPtr + sizeof(uint32_t) * 1;

    unsigned start;
    if (!Reader->readInteger(RemoteAddress(address), &start))
      return llvm::None;

    return start;
  }
  
  /// Given a remote pointer to metadata, attempt to turn it into a type.
  BuiltType readTypeFromMetadata(StoredPointer MetadataAddress,
                                 bool skipArtificialSubclasses = false) {
    auto Cached = TypeCache.find(MetadataAddress);
    if (Cached != TypeCache.end())
      return Cached->second;

    // If we see garbage data in the process of building a BuiltType, and get
    // the same metadata address again, we will hit an infinite loop.
    // Insert a negative result into the cache now so that, if we recur with
    // the same address, we will return the negative result with the check
    // just above.
    TypeCache.insert({MetadataAddress, BuiltType()});

    auto Meta = readMetadata(MetadataAddress);
    if (!Meta) return BuiltType();

    switch (Meta->getKind()) {
    case MetadataKind::Class:
      if (!cast<TargetClassMetadata<Runtime>>(Meta)->isTypeMetadata())
        return BuiltType();
      return readNominalTypeFromMetadata(Meta, skipArtificialSubclasses);
    case MetadataKind::Struct:
    case MetadataKind::Enum:
    case MetadataKind::Optional:
      return readNominalTypeFromMetadata(Meta);
    case MetadataKind::Tuple: {
      auto tupleMeta = cast<TargetTupleTypeMetadata<Runtime>>(Meta);

      std::vector<BuiltType> elementTypes;
      elementTypes.reserve(tupleMeta->NumElements);

      for (unsigned i = 0, n = tupleMeta->NumElements; i != n; ++i) {
        auto &element = tupleMeta->getElement(i);
        if (auto elementType = readTypeFromMetadata(element.Type))
          elementTypes.push_back(elementType);
        else
          return BuiltType();
      }

      // Read the labels string.
      std::string labels;
      if (tupleMeta->Labels &&
          !Reader->readString(RemoteAddress(tupleMeta->Labels), labels))
        return BuiltType();

      auto BuiltTuple = Builder.createTupleType(elementTypes, std::move(labels),
                                                /*variadic*/ false);
      TypeCache[MetadataAddress] = BuiltTuple;
      return BuiltTuple;
    }
    case MetadataKind::Function: {
      auto Function = cast<TargetFunctionTypeMetadata<Runtime>>(Meta);

      std::vector<FunctionParam<BuiltType>> Parameters;
      for (unsigned i = 0, n = Function->getNumParameters(); i != n; ++i) {
        auto ParamTypeRef = readTypeFromMetadata(Function->getParameter(i));
        if (!ParamTypeRef)
          return BuiltType();

        FunctionParam<BuiltType> Param;
        Param.setType(ParamTypeRef);
        Param.setFlags(Function->getParameterFlags(i));
        Parameters.push_back(std::move(Param));
      }

      auto Result = readTypeFromMetadata(Function->ResultType);
      if (!Result)
        return BuiltType();

      auto flags = FunctionTypeFlags()
                       .withConvention(Function->getConvention())
                       .withThrows(Function->throws())
                       .withParameterFlags(Function->hasParameterFlags())
                       .withEscaping(Function->isEscaping());
      auto BuiltFunction =
          Builder.createFunctionType(Parameters, Result, flags);
      TypeCache[MetadataAddress] = BuiltFunction;
      return BuiltFunction;
    }
    case MetadataKind::Existential: {
      auto Exist = cast<TargetExistentialTypeMetadata<Runtime>>(Meta);

      bool HasExplicitAnyObject = false;
      if (Exist->isClassBounded())
        HasExplicitAnyObject = true;

      BuiltType SuperclassType = BuiltType();
      if (Exist->Flags.hasSuperclassConstraint()) {
        // The superclass is stored after the list of protocols.
        SuperclassType = readTypeFromMetadata(
                                              Exist->Protocols[Exist->Protocols.NumProtocols]);
        if (!SuperclassType) return BuiltType();

        HasExplicitAnyObject = true;
      }

      std::vector<BuiltProtocolDecl> Protocols;
      for (size_t i = 0; i < Exist->Protocols.NumProtocols; ++i) {
        auto ProtocolAddress = Exist->Protocols[i];
        auto ProtocolDescriptor = readProtocolDescriptor(ProtocolAddress);
        if (!ProtocolDescriptor)
          return BuiltType();

        std::string MangledNameStr;
        if (!Reader->readString(RemoteAddress(ProtocolDescriptor->Name),
                                MangledNameStr))
          return BuiltType();

        StringRef MangledName =
          Demangle::dropSwiftManglingPrefix(MangledNameStr);

        Demangle::Context DCtx;
        auto Demangled = DCtx.demangleTypeAsNode(MangledName);
        if (!Demangled)
          return BuiltType();

        auto Protocol = Builder.createProtocolDecl(Demangled);
        if (!Protocol)
          return BuiltType();

        Protocols.push_back(Protocol);
      }
      auto BuiltExist = Builder.createProtocolCompositionType(
        Protocols, SuperclassType, HasExplicitAnyObject);
      TypeCache[MetadataAddress] = BuiltExist;
      return BuiltExist;
    }
    case MetadataKind::Metatype: {
      auto Metatype = cast<TargetMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Metatype->InstanceType);
      if (!Instance) return BuiltType();
      auto BuiltMetatype = Builder.createMetatypeType(Instance);
      TypeCache[MetadataAddress] = BuiltMetatype;
      return BuiltMetatype;
    }
    case MetadataKind::ObjCClassWrapper: {
      auto objcWrapper = cast<TargetObjCClassWrapperMetadata<Runtime>>(Meta);
      auto classAddress = objcWrapper->Class;

      std::string className;
      if (!readObjCClassName(classAddress, className))
        return BuiltType();

      auto BuiltObjCClass = Builder.createObjCClassType(std::move(className));
      TypeCache[MetadataAddress] = BuiltObjCClass;
      return BuiltObjCClass;
    }
    case MetadataKind::ExistentialMetatype: {
      auto Exist = cast<TargetExistentialMetatypeMetadata<Runtime>>(Meta);
      auto Instance = readTypeFromMetadata(Exist->InstanceType);
      if (!Instance) return BuiltType();
      auto BuiltExist = Builder.createExistentialMetatypeType(Instance);
      TypeCache[MetadataAddress] = BuiltExist;
      return BuiltExist;
    }
    case MetadataKind::ForeignClass: {
      auto descriptorAddr = readAddressOfNominalTypeDescriptor(Meta);
      if (!descriptorAddr)
        return BuiltType();
      auto descriptor = readContextDescriptor(descriptorAddr);
      if (!descriptor)
        return BuiltType();

      // Build the demangling tree from the context tree.
      Demangle::NodeFactory nodeFactory;
      auto node = buildNominalTypeMangling(descriptor, nodeFactory);
      if (!node)
        return BuiltType();

      auto name = Demangle::mangleNode(node);
      auto BuiltForeign = Builder.createForeignClassType(std::move(name));
      TypeCache[MetadataAddress] = BuiltForeign;
      return BuiltForeign;
    }
    case MetadataKind::HeapLocalVariable:
    case MetadataKind::HeapGenericLocalVariable:
    case MetadataKind::ErrorObject:
      // Treat these all as Builtin.NativeObject for type lowering purposes.
      return Builder.createBuiltinType("Bo");
    case MetadataKind::Opaque: {
      auto BuiltOpaque = Builder.getOpaqueType();
      TypeCache[MetadataAddress] = BuiltOpaque;
      return BuiltOpaque;
    }
    }

    swift_runtime_unreachable("Unhandled MetadataKind in switch");
  }

  BuiltType readTypeFromMangledName(const char *MangledTypeName,
                                    size_t Length) {
    Demangle::Demangler Dem;
    Demangle::NodePointer Demangled =
      Dem.demangleSymbol(StringRef(MangledTypeName, Length));
    return decodeMangledType(Demangled);
  }
  
  /// Read a context descriptor from the given address and build a mangling
  /// tree representing it.
  Demangle::NodePointer
  readDemanglingForContextDescriptor(StoredPointer contextAddress,
                                     Demangler &Dem) {
    auto context = readContextDescriptor(contextAddress);
    if (!context)
      return nullptr;
    return buildNominalTypeMangling(context, Dem);
  }

  /// Read the isa pointer of a class or closure context instance and apply
  /// the isa mask.
  llvm::Optional<StoredPointer>
  readMetadataFromInstance(StoredPointer objectAddress) {
    StoredPointer isa;
    if (!Reader->readInteger(RemoteAddress(objectAddress), &isa))
      return llvm::None;

    switch (getIsaEncoding()) {
    case IsaEncodingKind::Unknown:
    case IsaEncodingKind::Error:
      return llvm::None;

    case IsaEncodingKind::None:
      return isa;

    case IsaEncodingKind::Masked:
      return isa & IsaMask;

    case IsaEncodingKind::Indexed: {
      // If applying the magic mask doesn't give us the magic value,
      // it's not an indexed isa.
      if ((isa & IsaMagicMask) != IsaMagicValue)
        return isa;

      // Extract the index.
      auto classIndex = (isa & IsaIndexMask) >> IsaIndexShift;

      // 0 is never a valid index.
      if (classIndex == 0) {
        return llvm::None;

      // If the index is out of range, it's an error; but check for an
      // update first.  (This will also trigger the first time because
      // we initialize LastIndexedClassesCount to 0).
      } else if (classIndex >= LastIndexedClassesCount) {
        StoredPointer count;
        if (!Reader->readInteger(RemoteAddress(IndexedClassesCountPointer),
                                 &count)) {
          return llvm::None;
        }

        LastIndexedClassesCount = count;
        if (classIndex >= count) {
          return llvm::None;
        }
      }

      // Find the address of the appropriate array element.
      RemoteAddress eltPointer =
        RemoteAddress(IndexedClassesPointer
                        + classIndex * sizeof(StoredPointer));
      StoredPointer metadataPointer;
      if (!Reader->readInteger(eltPointer, &metadataPointer)) {
        return llvm::None;
      }

      return metadataPointer;
    }
    }

    swift_runtime_unreachable("Unhandled IsaEncodingKind in switch.");
  }

  /// Read the offset of the generic parameters of a class from the nominal
  /// type descriptor. If the class has a resilient superclass, we also
  /// have to read the superclass size and add that to the offset.
  ///
  /// The offset is in units of words, from the start of the class's
  /// metadata.
  llvm::Optional<int32_t>
  readGenericArgsOffset(MetadataRef metadata,
                        ContextDescriptorRef descriptor) {
    switch (descriptor->getKind()) {
    case ContextDescriptorKind::Class: {
      auto type = cast<TargetClassDescriptor<Runtime>>(descriptor);

      if (!type->hasResilientSuperclass())
        return type->getNonResilientGenericArgumentOffset();

      auto bounds = readMetadataBoundsOfSuperclass(descriptor);
      if (!bounds)
        return llvm::None;

      bounds->adjustForSubclass(type->areImmediateMembersNegative(),
                                type->NumImmediateMembers);

      return bounds->ImmediateMembersOffset / sizeof(StoredPointer);
    }

    case ContextDescriptorKind::Enum: {
      auto type = cast<TargetEnumDescriptor<Runtime>>(descriptor);
      return type->getGenericArgumentOffset();
    }

    case ContextDescriptorKind::Struct: {
      auto type = cast<TargetStructDescriptor<Runtime>>(descriptor);
      return type->getGenericArgumentOffset();
    }

    default:
      return llvm::None;
    }
  }

  using ClassMetadataBounds = TargetClassMetadataBounds<Runtime>;

  // This follows computeMetadataBoundsForSuperclass.
  llvm::Optional<ClassMetadataBounds>
  readMetadataBoundsOfSuperclass(ContextDescriptorRef subclassRef) {
    auto subclass = cast<TargetClassDescriptor<Runtime>>(subclassRef);

    auto rawSuperclass =
      resolveNullableRelativeField(subclassRef, subclass->Superclass);
    if (!rawSuperclass) {
      return ClassMetadataBounds::forSwiftRootClass();
    }

    return forTypeReference<ClassMetadataBounds>(
      subclass->getSuperclassReferenceKind(), *rawSuperclass,
      [&](ContextDescriptorRef superclass)
            -> llvm::Optional<ClassMetadataBounds> {
        if (!isa<TargetClassDescriptor<Runtime>>(superclass))
          return llvm::None;
        return readMetadataBoundsOfSuperclass(superclass);
      },
      [&](MetadataRef metadata) -> llvm::Optional<ClassMetadataBounds> {
        auto cls = dyn_cast<TargetClassMetadata<Runtime>>(metadata);
        if (!cls)
          return llvm::None;

        return cls->getClassBoundsAsSwiftSuperclass();
      });
  }

  template <class Result, class DescriptorFn, class MetadataFn>
  llvm::Optional<Result>
  forTypeReference(TypeMetadataRecordKind refKind, StoredPointer ref,
                   const DescriptorFn &descriptorFn,
                   const MetadataFn &metadataFn) {
    switch (refKind) {
    case TypeMetadataRecordKind::IndirectNominalTypeDescriptor: {
      StoredPointer descriptorAddress = 0;
      if (!Reader->readInteger(RemoteAddress(ref), &descriptorAddress))
        return llvm::None;

      ref = descriptorAddress;
      LLVM_FALLTHROUGH;
    }

    case TypeMetadataRecordKind::DirectNominalTypeDescriptor: {
      auto descriptor = readContextDescriptor(ref);
      if (!descriptor)
        return llvm::None;

      return descriptorFn(descriptor);
    }

    case TypeMetadataRecordKind::IndirectObjCClass: {
      StoredPointer classRef = 0;
      if (!Reader->readInteger(RemoteAddress(ref), &classRef))
        return llvm::None;

      auto metadata = readMetadata(classRef);
      if (!metadata)
        return llvm::None;

      return metadataFn(metadata);
    }

    default:
      return llvm::None;
    }
  }

  /// Read a single generic type argument from a bound generic type
  /// metadata.
  llvm::Optional<StoredPointer>
  readGenericArgFromMetadata(StoredPointer metadata, unsigned index) {
    auto Meta = readMetadata(metadata);
    if (!Meta)
      return llvm::None;

    auto descriptorAddress = readAddressOfNominalTypeDescriptor(Meta);
    if (!descriptorAddress)
      return llvm::None;

    // Read the nominal type descriptor.
    auto descriptor = readContextDescriptor(descriptorAddress);
    if (!descriptor)
      return llvm::None;

    auto generics = descriptor->getGenericContext();
    if (!generics)
      return llvm::None;
    
    auto offsetToGenericArgs = readGenericArgsOffset(Meta, descriptor);
    if (!offsetToGenericArgs)
      return llvm::None;

    auto addressOfGenericArgAddress =
      (Meta.getAddress() +
       *offsetToGenericArgs * sizeof(StoredPointer) +
       index * sizeof(StoredPointer));

    if (index >= generics->getGenericContextHeader().getNumArguments())
      return llvm::None;

    StoredPointer genericArgAddress;
    if (!Reader->readInteger(RemoteAddress(addressOfGenericArgAddress),
                             &genericArgAddress))
      return llvm::None;

    return genericArgAddress;
  }

  /// Given the address of a nominal type descriptor, attempt to resolve
  /// its nominal type declaration.
  BuiltNominalTypeDecl readNominalTypeFromDescriptor(StoredPointer address) {
    auto descriptor = readContextDescriptor(address);
    if (!descriptor)
      return BuiltNominalTypeDecl();

    return buildNominalTypeDecl(descriptor);
  }

  /// Try to read the offset of a tuple element from a tuple metadata.
  bool readTupleElementOffset(StoredPointer metadataAddress, unsigned eltIndex,
                              StoredSize *offset) {
    // Read the metadata.
    auto metadata = readMetadata(metadataAddress);
    if (!metadata)
      return false;

    // Ensure that the metadata actually is tuple metadata.
    auto tupleMetadata = dyn_cast<TargetTupleTypeMetadata<Runtime>>(metadata);
    if (!tupleMetadata)
      return false;

    // Ensure that the element is in-bounds.
    if (eltIndex >= tupleMetadata->NumElements)
      return false;

    // Read the offset.
    const auto &element = tupleMetadata->getElement(eltIndex);
    *offset = element.Offset;
    return true;
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  llvm::Optional<StoredPointer>
  readOffsetToFirstCaptureFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::HeapLocalVariable)
      return llvm::None;

    auto heapMeta = cast<TargetHeapLocalVariableMetadata<Runtime>>(meta);
    return heapMeta->OffsetToFirstCapture;
  }

  /// Given a remote pointer to class metadata, attempt to read its superclass.
  llvm::Optional<StoredPointer>
  readCaptureDescriptorFromMetadata(StoredPointer MetadataAddress) {
    auto meta = readMetadata(MetadataAddress);
    if (!meta || meta->getKind() != MetadataKind::HeapLocalVariable)
      return llvm::None;

    auto heapMeta = cast<TargetHeapLocalVariableMetadata<Runtime>>(meta);
    return heapMeta->CaptureDescription;
  }

protected:
  template<typename Offset>
  StoredPointer resolveRelativeOffset(StoredPointer targetAddress) {
    Offset relative;
    if (!Reader->readInteger(RemoteAddress(targetAddress), &relative))
      return 0;
    using SignedOffset = typename std::make_signed<Offset>::type;
    using SignedPointer = typename std::make_signed<StoredPointer>::type;
    auto signext = (SignedPointer)(SignedOffset)relative;
    return targetAddress + signext;
  }

  template<typename Offset>
  llvm::Optional<StoredPointer>
  resolveNullableRelativeOffset(StoredPointer targetAddress) {
    Offset relative;
    if (!Reader->readInteger(RemoteAddress(targetAddress), &relative))
      return llvm::None;
    if (relative == 0)
      return 0;
    using SignedOffset = typename std::make_signed<Offset>::type;
    using SignedPointer = typename std::make_signed<StoredPointer>::type;
    auto signext = (SignedPointer)(SignedOffset)relative;
    return targetAddress + signext;
  }

  template<typename Offset>
  llvm::Optional<StoredPointer>
  resolveNullableRelativeIndirectableOffset(StoredPointer targetAddress) {
    Offset relative;
    if (!Reader->readInteger(RemoteAddress(targetAddress), &relative))
      return llvm::None;
    if (relative == 0)
      return 0;
    bool indirect = relative & 1;
    relative &= ~1u;
    
    using SignedOffset = typename std::make_signed<Offset>::type;
    using SignedPointer = typename std::make_signed<StoredPointer>::type;
    auto signext = (SignedPointer)(SignedOffset)relative;
    
    StoredPointer resultAddress = targetAddress + signext;
    
    // Low bit set in the offset indicates that the offset leads to the absolute
    // address in memory.
    if (indirect) {
      if (!Reader->readBytes(RemoteAddress(resultAddress),
                             (uint8_t *)&resultAddress,
                             sizeof(StoredPointer)))
        return llvm::None;
    }
    return resultAddress;
  }

  template<typename Base, typename Field>
  StoredPointer resolveRelativeField(
                            RemoteRef<Runtime, Base> base, const Field &field) {
    // Map the offset from within our local buffer to the remote address.
    auto distance = (intptr_t)&field - (intptr_t)base.getLocalBuffer();
    return resolveRelativeOffset<int32_t>(base.getAddress() + distance);
  }
  
  template<typename Base, typename Field>
  llvm::Optional<StoredPointer> resolveNullableRelativeField(
                            RemoteRef<Runtime, Base> base, const Field &field) {
    // Map the offset from within our local buffer to the remote address.
    auto distance = (intptr_t)&field - (intptr_t)base.getLocalBuffer();

    return resolveNullableRelativeOffset<int32_t>(base.getAddress() + distance);
  }

  template<typename Base, typename Field>
  llvm::Optional<StoredPointer> resolveNullableRelativeIndirectableField(
                            RemoteRef<Runtime, Base> base, const Field &field) {
    // Map the offset from within our local buffer to the remote address.
    auto distance = (intptr_t)&field - (intptr_t)base.getLocalBuffer();
    
    return resolveNullableRelativeIndirectableOffset<int32_t>(
                                                  base.getAddress() + distance);
  }

  /// Given a pointer to an Objective-C class, try to read its class name.
  bool readObjCClassName(StoredPointer classAddress, std::string &className) {
    // The following algorithm only works on the non-fragile Apple runtime.

    // Grab the RO-data pointer.  This part is not ABI.
    StoredPointer roDataPtr = readObjCRODataPtr(classAddress);
    if (!roDataPtr) return false;

    // This is ABI.
    static constexpr auto OffsetToName =
    roundUpToAlignment(size_t(12), sizeof(StoredPointer))
      + sizeof(StoredPointer);

    // Read the name pointer.
    StoredPointer namePtr;
    if (!Reader->readInteger(RemoteAddress(roDataPtr + OffsetToName), &namePtr))
      return false;

    // If the name pointer is null, treat that as an error.
    if (!namePtr)
      return false;

    return Reader->readString(RemoteAddress(namePtr), className);
  }

  MetadataRef readMetadata(StoredPointer address) {
    auto cached = MetadataCache.find(address);
    if (cached != MetadataCache.end())
      return MetadataRef(address, cached->second.get());

    StoredPointer KindValue = 0;
    if (!Reader->readInteger(RemoteAddress(address), &KindValue))
      return nullptr;

    switch (getEnumeratedMetadataKind(KindValue)) {
      case MetadataKind::Class:
        return _readMetadata<TargetClassMetadata>(address);
      case MetadataKind::Enum:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::ErrorObject:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::Existential: {
        StoredPointer flagsAddress = address +
          sizeof(StoredPointer);

        StoredPointer flags;
        if (!Reader->readInteger(RemoteAddress(flagsAddress),
                                 &flags))
          return nullptr;

        StoredPointer numProtocolsAddress = address +
          TargetExistentialTypeMetadata<Runtime>::OffsetToNumProtocols;
        StoredPointer numProtocols;
        if (!Reader->readInteger(RemoteAddress(numProtocolsAddress),
                                 &numProtocols))
          return nullptr;

        // Make sure the number of protocols is reasonable
        if (numProtocols >= 256)
          return nullptr;

        auto totalSize = sizeof(TargetExistentialTypeMetadata<Runtime>)
          + numProtocols *
          sizeof(ConstTargetMetadataPointer<Runtime, TargetProtocolDescriptor>);

        if (ExistentialTypeFlags(flags).hasSuperclassConstraint())
          totalSize += sizeof(StoredPointer);

        return _readMetadata(address, totalSize);
      }
      case MetadataKind::ExistentialMetatype:
        return _readMetadata<TargetExistentialMetatypeMetadata>(address);
      case MetadataKind::ForeignClass:
        return _readMetadata<TargetForeignClassMetadata>(address);
      case MetadataKind::Function: {
        StoredSize flagsValue;
        auto flagsAddr =
            address + TargetFunctionTypeMetadata<Runtime>::OffsetToFlags;
        if (!Reader->readInteger(RemoteAddress(flagsAddr), &flagsValue))
          return nullptr;

        auto flags =
            TargetFunctionTypeFlags<StoredSize>::fromIntValue(flagsValue);

        auto totalSize =
            sizeof(TargetFunctionTypeMetadata<Runtime>) +
            flags.getNumParameters() * sizeof(FunctionTypeMetadata::Parameter);

        if (flags.hasParameterFlags())
          totalSize += flags.getNumParameters() * sizeof(uint32_t);

        return _readMetadata(address,
                             roundUpToAlignment(totalSize, sizeof(void *)));
      }
      case MetadataKind::HeapGenericLocalVariable:
        return _readMetadata<TargetGenericBoxHeapMetadata>(address);
      case MetadataKind::HeapLocalVariable:
        return _readMetadata<TargetHeapLocalVariableMetadata>(address);
      case MetadataKind::Metatype:
        return _readMetadata<TargetMetatypeMetadata>(address);
      case MetadataKind::ObjCClassWrapper:
        return _readMetadata<TargetObjCClassWrapperMetadata>(address);
      case MetadataKind::Opaque:
        return _readMetadata<TargetOpaqueMetadata>(address);
      case MetadataKind::Optional:
        return _readMetadata<TargetEnumMetadata>(address);
      case MetadataKind::Struct:
        return _readMetadata<TargetStructMetadata>(address);
      case MetadataKind::Tuple: {
        auto numElementsAddress = address +
          TargetTupleTypeMetadata<Runtime>::OffsetToNumElements;
        StoredSize numElements;
        if (!Reader->readInteger(RemoteAddress(numElementsAddress),
                                 &numElements))
          return nullptr;
        auto totalSize = sizeof(TargetTupleTypeMetadata<Runtime>) +
                         numElements * sizeof(TupleTypeMetadata::Element);

        // Make sure the number of elements is reasonable
        if (numElements >= 256)
          return nullptr;

        return _readMetadata(address, totalSize);
      }
    }

    // We can fall out here if the value wasn't actually a valid
    // MetadataKind.
    return nullptr;
  }

private:
  template <template <class R> class M>
  MetadataRef _readMetadata(StoredPointer address) {
    return _readMetadata(address, sizeof(M<Runtime>));
  }

  MetadataRef _readMetadata(StoredPointer address, size_t sizeAfter) {
    auto size = sizeAfter;
    uint8_t *buffer = (uint8_t *) malloc(size);
    if (!Reader->readBytes(RemoteAddress(address), buffer, size)) {
      free(buffer);
      return nullptr;
    }

    auto metadata = reinterpret_cast<TargetMetadata<Runtime>*>(buffer);
    MetadataCache.insert(std::make_pair(address, OwnedMetadataRef(metadata)));
    return MetadataRef(address, metadata);
  }

  StoredPointer
  readAddressOfNominalTypeDescriptor(MetadataRef &metadata,
                                     bool skipArtificialSubclasses = false) {
    switch (metadata->getKind()) {
    case MetadataKind::Class: {
      auto classMeta = cast<TargetClassMetadata<Runtime>>(metadata);
      while (true) {
        auto descriptorAddress = classMeta->getDescription();

        // If this class has a null descriptor, it's artificial,
        // and we need to skip it upon request.  Otherwise, we're done.
        if (descriptorAddress || !skipArtificialSubclasses)
          return static_cast<StoredPointer>(descriptorAddress);

        auto superclassMetadataAddress = classMeta->Superclass;
        if (!superclassMetadataAddress)
          return 0;

        auto superMeta = readMetadata(superclassMetadataAddress);
        if (!superMeta)
          return 0;

        auto superclassMeta = dyn_cast<TargetClassMetadata<Runtime>>(superMeta);
        if (!superclassMeta)
          return 0;

        classMeta = superclassMeta;
        metadata = superMeta;
      }
    }

    case MetadataKind::Struct:
    case MetadataKind::Optional:
    case MetadataKind::Enum: {
      auto valueMeta = cast<TargetValueMetadata<Runtime>>(metadata);
      return valueMeta->getDescription();
    }
        
    case MetadataKind::ForeignClass: {
      auto foreignMeta = cast<TargetForeignClassMetadata<Runtime>>(metadata);
      return foreignMeta->Description;
    }

    default:
      return 0;
    }
  }

  /// Given the address of a nominal type descriptor, attempt to read it.
  ContextDescriptorRef
  readContextDescriptor(StoredPointer address) {
    if (address == 0)
      return nullptr;

    auto cached = ContextDescriptorCache.find(address);
    if (cached != ContextDescriptorCache.end())
      return ContextDescriptorRef(address, cached->second.get());

    // Read the flags to figure out how much space we should read.
    ContextDescriptorFlags flags;
    if (!Reader->readBytes(RemoteAddress(address), (uint8_t*)&flags,
                           sizeof(flags)))
      return nullptr;
    
    unsigned baseSize = 0;
    unsigned genericHeaderSize = sizeof(GenericContextDescriptorHeader);
    bool hasVTable = false;
    switch (auto kind = flags.getKind()) {
    case ContextDescriptorKind::Module:
      baseSize = sizeof(TargetModuleContextDescriptor<Runtime>);
      break;
    // TODO: Should we include trailing generic arguments in this load?
    case ContextDescriptorKind::Extension:
      baseSize = sizeof(TargetExtensionContextDescriptor<Runtime>);
      break;
    case ContextDescriptorKind::Anonymous:
      baseSize = sizeof(TargetAnonymousContextDescriptor<Runtime>);
      break;
    case ContextDescriptorKind::Class:
      baseSize = sizeof(TargetClassDescriptor<Runtime>);
      genericHeaderSize = sizeof(TypeGenericContextDescriptorHeader);
      hasVTable = TypeContextDescriptorFlags(flags.getKindSpecificFlags())
                    .class_hasVTable();
      break;
    case ContextDescriptorKind::Enum:
      baseSize = sizeof(TargetEnumDescriptor<Runtime>);
      genericHeaderSize = sizeof(TypeGenericContextDescriptorHeader);
      break;
    case ContextDescriptorKind::Struct:
      baseSize = sizeof(TargetStructDescriptor<Runtime>);
      genericHeaderSize = sizeof(TypeGenericContextDescriptorHeader);
      break;
    default:
      // We don't know about this kind of context.
      return nullptr;
    }
    
    // Determine the full size of the descriptor. This is reimplementing a fair
    // bit of TrailingObjects but for out-of-process; maybe there's a way to
    // factor the layout stuff out...
    unsigned genericsSize = 0;
    if (flags.isGeneric()) {
      GenericContextDescriptorHeader header;
      auto headerAddr = address
        + baseSize
        + genericHeaderSize
        - sizeof(header);
      
      if (!Reader->readBytes(RemoteAddress(headerAddr),
                             (uint8_t*)&header, sizeof(header)))
        return nullptr;
      
      genericsSize = genericHeaderSize
        + (header.NumParams + 3u & ~3u)
        + header.NumRequirements
          * sizeof(TargetGenericRequirementDescriptor<Runtime>);
    }

    unsigned vtableSize = 0;
    if (hasVTable) {
      TargetVTableDescriptorHeader<Runtime> header;
      auto headerAddr = address
        + baseSize
        + genericsSize;
      
      if (!Reader->readBytes(RemoteAddress(headerAddr),
                             (uint8_t*)&header, sizeof(header)))
        return nullptr;

      vtableSize = sizeof(header)
        + header.VTableSize * sizeof(TargetMethodDescriptor<Runtime>);
    }
    
    unsigned size = baseSize + genericsSize + vtableSize;
    auto buffer = (uint8_t *)malloc(size);
    if (!Reader->readBytes(RemoteAddress(address), buffer, size)) {
      free(buffer);
      return nullptr;
    }

    auto descriptor
      = reinterpret_cast<TargetContextDescriptor<Runtime> *>(buffer);

    ContextDescriptorCache.insert(
      std::make_pair(address, OwnedContextDescriptorRef(descriptor)));
    return ContextDescriptorRef(address, descriptor);
  }
  
  ContextDescriptorRef
  readParentContextDescriptor(ContextDescriptorRef base) {
    auto parentAddress =
                  resolveNullableRelativeIndirectableField(base, base->Parent);

    if (parentAddress) {
      return readContextDescriptor(*parentAddress);
    }
    return nullptr;
  }

  /// Given a read nominal type descriptor, attempt to build a demangling tree
  /// for it.
  Demangle::NodePointer
  buildNominalTypeMangling(ContextDescriptorRef descriptor,
                           Demangle::NodeFactory &nodeFactory) {
    std::vector<std::pair<Demangle::Node::Kind, std::string>>
      nameComponents;
    ContextDescriptorRef parent = descriptor;
    
    while (parent) {
      std::string nodeName;
      Demangle::Node::Kind nodeKind;
      
      auto getTypeName = [&]() -> bool {
        auto typeBuffer =
          reinterpret_cast<const TargetTypeContextDescriptor<Runtime> *>
            (parent.getLocalBuffer());
        auto nameAddress = resolveRelativeField(parent, typeBuffer->Name);
        return Reader->readString(RemoteAddress(nameAddress), nodeName);
      };
      
      bool isTypeContext = false;
      switch (auto contextKind = parent->getKind()) {
      case ContextDescriptorKind::Class:
        if (!getTypeName())
          return nullptr;
        nodeKind = Demangle::Node::Kind::Class;
        isTypeContext = true;
        break;
      case ContextDescriptorKind::Struct:
        if (!getTypeName())
          return nullptr;
        nodeKind = Demangle::Node::Kind::Structure;
        isTypeContext = true;
        break;
      case ContextDescriptorKind::Enum:
        if (!getTypeName())
          return nullptr;
        nodeKind = Demangle::Node::Kind::Enum;
        isTypeContext = true;
        break;

      case ContextDescriptorKind::Extension:
        // TODO: Remangle something about the extension context here.
        return nullptr;
        
      case ContextDescriptorKind::Anonymous:
        // TODO: Remangle something about the anonymous context here.
        return nullptr;

      case ContextDescriptorKind::Module: {
        nodeKind = Demangle::Node::Kind::Module;
        auto moduleBuffer =
          reinterpret_cast<const TargetModuleContextDescriptor<Runtime> *>(
            parent.getLocalBuffer());
        auto nameAddress
          = resolveRelativeField(parent, moduleBuffer->Name);
        if (!Reader->readString(RemoteAddress(nameAddress), nodeName))
          return nullptr;
        break;
      }
      
      default:
        // Not a kind of context we know about.
        return nullptr;
      }

      // Override the node kind if this was a Clang-imported type.
      if (isTypeContext) {
        auto typeFlags =
          TypeContextDescriptorFlags(parent->Flags.getKindSpecificFlags());

        if (typeFlags.isCTag())
          nodeKind = Demangle::Node::Kind::Structure;
        else if (typeFlags.isCTypedef())
          nodeKind = Demangle::Node::Kind::TypeAlias;
      }

      nameComponents.emplace_back(nodeKind, nodeName);
      
      parent = readParentContextDescriptor(parent);
    }
    
    // We should have made our way up to a module context.
    if (nameComponents.empty())
      return nullptr;
    if (nameComponents.back().first != Node::Kind::Module)
      return nullptr;
    auto moduleInfo = std::move(nameComponents.back());
    nameComponents.pop_back();
    auto demangling =
      nodeFactory.createNode(Node::Kind::Module, moduleInfo.second);
    for (auto &component : reversed(nameComponents)) {
      auto name = nodeFactory.createNode(Node::Kind::Identifier,
                                         component.second);
      auto parent = nodeFactory.createNode(component.first);
      parent->addChild(demangling, nodeFactory);
      parent->addChild(name, nodeFactory);
      demangling = parent;
    }
    
    auto top = nodeFactory.createNode(Node::Kind::Type);
    top->addChild(demangling, nodeFactory);
    return top;
  }

  /// Given a read nominal type descriptor, attempt to build a
  /// nominal type decl from it.
  BuiltNominalTypeDecl
  buildNominalTypeDecl(ContextDescriptorRef descriptor) {
    // Build the demangling tree from the context tree.
    Demangle::NodeFactory nodeFactory;
    auto node = buildNominalTypeMangling(descriptor, nodeFactory);
    if (!node)
      return BuiltNominalTypeDecl();
    BuiltNominalTypeDecl decl = Builder.createNominalTypeDecl(node);
    return decl;
  }

  OwnedProtocolDescriptorRef
  readProtocolDescriptor(StoredPointer Address) {
    auto Size = sizeof(TargetProtocolDescriptor<Runtime>);
    auto Buffer = (uint8_t *)malloc(Size);
    if (!Reader->readBytes(RemoteAddress(Address), Buffer, Size)) {
      free(Buffer);
      return nullptr;
    }
    auto Casted
      = reinterpret_cast<TargetProtocolDescriptor<Runtime> *>(Buffer);
    return OwnedProtocolDescriptorRef(Casted);
  }

  // TODO: We need to be able to produce protocol conformances for each
  // substitution type as well in order to accurately rebuild bound generic
  // types or types in protocol-constrained inner contexts.
  std::vector<BuiltType>
  getGenericSubst(MetadataRef metadata, ContextDescriptorRef descriptor) {
    auto generics = descriptor->getGenericContext();
    if (!generics)
      return {};
    
    auto numGenericArgs =
      generics->getGenericContextHeader().getNumArguments();
    
    auto offsetToGenericArgs = readGenericArgsOffset(metadata, descriptor);
    if (!offsetToGenericArgs)
      return {};

    auto genericArgsAddr = metadata.getAddress()
      + sizeof(StoredPointer) * *offsetToGenericArgs;

    std::vector<BuiltType> builtSubsts;
    for (auto param : generics->getGenericParams()) {
      switch (param.getKind()) {
      case GenericParamKind::Type:
        // We don't know about type parameters with extra arguments.
        if (param.hasExtraArgument()) {
          return {};
        }
        
        // The type should have a key argument unless it's been same-typed
        // to another type.
        if (param.hasKeyArgument()) {
          if (numGenericArgs == 0)
            return {};
          --numGenericArgs;
          
          StoredPointer arg;
          if (!Reader->readBytes(RemoteAddress(genericArgsAddr),
                                 (uint8_t*)&arg, sizeof(arg))) {
            return {};
          }
          genericArgsAddr += sizeof(StoredPointer);
          
          auto builtArg = readTypeFromMetadata(arg);
          if (!builtArg)
            return {};
          builtSubsts.push_back(builtArg);
        } else {
          // TODO: If the key argument has been concretized by a same-type
          // constraint, that should be reflected in the built nominal type
          // decl's generic constraints. This isn't handled correctly yet.
          return {};
        }
        break;
        
      default:
        // We don't know about this kind of parameter.
        return {};
      }
    }
    return builtSubsts;
  }

  BuiltType readNominalTypeFromMetadata(MetadataRef origMetadata,
                                        bool skipArtificialSubclasses = false) {
    auto metadata = origMetadata;
    auto descriptorAddress =
      readAddressOfNominalTypeDescriptor(metadata,
                                         skipArtificialSubclasses);
    if (!descriptorAddress)
      return BuiltType();

    // If we've skipped an artificial subclasses, check the cache at
    // the superclass.  (This also protects against recursion.)
    if (skipArtificialSubclasses &&
        metadata.getAddress() != origMetadata.getAddress()) {
      auto it = TypeCache.find(metadata.getAddress());
      if (it != TypeCache.end())
        return it->second;
    }

    // Read the nominal type descriptor.
    ContextDescriptorRef descriptor = readContextDescriptor(descriptorAddress);
    if (!descriptor)
      return BuiltType();

    // From that, attempt to resolve a nominal type.
    BuiltNominalTypeDecl typeDecl = buildNominalTypeDecl(descriptor);
    if (!typeDecl)
      return BuiltType();

    // Build the nominal type.
    BuiltType nominal;
    if (descriptor->isGeneric()) {
      // Resolve the generic arguments.
      auto builtGenerics = getGenericSubst(metadata, descriptor);
      if (builtGenerics.empty())
        return BuiltType();
      nominal = Builder.createBoundGenericType(typeDecl, builtGenerics);
    } else {
      nominal = Builder.createNominalType(typeDecl);
    }

    if (!nominal)
      return BuiltType();
    
    TypeCache[metadata.getAddress()] = nominal;

    // If we've skipped an artificial subclass, remove the
    // recursion-protection entry we made for it.
    if (skipArtificialSubclasses &&
        metadata.getAddress() != origMetadata.getAddress()) {
      TypeCache.erase(origMetadata.getAddress());
    }

    return nominal;
  }

  /// Given that the remote process is running the non-fragile Apple runtime,
  /// grab the ro-data from a class pointer.
  StoredPointer readObjCRODataPtr(StoredPointer classAddress) {
    // WARNING: the following algorithm works on current modern Apple
    // runtimes but is not actually ABI.  But it is pretty reliable.

    StoredPointer dataPtr;
    if (!Reader->readInteger(RemoteAddress(classAddress +
                               TargetClassMetadata<Runtime>::offsetToData()),
                             &dataPtr))
      return StoredPointer();

    // Apply the data-pointer mask.
    // These values have been stolen from the runtime source.
    static constexpr uint64_t DataPtrMask =
      (Runtime::PointerSize == 8 ? 0x00007ffffffffff8ULL : 0xfffffffcULL);
    dataPtr &= StoredPointer(DataPtrMask);
    if (!dataPtr)
      return StoredPointer();

    // Read the flags, which is a 32-bit header on both formats.
    uint32_t flags;
    if (!Reader->readInteger(RemoteAddress(dataPtr), &flags))
      return StoredPointer();

    // If the type is not realized, this is the RO-data.
    static constexpr uint32_t RO_REALIZED = 0x80000000U;
    if (!(flags & RO_REALIZED))
      return dataPtr;

    // Otherwise, it's the RW-data; read the RO-data pointer from a
    // well-known position within the RW-data.
    static constexpr uint32_t OffsetToROPtr = 8;
    if (!Reader->readInteger(RemoteAddress(dataPtr + OffsetToROPtr), &dataPtr))
      return StoredPointer();

    return dataPtr;
  }

  IsaEncodingKind getIsaEncoding() {
    if (IsaEncoding != IsaEncodingKind::Unknown)
      return IsaEncoding;

    auto finish = [&](IsaEncodingKind result) -> IsaEncodingKind {
      IsaEncoding = result;
      return result;
    };

    /// Look up the given global symbol and bind 'varname' to its
    /// address if its exists.
#   define tryFindSymbol(varname, symbolName)                \
      auto varname = Reader->getSymbolAddress(symbolName);   \
      if (!varname)                                          \
        return finish(IsaEncodingKind::Error)
    /// Read from the given pointer into 'dest'.
#   define tryReadSymbol(varname, dest) do {                 \
      if (!Reader->readInteger(varname, &dest))              \
        return finish(IsaEncodingKind::Error);               \
    } while (0)
    /// Read from the given global symbol into 'dest'.
#   define tryFindAndReadSymbol(dest, symbolName) do {       \
      tryFindSymbol(_address, symbolName);                    \
      tryReadSymbol(_address, dest);                          \
    } while (0)

    // Check for the magic-mask symbol that indicates that the ObjC
    // runtime is using indexed ISAs.
    if (auto magicMaskAddress =
          Reader->getSymbolAddress("objc_debug_indexed_isa_magic_mask")) {
      tryReadSymbol(magicMaskAddress, IsaMagicMask);
      if (IsaMagicMask != 0) {
        tryFindAndReadSymbol(IsaMagicValue,
                             "objc_debug_indexed_isa_magic_value");
        tryFindAndReadSymbol(IsaIndexMask,
                             "objc_debug_indexed_isa_index_mask");
        tryFindAndReadSymbol(IsaIndexShift,
                             "objc_debug_indexed_isa_index_shift");
        tryFindSymbol(indexedClasses, "objc_indexed_classes");
        IndexedClassesPointer = indexedClasses.getAddressData();
        tryFindSymbol(indexedClassesCount, "objc_indexed_classes_count");
        IndexedClassesCountPointer = indexedClassesCount.getAddressData();

        return finish(IsaEncodingKind::Indexed);
      }
    }

    // Check for the ISA mask symbol.  This has to come second because
    // the standard library will define this even if the ObjC runtime
    // doesn't use it.
    if (auto maskAddress = Reader->getSymbolAddress("swift_isaMask")) {
      tryReadSymbol(maskAddress, IsaMask);
      if (IsaMask != 0) {
        return finish(IsaEncodingKind::Masked);
      }
    }

    return finish(IsaEncodingKind::None);
  }

  template <class T>
  static constexpr T roundUpToAlignment(T offset, T alignment) {
    return (offset + alignment - 1) & ~(alignment - 1);
  }
};

} // end namespace remote
} // end namespace swift

namespace llvm {
  template<typename Runtime, typename T>
  struct simplify_type<swift::remote::RemoteRef<Runtime, T>> {
    using SimpleType = const T *;
    static SimpleType
    getSimplifiedValue(swift::remote::RemoteRef<Runtime, T> value) {
      return value.getLocalBuffer();
    }
  };
}

#endif // SWIFT_REFLECTION_READER_H
