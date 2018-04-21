//===--- MetadataLookup.cpp - Swift Language Type Name Lookup -------------===//
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
// Implementations of runtime functions for looking up a type by name.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/TypeDecoder.h"
#include "swift/Reflection/Records.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Strings.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringExtras.h"
#include "Private.h"
#include "CompatibilityOverride.h"
#include "ImageInspection.h"
#include <functional>
#include <vector>
#include <list>

using namespace swift;
using namespace Demangle;
using namespace reflection;

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif

/// Produce a Demangler value suitable for resolving runtime type metadata
/// strings.
static Demangler getDemanglerForRuntimeTypeResolution() {
  Demangler dem;
  // Resolve symbolic references to type contexts into the absolute address of
  // the type context descriptor, so that if we see a symbolic reference in the
  // mangled name we can immediately find the associated metadata.
  dem.setSymbolicReferenceResolver([&](int32_t offset,
                                       const void *base) -> NodePointer {
    auto absolute_addr = (uintptr_t)detail::applyRelativeOffset(base, offset);
    auto reference = dem.createNode(Node::Kind::SymbolicReference, absolute_addr);
    auto type = dem.createNode(Node::Kind::Type);
    type->addChild(reference, dem);
    return type;
  });
  return dem;
}

#pragma mark Nominal type descriptor cache
// Type Metadata Cache.

namespace {
  struct TypeMetadataSection {
    const TypeMetadataRecord *Begin, *End;
    const TypeMetadataRecord *begin() const {
      return Begin;
    }
    const TypeMetadataRecord *end() const {
      return End;
    }
  };

  struct NominalTypeDescriptorCacheEntry {
  private:
    std::string Name;
    const TypeContextDescriptor *Description;

  public:
    NominalTypeDescriptorCacheEntry(const llvm::StringRef name,
                                    const TypeContextDescriptor *description)
      : Name(name.str()), Description(description) {}

    const TypeContextDescriptor *getDescription() {
      return Description;
    }

    int compareWithKey(llvm::StringRef aName) const {
      return aName.compare(Name);
    }

    template <class... T>
    static size_t getExtraAllocationSize(T &&... ignored) {
      return 0;
    }
  };
} // end anonymous namespace

struct TypeMetadataPrivateState {
  ConcurrentMap<NominalTypeDescriptorCacheEntry> NominalCache;
  std::vector<TypeMetadataSection> SectionsToScan;
  Mutex SectionsToScanLock;

  TypeMetadataPrivateState() {
    SectionsToScan.reserve(16);
    initializeTypeMetadataRecordLookup();
  }

};

static Lazy<TypeMetadataPrivateState> TypeMetadataRecords;

static void
_registerTypeMetadataRecords(TypeMetadataPrivateState &T,
                             const TypeMetadataRecord *begin,
                             const TypeMetadataRecord *end) {
  ScopedLock guard(T.SectionsToScanLock);
  T.SectionsToScan.push_back(TypeMetadataSection{begin, end});
}

void swift::addImageTypeMetadataRecordBlockCallback(const void *records,
                                                    uintptr_t recordsSize) {
  assert(recordsSize % sizeof(TypeMetadataRecord) == 0
         && "weird-sized type metadata section?!");

  // If we have a section, enqueue the type metadata for lookup.
  auto recordBytes = reinterpret_cast<const char *>(records);
  auto recordsBegin
    = reinterpret_cast<const TypeMetadataRecord*>(records);
  auto recordsEnd
    = reinterpret_cast<const TypeMetadataRecord*>(recordBytes + recordsSize);

  // Type metadata cache should always be sufficiently initialized by this
  // point. Attempting to go through get() may also lead to an infinite loop,
  // since we register records during the initialization of
  // TypeMetadataRecords.
  _registerTypeMetadataRecords(TypeMetadataRecords.unsafeGetAlreadyInitialized(),
                               recordsBegin, recordsEnd);
}

void
swift::swift_registerTypeMetadataRecords(const TypeMetadataRecord *begin,
                                         const TypeMetadataRecord *end) {
  auto &T = TypeMetadataRecords.get();
  _registerTypeMetadataRecords(T, begin, end);
}

bool
swift::_contextDescriptorMatchesMangling(const ContextDescriptor *context,
                                         Demangle::NodePointer node) {
  if (node->getKind() == Demangle::Node::Kind::Type)
    node = node->getChild(0);

  while (context) {
    // We can directly match symbolic references to the current context.
    if (node && node->getKind() == Demangle::Node::Kind::SymbolicReference) {
      if (equalContexts(context, reinterpret_cast<const ContextDescriptor *>(
                                     node->getIndex()))) {
        return true;
      }
    }

    switch (context->getKind()) {
    case ContextDescriptorKind::Module: {
      auto module = cast<ModuleContextDescriptor>(context);
      // Match to a mangled module name.
      if (node->getKind() != Demangle::Node::Kind::Module)
        return false;
      if (!node->getText().equals(module->Name.get()))
        return false;
      
      node = nullptr;
      break;
    }
    
    case ContextDescriptorKind::Extension: {
      // TODO: Check whether the extension context constraints match.
      return false;
    }
    
    default:
      if (auto type = llvm::dyn_cast<TypeContextDescriptor>(context)) {
        switch (node->getKind()) {
        // If the mangled name doesn't indicate a type kind, accept anything.
        // Otherwise, try to match them up.
        case Demangle::Node::Kind::OtherNominalType:
          break;
        case Demangle::Node::Kind::Structure:
          if (type->getKind() != ContextDescriptorKind::Struct
              && !type->getTypeContextDescriptorFlags().isCTag())
            return false;
          break;
        case Demangle::Node::Kind::Class:
          if (type->getKind() != ContextDescriptorKind::Class)
            return false;
          break;
        case Demangle::Node::Kind::Enum:
          if (type->getKind() != ContextDescriptorKind::Enum)
            return false;
          break;
        case Demangle::Node::Kind::TypeAlias:
          if (!type->getTypeContextDescriptorFlags().isCTypedef())
            return false;
          break;

        default:
          return false;
        }

        auto nameNode = node->getChild(1);
        if (nameNode->getKind() == Demangle::Node::Kind::PrivateDeclName)
          return false;

        if (nameNode->getText() != type->Name.get())
          return false;
        
        node = node->getChild(0);
        break;
      }
      
      // We don't know about this kind of context, or it doesn't have a stable
      // name we can match to.
      return false;
    }
    
    context = context->Parent;
  }
  
  // We should have reached the top of the node tree at the same time we reached
  // the top of the context tree.
  if (node)
    return false;
  
  return true;
}

// returns the nominal type descriptor for the type named by typeName
static const TypeContextDescriptor *
_searchTypeMetadataRecords(const TypeMetadataPrivateState &T,
                           Demangle::NodePointer node) {
  unsigned sectionIdx = 0;
  unsigned endSectionIdx = T.SectionsToScan.size();
  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = T.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto ntd = record.getTypeContextDescriptor()) {
        if (_contextDescriptorMatchesMangling(ntd, node)) {
          return ntd;
        }
      }
    }
  }

  return nullptr;
}

static const TypeContextDescriptor *
_findNominalTypeDescriptor(Demangle::NodePointer node,
                           Demangle::Demangler &Dem) {
  const TypeContextDescriptor *foundNominal = nullptr;
  auto &T = TypeMetadataRecords.get();

  // If we have a symbolic reference to a context, resolve it immediately.
  NodePointer symbolicNode = node;
  if (symbolicNode->getKind() == Node::Kind::Type)
    symbolicNode = symbolicNode->getChild(0);
  if (symbolicNode->getKind() == Node::Kind::SymbolicReference)
    return cast<TypeContextDescriptor>(
      (const ContextDescriptor *)symbolicNode->getIndex());

  auto mangledName =
    Demangle::mangleNode(node,
                         [&](const void *context) -> NodePointer {
                           return _buildDemanglingForContext(
                               (const ContextDescriptor *) context,
                               {}, false, Dem);
                         });

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.NominalCache.find(mangledName))
    return Value->getDescription();

  // Check type metadata records
  T.SectionsToScanLock.withLock([&] {
    foundNominal = _searchTypeMetadataRecords(T, node);
  });

  // Check protocol conformances table. Note that this has no support for
  // resolving generic types yet.
  if (!foundNominal)
    foundNominal = _searchConformancesByMangledTypeName(node);

  if (foundNominal) {
    T.NominalCache.getOrInsert(mangledName, foundNominal);
  }

  return foundNominal;
}

#pragma mark Protocol descriptor cache
namespace {
  struct ProtocolSection {
    const ProtocolRecord *Begin, *End;

    const ProtocolRecord *begin() const {
      return Begin;
    }
    const ProtocolRecord *end() const {
      return End;
    }
  };

  struct ProtocolDescriptorCacheEntry {
  private:
    std::string Name;
    const ProtocolDescriptor *Description;

  public:
    ProtocolDescriptorCacheEntry(const llvm::StringRef name,
                                 const ProtocolDescriptor *description)
        : Name(name.str()), Description(description) {}

    const ProtocolDescriptor *getDescription() { return Description; }

    int compareWithKey(llvm::StringRef aName) const {
      return aName.compare(Name);
    }

    template <class... T>
    static size_t getExtraAllocationSize(T &&... ignored) {
      return 0;
    }
  };

  struct ProtocolMetadataPrivateState {
    ConcurrentMap<ProtocolDescriptorCacheEntry> ProtocolCache;
    std::vector<ProtocolSection> SectionsToScan;
    Mutex SectionsToScanLock;

    ProtocolMetadataPrivateState() {
      SectionsToScan.reserve(16);
      initializeProtocolLookup();
    }
  };

  static Lazy<ProtocolMetadataPrivateState> Protocols;
}

static void
_registerProtocols(ProtocolMetadataPrivateState &C,
                   const ProtocolRecord *begin,
                   const ProtocolRecord *end) {
  ScopedLock guard(C.SectionsToScanLock);
  C.SectionsToScan.push_back(ProtocolSection{begin, end});
}

void swift::addImageProtocolsBlockCallback(const void *protocols,
                                           uintptr_t protocolsSize) {
  assert(protocolsSize % sizeof(ProtocolRecord) == 0 &&
         "protocols section not a multiple of ProtocolRecord");

  // If we have a section, enqueue the protocols for lookup.
  auto protocolsBytes = reinterpret_cast<const char *>(protocols);
  auto recordsBegin
    = reinterpret_cast<const ProtocolRecord *>(protocols);
  auto recordsEnd
    = reinterpret_cast<const ProtocolRecord *>(protocolsBytes + protocolsSize);

  // Conformance cache should always be sufficiently initialized by this point.
  _registerProtocols(Protocols.unsafeGetAlreadyInitialized(),
                     recordsBegin, recordsEnd);
}

void swift::swift_registerProtocols(const ProtocolRecord *begin,
                                    const ProtocolRecord *end) {
  auto &C = Protocols.get();
  _registerProtocols(C, begin, end);
}

static const ProtocolDescriptor *
_searchProtocolRecords(const ProtocolMetadataPrivateState &C,
                       const llvm::StringRef protocolName){
  unsigned sectionIdx = 0;
  unsigned endSectionIdx = C.SectionsToScan.size();
  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = C.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto protocol = record.Protocol.getPointer()) {
        // Drop the "S$" prefix from the protocol record. It's not used in
        // the type itself.
        StringRef foundProtocolName = protocol->Name;
        assert(foundProtocolName.startswith("$S"));
        foundProtocolName = foundProtocolName.drop_front(2);
        if (foundProtocolName == protocolName)
          return protocol;
      }
    }
  }

  return nullptr;
}

static const ProtocolDescriptor *
_findProtocolDescriptor(llvm::StringRef mangledName) {
  const ProtocolDescriptor *foundProtocol = nullptr;
  auto &T = Protocols.get();

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.ProtocolCache.find(mangledName))
    return Value->getDescription();

  // Check type metadata records
  T.SectionsToScanLock.withLock([&] {
    foundProtocol = _searchProtocolRecords(T, mangledName);
  });

  if (foundProtocol) {
    T.ProtocolCache.getOrInsert(mangledName, foundProtocol);
  }

  return foundProtocol;
}

#pragma mark Type field descriptor cache
namespace {
struct FieldDescriptorCacheEntry {
private:
  const Metadata *Type;
  const FieldDescriptor *Description;

public:
  FieldDescriptorCacheEntry(const Metadata *type,
                            const FieldDescriptor *description)
      : Type(type), Description(description) {}

  const FieldDescriptor *getDescription() { return Description; }

  int compareWithKey(const Metadata *other) const {
    auto a = (uintptr_t)Type;
    auto b = (uintptr_t)other;
    return a == b ? 0 : (a < b ? -1 : 1);
  }

  template <class... Args>
  static size_t getExtraAllocationSize(Args &&... ignored) {
    return 0;
  }
};

class StaticFieldSection {
  const void *Begin;
  const void *End;

public:
  StaticFieldSection(const void *begin, const void *end)
      : Begin(begin), End(end) {}

  FieldDescriptorIterator begin() const {
    return FieldDescriptorIterator(Begin, End);
  }

  FieldDescriptorIterator end() const {
    return FieldDescriptorIterator(End, End);
  }
};

class DynamicFieldSection {
  const FieldDescriptor **Begin;
  const FieldDescriptor **End;

public:
  DynamicFieldSection(const FieldDescriptor **fields, size_t size)
      : Begin(fields), End(fields + size) {}

  const FieldDescriptor **begin() { return Begin; }

  const FieldDescriptor **end() const { return End; }
};

struct FieldCacheState {
  ConcurrentMap<FieldDescriptorCacheEntry> FieldCache;

  Mutex SectionsLock;
  std::vector<StaticFieldSection> StaticSections;
  std::vector<DynamicFieldSection> DynamicSections;

  FieldCacheState() {
    StaticSections.reserve(16);
    DynamicSections.reserve(8);
    initializeTypeFieldLookup();
  }
};

static Lazy<FieldCacheState> FieldCache;
} // namespace

void swift::swift_registerFieldDescriptors(const FieldDescriptor **records,
                                           size_t size) {
  auto &cache = FieldCache.get();
  ScopedLock guard(cache.SectionsLock);
  cache.DynamicSections.push_back({records, size});
}

void swift::addImageTypeFieldDescriptorBlockCallback(const void *recordsBegin,
                                                     uintptr_t size) {
  auto sectionBytes = reinterpret_cast<const char *>(recordsBegin);
  auto recordsEnd = reinterpret_cast<const void *>(sectionBytes + size);

  // Field cache should always be sufficiently initialized by this point.
  auto &cache = FieldCache.unsafeGetAlreadyInitialized();
  ScopedLock guard(cache.SectionsLock);
  cache.StaticSections.push_back({recordsBegin, recordsEnd});
}

#pragma mark Metadata lookup via mangled name

#if SWIFT_OBJC_INTEROP
/// For a mangled node that refers to an Objective-C class or protocol,
/// return the class or protocol name.
static Optional<StringRef> getObjCClassOrProtocolName(
                                           const Demangle::NodePointer &node) {
  if (node->getKind() != Demangle::Node::Kind::Class &&
      node->getKind() != Demangle::Node::Kind::Protocol)
    return None;

  if (node->getNumChildren() != 2)
    return None;

  // Check whether we have the __ObjC module.
  auto moduleNode = node->getChild(0);
  if (moduleNode->getKind() != Demangle::Node::Kind::Module ||
      moduleNode->getText() != MANGLING_MODULE_OBJC)
    return None;

  // Check whether we have an identifier.
  auto nameNode = node->getChild(1);
  if (nameNode->getKind() != Demangle::Node::Kind::Identifier)
    return None;

  return nameNode->getText();
}
#endif

Optional<unsigned> swift::_depthIndexToFlatIndex(
                                              unsigned depth, unsigned index,
                                              ArrayRef<unsigned> paramCounts) {
  // Out-of-bounds depth.
  if (depth >= paramCounts.size()) return None;

  // Compute the flat index.
  unsigned flatIndex = index + (depth == 0 ? 0 : paramCounts[depth - 1]);

  // Out-of-bounds index.
  if (flatIndex >= paramCounts[depth]) return None;

  return flatIndex;
}

/// Gather generic parameter counts from a context descriptor.
///
/// \returns true if the innermost descriptor is generic.
bool swift::_gatherGenericParameterCounts(
                                 const ContextDescriptor *descriptor,
                                 std::vector<unsigned> &genericParamCounts) {
  // Once we hit a non-generic descriptor, we're done.
  if (!descriptor->isGeneric()) return false;

  // Recurse to record the parent context's generic parameters.
  if (auto parent = descriptor->Parent.get())
    (void)_gatherGenericParameterCounts(parent, genericParamCounts);

  // Record a new level of generic parameters if the count exceeds the
  // previous count.
  auto myCount =
    descriptor->getGenericContext()->getGenericContextHeader().NumParams;
  if (genericParamCounts.empty() || myCount > genericParamCounts.back()) {
    genericParamCounts.push_back(myCount);
    return true;
  }

  return false;
}

namespace {

/// Find the offset of the protocol requirement for an associated type with
/// the given name in the given protocol descriptor.
Optional<unsigned> findAssociatedTypeByName(const ProtocolDescriptor *protocol,
                                            StringRef name) {
  // Only Swift protocols have associated types.
  if (!protocol->Flags.isSwift()) return None;

  // If we don't have associated type names, there's nothing to do.
  const char *associatedTypeNamesPtr = protocol->AssociatedTypeNames.get();
  if (!associatedTypeNamesPtr) return None;

  // Look through the list of associated type names.
  StringRef associatedTypeNames(associatedTypeNamesPtr);
  unsigned matchingAssocTypeIdx = 0;
  bool found = false;
  while (!associatedTypeNames.empty()) {
    auto split = associatedTypeNames.split(' ');
    if (split.first == name) {
      found = true;
      break;
    }

    ++matchingAssocTypeIdx;
    associatedTypeNames = split.second;
  }

  if (!found) return None;

  // We have a match on the Nth associated type; go find the Nth associated
  // type requirement.
  unsigned currentAssocTypeIdx = 0;
  unsigned numRequirements = protocol->NumRequirements;
  const ProtocolRequirement *requirements = protocol->Requirements.get();
  for (unsigned reqIdx = 0; reqIdx != numRequirements; ++reqIdx) {
    if (requirements[reqIdx].Flags.getKind() !=
        ProtocolRequirementFlags::Kind::AssociatedTypeAccessFunction)
      continue;

    if (currentAssocTypeIdx == matchingAssocTypeIdx)
      return reqIdx + WitnessTableFirstRequirementOffset;

    ++currentAssocTypeIdx;
  }

  swift_runtime_unreachable("associated type names don't line up");
}

/// Constructs metadata by decoding a mangled type name, for use with
/// \c TypeDecoder.
class DecodedMetadataBuilder {
public:
  /// Callback used to handle the substitution of a generic parameter for
  /// its metadata.
  using SubstGenericParameterFn =
    std::function<const Metadata *(unsigned depth, unsigned index)>;

  /// Callback used to handle the lookup of dependent member types.
  using LookupDependentMemberFn =
    std::function<const Metadata *(const Metadata *base, StringRef assocType,
                                   const ProtocolDescriptor *protocol)>;

private:
  /// The demangler we'll use when building new nodes.
  Demangler &demangler;

  /// Substitute generic parameters.
  SubstGenericParameterFn substGenericParameter;

  /// Lookup dependent member types.
  LookupDependentMemberFn lookupDependentMember;

  /// Ownership information related to the metadata we are trying to lookup.
  TypeReferenceOwnership ReferenceOwnership;

public:
  DecodedMetadataBuilder(Demangler &demangler,
                         SubstGenericParameterFn substGenericParameter
                           = nullptr,
                         LookupDependentMemberFn lookupDependentMember
                           = nullptr)
    : demangler(demangler),
      substGenericParameter(substGenericParameter),
      lookupDependentMember(lookupDependentMember) { }

  using BuiltType = const Metadata *;

  struct BuiltNominalTypeDecl :
    llvm::PointerUnion<const TypeContextDescriptor *, const Metadata *>
  {
    using PointerUnion::PointerUnion;

    explicit operator bool() const { return !isNull(); }
  };

  using BuiltProtocolDecl = const ProtocolDescriptor *;

  Demangle::NodeFactory &getNodeFactory() { return demangler; }

  BuiltNominalTypeDecl createNominalTypeDecl(
                                     const Demangle::NodePointer &node) const {
#if SWIFT_OBJC_INTEROP
    // If we have an Objective-C class name, call into the Objective-C
    // runtime to find them.
    if (auto objcClassName = getObjCClassOrProtocolName(node)) {
      auto objcClass = objc_getClass(objcClassName->str().c_str());
      return swift_getObjCClassMetadata((const ClassMetadata *)objcClass);
    }
#endif

    // Look for a nominal type descriptor based on its mangled name.
    return _findNominalTypeDescriptor(node, demangler);
  }

  BuiltProtocolDecl createProtocolDecl(
                                    const Demangle::NodePointer &node) const {
#if SWIFT_OBJC_INTEROP
    // If we have an Objective-C class name, call into the Objective-C
    // runtime to find them.
    if (auto objcProtocolName = getObjCClassOrProtocolName(node)) {
      return (ProtocolDescriptor *)objc_getProtocol(
                                              objcProtocolName->str().c_str());
    }
#endif

    auto mangledName = Demangle::mangleNode(node);

    // Look for a Swift protocol with this mangled name.
    if (auto protocol = _findProtocolDescriptor(mangledName))
      return protocol;

#if SWIFT_OBJC_INTEROP
    // Look for a Swift-defined @objc protocol with the Swift 3 mangling that
    // is used for Objective-C entities.
    std::string objcMangledName =
      "_TtP" + mangledName.substr(0, mangledName.size()-1) + "_";
    if (auto protocol = objc_getProtocol(objcMangledName.c_str()))
      return (ProtocolDescriptor *)protocol;
#endif

    return nullptr;
  }

  BuiltType createNominalType(BuiltNominalTypeDecl metadataOrTypeDecl,
                              BuiltType parent) const {
    // Treat nominal type creation the same way as generic type creation,
    // but with no generic arguments at this level.
    return createBoundGenericType(metadataOrTypeDecl, { }, parent);
  }

  BuiltType createBoundGenericType(BuiltNominalTypeDecl metadataOrTypeDecl,
                                   const ArrayRef<BuiltType> genericArgs,
                                   const BuiltType parent) const {
    // If we already have metadata, return it.
    if (auto metadata = metadataOrTypeDecl.dyn_cast<const Metadata *>())
      return metadata;

    auto typeDecl = metadataOrTypeDecl.get<const TypeContextDescriptor *>();

    // Figure out the various levels of generic parameters we have in
    // this type.
    std::vector<unsigned> genericParamCounts;
    bool innermostIsGeneric;

    // If we have no parent given, try to form the whole type in one go.
    if (!parent) {
      innermostIsGeneric = !genericArgs.empty();
      if (innermostIsGeneric) {
        genericParamCounts.push_back(genericArgs.size());
      }
    // Otherwise, we'll need to steal the generic arguments from the parent
    // type to build a nested type.
    } else {
      innermostIsGeneric = _gatherGenericParameterCounts(typeDecl,
                                                         genericParamCounts);
    }
    bool isGeneric = !genericParamCounts.empty();

    // Gather the generic arguments.
    std::vector<const void *> allGenericArgsVec;
    ArrayRef<const void *> allGenericArgs;

    // If the innermost type is generic, we need to gather arguments and
    // check requirements.
    if (innermostIsGeneric) {
      // If no generic arguments were provided at this level, fail.
      if (genericArgs.empty()) return BuiltType();

      unsigned startParamIndex;
      if (genericParamCounts.size() > 1) {
        // When there is more than one level of generic parameters, copy all of
        // the key type parameters from the parent (but not any of the other
        // requirements, e.g., witness tables are excluded).
        auto parentGenericArgs = parent->getGenericArgs();
        auto parentGenericParams =
          typeDecl->Parent->getGenericContext()->getGenericParams();
        unsigned parentArgIndex = 0;
        for (const auto &parentGenericParam : parentGenericParams) {
          if (parentGenericParam.hasKeyArgument())
            allGenericArgsVec.push_back(parentGenericArgs[parentArgIndex++]);
        }

        startParamIndex = parentGenericParams.size();
      } else {
        startParamIndex = 0;
      }

      // If we have the wrong number of generic arguments, fail.
      auto genericContext = typeDecl->getGenericContext();
      auto genericParams = genericContext->getGenericParams();
      if (genericArgs.size() != genericParamCounts.back() - startParamIndex)
        return BuiltType();

      // Add generic arguments for the key parameters at this level.
      unsigned genericArgIndex = 0;
      for (const auto &genericParam : genericParams.slice(startParamIndex)) {
        if (genericParam.hasKeyArgument())
          allGenericArgsVec.push_back(genericArgs[genericArgIndex++]);
      }

      // Check whether the generic requirements are satisfied, collecting
      // any extra arguments we need for the instantiation function.
      bool failed =
        _checkGenericRequirements(genericContext->getGenericRequirements(),
                                  allGenericArgsVec,
            [&](unsigned flatIndex) -> BuiltType {
              // FIXME: Wrong for same-type-to-concrete
              // constraints.
              if (flatIndex < allGenericArgsVec.size())
                return static_cast<BuiltType>(allGenericArgsVec[flatIndex]);

              return BuiltType();
            },
            [&](unsigned depth, unsigned index) -> BuiltType {
              auto flatIndex = _depthIndexToFlatIndex(depth, index,
                                                      genericParamCounts);
              // FIXME: Wrong for same-type-to-concrete
              // constraints.
              if (flatIndex && *flatIndex < allGenericArgsVec.size())
                return static_cast<BuiltType>(allGenericArgsVec[*flatIndex]);

              return BuiltType();
            });
      if (failed)
        return BuiltType();

      // If we still have the wrong number of generic arguments, this is
      // some kind of metadata mismatch.
      // FIXME: Fail silently? Complain loudly?
      assert(typeDecl->getGenericContextHeader().getNumArguments() ==
             allGenericArgsVec.size());

      allGenericArgs = allGenericArgsVec;
    } else {
      // If generic arguments were provided at this level, fail.
      if (!genericArgs.empty()) return BuiltType();

      // If this is a generic context, get all of the arguments from our
      // parent.
      if (isGeneric) {
        if (!parent) return BuiltType();

        auto numGenericArgs =
          typeDecl->getGenericContextHeader().getNumArguments();
        auto parentGenericArgs =
          reinterpret_cast<const void * const *>(parent->getGenericArgs());
        allGenericArgs =
          llvm::makeArrayRef(parentGenericArgs, numGenericArgs);
      }
    }

    // Call the access function.
    auto accessFunction = typeDecl->getAccessFunction();
    if (!accessFunction) return BuiltType();

    return accessFunction(MetadataState::Complete, allGenericArgs).Value;
  }

  BuiltType createBuiltinType(StringRef mangledName) const {
#define BUILTIN_TYPE(Symbol, _) \
    if (mangledName.equals(#Symbol)) \
      return &METADATA_SYM(Symbol).base;
#include "swift/Runtime/BuiltinTypes.def"
    return BuiltType();
  }

  BuiltType createMetatypeType(BuiltType instance, bool wasAbstract) const {
    return swift_getMetatypeMetadata(instance);
  }

  BuiltType createExistentialMetatypeType(BuiltType instance) const {
    return swift_getExistentialMetatypeMetadata(instance);
  }

  BuiltType createProtocolCompositionType(ArrayRef<BuiltProtocolDecl> protocols,
                                          BuiltType superclass,
                                          bool isClassBound) const {
    // Determine whether we have a class bound.
    ProtocolClassConstraint classConstraint = ProtocolClassConstraint::Any;
    if (isClassBound || superclass) {
      classConstraint = ProtocolClassConstraint::Class;
    } else {
      for (auto protocol : protocols) {
        if (protocol->Flags.getClassConstraint()
              == ProtocolClassConstraint::Class) {
          classConstraint = ProtocolClassConstraint::Class;
          break;
        }
      }
    }

    return swift_getExistentialTypeMetadata(classConstraint, superclass,
                                            protocols.size(), protocols.data());
  }

  BuiltType createGenericTypeParameterType(unsigned depth,
                                           unsigned index) const {
    // Use the callback, when provided.
    if (substGenericParameter)
      return substGenericParameter(depth, index);

    return BuiltType();
  }

  BuiltType createFunctionType(
                           ArrayRef<Demangle::FunctionParam<BuiltType>> params,
                           BuiltType result, FunctionTypeFlags flags) const {
    std::vector<BuiltType> paramTypes;
    std::vector<uint32_t> paramFlags;

    // Fill in the parameters.
    paramTypes.reserve(params.size());
    if (flags.hasParameterFlags())
      paramFlags.reserve(params.size());
    for (const auto &param : params) {
      paramTypes.push_back(param.getType());
      if (flags.hasParameterFlags())
        paramFlags.push_back(param.getFlags().getIntValue());
    }

    return swift_getFunctionTypeMetadata(flags, paramTypes.data(),
                                         flags.hasParameterFlags()
                                           ? paramFlags.data()
                                           : nullptr,
                                         result);
  }

  BuiltType createTupleType(ArrayRef<BuiltType> elements,
                            std::string labels,
                            bool variadic) const {
    // TODO: 'variadic' should no longer exist
    auto flags = TupleTypeFlags().withNumElements(elements.size());
    if (!labels.empty())
      flags = flags.withNonConstantLabels(true);
    return swift_getTupleTypeMetadata(MetadataState::Complete,
                                      flags, elements.data(),
                                      labels.empty() ? nullptr : labels.c_str(),
                                      /*proposedWitnesses=*/nullptr).Value;
  }

  BuiltType createDependentMemberType(StringRef name, BuiltType base,
                                      BuiltProtocolDecl protocol) const {
    if (lookupDependentMember)
      return lookupDependentMember(base, name, protocol);

    return BuiltType();
  }

  BuiltType createUnownedStorageType(BuiltType base) {
    ReferenceOwnership.setUnowned();
    return base;
  }

  BuiltType createUnmanagedStorageType(BuiltType base) {
    ReferenceOwnership.setUnmanaged();
    return base;
  }

  BuiltType createWeakStorageType(BuiltType base) {
    ReferenceOwnership.setWeak();
    return base;
  }

  BuiltType createSILBoxType(BuiltType base) const {
    // FIXME: Implement.
    return BuiltType();
  }

  TypeReferenceOwnership getReferenceOwnership() const {
    return ReferenceOwnership;
  }
};

}

TypeInfo
swift::_getTypeByMangledName(StringRef typeName,
                             SubstGenericParameterFn substGenericParam) {
  auto demangler = getDemanglerForRuntimeTypeResolution();
  NodePointer node;

  // Check whether this is the convenience syntax "ModuleName.ClassName".
  auto getDotPosForConvenienceSyntax = [&]() -> size_t {
    size_t dotPos = typeName.find('.');
    if (dotPos == llvm::StringRef::npos)
      return llvm::StringRef::npos;
    if (typeName.find('.', dotPos + 1) != llvm::StringRef::npos)
      return llvm::StringRef::npos;
    if (typeName.find('\1') != llvm::StringRef::npos)
      return llvm::StringRef::npos;
    return dotPos;
  };

  auto dotPos = getDotPosForConvenienceSyntax();
  if (dotPos != llvm::StringRef::npos) {
    // Form a demangle tree for this class.
    NodePointer classNode = demangler.createNode(Node::Kind::Class);
    NodePointer moduleNode = demangler.createNode(Node::Kind::Module,
                                                  typeName.substr(0, dotPos));
    NodePointer nameNode = demangler.createNode(Node::Kind::Identifier,
                                                typeName.substr(dotPos + 1));
    classNode->addChild(moduleNode, demangler);
    classNode->addChild(nameNode, demangler);

    node = classNode;
  } else {
    // Demangle the type name.
    node = demangler.demangleType(typeName);
    if (!node)
      return TypeInfo();
  }
  
  DecodedMetadataBuilder builder(demangler, substGenericParam,
    [](const Metadata *base, StringRef assocType,
       const ProtocolDescriptor *protocol) -> const Metadata * {
      // Look for a conformance of the base type to the protocol.
      auto witnessTable = swift_conformsToProtocol(base, protocol);
      if (!witnessTable) return nullptr;

      // Look for the named associated type within the protocol.
      auto assocTypeReqIndex = findAssociatedTypeByName(protocol, assocType);
      if (!assocTypeReqIndex) return nullptr;

      // Call the associated type access function.
      // TODO: can we just request abstract metadata?  If so, do we have
      //   a responsibility to try to finish it later?
      return ((AssociatedTypeAccessFunction * const *)witnessTable)[*assocTypeReqIndex]
                (MetadataState::Complete, base, witnessTable).Value;
    });

  auto type = Demangle::decodeMangledType(builder, node);
  return {type, builder.getReferenceOwnership()};
}

static const Metadata * _Nullable
swift_getTypeByMangledNameImpl(const char *typeNameStart, size_t typeNameLength,
                           size_t numberOfLevels,
                           size_t *parametersPerLevel,
                           const Metadata * const *flatSubstitutions) {
  llvm::StringRef typeName(typeNameStart, typeNameLength);
  return _getTypeByMangledName(typeName,
    [&](unsigned depth, unsigned index) -> const Metadata * {
      if (depth >= numberOfLevels)
        return nullptr;

      if (index >= parametersPerLevel[depth])
        return nullptr;

      unsigned flatIndex = index;
      for (unsigned i = 0; i < depth; ++i)
        flatIndex += parametersPerLevel[i];

      return flatSubstitutions[flatIndex];
    });
}

void swift::swift_getFieldAt(
    const Metadata *base, unsigned index,
    std::function<void(llvm::StringRef name, FieldType fieldInfo)>
        callback) {
  auto *baseDesc = base->getTypeContextDescriptor();
  if (!baseDesc)
    return;

  auto getFieldAt = [&](const FieldDescriptor &descriptor) {
    auto &field = descriptor.getFields()[index];
    auto name = field.getFieldName(0);

    // Enum cases don't always have types.
    if (!field.hasMangledTypeName()) {
      callback(name, FieldType().withIndirect(field.isIndirectCase()));
      return;
    }

    std::vector<const ContextDescriptor *> descriptorPath;
    {
      const auto *parent = reinterpret_cast<
                              const ContextDescriptor *>(baseDesc);
      while (parent) {
        if (parent->isGeneric())
          descriptorPath.push_back(parent);

        parent = parent->Parent.get();
      }
    }

    auto typeInfo = _getTypeByMangledName(
        field.getMangledTypeName(0),
        [&](unsigned depth, unsigned index) -> const Metadata * {
          if (depth >= descriptorPath.size())
            return nullptr;

          unsigned currentDepth = 0;
          unsigned flatIndex = index;
          const ContextDescriptor *currentContext = descriptorPath.back();

          for (const auto *context : llvm::reverse(descriptorPath)) {
            if (currentDepth >= depth)
              break;

            flatIndex += context->getNumGenericParams();
            currentContext = context;
            ++currentDepth;
          }

          if (index >= currentContext->getNumGenericParams())
            return nullptr;

          return base->getGenericArgs()[flatIndex];
        });

    callback(name, FieldType()
                       .withType(typeInfo)
                       .withIndirect(field.isIndirectCase())
                       .withWeak(typeInfo.isWeak()));

  };

  auto dem = getDemanglerForRuntimeTypeResolution();
  auto &cache = FieldCache.get();
  auto isRequestedDescriptor = [&](const FieldDescriptor &descriptor) {
    assert(descriptor.hasMangledTypeName());
    auto mangledName = descriptor.getMangledTypeName(0);

    if (!_contextDescriptorMatchesMangling(baseDesc,
                                           dem.demangleType(mangledName)))
      return false;

    cache.FieldCache.getOrInsert(base, &descriptor);
    getFieldAt(descriptor);
    return true;
  };


  // Fast path: If we already have field descriptor cached.
  if (auto Value = cache.FieldCache.find(base)) {
    getFieldAt(*Value->getDescription());
    return;
  }

  ScopedLock guard(cache.SectionsLock);
  // Otherwise let's try to find it in one of the sections.
  for (auto &section : cache.DynamicSections) {
    for (const auto *descriptor : section) {
      if (isRequestedDescriptor(*descriptor))
        return;
    }
  }

  for (const auto &section : cache.StaticSections) {
    for (auto &descriptor : section) {
      if (isRequestedDescriptor(descriptor))
        return;
    }
  }
}

#define OVERRIDE_METADATALOOKUP COMPATIBILITY_OVERRIDE
#include "CompatibilityOverride.def"
