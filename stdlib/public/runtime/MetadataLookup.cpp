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
#include "ImageInspection.h"
#include <functional>
#include <vector>

using namespace swift;
using namespace Demangle;

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif

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

struct TypeMetadataState {
  ConcurrentMap<NominalTypeDescriptorCacheEntry> NominalCache;
  std::vector<TypeMetadataSection> SectionsToScan;
  Mutex SectionsToScanLock;

  TypeMetadataState() {
    SectionsToScan.reserve(16);
    initializeTypeMetadataRecordLookup();
  }

};

static Lazy<TypeMetadataState> TypeMetadataRecords;

static void
_registerTypeMetadataRecords(TypeMetadataState &T,
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
        auto flags = type->Flags.getKindSpecificFlags();
        switch (node->getKind()) {
        // If the mangled name doesn't indicate a type kind, accept anything.
        // Otherwise, try to match them up.
        case Demangle::Node::Kind::OtherNominalType:
          break;
        case Demangle::Node::Kind::Structure:
          if (type->getKind() != ContextDescriptorKind::Struct
              && !(flags & (uint16_t)TypeContextDescriptorFlags::IsCTag))
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
          if (!(flags & (uint16_t)TypeContextDescriptorFlags::IsCTypedef))
            return false;
          break;
        default:
          return false;
        }
        if (!node->getChild(1)->getText().equals(type->Name.get()))
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
_searchTypeMetadataRecords(const TypeMetadataState &T,
                           Demangle::NodePointer node) {
  unsigned sectionIdx = 0;
  unsigned endSectionIdx = T.SectionsToScan.size();
  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = T.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto ntd = record.getNominalTypeDescriptor()) {
        if (_contextDescriptorMatchesMangling(ntd, node)) {
          return ntd;
        }
      }
    }
  }

  return nullptr;
}

static const TypeContextDescriptor *
_findNominalTypeDescriptor(Demangle::NodePointer node) {
  const TypeContextDescriptor *foundNominal = nullptr;
  auto &T = TypeMetadataRecords.get();

  auto mangledName = Demangle::mangleNode(node);

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

    const ProtocolDescriptor *getDescription() {
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

  struct ProtocolMetadataState {
    ConcurrentMap<ProtocolDescriptorCacheEntry> ProtocolCache;
    std::vector<ProtocolSection> SectionsToScan;
    Mutex SectionsToScanLock;

    ProtocolMetadataState() {
      SectionsToScan.reserve(16);
      initializeProtocolLookup();
    }
  };

  static Lazy<ProtocolMetadataState> Protocols;
}

static void
_registerProtocols(ProtocolMetadataState &C,
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
_searchProtocolRecords(const ProtocolMetadataState &C,
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
    return _findNominalTypeDescriptor(node);
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
                                   ArrayRef<BuiltType> genericArgs,
                                   BuiltType parent) const {
    // If we already have metadata, return it.
    if (auto metadata = metadataOrTypeDecl.dyn_cast<const Metadata *>())
      return metadata;

    // Cannot specialize metadata.
    if (metadataOrTypeDecl.is<const Metadata *>())
      return BuiltType();

    auto typeDecl = metadataOrTypeDecl.get<const TypeContextDescriptor *>();

    // Gather all of the generic arguments.
    // FIXME: Need to also gather generic requirements.
    std::vector<BuiltType> allGenericArgsVec;
    ArrayRef<BuiltType> allGenericArgs;
    if (typeDecl->Parent->isGeneric()) {
      // TODO: The parent's generic arguments may not be a prefix of ours
      // if there are same type or protocol requirements.
      
      auto parentNominal = parent->getTypeContextDescriptor();
      if (!parentNominal) return BuiltType();

      auto parentGenericArgs = parent->getGenericArgs();

      // TODO: Handle generic arguments with protocol requirements or
      // same type constraints.
      if (parentGenericArgs
          && parentNominal->getGenericContextHeader().getNumArguments() !=
               parentNominal->getGenericContextHeader().NumParams)
        return nullptr;
      
      if (parentGenericArgs) {
        allGenericArgsVec.insert(
         allGenericArgsVec.end(),
         parentGenericArgs,
         parentGenericArgs + parentNominal->getGenericContextHeader().NumParams);
      }

      // Add the generic arguments for this type.
      allGenericArgsVec.insert(allGenericArgsVec.end(),
                               genericArgs.begin(), genericArgs.end());
      allGenericArgs = allGenericArgsVec;
    } else {
      // Only one level of generic arguments to consider.
      allGenericArgs = genericArgs;
    }

    // FIXME: We don't want the number of "primary" parameters, we want the
    // total number of parameters.
    if (typeDecl->isGeneric()
        && typeDecl->getGenericContextHeader().NumParams
             != typeDecl->getGenericContextHeader().getNumArguments())
      return BuiltType();

    // Call the access function.
    auto accessFunction = typeDecl->AccessFunction.get();
    if (!accessFunction) return BuiltType();

    static_assert(NumDirectGenericTypeMetadataAccessFunctionArgs == 3,
                  "Need to account for change in number of direct arguments");
    switch (allGenericArgs.size()) {
    case 0:
      return accessFunction();

    case 1:
      using GenericMetadataAccessFunction1 = const Metadata *(const void *);
      return ((GenericMetadataAccessFunction1 *)accessFunction)(
                                                          allGenericArgs[0]);

    case 2:
      using GenericMetadataAccessFunction2 =
        const Metadata *(const void *, const void *);
      return ((GenericMetadataAccessFunction2 *)accessFunction)(
                                                          allGenericArgs[0],
                                                          allGenericArgs[1]);

    case 3:
      using GenericMetadataAccessFunction3 =
        const Metadata *(const void *, const void *, const void *);
      return ((GenericMetadataAccessFunction3 *)accessFunction)(
                                                          allGenericArgs[0],
                                                          allGenericArgs[1],
                                                          allGenericArgs[2]);
    default:
      using GenericMetadataAccessFunction4 =
        const Metadata *(const void *, const void *, const void *,
                         const void *);
      return ((GenericMetadataAccessFunction4 *)accessFunction)(
                                                      allGenericArgs[0],
                                                      allGenericArgs[1],
                                                      allGenericArgs[2],
                                                      allGenericArgs.data());
    }
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
    return swift_getTupleTypeMetadata(flags, elements.data(),
                                      labels.empty() ? nullptr : labels.c_str(),
                                      /*proposedWitnesses=*/nullptr);
  }

  BuiltType createDependentMemberType(StringRef name, BuiltType base,
                                      BuiltProtocolDecl protocol) const {
    if (lookupDependentMember)
      return lookupDependentMember(base, name, protocol);

    return BuiltType();
  }

  BuiltType createUnownedStorageType(BuiltType base) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createUnmanagedStorageType(BuiltType base) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createWeakStorageType(BuiltType base) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createSILBoxType(BuiltType base) const {
    // FIXME: Implement.
    return BuiltType();
  }
};

}

SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
const Metadata * _Nullable
swift_getTypeByMangledName(const char *typeNameStart, size_t typeNameLength,
                           size_t numberOfLevels,
                           size_t *parametersPerLevel,
                           const Metadata * const *flatSubstitutions) {
  llvm::StringRef typeName(typeNameStart, typeNameLength);

  Demangler demangler;
  NodePointer node;

  // Check whether this is the convenience syntax "ModuleName.ClassName".
  size_t dotPos = typeName.find('.');
  if (dotPos != llvm::StringRef::npos &&
      typeName.find('.', dotPos + 1) == llvm::StringRef::npos) {
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
    if (!node) return nullptr;
  }

  DecodedMetadataBuilder builder(demangler,
    [&](unsigned depth, unsigned index) -> const Metadata * {
      if (depth >= numberOfLevels)
        return nullptr;

      if (index >= parametersPerLevel[depth])
        return nullptr;

      unsigned flatIndex = index;
      for (unsigned i = 0; i < depth; ++i)
        flatIndex += parametersPerLevel[i];

      return flatSubstitutions[flatIndex];
    },
    [](const Metadata *base, StringRef assocType,
       const ProtocolDescriptor *protocol) -> const Metadata * {
      // Look for a conformance of the base type to the protocol.
      auto witnessTable = swift_conformsToProtocol(base, protocol);
      if (!witnessTable) return nullptr;

      // Look for the named associated type within the protocol.
      auto assocTypeReqIndex = findAssociatedTypeByName(protocol, assocType);
      if (!assocTypeReqIndex) return nullptr;

      // Call the associated type access function.
      using AssociatedTypeAccessFn =
        const Metadata *(*)(const Metadata *base, const WitnessTable *);
      return ((const AssociatedTypeAccessFn *)witnessTable)[*assocTypeReqIndex]
                (base, witnessTable);
    });

  return Demangle::decodeMangledType(builder, node);
}
