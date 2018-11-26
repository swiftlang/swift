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
#include "swift/ABI/TypeIdentity.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
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
  dem.setSymbolicReferenceResolver(ResolveAsSymbolicReference(dem));
  return dem;
}

NodePointer
ResolveAsSymbolicReference::operator()(SymbolicReferenceKind kind,
                                       Directness isIndirect,
                                       int32_t offset,
                                       const void *base) {
  // Resolve the absolute pointer to the entity being referenced.
  auto ptr = detail::applyRelativeOffset(base, offset);
  if (isIndirect == Directness::Indirect) {
    ptr = *(const uintptr_t *)ptr;
  }

  // Figure out this symbolic reference's grammatical role.
  Node::Kind nodeKind;
  bool isType;
  switch (kind) {
  case Demangle::SymbolicReferenceKind::Context: {
    auto descriptor = (const ContextDescriptor *)ptr;
    switch (descriptor->getKind()) {
    case ContextDescriptorKind::Protocol:
      nodeKind = Node::Kind::ProtocolSymbolicReference;
      isType = false;
      break;
      
    default:
      if (auto typeContext = dyn_cast<TypeContextDescriptor>(descriptor)) {
        nodeKind = Node::Kind::TypeSymbolicReference;
        isType = true;
        break;
      }
      
      // References to other kinds of context aren't yet implemented.
      return nullptr;
    }
    break;
  }
  }
  
  auto node = Dem.createNode(nodeKind, ptr);
  if (isType) {
    auto typeNode = Dem.createNode(Node::Kind::Type);
    typeNode->addChild(node, Dem);
    node = typeNode;
  }
  return node;
}

static NodePointer
_buildDemanglingForSymbolicReference(SymbolicReferenceKind kind,
                                     const void *resolvedReference,
                                     Demangler &Dem) {
  switch (kind) {
  case SymbolicReferenceKind::Context:
    return _buildDemanglingForContext(
      (const ContextDescriptor *)resolvedReference, {}, Dem);
  }
  
  swift_runtime_unreachable("invalid symbolic reference kind");
}
  
NodePointer
ResolveToDemanglingForContext::operator()(SymbolicReferenceKind kind,
                                          Directness isIndirect,
                                          int32_t offset,
                                          const void *base) {
  auto ptr = detail::applyRelativeOffset(base, offset);
  if (isIndirect == Directness::Indirect) {
    ptr = *(const uintptr_t *)ptr;
  }
  
  return _buildDemanglingForSymbolicReference(kind, (const void *)ptr, Dem);
}

NodePointer
ExpandResolvedSymbolicReferences::operator()(SymbolicReferenceKind kind,
                                             const void *ptr) {
  return _buildDemanglingForSymbolicReference(kind, (const void *)ptr, Dem);
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
  ConcurrentReadableArray<TypeMetadataSection> SectionsToScan;

  TypeMetadataPrivateState() {
    initializeTypeMetadataRecordLookup();
  }

};

static Lazy<TypeMetadataPrivateState> TypeMetadataRecords;

static void
_registerTypeMetadataRecords(TypeMetadataPrivateState &T,
                             const TypeMetadataRecord *begin,
                             const TypeMetadataRecord *end) {
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

static const TypeContextDescriptor *
_findNominalTypeDescriptor(Demangle::NodePointer node,
                           Demangle::Demangler &Dem);

/// Recognize imported tag types, which have a special mangling rule.
///
/// This should be kept in sync with the AST mangler and with
/// buildContextDescriptorMangling in MetadataReader.
bool swift::_isCImportedTagType(const TypeContextDescriptor *type,
                                const ParsedTypeIdentity &identity) {
  // Tag types are always imported as structs or enums.
  if (type->getKind() != ContextDescriptorKind::Enum &&
      type->getKind() != ContextDescriptorKind::Struct)
    return false;

  // Not a typedef imported as a nominal type.
  if (identity.isCTypedef())
    return false;

  // Not a related entity.
  if (identity.isAnyRelatedEntity())
    return false;

  // Imported from C.
  return type->Parent->isCImportedContext();
}

ParsedTypeIdentity
ParsedTypeIdentity::parse(const TypeContextDescriptor *type) {
  ParsedTypeIdentity result;

  // The first component is the user-facing name and (unless overridden)
  // the ABI name.
  StringRef component = type->Name.get();
  result.UserFacingName = component;

  // If we don't have import info, we're done.
  if (!type->getTypeContextDescriptorFlags().hasImportInfo()) {
    result.FullIdentity = result.UserFacingName;
    return result;
  }

  // Otherwise, start parsing the import information.
  result.ImportInfo.emplace();

  // The identity starts with the user-facing name.
  const char *startOfIdentity = component.begin();
  const char *endOfIdentity = component.end();

#ifndef NDEBUG
  enum {
    AfterName,
    AfterABIName,
    AfterSymbolNamespace,
    AfterRelatedEntityName,
    AfterIdentity,
  } stage = AfterName;
#endif

  while (true) {
    // Parse the next component.  If it's empty, we're done.
    component = StringRef(component.end() + 1);
    if (component.empty()) break;

    // Update the identity bounds and assert that the identity
    // components are in the right order.
    auto kind = TypeImportComponent(component[0]);
    if (kind == TypeImportComponent::ABIName) {
#ifndef NDEBUG
      assert(stage < AfterABIName);
      stage = AfterABIName;
      assert(result.UserFacingName != component.drop_front(1) &&
             "user-facing name was same as the ABI name");
#endif
      startOfIdentity = component.begin() + 1;
      endOfIdentity = component.end();
    } else if (kind == TypeImportComponent::SymbolNamespace) {
#ifndef NDEBUG
      assert(stage < AfterSymbolNamespace);
      stage = AfterSymbolNamespace;
#endif
      endOfIdentity = component.end();
    } else if (kind == TypeImportComponent::RelatedEntityName) {
#ifndef NDEBUG
      assert(stage < AfterRelatedEntityName);
      stage = AfterRelatedEntityName;
#endif
      endOfIdentity = component.end();
    } else {
#ifndef NDEBUG
      // Anything else is assumed to not be part of the identity.
      stage = AfterIdentity;
#endif
    }

    // Collect the component, whatever it is.
    result.ImportInfo->collect</*asserting*/true>(component);
  }

  assert(stage != AfterName && "no components?");

  // Record the full identity.
  result.FullIdentity =
    StringRef(startOfIdentity, endOfIdentity - startOfIdentity);

  return result;
}

#if SWIFT_OBJC_INTEROP
/// Determine whether the two demangle trees both refer to the same
/// Objective-C class or protocol referenced by name.
static bool sameObjCTypeManglings(Demangle::NodePointer node1,
                                  Demangle::NodePointer node2) {
  // Entities need to be of the same kind.
  if (node1->getKind() != node2->getKind())
    return false;

  auto name1 = Demangle::getObjCClassOrProtocolName(node1);
  if (!name1) return false;

  auto name2 = Demangle::getObjCClassOrProtocolName(node2);
  if (!name2) return false;

  return *name1 == *name2;
}
#endif

bool
swift::_contextDescriptorMatchesMangling(const ContextDescriptor *context,
                                         Demangle::NodePointer node) {
  while (context) {
    if (node->getKind() == Demangle::Node::Kind::Type)
      node = node->getChild(0);
    
    // We can directly match symbolic references to the current context.
    if (node) {
      if (node->getKind() == Demangle::Node::Kind::TypeSymbolicReference
         || node->getKind() == Demangle::Node::Kind::ProtocolSymbolicReference){
        if (equalContexts(context,
               reinterpret_cast<const ContextDescriptor *>(node->getIndex()))) {
          return true;
        }
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
      auto extension = cast<ExtensionContextDescriptor>(context);
      
      // Check whether the extension context matches the mangled context.
      if (node->getKind() != Demangle::Node::Kind::Extension)
        return false;
      if (node->getNumChildren() < 2)
        return false;
      
      // Check that the context being extended matches as well.
      auto extendedContextNode = node->getChild(1);
      auto extendedContextMangledName = extension->getMangledExtendedContext();
      auto demangler = getDemanglerForRuntimeTypeResolution();
      auto extendedContextDemangled =
         demangler.demangleType(extendedContextMangledName);
      if (!extendedContextDemangled)
        return false;
      if (extendedContextDemangled->getKind() == Node::Kind::Type) {
        if (extendedContextDemangled->getNumChildren() < 1)
          return false;
        extendedContextDemangled = extendedContextDemangled->getChild(0);
      }
      extendedContextDemangled =
        stripGenericArgsFromContextNode(extendedContextDemangled, demangler);
      
      auto extendedDescriptorFromNode =
        _findNominalTypeDescriptor(extendedContextNode, demangler);
      auto extendedDescriptorFromDemangled =
        _findNominalTypeDescriptor(extendedContextDemangled, demangler);

      // Determine whether the contexts match.
      bool contextsMatch =
        extendedDescriptorFromNode && extendedDescriptorFromDemangled &&
        equalContexts(extendedDescriptorFromNode,
                      extendedDescriptorFromDemangled);
      
#if SWIFT_OBJC_INTEROP
      if (!contextsMatch &&
          (!extendedDescriptorFromNode || !extendedDescriptorFromDemangled) &&
          sameObjCTypeManglings(extendedContextNode,
                                extendedContextDemangled)) {
        contextsMatch = true;
      }
#endif

      if (!contextsMatch)
        return false;
      
      // Check whether the generic signature of the extension matches the
      // mangled constraints, if any.

      if (node->getNumChildren() >= 3) {
        // NB: If we ever support extensions with independent generic arguments
        // like `extension <T> Array where Element == Optional<T>`, we'd need
        // to look at the mangled context name to match up generic arguments.
        // That would probably need a new extension mangling form, though.
        
        // TODO
      }
      
      // The parent context of the extension should match in the mangling and
      // context descriptor.
      node = node->getChild(0);
      break;
    }

    case ContextDescriptorKind::Protocol:
      // Match a protocol context.
      if (node->getKind() == Demangle::Node::Kind::Protocol) {
        auto proto = llvm::cast<ProtocolDescriptor>(context);
        auto nameNode = node->getChild(1);
        if (nameNode->getKind() != Demangle::Node::Kind::Identifier)
          return false;
        if (nameNode->getText() == proto->Name.get()) {
          node = node->getChild(0);
          break;
        }
      }
      return false;

    default:
      if (auto type = llvm::dyn_cast<TypeContextDescriptor>(context)) {
        Optional<ParsedTypeIdentity> _identity;
        auto getIdentity = [&]() -> const ParsedTypeIdentity & {
          if (_identity) return *_identity;
          _identity = ParsedTypeIdentity::parse(type);
          return *_identity;
        };

        switch (node->getKind()) {
        // If the mangled name doesn't indicate a type kind, accept anything.
        // Otherwise, try to match them up.
        case Demangle::Node::Kind::OtherNominalType:
          break;
        case Demangle::Node::Kind::Structure:
          // We allow non-structs to match Kind::Structure if they are
          // imported C tag types.  This is necessary because we artificially
          // make imported C tag types Kind::Structure.
          if (type->getKind() != ContextDescriptorKind::Struct &&
              !_isCImportedTagType(type, getIdentity()))
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
          if (!getIdentity().isCTypedef())
            return false;
          break;

        default:
          return false;
        }

        auto nameNode = node->getChild(1);
        
        // Declarations synthesized by the Clang importer get a small tag
        // string in addition to their name.
        if (nameNode->getKind() == Demangle::Node::Kind::RelatedEntityDeclName){          
          if (!getIdentity().isRelatedEntity(nameNode->getText()))
            return false;
          
          nameNode = nameNode->getChild(0);
        } else if (getIdentity().isAnyRelatedEntity()) {
          return false;
        }
        
        // We should only match public or internal declarations with stable
        // names. The runtime metadata for private declarations would be
        // anonymized.
        if (nameNode->getKind() == Demangle::Node::Kind::Identifier) {
          if (nameNode->getText() != getIdentity().getABIName())
            return false;
          
          node = node->getChild(0);
          break;
        }
        
        return false;

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
_searchTypeMetadataRecords(TypeMetadataPrivateState &T,
                           Demangle::NodePointer node) {
  for (auto &section : T.SectionsToScan.snapshot()) {
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
  if (symbolicNode->getKind() == Node::Kind::TypeSymbolicReference)
    return cast<TypeContextDescriptor>(
      (const ContextDescriptor *)symbolicNode->getIndex());

  auto mangledName =
    Demangle::mangleNode(node, ExpandResolvedSymbolicReferences(Dem));

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.NominalCache.find(mangledName))
    return Value->getDescription();

  // Check type metadata records
  foundNominal = _searchTypeMetadataRecords(T, node);

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
    ConcurrentReadableArray<ProtocolSection> SectionsToScan;

    ProtocolMetadataPrivateState() {
      initializeProtocolLookup();
    }
  };

  static Lazy<ProtocolMetadataPrivateState> Protocols;
}

static void
_registerProtocols(ProtocolMetadataPrivateState &C,
                   const ProtocolRecord *begin,
                   const ProtocolRecord *end) {
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
_searchProtocolRecords(ProtocolMetadataPrivateState &C,
                       const Demangle::NodePointer &node) {
  for (auto &section : C.SectionsToScan.snapshot()) {
    for (const auto &record : section) {
      if (auto protocol = record.Protocol.getPointer()) {
        if (_contextDescriptorMatchesMangling(protocol, node))
          return protocol;
      }
    }
  }

  return nullptr;
}

static const ProtocolDescriptor *
_findProtocolDescriptor(const Demangle::NodePointer &node,
                        Demangle::Demangler &Dem,
                        std::string &mangledName) {
  const ProtocolDescriptor *foundProtocol = nullptr;
  auto &T = Protocols.get();

  // If we have a symbolic reference to a context, resolve it immediately.
  NodePointer symbolicNode = node;
  if (symbolicNode->getKind() == Node::Kind::Type)
    symbolicNode = symbolicNode->getChild(0);
  if (symbolicNode->getKind() == Node::Kind::ProtocolSymbolicReference)
    return cast<ProtocolDescriptor>(
      (const ContextDescriptor *)symbolicNode->getIndex());

  mangledName =
    Demangle::mangleNode(node, ExpandResolvedSymbolicReferences(Dem));

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.ProtocolCache.find(mangledName))
    return Value->getDescription();

  // Check type metadata records
  foundProtocol = _searchProtocolRecords(T, node);

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

  const FieldDescriptor **begin() const { return Begin; }

  const FieldDescriptor **end() const { return End; }
};

} // namespace

#pragma mark Metadata lookup via mangled name

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
Optional<const ProtocolRequirement *> findAssociatedTypeByName(
                                        const ProtocolDescriptor *protocol,
                                        StringRef name) {
  // If we don't have associated type names, there's nothing to do.
  const char *associatedTypeNamesPtr = protocol->AssociatedTypeNames.get();
  if (!associatedTypeNamesPtr) return None;

  // Look through the list of associated type names.
  StringRef associatedTypeNames(associatedTypeNamesPtr);
  unsigned matchingAssocTypeIdx = 0;
  bool found = false;
  while (!associatedTypeNames.empty()) {
    // Avoid using StringRef::split because its definition is not
    // provided in the header so that it requires linking with libSupport.a.
    auto splitIdx = associatedTypeNames.find(' ');
    if (associatedTypeNames.substr(0, splitIdx) == name) {
      found = true;
      break;
    }

    ++matchingAssocTypeIdx;
    associatedTypeNames = associatedTypeNames.substr(splitIdx).substr(1);
  }

  if (!found) return None;

  // We have a match on the Nth associated type; go find the Nth associated
  // type requirement.
  unsigned currentAssocTypeIdx = 0;
  unsigned numRequirements = protocol->NumRequirements;
  auto requirements = protocol->getRequirements();
  for (unsigned reqIdx = 0; reqIdx != numRequirements; ++reqIdx) {
    if (requirements[reqIdx].Flags.getKind() !=
        ProtocolRequirementFlags::Kind::AssociatedTypeAccessFunction)
      continue;

    if (currentAssocTypeIdx == matchingAssocTypeIdx)
      return requirements.begin() + reqIdx;

    ++currentAssocTypeIdx;
  }

  swift_runtime_unreachable("associated type names don't line up");
}

/// Retrieve the generic parameters introduced in this context.
static ArrayRef<GenericParamDescriptor> getLocalGenericParams(
    const ContextDescriptor *context) {
  if (!context->isGeneric())
    return { };

  // Determine where to start looking at generic parameters.
  unsigned startParamIndex;
  if (auto parent = context->Parent.get())
    startParamIndex = parent->getNumGenericParams();
  else
    startParamIndex = 0;

  auto genericContext = context->getGenericContext();
  return genericContext->getGenericParams().slice(startParamIndex);
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
  using BuiltNominalTypeDecl = const TypeContextDescriptor *;
  using BuiltProtocolDecl = ProtocolDescriptorRef;

  Demangle::NodeFactory &getNodeFactory() { return demangler; }

  BuiltNominalTypeDecl createNominalTypeDecl(
                                     const Demangle::NodePointer &node) const {
    // Look for a nominal type descriptor based on its mangled name.
    return _findNominalTypeDescriptor(node, demangler);
  }

  BuiltProtocolDecl createProtocolDecl(
                                    const Demangle::NodePointer &node) const {
    // Look for a protocol descriptor based on its mangled name.
    std::string mangledName;
    if (auto protocol = _findProtocolDescriptor(node, demangler, mangledName))
      return ProtocolDescriptorRef::forSwift(protocol);;

#if SWIFT_OBJC_INTEROP
    // Look for a Swift-defined @objc protocol with the Swift 3 mangling that
    // is used for Objective-C entities.
    std::string objcMangledName =
      "_TtP" + mangledName.substr(0, mangledName.size()-1) + "_";
    if (auto protocol = objc_getProtocol(objcMangledName.c_str()))
      return ProtocolDescriptorRef::forObjC(protocol);
#endif

    return ProtocolDescriptorRef();
  }

  BuiltProtocolDecl createObjCProtocolDecl(
                                         const std::string &mangledName) const {
#if SWIFT_OBJC_INTEROP
    return ProtocolDescriptorRef::forObjC(
        objc_getProtocol(mangledName.c_str()));
#else
    return ProtocolDescriptorRef();
#endif
  }
  
  BuiltType createObjCClassType(const std::string &mangledName) const {
#if SWIFT_OBJC_INTEROP
    auto objcClass = objc_getClass(mangledName.c_str());
    return swift_getObjCClassMetadata((const ClassMetadata *)objcClass);
#else
    return BuiltType();
#endif
  }

  BuiltType createNominalType(BuiltNominalTypeDecl metadataOrTypeDecl,
                              BuiltType parent) const {
    // Treat nominal type creation the same way as generic type creation,
    // but with no generic arguments at this level.
    return createBoundGenericType(metadataOrTypeDecl, { }, parent);
  }

  BuiltType createBoundGenericType(BuiltNominalTypeDecl typeDecl,
                                   const ArrayRef<BuiltType> genericArgs,
                                   const BuiltType parent) const {
    // Figure out the various levels of generic parameters we have in
    // this type.
    std::vector<unsigned> genericParamCounts;
    (void)_gatherGenericParameterCounts(typeDecl, genericParamCounts);
    unsigned numTotalGenericParams =
        genericParamCounts.empty() ? 0 : genericParamCounts.back();

    // Check whether we have the right number of generic arguments.
    if (genericArgs.size() == getLocalGenericParams(typeDecl).size()) {
      // Okay: genericArgs is the innermost set of generic arguments.
    } else if (genericArgs.size() == numTotalGenericParams && !parent) {
      // Okay: genericArgs is the complete set of generic arguments.
    } else {
      return BuiltType();
    }

    std::vector<const void *> allGenericArgsVec;

    // If there are generic parameters at any level, check the generic
    // requirements and fill in the generic arguments vector.
    if (!genericParamCounts.empty()) {
      // Compute the set of generic arguments "as written".
      std::vector<const Metadata *> allGenericArgs;

      // If we have a parent, gather it's generic arguments "as written".
      if (parent) {
        gatherWrittenGenericArgs(parent, parent->getTypeContextDescriptor(),
                                 allGenericArgs);
      }

      // Add the generic arguments we were given.
      allGenericArgs.insert(allGenericArgs.end(),
                            genericArgs.begin(), genericArgs.end());

      // Copy the generic arguments needed for metadata from the generic
      // arguments "as written".
      auto genericContext = typeDecl->getGenericContext();
      {
        auto genericParams = genericContext->getGenericParams();
        for (unsigned i = 0, n = genericParams.size(); i != n; ++i) {
          const auto &param = genericParams[i];
          if (param.getKind() != GenericParamKind::Type)
            return BuiltType();
          if (param.hasExtraArgument())
            return BuiltType();

          if (param.hasKeyArgument())
            allGenericArgsVec.push_back(allGenericArgs[i]);
        }
      }

      // If we have the wrong number of generic arguments, fail.

      // Check whether the generic requirements are satisfied, collecting
      // any extra arguments we need for the instantiation function.
      SubstGenericParametersFromWrittenArgs substitutions(allGenericArgs,
                                                          genericParamCounts);
      bool failed =
        _checkGenericRequirements(genericContext->getGenericRequirements(),
                                  allGenericArgsVec, substitutions,
                                  substitutions);
      if (failed)
        return BuiltType();

      // If we still have the wrong number of generic arguments, this is
      // some kind of metadata mismatch.
      if (typeDecl->getGenericContextHeader().getNumArguments() !=
            allGenericArgsVec.size())
        return BuiltType();
    }

    // Call the access function.
    auto accessFunction = typeDecl->getAccessFunction();
    if (!accessFunction) return BuiltType();

    return accessFunction(MetadataState::Abstract, allGenericArgsVec).Value;
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
        if (protocol.getClassConstraint() == ProtocolClassConstraint::Class) {
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
    return swift_getTupleTypeMetadata(MetadataState::Abstract,
                                      flags, elements.data(),
                                      labels.empty() ? nullptr : labels.c_str(),
                                      /*proposedWitnesses=*/nullptr).Value;
  }

  BuiltType createDependentMemberType(StringRef name, BuiltType base,
                                      BuiltProtocolDecl protocol) const {
#if SWIFT_OBJC_INTEROP
    if (protocol.isObjC())
      return BuiltType();
#endif

    if (lookupDependentMember)
      return lookupDependentMember(base, name, protocol.getSwiftProtocol());

    return BuiltType();
  }

#define REF_STORAGE(Name, ...) \
  BuiltType create##Name##StorageType(BuiltType base) { \
    ReferenceOwnership.set##Name(); \
    return base; \
  }
#include "swift/AST/ReferenceStorage.def"

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
    size_t dotPos = llvm::StringRef::npos;
    for (unsigned i = 0; i < typeName.size(); ++i) {
      // Should only contain one dot.
      if (typeName[i] == '.') {
        if (dotPos == llvm::StringRef::npos) {
          dotPos = i;
          continue;
        } else {
          return llvm::StringRef::npos;
        }
      }
      
      // Should not contain symbolic references.
      if ((unsigned char)typeName[i] <= '\x1F') {
        return llvm::StringRef::npos;
      }
    }
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
      auto assocTypeReq = findAssociatedTypeByName(protocol, assocType);
      if (!assocTypeReq) return nullptr;

      // Call the associated type access function.
      return swift_getAssociatedTypeWitness(
                                     MetadataState::Abstract,
                                     const_cast<WitnessTable *>(witnessTable),
                                     base,
                                     protocol->getRequirementBaseDescriptor(),
                                     *assocTypeReq).Value;
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
  auto metadata = _getTypeByMangledName(typeName,
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

  if (!metadata) return nullptr;

  return swift_checkMetadataState(MetadataState::Complete, metadata).Value;
}

unsigned SubstGenericParametersFromMetadata::
buildDescriptorPath(const ContextDescriptor *context) const {
  // Terminating condition: we don't have a context.
  if (!context)
    return 0;

  // Add the parent's contributino to the descriptor path.
  unsigned numKeyGenericParamsInParent =
    buildDescriptorPath(context->Parent.get());

  // If this context is non-generic, we're done.
  if (!context->isGeneric())
    return numKeyGenericParamsInParent;

  // Count the number of key generic params at this level.
  unsigned numKeyGenericParamsHere = 0;
  bool hasNonKeyGenericParams = false;
  for (const auto &genericParam : getLocalGenericParams(context)) {
    if (genericParam.hasKeyArgument())
      ++numKeyGenericParamsHere;
    else
      hasNonKeyGenericParams = true;
  }

  // Form the path element.
  descriptorPath.push_back(PathElement{context, numKeyGenericParamsInParent,
                                       numKeyGenericParamsHere,
                                       hasNonKeyGenericParams});
  return numKeyGenericParamsInParent + numKeyGenericParamsHere;
}

void SubstGenericParametersFromMetadata::setup() const {
  if (!descriptorPath.empty() || !base)
    return;

  buildDescriptorPath(base->getTypeContextDescriptor());
}

const Metadata *
SubstGenericParametersFromMetadata::operator()(unsigned flatIndex) const {
  // On first access, compute the descriptor path.
  setup();

  // Find the depth at which this parameter occurs.
  unsigned depth = descriptorPath.size();
  unsigned index = flatIndex;
  for (const auto &pathElement : descriptorPath) {
    // If the flat index is beyond the element at this position, we're done.
    if (flatIndex >= pathElement.context->getNumGenericParams()) {
      // Subtract off the number of parameters.
      index -= pathElement.context->getNumGenericParams();
      break;
    }

    --depth;
  }

  // Perform the access based on depth/index.
  return (*this)(depth, index);
}

const Metadata *
SubstGenericParametersFromMetadata::operator()(
                                        unsigned depth, unsigned index) const {
  // On first access, compute the descriptor path.
  setup();

  // If the depth is too great, there is nothing to do.
  if (depth >= descriptorPath.size())
    return nullptr;

  /// Retrieve the descriptor path element at this depth.
  auto &pathElement = descriptorPath[depth];
  auto currentContext = pathElement.context;

  // Check whether the index is clearly out of bounds.
  if (index >= currentContext->getNumGenericParams())
    return nullptr;

  // Compute the flat index.
  unsigned flatIndex = pathElement.numKeyGenericParamsInParent;
  if (pathElement.hasNonKeyGenericParams > 0) {
    // We have non-key generic parameters at this level, so the index needs to
    // be checked more carefully.
    auto genericParams = getLocalGenericParams(currentContext);

    // Make sure that the requested parameter itself has a key argument.
    if (!genericParams[index].hasKeyArgument())
      return nullptr;

    // Increase the flat index for each parameter with a key argument, up to
    // the given index.
    for (const auto &genericParam : genericParams.slice(0, index)) {
      if (genericParam.hasKeyArgument())
        ++flatIndex;
    }
  } else {
    flatIndex += index;
  }

  return base->getGenericArgs()[flatIndex];
}

const Metadata *SubstGenericParametersFromWrittenArgs::operator()(
                                                    unsigned flatIndex) const {
  if (flatIndex < allGenericArgs.size())
    return allGenericArgs[flatIndex];

  return nullptr;
}

const Metadata *SubstGenericParametersFromWrittenArgs::operator()(
                                                        unsigned depth,
                                                        unsigned index) const {
  if (auto flatIndex =
          _depthIndexToFlatIndex(depth, index, genericParamCounts)) {
    if (*flatIndex < allGenericArgs.size())
      return allGenericArgs[*flatIndex];
  }

  return nullptr;
}

void swift::gatherWrittenGenericArgs(
                             const Metadata *metadata,
                             const TypeContextDescriptor *description,
                             std::vector<const Metadata *> &allGenericArgs) {
  auto generics = description->getGenericContext();
  if (!generics)
    return;

  bool missingWrittenArguments = false;
  auto genericArgs = description->getGenericArguments(metadata);
  for (auto param : generics->getGenericParams()) {
    switch (param.getKind()) {
    case GenericParamKind::Type:
      // The type should have a key argument unless it's been same-typed to
      // another type.
      if (param.hasKeyArgument()) {
        auto genericArg = *genericArgs++;
        allGenericArgs.push_back(genericArg);
      } else {
        // Leave a gap for us to fill in by looking at same type info.
        allGenericArgs.push_back(nullptr);
        missingWrittenArguments = true;
      }

      // We don't know about type parameters with extra arguments. Leave
      // a hole for it.
      if (param.hasExtraArgument()) {
        allGenericArgs.push_back(nullptr);
        ++genericArgs;
      }
      break;

    default:
      // We don't know about this kind of parameter. Create placeholders where
      // needed.
      if (param.hasKeyArgument()) {
        allGenericArgs.push_back(nullptr);
        ++genericArgs;
      }

      if (param.hasExtraArgument()) {
        allGenericArgs.push_back(nullptr);
        ++genericArgs;
      }
      break;
    }
  }

  // If there is no follow-up work to do, we're done.
  if (!missingWrittenArguments)
    return;

  // We have generic arguments that would be written, but have been
  // canonicalized away. Use same-type requirements to reconstitute them.

  // Retrieve the mapping information needed for depth/index -> flat index.
  std::vector<unsigned> genericParamCounts;
  (void)_gatherGenericParameterCounts(description, genericParamCounts);

  // Walk through the generic requirements to evaluate same-type
  // constraints that are needed to fill in missing generic arguments.
  for (const auto &req : generics->getGenericRequirements()) {
    // We only care about same-type constraints.
    if (req.Flags.getKind() != GenericRequirementKind::SameType)
      continue;

    // Where the left-hand side is a generic parameter.
    if (req.Param.begin() != req.Param.end())
      continue;

    // If we don't yet have an argument for this parameter, it's a
    // same-type-to-concrete constraint.
    unsigned lhsFlatIndex = req.Param.getRootParamIndex();
    if (lhsFlatIndex >= allGenericArgs.size())
      continue;

    if (!allGenericArgs[lhsFlatIndex]) {
      // Substitute into the right-hand side.
      SubstGenericParametersFromWrittenArgs substitutions(allGenericArgs,
                                                          genericParamCounts);
      allGenericArgs[lhsFlatIndex] =
          _getTypeByMangledName(req.getMangledTypeName(), substitutions);
      continue;
    }

    // If we do have an argument for this parameter, it might be that
    // the right-hand side is itself a generic parameter, which means
    // we have a same-type constraint A == B where A is already filled in.
    Demangler demangler;
    NodePointer node = demangler.demangleType(req.getMangledTypeName());
    if (!node)
      continue;

    // Find the flat index that the right-hand side refers to.
    if (node->getKind() == Demangle::Node::Kind::Type)
      node = node->getChild(0);
    if (node->getKind() != Demangle::Node::Kind::DependentGenericParamType)
      continue;

    auto rhsFlatIndex =
      _depthIndexToFlatIndex(node->getChild(0)->getIndex(),
                             node->getChild(1)->getIndex(),
                             genericParamCounts);
    if (!rhsFlatIndex || *rhsFlatIndex >= allGenericArgs.size())
      continue;

    if (allGenericArgs[*rhsFlatIndex] || !allGenericArgs[lhsFlatIndex])
      continue;

    allGenericArgs[*rhsFlatIndex] = allGenericArgs[lhsFlatIndex];
  }
}

#define OVERRIDE_METADATALOOKUP COMPATIBILITY_OVERRIDE
#include "CompatibilityOverride.def"
