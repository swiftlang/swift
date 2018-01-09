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
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/StringExtras.h"
#include "Private.h"
#include "ImageInspection.h"
#include <vector>

using namespace swift;
using namespace Demangle;

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif

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
    const NominalTypeDescriptor *Description;

  public:
    NominalTypeDescriptorCacheEntry(const llvm::StringRef name,
                           const NominalTypeDescriptor *description)
      : Name(name.str()), Description(description) {}

    const NominalTypeDescriptor *getDescription() {
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

// returns the nominal type descriptor for the type named by typeName
static const NominalTypeDescriptor *
_searchTypeMetadataRecords(const TypeMetadataState &T,
                           const llvm::StringRef typeName) {
  unsigned sectionIdx = 0;
  unsigned endSectionIdx = T.SectionsToScan.size();
  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = T.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto ntd = record.getNominalTypeDescriptor()) {
        if (ntd->Name.get() == typeName)
          return ntd;
      }
    }
  }

  return nullptr;
}

static const NominalTypeDescriptor *
_findNominalTypeDescriptor(llvm::StringRef mangledName) {
  const NominalTypeDescriptor *foundNominal = nullptr;
  auto &T = TypeMetadataRecords.get();

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.NominalCache.find(mangledName))
    return Value->getDescription();

  // Check type metadata records
  T.SectionsToScanLock.withLock([&] {
    foundNominal = _searchTypeMetadataRecords(T, mangledName);
  });

  // Check protocol conformances table. Note that this has no support for
  // resolving generic types yet.
  if (!foundNominal)
    foundNominal = _searchConformancesByMangledTypeName(mangledName);

  if (foundNominal) {
    T.NominalCache.getOrInsert(mangledName, foundNominal);
  }

  return foundNominal;
}

#pragma mark Metadata lookup via mangled name

namespace {
/// Constructs metadata by decoding a mangled type name, for use with
/// \c TypeDecoder.
class DecodedMetadataBuilder {
public:
  using BuiltType = const Metadata *;
  using BuiltNominalTypeDecl = const NominalTypeDescriptor *;
  using BuiltProtocolDecl = const ProtocolDescriptor *;

  BuiltNominalTypeDecl createNominalTypeDecl(
                                     const Demangle::NodePointer &node) const {
    // FIXME: Mangled Objective-C class names should go directly
    // through objc_lookupClass.

    // Look for a nominal type descriptor based on its mangled name.
    auto mangledName = Demangle::mangleNode(node);
    return _findNominalTypeDescriptor(mangledName);
  }

  BuiltProtocolDecl createProtocolDecl(
                                    const Demangle::NodePointer &node) const {
    // FIXME: Implement.

    return BuiltProtocolDecl();
  }

  BuiltType createNominalType(BuiltNominalTypeDecl typeDecl,
                              BuiltType parent) const {
    // FIXME: Generic nominal types.
    if (typeDecl->GenericParams.isGeneric())
      return BuiltType();

    // Use the access function to compute type metadata.
    if (auto accessFunction = typeDecl->getAccessFunction()) {
      return accessFunction();
    }

    // FIXME: Shouldn't get here? We've hit an unsupported case.
    return BuiltType();
  }

  BuiltType createBoundGenericType(BuiltNominalTypeDecl typeDecl,
                                   ArrayRef<BuiltType> genericArgs,
                                   BuiltType parent) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createBuiltinType(StringRef mangledName) const {
    // FIXME: Implement.
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
    auto classConstraint = isClassBound ? ProtocolClassConstraint::Class
                                        : ProtocolClassConstraint::Any;
    return swift_getExistentialTypeMetadata(classConstraint, superclass,
                                            protocols.size(), protocols.data());
  }

  BuiltType createGenericTypeParameterType(unsigned depth,
                                           unsigned index) const {
    // FIXME: Implement substitution logic here.
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
    TupleTypeFlags flags(0);
    if (!labels.empty())
      flags = flags.withNonConstantLabels(true);
    return swift_getTupleTypeMetadata(elements.size(), elements.data(),
                                      labels.empty() ? nullptr : labels.c_str(),
                                      flags, /*proposedWitnesses=*/nullptr);
  }

  BuiltType createDependentMemberType(StringRef name, BuiltType base,
                                      BuiltProtocolDecl protocol) const {
    // FIXME: Implement.
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
swift_getTypeByMangledName(const char *typeNameStart, size_t typeNameLength) {
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

  DecodedMetadataBuilder builder;
  return Demangle::decodeMangledType(builder, node);
}
