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

  struct TypeMetadataCacheEntry {
  private:
    std::string Name;
    const Metadata *Metadata;

  public:
    TypeMetadataCacheEntry(const llvm::StringRef name,
                           const ::Metadata *metadata)
      : Name(name.str()), Metadata(metadata) {}

    const ::Metadata *getMetadata(void) {
      return Metadata;
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
  ConcurrentMap<TypeMetadataCacheEntry> Cache;
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

// returns the type metadata for the type named by typeNode
const Metadata *
swift::_matchMetadataByMangledTypeName(const llvm::StringRef typeName,
                                       const Metadata *metadata,
                                       const NominalTypeDescriptor *ntd) {
  if (metadata != nullptr) {
    assert(ntd == nullptr);
    ntd = metadata->getNominalTypeDescriptor();
  }

  if (ntd == nullptr || ntd->Name.get() != typeName)
    return nullptr;

  // Call the accessor if there is one.
  if (metadata == nullptr && !ntd->GenericParams.isGeneric()) {
    if (auto accessFn = ntd->getAccessFunction())
      metadata = accessFn();
  }

  return metadata;
}

// returns the type metadata for the type named by typeName
static const Metadata *
_searchTypeMetadataRecords(const TypeMetadataState &T,
                           const llvm::StringRef typeName) {
  unsigned sectionIdx = 0;
  unsigned endSectionIdx = T.SectionsToScan.size();
  const Metadata *foundMetadata = nullptr;

  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = T.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto ntd = record.getNominalTypeDescriptor())
        foundMetadata = _matchMetadataByMangledTypeName(typeName, nullptr, ntd);

      if (foundMetadata != nullptr)
        return foundMetadata;
    }
  }

  return nullptr;
}

static const Metadata *
_classByName(const llvm::StringRef typeName) {

  size_t DotPos = typeName.find('.');
  if (DotPos == llvm::StringRef::npos)
    return nullptr;
  if (typeName.find('.', DotPos + 1) != llvm::StringRef::npos)
    return nullptr;

  Demangle::NodeFactory Factory;

  NodePointer ClassNd = Factory.createNode(Node::Kind::Class);
  NodePointer ModuleNd = Factory.createNode(Node::Kind::Module,
                                            typeName.substr(0, DotPos));
  NodePointer NameNd = Factory.createNode(Node::Kind::Identifier,
                                          typeName.substr(DotPos + 1));
  ClassNd->addChild(ModuleNd, Factory);
  ClassNd->addChild(NameNd, Factory);

  std::string Mangled = mangleNode(ClassNd);
  StringRef MangledName = Mangled;

  const Metadata *foundMetadata = nullptr;
  auto &T = TypeMetadataRecords.get();

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.Cache.find(MangledName))
    return Value->getMetadata();

  // Check type metadata records
  T.SectionsToScanLock.withLock([&] {
    foundMetadata = _searchTypeMetadataRecords(T, MangledName);
  });

  // Check protocol conformances table. Note that this has no support for
  // resolving generic types yet.
  if (!foundMetadata)
    foundMetadata = _searchConformancesByMangledTypeName(MangledName);

  if (foundMetadata) {
    T.Cache.getOrInsert(MangledName, foundMetadata);
  }

#if SWIFT_OBJC_INTEROP
  // Check for ObjC class
  // FIXME does this have any value? any ObjC class with a Swift name
  // should already be registered as a Swift type.
  if (foundMetadata == nullptr) {
    std::string prefixedName("_Tt" + typeName.str());
    foundMetadata = reinterpret_cast<ClassMetadata *>
      (objc_lookUpClass(prefixedName.c_str()));
  }
#endif

  return foundMetadata;
}

/// Return the type metadata for a given name, used in the
/// implementation of _typeByName().
///
/// Currently only top-level classes are supported.

/// \param typeName The name of a class in the form: <module>.<class>
/// \return Returns the metadata of the type, if found.

/// internal func _getTypeByName(_ name: UnsafePointer<UInt8>,
///                              _ nameLength: UInt)  -> Any.Type?
#define _getTypeByName \
  MANGLE_SYM(s14_getTypeByNameyypXpSgSPys5UInt8VG_SutF)
SWIFT_CC(swift) SWIFT_RUNTIME_STDLIB_INTERNAL
const Metadata *
_getTypeByName(const char *typeName, size_t typeNameLength) {
  llvm::StringRef name(typeName, typeNameLength);
  return _classByName(name);
}

#pragma mark Metadata lookup via mangled name

namespace {
/// Constructs metadata by decoding a mangled type name, for use with
/// \c TypeDecoder.
class DecodedMetadataBuilder {
public:
  using BuiltType = const Metadata *;
  using BuiltNominalTypeDecl = const NominalTypeDescriptor *;

  BuiltNominalTypeDecl createNominalTypeDecl(
                                     const Demangle::NodePointer &node) const {
    // FIXME: Implement.
    return BuiltNominalTypeDecl();
  }

  BuiltType createNominalType(BuiltNominalTypeDecl typeDecl,
                              BuiltType parent) const {
    // FIXME: Implement.
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
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createExistentialMetatypeType(BuiltType instance) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createProtocolCompositionType(ArrayRef<BuiltType> protocols,
                                          bool hasExplicitAnyObject) const {
    // FIXME: Implement.
    // NOTE: protocols.back() could be a class type. Clean up this API.
    return BuiltType();
  }

  BuiltType createProtocolType(StringRef mangledName, StringRef moduleName,
                               StringRef privateDiscriminator,
                               StringRef name) const {
    // FIXME: Implement.
    return BuiltType();
  }

  BuiltType createGenericTypeParameterType(unsigned depth,
                                           unsigned index) const {
    // FIXME: Implement.
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
    return swift_getTupleTypeMetadata(elements.size(), elements.data(),
                                      labels.empty() ? nullptr : labels.c_str(),
                                      /*proposedWitnesses=*/nullptr);
  }

  BuiltType createDependentMemberType(StringRef name, BuiltType base,
                                      BuiltType protocol) const {
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
  // Demangle the type name.
  llvm::StringRef typeName(typeNameStart, typeNameLength);
  Demangler demangler;
  NodePointer node = demangler.demangleType(typeName);
  if (!node) {
    return nullptr;
  }

  DecodedMetadataBuilder builder;
  return Demangle::decodeMangledType(builder, node);
}
