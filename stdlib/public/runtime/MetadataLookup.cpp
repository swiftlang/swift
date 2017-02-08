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
#include "swift/Basic/Demangle.h"
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

// copied from ProtocolConformanceRecord::getCanonicalTypeMetadata()
template<>
const Metadata *TypeMetadataRecord::getCanonicalTypeMetadata() const {
  switch (getTypeKind()) {
  case TypeMetadataRecordKind::UniqueDirectType:
    return getDirectType();
  case TypeMetadataRecordKind::NonuniqueDirectType:
    return swift_getForeignTypeMetadata((ForeignTypeMetadata *)getDirectType());
  case TypeMetadataRecordKind::UniqueDirectClass:
    if (auto *ClassMetadata =
          static_cast<const ::ClassMetadata *>(getDirectType()))
      return swift_getObjCClassMetadata(ClassMetadata);
    else
      return nullptr;
  default:
    return nullptr;
  }
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
      if (auto metadata = record.getCanonicalTypeMetadata())
        foundMetadata = _matchMetadataByMangledTypeName(typeName, metadata, nullptr);
      else if (auto ntd = record.getNominalTypeDescriptor())
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

  using namespace Demangle;

  NodePointer ClassNd = NodeFactory::create(Node::Kind::Class);
  NodePointer ModuleNd = NodeFactory::create(Node::Kind::Module,
                                             typeName.substr(0, DotPos));
  NodePointer NameNd = NodeFactory::create(Node::Kind::Identifier,
                                           typeName.substr(DotPos + 1));
  ClassNd->addChildren(ModuleNd, NameNd);

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
SWIFT_RUNTIME_EXPORT
const Metadata *
swift_getTypeByName(const char *typeName, size_t typeNameLength) {
  llvm::StringRef name(typeName, typeNameLength);
  return _classByName(name);
}
