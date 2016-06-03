//===--- MetadataLookup.cpp - Swift Language Type Name Lookup -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implementations of runtime functions for looking up a type by name.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/StringExtras.h"
#include "Private.h"

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__) || defined(__ANDROID__)
#include <elf.h>
#include <link.h>
#endif

using namespace swift;
using namespace Demangle;

#if SWIFT_OBJC_INTEROP
#include <objc/runtime.h>
#include <objc/message.h>
#include <objc/objc.h>
#endif

#if defined(__APPLE__) && defined(__MACH__)
#define SWIFT_TYPE_METADATA_SECTION "__swift2_types"
#elif defined(__ELF__)
#define SWIFT_TYPE_METADATA_SECTION ".swift2_type_metadata_start"
#elif defined(__CYGWIN__) || defined(_MSC_VER)
#define SWIFT_TYPE_METADATA_SECTION ".sw2tymd"
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
}

#if defined(__APPLE__) && defined(__MACH__)
static void _initializeCallbacksToInspectDylib();
#else
namespace swift {
  void _swift_initializeCallbacksToInspectDylib(
    void (*fnAddImageBlock)(const uint8_t *, size_t),
    const char *sectionName);
}

static void _addImageTypeMetadataRecordsBlock(const uint8_t *records,
                                              size_t recordsSize);
#endif

struct TypeMetadataState {
  ConcurrentMap<TypeMetadataCacheEntry> Cache;
  std::vector<TypeMetadataSection> SectionsToScan;
  Mutex SectionsToScanLock;

  TypeMetadataState() {
    SectionsToScan.reserve(16);
#if defined(__APPLE__) && defined(__MACH__)
    _initializeCallbacksToInspectDylib();
#else
    _swift_initializeCallbacksToInspectDylib(
      _addImageTypeMetadataRecordsBlock,
      SWIFT_TYPE_METADATA_SECTION);
#endif
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

static void _addImageTypeMetadataRecordsBlock(const uint8_t *records,
                                              size_t recordsSize) {
  assert(recordsSize % sizeof(TypeMetadataRecord) == 0
         && "weird-sized type metadata section?!");

  // If we have a section, enqueue the type metadata for lookup.
  auto recordsBegin
    = reinterpret_cast<const TypeMetadataRecord*>(records);
  auto recordsEnd
    = reinterpret_cast<const TypeMetadataRecord*>
                                            (records + recordsSize);

  // type metadata cache should always be sufficiently initialized by this point.
  _registerTypeMetadataRecords(TypeMetadataRecords.unsafeGetAlreadyInitialized(),
                               recordsBegin, recordsEnd);
}

#if defined(__APPLE__) && defined(__MACH__)
static void _addImageTypeMetadataRecords(const mach_header *mh,
                                         intptr_t vmaddr_slide) {
#ifdef __LP64__
  using mach_header_platform = mach_header_64;
  assert(mh->magic == MH_MAGIC_64 && "loaded non-64-bit image?!");
#else
  using mach_header_platform = mach_header;
#endif

  // Look for a __swift2_types section.
  unsigned long recordsSize;
  const uint8_t *records =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_TEXT, SWIFT_TYPE_METADATA_SECTION,
                   &recordsSize);

  if (!records)
    return;

  _addImageTypeMetadataRecordsBlock(records, recordsSize);
}

static void _initializeCallbacksToInspectDylib() {
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(_addImageTypeMetadataRecords);
}
#endif

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
_typeByMangledName(const llvm::StringRef typeName) {
  const Metadata *foundMetadata = nullptr;
  auto &T = TypeMetadataRecords.get();

  // Look for an existing entry.
  // Find the bucket for the metadata entry.
  if (auto Value = T.Cache.find(typeName))
    return Value->getMetadata();

  // Check type metadata records
  T.SectionsToScanLock.withLock([&] {
    foundMetadata = _searchTypeMetadataRecords(T, typeName);
  });

  // Check protocol conformances table. Note that this has no support for
  // resolving generic types yet.
  if (!foundMetadata)
    foundMetadata = _searchConformancesByMangledTypeName(typeName);

  if (foundMetadata) {
    T.Cache.getOrInsert(typeName, foundMetadata);
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

/// Return the type metadata for a given mangled name, used in the
/// implementation of _typeByName(). The human readable name returned
/// by swift_getTypeName() is non-unique, so we used mangled names
/// internally.
SWIFT_RUNTIME_EXPORT
extern "C"
const Metadata *
swift_getTypeByMangledName(const char *typeName, size_t typeNameLength) {
  llvm::StringRef name(typeName, typeNameLength);
  return _typeByMangledName(name);
}
