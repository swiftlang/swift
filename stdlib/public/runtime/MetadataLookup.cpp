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
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/StringExtras.h"
#include "Private.h"

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__)
#include <elf.h>
#include <link.h>
#endif

#include <dlfcn.h>
#include <mutex>

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
                           const struct Metadata *metadata) {
      Name = name.str();
      Metadata = metadata;
    }

    bool matches(llvm::StringRef aName) {
      return aName.equals(Name);
    }

    const struct Metadata *getMetadata(void) {
      return Metadata;
    }
  };
}

static void _initializeCallbacksToInspectDylib();

struct TypeMetadataState {
  ConcurrentMap<size_t, TypeMetadataCacheEntry> Cache;
  std::vector<TypeMetadataSection> SectionsToScan;
  pthread_mutex_t SectionsToScanLock;

  TypeMetadataState() {
    SectionsToScan.reserve(16);
    pthread_mutex_init(&SectionsToScanLock, nullptr);
    _initializeCallbacksToInspectDylib();
  }
};

static Lazy<TypeMetadataState> TypeMetadataRecords;

static void
_registerTypeMetadataRecords(TypeMetadataState &T,
                             const TypeMetadataRecord *begin,
                             const TypeMetadataRecord *end) {
  pthread_mutex_lock(&T.SectionsToScanLock);
  T.SectionsToScan.push_back(TypeMetadataSection{begin, end});
  pthread_mutex_unlock(&T.SectionsToScanLock);
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
#elif defined(__ELF__)
static int _addImageTypeMetadataRecords(struct dl_phdr_info *info,
                                        size_t size, void * /*data*/) {
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
  auto records = reinterpret_cast<const uint8_t*>(
      dlsym(handle, SWIFT_TYPE_METADATA_SECTION));

  if (!records) {
    // if there are no type metadata records, don't hold this handle open.
    dlclose(handle);
    return 0;
  }

  // Extract the size of the type metadata block from the head of the section
  auto recordsSize = *reinterpret_cast<const uint64_t*>(records);
  records += sizeof(recordsSize);

  _addImageTypeMetadataRecordsBlock(records, recordsSize);

  dlclose(handle);
  return 0;
}
#endif

static void _initializeCallbacksToInspectDylib() {
#if defined(__APPLE__) && defined(__MACH__)
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(_addImageTypeMetadataRecords);
#elif defined(__ELF__)
  // Search the loaded dls. Unlike the above, this only searches the already
  // loaded ones.
  // FIXME: Find a way to have this continue to happen after.
  // rdar://problem/19045112
  dl_iterate_phdr(_addImageTypeMetadataRecords, nullptr);
#else
# error No known mechanism to inspect dynamic libraries on this platform.
#endif
}

void
swift::swift_registerTypeMetadataRecords(const TypeMetadataRecord *begin,
                                         const TypeMetadataRecord *end) {
  auto &T = TypeMetadataRecords.get();
  _registerTypeMetadataRecords(T, begin, end);
}

// copied from ProtocolConformanceRecord::getCanonicalTypeMetadata()
const Metadata *TypeMetadataRecord::getCanonicalTypeMetadata() const {
  switch (getTypeKind()) {
  case TypeMetadataRecordKind::UniqueDirectType:
    return getDirectType();
  case TypeMetadataRecordKind::NonuniqueDirectType:
    return swift_getForeignTypeMetadata((ForeignTypeMetadata *)getDirectType());
  case TypeMetadataRecordKind::UniqueDirectClass:
    if (auto *ClassMetadata =
          static_cast<const struct ClassMetadata *>(getDirectType()))
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
                                       const GenericMetadata *pattern) {
  const NominalTypeDescriptor *ntd = nullptr;
  const Metadata *foundMetadata = nullptr;

  if (metadata != nullptr)
    ntd = metadata->getNominalTypeDescriptor();
  else if (pattern != nullptr)
    ntd = pattern->getTemplateDescription();

  if (ntd == nullptr || ntd->Name.get() != typeName)
    return nullptr;

  if (pattern != nullptr) {
    if (!ntd->GenericParams.hasGenericParams())
      foundMetadata = swift_getResilientMetadata(const_cast<GenericMetadata *>(pattern));
  } else {
    foundMetadata = metadata;
  }

  return foundMetadata;
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
      else if (auto pattern = record.getGenericPattern())
        foundMetadata = _matchMetadataByMangledTypeName(typeName, nullptr, pattern);

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
  size_t hash = llvm::HashString(typeName);

  ConcurrentList<TypeMetadataCacheEntry> &Bucket = T.Cache.findOrAllocateNode(hash);

  // Check name to type metadata cache
  for (auto &Entry : Bucket) {
    if (Entry.matches(typeName))
      return Entry.getMetadata();
  }

  // Check type metadata records
  pthread_mutex_lock(&T.SectionsToScanLock);
  foundMetadata = _searchTypeMetadataRecords(T, typeName);
  pthread_mutex_unlock(&T.SectionsToScanLock);

  // Check protocol conformances table. Note that this has no support for
  // resolving generic types yet.
  if (foundMetadata == nullptr)
    foundMetadata = _searchConformancesByMangledTypeName(typeName);

  if (foundMetadata != nullptr)
    Bucket.push_front(TypeMetadataCacheEntry(typeName, foundMetadata));

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
extern "C"
const Metadata *
swift_getTypeByMangledName(const char *typeName, size_t typeNameLength) {
  llvm::StringRef name(typeName, typeNameLength);
  return _typeByMangledName(name);
}
