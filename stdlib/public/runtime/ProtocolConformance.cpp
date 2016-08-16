//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
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
// Checking and caching of Swift protocol conformances.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "Private.h"

#if defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#elif defined(__ELF__) || defined(__ANDROID__)
#include <elf.h>
#include <link.h>
#endif

#if defined(_MSC_VER)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace swift;

#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

template<> void ProtocolConformanceRecord::dump() const {
  auto symbolName = [&](const void *addr) -> const char * {
    Dl_info info;
    int ok = dladdr(addr, &info);
    if (!ok)
      return "<unknown addr>";
    return info.dli_sname;
  };

  switch (auto kind = getTypeKind()) {
    case TypeMetadataRecordKind::Universal:
      printf("universal");
      break;
    case TypeMetadataRecordKind::UniqueDirectType:
    case TypeMetadataRecordKind::NonuniqueDirectType:
      printf("%s direct type ",
             kind == TypeMetadataRecordKind::UniqueDirectType
             ? "unique" : "nonunique");
      if (auto &ntd = getDirectType()->getNominalTypeDescriptor()) {
        printf("%s", ntd->Name.get());
      } else {
        printf("<structural type>");
      }
      break;
    case TypeMetadataRecordKind::UniqueDirectClass:
      printf("unique direct class %s",
             class_getName(getDirectClass()));
      break;
    case TypeMetadataRecordKind::UniqueIndirectClass:
      printf("unique indirect class %s",
             class_getName(*getIndirectClass()));
      break;
      
    case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
      printf("unique nominal type descriptor %s", symbolName(getNominalTypeDescriptor()));
      break;
  }
  
  printf(" => ");
  
  switch (getConformanceKind()) {
    case ProtocolConformanceReferenceKind::WitnessTable:
      printf("witness table %s\n", symbolName(getStaticWitnessTable()));
      break;
    case ProtocolConformanceReferenceKind::WitnessTableAccessor:
      printf("witness table accessor %s\n",
             symbolName((const void *)(uintptr_t)getWitnessTableAccessor()));
      break;
  }
}
#endif

/// Take the type reference inside a protocol conformance record and fetch the
/// canonical metadata pointer for the type it refers to.
/// Returns nil for universal or generic type references.
template<> const Metadata *ProtocolConformanceRecord::getCanonicalTypeMetadata()
const {
  switch (getTypeKind()) {
  case TypeMetadataRecordKind::UniqueDirectType:
    // Already unique.
    return getDirectType();
  case TypeMetadataRecordKind::NonuniqueDirectType:
    // Ask the runtime for the unique metadata record we've canonized.
    return swift_getForeignTypeMetadata((ForeignTypeMetadata*)getDirectType());
  case TypeMetadataRecordKind::UniqueIndirectClass:
    // The class may be ObjC, in which case we need to instantiate its Swift
    // metadata. The class additionally may be weak-linked, so we have to check
    // for null.
    if (auto *ClassMetadata = *getIndirectClass())
      return swift_getObjCClassMetadata(ClassMetadata);
    return nullptr;
      
  case TypeMetadataRecordKind::UniqueDirectClass:
    // The class may be ObjC, in which case we need to instantiate its Swift
    // metadata.
    if (auto *ClassMetadata = getDirectClass())
      return swift_getObjCClassMetadata(ClassMetadata);
    return nullptr;
      
  case TypeMetadataRecordKind::UniqueNominalTypeDescriptor:
  case TypeMetadataRecordKind::Universal:
    // The record does not apply to a single type.
    return nullptr;
  }
}

template<>
const WitnessTable *
ProtocolConformanceRecord::getWitnessTable(const Metadata *type)
const {
  switch (getConformanceKind()) {
  case ProtocolConformanceReferenceKind::WitnessTable:
    return getStaticWitnessTable();

  case ProtocolConformanceReferenceKind::WitnessTableAccessor:
    return getWitnessTableAccessor()(type);
  }
}

#if defined(__APPLE__) && defined(__MACH__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION "__swift2_proto"
#elif defined(__ELF__)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION ".swift2_protocol_conformances_start"
#elif defined(__CYGWIN__) || defined(_MSC_VER)
#define SWIFT_PROTOCOL_CONFORMANCES_SECTION ".sw2prtc"
#endif

namespace {
  struct ConformanceSection {
    const ProtocolConformanceRecord *Begin, *End;
    const ProtocolConformanceRecord *begin() const {
      return Begin;
    }
    const ProtocolConformanceRecord *end() const {
      return End;
    }
  };

  struct ConformanceCacheKey {
    /// Either a Metadata* or a NominalTypeDescriptor*.
    const void *Type;
    const ProtocolDescriptor *Proto;

    ConformanceCacheKey(const void *type, const ProtocolDescriptor *proto)
      : Type(type), Proto(proto) {}
  };

  struct ConformanceCacheEntry {
  private:
    const void *Type; 
    const ProtocolDescriptor *Proto;
    std::atomic<const WitnessTable *> Table;
    std::atomic<uintptr_t> FailureGeneration;

  public:
    ConformanceCacheEntry(ConformanceCacheKey key,
                          const WitnessTable *table,
                          uintptr_t failureGeneration)
      : Type(key.Type), Proto(key.Proto), Table(table),
        FailureGeneration(failureGeneration) {
    }

    int compareWithKey(const ConformanceCacheKey &key) const {
      if (key.Type != Type) {
        return (uintptr_t(key.Type) < uintptr_t(Type) ? -1 : 1);
      } else if (key.Proto != Proto) {
        return (uintptr_t(key.Proto) < uintptr_t(Proto) ? -1 : 1);
      } else {
        return 0;
      }
    }

    template <class... Args>
    static size_t getExtraAllocationSize(Args &&... ignored) {
      return 0;
    }

    bool isSuccessful() const {
      return Table.load(std::memory_order_relaxed) != nullptr;
    }

    void makeSuccessful(const WitnessTable *table) {
      Table.store(table, std::memory_order_release);
    }

    void updateFailureGeneration(uintptr_t failureGeneration) {
      assert(!isSuccessful());
      FailureGeneration.store(failureGeneration, std::memory_order_relaxed);
    }
    
    /// Get the cached witness table, if successful.
    const WitnessTable *getWitnessTable() const {
      assert(isSuccessful());
      return Table.load(std::memory_order_acquire);
    }
    
    /// Get the generation number under which this lookup failed.
    unsigned getFailureGeneration() const {
      assert(!isSuccessful());
      return FailureGeneration.load(std::memory_order_relaxed);
    }
  };
}

// Conformance Cache.
#if defined(__APPLE__) && defined(__MACH__)
static void _initializeCallbacksToInspectDylib();
#else
namespace swift {
  void _swift_initializeCallbacksToInspectDylib(
    void (*fnAddImageBlock)(const uint8_t *, size_t),
    const char *sectionName);
}

static void _addImageProtocolConformancesBlock(const uint8_t *conformances,
                                               size_t conformancesSize);
#endif

struct ConformanceState {
  ConcurrentMap<ConformanceCacheEntry> Cache;
  std::vector<ConformanceSection> SectionsToScan;
  Mutex SectionsToScanLock;
  
  ConformanceState() {
    SectionsToScan.reserve(16);
#if defined(__APPLE__) && defined(__MACH__)
    _initializeCallbacksToInspectDylib();
#else
    _swift_initializeCallbacksToInspectDylib(
      _addImageProtocolConformancesBlock,
      SWIFT_PROTOCOL_CONFORMANCES_SECTION);
#endif
  }

  void cacheSuccess(const void *type, const ProtocolDescriptor *proto,
                    const WitnessTable *witness) {
    auto result = Cache.getOrInsert(ConformanceCacheKey(type, proto),
                                    witness, uintptr_t(0));

    // If the entry was already present, we may need to update it.
    if (!result.second) {
      result.first->makeSuccessful(witness);
    }
  }

  void cacheFailure(const void *type, const ProtocolDescriptor *proto) {
    uintptr_t failureGeneration = SectionsToScan.size();
    auto result = Cache.getOrInsert(ConformanceCacheKey(type, proto),
                                    (const WitnessTable *) nullptr,
                                    failureGeneration);

    // If the entry was already present, we may need to update it.
    if (!result.second) {
      result.first->updateFailureGeneration(failureGeneration);
    }
  }

  ConformanceCacheEntry *findCached(const void *type,
                                    const ProtocolDescriptor *proto) {
    return Cache.find(ConformanceCacheKey(type, proto));
  }
};

static Lazy<ConformanceState> Conformances;

static void
_registerProtocolConformances(ConformanceState &C,
                              const ProtocolConformanceRecord *begin,
                              const ProtocolConformanceRecord *end) {
  ScopedLock guard(C.SectionsToScanLock);
  C.SectionsToScan.push_back(ConformanceSection{begin, end});
}

static void _addImageProtocolConformancesBlock(const uint8_t *conformances,
                                               size_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0
         && "weird-sized conformances section?!");

  // If we have a section, enqueue the conformances for lookup.
  auto recordsBegin
    = reinterpret_cast<const ProtocolConformanceRecord*>(conformances);
  auto recordsEnd
    = reinterpret_cast<const ProtocolConformanceRecord*>
                                            (conformances + conformancesSize);
  
  // Conformance cache should always be sufficiently initialized by this point.
  _registerProtocolConformances(Conformances.unsafeGetAlreadyInitialized(),
                                recordsBegin, recordsEnd);
}

#if !defined(__APPLE__) || !defined(__MACH__)
// Common Structure
struct InspectArgs {
  void (*fnAddImageBlock)(const uint8_t *, size_t);
  const char *sectionName;
};
#endif

#if defined(__APPLE__) && defined(__MACH__)
static void _addImageProtocolConformances(const mach_header *mh,
                                          intptr_t vmaddr_slide) {
#ifdef __LP64__
  using mach_header_platform = mach_header_64;
  assert(mh->magic == MH_MAGIC_64 && "loaded non-64-bit image?!");
#else
  using mach_header_platform = mach_header;
#endif
  
  // Look for a __swift2_proto section.
  unsigned long conformancesSize;
  const uint8_t *conformances =
    getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                   SEG_TEXT, SWIFT_PROTOCOL_CONFORMANCES_SECTION,
                   &conformancesSize);
  
  if (!conformances)
    return;
  
  _addImageProtocolConformancesBlock(conformances, conformancesSize);
}

static void _initializeCallbacksToInspectDylib() {
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(_addImageProtocolConformances);
}

#elif defined(__ELF__) || defined(__ANDROID__)
static int _addImageProtocolConformances(struct dl_phdr_info *info,
                                          size_t size, void *data) {
  // inspectArgs contains addImage*Block function and the section name
  InspectArgs *inspectArgs = reinterpret_cast<InspectArgs *>(data);

  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);

  if (!handle) {
    // Not a shared library.
    return 0;
  }

  auto conformances = reinterpret_cast<const uint8_t*>(
      dlsym(handle, inspectArgs->sectionName));

  if (!conformances) {
    // if there are no conformances, don't hold this handle open.
    dlclose(handle);
    return 0;
  }

  // Extract the size of the conformances block from the head of the section
  auto conformancesSize = *reinterpret_cast<const uint64_t*>(conformances);
  conformances += sizeof(conformancesSize);

  inspectArgs->fnAddImageBlock(conformances, conformancesSize);

  dlclose(handle);
  return 0;
}

void swift::_swift_initializeCallbacksToInspectDylib(
    void (*fnAddImageBlock)(const uint8_t *, size_t),
    const char *sectionName) {
  InspectArgs inspectArgs = {fnAddImageBlock, sectionName};

  // Search the loaded dls. Unlike the above, this only searches the already
  // loaded ones.
  // FIXME: Find a way to have this continue to happen after.
  // rdar://problem/19045112
  dl_iterate_phdr(_addImageProtocolConformances, &inspectArgs);
}
#elif defined(__CYGWIN__) || defined(_MSC_VER)
static int _addImageProtocolConformances(struct dl_phdr_info *info,
                                          size_t size, void *data) {
  InspectArgs *inspectArgs = (InspectArgs *)data;
  // inspectArgs contains addImage*Block function and the section name
#if defined(_MSC_VER)
  HMODULE handle;

  if (!info->dlpi_name || info->dlpi_name[0] == '\0')
    handle = GetModuleHandle(nullptr);
  else
    handle = GetModuleHandle(info->dlpi_name);
#else
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0')
    handle = dlopen(nullptr, RTLD_LAZY);
  else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
#endif

  unsigned long conformancesSize;
  const uint8_t *conformances =
    _swift_getSectionDataPE(handle, inspectArgs->sectionName,
                           &conformancesSize);

  if (conformances)
    inspectArgs->fnAddImageBlock(conformances, conformancesSize);

#if defined(_MSC_VER)
  FreeLibrary(handle);
#else
  dlclose(handle);
#endif
  return 0;
}

void swift::_swift_initializeCallbacksToInspectDylib(
    void (*fnAddImageBlock)(const uint8_t *, size_t),
    const char *sectionName) {
  InspectArgs inspectArgs = {fnAddImageBlock, sectionName};

  _swift_dl_iterate_phdr(_addImageProtocolConformances, &inspectArgs);
}
#else
# error No known mechanism to inspect dynamic libraries on this platform.
#endif

// This variable is used to signal when a cache was generated and
// it is correct to avoid a new scan.
static unsigned ConformanceCacheGeneration = 0;

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  auto &C = Conformances.get();
  _registerProtocolConformances(C, begin, end);
}

/// Search the witness table in the ConformanceCache. \returns a pair of the
/// WitnessTable pointer and a boolean value True if a definitive value is
/// found. \returns false if the type or its superclasses were not found in
/// the cache.
static
std::pair<const WitnessTable *, bool>
searchInConformanceCache(const Metadata *type,
                         const ProtocolDescriptor *protocol,
                         ConformanceCacheEntry *&foundEntry) {
  auto &C = Conformances.get();
  auto origType = type;

  foundEntry = nullptr;

recur_inside_cache_lock:

  // See if we have a cached conformance. Try the specific type first.

  {
    // Check if the type-protocol entry exists in the cache entry that we found.
    if (auto *Value = C.findCached(type, protocol)) {
      if (Value->isSuccessful())
        return std::make_pair(Value->getWitnessTable(), true);

      // If we're still looking up for the original type, remember that
      // we found an exact match.
      if (type == origType)
        foundEntry = Value;

      // If we got a cached negative response, check the generation number.
      if (Value->getFailureGeneration() == C.SectionsToScan.size()) {
        // We found an entry with a negative value.
        return std::make_pair(nullptr, true);
      }
    }
  }

  {
    // For generic and resilient types, nondependent conformances
    // are keyed by the nominal type descriptor rather than the
    // metadata, so try that.
    auto *description = type->getNominalTypeDescriptor().get();

    // Hash and lookup the type-protocol pair in the cache.
    if (auto *Value = C.findCached(description, protocol)) {
      if (Value->isSuccessful())
        return std::make_pair(Value->getWitnessTable(), true);

      // We don't try to cache negative responses for generic
      // patterns.
    }
  }

  // If the type is a class, try its superclass.
  if (const ClassMetadata *classType = type->getClassObject()) {
    if (classHasSuperclass(classType)) {
      type = swift_getObjCClassMetadata(classType->SuperClass);
      goto recur_inside_cache_lock;
    }
  }

  // We did not find an entry.
  return std::make_pair(nullptr, false);
}

/// Checks if a given candidate is a type itself, one of its
/// superclasses or a related generic type.
///
/// This check is supposed to use the same logic that is used
/// by searchInConformanceCache.
///
/// \param candidate Pointer to a Metadata or a NominalTypeDescriptor.
///
static
bool isRelatedType(const Metadata *type, const void *candidate,
                   bool candidateIsMetadata) {

  while (true) {
    if (type == candidate && candidateIsMetadata)
      return true;

    // If the type is resilient or generic, see if there's a witness table
    // keyed off the nominal type descriptor.
    auto *description = type->getNominalTypeDescriptor().get();
    if (description == candidate && !candidateIsMetadata)
      return true;

    // If the type is a class, try its superclass.
    if (const ClassMetadata *classType = type->getClassObject()) {
      if (classHasSuperclass(classType)) {
        type = swift_getObjCClassMetadata(classType->SuperClass);
        continue;
      }
    }

    break;
  }

  return false;
}

const WitnessTable *
swift::swift_conformsToProtocol(const Metadata *type,
                                const ProtocolDescriptor *protocol) {
  auto &C = Conformances.get();
  auto origType = type;
  unsigned numSections = 0;
  ConformanceCacheEntry *foundEntry;

recur:
  // See if we have a cached conformance. The ConcurrentMap data structure
  // allows us to insert and search the map concurrently without locking.
  // We do lock the slow path because the SectionsToScan data structure is not
  // concurrent.
  auto FoundConformance = searchInConformanceCache(type, protocol, foundEntry);
  // The negative answer does not always mean that there is no conformance,
  // unless it is an exact match on the type. If it is not an exact match,
  // it may mean that all of the superclasses do not have this conformance,
  // but the actual type may still have this conformance.
  if (FoundConformance.second) {
    if (FoundConformance.first || foundEntry)
      return FoundConformance.first;
  }

  // If we didn't have an up-to-date cache entry, scan the conformance records.
  C.SectionsToScanLock.lock();
  unsigned failedGeneration = ConformanceCacheGeneration;

  // If we have no new information to pull in (and nobody else pulled in
  // new information while we waited on the lock), we're done.
  if (C.SectionsToScan.size() == numSections) {
    if (failedGeneration != ConformanceCacheGeneration) {
      // Someone else pulled in new conformances while we were waiting.
      // Start over with our newly-populated cache.
      C.SectionsToScanLock.unlock();
      type = origType;
      goto recur;
    }


    // Save the failure for this type-protocol pair in the cache.
    C.cacheFailure(type, protocol);

    C.SectionsToScanLock.unlock();
    return nullptr;
  }

  // Update the last known number of sections to scan.
  numSections = C.SectionsToScan.size();

  // Scan only sections that were not scanned yet.
  unsigned sectionIdx = foundEntry ? foundEntry->getFailureGeneration() : 0;
  unsigned endSectionIdx = C.SectionsToScan.size();

  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = C.SectionsToScan[sectionIdx];
    // Eagerly pull records for nondependent witnesses into our cache.
    for (const auto &record : section) {
      // If the record applies to a specific type, cache it.
      if (auto metadata = record.getCanonicalTypeMetadata()) {
        auto P = record.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, metadata, /*isMetadata=*/true))
          continue;

        // Store the type-protocol pair in the cache.
        auto witness = record.getWitnessTable(metadata);
        if (witness) {
          C.cacheSuccess(metadata, P, witness);
        } else {
          C.cacheFailure(metadata, P);
        }

      // If the record provides a nondependent witness table for all instances
      // of a generic type, cache it for the generic pattern.
      // TODO: "Nondependent witness table" probably deserves its own flag.
      // An accessor function might still be necessary even if the witness table
      // can be shared.
      } else if (record.getTypeKind()
                   == TypeMetadataRecordKind::UniqueNominalTypeDescriptor
                 && record.getConformanceKind()
                   == ProtocolConformanceReferenceKind::WitnessTable) {

        auto R = record.getNominalTypeDescriptor();
        auto P = record.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, R, /*isMetadata=*/false))
          continue;

        // Store the type-protocol pair in the cache.
        C.cacheSuccess(R, P, record.getStaticWitnessTable());
      }
    }
  }
  ++ConformanceCacheGeneration;

  C.SectionsToScanLock.unlock();
  // Start over with our newly-populated cache.
  type = origType;
  goto recur;
}

const Metadata *
swift::_searchConformancesByMangledTypeName(const llvm::StringRef typeName) {
  auto &C = Conformances.get();
  const Metadata *foundMetadata = nullptr;

  ScopedLock guard(C.SectionsToScanLock);

  unsigned sectionIdx = 0;
  unsigned endSectionIdx = C.SectionsToScan.size();

  for (; sectionIdx < endSectionIdx; ++sectionIdx) {
    auto &section = C.SectionsToScan[sectionIdx];
    for (const auto &record : section) {
      if (auto metadata = record.getCanonicalTypeMetadata())
        foundMetadata = _matchMetadataByMangledTypeName(typeName, metadata, nullptr);
      else if (auto ntd = record.getNominalTypeDescriptor())
        foundMetadata = _matchMetadataByMangledTypeName(typeName, nullptr, ntd);

      if (foundMetadata != nullptr)
        break;
    }
    if (foundMetadata != nullptr)
      break;
  }

  return foundMetadata;
}
