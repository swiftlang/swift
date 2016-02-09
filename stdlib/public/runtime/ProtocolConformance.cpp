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


#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

void ProtocolConformanceRecord::dump() const {
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
      if (auto ntd = getDirectType()->getNominalTypeDescriptor()) {
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
const Metadata *ProtocolConformanceRecord::getCanonicalTypeMetadata()
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

const WitnessTable *ProtocolConformanceRecord::getWitnessTable(const Metadata *type)
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

  struct ConformanceCacheEntry {
  private:
    const void *Type; 
    const ProtocolDescriptor *Proto;
    uintptr_t Data;
    // All Darwin 64-bit platforms reserve the low 2^32 of address space, which
    // is more than enough invalid pointer values for any realistic generation
    // number. It's a little easier to overflow on 32-bit, so we need an extra
    // bit there.
#if !__LP64__
    bool Success;
#endif

    ConformanceCacheEntry(const void *type,
                          const ProtocolDescriptor *proto,
                          uintptr_t Data, bool Success)
      : Type(type), Proto(proto), Data(Data)
#if !__LP64__
        , Success(Success)
#endif
    {
#if __LP64__
#  if __APPLE__
      assert((!Success && Data <= 0xFFFFFFFFU) ||
             (Success && Data > 0xFFFFFFFFU));
#  elif __linux__ || __FreeBSD__
      assert((!Success && Data <= 0x0FFFU) ||
             (Success && Data > 0x0FFFU));
#  else
#    error "port me"
#  endif
#endif
  }

  public:
    ConformanceCacheEntry() = default;

    static ConformanceCacheEntry createSuccess(
        const void *type, const ProtocolDescriptor *proto,
        const swift::WitnessTable *witness) {
      return ConformanceCacheEntry(type, proto, (uintptr_t) witness, true);
    }

    static ConformanceCacheEntry createFailure(
        const void *type, const ProtocolDescriptor *proto,
        unsigned failureGeneration) {
      return ConformanceCacheEntry(type, proto, (uintptr_t) failureGeneration,
          false);
    }

    /// \returns true if the entry represents an entry for the pair \p type
    /// and \p proto.
    bool matches(const void *type, const ProtocolDescriptor *proto) {
      return type == Type && Proto == proto;
    }
   
    bool isSuccessful() const {
#if __LP64__
#  if __APPLE__
      return Data > 0xFFFFFFFFU;
#  elif __linux__ || __FreeBSD__
      return Data > 0x0FFFU;
#  else
#    error "port me"
#  endif
#else
      return Success;
#endif
    }
    
    /// Get the cached witness table, if successful.
    const WitnessTable *getWitnessTable() const {
      assert(isSuccessful());
      return (const WitnessTable *)Data;
    }
    
    /// Get the generation number under which this lookup failed.
    unsigned getFailureGeneration() const {
      assert(!isSuccessful());
      return Data;
    }
  };
}

// Conformance Cache.

static void _initializeCallbacksToInspectDylib();

struct ConformanceState {
  ConcurrentMap<size_t, ConformanceCacheEntry> Cache;
  std::vector<ConformanceSection> SectionsToScan;
  pthread_mutex_t SectionsToScanLock;
  
  ConformanceState() {
    SectionsToScan.reserve(16);
    pthread_mutex_init(&SectionsToScanLock, nullptr);
    _initializeCallbacksToInspectDylib();
  }
};

static Lazy<ConformanceState> Conformances;

static void
_registerProtocolConformances(ConformanceState &C,
                              const ProtocolConformanceRecord *begin,
                              const ProtocolConformanceRecord *end) {
  pthread_mutex_lock(&C.SectionsToScanLock);
  C.SectionsToScan.push_back(ConformanceSection{begin, end});
  pthread_mutex_unlock(&C.SectionsToScanLock);
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
#elif defined(__ELF__)
static int _addImageProtocolConformances(struct dl_phdr_info *info,
                                          size_t size, void * /*data*/) {
  void *handle;
  if (!info->dlpi_name || info->dlpi_name[0] == '\0') {
    handle = dlopen(nullptr, RTLD_LAZY);
  } else
    handle = dlopen(info->dlpi_name, RTLD_LAZY | RTLD_NOLOAD);
  auto conformances = reinterpret_cast<const uint8_t*>(
      dlsym(handle, SWIFT_PROTOCOL_CONFORMANCES_SECTION));

  if (!conformances) {
    // if there are no conformances, don't hold this handle open.
    dlclose(handle);
    return 0;
  }

  // Extract the size of the conformances block from the head of the section
  auto conformancesSize = *reinterpret_cast<const uint64_t*>(conformances);
  conformances += sizeof(conformancesSize);

  _addImageProtocolConformancesBlock(conformances, conformancesSize);

  dlclose(handle);
  return 0;
}
#endif

static void _initializeCallbacksToInspectDylib() {
#if defined(__APPLE__) && defined(__MACH__)
  // Install our dyld callback.
  // Dyld will invoke this on our behalf for all images that have already
  // been loaded.
  _dyld_register_func_for_add_image(_addImageProtocolConformances);
#elif defined(__ELF__)
  // Search the loaded dls. Unlike the above, this only searches the already
  // loaded ones.
  // FIXME: Find a way to have this continue to happen after.
  // rdar://problem/19045112
  dl_iterate_phdr(_addImageProtocolConformances, nullptr);
#else
# error No known mechanism to inspect dynamic libraries on this platform.
#endif
}

// This variable is used to signal when a cache was generated and
// it is correct to avoid a new scan.
static unsigned ConformanceCacheGeneration = 0;

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  auto &C = Conformances.get();
  _registerProtocolConformances(C, begin, end);
}

static size_t hashTypeProtocolPair(const void *type,
                                   const ProtocolDescriptor *protocol) {
  // A simple hash function for the conformance pair.
  return (size_t)type + ((size_t)protocol >> 2);
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
    // Hash and lookup the type-protocol pair in the cache.
    size_t hash = hashTypeProtocolPair(type, protocol);

    // Check if the type-protocol entry exists in the cache entry that we found.
    while (auto *Value = C.Cache.findValueByKey(hash)) {
      if (Value->matches(type, protocol)) {
        if (Value->isSuccessful())
          return std::make_pair(Value->getWitnessTable(), true);

        if (type == origType)
          foundEntry = Value;

        // If we got a cached negative response, check the generation number.
        if (Value->getFailureGeneration() == C.SectionsToScan.size()) {
          // We found an entry with a negative value.
          return std::make_pair(nullptr, true);
        }
      }

      // The entry that we fetched does not match our key due to a collision.
      // If we have a collision increase the hash value by one and try again.
      hash++;
    }
  }

  {
    // For generic and resilient types, nondependent conformances
    // are keyed by the nominal type descriptor rather than the
    // metadata, so try that.
    auto *description = type->getNominalTypeDescriptor();

    // Hash and lookup the type-protocol pair in the cache.
    size_t hash = hashTypeProtocolPair(description, protocol);

    while (auto *Value = C.Cache.findValueByKey(hash)) {
      if (Value->matches(description, protocol)) {
        if (Value->isSuccessful())
          return std::make_pair(Value->getWitnessTable(), true);

        // We don't try to cache negative responses for generic
        // patterns.
      }

      // If we have a collision increase the hash value by one and try again.
      hash++;
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
    auto *description = type->getNominalTypeDescriptor();
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

  unsigned failedGeneration = ConformanceCacheGeneration;

  // If we didn't have an up-to-date cache entry, scan the conformance records.
  pthread_mutex_lock(&C.SectionsToScanLock);

  // If we have no new information to pull in (and nobody else pulled in
  // new information while we waited on the lock), we're done.
  if (C.SectionsToScan.size() == numSections) {
    if (failedGeneration != ConformanceCacheGeneration) {
      // Someone else pulled in new conformances while we were waiting.
      // Start over with our newly-populated cache.
      pthread_mutex_unlock(&C.SectionsToScanLock);
      type = origType;
      goto recur;
    }


    // Hash and lookup the type-protocol pair in the cache.
    size_t hash = hashTypeProtocolPair(type, protocol);
    auto E = ConformanceCacheEntry::createFailure(type, protocol,
                                                  C.SectionsToScan.size());

    while (!C.Cache.tryToAllocateNewNode(hash, E)) {
      // If we have a collision increase the hash value by one and try again.
      hash++;
    }

    pthread_mutex_unlock(&C.SectionsToScanLock);
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
        size_t hash = hashTypeProtocolPair(metadata, P);

        auto witness = record.getWitnessTable(metadata);
        ConformanceCacheEntry E;
        if (witness)
          E = ConformanceCacheEntry::createSuccess(metadata, P, witness);
        else
          E = ConformanceCacheEntry::createFailure(metadata, P,
                                                   C.SectionsToScan.size());

        while (!C.Cache.tryToAllocateNewNode(hash, E)) {
          // If we have a collision increase the hash value by one and try again.
          hash++;
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

        // Hash and store the type-protocol pair in the cache.
        size_t hash = hashTypeProtocolPair(R, P);
        auto E = ConformanceCacheEntry::createSuccess(R, P,
                                  record.getStaticWitnessTable());

        while (!C.Cache.tryToAllocateNewNode(hash, E)) {
          // If we have a collision increase the hash value by one and try again.
          hash++;
        }

      }
    }
  }
  ++ConformanceCacheGeneration;

  pthread_mutex_unlock(&C.SectionsToScanLock);
  // Start over with our newly-populated cache.
  type = origType;
  goto recur;
}

const Metadata *
swift::_searchConformancesByMangledTypeName(const llvm::StringRef typeName) {
  auto &C = Conformances.get();
  const Metadata *foundMetadata = nullptr;

  pthread_mutex_lock(&C.SectionsToScanLock);

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

  pthread_mutex_unlock(&C.SectionsToScanLock);

  return foundMetadata;
}
