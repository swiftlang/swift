//===--- ProtocolConformance.cpp - Swift protocol conformance checking ----===//
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
// Checking and caching of Swift protocol conformances.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/LLVM.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Mutex.h"
#include "swift/Runtime/Unreachable.h"
#include "ImageInspection.h"
#include "Private.h"

using namespace swift;

#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

template<> void ProtocolConformanceRecord::dump() const {
  auto symbolName = [&](const void *addr) -> const char * {
    SymbolInfo info;
    int ok = lookupSymbol(addr, &info);
    if (!ok)
      return "<unknown addr>";
    return info.symbolName;
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

  swift_runtime_unreachable("Unhandled TypeMetadataRecordKind in switch.");
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

  swift_runtime_unreachable(
      "Unhandled ProtocolConformanceReferenceKind in switch.");
}

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
} // end anonymous namespace

// Conformance Cache.
struct ConformanceState {
  ConcurrentMap<ConformanceCacheEntry> Cache;
  std::vector<ConformanceSection> SectionsToScan;
  Mutex SectionsToScanLock;
  
  ConformanceState() {
    SectionsToScan.reserve(16);
    initializeProtocolConformanceLookup();
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

void swift::addImageProtocolConformanceBlockCallback(const void *conformances,
                                                   uintptr_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0
         && "weird-sized conformances section?!");

  // If we have a section, enqueue the conformances for lookup.
  auto conformanceBytes = reinterpret_cast<const char *>(conformances);
  auto recordsBegin
    = reinterpret_cast<const ProtocolConformanceRecord*>(conformances);
  auto recordsEnd
    = reinterpret_cast<const ProtocolConformanceRecord*>
                                          (conformanceBytes + conformancesSize);
  
  // Conformance cache should always be sufficiently initialized by this point.
  _registerProtocolConformances(Conformances.unsafeGetAlreadyInitialized(),
                                recordsBegin, recordsEnd);
}

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  auto &C = Conformances.get();
  _registerProtocolConformances(C, begin, end);
}


struct ConformanceCacheResult {
  // true if witnessTable is an authoritative result as-is.
  // false if more searching is required (for example, because a cached
  // failure was returned in failureEntry but it is out-of-date.
  bool isAuthoritative;

  // The matching witness table, or null if no cached conformance was found.
  const WitnessTable *witnessTable;

  // If the search fails, this may be the negative cache entry for the
  // queried type itself. This entry may be null or out-of-date.
  ConformanceCacheEntry *failureEntry;

  static ConformanceCacheResult
  cachedSuccess(const WitnessTable *table) {
    return ConformanceCacheResult { true, table, nullptr };
  }

  static ConformanceCacheResult
  cachedFailure(ConformanceCacheEntry *entry, bool auth) {
    return ConformanceCacheResult { auth, nullptr, entry };
  }

  static ConformanceCacheResult
  cacheMiss() {
    return ConformanceCacheResult { false, nullptr, nullptr };
  }
};

/// Search for a witness table in the ConformanceCache.
static
ConformanceCacheResult
searchInConformanceCache(const Metadata *type,
                         const ProtocolDescriptor *protocol) {
  auto &C = Conformances.get();
  auto origType = type;
  ConformanceCacheEntry *failureEntry = nullptr;

recur:
  {
    // Try the specific type first.
    if (auto *Value = C.findCached(type, protocol)) {
      if (Value->isSuccessful()) {
        // Found a conformance on the type or some superclass. Return it.
        return ConformanceCacheResult::cachedSuccess(Value->getWitnessTable());
      }

      // Found a negative cache entry.

      bool isAuthoritative;
      if (type == origType) {
        // This negative cache entry is for the original query type.
        // Remember it so it can be returned later.
        failureEntry = Value;
        // An up-to-date entry for the original type is authoritative.
        isAuthoritative = true;
      } else {
        // An up-to-date cached failure for a superclass of the type is not
        // authoritative: there may be a still-undiscovered conformance
        // for the original query type.
        isAuthoritative = false;
      }

      // Check if the negative cache entry is up-to-date.
      // FIXME: Using SectionsToScan.size() outside SectionsToScanLock
      // is undefined.
      if (Value->getFailureGeneration() == C.SectionsToScan.size()) {
        // Negative cache entry is up-to-date. Return failure along with
        // the original query type's own cache entry, if we found one.
        // (That entry may be out of date but the caller still has use for it.)
        return ConformanceCacheResult::cachedFailure(failureEntry,
                                                     isAuthoritative);
      }

      // Negative cache entry is out-of-date.
      // Continue searching for a better result.
    }
  }

  {
    // For generic and resilient types, nondependent conformances
    // are keyed by the nominal type descriptor rather than the
    // metadata, so try that.
    const auto description = type->getNominalTypeDescriptor().get();

    // Hash and lookup the type-protocol pair in the cache.
    if (auto *Value = C.findCached(description, protocol)) {
      if (Value->isSuccessful())
        return ConformanceCacheResult::cachedSuccess(Value->getWitnessTable());

      // We don't try to cache negative responses for generic
      // patterns.
    }
  }

  // If the type is a class, try its superclass.
  if (const ClassMetadata *classType = type->getClassObject()) {
    if (classHasSuperclass(classType)) {
      type = swift_getObjCClassMetadata(classType->SuperClass);
      goto recur;
    }
  }

  // We did not find an up-to-date cache entry.
  // If we found an out-of-date entry for the original query type then
  // return it (non-authoritatively). Otherwise return a cache miss.
  if (failureEntry)
    return ConformanceCacheResult::cachedFailure(failureEntry, false);
  else
    return ConformanceCacheResult::cacheMiss();
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
    const auto description = type->getNominalTypeDescriptor().get();
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
swift::swift_conformsToProtocol(const Metadata * const type,
                                const ProtocolDescriptor *protocol) {
  auto &C = Conformances.get();

  // See if we have a cached conformance. The ConcurrentMap data structure
  // allows us to insert and search the map concurrently without locking.
  // We do lock the slow path because the SectionsToScan data structure is not
  // concurrent.
  auto FoundConformance = searchInConformanceCache(type, protocol);
  // If the result (positive or negative) is authoritative, return it.
  if (FoundConformance.isAuthoritative)
    return FoundConformance.witnessTable;

  auto failureEntry = FoundConformance.failureEntry;

  // No up-to-date cache entry found.
  // Acquire the lock so we can scan conformance records.
  ScopedLock guard(C.SectionsToScanLock);

  // The world may have changed while we waited for the lock.
  // If we found an out-of-date negative cache entry before
  // acquiring the lock, make sure the entry is still negative and out of date.
  // If we found no entry before acquiring the lock, search the cache again.
  if (failureEntry) {
    if (failureEntry->isSuccessful()) {
      // Somebody else found a conformance.
      return failureEntry->getWitnessTable();
    }
    if (failureEntry->getFailureGeneration() == C.SectionsToScan.size()) {
      // Somebody else brought the negative cache entry up to date.
      return nullptr;
    }
  }
  else {
    FoundConformance = searchInConformanceCache(type, protocol);
    if (FoundConformance.isAuthoritative) {
      // Somebody else found a conformance or cached an up-to-date failure.
      return FoundConformance.witnessTable;
    }
    failureEntry = FoundConformance.failureEntry;
  }

  // We are now caught up after acquiring the lock.
  // Prepare to scan conformance records.

  // Scan only sections that were not scanned yet.
  // If we found an out-of-date negative cache entry,
  // we need not to re-scan the sections that it covers.
  unsigned startSectionIdx =
    failureEntry ? failureEntry->getFailureGeneration() : 0;

  unsigned endSectionIdx = C.SectionsToScan.size();

  // If there are no unscanned sections outstanding
  // then we can cache failure and give up now.
  if (startSectionIdx == endSectionIdx) {
    C.cacheFailure(type, protocol);
    return nullptr;
  }

  // Really scan conformance records.

  for (unsigned sectionIdx = startSectionIdx;
       sectionIdx < endSectionIdx;
       ++sectionIdx) {
    auto &section = C.SectionsToScan[sectionIdx];
    // Eagerly pull records for nondependent witnesses into our cache.
    for (const auto &record : section) {
      // If the record applies to a specific type, cache it.
      if (auto metadata = record.getCanonicalTypeMetadata()) {
        auto P = record.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, metadata, /*candidateIsMetadata=*/true))
          continue;

        // Store the type-protocol pair in the cache.
        auto witness = record.getWitnessTable(metadata);
        if (witness) {
          C.cacheSuccess(metadata, P, witness);
        } else {
          C.cacheFailure(metadata, P);
        }

      // TODO: "Nondependent witness table" probably deserves its own flag.
      // An accessor function might still be necessary even if the witness table
      // can be shared.
      } else if (record.getTypeKind()
                   == TypeMetadataRecordKind::UniqueNominalTypeDescriptor) {

        auto R = record.getNominalTypeDescriptor();
        auto P = record.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, R, /*candidateIsMetadata=*/false))
          continue;

        // Store the type-protocol pair in the cache.
        switch (record.getConformanceKind()) {
        case ProtocolConformanceReferenceKind::WitnessTable:
          // If the record provides a nondependent witness table for all
          // instances of a generic type, cache it for the generic pattern.
          C.cacheSuccess(R, P, record.getStaticWitnessTable());
          break;

        case ProtocolConformanceReferenceKind::WitnessTableAccessor:
          // If the record provides a dependent witness table accessor,
          // cache the result for the instantiated type metadata.
          C.cacheSuccess(type, P, record.getWitnessTable(type));
          break;

        }
      }
    }
  }

  // Conformance scan is complete.
  // Search the cache once more, and this time update the cache if necessary.

  FoundConformance = searchInConformanceCache(type, protocol);
  if (FoundConformance.isAuthoritative) {
    return FoundConformance.witnessTable;
  } else {
    C.cacheFailure(type, protocol);
    return nullptr;
  }
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
