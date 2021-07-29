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

#include "llvm/ADT/StringExtras.h"
#include "swift/ABI/TypeIdentity.h"
#include "swift/Basic/Lazy.h"
#include "swift/Basic/STLExtras.h"
#include "swift/Demangling/Demangle.h"
#include "swift/Runtime/Bincompat.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Basic/Unreachable.h"
#include "llvm/ADT/DenseMap.h"
#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "ImageInspection.h"
#include "Private.h"

#include <vector>

#if __has_include(<mach-o/dyld_priv.h>)
#include <mach-o/dyld_priv.h>
#define DYLD_EXPECTED_SWIFT_OPTIMIZATIONS_VERSION 1u
#endif

// Set this to 1 to enable logging of calls to the dyld shared cache conformance
// table
#if 0
#define SHARED_CACHE_LOG(fmt, ...)                                             \
  fprintf(stderr, "PROTOCOL CONFORMANCE: " fmt "\n", __VA_ARGS__)
#define SHARED_CACHE_LOG_ENABLED 1
#else
#define SHARED_CACHE_LOG(fmt, ...) (void)0
#endif

// Enable dyld shared cache acceleration only when it's available and we have
// ObjC interop.
#if DYLD_FIND_PROTOCOL_CONFORMANCE_DEFINED && SWIFT_OBJC_INTEROP
#define USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES 1
#endif

using namespace swift;

#ifndef NDEBUG
template <> SWIFT_USED void ProtocolDescriptor::dump() const {
  printf("TargetProtocolDescriptor.\n"
         "Name: \"%s\".\n",
         Name.get());
}

void ProtocolDescriptorFlags::dump() const {
  printf("ProtocolDescriptorFlags.\n");
  printf("Is Swift: %s.\n", (isSwift() ? "true" : "false"));
  printf("Needs Witness Table: %s.\n",
         (needsWitnessTable() ? "true" : "false"));
  printf("Is Resilient: %s.\n", (isResilient() ? "true" : "false"));
  printf("Special Protocol: %s.\n",
         (bool(getSpecialProtocol()) ? "Error" : "None"));
  printf("Class Constraint: %s.\n",
         (bool(getClassConstraint()) ? "Class" : "Any"));
  printf("Dispatch Strategy: %s.\n",
         (bool(getDispatchStrategy()) ? "Swift" : "ObjC"));
}

#endif

#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

template<> void ProtocolConformanceDescriptor::dump() const {
  auto symbolName = [&](const void *addr) -> const char * {
    SymbolInfo info;
    int ok = lookupSymbol(addr, &info);
    if (!ok)
      return "<unknown addr>";
    return info.symbolName.get();
  };

  switch (auto kind = getTypeKind()) {
  case TypeReferenceKind::DirectObjCClassName:
    printf("direct Objective-C class name %s", getDirectObjCClassName());
    break;

  case TypeReferenceKind::IndirectObjCClass:
    printf("indirect Objective-C class %s",
           class_getName(*getIndirectObjCClass()));
    break;

  case TypeReferenceKind::DirectTypeDescriptor:
  case TypeReferenceKind::IndirectTypeDescriptor:
    printf("unique nominal type descriptor %s", symbolName(getTypeDescriptor()));
    break;
  }
  
  printf(" => ");
  
  printf("witness table %pattern s\n", symbolName(getWitnessTablePattern()));
}
#endif

#ifndef NDEBUG
template <> SWIFT_USED void ProtocolConformanceDescriptor::verify() const {
  auto typeKind = unsigned(getTypeKind());
  assert(((unsigned(TypeReferenceKind::First_Kind) <= typeKind) &&
          (unsigned(TypeReferenceKind::Last_Kind) >= typeKind)) &&
         "Corrupted type metadata record kind");
}
#endif

#if SWIFT_OBJC_INTEROP
template <>
const ClassMetadata *TypeReference::getObjCClass(TypeReferenceKind kind) const {
  switch (kind) {
  case TypeReferenceKind::IndirectObjCClass:
    return *getIndirectObjCClass(kind);

  case TypeReferenceKind::DirectObjCClassName:
    return reinterpret_cast<const ClassMetadata *>(
              objc_lookUpClass(getDirectObjCClassName(kind)));

  case TypeReferenceKind::DirectTypeDescriptor:
  case TypeReferenceKind::IndirectTypeDescriptor:
    return nullptr;
  }

  swift_unreachable("Unhandled TypeReferenceKind in switch.");
}
#endif

static MetadataState
tryGetCompleteMetadataNonblocking(const Metadata *metadata) {
  return swift_checkMetadataState(
             MetadataRequest(MetadataState::Complete, /*isNonBlocking*/ true),
             metadata)
      .State;
}

/// Get the superclass of metadata, which may be incomplete. When the metadata
/// is not sufficiently complete, then we fall back to demangling the superclass
/// in the nominal type descriptor, which is slow but works. Return {NULL,
/// MetadataState::Complete} if the metadata is not a class, or has no
/// superclass.
///
/// If the metadata's current state is known, it may be passed in as
/// knownMetadataState. This saves the cost of retrieving that info separately.
///
/// When instantiateSuperclassMetadata is true, this function will instantiate
/// superclass metadata when necessary. When false, this will return {NULL,
/// MetadataState::Abstract} to indicate that there's an uninstantiated
/// superclass that was not returned.
static MetadataResponse getSuperclassForMaybeIncompleteMetadata(
    const Metadata *metadata, llvm::Optional<MetadataState> knownMetadataState,
    bool instantiateSuperclassMetadata) {
  const ClassMetadata *classMetadata = dyn_cast<ClassMetadata>(metadata);
  if (!classMetadata)
    return {_swift_class_getSuperclass(metadata), MetadataState::Complete};

#if SWIFT_OBJC_INTEROP
    // Artificial subclasses are not valid type metadata and
    // tryGetCompleteMetadataNonblocking will crash on them. However, they're
    // always fully set up, so we can just skip it and fetch the Subclass field.
    if (classMetadata->isTypeMetadata() && classMetadata->isArtificialSubclass())
      return {classMetadata->Superclass, MetadataState::Complete};

    // Pure ObjC classes are already set up, and the code below will not be
    // happy with them.
    if (!classMetadata->isTypeMetadata())
      return {classMetadata->Superclass, MetadataState::Complete};
#endif

  MetadataState metadataState;
  if (knownMetadataState)
    metadataState = *knownMetadataState;
  else
    metadataState = tryGetCompleteMetadataNonblocking(classMetadata);

  if (metadataState == MetadataState::Complete) {
    // The subclass metadata is complete. Fetch and return the superclass.
    auto *superMetadata = getMetadataForClass(classMetadata->Superclass);
    return {superMetadata, MetadataState::Complete};
  } else if (metadataState == MetadataState::NonTransitiveComplete) {
    // The subclass metadata is complete, but, unlike above, not transitively.
    // Its Superclass field is valid, so just read that field to get to the
    // superclass to proceed to the next step.
    auto *superMetadata = getMetadataForClass(classMetadata->Superclass);
    auto superState = tryGetCompleteMetadataNonblocking(superMetadata);
    return {superMetadata, superState};
  } else if (instantiateSuperclassMetadata) {
    // The subclass metadata is either LayoutComplete or Abstract, so the
    // Superclass field is not valid.  To get to the superclass, make the
    // expensive call to getSuperclassMetadata which demangles the superclass
    // name from the nominal type descriptor to get the metadata for the
    // superclass.
    MetadataRequest request(MetadataState::Abstract,
                            /*non-blocking*/ true);
    return getSuperclassMetadata(request, classMetadata);
  } else {
    // The Superclass field is not valid and the caller did not request
    // instantiation. Return a NULL superclass and Abstract to indicate that a
    // superclass exists but is not yet instantiated.
    return {nullptr, MetadataState::Abstract};
  }
}

struct MaybeIncompleteSuperclassIterator {
  const Metadata *metadata;
  llvm::Optional<MetadataState> state;
  bool instantiateSuperclassMetadata;

  MaybeIncompleteSuperclassIterator(const Metadata *metadata,
                                    bool instantiateSuperclassMetadata)
      : metadata(metadata), state(llvm::None),
        instantiateSuperclassMetadata(instantiateSuperclassMetadata) {}

  MaybeIncompleteSuperclassIterator &operator++() {
    auto response = getSuperclassForMaybeIncompleteMetadata(
        metadata, state, instantiateSuperclassMetadata);
    metadata = response.Value;
    state = response.State;
    return *this;
  }

  const Metadata *operator*() const { return metadata; }

  bool operator!=(const MaybeIncompleteSuperclassIterator rhs) const {
    return metadata != rhs.metadata;
  }
};

/// Return a range that will iterate over the given metadata and all its
/// superclasses in order. If the metadata is not a class, iteration will
/// provide that metadata and then stop.
iterator_range<MaybeIncompleteSuperclassIterator>
iterateMaybeIncompleteSuperclasses(const Metadata *metadata,
                                   bool instantiateSuperclassMetadata) {
  return iterator_range<MaybeIncompleteSuperclassIterator>(
      MaybeIncompleteSuperclassIterator(metadata,
                                        instantiateSuperclassMetadata),
      MaybeIncompleteSuperclassIterator(nullptr, false));
}

/// Take the type reference inside a protocol conformance record and fetch the
/// canonical metadata pointer for the type it refers to.
/// Returns nil for universal or generic type references.
template <>
const Metadata *
ProtocolConformanceDescriptor::getCanonicalTypeMetadata() const {
  switch (getTypeKind()) {
  case TypeReferenceKind::IndirectObjCClass:
  case TypeReferenceKind::DirectObjCClassName:
#if SWIFT_OBJC_INTEROP
    // The class may be ObjC, in which case we need to instantiate its Swift
    // metadata. The class additionally may be weak-linked, so we have to check
    // for null.
    if (auto cls = TypeRef.getObjCClass(getTypeKind()))
      return getMetadataForClass(cls);
#endif
    return nullptr;

  case TypeReferenceKind::DirectTypeDescriptor:
  case TypeReferenceKind::IndirectTypeDescriptor: {
    if (auto anyType = getTypeDescriptor()) {
      if (auto type = dyn_cast<TypeContextDescriptor>(anyType)) {
        if (!type->isGeneric()) {
          if (auto accessFn = type->getAccessFunction())
            return accessFn(MetadataState::Abstract).Value;
        }
      } else if (auto protocol = dyn_cast<ProtocolDescriptor>(anyType)) {
        return _getSimpleProtocolTypeMetadata(protocol);
      }
    }

    return nullptr;
  }
  }

  swift_unreachable("Unhandled TypeReferenceKind in switch.");
}

template<>
const WitnessTable *
ProtocolConformanceDescriptor::getWitnessTable(const Metadata *type) const {
  // If needed, check the conditional requirements.
  llvm::SmallVector<const void *, 8> conditionalArgs;
  if (hasConditionalRequirements()) {
    SubstGenericParametersFromMetadata substitutions(type);
    auto error = _checkGenericRequirements(
        getConditionalRequirements(), conditionalArgs,
        [&substitutions](unsigned depth, unsigned index) {
          return substitutions.getMetadata(depth, index);
        },
        [&substitutions](const Metadata *type, unsigned index) {
          return substitutions.getWitnessTable(type, index);
        });
    if (error)
      return nullptr;
  }

  return swift_getWitnessTable(this, type, conditionalArgs.data());
}

namespace {
  struct ConformanceSection {
    const ProtocolConformanceRecord *Begin, *End;

    ConformanceSection(const ProtocolConformanceRecord *begin,
                       const ProtocolConformanceRecord *end)
        : Begin(begin), End(end) {}

    ConformanceSection(const void *ptr, uintptr_t size) {
      auto bytes = reinterpret_cast<const char *>(ptr);
      Begin = reinterpret_cast<const ProtocolConformanceRecord *>(ptr);
      End = reinterpret_cast<const ProtocolConformanceRecord *>(bytes + size);
    }

    const ProtocolConformanceRecord *begin() const {
      return Begin;
    }
    const ProtocolConformanceRecord *end() const {
      return End;
    }
  };

  struct ConformanceCacheKey {
    const Metadata *Type;
    const ProtocolDescriptor *Proto;

    ConformanceCacheKey(const Metadata *type, const ProtocolDescriptor *proto)
        : Type(type), Proto(proto) {
      assert(type);
    }

    friend llvm::hash_code hash_value(const ConformanceCacheKey &key) {
      return llvm::hash_combine(key.Type, key.Proto);
    }
  };

  struct ConformanceCacheEntry {
  private:
    ConformanceCacheKey Key;
    const WitnessTable *Witness;

  public:
    ConformanceCacheEntry(ConformanceCacheKey key, const WitnessTable *witness)
        : Key(key), Witness(witness) {}

    bool matchesKey(const ConformanceCacheKey &key) const {
      return Key.Type == key.Type && Key.Proto == key.Proto;
    }

    friend llvm::hash_code hash_value(const ConformanceCacheEntry &entry) {
      return hash_value(entry.Key);
    }

    template <class... Args>
    static size_t getExtraAllocationSize(Args &&... ignored) {
      return 0;
    }

    /// Get the cached witness table, or null if we cached failure.
    const WitnessTable *getWitnessTable() const {
      return Witness;
    }
  };
} // end anonymous namespace

// Conformance Cache.
struct ConformanceState {
  ConcurrentReadableHashMap<ConformanceCacheEntry> Cache;
  ConcurrentReadableArray<ConformanceSection> SectionsToScan;
  bool scanSectionsBackwards;

#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  uintptr_t dyldSharedCacheStart;
  uintptr_t dyldSharedCacheEnd;
  bool hasOverriddenImage;
  bool validateSharedCacheResults;

  // Only populated when validateSharedCacheResults is enabled.
  ConcurrentReadableArray<ConformanceSection> SharedCacheSections;

  bool inSharedCache(const void *ptr) {
    auto uintPtr = reinterpret_cast<uintptr_t>(ptr);
    return dyldSharedCacheStart <= uintPtr && uintPtr < dyldSharedCacheEnd;
  }

  bool sharedCacheOptimizationsActive() { return dyldSharedCacheStart != 0; }
#else
  bool sharedCacheOptimizationsActive() { return false; }
#endif

  ConformanceState() {
    scanSectionsBackwards =
        runtime::bincompat::workaroundProtocolConformanceReverseIteration();

#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
    if (__builtin_available(macOS 12.0, iOS 15.0, tvOS 15.0, watchOS 8.0, *)) {
      if (runtime::environment::SWIFT_DEBUG_ENABLE_SHARED_CACHE_PROTOCOL_CONFORMANCES()) {
        if (&_dyld_swift_optimizations_version) {
          if (_dyld_swift_optimizations_version() ==
              DYLD_EXPECTED_SWIFT_OPTIMIZATIONS_VERSION) {
            size_t length;
            dyldSharedCacheStart =
                (uintptr_t)_dyld_get_shared_cache_range(&length);
            dyldSharedCacheEnd =
                dyldSharedCacheStart ? dyldSharedCacheStart + length : 0;
            validateSharedCacheResults = runtime::environment::
                SWIFT_DEBUG_VALIDATE_SHARED_CACHE_PROTOCOL_CONFORMANCES();
            SHARED_CACHE_LOG("Shared cache range is %#lx-%#lx",
                             dyldSharedCacheStart, dyldSharedCacheEnd);
          } else {
            SHARED_CACHE_LOG(
                "Disabling shared cache optimizations due to unknown "
                "optimizations version %u",
                _dyld_swift_optimizations_version());
            dyldSharedCacheStart = 0;
            dyldSharedCacheEnd = 0;
          }
        }
      }
    }
#endif

    // This must run last, as it triggers callbacks that require
    // ConformanceState to be set up.
    initializeProtocolConformanceLookup();
  }

  void cacheResult(const Metadata *type, const ProtocolDescriptor *proto,
                   const WitnessTable *witness, size_t sectionsCount) {
    Cache.getOrInsert(ConformanceCacheKey(type, proto),
                      [&](ConformanceCacheEntry *entry, bool created) {
                        // Create the entry if needed. If it already exists,
                        // we're done.
                        if (!created)
                          return false;

                        // Check the current sections count against what was
                        // passed in. If a section count was passed in and they
                        // don't match, then this is not an authoritative entry
                        // and it may have been obsoleted, because the new
                        // sections could contain a conformance in a more
                        // specific type.
                        //
                        // If they DO match, then we can safely add. Another
                        // thread might be adding new sections at this point,
                        // but we will not race with them. That other thread
                        // will add the new sections, then clear the cache. When
                        // it clears the cache, it will block waiting for this
                        // code to complete and relinquish Cache's writer lock.
                        // If we cache a stale entry, it will be immediately
                        // cleared.
                        if (sectionsCount > 0 &&
                            SectionsToScan.snapshot().count() != sectionsCount)
                          return false; // abandon the new entry

                        new (entry) ConformanceCacheEntry(
                            ConformanceCacheKey(type, proto), witness);
                        return true; // keep the new entry
                      });
  }

#ifndef NDEBUG
  void verify() const SWIFT_USED;
#endif
};

#ifndef NDEBUG
void ConformanceState::verify() const {
  // Iterate over all of the sections and verify all of the protocol
  // descriptors.
  auto &Self = const_cast<ConformanceState &>(*this);
  for (const auto &Section : Self.SectionsToScan.snapshot()) {
    for (const auto &Record : Section) {
      Record.get()->verify();
    }
  }
}
#endif

static Lazy<ConformanceState> Conformances;

const void * const swift::_swift_debug_protocolConformanceStatePointer =
  &Conformances;

static void _registerProtocolConformances(ConformanceState &C,
                                          ConformanceSection section) {
  C.SectionsToScan.push_back(section);

  // Blow away the conformances cache to get rid of any negative entries that
  // may now be obsolete.
  C.Cache.clear();
}

void swift::addImageProtocolConformanceBlockCallbackUnsafe(
    const void *conformances, uintptr_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0 &&
         "conformances section not a multiple of ProtocolConformanceRecord");

  // Conformance cache should always be sufficiently initialized by this point.
  auto &C = Conformances.unsafeGetAlreadyInitialized();

#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  // If any image in the shared cache is overridden, we need to scan all
  // conformance sections in the shared cache. The pre-built table does NOT work
  // if the protocol, type, or descriptor are in overridden images. Example:
  //
  // libX.dylib: struct S {}
  // libY.dylib: protocol P {}
  // libZ.dylib: extension S: P {}
  //
  // If libX or libY are overridden, then dyld will not return the S: P
  // conformance from libZ. But that conformance still exists and we must still
  // return it! Therefore we must scan libZ (and all other dylibs) even though
  // it is not overridden.
  if (!dyld_shared_cache_some_image_overridden()) {
    // Sections in the shared cache are ignored in favor of the shared cache's
    // pre-built tables.
    if (C.inSharedCache(conformances)) {
      SHARED_CACHE_LOG("Skipping conformances section %p in the shared cache",
                       conformances);
      if (C.validateSharedCacheResults)
        C.SharedCacheSections.push_back(
            ConformanceSection{conformances, conformancesSize});
      return;
    } else {
      SHARED_CACHE_LOG(
          "Adding conformances section %p outside the shared cache",
          conformances);
    }
  }
#endif

  // If we have a section, enqueue the conformances for lookup.
  _registerProtocolConformances(
      C, ConformanceSection{conformances, conformancesSize});
}

void swift::addImageProtocolConformanceBlockCallback(
    const void *conformances, uintptr_t conformancesSize) {
  Conformances.get();
  addImageProtocolConformanceBlockCallbackUnsafe(conformances,
                                                 conformancesSize);
}

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  auto &C = Conformances.get();
  _registerProtocolConformances(C, ConformanceSection{begin, end});
}

/// Search for a conformance descriptor in the ConformanceCache.
/// First element of the return value is `true` if the result is authoritative
/// i.e. the result is for the type itself and not a superclass. If `false`
/// then we cached a conformance on a superclass, but that may be overridden.
/// A return value of `{ false, nullptr }` indicates nothing was cached.
static std::pair<bool, const WitnessTable *>
searchInConformanceCache(const Metadata *type,
                         const ProtocolDescriptor *protocol,
                         bool instantiateSuperclassMetadata) {
  auto &C = Conformances.get();
  auto origType = type;
  auto snapshot = C.Cache.snapshot();

  for (auto type : iterateMaybeIncompleteSuperclasses(
           type, instantiateSuperclassMetadata)) {
    if (auto *Value = snapshot.find(ConformanceCacheKey(type, protocol))) {
      return {type == origType, Value->getWitnessTable()};
    }
  }

  // We did not find a cache entry.
  return {false, nullptr};
}

/// Get the appropriate context descriptor for a type. If the descriptor is a
/// foreign type descriptor, also return its identity string.
static std::pair<const ContextDescriptor *, llvm::StringRef>
getContextDescriptor(const Metadata *conformingType) {
  const auto *description = conformingType->getTypeContextDescriptor();
  if (description) {
    if (description->hasForeignMetadataInitialization()) {
      auto identity = ParsedTypeIdentity::parse(description).FullIdentity;
      return {description, identity};
    }
    return {description, {}};
  }

  // Handle single-protocol existential types for self-conformance.
  auto *existentialType = dyn_cast<ExistentialTypeMetadata>(conformingType);
  if (existentialType == nullptr ||
      existentialType->getProtocols().size() != 1 ||
      existentialType->getSuperclassConstraint() != nullptr)
    return {nullptr, {}};

  auto proto = existentialType->getProtocols()[0];

#if SWIFT_OBJC_INTEROP
  if (proto.isObjC())
    return {nullptr, {}};
#endif

  return {proto.getSwiftProtocol(), {}};
}

namespace {
  /// Describes a protocol conformance "candidate" that can be checked
  /// against a type metadata.
  class ConformanceCandidate {
    const void *candidate;
    bool candidateIsMetadata;

  public:
    ConformanceCandidate() : candidate(0), candidateIsMetadata(false) { }

    ConformanceCandidate(const ProtocolConformanceDescriptor &conformance)
      : ConformanceCandidate()
    {
      if (auto description = conformance.getTypeDescriptor()) {
        candidate = description;
        candidateIsMetadata = false;
        return;
      }

      if (auto metadata = conformance.getCanonicalTypeMetadata()) {
        candidate = metadata;
        candidateIsMetadata = true;
        return;
      }
    }

    /// Whether the conforming type exactly matches the conformance candidate.
    bool matches(const Metadata *conformingType) const {
      // Check whether the types match.
      if (candidateIsMetadata && conformingType == candidate)
        return true;

      // Check whether the nominal type descriptors match.
      if (!candidateIsMetadata) {
        const auto *description = std::get<const ContextDescriptor *>(
            getContextDescriptor(conformingType));
        auto candidateDescription =
          static_cast<const ContextDescriptor *>(candidate);
        if (description && equalContexts(description, candidateDescription))
          return true;
      }

      return false;
    }

    /// Retrieve the type that matches the conformance candidate, which may
    /// be a superclass of the given type. Returns null if this type does not
    /// match this conformance.
    const Metadata *getMatchingType(const Metadata *conformingType,
                                    bool instantiateSuperclassMetadata) const {
      for (auto conformingType : iterateMaybeIncompleteSuperclasses(
               conformingType, instantiateSuperclassMetadata)) {
        if (matches(conformingType))
          return conformingType;
      }

      return nullptr;
    }
  };
}

static void validateSharedCacheResults(
    ConformanceState &C, const Metadata *type,
    const ProtocolDescriptor *protocol,
    const WitnessTable *dyldCachedWitnessTable,
    const ProtocolConformanceDescriptor *dyldCachedConformanceDescriptor,
    bool instantiateSuperclassMetadata) {
#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  if (!C.sharedCacheOptimizationsActive() || !C.validateSharedCacheResults)
    return;

  llvm::SmallVector<const ProtocolConformanceDescriptor *, 8> conformances;
  for (auto &section : C.SharedCacheSections.snapshot()) {
    for (const auto &record : section) {
      auto &descriptor = *record.get();
      if (descriptor.getProtocol() != protocol)
        continue;

      ConformanceCandidate candidate(descriptor);
      if (candidate.getMatchingType(type, instantiateSuperclassMetadata))
        conformances.push_back(&descriptor);
    }
  }

  auto conformancesString = [&]() -> std::string {
    std::string result = "";
    for (auto *conformance : conformances) {
      if (!result.empty())
        result += ", ";
      result += "0x";
      result += llvm::utohexstr(reinterpret_cast<uint64_t>(conformance));
    }
    return result;
  };

  if (dyldCachedConformanceDescriptor) {
    if (std::find(conformances.begin(), conformances.end(),
                  dyldCachedConformanceDescriptor) == conformances.end()) {
      auto typeName = swift_getTypeName(type, true);
      swift::fatalError(
          0,
          "Checking conformance of %.*s %p to %s %p - dyld cached conformance "
          "descriptor %p not found in conformance records: (%s)\n",
          (int)typeName.length, typeName.data, type, protocol->Name.get(),
          protocol, dyldCachedConformanceDescriptor,
          conformancesString().c_str());
    }
  } else {
    if (!conformances.empty()) {
      auto typeName = swift_getTypeName(type, true);
      swift::fatalError(
          0,
          "Checking conformance of %.*s %p to %s %p - dyld found no "
          "conformance descriptor, but matching descriptors exist: (%s)\n",
          (int)typeName.length, typeName.data, type, protocol->Name.get(),
          protocol, conformancesString().c_str());
    }
  }
#endif
}

/// Query the shared cache for a protocol conformance, if supported. The return
/// value is a tuple consisting of the found witness table (if any), the found
/// conformance descriptor (if any), and a bool that's true if a failure is
/// definitive.
static std::tuple<const WitnessTable *, const ProtocolConformanceDescriptor *,
                  bool>
findSharedCacheConformance(ConformanceState &C, const Metadata *type,
                           const ProtocolDescriptor *protocol,
                           bool instantiateSuperclassMetadata) {
#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  const ContextDescriptor *description;
  llvm::StringRef foreignTypeIdentity;
  std::tie(description, foreignTypeIdentity) = getContextDescriptor(type);

  // dyld expects the ObjC class, if any, as the second parameter.
  auto objcClassMetadata = swift_getObjCClassFromMetadataConditional(type);
#if SHARED_CACHE_LOG_ENABLED
  auto typeName = swift_getTypeName(type, true);
  SHARED_CACHE_LOG("Looking up conformance of %.*s to %s", (int)typeName.length,
                   typeName.data, protocol->Name.get());
#endif
  _dyld_protocol_conformance_result dyldResult;
  if (!foreignTypeIdentity.empty()) {
    SHARED_CACHE_LOG(
        "_dyld_find_foreign_type_protocol_conformance(%p, %.*s, %zu)", protocol,
        (int)foreignTypeIdentity.size(), foreignTypeIdentity.data(),
        foreignTypeIdentity.size());
    dyldResult = _dyld_find_foreign_type_protocol_conformance(
        protocol, foreignTypeIdentity.data(), foreignTypeIdentity.size());
  } else {
    SHARED_CACHE_LOG("_dyld_find_protocol_conformance(%p, %p, %p)", protocol,
                     objcClassMetadata, description);
    dyldResult = _dyld_find_protocol_conformance(protocol, objcClassMetadata,
                                                 description);
  }
  switch (dyldResult.kind) {
  case _dyld_protocol_conformance_result_kind_found_descriptor: {
    auto *conformanceDescriptor =
        reinterpret_cast<const ProtocolConformanceDescriptor *>(
            dyldResult.value);

    assert(conformanceDescriptor->getProtocol() == protocol);
    assert(ConformanceCandidate{*conformanceDescriptor}.getMatchingType(
        type, instantiateSuperclassMetadata));

    if (conformanceDescriptor->getGenericWitnessTable()) {
      SHARED_CACHE_LOG(
          "Found generic conformance descriptor %p for %s in shared "
          "cache, continuing",
          conformanceDescriptor, protocol->Name.get());
      return std::make_tuple(nullptr, conformanceDescriptor, false);
    } else {
      // When there are no generics, we can retrieve the witness table cheaply,
      // so do it up front.
      SHARED_CACHE_LOG("Found conformance descriptor %p for %s in shared cache",
                       conformanceDescriptor, protocol->Name.get());
      auto *witnessTable = conformanceDescriptor->getWitnessTable(type);
      return std::make_tuple(witnessTable, conformanceDescriptor, false);
    }
    break;
  }
  case _dyld_protocol_conformance_result_kind_found_witness_table:
    // If we found a witness table then we're done.
    SHARED_CACHE_LOG(
        "Found witness table %p for conformance to %s in shared cache",
        dyldResult.value, protocol->Name.get());
    return std::make_tuple(reinterpret_cast<const WitnessTable *>(dyldResult.value), nullptr,
            false);
  case _dyld_protocol_conformance_result_kind_not_found:
    // If nothing is found, then we'll proceed with checking the runtime's
    // caches and scanning conformance records.
    SHARED_CACHE_LOG("Conformance to %s not found in shared cache",
                     protocol->Name.get());
    return std::make_tuple(nullptr, nullptr, false);
    break;
  case _dyld_protocol_conformance_result_kind_definitive_failure:
    // This type is known not to conform to this protocol. Return failure
    // without any further checks.
    SHARED_CACHE_LOG("Found definitive failure for %s in shared cache",
                     protocol->Name.get());
    return std::make_tuple(nullptr, nullptr, true);
  default:
    // Other values may be added. Consider them equivalent to not_found until
    // we implement code to handle them.
    SHARED_CACHE_LOG(
        "Unknown result kind %lu from _dyld_find_protocol_conformance()",
        (unsigned long)dyldResult.kind);
    return std::make_tuple(nullptr, nullptr, false);
  }
#else
  return std::make_tuple(nullptr, nullptr, false);
#endif
}

/// Check if a type conforms to a protocol, possibly instantiating superclasses
/// that have not yet been instantiated. The return value is a pair consisting
/// of the witness table for the conformance (or NULL if no conformance was
/// found), and a boolean indicating whether there are uninstantiated
/// superclasses that were not searched.
static std::pair<const WitnessTable *, bool>
swift_conformsToProtocolMaybeInstantiateSuperclasses(
    const Metadata *const type, const ProtocolDescriptor *protocol,
    bool instantiateSuperclassMetadata) {
  auto &C = Conformances.get();

  const WitnessTable *dyldCachedWitnessTable = nullptr;
  const ProtocolConformanceDescriptor *dyldCachedConformanceDescriptor =
      nullptr;

  // Search the shared cache tables for a conformance for this type, and for
  // superclasses (if it's a class).
  if (C.sharedCacheOptimizationsActive()) {
    for (auto dyldSearchType : iterateMaybeIncompleteSuperclasses(
             type, instantiateSuperclassMetadata)) {
      bool definitiveFailure;
      std::tie(dyldCachedWitnessTable, dyldCachedConformanceDescriptor,
               definitiveFailure) =
          findSharedCacheConformance(C, dyldSearchType, protocol,
                                     instantiateSuperclassMetadata);

      if (definitiveFailure)
        return {nullptr, false};

      if (dyldCachedWitnessTable || dyldCachedConformanceDescriptor)
        break;
    }

    validateSharedCacheResults(C, type, protocol, dyldCachedWitnessTable,
                               dyldCachedConformanceDescriptor,
                               instantiateSuperclassMetadata);
    // Return a cached result if we got a witness table. We can't do this if
    // scanSectionsBackwards is set, since a scanned conformance can override a
    // cached result in that case.
    if (!C.scanSectionsBackwards)
      if (dyldCachedWitnessTable)
        return {dyldCachedWitnessTable, false};
  }

  // See if we have an authoritative cached conformance. The
  // ConcurrentReadableHashMap data structure allows us to search the map
  // concurrently without locking.
  auto found =
      searchInConformanceCache(type, protocol, instantiateSuperclassMetadata);
  if (found.first) {
    // An authoritative negative result can be overridden by a result from dyld.
    if (!found.second) {
      if (dyldCachedWitnessTable)
        return {dyldCachedWitnessTable, false};
    }
    return {found.second, false};
  }

  if (dyldCachedConformanceDescriptor) {
    ConformanceCandidate candidate(*dyldCachedConformanceDescriptor);
    auto *matchingType =
        candidate.getMatchingType(type, instantiateSuperclassMetadata);
    assert(matchingType);
    auto witness = dyldCachedConformanceDescriptor->getWitnessTable(matchingType);
    C.cacheResult(type, protocol, witness, /*always cache*/ 0);
    SHARED_CACHE_LOG("Caching generic conformance to %s found in shared cache",
                     protocol->Name.get());
    return {witness, false};
  }

  // Scan conformance records.
  llvm::SmallDenseMap<const Metadata *, const WitnessTable *> foundWitnesses;
  auto processSection = [&](const ConformanceSection &section) {
    // Eagerly pull records for nondependent witnesses into our cache.
    auto processDescriptor = [&](const ProtocolConformanceDescriptor &descriptor) {
      // We only care about conformances for this protocol.
      if (descriptor.getProtocol() != protocol)
        return;

      // If there's a matching type, record the positive result and return it.
      // The matching type is exact, so they can't go stale, and we should
      // always cache them.
      ConformanceCandidate candidate(descriptor);
      if (auto *matchingType =
              candidate.getMatchingType(type, instantiateSuperclassMetadata)) {
        auto witness = descriptor.getWitnessTable(matchingType);
        C.cacheResult(matchingType, protocol, witness, /*always cache*/ 0);
        foundWitnesses.insert({matchingType, witness});
      }
    };

    if (C.scanSectionsBackwards) {
      for (const auto &record : llvm::reverse(section))
        processDescriptor(*record.get());
    } else {
      for (const auto &record : section)
        processDescriptor(*record.get());
    }
  };

  auto snapshot = C.SectionsToScan.snapshot();
  if (C.scanSectionsBackwards) {
    for (auto &section : llvm::reverse(snapshot))
      processSection(section);
  } else {
    for (auto &section : snapshot)
      processSection(section);
  }

  // Find the most specific conformance that was scanned.
  const WitnessTable *foundWitness = nullptr;
  const Metadata *foundType = nullptr;

  // Use MaybeIncompleteSuperclassIterator directly so we can examine its final
  // state. Complete indicates that we finished normally, Abstract indicates
  // that there's an uninstantiated superclass we didn't iterate over.
  MaybeIncompleteSuperclassIterator superclassIterator{
      type, instantiateSuperclassMetadata};
  for (; auto searchType = superclassIterator.metadata; ++superclassIterator) {
    const WitnessTable *witness = foundWitnesses.lookup(searchType);
    if (witness) {
      if (!foundType) {
        foundWitness = witness;
        foundType = searchType;
      } else {
        swift::warning(RuntimeErrorFlagNone,
                       "Warning: '%s' conforms to protocol '%s', but it also "
                       "inherits conformance from '%s'.  Relying on a "
                       "particular conformance is undefined behaviour.\n",
                       foundType->getDescription()->Name.get(),
                       protocol->Name.get(),
                       searchType->getDescription()->Name.get());
      }
    }
  }

  // Do not cache negative results if there were uninstantiated superclasses we
  // didn't search. They might have a conformance that will be found later.
  bool hasUninstantiatedSuperclass =
      superclassIterator.state == MetadataState::Abstract;

  // If it's for a superclass or if we didn't find anything, then add an
  // authoritative entry for this type.
  if (foundType != type)
    if (foundWitness || !hasUninstantiatedSuperclass)
      C.cacheResult(type, protocol, foundWitness, snapshot.count());

  // A negative result can be overridden by a result from dyld.
  if (!foundWitness) {
    if (dyldCachedWitnessTable)
      return {dyldCachedWitnessTable, false};
  }
  return {foundWitness, hasUninstantiatedSuperclass};
}

static const WitnessTable *
swift_conformsToProtocolImpl(const Metadata *const type,
                             const ProtocolDescriptor *protocol) {
  const WitnessTable *table;
  bool hasUninstantiatedSuperclass;

  // First, try without instantiating any new superclasses. This avoids
  // an infinite loop for cases like `class Sub: Super<Sub>`. In cases like
  // that, the conformance must exist on the subclass (or at least somewhere
  // in the chain before we get to an uninstantiated superclass) so this search
  // will succeed without trying to instantiate Super while it's already being
  // instantiated.=
  std::tie(table, hasUninstantiatedSuperclass) =
      swift_conformsToProtocolMaybeInstantiateSuperclasses(
          type, protocol, false /*instantiateSuperclassMetadata*/);

  // If no conformance was found, and there is an uninstantiated superclass that
  // was not searched, then try the search again and instantiate all
  // superclasses.
  if (!table && hasUninstantiatedSuperclass)
    std::tie(table, hasUninstantiatedSuperclass) =
        swift_conformsToProtocolMaybeInstantiateSuperclasses(
            type, protocol, true /*instantiateSuperclassMetadata*/);
  return table;
}

const ContextDescriptor *
swift::_searchConformancesByMangledTypeName(Demangle::NodePointer node) {
  auto &C = Conformances.get();

  for (auto &section : C.SectionsToScan.snapshot()) {
    for (const auto &record : section) {
      if (auto ntd = record->getTypeDescriptor()) {
        if (_contextDescriptorMatchesMangling(ntd, node))
          return ntd;
      }
    }
  }
  return nullptr;
}

template <typename HandleObjc>
bool isSwiftClassMetadataSubclass(const ClassMetadata *subclass,
                                  const ClassMetadata *superclass,
                                  HandleObjc handleObjc) {
  assert(subclass);
  assert(superclass);

  llvm::Optional<MetadataState> subclassState = llvm::None;
  while (true) {
    auto response = getSuperclassForMaybeIncompleteMetadata(
        subclass, subclassState, true /*instantiateSuperclassMetadata*/);
    if (response.Value == superclass)
      return true;
    if (!response.Value)
      return false;

    subclass = dyn_cast<ClassMetadata>(response.Value);
    if (!subclass || subclass->isPureObjC())
      return handleObjc(response.Value, superclass);
  }
}

// Whether the provided `subclass` is metadata for a subclass* of the superclass
// whose metadata is specified.
//
// The function is robust against incomplete metadata for both subclass and
// superclass.  In the worst case, each intervening class between subclass and
// superclass is demangled.  Besides that slow path, there are a number of fast
// paths:
// - both classes are ObjC: swift_dynamicCastMetatype
// - Complete subclass metadata: loop over Superclass fields
// - NonTransitiveComplete: read the Superclass field once
//
// * A non-strict subclass; that is, given a class X, isSubclass(X.self, X.self)
//   is true.
static bool isSubclass(const Metadata *subclass, const Metadata *superclass) {
  assert(subclass);
  assert(superclass);
  assert(subclass->isAnyClass());
  assert(superclass->isAnyClass());

  if (subclass == superclass)
    return true;
  if (!isa<ClassMetadata>(subclass)) {
    if (!isa<ClassMetadata>(superclass)) {
      // Only ClassMetadata can be incomplete; when the class metadata is not
      // ClassMetadata, just use swift_dynamicCastMetatype.
      return swift_dynamicCastMetatype(subclass, superclass);
    } else {
      // subclass is ObjC, but superclass is not; since it is not possible for
      // any ObjC class to be a subclass of any Swift class, this subclass is
      // not a subclass of this superclass.
      return false;
    }
  }
  const ClassMetadata *swiftSubclass = cast<ClassMetadata>(subclass);
  if (auto *objcSuperclass = dyn_cast<ObjCClassWrapperMetadata>(superclass)) {
    // Walk up swiftSubclass's ancestors until we get to an ObjC class, then
    // kick over to swift_dynamicCastMetatype.
    return isSwiftClassMetadataSubclass(
        swiftSubclass, objcSuperclass->Class,
        [](const Metadata *intermediate, const Metadata *superclass) {
          // Intermediate is an ObjC class, and superclass is an ObjC class;
          // as above, just use swift_dynamicCastMetatype.
          return swift_dynamicCastMetatype(intermediate, superclass);
        });
    return false;
  }
  if (isa<ForeignClassMetadata>(superclass)) {
    // superclass is foreign, but subclass is not (if it were, the above
    // !isa<ClassMetadata> condition would have been entered).  Since it is not
    // possible for any Swift class to be a subclass of any foreign superclass,
    // this subclass is not a subclass of this superclass.
    return false;
  }
  auto swiftSuperclass = cast<ClassMetadata>(superclass);
  return isSwiftClassMetadataSubclass(swiftSubclass, swiftSuperclass,
                                      [](const Metadata *, const Metadata *) {
                                        // Because (1) no ObjC classes inherit
                                        // from Swift classes and (2)
                                        // `superclass` is not ObjC, if some
                                        // ancestor of `subclass` is ObjC, then
                                        // `subclass` cannot descend from
                                        // `superclass` (otherwise at some point
                                        // some ObjC class would have to inherit
                                        // from a Swift class).
                                        return false;
                                      });
}

llvm::Optional<TypeLookupError> swift::_checkGenericRequirements(
    llvm::ArrayRef<GenericRequirementDescriptor> requirements,
    llvm::SmallVectorImpl<const void *> &extraArguments,
    SubstGenericParameterFn substGenericParam,
    SubstDependentWitnessTableFn substWitnessTable) {
  for (const auto &req : requirements) {
    // Make sure we understand the requirement we're dealing with.
    if (!req.hasKnownKind())
      return TypeLookupError("unknown kind");

    // Resolve the subject generic parameter.
    auto result = swift_getTypeByMangledName(
        MetadataState::Abstract, req.getParam(), extraArguments.data(),
        substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    const Metadata *subjectType = result.getType().getMetadata();

    // Check the requirement.
    switch (req.getKind()) {
    case GenericRequirementKind::Protocol: {
      const WitnessTable *witnessTable = nullptr;
      if (!_conformsToProtocol(nullptr, subjectType, req.getProtocol(),
                               &witnessTable)) {
        const char *protoName =
            req.getProtocol() ? req.getProtocol().getName() : "<null>";
        return TYPE_LOOKUP_ERROR_FMT(
            "subject type %.*s does not conform to protocol %s",
            (int)req.getParam().size(), req.getParam().data(), protoName);
      }

      // If we need a witness table, add it.
      if (req.getProtocol().needsWitnessTable()) {
        assert(witnessTable);
        extraArguments.push_back(witnessTable);
      }

      continue;
    }

    case GenericRequirementKind::SameType: {
      // Demangle the second type under the given substitutions.
      auto result = swift_getTypeByMangledName(
          MetadataState::Abstract, req.getMangledTypeName(),
          extraArguments.data(), substGenericParam, substWitnessTable);
      if (result.getError())
        return *result.getError();
      auto otherType = result.getType().getMetadata();

      assert(!req.getFlags().hasExtraArgument());

      // Check that the types are equivalent.
      if (subjectType != otherType)
        return TYPE_LOOKUP_ERROR_FMT(
            "subject type %.*s does not match %.*s", (int)req.getParam().size(),
            req.getParam().data(), (int)req.getMangledTypeName().size(),
            req.getMangledTypeName().data());

      continue;
    }

    case GenericRequirementKind::Layout: {
      switch (req.getLayout()) {
      case GenericRequirementLayoutKind::Class:
        if (!subjectType->satisfiesClassConstraint())
          return TYPE_LOOKUP_ERROR_FMT(
              "subject type %.*s does not satisfy class constraint",
              (int)req.getParam().size(), req.getParam().data());
        continue;
      }

      // Unknown layout.
      return TYPE_LOOKUP_ERROR_FMT("unknown layout kind %u", req.getLayout());
    }

    case GenericRequirementKind::BaseClass: {
      // Demangle the base type under the given substitutions.
      auto result = swift_getTypeByMangledName(
          MetadataState::Abstract, req.getMangledTypeName(),
          extraArguments.data(), substGenericParam, substWitnessTable);
      if (result.getError())
        return *result.getError();
      auto baseType = result.getType().getMetadata();

      // If the type which is constrained to a base class is an existential 
      // type, and if that existential type includes a superclass constraint,
      // just require that the superclass by which the existential is
      // constrained is a subclass of the base class.
      if (auto *existential = dyn_cast<ExistentialTypeMetadata>(subjectType)) {
        if (auto *superclassConstraint = existential->getSuperclassConstraint())
          subjectType = superclassConstraint;
      }

      if (!isSubclass(subjectType, baseType))
        return TYPE_LOOKUP_ERROR_FMT(
            "%.*s is not subclass of %.*s", (int)req.getParam().size(),
            req.getParam().data(), (int)req.getMangledTypeName().size(),
            req.getMangledTypeName().data());

      continue;
    }

    case GenericRequirementKind::SameConformance: {
      // FIXME: Implement this check.
      continue;
    }
    }

    // Unknown generic requirement kind.
    return TYPE_LOOKUP_ERROR_FMT("unknown generic requirement kind %u",
                                 (unsigned)req.getKind());
  }

  // Success!
  return llvm::None;
}

const Metadata *swift::findConformingSuperclass(
                            const Metadata *type,
                            const ProtocolConformanceDescriptor *conformance) {
  // Figure out which type we're looking for.
  ConformanceCandidate candidate(*conformance);

  const Metadata *conformingType =
      candidate.getMatchingType(type, true /*instantiateSuperclassMetadata*/);
  assert(conformingType);
  return conformingType;
}

#define OVERRIDE_PROTOCOLCONFORMANCE COMPATIBILITY_OVERRIDE
#include COMPATIBILITY_OVERRIDE_INCLUDE_PATH
