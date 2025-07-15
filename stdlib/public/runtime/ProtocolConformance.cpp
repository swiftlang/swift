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
#include "llvm/ADT/PointerUnion.h"
#include "../CompatibilityOverride/CompatibilityOverride.h"
#include "ImageInspection.h"
#include "Private.h"
#include "Tracing.h"

#include <new>
#include <vector>

#if __has_include(<mach-o/dyld_priv.h>)
#include <mach-o/dyld_priv.h>
#define DYLD_EXPECTED_SWIFT_OPTIMIZATIONS_VERSION 1u

// Redeclare these functions as weak so we can build against a macOS 12 SDK and
// still test on macOS 11.
LLVM_ATTRIBUTE_WEAK
struct _dyld_protocol_conformance_result
_dyld_find_protocol_conformance(const void *protocolDescriptor,
                                const void *metadataType,
                                const void *typeDescriptor);

LLVM_ATTRIBUTE_WEAK
struct _dyld_protocol_conformance_result
_dyld_find_foreign_type_protocol_conformance(const void *protocol,
                                             const char *foreignTypeIdentityStart,
                                             size_t foreignTypeIdentityLength);

LLVM_ATTRIBUTE_WEAK
uint32_t _dyld_swift_optimizations_version(void);

#if DYLD_FIND_PROTOCOL_ON_DISK_CONFORMANCE_DEFINED
// Redeclare these functions as weak as well.
LLVM_ATTRIBUTE_WEAK bool _dyld_has_preoptimized_swift_protocol_conformances(
    const struct mach_header *mh);

LLVM_ATTRIBUTE_WEAK struct _dyld_protocol_conformance_result
_dyld_find_protocol_conformance_on_disk(const void *protocolDescriptor,
                                        const void *metadataType,
                                        const void *typeDescriptor,
                                        uint32_t flags);

LLVM_ATTRIBUTE_WEAK struct _dyld_protocol_conformance_result
_dyld_find_foreign_type_protocol_conformance_on_disk(
    const void *protocol, const char *foreignTypeIdentityStart,
    size_t foreignTypeIdentityLength, uint32_t flags);
#endif // DYLD_FIND_PROTOCOL_ON_DISK_CONFORMANCE_DEFINED

#endif // __has_include(<mach-o/dyld_priv.h>)

// Set this to 1 to enable logging of calls to the dyld shared cache conformance
// table
#if 0
#define DYLD_CONFORMANCES_LOG(fmt, ...)                                        \
  fprintf(stderr, "PROTOCOL CONFORMANCE: " fmt "\n", __VA_ARGS__)
#define SHARED_CACHE_LOG_ENABLED 1
#else
#define DYLD_CONFORMANCES_LOG(fmt, ...) (void)0
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

static bool IsDebugLog() {
#ifndef NDEBUG
  return runtime::environment::
      SWIFT_DEBUG_ENABLE_PROTOCOL_CONFORMANCES_LOOKUP_LOG();
#else
  return false;
#endif
}

#if !defined(NDEBUG) && SWIFT_OBJC_INTEROP
#include <objc/runtime.h>

static const char *class_getName(const ClassMetadata* type) {
  return class_getName(
    reinterpret_cast<Class>(const_cast<ClassMetadata*>(type)));
}

template<> void ProtocolConformanceDescriptor::dump() const {
  std::optional<SymbolInfo> info;
  auto symbolName = [&](const void *addr) -> const char * {
    info = SymbolInfo::lookup(addr);
    if (info.has_value() && info->getSymbolName()) {
      return info->getSymbolName();
    }
    return "<unknown addr>";
  };

  switch (getTypeKind()) {
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
  
  printf("witness table pattern (%p) %s\n", getWitnessTablePattern(), symbolName(getWitnessTablePattern()));
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
    const Metadata *metadata, std::optional<MetadataState> knownMetadataState,
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
  std::optional<MetadataState> state;
  bool instantiateSuperclassMetadata;

  MaybeIncompleteSuperclassIterator(const Metadata *metadata,
                                    bool instantiateSuperclassMetadata)
      : metadata(metadata), state(std::nullopt),
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

namespace {

/// Describes the result of looking in the conformance cache.
struct ConformanceLookupResult {
  /// The actual witness table, which will be NULL if the type does not
  /// conform.
  const WitnessTable *witnessTable = nullptr;

  /// The global actor to which this conformance is isolated, or NULL for
  /// a nonisolated conformances.
  const Metadata *globalActorIsolationType = nullptr;

  /// When the conformance is global-actor-isolated, this is the conformance
  /// of globalActorIsolationType to GlobalActor.
  const WitnessTable *globalActorIsolationWitnessTable = nullptr;

  ConformanceLookupResult() { }

  ConformanceLookupResult(std::nullptr_t) { }

  ConformanceLookupResult(const WitnessTable *witnessTable,
                          const Metadata *globalActorIsolationType,
                          const WitnessTable *globalActorIsolationWitnessTable)
    : witnessTable(witnessTable),
      globalActorIsolationType(globalActorIsolationType),
      globalActorIsolationWitnessTable(globalActorIsolationWitnessTable) { }

  explicit operator bool() const { return witnessTable != nullptr; }

  /// Given a type and conformance descriptor, form a conformance lookup
  /// result.
  static ConformanceLookupResult fromConformance(
      const Metadata *type,
      const ProtocolConformanceDescriptor *conformanceDescriptor);
};

}

/// Determine the global actor isolation for the given witness table.
///
/// Returns true if an error occurred, false if global actor isolation was
/// successfully computed (which can mean "not isolated").
static bool _checkWitnessTableIsolation(
  const Metadata *type,
  const WitnessTable *wtable,
  llvm::ArrayRef<const void *> conditionalArgs,
  ConformanceExecutionContext &context
);

template<>
const WitnessTable *
ProtocolConformanceDescriptor::getWitnessTable(
    const Metadata *type,
    ConformanceExecutionContext &context
) const {
  // If needed, check the conditional requirements.
  llvm::SmallVector<const void *, 8> conditionalArgs;

  llvm::ArrayRef<GenericParamDescriptor> genericParams;
  if (auto typeDescriptor = type->getTypeContextDescriptor())
    genericParams = typeDescriptor->getGenericParams();

  if (hasConditionalRequirements() || !genericParams.empty()) {
    SubstGenericParametersFromMetadata substitutions(type);
    auto error = _checkGenericRequirements(
        genericParams, getConditionalRequirements(), conditionalArgs,
        [&substitutions](unsigned depth, unsigned index) {
          return substitutions.getMetadata(depth, index).Ptr;
        },
        [&substitutions](unsigned fullOrdinal, unsigned keyOrdinal) {
          return substitutions.getMetadataKeyArgOrdinal(keyOrdinal).Ptr;
        },
        [&substitutions](const Metadata *type, unsigned index) {
          return substitutions.getWitnessTable(type, index);
        },
        &context);
    if (error)
      return nullptr;
  }
#if SWIFT_STDLIB_USE_RELATIVE_PROTOCOL_WITNESS_TABLES
  auto wtable = (const WitnessTable *)
    swift_getWitnessTableRelative(this, type, conditionalArgs.data());
#else
  auto wtable = swift_getWitnessTable(this, type, conditionalArgs.data());
#endif

  if (!wtable)
    return nullptr;

  // Check the global-actor isolation for this conformance, combining it with
  // any global-actor isolation determined based on the conditional
  // requirements above.
  if (_checkWitnessTableIsolation(type, wtable, conditionalArgs, context))
    return nullptr;

  return wtable;
}

ConformanceLookupResult ConformanceLookupResult::fromConformance(
    const Metadata *type,
    const ProtocolConformanceDescriptor *conformanceDescriptor) {
  ConformanceExecutionContext context;
  auto wtable = conformanceDescriptor->getWitnessTable(type, context);
  return {
    wtable,
    context.globalActorIsolationType,
    context.globalActorIsolationWitnessTable
  };
}

/// Determine the global actor isolation for the given witness table.
///
/// Returns true if an error occurred, false if global actor isolation was
/// successfully computed (which can mean "not isolated").
static bool _checkWitnessTableIsolation(
  const Metadata *type,
  const WitnessTable *wtable,
  llvm::ArrayRef<const void *> conditionalArgs,
  ConformanceExecutionContext &context
) {
#if SWIFT_STDLIB_USE_RELATIVE_PROTOCOL_WITNESS_TABLES
  auto description = lookThroughOptionalConditionalWitnessTable(
                         reinterpret_cast<const RelativeWitnessTable *>(wtable))
                         ->getDescription();
#else
  auto description = wtable->getDescription();
#endif

  // If there's no protocol conformance descriptor, do nothing.
  if (!description)
    return false;

  // If this conformance doesn't have global actor isolation, we're done.
  if (!description->hasGlobalActorIsolation())
    return false;

  // Resolve the global actor type.
  SubstGenericParametersFromMetadata substitutions(type);
  auto result = swift_getTypeByMangledName(
     MetadataState::Abstract, description->getGlobalActorType(),
     conditionalArgs.data(),
     [&substitutions](unsigned depth, unsigned index) {
        return substitutions.getMetadata(depth, index).Ptr;
      },
    [&substitutions](const Metadata *type, unsigned index) {
      return substitutions.getWitnessTable(type, index);
    });
  if (result.isError())
    return true;

  auto myGlobalActorIsolationType = result.getType().getMetadata();
  if (!myGlobalActorIsolationType)
    return true;

  // If the global actor isolation from this conformance conflicts with
  // the one we already have, fail.
  if (context.globalActorIsolationType &&
      context.globalActorIsolationType != myGlobalActorIsolationType)
    return true;

  // Dig out the witness table.
  auto myConformance = description->getGlobalActorConformance();
  if (!myConformance)
    return true;

  auto myWitnessTable = ConformanceLookupResult::fromConformance(
      myGlobalActorIsolationType, myConformance);
  if (!myWitnessTable)
    return true;

  context.globalActorIsolationType = myGlobalActorIsolationType;
  context.globalActorIsolationWitnessTable = myWitnessTable.witnessTable;
  return false;
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
    llvm::PointerUnion<const Metadata *, const TypeContextDescriptor *>
        TypeOrDescriptor;
    const ProtocolDescriptor *Proto;

    ConformanceCacheKey(const Metadata *type, const ProtocolDescriptor *proto)
        : TypeOrDescriptor(type), Proto(proto) {
      assert(type);
    }

    ConformanceCacheKey(llvm::PointerUnion<const Metadata *, const TypeContextDescriptor *> typeOrDescriptor, const ProtocolDescriptor *proto)
        : Proto(proto) {
      TypeOrDescriptor = typeOrDescriptor;
      assert(typeOrDescriptor);
    }

    ConformanceCacheKey(const TypeContextDescriptor *typeDescriptor, const ProtocolDescriptor *proto)
    : TypeOrDescriptor(typeDescriptor), Proto(proto) {
      assert(typeDescriptor);
    }

    friend llvm::hash_code hash_value(const ConformanceCacheKey &key) {
      return llvm::hash_combine(key.TypeOrDescriptor.getOpaqueValue(),
                                key.Proto);
    }
  };

  struct ConformanceCacheEntry {
  public:
    /// Storage used when we have global actor isolation on the conformance.
    struct ExtendedStorage {
      /// The protocol to which the type conforms.
      const ProtocolDescriptor *Proto;

      /// The global actor to which this conformance is isolated, or NULL for
      /// a nonisolated conformances.
      const Metadata *globalActorIsolationType = nullptr;

      /// When the conformance is global-actor-isolated, this is the conformance
      /// of globalActorIsolationType to GlobalActor.
      const WitnessTable *globalActorIsolationWitnessTable = nullptr;

      /// The next pointer in the list of extended storage allocations.
      ExtendedStorage *next = nullptr;
    };

    llvm::PointerUnion<const Metadata *, const TypeContextDescriptor *>
        TypeOrDescriptor;
    llvm::PointerUnion<const ProtocolDescriptor *, ExtendedStorage *>
        ProtoOrStorage;

    union {
      /// The witness table. Used for type cache records.
      const WitnessTable *Witness;

      /// The conformance. Used for type descriptor cache records.
      const ProtocolConformanceDescriptor *Conformance;
    };

  public:
    ConformanceCacheEntry(const Metadata *type, const ProtocolDescriptor *proto,
                          ConformanceLookupResult result,
                          std::atomic<ExtendedStorage *> &storageHead)
        : TypeOrDescriptor(type), Witness(result.witnessTable) {
      if (!result.globalActorIsolationType) {
        ProtoOrStorage = proto;
        return;
      }

      // Allocate extended storage.
      void *memory = malloc(sizeof(ExtendedStorage));
      auto storage = new (memory) ExtendedStorage{
        proto, result.globalActorIsolationType,
        result.globalActorIsolationWitnessTable
      };

      ProtoOrStorage = storage;

      // Add the storage pointer to the list of extended storage allocations
      // so that we can free them later.
      auto head = storageHead.load(std::memory_order_relaxed);
      while (true) {
        storage->next = head;
        if (storageHead.compare_exchange_weak(
                head, storage, std::memory_order_release,
                std::memory_order_relaxed))
          break;
      };
    }

    ConformanceCacheEntry(const TypeContextDescriptor *typeDescriptor,
                          const ProtocolDescriptor *proto,
                          const ProtocolConformanceDescriptor *conformance)
        : TypeOrDescriptor(typeDescriptor), ProtoOrStorage(proto),
          Conformance(conformance) {
      assert(TypeOrDescriptor);
      assert(ProtoOrStorage);
    }

    bool matchesKey(const ConformanceCacheKey &key) const {
      return TypeOrDescriptor == key.TypeOrDescriptor && getProtocol() == key.Proto;
    }

    friend llvm::hash_code hash_value(const ConformanceCacheEntry &entry) {
      return hash_value(entry.getKey());
    }

    /// Get the protocol.
    const ProtocolDescriptor *getProtocol() const {
      if (auto proto = ProtoOrStorage.dyn_cast<const ProtocolDescriptor *>())
        return proto;

      if (auto storage = ProtoOrStorage.dyn_cast<ExtendedStorage *>())
        return storage->Proto;

      return nullptr;
    }

    /// Get the conformance cache key.
    ConformanceCacheKey getKey() const {
      return ConformanceCacheKey(TypeOrDescriptor, getProtocol());
    }

    /// Get the cached witness table, or null if we cached failure.
    const WitnessTable *getWitnessTable() const {
      return Witness;
    }

    ConformanceLookupResult getResult() const {
      if (ProtoOrStorage.is<const ProtocolDescriptor *>())
        return ConformanceLookupResult { Witness, nullptr, nullptr };

      if (auto storage = ProtoOrStorage.dyn_cast<ExtendedStorage *>()) {
        return ConformanceLookupResult(
            Witness, storage->globalActorIsolationType,
            storage->globalActorIsolationWitnessTable);
      }

      return nullptr;
    }
  };
} // end anonymous namespace

static bool CanCacheTypeByDescriptor(const TypeContextDescriptor &descriptor) {
  return descriptor.isGeneric();
}

// Conformance Cache.
struct ConformanceState {
  using CacheType = ConcurrentReadableHashMap<ConformanceCacheEntry>;
  CacheType Cache;
  ConcurrentReadableArray<ConformanceSection> SectionsToScan;

  /// The head of an intrusive linked list that keeps track of all of the
  /// conformance cache entries that require extended storage.
  std::atomic<ConformanceCacheEntry::ExtendedStorage *> ExtendedStorageHead{nullptr};

  bool scanSectionsBackwards;
  bool envAllowCacheByDescriptors;

#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  uintptr_t dyldSharedCacheStart;
  uintptr_t dyldSharedCacheEnd;
  bool hasOverriddenImage;
  bool validateDyldResults;

  // Only populated when validateDyldResults is enabled.
  ConcurrentReadableArray<ConformanceSection> DyldOptimizedSections;

  bool inSharedCache(const void *ptr) {
    auto uintPtr = reinterpret_cast<uintptr_t>(ptr);
    return dyldSharedCacheStart <= uintPtr && uintPtr < dyldSharedCacheEnd;
  }

  bool dyldOptimizationsActive() { return dyldSharedCacheStart != 0; }
#else
  bool dyldOptimizationsActive() { return false; }

#endif

  ConformanceState() {
    scanSectionsBackwards =
        runtime::bincompat::useLegacyProtocolConformanceReverseIteration();
    envAllowCacheByDescriptors = runtime::environment::
        SWIFT_DEBUG_ENABLE_CACHE_PROTOCOL_CONFORMANCES_BY_TYPE_DESCRIPTOR();

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
            validateDyldResults = runtime::environment::
                SWIFT_DEBUG_VALIDATE_SHARED_CACHE_PROTOCOL_CONFORMANCES();
            DYLD_CONFORMANCES_LOG("Shared cache range is %#lx-%#lx",
                                  dyldSharedCacheStart, dyldSharedCacheEnd);
          } else {
            DYLD_CONFORMANCES_LOG("Disabling dyld protocol conformance "
                                  "optimizations due to unknown "
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
                   ConformanceLookupResult result, size_t sectionsCount,
                   bool allowSaveDescriptor) {
    CacheType::GetOrInsertManyScope lockedCache(Cache);
    
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
    // will add the new sections, then clear the cache.
    // When it clears the cache, it will block waiting for
    // this code to complete and relinquish Cache's writer
    // lock. If we cache a stale entry, it will be
    // immediately cleared.
    if (sectionsCount > 0 &&
      SectionsToScan.snapshot().count() != sectionsCount)
    return; // abandon the new entry

    lockedCache.getOrInsert(ConformanceCacheKey(type, proto),
                            [&](ConformanceCacheEntry *entry, bool created) {
                              // Create the entry if needed. If it already
                              // exists, we're done.
                              if (!created)
                                return false;

                              ::new (entry) ConformanceCacheEntry(
                                  type, proto, result, ExtendedStorageHead);
                              return true; // keep the new entry
                            });

    if (auto typeDescriptor = type->getTypeContextDescriptor();
        envAllowCacheByDescriptors && allowSaveDescriptor &&
        typeDescriptor && result.witnessTable &&
        CanCacheTypeByDescriptor(*typeDescriptor)) {
      auto conformance = result.witnessTable->getDescription();
      lockedCache.getOrInsert(ConformanceCacheKey(typeDescriptor, proto),
                              [&](ConformanceCacheEntry *entry, bool created) {
                                if (!created)
                                  return false;

                                ::new (entry) ConformanceCacheEntry(
                                    typeDescriptor, proto, conformance);
                                return true;
                              });
    }
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
  C.Cache.clear([&](ConcurrentFreeListNode *&freeListHead) {
    // The extended storage for conformance entries will need to be freed
    // eventually. Put it on the concurrent free list so the cache will do so.
    auto storageHead = C.ExtendedStorageHead.load(std::memory_order_relaxed);
    while (storageHead) {
      auto current = storageHead;
      auto newHead = current->next;
      if (C.ExtendedStorageHead.compare_exchange_weak(
              storageHead, newHead, std::memory_order_release,
              std::memory_order_relaxed)) {
        ConcurrentFreeListNode::add(&freeListHead, current);
      }
    }
  });
}

void swift::addImageProtocolConformanceBlockCallbackUnsafe(
    const void *baseAddress,
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
      DYLD_CONFORMANCES_LOG(
          "Skipping conformances section %p in the shared cache", conformances);
      if (C.validateDyldResults)
        C.DyldOptimizedSections.push_back(
            ConformanceSection{conformances, conformancesSize});
      return;
#if DYLD_FIND_PROTOCOL_ON_DISK_CONFORMANCE_DEFINED
    } else if (&_dyld_has_preoptimized_swift_protocol_conformances &&
               _dyld_has_preoptimized_swift_protocol_conformances(
                   reinterpret_cast<const mach_header *>(baseAddress))) {
      // dyld may optimize images outside the shared cache. Skip those too.
      DYLD_CONFORMANCES_LOG(
          "Skipping conformances section %p optimized by dyld", conformances);
      if (C.validateDyldResults)
        C.DyldOptimizedSections.push_back(
            ConformanceSection{conformances, conformancesSize});
      return;
#endif
    } else {
      DYLD_CONFORMANCES_LOG(
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
    const void *baseAddress,
    const void *conformances, uintptr_t conformancesSize) {
  Conformances.get();
  addImageProtocolConformanceBlockCallbackUnsafe(baseAddress,
                                                 conformances,
                                                 conformancesSize);
}

void
swift::swift_registerProtocolConformances(const ProtocolConformanceRecord *begin,
                                          const ProtocolConformanceRecord *end){
  auto &C = Conformances.get();
  _registerProtocolConformances(C, ConformanceSection{begin, end});
}

// Result of `searchInConformanceCache`
struct SearchInConformanceCacheResult {
  enum class Source {
    None,
    TypeMetadata,
    TypeDescriptor,
  };

  /// `IsAuthoritative` is `true` if the result is for the type itself and not a
  /// superclass. If `false` then we cached a conformance on a superclass, but
  /// that may be overridden.
  bool IsAuthoritative;
  ConformanceLookupResult Result;
#ifndef NDEBUG
  Source Source; // For logging purpose
#endif

  SearchInConformanceCacheResult(bool isAuthoritative,
                                 ConformanceLookupResult result,
                                 enum Source source)
      : IsAuthoritative(isAuthoritative), Result(result)
#ifndef NDEBUG
        , Source(source)
#endif
  {}

  static SearchInConformanceCacheResult NotFound() {
    return SearchInConformanceCacheResult(false, {},
                                          Source::None);
  }
};

/// Search for a conformance descriptor in the ConformanceCache.
static SearchInConformanceCacheResult
searchInConformanceCache(const Metadata *type,
                         const ProtocolDescriptor *protocol,
                         bool instantiateSuperclassMetadata) {
  auto &C = Conformances.get();
  auto origType = type;
  auto snapshot = C.Cache.snapshot();

  MaybeIncompleteSuperclassIterator superclassIterator{
      type, instantiateSuperclassMetadata};
  for (; auto type = superclassIterator.metadata; ++superclassIterator) {
    if (auto *cacheEntry = snapshot.find(ConformanceCacheKey(type, protocol))) {
      return SearchInConformanceCacheResult(
          type == origType, cacheEntry->getResult(),
          SearchInConformanceCacheResult::Source::TypeMetadata);
    }
    if (auto *typeDescriptor = type->getTypeContextDescriptor();
        typeDescriptor && CanCacheTypeByDescriptor(*typeDescriptor)) {
      auto *cacheEntry =
          snapshot.find(ConformanceCacheKey(typeDescriptor, protocol));
      if (!cacheEntry)
        continue;
      auto conformanceDescriptor = cacheEntry->Conformance;
      auto result =
          ConformanceLookupResult::fromConformance(type, conformanceDescriptor);
      // In case we couldn't get a witness table from the cached conformance
      // for this type. While it's possible we could find another conformance
      // that satisfies the requirements, we do NOT attempt to find it.
      // We cache it and return the result immediatelly.
      // This aligns with the current logic of the scanning:
      // When we find a conformance for the given type and protocol we attempt
      // to get the witness table and cache it and put into `foundWitnesses` no
      // matter if the witness is nullptr. If we find another conformance in
      // subsequent iterations we will get the witness and attempt to insert it
      // into the cache and `foundWitnesses` as well, but both of them will NOT
      // override the existing entry saved earlier. Later we select the first
      // witness in `foundWitnesses` in order of hierarchy even if it's null.
      // See the test case `(GenericSubClass<Int>() as Any as! Hello).hello()`
      // in `test/multifile/protocol-conformance-redundant.swift` for example.
      auto sectionsCount = C.SectionsToScan.snapshot().count();
      bool allowSaveDescriptor = false;
      C.cacheResult(type, protocol, result, sectionsCount,
                    allowSaveDescriptor);
      auto isAuthoritative = origType == type;
      return SearchInConformanceCacheResult(
          isAuthoritative, result,
          SearchInConformanceCacheResult::Source::TypeDescriptor);
    }
  }

  // We did not find a cache entry.
  return SearchInConformanceCacheResult::NotFound();
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
    /// match this conformance, along with the final metadata state of the
    /// superclass iterator.
    std::pair<const Metadata *, std::optional<MetadataState>>
    getMatchingType(const Metadata *conformingType,
                    bool instantiateSuperclassMetadata) const {
      MaybeIncompleteSuperclassIterator superclassIterator{
          conformingType, instantiateSuperclassMetadata};
      for (; auto conformingType = superclassIterator.metadata;
           ++superclassIterator) {
        if (matches(conformingType))
          return {conformingType, std::nullopt};
      }

      return {nullptr, superclassIterator.state};
    }
  };
}

static void validateDyldResults(
    ConformanceState &C, const Metadata *type,
    const ProtocolDescriptor *protocol,
    ConformanceLookupResult dyldCachedWitnessTable,
    const ProtocolConformanceDescriptor *dyldCachedConformanceDescriptor,
    bool instantiateSuperclassMetadata) {
#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
  if (!C.dyldOptimizationsActive() || !C.validateDyldResults)
    return;

  llvm::SmallVector<const ProtocolConformanceDescriptor *, 8> conformances;
  for (auto &section : C.DyldOptimizedSections.snapshot()) {
    for (const auto &record : section) {
      auto &descriptor = *record.get();
      if (descriptor.getProtocol() != protocol)
        continue;

      ConformanceCandidate candidate(descriptor);
      if (std::get<const Metadata *>(
              candidate.getMatchingType(type, instantiateSuperclassMetadata)))
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

#if USE_DYLD_SHARED_CACHE_CONFORMANCE_TABLES
static _dyld_protocol_conformance_result getDyldSharedCacheConformance(
    ConformanceState &C, const ProtocolDescriptor *protocol,
    const ClassMetadata *objcClassMetadata,
    const ContextDescriptor *description, llvm::StringRef foreignTypeIdentity) {
  // Protocols that aren't in the shared cache will never be found in the shared
  // cache conformances, skip the call.
  if (!C.inSharedCache(protocol)) {
    DYLD_CONFORMANCES_LOG(
        "Skipping shared cache lookup, protocol %p is not in shared cache.",
        protocol);
    return {_dyld_protocol_conformance_result_kind_not_found, nullptr};
  }

  if (!foreignTypeIdentity.empty()) {
    // Foreign types are non-unique so those can still be found in the shared
    // cache even if the identity string is outside.
    DYLD_CONFORMANCES_LOG(
        "_dyld_find_foreign_type_protocol_conformance(%p, %.*s, %zu)", protocol,
        (int)foreignTypeIdentity.size(), foreignTypeIdentity.data(),
        foreignTypeIdentity.size());
    return _dyld_find_foreign_type_protocol_conformance(
        protocol, foreignTypeIdentity.data(), foreignTypeIdentity.size());
  } else {
    // If both the ObjC class metadata and description are outside the shared
    // cache, then we'll never find a shared cache conformance, skip the call.
    // We can still find a shared cache conformance if one is inside and one is
    // outside.
    if (!C.inSharedCache(objcClassMetadata) && !C.inSharedCache(description)) {
      DYLD_CONFORMANCES_LOG("Skipping shared cache lookup, class %p and "
                            "description %p are not in shared cache.",
                            objcClassMetadata, description);
      return {_dyld_protocol_conformance_result_kind_not_found, nullptr};
    }

    DYLD_CONFORMANCES_LOG("_dyld_find_protocol_conformance(%p, %p, %p)",
                          protocol, objcClassMetadata, description);
    return _dyld_find_protocol_conformance(protocol, objcClassMetadata,
                                           description);
  }
}

static _dyld_protocol_conformance_result getDyldOnDiskConformance(
    ConformanceState &C, const ProtocolDescriptor *protocol,
    const ClassMetadata *objcClassMetadata,
    const ContextDescriptor *description, llvm::StringRef foreignTypeIdentity) {
#if DYLD_FIND_PROTOCOL_ON_DISK_CONFORMANCE_DEFINED
  if (&_dyld_find_foreign_type_protocol_conformance_on_disk &&
      &_dyld_find_protocol_conformance_on_disk) {
    if (!foreignTypeIdentity.empty()) {
      DYLD_CONFORMANCES_LOG(
          "_dyld_find_foreign_type_protocol_conformance_on_disk(%"
          "p, %.*s, %zu, 0)",
          protocol, (int)foreignTypeIdentity.size(), foreignTypeIdentity.data(),
          foreignTypeIdentity.size());
      return _dyld_find_foreign_type_protocol_conformance_on_disk(
          protocol, foreignTypeIdentity.data(), foreignTypeIdentity.size(), 0);
    } else {
      DYLD_CONFORMANCES_LOG(
          "_dyld_find_protocol_conformance_on_disk(%p, %p, %p, 0)", protocol,
          objcClassMetadata, description);
      return _dyld_find_protocol_conformance_on_disk(
          protocol, objcClassMetadata, description, 0);
    }
  }
#endif
  return {_dyld_protocol_conformance_result_kind_not_found, nullptr};
}
#endif

/// Query dyld for a protocol conformance, if supported. The return
/// value is a tuple consisting of the found witness table (if any), the found
/// conformance descriptor (if any), and a bool that's true if a failure is
/// definitive.
static std::tuple<ConformanceLookupResult,
                  const ProtocolConformanceDescriptor *,
                  bool>
findConformanceWithDyld(ConformanceState &C, const Metadata *type,
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
  DYLD_CONFORMANCES_LOG("Looking up conformance of %.*s (type=%p, "
                        "objcClassMetadata=%p, description=%p) to %s (%p)",
                        (int)typeName.length, typeName.data, type,
                        objcClassMetadata, description, protocol->Name.get(),
                        protocol);
#endif
  _dyld_protocol_conformance_result dyldResult;
  if (C.scanSectionsBackwards) {
    // Search "on disk" first, then shared cache.
    dyldResult = getDyldOnDiskConformance(C, protocol, objcClassMetadata,
                                          description, foreignTypeIdentity);
    if (dyldResult.kind == _dyld_protocol_conformance_result_kind_not_found)
      dyldResult = getDyldSharedCacheConformance(
          C, protocol, objcClassMetadata, description, foreignTypeIdentity);
  } else {
    // In normal operation, search the shared cache first.
    dyldResult = getDyldSharedCacheConformance(
        C, protocol, objcClassMetadata, description, foreignTypeIdentity);
    if (dyldResult.kind == _dyld_protocol_conformance_result_kind_not_found)
      dyldResult = getDyldOnDiskConformance(C, protocol, objcClassMetadata,
                                            description, foreignTypeIdentity);
  }

  switch (dyldResult.kind) {
  case _dyld_protocol_conformance_result_kind_found_descriptor: {
    auto *conformanceDescriptor =
        reinterpret_cast<const ProtocolConformanceDescriptor *>(
            dyldResult.value);

    assert(conformanceDescriptor->getProtocol() == protocol);
    assert(std::get<const Metadata *>(
        ConformanceCandidate{*conformanceDescriptor}.getMatchingType(
            type, instantiateSuperclassMetadata)));

    if (conformanceDescriptor->getGenericWitnessTable()) {
      DYLD_CONFORMANCES_LOG(
          "DYLD found generic conformance descriptor %p for %s, continuing",
          conformanceDescriptor, protocol->Name.get());
      return std::make_tuple(nullptr, conformanceDescriptor, false);
    } else {
      // When there are no generics, we can retrieve the witness table cheaply,
      // so do it up front.
      DYLD_CONFORMANCES_LOG("DYLD Found conformance descriptor %p for %s",
                            conformanceDescriptor, protocol->Name.get());
      auto result = ConformanceLookupResult::fromConformance(
          type, conformanceDescriptor);
      return std::make_tuple(result, conformanceDescriptor, false);
    }
    break;
  }
  case _dyld_protocol_conformance_result_kind_found_witness_table: {
    // If we found a witness table then we're done.
    DYLD_CONFORMANCES_LOG("DYLD found witness table %p for conformance to %s",
                          dyldResult.value, protocol->Name.get());
    auto result = ConformanceLookupResult{
        reinterpret_cast<const WitnessTable *>(dyldResult.value), nullptr,
        nullptr};
    return std::make_tuple(result, nullptr, false);
  }
  case _dyld_protocol_conformance_result_kind_not_found:
    // If nothing is found, then we'll proceed with checking the runtime's
    // caches and scanning conformance records.
    DYLD_CONFORMANCES_LOG("DYLD did not find conformance to %s",
                          protocol->Name.get());
    return std::make_tuple(nullptr, nullptr, false);
    break;
  case _dyld_protocol_conformance_result_kind_definitive_failure:
    // This type is known not to conform to this protocol. Return failure
    // without any further checks.
    DYLD_CONFORMANCES_LOG("DYLD found definitive failure for %s",
                          protocol->Name.get());
    return std::make_tuple(nullptr, nullptr, true);
  default:
    // Other values may be added. Consider them equivalent to not_found until
    // we implement code to handle them.
    DYLD_CONFORMANCES_LOG(
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
/// of the the result of the lookup (which evaluates false if no conformance was
/// found), and a boolean indicating whether there are uninstantiated
/// superclasses that were not searched.
static std::pair<ConformanceLookupResult, bool>
swift_conformsToProtocolMaybeInstantiateSuperclasses(
    const Metadata *const type, const ProtocolDescriptor *protocol,
    bool instantiateSuperclassMetadata) {
  auto &C = Conformances.get();

  ConformanceLookupResult dyldCachedWitnessTable;
  const ProtocolConformanceDescriptor *dyldCachedConformanceDescriptor =
      nullptr;

  // Track whether we have uninstantiated superclasses. Each time we iterate
  // over our superclasses, we check the final state to see if there are more
  // superclasses we haven't instantiated by calling noteFinalMetadataState.
  // If we ever see Abstract, that means there are more superclasses we can't
  // check yet, and we might get a false negative. We have to do this after each
  // iteration (really, just the first iteration, but it's hard to keep track of
  // which iteration is the first time), because another thread might
  // instantiate the superclass while we're in the middle of searching. If we
  // only look at the state after the last iteration, we might have hit a false
  // negative before that no longer shows up.
  bool hasUninstantiatedSuperclass = false;
  auto noteFinalMetadataState = [&](std::optional<MetadataState> state) {
    hasUninstantiatedSuperclass =
        hasUninstantiatedSuperclass || state == MetadataState::Abstract;
  };

  // Search the shared cache tables for a conformance for this type, and for
  // superclasses (if it's a class).
  if (C.dyldOptimizationsActive()) {
    MaybeIncompleteSuperclassIterator superclassIterator{
        type, instantiateSuperclassMetadata};
    for (; auto dyldSearchType = superclassIterator.metadata;
         ++superclassIterator) {
      bool definitiveFailure;
      std::tie(dyldCachedWitnessTable, dyldCachedConformanceDescriptor,
               definitiveFailure) =
          findConformanceWithDyld(C, dyldSearchType, protocol,
                                  instantiateSuperclassMetadata);

      if (definitiveFailure)
        return {ConformanceLookupResult{}, false};

      if (dyldCachedWitnessTable || dyldCachedConformanceDescriptor)
        break;
    }
    noteFinalMetadataState(superclassIterator.state);

    validateDyldResults(C, type, protocol, dyldCachedWitnessTable,
                        dyldCachedConformanceDescriptor,
                        instantiateSuperclassMetadata);
    // Return a cached result if we got a witness table. We can't do this if
    // scanSectionsBackwards is set, since a scanned conformance can override a
    // cached result in that case.
    if (!C.scanSectionsBackwards)
      if (dyldCachedWitnessTable)
        return {dyldCachedWitnessTable, false};
  }

  auto debugLogResult = [&](bool found, const char *source) {
    if (IsDebugLog()) {
      auto typeName = swift_getTypeName(type, true);
      const char *status = found ? "found" : "not found";
      fprintf(stderr, "Check confomance %.*s to %s: %s, source: %s\n",
             (int)typeName.length, typeName.data, protocol->Name.get(), status,
             source);
    }
  };

  // See if we have an authoritative cached conformance. The
  // ConcurrentReadableHashMap data structure allows us to search the map
  // concurrently without locking.
  if (auto cacheSearchResult = searchInConformanceCache(
          type, protocol, instantiateSuperclassMetadata);
      cacheSearchResult.IsAuthoritative) {
    // An authoritative negative result can be overridden by a result from dyld.
    if (!cacheSearchResult.Result.witnessTable) {
      if (dyldCachedWitnessTable)
        return {dyldCachedWitnessTable, false};
    }
#ifndef NDEBUG
    const char *source;
    switch (cacheSearchResult.Source) {
    case SearchInConformanceCacheResult::Source::None:
      source = "unknown";
      break;
    case SearchInConformanceCacheResult::Source::TypeMetadata:
      source = "cache by type metadata";
      break;
    case SearchInConformanceCacheResult::Source::TypeDescriptor:
      source = "cache by type descriptor";
      break;
    }
    debugLogResult(cacheSearchResult.Result.witnessTable != nullptr,  source);
#endif
    return {cacheSearchResult.Result, false};
  }

  if (dyldCachedConformanceDescriptor) {
    ConformanceCandidate candidate(*dyldCachedConformanceDescriptor);
    auto *matchingType = std::get<const Metadata *>(
        candidate.getMatchingType(type, instantiateSuperclassMetadata));
    assert(matchingType);
    auto witness = ConformanceLookupResult::fromConformance(
        matchingType, dyldCachedConformanceDescriptor);
    bool allowSaveDescriptor = false; // already have it in the dyld cache
    C.cacheResult(type, protocol, witness, /*always cache*/ 0, allowSaveDescriptor);
    DYLD_CONFORMANCES_LOG("Caching generic conformance to %s found by DYLD",
                          protocol->Name.get());
    return {witness, false};
  }

  // Scan conformance records.
  llvm::SmallDenseMap<const Metadata *, ConformanceLookupResult> foundWitnesses;
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
      const Metadata *matchingType;
      std::optional<MetadataState> finalState;
      std::tie(matchingType, finalState) =
          candidate.getMatchingType(type, instantiateSuperclassMetadata);
      noteFinalMetadataState(finalState);
      if (matchingType) {
        auto witness = ConformanceLookupResult::fromConformance(
            matchingType, &descriptor);
        bool allowSaveDescriptor = true;
        C.cacheResult(matchingType, protocol, witness, /*always cache*/ 0, allowSaveDescriptor);
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

  auto traceState =
      runtime::trace::protocol_conformance_scan_begin(type, protocol);

  auto snapshot = C.SectionsToScan.snapshot();
  if (C.scanSectionsBackwards) {
    for (auto &section : llvm::reverse(snapshot))
      processSection(section);
  } else {
    for (auto &section : snapshot)
      processSection(section);
  }

  // Find the most specific conformance that was scanned.
  ConformanceLookupResult foundWitness = nullptr;
  const Metadata *foundType = nullptr;

  MaybeIncompleteSuperclassIterator superclassIterator{
      type, instantiateSuperclassMetadata};
  for (; auto searchType = superclassIterator.metadata; ++superclassIterator) {
    const auto witnessIt = foundWitnesses.find(searchType);
    if (witnessIt != foundWitnesses.end()) {
      if (!foundType) {
        foundWitness = witnessIt->getSecond(); // may be null
        foundType = searchType;
      } else {
        auto foundName = swift_getTypeName(foundType, true);
        auto searchName = swift_getTypeName(searchType, true);
        swift::warning(RuntimeErrorFlagNone,
                       "Warning: '%.*s' conforms to protocol '%s', but it also "
                       "inherits conformance from '%.*s'.  Relying on a "
                       "particular conformance is undefined behaviour.\n",
                       (int)foundName.length, foundName.data,
                       protocol->Name.get(),
                       (int)searchName.length, searchName.data);
      }
    }
  }
  noteFinalMetadataState(superclassIterator.state);

  traceState.end(foundWitness.witnessTable);

  // If it's for a superclass or if we didn't find anything, then add an
  // authoritative entry for this type.
  if (foundType != type)
    // Do not cache negative results if there were uninstantiated superclasses
    // we didn't search. They might have a conformance that will be found later.
    if (foundWitness || !hasUninstantiatedSuperclass)
      C.cacheResult(type, protocol, foundWitness, snapshot.count(), /* allowSaveDescriptor */ false);

  // A negative result can be overridden by a result from dyld.
  if (!foundWitness) {
    if (dyldCachedWitnessTable) {
      debugLogResult(true, "dyld cache");
      return {dyldCachedWitnessTable, false};
    }
  }
  debugLogResult(static_cast<bool>(foundWitness), "section scan");
  return {foundWitness, hasUninstantiatedSuperclass};
}

static const WitnessTable *
swift_conformsToProtocolWithExecutionContextImpl(
    const Metadata *const type,
    const ProtocolDescriptor *protocol,
    ConformanceExecutionContext *context) {
  ConformanceLookupResult found;
  bool hasUninstantiatedSuperclass;

  // First, try without instantiating any new superclasses. This avoids
  // an infinite loop for cases like `class Sub: Super<Sub>`. In cases like
  // that, the conformance must exist on the subclass (or at least somewhere
  // in the chain before we get to an uninstantiated superclass) so this search
  // will succeed without trying to instantiate Super while it's already being
  // instantiated.=
  std::tie(found, hasUninstantiatedSuperclass) =
      swift_conformsToProtocolMaybeInstantiateSuperclasses(
          type, protocol, false /*instantiateSuperclassMetadata*/);

  // If no conformance was found, and there is an uninstantiated superclass that
  // was not searched, then try the search again and instantiate all
  // superclasses.
  if (!found && hasUninstantiatedSuperclass)
    std::tie(found, hasUninstantiatedSuperclass) =
        swift_conformsToProtocolMaybeInstantiateSuperclasses(
            type, protocol, true /*instantiateSuperclassMetadata*/);

  // Check for isolated conformances.
  if (found.globalActorIsolationType && context) {
    // If the existing global actor isolation differs from the one we
    // computed, it's a conflict. Fail.
    if (context->globalActorIsolationType &&
        context->globalActorIsolationType != found.globalActorIsolationType)
      return nullptr;

    // Report the global actor isolation.
    context->globalActorIsolationType = found.globalActorIsolationType;
    context->globalActorIsolationWitnessTable =
        found.globalActorIsolationWitnessTable;
  }

  return found.witnessTable;
}

static const WitnessTable *
swift_conformsToProtocolCommonImpl(
    const Metadata *const type,
    const ProtocolDescriptor *protocol) {
  return swift_conformsToProtocolWithExecutionContextImpl(
      type, protocol, nullptr);
}

static const WitnessTable *
swift_conformsToProtocol2Impl(const Metadata *const type,
                              const ProtocolDescriptor *protocol) {
  protocol = swift_auth_data_non_address(
      protocol, SpecialPointerAuthDiscriminators::ProtocolDescriptor);
  return swift_conformsToProtocolCommonImpl(type, protocol);
}

static const WitnessTable *
swift_conformsToProtocolImpl(const Metadata *const type,
                             const void *protocol) {
  // This call takes `protocol` without a ptrauth signature. We declare
  // it as `void *` to avoid the implicit ptrauth we get from the
  // ptrauth_struct attribute. The static_cast implicitly signs the
  // pointer when we call through to the implementation in
  // swift_conformsToProtocolCommon.
  return swift_conformsToProtocolCommonImpl(
      type, static_cast<const ProtocolDescriptor *>(protocol));
}

static bool swift_isInConformanceExecutionContextImpl(
    const Metadata *type,
    const ConformanceExecutionContext *context) {
  if (!context)
    return true;

  if (context->globalActorIsolationType) {
    // If the hook is not installed, assume we're on the right actor.
    if (!_swift_task_isCurrentGlobalActorHook)
      return true;

    // Check whether we are running on this global actor.
    if (!_swift_task_isCurrentGlobalActorHook(
           context->globalActorIsolationType,
           context->globalActorIsolationWitnessTable))
      return false;
  }

  return true;
}

const ContextDescriptor *
swift::_searchConformancesByMangledTypeName(Demangle::NodePointer node) {
  auto traceState = runtime::trace::protocol_conformance_scan_begin(node);

  auto &C = Conformances.get();

  for (auto &section : C.SectionsToScan.snapshot()) {
    for (const auto &record : section) {
      if (auto ntd = record->getTypeDescriptor()) {
        if (_contextDescriptorMatchesMangling(ntd, node))
          return traceState.end(ntd);
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

  std::optional<MetadataState> subclassState = std::nullopt;
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
#if SWIFT_OBJC_INTEROP
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
#endif
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

static bool isSubclassOrExistential(const Metadata *subclass,
                                    const Metadata *superclass) {
  // If the type which is constrained to a base class is an existential
  // type, and if that existential type includes a superclass constraint,
  // just require that the superclass by which the existential is
  // constrained is a subclass of the base class.
  if (auto *existential = dyn_cast<ExistentialTypeMetadata>(subclass)) {
    if (auto *superclassConstraint = existential->getSuperclassConstraint())
      subclass = superclassConstraint;
  }

  return isSubclass(subclass, superclass);
}

static std::optional<TypeLookupError>
satisfiesLayoutConstraint(const GenericRequirementDescriptor &req,
                          const Metadata *subjectType) {
  switch (req.getLayout()) {
  case GenericRequirementLayoutKind::Class:
    if (!subjectType->satisfiesClassConstraint()) {
      return TYPE_LOOKUP_ERROR_FMT(
          "subject type %.*s does not satisfy class constraint",
          (int)req.getParam().size(), req.getParam().data());
    }
    return std::nullopt;
  }

  // Unknown layout.
  return TYPE_LOOKUP_ERROR_FMT("unknown layout kind %u",
                               static_cast<uint32_t>(req.getLayout()));
}

SWIFT_CC(swift)
SWIFT_RUNTIME_STDLIB_SPI
bool swift::_swift_class_isSubclass(const Metadata *subclass,
                                    const Metadata *superclass) {
  return isSubclass(subclass, superclass);
}

static std::optional<TypeLookupError>
checkInvertibleRequirements(const Metadata *type,
                              InvertibleProtocolSet ignored);

static std::optional<TypeLookupError>
checkGenericRequirement(
    const GenericRequirementDescriptor &req,
    llvm::SmallVectorImpl<const void *> &extraArguments,
    SubstGenericParameterFn substGenericParam,
    SubstDependentWitnessTableFn substWitnessTable,
    llvm::SmallVectorImpl<InvertibleProtocolSet> &suppressed,
    ConformanceExecutionContext *context) {
  assert(!req.getFlags().isPackRequirement());

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
                             &witnessTable, context)) {
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

    return std::nullopt;
  }

  case GenericRequirementKind::SameType: {
    // Demangle the second type under the given substitutions.
    auto result = swift_getTypeByMangledName(
        MetadataState::Abstract, req.getMangledTypeName(),
        extraArguments.data(), substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    auto otherType = result.getType().getMetadata();

    // Check that the types are equivalent.
    if (subjectType != otherType) {
      return TYPE_LOOKUP_ERROR_FMT(
          "subject type %.*s does not match %.*s", (int)req.getParam().size(),
          req.getParam().data(), (int)req.getMangledTypeName().size(),
          req.getMangledTypeName().data());
    }

    return std::nullopt;
  }

  case GenericRequirementKind::Layout: {
    return satisfiesLayoutConstraint(req, subjectType);
  }

  case GenericRequirementKind::BaseClass: {
    // Demangle the base type under the given substitutions.
    auto result = swift_getTypeByMangledName(
        MetadataState::Abstract, req.getMangledTypeName(),
        extraArguments.data(), substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    auto baseType = result.getType().getMetadata();

    if (!isSubclassOrExistential(subjectType, baseType))
      return TYPE_LOOKUP_ERROR_FMT(
          "%.*s is not subclass of %.*s", (int)req.getParam().size(),
          req.getParam().data(), (int)req.getMangledTypeName().size(),
          req.getMangledTypeName().data());

    return std::nullopt;
  }

  case GenericRequirementKind::SameConformance: {
    // FIXME: Implement this check.
    return std::nullopt;
  }

  case GenericRequirementKind::SameShape: {
    return TYPE_LOOKUP_ERROR_FMT("can't have same-shape requirement where "
                                 "subject type is not a pack");
  }
  case GenericRequirementKind::InvertedProtocols: {
    uint16_t index = req.getInvertedProtocolsGenericParamIndex();
    if (index == 0xFFFF) {
      return checkInvertibleRequirements(subjectType,
                                         req.getInvertedProtocols());
    }

    // Expand the suppression set so we can record these protocols.
    if (index >= suppressed.size()) {
      suppressed.resize(index + 1, InvertibleProtocolSet());
    }

    // Record these suppressed protocols for this generic parameter.
    suppressed[index] |= req.getInvertedProtocols();
    return std::nullopt;
  }
  }

  // Unknown generic requirement kind.
  return TYPE_LOOKUP_ERROR_FMT("unknown generic requirement kind %u",
                               (unsigned)req.getKind());
}

static std::optional<TypeLookupError>
checkGenericPackRequirement(
    const GenericRequirementDescriptor &req,
    llvm::SmallVectorImpl<const void *> &extraArguments,
    SubstGenericParameterFn substGenericParam,
    SubstDependentWitnessTableFn substWitnessTable,
    llvm::SmallVectorImpl<InvertibleProtocolSet> &suppressed,
    ConformanceExecutionContext *context) {
  assert(req.getFlags().isPackRequirement());

  // Make sure we understand the requirement we're dealing with.
  if (!req.hasKnownKind())
    return TypeLookupError("unknown kind");

  // Resolve the subject generic parameter.
  auto result = swift::getTypePackByMangledName(
      req.getParam(), extraArguments.data(),
      substGenericParam, substWitnessTable);
  if (result.getError())
    return *result.getError();
  MetadataPackPointer subjectType = result.getType();
  assert(subjectType.getLifetime() == PackLifetime::OnHeap);

  // Check the requirement.
  switch (req.getKind()) {
  case GenericRequirementKind::Protocol: {
    llvm::SmallVector<const WitnessTable *, 4> witnessTables;

    // Look up the conformance of each pack element to the protocol.
    for (size_t i = 0, e = subjectType.getNumElements(); i < e; ++i) {
      const Metadata *elt = subjectType.getElements()[i];

      const WitnessTable *witnessTable = nullptr;
      if (!_conformsToProtocol(nullptr, elt, req.getProtocol(),
                               &witnessTable, context)) {
        const char *protoName =
            req.getProtocol() ? req.getProtocol().getName() : "<null>";
        return TYPE_LOOKUP_ERROR_FMT(
            "subject type %.*s does not conform to protocol %s at pack index %zu",
            (int)req.getParam().size(), req.getParam().data(), protoName, i);
      }

      if (req.getProtocol().needsWitnessTable())
        witnessTables.push_back(witnessTable);
    }

    // If we need a witness table, add it.
    if (req.getProtocol().needsWitnessTable()) {
      assert(witnessTables.size() == subjectType.getNumElements());
      auto *pack = swift_allocateWitnessTablePack(witnessTables.data(),
                                                  witnessTables.size());
      extraArguments.push_back(pack);
    }

    return std::nullopt;
  }

  case GenericRequirementKind::SameType: {
    // Resolve the constraint generic parameter.
    auto result = swift::getTypePackByMangledName(
        req.getMangledTypeName(), extraArguments.data(),
        substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    MetadataPackPointer constraintType = result.getType();
    assert(constraintType.getLifetime() == PackLifetime::OnHeap);

    if (subjectType.getNumElements() != constraintType.getNumElements()) {
      return TYPE_LOOKUP_ERROR_FMT(
            "mismatched pack lengths in same-type pack requirement %.*s: %zu vs %zu",
            (int)req.getParam().size(), req.getParam().data(),
            subjectType.getNumElements(), constraintType.getNumElements());
    }

    for (size_t i = 0, e = subjectType.getNumElements(); i < e; ++i) {
      auto *subjectElt = subjectType.getElements()[i];
      auto *constraintElt = constraintType.getElements()[i];

      if (subjectElt != constraintElt) {
        return TYPE_LOOKUP_ERROR_FMT(
            "subject type %.*s does not match %.*s at pack index %zu",
            (int)req.getParam().size(),
            req.getParam().data(), (int)req.getMangledTypeName().size(),
            req.getMangledTypeName().data(), i);
      }
    }

    return std::nullopt;
  }

  case GenericRequirementKind::Layout: {
    for (size_t i = 0, e = subjectType.getNumElements(); i < e; ++i) {
      const Metadata *elt = subjectType.getElements()[i];
      if (auto result = satisfiesLayoutConstraint(req, elt))
        return result;
    }

    return std::nullopt;
  }

  case GenericRequirementKind::BaseClass: {
    // Demangle the base type under the given substitutions.
    auto result = swift_getTypeByMangledName(
        MetadataState::Abstract, req.getMangledTypeName(),
        extraArguments.data(), substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    auto baseType = result.getType().getMetadata();

    // Check that each pack element inherits from the base class.
    for (size_t i = 0, e = subjectType.getNumElements(); i < e; ++i) {
      const Metadata *elt = subjectType.getElements()[i];

      if (!isSubclassOrExistential(elt, baseType))
      return TYPE_LOOKUP_ERROR_FMT(
          "%.*s is not subclass of %.*s at pack index %zu",
          (int)req.getParam().size(),
          req.getParam().data(), (int)req.getMangledTypeName().size(),
          req.getMangledTypeName().data(), i);
    }

    return std::nullopt;
  }

  case GenericRequirementKind::SameConformance: {
    // FIXME: Implement this check.
    return std::nullopt;
  }

  case GenericRequirementKind::SameShape: {
    auto result = swift::getTypePackByMangledName(
        req.getMangledTypeName(), extraArguments.data(),
        substGenericParam, substWitnessTable);
    if (result.getError())
      return *result.getError();
    MetadataPackPointer otherType = result.getType();
    assert(otherType.getLifetime() == PackLifetime::OnHeap);

    if (subjectType.getNumElements() != otherType.getNumElements()) {
      return TYPE_LOOKUP_ERROR_FMT("same-shape requirement unsatisfied; "
                                   "%zu != %zu",
                                   subjectType.getNumElements(),
                                   otherType.getNumElements() );
    }

    return std::nullopt;
  }

  case GenericRequirementKind::InvertedProtocols: {
    uint16_t index = req.getInvertedProtocolsGenericParamIndex();
    if (index == 0xFFFF) {
      // Check that each pack element meets the invertible requirements.
      for (size_t i = 0, e = subjectType.getNumElements(); i < e; ++i) {
        const Metadata *elt = subjectType.getElements()[i];

        if (auto error = checkInvertibleRequirements(
                elt, req.getInvertedProtocols()))
          return error;
      }

      return std::nullopt;
    }

    // Expand the suppression set so we can record these protocols.
    if (index >= suppressed.size()) {
      suppressed.resize(index + 1, InvertibleProtocolSet());
    }

    // Record these suppressed protocols for this generic parameter.
    suppressed[index] |= req.getInvertedProtocols();
    return std::nullopt;
  }
  }

  // Unknown generic requirement kind.
  return TYPE_LOOKUP_ERROR_FMT("unknown generic requirement kind %u",
                               (unsigned)req.getKind());
}

static std::optional<TypeLookupError>
checkGenericValueRequirement(const GenericRequirementDescriptor &req,
                             llvm::SmallVectorImpl<const void *> &extraArguments,
                             SubstGenericParameterFn substGenericParam,
                             SubstDependentWitnessTableFn substWitnessTable,
                     llvm::SmallVectorImpl<InvertibleProtocolSet> &suppressed) {
  assert(req.getFlags().isValueRequirement());

  // Make sure we understand the requirement we're dealing with.
  if (!req.hasKnownKind())
    return TypeLookupError("unknown kind");

  // Resolve the subject generic value.
  auto result = swift::getTypeValueByMangledName(
      req.getParam(), extraArguments.data(),
      substGenericParam, substWitnessTable);

  if (result.getError())
    return *result.getError();

  auto subjectValue = result.getType();

  // Check the requirement.
  switch (req.getKind()) {
  case GenericRequirementKind::SameType: {
    // Resolve the constraint generic value.
    auto result = swift::getTypeValueByMangledName(
        req.getMangledTypeName(), extraArguments.data(),
        substGenericParam, substWitnessTable);

    if (result.getError())
      return *result.getError();

    auto constraintValue = result.getType();

    if (subjectValue != constraintValue) {
      return TYPE_LOOKUP_ERROR_FMT(
            "subject value %" PRIiPTR " does not match constraint value %" PRIiPTR,
            subjectValue,
            constraintValue);
    }

    return std::nullopt;
  }

  default: {
    // Value requirements can only be same type'd at the moment.
    return TYPE_LOOKUP_ERROR_FMT("unknown value generic requirement kind %u",
                                 (unsigned)req.getKind());
  }
  }
}

static std::optional<TypeLookupError>
checkInvertibleRequirementsStructural(const Metadata *type,
                                        InvertibleProtocolSet ignored) {
  switch (type->getKind()) {
  case MetadataKind::Class:
  case MetadataKind::Struct:
  case MetadataKind::Enum:
  case MetadataKind::Optional:
  case MetadataKind::ForeignClass:
  case MetadataKind::ForeignReferenceType:
  case MetadataKind::ObjCClassWrapper:
    // All handled via context descriptor in the caller.
    return std::nullopt;

  case MetadataKind::HeapLocalVariable:
  case MetadataKind::Opaque:
  case MetadataKind::HeapGenericLocalVariable:
  case MetadataKind::ErrorObject:
  case MetadataKind::Task:
  case MetadataKind::Job:
    // Not part of the user-visible type system; assumed to handle all
    // invertible requirements.
    return std::nullopt;

  case MetadataKind::Tuple: {
    // Check every element type in the tuple.
    auto tupleMetadata = cast<TupleTypeMetadata>(type);
    for (unsigned i = 0, n = tupleMetadata->NumElements; i != n; ++i) {
      if (auto error =
              checkInvertibleRequirements(&*tupleMetadata->getElement(i).Type,
                                            ignored))
        return error;
    }
    return std::nullopt;
  }

  case MetadataKind::Function: {
    auto functionMetadata = cast<FunctionTypeMetadata>(type);

    // Determine the set of protocols that are suppressed by the function
    // type.
    InvertibleProtocolSet suppressed;
    if (functionMetadata->hasExtendedFlags()) {
      suppressed = functionMetadata->getExtendedFlags()
          .getInvertedProtocols();
    }

    // Map the existing "noescape" bit as a suppressed protocol, when
    // appropriate.
    switch (functionMetadata->getConvention()) {
    case FunctionMetadataConvention::Swift:
      // Swift function types can be non-escaping, so honor the bit.
      if (!functionMetadata->isEscaping())
        suppressed.insert(InvertibleProtocolKind::Escapable);
      break;

    case FunctionMetadataConvention::Block:
      // Objective-C block types don't encode non-escaping-ness in metadata,
      // so we assume that they are always escaping.
      break;

    case FunctionMetadataConvention::Thin:
    case FunctionMetadataConvention::CFunctionPointer:
      // Thin and C function pointers have no captures, so whether they
      // escape is irrelevant.
      break;
    }

    auto missing = suppressed - ignored;
    if (!missing.empty()) {
      return TYPE_LOOKUP_ERROR_FMT(
          "function type missing invertible protocols %x", missing.rawBits());
    }

    return std::nullopt;
  }

  case MetadataKind::ExtendedExistential: {
    auto existential = cast<ExtendedExistentialTypeMetadata>(type);
    auto &shape = *existential->Shape;
    llvm::ArrayRef<GenericRequirementDescriptor> reqs(
        shape.getReqSigRequirements(), shape.getNumReqSigRequirements());
    // Look for any suppressed protocol requirements. If the existential
    // has suppressed a protocol that is not ignored, then the existential
    // does not meet the specified requirements.
    for (const auto& req : reqs) {
      if (req.getKind() != GenericRequirementKind::InvertedProtocols)
        continue;

      auto suppressed = req.getInvertedProtocols();
      auto missing = suppressed - ignored;
      if (!missing.empty()) {
        return TYPE_LOOKUP_ERROR_FMT(
            "existential type missing invertible protocols %x",
            missing.rawBits());
      }
    }

    return std::nullopt;
  }

  case MetadataKind::Metatype:
  case MetadataKind::ExistentialMetatype:
    // Metatypes themselves can't have invertible protocols.
    return std::nullopt;

  case MetadataKind::Existential:
    // The existential representation has no room for specifying any
    // suppressed requirements, so it always succeeds.
    return std::nullopt;
    
  case MetadataKind::FixedArray:
    // Builtin.FixedArray has no conformances of its own.
    return std::nullopt;

  case MetadataKind::LastEnumerated:
    break;
  }

  // Just accept any unknown types.
  return std::nullopt;
}

/// Check that the given `type` meets all invertible protocol requirements
/// that haven't been explicitly suppressed by `ignored`.
std::optional<TypeLookupError>
checkInvertibleRequirements(const Metadata *type, 
                              InvertibleProtocolSet ignored) {
  auto contextDescriptor = type->getTypeContextDescriptor();
  if (!contextDescriptor)
    return checkInvertibleRequirementsStructural(type, ignored);

  // If no conformances are suppressed, then it conforms to everything.
  if (!contextDescriptor->hasInvertibleProtocols()) {
    return std::nullopt;
  }

  // If this type has suppressed conformances, but we can't find them...
  // bail out.
  auto InvertedProtocols = contextDescriptor->getInvertedProtocols();
  if (!InvertedProtocols) {
    return TYPE_LOOKUP_ERROR_FMT("unable to find suppressed protocols");
  }

  // Determine the set of invertible conformances that the type has
  // suppressed but aren't being ignored. These are missing conformances
  // based on the primary definition of the type.
  InvertibleProtocolSet missingConformances = *InvertedProtocols - ignored;
  if (missingConformances.empty())
    return std::nullopt;

  // If the context descriptor is not generic, there are no conditional
  // conformances: fail.
  if (!contextDescriptor->isGeneric()) {
    return TYPE_LOOKUP_ERROR_FMT("type missing invertible conformances %x",
                                 missingConformances.rawBits());
  }

  auto genericContext = contextDescriptor->getGenericContext();
  if (!genericContext ||
      !genericContext->hasConditionalInvertedProtocols()) {
    return TYPE_LOOKUP_ERROR_FMT("type missing invertible conformances %x",
                                 missingConformances.rawBits());
  }

  // If there are missing conformances that do not have corresponding
  // conditional conformances, then the nominal type does not satisfy these
  // suppressed conformances. We're done.
  auto conditionalSuppressed =
      genericContext->getConditionalInvertedProtocols();
  auto alwaysMissingConformances = missingConformances - conditionalSuppressed;
  if (!alwaysMissingConformances.empty()) {
    return TYPE_LOOKUP_ERROR_FMT("type missing invertible conformances %x",
                                 alwaysMissingConformances.rawBits());
  }

  // Now we need to check the conditional conformances for each of the
  // missing conformances.
  for (auto invertibleKind : missingConformances) {
    // Get the conditional requirements.
    // Note: This will end up being quadratic in the number of invertible
    // protocols. That number is small (currently 2) and cannot be more than 16,
    // but if it's a problem we can switch to a different strategy.
    auto condReqs =
        genericContext->getConditionalInvertibleProtocolRequirementsFor(
                                                             invertibleKind);

    // Check the conditional requirements.
    llvm::ArrayRef<GenericRequirementDescriptor> requirements(
        reinterpret_cast<const GenericRequirementDescriptor *>(condReqs.data()),
        condReqs.size());
    SubstGenericParametersFromMetadata substFn(type);
    llvm::SmallVector<const void *, 1> extraArguments;
    auto error = _checkGenericRequirements(
        genericContext->getGenericParams(),
        requirements, extraArguments,
        [&substFn](unsigned depth, unsigned index) {
          return substFn.getMetadata(depth, index).Ptr;
        },
        [&substFn](unsigned fullOrdinal, unsigned keyOrdinal) {
          return substFn.getMetadataKeyArgOrdinal(keyOrdinal).Ptr;
        },
        [&substFn](const Metadata *type, unsigned index) {
          return substFn.getWitnessTable(type, index);
        },
        nullptr);
    if (error)
      return error;
  }

  return std::nullopt;
}

std::optional<TypeLookupError> swift::_checkGenericRequirements(
    llvm::ArrayRef<GenericParamDescriptor> genericParams,
    llvm::ArrayRef<GenericRequirementDescriptor> requirements,
    llvm::SmallVectorImpl<const void *> &extraArguments,
    SubstGenericParameterFn substGenericParam,
    SubstGenericParameterOrdinalFn substGenericParamOrdinal,
    SubstDependentWitnessTableFn substWitnessTable,
    ConformanceExecutionContext *context) {
  // The suppressed conformances for each generic parameter.
  llvm::SmallVector<InvertibleProtocolSet, 4> allSuppressed;

  for (const auto &req : requirements) {
    if (req.getFlags().isPackRequirement()) {
      auto error = checkGenericPackRequirement(req, extraArguments,
                                               substGenericParam,
                                               substWitnessTable,
                                               allSuppressed,
                                               context);
      if (error)
        return error;
    } else if (req.getFlags().isValueRequirement()) {
      auto error = checkGenericValueRequirement(req, extraArguments,
                                                substGenericParam,
                                                substWitnessTable,
                                                allSuppressed);
      if (error)
        return error;
    } else {
      auto error = checkGenericRequirement(req, extraArguments,
                                           substGenericParam,
                                           substWitnessTable,
                                           allSuppressed,
                                           context);
      if (error)
        return error;
    }
  }

  // Now, check all of the generic arguments for invertible protocols.
  unsigned numGenericParams = genericParams.size();
  unsigned keyIndex = 0;
  for (unsigned index = 0; index != numGenericParams; ++index) {
    // Non-key arguments don't need to be checked, because they are
    // aliased to another type.
    if (!genericParams[index].hasKeyArgument())
      continue;

    InvertibleProtocolSet suppressed;
    if (index < allSuppressed.size())
      suppressed = allSuppressed[index];

    MetadataPackOrValue MetadataPackOrValue(substGenericParamOrdinal(index, keyIndex));
    switch (genericParams[index].getKind()) {
    case GenericParamKind::Type: {
      if (!MetadataPackOrValue || MetadataPackOrValue.isMetadataPack()) {
        return TYPE_LOOKUP_ERROR_FMT(
            "unexpected pack for generic parameter %u", index);
      }

      auto metadata = MetadataPackOrValue.getMetadata();
      if (auto error = checkInvertibleRequirements(metadata, suppressed))
        return error;

      break;
    }

    case GenericParamKind::TypePack: {
      // NULL can be used to indicate an empty pack.
      if (!MetadataPackOrValue)
        break;

      if (MetadataPackOrValue.isMetadata()) {
        return TYPE_LOOKUP_ERROR_FMT(
            "unexpected metadata for generic pack parameter %u", index);
      }

      auto pack = MetadataPackOrValue.getMetadataPack();
      if (pack.getElements() != 0) {
        llvm::ArrayRef<const Metadata *> elements(
            pack.getElements(), pack.getNumElements());
        for (auto element : elements) {
          if (auto error = checkInvertibleRequirements(element, suppressed))
            return error;
        }
      }
      break;
    }

    case GenericParamKind::Value: {
      // Value parameter can never conform to protocols or the inverse thereof.
      break;
    }

    default:
      return TYPE_LOOKUP_ERROR_FMT("unknown generic parameter kind %u",
                                   index);
    }
    keyIndex++;
  }

  // Success!
  return std::nullopt;
}

const Metadata *swift::findConformingSuperclass(
                            const Metadata *type,
                            const ProtocolConformanceDescriptor *conformance) {
  // Figure out which type we're looking for.
  ConformanceCandidate candidate(*conformance);

  const Metadata *conformingType = std::get<const Metadata *>(
      candidate.getMatchingType(type, true /*instantiateSuperclassMetadata*/));
  assert(conformingType);
  return conformingType;
}

size_t swift::swift_ConformanceExecutionContextSize =
    sizeof(ConformanceExecutionContext);

#define OVERRIDE_PROTOCOLCONFORMANCE COMPATIBILITY_OVERRIDE
#include "../CompatibilityOverride/CompatibilityOverrideIncludePath.h"
