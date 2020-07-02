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
#include "swift/Demangling/Demangle.h"
#include "swift/Runtime/Casting.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/HeapObject.h"
#include "swift/Runtime/Metadata.h"
#include "swift/Runtime/Unreachable.h"
#include "CompatibilityOverride.h"
#include "ImageInspection.h"
#include "Private.h"

#include <vector>

using namespace swift;

#ifndef NDEBUG
template <>
LLVM_ATTRIBUTE_USED
void ProtocolDescriptor::dump() const {
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
template<>
LLVM_ATTRIBUTE_USED
void ProtocolConformanceDescriptor::verify() const {
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

  swift_runtime_unreachable("Unhandled TypeReferenceKind in switch.");
}
#endif

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

  swift_runtime_unreachable("Unhandled TypeReferenceKind in switch.");
}

template<>
const WitnessTable *
ProtocolConformanceDescriptor::getWitnessTable(const Metadata *type) const {
  // If needed, check the conditional requirements.
  SmallVector<const void *, 8> conditionalArgs;
  if (hasConditionalRequirements()) {
    SubstGenericParametersFromMetadata substitutions(type);
    bool failed =
      _checkGenericRequirements(getConditionalRequirements(), conditionalArgs,
        [&substitutions](unsigned depth, unsigned index) {
          return substitutions.getMetadata(depth, index);
        },
        [&substitutions](const Metadata *type, unsigned index) {
          return substitutions.getWitnessTable(type, index);
        });
    if (failed) return nullptr;
  }

  return swift_getWitnessTable(this, type, conditionalArgs.data());
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
        : Type(type), Proto(proto) {
      assert(type);
    }
  };

  struct ConformanceCacheEntry {
  private:
    const void *Type; 
    const ProtocolDescriptor *Proto;
    std::atomic<const ProtocolConformanceDescriptor *> Description;
    std::atomic<size_t> FailureGeneration;

  public:
    ConformanceCacheEntry(ConformanceCacheKey key,
                          const ProtocolConformanceDescriptor *description,
                          size_t failureGeneration)
      : Type(key.Type), Proto(key.Proto), Description(description),
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
      return Description.load(std::memory_order_relaxed) != nullptr;
    }

    void makeSuccessful(const ProtocolConformanceDescriptor *description) {
      Description.store(description, std::memory_order_release);
    }

    void updateFailureGeneration(size_t failureGeneration) {
      assert(!isSuccessful());
      FailureGeneration.store(failureGeneration, std::memory_order_relaxed);
    }

    /// Get the cached conformance descriptor, if successful.
    const ProtocolConformanceDescriptor *getDescription() const {
      assert(isSuccessful());
      return Description.load(std::memory_order_acquire);
    }
    
    /// Get the generation in which this lookup failed.
    size_t getFailureGeneration() const {
      assert(!isSuccessful());
      return FailureGeneration.load(std::memory_order_relaxed);
    }
  };
} // end anonymous namespace

// Conformance Cache.
struct ConformanceState {
  ConcurrentMap<ConformanceCacheEntry> Cache;
  ConcurrentReadableArray<ConformanceSection> SectionsToScan;
  
  ConformanceState() {
    initializeProtocolConformanceLookup();
  }

  void cacheSuccess(const void *type, const ProtocolDescriptor *proto,
                    const ProtocolConformanceDescriptor *description) {
    auto result = Cache.getOrInsert(ConformanceCacheKey(type, proto),
                                    description, 0);

    // If the entry was already present, we may need to update it.
    if (!result.second) {
      result.first->makeSuccessful(description);
    }
  }

  void cacheFailure(const void *type, const ProtocolDescriptor *proto,
                    size_t failureGeneration) {
    auto result =
      Cache.getOrInsert(ConformanceCacheKey(type, proto),
                        (const ProtocolConformanceDescriptor *) nullptr,
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

#ifndef NDEBUG
  void verify() const LLVM_ATTRIBUTE_USED;
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

static void
_registerProtocolConformances(ConformanceState &C,
                              const ProtocolConformanceRecord *begin,
                              const ProtocolConformanceRecord *end) {
  C.SectionsToScan.push_back(ConformanceSection{begin, end});
}

void swift::addImageProtocolConformanceBlockCallbackUnsafe(
    const void *conformances, uintptr_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0 &&
         "conformances section not a multiple of ProtocolConformanceRecord");

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
  _registerProtocolConformances(C, begin, end);
}


struct ConformanceCacheResult {
  // true if description is an authoritative result as-is.
  // false if more searching is required (for example, because a cached
  // failure was returned in failureEntry but it is out-of-date.
  bool isAuthoritative;

  // The matching conformance descriptor, or null if no cached conformance
  // was found.
  const ProtocolConformanceDescriptor *description;

  // If the search fails, this may be the negative cache entry for the
  // queried type itself. This entry may be null or out-of-date.
  ConformanceCacheEntry *failureEntry;

  static ConformanceCacheResult
  cachedSuccess(const ProtocolConformanceDescriptor *description) {
    return ConformanceCacheResult { true, description, nullptr };
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

/// Retrieve the type key from the given metadata, to be used when looking
/// into the conformance cache.
static const void *getConformanceCacheTypeKey(const Metadata *type) {
  if (auto description = type->getTypeContextDescriptor())
    return description;

  return type;
}

/// Search for a conformance descriptor in the ConformanceCache.
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
        return ConformanceCacheResult::cachedSuccess(Value->getDescription());
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
      if (Value->getFailureGeneration() == C.SectionsToScan.snapshot().count()) {
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
    auto typeKey = getConformanceCacheTypeKey(type);

    // Hash and lookup the type-protocol pair in the cache.
    if (auto *Value = C.findCached(typeKey, protocol)) {
      if (Value->isSuccessful())
        return ConformanceCacheResult::cachedSuccess(Value->getDescription());

      // We don't try to cache negative responses for generic
      // patterns.
    }
  }

  // If there is a superclass, look there.
  if (auto superclass = _swift_class_getSuperclass(type)) {
    type = superclass;
    goto recur;
  }

  // We did not find an up-to-date cache entry.
  // If we found an out-of-date entry for the original query type then
  // return it (non-authoritatively). Otherwise return a cache miss.
  if (failureEntry)
    return ConformanceCacheResult::cachedFailure(failureEntry, false);
  else
    return ConformanceCacheResult::cacheMiss();
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

    /// Retrieve the conforming type as metadata, or NULL if the candidate's
    /// conforming type is described in another way (e.g., a nominal type
    /// descriptor).
    const Metadata *getConformingTypeAsMetadata() const {
      return candidateIsMetadata ? static_cast<const Metadata *>(candidate)
                                 : nullptr;
    }

    const ContextDescriptor *
    getContextDescriptor(const Metadata *conformingType) const {
      const auto *description = conformingType->getTypeContextDescriptor();
      if (description)
        return description;

      // Handle single-protocol existential types for self-conformance.
      auto *existentialType = dyn_cast<ExistentialTypeMetadata>(conformingType);
      if (existentialType == nullptr ||
          existentialType->getProtocols().size() != 1 ||
          existentialType->getSuperclassConstraint() != nullptr)
        return nullptr;

      auto proto = existentialType->getProtocols()[0];

#if SWIFT_OBJC_INTEROP
      if (proto.isObjC())
        return nullptr;
#endif

      return proto.getSwiftProtocol();
    }

    /// Whether the conforming type exactly matches the conformance candidate.
    bool matches(const Metadata *conformingType) const {
      // Check whether the types match.
      if (candidateIsMetadata && conformingType == candidate)
        return true;

      // Check whether the nominal type descriptors match.
      if (!candidateIsMetadata) {
        const auto *description = getContextDescriptor(conformingType);
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
    const Metadata *getMatchingType(const Metadata *conformingType) const {
      while (conformingType) {
        // Check for a match.
        if (matches(conformingType))
          return conformingType;

        // Look for a superclass.
        conformingType = _swift_class_getSuperclass(conformingType);
      }

      return nullptr;
    }
  };
}

static const ProtocolConformanceDescriptor *
swift_conformsToSwiftProtocolImpl(const Metadata * const type,
                                  const ProtocolDescriptor *protocol,
                                  StringRef module) {
  auto &C = Conformances.get();

  // See if we have a cached conformance. The ConcurrentMap data structure
  // allows us to insert and search the map concurrently without locking.
  auto FoundConformance = searchInConformanceCache(type, protocol);
  // If the result (positive or negative) is authoritative, return it.
  if (FoundConformance.isAuthoritative)
    return FoundConformance.description;

  auto failureEntry = FoundConformance.failureEntry;

  // Prepare to scan conformance records.
  auto snapshot = C.SectionsToScan.snapshot();
  
  // Scan only sections that were not scanned yet.
  // If we found an out-of-date negative cache entry,
  // we need not to re-scan the sections that it covers.
  auto startIndex = failureEntry ? failureEntry->getFailureGeneration() : 0;
  auto endIndex = snapshot.count();

  // If there are no unscanned sections outstanding
  // then we can cache failure and give up now.
  if (startIndex == endIndex) {
    C.cacheFailure(type, protocol, snapshot.count());
    return nullptr;
  }

  // Really scan conformance records.
  for (size_t i = startIndex; i < endIndex; i++) {
    auto &section = snapshot.Start[i];
    // Eagerly pull records for nondependent witnesses into our cache.
    for (const auto &record : section) {
      auto &descriptor = *record.get();

      // We only care about conformances for this protocol.
      if (descriptor.getProtocol() != protocol)
        continue;

      // If there's a matching type, record the positive result.
      ConformanceCandidate candidate(descriptor);
      if (candidate.getMatchingType(type)) {
        const Metadata *matchingType = candidate.getConformingTypeAsMetadata();
        if (!matchingType)
          matchingType = type;

        C.cacheSuccess(matchingType, protocol, &descriptor);
      }
    }
  }
  
  // Conformance scan is complete.

  // Search the cache once more, and this time update the cache if necessary.
  FoundConformance = searchInConformanceCache(type, protocol);
  if (FoundConformance.isAuthoritative) {
    return FoundConformance.description;
  } else {
    C.cacheFailure(type, protocol, snapshot.count());
    return nullptr;
  }
}

static const WitnessTable *
swift_conformsToProtocolImpl(const Metadata * const type,
                             const ProtocolDescriptor *protocol) {
  auto description =
    swift_conformsToSwiftProtocol(type, protocol, StringRef());
  if (!description)
    return nullptr;

  return description->getWitnessTable(
      findConformingSuperclass(type, description));
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

static MetadataState
tryGetCompleteMetadataNonblocking(const Metadata *metadata) {
  return swift_checkMetadataState(
             MetadataRequest(MetadataState::Complete, /*isNonBlocking*/ true),
             metadata)
      .State;
}

template <typename HandleObjc>
bool isSwiftClassMetadataSubclass(const ClassMetadata *subclass,
                                  const ClassMetadata *superclass,
                                  HandleObjc handleObjc) {
  assert(subclass);
  assert(superclass);

  MetadataState subclassState = tryGetCompleteMetadataNonblocking(subclass);

  do {
    if (subclassState == MetadataState::Complete) {
      // The subclass metadata is complete.  That means not just that its
      // Superclass field is valid, but that the Superclass field of the
      // referenced class metadata is valid, and the Superclass field of the
      // class metadata referenced there, and so on transitively.
      //
      // Scan the superclass chains in the ClassMetadata looking for a match.
      while ((subclass = subclass->Superclass)) {
        if (subclass == superclass)
          return true;
      }
      return false;
    }
    if (subclassState == MetadataState::NonTransitiveComplete) {
      // The subclass metadata is complete, but, unlike above, not transitively.
      // Its Superclass field is valid, so just read that field to get to the
      // superclass to proceed to the next step.
      subclass = subclass->Superclass;
      if (subclass->isPureObjC()) {
        return handleObjc(subclass, superclass);
      }
      subclassState = tryGetCompleteMetadataNonblocking(subclass);
    } else {
      // The subclass metadata is either LayoutComplete or Abstract, so the
      // Superclass field is not valid.  To get to the superclass, make the
      // expensive call to getSuperclassMetadata which demangles the superclass
      // name from the nominal type descriptor to get the metadata for the
      // superclass.
      MetadataRequest request(MetadataState::Complete,
                              /*non-blocking*/ true);
      auto response = getSuperclassMetadata(request, subclass);
      auto newMetadata = response.Value;
      if (auto newSubclass = dyn_cast<ClassMetadata>(newMetadata)) {
        subclass = newSubclass;
        subclassState = response.State;
      } else {
        return handleObjc(newMetadata, superclass);
      }
    }
    if (subclass == superclass)
      return true;
  } while (subclass);
  return false;
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

bool swift::_checkGenericRequirements(
                      llvm::ArrayRef<GenericRequirementDescriptor> requirements,
                      SmallVectorImpl<const void *> &extraArguments,
                      SubstGenericParameterFn substGenericParam,
                      SubstDependentWitnessTableFn substWitnessTable) {
  for (const auto &req : requirements) {
    // Make sure we understand the requirement we're dealing with.
    if (!req.hasKnownKind()) return true;

    // Resolve the subject generic parameter.
    const Metadata *subjectType =
      swift_getTypeByMangledName(MetadataState::Abstract,
                                 req.getParam(),
                                 extraArguments.data(),
                                 substGenericParam, substWitnessTable).getMetadata();
    if (!subjectType)
      return true;

    // Check the requirement.
    switch (req.getKind()) {
    case GenericRequirementKind::Protocol: {
      const WitnessTable *witnessTable = nullptr;
      if (!_conformsToProtocol(nullptr, subjectType, req.getProtocol(),
                               &witnessTable))
        return true;

      // If we need a witness table, add it.
      if (req.getProtocol().needsWitnessTable()) {
        assert(witnessTable);
        extraArguments.push_back(witnessTable);
      }

      continue;
    }

    case GenericRequirementKind::SameType: {
      // Demangle the second type under the given substitutions.
      auto otherType =
        swift_getTypeByMangledName(MetadataState::Abstract,
                                   req.getMangledTypeName(),
                                   extraArguments.data(),
                                   substGenericParam, substWitnessTable).getMetadata();
      if (!otherType) return true;

      assert(!req.getFlags().hasExtraArgument());

      // Check that the types are equivalent.
      if (subjectType != otherType) return true;

      continue;
    }

    case GenericRequirementKind::Layout: {
      switch (req.getLayout()) {
      case GenericRequirementLayoutKind::Class:
        if (!subjectType->satisfiesClassConstraint())
          return true;
        continue;
      }

      // Unknown layout.
      return true;
    }

    case GenericRequirementKind::BaseClass: {
      // Demangle the base type under the given substitutions.
      auto baseType =
        swift_getTypeByMangledName(MetadataState::Abstract,
                                   req.getMangledTypeName(),
                                   extraArguments.data(),
                                   substGenericParam, substWitnessTable).getMetadata();
      if (!baseType) return true;

      // If the type which is constrained to a base class is an existential 
      // type, and if that existential type includes a superclass constraint,
      // just require that the superclass by which the existential is
      // constrained is a subclass of the base class.
      if (auto *existential = dyn_cast<ExistentialTypeMetadata>(subjectType)) {
        if (auto *superclassConstraint = existential->getSuperclassConstraint())
          subjectType = superclassConstraint;
      }

      if (!isSubclass(subjectType, baseType))
        return true;

      continue;
    }

    case GenericRequirementKind::SameConformance: {
      // FIXME: Implement this check.
      continue;
    }
    }

    // Unknown generic requirement kind.
    return true;
  }

  // Success!
  return false;
}

const Metadata *swift::findConformingSuperclass(
                            const Metadata *type,
                            const ProtocolConformanceDescriptor *conformance) {
  // Figure out which type we're looking for.
  ConformanceCandidate candidate(*conformance);

  const Metadata *conformingType = candidate.getMatchingType(type);
  assert(conformingType);
  return conformingType;
}

#define OVERRIDE_PROTOCOLCONFORMANCE COMPATIBILITY_OVERRIDE
#include "CompatibilityOverride.def"
