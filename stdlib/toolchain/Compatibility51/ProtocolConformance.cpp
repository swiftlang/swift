//===--- ProtocolConformance.cpp - Swift conformance checking backport ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checking and caching of Swift protocol conformances.
//
// This is a version of the Swift 5.2 protocol conformance cache implementation
// adapted for backporting to Swift 5.1 with the following fixes applied:
//
// - rdar://problem/59460603, fixing a problem where the conformance cache would
//   eagerly instantiate metadata for types when not necessary, causing crashes
//   if instantiating the type relied on weak-linked symbols that aren't
//   available on the client OS
//
//===----------------------------------------------------------------------===//

#include "../../public/runtime/Private.h"
#include "Concurrent.h"
#include "Overrides.h"
#include "swift/Basic/Lazy.h"
#include <assert.h>
#include <dlfcn.h>
#include <mach-o/dyld.h>
#include <mach-o/getsect.h>
#include <objc/runtime.h>

using namespace swift;
using swift::overrides::ConcurrentMap;
using swift::overrides::ConcurrentReadableArray;

// Look up Swift runtime entry points dynamically. This handles the case
// where the main executable can't link against libswiftCore.dylib because
// it will be loaded dynamically from a location that isn't known at build
// time.
static const Metadata *getObjCClassMetadata(const ClassMetadata *c) {
  using FPtr = const Metadata *(*)(const ClassMetadata *);
  FPtr func = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<FPtr>(dlsym(RTLD_DEFAULT, "swift_getObjCClassMetadata")));

  return func(c);
}
static const ExistentialTypeMetadata *getExistentialTypeMetadata(
                                     ProtocolClassConstraint classConstraint,
                                     const Metadata *superclassConstraint,
                                     size_t numProtocols,
                                     const ProtocolDescriptorRef *protocols) {
  auto func = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const ExistentialTypeMetadata *(*)(ProtocolClassConstraint classConstraint,
      const Metadata *superclassConstraint,
      size_t numProtocols,
      const ProtocolDescriptorRef *protocols)>(
                     dlsym(RTLD_DEFAULT, "swift_getExistentialTypeMetadata")));
  return func(classConstraint, superclassConstraint, numProtocols, protocols);
}
static const TypeContextDescriptor *getTypeContextDescriptor(const Metadata *type) {
  auto func = SWIFT_LAZY_CONSTANT(
    reinterpret_cast<const TypeContextDescriptor *(*)(const Metadata *)>(
                     dlsym(RTLD_DEFAULT, "swift_getTypeContextDescriptor")));
  return func(type);
}

// Clone of private helper swift::_swift_class_getSuperclass
// for use in the override implementation.
//
// This also gets used from the Compatibility50 library.
const Metadata *_swiftoverride_class_getSuperclass(
                                                    const Metadata *theClass) {
  if (const ClassMetadata *classType = theClass->getClassObject()) {
    if (classHasSuperclass(classType))
      return getObjCClassMetadata(classType->Superclass);
  }
  
  if (const ForeignClassMetadata *foreignClassType
      = dyn_cast<ForeignClassMetadata>(theClass)) {
    if (const Metadata *superclass = foreignClassType->Superclass)
      return superclass;
  }
  
  return nullptr;
}

// Clone of private function getRootSuperclass. This returns the SwiftObject
// class in the ABI-stable dylib, regardless of what the local runtime build
// does, since we're always patching an ABI-stable dylib.
__attribute__((visibility("hidden"), weak))
const ClassMetadata *swift::getRootSuperclass() {
  auto theClass = SWIFT_LAZY_CONSTANT(objc_getClass("_TtCs12_SwiftObject"));
  return (const ClassMetadata *)theClass;
}

namespace {

StringRef getTypeContextIdentity(const TypeContextDescriptor *type) {
  // The first component is the user-facing name and (unless overridden)
  // the ABI name.
  StringRef component = type->Name.get();

  // If we don't have import info, we're done.
  if (!type->getTypeContextDescriptorFlags().hasImportInfo()) {
    return component;
  }
  
  // The identity starts with the user-facing name.
  const char *startOfIdentity = component.begin();
  const char *endOfIdentity = component.end();

  enum class TypeImportComponent : char {
    ABIName = 'N',
    SymbolNamespace = 'S',
    RelatedEntityName = 'R',
  };

  while (true) {
    // Parse the next component.  If it's empty, we're done.
    component = StringRef(component.end() + 1);
    if (component.empty()) break;

    // Update the identity bounds and assert that the identity
    // components are in the right order.
    auto kind = TypeImportComponent(component[0]);
    if (kind == TypeImportComponent::ABIName) {
      startOfIdentity = component.begin() + 1;
      endOfIdentity = component.end();
    } else if (kind == TypeImportComponent::SymbolNamespace) {
      endOfIdentity = component.end();
    } else if (kind == TypeImportComponent::RelatedEntityName) {
      endOfIdentity = component.end();
    }
  }
  
  return StringRef(startOfIdentity, endOfIdentity - startOfIdentity);
}

// Reimplementation of the runtime-private function `swift::equalContexts`
static bool override_equalContexts(const ContextDescriptor *a,
                                   const ContextDescriptor *b)
{
  // Fast path: pointer equality.
  if (a == b) return true;

  // If either context is null, we're done.
  if (a == nullptr || b == nullptr)
    return false;

  // If either descriptor is known to be unique, we're done.
  if (a->isUnique() || b->isUnique()) return false;
  
  // Do the kinds match?
  if (a->getKind() != b->getKind()) return false;
  
  // Do the parents match?
  if (!override_equalContexts(a->Parent.get(), b->Parent.get()))
    return false;
  
  // Compare kind-specific details.
  switch (auto kind = a->getKind()) {
  case ContextDescriptorKind::Module: {
    // Modules with the same name are equivalent.
    auto moduleA = cast<ModuleContextDescriptor>(a);
    auto moduleB = cast<ModuleContextDescriptor>(b);
    return strcmp(moduleA->Name.get(), moduleB->Name.get()) == 0;
  }
  
  case ContextDescriptorKind::Extension:
  case ContextDescriptorKind::Anonymous:
    // These context kinds are always unique.
    return false;
  
  default:
    // Types in the same context with the same name are equivalent.
    if (kind >= ContextDescriptorKind::Type_First
        && kind <= ContextDescriptorKind::Type_Last) {
      auto typeA = cast<TypeContextDescriptor>(a);
      auto typeB = cast<TypeContextDescriptor>(b);
      return getTypeContextIdentity(typeA) == getTypeContextIdentity(typeB);
    }
    
    // Otherwise, this runtime doesn't know anything about this context kind.
    // Conservatively return false.
    return false;
  }
}

// Reimplementation of the runtime-private function
// `ProtocolConformanceDescriptor::getCanonicalTypeMetadata`.
static const Metadata *
override_getCanonicalTypeMetadata(const ProtocolConformanceDescriptor *conf) {
  switch (conf->getTypeKind()) {
  // The class may be ObjC, in which case we need to instantiate its Swift
  // metadata. The class additionally may be weak-linked, so we have to check
  // for null.
  case TypeReferenceKind::IndirectObjCClass:
    if (auto cls = *conf->getIndirectObjCClass())
      return getObjCClassMetadata(cls);
    return nullptr;
      
  case TypeReferenceKind::DirectObjCClassName:
    if (auto cls = reinterpret_cast<const ClassMetadata *>(
                             objc_lookUpClass(conf->getDirectObjCClassName())))
      return getObjCClassMetadata(cls);
    return nullptr;

  case TypeReferenceKind::DirectTypeDescriptor:
  case TypeReferenceKind::IndirectTypeDescriptor: {
    if (auto anyType = conf->getTypeDescriptor()) {
      if (auto type = dyn_cast<TypeContextDescriptor>(anyType)) {
        if (!type->isGeneric()) {
          if (auto accessFn = type->getAccessFunction())
            return accessFn(MetadataState::Abstract).Value;
        }
      } else if (auto protocol = dyn_cast<ProtocolDescriptor>(anyType)) {
        auto protocolRef = ProtocolDescriptorRef::forSwift(protocol);
        auto constraint =
          protocol->getProtocolContextDescriptorFlags().getClassConstraint();
        return getExistentialTypeMetadata(constraint,
                                          /*superclass bound*/ nullptr,
                                          /*num protocols*/ 1,
                                          &protocolRef);
      }
    }

    return nullptr;
  }
  }

  swift_unreachable("Unhandled TypeReferenceKind in switch.");
}

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

      if (auto metadata = override_getCanonicalTypeMetadata(&conformance)) {
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
      const auto *description = getTypeContextDescriptor(conformingType);
      if (description)
        return description;

      // Handle single-protocol existential types for self-conformance.
      auto *existentialType = dyn_cast<ExistentialTypeMetadata>(conformingType);
      if (existentialType == nullptr ||
          existentialType->getProtocols().size() != 1 ||
          existentialType->getSuperclassConstraint() != nullptr)
        return nullptr;

      auto proto = existentialType->getProtocols()[0];

      if (proto.isObjC())
        return nullptr;

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
        if (description && override_equalContexts(description, candidateDescription))
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
        conformingType = _swiftoverride_class_getSuperclass(conformingType);
      }

      return nullptr;
    }
  };

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

#if __POINTER_WIDTH__ == 64
using mach_header_platform = mach_header_64;
#else
using mach_header_platform = mach_header;
#endif

// Conformance Cache.
struct ConformanceState {
  ConcurrentMap<ConformanceCacheEntry> Cache;
  ConcurrentReadableArray<ConformanceSection> SectionsToScan;
  
  ConformanceState();

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
};

static Lazy<ConformanceState> Conformances;

// The Swift runtime in the OS installs this callback to populate its original
// version of the conformance cache, but since we have our own implementation,
// we must install our own callback to populate our copy as well.
static void addImageCallback(const mach_header *mh) {
  // Look for a __swift5_proto section.
  unsigned long conformancesSize;
  const uint8_t *conformances =
  getsectiondata(reinterpret_cast<const mach_header_platform *>(mh),
                 SEG_TEXT, "__swift5_proto",
                 &conformancesSize);
  
  if (!conformances)
    return;

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
  Conformances.unsafeGetAlreadyInitialized()
    .SectionsToScan
    .push_back(ConformanceSection{recordsBegin, recordsEnd});
};

static void addImageCallback(const mach_header *mh, intptr_t vmaddr_slide) {
  addImageCallback(mh);
}

static void initializeProtocolConformanceLookup() {
  // If `objc_addLoadImageFunc` is available on this OS, use it.
  // We don't use `__builtin_available` because that requires libraries that may
  // not be linked into the binary carrying this compatibility shim.
  auto objc_addLoadImageFunc = reinterpret_cast<void(*)(objc_func_loadImage)>(
                                  dlsym(RTLD_DEFAULT, "objc_addLoadImageFunc"));
  if (objc_addLoadImageFunc) {
    objc_addLoadImageFunc(addImageCallback);
  } else {
    _dyld_register_func_for_add_image(addImageCallback);
  }
}

ConformanceState::ConformanceState() {
  initializeProtocolConformanceLookup();
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
  if (auto description = getTypeContextDescriptor(type))
    return description;

  return type;
}

/// Search for a conformance descriptor in the ConformanceCache.
static ConformanceCacheResult
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
  if (auto superclass = _swiftoverride_class_getSuperclass(type)) {
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

} // end anonymous namespace

const ProtocolConformanceDescriptor *
swift::swift51override_conformsToSwiftProtocol(const Metadata * const type,
                                           const ProtocolDescriptor *protocol,
                                           StringRef module,
                                           ConformsToSwiftProtocol_t *orig) {
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
  for (size_t i = startIndex; i < endIndex; ++i) {
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

