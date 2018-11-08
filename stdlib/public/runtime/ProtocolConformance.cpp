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
template <> void ProtocolDescriptor::dump() const {
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
    return info.symbolName;
  };

  switch (auto kind = getTypeKind()) {
  case TypeReferenceKind::DirectObjCClassName:
    printf("direct Objective-C class name %s", getDirectObjCClassName());
    break;

  case TypeReferenceKind::IndirectObjCClass:
    printf("indirect Objective-C class %s",
           class_getName(*getIndirectObjCClass()));
    break;

  case TypeReferenceKind::DirectNominalTypeDescriptor:
  case TypeReferenceKind::IndirectNominalTypeDescriptor:
    printf("unique nominal type descriptor %s", symbolName(getTypeContextDescriptor()));
    break;
  }
  
  printf(" => ");
  
  printf("witness table %pattern s\n", symbolName(getWitnessTablePattern()));
}
#endif

#ifndef NDEBUG
template<> void ProtocolConformanceDescriptor::verify() const {
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

  case TypeReferenceKind::DirectNominalTypeDescriptor:
  case TypeReferenceKind::IndirectNominalTypeDescriptor:
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

  case TypeReferenceKind::DirectNominalTypeDescriptor:
  case TypeReferenceKind::IndirectNominalTypeDescriptor: {
    auto type = getTypeContextDescriptor();
    if (!type->isGeneric()) {
      if (auto accessFn = type->getAccessFunction())
        return accessFn(MetadataState::Abstract).Value;
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
  std::vector<const void *> conditionalArgs;
  if (hasConditionalRequirements()) {
    SubstGenericParametersFromMetadata substitutions(type);
    bool failed =
      _checkGenericRequirements(getConditionalRequirements(), conditionalArgs,
                                substitutions, substitutions,
                                [&](unsigned) -> const WitnessTable * {
                                  return nullptr;
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
    /// Module in which we expect to find the conformance, or empty if
    /// we don't know/care.
    StringRef Module;

    ConformanceCacheKey(const void *type, const ProtocolDescriptor *proto,
                        StringRef module)
        : Type(type), Proto(proto), Module(module) {
      assert(type);
    }
  };

  struct ConformanceCacheEntry {
  private:
    const void *Type; 
    const ProtocolDescriptor *Proto;
    /// Module in which we expect to find the conformance, or empty if
    /// we don't know/care.
    StringRef Module;
    std::atomic<const ProtocolConformanceDescriptor *> Description;
    std::atomic<size_t> FailureGeneration;

  public:
    ConformanceCacheEntry(ConformanceCacheKey key,
                          const ProtocolConformanceDescriptor *description,
                          size_t failureGeneration)
      : Type(key.Type), Proto(key.Proto), Module(key.Module),
        Description(description),
        FailureGeneration(failureGeneration) {
    }

    int compareWithKey(const ConformanceCacheKey &key) const {
      if (key.Type != Type) {
        return (uintptr_t(key.Type) < uintptr_t(Type) ? -1 : 1);
      }
      if (key.Proto != Proto) {
        return (uintptr_t(key.Proto) < uintptr_t(Proto) ? -1 : 1);
      }
      if (int result = key.Module.compare(Module))
        return result;

      return 0;
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

    /// Get the module in which we expected to find this conformance.
    StringRef getModule() const { return Module; }

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

static constexpr size_t roundUpToAlignment(size_t offset, size_t alignment) {
  return ((offset + alignment - 1) & ~(alignment - 1));
}

// Conformance Cache.
struct ConformanceState {
  ConcurrentMap<ConformanceCacheEntry> Cache;
  ConcurrentReadableArray<ConformanceSection> SectionsToScan;
  
  ConformanceState() {
    initializeProtocolConformanceLookup();
  }

  std::pair<ConformanceCacheEntry *, bool>
  cacheResult(const void *type, const ProtocolDescriptor *proto,
              StringRef module,
              const ProtocolConformanceDescriptor *description,
              size_t failureGeneration) {
    // When there is no module, directly check/update the cache.
    ConformanceCacheKey key(type, proto, module);
    if (module.empty())
      return Cache.getOrInsert(key, description, failureGeneration);

    // Look for an existing entry. If we have one, return it.
    if (auto known = findCached(type, proto, module))
      return { known, false };

    // Clone the module string into the conformance allocator, so it refers
    // to permanent storage if it does get inserted.
    size_t moduleAllocSize = roundUpToAlignment(module.size(), sizeof(void*));
    char *newModule =
      (char *)Cache.getAllocator().Allocate(moduleAllocSize, alignof(char));
    memcpy(newModule, module.data(), module.size());
    key.Module = StringRef(newModule, module.size());

    // Update the cache.
    auto result = Cache.getOrInsert(key, description, failureGeneration);

    // If we didn't manage to perform the insertion, free the memory we just
    // allocated for the module: nobody else can reference it.
    if (result.first->getModule() != newModule) {
      Cache.getAllocator().Deallocate(newModule, moduleAllocSize);
    }

    return result;
  }

  void cacheSuccess(const void *type, const ProtocolDescriptor *proto,
                    StringRef module,
                    const ProtocolConformanceDescriptor *description) {
    auto result = cacheResult(type, proto, module, description, 0);

    // If the entry was already present, we may need to update it.
    if (!result.second) {
      result.first->makeSuccessful(description);
    }
  }

  void cacheFailure(const void *type, const ProtocolDescriptor *proto,
                    StringRef module,
                    size_t failureGeneration) {
    auto result = cacheResult(type, proto, module, nullptr, failureGeneration);

    // If the entry was already present, we may need to update it.
    if (!result.second) {
      result.first->updateFailureGeneration(failureGeneration);
    }
  }

  ConformanceCacheEntry *findCached(const void *type,
                                    const ProtocolDescriptor *proto,
                                    StringRef module) {
    return Cache.find(ConformanceCacheKey(type, proto, module));
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

void swift::addImageProtocolConformanceBlockCallback(const void *conformances,
                                                   uintptr_t conformancesSize) {
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
                         const ProtocolDescriptor *protocol,
                         StringRef module) {
  auto &C = Conformances.get();
  auto origType = type;
  ConformanceCacheEntry *failureEntry = nullptr;

recur:
  {
    // Try the specific type first.
    if (auto *Value = C.findCached(type, protocol, module)) {
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
    if (auto *Value = C.findCached(typeKey, protocol, module)) {
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
    // Check whether the types match.
    if (candidateIsMetadata && type == candidate)
      return true;

    // Check whether the nominal type descriptors match.
    if (!candidateIsMetadata) {
      const auto *description = type->getTypeContextDescriptor();
      auto candidateDescription =
        static_cast<const TypeContextDescriptor *>(candidate);
      if (description && equalContexts(description, candidateDescription))
        return true;
    }

    // If there is a superclass, look there.
    if (auto superclass = _swift_class_getSuperclass(type)) {
      type = superclass;
      continue;
    }

    break;
  }

  return false;
}

const ProtocolConformanceDescriptor *
swift::_conformsToSwiftProtocol(const Metadata * const type,
                                const ProtocolDescriptor *protocol,
                                StringRef module) {
  auto &C = Conformances.get();

  // See if we have a cached conformance. The ConcurrentMap data structure
  // allows us to insert and search the map concurrently without locking.
  auto FoundConformance = searchInConformanceCache(type, protocol, module);
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
    C.cacheFailure(type, protocol, module, snapshot.count());
    return nullptr;
  }

  /// Describes the result of matching a particular protocol conformance
  /// descriptor's module against
  enum class ModuleMatch {
    /// The descriptor does not match due to conflicting modules, i.e.,
    /// a specific module was requested but the conformance is a retroactive
    /// conformance from a different module.
    None,
    /// The protocol conformance descriptor exactly matches the module we
    /// asked for.
    Exact,
    /// The protocol conformance is retroactive, but we asked for anything,
    /// so it should be considered a retroactive candidate.
    Retroactive,
    /// The protocol conformance is not retroactive, but we were asked for
    /// a specific conformance.
    NonRetroactive,
  };

  /// Match a given descriptor against the requested module.
  auto matchModule = [module](const ProtocolConformanceDescriptor &descriptor) {
    bool isRetroactive = descriptor.isRetroactive();

    // If no module is specified, we can find any conformance.
    if (module.empty()) {
      return isRetroactive ? ModuleMatch::Retroactive : ModuleMatch::Exact;
    }

    // If the conformance is retroactive, check whether we have the same
    // module name.
    if (isRetroactive) {
      auto moduleContext =
        cast<ModuleContextDescriptor>(descriptor.getRetroactiveContext());
      return moduleContext->Name.get() == module
               ? ModuleMatch::Exact
               : ModuleMatch::None;
    }

    // We were asked for a conformance in a specific module, but we found
    // a conformance that is not retroactive.
    return ModuleMatch::NonRetroactive;
  };

  // Keep track of candidate conformances discovered that did not exactly
  // match, but can be used.
  using Candidate =
    std::pair<const ProtocolConformanceDescriptor *, const Metadata *>;
  std::vector<Candidate> candidates;

  /// Local function to record a result.
  bool foundExactMatch = false;
  auto recordResult = [&](const ProtocolConformanceDescriptor &descriptor,
                          const Metadata *type) {
    switch (auto match = matchModule(descriptor)) {
    case ModuleMatch::Exact:
      // Record the match below.
      break;

    case ModuleMatch::None:
      return;

    case ModuleMatch::NonRetroactive:
    case ModuleMatch::Retroactive:
      // Record the candidate and move on.
      candidates.push_back({&descriptor, type});
      return;
    }

    // Record the match.
    C.cacheSuccess(type, protocol, module, &descriptor);
    foundExactMatch = true;
  };

  // Really scan conformance records.
  for (size_t i = startIndex; i < endIndex; i++) {
    auto &section = snapshot.Start[i];
    // Eagerly pull records for nondependent witnesses into our cache.
    for (const auto &record : section) {
      auto &descriptor = *record.get();

      // If the record applies to a specific type, cache it.
      if (auto metadata = descriptor.getCanonicalTypeMetadata()) {
        auto P = descriptor.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, metadata, /*candidateIsMetadata=*/true))
          continue;

        // Record the conformance descriptor.
        recordResult(descriptor, metadata);
      } else if (descriptor.getTypeKind()
                   == TypeReferenceKind::DirectNominalTypeDescriptor ||
                 descriptor.getTypeKind()
                  == TypeReferenceKind::IndirectNominalTypeDescriptor) {
        auto R = descriptor.getTypeContextDescriptor();
        auto P = descriptor.getProtocol();

        // Look for an exact match.
        if (protocol != P)
          continue;

        if (!isRelatedType(type, R, /*candidateIsMetadata=*/false))
          continue;

        recordResult(descriptor, type);
      }
    }
  }
  
  // Conformance scan is complete.

  // If we didn't find an exact match, but we did find some candidates that
  // nearly matched, use them.
  if (!foundExactMatch && !candidates.empty()) {
    unsigned bestIdx = 0;

    // If there's one candidate, record it.
    if (candidates.size() > 1) {
      unsigned numCandidates = candidates.size();

      // If there is a non-retroactive conformance, use it.
      bool foundNonRetroactive = false;
      for (unsigned idx = 0; idx != numCandidates; ++idx) {
        if (!candidates[idx].first->isRetroactive()) {
          bestIdx = idx;
          foundNonRetroactive = true;
          break;
        }
      }

      // If we have only retroactive conformances, complain about the
      // conflict.
      // FIXME: Only do this once per type descriptor/protocol combination.
      if (!foundNonRetroactive) {
        // Form the list of modules in which we found retroactive conformances.
        std::string moduleNamesStr;
        const char *firstModuleName = nullptr;
        for (unsigned idx = 0; idx != numCandidates; ++idx) {
          const auto &candidate = candidates[idx];
          assert(candidate.first->isRetroactive());

          auto retroactiveContext =
            candidate.first->getRetroactiveContext();
          auto retroactiveModule =
            cast<ModuleContextDescriptor>(retroactiveContext);

          if (firstModuleName) {
            if (idx == numCandidates - 1)
              moduleNamesStr += " and ";
            else
              moduleNamesStr += ", ";
          } else {
            firstModuleName = retroactiveModule->Name;
          }

          moduleNamesStr += '\'';
          moduleNamesStr += retroactiveModule->Name.get();
          moduleNamesStr += '\'';
        }

        auto typeName = swift_getTypeName(type, true);

        // FIXME: Report to debugger
        swift::warning(0,
                       "***Swift runtime warning: multiple conformances for "
                       "'%s: %s' found in modules %s. "
                       "Arbitrarily selecting conformance from module '%s'\n",
                       StringRef(typeName.data, typeName.length).str().c_str(),
                       protocol->Name.get(),
                       moduleNamesStr.c_str(),
                       firstModuleName);
      }
    }

    C.cacheSuccess(candidates[bestIdx].second, protocol, module,
                   candidates[bestIdx].first);
  }

  // Search the cache once more, and this time update the cache if necessary.

  FoundConformance = searchInConformanceCache(type, protocol, module);
  if (FoundConformance.isAuthoritative) {
    return FoundConformance.description;
  } else {
    C.cacheFailure(type, protocol, module, snapshot.count());
    return nullptr;
  }
}

static const WitnessTable *
swift_conformsToProtocolImpl(const Metadata * const type,
                             const ProtocolDescriptor *protocol) {
  auto description = _conformsToSwiftProtocol(type, protocol, StringRef());
  if (!description)
    return nullptr;

  return description->getWitnessTable(type);
}

const TypeContextDescriptor *
swift::_searchConformancesByMangledTypeName(Demangle::NodePointer node) {
  auto &C = Conformances.get();

  for (auto &section : C.SectionsToScan.snapshot()) {
    for (const auto &record : section) {
      if (auto ntd = record->getTypeContextDescriptor()) {
        if (_contextDescriptorMatchesMangling(ntd, node))
          return ntd;
      }
    }
  }
  return nullptr;
}

/// Resolve a reference to a generic parameter to type metadata.
static const Metadata *resolveGenericParamRef(
                            const GenericParamRef &param,
                            SubstFlatGenericParameterFn substFlatGenericParam) {
  // Resolve the root generic parameter.
  const Metadata *current = substFlatGenericParam(param.getRootParamIndex());
  if (!current) return nullptr;

  // Follow the associated type path.
  for (const auto &assocTypeRef : param) {
    // Look for the witness table.
    auto witnessTable =
      swift_conformsToProtocol(current, assocTypeRef.Protocol);
    if (!witnessTable) return nullptr;

    // Retrieve the associated type.
    auto assocTypeReq = assocTypeRef.Requirement.get();
    current = swift_getAssociatedTypeWitness(
                                    MetadataState::Abstract,
                                    const_cast<WitnessTable *>(witnessTable),
                                    current,
                                    assocTypeRef.Protocol
                                      ->getRequirementBaseDescriptor(),
                                    assocTypeReq).Value;
    if (!current) return nullptr;
  }

  return current;
}

bool swift::_checkGenericRequirements(
          llvm::ArrayRef<GenericRequirementDescriptor> requirements,
          std::vector<const void *> &extraArguments,
          SubstFlatGenericParameterFn substFlatGenericParam,
          SubstGenericParameterFn substGenericParam,
          std::function<const WitnessTable *(unsigned index)> getWitnessTable) {
  unsigned conformanceReqIndex = 0;
  for (const auto &req : requirements) {
    // Make sure we understand the requirement we're dealing with.
    if (!req.hasKnownKind()) return true;

    // Resolve the subject generic parameter.
    auto subjectType =
      resolveGenericParamRef(req.getParam(), substFlatGenericParam);
    if (!subjectType) return true;

    // Check the requirement.
    switch (req.getKind()) {
    case GenericRequirementKind::Protocol: {
      // If we need a witness table, check whether the caller can provide it
      // directly.
      if (req.getProtocol().needsWitnessTable()) {
        if (auto witnessTable = getWitnessTable(conformanceReqIndex)) {
          extraArguments.push_back(witnessTable);
          ++conformanceReqIndex;
          continue;
        }
      }

      const WitnessTable *witnessTable = nullptr;
      if (!_conformsToProtocol(nullptr, subjectType, req.getProtocol(),
                               &witnessTable))
        return true;

      // If we need a witness table, add it.
      if (req.getProtocol().needsWitnessTable()) {
        assert(witnessTable);
        extraArguments.push_back(witnessTable);
        ++conformanceReqIndex;
      }

      continue;
    }

    case GenericRequirementKind::SameType: {
      // Demangle the second type under the given substitutions.
      auto otherType =
        _getTypeByMangledName(req.getMangledTypeName(), substGenericParam);
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
        _getTypeByMangledName(req.getMangledTypeName(), substGenericParam);
      if (!baseType) return true;

      // Check whether it's dynamically castable, which works as a superclass
      // check.
      // FIXME: We should be explicitly checking the superclass, so we
      // don't require the subject type to be complete.
      if (!swift_dynamicCastMetatype(subjectType, baseType)) return true;

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
                                          const ProtocolDescriptor *protocol) {
  const Metadata *conformingType = type;
  while (true) {
    const Metadata *superclass = _swift_class_getSuperclass(conformingType);
    if (!superclass)
      break;
    if (!swift_conformsToProtocol(superclass, protocol))
      break;
    conformingType = superclass;
  }

  return conformingType;
}

#define OVERRIDE_PROTOCOLCONFORMANCE COMPATIBILITY_OVERRIDE
#include "CompatibilityOverride.def"
