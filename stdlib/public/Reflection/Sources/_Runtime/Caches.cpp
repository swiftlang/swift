//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

// Only as SPI for Darwin platforms for right now...
#if defined(__MACH__)

#include "swift/ABI/Metadata.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Config.h"
#include "swift/Runtime/ImageInspection.h"
#include "swift/Runtime/ImageInspectionMachO.h"
#include <cstdint>
#include <unordered_map>

using namespace swift;

//===----------------------------------------------------------------------===//
// Protocol Conformance Cache
//===----------------------------------------------------------------------===//

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
}

struct ConformanceCache {
  // All accesses to Cache and LastSectionCount must be within CacheMutex's
  // lock scope.

  std::unordered_map<
    /* Key */   const ProtocolDescriptor *,
    /* Value */ std::vector<const Metadata *>
  > Cache;
  Mutex CacheMutex;
  ConcurrentReadableArray<ConformanceSection> Sections;

  size_t LastSectionCount = 0;

  ConformanceCache() {
    initializeProtocolConformanceLookup();
  }
};

static Lazy<ConformanceCache> Conformances;

void swift::addImageProtocolConformanceBlockCallbackUnsafe(
    const void *baseAddress,
    const void *conformances,
    uintptr_t conformancesSize) {
  assert(conformancesSize % sizeof(ProtocolConformanceRecord) == 0 &&
         "conformances section not a multiple of ProtocolConformanceRecord");

  // Conformance cache should always be sufficiently initialized by this point.
  auto &C = Conformances.unsafeGetAlreadyInitialized();

  C.Sections.push_back(ConformanceSection{conformances, conformancesSize});
}

// WARNING: the callbacks are called from unsafe contexts (with the dyld and
// ObjC runtime locks held) and must be very careful in what they do. Locking
// must be arranged to avoid deadlocks (other code must never call out to dyld
// or ObjC holding a lock that gets taken in one of these callbacks) and the
// new/delete operators must not be called, in case a program supplies an
// overload which does not cooperate with these requirements.

void swift::initializeProtocolConformanceLookup() {
  REGISTER_FUNC(
      addImageCallback<TextSegment, ProtocolConformancesSection,
                       addImageProtocolConformanceBlockCallbackUnsafe>);
}

static const Metadata *_getCanonicalTypeMetadata(
                             const ProtocolConformanceDescriptor *conformance) {
  switch (conformance->getTypeKind()) {
  case TypeReferenceKind::DirectTypeDescriptor:
  case TypeReferenceKind::IndirectTypeDescriptor: {
    if (auto anyType = conformance->getTypeDescriptor()) {
      if (auto type = dyn_cast<TypeContextDescriptor>(anyType)) {
        if (!type->isGeneric()) {
          if (auto accessFn = type->getAccessFunction()) {
            return accessFn(MetadataState::Abstract).Value;
          }
        }
      }
    }

    return nullptr;
  }

  case TypeReferenceKind::DirectObjCClassName:
  case TypeReferenceKind::IndirectObjCClass:
    return nullptr;
  }
}

using ConformanceCacheCallback = void (*)(const Metadata **,
                                          size_t, void *);

SWIFT_RUNTIME_STDLIB_SPI SWIFT_CC(swift)
void _swift_reflection_withConformanceCache(const ProtocolDescriptor *proto,
                                            void *context,
                                            ConformanceCacheCallback callback) {
  auto &C = Conformances.get();

  auto snapshot = C.Sections.snapshot();

  Mutex::ScopedLock lock(C.CacheMutex);

  if (C.LastSectionCount > 0 && snapshot.count() <= C.LastSectionCount) {
    auto entry = C.Cache.find(proto);

    if (entry != C.Cache.end()) {
      callback(entry->second.data(), entry->second.size(), context);
      return;
    }
  }

  std::vector<const Metadata *> types = {};

  for (auto &section : snapshot) {
    for (auto &record : section) {
      auto conformance = record.get();

      if (conformance->getProtocol() != proto) {
        continue;
      }

      if (conformance->hasConditionalRequirements()) {
        continue;
      }

      if (auto type = _getCanonicalTypeMetadata(conformance)) {
        types.push_back(type);
      }
    }
  }

  callback(types.data(), types.size(), context);

  C.Cache[proto] = types;
  C.LastSectionCount = snapshot.count();
}

#endif
