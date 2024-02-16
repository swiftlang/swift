//===---- AccessibleFunction.cpp - Swift protocol conformance checking ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Checking and caching of Swift accessible functions.
//
//===----------------------------------------------------------------------===//

#include "ImageInspection.h"
#include "Private.h"
#include "swift/Basic/Lazy.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Runtime/AccessibleFunction.h"
#include "swift/Runtime/Concurrent.h"
#include "swift/Runtime/Metadata.h"
#include "Tracing.h"

#include <cstdint>
#include <new>

using namespace swift;

#pragma mark Accessible function cache
namespace {

struct AccessibleFunctionsSection {
  const AccessibleFunctionRecord *__ptrauth_swift_accessible_function_record
      Begin;
  const AccessibleFunctionRecord *__ptrauth_swift_accessible_function_record
      End;

  AccessibleFunctionsSection(const AccessibleFunctionRecord *begin,
                             const AccessibleFunctionRecord *end)
      : Begin(begin), End(end) {}

  AccessibleFunctionsSection(const void *ptr, uintptr_t size) {
    auto bytes = reinterpret_cast<const char *>(ptr);
    Begin = reinterpret_cast<const AccessibleFunctionRecord *>(ptr);
    End = reinterpret_cast<const AccessibleFunctionRecord *>(bytes + size);
  }

  const AccessibleFunctionRecord *begin() const { return Begin; }
  const AccessibleFunctionRecord *end() const { return End; }
};

struct AccessibleProtocolFunctionsSection {
  const AccessibleProtocolRequirementFunctionRecord
      *__ptrauth_swift_accessible_protocol_requirement_function_record Begin;
  const AccessibleProtocolRequirementFunctionRecord
      *__ptrauth_swift_accessible_protocol_requirement_function_record End;

  AccessibleProtocolFunctionsSection(
      const AccessibleProtocolRequirementFunctionRecord *begin,
      const AccessibleProtocolRequirementFunctionRecord *end)
      : Begin(begin), End(end) {}

  AccessibleProtocolFunctionsSection(const void *ptr, uintptr_t size) {
    auto bytes = reinterpret_cast<const char *>(ptr);
    Begin =
        reinterpret_cast<const AccessibleProtocolRequirementFunctionRecord *>(
            ptr);
    End = reinterpret_cast<const AccessibleProtocolRequirementFunctionRecord *>(
        bytes + size);
  }

  const AccessibleProtocolRequirementFunctionRecord *begin() const {
    return Begin;
  }
  const AccessibleProtocolRequirementFunctionRecord *end() const { return End; }
};

struct AccessibleFunctionCacheEntry {
private:
  const char *Name;
  size_t NameLength;

  const AccessibleFunctionRecord *__ptrauth_swift_accessible_function_record R;

public:
  AccessibleFunctionCacheEntry(llvm::StringRef name,
                               const AccessibleFunctionRecord *record)
      : R(record) {
    char *Name = reinterpret_cast<char *>(malloc(name.size()));
    memcpy(Name, name.data(), name.size());

    this->Name = Name;
    this->NameLength = name.size();
  }

  const AccessibleFunctionRecord *getRecord() const { return R; }

  bool matchesKey(llvm::StringRef name) {
    return name == llvm::StringRef{Name, NameLength};
  }

  friend llvm::hash_code hash_value(const AccessibleFunctionCacheEntry &value) {
    return hash_value(llvm::StringRef{value.Name, value.NameLength});
  }

  template <class... T>
  static size_t getExtraAllocationSize(T &&...ignored) {
    return 0;
  }
};

struct AccessibleProtocolFunctionCacheKey {
public:
  llvm::StringRef TypeName;
  llvm::StringRef TargetName;

  friend llvm::hash_code
  hash_value(const AccessibleProtocolFunctionCacheKey &value) {
    return hash_value(llvm::hash_combine(value.TypeName, value.TargetName));
  }
};
struct AccessibleProtocolFunctionCacheEntry {
private:
  const char *TypeName; // TODO(distributed): Optimize and use metadata pointer
  size_t TypeNameLength;

  const char *TargetName;
  size_t TargetNameLength;

  const AccessibleProtocolRequirementFunctionRecord
      *__ptrauth_swift_accessible_protocol_requirement_function_record R;

public:
  AccessibleProtocolFunctionCacheEntry(
      llvm::StringRef typeName, llvm::StringRef targetName,
      const AccessibleProtocolRequirementFunctionRecord *record)
      : R(record) {
    char *TheTypeName = reinterpret_cast<char *>(malloc(typeName.size()));
    memcpy(TheTypeName, typeName.data(), typeName.size());
    this->TypeName = TheTypeName;
    this->TypeNameLength = typeName.size();

    char *TheTargetName = reinterpret_cast<char *>(malloc(targetName.size()));
    memcpy(TheTargetName, targetName.data(), targetName.size());
    this->TargetName = TheTargetName;
    this->TargetNameLength = targetName.size();
  }

  const AccessibleProtocolRequirementFunctionRecord *getRecord() const {
    return R;
  }

  bool matchesKey(AccessibleProtocolFunctionCacheKey key) {
    return key.TypeName == llvm::StringRef{TypeName, TypeNameLength} &&
           key.TargetName == llvm::StringRef{TargetName, TargetNameLength};
  }

  friend llvm::hash_code
  hash_value(const AccessibleProtocolFunctionCacheEntry &value) {
    return hash_value(llvm::hash_combine(
        llvm::StringRef{value.TypeName, value.TypeNameLength},
        llvm::StringRef{value.TargetName, value.TargetNameLength}));
  }

  template <class... T>
  static size_t getExtraAllocationSize(T &&...ignored) {
    return 0;
  }
};

struct AccessibleFunctionsState {
  ConcurrentReadableHashMap<AccessibleFunctionCacheEntry> FunctionCache;
  ConcurrentReadableHashMap<AccessibleProtocolFunctionCacheEntry>
      ProtocolRequirementFunctionCache;
  ConcurrentReadableArray<AccessibleFunctionsSection> SectionsToScan;
  ConcurrentReadableArray<AccessibleProtocolFunctionsSection>
      WitnessSectionsToScan;

  AccessibleFunctionsState() {
    initializeAccessibleFunctionsLookup();
  }
};

static Lazy<AccessibleFunctionsState> Functions;

} // end anonymous namespace

static void _registerAccessibleFunctions(AccessibleFunctionsState &C,
                                         AccessibleFunctionsSection section) {
  C.SectionsToScan.push_back(section);
}

static void _registerAccessibleProtocolFunctions(
    AccessibleFunctionsState &C, AccessibleProtocolFunctionsSection section) {
  C.WitnessSectionsToScan.push_back(section);
}

void swift::addImageAccessibleFunctionsBlockCallbackUnsafe(
  const void *baseAddress,
    const void *functions, uintptr_t size) {
  assert(
      size % sizeof(AccessibleFunctionRecord) == 0 &&
      "accessible function section not a multiple of AccessibleFunctionRecord");

  auto &C = Functions.unsafeGetAlreadyInitialized();
  _registerAccessibleFunctions(C, AccessibleFunctionsSection{functions, size});
}

void swift::addImageAccessibleProtocolFunctionsBlockCallbackUnsafe(
    const void *baseAddress, const void *dfunctions, uintptr_t dsize) {
  assert(dsize % sizeof(AccessibleProtocolRequirementFunctionRecord) == 0 &&
         "accessible protocol function section not a multiple of "
         "AccessibleProtocolRequirementFunctionRecord");

  auto &C = Functions.unsafeGetAlreadyInitialized();
  _registerAccessibleProtocolFunctions(
      C, AccessibleProtocolFunctionsSection{dfunctions, dsize});
}

void swift::addImageAccessibleFunctionsBlockCallback(
  const void *baseAddress,
    const void *functions, uintptr_t size) {
  Functions.get();
  addImageAccessibleFunctionsBlockCallbackUnsafe(baseAddress, functions, size);
}

void swift::addImageAccessibleProtocolFunctionsBlockCallback(
    const void *baseAddress, const void *functions, uintptr_t size) {
  Functions.get();
  addImageAccessibleProtocolFunctionsBlockCallbackUnsafe(baseAddress, functions,
                                                         size);
}

static const AccessibleFunctionRecord *
_searchForFunctionRecord(AccessibleFunctionsState &S, llvm::StringRef name) {
  auto traceState = runtime::trace::accessible_function_scan_begin(name);

  for (const auto &section : S.SectionsToScan.snapshot()) {
    for (auto &record : section) {
      auto recordName =
          swift::Demangle::makeSymbolicMangledNameStringRef(record.Name.get());
      if (recordName == name) {
        return traceState.end(&record);
      }
    }
  }
  return nullptr;
}

static const AccessibleProtocolRequirementFunctionRecord *
_searchForProtocolRequirementFunctionRecord(
    AccessibleFunctionsState &S, llvm::Optional<llvm::StringRef> actorTypeName,
    llvm::StringRef targetFuncName) {
  auto traceState = runtime::trace::accessible_protocol_requirement_function_scan_begin(
      targetFuncName);
  for (const auto &section : S.WitnessSectionsToScan.snapshot()) {
    for (auto &record : section) {
      llvm::StringRef recordName =
          swift::Demangle::makeSymbolicMangledNameStringRef(record.Name.get());

      if (recordName == targetFuncName) {
        if (actorTypeName) {
          auto recordConcreteActorName =
              swift::Demangle::makeSymbolicMangledNameStringRef(
                  record.ConcreteActorName.get());
          // FIXME: this is missing the "$s" on right side
          if (recordConcreteActorName.endswith(*actorTypeName)) {
            return traceState.end(&record);
          }
        }
      }
    }
  }

  return nullptr;
}

SWIFT_RUNTIME_STDLIB_SPI
const AccessibleFunctionRecord *
swift::runtime::swift_findAccessibleFunctionForConcreteType(
    bool findConcreteWitness, const char *targetActorTypeNameStart,
    size_t targetActorTypeNameLength, const char *targetNameStart,
    size_t targetNameLength) {
  auto &S = Functions.get();

  llvm::StringRef actorName{targetActorTypeNameStart,
                            targetActorTypeNameLength};
  llvm::StringRef targetFuncName{targetNameStart, targetNameLength};

  // Look for an existing entry by name only
  if (actorName.empty()) {
    // Check cached functions, we may be looking for a concrete function
    // directly
    auto snapshot = S.FunctionCache.snapshot();
    if (auto E = snapshot.find(targetFuncName)) {
      return E->getRecord();
    }
  } else {
    // Check cached protocol requirement functions
    auto snapshot = S.ProtocolRequirementFunctionCache.snapshot();
    AccessibleProtocolFunctionCacheKey actorAndTarget = {actorName,
                                                         targetFuncName};
    if (auto E = snapshot.find(actorAndTarget)) {
      return E->getRecord();
    }
  }

  // If entry doesn't exist (either record doesn't exist, hasn't been loaded, or
  // requested yet), let's try to find it and add to the cache.
  // FIXME: fix up the caching scheme we do here; Cache the protocol funcs too.
  auto *record = _searchForFunctionRecord(S, targetFuncName);
  if (record) {
    if (actorName.empty()) {
      S.FunctionCache.getOrInsert(
          targetFuncName,
          [&](AccessibleFunctionCacheEntry *entry, bool created) {
            if (created)
              ::new (entry)
                  AccessibleFunctionCacheEntry{targetFuncName, record};
            return true;
          });
    }
  }

  if (!record) {
    // We did not find it directly, so let's scan protocol requirement funcs
    auto requirementFuncRecord = _searchForProtocolRequirementFunctionRecord(
        S, actorName, targetFuncName);

    if (findConcreteWitness && requirementFuncRecord) {
      assert(requirementFuncRecord->ConcreteWitnessMethodName);
      auto witnessFuncName = swift::Demangle::makeSymbolicMangledNameStringRef(
          requirementFuncRecord->ConcreteWitnessMethodName.get());

      auto concreteRecord = swift_findAccessibleFunctionForConcreteType(
          // don't try again, no recursive trying again forever
          /*findConcreteWitness=*/false, targetActorTypeNameStart,
          targetActorTypeNameLength, witnessFuncName.data(),
          witnessFuncName.size());

      return concreteRecord;
    }
  }

  return record;
}

SWIFT_RUNTIME_STDLIB_SPI
const AccessibleFunctionRecord *
swift::runtime::swift_findAccessibleFunction(const char *targetNameStart,
                                             size_t targetNameLength) {
  return swift_findAccessibleFunctionForConcreteType(
      /*findConcreteWitness=*/false,
      /*targetActorTypeNameStart=*/nullptr, /*targetActorTypeNameLength=*/0,
      targetNameStart, targetNameLength);
}
