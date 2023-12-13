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

struct DistributedAccessibleFunctionsSection {
  const DistributedAccessibleFunctionRecord *__ptrauth_swift_accessible_function_record
      Begin;
  const DistributedAccessibleFunctionRecord *__ptrauth_swift_accessible_function_record
      End;

  DistributedAccessibleFunctionsSection(const DistributedAccessibleFunctionRecord *begin,
                             const DistributedAccessibleFunctionRecord *end)
      : Begin(begin), End(end) {}

  DistributedAccessibleFunctionsSection(const void *ptr, uintptr_t size) {
    auto bytes = reinterpret_cast<const char *>(ptr);
    Begin = reinterpret_cast<const DistributedAccessibleFunctionRecord *>(ptr);
    End = reinterpret_cast<const DistributedAccessibleFunctionRecord *>(bytes + size);
  }

  const DistributedAccessibleFunctionRecord *begin() const { return Begin; }
  const DistributedAccessibleFunctionRecord *end() const { return End; }
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

struct AccessibleFunctionsState {
  ConcurrentReadableHashMap<AccessibleFunctionCacheEntry> Cache;
  ConcurrentReadableArray<AccessibleFunctionsSection> SectionsToScan;
  ConcurrentReadableArray<DistributedAccessibleFunctionsSection> SectionsToScan2;

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

static void _registerDistributedAccessibleFunctions(AccessibleFunctionsState &C,
                                                    DistributedAccessibleFunctionsSection section) {
  C.SectionsToScan2.push_back(section);
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

void swift::addImageDistributedAccessibleFunctionsBlockCallbackUnsafe(
  const void *baseAddress,
    const void *dfunctions, uintptr_t dsize) {
  assert(
      dsize % sizeof(DistributedAccessibleFunctionRecord) == 0 &&
      "accessible distributed function section not a multiple of DistributedAccessibleFunctionRecord");

  auto &C = Functions.unsafeGetAlreadyInitialized();
  _registerDistributedAccessibleFunctions(C, DistributedAccessibleFunctionsSection{dfunctions, dsize});
}

void swift::addImageAccessibleFunctionsBlockCallback(
  const void *baseAddress,
    const void *functions, uintptr_t size) {
  Functions.get();
  addImageAccessibleFunctionsBlockCallbackUnsafe(
      baseAddress,
      functions, size);
}

void swift::addImageDistributedAccessibleFunctionsBlockCallback(
  const void *baseAddress,
    const void *functions, uintptr_t size) {
  Functions.get();
  addImageDistributedAccessibleFunctionsBlockCallbackUnsafe(
      baseAddress,
      functions, size);
}

static const AccessibleFunctionRecord *
_searchForFunctionRecord(AccessibleFunctionsState &S, llvm::StringRef name) {
  auto traceState = runtime::trace::accessible_function_scan_begin(name);
  fprintf(stderr, "[%s:%d](%s) ~~~~~ checking_searchForFunctionRecord\n", __FILE_NAME__, __LINE__, __FUNCTION__);

  for (const auto &section : S.SectionsToScan.snapshot()) {
    for (auto &record : section) {
      auto recordName =
          swift::Demangle::makeSymbolicMangledNameStringRef(record.Name.get());
      fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record: %s\n", __FILE_NAME__, __LINE__, __FUNCTION__,
              recordName.str().c_str());
      if (recordName == name)
        return traceState.end(&record);
    }
  }
  return nullptr;
}

static const DistributedAccessibleFunctionRecord *
_searchForDistributedFunctionRecord(AccessibleFunctionsState &S,
                                    std::optional<llvm::StringRef> actorTypeName,
                                    llvm::StringRef name) {
  auto traceState = runtime::trace::distributed_accessible_function_scan_begin(name);
  if (actorTypeName) {
    fprintf(stderr, "[%s:%d](%s) ~~~~~ checking_searchForFunctionRecord, NAME=%s, TY=%s\n", __FILE_NAME__, __LINE__, __FUNCTION__,
            name.str().c_str(),
            actorTypeName->str().c_str());
  } else {
    fprintf(stderr, "[%s:%d](%s) ~~~~~ checking_searchForFunctionRecord, NAME=%s\n", __FILE_NAME__, __LINE__, __FUNCTION__,
      name.str().c_str());
  }

  for (const auto &section : S.SectionsToScan2.snapshot()) {
    fprintf(stderr, "[%s:%d](%s) section...\n", __FILE_NAME__, __LINE__, __FUNCTION__);
    for (auto &record : section) {
    fprintf(stderr, "[%s:%d](%s) record...\n", __FILE_NAME__, __LINE__, __FUNCTION__);
      auto recordName =
          swift::Demangle::makeSymbolicMangledNameStringRef(record.Name.get());
        fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [OK PROTOCOL...]: %s\n",
                __FILE_NAME__, __LINE__, __FUNCTION__,
                recordName.str().c_str());

      if (recordName == name) {
        fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [OK PROTOCOL...]: %s\n",
        __FILE_NAME__, __LINE__, __FUNCTION__, "NAME OK");
        if (actorTypeName) {
          fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [OK PROTOCOL...]: %s\n",
                  __FILE_NAME__, __LINE__, __FUNCTION__, "HAVE TYPE");
          auto recordConcreteActorName =
              swift::Demangle::makeSymbolicMangledNameStringRef(
                  record.ConcreteActorName.get());
          if (recordConcreteActorName.endswith(*actorTypeName)) { // FIXME: this is missing the "$s" on right side
            fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [OK PROTOCOL...]: %s\n",
                    __FILE_NAME__, __LINE__, __FUNCTION__, "TYPE OK");
            return traceState.end(&record);
          } else {
            fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [OK PROTOCOL...]: %s; %s != %s\n",
                    __FILE_NAME__, __LINE__, __FUNCTION__, "TYPE BAD",
                    recordConcreteActorName.str().c_str(),
                    actorTypeName->str().c_str());
          }
        } else {
          // it's a call directly identified by the record/method name,
          // not a protocol call, so we return the record...
          assert(false); // FIXME: what to do here
          return traceState.end(&record);
        }
      } else {
        fprintf(stderr, "[%s:%d](%s) ~~~~~ checking record [no]: %s\n",
                __FILE_NAME__, __LINE__, __FUNCTION__,
                recordName.str().c_str());
      }
    }
  }

  fprintf(stderr, "[%s:%d](%s) ~~~~~ checking_searchForFunctionRecord, NOPE\n", __FILE_NAME__, __LINE__, __FUNCTION__);

  return nullptr;
}

SWIFT_RUNTIME_STDLIB_SPI
const AccessibleFunctionRecord *
swift::runtime::swift_findAccessibleFunction(const char *targetNameStart,
                                             size_t targetNameLength) {
  auto &S = Functions.get();

  llvm::StringRef name{targetNameStart, targetNameLength};

  // Look for an existing entry.
  {
    auto snapshot = S.Cache.snapshot();
    if (auto E = snapshot.find(name))
      return E->getRecord();
  }

  // If entry doesn't exist (either record doesn't exist, hasn't been loaded, or
  // requested yet), let's try to find it and add to the cache.

  auto *record = _searchForFunctionRecord(S, name);
  if (record) {
    S.Cache.getOrInsert(
        name, [&](AccessibleFunctionCacheEntry *entry, bool created) {
          if (created)
            ::new (entry) AccessibleFunctionCacheEntry{name, record};
          return true;
        });
  }

  return record;
}

SWIFT_RUNTIME_STDLIB_SPI
const DistributedAccessibleFunctionRecord *
swift::runtime::swift_findDistributedAccessibleFunction(
    const char *targetNameStart, size_t targetNameLength,
    const char *actorTypeNameStart, size_t actorTypeNameLength) {
  auto &S = Functions.get();

  llvm::StringRef targetMethodName{targetNameStart, targetNameLength};
  llvm::StringRef actorTypeName{actorTypeNameStart, actorTypeNameLength};

  // TODO: re-enable caches
  // Look for an existing entry.
//  {
//    auto snapshot = S.Cache.snapshot();
//    if (auto E = snapshot.find(name))
//      return E->getRecord();
//  }

  // If entry doesn't exist (either record doesn't exist, hasn't been loaded, or
  // requested yet), let's try to find it and add to the cache.

  // auto *record = _searchForFunctionRecord(S, name);
  auto *record = _searchForDistributedFunctionRecord(S, actorTypeName, targetMethodName);
//  if (record) { // FIXME: enable this
//    S.Cache.getOrInsert(
//        name, [&](AccessibleFunctionCacheEntry *entry, bool created) {
//          if (created)
//            ::new (entry) DistributedAccessibleFunctionCacheEntry{name, record};
//          return true;
//        });
//  }

  return record;
}
