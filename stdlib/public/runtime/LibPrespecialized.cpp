//===--- LibPrespecialized.cpp - Interface for prespecializations----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Runtime/LibPrespecialized.h"
#include "MetadataCache.h"
#include "Private.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Metadata.h"

#include <atomic>

#if SWIFT_STDLIB_HAS_DLADDR && __has_include(<dlfcn.h>)
#include <dlfcn.h>
#define USE_DLOPEN 1
#endif

#if __has_include(<mach-o/dyld_priv.h>)
#include <mach-o/dyld_priv.h>
#endif

using namespace swift;

static std::atomic<bool> disablePrespecializedMetadata = false;

static bool prespecializedLoggingEnabled = false;

#define LOG(fmt, ...)                                                          \
  do {                                                                         \
    if (SWIFT_UNLIKELY(prespecializedLoggingEnabled))                          \
      fprintf(stderr, "Prespecializations library: " fmt "\n", __VA_ARGS__);   \
  } while (0)

static bool environmentProcessListContainsProcess(const char *list,
                                                  const char *progname) {
  auto prognameLen = strlen(progname);

  const char *cursor = list;
  while (true) {
    const char *next = strchr(cursor, ':');
    if (!next) {
      // Last entry in the list. Compare with the entire rest of the string.
      return strcmp(progname, cursor) == 0;
    }

    // Entry at beginning or middle of the list. Compare against this substring.
    size_t len = next - cursor;
    if (len == prognameLen && strncmp(cursor, progname, len) == 0)
      return true;

    cursor = next + 1;
  }
}

static bool isThisProcessEnabled(const LibPrespecializedData<InProcess> *data) {
  extern const char *__progname;

  if (!__progname)
    return true;

  auto envEnabledProcesses =
      runtime::environment::SWIFT_DEBUG_LIB_PRESPECIALIZED_ENABLED_PROCESSES();
  if (envEnabledProcesses && *envEnabledProcesses) {
    if (environmentProcessListContainsProcess(envEnabledProcesses,
                                              __progname)) {
      LOG("Found %s in SWIFT_DEBUG_LIB_PRESPECIALIZED_ENABLED_PROCESSES, "
          "enabling",
          __progname);
      return true;
    }
  }

  auto envDisabledProcesses =
      runtime::environment::SWIFT_DEBUG_LIB_PRESPECIALIZED_DISABLED_PROCESSES();
  if (envDisabledProcesses && *envDisabledProcesses) {
    if (environmentProcessListContainsProcess(envDisabledProcesses,
                                              __progname)) {
      LOG("Found %s in SWIFT_DEBUG_LIB_PRESPECIALIZED_DISABLED_PROCESSES, "
          "disabling",
          __progname);
      return false;
    }
  }

  if (auto *disabledProcesses = data->getDisabledProcessesTable()) {
    auto *cursor = disabledProcesses;
    while (auto *name = *cursor) {
      if (strcmp(name, __progname) == 0) {
        LOG("Found %s in disabled processes list, disabling", name);
        return false;
      }
      cursor++;
    }
  }

  return true;
}

static const LibPrespecializedData<InProcess> *findLibPrespecialized() {
  if (!runtime::environment::SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED()) {
    LOG("Disabling, SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED = %d",
        runtime::environment::SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED());
    return nullptr;
  }

  const void *dataPtr = nullptr;
#if USE_DLOPEN
  auto path = runtime::environment::SWIFT_DEBUG_LIB_PRESPECIALIZED_PATH();
  if (path && path[0]) {
    // Use RTLD_NOLOAD to avoid actually loading the library. We just want to
    // find it if it has already been loaded by other means, such as
    // DYLD_INSERT_LIBRARIES.
    void *handle = dlopen(path, RTLD_LAZY | RTLD_NOLOAD);
    if (!handle) {
      swift::warning(0, "Failed to load prespecializations library: %s\n",
                     dlerror());
      return nullptr;
    }

    dataPtr = dlsym(handle, LIB_PRESPECIALIZED_TOP_LEVEL_SYMBOL_NAME);
    LOG("Loaded custom library from %s, found dataPtr %p", path, dataPtr);
  }
#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  else if (SWIFT_RUNTIME_WEAK_CHECK(_dyld_get_swift_prespecialized_data)) {
    // Disable the prespecializations library if anything in the shared cache is
    // overridden. Eventually we want to be cleverer and only disable the
    // prespecializations that have been invalidated, but we'll start with the
    // simplest approach.
    if (!dyld_shared_cache_some_image_overridden()) {
      dataPtr = SWIFT_RUNTIME_WEAK_USE(_dyld_get_swift_prespecialized_data());
      LOG("Got dataPtr %p from _dyld_get_swift_prespecialized_data", dataPtr);
    } else {
      LOG("Not calling _dyld_get_swift_prespecialized_data "
          "dyld_shared_cache_some_image_overridden = %d",
          dyld_shared_cache_some_image_overridden());
    }
  }
#endif
#endif

  if (!dataPtr)
    return nullptr;

  auto *data =
      reinterpret_cast<const LibPrespecializedData<InProcess> *>(dataPtr);
  if (data->majorVersion !=
      LibPrespecializedData<InProcess>::currentMajorVersion) {
    LOG("Unknown major version %" PRIu32 ", disabling", data->majorVersion);
    return nullptr;
  }

  if (!isThisProcessEnabled(data))
    return nullptr;

  LOG("Returning data %p, major version %" PRIu32 " minor %" PRIu32, data,
      data->majorVersion, data->minorVersion);

  return data;
}

struct LibPrespecializedState {
  struct AddressRange {
    uintptr_t start, end;

    bool contains(const void *ptr) {
      return start <= (uintptr_t)ptr && (uintptr_t)ptr < end;
    }
  };

  const LibPrespecializedData<InProcess> *data;
  AddressRange sharedCacheRange{0, 0};
  AddressRange metadataAllocatorInitialPoolRange{0, 0};

  LibPrespecializedState() {
    prespecializedLoggingEnabled =
        runtime::environment::SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_LOGGING();
    data = findLibPrespecialized();

#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
    size_t sharedCacheLength;
    sharedCacheRange.start =
        (uintptr_t)_dyld_get_shared_cache_range(&sharedCacheLength);
    sharedCacheRange.end = sharedCacheRange.start + sharedCacheLength;

    auto [initialPoolStart, initialPoolLength] =
        MetadataAllocator::InitialPoolLocation();
    metadataAllocatorInitialPoolRange.start = (uintptr_t)initialPoolStart;
    metadataAllocatorInitialPoolRange.end =
        metadataAllocatorInitialPoolRange.start + initialPoolLength;
#endif
  }
};

static Lazy<LibPrespecializedState> LibPrespecialized;

const LibPrespecializedData<InProcess> *swift::getLibPrespecializedData() {
  return SWIFT_LAZY_CONSTANT(findLibPrespecialized());
}

// Returns true if the type has any arguments that aren't plain types (packs or
// unknown kinds).
static bool
hasNonTypeGenericArguments(const TargetGenericContext<InProcess> *generics) {
  for (auto param : generics->getGenericParams())
    if (param.getKind() != GenericParamKind::Type)
      return true;

  return false;
}

static bool
isPotentialPrespecializedPointer(LibPrespecializedState &prespecialized,
                                 const void *pointer) {
  // Prespecialized metadata descriptors and arguments are always in the shared
  // cache. They're either statically emitted metadata, or they're
  // prespecialized metadata. Anything that's dynamically allocated, or
  // statically allocated outside the shared cache, is not a possible candidate.

  // If we're loading a debug libprespecialized, we can't do these checks, so
  // just say everything is a potential argument. Performance is not so
  // important in that case.
  if (!prespecialized.sharedCacheRange.contains(prespecialized.data))
    return true;

  // Anything outside the shared cache isn't a potential argument.
  if (!prespecialized.sharedCacheRange.contains(pointer))
    return false;

  // Dynamically allocated metadata could be within the shared cache, in the
  // initial metadata allocation pool. Reject anything in that region.
  if (prespecialized.metadataAllocatorInitialPoolRange.contains(pointer))
    return false;

  return true;
}

static bool disableForValidation = false;

void
swift::libPrespecializedImageLoaded() {
  #if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  // A newly loaded image might have caused us to load images that are
  // overriding images in the shared cache.  If we do that, turn off
  // prespecialized metadata.
  if (dyld_shared_cache_some_image_overridden())
    disablePrespecializedMetadata.store(true, std::memory_order_release);
  #endif
}

Metadata *
swift::getLibPrespecializedMetadata(const TypeContextDescriptor *description,
                                    const void *const *arguments) {
  if (SWIFT_UNLIKELY(
        disableForValidation
        || disablePrespecializedMetadata.load(std::memory_order_acquire)))
    return nullptr;

  auto &prespecialized = LibPrespecialized.get();

  auto *data = prespecialized.data;
  if (!data)
    return nullptr;

  auto *generics = description->getGenericContext();
  if (!generics)
    return nullptr;

  // We don't support types with pack parameters yet (and especially not types
  // with unknown parameter kinds) so don't even try to look those up.
  if (hasNonTypeGenericArguments(generics))
    return nullptr;

  if (!isPotentialPrespecializedPointer(prespecialized, description)) {
    LOG("Rejecting descriptor %p, not in the shared cache",
        (const void *)description);
    return nullptr;
  }

  auto numKeyArguments = generics->getGenericContextHeader().NumKeyArguments;
  for (unsigned i = 0; i < numKeyArguments; i++) {
    if (!isPotentialPrespecializedPointer(prespecialized, arguments[i])) {
      LOG("Rejecting argument %u %p to descriptor %p, not in the shared cache",
          i, arguments[i], (const void *)description);
      return nullptr;
    }
  }

  Demangler dem;
  auto mangleNode = _buildDemanglingForGenericType(description, arguments, dem);
  if (!mangleNode) {
    LOG("failed to build demangling with descriptor %p.", description);
    return nullptr;
  }

  if (mangleNode->getKind() != Node::Kind::Global) {
    auto wrapper = dem.createNode(Node::Kind::Global);
    wrapper->addChild(mangleNode, dem);
    mangleNode = wrapper;
  }
  auto resolver = [](SymbolicReferenceKind kind,
                     const void *ref) -> NodePointer {
    swift::fatalError(0,
                      "Unexpected symbolic reference %p in generated mangle "
                      "tree for generic type lookup.",
                      ref);
  };
  auto mangling = Demangle::mangleNode(mangleNode, resolver, dem);
  if (!mangling.isSuccess()) {
    swift::warning(0,
                   "Mangling for prespecialized metadata failed with code %d",
                   mangling.error().code);
    return nullptr;
  }

  auto key = mangling.result();
  auto *metadataMap = data->getMetadataMap();
  auto *element = metadataMap->find(key.data(), key.size());
  auto *result = element ? element->value : nullptr;
  LOG("found %p for key '%.*s'.", result, (int)key.size(), key.data());
  return result;
}

void _swift_validatePrespecializedMetadata() {
  auto *data = getLibPrespecializedData();
  if (!data) {
    return;
  }

  disableForValidation = true;

  unsigned validated = 0;
  unsigned failed = 0;

  auto *metadataMap = data->getMetadataMap();
  auto metadataMapSize = metadataMap->arraySize;
  auto *array = metadataMap->array();
  for (uint64_t i = 0; i < metadataMapSize; i++) {
    auto &element = array[i];
    if (!element.key || !element.value)
      continue;

    validated++;

    const char *mangledName = element.key;
    // Skip the leading $.
    if (mangledName[0] == '$')
      mangledName++;

    auto result = swift_getTypeByMangledName(MetadataState::Complete,
                                             mangledName, nullptr, {}, {});
    if (auto *error = result.getError()) {
      fprintf(stderr,
              "Prespecializations library validation: unable to build metadata "
              "for mangled name '%s'\n",
              mangledName);
      failed++;
      continue;
    }

    if (!compareGenericMetadata(result.getType().getMetadata(), element.value))
      failed++;
  }

  fprintf(stderr,
          "Prespecializations library validation: validated %u entries, %u "
          "failures.\n",
          validated, failed);
}
