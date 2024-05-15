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

#if __has_include(<os/feature_private.h>)
#include <os/feature_private.h> // for os_feature_enabled_simple()
#define HAS_OS_FEATURE 1
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

    bool contains(const void *ptr) const {
      return start <= (uintptr_t)ptr && (uintptr_t)ptr < end;
    }
  };

  enum class MapConfiguration {
    UseNameKeyedMap,
    UsePointerKeyedMap,
    UsePointerKeyedMapDebugMode,
    Disabled,
  };

  const LibPrespecializedData<InProcess> *data;
  MapConfiguration mapConfiguration;
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

    // Must do this after the shared cache range has been retrieved.
    mapConfiguration = computeMapConfiguration(data);
  }

  MapConfiguration
  computeMapConfiguration(const LibPrespecializedData<InProcess> *data) {
    // If no data, we have to disable.
    if (!data)
      return MapConfiguration::Disabled;

    auto nameKeyedMap = data->getMetadataMap();
    auto pointerKeyedMap = data->getPointerKeyedMetadataMap();

    // If we don't have either map, then disable it completely.
    if (!nameKeyedMap && !pointerKeyedMap) {
      LOG("No prespecializations map available from data at %p, disabling.",
          data);
      return MapConfiguration::Disabled;
    }

    // If we don't have the pointer-keyed map, fall back to the name-keyed map.
    if (!pointerKeyedMap) {
      LOG("Data at %p only contains name-keyed map.", data);
      return MapConfiguration::UseNameKeyedMap;
    }

    // If we don't have the name-keyed map, always use the pointer-keyed map.
    if (!nameKeyedMap) {
      LOG("Data at %p only contains pointer-keyed map.", data);
      return MapConfiguration::UsePointerKeyedMap;
    }

    // We have both. Consult the option flag.
    bool usePointerKeyedMap =
        data->getOptionFlags() &
        LibPrespecializedData<InProcess>::OptionFlagDefaultToPointerKeyedMap;

#if HAS_OS_FEATURE
    if (os_feature_enabled_simple(Swift, useAlternatePrespecializationMap,
                                  false))
      usePointerKeyedMap = !usePointerKeyedMap;
#endif

    LOG("Data at %p contains both maps. Using %s keyed map.", data,
        usePointerKeyedMap ? "pointer" : "name");
    if (usePointerKeyedMap) {
      // If we're using a map outside the shared cache, then we're in debug mode
      // and need to use our own slow lookup.
      if (!sharedCacheRange.contains(pointerKeyedMap))
        return MapConfiguration::UsePointerKeyedMapDebugMode;
      return MapConfiguration::UsePointerKeyedMap;
    }
    return MapConfiguration::UseNameKeyedMap;
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
isPotentialPrespecializedPointer(const LibPrespecializedState &state,
                                 const void *pointer) {
  // Prespecialized metadata descriptors and arguments are always in the shared
  // cache. They're either statically emitted metadata, or they're
  // prespecialized metadata. Anything that's dynamically allocated, or
  // statically allocated outside the shared cache, is not a possible candidate.

  // If we're loading a debug libprespecialized, we can't do these checks, so
  // just say everything is a potential argument. Performance is not so
  // important in that case.
  if (!state.sharedCacheRange.contains(state.data))
    return true;

  // Anything outside the shared cache isn't a potential argument.
  if (!state.sharedCacheRange.contains(pointer))
    return false;

  // Dynamically allocated metadata could be within the shared cache, in the
  // initial metadata allocation pool. Reject anything in that region.
  if (state.metadataAllocatorInitialPoolRange.contains(pointer))
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

static Metadata *
getMetadataFromNameKeyedMap(const LibPrespecializedState &state,
                            const TypeContextDescriptor *description,
                            const void *const *arguments) {
  auto *generics = description->getGenericContext();
  if (!generics)
    return nullptr;

  // We don't support types with pack parameters yet (and especially not types
  // with unknown parameter kinds) so don't even try to look those up.
  if (hasNonTypeGenericArguments(generics))
    return nullptr;

  if (!isPotentialPrespecializedPointer(state, description)) {
    LOG("Rejecting descriptor %p, not in the shared cache",
        (const void *)description);
    return nullptr;
  }

  auto numKeyArguments = generics->getGenericContextHeader().NumKeyArguments;
  for (unsigned i = 0; i < numKeyArguments; i++) {
    if (!isPotentialPrespecializedPointer(state, arguments[i])) {
      LOG("Rejecting argument %u %p to descriptor %p, not in the shared cache",
          i, arguments[i], (const void *)description);
      return nullptr;
    }
  }

  StackAllocatedDemangler<4096> dem;
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
  auto *metadataMap = state.data->getMetadataMap();
  auto *element = metadataMap->find(key.data(), key.size());
  auto *result = element ? element->value : nullptr;
  LOG("found %p for key '%.*s'.", result, (int)key.size(), key.data());
  return result;
}

static Metadata *
getMetadataFromPointerKeyedMap(const LibPrespecializedState &state,
                               const TypeContextDescriptor *description,
                               const void *const *arguments) {
#if DYLD_FIND_POINTER_HASH_TABLE_ENTRY_DEFINED
  auto *generics = description->getGenericContext();
  if (!generics)
    return nullptr;

  auto argumentCount = generics->getGenericContextHeader().NumKeyArguments;

  auto *map = state.data->getPointerKeyedMetadataMap();
  auto result = _dyld_find_pointer_hash_table_entry(
      map, description, argumentCount, const_cast<const void **>(arguments));
  LOG("Looking up description %p in dyld table, found %p.", description,
      result);
  return reinterpret_cast<Metadata *>(const_cast<void *>(result));
#else
  LOG("Looking up description %p but dyld hash table call not available.",
      description);
  return nullptr;
#endif
}

// When we have a pointer-keyed map from a debug library, it's not built as a
// hash table. We just scan it linearly.
static Metadata *getMetadataFromPointerKeyedMapDebugMode(
    const LibPrespecializedState &state,
    const TypeContextDescriptor *description, const void *const *arguments) {
  auto *generics = description->getGenericContext();
  if (!generics)
    return nullptr;

  auto argumentCount = generics->getGenericContextHeader().NumKeyArguments;
  auto *mapPtr = state.data->getPointerKeyedMetadataMap();

  struct MapKey {
    size_t count;
    void *pointers[];
  };

  struct MapEntry {
    const MapKey *key;
    Metadata *value;
  };

  struct Map {
    size_t count;
    MapEntry entries[];
  };

  const Map *map = reinterpret_cast<const Map *>(mapPtr);
  for (size_t i = 0; i < map->count; i++) {
    auto &entry = map->entries[i];

    // Keys are descriptor followed by arguments, so their count is 1 plus the
    // argument count.
    if (entry.key->count != argumentCount + 1)
      continue;

    // Check the descriptor.
    if (description != entry.key->pointers[0])
      continue;

    // Check the rest. The pointers array is now offset by 1 since index 0 is
    // the descriptor.
    bool equal = true;
    for (size_t j = 0; j < argumentCount; j++) {
      if (entry.key->pointers[j + 1] != arguments[j]) {
        equal = false;
        break;
      }
    }

    if (equal) {
      LOG("Looking up description %p in debug table, found %p.", description,
          entry.value);
      return entry.value;
    }
  }

  LOG("Looking up description %p in debug table, no entry found.", description);
  return nullptr;
}

Metadata *
swift::getLibPrespecializedMetadata(const TypeContextDescriptor *description,
                                    const void *const *arguments) {
  if (SWIFT_UNLIKELY(disableForValidation || disablePrespecializedMetadata.load(
                                                 std::memory_order_acquire)))
    return nullptr;

  auto &state = LibPrespecialized.get();

  switch (state.mapConfiguration) {
  case LibPrespecializedState::MapConfiguration::Disabled:
    return nullptr;
  case LibPrespecializedState::MapConfiguration::UseNameKeyedMap:
    return getMetadataFromNameKeyedMap(state, description, arguments);
  case LibPrespecializedState::MapConfiguration::UsePointerKeyedMap:
    return getMetadataFromPointerKeyedMap(state, description, arguments);
  case LibPrespecializedState::MapConfiguration::UsePointerKeyedMapDebugMode:
    return getMetadataFromPointerKeyedMapDebugMode(state, description,
                                                   arguments);
  }
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
