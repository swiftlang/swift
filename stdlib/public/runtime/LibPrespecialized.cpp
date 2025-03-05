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

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <ShlWapi.h>
#include <Windows.h>
#endif

using namespace swift;

static bool prespecializedLoggingEnabled = false;

#define LOG(fmt, ...)                                                          \
  do {                                                                         \
    if (SWIFT_UNLIKELY(prespecializedLoggingEnabled))                          \
      fprintf(stderr, "Prespecializations library: " fmt "\n", __VA_ARGS__);   \
  } while (0)

#define LOG0(string) LOG("%s", string)

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
#if defined(_WIN32)
  DWORD dwSize = MAX_PATH;
  DWORD dwResult;
  std::unique_ptr<WCHAR[]> pwszBuffer(new WCHAR[dwSize]);
  while (true) {
    dwResult = GetModuleFileNameW(nullptr, pwszBuffer.get(), dwSize);
    if (dwResult == 0)
      return true;
    if (dwResult < dwSize)
      break;
    if (dwResult == dwSize && GetLastError() == ERROR_INSUFFICIENT_BUFFER)
      pwszBuffer.reset(new WCHAR[dwSize <<= 1]);
  }

  PCWSTR pwszBaseName = PathFindFileNameW(pwszBuffer.get());

  DWORD cchLength =
      WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS | WC_NO_BEST_FIT_CHARS,
                          pwszBaseName, -1, nullptr, 0, nullptr, nullptr);
  if (cchLength == 0)
    return true;

  std::unique_ptr<char[]> pszBaseName{new char[cchLength]};
  cchLength = WideCharToMultiByte(
      CP_UTF8, WC_ERR_INVALID_CHARS | WC_NO_BEST_FIT_CHARS, pwszBaseName, -1,
      pszBaseName.get(), cchLength, nullptr, nullptr);
  if (cchLength == 0)
    return true;

  const char *__progname = pszBaseName.get();
#else
  extern const char *__progname;

  if (!__progname)
    return true;
#endif

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

struct LibPrespecializedState {
  struct AddressRange {
    uintptr_t start, end;

    bool contains(const void *ptr) const {
      return start <= (uintptr_t)ptr && (uintptr_t)ptr < end;
    }
  };

  enum class MapConfiguration {
    Unset,
    UseNameKeyedMap,
    UsePointerKeyedMap,
    UsePointerKeyedMapDebugMode,
    Disabled,
  };

  const LibPrespecializedData<InProcess> *data;
  std::atomic<MapConfiguration> mapConfiguration = MapConfiguration::Unset;
  AddressRange sharedCacheRange{0, 0};
  AddressRange metadataAllocatorInitialPoolRange{0, 0};
  bool descriptorMapEnabled;

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

    // Compute our map configuration if it hasn't already been set. We must do
    // this after the shared cache range has been retrieved, because the map
    // configuration can be different depending on whether the map is in the
    // shared cache.
    if (mapConfiguration.load(std::memory_order_relaxed) ==
        MapConfiguration::Unset)
      mapConfiguration.store(computeMapConfiguration(data),
                             std::memory_order_relaxed);

    if (data) {
      descriptorMapEnabled =
          data->getOptionFlags() &
          LibPrespecializedData<InProcess>::OptionFlagDescriptorMapDefaultOn;
      LOG("Setting descriptorMapEnabled=%s from the option flags.",
          descriptorMapEnabled ? "true" : "false");
    }

    if (runtime::environment::
            SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_DESCRIPTOR_LOOKUP_isSet()) {
      descriptorMapEnabled = runtime::environment::
          SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_DESCRIPTOR_LOOKUP();
      LOG("Setting descriptorMapEnabled=%s from "
          "SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_DESCRIPTOR_LOOKUP.",
          descriptorMapEnabled ? "true" : "false");
    } else {
#if HAS_OS_FEATURE
      if (os_feature_enabled_simple(Swift, togglePrespecializationDescriptorMap,
                                    false)) {
        descriptorMapEnabled = !descriptorMapEnabled;
        LOG("Toggling descriptorMapEnabled to %s "
            "togglePrespecializationDescriptorMap is set.",
            descriptorMapEnabled ? "true" : "false");
      }
#endif
    }
  }

  MapConfiguration
  computeMapConfiguration(const LibPrespecializedData<InProcess> *data) {
    // If no data, we have to disable.
    if (!data)
      return MapConfiguration::Disabled;

    if (!runtime::environment::
            SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_METADATA()) {
      LOG0("Disabling metadata, SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_METADATA "
           "is false.");
      return MapConfiguration::Disabled;
    }

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

  const LibPrespecializedData<InProcess> *findLibPrespecialized() {
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
      dataPtr = SWIFT_RUNTIME_WEAK_USE(_dyld_get_swift_prespecialized_data());
      LOG("Got dataPtr %p from _dyld_get_swift_prespecialized_data", dataPtr);

      // Disable the prespecialized metadata if anything in the shared cache is
      // overridden. Eventually we want to be cleverer and only disable the
      // prespecializations that have been invalidated, but we'll start with the
      // simplest approach.
      if (dyld_shared_cache_some_image_overridden()) {
        mapConfiguration.store(MapConfiguration::Disabled,
                               std::memory_order_release);
        LOG("Disabling prespecialized metadata, "
            "dyld_shared_cache_some_image_overridden = %d",
            dyld_shared_cache_some_image_overridden());
      }
    }
#endif
#endif

    LOG("Returning data pointer %p", dataPtr);

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
    LOG("  optionFlags=%#zx", data->getOptionFlags());
    LOG("  metadataMap=%p", data->getMetadataMap());
    LOG("  disabledProcessTable=%p", data->getDisabledProcessesTable());
    LOG("  pointerKeyedMetadataMap=%p", data->getPointerKeyedMetadataMap());
    LOG("  descriptorMap=%p", data->getDescriptorMap());

    return data;
  }
};

static Lazy<LibPrespecializedState> LibPrespecialized;

// Returns true if the type has any arguments that aren't plain types (packs,
// values, or unknown kinds).
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

static bool isDescriptorLoaded(const void *descriptor, uint16_t imageIndex) {
#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  return _dyld_is_preoptimized_objc_image_loaded(imageIndex);
#else
  // If we're not using the dyld SPI, then we're working with a test dylib, and
  // a test dylib can't have pointers to unloaded dylibs.
  return true;
#endif
}

void
swift::libPrespecializedImageLoaded() {
#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  // A newly loaded image might have caused us to load images that are
  // overriding images in the shared cache.  If we do that, turn off
  // prespecialized metadata.
  if (dyld_shared_cache_some_image_overridden())
    LibPrespecialized.get().mapConfiguration.store(
        LibPrespecializedState::MapConfiguration::Disabled,
        std::memory_order_release);
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
  auto mangling = Demangle::mangleNode(mangleNode, resolver, dem, Mangle::ManglingFlavor::Default);
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
  if (SWIFT_RUNTIME_WEAK_CHECK(_dyld_find_pointer_hash_table_entry)) {
    auto *generics = description->getGenericContext();
    if (!generics)
      return nullptr;

    auto argumentCount = generics->getGenericContextHeader().NumKeyArguments;

    auto *map = state.data->getPointerKeyedMetadataMap();
    auto result = SWIFT_RUNTIME_WEAK_USE(_dyld_find_pointer_hash_table_entry(
        map, description, argumentCount, const_cast<const void **>(arguments)));
    LOG("Looking up description %p in dyld table, found %p.", description,
        result);
    return reinterpret_cast<Metadata *>(const_cast<void *>(result));
  }
#else
  LOG("Looking up description %p but dyld hash table call not available.",
      description);
#endif
  return nullptr;
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
  auto &state = LibPrespecialized.get();

  switch (state.mapConfiguration) {
  case LibPrespecializedState::MapConfiguration::Unset:
    assert(false &&
           "Map configuration should never be unset after initialization.");
    return nullptr;
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

std::pair<LibPrespecializedLookupResult, const TypeContextDescriptor *>
swift::getLibPrespecializedTypeDescriptor(Demangle::NodePointer node) {
  auto &state = LibPrespecialized.get();

  // Retrieve the map and return immediately if we don't have it.
  auto *data = state.data;
  if (!data)
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};

  if (!state.descriptorMapEnabled)
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};

  auto *descriptorMap = data->getDescriptorMap();
  if (!descriptorMap)
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};

  // Demangler and resolver for subsequent mangling operations.
  StackAllocatedDemangler<4096> dem;
  ExpandResolvedSymbolicReferences resolver{dem};

  if (SWIFT_UNLIKELY(prespecializedLoggingEnabled)) {
    auto mangling = Demangle::mangleNode(node, resolver, dem, Mangle::ManglingFlavor::Default);
    if (!mangling.isSuccess()) {
      LOG("Failed to build demangling for node %p.", node);
      return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};
    }

    auto mangled = mangling.result();
    LOG("Looking up descriptor named '%.*s'.", (int)mangled.size(),
        mangled.data());
  }

  // Get the simplified mangling that we use as the map's key.
  auto simplifiedNode = buildSimplifiedDescriptorDemangling(node, dem);
  if (!simplifiedNode) {
    LOG("Failed to build simplified mangling for node %p.", node);
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};
  }

  auto simplifiedMangling = Demangle::mangleNode(simplifiedNode, resolver, dem, Mangle::ManglingFlavor::Default);
  if (!simplifiedMangling.isSuccess()) {
    LOG("Failed to build demangling for simplified node %p.\n", node);
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};
  }

  // The map key is the simplified mangled name.
  auto key = simplifiedMangling.result();

  // Track how many descriptors we checked and how many were actually loaded,
  // for logging.
  unsigned numDescriptorsChecked = 0;
  unsigned numDescriptorsLoaded = 0;

  // A descriptor is a match if it's actually loaded, and if it matches the node
  // we're looking up.
  auto isMatch = [&](auto pointers) {
    auto *descriptor = *pointers.first;
    uint16_t libraryIndex = *pointers.second;

    numDescriptorsChecked++;

    if (!isDescriptorLoaded(descriptor, libraryIndex))
      return false;

    numDescriptorsLoaded++;

    return _contextDescriptorMatchesMangling(
        (const TypeContextDescriptor *)descriptor, node);
  };

  // Perform the lookup.
  auto isNull = [](auto pointers) { return *pointers.first == nullptr; };
  auto found = descriptorMap->find(key.data(), key.size(), isMatch, isNull);

  LOG("Hash table lookup checked %u loaded entries, %u total entries.",
      numDescriptorsLoaded, numDescriptorsChecked);

  // The pointers in `found` are pointers to the map entries, and should always
  // be non-NULL. The only condition that returns NULL is if the map has no
  // entries where `isMatch` or `isNull` return true, and the map should always
  // have at least one NULL entry.
  assert(found.first);
  if (!found.first) {
    LOG("Descriptor table lookup of '%.*s' returned NULL pointer to descriptor "
        "pointer.",
        (int)key.size(), key.data());
    return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};
  }

  auto *foundDescriptor = *found.first;

  if (!foundDescriptor) {
    LOG("Did not find descriptor for key '%.*s'.", (int)key.size(), key.data());

    // This result is definitive if the descriptor map is comprehensive. If the
    // map is not comprehensive, return NonDefinitiveNotFound to tell the caller
    // that it needs to perform a full search.
    if (data->getOptionFlags() &
        LibPrespecializedData<
            InProcess>::OptionFlagDescriptorMapNotComprehensive)
      return {LibPrespecializedLookupResult::NonDefinitiveNotFound, nullptr};
    return {LibPrespecializedLookupResult::DefinitiveNotFound, nullptr};
  }

  LOG("Found descriptor %p for key '%.*s'.", foundDescriptor, (int)key.size(),
      key.data());
  return {LibPrespecializedLookupResult::Found,
          (const TypeContextDescriptor *)foundDescriptor};
}

void _swift_validatePrespecializedMetadata() {
  auto *data = LibPrespecialized.get().data;
  if (!data) {
    return;
  }

  LibPrespecialized.get().mapConfiguration.store(
      LibPrespecializedState::MapConfiguration::Disabled,
      std::memory_order_release);

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
    if (result.getError()) {
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
