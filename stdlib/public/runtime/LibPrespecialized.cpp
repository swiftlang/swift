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
#include "Private.h"
#include "swift/Basic/Lazy.h"
#include "swift/Runtime/EnvironmentVariables.h"
#include "swift/Runtime/Metadata.h"

#if SWIFT_STDLIB_HAS_DLADDR && __has_include(<dlfcn.h>)
#include <dlfcn.h>
#define USE_DLOPEN 1
#endif

#if __has_include(<mach-o/dyld_priv.h>)
#include <mach-o/dyld_priv.h>
#endif

using namespace swift;

static const LibPrespecializedData<InProcess> *findLibPrespecialized() {
  if (!runtime::environment::SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED())
    return nullptr;

  const void *dataPtr = nullptr;
#if USE_DLOPEN
  auto path = runtime::environment::SWIFT_DEBUG_LIB_PRESPECIALIZED_PATH();
  if (path && path[0]) {
    void *handle = dlopen(path, RTLD_LAZY);
    if (!handle)
      return nullptr;

    dataPtr = dlsym(handle, LIB_PRESPECIALIZED_TOP_LEVEL_SYMBOL_NAME);
  }
#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  else if (SWIFT_RUNTIME_WEAK_CHECK(_dyld_get_swift_prespecialized_data)) {
    // Disable the prespecializations library if anything in the shared cache is
    // overridden. Eventually we want to be cleverer and only disable the
    // prespecializations that have been invalidated, but we'll start with the
    // simplest approach.
    if (!dyld_shared_cache_some_image_overridden())
      dataPtr = SWIFT_RUNTIME_WEAK_USE(_dyld_get_swift_prespecialized_data());
  }
#endif
#endif

  if (!dataPtr)
    return nullptr;

  auto *data =
      reinterpret_cast<const LibPrespecializedData<InProcess> *>(dataPtr);
  if (data->majorVersion !=
      LibPrespecializedData<InProcess>::currentMajorVersion)
    return nullptr;

  return data;
}

const LibPrespecializedData<InProcess> *swift::getLibPrespecializedData() {
  return SWIFT_LAZY_CONSTANT(findLibPrespecialized());
}

Metadata *
swift::getLibPrespecializedMetadata(const TypeContextDescriptor *description,
                                    const void *const *arguments) {
  auto *data = getLibPrespecializedData();
  if (!data)
    return nullptr;

  Demangler dem;
  auto mangleNode = _buildDemanglingForGenericType(description, arguments, dem);
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
  if (SWIFT_UNLIKELY(runtime::environment::
                         SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED_LOGGING()))
    fprintf(stderr, "Prespecializations library: found %p for key '%.*s'.\n",
            result, (int)key.size(), key.data());
  return result;
}
