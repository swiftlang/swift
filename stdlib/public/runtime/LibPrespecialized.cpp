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

static LibPrespecializedData<InProcess> *findLibPrespecialized() {
  if (!runtime::environment::SWIFT_DEBUG_ENABLE_LIB_PRESPECIALIZED())
    return nullptr;

  void *dataPtr = nullptr;
#if USE_DLOPEN
  auto path = runtime::environment::SWIFT_DEBUG_LIB_PRESPECIALIZED_PATH();
  if (path && path[0]) {
    void *handle = dlopen(path, RTLD_LAZY);
    if (!handle)
      return nullptr;

    dataPtr = dlsym(handle, LIB_PRESPECIALIZED_TOP_LEVEL_SYMBOL_NAME);
  }
#if DYLD_GET_SWIFT_PRESPECIALIZED_DATA_DEFINED
  else {
    // Disable the prespecializations library if anything in the shared cache is
    // overridden. Eventually we want to be cleverer and only disable the
    // prespecializations that have been invalidated, but we'll start with the
    // simplest approach.
    if (!dyld_shared_cache_some_image_overridden())
      dataPtr = _dyld_get_swift_prespecialized_data();
  }
#endif
#endif

  if (!dataPtr)
    return nullptr;

  auto *data = reinterpret_cast<LibPrespecializedData<InProcess> *>(dataPtr);
  if (data->majorVersion !=
      LibPrespecializedData<InProcess>::currentMajorVersion)
    return nullptr;

  return data;
}

LibPrespecializedData<InProcess> *swift::getLibPrespecializedData() {
  return SWIFT_LAZY_CONSTANT(findLibPrespecialized());
}

Metadata *
swift::getLibPrespecializedMetadata(const TypeContextDescriptor *description,
                                    const void *const *arguments) {
  auto *data = getLibPrespecializedData();
  if (!data)
    return nullptr;

  auto *metadataMap = data->getMetadataMap();

  // TODO: This linear search is a stopgap until we work out the right
  // representation for the string keys.
  size_t count = metadataMap->arraySize;
  auto *entries = metadataMap->array();
  for (size_t i = 0; i < count; i++) {
    if (entries[i].key == nullptr)
      continue;

    Metadata *candidate = entries[i].value;
    auto *candidateDescription = candidate->getDescription();
    if (description != candidateDescription)
      continue;

    auto *candidateArguments = candidate->getGenericArgs();

    bool match = true;

    const auto &genericContext = *description->getGenericContext();
    const auto &header = genericContext.getGenericContextHeader();
    for (unsigned param = 0; param < header.NumParams; param++)
      if (arguments[param] != candidateArguments[param]) {
        match = false;
        break;
      }

    if (match)
      return candidate;
  }

  return nullptr;
}
