//===--- CompatibilityConcurrency.cpp -------------------------------------===//
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
#include "swift/Runtime/Metadata.h"
#include <dispatch/dispatch.h>
#include <dlfcn.h>

// Allow this library to get force-loaded by autolinking
__attribute__((weak, visibility("hidden"))) extern "C" char
    _swift_FORCE_LOAD_$_swiftCompatibilityConcurrency = 0;
using namespace swift;

namespace swift {

// Entrypoint called by the compiler when back-deploying concurrency, which
// switches between the real implementation of
// swift_getFunctionTypeMetadataGlobalActor and
// swift_getFunctionTypeMetadataGlobalActorStandalone depending on what system
// it is running on.
SWIFT_RUNTIME_STDLIB_INTERNAL
const FunctionTypeMetadata *
swift_getFunctionTypeMetadataGlobalActorBackDeploy(
    FunctionTypeFlags flags, FunctionMetadataDifferentiabilityKind diffKind,
    const Metadata *const *parameters, const uint32_t *parameterFlags,
    const Metadata *result, const Metadata *globalActor);

} // end namespace swift

const FunctionTypeMetadata *
swift::swift_getFunctionTypeMetadataGlobalActorBackDeploy(
    FunctionTypeFlags flags, FunctionMetadataDifferentiabilityKind diffKind,
    const Metadata *const *parameters, const uint32_t *parameterFlags,
    const Metadata *result, const Metadata *globalActor) {
  using BuilderFn = const FunctionTypeMetadata *(*)(
      FunctionTypeFlags, FunctionMetadataDifferentiabilityKind,
      const Metadata *const *, const uint32_t *,
      const Metadata *, const Metadata *);
  static BuilderFn builderFn;
  static dispatch_once_t builderToken;
  dispatch_once(&builderToken, ^{
      // Prefer the function from the Swift runtime if it is available.
      builderFn = reinterpret_cast<BuilderFn>(
        dlsym(RTLD_DEFAULT, "swift_getFunctionTypeMetadataGlobalActor"));
      if (builderFn)
        return;

      builderFn = reinterpret_cast<BuilderFn>(
        dlsym(RTLD_DEFAULT,
              "swift_getFunctionTypeMetadataGlobalActorStandalone"));
    });

  assert(builderFn && "No way to build global-actor-qualified function type");
  return builderFn(
      flags, diffKind, parameters, parameterFlags, result, globalActor);
}
