//===--- ExternalGenericMetadataBuilder.h - Public interface ----*- C++ -*-===//
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
///
/// \file
/// This header declares functions in the libswiftReflection library,
/// which provides mechanisms for reflecting heap information in a
/// remote Swift process.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_EXTERNAL_GENERIC_METADATA_BUILDER_H
#define SWIFT_EXTERNAL_GENERIC_METADATA_BUILDER_H

#include <stdint.h>

#include "swift/shims/Visibility.h"

#ifdef __cplusplus
extern "C" {
#endif

struct SwiftExternalMetadataBuilder;
struct mach_header;

// Create a builder object with the given platform and architecture name.
SWIFT_ATTRIBUTE_FOR_EXPORTS
struct SwiftExternalMetadataBuilder *
swift_externalMetadataBuilder_create(int platform, const char *arch);

// Destroy a builder object.
SWIFT_ATTRIBUTE_FOR_EXPORTS
void swift_externalMetadataBuilder_destroy(
    struct SwiftExternalMetadataBuilder *);

// Returns an error string if the dylib could not be added
// The builder owns the string, so the caller does not have to free it
// The mach_header* is the raw dylib from disk/memory, before the shared cache
// builder has created its own copy of it
SWIFT_ATTRIBUTE_FOR_EXPORTS
const char *swift_externalMetadataBuilder_addDylib(
    struct SwiftExternalMetadataBuilder *, const char *install_name,
    const struct mach_header *, uint64_t size);

SWIFT_ATTRIBUTE_FOR_EXPORTS
const char *swift_externalMetadataBuilder_readNamesJSON(
    struct SwiftExternalMetadataBuilder *, const char *names_json);

// Returns an error string if the dylib could not be added. The builder owns the
// string, so the caller does not have to free it. The string remains valid
// until at least the next call to the builder.
SWIFT_ATTRIBUTE_FOR_EXPORTS
const char *swift_externalMetadataBuilder_buildMetadata(
    struct SwiftExternalMetadataBuilder *);

// Get the JSON for the built metadata.
SWIFT_ATTRIBUTE_FOR_EXPORTS
const char *swift_externalMetadataBuilder_getMetadataJSON(
    struct SwiftExternalMetadataBuilder *);

// Convenience function that works with the filesystem and handles everything in
// one call.
SWIFT_ATTRIBUTE_FOR_EXPORTS
int swift_type_metadata_extract(const char *inputPath,       // mangled names
                                const char *dylibSearchPath, // images to add
                                const char *arch,
                                const char *outputPath // json output
);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // SWIFT_EXTERNAL_GENERIC_METADATA_BUILDER_H
