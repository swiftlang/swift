//===--- BinaryScan.h - C API for Swift Binary Scanning ---*- C -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This C API is primarily intended to serve as a "static mirror" library
// for querying Swift type information from binary object files.
//
//===----------------------------------------------------------------------===//

#include "StaticMirrorMacros.h"
#include "swift-c/CommonString/CommonString.h"

#ifndef SWIFT_C_BINARY_SCAN_H
#define SWIFT_C_BINARY_SCAN_H

/// The version constants for the SwiftStaticMirror C API.
/// SWIFTSTATICMIRROR_VERSION_MINOR should increase when there are API additions.
/// SWIFTSTATICMIRROR_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define SWIFTSTATICMIRROR_VERSION_MAJOR 0
#define SWIFTSTATICMIRROR_VERSION_MINOR 1

SWIFTSTATICMIRROR_BEGIN_DECLS

//=== Public Binary Scanner Data Types ------------------------------------===//

typedef swiftscan_string_ref_t swift_static_mirror_string_ref_t;
typedef swiftscan_string_set_t swift_static_mirror_string_set_t;

/// Container of the configuration state for binary static mirror scanning
/// instance
typedef void *swift_static_mirror_t;

/// Opaque container to a conformance type info of a given protocol conformance.
typedef struct swift_static_mirror_conformance_info_s
    *swift_static_mirror_conformance_info_t;

typedef struct {
  swift_static_mirror_conformance_info_t *conformances;
  size_t count;
} swift_static_mirror_conformances_set_t;

SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_type_name(
        swift_static_mirror_conformance_info_t);

SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_protocol_name(
        swift_static_mirror_conformance_info_t);

SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_mangled_type_name(
        swift_static_mirror_conformance_info_t);

SWIFTSTATICMIRROR_PUBLIC void
    swift_static_mirror_conformance_info_dispose(
        swift_static_mirror_conformance_info_t);

/// Create an \c swift_static_mirror_t instance.
/// The returned \c swift_static_mirror_t is owned by the caller and must be
/// disposed of using \c swift_static_mirror_dispose .
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_t
swift_static_mirror_create(int, const char **, const char *);

SWIFTSTATICMIRROR_PUBLIC void
swift_static_mirror_dispose(swift_static_mirror_t);

/// Identify and collect all types conforming to any of the protocol names
/// specified as arguments
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_conformances_set_t *
swift_static_mirror_conformances_set_create(
    swift_static_mirror_t, int, const char **);

SWIFTSTATICMIRROR_PUBLIC void swift_static_mirror_conformances_set_dispose(
    swift_static_mirror_conformances_set_t *);

SWIFTSTATICMIRROR_END_DECLS

#endif // SWIFT_C_BINARY_SCAN_H
