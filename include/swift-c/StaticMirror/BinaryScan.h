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
#define SWIFTSTATICMIRROR_VERSION_MINOR 3 // Added filed type info gather

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

/// Opaque container to a field info (property type or enum case)
typedef struct swift_static_mirror_field_info_s
    *swift_static_mirror_field_info_t;

/// Opaque container to a property type info
typedef struct swift_static_mirror_property_info_s
    *swift_static_mirror_property_info_t;

/// Opaque container to an enum case info
typedef struct swift_static_mirror_enum_case_info_s
    *swift_static_mirror_enum_case_info_t;

/// Opaque container to details of an associated type specification.
typedef struct swift_static_mirror_type_alias_s
    *swift_static_mirror_type_alias_t;

/// Opaque container to an associated type (typealias) info of a given type.
typedef struct swift_static_mirror_associated_type_info_s
    *swift_static_mirror_associated_type_info_t;

typedef struct {
  swift_static_mirror_type_alias_t *type_aliases;
  size_t count;
} swift_static_mirror_type_alias_set_t;

typedef struct {
  swift_static_mirror_associated_type_info_t *associated_type_infos;
  size_t count;
} swift_static_mirror_associated_type_info_set_t;

typedef struct {
  swift_static_mirror_conformance_info_t *conformances;
  size_t count;
} swift_static_mirror_conformances_set_t;

typedef struct {
  swift_static_mirror_property_info_t *properties;
  size_t count;
} swift_static_mirror_property_info_set_t;

typedef struct {
  swift_static_mirror_enum_case_info_t *enum_cases;
  size_t count;
} swift_static_mirror_enum_case_info_set_t;

typedef struct {
  swift_static_mirror_field_info_t *field_infos;
  size_t count;
} swift_static_mirror_field_info_set_t;

// swift_static_mirror_conformance_info query methods
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_type_name(
        swift_static_mirror_conformance_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_protocol_name(
        swift_static_mirror_conformance_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_conformance_info_get_mangled_type_name(
        swift_static_mirror_conformance_info_t);
SWIFTSTATICMIRROR_PUBLIC void swift_static_mirror_conformance_info_dispose(
    swift_static_mirror_conformance_info_t);

// swift_static_mirror_associated_type_info query methods
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_type_alias_get_type_alias_name(
        swift_static_mirror_type_alias_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_type_alias_get_substituted_type_name(
        swift_static_mirror_type_alias_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_type_alias_get_substituted_type_mangled_name(
        swift_static_mirror_type_alias_t);

// swift_static_mirror_associated_type_info query methods
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_associated_type_info_get_mangled_type_name(
        swift_static_mirror_associated_type_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_type_alias_set_t *
    swift_static_mirror_associated_type_info_get_type_alias_set(
        swift_static_mirror_associated_type_info_t);

// swift_static_mirror_field_info query methods
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_field_info_get_mangled_type_name(
        swift_static_mirror_field_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_property_info_set_t *
    swift_static_mirror_field_info_get_property_info_set(
        swift_static_mirror_field_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_enum_case_info_set_t *
    swift_static_mirror_field_info_get_enum_case_info_set(
        swift_static_mirror_field_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_property_info_get_label(
        swift_static_mirror_property_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_property_info_get_type_name(
        swift_static_mirror_property_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_property_info_get_mangled_type_name(
        swift_static_mirror_property_info_t);
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_string_ref_t
    swift_static_mirror_enum_case_info_get_label(
        swift_static_mirror_enum_case_info_t);

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
swift_static_mirror_conformances_set_create(swift_static_mirror_t, int,
                                            const char **);

SWIFTSTATICMIRROR_PUBLIC void swift_static_mirror_conformances_set_dispose(
    swift_static_mirror_conformances_set_t *);

/// Identify and collect all associated types of a given input type (by mangled
/// name)
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_associated_type_info_set_t *
swift_static_mirror_associated_type_info_set_create(swift_static_mirror_t,
                                                    const char *);

/// Identify and collect associated types of all discovered types.
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_associated_type_info_set_t *
    swift_static_mirror_all_associated_type_info_set_create(
        swift_static_mirror_t);

SWIFTSTATICMIRROR_PUBLIC void
swift_static_mirror_associated_type_info_set_dispose(
    swift_static_mirror_associated_type_info_set_t *);

/// Identify and collect all field types of a given input type (by mangled name)
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_field_info_set_t *
swift_static_mirror_field_info_set_create(swift_static_mirror_t, const char *);

/// Identify and collect field types of all discovered types.
SWIFTSTATICMIRROR_PUBLIC swift_static_mirror_field_info_set_t *
    swift_static_mirror_all_field_info_set_create(swift_static_mirror_t);

SWIFTSTATICMIRROR_PUBLIC void swift_static_mirror_field_info_set_dispose(
    swift_static_mirror_field_info_set_t *);

SWIFTSTATICMIRROR_END_DECLS

#endif // SWIFT_C_BINARY_SCAN_H
