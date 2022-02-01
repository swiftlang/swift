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

#include "DependencyScanMacros.h"
#include "CommonString.h"

#ifndef SWIFT_C_BINARY_SCAN_H
#define SWIFT_C_BINARY_SCAN_H

SWIFTSCAN_BEGIN_DECLS

//=== Public Binary Scanner Data Types ------------------------------------===//

/// Container of the configuration state for binary static mirror scanning
/// instance
typedef void *swiftscan_static_mirror_t;

/// Opaque container to a conformance type info of a given protocol conformance.
typedef struct swiftscan_conformance_info_s
    *swiftscan_static_mirror_conformance_info_t;

typedef struct {
  swiftscan_static_mirror_conformance_info_t *conformances;
  size_t count;
} swiftscan_static_mirror_conformances_set_t;

SWIFTSCAN_PUBLIC swiftscan_string_ref_t
    swiftscan_static_mirror_conformance_info_get_type_name(
        swiftscan_static_mirror_conformance_info_t);

SWIFTSCAN_PUBLIC swiftscan_string_ref_t
    swiftscan_static_mirror_conformance_info_get_protocol_name(
        swiftscan_static_mirror_conformance_info_t);

SWIFTSCAN_PUBLIC swiftscan_string_ref_t
    swiftscan_static_mirror_conformance_info_get_mangled_type_name(
        swiftscan_static_mirror_conformance_info_t);

SWIFTSCAN_PUBLIC void
    swiftscan_static_mirror_conformance_info_dispose(
        swiftscan_static_mirror_conformance_info_t);

/// Create an \c swiftscan_static_mirror_t instance.
/// The returned \c swiftscan_static_mirror_t is owned by the caller and must be
/// disposed of using \c swiftscan_static_mirror_dispose .
SWIFTSCAN_PUBLIC swiftscan_static_mirror_t
swiftscan_static_mirror_create(int, const char **, const char *);

SWIFTSCAN_PUBLIC void
swiftscan_static_mirror_dispose(swiftscan_static_mirror_t);

/// Identify and collect all types conforming to any of the protocol names
/// specified as arguments
SWIFTSCAN_PUBLIC swiftscan_static_mirror_conformances_set_t *
swiftscan_static_mirror_conformances_set_create(
    swiftscan_static_mirror_t, int, const char **);

SWIFTSCAN_PUBLIC void swiftscan_static_mirror_conformances_set_dispose(
    swiftscan_static_mirror_conformances_set_t *);

SWIFTSCAN_END_DECLS

#endif // SWIFT_C_BINARY_SCAN_H
