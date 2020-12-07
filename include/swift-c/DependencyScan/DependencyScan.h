//===--- DependencyScan.h - C API for Swift Dependency Scanning ---*- C -*-===//
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
// This C API is primarily intended to serve as the Swift Driver's
// dependency scanning facility (https://github.com/apple/swift-driver).
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_C_DEPENDENCY_SCAN_H
#define SWIFT_C_DEPENDENCY_SCAN_H

#include "DSString.h"
#include "DependencyScanMacros.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// The version constants for the SwiftDependencyScan C API.
/// SWIFTSCAN_VERSION_MINOR should increase when there are API additions.
/// SWIFTSCAN_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define SWIFTSCAN_VERSION_MAJOR 0
#define SWIFTSCAN_VERSION_MINOR 1

SWIFTSCAN_BEGIN_DECLS

//=== Public Scanner Data Types -------------------------------------------===//

typedef enum {
  SWIFTSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL = 0,
  SWIFTSCAN_DEPENDENCY_INFO_SWIFT_BINARY = 1,
  SWIFTSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER = 2,
  SWIFTSCAN_DEPENDENCY_INFO_CLANG = 3
} swiftscan_dependency_info_kind_t;

/// Opaque container of the details specific to a given module dependency.
typedef void *swiftscan_module_details_t;

/// Opaque container to a dependency info of a given module.
typedef void *swiftscan_dependency_info_t;

/// Opaque container to an overall result of a dependency scan.
typedef void *swiftscan_dependency_result_t;

/// Opaque container to contain the result of a dependency prescan.
typedef void *swiftscan_prescan_result_t;

/// Full Dependency Graph (Result)
typedef struct {
  int count;
  swiftscan_dependency_info_t *modules;
} swiftscan_dependency_set_t;

//=== Batch Scan Input Specification --------------------------------------===//

/// Opaque container to a container of batch scan entry information.
typedef void *swiftscan_batch_scan_entry_t;

typedef struct {
  int count;
  swiftscan_batch_scan_entry_t *modules;
} swiftscan_batch_scan_input_t;

typedef struct {
  int count;
  swiftscan_dependency_result_t *results;
} swiftscan_batch_scan_result_t;

//=== Dependency Result Functions -----------------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_dependency_result_get_main_module_name(
    swiftscan_dependency_result_t result);

SWIFTSCAN_PUBLIC swiftscan_dependency_set_t *
swiftscan_dependency_result_get_module_set(
    swiftscan_dependency_result_t result);

//=== Dependency Module Info Functions ------------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_module_info_get_module_name(swiftscan_dependency_info_t info);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_module_info_get_module_path(swiftscan_dependency_info_t info);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_module_info_get_source_files(swiftscan_dependency_info_t info);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_module_info_get_direct_dependencies(swiftscan_dependency_info_t info);

SWIFTSCAN_PUBLIC swiftscan_module_details_t
swiftscan_module_info_get_details(swiftscan_dependency_info_t info);

//=== Dependency Module Info Details Functions ----------------------------===//

SWIFTSCAN_PUBLIC swiftscan_dependency_info_kind_t
swiftscan_module_detail_get_kind(swiftscan_module_details_t details);

//=== Swift Textual Module Details query APIs -----------------------------===//
SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_textual_detail_get_module_interface_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_compiled_module_candidates(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_textual_detail_get_bridging_header_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_source_files(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_module_dependencies(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_command_line(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_extra_pcm_args(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_textual_detail_get_context_hash(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC bool swiftscan_swift_textual_detail_get_is_framework(
    swiftscan_module_details_t details);

//=== Swift Binary Module Details query APIs ------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_binary_detail_get_compiled_module_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_binary_detail_get_module_doc_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_binary_detail_get_module_source_info_path(
    swiftscan_module_details_t *details);

//=== Swift Placeholder Module Details query APIs -------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_placeholder_detail_get_compiled_module_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_placeholder_detail_get_module_doc_path(
    swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_swift_placeholder_detail_get_module_source_info_path(
    swiftscan_module_details_t details);

//=== Clang Module Details query APIs -------------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_clang_detail_get_module_map_path(swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_clang_detail_get_context_hash(swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_clang_detail_get_command_line(swiftscan_module_details_t details);

//=== Batch Scan Entry Functions ------------------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_batch_scan_entry_get_module_name(swiftscan_batch_scan_entry_t entry);

SWIFTSCAN_PUBLIC swiftscan_string_t
swiftscan_batch_scan_entry_get_arguments(swiftscan_batch_scan_entry_t entry);

SWIFTSCAN_PUBLIC bool
swiftscan_batch_scan_entry_get_is_swift(swiftscan_batch_scan_entry_t entry);

//=== Prescan Result Functions --------------------------------------------===//

SWIFTSCAN_PUBLIC swiftscan_string_set_t *
swiftscan_prescan_result_get_import_set(swiftscan_prescan_result_t result);

//=== Cleanup Functions ---------------------------------------------------===//

SWIFTSCAN_PUBLIC void
swiftscan_dependency_info_details_dispose(swiftscan_module_details_t details);

SWIFTSCAN_PUBLIC void
swiftscan_dependency_info_dispose(swiftscan_dependency_info_t info);

SWIFTSCAN_PUBLIC void
swiftscan_dependency_set_dispose(swiftscan_dependency_set_t *set);

SWIFTSCAN_PUBLIC void
swiftscan_dependency_result_dispose(swiftscan_dependency_result_t result);

SWIFTSCAN_PUBLIC void
swiftscan_prescan_result_dispose(swiftscan_prescan_result_t result);

SWIFTSCAN_PUBLIC void
swiftscan_batch_scan_entry_dispose(swiftscan_batch_scan_entry_t entry);

SWIFTSCAN_PUBLIC void
swiftscan_batch_scan_input_dispose(swiftscan_batch_scan_input_t *input);

SWIFTSCAN_PUBLIC void
swiftscan_batch_scan_result_dispose(swiftscan_batch_scan_result_t *result);

//=== Scanner Functions ---------------------------------------------------===//

/// Container of the configuration state and shared cache for dependency
/// scanning.
typedef void *swiftscan_scanner_t;

SWIFTSCAN_PUBLIC swiftscan_scanner_t swiftscan_scanner_create(void);

SWIFTSCAN_PUBLIC void swiftscan_scanner_dispose(swiftscan_scanner_t);

SWIFTSCAN_PUBLIC swiftscan_dependency_result_t
swiftscan_scan_dependencies(swiftscan_scanner_t *scanner,
                            const char *working_directory, int argc,
                            const char *const *argv);

SWIFTSCAN_PUBLIC swiftscan_batch_scan_result_t *
swiftscan_batch_scan_dependencies(swiftscan_scanner_t *scanner,
                                  const char *working_directory,
                                  swiftscan_batch_scan_input_t *batch_input,
                                  int argc, const char *const *argv);

SWIFTSCAN_PUBLIC swiftscan_prescan_result_t
swiftscan_prescan_dependencies(swiftscan_scanner_t *scanner,
                               const char *working_directory, int argc,
                               const char *const *argv);

SWIFTSCAN_END_DECLS

#endif // SWIFT_C_DEPENDENCY_SCAN_H
