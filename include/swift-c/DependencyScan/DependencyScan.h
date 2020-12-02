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
/// DEPSCAN_VERSION_MINOR should increase when there are API additions.
/// DEPSCAN_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define DEPSCAN_VERSION_MAJOR 0
#define DEPSCAN_VERSION_MINOR 1

DEPSCAN_BEGIN_DECLS

//=== Public Scanner Data Types -------------------------------------------===//

typedef enum {
  DEPSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL = 0,
  DEPSCAN_DEPENDENCY_INFO_SWIFT_BINARY = 1,
  DEPSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER = 2,
  DEPSCAN_DEPENDENCY_INFO_CLANG = 3
} ds_dependency_info_kind_t;

/// Opaque container of the details specific to a given module dependency.
typedef void *ds_module_details_t;

typedef struct {
  /// The module's name
  /// The format is:
  /// `<module-kind>:<module-name>`
  /// where `module-kind` is one of:
  /// "swiftTextual"
  /// "swiftBinary"
  /// "swiftPlaceholder"
  /// "clang""
  ds_string_t module_name;

  /// The path for the module.
  ds_string_t module_path;

  /// The source files used to build this module.
  ds_string_set_t *source_files;

  /**
   * The list of modules which this module direct depends on.
   * The format is:
   * `<module-kind>:<module-name>`
   */
  ds_string_set_t *direct_dependencies;

  /// Specific details of a particular kind of module.
  ds_module_details_t details;
} ds_dependency_info_t;

/// Full Dependency Graph (Result)

typedef struct {
  int count;
  ds_dependency_info_t *modules;
} ds_dependency_set_t;

typedef struct {
  /// The name of the main module for this dependency graph (root node)
  ds_string_t main_module_name;

  /// The complete list of modules discovered
  ds_dependency_set_t *module_set;
} ds_dependency_result_t;

typedef struct {
  /// The complete list of imports discovered
  ds_string_set_t *import_set;
} ds_prescan_result_t;

//=== Dependency Result Functions -----------------------------------------===//

DEPSCAN_PUBLIC ds_dependency_info_kind_t
ds_get_module_detail_kind(ds_module_details_t details);

//=== Swift Textual Module Details query APIs -----------------------------===//
DEPSCAN_PUBLIC ds_string_t
ds_get_swift_textual_detail_module_interface_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_swift_textual_detail_compiled_module_candidates(
    ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_textual_detail_bridging_header_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_swift_textual_detail_bridging_source_files(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_swift_textual_detail_bridging_module_dependencies(
    ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_swift_textual_detail_command_line(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_swift_textual_detail_extra_pcm_args(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_textual_detail_context_hash(ds_module_details_t details);

DEPSCAN_PUBLIC bool
ds_get_swift_textual_detail_is_framework(ds_module_details_t details);

//=== Swift Binary Module Details query APIs ------------------------------===//

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_binary_detail_compiled_module_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_binary_detail_module_doc_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t ds_get_swift_binary_detail_module_source_info_path(
    ds_module_details_t *details);

//=== Swift Placeholder Module Details query APIs -------------------------===//

DEPSCAN_PUBLIC ds_string_t ds_get_swift_placeholder_detail_compiled_module_path(
    ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_placeholder_detail_module_doc_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_swift_placeholder_detail_module_source_info_path(
    ds_module_details_t details);

//=== Clang Module Details query APIs -------------------------------------===//

DEPSCAN_PUBLIC ds_string_t
ds_get_clang_detail_module_map_path(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_t
ds_get_clang_detail_context_hash(ds_module_details_t details);

DEPSCAN_PUBLIC ds_string_set_t *
ds_get_clang_detail_command_line(ds_module_details_t details);

//=== Cleanup Functions ---------------------------------------------------===//

DEPSCAN_PUBLIC void
ds_dependency_info_details_dispose(ds_module_details_t details);

DEPSCAN_PUBLIC void ds_dependency_info_dispose(ds_dependency_info_t *info);

DEPSCAN_PUBLIC void ds_dependency_set_dispose(ds_dependency_set_t *set);

DEPSCAN_PUBLIC void
ds_dependency_result_dispose(ds_dependency_result_t *result);

DEPSCAN_PUBLIC void ds_prescan_result_dispose(ds_prescan_result_t *result);

//=== Scanner Functions ---------------------------------------------------===//

/// Container of the configuration state and shared cache for dependency
/// scanning.
typedef void *ds_scanner_t;

DEPSCAN_PUBLIC ds_scanner_t ds_scanner_create(void);

DEPSCAN_PUBLIC void ds_scanner_dispose(ds_scanner_t);

DEPSCAN_PUBLIC ds_dependency_result_t *
ds_scan_dependencies(ds_scanner_t *scanner, const char *working_directory,
                     int argc, const char *const *argv);

DEPSCAN_PUBLIC ds_prescan_result_t *
ds_prescan_dependencies(ds_scanner_t *scanner, const char *working_directory,
                        int argc, const char *const *argv);

DEPSCAN_END_DECLS

#endif // SWIFT_C_DEPENDENCY_SCAN_H
