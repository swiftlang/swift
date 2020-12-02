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

#include "DependencyScanMacros.h"
#include "DSString.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/// The version constants for the SwiftDependencyScan C API.
/// DEPSCAN_VERSION_MINOR should increase when there are API additions.
/// DEPSCAN_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
#define DEPSCAN_VERSION_MAJOR 0
#define DEPSCAN_VERSION_MINOR 1

DEPSCAN_BEGIN_DECLS

//=== Scanner Data Types --------------------------------------------------===//

typedef enum {
  DEPSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL = 0,
  DEPSCAN_DEPENDENCY_INFO_SWIFT_BINARY = 1,
  DEPSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER = 2,
  DEPSCAN_DEPENDENCY_INFO_CLANG = 3
} depscan_dependency_info_kind_t;

/// Swift modules to be built from a module interface, may have a bridging
/// header.
typedef struct {
  /// The module interface from which this module was built, if any.
  ds_string_t module_interface_path;

  /// The paths of potentially ready-to-use compiled modules for the interface.
  ds_string_set_t* compiled_module_candidates;

  /// The bridging header, if any.
  ds_string_t bridging_header_path;

  /// The source files referenced by the bridging header.
  ds_string_set_t* bridging_source_files;

  /// (Clang) modules on which the bridging header depends.
  ds_string_set_t* bridging_module_dependencies;

  /// Options to the compile command required to build this module interface
  ds_string_set_t* command_line;

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  ds_string_set_t* extra_pcm_args;

  /// The hash value that will be used for the generated module
  ds_string_t context_hash;

  /// A flag to indicate whether or not this module is a framework.
  bool is_framework;
} depscan_swift_textual_details_t;

/// Swift modules with only a binary module file.
typedef struct {
  /// The path to the pre-compiled binary module
  ds_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  ds_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  ds_string_t module_source_info_path;
} depscan_swift_binary_details_t;

/// Swift placeholder modules carry additional details that specify their
/// module doc path and source info paths.
typedef struct {
  /// The path to the pre-compiled binary module
  ds_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  ds_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  ds_string_t module_source_info_path;
} depscan_swift_placeholder_details_t;

/// Clang modules are built from a module map file.
typedef struct {
  /// The path to the module map used to build this module.
  ds_string_t module_map_path;

  /// clang-generated context hash
  ds_string_t context_hash;

  /// Options to the compile command required to build this clang modulemap
  ds_string_set_t* command_line;
} depscan_clang_details_t;

typedef struct {
  depscan_dependency_info_kind_t kind;
  union {
    depscan_swift_textual_details_t swift_textual_details;
    depscan_swift_binary_details_t swift_binary_details;
    depscan_swift_placeholder_details_t swift_placeholder_details;
    depscan_clang_details_t clang_details;
  };
} depscan_module_details_t;

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
  ds_string_set_t* source_files;

  /**
   * The list of modules which this module direct depends on.
   * The format is:
   * `<module-kind>:<module-name>`
   */
  ds_string_set_t* direct_dependencies;

  /// Specific details of a particular kind of module.
  depscan_module_details_t* details;
} depscan_dependency_info_t;

/// Full Dependency Graph (Result)

typedef struct {
  int count;
  depscan_dependency_info_t *modules;
} depscan_dependency_set_t;

typedef struct {
  /// The name of the main module for this dependency graph (root node)
  ds_string_t main_module_name;

  /// The complete list of modules discovered
  depscan_dependency_set_t* module_set;
} depscan_dependency_result_t;

//=== Dependency Result Functions -----------------------------------------===//

DEPSCAN_PUBLIC void
depscan_dependency_info_details_dispose(depscan_module_details_t *details);

DEPSCAN_PUBLIC void
depscan_dependency_info_dispose(depscan_dependency_info_t *info);

DEPSCAN_PUBLIC void
depscan_dependency_set_dispose(depscan_dependency_set_t *set);

DEPSCAN_PUBLIC void
depscan_dependency_result_dispose(depscan_dependency_result_t *result);

//=== Scanner Functions ---------------------------------------------------===//

/// Container of the configuration state and shared cache for dependency scanning.
typedef void *depscan_scanner_t;

DEPSCAN_PUBLIC depscan_scanner_t
depscan_scanner_create(void);

DEPSCAN_PUBLIC void
depscan_scanner_dispose(depscan_scanner_t);

DEPSCAN_PUBLIC depscan_dependency_result_t*
depscan_scan_dependencies(depscan_scanner_t* scanner,
                          const char *working_directory,
                          int argc,
                          const char *const *argv);

DEPSCAN_END_DECLS

#endif // SWIFT_C_DEPENDENCY_SCAN_H
