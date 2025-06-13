//===-------------- DependencyScanImpl.h - Swift Compiler -----------------===//
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
// Implementation details of the dependency scanning C API
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_C_DEPENDENCY_SCAN_IMPL_H
#define SWIFT_C_DEPENDENCY_SCAN_IMPL_H

#include "swift-c/DependencyScan/DependencyScan.h"

namespace swift {
namespace dependencies {
class DependencyScanningTool;
} // namespace dependencies
} // namespace swift

struct swiftscan_dependency_graph_s {
  /// The name of the main module for this dependency graph (root node)
  swiftscan_string_ref_t main_module_name;

  /// The complete list of modules discovered
  swiftscan_dependency_set_t *dependencies;

  /// Diagnostics produced during this scan
  swiftscan_diagnostic_set_t *diagnostics;
};

struct swiftscan_dependency_info_s {
  /// The module's name
  /// The format is:
  /// `<module-kind>:<module-name>`
  /// where `module-kind` is one of:
  /// "swiftInterface"
  /// "swiftSource"
  /// "swiftBinary"
  /// "swiftPlaceholder"
  /// "clang""
  swiftscan_string_ref_t module_name;

  /// The path for the module.
  swiftscan_string_ref_t module_path;

  /// The source files used to build this module.
  swiftscan_string_set_t *source_files;

  /**
   * The list of modules which this module direct depends on.
   * The format is:
   * `<module-kind>:<module-name>`
   */
  swiftscan_string_set_t *direct_dependencies;

  /// The list of link libraries for this module.
  swiftscan_link_library_set_t *link_libraries;

  /// The list of source import infos.
  swiftscan_import_info_set_t *imports;

  /// Specific details of a particular kind of module.
  swiftscan_module_details_t details;
};

struct swiftscan_link_library_info_s {
  swiftscan_string_ref_t name;
  bool isStatic;
  bool isFramework;
  bool forceLoad;
};

struct swiftscan_import_info_s {
  swiftscan_string_ref_t import_identifier;
  swiftscan_source_location_set_t *source_locations;
  swiftscan_access_level_t access_level;
};

struct swiftscan_macro_dependency_s {
  swiftscan_string_ref_t module_name;
  swiftscan_string_ref_t library_path;
  swiftscan_string_ref_t executable_path;
};

/// Swift modules to be built from a module interface, may have a bridging
/// header.
typedef struct {
  /// The module interface from which this module was built, if any.
  swiftscan_string_ref_t module_interface_path;

  /// The paths of potentially ready-to-use compiled modules for the interface.
  swiftscan_string_set_t *compiled_module_candidates;

  /// The bridging header, if any.
  swiftscan_string_ref_t bridging_header_path;

  /// The source files referenced by the bridging header.
  swiftscan_string_set_t *bridging_source_files;

  /// (Clang) modules on which the bridging header depends.
  swiftscan_string_set_t *bridging_module_dependencies;

  /// (Swift) module dependencies by means of being overlays of
  /// Clang module dependencies
  swiftscan_string_set_t *swift_overlay_module_dependencies;

  /// Directly-imported in source module dependencies
  swiftscan_string_set_t *source_import_module_dependencies;

  /// Options to the compile command required to build this module interface
  swiftscan_string_set_t *command_line;

  /// Options to the compile command required to build bridging header.
  swiftscan_string_set_t *bridging_pch_command_line;

  /// The hash value that will be used for the generated module
  swiftscan_string_ref_t context_hash;

  /// A flag to indicate whether or not this module is a framework.
  bool is_framework;

  /// A flag that indicates this dependency is associated with a static archive
  bool is_static;

  /// The CASID for CASFileSystemRoot
  swiftscan_string_ref_t cas_fs_root_id;

  /// The CASID for bridging header include tree
  swiftscan_string_ref_t bridging_header_include_tree;

  /// ModuleCacheKey
  swiftscan_string_ref_t module_cache_key;

  /// Macro dependecies.
  swiftscan_macro_dependency_set_t *macro_dependencies;

  /// User module version
  swiftscan_string_ref_t user_module_version;

  /// Chained bridging header path.
  swiftscan_string_ref_t chained_bridging_header_path;

  /// Chained bridging header content.
  swiftscan_string_ref_t chained_bridging_header_content;

} swiftscan_swift_textual_details_t;

/// Swift modules with only a binary module file.
typedef struct {
  /// The path to the pre-compiled binary module
  swiftscan_string_ref_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  swiftscan_string_ref_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  swiftscan_string_ref_t module_source_info_path;

  /// (Swift) module dependencies by means of being overlays of
  /// Clang module dependencies
  swiftscan_string_set_t *swift_overlay_module_dependencies;

  /// (Clang) header (.h) dependency of this binary module.
  swiftscan_string_ref_t header_dependency;

  /// (Clang) module dependencies of the textual header inputs
  swiftscan_string_set_t *header_dependencies_module_dependnecies;

  /// Source files included by the header dependencies of this binary module
  swiftscan_string_set_t *header_dependencies_source_files;

  /// A flag to indicate whether or not this module is a framework.
  bool is_framework;

  /// A flag that indicates this dependency is associated with a static archive
  bool is_static;

  /// Macro dependecies.
  swiftscan_macro_dependency_set_t *macro_dependencies;

  /// ModuleCacheKey
  swiftscan_string_ref_t module_cache_key;

  /// User module version
  swiftscan_string_ref_t user_module_version;
} swiftscan_swift_binary_details_t;

/// Swift placeholder modules carry additional details that specify their
/// module doc path and source info paths.
typedef struct {
  /// The path to the pre-compiled binary module
  swiftscan_string_ref_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  swiftscan_string_ref_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  swiftscan_string_ref_t module_source_info_path;
} swiftscan_swift_placeholder_details_t;

/// Clang modules are built from a module map file.
typedef struct {
  /// The path to the module map used to build this module.
  swiftscan_string_ref_t module_map_path;

  /// clang-generated context hash
  swiftscan_string_ref_t context_hash;

  /// Options to the compile command required to build this clang modulemap
  swiftscan_string_set_t *command_line;

  /// The CASID for CASFileSystemRoot
  swiftscan_string_ref_t cas_fs_root_id;

  /// The CASID for CASFileSystemRoot
  swiftscan_string_ref_t clang_include_tree;

  /// ModuleCacheKey
  swiftscan_string_ref_t module_cache_key;
} swiftscan_clang_details_t;

struct swiftscan_module_details_s {
  swiftscan_dependency_info_kind_t kind;
  union {
    swiftscan_swift_textual_details_t swift_textual_details;
    swiftscan_swift_binary_details_t swift_binary_details;
    swiftscan_swift_placeholder_details_t swift_placeholder_details;
    swiftscan_clang_details_t clang_details;
  };
};

struct swiftscan_import_set_s {
  /// The complete list of imports discovered
  swiftscan_string_set_t *imports;
  /// Diagnostics produced during this import scan
  swiftscan_diagnostic_set_t *diagnostics;
};

struct swiftscan_scan_invocation_s {
  swiftscan_string_ref_t working_directory;
  swiftscan_string_set_t *argv;
};

struct swiftscan_diagnostic_info_s {
  swiftscan_string_ref_t message;
  swiftscan_diagnostic_severity_t severity;
  swiftscan_source_location_t source_location;
};

struct swiftscan_source_location_s {
  swiftscan_string_ref_t buffer_identifier;
  uint32_t line_number;
  uint32_t column_number;
};

#endif // SWIFT_C_DEPENDENCY_SCAN_IMPL_H
