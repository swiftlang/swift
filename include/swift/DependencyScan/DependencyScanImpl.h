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
#include "swift/DependencyScan/DependencyScanningTool.h"

DEPSCAN_BEGIN_DECLS

/// Swift modules to be built from a module interface, may have a bridging
/// header.
typedef struct {
  /// The module interface from which this module was built, if any.
  ds_string_t module_interface_path;

  /// The paths of potentially ready-to-use compiled modules for the interface.
  ds_string_set_t *compiled_module_candidates;

  /// The bridging header, if any.
  ds_string_t bridging_header_path;

  /// The source files referenced by the bridging header.
  ds_string_set_t *bridging_source_files;

  /// (Clang) modules on which the bridging header depends.
  ds_string_set_t *bridging_module_dependencies;

  /// Options to the compile command required to build this module interface
  ds_string_set_t *command_line;

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  ds_string_set_t *extra_pcm_args;

  /// The hash value that will be used for the generated module
  ds_string_t context_hash;

  /// A flag to indicate whether or not this module is a framework.
  bool is_framework;
} ds_swift_textual_details_t;

/// Swift modules with only a binary module file.
typedef struct {
  /// The path to the pre-compiled binary module
  ds_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  ds_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  ds_string_t module_source_info_path;
} ds_swift_binary_details_t;

/// Swift placeholder modules carry additional details that specify their
/// module doc path and source info paths.
typedef struct {
  /// The path to the pre-compiled binary module
  ds_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  ds_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  ds_string_t module_source_info_path;
} ds_swift_placeholder_details_t;

/// Clang modules are built from a module map file.
typedef struct {
  /// The path to the module map used to build this module.
  ds_string_t module_map_path;

  /// clang-generated context hash
  ds_string_t context_hash;

  /// Options to the compile command required to build this clang modulemap
  ds_string_set_t *command_line;
} ds_clang_details_t;

typedef struct {
  ds_dependency_info_kind_t kind;
  union {
    ds_swift_textual_details_t swift_textual_details;
    ds_swift_binary_details_t swift_binary_details;
    ds_swift_placeholder_details_t swift_placeholder_details;
    ds_clang_details_t clang_details;
  };
} ds_impl_module_details_t;

inline swift::dependencies::DependencyScanningTool *
unwrap_scanner(ds_scanner_t P) {
  return reinterpret_cast<swift::dependencies::DependencyScanningTool *>(P);
}

inline ds_scanner_t
wrap_scanner(const swift::dependencies::DependencyScanningTool *P) {
  return reinterpret_cast<ds_scanner_t>(
      const_cast<swift::dependencies::DependencyScanningTool *>(P));
}

inline ds_impl_module_details_t *unwrap_details(ds_module_details_t P) {
  return reinterpret_cast<ds_impl_module_details_t *>(P);
}

inline ds_module_details_t wrap_details(const ds_impl_module_details_t *P) {
  return reinterpret_cast<ds_module_details_t>(
      const_cast<ds_impl_module_details_t *>(P));
}

DEPSCAN_END_DECLS

#endif // SWIFT_C_DEPENDENCY_SCAN_IMPL_H
