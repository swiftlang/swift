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

/// Swift modules to be built from a module interface, may have a bridging
/// header.
typedef struct {
  /// The module interface from which this module was built, if any.
  swiftscan_string_t module_interface_path;

  /// The paths of potentially ready-to-use compiled modules for the interface.
  swiftscan_string_set_t *compiled_module_candidates;

  /// The bridging header, if any.
  swiftscan_string_t bridging_header_path;

  /// The source files referenced by the bridging header.
  swiftscan_string_set_t *bridging_source_files;

  /// (Clang) modules on which the bridging header depends.
  swiftscan_string_set_t *bridging_module_dependencies;

  /// Options to the compile command required to build this module interface
  swiftscan_string_set_t *command_line;

  /// To build a PCM to be used by this Swift module, we need to append these
  /// arguments to the generic PCM build arguments reported from the dependency
  /// graph.
  swiftscan_string_set_t *extra_pcm_args;

  /// The hash value that will be used for the generated module
  swiftscan_string_t context_hash;

  /// A flag to indicate whether or not this module is a framework.
  bool is_framework;
} swiftscan_swift_textual_details_t;

/// Swift modules with only a binary module file.
typedef struct {
  /// The path to the pre-compiled binary module
  swiftscan_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  swiftscan_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  swiftscan_string_t module_source_info_path;
} swiftscan_swift_binary_details_t;

/// Swift placeholder modules carry additional details that specify their
/// module doc path and source info paths.
typedef struct {
  /// The path to the pre-compiled binary module
  swiftscan_string_t compiled_module_path;

  /// The path to the .swiftModuleDoc file.
  swiftscan_string_t module_doc_path;

  /// The path to the .swiftSourceInfo file.
  swiftscan_string_t module_source_info_path;
} swiftscan_swift_placeholder_details_t;

/// Clang modules are built from a module map file.
typedef struct {
  /// The path to the module map used to build this module.
  swiftscan_string_t module_map_path;

  /// clang-generated context hash
  swiftscan_string_t context_hash;

  /// Options to the compile command required to build this clang modulemap
  swiftscan_string_set_t *command_line;
} swiftscan_clang_details_t;

typedef struct {
  swiftscan_dependency_info_kind_t kind;
  union {
    swiftscan_swift_textual_details_t swift_textual_details;
    swiftscan_swift_binary_details_t swift_binary_details;
    swiftscan_swift_placeholder_details_t swift_placeholder_details;
    swiftscan_clang_details_t clang_details;
  };
} swiftscan_impl_module_details_t;

inline swift::dependencies::DependencyScanningTool *
unwrap_scanner(swiftscan_scanner_t P) {
  return reinterpret_cast<swift::dependencies::DependencyScanningTool *>(P);
}

inline swiftscan_scanner_t
wrap_scanner(const swift::dependencies::DependencyScanningTool *P) {
  return reinterpret_cast<swiftscan_scanner_t>(
      const_cast<swift::dependencies::DependencyScanningTool *>(P));
}

inline swiftscan_impl_module_details_t *unwrap_details(swiftscan_module_details_t P) {
  return reinterpret_cast<swiftscan_impl_module_details_t *>(P);
}

inline swiftscan_module_details_t wrap_details(const swiftscan_impl_module_details_t *P) {
  return reinterpret_cast<swiftscan_module_details_t>(
      const_cast<swiftscan_impl_module_details_t *>(P));
}

#endif // SWIFT_C_DEPENDENCY_SCAN_IMPL_H
