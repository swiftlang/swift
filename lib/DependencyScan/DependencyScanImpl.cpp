//===------------ DependencyScanImpl.cpp - Swift Compiler -----------------===//
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
// Implementation of the dependency scanning C API
//
//===----------------------------------------------------------------------===//

#include "swift/DependencyScan/DependencyScanImpl.h"

using namespace swift::dependencies;

//=== Scanner Functions ---------------------------------------------------===//

ds_scanner_t ds_scanner_create(void) {
  return wrap_scanner(new DependencyScanningTool());
}

void ds_scanner_dispose(ds_scanner_t c_scanner) {
  delete unwrap_scanner(c_scanner);
}

ds_dependency_result_t *ds_scan_dependencies(ds_scanner_t *scanner,
                                             const char *working_directory,
                                             int argc,
                                             const char *const *argv) {
  DependencyScanningTool *ScanningTool = unwrap_scanner(scanner);
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(argv[i]);

  // Execute the scan and bridge the result
  auto ScanResult = ScanningTool->getDependencies(Compilation, {});
  if (ScanResult.getError())
    return nullptr;
  auto DependencyGraph = std::move(*ScanResult);
  return DependencyGraph;
}

ds_prescan_result_t *ds_prescan_dependencies(ds_scanner_t *scanner,
                                             const char *working_directory,
                                             int argc,
                                             const char *const *argv) {
  DependencyScanningTool *ScanningTool = unwrap_scanner(scanner);
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(argv[i]);

  // Execute the scan and bridge the result
  auto PreScanResult = ScanningTool->getImports(Compilation);
  if (PreScanResult.getError())
    return nullptr;
  auto ImportSet = std::move(*PreScanResult);
  return ImportSet;
}

//=== Swift Textual Module Details query APIs -----------------------------===//

ds_dependency_info_kind_t
ds_get_module_detail_kind(ds_module_details_t details) {
  return unwrap_details(details)->kind;
}

ds_string_t
ds_get_swift_textual_detail_module_interface_path(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.module_interface_path;
}

ds_string_set_t *ds_get_swift_textual_detail_compiled_module_candidates(
    ds_module_details_t details) {
  return unwrap_details(details)
      ->swift_textual_details.compiled_module_candidates;
}

ds_string_t
ds_get_swift_textual_detail_bridging_header_path(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.bridging_header_path;
}

ds_string_set_t *
ds_get_swift_textual_detail_bridging_source_files(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.bridging_source_files;
}

ds_string_set_t *ds_get_swift_textual_detail_bridging_module_dependencies(
    ds_module_details_t details) {
  return unwrap_details(details)
      ->swift_textual_details.bridging_module_dependencies;
}

ds_string_set_t *
ds_get_swift_textual_detail_command_line(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.command_line;
}

ds_string_set_t *
ds_get_swift_textual_detail_extra_pcm_args(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.extra_pcm_args;
}

ds_string_t
ds_get_swift_textual_detail_context_hash(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.context_hash;
}

bool ds_get_swift_textual_detail_is_framework(ds_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.is_framework;
}

//=== Swift Binary Module Details query APIs ------------------------------===//

ds_string_t
ds_get_swift_binary_detail_compiled_module_path(ds_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.compiled_module_path;
}

ds_string_t
ds_get_swift_binary_detail_module_doc_path(ds_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.module_doc_path;
}

ds_string_t ds_get_swift_binary_detail_module_source_info_path(
    ds_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.module_source_info_path;
}

//=== Swift Placeholder Module Details query APIs -------------------------===//

ds_string_t ds_get_swift_placeholder_detail_compiled_module_path(
    ds_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

ds_string_t
ds_get_swift_placeholder_detail_module_doc_path(ds_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

ds_string_t ds_get_swift_placeholder_detail_module_source_info_path(
    ds_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

//=== Clang Module Details query APIs -------------------------------------===//

ds_string_t ds_get_clang_detail_module_map_path(ds_module_details_t details) {
  return unwrap_details(details)->clang_details.module_map_path;
}

ds_string_t ds_get_clang_detail_context_hash(ds_module_details_t details) {
  return unwrap_details(details)->clang_details.context_hash;
}

ds_string_set_t *ds_get_clang_detail_command_line(ds_module_details_t details) {
  return unwrap_details(details)->clang_details.command_line;
}

//=== Cleanup Functions ---------------------------------------------------===//

void ds_dependency_info_details_dispose(ds_module_details_t details) {
  ds_impl_module_details_t *details_impl = unwrap_details(details);
  switch (details_impl->kind) {
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL:
    ds_string_dispose(
        details_impl->swift_textual_details.module_interface_path);
    ds_string_set_dispose(
        details_impl->swift_textual_details.compiled_module_candidates);
    ds_string_dispose(details_impl->swift_textual_details.bridging_header_path);
    ds_string_set_dispose(
        details_impl->swift_textual_details.bridging_source_files);
    ds_string_set_dispose(
        details_impl->swift_textual_details.bridging_module_dependencies);
    ds_string_set_dispose(details_impl->swift_textual_details.command_line);
    ds_string_set_dispose(details_impl->swift_textual_details.extra_pcm_args);
    ds_string_dispose(details_impl->swift_textual_details.context_hash);
    break;
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_BINARY:
    ds_string_dispose(details_impl->swift_binary_details.compiled_module_path);
    ds_string_dispose(details_impl->swift_binary_details.module_doc_path);
    ds_string_dispose(
        details_impl->swift_binary_details.module_source_info_path);
    break;
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER:
    ds_string_dispose(
        details_impl->swift_placeholder_details.compiled_module_path);
    ds_string_dispose(details_impl->swift_placeholder_details.module_doc_path);
    ds_string_dispose(
        details_impl->swift_placeholder_details.module_source_info_path);
    break;
  case DEPSCAN_DEPENDENCY_INFO_CLANG:
    ds_string_dispose(details_impl->clang_details.module_map_path);
    ds_string_dispose(details_impl->clang_details.context_hash);
    ds_string_set_dispose(details_impl->clang_details.command_line);
    break;
  }
  delete details_impl;
}

void ds_dependency_info_dispose(ds_dependency_info_t *info) {
  ds_string_dispose(info->module_name);
  ds_string_dispose(info->module_path);
  ds_string_set_dispose(info->source_files);
  ds_string_set_dispose(info->direct_dependencies);
  ds_dependency_info_details_dispose(info->details);
}

void ds_dependency_set_dispose(ds_dependency_set_t *set) {
  for (int i = 0; i < set->count; ++i) {
    ds_dependency_info_dispose(&set->modules[i]);
  }
  delete[] set->modules;
  delete set;
}

void ds_dependency_result_dispose(ds_dependency_result_t *result) {
  ds_string_dispose(result->main_module_name);
  ds_dependency_set_dispose(result->module_set);
  delete result;
}

void ds_prescan_result_dispose(ds_prescan_result_t *result) {
  ds_string_set_dispose(result->import_set);
  delete result;
}
