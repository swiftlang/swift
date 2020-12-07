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

swiftscan_scanner_t swiftscan_scanner_create(void) {
  return wrap_scanner(new DependencyScanningTool());
}

void swiftscan_scanner_dispose(swiftscan_scanner_t c_scanner) {
  delete unwrap_scanner(c_scanner);
}

swiftscan_dependency_result_t
swiftscan_scan_dependencies(swiftscan_scanner_t *scanner,
                            const char *working_directory, int argc,
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

swiftscan_batch_scan_result_t *
swiftscan_batch_scan_dependencies(swiftscan_scanner_t *scanner,
                                  const char *working_directory,
                                  swiftscan_batch_scan_input_t *batch_input,
                                  int argc, const char *const *argv) {
  DependencyScanningTool *ScanningTool = unwrap_scanner(scanner);
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(argv[i]);

  std::vector<BatchScanInput> BatchInput;
  for (int i = 0; i < batch_input->count; ++i) {
    swiftscan_batch_scan_entry_t &Entry = batch_input->modules[i];
    BatchInput.push_back({swiftscan_get_C_string(Entry.module_name),
                          swiftscan_get_C_string(Entry.arguments),
                          /*outputPath*/ "", Entry.is_swift});
  }

  // Execute the scan and bridge the result
  auto BatchScanResult =
      ScanningTool->getDependencies(Compilation, BatchInput, {});
  swiftscan_batch_scan_result_t *Result = new swiftscan_batch_scan_result_t;
  auto ResultGraphs =
      new swiftscan_dependency_result_t[BatchScanResult.size()];
  for (size_t i = 0; i < BatchScanResult.size(); ++i) {
    auto &ResultOrErr = BatchScanResult[i];
    if (ResultOrErr.getError())
      ResultGraphs[i] = nullptr;

    ResultGraphs[i] = ResultOrErr.get();
  }

  Result->results = ResultGraphs;
  return Result;
}

swiftscan_prescan_result_t *
swiftscan_prescan_dependencies(swiftscan_scanner_t *scanner,
                               const char *working_directory, int argc,
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

//=== Module Dependency Info query APIs -----------------------------------===//

swiftscan_string_t
swiftscan_module_info_get_module_name(swiftscan_dependency_info_t info) {
  return unwrap_info(info)->module_name;
}

swiftscan_string_t
swiftscan_module_info_get_module_path(swiftscan_dependency_info_t info) {
  return unwrap_info(info)->module_path;
}

swiftscan_string_set_t *
swiftscan_module_info_get_source_files(swiftscan_dependency_info_t info) {
  return unwrap_info(info)->source_files;
}

swiftscan_string_set_t *swiftscan_module_info_get_direct_dependencies(
    swiftscan_dependency_info_t info) {
  return unwrap_info(info)->direct_dependencies;
}

swiftscan_module_details_t
swiftscan_module_info_get_details(swiftscan_dependency_info_t info) {
  return unwrap_info(info)->details;
}

//=== Swift Textual Module Details query APIs -----------------------------===//

swiftscan_dependency_info_kind_t
swiftscan_module_detail_get_kind(swiftscan_module_details_t details) {
  return unwrap_details(details)->kind;
}

swiftscan_string_t swiftscan_swift_textual_detail_get_module_interface_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.module_interface_path;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_compiled_module_candidates(
    swiftscan_module_details_t details) {
  return unwrap_details(details)
      ->swift_textual_details.compiled_module_candidates;
}

swiftscan_string_t swiftscan_swift_textual_detail_get_bridging_header_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.bridging_header_path;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_source_files(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.bridging_source_files;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_module_dependencies(
    swiftscan_module_details_t details) {
  return unwrap_details(details)
      ->swift_textual_details.bridging_module_dependencies;
}

swiftscan_string_set_t *swiftscan_swift_textual_detail_get_command_line(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.command_line;
}

swiftscan_string_set_t *swiftscan_swift_textual_detail_get_extra_pcm_args(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.extra_pcm_args;
}

swiftscan_string_t swiftscan_swift_textual_detail_get_context_hash(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.context_hash;
}

bool swiftscan_swift_textual_detail_get_is_framework(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_textual_details.is_framework;
}

//=== Swift Binary Module Details query APIs ------------------------------===//

swiftscan_string_t swiftscan_swift_binary_detail_get_compiled_module_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.compiled_module_path;
}

swiftscan_string_t swiftscan_swift_binary_detail_get_module_doc_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.module_doc_path;
}

swiftscan_string_t swiftscan_swift_binary_detail_get_module_source_info_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)->swift_binary_details.module_source_info_path;
}

//=== Swift Placeholder Module Details query APIs -------------------------===//

swiftscan_string_t swiftscan_swift_placeholder_detail_get_compiled_module_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

swiftscan_string_t swiftscan_swift_placeholder_detail_get_module_doc_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

swiftscan_string_t
swiftscan_swift_placeholder_detail_get_module_source_info_path(
    swiftscan_module_details_t details) {
  return unwrap_details(details)
      ->swift_placeholder_details.module_source_info_path;
}

//=== Clang Module Details query APIs -------------------------------------===//

swiftscan_string_t
swiftscan_clang_detail_get_module_map_path(swiftscan_module_details_t details) {
  return unwrap_details(details)->clang_details.module_map_path;
}

swiftscan_string_t
swiftscan_clang_detail_get_context_hash(swiftscan_module_details_t details) {
  return unwrap_details(details)->clang_details.context_hash;
}

swiftscan_string_set_t *
swiftscan_clang_detail_get_command_line(swiftscan_module_details_t details) {
  return unwrap_details(details)->clang_details.command_line;
}

//=== Cleanup Functions ---------------------------------------------------===//

void swiftscan_dependency_info_details_dispose(
    swiftscan_module_details_t details) {
  swiftscan_impl_module_details_t *details_impl = unwrap_details(details);
  switch (details_impl->kind) {
  case SWIFTSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL:
    swiftscan_string_dispose(
        details_impl->swift_textual_details.module_interface_path);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.compiled_module_candidates);
    swiftscan_string_dispose(
        details_impl->swift_textual_details.bridging_header_path);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.bridging_source_files);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.bridging_module_dependencies);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.command_line);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.extra_pcm_args);
    swiftscan_string_dispose(details_impl->swift_textual_details.context_hash);
    break;
  case SWIFTSCAN_DEPENDENCY_INFO_SWIFT_BINARY:
    swiftscan_string_dispose(
        details_impl->swift_binary_details.compiled_module_path);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.module_doc_path);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.module_source_info_path);
    break;
  case SWIFTSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER:
    swiftscan_string_dispose(
        details_impl->swift_placeholder_details.compiled_module_path);
    swiftscan_string_dispose(
        details_impl->swift_placeholder_details.module_doc_path);
    swiftscan_string_dispose(
        details_impl->swift_placeholder_details.module_source_info_path);
    break;
  case SWIFTSCAN_DEPENDENCY_INFO_CLANG:
    swiftscan_string_dispose(details_impl->clang_details.module_map_path);
    swiftscan_string_dispose(details_impl->clang_details.context_hash);
    swiftscan_string_set_dispose(details_impl->clang_details.command_line);
    break;
  }
  delete details_impl;
}

void swiftscan_dependency_info_dispose(swiftscan_dependency_info_t *info) {
  swiftscan_string_dispose(unwrap_info(info)->module_name);
  swiftscan_string_dispose(unwrap_info(info)->module_path);
  swiftscan_string_set_dispose(unwrap_info(info)->source_files);
  swiftscan_string_set_dispose(unwrap_info(info)->direct_dependencies);
  swiftscan_dependency_info_details_dispose(unwrap_info(info)->details);
}

void swiftscan_dependency_set_dispose(swiftscan_dependency_set_t *set) {
  for (int i = 0; i < set->count; ++i) {
    swiftscan_dependency_info_dispose(&set->modules[i]);
  }
  delete[] set->modules;
  delete set;
}

void swiftscan_dependency_result_dispose(
    swiftscan_dependency_result_t result) {
  swiftscan_impl_dependency_result_t *result_impl = unwrap_result(result);
  swiftscan_string_dispose(result_impl->main_module_name);
  swiftscan_dependency_set_dispose(result_impl->module_set);
  delete result_impl;
}

void swiftscan_prescan_result_dispose(swiftscan_prescan_result_t *result) {
  swiftscan_string_set_dispose(result->import_set);
  delete result;
}

void swiftscan_batch_scan_entry_dispose(swiftscan_batch_scan_entry_t *entry) {
  swiftscan_string_dispose(entry->module_name);
  swiftscan_string_dispose(entry->arguments);
  delete entry;
}

void swiftscan_batch_scan_input_dispose(swiftscan_batch_scan_input_t *input) {
  for (int i = 0; i < input->count; ++i) {
    swiftscan_batch_scan_entry_dispose(&input->modules[i]);
  }
}

void swiftscan_batch_scan_result_dispose(
    swiftscan_batch_scan_result_t *result) {
  for (int i = 0; i < result->count; ++i) {
    swiftscan_dependency_result_dispose(result->results[i]);
  }
}
