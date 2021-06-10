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

#include "swift/Basic/LLVMInitialize.h"
#include "swift/DependencyScan/DependencyScanImpl.h"
#include "swift/DependencyScan/DependencyScanningTool.h"
#include "swift/DependencyScan/StringUtils.h"
#include "swift/Option/Options.h"

using namespace swift::dependencies;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(DependencyScanningTool, swiftscan_scanner_t);

//=== Private Cleanup Functions -------------------------------------------===//

/// Free the given string.
void swiftscan_string_dispose(swiftscan_string_ref_t string) {
  if (string.data)
    free(const_cast<void *>(string.data));
}

void swiftscan_dependency_info_details_dispose(
    swiftscan_module_details_t details) {
  swiftscan_module_details_s *details_impl = details;
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

void swiftscan_dependency_info_dispose(swiftscan_dependency_info_t info) {
  swiftscan_string_dispose(info->module_name);
  swiftscan_string_dispose(info->module_path);
  swiftscan_string_set_dispose(info->source_files);
  swiftscan_string_set_dispose(info->direct_dependencies);
  swiftscan_dependency_info_details_dispose(info->details);
  delete info;
}

void swiftscan_dependency_set_dispose(swiftscan_dependency_set_t *set) {
  for (size_t i = 0; i < set->count; ++i) {
    swiftscan_dependency_info_dispose(set->modules[i]);
  }
  delete[] set->modules;
  delete set;
}

//=== Scanner Cache Operations --------------------------------------------===//

void swiftscan_scanner_cache_serialize(swiftscan_scanner_t scanner,
                                       const char * path) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  ScanningTool->serializeCache(path);
}

bool swiftscan_scanner_cache_load(swiftscan_scanner_t scanner,
                                  const char * path) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  return ScanningTool->loadCache(path);
}

void swiftscan_scanner_cache_reset(swiftscan_scanner_t scanner) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  ScanningTool->resetCache();
}

//=== Scanner Functions ---------------------------------------------------===//

swiftscan_scanner_t swiftscan_scanner_create(void) {
  INITIALIZE_LLVM();
  return wrap(new DependencyScanningTool());
}

void swiftscan_scanner_dispose(swiftscan_scanner_t c_scanner) {
  delete unwrap(c_scanner);
}

swiftscan_dependency_graph_t
swiftscan_dependency_graph_create(swiftscan_scanner_t scanner,
                                  swiftscan_scan_invocation_t invocation) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  int argc = invocation->argv->count;
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(get_C_string(invocation->argv->strings[i]));

  // Execute the scan and bridge the result
  auto ScanResult = ScanningTool->getDependencies(Compilation, {});
  if (ScanResult.getError())
    return nullptr;
  auto DependencyGraph = std::move(*ScanResult);
  return DependencyGraph;
}

swiftscan_batch_scan_result_t *
swiftscan_batch_scan_result_create(swiftscan_scanner_t scanner,
                                   swiftscan_batch_scan_input_t *batch_input,
                                   swiftscan_scan_invocation_t invocation) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  int argc = invocation->argv->count;
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(get_C_string(invocation->argv->strings[i]));

  std::vector<BatchScanInput> BatchInput;
  for (size_t i = 0; i < batch_input->count; ++i) {
    swiftscan_batch_scan_entry_s *Entry = batch_input->modules[i];
    BatchInput.push_back({get_C_string(Entry->module_name),
                          get_C_string(Entry->arguments),
                          /*outputPath*/ "", Entry->is_swift});
  }

  // Execute the scan and bridge the result
  auto BatchScanResult =
      ScanningTool->getDependencies(Compilation, BatchInput, {});
  swiftscan_batch_scan_result_t *Result = new swiftscan_batch_scan_result_t;
  auto ResultGraphs = new swiftscan_dependency_graph_t[BatchScanResult.size()];
  for (size_t i = 0; i < BatchScanResult.size(); ++i) {
    auto &ResultOrErr = BatchScanResult[i];
    if (ResultOrErr.getError()) {
      ResultGraphs[i] = nullptr;
      continue;
    }

    ResultGraphs[i] = ResultOrErr.get();
  }

  Result->results = ResultGraphs;
  Result->count = BatchScanResult.size();
  return Result;
}

swiftscan_import_set_t
swiftscan_import_set_create(swiftscan_scanner_t scanner,
                            swiftscan_scan_invocation_t invocation) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  int argc = invocation->argv->count;
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(get_C_string(invocation->argv->strings[i]));

  // Execute the scan and bridge the result
  auto PreScanResult = ScanningTool->getImports(Compilation);
  if (PreScanResult.getError())
    return nullptr;
  auto ImportSet = std::move(*PreScanResult);
  return ImportSet;
}

//=== Dependency Result Functions -----------------------------------------===//

swiftscan_string_ref_t swiftscan_dependency_graph_get_main_module_name(
    swiftscan_dependency_graph_t result) {
  return result->main_module_name;
}

swiftscan_dependency_set_t *swiftscan_dependency_graph_get_dependencies(
    swiftscan_dependency_graph_t result) {
  return result->dependencies;
}

//=== Module Dependency Info query APIs -----------------------------------===//

swiftscan_string_ref_t
swiftscan_module_info_get_module_name(swiftscan_dependency_info_t info) {
  return info->module_name;
}

swiftscan_string_ref_t
swiftscan_module_info_get_module_path(swiftscan_dependency_info_t info) {
  return info->module_path;
}

swiftscan_string_set_t *
swiftscan_module_info_get_source_files(swiftscan_dependency_info_t info) {
  return info->source_files;
}

swiftscan_string_set_t *swiftscan_module_info_get_direct_dependencies(
    swiftscan_dependency_info_t info) {
  return info->direct_dependencies;
}

swiftscan_module_details_t
swiftscan_module_info_get_details(swiftscan_dependency_info_t info) {
  return info->details;
}

//=== Swift Textual Module Details query APIs -----------------------------===//

swiftscan_dependency_info_kind_t
swiftscan_module_detail_get_kind(swiftscan_module_details_t details) {
  return details->kind;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_module_interface_path(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.module_interface_path;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_compiled_module_candidates(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.compiled_module_candidates;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_bridging_header_path(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.bridging_header_path;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_source_files(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.bridging_source_files;
}

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_module_dependencies(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.bridging_module_dependencies;
}

swiftscan_string_set_t *swiftscan_swift_textual_detail_get_command_line(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.command_line;
}

swiftscan_string_set_t *swiftscan_swift_textual_detail_get_extra_pcm_args(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.extra_pcm_args;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_context_hash(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.context_hash;
}

bool swiftscan_swift_textual_detail_get_is_framework(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.is_framework;
}

//=== Swift Binary Module Details query APIs ------------------------------===//

swiftscan_string_ref_t swiftscan_swift_binary_detail_get_compiled_module_path(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.compiled_module_path;
}

swiftscan_string_ref_t swiftscan_swift_binary_detail_get_module_doc_path(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.module_doc_path;
}

swiftscan_string_ref_t
swiftscan_swift_binary_detail_get_module_source_info_path(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.module_source_info_path;
}

//=== Swift Placeholder Module Details query APIs -------------------------===//

swiftscan_string_ref_t
swiftscan_swift_placeholder_detail_get_compiled_module_path(
    swiftscan_module_details_t details) {
  return details->swift_placeholder_details.module_source_info_path;
}

swiftscan_string_ref_t swiftscan_swift_placeholder_detail_get_module_doc_path(
    swiftscan_module_details_t details) {
  return details->swift_placeholder_details.module_source_info_path;
}

swiftscan_string_ref_t
swiftscan_swift_placeholder_detail_get_module_source_info_path(
    swiftscan_module_details_t details) {
  return details->swift_placeholder_details.module_source_info_path;
}

//=== Clang Module Details query APIs -------------------------------------===//

swiftscan_string_ref_t
swiftscan_clang_detail_get_module_map_path(swiftscan_module_details_t details) {
  return details->clang_details.module_map_path;
}

swiftscan_string_ref_t
swiftscan_clang_detail_get_context_hash(swiftscan_module_details_t details) {
  return details->clang_details.context_hash;
}

swiftscan_string_set_t *
swiftscan_clang_detail_get_command_line(swiftscan_module_details_t details) {
  return details->clang_details.command_line;
}

//=== Batch Scan Input Functions ------------------------------------------===//

swiftscan_batch_scan_input_t *swiftscan_batch_scan_input_create() {
  return new swiftscan_batch_scan_input_t;
}

void swiftscan_batch_scan_input_set_modules(
    swiftscan_batch_scan_input_t *input, int count,
    swiftscan_batch_scan_entry_t *modules) {
  input->count = count;
  input->modules = modules;
}

//=== Batch Scan Entry Functions ------------------------------------------===//

swiftscan_batch_scan_entry_t swiftscan_batch_scan_entry_create() {
  return new swiftscan_batch_scan_entry_s;
}

void swiftscan_batch_scan_entry_set_module_name(
    swiftscan_batch_scan_entry_t entry, const char *name) {
  entry->module_name = create_clone(name);
}

void swiftscan_batch_scan_entry_set_arguments(
    swiftscan_batch_scan_entry_t entry, const char *arguments) {
  entry->arguments = create_clone(arguments);
}

void swiftscan_batch_scan_entry_set_is_swift(swiftscan_batch_scan_entry_t entry,
                                             bool is_swift) {
  entry->is_swift = is_swift;
}

swiftscan_string_ref_t
swiftscan_batch_scan_entry_get_module_name(swiftscan_batch_scan_entry_t entry) {
  return entry->module_name;
}

swiftscan_string_ref_t
swiftscan_batch_scan_entry_get_arguments(swiftscan_batch_scan_entry_t entry) {
  return entry->arguments;
}

bool swiftscan_batch_scan_entry_get_is_swift(
    swiftscan_batch_scan_entry_t entry) {
  return entry->is_swift;
}

//=== Prescan Result Functions --------------------------------------------===//

swiftscan_string_set_t *
swiftscan_import_set_get_imports(swiftscan_import_set_t result) {
  return result->imports;
}

//=== Scanner Invocation Functions ----------------------------------------===//

swiftscan_scan_invocation_t swiftscan_scan_invocation_create() {
  return new swiftscan_scan_invocation_s;
}

void swiftscan_scan_invocation_set_working_directory(
    swiftscan_scan_invocation_t invocation, const char *working_directory) {
  invocation->working_directory = create_clone(working_directory);
}

SWIFTSCAN_PUBLIC void
swiftscan_scan_invocation_set_argv(swiftscan_scan_invocation_t invocation,
                                   int argc, const char **argv) {
  invocation->argv = create_set(argc, argv);
}

swiftscan_string_ref_t swiftscan_scan_invocation_get_working_directory(
    swiftscan_scan_invocation_t invocation) {
  return invocation->working_directory;
}

int swiftscan_scan_invocation_get_argc(swiftscan_scan_invocation_t invocation) {
  return invocation->argv->count;
}

swiftscan_string_set_t *
swiftscan_scan_invocation_get_argv(swiftscan_scan_invocation_t invocation) {
  return invocation->argv;
}

//=== Public Cleanup Functions --------------------------------------------===//

void swiftscan_string_set_dispose(swiftscan_string_set_t *set) {
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    swiftscan_string_dispose(set->strings[SI]);
  delete[] set->strings;
  delete set;
}

void swiftscan_dependency_graph_dispose(swiftscan_dependency_graph_t result) {
  swiftscan_string_dispose(result->main_module_name);
  swiftscan_dependency_set_dispose(result->dependencies);
  delete result;
}

void swiftscan_import_set_dispose(swiftscan_import_set_t result) {
  swiftscan_string_set_dispose(result->imports);
  delete result;
}

void swiftscan_batch_scan_entry_dispose(swiftscan_batch_scan_entry_t entry) {
  swiftscan_string_dispose(entry->module_name);
  swiftscan_string_dispose(entry->arguments);
  delete entry;
}

void swiftscan_batch_scan_input_dispose(swiftscan_batch_scan_input_t *input) {
  for (size_t i = 0; i < input->count; ++i) {
    swiftscan_batch_scan_entry_dispose(input->modules[i]);
  }
  delete[] input->modules;
  delete input;
}

void swiftscan_batch_scan_result_dispose(
    swiftscan_batch_scan_result_t *result) {
  for (size_t i = 0; i < result->count; ++i) {
    swiftscan_dependency_graph_dispose(result->results[i]);
  }
  delete[] result->results;
  delete result;
}

void swiftscan_scan_invocation_dispose(swiftscan_scan_invocation_t invocation) {
  swiftscan_string_dispose(invocation->working_directory);
  swiftscan_string_set_dispose(invocation->argv);
  delete invocation;
}

//=== Feature-Query Functions -----------------------------------------===//
static void addFrontendFlagOption(llvm::opt::OptTable &table,
                                  swift::options::ID id,
                                  std::vector<std::string> &frontendOptions) {
  if (table.getOption(id).hasFlag(swift::options::FrontendOption)) {
    auto name = table.getOptionName(id);
    if (strlen(name) > 0) {
      frontendOptions.push_back(std::string(name));
    }
  }
}

swiftscan_string_set_t *
swiftscan_compiler_supported_arguments_query() {
  std::unique_ptr<llvm::opt::OptTable> table = swift::createSwiftOptTable();
  std::vector<std::string> frontendFlags;
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  addFrontendFlagOption(*table, swift::options::OPT_##ID, frontendFlags);
#include "swift/Option/Options.inc"
#undef OPTION
  return create_set(frontendFlags);
}

swiftscan_string_set_t *
swiftscan_compiler_supported_features_query() {
  // TODO: We are yet to figure out how "Features" will be organized.
  return nullptr;
}
