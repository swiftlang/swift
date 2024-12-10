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

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/InitializeSwiftModules.h"
#include "swift/DependencyScan/DependencyScanImpl.h"
#include "swift/DependencyScan/DependencyScanningTool.h"
#include "swift/DependencyScan/StringUtils.h"
#include "swift/DriverTool/DriverTool.h"
#include "swift/Option/Options.h"
#include "swift/SIL/SILBridging.h"

using namespace swift::dependencies;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(DependencyScanningTool, swiftscan_scanner_t)

//=== Private Cleanup Functions -------------------------------------------===//
void swiftscan_macro_dependency_dispose(
    swiftscan_macro_dependency_set_t *macro) {
  if (!macro)
    return;

  for (unsigned i = 0; i < macro->count; ++i) {
    swiftscan_string_dispose(macro->macro_dependencies[i]->moduleName);
    swiftscan_string_dispose(macro->macro_dependencies[i]->libraryPath);
    swiftscan_string_dispose(macro->macro_dependencies[i]->executablePath);
    delete macro->macro_dependencies[i];
  }
  delete[] macro->macro_dependencies;
  delete macro;
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
        details_impl->swift_textual_details.swift_overlay_module_dependencies);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.command_line);
    swiftscan_string_set_dispose(
        details_impl->swift_textual_details.extra_pcm_args);
    swiftscan_string_dispose(details_impl->swift_textual_details.context_hash);
    swiftscan_string_dispose(
        details_impl->swift_textual_details.cas_fs_root_id);
    swiftscan_string_dispose(
        details_impl->swift_textual_details.bridging_header_include_tree);
    swiftscan_string_dispose(
        details_impl->swift_textual_details.module_cache_key);
    swiftscan_macro_dependency_dispose(
        details_impl->swift_textual_details.macro_dependencies);
    swiftscan_string_dispose(
        details_impl->swift_textual_details.user_module_version);
    break;
  case SWIFTSCAN_DEPENDENCY_INFO_SWIFT_BINARY:
    swiftscan_string_dispose(
        details_impl->swift_binary_details.compiled_module_path);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.module_doc_path);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.module_source_info_path);
    swiftscan_string_set_dispose(
        details_impl->swift_binary_details.swift_overlay_module_dependencies);
      swiftscan_string_dispose(
        details_impl->swift_binary_details.header_dependency);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.module_cache_key);
    swiftscan_string_dispose(
        details_impl->swift_binary_details.user_module_version);
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
    swiftscan_string_set_dispose(details_impl->clang_details.captured_pcm_args);
    swiftscan_string_dispose(details_impl->clang_details.cas_fs_root_id);
    swiftscan_string_dispose(details_impl->clang_details.module_cache_key);
    break;
  }
  delete details_impl;
}

void swiftscan_link_library_set_dispose(swiftscan_link_library_set_t *set) {
  for (size_t i = 0; i < set->count; ++i) {
    auto info = set->link_libraries[i];
    swiftscan_string_dispose(info->name);
    delete info;
  }
  delete[] set->link_libraries;
  delete set;
}

void swiftscan_dependency_info_dispose(swiftscan_dependency_info_t info) {
  swiftscan_string_dispose(info->module_name);
  swiftscan_string_dispose(info->module_path);
  swiftscan_string_set_dispose(info->source_files);
  swiftscan_string_set_dispose(info->direct_dependencies);
  swiftscan_link_library_set_dispose(info->link_libraries);
  swiftscan_dependency_info_details_dispose(info->details);
  delete info;
}

void swiftscan_dependency_set_dispose(swiftscan_dependency_set_t *set) {
  if (set) {
    for (size_t i = 0; i < set->count; ++i) {
      swiftscan_dependency_info_dispose(set->modules[i]);
    }
    delete[] set->modules;
    delete set;
  }
}

//=== Scanner Cache Operations --------------------------------------------===//

void swiftscan_scanner_cache_reset(swiftscan_scanner_t scanner) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  ScanningTool->resetCache();
}

//=== Scanner Functions ---------------------------------------------------===//

swiftscan_scanner_t swiftscan_scanner_create(void) {
  static std::mutex initializationMutex;
  std::lock_guard<std::mutex> lock(initializationMutex);
  INITIALIZE_LLVM();
  if (!swiftModulesInitialized())
    initializeSwiftModules();
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
    Compilation.push_back(swift::c_string_utils::get_C_string(invocation->argv->strings[i]));

  // Execute the scan and bridge the result
  auto ScanResult = ScanningTool->getDependencies(
      Compilation, {},
      swift::c_string_utils::get_C_string(invocation->working_directory));
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
    Compilation.push_back(swift::c_string_utils::get_C_string(invocation->argv->strings[i]));

  std::vector<BatchScanInput> BatchInput;
  for (size_t i = 0; i < batch_input->count; ++i) {
    swiftscan_batch_scan_entry_s *Entry = batch_input->modules[i];
    BatchInput.push_back({swift::c_string_utils::get_C_string(Entry->module_name),
        swift::c_string_utils::get_C_string(Entry->arguments),
                          /*outputPath*/ "", Entry->is_swift});
  }

  // Execute the scan and bridge the result
  auto BatchScanResult = ScanningTool->getDependencies(
      Compilation, BatchInput, {},
      swift::c_string_utils::get_C_string(invocation->working_directory));
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
    Compilation.push_back(swift::c_string_utils::get_C_string(invocation->argv->strings[i]));

  // Execute the scan and bridge the result
  auto PreScanResult = ScanningTool->getImports(
      Compilation,
      swift::c_string_utils::get_C_string(invocation->working_directory));
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

swiftscan_diagnostic_set_t *swiftscan_dependency_graph_get_diagnostics(
    swiftscan_dependency_graph_t result) {
  return result->diagnostics;
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


swiftscan_link_library_set_t *swiftscan_module_info_get_link_libraries(
    swiftscan_dependency_info_t info) {
  return info->link_libraries;
}

swiftscan_module_details_t
swiftscan_module_info_get_details(swiftscan_dependency_info_t info) {
  return info->details;
}

//=== Link Library Info query APIs -----------------------------------===//

swiftscan_string_ref_t
swiftscan_link_library_info_get_link_name(swiftscan_link_library_info_t info) {
  return info->name;
}
bool
swiftscan_link_library_info_get_is_framework(swiftscan_link_library_info_t info) {
  return info->isFramework;
}
bool
swiftscan_link_library_info_get_should_force_load(swiftscan_link_library_info_t info) {
  return info->forceLoad;
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

swiftscan_string_set_t *
swiftscan_swift_textual_detail_get_bridging_pch_command_line(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.bridging_pch_command_line;
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

swiftscan_string_set_t *swiftscan_swift_textual_detail_get_swift_overlay_dependencies(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.swift_overlay_module_dependencies;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_cas_fs_root_id(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.cas_fs_root_id;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_module_cache_key(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.module_cache_key;
}

swiftscan_string_ref_t swiftscan_swift_textual_detail_get_user_module_version(
    swiftscan_module_details_t details) {
  return details->swift_textual_details.user_module_version;
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

swiftscan_string_set_t *
swiftscan_swift_binary_detail_get_swift_overlay_dependencies(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.swift_overlay_module_dependencies;
}

swiftscan_string_ref_t
swiftscan_swift_binary_detail_get_header_dependency(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.header_dependency;
}

swiftscan_string_set_t *
swiftscan_swift_binary_detail_get_header_dependency_module_dependencies(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.header_dependencies_module_dependnecies;
}

bool swiftscan_swift_binary_detail_get_is_framework(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.is_framework;
}

swiftscan_string_ref_t swiftscan_swift_binary_detail_get_module_cache_key(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.module_cache_key;
}

SWIFTSCAN_PUBLIC swiftscan_string_ref_t
swiftscan_swift_binary_detail_get_user_module_version(
    swiftscan_module_details_t details) {
  return details->swift_binary_details.user_module_version;
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

swiftscan_string_set_t *
swiftscan_clang_detail_get_captured_pcm_args(swiftscan_module_details_t details) {
  return details->clang_details.captured_pcm_args;
}

swiftscan_string_ref_t
swiftscan_clang_detail_get_cas_fs_root_id(swiftscan_module_details_t details) {
  return details->clang_details.cas_fs_root_id;
}

swiftscan_string_ref_t swiftscan_clang_detail_get_module_cache_key(
    swiftscan_module_details_t details) {
  return details->clang_details.module_cache_key;
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
  entry->module_name = swift::c_string_utils::create_clone(name);
}

void swiftscan_batch_scan_entry_set_arguments(
    swiftscan_batch_scan_entry_t entry, const char *arguments) {
  entry->arguments = swift::c_string_utils::create_clone(arguments);
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

swiftscan_diagnostic_set_t *
swiftscan_import_set_get_diagnostics(swiftscan_import_set_t result) {
  return result->diagnostics;
}

//=== Scanner Invocation Functions ----------------------------------------===//

swiftscan_scan_invocation_t swiftscan_scan_invocation_create() {
  return new swiftscan_scan_invocation_s;
}

void swiftscan_scan_invocation_set_working_directory(
    swiftscan_scan_invocation_t invocation, const char *working_directory) {
  invocation->working_directory = swift::c_string_utils::create_clone(working_directory);
}

void
swiftscan_scan_invocation_set_argv(swiftscan_scan_invocation_t invocation,
                                   int argc, const char **argv) {
  invocation->argv = swift::c_string_utils::create_set(argc, argv);
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

void swiftscan_string_dispose(swiftscan_string_ref_t string) {
  if (string.data)
    free(const_cast<void *>(string.data));
}

void swiftscan_string_set_dispose(swiftscan_string_set_t *set) {
  for (unsigned SI = 0, SE = set->count; SI < SE; ++SI)
    swiftscan_string_dispose(set->strings[SI]);
  if (set->count > 0)
    delete[] set->strings;
  delete set;
}

void swiftscan_dependency_graph_dispose(swiftscan_dependency_graph_t result) {
  swiftscan_string_dispose(result->main_module_name);
  swiftscan_dependency_set_dispose(result->dependencies);
  swiftscan_diagnostics_set_dispose(result->diagnostics);
  delete result;
}

void swiftscan_import_set_dispose(swiftscan_import_set_t result) {
  swiftscan_string_set_dispose(result->imports);
  swiftscan_diagnostics_set_dispose(result->diagnostics);
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
    if (!name.empty()) {
      frontendOptions.push_back(std::string(name));
    }
  }
}

swiftscan_string_ref_t
swiftscan_compiler_target_info_query(swiftscan_scan_invocation_t invocation) {
  return swiftscan_compiler_target_info_query_v2(invocation, nullptr);
}

swiftscan_string_ref_t
swiftscan_compiler_target_info_query_v2(swiftscan_scan_invocation_t invocation,
                                        const char *main_executable_path) {
  int argc = invocation->argv->count;
  std::vector<const char *> Compilation;
  for (int i = 0; i < argc; ++i)
    Compilation.push_back(swift::c_string_utils::get_C_string(invocation->argv->strings[i]));

  auto TargetInfo = swift::dependencies::getTargetInfo(Compilation, main_executable_path);
  if (TargetInfo.getError())
    return swift::c_string_utils::create_null();
  return TargetInfo.get();
}

swiftscan_string_set_t *
swiftscan_compiler_supported_arguments_query() {
  std::unique_ptr<llvm::opt::OptTable> table = swift::createSwiftOptTable();
  std::vector<std::string> frontendFlags;
#define OPTION(...)                                                            \
  addFrontendFlagOption(*table, swift::options::LLVM_MAKE_OPT_ID(__VA_ARGS__), \
                        frontendFlags);
#include "swift/Option/Options.inc"
#undef OPTION
  return swift::c_string_utils::create_set(frontendFlags);
}

swiftscan_string_set_t *
swiftscan_compiler_supported_features_query() {
  std::vector<std::string> allFeatures;
  allFeatures.emplace_back("library-level");
  allFeatures.emplace_back("emit-abi-descriptor");
  return swift::c_string_utils::create_set(allFeatures);
}

//=== Scanner Diagnostics -------------------------------------------------===//
swiftscan_diagnostic_set_t*
swiftscan_scanner_diagnostics_query(swiftscan_scanner_t scanner) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  auto Diagnostics = ScanningTool->getDiagnostics();
  auto NumDiagnostics = Diagnostics.size();

  swiftscan_diagnostic_set_t *Result = new swiftscan_diagnostic_set_t;
  Result->count = NumDiagnostics;
  Result->diagnostics = new swiftscan_diagnostic_info_t[NumDiagnostics];
  
  for (size_t i = 0; i < NumDiagnostics; ++i) {
    const auto &Diagnostic = Diagnostics[i];
    swiftscan_diagnostic_info_s *DiagnosticInfo = new swiftscan_diagnostic_info_s;
    DiagnosticInfo->message = swift::c_string_utils::create_clone(Diagnostic.Message.c_str());
    switch (Diagnostic.Severity) {
    case llvm::SourceMgr::DK_Error:
      DiagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_ERROR;
      break;
    case llvm::SourceMgr::DK_Warning:
      DiagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_WARNING;
      break;
    case llvm::SourceMgr::DK_Note:
      DiagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_NOTE;
      break;
    case llvm::SourceMgr::DK_Remark:
      DiagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_REMARK;
      break;
    }
    // swiftscan_scanner_diagnostics_query is deprecated,
    // so it does not support source locations.
    DiagnosticInfo->source_location = nullptr;
    Result->diagnostics[i] = DiagnosticInfo;
  }

  return Result;
}

void
swiftscan_scanner_diagnostics_reset(swiftscan_scanner_t scanner) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
  ScanningTool->resetDiagnostics();
}

swiftscan_string_ref_t
swiftscan_diagnostic_get_message(swiftscan_diagnostic_info_t diagnostic) {
  return diagnostic->message;
}

swiftscan_diagnostic_severity_t
swiftscan_diagnostic_get_severity(swiftscan_diagnostic_info_t diagnostic) {
  return diagnostic->severity;
}

swiftscan_source_location_t
swiftscan_diagnostic_get_source_location(swiftscan_diagnostic_info_t diagnostic) {
  return diagnostic->source_location;
}

void swiftscan_diagnostic_dispose(swiftscan_diagnostic_info_t diagnostic) {
  swiftscan_string_dispose(diagnostic->message);
  if (diagnostic->source_location) {
    swiftscan_string_dispose(diagnostic->source_location->buffer_identifier);
    delete diagnostic->source_location;
  }
  delete diagnostic;
}

void
swiftscan_diagnostics_set_dispose(swiftscan_diagnostic_set_t* diagnostics){
  if (diagnostics) {
    for (size_t i = 0; i < diagnostics->count; ++i) {
      swiftscan_diagnostic_dispose(diagnostics->diagnostics[i]);
    }
    delete[] diagnostics->diagnostics;
    delete diagnostics;
  }
}

//=== Source Location -----------------------------------------------------===//

swiftscan_string_ref_t
swiftscan_source_location_get_buffer_identifier(swiftscan_source_location_t source_location) {
  return source_location->buffer_identifier;
}

int64_t
swiftscan_source_location_get_line_number(swiftscan_source_location_t source_location) {
  return source_location->line_number;
}

int64_t
swiftscan_source_location_get_column_number(swiftscan_source_location_t source_location) {
  return source_location->column_number;
}

//=== Experimental Compiler Invocation Functions ------------------------===//

int invoke_swift_compiler(int argc, const char **argv) {
  return swift::mainEntry(argc, argv);
}
