//===------------ DependencyScanningTool.cpp - Swift Compiler -------------===//
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

#include "swift/DependencyScan/DependencyScanningTool.h"
#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Frontend/Frontend.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"

#include <sstream>

namespace swift {
namespace dependencies {

DependencyScanningTool::DependencyScanningTool()
    : SharedCache(std::make_unique<ModuleDependenciesCache>()), PDC(), Alloc(),
      Saver(Alloc) {}

llvm::ErrorOr<depscan_dependency_result_t *>
DependencyScanningTool::getDependencies(
    ArrayRef<const char *> Command,
    const llvm::StringSet<> &PlaceholderModules) {
  // The primary instance used to scan the query Swift source-code
  auto InstanceOrErr = initCompilerInstanceForScan(Command);
  if (std::error_code EC = InstanceOrErr.getError())
    return EC;
  auto Instance = std::move(*InstanceOrErr);

  // Execute the scanning action, retreiving the in-memory result
  auto DependenciesOrErr = performModuleScan(*Instance.get(), *SharedCache);
  if (DependenciesOrErr.getError())
    return std::make_error_code(std::errc::not_supported);
  auto Dependencies = std::move(*DependenciesOrErr);

  return Dependencies;
}

std::vector<llvm::ErrorOr<depscan_dependency_result_t *>>
DependencyScanningTool::getDependencies(
    ArrayRef<const char *> Command,
    const std::vector<BatchScanInput> &BatchInput,
    const llvm::StringSet<> &PlaceholderModules) {
  // The primary instance used to scan Swift modules
  auto InstanceOrErr = initCompilerInstanceForScan(Command);
  if (std::error_code EC = InstanceOrErr.getError())
    return std::vector<llvm::ErrorOr<depscan_dependency_result_t *>>(
        BatchInput.size(), std::make_error_code(std::errc::invalid_argument));
  auto Instance = std::move(*InstanceOrErr);

  auto batchScanResults =
      performBatchModuleScan(*Instance.get(), *SharedCache, Saver, BatchInput);

  return batchScanResults;
}

llvm::ErrorOr<std::unique_ptr<CompilerInstance>>
DependencyScanningTool::initCompilerInstanceForScan(
    ArrayRef<const char *> Command) {
  // State unique to an individual scan
  auto Instance = std::make_unique<CompilerInstance>();
  Instance->addDiagnosticConsumer(&PDC);

  // Basic error checking on the arguments
  if (Command.empty()) {
    Instance->getDiags().diagnose(SourceLoc(), diag::error_no_frontend_args);
    return std::make_error_code(std::errc::invalid_argument);
  }

  CompilerInvocation Invocation;
  SmallString<128> WorkingDirectory;
  llvm::sys::fs::current_path(WorkingDirectory);

  // Parse arguments.
  std::string CommandString;
  for (const auto *c : Command) {
    CommandString.append(c);
    CommandString.append(" ");
  }
  SmallVector<const char *, 4> Args;
  llvm::cl::TokenizeGNUCommandLine(CommandString, Saver, Args);
  if (Invocation.parseArgs(Args, Instance->getDiags())) {
    return std::make_error_code(std::errc::invalid_argument);
  }

  // Setup the instance
  Instance->setup(Invocation);
  (void)Instance->getMainModule();

  return Instance;
}

} // namespace dependencies
} // namespace swift

//===--- C API ------------------------------------------------------------===//

using namespace swift::dependencies;

DEFINE_SIMPLE_CONVERSION_FUNCTIONS(DependencyScanningTool, depscan_scanner_t)

depscan_scanner_t depscan_scanner_create(void) {
  return wrap(new DependencyScanningTool());
}

void depscan_scanner_dispose(depscan_scanner_t c_scanner) {
  delete unwrap(c_scanner);
}

depscan_dependency_result_t *
depscan_scan_dependencies(depscan_scanner_t *scanner,
                          const char *working_directory, int argc,
                          const char *const *argv) {
  DependencyScanningTool *ScanningTool = unwrap(scanner);
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

void depscan_dependency_info_details_dispose(
    depscan_module_details_t *details) {
  switch (details->kind) {
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL:
    depscan_string_dispose(
        details->swift_textual_details.module_interface_path);
    depscan_string_set_dispose(
        details->swift_textual_details.compiled_module_candidates);
    depscan_string_dispose(details->swift_textual_details.bridging_header_path);
    depscan_string_set_dispose(
        details->swift_textual_details.bridging_source_files);
    depscan_string_set_dispose(
        details->swift_textual_details.bridging_module_dependencies);
    depscan_string_set_dispose(details->swift_textual_details.command_line);
    depscan_string_set_dispose(details->swift_textual_details.extra_pcm_args);
    depscan_string_dispose(details->swift_textual_details.context_hash);
    break;
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_BINARY:
    depscan_string_dispose(details->swift_binary_details.compiled_module_path);
    depscan_string_dispose(details->swift_binary_details.module_doc_path);
    depscan_string_dispose(
        details->swift_binary_details.module_source_info_path);
    break;
  case DEPSCAN_DEPENDENCY_INFO_SWIFT_PLACEHOLDER:
    depscan_string_dispose(
        details->swift_placeholder_details.compiled_module_path);
    depscan_string_dispose(details->swift_placeholder_details.module_doc_path);
    depscan_string_dispose(
        details->swift_placeholder_details.module_source_info_path);
    break;
  case DEPSCAN_DEPENDENCY_INFO_CLANG:
    depscan_string_dispose(details->clang_details.module_map_path);
    depscan_string_dispose(details->clang_details.context_hash);
    depscan_string_set_dispose(details->clang_details.command_line);
    break;
  }
  delete details;
}

void depscan_dependency_info_dispose(depscan_dependency_info_t *info) {
  depscan_string_dispose(info->module_name);
  depscan_string_dispose(info->module_path);
  depscan_string_set_dispose(info->source_files);
  depscan_string_set_dispose(info->direct_dependencies);
  depscan_dependency_info_details_dispose(info->details);
}

void depscan_dependency_set_dispose(depscan_dependency_set_t *set) {
  for (int i = 0; i < set->count; ++i) {
    depscan_dependency_info_dispose(&set->modules[i]);
  }
  delete[] set->modules;
  delete set;
}

void depscan_dependency_result_dispose(depscan_dependency_result_t *result) {
  depscan_string_dispose(result->main_module_name);
  depscan_dependency_set_dispose(result->module_set);
  delete result;
}
