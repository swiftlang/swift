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

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsFrontend.h"
#include "swift/Basic/LLVMInitialize.h"
#include "swift/Basic/TargetInfo.h"
#include "swift/Basic/ColorUtils.h"
#include "swift/DependencyScan/DependencyScanningTool.h"
#include "swift/DependencyScan/DependencyScanImpl.h"
#include "swift/DependencyScan/SerializedModuleDependencyCacheFormat.h"
#include "swift/DependencyScan/StringUtils.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/VirtualOutputBackends.h"

#include <sstream>

namespace swift {
namespace dependencies {

// Global mutex for target info queries since they are executed separately .
llvm::sys::SmartMutex<true> TargetInfoMutex;

llvm::ErrorOr<swiftscan_string_ref_t> getTargetInfo(ArrayRef<const char *> Command,
                                                    const char *main_executable_path) {
  llvm::sys::SmartScopedLock<true> Lock(TargetInfoMutex);

  // Parse arguments.
  std::string CommandString;
  for (const auto *c : Command) {
    CommandString.append(c);
    CommandString.append(" ");
  }
  SmallVector<const char *, 4> Args;
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver(Alloc);
  // Ensure that we use the Windows command line parsing on Windows as we need
  // to ensure that we properly handle paths.
  if (llvm::Triple(llvm::sys::getProcessTriple()).isOSWindows()) 
    llvm::cl::TokenizeWindowsCommandLine(CommandString, Saver, Args);
  else
    llvm::cl::TokenizeGNUCommandLine(CommandString, Saver, Args);
  SourceManager dummySM;
  DiagnosticEngine DE(dummySM);
  CompilerInvocation Invocation;
  if (Invocation.parseArgs(Args, DE, nullptr, {}, main_executable_path)) {
    return std::make_error_code(std::errc::invalid_argument);
  }

  // Store the result to a string.
  std::string ResultStr;
  llvm::raw_string_ostream StrOS(ResultStr);
  swift::targetinfo::printTargetInfo(Invocation, StrOS);
  return c_string_utils::create_clone(ResultStr.c_str());
}

void DependencyScanDiagnosticCollector::handleDiagnostic(SourceManager &SM,
                      const DiagnosticInfo &Info) {
  addDiagnostic(SM, Info);
  for (auto ChildInfo : Info.ChildDiagnosticInfo) {
    addDiagnostic(SM, *ChildInfo);
  }
}

void DependencyScanDiagnosticCollector::addDiagnostic(
    SourceManager &SM, const DiagnosticInfo &Info) {
  llvm::sys::SmartScopedLock<true> Lock(ScanningDiagnosticConsumerStateLock);

  // Determine what kind of diagnostic we're emitting.
  llvm::SourceMgr::DiagKind SMKind;
  switch (Info.Kind) {
  case DiagnosticKind::Error:
    SMKind = llvm::SourceMgr::DK_Error;
    break;
  case DiagnosticKind::Warning:
    SMKind = llvm::SourceMgr::DK_Warning;
    break;
  case DiagnosticKind::Note:
    SMKind = llvm::SourceMgr::DK_Note;
    break;
  case DiagnosticKind::Remark:
    SMKind = llvm::SourceMgr::DK_Remark;
    break;
  }

  // Translate ranges.
  SmallVector<llvm::SMRange, 2> Ranges;
  for (auto R : Info.Ranges)
    Ranges.push_back(getRawRange(SM, R));

  // Translate fix-its.
  SmallVector<llvm::SMFixIt, 2> FixIts;
  for (DiagnosticInfo::FixIt F : Info.FixIts)
    FixIts.push_back(getRawFixIt(SM, F));

  // Display the diagnostic.
  std::string FormattedMessage;
  llvm::raw_string_ostream Stream(FormattedMessage);
  // Actually substitute the diagnostic arguments into the diagnostic text.
  llvm::SmallString<256> Text;
  {
    llvm::raw_svector_ostream Out(Text);
    DiagnosticEngine::formatDiagnosticText(Out, Info.FormatString,
                                           Info.FormatArgs);
    auto Msg = SM.GetMessage(Info.Loc, SMKind, Text, Ranges, FixIts, true);
    Msg.print(nullptr, Stream, false, false, false);
    Stream.flush();
  }

  if (Info.Loc && Info.Loc.isValid()) {
    auto bufferIdentifier = SM.getDisplayNameForLoc(Info.Loc);
    auto lineAndColumnNumbers = SM.getLineAndColumnInBuffer(Info.Loc);
    auto importLocation = ScannerImportStatementInfo::ImportDiagnosticLocationInfo(
      bufferIdentifier.str(), lineAndColumnNumbers.first,
      lineAndColumnNumbers.second);
    Diagnostics.push_back(
      ScannerDiagnosticInfo{FormattedMessage, SMKind, importLocation});
  } else {
    Diagnostics.push_back(
      ScannerDiagnosticInfo{FormattedMessage, SMKind, std::nullopt});
  }
}

swiftscan_diagnostic_set_t *mapCollectedDiagnosticsForOutput(
    const DependencyScanDiagnosticCollector *diagnosticCollector) {
  auto collectedDiagnostics = diagnosticCollector->getDiagnostics();
  auto numDiagnostics = collectedDiagnostics.size();
  swiftscan_diagnostic_set_t *diagnosticOutput = new swiftscan_diagnostic_set_t;
  diagnosticOutput->count = numDiagnostics;
  diagnosticOutput->diagnostics =
      new swiftscan_diagnostic_info_t[numDiagnostics];
  for (size_t i = 0; i < numDiagnostics; ++i) {
    const auto &Diagnostic = collectedDiagnostics[i];
    swiftscan_diagnostic_info_s *diagnosticInfo =
        new swiftscan_diagnostic_info_s;
    diagnosticInfo->message =
        swift::c_string_utils::create_clone(Diagnostic.Message.c_str());
    switch (Diagnostic.Severity) {
    case llvm::SourceMgr::DK_Error:
      diagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_ERROR;
      break;
    case llvm::SourceMgr::DK_Warning:
      diagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_WARNING;
      break;
    case llvm::SourceMgr::DK_Note:
      diagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_NOTE;
      break;
    case llvm::SourceMgr::DK_Remark:
      diagnosticInfo->severity = SWIFTSCAN_DIAGNOSTIC_SEVERITY_REMARK;
      break;
    }

    if (Diagnostic.ImportLocation.has_value()) {
      auto importLocation = Diagnostic.ImportLocation.value();
      swiftscan_source_location_s *sourceLoc = new swiftscan_source_location_s;
      if (importLocation.bufferIdentifier.empty())
        sourceLoc->buffer_identifier = swift::c_string_utils::create_null();
      else
        sourceLoc->buffer_identifier = swift::c_string_utils::create_clone(
            importLocation.bufferIdentifier.c_str());
      sourceLoc->line_number = importLocation.lineNumber;
      sourceLoc->column_number = importLocation.columnNumber;
      diagnosticInfo->source_location = sourceLoc;
    } else {
      diagnosticInfo->source_location = nullptr;
    }

    diagnosticOutput->diagnostics[i] = diagnosticInfo;
  }
  return diagnosticOutput;
}

// Generate an instance of the `swiftscan_dependency_graph_s` which contains no
// module dependnecies but captures the diagnostics emitted during the attempted
// scan query.
static swiftscan_dependency_graph_t generateHollowDiagnosticOutput(
    const DependencyScanDiagnosticCollector &ScanDiagnosticConsumer) {
  // Create a dependency graph instance
  swiftscan_dependency_graph_t hollowResult = new swiftscan_dependency_graph_s;

  // Populate the `modules` with a single info for the main module
  // containing no dependencies
  swiftscan_dependency_set_t *dependencySet = new swiftscan_dependency_set_t;
  dependencySet->count = 1;
  dependencySet->modules = new swiftscan_dependency_info_t[1];
  swiftscan_dependency_info_s *hollowMainModuleInfo =
      new swiftscan_dependency_info_s;
  dependencySet->modules[0] = hollowMainModuleInfo;
  hollowResult->dependencies = dependencySet;

  // Other main module details empty
  hollowMainModuleInfo->direct_dependencies =
      c_string_utils::create_empty_set();
  hollowMainModuleInfo->source_files = c_string_utils::create_empty_set();
  hollowMainModuleInfo->module_path = c_string_utils::create_null();
  hollowResult->main_module_name = c_string_utils::create_clone("unknown");
  hollowMainModuleInfo->module_name =
      c_string_utils::create_clone("swiftTextual:unknown");

  // Hollow info details
  swiftscan_module_details_s *hollowDetails = new swiftscan_module_details_s;
  hollowDetails->kind = SWIFTSCAN_DEPENDENCY_INFO_SWIFT_TEXTUAL;
  hollowDetails->swift_textual_details = {c_string_utils::create_null(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_null(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_empty_set(),
                                          c_string_utils::create_null(),
                                          false,
                                          false,
                                          c_string_utils::create_null(),
                                          c_string_utils::create_null(),
                                          c_string_utils::create_null(),
                                          nullptr,
                                          c_string_utils::create_null(),
                                          c_string_utils::create_null(),
                                          c_string_utils::create_null()};
  hollowMainModuleInfo->details = hollowDetails;

  // Empty Link Library set
  swiftscan_link_library_set_t *hollowLinkLibrarySet =
      new swiftscan_link_library_set_t;
  hollowLinkLibrarySet->count = 0;
  hollowLinkLibrarySet->link_libraries = nullptr;
  hollowMainModuleInfo->link_libraries = hollowLinkLibrarySet;

  // Empty Import set
  swiftscan_import_info_set_t *hollowImportInfoSet =
      new swiftscan_import_info_set_t;
  hollowImportInfoSet->count = 0;
  hollowImportInfoSet->imports = nullptr;
  hollowMainModuleInfo->imports = hollowImportInfoSet;

  // Populate the diagnostic info
  hollowResult->diagnostics =
      mapCollectedDiagnosticsForOutput(&ScanDiagnosticConsumer);
  return hollowResult;
}

// Generate an instance of the `swiftscan_import_set_t` which contains no
// imports but captures the diagnostics emitted during the attempted
// scan query.
static swiftscan_import_set_t generateHollowDiagnosticOutputImportSet(
    const DependencyScanDiagnosticCollector &ScanDiagnosticConsumer) {
  // Create an dependency graph instance
  swiftscan_import_set_t hollowResult = new swiftscan_import_set_s;
  hollowResult->imports = c_string_utils::create_empty_set();
  hollowResult->diagnostics =
      mapCollectedDiagnosticsForOutput(&ScanDiagnosticConsumer);
  return hollowResult;
}

DependencyScanningTool::DependencyScanningTool()
    : ScanningService(std::make_unique<SwiftDependencyScanningService>()),
      Alloc(), Saver(Alloc) {}

llvm::ErrorOr<swiftscan_dependency_graph_t>
DependencyScanningTool::getDependencies(
    ArrayRef<const char *> Command,
    const llvm::StringSet<> &PlaceholderModules,
    StringRef WorkingDirectory) {
  // There may be errors as early as in instance initialization, so we must ensure
  // we can catch those.
  auto ScanDiagnosticConsumer =
    std::make_shared<DependencyScanDiagnosticCollector>();

  // The primary instance used to scan the query Swift source-code
  auto QueryContextOrErr = initCompilerInstanceForScan(Command,
                                                       WorkingDirectory,
                                                       ScanDiagnosticConsumer);
  if (QueryContextOrErr.getError())
    return generateHollowDiagnosticOutput(*ScanDiagnosticConsumer);

  auto QueryContext = std::move(*QueryContextOrErr);

  // Local scan cache instance, wrapping the shared global cache.
  ModuleDependenciesCache cache(
      *ScanningService, QueryContext.ScanInstance->getMainModule()->getNameStr().str(),
      QueryContext.ScanInstance->getInvocation().getFrontendOptions().ExplicitModulesOutputPath,
      QueryContext.ScanInstance->getInvocation().getFrontendOptions().ExplicitSDKModulesOutputPath,
      QueryContext.ScanInstance->getInvocation().getModuleScanningHash());
  // Execute the scanning action, retrieving the in-memory result
  auto DependenciesOrErr = performModuleScan(*QueryContext.ScanInstance.get(), 
                                             QueryContext.ScanDiagnostics.get(),
                                             cache);
  if (DependenciesOrErr.getError())
    return generateHollowDiagnosticOutput(*ScanDiagnosticConsumer);

  return std::move(*DependenciesOrErr);
}

llvm::ErrorOr<swiftscan_import_set_t>
DependencyScanningTool::getImports(ArrayRef<const char *> Command,
                                   StringRef WorkingDirectory) {
  // There may be errors as early as in instance initialization, so we must ensure
  // we can catch those
  auto ScanDiagnosticConsumer = std::make_shared<DependencyScanDiagnosticCollector>();
  // The primary instance used to scan the query Swift source-code
  auto QueryContextOrErr = initCompilerInstanceForScan(Command,
                                                       WorkingDirectory,
                                                       ScanDiagnosticConsumer);
  if (QueryContextOrErr.getError())
    return generateHollowDiagnosticOutputImportSet(*ScanDiagnosticConsumer);

  auto QueryContext = std::move(*QueryContextOrErr);

  // Local scan cache instance, wrapping the shared global cache.
  ModuleDependenciesCache cache(
      *ScanningService, QueryContext.ScanInstance->getMainModule()->getNameStr().str(),
      QueryContext.ScanInstance->getInvocation().getFrontendOptions().ExplicitModulesOutputPath,
      QueryContext.ScanInstance->getInvocation().getFrontendOptions().ExplicitSDKModulesOutputPath,
      QueryContext.ScanInstance->getInvocation().getModuleScanningHash());
  auto DependenciesOrErr = performModulePrescan(*QueryContext.ScanInstance.get(), 
                                                QueryContext.ScanDiagnostics.get(),
                                                cache);
  if (DependenciesOrErr.getError())
    return generateHollowDiagnosticOutputImportSet(*ScanDiagnosticConsumer);

  return std::move(*DependenciesOrErr);
}

llvm::ErrorOr<ScanQueryInstance>
DependencyScanningTool::initCompilerInstanceForScan(
    ArrayRef<const char *> CommandArgs,
    StringRef WorkingDir,
    std::shared_ptr<DependencyScanDiagnosticCollector> scannerDiagnosticsCollector) {
  // The remainder of this method operates on shared state in the
  // scanning service
  llvm::sys::SmartScopedLock<true> Lock(DependencyScanningToolStateLock);
  // FIXME: Instead, target-info and supported-features queries must use
  // `DependencyScanningToolStateLock`, but this currently requires further
  // client-side API plumbing.
  llvm::sys::SmartScopedLock<true> TargetInfoLock(TargetInfoMutex);

  // State unique to an individual scan
  auto Instance = std::make_unique<CompilerInstance>();
  Instance->addDiagnosticConsumer(scannerDiagnosticsCollector.get());

  // Basic error checking on the arguments
  if (CommandArgs.empty()) {
    Instance->getDiags().diagnose(SourceLoc(), diag::error_no_frontend_args);
    return std::make_error_code(std::errc::invalid_argument);
  }

  CompilerInvocation Invocation;
  SmallString<128> WorkingDirectory(WorkingDir);
  if (WorkingDirectory.empty())
    llvm::sys::fs::current_path(WorkingDirectory);

  // Parse/tokenize arguments.
  std::string CommandString;
  for (const auto *c : CommandArgs) {
    CommandString.append(c);
    CommandString.append(" ");
  }
  SmallVector<const char *, 4> Args;
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver(Alloc);
  // Ensure that we use the Windows command line parsing on Windows as we need
  // to ensure that we properly handle paths.
  if (llvm::Triple(llvm::sys::getProcessTriple()).isOSWindows())
    llvm::cl::TokenizeWindowsCommandLine(CommandString, Saver, Args);
  else
    llvm::cl::TokenizeGNUCommandLine(CommandString, Saver, Args);

  if (Invocation.parseArgs(Args, Instance->getDiags(),
                           nullptr, WorkingDirectory, "/tmp/foo")) {
    return std::make_error_code(std::errc::invalid_argument);
  }

  // Setup the instance
  std::string InstanceSetupError;
  if (Instance->setup(Invocation, InstanceSetupError)) {
    return std::make_error_code(std::errc::not_supported);
  }
  Invocation.getFrontendOptions().LLVMArgs.clear();

  // Setup the caching service after the instance finishes setup.
  if (ScanningService->setupCachingDependencyScanningService(*Instance))
    return std::make_error_code(std::errc::invalid_argument);

  (void)Instance->getMainModule();

  return ScanQueryInstance{std::move(Instance), 
                           scannerDiagnosticsCollector};
}

} // namespace dependencies
} // namespace swift
