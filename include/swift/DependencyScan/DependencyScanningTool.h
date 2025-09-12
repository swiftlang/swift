//===-------------- DependencyScanningTool.h - Swift Compiler -------------===//
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

#ifndef SWIFT_DEPENDENCY_SCANNING_TOOL_H
#define SWIFT_DEPENDENCY_SCANNING_TOOL_H

#include "swift-c/DependencyScan/DependencyScan.h"
#include "swift/Frontend/Frontend.h"
#include "swift/AST/DiagnosticConsumer.h"
#include "swift/AST/ModuleDependencies.h"
#include "swift/DependencyScan/ScanDependencies.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "swift/Frontend/SerializedDiagnosticConsumer.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/StringSaver.h"

namespace swift {
namespace dependencies {
class DependencyScanningTool;
class DepScanInMemoryDiagnosticCollector;

struct ScanQueryContext {
  /// Primary CompilerInstance configured for this scanning action
  std::unique_ptr<CompilerInstance> ScanInstance;
  /// An thread-safe diagnostic consumer which collects all emitted
  /// diagnostics in the scan to be reporte via libSwiftScan API
  std::unique_ptr<DepScanInMemoryDiagnosticCollector> InMemoryDiagnosticCollector;
  /// A thread-safe serialized diagnostics consumer.
  /// Note, although type-erased, this must be an instance of
  /// 'ThreadSafeSerializedDiagnosticConsumer'
  std::unique_ptr<DiagnosticConsumer> SerializedDiagnosticConsumer;

  ScanQueryContext(
      std::unique_ptr<CompilerInstance> ScanInstance,
      std::unique_ptr<DepScanInMemoryDiagnosticCollector>
          InMemoryDiagnosticCollector,
      std::unique_ptr<DiagnosticConsumer> SerializedDiagnosticConsumer)
      : ScanInstance(std::move(ScanInstance)),
        InMemoryDiagnosticCollector(std::move(InMemoryDiagnosticCollector)),
        SerializedDiagnosticConsumer(std::move(SerializedDiagnosticConsumer)) {}

  ScanQueryContext(ScanQueryContext &&other)
      : ScanInstance(std::move(other.ScanInstance)),
        InMemoryDiagnosticCollector(
            std::move(other.InMemoryDiagnosticCollector)),
        SerializedDiagnosticConsumer(
            std::move(other.SerializedDiagnosticConsumer)) {}

  ~ScanQueryContext() {
    if (SerializedDiagnosticConsumer)
      SerializedDiagnosticConsumer->finishProcessing();
  }
};

/// Pure virtual Diagnostic consumer intended for collecting
/// emitted diagnostics in a thread-safe fashion
class ThreadSafeDiagnosticCollector : public DiagnosticConsumer {
private:
  llvm::sys::SmartMutex<true> DiagnosticConsumerStateLock;
  void handleDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) final;

protected:
  virtual void addDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) = 0;

public:
  ThreadSafeDiagnosticCollector() {}
  bool finishProcessing() final { return false; }
};

/// Diagnostic consumer that simply collects the diagnostics emitted so-far
/// and uses a representation agnostic from any specific CompilerInstance state
/// which may have been used to emit the diagnostic
class DepScanInMemoryDiagnosticCollector
    : public ThreadSafeDiagnosticCollector {
public:
  struct ScannerDiagnosticInfo {
    std::string Message;
    llvm::SourceMgr::DiagKind Severity;
    std::optional<ScannerImportStatementInfo::ImportDiagnosticLocationInfo>
        ImportLocation;
  };
  std::vector<ScannerDiagnosticInfo> Diagnostics;

protected:
  void addDiagnostic(SourceManager &SM, const DiagnosticInfo &Info) override;

public:
  friend DependencyScanningTool;
  DepScanInMemoryDiagnosticCollector() {}
  void reset() { Diagnostics.clear(); }
  std::vector<ScannerDiagnosticInfo> getDiagnostics() const {
    return Diagnostics;
  }
  const std::vector<ScannerDiagnosticInfo> &getDiagnosticsRef() const {
    return Diagnostics;
  }
};

/// Given a set of arguments to a print-target-info frontend tool query, produce the
/// JSON target info.
llvm::ErrorOr<swiftscan_string_ref_t> getTargetInfo(ArrayRef<const char *> Command,
                                                    const char *main_executable_path);

/// The high-level implementation of the dependency scanner that runs on
/// an individual worker thread.
class DependencyScanningTool {
public:
  /// Construct a dependency scanning tool.
  DependencyScanningTool();

  /// Collect the full module dependency graph for the input.
  ///
  /// \returns a \c StringError with the diagnostic output if errors
  /// occurred, \c swiftscan_dependency_result_t otherwise.
  llvm::ErrorOr<swiftscan_dependency_graph_t>
  getDependencies(ArrayRef<const char *> Command,
                  StringRef WorkingDirectory);

  /// Collect the set of imports for the input module
  ///
  /// \returns a \c StringError with the diagnostic output if errors
  /// occurred, \c swiftscan_prescan_result_t otherwise.
  llvm::ErrorOr<swiftscan_import_set_t>
  getImports(ArrayRef<const char *> Command, StringRef WorkingDirectory);

  /// Using the specified invocation command, instantiate a CompilerInstance
  /// that will be used for this scan.
  llvm::ErrorOr<ScanQueryContext> createScanQueryContext(
      ArrayRef<const char *> Command, StringRef WorkingDirectory,
      std::vector<DepScanInMemoryDiagnosticCollector::ScannerDiagnosticInfo>
          &initializationDiagnostics);

private:
  /// Shared cache of module dependencies, re-used by individual full-scan queries
  /// during the lifetime of this Tool.
  std::unique_ptr<SwiftDependencyScanningService> ScanningService;

  /// Shared state mutual-exclusivity lock
  llvm::sys::SmartMutex<true> DependencyScanningToolStateLock;
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver;
};

swiftscan_diagnostic_set_t *mapCollectedDiagnosticsForOutput(
    ArrayRef<DepScanInMemoryDiagnosticCollector::ScannerDiagnosticInfo>
        diagnostics);

} // end namespace dependencies
} // end namespace swift

#endif // SWIFT_DEPENDENCY_SCANNING_TOOL_H
