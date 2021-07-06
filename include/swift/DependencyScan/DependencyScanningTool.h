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
#include "swift/AST/ModuleDependencies.h"
#include "swift/DependencyScan/ScanDependencies.h"
#include "swift/Frontend/PrintingDiagnosticConsumer.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/StringSaver.h"

namespace swift {
namespace dependencies {

/// The high-level implementation of the dependency scanner that runs on
/// an individual worker thread.
class DependencyScanningTool {
public:
  /// Construct a dependency scanning tool.
  DependencyScanningTool();

  /// Collect the full module dependency graph for the input, ignoring any
  /// placeholder modules.
  ///
  /// \returns a \c StringError with the diagnostic output if errors
  /// occurred, \c swiftscan_dependency_result_t otherwise.
  llvm::ErrorOr<swiftscan_dependency_graph_t>
  getDependencies(ArrayRef<const char *> Command,
                  const llvm::StringSet<> &PlaceholderModules);

  /// Collect the set of imports for the input module
  ///
  /// \returns a \c StringError with the diagnostic output if errors
  /// occurred, \c swiftscan_prescan_result_t otherwise.
  llvm::ErrorOr<swiftscan_import_set_t>
  getImports(ArrayRef<const char *> Command);

  /// Collect the full module dependency graph for the input collection of
  /// module names (batch inputs) and output them to the
  /// BatchScanInput-specified output locations.
  ///
  /// \returns a \c std::error_code if errors occurred during scan.
  std::vector<llvm::ErrorOr<swiftscan_dependency_graph_t>>
  getDependencies(ArrayRef<const char *> Command,
                  const std::vector<BatchScanInput> &BatchInput,
                  const llvm::StringSet<> &PlaceholderModules);

  /// Writes the current `SharedCache` instance to a specified FileSystem path.
  void serializeCache(llvm::StringRef path);
  /// Loads an instance of a `ModuleDependenciesCache` to serve as the `SharedCache`
  /// from a specified FileSystem path.
  bool loadCache(llvm::StringRef path);
  /// Discard the tool's current `SharedCache` and start anew.
  void resetCache();

private:
  /// Using the specified invocation command, instantiate a CompilerInstance
  /// that will be used for this scan.
  llvm::ErrorOr<std::unique_ptr<CompilerInstance>>
  initCompilerInstanceForScan(ArrayRef<const char *> Command);

  /// Shared cache of module dependencies, re-used by individual full-scan queries
  /// during the lifetime of this Tool.
  std::unique_ptr<ModuleDependenciesCache> SharedCache;

  /// Shared cache of compiler instances created during batch scanning, corresponding to
  /// command-line options specified in the batch scan input entry.
  std::unique_ptr<CompilerArgInstanceCacheMap> VersionedPCMInstanceCacheCache;

  /// A shared consumer that, for now, just prints the encountered diagnostics.
  PrintingDiagnosticConsumer PDC;
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver;
};

} // end namespace dependencies
} // end namespace swift

#endif // SWIFT_DEPENDENCY_SCANNING_TOOL_H
