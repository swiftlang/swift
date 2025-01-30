//===--- ScanDependencies.h -- Scans the dependencies of a module ------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_DEPENDENCY_SCANDEPENDENCIES_H
#define SWIFT_DEPENDENCY_SCANDEPENDENCIES_H

#include "swift-c/DependencyScan/DependencyScan.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/Error.h"

namespace llvm {
class StringSaver;
}

namespace swift {

class CompilerInvocation;
class CompilerInstance;
class ModuleDependenciesCache;
class SwiftDependencyScanningService;

namespace dependencies {
class DependencyScanDiagnosticCollector;

using CompilerArgInstanceCacheMap =
    llvm::StringMap<std::tuple<std::unique_ptr<CompilerInstance>,
                               std::unique_ptr<SwiftDependencyScanningService>,
                               std::unique_ptr<ModuleDependenciesCache>>>;

// MARK: FrontendTool dependency scanner entry points
/// Scans the dependencies of the main module of \c instance and writes out
/// the resulting JSON according to the instance's output parameters.
bool scanDependencies(CompilerInstance &instance);

/// Identify all imports in the translation unit's module.
bool prescanDependencies(CompilerInstance &instance);

// MARK: Dependency scanning execution
/// Scans the dependencies of the main module of \c instance.
llvm::ErrorOr<swiftscan_dependency_graph_t>
performModuleScan(CompilerInstance &instance,
                  DependencyScanDiagnosticCollector *diagnostics,
                  ModuleDependenciesCache &cache);

/// Scans the main module of \c instance for all direct module imports
llvm::ErrorOr<swiftscan_import_set_t>
performModulePrescan(CompilerInstance &instance,
                     DependencyScanDiagnosticCollector *diagnostics,
                     ModuleDependenciesCache &cache);

} // end namespace dependencies
} // end namespace swift

#endif
