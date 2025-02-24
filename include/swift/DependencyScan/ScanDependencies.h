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
#include "swift/AST/DiagnosticEngine.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/Support/Chrono.h"
#include "llvm/Support/ErrorOr.h"
#include <unordered_set>

namespace llvm {
class StringSaver;
namespace vfs {
class FileSystem;
} // namespace vfs
} // namespace llvm

namespace swift {

class CompilerInvocation;
class CompilerInstance;
class DiagnosticEngine;
class ModuleDependenciesCache;
struct ModuleDependencyID;
struct ModuleDependencyIDHash;
using ModuleDependencyIDSet =
    std::unordered_set<ModuleDependencyID, ModuleDependencyIDHash>;
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

namespace incremental {
/// For the given module dependency graph captured in the 'cache',
/// validate whether each dependency node is up-to-date w.r.t. serialization
/// time-stamp. i.e. if any of the input files of a module dependency are newer
/// than the serialized dependency graph, it is considered invalidated and must
/// be re-scanned.
void validateInterModuleDependenciesCache(
    const ModuleDependencyID &rootModuleID, ModuleDependenciesCache &cache,
    const llvm::sys::TimePoint<> &cacheTimeStamp, llvm::vfs::FileSystem &fs,
    DiagnosticEngine &diags, bool emitRemarks = false);

/// Perform a postorder DFS to locate modules whose build recipe is out-of-date
/// with respect to their inputs. Upon encountering such a module, add it to the
/// set of invalidated modules, along with the path from the root to this
/// module.
void outOfDateModuleScan(const ModuleDependencyID &sourceModuleID,
                         const ModuleDependenciesCache &cache,
                         const llvm::sys::TimePoint<> &cacheTimeStamp,
                         llvm::vfs::FileSystem &fs, DiagnosticEngine &diags,
                         bool emitRemarks, ModuleDependencyIDSet &visited,
                         ModuleDependencyIDSet &modulesRequiringRescan);

/// Validate whether all inputs of a given module dependency
/// are older than the cache serialization time.
bool verifyModuleDependencyUpToDate(
    const ModuleDependencyID &moduleID, const ModuleDependenciesCache &cache,
    const llvm::sys::TimePoint<> &cacheTimeStamp, llvm::vfs::FileSystem &fs,
    DiagnosticEngine &diags, bool emitRemarks);
} // end namespace incremental
} // end namespace dependencies
} // end namespace swift

#endif
