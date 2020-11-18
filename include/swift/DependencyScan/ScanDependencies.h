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

#ifndef SWIFT_FRONTENDTOOL_SCANDEPENDENCIES_H
#define SWIFT_FRONTENDTOOL_SCANDEPENDENCIES_H

#include "llvm/ADT/StringRef.h"

namespace llvm {
class StringSaver;
}

namespace swift {

class CompilerInvocation;
class CompilerInstance;
class ModuleDependenciesCache;

namespace dependencies {

struct BatchScanInput {
  llvm::StringRef moduleName;
  llvm::StringRef arguments;
  llvm::StringRef outputPath;
  bool isSwift;
};

// MARK: swift-frontend -scan-dependencies entry points
/// Scans the dependencies of the main module of \c instance and writes out
/// the resulting JSON according to the instance's output parameters.
/// This method is used for swift-frontend invocations in dependency scanning mode
/// (-scan-dependencies), where the module dependency cache is not shared.
bool scanDependencies(CompilerInstance &instance);

/// Batch scan the dependencies for modules specified in \c batchInputFile.
bool batchScanDependencies(CompilerInstance &instance,
                           llvm::StringRef batchInputFile);

/// Identify all imports in the translation unit's module.
bool prescanMainModuleDependencies(CompilerInstance &instance);


// MARK: dependency scanning execution
/// Scans the dependencies of the main module of \c instance.
bool performModuleScan(CompilerInstance &instance,
                       ModuleDependenciesCache &cache,
                       llvm::raw_ostream &out);

/// Batch scan the dependencies for modules specified in \c batchInputFile.
bool performBatchModuleScan(CompilerInstance &instance,
                            ModuleDependenciesCache &cache,
                            llvm::StringSaver &saver,
                            const std::vector<BatchScanInput> &BatchInput);

/// Scan for dependencies of a module with a specified name, producing the resulting output
/// at the specified output path.
bool scanBatchModuleEntry(CompilerInstance &instance,
                          ModuleDependenciesCache &cache,
                          llvm::StringRef moduleName,
                          bool isClang,
                          llvm::StringRef outputPath);

} // end namespace dependencies
} // end namespace swift

#endif
