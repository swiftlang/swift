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
  StringRef moduleName;
  StringRef arguments;
  StringRef outputPath;
  bool isSwift;
};

/// Scans the dependencies of the main module of \c instance and writes out
/// the resulting JSON according to the instance's output parameters.
/// This method is used for swift-frontend invocations in dependency scanning mode
/// (-scan-dependencies), where the module dependency cache is not shared.
bool scanAndOutputDependencies(CompilerInstance &instance);

/// Scans the dependencies of the underlying clang module of the main module
/// of \c instance.
bool scanClangDependencies(CompilerInstance &instance);

/// Scans the dependencies of the main module of \c instance.
bool scanDependencies(CompilerInstance &instance,
                      ModuleDependenciesCache &cache,
                      llvm::raw_ostream &out);

/// Batch scan the dependencies for modules specified in \c batchInputFile.
bool batchScanDependencies(CompilerInstance &instance,
                                 llvm::StringRef batchInputFile);


/// Batch scan the dependencies for modules specified in \c batchInputFile.
bool executeBatchModuleScan(CompilerInstance &instance,
                            ModuleDependenciesCache &cache,
                            llvm::StringSaver &saver,
                            const std::vector<BatchScanInput> &BatchInput);

/// Scan for dependencies of a module with a specified name, producing the resulting output
/// at the specified output path.
bool executeSingleModuleScan(CompilerInstance &instance,
                             ModuleDependenciesCache &cache,
                             StringRef moduleName,
                             bool isClang,
                             StringRef outputPath);

} // end namespace dependencies
} // end namespace swift

#endif
