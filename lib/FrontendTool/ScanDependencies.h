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

namespace swift {

class CompilerInvocation;
class CompilerInstance;

/// Batch scan the dependencies for modules specified in \c batchInputFile.
bool batchScanModuleDependencies(CompilerInstance &instance,
                                 llvm::StringRef batchInputFile);

/// Scans the dependencies of the main module of \c instance.
bool scanDependencies(CompilerInstance &instance);

/// Scans the dependencies of the underlying clang module of the main module
/// of \c instance.
bool scanClangDependencies(CompilerInstance &instance);

} // end namespace swift

#endif
