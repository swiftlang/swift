//===--- ReferenceDependencies.h - Generates swiftdeps files ----*- C++ -*-===//
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

#ifndef SWIFT_FRONTENDTOOL_REFERENCEDEPENDENCIES_H
#define SWIFT_FRONTENDTOOL_REFERENCEDEPENDENCIES_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include <string>
#include <vector>

namespace swift {

class DependencyTracker;
class DiagnosticEngine;
class FrontendOptions;
class SourceFile;

/// Sort a set of paths in an order that's (comparatively) stable against
/// variation in filesystem prefix: lexicographic comparison of the
/// byte-reversals of each path. Used for emitting external dependencies.
std::vector<std::string>
reversePathSortedFilenames(const llvm::ArrayRef<std::string> paths);

/// Emit a Swift-style dependencies file for \p SF.
bool emitReferenceDependencies(DiagnosticEngine &diags, SourceFile *SF,
                               const DependencyTracker &depTracker,
                               StringRef outputPath);
} // end namespace swift

#endif
