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

#include "swift/AST/ModuleDependencies.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/StringSaver.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace dependencies {

/// The high-level implementation of the dependency scanner that runs on
/// an individual worker thread.
class DependencyScanningTool {
public:
  /// Construct a dependency scanning tool.
  DependencyScanningTool();

  /// Collect the full module depenedency graph for the input, ignoring any placeholder
  /// modules.
  ///
  /// \returns a \c StringError with the diagnostic output if clang errors
  /// occurred, \c FullDependencies otherwise.
  std::string
  getFullDependencies(ArrayRef<const char *> Command,
                      const llvm::StringSet<> &InputFiles,
                      const llvm::StringSet<> &PlaceholderModules);

private:
  /// Shared cache of module dependencies, re-used by individual queries
  /// during the lifetime of this Tool
  ModuleDependenciesCache GlobalCache;
  
  llvm::BumpPtrAllocator Alloc;
  llvm::StringSaver Saver;
};

} // end namespace dependencies
} // end namespace swift

#endif // SWIFT_DEPENDENCY_SCANNING_TOOL_H
