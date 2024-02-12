//===--- Dependencies.h -- Unified header for dependency tracing utilities --===//
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

#ifndef SWIFT_FRONTENDTOOL_DEPENDENCIES_H
#define SWIFT_FRONTENDTOOL_DEPENDENCIES_H

namespace llvm {
namespace vfs {
class OutputBackend;
}
} // namespace llvm

namespace swift {

class ASTContext;
class DependencyTracker;
class DiagnosticEngine;
class FrontendOptions;
class InputFile;
class ModuleDecl;

/// Emit the names of the modules imported by \c mainModule.
bool emitImportedModules(ModuleDecl *mainModule, const FrontendOptions &opts,
                         llvm::vfs::OutputBackend &backend);
bool emitMakeDependenciesIfNeeded(DiagnosticEngine &diags,
                                  DependencyTracker *depTracker,
                                  const FrontendOptions &opts,
                                  const InputFile &input,
                                  llvm::vfs::OutputBackend &backend);
bool emitLoadedModuleTraceIfNeeded(ModuleDecl *mainModule,
                                   DependencyTracker *depTracker,
                                   const FrontendOptions &opts,
                                   const InputFile &input);

} // end namespace swift

#endif
