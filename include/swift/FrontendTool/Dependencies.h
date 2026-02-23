//===----------------------------------------------------------------------===//
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

namespace llvm::vfs {
class OutputBackend;
} // namespace llvm::vfs

namespace swift {

class DependencyTracker;
class FrontendOptions;
class InputFile;
class ModuleDecl;
class CompilerInstance;
struct ModuleDependencyID;
class ModuleDependenciesCache;
class ASTContext;

/// Emit the names of the modules imported by \c mainModule.
bool emitImportedModules(ModuleDecl *mainModule, const FrontendOptions &opts,
                         llvm::vfs::OutputBackend &backend);

/// Emit loadedModuleTrace file if needed.
bool emitLoadedModuleTraceIfNeeded(ModuleDecl *mainModule,
                                   DependencyTracker *depTracker,
                                   const FrontendOptions &opts,
                                   const InputFile &input);

/// Emit loadedModuleTrace file from module dependency.
bool emitLoadedModuleTraceIfNeeded(const ModuleDependencyID &mainModule,
                                   const ModuleDependenciesCache &cache,
                                   const ASTContext &context,
                                   const FrontendOptions &opts);

/// Emit fine grain dependency file if needed.
bool emitFineModuleTraceIfNeeded(CompilerInstance &Instance,
                                 const FrontendOptions &opts);

} // end namespace swift

#endif
