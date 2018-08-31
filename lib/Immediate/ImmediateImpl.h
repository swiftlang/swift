//===--- ImmediateImpl.h - Support functions for immediate mode -*- C++ -*-===//
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

#ifndef SWIFT_IMMEDIATEIMPL_H
#define SWIFT_IMMEDIATEIMPL_H

#include "swift/AST/LinkLibrary.h"
#include "swift/AST/SearchPathOptions.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace llvm {
  class Function;
  class Module;
}

namespace swift {
  class CompilerInstance;
  class DiagnosticEngine;
  class IRGenOptions;
  class ModuleDecl;
  class SILOptions;

namespace immediate {

/// Returns a handle to the runtime suitable for other \c dlsym or \c dlclose
/// calls or \c null if an error occurred.
///
/// \param runtimeLibPath Path to search for compiler-relative stdlib dylibs.
void *loadSwiftRuntime(StringRef runtimeLibPath);
bool tryLoadLibraries(ArrayRef<LinkLibrary> LinkLibraries,
                      SearchPathOptions SearchPathOpts,
                      DiagnosticEngine &Diags);
bool linkLLVMModules(llvm::Module *Module,
                     std::unique_ptr<llvm::Module> SubModule);
bool IRGenImportedModules(
    CompilerInstance &CI, llvm::Module &Module,
    llvm::SmallPtrSetImpl<swift::ModuleDecl *> &ImportedModules,
    SmallVectorImpl<llvm::Function *> &InitFns, IRGenOptions &IRGenOpts,
    const SILOptions &SILOpts);

} // end namespace immediate
} // end namespace swift

#endif

