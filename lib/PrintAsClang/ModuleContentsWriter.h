//===--- ModuleContentsWriter.h - Walk module to print ObjC/C++ -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PRINTASCLANG_MODULECONTENTSWRITER_H
#define SWIFT_PRINTASCLANG_MODULECONTENTSWRITER_H

#include "swift/AST/AttrKind.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringSet.h"

namespace clang {
  class Module;
}

namespace swift {
class Decl;
class ModuleDecl;
class SwiftToClangInteropContext;

using ImportModuleTy = PointerUnion<ModuleDecl*, const clang::Module*>;

/// Prints the declarations of \p M to \p os and collecting imports in
/// \p imports along the way.
void printModuleContentsAsObjC(raw_ostream &os,
                               llvm::SmallPtrSetImpl<ImportModuleTy> &imports,
                               ModuleDecl &M,
                               SwiftToClangInteropContext &interopContext);

struct EmittedClangHeaderDependencyInfo {
    /// The set of imported modules used by this module.
    SmallPtrSet<ImportModuleTy, 8> imports;
    /// True if the printed module depends on types from the Stdlib module.
    bool dependsOnStandardLibrary = false;
};

/// Prints the declarations of \p M to \p os in C++ language mode.
///
/// \returns Dependencies required by this module.
EmittedClangHeaderDependencyInfo printModuleContentsAsCxx(
    raw_ostream &os, ModuleDecl &M, SwiftToClangInteropContext &interopContext,
    bool requiresExposedAttribute, llvm::StringSet<> &exposedModules);

} // end namespace swift

#endif

