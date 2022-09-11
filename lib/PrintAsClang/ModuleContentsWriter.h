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

/// Prints the declarations of \p M to \p os in C++ language mode and collects
/// imports in \p imports along the way.
void printModuleContentsAsCxx(raw_ostream &os,
                              llvm::SmallPtrSetImpl<ImportModuleTy> &imports,
                              ModuleDecl &M,
                              SwiftToClangInteropContext &interopContext,
                              bool requiresExposedAttribute);

} // end namespace swift

#endif

